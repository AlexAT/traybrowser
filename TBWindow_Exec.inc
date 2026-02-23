(*
Licensed under BSD 3-clause license
https://opensource.org/license/bsd-3-clause

Copyright 2025-2026 Alex/AT (alex@alex-at.net)

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

// external process execution, included from TBWindow unit

// this one returns GUID (process with specific GUID started) or empty string if process failed to start
function TTrayBrowserWindow.SpawnExecProcess(const inCommand: String; const inParams: TProcessStringList; const inCallbackID: Integer; const inTimeout: Integer; const inWindowMode: TTBExecWindowMode;
  const inSynchronous: Boolean; const inIndependent: Boolean): String;
var
  LGUID: TGuid;
  LProcess: TTBExecProcess;
begin
  Result := '';
  if FWindowState.ExecProcesses.Count >= tbeMaxProcesses then Exit; // limit number of processes invoked to prevent accidental process bombing
  try
    LProcess := TTBExecProcess.Create(nil);
    repeat
      if CreateGUID(LGUID) <> 0 then TrayBrowserApplication.Die('Failed to generate executed process GUID');
      LProcess.XTBGUID := LGUID.ToString;
    until (not FWindowState.ExecProcesses.ContainsKey(LProcess.XTBGUID));
    LProcess.XTBCallbackID := inCallbackID;
    LProcess.XTBTimeout := inTimeout;
    LProcess.XTBIsIndependent := inIndependent;
    LProcess.XTBIsSynchronous := inSynchronous;
    LProcess.XTBFinished := False;
    LProcess.XTBStdOut := '';
    LProcess.XTBStdErr := '';
    LProcess.XTBExitCode := 0; // use ExitStatus
    LProcess.XTBErrorMessage := '';
    LProcess.PipeBufferSize := 16384;
    LProcess.Executable := inCommand;
    LProcess.Parameters.Assign(inParams);
    LProcess.Options := [];
    if not inIndependent then
    begin
      LProcess.Options := LProcess.Options + [poUsePipes];
      LProcess.InheritHandles := True; // this is necessary on Windows to properly retrieve the output
    end else
    begin
      LProcess.Options := LProcess.Options + [poNewProcessGroup];
      LProcess.InheritHandles := False; // independent process does not need our handles
    end;
    if inSynchronous then LProcess.Options := LProcess.Options + [poStderrToOutput];
    LProcess.StartupOptions := [suoUseShowWindow];
    LProcess.ShowWindow := swoNone;
    case inWindowMode of
      tbewHide:
      begin
        LProcess.Options := LProcess.Options + [poNoConsole];
        LProcess.ShowWindow := swoHIDE;
      end;
      tbewMinimize: LProcess.ShowWindow := swoMinimize;
      tbewMaximize: LProcess.ShowWindow := swoMaximize;
    end;
    LProcess.OnRunCommandEvent := @OnExecProcessEvent;
    ServiceTimer.Enabled := True; // enable service timer as it reaps complete processes
    FWindowState.ExecProcesses.Add(LProcess.XTBGUID, LProcess);
    Result := LProcess.XTBGUID;
    try
      LProcess.Execute;
    except
      // well, the process could not be started, indicate it
      LProcess.XTBExitCode := -1;
      LProcess.XTBErrorMessage := 'EXEC_FAILED';
    end;
  except end;
end;

procedure TTrayBrowserWindow.OnExecProcessEvent(Sender: TObject; Context: TObject; Status: TRunCommandEventCode; const Message: String);
begin
  // it is kind of tempting to use this one to do everything, but there is a lot of corner cases so we still reap processes in service timer
  // at first we do not even check status, just attempt reads immediately, and only then check for possible errors
  try
    if Assigned(Sender) and (Sender is TTBExecProcess) then
    begin
      if not TTBExecProcess(Sender).XTBIsIndependent then ReadExecProcessStreams(TTBExecProcess(Sender));
      if Status = RunCommandException then TTBExecProcess(Sender).XTBErrorMessage := Message;
    end;
  except end;
end;

procedure TTrayBrowserWindow.ReadExecProcessStreams(const inProcess: TTBExecProcess);
var
  i: Integer;
  s: String = '';
begin
  // read streams
  try
    if Assigned(inProcess.Output) then
    begin
      while inProcess.Output.NumBytesAvailable > 0 do
      begin
        i := Min(inProcess.Output.NumBytesAvailable, 16384);
        Setlength(s, i);
        i := inProcess.Output.Read(s[1], i);
        if i = 0 then Break; // somehow empty read, break out to prevent infinite loop
        SetLength(s, i);
        inProcess.XTBStdOut := inProcess.XTBStdOut + s;
      end;
    end;
  except end;
  try
    if Assigned(inProcess.Stderr) then
    begin
      while inProcess.Stderr.NumBytesAvailable > 0 do
      begin
        i := Min(inProcess.Stderr.NumBytesAvailable, 16384);
        Setlength(s, i);
        i := inProcess.Stderr.Read(s[1], inProcess.Stderr.NumBytesAvailable);
        if i = 0 then Break; // somehow empty read, break out to prevent infinite loop
        SetLength(s, i);
        inProcess.XTBStdErr := inProcess.XTBStdErr + s;
      end;
    end;
  except end;
end;

function TTrayBrowserWindow.ServiceReapExecProcesses: Boolean;
var
  LExecProcessPair: TTBExecProcessPair;
  LProcess: TTBExecProcess;
  LDeleteList: TStringList;
  s: String;
begin
  Result := False;

  // check processes to be reaped
  LDeleteList := nil;
  try
    for LExecProcessPair in FWindowState.ExecProcesses do
    begin
      LProcess := LExecProcessPair.Value;
      if not LProcess.XTBFinished then
      begin
        Result := True; // we still have processes to check
        if LProcess.Running and (LProcess.XTBExitCode = 0) then // take care internal exit code <> 0 means reaping yet unterminated process
        begin
          // the process is running, check if it has not timed out
          if LProcess.XTBTimeout >= 0 then
          begin
            LProcess.XTBTimeout := LProcess.XTBTimeout - ServiceTimer.Interval;
            if LProcess.XTBTimeout <= 0 then
            begin
              // yes, the process has timed out
              LProcess.XTBExitCode := -1;
              LProcess.XTBTimeout := -1;
              LProcess.XTBErrorMessage := 'TIMEOUT';
              if not LProcess.XTBIsIndependent then LProcess.Terminate(0); // really terminate the process
            end;
          end;
        end else
        begin
          // the process has finished, reap it
          try
            LProcess.XTBFinished := True;
            ReadExecProcessStreams(LProcess);
            ReadExecProcessStreams(LProcess); // yes, twice, just in case we somehow get zero read somewhere, if we get it twice, so be it
            if not LProcess.XTBIsSynchronous then
            begin
              // asynchronous exec processes just send event and die while synchronous must be explicitly reaped from tbSyncExecState
              // (exitCode, stdout, stderr, errorMsg)
              if LProcess.XTBCallbackID >= 0 then
              begin
                PostJSEvent(
                  '"__callback"'
                  + ',' + IntToStr(LProcess.XTBCallbackID)
                  + ',' + IntToStr(IfThen(LProcess.XTBExitCode <> 0, LProcess.XTBExitCode, LProcess.ExitStatus))
                  + ',' + QuoteJSString(LProcess.XTBStdOut)
                  + ',' + QuoteJSString(LProcess.XTBStdErr)
                  + ',' + QuoteJSString(LProcess.XTBErrorMessage)
                );
              end;
              if LDeleteList = nil then LDeleteList := TStringList.Create; // delete list is created late and only if necessary
              LDeleteList.Add(LProcess.XTBGUID);
            end;
          except end;
        end;
      end;
    end;
  except end;

  // now delete all the processes that were successfully reaped in full
  if LDeleteList <> nil then
  begin
    try
      for s in LDeleteList do
      begin
        LProcess := FWindowState.ExecProcesses[s];
        FWindowState.ExecProcesses.Remove(s);
        FreeAndNil(LProcess);
      end;
    except end;
    FreeAndNil(LDeleteList);
  end;
end;

procedure TTrayBrowserWindow.KillExecProcesses;
var
  LExecProcessPair: TTBExecProcessPair;
  LProcess: TTBExecProcess;
begin
  try
    for LExecProcessPair in FWindowState.ExecProcesses do
    begin
      LProcess := LExecProcessPair.Value;
      if LProcess.Running then
      begin
        LProcess.XTBCallbackID := -1; // remove callback so it does not fire, processes will be reaped but not reported
        LProcess.XTBIsSynchronous := False; // also mark synchronous processes as asynchronous so they will be reaped properly
        LProcess.XTBExitCode := -1;
        LProcess.XTBTimeout := -1;
        LProcess.XTBErrorMessage := 'KILLED';
        if (not LProcess.XTBFinished) and (not LProcess.XTBIsIndependent) then LProcess.Terminate(0); // really terminate the process
      end;
      FreeAndNil(LProcess);
    end;
    FWindowState.ExecProcesses.Clear;
  except end;
end;

