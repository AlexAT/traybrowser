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

// take care: no tracing should be done in this module as it obviously can cause tracing recursion

// logging, including emergency logging

function TTrayBrowserApplication.LogKVMessage(const inMessage: TTBKVMessage): String;
var
  LElement: TTBKVMessagePair;
  LKeys: TStringList;
  LKey: String;
  i: Integer;
begin
  Result := '';

  if not Assigned(inMessage) then
  begin
    Result := '<NO MESSAGE>';
    Exit;
  end;

  LKeys := TStringList.Create;
  try
    for LElement in inMessage do LKeys.Add(LElement.Key);
    LKeys.Sort;
    i := 0;
    for LKey in LKeys do
    begin
      if i <> 0 then Result := Result + ',';
      Result := Result + '`' + LKey + '`=`' + inMessage[LKey] + '`';
      i := i + 1;
    end;
  except end;
  FreeAndNil(LKeys);
end;

function TTrayBrowserApplication.LogValues(const inValues: TStringList): String;
var
  s: String;
  i: Integer;
begin
  Result := '';

  if not Assigned(inValues) then
  begin
    Result := '<NO VALUES>';
    Exit;
  end;

  try
    i := 0;
    for s in inValues do
    begin
      if i <> 0 then Result := Result + ',';
      Result := Result + '`' + s + '`';
      i := i + 1;
    end;
  except end;
end;

// debug logging and tracing

procedure DebugLog(const inMessage: String; const inSeverity: Integer = CEF_LOG_SEVERITY_INFO);
{$IFDEF TB_INIT_DEBUG}
var
  i: Integer;
  LStream: TFileStream;
  s: String;
{$ENDIF}
begin
  if Assigned(GlobalCEFApp) then
  begin
    CefDebugLog(inMessage, inSeverity);
  end else
  begin
    {$IFDEF TB_INIT_DEBUG}
    // long and hard way...
    DebugFileCS.Acquire;
    try
      // as file may be locked, we retry a little (this may still fail long term, but better than nothing)
      i := 100;
      while i > 0 do
      begin
        try
          if not FileExists('tbdebug.log') then LStream :=  TFileStream.Create('tbdebug.log', fmCreate or fmOpenWrite)
            else LStream := TFileStream.Create('tbdebug.log', fmOpenWrite);
          Break;
        except end;
        Sleep(1);
        Dec(i);
      end;
      if i > 0 then
      begin
        try
          LStream.Seek(0, soEnd);
          s := '[' + DateTimeToStr(Now, DefaultFormatSettings, True) + '] [' + IntToStr(GetProcessID) + '] ' + inMessage + LineEnding;
          LStream.Write(s[1], Length(s));
        except end;
        FreeAndNil(LStream);
      end;
    except end;
    DebugFileCS.Release;
    {$ENDIF}
  end;
end;

function DebugTraceRegisterEnter(const inRoutine: String; const inFile: String; const inLine: Integer): String;
var
  tid: LongInt;
  StackElement: TTBTraceStackElement;
begin
  StackElement.inRoutine := inRoutine;
  StackElement.inFile := inFile;
  StackElement.inLine := inLine;

  Result := DebugTracePrintPath('ENTER', inRoutine, '', inFile, inLine);

  DebugThreadTraceCS.Acquire;
  try
    tid := GetCurrentThreadId();
    if not DebugThreadTrace.ContainsKey(tid) then DebugThreadTrace.Add(tid, TTBTraceStack.Create); // create stack
    DebugThreadTrace[tid].Add(StackElement);
  except end;
  DebugThreadTraceCS.Release;
end;

function DebugTraceRegisterExit(const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer): String;
var
  tid: LongInt;
  Stack: TTBTraceStack;
  StackElement: TTBTraceStackElement;
begin
  DebugThreadTraceCS.Acquire;
  try
    tid := GetCurrentThreadId();
    if DebugThreadTrace.ContainsKey(tid) then
    begin
      Stack := DebugThreadTrace[tid];
      while Assigned(Stack) and (Stack.Count > 0) do // we may destroy empty stack, thus Assigned() check
      begin
        StackElement := Stack[Stack.Count - 1];
        Stack.Delete(Stack.Count - 1);
        if Stack.Count = 0 then
        begin
          // remove empty stack
          FreeAndNil(Stack);
          DebugThreadTrace.Remove(tid);
        end;

        if (StackElement.inRoutine = inRoutine) and (StackElement.inFile = inFile) then
        begin
          // found our entry point
          DebugThreadTraceCS.Release;
          try Result := DebugTracePrintPath('EXIT', inRoutine, inPath, inFile, inLine) + '(' + IntToStr(StackElement.inLine) + ')'; except end; // this framing is really needed, we do not want to re-release critical section
          Exit;
        end;

        // weird, this is not our entry point, report missing exit (take care PrintPath acquires critical section by itself, so we need to release meanwhile)
        DebugThreadTraceCS.Release;
        try DebugLog('TRACE ' + DebugTracePrintPath('MISSING EXIT', StackElement.inRoutine, '', StackElement.inFile, StackElement.inLine)); except end; // this framing is really needed, we do not want to re-release critical section
        DebugThreadTraceCS.Acquire;
      end;
    end;
  except end;
  DebugThreadTraceCS.Release;

  // no call stack, this is an error, but ok, bear with it
  Result := DebugTracePrintPath('EXIT W/O ENTER', inRoutine, inPath, inFile, inLine);
end;

function DebugTracePrintPath(const inAction: String; const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer): String;
var
  tid: LongInt;
  Stack: TTBTraceStack;
begin
  DebugThreadTraceCS.Acquire;
  try
    tid := GetCurrentThreadId();
    if DebugThreadTrace.ContainsKey(tid) then
    begin
      Stack := DebugThreadTrace[tid];
      if Stack.Count > 0 then
      begin
        DebugThreadTraceCS.Release;
        try Result := '[' + AddChar('0', LowerCase(HexStr(tid, 8)), 8) + '] ' + StringOfChar('.', Stack.Count) + '[' + inAction + '] ' + inRoutine + IfThen(inPath = '', '', ' (' + inPath + ')') + ' @ ' + inFile + ':' + IntToStr(inLine); except end; // this framing is really needed, we do not want to re-release critical section
        Exit;
      end;
    end;
  except end;
  Result := '[' + AddChar('0', LowerCase(HexStr(tid, 8)), 8) + '] [' + inAction + '] ' + inRoutine + IfThen(inPath = '', '', ' (' + inPath + ')') + ' @ ' + inFile + ':' + IntToStr(inLine);
  DebugThreadTraceCS.Release;
end;

function DebugTracePrintPointer(const inPointer: Pointer): String;
begin
  if (inPointer <> nil) then
  begin
    Result := '<' + AddChar('0', LowerCase(HexStr(inPointer)), 8) + '>'
  end else
  begin
    Result := '<nil>';
  end;
end;

function DebugTracePrintObjectLeft(const inObject: TObject): String;
begin
  if Assigned(inObject) then
  begin
    try
      Result := DebugTracePrintPointer(inObject) + inObject.ClassName;
    except
      try
        Result := DebugTracePrintPointer(inObject) + 'INVALID';
      except
        Result := '<INVALID>INVALID';
      end;
    end
  end else
  begin
    Result := DebugTracePrintPointer(nil) + 'TObject';
  end;
end;

function DebugTracePrintObjectRight(const inObject: TObject): String;
begin
  if Assigned(inObject) then
  begin
    try
      Result := inObject.ClassName + DebugTracePrintPointer(inObject);
    except
      try
        Result := 'INVALID' + DebugTracePrintPointer(inObject);
      except
        Result := 'INVALID<INVALID>';
      end;
    end;
  end else
  begin
    Result := 'TObject' + DebugTracePrintPointer(nil);
  end;
end;

function DebugTracePrintObjectRight(const inObject: TTBJSVariant): String;
begin
  try
    case inObject.vType of
      tbjvBoolean: Result := 'TTBJSVariant(Boolean)<' + IfThen(inObject.vBoolean, 'TRUE', 'FALSE') + '>';
      tbjvInteger: Result := 'TTBJSVariant(Integer)<' + IntToStr(inObject.vInteger) + '>';
      tbjvString: Result := 'TTBJSVariant(String)<' + inObject.vString + '>';
      tbjvFloat: Result := 'TTBJSVariant(Float)<' + FloatToStr(inObject.vFloat) + '>';
      tbjvJson: Result := 'TTBJSVariant(JSON)<' + inObject.vJSON + '>';
      tbjvEmpty: Result := 'TTBJSVariant(EMPTY)';
    end;
  except
    Result := 'TTBJSVariant<INVALID>';
  end;
end;

// enter tracing

procedure DebugTraceEnter(const inRoutine: String; const inFile: String; const inLine: Integer);
begin
  DebugLog('TRACE ' + DebugTraceRegisterEnter(inRoutine, inFile, inLine));
end;

// enter tracing: object

procedure DebugTraceEnter(const inObject: TObject; const inRoutine: String; const inFile: String; const inLine: Integer);
begin
  DebugTraceEnter(DebugTracePrintObjectLeft(inObject) + '.' + inRoutine, inFile, inLine);
end;

// enter tracing: custom source

procedure DebugTraceEnter(const inType: String; inPointer: Pointer; const inRoutine: String; const inFile: String; const inLine: Integer);
begin
  DebugTraceEnter(DebugTracePrintPointer(inPointer) + inType + '.' + inRoutine, inFile, inLine);
end;

// exit tracing

procedure DebugTraceExit(const inRoutine: String; const inFile: String; const inLine: Integer);
begin
  DebugTraceExit(inRoutine, '', inFile, inLine);
end;

// exit tracing: path

procedure DebugTraceExit(const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer);
begin
  DebugLog('TRACE ' + DebugTraceRegisterExit(inRoutine, inPath, inFile, inLine));
end;

// exit tracing: object

procedure DebugTraceExit(const inObject: TObject; const inRoutine: String; const inFile: String; const inLine: Integer);
begin
  DebugTraceExit(DebugTracePrintObjectLeft(inObject) + '.' + inRoutine, inFile, inLine);
end;

// exit tracing: object, path

procedure DebugTraceExit(const inObject: TObject; const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer);
begin
  DebugTraceExit(DebugTracePrintObjectLeft(inObject) + '.' + inRoutine, inPath, inFile, inLine);
end;

// exit tracing: custom source

procedure DebugTraceExit(const inType: String; inPointer: Pointer; const inRoutine: String; const inFile: String; const inLine: Integer);
begin
  DebugTraceExit(DebugTracePrintPointer(inPointer) + inType + '.' + inRoutine, inFile, inLine);
end;

// exit tracing: custom source, path

procedure DebugTraceExit(const inType: String; inPointer: Pointer; const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer);
begin
  DebugTraceExit(DebugTracePrintPointer(inPointer) + inType + '.' + inRoutine, inPath, inFile, inLine);
end;

// exit tracing: result

procedure DebugTraceExit(const inRoutine: String; const inFile: String; const inLine: Integer; const inResult: Boolean);
begin
  DebugTraceExit(inRoutine, '', inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inRoutine: String; const inFile: String; const inLine: Integer; const inResult: Integer);
begin
  DebugTraceExit(inRoutine, '', inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inRoutine: String; const inFile: String; const inLine: Integer; const inResult: String);
begin
  DebugTraceExit(inRoutine, '', inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inRoutine: String; const inFile: String; const inLine: Integer; const inResult: Float);
begin
  DebugTraceExit(inRoutine, '', inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inRoutine: String; const inFile: String; const inLine: Integer; const inResult: TObject);
begin
  DebugTraceExit(inRoutine, '', inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inRoutine: String; const inFile: String; const inLine: Integer; const inResult: TTBJSVariant);
begin
  DebugTraceExit(inRoutine, '', inFile, inLine, inResult);
end;

// exit tracing: path, result

procedure DebugTraceExit(const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer; const inResult: Boolean);
begin
  DebugLog('TRACE ' + DebugTraceRegisterExit(inRoutine, inPath, inFile, inLine) + ' >>> Boolean<' + IfThen(inResult, 'TRUE', 'FALSE') + '>');
end;

procedure DebugTraceExit(const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer; const inResult: Integer);
begin
  DebugLog('TRACE ' + DebugTraceRegisterExit(inRoutine, inPath, inFile, inLine) + ' >>> Integer<' + IntToStr(inResult) + '>');
end;

procedure DebugTraceExit(const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer; const inResult: String);
begin
  DebugLog('TRACE ' + DebugTraceRegisterExit(inRoutine, inPath, inFile, inLine) + ' >>> String<' + inResult + '>');
end;

procedure DebugTraceExit(const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer; const inResult: Float);
begin
  DebugLog('TRACE ' + DebugTraceRegisterExit(inRoutine, inPath, inFile, inLine) + ' >>> Float<' + FloatToStr(inResult) + '>');
end;

procedure DebugTraceExit(const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer; const inResult: TObject);
begin
  DebugLog('TRACE ' + DebugTraceRegisterExit(inRoutine, inPath, inFile, inLine) + ' >>> ' + DebugTracePrintObjectRight(inResult));
end;

procedure DebugTraceExit(const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer; const inResult: TTBJSVariant);
begin
  DebugLog('TRACE ' + DebugTraceRegisterExit(inRoutine, inPath, inFile, inLine) + ' >>> ' + DebugTracePrintObjectRight(inResult));
end;

// exit tracing: object, result

procedure DebugTraceExit(const inObject: TObject; const inRoutine: String; const inFile: String; const inLine: Integer; const inResult: Boolean);
begin
  DebugTraceExit(DebugTracePrintObjectLeft(inObject) + '.' + inRoutine, inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inObject: TObject; const inRoutine: String; const inFile: String; const inLine: Integer; const inResult: Integer);
begin
  DebugTraceExit(DebugTracePrintObjectLeft(inObject) + '.' + inRoutine, inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inObject: TObject; const inRoutine: String; const inFile: String; const inLine: Integer; const inResult: String);
begin
  DebugTraceExit(DebugTracePrintObjectLeft(inObject) + '.' + inRoutine, inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inObject: TObject; const inRoutine: String; const inFile: String; const inLine: Integer; const inResult: Float);
begin
  DebugTraceExit(DebugTracePrintObjectLeft(inObject) + '.' + inRoutine, inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inObject: TObject; const inRoutine: String; const inFile: String; const inLine: Integer; const inResult: TObject);
begin
  DebugTraceExit(DebugTracePrintObjectLeft(inObject) + '.' + inRoutine, inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inObject: TObject; const inRoutine: String; const inFile: String; const inLine: Integer; const inResult: TTBJSVariant);
begin
  DebugTraceExit(DebugTracePrintObjectLeft(inObject) + '.' + inRoutine, inFile, inLine, inResult);
end;

// exit tracing: object, path, result

procedure DebugTraceExit(const inObject: TObject; const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer; const inResult: Boolean);
begin
  DebugTraceExit(DebugTracePrintObjectLeft(inObject) + '.' + inRoutine, inPath, inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inObject: TObject; const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer; const inResult: Integer);
begin
  DebugTraceExit(DebugTracePrintObjectLeft(inObject) + '.' + inRoutine, inPath, inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inObject: TObject; const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer; const inResult: String);
begin
  DebugTraceExit(DebugTracePrintObjectLeft(inObject) + '.' + inRoutine, inPath, inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inObject: TObject; const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer; const inResult: Float);
begin
  DebugTraceExit(DebugTracePrintObjectLeft(inObject) + '.' + inRoutine, inPath, inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inObject: TObject; const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer; const inResult: TObject);
begin
  DebugTraceExit(DebugTracePrintObjectLeft(inObject) + '.' + inRoutine, inPath, inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inObject: TObject; const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer; const inResult: TTBJSVariant);
begin
  DebugTraceExit(DebugTracePrintObjectLeft(inObject) + '.' + inRoutine, inPath, inFile, inLine, inResult);
end;

// exit tracing: custom source, result

procedure DebugTraceExit(const inType: String; const inPointer: Pointer; const inRoutine: String; const inFile: String; const inLine: Integer; const inResult: Boolean);
begin
  DebugTraceExit(DebugTracePrintPointer(inPointer) + inType + '.' + inRoutine, inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inType: String; const inPointer: Pointer; const inRoutine: String; const inFile: String; const inLine: Integer; const inResult: Integer);
begin
  DebugTraceExit(DebugTracePrintPointer(inPointer) + inType + '.' + inRoutine, inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inType: String; const inPointer: Pointer; const inRoutine: String; const inFile: String; const inLine: Integer; const inResult: String);
begin
  DebugTraceExit(DebugTracePrintPointer(inPointer) + inType + '.' + inRoutine, inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inType: String; const inPointer: Pointer; const inRoutine: String; const inFile: String; const inLine: Integer; const inResult: Float);
begin
  DebugTraceExit(DebugTracePrintPointer(inPointer) + inType + '.' + inRoutine, inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inType: String; const inPointer: Pointer; const inRoutine: String; const inFile: String; const inLine: Integer; const inResult: TObject);
begin
  DebugTraceExit(DebugTracePrintPointer(inPointer) + inType + '.' + inRoutine, inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inType: String; const inPointer: Pointer; const inRoutine: String; const inFile: String; const inLine: Integer; const inResult: TTBJSVariant);
begin
  DebugTraceExit(DebugTracePrintPointer(inPointer) + inType + '.' + inRoutine, inFile, inLine, inResult);
end;

// exit tracing: custom source, path, result

procedure DebugTraceExit(const inType: String; const inPointer: Pointer; const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer; const inResult: Boolean);
begin
  DebugTraceExit(DebugTracePrintPointer(inPointer) + inType + '.' + inRoutine, inPath, inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inType: String; const inPointer: Pointer; const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer; const inResult: Integer);
begin
  DebugTraceExit(DebugTracePrintPointer(inPointer) + inType + '.' + inRoutine, inPath, inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inType: String; const inPointer: Pointer; const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer; const inResult: String);
begin
  DebugTraceExit(DebugTracePrintPointer(inPointer) + inType + '.' + inRoutine, inPath, inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inType: String; const inPointer: Pointer; const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer; const inResult: Float);
begin
  DebugTraceExit(DebugTracePrintPointer(inPointer) + inType + '.' + inRoutine, inPath, inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inType: String; const inPointer: Pointer; const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer; const inResult: TObject);
begin
  DebugTraceExit(DebugTracePrintPointer(inPointer) + inType + '.' + inRoutine, inPath, inFile, inLine, inResult);
end;

procedure DebugTraceExit(const inType: String; const inPointer: Pointer; const inRoutine: String; const inPath: String; const inFile: String; const inLine: Integer; const inResult: TTBJSVariant);
begin
  DebugTraceExit(DebugTracePrintPointer(inPointer) + inType + '.' + inRoutine, inPath, inFile, inLine, inResult);
end;

// trace messages

procedure DebugTraceMessage(const inRoutine: String; const inFile: String; const inLine: Integer; const inMessage: String);
begin
  DebugLog('TRACE [MESSAGE]' + DebugTracePrintPath('MESSAGE', '', inRoutine, inFile, inLine) + ' --- ' + inMessage);
end;

// trace messages: object

procedure DebugTraceMessage(const inObject: TObject; const inRoutine: String; const inFile: String; const inLine: Integer; const inMessage: String);
begin
  DebugTraceMessage(DebugTracePrintObjectLeft(inObject) + '.' + inRoutine, inFile, inLine, inMessage);
end;

// trace messages: custom source

procedure DebugTraceMessage(const inType: String; const inPointer: Pointer; const inRoutine: String; const inFile: String; const inLine: Integer; const inMessage: String);
begin
  DebugTraceMessage(DebugTracePrintPointer(inPointer) + inType + '.' + inRoutine, inFile, inLine, inMessage);
end;

