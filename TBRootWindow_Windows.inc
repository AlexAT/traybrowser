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

// window creation, registration and settings, included from TBRootWindow unit

function TTrayBrowserRootWindow.CreateBrowserWindow: TForm;
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  Application.CreateForm(TTrayBrowserWindow, Result);

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, Result);{$ENDIF}
end;

function TTrayBrowserRootWindow.GetBrowserWindow(const inWindowID: String): TForm;
begin
  {$IFDEF TB_TRACE_FEE_INSANE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  Result := nil;
  if WindowRegistry.ContainsKey(inWindowID) then Result := WindowRegistry[inWindowID];

  {$IFDEF TB_TRACE_FEE_INSANE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, Result);{$ENDIF}
end;

procedure TTrayBrowserRootWindow.GetBrowserWindowIdList(const inIDList: TStringList);
var
  LPair: TTBWindowPair;
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  inIDList.Clear;
  for LPair in WindowRegistry do inIDList.Add(LPair.Key);

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

function TTrayBrowserRootWindow.RegisterBrowserWindow(const inWindow: TForm): String;
var
  LGUID: TGuid;
  LWindowID: String;
  LWindow: TTrayBrowserWindow;
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  try
    repeat
      if CreateGUID(LGUID) <> 0 then Die('Failed to generate browser window GUID');
      LWindowID := LGUID.ToString;
    until (not WindowRegistry.ContainsKey(LWindowID));

    WindowRegistry.Add(LWindowID, inWindow);
    Result := LWindowID;
  except Die('Failed to register browser window'); end;

  WindowFocusList.Insert(0, LWindowID); // add window to the focus list
  if ExitRequestReceived then TTrayBrowserWindow(inWindow).ExitRequest; // exit handling: send exit request to the window just registered

  if not TTrayBrowserWindow(inWindow).FWindowSettings.Main then
  begin
    // post window creation event to the main window
    LWindow := TTrayBrowserWindow(GetBrowserWindow(MainWindowID));
    if Assigned(LWindow) then TTrayBrowserWindow(LWindow).PostJSEvent('"onwindowcreated",' + LWindow.QuoteJSString(LWindowID));
  end;

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, Result);{$ENDIF}
end;

procedure TTrayBrowserRootWindow.RelinkBrowserWindow(const inWindowID: String; const inWindow: TForm);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  if not WindowRegistry.ContainsKey(inWindowID) then
  begin
    {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'no window', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
    raise Exception.Create('Failed to find window to relink');
  end;
  WindowRegistry[inWindowID] := inWindow;

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

function TTrayBrowserRootWindow.UnregisterBrowserWindow(const inWindowID: String): Boolean;
var
  LWindow: TTrayBrowserWindow;
  i: Integer;
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  Result := False;

  // remove window from focus list
  try
    repeat
      i := WindowFocusList.IndexOf(inWindowID);
      if i >= 0 then WindowFocusList.Delete(i);
    until i < 0;
  except end;

  // remove window from registry
  LWindow := TTrayBrowserWindow(GetBrowserWindow(inWindowID));
  try
    if Assigned(LWindow) and (not LWindow.FWindowSettings.Main) then
    begin
      // post window close event to the main window
      LWindow := TTrayBrowserWindow(GetBrowserWindow(MainWindowID));
      if Assigned(LWindow) then TTrayBrowserWindow(LWindow).PostJSEvent('"onwindowclosed",' + LWindow.QuoteJSString(inWindowID));
    end;
  except end;
  WindowRegistry.Remove(inWindowID);
  Result := True;

  ClearMenuOrphans; // clear tray menu entries that belong to the closed window (and potentially other uncleared elements)
  if (not ExitRequestReceived) and (inWindowID = MainWindowID) then TerminateTrayBrowser; // exit handling: main window exited, terminate the rest
  if ExitRequestReceived then TerminateTrayBrowser; // process exit request further on window termination

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, Result);{$ENDIF}
end;

