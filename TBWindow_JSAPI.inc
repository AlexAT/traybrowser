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

// JS API, included from TBWindow unit

////////////////////////////////////////////////////////////
// main handler

procedure TTrayBrowserWindow.JSAPI(const inFunction: String; const inArgc: Integer; const inRequest: TTBKVMessage; const inReply: TTBKVMessage);
var
  LResult: TTBJSVariant;
begin
  try
    case inFunction of
      // base window commands and information
      'tbShow': LResult := jsShow(inArgc, inRequest);
      'tbHide': LResult := jsHide(inArgc, inRequest);
      'tbFocus': LResult := jsFocus(inArgc, inRequest);
      'tbClose': LResult := jsClose(inArgc, inRequest);
      'tbExit': LResult := jsExit(inArgc, inRequest);
      'tbIsVisible': LResult := jsIsVisible(inArgc, inRequest);
      'tbIsInFocus': LResult := jsIsInFocus(inArgc, inRequest);
      'tbWasPositionRestored': LResult := jsWasPositionRestored(inArgc, inRequest);

      // window sizing and positioning
      'tbGetBrowserSize': LResult := jsGetSize(inArgc, inRequest, False); // browser dots
      'tbSetBrowserSize': LResult := jsSetSize(inArgc, inRequest, False); // browser dots
      'tbGetPixelSize': LResult := jsGetSize(inArgc, inRequest, True); // pixels
      'tbSetPixelSize': LResult := jsSetSize(inArgc, inRequest, True); // pixels
      'tbGetPixelPosition': LResult := jsGetPosition(inArgc, inRequest, True, False); // pixels, monitor
      'tbSetPixelPosition': LResult := jsSetPosition(inArgc, inRequest, True, False); // pixels, monitor
      'tbGetScaledPosition': LResult := jsGetPosition(inArgc, inRequest, False, False); // browser dots, monitor
      'tbSetScaledPosition': LResult := jsSetPosition(inArgc, inRequest, False, False); // browser dots, monitor
      'tbGetDesktopPosition': LResult := jsGetPosition(inArgc, inRequest, True, True); // always pixels, desktop
      'tbSetDesktopPosition': LResult := jsSetPosition(inArgc, inRequest, True, True); // always pixels, desktop

      // window and screen information
      'tbGetMonitorBrowserSize': LResult := jsGetMonitorSize(inArgc, inRequest, False); // browser dots
      'tbGetMonitorPixelSize': LResult := jsGetMonitorSize(inArgc, inRequest, True); // pixels
      'tbGetDesktopSize': LResult := jsGetDesktopSize(inArgc, inRequest); // always pixels
      'tbGetWindowInfo': LResult := jsGetWindowInfo(inArgc, inRequest);
      'tbGetScreenInfo': LResult := jsGetScreenInfo(inArgc, inRequest);
      'tbGetColorScheme': LResult := jsGetColorScheme(inArgc, inRequest);

      // window state
      'tbMinimize': LResult := jsMinimize(inArgc, inRequest);
      'tbMaximize': LResult := jsMaximize(inArgc, inRequest);
      'tbRestore': LResult := jsRestore(inArgc, inRequest);
      'tbGetWindowState': LResult := jsGetWindowState(inArgc, inRequest);

      // custom drag region
      'tbSetCustomDragRegion': LResult := jsSetCustomDragRegion(inArgc, inRequest);

      // browser commands
      'tbStart': LResult := jsStart(inArgc, inRequest);
      'tbNavigate': LResult := jsNavigate(inArgc, inRequest);
      'tbRestartBrowser': LResult := jsRestartBrowser(inArgc, inRequest);

      // window and application properties
      'tbGetParameters': LResult := jsGetParameters(inArgc, inRequest);
      'tbGetCommandLine': LResult := jsGetCommandLine(inArgc, inRequest);
      'tbGetLoggedInUser': LResult := jsGetLoggedInUser(inArgc, inRequest);
      'tbSetTitle': LResult := jsSetTitle(inArgc, inRequest);
      'tbSetHint': LResult := jsSetHint(inArgc, inRequest); // main window only
      'tbGetApplicationSettings': LResult := jsGetApplicationSettings(inArgc, inRequest);
      'tbSetApplicationSettings': LResult := jsSetApplicationSettings(inArgc, inRequest); // main window only
      'tbGetWindowSettings': LResult := jsGetWindowSettings(inArgc, inRequest);
      'tbSetWindowSettings': LResult := jsSetWindowSettings(inArgc, inRequest);

      // tray menu and notifications
      'tbAddMenuItem': LResult := jsAddMenuItem(inArgc, inRequest);
      'tbClearMenu': LResult := jsClearMenu(inArgc, inRequest);
      'tbClearMenuAll': LResult := jsClearMenu(inArgc, inRequest); // main window only
      'tbShowBalloon': LResult := jsShowBalloon(inArgc, inRequest);
      'tbHideBalloon': LResult := jsHideBalloon(inArgc, inRequest);

      // key-value storage
      'tbReadKV': LResult := jsReadKV(inArgc, inRequest);
      'tbWriteKV': LResult := jsWriteKV(inArgc, inRequest);
      'tbDeleteKV': LResult := jsDeleteKV(inArgc, inRequest);
      'tbReadTempKV': LResult := jsReadTempKV(inArgc, inRequest);
      'tbWriteTempKV': LResult := jsWriteTempKV(inArgc, inRequest);
      'tbDeleteTempKV': LResult := jsDeleteTempKV(inArgc, inRequest);

      // process execution
      'tbOpenBrowser': LResult := jsOpenBrowser(inArgc, inRequest);
      'tbExec': LResult := jsExec(inArgc, inRequest); // internal API for both legacy Exec and native ExecAsync

      // event handling support
      'tbRegisterEvent': LResult := jsRegisterEvent(inArgc, inRequest);
      'tbClearEvent': LResult := jsClearEvent(inArgc, inRequest);

      // multiple windows API
      'tbGetWindowId': LResult := jsGetWindowID(inArgc, inRequest);
      'tbGetMainWindowId': LResult := jsGetMainWindowId(inArgc, inRequest);
      'tbIsMainWindow': LResult := jsIsMainWindow(inArgc, inRequest);
      'tbGetWindowList': LResult := jsGetWindowList(inArgc, inRequest);
      'tbCreateWindow': LResult := jsCreateWindow(inArgc, inRequest);
      'tbWindowExists': LResult := jsWindowExists(inArgc, inRequest);
      'tbWindowMessage': LResult := jsWindowMessage(inArgc, inRequest);
      'tbWindowBroadcast': LResult := jsWindowBroadcast(inArgc, inRequest);
      'tbIsAnyWindowVisible': LResult := jsIsAnyWindowVisible(inArgc, inRequest);
      'tbIsApplicationVisible': LResult := jsIsApplicationVisible(inArgc, inRequest);
      'tbIsApplicationInFocus': LResult := jsIsApplicationInFocus(inArgc, inRequest);
      'tbApplicationShow': LResult := jsApplicationShow(inArgc, inRequest);
      'tbApplicationHide': LResult := jsApplicationHide(inArgc, inRequest);
      'tbApplicationTerminate': LResult := jsApplicationTerminate(inArgc, inRequest);
      'tbGetSavedPosition': LResult := jsGetSavedPosition(inArgc, inRequest);
      'tbSetSavedPosition': LResult := jsSetSavedPosition(inArgc, inRequest);

      // multiple windows API: window API aliases for remote windows
      'tbWindowShow': jsWindowAPIAlias('tbShow', inArgc, inRequest, inReply);
      'tbWindowHide': jsWindowAPIAlias('tbHide', inArgc, inRequest, inReply);
      'tbWindowFocus': jsWindowAPIAlias('tbFocus', inArgc, inRequest, inReply);
      'tbWindowClose': jsWindowAPIAlias('tbClose', inArgc, inRequest, inReply);
      'tbWindowExit': jsWindowAPIAlias('tbExit', inArgc, inRequest, inReply);
      'tbWindowIsVisible': jsWindowAPIAlias('tbIsVisible', inArgc, inRequest, inReply, False);
      'tbWindowIsInFocus': jsWindowAPIAlias('tbIsInFocus', inArgc, inRequest, inReply, False);
      'tbWindowWasPositionRestored': jsWindowAPIAlias('tbWasPositionRestored', inArgc, inRequest, inReply, False);
      'tbWindowGetBrowserSize': jsWindowAPIAlias('tbGetBrowserSize', inArgc, inRequest, inReply, False);
      'tbWindowSetBrowserSize': jsWindowAPIAlias('tbSetBrowserSize', inArgc, inRequest, inReply);
      'tbWindowGetPixelSize': jsWindowAPIAlias('tbGetPixelSize', inArgc, inRequest, inReply, False);
      'tbWindowSetPixelSize': jsWindowAPIAlias('tbSetPixelSize', inArgc, inRequest, inReply);
      'tbWindowGetPixelPosition': jsWindowAPIAlias('tbGetPixelPosition', inArgc, inRequest, inReply, False);
      'tbWindowSetPixelPosition': jsWindowAPIAlias('tbSetPixelPosition', inArgc, inRequest, inReply);
      'tbWindowGetScaledPosition': jsWindowAPIAlias('tbGetScaledPosition', inArgc, inRequest, inReply, False);
      'tbWindowSetScaledPosition': jsWindowAPIAlias('tbSetScaledPosition', inArgc, inRequest, inReply);
      'tbWindowGetDesktopPosition': jsWindowAPIAlias('tbGetDesktopPosition', inArgc, inRequest, inReply, False);
      'tbWindowSetDesktopPosition': jsWindowAPIAlias('tbSetDesktopPosition', inArgc, inRequest, inReply);
      'tbWindowGetWindowInfo': jsWindowAPIAlias('tbGetWindowInfo', inArgc, inRequest, inReply, False);
      'tbWindowMinimize': jsWindowAPIAlias('tbMinimize', inArgc, inRequest, inReply);
      'tbWindowMaximize': jsWindowAPIAlias('tbMaximize', inArgc, inRequest, inReply);
      'tbWindowRestore': jsWindowAPIAlias('tbRestore', inArgc, inRequest, inReply);
      'tbWindowGetWindowState': jsWindowAPIAlias('tbGetWindowState', inArgc, inRequest, inReply, False);
      'tbWindowSetCustomDragRegion': jsWindowAPIAlias('tbSetCustomDragRegion', inArgc, inRequest, inReply);
      'tbWindowStart': jsWindowAPIAlias('tbStart', inArgc, inRequest, inReply);
      'tbWindowNavigate': jsWindowAPIAlias('tbNavigate', inArgc, inRequest, inReply);
      'tbWindowRestartBrowser': jsWindowAPIAlias('tbRestartBrowser', inArgc, inRequest, inReply);
      'tbWindowSetTitle': jsWindowAPIAlias('tbSetTitle', inArgc, inRequest, inReply);
      'tbWindowGetWindowSettings': jsWindowAPIAlias('tbGetWindowSettings', inArgc, inRequest, inReply);
      'tbWindowSetWindowSettings': jsWindowAPIAlias('tbSetWindowSettings', inArgc, inRequest, inReply);

      // debugging
      'tbDebugLog': LResult := jsDebugLog(inArgc, inRequest);

      // legacy API: window sizing and positioning, screen information
      'tbSetSize': LResult := jsSetSize(inArgc, inRequest, False); // browser dots
      'tbSetPosition': LResult := jsSetPosition(inArgc, inRequest, False, False); // browser dots, monitor
      'tbGetWidth': LResult := jsGetWidth(inArgc, inRequest);
      'tbGetHeight': LResult := jsGetHeight(inArgc, inRequest);
      'tbGetLeft': LResult := jsGetLeft(inArgc, inRequest);
      'tbGetTop': LResult := jsGetTop(inArgc, inRequest);
      'tbGetScreenWidth': LResult := jsGetScreenWidth(inArgc, inRequest);
      'tbGetScreenHeight': LResult := jsGetScreenHeight(inArgc, inRequest);

      // legacy API: system information and process execution
      'tbGetApplicationPath': LResult := jsGetApplicationPath(inArgc, inRequest); // main window only
      'tbGetRuntimePath': LResult := jsGetRuntimePath(inArgc, inRequest); // main window only
      'tbGetSettingsPath': LResult := jsGetSettingsPath(inArgc, inRequest); // main window only
      'tbExecSyncCheck': LResult := jsExecSyncCheck(inArgc, inRequest); // internal API for synchronous process execution

      else
      begin
        inReply.Add('ERROR', 'UNKNOWN_JS_CALL_FUNCTION');
        Exit;
      end;
    end;

    case LResult.vType of
      tbjvBoolean: if LResult.vBoolean then inReply.Add('RESULT[TRUE]', '1') else inReply.Add('RESULT[FALSE]', '0');
      tbjvInteger: inReply.Add('RESULT[INT]', IntToStr(LResult.vInteger));
      tbjvFloat: inReply.Add('RESULT[FLOAT]', FloatToStr(LResult.vFloat, TrayBrowserApplication.FormatSettings));
      tbjvString: inReply.Add('RESULT[STRING]', LResult.vString);
      tbjvJson: inReply.Add('RESULT[JSON]', LResult.vString);
    end;
  except
    on e: Exception do
    begin
      inReply.Add('ERROR', 'EXCEPTION:' + e.Message);
      {$IFDEF TB_DEBUG}TrayBrowserApplication.DebugLog('JS_CALL[' + inRequest['FUNCTION'] + ']: ' + e.Message, CEF_LOG_SEVERITY_WARNING){$ENDIF};
    end;
  end;
end;

////////////////////////////////////////////////////////////
// JS event spawning

// why not posting directly from PostJSEvent but with locks and on timer which adds latency?
// well, need to stay threadsafe here, so main thread is used for actual message posting
procedure TTrayBrowserWindow.PostJSEvent(const inEventArgs: String);
begin
  if not FWindowSettings.APIEnabled then Exit; // API is not enabled, no events available
  if FExit.Terminating or FIsBrowserRestarting then Exit; // the window is terminating or the browser is restarting, no events anymore
  if not Assigned(FWindowState.JSEventQueueLock) then Exit; // prevent corner case when this comes during window destruction

  FWindowState.JSEventQueueLock.Acquire;
  try
    if Assigned(FWindowState.JSEventQueue) then // prevent corner case when this comes during window destruction
    begin
      FWindowState.JSEventQueue.Add(inEventArgs);
      if not JSEventTimer.Enabled then JSEventTimer.Enabled := True; // start event timer
    end;
  except end;
  FWindowState.JSEventQueueLock.Release;
end;

// string escape
function TTrayBrowserWindow.QuoteJSString(const inString: String): String;
begin
  Result := '"' + StringToJSONString(inString, True) + '"';
end;

// and this one is for external DLL integration and it explains the thread safety above
procedure TTrayBrowserWindow.PostJSEventExternal(const inEventArgs: PChar); stdcall;
begin
  PostJSEvent(String(inEventArgs)); // we rely on that this effectively copies the string from PChar, so DLLs do not need to hold the string buffer further
end;

procedure TTrayBrowserWindow.JSEventTimerTimer(Sender: TObject);
begin
  if FIsBrowserRestarting then Exit; // wait up while the browser is restarting, we may still lose some events while page reloads but at least we do not attempt to push them to closing browser
  if not Assigned(FWindowState.JSEventQueueLock) then Exit; // this should never happen, but better safe than sorry

  FWindowState.JSEventQueueLock.Acquire;
  try
    if FWindowState.JSEventQueue.Count > 0 then
    begin
      if FInitialized and Assigned(Chromium) and Chromium.HasValidMainFrame then
      begin
        Chromium.ExecuteJavaScript(UnicodeString('__traybrowser.__callEvent(' + FWindowState.JSEventQueue[0] + ')'), Chromium.Browser.MainFrame.GetUrl, Chromium.Browser.MainFrame, 0);
        FWindowState.JSEventQueue.Delete(0);
      end;
    end;
    if FWindowState.JSEventQueue.Count = 0 then JSEventTimer.Enabled := False; // stop ourselves till the next event comes
  except end;
  FWindowState.JSEventQueueLock.Release;
end;

////////////////////////////////////////////////////////////
// base window commands and information

// here we suppress 'function result does not seem to be set', as if TTBJSVariant is not initialized, it is tbjvEmpty by default
{$WARN 5033 OFF}
{$WARN 5093 OFF}

function TTrayBrowserWindow.jsShow(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if FExit.Requested or TrayBrowserRootWindow.ExitRequestReceived then Exit; // do nothing if exit is requested

  if Visible and not FWindowSettings.AlwaysOnTop then FlashWindow(Handle, True); // this is Windows-specific way of indicating we want attention when we are not always on top and shown when visible
  Visible := True; // we must not grab focus on Show API call as we may just want to pop up without annoying user by switching window
  if (WindowState = wsMinimized) and FWindowSettings.RestoreOnShow then
  begin
    if FWindowState.PreMinimizeWindowState <> wsMinimized then WindowState := FWindowState.PreMinimizeWindowState else WindowState := wsNormal; // return us to the normal window state if we were minimized, handle corner case for safety
    FormWindowStateChange(Self); // yes, this is necessary, programmatic changes do not fire the event
  end;
end;

function TTrayBrowserWindow.jsHide(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  FWindowState.WasVisible := False; // if Hide comes when we are already hidden, make sure root window does not restore us on application show
  Hide;
end;

function TTrayBrowserWindow.jsFocus(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  // this is much like Show API call, but brings our window in focus
  if FExit.Requested or TrayBrowserRootWindow.ExitRequestReceived then Exit; // do nothing if exit is requested

  if not FWindowSettings.Main then TrayBrowserRootWindow.ShowApplication; // if this is not the main window, show the whole application first
  if WindowState = wsMinimized then
  begin
    if FWindowState.PreMinimizeWindowState <> wsMinimized then WindowState := FWindowState.PreMinimizeWindowState else WindowState := wsNormal; // return us to the normal window state if we were minimized, handle corner case for safety
    FormWindowStateChange(Self); // yes, this is necessary, programmatic changes do not fire the event
  end;
  Show;
  BringToFront;
  ActiveControl := CEFBrowser;
end;

function TTrayBrowserWindow.jsClose(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  // this call immediately terminates the window without any graceful OnExit notifications, available for all windows
  // if this is main window, all other windows will get graceful termination requests from the root window
  FWindowState.ExitEventNeeded := False;
  FExit.EventComplete := True;
  ExitRequest;
end;

function TTrayBrowserWindow.jsExit(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  // if this is main window, this call gracefully closes the whole application, otherwise it gracefully closes the current window or confirms the close
  if not FExit.Requested then
  begin
    // first call just requests exit
    if FWindowSettings.Main then
    begin
      // terminate the whole application
      TrayBrowserRootWindow.TerminateTrayBrowser;
    end else
    begin
      // just this window
      FExit.Requested := True;
      ExitRequest;
    end;
  end else
  begin
    // second call completes the exit request on graceful exit
    if FWindowState.ExitEventNeeded and not FExit.EventComplete then
    begin
      FExit.EventComplete := True;
      ExitRequest;
    end;
  end;
end;

function TTrayBrowserWindow.jsIsVisible(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  Result.vBoolean := Visible;
end;

////////////////////////////////////////////////////////////
// window sizing and positioning

function TTrayBrowserWindow.jsGetSize(const inArgc: Integer; const inRequest: TTBKVMessage; const inPixelSize: Boolean): TTBJSVariant;
var
  LSize: TSize;
begin
  if FRememberedPosition.Active then LSize := FRememberedPosition.BrowserSize else LSize := FBrowserSize;

  if inPixelSize then
  begin
    LSize.Width := ScaleFromBrowser(LSize.Width, IfThen(FRememberedPosition.Active, FRememberedPosition.DPI, FMonitorInfo.DPI));
    LSize.Height := ScaleFromBrowser(LSize.Height, IfThen(FRememberedPosition.Active, FRememberedPosition.DPI, FMonitorInfo.DPI));
  end;

  Result.vJSON :=
      '{'
     +  '"width":' + IntToStr(LSize.Width)
     +  ',"height":' + IntToStr(LSize.Height)
    + '}';
end;

function TTrayBrowserWindow.jsSetSize(const inArgc: Integer; const inRequest: TTBKVMessage; const inPixelSize: Boolean): TTBJSVariant;
var
  LSize: TSize;
  LDict: TTBStringDict;
  LSizeSet: Boolean;
begin
  LSizeSet := False;

  if inArgc >= 2 then
  begin
    // (width,height)
    try
      LSize.Width := MsgArgToInt(inRequest, 'ARGV[1]');
      LSize.Height := MsgArgToInt(inRequest, 'ARGV[2]');
      LSizeSet := True;
    except end;
  end;

  if (not LSizeSet) and (inArgc >= 1) and inRequest.ContainsKey('ARGV[1].TYPE') and (inRequest['ARGV[1].TYPE'] = 'OBJECT') then
  begin
    LDict := TTBStringDict.Create;
    try
      MsgArgObjectToStringDict(inRequest, 'ARGV[1]', LDict, True, False, 100, 1000);

      // {"width","height"}
      if (not LSizeSet) and LDict.ContainsKey('width') and LDict.ContainsKey('height') then
      begin
        try
          LSize.Width := StrToInt(LDict['width']);
          LSize.Height := StrToInt(LDict['height']);
          LSizeSet := True;
        except end;
      end;

      // {"w","h"}
      if (not LSizeSet) and LDict.ContainsKey('w') and LDict.ContainsKey('h') then
      begin
        try
          LSize.Width := StrToInt(LDict['w']);
          LSize.Height := StrToInt(LDict['h']);
          LSizeSet := True;
        except end;
      end;
    except end;
    FreeAndNil(LDict);
  end;

  if not LSizeSet then Exit; // invalid arguments

  Result.vBoolean := False;

  if (LSize.Width >= 1) and (LSize.Height >= 1) and inPixelSize then
  begin
    LSize.Width := Max(1, ScaleToBrowser(LSize.Width, IfThen(FRememberedPosition.Active, FRememberedPosition.DPI, FMonitorInfo.DPI)));
    LSize.Height := Max(1, ScaleToBrowser(LSize.Height, IfThen(FRememberedPosition.Active, FRememberedPosition.DPI, FMonitorInfo.DPI)));
  end;

  if (LSize.Width >= 1) and (LSize.Height >= 1) then
  begin
    FBrowserSize := LSize;
    FRememberedPosition.BrowserSize := LSize;
    PositionOurselves(False, True); // keep monitor if resizing too much
    Result.vBoolean := True;
  end;
end;

function TTrayBrowserWindow.jsGetPosition(const inArgc: Integer; const inRequest: TTBKVMessage; const inPixelSize: Boolean; const inDesktop: Boolean): TTBJSVariant;
var
  LPosition: TPoint;
begin
  if FRememberedPosition.Active then LPosition := FRememberedPosition.Window.Location else LPosition := BoundsRect.Location;

  if not inDesktop then
  begin
    if FRememberedPosition.Active then LPosition := LPosition - FRememberedPosition.Monitor.Location else LPosition := LPosition - FMonitorInfo.WorkArea.Location;
    LPosition.X := Max(0, LPosition.X);
    LPosition.Y := Max(0, LPosition.Y);
  end;

  if not inPixelSize then
  begin
    LPosition.X := ScaleToBrowser(LPosition.X, IfThen(FRememberedPosition.Active, FRememberedPosition.DPI, FMonitorInfo.DPI));
    LPosition.Y := ScaleToBrowser(LPosition.Y, IfThen(FRememberedPosition.Active, FRememberedPosition.DPI, FMonitorInfo.DPI));
  end;

  Result.vJSON :=
      '{'
     +  '"left":' + IntToStr(LPosition.X)
     +  ',"top":' + IntToStr(LPosition.Y)
    + '}';
end;

function TTrayBrowserWindow.jsSetPosition(const inArgc: Integer; const inRequest: TTBKVMessage; const inPixelSize: Boolean; const inDesktop: Boolean): TTBJSVariant;
var
  LPosition: TPoint;
  LRect: TRect;
  LDict: TTBStringDict;
  LPositionSet: Boolean;
begin
  LPositionSet := False;

  if inArgc >= 2 then
  begin
    // (left,top)
    try
      LPosition.X := MsgArgToInt(inRequest, 'ARGV[1]');
      LPosition.Y := MsgArgToInt(inRequest, 'ARGV[2]');
      LPositionSet := True;
    except end;
  end;

  if (not LPositionSet) and (inArgc >= 1) and inRequest.ContainsKey('ARGV[1].TYPE') and (inRequest['ARGV[1].TYPE'] = 'OBJECT') then
  begin
    LDict := TTBStringDict.Create;
    try
      MsgArgObjectToStringDict(inRequest, 'ARGV[1]', LDict, True, False, 100, 1000);

      // {"left","top"}
      if (not LPositionSet) and LDict.ContainsKey('left') and LDict.ContainsKey('top') then
      begin
        try
          LPosition.X := StrToInt(LDict['left']);
          LPosition.Y := StrToInt(LDict['top']);
          LPositionSet := True;
        except end;
      end;

      // {"x","y"}
      if (not LPositionSet) and LDict.ContainsKey('x') and LDict.ContainsKey('y') then
      begin
        try
          LPosition.X := StrToInt(LDict['x']);
          LPosition.Y := StrToInt(LDict['y']);
          LPositionSet := True;
        except end;
      end;
    except end;
    FreeAndNil(LDict);
  end;

  if not LPositionSet then Exit; // invalid arguments

  Result.vBoolean := False;

  if not FWindowSettings.Movable then Exit; // impossible when not movable

  if not inPixelSize then
  begin
    LPosition.X := ScaleFromBrowser(LPosition.X, IfThen(FRememberedPosition.Active, FRememberedPosition.DPI, FMonitorInfo.DPI));
    LPosition.Y := ScaleFromBrowser(LPosition.Y, IfThen(FRememberedPosition.Active, FRememberedPosition.DPI, FMonitorInfo.DPI));
  end;

  if not inDesktop then
  begin
    if (LPosition.X >= 0) and (LPosition.Y >= 0)
      and (LPosition.X < IfThen(FRememberedPosition.Active, FRememberedPosition.Monitor.Width, FMonitorInfo.WorkArea.Width))
      and (LPosition.Y < IfThen(FRememberedPosition.Active, FRememberedPosition.Monitor.Height, FMonitorInfo.WorkArea.Height)) then
    begin
      if FRememberedPosition.Active then LPosition := LPosition + FRememberedPosition.Monitor.Location else LPosition := LPosition + FMonitorInfo.WorkArea.Location;
    end else Exit; // invalid monitor position
  end;

  if (LPosition.X >= Screen.WorkAreaRect.Left) and (LPosition.Y >= Screen.WorkAreaRect.Top)
    and (LPosition.X < (Screen.WorkAreaRect.Left + Screen.WorkAreaRect.Width))
    and (LPosition.Y < (Screen.WorkAreaRect.Top + Screen.WorkAreaRect.Height)) then
  begin
    if FRememberedPosition.Active then FRememberedPosition.Window.Location := FRememberedPosition.Monitor.Location + LPosition else FRememberedPosition.Window.Location := FMonitorInfo.WorkArea.Location + LPosition;
    if (WindowState <> wsMaximized) then // moving window in maximized state actually works and gives inconsistent results, avoid that
    begin
      FInManualPositionChange := True;
      LRect := BoundsRect;
      LRect.Location := FRememberedPosition.Window.Location;
      BoundsRect := LRect;
      FInManualPositionChange := False;
    end;
    PositionOurselves;
    Result.vBoolean := True;
  end else Exit; // invalid desktop position
end;

////////////////////////////////////////////////////////////
// window and screen information

function TTrayBrowserWindow.jsGetMonitorSize(const inArgc: Integer; const inRequest: TTBKVMessage; const inPixelSize: Boolean): TTBJSVariant;
var
  LSize: TSize;
begin
  if FRememberedPosition.Active then LSize := FRememberedPosition.Monitor.Size else LSize := FMonitorInfo.WorkArea.Size;

  if not inPixelSize then
  begin
    LSize.Width := ScaleToBrowser(LSize.Width, IfThen(FRememberedPosition.Active, FRememberedPosition.DPI, FMonitorInfo.DPI));
    LSize.Height := ScaleToBrowser(LSize.Height, IfThen(FRememberedPosition.Active, FRememberedPosition.DPI, FMonitorInfo.DPI));
  end;

  Result.vJSON :=
      '{'
     +  '"width":' + IntToStr(LSize.Width)
     +  ',"height":' + IntToStr(LSize.Height)
    + '}';
end;

function TTrayBrowserWindow.jsGetDesktopSize(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  Result.vJSON :=
      '{'
     +  '"width":' + IntToStr(Screen.WorkAreaRect.Width)
     +  ',"height":' + IntToStr(Screen.WorkAreaRect.Height)
    + '}';
end;

function TTrayBrowserWindow.jsGetWindowInfo(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  // this one is faster as we do not pre-position ourselves here as we do not provide on-monitor information
  Result.vJSON := GetWindowSizePositionJSON;
end;

function TTrayBrowserWindow.jsGetScreenInfo(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  i: Integer;
  s: String;
begin
  PositionOurselves; // yes, this is slow, but take care monitor may change in the background, so retrieve new state to make sure this one is operating with relevant information

  // {
  //   window: { -- see tbGetWindowInfoAbove -- },
  //   screen: {
  //     pixelLeft, pixelTop, pixelWidth, pixelHeight,
  //     mainMonitor,
  //     trayMonitor,
  //   },
  //   monitor: {
  //     id,
  //     width, height,
  //     pixelLeft, pixelTop, pixelWidth, pixelHeight,
  //     realLeft, realTop, realWidth, realHeight
  //     dpi,
  //     scale,
  //   },
  //   monitors: [ array of monitor ]
  // }
  s :=
       '{'
    +    '"window":' + GetWindowSizePositionJSON
    +    ',"screen":{'
    +      '"pixelLeft":' + IntToStr(Screen.DesktopLeft)
    +      ',"pixelTop":' + IntToStr(Screen.DesktopTop)
    +      ',"pixelWidth":' + IntToStr(Screen.DesktopWidth)
    +      ',"pixelHeight":' + IntToStr(Screen.DesktopHeight)
    +      ',"mainMonitor":' + IntToStr(FMonitorInfo.Main)
    +      ',"trayMonitor":' + IntToStr(FMonitorInfo.Tray)
    +    '}'
    +    ',"monitor":{'
    +      '"id":' + IntToStr(FMonitorInfo.Active)
    +      ',"width":' + IntToStr(ScaleToBrowser(FMonitorInfo.WorkArea.Width, FMonitorInfo.DPI))
    +      ',"height":' + IntToStr(ScaleToBrowser(FMonitorInfo.WorkArea.Height, FMonitorInfo.DPI))
    +      ',"pixelLeft":' + IntToStr(FMonitorInfo.WorkArea.Left)
    +      ',"pixelTop":' + IntToStr(FMonitorInfo.WorkArea.Top)
    +      ',"pixelWidth":' + IntToStr(FMonitorInfo.WorkArea.Width)
    +      ',"pixelHeight":' + IntToStr(FMonitorInfo.WorkArea.Height)
    +      ',"realLeft":' + IntToStr(FMonitorInfo.RealBounds.Left)
    +      ',"realTop":' + IntToStr(FMonitorInfo.RealBounds.Top)
    +      ',"realWidth":' + IntToStr(FMonitorInfo.RealBounds.Width)
    +      ',"realHeight":' + IntToStr(FMonitorInfo.RealBounds.Height)
    +    '}'
    +    ',"monitors":[';
  for i := 0 to Screen.MonitorCount - 1 do
  begin
    if (i <> 0) then s := s + ',';
    s := s
      +    '{'
      +      '"id":' + IntToStr(Screen.Monitors[i].MonitorNum)
      +      ',"width":' + IntToStr(ScaleToBrowser(Screen.Monitors[i].WorkareaRect.Width, Screen.Monitors[i].PixelsPerInch))
      +      ',"height":' + IntToStr(ScaleToBrowser(Screen.Monitors[i].WorkareaRect.Height, Screen.Monitors[i].PixelsPerInch))
      +      ',"pixelLeft":' + IntToStr(Screen.Monitors[i].WorkareaRect.Left)
      +      ',"pixelTop":' + IntToStr(Screen.Monitors[i].WorkareaRect.Top)
      +      ',"pixelWidth":' + IntToStr(Screen.Monitors[i].WorkareaRect.Width)
      +      ',"pixelHeight":' + IntToStr(Screen.Monitors[i].WorkareaRect.Height)
      +      ',"realLeft":' + IntToStr(Screen.Monitors[i].Left)
      +      ',"realTop":' + IntToStr(Screen.Monitors[i].Top)
      +      ',"realWidth":' + IntToStr(Screen.Monitors[i].Width)
      +      ',"realHeight":' + IntToStr(Screen.Monitors[i].Height)
      +      ',"dpi":' + IntToStr(Screen.Monitors[i].PixelsPerInch)
      +      ',"scale":' + FloatToStr(Screen.Monitors[i].PixelsPerInch / 96, TrayBrowserApplication.FormatSettings)
      +    '}';
  end;
  s := s
    +    ']'
    +  '}';
  Result.vJSON := s;
end;

function TTrayBrowserWindow.jsGetColorScheme(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  s: String;
begin
  s := '"clBlack":"' + ColorToCSS(clBlack) + '"';
  s := s + ',"clMaroon":"' + ColorToCSS(clMaroon) + '"';
  s := s + ',"clGreen":"' + ColorToCSS(clGreen) + '"';
  s := s + ',"clOlive":"' + ColorToCSS(clOlive) + '"';
  s := s + ',"clNavy":"' + ColorToCSS(clNavy) + '"';
  s := s + ',"clPurple":"' + ColorToCSS(clPurple) + '"';
  s := s + ',"clTeal":"' + ColorToCSS(clTeal) + '"';
  s := s + ',"clGray":"' + ColorToCSS(clGray) + '"';
  s := s + ',"clSilver":"' + ColorToCSS(clSilver) + '"';
  s := s + ',"clRed":"' + ColorToCSS(clRed) + '"';
  s := s + ',"clLime":"' + ColorToCSS(clLime) + '"';
  s := s + ',"clYellow":"' + ColorToCSS(clYellow) + '"';
  s := s + ',"clBlue":"' + ColorToCSS(clBlue) + '"';
  s := s + ',"clFuchsia":"' + ColorToCSS(clFuchsia) + '"';
  s := s + ',"clAqua":"' + ColorToCSS(clAqua) + '"';
  s := s + ',"clLtGray":"' + ColorToCSS(clLtGray) + '"';
  s := s + ',"clDkGray":"' + ColorToCSS(clDkGray) + '"';
  s := s + ',"clWhite":"' + ColorToCSS(clWhite) + '"';

  s := s + ',"clMoneyGreen":"' + ColorToCSS(clMoneyGreen) + '"';
  s := s + ',"clSkyBlue":"' + ColorToCSS(clSkyBlue) + '"';
  s := s + ',"clCream":"' + ColorToCSS(clCream) + '"';
  s := s + ',"clMedGray":"' + ColorToCSS(clMedGray) + '"';

  s := s + ',"clScrollBar":"' + ColorToCSS(clScrollBar) + '"';
  s := s + ',"clBackground":"' + ColorToCSS(clBackground) + '"';
  s := s + ',"clActiveCaption":"' + ColorToCSS(clActiveCaption) + '"';
  s := s + ',"clInactiveCaption":"' + ColorToCSS(clInactiveCaption) + '"';
  s := s + ',"clWindow":"' + ColorToCSS(clWindow) + '"';
  s := s + ',"clWindowFrame":"' + ColorToCSS(clWindowFrame) + '"';
  s := s + ',"clMenuText":"' + ColorToCSS(clMenuText) + '"';
  s := s + ',"clWindowText":"' + ColorToCSS(clWindowText) + '"';
  s := s + ',"clCaptionText":"' + ColorToCSS(clCaptionText) + '"';
  s := s + ',"clActiveBorder":"' + ColorToCSS(clActiveBorder) + '"';
  s := s + ',"clInactiveBorder":"' + ColorToCSS(clInactiveBorder) + '"';
  s := s + ',"clAppWorkspace":"' + ColorToCSS(clAppWorkspace) + '"';
  s := s + ',"clHighlight":"' + ColorToCSS(clHighlight) + '"';
  s := s + ',"clHighlightText":"' + ColorToCSS(clHighlightText) + '"';
  s := s + ',"clBtnFace":"' + ColorToCSS(clBtnFace) + '"';
  s := s + ',"clBtnShadow":"' + ColorToCSS(clBtnShadow) + '"';
  s := s + ',"clGrayText":"' + ColorToCSS(clGrayText) + '"';
  s := s + ',"clBtnText":"' + ColorToCSS(clBtnText) + '"';
  s := s + ',"clInactiveCaptionText":"' + ColorToCSS(clInactiveCaptionText) + '"';
  s := s + ',"clBtnHighlight":"' + ColorToCSS(clBtnHighlight) + '"';
  s := s + ',"cl3DDkShadow":"' + ColorToCSS(cl3DDkShadow) + '"';
  s := s + ',"cl3DLight":"' + ColorToCSS(cl3DLight) + '"';
  s := s + ',"clInfoText":"' + ColorToCSS(clInfoText) + '"';
  s := s + ',"clInfoBk":"' + ColorToCSS(clInfoBk) + '"';

  s := s + ',"clHotLight":"' + ColorToCSS(clHotLight) + '"';
  s := s + ',"clGradientActiveCaption":"' + ColorToCSS(clGradientActiveCaption) + '"';
  s := s + ',"clGradientInactiveCaption":"' + ColorToCSS(clGradientInactiveCaption) + '"';
  s := s + ',"clMenuHighlight":"' + ColorToCSS(clMenuHighlight) + '"';
  s := s + ',"clMenuBar":"' + ColorToCSS(clMenuBar) + '"';
  s := s + ',"clForm":"' + ColorToCSS(clForm) + '"';

  s := s + ',"clColorDesktop":"' + ColorToCSS(clColorDesktop) + '"';
  s := s + ',"cl3DFace":"' + ColorToCSS(cl3DFace) + '"';
  s := s + ',"cl3DShadow":"' + ColorToCSS(cl3DShadow) + '"';
  s := s + ',"cl3DHiLight":"' + ColorToCSS(cl3DHiLight) + '"';
  s := s + ',"clBtnHiLight":"' + ColorToCSS(clBtnHiLight) + '"';

  Result.vJSON := '{' + s + '}';
end;

////////////////////////////////////////////////////////////
// window state

function TTrayBrowserWindow.jsMinimize(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if not FWindowSettings.ShowOnTaskBar then Exit; // if window is not on taskbar, there is nowhere to minimize obviously
  if not FWindowSettings.ShowOnTaskBar then Exit; // impossible if no taskbar icon
  if WindowState = wsMinimized then
  begin
    // impossible if already minimized, but indicate that
    Result.vBoolean := False;
    Exit;
  end;

  FForceWindowState := True; // ensure we do minimize even if minimize action is set to something different
  FWindowState.PreMinimizeWindowState := WindowState;
  WindowState := wsMinimized;
  FormWindowStateChange(Self); // yes, this is necessary, programmatic changes do not fire the event
  Result.vBoolean := True;
end;

function TTrayBrowserWindow.jsMaximize(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if FWindowSettings.FullScreen <> tbfsNone then Exit; // we can only change to maximized state when we have no default fullscreen mode active
  if WindowState = wsMaximized then
  begin
    // impossible if already maximized, but indicate that
    Result.vBoolean := False;
    Exit;
  end;

  FForceWindowState := True; // ensure we do maximize even if maximize action is set to something different
  WindowState := wsMaximized;
  FormWindowStateChange(Self); // yes, this is necessary, programmatic changes do not fire the event
  Result.vBoolean := True;
end;

function TTrayBrowserWindow.jsRestore(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if WindowState = wsNormal then
  begin
    // impossible if already normal, indicate that
    Result.vBoolean := False;
    Exit;
  end;

  if (WindowState = wsMinimized) and (FWindowState.PreMinimizeWindowState <> wsMinimized) then // anything besides minimize returns to normal, also handle corner case that should never happen, but better safe than sorry
    WindowState := FWindowState.PreMinimizeWindowState
  else WindowState := wsNormal;
  FormWindowStateChange(Self); // yes, this is necessary, programmatic changes do not fire the event
  Result.vBoolean := True;
end;

function TTrayBrowserWindow.jsGetWindowState(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  case WindowState of
    wsNormal: Result.vString := 'normal';
    wsMinimized: Result.vString := 'minimized';
    wsMaximized: Result.vString := 'maximized';
    else Result.vString := 'unknown';
  end;
end;

function TTrayBrowserWindow.jsIsInFocus(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  Result.vBoolean := Active;
end;

function TTrayBrowserWindow.jsWasPositionRestored(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  Result.vBoolean := FWindowState.PositionRestored;
end;

////////////////////////////////////////////////////////////
// custom drag region

function TTrayBrowserWindow.jsSetCustomDragRegion(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  LDict: TTBStringDict;
  LRect: TRect;
  LPositionSet: Boolean;
  LSizeSet: Boolean;
begin
  if (inRequest['ARGV[1].TYPE'] = 'NULL') or (inRequest['ARGV[1].TYPE'] = 'UNDEFINED') then
  begin
    // reset region
    ResetCustomDragRegion;
    Result.vBoolean := True;
    Exit;
  end;

  if (inArgc >= 4) then
  begin
    // (left,top,width,height)
    try
      LRect.Left := MsgArgToInt(inRequest, 'ARGV[1]');
      LRect.Top := MsgArgToInt(inRequest, 'ARGV[2]');
      LRect.Width := MsgArgToInt(inRequest, 'ARGV[3]');
      LRect.Height := MsgArgToInt(inRequest, 'ARGV[4]');
      LPositionSet := True;
      LSizeSet := True;
    except end;
  end;

  if ((not LPositionSet) or (not LSizeSet)) and (inArgc >= 1) and inRequest.ContainsKey('ARGV[1].TYPE') and (inRequest['ARGV[1].TYPE'] = 'OBJECT') then
  begin
    LDict := TTBStringDict.Create;
    try
      MsgArgObjectToStringDict(inRequest, 'ARGV[1]', LDict, True, False, 100, 1000);

      // {"left","top"}
      if (not LPositionSet) and LDict.ContainsKey('left') and LDict.ContainsKey('top') then
      begin
        try
          LRect.Left := StrToInt(LDict['left']);
          LRect.Top := StrToInt(LDict['top']);
          LPositionSet := True;
        except end;
      end;

      // {"x","y"}
      if (not LPositionSet) and LDict.ContainsKey('x') and LDict.ContainsKey('y') then
      begin
        try
          LRect.Left := StrToInt(LDict['x']);
          LRect.Top := StrToInt(LDict['y']);
          LPositionSet := True;
        except end;
      end;

      // {"width","height"}
      if (not LSizeSet) and LDict.ContainsKey('width') and LDict.ContainsKey('height') then
      begin
        try
          LRect.Width := StrToInt(LDict['width']);
          LRect.Height := StrToInt(LDict['height']);
          LSizeSet := True;
        except end;
      end;

      // {"w","h"}
      if (not LSizeSet) and LDict.ContainsKey('w') and LDict.ContainsKey('h') then
      begin
        try
          LRect.Width := StrToInt(LDict['w']);
          LRect.Height := StrToInt(LDict['h']);
          LSizeSet := True;
        except end;
      end;
    except end;
    FreeAndNil(LDict);
  end;

  if (not LPositionSet) or (not LSizeSet) then Exit; // invalid arguments

  Result.vBoolean := False;
  if (LRect.Left >= 0) and (LRect.Left <= 65535) and (LRect.Top >= 0) and (LRect.Top <= 65535)
    and (LRect.Width >= 0) and (LRect.Width <= 65535) and (LRect.Height >= 0) and (LRect.Height <= 65535)
  then // why not size check? being off-form is allowed as form can be resized later
  begin
    CustomDragRegion.Left := ScaleFromBrowser(LRect.Left, FMonitorInfo.DPI);
    CustomDragRegion.Top := ScaleFromBrowser(LRect.Top, FMonitorInfo.DPI);
    CustomDragRegion.Width := ScaleFromBrowser(LRect.Width, FMonitorInfo.DPI);
    CustomDragRegion.Height := ScaleFromBrowser(LRect.Height, FMonitorInfo.DPI);
    CustomDragRegion.Visible := True;
    Result.vBoolean := True;
  end else
  begin
    // invalid custom drag region means we reset one
    ResetCustomDragRegion;
  end;
end;

////////////////////////////////////////////////////////////
// browser commands

function TTrayBrowserWindow.jsStart(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  Navigate(FWindowSettings.URL);
end;

function TTrayBrowserWindow.jsNavigate(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if inArgc < 1 then Exit; // invalid arguments
  Navigate(Trim(MsgArgToStringWithNullUndef(inRequest, 'ARGV[1]', '', 64000)));
end;

function TTrayBrowserWindow.jsRestartBrowser(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  FBrowserSize := FWindowSettings.Size;
  FIsBrowserRestarting := True;
  if Assigned(Chromium) then Chromium.CloseAllBrowsers;
  PositionOurselves;
end;

////////////////////////////////////////////////////////////
// window and application properties

function TTrayBrowserWindow.jsGetParameters(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  Result.vString := FWindowSettings.Parameters;
end;

function TTrayBrowserWindow.jsGetCommandLine(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  s: String;
  i: Integer;
begin
  s := '';
  for i := 0 to TrayBrowserApplication.CommandLine.Count - 1 do
  begin
    if i <> 0 then s := s + ',';
    s := s + QuoteJSString(TrayBrowserApplication.CommandLine[i]);
  end;
  Result.vJSON := '[' + s + ']';
end;

function TTrayBrowserWindow.jsGetLoggedInUser(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  i: LongWord;
  ws: WideString = '';
begin
  i := 4096;
  SetLength(ws, i);
  if (not GetUserNameExW(2, PWideChar(ws), i)) then
    ws := ''
  else
    SetLength(ws, i);
  Result.vString := String(ws);
end;

function TTrayBrowserWindow.jsSetTitle(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if inArgc < 1 then Exit; // invalid arguments
  Caption := Trim(MsgArgToStringWithNullUndef(inRequest, 'ARGV[1]', '', 250));
end;

function TTrayBrowserWindow.jsSetHint(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if inArgc < 1 then Exit; // invalid arguments
  if not FWindowSettings.Main then Exit; // main window only
  TrayBrowserRootWindow.TrayIcon.Hint := Trim(MsgArgToStringWithNullUndef(inRequest, 'ARGV[1]', '', 250));
end;

function TTrayBrowserWindow.jsGetApplicationSettings(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  LDict: TTBStringDict;
  kv: TTBStringPair;
  s: String;
  b: Boolean;
begin
  s := '';
  TrayBrowserApplication.TempSettings := TrayBrowserApplication.Settings;
  LDict := TTBStringDict.Create;
  try
    TrayBrowserApplication.PrintSettings(LDict, [tbsefApplication], False, True);
    b := False;
    for kv in LDict do
    begin
      if b then s := s + ',' else b := True;
      s := s + QuoteJSString(kv.Key) + ':' + QuoteJSString(kv.Value);
    end;
  except end;
  FreeAndNil(LDict);
  Result.vJSON := '{' + s + '}';
end;

function TTrayBrowserWindow.jsSetApplicationSettings(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  LSettings: TTBStringDict;
  kv: TTBStringPair;
begin
  if inArgc < 1 then Exit; // invalid arguments

  Result.vBoolean := False;
  LSettings := TTBStringDict.Create;
  try
    MsgArgObjectToStringDict(inRequest, 'ARGV[1]', LSettings, True, False, 200, 100000);
    for kv in LSettings do LSettings[kv.Key] := Trim(kv.Value);
    TrayBrowserApplication.TempSettings := TrayBrowserApplication.Settings;
    TrayBrowserApplication.ApplySettings(LSettings, [tbsefApplication], True);
    TrayBrowserApplication.FixupSettings(LSettings);
    TrayBrowserApplication.Settings := TrayBrowserApplication.TempSettings;
    TrayBrowserRootWindow.ApplySettings;
    Result.vBoolean := True;
  except end;
  FreeAndNil(LSettings);
end;

function TTrayBrowserWindow.jsGetWindowSettings(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  LDict: TTBStringDict;
  kv: TTBStringPair;
  s: String;
  b: Boolean;
begin
  s := '';
  TrayBrowserApplication.TempWindowSettings := FWindowSettings;
  LDict := TTBStringDict.Create;
  try
    TrayBrowserApplication.PrintSettings(LDict, [tbsefWindow], False, True);
    b := False;
    for kv in LDict do
    begin
      if b then s := s + ',' else b := True;
      s := s + QuoteJSString(kv.Key) + ':' + QuoteJSString(kv.Value);
    end;
  except end;
  FreeAndNil(LDict);
  Result.vJSON := '{' + s + '}';
end;

function TTrayBrowserWindow.jsSetWindowSettings(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  LSettings, LDict, LDict2: TTBStringDict;
  LElement: TTBSettingElement;
  LRestartRequired: Boolean;
  kv: TTBStringPair;
  s: String;
begin
  if inArgc < 1 then Exit; // invalid arguments

  LSettings := TTBStringDict.Create;
  LDict := TTBStringDict.Create;
  LDict2 := TTBStringDict.Create;
  try
    MsgArgObjectToStringDict(inRequest, 'ARGV[1]', LSettings, True, False, 200, 100000);
    for kv in LSettings do LSettings[kv.Key] := Trim(kv.Value);

    TrayBrowserApplication.TempWindowSettings := FWindowSettings;

    for LElement in TrayBrowserApplication.SettingsList do
    begin
      if (LElement.Flags >= [tbsefWindow,tbsefRestart]) and (not (tbsefProtected in LElement.Flags)) then
      begin
        if LElement.Print(s) then
        begin
          LDict.AddOrSetValue(LElement.NameLC, s);
        end;
      end;
    end;

    if (inArgc >= 2) and MsgArgToBool(inRequest, 'ARGV[2]', True) then
    begin
      // start from defaults
      TrayBrowserApplication.SetWindowDefaults;
      if FWindowSettings.Main then TrayBrowserApplication.ApplyMainWindowDefaults(LSettings, True);

      // restore protected settings
      for LElement in TrayBrowserApplication.SettingsList do
      begin
        if LElement.Flags >= [tbsefWindow,tbsefProtected] then
        begin
          if LElement.Print(s) then
          begin
            LDict2.AddOrSetValue(LElement.NameLC, s);
          end;
        end;
      end;
      TrayBrowserApplication.ApplySettings(LDict2, [tbsefWindow]);
      LDict2.Clear; // prepare for reuse
    end;

    // apply and fixup changeable settings
    TrayBrowserApplication.ApplySettings(LSettings, [tbsefWindow], True);
    TrayBrowserApplication.FixupWindowSettings(LSettings);

    for LElement in TrayBrowserApplication.SettingsList do
    begin
      if (LElement.Flags >= [tbsefWindow,tbsefRestart]) and (not (tbsefProtected in LElement.Flags)) then
      begin
        if LElement.Print(s) then
        begin
          LDict2.AddOrSetValue(LElement.NameLC, s);
        end;
      end;
    end;

    // check if window restart is needed
    LRestartRequired := False;
    for kv in LDict do begin
      if LDict2.ContainsKey(kv.Key) and (kv.Value <> LDict2[kv.Key]) then
      begin
        // special handling for specific settings
        case kv.Key of
          'border':
          begin
            // normal and borderless modes are interchangeable without restart
            case kv.Value of
              'normal', 'borderless':
              case LDict2[kv.Key] of
                'normal', 'borderless': Continue;
              end;
            end;
          end;
        end;

        // window restart is required
        LRestartRequired := True;
        Break;
      end;
    end;

    if LRestartRequired then
    begin
      // perform window restart
      TrayBrowserApplication.TempWindowSettings.RestartSource := Self;
      TrayBrowserRootWindow.CreateBrowserWindow; // newly created window will use our settings, copy our internals, relink our browser, take our glasses and bike and then close us
      Result.vString := 'restarting';
    end else
    begin
      // just apply safe settings and reposition
      FWindowSettings := TrayBrowserApplication.TempWindowSettings;
      ApplySafeSettings;
      PositionOurselves;
      Result.vBoolean := True;
    end;
  except end;
  FreeAndNil(LDict2);
  FreeANdNil(LDict);
  FreeAndNil(LSettings);
end;

////////////////////////////////////////////////////////////
// tray menu and notifications

function TTrayBrowserWindow.jsAddMenuItem(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  i: Integer;
begin
  if inArgc < 2 then Exit; // invalid arguments

  try
    i := MsgArgToInt(inRequest, 'ARGV[1]');
    if i = 0 then raise Exception.Create('Invalid argument');
    TrayBrowserRootWindow.AddUserMenuItem(FWindowID, i, Trim(MsgArgToString(inRequest, 'ARGV[2]', 200)));
  except end;
end;

function TTrayBrowserWindow.jsClearMenu(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  TrayBrowserRootWindow.ClearUserMenu(FWindowID);
end;

function TTrayBrowserWindow.jsClearMenuAll(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if not FWindowSettings.Main then Exit; // main window only
  TrayBrowserRootWindow.ClearUserMenu('*');
end;

function TTrayBrowserWindow.jsShowBalloon(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  i: Integer;
begin
  if inArgc < 3 then Exit; // invalid arguments
  if not FWindowSettings.Main then Exit; // this one is only allowed in the main window

  TrayBrowserRootWindow.HideBalloon;
  case LowerCase(Trim(MsgArgToStringWithNullUndef(inRequest, 'ARGV[1]'))) of
    'i','info','information','informational': TrayBrowserRootWindow.TrayIcon.BalloonFlags := bfInfo;
    'w','warn','warning': TrayBrowserRootWindow.TrayIcon.BalloonFlags := bfWarning;
    'e','err','error': TrayBrowserRootWindow.TrayIcon.BalloonFlags := bfError;
    else TrayBrowserRootWindow.TrayIcon.BalloonFlags := bfNone;
  end;
  i := MsgArgToIntWithNullUndef(inRequest, 'ARGV[2]', 30);
  if (i < 1) or (i > 300) then i := 30;
  TrayBrowserRootWindow.ShowBalloon(FWindowID, Trim(MsgArgToStringWithNullUndef(inRequest, 'ARGV[3]', '', 64000)), IfThen(inRequest.ContainsKey('ARGV[4]'), Trim(MsgArgToStringWithNullUndef(inRequest, 'ARGV[4]', '', 64000))), i);
end;

function TTrayBrowserWindow.jsHideBalloon(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if not FWindowSettings.Main then Exit; // this one is only allowed in the main window
  TrayBrowserRootWindow.HideBalloon;
end;

////////////////////////////////////////////////////////////
// key-value storage

function TTrayBrowserWindow.jsReadKV(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  s: String;
begin
  if inArgc < 1 then Exit; // invalid arguments
  if TrayBrowserRootWindow.KVStoreRead('kvuser', Trim(MsgArgToString(inRequest, 'ARGV[1]', 1000)), s) then Result.vString := s;
end;

function TTrayBrowserWindow.jsWriteKV(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if inArgc < 2 then Exit; // invalid arguments
  Result.vBoolean := TrayBrowserRootWindow.KVStoreWrite('kvuser', Trim(MsgArgToString(inRequest, 'ARGV[1]', 1000)), Trim(MsgArgToStringWithNullUndef(inRequest, 'ARGV[2]', '', 16777216)));
end;

function TTrayBrowserWindow.jsDeleteKV(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if inArgc < 1 then Exit; // invalid arguments
  Result.vBoolean := TrayBrowserRootWindow.KVStoreDelete('kvuser', Trim(MsgArgToString(inRequest, 'ARGV[1]', 1000)));
end;

function TTrayBrowserWindow.jsReadTempKV(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  s: String;
begin
  if inArgc < 1 then Exit; // invalid arguments
  if TrayBrowserRootWindow.MemoryKVStoreRead(Trim(MsgArgToString(inRequest, 'ARGV[1]', 1000)), s) then Result.vString := s;
end;

function TTrayBrowserWindow.jsWriteTempKV(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if inArgc < 2 then Exit; // invalid arguments
  Result.vBoolean := TrayBrowserRootWindow.MemoryKVStoreWrite(Trim(MsgArgToString(inRequest, 'ARGV[1]', 1000)), Trim(MsgArgToStringWithNullUndef(inRequest, 'ARGV[2]', '', 16777216)));
end;

function TTrayBrowserWindow.jsDeleteTempKV(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if inArgc < 1 then Exit; // invalid arguments
  Result.vBoolean := TrayBrowserRootWindow.MemoryKVStoreDelete(Trim(MsgArgToString(inRequest, 'ARGV[1]', 1000)));
end;

////////////////////////////////////////////////////////////
// process execution

function TTrayBrowserWindow.jsOpenBrowser(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  s: String;
begin
  if inArgc < 1 then Exit; // invalid arguments

  s := Trim(MsgArgToStringWithNullUndef(inRequest, 'ARGV[1]', '', 64000));
  if (MidStr(s, 1, 7) = 'http://') or (MidStr(s, 1, 8) = 'https://') or (MidStr(s, 1, 6) = 'ftp://') or (MidStr(s, 1, 7) = 'mailto:') then
  begin
    OpenURL(s);
    Result.vBoolean := True;
  end else Result.vBoolean := False;
end;

function TTrayBrowserWindow.jsExec(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  LCallbackID, LMode, LTimeout: Integer;
  LWMode: TTBExecWindowMode;
  LCommand: String;
  LParameters: TProcessStringList;
  LAllowedToExec: Boolean;
  LRegexp: TRegExpr;
  s, s2, s3, s4: String;
  i: Integer;
begin
  if inArgc < 5 then Exit; // invalid arguments

  if (not TrayBrowserApplication.Settings.AllowExec) and (not TrayBrowserApplication.Settings.AllowSystemInfo) then
  begin
    // process execution is not allowed at all anyhow
    Result.vBoolean := False;
    Exit;
  end;

  LCallbackID := MsgArgToIntWithNullUndef(inRequest, 'ARGV[1]', -1); // <0 means no callback
  LMode := MsgArgToIntWithNullUndef(inRequest, 'ARGV[4]', 0);
  LTimeout := MsgArgToIntWithNullUndef(inRequest, 'ARGV[5]', -1); // <0 means no timeout

  LCommand := Trim(MsgArgToStringWithNullUndef(inRequest, 'ARGV[2]', '', 4000));
  if LCommand = '' then Exit;

  case (LMode and 3) of
    tbeHide: LWMode := tbewHide;
    tbeNormal: LWMode := tbewNormal;
    tbeMinimize: LWMode := tbewMinimize;
    tbeMaximize: LWMode := tbewMaximize;
  end;

  LParameters := TProcessStringList.Create;
  LParameters.Delimiter := ' ';
  LParameters.QuoteChar := '"';
  LParameters.AlwaysQuote := False;
  LParameters.StrictDelimiter := False;

  try
    // parse parameters
    if inRequest['ARGV[3].TYPE'] <> 'ARRAY' then
    begin
      s := '';
      try s := Trim(MsgArgToStringWithNullUndef(inRequest, 'ARGV[3]', '', 4000)); except end;
      if s = '' then
      begin
        // no parameters, treat the whole command as parameterized
        LParameters.DelimitedText := LCommand;
        if LParameters.Count = 0 then
        begin
          FreeAndNil(LParameters);
          Result.vBoolean := False; // failed execution
          Exit;
        end;
        LCommand := LParameters[0];
        LParameters.Delete(0);
      end else
      begin
        // parameter string
        LParameters.DelimitedText := s;
      end;
    end else
    begin
      // parameters can be array of strings
      i := 1;
      while True do
      begin
        s := 'ARGV[3][' + IntToStr(i) + ']';
        if (not inRequest.ContainsKey(s) or not inRequest.ContainsKey(s + '.TYPE')) then Break; // end of array
        LParameters.Add(MsgArgToString(inRequest, s, 4000));
        Inc(i);
      end;
    end;

    // check permissions to execute commands
    LAllowedToExec := False;

    // system information commands (each command is translated/verified and is allowed explicitly independent of AllowExec is AllowSystemInfo is true)
    if TrayBrowserApplication.Settings.AllowSystemInfo then
    begin
      case LCommand of
        '%PING%':
        begin
          // the syntax is %PING% "<host>" ["[<count>]"[ "[<timeout_ms>]" ["[<size>]"]]]
          // count, timeout_ms and size all default to system ping command settings, count is 1-100, timeout_ms is 1-10000, size is 1-65535
          if LParameters.Count > 0 then
          begin
            LAllowedToExec := True;
            LCommand := 'ping';
            s := LParameters[0];
            s2 := ''; if LParameters.Count > 1 then s2 := LParameters[1];
            s3 := ''; if LParameters.Count > 2 then s3 := LParameters[2];
            s4 := ''; if LParameters.Count > 3 then s4 := LParameters[3];
            LParameters.Clear;

            // count
            if s2 <> '' then
            begin
              try
                i := StrToInt(s2);
                if (i >= 1) and (i <= 100) then
                begin
                  LParameters.Add('-n');
                  LParameters.Add(IntToStr(i));
                end;
              except end;
            end;

            // timeout_ms
            if s3 <> '' then
            begin
              try
                i := StrToInt(s3);
                if (i >= 1) and (i <= 10000) then
                begin
                  LParameters.Add('-w');
                  LParameters.Add(IntToStr(i));
                end;
              except end;
            end;

            // size
            if s4 <> '' then
            begin
              try
                i := StrToInt(s4);
                if (i >= 1) and (i <= 65535) then
                begin
                  LParameters.Add('-l');
                  LParameters.Add(IntToStr(i));
                end;
              except end;
            end;

            LParameters.Add(s); // host
          end;
        end;

        '%TRACEROUTE%':
        begin
          // the syntax is %TRACEROUTE% "host" ["noresolve"] ["<timeout_ms>"]
          // timeout defaults to system traceroute command settings, noresolve is off by default, timeout_ms is 1-10000
          LAllowedToExec := True;
          LCommand := 'tracert';
          s := LParameters[0];
          s2 := ''; if LParameters.Count > 1 then s2 := LParameters[1];
          s3 := ''; if LParameters.Count > 2 then s3 := LParameters[2];
          LParameters.Clear;

          // noresolve
          if LowerCase(s2) = 'noresolve' then
          begin
            LParameters.Add('-d');

            // move timeout_ms parameter down
            s2 := s3;
            s3 := '';
          end;

          // timeout_ms
          if s2 <> '' then
          begin
            try
              i := StrToInt(s3);
              if (i >= 1) and (i <= 10000) then
              begin
                LParameters.Add('-w');
                LParameters.Add(IntToStr(i));
              end;
            except end;
          end;

          LParameters.Add(s); // host
        end;

        '%SYSINFO%':
        begin
          LAllowedToExec := True;
          LCommand := 'systeminfo';
          LParameters.Clear;
        end;

        '%IPINFO%':
        begin
          LAllowedToExec := True;
          LCommand := 'ipconfig';
          LParameters.Clear;
          LParameters.Add('/all');
        end;

        '%ROUTEINFO%':
        begin
          LAllowedToExec := True;
          LCommand := 'route';
          LParameters.Clear;
          LParameters.Add('print');
        end;
      end;
    end;

    // check if process execution is allowed per se
    if (not LAllowedToExec) and (not TrayBrowserApplication.Settings.AllowExec) then
    begin
      // no, goodbye
      Result.vBoolean := False;
      Exit;
    end;

    // perform commands filtering
    if (not LAllowedToExec) and ((TrayBrowserApplication.AllowExecFilter.Count <> 0) or (TrayBrowserApplication.AllowExecRegexp.Count <> 0)) then
    begin
      // execution is allowed and this is not legacy system command, but filtering is enabled, thus filter full command line
      s := LCommand + ' ' + LParameters.DelimitedText;

      if (not LAllowedToExec) and (TrayBrowserApplication.AllowExecFilter.Count <> 0) then
      begin
        for s2 in TrayBrowserApplication.AllowExecFilter do
        begin
          if LCommand = s2 then
          begin
            // direct command match (i.e. 'ping' matches 'ping' and 'ping <any parameters>' also matches 'ping' here)
            LAllowedToExec := True;
            Break;
          end;
          if s = s2 then
          begin
            // direct command with parameters match (i.e. 'ping -t' matches 'ping -t', but 'ping -t 1.2.3.4' does not match 'ping -t here')
            LAllowedToExec := True;
            Break;
          end;
          if (Length(s) > Length(s2)) and (MidStr(s, Length(s2) + 1, 1) = ' ') and (MidStr(s, 1, Length(s2)) = s2) then
          begin
            // command with parameters space-delimited prefix match (i.e. 'ping -t 1.2.3.4' matches 'ping -t', but 'ping -t1.2.3.4' does not match 'ping -t' here)
            LAllowedToExec := True;
            Break;
          end;
        end;
      end;

      if (not LAllowedToExec) and (TrayBrowserApplication.AllowExecRegexp.Count <> 0) then
      begin
        LRegexp := TRegExpr.Create;
        try
          LRegexp.ModifierStr := 'isg-m-r-x';
          for s2 in TrayBrowserApplication.AllowExecRegexp do
          begin
            try
              LRegexp.Expression := s2;
              if LRegexp.Exec(s) then
              begin
                // regexp matches
                LAllowedToExec := True;
                Break;
              end;
            except end;
          end;
        except end;
        FreeAndNil(LRegExp);
      end;
    end else LAllowedToExec := True; // no filters means all commands are allowed

    // still not allowed?
    if (not LAllowedToExec) then
    begin
      // nope
      Result.vBoolean := False;
      Exit;
    end;

    // execute process
    s := SpawnExecProcess(LCommand, LParameters, LCallbackID, LTimeout, LWMode, (LMode and tbeSync) <> 0, (LMode and tbeNoWait) <> 0);
    if s <> '' then
      Result.vString := s // return GUID
    else Result.vBoolean := False;
  except
    Result.vBoolean := False; // failed execution
  end;

  FreeAndNil(LParameters);
end;

////////////////////////////////////////////////////////////
// event handling support

function TTrayBrowserWindow.jsRegisterEvent(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if inArgc < 1 then Exit; // invalid arguments
  case LowerCase(Trim(MsgArgToStringWithNullUndef(inRequest, 'ARGV[1]'))) of
    'onexit': FWindowState.ExitEventNeeded := True; // graceful exit event is registered, honor it on exit
  end;
end;

function TTrayBrowserWindow.jsClearEvent(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if inArgc < 1 then Exit; // invalid arguments
  case LowerCase(Trim(MsgArgToStringWithNullUndef(inRequest, 'ARGV[1]'))) of
    'onexit':
    begin
      // graceful exit event deregistered
      if FWindowState.ExitEventNeeded then FExit.EventComplete := True;
      FWindowState.ExitEventNeeded := False;
    end;
  end;
end;

////////////////////////////////////////////////////////////
// multiple windows API

function TTrayBrowserWindow.jsGetWindowID(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  Result.vString := FWindowID;
end;

function TTrayBrowserWindow.jsGetMainWindowId(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  Result.vString := TrayBrowserRootWindow.MainWindowID;
end;

function TTrayBrowserWindow.jsIsMainWindow(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  Result.vBoolean := FWindowSettings.Main;
end;

function TTrayBrowserWindow.jsGetWindowList(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  LIDList: TStringList;
  i: Integer;
  s: String;
begin
  if not FWindowSettings.Main then Exit; // main window only

  LIDList := TStringList.Create;
  try
     TrayBrowserRootWindow.GetBrowserWindowIdList(LIDList);
     s := '';
     for i := 0 to LIDList.Count - 1 do
     begin
       if i <> 0 then s := s + ',';
       s := s + QuoteJSString(LIDList[i]);
     end;
     Result.vJSON := '[' + s + ']';
  except end;
  FreeAndNil(LIDList);
end;

function TTrayBrowserWindow.jsCreateWindow(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  LURL: String;
  LSettings: TTBStringDict;
  LWindow: TTrayBrowserWindow;
  kv: TTBStringPair;
begin
  if not FWindowSettings.Main then Exit; // main window only
  if TrayBrowserRootWindow.ExitRequestReceived then Exit; // do not do anything when exit is happening

  if inArgc < 1 then Exit; // invalid arguments

  LSettings := TTBStringDict.Create;
  try
    LURL :=  Trim(MsgArgToString(inRequest, 'ARGV[1]', 16384));
    if inRequest.ContainsKey('ARGV[2]') then MsgArgObjectToStringDictWithNullUndef(inRequest, 'ARGV[2]', LSettings, True, False, '', 1000, 65535);
    for kv in LSettings do LSettings[kv.Key] := Trim(kv.Value);
    TrayBrowserApplication.SetWindowDefaults;
    TrayBrowserApplication.ApplySettings(LSettings, [tbsefWindow]);
    TrayBrowserApplication.FixupWindowSettings(LSettings);
    if LURL <> '' then TrayBrowserApplication.TempWindowSettings.URL := LURL;
    LWindow := TTrayBrowserWindow(TrayBrowserRootWindow.CreateBrowserWindow);
    if Assigned(LWindow) then Result.vString := LWindow.FWindowID;
  except end;
  FreeAndNil(LSettings);
end;

function TTrayBrowserWindow.jsWindowExists(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  s: String;
  LWindow: TTrayBrowserWindow;
begin
  if inArgc < 1 then Exit; // invalid arguments

  s := Trim(MsgArgToString(inRequest, 'ARGV[1]', 128));
  Result.vBoolean := False;
  LWindow := nil;
  try LWindow := TTrayBrowserWindow(TrayBrowserRootWindow.GetBrowserWindow(s)); except end;
  if Assigned(LWindow) then Result.vBoolean := True;
end;

function TTrayBrowserWindow.jsWindowMessage(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  s: String;
  LWindow: TTrayBrowserWindow;
begin
  if inArgc < 2 then Exit; // invalid arguments

  s := Trim(MsgArgToStringWithNullUndef(inRequest, 'ARGV[1]', TrayBrowserRootWindow.MainWindowID, 128));
  if s = '' then s := TrayBrowserRootWindow.MainWindowID;
  LWindow := TTrayBrowserWindow(TrayBrowserRootWindow.GetBrowserWindow(s));
  if Assigned(LWindow) then
  begin
    LWindow.PostJSEvent('"onmessage",' + QuoteJSString(FWindowID) + ',' + QuoteJSString(MsgArgToString(inRequest, 'ARGV[2]', 1048576)));
    Result.vBoolean := True;
  end else Result.vBoolean := False;
end;

function TTrayBrowserWindow.jsWindowBroadcast(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  LIDList: TStringList;
  LWindow: TTrayBrowserWindow;
  LWithSelf: Boolean;
  i: Integer;
  s: String;
begin
  if not FWindowSettings.Main then Exit; // main window only

  if inArgc < 1 then Exit; // invalid arguments

  LWithSelf := False;
  if inRequest.ContainsKey('ARGV[2]') then
    try LWithSelf := MsgArgToBool(inRequest, 'ARGV[2]', True, False); except end;

  LIDList := TStringList.Create;
  try
     TrayBrowserRootWindow.GetBrowserWindowIdList(LIDList);
     s := '"onmessage",' + QuoteJSString(FWindowID) + ',' + QuoteJSString(MsgArgToString(inRequest, 'ARGV[1]', 1048576));
     for i := 0 to LIDList.Count - 1 do
     begin
       if LWithSelf or (LIDList[i] <> FWindowID) then
       begin
         LWindow := TTrayBrowserWindow(TrayBrowserRootWindow.GetBrowserWindow(LIDList[i]));
         if Assigned(LWindow) then LWindow.PostJSEvent(s);
       end;
     end;
  except end;
  FreeAndNil(LIDList);
end;

function TTrayBrowserWindow.jsIsAnyWindowVisible(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  Result.vBoolean := TrayBrowserRootWindow.CheckApplicationVisibility(True);
end;

function TTrayBrowserWindow.jsIsApplicationVisible(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  Result.vBoolean := TrayBrowserRootWindow.CheckApplicationVisibility;
end;

function TTrayBrowserWindow.jsIsApplicationInFocus(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  Result.vBoolean := TrayBrowserRootWindow.CheckApplicationFocus;
end;

function TTrayBrowserWindow.jsApplicationShow(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if not FWindowSettings.Main then Exit; // main window only
  if TrayBrowserRootWindow.ExitRequestReceived then Exit; // do not do anything when exit is happening
  TrayBrowserRootWindow.ShowApplication;
end;

function TTrayBrowserWindow.jsApplicationHide(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if not FWindowSettings.Main then Exit; // main window only
  if TrayBrowserRootWindow.ExitRequestReceived then Exit; // do not do anything when exit is happening
  TrayBrowserRootWindow.HideApplication;
end;

function TTrayBrowserWindow.jsApplicationTerminate(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if not FWindowSettings.Main then Exit; // main window only
  if TrayBrowserRootWindow.ExitRequestReceived then Exit; // do not do anything when exit is happening
  TrayBrowserRootWindow.TerminateTrayBrowser;
end;

function TTrayBrowserWindow.jsGetSavedPosition(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  LPositionID: String;
  LPoint: TPoint;
  LSize: TSize;
  s, s2: String;
begin
  LPositionID := FWindowSettings.PositionID;
  if inArgc >= 1 then
  begin
    LPositionID := Trim(MsgArgToStringWithNullUndef(inRequest, 'ARGV[1]', '', 1024));
  end;
  if (not FWindowSettings.Main) and (LPositionID = '') then Exit; // not a main window and no PositionID = no result

  s2 := '';

  try
    if TrayBrowserRootWindow.KVStoreRead('kvstore', 'window.px[' + LPositionID + ']', s) then
    begin
      LPoint.X := StrToInt(s);
      if TrayBrowserRootWindow.KVStoreRead('kvstore', 'window.py[' + LPositionID + ']', s) then
      begin
        LPoint.Y := StrToInt(s);
        if (LPoint.X >= -65535) and (LPoint.X <= 65535) and (LPoint.Y >= -65535) and (LPoint.Y <= 65535) then
        begin
          if (s2 <> '') then s2 := s2 + ','; // yeah, may be excessive here but make it generic
          s2 := s2 + '"left":' + IntToStr(LPoint.X) + ',"top":' + IntToStr(LPoint.Y);
        end;
      end;
    end;
  except
  end;

  try
    if TrayBrowserRootWindow.KVStoreRead('kvstore', 'window.bw[' + LPositionID + ']', s) then
    begin
      LSize.Width := StrToInt(s);
      if TrayBrowserRootWindow.KVStoreRead('kvstore', 'window.bh[' + LPositionID + ']', s) then
      begin
        LSize.Height := StrToInt(s);
        if (LSize.Width >= 1) and (LSize.Width <= 65535) and (LSize.Height >= 1) and (LSize.Height <= 65535) then
        begin
          if (s2 <> '') then s2 := s2 + ','; // yeah, may be excessive here but make it generic
          s2 := s2 + '"width":' + IntToStr(LSize.Width) + ',"height":' + IntToStr(LSize.Height);
        end;
      end;
    end;
  except
  end;

  Result.vJSON := '{' + s2 + '}';
end;

function TTrayBrowserWindow.jsSetSavedPosition(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  LPositionID: String;
  LDict: TTBStringDict;
  LPoint: TPoint;
  LSize: TSize;
  LHasPoint, LHasSize: Boolean;
begin
  if not FWindowSettings.Main then Exit; // main window only
  if inArgc < 2 then Exit;

  LPositionID := Trim(MsgArgToStringWithNullUndef(inRequest, 'ARGV[1]', '', 1024));
  LDict := TTBStringDict.Create;
  try
    MsgArgObjectToStringDict(inRequest, 'ARGV[2]', LDict, True, False, 100, 100);

    LHasPoint := False;
    if LDict.ContainsKey('left') and LDict.ContainsKey('top') then
    begin
      try
        LPoint.X := StrToInt(LDict['left']);
        LPoint.Y := StrToInt(LDict['top']);
        if (LPoint.X >= -65535) and (LPoint.X <= 65535) and (LPoint.Y >= -65535) and (LPoint.Y <= 65535) then LHasPoint := True;
      except end;
    end else if LDict.ContainsKey('x') and LDict.ContainsKey('y') then
    begin
      try
        LPoint.X := StrToInt(LDict['x']);
        LPoint.Y := StrToInt(LDict['y']);
        if (LPoint.X >= -65535) and (LPoint.X <= 65535) and (LPoint.Y >= -65535) and (LPoint.Y <= 65535) then LHasPoint := True;
      except end;
    end;

    LHasSize := False;
    if LDict.ContainsKey('width') and LDict.ContainsKey('height') then
    begin
      try
        LSize.Width := StrToInt(LDict['width']);
        LSize.Height := StrToInt(LDict['height']);
        if (LSize.Width >= 1) and (LSize.Width <= 65535) and (LSize.Height >= 1) and (LSize.Height <= 65535) then LHasSize := True;
      except end;
    end else if LDict.ContainsKey('w') and LDict.ContainsKey('h') then
    begin
      try
        LSize.Width := StrToInt(LDict['w']);
        LSize.Height := StrToInt(LDict['h']);
        if (LSize.Width >= 1) and (LSize.Width <= 65535) and (LSize.Height >= 1) and (LSize.Height <= 65535) then LHasSize := True;
      except end;
    end;

    try TrayBrowserRootWindow.KVStoreDelete('kvstore', 'window.px[' + LPositionID + ']', False); except end;
    try TrayBrowserRootWindow.KVStoreDelete('kvstore', 'window.py[' + LPositionID + ']', False); except end;
    try TrayBrowserRootWindow.KVStoreDelete('kvstore', 'window.bw[' + LPositionID + ']', False); except end;
    try TrayBrowserRootWindow.KVStoreDelete('kvstore', 'window.bh[' + LPositionID + ']', False); except end;
    try TrayBrowserRootWindow.KVStoreCommit; except end;

    if LHasPoint then
    begin
      try TrayBrowserRootWindow.KVStoreWrite('kvstore', 'window.px[' + LPositionID + ']', IntToStr(LPoint.X), False); except end;
      try TrayBrowserRootWindow.KVStoreWrite('kvstore', 'window.py[' + LPositionID + ']', IntToStr(LPoint.Y), False); except end;
      try TrayBrowserRootWindow.KVStoreCommit; except end;
    end;

    if LHasSize then
    begin
      try TrayBrowserRootWindow.KVStoreWrite('kvstore', 'window.bw[' + LPositionID + ']', IntToStr(LSize.Width), False); except end;
      try TrayBrowserRootWindow.KVStoreWrite('kvstore', 'window.bh[' + LPositionID + ']', IntToStr(LSize.Height), False); except end;
      try TrayBrowserRootWindow.KVStoreCommit; except end;
    end;

    Result.vBoolean := True;
  except end;
  FreeAndNil(LDict);
end;

////////////////////////////////////////////////////////////
// multiple windows API: window API aliasing for remote windows

procedure TTrayBrowserWindow.jsWindowAPIAlias(const inFunction: String; const inArgc: Integer; const inRequest: TTBKVMessage; const inReply: TTBKVMessage; const inMainWindowOnly: Boolean = True);
var
  LLastArgvKey, LWindowID: String;
  LWindow: TTrayBrowserWindow;
begin
  if inMainWindowOnly and (not FWindowSettings.Main) then Exit;
  if inArgc < 1 then Exit;

  try
    LLastArgvKey := 'ARGV[' + IntToStr(inArgc) + ']';
    LWindowID := Trim(MsgArgToString(inRequest, LLastArgvKey, 128));
    LWindow := TTrayBrowserWindow(TrayBrowserRootWindow.GetBrowserWindow(LWindowID));
    if Assigned(LWindow) then
    begin
      // we need to translate request removing last ARGV and reducing ARGC before marshalling
      if inRequest.ContainsKey(LLastArgvKey) then inRequest.Remove(LLastArgvKey);
      if inRequest.ContainsKey(LLastArgvKey + '.TYPE') then inRequest.Remove(LLastArgvKey + '.TYPE');
      LWindow.JSAPI(inFunction, inArgc - 1, inRequest, inReply); // marshall the call
    end else inReply.Add('ERROR', 'UNKNOWN_WINDOW [' + LWindowID + ']');
  except
    on e: Exception do
    begin
      inReply.Add('ERROR', 'EXCEPTION:' + e.Message + ' [MARSHALLING]');
      {$IFDEF TB_DEBUG}TrayBrowserApplication.DebugLog('JS_CALL[' + inRequest['FUNCTION'] + ']: ' + e.Message + ' [MARSHALLING]', CEF_LOG_SEVERITY_WARNING){$ENDIF};
    end;
  end;
end;

////////////////////////////////////////////////////////////
// debugging

function TTrayBrowserWindow.jsDebugLog(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if inArgc < 1 then Exit; // invalid arguments
  TrayBrowserApplication.DebugLog(MsgArgToStringWithNullUndef(inRequest, 'ARGV[1]'));
end;

////////////////////////////////////////////////////////////
// legacy API: window sizing and positioning, screen information

function TTrayBrowserWindow.jsGetWidth(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  Result.vInteger := IfThen(FRememberedPosition.Active, FRememberedPosition.BrowserSize.Width, FBrowserSize.Width); // browser dots
end;

function TTrayBrowserWindow.jsGetHeight(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  Result.vInteger := IfThen(FRememberedPosition.Active, FRememberedPosition.BrowserSize.Height, FBrowserSize.Height); // browser dots
end;

function TTrayBrowserWindow.jsGetLeft(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  Result.vInteger := IfThen(FRememberedPosition.Active, Max(0, ScaleToBrowser(FRememberedPosition.Window.Left - FRememberedPosition.Monitor.Left, FRememberedPosition.DPI)), Max(0, ScaleToBrowser(Left - FMonitorInfo.WorkArea.Left, FMonitorInfo.DPI))); // browser dots
end;

function TTrayBrowserWindow.jsGetTop(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  Result.vInteger := IfThen(FRememberedPosition.Active, Max(0, ScaleToBrowser(FRememberedPosition.Window.Top - FRememberedPosition.Monitor.Top, FRememberedPosition.DPI)), Max(0, ScaleToBrowser(Top - FMonitorInfo.WorkArea.Top, FMonitorInfo.DPI))); // browser dots
end;

function TTrayBrowserWindow.jsGetScreenWidth(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  Result.vInteger := IfThen(FRememberedPosition.Active, ScaleToBrowser(FRememberedPosition.Monitor.Width, FMonitorInfo.DPI), ScaleToBrowser(FMonitorInfo.WorkArea.Width, FMonitorInfo.DPI)); // browser dots
end;

function TTrayBrowserWindow.jsGetScreenHeight(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  Result.vInteger := IfThen(FRememberedPosition.Active, ScaleToBrowser(FRememberedPosition.Monitor.Height, FMonitorInfo.DPI), ScaleToBrowser(FMonitorInfo.WorkArea.Height, FMonitorInfo.DPI)); // browser dots
end;

////////////////////////////////////////////////////////////
// legacy API: system information and process execution

function TTrayBrowserWindow.jsGetApplicationPath(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if not FWindowSettings.Main then Exit; // main window only
  Result.vString := TrayBrowserApplication.AppPath;
end;

function TTrayBrowserWindow.jsGetRuntimePath(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if not FWindowSettings.Main then Exit; // main window only
  Result.vString := TrayBrowserApplication.RunPath;
end;

function TTrayBrowserWindow.jsGetSettingsPath(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
begin
  if not FWindowSettings.Main then Exit; // main window only
  Result.vString := TrayBrowserApplication.DataPath;
end;

function TTrayBrowserWindow.jsExecSyncCheck(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
var
  LGUID: String;
  LWantCode: Boolean;
  LProcess: TTBExecProcess;
  s: String;
begin
   if inArgc < 2 then Exit; // invalid arguments

  try
    LGUID := Trim(MsgArgToString(inRequest, 'ARGV[1]')); // GUID
    LWantCode := MsgArgToBool(inRequest, 'ARGV[2]'); // mode flag
    if FWindowState.ExecProcesses.ContainsKey(LGUID) then
    begin
      LProcess := FWindowState.ExecProcesses[LGUID];
      if LProcess.XTBIsSynchronous then
      begin
        if LProcess.XTBFinished then
        begin
          // build the output
          s := '';
          if LWantCode then
          begin
            if LProcess.XTBExitCode <> 0 then
            begin
              if LProcess.XTBErrorMessage = 'TIMEOUT' then s := 'T/O' else s := 'ERR'; // return T/O for timeouts, ERR for other internal errors
            end else s := IntToStr(LProcess.ExitStatus); // return process exit status
          end;
          if (s <> '') and (LProcess.XTBStdOut <> '') then s := s + #10; // insert line break after return code
          s := s + LProcess.XTBStdOut; // return the output (stderr is redirected to stdout in case of sync exec flag)
          Result.vString := s;

          // reap process
          try
            FWindowState.ExecProcesses.Remove(LGUID);
            FreeAndNil(LProcess);
          except end;

          // and we are done
          Exit;
        end else
        begin
          // still running
          Result.vBoolean := True;
          Exit;
        end;
      end;
    end;
  except end;

  // we somehow failed
  Result.vBoolean := False;
end;

// here we unsuppress 'function result does not seem to be set' back, it was suppressed as if TTBJSVariant is not initialized, it is tbjvEmpty by default
{$WARN 5033 ON}
{$WARN 5093 ON}

////////////////////////////////////////////////////////////
// helpers

function TTrayBrowserWindow.GetWindowSizePositionJSON: String;
begin
  // {
  //   width, height, // preset (via windows settings or API) width and height, this one is normally equal to width and height except when maximized (basically size to return to when restored), accurate
  //   actualWidth, actualHeight, // actual width and height in browser dots (before scaling to monitor DPI), this one should be equal (+/- 1 dot) to the window size reported by browser
  //   pixelWidth, pixelHeight, // browser width and height in real monitor pixels (after scaling to monitor DPI), roughly size*dpi/96
  //   pixelSetWidth, pixelSetHeight, // preset width and height in real monitor pixels
  //   fullPixelWidth, fullPixelHeight, // full window size in real monitor pixels, including borders and title bar - this may be incorrect when window is not visible
  //   pixelLeft, pixelTop, // actual window position on current monitor in real monitor pixels, that is the accurate position for left/top of the window border, may be slightly off active monitor when window is maximized
  //   pixelSetLeft, pixelSetTop, // preset window position on preset monitor in real monitor pixels (basically the position window will return to), accurate
  //   pixelDesktopLeft, pixelDesktopTop, // actual absolute window position on desktop in real monitor pixels, that is the accurate position for left/top of the window border, may be slightly off active monitor when window is maximized
  //   pixelDesktopSetLeft, pixelDesktopSetTop, // preset absolute window position on desktop in real monitor pixels (basically the position window will return to), accurate
  //   browserLeft, browserTop, // current window position on current monitor in browser dots, take care this is not a real screen position (real one is in pixels) and also may be slightly off active monitor when window is maximized
  //   browserSetLeft, browserSetTop, // preset window position on preset monitor in browser dots, normally same as left and top except when maximized, inaccurate the same way as left/top
  //   visible, // true if the window is currently visible
  //   focus, // true if the window is currently in focus
  //   windowState, // 'normal', 'minimized', 'maximized' or 'unknown', 'unknown' should not appear, 'minimized' and 'unknown' means inaccurate sizes/positions, 'maximized' adjusts width/height/left/top
  //   monitor, // the current monitor the window is residing on, accurate only when window is visible and 'normal'
  //   dpi, // DPI for the current monitor the window is residing on, accurate only when window is visible and 'normal'
  //   tray, // tray position for the current monitor the window is residing on, of 'topLeft', 'bottomLeft', 'topRight', 'bottomRight', accurate only when window is visible and 'normal'
  //   scale, // scale factor (dpi / 96) for the current monitor the window is residing on, accurate only when window is visible and 'normal'
  //   borderSize, // detected full border size (spacing besides browser) in real monitor pixels, accurate only when window is visible and 'normal', applies to left, right and bottom borders, but not title bar
  //   titleSize, // detected window title bar size in real monitor pixels, accurate only when window is visible and 'normal', will be equal to border size if there is no title bar, applies to window top
  //   sizeAlign,
  //   screenSizeAlign
  // }

  Result :=
      '{'
    +   '"width":' + IntToStr(IfThen(FRememberedPosition.Active, FRememberedPosition.BrowserSize.Width, FBrowserSize.Width))
    +   ',"height":' + IntToStr(IfThen(FRememberedPosition.Active, FRememberedPosition.BrowserSize.Height, FBrowserSize.Height))
    +   ',"actualWidth":' + IntToStr(ScaleToBrowser(CEFBrowser.Width, FMonitorInfo.DPI))
    +   ',"actualHeight":' + IntToStr(ScaleToBrowser(CEFBrowser.Height, FMonitorInfo.DPI))
    +   ',"pixelWidth":' + IntToStr(CEFBrowser.Width)
    +   ',"pixelHeight":' + IntToStr(CEFBrowser.Height)
    +   ',"pixelSetWidth":' + IntToStr(IfThen(FRememberedPosition.Active, FRememberedPosition.PixelBrowserSize.Width, CEFBrowser.Width))
    +   ',"pixelSetHeight":' + IntToStr(IfThen(FRememberedPosition.Active, FRememberedPosition.PixelBrowserSize.Height, CEFBrowser.Height))
    +   ',"fullPixelWidth":' + IntToStr(Width + FMonitorInfo.BorderWidth * 2) // take care our own border is already applied in height
    +   ',"fullPixelHeight":' + IntToStr(Height + FMonitorInfo.TitleHeight + FMonitorInfo.BorderWidth) // take care our own border is already applied in height
    +   ',"pixelLeft":' + IntToStr(Max(0, Left - FMonitorInfo.WorkArea.Left))
    +   ',"pixelTop":' + IntToStr(Max(0, Top - FMonitorInfo.WorkArea.Top))
    +   ',"pixelSetLeft":' + IntToStr(IfThen(FRememberedPosition.Active, Max(0, FRememberedPosition.Window.Left - FRememberedPosition.Monitor.Left), Max(0, Left - FMonitorInfo.WorkArea.Left)))
    +   ',"pixelSetTop":' + IntToStr(IfThen(FRememberedPosition.Active, Max(0, FRememberedPosition.Window.Top - FRememberedPosition.Monitor.Top), Max(0, Top -  - FMonitorInfo.WorkArea.Top)))
    +   ',"pixelDesktopLeft":' + IntToStr(Left)
    +   ',"pixelDesktopTop":' + IntToStr(Top)
    +   ',"pixelSetDesktopLeft":' + IntToStr(IfThen(FRememberedPosition.Active, FRememberedPosition.Window.Left, Left))
    +   ',"pixelSetDesktopTop":' + IntToStr(IfThen(FRememberedPosition.Active, FRememberedPosition.Window.Top, Top))
    +   ',"browserLeft":' + IntToStr(ScaleToBrowser(Max(0, Left - FMonitorInfo.WorkArea.Left), FMonitorInfo.DPI))
    +   ',"browserTop":' + IntToStr(ScaleToBrowser(Max(0, Top - FMonitorInfo.WorkArea.Top), FMonitorInfo.DPI))
    +   ',"browserSetLeft":' + IntToStr(IfThen(FRememberedPosition.Active, ScaleToBrowser(Max(0, FRememberedPosition.Window.Left - FRememberedPosition.Monitor.Left), FRememberedPosition.DPI), ScaleToBrowser(Max(0, Left - FMonitorInfo.WorkArea.Left), FMonitorInfo.DPI)))
    +   ',"browserSetTop":' + IntToStr(IfThen(FRememberedPosition.Active, ScaleToBrowser(Max(0, FRememberedPosition.Window.Top - FRememberedPosition.Monitor.Top), FRememberedPosition.DPI), ScaleToBrowser(Max(0, Top - FMonitorInfo.WorkArea.Top), FMonitorInfo.DPI)))
    +   ',"visible":' + IfThen(Visible, 'true', 'false')
    +   ',"focus":' + IfThen(Active, 'true', 'false');

  case WindowState of
    wsNormal: Result := Result + ',"windowState":"normal"';
    wsMinimized: Result := Result + ',"windowState":"minimized"';
    wsMaximized: Result := Result + ',"windowState":"maximized"';
    else Result := Result + ',"windowState":"unknown"';
  end;

  Result := Result
    +   ',"monitor":' + IntToStr(FMonitorInfo.Active);

  case FMonitorInfo.TrayPosition of
    tbtpTopLeft: Result := Result + ',"tray":"topLeft"';
    tbtpTopRight: Result := Result + ',"tray":"topRight"';
    tbtpBottomLeft: Result := Result + ',"tray":"bottomLeft"';
    tbtpBottomRight: Result := Result + ',"tray":"bottomRight"';
    else Result := Result + ',"tray":"unknown"';
  end;

  Result := Result
    +   ',"dpi":' + IntToStr(FMonitorInfo.DPI)
    +   ',"scale":' + FloatToStr(FMonitorInfo.DPI / 96, TrayBrowserApplication.FormatSettings)
    +   ',"borderSize":' + IntToStr(FMonitorInfo.BorderWidth)
    +   ',"titleSize":' + IntToStr(FMonitorInfo.TitleHeight)
    +   ',"sizeAlign":' + IntToStr(IfThen(FMonitorInfo.ScalingAlignment <> 0, Round(FMonitorInfo.ScalingAlignment * 96 / FMonitorInfo.DPI), 1))
    +   ',"screenSizeAlign":' + IntToStr(IfThen(FMonitorInfo.ScalingAlignment <> 0, Round(FMonitorInfo.ScalingAlignment), 1))
    + '}';
end;

function TTrayBrowserWindow.ColorToCSS(const inColor: TColor): String;
var
  RGB: Integer;
begin
  RGB := ColorToRGB(inColor);
  Result := '#' + LowerCase(Red(RGB).ToHexString(2)) + LowerCase(Green(RGB).ToHexString(2)) + LowerCase(Blue(RGB).ToHexString(2));
end;

////////////////////////////////////////////////////////////
// argument parsing and conversion

function TTrayBrowserWindow.MsgArgToBool(const inMessage: TTBKVMessage; const inIndex: String; const inUseLiterals: Boolean = False; const inLiteralsOnly: Boolean = False): Boolean;
begin
  Result := False;
  try
    case inMessage[inIndex + '.TYPE'] of
      'INT','BOOL': if StrToInt(inMessage[inIndex]) <> 0 then Result := True;
      'FLOAT': if StrToFloat(inMessage[inIndex]) <> 0 then Result := True;
      'STRING': Result := TrayBrowserApplication.StrArgToBool(inMessage[inIndex], inUseLiterals, inLiteralsOnly);
      'NULL', 'UNDEFINED': Result := False;
      else raise Exception.Create('Message argument ' + inIndex + ' cannot be converted to boolean');
    end;
  except
    raise Exception.Create('Message argument ' + inIndex + ' cannot be converted to boolean');
  end;
end;

function TTrayBrowserWindow.MsgArgToInt(const inMessage: TTBKVMessage; const inIndex: String): Integer;
begin
  try
    case inMessage[inIndex + '.TYPE'] of
      'STRING','INT','BOOL': Result := StrToInt(inMessage[inIndex]);
      'FLOAT': Result := Round(StrToFloat(inMessage[inIndex], TrayBrowserApplication.FormatSettings));
      else raise Exception.Create('Message argument ' + inIndex + ' cannot be converted to integer');
    end;
  except
    raise Exception.Create('Message argument ' + inIndex + ' cannot be converted to integer');
  end;
end;

function TTrayBrowserWindow.MsgArgToIntWithNullUndef(const inMessage: TTBKVMessage; const inIndex: String; const inOnNullUndefined: Integer = 0): Integer;
begin
  try
    case inMessage[inIndex + '.TYPE'] of
      'STRING','INT','BOOL': Result := StrToInt(inMessage[inIndex]);
      'FLOAT': Result := Round(StrToFloat(inMessage[inIndex], TrayBrowserApplication.FormatSettings));
      'NULL','UNDEFINED': Result := inOnNullUndefined;
      else raise Exception.Create('Message argument ' + inIndex + ' cannot be converted to integer');
    end;
  except
    raise Exception.Create('Message argument ' + inIndex + ' cannot be converted to integer');
  end;
end;

function TTrayBrowserWindow.MsgArgToFloat(const inMessage: TTBKVMessage; const inIndex: String): Float;
begin
  try
    case inMessage[inIndex + '.TYPE'] of
      'STRING','INT','BOOL','FLOAT': Result := StrToFloat(inMessage[inIndex], TrayBrowserApplication.FormatSettings);
      else raise Exception.Create('Message argument ' + inIndex + ' cannot be converted to floating point');
    end;
  except
    raise Exception.Create('Message argument ' + inIndex + ' cannot be converted to floating point');
  end;
end;

function TTrayBrowserWindow.MsgArgToFloatWithNullUndef(const inMessage: TTBKVMessage; const inIndex: String; const inOnNullUndefined: Float = 0): Float;
begin
  try
    case inMessage[inIndex + '.TYPE'] of
      'STRING','INT','BOOL','FLOAT': Result := StrToFloat(inMessage[inIndex], TrayBrowserApplication.FormatSettings);
      'NULL','UNDEFINED': Result := inOnNullUndefined;
      else raise Exception.Create('Message argument ' + inIndex + ' cannot be converted to floating point');
    end;
  except
    raise Exception.Create('Message argument ' + inIndex + ' cannot be converted to floating point');
  end;
end;

function TTrayBrowserWindow.MsgArgToString(const inMessage: TTBKVMessage; const inIndex: String; const inMaxLength: Integer = -1): String;
begin
  case inMessage[inIndex + '.TYPE'] of
    'STRING','INT','BOOL','FLOAT': Result := inMessage[inIndex];
    else raise Exception.Create('Message argument ' + inIndex + ' cannot be converted to string');
  end;
  if (inMaxLength >= 0) and (Length(Result) > inMaxLength) then raise Exception.Create('Message argument ' + inIndex + ' is too long');
end;

function TTrayBrowserWindow.MsgArgToStringWithNullUndef(const inMessage: TTBKVMessage; const inIndex: String; const inOnNullUndefined: String = ''; const inMaxLength: Integer = -1): String;
begin
  case inMessage[inIndex + '.TYPE'] of
    'STRING','INT','BOOL','FLOAT': Result := inMessage[inIndex];
    'NULL','UNDEFINED': Result := inOnNullUndefined;
    else raise Exception.Create('Message argument ' + inIndex + ' cannot be converted to string');
  end;
  if (inMaxLength >= 0) and (Length(Result) > inMaxLength) then raise Exception.Create('Message argument ' + inIndex + ' is too long');
end;

procedure TTrayBrowserWindow.MsgArgObjectToStringDict(const inMessage: TTBKVMessage; const inIndex: String; const inDict: TTBStringDict; const inLowerCaseKeys: Boolean = True; const inWithArrays: Boolean = False; const inMaxCount: Integer = -1; const inMaxLength: Integer = -1);
var
  i: Integer;
begin
  case inMessage[inIndex + '.TYPE'] of
    'OBJECT':
    begin
      if (inMaxCount >= 0) and (StrToInt(inMessage[inIndex]) > inMaxCount) then Exception.Create('Message argument ' + inIndex + ' has too many elements');
      for i := 1 to StrToInt(inMessage[inIndex]) do
        inDict.AddOrSetValue(
          IfThen(inLowerCaseKeys, LowerCase(Trim(inMessage[inIndex + '[' + IntToStr(i) + '].KEY'])), Trim(inMessage[inIndex + '[' + IntToStr(i) + '].KEY'])),
          MsgArgToString(inMessage, inIndex + '[' + IntToStr(i) + ']', inMaxLength)
        );
    end;

    'ARRAY':
    begin
      if not inWithArrays then Exception.Create('Message argument ' + inIndex + ' cannot be converted to string dictionary');
      if (inMaxCount >= 0) and (StrToInt(inMessage[inIndex]) > inMaxCount) then Exception.Create('Message argument ' + inIndex + ' has too many elements');
      for i := 1 to StrToInt(inMessage[inIndex]) do
        inDict.AddOrSetValue(IntToStr(i - 1), MsgArgToString(inMessage, inIndex + '[' + IntToStr(i) + ']', inMaxLength));
    end;

    else raise Exception.Create('Message argument ' + inIndex + ' cannot be converted to string dictionary');
  end;
end;

procedure TTrayBrowserWindow.MsgArgObjectToStringDictWithNullUndef(const inMessage: TTBKVMessage; const inIndex: String; const inDict: TTBStringDict; const inLowerCaseKeys: Boolean = True; const inWithArrays: Boolean = False; const inOnNullUndefined: String = ''; const inMaxCount: Integer = -1; const inMaxLength: Integer = -1);
var
  i: Integer;
begin
  case inMessage[inIndex + '.TYPE'] of
    'NULL','UNDEFINED': Exit; // nothing

    'OBJECT':
    begin
      if (inMaxCount >= 0) and (StrToInt(inMessage[inIndex]) > inMaxCount) then Exception.Create('Message argument ' + inIndex + ' has too many elements');
      for i := 1 to StrToInt(inMessage[inIndex]) do
        inDict.AddOrSetValue(
          IfThen(inLowerCaseKeys, LowerCase(Trim(inMessage[inIndex + '[' + IntToStr(i) + '].KEY'])), Trim(inMessage[inIndex + '[' + IntToStr(i) + '].KEY'])),
          MsgArgToStringWithNullUndef(inMessage, inIndex + '[' + IntToStr(i) + ']', inOnNullUndefined, inMaxLength)
        );
    end;

    'ARRAY':
    begin
      if not inWithArrays then Exception.Create('Message argument ' + inIndex + ' cannot be converted to string dictionary');
      if (inMaxCount >= 0) and (StrToInt(inMessage[inIndex]) > inMaxCount) then Exception.Create('Message argument ' + inIndex + ' has too many elements');
      for i := 1 to StrToInt(inMessage[inIndex]) do
        inDict.AddOrSetValue(IntToStr(i - 1), MsgArgToStringWithNullUndef(inMessage, inIndex + '[' + IntToStr(i) + ']', inOnNullUndefined, inMaxLength));
    end;

    else raise Exception.Create('Message argument ' + inIndex + ' cannot be converted to string dictionary');
  end;
end;

function TTrayBrowserWindow.IsMsgArgScalar(const inMessage: TTBKVMessage; const inIndex: String): Boolean;
begin
  if not inMessage.ContainsKey(inIndex) then raise Exception.Create('Message argument ' + inIndex + ' not found');
  if not inMessage.ContainsKey(inIndex + '.TYPE') then raise Exception.Create('Message argument ' + inIndex + ' not found');
  case inMessage[inIndex + '.TYPE'] of
    'STRING','INT','BOOL','FLOAT','NULL': Result := True;
    else Result := False;
  end;
end;

