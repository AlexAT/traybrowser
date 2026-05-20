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

(* FULL AVAILABILITY: FEEL FREE TO REUSE PARTS OF THIS FILE CODE WITH OR WITHOUT MODIFICATIONS IN YOUR APPLICATIONS WITHOUT RETAINING THE ABOVE COPYRIGHT NOTICE IF YOU WANT *)

// browser functions, included from TBWindow unit

////////////////////////////////////////////////////////////
// navigation and content load

procedure TTrayBrowserWindow.Navigate(const inURL: String);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  if FInitialized and Assigned(Chromium) and Chromium.HasValidMainFrame and (not FExit.Requested) and (not FIsBrowserRestarting) then
  begin
    case inURL of
      TB_ABOUT_URL:
      begin
        try
          Chromium.LoadString(UnicodeString(TrayBrowserApplication.ReadResource('TRAYBROWSER_DEFAULT_HTML')), Chromium.Browser.MainFrame);
        except end;
      end;

      else Chromium.LoadURL(UnicodeString(inURL), Chromium.Browser.MainFrame);
    end;
  end;

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

procedure TTrayBrowserWindow.BrowserOnLoadStartMsg(var Message: TMessage);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  // any page load start: hide balloon, clear user menu, reset custom drag region, unset graceful exit event handling
  TrayBrowserRootWindow.HideBalloon;
  ResetCustomDragRegion;
  KillExecProcesses;
  TrayBrowserRootWindow.ClearUserMenu(FWindowID);
  FWindowState.ExitEventNeeded := False;

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

procedure TTrayBrowserWindow.BrowserOnRemoteLoadStartMsg(var Message: TMessage);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  // non-internal (remote) page load start, clear error state
  FWindowState.InLoadError := False;
  TrayBrowserRootWindow.UpdateMenuRetryState;

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

procedure TTrayBrowserWindow.BrowserOnLoadErrorMsg(var Message: TMessage);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  // page load error
  TrayBrowserRootWindow.UpdateMenuRetryState;

  TrayBrowserRootWindow.HideBalloon;
  ResetCustomDragRegion;
  KillExecProcesses;
  TrayBrowserRootWindow.ClearUserMenu(FWindowID);

  FWindowState.ExitEventNeeded := False;
  TrayBrowserSynchronizeWidgetSet;

  FWindowState.RetryTimer := TrayBrowserApplication.Settings.RetryInterval * 1000;
  FWindowState.InLoadError := True;
  ServiceTimer.Enabled := True; // enable service timer that handles retries

  if FWindowSettings.LoadErrorPage = tbleInternal then
  begin
     FWindowState.ErrorPageLoadNeeded := True; // error page loading is handled by service timer as if we do it immediately, sometimes Chromium is busy and cannot process the loading request
     ServiceTimer.Enabled := True; // enable service timer that handles our loading
  end;

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

////////////////////////////////////////////////////////////
// load events
// take care all of these execute under Chromium thread and thus we need to post events to the main thread instead of acting immediately

procedure TTrayBrowserWindow.ChromiumLoadStart(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  if (not FInitialized) or (not Assigned(Chromium)) or (not Chromium.IsSameBrowser(browser)) or (not frame.IsMain) then
  begin
    {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'not eligible', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
    Exit;
  end;

  // take care here we wait for message completion as we must have stuff clean before accessing new page
  SendMessage(Handle, TB_MSG_BROWSER_ONLOADSTART, 0, 0);
  if LowerCase(MidStr(String(frame.Url), 1, 15)) <> 'data:text/html;' then SendMessage(Handle, TB_MSG_BROWSER_ONREMOTELOADSTART, 0, 0); // remote page load starting

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

procedure TTrayBrowserWindow.ChromiumLoadError(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  if FInitialized and Assigned(Chromium) and frame.IsMain and (errorCode <> ERR_ABORTED) then
  begin
    try
      FCEFBrowserOpLock.Acquire;
      FWindowState.BrowserErrorText := String(errorText);
      FCEFBrowserOpLock.Release;
      PostMessage(Handle, TB_MSG_BROWSER_ONLOADERROR, 0, 0);
    except end;
  end;

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

procedure TTrayBrowserWindow.ChromiumLoadEnd(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  if (not FInitialized) or (not Assigned(Chromium)) or (not Chromium.IsSameBrowser(browser)) or (not frame.IsMain) then
  begin
    {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'not eligible', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
    Exit;
  end;

  if (httpStatusCode >= 400) and (httpStatusCode <= 599) then
  begin
    // oopsie-daisy
    FCEFBrowserOpLock.Acquire;
    FWindowState.BrowserErrorText := 'ERROR_HTTP_STATUS_CODE_' + IntToStr(httpStatusCode);
    FCEFBrowserOpLock.Release;
    PostMessage(Handle, TB_MSG_BROWSER_ONLOADERROR, 0, 0);
  end;

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

function TTrayBrowserWindow.ServiceCheckLoadErrorState: Boolean;
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  Result := False;
  // error page loading, yes, this needs timer delay as it can arrive while some internal error processing in Chromium is still incomplete
  if FWindowState.ErrorPageLoadNeeded then
  begin
    if Assigned(Chromium) then
    begin
      if not Chromium.IsLoading then
      begin
        Chromium.LoadString(UnicodeString(ReplaceStr(TrayBrowserApplication.ReadResource('TRAYBROWSER_ERROR_HTML'), '%ERROR%', EscapeHTML(FWindowState.BrowserErrorText))), Chromium.Browser.MainFrame);
        FWindowState.ErrorPageLoadNeeded := False;
      end;
    end else FWindowState.ErrorPageLoadNeeded := False;
    if FWindowState.ErrorPageLoadNeeded then Result := True; // do not disable service timer if we are still waiting
  end else if FWindowState.RetryTimer >= 0 then
  begin
    // retry handling
    FWindowState.RetryTimer := FWindowState.RetryTimer - ServiceTimer.Interval;
    if FWindowState.RetryTimer <= 0 then
    begin
      FWindowState.RetryTimer := -1;
      FWindowState.InLoadError := False;
      TrayBrowserRootWindow.UpdateMenuRetryState;
      Navigate(FWindowSettings.URL);
    end else Result := True; // do not disable service timer if we are still waiting
  end;

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, Result);{$ENDIF}
end;

////////////////////////////////////////////////////////////
// browser initialization

procedure TTrayBrowserWindow.InitializeBrowser(const inExistingBrowser: Boolean = False);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  if FInitialized then
  begin
    // do not allow re-initialization without closing first
    {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'already initialized', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
    Exit;
  end;

  // create TChromium component (can be skipped i.e. for window restarts) and assign events
  if not inExistingBrowser then Chromium := TChromium.Create(nil);
  Chromium.OnAfterCreated := @ChromiumAfterCreated;
  Chromium.OnBeforeClose := @ChromiumBeforeClose;
  Chromium.OnBeforeContextMenu := @ChromiumBeforeContextMenu;
  Chromium.OnBeforeDownload := @ChromiumBeforeDownload;
  Chromium.OnBeforePopup := @ChromiumBeforePopup;
  Chromium.OnCanDownload := @ChromiumCanDownload;
  Chromium.OnCertificateError := @ChromiumCertificateError;
  Chromium.OnChromeCommand := @ChromiumChromeCommand;
  Chromium.OnClose := @ChromiumClose;
  Chromium.OnLoadEnd := @ChromiumLoadEnd;
  Chromium.OnLoadError := @ChromiumLoadError;
  Chromium.OnLoadStart := @ChromiumLoadStart;
  Chromium.OnOpenUrlFromTab := @ChromiumOpenUrlFromTab;
  Chromium.OnPreKeyEvent := @ChromiumPreKeyEvent;
  Chromium.OnShowPermissionPrompt := @ChromiumOnShowPermissionPrompt;

  // associate TChromium component with parent window component
  CEFBrowser.Chromium := Chromium;
  Chromium.SetNewBrowserParent(CEFBrowser.Handle);
  CEFBrowser.UpdateSize;

  // initialize browser
  if not inExistingBrowser then
  begin
    FCEFBrowserParams := TCefDictionaryValueRef.New;
    FCEFBrowserParams.SetString('_TBWindowID', UnicodeString(FWindowID));
    Chromium.MultiBrowserMode := False;
    Chromium.DefaultUrl := '';
    Chromium.Options.ChromeStatusBubble := STATE_DISABLED;
    Chromium.Options.ChromeZoomBubble := STATE_DISABLED;
    if not (Chromium.CreateBrowser(CEFBrowser, UnicodeString(Caption), nil, FCEFBrowserParams)) then
    begin
      {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'failed to create browser', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
      raise Exception.Create('Failed to create browser object');
    end;
  end;

  // done
  FInitialized := True;

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

////////////////////////////////////////////////////////////
// browser startup and termination

procedure TTrayBrowserWindow.ChromiumAfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  if (not Assigned(Chromium)) or (not Chromium.IsSameBrowser(browser)) then
  begin
    {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'not eligible', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
    Exit;
  end;

  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

procedure TTrayBrowserWindow.BrowserCreatedMsg(var Message: TMessage);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  PositionOurselves; // make sure our window positioning is correct
  CEFBrowser.UpdateSize; // this is required for browser to catch up with actual initial component sizing
  if FWindowSettings.Main and not FIsWindowRestarted and TrayBrowserApplication.Settings.ClearCache then Chromium.ClearHttpCache(False); // reduce application footprint by clearing cache when main window starts for the first time
  Navigate(FWindowSettings.URL); // load default page

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

procedure TTrayBrowserWindow.BrowserBeforeCloseMsg(var Message: TMessage);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  CEFBrowser.Chromium := nil;
  if not FIsBrowserRestarting then
  begin
    FExit.CanClose := True;
    ExitRequest;
  end else
  begin
    FInitialized := False;
    FreeAndNil(Chromium);
    FIsBrowserRestarting := False;
    InitializeBrowser;
  end;

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

procedure TTrayBrowserWindow.ChromiumBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  if (not Assigned(Chromium)) or (Chromium.BrowserId <> 0) then
  begin
    // take care, BrowserId must be 0 for proper termination, meaning all in-page browsers terminated
    {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'not eligible', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
    Exit;
  end;

  PostMessage(Handle, CEF_BEFORECLOSE, 0, 0);

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

procedure TTrayBrowserWindow.ChromiumClose(Sender: TObject; const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  if (not Assigned(Chromium)) or (not Chromium.IsSameBrowser(browser)) then
  begin
    {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'not eligible', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
    Exit;
  end;

  PostMessage(Handle, CEF_DESTROY, 0, 0);
  aAction := cbaDelay;

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

////////////////////////////////////////////////////////////
// command and action limits

procedure TTrayBrowserWindow.ChromiumCanDownload(Sender: TObject; const browser: ICefBrowser; const url, request_method: ustring; var aResult: Boolean);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  aResult := False;

  if (not Assigned(Chromium)) or (not Chromium.IsSameBrowser(browser)) then
  begin
    {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'not eligible', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, aResult);{$ENDIF}
    Exit;
  end;

  aResult := FWindowSettings.AllowDownload;

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, aResult);{$ENDIF}
end;

procedure TTrayBrowserWindow.ChromiumCertificateError(Sender: TObject; const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo;
  const callback: ICefCallback; out Result: Boolean);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  Result := False;
  if FWindowSettings.IgnoreCertificateErrors then
  begin
    callback.Cont;
    Result := True;
  end;

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, Result);{$ENDIF}
end;

procedure TTrayBrowserWindow.ChromiumBeforeDownload(Sender: TObject; const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring;
  const callback: ICefBeforeDownloadCallback; var aResult: Boolean);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  aResult := False;

  if (not Assigned(Chromium)) or (not Chromium.IsSameBrowser(browser)) then
  begin
    {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'not eligible', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, aResult);{$ENDIF}
    Exit;
  end;

  aResult := True;

  if not FWindowSettings.AllowDownload then
  begin
    {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'not allowed', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, aResult);{$ENDIF}
    Exit;
  end;

  // force download dialog
  callback.Cont(UnicodeString(TrayBrowserApplication.RunPath), True);

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, aResult);{$ENDIF}
end;

procedure TTrayBrowserWindow.ChromiumBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; popup_id: Integer; const targetUrl, targetFrameName: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings;
  var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  Result := False;

  if (not Assigned(Chromium)) or (not Chromium.IsSameBrowser(browser)) then
  begin
    {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'not eligible', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, Result);{$ENDIF}
    Exit;
  end;

  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]); // disallow all tabs and popup windows

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, Result);{$ENDIF}
end;

procedure TTrayBrowserWindow.ChromiumOpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; out Result: Boolean);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  Result := False;

  if (not Assigned(Chromium)) or (not Chromium.IsSameBrowser(browser)) then
  begin
    {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'not eligible', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, Result);{$ENDIF}
    Exit;
  end;

  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]); // disallow all tabs and popup windows

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

procedure TTrayBrowserWindow.ChromiumChromeCommand(Sender: TObject; const browser: ICefBrowser; command_id: Integer; disposition: TCefWindowOpenDisposition; var aResult: Boolean);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  aResult := False; // allow by default

  if (not Assigned(Chromium)) or (not Chromium.IsSameBrowser(browser)) then
  begin
    {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'not eligible', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, aResult);{$ENDIF}
    Exit;
  end;

  case command_id of
    IDC_CLOSE_WINDOW:
    begin
      if not FExit.Requested then
      begin
        // request close operation
        aResult := True;
        PostMessage(Handle, WM_CLOSE, 0, 0);
      end;
    end;

    IDC_RELOAD,IDC_RELOAD_BYPASSING_CACHE,IDC_RELOAD_CLEARING_CACHE: aResult := not FWindowSettings.AllowReload;
    IDC_FIND,IDC_FIND_NEXT,IDC_FIND_PREVIOUS,IDC_CLOSE_FIND_OR_STOP: aResult := not FWindowSettings.AllowSearch;
    IDC_CUT,IDC_COPY: aResult := not FWindowSettings.AllowCopy;
    IDC_PASTE: aResult := not FWindowSettings.AllowPaste;
    IDC_PRINT: aResult := not FWindowSettings.AllowPrint;
    IDC_FOCUS_WEB_CONTENTS_PANE: aResult := False;

    IDC_DEV_TOOLS, IDC_DEV_TOOLS_CONSOLE, IDC_DEV_TOOLS_INSPECT, IDC_DEV_TOOLS_TOGGLE:
    {$IFNDEF TB_DEBUG}if not TrayBrowserApplication.Settings.AllowDebug then aResult := True{$ENDIF};

    else aResult := True; // disallow everything else
  end;

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, aResult);{$ENDIF}
end;

procedure TTrayBrowserWindow.ChromiumPreKeyEvent(Sender: TObject; const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean; out Result: Boolean);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  isKeyboardShortcut := False;
  Result := False;

  if (not Assigned(Chromium)) or (not Assigned(event)) or (not Chromium.IsSameBrowser(browser)) then
  begin
    {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'not eligible', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, Result);{$ENDIF}
    Exit;
  end;

  case event^.windows_key_code of
    // trying to be as promiscuous as possible but yeah, still not 100% guaranteed to not be avoided
    // CTRL(CMD)-C (copy), CTRL(CMD)-X (cut)
    VK_C, VK_X:
    begin
      if ((event^.modifiers and (EVENTFLAG_COMMAND_DOWN or EVENTFLAG_CONTROL_DOWN)) <> 0) then
      begin
        Result := not FWindowSettings.AllowCopy;
        {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'copy', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, Result);{$ENDIF}
        Exit;
      end;
    end;

    // CTRL(CMD)-V (paste)
    VK_V:
    begin
      if ((event^.modifiers and (EVENTFLAG_COMMAND_DOWN or EVENTFLAG_CONTROL_DOWN)) <> 0) then
      begin
        Result := not FWindowSettings.AllowPaste;
        {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'paste', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, Result);{$ENDIF}
        Exit;
      end;
    end;

    // CTRL(CMD)-INS (copy), SHIFT-INS (paste), CTRL(CMD)-SHIFT-INS (whatever)
    VK_INSERT:
    begin
      if ((event^.modifiers and (EVENTFLAG_COMMAND_DOWN or EVENTFLAG_CONTROL_DOWN)) <> 0) then
      begin
        if ((event^.modifiers and EVENTFLAG_SHIFT_DOWN) <> 0) then
        begin
          Result := not (FWindowSettings.AllowCopy and FWindowSettings.AllowPaste);
        end;

        Result := not FWindowSettings.AllowCopy;
        {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'copy-alt', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, Result);{$ENDIF}
        Exit;
      end;

      if ((event^.modifiers and EVENTFLAG_SHIFT_DOWN) <> 0) then
      begin
        Result := not FWindowSettings.AllowPaste;
        {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'paste-alt', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, Result);{$ENDIF}
        Exit;
      end;
    end;

    // SHIFT-DEL (cut)
    VK_DELETE:
    begin
      if ((event^.modifiers and EVENTFLAG_SHIFT_DOWN) <> 0) then
      begin
        Result := not FWindowSettings.AllowCopy;
        {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'cut', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%}, Result);{$ENDIF}
        Exit;
      end;
    end;
  end;

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

procedure TTrayBrowserWindow.ChromiumOnShowPermissionPrompt(Sender: TObject; const browser: ICefBrowser; prompt_id: uint64; const requesting_origin: ustring; requested_permissions: cardinal; const callback: ICefPermissionPromptCallback; var aResult: Boolean);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  aResult := False;
  if (requested_permissions and CEF_PERMISSION_TYPE_NOTIFICATIONS) <> 0 then
  begin
    callback.Cont(CEF_PERMISSION_RESULT_ACCEPT);
    aResult := True;
  end;

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

////////////////////////////////////////////////////////////
// menu loop control

procedure TTrayBrowserWindow.ChromiumBeforeContextMenu(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  if (not Assigned(Chromium)) or (not Chromium.IsSameBrowser(browser)) then
  begin
    {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, 'not eligible', {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
    Exit;
  end;

  model.Clear; // disallow menus

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

procedure TTrayBrowserWindow.WMEnterMenuLoop(var Message: TMessage);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  inherited;
  if (Message.wParam = 0) and Assigned(GlobalCEFApp) then GlobalCEFApp.OsmodalLoop := True;

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

procedure TTrayBrowserWindow.WMExitMenuLoop(var Message: TMessage);
begin
  {$IFDEF TB_TRACE_FEE}DebugTraceEnter(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}

  inherited;
  if (Message.wParam = 0) and Assigned(GlobalCEFApp) then GlobalCEFApp.OsmodalLoop := False;

  {$IFDEF TB_TRACE_FEE}DebugTraceExit(Self, {$INCLUDE %CURRENTROUTINE%}, {$INCLUDE %FILE%}, {$INCLUDE %LINENUM%});{$ENDIF}
end;

