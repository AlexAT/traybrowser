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
  if FInitialized and Assigned(Chromium) and Chromium.HasValidMainFrame and (not FExit.Requested) then
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
end;

procedure TTrayBrowserWindow.BrowserOnLoadStartMsg(var Message: TMessage);
begin
  // any page load start: hide balloon, clear user menu, reset custom drag region, unset graceful exit event handling
  TrayBrowserRootWindow.HideBalloon;
  ResetCustomDragRegion;
  KillExecProcesses;
  TrayBrowserRootWindow.ClearUserMenu(FWindowID);
  FWindowState.ExitEventNeeded := False;
end;

procedure TTrayBrowserWindow.BrowserOnRemoteLoadStartMsg(var Message: TMessage);
begin
  // non-internal (remote) page load start, clear error state
  FWindowState.InLoadError := False;
  TrayBrowserRootWindow.UpdateMenuRetryState;
end;

procedure TTrayBrowserWindow.BrowserOnLoadErrorMsg(var Message: TMessage);
begin
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
end;

////////////////////////////////////////////////////////////
// load events
// take care all of these execute under Chromium thread and thus we need to post events to the main thread instead of acting immediately

procedure TTrayBrowserWindow.ChromiumLoadStart(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType);
begin
  if FInitialized and Assigned(Chromium) and frame.IsMain then
  begin
    // take care here we wait for message completion as we must have stuff clean before accessing new page
    SendMessage(Handle, TB_MSG_BROWSER_ONLOADSTART, 0, 0);
    if LowerCase(MidStr(String(frame.Url), 1, 15)) <> 'data:text/html;' then SendMessage(Handle, TB_MSG_BROWSER_ONREMOTELOADSTART, 0, 0); // remote page load starting
  end;
end;

procedure TTrayBrowserWindow.ChromiumLoadError(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring);
begin
  if FInitialized and Assigned(Chromium) and frame.IsMain and (errorCode <> ERR_ABORTED) then
  begin
    try
      FCEFBrowserOpLock.Acquire;
      FWindowState.BrowserErrorText := String(errorText);
      FCEFBrowserOpLock.Release;
      PostMessage(Handle, TB_MSG_BROWSER_ONLOADERROR, 0, 0);
    except end;
  end;
end;

procedure TTrayBrowserWindow.ChromiumLoadEnd(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
begin
  if FInitialized and Assigned(Chromium) and frame.IsMain then
  begin
    if (httpStatusCode >= 400) and (httpStatusCode <= 599) then
    begin
      // oopsie-daisy
      FCEFBrowserOpLock.Acquire;
      FWindowState.BrowserErrorText := 'ERROR_HTTP_STATUS_CODE_' + IntToStr(httpStatusCode);
      FCEFBrowserOpLock.Release;
      PostMessage(Handle, TB_MSG_BROWSER_ONLOADERROR, 0, 0);
    end;
  end;
end;

function TTrayBrowserWindow.ServiceCheckLoadErrorState: Boolean;
begin
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
end;

////////////////////////////////////////////////////////////
// browser initialization

procedure TTrayBrowserWindow.InitializeBrowser(const inExistingBrowser: Boolean = False);
begin
  if FInitialized then Exit; // do not allow re-initialization without closing first

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
    if not(Chromium.CreateBrowser(CEFBrowser, UnicodeString(Caption), nil, FCEFBrowserParams)) then raise Exception.Create('Failed to create browser object');
  end;
end;

////////////////////////////////////////////////////////////
// browser startup and termination

procedure TTrayBrowserWindow.ChromiumAfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  if (not Assigned(Chromium)) or (not Chromium.IsSameBrowser(browser)) then Exit;
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TTrayBrowserWindow.BrowserCreatedMsg(var Message: TMessage);
begin
  CEFBrowser.UpdateSize; // this is required for browser to catch up with actual initial component sizing
  Navigate(FWindowSettings.URL); // load default page
end;

procedure TTrayBrowserWindow.ChromiumBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  if Assigned(Chromium) then
  begin
    if (Chromium.BrowserId <> 0) then Exit; // take care, BrowserId must be 0 for proper termination, meaning all in-page browsers terminated
    CEFBrowser.Chromium := nil;
  end;

  if not FIsBrowserRestarting then
  begin
    FExit.CanClose := True;
    ExitRequest;
  end else
  begin
    FInitialized := False;
    CEFBrowser.Chromium := nil;
    FreeAndNil(Chromium);
    FIsBrowserRestarting := False;
    InitializeBrowser;
  end;
end;

procedure TTrayBrowserWindow.ChromiumClose(Sender: TObject; const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
begin
  if (not Assigned(Chromium)) or (not Chromium.IsSameBrowser(browser)) then Exit;
  PostMessage(Handle, CEF_DESTROY, 0, 0);
  aAction := cbaDelay;
end;

////////////////////////////////////////////////////////////
// command and action limits

procedure TTrayBrowserWindow.ChromiumCanDownload(Sender: TObject; const browser: ICefBrowser; const url, request_method: ustring; var aResult: Boolean);
begin
  aResult := False;
  if (not Assigned(Chromium)) or (not Chromium.IsSameBrowser(browser)) then Exit;
  aResult := FWindowSettings.AllowDownload;
end;

procedure TTrayBrowserWindow.ChromiumCertificateError(Sender: TObject; const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo;
  const callback: ICefCallback; out Result: Boolean);
begin
  Result := False;
  if FWindowSettings.IgnoreCertificateErrors then
  begin
    callback.Cont;
    Result := True;
  end;
end;

procedure TTrayBrowserWindow.ChromiumBeforeDownload(Sender: TObject; const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring;
  const callback: ICefBeforeDownloadCallback; var aResult: Boolean);
begin
  aResult := False;
  if (not Assigned(Chromium)) or (not Chromium.IsSameBrowser(browser)) then Exit;

  aResult := True;
  if not FWindowSettings.AllowDownload then Exit;

  // force download dialog
  callback.Cont(UnicodeString(TrayBrowserApplication.RunPath), True);
end;

procedure TTrayBrowserWindow.ChromiumBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; popup_id: Integer; const targetUrl, targetFrameName: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings;
  var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
begin
  Result := False;
  if (not Assigned(Chromium)) or (not Chromium.IsSameBrowser(browser)) then Exit;
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]); // disallow all tabs and popup windows
end;

procedure TTrayBrowserWindow.ChromiumOpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; out Result: Boolean);
begin
  Result := False;
  if (not Assigned(Chromium)) or (not Chromium.IsSameBrowser(browser)) then Exit;
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]); // disallow all tabs and popup windows
end;

procedure TTrayBrowserWindow.ChromiumChromeCommand(Sender: TObject; const browser: ICefBrowser; command_id: Integer; disposition: TCefWindowOpenDisposition; var aResult: Boolean);
begin
  aResult := False; // allow by default
  if (not Assigned(Chromium)) or (not Chromium.IsSameBrowser(browser)) then Exit;

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
end;

procedure TTrayBrowserWindow.ChromiumPreKeyEvent(Sender: TObject; const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean; out Result: Boolean);
begin
  isKeyboardShortcut := False;
  Result := False;
  if (not Assigned(Chromium)) or (not Assigned(event)) or (not Chromium.IsSameBrowser(browser)) then Exit;

  case event^.windows_key_code of
    // trying to be as promiscuous as possible but yeah, still not 100% guaranteed to not be avoided
    // CTRL(CMD)-C (copy), CTRL(CMD)-X (cut)
    VK_C, VK_X:
    begin
      if ((event^.modifiers and (EVENTFLAG_COMMAND_DOWN or EVENTFLAG_CONTROL_DOWN)) <> 0) then
      begin
        Result := not FWindowSettings.AllowCopy;
        Exit;
      end;
    end;

    // CTRL(CMD)-V (paste)
    VK_V:
    begin
      if ((event^.modifiers and (EVENTFLAG_COMMAND_DOWN or EVENTFLAG_CONTROL_DOWN)) <> 0) then
      begin
        Result := not FWindowSettings.AllowPaste;
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
        Exit;
      end;

      if ((event^.modifiers and EVENTFLAG_SHIFT_DOWN) <> 0) then
      begin
        Result := not FWindowSettings.AllowPaste;
        Exit;
      end;
    end;

    // SHIFT-DEL (cut)
    VK_DELETE:
    begin
      if ((event^.modifiers and EVENTFLAG_SHIFT_DOWN) <> 0) then
      begin
        Result := not FWindowSettings.AllowCopy;
        Exit;
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////
// menu loop control

procedure TTrayBrowserWindow.ChromiumBeforeContextMenu(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  if (not Assigned(Chromium)) or (not Chromium.IsSameBrowser(browser)) then Exit;
  model.Clear; // disallow menus
end;

procedure TTrayBrowserWindow.WMEnterMenuLoop(var Message: TMessage);
begin
  inherited;
  if (Message.wParam = 0) and Assigned(GlobalCEFApp) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TTrayBrowserWindow.WMExitMenuLoop(var Message: TMessage);
begin
  inherited;
  if (Message.wParam = 0) and Assigned(GlobalCEFApp) then GlobalCEFApp.OsmodalLoop := False;
end;

