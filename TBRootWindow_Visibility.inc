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

// global application visibility and focus control, included from TBRootWindow unit

procedure TTrayBrowserRootWindow.ShowApplication(const inGetFocus: Boolean = True);
var
  LWindow: TTrayBrowserWindow;
  LWindowList, LWindowList2: TStringList;
  i, j: Integer;
begin
  if ExitRequestReceived then Exit; // do not show any application windows when exiting

  FocusTimer.Enabled := False; // stop focus lost timer
  ApplicationInFocus := True; // application gets focus after that

  ApplicationChangingVisibility := True; // prevent updating focus list when massively changing visibility
  LWindowList := TStringList.Create;
  try
    // this may look kind of excessive, but this ensures windows reappear exactly in the same order on top of each other
    LWindowList2 := TStringList.Create;
    try
      GetBrowserWindowIdList(LWindowList2); // get full window list
      for i := 0 to WindowFocusList.Count - 1 do
      begin
        LWindowList.Add(WindowFocusList[i]);
        j := LWindowList2.IndexOf(WindowFocusList[i]);
        if j >= 0 then LWindowList2.Delete(j);
      end;
      for i := 0 to LWindowList2.Count - 1 do LWindowList.Insert(0, LWindowList2[i]); // put all unknown (yet not focused) windows to back (first)
    except end;
    FreeAndNil(LWindowList2);

    if not CheckApplicationVisibility then
    begin
      // something is not visible, so recover all our windows that were shown in prior from hiding
      for i := 0 to LWindowList.Count - 1 do
      begin
        LWindow := TTrayBrowserWindow(GetBrowserWindow(LWindowList[i]));
        if Assigned(LWindow) and (not LWindow.FWindowSettings.KeepHidden) and LWindow.FWindowState.WasVisible then
        begin
          if inGetFocus then LWindow.Show else LWindow.Visible := True;
          if inGetFocus then LWindow.BringToFront;
        end;
      end;
    end;

    if not CheckApplicationVisibility(True) then
    begin
      // still totally nothing is shown, we need to either show main window or everything
      LWindow := TTrayBrowserWindow(GetBrowserWindow(MainWindowID));
      if Assigned(LWindow) then
      begin
        if not LWindow.FWindowSettings.KeepHidden then
        begin
          // normal inner workings: show main window only
          if inGetFocus then LWindow.Show else LWindow.Visible := True;
          if inGetFocus then LWindow.BringToFront;
        end else
        begin
          // main window is set to be kept hidden, so show all the rest that is not
          for i := 0 to LWindowList.Count - 1 do
          begin
            LWindow := TTrayBrowserWindow(GetBrowserWindow(LWindowList[i]));
            if Assigned(LWindow) and (not LWindow.FWindowSettings.Main) and (not LWindow.FWindowSettings.KeepHidden) then
            begin
              if inGetFocus then LWindow.Show else LWindow.Visible := True;
              if inGetFocus then LWindow.BringToFront;
            end;
          end;
        end;
      end;
    end;

    // now there can be another tricky condition: something may be shown but everything is minimized, if so, restore all visible windows back from their minimized state
    if CheckApplicationMinimized then
    begin
      // restore all windows that allow it to their pre-minimization state
      TrayBrowserSynchronizeWidgetSet; // this is required as otherwise events will not fire
      for i := 0 to LWindowList.Count - 1 do
      begin
        LWindow := TTrayBrowserWindow(GetBrowserWindow(LWindowList[i]));
        if Assigned(LWindow) and LWindow.Visible and LWindow.FWindowSettings.RestoreOnShow and (LWindow.WindowState = wsMinimized) then
        begin
          if LWindow.FWindowState.PreMinimizeWindowState <> wsMinimized then // handle corner case just to be safe
          begin
            LWindow.WindowState := LWindow.FWindowState.PreMinimizeWindowState;
            if inGetFocus then LWindow.BringToFront;
          end else
          begin
            LWindow.WindowState := wsNormal;
            if inGetFocus then LWindow.BringToFront;
          end;
          LWindow.FormWindowStateChange(LWindow); // yes, this is necessary, programmatic changes do not fire the event
        end;
      end;
      TrayBrowserSynchronizeWidgetSet; // this is required as otherwise events will not fire
    end;

    if inGetFocus then
    begin
      ApplicationChangingVisibility := False;
      FocusEligibleWindow;
    end;
  except end;
  ApplicationChangingVisibility := False;
  FreeAndNil(LWindowList);
end;

procedure TTrayBrowserRootWindow.FocusEligibleWindow;
var
  LWindow, LWindowE, LWindowM: TTrayBrowserWindow;
  i: Integer;
begin
  if ApplicationChangingVisibility then Exit; // application is still undergoing visibility changes, don't

  // activate last active visible and non-minimized window, if nothing can be activated, just activate main window
  LWindowE := nil; // eligible window
  LWindowM := nil; // eligible minimized window
  for i := WindowFocusList.Count - 1 downto 0 do
  begin
    LWindow := TTrayBrowserWindow(GetBrowserWindow(WindowFocusList[i]));
    if Assigned(LWindow) and LWindow.Visible then begin
      if (LWindow.WindowState <> wsMinimized) then
      begin
        // found our eligible window
        LWindowE := LWindow;
        Break;
      end else if not Assigned(LWindowM) then LWindowM := LWindow; // found eligible, but minimized window
    end;
  end;
  if Assigned(LWindowE) then LWindowE.BringToFront // focus eligible window
    else if Assigned(LWindowM) then LWindowM.BringToFront; // otherwise focus eligible minimized window

  // stop lost focus timer even if something caused it running and reset application focus state to focused
  FocusTimer.Enabled := False;
  TrayBrowserSynchronizeWidgetSet;
  FocusTimer.Enabled := False;
  ApplicationInFocus := True;
end;

procedure TTrayBrowserRootWindow.HideApplication;
var
  LWPair: TTBWindowPair;
begin
  FocusTimer.Enabled := False; // stop focus lost timer
  ApplicationInFocus := False; // application loses focus after that

  if not CheckApplicationVisibility(True) then Exit; // nothing at all is visible, nothing more to do

  // send hide command to all windows, preventing any focus updates and visibility state storage
  ApplicationChangingVisibility := True;
  for LWPair in WindowRegistry do
    if TTrayBrowserWindow(LWPair.Value).Visible then TTrayBrowserWindow(LWPair.Value).Hide;
  ApplicationChangingVisibility := False;
end;

procedure TTrayBrowserRootWindow.ApplicationActivate(Sender: TObject);
begin
  // we have got focus
  ApplicationInFocus := True;
  FocusTimer.Enabled := False;
end;

procedure TTrayBrowserRootWindow.ApplicationDeactivate(Sender: TObject);
begin
  // this is tricky. we can lose focus i.e. due to window switch or clicking on our own tray icon, so we need to wait just a little bit to see if we get some other event until we really consider us lost focus
  if CheckApplicationVisibility(True) then FocusTimer.Enabled := True else ApplicationInFocus := False;
end;

function TTrayBrowserRootWindow.CheckApplicationVisibility(const inAnyWindow: Boolean = False): Boolean;
var
  LWPair: TTBWindowPair;
begin
  Result := False;
  TrayBrowserSynchronizeWidgetSet;

  if WindowRegistry.Count = 0 then Exit; // rare corner case

  // first, check if anything is visible at all (quick check for inAnyWindow = True)
  for LWPair in WindowRegistry do
  begin
    if TTrayBrowserWindow(LWPair.Value).Visible then
    begin
      Result := True;
      Break;
    end;

    // but if we need to check for all windows to be visible (inAnyWindow = False), encountering first window means we are not fully visible
    if (not inAnyWindow) and (not TTrayBrowserWindow(LWPair.Value).Visible) and TTrayBrowserWindow(LWPair.Value).FWindowState.WasVisible then
    begin
      Result := False;
      Exit;
    end;
  end;

  if not Result then Exit; // if nothing is visible, no more checking

  // additionally, if inAnyWindow = False, check for all windows that need to be visible to be visible
  if not inAnyWindow then
  begin
    for LWPair in WindowRegistry do
    begin
      if (not TTrayBrowserWindow(LWPair.Value).Visible) and TTrayBrowserWindow(LWPair.Value).FWindowState.WasVisible then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

function TTrayBrowserRootWindow.CheckApplicationMinimized: Boolean;
var
  LWPair: TTBWindowPair;
begin
  Result := False;
  if not CheckApplicationVisibility(True) then Exit; // nothing is visible and so minimized state is irrelevant, return false

  Result := True;
  for LWPair in WindowRegistry do
  begin
    if TTrayBrowserWindow(LWPair.Value).Visible and (TTrayBrowserWindow(LWPair.Value).WindowState <> wsMinimized) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function TTrayBrowserRootWindow.CheckApplicationStayOnTop: Boolean;
var
  LWPair: TTBWindowPair;
begin
  Result := False;
  for LWPair in WindowRegistry do
  begin
    if TTrayBrowserWindow(LWPair.Value).FWindowSettings.AlwaysOnTop then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TTrayBrowserRootWindow.CheckApplicationFocus: Boolean;
var
  LWPair: TTBWindowPair;
begin
  Result := False;
  TrayBrowserSynchronizeWidgetSet;
  for LWPair in WindowRegistry do
  begin
    if TTrayBrowserWindow(LWPair.Value).Active then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TTrayBrowserRootWindow.CheckApplicationKiosk: Boolean;
var
  LWPair: TTBWindowPair;
begin
  Result := False;
  TrayBrowserSynchronizeWidgetSet;
  for LWPair in WindowRegistry do
  begin
    if (TTrayBrowserWindow(LWPair.Value).FWindowSettings.FullScreen = tbfsKiosk) or (TTrayBrowserWindow(LWPair.Value).FWindowSettings.FullScreen = tbfsAll) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TTrayBrowserRootWindow.WindowGotFocus(inWindowID: String);
var
  i: Integer;
begin
  if ApplicationChangingVisibility then Exit; // aplication is in massive visibility change, ignore
  try
    repeat
      i := WindowFocusList.IndexOf(inWindowID);
      if i >= 0 then WindowFocusList.Delete(i);
    until i < 0;
  except end;
  WindowFocusList.Add(inWindowID);
end;

procedure TTrayBrowserRootWindow.FocusTimerTimer(Sender: TObject);
begin
  // when this timer fires, we consider ourselves losing focus for real
  FocusTimer.Enabled := False;
  ApplicationInFocus := True; // let us assume we still have focus if this timer fired
  if CheckApplicationFocus then Exit; // one of our windows is active, we have not lost the focus
  ApplicationInFocus := False; // yup, lost focus completely

  // application autohide
  if TrayBrowserApplication.Settings.AutoHide then
    if (not CheckApplicationMinimized) and (not CheckApplicationKiosk) then // do not autohide when minimized or some window is using kiosk mode
    HideApplication;
end;

