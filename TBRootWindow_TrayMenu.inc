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

// tray icon menu handling, included from TBRootWindow unit

procedure TTrayBrowserRootWindow.AddUserMenuItem(const inWindowID: String; const inCallbackID: Integer; const inMenuText: String);
var
  i: Integer;
  LMenuitem: TTrayBrowserMenuItem;
begin
  LMenuitem := TTrayBrowserMenuItem.Create(TrayMenu);
  LMenuitem.Caption := inMenuText;
  LMenuitem.Enabled := True;
  LMenuitem.Visible := True;
  LMenuitem.OnClick := @UserMenuClick;
  LMenuitem.XTBWindowID := inWindowID;
  LMenuitem.XTBCallbackID := inCallbackID;
  try
    for i := 0 to TrayMenu.Items.Count - 1 do
    begin
      if not TrayMenu.Items[i].InheritsFrom(TTrayBrowserMenuItem) then
      begin
        TrayMenu.Items.Insert(i, LMenuitem);
        Break;
      end;
    end;
  except end;
end;

procedure TTrayBrowserRootWindow.ClearUserMenu(const inWindowID: String);
var
  i: Integer;
  LDeleteList: specialize TList<TMenuItem>;
begin
  LDeleteList := specialize TList<TMenuItem>.Create;
  try
    if inWindowID <> '*' then
    begin
      // single window
      for i := 0 to TrayMenu.Items.Count - 1 do
        if TrayMenu.Items[i].InheritsFrom(TTrayBrowserMenuItem) then
          if TTrayBrowserMenuItem(TrayMenu.Items[i]).XTBWindowID = inWindowID then
            LDeleteList.Add(TrayMenu.Items[i]);
    end else
    begin
      // all windows
      for i := 0 to TrayMenu.Items.Count - 1 do
        if TrayMenu.Items[i].InheritsFrom(TTrayBrowserMenuItem) then
          LDeleteList.Add(TrayMenu.Items[i]);
    end;
    for i := 0 to LDeleteList.Count - 1 do
    begin
      TrayMenu.Items.Remove(LDeleteList[i]);
      LDeleteList[i].Free;
    end;
  except end;
  LDeleteList.Clear;
  FreeAndNil(LDeleteList);
end;

procedure TTrayBrowserRootWindow.UpdateMenuRetryState;
var
  LWPair: TTBWindowPair;
  LHaveError: Boolean;
begin
  if TrayBrowserApplication.Settings.MenuHideRetry then Exit; // requested to have this hidden, so have nothing to do

  // check all windows
  LHaveError := False;
  for LWPair in WindowRegistry do
  begin
    if TTrayBrowserWindow(LWPair.Value).FWindowState.InLoadError then
    begin
      LHaveError := True;
      Break;
    end;
  end;

  // and so we have it
  TrayMenuRetry.Visible := LHaveError;
end;

procedure TTrayBrowserRootWindow.UserMenuClick(Sender: TObject);
var
  LWindow: TTrayBrowserWindow;
begin
  if Assigned(Sender) and Sender.InheritsFrom(TTrayBrowserMenuItem) then
  begin
    LWindow := TTrayBrowserWindow(GetBrowserWindow(TTrayBrowserMenuItem(Sender).XTBWindowID));
    if Assigned(LWindow) then LWindow.PostJSEvent('"__callback",' + IntToStr(TTrayBrowserMenuItem(Sender).XTBCallbackID));
  end;
end;

procedure TTrayBrowserRootWindow.ClearMenuOrphans;
var
  i: Integer;
  LDeleteList: specialize TList<TMenuItem>;
begin
  LDeleteList := specialize TList<TMenuItem>.Create;
  try
    for i := 0 to TrayMenu.Items.Count - 1 do
      if TrayMenu.Items[i].InheritsFrom(TTrayBrowserMenuItem) then
        if not WindowRegistry.ContainsKey(TTrayBrowserMenuItem(TrayMenu.Items[i]).XTBWindowID) then
          LDeleteList.Add(TrayMenu.Items[i]);
    for i := 0 to LDeleteList.Count - 1 do
    begin
      TrayMenu.Items.Remove(LDeleteList[i]);
      LDeleteList[i].Free;
    end;
  except end;
  LDeleteList.Clear;
  FreeAndNil(LDeleteList);
end;

procedure TTrayBrowserRootWindow.TrayMenuPopup(Sender: TObject);
begin
  // if we are clicking on tray menu, prevent us from considering focus lost by this click
  FocusTimer.Enabled := False;
end;

procedure TTrayBrowserRootWindow.TrayMenuRetryClick(Sender: TObject);
var
  LWPair: TTBWindowPair;
begin
  // navigate all windows in error to starting page
  for LWPair in WindowRegistry do
    if TTrayBrowserWindow(LWPair.Value).FWindowState.InLoadError then
      TTrayBrowserWindow(LWPair.Value).Navigate(TTrayBrowserWindow(LWPair.Value).FWindowSettings.URL);
end;

procedure TTrayBrowserRootWindow.TrayMenuExitClick(Sender: TObject);
begin
  TrayBrowserSynchronizeWidgetSet;
  TerminateTrayBrowser;
end;


