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

// tray icon handling, included from TBRootWindow unit

procedure TTrayBrowserRootWindow.TrayIconClick(Sender: TObject);
var
  LWindow: TTrayBrowserWindow;
begin
  if (TrayBrowserApplication.Settings.TrayIconClickMode <> tbtcNone) and (TrayBrowserApplication.Settings.TrayIconClickMode <> tbtcEvent) then
  begin
    // standard visibility and focus handling (normal / restore / show)
    if not CheckApplicationVisibility then
    begin
      // this happens when some or all of open application windows are not visible
      ShowApplication;
      TrayIconClickCheckMonitorChange;
    end else
    begin
      // something is visible, but application can be not in focus or minimized completely
      if CheckApplicationMinimized then
      begin
        // the application is minimized, what to do...
        if TrayBrowserApplication.Settings.TrayIconClickMode = tbtcNormal then
        begin
          HideApplication; // just hide minimized application
        end else // (restore / show)
        begin
          ShowApplication;
          TrayIconClickCheckMonitorChange;
        end;
      end else
      begin
        if TrayBrowserApplication.Settings.TrayIconClickMode = tbtcShow then
        begin
          // always show
          ShowApplication;
          TrayIconClickCheckMonitorChange;
        end else // (normal / restore)
        begin
          // check application focus
          if ApplicationInFocus or CheckApplicationFocus then
          begin
            // if focus and not minimized, hide the application completely - or just reposition and bring to front if monitor changed
            if TrayIconClickCheckMonitorChange then ShowApplication else HideApplication;
          end else
          begin
            // not in focus, does any window stay on top?
            if CheckApplicationStayOnTop then
            begin
              // yes, we are still visible even if not in focus, hide application completely - or just reposition and bring to front if monitor changed
              if TrayIconClickCheckMonitorChange then ShowApplication else HideApplication;
            end else
            begin
              // not in focus and not staying on top, just bring the whole application to front
              ShowApplication;
              TrayIconClickCheckMonitorChange;
            end;
          end;
        end;
      end;
    end;
  end;

  if TrayBrowserApplication.Settings.TrayIconClickMode <> tbtcNone then
  begin
    // also send event to the main window
    LWindow := TTrayBrowserWindow(GetBrowserWindow(MainWindowID));
    if Assigned(LWindow) then TTrayBrowserWindow(LWindow).PostJSEvent('"ontrayiconclick"');
  end;
end;

function TTrayBrowserRootWindow.TrayIconClickCheckMonitorChange: Boolean;
var
  LWindowPair: TTBWindowPair;
  LMonitor, LWMonitor: TMonitor;
  LWindowState: TWindowState;
begin
  Result := False;
  LMonitor := Screen.MonitorFromPoint(Mouse.CursorPos, mdNull); // grab monitor from tray icon clicked
  if Assigned(LMonitor) then
  begin
    for LWindowPair in WindowRegistry do
    begin
      if TTrayBrowserWindow(LWindowPair.Value).FWindowSettings.SnapToFollowTrayClick and (TTrayBrowserWindow(LWindowPair.Value).FWindowSettings.SnapTo <> tbstNone) then
      begin
        TTrayBrowserWindow(LWindowPair.Value).GetWindowMonitor(LWMonitor);
        if LMonitor <> LWMonitor then
        begin
          // initially position main window into the new monitor for window sizing/positioning to work properly
          LWindowState := TTrayBrowserWindow(LWindowPair.Value).WindowState;
          if LWindowState = wsMaximized then TTrayBrowserWindow(LWindowPair.Value).WindowState := wsNormal; // maximize must be reset for proper repositioning
          TTrayBrowserWindow(LWindowPair.Value).FInManualPositionChange := True;
          if LWindowState = wsMaximized then TTrayBrowserWindow(LWindowPair.Value).BoundsRect := LMonitor.WorkareaRect; // take full monitor size
          TTrayBrowserWindow(LWindowPair.Value).FInManualPositionChange := False;
          TTrayBrowserWindow(LWindowPair.Value).WindowState := LWindowState; // return maximization back if changed
          TTrayBrowserWindow(LWindowPair.Value).PositionOurselves(False, True, False, False, LMonitor); // do not synchronize widget set (do not process messages) to avoid flickering in the process, also directly provide monitor to snap onto
          Result := True; // some snapped window was repositioned
        end;
      end;
    end;
  end;
end;

