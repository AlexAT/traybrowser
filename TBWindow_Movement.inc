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

// window movement, included from TBWindow unit

////////////////////////////////////////////////////////////
// custom drag region support

procedure TTrayBrowserWindow.ResetCustomDragRegion;
begin
  CustomDragRegion.Visible := False;
  CustomDragRegion.Left := 0;
  CustomDragRegion.Top := 0;
  CustomDragRegion.Width := 1;
  CustomDragRegion.Height := 1;
end;

procedure TTrayBrowserWindow.CustomDragRegionStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  if FWindowSettings.Movable and CustomDragRegion.Visible then
  begin
    FCustomDrag.WindowPosition := BoundsRect.Location;
    FCustomDrag.MousePosition := Mouse.CursorPos;
    FCustomDrag.Dragging := True;
    DragTimer.Enabled := True;
  end;
end;

procedure TTrayBrowserWindow.CustomDragRegionEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if FCustomDrag.Dragging and FWindowSettings.Movable then
  begin
    CustomDragDrag;
    DragTimer.Enabled := False;
    FCustomDrag.Dragging := False;
    PositionOurselves;
  end;
end;

procedure TTrayBrowserWindow.CustomDragDrag;
var
  LRect: TRect;
begin
  if (not FWindowSettings.Movable) or (WindowState <> wsNormal) then Exit; // immovable as Zin

  LRect := BoundsRect;
  LRect.Location := FCustomDrag.WindowPosition + Mouse.CursorPos - FCustomDrag.MousePosition;
  if LRect.Location <> BoundsRect.Location then
  begin
    FInManualPositionChange := True;
    BoundsRect := LRect;
    FInManualPositionChange := False;
  end;
end;

procedure TTrayBrowserWindow.FormDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if FCustomDrag.Dragging and FWindowSettings.Movable then
  begin
    Accept := True;
    CustomDragDrag;
  end;
end;

procedure TTrayBrowserWindow.BrowserPanelDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if FCustomDrag.Dragging and FWindowSettings.Movable then
  begin
    Accept := True;
    CustomDragDrag;
  end;
end;

procedure TTrayBrowserWindow.CEFBrowserDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if FCustomDrag.Dragging and FWindowSettings.Movable then
  begin
    Accept := True;
    CustomDragDrag;
  end;
end;

procedure TTrayBrowserWindow.CustomDragRegionDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if FCustomDrag.Dragging and FWindowSettings.Movable then
  begin
    Accept := True;
    CustomDragDrag;
  end;
end;

procedure TTrayBrowserWindow.DragTimerTimer(Sender: TObject);
begin
  if FCustomDrag.Dragging and FWindowSettings.Movable then CustomDragDrag;
end;

////////////////////////////////////////////////////////////
// windows platform specifics: movement handling

// for movement and resizing, we have to use distinct message handlers as OnChangeBounds has few issues
// it does not allow us to cancel the event, it is re-fired multiple times on programmatic bounds changes, it does not allow us to remember position prior to change on maximize
// to add insult to injury, LCL does not register proper position on direct monitor changes via Windows key combinations (Win-Shift-Left/Right), OnChangeBounds is not fired and Left/Top values are wrong
procedure TTrayBrowserWindow.WMPosChanging(var Message: TWMWindowPosChanging);
var
  LWindowState: TWindowState;
  LMonitor, LTargetMonitor: TMonitor;
  LOffset: Integer;
begin
  // prevent reentrancy when we are in some manual positioning code (just allow everything to change)
  if FInPositionChanging or FInManualPositionChange or FInPositioning then
  begin
    inherited;
    Exit;
  end;

  if (Message.WindowPos^.flags and SWP_NOMOVE) = 0 then
  begin
    try
      // oh windows... minimizing actually moves the window offscreen (-32000/-32000), we cannot really rely on any coordinates here so we just check if we are offscreen or not
      if ((Message.WindowPos^.x + Width) < Screen.DesktopLeft) or (Message.WindowPos^.x >= (Screen.DesktopLeft + Screen.DesktopWidth))
        or ((Message.WindowPos^.y + Height) < Screen.DesktopTop) or (Message.WindowPos^.y >= (Screen.DesktopTop + Screen.DesktopHeight)) then
      begin
        // do nothing in these weird cases, allow OS to roam freely
        inherited;
        Exit;
      end;

      FInPositionChanging := True;

      // position is changing, check if this not a monitor change, and yes, tricky again
      LOffset := GetWindowMonitor(LMonitor); // this returns offset we used for detection, zero for normal window, non-zero for maximized window
      LTargetMonitor := Screen.MonitorFromPoint(Point(Message.WindowPos^.x + LOffset, Message.WindowPos^.y + LOffset), mdNearest); // target monitor, we need it under all occasions so mdNearest is the best be
      if (not FIsMoving) and (LMonitor <> LTargetMonitor) then // in case we are moving, ignore monitor changes here, they will all be accounted for on move exit
      begin
        // this is a monitor change
        LWindowState := WindowState;
        if LWindowState <> wsNormal then WindowState := wsNormal; // maximize must be reset for proper repositioning
        FInManualPositionChange := True;
        if LWindowState <> wsMaximized then
        begin
          // take direct coordinates supplied
          Left := Message.WindowPos^.x;
          Top := Message.WindowPos^.y;
        end else
        begin
          // take leftmost corner and size of the target monitor (take care: offset here is necessary due to windows peculiarity (off-monitor left/top), but I really cannot imagine 64 pixel monitor with windows :D)
          Left := LTargetMonitor.WorkareaRect.Left;
          Top := LTargetMonitor.WorkareaRect.Top;
          Width := LTargetMonitor.WorkareaRect.Width;
          Height := LTargetMonitor.WorkareaRect.Height;
        end;
        FInManualPositionChange := False;
        WindowState := LWindowState; // return maximization back if changed
        PositionOurselves(False, True); // do not synchronize widget set (do not process messages) to avoid flickering in the process and keep the monitor we are now on
        Message.WindowPos^.x := Left;
        Message.WindowPos^.y := Top;
        Message.WindowPos^.flags := Message.WindowPos^.flags or SWP_NOMOVE; // reset movement flag as position has already been set
        Message.WindowPos^.flags := Message.WindowPos^.flags or SWP_NOSIZE; // reset size flag as size has already been set
      end else if (not FWindowSettings.Movable) and (WindowState = wsNormal) then // any movement in minimized/maximized states is allowed as it is system initiated
      begin
        // prevent us from moving if we are in normal window state and not allowed to
        Message.WindowPos^.x := Left;
        Message.WindowPos^.y := Top;
        Message.WindowPos^.flags := Message.WindowPos^.flags or SWP_NOMOVE; // reset movement flag as position has already been set
        Message.WindowPos^.flags := Message.WindowPos^.flags or SWP_NOSIZE; // reset size flag as size has already been set
      end;

      // remember position *prior to change* for un-maximizing
      // why prior to change here? because this event also fires on maximize and provides monitor left/top as coordinates so pre-maximize window coordinates are lost
      // this may cause one service timer movement tick offset for the position when maximizing quickly after move (before PositionOurselves runs) but it is better than losing position on maximize
      // why not just on ExitSizeMove only? well, just in case it doesn't fire under specific conditions
      if (not FRememberedPosition.Active) and (WindowState = wsNormal) then
      begin
        FRememberedPosition.Window.Location := BoundsRect.Location;
        FRememberedPosition.Monitor.Location := FMonitorInfo.WorkArea.Location;
      end;
    except end;

    FInPositionChanging := False;
  end;

  inherited;
end;

////////////////////////////////////////////////////////////
// general movement message handling

procedure TTrayBrowserWindow.WMEnterSizeMove;
begin
  FIsMoving := True;
  inherited;
end;

procedure TTrayBrowserWindow.WMExitSizeMove;
begin
  inherited;

  // remember position for un-maximizing
  if (not FRememberedPosition.Active) and (WindowState = wsNormal) then
    FRememberedPosition.Window.Location := BoundsRect.Location;

  // reposition ourselves in case we need to snap to monitor
  PositionOurselves(True, False, False, FWindowSettings.Sizeable);

  FIsMoving := False;
end;

procedure TTrayBrowserWindow.WMMove(var Message: TWMMove);
begin
  inherited;
  if Assigned(Chromium) then Chromium.NotifyMoveOrResizeStarted;
end;

