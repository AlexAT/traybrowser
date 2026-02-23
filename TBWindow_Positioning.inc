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

// window positioning, included from TBWindow unit

////////////////////////////////////////////////////////////
// main window positioning code

procedure TTrayBrowserWindow.PositionOurselves(inSynchronizeWidgetSet: Boolean = True; inKeepMonitor: Boolean = False; inCenterOnMonitor: Boolean = False; inResizeSizeable: Boolean = False; inForceMonitor: TMonitor = nil);
var
  LPosition: TPoint;
  LSize, LBrowserSize, LBorderMargins, LSizeCompensation, LCompensatedSize: TSize;
  LDivResult: Integer = 0;
  LDivRemainder: Integer = 0;
  LSkipChecks, LForcedCentering: Boolean;
  LSnapTo: TTBSnapToMode;
  LMonitor: TMonitor;
  LTargetBounds: TRect;
  i: Integer;
begin
  // general sizing considerations:
  // browser size (inner) = ScaleFormTo96(window size), so window size = Scale96ToForm(browser size), it is feasible to specify and remember browser sizes, not window sizes
  // of course scaling functions are not used directly but implemented locally because we need to scale to specific monitor and not to what the form is on in multiple places
  // left/top border/caption sizes for movable (bsSingle) can be obtained using ClientToScreen(Point(0,0)), as ClientToScreen is not reliable overall and can only be used when the window is visible
  // right/bottom border sizes cannot be obtained, right can be assumed equal to left, bottom probably too
  // for bsNone border type, border sizes can be 0 (check!) and so BrowserPanel bsNone border size can be set to 1 to provide window framing for tray mode, but then window width/height needs to be offset by border size twice

  // scaling is tricky: the Chrome perk and problem is, the result must fit the screen-to-browser DPI conversion as integer, otherwise 1 browser pixel viewport lines at the right and bottom will not be displayed
  // thus we need to find pixel divisor (alignment factor) that results in precisely integer value from ScaleFormTo96, but we do not allow this to offset size more than 16 pixels, thus sometimes this will not work
  // this is very important as i.e. borderless browser application design may rely on full single pixel application-drawn border visibility

  // positioning and multiple monitors support has a whole load of different caveats that are spread out through the application code, so read the whole TBWindow code to find out which and where
  // it is tempting to say the whole TrayBrowser is about this very positioning code as most of the effort really went to making it work as expected under lots of OS-specific conditions and caveats

  // prevent reentrancy
  if FInPositioning then Exit;
  FInPositioning := True;

  try
    // actually, this is a good idea, we let the widget set settle before anything else happens but as it can cause flicker on temporary programmatic movements like monitor change on tray icon click, we have a control flag here
    if inSynchronizeWidgetSet then TrayBrowserSynchronizeWidgetSet;

    if not Assigned(inForceMonitor) then
    begin
      // detect specific situations where window moves totally offscreen by some occasion (this implies recentering on active monitor)
      LForcedCentering := False;
      if Visible then
      begin
        LPosition := ClientToScreen(Point(0,0));
        LForcedCentering := True;
        for i := 0 to Screen.MonitorCount - 1 do
        begin
          if (Screen.Monitors[i].WorkareaRect.Right > LPosition.X) and (Screen.Monitors[i].WorkareaRect.Left < (LPosition.X + Width))
            and (Screen.Monitors[i].WorkareaRect.Bottom > LPosition.Y) and (Screen.Monitors[i].WorkareaRect.Top < (LPosition.Y + Height)) then
          begin
            // yes, we are covering this monitor
            LForcedCentering := False;
            Break;
          end;
        end;
        if LForcedCentering then LForcedCentering := SetStartingPosition; // we are fully offscreen, recover by moving to the starting position or monitor (and maybe do centering later on)
        LForcedCentering := LForcedCentering or inCenterOnMonitor; // also honor forced centering passed from the caller
      end;

      // find active monitor from the application center (worst case from the application Monitor property) or just current left/top monitor if we are to keep it
      LMonitor := nil;
      if inKeepMonitor then GetWindowMonitor(LMonitor);
      if not Assigned(LMonitor) then
      begin
        if Visible then LMonitor := Screen.MonitorFromPoint(ClientToScreen(Point(Width div 2, Height div 2)), mdNull) // take care: ClientToScreen is unreliable and can only be used when visible
          else LMonitor := Screen.MonitorFromPoint(Point(Left + (Width div 2) + FMonitorInfo.BorderWidth, Top + ((Height + FMonitorInfo.TitleHeight + FMonitorInfo.BorderWidth) div 2))); // yes, there may be a certain mistake in this heuristics, but it is better then nothing
        if not Assigned(LMonitor) then LMonitor := Monitor; // last resort
      end;
      if not Assigned(LMonitor) then TrayBrowserApplication.Die('Failed to find monitor to run on'); // whoops, this should never ever happen (means even Monitor is null)
    end else LMonitor := inForceMonitor; // use forced monitor
    FMonitorInfo.Active := LMonitor.MonitorNum;

    // load monitor work area for further processing and API, resolution and monitor (and tray position) changes detection (we do use this a lot in the following code)
    FMonitorInfo.WorkArea := LMonitor.WorkareaRect;
    FMonitorInfo.RealBounds := LMonitor.BoundsRect;
    FMonitorInfo.DPI := LMonitor.PixelsPerInch;

    // find main monitor
    FMonitorInfo.Main := FMonitorInfo.Active;
    LMonitor := Screen.MonitorFromPoint(Point(0,0), mdPrimary);
    if Assigned(LMonitor) then FMonitorInfo.Main := LMonitor.MonitorNum;

    // clamp default border margins to maximum half of the monitor size (implying minimal window size of 0 worst case but that would be corrected later)
    LBorderMargins.Width := Min(FWindowSettings.BorderMargin, FMonitorInfo.WorkArea.Width div 2);
    LBorderMargins.Height := Min(FWindowSettings.BorderMargin, FMonitorInfo.WorkArea.Height div 2);

    // detect tray position
    if (FMonitorInfo.WorkArea.Size <> FMonitorInfo.RealBounds.Size) then
      FMonitorInfo.Tray := FMonitorInfo.Active // use active monitor to determine tray position
    else
      FMonitorInfo.Tray := FMonitorInfo.Main; // active monitor supposedly does not have any tray, use main monitor to determine tray position

    // this is the default Windows assumption, on other platforms it is feasible to assume TrayAtBottom = false
    FMonitorInfo.TrayPosition := tbtpUnknown;
    LMonitor := Screen.Monitors[FMonitorInfo.Tray];
    if LMonitor.WorkareaRect <> LMonitor.BoundsRect then
    begin
      if (LMonitor.WorkareaRect.Left - LMonitor.Left) > (LMonitor.BoundsRect.Right - LMonitor.WorkareaRect.Right) then
      begin
        // left margin > right margin, assume tray is at left
        if (LMonitor.WorkareaRect.Top - LMonitor.Top) > (LMonitor.BoundsRect.Bottom - LMonitor.WorkareaRect.Bottom) then
          FMonitorInfo.TrayPosition := tbtpTopLeft // top margin > bottom margin, assume tray is at top
        else
          FMonitorInfo.TrayPosition := tbtpBottomLeft; // top margin <= bottom margin, assume tray is at bottom
      end else
      begin
        // left margin <= right margin, assume tray is at right
        if (LMonitor.WorkareaRect.Top - LMonitor.Top) > (LMonitor.BoundsRect.Bottom - LMonitor.WorkareaRect.Bottom) then
          FMonitorInfo.TrayPosition := tbtpTopRight // top margin > bottom margin, assume tray is at top
        else
          FMonitorInfo.TrayPosition := tbtpBottomRight; // top margin <= bottom margin, assume tray is at bottom
      end;
    end;

    // determine window size compensations to maintain browser size: window projected borders and title bar sizes, border margins
    if Visible then // ClientToScreen is unreliable, we cannot get projected point until we are visible
    begin
      LPosition := ClientToScreen(Point(0,0)); // get real client area left/top, take care this is unreliable and can only be used when visible
      FMonitorInfo.BorderWidth := Max(0, LPosition.X - Left); // calculate projected border size
      FMonitorInfo.TitleHeight := Max(0, LPosition.Y - Top); // calculate projected top bar size
    end else
    begin
      if FWindowSettings.Border = tbbtWindow then
      begin
        // on Windows, we can use GetSystemMetrics to at least approximate what OS border is
        {$IFDEF WINDOWS}
        FMonitorInfo.BorderWidth := GetSystemMetrics(SM_CXBORDER) + GetSystemMetrics(SM_CXEDGE);
        FMonitorInfo.TitleHeight := GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYBORDER) + GetSystemMetrics(SM_CYEDGE);
        {$ENDIF}
      end else
      begin
        // for OS-borderless windows assume borders are really zero
        FMonitorInfo.BorderWidth := 0;
        FMonitorInfo.TitleHeight := 0;
      end;
    end;

    LSizeCompensation.Width := (BrowserPanel.BorderSpacing.Around * 2) + (FMonitorInfo.BorderWidth * 2) + (LBorderMargins.Width * 2); // local left and right borders, window left and right borders, border margin
    LSizeCompensation.Height := (BrowserPanel.BorderSpacing.Around * 2) + FMonitorInfo.TitleHeight + FMonitorInfo.BorderWidth + (LBorderMargins.Height * 2); // local top and bottom borders, window top bar and bottom border, border margin

    LTargetBounds := BoundsRect; // initialize target bounds

    // check if we are not minimized or maximized, if not, we can't do much there
    if WindowState = wsNormal then
    begin
      // custom fullscreen handling
      LSkipChecks := False;
      case FWindowSettings.FullScreen of
        tbfsWindow:
        begin
          // window implies sizing window to full available workable size (honoring borders / alignment / whatever) for current monitor and centers window
          FBrowserSize.Width := Max(1, ScaleToBrowser(Max(1, FMonitorInfo.WorkArea.Width - LSizeCompensation.Width), FMonitorInfo.DPI));
          FBrowserSize.Height := Max(1, ScaleToBrowser(Max(1, FMonitorInfo.WorkArea.Height - LSizeCompensation.Height), FMonitorInfo.DPI));
          LBrowserSize.Width := Max(1, ScaleFromBrowser(FBrowserSize.Width, FMonitorInfo.DPI));
          LBrowserSize.Height := Max(1, ScaleFromBrowser(FBrowserSize.Height, FMonitorInfo.DPI));
        end;

        tbfsDesktop, tbfsKiosk, tbfsAll:
        begin
          case FWindowSettings.FullScreen of
            tbfsDesktop: LTargetBounds := FMonitorInfo.WorkArea; // desktop enforces full desktop size without any alignments
            tbfsKiosk: LTargetBounds := FMonitorInfo.RealBounds; // kiosk enforces full monitor size without any alignments
            tbfsAll: LTargetBounds := Screen.DesktopRect;
          end;

          // use enforced size and position data and skip any checks
          FBrowserSize.Width := ScaleToBrowser(LTargetBounds.Width, FMonitorInfo.DPI);
          FBrowserSize.Height := ScaleToBrowser(LTargetBounds.Height, FMonitorInfo.DPI);
          LBrowserSize := LTargetBounds.Size;
          LCompensatedSize := LTargetBounds.Size;
          LSkipChecks := True;
        end;
      end;

      // perform sizing first
      if not LSkipChecks then
      begin
        // if we are resizing sizeable window, put current window size into active size
        if inResizeSizeable then
        begin
          FBrowserSize.Width := Max(1, ScaleToBrowser(Max(1, Width), FMonitorInfo.DPI));
          FBrowserSize.Height := Max(1, ScaleToBrowser(Max(1, Height), FMonitorInfo.DPI));
        end;

        // /=====================================\
        // | do little ascii window sizing first |
        // \=====================================/

        // calculate scaled screen size
        LBrowserSize.Width := Max(1, ScaleFromBrowser(Max(1, FBrowserSize.Width), FMonitorInfo.DPI)); // convert browser width to pixels (accounting for DPI/scaling), 1 pixel here and there is the limit
        LBrowserSize.Height := Max(1, ScaleFromBrowser(Max(1, FBrowserSize.Height), FMonitorInfo.DPI)); // convert browser height to pixels (accounting for DPI/scaling), 1 pixel here and there is the limit

        // get scaling alignment factor and default to 1 if it cannot be found
        FMonitorInfo.ScalingAlignment := 1;
        if (TrayBrowserApplication.Settings.ScalingMode = tbscNormal) or (TrayBrowserApplication.Settings.ScalingMode = tbscFloor) then
        begin
          FMonitorInfo.ScalingAlignment := GetScalingAlignment(Screen.Monitors[FMonitorInfo.Active]);
          if FMonitorInfo.ScalingAlignment = 0 then FMonitorInfo.ScalingAlignment := 1; // could not find scaling factor so default to 1

          // ceil up (default) or floor down to alignment
          DivMod(LBrowserSize.Width, FMonitorInfo.ScalingAlignment, LDivResult, LDivRemainder);
          LBrowserSize.Width := LDivResult * FMonitorInfo.ScalingAlignment;
          if (TrayBrowserApplication.Settings.ScalingMode = tbscNormal) and (LDivRemainder > 0) then LBrowserSize.Width := LBrowserSize.Width + FMonitorInfo.ScalingAlignment;

          DivMod(LBrowserSize.Height, FMonitorInfo.ScalingAlignment, LDivResult, LDivRemainder);
          LBrowserSize.Height := LDivResult * FMonitorInfo.ScalingAlignment;
          if (TrayBrowserApplication.Settings.ScalingMode = tbscNormal) and (LDivRemainder > 0) then LBrowserSize.Height := LBrowserSize.Height + FMonitorInfo.ScalingAlignment;
        end;

        // convert real size back to browser size
        FBrowserSize.Width := Max(1, ScaleToBrowser(Max(1, LBrowserSize.Width), FMonitorInfo.DPI)); // yes, 1 pixel is the limit
        FBrowserSize.Height := Max(1, ScaleToBrowser(Max(1, LBrowserSize.Height), FMonitorInfo.DPI)); // yes, 1 pixel is the limit
        LSize := LBrowserSize + LSizeCompensation; // projected size is full window size including browser, local borders and window framing

        // clamp projection to monitor size
        if (LSize.Width > FMonitorInfo.WorkArea.Width) or (LSize.Height > FMonitorInfo.WorkArea.Height) then
        begin
          if LSize.Width > FMonitorInfo.WorkArea.Width then
          begin
            // we have to clamp, first things first, check if we can reduce border margins to fit
            LBorderMargins.Width := (FMonitorInfo.WorkArea.Width - (LSize.Width - (LBorderMargins.Width * 2))) div 2;
            if (LBorderMargins.Width >= 0) then
            begin
              // yes, we are able to compensate
              LSizeCompensation.Width := (BrowserPanel.BorderSpacing.Around * 2) + (FMonitorInfo.BorderWidth * 2) + (LBorderMargins.Width * 2); // local left and right borders, window left and right borders, border margin
              LSize.Width := LBrowserSize.Width + LSizeCompensation.Width; // projected width is full window width including browser, local borders and window framing
            end else LBorderMargins.Width := Min(FWindowSettings.BorderMargin, FMonitorInfo.WorkArea.Width div 2); // restore original border margin
          end;

          if LSize.Height > FMonitorInfo.WorkArea.Height then
          begin
            // we have to clamp, first things first, check if we can reduce border margins to fit
            LBorderMargins.Height := (FMonitorInfo.WorkArea.Height - (LSize.Height - (LBorderMargins.Height * 2))) div 2;
            if (LBorderMargins.Height >= 0) then
            begin
              // yes, we are able to compensate
              LSizeCompensation.Height := (BrowserPanel.BorderSpacing.Around * 2) + FMonitorInfo.TitleHeight + FMonitorInfo.BorderWidth + (LBorderMargins.Height * 2); // local top and bottom borders, window top bar and bottom border, border margin
              LSize.Height := LBrowserSize.Height + LSizeCompensation.Height; // projected width is full window height including browser, local borders and window framing
            end else LBorderMargins.Height := Min(FWindowSettings.BorderMargin, FMonitorInfo.WorkArea.Height div 2); // restore original border margin
          end;

          // convert corrected projection back to browser width
          FBrowserSize.Width := Max(1, ScaleToBrowser(Max(1, Min(LSize.Width, FMonitorInfo.WorkArea.Width) - LSizeCompensation.Width), FMonitorInfo.DPI)); // yes, 1 pixel is the limit
          FBrowserSize.Height := Max(1, ScaleToBrowser(Max(1, Min(LSize.Height, FMonitorInfo.WorkArea.Height) - LSizeCompensation.Height), FMonitorInfo.DPI)); // yes, 1 pixel is the limit

          // calculate scaled screen size
          LBrowserSize.Width := ScaleFromBrowser(FBrowserSize.Width, FMonitorInfo.DPI); // convert browser width to pixels (accounting for DPI/scaling)
          LBrowserSize.Height := ScaleFromBrowser(FBrowserSize.Height, FMonitorInfo.DPI); // convert browser height to pixels (accounting for DPI/scaling)

          if (TrayBrowserApplication.Settings.ScalingMode = tbscNormal) or (TrayBrowserApplication.Settings.ScalingMode = tbscFloor) then
          begin
            // floor down to scaling alignment, we would get over screen size otherwise
            DivMod(LBrowserSize.Width, FMonitorInfo.ScalingAlignment, LDivResult, LDivRemainder);
            LBrowserSize.Width := LDivResult * FMonitorInfo.ScalingAlignment;
            DivMod(LBrowserSize.Width, FMonitorInfo.ScalingAlignment, LDivResult, LDivRemainder);
            LBrowserSize.Height := LDivResult * FMonitorInfo.ScalingAlignment;
          end else
          begin
            // just clamp to screen size (should not happen, but just in case)
            LBrowserSize.Width := Min(LBrowserSize.Width, FMonitorInfo.WorkArea.Width);
            LBrowserSize.Height := Min(LBrowserSize.Height, FMonitorInfo.WorkArea.Height);
          end;

          // convert real size back to browser size
          FBrowserSize.Width := Max(1, ScaleToBrowser(Max(1, LBrowserSize.Width), FMonitorInfo.DPI)); // yes, 1 pixel is the limit
          FBrowserSize.Height := Max(1, ScaleToBrowser(Max(1, LBrowserSize.Height), FMonitorInfo.DPI)); // yes, 1 pixel is the limit
        end;

        // set real and projected and browser sizes, here we also clamp to screen size for safety
        LSize.Width := Min(LBrowserSize.Width + (BrowserPanel.BorderSpacing.Around * 2), FMonitorInfo.WorkArea.Width); // real form width setting only needs to account for local borders
        LSize.Height := Min(LBrowserSize.Height + (BrowserPanel.BorderSpacing.Around * 2), FMonitorInfo.WorkArea.Height); // real form height setting only needs to account for local borders
        LCompensatedSize.Width := Min(LBrowserSize.Width + LSizeCompensation.Width, FMonitorInfo.WorkArea.Width);
        LCompensatedSize.Height := Min(LBrowserSize.Height + LSizeCompensation.Height, FMonitorInfo.WorkArea.Height);

        // set our new size in the target bounds
        LTargetBounds.Size := LSize;

        // /===================================\
        // | now going to position accordingly |
        // \===================================/

        LSnapTo := FWindowSettings.SnapTo;
        if (LSnapTo = tbstNone) and LForcedCentering then LSnapTo := tbstCenter; // forced centering only applies when SnapTo=none
        case LSnapTo of
          tbstTray:
          begin
            // clamping there should not normally happen but there are corner cases hidden, i.e. 'nocorrect' scaling mode can give sizes 1 dot (scaled to pixels) larger than screen
            LPosition.X := FMonitorInfo.WorkArea.Left + LBorderMargins.Width; // default to left
            LPosition.Y := FMonitorInfo.WorkArea.Top + LBorderMargins.Height; // default to top
            case FMonitorInfo.TrayPosition of
              tbtpTopRight, tbtpBottomRight: // at right, LSizeCompensation.Width contains 2 x LBorderMargins.Width, so we compensate one back
                LPosition.X := Max(FMonitorInfo.WorkArea.Left, FMonitorInfo.WorkArea.Right - LCompensatedSize.Width + LBorderMargins.Width);
            end;
            case FMonitorInfo.TrayPosition of
              tbtpBottomLeft, tbtpBottomRight: // at bottom, LSizeCompensation.Height contains 2 x LBorderMargins.Height, so we compensate one back
                LPosition.Y := Max(FMonitorInfo.WorkArea.Top, FMonitorInfo.WorkArea.Bottom - LCompensatedSize.Height + LBorderMargins.Height);
            end;
          end;

          // positional snapping
          tbstTopLeft, tbstTopCenter, tbstTopRight, tbstCenterLeft, tbstCenter, tbstCenterRight, tbstBottomLeft, tbstBottomCenter, tbstBottomRight:
          begin
            // X
            case LSnapTo of
              tbstTopLeft, tbstCenterLeft, tbstBottomLeft: LPosition.X := FMonitorInfo.WorkArea.Left + LBorderMargins.Width;
              tbstTopCenter, tbstCenter, tbstBottomCenter: LPosition.X := FMonitorInfo.WorkArea.Left + (Max(0, FMonitorInfo.WorkArea.Width - LCompensatedSize.Width) div 2) + LBorderMargins.Width; // do not forget to move one border margin right
              tbstTopRight, tbstCenterRight, tbstBottomRight: LPosition.X := Max(FMonitorInfo.WorkArea.Left, FMonitorInfo.WorkArea.Right - LCompensatedSize.Width + LBorderMargins.Width); // do not forget to move one border margin right
            end;

            // Y
            case LSnapTo of
              tbstTopLeft, tbstTopCenter, tbstTopRight: LPosition.Y := FMonitorInfo.WorkArea.Top + LBorderMargins.Height;
              tbstCenterLeft, tbstCenter, tbstCenterRight: LPosition.Y := FMonitorInfo.WorkArea.Top + (Max(0, FMonitorInfo.WorkArea.Height - LCompensatedSize.Height) div 2) + LBorderMargins.Height; // do not forget to move one border margin down
              tbstBottomLeft, tbstBottomCenter, tbstBottomRight: LPosition.Y := Max(FMonitorInfo.WorkArea.Top, FMonitorInfo.WorkArea.Bottom - LCompensatedSize.Height + LBorderMargins.Height); // do not forget to move one border margin down
            end;
          end;

          else
          begin
            // snapping to nothing (arbitrarily placed window), just clamp it to monitor edges if ClampToMonitor is set
            LPosition := BoundsRect.Location;
            if FWindowSettings.ClampToMonitor then
            begin
              if LPosition.X < (FMonitorInfo.WorkArea.Left + LBorderMargins.Width) then
                LPosition.X := FMonitorInfo.WorkArea.Left + LBorderMargins.Width
              else if (LPosition.X + LCompensatedSize.Width) > (FMonitorInfo.WorkArea.Right - LBorderMargins.Width) then
                LPosition.X := FMonitorInfo.WorkArea.Right - LCompensatedSize.Width - LBorderMargins.Width;

              if LPosition.Y < (FMonitorInfo.WorkArea.Top + LBorderMargins.Height) then
                LPosition.Y := FMonitorInfo.WorkArea.Top + LBorderMargins.Height
              else if (LPosition.Y + LCompensatedSize.Height) > (FMonitorInfo.WorkArea.Bottom - LBorderMargins.Height) then
                LPosition.Y := FMonitorInfo.WorkArea.Bottom - LCompensatedSize.Height - LBorderMargins.Height;
            end;
          end;
        end;

        // set our new size in the target bounds
        LTargetBounds.Location := LPosition;
      end;
    end;
  except end;

  // the target bounds has been build, check if we need to change anything
  if LTargetBounds <> BoundsRect then
  begin
    if Assigned(Chromium) then Chromium.NotifyMoveOrResizeStarted;
    FInManualPositionChange := True;
    if (not FRememberedPosition.Active) then
    begin
      // we have not so funny issue where browser size is detected incorrectly in JS because alClient resizing happens too late, thus we need to enforce CEFBrowser component sizing here
      CEFBrowser.Width := LBrowserSize.Width;
      CEFBrowser.Height := LBrowserSize.Height;
    end;
    BoundsRect := LTargetBounds;
    CEFBrowser.UpdateSize;
    FInManualPositionChange := False;
  end;

  // on position changes, save new position and generate JS event
  if FLastPosition.Window.Location <> BoundsRect.Location then
  begin
    if not FRememberedPosition.Active then // position cannot be changed to real one while it is remembered
    begin
      FRememberedPosition.Window.Location := BoundsRect.Location;
      FRememberedPosition.Monitor := FMonitorInfo.WorkArea;
      FRememberedPosition.DPI := FMonitorInfo.DPI;

      try
        if (FWindowSettings.Main or (FWindowSettings.PositionID <> '')) then
        begin
          TrayBrowserRootWindow.KVStoreWrite('kvstore', 'window.px[' + IfThen(FWindowSettings.Main, '', FWindowSettings.PositionID) + ']', IntToStr(Left), False);
          TrayBrowserRootWindow.KVStoreWrite('kvstore', 'window.py[' + IfThen(FWindowSettings.Main, '', FWindowSettings.PositionID) + ']', IntToStr(Top), False);
        end;
        try TrayBrowserRootWindow.KVStoreCommit; finally end;
      except
        try TrayBrowserRootWindow.KVStoreRollback; finally; end;
      end;
    end;

    FLastPosition.Window.Location := BoundsRect.Location;

    PostJSEvent('"onpositionchange"'
      + ',' + IntToStr(IfThen(FRememberedPosition.Active, FRememberedPosition.Window.Left - FRememberedPosition.Monitor.Left, Left - FMonitorInfo.WorkArea.Left)) // on-monitor pixel left
      + ',' + IntToStr(IfThen(FRememberedPosition.Active, FRememberedPosition.Window.Top - FRememberedPosition.Monitor.Top, Top - FMonitorInfo.WorkArea.Top)) // on-monitor pixel top
      + ',' + IntToStr(Left) // screen pixel left
      + ',' + IntToStr(Top) // screen pixel top
    );
  end;

  // on size changes, save new size and generate JS event
  if (FLastPosition.Window.Size <> BoundsRect.Size) or (FLastPosition.BrowserSize <> FBrowserSize) then
  begin
    if (not FRememberedPosition.Active) or (FLastPosition.BrowserSize <> FBrowserSize) then // yes, browser size can easily be changed even when remembered
    begin
      FRememberedPosition.Window.Size := BoundsRect.Size;
      FRememberedPosition.BrowserSize := FBrowserSize;
      if (not FRememberedPosition.Active) then FRememberedPosition.PixelBrowserSize := CEFBrowser.BoundsRect.Size;
      FRememberedPosition.Monitor := FMonitorInfo.WorkArea;
      FRememberedPosition.DPI := FMonitorInfo.DPI;

      try
        if (FWindowSettings.SnapTo = tbstNone) and (FWindowSettings.Main or (FWindowSettings.PositionID <> '')) then
        begin
          TrayBrowserRootWindow.KVStoreWrite('kvstore', 'window.bw[' + IfThen(FWindowSettings.Main, '', FWindowSettings.PositionID) + ']', IntToStr(FBrowserSize.Width), False);
          TrayBrowserRootWindow.KVStoreWrite('kvstore', 'window.bh[' + IfThen(FWindowSettings.Main, '', FWindowSettings.PositionID) + ']', IntToStr(FBrowserSize.Height), False);
        end;
        try TrayBrowserRootWindow.KVStoreCommit; finally end;
      except
        try TrayBrowserRootWindow.KVStoreRollback; finally; end;
      end;
    end;

    FLastPosition.BrowserSize := FBrowserSize;
    FLastPosition.Window.Size := BoundsRect.Size;

    PostJSEvent('"onsizechange"'
      + ',' + IntToStr(FBrowserSize.Width) // browser width in browser dots
      + ',' + IntToStr(FBrowserSize.Height) // browser height in browser dots
      + ',' + IntToStr(IfThen(FRememberedPosition.Active, ScaleToBrowser(FRememberedPosition.PixelBrowserSize.Width, FRememberedPosition.DPI), ScaleToBrowser(CEFBrowser.Width, FMonitorInfo.DPI))) // set browser width in pixels
      + ',' + IntToStr(IfThen(FRememberedPosition.Active, ScaleToBrowser(FRememberedPosition.PixelBrowserSize.Height, FRememberedPosition.DPI), ScaleToBrowser(CEFBrowser.Height, FMonitorInfo.DPI))) // set browser height in pixels
      + ',' + IntToStr(CEFBrowser.Width) // actual browser width in pixels
      + ',' + IntToStr(CEFBrowser.Height) // actual browser height in pixels
    );
  end;

  FInPositioning := False;
end;

////////////////////////////////////////////////////////////
// starting position

function TTrayBrowserWindow.SetStartingPosition: Boolean;
var
  LMonitor, LXYMonitor: TMonitor;
  LPoint: TPoint;
  LSize: TSize;
  LRect: TRect;
  s: String;
begin
  Result := True; // we need centering after this
  LMonitor := nil;

  // remember position and size or set up start position
  if (FWindowSettings.Main or (FWindowSettings.PositionID <> '')) and (FWindowSettings.RememberPosition <> tbrpNone) then
  begin
    if FWindowSettings.RememberSize then
    begin
      // recover size from database
      try
        if TrayBrowserRootWindow.KVStoreRead('kvstore', 'window.bw[' + IfThen(FWindowSettings.Main, '', FWindowSettings.PositionID) + ']', s) then
        begin
          LSize.Width := StrToInt(s);
          if TrayBrowserRootWindow.KVStoreRead('kvstore', 'window.bh[' + IfThen(FWindowSettings.Main, '', FWindowSettings.PositionID) + ']', s) then
          begin
            LSize.Height := StrToInt(s);
            if (LSize.Width >= 1) and (LSize.Width <= 65535) and (LSize.Height > 1) and (LSize.Height <= 65535) then
            begin
              FBrowserSize := LSize;
              FLastPosition.BrowserSize := LSize;
              FRememberedPosition.BrowserSize := LSize;
            end;
          end;
        end;
      except end;
    end;

    // recover position from database
    try
      if TrayBrowserRootWindow.KVStoreRead('kvstore', 'window.px[' + IfThen(FWindowSettings.Main, '', FWindowSettings.PositionID) + ']', s) then
      begin
        LPoint.X := StrToInt(s);
        if TrayBrowserRootWindow.KVStoreRead('kvstore', 'window.py[' + IfThen(FWindowSettings.Main, '', FWindowSettings.PositionID) + ']', s) then
        begin
          LPoint.Y := StrToInt(s);
          if (LPoint.X >= -65535) and (LPoint.X <= 65535) and (LPoint.Y >= -65535) and (LPoint.Y <= 65535) then
          begin
            // position recovered, now check if we need to recover monitor only
            LMonitor := Screen.MonitorFromPoint(LPoint, mdNull);
            if Assigned(LMonitor) then
            begin
              if FWindowSettings.RememberPosition = tbrpMonitor then
              begin
                // just position us to the monitor we recovered
                FInManualPositionChange := True;
                LRect := BoundsRect;
                LRect.Location := LMonitor.WorkareaRect.Location;
                BoundsRect := LRect;
                FLastPosition.Window := LRect;
                FRememberedPosition.Window := LRect;
                FInManualPositionChange := False;
                Exit; // position recovered, nothing to do anymore
              end else
              begin
                // recover full position
                FInManualPositionChange := True;
                LRect := BoundsRect;
                LRect.Location := LPoint;
                BoundsRect := LRect;
                FLastPosition.Window := LRect;
                FRememberedPosition.Window := LRect;
                FInManualPositionChange := False;
                FWindowState.PositionRestored := True;
                Result := False; // full position recovered, centering not required
                Exit; // position recovered, nothing to do anymore
              end;
            end;
          end;
        end;
      end;
    except end;
  end;

  // no remembered position, select starting monitor
  if not Assigned(LMonitor) then LMonitor := Screen.MonitorFromPoint(Mouse.CursorPos, mdPrimary); // monitor by mouse position or main monitor
  case FWindowSettings.StartMonitor of
    tbsmID:
    begin
      if (FWindowSettings.StartMonitorID >= 0) and (FWindowSettings.StartMonitorID < Screen.MonitorCount) and Assigned(Screen.Monitors[FWindowSettings.StartMonitorID]) then
        LMonitor := Screen.Monitors[FWindowSettings.StartMonitorID];
    end;

    tbsmXY:
    begin
      LXYMonitor := Screen.MonitorFromPoint(FWindowSettings.StartMonitorPosition, mdNull);
      if Assigned(LXYMonitor) then LMonitor := LXYMonitor;
    end;
  end;
  if not Assigned(LMonitor) then TrayBrowserApplication.Die('Failed to find monitor to run on'); // whoops, this should never ever happen
  FInManualPositionChange := True;
  LRect := BoundsRect;
  LRect.Location := LMonitor.WorkareaRect.Location;
  BoundsRect := LRect;
  FLastPosition.Window := LRect;
  FInManualPositionChange := False;
end;

////////////////////////////////////////////////////////////
// monitor helpers

function TTrayBrowserWindow.GetWindowMonitor(out outMonitor: TMonitor): Integer;
begin
  Result := 0;
  outMonitor := nil;
  // ClientToScreen is unreliable and maximized windows lie slightly offscreen, thus we have to rely on this heuristics
  if WindowState <> wsMaximized then
  begin
    // use left/top as normal
    outMonitor := Screen.MonitorFromPoint(BoundsRect.Location, mdNull);
  end else
  begin
    // use 64 pixel offset compensation for maximized windows
    Result := 64;
    outMonitor := Screen.MonitorFromPoint(Point(Left + 64, Top + 64), mdNull);
  end;
end;

////////////////////////////////////////////////////////////
// scaling helpers

function TTrayBrowserWindow.ScaleToBrowser(const inSize: Integer; const inDPI: Integer): Integer;
begin
  if TrayBrowserApplication.Settings.ScalingMode <> tbscNoScale then Result := (inSize * 96) div inDPI else Result := inSize;
end;

function TTrayBrowserWindow.ScaleFromBrowser(const InSize: Integer; const inDPI: Integer): Integer;
begin
  if TrayBrowserApplication.Settings.ScalingMode <> tbscNoScale then Result := (inSize * inDPI) div 96 else Result := inSize;
end;

function TTrayBrowserWindow.GetScalingAlignment(const inMonitor: TMonitor): Integer;
var
  LDivisor: Integer = 0;
  LDivResult: Integer = 0;
  LDivRemainder: Integer = 0;
  LFindResult: Integer = 0;
  LFindRemainder: Integer = 0;
begin
  if TrayBrowserApplication.Settings.ScalingMode = tbscNoScale then
  begin
    // we are told to ignore scaling
    Result := 1;
    Exit;
  end;

  // the general integer scaling formula is: ppi/(ppi-96) for positive scaling, ppi/96 for negative scaling, then multiply by some unknown X until it gets integer
  Result := 0; // 0 indicates we could not find proper alignment
  if inMonitor.PixelsPerInch >= 96 then
  begin
    if inMonitor.PixelsPerInch = 96 then
    begin
      // no scaling
      Result := 1;
      Exit;
    end else if inMonitor.PixelsPerInch > 96 then
    begin
      // positive scaling
      LDivisor := inMonitor.PixelsPerInch - 96;
      DivMod(inMonitor.PixelsPerInch, LDivisor, LDivResult, LDivRemainder);
    end else
    begin
      // negative scaling
      LDivisor := 96;
      LDivResult := 0;
      LDivRemainder := inMonitor.PixelsPerInch;
    end;

    if LDivRemainder = 0 then
    begin
      // yay, already found our factor
      Result := LDivResult;
      if Result > FWindowSettings.ScaleAlignmentMax then Result := 0; // the factor found is over the limit
      Exit;
    end else
    begin
      // another easy way out is when divisor is evenly divisible by the remainder, then we just multiply by this value, add 1 and we are out
      DivMod(LDivisor, LDivRemainder, LFindResult, LFindRemainder);
      if (LFindRemainder = 0) then
      begin
        // seem like to be the case
        Result := (LDivResult * LFindResult) + 1;
        if Result > FWindowSettings.ScaleAlignmentMax then Result := 0; // the factor found is over the limit
        Exit;
      end;

      // no easy way for us, try going up until we are integer or over the limit
      LFindResult := LDivResult;
      LFindRemainder := LDivRemainder;
      repeat
        LFindResult := LFindResult + LDivResult;
        LFindRemainder := LFindRemainder + LDivRemainder;
        if LFindRemainder >= LDivisor then
        begin
          LFindResult := LFindResult + 1;
          LFindRemainder := LFindRemainder - LDivisor;
        end;
        if LFindRemainder = 0 then begin
          // found closest integer factor value
          Result := LFindResult;
          Exit;
        end;
      until (LFindResult > FWindowSettings.ScaleAlignmentMax) or ((LFindResult = FWindowSettings.ScaleAlignmentMax) and (LFindRemainder > 0));
    end;
  end;
end;

