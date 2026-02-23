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

// tray icon balloon (notification) handling, included from TBRootWindow unit

procedure TTrayBrowserRootWindow.ShowBalloon(const inWindowID: String; const inHint: String; const inTitle: String; const inTimeout: Integer);
begin
  HideBalloon;
  TrayIcon.BalloonTimeout := inTimeout;
  TrayIcon.BalloonHint := inHint;
  TrayIcon.BalloonTitle := inTitle;
  TrayIcon.ShowBalloonHint;
  BalloonWindowID := inWindowID;
  BalloonShowing := True;
end;

procedure TTrayBrowserRootWindow.HideBalloon;
begin
  if BalloonShowing then
  begin
    {$IFDEF WINDOWS}
    // on Windows 10+ destroying notification window is the only way to remove toast, and we need to recover event handler afterwards
    TrayIcon.Hide;
    if TrayBrowserApplication.Settings.ShowTrayIcon then TrayIcon.Show;
    SetWindowSubclass(TrayIcon.Handle, @TBTraySubWndProc, $C00, 1);
    {$ENDIF}
  end;

  BalloonShowing := False;
  BalloonWindowID := '';
end;

// custom windows-only tray notification events handling

{$IFDEF WINDOWS}
procedure TTrayBrowserRootWindow.WMTrayBalloonClick(var Message: TMessage);
var
  LWindow: TTrayBrowserWindow;
begin
  BalloonShowing := False;
  if CheckApplicationVisibility(True) then FocusTimer.Enabled := False; // make sure we do not lose focus flag when clicking on balloon
  LWindow := TTrayBrowserWindow(GetBrowserWindow(BalloonWindowID));
  if Assigned(LWindow) then LWindow.PostJSEvent('"onballoonclick"');
end;

procedure TTrayBrowserRootWindow.WMTrayBalloonHide(var Message: TMessage);
var
  LWindow: TTrayBrowserWindow;
begin
  BalloonShowing := False;
  if CheckApplicationVisibility(True) then FocusTimer.Enabled := False; // make sure we do not lose focus flag when clicking on balloon hide button
  LWindow := TTrayBrowserWindow(GetBrowserWindow(BalloonWindowID));
  if Assigned(LWindow) then LWindow.PostJSEvent('"onballoonhide"');
end;

procedure TTrayBrowserRootWindow.WMTrayBalloonTimeout(var Message: TMessage);
var
  LWindow: TTrayBrowserWindow;
begin
  BalloonShowing := False;
  LWindow := TTrayBrowserWindow(GetBrowserWindow(BalloonWindowID));
  if Assigned(LWindow) then LWindow.PostJSEvent('"onballoonhide"');
end;

// custom windows-only tray icon event handler that allows for windows-specific notification handling

function TBTraySubWndProc(hwnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM; subId: UINT_PTR; refData: DWORD_PTR): LRESULT; stdcall;
begin
  if msg = WM_TRAYICON then
  begin
    case lParam of
      NIN_BALLOONSHOW:
      begin
        PostMessage(TrayBrowserRootWindow.Handle, TB_MSG_BALLOONSHOW, 0, 0);
        Result := 1;
        Exit;
      end;

      NIN_BALLOONHIDE:
      begin
        PostMessage(TrayBrowserRootWindow.Handle, TB_MSG_BALLOONHIDE, 0, 0);
        Result := 1;
        Exit;
      end;

      NIN_BALLOONTIMEOUT:
      begin
        PostMessage(TrayBrowserRootWindow.Handle, TB_MSG_BALLOONTIMEOUT, 0, 0);
        Result := 1;
        Exit;
      end;

      NIN_BALLOONUSERCLICK:
      begin
        PostMessage(TrayBrowserRootWindow.Handle, TB_MSG_BALLOONCLICK, 0, 0);
        Result := 1;
        Exit;
      end;
    end;
  end;

  Result := DefSubclassProc(hwnd, msg, wParam, lParam);
end;
{$ENDIF}

