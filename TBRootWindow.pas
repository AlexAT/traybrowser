unit TBRootWindow;

{$mode ObjFPC}{$modeswitch AdvancedRecords}{$H+}{$codepage utf8}

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

interface

// root window for the TrayBrowser process
// this window never becomes visible nor is shown on taskbar separately itself, and exists solely for:
//  - main taskbar icon or element handling (or should we say not handling as we are never visible)
//  - global tray icon handling
//  - global tray icon menu handling
//  - global tray icon popups handling
//  - browser window creation and registration
//  - application termination, visibility, focus and autohide control
//  - global key-value storage (both SQLite3-based and in-memory)
//  - relieving Windows LCL quirk of small meaningless window appearing along main taskbar element when root application window is minimized and then hidden/shown again (never visible = never happens)

uses
  Windows, CommCtrl, ShellApi, Win32Extra,
  Classes, SysUtils, StrUtils, Generics.Collections, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  SQLDB, SQLite3Conn, SQLite3Dyn,
  TBTrayBrowser, TBWindow, TBZMQ;

type
  { TTrayBrowserMenuItem }
  TTrayBrowserMenuItem = class(TMenuItem)
    public
      XTBWindowID: String;
      XTBCallbackID: Integer;
  end;

  { TTrayBrowserRootWindow }
  TTrayBrowserRootWindow = class(TForm)
    AppProps: TApplicationProperties;
    FocusTimer: TTimer;
    KVDB: TSQLite3Connection;
    KVQuery: TSQLQuery;
    KVTransaction: TSQLTransaction;
    TrayMenuRetry: TMenuItem;
    TrayMenuExit: TMenuItem;
    TrayMenu: TPopupMenu;
    TrayIcon: TTrayIcon;

    procedure AppPropsQueryEndSession(var Cancel: Boolean);
    procedure FocusTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
    procedure TrayMenuExitClick(Sender: TObject);
    procedure TrayMenuPopup(Sender: TObject);
    procedure TrayMenuRetryClick(Sender: TObject);

    protected
      ////////////////////////////////////////////////////////////
      // application visibility and focus control
      WindowFocusList: TStringList;

      ////////////////////////////////////////////////////////////
      // tray icon notifications
      BalloonShowing: Boolean;
      BalloonWindowID: String;

      ////////////////////////////////////////////////////////////
      // window registry
      WindowRegistry: TTBWindowList;

      ////////////////////////////////////////////////////////////
      // key-value storage
      MemoryKVStore: TTBStringDict;

      ////////////////////////////////////////////////////////////
      // ZMQ messaging
      ZMQPollerThread: TThread;

      ////////////////////////////////////////////////////////////
      // application visibility and focus control
      function CheckApplicationMinimized: Boolean;
      function CheckApplicationStayOnTop: Boolean;
      function CheckApplicationKiosk: Boolean;
      procedure ApplicationActivate(Sender: TObject);
      procedure ApplicationDeactivate(Sender: TObject);

      ////////////////////////////////////////////////////////////
      // tray icon
      function TrayIconClickCheckMonitorChange: Boolean;

      ////////////////////////////////////////////////////////////
      // tray icon menu
      procedure ClearMenuOrphans;
      procedure UserMenuClick(Sender: TObject);

      ////////////////////////////////////////////////////////////
      // tray icon notifications
      {$IFDEF WINDOWS}
      procedure WMTrayBalloonClick(var Message: TMessage); message TB_MSG_BALLOONCLICK;
      procedure WMTrayBalloonHide(var Message: TMessage); message TB_MSG_BALLOONHIDE;
      procedure WMTrayBalloonTimeout(var Message: TMessage); message TB_MSG_BALLOONTIMEOUT;
      {$ENDIF}

      ////////////////////////////////////////////////////////////
      // key-value storage
      procedure InitKVStore;

      ////////////////////////////////////////////////////////////
      // ZMQ messaging
      procedure StopZMQPollerThread;

    public
      ////////////////////////////////////////////////////////////
      // application visibility and focus control
      ApplicationInFocus: Boolean;
      ApplicationChangingVisibility: Boolean; // this flag prevents focus list changes during application show

      ////////////////////////////////////////////////////////////
      // window registry
      MainWindowID: String;

      ////////////////////////////////////////////////////////////
      // application termination
      ExitRequestReceived: Boolean;

      ////////////////////////////////////////////////////////////
      // application initialization
      procedure ApplySettings;

      ////////////////////////////////////////////////////////////
      // application visibility and focus control
      procedure ShowApplication(const inGetFocus: Boolean = True);
      procedure HideApplication;
      procedure FocusEligibleWindow;
      function CheckApplicationVisibility(const inAnyWindow: Boolean = False): Boolean;
      function CheckApplicationFocus: Boolean;
      procedure WindowGotFocus(inWindowID: String);

      ////////////////////////////////////////////////////////////
      // tray icon menu
      procedure AddUserMenuItem(const inWindowID: String; const inCallbackID: Integer; const inMenuText: String);
      procedure ClearUserMenu(const inWindowID: String);
      procedure UpdateMenuRetryState;

      ////////////////////////////////////////////////////////////
      // tray icon notifications
      procedure ShowBalloon(const inWindowID: String; const inHint: String; const inTitle: String; const inTimeout: Integer);
      procedure HideBalloon;

      ////////////////////////////////////////////////////////////
      // window registry
      function GetBrowserWindow(const inWindowID: String): TForm;
      procedure GetBrowserWindowIdList(const inIDList: TStringList);
      function RegisterBrowserWindow(const inWindow: TForm): String;
      procedure RelinkBrowserWindow(const inWindowID: String; const inWindow: TForm);
      function UnregisterBrowserWindow(const inWindowID: String): Boolean;
      function CreateBrowserWindow: TForm;

      ////////////////////////////////////////////////////////////
      // key-value storage
      function KVStoreRead(const inTable: String; const inKey: String; out outValue: String): Boolean;
      function KVStoreWrite(const inTable: String; const inKey: String; const inValue: String; const inCommit: Boolean = True): Boolean;
      function KVStoreDelete(const inTable: String; const inKey: String; const inCommit: Boolean = True): Boolean;
      procedure KVStoreCommit;
      procedure KVStoreRollback;
      function MemoryKVStoreRead(const inKey: String; out outValue: String): Boolean;
      function MemoryKVStoreWrite(const inKey: String; const inValue: String): Boolean;
      function MemoryKVStoreDelete(const inKey: String): Boolean;

      ////////////////////////////////////////////////////////////
      // application termination
      procedure TerminateTrayBrowser;
  end;

////////////////////////////////////////////////////////////
// windows platform specifics

{$IFDEF WINDOWS}
function TBTraySubWndProc(hwnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM; subId: UINT_PTR; refData: DWORD_PTR): LRESULT; stdcall; // see TBRootWindow_TrayBalloon.inc
{$ENDIF}

////////////////////////////////////////////////////////////
// global root window singleton instance

var
  TrayBrowserRootWindow: TTrayBrowserRootWindow;

implementation

{$R *.lfm}

{ TTrayBrowserRootWindow }

////////////////////////////////////////////////////////////
// bits and pieces
{$INCLUDE TBRootWindow_Visibility.inc}
{$INCLUDE TBRootWindow_TrayIcon.inc}
{$INCLUDE TBRootWindow_TrayMenu.inc}
{$INCLUDE TBRootWindow_TrayBalloon.inc}
{$INCLUDE TBRootWindow_Windows.inc}
{$INCLUDE TBRootWindow_KVStore.inc}
////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////
// minimalistic root form creation and termination handling

procedure TTrayBrowserRootWindow.FormCreate(Sender: TObject);
var
  LWindow: TTrayBrowserWindow;
begin
  ApplicationInFocus := False;
  ApplicationChangingVisibility := False;
  BalloonShowing := False;
  ExitRequestReceived := False;
  MainWindowID := '';
  WindowRegistry := TTBWindowList.Create;
  WindowFocusList := TStringList.Create;
  ZMQPollerThread := nil;

  // we stay hidden anyways, but set us to one pixel just to be safe we are not showing anything weird in case we are shown somehow
  Width := 1;
  Height := 1;

  // initialize tray icon
  TrayIcon.Icon.Assign(Application.Icon);

  // apply settings
  ApplySettings;

  // set application focus events
  Application.OnActivate := @ApplicationActivate;
  Application.OnDeactivate := @ApplicationDeactivate;

  {$IFDEF WINDOWS}
  // on Windows set our own subclass handler for tray icon events to handle popups properly
  SetWindowSubclass(TrayIcon.Handle, @TBTraySubWndProc, $C00, 1);
  {$ENDIF}

  // initialize key-value storage
  InitKVStore;

  // create and start main ZMQ poller thread
  ZMQPollerThread := TTrayBrowserZMQPollerThread.Create(True);
  ZMQPollerThread.FreeOnTerminate := False;
  ZMQPollerThread.OnTerminate := nil;
  ZMQPollerThread.Start;

  // create main browser window
  TrayBrowserApplication.TempWindowSettings := TrayBrowserApplication.MainWindowSettings;
  LWindow := TTrayBrowserWindow(CreateBrowserWindow);
  if not Assigned(LWindow) then TrayBrowserApplication.Die('Failed to create main browser window');
  MainWindowID := LWindow.FWindowID;
end;

procedure TTrayBrowserRootWindow.ApplySettings;
begin
  TrayIcon.Hint := TrayBrowserApplication.Settings.Hint;
  if TrayBrowserApplication.Settings.MenuDisable then TrayIcon.PopUpMenu := nil else TrayIcon.PopUpMenu := TrayMenu;
  TrayMenuExit.Caption := TrayBrowserApplication.Settings.MenuExitCaption;
  TrayMenuRetry.Caption := TrayBrowserApplication.Settings.MenuRetryCaption;
  TrayMenuExit.Visible := not TrayBrowserApplication.Settings.MenuHideExit;
  if TrayBrowserApplication.Settings.ShowTrayIcon then TrayIcon.Show else TrayIcon.Hide;
end;

procedure TTrayBrowserRootWindow.FormDestroy(Sender: TObject);
begin
  StopZMQPollerThread; // normally in onClose, but better safe than sorry
  FreeAndNil(WindowFocusList);
  FreeAndNil(MemoryKVStore);
  FreeAndNil(WindowRegistry);
end;

////////////////////////////////////////////////////////////
// minimalistic root window visibility and window state handling

procedure TTrayBrowserRootWindow.FormShow(Sender: TObject);
begin
  Hide; // keep us hidden
end;

procedure TTrayBrowserRootWindow.FormWindowStateChange(Sender: TObject);
begin
  // prevent any window state changes
  WindowState := wsNormal;
end;

////////////////////////////////////////////////////////////
// application termination handling
// take care all exit requests are processed inside windows to make sure browsers are terminated gracefully, so if any window is hung (this should not happen) exit will not happen

procedure TTrayBrowserRootWindow.TerminateTrayBrowser;
var
  LWPair: TTBWindowPair;
begin
  if ExitRequestReceived then
  begin
    // exit request already received
    if WindowRegistry.Count = 0 then Close // terminate application when no windows are remaining
      else if (WindowRegistry.Count = 1) and WindowRegistry.ContainsKey(MainWindowID) then
        TTrayBrowserWindow(WindowRegistry[MainWindowID]).ExitRequest; // only main window remains, send termination request to the main window
    TrayBrowserSynchronizeWidgetSet; // allow window to catch request
    Exit;
  end;

  // mark receiving exit request
  ExitRequestReceived := True;

  // hide application
  HideApplication;

  // disable tray menu elements and try to close menu (keep tray icon intact for now to show user we are there yet)
  TrayMenuRetry.Visible := False;
  TrayMenuExit.Visible := False;
  TrayMenu.Close;

  // another fix for LCL quirk, if tray menu is open when exiting, not doing this causes application to wait until it is closed to exit
  TrayIcon.PopUpMenu := nil;
  TrayIcon.Hide;
  if TrayBrowserApplication.Settings.ShowTrayIcon then TrayIcon.Show;

  if (WindowRegistry.Count > 1) or (not WindowRegistry.ContainsKey(MainWindowID)) then // the second check is a safety measure
  begin
    // send exit requests to all windows but main window
    for LWPair in WindowRegistry do
      if not TTrayBrowserWindow(LWPair.Value).FWindowSettings.Main then TTrayBrowserWindow(LWPair.Value).ExitRequest;
  end else TTrayBrowserWindow(WindowRegistry[MainWindowID]).ExitRequest; // only main window is present, send exit request to it

  TrayBrowserSynchronizeWidgetSet; // allow windows to catch requests
  if WindowRegistry.Count = 0 then Close; // well, this should not happen here, but if we already have no windows, we can terminate immediately
end;

procedure TTrayBrowserRootWindow.AppPropsQueryEndSession(var Cancel: Boolean);
begin
  Cancel := False;
  TerminateTrayBrowser;
end;

procedure TTrayBrowserRootWindow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  StopZMQPollerThread;
  Application.Terminate;
end;

procedure TTrayBrowserRootWindow.StopZMQPollerThread;
begin
  if Assigned(ZMQPollerThread) then
  begin
    try
      ZMQPollerThread.Terminate;
      if (not ZMQPollerThread.Finished) and (not ZMQPollerThread.Suspended) then ZMQPollerThread.WaitFor;
    except end;
    FreeAndNil(ZMQPollerThread);
  end;
end;

end.

