unit TBWindow;

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

// TrayBrowser browser window
// each browser window handles the embedded browser user interacts with and all its function
// the first window created is registered as main window that handles the rest of the application windows
// this is actually the main TrayBrowser unit implementing most of the TrayBrowser functions
// as it is ultimately large, it included a bunch of TBWindow_*.inc files containing functions specific for each area of operation

uses
  Windows, Win32Extra,
  CommCtrl, Messages, SyncObjs, Generics.Collections, Process,
  Classes, SysUtils, Forms, Controls, Dialogs, ExtCtrls, Buttons, Graphics, ActnList, Menus, StrUtils, Math, htmlelements, LCLIntf, fpjson, RegExpr,
  uCEFApplication, uCEFChromium, uCEFLinkedWindowParent, uCEFInterfaces, uCEFConstants, uCEFTypes, uCEFDictionaryValue, uCEFMiscFunctions,
  TBTrayBrowser;

const
  tbeHide = 0;
  tbeNormal = 1;
  tbeMinimize = 2;
  tbeMaximize = 3;
  tbeNoWait = 128; // EXEC_NOWAIT flag
  tbeSync = 256; // internal EXEC_SYNC flag
  tbeMaxProcesses = 100; // process limit to prevent accidental process bombing

type
  TTBTrayPosition = (tbtpTopLeft, tbtpTopRight, tbtpBottomLeft, tbtpBottomRight, tbtpUnknown);

  TTBLastPosition = record
    Window: TRect;
    BrowserSize: TSize;
  end;

  TTBRememberedPosition = record
    Active: Boolean;
    Window: TRect;
    BrowserSize: TSize;
    PixelBrowserSize: TSize;
    Monitor: TRect;
    DPI: Integer;
  end;

  TTBMonitorInfo = record
    Active, Main, Tray: Integer;
    // active monitor parameters
    WorkArea: TRect;
    RealBounds: TRect;
    DPI: Integer;
    ScalingAlignment: Integer;
    BorderWidth: Integer;
    TitleHeight: Integer;
    TrayPosition: TTBTrayPosition;
  end;

  TTBExecWindowMode = (tbewHide = tbeHide, tbewNormal = tbeNormal, tbewMinimize = tbeMinimize, tbewMaximize = tbeMaximize);
  TTBExecProcess = class(TProcess)
    public
      XTBGUID: String;
      XTBStdOut: String;
      XTBStdErr: String;
      XTBExitCode: Integer; // if non-zero, used instead of ExitStatus
      XTBErrorMessage: String;
      XTBIsSynchronous: Boolean;
      XTBIsIndependent: Boolean;
      XTBFinished: Boolean;
      XTBTimeout: Integer;
      XTBCallbackID: Integer;
  end;
  TTBExecProcessDict = specialize TDictionary<String, TTBExecProcess>;
  TTBExecProcessPair = specialize TPair<String, TTBExecProcess>;

  // this one makes each line accessing it longer, but as we need to massively copy state on window restart, it is better than forgetting to copy something
  TTBWindowState = record
    PreMinimizeWindowState: TWindowState; // for minimization we also need to remember which window state we went there from, but this needs to be public as root window can use that
    WasVisible: Boolean; // this is only used by the root window on global application hiding to remember window states before hiding
    InLoadError: Boolean; // this is used by the root window to determine retry menu element state
    ErrorPageLoadNeeded: Boolean;
    BrowserErrorText: String; // browser error text to pass to the error page load event
    RetryTimer: Integer;
    ExitEventNeeded: Boolean;
    PositionRestored: Boolean;

    // these require creation/destruction
    JSEventQueueLock: TCriticalSection; // used to lock event queue
    JSEventQueue: TStringList;
    ExecProcesses: TTBExecProcessDict;
  end;

  TTBFirstShowState = record
    Pending: Boolean;
    Restarting: Boolean;
  end;

  TTBCustomDragState = record
    Dragging: Boolean;
    WindowPosition: TPoint; // window position on custom drag region dragging start
    MousePosition: TPoint; // mouse position on custom drag region dragging start
  end;

  TTBTerminationState = record
    Requested: Boolean;
    EventComplete: Boolean;
    Terminating: Boolean;
    CanClose: Boolean;
    Timer: Integer;
  end;

  { TTrayBrowserWindow }
  TTrayBrowserWindow = class(TForm)
    CEFBrowser: TCEFLinkedWindowParent;
    BrowserPanel: TPanel;
    CustomDragRegion: TPanel;
    BorderPanel: TPanel;
    DragTimer: TTimer;
    JSEventTimer: TTimer;
    ServiceTimer: TTimer;
    procedure BrowserPanelDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure CEFBrowserDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ChromiumAfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure ChromiumBeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure ChromiumBeforeContextMenu(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure ChromiumBeforeDownload(Sender: TObject; const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring; const callback: ICefBeforeDownloadCallback; var aResult: Boolean);
    procedure ChromiumBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; popup_id: Integer; const targetUrl, targetFrameName: ustring;
      targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient;
      var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure ChromiumCanDownload(Sender: TObject; const browser: ICefBrowser; const url, request_method: ustring; var aResult: boolean);
    procedure ChromiumCertificateError(Sender: TObject; const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefCallback; out Result: Boolean);
    procedure ChromiumChromeCommand(Sender: TObject; const browser: ICefBrowser; command_id: Integer; disposition: TCefWindowOpenDisposition; var aResult: boolean);
    procedure ChromiumClose(Sender: TObject; const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
    procedure ChromiumLoadEnd(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
    procedure ChromiumLoadError(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring);
    procedure ChromiumLoadStart(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType);
    procedure ChromiumOpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);
    procedure ChromiumPreKeyEvent(Sender: TObject; const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean; out Result: Boolean);
    procedure CustomDragRegionDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure CustomDragRegionEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure CustomDragRegionStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure JSEventTimerTimer(Sender: TObject);
    procedure ServiceTimerTimer(Sender: TObject);
    procedure DragTimerTimer(Sender: TObject);

    protected
      ////////////////////////////////////////////////////////////
      // browser
      Chromium: TChromium;

      ////////////////////////////////////////////////////////////
      // internal window state
      FInitialized: Boolean;
      FIsBrowserRestarting: Boolean;
      FForceWindowState: Boolean;
      FFirstShow: TTBFirstShowState;

      ////////////////////////////////////////////////////////////
      // window movement state
      FIsMoving: Boolean;
      FCustomDrag: TTBCustomDragState;

      ////////////////////////////////////////////////////////////
      // browser operations
      FCEFBrowserOpLock: TCriticalSection; // used to lock for variable modifications in Chromium thread

      ////////////////////////////////////////////////////////////
      // termination process state
      FExit: TTBTerminationState;

      ////////////////////////////////////////////////////////////
      // windows platform specifics
      FLCLWndProc: Windows.WNDPROC;

      ////////////////////////////////////////////////////////////
      // window initialization
      procedure ApplySafeSettings;
      procedure ApplyUnsafeSettings;
      procedure RestartFromSourceWindow;

      ////////////////////////////////////////////////////////////
      // window positioning operations
      function SetStartingPosition: Boolean; // return true if centering is necessary
      function GetScalingAlignment(const inMonitor: TMonitor): Integer;
      function ScaleToBrowser(const inSize: Integer; const inDPI: Integer): Integer;
      function ScaleFromBrowser(const inSize: Integer; const inDPI: Integer): Integer;
      procedure WMEnterSizeMove;
      procedure WMExitSizeMove;

      ////////////////////////////////////////////////////////////
      // window movement operations
      procedure ResetCustomDragRegion;
      procedure CustomDragDrag;

      ////////////////////////////////////////////////////////////
      // browser operations
      procedure InitializeBrowser(const inExistingBrowser: Boolean = False);
      procedure BrowserCreatedMsg(var Message: TMessage); message CEF_AFTERCREATED;
      procedure BrowserOnLoadStartMsg(var Message: TMessage); message TB_MSG_BROWSER_ONLOADSTART;
      procedure BrowserOnRemoteLoadStartMsg(var Message: TMessage); message TB_MSG_BROWSER_ONREMOTELOADSTART;
      procedure BrowserOnLoadErrorMsg(var Message: TMessage); message TB_MSG_BROWSER_ONLOADERROR;
      function ServiceCheckLoadErrorState: Boolean;

      ////////////////////////////////////////////////////////////
      // external process execution
      function SpawnExecProcess(const inCommand: String; const inParams: TProcessStringList; const inCallbackID: Integer; const inTimeout: Integer; const inWindowMode: TTBExecWindowMode;
        const inSynchronous: Boolean; const inIndependent: Boolean): String;
      procedure OnExecProcessEvent(Sender: TObject; Context: TObject; Status: TRunCommandEventCode; const Message: String);
      procedure ReadExecProcessStreams(const inProcess: TTBExecProcess);
      function ServiceReapExecProcesses: Boolean;
      procedure KillExecProcesses;

      ////////////////////////////////////////////////////////////
      // termination
      function ServiceCheckTerminationTimeout: Boolean;

      ////////////////////////////////////////////////////////////
      // JS API
      function jsGetSize(const inArgc: Integer; const inRequest: TTBKVMessage; const inPixelSize: Boolean): TTBJSVariant;
      function jsSetSize(const inArgc: Integer; const inRequest: TTBKVMessage; const inPixelSize: Boolean): TTBJSVariant;
      function jsGetPosition(const inArgc: Integer; const inRequest: TTBKVMessage; const inPixelSize: Boolean; const inDesktop: Boolean): TTBJSVariant;
      function jsSetPosition(const inArgc: Integer; const inRequest: TTBKVMessage; const inPixelSize: Boolean; const inDesktop: Boolean): TTBJSVariant;
      function jsIsInFocus(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsWasPositionRestored(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;

      function jsGetMonitorSize(const inArgc: Integer; const inRequest: TTBKVMessage; const inPixelSize: Boolean): TTBJSVariant;
      function jsGetDesktopSize(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsGetWindowInfo(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsGetScreenInfo(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsGetColorScheme(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;

      function jsMinimize(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsMaximize(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsRestore(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsGetWindowState(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;

      function jsSetCustomDragRegion(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;

      function jsStart(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsNavigate(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsRestartBrowser(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;

      function jsGetParameters(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsGetCommandLine(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsGetLoggedInUser(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsSetTitle(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsSetHint(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsGetApplicationSettings(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsSetApplicationSettings(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsGetWindowSettings(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsSetWindowSettings(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;

      function jsAddMenuItem(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsClearMenu(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsClearMenuAll(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsShowBalloon(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsHideBalloon(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;

      function jsReadKV(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsWriteKV(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsDeleteKV(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsReadTempKV(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsWriteTempKV(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsDeleteTempKV(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;

      function jsRegisterEvent(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsClearEvent(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;

      function jsOpenBrowser(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsExec(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;

      function jsGetWindowID(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsGetMainWindowId(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsIsMainWindow(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsGetWindowList(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsCreateWindow(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsWindowMessage(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsWindowBroadcast(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsIsAnyWindowVisible(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsIsApplicationVisible(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsIsApplicationInFocus(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsApplicationShow(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsApplicationHide(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsApplicationTerminate(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsWindowExists(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsGetSavedPosition(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsSetSavedPosition(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;

      procedure jsWindowAPIAlias(const inFunction: String; const inArgc: Integer; const inRequest: TTBKVMessage; const inReply: TTBKVMessage; const inMainWindowOnly: Boolean = True);

      function jsDebugLog(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;

      function jsGetWidth(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsGetHeight(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsGetLeft(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsGetTop(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsGetScreenWidth(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsGetScreenHeight(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;

      function jsGetApplicationPath(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsGetRuntimePath(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsGetSettingsPath(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsExecSyncCheck(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;

      function GetWindowSizePositionJSON: String;
      function ColorToCSS(const inColor: TColor): String;

      function MsgArgToBool(const inMessage: TTBKVMessage; const inIndex: String; const inUseLiterals: Boolean = False; const inLiteralsOnly: Boolean = False): Boolean;
      function MsgArgToInt(const inMessage: TTBKVMessage; const inIndex: String): Integer;
      function MsgArgToIntWithNullUndef(const inMessage: TTBKVMessage; const inIndex: String; const inOnNullUndefined: Integer = 0): Integer;
      function MsgArgToFloat(const inMessage: TTBKVMessage; const inIndex: String): Float;
      function MsgArgToFloatWithNullUndef(const inMessage: TTBKVMessage; const inIndex: String; const inOnNullUndefined: Float = 0): Float;
      function MsgArgToString(const inMessage: TTBKVMessage; const inIndex: String; const inMaxLength: Integer = -1): String;
      function MsgArgToStringWithNullUndef(const inMessage: TTBKVMessage; const inIndex: String; const inOnNullUndefined: String = ''; const inMaxLength: Integer = -1): String;
      procedure MsgArgObjectToStringDict(const inMessage: TTBKVMessage; const inIndex: String; const inDict: TTBStringDict; const inLowerCaseKeys: Boolean = True; const inWithArrays: Boolean = False; const inMaxCount: Integer = -1; const inMaxLength: Integer = -1);
      procedure MsgArgObjectToStringDictWithNullUndef(const inMessage: TTBKVMessage; const inIndex: String; const inDict: TTBStringDict; const inLowerCaseKeys: Boolean = True; const inWithArrays: Boolean = False; const inOnNullUndefined: String = ''; const inMaxCount: Integer = -1; const inMaxLength: Integer = -1);
      function IsMsgArgScalar(const inMessage: TTBKVMessage; const inIndex: String): Boolean;

      ////////////////////////////////////////////////////////////
      // windows platform specifics
      procedure WMMove(var Message: TWMMove); message WM_MOVE;
      procedure WMPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
      procedure WMEnterMenuLoop(var Message: TMessage); message WM_ENTERMENULOOP;
      procedure WMExitMenuLoop(var Message: TMessage); message WM_EXITMENULOOP;
      procedure WMQueryEndSession(var Message: TWMQueryEndSession); message WM_QUERYENDSESSION;

    public
      ////////////////////////////////////////////////////////////
      // general window settings and state
      FWindowID: String;
      FWindowSettings: TTBWindowSettings;
      FWindowState: TTBWindowState;

      ////////////////////////////////////////////////////////////
      // window positioning state
      FBrowserSize: TSize; // actual browser size in browser units (dots)
      FMonitorInfo: TTBMonitorInfo; // monitor information for API
      FLastPosition: TTBLastPosition; // last really set window position/size and browser size for event generation (both), window restarts (position only), onshow repositioning (position only)
      FRememberedPosition: TTBRememberedPosition; // here window position/size and other monitor parameters are remembered before maximizing, as we need them back on window restore and use them for JS API

      ////////////////////////////////////////////////////////////
      // volatile window positioning state (re-entry prevention)
      FInPositioning: Boolean;
      FInPositionChanging: Boolean;
      FInManualPositionChange: Boolean;

      ////////////////////////////////////////////////////////////
      // browser operations
      FCEFBrowserParams: ICefDictionaryValue;

      ////////////////////////////////////////////////////////////
      // window positioning operations
      procedure PositionOurselves(inSynchronizeWidgetSet: Boolean = True; inKeepMonitor: Boolean = False; inCenterOnMonitor: Boolean = False; inResizeSizeable: Boolean = False; inForceMonitor: TMonitor = nil);
      function GetWindowMonitor(out outMonitor: TMonitor): Integer; // returns detection offset

      ////////////////////////////////////////////////////////////
      // browser operations
      procedure Navigate(const inURL: String);

      ////////////////////////////////////////////////////////////
      // JS API
      procedure JSAPI(const inFunction: String; const inArgc: Integer; const inRequest: TTBKVMessage; const inReply: TTBKVMessage);
      procedure PostJSEvent(const inEventArgs: String);
      function QuoteJSString(const inString: String): String;
      procedure PostJSEventExternal(const inEventArgs: PChar); stdcall;

      // part of JS API is public as we can marshall these calls from the main window
      function jsShow(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsHide(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsFocus(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsClose(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsExit(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;
      function jsIsVisible(const inArgc: Integer; const inRequest: TTBKVMessage): TTBJSVariant;

      ////////////////////////////////////////////////////////////
      // ZMQ message handler
      procedure ZMQMessage(const inCommand: String; const inRequest: TTBKVMessage; const inReply: TTBKVMessage);

      ////////////////////////////////////////////////////////////
      // termination
      procedure ExitRequest;

      ////////////////////////////////////////////////////////////
      // windows platform specifics
      function WMWndProc(const inHWND: HWND; const inUMSG: UINT; const inWPARAM: WParam; const inLPARAM: LParam; out outPrevProc: Windows.WNDPROC): LRESULT;
  end;

////////////////////////////////////////////////////////////
// windows platform specifics
function TBWindowWndProc(hwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam): LRESULT; stdcall;

implementation

uses
  TBRootWindow;

{$R *.lfm}

{ TTrayBrowserWindow }

////////////////////////////////////////////////////////////
// bits and pieces
{$INCLUDE TBWindow_Positioning.inc}
{$INCLUDE TBWindow_Movement.inc}
{$INCLUDE TBWindow_Browser.inc}
{$INCLUDE TBWindow_JSAPI.inc}
{$INCLUDE TBWindow_Exec.inc}
////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////
// window initialization and destruction

procedure TTrayBrowserWindow.FormCreate(Sender: TObject);
var
  l: Long;
begin
  FWindowID := '';
  Chromium := nil;
  FInitialized := False;
  FIsBrowserRestarting := False;
  FForceWindowState := False;
  FIsMoving := False;
  FCustomDrag.Dragging := False;

  FFirstShow.Pending := False;
  FFirstShow.Restarting := False;

  FInPositioning := False;
  FInPositionChanging := False;
  FInManualPositionChange := False;

  FExit.Requested := False;
  FExit.EventComplete := False;
  FExit.Terminating := False;
  FExit.CanClose := False;
  FExit.Timer := -1;

  FWindowState.WasVisible := False;
  FWindowState.PositionRestored := False;
  FWindowState.InLoadError := False;
  FWindowState.PreMinimizeWindowState := wsNormal;
  FWindowState.ExitEventNeeded := False;
  FWindowState.ErrorPageLoadNeeded := False;
  FWindowState.RetryTimer := -1;

  FCEFBrowserOpLock := TCriticalSection.Create;
  FWindowState.JSEventQueue := TStringList.Create;
  FWindowState.JSEventQueueLock := TCriticalSection.Create;
  FWindowState.ExecProcesses := TTBExecProcessDict.Create;

  // load parameters from global process and initialize them, we are being created properly locked for that
  FWindowSettings := TrayBrowserApplication.TempWindowSettings;
  FBrowserSize := FWindowSettings.Size;
  ApplySafeSettings;
  ApplyUnsafeSettings;

  {$IFDEF WINDOWS}
  // windows-specific style change to make custom drag region box transparent
  l := GetWindowLong(CustomDragRegion.Handle, GWL_EXSTYLE);
  SetWindowLong(CustomDragRegion.Handle, GWL_EXSTYLE, l or WS_EX_TRANSPARENT);

  // windows-specific WndProc hook for positioning and prevention of minimize/maximize/move events before they reach LCL
  FLCLWndProc := Windows.WNDPROC(SetWindowLongPtr(Self.Handle, GWL_WNDPROC, PtrInt(@TBWindowWndProc)));
  {$ENDIF}

  // initialize positioning
  FMonitorInfo.Active := Monitor.MonitorNum;
  FMonitorInfo.Main := Monitor.MonitorNum;
  FMonitorInfo.Tray := Monitor.MonitorNum;
  FMonitorInfo.WorkArea := Monitor.WorkareaRect;
  FMonitorInfo.RealBounds := Monitor.BoundsRect;
  FMonitorInfo.DPI := Monitor.PixelsPerInch;
  FMonitorInfo.TrayPosition := tbtpUnknown;
  FMonitorInfo.BorderWidth := 0;
  FMonitorInfo.TitleHeight := 0;
  FMonitorInfo.ScalingAlignment := GetScalingAlignment(Monitor);

  FLastPosition.BrowserSize := FBrowserSize;
  FLastPosition.Window := BoundsRect;

  FRememberedPosition.Active := False;
  FRememberedPosition.Window := BoundsRect;
  FRememberedPosition.Monitor := Monitor.WorkareaRect;
  FRememberedPosition.BrowserSize := FBrowserSize;
  FRememberedPosition.Window := BoundsRect;
  FRememberedPosition.PixelBrowserSize.Width := ScaleToBrowser(FBrowserSize.Width, Monitor.PixelsPerInch);
  FRememberedPosition.PixelBrowserSize.Height := ScaleToBrowser(FBrowserSize.Height, Monitor.PixelsPerInch);
  FRememberedPosition.DPI := FMonitorInfo.DPI;

  // we are good to go, positioning will be reinitialized on the first show
  if not Assigned(FWindowSettings.RestartSource) then
  begin
    // normal browser initialization
    FWindowID := TrayBrowserRootWindow.RegisterBrowserWindow(Self);
    FFirstShow.Pending := True;
    PositionOurselves(False, True, SetStartingPosition); // SetStartingPosition returns if we need centering, this call is required to initialize coordinates for JS API
    InitializeBrowser;
    FInitialized := True;
    if FWindowSettings.ShowOnStart and (not FWindowSettings.KeepHidden) then Visible := True; // show us on startup
  end else
  begin
    // we are restarting from another window, this is tricky process that involves copying a bunch of settings and relinking browser and some structures
    RestartFromSourceWindow;
  end;
  ServiceTimer.Enabled := True;
end;

procedure TTrayBrowserWindow.ApplySafeSettings;
begin
  // set styling
  if FWindowSettings.AlwaysOnTop then FormStyle := fsSystemStayOnTop else FormStyle := fsNormal;
  if FWindowSettings.Main and (not FWindowSettings.ShowOnTaskBar) then ShowInTaskBar := stNever else ShowInTaskBar := stAlways;
  if FWindowSettings.SnapToWindows then SnapOptions.SnapToForms := True;
  SnapOptions.Distance := FWindowSettings.SnapToWindowsDistance;

  // border styling
  BrowserPanel.BorderWidth := 0;
  BrowserPanel.BorderSpacing.Around := 0;
  CEFBrowser.BorderWidth := 0;
  case FWindowSettings.Border of
    tbbtBorderless: ; // default borderless mode
    tbbtWindow: ; // OS window borders
    else BrowserPanel.BorderSpacing.Around := 1; // normal internally provided border
  end;

  // border buttons
  if FWindowSettings.Border = tbbtWindow then
  begin
    if FWindowSettings.CloseButton or FWindowSettings.MaximizeButton or FWindowSettings.MinimizeButton then
    begin
      BorderIcons := [biSystemMenu];
      if FWindowSettings.MaximizeButton then BorderIcons := BorderIcons + [biMaximize];
      if FWindowSettings.MinimizeButton then BorderIcons := BorderIcons + [biMinimize];
    end else
    begin
      BorderIcons := [biMinimize]; // this effectively disables any buttons on Windows, it is not enough to set it empty
    end;
  end;
end;

procedure TTrayBrowserWindow.ApplyUnsafeSettings;
begin
  Icon.Assign(Application.Icon);

  // border styling (take care: BorderStyle cannot be changed at runtime, it causes LCL to crash)
  BorderStyle := bsNone;
  BorderWidth := 0;
  BrowserPanel.BorderStyle := bsNone;
  case FWindowSettings.Border of
    tbbtBorderless: ; // default borderless mode
    tbbtWindow: if FWindowSettings.Sizeable then BorderStyle := bsSizeable else BorderStyle := bsSingle; // OS window borders
  end;
end;

procedure TTrayBrowserWindow.RestartFromSourceWindow;
begin
  // stop all source window timers
  TTrayBrowserWindow(FWindowSettings.RestartSource).DragTimer.Enabled := False;
  TTrayBrowserWindow(FWindowSettings.RestartSource).ServiceTimer.Enabled := False;
  TTrayBrowserWindow(FWindowSettings.RestartSource).JSEventTimer.Enabled := False;

  // set window ID, set first show pending anew
  FWindowID := TTrayBrowserWindow(FWindowSettings.RestartSource).FWindowID;
  FFirstShow.Pending := True;

  // copy state, internal objects are also relinked and reused
  FBrowserSize := TTrayBrowserWindow(FWindowSettings.RestartSource).FBrowserSize;
  FMonitorInfo := TTrayBrowserWindow(FWindowSettings.RestartSource).FMonitorInfo;
  FLastPosition := TTrayBrowserWindow(FWindowSettings.RestartSource).FLastPosition;
  FRememberedPosition := TTrayBrowserWindow(FWindowSettings.RestartSource).FRememberedPosition;
  FreeAndNil(FWindowState.ExecProcesses);
  FreeAndNil(FWindowState.JSEventQueue);
  FreeAndNil(FWindowState.JSEventQueueLock);
  FWindowState := TTrayBrowserWindow(FWindowSettings.RestartSource).FWindowState;

  // browser and its parameters are relinked
  FCEFBrowserParams := TTrayBrowserWindow(FWindowSettings.RestartSource).FCEFBrowserParams;
  Chromium := TTrayBrowserWindow(FWindowSettings.RestartSource).Chromium;
  TTrayBrowserWindow(FWindowSettings.RestartSource).CEFBrowser.Chromium := nil;
  InitializeBrowser(True);

  // here window ID is relinked to us so all events start coming to us from this point onwards
  try TrayBrowserRootWindow.RelinkBrowserWindow(FWindowID, Self); except end; // if this fails, we are doomed, but anyways
  TTrayBrowserWindow(FWindowSettings.RestartSource).FWindowID := ''; // and reset source window ID to nothing to make sure it does not deregister

  // here we take care to make sure original window does not free anything that was relinked
  FWindowState.JSEventQueueLock.Acquire;
  TTrayBrowserWindow(FWindowSettings.RestartSource).JSEventTimer.Enabled := False;
  TTrayBrowserWindow(FWindowSettings.RestartSource).FWindowState.JSEventQueue := nil;
  TTrayBrowserWindow(FWindowSettings.RestartSource).FWindowState.JSEventQueueLock := nil;
  FWindowState.JSEventQueueLock.Release;
  TTrayBrowserWindow(FWindowSettings.RestartSource).FWindowState.ExecProcesses := nil;
  TTrayBrowserWindow(FWindowSettings.RestartSource).FCEFBrowserParams := nil;
  TTrayBrowserWindow(FWindowSettings.RestartSource).Chromium := nil;

  // we are good to go
  FInitialized := True;

  // restore window bounds and state
  FInManualPositionChange := True;
  FForceWindowState := True;
  try
    BoundsRect := TTrayBrowserWindow(FWindowSettings.RestartSource).BoundsRect;
    WindowState := TTrayBrowserWindow(FWindowSettings.RestartSource).WindowState;
  except end;
  FInManualPositionChange := False;

  // restore custom drag region bounds and state
  try
    CustomDragRegion.BoundsRect := TTrayBrowserWindow(FWindowSettings.RestartSource).CustomDragRegion.BoundsRect;
    CustomDragRegion.Visible := TTrayBrowserWindow(FWindowSettings.RestartSource).CustomDragRegion.Visible;
  except end;

  // post restart event (this also starts JS event timer)
  PostJSEvent('"onrestarted"');

  // restore visibility and allow window to catch events
  if TTrayBrowserWindow(FWindowSettings.RestartSource).Visible then
  begin
    FFirstShow.Restarting := True; // avoid onshow event and window restoration when we are re-shown
    Visible := True;
  end;

  // close restart source (it looks better when we do it after showing ourselves)
  FWindowSettings.RestartSource.Close;

  // and we are done, remove restart source reference
  FWindowSettings.RestartSource := nil;
end;

procedure TTrayBrowserWindow.FormDestroy(Sender: TObject);
begin
  // stop timers
  ServiceTimer.Enabled := False;
  DragTimer.Enabled := False;
  JSEventTimer.Enabled := False;

  if FWindowID <> '' then TrayBrowserRootWindow.UnregisterBrowserWindow(FWindowID); // FWindowID can be reset to nothing after window restart

  if Assigned(FWindowState.JSEventQueueLock) then // yes, this can really happen when we are restarting window
  begin
    // lock here is necessary to prevent corner cases of event happening from external DLL source during window destruction
    FWindowState.JSEventQueueLock.Acquire;
    JSEventTimer.Enabled := False; // once more to be safe as it may be reenabled in other thread
    FreeAndNil(FWindowState.JSEventQueue);
    FWindowState.JSEventQueueLock.Release;
  end;
  FreeAndNil(FWindowState.JSEventQueueLock);

  FreeAndNil(FWindowState.ExecProcesses);
  FreeAndNil(FCEFBrowserOpLock);

  FCEFBrowserParams := nil; // prevent us from destroying this one, as CEF destroys it by itself
end;

////////////////////////////////////////
// window visibility change handling

procedure TTrayBrowserWindow.FormShow(Sender: TObject);
var
  LRect: TRect;
begin
  if FExit.Requested or TrayBrowserRootWindow.ExitRequestReceived then
  begin
    // we do not allow us to be shown when exit is requested
    Hide;
    Exit;
  end;

  // if browser is not yet initialized, initialize
  if not FInitialized then InitializeBrowser;

  // LCL somehow manages to reset position on show, thus the special handling
  if WindowState <> wsMaximized then
  begin
    FInManualPositionChange := True;
    LRect := BoundsRect;
    LRect.Location := FLastPosition.Window.Location;
    BoundsRect := LRect;
    FInManualPositionChange := False;
  end;

  PositionOurselves; // reposition ourselves

  if FFirstShow.Pending then
  begin
    FFirstShow.Pending := False;

    if FWindowSettings.Main and (not FWindowSettings.KeepHidden) and (not FFirstShow.Restarting) then
    begin
      // focus us on application startup
      BringToFront;
      Activate;
    end;

    if FWindowSettings.KeepHidden then
    begin
      Hide;
      Exit;
    end;
  end;

  if not TrayBrowserRootWindow.ApplicationChangingVisibility then FWindowState.WasVisible := True; // make sure root window considers us visible on next application show/hide

  if (not FFirstShow.Pending) or (not FFirstShow.Restarting) then // avoid onshow event and state restore on window restarts
  begin
    PostJSEvent('"onshow"');

    // if we are requested to return from minimized state on show, do it
    if (WindowState = wsMinimized) and FWindowSettings.RestoreOnShow then
    begin
      if FWindowState.PreMinimizeWindowState <> wsMinimized then WindowState := FWindowState.PreMinimizeWindowState else WindowState := wsNormal; // return us to the normal window state if we were minimized, handle corner case for safety
      FormWindowStateChange(Self); // yes, this is necessary, programmatic changes do not fire the event
    end;
  end;
end;

procedure TTrayBrowserWindow.FormHide(Sender: TObject);
begin
  if not (FExit.Requested or TrayBrowserRootWindow.ExitRequestReceived) then PostJSEvent('"onhide"'); // onhide events are not posted when exit is requested

  // if application is in focus, focus to next eligible window in reverse order
  if not TrayBrowserRootWindow.ApplicationChangingVisibility then
  begin
    FWindowState.WasVisible := False; // make sure we are not in application visibility pool anymore
    TrayBrowserRootWindow.FocusEligibleWindow;
  end;
end;

////////////////////////////////////////////////////////////
// window focus handling

procedure TTrayBrowserWindow.FormActivate(Sender: TObject);
begin
  ActiveControl := CEFBrowser;
  TrayBrowserRootWindow.WindowGotFocus(FWindowID);
end;

////////////////////////////////////////////////////////////
// window state (minimize/maximize) change handling

procedure TTrayBrowserWindow.FormWindowStateChange(Sender: TObject);
var
  LNewRect: TRect;
begin
  if WindowState = wsNormal then
  begin
    // returning to normal window state, restore internally remembered position if any
    // why going to such lengths? because otherwise un-maximizing window minimized in prior will get us wrong coordinates (remember we can minimize maximized window, LCL coordinates get borked on that)
    TrayBrowserSynchronizeWidgetSet;
    PostJSEvent('"onrestored"'); // this event is called before repositioning events come

    if FRememberedPosition.Active then
    begin
      FBrowserSize := FRememberedPosition.BrowserSize;
      LNewRect := BoundsRect;
      LNewRect.Location := FRememberedPosition.Window.Location;
      BoundsRect := LNewRect;
      FRememberedPosition.Active := False;
    end;
    PositionOurselves;
  end else
  begin
    if not FForceWindowState then
    begin
      // handle button actions
      case WindowState of
        wsMinimized:
        begin
          if FWindowSettings.MinimizeMode <> tbmiTaskbar then WindowState := FWindowState.PreMinimizeWindowState; // not minimizing
          TrayBrowserSynchronizeWidgetSet;

          case FWindowSettings.MinimizeMode of
            tbmiHide: Hide; // hide instead of minimizing

            tbmiTaskbar:
            begin
              // normal minimize, indicate we remember the position, reset movement and reposition timers, send JS event
              FRememberedPosition.Active := True;
              PostJSEvent('"onminimized"');
            end;

            tbmiEvent: PostJSEvent('"onminimizebutton"'); // JS event instead of minimizing
          end;
        end;

        wsMaximized:
        begin
          if FWindowSettings.MaximizeMode <> tbmaOS then WindowState := wsNormal; // not maximizing
          TrayBrowserSynchronizeWidgetSet;

          case FWindowSettings.MaximizeMode of
            tbmaOS:
            begin
              // normal maximize, indicate we remember the position, reset movement and reposition timers, send JS event
              FRememberedPosition.Active := True;
              PostJSEvent('"onmaximized"');
            end;

            tbmaEvent: PostJSEvent('"onmaximizebutton"'); // JS event instead of minimizing
          end;
        end;
      end;
    end else
    begin
      // forced setting, just events and no action handling
      case WindowState of
        wsMinimized: PostJSEvent('"onminimized"');

        wsMaximized:
        begin
          FRememberedPosition.Active := True;
          PostJSEvent('"onmaximized"');
        end;
      end;
    end;
  end;

  FForceWindowState := False; // reset forced setting flag if it was set
  TrayBrowserSynchronizeWidgetSet;

  if WindowState <> wsMinimized then
  begin
    FWindowState.PreMinimizeWindowState := WindowState; // remember last window state to restore us from minimization properly
    if Assigned(Chromium) then Chromium.NotifyMoveOrResizeStarted; // notify browser when we go maximizing or widget-based fullscreen
  end;
  PositionOurselves;
end;

////////////////////////////////////////////////////////////
// termination process

procedure TTrayBrowserWindow.ExitRequest;
begin
  // this one may safely be called multiple times, it just re-requests window close each time it is called
  FExit.Requested := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TTrayBrowserWindow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TTrayBrowserWindow.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;

  if not Assigned(Chromium) then
  begin
    // no browser (yet or in the restart process), so safe to exit
    CanClose := True;
    Exit;
  end;

  // step 1: check if exit is requested, if not, we are just dealing with ALT-F4 or close button click, so we need to discern what we need to do
  if not FExit.Requested then
  begin
    // if window termination is not requested, handle close button click
    case FWindowSettings.CloseMode of
      tbcmHide: Hide; // just hide ourselves
      tbcmExit: FExit.Requested := True; // continue with the exit process

      tbcmMinimize:
      begin
        // try to minimize (minimize mode still applies)
        if WindowState <> wsMinimized then
        begin
          case FWindowSettings.MinimizeMode of
            tbmiNone: ; // nothing to do
            tbmiHide: Hide; // just hide ourselves
            tbmiEvent: PostJSEvent('"onminimizebutton"');

            else
            begin
              // normal minimize operation
              if FWindowSettings.ShowOnTaskBar then
              begin
                FWindowState.PreMinimizeWindowState := WindowState;
                WindowState := wsMinimized;
                FormWindowStateChange(Self); // yes, this is necessary, programmatic changes do not fire the event
              end;
            end;
          end;
        end;
        Exit;
      end;

      tbcmEvent:
      begin
        // just a JavaScript event
        PostJSEvent('"onclosebutton"');
        Exit;
      end;
    end;

    // if exit is not needed, exit :D
    if not FExit.Requested then Exit;
  end;

  // step 2: exit is now requested, check if need to do graceful exit (if JS API registered onexit event), if yes, then start graceful exit timer and wait for its completion, not giving any damn about any exit requests
  if FWindowState.ExitEventNeeded and (not FExit.EventComplete) and (FWindowSettings.GracefulExitTime > 0)then
  begin
    // graceful exit event handling is necessary
    if FExit.Timer >= 0 then Exit; // waiting for graceful exit event to be complete
    FExit.Timer := FWindowSettings.GracefulExitTime * 1000;
    TrayBrowserRootWindow.HideBalloon;
    TrayBrowserRootWindow.ClearUserMenu(FWindowID);
    PostJSEvent('"onexit"');
    Exit;
  end;

  // step 3: finally exit is confirmed, check if we are in the termination phase already and if not, transition to the termination phase and start closing browser
  if not FExit.Terminating then
  begin
    // we are not terminating yet, transition to termination phase
    FExit.Terminating := True;
    Hide;
    KillExecProcesses;
    TrayBrowserRootWindow.HideBalloon;
    TrayBrowserRootWindow.ClearUserMenu(FWindowID);
    if Assigned(Chromium) then Chromium.CloseAllBrowsers;
    Exit;
  end;

  // step 4: yes, we are in the termination phase, check if we are allowed to terminate (yeah, more exit requests can still happen, but we again don't give a damn until browser termination is complete)
  if FExit.CanClose then
  begin
    // browser indicates no graceful handling is possible and we can terminate, allow it
    CEFBrowser.Chromium := nil;
    FreeAndNil(Chromium);
    FreeAndNil(CEFBrowser);
    CanClose := True;
    Exit;
  end;
end;

procedure TTrayBrowserWindow.WMQueryEndSession(var Message: TWMQueryEndSession);
begin
  Message.Result := 0;
  ExitRequest;
end;

function TTrayBrowserWindow.ServiceCheckTerminationTimeout: Boolean;
begin
  Result := False;
  FExit.Timer := FExit.Timer - ServiceTimer.Interval;
  if FExit.Timer <= 0 then
  begin
    FExit.Timer := -1;
    FExit.EventComplete := True;
    ExitRequest;
  end else Result := True; // do not disable service timer if we are still waiting
end;

////////////////////////////////////////////////////////////
// ZMQ message handler

procedure TTrayBrowserWindow.ZMQMessage(const inCommand: String; const inRequest: TTBKVMessage; const inReply: TTBKVMessage);
var
  LArguments: TStringList;
  LCount: Integer;
  i: Integer;
  s, s2: String;
  b: Boolean;
begin
  case inCommand of
    // window commands
    'GOT_BROWSER':
    begin
      inReply.Add('RESULT', 'OK');
      inReply.Add('IS_MAIN', BoolToStr(FWindowSettings.Main, '1', '0'));
      inReply.Add('API_ENABLED', BoolToStr(FWindowSettings.APIEnabled, '1', '0'));
      inReply.Add('TITLE', Caption);
    end;

    'CLICOMMAND':
    begin
      LArguments := TStringList.Create;
      try
        b := True;

        // read argument count
        if b then if not inRequest.ContainsKey('ARGC') then b := False;
        if b then try LCount := StrToInt(inRequest['ARGC']); except b := False end;

        // show us
        case TrayBrowserApplication.Settings.ShowOnCLI of
          tbccOnEmpty: if LCount = 0 then TrayBrowserRootWindow.ShowApplication(True);
          tbccAlways: TrayBrowserRootWindow.ShowApplication(True);
        end;

        // pass arguments to onCLICommand event
        if b then
        begin
          for i := 1 to LCount do
          begin
            if not inRequest.ContainsKey('ARGV[' + IntToStr(i) + ']') then
            begin
              b := False;
              Break;
            end;
            LArguments.Add(inRequest['ARGV[' + IntToStr(i) + ']']);
          end;
        end;
        if b then
        begin
          s := '';
          for s2 in LArguments do s := s + ',' + QuoteJSString(s2);
          PostJSEvent('"onclicommand"' + s);
        end;
        if not b then inReply.Add('ERROR', 'INVALID_ARGUMENTS');
      except
        inReply.Add('ERROR', 'INTERNAL_ERROR');
      end;
      FreeAndNil(LArguments);
    end;

    'JS_CALL':
    begin
      if inRequest.ContainsKey('FUNCTION') and inRequest.ContainsKey('ARGC') then
      begin
        try
          JSAPI(inRequest['FUNCTION'], StrToInt(inRequest['ARGC']), inRequest, inReply);
        except
          on e: Exception do
          begin
            inReply.Add('ERROR', 'JS_CALL_REQUEST_FAILED:' + e.Message);
            Exit;
          end;
        end;

        if inReply.Count = 0 then
        begin
          // add default reply of undefined
          inReply.Add('RESULT[UNDEFINED]', '');
        end;
      end else
      begin
        inReply.Add('ERROR', 'INVALID_JS_CALL_REQUEST');
      end;
    end;

    else
    begin
      // invalid command
      inReply.Add('ERROR', 'INVALID_WINDOW_COMMAND');
    end;
  end;
end;

////////////////////////////////////////////////////////////
// windows platform specifics - message flow interception

// on windows, we need to intercept WndProc to disable minimize/maximize messages when necessary, detect end of movement and monitor changes, these messages are not passed by LCL
function TTrayBrowserWindow.WMWndProc(const inHWND: HWND; const inUMSG: UINT; const inWPARAM: WParam; const inLPARAM: LParam; out outPrevProc: Windows.WNDPROC): LRESULT;
begin
  Result := 0;
  outPrevProc := FLCLWndProc; // we return prevProc = nil to say the message is handled

  case inUMSG of
    WM_ENTERSIZEMOVE: WMEnterSizeMove; // call our EnterSizeMove handler
    WM_EXITSIZEMOVE: WMExitSizeMove; // call our ExitSizeMove handler

    WM_DISPLAYCHANGE: PositionOurselves; // reposition ourselves on display changes

    WM_NCLBUTTONDBLCLK:
    begin
      case FWindowSettings.MaximizeMode of
        tbmaNone: outPrevProc := nil;

        tbmaOS:
        begin
          // workaround for setting window state early so WMPositionChanging can detect it
          if (WindowState <> wsMaximized) then
          begin
            WindowState := wsMaximized;
            outPrevProc := nil;
            FormWindowStateChange(Self); // we need to invoke event manually as we intercepted it from LCL
          end;
        end;

        tbmaEvent:
        begin
          outPrevProc := nil;
          PostJSEvent('"onmaximizebutton"');
        end;
      end;
    end;

    WM_SYSCOMMAND:
    begin
      case inWPARAM of
        SC_MINIMIZE:
        case FWindowSettings.MinimizeMode of
          tbmiNone: outPrevProc := nil;

          tbmiHide:
          begin
            outPrevProc := nil;
            Hide;
          end;

          tbmiEvent:
          begin
            outPrevProc := nil;
            PostJSEvent('"onminimizebutton"');
          end;
        end;

        SC_MAXIMIZE:
        case FWindowSettings.MaximizeMode of
          tbmaNone: outPrevProc := nil;

          tbmaOS:
          begin
            // workaround for setting window state early so WMPositionChanging can detect it
            if (WindowState <> wsMaximized) then
            begin
              WindowState := wsMaximized;
              outPrevProc := nil;
              FormWindowStateChange(Self); // we need to invoke event manually as we intercepted it from LCL
            end;
          end;

          tbmaEvent:
          begin
            outPrevProc := nil;
            PostJSEvent('"onmaximizebutton"');
          end;
        end;
      end;
    end;
  end;
end;

function TBWindowWndProc(hwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam): LRESULT; stdcall;
var
  LControl: TWinControl;
  LPrevProc: WNDPROC;
begin
  Result := 0; // silence messages if there is no window control
  LControl := Controls.FindControl(hwnd);
  if Assigned(LControl) and (LControl is TTrayBrowserWindow) then
  begin
    Result := TTrayBrowserWindow(LControl).WMWndProc(hwnd, uMsg, wParam, lParam, LPrevProc);
    if Assigned(LPrevProc) then Result := LPrevProc(hwnd, uMsg, wParam, lParam);
  end;
end;

////////////////////////////////////////////////////////////
// service timer used for doing background maintenance

procedure TTrayBrowserWindow.ServiceTimerTimer(Sender: TObject);
var
  LHasJobs: Boolean = False;
begin
  // this ticks each 100 ms
  try if FExit.Timer >= 0 then LHasJobs := LHasJobs or ServiceCheckTerminationTimeout; except LHasJobs := True; end; // graceful termination timeout
  try if FWindowState.InLoadError then LHasJobs := LHasJobs or ServiceCheckLoadErrorState; except LHasJobs := True; end; // browser load error handling
  try if FWindowState.ExecProcesses.Count > 0 then LHasJobs := LHasJobs or ServiceReapExecProcesses; except LHasJobs := True; end; // reap running external processes

  if not LHasJobs then ServiceTimer.Enabled := False; // disable ourselves until we get some next job to do
end;

end.

