unit TBTrayBrowser;

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

uses
  windows, windirs, // these go first as they clash with file utils
  Classes, SysUtils, Forms, Generics.Collections, FileUtil, LazFileUtils, StrUtils, Math,
  RegExpr, sqldb, sqlite3conn, sqlite3dyn,
  uCEFApplication, uCEFConstants, uCEFMiscFunctions, uCEFInterfaces;

const
  TB_VERSION = '3.0';
  TB_API_VERSION = '3.0';
  TB_ID_STRING = 'TrayBrowser';
  TB_TITLE_STRING = 'TrayBrowser v3.0 by Alex/AT (alex@alex-at.net)';
  TB_COPYRIGHT_STRING = 'by Alex/AT (<a href="javascript:;" onclick="traybrowser.OpenBrowser(''mailto:alex@alex-at.net'');">alex@alex-at.net</a>), 2025-2026';
  TB_ABOUT_URL = 'about:traybrowser';
  TB_MENU_EXIT_STRING = 'Exit';
  TB_MENU_RETRY_STRING = 'Try loading again';

  TB_HALT_FATAL_APPLICATION_ERROR = 200;
  TB_HALT_ZMQ_PROTOCOL_ERROR = 201;
  TB_HALT_ZMQ_POLLER_THREAD_DIED = 202;

  TB_MAX_CMDLINE_PARAMS = 100;

  WM_TRAYICON = WM_USER + 25;
  TB_MSG_BALLOONSHOW = WM_APP + $C01;
  TB_MSG_BALLOONHIDE = WM_APP + $C02;
  TB_MSG_BALLOONTIMEOUT = WM_APP + $C03;
  TB_MSG_BALLOONCLICK = WM_APP + $C04;
  TB_MSG_BROWSER_ONLOADSTART = WM_APP + $C11;
  TB_MSG_BROWSER_ONREMOTELOADSTART = WM_APP + $C12;
  TB_MSG_BROWSER_ONLOADERROR = WM_APP + $C13;

type
  TTBBorderMode = (tbbtNormal, tbbtBorderless, tbbtWindow);
  TTBCloseMode = (tbcmNone, tbcmHide, tbcmExit, tbcmMinimize, tbcmEvent);
  TTBFullscreenMode = (tbfsNone, tbfsWindow, tbfsDesktop, tbfsKiosk, tbfsAll);
  TTBLoadErrorMode = (tbleInternal, tbleBrowser);
  TTBMaximizeMode = (tbmaNone, tbmaOS, tbmaEvent);
  TTBMinimizeMode = (tbmiNone, tbmiHide, tbmiTaskbar, tbmiEvent);
  TTBRememberPositionMode = (tbrpFull, tbrpMonitor, tbrpNone);
  TTBScalingMode = (tbscNormal, tbscFloor, tbscNoCorrect, tbscNoScale);
  TTBSnapToMode = (tbstNone, tbstTray, tbstCenter, tbstTopLeft, tbstTopCenter, tbstTopRight, tbstCenterLeft, tbstCenterRight, tbstBottomLeft, tbstBottomCenter, tbstBottomRight);
  TTBStartMonitorMode = (tbsmDefault, tbsmMain, tbsmID, tbsmXY);
  TTBTrayClickMode = (tbtcNormal, tbtcRestore, tbtcShow, tbtcEvent, tbtcNone);
  TTBCLIShowMode = (tbccOnEmpty, tbccAlways, tbccNever);

  TTBStringDict = specialize TDictionary<String, String>;
  TTBStringPair = specialize TPair<String, String>;
  TTBIntDict = specialize TDictionary<String, Integer>;
  TTBIntPair = specialize TPair<String, Integer>;
  TTBObjectDict = specialize TDictionary<String, TObject>;
  TTBObjectPair = specialize TPair<String, TObject>;
  TTBWindowList = specialize TDictionary<String, TForm>;
  TTBWindowPair = specialize TPair<String, TForm>;
  TTBKVMessage = TTBStringDict;
  TTBKVMessagePair = TTBStringPair;

  // settings list and its elements
  TTBSettingFlags = (
    tbsefApplication, // all application level settings
    tbsefInit, // application settings applied first on early initialization and before profile configuration files are read
    tbsefProcessInit, // application settings applied for both main process and subprocesses after tbsefInit and before tbsefApplicationInit
    tbsefApplicationInit, // application settings that are applied on main application startup after tbsefInit
    tbsefWindow, // window level setting
    tbsefNoClamp, // instead of clamping to min/max or cutting string, do not apply if out of range
    tbsefNoEmpty, // do not apply if the string is empty
    tbsefNoPrint, // not printed at all
    tbsefProtected, // not changeable by user
    tbsefInvisible, // not visible (not printed) to user
    tbsefRestart // window setting requires window restart to be processed, ignored otherwise
  );
  TTBSettingFlagsSet = set of TTBSettingFlags;
  TTBSettingTypes = (tbsetUndefined, tbsetBoolean, tbsetInteger, tbsetString, tbsetChoice);

  TTBSettingChoiceDef = record
    i: Integer;
    s: String;
  end;

  TTBSettingChoiceList = specialize TDictionary<Integer, String>;
  TTBSettingChoiceSelectorList = TTBIntDict;

  TTBSettingElement = class
      Name: String;
      NameLC: String;
      ValueType: TTBSettingTypes;
      ValuePtr: Pointer; // pointer to the actual value in TempSettings or TempWindowSettings
      Flags: TTBSettingFlagsSet;
      DefaultBool: Boolean;
      DefaultInt: Integer; // also used for choice type
      DefaultString: String;
      IntMin, IntMax, StringMaxLength: Integer;
      Choices: TTBSettingChoiceList;
      Selectors: TTBSettingChoiceSelectorList;

      constructor Create(const inPtr: Pointer; const inName: String; const inDefault: Boolean; const inFlags: TTBSettingFlagsSet);
      constructor Create(const inPtr: Pointer; const inName: String; const inDefault: Integer; const inMin: Integer; const inMax: Integer; const inFlags: TTBSettingFlagsSet);
      constructor Create(const inPtr: Pointer; const inName: String; const inDefault: String; const inMaxLength: Integer; const inFlags: TTBSettingFlagsSet);

      constructor CreateChoice(const inPtr: Pointer; const inName: String; const inDefault: Integer; const inFlags: TTBSettingFlagsSet; const inChoices: array of TTBSettingChoiceDef; const inSelectors: array of TTBSettingChoiceDef);

      destructor Destroy; override;

      procedure Initialize(const inPtr: Pointer; const inName: String; const inFlags: TTBSettingFlagsSet);
      procedure ApplyDefault;
      function ApplyConfig(const inConfig: TTBStringDict): Boolean;
      function Print(out outStr: String): Boolean;
  end;
  TTBSettingList = specialize TList<TTBSettingElement>;

  TTBCustomSettingProcMode = (tbscpDefault, tbscpPreSetting, tbscpNoSetting, tbscpPostSetting, tbscpPrint);
  TTBCustomSettingProc = function(var Element: TTBSettingElement; const inMode: TTBCustomSettingProcMode; const inConfig: TTBStringDict; out outStr: String): Boolean;

  // JS variables are technically a non-Variant 'Variant' than can hold values of multiple types, but only for short term storage and without implicit casting
  // the benefit is possibility to return one of the types without worrying for it being managed or i.e. differing raw string from JSON string inside JS API
  // we use FPC advanced records here, they are very handy for handling parameter and result passing
  // currently only used in TBWindow_JSAPI result handling code, but has potential to rewrite all JS variable passing back and forth to it
  TTBJSVariantType = (tbjvEmpty, tbjvBoolean, tbjvInteger, tbjvFloat, tbjvString, tbjvJson);
  TTBJSVariant = record
    private
      pvType: TTBJSVariantType;
      pvBoolean: Boolean;
      pvInteger: Integer;
      pvFloat: Float;
      pvString: String;
      procedure SetBoolean(const inBoolean: Boolean); inline;
      procedure SetInteger(const inInteger: Integer); inline;
      procedure SetFloat(const inFloat: Float); inline;
      procedure SetString(const inString: String); inline;
      procedure SetJSON(const inString: String); inline;
    public
      property vType: TTBJSVariantType read pvType;
      property vBoolean: Boolean read pvBoolean write SetBoolean;
      property vInteger: Integer read pvInteger write SetInteger;
      property vFloat: Float read pvFloat write SetFloat;
      property vString: String read pvString write SetString;
      property vJSON: String read pvString write SetJSON;

      class operator Initialize(var Rec: TTBJSVariant); inline;
  end;

  TTBWindowSettings = record
    Main: Boolean;
    RestartSource: TForm;

    APIEnabled: Boolean;
    PositionID: String;
    Size: TSize;

    AllowCopy: Boolean;
    AllowDownload: Boolean;
    AllowPaste: Boolean;
    AllowPrint: Boolean;
    AllowReload: Boolean;
    AllowSearch: Boolean;
    AlwaysOnTop: Boolean;
    Border: TTBBorderMode;
    BorderMargin: Integer;
    Caption: String;
    ClampToMonitor: Boolean;
    CloseButton: Boolean;
    CloseMode: TTBCloseMode;
    FullScreen: TTBFullscreenMode;
    GracefulExitTime: Integer;
    IgnoreCertificateErrors: Boolean;
    KeepHidden: Boolean;
    LoadErrorPage: TTBLoadErrorMode;
    MaximizeButton: Boolean;
    MaximizeMode: TTBMaximizeMode;
    MinimizeButton: Boolean;
    MinimizeMode: TTBMinimizeMode;
    Movable: Boolean;
    Parameters: String;
    RememberPosition: TTBRememberPositionMode;
    RememberSize: Boolean;
    RestoreOnShow: Boolean;
    ScaleAlignmentMax: Integer;
    ShowOnStart: Boolean;
    ShowOnTaskBar: Boolean;
    Sizeable: Boolean;
    SnapTo: TTBSnapToMode;
    SnapToFollowTrayClick: Boolean;
    SnapToWindows: Boolean;
    SnapToWindowsDistance: Integer;
    StartMonitor: TTBStartMonitorMode;
    StartMonitorID: Integer;
    StartMonitorPosition: TPoint;
    StartMonitorText: String;
    URL: String;
  end;

  TTBSettings = record
    ApplicationID: String;
    UseNativeDataDirectory: Boolean;

    AllowDebug: Boolean;
    AllowExec: Boolean;
    AllowSystemInfo: Boolean;
    AutoHide: Boolean;
    AutoPrint: Boolean;
    ClearKVStore: Boolean;
    ClearDataDirectory: Boolean;
    ConserveResources: Boolean;
    ConservativeCache: Boolean;
    Copyright: String;
    EnableGPU: Boolean;
    EnableHangMonitor: Boolean;
    GracefulExitTime: Integer;
    Hint: String;
    KeepCookies: Boolean;
    KeepStorage: Boolean;
    LoadErrorPage: TTBLoadErrorMode;
    MenuDisable: Boolean;
    MenuExitCaption: String;
    MenuHideExit: Boolean;
    MenuHideRetry: Boolean;
    MenuRetryCaption: String;
    RetryInterval: Integer;
    ScaleAlignmentMax: Integer;
    ScalingMode: TTBScalingMode;
    ShowOnCLI: TTBCLIShowMode;
    ShowTrayIcon: Boolean;
    SingleProcess: Boolean;
    Size: TSize;
    TrayIconClickMode: TTBTrayClickMode;
    Title: String;
    URL: String;
    UserAgent: String;
  end;

  // application single instance class, created in both main application process and subprocesses
  TTrayBrowserApplication = class
    constructor Create;
    destructor Destroy; override;

    protected
      LockFile: TFileStream;
      IconPathList: TStringList;

      // initialization
      function ConfigureMainProcess: Boolean;
      procedure ConfigureSubprocess;
      procedure BuildDataPath;

      // cleanup
      procedure CleanupVolatileDirectory;
      procedure CleanDirectoryContents(const inPath: string; const inSubPath: string; const inSkipList: TTBStringDict);

      // settings handling
      procedure InitializeSettingsList;
      procedure ReadSettings;
      procedure ReadSettingsFromFile(const inPath: String; const inFile: String; const inBaseName: String; const inConfig: TTBStringDict);
      procedure ReadCommandLine(const inConfig: TTBStringDict);
      procedure AddParsedSetting(const inConfig: TTBStringDict; const inKey: String; const inValue: String; const inNoOverrideAdd: TStringList);
      procedure ParseSettings(const inText: String; const inConfig: TTBStringDict);
      function SettingChoiceDef(const inValue: Integer; const inName: String): TTBSettingChoiceDef;

    public
      Initialized: Boolean;
      AllowExecFilter: TStringList;
      AllowExecRegexp: TStringList;
      ApplicationID: String;
      AppPath: String;
      AppName: String;
      RunPath: String;
      ProfilePath: String;
      RoamingProfilePath: String;
      DataPath: String;
      VolatilePath: String;
      IPCPath: String;
      IPCKey: String;
      ProcessId: String;
      IsSubprocess: Boolean;
      CommandLine: TStringList;
      NoOverride: TTBStringDict;
      SettingsList: TTBSettingList;
      Settings: TTBSettings;
      MainWindowSettings: TTBWindowSettings;
      TempSettings: TTBSettings;
      TempWindowSettings: TTBWindowSettings;
      FormatSettings: TFormatSettings;

      // initialization
      procedure InitGlobalCEFApplication;
      procedure Run;

      // logging, including emergency logging
      procedure DebugLog(const inMessage: String; const inSeverity: Integer = CEF_LOG_SEVERITY_INFO);
      function LogKVMessage(const inMessage: TTBKVMessage): String;
      function LogValues(const inValues: TStringList): String;
      procedure Die(const inMessage: String; const inCode: Integer = TB_HALT_FATAL_APPLICATION_ERROR);

      // settings handling
      procedure SetDefaults;
      procedure SetWindowDefaults;
      procedure ApplyMainWindowDefaults(const inConfig: TTBStringDict; const inUser: Boolean = False);
      procedure ApplySettings(const inConfig: TTBStringDict; const inFlags: TTBSettingFlagsSet; const inUser: Boolean = False);
      procedure FixupSettings(const inConfig: TTBStringDict);
      procedure FixupWindowSettings(const inConfig: TTBStringDict);
      procedure PrintSettings(const inConfig: TTBStringDict; const inFlags: TTBSettingFlagsSet; const inLowerCase: Boolean = False; const inUser: Boolean = False);
      function StrArgToBool(const inStr: String; const inUseLiterals: Boolean = False; const inLiteralsOnly: Boolean = False): Boolean;
      function ReadResource(const inName: String): String;
      function ReadFile(const inPath: String): String;
  end;

// global helper wrappers
procedure TrayBrowserSynchronizeWidgetSet;
function TrayBrowserGetApplicationName: String;
function TrayBrowserGetVendorName: String;
procedure TrayBrowserCEFOnBeforeChildProcessLaunch(const inCommandline: ICefCommandLine);

// windows platform specifics
{$IFDEF WINDOWS}
function GetUserNameExW(NameFormat: DWORD; lpNameBuffer: LPWSTR; var nSize: DWORD): Boolean; stdcall; external 'secur32.dll' name 'GetUserNameExW';
function SetCurrentProcessExplicitAppUserModelID(AppID: PWidechar): HResult; stdcall; external 'shell32.dll';
{$ENDIF}

var
  TrayBrowserApplication: TTrayBrowserApplication = nil; // application singleton instance

  // additional 'constants' initialized as variables
  TrayBrowserProfileKeepList: array of String = ('Local State'); // these files are kept if KeepCookies or KeepStorage options are enabled
  TrayBrowserProfileKeepCookiesList: array of String = ('Default/Network/Cookies', 'Default/Network/Cookies-journal'); // these files are kept if KeepCookies option is enabled (the default)
  TrayBrowserProfileKeepStorageList: array of String = ('Default/Local Storage', 'Default/SharedStorage', 'Default/SharedStorage-wal'); // these files are kept if KeepStorage option is enabled (NOT the default)

implementation

uses
 TBRootWindow, TBRenderer, TBZMQ;

{$INCLUDE TBTrayBrowser_Settings.inc}

// application construction / destruction

constructor TTrayBrowserApplication.Create;
begin
  inherited;

  Randomize;

  GlobalCEFApp := nil;
  ProcessId := IntToStr(GetProcessID);

  {$IFDEF TB_DEBUG}DebugLog('Initializing process');{$ENDIF}

  if Assigned(TrayBrowserApplication) then Die('TrayBrowserApplication class cannot be created more than once');
  TrayBrowserApplication := Self;

  ApplicationID := 'TrayBrowser';
  IsSubprocess := IsCEFSubprocess;
  LockFile := nil;

  OnGetApplicationName := @TrayBrowserGetApplicationName;
  OnGetVendorName := @TrayBrowserGetVendorName;

  SettingsList := TTBSettingList.Create;
  AllowExecFilter := TStringList.Create;
  AllowExecRegexp := TStringList.Create;
  IconPathList := TStringList.Create;
  NoOverride := TTBStringDict.Create;
  CommandLine := TStringList.Create;

  FormatSettings := DefaultFormatSettings;
  FormatSettings.DecimalSeparator := '.'; // take care, we need to always use dot for float to int conversion

  AppPath := IncludeTrailingPathDelimiter(CleanAndExpandDirectory(ExtractFilePath(Application.ExeName)));
  RunPath := IncludeTrailingPathDelimiter(GetCurrentDir);
  AppName := {$IFDEF WINDOWS}ExtractFileNameWithoutExt(ExtractFileName(Application.ExeName)){$ELSE}ExtractFileName(Application.ExeName){$ENDIF};

  InitializeSettingsList; // initialize settings list
  SetDefaults; // set all settings to their defaults
  if not IsSubprocess then
  begin
    Initialized := ConfigureMainProcess;
  end else
  begin
    ConfigureSubprocess;
    Initialized := True;
  end;
end;

destructor TTrayBrowserApplication.Destroy;
var
  LElement: TTBSettingElement;
  i: Integer;
begin
  {$IFDEF TB_DEBUG}if not IsSubprocess then DebugLog('Terminating main process') else DebugLog('Terminating subprocess');{$ENDIF}

  if Assigned(GlobalCEFApp) then
  begin
    // allow CEF and application to finish some work if there is some left
    for i := 1 to 20 do
    begin
      if not IsSubprocess then Application.ProcessMessages;
      GlobalCEFApp.DoMessageLoopWork;
    end;

    // CEF single process mode is intended for easiness of debugging only, thus this is allowable
    if not Settings.SingleProcess then
    begin
      DestroyGlobalCEFApp;

      // wait for global CEF application destruction a little while, if it does not happen it does not happen though
      for i := 1 to 10 do
      begin
        if not IsSubprocess then Application.ProcessMessages;
        Sleep(10);
      end;
    end;
  end;

  // destroy our own bits and pieces
  FreeAndNil(TrayBrowserRenderer);
  FreeAndNil(TrayBrowserZMQ);
  FreeAndNil(CommandLine);
  FreeAndNil(NoOverride);
  FreeAndNil(IconPathList);
  FreeAndNil(AllowExecFilter);
  FreeAndNil(AllowExecRegexp);
  for i := 0 to SettingsList.Count - 1 do
  begin
    LElement := SettingsList[i];
    FreeAndNil(LElement);
  end;
  FreeAndNil(SettingsList);

  if Initialized and (not IsSubprocess) then
  begin
    try DeleteFile(AppPath + 'debug.log'); except end; // remove local debug.log
    CleanupVolatileDirectory; // clean up volatile data directory again, yes, this leaves some locked files on Windows but is better than leaving everything
    try if FileExists(IPCPath) then DeleteFile(IPCPath); except end; // remove IPC socket
    FreeAndNil(LockFile); // unlock application startup
    try DeleteFile(DataPath + 'traybrowser.lock'); except end; // remove lockfile
  end;

  inherited;
end;

// initial configuration for main process and subprocesses

function TTrayBrowserApplication.ConfigureMainProcess: Boolean;
var
  LGUID: TGUID;
  LMessage, LReply: TTBKVMessage;
  i: Integer;
begin
  Result := True;

  {$IFDEF TB_DEBUG}DebugLog('Configuring main process');{$ENDIF}

  // read settings from configuration files
  ReadSettings;

  // set AUMID on Windows to make sure we are known to shell, this does not affect tray icon notifications though :(
  SetCurrentProcessExplicitAppUserModelID(PWideChar('TrayBrowser.' + ApplicationID));

  // create data path if it does not exist
  if not DirectoryExists(DataPath) then
  begin
    try if FileExists(DataPath) then DeleteFile(DataPath); except end;
    if not ForceDirectories(DataPath) then Die('Failed to create data directory `' + DataPath + '`');
  end;

  // set up IPC path and generate IPC key for the main process
  IPCPath := DataPath + 'traybrowser.ipc';
  if CreateGUID(LGUID) <> 0 then Die('Failed to generate IPC key GUID');
  try IPCKey := LGUID.ToString(); except Die('Failed to generate IPC key GUID'); end;

  // lock us from secondary runs
  try
    LockFile := TFileStream.Create(DataPath + 'traybrowser.lock', fmOpenReadWrite or fmCreate or fmShareExclusive);
  except
    // pass command line to the running instance if possible
    try
      if FileExists(IPCPath) then
      begin
        TTrayBrowserZMQ.Create;
        TrayBrowserZMQ.ZMQPollTimeout := TB_ZMQ_QUICK_POLL_TIMEOUT;
        TrayBrowserZMQ.CreateDealerSocket;

        IPCKey := '{CLICOMMAND}'; // set special CLI key for CLI commands, actually it does not matter what it is, the same is verified by poller thread for CLICOMMAND command
        LMessage := TTBStringDict.Create;
        try
          // send command line parameters
          LMessage.Add('ARGC', IntToStr(CommandLine.Count));
          for i := 0 to CommandLine.Count - 1 do
            LMessage.Add('ARGV[' + IntToStr(i + 1) + ']', CommandLine[i]);
          LReply := TrayBrowserZMQ.Command(TrayBrowserZMQ.ZMQDealerSocket, ProcessId, 'CLICOMMAND', LMessage);
          if Assigned(LReply) then FreeAndNil(LReply); // we do not need the reply here
        except end;
        FreeAndNil(LMessage);
        IPCKey := LGUID.ToString(); // restore IPC key

        // all_is_fine.jpg
        FreeAndNil(TrayBrowserZMQ);
        Result := False;
        Exit;
      end;
    except end;

    // destroy ZMQ singleton we created above, also destroying socket and context
    if Assigned(TrayBrowserZMQ) then FreeAndNil(TrayBrowserZMQ);

    // try once more just in case we have existing process that terminates
    Sleep(250);
    try
      LockFile := TFileStream.Create(DataPath + 'traybrowser.lock', fmOpenReadWrite or fmCreate or fmShareExclusive);
    except
      Die('Another instance is already running');
    end;
  end;

  // just in case
  if not Assigned(LockFile) then Die('Another instance is already running');

  // clean up cache and stuff
  CleanupVolatileDirectory;

  // initialize ZeroMQ
  TTrayBrowserZMQ.Create;

  // for single process mode, also initialize renderer bits
  if Settings.SingleProcess then TTrayBrowserRenderer.Create; // initialize renderer bits for subprocesses or single process mode
end;

procedure TTrayBrowserApplication.ConfigureSubprocess;
var
  LConfig: TTBStringDict;
  LMessage, LReply: TTBKVMessage;
  LRegexp: TRegExpr;
  LCount: Integer;
  i: Integer;
begin
  {$IFDEF TB_DEBUG}DebugLog('Configuring subprocess');{$ENDIF}

  if Settings.SingleProcess then Die('Subprocess found in single process mode');

  // obtain application ID, application directory mode and IPC key from command line parameters
  ApplicationID := '';
  IPCKey := '';
  Settings.UseNativeDataDirectory := False;
  LRegexp := TRegExpr.Create;
  try
    LRegexp.Expression := '^[\-\/]+(tbapplicationid|tbusenativedatadirectory|tbipckey)=(.+)$';
    LRegexp.ModifierStr := 'ig-m-r-s-x';
    for i := 1 to ParamCount do
    begin
      if LRegexp.Exec(Paramstr(i)) then
      begin
        case LowerCase(LRegexp.Match[1]) of
          'tbapplicationid': ApplicationID := LRegexp.Match[2];
          'tbusenativedatadirectory': Settings.UseNativeDataDirectory := True; // only key presence matters
          'tbipckey': IPCKey := LRegexp.Match[2];
        end;
      end;
    end;
  except end;
  FreeAndNil(LRegexp);
  if ApplicationID = '' then Die('Failed to obtain application ID');
  if IPCKey = '' then Die('Failed to obtain IPC key');
  {$IFDEF TB_DEBUG}DebugLog('Obtained application ID: `' + ApplicationID + '`');{$ENDIF}
  {$IFDEF TB_DEBUG}DebugLog('Obtained IPC key: `' + IPCKey + '`');{$ENDIF}

  // set data path required for IPC communication and volatile storage
  BuildDataPath;
  if not DirectoryExists(DataPath) then Die('Failed to find data directory `' + DataPath + '`');

  // set AUMID on Windows to make sure we are known to shell, this does not affect tray icon notifications though :(
  SetCurrentProcessExplicitAppUserModelID(PWideChar('TrayBrowser.' + ApplicationID));

  // initialize ZeroMQ
  IPCPath := DataPath + 'traybrowser.ipc';
  TTrayBrowserZMQ.Create;

  // initialize renderer bits
  TTrayBrowserRenderer.Create;

  // retrieve configuration from the main process
  LConfig := TTBStringDict.Create;
  LMessage := TTBKVMessage.Create;
  LReply := nil;
  {$IFDEF TB_DEBUG}DebugLog('Obtaining application settings');{$ENDIF}
  try
    // send initialization hello
    try
      LReply := TrayBrowserZMQ.Command(TrayBrowserZMQ.ZMQDealerSocket, ProcessId, 'INIT', LMessage);
      if Assigned(LReply) then
      begin
        try
          if (not LReply.ContainsKey('RESULT')) or (LReply['RESULT'] <> 'HELLO') then Die('Unexpected response from main process on initial contact');
        except end;
        FreeAndNil(LReply);
      end else Die('Invalid INIT command response: no reply');
    except Die('Failed to contact the main process'); end;

    // retrieve settings
    try
      LReply := TrayBrowserZMQ.Command(TrayBrowserZMQ.ZMQDealerSocket, ProcessId, 'GETAPPCONFIG', LMessage);
      try
        if Assigned(LReply) then
        begin
          if not LReply.ContainsKey('COUNT') then Die('Unexpected response from the main process on configuration retrieval');
          LCount := StrToInt(LReply['COUNT']);
          for i := 1 to LCount do
          begin
            if (not LReply.ContainsKey('CONFIG[' + IntToStr(i) + '].KEY')) or (not LReply.ContainsKey('CONFIG[' + IntToStr(i) + '].VALUE')) then Die('Invalid response from main process on configuration retrieval');
            LConfig.Add(LReply['CONFIG[' + IntToStr(i) + '].KEY'], LReply['CONFIG[' + IntToStr(i) + '].VALUE']);
          end;
        end else Die('Invalid GETAPPCONFIG command response: no reply');
      except end;
      FreeAndNil(LReply);
    except Die('Failed to obtain configuration from main process'); end;

    // now apply the retrieved settings (only necessary part of them: early initialization and process initialization)
    ApplySettings(LConfig, [tbsefApplication,tbsefInit]);
    ApplySettings(LConfig, [tbsefApplication,tbsefProcessInit]);
  except Die('Failed to apply subprocess configuration'); end;
  FreeAndNil(LConfig);
  FreeAndNil(LReply);
  FreeAndNil(LMessage);
end;

// global CEF application initialization

procedure TTrayBrowserApplication.InitGlobalCEFApplication;
var
  s: String;
begin
  GlobalCEFApp := TCefApplication.Create;

  GlobalCEFApp.FrameworkDirPath := UnicodeString(AppPath + 'cef' + DirectorySeparator);
  GlobalCEFApp.MainBundlePath := UnicodeString(AppPath + 'cef' + DirectorySeparator);
  GlobalCEFApp.ResourcesDirPath := UnicodeString(AppPath + 'cef' + DirectorySeparator);
  GlobalCEFApp.LocalesDirPath := UnicodeString(AppPath + 'locales' + DirectorySeparator);

  GlobalCEFApp.Cache := UnicodeString(VolatilePath);
  GlobalCEFApp.LogFile := UnicodeString(DataPath + 'debug.log');

  GlobalCEFApp.LogProcessInfo := True; // log process IDs if logging
  GlobalCEFApp.LogSeverity := {$IFDEF TB_DEBUG}LOGSEVERITY_INFO{$ELSE}LOGSEVERITY_DISABLE{$ENDIF};
  if Settings.AllowDebug then GlobalCEFApp.LogSeverity := LOGSEVERITY_INFO;
  GlobalCEFApp.RemoteDebuggingPort := 0; // no remote debugging
  GlobalCEFApp.CommandLineArgsDisabled := True; // no command line arguments from user side please

  s := ReplaceStr(Settings.UserAgent, '%DEFAULT%', '%TRAYBROWSER% %BROWSER%');
  s := ReplaceStr(s, '%TRAYBROWSER%', TB_ID_STRING + '/' + TB_VERSION);
  s := ReplaceStr(s, '%BROWSER%', GetDefaultCEFUserAgent);
  GlobalCEFApp.UserAgent := UnicodeString(s);

  GlobalCEFApp.DeleteCache := False; // embedded c4d mechanism is not necessary
  GlobalCEFApp.DeleteCookies := False; // embedded c4d mechanism is not necessary
  GlobalCEFApp.DisableJavascriptAccessClipboard := True; // no, we do not want applications to randomly monitor clipboard
  GlobalCEFApp.DisableJavascriptDomPaste := True; // no DOM paste access
  GlobalCEFApp.EnablePrintPreview := True;
  GlobalCEFApp.EnableGPU := Settings.EnableGPU;
  GlobalCEFApp.KioskPrinting := Settings.AutoPrint;
  GlobalCEFApp.SetCurrentDir := False; // no, we do not want this
  GlobalCEFApp.EnableSpeechInput := True; // seems obsolete, but enabling for safety
  GlobalCEFApp.EnableMediaStream := True; // seems obsolete, but enabling for safety
  GlobalCEFApp.DisableFeatures := 'WebRtcHideLocalIpsWithMdns'; // makes WebRTC work with native IP addressing
  GlobalCEFApp.DisableHangMonitor := not Settings.EnableHangMonitor;

  // add explicit command line options to be used in subprocesses as we are running with CommandLineArgsDisabled=true
  if Settings.AutoPrint then GlobalCEFApp.AddCustomCommandLine('kiosk-printing');
  if not Settings.EnableGPU then GlobalCEFApp.AddCustomCommandLine('disable-gpu');
  if Settings.ConservativeCache then GlobalCEFApp.AddCustomCommandLine('aggressive-cache-discard');
  GlobalCEFApp.AddCustomCommandLine('disable-javascript-access-clipboard'); // no, we do not want applications to randomly monitor clipboard
  GlobalCEFApp.AddCustomCommandLine('disable-javascript-dom-paste'); // no DOM paste access
  GlobalCEFApp.AddCustomCommandLine('disable-features=WebRtcHideLocalIpsWithMdns'); // makes WebRTC work with native IP addressing
  if not Settings.EnableHangMonitor then GlobalCEFApp.AddCustomCommandLine('disable-hang-monitor');
  if Settings.ConserveResources then GlobalCEFApp.AddCustomCommandLine('enable-low-end-device-mode');
  if Settings.ScalingMode = tbscNoScale then GlobalCEFApp.AddCustomCommandLine('force-device-scale-factor', '1');

  if Settings.SingleProcess then
  begin
    GlobalCEFApp.SingleProcess := True;
    GlobalCEFApp.AddCustomCommandLine('single-process');
  end;

  // set up subprocess launch command line tweak for passing IPC ID
  GlobalCEFApp.OnBeforeChildProcessLaunch := @TrayBrowserCEFOnBeforeChildProcessLaunch;

  // set up renderer specific event handlers
  if IsSubprocess or Settings.SingleProcess then
  begin
    GlobalCEFApp.OnWebKitInitialized := @TrayBrowserRendererCEFOnWebkitInitialized;
    GlobalCEFApp.OnBrowserCreated := @TrayBrowserRendererCEFOnBrowserCreated;
    GlobalCEFApp.OnContextCreated := @TrayBrowserRendererCEFOnContextCreated;
  end;
end;

// logging

procedure TTrayBrowserApplication.DebugLog(const inMessage: String; const inSeverity: Integer = CEF_LOG_SEVERITY_INFO);
{$IFDEF TB_INIT_DEBUG}
var
  LStream: TFileStream;
  s: String;
{$ENDIF}
begin
  if Assigned(GlobalCEFApp) then
  begin
    CefDebugLog(inMessage, inSeverity);
  end else
  begin
    {$IFDEF TB_INIT_DEBUG}
    // long and hard way...
    try
      if not FileExists('tbdebug.log') then LStream :=  TFileStream.Create('tbdebug.log', fmCreate or fmOpenWrite)
        else LStream :=  TFileStream.Create('tbdebug.log', fmOpenWrite);
      try
        LStream.Seek(0, soEnd);
        s := '[' + ProcessId + '] ' + inMessage + LineEnding;
        LStream.Write(s[1], Length(s));
      except end;
      FreeAndNil(LStream);
    except end;
    {$ENDIF}
  end;
end;

function TTrayBrowserApplication.LogKVMessage(const inMessage: TTBKVMessage): String;
var
  LElement: TTBKVMessagePair;
  LKeys: TStringList;
  LKey: String;
  i: Integer;
begin
  Result := '';

  if not Assigned(inMessage) then
  begin
    Result := '<NO MESSAGE>';
    Exit;
  end;

  LKeys := TStringList.Create;
  try
    for LElement in inMessage do LKeys.Add(LElement.Key);
    LKeys.Sort;
    i := 0;
    for LKey in LKeys do
    begin
      if i <> 0 then Result := Result + ',';
      Result := Result + '`' + LKey + '`=`' + inMessage[LKey] + '`';
      i := i + 1;
    end;
  except end;
  FreeAndNil(LKeys);
end;

function TTrayBrowserApplication.LogValues(const inValues: TStringList): String;
var
  s: String;
  i: Integer;
begin
  Result := '';

  if not Assigned(inValues) then
  begin
    Result := '<NO VALUES>';
    Exit;
  end;

  try
    i := 0;
    for s in inValues do
    begin
      if i <> 0 then Result := Result + ',';
      Result := Result + '`' + s + '`';
      i := i + 1;
    end;
  except end;
end;

procedure TTrayBrowserApplication.Die(const inMessage: String; const inCode: Integer = TB_HALT_FATAL_APPLICATION_ERROR);
begin
  try DebugLog(inMessage, CEF_LOG_SEVERITY_FATAL); except end;
  {$IFDEF TB_INIT_DEBUG}if not Assigned(GlobalCEFApp) then raise Exception.Create(inMessage);{$ENDIF}
  Halt(inCode);
end;

// main application startup

procedure TTrayBrowserApplication.Run;
var
  i: Integer;
begin
  // wait for global CEF context initialization
  i := 0;
  while (not GlobalCEFApp.GlobalContextInitialized) do
  begin
    Sleep(10);
    i := i + 1;
    if (i > 1000) then
    begin
      // failed to initialize in 10 seconds
      Die('Failed to initialize CEF');
    end;
  end;

  // start main windowed application
  Application.ShowMainForm := False;
  Application.Initialize;
  Application.CreateForm(TTrayBrowserRootWindow, TrayBrowserRootWindow);
  Application.Run;
end;

// volatile directory cleanup

procedure TTrayBrowserApplication.CleanDirectoryContents(const inPath: String; const inSubPath: String; const inSkipList: TTBStringDict);
var
  LSearchRec: TSearchRec;
  LCurrentPathWithSlash, LCurrentSubPath, LCurrentSubPathWithSlash, LCurrentDir, LCurrentDirWithSlash, LCurrentFile, LCurrentFilePath: String;
begin
  LCurrentPathWithSlash := IncludeTrailingPathDelimiter(inPath);
  LCurrentSubPath := ExcludeLeadingPathDelimiter(ExcludeTrailingPathDelimiter(inSubPath));
  if LCurrentSubPath <> '' then
  begin
    // subdirectory
    LCurrentSubPathWithSlash := IncludeTrailingPathDelimiter(LCurrentSubPath);
    LCurrentDir := LCurrentPathWithSlash + LCurrentSubPath;
    LCurrentDirWithSlash := IncludeTrailingPathDelimiter(LCurrentDir);
  end else
  begin
    // initial directory
    LCurrentSubPathWithSlash := '';
    LCurrentDir := ExcludeTrailingPathDelimiter(inPath);
    LCurrentDirWithSlash := LCurrentPathWithSlash;
  end;

  try
    if not DirectoryExists(LCurrentDir) then Exit;
    if FindFirst(IncludeTrailingPathDelimiter(LCurrentDir) + GetAllFilesMask, faAnyFile, LSearchRec) = 0 then
    begin
      repeat
        try
          LCurrentFile := ExcludeLeadingPathDelimiter(ExcludeTrailingPathDelimiter(LSearchRec.Name));
          if (LCurrentFile = '.') or (LCurrentFile = '..') or (LCurrentFile = '') then Continue; // skip what should be skipped
          LCurrentFilePath := LCurrentDirWithSlash + LCurrentFile;

          if inSkipList.ContainsKey(LCurrentSubPathWithSlash + LCurrentFile) then Continue; // skip what is defined to be skipped

          if (LSearchRec.Attr and faReadOnly) > 0 then
            try FileSetAttr(LCurrentFilePath, LSearchRec.Attr - faReadOnly); except end; // remove readonly attribute if set

          if (LSearchRec.Attr and faDirectory) > 0 then
          begin
            // is a directory
            CleanDirectoryContents(inPath, LCurrentSubPathWithSlash + LCurrentFile, inSkipList); // clean us up recursively
            try RemoveDir(LCurrentFilePath); except end;
          end else
          begin
            // is a file
            try DeleteFile(LCurrentFilePath); except end;
          end;
        except end;
      until FindNext(LSearchRec) <> 0;
    end;
  except end;
end;

procedure TTrayBrowserApplication.CleanupVolatileDirectory;
var
  LSkipList: TTBStringDict;
  i: Integer;
  s: String;
begin
  if not Settings.ClearDataDirectory then Exit; // keep the data directory, nothing to do

  LSkipList := TTBStringDict.Create;
  try
    if Settings.KeepCookies then
    begin
      for i := 0 to Length(TrayBrowserProfileKeepList) - 1 do
      begin
        s := StringReplace(TrayBrowserProfileKeepList[i], '/', DirectorySeparator, [rfReplaceAll]);
        LSkipList.AddOrSetValue(s, s);
      end;
      for i := 0 to Length(TrayBrowserProfileKeepCookiesList) - 1 do
      begin
        s := StringReplace(TrayBrowserProfileKeepCookiesList[i], '/', DirectorySeparator, [rfReplaceAll]);
        LSkipList.AddOrSetValue(s, s);
      end;
    end;

    If Settings.KeepStorage then
    begin
      for i := 0 to Length(TrayBrowserProfileKeepList) - 1 do
      begin
        s := StringReplace(TrayBrowserProfileKeepList[i], '/', DirectorySeparator, [rfReplaceAll]);
        LSkipList.AddOrSetValue(s, s);
      end;
      for i := 0 to Length(TrayBrowserProfileKeepStorageList) - 1 do
      begin
        s := StringReplace(TrayBrowserProfileKeepStorageList[i], '/', DirectorySeparator, [rfReplaceAll]);
        LSkipList.AddOrSetValue(s, s);
      end;
    end;

    CleanDirectoryContents(VolatilePath, '', LSkipList);
  except end;
  FreeAndNil(LSkipList);
end;

// child process launch command line modification (adds IPC key used to communicate with main process) handler

procedure TrayBrowserCEFOnBeforeChildProcessLaunch(const inCommandline: ICefCommandLine);
begin
  inCommandline.AppendSwitchWithValue('TBApplicationID', UnicodeString(TrayBrowserApplication.ApplicationID));
  inCommandline.AppendSwitchWithValue('TBIPCKey', UnicodeString(TrayBrowserApplication.IPCKey));
  if TrayBrowserApplication.Settings.UseNativeDataDirectory then inCommandline.AppendSwitchWithValue('TBUseNativeDataDirectory', 'yes');
end;

// application helper handlers

procedure TrayBrowserSynchronizeWidgetSet;
begin
  // this is one real hack we need to synchronize with the widget system here as proper position is only available after widget system catches up with events
  // basically we need state restore messages to all be processed by widget system before we can act and thus we allow application to process window event messages
  // why? try removing this on Windows platform and switching maximized / minimized modes & hide / unhide windows in these modes, you'll quickly see what happens
  // it actually seems just one call to ProcessMessages is really necessary, but we do three to stay safe in possibly complex situations
  Application.ProcessMessages;
  Application.ProcessMessages;
  Application.ProcessMessages;
end;

function TrayBrowserGetApplicationName: String;
begin
  Result := TrayBrowserApplication.ApplicationID;
end;

function TrayBrowserGetVendorName: String;
begin
  Result := '';
end;

////////////////////////////////////////////////////////////
// TBJSVariant

class operator TTBJSVariant.Initialize(var Rec: TTBJSVariant); inline;
begin
  Rec.pvType := tbjvEmpty;
end;

procedure TTBJSVariant.SetBoolean(const inBoolean: Boolean); inline;
begin
  pvBoolean := inBoolean;
  pvType := tbjvBoolean;
  pvString := '';
end;

procedure TTBJSVariant.SetInteger(const inInteger: Integer); inline;
begin
  pvInteger := inInteger;
  pvType := tbjvInteger;
  pvString := '';
end;

procedure TTBJSVariant.SetFloat(const inFloat: Float); inline;
begin
  pvFloat := inFloat;
  pvType := tbjvFloat;
  pvString := '';
end;

procedure TTBJSVariant.SetString(const inString: String); inline;
begin
  pvString := inString;
  pvType := tbjvString;
end;

procedure TTBJSVariant.SetJSON(const inString: String); inline;
begin
  pvString := inString;
  pvType := tbjvJson;
end;

end.

