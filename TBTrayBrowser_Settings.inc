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

// application and main window settings (.ini/.conf files, icon loading, that kind of stuff)

procedure TTrayBrowserApplication.InitializeSettingsList;
begin
  // early initialization
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.ApplicationID,                 'ApplicationID',           TB_ID_STRING,          64,            [tbsefApplication,tbsefInit,tbsefNoEmpty,tbsefProtected]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.UseNativeDataDirectory,        'UseNativeDataDirectory',  False,                                [tbsefApplication,tbsefInit,tbsefProtected,tbsefInvisible]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.AllowDebug,                    'AllowDebug',              False,                                [tbsefApplication,tbsefInit,tbsefProtected]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.SingleProcess,                 'SingleProcess',           False,                                [tbsefApplication,tbsefInit,tbsefProtected,tbsefInvisible]));

  // process initialization
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.AutoPrint,                     'AutoPrint',               False,                                [tbsefApplication,tbsefProcessInit,tbsefProtected]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.ConservativeCache,             'ConservativeCache',       True,                                 [tbsefApplication,tbsefProcessInit,tbsefProtected]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.ConserveResources,             'ConserveResources',       False,                                [tbsefApplication,tbsefProcessInit,tbsefProtected]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.Copyright,                     'Copyright',               TB_COPYRIGHT_STRING,   250,           [tbsefApplication,tbsefProcessInit,tbsefProtected]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.EnableGPU,                     'EnableGPU',               False,                                [tbsefApplication,tbsefProcessInit,tbsefProtected]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.EnableHangMonitor,             'EnableHangMonitor',       False,                                [tbsefApplication,tbsefProcessInit,tbsefProtected]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.Title,                         'Caption',                 TB_TITLE_STRING,       250,           [tbsefApplication,tbsefProcessInit,tbsefNoEmpty,tbsefProtected,tbsefNoPrint,tbsefInvisible])); // alias, order matters: Caption first, Title overrides
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.Title,                         'Title',                   TB_TITLE_STRING,       250,           [tbsefApplication,tbsefProcessInit,tbsefNoEmpty,tbsefProtected])); // alias, order matters: Caption first, Title overrides
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.UserAgent,                     'UserAgent',               '%DEFAULT%',           1000,          [tbsefApplication,tbsefProcessInit,tbsefProtected]));

  SettingsList.Add(TTBSettingElement.CreateChoice(@TempSettings.ScalingMode,             'ScalingMode',             Integer(tbscNormal),                  [tbsefApplication,tbsefProcessInit,tbsefProtected],
    [
      SettingChoiceDef(Integer(tbscNormal), 'normal'),
      SettingChoiceDef(Integer(tbscFloor), 'floor'),
      SettingChoiceDef(Integer(tbscNoCorrect), 'nocorrect'),
      SettingChoiceDef(Integer(tbscNoScale), 'noscale')
    ],[
      SettingChoiceDef(Integer(tbscNormal), 'normal'), SettingChoiceDef(Integer(tbscNormal), 'ceil'),
      SettingChoiceDef(Integer(tbscFloor), 'floor'),
      SettingChoiceDef(Integer(tbscNoCorrect), 'nocorrect'), SettingChoiceDef(Integer(tbscNoCorrect), 'precise'),
      SettingChoiceDef(Integer(tbscNoScale), 'noscale'), SettingChoiceDef(Integer(tbscNoScale), 'noscaling'), SettingChoiceDef(Integer(tbscNoScale), 'ignore')
    ]
  ));

  // application
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.AllowExec,                     'AllowExec',               False,                                [tbsefApplication,tbsefApplicationInit,tbsefProtected]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.AllowSystemInfo,               'AllowSystemInfo',         True,                                 [tbsefApplication,tbsefApplicationInit,tbsefProtected]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.AutoHide,                      'AutoHide',                False,                                [tbsefApplication,tbsefApplicationInit]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.ClearDataDirectory,            'ClearDataDirectory',      False,                                [tbsefApplication,tbsefApplicationInit,tbsefProtected,tbsefInvisible]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.ClearKVStore,                  'ClearKVStore',            False,                                [tbsefApplication,tbsefApplicationInit,tbsefProtected,tbsefInvisible]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.Hint,                          'Hint',                    TB_TITLE_STRING,       250,           [tbsefApplication,tbsefApplicationInit]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.KeepCookies,                   'KeepCookies',             True,                                 [tbsefApplication,tbsefApplicationInit,tbsefProtected,tbsefInvisible]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.KeepStorage,                   'KeepStorage',             False,                                [tbsefApplication,tbsefApplicationInit,tbsefProtected,tbsefInvisible]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.MenuDisable,                   'MenuDisable',             False,                                [tbsefApplication,tbsefApplicationInit]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.MenuExitCaption,               'MenuExitCaption',         TB_MENU_EXIT_STRING,   250,           [tbsefApplication,tbsefApplicationInit,tbsefNoEmpty]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.MenuHideExit,                  'MenuHideExit',            False,                                [tbsefApplication,tbsefApplicationInit]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.MenuHideRetry,                 'MenuHideRetry',           False,                                [tbsefApplication,tbsefApplicationInit]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.MenuRetryCaption,              'MenuRetryCaption',        TB_MENU_RETRY_STRING,  250,           [tbsefApplication,tbsefApplicationInit,tbsefNoEmpty]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.RetryInterval,                 'RetryInterval',           15,                    1, 86400,      [tbsefApplication,tbsefApplicationInit,tbsefProtected]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.ShowTrayIcon,                  'ShowTrayIcon',            True,                                 [tbsefApplication,tbsefApplicationInit]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.URL,                           'URL',                     TB_ABOUT_URL,          16384,         [tbsefApplication,tbsefApplicationInit,tbsefNoClamp,tbsefNoEmpty,tbsefProtected]));

  SettingsList.Add(TTBSettingElement.CreateChoice(@TempSettings.ShowOnCLI,               'ShowOnCLI',               Integer(tbccOnEmpty),                 [tbsefApplication,tbsefApplicationInit],
    [
      SettingChoiceDef(Integer(tbccOnEmpty), 'empty'),
      SettingChoiceDef(Integer(tbccAlways), 'always'),
      SettingChoiceDef(Integer(tbccNever), 'never')
    ],[
      SettingChoiceDef(Integer(tbccOnEmpty), 'empty'), SettingChoiceDef(Integer(tbccOnEmpty), 'onempty'),
      SettingChoiceDef(Integer(tbccAlways), 'always'), SettingChoiceDef(Integer(tbccAlways), '+'), SettingChoiceDef(Integer(tbccAlways), 'yes'), SettingChoiceDef(Integer(tbccAlways), 'on'), SettingChoiceDef(Integer(tbccAlways), 'true'), SettingChoiceDef(Integer(tbccAlways), 'enable'), SettingChoiceDef(Integer(tbccAlways), 'enabled'), SettingChoiceDef(Integer(tbccAlways), 'active'), SettingChoiceDef(Integer(tbccAlways), '1'),
      SettingChoiceDef(Integer(tbccNever), 'never'), SettingChoiceDef(Integer(tbccNever), '-'), SettingChoiceDef(Integer(tbccNever), 'no'), SettingChoiceDef(Integer(tbccNever), 'none'), SettingChoiceDef(Integer(tbccNever), 'off'), SettingChoiceDef(Integer(tbccNever), 'false'), SettingChoiceDef(Integer(tbccNever), 'disable'), SettingChoiceDef(Integer(tbccNever), 'disabled'), SettingChoiceDef(Integer(tbccNever), 'inactive'), SettingChoiceDef(Integer(tbccNever), 'passive'), SettingChoiceDef(Integer(tbccNever), '0')
    ]
  ));

  SettingsList.Add(TTBSettingElement.CreateChoice(@TempSettings.TrayIconClickMode,       'TrayIconClickMode',       Integer(tbtcNormal),                  [tbsefApplication,tbsefApplicationInit],
    [
      SettingChoiceDef(Integer(tbtcNormal), 'normal'),
      SettingChoiceDef(Integer(tbtcRestore), 'restore'),
      SettingChoiceDef(Integer(tbtcShow), 'show'),
      SettingChoiceDef(Integer(tbtcEvent), 'event'),
      SettingChoiceDef(Integer(tbtcNone), 'none')
    ],[
      SettingChoiceDef(Integer(tbtcNormal), 'normal'),
      SettingChoiceDef(Integer(tbtcRestore), 'restore'),
      SettingChoiceDef(Integer(tbtcShow), 'show'), SettingChoiceDef(Integer(tbtcShow), 'showonly'), SettingChoiceDef(Integer(tbtcShow), 'nohide'),
      SettingChoiceDef(Integer(tbtcEvent), 'event'),
      SettingChoiceDef(Integer(tbtcNone), 'none'), SettingChoiceDef(Integer(tbtcNone), '-'), SettingChoiceDef(Integer(tbtcNone), 'no'), SettingChoiceDef(Integer(tbtcNone), 'off'), SettingChoiceDef(Integer(tbtcNone), 'false'), SettingChoiceDef(Integer(tbtcNone), 'disable'), SettingChoiceDef(Integer(tbtcNone), 'disabled'), SettingChoiceDef(Integer(tbtcNone), 'inactive'), SettingChoiceDef(Integer(tbtcNone), 'passive'), SettingChoiceDef(Integer(tbtcNone), '0')
    ]
  ));

  // application settings that are actually just defaults for window settings that need to be remembered
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.Size.Width,                    'Width',                   450,                   1, 65535,      [tbsefApplication,tbsefApplicationInit,tbsefProtected]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.Size.Height,                   'Height',                  350,                   1, 65535,      [tbsefApplication,tbsefApplicationInit,tbsefProtected]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.GracefulExitTime,              'GracefulExitTime',        1,                     1, 86400,      [tbsefApplication,tbsefApplicationInit,tbsefProtected]));
  SettingsList.Add(TTBSettingElement.Create(@TempSettings.ScaleAlignmentMax,             'ScaleAlignmentMax',       128,                   1, 1024,       [tbsefApplication,tbsefApplicationInit,tbsefProtected]));

  SettingsList.Add(TTBSettingElement.CreateChoice(@TempSettings.LoadErrorPage,           'LoadErrorPage',           Integer(tbleInternal),                [tbsefApplication,tbsefApplicationInit,tbsefProtected],
    [
      SettingChoiceDef(Integer(tbleInternal), 'internal'),
      SettingChoiceDef(Integer(tbleBrowser), 'browser')
    ],[
      SettingChoiceDef(Integer(tbleInternal), 'internal'),
      SettingChoiceDef(Integer(tbleBrowser), 'browser')
    ]
  ));

  // window
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.Movable,                 'Movable',                 True,                                 [tbsefWindow]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.Sizeable,                'Sizable',                 False,                                [tbsefWindow,tbsefNoPrint,tbsefInvisible])); // alias, order matters: Sizable first, Sizeable overrides
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.Sizeable,                'Sizeable',                False,                                [tbsefWindow,tbsefRestart])); // alias, order matters: Sizable first, Sizeable overrides
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.Size.Width,              'Width',                   450,                   1, 65535,      [tbsefWindow,tbsefProtected]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.Size.Height,             'Height',                  350,                   1, 65535,      [tbsefWindow,tbsefProtected]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.AllowCopy,               'AllowCopy',               True,                                 [tbsefWindow]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.AllowDownload,           'AllowDownload',           True,                                 [tbsefWindow]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.AllowPaste,              'AllowPaste',              True,                                 [tbsefWindow]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.AllowPrint,              'AllowPrint',              False,                                [tbsefWindow]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.AllowReload,             'AllowReload',             True,                                 [tbsefWindow]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.AllowSearch,             'AllowSearch',             True,                                 [tbsefWindow]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.AlwaysOnTop,             'AlwaysOnTop',             False,                                [tbsefWindow,tbsefRestart]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.APIEnabled,              'APIEnabled',              True,                                 [tbsefWindow,tbsefProtected,tbsefInvisible]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.BorderMargin,            'BorderMargin',            0,                     0, 65535,      [tbsefWindow]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.Caption,                 'Title',                   '',                    250,           [tbsefWindow,tbsefNoPrint,tbsefInvisible])); // alias, order matters: Title first, Caption overrides
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.Caption,                 'Caption',                 '',                    250,           [tbsefWindow])); // alias, order matters: Title first, Caption overrides
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.ClampToMonitor,          'ClampToMonitor',          True,                                 [tbsefWindow]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.CloseButton,             'CloseButton',             True,                                 [tbsefWindow]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.GracefulExitTime,        'GracefulExitTime',        1,                     1, 86400,      [tbsefWindow,tbsefProtected]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.IgnoreCertificateErrors, 'IgnoreCertificateErrors', False,                                [tbsefWindow,tbsefProtected]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.KeepHidden,              'KeepHidden',              False,                                [tbsefWindow]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.MaximizeButton,          'MaximizeButton',          False,                                [tbsefWindow]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.MinimizeButton,          'MinimizeButton',          False,                                [tbsefWindow]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.Parameters,              'Parameters',              '',                    16384,         [tbsefWindow,tbsefProtected,tbsefInvisible]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.PositionID,              'PositionID',              '',                    1024,          [tbsefWindow,tbsefProtected,tbsefInvisible]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.RememberSize,            'RememberSize',            False,                                [tbsefWindow]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.RestoreOnShow,           'RestoreOnShow',           True,                                 [tbsefWindow]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.ScaleAlignmentMax,       'ScaleAlignmentMax',       128,                   1, 1024,       [tbsefWindow,tbsefProtected,tbsefInvisible]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.ShowOnStart,             'ShowOnStart',             True,                                 [tbsefWindow,tbsefProtected]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.ShowOnTaskBar,           'ShowOnTaskbar',           True,                                 [tbsefWindow,tbsefRestart]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.SnapToFollowTrayClick,   'SnapToFollowTrayClick',   True,                                 [tbsefWindow]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.SnapToWindows,           'SnapToWindows',           True,                                 [tbsefWindow]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.SnapToWindowsDistance,   'SnapToWindowsDistance',   16,                    1, 128,        [tbsefWindow]));
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.StartMonitorText,        'StartMonitor',            '',                    16,            [tbsefWindow,tbsefProtected])); // complex setting that is parsed in FixupWindowSettings
  SettingsList.Add(TTBSettingElement.Create(@TempWindowSettings.URL,                     'URL',                     TB_ABOUT_URL,          16384,         [tbsefWindow,tbsefNoClamp,tbsefNoEmpty]));

  SettingsList.Add(TTBSettingElement.CreateChoice(@TempWindowSettings.Border,            'Border',                  Integer(tbbtWindow),                  [tbsefWindow,tbsefRestart],
    [
      SettingChoiceDef(Integer(tbbtNormal), 'normal'),
      SettingChoiceDef(Integer(tbbtBorderless), 'borderless'),
      SettingChoiceDef(Integer(tbbtWindow), 'window')
    ],[
      SettingChoiceDef(Integer(tbbtNormal), 'normal'),
      SettingChoiceDef(Integer(tbbtBorderless), 'never'), SettingChoiceDef(Integer(tbbtBorderless), '-'), SettingChoiceDef(Integer(tbbtBorderless), 'no'), SettingChoiceDef(Integer(tbbtBorderless), 'none'), SettingChoiceDef(Integer(tbbtBorderless), 'off'), SettingChoiceDef(Integer(tbbtBorderless), 'false'), SettingChoiceDef(Integer(tbbtBorderless), 'disable'), SettingChoiceDef(Integer(tbbtBorderless), 'disabled'), SettingChoiceDef(Integer(tbbtBorderless), 'inactive'), SettingChoiceDef(Integer(tbbtBorderless), 'passive'), SettingChoiceDef(Integer(tbbtBorderless), '0'),
      SettingChoiceDef(Integer(tbbtWindow), 'window'), SettingChoiceDef(Integer(tbbtWindow), 'os')
    ]
  ));

  SettingsList.Add(TTBSettingElement.CreateChoice(@TempWindowSettings.CloseMode,         'CloseMode',               Integer(tbcmExit),                    [tbsefWindow],
    [
      SettingChoiceDef(Integer(tbcmNone), 'none'),
      SettingChoiceDef(Integer(tbcmHide), 'hide'),
      SettingChoiceDef(Integer(tbcmMinimize), 'minimize'),
      SettingChoiceDef(Integer(tbcmExit), 'exit'),
      SettingChoiceDef(Integer(tbcmEvent), 'event')
    ],[
      SettingChoiceDef(Integer(tbcmNone), 'none'), SettingChoiceDef(Integer(tbcmNone), '-'), SettingChoiceDef(Integer(tbcmNone), 'no'), SettingChoiceDef(Integer(tbcmNone), 'off'), SettingChoiceDef(Integer(tbcmNone), 'false'), SettingChoiceDef(Integer(tbcmNone), 'disable'), SettingChoiceDef(Integer(tbcmNone), 'disabled'), SettingChoiceDef(Integer(tbcmNone), 'inactive'), SettingChoiceDef(Integer(tbcmNone), 'passive'), SettingChoiceDef(Integer(tbcmNone), '0'),
      SettingChoiceDef(Integer(tbcmHide), 'hide'),
      SettingChoiceDef(Integer(tbcmMinimize), 'minimize'),
      SettingChoiceDef(Integer(tbcmExit), 'exit'), SettingChoiceDef(Integer(tbcmExit), 'close'),
      SettingChoiceDef(Integer(tbcmEvent), 'event')
    ]
  ));

  SettingsList.Add(TTBSettingElement.CreateChoice(@TempWindowSettings.FullScreen,        'Fullscreen',              Integer(tbfsNone),                    [tbsefWindow,tbsefRestart],
    [
      SettingChoiceDef(Integer(tbfsNone), 'no'),
      SettingChoiceDef(Integer(tbfsWindow), 'window'),
      SettingChoiceDef(Integer(tbfsDesktop), 'desktop'),
      SettingChoiceDef(Integer(tbfsKiosk), 'kiosk'),
      SettingChoiceDef(Integer(tbfsAll), 'all')
    ],[
      SettingChoiceDef(Integer(tbfsNone), 'no'), SettingChoiceDef(Integer(tbfsNone), '-'), SettingChoiceDef(Integer(tbfsNone), 'none'), SettingChoiceDef(Integer(tbfsNone), 'off'), SettingChoiceDef(Integer(tbfsNone), 'false'), SettingChoiceDef(Integer(tbfsNone), 'disable'), SettingChoiceDef(Integer(tbfsNone), 'disabled'), SettingChoiceDef(Integer(tbfsNone), 'inactive'), SettingChoiceDef(Integer(tbfsNone), 'passive'), SettingChoiceDef(Integer(tbfsNone), '0'),
      SettingChoiceDef(Integer(tbfsWindow), 'window'),
      SettingChoiceDef(Integer(tbfsDesktop), 'desktop'),
      SettingChoiceDef(Integer(tbfsKiosk), 'kiosk'),
      SettingChoiceDef(Integer(tbfsAll), 'all')
    ]
  ));

  SettingsList.Add(TTBSettingElement.CreateChoice( @TempWindowSettings.LoadErrorPage,    'LoadErrorPage',           Integer(tbleInternal),                [tbsefWindow,tbsefProtected],
    [
      SettingChoiceDef(Integer(tbleInternal), 'internal'),
      SettingChoiceDef(Integer(tbleBrowser), 'browser')
    ],[
      SettingChoiceDef(Integer(tbleInternal), 'internal'),
      SettingChoiceDef(Integer(tbleBrowser), 'browser')
    ]
  ));

  SettingsList.Add(TTBSettingElement.CreateChoice( @TempWindowSettings.MaximizeMode,     'MaximizeMode',            Integer(tbmaNone),                    [tbsefWindow],
    [
      SettingChoiceDef(Integer(tbmaNone), 'none'),
      SettingChoiceDef(Integer(tbmaOS), 'os'),
      SettingChoiceDef(Integer(tbmaEvent), 'event')
    ],[
      SettingChoiceDef(Integer(tbmaNone), 'none'), SettingChoiceDef(Integer(tbmaNone), '-'), SettingChoiceDef(Integer(tbmaNone), 'no'), SettingChoiceDef(Integer(tbmaNone), 'off'), SettingChoiceDef(Integer(tbmaNone), 'false'), SettingChoiceDef(Integer(tbmaNone), 'disable'), SettingChoiceDef(Integer(tbmaNone), 'disabled'), SettingChoiceDef(Integer(tbmaNone), 'inactive'), SettingChoiceDef(Integer(tbmaNone), 'passive'), SettingChoiceDef(Integer(tbmaNone), '0'),
      SettingChoiceDef(Integer(tbmaOS), 'os'),
      SettingChoiceDef(Integer(tbmaEvent), 'event')
    ]
  ));

  SettingsList.Add(TTBSettingElement.CreateChoice( @TempWindowSettings.MinimizeMode,     'MinimizeMode',            Integer(tbmiNone),                    [tbsefWindow],
    [
      SettingChoiceDef(Integer(tbmiNone), 'none'),
      SettingChoiceDef(Integer(tbmiHide), 'hide'),
      SettingChoiceDef(Integer(tbmiTaskbar), 'taskbar'),
      SettingChoiceDef(Integer(tbmiEvent), 'event')
    ],[
      SettingChoiceDef(Integer(tbmiNone), 'none'), SettingChoiceDef(Integer(tbmiNone), '-'), SettingChoiceDef(Integer(tbmiNone), 'no'), SettingChoiceDef(Integer(tbmiNone), 'off'), SettingChoiceDef(Integer(tbmiNone), 'false'), SettingChoiceDef(Integer(tbmiNone), 'disable'), SettingChoiceDef(Integer(tbmiNone), 'disabled'), SettingChoiceDef(Integer(tbmiNone), 'inactive'), SettingChoiceDef(Integer(tbmiNone), 'passive'), SettingChoiceDef(Integer(tbmiNone), '0'),
      SettingChoiceDef(Integer(tbmiHide), 'hide'),
      SettingChoiceDef(Integer(tbmiTaskbar), 'taskbar'),
      SettingChoiceDef(Integer(tbmiEvent), 'event')
    ]
  ));

  SettingsList.Add(TTBSettingElement.CreateChoice( @TempWindowSettings.RememberPosition, 'RememberPosition',        Integer(tbrpFull),                    [tbsefWindow],
    [
      SettingChoiceDef(Integer(tbrpNone), 'no'),
      SettingChoiceDef(Integer(tbrpFull), 'full'),
      SettingChoiceDef(Integer(tbrpMonitor), 'monitor')
    ],[
      SettingChoiceDef(Integer(tbrpNone), 'no'), SettingChoiceDef(Integer(tbrpNone), '-'), SettingChoiceDef(Integer(tbrpNone), 'none'), SettingChoiceDef(Integer(tbrpNone), 'off'), SettingChoiceDef(Integer(tbrpNone), 'false'), SettingChoiceDef(Integer(tbrpNone), 'disable'), SettingChoiceDef(Integer(tbrpNone), 'disabled'), SettingChoiceDef(Integer(tbrpNone), 'inactive'), SettingChoiceDef(Integer(tbrpNone), 'passive'), SettingChoiceDef(Integer(tbrpNone), '0'),
      SettingChoiceDef(Integer(tbrpFull), 'full'), SettingChoiceDef(Integer(tbrpFull), '+'), SettingChoiceDef(Integer(tbrpFull), 'yes'), SettingChoiceDef(Integer(tbrpFull), 'on'), SettingChoiceDef(Integer(tbrpFull), 'true'), SettingChoiceDef(Integer(tbrpFull), 'enable'), SettingChoiceDef(Integer(tbrpFull), 'enabled'), SettingChoiceDef(Integer(tbrpFull), 'active'), SettingChoiceDef(Integer(tbrpFull), '1'),
      SettingChoiceDef(Integer(tbrpMonitor), 'monitor')
    ]
  ));

  SettingsList.Add(TTBSettingElement.CreateChoice( @TempWindowSettings.SnapTo,           'SnapTo',                  Integer(tbstNone),                    [tbsefWindow],
    [
      SettingChoiceDef(Integer(tbstNone), 'none'),
      SettingChoiceDef(Integer(tbstTray), 'tray'),
      SettingChoiceDef(Integer(tbstTopLeft), 'topleft'),
      SettingChoiceDef(Integer(tbstTopCenter), 'topcenter'),
      SettingChoiceDef(Integer(tbstTopRight), 'topright'),
      SettingChoiceDef(Integer(tbstCenterLeft), 'centerleft'),
      SettingChoiceDef(Integer(tbstCenter), 'center'),
      SettingChoiceDef(Integer(tbstCenterRight), 'centerright'),
      SettingChoiceDef(Integer(tbstBottomLeft), 'bottomleft'),
      SettingChoiceDef(Integer(tbstBottomCenter), 'bottomcenter'),
      SettingChoiceDef(Integer(tbstBottomRight), 'bottomright')
    ],[
      SettingChoiceDef(Integer(tbstNone), 'none'), SettingChoiceDef(Integer(tbstNone), '-'), SettingChoiceDef(Integer(tbstNone), 'no'), SettingChoiceDef(Integer(tbstNone), 'off'), SettingChoiceDef(Integer(tbstNone), 'false'), SettingChoiceDef(Integer(tbstNone), 'disable'), SettingChoiceDef(Integer(tbstNone), 'disabled'), SettingChoiceDef(Integer(tbstNone), 'inactive'), SettingChoiceDef(Integer(tbstNone), 'passive'), SettingChoiceDef(Integer(tbstNone), '0'),
      SettingChoiceDef(Integer(tbstTray), 'tray'),
      SettingChoiceDef(Integer(tbstTopLeft), 'topleft'), SettingChoiceDef(Integer(tbstTopLeft), 'lefttop'), SettingChoiceDef(Integer(tbstTopLeft), 'tl'), SettingChoiceDef(Integer(tbstTopLeft), 'lt'),
      SettingChoiceDef(Integer(tbstTopCenter), 'topcenter'), SettingChoiceDef(Integer(tbstTopCenter), 'centertop'), SettingChoiceDef(Integer(tbstTopCenter), 'tc'), SettingChoiceDef(Integer(tbstTopCenter), 'ct'),
      SettingChoiceDef(Integer(tbstTopRight), 'topright'), SettingChoiceDef(Integer(tbstTopRight), 'righttop'), SettingChoiceDef(Integer(tbstTopRight), 'tr'), SettingChoiceDef(Integer(tbstTopRight), 'rt'),
      SettingChoiceDef(Integer(tbstCenterLeft), 'centerleft'), SettingChoiceDef(Integer(tbstCenterLeft), 'leftcenter'), SettingChoiceDef(Integer(tbstCenterLeft), 'lc'), SettingChoiceDef(Integer(tbstCenterLeft), 'cl'),
      SettingChoiceDef(Integer(tbstCenter), 'center'),
      SettingChoiceDef(Integer(tbstCenterRight), 'centerright'), SettingChoiceDef(Integer(tbstCenterRight), 'rightcenter'), SettingChoiceDef(Integer(tbstCenterRight), 'rc'), SettingChoiceDef(Integer(tbstCenterRight), 'cr'),
      SettingChoiceDef(Integer(tbstBottomLeft), 'bottomleft'), SettingChoiceDef(Integer(tbstBottomLeft), 'leftbottom'), SettingChoiceDef(Integer(tbstBottomLeft), 'bl'), SettingChoiceDef(Integer(tbstBottomLeft), 'lb'),
      SettingChoiceDef(Integer(tbstBottomCenter), 'bottomcenter'), SettingChoiceDef(Integer(tbstBottomCenter), 'centerbottom'), SettingChoiceDef(Integer(tbstBottomCenter), 'bc'), SettingChoiceDef(Integer(tbstBottomCenter), 'cb'),
      SettingChoiceDef(Integer(tbstBottomRight), 'bottomright'), SettingChoiceDef(Integer(tbstBottomRight), 'rightbottom'), SettingChoiceDef(Integer(tbstBottomRight), 'br'), SettingChoiceDef(Integer(tbstBottomRight), 'rb')
    ]
  ));
end;

procedure TTrayBrowserApplication.SetDefaults;
var
  LElement: TTBSettingElement;
begin
  // apply all the application level defaults
  for LElement in SettingsList do
    if tbsefApplication in LElement.Flags then LElement.ApplyDefault;
end;

procedure TTrayBrowserApplication.SetWindowDefaults;
var
  LElement: TTBSettingElement;
begin
  // apply all window level defaults
  for LElement in SettingsList do
    if tbsefWindow in LElement.Flags then LElement.ApplyDefault;

  // there are few settings that require customized handling, apply their defaults
  TempWindowSettings.Main := False;
  TempWindowSettings.RestartSource := nil;
  TempWindowSettings.Size := Settings.Size;
  TempWindowSettings.GracefulExitTime := Settings.GracefulExitTime;
  TempWindowSettings.LoadErrorPage := Settings.LoadErrorPage;
  TempWindowSettings.ScaleAlignmentMax := Settings.ScaleAlignmentMax;
  TempWindowSettings.StartMonitorID := 0;
  TempWindowSettings.StartMonitorPosition := Point(0,0);
end;

procedure TTrayBrowserApplication.ApplyMainWindowDefaults(const inConfig: TTBStringDict; const inUser: Boolean = False);
begin
  // apply main window specific defaults
  TempWindowSettings.Main := True;
  TempWindowSettings.Caption := Settings.Title;
  TempWindowSettings.CloseButton := False;
  TempWindowSettings.CloseMode := tbcmHide;
  TempWindowSettings.Movable := False;
  TempWindowSettings.ShowOnStart := False;
  TempWindowSettings.ShowOnTaskBar := False;
  TempWindowSettings.URL := Settings.URL;

  // immovable main window has few different defaults applied, thus Movable is read early
  if inConfig.ContainsKey('movable') then
    try TempWindowSettings.Movable := StrArgToBool(inConfig['movable'], True, True); except end;

  if not TempWindowSettings.Movable then
  begin
    if (not inUser) and (not inConfig.ContainsKey('autohide')) then Settings.AutoHide := True; // if main window is immovable initially, global AutoHide also defaults to true (but it's no default if set explicitly)
    TempWindowSettings.AlwaysOnTop := True;
    TempWindowSettings.Border := tbbtNormal;
    TempWindowSettings.BorderMargin := 1;
    TempWindowSettings.SnapTo := tbstTray;
  end;
end;

procedure TTrayBrowserApplication.ApplySettings(const inConfig: TTBStringDict; const inFlags: TTBSettingFlagsSet; const inUser: Boolean = False);
var
  LElement: TTBSettingElement;
begin
  for LElement in SettingsList do
    if LElement.Flags >= inFlags then
      if (not inUser) or (not (tbsefProtected in LElement.Flags)) then
        if not LElement.ApplyConfig(inConfig) then
          if inConfig.ContainsKey(LElement.NameLC) then
            inConfig.Remove(LElement.NameLC);
end;

procedure TTrayBrowserApplication.FixupSettings(const inConfig: TTBStringDict);
begin
  // user-agent has few choice variants
  case TempSettings.UserAgent of
   '', 'default': TempSettings.UserAgent := '%DEFAULT%'; // empty user agent also means we use default
   'browser': TempSettings.UserAgent := '%BROWSER%';
  end;
end;

procedure TTrayBrowserApplication.FixupWindowSettings(const inConfig: TTBStringDict);
var
  LRegexp: TRegExpr;
  s: String;
begin
  // main window always has empty position ID
  if TempWindowSettings.Main then TempWindowSettings.PositionID := '';

  // set default minimize and maximize modes ('os' and 'taskbar') if corresponding buttons are enabled and setting is not done explicitly
  if TempWindowSettings.MaximizeButton and (not inConfig.ContainsKey('maximizemode')) then TempWindowSettings.MaximizeMode := tbmaOS;
  if TempWindowSettings.MinimizeButton and (not inConfig.ContainsKey('minimizemode')) then TempWindowSettings.MinimizeMode := tbmiTaskbar;

  // StartMonitor is a tricky setting that may have <monitor ID> or <x>,<y> options in addition to choice so we get it as string and parse here
  s := LowerCase(TempWindowSettings.StartMonitorText);
  case s of
    'default', '', '-', 'no', 'none', 'off', 'false', 'disable', 'disabled', 'inactive', 'passive': TempWindowSettings.StartMonitor := tbsmDefault;
    'main': TempWindowSettings.StartMonitor := tbsmMain;
    else
    begin
      try
        LRegexp := TRegExpr.Create;
        try
          LRegexp.Expression := '^\d+$';
          LRegexp.ModifierStr := 'isg-m-r-x';
          if LRegexp.Exec(s) then
          begin
             TempWindowSettings.StartMonitorID := StrToInt(s);
             if (TempWindowSettings.StartMonitorID >= 0) and (TempWindowSettings.StartMonitorID <= 255) then
               TempWindowSettings.StartMonitor := tbsmID;
          end else
          begin
            LRegexp.Expression := '^(-?\d+),(-?\d+)$';
            LRegexp.ModifierStr := 'isg-m-r-x';
            if LRegexp.Exec(s) then
            begin
              TempWindowSettings.StartMonitorPosition.X := StrToInt(LRegexp.Match[1]);
              TempWindowSettings.StartMonitorPosition.Y := StrToInt(LRegexp.Match[2]);
              if (TempWindowSettings.StartMonitorPosition.X >= -65535) and (TempWindowSettings.StartMonitorPosition.X <= 65535)
                and (TempWindowSettings.StartMonitorPosition.Y >= -65535) and (TempWindowSettings.StartMonitorPosition.Y <= 65535) then
                  TempWindowSettings.StartMonitor := tbsmXY
                else try inConfig.Remove('startmonitor'); except end;
            end else try inConfig.Remove('startmonitor'); except end;
          end;
        except
          try inConfig.Remove('startmonitor'); except end;
        end;
        FreeAndNil(LRegexp);
      except
        try inConfig.Remove('startmonitor'); except end;
      end;
    end;
  end;

  // fullscreen modes are also tricky:
  // 'window' mode allows window-based fullscreen with normal borders and stuff, enforcing: Movable=false, SnapToTray=false, MaximizeButton=False, MaximizeMode=none, SnapTo=center
  // any other mode additionally enforces: Border=none, BorderMargin=0, CloseButton=False, MinimizeButton=False, ScalingAlignment=0
  // kiosk and all monitors kiosk modes also add: AlwaysOnTop=true, AutoHide=false, MinimizeMode=none, ShowOnTaskbar=false
  if TempWindowSettings.FullScreen <> tbfsNone then
  begin
    TempWindowSettings.Movable := False;
    TempWindowSettings.Sizeable := False;
    TempWindowSettings.MaximizeButton := False;
    TempWindowSettings.MaximizeMode := tbmaNone;
    TempWindowSettings.RememberPosition := tbrpMonitor;
    TempWindowSettings.SnapTo := tbstCenter;

    if TempWindowSettings.FullScreen <> tbfsWindow then
    begin
      TempWindowSettings.Border := tbbtBorderless;
      TempWindowSettings.BorderMargin := 0;
      TempWindowSettings.CloseButton := False;
      TempWindowSettings.MinimizeButton := False;
      TempWindowSettings.ScaleAlignmentMax := 0;

      if TempWindowSettings.FullScreen <> tbfsDesktop then
      begin
        TempWindowSettings.AlwaysOnTop := True;
        TempWindowSettings.MinimizeMode := tbmiNone;
        TempWindowSettings.ShowOnTaskBar := False;
      end;
    end;
  end;

  // if we do not have OS borders, we have no sizeability and no buttons obviously
  if TempWindowSettings.Border <> tbbtWindow then
  begin
    TempWindowSettings.CloseButton := False;
    TempWindowSettings.MinimizeButton := False;
    TempWindowSettings.MaximizeButton := False;
    TempWindowSettings.Sizeable := False;
  end;

  // the rest of small adjustments
  if TempWindowSettings.MaximizeButton or TempWindowSettings.MinimizeButton then TempWindowSettings.CloseButton := True; // MinimizeButton and MaximizeButton enforce CloseButton
  if TempWindowSettings.SnapTo <> tbstNone then TempWindowSettings.Movable := False; // SnapTo inhibits final Movable value
  if (not TempWindowSettings.Movable) or (TempWindowSettings.Border <> tbbtWindow) then TempWindowSettings.Sizeable := False; // Sizeable cannot be enabled without Movable and BorderType=window
  if (TempWindowSettings.MinimizeMode = tbmiTaskbar) and (not TempWindowSettings.ShowOnTaskBar) then TempWindowSettings.MinimizeMode := tbmiEvent; // MinimizeMode of 'taskbar' does not work without ShowTaskbar=true
end;

procedure TTrayBrowserApplication.ReadSettings;
var
  LConfig: TTBStringDict;
  LRegexp: TRegExpr;
  i: Integer;
  s: String = '';
  b: Boolean;
begin
  LConfig := TTBStringDict.Create();
  try
    // read internally embedded settings
    try ParseSettings(ReadResource('TRAYBROWSER_INI'), LConfig); except end;
    try if LConfig.ContainsKey('icon') then IconPathList.Add(IfThen(FilenameIsAbsolute(LConfig['icon']), LConfig['icon'], AppPath + LConfig['icon'])); except end;

    // read settings files close to the application executable
    ReadSettingsFromFile(AppPath, AppName + '.ini', AppName, LConfig); // <application>.ini file from application directory
    ReadSettingsFromFile(AppPath, AppName + '.conf', AppName, LConfig); // <application>.conf file from application directory

    // TODO: read settings files from application symlink directory (Unix only)

    // read command line and command line settings, including configuration file supplied
    ReadCommandLine(LConfig);

    // apply early initialization level settings
    ApplySettings(LConfig, [tbsefApplication,tbsefInit]);

    // verify application ID, generate data directory path and make sure next set of files cannot override these
    LRegexp := TRegExpr.Create;
    try
      LRegexp.Expression := '[^a-z0-9_\-]';
      LRegexp.ModifierStr := 'isg-m-r-x';
      TempSettings.ApplicationId := LRegexp.Replace(TempSettings.ApplicationId, '');
    except end;
    FreeAndNil(LRegexp);
    if TempSettings.ApplicationId = '' then TempSettings.ApplicationId := TB_ID_STRING;

    NoOverride.AddOrSetValue('applicationid', '');
    NoOverride.AddOrSetValue('usenativedatadirectory', '');
    NoOverride.AddOrSetValue('allowdebug', '');
    NoOverride.AddOrSetValue('singleprocess', '');

    // use final settings set and set global application ID
    Settings := TempSettings;
    ApplicationID := Settings.ApplicationID;

    // set data directory paths
    BuildDataPath;

    {$IFDEF UNIX}
    // read /etc/<application ID>.ini and /etc/<application ID>.conf files
    ReadSettingsFromFile('/etc/', ApplicationId + '.ini', ApplicationId, LConfig);
    ReadSettingsFromFile('/etc/', ApplicationId + '.conf', ApplicationId, LConfig);
    {$ENDIF}

    // read configuration files from roaming (Windows-only) and local profile paths
    if (RoamingProfilePath <> ProfilePath) then
    begin
      ReadSettingsFromFile(RoamingProfilePath, ApplicationID + '.ini', ApplicationID, LConfig);
      ReadSettingsFromFile(RoamingProfilePath, ApplicationID + '.conf', ApplicationID, LConfig);
    end;
    ReadSettingsFromFile(ProfilePath, ApplicationID + '.ini', ApplicationID, LConfig);
    ReadSettingsFromFile(ProfilePath, ApplicationID + '.conf', ApplicationID, LConfig);

    // apply process initialization level settings
    ApplySettings(LConfig, [tbsefApplication,tbsefProcessInit]);

    // subprocesses do not need any other settings
    if IsSubprocess and (not Settings.SingleProcess) then
    begin
      FreeAndNil(LConfig);
      Exit;
    end;
  except Die('Failed to read configuration files'); end;

  // load application icon in order reverse to the configuration files read order
  b := False;
  s := '';
  if IconPathList.Count > 0 then
    for i := IconPathList.Count - 1 downto 0 do
    begin
      try
        if s = IconPathList[i] then Continue; // do not attempt to check / load the same icon multiple times
        s := IconPathList[i];
        if FileExists(s) then
        begin
          Application.Icon.LoadFromFile(s);
          b := True; // if we reached here, the icon was loases
          Break;
        end;
      except end;
    end;
  if not b then
    try Application.Icon.LoadFromResourceName(HINSTANCE, 'TRAYBROWSER_ICON'); except end; // last resort: embedded application icon

  // apply main application level settings
  ApplySettings(LConfig, [tbsefApplication,tbsefApplicationInit]);

  // fixup and use application settings
  FixupSettings(LConfig);
  Settings := TempSettings;

  // build main window defaults
  SetWindowDefaults;
  ApplyMainWindowDefaults(LConfig);

  // apply window level settings
  ApplySettings(LConfig, [tbsefWindow]);

  // fixup and use main window settings
  FixupWindowSettings(LConfig);
  MainWindowSettings := TempWindowSettings;

  // kiosk and above fullscreen modes inhibit autohide, but only if not set explicitly
  case MainWindowSettings.FullScreen of
    tbfsNone, tbfsDesktop, tbfsWindow: ; // nope
    else if not LConfig.ContainsKey('autohide') then Settings.AutoHide := False;
  end;

  // propagate some values to the application
  Application.Title := Settings.Title;
  Application.Hint := Settings.Hint;

  FreeAndNil(LConfig);
end;

procedure TTrayBrowserApplication.BuildDataPath;
begin
  {$IFDEF WINDOWS}
  // on Windows, we have both local and roaming profile paths, while on Unix we do not
  ProfilePath := IncludeTrailingPathDelimiter(CleanAndExpandDirectory(IncludeTrailingPathDelimiter(GetWindowsSpecialDir(CSIDL_LOCAL_APPDATA)) + IfThen(Settings.UseNativeDataDirectory, '', IncludeTrailingPathDelimiter('TrayBrowser'))));
  RoamingProfilePath := IncludeTrailingPathDelimiter(CleanAndExpandDirectory(IncludeTrailingPathDelimiter(GetWindowsSpecialDir(CSIDL_APPDATA)) + IfThen(Settings.UseNativeDataDirectory, '', IncludeTrailingPathDelimiter('TrayBrowser'))));
  {$ELSE}
  // on Unix platforms, we only use the general application-specific data directory
  ProfilePath := IncludeTrailingPathDelimiter(CleanAndExpandDirectory(IncludeTrailingPathDelimiter(GetAppConfigDir(False)) + '../' + IfThen(Settings.UseNativeDataDirectory, '', IncludeTrailingPathDelimiter('TrayBrowser'))));
  RoamingProfilePath := ProfilePath;
  {$ENDIF}

  // set data path and volatile data path according to local profile path
  DataPath := IncludeTrailingPathDelimiter(ProfilePath + ApplicationID);
  VolatilePath := IncludeTrailingPathDelimiter(DataPath + 'volatile');
end;

// listing settings back

procedure TTrayBrowserApplication.PrintSettings(const inConfig: TTBStringDict; const inFlags: TTBSettingFlagsSet; const inLowerCase: Boolean = False; const inUser: Boolean = False);
var
  LElement: TTBSettingElement;
  s: String;
begin
  for LElement in SettingsList do
  begin
    if LElement.Flags >= inFlags then
    begin
      if not (tbsefNoPrint in LElement.Flags) then
      begin
        if (not inUser) or (not (tbsefInvisible in LElement.Flags)) then
        begin
          if LElement.Print(s) then
          begin
            // few tricky settings need special printing
            case LElement.NameLC of
              'startmonitor':
              begin
                case TempWindowSettings.StartMonitor of
                  tbsmDefault: s := 'default';
                  tbsmMain: s := 'main';
                  tbsmID: s := IntToStr(TempWindowSettings.StartMonitorID);
                  tbsmXY: s := IntToStr(TempWindowSettings.StartMonitorPosition.X) + ',' + IntToStr(TempWindowSettings.StartMonitorPosition.Y);
                end;
              end;

              'useragent':
              begin
                case s of
                  '%DEFAULT%': s := 'default';
                  '%BROWSER%': s := 'browser';
                end;
              end;
            end;

            inConfig.AddOrSetValue(IfThen(inLowerCase, LElement.NameLC, LElement.Name), s);
          end;
        end;
      end;
    end;
  end;
end;

// argument read helpers

function TTrayBrowserApplication.StrArgToBool(const inStr: String; const inUseLiterals: Boolean = False; const inLiteralsOnly: Boolean = False): Boolean;
begin
  Result := False;
  if inUseLiterals then
  begin
    case LowerCase(inStr) of
      '', '-', 'no',  'none', 'off',  'false',  'disable', 'disabled', 'inactive', 'passive', '0': Exit;
          '+', 'yes', 'on',   'true', 'enable', 'enabled', 'active',                          '1':
      begin
        Result := True;
        Exit;
      end;
    end;
    if inLiteralsOnly then raise Exception.Create('Argument `' + inStr + '` cannot be converted to boolean value');
  end;

  try if StrToInt(inStr) = 0 then Exit; except end;
  try if StrToFloat(inStr) = 0 then Exit; except end;
  if inStr <> '' then Result := True;
end;

// settings read helpers

procedure TTrayBrowserApplication.ReadSettingsFromFile(const inPath: String; const inFile: String; const inBaseName: String; const inConfig: TTBStringDict);
var
  NoOverrideIcon: Boolean;
begin
  if not NoOverride.ContainsKey('*') then
  begin
    NoOverrideIcon := NoOverride.ContainsKey('icon'); // remember icon override status before processing this batch

    // parse settings from the file
    try ParseSettings(ReadFile(IncludeTrailingPathDelimiter(CleanAndExpandDirectory(inPath)) + inFile), inConfig); except end;

    // add icon load paths if we were allowed to override
    if not NoOverrideIcon then
    begin
      // base path and name icon
      if inBaseName <> '' then IconPathList.Add(IncludeTrailingPathDelimiter(CleanAndExpandDirectory(inPath)) + inBaseName + '.ico');

      // customized icon
      if inConfig.ContainsKey('icon') then
          IconPathList.Add(IfThen(FilenameIsAbsolute(inConfig['icon']), inConfig['icon'], IncludeTrailingPathDelimiter(CleanAndExpandDirectory(inPath)) + inConfig['icon']));
    end;
  end;
end;

procedure TTrayBrowserApplication.ReadCommandLine(const inConfig: TTBStringDict);
var
  LRegexp: TRegExpr;
  LNoOverrideAdd: TStringList;
  i: Integer;
  s: String;
begin
  CommandLine.Clear;
  LRegexp := TRegExpr.Create;
  LNoOverrideAdd := TStringList.Create;
  try
    for i := 1 to ParamCount do CommandLine.Add(ParamStr(i));
    if CommandLine.Count > 0 then
    begin
      if not NoOverride.ContainsKey('commandline') then
      begin
        try
          // the first parameter may be configuration file name
          if FileExists(CommandLine[0]) then
          begin
            ReadSettingsFromFile(IncludeTrailingPathDelimiter(CleanAndExpandDirectory(ExtractFilePath(CommandLine[0]))), ExtractFileName(CommandLine[0]), '', inConfig);
            CommandLine.Delete(0);
          end else
          begin
            // also the first parameter may be http:// or https:// URL
            LRegexp.Expression := '^https?://';
            LRegexp.ModifierStr := 'isg-m-r-x';
            if LRegexp.Exec(CommandLine[0]) then
            begin
              inConfig.AddOrSetValue('url', CommandLine[0]);
              CommandLine.Delete(0);
            end;
          end;

          // the rest of settings is parsed for key=value combination
          LRegexp.Expression := '^\s*([0-9a-z]+)\s*\=(.*)$';
          LRegexp.ModifierStr := 'isg-m-r-x';
          while (CommandLine.Count > 0) and LRegexp.Exec(CommandLine[0]) do
          begin
            AddParsedSetting(inConfig, LRegexp.Match[1], LRegexp.Match[2], LNoOverrideAdd);
            CommandLine.Delete(0);
          end;
          for s in LNoOverrideAdd do NoOverride.AddOrSetValue(s, ''); // combine additional NoOverride values
        except end;
      end;

      if (CommandLine.Count > 0) and (CommandLine[0] = '--') then CommandLine.Delete(0); // remove first encountered '--' parameter
      if (CommandLine.Count > TB_MAX_CMDLINE_PARAMS) then
        for i := CommandLine.Count - 1 downto TB_MAX_CMDLINE_PARAMS do
          CommandLine.Delete(i); // remove excessive number of parameters
    end;
  except end;
  FreeAndNil(LNoOverrideAdd);
  FreeAndNil(LRegexp);
end;

procedure TTrayBrowserApplication.AddParsedSetting(const inConfig: TTBStringDict; const inKey: String; const inValue: String; const inNoOverrideAdd: TStringList);
var
  LRegexp: TRegExpr;
  s, s2: String;
begin
  s := LowerCase(inKey);
  if not NoOverride.ContainsKey(s) then
  begin
    // handle empty keys and multi-value parameters that can be encountered more than once
    case s of
      '': ; // skip empty keys

      'nooverride':
      if (Length(inValue) >= 1) and (Length(inValue) <= 4000) then
        for s2 in SplitString(LowerCase(inValue), ',') do
          if (Trim(s2) <> '') then inNoOverrideAdd.Add(Trim(s2));

      'allowexecfilter':
      if (Length(inValue) >= 1) and (Length(inValue) <= 4000) and (Trim(inValue) <> '') then
        AllowExecFilter.Add(Trim(inValue));

      'allowexecregexp':
      if (Length(inValue) >= 1) and (Length(inValue) <= 4000) and (Trim(inValue) <> '') then
      begin
        LRegexp := TRegExpr.Create;
        try
          LRegexp.Expression := Trim(inValue);
          LRegexp.Compile; // this basically verifies if the expression is valid
          AllowExecRegexp.Add(LRegexp.Expression);
        except end;
        FreeAndNil(LRegexp);
      end;

      else inConfig.AddOrSetValue(s, Trim(inValue)); // normal parameter
    end;
  end;
end;

procedure TTrayBrowserApplication.ParseSettings(const inText: String; const inConfig: TTBStringDict);
var
  LNoOverrideAdd: TStringList;
  LRegexp: TRegExpr;
  s: String;
begin
  if NoOverride.ContainsKey('*') then Exit; // if override was disabled completely, we have nothing to do

  // parse
  LRegexp := TRegExpr.Create;
  LNoOverrideAdd := TStringList.Create;
  try
    LRegexp.Expression := '^\s*([0-9a-z]+)\s*\=(.*)$';
    LRegexp.ModifierStr := 'img-r-s-x';
    if LRegexp.Exec(inText) then
    begin
      AddParsedSetting(inConfig, LRegexp.Match[1], LRegexp.Match[2], LNoOverrideAdd);
      while LRegexp.ExecNext do AddParsedSetting(inConfig, LRegexp.Match[1], LRegexp.Match[2], LNoOverrideAdd);
    end;
    for s in LNoOverrideAdd do NoOverride.AddOrSetValue(s, ''); // combine additional NoOverride values
  except end;
  FreeAndNil(LNoOverrideAdd);
  FreeAndNil(LRegexp);
end;

function TTrayBrowserApplication.ReadResource(const inName: String): String;
var
  LStream: TResourceStream;
begin
  Result := '';
  LStream := TResourceStream.Create(HINSTANCE, inName, RT_RCDATA);
  if LStream.Size > 1048576 then
  begin
    FreeAndNil(LStream);
    raise Exception.Create('Resource `' + inName + '` is too large');
  end;
  try
    LStream.Seek(0, soBeginning);
    SetLength(Result, LStream.Size);
    LStream.Read(Result[1], LStream.Size);
  except
    FreeAndNil(LStream);
    raise Exception.Create('Failed to read resource `' + inName + '`');
  end;
  FreeAndNil(LStream);
end;

function TTrayBrowserApplication.ReadFile(const inPath: String): String;
var
  LStream: TFileStream;
begin
  Result := '';
  if not FileExists(inPath) then raise Exception.Create('File `' + inPath + '` not found');
  if FileSize(inPath) > 1048576 then raise Exception.Create('File `' + inPath + '` too large');
  LStream := TFileStream.Create(inPath, fmOpenRead or fmShareDenyNone);
  try
    LStream.Seek(0, soBeginning);
    SetLength(Result, LStream.Size);
    LStream.Read(Result[1], LStream.Size);
  except
    FreeAndNil(LStream);
    raise Exception.Create('Failed to read file `' + inPath + '`');
  end;
  FreeAndNil(LStream);
end;

////////////////////////////////////////////////////////////
// TTBSettingElement

constructor TTBSettingElement.Create(const inPtr: Pointer; const inName: String; const inDefault: Boolean; const inFlags: TTBSettingFlagsSet);
begin
  Initialize(inPtr, inName, inFlags);
  ValueType := tbsetBoolean;
  DefaultBool := inDefault;
end;

constructor TTBSettingElement.Create(const inPtr: Pointer; const inName: String; const inDefault: Integer; const inMin: Integer; const inMax: Integer; const inFlags: TTBSettingFlagsSet);
begin
  Initialize(inPtr, inName, inFlags);
  ValueType := tbsetInteger;
  DefaultInt := inDefault;
  IntMin := inMin;
  IntMax := inMax;
end;

constructor TTBSettingElement.Create(const inPtr: Pointer; const inName: String; const inDefault: String; const inMaxLength: Integer; const inFlags: TTBSettingFlagsSet);
begin
  Initialize(inPtr, inName, inFlags);
  ValueType := tbsetString;
  DefaultString := inDefault;
  StringMaxLength := inMaxLength;
end;

constructor TTBSettingElement.CreateChoice(const inPtr: Pointer; const inName: String; const inDefault: Integer; const inFlags: TTBSettingFlagsSet; const inChoices: array of TTBSettingChoiceDef; const inSelectors: array of TTBSettingChoiceDef);
var
  LChoiceDef: TTBSettingChoiceDef;
begin
  Initialize(inPtr, inName, inFlags);
  ValueType := tbsetChoice;
  DefaultInt := inDefault;
  Choices := TTBSettingChoiceList.Create;
  Selectors := TTBSettingChoiceSelectorList.Create;
  for LChoiceDef in inChoices do Choices.Add(LChoiceDef.i, LChoiceDef.s);
  for LChoiceDef in inSelectors do Selectors.Add(LowerCase(LChoiceDef.s), LChoiceDef.i);
end;

destructor TTBSettingElement.Destroy;
begin
  FreeAndNil(Choices);
  FreeAndNil(Selectors);
end;

procedure TTBSettingElement.Initialize(const inPtr: Pointer; const inName: String; const inFlags: TTBSettingFlagsSet);
begin
  Name := inName;
  NameLC := LowerCase(inName);
  ValueType := tbsetUndefined;
  ValuePtr := inPtr;
  Flags := inFlags;
  DefaultBool := False;
  DefaultInt := 0;
  DefaultString := '';
  IntMin := 0;
  IntMax := 0;
  StringMaxLength := 0;
  Choices := nil;
  Selectors := nil;
end;

procedure TTBSettingElement.ApplyDefault;
begin
  if ValueType = tbsetUndefined then Exit;
  if not Assigned(ValuePtr) then Exit;
  case ValueType of
    tbsetBoolean: PBoolean(ValuePtr)^ := DefaultBool;
    tbsetInteger, tbsetChoice: PInteger(ValuePtr)^ := DefaultInt;
    tbsetString: PString(ValuePtr)^ := DefaultString;
  end;
end;

function TTBSettingElement.ApplyConfig(const inConfig: TTBStringDict): Boolean;
var
  i: Integer;
  s: String;
begin
  Result := False;
  if ValueType = tbsetUndefined then Exit;
  if not Assigned(ValuePtr) then Exit;
  try
    if not inConfig.ContainsKey(NameLC) then Exit;
    s := inConfig[NameLC];
    case ValueType of
      tbsetBoolean:
      begin
        PBoolean(ValuePtr)^ := TrayBrowserApplication.StrArgToBool(s, True, True);
        Result := True;
      end;

      tbsetInteger:
      begin
        i := StrToInt(s);
        if (i >= IntMin) and (i <= IntMax) then
          Result := True
        else if not (tbsefNoClamp in Flags) then
        begin
          if i < IntMin then i := IntMin
            else if i > IntMax then i := IntMax;
          Result := True;
        end;
        if Result then PInteger(ValuePtr)^ := i;
      end;

      tbsetString:
      begin
        if (StringMaxLength = 0) or (Length(s) <= StringMaxLength) then
          Result := True
        else if not (tbsefNoClamp in Flags) then
        begin
          s := MidStr(s, 1, StringMaxLength);
          Result := True;
        end;
        if Result then PString(ValuePtr)^ := s;
      end;

      tbsetChoice:
      begin
        s := LowerCase(s);
        if Selectors.ContainsKey(s) then
        begin
          PInteger(ValuePtr)^ := Selectors[s];
          Result := True;
        end;
      end;
    end;
  except end;
end;

function TTBSettingElement.Print(out outStr: String): Boolean;
begin
  Result := False;
  outStr := '';
  if ValueType = tbsetUndefined then Exit;
  if not Assigned(ValuePtr) then Exit;
  try
    case ValueType of
      tbsetBoolean:
      begin
        outStr := IfThen(PBoolean(ValuePtr)^, 'yes', 'no');
        Result := True;
      end;

      tbsetInteger:
      begin
        outStr := IntToStr(PInteger(ValuePtr)^);
        Result := True;
      end;

      tbsetString:
      begin
        outStr := PString(ValuePtr)^;
        Result := True;
      end;

      tbsetChoice:
      begin
        if Choices.ContainsKey(PInteger(ValuePtr)^) then
        begin
          outStr := Choices[PInteger(ValuePtr)^];
          Result := True;
        end;
      end;
    end;
  except end;
end;

function TTrayBrowserApplication.SettingChoiceDef(const inValue: Integer; const inName: String): TTBSettingChoiceDef;
begin
  Result.i := inValue;
  Result.s := inName;
end;

