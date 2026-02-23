var __traybrowser;

// Licensed under BSD 3-clause license
// https://opensource.org/license/bsd-3-clause

// Copyright 2025-2026 Alex/AT (alex@alex-at.net)

// Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
// 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
// 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(function() {
  // general TrayBrowser API (TrayBrowser 1.0 and 2.0 compatible)
  __traybrowser = {};

  // information layer
  __traybrowser.GetAPIModel = function () { native function tbGetAPIModel(); return this.__convertResult(tbGetAPIModel()); }
  __traybrowser.GetAPIVersion = function () { native function tbGetAPIVersion(); return this.__convertResult(tbGetAPIVersion()); }
  __traybrowser.GetAPIOS = function () { native function tbGetAPIOS(); return this.__convertResult(tbGetAPIOS()); }
  __traybrowser.GetAPIBrowser = function () { native function tbGetAPIBrowser(); return this.__convertResult(tbGetAPIBrowser()); }
  __traybrowser.GetVersion = function () { native function tbGetVersion(); return this.__convertResult(tbGetVersion()); }
  __traybrowser.GetCopyright = function () { native function tbGetCopyright(); return this.__convertResult(tbGetCopyright()); }

  // TrayBrowser API

  // base window commands and information
  __traybrowser.Show = function () { native function tbShow(); return this.__convertResult(tbShow()); } // v1,v2,v3
  __traybrowser.Hide = function () { native function tbHide(); return this.__convertResult(tbHide()); } // v1,v2,v3
  __traybrowser.Focus = function () { native function tbFocus(); return this.__convertResult(tbFocus()); } // v3
  __traybrowser.Close = function () { native function tbClose(); return this.__convertResult(tbClose()); } // v3
  __traybrowser.Exit = function () { native function tbExit(); return this.__convertResult(tbExit()); } // v1,v2,v3
  __traybrowser.IsVisible = function () { native function tbIsVisible(); return this.__convertResult(tbIsVisible()); } // v1,v2,v3
  __traybrowser.IsInFocus = function () { native function tbIsInFocus(); return this.__convertResult(tbIsInFocus()); } // v1,v2,v3
  __traybrowser.WasPositionRestored = function () { native function tbWasPositionRestored(); return this.__convertResult(tbWasPositionRestored()); } // v1,v2,v3

  // window sizing and positioning
  __traybrowser.GetBrowserSize = function () { native function tbGetBrowserSize(); return this.__convertResult(tbGetBrowserSize()); } // v3
  __traybrowser.SetBrowserSize = function (width, height) { native function tbSetBrowserSize(width, height); return this.__convertResult(tbSetBrowserSize(width, height)); } // v3
  __traybrowser.GetPixelSize = function () { native function tbGetPixelSize(); return this.__convertResult(tbGetPixelSize()); } // v3
  __traybrowser.SetPixelSize = function (width, height) { native function tbSetPixelSize(width, height); return this.__convertResult(tbSetPixelSize(width, height)); } // v3
  __traybrowser.GetPixelPosition = function () { native function tbGetPixelPosition(); return this.__convertResult(tbGetPixelPosition()); } // v3
  __traybrowser.SetPixelPosition = function (x, y) { native function tbSetPixelPosition(x, y); return this.__convertResult(tbSetPixelPosition(x, y)); } // v3
  __traybrowser.GetScaledPosition = function () { native function tbGetScaledPosition(); return this.__convertResult(tbGetScaledPosition()); } // v3
  __traybrowser.SetScaledPosition = function (x, y) { native function tbSetScaledPosition(x, y); return this.__convertResult(tbSetScaledPosition(x, y)); } // v3
  __traybrowser.GetDesktopPosition = function () { native function tbGetDesktopPosition(); return this.__convertResult(tbGetDesktopPosition()); } // v3
  __traybrowser.SetDesktopPosition = function (x, y) { native function tbSetDesktopPosition(x, y); return this.__convertResult(tbSetDesktopPosition(x, y)); } // v3

  // window and screen information
  __traybrowser.GetMonitorBrowserSize = function () { native function tbGetMonitorBrowserSize(); return this.__convertResult(tbGetMonitorBrowserSize()); } // v3
  __traybrowser.GetMonitorPixelSize = function () { native function tbGetMonitorPixelSize(); return this.__convertResult(tbGetMonitorPixelSize()); } // v3
  __traybrowser.GetDesktopSize = function () { native function tbGetDesktopSize(); return this.__convertResult(tbGetDesktopSize()); } // v3
  __traybrowser.GetWindowInfo = function () { native function tbGetWindowInfo(); return this.__convertResult(tbGetWindowInfo()); } // v3
  __traybrowser.GetScreenInfo = function () { native function tbGetScreenInfo(); return this.__convertResult(tbGetScreenInfo()); } // v3
  __traybrowser.GetColorScheme = function () { native function tbGetColorScheme(); return this.__convertResult(tbGetColorScheme()); } // v3

  // window state
  __traybrowser.Minimize = function () { native function tbMinimize(); return this.__convertResult(tbMinimize()); } // v3
  __traybrowser.Maximize = function () { native function tbMaximize(); return this.__convertResult(tbMaximize()); } // v3
  __traybrowser.Restore = function () { native function tbRestore(); return this.__convertResult(tbRestore()); } // v3
  __traybrowser.GetWindowState = function () { native function tbGetWindowState(); return this.__convertResult(tbGetWindowState()); } // v3

  // custom drag region
  __traybrowser.SetCustomDragRegion = function (x, y, width, height) { native function tbSetCustomDragRegion(x, y, width, height); return this.__convertResult(tbSetCustomDragRegion(x, y, width, height)); }

  // browser commands
  __traybrowser.Start = function () { native function tbStart(); return this.__convertResult(tbStart()); }
  __traybrowser.Navigate = function (url) { native function tbNavigate(url); return this.__convertResult(tbNavigate(url)); }
  __traybrowser.RestartBrowser = function (url) { native function tbRestartBrowser(url); return this.__convertResult(tbRestartBrowser(url)); }

  // window and application properties
  __traybrowser.GetParameters = function () { native function tbGetParameters(); return this.__convertResult(tbGetParameters()); } // v3
  __traybrowser.GetCommandLine = function () { native function tbGetCommandLine(); return this.__convertResult(tbGetCommandLine()); } // v3
  __traybrowser.GetLoggedInUser = function () { native function tbGetLoggedInUser(); return this.__convertResult(tbGetLoggedInUser()); } // v1,v2,v3
  __traybrowser.SetTitle = function (text) { native function tbSetTitle(text); return this.__convertResult(tbSetTitle(text)); } // v1,v2,v3
  __traybrowser.SetCaption = function (text) { return this.SetTitle(text); } // v2,v3
  __traybrowser.SetHint  = function (text) { native function tbSetHint(text); return this.__convertResult(tbSetHint(text)); } // v1,v2,v3
  __traybrowser.GetApplicationSettings = function () { native function tbGetApplicationSettings(); return this.__convertResult(tbGetApplicationSettings()); } // v3
  __traybrowser.SetApplicationSettings = function (settings) { native function tbGetApplicationSettings(settings); return this.__convertResult(tbGetApplicationSettings(settings)); } // v3
  __traybrowser.GetWindowSettings = function () { native function tbGetWindowSettings(); return this.__convertResult(tbGetWindowSettings()); } // v3
  __traybrowser.SetWindowSettings = function (settings, fromDefaults) { native function tbSetWindowSettings(settings, fromDefaults); return this.__convertResult(tbSetWindowSettings(settings, fromDefaults)); } // v3

  // tray menu and notifications
  __traybrowser.__menuItemIDs = { };

  __traybrowser.AddMenuItem = function (text, callback) { // v1,v2,v3
    native function tbAddMenuItem(id, text);
    var id = this.__registerCallback(callback, false);
    this.__menuItemIDs[id] = id;
    return this.__convertResult(tbAddMenuItem(id, text));
  }

  __traybrowser.ClearMenu = function () { // v1,v2,v3
    native function tbClearMenu();
    for (var id of Object.keys(this.__menuItemIDs)) this.__clearCallback(id);
    this.__menuItemIDs = { };
    return this.__convertResult(tbClearMenu());
  };

  __traybrowser.ClearMenuAll = function () { // v3
    native function tbClearMenuAll();
    for (var id of Object.keys(this.__menuItemIDs)) this.__clearCallback(id);
    this.__menuItemIDs = { };
    return this.__convertResult(tbClearMenuAll());
  };

  __traybrowser.ShowBalloon = function (type, timeout, text, title, callback) { // v1,v2,v3
    native function tbShowBalloon(id, type, timeout, text, title);
    if (((typeof callback).toLowerCase() == 'string') && (callback.substring(0, 11).toLowerCase() != 'javascript:')) {
      // URL navigation type callback needs to be emulated
      var url = callback;
      callback = function () { traybrowser.Navigate(url); };
    }
    this.RegisterEvent('onballoonclick', callback);
    return this.__convertResult(tbShowBalloon(type, timeout, text, title));
  }

  __traybrowser.HideBalloon = function () { // v3
    native function tbHideBalloon();
    this.ClearEvent('onballoonclick');
    return this.__convertResult(tbHideBalloon());
  }

  // persistent key-value storage
  __traybrowser.ReadPersistentValue = function (name, defValue) { // v3
    native function tbReadKV(name);
    var result = this.__convertResult(tbReadKV(name));
    if ((typeof result).toLowerCase() == 'undefined') return defValue;
    return result;
  }

  __traybrowser.WritePersistentValue = function (name, value) { native function tbWriteKV(name, value); return this.__convertResult(tbWriteKV(name, value)); } // v3
  __traybrowser.RemovePersistentValue = function (name) { native function tbDeleteKV(name); return this.__convertResult(tbDeleteKV(name)); } // v3

  // temporary in-memory key-value storage
  __traybrowser.ReadTemporaryValue = function (name, defValue) { // v3
    native function tbReadTempKV(name);
    var result = this.__convertResult(tbReadTempKV(name));
    if ((typeof result).toLowerCase() == 'undefined') return defValue;
    return result;
  }

  __traybrowser.WriteTemporaryValue = function (name, value) { native function tbWriteTempKV(name, value); return this.__convertResult(tbWriteTempKV(name, value)); } // v3
  __traybrowser.RemoveTemporaryValue = function (name) { native function tbDeleteTempKV(name); return this.__convertResult(tbDeleteTempKV(name)); } // v3

  // process execution
  __traybrowser.EXEC_HIDDEN = 0;
  __traybrowser.EXEC_MINIMIZED = 1;
  __traybrowser.EXEC_NORMAL = 2;
  __traybrowser.EXEC_MAXIMIZED = 3;
  __traybrowser.EXEC_NOWAIT = 128;
  __traybrowser.EXEC_NOOUTPUT = 128;
  __traybrowser.EXEC_INDEPENDENT = 128;

  __traybrowser.OpenBrowser = function (url) { native function tbOpenBrowser(url); return this.__convertResult(tbOpenBrowser(url)); } // v1,v2,v3

  __traybrowser.ExecAsync = function (command, parameters, callback, mode, timeout) { // v3
    native function tbExec(id, command, parameters, mode, timeout);
    var cbid = null;
    if (((typeof mode) == 'undefined') || (mode === null)) mode = 0; // default to hiddent
    if (((typeof callback) != 'undefined') && (callback !== null))
      cbid = this.__registerCallback(callback, true); // if we are starting independent process, callback may still be used to know when process has terminated
    var res = this.__convertResult(tbExec(cbid, command, parameters, mode & this.EXEC_INTERNAL_ASYNC_MASK, timeout)); // take care mode is trimmed from internal flags
    if ((typeof result).toLowerCase() == 'string') return true; // process GUID is converted to true
    return result; // otherwise, undefined or false is returned
  }

  // multiple windows API
  __traybrowser.GetWindowId = function () { native function tbGetWindowId(); return this.__convertResult(tbGetWindowId()); } // v3
  __traybrowser.GetWindowID = function () { native function tbGetWindowId(); return this.__convertResult(tbGetWindowId()); } // v3
  __traybrowser.GetMainWindowId = function () { native function tbGetMainWindowId(); return this.__convertResult(tbGetMainWindowId()); } // v3
  __traybrowser.GetMainWindowID = function () { native function tbGetMainWindowId(); return this.__convertResult(tbGetMainWindowId()); } // v3
  __traybrowser.IsMainWindow = function () { native function tbIsMainWindow(); return this.__convertResult(tbIsMainWindow()); } // v3
  __traybrowser.GetWindowList = function () { native function tbGetWindowList(); return this.__convertResult(tbGetWindowList()); } // v3
  __traybrowser.CreateWindow = function (url, settings) { native function tbCreateWindow(url, settings); return this.__convertResult(tbCreateWindow(url, settings)); } // v3
  __traybrowser.WindowMessage = function (id, message) { native function tbWindowMessage(id, message); return this.__convertResult(tbWindowMessage(id, message)); } // v3
  __traybrowser.WindowBroadcast = function (message, withSelf) { native function tbWindowBroadcast(message, withSelf); return this.__convertResult(tbWindowBroadcast(message, withSelf)); } // v3
  __traybrowser.IsAnyWindowVisible = function () { native function tbIsAnyWindowVisible(); return this.__convertResult(tbIsAnyWindowVisible()); } // v3
  __traybrowser.IsApplicationVisible = function () { native function tbIsApplicationVisible(); return this.__convertResult(tbIsApplicationVisible()); } // v3
  __traybrowser.IsApplicationInFocus = function () { native function tbIsApplicationInFocus(); return this.__convertResult(tbIsApplicationInFocus()); } // v3
  __traybrowser.ApplicationShow = function () { native function tbApplicationShow(); return this.__convertResult(tbApplicationShow()); } // v3
  __traybrowser.ApplicationHide = function () { native function tbApplicationHide(); return this.__convertResult(tbApplicationHide()); } // v3
  __traybrowser.ApplicationTerminate = function () { native function tbApplicationTerminate(); return this.__convertResult(tbApplicationTerminate()); } // v3

  // multiple windows API: window API aliases
  __traybrowser.WindowShow = function (id) { native function tbWindowShow(id); return this.__convertResult(tbWindowShow(id)); } // v3
  __traybrowser.WindowHide = function (id) { native function tbWindowHide(id); return this.__convertResult(tbWindowHide(id)); } // v3
  __traybrowser.WindowFocus = function (id) { native function tbWindowFocus(id); return this.__convertResult(tbWindowFocus(id)); } // v3
  __traybrowser.WindowClose = function (id) { native function tbWindowClose(id); return this.__convertResult(tbWindowClose(id)); } // v3
  __traybrowser.WindowExit = function (id) { native function tbWindowExit(id); return this.__convertResult(tbWindowExit(id)); } // v3
  __traybrowser.WindowIsVisible = function (id) { native function tbWindowIsVisible(id); return this.__convertResult(tbWindowIsVisible(id)); } // v3
  __traybrowser.WindowIsInFocus = function (id) { native function tbWindowIsInFocus(id); return this.__convertResult(tbWindowIsInFocus(id)); } // v3
  __traybrowser.WindowWasPositionRestored = function (id) { native function tbWindowWindowWasPositionRestored(id); return this.__convertResult(tbWindowWasPositionRestored(id)); } // v3
  __traybrowser.WindowGetBrowserSize = function (id) { native function tbWindowGetBrowserSize(id); return this.__convertResult(tbWindowGetBrowserSize(id)); } // v3
  __traybrowser.WindowSetBrowserSize = function (id, width, height) { native function tbWindowSetBrowserSize(width, height, id); return this.__convertResult(tbWindowSetBrowserSize(width, height, id)); } // v3
  __traybrowser.WindowGetPixelSize = function (id) { native function tbWindowGetPixelSize(id); return this.__convertResult(tbWindowGetPixelSize(id)); } // v3
  __traybrowser.WindowSetPixelSize = function (id, width, height) { native function tbWindowSetPixelSize(width, height, id); return this.__convertResult(tbWindowSetPixelSize(width, height, id)); } // v3
  __traybrowser.WindowGetPixelPosition = function (id) { native function tbWindowGetPixelPosition(id); return this.__convertResult(tbWindowGetPixelPosition(id)); } // v3
  __traybrowser.WindowSetPixelPosition = function (id, x, y) { native function tbWindowSetPixelPosition(x, y, id); return this.__convertResult(tbWindowSetPixelPosition(x, y, id)); } // v3
  __traybrowser.WindowGetScaledPosition = function (id) { native function tbWindowGetScaledPosition(id); return this.__convertResult(tbWindowGetScaledPosition(id)); } // v3
  __traybrowser.WindowSetScaledPosition = function (id, x, y) { native function tbWindowSetScaledPosition(x, y, id); return this.__convertResult(tbWindowSetScaledPosition(x, y, id)); } // v3
  __traybrowser.WindowGetDesktopPosition = function (id) { native function tbWindowGetDesktopPosition(id); return this.__convertResult(tbWindowGetDesktopPosition(id)); } // v3
  __traybrowser.WindowSetDesktopPosition = function (id, x, y) { native function tbWindowSetDesktopPosition(x, y, id); return this.__convertResult(tbWindowSetDesktopPosition(x, y, id)); } // v3
  __traybrowser.WindowGetWindowInfo = function (id) { native function tbWindowGetWindowInfo(id); return this.__convertResult(tbWindowGetWindowInfo(id)); } // v3
  __traybrowser.WindowMinimize = function (id) { native function tbWindowMinimize(id); return this.__convertResult(tbWindowMinimize(id)); } // v3
  __traybrowser.WindowMaximize = function (id) { native function tbWindowMaximize(id); return this.__convertResult(tbWindowMaximize(id)); } // v3
  __traybrowser.WindowRestore = function (id) { native function tbWindowRestore(id); return this.__convertResult(tbWindowRestore(id)); } // v3
  __traybrowser.WindowGetWindowState = function (id) { native function tbWindowGetWindowState(); return this.__convertResult(tbWindowGetWindowState()); } // v3
  __traybrowser.WindowSetCustomDragRegion = function (id, x, y, width, height) { native function tbWindowSetCustomDragRegion(x, y, width, height, id); return this.__convertResult(tbWindowSetCustomDragRegion(x, y, width, height, id)); }
  __traybrowser.WindowStart = function (id) { native function tbWindowStart(id); return this.__convertResult(tbWindowStart(id)); } // v3
  __traybrowser.WindowNavigate = function (id, url) { native function tbWindowNavigate(url, id); return this.__convertResult(tbWindowNavigate(url, id)); } // v3
  __traybrowser.WindowRestartBrowser = function (id, url) { native function tbWindowRestartBrowser(url, id); return this.__convertResult(tbWindowRestartBrowser(url, id)); }
  __traybrowser.WindowSetTitle = function (id, text) { native function tbWindowSetTitle(text, id); return this.__convertResult(tbWindowSetTitle(text, id)); } // v3
  __traybrowser.WindowSetCaption = function (id, text) { return this.WindowSetTitle(id, text); } // v3
  __traybrowser.WindowGetWindowSettings = function (id) { native function tbWindowGetWindowSettings(id); return this.__convertResult(tbWindowGetWindowSettings(id)); } // v3
  __traybrowser.WindowSetWindowSettings = function (id, settings, fromDefaults) { native function tbWindowSetWindowSettings(settings, fromDefaults, id); return this.__convertResult(tbWindowSetWindowSettings(settings, fromDefaults, id)); } // v3

  // event handling
  __traybrowser.__eventHandlers = { // event handler container
    onshow: null,
    onhide: null,
    onsizechange: null,
    onpositionchange: null,
    ontrayiconclick: null,
    onballoonclick: null,
    onballoonhide: null,
    onclosebutton: null,
    onminimizebutton: null,
    onmaximizebutton: null,
    onrestored: null,
    onminimized: null,
    onmaximized: null,
    onexit: null,
    onclicommand: null,
    onwindowcreated: null,
    onwindowclosed: null,
    onrestarted: null,
    onmessage: null,
    __callback: null
  };

  __traybrowser.RegisterEvent = function (name, callback) { // v1,v2,v3
    native function tbRegisterEvent(name);
    var lName = name.toLowerCase();
    if ((typeof this.__eventHandlers[lName]).toLowerCase() != 'undefined') {
      this.__eventHandlers[lName] = callback;
      if (lName != '__callback') tbRegisterEvent(lName); // internal callback event comes earlier than contexts
    }
  }

  __traybrowser.ClearEvent = function (name) { // v1,v2,v3
    native function tbClearEvent(name);
    var lName = name.toLowerCase();
    if ((typeof this.__eventHandlers[lName]).toLowerCase() != 'undefined') {
      this.__eventHandlers[lName] = null;
      if (lName != '__callback') tbClearEvent(lName);
    }
  }

  // debugging
  __traybrowser.DebugLog = function (s) { native function tbDebugLog(s); return this.__convertResult(tbDebugLog(s)); } // v2,v3

  // legacy API
  __traybrowser.EXEC_OLD_GET_RETURN_CODE = 4; // compatibility API only, not used in ExecAsync
  __traybrowser.EXEC_OLD_NOWAIT = 8; // compatibility API only, not used in ExecAsync
  __traybrowser.EXEC_INTERNAL_SYNC = 256; // this one is internal and cannot be used by JS application

  __traybrowser.SetSize = function (width, height) { native function tbSetSize(width, height); return this.__convertResult(tbSetSize(width, height)); } // v1,v2
  __traybrowser.GetWidth = function () { native function tbGetWidth(); return this.__convertResult(tbGetWidth()); } // v1,v2
  __traybrowser.GetHeight = function () { native function tbGetHeight(); return this.__convertResult(tbGetHeight()); } // v1,v2
  __traybrowser.SetPosition = function (x, y) { native function tbSetPosition(x, y); return this.__convertResult(tbSetPosition(x, y)); } // v1,v2
  __traybrowser.GetLeft = function () { native function tbGetLeft(); return this.__convertResult(tbGetLeft()); } // v1,v2
  __traybrowser.GetTop = function () { native function tbGetTop(); return this.__convertResult(tbGetTop()); } // v1,v2
  __traybrowser.GetScreenWidth = function () { native function tbGetScreenWidth(); return this.__convertResult(tbGetScreenWidth()); } // v1,v2
  __traybrowser.GetScreenHeight = function () { native function tbGetScreenHeight(); return this.__convertResult(tbGetScreenHeight()); } // v1,v2

  __traybrowser.ReadINISetting = function (name, defValue) { return this.ReadPersistentValue(name, defValue); } // v1,v2
  __traybrowser.WriteINISetting = function (name, value) { return this.WritePersistentValue(name, value); } // v1,v2
  __traybrowser.RemoveINISetting = function (name) { return this.RemovePersistentValue(name); } // v1,v2
  __traybrowser.ReadIniSetting = function (name, defValue) { return this.ReadPersistentValue(name, defValue); } // v1,v2
  __traybrowser.WriteIniSetting = function (name, value) { return this.WritePersistentValue(name, value); } // v1,v2
  __traybrowser.RemoveIniSetting = function (name) { return this.RemovePersistentValue(name); } // v1,v2

  __traybrowser.GetApplicationPath = function () { native function tbGetApplicationPath(); return this.__convertResult(tbGetApplicationPath()); } // v1,v2
  __traybrowser.GetRuntimePath = function () { native function tbGetRuntimePath(); return this.__convertResult(tbGetRuntimePath()); } // v1,v2
  __traybrowser.GetSettingsPath = function () { native function tbGetSettingsPath(); return this.__convertResult(tbGetSettingsPath()); } // v1,v2

  __traybrowser.Exec = function (command, mode, timeout) { // v1
    // synchronous execution emulation is tricky: we need to spawn asynchronous process and then explicitly wait for its completion
    // yes, this one exists solely for compatibility, the very proper 3.0 process spawning method is ExecAsync
    native function tbExec(id, command, parameters, mode, timeout);
    native function tbExecSyncCheck(id, withCode); // this one also sleeps 20ms if the process is not complete
    if (((typeof mode).toLowerCase() == 'undefined') || ((typeof mode).toLowerCase() == 'null')) mode = 0; // default to hidden
    var noWait = 0;
    if (mode & this.EXEC_OLD_NOWAIT) noWait = this.EXEC_NOWAIT;
    if (((typeof timeout).toLowerCase() == 'undefined') || ((typeof timeout).toLowerCase() == 'null')) timeout = 1000; // default to 1000ms
    var id = this.__convertResult(tbExec(-1, command, null, (mode & this.EXEC_INTERNAL_WINDOW_MASK) | noWait | this.EXEC_INTERNAL_SYNC, timeout));
    if ((typeof id).toLowerCase() != 'string') {
      // process GUID not retrieved, means execute failed
      if (mode & this.EXEC_OLD_GET_RETURN_CODE) return 'ERR'; // ERR code and no output in case we want code back
      return ''; // otherwise just empty string
    }
    if (mode & this.EXEC_OLD_NOWAIT) {
      // we do not need to wait
      if (mode & this.EXEC_OLD_GET_RETURN_CODE) return '0'; // zero code and no output in case we want code back
      return ''; // otherwise just empty string
    }
    // wait until process completes
    while ((res = tbExecSyncCheck(id, (mode & this.EXEC_OLD_GET_RETURN_CODE) != 0)) === true);
    if ((typeof res).toLowerCase() != 'string') {
      if (mode & this.EXEC_OLD_GET_RETURN_CODE) return 'ERR'; // ERR code and no output in case we want code back
      return ''; // otherwise just empty string
    }
    return res;
  }

  __traybrowser.Ping = function (host) { return this.Exec('%PING% "' + host + '"', 0, 15000); } // v1
  __traybrowser.Traceroute = function (host) { return this.Exec('%TRACEROUTE% "' + host + '"', 0, 60000); } // v1
  __traybrowser.GetSystemInfo = function () { return this.Exec('%SYSINFO%', 0, 10000); } // v1
  __traybrowser.GetIPInfo = function () { return this.Exec('%IPINFO%', 0, 10000); } // v1
  __traybrowser.GetRoutingTable = function () { return this.Exec('%ROUTEINFO%', 0, 10000); } // v1

  // API helpers (not intended to be called from user JS code)

  // process execution
  __traybrowser.EXEC_INTERNAL_WINDOW_MASK = 3; // mode mask for window mode
  __traybrowser.EXEC_INTERNAL_ASYNC_MASK = 255; // mode mask for ExecAsync

  // JSON conversion
  __traybrowser.__convertResult = function (s) {
    if (((typeof s).toLowerCase() == 'object') && ((typeof s['json']).toLowerCase() != 'undefined')) {
      return JSON.parse(s['json']);
    } else {
      return s;
    }
  }

  // event handlers
  __traybrowser.__callEvent = function (e, ...args) {
    e = e.toLowerCase();
    switch ((typeof this.__eventHandlers[e]).toLowerCase()) {
      case 'function': this.__eventHandlers[e](...args); break;
      case 'string':
      if (this.__eventHandlers[e].substring(0, 11).toLowerCase() == 'javascript:') {
        // javascript injection of v1 stype
        eval(this.__eventHandlers[e].substring(11));
      } else {
        // just javascript injection, as events do not support navigation
        eval(this.__eventHandlers[e]);
      }
      break;
    }
  }

  // callbacks
  __traybrowser.__callbackID = 0; // last registered callback ID
  __traybrowser.__callbacks = { }; // callback handler container
  __traybrowser.__callCallback
  __traybrowser.__registerCallback = function (callback, oneshot) {
    this.__callbackID++;
    if (this.__callbackID > 1000000000) this.__callbackID = 1;
    this.__callbacks[this.__callbackID] = { fn: callback, del: oneshot };
    return this.__callbackID;
  }
  __traybrowser.__clearCallback = function (id) { if ((typeof this.__callbacks[id]).toLowerCase() != 'undefined') delete this.__callbacks[id]; }
  __traybrowser.__callCallback = function (id, ...args) {
    var callback;
    if ((typeof this.__callbacks[id]).toLowerCase() != 'undefined') {
      var callback = this.__callbacks[id].fn;
      if (this.__callbacks[id].del) delete this.__callbacks[id];

      switch ((typeof callback).toLowerCase()) {
        case 'function': callback(...args); break;
        case 'string':
        if (callback.substring(0, 11).toLowerCase() == 'javascript:') {
          // javascript injection
          eval(callback.substring(11));
        } else {
          // URL navigation
          this.Navigate(callback);
        }
        break;
      }
    }
  };
  __traybrowser.RegisterEvent('__callback', __traybrowser.__callCallback.bind(__traybrowser));
})();

