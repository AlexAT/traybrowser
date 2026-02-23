unit TBRenderer;

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

(* FULL AVAILABILITY: FEEL FREE TO REUSE PARTS OF THIS FILE CODE WITH OR WITHOUT MODIFICATIONS IN YOUR APPLICATIONS WITHOUT RETAINING THE ABOVE COPYRIGHT NOTICE IF YOU WANT *)

interface

// CEF renderer process bits

uses
  Classes, SysUtils,
  uCEFConstants, uCEFInterfaces, uCEFMiscFunctions, uCEFv8Handler,
  TBV8Handler;

type
  // renderer support code single instance class, must be created in subprocesses only
  TTrayBrowserRenderer = class
    constructor Create;

    protected
      V8Handler: ICefv8Handler;

      procedure CEFOnWebkitInitialized;
      procedure CEFOnBrowserCreated(const inBrowser: ICefBrowser; const inExtraInfo: ICefDictionaryValue);
      procedure CEFOnContextCreated(const inBrowser: ICefBrowser; const inFrame: ICefFrame; const inContext: ICefv8Context);

    public
      APIEnabled: Boolean;
      IsMain: Boolean;
      WindowId: String;
  end;

// global CEF handler wrappers
procedure TrayBrowserRendererCEFOnWebkitInitialized;
procedure TrayBrowserRendererCEFOnBrowserCreated(const inBrowser: ICefBrowser; const inExtraInfo: ICefDictionaryValue);
procedure TrayBrowserRendererCEFOnContextCreated(const inBrowser: ICefBrowser; const inFrame: ICefFrame; const inContext: ICefv8Context);

var
  TrayBrowserRenderer: TTrayBrowserRenderer = nil; // single renderer support code instance

implementation

uses
  TBTrayBrowser, TBZMQ;

constructor TTrayBrowserRenderer.Create;
begin
  inherited;

  if not Assigned(TrayBrowserApplication) then TrayBrowserApplication.Die('TrayBrowserRenderer class cannot be created before TrayBrowserApplication class');
  if Assigned(TrayBrowserRenderer) then TrayBrowserApplication.Die('TrayBrowserApplication class cannot be created more than once');
  TrayBrowserRenderer := Self;
  WindowId := '';

  // create ZMQ DEALER socket and initialize renderer ZMQ processing
  TrayBrowserZMQ.CreateDealerSocket;
end;

// CEF handling

procedure TTrayBrowserRenderer.CEFOnWebkitInitialized;
var
  msg, reply: TTBKVMessage;
begin
  {$IFDEF TB_DEBUG}TrayBrowserApplication.DebugLog('Fired WebkitInitialized');{$ENDIF}

  APIEnabled := False;
  IsMain := False;

  try
    V8Handler := TTBV8Handler.Create;
    CefRegisterExtension('v8/traybrowser', UnicodeString(TrayBrowserApplication.ReadResource('TRAYBROWSER_JS')), V8Handler);
  except TrayBrowserApplication.Die('Failed to register TrayBrowser V8 handler'); end;

  // send hello to the main process
  msg := TTBKVMessage.Create;
  try
    msg.Add('PID', TrayBrowserApplication.ProcessId);
    reply := TrayBrowserZMQ.Command(TrayBrowserZMQ.ZMQDealerSocket, TrayBrowserApplication.ProcessId, 'HELLO', msg);
    if Assigned(Reply) then
    begin
      {$IFDEF TB_DEBUG}TrayBrowserApplication.DebugLog('Attached to main process [' + TrayBrowserApplication.LogKVMessage(msg) + '] => [' + TrayBrowserApplication.LogKVMessage(reply) + ']');{$ENDIF}
      FreeAndNil(reply);
    end else
    begin
      {$IFDEF TB_DEBUG_JS_CALL}TrayBrowserApplication.DebugLog('Invalid HELLO command response: no reply');{$ENDIF}
    end;
  except end;
  FreeAndNil(msg);
end;

procedure TTrayBrowserRenderer.CEFOnBrowserCreated(const inBrowser: ICefBrowser; const inExtraInfo: ICefDictionaryValue);
var
  LMessage, LReply: TTBKVMessage;
begin
  // obtain window ID passed from main process
  WindowId := String(inExtraInfo.GetString('_TBWindowID'));
  {$IFDEF TB_DEBUG}TrayBrowserApplication.DebugLog('Fired BrowserCreated [' + WindowId + ']');{$ENDIF}

  TrayBrowserZMQ.GenerateMessageId; // refresh message ID on new browser connection

  LMessage := TTBKVMessage.Create;
  try
    LMessage.Add('WINDOW', WindowId);
    LReply := TrayBrowserZMQ.Command(TrayBrowserZMQ.ZMQDealerSocket, TrayBrowserApplication.ProcessId, 'GOT_BROWSER', LMessage);
    if Assigned(LReply) then
    begin
      {$IFDEF TB_DEBUG}TrayBrowserApplication.DebugLog('Attached to browser [' + WindowId + '] [' + TrayBrowserApplication.LogKVMessage(LMessage) + '] => [' + TrayBrowserApplication.LogKVMessage(LReply) + ']');{$ENDIF}
      try if LReply.ContainsKey('IS_MAIN') then IsMain := (StrToInt(LReply['IS_MAIN']) <> 0); except end;
      try if LReply.ContainsKey('API_ENABLED') then APIEnabled := (StrToInt(LReply['API_ENABLED']) <> 0); except end;
      FreeAndNil(LReply);
    end else
    begin
      {$IFDEF TB_DEBUG_JS_CALL}TrayBrowserApplication.DebugLog('Invalid WINDOW command response: no reply');{$ENDIF}
    end;
  except end;
  FreeAndNil(LMessage);
end;

procedure TTrayBrowserRenderer.CEFOnContextCreated(const inBrowser: ICefBrowser; const inFrame: ICefFrame; const inContext: ICefv8Context);
var
  LWindow, LAPI: ICefv8Value;
begin
  {$IFDEF TB_DEBUG}TrayBrowserApplication.DebugLog('Fired ContextCreated [' + WindowId + ']');{$ENDIF}
  LWindow := inContext.GetGlobal;
  if (LWindow.IsObject) then
  begin
    LAPI := LWindow.GetValueByKey('__traybrowser');
    if LAPI.IsObject then
    begin
      {$IFDEF TB_DEBUG}TrayBrowserApplication.DebugLog('Found global internal JS API object');{$ENDIF}
      if inFrame.IsMain then
      begin
        if APIEnabled then
        begin
          {$IFDEF TB_DEBUG}TrayBrowserApplication.DebugLog('Instantiating public JS API');{$ENDIF}
          LWindow.SetValueByKey('traybrowser', LAPI, V8_PROPERTY_ATTRIBUTE_DONTDELETE);
          LWindow.SetValueByKey('trayBrowser', LAPI, V8_PROPERTY_ATTRIBUTE_DONTDELETE);
          LWindow.SetValueByKey('external', LAPI, V8_PROPERTY_ATTRIBUTE_DONTDELETE);
        end else {$IFDEF TB_DEBUG}TrayBrowserApplication.DebugLog('Public JS API is disabled for this window and will not be instantiated');{$ENDIF}
      end else {$IFDEF TB_DEBUG}TrayBrowserApplication.DebugLog('Not a main frame, no public JS API will be instantiated');{$ENDIF}
    end else {$IFDEF TB_DEBUG}TrayBrowserApplication.DebugLog('Failed to find global internal JS API object', CEF_LOG_SEVERITY_ERROR);{$ENDIF}
  end else {$IFDEF TB_DEBUG}TrayBrowserApplication.DebugLog('Somehow global window is not an object', CEF_LOG_SEVERITY_ERROR);{$ENDIF}
end;

procedure TrayBrowserRendererCEFOnWebkitInitialized;
begin
  TrayBrowserRenderer.CEFOnWebkitInitialized;
end;

procedure TrayBrowserRendererCEFOnBrowserCreated(const inBrowser: ICefBrowser; const inExtraInfo: ICefDictionaryValue);
begin
  TrayBrowserRenderer.CEFOnBrowserCreated(inBrowser, inExtraInfo);
end;

procedure TrayBrowserRendererCEFOnContextCreated(const inBrowser: ICefBrowser; const inFrame: ICefFrame; const inContext: ICefv8Context);
begin
  TrayBrowserRenderer.CEFOnContextCreated(inBrowser, inFrame, inContext);
end;

end.

