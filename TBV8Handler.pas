unit TBV8Handler;

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

// V8 Javascript handler

uses
  Classes, SysUtils,
  uCEFTypes, uCEFv8Handler, uCEFv8Value, uCEFInterfaces, uCEFConstants, uCEFv8Context,
  TBTrayBrowser, TBZMQ;

type
  TTBV8Handler = class(TCefv8HandlerOwn)
    protected
      function ArgToStr(const inValue: ICefv8Value): String;
      procedure ArgToMsg(const inMessage: TTBKVMessage; const inPrefix: String; const inIndex: Integer; const inValue: ICefv8Value);

      function TBJSCall(const inName: ustring; const inArguments: TCefv8ValueArray; var RetValue: ICefv8Value): Boolean;
      function Execute(const inName: ustring; const inObject: ICefv8Value; const inArguments: TCefv8ValueArray; var RetValue: ICefv8Value; var RetException: ustring): Boolean; override;
  end;

implementation

uses
  TBRenderer;

function TTBV8Handler.ArgToStr(const inValue: ICefv8Value): String;
begin
  Result := '';
  if inValue.IsString then
    Result := String(inValue.GetStringValue)
  else if inValue.IsInt then
    Result := IntToStr(inValue.GetIntValue)
  else if inValue.IsUInt then
    Result := UIntToStr(inValue.GetUIntValue)
  else if inValue.IsBool then
    Result := BoolToStr(inValue.GetBoolValue, '1', '0')
  else if inValue.IsDouble then
    Result := FloatToStr(inValue.GetDoubleValue, TrayBrowserApplication.FormatSettings)
  else
    raise Exception.Create('Argument cannot be converted to string');
end;

procedure TTBV8Handler.ArgToMsg(const inMessage: TTBKVMessage; const inPrefix: string; const inIndex: Integer; const inValue: ICefv8Value);
var
  LKey, LTypeKey: String;
  LKeys: TStringList;
  i: Integer;
  s: String = '';
  v: ICefv8Value;
begin
  LKey := inPrefix + '[' + IntToStr(inIndex) + ']';
  LTypeKey := LKey + '.TYPE';
  try
    if inValue.IsValid then
    begin
      if inValue.IsString then
      begin
        inMessage.Add(LKey, String(inValue.GetStringValue));
        inMessage.Add(LTypeKey, 'STRING');
      end else if inValue.IsInt then
      begin
        inMessage.Add(LKey, IntToStr(inValue.GetIntValue));
        inMessage.Add(LTypeKey, 'INT');
      end else if inValue.IsUInt then
      begin
        inMessage.Add(LKey, UIntToStr(inValue.GetUIntValue));
        inMessage.Add(LTypeKey, 'INT');
      end else if inValue.IsBool then
      begin
        inMessage.Add(LKey, BoolToStr(inValue.GetBoolValue, '1', '0'));
        inMessage.Add(LTypeKey, 'BOOL');
      end else if inValue.IsDouble then
      begin
        inMessage.Add(LKey, FloatToStr(inValue.GetDoubleValue, TrayBrowserApplication.FormatSettings));
        inMessage.Add(LTypeKey, 'FLOAT');
      end else if inValue.IsArray then
      begin
        inMessage.Add(LKey, IntToStr(inValue.GetArrayLength));
        inMessage.Add(LTypeKey, 'ARRAY');
        for i := 0 to inValue.GetArrayLength - 1 do
          ArgToMsg(inMessage, LKey, i + 1, inValue.GetValueByIndex(i));
      end else if inValue.IsObject then
      begin
        LKeys := TStringList.Create;
        try
          inValue.GetKeys(LKeys);
          inMessage.Add(LKey, IntToStr(LKeys.Count));
          inMessage.Add(LTypeKey, 'OBJECT');
          i := 1;
          for s in LKeys do
          begin
            v := inValue.GetValueByKey(UnicodeString(s));
            if v.IsValid and not v.IsFunction and not v.IsPromise then
            begin
              inMessage.Add(LKey + '[' + IntToStr(i) + '].KEY', s);
              ArgToMsg(inMessage, LKey, i, v);
              i := i + 1;
            end;
          end;
        except end;
        FreeAndNil(LKeys);
      end else if inValue.IsArrayBuffer then
      begin
        SetLength(s, inValue.GetArrayBufferByteLength);
        Move(inValue.GetArrayBufferData^, s, Length(s));
        inMessage.Add(LKey, s);
        inMessage.Add(LTypeKey, 'STRING');
      end else if inValue.IsNull then
      begin
        inMessage.Add(LKey, '');
        inMessage.Add(LTypeKey, 'NULL');
      end else if inValue.IsUndefined then
      begin
        inMessage.Add(LKey, '');
        inMessage.Add(LTypeKey, 'UNDEFINED');
      end else
      begin
        inMessage.Add(LKey, '');
        inMessage.Add(LTypeKey, 'UNSUPPORTED');
      end;
    end else
    begin
      inMessage.Add(LKey, 'INVALID');
      inMessage.Add(LTypeKey, 'INVALID');
    end;
  except
    inMessage.Add(LKey, 'EXCEPTION');
    inMessage.Add(LTypeKey, 'INVALID');
  end;
end;

function TTBV8Handler.TBJSCall(const inName: ustring; const inArguments: TCefv8ValueArray; var RetValue: ICefv8Value): Boolean;
var
  LMessage, LReply: TTBKVMessage;
  i: Integer;
begin
  // generic call case, pack all possible scalars as argument strings (!) and send, complex types are not implemented yet, booleans are packed as 0/1, the rest including null and undefined are packed as empty strings
  Result := False;
  LMessage := TTBKVMessage.Create;
  try
    LMessage.Add('WINDOW', TrayBrowserRenderer.WindowId);
    LMessage.Add('FUNCTION', String(inName));
    LMessage.Add('ARGC', IntToStr(Length(inArguments)));
    for i := 0 to Length(inArguments) - 1 do
      ArgToMsg(LMessage, 'ARGV', i + 1, inArguments[i]);

    {$IFDEF TB_DEBUG_JS_CALL}TrayBrowserApplication.DebugLog('JS call: [' + String(inName) + '] [' + TrayBrowserRenderer.WindowId + '] ' + TrayBrowserApplication.LogKVMessage(LMessage));{$ENDIF}
    LReply := TrayBrowserZMQ.Command(TrayBrowserZMQ.ZMQDealerSocket, TrayBrowserApplication.ProcessId, 'JS_CALL', LMessage);
  except end;
  FreeAndNil(LMessage);

  // result is expected to be string by default, undefined reply means undefined result, explicit result types are possible via RESULT[TYPE]
  if Assigned(LReply) then
  begin
    try
      if LReply.ContainsKey('ERROR') then
      begin
        // got some error in
        {$IFDEF TB_DEBUG_JS_CALL}TrayBrowserApplication.DebugLog('JS call resulted in error: ' + LReply['ERROR'], CEF_LOG_SEVERITY_ERROR);{$ENDIF}
      end else
      begin
        if LReply.ContainsKey('RESULT') then
        begin
          RetValue := TCefv8ValueRef.NewString(UnicodeString(LReply['RESULT']));
          Result := True;
        end else
        if LReply.ContainsKey('RESULT[STRING]') then
        begin
          RetValue := TCefv8ValueRef.NewString(UnicodeString(LReply['RESULT[STRING]']));
          Result := True;
        end else
        if LReply.ContainsKey('RESULT[INT]') then
        begin
          RetValue := TCefv8ValueRef.NewInt(StrToInt(LReply['RESULT[INT]']));
          Result := True;
        end else
        if LReply.ContainsKey('RESULT[FLOAT]') then
        begin
          RetValue := TCefv8ValueRef.NewDouble(StrToFloat(LReply['RESULT[FLOAT]'], TrayBrowserApplication.FormatSettings));
          Result := True;
        end else
        if LReply.ContainsKey('RESULT[TRUE]') then
        begin
          RetValue := TCefv8ValueRef.NewBool(True);
          Result := True;
        end else
        if LReply.ContainsKey('RESULT[FALSE]') then
        begin
          RetValue := TCefv8ValueRef.NewBool(False);
          Result := True;
        end else
        if LReply.ContainsKey('RESULT[NULL]') then
        begin
          RetValue := TCefv8ValueRef.NewNull;
          Result := True;
        end else
        if LReply.ContainsKey('RESULT[JSON]') then
        begin
          RetValue := TCefv8ValueRef.NewObject(nil, nil);
          RetValue.SetValueByKey('json', TCefv8ValueRef.NewString(UnicodeString(LReply['RESULT[JSON]'])), V8_PROPERTY_ATTRIBUTE_NONE);
          Result := True;
        end else
        if LReply.ContainsKey('RESULT[UNDEFINED]') then
        begin
          RetValue := TCefv8ValueRef.NewUndefined;
          Result := True;
        end else
        begin
          {$IFDEF TB_DEBUG_JS_CALL}TrayBrowserApplication.DebugLog('Invalid JS call response: ' + TrayBrowserApplication.LogKVMessage(LReply), CEF_LOG_SEVERITY_ERROR);{$ENDIF}
        end;
      end;
    except end;
    FreeAndNil(LReply);
  end else
  begin
    {$IFDEF TB_DEBUG_JS_CALL}TrayBrowserApplication.DebugLog('Invalid JS call response: no reply');{$ENDIF}
  end;
end;

function TTBV8Handler.Execute(const inName: ustring; const inObject: ICefv8Value; const inArguments: TCefv8ValueArray; var RetValue: ICefv8Value; var RetException: ustring): Boolean;
var
  s: String;
begin
 Result := False;

 // if current renderer has API disabled, prevent executing any API as well
 if not TrayBrowserRenderer.APIEnabled then Exit;

 // prevent executing any native JS API for iframes and other embedding types
 if not TCefv8ContextRef.Current.GetFrame.IsMain then Exit;

 try
   case inName of
     // locally handled calls

     'tbGetAPIModel':
     begin
       RetValue := TCefv8ValueRef.NewString('cef');
       Result := True;
     end;

     'tbGetAPIVersion':
     begin
       RetValue := TCefv8ValueRef.NewString(TB_API_VERSION);
       Result := True;
     end;

     'tbGetAPIOS':
     begin
       RetValue := TCefv8ValueRef.NewString('windows');
       Result := True;
     end;

     'tbGetAPIBrowser':
     begin
       RetValue := TCefv8ValueRef.NewString('chromium');
       Result := True;
     end;

     'tbGetVersion':
     begin
       RetValue := TCefv8ValueRef.NewString(TB_VERSION);
       Result := True;
     end;

     'tbGetCopyright':
     begin
       RetValue := TCefv8ValueRef.NewString(UnicodeString(TrayBrowserApplication.Settings.Copyright));
       Result := True;
     end;

     'tbExecSyncCheck':
     begin
       // a little special handling for TBExecSyncCheck: we must sleep if the response is true to make sure we do not hog all the CPU and do not do too much ZMQ requests
       Result := TBJSCall(inName, inArguments, RetValue);
       if Result and RetValue.IsBool and RetValue.GetBoolValue then Sleep(60); // ~16 calls/second, anyways our main code timer reaps processes once a 100ms
     end;

     'tbDebugLog':
     begin
       if Length(inArguments) > 0 then
       begin
         try
           s := ArgToStr(inArguments[0]);
           if Length(s) <= 4096 then
             TrayBrowserApplication.DebugLog('JS DEBUG MESSAGE: ' + s)
           else
             {$IFDEF TB_DEBUG_JS_CALL}TrayBrowserApplication.DebugLog('JS call: ' + String(inName) + ' string too long', CEF_LOG_SEVERITY_ERROR){$ENDIF};
         except end;
       end;
     end;

     // all the rest is being sent to browser process (and potentially corresponding window)
     else Result := TBJSCall(inName, inArguments, RetValue);
   end;
 except Result := False; end;
end;

end.

