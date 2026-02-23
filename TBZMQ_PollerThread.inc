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

// main process ZMQ poller thread for ROUTER socket, included from TBZMQ unit

constructor TTrayBrowserZMQPollerThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
begin
  inherited;
  TrayBrowserZMQ.CreateRouterSocket;
end;

procedure TTrayBrowserZMQPollerThread.ProcessMessage;
var
  LWindow: TTrayBrowserWindow;
  LConfig: TTBStringDict;
  LParameter: TTBStringPair;
  i: Integer;
begin
  case TBCommand of
    // locally processed messages
    'INIT', 'HELLO': TBReply.Add('RESULT', 'HELLO'); // renderer subprocess and window hello messages

    'GETAPPCONFIG':
    begin
      // renderer subprocess application configuration retrieval message
      // we actually supply all available application configuration parameters here, but only part of them is used by subprocess
      LConfig := TTBStringDict.Create;
      try
        TrayBrowserApplication.TempSettings := TrayBrowserApplication.Settings;
        TrayBrowserApplication.PrintSettings(LConfig, [tbsefApplication], True);
        TBReply.Add('COUNT', IntToStr(LConfig.Count));
        i := 1;
        for LParameter in LConfig do
        begin
          TBReply.Add('CONFIG[' + IntToStr(i) + '].KEY', LParameter.Key);
          TBReply.Add('CONFIG[' + IntToStr(i) + '].VALUE', LParameter.Value);
          Inc(i);
        end;
      except end;
      FreeAndNil(LConfig);
    end;

    'CLICOMMAND':
    begin
      // an exceptional case where CLI command event needs to be sent to the main window JS handler
      LWindow := TTrayBrowserWindow(TrayBrowserRootWindow.GetBrowserWindow(TrayBrowserRootWindow.MainWindowID));
      if Assigned(LWindow) then
      begin
        // we found our window, allow window to process messages
        try
          LWindow.ZMQMessage(TBCommand, TBRequest, TBReply);
        except
          // somehow processing message resulted in an exception
          TBReply.Add('ERROR', 'INTERNAL_WINDOW_ERROR');
        end;
      end else TBReply.Add('ERROR', 'NO_WINDOW');
    end

    else
    begin
      // otherwise message may be addressed to one of our windows
      if TBRequest.ContainsKey('WINDOW') then
      begin
        LWindow := TTrayBrowserWindow(TrayBrowserRootWindow.GetBrowserWindow(TBRequest['WINDOW']));
        if Assigned(LWindow) then
        begin
          // we found our window, allow window to process messages
          try
            LWindow.ZMQMessage(TBCommand, TBRequest, TBReply);
          except
            // somehow processing message resulted in an exception
            TBReply.Add('ERROR', 'INTERNAL_WINDOW_ERROR');
          end;
        end else TBReply.Add('ERROR', 'NO_WINDOW'); // failed to get browser window
      end else TBReply.Add('ERROR', 'INVALID_COMMAND'); // if no, this is definitely an error
    end;
  end;
end;

procedure TTrayBrowserZMQPollerThread.Execute;
var
  LElement: TTBKVMessagePair;
  LMessage, LResponse: TStringList;
  LHexID: String = '';
  i: Integer;
  t, tt: QWord;
begin
  try
    LMessage := TStringList.Create;
    LResponse := TStringList.Create;
    TBRequest := TTBKVMessage.Create;
    TBReply := TTBKVMessage.Create;

    {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ poller thread started');{$ENDIF}

    while not (Terminated or Application.Terminated) do
    begin
      // clean up everything before new request is coming
      LMessage.Clear;
      LResponse.Clear;
      TBRequest.Clear;
      TBReply.Clear;

      // poll until ZMQ ROUTER socket has something
      {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ r/poll start');{$ENDIF}
      t := GetTickCount64;
      while not (Terminated or Application.Terminated) do
      begin
        if TrayBrowserZMQ.Poll(TrayBrowserZMQ.ZMQRouterSocket, ZMQ_POLLIN) then Break; // success
        Yield; // relinquish some time to idle
        tt := GetTickCount64;
        if (t > tt) then t := tt; // handle overflows, this prolongs timeout, but okay, that is rare
        if (tt - t) > TrayBrowserZMQ.ZMQPollTimeout then
        begin
          // timed out, this is normal as we may just have no messages for us
          t := GetTickCount64;
          Continue;
        end;
      end;
      if Terminated or Application.Terminated then Break; // well, here we can already be in terminated state
      {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ r/poll done');{$ENDIF}

      // receive ZMQ message (ROUTER connection ID, msgid, IPC key, cmd, fields count, fields)
      LMessage := TrayBrowserZMQ.ReceiveMultipart(TrayBrowserZMQ.ZMQRouterSocket);
      if Assigned(LMessage) then
      begin
        if LMessage.Count > 0 then
        begin
          // store hex representation of message ID for logging
          SetLength(LHexID, Length(LMessage[0]) * 2);
          BinToHex(PAnsiChar(LMessage[0]), PAnsiChar(LHexID), Length(LMessage[0]));
        end else LHexID := '<UNKNOWN>'; // should not happen, unknown sender

        if LMessage.Count >= 5 then
        begin
          // check IPC key, with an exceptional case for CLI command handling (second instance does not know IPC key, this pseudo key matches what is set in TBTrayBrowser.pas ConfigureMainProcess function)
          if (LMessage[2] = TrayBrowserApplication.IPCKey) or ((LMessage[3] = 'CLICOMMAND') and (LMessage[2] = '{CLICOMMAND}'))
          then
          begin
            // check arguments length
            try
              i := StrToInt(LMessage[4]);
            except
              i := -1; // this will always fail on the next check
              if LResponse.Count = 0 then LResponse.Add('ERROR INVALID_SIZE_FIELD'); // we deliberately send one-liner errors to make receiver err on runt message and not desynchronize
              {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ request error: unparseable argument count: ' + LMessage[4], CEF_LOG_SEVERITY_ERROR);{$ENDIF}
            end;

            // check message size with arguments
            if LMessage.Count = ((i*2) + 5) then
            begin
              // load arguments
              try
                i := 5;
                while i < LMessage.Count do
                begin
                  TBRequest.Add(LMessage[i], LMessage[i + 1]);
                  i := i + 2;
                end;

                {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ request received: [' + LHexID + '] [' + LMessage[1] + '] [' + LMessage[3] + '] ' + TrayBrowserApplication.LogKVMessage(TBRequest));{$ENDIF}

                // process message
                try
                  TBCommand := LMessage[3];
                  Synchronize(@ProcessMessage);
                except
                  // ProcessMessage should not cause any exceptions, if it did, we return corresponding error
                  TBReply.Add('ERROR', 'INTERNAL_ERROR');
                  {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ request processing error: internal error', CEF_LOG_SEVERITY_ERROR);{$ENDIF}
                end;

                // build reply message
                LResponse.Add(LMessage[1]); // message ID
                LResponse.Add(LMessage[2]); // IPC key
                LResponse.Add(LMessage[3]); // command
                LResponse.Add(IntToStr(TBReply.Count));
                for LElement in TBReply do
                begin
                  LResponse.Add(LElement.Key);
                  LResponse.Add(LElement.Value);
                end;
              except
                // this is tricky, our caller definitely expects response and we theoretically may send normal error, but exceptions here are rare and can be caused by i.e. request/reply processing, so we send one-liner there
                LResponse.Clear;
                LResponse.Add('ERROR INTERNAL_ERROR'); // we deliberately send one-liner errors to make receiver err on runt message and not desynchronize
                {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ request error: internal error', CEF_LOG_SEVERITY_ERROR);{$ENDIF}
              end;
            end else
            begin
              if LResponse.Count = 0 then
              begin
                LResponse.Add('ERROR INVALID_MESSAGE_SIZE'); // we deliberately send one-liner errors to make receiver err on runt message and not desynchronize
                {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ request error: invalid message size: expected ' + IntToStr((i * 2) + 4) + ' got ' + IntToStr(LMessage.Count - 1), CEF_LOG_SEVERITY_ERROR);{$ENDIF}
              end;
            end;
          end else
          begin
            if LResponse.Count = 0 then LResponse.Add('ERROR INVALID_IPC_KEY'); // we deliberately send one-liner errors to make receiver err on runt message and not desynchronize
            {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ request error: invalid IPC key', CEF_LOG_SEVERITY_ERROR);{$ENDIF}
          end;
        end else
        begin
          if LResponse.Count = 0 then LResponse.Add('ERROR INVALID_MESSAGE_SIZE'); // we deliberately send one-liner errors to make receiver err on runt message and not desynchronize
          {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ request error: runt message', CEF_LOG_SEVERITY_ERROR);{$ENDIF}
        end;

        if LResponse.Count = 0 then
        begin
          // should not happen, but we fill in just in case
          LResponse.Add('ERROR INTERNAL_ERROR');
          {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ request processing error: somehow the reply is empty', CEF_LOG_SEVERITY_ERROR);{$ENDIF}
        end;

        // poll until ZMQ ROUTER socket allows to send something
        {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ w/poll start');{$ENDIF}
        t := GetTickCount64;
        while not (Terminated or Application.Terminated) do
        begin
          if TrayBrowserZMQ.Poll(TrayBrowserZMQ.ZMQRouterSocket, ZMQ_POLLOUT) then Break; // success
          Yield; // relinquish some time to idle
          tt := GetTickCount64;
          if (t > tt) then t := tt; // handle overflows, this prolongs timeout, but okay, that is rare
          if (tt - t) > TrayBrowserZMQ.ZMQPollTimeout then TrayBrowserApplication.Die('ZMQ w/poll timeout', TB_HALT_ZMQ_PROTOCOL_ERROR); // timed out, this potentially leads to protocol desynchronization so we halt here
        end;
        if Terminated or Application.Terminated then Break;
        {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ w/poll done');{$ENDIF}

        // send the reply
        LResponse.Insert(0, LMessage[0]); // prepend ROUTER connection ID
        if TrayBrowserZMQ.SendMultipart(TrayBrowserZMQ.ZMQRouterSocket, LResponse) then
        begin
          if LResponse.Count >= 5 then // on fatal receive errors, we never send normal sized messages
          begin
            {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ reply sent: [' + LHexID + '] [' + LMessage[1] + '] [' + LMessage[3] + '] ' + TrayBrowserApplication.LogKVMessage(TBReply));{$ENDIF}
          end else
          begin
            LResponse.Delete(0);
            {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ error reply sent: [' + LHexID + ']' + TrayBrowserApplication.LogValues(LResponse));{$ENDIF}
          end;
        end else TrayBrowserApplication.Die('ZMQ failed to send reply message', TB_HALT_ZMQ_PROTOCOL_ERROR); // failed to send reply message, this potentially leads to protocol desynchronization so we halt here
      end else
      begin
        {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ request processing error: failed to receive message', CEF_LOG_SEVERITY_ERROR);{$ENDIF}
      end;
    end;
    {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ poller thread exited');{$ENDIF}
  except
    // um, our listener thread must never terminate with exception while we are functioning, we are as good as dead
    TrayBrowserApplication.Die('ZMQ poller thread died', TB_HALT_ZMQ_POLLER_THREAD_DIED);
  end;
end;

