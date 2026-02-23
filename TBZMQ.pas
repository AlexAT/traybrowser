unit TBZMQ;

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

// ZMQ operations

uses
  Forms, Classes, SysUtils, syncobjs, ctypes,
  uCEFConstants,
  zmq,
  TBTrayBrowser;

const
  TB_ZMQ_POLL_TIMEOUT = 10000; // more of a safety margin, should not be set too low
  TB_ZMQ_QUICK_POLL_TIMEOUT = 1500; // fast path (i.e. CLI command) timeout to not make user wait for eternity, also do not set too low
  TB_ZMQ_ERR_EAGAIN = 11;
  TB_ZMQ_ERR_EINTR = 4;
  TB_ZMQ_ERR_EFSM = 156384763;
  TB_ZMQ_MAX_PARTS = 1024;

type
  TTrayBrowserZMQ = class
    constructor Create;
    destructor Destroy; override;

    protected
      TBMessageId: Integer;
      ZMQCommandCS: TCriticalSection;

    public
      ZMQContext: Pointer;
      ZMQRouterSocket: Pointer; // used by main process
      ZMQDealerSocket: Pointer; // used by subprocesses (well, also in single process mode)
      ZMQPollTimeout: Integer; // can be adjusted by i.e. command line sending fast path

      procedure CreateRouterSocket;
      procedure CreateDealerSocket;
      function Poll(const inSocket: Pointer; const inMode: cshort): Boolean;
      function SendMessage(const inSocket: Pointer; const inMessage: String; const inIsLast: Boolean): Integer;
      function ReceiveMessage(const inSocket: Pointer; out outMessage: String; out outIsLast: Boolean): Integer;
      function SendMultipart(const inSocket: Pointer; const inParts: TStringList): Boolean;
      function ReceiveMultipart(const inSocket: Pointer): TStringList;

      // renderer process DEALER socket commands handling
      procedure GenerateMessageId;
      function Command(const inSocket: Pointer; const inPIDText: String; const inCommand: String; const inMessage: TTBKVMessage): TTBKVMessage;
  end;

  // poller thread for main process ROUTER socket
  TTrayBrowserZMQPollerThread = class(TThread)
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
    procedure Execute; override;

    protected
      TBCommand: String;
      TBRequest, TBReply: TTBKVMessage;

      procedure ProcessMessage;
  end;

var
  TrayBrowserZMQ: TTrayBrowserZMQ;

implementation

uses
  TBRootWindow, TBWindow;

{$INCLUDE TBZMQ_PollerThread.inc}

constructor TTrayBrowserZMQ.Create;
begin
  inherited;

  if not Assigned(TrayBrowserApplication) then TrayBrowserApplication.Die('TrayBrowserZMQ class cannot be created before TrayBrowserApplication class');
  if Assigned(TrayBrowserZMQ) then TrayBrowserApplication.Die('TrayBrowserZMQ class cannot be created more than once');
  TrayBrowserZMQ := Self;

  TBMessageId := Random($3FFFFFFF);
  ZMQCommandCS := TCriticalSection.Create;

  ZMQContext := zmq_ctx_new();
  if not Assigned(ZMQContext) then TrayBrowserApplication.Die('Failed to initialize ZeroMQ context (' + IntToStr(zmq_errno()) + ')');

  ZMQRouterSocket := nil;
  ZMQDealerSocket := nil;
  ZMQPollTimeout := TB_ZMQ_POLL_TIMEOUT;
end;

destructor TTrayBrowserZMQ.Destroy;
begin
  if Assigned(ZMQDealerSocket) then
  begin
    zmq_close(ZMQDealerSocket);
    ZMQDealerSocket := nil;
  end;

  if Assigned(ZMQRouterSocket) then
  begin
    zmq_close(ZMQRouterSocket);
    ZMQRouterSocket := nil;
  end;

  zmq_ctx_destroy(ZMQContext);
  ZMQContext := nil;

  FreeAndNil(ZMQCommandCS);
  inherited;
end;

procedure TTrayBrowserZMQ.CreateRouterSocket;
var
  rc: Integer;
begin
  ZMQRouterSocket := zmq_socket(ZMQContext, ZMQ_ROUTER);
  if not Assigned(ZMQRouterSocket) then TrayBrowserApplication.Die('Failed to create ZeroMQ ROUTER socket (' + IntToStr(zmq_errno()) + ')');
  rc := zmq_bind(ZMQRouterSocket, PChar('ipc://' + TrayBrowserApplication.IPCPath));
  if rc <> 0 then TrayBrowserApplication.Die('Failed to initialize ZeroMQ ROUTER socket (' + IntToStr(zmq_errno()) + ')');
end;

procedure TTrayBrowserZMQ.CreateDealerSocket;
var
  rc: Integer;
begin
  ZMQDealerSocket := zmq_socket(ZMQContext, ZMQ_DEALER);
  if not Assigned(ZMQDealerSocket) then TrayBrowserApplication.Die('Failed to create ZeroMQ DEALER socket (' + IntToStr(zmq_errno()) + ')');
  rc := zmq_connect(ZMQDealerSocket, PChar('ipc://' + TrayBrowserApplication.IPCPath));
  if rc <> 0 then TrayBrowserApplication.Die('Failed to initialize ZeroMQ DEALER socket (' + IntToStr(zmq_errno()) + ')');
  GenerateMessageId; // generate initial ZMQ message ID on startup
end;

// low level ZMQ message communication

function TTrayBrowserZMQ.Poll(const inSocket: Pointer; const inMode: cshort): Boolean;
var
  LPollItems: array of zmq_pollitem_t = ();
begin
  Result := False;
  SetLength(LPollItems, 1);
  LPollItems[0].socket := inSocket;
  LPollItems[0].events := inMode;
  zmq_poll(@LPollItems[0], 1, 100);
  if (LPollItems[0].revents and inMode) = inMode then Result := True;
end;

function TTrayBrowserZMQ.SendMessage(const inSocket: Pointer; const inMessage: String; const inIsLast: Boolean): Integer;
var
  LRetry: Boolean;
  msg_zmq: zmq_msg_t;
begin
  Result := 0;

  zmq_msg_init_size(@msg_zmq, Length(inMessage));
  move(PChar(inMessage)^, zmq_msg_data(@msg_zmq)^, Length(inMessage));
  repeat
    LRetry := False;
    if inIsLast then
      Result := zmq_msg_send(@msg_zmq, inSocket, 0)
    else
      Result := zmq_msg_send(@msg_zmq, inSocket, ZMQ_SNDMORE);
    if Result <> 0 then
    begin
      Result := zmq_errno();
      if (Result <> TB_ZMQ_ERR_EAGAIN) then
      begin
        // some weird shit happens with EAGAIN, it is just equivalent to no error on blocking sockets under Windows
        {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ send error: ' + IntToStr(Result) + ' ' + zmq_strerror(Result), CEF_LOG_SEVERITY_ERROR);{$ENDIF}
      end;
      if Result = TB_ZMQ_ERR_EINTR then
      begin
        LRetry := True;
        Sleep(1); // allow minor timeslice to prevent CPU hog
      end;
    end;
  until not LRetry;
  zmq_msg_close(@msg_zmq);

  if Result = TB_ZMQ_ERR_EAGAIN then Result := 0; // some weird shit happens with EAGAIN, it is just equivalent to no error on blocking sockets under Windows
end;

function TTrayBrowserZMQ.ReceiveMessage(const inSocket: Pointer; out outMessage: String; out outIsLast: Boolean): Integer;
var
  LSize: Integer;
  LRetry: Boolean;
  msg_zmq: zmq_msg_t;
begin
  outMessage := '';
  zmq_msg_init(@msg_zmq);
  repeat
    LRetry := False;
    LSize := zmq_msg_recv(@msg_zmq, inSocket, 0);
    if LSize >= 0 then
    begin
      Result := 0;
      if (LSize > 0) then
      begin
        SetLength(outMessage, zmq_msg_size(@msg_zmq));
        Move(zmq_msg_data(@msg_zmq)^, PChar(outMessage)^, Length(outMessage));
      end else outMessage := '';
      outIsLast := (zmq_msg_more(@msg_zmq) = 0);
    end else
    begin
      Result := zmq_errno();
      {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ receive error: ' + IntToStr(Result) + ' ' + zmq_strerror(Result), CEF_LOG_SEVERITY_ERROR);{$ENDIF}
      if Result = TB_ZMQ_ERR_EINTR then
      begin
        LRetry := True;
        Sleep(1); // allow minor timeslice to prevent CPU hog
      end;
    end;
  until not LRetry;
  zmq_msg_close(@msg_zmq);
end;

function TTrayBrowserZMQ.SendMultipart(const inSocket: Pointer; const inParts: TStringList): Boolean;
var
  i, l, rc: Integer;
begin
  Result := True;

  // try sending request as multipart ZMQ message until we can, honoring EINTR
  l := inParts.Count - 1;
  for i := 0 to l do
  begin
    rc := SendMessage(inSocket, inParts[i], i = l);
    if rc <> 0 then
    begin
      if i = l then
      begin
        // we somehow failed to send final part of ZMQ message, this is no error protocol wise
        {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ send error (data): ' + IntToStr(rc) + ' ' + zmq_strerror(rc), CEF_LOG_SEVERITY_ERROR);{$ENDIF}
        Result := False;
      end else
        TrayBrowserApplication.Die('ZMQ fatal send error (part ' + IntToStr(i) + '): ' + IntToStr(rc) + ' ' + zmq_strerror(rc), TB_HALT_ZMQ_PROTOCOL_ERROR); // we somehow failed to send part of ZMQ message, here it should not happen at all and will break protocol so we assert if we do
    end;
  end;
end;

function TTrayBrowserZMQ.ReceiveMultipart(const inSocket: Pointer): TStringList;
var
  LIsLast: Boolean = True;
  s: String = '';
  rc: Integer;
begin
  Result := TStringList.Create;

  // try receiving request as multipart ZMQ message if we can, honoring EINTR
  repeat
    rc := ReceiveMessage(inSocket, s, LIsLast);
    if rc <> 0 then
    begin
      if Result.Count = 0 then
      begin
        // we somehow failed to receive first part of ZMQ message, this is no error protocol wise
        {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ receive error (first part):' + IntToStr(rc) + zmq_strerror(rc), CEF_LOG_SEVERITY_ERROR);{$ENDIF}
        FreeAndNil(Result);
        Exit;
      end else
        TrayBrowserApplication.Die('ZMQ fatal receive error (part ' + IntToStr(Result.Count + 1) + '): ' + IntToStr(rc) + ' ' + zmq_strerror(rc), TB_HALT_ZMQ_PROTOCOL_ERROR); // we somehow failed to receive next part of ZMQ message, here it should not happen at all and will break protocol so we assert if we do
    end;
    Result.Add(s);
  until LIsLast or (Result.Count >= TB_ZMQ_MAX_PARTS);
  if LIsLast then Exit; // all green

  // sometimes shit happens and we may have received message too long, continue receiving unless we get last part of it and return error
  {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ receive error: giant message encountered', CEF_LOG_SEVERITY_ERROR);{$ENDIF}
  FreeAndNil(Result);
  repeat
    rc := ReceiveMessage(inSocket, s, LIsLast);
    if rc <> 0 then TrayBrowserApplication.Die('ZMQ fatal receive error (giant): ' + IntToStr(rc) + ' ' + zmq_strerror(rc), TB_HALT_ZMQ_PROTOCOL_ERROR); // we somehow failed to receive next part of ZMQ message, here it should not happen at all and will break protocol so we assert if we do
  until LIsLast;
end;

// high level ZMQ command exchange towards the main process

procedure TTrayBrowserZMQ.GenerateMessageId;
var
  r: Integer;
begin
  repeat
    r := Random($3FFFFFFF);
  until Abs(TBMessageId - r) < $100000; // ensure a little offset between consecutive generations
  TBMessageId := r;
end;

function TTrayBrowserZMQ.Command(const inSocket: Pointer; const inPIDText: String; const inCommand: String; const inMessage: TTBKVMessage): TTBKVMessage;
var
  LMessageID: String;
  LMessage: TStringList;
  LElement: TTBKVMessagePair;
  LRetry: Boolean;
  i: Integer;
  t, tt: QWord;
begin
  // lock us just in case multiple threads try to do this simultaneously, ZMQ socket cannot be shared
  ZMQCommandCS.Enter;

  try
    Result := nil;

    LMessageID := inPIDText + ':' + IntToStr(TBMessageId);
    TBMessageId := TBMessageId + 1;

    {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ request: [' + LMessageID + '] [' + inCommand + '] ' + TrayBrowserApplication.LogKVMessage(inMessage));{$ENDIF}

    // poll until ZMQ socket allows to send something
    {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ w/poll start');{$ENDIF}
    t := GetTickCount64;
    while True do
    begin
      if Poll(inSocket, ZMQ_POLLOUT) then Break; // success
      Sleep(1); // relinquish some time to idle
      tt := GetTickCount64;
      if (t > tt) then t := tt; // handle overflows, this prolongs timeout, but okay, that is rare
      if (tt - t) > ZMQPollTimeout then
      begin
        // timed out
        {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ w/poll timeout', CEF_LOG_SEVERITY_ERROR);{$ENDIF}
        Exit;
      end;
    end;
    {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ w/poll done');{$ENDIF}

    // send message (msgid, IPC key, cmd, data)
    LMessage := TStringList.Create;
    try
      LMessage.Add(LMessageID);
      LMessage.Add(TrayBrowserApplication.IPCKey);
      LMessage.Add(inCommand);
      if Assigned(inMessage) then
      begin
        LMessage.Add(IntToStr(inMessage.Count));
        for LElement in inMessage do
        begin
          LMessage.Add(LElement.Key);
          LMessage.Add(LElement.Value);
        end;
      end else LMessage.Add('0'); // no message

      if not SendMultipart(inSocket, LMessage) then
      begin
        FreeAndNil(LMessage);
        Exit;
      end;
      {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ sent: [' + LMessageID + '] [' + inCommand + '] ' + TrayBrowserApplication.LogKVMessage(inMessage));{$ENDIF}
    except end;
    FreeAndNil(LMessage);

    // poll until ZMQ socket has something back for us
    {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ r/poll start');{$ENDIF}
    t := GetTickCount64;
    while True do
    begin
      if Poll(inSocket, ZMQ_POLLIN) then Break; // success
      Sleep(1); // relinquish some time to idle
      tt := GetTickCount64;
      if (t > tt) then t := tt; // handle overflows, this prolongs timeout, but okay, that is rare
      if (tt - t) > ZMQPollTimeout then
      begin
        // timed out
        {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ r/poll timeout', CEF_LOG_SEVERITY_ERROR);{$ENDIF}
        Exit;
      end;
    end;
    {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ r/poll done');{$ENDIF}

    // now going to receive the response
    repeat
      LRetry := False;

      // receive message (msgid, IPC key, cmd, data)
      LMessage := ReceiveMultipart(inSocket);
      if not Assigned(LMessage) then
      begin
        // failed to receive message
        {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ receive error (no message)', CEF_LOG_SEVERITY_ERROR);{$ENDIF}
        raise Exception.Create('Failed to receive ZMQ message'); // attempt to resynchronize
      end;

      try
        if LMessage.Count < 4 then
        begin
          // runt message
          {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ receive error (runt message)', CEF_LOG_SEVERITY_ERROR);{$ENDIF}
          FreeAndNil(LMessage);
          raise Exception.Create('ZMQ protocol desynchronized'); // attempt to resynchronize
        end;

        if LMessage[0] <> LMessageID then
        begin
          {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ receive error (wrong message id): expected ' + LMessageID + ' got ' + LMessage[0], CEF_LOG_SEVERITY_ERROR);{$ENDIF} // getting wrong message ID means protocol desynchronized
          FreeAndNil(LMessage);
          raise Exception.Create('ZMQ protocol desynchronized'); // attempt to resynchronize
        end;

        if LMessage[1] <> TrayBrowserApplication.IPCKey then
          TrayBrowserApplication.Die('ZMQ fatal receive error (wrong IPC key)', TB_HALT_ZMQ_PROTOCOL_ERROR); // wrong IPC key means we are knocking to some wrong door, thus we die

        if LMessage[2] <> inCommand then
        begin
          {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.Die('ZMQ receive error (wrong command): expected ' + inCommand + ' got ' + LMessage[2], CEF_LOG_SEVERITY_ERROR);{$ENDIF} // wrong command means protocol desynchronized
          FreeAndNil(LMessage);
          raise Exception.Create('ZMQ protocol desynchronized'); // attempt to resynchronize
        end;

        try
          i := StrToInt(LMessage[3]);
        except
          {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ receive error (unparseable arguments count): ' + LMessage[3], CEF_LOG_SEVERITY_ERROR);{$ENDIF}
          raise Exception.Create('Wrong ZMQ message arguments count');
        end;
        if LMessage.Count <> ((i * 2) + 4) then
        begin
          {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ receive error (wrong message length): expected ' + IntToStr((i * 2) + 4) + ' got ' + LMessage[3], CEF_LOG_SEVERITY_ERROR);{$ENDIF}
          raise Exception.Create('Wrong ZMQ message length');
        end;

        Result := TTBKVMessage.Create;
        try
          i := 4;
          while i < LMessage.Count do
          begin
            Result.Add(LMessage[i], LMessage[i + 1]);
            i := i + 2;
          end;
        except
          FreeAndNil(Result);
          raise Exception.Create('Failed to read ZMQ message elements');
        end;

        {$IFDEF TB_DEBUG_ZMQ}TrayBrowserApplication.DebugLog('ZMQ request result: ' + TrayBrowserApplication.LogKVMessage(Result));{$ENDIF}
      except
        on e: Exception do
        begin
          case e.Message of
            'ZMQ protocol desynchronized':
            begin
              // on protocol desynchronization, we try to read next messages until we hit our desired message
              // if we cannot find our desired message, we read everything and abort reading, because next one should be in sync
              TrayBrowserApplication.DebugLog('Browser communication protocol desynchronized, attempting to resynchronize', CEF_LOG_SEVERITY_ERROR);
              if Poll(inSocket, ZMQ_POLLIN) then LRetry := True // read next message that is waiting for us
                else TrayBrowserApplication.DebugLog('Emergency browser communication protocol resynchronization complete, but the message is lost', CEF_LOG_SEVERITY_FATAL);
            end;
          end;
        end;
      end;

      FreeAndNil(LMessage);
    until not LRetry;
  except end;

  ZMQCommandCS.Leave;
end;

end.

