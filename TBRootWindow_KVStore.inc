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

// key-value storage, included from TBRootWindow unit

procedure TTrayBrowserRootWindow.InitKVStore;
var
  LError: PChar;
begin
  try
    try
      KVDB.DatabaseName := TrayBrowserApplication.DataPath + 'kvstore.sqlite3';
      KVDB.OpenFlags := [sofCreate, sofReadWrite, sofFullMutex];
      KVDB.Open;
    except TrayBrowserApplication.Die('Failed to initialize volatile database storage'); end;

    // we really do ignore any errors here, this may bite us in the leg somewhere but these are really not showstoppers
    sqlite3_exec(KVDB.Handle, 'PRAGMA locking_mode = EXCLUSIVE', nil, nil, @LError);
    sqlite3_exec(KVDB.Handle, 'PRAGMA journal_mode = DELETE', nil, nil, @LError);
    sqlite3_exec(KVDB.Handle, 'PRAGMA auto_vacuum = FULL', nil, nil, @LError);

    // check and create general kvstore
    try
      KVQuery.SQL.Text := 'SELECT key, value FROM kvstore LIMIT 1';
      KVQuery.Open;
      KVQuery.Close;
    except
      try
        KVQuery.SQL.Text := 'DROP TABLE IF EXISTS kvstore';
        KVQuery.ExecSQL;
        KVQuery.SQL.Text := 'CREATE TABLE IF NOT EXISTS kvstore (key PRIMARY KEY, value)';
        KVQuery.ExecSQL;
        KVTransaction.Commit;
      except TrayBrowserApplication.Die('Failed to initialize volatile database storage'); end;
    end;

    // create user kvstore
    try
      KVQuery.SQL.Text := 'SELECT key, value FROM kvuser LIMIT 1';
      KVQuery.Open;
      KVQuery.Close;
    except
      try
        KVQuery.SQL.Text := 'DROP TABLE IF EXISTS kvuser';
        KVQuery.ExecSQL;
        KVQuery.SQL.Text := 'CREATE TABLE IF NOT EXISTS kvuser (key PRIMARY KEY, value)';
        KVQuery.ExecSQL;
        KVTransaction.Commit;
      except TrayBrowserApplication.Die('Failed to initialize volatile database storage'); end;
    end;

    // final verification
    KVQuery.SQL.Text := 'SELECT key, value FROM kvstore LIMIT 1';
    KVQuery.Open;
    KVQuery.Close;
    KVQuery.SQL.Text := 'SELECT key, value FROM kvuser LIMIT 1';
    KVQuery.Open;
    KVQuery.Close;

    // clear requested?
    if TrayBrowserApplication.Settings.ClearKVStore then
    begin
      try
        // yes, clear user store
        KVQuery.SQL.Text := 'DELETE FROM kvuser';
        KVQuery.ExecSQL;
        KVTransaction.Commit;
      except end;
    end;
  except TrayBrowserApplication.Die('Failed to initialize volatile database storage'); end;
end;

function TTrayBrowserRootWindow.KVStoreRead(const inTable: String; const inKey: String; out outValue: String): Boolean;
begin
  Result := False;
  try
    KVQuery.SQL.Text := 'SELECT value FROM "' + ReplaceStr(inTable, '"', '""') + '" WHERE key = :k';
    KVQuery.Params.ParamByName('k').AsString := inKey;
    KVQuery.Open;
    KVQuery.First;
    if not KVQuery.EOF then
    begin
      outValue := KVQuery.FieldByName('value').AsString;
      Result := True;
    end;
    KVQuery.Close;
  except end;
end;

function TTrayBrowserRootWindow.KVStoreWrite(const inTable: String; const inKey: String; const inValue: String; const inCommit: Boolean = True): Boolean;
begin
  Result := False;
  try
    KVQuery.SQL.Text := 'INSERT INTO "' + ReplaceStr(inTable, '"', '""') + '" (key, value) VALUES (:k,:v) ON CONFLICT DO UPDATE SET value = excluded.value';
    KVQuery.Params.ParamByName('k').AsString := inKey;
    KVQuery.Params.ParamByName('v').AsString := inValue;
    KVQuery.ExecSQL;
    if inCommit then KVTransaction.Commit;
    Result := True;
  except end;
end;

function TTrayBrowserRootWindow.KVStoreDelete(const inTable: String; const inKey: String; const inCommit: Boolean = True): Boolean;
begin
  Result := False;
  try
    KVQuery.SQL.Text := 'DELETE FROM "' + ReplaceStr(inTable, '"', '""') + '" WHERE key = :k';
    KVQuery.Params.ParamByName('k').AsString := inKey;
    KVQuery.ExecSQL;
    if KVQuery.RowsAffected > 0 then Result := True;
    if inCommit then KVTransaction.Commit;
    Result := True;
  except end;
end;

procedure TTrayBrowserRootWindow.KVStoreCommit;
begin
  try KVTransaction.Commit; except end;
end;

procedure TTrayBrowserRootWindow.KVStoreRollback;
begin
  try KVTransaction.Rollback; except end;
end;

function TTrayBrowserRootWindow.MemoryKVStoreRead(const inKey: String; out outValue: String): Boolean;
begin
  Result := False;
  try
    if MemoryKVStore.ContainsKey(inKey) then
    begin
      outValue := MemoryKVStore[inKey];
      Result := True;
    end;
  except end;
end;

function TTrayBrowserRootWindow.MemoryKVStoreWrite(const inKey: String; const inValue: String): Boolean;
begin
  Result := False;
  try
    MemoryKVStore.AddOrSetValue(inKey, inValue);
    Result := True;
  except end;
end;

function TTrayBrowserRootWindow.MemoryKVStoreDelete(const inKey: String): Boolean;
begin
  Result := False;
  try
    if MemoryKVStore.ContainsKey(inKey) then
    begin
      MemoryKVStore.Remove(inKey);
      Result := True;
    end;
  except end;
end;

