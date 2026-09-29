unit cu_tcpserver;


{
Copyright (C) 2019 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. 

}

{ TCP/IP Connection, based on Synapse Echo demo }

{$MODE objfpc}{$H+}

interface

uses
  u_global, blcksock, synsock, synautil, fpjson, jsonparser,
  Dialogs, LazUTF8, LazFileUtils, LCLIntf, SysUtils, Classes;

type

  TStringProc = procedure(var S: string) of object;
  TIntProc = procedure(var i: integer) of object;
  TExCmd = function(cmd: string): string of object;
  TExJSON = function(id:string; attrib,value:Tstringlist): string of object;
  TGetImage = procedure(n: string; var i: Tmemorystream) of object;

  TTCPThrd = class(TThread)
  private
    FSock: TTCPBlockSocket;
    CSock: TSocket;
    cmd: string;
    cmdresult: string;
    FHttpRequest, Fbody, FJSONRequest, FJSONid: string;
    JsonRecurseLevel: integer;
    FGetImage: TGetImage;
    FConnectTime: double;
    FTerminate: TIntProc;
    FExecuteCmd: TExCmd;
    FExecuteJSON: TExJSON;
    procedure JsonDataToStringlist(var SK,SV: TStringList; prefix:string; D : TJSONData);
  public
    id: integer;
    abort, stoping: boolean;
    remoteip, remoteport: string;
    // 2024 addition (report item 10f): last unexpected error seen by Execute,
    // so a crashed client thread is at least diagnosable.
    LastClientError: string;
    constructor Create(hsock: tSocket);
    procedure Execute; override;
    procedure SendData(str: string);
    procedure ExecuteCmd;
    procedure ProcessHttp;
    procedure ProcessPost;
    procedure ProcessJSON;
    property sock: TTCPBlockSocket read FSock;
    property ConnectTime: double read FConnectTime;
    property Terminated;
    // 2024 fix (report item 10e): this property used to be named onTerminate,
    // which silently hid the inherited TThread.OnTerminate (a TNotifyEvent).
    // Renamed so the inherited member stays available and reachable.
    property onSlotFree: TIntProc read FTerminate write FTerminate;
    property onExecuteCmd: TExCmd read FExecuteCmd write FExecuteCmd;
    property onGetImage: TGetImage read  FGetImage write FGetImage;
    property onExecuteJSON: TExJSON read FExecuteJSON write FExecuteJSON;
  end;

  TTCPDaemon = class(TThread)
  private
    Sock: TTCPBlockSocket;
    FErrorMsg: TStringProc;
    FShowSocket: TStringProc;
    FIPaddr, FIPport: string;
    FExecuteCmd: TExCmd;
    FExecuteJSON: TExJSON;
    FGetImage: TGetImage;
    procedure ShowError;
    procedure ThrdTerminate(var i: integer);
    function GetIPport: string;
    // 2024 fix (report item 4): client threads are no longer FreeOnTerminate,
    // so the daemon owns them and must release them itself.
    procedure ReleaseClient(n: integer);
    procedure ReleaseAllClients;
  public
    stoping: boolean;
    TCPThrd: array [1..Maxclient] of TTCPThrd;
    ThrdActive: array [1..Maxclient] of boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    procedure ShowSocket;
    property IPaddr: string read FIPaddr write FIPaddr;
    property IPport: string read GetIPport write FIPport;
    property onErrorMsg: TStringProc read FErrorMsg write FErrorMsg;
    property onShowSocket: TStringProc read FShowSocket write FShowSocket;
    property onExecuteCmd: TExCmd read FExecuteCmd write FExecuteCmd;
    property onExecuteJSON: TExJSON read FExecuteJSON write FExecuteJSON;
    property onGetImage: TGetImage read  FGetImage write FGetImage;
  end;

implementation

{$ifdef darwin}
uses BaseUnix;       //  to catch SIGPIPE

var
  NewSigRec, OldSigRec: SigActionRec;
  res: integer;

{$endif}
Const
  {$ifdef mswindows}
  NoSocket = High(TSocket);
  {$else}
  NoSocket = -1;
  {$endif}
  // 2024 addition (report items 10a/10b): upper bound on the body of a single
  // HTTP request. The server is reachable by any client that can open the port,
  // so an announced or streamed body must never be trusted for sizing.
  MaxRequestBody = 8 * 1024 * 1024;

constructor TTCPDaemon.Create;
var i: integer;
begin
  inherited Create(True);
  // 2024 fix (report item 4): this used to be FreeOnTerminate := True while
  // pu_main kept the object in a field and went on reading TCPDaemon.Finished,
  // .stoping and .TCPThrd[] after the thread had destroyed itself - a
  // use-after-free that the "if TCPDaemon<>nil" guards could not catch, because
  // the reference was never nil'd. The owner (Tf_main.StopServer) now waits for
  // the thread and frees it explicitly.
  FreeOnTerminate := False;
  for i:=1 to Maxclient do begin
    TCPThrd[i]:=nil;
    ThrdActive[i]:=False;
  end;
end;

destructor TTCPDaemon.Destroy;
begin
  // safety net: Execute's finally block normally does this already
  ReleaseAllClients;
  inherited Destroy;
end;

// 2024 addition (report item 4): wait for one finished client thread and free
// it, clearing the slot so no dangling pointer is ever left behind.
// Only ever called from the daemon thread itself.
procedure TTCPDaemon.ReleaseClient(n: integer);
begin
  if (n<1) or (n>Maxclient) then exit;
  if TCPThrd[n]=nil then exit;
  try
    TCPThrd[n].stoping := True;
    TCPThrd[n].Terminate;
    TCPThrd[n].WaitFor;
    TCPThrd[n].Free;
  except
    // never let a failing client teardown kill the daemon
  end;
  TCPThrd[n] := nil;
  ThrdActive[n] := False;
end;

procedure TTCPDaemon.ReleaseAllClients;
var i: integer;
begin
  // ask every client to stop first, then collect them, so the shutdown of n
  // clients costs one timeout instead of n
  for i := 1 to Maxclient do
    if TCPThrd[i]<>nil then begin
      TCPThrd[i].stoping := True;
      TCPThrd[i].Terminate;
    end;
  for i := 1 to Maxclient do
    ReleaseClient(i);
end;

procedure TTCPDaemon.ShowError;
var
  msg: string;
begin
  msg := IntToStr(sock.lasterror) + ' ' + sock.GetErrorDesc(sock.lasterror);
  if assigned(FErrorMsg) then
    FErrorMsg(msg);
end;

function TTCPDaemon.GetIPport: string;
begin
  if sock=nil then
    result:=FIPport
  else begin
    sock.GetSins;
    result := IntToStr(sock.GetLocalSinPort);
  end;
end;

procedure TTCPDaemon.ShowSocket;
var
  locport: string;
begin
  sock.GetSins;
  locport := IntToStr(sock.GetLocalSinPort);
  if assigned(FShowSocket) then
    FShowSocket(locport);
end;

procedure TTCPDaemon.ThrdTerminate(var i: integer);
begin
  if (i>0) and (i<=Maxclient) then
     ThrdActive[i] := False;
end;

procedure TTCPDaemon.Execute;
var
  ClientSock: TSocket;
  RefuseSock: TTCPBlockSocket;
  i, n: integer;
begin
  //writetrace('start tcp deamon');
  stoping := False;
  for i := 1 to Maxclient do
    ThrdActive[i] := False;
  sock := TTCPBlockSocket.Create;
  //writetrace('blocksocked created');
  try
    with sock do
    begin
      //writetrace('create socket');
      CreateSocket;
      if lasterror <> 0 then begin
        Synchronize(@ShowError);
        exit;   // 2024 fix (report item 10d): fatal, do not enter the accept loop
      end;
      MaxLineLength := 1024;
      //writetrace('setlinger');
      setLinger(True, 15000);
      if lasterror <> 0 then
        Synchronize(@ShowError);   // not fatal, keep going
      //socket timeout for accept
      SetTimeout(50);
      if lasterror <> 0 then
        Synchronize(@ShowError);   // not fatal, keep going
      //writetrace('bind to '+fipaddr+' '+fipport);
      bind(FIPaddr, FIPport);
      if (lasterror=9)and(FIPaddr='::0') then begin
        FIPaddr:='0.0.0.0';
        bind(FIPaddr, FIPport);
      end;
      // 2024 fix (report item 10d): a failed bind - typically "port already in
      // use" after a restart - used to be reported and then ignored, leaving the
      // thread spinning forever on Accept against an unbound socket.
      if lasterror <> 0 then begin
        Synchronize(@ShowError);
        exit;
      end;
      //writetrace('listen');
      listen;
      if lasterror <> 0 then begin
        Synchronize(@ShowError);
        exit;
      end;
      Synchronize(@ShowSocket);
      //writetrace('start main loop');
      repeat
        if stoping or terminated then
          break;
        ClientSock := Accept;
        if ClientSock<>NoSocket then
        begin
          if lastError = 0 then
          begin
            // 2024 fix (report item 4): look for a slot that is free or holds a
            // thread that has finished. The old test dereferenced TCPThrd[i]
            // even though the object could already have destroyed itself; now
            // the objects live until we release them here, so the test is safe.
            n := -1;
            for i := 1 to Maxclient do
              if (TCPThrd[i] = nil) or (not ThrdActive[i]) or
                (TCPThrd[i].Finished) then
              begin
                n := i;
                break;
              end;
            if n > 0 then
            begin
              // collect the previous occupant of this slot before overwriting
              // the reference - this is where the old code leaked the thread
              // object and left a dangling pointer in the array
              ReleaseClient(n);
              TCPThrd[n] := TTCPThrd.Create(ClientSock);
              TCPThrd[n].onSlotFree := @ThrdTerminate;
              TCPThrd[n].onExecuteCmd := FExecuteCmd;
              TCPThrd[n].onExecuteJSON := FExecuteJSON;
              TCPThrd[n].onGetImage := FGetImage;
              TCPThrd[n].id := n;
              ThrdActive[n] := True;
              TCPThrd[n].Start;
            end
            else
            begin
              // 2024 fix (report item 4): sending the 503 no longer needs a
              // thread at all. The old code created a TTCPThrd it never
              // started, reached into its private Fsock field and then called
              // Free on a suspended thread.
              RefuseSock := TTCPBlockSocket.Create;
              try
                RefuseSock.socket := ClientSock;
                RefuseSock.GetSins;
                RefuseSock.MaxLineLength := 1024;
                RefuseSock.SendString('HTTP/1.0 503' + CRLF);
                RefuseSock.SendString('' + CRLF);
                RefuseSock.SendString(msgFailed + ' Maximum connection reach!' + CRLF);
                RefuseSock.CloseSocket;
              finally
                RefuseSock.Free;
              end;
            end;
          end
          else if lasterror <> 0 then
            Synchronize(@ShowError);
        end;
      until False;
    end;
  finally
    //  Suspended:=true;
    // 2024 fix (report item 4): shut the clients down from the thread that owns
    // them, before the daemon itself goes away. This also replaces the loop
    // Tf_main.StopServer used to run over TCPDaemon.TCPThrd[], which touched
    // the array from the main thread with no synchronisation.
    ReleaseAllClients;
    Sock.AbortSocket;
    Sock.Free;
    //  terminate;
  end;
end;

constructor TTCPThrd.Create(Hsock: TSocket);
begin
  inherited Create(True);
  // 2024 fix (report item 4): the daemon keeps a reference to this object in
  // TCPThrd[] and must be able to inspect it after the connection ends, so the
  // thread may not destroy itself. TTCPDaemon.ReleaseClient frees it.
  FreeOnTerminate := False;
  Csock := Hsock;
  abort := False;
  id:=-1;
end;

procedure TTCPThrd.Execute;
var
  s,su,buf,hdr: string;
  cl: integer;
  bodybuf: TMemoryStream;   // 2024: bounded accumulation of a chunked body
begin
  try
    Fsock := TTCPBlockSocket.Create;
    FConnectTime := now;
    stoping := False;
    try
      Fsock.socket := CSock;
      Fsock.GetSins;
      Fsock.MaxLineLength := 1024;
      remoteip := Fsock.GetRemoteSinIP;
      remoteport := IntToStr(Fsock.GetRemoteSinPort);
      with Fsock do
      begin
        repeat
          if stoping or terminated then
            break;
          s := RecvString(500);
          if lastError = 0 then
          begin
            su:=uppercase(s);
            if (su = 'QUIT') or (su = 'EXIT') then begin
              break;
            end
            else if copy(su,1,3)='GET' then begin
              hdr:='';
              repeat
                buf:=RecvString(500);
                if trim(buf)='' then break;
                hdr:=hdr+crlf+buf;
              until LastError<>0;
              FHttpRequest:=s;
              Synchronize(@ProcessHttp);
              break;
            end
            else if copy(su,1,4)='POST' then begin
              hdr:='';
              cl:=-1;
              repeat
                buf:=RecvString(500);
                if trim(buf)='' then break;
                hdr:=hdr+crlf+buf;
                if Pos('CONTENT-LENGTH:',UpperCase(buf))=1 then begin
                  delete(buf,1,15);
                  cl:=StrToIntDef(trim(buf),0);
                end;
              until LastError<>0;
              Fbody:='';
              if cl>0 then begin
                // 2024 fix (report item 10a): the announced Content-Length used
                // to be passed straight to RecvBufferStr, so an unauthenticated
                // client could force an arbitrarily large allocation just by
                // claiming a huge body. Refuse anything over the limit.
                if cl>MaxRequestBody then begin
                  SendString('HTTP/1.0 413' + CRLF);
                  SendString('' + CRLF);
                  SendString(msgFailed + ' Request body too large!' + CRLF);
                  break;
                end;
                Fbody:=RecvBufferStr(cl,500);
              end
              else if cl=0 then begin
                // 2024 fix (report item 10b): this loop used to run until the
                // peer errored out, with no size limit and O(n^2) string
                // concatenation. Bounded now, and accumulated in a stream.
                bodybuf:=TMemoryStream.Create;
                try
                  repeat
                    buf:=RecvPacket(500);
                    if buf<>'' then
                      bodybuf.Write(buf[1],Length(buf));
                    if bodybuf.Size>MaxRequestBody then begin
                      SendString('HTTP/1.0 413' + CRLF);
                      SendString('' + CRLF);
                      SendString(msgFailed + ' Request body too large!' + CRLF);
                      break;
                    end;
                  until LastError<>0;
                  SetLength(Fbody,bodybuf.Size);
                  if bodybuf.Size>0 then begin
                    bodybuf.Position:=0;
                    bodybuf.Read(Fbody[1],bodybuf.Size);
                  end;
                finally
                  bodybuf.Free;
                end;
              end;
              FHttpRequest:=s;
              Synchronize(@ProcessPost);
              break;
            end
            else if copy(s,1,1)='{' then begin
               FJSONRequest:=s;
               Synchronize(@ProcessJSON);
               SendString(cmdresult + crlf);
               if lastError <> 0 then break;
            end
            else begin
              cmd:=su;
              Synchronize(@ExecuteCmd);
              SendString(cmdresult + crlf);
              if lastError <> 0 then break;
            end
          end
          else begin
            if LastError<>WSAETIMEDOUT then break;
          end;
        until False;
      end;
    finally
      if assigned(FTerminate) then
        FTerminate(id);
      Fsock.CloseSocket;
      Fsock.Free;
    end;
  except
    // 2024 (report item 10f): this used to be a bare "except end", so a failing
    // client thread left no trace at all. There is no logging channel usable
    // from this thread without a Synchronize, so at least record the reason
    // where the daemon can pick it up.
    on E: Exception do
      LastClientError := E.Message;
  end;
end;

procedure TTCPThrd.Senddata(str: string);
begin
  try
    if Fsock <> nil then
      with Fsock do
      begin
        if terminated then
          exit;
        SendString(UTF8ToSys(str) + CRLF);
        if LastError <> 0 then
          terminate;
      end;
  except
    terminate;
  end;
end;

procedure TTCPThrd.ExecuteCmd;
begin
  try
    if Assigned(FExecuteCmd) then
      cmdresult := FExecuteCmd(cmd);
  except
    cmdresult := msgFailed;
  end;
end;

procedure TTCPThrd.ProcessJSON;
var attrib,value:Tstringlist;
    J: TJSONData;
    p: integer;
begin
  try
    Fjsonid:='null';
    attrib:=Tstringlist.Create;
    value:=Tstringlist.Create;
    try
    J:=GetJSON(FJSONRequest);
    JsonRecurseLevel:=0;
    JsonDataToStringlist(attrib,value,'',J);
    J.Free;
    p:=attrib.IndexOf('id');
    if p>=0 then
      Fjsonid:=value[p];
    if (Fjsonid<>'null') and Assigned(FExecuteJSON) then
      cmdresult := FExecuteJSON(Fjsonid,attrib,value);
    finally
      attrib.Free;
      value.Free;
    end;
  except
    on E: Exception do cmdresult := '{"jsonrpc": "2.0", "error": {"code": -32603, "message": "Internal error:'+E.Message+'"}, "id": '+Fjsonid+'}';
  end;
end;

procedure TTCPThrd.JsonDataToStringlist(var SK,SV: TStringList; prefix:string; D : TJSONData);
var i:integer;
    pr,buf:string;
begin
inc(JsonRecurseLevel);
if Assigned(D) then begin
  case D.JSONType of
    jtArray,jtObject: begin
        for i:=0 to D.Count-1 do begin
           if D.JSONType=jtArray then begin
              if prefix='' then pr:=IntToStr(I) else pr:=prefix+'.'+IntToStr(I);
              if JsonRecurseLevel<100 then JsonDataToStringlist(SK,SV,pr,D.items[i])
                 else raise Exception.Create('JSON data recursion > 100');
           end else begin
              if prefix='' then pr:=TJSONObject(D).Names[i] else pr:=prefix+'.'+TJSONObject(D).Names[i];
              if JsonRecurseLevel<100 then JsonDataToStringlist(SK,SV,pr,D.items[i])
                 else raise Exception.Create('JSON data recursion > 100');
           end;
        end;
       end;
    jtNull: begin
       SK.Add(prefix);
       SV.Add('null');
    end;
    jtNumber: begin
       SK.Add(prefix);
       buf:=floattostr(D.AsFloat);
       SV.Add(buf);
    end
    else begin
       SK.Add(prefix);
       SV.Add(D.AsString);
    end;
 end;
end;
end;

procedure TTCPThrd.ProcessHttp;
var method, uri, protocol, Doc: string;
   i: integer;
   img: TMemoryStream;
begin
  method := fetch(FHttpRequest, ' ');
  uri := fetch(FHttpRequest, ' ');
  protocol := fetch(FHttpRequest, ' ');
  if method<>'GET' then begin
     Fsock.SendString('HTTP/1.0 405' + CRLF);
     Fsock.SendString('' + CRLF);
     Fsock.SendString('Invalid method '+method + CRLF);
  end
  else if pos('HTTP/',protocol)<0 then begin
     Fsock.SendString('HTTP/1.0 406' + CRLF);
     Fsock.SendString('' + CRLF);
     Fsock.SendString('Invalid protocol '+protocol + CRLF);
  end
  else if uri='/' then begin
    Doc := FExecuteCmd('HTML_STATUS');
    Fsock.SendString('HTTP/1.0 200' + CRLF);
    Fsock.SendString('Content-type: Text/Html' + CRLF);
    Fsock.SendString('Content-length: ' + IntTostr(Length(Doc)) + CRLF);
    Fsock.SendString('Connection: close' + CRLF);
    Fsock.SendString('Date: ' + Rfc822DateTime(now) + CRLF);
    Fsock.SendString('Server: CCDciel' + CRLF);
    Fsock.SendString('' + CRLF);
    Fsock.SendString(Doc);
  end
  else if (pos('.jpg',uri)>0)and(assigned(FGetImage)) then begin
    Doc:=StringReplace(uri,'/','',[]);
    i:=pos('.jpg',Doc);
    Doc:=copy(Doc,1,i-1);
    img:=TMemoryStream.Create;
    FGetImage(Doc,img);
    Fsock.SendString('HTTP/1.0 200' + CRLF);
    Fsock.SendString('Content-type: image/jpeg' + CRLF);
    Fsock.SendString('Content-length: ' + IntTostr(img.Size) + CRLF);
    Fsock.SendString('Connection: close' + CRLF);
    Fsock.SendString('Date: ' + Rfc822DateTime(now) + CRLF);
    Fsock.SendString('Server: CCDciel' + CRLF);
    Fsock.SendString('' + CRLF);
    img.Position:=0;
    Fsock.SendStreamRaw(img);
    img.free;
  end
  else begin
    Fsock.SendString('HTTP/1.0 404' + CRLF);
    Fsock.SendString('' + CRLF);
    Fsock.SendString('Not Found' + CRLF);
  end;
end;

procedure TTCPThrd.ProcessPost;
var method, uri, protocol,Doc: string;
begin
  method := fetch(FHttpRequest, ' ');
  uri := fetch(FHttpRequest, ' ');
  protocol := fetch(FHttpRequest, ' ');
  if uri='/jsonrpc' then begin
    if trim(Fbody)='' then begin
      Fsock.SendString('HTTP/1.0 405' + CRLF);
      Fsock.SendString('' + CRLF);
      Fsock.SendString('Invalid request' + CRLF);
    end;
    FJSONRequest:=trim(Fbody);
    ProcessJSON;
    Doc:=cmdresult;
    Fsock.SendString('HTTP/1.0 200' + CRLF);
    Fsock.SendString('Content-type: application/json' + CRLF);
    Fsock.SendString('Content-length: ' + IntTostr(Length(Doc)) + CRLF);
    Fsock.SendString('Connection: close' + CRLF);
    Fsock.SendString('Date: ' + Rfc822DateTime(now) + CRLF);
    Fsock.SendString('Server: CCDciel' + CRLF);
    Fsock.SendString('' + CRLF);
    Fsock.SendString(Doc);
  end
  else begin
    Fsock.SendString('HTTP/1.0 404' + CRLF);
    Fsock.SendString('' + CRLF);
    Fsock.SendString('Not Found' + CRLF);
  end;
end;

initialization

 {$ifdef darwin}//  ignore SIGPIPE
 {$ifdef CPU32}
  with NewSigRec do
  begin
    integer(Sa_Handler) := SIG_IGN; // ignore signal
    Sa_Mask[0] := 0;
    Sa_Flags := 0;
  end;
  res := fpsigaction(SIGPIPE, @NewSigRec, @OldSigRec);
 {$endif}
 {$endif}
end.
