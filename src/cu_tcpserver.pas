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

{ TCP/IP Connexion, based on Synapse Echo demo }

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
    FHttpRequest, FJSONRequest, FJSONid: string;
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
    constructor Create(hsock: tSocket);
    procedure Execute; override;
    procedure SendData(str: string);
    procedure ExecuteCmd;
    procedure ProcessHttp;
    procedure ProcessJSON;
    property sock: TTCPBlockSocket read FSock;
    property ConnectTime: double read FConnectTime;
    property Terminated;
    property onTerminate: TIntProc read FTerminate write FTerminate;
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
  public
    stoping: boolean;
    TCPThrd: array [1..Maxclient] of TTCPThrd;
    ThrdActive: array [1..Maxclient] of boolean;
    constructor Create;
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

constructor TTCPDaemon.Create;
var i: integer;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  for i:=1 to Maxclient do TCPThrd[i]:=nil;
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
      if lasterror <> 0 then
        Synchronize(@ShowError);
      MaxLineLength := 1024;
      //writetrace('setlinger');
      setLinger(True, 15000);
      if lasterror <> 0 then
        Synchronize(@ShowError);
      //writetrace('bind to '+fipaddr+' '+fipport);
      bind(FIPaddr, FIPport);
      if lasterror <> 0 then
        Synchronize(@ShowError);
      //writetrace('listen');
      listen;
      if lasterror <> 0 then
        Synchronize(@ShowError);
      Synchronize(@ShowSocket);
      //writetrace('start main loop');
      repeat
        if stoping or terminated then
          break;
        if canread(500) and (not terminated) and (not stoping) then
        begin
          ClientSock := accept;
          if lastError = 0 then
          begin
            n := -1;
            for i := 1 to Maxclient do
              if (not ThrdActive[i]) or
                (TCPThrd[i] = nil) or (TCPThrd[i].Fsock = nil) or
                (TCPThrd[i].terminated) then
              begin
                n := i;
                break;
              end;
            if n > 0 then
            begin
              TCPThrd[n] := TTCPThrd.Create(ClientSock);
              TCPThrd[n].onTerminate := @ThrdTerminate;
              TCPThrd[n].onExecuteCmd := FExecuteCmd;
              TCPThrd[n].onExecuteJSON := FExecuteJSON;
              TCPThrd[n].onGetImage := FGetImage;
              TCPThrd[n].id := n;
              ThrdActive[n] := True;
              TCPThrd[n].Start;
            end
            else
              with TTCPThrd.Create(ClientSock) do
              begin
                Fsock := TTCPBlockSocket.Create;
                Fsock.socket := CSock;
                Fsock.GetSins;
                Fsock.MaxLineLength := 1024;
                if not terminated then
                begin
                  if Fsock <> nil then begin
                   Fsock.SendString('HTTP/1.0 503' + CRLF);
                   Fsock.SendString('' + CRLF);
                   Fsock.SendString(msgFailed + ' Maximum connection reach!' + CRLF);
                  end;
                  Fsock.CloseSocket;
                  Fsock.Free;
                end;
                Free;
              end;
          end
          else if lasterror <> 0 then
            Synchronize(@ShowError);
        end;
      until False;
    end;
  finally
    //  Suspended:=true;
    Sock.AbortSocket;
    Sock.Free;
    //  terminate;
  end;
end;

constructor TTCPThrd.Create(Hsock: TSocket);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  Csock := Hsock;
  abort := False;
  id:=-1;
end;

procedure TTCPThrd.Execute;
var
  s,su: string;
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
          //if s<>'' then writeln(s);   // for debuging only, not thread safe!
          if lastError = 0 then
          begin
            su:=uppercase(s);
            if (su = 'QUIT') or (su = 'EXIT') then begin
              break;
            end
            else if copy(su,1,3)='GET' then begin
               FHttpRequest:=s;
               Synchronize(@ProcessHttp);
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
    i,p: integer;
begin
  try
    Fjsonid:='null';
    attrib:=Tstringlist.Create;
    value:=Tstringlist.Create;
    J:=GetJSON(FJSONRequest);
    JsonRecurseLevel:=0;
    JsonDataToStringlist(attrib,value,'',J);
    J.Free;
    for i:=0 to attrib.Count-1 do begin
      WriteLn(attrib[i]+' = '+value[i]);
    end;
    p:=attrib.IndexOf('id');
    if p>=0 then
      Fjsonid:=value[p];
    if (Fjsonid<>'null') and Assigned(FExecuteJSON) then
      cmdresult := FExecuteJSON(Fjsonid,attrib,value);
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
