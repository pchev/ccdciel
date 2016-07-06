unit cu_planetarium_cdc;

{$mode objfpc}{$H+}

{                                        
Copyright (C) 2015 Patrick Chevalley

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
{
  TCP/IP client object thread to connect to Cartes du Ciel
}

interface

uses u_global, u_utils, cu_planetarium, cu_tcpclient, blcksock, Classes, SysUtils,
    Forms;

type

  TPlanetarium_cdc = class(TPlanetarium)
  private
    TcpClient : TTcpclient;
  protected
    procedure Execute; override;
    procedure ProcessDataSyn; override;
  public
    Constructor Create;
    procedure Connect(cp1: string; cp2:string=''); override;
    procedure Disconnect; override;
    function Cmd(const Value: string):string; override;
    function ShowImage(fn: string):boolean; override;
  end;

const msgTimeout='Timeout!';
      msgOK='OK';
      msgFailed='Failed!';
      msgBye='Bye!';

implementation

/////////////////// TPlanetarium_cdc ///////////////////////////

Constructor TPlanetarium_cdc.Create ;
begin
inherited Create;
FPlanetariumType:=CDC;
FTargetHost:='localhost';
FTargetPort:='3292';
FTimeout:=200;
FCmdTimeout:=10/86400;
end;

procedure TPlanetarium_cdc.Connect(cp1: string; cp2:string='');
begin
  FTargetHost:=cp1;
  if cp2='' then FTargetPort:=GetCdCPort
            else FTargetPort:=cp2;
  Start;
end;

procedure TPlanetarium_cdc.Disconnect;
begin
 Terminate;
end;

procedure TPlanetarium_cdc.Execute;
var buf:string;
    dateto : double;
    i : integer;
    ending : boolean;
begin
ending:=false;
tcpclient:=TTCPClient.Create;
try
 tcpclient.TargetHost:=FTargetHost;
 tcpclient.TargetPort:=FTargetPort;
 tcpclient.Timeout := FTimeout;
 // connect
 if tcpclient.Connect then begin
    FRunning:=true;
    // wait connect message
    dateto:=now+Fcmdtimeout;
    repeat
      buf:=tcpclient.recvstring;
      if (buf='')or(buf='.') then continue;  // keepalive
      if copy(buf,1,1)='>' then ProcessData(buf) // mouse click
      else
        if copy(buf,1,2)=msgOK then begin
           // success, parse response
           ProcessData(buf);
           i:=pos('id=',buf);
           Fclientid:=trim(copy(buf,i+3,2));
           i:=pos('chart=',buf);
           buf:=copy(buf,i+6,999)+' ';
           i:=pos(' ',buf);
           FclientName:=trim(copy(buf,1,i-1));
           FStatus:=true;
           if assigned(FonConnect) then FonConnect(self);
           break;
        end else begin
           // failure, close thread
           ProcessData(buf);
           terminate;
           break;
        end;
   until now>dateto;
   if tcpclient.resultbuffer='' then tcpclient.resultbuffer:=msgTimeout;
   DisplayMessage(tcpclient.GetErrorDesc);
   // main loop
   repeat
     if terminated then break;
     // handle unattended messages (mouseclick...)
     buf:=tcpclient.recvstring;
     if ending and (tcpclient.Sock.LastError<>0) then break; // finish to read data before to exit
     if (buf<>'')and(buf<>'.') then ProcessData(buf);
     if buf=msgBye then ending:=true;
     // handle synchronous command and response
     if tcpclient.sendbuffer<>'' then begin
        tcpclient.resultbuffer:='';
        // send command
        tcpclient.Sock.SendString(tcpclient.sendbuffer+crlf);
        if tcpclient.Sock.LastError<>0 then begin
           terminate;
           break;
        end;
        tcpclient.sendbuffer:='';
        // wait response
        dateto:=now+Fcmdtimeout;
        repeat
          buf:=tcpclient.recvstring;
          if (buf='')or(buf='.') then continue;  // keepalive
          if copy(buf,1,1)='>' then ProcessData(buf) // mouse click
             else tcpclient.resultbuffer:=buf;   // set result
        until (tcpclient.resultbuffer>'')or(now>dateto);
        if tcpclient.resultbuffer='' then tcpclient.resultbuffer:=msgTimeout;
     end;
   until false;
 end
 else begin
   DisplayMessage('Cannot connect to CdC, Is CdC running and the server active?');

 end;
FRunning:=false;
FStatus:=false;
DisplayMessage(tcpclient.GetErrorDesc);
finally
  if assigned(FonDisconnect) then FonDisconnect(self);
  terminate;
  tcpclient.Disconnect;
  tcpclient.Free;
end;
end;

procedure TPlanetarium_cdc.ProcessDataSyn;
var p:Tstringlist;
begin
if FRecvData<>'' then begin
  p:=Tstringlist.Create;
  SplitRec(FRecvData,#9,p);
  if (p.Count>=4)and(p[0]='>') then begin
    Fra:=StrToAR(p[2]);
    Fde:=StrToDE(p[3]);
  end;
  if (p.Count>=6)and(p[0]='>') then begin
    Fobjname:=StringReplace(p[5],' ','',[rfReplaceAll]);
  end;
  p.free;
end;
if assigned(FonReceiveData) then FonReceiveData(FRecvData);
end;


function TPlanetarium_cdc.Cmd(const Value: string):string;
// this function is called in the main thread only!
// do not use in a planetarium event.
var dateto:double;
begin
  if TcpClient<>nil then begin
     tcpclient.resultbuffer:='';
     if Value>'' then begin
       tcpclient.sendbuffer:=Value;
       dateto:=now+Fcmdtimeout;
       while (tcpclient.resultbuffer='')and(now<dateto) do begin
          sleep(100);
          application.ProcessMessages;
       end;
       if tcpclient.resultbuffer='' then tcpclient.resultbuffer:=msgTimeout;
       result:=tcpclient.resultbuffer;
     end;
  end;
end;

function TPlanetarium_cdc.ShowImage(fn: string):boolean;
begin
  Cmd('SHOWBGIMAGE OFF');
  Cmd('LOADBGIMAGE '+fn);
  Cmd('SHOWBGIMAGE ON');
  result:=true;
end;

end.
