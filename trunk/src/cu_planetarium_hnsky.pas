unit cu_planetarium_hnsky;

{$mode objfpc}{$H+}

{                                        
Copyright (C) 2017 Patrick Chevalley

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
  TCP/IP client object thread to connect to HNSKY
}

interface

uses u_global, u_utils, cu_planetarium, cu_tcpclient, blcksock, synsock,
     Classes, SysUtils, Forms;

type

  TPlanetarium_hnsky = class(TPlanetarium)
  private
    started: boolean;
    TcpClient : TTcpclient;
  protected
    procedure Execute; override;
    procedure ProcessDataSyn; override;
  public
    Constructor Create;
    procedure Connect(cp1: string; cp2:string=''); override;
    procedure Disconnect; override;
    procedure Shutdown; override;
    function Cmd(const Value: string):string; override;
    function ShowImage(fn: string):boolean; override;
    function DrawFrame(frra,frde,frsizeH,frsizeV,frrot: double):boolean; override;
    function GetEqSys: double; override;
  end;

const msgTimeout='Timeout';
      msgOK='OK';
      msgFailed='?';

implementation

/////////////////// TPlanetarium_hnsky ///////////////////////////

Constructor TPlanetarium_hnsky.Create ;
begin
inherited Create;
started:=false;
FPlanetariumType:=HNSKY;
FTargetHost:='localhost';
FTargetPort:='7700';
FTimeout:=200;
FCmdTimeout:=10/86400;
end;

function TPlanetarium_hnsky.GetEqSys: double;
begin
  result:=2000.0;
end;

procedure TPlanetarium_hnsky.Connect(cp1: string; cp2:string='');
begin
  if started or Terminated then exit;
  Start;
end;

procedure TPlanetarium_hnsky.Disconnect;
begin
 Terminate;
end;

procedure TPlanetarium_hnsky.Shutdown;
begin
 Cmd('SHUTDOWN');
end;

procedure TPlanetarium_hnsky.Execute;
var buf:string;
    dateto : double;
    i : integer;
    ending : boolean;
begin
started:=true;
ending:=false;
tcpclient:=TTCPClient.Create;
try
 tcpclient.TargetHost:=FTargetHost;
 tcpclient.TargetPort:=FTargetPort;
 tcpclient.Timeout := FTimeout;
 // connect
 if tcpclient.Connect then begin
   FRunning:=true;
   FStatus:=true;
   if assigned(FonConnect) then FonConnect(self);
   // main loop
   repeat
     if terminated then break;
     // handle unattended messages (mouseclick...)
     buf:=tcpclient.RecvPacket;
     if (tcpclient.Sock.LastError<>0)and(tcpclient.Sock.LastError<>WSAETIMEDOUT) then break; // unexpected error
     if ending and (tcpclient.Sock.LastError<>0) then break; // finish to read data before to exit
     if (buf<>'') then ProcessData(buf);
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
          buf:=tcpclient.RecvPacket;
          if (buf='') then continue;
          //if copy(buf,1,1)='>' then ProcessData(buf) // mouse click
          tcpclient.resultbuffer:=buf;   // set result
        until (tcpclient.resultbuffer>'')or(now>dateto);
        if tcpclient.resultbuffer='' then tcpclient.resultbuffer:=msgTimeout;
     end;
   until false;
 end
 else begin
   DisplayMessage('Cannot connect to HNSKY, Is HNSKY running?');
 end;
FRunning:=false;
FStatus:=false;
if not terminated then DisplayMessage(tcpclient.GetErrorDesc);
finally
  terminate;
  if assigned(FonDisconnect) then FonDisconnect(self);
  tcpclient.Disconnect;
  tcpclient.Free;
end;
end;

procedure TPlanetarium_hnsky.ProcessDataSyn;
var p:Tstringlist;
begin
if FRecvData<>'' then begin
  p:=Tstringlist.Create;
  SplitRec(FRecvData,blank,p);
  if (p.Count>=3) then begin
    Fra:=StrToFloatDef(StringReplace(p[0],',','.',[]),NullCoord);
    Fde:=StrToFloatDef(StringReplace(p[1],',','.',[]),NullCoord);
    if (Fra<>NullCoord)and(Fde<>NullCoord) then begin
      J2000ToApparent(Fra,Fde);
      Fra:=rad2deg*Fra/15;
      Fde:=rad2deg*Fde;
      Fobjname:=p[2];
    end;
  end;
  p.free;
end;
if assigned(FonReceiveData) then FonReceiveData(FRecvData);
end;

function TPlanetarium_hnsky.Cmd(const Value: string):string;
// this function is called in the main thread only!
// do not use in a planetarium event.
var dateto:double;
begin
  result:=msgFailed;
  if (TcpClient<>nil)and(not Terminated) then begin
     tcpclient.resultbuffer:='';
     if (not Terminated)and(Value>'') then begin
       tcpclient.sendbuffer:=Value;
       dateto:=now+Fcmdtimeout;
       while (not Terminated)and(tcpclient.resultbuffer='')and(now<dateto) do begin
          sleep(100);
          application.ProcessMessages;
       end;
       if (not Terminated) then begin
         if tcpclient.resultbuffer='' then tcpclient.resultbuffer:=msgTimeout;
         result:=tcpclient.resultbuffer;
       end;
     end;
  end;
end;

function TPlanetarium_hnsky.ShowImage(fn: string):boolean;
begin
result:=(Cmd('LOAD_FITS '+fn)=msgOK);
end;

function TPlanetarium_hnsky.DrawFrame(frra,frde,frsizeH,frsizeV,frrot: double):boolean;
var buf,r: string;
    fra_2000, fdec_2000: double;
begin
  fra_2000:=frra*deg2rad;
  fdec_2000:=frde*deg2rad;
  ApparentToJ2000(fra_2000,fdec_2000);
  buf:='SET_FRAME '+
       formatfloat(f5,frsizeH*deg2rad) + blank +
       formatfloat(f5,frsizeV*deg2rad) + blank +
       formatfloat(f5,frrot*deg2rad) + blank +
       formatfloat(f5,fra_2000) + blank +
       formatfloat(f5,fdec_2000);
  r:=Cmd(buf);
  result:=(r=msgOK);
end;


end.
