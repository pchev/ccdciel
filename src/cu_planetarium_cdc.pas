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

uses u_global, u_utils, cu_planetarium, cu_tcpclient, blcksock, synsock,
     Classes, SysUtils, Forms;

type

  TPlanetarium_cdc = class(TPlanetarium)
  private
    started: boolean;
    TcpClient : TTcpclient;
    FLastObjname: string;
  protected
    procedure Execute; override;
    procedure ProcessDataSyn; override;
  public
    Constructor Create;
    procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cb1:boolean=False); override;
    procedure Disconnect; override;
    procedure Shutdown; override;
    function Cmd(const Value: string):string; override;
    function ShowImage(fn: string; fovdeg:double=0):boolean; override;
    function DrawFrame(frra,frde,frsizeH,frsizeV,frrot: double):boolean; override;
    function GetEqSys: double; override;
    function Search(sname: string; out sra,sde,v_solar,vpa_solar: double): boolean; override;
    procedure ShowAstrometry(sra,sde: double); override;
  end;

const msgTimeout='Timeout!';
      msgOK='OK!';
      msgFailed='Failed!';
      msgBye='Bye!';

implementation

/////////////////// TPlanetarium_cdc ///////////////////////////

Constructor TPlanetarium_cdc.Create ;
begin
inherited Create;
started:=false;
FPlanetariumType:=CDC;
FTargetHost:='localhost';
FTargetPort:='3292';
FTimeout:=200;
FCmdTimeout:=10/86400;
FLastObjname:='';
end;

procedure TPlanetarium_cdc.Connect(cp1: string; cp2:string=''; cp3:string=''; cb1:boolean=False);
begin
  if started then exit;
  started:=true;
  FTargetHost:=cp1;
  if cb1 and (FTargetHost='localhost')and(cp3<>'') then begin
    FProgramPath:=ExtractFilePath(cp3);
    FProgramName:=ExtractFileName(cp3);
    if FProgramName<>'' then begin
      FStartedProgram:=StartProgram(FProgramName,FProgramPath);
      if FStartedProgram then wait(10);
    end;
  end;
  if cp2='' then FTargetPort:=GetCdCPort
            else FTargetPort:=cp2;
  Start;
end;

procedure TPlanetarium_cdc.Disconnect;
begin
 if FStartedProgram then
   Shutdown
 else
   Terminate;
end;


procedure TPlanetarium_cdc.Shutdown;
begin
 Cmd('SHUTDOWN');
end;

function TPlanetarium_cdc.GetEqSys: double;
var r: string;
begin
  r:=Cmd('GETCHARTEQSYS');
  if r='Date' then
    result:=0
  else
    result:=StrToFloatDef(r,0);
end;

procedure TPlanetarium_cdc.Execute;
var buf,rbuf:string;
    dateto : double;
    i : integer;
    ending : boolean;
begin
started:=true;
ending:=false;
tcpclient:=TTCPClient.Create;
try
 if Terminated then exit;
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
        if copy(buf,1,3)=msgOK then begin
           // success, parse response
           ProcessData(buf);
           i:=pos('id=',buf);
           Fclientid:=trim(copy(buf,i+3,2));
           i:=pos('chart=',buf);
           buf:=copy(buf,i+6,999)+' ';
           i:=pos(' ',buf);
           FclientName:=trim(copy(buf,1,i-1));
           FStatus:=true;
           Synchronize(@SyncOnConnect);
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
     buf:=tcpclient.RecvString;
     if (tcpclient.Sock.LastError<>0)and(tcpclient.Sock.LastError<>WSAETIMEDOUT) then break; // unexpected error
     if ending and (tcpclient.Sock.LastError<>0) then break; // finish to read data before to exit
     if (buf<>'')and(buf<>'.') then ProcessData(buf);
     if buf=msgBye then ending:=true;
     // handle synchronous command and response
     if tcpclient.sendbuffer<>'' then begin
        tcpclient.resultbuffer:='';
        rbuf:='';
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
          buf:=tcpclient.RecvString;
          if (buf='')or(buf='.') then continue;  // keepalive
          if copy(buf,1,1)='>' then rbuf:=buf+rbuf // response data
             else tcpclient.resultbuffer:=buf+rbuf;   // set result
        until (tcpclient.resultbuffer>'')or(now>dateto);
        if tcpclient.resultbuffer='' then tcpclient.resultbuffer:=msgTimeout;
     end;
   until false;
 end;
FRunning:=false;
FStatus:=false;
if not terminated then DisplayMessage(tcpclient.GetErrorDesc);
finally
  terminate;
  Synchronize(@SyncOnDisconnect);
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
  if p[0]='>' then begin
    if (p.Count>=4) then begin
      Fra:=StrToAR(p[2]);
      Fde:=StrToDE(p[3]);
      if (FplanetariumEquinox<>2000)and(FplanetariumJD>0) then begin
        if (Fra<>NullCoord)and(Fde<>NullCoord) then begin
          Fra:=Fra*15*deg2rad;
          Fde:=Fde*deg2rad;
          mean_equatorial(Fra,Fde);
          PrecessionFK5(FplanetariumJD,jd2000,Fra,Fde);
          Fra:=rad2deg*Fra/15;
          Fde:=rad2deg*Fde;
        end;
      end;
    end;
    if (p.Count>=6) then begin
       Fobjname:=trim(p[5]);
    end;
    if (p.Count>=8)and(copy(p[7],1,3)='pa:') then begin
       Fpa:=StrToFloatDef(trim(copy(p[7],4,99)),NullCoord);
    end
    else
       Fpa:=NullCoord;
    if assigned(FonReceiveData) and (Fobjname<>FLastObjname) then FonReceiveData(FRecvData);
    FLastObjname:=Fobjname;
  end;
  p.free;
end;
end;


function TPlanetarium_cdc.Cmd(const Value: string):string;
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
          if GetCurrentThreadId=MainThreadID then application.ProcessMessages;
       end;
       if (not Terminated) then begin
         if tcpclient.resultbuffer='' then tcpclient.resultbuffer:=msgTimeout;
         result:=tcpclient.resultbuffer;
       end;
     end;
  end;
end;

function CompareResult(resp,comp: string): boolean;
begin
  result:=(copy(resp,1,Length(comp))=comp);
end;

function TPlanetarium_cdc.ShowImage(fn: string; fovdeg:double=0):boolean;
var buf,adjust:string;
begin
  result:=false;
  if CdCAdjustFrame then
    adjust:='ON'
  else
    adjust:='OFF';
  buf:='SHOWBGIMAGE OFF';
  FLastErrorTxt:=Cmd(buf);
  if not CompareResult(FLastErrorTxt,msgOK) then begin FLastErrorTxt:=buf+': '+FLastErrorTxt; exit; end;
  buf:='LOADBGIMAGE '+fn+' '+adjust;
  FLastErrorTxt:=Cmd(buf);
  if not CompareResult(FLastErrorTxt,msgOK) then begin FLastErrorTxt:=buf+': '+FLastErrorTxt; exit; end;
  if fovdeg=0 then begin
    buf:='SHOWBGIMAGE ON '+adjust;
    FLastErrorTxt:=Cmd(buf);
    if not CompareResult(FLastErrorTxt,msgOK) then begin FLastErrorTxt:=buf+': '+FLastErrorTxt; exit; end;
  end
  else begin
    buf:='SetFOV '+FormatFloat(f4,fovdeg);
    FLastErrorTxt:=Cmd(buf);
    if not CompareResult(FLastErrorTxt,msgOK) then begin FLastErrorTxt:=buf+': '+FLastErrorTxt; exit; end;
    buf:='SHOWBGIMAGE ON OFF';
    FLastErrorTxt:=Cmd(buf);
    if not CompareResult(FLastErrorTxt,msgOK) then begin FLastErrorTxt:=buf+': '+FLastErrorTxt; exit; end;
  end;
  FLastErrorTxt:='';
  result:=true;
end;

function TPlanetarium_cdc.DrawFrame(frra,frde,frsizeH,frsizeV,frrot: double):boolean;
var buf:string;
    cjd: double;
    f:textfile;
begin
  result:=false;
  cjd:=jdtoday;
  if (FplanetariumEquinox<>0)and(FplanetariumJD>0) then begin
    frra:=frra*deg2rad;
    frde:=frde*deg2rad;
    mean_equatorial(frra,frde);
    PrecessionFK5(jdtoday,FplanetariumJD,frra,frde);
    cjd:=FplanetariumJD;
    frra:=rad2deg*frra;
    frde:=rad2deg*frde;
  end;
  buf := 'SHOWRECTANGLE 10';
  FLastErrorTxt:=Cmd(buf);
  if not CompareResult(FLastErrorTxt,msgOK) then begin FLastErrorTxt:=buf+': '+FLastErrorTxt; exit; end;
  buf := 'SETRECTANGLE 10 ' + FormatFloat('0.000', frsizeH*60) + ' ' +
    FormatFloat('0.00', frsizeV*60) + ' ' +
    FormatFloat('0.00', frrot) + ' 0';
  FLastErrorTxt:=Cmd(buf);
  if not CompareResult(FLastErrorTxt,msgOK) then begin FLastErrorTxt:=buf+': '+FLastErrorTxt; exit; end;
  AssignFile(f,slash(TmpDir)+'ccdciel.cdcc');
  Rewrite(f);
  writeln(f,'EQUINOX='+FormatFloat(f1,cjd));
  writeln(f,'Circle_01 '+ARToStr3(frra/15.0)+' '+DEToStr3(frde)+' - - -');
  CloseFile(f);
  buf:='LOADCIRCLE '+slash(TmpDir)+'ccdciel.cdcc';
  FLastErrorTxt:=Cmd(buf);
  if not CompareResult(FLastErrorTxt,msgOK) then begin FLastErrorTxt:=buf+': '+FLastErrorTxt; exit; end;
  buf:='MARKCENTER OFF';
  FLastErrorTxt:=Cmd(buf);
  if not CompareResult(FLastErrorTxt,msgOK) then begin FLastErrorTxt:=buf+': '+FLastErrorTxt; exit; end;
  if CdCAdjustFrame then begin
    buf := 'SETRA ' + FormatFloat('0.00000', frra/15.0);
    FLastErrorTxt:=Cmd(buf);
    if not CompareResult(FLastErrorTxt,msgOK) then begin FLastErrorTxt:=buf+': '+FLastErrorTxt; exit; end;
    buf := 'SETDEC ' + FormatFloat('0.00000', frde);
    FLastErrorTxt:=Cmd(buf);
    if not CompareResult(FLastErrorTxt,msgOK) then begin FLastErrorTxt:=buf+': '+FLastErrorTxt; exit; end;
    buf := 'SETFOV ' + FormatFloat('0.000', frsizeH*2.2);
    FLastErrorTxt:=Cmd(buf);
    if not CompareResult(FLastErrorTxt,msgOK) then begin FLastErrorTxt:=buf+': '+FLastErrorTxt; exit; end;
  end;
  buf:='REDRAW';
  FLastErrorTxt:=Cmd(buf);
  if not CompareResult(FLastErrorTxt,msgOK) then begin FLastErrorTxt:=buf+': '+FLastErrorTxt; exit; end;
  FLastErrorTxt:='';
  result:=true;
end;

function TPlanetarium_cdc.Search(sname: string; out sra,sde,v_solar,vpa_solar: double): boolean;
var buf: string;
    p: TStringList;
    i: integer;
begin
  // return J2000 coord.
  result:=false;

  v_solar:=NullCoord;
  vpa_solar:=NullCoord;

  p:=TStringList.Create;
  try
  buf:=Cmd('SEARCH "'+sname+'"');
  if copy(buf,1,3)=msgOK then begin
     SplitRec(buf,tab,p);
     if (p.Count>=4) then begin
       sra:=StrToAR(p[2]);
       sde:=StrToDE(p[3]);
       if (sra<>NullCoord)and(sde<>NullCoord) then begin
         result:=true;
         if (FplanetariumEquinox<>2000)and(FplanetariumJD>0)then begin
           if (sra<>NullCoord)and(sde<>NullCoord) then begin
             sra:=sra*15*deg2rad;
             sde:=sde*deg2rad;
             mean_equatorial(sra,sde);
             PrecessionFK5(FplanetariumJD,jd2000,sra,sde);
             sra:=rad2deg*sra/15;
             sde:=rad2deg*sde;
           end;
         end;
         for i:=0 to p.count-1 do begin
           if copy(p[i],1,11)='MotionArcm:' then v_solar:=StrToFloatDef(copy(p[i],12,99),NullCoord);
           if copy(p[i],1,9)='MotionPA:' then vpa_solar:=StrToFloatDef(copy(p[i],10,99),NullCoord);
         end;
       end;
     end;
  end
  else
    FLastErrorTxt:=buf;
  finally
    p.Free;
  end;
end;

procedure TPlanetarium_cdc.ShowAstrometry(sra,sde: double);
var buf: string;
begin
  sra:=sra*deg2rad*15;
  sde:=sde*deg2rad;
  PrecessionFK5(jd2000,FplanetariumJD,sra,sde);
  if FplanetariumEquinox=0 then apparent_equatorial(sra,sde);
  sra:=rad2deg*sra/15;
  sde:=rad2deg*sde;
  buf := 'MOVESCOPE ' + FormatFloat('0.00000', sra) + ' ' + FormatFloat('0.00000', sde);
  FLastErrorTxt:=Cmd(buf);
end;

end.
