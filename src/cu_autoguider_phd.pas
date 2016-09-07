unit cu_autoguider_phd;

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

interface

uses cu_autoguider, cu_tcpclient, u_global, u_utils, blcksock, synsock, fpjson, jsonparser,
  Forms, Classes, SysUtils;

type

  T_autoguider_phd = class(T_autoguider)
  protected
    TcpClient : TTcpclient;
    procedure JsonDataToStringlist(var SK,SV: TStringList; prefix:string; D : TJSONData);
    procedure JsonToStringlist(jsontxt:string; var SK,SV: TStringList);
    procedure Send(const Value: string);
    procedure SetState;
    Procedure ProcessEvent(txt:string); override;
    procedure Execute; override;
    procedure StarLostTimerTimer(Sender: TObject); override;
  public
    Constructor Create;
    Destructor Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''); override;
    procedure Disconnect; override;
    procedure ConnectGear; override;
    procedure SettleTolerance(pixel:double; mintime,maxtime: integer); override;
    procedure Calibrate; override;
    procedure Guide(onoff:boolean; recalibrate:boolean=false); override;
    procedure Pause(onoff:boolean); override;
    procedure Dither(pixel:double; raonly:boolean); override;
    function WaitBusy(maxwait:integer=5):boolean; override;
  end;

implementation

Constructor T_autoguider_phd.Create ;
begin
  inherited Create;
  FAutoguiderType:=PHD;
  FTargetHost:='localhost';
  FTargetPort:='4400';
  FTimeout:=500;
end;

Destructor T_autoguider_phd.Destroy;
begin
  inherited Destroy;
end;

Procedure T_autoguider_phd.Connect(cp1: string; cp2:string='');
begin
  FTargetHost:=cp1;
  FTargetPort:=cp2;
  FStatus:='Connecting';
  Start;
end;

procedure T_autoguider_phd.Disconnect;
begin
 Terminate;
end;

procedure T_autoguider_phd.Execute;
var buf:string;
begin
tcpclient:=TTCPClient.Create;
try
 tcpclient.TargetHost:=FTargetHost;
 tcpclient.TargetPort:=FTargetPort;
 tcpclient.Timeout := FTimeout;
 // connect
 if tcpclient.Connect then begin
   FRunning:=true;
   if assigned(FonConnect) then FonConnect(self);
   // main loop
   repeat
     if terminated then break;
     buf:=tcpclient.recvstring;
     if (tcpclient.Sock.lastError<>0)and(tcpclient.Sock.lastError<>WSAETIMEDOUT) then break;
     if (buf<>'') then ProcessEvent(buf);
   until false;
 end
 else begin
   DisplayMessage('Cannot connect to PHD2, Is PHD2 running and the server active?');
   if assigned(FonConnectError) then FonConnectError(self);
 end;
DisplayMessage(tcpclient.GetErrorDesc);
finally
FRunning:=false;
StarLostTimer.Enabled:=false;
terminate;
tcpclient.Disconnect;
FStatus:='Disconnected';
ProcessDisconnect;
tcpclient.Free;
end;
end;

procedure T_autoguider_phd.Send(const Value: string);
begin
//writeln('>>'+Value);
 if (tcpclient<>nil)and(Value>'') then begin
   tcpclient.Sock.SendString(Value+crlf);
   if tcpclient.Sock.LastError<>0 then begin
      Terminate;
   end;
 end;
end;

procedure T_autoguider_phd.JsonDataToStringlist(var SK,SV: TStringList; prefix:string; D : TJSONData);
var i:integer;
    pr,buf:string;
begin
if Assigned(D) then begin
  case D.JSONType of
    jtArray,jtObject: begin
        for i:=0 to D.Count-1 do begin
           if D.JSONType=jtArray then begin
              if prefix='' then pr:=IntToStr(I) else pr:=prefix+'.'+IntToStr(I);
              JsonDataToStringlist(SK,SV,pr,D.items[i]);
           end else begin
              if prefix='' then pr:=TJSONObject(D).Names[i] else pr:=prefix+'.'+TJSONObject(D).Names[i];
              JsonDataToStringlist(SK,SV,pr,D.items[i]);
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

procedure T_autoguider_phd.JsonToStringlist(jsontxt:string; var SK,SV: TStringList);
var  J: TJSONData;
     i: integer;
     c1,c2: string;
     b1: string;
begin
jsontxt:=StringReplace(jsontxt,chr(10),' ',[rfReplaceAll]);
jsontxt:=StringReplace(jsontxt,chr(13),' ',[rfReplaceAll]);
J:=GetJSON(jsontxt);
JsonDataToStringlist(SK,SV,'',J);
J.Free;
end;

Procedure T_autoguider_phd.ProcessEvent(txt:string);
var eventname,rpcid,rpcresult,rpcerror,err: string;
    attrib,value:Tstringlist;
    p,i,s,k:integer;
begin
attrib:=Tstringlist.Create;
value:=Tstringlist.Create;
FLastError:='';
try
JsontoStringlist(txt,attrib,value);
p:=attrib.IndexOf('Event');    // PHD events
if p>=0 then begin
   eventname:=value[p];
   if (eventname='GuideStep') then begin
     if (FStatus<>'Settling') then FStatus:='Guiding';
     i:=attrib.IndexOf('ErrorCode');
     if i>=0 then begin
       k:=StrToIntDef(trim(value[i]),-1);
       case k of
         0: FLastError:='STAR_OK';
         1: FLastError:='STAR_SATURATED';
         2: FLastError:='STAR_LOWSNR';
         3: FLastError:='STAR_LOWMASS';
         4: FLastError:='STAR_TOO_NEAR_EDGE';
         5: FLastError:='STAR_MASSCHANGE';
         6: FLastError:='STAR_ERROR';
         else FLastError:='';
       end;
     end;
   end
   else if eventname='StartGuiding' then FStatus:='Start Guiding'
   else if eventname='GuidingStopped' then FStatus:='Stopped'
   else if eventname='StarSelected' then FStatus:='Star Selected'
   else if eventname='StarLost' then begin
      i:=attrib.IndexOf('Status');
      if i>=0 then FLastError:='StarLost '+value[i];
      if (FStatus='Guiding')and(not StarLostTimer.Enabled) then begin
        FStarLostTime:=now;
        StarLostTimer.Enabled:=true;
      end;
      FStatus:='Star lost';
   end
   else if eventname='Paused' then FStatus:='Paused'
   else if eventname='Resumed' then FStatus:='Resumed'
   else if eventname='LockPositionSet' then FStatus:='Lock Position Set'
   else if eventname='CalibrationComplete' then FStatus:='Calibration Complete'
   else if eventname='StarSelected' then FStatus:='Star Selected'
   else if eventname='StartCalibration' then FStatus:='Start Calibration'
   else if eventname='CalibrationFailed' then begin
     FStatus:='Calibration Failed';
     i:=attrib.IndexOf('Reason');
     if i>=0 then FLastError:='CalibrationFailed '+value[i];
   end
   else if eventname='CalibrationDataFlipped' then FStatus:='Calibration Data Flipped'
   else if eventname='LoopingExposures' then FStatus:='Looping Exposures'
   else if eventname='LoopingExposuresStopped' then FStatus:='Looping Exposures Stopped'
   else if eventname='Settling' then FStatus:='Settling'
   else if eventname='SettleDone' then begin
     i:=attrib.IndexOf('Status');
     if i>=0 then s:=StrToIntDef(value[i],0) else s:=0;
     if s=0 then
       FStatus:='Settle Done'
     else begin
       i:=attrib.IndexOf('Error');
       if i>=0 then err:=value[i] else err:='?';
       FStatus:='Error: '+err;
     end;
   end
   else if eventname='GuidingDithered' then FStatus:='Guiding Dithered'
   else if eventname='LockPositionLost' then FStatus:='Lock Position Lost'
   else if eventname='Alert' then begin
     FStatus:='Alert';
     i:=attrib.IndexOf('Type');
     if i>=0 then err:=value[i]+', ' else err:='';
     i:=attrib.IndexOf('Msg');
     if i>=0 then err:=err+value[i];
     FLastError:='Alert '+err;
   end
   else if eventname='AppState' then begin
     i:=attrib.IndexOf('State');
     if i>=0 then FStatus:=value[i];
   end
   else if eventname='Version' then begin
     i:=attrib.IndexOf('PHDVersion');
     if i>=0 then begin
        FVersion:=value[i];
     end;
     i:=attrib.IndexOf('MsgVersion');
     if i>=0 then begin
        FMsgVersion:=value[i];
     end;
   end;
end else begin
  p:=attrib.IndexOf('jsonrpc');   // Response to command
  if p>=0 then begin
     p:=attrib.IndexOf('id');
     rpcid:=value[p];
     i:=attrib.IndexOf('result');
     if i>=0 then rpcresult:=value[i]
     else begin
       i:=attrib.IndexOf('error.code');
       if i>=0 then begin
          rpcresult:='error';
          i:=attrib.IndexOf('error.message');
          if i>=0 then rpcerror:=value[i]
                  else rpcerror:='no response';
       end
       else rpcresult:='ok';
     end;
     if rpcid='1000' then begin  // set_connected
       if rpcresult='error' then begin
          DisplayMessage('set_connected'+' '+rpcresult+' '+rpcerror);
       end
       else FState:=GUIDER_IDLE;
     end
     else if rpcid='2001' then begin  // stop capture
       if rpcresult='error' then begin
          DisplayMessage('stop capture'+' '+rpcresult+' '+rpcerror);
       end;
     end
     else if rpcid='2002' then begin  // set_paused
       if rpcresult='error' then begin
          DisplayMessage('set_paused'+' '+rpcresult+' '+rpcerror);
       end;
     end
     else if rpcid='2003' then begin  // guide
       if rpcresult='error' then begin
          DisplayMessage('guide'+' '+rpcresult+' '+rpcerror);
       end;
     end
     else if rpcid='2004' then begin  // loop (stop guiding)
       if rpcresult='error' then begin
          DisplayMessage('loop'+' '+rpcresult+' '+rpcerror);
       end;
     end
     else if rpcid='2005' then begin  // find_star
       if rpcresult='error' then begin
          DisplayMessage('find_star'+' '+rpcresult+' '+rpcerror);
       end;
     end
     else if rpcid='2010' then begin  // dither
       if rpcresult='error' then begin
          DisplayMessage('dither'+' '+rpcresult+' '+rpcerror);
       end;
     end;

  end;
end;
SetState;
StatusChange;
finally
 attrib.Free;
 value.Free;
end;
end;

procedure T_autoguider_phd.SetState;
begin
  if FStatus='Guiding' then FState:=GUIDER_GUIDING
  else if FStatus='Start Guiding' then FState:=GUIDER_BUSY
  else if FStatus='Stopped' then FState:=GUIDER_IDLE
  else if FStatus='Star Selected' then FState:=GUIDER_IDLE
  else if FStatus='Star lost' then FState:=GUIDER_ALERT
  else if FStatus='Paused' then FState:=GUIDER_IDLE
  else if FStatus='Resumed' then FState:=GUIDER_GUIDING
  else if FStatus='Lock Position Set' then FState:=FState
  else if FStatus='Calibration Complete' then FState:=GUIDER_IDLE
  else if FStatus='Star Selected' then FState:=FState
  else if FStatus='Start Calibration' then FState:=GUIDER_BUSY
  else if FStatus='Calibration Failed' then FState:=GUIDER_ALERT
  else if FStatus='Calibration Data Flipped' then FState:=FState
  else if FStatus='Looping Exposures' then FState:=GUIDER_IDLE
  else if FStatus='Looping Exposures Stopped' then FState:=GUIDER_IDLE
  else if FStatus='Settling' then FState:=FState
  else if FStatus='Settle Done' then FState:=FState
  else if FStatus='Guiding Dithered' then FState:=GUIDER_BUSY
  else if FStatus='Lock Position Lost' then FState:=FState
  else if FStatus='Alert' then FState:=GUIDER_ALERT
  else if copy(FStatus,1,6)='Error:' then FState:=GUIDER_ALERT;
end;

procedure T_autoguider_phd.ConnectGear;
var buf:string;
begin
 buf:='{"method": "set_connected", "params": [';
 buf:=buf+'true';
 buf:=buf+'], "id": 1000}';
 Send(buf);
end;

procedure T_autoguider_phd.SettleTolerance(pixel:double; mintime,maxtime: integer);
begin
FSettlePix:=FormatFloat(f1,pixel);
FSettleTmin:=IntToStr(mintime);
FSettleTmax:=IntToStr(maxtime);
end;

function T_autoguider_phd.WaitBusy(maxwait:integer=5):boolean;
var endt: TDateTime;
begin
  endt:=now+maxwait/secperday;
  while now<endt do begin
    Sleep(100);
    Application.ProcessMessages;
    if FState<>GUIDER_BUSY then break;
  end;
  result:=(FState<>GUIDER_BUSY);
end;

procedure T_autoguider_phd.Calibrate;
begin
  Guide(true,true);
end;

procedure T_autoguider_phd.Guide(onoff:boolean; recalibrate:boolean=false);
var buf:string;
    cguide: boolean;
begin
  cguide:=(FState=GUIDER_GUIDING);
  if onoff then begin
    buf:='{"method": "loop","id":2004}';
    FState:=GUIDER_BUSY;
    Send(buf);
    WaitBusy(60);
    Wait(10); //enough time for a new guide exposure
    buf:='{"method": "find_star","id":2005}';
    FState:=GUIDER_BUSY;
    Send(buf);
    WaitBusy(60);
    Wait(2);
    buf:='{"method": "guide", "params": [';
    buf:=buf+'{"pixels": '+FSettlePix+',';      // settle tolerance
    buf:=buf+'"time": '+FSettleTmin+',';       // min time
    buf:=buf+'"timeout": '+FSettleTmax+'},';   // max time
    if recalibrate then buf:=buf+'true' else buf:=buf+'false';
    buf:=buf+'],';
    buf:=buf+'"id": 2003}';
    Send(buf);
  end else begin
    buf:='{"method": "stop_capture","id":2001}';
    Send(buf);
  end;
  if cguide<>onoff then FState:=GUIDER_BUSY;
end;

procedure T_autoguider_phd.Pause(onoff:boolean);
var buf:string;
begin
  if onoff then begin
    buf:='{"method": "set_paused","params":[true,"full"],"id":2002}';
    Send(buf);
  end else begin
    buf:='{"method": "set_paused","params":[false],"id":2002}';
    Send(buf);
  end;
end;

procedure T_autoguider_phd.Dither(pixel:double; raonly:boolean);
var pix,rao,buf:string;
begin
  pix:=FormatFloat(f1,pixel);
  if raonly then rao:='true' else rao:='false';
  buf:='{"method": "dither", "params": [';
  buf:=buf+pix+',';                    // pixels
  buf:=buf+rao+',';                    // ra only
  buf:=buf+'{"pixels": '+FSettlePix+',';      // settle tolerance
  buf:=buf+'"time": '+FSettleTmin+',';        // min time
  buf:=buf+'"timeout": '+FSettleTmax+'}],';   // max time
  buf:=buf+'"id": 2010}';
  Send(buf);
  FState:=GUIDER_BUSY;
end;

procedure T_autoguider_phd.StarLostTimerTimer(Sender: TObject);
var losttime: integer;
begin
 if FState=GUIDER_GUIDING then begin
    if assigned(FonShowMessage) then FonShowMessage('Guiding recovered from star lost');
    StarLostTimer.Enabled:=false;
 end
 else begin
    losttime:=round(secperday*(now-FStarLostTime));
    FonShowMessage('Star lost for '+inttostr(losttime)+' seconds...');
    if losttime>FStarLostTimeout2 then begin
       if assigned(FonShowMessage) then FonShowMessage('Cannot recover from star lost. Stop guiding now.');
       StarLostTimer.Enabled:=false;
       guide(false);
    end
    else if (FState<>GUIDER_BUSY)and(losttime>FStarLostTimeout1) then begin
       if assigned(FonShowMessage) then FonShowMessage('Try to restart guiding');
       StarLostTimer.Enabled:=false;
       guide(false);
       WaitBusy();
       Sleep(1000);
       guide(true);
       WaitBusy();
       Sleep(1000);
       StarLostTimer.Enabled:=true;
    end;
 end;
end;

end.

