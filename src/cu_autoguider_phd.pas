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

uses cu_autoguider, cu_tcpclient, u_global, u_utils, blcksock, synsock, fpjson, jsonparser, u_translation,
  Forms, Classes, SysUtils;

type

  T_autoguider_phd = class(T_autoguider)
  private
    FArcsecPixel: double;
    FLockPositionX,FLockPositionY: string;
    FLockPositionReceived: boolean;
    procedure ProcessEventAsync(Data: PtrInt);
  protected
    TcpClient : TTcpclient;
    JsonRecurseLevel: integer;
    procedure JsonDataToStringlist(var SK,SV: TStringList; prefix:string; D : TJSONData);
    procedure JsonToStringlist(jsontxt:TStringStream; var SK,SV: TStringList);
    procedure Send(const Value: string);
    procedure SetState;
    Procedure ProcessEvent(txt:string); override;
    procedure Execute; override;
    procedure StarLostTimerTimer(Sender: TObject); override;
    procedure GetInfo;
  public
    Constructor Create;
    Destructor Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cb1:boolean=False); override;
    procedure Disconnect; override;
    procedure Shutdown; override;
    procedure ConnectGear; override;
    procedure SettleTolerance(pixel:double; mintime,maxtime: integer); override;
    procedure Calibrate; override;
    procedure Guide(onoff:boolean; recalibrate:boolean=false); override;
    procedure Pause(onoff:boolean; settle:boolean=true); override;
    procedure Dither(pixel:double; raonly:boolean; waittime:double); override;
    function GetLockPosition(out x,y:double):boolean; override;
    procedure SetLockPosition(x,y: double); override;
    function SpectroSetGuideStar(GuideRa,GuideDec:double):boolean; override;
    function SpectroSetTarget(TargetRa,TargetDec: double):Boolean; override;
    function WaitBusy(maxwait:integer=5):boolean; override;
    function WaitGuiding(maxwait:integer=5):boolean; override;
    function WaitDithering(maxwait:integer=5):boolean; override;
  end;

implementation

Constructor T_autoguider_phd.Create ;
begin
  inherited Create;
  FAutoguiderType:=agPHD;
  FTargetHost:='localhost';
  FTargetPort:='4400';
  FTimeout:=500;
  FArcsecPixel:=1;
end;

Destructor T_autoguider_phd.Destroy;
begin
  inherited Destroy;
end;

Procedure T_autoguider_phd.Connect(cp1: string; cp2:string=''; cp3:string=''; cb1:boolean=False);
begin
  if FRunning then exit;
  FTargetHost:=cp1;
  FTargetPort:=cp2;
  FStatus:='Connecting';
  if cb1 and (FTargetHost='localhost')and(cp3<>'') then begin
     FProgramPath:=ExtractFilePath(cp3);
     FProgramName:=ExtractFileName(cp3);
     if FProgramName<>'' then begin
       FStartedProgram:=StartProgram(FProgramName,FProgramPath);
       if FStartedProgram then wait(5);
     end;
  end;
  Start;
end;

procedure T_autoguider_phd.Disconnect;
begin
  if FStartedProgram then
    Shutdown
  else
    Terminate;
end;

procedure T_autoguider_phd.Execute;
var buf:string;
begin
try
tcpclient:=TTCPClient.Create;
try
 if Terminated then exit;
 tcpclient.TargetHost:=FTargetHost;
 tcpclient.TargetPort:=FTargetPort;
 tcpclient.Timeout := FTimeout;
 // connect
 if tcpclient.Connect then begin
   FRunning:=true;
   if assigned(FonConnect) then FonConnect(self);
   // main loop
   try
   repeat
     if terminated then break;
     buf:=tcpclient.recvstring;
     if (tcpclient.Sock.lastError<>0)and(tcpclient.Sock.lastError<>WSAETIMEDOUT) then break;
     if (buf<>'') then ProcessEvent(buf);
   until false;
   except
   end;
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
except
  on E: Exception do DisplayMessage('Main loop error: '+ E.Message);
end;
end;

procedure T_autoguider_phd.Send(const Value: string);
begin
//writeln('>>'+Value);
try
 if (not terminated)and(tcpclient<>nil)and(Value>'') then begin
   tcpclient.Sock.SendString(Value+crlf);
   if tcpclient.Sock.LastError<>0 then begin
      DisplayMessage(tcpclient.GetErrorDesc);
      Terminate;
   end;
 end;
except
end;
end;

procedure T_autoguider_phd.JsonDataToStringlist(var SK,SV: TStringList; prefix:string; D : TJSONData);
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

procedure T_autoguider_phd.JsonToStringlist(jsontxt:TStringStream; var SK,SV: TStringList);
var  J: TJSONData;
begin
try
J:=GetJSON(jsontxt);
JsonRecurseLevel:=0;
JsonDataToStringlist(SK,SV,'',J);
J.Free;
except
  on E: Exception do DisplayMessage('JsonToStringlist Error: '+ E.Message);
end;
end;

procedure T_autoguider_phd.ProcessEvent(txt:string);
var s:TStringStream;
begin
try
 txt:=StringReplace(txt,chr(10),' ',[rfReplaceAll]);
 txt:=StringReplace(txt,chr(13),' ',[rfReplaceAll]);
 s:=TStringStream.Create(txt);
 Application.QueueAsyncCall(@ProcessEventAsync, PtrInt(s))
 except
   on E: Exception do DisplayMessage('ProcessEvent Error: '+ E.Message);
 end;
end;

procedure T_autoguider_phd.ProcessEventAsync(Data: PtrInt);
var eventname,rpcid,rpcresult,rpcerror,err: string;
    attrib,value:Tstringlist;
    p,q,r,i,s,k:integer;
    radiff, decdiff, buf:string;
begin
try
attrib:=Tstringlist.Create;
value:=Tstringlist.Create;
FLastError:='';
try
JsontoStringlist(TStringStream(Data),attrib,value);
TStringStream(Data).Free;
p:=attrib.IndexOf('Event');    // PHD events
if p>=0 then begin
   eventname:=value[p];
   if (eventname='GuideStep') then begin
     if (FStatus<>'Settling')and(not FDithering) then FStatus:='Guiding';
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
     p:=attrib.IndexOf('RADistanceRaw');
     q:=attrib.IndexOf('DECDistanceRaw');
     r:=attrib.IndexOf('StarMass');
     if (p>=0) and (q>=0) and (r>=0) then
         begin
           GuideStat(-FArcsecPixel*StrToFloatDef(value[p],0),FArcsecPixel*StrToFloatDef(value[q],0),StrToFloatDef(value[r],0));
         end;
     if (not FDithering)and(FStatus<>'Settling')and(FMaxGuideDrift<=99) then
         begin
           p:=attrib.IndexOf('RADistanceGuide');
           q:=attrib.IndexOf('DecDistanceGuide');
           if (p>=0) and (q>=0) then
               begin
                 radiff:=value[p];
                 decdiff:=value[q];

                 if Sqrt(Sqr(Abs(StrToFloatDef(radiff,0)))+Sqr(Abs(StrToFloatDef(decdiff,0))))>FMaxGuideDrift then
                     begin
                          FStatus:='Drift ('+radiff+'/'+decdiff+')';
                          StatusChange;
                          if (FCancelExposure) and (not LockRestartExposure) then
                              begin
                                if FMaxDriftAbort and (FDriftRestartCount>FMaxDriftAbortNum) then begin
                                  // Abort
                                  DisplayMessage('Maximum autoguider drift restart reach.');
                                  AbortTarget;
                                end
                                else begin
                                  // Cancel the exposure and start a new one
                                  DisplayMessage('Restart exposure because of drift.');
                                  inc(FDriftRestartCount);
                                  CancelExposure;
                                end;
                              end;
                     end
                     else
                       FDriftRestartCount:=0;
               end;
         end;

   end
   else if eventname='StartGuiding' then begin
      FStatus:='Start Guiding';
      GetInfo;
   end
   else if eventname='GuidingStopped' then FStatus:='Stopped'
   else if eventname='StarSelected' then FStatus:='Star Selected'
   else if eventname='StarLost' then begin
      i:=attrib.IndexOf('Status');
      if i>=0 then FLastError:='StarLost '+value[i];
      if (FStatus='Guiding') and(not StarLostTimer.Enabled) then begin
        FStarLostTime:=now;
        FStarLostCount:=1;
        StarLostTimer.Enabled:=true;
      end
      else if (FStatus=StarLostStatus)and(StarLostTimer.Enabled) then begin
        inc(FStarLostCount);
      end;
      FStatus:=StarLostStatus;
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
   else if eventname='LoopingExposuresStopped' then FStatus:='Exposures Stopped'
   else if eventname='Settling' then begin
     FStatus:='Settling';
   end
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
     FDithering:=false;
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
     i:=attrib.IndexOf('PHDSubver');
     if i>=0 then begin
        FVersion:=FVersion+value[i];
     end;
     DisplayMessage('PHD version: '+FVersion);
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
     else if rpcid='1001' then begin  // shutdown
       if rpcresult='error' then begin
          DisplayMessage('shutdown'+' '+rpcresult+' '+rpcerror);
       end;
     end
     else if rpcid='1002' then begin  // get_pixel_scale
       if rpcresult='error' then begin
          DisplayMessage('get_pixel_scale'+' '+rpcresult+' '+rpcerror);
          FArcsecPixel:=1;
       end
       else begin
          FArcsecPixel:=StrToFloatDef(rpcresult,1);
       end;
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
     end
     else if rpcid='2011' then begin  // set_lock_position
       if rpcresult='error' then begin
          DisplayMessage('set_lock_position'+' '+rpcresult+' '+rpcerror);
       end;
     end
     else if rpcid='2012' then begin  // get_lock_position
       if rpcresult='error' then begin
          DisplayMessage('get_lock_position'+' '+rpcresult+' '+rpcerror);
       end
       else begin
          i:=attrib.IndexOf('result.0');
          if i>0 then FLockPositionX:=value[i];
          i:=attrib.IndexOf('result.1');
          if i>0 then FLockPositionY:=value[i];
          FLockPositionReceived:=true;
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
except
  on E: Exception do DisplayMessage('ProcessEventAsync Error: '+ E.Message);
end;
end;

procedure T_autoguider_phd.SetState;
begin
  if FStatus='Guiding' then FState:=GUIDER_GUIDING
  else if FStatus='Start Guiding' then FState:=GUIDER_BUSY
  else if FStatus='Stopped' then FState:=GUIDER_IDLE
  else if (FStatus='Star Selected')and(FState=GUIDER_BUSY) then FState:=GUIDER_INITIALIZING
  else if FStatus=StarLostStatus then FState:=GUIDER_ALERT
  else if FStatus='Paused' then FState:=GUIDER_IDLE
  else if FStatus='Resumed' then FState:=FState
  else if FStatus='Lock Position Set' then FState:=FState
  else if FStatus='Calibration Complete' then FState:=GUIDER_IDLE
  else if FStatus='Star Selected' then FState:=FState
  else if FStatus='Start Calibration' then FState:=GUIDER_BUSY
  else if FStatus='Calibration Failed' then FState:=GUIDER_ALERT
  else if FStatus='Calibration Data Flipped' then FState:=FState
  else if (FStatus='Looping Exposures')and(FState=GUIDER_BUSY) then FState:=GUIDER_INITIALIZING
  else if FStatus='Exposures Stopped' then FState:=GUIDER_IDLE
  else if FStatus='Settling' then FState:=FState
  else if FStatus='Settle Done' then FState:=FState
  else if FStatus='Guiding Dithered' then FState:=GUIDER_BUSY
  else if FStatus='Lock Position Lost' then FState:=FState
  else if FStatus='Alert' then FState:=GUIDER_ALERT
  else if copy(FStatus,1,5)='Drift' then FState:=GUIDER_ALERT
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

procedure T_autoguider_phd.Shutdown;
var buf:string;
begin
 buf:='{"method": "shutdown","id":1001}';
 Send(buf);
end;

procedure T_autoguider_phd.GetInfo;
var buf:string;
begin
 buf:='{"method": "get_pixel_scale","id":1002}';
 Send(buf);
end;

procedure T_autoguider_phd.SettleTolerance(pixel:double; mintime,maxtime: integer);
begin
FSettlePix:=pixel;
FSettleTmin:=mintime;
FSettleTmax:=maxtime;
end;

function T_autoguider_phd.WaitBusy(maxwait:integer=5):boolean;
var endt: TDateTime;
begin
result:=false;
try
  endt:=now+maxwait/secperday;
  while now<endt do begin
    Sleep(100);
    if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
    if terminated then break;
    if CancelAutofocus then break;
    if FStopGuiding then break;
    if FState<>GUIDER_BUSY then break;
  end;
  result:=(FState<>GUIDER_BUSY) or FStopGuiding;
except
end;
end;

function T_autoguider_phd.WaitGuiding(maxwait:integer=5):boolean;
var endt: TDateTime;
    n: integer;
begin
result:=false;
try
  endt:=now+maxwait/secperday;
  n:=0;
  while now<endt do begin
    Sleep(100);
    if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
    if terminated then break;
    if CancelAutofocus then break;
    if FStopGuiding then break;
    if FState=GUIDER_GUIDING then break;
    inc(n);
    if ((n mod 150)=0) and assigned(FonShowMessage) then
        FonShowMessage('Waiting for autoguider to start...');
  end;
  result:=(FState=GUIDER_GUIDING);
except
end;
end;

function T_autoguider_phd.WaitDithering(maxwait:integer=5):boolean;
var endt: TDateTime;
begin
result:=false;
try
  endt:=now+maxwait/secperday;
//  FonShowMessage('Enter dithering wait loop,  FDithering='+BoolToStr(FDithering, rsTrue, rsFalse)+' terminated='+BoolToStr(terminated, rsTrue, rsFalse)+' CancelAutofocus='+BoolToStr(CancelAutofocus, rsTrue, rsFalse)+' FStopGuiding='+BoolToStr(FStopGuiding, rsTrue, rsFalse),9);
  while now<endt do begin
    Sleep(100);
    if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
    if terminated then break;
    if CancelAutofocus then break;
    if FStopGuiding then break;
    if not FDithering then break;
  end;
//  FonShowMessage('Exit dithering wait loop,  FDithering='+BoolToStr(FDithering, rsTrue, rsFalse)+' terminated='+BoolToStr(terminated, rsTrue, rsFalse)+' CancelAutofocus='+BoolToStr(CancelAutofocus, rsTrue, rsFalse)+' FStopGuiding='+BoolToStr(FStopGuiding, rsTrue, rsFalse),9);
  result:=(not FDithering);
except
end;
end;

procedure T_autoguider_phd.Calibrate;
begin
  Guide(true,true);
end;

procedure T_autoguider_phd.Guide(onoff:boolean; recalibrate:boolean=false);
var buf:string;
    cguide: boolean;
begin
try
  cguide:=(FState=GUIDER_GUIDING);
  if onoff then begin
    FStopGuiding:=false;
    if (FState=GUIDER_ALERT)or(FState=GUIDER_BUSY) then begin
       // try to reconnect the camera and mount
       ConnectGear;
       Wait(10);
    end;
    Pause(false, false);
    wait(1);
    FStarLostCancelExposure:=config.GetValue('/Autoguider/Recovery/StarLostCancelExposure',0);
    FStarLostTimeoutRestart:=config.GetValue('/Autoguider/Recovery/RestartTimeout',0);
    FStarLostTimeoutCancel:=config.GetValue('/Autoguider/Recovery/CancelTimeout',1800);
    FMaxGuideDrift:=config.GetValue('/Autoguider/Recovery/MaxGuideDrift',100.0);
    FCancelExposure:=config.GetValue('/Autoguider/Recovery/CancelExposure',false);
    FRestartDelay:=config.GetValue('/Autoguider/Recovery/RestartDelay',15);
    FMaxDriftAbort:=config.GetValue('/Autoguider/Recovery/MaxDriftAbort',false);
    FMaxDriftAbortNum:=config.GetValue('/Autoguider/Recovery/MaxDriftAbortNum',10);
    FStarLostCount:=0;
    FDriftRestartCount:=0;
    buf:='{"method": "loop","id":2004}';
    FState:=GUIDER_BUSY;
    Send(buf);
    WaitBusy(60);
    if terminated or FStopGuiding then exit;
    Wait(10); //enough time for a new guide exposure
    if terminated or FStopGuiding then exit;
    buf:='{"method": "find_star","id":2005}';
    FState:=GUIDER_BUSY;
    Send(buf);
    WaitBusy(60);
    if terminated or FStopGuiding then exit;
    Wait(2);
    if terminated or FStopGuiding then exit;
    buf:='{"method": "guide", "params": [';
    buf:=buf+'{"pixels": '+FormatFloat(f1,FSettlePix)+',';      // settle tolerance
    buf:=buf+'"time": '+IntToStr(FSettleTmin)+',';       // min time
    buf:=buf+'"timeout": '+IntToStr(FSettleTmax)+'},';   // max time
    if recalibrate then buf:=buf+'true' else buf:=buf+'false';
    buf:=buf+'],';
    buf:=buf+'"id": 2003}';
    Send(buf);
    if PHD2GuideSetLock then begin
      wait(1);
      SetLockPosition(PHD2GuideLockX,PHD2GuideLockY);
    end;
  end else begin
    AutoguiderAlert:=false;
    FStopGuiding:=true;
    buf:='{"method": "stop_capture","id":2001}';
    Send(buf);
    StarLostTimer.Enabled:=false;
    FRecovering:=false;
  end;
  if cguide<>onoff then FState:=GUIDER_BUSY;
except
end;
end;

procedure T_autoguider_phd.SetLockPosition(x,y:double);
var buf:string;
begin
if FState<>GUIDER_DISCONNECTED then begin
  buf:='{"method": "set_lock_position", "params": [';
  buf:=buf+FormatFloat(f2,x)+',';
  buf:=buf+FormatFloat(f2,y)+',';
  buf:=buf+'true],"id": 2011}';
  Send(buf);
end;
end;

function T_autoguider_phd.GetLockPosition(out x,y:double):boolean;
var buf:string;
    endt: TDateTime;
begin
 FLockPositionReceived:=false;
 FLockPositionX:='';
 FLockPositionY:='';
 buf:='{"method": "get_lock_position","id":2012}';
 Send(buf);
 endt:=now+10/secperday;
 while now<endt do begin
   Sleep(100);
   if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
   if terminated then break;
   if FLockPositionReceived then break;
 end;
 result:=(FLockPositionReceived)and(FLockPositionX<>'')and(FLockPositionY<>'');
 if result then begin
   x:=StrToFloatDef(FLockPositionX,0);
   y:=StrToFloatDef(FLockPositionY,0);
 end;
end;



procedure T_autoguider_phd.Pause(onoff:boolean; settle:boolean=true);
var buf:string;
begin
  FStopGuiding:=false;
  if onoff then begin
    buf:='{"method": "set_paused","params":[true,"full"],"id":2002}';
    Send(buf);
  end else begin
    buf:='{"method": "set_paused","params":[false],"id":2002}';
    Send(buf);
    wait(1);
    if settle then begin
      buf:='{"method": "guide", "params": [';
      buf:=buf+'{"pixels": '+FormatFloat(f1,FSettlePix)+',';     // settle tolerance
      buf:=buf+'"time": '+IntToStr(FSettleTmin)+',';       // min time
      buf:=buf+'"timeout": '+IntToStr(FSettleTmax)+'},';   // max time
      buf:=buf+'false';                          // don't calibrate
      buf:=buf+'],';
      buf:=buf+'"id": 2003}';
      Send(buf);
      FStatus:='Settling';
      FState:=GUIDER_BUSY;
      WaitGuiding(FSettleTmax+5);
    end;
  end;
end;

procedure T_autoguider_phd.Dither(pixel:double; raonly:boolean; waittime:double);
var pix,rao,buf:string;
begin
try
  FStopGuiding:=false;
  pix:=FormatFloat(f1,pixel);
  if raonly then rao:='true' else rao:='false';
  buf:='{"method": "dither", "params": [';
  buf:=buf+pix+',';                    // pixels
  buf:=buf+rao+',';                    // ra only
  buf:=buf+'{"pixels": '+FormatFloat(f1,FSettlePix)+',';      // settle tolerance
  buf:=buf+'"time": '+IntToStr(FSettleTmin)+',';        // min time
  buf:=buf+'"timeout": '+IntToStr(FSettleTmax)+'}],';   // max time
  buf:=buf+'"id": 2010}';
  FState:=GUIDER_BUSY;
  FDithering:=true;
  Send(buf);
except
end;
end;

procedure T_autoguider_phd.StarLostTimerTimer(Sender: TObject);
var losttime: integer;
begin
 if FState=GUIDER_GUIDING then begin
    if assigned(FonShowMessage) then FonShowMessage('Guiding recovered from star lost');
    StarLostTimer.Enabled:=false;
    FStarLostCount:=0;
    FRecovering:=false;
 end
 else begin
    losttime:=round(secperday*(now-FStarLostTime));
    FonShowMessage('Star lost for '+inttostr(losttime)+' seconds...',0);
    if (FStarLostCancelExposure>0)and(FStarLostCount>FStarLostCancelExposure) then begin
      if assigned(FonShowMessage) then FonShowMessage('Cancel exposure after '+inttostr(FStarLostCount)+' frames without guide star',0);
      FStarLostCount:=0;
      CancelExposure;
    end;
    if (FStarLostTimeoutCancel>0)and(losttime>FStarLostTimeoutCancel) then begin
       if assigned(FonShowMessage) then FonShowMessage('Cannot recover from star lost. Stop guiding now.',0);
       StarLostTimer.Enabled:=false;
       FRecovering:=false;
       guide(false);
    end
    else if (FState<>GUIDER_BUSY)and(not FRecovering)and(FStarLostTimeoutRestart>0)and(losttime>FStarLostTimeoutRestart) then begin
       if assigned(FonShowMessage) then FonShowMessage('Try to restart guiding',0);
       StarLostTimer.Enabled:=false;
       FRecovering:=true;
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

function T_autoguider_phd.SpectroSetGuideStar(GuideRa,GuideDec:double): Boolean;
begin
  //Unsupported by phd2
  result:=true;
end;

function T_autoguider_phd.SpectroSetTarget(TargetRa,TargetDec: double):Boolean;
begin
  //Unsupported by phd2
  result:=true;
end;

end.

