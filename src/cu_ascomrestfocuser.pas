unit cu_ascomrestfocuser;

{$mode objfpc}{$H+}

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

interface

uses cu_focuser, cu_ascomrest, u_global,
    u_translation, u_utils, indiapi, math,
    Forms, ExtCtrls,Classes, SysUtils;

type
T_ascomrestfocuser = class(T_focuser)
 private
   V: TAscomRest;
   stPosition,CheckTemp: integer;
   stTemperature: double;
   FPositionRange: TNumRange;
   FRelPositionRange: TNumRange;
   FInterfaceVersion: integer;
   FhasAbsolutePosition,FhasRelativePosition: boolean;
   FRelIncr: integer;
   FmsgTemp,FmsgAPos,FmsgRPos: integer;
   StatusTimer: TTimer;
   FDeviceName: string;
   procedure StatusTimerTimer(sender: TObject);
   function  Connected: boolean;
   function  InterfaceVersion: integer;
   function  DeviceName: string;
 protected
   procedure SetPosition(p:integer); override;
   function  GetPosition:integer; override;
   function  GetPositionReal:integer;
   procedure SetRelPosition(p:integer); override;
   function  GetRelPosition:integer; override;
   procedure SetSpeed(p:integer); override;
   function  GetSpeed:integer; override;
   procedure SetTimer(p:integer); override;
   function  GetTimer:integer; override;
   function  GethasAbsolutePositionReal: boolean;
   function  GethasAbsolutePosition: boolean; override;
   function  GethasRelativePositionReal: boolean;
   function  GethasRelativePosition: boolean; override;
   function  GethasTimerSpeed: boolean; override;
   function  GetPositionRange: TNumRange; override;
   function  GetRelPositionRange: TNumRange; override;
   procedure SetTimeout(num:integer); override;
   function  WaitFocuserMoving(maxtime:integer):boolean;
   function  GetTemperature:double; override;
   function  GetTemperatureReal:double;
public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');  override;
   procedure Disconnect; override;
   procedure FocusIn; override;
   procedure FocusOut; override;
end;

const waitpoll=1000;
      statusinterval=3000;

implementation

constructor T_ascomrestfocuser.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 V:=TAscomRest.Create(self);
 V.ClientId:=3202;
 FFocuserInterface:=ASCOMREST;
 FInterfaceVersion:=1;
 FPositionRange:=NullRange;
 FRelPositionRange:=NullRange;
 FhasAbsolutePosition:=false;
 FhasRelativePosition:=false;
 FmsgTemp:=9999;
 FmsgAPos:=-1;
 FmsgRPos:=-1;
 stPosition:=NullInt;
 stTemperature:=NullCoord;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=statusinterval;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomrestfocuser.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

function  T_ascomrestfocuser.InterfaceVersion: integer;
begin
 result:=1;
  try
   result:=V.Get('interfaceversion').AsInt;
  if debug_ascom then msg('Interface version = '+inttostr(Result));
  except
    result:=1;
  end;
end;

function  T_ascomrestfocuser.DeviceName: string;
begin
 result:='';
  try
   result:=V.Get('name').AsString;
  except
    result:='';
  end;
end;

procedure T_ascomrestfocuser.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
begin
  try
  FStatus := devConnecting;
  if debug_ascom then msg('Connecting... ');
  V.Host:=cp1;
  V.Port:=cp2;
  V.Protocol:=cp3;
  V.User:=cp5;
  V.Password:=cp6;
  Fdevice:=cp4;
  V.Device:=Fdevice;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  V.Timeout:=2000;
  FInterfaceVersion:=InterfaceVersion;
  if FInterfaceVersion=1 then
    raise Exception.Create('IFocuser V1 is not supported');
  V.Put('Connected',true);
  if Connected then begin
     FDeviceName:=DeviceName;
     V.Timeout:=120000;
     try
     msg('Driver version: '+V.Get('driverversion').AsString,9);
     except
       msg('Error: unknown driver version',9);
     end;
     FStatus := devConnected;
     GetTemperature;
     FhasAbsolutePosition:=GethasAbsolutePositionReal;
     FhasRelativePosition:=GethasRelativePositionReal;
     msg(rsConnected3);
     if Assigned(FonStatusChange) then FonStatusChange(self);
     CheckTemp:=0;
     StatusTimer.Enabled:=true;
  end
  else begin
     if debug_ascom then msg('Not connected');
     Disconnect;
  end;
  except
    on E: Exception do begin
       msg(Format(rsConnectionEr, [E.Message]),0);
       Disconnect;
    end;
  end;
end;

procedure T_ascomrestfocuser.Disconnect;
begin
   StatusTimer.Enabled:=false;
   FStatus := devDisconnected;
   if Assigned(FonStatusChange) then FonStatusChange(self);
   try
     msg(rsDisconnected3,0);
     // the server is responsible for device disconnection
   except
     on E: Exception do msg('Disconnection error: ' + E.Message,0);
   end;
end;

function T_ascomrestfocuser.Connected: boolean;
begin
result:=false;
  try
  result:=V.Get('connected').AsBool;
  except
   on E: Exception do begin
      if debug_ascom then msg('Error Connected : '+ E.Message);
      result:=false;
   end;
  end;
end;

procedure T_ascomrestfocuser.StatusTimerTimer(sender: TObject);
var t: double;
    p: integer;
begin
 StatusTimer.Enabled:=false;
 try
  if not Connected then begin
     FStatus := devDisconnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     msg(rsDisconnected3,0);
  end
  else begin
    try
    if hasAbsolutePosition then begin
      p:=GetPositionReal;
      if p<>stPosition then begin
        stPosition:=p;
        if Assigned(FonPositionChange) then FonPositionChange(p);
      end;
    end else begin
      p:=FRelIncr;
      if p<>stPosition then begin
        stPosition:=p;
        if Assigned(FonPositionChange) then FonPositionChange(p);
      end;
    end;
    if hasTemperature then begin
      if (CheckTemp mod 10) = 0 then begin
       CheckTemp:=0;
       t:=GetTemperatureReal;
       if abs(t-stTemperature)>0.1 then begin
          stTemperature:=t;
          if Assigned(FonTemperatureChange) then FonTemperatureChange(t);
       end;
      end;
      inc(CheckTemp);
    end;
    except
     on E: Exception do msg('Status error: ' + E.Message,0);
    end;
  end;
  finally
   if FStatus=devConnected then StatusTimer.Enabled:=true;
  end;
end;

function T_ascomrestfocuser.WaitFocuserMoving(maxtime:integer):boolean;
var count,maxcount:integer;
begin
 result:=true;
 if FStatus<>devConnected then exit;
 try
   if debug_ascom then msg('Wait moving ... ');
   maxcount:=maxtime div waitpoll;
   count:=0;
   while (V.Get('ismoving').AsBool)and(count<maxcount) do begin
      if debug_ascom then begin
      if (count mod 20) = 0 then begin
         msg('Wait moving');
      end;
      end;
      sleep(waitpoll);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
      inc(count);
   end;
   result:=(count<maxcount);
   if debug_ascom then msg('Move completed '+BoolToStr(result, True));
 except
  on E: Exception do begin
       if debug_ascom then msg('Error IsMoving : '+ E.Message);
       result:=false;
    end;
 end;
end;

procedure T_ascomrestfocuser.FocusIn;
begin
 if FStatus<>devConnected then exit;
 FFocusdirection:=-1;
 FLastDirection:=FocusDirIn;
 if debug_ascom then msg('Set direction IN');
end;

procedure T_ascomrestfocuser.FocusOut;
begin
 if FStatus<>devConnected then exit;
 FFocusdirection:=1;
 FLastDirection:=FocusDirOut;
 if debug_ascom then msg('Set direction OUT');
end;

procedure T_ascomrestfocuser.SetPosition(p:integer);
var n,np: integer;
begin
 if FStatus<>devConnected then exit;
   try
   if FPositionRange<>NullRange then
     n:=max(min(p,round(FPositionRange.max)),round(FPositionRange.min))
   else
     n:=p;
   if debug_ascom then msg('Move '+inttostr(p)+' '+inttostr(n));
   V.Put('move',['Position',IntToStr(n)]);
   FocuserLastTemp:=FocuserTemp;
   WaitFocuserMoving(60000);
   stPosition:=GetPositionReal;
   // Fix for usb-focus
   if pos('USB_Focus',FDeviceName)>0 then begin
     np:=stPosition;
     if (np<>n) then begin
       msg('Error, new position is '+IntToStr(np)+' instead of '+IntToStr(n),0);
     end; {fix for some poor written focuser drivers. The getposition is already sufficient to fix the problem, so message should never occur.}
   end;
   except
    on E: Exception do msg('Error, can''t move to. ' + E.Message,0);
   end;
end;

function  T_ascomrestfocuser.GetPosition:integer;
begin
 result:=0;
 if FStatus<>devConnected then exit;
 if stPosition=NullInt then stPosition:=GetPositionReal;
 result:=stPosition;
end;

function  T_ascomrestfocuser.GetPositionReal:integer;
begin
 result:=0;
 if FStatus<>devConnected then exit;
   try
   result:=V.Get('position').AsInt;
   if debug_ascom then begin
   if FmsgAPos<>Result then begin
     msg('Position = '+inttostr(Result));
     FmsgAPos:=Result;
   end;
   end;
   except
    on E: Exception do msg('Get position error: ' + E.Message,0);
   end;
end;

function  T_ascomrestfocuser.GetPositionRange: TNumRange;
begin
 result:=NullRange;
 if FStatus<>devConnected then exit;
   if FPositionRange=NullRange then begin
     try
     result.min:=0;
     result.max:=V.Get('maxstep').AsFloat;
     result.step:=1;
     FPositionRange:=result;
     if debug_ascom then msg('Position range = '+FormatFloat(f0,FPositionRange.min)+' '+FormatFloat(f0,FPositionRange.max)+' '+FormatFloat(f0,FPositionRange.step) );
     except
       on E: Exception do begin
          if debug_ascom then msg('Error MaxStep : '+ E.Message);
          result:=NullRange;
       end;
     end;
   end
   else
     result:=FPositionRange;
end;

function  T_ascomrestfocuser.GetRelPositionRange: TNumRange;
begin
 result:=NullRange;
   if FStatus<>devConnected then exit;
   if FRelPositionRange=NullRange then begin
     try
     result.min:=0;
     result.max:=V.Get('maxstep').AsFloat;
     result.step:=1;
     FRelPositionRange:=result;
     if debug_ascom then msg('Relative position range = '+FormatFloat(f0,FRelPositionRange.min)+' '+FormatFloat(f0,FRelPositionRange.max)+' '+FormatFloat(f0,FRelPositionRange.step) );
     except
       on E: Exception do begin
          if debug_ascom then msg('Error MaxStep : '+ E.Message);
          result:=NullRange;
       end;
     end;
   end
   else
     result:=FRelPositionRange;
end;

procedure T_ascomrestfocuser.SetRelPosition(p:integer);
var i: integer;
begin
   if FStatus<>devConnected then exit;
   try
   if FRelPositionRange<>NullRange then
     FRelIncr:=max(min(p,round(FRelPositionRange.max)),round(FRelPositionRange.min))
   else
     FRelIncr:=p;
   i:=FFocusdirection*FRelIncr;
   if debug_ascom then msg('Relative move '+inttostr(p)+' '+inttostr(i));
   V.Put('move',['Position',IntToStr(i)]);
   FocuserLastTemp:=FocuserTemp;
   WaitFocuserMoving(60000);
   if FDelay>0 then Wait(FDelay);
   except
    on E: Exception do msg('Set relative position error: ' + E.Message,0);
   end;
end;

function  T_ascomrestfocuser.GetRelPosition:integer;
begin
 if FStatus<>devConnected then exit;
 result:=FRelIncr;
 if FmsgRPos<>Result then begin
    msg('Relative position = '+inttostr(Result));
    FmsgRPos:=Result;
 end;
end;

procedure T_ascomrestfocuser.SetSpeed(p:integer);
begin
  // not implemented in ASCOM
end;

function  T_ascomrestfocuser.GetSpeed:integer;
begin
 // not implemented in ASCOM
 result:=0;
end;

procedure T_ascomrestfocuser.SetTimer(p:integer);
begin
 // not implemented in ASCOM
end;

function  T_ascomrestfocuser.GetTimer:integer;
begin
 // not implemented in ASCOM
 result:=0;
end;

function  T_ascomrestfocuser.GethasAbsolutePositionReal: boolean;
begin
 result:=False;
 if FStatus<>devConnected then exit;
   try
   result:=V.Get('absolute').AsBool;
   if debug_ascom then msg('Use AbsolutePosition: '+BoolToStr(result, True));
   except
    on E: Exception do msg('GethasAbsolutePosition error: ' + E.Message,0);
   end;
end;

function  T_ascomrestfocuser.GethasAbsolutePosition: boolean;
begin
 result:=False;
 if FStatus<>devConnected then exit;
 result:=FhasAbsolutePosition;
end;

function  T_ascomrestfocuser.GethasRelativePositionReal: boolean;
begin
 result:=False;
 if FStatus<>devConnected then exit;
   try
   result:=not V.Get('absolute').AsBool;
   if debug_ascom then msg('Use RelativePosition: '+BoolToStr(result, True));
   except
    on E: Exception do msg('GethasRelativePosition error: ' + E.Message,0);
   end;
end;

function  T_ascomrestfocuser.GethasRelativePosition: boolean;
begin
 result:=False;
 if FStatus<>devConnected then exit;
 result:=FhasRelativePosition;
end;

function  T_ascomrestfocuser.GethasTimerSpeed: boolean;
begin
 result:=false;
end;

procedure T_ascomrestfocuser.SetTimeout(num:integer);
begin
 FTimeOut:=num;
 if debug_ascom then msg('Set timeout: '+inttostr(num));
end;

function  T_ascomrestfocuser.GetTemperature:double;
begin
 result:=0;
 if FStatus<>devConnected then exit;
 if stTemperature=NullCoord then stTemperature:=GetTemperatureReal;
 result:=stTemperature;
end;

function  T_ascomrestfocuser.GetTemperatureReal:double;
var i: integer;
begin
 result:=0;
 if FStatus<>devConnected then exit;
   try
   result:= V.Get('temperature').AsFloat;
   FhasTemperature:=true;
   if debug_ascom then begin
   i:=round(10*Result);
   if FmsgTemp<>i then begin
      msg('Temperature: '+FormatFloat(f1,result));
      FmsgTemp:=i;
   end;
   end;
   except
    result:=0;
    FhasTemperature:=false;
    if debug_ascom then msg('No temperature');
   end;
end;

end.

