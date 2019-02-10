unit cu_ascomrestfocuser;

{$mode objfpc}{$H+}

//{$define debug_ascom}

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
   procedure StatusTimerTimer(sender: TObject);
   function  Connected: boolean;
   function  InterfaceVersion: integer;
 protected
   procedure SetPosition(p:integer); override;
   function  GetPosition:integer; override;
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
  {$ifdef debug_ascom}msg('Interface version = '+inttostr(Result));{$endif}
  except
    result:=1;
  end;
end;

procedure T_ascomrestfocuser.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
begin
  try
  FStatus := devConnecting;
  {$ifdef debug_ascom}msg('Connecting... ');{$endif}
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
    V.Put('link',true)
  else
    V.Put('connected',true);
  if Connected then begin
     V.Timeout:=120000;
     try
     msg('Driver version: '+V.Get('driverversion').AsString,9);
     except
       msg('Error: unknown driver version',9);
     end;
     GetTemperature;
     FhasAbsolutePosition:=GethasAbsolutePositionReal;
     FhasRelativePosition:=GethasRelativePositionReal;
     msg(rsConnected3);
     FStatus := devConnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     CheckTemp:=0;
     StatusTimer.Enabled:=true;
  end
  else begin
     {$ifdef debug_ascom}msg('Not connected');{$endif}
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
     if FInterfaceVersion=1 then
       V.Put('link',false)
     else
       V.Put('connected',false);
   except
     on E: Exception do msg('Disconnection error: ' + E.Message,0);
   end;
end;

function T_ascomrestfocuser.Connected: boolean;
begin
result:=false;
  try
  if FInterfaceVersion=1 then
     result:=V.Get('link').AsBool
  else
     result:=V.Get('connected').AsBool;
  except
   on E: Exception do begin
      {$ifdef debug_ascom}msg('Error Connected : '+ E.Message);{$endif}
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
  end
  else begin
    try
    if hasAbsolutePosition then begin
      p:=GetPosition;
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
       t:=GetTemperature;
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
   StatusTimer.Enabled:=true;
  end;
end;

function T_ascomrestfocuser.WaitFocuserMoving(maxtime:integer):boolean;
var count,maxcount:integer;
begin
 result:=true;
 try
   {$ifdef debug_ascom}msg('Wait moving ... ');{$endif}
   maxcount:=maxtime div waitpoll;
   count:=0;
   while (V.Get('ismoving').AsBool)and(count<maxcount) do begin
      {$ifdef debug_ascom}
      if (count mod 20) = 0 then begin
         msg('Wait moving, IsMoving = '+BoolToStr(V.IsMoving, True));
      end;
      {$endif}
      sleep(waitpoll);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
      inc(count);
   end;
   result:=(count<maxcount);
   {$ifdef debug_ascom}msg('Move completed '+BoolToStr(result, True));{$endif}
 except
  on E: Exception do begin
       {$ifdef debug_ascom}msg('Error IsMoving : '+ E.Message);{$endif}
       result:=false;
    end;
 end;
end;

procedure T_ascomrestfocuser.FocusIn;
begin
 FFocusdirection:=-1;
 FLastDirection:=FocusDirIn;
 {$ifdef debug_ascom}msg('Set direction IN');{$endif}
end;

procedure T_ascomrestfocuser.FocusOut;
begin
 FFocusdirection:=1;
 FLastDirection:=FocusDirOut;
 {$ifdef debug_ascom}msg('Set direction OUT');{$endif}
end;

procedure T_ascomrestfocuser.SetPosition(p:integer);
var n: integer;
begin
   try
   if FPositionRange<>NullRange then
     n:=max(min(p,round(FPositionRange.max)),round(FPositionRange.min))
   else
     n:=p;
   {$ifdef debug_ascom}msg('Move '+inttostr(p)+' '+inttostr(n));{$endif}
   V.Put('move',['Position',IntToStr(n)]);
   FocuserLastTemp:=FocuserTemp;
   WaitFocuserMoving(60000);
   except
    on E: Exception do msg('Error, can''t move to. ' + E.Message,0);
   end;
end;

function  T_ascomrestfocuser.GetPosition:integer;
begin
 result:=0;
   try
   result:=V.Get('position').AsInt;
   {$ifdef debug_ascom}
   if FmsgAPos<>Result then begin
     msg('Position = '+inttostr(Result));
     FmsgAPos:=Result;
   end;
   {$endif}
   except
    on E: Exception do msg('Get position error: ' + E.Message,0);
   end;
end;

function  T_ascomrestfocuser.GetPositionRange: TNumRange;
begin
 result:=NullRange;
   if FPositionRange=NullRange then begin
     try
     result.min:=0;
     result.max:=V.Get('maxstep').AsFloat;
     result.step:=1;
     FPositionRange:=result;
     {$ifdef debug_ascom}msg('Position range = '+FormatFloat(f0,FPositionRange.min)+' '+FormatFloat(f0,FPositionRange.max)+' '+FormatFloat(f0,FPositionRange.step) );{$endif}
     except
       on E: Exception do begin
          {$ifdef debug_ascom}msg('Error MaxStep : '+ E.Message);{$endif}
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
   if FRelPositionRange=NullRange then begin
     try
     result.min:=0;
     result.max:=V.Get('maxstep').AsFloat;
     result.step:=1;
     FRelPositionRange:=result;
     {$ifdef debug_ascom}msg('Relative position range = '+FormatFloat(f0,FRelPositionRange.min)+' '+FormatFloat(f0,FRelPositionRange.max)+' '+FormatFloat(f0,FRelPositionRange.step) );{$endif}
     except
       on E: Exception do begin
          {$ifdef debug_ascom}msg('Error MaxStep : '+ E.Message);{$endif}
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
   try
   if FRelPositionRange<>NullRange then
     FRelIncr:=max(min(p,round(FRelPositionRange.max)),round(FRelPositionRange.min))
   else
     FRelIncr:=p;
   i:=FFocusdirection*FRelIncr;
   {$ifdef debug_ascom}msg('Relative move '+inttostr(p)+' '+inttostr(i));{$endif}
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
   try
   result:=V.Get('absolute').AsBool;
   {$ifdef debug_ascom}msg('Use AbsolutePosition: '+BoolToStr(result, True));{$endif}
   except
    on E: Exception do msg('GethasAbsolutePosition error: ' + E.Message,0);
   end;
end;

function  T_ascomrestfocuser.GethasAbsolutePosition: boolean;
begin
 result:=False;
 result:=FhasAbsolutePosition;
end;

function  T_ascomrestfocuser.GethasRelativePositionReal: boolean;
begin
 result:=False;
   try
   result:=not V.Get('absolute').AsBool;
   {$ifdef debug_ascom}msg('Use RelativePosition: '+BoolToStr(result, True));{$endif}
   except
    on E: Exception do msg('GethasRelativePosition error: ' + E.Message,0);
   end;
end;

function  T_ascomrestfocuser.GethasRelativePosition: boolean;
begin
 result:=False;
 result:=FhasRelativePosition;
end;

function  T_ascomrestfocuser.GethasTimerSpeed: boolean;
begin
 result:=false;
end;

procedure T_ascomrestfocuser.SetTimeout(num:integer);
begin
 FTimeOut:=num;
 {$ifdef debug_ascom}msg('Set timeout: '+inttostr(num));{$endif}
end;

function  T_ascomrestfocuser.GetTemperature:double;
{$ifdef debug_ascom}
var i: integer;
{$endif}
begin
 result:=0;
   try
   result:= V.Get('temperature').AsFloat;
   FhasTemperature:=true;
   {$ifdef debug_ascom}
   i:=round(10*Result);
   if FmsgTemp<>i then begin
      msg('Temperature: '+FormatFloat(f1,result));
      FmsgTemp:=i;
   end;
   {$endif}
   except
    result:=0;
    FhasTemperature:=false;
    {$ifdef debug_ascom}msg('No temperature');{$endif}
   end;
end;

end.

