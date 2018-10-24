unit cu_ascomfocuser;

{$mode objfpc}{$H+}

//{$define debug_ascom}

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

uses cu_focuser, u_global,
    {$ifdef mswindows}
    u_translation, u_utils, indiapi, Variants, comobj, math,
    {$endif}
    Forms, ExtCtrls,Classes, SysUtils;

type
T_ascomfocuser = class(T_focuser)
 private
   {$ifdef mswindows}
   V: variant;
   stPosition: integer;
   stTemperature: double;
   {$endif}
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
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');  override;
   procedure Disconnect; override;
   procedure FocusIn; override;
   procedure FocusOut; override;
end;


implementation

constructor T_ascomfocuser.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FFocuserInterface:=ASCOM;
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
 StatusTimer.Interval:=1000;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomfocuser.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

function  T_ascomfocuser.InterfaceVersion: integer;
begin
 result:=1;
 {$ifdef mswindows}
  try
  if not VarIsEmpty(V) then begin
   result:=V.InterfaceVersion;
  end;
  {$ifdef debug_ascom}msg('Interface version = '+inttostr(Result));{$endif}
  except
    result:=1;
  end;
 {$endif}
end;

procedure T_ascomfocuser.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');
begin
 {$ifdef mswindows}
  try
  FStatus := devConnecting;
  {$ifdef debug_ascom}msg('Connecting... ');{$endif}
  Fdevice:=cp1;
  V:=Unassigned;
  V:=CreateOleObject(Fdevice);
  FInterfaceVersion:=InterfaceVersion;
  if FInterfaceVersion=1 then
    V.Link:=true
  else
    V.Connected:=true;
  if Connected then begin
     GetTemperature;
     FhasAbsolutePosition:=GethasAbsolutePositionReal;
     FhasRelativePosition:=GethasRelativePositionReal;
     msg(rsConnected3);
     FStatus := devConnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     StatusTimer.Enabled:=true;
  end
  else begin
     {$ifdef debug_ascom}msg('Not connected');{$endif}
     Disconnect;
  end;
  except
    on E: Exception do msg('Connection error: ' + E.Message,0);
  end;
 {$endif}
end;

procedure T_ascomfocuser.Disconnect;
begin
 {$ifdef mswindows}
   StatusTimer.Enabled:=false;
   FStatus := devDisconnected;
   if Assigned(FonStatusChange) then FonStatusChange(self);
   try
   if not VarIsEmpty(V) then begin
     msg(rsDisconnected3,0);
     if FInterfaceVersion=1 then
       V.Link:=false
     else
       V.Connected:=false;
     V:=Unassigned;
   end;
   except
     on E: Exception do msg('Disconnection error: ' + E.Message,0);
   end;
 {$endif}
end;

function T_ascomfocuser.Connected: boolean;
begin
result:=false;
{$ifdef mswindows}
if not VarIsEmpty(V) then begin
  try
  if FInterfaceVersion=1 then
     result:=V.Link
  else
     result:=V.connected;
  except
   on E: Exception do begin
      {$ifdef debug_ascom}msg('Error Connected : '+ E.Message);{$endif}
      result:=false;
   end;
  end;
end;
{$endif}
end;

procedure T_ascomfocuser.StatusTimerTimer(sender: TObject);
{$ifdef mswindows}
var t: double;
    p: integer;
{$endif}
begin
 {$ifdef mswindows}
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
       t:=GetTemperature;
       if abs(t-stTemperature)>0.1 then begin
          stTemperature:=t;
          if Assigned(FonTemperatureChange) then FonTemperatureChange(t);
       end;
    end;
    except
     on E: Exception do msg('Status error: ' + E.Message,0);
    end;
  end;
 {$endif}
end;

function T_ascomfocuser.WaitFocuserMoving(maxtime:integer):boolean;
{$ifdef mswindows}
var count,maxcount:integer;
{$endif}
begin
 result:=true;
 {$ifdef mswindows}
 try
 if Connected then begin
   {$ifdef debug_ascom}msg('Wait moving ... ');{$endif}
   maxcount:=maxtime div 100;
   count:=0;
   while (V.IsMoving)and(count<maxcount) do begin
      {$ifdef debug_ascom}
      if (count mod 20) = 0 then begin
         msg('Wait moving, IsMoving = '+BoolToStr(V.IsMoving, True));
      end;
      {$endif}
      sleep(100);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
      inc(count);
   end;
   result:=(count<maxcount);
   {$ifdef debug_ascom}msg('Move completed '+BoolToStr(result, True));{$endif}
 end;
 except
  on E: Exception do begin
       {$ifdef debug_ascom}msg('Error IsMoving : '+ E.Message);{$endif}
       result:=false;
    end;
 end;
 {$endif}
end;

procedure T_ascomfocuser.FocusIn;
begin
 {$ifdef mswindows}
 FFocusdirection:=-1;
 FLastDirection:=FocusDirIn;
 {$ifdef debug_ascom}msg('Set direction IN');{$endif}
 {$endif}
end;

procedure T_ascomfocuser.FocusOut;
begin
 {$ifdef mswindows}
 FFocusdirection:=1;
 FLastDirection:=FocusDirOut;
 {$ifdef debug_ascom}msg('Set direction OUT');{$endif}
 {$endif}
end;

procedure T_ascomfocuser.SetPosition(p:integer);
{$ifdef mswindows}
var n: integer;
{$endif}
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   if FPositionRange<>NullRange then
     n:=max(min(p,round(FPositionRange.max)),round(FPositionRange.min))
   else
     n:=p;
   {$ifdef debug_ascom}msg('Move '+inttostr(p)+' '+inttostr(n));{$endif}
   V.Move(n);
   FocuserLastTemp:=FocuserTemp;
   WaitFocuserMoving(60000);
   except
    on E: Exception do msg('Error, can''t move to. ' + E.Message,0);
   end;
 end;
 {$endif}
end;

function  T_ascomfocuser.GetPosition:integer;
begin
 result:=0;
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=V.Position;
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
 {$endif}
end;

function  T_ascomfocuser.GetPositionRange: TNumRange;
begin
 result:=NullRange;
 {$ifdef mswindows}
 if Connected then begin
   if FPositionRange=NullRange then begin
     try
     result.min:=0;
     result.max:=V.MaxStep;
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
 end
 else result:=NullRange;
 {$endif}
end;

function  T_ascomfocuser.GetRelPositionRange: TNumRange;
begin
 result:=NullRange;
 {$ifdef mswindows}
 if Connected then begin
   if FRelPositionRange=NullRange then begin
     try
     result.min:=0;
     result.max:=V.MaxStep;
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
 end
 else result:=NullRange;
 {$endif}
end;

procedure T_ascomfocuser.SetRelPosition(p:integer);
{$ifdef mswindows}
var i: integer;
{$endif}
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   if FRelPositionRange<>NullRange then
     FRelIncr:=max(min(p,round(FRelPositionRange.max)),round(FRelPositionRange.min))
   else
     FRelIncr:=p;
   i:=FFocusdirection*FRelIncr;
   {$ifdef debug_ascom}msg('Relative move '+inttostr(p)+' '+inttostr(i));{$endif}
   V.Move(i);
   FocuserLastTemp:=FocuserTemp;
   WaitFocuserMoving(60000);
   if FDelay>0 then Wait(FDelay);
   except
    on E: Exception do msg('Set relative position error: ' + E.Message,0);
   end;
 end;
 {$endif}
end;

function  T_ascomfocuser.GetRelPosition:integer;
begin
 result:=FRelIncr;
 {$ifdef debug_ascom}
 if FmsgRPos<>Result then begin
    msg('Relative position = '+inttostr(Result));
    FmsgRPos:=Result;
 end;
 {$endif}
end;

procedure T_ascomfocuser.SetSpeed(p:integer);
begin
  // not implemented in ASCOM
end;

function  T_ascomfocuser.GetSpeed:integer;
begin
 // not implemented in ASCOM
 result:=0;
end;

procedure T_ascomfocuser.SetTimer(p:integer);
begin
 // not implemented in ASCOM
end;

function  T_ascomfocuser.GetTimer:integer;
begin
 // not implemented in ASCOM
 result:=0;
end;

function  T_ascomfocuser.GethasAbsolutePositionReal: boolean;
begin
 result:=False;
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=V.Absolute;
   {$ifdef debug_ascom}msg('Use AbsolutePosition: '+BoolToStr(result, True));{$endif}
   except
    on E: Exception do msg('GethasAbsolutePosition error: ' + E.Message,0);
   end;
 end;
 {$endif}
end;

function  T_ascomfocuser.GethasAbsolutePosition: boolean;
begin
 result:=False;
 {$ifdef mswindows}
 if Connected then begin
   result:=FhasAbsolutePosition;
 end;
 {$endif}
end;

function  T_ascomfocuser.GethasRelativePositionReal: boolean;
begin
 result:=False;
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=not V.Absolute;
   {$ifdef debug_ascom}msg('Use RelativePosition: '+BoolToStr(result, True));{$endif}
   except
    on E: Exception do msg('GethasRelativePosition error: ' + E.Message,0);
   end;
 end;
 {$endif}
end;

function  T_ascomfocuser.GethasRelativePosition: boolean;
begin
 result:=False;
 {$ifdef mswindows}
 if Connected then begin
   result:=FhasRelativePosition;
 end;
 {$endif}
end;

function  T_ascomfocuser.GethasTimerSpeed: boolean;
begin
 result:=false;
end;

procedure T_ascomfocuser.SetTimeout(num:integer);
begin
 FTimeOut:=num;
 {$ifdef debug_ascom}msg('Set timeout: '+inttostr(num));{$endif}
end;

function  T_ascomfocuser.GetTemperature:double;
{$ifdef debug_ascom}
var i: integer;
{$endif}
begin
 result:=0;
 {$ifdef mswindows}
 if Connected then begin
   try
   result:= V.Temperature;
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
 {$endif}
end;

end.

