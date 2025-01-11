unit cu_ascomfocuser;

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
   function WaitConnecting(maxtime:integer):boolean;
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
   function GetV: variant;
   procedure FocusIn; override;
   procedure FocusOut; override;
end;

const waitpoll=500;
      statusinterval=2000;

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
 StatusTimer.Interval:=statusinterval;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomfocuser.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

procedure T_ascomfocuser.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
begin
 {$ifdef mswindows}
  try
  FStatus := devConnecting;
  if debug_msg then msg('Connecting... ');
  Fdevice:=cp1;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  V:=Unassigned;
  V:=CreateOleObject(Fdevice);
  try
  FInterfaceVersion:=V.InterfaceVersion;
  except
    FInterfaceVersion:=1;
  end;
  msg('Interface version: '+inttostr(FInterfaceVersion),9);
  if FInterfaceVersion>=4 then begin
    V.Connect;
    WaitConnecting(30000);
  end
  else if FInterfaceVersion>=2 then
    V.Connected:=true
  else
    V.Link:=true;
  if Connected then begin
     try
     msg(V.DriverInfo,9);
     except
     end;
     try
     msg('Driver version: '+V.DriverVersion,9);
     except
       msg('Error: unknown driver version',9);
     end;
     GetTemperatureInt;
     FhasAbsolutePosition:=GethasAbsolutePositionReal;
     FhasRelativePosition:=GethasRelativePositionReal;
     msg(rsConnected3);
     FStatus := devConnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     StatusTimer.Enabled:=true;
  end
  else begin
     if debug_msg then msg('Not connected');
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
   try
   if not VarIsEmpty(V) then begin
     if FInterfaceVersion>=4 then begin
       V.Disconnect;
       WaitConnecting(30000);
     end
     else if FInterfaceVersion>=2 then
       V.Connected:=false
     else
       V.Link:=false;
     V:=Unassigned;
   end;
   except
     on E: Exception do msg('Disconnection error: ' + E.Message,0);
   end;
   FStatus := devDisconnected;
   if Assigned(FonStatusChange) then FonStatusChange(self);
   msg(rsDisconnected3,1);
 {$endif}
end;

function T_ascomfocuser.GetV: variant;
begin
 {$ifdef mswindows}
 result:=V;
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
      if debug_msg then msg('Error Connected : '+ E.Message);
      result:=false;
   end;
  end;
end;
{$endif}
end;

function T_ascomfocuser.WaitConnecting(maxtime:integer):boolean;
{$ifdef mswindows}
var count,maxcount:integer;
{$endif}
begin
 result:=true;
 {$ifdef mswindows}
 try
   maxcount:=maxtime div waitpoll;
   count:=0;
   while (V.Connecting)and(count<maxcount) do begin
      sleep(waitpoll);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
      inc(count);
   end;
   result:=(count<maxcount);
   if debug_msg then msg('finish to wait for connecting '+BoolToStr(result,true),9);
 except
   result:=false;
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
 try
 StatusTimer.Enabled:=false;
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
       t:=GetTemperatureInt;
       if abs(t-stTemperature)>0.1 then begin
          stTemperature:=t;
          if Assigned(FonTemperatureChange) then FonTemperatureChange(t);
       end;
    end;
    except
     on E: Exception do msg('Status error: ' + E.Message,0);
    end;
  end;
  finally
  if FStatus=devConnected then StatusTimer.Enabled:=true;
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
   if debug_msg then msg('Wait moving ... ');
   maxcount:=maxtime div waitpoll;
   count:=0;
   while (V.IsMoving)and(count<maxcount) do begin
      if debug_msg then begin
      if (count mod 20) = 0 then begin
         msg('Wait moving, IsMoving = '+BoolToStr(V.IsMoving, True));
      end;
      end;
      sleep(waitpoll);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
      inc(count);
   end;
   result:=(count<maxcount);
   if debug_msg then msg('Move completed '+BoolToStr(result, True));
 except
  on E: Exception do begin
       if debug_msg then msg('Error IsMoving : '+ E.Message);
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
 if debug_msg then msg('Set direction IN');
 {$endif}
end;

procedure T_ascomfocuser.FocusOut;
begin
 {$ifdef mswindows}
 FFocusdirection:=1;
 FLastDirection:=FocusDirOut;
 if debug_msg then msg('Set direction OUT');
 {$endif}
end;

procedure T_ascomfocuser.SetPosition(p:integer);
{$ifdef mswindows}
var np: integer;
{$endif}
begin
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
   try
   if PositionRange<>NullRange then begin
      if (p>FPositionRange.max)or(p<FPositionRange.min) then begin
        msg('Invalid position request: '+inttostr(p),1);
        exit;
      end;
   end;
   if debug_msg then msg('Move '+inttostr(p));
   V.Move(p);
   FocuserLastTemp:=FocuserTemp;
   WaitFocuserMoving(60000);
   // Fix for usb-focus
   if Fdevice='ASCOM.USB_Focus.Focuser' then begin
     np:=GetPosition;
     if (np<>p) then begin
       msg('Error, new position is '+IntToStr(np)+' instead of '+IntToStr(p),0);
     end; {fix for some poor written focuser drivers. The getposition is already sufficient to fix the problem, so message should never occur.}
   end;
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
 if not VarIsEmpty(V) then begin
   try
   result:=V.Position;
   if debug_msg then begin
   if FmsgAPos<>Result then begin
     msg('Position = '+inttostr(Result));
     FmsgAPos:=Result;
   end;
   end;
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
 if not VarIsEmpty(V) then begin
   if FPositionRange=NullRange then begin
     try
     result.min:=0;
     result.max:=V.MaxStep;
     result.step:=1;
     FPositionRange:=result;
     if debug_msg then msg('Position range = '+FormatFloat(f0,FPositionRange.min)+' '+FormatFloat(f0,FPositionRange.max)+' '+FormatFloat(f0,FPositionRange.step) );
     except
       on E: Exception do begin
          if debug_msg then msg('Error MaxStep : '+ E.Message);
          result:=NullRange;
       end;
     end;
   end
   else
     result:=FPositionRange;
 end;
 {$endif}
end;

function  T_ascomfocuser.GetRelPositionRange: TNumRange;
begin
 result:=NullRange;
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
   if FRelPositionRange=NullRange then begin
     try
     result.min:=0;
     result.max:=V.MaxStep;
     result.step:=1;
     FRelPositionRange:=result;
     if debug_msg then msg('Relative position range = '+FormatFloat(f0,FRelPositionRange.min)+' '+FormatFloat(f0,FRelPositionRange.max)+' '+FormatFloat(f0,FRelPositionRange.step) );
     except
       on E: Exception do begin
          if debug_msg then msg('Error MaxStep : '+ E.Message);
          result:=NullRange;
       end;
     end;
   end
   else
     result:=FRelPositionRange;
 end;
 {$endif}
end;

procedure T_ascomfocuser.SetRelPosition(p:integer);
{$ifdef mswindows}
var i: integer;
{$endif}
begin
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
   try
   if FRelPositionRange<>NullRange then
     FRelIncr:=max(min(p,round(FRelPositionRange.max)),round(FRelPositionRange.min))
   else
     FRelIncr:=p;
   i:=FFocusdirection*FRelIncr;
   if debug_msg then msg('Relative move '+inttostr(p)+' '+inttostr(i));
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
 if debug_msg then begin
 if FmsgRPos<>Result then begin
    msg('Relative position = '+inttostr(Result));
    FmsgRPos:=Result;
 end;
 end;
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
 if not VarIsEmpty(V) then begin
   try
   result:=V.Absolute;
   if debug_msg then msg('Use AbsolutePosition: '+BoolToStr(result, True));
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
 result:=FhasAbsolutePosition;
 {$endif}
end;

function  T_ascomfocuser.GethasRelativePositionReal: boolean;
begin
 result:=False;
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
   try
   result:=not V.Absolute;
   if debug_msg then msg('Use RelativePosition: '+BoolToStr(result, True));
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
 result:=FhasRelativePosition;
 {$endif}
end;

function  T_ascomfocuser.GethasTimerSpeed: boolean;
begin
 result:=false;
end;

procedure T_ascomfocuser.SetTimeout(num:integer);
begin
 FTimeOut:=num;
 if debug_msg then msg('Set timeout: '+inttostr(num));
end;

function  T_ascomfocuser.GetTemperature:double;
var i: integer;
begin
 result:=0;
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
   try
   result:= V.Temperature;
   FhasTemperature:=true;
   if debug_msg then begin
   i:=round(10*Result);
   if FmsgTemp<>i then begin
      msg('Temperature: '+FormatFloat(f1,result));
      FmsgTemp:=i;
   end;
   end;
   except
    result:=0;
    FhasTemperature:=false;
    if debug_msg then msg('No temperature');
   end;
 end;
 {$endif}
end;

initialization
{$ifdef mswindows}
{$if defined(cpui386) or defined(cpux86_64)}
// some Ascom driver raise this exceptions
SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
{$endif}
{$endif}

end.

