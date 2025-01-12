unit cu_ascomrotator;

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

interface

uses cu_rotator, u_global,
    {$ifdef mswindows}
    u_translation, indiapi, Variants, comobj, math,
    {$endif}
    Forms, ExtCtrls,Classes, SysUtils;

type
T_ascomrotator = class(T_rotator)
 private
   {$ifdef mswindows}
   V: variant;
   stAngle: double;
   {$endif}
   FInterfaceVersion: integer;
   StatusTimer: TTimer;
   procedure StatusTimerTimer(sender: TObject);
   function  Connected: boolean;
   function WaitConnecting(maxtime:integer):boolean;
 protected
   procedure SetAngle(p:double); override;
   function  GetAngle:double; override;
   procedure SetTimeout(num:integer); override;
   function  GetDriverReverse:boolean; override;
   procedure SetDriverReverse(value:boolean); override;

   function  WaitRotatorMoving(maxtime:integer):boolean;
public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');  override;
   procedure Disconnect; override;
   procedure Sync(p:double); override;
   function GetV: variant;
   Procedure Halt; override;
end;

const waitpoll=500;
      statusinterval=2000;

implementation

constructor T_ascomrotator.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FRotatorInterface:=ASCOM;
 FInterfaceVersion:=1;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=statusinterval;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomrotator.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

procedure T_ascomrotator.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
begin
 {$ifdef mswindows}
  try
  FStatus := devConnecting;
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
  else
    V.Connected:=true;
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
     msg(rsConnected3);
     FStatus := devConnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     StatusTimer.Enabled:=true;
  end
  else
     Disconnect;
  except
    on E: Exception do msg('Connection error: ' + E.Message,0);
  end;
 {$endif}
end;

procedure T_ascomrotator.Disconnect;
begin
 {$ifdef mswindows}
   StatusTimer.Enabled:=false;
   try
   if not VarIsEmpty(V) then begin
     if FInterfaceVersion>=4 then begin
       V.Disconnect;
       WaitConnecting(30000);
     end
     else
       V.Connected:=false;
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

function T_ascomrotator.GetV: variant;
begin
 {$ifdef mswindows}
 result:=V;
 {$endif}
end;

function T_ascomrotator.Connected: boolean;
begin
result:=false;
{$ifdef mswindows}
if not VarIsEmpty(V) then begin
  try
  result:=V.connected;
  except
   result:=false;
  end;
end;
{$endif}
end;

function T_ascomrotator.WaitConnecting(maxtime:integer):boolean;
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
  on E: Exception do begin
    msg(Format(rsConnectionEr, [E.Message]),0);
    result:=false;
  end;
 end;
 {$endif}
end;

procedure T_ascomrotator.StatusTimerTimer(sender: TObject);
{$ifdef mswindows}
var p: double;
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
      p:=GetAngle;
      if p<>stAngle then begin
        stAngle:=p;
        if Assigned(FonAngleChange) then FonAngleChange(self);
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

function T_ascomrotator.WaitRotatorMoving(maxtime:integer):boolean;
{$ifdef mswindows}
var count,maxcount:integer;
{$endif}
begin
 result:=true;
 {$ifdef mswindows}
 try
   maxcount:=maxtime div waitpoll;
   count:=0;
   while (V.IsMoving)and(count<maxcount) do begin
      sleep(waitpoll);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
      inc(count);
   end;
   result:=(count<maxcount);
 except
   result:=false;
 end;
 {$endif}
end;

procedure T_ascomrotator.SetAngle(p:double);
begin
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
 try
   //msg('Rotator '+Fdevice+' move to internal '+FormatFloat(f1,p));
   V.MoveAbsolute(p);
   WaitRotatorMoving(30000);

   except
    on E: Exception do msg('Error, can''t move to. ' + E.Message,0);
   end;
 end;
 {$endif}
end;

function  T_ascomrotator.GetAngle:double;
begin
 result:=0;
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
 try
   result:=V.Position;
   except
    on E: Exception do msg('Get position error: ' + E.Message,0);
   end;
 end;
 {$endif}
end;

procedure T_ascomrotator.Sync(p:double);
begin
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
 try
   //msg('Rotator '+Fdevice+' sync '+FormatFloat(f1,p));
   V.Sync(p);

   except
      // ignore if not supported
   end;
 end;
 {$endif}
end;

function T_ascomrotator.GetDriverReverse:boolean;
begin
 result:=false;
 {$ifdef mswindows}
   try
   if V.CanReverse then result:=V.Reverse;
   except
    result:=false;
   end;
 {$endif}
end;

procedure T_ascomrotator.SetDriverReverse(value:boolean);
begin
 {$ifdef mswindows}
   try
   if V.CanReverse then V.Reverse:=value;
   except
   end;
 {$endif}
end;

Procedure T_ascomrotator.Halt;
begin
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
   try
    V.Halt;
   except
    on E: Exception do msg('Halt error: ' + E.Message,0);
   end;
 end;
 {$endif}
end;

procedure T_ascomrotator.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

initialization
{$ifdef mswindows}
{$if defined(cpui386) or defined(cpux86_64)}
// some Ascom driver raise this exceptions
SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
{$endif}
{$endif}

end.

