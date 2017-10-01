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
    indiapi, Variants, comobj,
    {$endif}
    Forms, ExtCtrls,Classes, SysUtils;

type
T_ascomrotator = class(T_rotator)
 private
   {$ifdef mswindows}
   V: variant;
   stAngle: double;
   Fdevice: string;
   {$endif}
   FInterfaceVersion: integer;
   StatusTimer: TTimer;
   procedure StatusTimerTimer(sender: TObject);
   function  Connected: boolean;
   function  InterfaceVersion: integer;
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
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');  override;
   procedure Disconnect; override;
   Procedure Halt; override;
end;


implementation

constructor T_ascomrotator.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FRotatorInterface:=ASCOM;
 FInterfaceVersion:=1;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=1000;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomrotator.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

function  T_ascomrotator.InterfaceVersion: integer;
begin
 result:=1;
 {$ifdef mswindows}
  try
  if not VarIsEmpty(V) then begin
   result:=V.InterfaceVersion;
  end;
  except
    result:=1;
  end;
 {$endif}
end;

procedure T_ascomrotator.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');
begin
 {$ifdef mswindows}
  try
  FStatus := devConnecting;
  FCalibrationAngle:=0;
  FReverse:=False;
  Fdevice:=cp1;
  V:=Unassigned;
  V:=CreateOleObject(WideString(Fdevice));
  FInterfaceVersion:=InterfaceVersion;
  V.Connected:=true;
  if Connected then begin
     msg('Rotator '+Fdevice+' connected.');
     FStatus := devConnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     StatusTimer.Enabled:=true;
  end
  else
     Disconnect;
  except
    on E: Exception do msg('Rotator '+Fdevice+' Connection error: ' + E.Message);
  end;
 {$endif}
end;

procedure T_ascomrotator.Disconnect;
begin
 {$ifdef mswindows}
   StatusTimer.Enabled:=false;
   FStatus := devDisconnected;
   if Assigned(FonStatusChange) then FonStatusChange(self);
   try
   if not VarIsEmpty(V) then begin
     msg('Rotator '+Fdevice+' disconnected.');
     V.Connected:=false;
     V:=Unassigned;
   end;
   except
     on E: Exception do msg('Rotator '+Fdevice+' Disconnection error: ' + E.Message);
   end;
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

procedure T_ascomrotator.StatusTimerTimer(sender: TObject);
{$ifdef mswindows}
var p: double;
{$endif}
begin
 {$ifdef mswindows}
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
     on E: Exception do msg('Rotator '+Fdevice+' Status error: ' + E.Message);
    end;
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
 if Connected then begin
   maxcount:=maxtime div 100;
   count:=0;
   while (V.IsMoving)and(count<maxcount) do begin
      sleep(100);
      Application.ProcessMessages;
      inc(count);
   end;
   result:=(count<maxcount);
 end;
 except
   result:=false;
 end;
 {$endif}
end;

procedure T_ascomrotator.SetAngle(p:double);
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   //msg('Rotator '+Fdevice+' move to internal '+FormatFloat(f1,p));
   V.MoveAbsolute(p);
   WaitRotatorMoving(30000);

   except
    on E: Exception do msg('Rotator '+Fdevice+' Error, can''t move to. ' + E.Message);
   end;
 end;
 {$endif}
end;

function  T_ascomrotator.GetAngle:double;
begin
 result:=0;
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=V.Position;
   except
    on E: Exception do msg('Rotator '+Fdevice+' Get position error: ' + E.Message);
   end;
 end;
 {$endif}
end;

function T_ascomrotator.GetDriverReverse:boolean;
begin
 result:=false;
 {$ifdef mswindows}
 if Connected then begin
   try
   if V.CanReverse then result:=V.Reverse;
   except
    result:=false;
   end;
 end;
 {$endif}
end;

procedure T_ascomrotator.SetDriverReverse(value:boolean);
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   if V.CanReverse then V.Reverse:=value;
   except
   end;
 end;
 {$endif}
end;

Procedure T_ascomrotator.Halt;
begin
 {$ifdef mswindows}
 if Connected then begin
   try
    V.Halt;
   except
    on E: Exception do msg('Rotator '+Fdevice+' Halt error: ' + E.Message);
   end;
 end;
 {$endif}
end;

procedure T_ascomrotator.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

end.

