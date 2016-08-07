unit cu_ascommount;

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

uses  cu_mount, u_global,
  {$ifdef mswindows}
    indiapi, Variants, comobj,
  {$endif}
  ExtCtrls, Classes, SysUtils;

type
T_ascommount = class(T_mount)
 private
   {$ifdef mswindows}
   V: variant;
   Fdevice: string;
   stRA,stDE: double;
   {$endif}
   StatusTimer: TTimer;
   function Connected: boolean;
   procedure StatusTimerTimer(sender: TObject);
   procedure msg(txt: string);
 protected
   procedure SetPark(value:Boolean); override;
   function  GetPark:Boolean; override;
   function  GetRA:double; override;
   function  GetDec:double; override;
   function  GetEquinox: double; override;
   function  GetAperture:double; override;
   function  GetFocaleLength:double; override;
   procedure SetTimeout(num:integer); override;
public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''); override;
   procedure Disconnect; override;
   function Slew(sra,sde: double):boolean; override;
   function Sync(sra,sde: double):boolean; override;
   function Track:boolean; override;
   procedure AbortMotion; override;
end;


implementation

constructor T_ascommount.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FMountInterface:=ASCOM;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=1000;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascommount.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

procedure T_ascommount.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');
begin
 {$ifdef mswindows}
  try
  FDevice:=cp1;
  V:=Unassigned;
  V:=CreateOleObject(WideString(Fdevice));
  V.connected:=true;
  if V.connected then begin
     FStatus := devConnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     StatusTimer.Enabled:=true;
  end
  else
     Disconnect;
  except
    on E: EOleException do msg('Error: ' + E.Message);
  end;
 {$endif}
end;

procedure T_ascommount.Disconnect;
begin
 {$ifdef mswindows}
   StatusTimer.Enabled:=false;
   FStatus := devDisconnected;
   if Assigned(FonStatusChange) then FonStatusChange(self);
   try
   if not VarIsEmpty(V) then begin
     V.connected:=false;
     V:=Unassigned;
   end;
   except
     on E: EOleException do msg('Error: ' + E.Message);
   end;
 {$endif}
end;

function T_ascommount.Connected: boolean;
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

procedure T_ascommount.StatusTimerTimer(sender: TObject);
{$ifdef mswindows}
var x,y: double;
  {$endif}
begin
 {$ifdef mswindows}
  if not Connected then begin
     FStatus := devDisconnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
  end
  else begin
    x:=GetRA;
    y:=GetDec;
    if (x<>stRA)or(y<>stDE) then begin
       stRA:=x;
       stDE:=y;
       if Assigned(FonCoordChange) then FonCoordChange(self);
    end;
  end;
 {$endif}
end;

procedure T_ascommount.SetPark(value:Boolean);
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   V.Park:=value;
   except
    on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

function  T_ascommount.GetPark:Boolean;
begin
 result:=false;
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=V.Park;
   except
    result:=false;
   end;
 end
 else result:=false;
 {$endif}
end;

function  T_ascommount.GetRA:double;
begin
 result:=NullCoord;
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=V.RightAscension;
   except
    result:=NullCoord;
   end;
 end
 else result:=NullCoord;
 {$endif}
end;

function  T_ascommount.GetDec:double;
begin
 result:=NullCoord;
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=V.Declination;
   except
    result:=NullCoord;
   end;
 end
 else result:=NullCoord;
 {$endif}
end;

function  T_ascommount.GetEquinox: double;
{$ifdef mswindows}
var i: Integer;
{$endif}
begin
 result:=0;
{$ifdef mswindows}
if Connected then begin
  try
  i:=V.EquatorialSystem;
  case i of
  0 : result:=0;
  1 : result:=0;
  2 : result:=2000;
  3 : result:=2050;
  4 : result:=1950;
  end;
  except
   result:=0;
  end;
end
else result:=0;
{$endif}
end;

function  T_ascommount.GetAperture:double;
begin
 result:=-1;
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=V.ApertureDiameter*1000;
   except
    result:=-1;
   end;
 end
 else result:=-1;
 {$endif}
end;

function  T_ascommount.GetFocaleLength:double;
begin
 result:=-1;
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=V.FocalLength*1000;
   except
    result:=-1;
   end;
 end
 else result:=-1;
 {$endif}
end;

procedure T_ascommount.msg(txt: string);
begin
  if Assigned(FonMsg) then FonMsg(txt);
end;

function T_ascommount.Slew(sra,sde: double):boolean;
begin
 result:=false;
 {$ifdef mswindows}
 result:=false;
 if Connected and V.CanSlew then begin
   try
   if not V.tracking then begin
      V.tracking:=true;
   end;
   FMountSlewing:=true;
   V.SlewToCoordinates(sra,sde);
   FMountSlewing:=false;
   result:=true;
   except
     on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

function T_ascommount.Sync(sra,sde: double):boolean;
begin
 result:=false;
 {$ifdef mswindows}
 result:=false;
 if Connected and V.CanSync then begin
   try
   if not V.tracking then begin
      V.tracking:=true;
   end;
   V.SyncToCoordinates(sra,sde);
   result:=true;
   except
     on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

function T_ascommount.Track:boolean;
begin
 result:=false;
 {$ifdef mswindows}
 result:=false;
 if Connected then begin
   try
   if not V.tracking then begin
      V.tracking:=true;
   end;
   result:=true;
   except
     on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

procedure T_ascommount.AbortMotion;
begin
 {$ifdef mswindows}
 if Connected and V.CanSlew then begin
   try
   V.AbortSlew;
   except
     on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

procedure T_ascommount.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

end.

