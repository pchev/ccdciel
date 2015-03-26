unit cu_ascommount;

{$mode objfpc}{$H+}

{
Copyright (C) 2015 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

interface

uses  cu_mount, u_global,
  {$ifdef mswindows}
    Variants, comobj,
  {$endif}
  ExtCtrls, Classes, SysUtils;

type
T_ascommount = class(T_mount)
 private
   {$ifdef mswindows}
   V: variant;
   {$endif}
   Fdevice: string;
   stRA,stDE: double;
   StatusTimer: TTimer;
   function Connected: boolean;
   procedure StatusTimerTimer(sender: TObject);
   procedure msg(txt: string);
 protected
   function  GetRA:double; override;
   function  GetDec:double; override;
   function  GetEquinox: double; override;
   function  GetAperture:double; override;
   function  GetFocaleLength:double; override;
public
   constructor Create;
   destructor  Destroy; override;
   procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''); override;
   procedure Disconnect; override;
   procedure Slew(sra,sde: double); override;
   procedure Sync(sra,sde: double); override;

end;


implementation

constructor T_ascommount.Create;
begin
 inherited Create(nil);
 FMountInterface:=ASCOM;
 FStatus := devDisconnected;
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

function  T_ascommount.GetRA:double;
begin
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

procedure T_ascommount.Slew(sra,sde: double);
begin
 {$ifdef mswindows}
 if Connected and V.CanSlew then begin
   try
   if not V.tracking then begin
      V.tracking:=true;
   end;
   V.SlewToCoordinates(sra,sde);
   except
     on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

procedure T_ascommount.Sync(sra,sde: double);
begin
 {$ifdef mswindows}
 if Connected and V.CanSync then begin
   try
   if not V.tracking then begin
      V.tracking:=true;
   end;
   V.SyncToCoordinates(sra,sde);
   except
     on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

end.

