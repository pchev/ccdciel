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

uses  u_global,
  {$ifdef mswindows}
    Variants, comobj,
  {$endif}
  ExtCtrls, Classes, SysUtils;

type
T_ascommount = class(TObject)
 private
   {$ifdef mswindows}
   V: variant;
   {$endif}
   Fdevice: string;
   stRA,stDE: double;
   FStatus: TDeviceStatus;
   FonMsg: TNotifyMsg;
   FonStatusChange: TNotifyEvent;
   FonCoordChange: TNotifyEvent;
   StatusTimer: TTimer;
   function Connected: boolean;
   procedure StatusTimerTimer(sender: TObject);
   function  GetRA:double;
   function  GetDec:double;
   function  GetAperture:double;
   function  GetFocaleLength:double;
   procedure msg(txt: string);
public
   constructor Create;
   destructor  Destroy; override;
   procedure Connect;
   procedure Disconnect;
   property Device: string read Fdevice write Fdevice;
   property RA: double read GetRA;
   property Dec: double read GetDec;
   property Aperture: double read GetAperture;
   property FocaleLength: double read GetFocaleLength;
   property Status: TDeviceStatus read FStatus;
   property onMsg: TNotifyMsg read FonMsg write FonMsg;
   property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
   property onCoordChange: TNotifyEvent read FonCoordChange write FonCoordChange;
end;


implementation

constructor T_ascommount.Create;
begin
 inherited Create;
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

procedure T_ascommount.Connect;
begin
 {$ifdef mswindows}
  try
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
var x,y: double;
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


end.

