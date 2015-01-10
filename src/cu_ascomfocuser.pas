unit cu_ascomfocuser;

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

uses u_global,
    {$ifdef mswindows}
    Variants, comobj,
    {$endif}
    ExtCtrls,Classes, SysUtils;

type
T_ascomfocuser = class(TObject)
 private
   {$ifdef mswindows}
   V: variant;
   {$endif}
   FSpeed,FFocusdirection,FRelIncr: integer;
   stPosition,stRelpos: integer;
   Fdevice: string;
   FStatus: TDeviceStatus;
   FonMsg: TNotifyMsg;
   FonStatusChange: TNotifyEvent;
   FonPositionChange: TNotifyNum;
   FonTimerChange: TNotifyNum;
   FonSpeedChange: TNotifyNum;
   StatusTimer: TTimer;
   procedure StatusTimerTimer(sender: TObject);
   procedure SetPosition(p:integer);
   function  GetPosition:integer;
   procedure SetRelPosition(p:integer);
   function  GetRelPosition:integer;
   procedure SetSpeed(p:integer);
   function  GetSpeed:integer;
   procedure SetTimer(p:integer);
   function  GetTimer:integer;
   function  GethasAbsolutePosition: boolean;
   function  GethasRelativePosition: boolean;
   function  GethasTimerSpeed: boolean;
   procedure msg(txt: string);
 public
   constructor Create;
   destructor  Destroy; override;
   procedure Connect;
   procedure Disconnect;
   function  Connected: boolean;
   procedure FocusIn;
   procedure FocusOut;
   property Device: string read Fdevice write Fdevice;
   property hasAbsolutePosition: boolean read GethasAbsolutePosition;
   property hasRelativePosition: boolean read GethasRelativePosition;
   property hasTimerSpeed: boolean read GethasTimerSpeed;
   property Position: integer read GetPosition write SetPosition;
   property RelPosition: integer read GetRelPosition write SetRelPosition;
   property Speed: integer read GetSpeed write SetSpeed;
   property Timer: integer read GetTimer write SetTimer;
   property Status: TDeviceStatus read FStatus;
   property onMsg: TNotifyMsg read FonMsg write FonMsg;
   property onPositionChange: TNotifyNum read FonPositionChange write FonPositionChange;
   property onSpeedChange: TNotifyNum read FonSpeedChange write FonSpeedChange;
   property onTimerChange: TNotifyNum read FonTimerChange write FonTimerChange;
   property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
end;


implementation

constructor T_ascomfocuser.Create;
begin
 inherited Create;
 FStatus := devDisconnected;
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

procedure T_ascomfocuser.Connect;
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

procedure T_ascomfocuser.Disconnect;
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

function T_ascomfocuser.Connected: boolean;
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

procedure T_ascomfocuser.StatusTimerTimer(sender: TObject);
var x,y: double;
    p: integer;
begin
 {$ifdef mswindows}
  if not Connected then begin
     FStatus := devDisconnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
  end
  else begin
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
  end;
 {$endif}
end;

procedure T_ascomfocuser.FocusIn;
begin
 FFocusdirection:=-1;
end;

procedure T_ascomfocuser.FocusOut;
begin
 FFocusdirection:=1;
end;

procedure T_ascomfocuser.SetPosition(p:integer);
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   V.Move(p);
   except
    on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

function  T_ascomfocuser.GetPosition:integer;
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=V.Position;
   except
    on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

procedure T_ascomfocuser.SetRelPosition(p:integer);
var i: integer;
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   FRelIncr:=p;
   i:=FFocusdirection*FRelIncr;
   V.Move(i);
   except
    on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

function  T_ascomfocuser.GetRelPosition:integer;
begin
 result:=FRelIncr;
end;

procedure T_ascomfocuser.SetSpeed(p:integer);
begin
  // not implemented in ASCOM
end;

function  T_ascomfocuser.GetSpeed:integer;
begin
 // not implemented in ASCOM
end;

procedure T_ascomfocuser.SetTimer(p:integer);
begin
 // not implemented in ASCOM
end;

function  T_ascomfocuser.GetTimer:integer;
begin
 // not implemented in ASCOM
end;

function  T_ascomfocuser.GethasAbsolutePosition: boolean;
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=V.Absolute;
   except
    on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

function  T_ascomfocuser.GethasRelativePosition: boolean;
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=not V.Absolute;
   except
    on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

function  T_ascomfocuser.GethasTimerSpeed: boolean;
begin
 result:=false;
end;

procedure T_ascomfocuser.msg(txt: string);
begin
  if Assigned(FonMsg) then FonMsg(txt);
end;


end.

