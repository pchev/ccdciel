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
  Classes, SysUtils;

type
T_ascomfocuser = class(TObject)
 private
   Fdevice: string;
   FStatus: TDeviceStatus;
   FonMsg: TNotifyMsg;
   FonStatusChange: TNotifyEvent;
   FonPositionChange: TNotifyNum;
   FonTimerChange: TNotifyNum;
   FonSpeedChange: TNotifyNum;
   procedure SetPosition(p:integer);
   function  GetPosition:integer;
   procedure SetSpeed(p:integer);
   function  GetSpeed:integer;
   procedure SetTimer(p:integer);
   function  GetTimer:integer;
   function  GethasAbsolutePosition: boolean;
   procedure msg(txt: string);
 public
   constructor Create;
   destructor  Destroy; override;
   procedure Connect;
   procedure Disconnect;
   procedure FocusIn;
   procedure FocusOut;
   property Device: string read Fdevice write Fdevice;
   property hasAbsolutePosition: boolean read GethasAbsolutePosition;
   property Position: integer read GetPosition write SetPosition;
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
end;

destructor  T_ascomfocuser.Destroy;
begin
 inherited Destroy;
end;

procedure T_ascomfocuser.Connect;
begin

end;

procedure T_ascomfocuser.Disconnect;
begin

end;

procedure T_ascomfocuser.FocusIn;
begin

end;

procedure T_ascomfocuser.FocusOut;
begin

end;

procedure T_ascomfocuser.SetPosition(p:integer);
begin

end;

function  T_ascomfocuser.GetPosition:integer;
begin

end;

procedure T_ascomfocuser.SetSpeed(p:integer);
begin

end;

function  T_ascomfocuser.GetSpeed:integer;
begin

end;

procedure T_ascomfocuser.SetTimer(p:integer);
begin

end;

function  T_ascomfocuser.GetTimer:integer;
begin

end;

function  T_ascomfocuser.GethasAbsolutePosition: boolean;
begin

end;

procedure T_ascomfocuser.msg(txt: string);
begin
  if Assigned(FonMsg) then FonMsg(txt);
end;


end.

