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
  Classes, SysUtils;

type
T_ascommount = class(TObject)
 private
   Fdevice: string;
   FStatus: TDeviceStatus;
   FonMsg: TNotifyMsg;
   FonStatusChange: TNotifyEvent;
   FonCoordChange: TNotifyEvent;
   function  GetRA:double;
   function  GetDec:double;
   procedure msg(txt: string);
public
   constructor Create;
   destructor  Destroy; override;
   procedure Connect;
   procedure Disconnect;
   property Device: string read Fdevice write Fdevice;
   property RA: double read GetRA;
   property Dec: double read GetDec;
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
end;

destructor  T_ascommount.Destroy;
begin
 inherited Destroy;
end;

procedure T_ascommount.Connect;
begin

end;

procedure T_ascommount.Disconnect;
begin

end;

function  T_ascommount.GetRA:double;
begin

end;

function  T_ascommount.GetDec:double;
begin

end;

procedure T_ascommount.msg(txt: string);
begin
  if Assigned(FonMsg) then FonMsg(txt);
end;


end.

