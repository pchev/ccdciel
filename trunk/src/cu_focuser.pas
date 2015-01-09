unit cu_focuser;

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

uses cu_indifocuser, cu_ascomfocuser, u_global,
  Classes, SysUtils;

type

T_focuser = class(TObject)
  private
    indifocuser: T_indifocuser;
    ascomfocuser:T_ascomfocuser;
    indiready: Boolean;
    FFocuserInterface: TDevInterface;
    FonMsg: TNotifyMsg;
    FonPositionChange: TNotifyNum;
    FonTimerChange: TNotifyNum;
    FonSpeedChange: TNotifyNum;
    FonStatusChange: TNotifyEvent;
    procedure IndiDestroy(Sender: TObject);
    function GetFocuserStatus: TDeviceStatus;
    function GetonMsg: TNotifyMsg;
    procedure SetonMsg(value: TNotifyMsg);
    function GetonPositionChange: TNotifyNum;
    procedure SetonPositionChange(value: TNotifyNum);
    function GetonSpeedChange: TNotifyNum;
    procedure SetonSpeedChange(value: TNotifyNum);
    function GetonTimerChange: TNotifyNum;
    procedure SetonTimerChange(value: TNotifyNum);
    function GetonStatusChange: TNotifyEvent;
    procedure SetonStatusChange(value: TNotifyEvent);
    function  GetPosition:integer;
    procedure SetPosition(p:integer);
    procedure SetSpeed(p:integer);
    function  GetSpeed:integer;
    procedure SetTimer(p:integer);
    function  GetTimer:integer;
    function GethasAbsolutePosition: boolean;
  public
    constructor Create(devinterface: TDevInterface);
    destructor  Destroy; override;
    Procedure Connect(indiserver, indiserverport, indidevice, indideviceport: string);
    Procedure Connect(ascomdevice: string);
    Procedure Disconnect;
    procedure FocusIn;
    procedure FocusOut;
    property FocuserInterface: TDevInterface read FFocuserInterface;
    property Status: TDeviceStatus read GetFocuserStatus;
    property hasAbsolutePosition: boolean read GethasAbsolutePosition;
    property Position: integer read GetPosition write SetPosition;
    property Speed: integer read GetSpeed write SetSpeed;
    property Timer: integer read GetTimer write SetTimer;
    property onMsg: TNotifyMsg read GetonMsg write SetonMsg;
    property onPositionChange: TNotifyNum read GetonPositionChange write SetonPositionChange;
    property onSpeedChange: TNotifyNum read GetonSpeedChange write SetonSpeedChange;
    property onTimerChange: TNotifyNum read GetonTimerChange write SetonTimerChange;
    property onStatusChange: TNotifyEvent read GetonStatusChange write SetonStatusChange;
end;

implementation

constructor T_focuser.Create(devinterface: TDevInterface);
begin
 inherited Create;
 indiready:=False;
 FFocuserInterface:=devinterface;
 case FFocuserInterface of
    INDI : begin
             indifocuser:= T_indifocuser.Create;
             indifocuser.onDestroy:=@IndiDestroy;
             indiready:=True;
           end;
    ASCOM: begin
             ascomfocuser:=T_ascomfocuser.Create;
           end;
 end;
end;

destructor  T_focuser.Destroy;
begin
 case FFocuserInterface of
    INDI : begin
             if indiready then indifocuser.Free;
           end;
    ASCOM :begin
             ascomfocuser.Free;
          end;
 end;
 inherited Destroy;
end;

procedure T_focuser.IndiDestroy(Sender: TObject);
begin
 indiready:=False;
end;

Procedure T_focuser.Connect(indiserver, indiserverport, indidevice, indideviceport: string);
begin
 if not indiready then begin
   indifocuser:= T_indifocuser.Create;
   indifocuser.onDestroy:=@IndiDestroy;
   indifocuser.onMsg:=FonMsg;
   indifocuser.onPositionChange:=FonPositionChange;
   indifocuser.onSpeedChange:=FonSpeedChange;
   indifocuser.onTimerChange:=FonTimerChange;
   indifocuser.onStatusChange:=FonStatusChange;
   indiready:=True;
 end;
 if indiserver<>'' then indifocuser.indiserver:=indiserver;
 if indiserverport<>'' then indifocuser.indiserverport:=indiserverport;
 if indidevice<>'' then indifocuser.indidevice:=indidevice;
 indifocuser.indideviceport:=indideviceport;
 indifocuser.Connect;
end;

Procedure T_focuser.Connect(ascomdevice: string);
begin
 ascomfocuser.Device:=ascomdevice;
 ascomfocuser.Connect;
end;

Procedure T_focuser.Disconnect;
begin
 case FFocuserInterface of
    INDI : begin
             if indiready then indifocuser.Disconnect;
           end;
    ASCOM: begin
             ascomfocuser.Disconnect;
           end;
 end;
end;

function  T_focuser.GetPosition:integer;
begin
 case FFocuserInterface of
    INDI : begin
             if indiready then result:=indifocuser.Position;
           end;
    ASCOM: begin
             result:=ascomfocuser.Position;
           end;
 end;
end;

procedure T_focuser.FocusIn;
begin
 case FFocuserInterface of
    INDI : begin
             if indiready then indifocuser.FocusIn;
           end;
    ASCOM: begin
             ascomfocuser.FocusIn;
           end;
 end;
end;

procedure T_focuser.FocusOut;
begin
 case FFocuserInterface of
    INDI : begin
             if indiready then indifocuser.FocusOut;
           end;
    ASCOM: begin
             ascomfocuser.FocusOut;
           end;
 end;
end;

procedure  T_focuser.SetPosition(p:integer);
begin
 case FFocuserInterface of
    INDI : begin
             if indiready then indifocuser.Position:=p;
           end;
    ASCOM: begin
             ascomfocuser.Position:=p;
           end;
 end;
end;

procedure T_focuser.SetSpeed(p:integer);
begin
 case FFocuserInterface of
    INDI : begin
             if indiready then indifocuser.Speed:=p;
           end;
    ASCOM: begin
             ascomfocuser.Speed:=p;
           end;
 end;
end;

function  T_focuser.GetSpeed:integer;
begin
 case FFocuserInterface of
    INDI : begin
             if indiready then result:=indifocuser.Speed;
           end;
    ASCOM: begin
             result:=ascomfocuser.Speed;
           end;
 end;
end;

procedure T_focuser.SetTimer(p:integer);
begin
 case FFocuserInterface of
    INDI : begin
             if indiready then indifocuser.Timer:=p;
           end;
    ASCOM: begin
             ascomfocuser.Timer:=p;
           end;
 end;
end;

function  T_focuser.GetTimer:integer;
begin
 case FFocuserInterface of
    INDI : begin
             if indiready then result:=indifocuser.Timer;
           end;
    ASCOM: begin
             result:=ascomfocuser.Timer;
           end;
 end;
end;

function T_focuser.GethasAbsolutePosition: boolean;
begin
 case FFocuserInterface of
    INDI : begin
             if indiready then result:=indifocuser.hasAbsolutePosition;
           end;
    ASCOM: begin
             result:=ascomfocuser.hasAbsolutePosition;
           end;
 end;
end;

function T_focuser.GetFocuserStatus: TDeviceStatus;
begin
 case FFocuserInterface of
    INDI : begin
             if indiready then result:=indifocuser.Status
                          else result:=devDisconnected;
           end;
    ASCOM: begin
             result:=ascomfocuser.Status
           end;
 end;
end;

function T_focuser.GetonMsg: TNotifyMsg;
begin
 case FFocuserInterface of
    INDI : begin
             if indiready then result:=indifocuser.onMsg;
           end;
    ASCOM: begin
             result:=ascomfocuser.onMsg;
           end;
 end;
end;

procedure T_focuser.SetonMsg(value: TNotifyMsg);
begin
 FonMsg:=value;
 case FFocuserInterface of
    INDI : begin
             if indiready then indifocuser.onMsg:=value;
           end;
    ASCOM: begin
             ascomfocuser.onMsg:=value;
           end;
 end;
end;

function T_focuser.GetonPositionChange: TNotifyNum;
begin
 case FFocuserInterface of
    INDI : begin
             if indiready then result:=indifocuser.onPositionChange;
           end;
    ASCOM: begin
             result:=ascomfocuser.onPositionChange;
           end;
 end;
end;

procedure T_focuser.SetonPositionChange(value: TNotifyNum);
begin
 FonPositionChange:=value;
 case FFocuserInterface of
    INDI : begin
             if indiready then indifocuser.onPositionChange:=value;
           end;
    ASCOM: begin
             ascomfocuser.onPositionChange:=value;
           end;
 end;
end;

function T_focuser.GetonSpeedChange: TNotifyNum;
begin
 case FFocuserInterface of
    INDI : begin
             if indiready then result:=indifocuser.onSpeedChange;
           end;
    ASCOM: begin
             result:=ascomfocuser.onSpeedChange;
           end;
 end;
end;

procedure T_focuser.SetonSpeedChange(value: TNotifyNum);
begin
 FonSpeedChange:=value;
 case FFocuserInterface of
    INDI : begin
             if indiready then indifocuser.onSpeedChange:=value;
           end;
    ASCOM: begin
              ascomfocuser.onSpeedChange:=value;
           end;
 end;
end;

function T_focuser.GetonTimerChange: TNotifyNum;
begin
 case FFocuserInterface of
    INDI : begin
             if indiready then result:=indifocuser.onTimerChange;
           end;
    ASCOM: begin
             result:=ascomfocuser.onTimerChange;
           end;
 end;
end;

procedure T_focuser.SetonTimerChange(value: TNotifyNum);
begin
 FonTimerChange:=value;
 case FFocuserInterface of
    INDI : begin
             if indiready then indifocuser.onTimerChange:=value;
           end;
    ASCOM: begin
             ascomfocuser.onTimerChange:=value;
           end;
 end;
end;

function T_focuser.GetonStatusChange: TNotifyEvent;
begin
 case FFocuserInterface of
    INDI : begin
             if indiready then result:=indifocuser.onStatusChange;
           end;
    ASCOM: begin
             result:=ascomfocuser.onStatusChange;
           end;
 end;
end;

procedure T_focuser.SetonStatusChange(value: TNotifyEvent);
begin
 FonStatusChange:=value;
 case FFocuserInterface of
    INDI : begin
             if indiready then indifocuser.onStatusChange:=value;
           end;
    ASCOM: begin
             ascomfocuser.onStatusChange:=value;
           end;
 end;
end;

end.

