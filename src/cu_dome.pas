unit cu_dome;

{$mode objfpc}{$H+}

{
Copyright (C) 2018 Patrick Chevalley

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

uses u_global, indiapi, fu_safety, u_translation,
  Classes, SysUtils;

type

T_dome = class(TComponent)
 private
   procedure SetParkInterface(value:boolean);
   procedure SetShutterInterface(value:boolean);
 protected
    Fsafety: Tf_safety;
    FDomeInterface: TDevInterface;
    FStatus: TDeviceStatus;
    FonMsg,FonDeviceMsg: TNotifyMsg;
    FonStatusChange, FonShutterChange,FonSlaveChange: TNotifyEvent;
    FTimeOut: integer;
    Fdevice: string;
    FAutoLoadConfig: boolean;
    FhasPark,FhasShutter,FhasSlaving: boolean;
    procedure msg(txt: string; level:integer=3);
    procedure SetTimeout(num:integer); virtual; abstract;
    function GetPark: boolean; virtual; abstract;
    procedure SetPark(value:boolean); virtual; abstract;
    function GetShutter: boolean; virtual; abstract;
    procedure SetShutter(value:boolean); virtual; abstract;
    function GetSlave: boolean; virtual; abstract;
    procedure SetSlave(value:boolean); virtual; abstract;
  public
    constructor Create(AOwner: TComponent);override;
    destructor  Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string=''); virtual; abstract;
    Procedure Disconnect; virtual; abstract;
    property DeviceName: string read FDevice;
    property DomeInterface: TDevInterface read FDomeInterface;
    property Status: TDeviceStatus read FStatus;
    property hasPark: boolean read FhasPark;
    property hasShutter: boolean read FhasShutter;
    property hasSlaving: boolean read FhasSlaving;
    property Park: boolean read GetPark write SetParkInterface;
    property Shutter: boolean read GetShutter write SetShutterInterface;
    property Slave: boolean read GetSlave write SetSlave;
    property Timeout: integer read FTimeout write SetTimeout;
    property AutoLoadConfig: boolean read FAutoLoadConfig write FAutoLoadConfig;
    property Safety: Tf_safety read Fsafety write Fsafety;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onDeviceMsg: TNotifyMsg read FonDeviceMsg write FonDeviceMsg;
    property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
    property onShutterChange: TNotifyEvent read FonShutterChange write FonShutterChange;
    property onSlaveChange: TNotifyEvent read FonSlaveChange write FonSlaveChange;
end;

implementation

constructor T_dome.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStatus := devDisconnected;
  FhasPark:=false;
  FhasSlaving:=false;
  FhasShutter:=false;
  Fsafety:=nil;
end;

destructor  T_dome.Destroy;
begin
  inherited Destroy;
end;

procedure T_dome.msg(txt: string; level:integer=3);
begin
  if Assigned(FonMsg) then FonMsg(Fdevice+': '+txt,level);
end;

procedure T_dome.SetParkInterface(value:boolean);
begin
  // check weather
  if (not value) and (Fsafety<>nil) and Fsafety.Connected and (not Fsafety.Safe) then begin
     msg(rsUnsafeCondit,0);
     msg('Abort dome unpark',0);
     exit;
  end;
  SetPark(value);
end;

procedure T_dome.SetShutterInterface(value:boolean);
begin
  // check weather
  if value and (Fsafety<>nil) and Fsafety.Connected and (not Fsafety.Safe) then begin
     msg(rsUnsafeCondit,0);
     msg('Abort open dome shutter',0);
     exit;
  end;
  SetShutter(value);
end;

end.

