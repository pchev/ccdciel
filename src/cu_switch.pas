unit cu_switch;

{$mode objfpc}{$H+}

{
Copyright (C) 2021 Patrick Chevalley

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

uses u_global, indiapi, Classes, SysUtils;

type

TSwitchList = array of boolean;

T_switch = class(TComponent)
 private
 protected
    FSwitchInterface: TDevInterface;
    FStatus: TDeviceStatus;
    FonMsg,FonDeviceMsg: TNotifyMsg;
    FonStatusChange, FonSwitchChange: TNotifyEvent;
    FTimeOut: integer;
    Fdevice: string;
    FAutoLoadConfig: boolean;
    FNumSwitch: integer;
    FSwitch: TSwitchList;
    procedure msg(txt: string; level:integer=3);
    procedure SetTimeout(num:integer); virtual; abstract;
    function GetSwitch: TSwitchList; virtual; abstract;
    procedure SetSwitch(value: TSwitchList); virtual; abstract;
  public
    constructor Create(AOwner: TComponent);override;
    destructor  Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string=''); virtual; abstract;
    Procedure Disconnect; virtual; abstract;
    property DeviceName: string read FDevice;
    property SwitchInterface: TDevInterface read FSwitchInterface;
    property Status: TDeviceStatus read FStatus;
    property NumSwitch: integer read FNumSwitch;
    property Switch: TSwitchList read GetSwitch write SetSwitch;
    property Timeout: integer read FTimeout write SetTimeout;
    property AutoLoadConfig: boolean read FAutoLoadConfig write FAutoLoadConfig;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onDeviceMsg: TNotifyMsg read FonDeviceMsg write FonDeviceMsg;
    property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
    property onSwitchChange: TNotifyEvent read FonSwitchChange write FonSwitchChange;
end;

implementation

constructor T_switch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStatus := devDisconnected;
  FNumSwitch:=0;
end;

destructor  T_switch.Destroy;
begin
  FNumSwitch:=0;
  SetLength(FSwitch,0);
  inherited Destroy;
end;

procedure T_switch.msg(txt: string; level:integer=3);
begin
  if Assigned(FonMsg) then FonMsg(Fdevice+': '+txt,level);
end;

end.

