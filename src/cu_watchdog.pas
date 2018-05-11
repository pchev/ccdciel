unit cu_watchdog;

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

uses u_global, indiapi, u_utils,
  Classes, SysUtils;

type

T_watchdog = class(TComponent)
 private
 protected
    FonMsg,FonDeviceMsg: TNotifyMsg;
    FTimeOut,FThreshold: integer;
    FAutoLoadConfig: boolean;
    FonStatusChange: TNotifyEvent;
    FStatus: TDeviceStatus;
    FDevice: string;
    procedure msg(txt: string; level:integer=3);
    procedure SetTimeout(num:integer); virtual; abstract;
    procedure SetTThreshold(num:integer); virtual; abstract;
  public
    constructor Create(AOwner: TComponent);override;
    destructor  Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''); virtual; abstract;
    Procedure Disconnect; virtual; abstract;
    property Timeout: integer read FTimeout write SetTimeout;
    property Threshold: integer read FThreshold write SetTThreshold;
    property AutoLoadConfig: boolean read FAutoLoadConfig write FAutoLoadConfig;
    property Status: TDeviceStatus read FStatus;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onDeviceMsg: TNotifyMsg read FonDeviceMsg write FonDeviceMsg;
    property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
end;

implementation

constructor T_watchdog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreshold:=0;
  FStatus := devDisconnected;
  FTimeOut:=100;
end;

destructor  T_watchdog.Destroy;
begin
  inherited Destroy;
end;

procedure T_watchdog.msg(txt: string; level:integer=3);
begin
  if Assigned(FonMsg) then FonMsg(fdevice+': '+txt,level);
end;

end.

