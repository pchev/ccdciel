unit cu_cover;

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

TCoverStatus = (covNotPresent,covClosed,covMoving,covOpen,covUnknown,covError);
TCalibratorStatus = (calNotPresent,calOff,calNotReady,calReady,calUnknown,calError);

T_cover = class(TComponent)
 private
 protected
    FCoverInterface: TDevInterface;
    FStatus: TDeviceStatus;
    FonMsg,FonDeviceMsg: TNotifyMsg;
    FonStatusChange, FonCoverChange: TNotifyEvent;
    FTimeOut: integer;
    Fdevice: string;
    FHasCover, FHasCalibrator: boolean;
    st_cov: TCoverStatus;
    st_cal: TCalibratorStatus;
    FAutoLoadConfig: boolean;
    procedure msg(txt: string; level:integer=3);
    procedure SetTimeout(num:integer); virtual; abstract;
    function GetCoverState: TCoverStatus; virtual; abstract;
    function GetCalibratorState: TCalibratorStatus; virtual; abstract;
  public
    constructor Create(AOwner: TComponent);override;
    destructor  Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string=''); virtual; abstract;
    Procedure Disconnect; virtual; abstract;
    property DeviceName: string read FDevice;
    property CoverInterface: TDevInterface read FCoverInterface;
    property Status: TDeviceStatus read FStatus;
    property Timeout: integer read FTimeout write SetTimeout;
    property AutoLoadConfig: boolean read FAutoLoadConfig write FAutoLoadConfig;
    property HasCover: boolean read FHasCover;
    property HasCalibrator: boolean read FHasCalibrator;
    property CoverState: TCoverStatus read GetCoverState;
    property CalibratorState: TCalibratorStatus read GetCalibratorState;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onDeviceMsg: TNotifyMsg read FonDeviceMsg write FonDeviceMsg;
    property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
    property onCoverChange: TNotifyEvent read FonCoverChange write FonCoverChange;
end;

implementation

constructor T_cover.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStatus := devDisconnected;
  FHasCover:=false;
  FHasCalibrator:=false;
  st_cov := covUnknown;
  st_cal := calUnknown;
end;

destructor  T_cover.Destroy;
begin
  inherited Destroy;
end;

procedure T_cover.msg(txt: string; level:integer=3);
begin
  if Assigned(FonMsg) then FonMsg(Fdevice+': '+txt,level);
end;

end.

