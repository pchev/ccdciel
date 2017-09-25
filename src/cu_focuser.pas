unit cu_focuser;

{$mode objfpc}{$H+}

{
Copyright (C) 2015 Patrick Chevalley

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

uses u_global, indiapi,
  Classes, SysUtils;

type

T_focuser = class(TComponent)
  protected
    FFocuserInterface: TDevInterface;
    FStatus: TDeviceStatus;
    FonMsg,FonDeviceMsg: TNotifyMsg;
    FonPositionChange: TNotifyNum;
    FonTimerChange: TNotifyNum;
    FonSpeedChange: TNotifyNum;
    FonTemperatureChange: TNotifyNum;
    FonStatusChange: TNotifyEvent;
    FTimeOut: integer;
    FAutoLoadConfig: boolean;
    FLastDirection: boolean;
    FhasTemperature: boolean;
    function  GetPosition:integer; virtual; abstract;
    procedure SetPosition(p:integer); virtual; abstract;
    function  GetRelPosition:integer; virtual; abstract;
    procedure SetRelPosition(p:integer); virtual; abstract;
    function  GetPositionRange: TNumRange; virtual; abstract;
    function  GetRelPositionRange: TNumRange; virtual; abstract;
    procedure SetSpeed(p:integer); virtual; abstract;
    function  GetSpeed:integer; virtual; abstract;
    procedure SetTimer(p:integer); virtual; abstract;
    function  GetTimer:integer; virtual; abstract;
    function  GethasAbsolutePosition: boolean; virtual; abstract;
    function  GethasRelativePosition: boolean; virtual; abstract;
    function  GethasTimerSpeed: boolean; virtual; abstract;
    procedure SetTimeout(num:integer); virtual; abstract;
    function  GetTemperature:double; virtual; abstract;
  public
    constructor Create(AOwner: TComponent);override;
    destructor  Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''); virtual; abstract;
    Procedure Disconnect; virtual; abstract;
    procedure FocusIn; virtual; abstract;
    procedure FocusOut; virtual; abstract;
    property LastDirection: boolean read FLastDirection;
    property FocuserInterface: TDevInterface read FFocuserInterface;
    property Status: TDeviceStatus read FStatus;
    property hasAbsolutePosition: boolean read GethasAbsolutePosition;
    property hasRelativePosition: boolean read GethasRelativePosition;
    property hasTimerSpeed: boolean read GethasTimerSpeed;
    property hasTemperature: boolean read FhasTemperature;
    property Temperature: double read GetTemperature;
    property Position: integer read GetPosition write SetPosition;
    property RelPosition: integer read GetRelPosition write SetRelPosition;
    property PositionRange: TNumRange read GetPositionRange;
    property RelPositionRange: TNumRange read GetRelPositionRange;
    property Speed: integer read GetSpeed write SetSpeed;
    property Timer: integer read GetTimer write SetTimer;
    property Timeout: integer read FTimeout write SetTimeout;
    property AutoLoadConfig: boolean read FAutoLoadConfig write FAutoLoadConfig;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onDeviceMsg: TNotifyMsg read FonDeviceMsg write FonDeviceMsg;
    property onPositionChange: TNotifyNum read FonPositionChange write FonPositionChange;
    property onSpeedChange: TNotifyNum read FonSpeedChange write FonSpeedChange;
    property onTimerChange: TNotifyNum read FonTimerChange write FonTimerChange;
    property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
    property onTemperatureChange: TNotifyNum read FonTemperatureChange write FonTemperatureChange;
end;

implementation

constructor T_focuser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStatus := devDisconnected;
  FTimeOut:=100;
  FhasTemperature:=false;
end;

destructor  T_focuser.Destroy;
begin
  inherited Destroy;
end;

end.

