unit cu_rotator;

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

uses u_global, indiapi,
  Classes, SysUtils;

type

T_rotator = class(TComponent)
  protected
    FRotatorInterface: TDevInterface;
    FStatus: TDeviceStatus;
    FonMsg,FonDeviceMsg: TNotifyMsg;
    FonAngleChange: TNotifyNum;
    FonStatusChange: TNotifyEvent;
    FTimeOut: integer;
    FAutoLoadConfig: boolean;
    function  GetAngle:double; virtual; abstract;
    procedure SetAngle(p:double); virtual; abstract;
    procedure SetTimeout(num:integer); virtual; abstract;
  public
    constructor Create(AOwner: TComponent);override;
    destructor  Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''); virtual; abstract;
    Procedure Disconnect; virtual; abstract;
    property RotatorInterface: TDevInterface read FRotatorInterface;
    property Status: TDeviceStatus read FStatus;
    property Timeout: integer read FTimeout write SetTimeout;
    property AutoLoadConfig: boolean read FAutoLoadConfig write FAutoLoadConfig;
    property Angle: double read GetAngle write SetAngle;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onAngleChange: TNotifyNum read FonAngleChange write FonAngleChange;
    property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
end;

implementation

constructor T_rotator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStatus := devDisconnected;
end;

destructor  T_rotator.Destroy;
begin
  inherited Destroy;
end;

end.

