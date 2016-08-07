unit cu_mount;

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

T_mount = class(TComponent)
  protected
    FMountInterface: TDevInterface;
    FonMsg,FonDeviceMsg: TNotifyMsg;
    FonCoordChange: TNotifyEvent;
    FonParkChange: TNotifyEvent;
    FonStatusChange: TNotifyEvent;
    FStatus: TDeviceStatus;
    FMountSlewing: boolean;
    FTimeOut: integer;
    FAutoLoadConfig: boolean;
    function  GetPark:Boolean; virtual; abstract;
    procedure SetPark(value:Boolean); virtual; abstract;
    function  GetRA:double; virtual; abstract;
    function  GetDec:double; virtual; abstract;
    function  GetEquinox: double; virtual; abstract;
    function  GetAperture:double; virtual; abstract;
    function  GetFocaleLength:double; virtual; abstract;
    procedure SetTimeout(num:integer); virtual; abstract;
 public
    constructor Create(AOwner: TComponent);override;
    destructor  Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''); virtual; abstract;
    Procedure Disconnect; virtual; abstract;
    function Slew(sra,sde: double):boolean; virtual; abstract;
    function Sync(sra,sde: double):boolean; virtual; abstract;
    function Track:boolean; virtual; abstract;
    procedure AbortMotion; virtual; abstract;
    property MountInterface: TDevInterface read FMountInterface;
    property Status: TDeviceStatus read FStatus;
    property Park: Boolean read GetPark write SetPark;
    property MountSlewing: boolean read FMountSlewing;
    property RA: double read GetRA;
    property Dec: double read GetDec;
    property Equinox: double read GetEquinox;
    property Aperture: double read GetAperture;
    property FocaleLength: double read GetFocaleLength;
    property Timeout: integer read FTimeout write SetTimeout;
    property AutoLoadConfig: boolean read FAutoLoadConfig write FAutoLoadConfig;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onDeviceMsg: TNotifyMsg read FonDeviceMsg write FonDeviceMsg;
    property onCoordChange: TNotifyEvent read FonCoordChange write FonCoordChange;
    property onParkChange: TNotifyEvent read FonParkChange write FonParkChange;
    property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
end;

implementation

constructor T_mount.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMountSlewing:=false;
  FStatus := devDisconnected;
  FTimeOut:=100;
end;

destructor  T_mount.Destroy;
begin
  inherited Destroy;
end;

end.

