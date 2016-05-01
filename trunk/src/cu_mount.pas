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

uses u_global,
  Classes, SysUtils;

type

T_mount = class(TComponent)
  protected
    FMountInterface: TDevInterface;
    FonMsg,FonDeviceMsg: TNotifyMsg;
    FonCoordChange: TNotifyEvent;
    FonStatusChange: TNotifyEvent;
    FStatus: TDeviceStatus;
    function  GetRA:double; virtual; abstract;
    function  GetDec:double; virtual; abstract;
    function  GetEquinox: double; virtual; abstract;
    function  GetAperture:double; virtual; abstract;
    function  GetFocaleLength:double; virtual; abstract;
  public
    constructor Create;
    destructor  Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''); virtual; abstract;
    Procedure Disconnect; virtual; abstract;
    procedure Slew(sra,sde: double); virtual; abstract;
    procedure Sync(sra,sde: double); virtual; abstract;
    procedure AbortMotion; virtual; abstract;
    property MountInterface: TDevInterface read FMountInterface;
    property Status: TDeviceStatus read FStatus;
    property RA: double read GetRA;
    property Dec: double read GetDec;
    property Equinox: double read GetEquinox;
    property Aperture: double read GetAperture;
    property FocaleLength: double read GetFocaleLength;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onDeviceMsg: TNotifyMsg read FonDeviceMsg write FonDeviceMsg;
    property onCoordChange: TNotifyEvent read FonCoordChange write FonCoordChange;
    property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
end;

implementation

constructor T_mount.Create;
begin
  inherited Create(nil);
  FStatus := devDisconnected;
end;

destructor  T_mount.Destroy;
begin
  inherited Destroy;
end;

end.

