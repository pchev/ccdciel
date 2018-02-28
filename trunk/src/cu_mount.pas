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

uses u_global, u_utils, indiapi,
  Classes, SysUtils;

type

T_mount = class(TComponent)
  protected
    FMountInterface: TDevInterface;
    FonMsg,FonDeviceMsg: TNotifyMsg;
    FonCoordChange: TNotifyEvent;
    FonPiersideChange: TNotifyEvent;
    FonParkChange: TNotifyEvent;
    FonStatusChange: TNotifyEvent;
    FStatus: TDeviceStatus;
    Fdevice: string;
    FMountSlewing: boolean;
    FTimeOut: integer;
    FAutoLoadConfig: boolean;
    FIsEqmod: boolean;
    procedure msg(txt: string);
    function  GetPark:Boolean; virtual; abstract;
    procedure SetPark(value:Boolean); virtual; abstract;
    function  GetRA:double; virtual; abstract;
    function  GetDec:double; virtual; abstract;
    function  GetEquinox: double; virtual; abstract;
    function  GetAperture:double; virtual; abstract;
    function  GetFocaleLength:double; virtual; abstract;
    procedure SetTimeout(num:integer); virtual; abstract;
    function  GetSyncMode:TEqmodAlign; virtual; abstract;
    procedure SetSyncMode(value:TEqmodAlign); virtual; abstract;
    function GetMountSlewing:boolean; virtual; abstract;
    function GetPierSide: TPierSide; virtual; abstract;
 public
    constructor Create(AOwner: TComponent);override;
    destructor  Destroy; override;
    procedure SlewToSkyFlatPosition;
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''); virtual; abstract;
    Procedure Disconnect; virtual; abstract;
    function Slew(sra,sde: double):boolean; virtual; abstract;
    function SlewAsync(sra,sde: double):boolean; virtual; abstract;
    function Sync(sra,sde: double):boolean; virtual; abstract;
    function Track:boolean; virtual; abstract;
    procedure AbortMotion; virtual; abstract;
    function FlipMeridian:boolean; virtual; abstract;
    // Eqmod specific
    function ClearAlignment:boolean; virtual; abstract;
    function ClearDelta:boolean; virtual; abstract;
    property IsEqmod: boolean read FIsEqmod;
    property SyncMode:TEqmodAlign read GetSyncMode write SetSyncMode;
    // Eqmod specific
    property MountInterface: TDevInterface read FMountInterface;
    property Status: TDeviceStatus read FStatus;
    property Park: Boolean read GetPark write SetPark;
    property MountSlewing: boolean read GetMountSlewing;
    property RA: double read GetRA;
    property Dec: double read GetDec;
    property PierSide: TPierSide read GetPierSide;
    property Equinox: double read GetEquinox;
    property Aperture: double read GetAperture;
    property FocaleLength: double read GetFocaleLength;
    property Timeout: integer read FTimeout write SetTimeout;
    property AutoLoadConfig: boolean read FAutoLoadConfig write FAutoLoadConfig;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onDeviceMsg: TNotifyMsg read FonDeviceMsg write FonDeviceMsg;
    property onCoordChange: TNotifyEvent read FonCoordChange write FonCoordChange;
    property onPiersideChange: TNotifyEvent read FonPiersideChange write FonPiersideChange;
    property onParkChange: TNotifyEvent read FonParkChange write FonParkChange;
    property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
end;

implementation

constructor T_mount.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsEqmod:=false;
  FMountSlewing:=false;
  FStatus := devDisconnected;
  FTimeOut:=100;
end;

destructor  T_mount.Destroy;
begin
  inherited Destroy;
end;

procedure T_mount.msg(txt: string);
begin
 if Assigned(FonMsg) then FonMsg(Fdevice+': '+txt);
end;

procedure T_mount.SlewToSkyFlatPosition;
var zra,zde: double;
begin
    // every 5 minutes maintain position near the zenith, add some randomness and stop tracking
   if now-FlatSlewTime>(5/minperday) then begin
     FlatSlewTime:=now;
     cmdHz2Eq(0,90,zra,zde);
     zra:=zra+(Random-0.5)/15;     // move +/- 0.5 degree
     zde:=zde+(Random-0.5);
     if FlatWaitDusk then
        zra:=rmod(zra+1,24)        // dusk, 1 hour east of zenith
     else
        zra:=rmod(zra-1+24,24);    // dawn, 1 hour west of zenith
     // slew
     Slew(zra,zde);
     // stop tracking to blur the stars
     AbortMotion;
   end;
end;


end.

