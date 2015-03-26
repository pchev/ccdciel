unit cu_mount;

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

uses u_global,
  Classes, SysUtils;

type

T_mount = class(TComponent)
  protected
    FMountInterface: TDevInterface;
    FonMsg: TNotifyMsg;
    FonCoordChange: TNotifyEvent;
    FonStatusChange: TNotifyEvent;
    FStatus: TDeviceStatus;
    function  GetRA:double; virtual; abstract;
    function  GetDec:double; virtual; abstract;
    function  GetEquinox: double; virtual; abstract;
    function  GetAperture:double; virtual; abstract;
    function  GetFocaleLength:double; virtual; abstract;
  public
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''); virtual; abstract;
    Procedure Disconnect; virtual; abstract;
    procedure Slew(sra,sde: double); virtual; abstract;
    procedure Sync(sra,sde: double); virtual; abstract;
    property MountInterface: TDevInterface read FMountInterface;
    property Status: TDeviceStatus read FStatus;
    property RA: double read GetRA;
    property Dec: double read GetDec;
    property Equinox: double read GetEquinox;
    property Aperture: double read GetAperture;
    property FocaleLength: double read GetFocaleLength;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onCoordChange: TNotifyEvent read FonCoordChange write FonCoordChange;
    property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
end;

implementation

end.

