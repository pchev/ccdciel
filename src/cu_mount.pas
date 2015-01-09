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

uses cu_indimount, cu_ascommount, u_global,
  Classes, SysUtils;

type

T_mount = class(TObject)
  private
    indimount: T_indimount;
    ascommount:T_ascommount;
    indiready: Boolean;
    FMountInterface: TDevInterface;
    FonMsg: TNotifyMsg;
    FonCoordChange: TNotifyEvent;
    FonStatusChange: TNotifyEvent;
    procedure IndiDestroy(Sender: TObject);
    function GetMountStatus: TDeviceStatus;
    function GetonMsg: TNotifyMsg;
    procedure SetonMsg(value: TNotifyMsg);
    function GetonCoordChange: TNotifyEvent;
    procedure SetonCoordChange(value: TNotifyEvent);
    function GetonStatusChange: TNotifyEvent;
    procedure SetonStatusChange(value: TNotifyEvent);
    function  GetRA:double;
    function  GetDec:double;
  public
    constructor Create(devinterface: TDevInterface);
    destructor  Destroy; override;
    Procedure Connect(indiserver, indiserverport, indidevice, indideviceport: string);
    Procedure Connect(ascomdevice: string);
    Procedure Disconnect;
    property MountInterface: TDevInterface read FMountInterface;
    property Status: TDeviceStatus read GetMountStatus;
    property RA: double read GetRA;
    property Dec: double read GetDec;
    property onMsg: TNotifyMsg read GetonMsg write SetonMsg;
    property onCoordChange: TNotifyEvent read GetonCoordChange write SetonCoordChange;
    property onStatusChange: TNotifyEvent read GetonStatusChange write SetonStatusChange;
end;

implementation

constructor T_mount.Create(devinterface: TDevInterface);
begin
 inherited Create;
 indiready:=False;
 FMountInterface:=devinterface;
 case FMountInterface of
    INDI : begin
             indimount:= T_indimount.Create;
             indimount.onDestroy:=@IndiDestroy;
             indiready:=True;
           end;
    ASCOM: begin
             ascommount:=T_ascommount.Create;
           end;
 end;
end;

destructor  T_mount.Destroy;
begin
 case FMountInterface of
    INDI : begin
             if indiready then indimount.Free;
           end;
    ASCOM :begin
             ascommount.Free;
          end;
 end;
 inherited Destroy;
end;

procedure T_mount.IndiDestroy(Sender: TObject);
begin
 indiready:=False;
end;

Procedure T_mount.Connect(indiserver, indiserverport, indidevice, indideviceport: string);
begin
 if not indiready then begin
   indimount:= T_indimount.Create;
   indimount.onDestroy:=@IndiDestroy;
   indimount.onMsg:=FonMsg;
   indimount.onCoordChange:=FonCoordChange;
   indimount.onStatusChange:=FonStatusChange;
   indiready:=True;
 end;
 if indiserver<>'' then indimount.indiserver:=indiserver;
 if indiserverport<>'' then indimount.indiserverport:=indiserverport;
 if indidevice<>'' then indimount.indidevice:=indidevice;
 indimount.indideviceport:=indideviceport;
 indimount.Connect;
end;

Procedure T_mount.Connect(ascomdevice: string);
begin
  ascommount.Device:=ascomdevice;
  ascommount.Connect;
end;

Procedure T_mount.Disconnect;
begin
 case FMountInterface of
    INDI : begin
             if indiready then indimount.Disconnect;
           end;
    ASCOM: begin
             ascommount.Disconnect;
           end;
 end;
end;

function  T_mount.GetRA:double;
begin
 case FMountInterface of
    INDI : begin
             if indiready then result:=indimount.RA;
           end;
    ASCOM: begin
             result:=ascommount.RA;
           end;
 end;
end;

function  T_mount.GetDec:double;
begin
 case FMountInterface of
    INDI : begin
             if indiready then result:=indimount.Dec;
           end;
    ASCOM: begin
             result:=ascommount.Dec;
           end;
 end;
end;

function T_mount.GetMountStatus: TDeviceStatus;
begin
 case FMountInterface of
    INDI : begin
             if indiready then result:=indimount.Status
                          else result:=devDisconnected;
           end;
    ASCOM: begin
             result:=ascommount.Status;
           end;
 end;
end;

function T_mount.GetonMsg: TNotifyMsg;
begin
 case FMountInterface of
    INDI : begin
             if indiready then result:=indimount.onMsg;
           end;
    ASCOM: begin
             result:=ascommount.onMsg;
           end;
 end;
end;

procedure T_mount.SetonMsg(value: TNotifyMsg);
begin
 FonMsg:=value;
 case FMountInterface of
    INDI : begin
             if indiready then indimount.onMsg:=value;
           end;
    ASCOM: begin
             ascommount.onMsg:=value;
           end;
 end;
end;

function T_mount.GetonCoordChange: TNotifyEvent;
begin
 case FMountInterface of
    INDI : begin
             if indiready then result:=indimount.onCoordChange;
           end;
    ASCOM: begin
             result:=ascommount.onCoordChange;
           end;
 end;
end;

procedure T_mount.SetonCoordChange(value: TNotifyEvent);
begin
 FonCoordChange:=value;
 case FMountInterface of
    INDI : begin
             if indiready then indimount.onCoordChange:=value;
           end;
    ASCOM: begin
             ascommount.onCoordChange:=value;
           end;
 end;
end;

function T_mount.GetonStatusChange: TNotifyEvent;
begin
 case FMountInterface of
    INDI : begin
             if indiready then result:=indimount.onStatusChange;
           end;
    ASCOM: begin
             result:=ascommount.onStatusChange;
           end;
 end;
end;

procedure T_mount.SetonStatusChange(value: TNotifyEvent);
begin
 FonStatusChange:=value;
 case FMountInterface of
    INDI : begin
             if indiready then indimount.onStatusChange:=value;
           end;
    ASCOM: begin
             ascommount.onStatusChange:=value;
           end;
 end;
end;

end.

