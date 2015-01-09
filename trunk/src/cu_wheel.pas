unit cu_wheel;

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

uses cu_indiwheel, cu_ascomwheel, cu_camera, u_global,
  Classes, SysUtils;

type

T_wheel = class(TObject)
  private
    indiwheel: T_indiwheel;
    ascomwheel:T_ascomwheel;
    indiready: Boolean;
    Fcamera: T_camera;
    FWheelInterface: TDevInterface;
    FonMsg: TNotifyMsg;
    FonFilterChange: TNotifyNum;
    FonStatusChange: TNotifyEvent;
    FonFilterNameChange: TNotifyEvent;
    procedure IndiDestroy(Sender: TObject);
    function GetWheelStatus: TDeviceStatus;
    function GetonMsg: TNotifyMsg;
    procedure SetonMsg(value: TNotifyMsg);
    function GetonFilterChange: TNotifyNum;
    procedure SetonFilterChange(value: TNotifyNum);
    function GetonStatusChange: TNotifyEvent;
    procedure SetonStatusChange(value: TNotifyEvent);
    function GetonFilterNameChange: TNotifyEvent;
    procedure SetonFilterNameChange(value: TNotifyEvent);
    procedure SetFilter(num:integer);
    function  GetFilter:integer;
    procedure SetFilterNames(value:TStringList);
    function  GetFilterNames:TStringList;
  public
    constructor Create(devinterface: TDevInterface);
    destructor  Destroy; override;
    Procedure Connect(indiserver, indiserverport, indidevice, indideviceport: string);
    Procedure Connect(ascomdevice: string);
    Procedure Connect;
    Procedure Disconnect;
    property WheelInterface: TDevInterface read FWheelInterface;
    property camera: T_camera read Fcamera write Fcamera;
    property Status: TDeviceStatus read GetWheelStatus;
    property Filter: integer read GetFilter write SetFilter;
    property FilterNames: TStringList read GetFilterNames write SetFilterNames;
    property onMsg: TNotifyMsg read GetonMsg write SetonMsg;
    property onFilterChange: TNotifyNum read GetonFilterChange write SetonFilterChange;
    property onFilterNameChange: TNotifyEvent read GetonFilterNameChange write SetonFilterNameChange;
    property onStatusChange: TNotifyEvent read GetonStatusChange write SetonStatusChange;
end;

implementation

constructor T_wheel.Create(devinterface: TDevInterface);
begin
 inherited Create;
 indiready:=False;
 FWheelInterface:=devinterface;
 case FWheelInterface of
    INDI : begin
             indiwheel:= T_indiwheel.Create;
             indiwheel.onDestroy:=@IndiDestroy;
             indiready:=True;
           end;
    ASCOM: begin
             ascomwheel:=T_ascomwheel.Create;
           end;
    INCAMERA: ;
 end;
end;

destructor  T_wheel.Destroy;
begin
 case FWheelInterface of
    INDI : begin
             if indiready then indiwheel.Free;
           end;
    ASCOM :begin
             ascomwheel.Free;
          end;
    INCAMERA: ;
 end;
 inherited Destroy;
end;

procedure T_wheel.IndiDestroy(Sender: TObject);
begin
 indiready:=False;
end;

Procedure T_wheel.Connect(indiserver, indiserverport, indidevice, indideviceport: string);
begin
 if not indiready then begin
   indiwheel:= T_indiwheel.Create;
   indiwheel.onDestroy:=@IndiDestroy;
   indiwheel.onMsg:=FonMsg;
   indiwheel.onFilterChange:=FonFilterChange;
   indiwheel.onFilterNameChange:=FonFilterNameChange;
   indiwheel.onStatusChange:=FonStatusChange;
   indiready:=True;
 end;
 if indiserver<>'' then indiwheel.indiserver:=indiserver;
 if indiserverport<>'' then indiwheel.indiserverport:=indiserverport;
 if indidevice<>'' then indiwheel.indidevice:=indidevice;
 indiwheel.indideviceport:=indideviceport;
 indiwheel.Connect;
end;

Procedure T_wheel.Connect(ascomdevice: string);
begin
  ascomwheel.Device:=ascomdevice;
  ascomwheel.Connect;
end;

Procedure T_wheel.Connect;
begin

end;

Procedure T_wheel.Disconnect;
begin
 case FWheelInterface of
    INCAMERA: begin

              end;
    INDI : begin
             if indiready then indiwheel.Disconnect;
           end;
    ASCOM: begin
             ascomwheel.Disconnect;
           end;
 end;
end;

Procedure T_wheel.SetFilter(num: integer);
begin
 case FWheelInterface of
    INCAMERA: begin
             camera.Filter:=num;
           end;
    INDI : begin
             if indiready then indiwheel.Filter:=num;
           end;
    ASCOM: begin
             ascomwheel.Filter:=num;
           end;
 end;
end;

function  T_wheel.GetFilter:integer;
begin
 case FWheelInterface of
    INCAMERA: begin
             result:=camera.Filter;
           end;
    INDI : begin
             if indiready then result:=indiwheel.Filter;
           end;
    ASCOM: begin
             result:=ascomwheel.Filter;
           end;
 end;
end;

function T_wheel.GetWheelStatus: TDeviceStatus;
begin
 case FWheelInterface of
    INCAMERA: begin
             result:=camera.Status;
           end;
    INDI : begin
             if indiready then result:=indiwheel.Status
                          else result:=devDisconnected;
           end;
    ASCOM: begin
             result:=ascomwheel.Status
           end;
 end;
end;

function T_wheel.GetonMsg: TNotifyMsg;
begin
 case FWheelInterface of
    INCAMERA: begin
             result:=camera.onMsg;
           end;
    INDI : begin
             if indiready then result:=indiwheel.onMsg;
           end;
    ASCOM: begin
             result:=ascomwheel.onMsg;
           end;
 end;
end;

procedure T_wheel.SetonMsg(value: TNotifyMsg);
begin
 FonMsg:=value;
 case FWheelInterface of
    INCAMERA: begin
             camera.onMsg:=value;
           end;
    INDI : begin
             if indiready then indiwheel.onMsg:=value;
           end;
    ASCOM: begin
             ascomwheel.onMsg:=value;
           end;
 end;
end;

function T_wheel.GetonFilterChange: TNotifyNum;
begin
 case FWheelInterface of
    INCAMERA: begin
             result:=camera.onFilterChange;
           end;
    INDI : begin
             if indiready then result:=indiwheel.onFilterChange;
           end;
    ASCOM: begin
             result:=ascomwheel.onFilterChange;
           end;
 end;
end;

procedure T_wheel.SetonFilterChange(value: TNotifyNum);
begin
 FonFilterChange:=value;
 case FWheelInterface of
    INCAMERA: begin
             camera.onFilterChange:=value;
           end;
    INDI : begin
             if indiready then indiwheel.onFilterChange:=value;
           end;
    ASCOM: begin
             ascomwheel.onFilterChange:=value;
           end;
 end;
end;

function T_wheel.GetonStatusChange: TNotifyEvent;
begin
 case FWheelInterface of
    INCAMERA: begin
             result:=camera.onWheelStatusChange;
           end;
    INDI : begin
             if indiready then result:=indiwheel.onStatusChange;
           end;
    ASCOM: begin
             result:=ascomwheel.onStatusChange;
           end;
 end;
end;

procedure T_wheel.SetonStatusChange(value: TNotifyEvent);
begin
 FonStatusChange:=value;
 case FWheelInterface of
    INCAMERA: begin
             camera.onWheelStatusChange:=value;
           end;
    INDI : begin
             if indiready then indiwheel.onStatusChange:=value;
           end;
    ASCOM: begin
             ascomwheel.onStatusChange:=value;
           end;
 end;
end;

function T_wheel.GetonFilterNameChange: TNotifyEvent;
begin
 case FWheelInterface of
    INCAMERA: begin
             result:=camera.onFilterNameChange;
           end;
    INDI : begin
             if indiready then result:=indiwheel.onFilterNameChange;
           end;
    ASCOM: begin
             result:=ascomwheel.onFilterNameChange;
           end;
 end;
end;

procedure T_wheel.SetonFilterNameChange(value: TNotifyEvent);
begin
 FonFilterNameChange:=value;
 case FWheelInterface of
    INCAMERA: begin
             camera.onFilterNameChange:=value;
           end;
    INDI : begin
             if indiready then indiwheel.onFilterNameChange:=value;
           end;
    ASCOM: begin
             ascomwheel.onFilterNameChange:=value;
           end;
 end;
end;

procedure T_wheel.SetFilterNames(value:TStringList);
begin
 case FWheelInterface of
    INCAMERA: begin
             camera.FilterNames:=value;
           end;
    INDI : begin
             if indiready then indiwheel.FilterNames:=value;
           end;
    ASCOM: begin
             ascomwheel.FilterNames:=value;
           end;
 end;
end;

function  T_wheel.GetFilterNames:TStringList;
begin
 case FWheelInterface of
    INCAMERA: begin
             result:=camera.FilterNames;
           end;
    INDI : begin
             if indiready then result:=indiwheel.FilterNames;
           end;
    ASCOM: begin
             result:=ascomwheel.FilterNames;
           end;
 end;
end;

end.

