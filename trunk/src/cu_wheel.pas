unit cu_wheel;

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

T_wheel = class(TComponent)
  protected
    FWheelInterface: TDevInterface;
    FFilterNames: TStringList;
    FStatus: TDeviceStatus;
    FonMsg,FonDeviceMsg: TNotifyMsg;
    FonFilterChange: TNotifyNum;
    FonStatusChange: TNotifyEvent;
    FonFilterNameChange: TNotifyEvent;
    FTimeOut: integer;
    FAutoLoadConfig: boolean;
    procedure SetFilter(num:integer);  virtual; abstract;
    function  GetFilter:integer; virtual; abstract;
    procedure SetFilterNames(value:TStringList); virtual; abstract;
    procedure SetTimeout(num:integer); virtual; abstract;
  public
    constructor Create;
    destructor  Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''); virtual; abstract;
    Procedure Disconnect; virtual; abstract;
    property WheelInterface: TDevInterface read FWheelInterface;
    property Status: TDeviceStatus read FStatus;
    property Filter: integer read GetFilter write SetFilter;
    property FilterNames: TStringList read FFilterNames write SetFilterNames;
    property Timeout: integer read FTimeout write SetTimeout;
    property AutoLoadConfig: boolean read FAutoLoadConfig write FAutoLoadConfig;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onDeviceMsg: TNotifyMsg read FonDeviceMsg write FonDeviceMsg;
    property onFilterChange: TNotifyNum read FonFilterChange write FonFilterChange;
    property onFilterNameChange: TNotifyEvent read FonFilterNameChange write FonFilterNameChange;
    property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
end;

implementation

constructor T_wheel.Create;
begin
  inherited Create(nil);
  FFilterNames:=TStringList.Create;
  FStatus := devDisconnected;
  FTimeOut:=100;
  FAutoLoadConfig:=false;
end;

destructor  T_wheel.Destroy;
begin
  FFilterNames.Free;
  inherited Destroy;
end;

end.

