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

uses u_global,
  Classes, SysUtils;

type

T_wheel = class(TComponent)
  protected
    FWheelInterface: TDevInterface;
    FFilterNames: TStringList;
    FStatus: TDeviceStatus;
    FonMsg: TNotifyMsg;
    FonFilterChange: TNotifyNum;
    FonStatusChange: TNotifyEvent;
    FonFilterNameChange: TNotifyEvent;
    procedure SetFilter(num:integer);  virtual; abstract;
    function  GetFilter:integer; virtual; abstract;
    procedure SetFilterNames(value:TStringList); virtual; abstract;
  public
    constructor Create;
    destructor  Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''); virtual; abstract;
    Procedure Disconnect; virtual; abstract;
    property WheelInterface: TDevInterface read FWheelInterface;
    property Status: TDeviceStatus read FStatus;
    property Filter: integer read GetFilter write SetFilter;
    property FilterNames: TStringList read FFilterNames write SetFilterNames;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
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
end;

destructor  T_wheel.Destroy;
begin
  FFilterNames.Free;
  inherited Destroy;
end;

end.

