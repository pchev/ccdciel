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

uses u_global, indiapi, u_utils,
  Classes, SysUtils;

type

T_rotator = class(TComponent)
 private
    function  GetCalibratedAngle:double;
    procedure SetCalibratedAngle(p:double);
 protected
    FRotatorInterface: TDevInterface;
    FStatus: TDeviceStatus;
    FonMsg,FonDeviceMsg: TNotifyMsg;
    FonAngleChange: TNotifyEvent;
    FonStatusChange: TNotifyEvent;
    FTimeOut: integer;
    FAutoLoadConfig: boolean;
    FCalibrationAngle: double;
    FReverse: Boolean;
    procedure msg(txt: string);
    procedure SetReverse(value:boolean);
    function GetReverse:boolean;
    procedure SetDriverReverse(value:boolean); virtual; abstract;
    function GetDriverReverse:boolean; virtual; abstract;
    function  GetAngle:double; virtual; abstract;
    procedure SetAngle(p:double); virtual; abstract;
    procedure SetTimeout(num:integer); virtual; abstract;
  public
    constructor Create(AOwner: TComponent);override;
    destructor  Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''); virtual; abstract;
    Procedure Disconnect; virtual; abstract;
    Procedure Halt; virtual; abstract;
    Procedure Sync(angle:double);
    property RotatorInterface: TDevInterface read FRotatorInterface;
    property Status: TDeviceStatus read FStatus;
    property Timeout: integer read FTimeout write SetTimeout;
    property AutoLoadConfig: boolean read FAutoLoadConfig write FAutoLoadConfig;
    property Angle: double read GetCalibratedAngle write SetCalibratedAngle;
    property CalibrationAngle: double read FCalibrationAngle write FCalibrationAngle;
    property Reverse: Boolean read GetReverse write SetReverse;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onAngleChange: TNotifyEvent read FonAngleChange write FonAngleChange;
    property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
end;

implementation

constructor T_rotator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStatus := devDisconnected;
  FCalibrationAngle:=0;
  FReverse:=False;
end;

destructor  T_rotator.Destroy;
begin
  inherited Destroy;
end;

procedure T_rotator.SetReverse(value:boolean);
begin
  if value then begin
    SetDriverReverse(True);           // try to set in driver
    FReverse:= not GetDriverReverse;  // if not use software revert
  end
  else begin
    if GetDriverReverse then SetDriverReverse(False);   // set in driver
    FReverse:=False;                                    // set in software
  end;
end;

function T_rotator.GetReverse:boolean;
begin
  result:=FReverse or GetDriverReverse;   // in software or in driver
end;

function  T_rotator.GetCalibratedAngle:double;
begin
  if FReverse then
    result:=360-GetAngle-CalibrationAngle
  else
    result:=GetAngle+CalibrationAngle;
  result:=Rmod(720+result,360);
end;

procedure T_rotator.SetCalibratedAngle(p:double);
begin
  msg('Rotator move to PA '+FormatFloat(f1,p));
  if FReverse then
    p:=360-p-CalibrationAngle
  else
    p:=p-CalibrationAngle;
  p:=rmod(720+p,360);
  SetAngle(p);
end;

Procedure T_rotator.Sync(angle:double);
begin
  if FReverse then
    FCalibrationAngle:=360-angle-GetAngle
  else
    FCalibrationAngle:=angle-GetAngle;
  FCalibrationAngle:=rmod(720+FCalibrationAngle,360);
  if Assigned(FonAngleChange) then FonAngleChange(self);
end;

procedure T_rotator.msg(txt: string);
begin
  if Assigned(FonMsg) then FonMsg(txt);
end;

end.

