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

uses u_global, indiapi, u_utils, u_translation,
  Classes, SysUtils;

type

T_rotator = class(TComponent)
 private
   function  GetAngleIntf:double;
   procedure SetAngleIntf(p:double);
 protected
    FRotatorInterface: TDevInterface;
    FStatus: TDeviceStatus;
    FonMsg,FonDeviceMsg: TNotifyMsg;
    FonAngleChange: TNotifyEvent;
    FonStatusChange: TNotifyEvent;
    FTimeOut: integer;
    Fdevice: string;
    FAutoLoadConfig: boolean;
    FSoftSync, FSoftLimit: boolean;
    FCalibrationAngle: double;
    FReverse: Boolean;
    procedure msg(txt: string; level:integer=3);
    procedure SetReverse(value:boolean);
    function GetReverse:boolean;
    procedure SetDriverReverse(value:boolean); virtual; abstract;
    function GetDriverReverse:boolean; virtual; abstract;
    function  GetAngle:double; virtual; abstract;
    procedure SetAngle(p:double); virtual; abstract;
    function  GetMechAngle:double; virtual; abstract;
    procedure SetMechAngle(p:double); virtual; abstract;
    Procedure SyncAngle(angle:double); virtual; abstract;
    procedure SetTimeout(num:integer); virtual; abstract;
  public
    constructor Create(AOwner: TComponent);override;
    destructor  Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string=''); virtual; abstract;
    Procedure Disconnect; virtual; abstract;
    Procedure Halt; virtual; abstract;
    property DeviceName: string read FDevice;
    property RotatorInterface: TDevInterface read FRotatorInterface;
    property Status: TDeviceStatus read FStatus;
    property Timeout: integer read FTimeout write SetTimeout;
    property AutoLoadConfig: boolean read FAutoLoadConfig write FAutoLoadConfig;
    property Angle: double read GetAngleIntf write SetAngleIntf;
    property MechAngle: double read GetMechAngle write SetMechAngle;
    Procedure Sync(p:double);
    property CalibrationAngle: double read FCalibrationAngle write FCalibrationAngle;
    property Reverse: Boolean read GetReverse write SetReverse;
    property SoftSync: boolean read FSoftSync write FSoftSync;
    property SoftLimit: boolean read FSoftLimit write FSoftLimit;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onDeviceMsg: TNotifyMsg read FonDeviceMsg write FonDeviceMsg;
    property onAngleChange: TNotifyEvent read FonAngleChange write FonAngleChange;
    property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
end;

implementation

constructor T_rotator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStatus := devDisconnected;
  FSoftSync:=false;
  FCalibrationAngle:=0;
  FReverse:=False;
end;

destructor  T_rotator.Destroy;
begin
  inherited Destroy;
end;

procedure T_rotator.SetReverse(value:boolean);
begin
  if FSoftSync then
    FReverse:=value
  else
    SetDriverReverse(value);           // in driver
  if Assigned(FonAngleChange) then FonAngleChange(self);
end;

function T_rotator.GetReverse:boolean;
begin
 if FSoftSync then
   result:=FReverse
 else
   result:=GetDriverReverse;   // in driver
end;

procedure T_rotator.msg(txt: string; level:integer=3);
begin
  if Assigned(FonMsg) then FonMsg(Fdevice+': '+txt,level);
end;

function  T_rotator.GetAngleIntf:double;
begin
if FSoftSync then begin
  if FReverse then
    result:=(360-GetMechAngle)-FCalibrationAngle
  else
    result:=GetMechAngle-FCalibrationAngle;
  result:=Rmod(7200+result,360);
  if result>359 then result:=0;
end
else
  result:=GetAngle;
end;

procedure T_rotator.SetAngleIntf(p:double);
begin
msg(Format(rsRotatorMoveT, [FormatFloat(f1, p)]));
if FSoftSync then begin
  if FReverse then
    p:=360-p-FCalibrationAngle
  else
    p:=p+FCalibrationAngle;
  p:=rmod(7200+p,360);
  if p>=360 then p:=0;
  if FSoftLimit then begin
    p:=rmod(180+p,180);
  end;
  SetMechAngle(p);
end
else
  SetAngle(p);
end;

Procedure T_rotator.Sync(p:double);
begin
msg(Format(rsSyncToS, [FormatFloat(f1, p)]));
if FSoftSync then begin
  if FReverse then
    FCalibrationAngle:=360-GetMechAngle-p
  else
    FCalibrationAngle:=GetMechAngle-p;
  FCalibrationAngle:=rmod(7200+FCalibrationAngle,360);
  if FCalibrationAngle>359 then FCalibrationAngle:=0;
  msg(rsSyncOffset+': '+FormatFloat(f1, FCalibrationAngle));
  if Assigned(FonAngleChange) then FonAngleChange(self);
end
else
  SyncAngle(p);
end;

end.

