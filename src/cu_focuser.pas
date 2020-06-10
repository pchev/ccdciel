unit cu_focuser;

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

uses u_global, u_utils, indiapi, u_translation, cu_weather,
  Classes, SysUtils;

type

T_focuser = class(TComponent)
  private
    Fweather: T_weather;
    procedure SetPositionInt(p:integer);
    procedure SetRelPositionInt(p:integer);
  protected
    FFocuserInterface: TDevInterface;
    FStatus: TDeviceStatus;
    FonMsg,FonDeviceMsg: TNotifyMsg;
    FonPositionChange: TNotifyNum;
    FonTimerChange: TNotifyNum;
    FonSpeedChange: TNotifyNum;
    FonTemperatureChange: TNotifyNum;
    FonStatusChange: TNotifyEvent;
    Fdevice: string;
    FTimeOut: integer;
    FAutoLoadConfig: boolean;
    FLastDirection: boolean;
    FhasTemperature: boolean;
    FDelay: integer;
    FFocusdirection: integer;
    FBacklashDirection,FBacklashActive: boolean;
    FBacklash: integer;
    FUseExternalTemperature: boolean;
    procedure msg(txt: string; level:integer=3);
    function  GetPosition:integer; virtual; abstract;
    procedure SetPosition(p:integer); virtual; abstract;
    function  GetRelPosition:integer; virtual; abstract;
    procedure SetRelPosition(p:integer); virtual; abstract;
    function  GetPositionRange: TNumRange; virtual; abstract;
    function  GetRelPositionRange: TNumRange; virtual; abstract;
    procedure SetSpeed(p:integer); virtual; abstract;
    function  GetSpeed:integer; virtual; abstract;
    procedure SetTimer(p:integer); virtual; abstract;
    function  GetTimer:integer; virtual; abstract;
    function  GethasAbsolutePosition: boolean; virtual; abstract;
    function  GethasRelativePosition: boolean; virtual; abstract;
    function  GethasTimerSpeed: boolean; virtual; abstract;
    procedure SetTimeout(num:integer); virtual; abstract;
    function  GetTemperature:double; virtual; abstract;
    function  GetTemperatureInt:double;
  public
    constructor Create(AOwner: TComponent);override;
    destructor  Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string=''); virtual; abstract;
    Procedure Disconnect; virtual; abstract;
    property DeviceName: string read FDevice;
    procedure FocusIn; virtual; abstract;
    procedure FocusOut; virtual; abstract;
    property Delay: integer read FDelay write FDelay;
    property LastDirection: boolean read FLastDirection;
    property FocuserInterface: TDevInterface read FFocuserInterface;
    property Status: TDeviceStatus read FStatus;
    property hasAbsolutePosition: boolean read GethasAbsolutePosition;
    property hasRelativePosition: boolean read GethasRelativePosition;
    property hasTimerSpeed: boolean read GethasTimerSpeed;
    property hasTemperature: boolean read FhasTemperature;
    property Temperature: double read GetTemperatureInt;
    property Position: integer read GetPosition write SetPositionInt;
    property RelPosition: integer read GetRelPosition write SetRelPositionInt;
    property PositionRange: TNumRange read GetPositionRange;
    property RelPositionRange: TNumRange read GetRelPositionRange;
    property Speed: integer read GetSpeed write SetSpeed;
    property Timer: integer read GetTimer write SetTimer;
    property Timeout: integer read FTimeout write SetTimeout;
    property AutoLoadConfig: boolean read FAutoLoadConfig write FAutoLoadConfig;
    property BacklashDirection: boolean read FBacklashDirection write FBacklashDirection;
    property BacklashActive: boolean read FBacklashActive write FBacklashActive;
    property Backlash: integer read FBacklash write FBacklash;
    property UseExternalTemperature: boolean read FUseExternalTemperature write FUseExternalTemperature;
    property weather: T_weather read Fweather write Fweather;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onDeviceMsg: TNotifyMsg read FonDeviceMsg write FonDeviceMsg;
    property onPositionChange: TNotifyNum read FonPositionChange write FonPositionChange;
    property onSpeedChange: TNotifyNum read FonSpeedChange write FonSpeedChange;
    property onTimerChange: TNotifyNum read FonTimerChange write FonTimerChange;
    property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
    property onTemperatureChange: TNotifyNum read FonTemperatureChange write FonTemperatureChange;
end;

implementation

constructor T_focuser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStatus := devDisconnected;
  FTimeOut:=100;
  FDelay:=0;
  FFocusdirection:=1;
  FBacklashActive:=false;
  FhasTemperature:=false;
  FUseExternalTemperature:=false;
end;

destructor  T_focuser.Destroy;
begin
  inherited Destroy;
end;

procedure T_focuser.msg(txt: string; level:integer=3);
begin
  if Assigned(FonMsg) then FonMsg(Fdevice+': '+txt,level);
end;

procedure T_focuser.SetPositionInt(p:integer);
begin
  if FBacklashActive and ((p<Position)<>FBacklashDirection) then begin   // p<position = focus IN
    if FBacklashDirection then
    begin
       msg(Format(rsFocuserMoveT, [inttostr(p)])+' + '+inttostr(FBacklash)+' '+rsBacklashComp);
       SetPosition(p+FBacklash);   // backlash IN, go OUT first
    end
    else
    begin
       msg(Format(rsFocuserMoveT, [inttostr(p)])+' - '+inttostr(FBacklash)+' '+rsBacklashComp);
       SetPosition(p-FBacklash);  // backlash OUT, go IN first
    end;
    Wait(FDelay+1);
  end;
  msg(Format(rsFocuserMoveT, [inttostr(p)]));
  SetPosition(p);                 // go to final position
  Wait(FDelay);
end;

procedure T_focuser.SetRelPositionInt(p:integer);
begin
  if BacklashActive and (FLastDirection<>FBacklashDirection) then begin   // FLastDirection = focus IN
    if FBacklashDirection then begin // want to go OUT, backlash IN
       msg(Format(rsFocuserMoveB, [inttostr(FFocusdirection*p)])+', '+rsBacklashComp+' + '+inttostr(FBacklash));
       SetRelPosition(p+FBacklash);  // go more OUT than required
       FocusIN;                      // go IN by backlash
       SetRelPosition(FBacklash);
    end
    else begin                       // want to go IN, backlash OUT
       msg(Format(rsFocuserMoveB, [inttostr(FFocusdirection*p)])+', '+rsBacklashComp+' - '+inttostr(FBacklash));
       SetRelPosition(p+FBacklash);  // go more IN than required
       FocusOUT;                     // go OUT by backlash
       SetRelPosition(FBacklash)
    end;
  end
  else begin
    msg(Format(rsFocuserMoveB, [inttostr(FFocusdirection*p)]));
    SetRelPosition(p);
  end;
end;

function T_focuser.GetTemperatureInt:double;
begin
  if FUseExternalTemperature and (Fweather<>nil) then begin
    FhasTemperature:=true;
    result:=Fweather.Temperature;
  end
  else begin
    result:=GetTemperature;
  end;
end;

end.

