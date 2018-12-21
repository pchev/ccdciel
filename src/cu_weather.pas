unit cu_weather;

{$mode objfpc}{$H+}

{
Copyright (C) 2018 Patrick Chevalley

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

T_weather = class(TComponent)
 private
 protected
    FWeatherInterface: TDevInterface;
    FStatus: TDeviceStatus;
    FonMsg,FonDeviceMsg: TNotifyMsg;
    FonStatusChange,FonClearChange: TNotifyEvent;
    FTimeOut: integer;
    Fdevice: string;
    FAutoLoadConfig: boolean;
    FhasCloudCover,FhasDewPoint,FhasHumidity,FhasPressure,FhasRainRate,FhasSkyBrightness,
    FhasSkyQuality,FhasSkyTemperature,FhasStarFWHM,FhasTemperature,FhasWindDirection,
    FhasWindGust,FhasWindSpeed,FhasStatus: boolean;
    procedure msg(txt: string; level:integer=3);
    function GetClear: boolean; virtual; abstract;
    procedure GetCapabilities; virtual; abstract;
    function GetCloudCover: double; virtual; abstract;
    function GetDewPoint: double; virtual; abstract;
    function GetHumidity: double; virtual; abstract;
    function GetPressure: double; virtual; abstract;
    function GetRainRate: double; virtual; abstract;
    function GetSkyBrightness: double; virtual; abstract;
    function GetSkyQuality: double; virtual; abstract;
    function GetSkyTemperature: double; virtual; abstract;
    function GetStarFWHM: double; virtual; abstract;
    function GetTemperature: double; virtual; abstract;
    function GetWindDirection: double; virtual; abstract;
    function GetWindGust: double; virtual; abstract;
    function GetWindSpeed: double; virtual; abstract;
    function GetWeatherStatus: boolean; virtual; abstract;
    procedure SetTimeout(num:integer); virtual; abstract;
  public
    UseCloudCover,UseDewPoint,UseHumidity,UsePressure,UseRainRate,UseSkyBrightness,
    UseSkyQuality,UseSkyTemperature,UseStarFWHM,UseTemperature,UseWindDirection,
    UseWindGust,UseWindSpeed,UseStatus: boolean;
    MinCloudCover,MinDewPoint,MinHumidity,MinPressure,MinRainRate,MinSkyBrightness,
    MinSkyQuality,MinSkyTemperature,MinStarFWHM,MinTemperature,MinWindDirection,
    MinWindGust,MinWindSpeed: double;
    MaxCloudCover,MaxDewPoint,MaxHumidity,MaxPressure,MaxRainRate,MaxSkyBrightness,
    MaxSkyQuality,MaxSkyTemperature,MaxStarFWHM,MaxTemperature,MaxWindDirection,
    MaxWindGust,MaxWindSpeed: double;
    constructor Create(AOwner: TComponent);override;
    destructor  Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''); virtual; abstract;
    Procedure Disconnect; virtual; abstract;
    property WeatherInterface: TDevInterface read FWeatherInterface;
    property Status: TDeviceStatus read FStatus;
    property Timeout: integer read FTimeout write SetTimeout;
    property AutoLoadConfig: boolean read FAutoLoadConfig write FAutoLoadConfig;
    property Clear: boolean read GetClear;
    property hasCloudCover: boolean read FhasCloudCover ;
    property hasDewPoint: boolean read FhasDewPoint ;
    property hasHumidity: boolean read FhasHumidity ;
    property hasPressure: boolean read FhasPressure ;
    property hasRainRate: boolean read FhasRainRate ;
    property hasSkyBrightness: boolean read FhasSkyBrightness ;
    property hasSkyQuality: boolean read FhasSkyQuality ;
    property hasSkyTemperature: boolean read FhasSkyTemperature ;
    property hasStarFWHM: boolean read FhasStarFWHM ;
    property hasTemperature: boolean read FhasTemperature ;
    property hasWindDirection: boolean read FhasWindDirection ;
    property hasWindGust: boolean read FhasWindGust ;
    property hasWindSpeed: boolean read FhasWindSpeed ;
    property hasStatus: boolean read FhasStatus;
    property CloudCover: double read GetCloudCover ;
    property DewPoint: double read GetDewPoint ;
    property Humidity: double read GetHumidity ;
    property Pressure: double read GetPressure ;
    property RainRate: double read GetRainRate ;
    property SkyBrightness: double read GetSkyBrightness ;
    property SkyQuality: double read GetSkyQuality ;
    property SkyTemperature: double read GetSkyTemperature ;
    property StarFWHM: double read GetStarFWHM ;
    property Temperature: double read GetTemperature ;
    property WindDirection: double read GetWindDirection ;
    property WindGust: double read GetWindGust ;
    property WindSpeed: double read GetWindSpeed ;
    property WeatherStatus: boolean read GetWeatherStatus;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onDeviceMsg: TNotifyMsg read FonDeviceMsg write FonDeviceMsg;
    property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
    property onClearChange: TNotifyEvent read FonClearChange write FonClearChange;
end;

implementation

constructor T_weather.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStatus := devDisconnected;
  FhasCloudCover:=false;
  FhasDewPoint:=false;
  FhasHumidity:=false;
  FhasPressure:=false;
  FhasRainRate:=false;
  FhasSkyBrightness:=false;
  FhasSkyQuality:=false;
  FhasSkyTemperature:=false;
  FhasStarFWHM:=false;
  FhasTemperature:=false;
  FhasWindDirection:=false;
  FhasWindGust:=false;
  FhasWindSpeed:=false;
  FhasStatus:=false;
  UseCloudCover:=false;
  UseDewPoint:=false;
  UseHumidity:=false;
  UsePressure:=false;
  UseRainRate:=false;
  UseSkyBrightness:=false;
  UseSkyQuality:=false;
  UseSkyTemperature:=false;
  UseStarFWHM:=false;
  UseTemperature:=false;
  UseWindDirection:=false;
  UseWindGust:=false;
  UseWindSpeed:=false;
  UseStatus:=false;
end;

destructor  T_weather.Destroy;
begin
  inherited Destroy;
end;

procedure T_weather.msg(txt: string; level:integer=3);
begin
  if Assigned(FonMsg) then FonMsg(Fdevice+': '+txt,level);
end;

end.

