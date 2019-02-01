unit cu_ascomweather;

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

uses cu_weather, u_global,
    {$ifdef mswindows}
    u_translation, indiapi, Variants, comobj, math,
    {$endif}
    Forms, ExtCtrls,Classes, SysUtils;

type
T_ascomweather = class(T_weather)
 private
   {$ifdef mswindows}
   V: variant;
   stClear: boolean;
   {$endif}
   FInterfaceVersion: integer;
   StatusTimer: TTimer;
   procedure StatusTimerTimer(sender: TObject);
   function  Connected: boolean;
   function  InterfaceVersion: integer;
 protected
   function GetClear:boolean; override;
   procedure GetCapabilities; override;
   function GetCloudCover: double; override;
   function GetDewPoint: double; override;
   function GetHumidity: double; override;
   function GetPressure: double; override;
   function GetRainRate: double; override;
   function GetSkyBrightness: double; override;
   function GetSkyQuality: double; override;
   function GetSkyTemperature: double; override;
   function GetStarFWHM: double; override;
   function GetTemperature: double; override;
   function GetWindDirection: double; override;
   function GetWindGust: double; override;
   function GetWindSpeed: double; override;
   function GetWeatherStatus: boolean; override;
   procedure SetTimeout(num:integer); override;
public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');  override;
   procedure Disconnect; override;
end;

const statusinterval=2000;

implementation

constructor T_ascomweather.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FweatherInterface:=ASCOM;
 FInterfaceVersion:=1;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=statusinterval;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomweather.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

function  T_ascomweather.InterfaceVersion: integer;
begin
 result:=1;
 {$ifdef mswindows}
  try
  if not VarIsEmpty(V) then begin
   result:=V.InterfaceVersion;
  end;
  except
    result:=1;
  end;
 {$endif}
end;

procedure T_ascomweather.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');
begin
 {$ifdef mswindows}
  try
  FStatus := devConnecting;
  Fdevice:=cp1;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  V:=Unassigned;
  V:=CreateOleObject(Fdevice);
  FInterfaceVersion:=InterfaceVersion;
  V.Connected:=true;
  if Connected then begin
     try
     msg('Driver version: '+V.DriverVersion,9);
     except
       msg('Error: unknown driver version',9);
     end;
     msg(rsConnected3);
     FStatus := devConnected;
     GetCapabilities;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     StatusTimer.Enabled:=true;
  end
  else
     Disconnect;
  except
    on E: Exception do msg('Connection error: ' + E.Message,0);
  end;
 {$endif}
end;

procedure T_ascomweather.Disconnect;
begin
 {$ifdef mswindows}
   StatusTimer.Enabled:=false;
   FStatus := devDisconnected;
   if Assigned(FonStatusChange) then FonStatusChange(self);
   try
   if not VarIsEmpty(V) then begin
     msg(rsDisconnected3,0);
     V.Connected:=false;
     V:=Unassigned;
   end;
   except
     on E: Exception do msg('Disconnection error: ' + E.Message,0);
   end;
 {$endif}
end;

function T_ascomweather.Connected: boolean;
begin
result:=false;
{$ifdef mswindows}
if not VarIsEmpty(V) then begin
  try
  result:=V.connected;
  except
   result:=false;
  end;
end;
{$endif}
end;

procedure T_ascomweather.StatusTimerTimer(sender: TObject);
{$ifdef mswindows}
var s: boolean;
{$endif}
begin
 {$ifdef mswindows}
  if not Connected then begin
     FStatus := devDisconnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
  end
  else begin
    try
      s:=GetClear;
      if s<>stClear then begin
        stClear:=s;
        if Assigned(FonClearChange) then FonClearChange(self);
      end;
     except
     on E: Exception do msg('Status error: ' + E.Message,0);
    end;
  end;
 {$endif}
end;

function  T_ascomweather.GetClear:boolean;
{$ifdef mswindows}
var x: double;
    nullcheck,ok: boolean;
{$endif}
begin
 result:=false;
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
 try
   nullcheck:=false;
   result:=true;
   FWeatherMessage:='';
   if FhasStatus then begin
     // SafetyMonitor interface
     ok:=WeatherStatus;
     result:=result and ok;
     if not ok then FWeatherMessage:=FWeatherMessage+' '+'IsSafe=false';
     nullcheck:=true;
   end
   else begin
     // ObservingConditions interface
     if FhasCloudCover and UseCloudCover then begin
        x:=CloudCover;
        ok:=(x>=MinCloudCover)and(x<=MaxCloudCover);
        result:=result and ok;
        if not ok then FWeatherMessage:=FWeatherMessage+' '+'CloudCover='+FormatFloat(f2,x);
        nullcheck:=true;
     end;
     if FhasDewPoint and UseDewPoint then begin
        x:=DewPoint;
        ok:=(x>=MinDewPoint)and(x<=MaxDewPoint);
        result:=result and ok;
        if not ok then FWeatherMessage:=FWeatherMessage+' '+'DewPoint='+FormatFloat(f2,x);
        nullcheck:=true;
     end;
     if FhasHumidity and UseHumidity then begin
        x:=Humidity;
        ok:=(x>=MinHumidity)and(x<=MaxHumidity);
        result:=result and ok;
        if not ok then FWeatherMessage:=FWeatherMessage+' '+'Humidity='+FormatFloat(f2,x);
        nullcheck:=true;
     end;
     if FhasPressure and UsePressure then begin
        x:=Pressure;
        ok:=(x>=MinPressure)and(x<=MaxPressure);
        result:=result and ok;
        if not ok then FWeatherMessage:=FWeatherMessage+' '+'Pressure='+FormatFloat(f2,x);
        nullcheck:=true;
     end;
     if FhasRainRate and UseRainRate then begin
        x:=RainRate;
        ok:=(x>=MinRainRate)and(x<=MaxRainRate);
        result:=result and ok;
        if not ok then FWeatherMessage:=FWeatherMessage+' '+'RainRate='+FormatFloat(f2,x);
        nullcheck:=true;
     end;
     if FhasSkyBrightness and UseSkyBrightness then begin
        x:=SkyBrightness;
        ok:=(x>=MinSkyBrightness)and(x<=MaxSkyBrightness);
        result:=result and ok;
        if not ok then FWeatherMessage:=FWeatherMessage+' '+'SkyBrightness='+FormatFloat(f4,x);
        nullcheck:=true;
     end;
     if FhasSkyQuality and UseSkyQuality then begin
        x:=SkyQuality;
        ok:=(x>=MinSkyQuality)and(x<=MaxSkyQuality);
        result:=result and ok;
        if not ok then FWeatherMessage:=FWeatherMessage+' '+'SkyQuality='+FormatFloat(f2,x);
        nullcheck:=true;
     end;
     if FhasSkyTemperature and UseSkyTemperature then begin
        x:=SkyTemperature;
        ok:=(x>=MinSkyTemperature)and(x<=MaxSkyTemperature);
        result:=result and ok;
        if not ok then FWeatherMessage:=FWeatherMessage+' '+'SkyTemperature='+FormatFloat(f2,x);
        nullcheck:=true;
     end;
     if FhasStarFWHM and UseStarFWHM then begin
        x:=StarFWHM;
        ok:=(x>=MinStarFWHM)and(x<=MaxStarFWHM);
        result:=result and ok;
        if not ok then FWeatherMessage:=FWeatherMessage+' '+'StarFWHM='+FormatFloat(f2,x);
        nullcheck:=true;
     end;
     if FhasTemperature and UseTemperature then begin
        x:=Temperature;
        ok:=(x>=MinTemperature)and(x<=MaxTemperature);
        result:=result and ok;
        if not ok then FWeatherMessage:=FWeatherMessage+' '+'Temperature='+FormatFloat(f2,x);
        nullcheck:=true;
     end;
     if FhasWindDirection and UseWindDirection then begin
        x:=WindDirection;
        ok:=(x>=MinWindDirection)and(x<=MaxWindDirection);
        result:=result and ok;
        if not ok then FWeatherMessage:=FWeatherMessage+' '+'WindDirection='+FormatFloat(f2,x);
        nullcheck:=true;
     end;
     if FhasWindGust and UseWindGust then begin
        x:=WindGust;
        ok:=(x>=MinWindGust)and(x<=MaxWindGust);
        result:=result and ok;
        if not ok then FWeatherMessage:=FWeatherMessage+' '+'WindGust='+FormatFloat(f2,x);
        nullcheck:=true;
     end;
     if FhasWindSpeed and UseWindSpeed then begin
        x:=WindSpeed;
        ok:=(x>=MinWindSpeed)and(x<=MaxWindSpeed);
        result:=result and ok;
        if not ok then FWeatherMessage:=FWeatherMessage+' '+'WindSpeed='+FormatFloat(f2,x);
        nullcheck:=true;
     end;
   end;
   result:=result and nullcheck;
   except
    on E: Exception do begin
     msg('Get Clear error: ' + E.Message,0);
     result:=false;
    end;
   end;
 end;
 {$endif}
end;

procedure T_ascomweather.GetCapabilities;
begin
 {$ifdef mswindows}
 try
   FhasCloudCover:=false;
   DummyDouble:=V.CloudCover;
   FhasCloudCover:=true;
 except
 end;
 try
   FhasDewPoint:=false;
   DummyDouble:=V.DewPoint;
   FhasDewPoint:=true;
 except
 end;
 try
   FhasHumidity:=false;
   DummyDouble:=V.Humidity;
   FhasHumidity:=true;
 except
 end;
 try
   FhasPressure:=false;
   DummyDouble:=V.Pressure;
   FhasPressure:=true;
 except
 end;
 try
   FhasRainRate:=false;
   DummyDouble:=V.RainRate;
   FhasRainRate:=true;
 except
 end;
 try
   FhasSkyBrightness:=false;
   DummyDouble:=V.SkyBrightness;
   FhasSkyBrightness:=true;
 except
 end;
 try
   FhasSkyQuality:=false;
   DummyDouble:=V.SkyQuality;
   FhasSkyQuality:=true;
 except
 end;
 try
   FhasSkyTemperature:=false;
   DummyDouble:=V.SkyTemperature;
   FhasSkyTemperature:=true;
 except
 end;
 try
   FhasStarFWHM:=false;
   DummyDouble:=V.StarFWHM;
   FhasStarFWHM:=true;
 except
 end;
 try
   FhasTemperature:=false;
   DummyDouble:=V.Temperature;
   FhasTemperature:=true;
 except
 end;
 try
   FhasWindDirection:=false;
   DummyDouble:=V.WindDirection;
   FhasWindDirection:=true;
 except
 end;
 try
   FhasWindGust:=false;
   DummyDouble:=V.WindGust;
   FhasWindGust:=true;
 except
 end;
 try
   FhasWindSpeed:=false;
   DummyDouble:=V.WindSpeed;
   FhasWindSpeed:=true;
 except
 end;
 try
   FhasStatus:=false;
   DummyBool:=V.IsSafe;
   FhasStatus:=true;
 except
 end;
 {$endif}
end;

function T_ascomweather.GetCloudCover: double;
begin
 result:=NullCoord;
 {$ifdef mswindows}
 try
 if FhasCloudCover then begin
   result:=V.CloudCover;
 end;
 except
 end;
 {$endif}
end;

function T_ascomweather.GetDewPoint: double;
begin
 result:=NullCoord;
 {$ifdef mswindows}
 try
 if FhasDewPoint then begin
   result:=V.DewPoint;
 end;
 except
 end;
 {$endif}
end;

function T_ascomweather.GetHumidity: double;
begin
 result:=NullCoord;
 {$ifdef mswindows}
 try
 if FhasHumidity then begin
   result:=V.Humidity;
 end;
 except
 end;
 {$endif}
end;

function T_ascomweather.GetPressure: double;
begin
 result:=NullCoord;
 {$ifdef mswindows}
 try
 if FhasPressure then begin
   result:=V.Pressure;
 end;
 except
 end;
 {$endif}
end;

function T_ascomweather.GetRainRate: double;
begin
 result:=NullCoord;
 {$ifdef mswindows}
 try
 if FhasRainRate then begin
   result:=V.RainRate;
 end;
 except
 end;
 {$endif}
end;

function T_ascomweather.GetSkyBrightness: double;
begin
 result:=NullCoord;
 {$ifdef mswindows}
 try
 if FhasSkyBrightness then begin
   result:=V.SkyBrightness;
 end;
 except
 end;
 {$endif}
end;

function T_ascomweather.GetSkyQuality: double;
begin
 result:=NullCoord;
 {$ifdef mswindows}
 try
 if FhasSkyQuality then begin
   result:=V.SkyQuality;
 end;
 except
 end;
 {$endif}
end;

function T_ascomweather.GetSkyTemperature: double;
begin
 result:=NullCoord;
 {$ifdef mswindows}
 try
 if FhasSkyTemperature then begin
   result:=V.SkyTemperature;
 end;
 except
 end;
 {$endif}
end;

function T_ascomweather.GetStarFWHM: double;
begin
 result:=NullCoord;
 {$ifdef mswindows}
 try
 if FhasStarFWHM then begin
   result:=V.StarFWHM;
 end;
 except
 end;
 {$endif}
end;

function T_ascomweather.GetTemperature: double;
begin
 result:=NullCoord;
 {$ifdef mswindows}
 try
 if FhasTemperature then begin
   result:=V.Temperature;
 end;
 except
 end;
 {$endif}
end;

function T_ascomweather.GetWindDirection: double;
begin
 result:=NullCoord;
 {$ifdef mswindows}
 try
 if FhasWindDirection then begin
   result:=V.WindDirection;
 end;
 except
 end;
 {$endif}
end;

function T_ascomweather.GetWindGust: double;
begin
 result:=NullCoord;
 {$ifdef mswindows}
 try
 if FhasWindGust then begin
   result:=V.WindGust;
 end;
 except
 end;
 {$endif}
end;

function T_ascomweather.GetWindSpeed: double;
begin
 result:=NullCoord;
 {$ifdef mswindows}
 try
 if FhasWindSpeed then begin
   result:=V.WindSpeed;
 end;
 except
 end;
 {$endif}
end;

function T_ascomweather.GetWeatherStatus: boolean;
begin
 result:=false;
 {$ifdef mswindows}
 try
 if FhasStatus then begin
   result:=V.IsSafe;
 end;
 except
 end;
 {$endif}
end;

procedure T_ascomweather.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

initialization
{$ifdef mswindows}
{$if defined(cpui386) or defined(cpux86_64)}
// some Ascom driver raise this exceptions
SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
{$endif}
{$endif}

end.

