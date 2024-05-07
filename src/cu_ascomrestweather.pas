unit cu_ascomrestweather;

{$mode objfpc}{$H+}

{
Copyright (C) 2019 Patrick Chevalley

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

uses cu_weather, cu_ascomrest, u_global, u_utils,
    u_translation, indiapi,
    Forms, ExtCtrls,Classes, SysUtils;

type
T_ascomrestweather = class(T_weather)
 private
   V: TAscomRest;
   stClear: boolean;
   FInterfaceVersion: integer;
   StatusTimer: TTimer;
   statusinterval: integer;
   procedure StatusTimerTimer(sender: TObject);
   function  Connected: boolean;
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
   function GetWeatherDetail: string; override;
   procedure SetTimeout(num:integer); override;
public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');  override;
   procedure Disconnect; override;
end;

implementation

constructor T_ascomrestweather.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 V:=TAscomRest.Create(self);
 V.ClientId:=3206;
 FweatherInterface:=ASCOMREST;
 FInterfaceVersion:=1;
 statusinterval:=2000;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=statusinterval;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomrestweather.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

procedure T_ascomrestweather.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
begin
  try
  FStatus := devConnecting;
  V.Host:=cp1;
  V.Port:=cp2;
  V.Protocol:=cp3;
  V.User:=cp5;
  V.Password:=cp6;
  Fdevice:=cp4;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  V.Device:=Fdevice;
  V.Timeout:=5000;
  V.Put('Connected',true);
  if V.Get('connected').AsBool then begin
     V.Timeout:=120000;
     try
     msg(V.Get('driverinfo').AsString,9);
     except
     end;
     try
       msg('Driver version: '+V.Get('driverversion').AsString,9);
     except
       msg('Error: unknown driver version',9);
     end;
     try
     FInterfaceVersion:=V.Get('interfaceversion').AsInt;
     except
       FInterfaceVersion:=1;
     end;
     if isLocalIP(V.RemoteIP) then
       statusinterval:=2000
     else
       statusinterval:=20000;
     msg('Interface version: '+inttostr(FInterfaceVersion),9);
     msg(rsConnected3);
     FStatus := devConnected;
     GetCapabilities;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     StatusTimer.Enabled:=true;
  end
  else
     Disconnect;
  except
   on E: Exception do begin
      msg(Format(rsConnectionEr, [E.Message]),0);
      Disconnect;
   end;
  end;
end;

procedure T_ascomrestweather.Disconnect;
begin
   StatusTimer.Enabled:=false;
   FStatus := devDisconnected;
   if Assigned(FonStatusChange) then FonStatusChange(self);
   try
     msg(rsDisconnected3,1);
     // the server is responsible for device disconnection
   except
     on E: Exception do msg(Format(rsDisconnectio, [E.Message]),0);
   end;
end;

function T_ascomrestweather.Connected: boolean;
begin
result:=false;
  try
  result:=V.Get('connected').AsBool;
  except
   result:=false;
  end;
end;

procedure T_ascomrestweather.StatusTimerTimer(sender: TObject);
var s: boolean;
    x: double;
begin
 StatusTimer.Enabled:=false;
 try
  if not Connected then begin
     FStatus := devDisconnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     msg(rsDisconnected3,1);
  end
  else begin
    try
      s:=GetClear;
      if s<>stClear then begin
        stClear:=s;
        if Assigned(FonClearChange) then FonClearChange(self);
      end;
      if ObsWeather then begin
        x:=GetPressure;
        if x<>NullCoord then ObsPressure:=x;
        x:=GetTemperature;
        if x<>NullCoord then ObsTemperature:=x;
        x:=GetHumidity;
        if x<>NullCoord then ObsHumidity:=x;
      end;
     except
     on E: Exception do msg('Status error: ' + E.Message,0);
    end;
  end;
  finally
   if FStatus=devConnected then StatusTimer.Enabled:=true;
  end;
end;

function  T_ascomrestweather.GetClear:boolean;
var x: double;
    nullcheck,ok: boolean;
begin
 result:=false;
 if FStatus<>devConnected then exit;
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
     if FhasCloudCover then begin
        x:=CloudCover;
        FCloudCover:=x;
        if UseCloudCover then begin
          ok:=(x>=MinCloudCover)and(x<=MaxCloudCover);
          result:=result and ok;
          if not ok then FWeatherMessage:=FWeatherMessage+' '+'CloudCover='+FormatFloat(f2,x);
          nullcheck:=true;
        end;
     end;
     if FhasDewPoint then begin
        x:=DewPoint;
        FDewPoint:=x;
        if UseDewPoint then begin
          ok:=(x>=MinDewPoint)and(x<=MaxDewPoint);
          result:=result and ok;
          if not ok then FWeatherMessage:=FWeatherMessage+' '+'DewPoint='+FormatFloat(f2,x);
          nullcheck:=true;
        end;
       end;
     if FhasHumidity then begin
        x:=Humidity;
        FHumidity:=x;
        if UseHumidity then begin
          ok:=(x>=MinHumidity)and(x<=MaxHumidity);
          result:=result and ok;
          if not ok then FWeatherMessage:=FWeatherMessage+' '+'Humidity='+FormatFloat(f2,x);
          nullcheck:=true;
        end;
     end;
     if FhasPressure then begin
        x:=Pressure;
        FPressure:=x;
        if UsePressure then begin
          ok:=(x>=MinPressure)and(x<=MaxPressure);
          result:=result and ok;
          if not ok then FWeatherMessage:=FWeatherMessage+' '+'Pressure='+FormatFloat(f2,x);
          nullcheck:=true;
        end;
     end;
     if FhasRainRate then begin
        x:=RainRate;
        FRainRate:=x;
        if UseRainRate then begin
          ok:=(x>=MinRainRate)and(x<=MaxRainRate);
          result:=result and ok;
          if not ok then FWeatherMessage:=FWeatherMessage+' '+'RainRate='+FormatFloat(f2,x);
          nullcheck:=true;
        end;
     end;
     if FhasSkyBrightness then begin
        x:=SkyBrightness;
        FSkyBrightness:=x;
        if UseSkyBrightness then begin
          ok:=(x>=MinSkyBrightness)and(x<=MaxSkyBrightness);
          result:=result and ok;
          if not ok then FWeatherMessage:=FWeatherMessage+' '+'SkyBrightness='+FormatFloat(f4,x);
          nullcheck:=true;
        end;
     end;
     if FhasSkyQuality then begin
        x:=SkyQuality;
        FSkyQuality:=x;
        if UseSkyQuality then begin
          ok:=(x>=MinSkyQuality)and(x<=MaxSkyQuality);
          result:=result and ok;
          if not ok then FWeatherMessage:=FWeatherMessage+' '+'SkyQuality='+FormatFloat(f2,x);
          nullcheck:=true;
        end;
       end;
     if FhasSkyTemperature then begin
        x:=SkyTemperature;
        FSkyTemperature:=x;
        if UseSkyTemperature then begin
          ok:=(x>=MinSkyTemperature)and(x<=MaxSkyTemperature);
          result:=result and ok;
          if not ok then FWeatherMessage:=FWeatherMessage+' '+'SkyTemperature='+FormatFloat(f2,x);
          nullcheck:=true;
        end;
     end;
     if FhasStarFWHM then begin
        x:=StarFWHM;
        FStarFWHM:=x;
        if UseStarFWHM then begin
          ok:=(x>=MinStarFWHM)and(x<=MaxStarFWHM);
          result:=result and ok;
          if not ok then FWeatherMessage:=FWeatherMessage+' '+'StarFWHM='+FormatFloat(f2,x);
          nullcheck:=true;
        end;
     end;
     if FhasTemperature then begin
        x:=Temperature;
        FTemperature:=x;
        if UseTemperature then begin
          ok:=(x>=MinTemperature)and(x<=MaxTemperature);
          result:=result and ok;
          if not ok then FWeatherMessage:=FWeatherMessage+' '+'Temperature='+FormatFloat(f2,x);
          nullcheck:=true;
        end;
     end;
     if FhasWindDirection then begin
        x:=WindDirection;
        FWindDirection:=x;
        if UseWindDirection then begin
          ok:=(x>=MinWindDirection)and(x<=MaxWindDirection);
          result:=result and ok;
          if not ok then FWeatherMessage:=FWeatherMessage+' '+'WindDirection='+FormatFloat(f2,x);
          nullcheck:=true;
        end;
     end;
     if FhasWindGust then begin
        x:=WindGust;
        FWindGust:=x;
        if UseWindGust then begin
          ok:=(x>=MinWindGust)and(x<=MaxWindGust);
          result:=result and ok;
          if not ok then FWeatherMessage:=FWeatherMessage+' '+'WindGust='+FormatFloat(f2,x);
          nullcheck:=true;
        end;
     end;
     if FhasWindSpeed then begin
        x:=WindSpeed;
        FWindSpeed:=x;
        if UseWindSpeed then begin
          ok:=(x>=MinWindSpeed)and(x<=MaxWindSpeed);
          result:=result and ok;
          if not ok then FWeatherMessage:=FWeatherMessage+' '+'WindSpeed='+FormatFloat(f2,x);
          nullcheck:=true;
        end;
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

procedure T_ascomrestweather.GetCapabilities;
begin
 if FStatus<>devConnected then exit;
 try
   FhasCloudCover:=false;
   DummyDouble:=V.Get('cloudcover').AsFloat;
   FhasCloudCover:=true;
 except
 end;
 try
   FhasDewPoint:=false;
   DummyDouble:=V.Get('dewpoint').AsFloat;
   FhasDewPoint:=true;
 except
 end;
 try
   FhasHumidity:=false;
   DummyDouble:=V.Get('humidity').AsFloat;
   FhasHumidity:=true;
 except
 end;
 try
   FhasPressure:=false;
   DummyDouble:=V.Get('pressure').AsFloat;
   FhasPressure:=true;
 except
 end;
 try
   FhasRainRate:=false;
   DummyDouble:=V.Get('rainrate').AsFloat;
   FhasRainRate:=true;
 except
 end;
 try
   FhasSkyBrightness:=false;
   DummyDouble:=V.Get('skybrightness').AsFloat;
   FhasSkyBrightness:=true;
 except
 end;
 try
   FhasSkyQuality:=false;
   DummyDouble:=V.Get('skyquality').AsFloat;
   FhasSkyQuality:=true;
 except
 end;
 try
   FhasSkyTemperature:=false;
   DummyDouble:=V.Get('skytemperature').AsFloat;
   FhasSkyTemperature:=true;
 except
 end;
 try
   FhasStarFWHM:=false;
   DummyDouble:=V.Get('starfwhm').AsFloat;
   FhasStarFWHM:=true;
 except
 end;
 try
   FhasTemperature:=false;
   DummyDouble:=V.Get('temperature').AsFloat;
   FhasTemperature:=true;
 except
 end;
 try
   FhasWindDirection:=false;
   DummyDouble:=V.Get('winddirection').AsFloat;
   FhasWindDirection:=true;
 except
 end;
 try
   FhasWindGust:=false;
   DummyDouble:=V.Get('windgust').AsFloat;
   FhasWindGust:=true;
 except
 end;
 try
   FhasWindSpeed:=false;
   DummyDouble:=V.Get('windspeed').AsFloat;
   FhasWindSpeed:=true;
 except
 end;
 try
   FhasStatus:=false;
   DummyBool:=V.Get('issafe').AsBool;
   FhasStatus:=true;
 except
 end;
end;

function T_ascomrestweather.GetCloudCover: double;
begin
 result:=NullCoord;
 if FStatus<>devConnected then exit;
 try
 if FhasCloudCover then begin
   result:=V.Get('cloudcover').AsFloat;
 end;
 except
 end;
end;

function T_ascomrestweather.GetDewPoint: double;
begin
 result:=NullCoord;
 if FStatus<>devConnected then exit;
 try
 if FhasDewPoint then begin
   result:=V.Get('dewpoint').AsFloat;
 end;
 except
 end;
end;

function T_ascomrestweather.GetHumidity: double;
begin
 result:=NullCoord;
 if FStatus<>devConnected then exit;
 try
 if FhasHumidity then begin
   result:=V.Get('humidity').AsFloat;
 end;
 except
 end;
end;

function T_ascomrestweather.GetPressure: double;
begin
 result:=NullCoord;
 if FStatus<>devConnected then exit;
 try
 if FhasPressure then begin
   result:=V.Get('pressure').AsFloat;
 end;
 except
 end;
end;

function T_ascomrestweather.GetRainRate: double;
begin
 result:=NullCoord;
 if FStatus<>devConnected then exit;
 try
 if FhasRainRate then begin
   result:=V.Get('rainrate').AsFloat;
 end;
 except
 end;
end;

function T_ascomrestweather.GetSkyBrightness: double;
begin
 result:=NullCoord;
 if FStatus<>devConnected then exit;
 try
 if FhasSkyBrightness then begin
   result:=V.Get('skybrightness').AsFloat;
 end;
 except
 end;
end;

function T_ascomrestweather.GetSkyQuality: double;
begin
 result:=NullCoord;
 if FStatus<>devConnected then exit;
 try
 if FhasSkyQuality then begin
   result:=V.Get('skyquality').AsFloat;
 end;
 except
 end;
end;

function T_ascomrestweather.GetSkyTemperature: double;
begin
 result:=NullCoord;
 if FStatus<>devConnected then exit;
 try
 if FhasSkyTemperature then begin
   result:=V.Get('skytemperature').AsFloat;
 end;
 except
 end;
end;

function T_ascomrestweather.GetStarFWHM: double;
begin
 result:=NullCoord;
 if FStatus<>devConnected then exit;
 try
 if FhasStarFWHM then begin
   result:=V.Get('starfwhm').AsFloat;
 end;
 except
 end;
end;

function T_ascomrestweather.GetTemperature: double;
begin
 result:=NullCoord;
 if FStatus<>devConnected then exit;
 try
 if FhasTemperature then begin
   result:=V.Get('temperature').AsFloat;
 end;
 except
 end;
end;

function T_ascomrestweather.GetWindDirection: double;
begin
 result:=NullCoord;
 if FStatus<>devConnected then exit;
 try
 if FhasWindDirection then begin
   result:=V.Get('winddirection').AsFloat;
 end;
 except
 end;
end;

function T_ascomrestweather.GetWindGust: double;
begin
 result:=NullCoord;
 if FStatus<>devConnected then exit;
 try
 if FhasWindGust then begin
   result:=V.Get('windgust').AsFloat;
 end;
 except
 end;
end;

function T_ascomrestweather.GetWindSpeed: double;
begin
 result:=NullCoord;
 if FStatus<>devConnected then exit;
 try
 if FhasWindSpeed then begin
   result:=V.Get('windspeed').AsFloat;
 end;
 except
 end;
end;

function T_ascomrestweather.GetWeatherStatus: boolean;
begin
 result:=false;
 if FStatus<>devConnected then exit;
 try
 if FhasStatus then begin
   result:=V.Get('issafe').AsBool;
 end;
 except
 end;
end;

procedure T_ascomrestweather.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

function T_ascomrestweather.GetWeatherDetail: string;
var x: double;
    ok: boolean;
begin
 result:='';
 if FStatus<>devConnected then exit;
 try
   if FhasStatus then begin
     // SafetyMonitor interface
     result:='IsSafe='+BoolToStr(WeatherStatus,True);
   end
   else begin
     // ObservingConditions interface
     if FhasCloudCover then begin
        x:=FCloudCover;
        result:=result+crlf+'CloudCover='+FormatFloat(f2,x);
     end;
     if FhasDewPoint then begin
        x:=FDewPoint;
        result:=result+crlf+'DewPoint='+FormatFloat(f2,x);
     end;
     if FhasHumidity then begin
        x:=FHumidity;
        result:=result+crlf+'Humidity='+FormatFloat(f2,x);
     end;
     if FhasPressure then begin
        x:=FPressure;
        result:=result+crlf+'Pressure='+FormatFloat(f2,x);
     end;
     if FhasRainRate then begin
        x:=FRainRate;
        result:=result+crlf+'RainRate='+FormatFloat(f2,x);
     end;
     if FhasSkyBrightness then begin
        x:=FSkyBrightness;
        result:=result+crlf+'SkyBrightness='+FormatFloat(f4,x);
     end;
     if FhasSkyQuality then begin
        x:=FSkyQuality;
        result:=result+crlf+'SkyQuality='+FormatFloat(f2,x);
     end;
     if FhasSkyTemperature then begin
        x:=FSkyTemperature;
        result:=result+crlf+'SkyTemperature='+FormatFloat(f2,x);
     end;
     if FhasStarFWHM then begin
        x:=FStarFWHM;
        result:=result+crlf+'StarFWHM='+FormatFloat(f2,x);
     end;
     if FhasTemperature then begin
        x:=FTemperature;
        result:=result+crlf+'Temperature='+FormatFloat(f2,x);
     end;
     if FhasWindDirection then begin
        x:=FWindDirection;
        result:=result+crlf+'WindDirection='+FormatFloat(f2,x);
     end;
     if FhasWindGust then begin
        x:=FWindGust;
        result:=result+crlf+'WindGust='+FormatFloat(f2,x);
     end;
     if FhasWindSpeed then begin
        x:=FWindSpeed;
        result:=result+crlf+'WindSpeed='+FormatFloat(f2,x);
     end;
     result:=result+crlf;
   end;
   except
    on E: Exception do begin
     result:=result+crlf+e.Message;
    end;
   end;
end;

end.

