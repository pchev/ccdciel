unit cu_ascomrestmount;

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

uses  cu_mount, cu_ascomrest, u_global,  indiapi,
    u_translation, u_utils,
  Forms, ExtCtrls, Classes, SysUtils;

type
T_ascomrestmount = class(T_mount)
 private
   V: TAscomRest;
   CanPark,CanSlew,CanSlewAsync,CanSetPierSide,CanSync,CanSetTracking: boolean;
   stRA,stDE: double;
   stPark:boolean;
   stPierside: TPierSide;
   stTracking: boolean;
   StatusTimer: TTimer;
   function Connected: boolean;
   procedure StatusTimerTimer(sender: TObject);
   procedure CheckEqmod;
   function WaitMountSlewing(maxtime:integer):boolean;
   function WaitMountPark(maxtime:integer):boolean;
 protected
   function  GetTracking:Boolean; override;
   procedure SetPark(value:Boolean); override;
   function  GetPark:Boolean; override;
   function  GetRA:double; override;
   function  GetDec:double; override;
   function  GetPierSide: TPierSide; override;
   function  GetEquinox: double; override;
   function  GetAperture:double; override;
   function  GetFocaleLength:double; override;
   procedure SetTimeout(num:integer); override;
   function  GetSyncMode:TEqmodAlign; override;
   procedure SetSyncMode(value:TEqmodAlign); override;
   function GetMountSlewing:boolean; override;
public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string=''); override;
   procedure Disconnect; override;
   function Slew(sra,sde: double):boolean; override;
   function SlewAsync(sra,sde: double):boolean; override;
   function FlipMeridian: boolean; override;
   function Sync(sra,sde: double):boolean; override;
   function Track:boolean; override;
   procedure AbortMotion; override;
   function ClearAlignment:boolean; override;
   function ClearDelta:boolean; override;
   function GetSite(var long,lat,elev: double): boolean; override;
   function SetSite(long,lat,elev: double): boolean; override;
   function GetDate(var utc,offset: double): boolean; override;
   function SetDate(utc,offset: double): boolean; override;
end;

const waitpoll=1000;
      statusinterval=3000;

implementation

constructor T_ascomrestmount.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 V:=TAscomRest.Create(self);
 V.ClientId:=3203;
 FMountInterface:=ASCOMREST;
 stRA:=0;
 stDE:=0;;
 stPark:=false;
 stPierside:=pierUnknown;
 stTracking:=false;
 CanPark:=false;
 CanSlew:=false;
 CanSlewAsync:=false;
 CanSetPierSide:=false;
 CanSync:=false;
 CanSetTracking:=false;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=statusinterval;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomrestmount.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

procedure T_ascomrestmount.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
var buf: string;
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
  V.Timeout:=2000;
  V.Put('Connected',true);
  if V.Get('Connected').AsBool then begin
     V.Timeout:=120000;
     try
     msg('Driver version: '+V.Get('DriverVersion').AsString,9);
     except
       msg('Error: unknown driver version',9);
     end;
     CheckEqmod;
     CanPark:=V.Get('CanPark').AsBool;
     CanSlew:=V.Get('CanSlew').AsBool;
     CanSlewAsync:=V.Get('CanSlewAsync').AsBool;
     CanSetPierSide:=V.Get('CanSetPierSide').AsBool;
     CanSync:=V.Get('CanSync').AsBool;
     CanSetTracking:=V.Get('CanSetTracking').AsBool;
     buf:='';
     if IsEqmod then buf:=buf+'EQmod ';
     if CanPark then buf:=buf+'CanPark ';
     if CanSlew then buf:=buf+'CanSlew ';
     if CanSlewAsync then buf:=buf+'CanSlewAsync ';
     if CanSetPierSide then buf:=buf+'CanSetPierSide ';
     if CanSync then buf:=buf+'CanSync ';
     if CanSetTracking then buf:=buf+'CanSetTracking ';
     msg(rsConnected3);
     msg(Format(rsMountCapabil, [buf]));
     FStatus := devConnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     if Assigned(FonParkChange) then FonParkChange(self);
     if Assigned(FonPiersideChange) then FonPiersideChange(self);
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

procedure T_ascomrestmount.Disconnect;
begin
   StatusTimer.Enabled:=false;
   FStatus := devDisconnected;
   if Assigned(FonStatusChange) then FonStatusChange(self);
   try
     msg(rsDisconnected3,0);
     V.Put('Connected',false);
   except
     on E: Exception do msg(Format(rsDisconnectio, [E.Message]),0);
   end;
end;

function T_ascomrestmount.Connected: boolean;
begin
result:=false;
  try
  result:=V.Get('Connected').AsBool;
  except
   result:=false;
  end;
end;

procedure T_ascomrestmount.StatusTimerTimer(sender: TObject);
var x,y: double;
    pk: boolean;
    ps: TPierSide;
    tr: Boolean;
begin
 StatusTimer.Enabled:=false;
 try
  if not Connected then begin
     FStatus := devDisconnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
  end
  else begin
    try
    x:=GetRA;
    y:=GetDec;
    pk:=GetPark;
    ps:=GetPierSide;
    tr:=GetTracking;
    if (x<>stRA)or(y<>stDE) then begin
       stRA:=x;
       stDE:=y;
       if Assigned(FonCoordChange) then FonCoordChange(self);
    end;
    if pk<>stPark then begin
       stPark:=pk;
       if Assigned(FonParkChange) then FonParkChange(self);
    end;
    if ps<>stPierside then begin
       stPierside:=ps;
       if Assigned(FonPiersideChange) then FonPiersideChange(self);
    end;
    if tr<>stTracking then begin
       stTracking:=tr;
       if Assigned(FonTrackingChange) then FonTrackingChange(self);
    end;
    except
     on E: Exception do msg('Status error: ' + E.Message,0);
    end;
  end;
  finally
   StatusTimer.Enabled:=true;
  end;
end;

procedure T_ascomrestmount.SetPark(value:Boolean);
begin
   try
   if CanPark then begin
      if value then begin
         msg(rsPark);
         V.Put('Park');
         WaitMountPark(120000);
      end else begin
         msg(rsUnpark);
         V.Put('UnPark');
      end;
   end;
   except
    on E: Exception do msg('Park error: ' + E.Message,0);
   end;
end;

function  T_ascomrestmount.GetPark:Boolean;
begin
 result:=false;
   try
   result:=V.Get('AtPark').AsBool;
   except
    result:=false;
   end;
end;

function  T_ascomrestmount.GetRA:double;
begin
 result:=NullCoord;
   try
   result:=V.Get('RightAscension').AsFloat;
   except
    result:=NullCoord;
   end;
end;

function  T_ascomrestmount.GetDec:double;
begin
 result:=NullCoord;
   try
   result:=V.Get('Declination').AsFloat;
   except
    result:=NullCoord;
   end;
end;

function  T_ascomrestmount.GetPierSide:TPierSide;
var i: integer;
begin
 result:=pierUnknown;
   try
   i:=V.Get('SideOfPier').AsInt;  // pascal enum may have different size
   case i of
     -1: result:=pierUnknown;
      0: result:=pierEast;
      1: result:=pierWest;
   end;
   except
    result:=pierUnknown;
   end;
end;

function  T_ascomrestmount.GetEquinox: double;
var i: Integer;
begin
 result:=0;
  try
  i:=V.Get('EquatorialSystem').AsInt;
  case i of
  0 : result:=0;
  1 : result:=0;
  2 : result:=2000;
  3 : result:=2050;
  4 : result:=1950;
  end;
  except
   result:=0;
  end;
end;

function  T_ascomrestmount.GetAperture:double;
begin
 result:=-1;
   try
   result:=V.Get('ApertureDiameter').AsFloat*1000;
   except
    result:=-1;
   end;
end;

function  T_ascomrestmount.GetFocaleLength:double;
begin
 result:=-1;
   try
   result:=V.Get('FocalLength').AsFloat*1000;
   except
    result:=-1;
   end;
end;

function T_ascomrestmount.SlewAsync(sra,sde: double):boolean;
begin
 result:=false;
 if CanSlew then begin
   try
   if CanSetTracking and (not V.Get('Tracking').AsBool) then begin
     try
      V.Put('Tracking',true);
     except
       on E: Exception do msg('Set tracking error: ' + E.Message,0);
     end;
   end;
   if Equinox=0 then
      msg(Format(rsSlewTo, [ARToStr3(sra), DEToStr(sde)]))
   else
      msg(Format(rsSlewToEQ, ['J'+inttostr(round(Equinox)) ,ARToStr3(sra), DEToStr(sde)]));
   if CanSlewAsync then begin
     V.Put('SlewToCoordinatesAsync',['RightAscension',FormatFloat(f6,sra),'Declination',FormatFloat(f6,sde)]);
   end
   else
     V.Put('SlewToCoordinates',['RightAscension',FormatFloat(f6,sra),'Declination',FormatFloat(f6,sde)]);
   result:=true;
   except
     on E: Exception do msg('Slew error: ' + E.Message,0);
   end;
 end;
end;

function T_ascomrestmount.Slew(sra,sde: double):boolean;
begin
 result:=false;
 if CanSlew then begin
   try
   if CanSetTracking and (not V.Get('Tracking').AsBool) then begin
     try
      V.Put('Tracking',true);
     except
       on E: Exception do msg('Set tracking error: ' + E.Message,0);
     end;
   end;
   FMountSlewing:=true;
   if Equinox=0 then
      msg(Format(rsSlewTo, [ARToStr3(sra), DEToStr(sde)]))
   else
      msg(Format(rsSlewToEQ, ['J'+inttostr(round(Equinox)) ,ARToStr3(sra), DEToStr(sde)]));
   if CanSlewAsync then begin
     V.Put('SlewToCoordinatesAsync',['RightAscension',FormatFloat(f6,sra),'Declination',FormatFloat(f6,sde)]);
     WaitMountSlewing(120000);
   end
   else
     V.Put('SlewToCoordinates',['RightAscension',FormatFloat(f6,sra),'Declination',FormatFloat(f6,sde)]);
   wait(2);
   msg(rsSlewComplete);
   FMountSlewing:=false;
   result:=true;
   except
     on E: Exception do msg('Slew error: ' + E.Message,0);
   end;
 end;
end;

function T_ascomrestmount.GetMountSlewing:boolean;
var islewing: boolean;
begin
 result:=false;
 try
 islewing:=false;
  if CanSlewAsync then
    islewing:=V.Get('Slewing').AsBool
  else
    islewing:=false;
  result:=(islewing or FMountSlewing);
  except
    on E: Exception do msg('Get slewing error: ' + E.Message,0);
  end;
end;

function T_ascomrestmount.WaitMountSlewing(maxtime:integer):boolean;
var count,maxcount:integer;
begin
 result:=true;
 try
 if CanSlewAsync then begin
   maxcount:=maxtime div waitpoll;
   count:=0;
   while (V.Get('Slewing').AsBool)and(count<maxcount) do begin
      sleep(waitpoll);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
      inc(count);
   end;
   result:=(count<maxcount);
 end;
 except
   result:=false;
 end;
end;

function T_ascomrestmount.WaitMountPark(maxtime:integer):boolean;
var count,maxcount:integer;
begin
 result:=true;
 try
 if CanPark then begin
   maxcount:=maxtime div waitpoll;
   count:=0;
   while (not V.Get('atPark').AsBool)and(count<maxcount) do begin
      sleep(waitpoll);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
      inc(count);
   end;
   result:=(count<maxcount);
 end;
 except
   result:=false;
 end;
end;

function T_ascomrestmount.FlipMeridian:boolean;
var sra,sde,ra1,ra2: double;
    pierside1,pierside2:TPierSide;
begin
  result:=false;
  if Connected then begin
    sra:=GetRA;
    sde:=GetDec;
    pierside1:=GetPierSide;
    if pierside1=pierEast then exit; // already right side
    if (sra=NullCoord)or(sde=NullCoord) then exit;
    msg(rsMeridianFlip5);
    {TODO: someone with a mount that support this feature can test it}
{    if CanSetPierSide then begin
       // do the flip
       V.SideOfPier:=0; // pierEast
       WaitMountSlewing(240000);
       // return to position
       slew(sra,sde);
       WaitMountSlewing(240000);
       // check result
       pierside2:=GetPierSide;
       result:=(pierside2<>pierside1);
    end
    else} begin
      // point one hour to the east
      ra1:=sra+1;
      if ra1>=24 then ra1:=ra1-24;
      slew(ra1,sde);
      // point one hour to the west to force the flip
      ra2:=sra-1;
      if ra2<0 then ra2:=ra2+24;
      slew(ra2,sde);
      // return to position
      slew(sra,sde);
      // check result
      pierside2:=GetPierSide;
      result:=(pierside2<>pierside1);
    end;
  end;
end;

function T_ascomrestmount.Sync(sra,sde: double):boolean;
begin
 result:=false;
 result:=false;
 if CanSync then begin
   try
   if CanSetTracking and (not V.Get('Tracking').AsBool) then begin
     msg(rsCannotSyncWh,0);
     exit;
   end;
   if Equinox=0 then
      msg(Format(rsSyncTo, [ARToStr3(sra), DEToStr(sde)]))
   else
      msg(Format(rsSyncToEQ, ['J'+inttostr(round(Equinox)) ,ARToStr3(sra), DEToStr(sde)]));
   V.Put('SyncToCoordinates',['RightAscension',FormatFloat(f6,sra),'Declination',FormatFloat(f6,sde)]);
   result:=true;
   except
     on E: Exception do msg('Sync error: ' + E.Message,0);
   end;
 end;
end;

function T_ascomrestmount.GetTracking:Boolean;
begin
 result:=true;
   try
   result:=V.Get('Tracking').AsBool;
   except
   end;
end;

function T_ascomrestmount.Track:boolean;
begin
 result:=false;
   try
   if CanSetTracking and (not V.Get('Tracking').AsBool) then begin
     try
      msg(rsStartTraking);
      V.Put('Tracking',true);
     except
       on E: Exception do msg('Set tracking error: ' + E.Message,0);
     end;
   end;
   result:=true;
   except
     on E: Exception do msg('Track error: ' + E.Message,0);
   end;
end;

procedure T_ascomrestmount.AbortMotion;
begin
 if CanSlew then begin
   try
   msg(rsStopTelescop);
   V.Put('AbortSlew');
   if CanSetTracking  then V.Put('Tracking',false);
   except
     on E: Exception do msg('Abort motion error: ' + E.Message,0);
   end;
 end;
end;

procedure T_ascomrestmount.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

// Eqmod specific

procedure T_ascomrestmount.CheckEqmod;
var buf:string;
begin
  FIsEqmod:=false;
    try
    buf:=V.PutR('CommandString',['Command',':MOUNTVER#','Raw','true']).AsString;
    if length(buf)=8 then FIsEqmod:=true;
    except
     FIsEqmod:=false;
    end;
end;

function  T_ascomrestmount.GetSyncMode:TEqmodAlign;
var buf:string;
begin
 result:=alUNSUPPORTED;
 if IsEqmod then begin
   try
   buf:=V.PutR('CommandString',['Command',':ALIGN_MODE#','Raw','true']).AsString;
   if buf='1#' then result:=alADDPOINT
   else if buf='0#' then result:=alSTDSYNC;
   except
    result:=alUNSUPPORTED;
   end;
 end
 else result:=alUNSUPPORTED;
end;

procedure T_ascomrestmount.SetSyncMode(value:TEqmodAlign);
begin
 if IsEqmod and (value<>alUNSUPPORTED) then begin
   try
   if value=alSTDSYNC then begin
     msg('align mode Std Sync');
     V.Put('CommandString',['Command',':ALIGN_MODE,0#','Raw','true']);
   end
   else if value=alADDPOINT then begin
     msg('align mode Add Point');
     V.Put('CommandString',['Command',':ALIGN_MODE,1#','Raw','true']);
   end;
   except
     on E: Exception do msg('Eqmod set sync mode error: ' + E.Message,0);
   end;
 end;
end;

function T_ascomrestmount.ClearAlignment:boolean;
begin
 result:=false;
 if IsEqmod then begin
   try
   msg('clear alignment');
   V.Put('CommandString',['Command',':ALIGN_CLEAR_POINTS#','Raw','true']);
   result:=true;
   except
     on E: Exception do msg('Eqmod clear alignment error: ' + E.Message,0);
   end;
 end;
end;

function T_ascomrestmount.ClearDelta:boolean;
begin
 result:=false;
 if IsEqmod then begin
   try
   msg('clear delta sync');
   V.Put('CommandString',['Command',':ALIGN_CLEAR_SYNC#','Raw','true']);
   result:=true;
   except
     on E: Exception do msg('Eqmod clear delta error: ' + E.Message,0);
   end;
 end;
end;

function T_ascomrestmount.GetSite(var long,lat,elev: double): boolean;
begin
 result:=false;
   try
   long:=V.Get('SiteLongitude').AsFloat;
   lat:=V.Get('SiteLatitude').AsFloat;
   elev:=V.Get('SiteElevation').AsFloat;
   result:=true;
   except
     on E: Exception do msg('Cannot get site information: ' + E.Message,0);
   end;
end;

function T_ascomrestmount.SetSite(long,lat,elev: double): boolean;
begin
 result:=false;
   try
   V.Put('SiteLongitude',long);
   V.Put('SiteLatitude',lat);
   V.Put('SiteElevation',elev);
   result:=true;
   except
     on E: Exception do msg('Cannot set site information: ' + E.Message,0);
   end;
end;

function T_ascomrestmount.GetDate(var utc,offset: double): boolean;
begin
 result:=false;
   try
   utc:=DateIso2DateTime(V.Get('UTCDate').AsString);     //2019-01-30T18:15:23.734
   offset:=ObsTimeZone; // No offset in ASCOM telescope interface
   result:=true;
   except
     on E: Exception do msg('Cannot get date: ' + E.Message,0);
   end;
end;

function T_ascomrestmount.SetDate(utc,offset: double): boolean;
begin
 result:=false;
   try
   V.Put('UTCDate',FormatDateTime(dateiso,utc));
   result:=true;
   except
     on E: Exception do msg('Cannot set date: ' + E.Message,0);
   end;
end;


end.

