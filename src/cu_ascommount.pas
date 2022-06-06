unit cu_ascommount;

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

uses  cu_mount, u_global,  indiapi,
  {$ifdef mswindows}
    u_translation, Variants, comobj, u_utils, math,
  {$endif}
  Forms, ExtCtrls, Classes, SysUtils;

type
T_ascommount = class(T_mount)
 private
   {$ifdef mswindows}
   V: variant;
   {$endif}
   CanPark,CanSlew,CanSlewAsync,CanSetPierSide,CanSync,CanSetTracking: boolean;
   stRA,stDE: double;
   stPark:boolean;
   stPierside: TPierSide;
   stTracking: boolean;
   StatusTimer: TTimer;
   FInterfaceVersion: integer;
   function Connected: boolean;
   procedure StatusTimerTimer(sender: TObject);
   procedure CheckEqmod;
   function WaitMountSlewing(maxtime:integer):boolean;
   function WaitMountPark(maxtime:integer):boolean;
 protected
   function  GetTracking:Boolean; override;
   function  getCanSetGuideRates:Boolean; override;
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
   function GetGuideRateRa: double; override;
   function GetGuideRateDe: double; override;
   procedure SetGuideRateRa(value:double); override;
   procedure SetGuideRateDe(value:double); override;
   function GetPulseGuiding: boolean; override;
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
   function PulseGuide(direction,duration:integer): boolean; override;
end;

const waitpoll=500;
      statusinterval=2000;

implementation

constructor T_ascommount.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FMountInterface:=ASCOM;
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

destructor  T_ascommount.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

procedure T_ascommount.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
{$ifdef mswindows}
var buf: string;
    j: double;
{$endif}
begin
 {$ifdef mswindows}
  try
  if debug_msg then msg('Start connection '+cp1);
  FStatus := devConnecting;
  FDevice:=cp1;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  V:=Unassigned;
  V:=CreateOleObject(Fdevice);
  V.connected:=true;
  if V.connected then begin
     if debug_msg then msg('Connect OK');
     FStatus := devConnected;
     try
     msg(V.DriverInfo,9);
     except
     end;
     try
     msg('Driver version: '+V.DriverVersion,9);
     except
       msg('Error: unknown driver version',9);
     end;
     try
     FInterfaceVersion:=V.InterfaceVersion;
     except
       FInterfaceVersion:=1;
     end;
     msg('Interface version: '+inttostr(FInterfaceVersion),9);
     CheckEqmod;
     if debug_msg then msg('Get park capability');
     CanPark:=V.CanPark;
     if debug_msg then msg('Get slew capability');
     CanSlew:=V.CanSlew;
     if debug_msg then msg('Get slewasync capability');
     CanSlewAsync:=V.CanSlewAsync;
     if debug_msg then msg('Get side of pier');
     CanSetPierSide:=V.CanSetPierSide;
     if debug_msg then msg('Get sync capability');
     CanSync:=V.CanSync;
     if debug_msg then msg('Get set tracking capability');
     CanSetTracking:=V.CanSetTracking;
     if debug_msg then msg('Get pulse guiding capability');
     FCanPulseGuide:=V.CanPulseGuide;
     buf:='';
     if IsEqmod then buf:=buf+'EQmod ';
     if CanPark then buf:=buf+'CanPark ';
     if CanSlew then buf:=buf+'CanSlew ';
     if CanSlewAsync then buf:=buf+'CanSlewAsync ';
     if CanSetPierSide then buf:=buf+'CanSetPierSide ';
     if CanSync then buf:=buf+'CanSync ';
     if CanSetTracking then buf:=buf+'CanSetTracking ';
     FEquinox:=NullCoord;
     FEquinoxJD:=NullCoord;
     j:=GetEquinox;
     if j=0 then buf:=buf+'EquatorialSystem: Local '
            else buf:=buf+'EquatorialSystem: '+FormatFloat(f0,j)+' ';
     msg(rsConnected3);
     msg(Format(rsMountCapabil, [buf]));
     if Assigned(FonStatusChange) then FonStatusChange(self);
     if Assigned(FonParkChange) then FonParkChange(self);
     if Assigned(FonPiersideChange) then FonPiersideChange(self);
     StatusTimer.Enabled:=true;
  end
  else begin
     msg('connect failed');
     Disconnect;
  end;
  except
    on E: Exception do msg('Connection error: ' + E.Message,0);
  end;
 {$endif}
end;

procedure T_ascommount.Disconnect;
begin
 {$ifdef mswindows}
   StatusTimer.Enabled:=false;
   FStatus := devDisconnected;
   if debug_msg then msg('Request to disconnect');
   if Assigned(FonStatusChange) then FonStatusChange(self);
   try
   if not VarIsEmpty(V) then begin
     msg(rsDisconnected3,0);
     V.connected:=false;
     V:=Unassigned;
   end;
   except
     on E: Exception do msg('Disconnection error: ' + E.Message,0);
   end;
 {$endif}
end;

function T_ascommount.Connected: boolean;
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

procedure T_ascommount.StatusTimerTimer(sender: TObject);
{$ifdef mswindows}
var x,y: double;
    pk: boolean;
    ps: TPierSide;
    tr: Boolean;
  {$endif}
begin
 {$ifdef mswindows}
 try
 StatusTimer.Enabled:=false;
  if not Connected then begin
     if debug_msg then msg('Status not connected');
     FStatus := devDisconnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
  end
  else begin
    try
    if debug_msg then msg('query status',9);
    x:=GetRA;
    y:=GetDec;
    pk:=GetPark;
    ps:=GetPierSide;
    tr:=GetTracking;
    if (x<>stRA)or(y<>stDE) then begin
       if debug_msg then msg('coordinate change',9);
       stRA:=x;
       stDE:=y;
       if Assigned(FonCoordChange) then FonCoordChange(self);
    end;
    if pk<>stPark then begin
       if debug_msg then msg('park change',9);
       stPark:=pk;
       if Assigned(FonParkChange) then FonParkChange(self);
    end;
    if ps<>stPierside then begin
       if debug_msg then msg('pier side change',9);
       stPierside:=ps;
       if Assigned(FonPiersideChange) then FonPiersideChange(self);
    end;
    if tr<>stTracking then begin
       if debug_msg then msg('tracking change',9);
       stTracking:=tr;
       if Assigned(FonTrackingChange) then FonTrackingChange(self);
    end;
    except
     on E: Exception do msg('Status error: ' + E.Message,0);
    end;
  end;
  finally
  if FStatus=devConnected then StatusTimer.Enabled:=true;
  end;
 {$endif}
end;

procedure T_ascommount.SetPark(value:Boolean);
begin
 {$ifdef mswindows}
   try
   if CanPark then begin
      if debug_msg then msg('set park '+BoolToStr(value,true));
      if value then begin
         msg(rsPark);
         V.Park;
         WaitMountPark(120000);
      end else begin
         msg(rsUnpark);
         V.UnPark;
      end;
   end;
   except
    on E: Exception do msg('Park error: ' + E.Message,0);
   end;
 {$endif}
end;

function  T_ascommount.GetPark:Boolean;
begin
 result:=false;
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
   try
   result:=V.AtPark;
   except
    result:=false;
   end;
 end;
 {$endif}
end;

function  T_ascommount.GetRA:double;
begin
 result:=NullCoord;
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
   try
   result:=V.RightAscension;
   except
    result:=NullCoord;
   end;
 end;
 {$endif}
end;

function  T_ascommount.GetDec:double;
begin
 result:=NullCoord;
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
   try
   result:=V.Declination;
   except
    result:=NullCoord;
   end;
 end;
 {$endif}
end;

function  T_ascommount.GetPierSide:TPierSide;
{$ifdef mswindows}
var i: integer;
{$endif}
begin
 result:=pierUnknown;
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
   try
   i:=V.SideOfPier;  // pascal enum may have different size
   case i of
     -1: result:=pierUnknown;
      0: result:=pierEast;
      1: result:=pierWest;
   end;
   except
    result:=pierUnknown;
   end;
 end;
 {$endif}
end;

function  T_ascommount.GetEquinox: double;
{$ifdef mswindows}
var i: Integer;
{$endif}
begin
 result:=0;
{$ifdef mswindows}
if not VarIsEmpty(V) then begin
  try
  i:=V.EquatorialSystem;
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
{$endif}
end;

function  T_ascommount.GetAperture:double;
begin
 result:=-1;
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
   try
   result:=V.ApertureDiameter*1000;
   except
    result:=-1;
   end;
 end;
 {$endif}
end;

function  T_ascommount.GetFocaleLength:double;
begin
 result:=-1;
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
   try
   result:=V.FocalLength*1000;
   except
    result:=-1;
   end;
 end;
 {$endif}
end;

function T_ascommount.SlewAsync(sra,sde: double):boolean;
begin
 result:=false;
 {$ifdef mswindows}
 if CanSlew then begin
   if debug_msg then msg('request to slew async to '+formatfloat(f5,sra)+blank+formatfloat(f5,sde));
   try
   if CanSetTracking and (not V.tracking) then begin
     try
      if debug_msg then msg('must set tracking');
      V.tracking:=true;
     except
       on E: Exception do msg('Set tracking error: ' + E.Message,0);
     end;
   end;
   if Equinox=0 then
      msg(Format(rsSlewToEQ, ['Local', ARToStr3(sra), DEToStr(sde)]))
   else
      msg(Format(rsSlewToEQ, ['J'+inttostr(round(Equinox)) ,ARToStr3(sra), DEToStr(sde)]));
   if CanSlewAsync then begin
     if debug_msg then msg('slew async');
     V.SlewToCoordinatesAsync(sra,sde);
   end
   else begin
     if debug_msg then msg('slew synchronous');
     V.SlewToCoordinates(sra,sde);
   end;
   result:=true;
   except
     on E: Exception do msg('Slew error: ' + E.Message,0);
   end;
 end;
 {$endif}
end;

function T_ascommount.Slew(sra,sde: double):boolean;
begin
 result:=false;
 {$ifdef mswindows}
 if CanSlew then begin
   if debug_msg then msg('request to slew to '+formatfloat(f5,sra)+blank+formatfloat(f5,sde));
   try
   if CanSetTracking and (not V.tracking) then begin
     try
      if debug_msg then msg('must set tracking');
      V.tracking:=true;
     except
       on E: Exception do msg('Set tracking error: ' + E.Message,0);
     end;
   end;
   FMountSlewing:=true;
   if Equinox=0 then
      msg(Format(rsSlewToEQ, ['Local', ARToStr3(sra), DEToStr(sde)]))
   else
      msg(Format(rsSlewToEQ, ['J'+inttostr(round(Equinox)) ,ARToStr3(sra), DEToStr(sde)]));
   if CanSlewAsync then begin
     if debug_msg then msg('slew async');
     V.SlewToCoordinatesAsync(sra,sde);
     WaitMountSlewing(120000);
     if debug_msg then msg('slew async complete');
   end
   else begin
     if debug_msg then msg('slew synchronous');
     V.SlewToCoordinates(sra,sde);
   end;
   wait(2);
   msg(rsSlewComplete);
   FMountSlewing:=false;
   result:=true;
   except
     on E: Exception do msg('Slew error: ' + E.Message,0);
   end;
 end;
 {$endif}
end;

function T_ascommount.GetMountSlewing:boolean;
{$ifdef mswindows}
var islewing: boolean;
{$endif}
begin
 result:=false;
 {$ifdef mswindows}
 try
 islewing:=false;
  if CanSlewAsync then
    islewing:=V.Slewing
  else
    islewing:=false;
  result:=(islewing or FMountSlewing);
  if debug_msg then msg('query slewing '+BoolToStr(result,true),9);
  except
    on E: Exception do msg('Get slewing error: ' + E.Message,0);
  end;
 {$endif}
end;

function T_ascommount.WaitMountSlewing(maxtime:integer):boolean;
{$ifdef mswindows}
var count,maxcount:integer;
{$endif}
begin
 result:=true;
 {$ifdef mswindows}
 try
 if CanSlewAsync then begin
   maxcount:=maxtime div waitpoll;
   count:=0;
   while (V.Slewing)and(count<maxcount) do begin
      sleep(waitpoll);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
      inc(count);
   end;
   result:=(count<maxcount);
   if debug_msg then msg('finish to wait for slew '+BoolToStr(result,true),9);
 end;
 except
   result:=false;
 end;
 {$endif}
end;

function T_ascommount.WaitMountPark(maxtime:integer):boolean;
{$ifdef mswindows}
var count,maxcount:integer;
{$endif}
begin
 result:=true;
 {$ifdef mswindows}
 try
 if CanPark then begin
   maxcount:=maxtime div waitpoll;
   count:=0;
   while (not V.atPark)and(count<maxcount) do begin
      sleep(waitpoll);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
      inc(count);
   end;
   result:=(count<maxcount);
   if debug_msg then msg('finish to wait for park '+BoolToStr(result,true),9);
 end;
 except
   result:=false;
 end;
 {$endif}
end;

function T_ascommount.FlipMeridian:boolean;
{$ifdef mswindows}
var sra,sde,ra1,ra2: double;
    pierside1,pierside2:TPierSide;
{$endif}
begin
  result:=false;
  {$ifdef mswindows}
  if Connected then begin
    sra:=GetRA;
    sde:=GetDec;
    pierside1:=GetPierSide;
    if pierside1=pierEast then exit; // already right side
    if (sra=NullCoord)or(sde=NullCoord) then exit;
    msg(rsMeridianFlip5);
    if FWantSetPierSide and CanSetPierSide and CanSlewAsync then begin
       // do the flip
       V.SideOfPier:=0; // pierEast
       WaitMountSlewing(240000);
       {// return to position
       slew(sra,sde);
       WaitMountSlewing(240000);}
       // check result
       pierside2:=GetPierSide;
       result:=(pierside2<>pierside1);
    end
    else begin
      // point one hour to the east of meridian
      ra1:=rmod(24+1+rad2deg*CurrentSidTim/15,24);
      slew(ra1,sde);
      // point one hour to the west of target to force the flip
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
  {$endif}
end;

function T_ascommount.Sync(sra,sde: double):boolean;
begin
 result:=false;
 {$ifdef mswindows}
 result:=false;
 if CanSync then begin
   try
   if debug_msg then msg('request to sync to '+formatfloat(f5,sra)+blank+formatfloat(f5,sde));
   if CanSetTracking and (not V.tracking) then begin
     msg(rsCannotSyncWh,0);
     exit;
   end;
   if Equinox=0 then
      msg(Format(rsSyncToEQ, ['Local', ARToStr3(sra), DEToStr(sde)]))
   else
      msg(Format(rsSyncToEQ, ['J'+inttostr(round(Equinox)) ,ARToStr3(sra), DEToStr(sde)]));
   V.SyncToCoordinates(sra,sde);
   result:=true;
   except
     on E: Exception do msg('Sync error: ' + E.Message,0);
   end;
 end;
 {$endif}
end;

function T_ascommount.GetTracking:Boolean;
begin
 result:=true;
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
   try
   result:=V.tracking;
   except
   end;
 end;
 {$endif}
end;


function T_ascommount.getCanSetGuideRates:Boolean;
begin
 result:=true;
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
   try
   result:=V.CanSetGuideRates;
   except
   end;
 end;
 {$endif}
end;



function T_ascommount.Track:boolean;
begin
 result:=false;
 {$ifdef mswindows}
   try
   if CanSetTracking and (not V.tracking) then begin
     try
      msg(rsStartTracking);
      V.tracking:=true;
     except
       on E: Exception do msg('Set tracking error: ' + E.Message,0);
     end;
   end;
   result:=true;
   except
     on E: Exception do msg('Track error: ' + E.Message,0);
   end;
 {$endif}
end;

procedure T_ascommount.AbortMotion;
begin
 {$ifdef mswindows}
 MountTrackingAlert:=false;
 if CanSlew then begin
   try
   msg(rsStopTelescop);
   V.AbortSlew;
   if CanSetTracking  then V.tracking:=false;
   except
     on E: Exception do msg('Abort motion error: ' + E.Message,0);
   end;
 end;
 {$endif}
end;

procedure T_ascommount.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

// Eqmod specific

procedure T_ascommount.CheckEqmod;
{$ifdef mswindows}
var buf:string;
{$endif}
begin
  FIsEqmod:=false;
  {$ifdef mswindows}
  if pos('EQMOD',uppercase(FDevice))>0 then begin
    try
    if debug_msg then msg('check if mount driver is eqmod',9);
    buf:=V.CommandString(':MOUNTVER#');
    if length(buf)=8 then FIsEqmod:=true;
    except
     FIsEqmod:=false;
    end;
  end;
  {$endif}
end;

function  T_ascommount.GetSyncMode:TEqmodAlign;
{$ifdef mswindows}
var buf:string;
{$endif}
begin
 result:=alUNSUPPORTED;
 {$ifdef mswindows}
 if IsEqmod then begin
   try
   buf:=V.CommandString(':ALIGN_MODE#');
   if buf='1#' then result:=alADDPOINT
   else if buf='0#' then result:=alSTDSYNC;
   except
    result:=alUNSUPPORTED;
   end;
 end
 else result:=alUNSUPPORTED;
 {$endif}
end;

procedure T_ascommount.SetSyncMode(value:TEqmodAlign);
begin
 {$ifdef mswindows}
 if IsEqmod and (value<>alUNSUPPORTED) then begin
   try
   if value=alSTDSYNC then begin
     msg('align mode Std Sync');
     V.CommandString(':ALIGN_MODE,0#');
   end
   else if value=alADDPOINT then begin
     msg('align mode Add Point');
     V.CommandString(':ALIGN_MODE,1#');
   end;
   except
     on E: Exception do msg('Eqmod set sync mode error: ' + E.Message,0);
   end;
 end;
 {$endif}
end;

function T_ascommount.ClearAlignment:boolean;
begin
 result:=false;
 {$ifdef mswindows}
 if IsEqmod then begin
   try
   msg('clear alignment');
   V.CommandString(':ALIGN_CLEAR_POINTS#');
   result:=true;
   except
     on E: Exception do msg('Eqmod clear alignment error: ' + E.Message,0);
   end;
 end;
 {$endif}

end;

function T_ascommount.ClearDelta:boolean;
begin
 result:=false;
 {$ifdef mswindows}
 if IsEqmod then begin
   try
   msg('clear delta sync');
   V.CommandString(':ALIGN_CLEAR_SYNC#');
   result:=true;
   except
     on E: Exception do msg('Eqmod clear delta error: ' + E.Message,0);
   end;
 end;
 {$endif}
end;

function T_ascommount.GetSite(var long,lat,elev: double): boolean;
begin
 result:=false;
 {$ifdef mswindows}
   try
   if debug_msg then msg('get site location');
   long:=V.SiteLongitude;
   lat:=V.SiteLatitude;
   elev:=V.SiteElevation;
   result:=true;
   except
     on E: Exception do msg('Cannot get site information: ' + E.Message,0);
   end;
 {$endif}
end;

function T_ascommount.SetSite(long,lat,elev: double): boolean;
begin
 result:=false;
 {$ifdef mswindows}
   try
   if debug_msg then msg('set site location');
   V.SiteLongitude := long;
   V.SiteLatitude  := lat;
   V.SiteElevation := elev;
   result:=true;
   except
     on E: Exception do msg('Cannot set site information: ' + E.Message,0);
   end;
 {$endif}
end;

function T_ascommount.GetDate(var utc,offset: double): boolean;
begin
 result:=false;
 {$ifdef mswindows}
   try
   if debug_msg then msg('get date');
   utc:=VarToDateTime(V.UTCDate);
   offset:=ObsTimeZone; // No offset in ASCOM telescope interface
   result:=true;
   except
     on E: Exception do msg('Cannot get date: ' + E.Message,0);
   end;
 {$endif}
end;

function T_ascommount.SetDate(utc,offset: double): boolean;
begin
 result:=false;
 {$ifdef mswindows}
   try
   if debug_msg then msg('set date');
   V.UTCDate:=VarFromDateTime(utc);
   result:=true;
   except
     on E: Exception do msg('Cannot set date: ' + E.Message,0);
   end;
 {$endif}
end;

function T_ascommount.GetGuideRateRa: double;
begin
 result:=0;
 {$ifdef mswindows}
   try
   result:=V.GuideRateRightAscension;
   if debug_msg then msg('GuideRateRightAscension = '+formatfloat(f6,Result));
   except
     on E: Exception do msg('Cannot get guide rate: ' + E.Message,0);
   end;
 {$endif}
end;

function T_ascommount.GetGuideRateDe: double;
begin
 result:=0;
 {$ifdef mswindows}
   try
   result:=V.GuideRateDeclination;
   if debug_msg then msg('GuideRateDeclination = '+formatfloat(f6,Result));
   except
     on E: Exception do msg('Cannot get guide rate: ' + E.Message,0);
   end;
 {$endif}
end;

procedure T_ascommount.SetGuideRateRa(value:double);
begin
 {$ifdef mswindows}
   try
   if debug_msg then msg('Set GuideRateRightAscension = '+formatfloat(f6,value));
   V.GuideRateRightAscension:=value;
   except
   end;
 {$endif}
end;

procedure T_ascommount.SetGuideRateDe(value:double);
begin
 {$ifdef mswindows}
   try
   if debug_msg then msg('Set GuideRateDeclination = '+formatfloat(f6,value));
   V.GuideRateDeclination:=value;
   except
   end;
 {$endif}
end;

function T_ascommount.PulseGuide(direction,duration:integer): boolean;
begin
 result:=false;
 {$ifdef mswindows}
   try
    if debug_msg then msg('PulseGuide, Direction='+inttostr(direction)+', Duration='+inttostr(duration));
    V.PulseGuide(direction,duration);
    result:=true;
   except
     on E: Exception do msg('Pulse guide error: ' + E.Message,0);
   end;
 {$endif}
end;

function T_ascommount.GetPulseGuiding: boolean;
begin
 result:=false;
 {$ifdef mswindows}
   try
   result:=V.IsPulseGuiding;
   if debug_msg then msg('IsPulseGuiding = '+BoolToStr(result, rsTrue, rsFalse));
   except
     on E: Exception do msg('Cannot get pulse guide state: ' + E.Message,0);
   end;
 {$endif}
end;

initialization
{$ifdef mswindows}
{$if defined(cpui386) or defined(cpux86_64)}
// some Ascom driver raise this exceptions
SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
{$endif}
{$endif}

end.

