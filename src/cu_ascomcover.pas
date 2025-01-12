unit cu_ascomcover;

{$mode objfpc}{$H+}

{
Copyright (C) 2021 Patrick Chevalley

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

uses cu_cover, u_global,
    {$ifdef mswindows}
    u_translation, indiapi, Variants, comobj, math,
    {$endif}
    Forms, ExtCtrls,Classes, SysUtils;

type
T_ascomcover = class(T_cover)
 private
   {$ifdef mswindows}
   V: variant;
   {$endif}
   FInterfaceVersion: integer;
   StatusTimer: TTimer;
   procedure StatusTimerTimer(sender: TObject);
   function  Connected: boolean;
   function WaitConnecting(maxtime:integer):boolean;
 protected
   function GetCoverState: TCoverStatus; override;
   function GetCalibratorState: TCalibratorStatus; override;
   procedure SetTimeout(num:integer); override;
public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');  override;
   procedure Disconnect; override;
   function GetV: variant;
   Procedure OpenCover; override;
   Procedure CloseCover; override;
   function GetBrightness: integer; override;
   Procedure CalibratorOn(value: integer); override;
   Procedure CalibratorOff; override;
end;

const statusinterval=1000;
      waitpoll=500;

implementation

constructor T_ascomcover.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FCoverInterface:=ASCOM;
 FInterfaceVersion:=1;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=statusinterval;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomcover.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

procedure T_ascomcover.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
begin
 {$ifdef mswindows}
  try
  FStatus := devConnecting;
  Fdevice:=cp1;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  V:=Unassigned;
  V:=CreateOleObject(Fdevice);
  try
  FInterfaceVersion:=V.InterfaceVersion;
  except
    FInterfaceVersion:=1;
  end;
  msg('Interface version: '+inttostr(FInterfaceVersion),9);
  if FInterfaceVersion>=2 then begin
    V.Connect;
    WaitConnecting(30000);
  end
  else
    V.Connected:=true;
  if Connected then begin
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
       st_cov:=TCoverStatus(V.CoverState);
     except
       st_cov:=covNotPresent;
     end;
     FHasCover:=(st_cov<>covNotPresent);
     try
       st_cal:=TCalibratorStatus(V.CalibratorState);
     except
       st_cal:=calNotPresent;
     end;
     FHasCalibrator:=(st_cal<>calNotPresent);
     if FHasCalibrator then begin
       try
       FMaxBrightness:=V.MaxBrightness;
       except
         FMaxBrightness:=0;
       end;
     end;
     msg(rsConnected3);
     FStatus := devConnected;
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

procedure T_ascomcover.Disconnect;
begin
 {$ifdef mswindows}
   StatusTimer.Enabled:=false;
   try
   if not VarIsEmpty(V) then begin
     if FInterfaceVersion>=2 then begin
       V.Disconnect;
       WaitConnecting(30000);
     end
     else
       V.Connected:=false;
     V:=Unassigned;
   end;
   except
     on E: Exception do msg('Disconnection error: ' + E.Message,0);
   end;
   FStatus := devDisconnected;
   if Assigned(FonStatusChange) then FonStatusChange(self);
   msg(rsDisconnected3,1);
 {$endif}
end;

function T_ascomcover.GetV: variant;
begin
 {$ifdef mswindows}
 result:=V;
 {$endif}
end;

function T_ascomcover.Connected: boolean;
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

function T_ascomcover.WaitConnecting(maxtime:integer):boolean;
{$ifdef mswindows}
var count,maxcount:integer;
{$endif}
begin
 result:=true;
 {$ifdef mswindows}
 try
   maxcount:=maxtime div waitpoll;
   count:=0;
   while (V.Connecting)and(count<maxcount) do begin
      sleep(waitpoll);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
      inc(count);
   end;
   result:=(count<maxcount);
   if debug_msg then msg('finish to wait for connecting '+BoolToStr(result,true),9);
 except
   on E: Exception do begin
     msg(Format(rsConnectionEr, [E.Message]),0);
     result:=false;
   end;
 end;
 {$endif}
end;

procedure T_ascomcover.StatusTimerTimer(sender: TObject);
{$ifdef mswindows}
var scal:TCalibratorStatus;
    scov:TCoverStatus;
{$endif}
begin
 {$ifdef mswindows}
 try
 StatusTimer.Enabled:=false;
  if not Connected then begin
     FStatus := devDisconnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
  end
  else begin
    try
      scal:=GetCalibratorState;
      scov:=GetCoverState;
      if (scal<>st_cal)or(scov<>st_cov) then begin
        st_cov:=scov;
        st_cal:=scal;
        if Assigned(FonCoverChange) then FonCoverChange(self);
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

function T_ascomcover.GetCoverState: TCoverStatus;
begin
 result:=covUnknown;
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
 try
   result:=TCoverStatus(V.CoverState);
   except
    on E: Exception do msg('CoverState error: ' + E.Message,0);
   end;
 end;
 {$endif}
end;

function  T_ascomcover.GetCalibratorState: TCalibratorStatus;
begin
 result:=calUnknown;
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
 try
   result:=TCalibratorStatus(V.CalibratorState);
   except
    on E: Exception do msg('CalibratorState error: ' + E.Message,0);
   end;
 end;
 {$endif}
end;

procedure T_ascomcover.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

Procedure T_ascomcover.OpenCover;
begin
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
 try
   V.OpenCover;
   except
    on E: Exception do msg('OpenCover error: ' + E.Message,0);
   end;
 end;
 {$endif}
end;

Procedure T_ascomcover.CloseCover;
begin
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
 try
   V.CloseCover;
   except
    on E: Exception do msg('CloseCover error: ' + E.Message,0);
   end;
 end;
 {$endif}
end;

function T_ascomcover.GetBrightness: integer;
begin
 result:=0;
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
 try
   result:=V.Brightness;
   except
    on E: Exception do msg('Get Brightness error: ' + E.Message,0);
   end;
 end;
 {$endif}
end;

Procedure T_ascomcover.CalibratorOn(value: integer);
begin
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
   try
     V.CalibratorOn(value);
   except
     on E: Exception do msg('CalibratorOn error: ' + E.Message,0);
   end;
 end;
 {$endif}
end;

Procedure T_ascomcover.CalibratorOff;
begin
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
   try
     V.CalibratorOff;
   except
     on E: Exception do msg('CalibratorOff error: ' + E.Message,0);
   end;
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

