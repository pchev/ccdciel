unit cu_ascomfocuser;

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

uses cu_focuser, u_global,
    {$ifdef mswindows}
    indiapi, Variants, comobj,
    {$endif}
    Forms, ExtCtrls,Classes, SysUtils;

type
T_ascomfocuser = class(T_focuser)
 private
   {$ifdef mswindows}
   V: variant;
   FFocusdirection: integer;
   stPosition: integer;
   Fdevice: string;
   {$endif}
   FInterfaceVersion: integer;
   FRelIncr: integer;
   StatusTimer: TTimer;
   procedure StatusTimerTimer(sender: TObject);
   procedure msg(txt: string);
   function  Connected: boolean;
   function  InterfaceVersion: integer;
 protected
   procedure SetPosition(p:integer); override;
   function  GetPosition:integer; override;
   procedure SetRelPosition(p:integer); override;
   function  GetRelPosition:integer; override;
   procedure SetSpeed(p:integer); override;
   function  GetSpeed:integer; override;
   procedure SetTimer(p:integer); override;
   function  GetTimer:integer; override;
   function  GethasAbsolutePosition: boolean; override;
   function  GethasRelativePosition: boolean; override;
   function  GethasTimerSpeed: boolean; override;
   function  GetPositionRange: TNumRange; override;
   function  GetRelPositionRange: TNumRange; override;
   procedure SetTimeout(num:integer); override;
   function  WaitFocuserMoving(maxtime:integer):boolean;
   function  GetTemperature:double; override;
public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');  override;
   procedure Disconnect; override;
   procedure FocusIn; override;
   procedure FocusOut; override;
end;


implementation

constructor T_ascomfocuser.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FFocuserInterface:=ASCOM;
 FInterfaceVersion:=1;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=1000;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomfocuser.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

function  T_ascomfocuser.InterfaceVersion: integer;
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

procedure T_ascomfocuser.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');
begin
 {$ifdef mswindows}
  try
  FStatus := devConnecting;
  Fdevice:=cp1;
  V:=Unassigned;
  V:=CreateOleObject(WideString(Fdevice));
  FInterfaceVersion:=InterfaceVersion;
  if FInterfaceVersion=1 then
    V.Link:=true
  else
    V.Connected:=true;
  if Connected then begin
     GetTemperature;
     msg('Focuser '+Fdevice+' connected.');
     FStatus := devConnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     StatusTimer.Enabled:=true;
  end
  else
     Disconnect;
  except
    on E: Exception do msg('Focuser '+Fdevice+' Connection error: ' + E.Message);
  end;
 {$endif}
end;

procedure T_ascomfocuser.Disconnect;
begin
 {$ifdef mswindows}
   StatusTimer.Enabled:=false;
   FStatus := devDisconnected;
   if Assigned(FonStatusChange) then FonStatusChange(self);
   try
   if not VarIsEmpty(V) then begin
     msg('Focuser '+Fdevice+' disconnected.');
     if FInterfaceVersion=1 then
       V.Link:=false
     else
       V.Connected:=false;
     V:=Unassigned;
   end;
   except
     on E: Exception do msg('Focuser '+Fdevice+' Disconnection error: ' + E.Message);
   end;
 {$endif}
end;

function T_ascomfocuser.Connected: boolean;
begin
result:=false;
{$ifdef mswindows}
if not VarIsEmpty(V) then begin
  try
  if FInterfaceVersion=1 then
     result:=V.Link
  else
     result:=V.connected;
  except
   result:=false;
  end;
end;
{$endif}
end;

procedure T_ascomfocuser.StatusTimerTimer(sender: TObject);
{$ifdef mswindows}
var x,y: double;
    p: integer;
{$endif}
begin
 {$ifdef mswindows}
  if not Connected then begin
     FStatus := devDisconnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
  end
  else begin
    try
    if hasAbsolutePosition then begin
      p:=GetPosition;
      if p<>stPosition then begin
        stPosition:=p;
        if Assigned(FonPositionChange) then FonPositionChange(p);
      end;
    end else begin
      p:=FRelIncr;
      if p<>stPosition then begin
        stPosition:=p;
        if Assigned(FonPositionChange) then FonPositionChange(p);
      end;
    end;
    except
     on E: Exception do msg('Focuser '+Fdevice+' Status error: ' + E.Message);
    end;
  end;
 {$endif}
end;

function T_ascomfocuser.WaitFocuserMoving(maxtime:integer):boolean;
{$ifdef mswindows}
var count,maxcount:integer;
{$endif}
begin
 result:=true;
 {$ifdef mswindows}
 try
 if Connected then begin
   maxcount:=maxtime div 100;
   count:=0;
   while (V.IsMoving)and(count<maxcount) do begin
      sleep(100);
      Application.ProcessMessages;
      inc(count);
   end;
   result:=(count<maxcount);
 end;
 except
   result:=false;
 end;
 {$endif}
end;

procedure T_ascomfocuser.FocusIn;
begin
 {$ifdef mswindows}
 FFocusdirection:=-1;
 FLastDirection:=FocusDirIn;
 msg('Focuser '+Fdevice+' set direction in.');
 {$endif}
end;

procedure T_ascomfocuser.FocusOut;
begin
 {$ifdef mswindows}
 FFocusdirection:=1;
 FLastDirection:=FocusDirOut;
 msg('Focuser '+Fdevice+' set direction out.');
 {$endif}
end;

procedure T_ascomfocuser.SetPosition(p:integer);
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   msg('Focuser '+Fdevice+' move to '+inttostr(p));
   V.Move(p);
   WaitFocuserMoving(30000);

   except
    on E: Exception do msg('Focuser '+Fdevice+' Error, can''t move to. ' + E.Message);
   end;
 end;
 {$endif}
end;

function  T_ascomfocuser.GetPosition:integer;
begin
 result:=0;
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=V.Position;
   except
    on E: Exception do msg('Focuser '+Fdevice+' Get position error: ' + E.Message);
   end;
 end;
 {$endif}
end;

function  T_ascomfocuser.GetPositionRange: TNumRange;
begin
 result:=NullRange;
 {$ifdef mswindows}
 if Connected then begin
   try
   result.min:=0;
   result.max:=V.MaxStep;
   result.step:=1;
   except
    result:=NullRange;
   end;
 end
 else result:=NullRange;
 {$endif}
end;

function  T_ascomfocuser.GetRelPositionRange: TNumRange;
begin
 result:=NullRange;
 {$ifdef mswindows}
 if Connected then begin
   try
   result.min:=0;
   result.max:=V.MaxStep;
   result.step:=1;
   except
    result:=NullRange;
   end;
 end
 else result:=NullRange;
 {$endif}
end;

procedure T_ascomfocuser.SetRelPosition(p:integer);
{$ifdef mswindows}
var i: integer;
{$endif}
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   FRelIncr:=p;
   i:=FFocusdirection*FRelIncr;
   msg('Focuser '+Fdevice+' move by '+inttostr(i));
   V.Move(i);
   WaitFocuserMoving(30000);

   except
    on E: Exception do msg('Focuser '+Fdevice+' Set relative position error: ' + E.Message);
   end;
 end;
 {$endif}
end;

function  T_ascomfocuser.GetRelPosition:integer;
begin
 result:=FRelIncr;
end;

procedure T_ascomfocuser.SetSpeed(p:integer);
begin
  // not implemented in ASCOM
end;

function  T_ascomfocuser.GetSpeed:integer;
begin
 // not implemented in ASCOM
 result:=0;
end;

procedure T_ascomfocuser.SetTimer(p:integer);
begin
 // not implemented in ASCOM
end;

function  T_ascomfocuser.GetTimer:integer;
begin
 // not implemented in ASCOM
 result:=0;
end;

function  T_ascomfocuser.GethasAbsolutePosition: boolean;
begin
 result:=False;
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=V.Absolute;
   except
    on E: Exception do msg('Focuser '+Fdevice+' GethasAbsolutePosition error: ' + E.Message);
   end;
 end;
 {$endif}
end;

function  T_ascomfocuser.GethasRelativePosition: boolean;
begin
 result:=False;
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=not V.Absolute;
   except
    on E: Exception do msg('Focuser '+Fdevice+' GethasRelativePosition error: ' + E.Message);
   end;
 end;
 {$endif}
end;

function  T_ascomfocuser.GethasTimerSpeed: boolean;
begin
 result:=false;
end;

procedure T_ascomfocuser.msg(txt: string);
begin
  if Assigned(FonMsg) then FonMsg(txt);
end;

procedure T_ascomfocuser.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

function  T_ascomfocuser.GetTemperature:double;
begin
 result:=0;
 {$ifdef mswindows}
 if Connected then begin
   try
   result:= V.Temperature;
   FhasTemperature:=true;
   except
    result:=0;
    FhasTemperature:=false;
   end;
 end;
 {$endif}
end;

end.

