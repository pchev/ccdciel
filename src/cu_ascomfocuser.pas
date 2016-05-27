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

uses cu_focuser, u_global, indiapi,
    {$ifdef mswindows}
    Variants, comobj,
    {$endif}
    ExtCtrls,Classes, SysUtils;

type
T_ascomfocuser = class(T_focuser)
 private
   {$ifdef mswindows}
   V: variant;
   {$endif}
   FSpeed,FFocusdirection,FRelIncr: integer;
   stPosition,stRelpos: integer;
   Fdevice: string;
   StatusTimer: TTimer;
   procedure StatusTimerTimer(sender: TObject);
   procedure msg(txt: string);
   function  Connected: boolean;
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
public
   constructor Create;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');  override;
   procedure Disconnect; override;
   procedure FocusIn; override;
   procedure FocusOut; override;
end;


implementation

constructor T_ascomfocuser.Create;
begin
 inherited Create;
 FFocuserInterface:=ASCOM;
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

procedure T_ascomfocuser.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');
begin
 {$ifdef mswindows}
  try
  Fdevice:=cp1;
  V:=Unassigned;
  V:=CreateOleObject(WideString(Fdevice));
  V.connected:=true;
  if V.connected then begin
     FStatus := devConnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     StatusTimer.Enabled:=true;
  end
  else
     Disconnect;
  except
    on E: EOleException do msg('Error: ' + E.Message);
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
     V.connected:=false;
     V:=Unassigned;
   end;
   except
     on E: EOleException do msg('Error: ' + E.Message);
   end;
 {$endif}
end;

function T_ascomfocuser.Connected: boolean;
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
  end;
 {$endif}
end;

procedure T_ascomfocuser.FocusIn;
begin
 FFocusdirection:=-1;
end;

procedure T_ascomfocuser.FocusOut;
begin
 FFocusdirection:=1;
end;

procedure T_ascomfocuser.SetPosition(p:integer);
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   V.Move(p);
   except
    on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

function  T_ascomfocuser.GetPosition:integer;
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=V.Position;
   except
    on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

function  T_ascomfocuser.GetPositionRange: TNumRange;
begin
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
   V.Move(i);
   except
    on E: EOleException do msg('Error: ' + E.Message);
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
end;

procedure T_ascomfocuser.SetTimer(p:integer);
begin
 // not implemented in ASCOM
end;

function  T_ascomfocuser.GetTimer:integer;
begin
 // not implemented in ASCOM
end;

function  T_ascomfocuser.GethasAbsolutePosition: boolean;
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=V.Absolute;
   except
    on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

function  T_ascomfocuser.GethasRelativePosition: boolean;
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=not V.Absolute;
   except
    on E: EOleException do msg('Error: ' + E.Message);
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

end.

