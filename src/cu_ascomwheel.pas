unit cu_ascomwheel;

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

uses  cu_wheel, u_global,
  {$ifdef mswindows}
    Variants, comobj,
  {$endif}
   ExtCtrls, Classes, SysUtils;

type
T_ascomwheel = class(T_wheel)
 private
   {$ifdef mswindows}
   V: variant;
   {$endif}
   Fdevice: string;
   FFilterNum: integer;
   stFilter: integer;
   StatusTimer: TTimer;
   function Connected: boolean;
   procedure StatusTimerTimer(sender: TObject);
   procedure msg(txt: string);
   procedure GetFilterNames(var value:TStringList; var n: integer);
 protected
   procedure SetFilter(num:integer); override;
   function  GetFilter:integer; override;
   procedure SetFilterNames(value:TStringList); override;
   procedure SetTimeout(num:integer); override;
 public
   constructor Create;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''); override;
   procedure Disconnect; override;
end;


implementation

constructor T_ascomwheel.Create;
begin
 inherited Create;
 FWheelInterface:=ASCOM;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=1000;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomwheel.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

procedure T_ascomwheel.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');
begin
 {$ifdef mswindows}
  try
  Fdevice:=cp1;
  V:=Unassigned;
  V:=CreateOleObject(WideString(Fdevice));
  V.connected:=true;
  if V.connected then begin
     GetFilterNames(FFilterNames,FFilterNum);
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

procedure T_ascomwheel.Disconnect;
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

function T_ascomwheel.Connected: boolean;
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

procedure T_ascomwheel.StatusTimerTimer(sender: TObject);
{$ifdef mswindows}
var fnum: integer;
    fnam:Tstringlist;
    i,n: integer;
    fnchanged:  boolean;
{$endif}
begin
 {$ifdef mswindows}
  if not Connected then begin
     FStatus := devDisconnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
  end
  else begin
    fnum:=GetFilter;
    if fnum<>stFilter then begin
       stFilter:=fnum;
       if Assigned(FonFilterChange) then FonFilterChange(stFilter);
    end;
    fnam:=Tstringlist.Create;
    GetFilterNames(fnam,n);
    fnchanged:=n<>FFilterNum;
    if not fnchanged then
      for i:=0 to n-1 do
        if fnam[i]<>FFilterNames[i] then fnchanged:=true;
    if fnchanged then begin
       FFilterNum:=n;
       FFilterNames.Assign(fnam);
       if Assigned(FonFilterNameChange) then FonFilterNameChange(self);
    end;
  end;
 {$endif}
end;

procedure T_ascomwheel.SetFilter(num:integer);
begin
 {$ifdef mswindows}
 if Connected and (num>0) then begin
   try
   V.Position:=num-1;
   except
    on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

function  T_ascomwheel.GetFilter:integer;
begin
 {$ifdef mswindows}
 result:=0;
 if Connected then begin
   try
   result:=V.Position+1;
   except
    on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

procedure T_ascomwheel.SetFilterNames(value:TStringList);
{$ifdef mswindows}
var i:integer;
{$endif}
begin
 {$ifdef mswindows}
  if (value.Count=FFilterNum) then begin
    for i:=0 to value.Count-1 do begin
       FFilterNames[i]:=value[i];
    end;
  end;
 {$endif}
end;

procedure T_ascomwheel.GetFilterNames(var value:TStringList; var n: integer);
{$ifdef mswindows}
var fnames: array of WideString;
    i: integer;
{$endif}
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   fnames:=V.Names;
   n:=Length(fnames);
   value.Clear;
   value.Add(Filter0);
   for i:=0 to n-1 do begin
     value.Add(fnames[i]);
   end;
   except
    on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

procedure T_ascomwheel.msg(txt: string);
begin
 if Assigned(FonMsg) then FonMsg(txt);
end;

procedure T_ascomwheel.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

end.

