unit cu_incamerawheel;

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

uses cu_wheel, cu_camera, indiapi,
     u_global, ExtCtrls, Forms, Classes, SysUtils;

type

T_incamerawheel = class(T_wheel)
 private
   Fcamera: T_camera;
   procedure msg(txt: string);
 protected
   procedure SetFilter(num:integer); override;
   function  GetFilter:integer; override;
   procedure SetFilterNames(value:TStringList); override;
   procedure SetTimeout(num:integer); override;
   procedure SetCamera(value: TObject); override;
   function  GetFilterNames:TStringList; override;
   function  GetStatus: TDeviceStatus; override;
public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''); override;
   Procedure Disconnect; override;
end;

implementation

constructor T_incamerawheel.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 Fcamera:=nil;
 FWheelInterface:=INCAMERA;
end;

procedure T_incamerawheel.SetCamera(value: TObject);
begin
 Fcameraobj:=value;
 Fcamera:=T_camera(value);
 if Fcamera<>nil then begin;
   Fcamera.onWheelStatusChange:=FonStatusChange;
   Fcamera.onFilterChange:=FonFilterChange;
   Fcamera.onFilterNameChange:=FonFilterNameChange;
 end;
end;

destructor  T_incamerawheel.Destroy;
begin
 inherited Destroy;
end;

procedure T_incamerawheel.msg(txt: string);
begin
  if Assigned(FonMsg) then FonMsg(txt);
end;

Procedure T_incamerawheel.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');
begin
end;

Procedure T_incamerawheel.Disconnect;
begin
end;

procedure T_incamerawheel.SetFilter(num:integer);
begin
if (Fcamera<>nil) then begin
  Fcamera.Filter:=num;
end;
end;

function  T_incamerawheel.GetFilter:integer;
begin
if (Fcamera<>nil) then begin
  result:=Fcamera.Filter;
end else
  result:=0;
end;

procedure T_incamerawheel.SetFilterNames(value:TStringList);
begin
if (Fcamera<>nil) then begin
  Fcamera.FilterNames:=value;
end;
end;

function  T_incamerawheel.GetFilterNames:TStringList;
begin
if (Fcamera<>nil) then begin
  result:=Fcamera.FilterNames;
end;
end;

procedure T_incamerawheel.SetTimeout(num:integer);
begin
if (Fcamera<>nil)and(num>Fcamera.Timeout) then begin
  Fcamera.Timeout:=num;
end;
end;

function  T_incamerawheel.GetStatus: TDeviceStatus;
begin
if (Fcamera<>nil) then begin
  result:=Fcamera.WheelStatus;
end else
  result:=FStatus;
end;

end.

