unit cu_manualwheel;

{$mode objfpc}{$H+}

{
Copyright (C) 2020 Patrick Chevalley

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

uses cu_wheel, pu_pause, indiapi, u_translation,
     u_global, ExtCtrls, Forms, Classes, SysUtils;

type

T_manualwheel = class(T_wheel)
 private
   pause: Tf_pause;
   FCurrentFilter: integer;
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
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string=''); override;
   Procedure Disconnect; override;
end;

implementation

constructor T_manualwheel.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FWheelInterface:=MANUAL;
 pause:=Tf_pause.Create(self);
 FCurrentFilter:=0;
end;

destructor  T_manualwheel.Destroy;
begin
 pause.Free;
 inherited Destroy;
end;

Procedure T_manualwheel.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
begin
 FStatus := devConnecting;
 if Assigned(FonStatusChange) then FonStatusChange(self);
 FStatus := devConnected;
 if Assigned(FonStatusChange) then FonStatusChange(self);
end;

Procedure T_manualwheel.Disconnect;
begin
 FStatus := devDisconnected;
 if Assigned(FonStatusChange) then FonStatusChange(self);
end;

procedure T_manualwheel.SetFilter(num:integer);
begin
if num=0 then exit;
if FCurrentFilter<>num then begin
  if (num>0)and(num<=FFilterNames.Count) then begin
    pause.Caption:=rsPause;
    pause.Text:=Format(rsSetFilterPos,[IntToStr(num)+' : '+FFilterNames[num]]);
    if not pause.Wait then begin
      msg(rsManualFilter);
      if Assigned(FonFilterChange) then FonFilterChange(FCurrentFilter);
      exit;
    end
    else begin
      FCurrentFilter:=num;
    end;
  end
  else begin
    msg('Filter '+IntToStr(num)+' out of range');
  end;
end;
end;

function  T_manualwheel.GetFilter:integer;
begin
  result:=FCurrentFilter;
end;

procedure T_manualwheel.SetFilterNames(value:TStringList);
begin
  FFilterNames.Assign(value);
end;

function  T_manualwheel.GetFilterNames:TStringList;
begin
  result:=FFilterNames;
end;

procedure T_manualwheel.SetTimeout(num:integer);
begin
  FTimeOut:=num;
end;

function  T_manualwheel.GetStatus: TDeviceStatus;
begin
  result:=FStatus;
end;

procedure T_manualwheel.SetCamera(value: TObject);
begin
 Fcameraobj:=value;
end;

end.

