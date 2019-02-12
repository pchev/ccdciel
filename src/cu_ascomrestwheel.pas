unit cu_ascomrestwheel;

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

uses  cu_wheel, cu_ascomrest, u_global, indiapi,
     u_translation,
     ExtCtrls, Forms, Classes, SysUtils;

type
T_ascomrestwheel = class(T_wheel)
 private
   V: TAscomRest;
   FFilterNum: integer;
   stFilter,CheckFiltername: integer;
   StatusTimer: TTimer;
   function Connected: boolean;
   procedure StatusTimerTimer(sender: TObject);
   procedure GetAscomFilterNames(var value:TStringList; var n: integer);
   function WaitFilter(maxtime:integer):boolean;
 protected
   procedure SetFilter(num:integer); override;
   function  GetFilter:integer; override;
   procedure SetFilterNames(value:TStringList); override;
   function  GetFilterNames:TStringList; override;
   procedure SetTimeout(num:integer); override;
   procedure SetCamera(value: TObject); override;
   function  GetStatus: TDeviceStatus; override;
 public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string=''); override;
   procedure Disconnect; override;
end;

const waitpoll=1000;
      statusinterval=3000;

implementation

constructor T_ascomrestwheel.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 V:=TAscomRest.Create(self);
 V.ClientId:=3207;
 FWheelInterface:=ASCOMREST;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=statusinterval;
 StatusTimer.OnTimer:=@StatusTimerTimer;
 CheckFiltername:=0;
end;

destructor  T_ascomrestwheel.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

procedure T_ascomrestwheel.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
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
  V.Put('connected',true);
  if V.Get('connected').AsBool then begin
     V.Timeout:=120000;
     try
     msg('Driver version: '+V.Get('driverversion').AsString,9);
     except
       msg('Error: unknown driver version',9);
     end;
     msg(rsConnected3);
     GetAscomFilterNames(FFilterNames,FFilterNum);
     FStatus := devConnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     CheckFiltername:=0;
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

procedure T_ascomrestwheel.Disconnect;
begin
   StatusTimer.Enabled:=false;
   FStatus := devDisconnected;
   if Assigned(FonStatusChange) then FonStatusChange(self);
   try
     msg(rsDisconnected3,0);
     // the server is responsible for device disconnection
   except
     on E: Exception do msg(Format(rsDisconnectio, [E.Message]),0);
   end;
end;

function T_ascomrestwheel.Connected: boolean;
begin
result:=false;
  try
  result:=V.Get('connected').AsBool;
  except
   result:=false;
  end;
end;

procedure T_ascomrestwheel.StatusTimerTimer(sender: TObject);
var fnum: integer;
    fnam:Tstringlist;
    i,n: integer;
    fnchanged:  boolean;
begin
 StatusTimer.Enabled:=false;
 try
  if not Connected then begin
     FStatus := devDisconnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     msg(rsDisconnected3,0);
  end
  else begin
    try
    fnum:=GetFilter;
    if fnum<>stFilter then begin
       stFilter:=fnum;
       if Assigned(FonFilterChange) then FonFilterChange(stFilter);
    end;
    if (CheckFiltername mod 10)=0 then begin
      CheckFiltername:=0;
      fnam:=Tstringlist.Create;
      n:=0;
      GetAscomFilterNames(fnam,n);
      fnchanged:=n<>FFilterNum;
      if not fnchanged then
        for i:=0 to n-1 do
          if fnam[i]<>FFilterNames[i] then fnchanged:=true;
      if fnchanged then begin
         FFilterNum:=n;
         FFilterNames.Assign(fnam);
         if Assigned(FonFilterNameChange) then FonFilterNameChange(self);
      end;
      fnam.Free;
    end;
    inc(CheckFiltername);
    except
     on E: Exception do msg('Error: ' + E.Message,0);
    end;
  end;
  finally
   if FStatus=devConnected then StatusTimer.Enabled:=true;
  end;
end;

function T_ascomrestwheel.WaitFilter(maxtime:integer):boolean;
var count,maxcount:integer;
begin
 result:=true;
 try
   maxcount:=maxtime div waitpoll;
   count:=0;
   while (V.Get('position').AsInt<0)and(count<maxcount) do begin
      sleep(waitpoll);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
      inc(count);
   end;
   result:=(count<maxcount);
 except
   result:=false;
 end;
end;


procedure T_ascomrestwheel.SetFilter(num:integer);
begin
 if (num>0) then begin
   try
   msg(Format(rsSetFilterPos, [inttostr(num)]));
   V.Put('position',num-1);
   WaitFilter(60000);
   except
    on E: Exception do msg('Set filter error: ' + E.Message,0);
   end;
 end;
end;

function  T_ascomrestwheel.GetFilter:integer;
begin
 result:=0;
   try
   result:=V.Get('position').AsInt+1;
   except
    on E: Exception do msg('Get filter error: ' + E.Message,0);
   end;
end;

procedure T_ascomrestwheel.SetFilterNames(value:TStringList);
var i:integer;
begin
  if (value.Count=FFilterNum) then begin
    for i:=0 to value.Count-1 do begin
       FFilterNames[i]:=value[i];
    end;
  end;
end;

function  T_ascomrestwheel.GetFilterNames:TStringList;
begin
  result:=FFilterNames;
end;

procedure T_ascomrestwheel.GetAscomFilterNames(var value:TStringList; var n: integer);
var fnames: array of string;
    i: integer;
begin
 value.Clear;
 n:=0;
   try
   fnames:=V.Get('names').AsStringArray;
   n:=Length(fnames);
   value.Add(Filter0);
   for i:=0 to n-1 do begin
     value.Add(string(fnames[i]));
   end;
   except
    on E: Exception do msg('List filter names error: ' + E.Message,0);
   end;
end;

function  T_ascomrestwheel.GetStatus: TDeviceStatus;
begin
  result:=FStatus;
end;

procedure T_ascomrestwheel.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

procedure T_ascomrestwheel.SetCamera(value: TObject);
begin
 Fcameraobj:=value;
end;

end.

