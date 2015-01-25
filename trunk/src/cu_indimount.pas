unit cu_indimount;

{$mode objfpc}{$H+}

{
Copyright (C) 2015 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

interface

uses indibaseclient, indibasedevice, indiapi, indicom,
     u_global, ExtCtrls, Forms, Classes, SysUtils;

type

T_indimount = class(TIndiBaseClient)
 private
   InitTimer: TTimer;
   MountDevice: Basedevice;
   Mountport: ITextVectorProperty;
   coord_prop: INumberVectorProperty;
   coord_ra:   INumber;
   coord_dec:  INumber;
   TelescopeInfo: INumberVectorProperty;
   TelescopeAperture, TelescopeFocale: INumber;
   eod_coord:  boolean;
   Fready,Fconnected: boolean;
   Findiserver, Findiserverport, Findidevice, Findideviceport: string;
   FStatus: TDeviceStatus;
   FonMsg: TNotifyMsg;
   FonStatusChange: TNotifyEvent;
   FonCoordChange: TNotifyEvent;
   FonDestroy: TNotifyEvent;
   procedure InitTimerTimer(Sender: TObject);
   procedure ClearStatus;
   procedure CheckStatus;
   procedure NewDevice(dp: Basedevice);
   procedure NewMessage(txt: string);
   procedure NewProperty(indiProp: IndiProperty);
   procedure NewNumber(nvp: INumberVectorProperty);
   procedure NewText(tvp: ITextVectorProperty);
   procedure NewSwitch(svp: ISwitchVectorProperty);
   procedure NewLight(lvp: ILightVectorProperty);
   procedure ServerConnected(Sender: TObject);
   procedure ServerDisconnected(Sender: TObject);
   function  GetRA:double;
   function  GetDec:double;
   function  GetEquinox: double;
   function  GetAperture:double;
   function  GetFocaleLength:double;
   procedure msg(txt: string);
 public
   constructor Create;
   destructor  Destroy; override;
   Procedure Connect;
   Procedure Disconnect;
   property indiserver: string read Findiserver write Findiserver;
   property indiserverport: string read Findiserverport write Findiserverport;
   property indidevice: string read Findidevice write Findidevice;
   property indideviceport: string read Findideviceport write Findideviceport;
   property RA: double read GetRA;
   property Dec: double read GetDec;
   property Equinox: double read GetEquinox;
   property Aperture: double read GetAperture;
   property FocaleLength: double read GetFocaleLength;
   property Status: TDeviceStatus read FStatus;
   property onDestroy: TNotifyEvent read FonDestroy write FonDestroy;
   property onMsg: TNotifyMsg read FonMsg write FonMsg;
   property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
   property onCoordChange: TNotifyEvent read FonCoordChange write FonCoordChange;
end;

implementation

constructor T_indimount.Create;
begin
 inherited Create;
 ClearStatus;
 Findiserver:='localhost';
 Findiserverport:='7624';
 Findidevice:='';
 Findideviceport:='';
 InitTimer:=TTimer.Create(nil);
 InitTimer.Enabled:=false;
 InitTimer.Interval:=10000;
 InitTimer.OnTimer:=@InitTimerTimer;
 onNewDevice:=@NewDevice;
 onNewMessage:=@NewMessage;
 onNewProperty:=@NewProperty;
 onNewNumber:=@NewNumber;
 onNewText:=@NewText;
 onNewSwitch:=@NewSwitch;
 onNewLight:=@NewLight;
 onServerConnected:=@ServerConnected;
 onServerDisconnected:=@ServerDisconnected;
end;

destructor  T_indimount.Destroy;
begin
 if assigned(FonDestroy) then FonDestroy(self);
 onNewDevice:=nil;
 onNewMessage:=nil;
 onNewProperty:=nil;
 onNewNumber:=nil;
 onNewText:=nil;
 onNewSwitch:=nil;
 onNewLight:=nil;
 onNewBlob:=nil;
 onServerConnected:=nil;
 onServerDisconnected:=nil;
 if InitTimer<>nil then FreeAndNil(InitTimer);
 inherited Destroy;
end;

procedure T_indimount.ClearStatus;
begin
    MountDevice:=nil;
    Mountport:=nil;
    TelescopeInfo:=nil;
    coord_prop:=nil;
    Fready:=false;
    Fconnected := false;
    FStatus := devDisconnected;
    if Assigned(FonStatusChange) then FonStatusChange(self);
end;

procedure T_indimount.CheckStatus;
begin
    if Fconnected and
       (coord_prop<>nil)
    then begin
       FStatus := devConnected;
      if (not Fready) and Assigned(FonStatusChange) then FonStatusChange(self);
      Fready:=true;
    end;
end;

procedure T_indimount.msg(txt: string);
begin
  if Assigned(FonMsg) then FonMsg(txt);
end;

Procedure T_indimount.Connect;
begin
if not Connected then begin
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  SetServer(indiserver,indiserverport);
  watchDevice(indidevice);
  ConnectServer;
  FStatus := devConnecting;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  InitTimer.Enabled:=true;
end
else msg('Already connected');
end;

procedure T_indimount.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Enabled:=false;
  if (MountDevice=nil)or(not Fready) then begin
     msg('No response from server');
     msg('Is "'+indidevice+'" a running telescope mount driver?');
     Disconnect;
  end;
end;

Procedure T_indimount.Disconnect;
begin
Terminate;
ClearStatus;
end;

procedure T_indimount.ServerConnected(Sender: TObject);
begin
   if (Mountport<>nil)and(Findideviceport<>'') then begin
      Mountport.tp[0].text:=Findideviceport;
      sendNewText(Mountport);
   end;
   connectDevice(Findidevice);
end;

procedure T_indimount.ServerDisconnected(Sender: TObject);
begin
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  msg('Mount server disconnected');
end;

procedure T_indimount.NewDevice(dp: Basedevice);
begin
  //writeln('Newdev: '+dp.getDeviceName);
  if dp.getDeviceName=Findidevice then begin
     Fconnected:=true;
     MountDevice:=dp;
  end;
end;

procedure T_indimount.NewMessage(txt: string);
begin
  msg(txt);
end;

procedure T_indimount.NewProperty(indiProp: IndiProperty);
var propname: string;
    proptype: INDI_TYPE;
begin
  propname:=indiProp.getName;
  proptype:=indiProp.getType;

  if (proptype=INDI_TEXT)and(propname='DEVICE_PORT') then begin
     Mountport:=indiProp.getText;
  end
  else if (proptype=INDI_NUMBER)and(propname='EQUATORIAL_EOD_COORD') then begin
      coord_prop:=indiProp.getNumber;
      coord_ra:=IUFindNumber(coord_prop,'RA');
      coord_dec:=IUFindNumber(coord_prop,'DEC');
      eod_coord:=true;
      if (coord_ra=nil)or(coord_dec=nil) then coord_prop:=nil;
   end
   else if (proptype=INDI_NUMBER)and(coord_prop=nil)and(propname='EQUATORIAL_COORD') then begin
      coord_prop:=indiProp.getNumber;
      coord_ra:=IUFindNumber(coord_prop,'RA');
      coord_dec:=IUFindNumber(coord_prop,'DEC');
      eod_coord:=false;
      if (coord_ra=nil)or(coord_dec=nil) then coord_prop:=nil;
   end
  else if (proptype=INDI_NUMBER)and(propname='TELESCOPE_INFO') then begin
     TelescopeInfo:=indiProp.getNumber;
     TelescopeAperture:=IUFindNumber(TelescopeInfo,'TELESCOPE_APERTURE');
     TelescopeFocale:=IUFindNumber(TelescopeInfo,'TELESCOPE_FOCAL_LENGTH');
     if (TelescopeAperture=nil)or(TelescopeFocale=nil) then TelescopeInfo:=nil;
  end;
  CheckStatus;
end;

procedure T_indimount.NewNumber(nvp: INumberVectorProperty);
begin
  if nvp=coord_prop then begin
     if Assigned(FonCoordChange) then FonCoordChange(self);
  end;
end;

procedure T_indimount.NewText(tvp: ITextVectorProperty);
begin
//  writeln('NewText: '+tvp.name+' '+tvp.tp[0].text);
end;

procedure T_indimount.NewSwitch(svp: ISwitchVectorProperty);
begin
//  writeln('NewSwitch: '+svp.name);
end;

procedure T_indimount.NewLight(lvp: ILightVectorProperty);
begin
//  writeln('NewLight: '+lvp.name);
end;

function  T_indimount.GetRA:double;
begin
if coord_prop<>nil then begin;
  result:=coord_ra.value;
end
else result:=NullCoord;
end;

function  T_indimount.GetDec:double;
begin
if coord_prop<>nil then begin;
  result:=coord_dec.value;
end
else result:=NullCoord;
end;

function  T_indimount.GetEquinox: double;
begin
 if eod_coord then result:=0
              else result:=2000;
end;

function  T_indimount.GetAperture:double;
begin
if TelescopeInfo<>nil then begin;
  result:=TelescopeAperture.value;
end
else result:=-1;
end;

function  T_indimount.GetFocaleLength:double;
begin
if TelescopeInfo<>nil then begin;
  result:=TelescopeFocale.value;
end
else result:=-1;
end;

end.

