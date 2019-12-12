unit fu_devicesconnection;

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

uses UScaleDPI, u_translation, u_hints, u_global,
  Classes, SysUtils, FileUtil, Forms, Graphics, Controls, StdCtrls, ExtCtrls, Menus;

type

  { Tf_devicesconnection }

  Tf_devicesconnection = class(TFrame)
    BtnConnect: TButton;
    BtnProfile: TButton;
    LabelDome: TLabel;
    LabelSafety: TLabel;
    LabelWeather: TLabel;
    LabelWatchdog: TLabel;
    LabelRotator: TLabel;
    MenuConnectDevice: TMenuItem;
    MenuDisconnectDevice: TMenuItem;
    Panel2: TPanel;
    Panel3: TPanel;
    PopupMenu1: TPopupMenu;
    ProfileLabel: TLabel;
    LabelCamera: TLabel;
    LabelWheel: TLabel;
    LabelFocuser: TLabel;
    LabelMount: TLabel;
    Panel1: TPanel;
    led: TShape;
    PanelDev: TPanel;
    Title: TLabel;
    procedure BtnConnectClick(Sender: TObject);
    procedure BtnProfileClick(Sender: TObject);
    procedure DeviceMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MenuConnectDeviceClick(Sender: TObject);
    procedure MenuDisconnectDeviceClick(Sender: TObject);
  private
    { private declarations }
    FonSelectProfile,FonConnect,FonDisconnect: TNotifyEvent;
    FonConnectDevice,FonDisconnectDevice: TNotifyNum;
    SelectedDevice: integer;
    procedure SetLang;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect;
    procedure Disconnect(confirm:boolean);
    property onSelectProfile: TNotifyEvent read FonSelectProfile write FonSelectProfile;
    property onConnect: TNotifyEvent read FonConnect write FonConnect;
    property onDisconnect: TNotifyEvent read FonDisconnect write FonDisconnect;
    property onConnectDevice: TNotifyNum read FonConnectDevice write FonConnectDevice;
    property onDisconnectDevice: TNotifyNum read FonDisconnectDevice write FonDisconnectDevice;
  end;

implementation

{$R *.lfm}

{ Tf_devicesconnection }

constructor Tf_devicesconnection.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 Panel1.ChildSizing.LeftRightSpacing:=8;
 Panel1.ChildSizing.VerticalSpacing:=4;
 {$endif}
 ScaleDPI(Self);
 SetLang;
 SelectedDevice:=0;
end;

destructor  Tf_devicesconnection.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_devicesconnection.SetLang;
begin
  Title.Caption:=rsDevicesConne;
  BtnProfile.Caption:=trim(format(rsProfile,['']));
  BtnConnect.Caption:=rsConnect;
  BtnConnect.Hint:=rsConnectAllDe;
  LabelCamera.Caption:=rsCam;
  LabelWheel.Caption:=rsFil;
  LabelFocuser.Caption:=rsFoc;
  LabelRotator.Caption:=rsRot;
  LabelMount.Caption:=rsMnt;
  LabelWatchdog.Caption:=rsWch;
end;

procedure Tf_devicesconnection.BtnProfileClick(Sender: TObject);
begin
  if Assigned(FonSelectProfile) then FonSelectProfile(Self);
end;

procedure Tf_devicesconnection.BtnConnectClick(Sender: TObject);
begin
  if BtnConnect.Caption=rsDisconnect then begin
    Disconnect(true);
  end else begin
    BtnConnect.Caption:=rsDisconnect;
    Connect;
  end;
end;

procedure Tf_devicesconnection.DeviceMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var txt:string;
begin
 if Sender is TLabel then begin
   SelectedDevice:=TLabel(Sender).tag;
   case SelectedDevice of
      1:  txt:=rsCamera;
      2:  txt:=rsFilterWheel;
      3:  txt:=rsFocuser;
      4:  txt:=rsRotator;
      5:  txt:=rsMount;
      6:  txt:=rsDome;
      7:  txt:=rsWatchdog;
      8:  txt:=rsWeatherStati;
      9:  txt:=rsSafetyMonito;
   end;
   MenuConnectDevice.Caption:=rsConnect+': '+txt;
   MenuDisconnectDevice.Caption:=rsDisconnect+': '+txt;
   PopupMenu1.PopUp;
 end;
end;

procedure Tf_devicesconnection.MenuConnectDeviceClick(Sender: TObject);
begin
 if Assigned(FonConnectDevice) then FonConnectDevice(SelectedDevice);
end;

procedure Tf_devicesconnection.MenuDisconnectDeviceClick(Sender: TObject);
begin
 if Assigned(FonDisconnectDevice) then FonDisconnectDevice(SelectedDevice);
end;

procedure Tf_devicesconnection.Connect;
begin
  if Assigned(FonConnect) then FonConnect(self);
end;

procedure Tf_devicesconnection.Disconnect(confirm:boolean);
begin
  if Assigned(FonDisconnect) then begin
    if confirm then
      FonDisconnect(self)
    else
      FonDisconnect(nil);
  end;
end;

end.

