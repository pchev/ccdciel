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

uses UScaleDPI, u_translation,
  Classes, SysUtils, FileUtil, Forms, Graphics, Controls, StdCtrls, ExtCtrls;

type

  { Tf_devicesconnection }

  Tf_devicesconnection = class(TFrame)
    BtnConnect: TButton;
    LabelDome: TLabel;
    LabelSafety: TLabel;
    LabelWeather: TLabel;
    LabelWatchdog: TLabel;
    LabelRotator: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
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
  private
    { private declarations }
    FonConnect,FonDisconnect: TNotifyEvent;
    procedure SetLang;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect;
    procedure Disconnect(confirm:boolean);
    property onConnect: TNotifyEvent read FonConnect write FonConnect;
    property onDisconnect: TNotifyEvent read FonDisconnect write FonDisconnect;
  end;

implementation

{$R *.lfm}

{ Tf_devicesconnection }

constructor Tf_devicesconnection.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 {$endif}
 ScaleDPI(Self);
 SetLang;
end;

destructor  Tf_devicesconnection.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_devicesconnection.SetLang;
begin
  Title.Caption:=rsDevicesConne;
  BtnConnect.Caption:=rsConnect;
  LabelCamera.Caption:=rsCam;
  LabelWheel.Caption:=rsFil;
  LabelFocuser.Caption:=rsFoc;
  LabelRotator.Caption:=rsRot;
  LabelMount.Caption:=rsMnt;
  LabelWatchdog.Caption:=rsWch;
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

