unit fu_autoguider;

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

{$mode objfpc}{$H+}

interface

uses  UScaleDPI, u_translation,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

type

  { Tf_autoguider }

  Tf_autoguider = class(TFrame)
    BtnConnect: TButton;
    BtnCal: TButton;
    BtnGuide: TButton;
    BtnDither: TButton;
    Panel1: TPanel;
    led: TShape;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Status: TEdit;
    Title: TLabel;
    procedure BtnCalClick(Sender: TObject);
    procedure BtnConnectClick(Sender: TObject);
    procedure BtnDitherClick(Sender: TObject);
    procedure BtnGuideClick(Sender: TObject);
  private
    { private declarations }
    FonConnect,FonCalibrate,FonGuide,FonDither: TNotifyEvent;
    procedure SetLang;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    property onConnect: TNotifyEvent read FonConnect write FonConnect;
    property onCalibrate: TNotifyEvent read FonCalibrate write FonCalibrate;
    property onGuide: TNotifyEvent read FonGuide write FonGuide;
    property onDither: TNotifyEvent read FonDither write FonDither;
  end;

implementation

{$R *.lfm}

{ Tf_autoguider }

constructor Tf_autoguider.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 ScaleDPI(Self);
 SetLang;
end;

destructor  Tf_autoguider.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_autoguider.SetLang;
begin
  Title.Caption:=rsAutoguider;
  BtnConnect.Caption:=rsConnect;
  BtnCal.Caption:=rsCalibrate;
  BtnGuide.Caption:=rsGuide;
  BtnDither.Caption:=rsDither;
end;

procedure Tf_autoguider.BtnConnectClick(Sender: TObject);
begin
   if Assigned(FonConnect) then FonConnect(self);
end;

procedure Tf_autoguider.BtnDitherClick(Sender: TObject);
begin
   if Assigned(FonDither) then FonDither(self);
end;

procedure Tf_autoguider.BtnGuideClick(Sender: TObject);
begin
   if Assigned(FonGuide) then FonGuide(self);
end;

procedure Tf_autoguider.BtnCalClick(Sender: TObject);
begin
   if Assigned(FonCalibrate) then FonCalibrate(self);
end;

end.

