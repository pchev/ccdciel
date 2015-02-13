unit fu_autoguider;

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

type

  { Tf_autoguider }

  Tf_autoguider = class(TFrame)
    BtnConnect: TButton;
    BtnCal: TButton;
    BtnGuide: TButton;
    BtnDither: TButton;
    Panel1: TPanel;
    Status: TEdit;
    StaticText1: TStaticText;
    procedure BtnCalClick(Sender: TObject);
    procedure BtnConnectClick(Sender: TObject);
    procedure BtnDitherClick(Sender: TObject);
    procedure BtnGuideClick(Sender: TObject);
  private
    { private declarations }
    FonConnect,FonCalibrate,FonGuide,FonDither: TNotifyEvent;
  public
    { public declarations }
    property onConnect: TNotifyEvent read FonConnect write FonConnect;
    property onCalibrate: TNotifyEvent read FonCalibrate write FonCalibrate;
    property onGuide: TNotifyEvent read FonGuide write FonGuide;
    property onDither: TNotifyEvent read FonDither write FonDither;
  end;

implementation

{$R *.lfm}

{ Tf_autoguider }

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

