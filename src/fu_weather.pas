unit fu_weather;

{$mode objfpc}{$H+}

{
Copyright (C) 2018 Patrick Chevalley

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

uses  UScaleDPI, u_global, Graphics, Dialogs, u_translation,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

type

  { Tf_weather }

  Tf_weather = class(TFrame)
    ButtonDetail: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    led: TShape;
    Panel1: TPanel;
    Title: TLabel;
    procedure ButtonDetailClick(Sender: TObject);
  private
    { private declarations }
    FConnected,FClear: boolean;
    FonDetail: TNotifyEvent;
    procedure SetConnected(value:boolean);
    procedure SetClear(value:boolean);
    procedure SetLed;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    property Connected: boolean read FConnected write SetConnected;
    property Clear: boolean read FClear write SetClear;
    property onDetail: TNotifyEvent read FonDetail write FonDetail;
  end;

implementation

{$R *.lfm}

{ Tf_weather }

constructor Tf_weather.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 Panel1.ChildSizing.LeftRightSpacing:=8;
 Panel1.ChildSizing.VerticalSpacing:=4;
 {$endif}
 FConnected:=false;
 FClear:=false;
 led.Brush.Color:=clGray;
 led.Hint:=rsDisconnected3;
 ScaleDPI(Self);
 SetLang;
 led.Canvas.AntialiasingMode:=amOn;
end;

destructor  Tf_weather.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_weather.SetLang;
begin
  Title.Caption:=rsWeatherStati;
  label1.Caption:=rsClearConditi;
  ButtonDetail.Caption:=rsDetails;
end;

procedure Tf_weather.SetLed;
begin
  if FConnected then begin
     if FClear then begin
        led.Brush.Color:=clLime;
        led.Hint:=Format(rsWeatherMonit,[rsGood]);
     end
     else begin
        led.Brush.Color:=clRed;
        led.Hint:=Format(rsWeatherMonit,[rsBad]);
     end;
  end
  else begin
     led.Brush.Color:=clGray;
     led.Hint:=rsDisconnected3;
  end;
end;

procedure Tf_weather.ButtonDetailClick(Sender: TObject);
begin
  if Assigned(FonDetail) then FonDetail(self);
end;

procedure Tf_weather.SetConnected(value:boolean);
begin
  FConnected:=value;
  SetLed;
end;

procedure Tf_weather.SetClear(value:boolean);
begin
  FClear:=value;
  SetLed;
end;

end.

