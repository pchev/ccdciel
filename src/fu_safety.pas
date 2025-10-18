unit fu_safety;

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

  { Tf_safety }

  Tf_safety = class(TFrame)
    Panel2: TPanel;
    Label1: TLabel;
    led: TShape;
    Panel1: TPanel;
    Title: TLabel;
  private
    { private declarations }
    FConnected,FSafe: boolean;
    procedure SetConnected(value:boolean);
    procedure SetSafe(value:boolean);
    procedure SetLed;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    procedure SetTitleColor;
    property Connected: boolean read FConnected write SetConnected;
    property Safe: boolean read FSafe write SetSafe;
  end;

implementation

{$R *.lfm}

{ Tf_safety }

constructor Tf_safety.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Panel1.ChildSizing.LeftRightSpacing:=8;
 Panel1.ChildSizing.VerticalSpacing:=4;
 {$endif}
 FConnected:=false;
 FSafe:=false;;
 led.Brush.Color:=clGray;
 led.Hint:=rsUnsafe;
 ScaleDPI(Self);
 SetLang;
 led.Canvas.AntialiasingMode:=amOn;
end;

destructor  Tf_safety.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_safety.SetTitleColor;
begin
  Title.Color:=InterfaceColor[TitleColor,1];
  Title.Font.Color:=InterfaceColor[TitleColor,2];
  Title.Font.Style:=[fsBold];
end;

procedure Tf_safety.SetLang;
begin
  Title.Caption:=rsSafetyMonito;
  label1.Caption:=rsSafeForOpera;
end;

procedure Tf_safety.SetLed;
begin
  if FConnected then begin
     if FSafe then begin
        led.Brush.Color:=clLime;
        led.Hint:=rsSafe;
     end
     else begin
        led.Brush.Color:=clRed;
        led.Hint:=rsUnsafe;
     end;
  end
  else begin
     led.Brush.Color:=clGray;
     led.Hint:=rsDisconnected3;
  end;
end;

procedure Tf_safety.SetConnected(value:boolean);
begin
  FConnected:=value;
  SetLed;
end;

procedure Tf_safety.SetSafe(value:boolean);
begin
  FSafe:=value;
  SetLed;
end;

end.

