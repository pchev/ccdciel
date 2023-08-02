unit fu_dome;

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

  { Tf_dome }

  Tf_dome = class(TFrame)
    BtnPark: TButton;
    BtnSlave: TButton;
    Label1: TLabel;
    ledOpen: TShape;
    Panel2: TPanel;
    Label2: TLabel;
    ledSlaved: TShape;
    Panel1: TPanel;
    Panel3: TPanel;
    Title: TLabel;
    procedure BtnParkClick(Sender: TObject);
    procedure BtnSlaveClick(Sender: TObject);
  private
    { private declarations }
    FConnected,FShutter,FSlave,FCanSlave: boolean;
    FParkDome, FStartSlaving: TNotifyEvent;
    procedure SetConnected(value:boolean);
    procedure SetShutter(value:boolean);
    procedure SetCanSlave(value:boolean);
    procedure SetSlave(value:boolean);
    procedure SetLed;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    property Connected: boolean read FConnected write SetConnected;
    property Shutter: boolean read FShutter write SetShutter;
    property CanSlave: boolean read FCanSlave write SetCanSlave;
    property Slave: boolean read FSlave write SetSlave;
    property onParkDome: TNotifyEvent read FParkDome write FParkDome;
    property onStartSlaving: TNotifyEvent read FStartSlaving write FStartSlaving;
  end;

implementation

{$R *.lfm}

{ Tf_dome }

constructor Tf_dome.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 Panel1.ChildSizing.LeftRightSpacing:=8;
 Panel1.ChildSizing.VerticalSpacing:=4;
 {$endif}
 FConnected:=false;
 FShutter:=false;
 FCanSlave:=true;
 FSlave:=false;
 ledOpen.Brush.Color:=clGray;
 ledSlaved.Brush.Color:=clGray;
 ScaleDPI(Self);
 SetLang;
 ledOpen.Canvas.AntialiasingMode:=amOn;
 ledSlaved.Canvas.AntialiasingMode:=amOn;
end;

destructor  Tf_dome.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_dome.SetLang;
begin
  Title.Caption:=rsDome;
  Label1.Caption:=rsOpen;
  Label2.Caption:=rsSlaved;
  BtnPark.Caption:=rsPark;
  BtnSlave.Caption:=rsSlave;
end;

procedure Tf_dome.SetLed;
begin
  if FConnected then begin
    if FShutter then
       ledOpen.Brush.Color:=clLime
    else
       ledOpen.Brush.Color:=clRed;
    if FSlave then
       ledSlaved.Brush.Color:=clLime
    else
       ledSlaved.Brush.Color:=clRed;
  end
  else begin
     ledOpen.Brush.Color:=clGray;
     ledSlaved.Brush.Color:=clGray;
  end;
end;

procedure Tf_dome.BtnParkClick(Sender: TObject);
begin
  if Assigned(FParkDome) then FParkDome(self);
end;

procedure Tf_dome.BtnSlaveClick(Sender: TObject);
begin
  if Assigned(FStartSlaving) then FStartSlaving(self);
end;

procedure Tf_dome.SetConnected(value:boolean);
begin
  FConnected:=value;
  SetLed;
end;

procedure Tf_dome.SetShutter(value:boolean);
begin
  FShutter:=value;
  SetLed;
end;

procedure Tf_dome.SetCanSlave(value:boolean);
begin
  FCanSlave:=value;
  Label2.Visible:=FCanSlave;
  ledSlaved.Visible:=FCanSlave;
  BtnSlave.Visible:=FCanSlave;
end;

procedure Tf_dome.SetSlave(value:boolean);
begin
  FSlave:=value;
  SetLed;
end;

end.

