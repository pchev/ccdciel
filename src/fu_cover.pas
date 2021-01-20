unit fu_cover;

{$mode objfpc}{$H+}

{
Copyright (C) 2021 Patrick Chevalley

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

uses  UScaleDPI, u_global, Graphics, Dialogs, u_translation, cu_cover,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

type

  { Tf_cover }

  Tf_cover = class(TFrame)
    Label2: TLabel;
    ledCalibrator: TShape;
    PanelCover: TPanel;
    Label1: TLabel;
    ledCover: TShape;
    Panel1: TPanel;
    PanelCalibrator: TPanel;
    Title: TLabel;
  private
    { private declarations }
    FConnected: boolean;
    FCover: TCoverStatus;
    FCalibrator: TCalibratorStatus;
    procedure SetConnected(value:boolean);
    procedure SetCover(value:TCoverStatus);
    procedure SetCalibrator(value:TCalibratorStatus);
    procedure SetCoverLed;
    procedure SetCalibratorLed;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    property Connected: boolean read FConnected write SetConnected;
    property Cover: TCoverStatus read FCover write SetCover;
    property Calibrator: TCalibratorStatus read FCalibrator write SetCalibrator;
  end;

implementation

{$R *.lfm}

{ Tf_cover }

constructor Tf_cover.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 Panel1.ChildSizing.LeftRightSpacing:=8;
 Panel1.ChildSizing.VerticalSpacing:=4;
 {$endif}
 FConnected:=false;
 FCover:=covUnknown;;
 FCalibrator:=calUnknown;
 ledCover.Brush.Color:=clGray;
 ledCalibrator.Brush.Color:=clGray;
 ScaleDPI(Self);
 SetLang;
end;

destructor  Tf_cover.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_cover.SetLang;
begin
  Title.Caption:=rsCoverCalibra;
  label1.Caption:=rsCover;
  label2.Caption:=rsCalibrator;
end;

procedure Tf_cover.SetCoverLed;
begin
  if FConnected then begin
     if FCover=covOpen then begin
        ledCover.Brush.Color:=clLime;
     end
     else if FCover=covClosed then begin
        ledCover.Brush.Color:=clGreen;
     end
     else if FCover=covError then begin
        ledCover.Brush.Color:=clRed;
     end
     else begin
        ledCover.Brush.Color:=clGray;
     end;
  end
  else begin
     ledCover.Brush.Color:=clGray;
  end;
end;

procedure Tf_cover.SetCalibratorLed;
begin
  if FConnected then begin
     if FCalibrator=calOff then begin
        ledCalibrator.Brush.Color:=clLime;
     end
     else if FCalibrator=calReady then begin
        ledCalibrator.Brush.Color:=clGreen;
     end
     else if FCalibrator=calError then begin
        ledCalibrator.Brush.Color:=clRed;
     end
     else begin
        ledCalibrator.Brush.Color:=clGray;
     end;
  end
  else begin
     ledCalibrator.Brush.Color:=clGray;
  end;
end;

procedure Tf_cover.SetConnected(value:boolean);
begin
  FConnected:=value;
  SetCoverLed;
  SetCalibratorLed;
end;

procedure Tf_cover.SetCover(value:TCoverStatus);
begin
  FCover:=value;
  SetCoverLed;
end;

procedure Tf_cover.SetCalibrator(value:TCalibratorStatus);
begin
  FCalibrator:=value;
  SetCalibratorLed;
end;

end.

