unit fu_ccdtemp;

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

uses   UScaleDPI, Dialogs, u_translation, u_global,
  Classes, SysUtils, FileUtil, Forms, Graphics, Controls, StdCtrls, ExtCtrls, SpinEx;

type

  { Tf_ccdtemp }

  Tf_ccdtemp = class(TFrame)
    BtnSet: TButton;
    CCDcooler: TCheckBox;
    Current: TLabel;
    Setpoint: TFloatSpinEditEx;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Title: TLabel;
    procedure BtnSetClick(Sender: TObject);
    procedure CCDcoolerChange(Sender: TObject);
  private
    { private declarations }
    FonSetTemperature,FonSetCooler: TNotifyEvent;
    procedure SetLang;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    property onSetTemperature: TNotifyEvent read FonSetTemperature write FonSetTemperature;
    property onSetCooler: TNotifyEvent read FonSetCooler write FonSetCooler;
  end;

implementation

{$R *.lfm}

{ Tf_ccdtemp }

constructor Tf_ccdtemp.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 Panel1.ChildSizing.LeftRightSpacing:=8;
 Panel1.ChildSizing.VerticalSpacing:=4;
 {$endif}
 ScaleDPI(Self);
 SetLang;
end;

destructor  Tf_ccdtemp.Destroy;
begin
 inherited Destroy;
end;


procedure Tf_ccdtemp.SetLang;
begin
  Title.Caption:=rsSensorTemperatu+blank+sdeg+'C';
  Label1.Caption:=rsCurrent;
  CCDcooler.Caption:=rsCooler;
  Label2.Caption:=rsSetpoint;
  BtnSet.Caption:=rsSet;
end;

procedure Tf_ccdtemp.BtnSetClick(Sender: TObject);
begin
  if Assigned(FonSetTemperature) then FonSetTemperature(self);
end;

procedure Tf_ccdtemp.CCDcoolerChange(Sender: TObject);
begin
  if Assigned(FonSetCooler) then FonSetCooler(self);
end;

end.

