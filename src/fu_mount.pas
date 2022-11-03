unit fu_mount;

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

uses UScaleDPI,  u_translation, u_hints, u_global, u_utils,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, Graphics, Buttons;

type

  { Tf_mount }

  Tf_mount = class(TFrame)
    BtnGoto: TButton;
    DE: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    ALT: TLabel;
    AZ: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Label4: TLabel;
    btnTrack: TSpeedButton;
    BtnPark: TSpeedButton;
    Panel6: TPanel;
    Pierside: TLabel;
    RA: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    LabelMeridian: TLabel;
    Panel1: TPanel;
    BtnHandpad: TSpeedButton;
    TimeToMeridian: TLabel;
    Title: TLabel;
    procedure BtnGotoClick(Sender: TObject);
    procedure BtnHandpadClick(Sender: TObject);
    procedure BtnParkClick(Sender: TObject);
    procedure BtnTrackClick(Sender: TObject);
  private
    { private declarations }
    FCurrentRA,FCurrentDec: double;
    FonPark  : TNotifyEvent;
    FonTrack  : TNotifyEvent;
    FonGoto  : TNotifyEvent;
    FonHandpad : TNotifyEvent;
    procedure SetCurrentRA(value:double);
    procedure SetCurrentDec(value:double);
    procedure ShowAltAz;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    property CurrentRA: double read FCurrentRA write SetCurrentRA;
    property CurrentDec: double read FCurrentDec write SetCurrentDec;
    property onPark  : TNotifyEvent read FonPark write FonPark;
    property onTrack  : TNotifyEvent read FonTrack write FonTrack;
    property onGoto  : TNotifyEvent read FonGoto write FonGoto;
    property onHandpad : TNotifyEvent read FonHandpad write FonHandpad;
  end;

implementation

{$R *.lfm}

{ Tf_mount }

constructor Tf_mount.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 Panel1.ChildSizing.LeftRightSpacing:=8;
 Panel1.ChildSizing.VerticalSpacing:=4;
 {$endif}
 ScaleDPI(Self);
 SetLang;
 FCurrentRA:=NullCoord;
 FCurrentDec:=NullCoord;
end;

destructor  Tf_mount.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_mount.SetLang;
begin
  Title.Caption:=rsTelescopePos;
  label1.Caption:=rsRA+':';
  label2.Caption:=rsDec+':';
  label3.Caption:=rsAz+':';
  label5.Caption:=rsAlt+':';
  BtnPark.Caption:=rsPark;
  BtnTrack.Caption:=rsTrack;
  BtnGoto.Caption:=rsGoto;
  LabelMeridian.Caption:=rsMeridianIn;
  label4.Caption:=rsMin;
  Pierside.Caption:=rsUnknowPierSi;
  Ra.Hint:=rsCurrentTeles;
  DE.Hint:=rsCurrentTeles2;
  Pierside.Hint:=rsCurrentTeles3;
  TimeToMeridian.Hint:=rsTimeFromMeri;
  BtnHandpad.Caption:=ellipsis;
end;

procedure Tf_mount.SetCurrentRA(value:double);
begin
  FCurrentRA:=value;
  RA.Caption:=RAToStr(FCurrentRA);
  ShowAltAz;
end;

procedure Tf_mount.SetCurrentDec(value:double);
begin
  FCurrentDec:=value;
  DE.Caption:=DEToStr(FCurrentDec);
  ShowAltAz;
end;

procedure Tf_mount.ShowAltAz;
var a,h: double;
begin
 if (FCurrentRA<>NullCoord)and(FCurrentDec<>NullCoord) then begin
   cmdEq2Hz(FCurrentRA,FCurrentDec,a,h);
   if AzimuthOrigin=azSouth then a:=rmod(180+a,360);
   AZ.Caption:=FormatFloat(f2,a);
   ALT.Caption:=FormatFloat(f2,h);
 end;
end;

procedure Tf_mount.BtnParkClick(Sender: TObject);
begin
  if BtnPark.Font.Color=clGreen then begin
     if MessageDlg(rsParkTheTeles, mtConfirmation, mbYesNo, 0)<>mrYes then exit;
  end;
  if assigned(FonPark) then FonPark(self);
end;

procedure Tf_mount.BtnGotoClick(Sender: TObject);
begin
  if assigned(FonGoto) then FonGoto(self);
end;

procedure Tf_mount.BtnHandpadClick(Sender: TObject);
begin
  if assigned(FonHandpad) then FonHandpad(self);
end;

procedure Tf_mount.BtnTrackClick(Sender: TObject);
begin
  if assigned(FonTrack) then FonTrack(self);
end;

end.

