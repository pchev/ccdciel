unit fu_rotator;

{$mode objfpc}{$H+}

{
Copyright (C) 2017 Patrick Chevalley

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
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, Buttons, SpinEx;

type

  { Tf_rotator }

  Tf_rotator = class(TFrame)
    Angle180: TStaticText;
    BtnResetSync: TButton;
    BtnRotate: TButton;
    BtnHalt: TButton;
    Angle: TFloatSpinEditEx;
    CalAngle: TStaticText;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    PanelDetails: TPanel;
    PanelSoftsync: TPanel;
    PanelSoftlimit: TPanel;
    PanelSoft: TPanel;
    Reverse: TCheckBox;
    Label6: TLabel;
    Panel1: TPanel;
    btnShowDetails: TSpeedButton;
    MechAngle: TStaticText;
    Title: TLabel;
    procedure BtnResetSyncClick(Sender: TObject);
    procedure BtnHaltClick(Sender: TObject);
    procedure BtnRotateClick(Sender: TObject);
    procedure ReverseChange(Sender: TObject);
    procedure btnShowDetailsClick(Sender: TObject);
  private
    { private declarations }
    FonRotate: TNotifyEvent;
    FonReverse: TNotifyEvent;
    FonHalt: TNotifyEvent;
    FonResetSync: TNotifyEvent;
    lockreverse, noprompt: boolean;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    procedure SetTitleColor;
    procedure SetReverse(onoff:boolean);
    property onRotate: TNotifyEvent read FonRotate write FonRotate;
    property onReverse: TNotifyEvent read FonReverse write FonReverse;
    property onHalt: TNotifyEvent read FonHalt write FonHalt;
    property onResetSync: TNotifyEvent read FonResetSync write FonResetSync;
  end;

implementation

{$R *.lfm}

{ Tf_rotator }

constructor Tf_rotator.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Panel1.ChildSizing.LeftRightSpacing:=8;
 Panel1.ChildSizing.VerticalSpacing:=4;
 {$endif}
 ScaleDPI(Self);
 noprompt:=false;
 lockreverse:=false;
 SetLang;
end;

destructor  Tf_rotator.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_rotator.SetTitleColor;
begin
  Title.Color:=InterfaceColor[TitleColor,1];
  Title.Font.Color:=InterfaceColor[TitleColor,2];
  Title.Font.Style:=[fsBold];
end;

procedure Tf_rotator.SetLang;
begin
  Title.Caption:=rsRotator;
  label6.Caption:=rsPA;
  BtnRotate.Caption:=rsRotate;
  Reverse.Caption:=rsReverse;
  BtnHalt.Caption:=rsHalt;
  BtnResetSync.Caption:=rsReset;
  Label2.Caption:=rsSyncOffset;
  Label1.Caption:=rsPA+'+180'+sdeg;
  Label3.Caption:=rsMechanical;
  btnShowDetails.Caption:=ellipsis;
end;

procedure Tf_rotator.SetReverse(onoff:boolean);
begin
  noprompt:=true;
  Reverse.Checked:=onoff;
  noprompt:=false;
end;

procedure Tf_rotator.BtnRotateClick(Sender: TObject);
begin
   if Assigned(FonRotate) then FonRotate(self);
end;

procedure Tf_rotator.BtnHaltClick(Sender: TObject);
begin
   if Assigned(FonHalt) then FonHalt(self);
end;

procedure Tf_rotator.BtnResetSyncClick(Sender: TObject);
begin
  if Assigned(FonResetSync) then FonResetSync(self);
end;

procedure Tf_rotator.ReverseChange(Sender: TObject);
begin
   if lockreverse then exit;
   if Assigned(FonReverse) then FonReverse(self);
end;

procedure Tf_rotator.btnShowDetailsClick(Sender: TObject);
begin
  PanelDetails.Visible:=btnShowDetails.Down;
end;

end.

