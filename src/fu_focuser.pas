unit fu_focuser;

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

uses  UScaleDPI, u_global, u_translation, u_hints,
  Classes, SysUtils, FileUtil, Forms, Graphics, Controls, StdCtrls, ExtCtrls, Spin;

type

  { Tf_focuser }

  Tf_focuser = class(TFrame)
    BtnDown: TButton;
    BtnUp: TButton;
    BtnSetAbsPos: TButton;
    BtnVcurve: TButton;
    Label5: TLabel;
    lblTemp: TLabel;
    PanelTemp: TPanel;
    speed: TSpinEdit;
    Position: TSpinEdit;
    timer: TSpinEdit;
    Temp: TEdit;
    Label6: TLabel;
    Notebook1: TNotebook;
    PageTimerMove: TPage;
    PageRelPos: TPage;
    PageAbsPos: TPage;
    PanelRelPos: TPanel;
    PanelParam: TPanel;
    RelIncr: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PanelAbsPos: TPanel;
    PanelTimerMove: TPanel;
    PosIncr: TComboBox;
    PanelBtn: TPanel;
    Panel1: TPanel;
    Title: TLabel;
    procedure BtnDownClick(Sender: TObject);
    procedure BtnSetAbsPosClick(Sender: TObject);
    procedure BtnUpClick(Sender: TObject);
    procedure BtnVcurveClick(Sender: TObject);
  private
    { private declarations }
    FonFocusIN, FonFocusOUT, FonSetAbsPos,FonVcurveLearning: TNotifyEvent;
    FBacklashActive: boolean;
    procedure SetSpeed(value:integer);
    function GetSpeed:integer;
    procedure SetPosition(value:integer);
    function GetPosition:integer;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    function TempOffset(TempRef,TempNow: double):integer;
    property FocusSpeed: integer read GetSpeed write SetSpeed;
    property FocusPosition: integer read GetPosition write SetPosition;
    property BacklashActive: boolean read FBacklashActive write FBacklashActive;
    property onFocusIN: TNotifyEvent read FonFocusIN write FonFocusIN;
    property onFocusOUT: TNotifyEvent read FonFocusOUT write FonFocusOUT;
    property onSetAbsolutePosition: TNotifyEvent read FonSetAbsPos write FonSetAbsPos;
    property onVcurveLearning: TNotifyEvent read FonVcurveLearning write FonVcurveLearning;
  end;

implementation

{$R *.lfm}

{ Tf_focuser }

constructor Tf_focuser.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 Panel1.ChildSizing.LeftRightSpacing:=8;
 Panel1.ChildSizing.VerticalSpacing:=4;
 {$endif}
 Notebook1.PageIndex:=2;
 ScaleDPI(Self);
 SetLang;
end;

destructor  Tf_focuser.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_focuser.SetLang;
begin
  Title.Caption:=rsFocuser;
  label3.Caption:=rsSpeed;
  label4.Caption:=rsTimer;
  label6.Caption:=rsIncr;
  label1.Caption:=rsPos;
  label2.Caption:=rsStep;
  BtnSetAbsPos.Caption:=rsSet;
  BtnVcurve.Caption:=rsVLearn;
  label5.Caption:=rsTemp;
  RelIncr.Hint:=rsRelativeIncr;
  PosIncr.Hint:=rsIncrementSte;
  BtnSetAbsPos.Hint:=rsSetAbsoluteF;
  BtnVcurve.Hint:=rsVCurveLearni;
  BtnDown.Hint:=rsMoveFocuserI;
  BtnUp.Hint:=rsMoveFocuserO;
end;

procedure Tf_focuser.BtnDownClick(Sender: TObject);
begin
  if Assigned(FonFocusIN) then FonFocusIN(self);
end;

procedure Tf_focuser.BtnSetAbsPosClick(Sender: TObject);
begin
  if Assigned(FonSetAbsPos) then FonSetAbsPos(self);
end;

procedure Tf_focuser.BtnUpClick(Sender: TObject);
begin
  if Assigned(FonFocusOUT) then FonFocusOUT(self);
end;

procedure Tf_focuser.BtnVcurveClick(Sender: TObject);
begin
  if Assigned(FonVcurveLearning) then FonVcurveLearning(self);
end;

procedure Tf_focuser.SetSpeed(value:integer);
begin
  case Notebook1.PageIndex of
    0: timer.Value:=value;  // Timer
    1: RelIncr.Text:=inttostr(value);// Relative
    2: PosIncr.Text:=inttostr(value);// Absolute
  end;
end;

function Tf_focuser.GetSpeed:integer;
begin
 case Notebook1.PageIndex of
   0: result:=timer.Value;  // Timer
   1: result:=Strtointdef(RelIncr.Text,-1);// Relative
   2: result:=Strtointdef(PosIncr.Text,-1);// Absolute
 end;
end;

procedure Tf_focuser.SetPosition(value:integer);
begin
 begin
   case Notebook1.PageIndex of
     2: Position.Value:=value;// Absolute
   end;
 end;
end;

function Tf_focuser.GetPosition:integer;
begin
 case Notebook1.PageIndex of
   2: result:=Position.Value;// Absolute
   else result:=-1;
 end;
end;

function Tf_focuser.TempOffset(TempRef,TempNow: double):integer;
begin
  // coeff: steps/C , positive = move OUT for temperature drop
  // result: positive=OUT , negative=IN
  result:=round((TempRef-TempNow)*FocuserTempCoeff);
end;

end.

