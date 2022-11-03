unit pu_handpad;

{$mode ObjFPC}{$H+}

{
Copyright (C) 2022 Patrick Chevalley

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

uses u_global, UScaleDPI, u_hints, u_translation, cu_mount, u_utils, indiapi,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Spin, Arrow;

type

  { Tf_handpad }

  Tf_handpad = class(TForm)
    ArrowDown: TArrow;
    ArrowLeft: TArrow;
    ArrowRight: TArrow;
    ArrowStop: TButton;
    ArrowUp: TArrow;
    AxisRates: TComboBox;
    FlipNS: TRadioGroup;
    Handpad: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    StopMoveTimer: TTimer;
    procedure ArrowMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ArrowMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ArrowStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StopMoveTimerTimer(Sender: TObject);
  private
    FMount: T_mount;
    FLastArrow: integer;
    procedure SetMount(m:T_mount);
    procedure InitHandpad;
  public
    procedure SetLang;
    property Mount: T_mount read FMount write SetMount;

  end;

var
  f_handpad: Tf_handpad;

implementation

{$R *.lfm}

{ Tf_handpad }

procedure Tf_handpad.FormCreate(Sender: TObject);
begin
  ScaleDPI(self);
  SetLang;
  Handpad.Visible:=false;
  FlipNS.Visible:=false;
  AxisRates.Visible:=false;
end;

procedure Tf_handpad.SetLang;
begin
  Caption:=rsHandpad;
  FlipNS.Caption:=rsFlipNS;
end;

procedure Tf_handpad.SetMount(m:T_mount);
begin
  FMount:=m;
  InitHandpad;
end;

procedure Tf_handpad.InitHandpad;
var SlewRateList: TStringList;
begin
  if not Fmount.CanMoveAxis then begin
    Handpad.Visible:=false;
    FlipNS.Visible:=false;
    AxisRates.Visible:=false;
  end
  else begin
    Handpad.Visible:=true;
    FlipNS.Visible:=true;
    SlewRateList:=mount.SlewRates;
    if SlewRateList.Count>0 then begin
      AxisRates.Visible:=true;
      AxisRates.Items.Assign(SlewRateList);
      if  AxisRates.Items.Count>0 then AxisRates.ItemIndex:=0;
    end
    else
      AxisRates.Visible:=false;
    SlewRateList.Free;
    if mount.PierSide=pierEast then
      FlipNS.ItemIndex:=0
    else if mount.PierSide=pierWest then
      FlipNS.ItemIndex:=1;
  end;
end;


procedure Tf_handpad.ArrowMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var rate: string;
    flip: boolean;
begin
  rate:=AxisRates.Text;
  if sender is TArrow then begin
    with Sender as TArrow do begin
      ArrowColor:=clRed;
      FLastArrow:=tag;
      flip:=(FlipNS.ItemIndex>0);
      case Tag of
         1: FMount.MoveAxis(0,'-'+rate); //Left
         2: FMount.MoveAxis(0,rate); //Right
         3: FMount.MoveAxis(1,BoolToStr(flip,'-','')+rate); //Up
         4: FMount.MoveAxis(1,BoolToStr(not flip,'-','')+rate); //Down
      end;
    end;
  end;
end;

procedure Tf_handpad.ArrowStopClick(Sender: TObject);
begin
  FMount.AbortSlew;
  FMount.MoveAxis(0, '0');
  FMount.MoveAxis(1, '0');
  case FLastArrow of
    1: ArrowLeft.ArrowColor:=clBtnText;
    2: ArrowRight.ArrowColor:=clBtnText;
    3: ArrowUp.ArrowColor:=clBtnText;
    4: ArrowDown.ArrowColor:=clBtnText;
  end;
  FLastArrow:=0;
end;

procedure Tf_handpad.ArrowMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // let enough time if moveaxis is still not started
  StopMoveTimer.Interval:=100;
  StopMoveTimer.Enabled:=true;
end;

procedure Tf_handpad.StopMoveTimerTimer(Sender: TObject);
begin
  StopMoveTimer.Enabled:=false;
  ArrowStopClick(Sender);
end;

end.

