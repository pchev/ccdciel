unit pu_autoexposurestep;

{
Copyright (C) 2024 Patrick Chevalley

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

{$mode ObjFPC}{$H+}

interface

uses  u_translation, u_global, math, Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Spin, ExtCtrls, Menus;

type

  { Tf_autoexposurestep }

  Tf_autoexposurestep = class(TForm)
    btnOK: TButton;
    cbRef: TComboBox;
    Exposure: TFloatSpinEdit;
    MaxExp: TFloatSpinEdit;
    GroupBox2: TGroupBox;
    Label6: TLabel;
    MenuItemAddRef: TMenuItem;
    MenuItemDelRef: TMenuItem;
    MenuRef: TButton;
    Panel1: TPanel;
    PopupMenuRef: TPopupMenu;
    RefMagn: TFloatSpinEdit;
    RefExp: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Magnitude: TFloatSpinEdit;
    Label2: TLabel;
    procedure cbRefChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MagnitudeChange(Sender: TObject);
    procedure MenuItemAddRefClick(Sender: TObject);
    procedure MenuItemDelRefClick(Sender: TObject);
    procedure MenuRefMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RefChange(Sender: TObject);
  private
    procedure ComputeExposure;
    procedure UpdateRef;
    function GetExposure_str: string;
  public
    procedure SetLang;
    procedure ClearRefList;
    function SetRef(refname:string): boolean;
    property Exposure_str: string read GetExposure_str;
  end;

var
  f_autoexposurestep: Tf_autoexposurestep;

implementation

{$R *.lfm}

{ Tf_autoexposurestep }

procedure Tf_autoexposurestep.SetLang;
begin
  Caption:=Format(rsStarAutoExpo, [' ']);
  GroupBox1.Caption:=rsReferenceExp;
  GroupBox2.Caption:=rsTest;
  Label4.Caption:=rsMagnitude;
  Label5.Caption:=rsExposure;
  Label2.Caption:=rsMagnitude;
  Label3.Caption:=rsExposure;
  Label6.Caption:=rsMaxExposure;
  MenuRef.Caption:=rsManage;
  MenuItemAddRef.Caption:=rsAdd;
  MenuItemDelRef.Caption:=rsDelete;
  btnOK.Caption:=rsOK;
end;

procedure Tf_autoexposurestep.FormCreate(Sender: TObject);
begin
  SetLang;
end;

procedure Tf_autoexposurestep.FormShow(Sender: TObject);
begin
  ComputeExposure;
end;

procedure Tf_autoexposurestep.FormDestroy(Sender: TObject);
begin
  ClearRefList;
end;

procedure Tf_autoexposurestep.ClearRefList;
var i: integer;
begin
  for i:=cbRef.Items.Count-1 downto 0 do begin
    if cbRef.Items.Objects[i]<>nil then cbRef.Items.Objects[i].Free;
    cbRef.Items.Delete(i);
  end;
end;

function Tf_autoexposurestep.SetRef(refname:string): boolean;
var i: integer;
begin
  i:=cbRef.Items.IndexOf(refname);
  if i>=0 then begin
    cbRef.ItemIndex:=i;
    result:=true;
  end
  else
    result:=false;
end;

procedure Tf_autoexposurestep.cbRefChange(Sender: TObject);
var ref:TStarAutoexposureRef;
    i: integer;
begin
  i:=cbRef.ItemIndex;
  if (i>=0)and(i<cbRef.Items.Count) then begin
    ref:=TStarAutoexposureRef(cbRef.Items.Objects[i]);
    cbRef.text := ref.refname;
    RefMagn.Value := ref.magn;
    RefExp.Value := ref.exp;
    MaxExp.Value := ref.maxexp;
    ComputeExposure;
  end;
end;

procedure Tf_autoexposurestep.UpdateRef;
var ref:TStarAutoexposureRef;
    i: integer;
begin
  i:=cbRef.ItemIndex;
  if i>=0 then begin
    ref:=TStarAutoexposureRef(cbRef.items.Objects[i]);
    ref.magn:=RefMagn.Value;
    ref.exp:=RefExp.Value;
    ref.maxexp:=MaxExp.Value;
    cbRef.Text:=ref.refname;
  end;
end;

procedure Tf_autoexposurestep.MenuItemAddRefClick(Sender: TObject);
var ref:TStarAutoexposureRef;
    i: integer;
    n: string;
begin
  n:=trim(cbRef.text);
  i:=cbRef.Items.IndexOf(n);
  if i>=0 then begin
    ref:=TStarAutoexposureRef(cbRef.items.Objects[i]);
    ref.magn:=RefMagn.Value;
    ref.exp:=RefExp.Value;
    ref.maxexp:=MaxExp.Value;
  end
  else begin
    ref:=TStarAutoexposureRef.Create;
    ref.refname:=n;
    ref.magn:=RefMagn.Value;
    ref.exp:=RefExp.Value;
    ref.maxexp:=MaxExp.Value;
    i:=cbRef.Items.AddObject(ref.refname,ref);
  end;
  cbRef.ItemIndex:=i;
end;

procedure Tf_autoexposurestep.MenuItemDelRefClick(Sender: TObject);
var i: integer;
begin
 i:=cbRef.ItemIndex;
 if (i>=0)and(i<cbRef.Items.Count) then begin
   if cbRef.Items.Objects[i]<>nil then cbRef.Items.Objects[i].Free;
   cbRef.Items.Delete(i);
   if i>1 then
     cbRef.ItemIndex:=i-1
   else
     cbRef.ItemIndex:=0;
   cbRefChange(Sender);
 end;
end;

procedure Tf_autoexposurestep.MenuRefMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var p: TPoint;
begin
  p.x:=0;
  p.y:=MenuRef.Height;
  p:=MenuRef.ClientToScreen(p);
  PopupMenuRef.PopUp(p.x,p.y);
end;


procedure Tf_autoexposurestep.RefChange(Sender: TObject);
begin
  UpdateRef;
  ComputeExposure;
end;

procedure Tf_autoexposurestep.MagnitudeChange(Sender: TObject);
begin
  ComputeExposure;
end;

procedure Tf_autoexposurestep.ComputeExposure;
var exp: double;
begin
  exp := RefExp.Value / (10.0 ** ((RefMagn.Value-Magnitude.Value)/2.5));
  if exp>MaxExp.Value then exp:=MaxExp.Value;
  if exp>10 then exp:=round(exp);
  Exposure.Value := exp;
end;

function Tf_autoexposurestep.GetExposure_str: string;
begin
  result:=FormatFloat(f3v,Exposure.Value);
end;

end.

