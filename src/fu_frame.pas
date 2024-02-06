unit fu_frame;

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

uses  UScaleDPI, u_translation, u_hints, u_global,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, StdCtrls, ExtCtrls;

type

  { Tf_frame }

  Tf_frame = class(TFrame)
    BtnSet: TButton;
    BtnReset: TButton;
    RoiList: TComboBox;
    FX: TEdit;
    FY: TEdit;
    FWidth: TEdit;
    FHeight: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PanelRoi: TPanel;
    Title: TLabel;
    procedure BtnResetClick(Sender: TObject);
    procedure BtnSetClick(Sender: TObject);
    procedure RoiListChange(Sender: TObject);
  private
    { private declarations }
    FonSet, FonReset: TNotifyEvent;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    procedure ClearRoi;
    procedure SetRoi(RoiName: string);
    property onSet: TNotifyEvent read FonSet write FonSet;
    property onReset: TNotifyEvent read FonReset write FonReset;
  end;

implementation

{$R *.lfm}

{ Tf_frame }

constructor Tf_frame.Create(aOwner: TComponent);
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

destructor  Tf_frame.Destroy;
begin
 ClearRoi;
 inherited Destroy;
end;

procedure Tf_frame.SetLang;
begin
  Title.Caption:=rsFrame;
  BtnSet.Caption:=rsSet;
  BtnReset.Caption:=rsReset;
  FX.Hint:=rsStartX;
  FY.Hint:=rsStartY;
  FWidth.Hint:=rsWidth;
  FHeight.Hint:=rsHeight;
end;

procedure Tf_frame.BtnSetClick(Sender: TObject);
begin
  if Assigned(FonSet) then FonSet(self);
end;

procedure Tf_frame.RoiListChange(Sender: TObject);
var i: integer;
begin
  i:=RoiList.ItemIndex;
  if (i>=0)and(RoiList.Items.Objects[i]<>nil) then begin
    FX.Text:=IntToStr(TRoi(RoiList.Items.Objects[i]).x);
    FY.Text:=IntToStr(TRoi(RoiList.Items.Objects[i]).y);
    FWidth.Text:=IntToStr(TRoi(RoiList.Items.Objects[i]).w);
    FHeight.Text:=IntToStr(TRoi(RoiList.Items.Objects[i]).h);
  end;
end;

procedure Tf_frame.SetRoi(RoiName: string);
var i: integer;
begin
  for i:=0 to RoiList.Items.Count-1 do begin
    if RoiList.Items[i]=RoiName then begin
      RoiList.ItemIndex:=i;
      RoiListChange(RoiList);
      if Assigned(FonSet) then FonSet(self);
      break;
    end;
  end;
end;

procedure Tf_frame.ClearRoi;
var i: integer;
begin
  for i:=RoiList.Items.Count-1 downto 0 do begin
    if RoiList.Items.Objects[i]<>nil then RoiList.Items.Objects[i].Free;
    RoiList.Items.Delete(i);
  end;
end;

procedure Tf_frame.BtnResetClick(Sender: TObject);
begin
  RoiList.ItemIndex:=-1;
  if Assigned(FonReset) then FonReset(self);
end;

end.

