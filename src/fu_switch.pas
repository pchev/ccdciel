unit fu_switch;

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

uses  UScaleDPI, u_global, u_utils, Graphics, Dialogs, u_translation, cu_switch, fu_switchpage,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls;

type

  { Tf_switch }

  Tf_switch = class(TFrame)
    PageControl1: TPageControl;
    Panel1: TPanel;
    Title: TLabel;
  private
    { private declarations }
    FConnected: array[0..MaxSwitches] of boolean;
    FonSetSwitch: TNotifyEvent;
    procedure DetachForm(Sender: TObject);
    procedure DetachFormClose(Sender: TObject; var CloseAction: TCloseAction);

  public
    { public declarations }
    SwitchPage: array[0..MaxSwitches] of integer;
    PinGlyph: TImage;
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    procedure SetTitleColor;
    procedure Clear;
    procedure AddSwitch(sw:T_switch);
    procedure SetConnected(n: integer; s:boolean);
    function Connected(n: integer): boolean;
    procedure SwitchChange(sw:T_switch);
    property onSetSwitch: TNotifyEvent read FonSetSwitch write FonSetSwitch;
  end;

implementation

{$R *.lfm}

{ Tf_switch }

constructor Tf_switch.Create(aOwner: TComponent);
var i: integer;
begin
 inherited Create(aOwner);
 for i:=0 to MaxSwitches do begin
   FConnected[i]:=false;
   SwitchPage[i]:=-1;
 end;
 PinGlyph:=TImage.Create(self);
 ScaleDPI(Self);
 SetLang;
end;

destructor  Tf_switch.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure Tf_switch.SetTitleColor;
begin
  Title.Color:=InterfaceColor[TitleColor,1];
  Title.Font.Color:=InterfaceColor[TitleColor,2];
  Title.Font.Style:=[fsBold];
end;

procedure Tf_switch.Clear;
var i: integer;
    sp: Tf_switchpage;
begin
 for i:=0 to PageControl1.PageCount-1 do begin
   sp:=Tf_switchpage(PageControl1.Pages[i].Controls[0]);
   sp.Free;
 end;
 PageControl1.Clear;
 for i:=0 to MaxSwitches do begin
   FConnected[i]:=false;
   SwitchPage[i]:=-1;
 end;
 Height:=DoScaleY(80);
 PageControl1.ShowTabs:=false;
end;

procedure Tf_switch.SetLang;
begin
  Title.Caption:=rsSwitch;
end;

procedure Tf_switch.AddSwitch(sw:T_switch);
var tb: TTabSheet;
    sp: Tf_switchpage;
begin
 if sw.tag>MaxSwitches then exit;
 tb:=PageControl1.AddTabSheet;
 SwitchPage[sw.Tag]:=tb.PageIndex;
 tb.Caption:=sw.Nickname;
 sp:=Tf_switchpage.Create(self);
 sp.name:='Switchpage'+inttostr(sw.tag);
 sp.Parent:=tb;
 sp.Align:=alClient;
 sp.Tag:=sw.Tag;
 sp.BtnPinSwitch.Glyph.Assign(PinGlyph.Picture.Bitmap);
 sp.NumSwitch:=sw.NumSwitch;
 sp.Switch:=sw.Switch;
 sp.onSetSwitch:=FonSetSwitch;
 sp.onDetach:=@DetachForm;
 PageControl1.ShowTabs:=(PageControl1.PageCount>1)or(sw.DeviceName<>'');
 Height:=DoScaleY(250);
end;

procedure Tf_switch.SetConnected(n: integer; s:boolean);
begin
  if n>MaxSwitches then exit;
  FConnected[n]:=s;
end;

function Tf_switch.Connected(n: integer): boolean;
begin
 if n>MaxSwitches then exit;
 result:=FConnected[n];
end;

procedure Tf_switch.SwitchChange(sw:T_switch);
var i: integer;
    sp: Tf_switchpage;
begin
  if sw.tag>MaxSwitches then exit;
  i:=SwitchPage[sw.tag];
  if i<0 then begin
    AddSwitch(sw);
    i:=SwitchPage[sw.tag];
  end;
  sp:=Tf_switchpage(PageControl1.Pages[i].Controls[0]);
  sp.Parent.Caption:=sw.Nickname;
  sp.NumSwitch:=sw.NumSwitch;
  sp.Switch:=sw.Switch;
  sp.LabelMsg.Caption:=sw.LastError;
  PageControl1.ShowTabs:=(PageControl1.PageCount>1)or(sw.DeviceName<>'');
end;

procedure Tf_switch.DetachForm(Sender: TObject);
var f: TForm;
    x,y,w,h: integer;
begin
  if PageControl1.Parent=Panel1 then begin
   // detach the graph
   x:=config.GetValue('/Switch/PosX',-1);
   y:=config.GetValue('/Switch/PosY',-1);
   w:=config.GetValue('/Switch/PosW',-1);
   h:=config.GetValue('/Switch/PosH',-1);
   f:=TForm.Create(self);
   f.FormStyle:=fsStayOnTop;
   f.OnClose:=@DetachFormClose;
   if w>0 then
     f.Width:=w
   else
     f.Width:=DoScaleX(400);
   if h>0 then
     f.Height:=h
   else
     f.Height:=DoScaleY(350);
   f.Caption:=rsSwitch;
   PageControl1.Parent:=f;
   if (x>0)and(y>0) then begin
     f.Left:=x;
     f.Top:=y;
   end
   else
     FormPos(f,mouse.CursorPos.x,mouse.CursorPos.y);
   f.Show;
  end
  else if PageControl1.Parent is TForm then begin
    TForm(PageControl1.Parent).Close;
  end;
end;

procedure Tf_switch.DetachFormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  config.SetValue('/Switch/PosX',TForm(Sender).Left);
  config.SetValue('/Switch/PosY',TForm(Sender).Top);
  config.SetValue('/Switch/PosW',TForm(Sender).Width);
  config.SetValue('/Switch/PosH',TForm(Sender).Height);
  CloseAction:=caFree;
  PageControl1.Parent:=Panel1;
end;

end.

