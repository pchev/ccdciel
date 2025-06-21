unit fu_switchpage;

{$mode objfpc}{$H+}

{
Copyright (C) 2023 Patrick Chevalley

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

uses  UScaleDPI, u_global, Graphics, Dialogs, u_translation, cu_switch, SpinEx, indiapi, Clipbrd,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Menus;

type

  { Tf_switchpage }

  Tf_switchpage = class(Tframe)
    BtnSet: TButton;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    ScrollBox1: TScrollBox;
    TimerResize: TTimer;
    procedure BtnSetClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure ScrollBox1Resize(Sender: TObject);
    procedure TimerResizeTimer(Sender: TObject);
  private
    { private declarations }
    FConnected, initialized: boolean;
    FNumSwitch: integer;
    FSwitch: TSwitchList;
    CtrlList: TStringList;
    FonSetSwitch: TNotifyEvent;
    ClipboardText: string;
    MaxLabel,MaxCheck:integer;
    procedure SetConnected(value:boolean);
    procedure SetNumSwitch(value:integer);
    procedure SetSwitch(value:TSwitchList);
    procedure ResizeText(Control: TWinControl);
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    procedure Clear;
    property Connected: boolean read FConnected write SetConnected;
    property NumSwitch: integer read FNumSwitch write SetNumSwitch;
    property Switch: TSwitchList read FSwitch write SetSwitch;
    property onSetSwitch: TNotifyEvent read FonSetSwitch write FonSetSwitch;
  end;

implementation

{$R *.lfm}

{ Tf_switchpage }

constructor Tf_switchpage.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 FConnected:=false;
 FNumSwitch:=0;
 initialized:=false;
 CtrlList:=TStringList.Create;
 ScaleDPI(Self);
 SetLang;
end;

destructor  Tf_switchpage.Destroy;
begin
 Clear;
 CtrlList.Free;
 inherited Destroy;
end;

procedure Tf_switchpage.Clear;
var i: integer;
begin
 for i:=0 to CtrlList.Count-1 do
   CtrlList.Objects[i].Free;
 CtrlList.Clear;
 FNumSwitch:=0;
 SetLength(FSwitch,FNumSwitch);
 initialized:=false;
 BtnSet.Enabled:=false;
 Height:=DoScaleY(80);
end;

procedure Tf_switchpage.SetLang;
begin
  BtnSet.Caption:=rsSet;
end;

procedure Tf_switchpage.SetConnected(value:boolean);
begin
  FConnected:=value;
end;

procedure Tf_switchpage.SetNumSwitch(value:integer);
var i: integer;
begin
  if FNumSwitch<>value then begin
    initialized:=false;
    FNumSwitch:=value;
    SetLength(FSwitch,FNumSwitch);
    for i:=0 to CtrlList.Count-1 do
       CtrlList.Objects[i].Free;
    CtrlList.Clear;
  end;
end;

procedure Tf_switchpage.SetSwitch(value:TSwitchList);
var i:integer;
    c:TControl;
    cb: TCheckBox;
    rb: TRadioButton;
    p: TPanel;
    l: TLabel;
    s: TFloatSpinEditEx;
    buf: string;
    cp: TComponent;
begin
 if initialized then begin
   for i:=0 to FNumSwitch-1 do begin
     FSwitch[i].Checked := value[i].Checked;
     FSwitch[i].Value   := value[i].Value;
     if CtrlList.Objects[i] is TCheckBox then
       TCheckBox(CtrlList.Objects[i]).Checked:=value[i].Checked;
     if CtrlList.Objects[i] is TRadioButton then
       TRadioButton(CtrlList.Objects[i]).Checked:=value[i].Checked;
     if CtrlList.Objects[i] is TPanel then begin
       s:=TFloatSpinEditEx(FindComponent('Switch_'+IntToStr(i)));
       if s<>nil then
         s.Value:=value[i].Value;
     end;
   end;
 end
 else begin
  for i:=0 to FNumSwitch-1 do begin
    FSwitch[i].Name       := value[i].Name;
    FSwitch[i].IndiType   := value[i].IndiType;
    FSwitch[i].IndiGroup  := value[i].IndiGroup;
    FSwitch[i].CanWrite   := value[i].CanWrite;
    FSwitch[i].MultiState := value[i].MultiState;
    FSwitch[i].Min        := value[i].Min;
    FSwitch[i].Max        := value[i].Max;
    FSwitch[i].Step       := value[i].Step;
    FSwitch[i].Checked    := value[i].Checked;
    FSwitch[i].Value      := value[i].Value;
    if FSwitch[i].MultiState then begin
      p:=TPanel.Create(self);
      l:=TLabel.Create(p);
      s:=TFloatSpinEditEx.Create(self);
      p.Name:='Panel_'+IntToStr(i);
      p.Caption:='';
      p.Top:=i*30;
      p.Align:=alTop;
      p.AutoSize:=true;
      p.BevelOuter:=bvNone;
      l.Name:='Label_'+IntToStr(i);
      l.Caption:=FSwitch[i].Name;
      l.Hint:=FSwitch[i].Name;
      l.AnchorSideTop.Control:=s;
      l.AnchorSideTop.Side:=asrCenter;
      l.Parent:=p;
      s.name:='Switch_'+IntToStr(i);
      s.MinValue:=FSwitch[i].Min;
      s.MaxValue:=FSwitch[i].Max;
      s.Increment:=FSwitch[i].Step;
      if FSwitch[i].Step>=1 then s.DecimalPlaces:=0
      else if FSwitch[i].Step>=0.1 then s.DecimalPlaces:=1
      else s.DecimalPlaces:=2;
      s.Hint:=FloatToStr(s.MinValue)+ellipsis+FloatToStr(s.MaxValue);
      s.Enabled:=FSwitch[i].CanWrite;
      s.Value:=FSwitch[i].Value;
      s.Width:=DoScaleX(80);
      s.Constraints.MaxHeight:=DoScaleY(28);
      s.BringToFront;
      s.Align:=alRight;
      s.Parent:=p;
      s.BringToFront;
      p.Parent:=ScrollBox1;
      c:=p;
      CtrlList.AddObject(p.Name,p);
    end
    else if (FSwitch[i].IndiType=INDI_SWITCH)and(FSwitch[i].IndiGroup>=0) then begin
      buf:='RBGroup_'+inttostr(FSwitch[i].IndiGroup);
      cp:=FindComponent(buf);
      if cp=nil then begin
        p:=tpanel.Create(self);
        p.Name:=buf;
        p.Caption:='';
        p.BevelInner:=bvLowered;
        p.BevelOuter:=bvRaised;
        p.Top:=i*30;
        p.Align:=alTop;
        p.AutoSize:=true;
        p.Parent:=ScrollBox1;
      end
      else
        p:=TPanel(cp);
      rb:=TRadioButton.Create(self);
      rb.Name:='RB_'+IntToStr(i);
      rb.Top:=i*30;
      rb.Align:=alTop;
      rb.Caption:=FSwitch[i].Name;
      rb.Hint:=FSwitch[i].Name;
      rb.Enabled:=FSwitch[i].CanWrite;
      rb.Checked:=value[i].Checked;
      rb.Parent:=p;
      c:=p;
      CtrlList.AddObject(rb.Name,rb);
    end
    else begin
      cb:=TCheckBox.Create(self);
      cb.Name:='CB_'+IntToStr(i);
      cb.Top:=i*30;
      cb.Align:=alTop;
      cb.Caption:=FSwitch[i].Name;
      cb.Hint:=FSwitch[i].Name;
      cb.Enabled:=FSwitch[i].CanWrite;
      cb.Checked:=value[i].Checked;
      cb.Parent:=ScrollBox1;
      c:=cb;
      CtrlList.AddObject(cb.Name,cb);
    end;
  end;
  // try to adjust the size, limited by Constraints
  if FNumSwitch=0 then
    Height:=panel1.Height
  else
    Height:=Panel1.Height+c.Top+c.Height+8;
  initialized:=true;
  BtnSet.Enabled:=true;
 end;
 TimerResize.Enabled:=False;
 TimerResize.Enabled:=True;
end;

procedure Tf_switchpage.BtnSetClick(Sender: TObject);
var i:integer;
    s: TFloatSpinEditEx;
    swchanged: boolean;
begin
  if (not initialized)or(FNumSwitch=0) then exit;
  swchanged:=false;
  for i:=0 to FNumSwitch-1 do begin
    if FSwitch[i].CanWrite then begin
      if CtrlList.Objects[i] is TCheckBox then begin
        swchanged:=swchanged or (TCheckBox(CtrlList.Objects[i]).Checked<>FSwitch[i].Checked);
        FSwitch[i].Checked:=TCheckBox(CtrlList.Objects[i]).Checked;
      end;
      if CtrlList.Objects[i] is TRadioButton then begin
        swchanged:=swchanged or (TRadioButton(CtrlList.Objects[i]).Checked<>FSwitch[i].Checked);
        FSwitch[i].Checked:=TRadioButton(CtrlList.Objects[i]).Checked;
      end;
      if CtrlList.Objects[i] is TPanel then begin
        s:=TFloatSpinEditEx(FindComponent('Switch_'+IntToStr(i)));
        if s<>nil then begin
          swchanged:=swchanged or (s.Value<>FSwitch[i].Value);
          FSwitch[i].Value:=s.Value;
        end;
      end;
    end;
  end;
  if swchanged and (Assigned(FonSetSwitch)) then FonSetSwitch(self);
end;

procedure Tf_switchpage.MenuItem1Click(Sender: TObject);
begin
  if ClipboardText<>'' then
    Clipboard.AsText := ClipboardText;
end;

procedure Tf_switchpage.PopupMenu1Popup(Sender: TObject);
var p:TPoint;
    c: TControl;
    buf: string;
    m: TMenuItem;
begin
  PopupMenu1.Items.Clear;
  p:=mouse.CursorPos;
  p:=ScreenToClient(p);
  c:=ControlAtPos(p,[capfAllowWinControls,capfAllowDisabled,capfRecursive,capfOnlyClientAreas]);
  if c<>nil then begin
    buf:='';
    if c is TCheckBox then
      buf:=TCheckBox(c).Hint;
    if c is TRadioButton then
      buf:=TRadioButton(c).Hint;
    if c is TLabel then
      buf:=TLabel(c).Hint;
    if buf<>'' then begin
       m:=TMenuItem.Create(self);
       m.Caption:=Format(rsCopyToClipbo, [buf]);
       m.OnClick:=@MenuItem1Click;
       PopupMenu1.Items.Add(m);
       ClipboardText:=buf;
    end
    else begin
      ClipboardText:='';
    end;
  end;

end;

procedure Tf_switchpage.ScrollBox1Resize(Sender: TObject);
begin
  TimerResize.Enabled:=False;
  TimerResize.Enabled:=True;
end;

procedure Tf_switchpage.ResizeText(Control: TWinControl);
var i: integer;
begin
  for i:=0 to Control.ControlCount-1 do begin
    if (Control.Controls[i] is TLabel) and (Control.Controls[i].Hint<>'') then
        Control.Controls[i].Caption:=Copy(Control.Controls[i].Hint,1,MaxLabel)
    else if (Control.Controls[i] is TRadioButton) and (Control.Controls[i].Hint<>'') then
        Control.Controls[i].Caption:=Copy(Control.Controls[i].Hint,1,MaxCheck)
    else if (Control.Controls[i] is TCheckBox) and (Control.Controls[i].Hint<>'') then
        Control.Controls[i].Caption:=Copy(Control.Controls[i].Hint,1,MaxCheck)
    else if Control.Controls[i] is TWinControl then
        ResizeText(TWinControl(Control.Controls[i]));
  end;
end;

procedure Tf_switchpage.TimerResizeTimer(Sender: TObject);
var n:integer;
begin
  TimerResize.Enabled:=False;
  n:=Canvas.TextExtent('a').cx;
  MaxLabel:=max(5,(ScrollBox1.ClientWidth-DoScaleX(80)) div n);
  MaxCheck:=max(5,(ScrollBox1.ClientWidth-DoScaleX(20)) div n);
  ResizeText(ScrollBox1);
end;

end.

