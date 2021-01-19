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

uses  UScaleDPI, u_global, Graphics, Dialogs, u_translation, cu_switch, Spin,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

type

  { Tf_switch }

  Tf_switch = class(TFrame)
    ScrollBox1: TScrollBox;
    Title: TLabel;
  private
    { private declarations }
    FConnected, initialized: boolean;
    FNumSwitch: integer;
    FSwitch: TSwitchList;
    CtrlList: TStringList;
    procedure SetConnected(value:boolean);
    procedure SetNumSwitch(value:integer);
    procedure SetSwitch(value:TSwitchList);
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    property Connected: boolean read FConnected write SetConnected;
    property NumSwitch: integer read FNumSwitch write SetNumSwitch;
    property Switch: TSwitchList read FSwitch write SetSwitch;
  end;

implementation

{$R *.lfm}

{ Tf_switch }

constructor Tf_switch.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 {$endif}
 FConnected:=false;
 FNumSwitch:=0;
 initialized:=false;
 CtrlList:=TStringList.Create;
 ScaleDPI(Self);
 SetLang;
end;

destructor  Tf_switch.Destroy;
var i: integer;
begin
 for i:=0 to CtrlList.Count-1 do
   CtrlList.Objects[i].Free;
 CtrlList.Free;
 inherited Destroy;
end;

procedure Tf_switch.SetLang;
begin
  Title.Caption:=rsSwitch;
end;

procedure Tf_switch.SetConnected(value:boolean);
begin
  FConnected:=value;
end;

procedure Tf_switch.SetNumSwitch(value:integer);
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

procedure Tf_switch.SetSwitch(value:TSwitchList);
var i:integer;
    cb: TCheckBox;
    p: TPanel;
    l: TLabel;
    s: TFloatSpinEdit;
begin
 if initialized then begin
   for i:=0 to FNumSwitch-1 do begin
     if CtrlList.Objects[i] is TCheckBox then
       TCheckBox(CtrlList.Objects[i]).Checked:=value[i].Checked;
     if CtrlList.Objects[i] is TPanel then begin
       s:=TFloatSpinEdit(FindComponent('Switch_'+IntToStr(i)));
       if s<>nil then
         s.Value:=value[i].Value;
     end;
   end;
 end
 else begin
  for i:=0 to FNumSwitch-1 do begin
    FSwitch[i].Name       := value[i].Name;
    FSwitch[i].CanWrite   := value[i].CanWrite;
    FSwitch[i].MultiState := value[i].MultiState;
    FSwitch[i].Min        := value[i].Min;
    FSwitch[i].Max        := value[i].Max;
    FSwitch[i].Step       := value[i].Step;
    FSwitch[i].Checked    := value[i].Checked;
    FSwitch[i].Value      := value[i].Value;
    if FSwitch[i].MultiState then begin
      p:=TPanel.Create(self);
      l:=TLabel.Create(self);
      s:=TFloatSpinEdit.Create(self);
      p.Name:='Panel_'+IntToStr(i);
      p.Caption:='';
      p.Top:=i*30;
      p.Align:=alTop;
      p.AutoSize:=true;
      p.BevelOuter:=bvNone;
      l.Name:='Label_'+IntToStr(i);
      l.Caption:=FSwitch[i].Name;
      l.Align:=alLeft;
      l.Parent:=p;
      s.name:='Switch_'+IntToStr(i);
      s.MinValue:=FSwitch[i].Min;
      s.MaxValue:=FSwitch[i].Max;
      s.Increment:=FSwitch[i].Step;
      s.Enabled:=FSwitch[i].CanWrite;
      s.Value:=FSwitch[i].Value;
      s.Width:=80;
      s.Align:=alRight;
      s.Parent:=p;
      p.Parent:=ScrollBox1;
      CtrlList.AddObject(p.Name,p);
    end
    else begin
      cb:=TCheckBox.Create(self);
      cb.Name:='CB_'+IntToStr(i);
      cb.Top:=i*30;
      cb.Align:=alTop;
      cb.Caption:=FSwitch[i].Name;
      cb.Enabled:=FSwitch[i].CanWrite;
      cb.Checked:=value[i].Checked;
      cb.Parent:=ScrollBox1;
      CtrlList.AddObject(cb.Name,cb);
    end;
  end;
  initialized:=true;
 end;
end;

end.

