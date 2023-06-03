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

uses  UScaleDPI, u_global, Graphics, Dialogs, u_translation, cu_switch, fu_switchpage,
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
  public
    { public declarations }
    SwitchPage: array[0..MaxSwitches] of integer;
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
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
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 {$endif}
 for i:=0 to MaxSwitches do begin
   FConnected[i]:=false;
   SwitchPage[i]:=-1;
 end;
 ScaleDPI(Self);
 SetLang;
end;

destructor  Tf_switch.Destroy;
begin
 Clear;
 inherited Destroy;
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
 sp.NumSwitch:=sw.NumSwitch;
 sp.Switch:=sw.Switch;
 sp.onSetSwitch:=FonSetSwitch;
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
  PageControl1.ShowTabs:=(PageControl1.PageCount>1)or(sw.DeviceName<>'');
end;

end.

