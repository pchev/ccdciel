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

uses  UScaleDPI, u_global, Graphics, Dialogs, u_translation, cu_switch,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

type

  { Tf_switch }

  Tf_switch = class(TFrame)
    SwitchGroup: TCheckGroup;
    ScrollBox1: TScrollBox;
    Title: TLabel;
  private
    { private declarations }
    FConnected: boolean;
    FNumSwitch: integer;
    FSwitch: TSwitchList;
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
 ScaleDPI(Self);
 SetLang;
end;

destructor  Tf_switch.Destroy;
begin
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
begin
  FNumSwitch:=value;
  SetLength(FSwitch,FNumSwitch);
end;

procedure Tf_switch.SetSwitch(value:TSwitchList);
var i:integer;
begin
  SwitchGroup.Items.Clear;
  for i:=0 to FNumSwitch-1 do begin
    FSwitch[i]:=value[i];
    SwitchGroup.Items.Add('Switch-'+IntToStr(i+1));
    SwitchGroup.Checked[i]:=FSwitch[i];
  end;
end;

end.

