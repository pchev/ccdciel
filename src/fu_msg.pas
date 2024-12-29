unit fu_msg;

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

uses UScaleDPI, u_translation,
  Classes, SysUtils, FileUtil, Forms, Graphics, Controls, ExtCtrls, StdCtrls, Buttons, Menus;

type

  { Tf_msg }

  Tf_msg = class(TFrame)
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    msg: TMemo;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    SpeedButton1: TSpeedButton;
    Title: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure MenuItemClick(Sender: TObject);
    procedure msgDblClick(Sender: TObject);
  private
    { private declarations }
    FonMsgLevelChange, FonOpenLog: TNotifyEvent;
    function GetMsgLevel: integer;
    procedure SetMsgLevel(value: integer);
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    property MsgLevel: integer read GetMsgLevel write SetMsgLevel;
    property onMsgLevelChange: TNotifyEvent read FonMsgLevelChange write FonMsgLevelChange;
    property onOpenLog: TNotifyEvent read FonOpenLog write FonOpenLog;
  end;

implementation

{$R *.lfm}

constructor Tf_msg.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 {$endif}
 ScaleDPI(Self);
 SetLang;
end;

destructor  Tf_msg.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_msg.Setlang;
begin
 MenuItem1.Caption:=rsSummary;
 MenuItem2.Caption:=rsCommands;
 MenuItem3.Caption:=rsDetails;
end;

procedure Tf_msg.FrameResize(Sender: TObject);
var i,w: integer;
begin
  if Parent is TPanel then begin
    w:=TPanel(Parent).Width-Left;
    for i:=0 to TPanel(Parent).ComponentCount-1 do
       if TPanel(Parent).Components[i] is TFrame then
         w:=w-Tframe(TPanel(Parent).Components[i]).Width;
    Width:=w;
  end;
end;

procedure Tf_msg.Button1Click(Sender: TObject);
begin
  PopupMenu1.PopUp;
end;

procedure Tf_msg.MenuItemClick(Sender: TObject);
begin
  if Assigned(FonMsgLevelChange) then FonMsgLevelChange(Self);
end;

function Tf_msg.GetMsgLevel: integer;
begin
  if MenuItem1.Checked then
    result:=1
  else if MenuItem2.Checked then
    result:=2
  else if MenuItem3.Checked then
    result:=3
  else
    result:=3;
end;

procedure Tf_msg.SetMsgLevel(value: integer);
begin
  case value of
    1: MenuItem1.Checked:=true;
    2: MenuItem2.Checked:=true;
    3: MenuItem3.Checked:=true;
    else MenuItem3.Checked:=true;
  end;
end;

procedure Tf_msg.msgDblClick(Sender: TObject);
begin
   if Assigned(FonOpenLog) then FonOpenLog(Self);
end;

end.

