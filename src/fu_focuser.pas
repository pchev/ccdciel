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

uses  UScaleDPI,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

type

  { Tf_focuser }

  Tf_focuser = class(TFrame)
    BtnDown: TButton;
    BtnUp: TButton;
    Label6: TLabel;
    Notebook1: TNotebook;
    PageTimerMove: TPage;
    PageRelPos: TPage;
    PageAbsPos: TPage;
    PanelRelPos: TPanel;
    PanelParam: TPanel;
    RelIncr: TComboBox;
    speed: TEdit;
    timer: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PanelAbsPos: TPanel;
    PanelTimerMove: TPanel;
    PosIncr: TComboBox;
    PanelBtn: TPanel;
    Position: TEdit;
    Panel1: TPanel;
    StaticText1: TStaticText;
    procedure BtnDownClick(Sender: TObject);
    procedure BtnUpClick(Sender: TObject);
  private
    { private declarations }
    FonFocusIN, FonFocusOUT: TNotifyEvent;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    property onFocusIN: TNotifyEvent read FonFocusIN write FonFocusIN;
    property onFocusOUT: TNotifyEvent read FonFocusOUT write FonFocusOUT;
  end;

implementation

{$R *.lfm}

{ Tf_focuser }

constructor Tf_focuser.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 ScaleDPI(Self);
end;

destructor  Tf_focuser.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_focuser.BtnDownClick(Sender: TObject);
begin
  if Assigned(FonFocusIN) then FonFocusIN(self);
end;

procedure Tf_focuser.BtnUpClick(Sender: TObject);
begin
  if Assigned(FonFocusOUT) then FonFocusOUT(self);
end;

end.

