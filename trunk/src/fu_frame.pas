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

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

type

  { Tf_frame }

  Tf_frame = class(TFrame)
    BtnSet: TButton;
    BtnReset: TButton;
    FX: TEdit;
    FY: TEdit;
    FWidth: TEdit;
    FHeight: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    StaticText1: TStaticText;
    procedure BtnResetClick(Sender: TObject);
    procedure BtnSetClick(Sender: TObject);
  private
    { private declarations }
    FonSet, FonReset: TNotifyEvent;
  public
    { public declarations }
    property onSet: TNotifyEvent read FonSet write FonSet;
    property onReset: TNotifyEvent read FonReset write FonReset;
  end;

implementation

{$R *.lfm}

{ Tf_frame }

procedure Tf_frame.BtnSetClick(Sender: TObject);
begin
  if Assigned(FonSet) then FonSet(self);
end;

procedure Tf_frame.BtnResetClick(Sender: TObject);
begin
  if Assigned(FonReset) then FonReset(self);
end;

end.

