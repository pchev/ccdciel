unit fu_mount;

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
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, Graphics;

type

  { Tf_mount }

  Tf_mount = class(TFrame)
    BtnPark: TButton;
    BtnTrack: TButton;
    Pierside: TEdit;
    Label4: TLabel;
    TimeToMeridian: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    LabelMeridian: TLabel;
    RA: TEdit;
    DE: TEdit;
    Panel1: TPanel;
    StaticText1: TStaticText;
    procedure BtnParkClick(Sender: TObject);
    procedure BtnTrackClick(Sender: TObject);
  private
    { private declarations }
    FonPark  : TNotifyEvent;
    FonTrack  : TNotifyEvent;
  public
    { public declarations }
    property onPark  : TNotifyEvent read FonPark write FonPark;
    property onTrack  : TNotifyEvent read FonTrack write FonTrack;
  end;

implementation

{$R *.lfm}

{ Tf_mount }

procedure Tf_mount.BtnParkClick(Sender: TObject);
begin
  if BtnPark.Font.Color=clGreen then begin
     if MessageDlg('Park the telescope now?',mtConfirmation,mbYesNo,0)<>mrYes then exit;
  end;
  if assigned(FonPark) then FonPark(self);
end;

procedure Tf_mount.BtnTrackClick(Sender: TObject);
begin
  if assigned(FonTrack) then FonTrack(self);
end;

end.

