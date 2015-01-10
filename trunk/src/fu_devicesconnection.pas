unit fu_devicesconnection;

{$mode objfpc}{$H+}

{
Copyright (C) 2015 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

type

  { Tf_devicesconnection }

  Tf_devicesconnection = class(TFrame)
    BtnConnect: TButton;
    LabelCamera: TLabel;
    LabelWheel: TLabel;
    LabelFocuser: TLabel;
    LabelMount: TLabel;
    Panel1: TPanel;
    led: TShape;
    PanelDev: TPanel;
    StaticText1: TStaticText;
    procedure BtnConnectClick(Sender: TObject);
  private
    { private declarations }
    FonConnect: TNotifyEvent;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    property onConnect: TNotifyEvent read FonConnect write FonConnect;
  end;

implementation

{$R *.lfm}

{ Tf_devicesconnection }

constructor Tf_devicesconnection.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
end;

destructor  Tf_devicesconnection.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_devicesconnection.BtnConnectClick(Sender: TObject);
begin
  if Assigned(FonConnect) then FonConnect(self);
end;

end.

