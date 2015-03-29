unit fu_planetarium;

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

type

  { Tf_planetarium }

  Tf_planetarium = class(TFrame)
    BtnConnect: TButton;
    Panel1: TPanel;
    led: TShape;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Status: TEdit;
    StaticText1: TStaticText;
    procedure BtnConnectClick(Sender: TObject);
  private
    { private declarations }
    FonConnect: TNotifyEvent;
  public
    { public declarations }
    property onConnect: TNotifyEvent read FonConnect write FonConnect;
  end;

implementation

{$R *.lfm}

{ Tf_planetarium }

procedure Tf_planetarium.BtnConnectClick(Sender: TObject);
begin
   if Assigned(FonConnect) then FonConnect(self);
end;

end.

