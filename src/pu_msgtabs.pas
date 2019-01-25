unit pu_msgtabs;

{$mode objfpc}{$H+}

{
Copyright (C) 2019 Patrick Chevalley

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

uses  UScaleDPI, u_translation,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls;

type

  { Tf_msgtabs }

  Tf_msgtabs = class(TForm)
    TabControl1: TTabControl;
    procedure FormCreate(Sender: TObject);

  private
    procedure SetLang;
  public
  end;

var
  f_msgtabs: Tf_msgtabs;

implementation

{$R *.lfm}

{ Tf_msgtabs }



procedure Tf_msgtabs.FormCreate(Sender: TObject);
begin
  {$ifdef lclcocoa}
  ClientHeight:=48;
  TabControl1.Height:=44;
  {$endif}
  ScaleDPI(Self);
  Setlang;
end;

procedure Tf_msgtabs.SetLang;
begin
  TabControl1.Tabs[0]:=rsSummary;
  TabControl1.Tabs[1]:=rsCommands;
  TabControl1.Tabs[2]:=rsDetails;
end;

end.

