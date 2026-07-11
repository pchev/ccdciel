unit pu_csvoption;

{$mode ObjFPC}{$H+}

{
Copyright (C) 2026 Patrick Chevalley

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { Tf_csvoption }

  Tf_csvoption = class(TForm)
    Button1: TButton;
    cbSep: TComboBox;
    cbQuote: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
  private
    function GetSeparator: char;
    function GetQuote: char;
    procedure SetSeparator(value: char);
    procedure SetQuote(value: char);
  public
    property separator: char read GetSeparator write SetSeparator;
    property quote: char read GetQuote write SetQuote;
  end;

var
  f_csvoption: Tf_csvoption;

implementation

{$R *.lfm}

function Tf_csvoption.GetSeparator: char;
begin
  case cbSep.ItemIndex of
     0: result:=';';
     1: result:=',';
     2: result:='|';
     3: result:=chr(9);
     else result:=';';
  end;
end;

function Tf_csvoption.GetQuote: char;
begin
  case cbQuote.ItemIndex of
     0: result:=chr(0);
     1: result:='"';
     2: result:='''';
     else result:=chr(0);
  end;
end;

procedure Tf_csvoption.SetSeparator(value: char);
begin
  if value=';' then cbSep.ItemIndex:=0
  else if value=',' then cbSep.ItemIndex:=1
  else if value='|' then cbSep.ItemIndex:=2
  else if value=chr(9) then cbSep.ItemIndex:=3
  else cbSep.ItemIndex:=0;
end;

procedure Tf_csvoption.SetQuote(value: char);
begin
  if value=chr(0) then cbSep.ItemIndex:=0
  else if value='"' then cbSep.ItemIndex:=1
  else if value='''' then cbSep.ItemIndex:=2
  else cbSep.ItemIndex:=0;
end;

end.

