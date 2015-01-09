unit pu_options;

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ExtCtrls;

type

  { Tf_option }

  Tf_option = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CaptureDir: TDirectoryEdit;
    FocusWindow: TEdit;
    Label2: TLabel;
    Logtofile: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label14: TLabel;
    Panel1: TPanel;
    StarWindow: TEdit;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  f_option: Tf_option;

implementation

{$R *.lfm}

end.

