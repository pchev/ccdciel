unit pu_compute;

{$mode objfpc}{$H+}
{
Copyright (C) 2020 Patrick Chevalley

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

uses u_global, u_translation,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, StdCtrls;

type

  { Tf_compute }

  Tf_compute = class(TForm)
    Button1: TButton;
    Button2: TButton;
    BtnComputeImgScale: TButton;
    ImgScaleFocal: TEdit;
    ImgScalePx: TEdit;
    ImgScale: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    ImageScale: TTabSheet;
    procedure BtnComputeImgScaleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure Setlang;
  public

  end;

var
  f_compute: Tf_compute;

implementation

{$R *.lfm}

{ Tf_compute }

procedure Tf_compute.BtnComputeImgScaleClick(Sender: TObject);
var f,p,s: double;
begin
  f:=StrToFloatDef(ImgScaleFocal.Text,-1);
  p:=StrToFloatDef(ImgScalePx.Text,-1);
  if (f>0)and(p>0) then begin
    s:=p/f/1000;
    s:=3600*rad2deg*arctan(s);
    ImgScale.Text:=FormatFloat(f2,s);
  end;
end;

procedure Tf_compute.FormCreate(Sender: TObject);
begin
  Setlang;
end;

procedure Tf_compute.Setlang;
begin
  Caption:=rsCompute;
  Button1.Caption:=rsOK;
  Button2.Caption:=rsCancel;
  Label1.Caption:=rsFocaleLength;
  Label2.Caption:=rsPixelSize;
  BtnComputeImgScale.Caption:=rsCompute;
end;

end.

