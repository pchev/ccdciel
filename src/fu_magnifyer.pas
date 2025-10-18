unit fu_magnifyer;

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

uses  UScaleDPI, u_translation, u_global,
  Classes, SysUtils, FileUtil, Forms, Graphics, Controls, ExtCtrls, StdCtrls;

type

  { Tf_magnifyer }

  Tf_magnifyer = class(TFrame)
    Image1: TImage;
    Panel1: TPanel;
    Title: TLabel;
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    procedure SetTitleColor;
  end;

implementation

{$R *.lfm}

{ Tf_magnifyer }

constructor Tf_magnifyer.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Panel1.ChildSizing.LeftRightSpacing:=8;
 {$endif}
 Image1.Picture.Bitmap.Height:=Image1.Height;
 Image1.Picture.Bitmap.Width:=Image1.Width;
 ScaleDPI(Self);
 SetLang;
end;

destructor  Tf_magnifyer.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_magnifyer.SetLang;
begin
  Title.Caption:=rsMagnifyer;
end;

procedure Tf_magnifyer.SetTitleColor;
begin
  Title.Color:=InterfaceColor[TitleColor,1];
  Title.Font.Color:=InterfaceColor[TitleColor,2];
  Title.Font.Style:=[fsBold];
end;

end.

