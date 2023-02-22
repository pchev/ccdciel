unit fu_finder;

{$mode objfpc}{$H+}

{
Copyright (C) 2023 Patrick Chevalley

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

uses  UScaleDPI, u_global, Graphics, Dialogs, u_translation, cu_mount, cu_camera,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

type

  { Tf_finder }

  Tf_finder = class(TFrame)
    Button1: TButton;
    Panel1: TPanel;
    Title: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
    FMount: T_mount;
    FCamera: T_camera;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    property Mount: T_mount read FMount write FMount;
    property Camera: T_camera read FCamera write FCamera;
  end;

implementation

{$R *.lfm}

{ Tf_finder }

procedure Tf_finder.Button1Click(Sender: TObject);
begin
  camera.StartExposure(2.0);
end;

constructor Tf_finder.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 Panel1.ChildSizing.LeftRightSpacing:=8;
 Panel1.ChildSizing.VerticalSpacing:=4;
 {$endif}
 ScaleDPI(Self);
 SetLang;
end;

destructor  Tf_finder.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_finder.SetLang;
begin
  Title.Caption:=rsFinderCamera;
end;

end.

