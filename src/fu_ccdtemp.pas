unit fu_ccdtemp;

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

  { Tf_ccdtemp }

  Tf_ccdtemp = class(TFrame)
    Button1: TButton;
    Current: TEdit;
    Setpoint: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    StaticText1: TStaticText;
    procedure Button1Click(Sender: TObject);
    procedure StaticText1StartDrag(Sender: TObject; var DragObject: TDragObject
      );
  private
    { private declarations }
    FonSetTemperature: TNotifyEvent;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    property onSetTemperature: TNotifyEvent read FonSetTemperature write FonSetTemperature;
  end;

implementation

{$R *.lfm}

{ Tf_ccdtemp }

constructor Tf_ccdtemp.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
end;

destructor  Tf_ccdtemp.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_ccdtemp.Button1Click(Sender: TObject);
begin
  if Assigned(FonSetTemperature) then FonSetTemperature(self);
end;

procedure Tf_ccdtemp.StaticText1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  DragObject := TDragObject.Create(self as TControl);
end;

end.

