unit fu_msg;

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
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls;

type

  { Tf_msg }

  Tf_msg = class(TFrame)
    msg: TMemo;
    Panel1: TPanel;
    procedure FrameEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FrameResize(Sender: TObject);
    procedure msgStartDrag(Sender: TObject; var DragObject: TDragObject);
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
  end;

implementation

{$R *.lfm}

procedure Tf_msg.msgStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
    DragObject := TDragObject.Create(self as TControl);
end;

procedure Tf_msg.FrameEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if Target is TPanel then begin
     if TPanel(Target).tag=0 then begin
        msg.Constraints.MaxWidth:=500;
        msg.Constraints.MinWidth:=500;
     end else begin
        msg.Constraints.MaxWidth:=120;
        msg.Constraints.MinWidth:=120;
     end;
  end;
end;

procedure Tf_msg.FrameResize(Sender: TObject);
begin
  if Parent is TPanel then begin
     if TPanel(Parent).tag=0 then begin
        msg.Constraints.MaxWidth:=500;
        msg.Constraints.MinWidth:=500;
     end else begin
        msg.Constraints.MaxWidth:=120;
        msg.Constraints.MinWidth:=120;
     end;
  end;
end;

constructor Tf_msg.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
end;

destructor  Tf_msg.Destroy;
begin
 inherited Destroy;
end;

end.

