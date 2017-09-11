unit fu_msg;

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

uses UScaleDPI,
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls;

type

  { Tf_msg }

  Tf_msg = class(TFrame)
    msg: TMemo;
    Panel1: TPanel;
    procedure FrameResize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
  end;

implementation

{$R *.lfm}

procedure Tf_msg.FrameResize(Sender: TObject);
var i,w: integer;
begin
  if Parent is TPanel then begin
    w:=TPanel(Parent).Width-Left;
    for i:=0 to TPanel(Parent).ComponentCount-1 do
       if TPanel(Parent).Components[i] is TFrame then
         w:=w-Tframe(TPanel(Parent).Components[i]).Width;
    Width:=w;
  end;
end;

constructor Tf_msg.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 ScaleDPI(Self);
end;

destructor  Tf_msg.Destroy;
begin
 inherited Destroy;
end;

end.

