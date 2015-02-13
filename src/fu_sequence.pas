unit fu_sequence;

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Grids;

type

  { Tf_sequence }

  Tf_sequence = class(TFrame)
    Panel1: TPanel;
    Panel2: TPanel;
    StaticText1: TStaticText;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
  end;

var
  f_sequence: Tf_sequence;

implementation

{$R *.lfm}

constructor Tf_sequence.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 StringGrid1.Cells[0,0]:='Object';
 StringGrid1.Cells[1,0]:='Plan';
 StringGrid1.Cells[2,0]:='Start';
 StringGrid1.Cells[3,0]:='End';
 StringGrid1.Cells[4,0]:='RA';
 StringGrid1.Cells[5,0]:='DEC';
 StringGrid2.Cells[0,0]:='Exp.';
 StringGrid2.Cells[1,0]:='Count';
 StringGrid2.Cells[2,0]:='Filter';
 StringGrid2.Cells[3,0]:='Type';
end;

destructor  Tf_sequence.Destroy;
begin
 inherited Destroy;
end;

end.

