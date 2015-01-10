unit fu_filterwheel;

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

  { Tf_filterwheel }

  Tf_filterwheel = class(TFrame)
    BtnSetFilter: TButton;
    Filters: TComboBox;
    Panel1: TPanel;
    StaticText1: TStaticText;
    procedure BtnSetFilterClick(Sender: TObject);
  private
    { private declarations }
    FonSetFilter: TNotifyEvent;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    property onSetFilter: TNotifyEvent read FonSetFilter write FonSetFilter;
  end;

implementation

{$R *.lfm}

{ Tf_filterwheel }

constructor Tf_filterwheel.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
end;

destructor  Tf_filterwheel.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_filterwheel.BtnSetFilterClick(Sender: TObject);
begin
  if Assigned(FonSetFilter) then FonSetFilter(self);
end;

end.

