unit fu_filterwheel;

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

uses  UScaleDPI,
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls;

type

  { Tf_filterwheel }

  Tf_filterwheel = class(TFrame)
    Filters: TComboBox;
    Label1: TLabel;
    Panel1: TPanel;
    StaticText1: TStaticText;
    procedure FiltersChange(Sender: TObject);
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
 ScaleDPI(Self);
end;

destructor  Tf_filterwheel.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_filterwheel.FiltersChange(Sender: TObject);
begin
  if Assigned(FonSetFilter) then FonSetFilter(self);
end;

end.

