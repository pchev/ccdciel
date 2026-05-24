unit pu_viewtext;

{$mode objfpc}{$H+}

{
Copyright (C) 2019 Patrick Chevalley

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

uses  UScaleDPI, u_translation, StrUtils, LazUTF8, LCLType,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons;

type

  { Tf_viewtext }

  Tf_viewtext = class(TForm)
    ActionButton: TSpeedButton;
    btnClose: TButton;
    btnSearch: TButton;
    edSearch: TEdit;
    Memo1: TMemo;
    Panel1: TPanel;
    PanelSearch: TPanel;
    procedure btnCloseClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure edSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    searchtext,searchbuf: string;
    searchpos: integer;
  public
    { public declarations }
  end;

var
  f_viewtext: Tf_viewtext;

implementation

{$R *.lfm}

{ Tf_viewtext }

procedure Tf_viewtext.FormCreate(Sender: TObject);
begin
  {$ifdef linux}
  Memo1.Font.Name:='Monospace';
  {$endif}
  ScaleDPI(Self);
  btnClose.Caption:=rsClose;
  btnSearch.Caption:=rsSearch;
  searchpos:=1;
  searchtext:='';
  searchbuf:='';
end;

procedure Tf_viewtext.FormShow(Sender: TObject);
begin
  PanelSearch.Visible:=(width>DoScaleX(500));
end;

procedure Tf_viewtext.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_viewtext.btnSearchClick(Sender: TObject);
var i:integer;
    txt: string;
begin
  btnSearch.Caption:=rsSearch;
  if trim(edSearch.text)='' then exit;
  if searchbuf='' then searchbuf:=uppercase(memo1.Text);
  txt:=UpperCase(edSearch.text);
  if txt<>searchtext then begin
    searchpos:=1;
    searchtext:=txt;
  end;
  i:=PosEx(searchtext,searchbuf,searchpos);
  if i>0 then begin
    memo1.SelStart:=UTF8Length(PChar(searchbuf), i-1);
    memo1.SelLength:=UTF8Length(PChar(searchtext),length(searchtext));
    searchpos:=i+1;
    btnSearch.Caption:=rsNext;
  end
  else begin
    searchpos:=1;
    btnSearch.Caption:=rsSearch;
  end;
end;

procedure Tf_viewtext.edSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  btnSearch.Caption:=rsSearch;
  if key=VK_RETURN then btnSearchClick(Sender);
end;

procedure Tf_viewtext.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

end.

