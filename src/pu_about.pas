unit pu_about;

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

uses u_translation, UScaleDPI, u_global,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, ExtCtrls;

type

  { Tf_about }

  Tf_about = class(TForm)
    BtnClose: TButton;
    MemoGPL: TMemo;
    MemoAbout: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    procedure SetLang;
  end;

var
  f_about: Tf_about;

implementation

{$R *.lfm}

{ Tf_about }

procedure Tf_about.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self);
  SetLang;
end;

procedure Tf_about.FormShow(Sender: TObject);
begin
  PageControl1.ActivePageIndex:=0;
end;

procedure Tf_about.SetLang;
var aboutmsg: string;
begin
  caption:=rsAbout+' CCDciel';
  BtnClose.Caption:=rsClose;
  TabSheet1.Caption:=rsAbout;
  TabSheet2.Caption:=rsLicenseAgree;
  aboutmsg:='CCDciel '+crlf;
  aboutmsg:=aboutmsg+ccdciel_version+'-'+RevisionStr+blank+compile_time+crlf;
  aboutmsg:=aboutmsg+rsCompiledWith+':'+crlf;
  aboutmsg:=aboutmsg+blank+compile_version+crlf+crlf;
  aboutmsg:=aboutmsg+'Credits:'+crlf;
  aboutmsg:=aboutmsg+tab+'Patrick Chevalley'+crlf;
  aboutmsg:=aboutmsg+tab+'Han Kleijn'+crlf;
  aboutmsg:=aboutmsg+tab+'Chris Konvalin'+crlf+crlf;
  aboutmsg:=aboutmsg+'Copyright (C) '+cdate+' Patrick Chevalley pch@ap-i.net'+crlf;
  aboutmsg:=aboutmsg+'http://www.ap-i.net'+crlf;
  aboutmsg:=aboutmsg+'This program is free software; you can redistribute it and/or'+crlf;
  aboutmsg:=aboutmsg+'modify it under the terms of the GNU General Public License'+crlf;
  aboutmsg:=aboutmsg+'as published by the Free Software Foundation; either version 3'+crlf;
  aboutmsg:=aboutmsg+'of the License, or (at your option) any later version.'+crlf+crlf;
  aboutmsg:=aboutmsg+'This program is distributed in the hope that it will be useful,'+crlf;
  aboutmsg:=aboutmsg+'but WITHOUT ANY WARRANTY; without even the implied warranty of'+crlf;
  aboutmsg:=aboutmsg+'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the'+crlf;
  aboutmsg:=aboutmsg+'GNU General Public License for more details.'+crlf;
  MemoAbout.Text:=aboutmsg;
end;

end.

