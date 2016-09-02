unit pu_pause;
{
Copyright (C) 2016 Patrick Chevalley

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

{$mode objfpc}{$H+}

interface

uses  u_global, UScaleDPI,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { Tf_pause }

  Tf_pause = class(TForm)
    Button1: TButton;
    Button2: TButton;
    PauseLabel: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FContinue, Fresult : boolean;
    procedure SetText(value:string);
    function GetText: string;
  public
    { public declarations }
    function Wait(timeout:integer=0): boolean;
    property Text: string read GetText write SetText;
  end;

var
  f_pause: Tf_pause;

implementation

{$R *.lfm}

{ Tf_pause }

procedure Tf_pause.SetText(value:string);
begin
  PauseLabel.Caption:=value;
end;

function Tf_pause.GetText: string;
begin
  result:=PauseLabel.Caption;
end;

procedure Tf_pause.FormShow(Sender: TObject);
begin
  FContinue:=false;
  Fresult:=false;
end;

procedure Tf_pause.Button1Click(Sender: TObject);
begin
  Fresult:=true;
  FContinue:=true;
end;

procedure Tf_pause.Button2Click(Sender: TObject);
begin
  Fresult:=false;
  FContinue:=true;
end;

procedure Tf_pause.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=FContinue;
end;

procedure Tf_pause.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self);
end;

function Tf_pause.Wait(timeout:integer=0): boolean;
var endt: TDateTime;
begin
  if timeout>0 then begin
    endt:=now+timeout/secperday;
    PauseLabel.Caption:=PauseLabel.Caption+crlf+'Continue automatically in '+inttostr(timeout)+' seconds.';
  end
  else
    endt:=MaxInt;
  Show;
  while (not FContinue) do begin
    Application.ProcessMessages;
    Sleep(100);
    if now>endt then begin
       Fresult:=true;
       FContinue:=true;
    end;
  end;
  result:=Fresult;
  Close;
end;

end.

