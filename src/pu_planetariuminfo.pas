unit pu_planetariuminfo;

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

uses  cu_planetarium, u_utils, u_global, UScaleDPI, u_translation,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { Tf_planetariuminfo }

  Tf_planetariuminfo = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    PA: TEdit;
    Magn: TEdit;
    Ra: TEdit;
    De: TEdit;
    Obj: TEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    LastMsg: string;
    FPlanetarium: TPlanetarium;
    FNewTarget: TNotifyEvent;
    procedure recvdata(msg:string);
    procedure SetPlanetarium(value: TPlanetarium);
  public
    { public declarations }
    procedure SetLang;
    property planetarium: TPlanetarium read FPlanetarium write SetPlanetarium;
    property onNewTarget: TNotifyEvent read FNewTarget write FNewTarget;
  end;

var
  f_planetariuminfo: Tf_planetariuminfo;

implementation

{$R *.lfm}

{ Tf_planetariuminfo }

procedure Tf_planetariuminfo.FormShow(Sender: TObject);
begin
  if (planetarium=nil) or (not planetarium.Connected) then begin
    ShowMessage(rsPleaseConnec);
    ModalResult:=mrAbort;
  end
  else begin
    LastMsg:='';
    planetarium.onReceiveData:=@recvdata;
    recvdata('');
  end;
  if Assigned(FNewTarget) then begin
    // wait for multiple entries
    Button1.Caption:=rsClose;
    Button2.Visible:=False;
    Label4.Caption:=rsClickTheObje+crlf+'Every click insert a new target';
  end
  else begin
    // keep only last entry, process on exit
    Button1.Caption:=rsOK;
    Button2.Visible:=True;
    Label4.Caption:=rsClickTheObje;
  end;
end;

procedure Tf_planetariuminfo.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self);
  SetLang;
end;

procedure Tf_planetariuminfo.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
   planetarium.onReceiveData:=nil;
   FNewTarget:=nil;
end;

procedure Tf_planetariuminfo.SetLang;
begin
  Caption:=rsPlanetariumP;
  Button1.Caption:=rsOK;
  Button2.Caption:=rsCancel;
  Label1.Caption:=rsCenterRA+j2000;
  Label2.Caption:=rsCenterDec+j2000;
  Label3.Caption:=rsObjectName;
  Label4.Caption:=rsClickTheObje;
  label5.Caption :=rsPA;
  Label6.Caption:=rsMagnitude;
end;

procedure Tf_planetariuminfo.SetPlanetarium(value: TPlanetarium);
begin
  FPlanetarium:=value;
  ModalResult:=mrCancel;
end;

procedure Tf_planetariuminfo.recvdata(msg:string);
var ok: boolean;
begin
 if (planetarium=nil) or (not planetarium.Connected) then begin
   Close;
   Exit;
 end;
 ok:=(msg>'')and(msg<>LastMsg);
 LastMsg:=msg;
 if (planetarium.RA<>NullCoord)and(planetarium.DE<>NullCoord) then begin
  Ra.Text:=RAToStr(planetarium.RA);
  De.Text:=DEToStr(planetarium.DE);
 end
 else
  ok:=false;
 if planetarium.PA<>NullCoord then begin
  PA.Text:=FormatFloat(f2,planetarium.PA);
 end
 else
  PA.Text:='';
 if planetarium.Magnitude<>NullCoord then begin
  Magn.Text:=FormatFloat(f2,planetarium.Magnitude);
 end
 else
  Magn.Text:='';
 if planetarium.Objname<>'' then begin
  Obj.Text:=trim(planetarium.Objname);
 end;
 if ok and Assigned(FNewTarget) then FNewTarget(self);
end;

end.

