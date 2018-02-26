unit pu_planetariuminfo;

{$mode objfpc}{$H+}

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
    Ra: TEdit;
    De: TEdit;
    Obj: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FPlanetarium: TPlanetarium;
    procedure recvdata(msg:string);
    procedure SetLang;
  public
    { public declarations }
    property planetarium: TPlanetarium read FPlanetarium write FPlanetarium;
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
    planetarium.onReceiveData:=@recvdata;
    recvdata('');
  end;
end;

procedure Tf_planetariuminfo.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self);
  SetLang;
end;

procedure Tf_planetariuminfo.SetLang;
begin
  Caption:=rsPlanetariumP;
  Button1.Caption:=rsOK;
  Button2.Caption:=rsCancel;
  Label1.Caption:=rsCenterRA;
  Label2.Caption:=rsCenterDec;
  Label3.Caption:=rsObjectName;
  Label4.Caption:=rsClickTheObje;
end;

procedure Tf_planetariuminfo.recvdata(msg:string);
begin
 if (planetarium.RA<>NullCoord)and(planetarium.DE<>NullCoord) then begin
  Ra.Text:=RAToStr(planetarium.RA);
  De.Text:=DEToStr(planetarium.DE);
 end;
 if planetarium.Objname<>'' then begin
  Obj.Text:=trim(planetarium.Objname);
 end;
end;

end.

