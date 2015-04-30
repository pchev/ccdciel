unit pu_planetariuminfo;

{$mode objfpc}{$H+}

interface

uses  cu_planetarium, u_utils,
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
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FPlanetarium: TPlanetarium;
    procedure recvdata(msg:string);
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
  if (planetarium=nil) or (not planetarium.Connected) then
    ModalResult:=mrAbort
  else begin
    planetarium.onReceiveData:=@recvdata;
    recvdata('');
  end;
end;

procedure Tf_planetariuminfo.recvdata(msg:string);
begin
  Ra.Text:=RAToStr(planetarium.RA);
  De.Text:=DEToStr(planetarium.DE);
  Obj.Text:=planetarium.Objname;
end;

end.

