unit pu_findercalibration;

{$mode ObjFPC}{$H+}

interface

uses u_utils, u_global, u_annotation, UScaleDPI, u_translation, Clipbrd,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { Tf_findercalibration }

  Tf_findercalibration = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ButtonPast: TButton;
    edDe: TEdit;
    Label1: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    edRa: TEdit;
    Panel1: TPanel;
    PanelTop: TPanel;
    PanelBottom: TPanel;
    procedure ButtonPastClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetRA(value: double);
    procedure SetDE(value: double);
    function  GetRA: double;
    function  GetDE: double;
  public
    procedure SetLang;
    property RA: double read GetRa write SetRA;
    property DE: double read GetDE write SetDE;
  end;

var
  f_findercalibration: Tf_findercalibration;

implementation

{$R *.lfm}

procedure Tf_findercalibration.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self);
  SetLang;
end;

procedure Tf_findercalibration.SetLang;
begin
  Caption:=rsCalibration;
  label3.Caption:=rsThisProcedur2+crlf+rsEnterTheJ200;
  label1.Caption:=rsCenterRA;
  label2.Caption:=rsCenterDec;
  ButtonPast.Caption:=rsPastFromClip;
  button1.Caption:=rsOK;
  button2.Caption:=rsCancel;
end;

procedure Tf_findercalibration.ButtonPastClick(Sender: TObject);
var buf: string;
    cra,cde: double;
begin
  buf := Clipboard.AsText;
  Str2RaDec(buf,cra,cde);
  if (cra>-9999) and (cde>-9999) then begin
    edRa.Text:=RAToStr(cra);
    edDe.Text:=DEToStr(cde);
  end
  else begin
    ShowMessage(Format(rsCannotInterp, [buf]));
  end;
end;

procedure Tf_findercalibration.SetRA(value: double);
begin
  edRa.Text:=RAToStr(value);
end;

procedure Tf_findercalibration.SetDE(value: double);
begin
  edDe.Text:=DEToStr(value);
end;

function  Tf_findercalibration.GetRA: double;
begin
  if edRa.Text='' then
    result:=NullCoord
  else
    result:=StrToAR(edRa.Text);
end;

function  Tf_findercalibration.GetDE: double;
begin
  if edDe.Text='' then
    result:=NullCoord
  else
    result:=StrToDE(edDe.Text);
end;

end.

