unit pu_findercalibration;

{$mode ObjFPC}{$H+}

interface

uses u_utils, u_global,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { Tf_findercalibration }

  Tf_findercalibration = class(TForm)
    Button1: TButton;
    Button2: TButton;
    edDe: TEdit;
    Label1: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    edRa: TEdit;
  private
    procedure SetRA(value: double);
    procedure SetDE(value: double);
    function  GetRA: double;
    function  GetDE: double;
  public
    property RA: double read GetRa write SetRA;
    property DE: double read GetDE write SetDE;
  end;

var
  f_findercalibration: Tf_findercalibration;

implementation

{$R *.lfm}

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

