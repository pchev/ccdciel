unit pu_findercalibration;

{$mode ObjFPC}{$H+}

interface

uses u_utils, u_global, u_annotation,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { Tf_findercalibration }

  Tf_findercalibration = class(TForm)
    BtnSearch: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    edDe: TEdit;
    Label1: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    edRa: TEdit;
    Obj: TEdit;
    Panel1: TPanel;
    PanelTop: TPanel;
    PanelBottom: TPanel;
    procedure BtnSearchClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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

procedure Tf_findercalibration.BtnSearchClick(Sender: TObject);
var ra0,dec0,length0,width0,pa : double;
    objname : string;
    found: boolean;
    p: integer;
begin
  found:=false;
  objname:=uppercase(trim(Obj.Text));
  p:=pos('_',objname);
  if p>0 then objname:=copy(objname,1,p-1);
  if length(objname)>1 then {Object name length should be two or longer}
  begin
    load_deep;{Load the deepsky database once. If already loaded, no action}
    linepos:=2;{Set pointer to the beginning}
    repeat
      read_deepsky('T' {full database search} ,0 {ra},0 {dec},1 {cos(telescope_dec)},2*pi{fov},{var} ra0,dec0,length0,width0,pa);{Deepsky database search}
      if ((objname=uppercase(naam2)) or (objname=uppercase(naam3)) or (objname=uppercase(naam4))) then
      begin
        edRa.Text:=RAToStr(ra0*12/pi);{Add position}
        edDe.Text:=DEToStr(dec0*180/pi);
        if naam3='' then
           Obj.Text:=naam2 {Add one object name only}
        else
           Obj.Text:=naam2+'_'+naam3; {Add two object names}
        linepos:=$FFFFFF; {Stop searching}
        found:=true;
     end;
    until linepos>=$FFFFFF;{Found object or end of database}
    if not found then begin
      edRa.Text:='';
      edDe.Text:='';
    end;
  end;
end;

procedure Tf_findercalibration.Button3Click(Sender: TObject);
begin
 if (WCScenterRA<>NullCoord)and(WCScenterDEC<>NullCoord) then begin
   edRa.Text:=RAToStr(WCScenterRA/15);
   edDe.Text:=DEToStr(WCScenterDEC);
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

