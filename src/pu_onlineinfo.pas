unit pu_onlineinfo;

{$mode ObjFPC}{$H+}

interface

uses cu_onlinesearch, u_utils, u_global, u_translation, UScaleDPI, StdCtrls,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { Tf_onlineinfo }

  Tf_onlineinfo = class(TForm)
    BtnSearch: TButton;
    Button2: TButton;
    ButtonOK: TButton;
    De: TEdit;
    MagBand: TLabel;
    Magn: TEdit;
    Label1: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    LabelResolver: TLabel;
    Obj: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Ra: TEdit;
    procedure BtnSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure SetLang;
  end;

var
  f_onlineinfo: Tf_onlineinfo;

implementation

{$R *.lfm}

{ Tf_onlineinfo }

procedure Tf_onlineinfo.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self);
  SetLang;
  LabelResolver.Caption:='';
  MagBand.Caption:='';
end;

procedure Tf_onlineinfo.SetLang;
begin
  Caption:=rsSearchOnline;
  ButtonOK.Caption:=rsOK;
  Button2.Caption:=rsCancel;
  BtnSearch.Caption:=rsSearch;
  Label1.Caption:=rsCenterRA;
  Label6.Caption:=j2000;
  Label2.Caption:=rsCenterDec;
  Label16.Caption:=j2000;
  Label3.Caption:=rsObjectName;
end;

procedure Tf_onlineinfo.BtnSearchClick(Sender: TObject);
var ra0,dec0,mag0 : double;
    objname,sname,sresolv,band0 : string;
    found: boolean;
    p: integer;
begin
  LabelResolver.Caption:='';
  found:=false;
  objname:=uppercase(trim(Obj.Text));
  p:=pos('_',objname);
  if p>0 then objname:=copy(objname,1,p-1);
  // online search
  found:=SearchOnline(objname,sname,sresolv,ra0,dec0,mag0,band0);
  if found then begin
    Ra.Text:=RAToStr(ra0*12/pi);{Add position}
    De.Text:=DEToStr(dec0*180/pi);
    if mag0<>NullCoord then begin
      Magn.Text:=FormatFloat(f2,mag0);
      MagBand.Caption:=band0;
    end
    else begin
      Magn.Text:='';
      MagBand.Caption:='';
    end;
    // do not change the name by other synonym that can be returned by Simbad
    LabelResolver.Caption:=sname+', from '+sresolv;
  end;
  if not found then begin
    Ra.Text:='';
    De.Text:='';
    Magn.Text:='';
    MagBand.Caption:='';
    LabelResolver.Caption:='Not found!';
  end;
end;



end.

