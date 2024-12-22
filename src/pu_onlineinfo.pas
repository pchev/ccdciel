unit pu_onlineinfo;

{$mode ObjFPC}{$H+}

interface

uses cu_onlinesearch, u_utils, u_global, u_translation, UScaleDPI, StdCtrls, LCLType,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { Tf_onlineinfo }

  Tf_onlineinfo = class(TForm)
    BtnSearch: TButton;
    Button2: TButton;
    ButtonOK: TButton;
    cbMagBand: TComboBox;
    De: TEdit;
    Label5: TLabel;
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
    procedure cbMagBandChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ObjKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    maglist: TMagnitudeList;
    function GetMagnitude: string;
    procedure SetMagnitude(value:string);
  public
    procedure SetLang;
    property magnitude: string read GetMagnitude write SetMagnitude;
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
  label4.Caption:=rsMagnitude;
end;

procedure Tf_onlineinfo.ObjKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key=VK_RETURN then BtnSearchClick(nil);
end;

procedure Tf_onlineinfo.BtnSearchClick(Sender: TObject);
var ra0,dec0 : double;
    objname,sname,sresolv,band : string;
    found: boolean;
    i,p: integer;
begin
  LabelResolver.Caption:='';
  found:=false;
  objname:=uppercase(trim(Obj.Text));
  p:=pos('_',objname);
  if p>0 then objname:=copy(objname,1,p-1);
  // online search
  found:=SearchOnline(objname,sname,sresolv,ra0,dec0,maglist);
  if found then begin
    Ra.Text:=RAToStr(ra0*12/pi);{Add position}
    De.Text:=DEToStr(dec0*180/pi);
    if length(maglist)>0 then begin
      band:=cbMagBand.Text;
      cbMagBand.Clear;
      for i:=0 to length(maglist)-1 do begin
        cbMagBand.Items.Add(maglist[i].band);
      end;
      i:=cbMagBand.Items.IndexOf(band);
      if i>0 then
        cbMagBand.ItemIndex:=i
      else
        cbMagBand.ItemIndex:=0;
      cbMagBandChange(nil);
    end
    else begin
      Magn.Text:='no magnitude found';
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


procedure Tf_onlineinfo.cbMagBandChange(Sender: TObject);
var i:integer;
begin
  if length(maglist)>0 then begin
    i:=cbMagBand.ItemIndex;
    if (i>=0)and(i<length(maglist)) then begin
      Magn.Text:=FormatFloat(f2,maglist[i].magn);
      MagBand.Caption:=maglist[i].band;
    end;
  end
  else begin
    Magn.Text:='no magnitude found';
    MagBand.Caption:='';
  end;
end;

function Tf_onlineinfo.GetMagnitude: string;
var x: double;
    n: integer;
begin
  val(Magn.Text,x,n);
  if n=0 then
    result:=Magn.Text
  else
    result:='';
end;

procedure Tf_onlineinfo.SetMagnitude(value:string);
begin
  Magn.Text:=value;
end;

end.

