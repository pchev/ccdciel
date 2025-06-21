unit pu_goto;

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

uses cu_planetarium, u_utils, u_global, cu_fits, UScaleDPI, u_translation, u_annotation, LCLType, pu_compute,
  cu_onlinesearch, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { Tf_goto }

  Tf_goto = class(TForm)
    BtnCompute: TButton;
    BtnSearch: TButton;
    ButtonOK: TButton;
    Button2: TButton;
    De: TEdit;
    GotoAstrometry: TCheckBox;
    Label1: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    LabelResolver: TLabel;
    msginfo: TLabel;
    Obj: TEdit;
    Panel2: TPanel;
    PxSz: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    LabelAz: TLabel;
    LabelAlt: TLabel;
    Panel1: TPanel;
    PanelAltAz: TPanel;
    PanelPxSz: TPanel;
    Ra: TEdit;
    procedure BtnSearchClick(Sender: TObject);
    procedure BtnComputeClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure CenterChange(Sender: TObject);
    function CheckImageInfo(fits:Tfits): boolean;
    procedure FormShow(Sender: TObject);
    procedure ObjKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    LastMsg: string;
    FPlanetarium: TPlanetarium;
    procedure recvdata(msg:string);
    procedure SetPlanetarium(value: TPlanetarium);
  public
    focallength,pixelsize: double;
    procedure SetLang;
    property planetarium: TPlanetarium read FPlanetarium write SetPlanetarium;
  end;

var
  f_goto: Tf_goto;

implementation

{$R *.lfm}

{ Tf_goto }

procedure Tf_goto.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self);
  SetLang;
end;

procedure Tf_goto.SetLang;
begin
  Caption:=rsGoto;
  ButtonOK.Caption:=rsGoto;
  Button2.Caption:=rsCancel;
  BtnSearch.Caption:=rsSearch;
  Label1.Caption:=rsCenterRA;
  Label6.Caption:=j2000;
  Label2.Caption:=rsCenterDec;
  Label16.Caption:=j2000;
  Label3.Caption:=rsObjectName;
  Label4.Caption:=rsAzimuth;
  Label5.Caption:=rsElevation;
  Label7.Caption:=rsImageScale;
  Label8.Caption:=ssec+'/'+rsPixel;
  GotoAstrometry.Caption:=rsUseAstrometr;
  msginfo.Caption:='';
  LabelResolver.Caption:='';
  BtnCompute.Caption:=rsCompute;
  if f_compute<>nil then f_compute.Setlang;
end;

procedure Tf_goto.FormShow(Sender: TObject);
begin
  Obj.SetFocus;
  if (planetarium<>nil) and (planetarium.Connected) then begin
    LastMsg:='';
    planetarium.onReceiveData:=@recvdata;
    recvdata('');
  end;
  if PanelAltAz.Visible then msginfo.Caption:='Search object name, click on planetarium or type the coordinates';
end;

procedure Tf_goto.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (planetarium<>nil) then planetarium.onReceiveData:=nil;
end;

procedure Tf_goto.SetPlanetarium(value: TPlanetarium);
begin
  FPlanetarium:=value;
end;

procedure Tf_goto.recvdata(msg: string);
begin
   if (planetarium=nil) or (not planetarium.Connected) then begin
     Exit;
   end;
   LastMsg:=msg;
   if (planetarium.RA<>NullCoord)and(planetarium.DE<>NullCoord) then begin
     Ra.Text:=RAToStr(planetarium.RA);
     De.Text:=DEToStr(planetarium.DE);
     if planetarium.Objname<>'' then
       Obj.Text:=trim(planetarium.Objname);
     LabelResolver.Caption:=rsFromPlanetar;
     planetarium.ClearData;
   end;
end;

procedure Tf_goto.ObjKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key=VK_RETURN then begin
    ActiveControl:=BtnSearch;
    BtnSearchClick(Sender);
    Obj.SetFocus;
  end;
end;

procedure Tf_goto.BtnSearchClick(Sender: TObject);
var ra0,dec0,length0,width0,pa : double;
    objname,sname,sresolv : string;
    maglist: TMagnitudeList;
    found: boolean;
    p: integer;
begin
  LabelResolver.Caption:='';
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
        Ra.Text:=RAToStr(ra0*12/pi);{Add position}
        De.Text:=DEToStr(dec0*180/pi);
        if naam3='' then
           Obj.Text:=naam2 {Add one object name only}
        else
           Obj.Text:=naam2+'_'+naam3; {Add two object names}
        linepos:=$FFFFFF; {Stop searching}
        found:=true;
        LabelResolver.Caption:='From internal database';
     end;
    until linepos>=$FFFFFF;{Found object or end of database}
    if not found then begin
      // online search
      found:=SearchOnline(objname,sname,sresolv,ra0,dec0,maglist);
      if found then begin
        Ra.Text:=RAToStr(ra0*12/pi);{Add position}
        De.Text:=DEToStr(dec0*180/pi);
        // do not change the name by other synonym that can be returned by Simbad
        LabelResolver.Caption:=sname+', from '+sresolv;
      end;
    end;
    if not found then begin
      Ra.Text:='';
      De.Text:='';
      LabelResolver.Caption:='Not found!';
    end
    else begin
      if PanelPxSz.Visible then begin
         PxSz.SetFocus;
         PxSz.SelStart := Length(PxSz.Text);
      end;
    end;
  end;
end;

procedure Tf_goto.CenterChange(Sender: TObject);
var gra,gde,az,alt:double;
    tra,tde: string;
begin
  try
  az:=0; alt:=0;
  tra:=Ra.Text;
  tde:=De.Text;
  if tra='' then
    gra:=NullCoord
  else
    gra:=StrToAR(tra);
  if tde='' then
    gde:=NullCoord
  else
    gde:=StrToDE(tde);
  if (gra<>NullCoord) and (gde<>NullCoord) then begin
    gra:=deg2rad*gra*15;
    gde:=deg2rad*gde;
    J2000ToApparent(gra,gde);
    gra:=rad2deg*gra/15;
    gde:=rad2deg*gde;
    cmdEq2Hz(gra,gde,az,alt);
    if AzimuthOrigin=azSouth then az:=rmod(180+az,360);
    LabelAz.Caption:=FormatFloat(f2,az);
    LabelAlt.Caption:=FormatFloat(f2,alt);
  end
  else begin
    LabelAz.Caption:='-';
    LabelAlt.Caption:='-';
  end;
  except
    LabelAz.Caption:='-';
    LabelAlt.Caption:='-';
  end;
end;

procedure Tf_goto.BtnComputeClick(Sender: TObject);
begin
  FormPos(f_compute,mouse.CursorPos.X,mouse.CursorPos.Y);
  f_compute.PageControl1.ActivePage:=f_compute.ImageScale;
  if focallength>0 then f_compute.ImgScaleFocal.Text:=FormatFloat(f1,focallength);
  if pixelsize>0 then f_compute.ImgScalePx.Text:=FormatFloat(f2,pixelsize);
  f_compute.showmodal;
  if f_compute.ModalResult=mrOK then begin
    pxsz.Text:=f_compute.ImgScale.Text;
  end;
end;

procedure Tf_goto.ButtonOKClick(Sender: TObject);
begin
  if PanelAltAz.Visible then begin
    if LabelAlt.Caption='-' then begin
      ShowMessage('Cannot slew to unknown coordinates');
      exit;
    end;
    if StrToFloatDef(LabelAlt.Caption,-1)<0 then begin
      ShowMessage('Cannot slew below the horizon');
      exit;
    end;
  end;
  ModalResult:=mrOK;
end;

function Tf_goto.CheckImageInfo(fits:Tfits): boolean;
var fra,fdec,px,p,fl: double;
    i: integer;
begin
  if fits.HeaderInfo.valid and fits.ImageValid then begin
    fra:=fits.HeaderInfo.ra;
    fdec:=fits.HeaderInfo.dec;
    px:=fits.HeaderInfo.scale;
    p:=fits.HeaderInfo.pixsz1;
    fl:=fits.HeaderInfo.focallen;
    if px=0 then px:=LastPixelSize;
    if (fra=NullCoord)or(fdec=NullCoord)or(px=0) then begin
      FormPos(self,mouse.CursorPos.X,mouse.CursorPos.Y);
      Caption:=rsResolve;
      PanelAltAz.Visible:=false;
      PanelPxSz.Visible:=true;
      ButtonOK.Caption:=rsResolve;
      msginfo.Caption:=rsApproximateC;
      focallength:=fl;
      pixelsize:=p;
      if fra<>NullCoord then Ra.Text:=RAToStr(fra/15) else Ra.Text:='';
      if fdec<>NullCoord then De.Text:=DEToStr(fdec) else De.Text:='';
      if px<>0 then PxSz.Text:=FormatFloat(f2,px) else PxSz.Text:='';
      ActiveControl:=Obj;
      ShowModal;
      if ModalResult=mrok then begin
        fra:=StrToAR(Ra.Text);
        fdec:=StrToDE(De.Text);
        px:=StrToFloatDef(PxSz.Text,0);
        LastPixelSize:=px;
        i:=fits.Header.Indexof('END');
        if i<7 then i:=7;  // skip mandatory keywords
        if i>=fits.Header.Rows.Count then
          i:=fits.Header.Rows.Count-1;
        if px<>0 then begin
           fits.Header.Insert(i,'SECPIX1',px,'');
           fits.Header.Insert(i,'SECPIX2',px,'');
           fits.Header.Insert(i,'SCALE',px,'');
        end;
        if fdec<>NullCoord then fits.Header.Insert(i,'DEC',fdec,'');
        if fra<>NullCoord then fits.Header.Insert(i,'RA',15*fra,'');
        result:=true;
      end
      else begin
        globalmsg(rsStopAstromet2);
        result:=false;
      end;
    end
    else
      result:=true;
  end;
end;

end.

