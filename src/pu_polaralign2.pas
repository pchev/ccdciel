unit pu_polaralign2;

{$mode ObjFPC}{$H+}

{
Copyright (C) 2021 Patrick Chevalley

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

uses u_translation, u_utils, u_global, fu_preview, cu_fits, cu_astrometry, cu_mount, cu_wheel,
     fu_visu, indiapi, UScaleDPI, math,
     Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { Tf_polaralign2 }

  Tf_polaralign2 = class(TForm)
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Measurement2: TButton;
    Measurement1: TButton;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Measurement1Click(Sender: TObject);
    procedure Measurement2Click(Sender: TObject);
  private
    FFits: TFits;
    Fpreview: Tf_preview;
    Fvisu: Tf_visu;
    Fwheel: T_wheel;
    FAstrometry: TAstrometry;
    FMount: T_mount;
    FonShowMessage: TNotifyMsg;
    FonClose: TNotifyEvent;
    FSidt,FRa,FDe,FMountRa,FmountDe: array[1..2] of double;
    corr_alt, corr_az, corr_ra, corr_de: double;
    FInProgress: boolean;
    FTerminate: boolean;
    procedure SetLang;
    procedure msg(txt:string; level: integer);
    procedure tracemsg(txt: string);
    procedure TakeExposure;
    procedure Solve(step: integer);
    procedure MountPosition(step: integer);
    procedure Compute;
    procedure InitAlignment;
    procedure AbortAlignment;
  public
    property Fits: TFits read FFits write FFits;
    property Preview:Tf_preview read Fpreview write Fpreview;
    property Visu: Tf_visu read Fvisu write Fvisu;
    property Wheel: T_wheel read Fwheel write Fwheel;
    property Astrometry: TAstrometry read FAstrometry write FAstrometry;
    property Mount: T_mount read Fmount write Fmount;
    property onShowMessage: TNotifyMsg read FonShowMessage write FonShowMessage;
    property onClose: TNotifyEvent read FonClose write FonClose;
  end;

var
  f_polaralign2: Tf_polaralign2;

implementation

{$R *.lfm}

{ Tf_polaralign2 }

procedure Tf_polaralign2.Measurement1Click(Sender: TObject);
begin
  memo1.Clear;
  MountPosition(1);
  TakeExposure;
  Solve(1);
end;

procedure Tf_polaralign2.Measurement2Click(Sender: TObject);
begin
  MountPosition(2);
  TakeExposure;
  Solve(2);
  Compute;
end;

procedure Tf_polaralign2.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self);
  SetLang;
end;

procedure Tf_polaralign2.FormShow(Sender: TObject);
begin
  InitAlignment;
end;

procedure Tf_polaralign2.SetLang;
begin

end;

procedure Tf_polaralign2.tracemsg(txt: string);
begin
  if assigned(FonShowMessage) then FonShowMessage('PolarAlign: '+txt,9);
end;

procedure Tf_polaralign2.msg(txt:string; level: integer);
begin
 memo1.lines.add(txt);
 if assigned(FonShowMessage) then FonShowMessage(txt,level);
end;

procedure Tf_polaralign2.TakeExposure;
var exp:double;
    bin,filter,pgain,poffset: integer;
    fn: string;
begin
// Start an exposure
fits.SetBPM(bpm,bpmNum,bpmX,bpmY,bpmAxis);
fits.DarkOn:=true;
exp:=config.GetValue('/PrecSlew/Exposure',10.0);
bin:=config.GetValue('/PrecSlew/Binning',1);
pgain:=config.GetValue('/PrecSlew/Gain',NullInt);
poffset:=config.GetValue('/PrecSlew/Offset',NullInt);
filter:=config.GetValue('/PrecSlew/Filter',0);
if (filter>0)and(Assigned(Fwheel)) then begin
  Fwheel.Filter:=filter;
end;
tracemsg('Exposure exptime='+FormatFloat(f3,exp)+' binning='+inttostr(bin)+' filter='+inttostr(filter));
if not preview.ControlExposure(exp,bin,bin,LIGHT,ReadoutModeAstrometry,pgain,poffset) then begin
    msg(rsExposureFail,1);
    AbortAlignment;
end;
end;

procedure Tf_polaralign2.Solve(step: integer);
var cra,cde,eq,pa,jd0: double;
    y,m,d: integer;
    h: double;
begin
  // solve the current image
  FAstrometry.SolveCurrentImage(true);
  if (not FAstrometry.Busy)and FAstrometry.LastResult then begin
     if FAstrometry.CurrentCoord(cra,cde,eq,pa) then begin
       tracemsg('Plate solve successful for image '+inttostr(step));
       tracemsg('Image center J2000: RA='+FormatFloat(f6,cra)+' DEC='+FormatFloat(f6,cde)+' PA='+FormatFloat(f6,pa));
       cra:=cra*15*deg2rad;
       cde:=cde*deg2rad;
       PrecessionFK5(jd2000,jdtoday,cra,cde);
       tracemsg('Image center JNOW: RA='+FormatFloat(f6,rad2deg*cra/15)+' DEC='+FormatFloat(f6,rad2deg*cde));
       FRa[step]:=cra;
       FDe[step]:=cde;
       Djd(FFits.dateobs+FFits.HeaderInfo.exptime/secperday/2,y,m,d,h);
       jd0:=jd(y,m,d,0);
       FSidt[step]:=Sidtim(jd0,h,ObsLongitude);
       memo1.Lines.Add('Image position '+inttostr(step)+': '+RAToStr(FRa[step]*rad2deg/15)+'/'+DEToStr(FDe[step]*rad2deg)+' sidereal time: '+RAToStr(FSidt[step]*rad2deg/15));
     end
     else begin
       msg(rsFailToResolv,1);
       AbortAlignment;
     end;
  end
  else begin
    msg(rsFailToResolv,1);
    AbortAlignment;
  end;
end;

procedure Tf_polaralign2.MountPosition(step: integer);
var tra,tde: double;
begin
  tra:=mount.RA;
  tde:=mount.Dec;
  MountToLocal(mount.EquinoxJD,tra,tde);
  FMountRa[step]:=deg2rad*15*tra;
  FmountDe[step]:=deg2rad*tde;
  memo1.Lines.Add('Mount position '+inttostr(step)+': '+RAToStr(tra)+'/'+DEToStr(tde));
end;


{Polar error calculation based on two celestial reference points and the error of the telescope mount at these point(s).
 Based on formulas from Ralph Pass documented at https://rppass.com/align.pdf.
 They are based on the book “Telescope Control’ by Trueblood and Genet, p.111
 Ralph added sin(latitude) term in the equation for the error in RA.


 For one reference image the difference in RA and DEC caused by the misalignment of the polar axis, formula (3):
   delta_ra:= de * TAN(dec)*SIN(h)  + da * (sin(lat)- COS(lat)*(TAN(dec1)*COS(h_1))
   delta_dec:=de * COS(h)  + da * COS(lat)*SIN(h))

   where de is the polar error in elevation (altitude)
   where da is the polar error in azimuth
   where h is the hour angle of the reference point equal ra - local_sidereal_time

 Using the above formula calculate the difference in RA and DEC by subtracting the first image postion from the second reference image. The common term sin(lat) will be nulified. Formula (4)
   delta_ra:= de * (TAN(dec)*SIN(h_2)-TAN(dec1)*SIN(h_1))  + da * COS(lat)*(TAN(dec1)*COS(h_1)-TAN(dec2)*COS(h_2));
   delta_dec:=de * (COS(h_2)-COS(h_1))  + da * COS(lat)*(SIN(h_2)-SIN(h_1));

 Writing the above formulas in matrix notation:
   [delta_Ra;delta_Dec]= C * [delta_Elv;delta_Azm]
   then
   [delta_Elv;delta_Az] = inv(C)*[delta_Ra;delta_Dec]

 Mount is assumed to be ideal. Mount fabrication error & cone errors are assumed to be zero. Meridian crossing between the two images should be avoided}
procedure Tf_polaralign2.Compute;
var determinant,delta_ra, delta_dec, h_1, h_2, lat_rad: double;
    ew,ns  : string;
    A,B,C, C_inv : array[0..1,0..1] of double;
begin
  lat_rad:=ObsLatitude*pi/180;{obs latitude in radians}
  delta_ra:=(FMountRa[2]-FRa[2]) - (FMountRa[1]-FRa[1]);
  delta_dec:=(FmountDe[2]-FDe[2]) - (FmountDe[1]-FDe[1]);

  h_1:=FMountRa[1]-FSidt[1];
  h_2:=FMountRa[2]-FSidt[2];

  // [delta_Ra;delta_Dec]= A * [delta_Elv;delta_Azm]
  // Fill matrix image 1 with data.
  A[0,0]:=TAN(FmountDe[1])*SIN(h_1);
  A[1,0]:=COS(lat_rad)*(SIN(LAT_rad)-TAN(FmountDe[1])*COS(h_1));{keep the sin(lat_rad) for the calculation of star movement in the last step}
  A[0,1]:=COS(h_1);
  A[1,1]:=COS(lat_rad)*SIN(h_1);

  // Fill matrix image 2 with data.
  B[0,0]:=TAN(FmountDe[2])*SIN(h_2);
  B[1,0]:=COS(lat_rad)*(SIN(LAT_rad)-TAN(FmountDe[2])*COS(h_2)) ;
  B[0,1]:=COS(h_2);
  B[1,1]:=COS(lat_rad)*SIN(h_2);

  //difference,  image 2 - image 1
  C[0,0]:=B[0,0]-A[0,0];
  C[1,0]:=B[1,0]-A[1,0];
  C[0,1]:=B[0,1]-A[0,1];
  C[1,1]:=B[1,1]-A[1,1];


  // Calculate the inverse matrix inv(C)
  determinant:=C[0,0]*C[1,1]-C[0,1]*C[1,0];
  C_inv[0,0]:=+C[1,1]/determinant;
  C_inv[1,1]:=+C[0,0]/determinant;
  C_inv[1,0]:=-C[1,0]/determinant;
  C_inv[0,1]:=-C[0,1]/determinant;

  // [delta_Elv;delta_Az] = inv(C)*[delta_Ra;delta_Dec]
  // Use the inverse matrix to calculate the polar axis elevation and azimuth error from the delta_dec and delta_ra between the two image positions.
  corr_alt:=C_inv[0,0]*delta_ra+C_inv[1,0]*delta_Dec;{delta_Elv}
  corr_az :=C_inv[0,1]*delta_ra+C_inv[1,1]*delta_Dec; {delta_Az}

  if abs(determinant)<0.1 then
     memo1.lines.add('Warning the calculation determinant is close to zero! Select other celestial locations. Avoid locations with similar hour angles, locations close to the celestial equator and locations whose declinations are close to negatives of each other.');

  if corr_az>0 then ew:=' east of the celestial pole.' else ew:=' west of the celestial pole.';
  if corr_alt>0  then ns:=' above the celestial pole' else ns:=' below the celestial pole';

  memo1.Lines.add('Determinant: '+FormatFloat(f2,determinant));
  memo1.Lines.add('Polar error Az: '+FormatFloat(f2,rad2deg*abs(corr_az)*60)+' arcminutes'+ew);
  memo1.Lines.add('Polar error Alt: '+FormatFloat(f2,rad2deg*abs(corr_alt)*60)+' arcminutes'+ns);

  //calculate the Ra, Dec correction for stars in image 2
  corr_ra:=B[0,0]*corr_alt + B[1,0]*corr_az;
  corr_de:=B[0,1]*corr_alt+  B[1,1]*corr_az;
  memo1.Lines.add('Stars in image 2 have to move: '+FormatFloat(f2,rad2deg*(corr_ra)*60)+' arcminutes in RA and '+FormatFloat(f2,rad2deg*(corr_de)*60)+' arcminutes in DEC by the correction.');
  //Warning avoid the zenith for the second image!! Azimuth changes will not create any change at zenith
end;

procedure Tf_polaralign2.InitAlignment;
var i: integer;
begin
  memo1.Clear;
  FInProgress:=false;
  FTerminate:=false;
  for i:=1 to 2 do begin
    FMountRa[i]:=0;
    FmountDe[i]:=0;
    FRa[i]:=0;
    FDe[i]:=0;
    FSidt[i]:=0;
  end;
end;

procedure Tf_polaralign2.AbortAlignment;
begin
  if FTerminate then exit;
  FTerminate:=true;
  msg(rsCancelPolarA,1);
  if FInProgress then begin
    if Mount.MountSlewing then Mount.AbortMotion;
    if Astrometry.Busy then Astrometry.StopAstrometry;
  end;
  FInProgress:=false;
  Close;
end;

end.

