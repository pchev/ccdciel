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

uses u_translation, u_utils, u_global, fu_preview, pu_goto, cu_fits, cu_astrometry, cu_mount, cu_wheel,
     fu_visu, indiapi, UScaleDPI, math, LazSysUtils,
     Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type

  { Tf_polaralign2 }

  Tf_polaralign2 = class(TForm)
    BtnLock: TToggleBox;
    ButtonStart: TButton;
    IgnoreMount: TCheckBox;
    LabelPol2: TLabel;
    LabelQ: TLabel;
    LabelPol1: TLabel;
    Instruction: TMemo;
    QualityBar: TPanel;
    PanelQuality: TPanel;
    Panel1: TPanel;
    DeterminantTimer: TTimer;
    QualityShape: TShape;
    procedure BtnLockClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure DeterminantTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IgnoreMountClick(Sender: TObject);
  private
    FFits: TFits;
    Fpreview: Tf_preview;
    Fvisu: Tf_visu;
    Fwheel: T_wheel;
    FAstrometry: TAstrometry;
    FMount: T_mount;
    FonShowMessage: TNotifyMsg;
    FonClose: TNotifyEvent;
    CurrentStep: integer;
    FSidt,FRa,FDe,FMountRa,FMountDe: array[1..2] of double;
    corr_alt, corr_az, corr_ra, corr_de: double;
    FInProgress: boolean;
    FTerminate: boolean;
    lockbutton: boolean;
    procedure SetLang;
    procedure msg(txt:string; level: integer);
    procedure tracemsg(txt: string);
    procedure ButtonStartAsync(Data: PtrInt);
    procedure TakeExposure;
    procedure Solve(step: integer);
    procedure Sync(step: integer);
    procedure MountPosition(step: integer);
    procedure NoMountPosition(step: integer);
    procedure Measurement1;
    procedure Measurement2;
    procedure StartAdjustement;
    procedure Compute;
    procedure ComputeCorrection;
    Function  CurrentDeterminant: double;
    procedure CurrentAdjustement(RA,DE: double; out cra,cde,caza,cazd: double);
    procedure InitAlignment;
    procedure AbortAlignment;
    procedure StartImageLoop;
    procedure StopImageLoop;
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

procedure Tf_polaralign2.Measurement1;
begin
  FInProgress:=true;
  if (not IgnoreMount.Checked) then begin
    MountPosition(1);
  end;
  TakeExposure;
  Solve(1);
  if (not IgnoreMount.Checked) then
    DeterminantTimer.Enabled:=true
  else
    NoMountPosition(1);
  CurrentStep:=1;
end;

procedure Tf_polaralign2.Measurement2;
begin
  DeterminantTimer.Enabled:=false;
  if (not IgnoreMount.Checked) then begin
    MountPosition(2);
  end;
  TakeExposure;
  Solve(2);
  if IgnoreMount.Checked then
    NoMountPosition(2);
  CurrentStep:=2;
  Compute;
  FInProgress:=true;
  StartImageLoop;
end;

procedure Tf_polaralign2.StartAdjustement;
var cra,cde,eq,pa: double;
begin
  StopImageLoop;
  TakeExposure;
  f_goto.CheckImageInfo(fits);
  FAstrometry.SolveCurrentImage(true);
  if FAstrometry.Busy or (not FAstrometry.LastResult) or
    (not FAstrometry.CurrentCoord(cra,cde,eq,pa)) then begin
    msg(rsFailToResolv,1);
    AbortAlignment;
  end;
  ComputeCorrection;
  StartImageLoop;
end;

procedure Tf_polaralign2.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self);
  SetLang;
  lockbutton:=false;
end;

procedure Tf_polaralign2.FormShow(Sender: TObject);
begin
  InitAlignment;
end;

procedure Tf_polaralign2.IgnoreMountClick(Sender: TObject);
begin
   if FMount.Status<>devConnected then IgnoreMount.Checked:=true;
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
 if assigned(FonShowMessage) then FonShowMessage('PolarAlign: '+txt,level);
end;

procedure Tf_polaralign2.TakeExposure;
var exp:double;
    bin,filter,pgain,poffset: integer;
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
  f_goto.CheckImageInfo(fits);
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
       tracemsg('Image position '+inttostr(step)+': '+RAToStr(FRa[step]*rad2deg/15)+'/'+DEToStr(FDe[step]*rad2deg)+' sidereal time: '+RAToStr(FSidt[step]*rad2deg/15));
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

procedure Tf_polaralign2.Sync(step: integer);
var ra,de: double;
begin
  ra:=rad2deg*FRa[step]/15;
  de:=rad2deg*FDe[step];
  tracemsg('Mount sync to local '+RAToStr(ra)+'/'+DEToStr(de));
  LocalToMount(mount.EquinoxJD,ra,de);
  if not mount.Sync(ra,de) then
    AbortAlignment;
end;

procedure Tf_polaralign2.MountPosition(step: integer);
var tra,tde: double;
begin
  tra:=mount.RA;
  tde:=mount.Dec;
  if (tra=NullCoord)or(tde=NullCoord) then begin
    msg('Error reading mount coordinates',1);
    AbortAlignment;
    exit;
  end;
  MountToLocal(mount.EquinoxJD,tra,tde);
  FMountRa[step]:=deg2rad*15*tra;
  FMountDe[step]:=deg2rad*tde;
  tracemsg('Mount position '+inttostr(step)+': '+RAToStr(tra)+'/'+DEToStr(tde));
end;

procedure Tf_polaralign2.NoMountPosition(step: integer);
begin
 if step=1 then begin
   FMountRa[step]:=FRa[step];
   FMountDe[step]:=FDe[step];
 end
 else if step=2 then begin
   FMountRa[step]:=FRa[step];
   FMountDe[step]:=FMountDe[1];
 end;
end;

procedure Tf_polaralign2.DeterminantTimerTimer(Sender: TObject);
var det: double;
begin
if (not IgnoreMount.Checked) then begin
  det:=abs(CurrentDeterminant);
  LabelQ.Caption:='Quality: '+formatfloat(f2,det);
  QualityShape.Width:=max(5,min(100,round(100*det)));
  if QualityShape.Width<20 then QualityShape.Brush.Color:=clRed
  else if QualityShape.Width<60 then QualityShape.Brush.Color:=clYellow
  else QualityShape.Brush.Color:=clLime;
end;
end;

procedure Tf_polaralign2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  PolarAlignmentOverlay:=false;
  tracemsg('Close polar alignment form');
  if preview.Loop then preview.BtnLoopClick(nil);
  if Assigned(FonClose) then FonClose(self);
end;

Function Tf_polaralign2.CurrentDeterminant: double;
var A,B,C: array[0..1,0..1] of double;
    RA2,DE2,SIDT2,jd0,h,lat_rad,h_1,h_2: double;
    y,m,d: word;
begin
  RA2:=mount.RA;
  DE2:=mount.Dec;
  if (RA2=NullCoord)or(DE2=NullCoord) then begin
    msg('Error reading mount coordinates',1);
    AbortAlignment;
    exit;
  end;
  MountToLocal(mount.EquinoxJD,RA2,DE2);
  RA2:=RA2*deg2rad*15;
  DE2:=DE2*deg2rad;
  DecodeDate(now,y,m,d);
  jd0:=jd(y,m,d,0);
  h:=frac(NowUTC)*24;
  SIDT2:=Sidtim(jd0,h,ObsLongitude);
  lat_rad:=ObsLatitude*pi/180;
  h_1:=FMountRa[1]-FSidt[1];
  h_2:=RA2-SIDT2;
  // Fill matrix 1 with data.
  A[0,0]:=TAN(FMountDe[1])*SIN(h_1);
  A[1,0]:={SIN(LAT_rad)}-COS(lat_rad)*TAN(FMountDe[1])*COS(h_1);{sin(lat_rad) will be nulified anyhow}
  A[0,1]:=COS(h_1);
  A[1,1]:=COS(lat_rad)*SIN(h_1);
  // Fill matrix 2 with telescope position.
  B[0,0]:=TAN(DE2)*SIN(h_2);
  B[1,0]:={SIN(LAT_rad)}-COS(lat_rad)*TAN(DE2)*COS(h_2);{sin(lat_rad) will be nulified anyhow}
  B[0,1]:=COS(h_2);
  B[1,1]:=COS(lat_rad)*SIN(h_2);
  //difference,  matrix 2 - matrix 1
  C[0,0]:=B[0,0]-A[0,0];
  C[1,0]:=B[1,0]-A[1,0];
  C[0,1]:=B[0,1]-A[0,1];
  C[1,1]:=B[1,1]-A[1,1];
  // Calculate the determinant
  result:=C[0,0]*C[1,1]-C[0,1]*C[1,0];
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
  [delta_Ra;delta_Dec]= C * [de;da]
   then
   [de;da] = inv(C)*[delta_Ra;delta_Dec]

 Mount is assumed to be ideal. Mount fabrication error & cone errors are assumed to be zero. Meridian crossing between the two images should be avoided}
procedure Tf_polaralign2.Compute;
var determinant,delta_ra, delta_dec, h_1, h_2, lat_rad: double;
    ew,ns  : string;
    A,B,C, C_inv : array[0..1,0..1] of double;
begin
  lat_rad:=ObsLatitude*pi/180;{obs latitude in radians}
  delta_ra:=(FMountRa[2]-FRa[2]) - (FMountRa[1]-FRa[1]);
  delta_dec:=(FMountDe[2]-FDe[2]) - (FMountDe[1]-FDe[1]);

  h_1:=FMountRa[1]-FSidt[1];
  h_2:=FMountRa[2]-FSidt[2];

  // [delta_Ra;delta_Dec]= A * [delta_Elv;delta_Azm]
  // Fill matrix image 1 with data.
  A[0,0]:=TAN(FMountDe[1])*SIN(h_1);
  A[1,0]:=SIN(LAT_rad)-COS(lat_rad)*TAN(FMountDe[1])*COS(h_1);{keep the sin(lat_rad) for the calculation of star movement in the last step}
  A[0,1]:=COS(h_1);
  A[1,1]:=COS(lat_rad)*SIN(h_1);

  // Fill matrix image 2 with data.
  B[0,0]:=TAN(FMountDe[2])*SIN(h_2);
  B[1,0]:=SIN(LAT_rad)-COS(lat_rad)*TAN(FMountDe[2])*COS(h_2);
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

  // [de;da] = inv(C)*[delta_Ra;delta_Dec]
  // Use the inverse matrix to calculate the polar axis elevation and azimuth error from the delta_dec and delta_ra between the two image positions.
  corr_alt:=C_inv[0,0]*delta_ra+C_inv[1,0]*delta_Dec;{delta_Elv}
  corr_az :=C_inv[0,1]*delta_ra+C_inv[1,1]*delta_Dec; {delta_Az}

  if abs(determinant)<0.1 then
     msg('Warning the calculation determinant is close to zero! Select other celestial locations. Avoid locations with similar hour angles, locations close to the celestial equator and locations whose declinations are close to negatives of each other.',1);

  if corr_az>0 then ew:=' east of the celestial pole.' else ew:=' west of the celestial pole.';
  if corr_alt>0  then ns:=' above the celestial pole' else ns:=' below the celestial pole';

  msg('Determinant: '+FormatFloat(f2,determinant),1);
  LabelPol1.Caption:='Polar error Az: '+FormatFloat(f2,rad2deg*abs(corr_az)*60)+' arcminutes'+ew;
  LabelPol2.Caption:='Polar error Alt: '+FormatFloat(f2,rad2deg*abs(corr_alt)*60)+' arcminutes'+ns;
  tracemsg(LabelPol1.Caption);
  tracemsg(LabelPol2.Caption);

  //calculate the Ra, Dec correction for stars in image 2
  corr_ra:=B[0,0]*corr_alt + B[1,0]*corr_az;
  corr_de:=B[0,1]*corr_alt+  B[1,1]*corr_az;

//  dRa:=de*(TAN(dec4)*SIN(sidereal_time-ra4) ) +azimuth_error*(sin(latitude)-COS(latitude)*TAN(dec4)*COS(sidereal_time-ra4));
//  dDec:=de*(COS(sidereal_time-ra4))  +azimuth_error*COS(latitude)*(SIN(sidereal_time-ra4));



  tracemsg('Stars in image 2 have to move: '+FormatFloat(f2,rad2deg*(corr_ra)*60)+' arcminutes in RA and '+FormatFloat(f2,rad2deg*(corr_de)*60)+' arcminutes in DEC by the correction.');
  //Warning avoid the zenith for the second image!! Azimuth changes will not create any change at zenith
end;

procedure Tf_polaralign2.CurrentAdjustement(RA,DE: double; out cra,cde,caza,cazd: double);
var B: array[0..1,0..1] of double;
    SIDT2,jd0,h,lat_rad,h_2: double;
    y,m,d: word;
begin
  DecodeDate(now,y,m,d);
  jd0:=jd(y,m,d,0);
  h:=frac(NowUTC)*24;
  SIDT2:=Sidtim(jd0,h,ObsLongitude);
  lat_rad:=ObsLatitude*pi/180;
  h_2:=RA-SIDT2;
  // Fill matrix 2 with telescope position.
  B[0,0]:=TAN(DE)*SIN(h_2);
  B[1,0]:=SIN(LAT_rad)-COS(lat_rad)*TAN(DE)*COS(h_2);
  B[0,1]:=COS(h_2);
  B[1,1]:=COS(lat_rad)*SIN(h_2);

  //calculate the Ra, Dec correction for stars in image 2
  cra:=B[0,0]*corr_alt + B[1,0]*corr_az;
  cde:=B[0,1]*corr_alt+  B[1,1]*corr_az;
  // same with corr_alt=0 for intermediate point
  caza:=B[1,0]*corr_az;
  cazd:=B[1,1]*corr_az;
end;

procedure Tf_polaralign2.ComputeCorrection;
var p: TcdcWCScoord;
    ra,de,cazr,cazd,eq,pa,lra,lde: double;
    n: integer;
    ok:boolean;
begin
try
  ok:=true;
  FAstrometry.CurrentCoord(ra,de,eq,pa);
  lra:=ra*15*deg2rad;
  lde:=de*deg2rad;
  J2000ToApparent(lra,lde);
  // adjustement at current position
  CurrentAdjustement(lra,lde,corr_ra,corr_de,cazr,cazd);
  tracemsg('Stars in image 3 have to move: '+FormatFloat(f2,rad2deg*(corr_ra)*60)+' arcminutes in RA and '+FormatFloat(f2,rad2deg*(corr_de)*60)+' arcminutes in DEC by the correction.');

  // start point
  p.ra:=ra*15;
  p.dec:=de;
  n:=cdcwcs_sky2xy(@p,0);
  if n=1 then begin ok:=false; exit; end;
  PolarAlignmentStartx:=p.x;
  PolarAlignmentStarty:=fits.HeaderInfo.naxis2-p.y;

  // intermediate point
  p.ra:=ra*15-rad2deg*cazr/max(0.00000000001,cos(de*deg2rad));
  p.dec:=de-rad2deg*cazd;
  n:=cdcwcs_sky2xy(@p,0);
  if n=1 then begin ok:=false; exit; end;
  PolarAlignmentAzx:=p.x;
  PolarAlignmentAzy:=fits.HeaderInfo.naxis2-p.y;
  PolarAlignmentOverlay:=true;

  // end point
  p.ra:=ra*15-rad2deg*corr_ra/max(0.00000000001,cos(de*deg2rad));
  p.dec:=de-rad2deg*corr_de;
  n:=cdcwcs_sky2xy(@p,0);
  if n=1 then begin ok:=false; exit; end;
  PolarAlignmentEndx:=p.x;
  PolarAlignmentEndy:=fits.HeaderInfo.naxis2-p.y;

finally
  if not ok then begin
    msg('WCS error, offscale',1);
    AbortAlignment;
  end;
end;
end;

procedure Tf_polaralign2.ButtonStartClick(Sender: TObject);
begin
  if lockbutton then exit;
  lockbutton:=true;
  Application.QueueAsyncCall(@ButtonStartAsync,0);
end;

procedure Tf_polaralign2.BtnLockClick(Sender: TObject);
begin
  PolarAlignmentLock:=BtnLock.Checked;
  if PolarAlignmentLock then begin
    tracemsg('Overlay locked');
    tracemsg('Overlay offset X='+FormatFloat(f6,PolarAlignmentOverlayOffsetX)+' Y='+FormatFloat(f6,PolarAlignmentOverlayOffsetY));
  end
  else begin
    tracemsg('Overlay unlocked');
  end;
end;

procedure Tf_polaralign2.ButtonStartAsync(Data: PtrInt);
begin
 try
 try
 case ButtonStart.tag of
   1: begin
      IgnoreMount.Enabled:=false;
      IgnoreMount.Visible:=false;
      Instruction.Clear;
      Instruction.Lines.Add('Measuring first position, please wait...');
      Application.ProcessMessages;
      Measurement1;
      Instruction.Clear;
      Instruction.Lines.Add('Point the telescope for the second measurement.');
      Instruction.Lines.Add('The ideal position is near the zenit');
      if (not IgnoreMount.Checked) then begin
        Instruction.Lines.Add('at a declination near '+FormatFloat(f0,ObsLatitude)+'°');
        Instruction.Lines.Add('Look to make the Quality indicator the highest as possible.');
      end
      else begin
        Instruction.Lines.Add('Do not touch the declination, move only in RA');
      end;
      Instruction.Lines.Add('Be careful to not cross the meridian.');
      Instruction.Lines.Add('');
      Instruction.Lines.Add('When ready click the Next button');
      if (not IgnoreMount.Checked) then PanelQuality.Visible:=true;
      BtnLock.Visible:=false;
      ButtonStart.Caption:='Next';
      ButtonStart.tag:=2;
      end;
   2: begin
      Instruction.Clear;
      Instruction.Lines.Add('Measuring second position, please wait...');
      Application.ProcessMessages;
      Measurement2;
      Instruction.Clear;
      Instruction.Lines.Add('Point the telescope at the adjustement position.');
      Instruction.Lines.Add('The ideal position is near the meridian');
      Instruction.Lines.Add('at a declination near '+FormatFloat(f0,(ObsLatitude-90)/2)+'°');
      Instruction.Lines.Add('Be careful to not cross the meridian.');
      Instruction.Lines.Add('');
      Instruction.Lines.Add('When ready click the Next button');
      PanelQuality.Visible:=false;
      IgnoreMount.Visible:=false;
      BtnLock.Visible:=false;
      ButtonStart.Caption:='Next';
      ButtonStart.tag:=3;
      end;
   3: begin
      Instruction.Clear;
      Instruction.Lines.Add('Measuring third position, please wait...');
      Application.ProcessMessages;
      StartAdjustement;
      Instruction.Clear;
      Instruction.Lines.Add(rsMoveTheGreen);
      Instruction.Lines.Add(rsThenAdjustTh);
      Instruction.Lines.Add(rsForGuidanceA);
      Instruction.Lines.Add(rsYouCanCloseT);
      PanelQuality.Visible:=false;
      IgnoreMount.Visible:=false;
      BtnLock.Visible:=true;
      ButtonStart.Caption:='Close';
      ButtonStart.tag:=4;
      end;
   4: begin
      Close;
      end;
   else Close;
 end;
 except
   on E: Exception do begin
      msg(E.Message,1);
      Close;
   end;
 end;
 finally
   lockbutton:=false;
 end;
end;

procedure Tf_polaralign2.InitAlignment;
var i: integer;
begin
  Instruction.clear;
  Instruction.Lines.Add('Point the telescope for the first measurement.');
  Instruction.Lines.Add('The ideal position is  near the East or West horizon.');
  Instruction.Lines.Add('at an elevation of 30° and a declination near '+FormatFloat(f0,ObsLatitude)+'°');
  Instruction.Lines.Add('');
  Instruction.Lines.Add('When ready click the Start button');
  IgnoreMount.Visible:=true;
  IgnoreMount.Enabled:=true;
  IgnoreMount.Checked:=(FMount.Status<>devConnected);
  ButtonStart.Caption:='Start';
  ButtonStart.tag:=1;
  lockbutton:=false;
  LabelQ.Caption:='';
  LabelPol1.Caption:='';
  LabelPol2.Caption:='';
  PanelQuality.Visible:=false;
  BtnLock.Visible:=false;
  DeterminantTimer.Enabled:=false;
  PolarAlignmentOverlay:=false;
  StopImageLoop;
  FInProgress:=false;
  FTerminate:=false;
  CurrentStep:=0;
  for i:=1 to 2 do begin
    FMountRa[i]:=0;
    FMountDe[i]:=0;
    FRa[i]:=0;
    FDe[i]:=0;
    FSidt[i]:=0;
  end;
end;

procedure Tf_polaralign2.StartImageLoop;
begin
  // start image loop
  FVisu.BtnZoomAdjust.Click;
  preview.Exposure:=config.GetValue('/PrecSlew/Exposure',1.0);
  preview.Bin:=config.GetValue('/PrecSlew/Binning',1);
  preview.Gain:=config.GetValue('/PrecSlew/Gain',NullInt);
  preview.Offset:=config.GetValue('/PrecSlew/Offset',NullInt);
  if not preview.Loop then preview.BtnLoopClick(nil);
end;

procedure Tf_polaralign2.StopImageLoop;
begin
  // stop image loop
  if preview.Loop then begin
     preview.BtnLoopClick(nil);
     wait;
  end;
end;

procedure Tf_polaralign2.AbortAlignment;
begin
  if FTerminate then exit;
  FTerminate:=true;
  DeterminantTimer.Enabled:=false;
  PolarAlignmentOverlay:=false;
  if FInProgress then begin
    if (not IgnoreMount.Checked) and Mount.MountSlewing then Mount.AbortMotion;
    if Astrometry.Busy then Astrometry.StopAstrometry;
  end;
  FInProgress:=false;
  raise exception.Create(rsCancelPolarA);
end;

end.

