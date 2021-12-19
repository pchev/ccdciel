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

uses u_translation, u_utils, u_global, fu_preview, pu_goto, cu_fits,
  cu_astrometry, cu_mount, cu_wheel, cu_camera, fu_visu, indiapi, UScaleDPI, math,
  LazSysUtils, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, ActnList, Spin;

type

  { Tf_polaralign2 }

  Tf_polaralign2 = class(TForm)
    BtnLock: TToggleBox;
    ButtonAbort1: TButton;
    ButtonAbort3: TButton;
    ButtonAbort2: TButton;
    ButtonContinue: TButton;
    ButtonClose: TButton;
    ButtonMove: TButton;
    ButtonNext: TButton;
    FloatSpinEditAz1: TFloatSpinEdit;
    FloatSpinEditAz2: TFloatSpinEdit;
    FloatSpinEditAlt1: TFloatSpinEdit;
    FloatSpinEditAlt2: TFloatSpinEdit;
    GotoPosition: TComboBox;
    ButtonStart: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Instruction: TMemo;
    PageControl1: TPageControl;
    PanelCustomPosition: TPanel;
    Panel1: TPanel;
    TabSheetMove1: TTabSheet;
    TabSheetStart: TTabSheet;
    TabSheetAuto: TTabSheet;
    TabSheetAdjust: TTabSheet;
    TabSheetMove2: TTabSheet;
    procedure BtnLockClick(Sender: TObject);
    procedure ButtonAbortClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonContinueClick(Sender: TObject);
    procedure ButtonMoveClick(Sender: TObject);
    procedure ButtonNextClick(Sender: TObject);
    procedure GotoPositionChange(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FFits: TFits;
    Fpreview: Tf_preview;
    Fvisu: Tf_visu;
    Fwheel: T_wheel;
    Fcamera: T_camera;
    FAstrometry: TAstrometry;
    FMount: T_mount;
    FonShowMessage: TNotifyMsg;
    FonClose: TNotifyEvent;
    CurrentStep: integer;
    FSidt,FRa,FDe,FMountRa,FMountDe: array[1..2] of double;
    corr_alt, corr_az, corr_ra, corr_de, corr_rai, corr_dei: double;
    FInProgress: boolean;
    FAborted: boolean;
    procedure msg(txt:string; level: integer);
    procedure tracemsg(txt: string);
    procedure AutoMeasurementAsync(Data: PtrInt);
    procedure TakeExposure;
    procedure Solve(step: integer);
    procedure Sync(step: integer);
    procedure MountPosition(step: integer);
    procedure decode_combobox(out caz1,calt1,caz2,calt2: double);
    procedure Measurement1(ra,de: double);
    procedure Measurement2(ra,de: double);
    procedure StartAdjustement;
    procedure Compute;
    procedure ComputeCorrection;
    procedure CurrentAdjustement(RA,DE: double);
    procedure InitAlignment;
    procedure AbortAlignment;
    procedure StartImageLoop;
    procedure StopImageLoop;
    procedure SaveConfig;
  public
    procedure SetLang;
    property Fits: TFits read FFits write FFits;
    property Preview:Tf_preview read Fpreview write Fpreview;
    property Visu: Tf_visu read Fvisu write Fvisu;
    property Wheel: T_wheel read Fwheel write Fwheel;
    property Camera: T_camera read Fcamera write Fcamera;
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

procedure Tf_polaralign2.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self);
  SetLang;
end;

procedure Tf_polaralign2.FormDestroy(Sender: TObject);
var i: integer;
begin
  for i:=0 to GotoPosition.items.Count-1 do
     GotoPosition.items.Objects[i].free;
end;

procedure Tf_polaralign2.FormShow(Sender: TObject);
var altaz: TAltAzPosition;
    i: integer;
  procedure GetAE(h,d: double; n: integer; var azp:TAltAzPosition);
  var az,al: double;
  begin
      Eq2Hz(h*15*deg2rad,d*deg2rad,az,al);
      az:=rad2deg*rmod(az+pi,pi2);
      al:=al*rad2deg;
      al:=max(al,30); {stay 30 degrees above horizon}
      if n=1 then begin
        azp.az1:=az;
        azp.alt1:=al;
      end
      else begin
        azp.az2:=az;
        azp.alt2:=al;
      end;
  end;
begin
  InitAlignment;
  // Refresh the combobox every time if the observatory latitude change
  for i:=0 to GotoPosition.items.Count-1 do
     GotoPosition.items.Objects[i].free;
  GotoPosition.clear;
  altaz:=TAltAzPosition.Create;
  altaz.az1:=-1; altaz.alt1:=-1;
  altaz.az2:=-1; altaz.alt2:=-1;
  GotoPosition.items.AddObject(rsCustomPositi, altaz);
  if ObsLatitude>30 then
  begin
    {high latitude}
    altaz:=TAltAzPosition.Create;
    GetAE(-5,15,1,altaz); {15 degrees above celestial equator}
    GetAE(-0.5,0,2,altaz);{celestial equator, stay away from zenith for azimuth adjustment. altitude is limited to30 degrees minimum}
    GotoPosition.items.AddObject(rsMeasureES, altaz);

    altaz:=TAltAzPosition.Create;
    GetAE(+5,15,1,altaz); {15 degrees above celestial equator}
    GetAE(+0.5,0,2,altaz);{celestial equator, stay away from zenith for azimuth adjustment}
    GotoPosition.items.AddObject(rsMeasureWS, altaz);
  end
  else
  if ObsLatitude<-30 then {southern hemisphere}
  begin
    {high latitude}
    altaz:=TAltAzPosition.Create;
    GetAE(-5,15,1,altaz); {15 degrees above celestial equator}
    GetAE(-0.5,0,2,altaz);{celestial equator, stay away from zenith for azimuth adjustment. altitude is limited to 30 degrees minimum}
    GotoPosition.items.AddObject(rsMeasureEN, altaz);

    altaz:=TAltAzPosition.Create;
    GetAE(+5,15,1,altaz); {15 degrees above celestial equator}
    GetAE(+0.5,0,2,altaz);{celestial equator, stay away from zenith for azimuth adjustment}
    GotoPosition.items.AddObject(rsMeasureWN, altaz);
  end
  else
  begin
    {Near equator}
    if ObsLatitude>0 then begin
      {North, preference for south horizon}
      GetAE(-4,0,1,altaz);
      GetAE(-0.5,-40,2,altaz);{avoid zenith for azimuth adjustment}
      GotoPosition.items.AddObject(rsMeasureES, altaz);
      altaz:=TAltAzPosition.Create;
      GetAE(4,0,1,altaz);
      GetAE(0.5,-40,2,altaz);{avoid zenith for azimuth adjustment}
      GotoPosition.items.AddObject(rsMeasureWS, altaz);
      altaz:=TAltAzPosition.Create;
      GetAE(-4,0,1,altaz);
      GetAE(-0.5,70,2,altaz);{avoid zenith for azimuth adjustment}
      GotoPosition.items.AddObject(rsMeasureEN, altaz);
      altaz:=TAltAzPosition.Create;
      GetAE(4,25,1,altaz);
      GetAE(0.5,70,2,altaz);{avoid zenith for azimuth adjustment}
      GotoPosition.items.AddObject(rsMeasureWN, altaz);
    end
    else begin
      {South, preference for north horizon}
      altaz:=TAltAzPosition.Create;
      altaz:=TAltAzPosition.Create;
      GetAE(-4,0,1,altaz);
      GetAE(-0.5,70,2,altaz);
      GotoPosition.items.AddObject(rsMeasureEN, altaz);
      altaz:=TAltAzPosition.Create;
      GetAE(4,0,1,altaz);
      GetAE(0.5,70,2,altaz);
      GotoPosition.items.AddObject(rsMeasureWN, altaz);
      altaz:=TAltAzPosition.Create;
      GetAE(-4,0,1,altaz);
      GetAE(-0.5,-40,2,altaz);
      GotoPosition.items.AddObject(rsMeasureES, altaz);
      altaz:=TAltAzPosition.Create;
      GetAE(4,-25,1,altaz);
      GetAE(0.5,-40,2,altaz);
      GotoPosition.items.AddObject(rsMeasureWS, altaz);
    end;
  end;
  FloatSpinEditAz1.Value:=config.GetValue('/PolarAlignment2/Az1',TAltAzPosition(GotoPosition.Items.Objects[1]).az1);
  FloatSpinEditAlt1.Value:=config.GetValue('/PolarAlignment2/Alt1',TAltAzPosition(GotoPosition.Items.Objects[1]).alt1);
  FloatSpinEditAz2.Value:=config.GetValue('/PolarAlignment2/Az2',TAltAzPosition(GotoPosition.Items.Objects[1]).az2);
  FloatSpinEditAlt2.Value:=config.GetValue('/PolarAlignment2/Alt2',TAltAzPosition(GotoPosition.Items.Objects[1]).alt2);
  i:=config.GetValue('/PolarAlignment2/Method',1);
  if i<GotoPosition.Items.Count then
    GotoPosition.ItemIndex:=i
  else
    GotoPosition.ItemIndex:=1;
  PanelCustomPosition.Visible:=(GotoPosition.ItemIndex=0);
end;

procedure Tf_polaralign2.SetLang;
begin
  Caption:=rsPolarAlignme;
  ButtonStart.Caption:=rsStart;
  ButtonAbort1.Caption:=rsCancel;
  ButtonAbort2.Caption:=rsCancel;
  ButtonAbort3.Caption:=rsCancel;
  ButtonNext.Caption:=rsNext;
  ButtonMove.Caption:=rsMove;
  ButtonContinue.Caption:=rsContinue;
  BtnLock.Caption:=rsLockOverlay;
  ButtonClose.Caption:=rsClose;
  label1.Caption:=rsPosition+' 1';
  label2.Caption:=rsPosition+' 2';
  label3.Caption:=rsAz;
  label4.Caption:=rsAz;
  label5.Caption:=rsAlt;
  label6.Caption:=rsAlt;
end;

procedure Tf_polaralign2.SaveConfig;
begin
 config.SetValue('/PolarAlignment2/Az1',FloatSpinEditAz1.Value);
 config.SetValue('/PolarAlignment2/Alt1',FloatSpinEditAlt1.Value);
 config.SetValue('/PolarAlignment2/Az2',FloatSpinEditAz2.Value);
 config.SetValue('/PolarAlignment2/Alt2',FloatSpinEditAlt2.Value);
 config.SetValue('/PolarAlignment2/Method',GotoPosition.ItemIndex);
end;

procedure Tf_polaralign2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  PolarAlignmentOverlay:=false;
  tracemsg('Close polar alignment form');
  if FInProgress then
    AbortAlignment;
  if not FAborted then
    SaveConfig;
  if preview.Loop then preview.BtnLoopClick(nil);
  if Assigned(FonClose) then FonClose(self);
end;

procedure Tf_polaralign2.tracemsg(txt: string);
begin
  if assigned(FonShowMessage) then FonShowMessage('PolarAlign2: '+txt,9);
end;

procedure Tf_polaralign2.msg(txt:string; level: integer);
begin
 if assigned(FonShowMessage) then FonShowMessage('PolarAlign2: '+txt,level);
end;

procedure Tf_polaralign2.Measurement1(ra,de: double);
var mra,mde: double;
begin
  mra:=ra;
  mde:=de;
  LocalToMount(mount.EquinoxJD,mra,mde);
  Instruction.Lines.Add(rsMovingTo+' '+rsRA+','+rsDec +': '+RAToStr(mra)+'  '+DEToStr(mde));
  if not FMount.Slew(mra,mde) then AbortAlignment;
  if FAborted then exit;
  wait(2);

  Instruction.Lines.Add('');
  Instruction.Lines.Add(rsMeasuringFir+', '+rsPleaseWait+'...');
  Application.ProcessMessages;

  TakeExposure;
  if FAborted then exit;
  Solve(1);
  if FAborted then exit;
  if rad2deg*AngularDistance(FRa[1],FDe[1],ra*15*deg2rad,de*deg2rad)>2 then begin
    // reposition if the mount position error is bigger than 2 degrees
    Sync(1);
    if not FMount.Slew(mra,mde) then AbortAlignment;
    if FAborted then exit;
    wait(2);
    TakeExposure;
    if FAborted then exit;
    Solve(1);
    if FAborted then exit;
  end;

  {do not sync in ASCOM simulation since it will move the mount in the Sky simulator.
  Not required for the Sky Simulator Alpaca mount simulation in since it maintains a
  separate ra, dec position for the indication and mount}
  if pos('simulator',LowerCase(camera.DriverInfo))=0 then Sync(1);

  MountPosition(1);
  CurrentStep:=1;
end;

procedure Tf_polaralign2.Measurement2(ra,de: double);
var mra,mde: double;
begin
  mra:=ra;
  mde:=de;
  LocalToMount(mount.EquinoxJD,mra,mde);
  Instruction.Lines.Add(rsMovingTo+' '+rsRA+','+rsDec +': '+RAToStr(mra)+'  '+DEToStr(mde));
  if not FMount.Slew(mra,mde) then AbortAlignment;
  if FAborted then exit;
  wait(2);

  Instruction.Lines.Add('');
  Instruction.Lines.Add(rsMeasuringSec+', '+rsPleaseWait+'...');
  Application.ProcessMessages;

  MountPosition(2);
  TakeExposure;
  if FAborted then exit;
  Solve(2);
  if FAborted then exit;
  CurrentStep:=2;
  Compute;
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
       wait(1); // show solve messages now
       tracemsg('Plate solve successful for image '+inttostr(step));
       tracemsg('Image center J2000: RA='+FormatFloat(f6,cra)+' DEC='+FormatFloat(f6,cde)+' PA='+FormatFloat(f6,pa));
       cra:=cra*15*deg2rad;
       cde:=cde*deg2rad;
       J2000ToApparent(cra,cde);
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
  wait(5); // let time to the mount to update the coordinates
end;

procedure Tf_polaralign2.MountPosition(step: integer);
var tra,tde: double;
begin
  if step=1 then begin
    // use sync coordinates to remove mount error
    tra:=rad2deg*FRa[step]/15;
    tde:=rad2deg*FDe[step];
    LocalToMount(mount.EquinoxJD,tra,tde);
  end
  else begin
    tra:=mount.RA;
    tde:=mount.Dec;
  end;
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

  if abs(determinant)<0.1 then begin
     msg('Warning the calculation determinant is close to zero! Select other celestial locations. Avoid locations with similar hour angles, locations close to the celestial equator and locations whose declinations are close to negatives of each other.',1);
     AbortAlignment;
     exit;
  end;

  if corr_az>0 then ew:=' east of the celestial pole.' else ew:=' west of the celestial pole.';
  if corr_alt>0  then ns:=' above the celestial pole' else ns:=' below the celestial pole';

  msg('Determinant: '+FormatFloat(f2,determinant),1);
  msg('Polar error Az: '+DEToStrShort(rad2deg*abs(corr_az),0)+' '+ew,1);
  msg('Polar error Alt: '+DEToStrShort(rad2deg*abs(corr_alt),0)+' '+ns,1);

  //calculate the Ra, Dec correction for stars in image 2
  corr_ra:=B[0,0]*corr_alt + B[1,0]*corr_az;
  corr_de:=B[0,1]*corr_alt+  B[1,1]*corr_az;

  // same with corr_alt=0 for intermediate point
  corr_rai:=B[1,0]*corr_az;
  corr_dei:=B[1,1]*corr_az;

  tracemsg('Stars in image 2 have to move: '+FormatFloat(f2,rad2deg*(corr_ra/max(0.00000000000000000001,cos(FDe[2])))*60)+' arcminutes in RA and '+FormatFloat(f2,rad2deg*(corr_de)*60)+' arcminutes in DEC by the correction.');
  //Warning avoid the zenith for the second image!! Azimuth changes will not create any change at zenith
end;

procedure Tf_polaralign2.CurrentAdjustement(RA,DE: double);
var B: array[0..1,0..1] of double;
    SIDT2,jd0,h,lat_rad,h_2: double;
    y,m,d: word;
begin
  // compute new ra/dec correction for new telescope position
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
  corr_ra:=B[0,0]*corr_alt + B[1,0]*corr_az;
  corr_de:=B[0,1]*corr_alt+  B[1,1]*corr_az;
  // same with corr_alt=0 for intermediate point
  corr_rai:=B[1,0]*corr_az;
  corr_dei:=B[1,1]*corr_az;
end;

procedure Tf_polaralign2.StartAdjustement;
var cra,cde,eq,pa: double;
begin
  // new measurement after telescope as moved to the position to make the polar axis correction
  StopImageLoop;
  TakeExposure;
  if FAborted then exit;
  f_goto.CheckImageInfo(fits);
  FAstrometry.SolveCurrentImage(true);
  if FAborted then exit;
  if FAstrometry.Busy or (not FAstrometry.LastResult) or
    (not FAstrometry.CurrentCoord(cra,cde,eq,pa)) then begin
    msg(rsFailToResolv,1);
    AbortAlignment;
  end;
  cra:=cra*15*deg2rad;
  cde:=cde*deg2rad;
  J2000ToApparent(cra,cde);
  // adjustement at current position
  CurrentAdjustement(cra,cde);
  tracemsg('Stars in new image have to move: '+FormatFloat(f2,rad2deg*(corr_ra/max(0.00000000000000000001,cos(FDe[2])))*60)+' arcminutes in RA and '+FormatFloat(f2,rad2deg*(corr_de)*60)+' arcminutes in DEC by the correction.');
  ComputeCorrection;
  StartImageLoop;
end;

procedure Tf_polaralign2.ComputeCorrection;
var p: TcdcWCScoord;
    ra,de,eq,pa: double;
    n: integer;
    ok:boolean;
begin
try
  ok:=true;
  if (not FAstrometry.CurrentCoord(ra,de,eq,pa)) then begin
    msg(rsFailToResolv,1);
    AbortAlignment;
  end;
  // start point
  p.ra:=ra*15;
  p.dec:=de;
  n:=cdcwcs_sky2xy(@p,0);
  if n=1 then begin ok:=false; exit; end;
  PolarAlignmentStartx:=p.x;
  PolarAlignmentStarty:=fits.HeaderInfo.naxis2-p.y;

  // intermediate point
  p.ra:=ra*15-rad2deg*corr_rai;
  p.dec:=de-rad2deg*corr_dei;
  n:=cdcwcs_sky2xy(@p,0);
  if n=1 then begin ok:=false; exit; end;
  PolarAlignmentAzx:=p.x;
  PolarAlignmentAzy:=fits.HeaderInfo.naxis2-p.y;

  // end point
  p.ra:=ra*15-rad2deg*corr_ra;
  p.dec:=de-rad2deg*corr_de;
  n:=cdcwcs_sky2xy(@p,0);
  if n=1 then begin ok:=false; exit; end;
  PolarAlignmentEndx:=p.x;
  PolarAlignmentEndy:=fits.HeaderInfo.naxis2-p.y;

  PolarAlignmentOverlayOffsetX:=0;
  PolarAlignmentOverlayOffsetY:=0;
  PolarAlignmentLock:=false;
  PolarAlignmentOverlay:=true;

finally
  if not ok then begin
    msg('WCS error, offscale',1);
    AbortAlignment;
  end;
end;
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

procedure Tf_polaralign2.ButtonAbortClick(Sender: TObject);
begin
   AbortAlignment;
end;

procedure Tf_polaralign2.ButtonCloseClick(Sender: TObject);
begin
  close;
end;

procedure Tf_polaralign2.decode_combobox(out caz1,calt1,caz2,calt2: double);
begin
try
  with GotoPosition.Items.Objects[GotoPosition.ItemIndex] as TAltAzPosition do begin
    caz1:=az1;
    caz2:=az2;
    calt1:=alt1;
    calt2:=alt2;
  end;
except
  caz1:=1000;
end;
end;

procedure Tf_polaralign2.ButtonStartClick(Sender: TObject);
begin
  FInProgress:=true;
  if GotoPosition.ItemIndex=0 then begin
    with GotoPosition.Items.Objects[0] as TAltAzPosition do begin
      // update custom position
      az1:=FloatSpinEditAz1.Value;
      alt1:=FloatSpinEditAlt1.Value;
      az2:=FloatSpinEditAz2.Value;
      alt2:=FloatSpinEditAlt2.Value;
    end;
  end;
  Application.QueueAsyncCall(@AutoMeasurementAsync,0)
end;

procedure Tf_polaralign2.AutoMeasurementAsync(Data: PtrInt);
var
  az1,alt1,az2,alt2,ra,de : double;
begin
 try
 // 1
    PageControl1.ActivePage:=TabSheetAuto;
    Instruction.Clear;
    decode_combobox(az1,alt1,az2,alt2);
    if az1>999 then close;{something wrong}

    Instruction.Lines.Add(rsMovingToFirs+', '+rsPleaseWait+'...');
    Instruction.Lines.Add(rsMovingTo+' '+rsAz+','+rsAlt +': '+ DEToStr(az1)+'  '+DEToStr(alt1));
    cmdHz2Eq(az1,alt1,ra,de);
    Measurement1(ra,de);
    if FAborted then exit;

 // 2
    Instruction.Clear;
    Instruction.Lines.Add(rsMovingToSeco+', '+rsPleaseWait+'...');
    Instruction.Lines.Add(rsMovingTo+' '+rsAz+','+rsAlt +': '+ DEToStr(az2)+'  '+DEToStr(alt2));
    cmdHz2Eq(az2,alt2,ra,de);
    Measurement2(ra,de);
    if FAborted then exit;

 // show correction
    ComputeCorrection;
    if FAborted then exit;
    PageControl1.ActivePage:=TabSheetMove1;
    Instruction.Clear;
    Instruction.Lines.Add(rsInTheNextScr);
    Instruction.Lines.Add(rsIfaBrightsta);
    Instruction.Lines.Add('');
    Instruction.Lines.Add(rsIfNoBrightSt);
    StartImageLoop;

 except
   on E: Exception do begin
      msg(E.Message,1);
      Close;
   end;
 end;
end;

procedure Tf_polaralign2.ButtonNextClick(Sender: TObject);
var txt: string;
begin
  Instruction.Clear;
  Instruction.Lines.Add(rsHorizontalCo);
  if corr_az>0 then
     txt:=rsMoveWestBy
  else
     txt:=rsMoveEastBy;
  txt:=txt+DEToStrShort(rad2deg*abs(corr_az),0);
  Instruction.Lines.Add(txt);
  Instruction.Lines.Add(rsVerticalCorr);
  if corr_alt>0 then
     txt:=rsMoveDownBy
  else
     txt:=rsMoveUpBy;
  txt:=txt+DEToStrShort(rad2deg*abs(corr_alt),0);
  Instruction.Lines.Add(txt);
  Instruction.Lines.Add('');
  Instruction.Lines.Add(rsMoveTheGreen);
  Instruction.Lines.Add(rsThenAdjustTh);
  Instruction.Lines.Add(rsForGuidanceA);
  Instruction.Lines.Add(rsYouCanCloseT);
  PageControl1.ActivePage:=TabSheetAdjust;
  FInProgress:=false;
end;

procedure Tf_polaralign2.GotoPositionChange(Sender: TObject);
begin
   PanelCustomPosition.Visible:=(GotoPosition.ItemIndex=0);
end;

procedure Tf_polaralign2.ButtonMoveClick(Sender: TObject);
begin
  Instruction.Clear;
  Instruction.Lines.Add(rsMoveTheTeles);
  Instruction.Lines.Add(rsWhenReadyCli+': '+rsContinue);
  Instruction.Lines.Add('');
  PageControl1.ActivePage:=TabSheetMove2;
  PolarAlignmentOverlay:=false;
  StartImageLoop;
end;

procedure Tf_polaralign2.ButtonContinueClick(Sender: TObject);
begin
  Instruction.Clear;
  Instruction.Lines.Add(rsMeasuringThe+', '+rsPleaseWait+'...');
  Application.ProcessMessages;
  StartAdjustement;
  if FAborted then exit;
  ButtonNextClick(Sender);
end;

procedure Tf_polaralign2.InitAlignment;
var i: integer;
begin
  Instruction.Clear;
  Instruction.Lines.Add(rsSelectOneOfT);
  Instruction.Lines.Add('');
  Instruction.Lines.Add(' - '+rsPredefined);
  Instruction.Lines.Add(' - '+rsCustom);
  Instruction.Lines.Add('');
  Instruction.Lines.Add(rsWhenReadyCli+': '+rsStart);
  PageControl1.ActivePage:=TabSheetStart;
  PolarAlignmentOverlay:=false;
  PolarAlignmentLock:=false;
  BtnLock.Checked:=false;
  StopImageLoop;
  FInProgress:=false;
  FAborted:=false;
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
  if FAborted then exit; // do not do this action multiple time
  FAborted:=true;
  PolarAlignmentOverlay:=false;
  if FInProgress then begin
    if Mount.MountSlewing then Mount.AbortMotion;
    if preview.Loop then preview.BtnLoopClick(nil);
    Fcamera.AbortExposure;
    if Astrometry.Busy then Astrometry.StopAstrometry;
  end;
  FInProgress:=false;
  msg(rsCancelPolarA,1);
  close;
end;

end.

