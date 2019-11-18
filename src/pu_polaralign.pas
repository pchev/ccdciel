unit pu_polaralign;

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

uses u_translation, u_utils, u_global, fu_preview, cu_fits, cu_astrometry, cu_mount, fu_visu, indiapi,
  math, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, ExtCtrls, CheckLst, Spin;

type

  { Tf_polaralign }

  Tf_polaralign = class(TForm)
    BtnClose: TButton;
    BtnStart: TButton;
    BtnCancel: TButton;
    BtnContinue: TButton;
    ExposeList: TCheckListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LabelMsg1: TLabel;
    LabelMsg2: TLabel;
    LabelMsg3: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    PanelAutoSlew: TPanel;
    MountSlewing: TRadioGroup;
    Panel6: TPanel;
    PanelManualSlew: TPanel;
    RotDir: TRadioGroup;
    RotAngle: TSpinEdit;
    TabSheetCompute: TTabSheet;
    TabSheetExpose: TTabSheet;
    TabSheetStart: TTabSheet;
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnContinueClick(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure ExposeListItemClick(Sender: TObject; Index: integer);
    procedure ExposeListSelectionChange(Sender: TObject; User: boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MountSlewingClick(Sender: TObject);
  private
    FFits: TFits;
    Fpreview: Tf_preview;
    Fvisu: Tf_visu;
    FAstrometry: TAstrometry;
    FMount: T_mount;
    FExposeStep:Integer;
    FInProgress: boolean;
    FTerminate: boolean;
    FonShowMessage: TNotifyMsg;
    Fx, Fy: array[1..3] of double;
    FSidtimStart: double;
    FOffsetAz, FOffsetH, FCameraRotation: double;
    Fstartx,Fstarty,Fendx,Fendy:double;
    procedure msg(txt:string; level: integer);
    procedure InitAlignment;
    procedure AbortAlignment;
    procedure DoStart(Data: PtrInt);
    procedure DoCompute(Data: PtrInt);
    procedure Proj(ar, de: double; out X, Y: double);
    procedure InvProj(x, y: double; out ar, de: double);
    procedure TakeExposure;
    procedure Solve(step: integer);
    procedure Rotate;
  public
    procedure SetLang;
    property Fits: TFits read FFits write FFits;
    property Preview:Tf_preview read Fpreview write Fpreview;
    property Visu: Tf_visu read Fvisu write Fvisu;
    property Astrometry: TAstrometry read FAstrometry write FAstrometry;
    property Mount: T_mount read Fmount write Fmount;
    property OffsetH: double read FOffsetH;
    property OffsetAz: double read FOffsetAz;
    property CameraRotation: double read FCameraRotation;
    property StartX: double read Fstartx;
    property StartY: double read Fstarty;
    property EndX: double read Fendx;
    property EndY: double read Fendy;
    property onShowMessage: TNotifyMsg read FonShowMessage write FonShowMessage;

  end;

var
  f_polaralign: Tf_polaralign;

implementation

{$R *.lfm}

/// Line intersection copied from Bgrabitmap but with double precision points
type
  (* A double precision point *)
  TPointDouble = packed record x, y: double;
  end;
  {* Defition of a line in the euclidian plane }
  TLineDouble = record
    {** Some point in the line }
    origin: TPointDouble;
    {** Vector indicating the direction }
    dir: TPointDouble;
  end;

  function IntersectLine(line1, line2: TLineDouble; out parallel: boolean): TPointDouble;
  var divFactor: double;
  begin
    parallel := false;
    //if lines are parallel
    if ((line1.dir.x = line2.dir.x) and (line1.dir.y = line2.dir.y)) or
       ((abs(line1.dir.y) < 1e-6) and (abs(line2.dir.y) < 1e-6)) then
    begin
         parallel := true;
         //return the center of the segment between line origins
         result.x := (line1.origin.x+line2.origin.x)/2;
         result.y := (line1.origin.y+line2.origin.y)/2;
    end else
    if abs(line1.dir.y) < 1e-6 then //line1 is horizontal
    begin
         result.y := line1.origin.y;
         result.x := line2.origin.x + (result.y - line2.origin.y)
                 /line2.dir.y*line2.dir.x;
    end else
    if abs(line2.dir.y) < 1e-6 then //line2 is horizontal
    begin
         result.y := line2.origin.y;
         result.x := line1.origin.x + (result.y - line1.origin.y)
                 /line1.dir.y*line1.dir.x;
    end else
    begin
         divFactor := line1.dir.x/line1.dir.y - line2.dir.x/line2.dir.y;
         if abs(divFactor) < 1e-6 then //almost parallel
         begin
              parallel := true;
              //return the center of the segment between line origins
              result.x := (line1.origin.x+line2.origin.x)/2;
              result.y := (line1.origin.y+line2.origin.y)/2;
         end else
         begin
           result.y := (line2.origin.x - line1.origin.x +
                    line1.origin.y*line1.dir.x/line1.dir.y -
                    line2.origin.y*line2.dir.x/line2.dir.y)
                    / divFactor;
           result.x := line1.origin.x + (result.y - line1.origin.y)
                   /line1.dir.y*line1.dir.x;
         end;
    end;
  end;

{ Tf_polaralign }

procedure Tf_polaralign.SetLang;
begin
   Caption:=rsPolarAlignme;
   BtnStart.Caption:=rsStart;
   BtnCancel.Caption:=rsCancel;
   BtnClose.Caption:=rsClose;
   BtnContinue.Caption:=rsContinue;
   Label5.Caption:=rsPolarAlignme2;
   Label1.Caption:=Format(rsMakeAFirstPo, [crlf, crlf]);
   MountSlewing.Caption:=rsMovingMount;
   MountSlewing.Items[0]:=rsAutomatic;
   MountSlewing.Items[1]:=rsManual;
   Label3.Caption:=rsWhenAskedMov;
   RotDir.Caption:=rsMountRotatio2;
   RotDir.Items[0]:=rsWest;
   RotDir.Items[1]:=rsEast;
   Label2.Caption:=rsRotateBy;
   Label4.Caption:=rsDegree;
   ExposeList.Items[0]:=Format(rsExposureS, ['1']);
   ExposeList.Items[1]:=Format(rsPlateSolveEx, ['1']);
   ExposeList.Items[2]:=rsRotateTelesc;
   ExposeList.Items[3]:=Format(rsExposureS, ['2']);
   ExposeList.Items[4]:=Format(rsPlateSolveEx, ['2']);
   ExposeList.Items[5]:=rsRotateTelesc;
   ExposeList.Items[6]:=Format(rsExposureS, ['3']);
   ExposeList.Items[7]:=Format(rsPlateSolveEx, ['3']);
end;

procedure Tf_polaralign.FormCreate(Sender: TObject);
begin
  SetLang;
end;

procedure Tf_polaralign.FormShow(Sender: TObject);
begin
  InitAlignment;
end;

procedure Tf_polaralign.MountSlewingClick(Sender: TObject);
begin
  if FMount.Status<>devConnected then MountSlewing.ItemIndex:=1;
  PanelAutoSlew.Visible:=(MountSlewing.ItemIndex=0);
  PanelManualSlew.Visible:=not PanelAutoSlew.Visible;
end;

procedure Tf_polaralign.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  PolarAlignmentOverlay:=false;
  if preview.Loop then preview.BtnLoopClick(nil);
end;

procedure Tf_polaralign.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if FInProgress then begin
    CanClose:=MessageDlg(rsDoYouWantToC, mtConfirmation, mbYesNo, 0)=mrYes;
    if CanClose then begin
       FTerminate:=true;
       msg(rsCancelPolarA, 1);
       if FInProgress then begin
         if Mount.MountSlewing then Mount.AbortMotion;
         if Astrometry.Busy then Astrometry.StopAstrometry;
       end;
       FInProgress:=false;
    end;
  end
  else
    CanClose:=true;
end;

procedure Tf_polaralign.msg(txt:string; level: integer);
begin
 LabelMsg1.Caption:=txt;
 LabelMsg2.Caption:=txt;
 LabelMsg3.Caption:=txt;
 if assigned(FonShowMessage) then FonShowMessage(txt,level);
end;

procedure Tf_polaralign.InitAlignment;
begin
  LabelMsg1.Caption:='';
  LabelMsg2.Caption:='';
  LabelMsg3.Caption:='';
  PageControl1.ActivePageIndex:=0;
  MountSlewing.ItemIndex:=0;
  MountSlewingClick(nil);
  FInProgress:=false;
  FTerminate:=false;
end;

procedure Tf_polaralign.AbortAlignment;
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

procedure Tf_polaralign.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_polaralign.BtnCancelClick(Sender: TObject);
begin
  AbortAlignment;
end;

procedure Tf_polaralign.BtnStartClick(Sender: TObject);
begin
  Application.QueueAsyncCall(@DoStart,0);
end;

procedure Tf_polaralign.DoStart(Data: PtrInt);
var i: integer;
begin
  if FInProgress then exit;
  // Start the measurement
  FInProgress:=true;
  FSidtimStart:=CurrentSidTim;
  CancelAutofocus:=false;
  memo1.Clear;
  BtnContinue.Visible:=false;
  LabelMsg2.Caption:=rsPleaseWaitUn;
  PageControl1.ActivePage:=TabSheetExpose;
  for i:=0 to 7 do
    ExposeList.State[i]:=cbUnchecked;
  for i:=0 to 7 do begin
    if FTerminate then exit;
    FExposeStep:=i;
    ExposeList.Selected[FExposeStep]:=true;
    case FExposeStep of
      0: TakeExposure;
      1: Solve(1);
      2: Rotate;
      3: TakeExposure;
      4: solve(2);
      5: Rotate;
      6: TakeExposure;
      7: solve(3);
    end;
    ExposeList.State[FExposeStep]:=cbChecked;
  end;
  FExposeStep:=8;
  wait(2);
  Application.QueueAsyncCall(@DoCompute,0);
end;

procedure Tf_polaralign.ExposeListItemClick(Sender: TObject; Index: integer);
begin
  ExposeList.Checked[index]:=index<FExposeStep
end;

procedure Tf_polaralign.ExposeListSelectionChange(Sender: TObject; User: boolean);
begin
  ExposeList.Selected[min(ExposeList.Items.Count-1,FExposeStep)]:=true;
end;

procedure Tf_polaralign.TakeExposure;
var exp:double;
    bin: integer;
begin
// Start an exposure
fits.SetBPM(bpm,bpmNum,bpmX,bpmY,bpmAxis);
fits.DarkOn:=true;
exp:=config.GetValue('/PrecSlew/Exposure',10.0);
bin:=config.GetValue('/PrecSlew/Binning',1);
if not preview.ControlExposure(exp,bin,bin,LIGHT,ReadoutModeAstrometry) then begin
    msg(rsExposureFail,1);
    AbortAlignment;
end;
end;

procedure Tf_polaralign.Proj(ar, de: double; out X, Y: double);
var
  r, hh, ac, dc, s1, s2, s3, c1, c2, c3: extended;
begin
  //Coordinates projection in plane centered on the pole
  //X axis is parallel to the horizon at the time the procedure is started
  // center of the projection in DEC
  dc:=sgn(ObsLatitude)*(pid2-secarc);
  // center of the projection in RA
  ac:=rmod(FSidtimStart+pi,pi2);
  hh := ar - ac;
  sincos(dc, s1, c1);
  sincos(de, s2, c2);
  sincos(hh, s3, c3);
  r := s1 * s2 + c2 * c1 * c3;     // cos the
  // arc
  if r > 1 then
    r := 1;
  r := arccos(r);
  if r <> 0 then
    r := (r / sin(r));
  x := -r * c2 * s3;
  y := r * (s2 * c1 - c2 * s1 * c3);
end;

procedure Tf_polaralign.InvProj(x, y: double; out ar, de: double);
var
  ac, dc, a, r, hh, s1, c1, s2, c2, s3, c3: extended;
begin
  //Return ra,dec from the position in projection plane
  // center of the projection in DEC
  dc:=sgn(ObsLatitude)*(pid2-secarc);
  // center of the projection in RA
  ac:=rmod(FSidtimStart+pi,pi2);
  r := (pid2 - sqrt(x * x + y * y));
  a := arctan2(x, y);
  sincos(a, s1, c1);
  sincos(dc, s2, c2);
  sincos(r, s3, c3);
  de := (arcsin(s2 * s3 - c2 * c3 * c1)) + 1E-9;
  hh := (arctan2((c3 * s1), (c2 * s3 + s2 * c3 * c1)));
  ar := ac - hh - 1E-9;
  ar := rmod(2*pi2 + ar, pi2);
end;

procedure Tf_polaralign.Solve(step: integer);
var cra,cde,eq,pa,x,y,dx,dy: double;
begin
  // solve the current image
  FAstrometry.SolveCurrentImage(true);
  if (not FAstrometry.Busy)and FAstrometry.LastResult then begin
     if FAstrometry.CurrentCoord(cra,cde,eq,pa) then begin
       cra:=cra*15*deg2rad;
       cde:=cde*deg2rad;
       J2000ToApparent(cra,cde);
       Proj(cra,cde,x,y);
       // store this image result
       Fx[step]:=rad2deg*x;
       Fy[step]:=rad2deg*y;
       if step>1 then begin
         dx:=abs(Fx[step]-Fx[step-1]);
         dy:=abs(Fy[step]-Fy[step-1]);
         if sqrt(dx*dx+dy*dy)<(10/3600) then begin
           msg(rsTheMountDoNo, 1);
           AbortAlignment;
         end;
       end;
       // store position angle
       FCameraRotation:=pa;
     end;
  end
  else begin
    msg(rsFailToResolv,1);
    AbortAlignment;
  end;
end;

procedure Tf_polaralign.Rotate;
var cra,cde: double;
begin
  if MountSlewing.ItemIndex=0 then begin
    // Rotate mount in RA by configured angle
    cra:=FMount.RA;
    cde:=FMount.Dec;
    if RotDir.ItemIndex=0 then
      cra:=cra-RotAngle.Value/15
    else
      cra:=cra+RotAngle.Value/15;
    cra:=rmod(cra+24,24);
    if not FMount.Slew(cra,cde) then begin
      msg(rsTelescopeSle3,1);
      AbortAlignment;
    end;
  end
  else begin
    LabelMsg2.Caption:=rsMoveTheMount;
    BtnContinue.Visible:=True;
    while BtnContinue.Visible do begin
      Wait(2);
    end;
  end;
end;

procedure Tf_polaralign.BtnContinueClick(Sender: TObject);
begin
  BtnContinue.Visible:=False;
end;

procedure Tf_polaralign.DoCompute(Data: PtrInt);
var bisect1, bisect2, bisect3: TLineDouble;
    rotcenter,rotcenter1,rotcenter2,rotcenter3:TPointDouble;
    parallel1,parallel2,parallel3: boolean;
    err,errx,erry,poleoffset,poleH,poleRefraction: Double;
    txt: string;
    n: integer;
    p: TcdcWCScoord;
    rotRa, rotDec, ra, de: double;
begin
  // Compute the polar offset from the measurement
  PageControl1.ActivePage:=TabSheetCompute;
  // bisector of the first and second measurement
  bisect1.origin.x:=(Fx[1]+Fx[2])/2;
  bisect1.origin.y:=(Fy[1]+Fy[2])/2;
  bisect1.dir.x:=1;
  bisect1.dir.y:=-1/((fy[2]-fy[1])/(fx[2]-fx[1]));
  // bisector of the second and third measurement
  bisect2.origin.x:=(Fx[2]+Fx[3])/2;
  bisect2.origin.y:=(Fy[2]+Fy[3])/2;
  bisect2.dir.x:=1;
  bisect2.dir.y:=-1/((fy[3]-fy[2])/(fx[3]-fx[2]));
  // bisector of the first and third measurement
  bisect3.origin.x:=(Fx[1]+Fx[3])/2;
  bisect3.origin.y:=(Fy[1]+Fy[3])/2;
  bisect3.dir.x:=1;
  bisect3.dir.y:=-1/((fy[3]-fy[1])/(fx[3]-fx[1]));
  // intersection of two bisector is the center of rotation of the mount
  rotcenter1:=IntersectLine(bisect1,bisect2,parallel1);
  rotcenter2:=IntersectLine(bisect2,bisect3,parallel2);
  rotcenter3:=IntersectLine(bisect1,bisect3,parallel3);
  if parallel1 or parallel2 or parallel3 then begin
    FOffsetAz:=0;
    FOffsetH:=0;
    // almost impossible condition, do not translate the text
    txt:='Cannot compute the center of rotation of the mount';
    Memo1.Lines.Add(txt);
    msg(txt,1);
    exit;
  end;
  // mean center and error
  rotcenter.x:=(rotcenter1.x+rotcenter2.x+rotcenter3.x)/3;
  rotcenter.y:=(rotcenter1.y+rotcenter2.y+rotcenter3.y)/3;
  errx:=maxvalue([abs(rotcenter.x-rotcenter1.x),abs(rotcenter.x-rotcenter2.x),abs(rotcenter.x-rotcenter3.x)]);
  erry:=maxvalue([abs(rotcenter.y-rotcenter1.y),abs(rotcenter.y-rotcenter2.y),abs(rotcenter.y-rotcenter3.y)]);
  // Position of pole corrected for refraction
  poleH:=deg2rad*abs(ObsLatitude);        // geometric
  Refraction(poleH,true);                 // refracted
  poleH:=rad2deg*poleH;
  poleRefraction:=poleH-abs(ObsLatitude); // correction
  // the offset in degree
  FOffsetAz:=rotcenter.x;
  FOffsetH:=rotcenter.y-poleRefraction;
  poleoffset:=sqrt(FOffsetAz*FOffsetAz+FOffsetH*FOffsetH);
  err:=sqrt(errx*errx+erry*erry);
  // position of rotation axis
  InvProj(deg2rad*FOffsetAz,deg2rad*FOffsetH,rotRa,rotDec);
  rotRa:=rad2deg*rotRa/15;
  rotDec:=rad2deg*rotDec;
  // display result
  Memo1.Lines.Add(rsComputationR);
  Memo1.Lines.Add('');
  Memo1.Lines.Add(rsMountRotatio);
  Memo1.Lines.Add(rsRA+' : '+RAToStr(rotRa)+'  '+rsDec+' : '+DEToStr(rotDec));
  Memo1.Lines.Add(rsTotalPolarEr);
  Memo1.Lines.Add(DEToStrShort(poleoffset,0)+' +/- '+DEToStrShort(err,0));
  Memo1.Lines.Add('');
  Memo1.Lines.Add(rsHorizontalCo);
  if FOffsetAz>0 then txt:=rsMoveWestBy
                 else txt:=rsMoveEastBy;
  txt:=txt+DEToStrShort(abs(FOffsetAz),0);
  Memo1.Lines.Add(txt);
  Memo1.Lines.Add(rsVerticalCorr);
  if FOffsetH>0 then txt:=rsMoveUpBy
                else txt:=rsMoveDownBy;
  txt:=txt+DEToStrShort(abs(FOffsetH),0);
  Memo1.Lines.Add(txt);
  Memo1.Lines.Add('');
  // vector to new position
  // From the pole
  ra:=0;
  de:=sgn(ObsLatitude)*(pid2-secarc);
  ApparentToJ2000(ra,de);
  p.ra:=rad2deg*ra;
  p.dec:=rad2deg*de;
  n:=cdcwcs_sky2xy(@p,0);
  Fstartx:=p.x;
  Fstarty:=FFits.HeaderInfo.naxis2-p.y;
  if n=1 then begin
    txt:='Pole is outside the image coordinates range, point the telescope closer to the pole.';
    msg(txt,1);
    exit;
  end;
  // to mount axis
  ra:=deg2rad*rotRa*15;
  de:=deg2rad*rotDec;
  ApparentToJ2000(ra,de);
  p.ra:=rad2deg*ra;
  p.dec:=rad2deg*de;
  n:=cdcwcs_sky2xy(@p,0);
  Fendx:=p.x;
  Fendy:=FFits.HeaderInfo.naxis2-p.y;
  if n=1 then begin
    txt:='Mount axis is outside the image coordinates range, point the telescope closer to the pole.';
    msg(txt,1);
    exit;
  end;
  // line origin centered on screen
  PolarAlignmentOverlayOffsetX:=(FFits.HeaderInfo.naxis1 div 2)-Fstartx;
  PolarAlignmentOverlayOffsetY:=(FFits.HeaderInfo.naxis2 div 2)-Fstarty;
  // draw offset overlay
  PolarAlignmentOverlay:=true;
  Memo1.Lines.Add(rsMoveTheGreen);
  Memo1.Lines.Add(rsThenAdjustTh);
  Memo1.Lines.Add(rsYouCanCloseT);
  // start image loop
  FVisu.BtnZoomAdjust.Click;
  preview.Exposure:=config.GetValue('/PrecSlew/Exposure',1.0);
  if not preview.Loop then preview.BtnLoopClick(nil);
  FInProgress:=false;
end;

end.

