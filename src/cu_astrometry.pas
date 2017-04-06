unit cu_astrometry;

{$mode objfpc}{$H+}

{
Copyright (C) 2015 Patrick Chevalley

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

uses  u_global, u_utils, fu_preview, cu_astrometry_engine, cu_mount, cu_camera, cu_fits, indiapi,
      LCLIntf, math, Forms, LazFileUtils, Classes, SysUtils, ExtCtrls;

type

TAstrometry = class(TComponent)
  private
    engine: TAstrometry_engine;
    Fpreview:Tf_preview;
    Fterminatecmd: TNotifyEvent;
    FonStartAstrometry: TNotifyEvent;
    FonEndAstrometry: TNotifyEvent;
    FonShowMessage: TNotifyMsg;
    FBusy, FSlewBusy, FLastResult: Boolean;
    FLastSlewErr: double;
    Fmount: T_mount;
    Fcamera: T_camera;
    FFits: TFits;
    FResolverName: string;
    logfile,solvefile,savefile: string;
    Xslew, Yslew: integer;
    AstrometryTimeout: double;
    TimerAstrometrySolve, TimerAstrometrySync, TimerAstrometrySlewScreenXY : TTimer;
    procedure AstrometrySolveonTimer(Sender: TObject);
    procedure AstrometrySynconTimer(Sender: TObject);
    procedure AstrometrySlewScreenXYonTimer(Sender: TObject);
    procedure msg(txt:string);
    function WaitBusy(Timeout:double=60): boolean;
    procedure AstrometrySolve(Sender: TObject);
    procedure AstrometrySync(Sender: TObject);
    procedure AstrometrySlewScreenXY(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);override;
    function StartAstrometry(infile,outfile: string; terminatecmd:TNotifyEvent): boolean;
    procedure StopAstrometry;
    procedure AstrometryDone;
    function  CurrentCoord(out cra,cde,eq: double):boolean;
    procedure SolveCurrentImage(wait: boolean);
    procedure SyncCurrentImage(wait: boolean);
    procedure SlewScreenXY(x,y: integer);
    function PrecisionSlew(ra,de,prec,exp:double; binx,biny,method,maxslew: integer; out err: double):boolean;
    property Busy: Boolean read FBusy;
    property SlewBusy: Boolean read FSlewBusy;
    property LastSlewErr: double read FLastSlewErr;
    property LastResult: Boolean read FLastResult;
    property ResultFile: string read savefile;
    property Resolver: string read FResolverName;
    property Mount: T_mount read Fmount write Fmount;
    property Camera: T_camera read Fcamera write Fcamera;
    property Fits: TFits read FFits write FFits;
    property preview:Tf_preview read Fpreview write Fpreview;
    property onShowMessage: TNotifyMsg read FonShowMessage write FonShowMessage;
    property onAstrometryStart: TNotifyEvent read FonStartAstrometry write FonStartAstrometry;
    property onAstrometryEnd: TNotifyEvent read FonEndAstrometry write FonEndAstrometry;
end;

implementation

constructor TAstrometry.Create(AOwner: TComponent);
begin
  Inherited create(AOwner);
  FBusy:=false;
  FSlewBusy:=false;
  FLastResult:=false;
  FLastSlewErr:=0;
  AstrometryTimeout:=60;
  TimerAstrometrySolve:=TTimer.Create(self);
  TimerAstrometrySolve.Enabled:=false;
  TimerAstrometrySolve.Interval:=100;
  TimerAstrometrySolve.OnTimer:=@AstrometrySolveonTimer;
  TimerAstrometrySync:=TTimer.Create(self);
  TimerAstrometrySync.Enabled:=false;
  TimerAstrometrySync.Interval:=100;
  TimerAstrometrySync.OnTimer:=@AstrometrySynconTimer;
  TimerAstrometrySlewScreenXY:=TTimer.Create(self);
  TimerAstrometrySlewScreenXY.Enabled:=false;
  TimerAstrometrySlewScreenXY.Interval:=100;
  TimerAstrometrySlewScreenXY.OnTimer:=@AstrometrySlewScreenXYonTimer;
end;

procedure TAstrometry.msg(txt:string);
begin
 if assigned(FonShowMessage) then FonShowMessage(txt);
end;

function TAstrometry.WaitBusy(Timeout:double=60): boolean;
var endt: TDateTime;
begin
  endt:=now+Timeout/secperday;
  while (FBusy)and(now<endt) do begin
     sleep(100);
     Application.ProcessMessages;
  end;
  result:=not FBusy;
end;

function TAstrometry.StartAstrometry(infile,outfile: string; terminatecmd:TNotifyEvent): boolean;
var pixsize,pixscale,telescope_focal_length,tolerance,MinRadius,ra,de: double;
    n,iwidth:integer;
    info: TcdcWCSinfo;
begin
 if (not FBusy) then begin
   Fterminatecmd:=terminatecmd;
   n:=cdcwcs_initfitsfile(PChar(infile),0);
   ra:=NullCoord;
   de:=NullCoord;
   pixscale:=NullCoord;
   iwidth:=1000;
   if n=0 then begin
     n:=cdcwcs_getinfo(addr(info),0);
     if n=0 then begin
       ra:=info.cra;
       de:=info.cdec;
       iwidth:=info.wp;
       pixscale:=info.secpix;
     end;
   end;
   if (ra=NullCoord)or(de=NullCoord) then begin
       msg('Cannot find approximate coordinates for this image.'+crlf+'The astrometry resolution may take a very long time.');
   end;
   FLastResult:=false;
   logfile:=ChangeFileExt(infile,'.log');
   solvefile:=ChangeFileExt(infile,'.solved');
   savefile:=outfile;
   DeleteFileUTF8(outfile);
   DeleteFileUTF8(solvefile);
   engine:=TAstrometry_engine.Create;
   engine.Resolver:=config.GetValue('/Astrometry/Resolver',ResolverAstrometryNet);
   FResolverName:=ResolverName[engine.Resolver];
   engine.CygwinPath:=config.GetValue('/Astrometry/CygwinPath','C:\cygwin');
   engine.ElbrusFolder:=config.GetValue('/Astrometry/ElbrusFolder','');
   engine.ElbrusUnixpath:=config.GetValue('/Astrometry/ElbrusUnixpath','');
   engine.LogFile:=logfile;
   engine.InFile:=infile;
   engine.OutFile:=outfile;
   tolerance:=config.GetValue('/Astrometry/ScaleTolerance',0.1);
   MinRadius:=config.GetValue('/Astrometry/MinRadius',15.0);
   AstrometryTimeout:=config.GetValue('/Astrometry/Timeout',60.0);
   if pixscale=NullCoord then begin
     if config.GetValue('/Astrometry/PixelSizeFromCamera',true)
     then
        pixsize:=camera.PixelSizeX * camera.BinX
     else
        pixsize:=config.GetValue('/Astrometry/PixelSize',5.0);
     if config.GetValue('/Astrometry/FocaleFromTelescope',true)
     then
        telescope_focal_length:=mount.FocaleLength
     else
        telescope_focal_length:=config.GetValue('/Astrometry/FocaleLength',1000.0);
     if (pixsize>0)and(telescope_focal_length>0)  then begin
        pixscale:=3600*rad2deg*arctan(pixsize/1000/telescope_focal_length);
     end;
   end;
   if pixscale<>NullCoord then begin
      engine.scalelow:=(1-tolerance)*pixscale;
      engine.scalehigh:=(1+tolerance)*pixscale;
   end;
   engine.downsample:=config.GetValue('/Astrometry/DownSample',4);
   engine.objs:=config.GetValue('/Astrometry/SourcesLimit',150);
   engine.plot:=config.GetValue('/Astrometry/Plot',false);
   engine.OtherOptions:=config.GetValue('/Astrometry/OtherOptions','');
   engine.ra:=ra;
   engine.de:=de;
   engine.radius:=max(MinRadius,pixscale*iwidth/3600);
   engine.timeout:=AstrometryTimeout;
   FBusy:=true;
   engine.Resolve;
   msg('Resolving using '+ResolverName[engine.Resolver]+' ...');
   if Assigned(FonStartAstrometry) then FonStartAstrometry(self);
   result:=true;
 end else begin
   result:=false;
 end;
end;

procedure TAstrometry.AstrometryDone;
begin
 if FileExistsUTF8(savefile) and FileExistsUTF8(solvefile) then
   FLastResult:=true
 else
   FLastResult:=false;
 if Assigned(Fterminatecmd) then Fterminatecmd(self);
 FBusy:=false;
 if Assigned(FonEndAstrometry) then FonEndAstrometry(self);
 Fterminatecmd:=nil;
end;

procedure TAstrometry.StopAstrometry;
begin
  if FBusy then begin
    engine.Stop;
    msg('Stop astrometry resolver.');
  end;
end;

function TAstrometry.CurrentCoord(out cra,cde,eq: double):boolean;
var n,m: integer;
    i: TcdcWCSinfo;
    c: TcdcWCScoord;
begin
  result:=false;
  if cdcwcs_xy2sky<>nil then begin
    n:=cdcwcs_getinfo(addr(i),0);
    if (n=0)and(i.secpix<>0) then begin
      c.x:=0.5+i.wp/2;
      c.y:=0.5+i.hp/2;
      m:=cdcwcs_xy2sky(@c,0);
      if m=0 then begin
        cra:=c.ra/15;
        cde:=c.dec;
        eq:=2000;
        result:=true;
      end;
    end;
  end;
end;

procedure TAstrometry.SolveCurrentImage(wait: boolean);
begin
  if (not FBusy) and (FFits.HeaderInfo.naxis>0) then begin
   if not fits.HeaderInfo.solved then begin
    FFits.SaveToFile(slash(TmpDir)+'ccdcieltmp.fits');
    StartAstrometry(slash(TmpDir)+'ccdcieltmp.fits',slash(TmpDir)+'ccdcielsolved.fits',@AstrometrySolve);
    if wait then WaitBusy(AstrometryTimeout+30);
   end;
  end;
end;

procedure TAstrometry.AstrometrySolve(Sender: TObject);
begin
  TimerAstrometrySolve.Enabled:=true;
end;

procedure TAstrometry.AstrometrySolveonTimer(Sender: TObject);
begin
TimerAstrometrySolve.Enabled:=false;
end;

procedure TAstrometry.SyncCurrentImage(wait: boolean);
begin
  if (not FBusy) and (FFits.HeaderInfo.naxis>0) and (Mount.Status=devConnected) then begin
   if fits.HeaderInfo.solved then begin
     FFits.SaveToFile(slash(TmpDir)+'ccdcielsolved.fits');
     AstrometrySync(nil);
   end else begin
    FFits.SaveToFile(slash(TmpDir)+'ccdcieltmp.fits');
    StartAstrometry(slash(TmpDir)+'ccdcieltmp.fits',slash(TmpDir)+'ccdcielsolved.fits',@AstrometrySync);
    if wait then WaitBusy(AstrometryTimeout+30);
   end;
  end;
end;

procedure TAstrometry.AstrometrySync(Sender: TObject);
begin
  TimerAstrometrySync.Enabled:=true;
end;

procedure TAstrometry.AstrometrySynconTimer(Sender: TObject);
var fn: string;
    ra,de,eq,jd0,jd1: double;
    n:integer;
begin
TimerAstrometrySync.Enabled:=false;
if LastResult and (cdcwcs_xy2sky<>nil) then begin
   fn:=slash(TmpDir)+'ccdcielsolved.fits';
   n:=cdcwcs_initfitsfile(pchar(fn),0);
   if (n=0) and CurrentCoord(ra,de,eq) then begin
       if mount.Equinox=0 then begin
         jd0:=Jd(trunc(eq),0,0,0);
         jd1:=DateTimetoJD(now);
         ra:=deg2rad*15*ra;
         de:=deg2rad*de;
         PrecessionFK5(jd0,jd1,ra,de);
         ra:=rad2deg*ra/15;
         de:=rad2deg*de;
       end;
       mount.Sync(ra,de);
   end;
end;
end;

procedure TAstrometry.SlewScreenXY(x,y: integer);
begin
  if (not FSlewBusy) and (not FBusy) and (FFits.HeaderInfo.naxis>0) and (Mount.Status=devConnected)and(Camera.Status=devConnected) then begin
   FSlewBusy:=true;
   Xslew:=x;
   Yslew:=y;
   if fits.HeaderInfo.solved then begin
    FFits.SaveToFile(slash(TmpDir)+'ccdcielsolved.fits');
    AstrometrySlewScreenXY(nil);
   end else begin
    FFits.SaveToFile(slash(TmpDir)+'ccdcieltmp.fits');
    StartAstrometry(slash(TmpDir)+'ccdcieltmp.fits',slash(TmpDir)+'ccdcielsolved.fits',@AstrometrySlewScreenXY);
   end;
  end;
end;

procedure TAstrometry.AstrometrySlewScreenXY(Sender: TObject);
begin
  TimerAstrometrySlewScreenXY.Enabled:=true;
end;

procedure TAstrometry.AstrometrySlewScreenXYonTimer(Sender: TObject);
var fn: string;
    xx,yy,n,m: integer;
    ra,de,jd0,jd1: double;
    i: TcdcWCSinfo;
    c: TcdcWCScoord;
    err,prec,exp:double;
    cormethod,bin,maxretry: integer;
begin
TimerAstrometrySlewScreenXY.Enabled:=false;
try
if LastResult and (cdcwcs_xy2sky<>nil) then begin
   fn:=slash(TmpDir)+'ccdcielsolved.fits';
   n:=cdcwcs_initfitsfile(pchar(fn),0);
   n:=cdcwcs_getinfo(addr(i),0);
   if (n=0)and(i.secpix<>0) then begin
     Screen2fits(Xslew,Yslew,xx,yy);
     c.x:=xx;
     c.y:=i.hp-yy;
     m:=cdcwcs_xy2sky(@c,0);
     if m=0 then begin
       ra:=c.ra;
       de:=c.dec;
       if mount.Equinox=0 then begin
         jd0:=jd2000;
         jd1:=DateTimetoJD(now);
         ra:=deg2rad*ra;
         de:=deg2rad*de;
         PrecessionFK5(jd0,jd1,ra,de);
         ra:=rad2deg*ra;
         de:=rad2deg*de;
       end;
       prec:=config.GetValue('/PrecSlew/Precision',5.0)/60;
       cormethod:=config.GetValue('/PrecSlew/Method',1);
       maxretry:=config.GetValue('/PrecSlew/Retry',3);
       exp:=config.GetValue('/PrecSlew/Exposure',10.0);
       bin:=config.GetValue('/PrecSlew/Binning',1);
       PrecisionSlew(ra/15,de,prec,exp,bin,bin,cormethod,maxretry,err);
     end;
   end;
end;
finally
  FSlewBusy:=false;
end;
end;

function TAstrometry.PrecisionSlew(ra,de,prec,exp:double; binx,biny,method,maxslew: integer; out err: double): boolean;
var cra,cde,eq,ar1,ar2,de1,de2,dist,raoffset,deoffset,newra,newde: double;
    jd0,jd1: double;
    fn:string;
    n,i:integer;
begin
  dist:=abs(NullCoord/60);
  FLastSlewErr:=dist;
  if (Mount.Status=devConnected)and(Camera.Status=devConnected) then begin
    raoffset:=0;
    deoffset:=0;
    ar1:=deg2rad*15*ra;
    de1:=deg2rad*de;
    msg('Slew to '+ARToStr3(ra)+'/'+DEToStr(de));
    Mount.Slew(ra, de);
    i:=1;
    fits.SetBPM(bpm,bpmNum,bpmX,bpmY,bpmAxis);
    repeat
      Wait;
      Fpreview.ControlExposure(exp,binx,biny);
      msg('Resolve control exposure');
      FFits.SaveToFile(slash(TmpDir)+'ccdcieltmp.fits');
      StartAstrometry(slash(TmpDir)+'ccdcieltmp.fits',slash(TmpDir)+'ccdcielsolved.fits',nil);
      WaitBusy(AstrometryTimeout+30);
      if not LastResult then begin
         StopAstrometry;
         msg('Fail to resolve control exposure');
         break;
      end;
      fn:=slash(TmpDir)+'ccdcielsolved.fits';
      n:=cdcwcs_initfitsfile(pchar(fn),0);
      if (n<>0) or (not CurrentCoord(cra,cde,eq)) then break;
      if mount.Equinox=0 then begin
        jd0:=Jd(trunc(eq),0,0,0);
        jd1:=DateTimetoJD(now);
        cra:=deg2rad*15*cra;
        cde:=deg2rad*cde;
        PrecessionFK5(jd0,jd1,cra,cde);
        cra:=rad2deg*cra/15;
        cde:=rad2deg*cde;
      end;
      ar2:=deg2rad*15*cra;
      de2:=deg2rad*cde;
      dist:=rad2deg*rmod(AngularDistance(ar1,de1,ar2,de2)+pi2,pi2);
      msg('Distance to target: '+FormatFloat(f5,60*dist)+' arcmin');
      if dist>prec then begin
        case method of
         0: begin
               msg('Sync to '+ARToStr3(cra)+'/'+DEToStr(cde));
               mount.Sync(cra,cde);
               Wait(2);
               msg('Slew to '+ARToStr3(ra)+'/'+DEToStr(de));
               Mount.Slew(ra, de);
            end;
         else begin
               raoffset:=ra+raoffset-cra;
               deoffset:=de+deoffset-cde;
               newra:=rmod(ra+raoffset+24,24.0);
               newde:=de+deoffset;
               if de>90.0 then de:=90;
               if de<-90.0 then de:=-90;
               msg('Slew with offset '+FormatFloat(f5,raoffset)+'/'+FormatFloat(f5,deoffset));
               Mount.Slew(newra,newde);
            end;
         end;
      end;
      inc(i);
    until (dist<=prec)or(i>maxslew);
  end;
  fits.SetBPM(bpm,0,0,0,0);
  result:=(dist<=prec);
  err:=dist;
  FLastSlewErr:=dist;
  if result then msg('Precision slew terminated.')
            else msg('Precision slew failed!');
end;


end.

