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

uses  u_global, u_utils, fu_preview, fu_visu, cu_astrometry_engine, cu_mount, cu_camera,
      cu_wheel, cu_fits, indiapi, cu_planetarium,
      u_translation, LCLIntf, math, Forms, LazFileUtils, Classes, SysUtils, ExtCtrls;

type

TAstrometry = class(TComponent)
  private
    engine: TAstrometry_engine;
    Fpreview:Tf_preview;
    Fvisu: Tf_visu;
    Fplanetarium: TPlanetarium;
    Fterminatecmd: TNotifyEvent;
    FonStartAstrometry: TNotifyEvent;
    FonEndAstrometry: TNotifyInt;
    FonStartGoto,FonEndGoto: TNotifyEvent;
    FonShowMessage: TNotifyMsg;
    FBusy, FSlewBusy, FLastResult: Boolean;
    FLastError: string;
    FLastSlewErr,FInitra,FInitdec,FStartTime: double;
    Fmount: T_mount;
    Fcamera, FFinderCamera: T_camera;
    Fwheel: T_wheel;
    FFits, FGuideFits, FFinderFits: TFits;
    FResolverName: string;
    logfile,solvefile,savefile: string;
    Xslew, Yslew: integer;
    FFinderOffsetX, FFinderOffsetY: double;
    AstrometryTimeout: double;
    TimerAstrometrySolve, TimerAstrometrySync, TimerAstrometrySlewScreenXY,TimerAstrometrySyncFinder,TimerAstrometrySyncGuider : TTimer;
    procedure AstrometrySolveonTimer(Sender: TObject);
    procedure AstrometrySynconTimer(Sender: TObject);
    procedure AstrometrySlewScreenXYonTimer(Sender: TObject);
    procedure msg(txt:string; level: integer);
    function WaitBusy(Timeout:double=60): boolean;
    procedure AstrometrySolve(Sender: TObject);
    procedure AstrometrySync(Sender: TObject);
    procedure AstrometrySyncFinder(Sender: TObject);
    procedure AstrometrySyncGuider(Sender: TObject);
    procedure AstrometrySlewScreenXY(Sender: TObject);
    procedure AstrometrySolveGuide(Sender: TObject);
    procedure AstrometrySolveFinder(Sender: TObject);
    procedure AstrometrySyncFinderonTimer(Sender: TObject);
    procedure AstrometrySyncGuideronTimer(Sender: TObject);
    procedure AstrometrySolvePreview(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);override;
    function StartAstrometry(infile,outfile: string; terminatecmd:TNotifyEvent): boolean;
    procedure StopAstrometry;
    procedure AstrometryDone(errstr:string);
    function  CurrentCoord(out cra,cde,eq,pa: double):boolean;
    function  FinderCurrentCoord(out cra,cde,eq,pa: double):boolean;
    function  GuideCurrentCoord(out cra,cde,eq,pa: double):boolean;
    procedure SolveCurrentImage(wait: boolean; forcesolve:boolean=false);
    procedure SolvePreviewImage;
    procedure SolveGuideImage(wait: boolean = false);
    procedure SyncGuideImage(wait: boolean);
    procedure SolveFinderImage;
    procedure SyncFinderImage(wait: boolean);
    function GetFinderOffset(ra2000,de2000: double):boolean;
    procedure SyncCurrentImage(wait: boolean);
    procedure SlewScreenXY(x,y: integer);
    function PrecisionSlew(ra,de,prec,exp:double; filter,binx,biny,method,maxslew,sgain,soffset: integer; out err: double):boolean;
    function PrecisionSlew(ra,de:double; out err: double):boolean;
    function AutofocusPrecisionSlew(ra,de:double; out err: double):boolean;
    procedure MarkPlanetarium(ra,de: double);
    property Busy: Boolean read FBusy;
    property SlewBusy: Boolean read FSlewBusy;
    property LastSlewErr: double read FLastSlewErr;
    property LastResult: Boolean read FLastResult;
    property LastError: String read FLastError;
    property InitRA: double read Finitra;
    property InitDEC: double read Finitdec;
    property StartTime: double read FStartTime;
    property ResultFile: string read savefile;
    property Resolver: string read FResolverName;
    property Mount: T_mount read Fmount write Fmount;
    property Camera: T_camera read Fcamera write Fcamera;
    property FinderCamera: T_camera read FFinderCamera write FFinderCamera;
    property Wheel: T_wheel read Fwheel write Fwheel;
    property Fits: TFits read FFits write FFits;
    property GuideFits: TFits read FGuideFits write FGuideFits;
    property FinderFits: TFits read FFinderFits write FFinderFits;
    property preview:Tf_preview read Fpreview write Fpreview;
    property visu:Tf_visu read Fvisu write Fvisu;
    property planetarium: TPlanetarium read Fplanetarium write Fplanetarium;
    property FinderOffsetX: double read FFinderOffsetX write FFinderOffsetX;
    property FinderOffsetY: double read FFinderOffsetY write FFinderOffsetY;
    property onShowMessage: TNotifyMsg read FonShowMessage write FonShowMessage;
    property onAstrometryStart: TNotifyEvent read FonStartAstrometry write FonStartAstrometry;
    property onAstrometryEnd: TNotifyInt read FonEndAstrometry write FonEndAstrometry;
    property onGotoStart: TNotifyEvent read FonStartGoto write FonStartGoto;
    property onGotoEnd: TNotifyEvent read FonEndGoto write FonEndGoto;
end;

implementation

constructor TAstrometry.Create(AOwner: TComponent);
begin
  Inherited create(AOwner);
  FBusy:=false;
  FSlewBusy:=false;
  FLastResult:=false;
  FLastError:='';
  FLastSlewErr:=0;
  FFinderCamera:=nil;
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
  TimerAstrometrySyncFinder:=TTimer.Create(self);
  TimerAstrometrySyncFinder.Enabled:=false;
  TimerAstrometrySyncFinder.Interval:=100;
  TimerAstrometrySyncFinder.OnTimer:=@AstrometrySyncFinderonTimer;
  TimerAstrometrySyncGuider:=TTimer.Create(self);
  TimerAstrometrySyncGuider.Enabled:=false;
  TimerAstrometrySyncGuider.Interval:=100;
  TimerAstrometrySyncGuider.OnTimer:=@AstrometrySyncGuideronTimer;
end;

procedure TAstrometry.msg(txt:string; level: integer);
begin
 if assigned(FonShowMessage) then FonShowMessage(txt,level);
end;

function TAstrometry.WaitBusy(Timeout:double=60): boolean;
var endt: TDateTime;
begin
  endt:=now+Timeout/secperday;
  while (FBusy)and(now<endt)and (not CancelAutofocus) do begin
     sleep(100);
     if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
  end;
  result:=not FBusy;
end;

function TAstrometry.StartAstrometry(infile,outfile: string; terminatecmd:TNotifyEvent): boolean;
var pixsize,pixscale,telescope_focal_length,tolerance,MaxRadius,ra,de: double;
    iwidth,iheight:integer;
    online: boolean;
    f: TFits;
begin
 if (not FBusy) then begin
   Fterminatecmd:=terminatecmd;
   ra:=NullCoord;
   de:=NullCoord;
   pixscale:=NullCoord;
   iwidth:=1000;
   iheight:=1000;
   f:=TFits.Create(nil);
   f.LoadHeaderFromFile(infile);
   if f.HeaderInfo.valid then begin
     ra:=f.HeaderInfo.ra;
     de:=f.HeaderInfo.dec;
     pixscale:=f.HeaderInfo.scale;
     iwidth:=f.HeaderInfo.naxis1;
     iheight:=f.HeaderInfo.naxis2;
   end;
   f.free;
   if (ra=NullCoord)or(de=NullCoord) then begin
       msg(Format(rsCannotFindAp, [crlf]),2);
   end;
   FLastResult:=false;
   FLastError:='';
   FInitra:=ra;
   FInitdec:=de;
   FStartTime:=now;
   logfile:=ChangeFileExt(infile,'.log');
   solvefile:=ChangeFileExt(infile,'.solved');
   savefile:=outfile;
   DeleteFileUTF8(outfile);
   DeleteFileUTF8(solvefile);
   engine:=TAstrometry_engine.Create;
   if ((Fterminatecmd=@AstrometrySolveFinder)or(Fterminatecmd=@AstrometrySyncFinder)) and config.GetValue('/Astrometry/UseFinderSolver',false) then begin
     engine.Resolver:=config.GetValue('/Astrometry/FinderSolver',ResolverAstap);
     engine.Fallback:=false;
   end
   else begin
     engine.Resolver:=config.GetValue('/Astrometry/Resolver',ResolverAstap);
     engine.Fallback:=config.GetValue('/Astrometry/Fallback',false);
   end;
   FResolverName:=ResolverName[engine.Resolver];
   engine.AstrometryPath:=config.GetValue('/Astrometry/AstrometryPath','');
   engine.CygwinPath:=config.GetValue('/Astrometry/CygwinPath','C:\cygwin');
   engine.ElbrusFolder:=config.GetValue('/Astrometry/ElbrusFolder','');
   engine.ElbrusUnixpath:=config.GetValue('/Astrometry/ElbrusUnixpath','');
   engine.PlateSolveFolder:=config.GetValue('/Astrometry/PlatesolveFolder','');
   engine.PlateSolveWait:=config.GetValue('/Astrometry/PlatesolveWait',0);
   engine.ASTAPFolder:=config.GetValue('/Astrometry/ASTAPFolder','');
   engine.ASTAPSearchRadius:=config.GetValue('/Astrometry/ASTAPSearchRadius',30);
   engine.ASTAPdownsample:=config.GetValue('/Astrometry/ASTAPdownsample',0);
   engine.LogFile:=logfile;
   engine.InFile:=infile;
   engine.OutFile:=outfile;
   tolerance:=min(0.99,max(0.01,config.GetValue('/Astrometry/ScaleTolerance',0.5)));
   MaxRadius:=config.GetValue('/Astrometry/MaxRadius',15.0);
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
   if (pixscale<>NullCoord)and(pixscale>0) then begin
      engine.scalelow:=(1-tolerance)*pixscale;
      engine.scalehigh:=(1+tolerance)*pixscale;
   end;
   engine.downsample:=config.GetValue('/Astrometry/DownSample',4);
   engine.objs:=config.GetValue('/Astrometry/SourcesLimit',150);
   engine.OtherOptions:=config.GetValue('/Astrometry/OtherOptions','');
   online:=config.GetValue('/Astrometry/AstUseOnline',false);
   if online then begin
     engine.UseScript:=true;
     {$ifdef mswindows}
     engine.CustomScript:=slash(ScriptsDir)+'astrometry-online.bat';
     {$else}
     engine.CustomScript:=slash(ScriptsDir)+'astrometry-online.sh';
     {$endif}
   end
   else begin
     engine.UseScript:=config.GetValue('/Astrometry/AstUseScript',false);
     engine.CustomScript:=config.GetValue('/Astrometry/AstCustScript','');
   end;
   engine.ra:=ra;
   engine.de:=de;
   engine.radius:=max(MaxRadius,pixscale*iwidth/3600);
   engine.Xsize:=pixscale*iwidth/3600;
   engine.Ysize:=pixscale*iheight/3600;
   engine.iwidth:=iwidth;
   engine.iheight:=iheight;
   engine.timeout:=AstrometryTimeout;
   FBusy:=true;
   engine.Resolve;
   if (Fterminatecmd<>@AstrometrySolvePreview) then msg(Format(rsResolvingUsi, [ResolverName[engine.Resolver]]),3);
   if Assigned(FonStartAstrometry) then FonStartAstrometry(self);
   result:=true;
 end else begin
   msg(rsResolverAlre,0);
   result:=false;
 end;
end;

procedure TAstrometry.AstrometryDone(errstr:string);
var buf:string;
    f:TextFile;
begin
 if FileExistsUTF8(savefile) and FileExistsUTF8(solvefile) then
   FLastResult:=true
 else
   FLastResult:=false;
 FBusy:=false;
 FLastError:=trim(errstr);
 if (FLastError<>'') and (logfile<>'') and FileExistsUTF8(logfile) then begin
   try
   AssignFile(f,logfile);
   Reset(f);
   while not EOF(f) do begin
     ReadLn(f,buf);
     msg(buf,9);
   end;
   CloseFile(f);
   except
   end;
 end;
 if (Fterminatecmd<>@AstrometrySolveGuide)and
    (Fterminatecmd<>@AstrometrySolvePreview)and
    (Fterminatecmd<>@AstrometrySolveFinder) and
    Assigned(FonEndAstrometry)
    then
       FonEndAstrometry(0);
 if Assigned(Fterminatecmd) then Fterminatecmd(self);
 Fterminatecmd:=nil;
end;

procedure TAstrometry.StopAstrometry;
begin
  if FBusy then begin
    FBusy:=false;
    engine.Stop;
    msg(rsStopAstromet2,1);
  end;
end;

function TAstrometry.CurrentCoord(out cra,cde,eq,pa: double):boolean;
var n,m: integer;
    i: TcdcWCSinfo;
    c: TcdcWCScoord;
begin
  result:=false;
  if cdcwcs_xy2sky<>nil then begin
    n:=cdcwcs_getinfo(addr(i),wcsmain);
    if (n=0)and(i.secpix<>0) then begin
      c.x:=0.5+i.wp/2;
      c.y:=0.5+i.hp/2;
      m:=cdcwcs_xy2sky(@c,wcsmain);
      if m=0 then begin
        cra:=c.ra/15;
        cde:=c.dec;
        eq:=2000;
        pa:=i.rot;
        result:=true;
      end;
    end;
  end
  else
    msg('Missing library '+libwcs,1);
end;

function TAstrometry.FinderCurrentCoord(out cra,cde,eq,pa: double):boolean;
var n,m: integer;
    i: TcdcWCSinfo;
    c: TcdcWCScoord;
begin
  result:=false;
  if cdcwcs_xy2sky<>nil then begin
    n:=cdcwcs_getinfo(addr(i),wcsfind);
    if (n=0)and(i.secpix<>0) then begin
      c.x:=FFinderOffsetX;
      c.y:=FFinderOffsetY;
      m:=cdcwcs_xy2sky(@c,wcsfind); // do not check the result, can be out of frame
      cra:=c.ra/15;
      cde:=c.dec;
      eq:=2000;
      pa:=i.rot;
      result:=true;
    end;
  end
  else
    msg('Missing library '+libwcs,1);
end;

function TAstrometry.GuideCurrentCoord(out cra,cde,eq,pa: double):boolean;
var n,m: integer;
    i: TcdcWCSinfo;
    c: TcdcWCScoord;
begin
  result:=false;
  if cdcwcs_xy2sky<>nil then begin
    n:=cdcwcs_getinfo(addr(i),wcsguide);
    if (n=0)and(i.secpix<>0) then begin
      c.x:=0.5+i.wp/2;
      c.y:=0.5+i.hp/2;
      m:=cdcwcs_xy2sky(@c,wcsguide); // do not check the result, can be out of frame
      cra:=c.ra/15;
      cde:=c.dec;
      eq:=2000;
      pa:=i.rot;
      result:=true;
    end;
  end
  else
    msg('Missing library '+libwcs,1);
end;

procedure TAstrometry.SolveCurrentImage(wait: boolean; forcesolve:boolean=false);
var n: integer;
begin
  if (not FBusy) and (FFits.HeaderInfo.naxis>0) and FFits.ImageValid then begin
   if (not forcesolve) and FFits.HeaderInfo.solved and (cdcwcs_initfitsfile<>nil) then begin
     FFits.SaveToFile(slash(TmpDir)+'ccdcielsolved.fits');
     n:=cdcwcs_initfitsfile(pchar(slash(TmpDir)+'ccdcielsolved.fits'),wcsmain);
     FLastResult:=(n=0);
   end
   else begin
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
var ra,de,eq,pa,ra2000,de2000: double;
begin
TimerAstrometrySolve.Enabled:=false;
if FFits.HeaderInfo.solved and CurrentCoord(ra,de,eq,pa) then begin
   ra2000:=ra;
   de2000:=de;
   ra:=ra*15*deg2rad;
   de:=de*deg2rad;
   J2000ToApparent(ra,de);
   ra:=rad2deg*ra/15;
   de:=rad2deg*de;
   msg(Format(rsCenterAppare, [RAToStr(ra), DEToStr(de), FormatFloat(f1, pa)])+', J2000 '+rsRA+'='+RAToStr(ra2000)+' '+rsDec+'='+DEToStr(de2000),3);
end;
end;

procedure TAstrometry.SolvePreviewImage;
var n: integer;
begin
  if (not FBusy) and (FFits.HeaderInfo.naxis>0) and FFits.ImageValid then begin
    FFits.SaveToFile(slash(TmpDir)+'ccdcieltmp.fits');
    StartAstrometry(slash(TmpDir)+'ccdcieltmp.fits',slash(TmpDir)+'ccdcielsolved.fits',@AstrometrySolvePreview);
  end;
end;

procedure TAstrometry.AstrometrySolvePreview(Sender: TObject);
var ra,de,pa,ra2000,de2000: double;
    n,m: integer;
    i: TcdcWCSinfo;
    c: TcdcWCScoord;
begin
  if Assigned(FonEndAstrometry) then FonEndAstrometry(1);
  if cdcwcs_xy2sky<>nil then begin
    n:=cdcwcs_initfitsfile(pchar(slash(TmpDir)+'ccdcielsolved.fits'),wcspreview);
    try
    if n=0 then n:=cdcwcs_getinfo(addr(i),wcspreview);
    if (n=0)and(i.secpix<>0) then begin
      c.x:=0.5+i.wp/2;
      c.y:=0.5+i.hp/2;
      m:=cdcwcs_xy2sky(@c,wcspreview);
      if m=0 then begin
        ra:=c.ra/15;
        de:=c.dec;
        pa:=i.rot;
        ra2000:=ra;
        de2000:=de;
        ra:=ra*15*deg2rad;
        de:=de*deg2rad;
        J2000ToApparent(ra,de);
        ra:=rad2deg*ra/15;
        de:=rad2deg*de;
        if preview.CheckBoxAstrometry.Checked then
          preview.LabelAstrometry.Caption:= 'Apparent: '+RAToStr(ra)+' '+ DEToStr(de)+crlf+
                                            'J2000   : '+RAToStr(ra2000)+' '+DEToStr(de2000)+crlf+
                                            'PA      : '+FormatFloat(f1, pa);
        MarkPlanetarium(ra2000,de2000);
      end;
    end;
    finally
    end;
  end
  else
    msg('Missing library '+libwcs,1);
end;

procedure TAstrometry.SolveFinderImage;
begin
  if (not FBusy) and (FFinderFits.HeaderInfo.naxis>0) and FFinderFits.ImageValid then begin
    FFinderFits.SaveToFile(slash(TmpDir)+'findertmp.fits');
    StartAstrometry(slash(TmpDir)+'findertmp.fits',slash(TmpDir)+'findersolved.fits',@AstrometrySolveFinder);
    WaitBusy();
  end;
end;

procedure TAstrometry.AstrometrySolveFinder(Sender: TObject);
var ra,de,pa,ra2000,de2000: double;
    n,m: integer;
    i: TcdcWCSinfo;
    c: TcdcWCScoord;
begin
  if Assigned(FonEndAstrometry) then FonEndAstrometry(2);
  if cdcwcs_xy2sky<>nil then begin
    n:=cdcwcs_initfitsfile(pchar(slash(TmpDir)+'findersolved.fits'),wcsfind);
    try
    if n=0 then n:=cdcwcs_getinfo(addr(i),wcsfind);
    if (n=0)and(i.secpix<>0) then begin
      c.x:=0.5+i.wp/2;
      c.y:=0.5+i.hp/2;
      m:=cdcwcs_xy2sky(@c,wcsfind);
      if m=0 then begin
        ra:=c.ra/15;
        de:=c.dec;
        pa:=i.rot;
        ra2000:=ra;
        de2000:=de;
        ra:=ra*15*deg2rad;
        de:=de*deg2rad;
        J2000ToApparent(ra,de);
        ra:=rad2deg*ra/15;
        de:=rad2deg*de;
        msg(rsFinderCamera+': '+rsFOV+blank+FormatFloat(f2, i.wp*i.secpix/60)+'x'+FormatFloat(f2, i.hp*i.secpix/60)+smin+', '+rsPixelScale+': '+FormatFloat(f2,i.secpix)+ssec+'/'+rsPixel,3);
        msg(rsFinderCamera+': '+Format(rsCenterAppare, [RAToStr(ra), DEToStr(de), FormatFloat(f1, pa)])+', J2000 '+rsRA+'='+RAToStr(ra2000)+' '+rsDec+'='+DEToStr(de2000),3);
        MarkPlanetarium(ra2000,de2000);
      end;
    end;
    finally
    end;
  end
  else
    msg('Missing library '+libwcs,1);
end;

procedure TAstrometry.SyncFinderImage(wait: boolean);
begin
  if (not FBusy) and (FFinderFits.HeaderInfo.naxis>0) and FFinderFits.ImageValid and (Mount.Status=devConnected) then begin
    if FFinderFits.HeaderInfo.solved then begin
      FFinderFits.SaveToFile(slash(TmpDir)+'findersolved.fits');
      AstrometrySyncFinder(nil);
    end else begin
     FFinderFits.SaveToFile(slash(TmpDir)+'findertmp.fits');
     StartAstrometry(slash(TmpDir)+'findertmp.fits',slash(TmpDir)+'findersolved.fits',@AstrometrySyncFinder);
     if wait then WaitBusy(AstrometryTimeout+30);
    end;
  end;
end;

procedure TAstrometry.AstrometrySyncFinder(Sender: TObject);
begin
  TimerAstrometrySyncFinder.Enabled:=true;
end;

procedure TAstrometry.AstrometrySyncFinderonTimer(Sender: TObject);
var fn: string;
    ra,de,eq,pa: double;
    n:integer;
begin
TimerAstrometrySyncFinder.Enabled:=false;
if LastResult and (cdcwcs_xy2sky<>nil) then begin
   fn:=slash(TmpDir)+'findersolved.fits';
   n:=cdcwcs_initfitsfile(pchar(fn),wcsfind);
   if FinderCurrentCoord(ra,de,eq,pa) then begin
       J2000ToMount(mount.EquinoxJD,ra,de);
       mount.Sync(ra,de);
   end;
end;
end;

function TAstrometry.GetFinderOffset(ra2000,de2000: double): boolean;
var n: integer;
    c: TcdcWCScoord;
begin
  result:=false;
  try
  c.ra:=15*ra2000;
  c.dec:=de2000;
  n:=cdcwcs_sky2xy(@c,wcsfind); // do not check the result, can be out of frame
  FFinderOffsetX:=c.x;
  FFinderOffsetY:=c.y;
  result:=true;
  except
  end;
end;

procedure TAstrometry.SolveGuideImage(wait: boolean = false);
begin
  if (not FBusy) and (FGuideFits.HeaderInfo.naxis>0) and FGuideFits.ImageValid then begin
    FGuideFits.SaveToFile(slash(TmpDir)+'guidetmp.fits');
    StartAstrometry(slash(TmpDir)+'guidetmp.fits',slash(TmpDir)+'guidesolved.fits',@AstrometrySolveGuide);
    if wait then WaitBusy(AstrometryTimeout+30);
  end;
end;

procedure TAstrometry.AstrometrySolveGuide(Sender: TObject);
var ra,de,pa,ra2000,de2000: double;
    n,m: integer;
    i: TcdcWCSinfo;
    c: TcdcWCScoord;
begin
if Assigned(FonEndAstrometry) then FonEndAstrometry(3);
if cdcwcs_xy2sky<>nil then begin
  n:=cdcwcs_initfitsfile(pchar(slash(TmpDir)+'guidesolved.fits'),wcsguide);
  if n=0 then n:=cdcwcs_getinfo(addr(i),wcsguide);
  if (n=0)and(i.secpix<>0) then begin
    c.x:=0.5+i.wp/2;
    c.y:=0.5+i.hp/2;
    m:=cdcwcs_xy2sky(@c,wcsguide);
    if m=0 then begin
      ra:=c.ra/15;
      de:=c.dec;
      pa:=i.rot;
      ra2000:=ra;
      de2000:=de;
      ra:=ra*15*deg2rad;
      de:=de*deg2rad;
      J2000ToApparent(ra,de);
      ra:=rad2deg*ra/15;
      de:=rad2deg*de;
      msg(rsGuideCamera+': '+rsFOV+blank+FormatFloat(f2, i.wp*i.secpix/60)+'x'+FormatFloat(f2, i.hp*i.secpix/60)+smin+', '+rsPixelScale+': '+FormatFloat(f2,i.secpix)+ssec+'/'+rsPixel,3);
      msg(rsGuideCamera+': '+Format(rsCenterAppare, [RAToStr(ra), DEToStr(de), FormatFloat(f1, pa)])+', J2000 '+rsRA+'='+RAToStr(ra2000)+' '+rsDec+'='+DEToStr(de2000),3);
    end;
  end;
end
else
  msg('Missing library '+libwcs,1);
end;

procedure TAstrometry.SyncGuideImage(wait: boolean);
begin
  if (not FBusy) and (FGuideFits.HeaderInfo.naxis>0) and FGuideFits.ImageValid and (Mount.Status=devConnected) then begin
    if FGuideFits.HeaderInfo.solved then begin
      FGuideFits.SaveToFile(slash(TmpDir)+'guidesolved.fits');
      AstrometrySyncGuider(nil);
    end else begin
     FGuideFits.SaveToFile(slash(TmpDir)+'guidetmp.fits');
     StartAstrometry(slash(TmpDir)+'guidetmp.fits',slash(TmpDir)+'guidesolved.fits',@AstrometrySyncGuider);
     if wait then WaitBusy(AstrometryTimeout+30);
    end;
  end;
end;

procedure TAstrometry.AstrometrySyncGuider(Sender: TObject);
begin
  TimerAstrometrySyncGuider.Enabled:=true;
end;

procedure TAstrometry.AstrometrySyncGuideronTimer(Sender: TObject);
var fn: string;
    ra,de,eq,pa: double;
    n:integer;
begin
TimerAstrometrySyncGuider.Enabled:=false;
if LastResult and (cdcwcs_xy2sky<>nil) then begin
   fn:=slash(TmpDir)+'guidesolved.fits';
   n:=cdcwcs_initfitsfile(pchar(fn),wcsguide);
   if GuideCurrentCoord(ra,de,eq,pa) then begin
       J2000ToMount(mount.EquinoxJD,ra,de);
       mount.Sync(ra,de);
   end;
end;
end;

procedure TAstrometry.SyncCurrentImage(wait: boolean);
begin
  if (not FBusy) and (FFits.HeaderInfo.naxis>0) and FFits.ImageValid and (Mount.Status=devConnected) then begin
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
    ra,de,eq,pa: double;
    n:integer;
begin
TimerAstrometrySync.Enabled:=false;
if LastResult and (cdcwcs_xy2sky<>nil) then begin
   fn:=slash(TmpDir)+'ccdcielsolved.fits';
   n:=cdcwcs_initfitsfile(pchar(fn),wcsmain);
   if n<>0 then begin
     msg(Format(rsErrorProcess, [TmpDir]),0);
     exit;
   end;
   if (n=0) and CurrentCoord(ra,de,eq,pa) then begin
       J2000ToMount(mount.EquinoxJD,ra,de);
       mount.Sync(ra,de);
   end;
end;
end;

procedure TAstrometry.SlewScreenXY(x,y: integer);
begin
  if (not FSlewBusy) and (not FBusy) and (FFits.HeaderInfo.naxis>0) and FFits.ImageValid and AllDevicesConnected and(Mount.Status=devConnected)and(Camera.Status=devConnected) then begin
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
    ra,de: double;
    i: TcdcWCSinfo;
    c: TcdcWCScoord;
    err,prec,exp:double;
    sgain,soffset: integer;
    fi,cormethod,bin,maxretry: integer;
begin
TimerAstrometrySlewScreenXY.Enabled:=false;
try
if LastResult and (cdcwcs_xy2sky<>nil) then begin
   fn:=slash(TmpDir)+'ccdcielsolved.fits';
   n:=cdcwcs_initfitsfile(pchar(fn),wcsmain);
   if n<>0 then begin
     msg(Format(rsErrorProcess, [TmpDir]),0);
     exit;
   end;
   n:=cdcwcs_getinfo(addr(i),wcsmain);
   if (n=0)and(i.secpix<>0) then begin
     Screen2fits(Xslew,Yslew,Fvisu.FlipHorz,Fvisu.FlipVert,xx,yy);
     c.x:=xx;
     c.y:=i.hp-yy;
     m:=cdcwcs_xy2sky(@c,wcsmain);
     if m=0 then begin
       ra:=c.ra/15;
       de:=c.dec;
       J2000ToMount(mount.EquinoxJD,ra,de);
       prec:=config.GetValue('/PrecSlew/Precision',SlewPrecision)/60;
       cormethod:=config.GetValue('/PrecSlew/Method',1);
       maxretry:=config.GetValue('/PrecSlew/Retry',3);
       exp:=config.GetValue('/PrecSlew/Exposure',10.0);
       sgain:=config.GetValue('/PrecSlew/Gain',NullInt);
       soffset:=config.GetValue('/PrecSlew/Offset',NullInt);
       bin:=config.GetValue('/PrecSlew/Binning',1);
       fi:=config.GetValue('/PrecSlew/Filter',0);
       PrecisionSlew(ra,de,prec,exp,fi,bin,bin,cormethod,maxretry,sgain,soffset,err);
     end;
   end;
end;
finally
  FSlewBusy:=false;
end;
end;

function TAstrometry.PrecisionSlew(ra,de,prec,exp:double; filter,binx,biny,method,maxslew,sgain,soffset: integer; out err: double): boolean;
var cra,cde,eq,ar1,ar2,de1,de2,dist,raoffset,deoffset,newra,newde,pa,ara,ade: double;
    fn:string;
    n,i,oldfilter,delay,RetryMeridianSyncCount:integer;
    SyncOK,NearMeridian,RetryMeridianSync: boolean;
begin
// ra,de parameters use equinox of the mount (local or 2000), same as slew()
  result:=false;
  if Mount.Park then begin
    msg('Mount is parked!',1);
    exit;
  end;
  if cdcwcs_xy2sky=nil then begin
    msg('Missing library '+libwcs,1);
    exit;
  end;
  oldfilter:=0;
  try
  if assigned(FonStartGoto) then FonStartGoto(self);
  delay:=config.GetValue('/PrecSlew/Delay',5);
  dist:=abs(NullCoord/60);
  FLastSlewErr:=dist;
  if (Mount.Status=devConnected)and(Camera.Status=devConnected)and AllDevicesConnected then begin
   if astrometryResolver=ResolverNone then begin
      msg(rsNoResolverCo,2);
      msg(Format(rsDoSimpleSlew, [ARToStr3(ra), DEToStr(de)]),2);
      if not Mount.Slew(ra, de) then exit;
      if CancelAutofocus or CancelGoto then exit;
      Wait(delay);
      dist:=0;
   end
   else begin
    if FFinderCamera=nil then begin
      fits.SetBPM(bpm,bpmNum,bpmX,bpmY,bpmAxis);
      if filter>0 then begin
        oldfilter:=Fwheel.Filter;
        Fwheel.Filter:=filter;
      end;
    end;
    raoffset:=0;
    deoffset:=0;
    ar1:=deg2rad*15*ra;
    de1:=deg2rad*de;
    if not Mount.Slew(ra, de) then exit;
    if CancelAutofocus or CancelGoto then exit;
    i:=1;
    RetryMeridianSyncCount:=0;
    repeat
      RetryMeridianSync:=false;
      Wait(delay);
      if CancelAutofocus or CancelGoto then exit;
      if FFinderCamera=nil then begin
        // Use main camera
        if not Fcamera.ControlExposure(exp,binx,biny,LIGHT,ReadoutModeAstrometry,sgain,soffset) then begin
          msg(rsExposureFail,0);
          exit;
        end;
        if CancelAutofocus or CancelGoto then exit;
        msg(rsResolveContr,3);
        FFits.SaveToFile(slash(TmpDir)+'ccdcieltmp.fits');
        if StartAstrometry(slash(TmpDir)+'ccdcieltmp.fits',slash(TmpDir)+'ccdcielsolved.fits',nil) then
           WaitBusy(AstrometryTimeout+30);
        if not LastResult then begin
           StopAstrometry;
           msg(rsFailToResolv,0);
           inc(i);
           continue;
        end;
        if CancelAutofocus or CancelGoto then exit;
        fn:=slash(TmpDir)+'ccdcielsolved.fits';
        n:=cdcwcs_initfitsfile(pchar(fn),wcsmain);
        if n<>0 then begin
          msg(Format(rsErrorProcess, [TmpDir]),0);
          exit;
        end;
        if (n<>0) or (not CurrentCoord(cra,cde,eq,pa)) then break;
      end
      else begin
        // Use finder camera
        if not FFinderCamera.ControlExposure(exp,binx,biny,LIGHT,ReadoutModeAstrometry,sgain,soffset) then begin
          msg(rsExposureFail,0);
          exit;
        end;
        if CancelAutofocus or CancelGoto then exit;
        msg(rsResolveContr,3);
        FFinderFits.SaveToFile(slash(TmpDir)+'finder.fits');
        if StartAstrometry(slash(TmpDir)+'finder.fits',slash(TmpDir)+'findersolved.fits',@AstrometrySolveFinder) then
           WaitBusy(AstrometryTimeout+30);
        if not LastResult then begin
           StopAstrometry;
           msg(rsFailToResolv,0);
           inc(i);
           continue;
        end;
        if CancelAutofocus or CancelGoto then exit;
        fn:=slash(TmpDir)+'findersolved.fits';
        n:=cdcwcs_initfitsfile(pchar(fn),wcsfind);
        if n<>0 then begin
          msg(Format(rsErrorProcess, [TmpDir]),0);
          exit;
        end;
        if (n<>0) or (not FinderCurrentCoord(cra,cde,eq,pa)) then break;
      end;

      ara:=deg2rad*15*cra;
      ade:=deg2rad*cde;
      J2000ToApparent(ara,ade);
      NearMeridian:=(abs(CurrentSidTim-ara)<=(4*deg2rad));    // we are pointing within 4 degree of the meridian
      J2000ToMount(mount.EquinoxJD,cra,cde);
      ar2:=deg2rad*15*cra;
      de2:=deg2rad*cde;
      dist:=rad2deg*rmod(AngularDistance(ar1,de1,ar2,de2)+pi2,pi2);
      msg(Format(rsDistanceToTa, [FormatFloat(f5, 60*dist)]),3);
      if CancelAutofocus or CancelGoto then exit;
      if dist>prec then begin
        case method of
         0: begin
               SyncOK:=mount.Sync(cra,cde);
               if SyncOK then begin
                  Wait(2);
                  if CancelAutofocus or CancelGoto then exit;
                  if not Mount.Slew(ra, de) then exit;
                  if CancelAutofocus or CancelGoto then exit;
               end
               else begin
                 if NearMeridian then begin        // some mount cannot sync across the meridian
                   inc(RetryMeridianSyncCount);
                   if RetryMeridianSyncCount<=10 then begin        // retry for 10 minutes so the mount move a bit further
                     msg('Mount Sync failed near the meridian.',2);
                     msg('Waiting 1 minute before to retry',2);
                     Wait(60);
                     if CancelAutofocus or CancelGoto then exit;
                     RetryMeridianSync:=true;
                     dec(i);
                   end
                   else begin
                     msg('Mount Sync failed near the meridian.',0);
                     msg('Abandon after 10 retries.',0);
                     break;
                   end;
                 end
                 else begin
                   msg('Mount Sync failed!',0);
                   break;
                 end;
               end;
            end;
         else begin
               if CancelAutofocus or CancelGoto then exit;
               raoffset:=ra+raoffset-cra;
               deoffset:=de+deoffset-cde;
               newra:=rmod(ra+raoffset+24,24.0);
               newde:=de+deoffset;
               if de>90.0 then de:=90;
               if de<-90.0 then de:=-90;
               msg(Format(rsSlewWithOffs, [FormatFloat(f5, raoffset),
                 FormatFloat(f5, deoffset)]),3);
               if not Mount.Slew(newra,newde) then exit;
               if CancelAutofocus or CancelGoto then exit;
            end;
         end;
      end;
      if CancelAutofocus or CancelGoto then exit;
      inc(i);
    until (not RetryMeridianSync)and((dist<=prec)or(i>maxslew));
   end;
  end;
  result:=(dist<=prec);

  finally
    err:=dist;
    FLastSlewErr:=dist;
    if assigned(FonEndGoto) then FonEndGoto(self);
    if result then msg(rsPrecisionSle2,2)
              else msg(rsPrecisionSle3,0);
    fits.SetBPM(bpm,0,0,0,0);
    if oldfilter>0 then Fwheel.Filter:=oldfilter;
  end;
end;

function TAstrometry.PrecisionSlew(ra,de:double; out err: double):boolean;
var prec,exp:double;
    fi,cormethod,bin,maxretry: integer;
    sgain,soffset: integer;
begin
  prec:=config.GetValue('/PrecSlew/Precision',SlewPrecision)/60;
  cormethod:=config.GetValue('/PrecSlew/Method',1);
  maxretry:=config.GetValue('/PrecSlew/Retry',3);
  exp:=config.GetValue('/PrecSlew/Exposure',10.0);
  sgain:=config.GetValue('/PrecSlew/Gain',NullInt);
  soffset:=config.GetValue('/PrecSlew/Offset',NullInt);
  bin:=config.GetValue('/PrecSlew/Binning',1);
  fi:=config.GetValue('/PrecSlew/Filter',0);
  result:=PrecisionSlew(ra,de,prec,exp,fi,bin,bin,cormethod,maxretry,sgain,soffset,err);
end;

function TAstrometry.AutofocusPrecisionSlew(ra,de:double; out err: double):boolean;
var prec,exp:double;
    fi,cormethod,bin,maxretry: integer;
    sgain,soffset: integer;
begin
  prec:=config.GetValue('/StarAnalysis/AutofocusPrecisionSlew',2.0)/60;
  cormethod:=config.GetValue('/PrecSlew/Method',1);
  maxretry:=config.GetValue('/PrecSlew/Retry',3);
  exp:=config.GetValue('/PrecSlew/Exposure',10.0);
  sgain:=config.GetValue('/PrecSlew/Gain',NullInt);
  soffset:=config.GetValue('/PrecSlew/Offset',NullInt);
  bin:=config.GetValue('/PrecSlew/Binning',1);
  fi:=config.GetValue('/PrecSlew/Filter',0);
  result:=PrecisionSlew(ra,de,prec,exp,fi,bin,bin,cormethod,maxretry,sgain,soffset,err);
end;

procedure TAstrometry.MarkPlanetarium(ra,de: double);
begin
  if PlanetariumShowAstrometry and (planetarium<>nil)and(planetarium.Connected) then begin
     planetarium.ShowAstrometry(ra,de);
  end;
end;

end.

