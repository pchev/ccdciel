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

uses  u_global, u_utils, cu_astrometry_engine, cu_mount, cu_camera,
      math, FileUtil, Classes, SysUtils;

type

TAstrometry = class(TComponent)
  private
    engine: TAstrometry_engine;
    Fterminatecmd: TNotifyEvent;
    FonStartAstrometry: TNotifyEvent;
    FonEndAstrometry: TNotifyEvent;
    FonShowMessage: TNotifyMsg;
    FBusy, FLastResult: Boolean;
    Fmount: T_mount;
    Fcamera: T_camera;
    FResolverName: string;
    logfile,solvefile,savefile: string;
    procedure msg(txt:string);
    procedure AstrometryDone(Sender: TObject);
  public
    constructor Create;
    function StartAstrometry(infile,outfile: string; terminatecmd:TNotifyEvent): boolean;
    procedure StopAstrometry;
    property Busy: Boolean read FBusy;
    property LastResult: Boolean read FLastResult;
    property Resolver: string read FResolverName;
    property Mount: T_mount read Fmount write Fmount;
    property Camera: T_camera read Fcamera write Fcamera;
    property onShowMessage: TNotifyMsg read FonShowMessage write FonShowMessage;
    property onAstrometryStart: TNotifyEvent read FonStartAstrometry write FonStartAstrometry;
    property onAstrometryEnd: TNotifyEvent read FonEndAstrometry write FonEndAstrometry;
end;

implementation

constructor TAstrometry.Create;
begin
  Inherited create(nil);
  FBusy:=false;
  FLastResult:=false;
end;

procedure TAstrometry.msg(txt:string);
begin
 if assigned(FonShowMessage) then FonShowMessage(txt);
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
   iwidth:=1000;
   if n=0 then begin
     n:=cdcwcs_getinfo(addr(info),0);
     if n=0 then begin
       ra:=info.cra;
       de:=info.cdec;
       iwidth:=info.wp;
     end;
   end;
   if (ra=NullCoord)or(de=NullCoord) then begin
       msg('Cannot find approximate coordinates for this image.'+crlf+'The astrometry resolution may take a very long time.');
   end;
   logfile:=ChangeFileExt(infile,'.log');
   solvefile:=ChangeFileExt(infile,'.solved');
   savefile:=outfile;
   DeleteFileUTF8(outfile);
   DeleteFileUTF8(solvefile);
   engine:=TAstrometry_engine.Create;
   engine.onCmdTerminate:=@AstrometryDone;
   engine.Resolver:=config.GetValue('/Astrometry/Resolver',ResolverAstrometryNet);
   FResolverName:=ResolverName[engine.Resolver];
   engine.ElbrusFolder:=config.GetValue('/Astrometry/ElbrusFolder','');
   engine.ElbrusUnixpath:=config.GetValue('/Astrometry/ElbrusUnixpath','');
   engine.LogFile:=logfile;
   engine.InFile:=infile;
   engine.OutFile:=outfile;
   tolerance:=config.GetValue('/Astrometry/ScaleTolerance',0.1);
   MinRadius:=config.GetValue('/Astrometry/MinRadius',5.0);
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
      engine.scalelow:=(1-tolerance)*pixscale;
      engine.scalehigh:=(1+tolerance)*pixscale;
   end;
   engine.downsample:=config.GetValue('/Astrometry/DownSample',4);
   engine.objs:=config.GetValue('/Astrometry/SourcesLimit',150);
   engine.plot:=config.GetValue('/Astrometry/Plot',false);
   engine.ra:=ra;
   engine.de:=de;
   engine.radius:=max(MinRadius,pixscale*iwidth/3600);
   FBusy:=true;
   engine.Resolve;
   msg('Resolving using '+ResolverName[engine.Resolver]+' ...');
   if Assigned(FonStartAstrometry) then FonStartAstrometry(self);
   result:=true;
 end else begin
   result:=false;
 end;
end;

procedure TAstrometry.AstrometryDone(Sender: TObject);
begin
 if FileExistsUTF8(savefile) and FileExistsUTF8(solvefile) then
   FLastResult:=true
 else
   FLastResult:=false;
 FBusy:=false;
 if Assigned(FonEndAstrometry) then FonEndAstrometry(self);
 if Assigned(Fterminatecmd) then Fterminatecmd(self);
 Fterminatecmd:=nil;
end;

procedure TAstrometry.StopAstrometry;
begin
  if FBusy then engine.Stop;
end;

end.

