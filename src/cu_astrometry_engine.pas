unit cu_astrometry_engine;

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

uses  u_global, u_utils, cu_fits,
  {$ifdef unix}
  Unix, BaseUnix,
  {$endif}
  LCLIntf, math, UTF8Process, process, FileUtil, Classes, SysUtils,u_translation;

type
TAstrometry_engine = class(TThread)
   private
     FInFile, FOutFile, FLogFile, FElbrusFile, FElbrusDir, FElbrusFolder, FElbrusUnixpath, FCygwinPath, wcsfile, apmfile, ps3file: string;
     FPlateSolveFolder,FPlateSolve3Cmd,FASTAPFolder,FAstrometryPath,FAstapLogFile: string;
     Fscalelow,Fscalehigh,Fra,Fde,Fradius,FTimeout,FXsize,FYsize: double;
     FObjs,FDown,FResolver,FPlateSolveWait,Fiwidth,Fiheight: integer;
     Fresult,Fcode:integer;
     FASTAPSearchRadius,FASTAPdownsample: integer;
     Fcmd: string;
     FOtherOptions: string;
     FUseScript,FUseWSL: Boolean;
     FCustomScript: string;
     Fparam: TStringList;
     process: TProcessUTF8;
     Ftmpfits: TFits;
     FFallback,Fretry,FKilled: boolean;
     FRetryEngine:TAstrometry_engine;
   protected
     procedure Execute; override;
     function Apm2Wcs: boolean;
     function PS3Wcs:boolean;
     procedure AstapWcs(out warn: string);
     function AstapErr(errnum:integer):string;
   public
     constructor Create;
     destructor Destroy; override;
     procedure Assign(Source: TAstrometry_engine);
     procedure Resolve;
     procedure Stop;
     property Resolver: integer read FResolver write FResolver;
     property InFile: string read FInFile write FInFile;
     property OutFile: string read FOutFile write FOutFile;
     property LogFile: string read FLogFile write FLogFile;
     property scalelow: double read Fscalelow write Fscalelow;
     property scalehigh: double read Fscalehigh write Fscalehigh;
     property ra: double read Fra write Fra;
     property de: double read Fde write Fde;
     property radius: double read Fradius write Fradius;
     property Xsize: double read FXsize write FXsize;
     property Ysize: double read FYsize write FYsize;
     property iwidth: integer read Fiwidth write Fiwidth;
     property iheight: integer read Fiheight write Fiheight;
     property timeout: double read FTimeout write FTimeout;
     property objs: integer read FObjs write FObjs;
     property downsample: integer read FDown write FDown;
     property OtherOptions: string read FOtherOptions write FOtherOptions;
     property UseScript: boolean read FUseScript write FUseScript;
     property CustomScript: string read FCustomScript write FCustomScript;
     property result: integer read Fresult;
     property param: TStringList read Fparam write Fparam;
     property AstrometryPath: string read FAstrometryPath write FAstrometryPath;
     property CygwinPath: string read FCygwinPath write FCygwinPath;
     property ElbrusFolder: string read FElbrusFolder write FElbrusFolder;
     property ElbrusUnixpath: string read FElbrusUnixpath write FElbrusUnixpath;
     property PlateSolveFolder: string read FPlateSolveFolder write FPlateSolveFolder;
     property PlateSolve3Cmd: string read FPlateSolve3Cmd write FPlateSolve3Cmd;
     property PlateSolveWait:integer read FPlateSolveWait write FPlateSolveWait;
     property ASTAPFolder: string read FASTAPFolder write FASTAPFolder;
     property ASTAPSearchRadius: integer read FASTAPSearchRadius write FASTAPSearchRadius;
     property ASTAPdownsample: Integer read FASTAPdownsample write FASTAPdownsample;
     property Fallback: boolean read FFallback write FFallback;
end;

implementation
uses LazFileUtils;

constructor TAstrometry_engine.Create;
begin
  inherited create(true);
  FInFile:='';
  FOutFile:='';
  Fcmd:='';
  FOtherOptions:='';
  FUseScript:=false;
  FCustomScript:='';
  Fra:=NullCoord;
  Fde:=NullCoord;
  Fradius:=NullCoord;
  FResolver:=ResolverAstap;
  FObjs:=0;
  FDown:=0;
  Fscalelow:=0;
  Fscalehigh:=0;
  FPlateSolveWait:=0;
  FFallback:=false;
  Fretry:=false;
  Fparam:=TStringList.Create;
  process:=TProcessUTF8.Create(nil);
  FreeOnTerminate:=true;
end;

destructor TAstrometry_engine.Destroy;
begin
  process.Free;
  Fparam.Free;
  inherited Destroy;
end;

procedure TAstrometry_engine.Assign(Source: TAstrometry_engine);
begin
Resolver:=Source.Resolver ;
AstrometryPath:=Source.AstrometryPath ;
CygwinPath:=Source.CygwinPath ;
ElbrusFolder:=Source.ElbrusFolder ;
ElbrusUnixpath:=Source.ElbrusUnixpath ;
PlateSolve3Cmd:=Source.PlateSolve3Cmd ;
PlateSolveFolder:=Source.PlateSolveFolder ;
PlateSolveWait:=Source.PlateSolveWait ;
ASTAPFolder:=Source.ASTAPFolder ;
ASTAPSearchRadius:=Source.ASTAPSearchRadius ;
ASTAPdownsample:=Source.ASTAPdownsample ;
LogFile:=Source.LogFile ;
InFile:=Source.InFile ;
OutFile:=Source.OutFile ;
scalelow:=Source.scalelow ;
scalehigh:=Source.scalehigh ;
downsample:=Source.downsample ;
objs:=Source.objs ;
OtherOptions:=Source.OtherOptions ;
UseScript:= Source.UseScript ;
CustomScript:=Source.CustomScript ;
ra:=Source.ra ;
de:=Source.de ;
radius:=Source.radius ;
Xsize:=Source.Xsize ;
Ysize:=Source.Ysize ;
iwidth:=Source.iwidth ;
iheight:=Source.iheight ;
timeout:=Source.timeout ;
Fallback:=source.Fallback;
end;

procedure TAstrometry_engine.Stop;
{$ifdef unix}
const maxchild=100;
var hnd: Thandle;
    childs: array [0..maxchild] of THandle;
    i,childnum:integer;
    resp: Tstringlist;
{$else}
var  Kcmd,Kpos: string;
     Kparam: TStringList;
     Kprocess: TProcessUTF8;
{$endif}
begin
try
FKilled:=true;
if (FResolver=ResolverAstrometryNet) or Fretry then begin
{$ifdef unix}
  // Kill all child process
  if (process<>nil) and process.Running then begin
    resp:=Tstringlist.Create;
    hnd:=process.Handle;
    childnum:=0;
    childs[childnum]:=hnd;
    while (hnd>0)and(childnum<maxchild) do begin
      resp.clear;
      if (ExecProcess('pgrep -P '+inttostr(hnd),resp)=0)and(resp.count>0) then begin
         hnd:=StrToIntDef(resp[0],0);
      end else
         hnd:=0;
      if hnd>0 then begin
         inc(childnum);
         childs[childnum]:=hnd;
      end;
    end;
    for i:=childnum downto 0 do
       FpKill(childs[i],SIGKILL);
    resp.Free;
  end;
  if (process<>nil) then process.Active:=false;
{$else}
 if FUseWSL or FUseScript then begin
   // with windows script or wsl it is sufficient to kill the main process
   if (process<>nil) and process.Running then
     process.Terminate(1);
 end
 else begin
  // cygwin need to kill all process but do not include pgrep
  Kparam:=TStringList.Create;
  Kprocess:=TProcessUTF8.Create(nil);
  try
  Kcmd:=slash(Fcygwinpath)+slash('bin')+'bash.exe';
  Kpos:='$1';
  Kparam.Clear;
  Kparam.Add('--login');
  Kparam.Add('-c');
  Kparam.Add('"ps aux|grep backend| awk ''{print '+Kpos+'}'' | xargs -n1 kill "');
  Kprocess.Executable:=Kcmd;
  Kprocess.Parameters:=Kparam;
  Kprocess.ShowWindow:=swoHIDE;
  Kprocess.Execute;
  Kparam.Clear;
  Kparam.Add('--login');
  Kparam.Add('-c');
  Kparam.Add('"ps aux|grep solve-field| awk ''{print '+Kpos+'}'' | xargs -n1 kill "');
  Kprocess.Parameters:=Kparam;
  Kprocess.Execute;
  Kparam.Clear;
  Kparam.Add('--login');
  Kparam.Add('-c');
  Kparam.Add('"ps aux|grep astrometry-engine| awk ''{print '+Kpos+'}'' | xargs -n1 kill "');
  Kprocess.Parameters:=Kparam;
  Kprocess.Execute;
  finally
    Kprocess.Free;
    Kparam.Free;
  end;
  if (process<>nil) and process.Running then process.Active:=false;
 end;
{$endif}
end
else begin
  if (process<>nil) and process.Running then
    process.Terminate(1);
end;
if (FRetryEngine<>nil)and(not FRetryEngine.Finished) then FRetryEngine.Stop;
except
end;
end;

procedure TAstrometry_engine.Resolve;
var str: TStringList;
    i: integer;
    buf: String;
    {$ifdef mswindows}
    fIn,fDrive: string;
    {$endif}
begin
if (FResolver=ResolverAstrometryNet)or Fretry then begin
 wcsfile:=ChangeFileExt(FInFile,'.wcs');
 DeleteFileUTF8(wcsfile);
 if FUseScript then begin
   Fcmd:=ExpandFileNameUTF8(FCustomScript);
   buf:='--overwrite';
   if (Fscalelow>0)and(Fscalehigh>0) then begin
     buf:=buf+' --scale-low';
     buf:=buf+blank+FloatToStr(Fscalelow);
     buf:=buf+' --scale-high';
     buf:=buf+blank+FloatToStr(Fscalehigh);
     buf:=buf+' --scale-units';
     buf:=buf+' arcsecperpix';
   end;
   if (Fra<>NullCoord)and(Fde<>NullCoord)and(Fradius<>NullCoord) then begin
     buf:=buf+' --ra';
     buf:=buf+blank+FloatToStr(Fra);
     buf:=buf+' --dec';
     buf:=buf+blank+FloatToStr(Fde);
     buf:=buf+' --radius';
     buf:=buf+blank+FloatToStr(Fradius);
   end;
   if FObjs>0 then begin
     buf:=buf+' --objs';
     buf:=buf+blank+inttostr(FObjs);
   end;
   if FDown>1 then begin
     buf:=buf+' --downsample';
     buf:=buf+blank+inttostr(FDown);
   end;
   buf:=buf+' --no-plots';
   if FOtherOptions<>'' then begin
     str:=TStringList.Create;
     SplitRec(FOtherOptions,' ',str);
     for i:=0 to str.Count-1 do buf:=buf+blank+str[i];
     str.Free;
   end;
   Fparam.Add(FInFile);
   Fparam.Add(buf);
   Start;
 end else begin
  {$ifdef mswindows}
  FUseWSL:=false;
  Fcmd:=slash(Fcygwinpath)+slash('bin')+'bash.exe';
  {$ifdef cpu64}
  // Windows Subsystem for Linux cannot be called from a 32bit application
    if not FileExistsUTF8(fcmd) then begin
      buf:='C:\Windows\System32\wsl.exe';
      if FileExistsUTF8(buf)then begin
        Fcmd:=buf;
        FUseWSL:=true;
      end;
    end;
  {$endif}
  if not FUseWSL then begin
    Fparam.Add('--login');
    Fparam.Add('-c');
    buf:='" ';
  end
  else begin
    buf:='';
  end;
  buf:=buf+'solve-field ';
  buf:=buf+' --overwrite ';
  if (Fscalelow>0)and(Fscalehigh>0) then begin
    buf:=buf+'--scale-low ';
    buf:=buf+FloatToStr(Fscalelow);
    buf:=buf+' --scale-high ';
    buf:=buf+FloatToStr(Fscalehigh);
    buf:=buf+' --scale-units ';
    buf:=buf+' arcsecperpix ';
  end;
  if (Fra<>NullCoord)and(Fde<>NullCoord)and(Fradius<>NullCoord) then begin
    buf:=buf+' --ra ';
    buf:=buf+FloatToStr(Fra);
    buf:=buf+' --dec ';
    buf:=buf+FloatToStr(Fde);
    buf:=buf+' --radius ';
    buf:=buf+FloatToStr(Fradius);
  end;
  if FObjs>0 then begin
    buf:=buf+' --objs ';
    buf:=buf+inttostr(FObjs);
  end;
  if FDown>1 then begin
    buf:=buf+' --downsample ';
    buf:=buf+inttostr(FDown);
  end;
  buf:=buf+' --no-plots ';
  if FOtherOptions<>'' then begin
    str:=TStringList.Create;
    SplitRec(FOtherOptions,' ',str);
    for i:=0 to str.Count-1 do buf:=buf+blank+str[i];
    str.Free;
  end;
  fIn:=StringReplace(FInFile,'\','/',[rfReplaceAll]);
  if FUseWSL then begin
     fDrive:=LowerCase(copy(fIn,1,1));
     Delete(fIn,1,2);
     fIn:='/mnt/'+fDrive+fIn;
  end;
  buf:=buf+' ""'+fIn+'""'+blank;
  if not FUseWSL then buf:=buf+'"';
  Fparam.Add(buf);
  {$else}
  if FAstrometryPath='' then
    Fcmd:='solve-field'
  else begin
    Fcmd:=slash(FAstrometryPath)+'solve-field';
  end;
  Fparam.Add('--overwrite');
  if (Fscalelow>0)and(Fscalehigh>0) then begin
    Fparam.Add('--scale-low');
    Fparam.Add(FloatToStr(Fscalelow));
    Fparam.Add('--scale-high');
    Fparam.Add(FloatToStr(Fscalehigh));
    Fparam.Add('--scale-units');
    Fparam.Add('arcsecperpix');
  end;
  if (Fra<>NullCoord)and(Fde<>NullCoord)and(Fradius<>NullCoord) then begin
    Fparam.Add('--ra');
    Fparam.Add(FloatToStr(Fra));
    Fparam.Add('--dec');
    Fparam.Add(FloatToStr(Fde));
    Fparam.Add('--radius');
    Fparam.Add(FloatToStr(Fradius));
  end;
  if FObjs>0 then begin
    Fparam.Add('--objs');
    Fparam.Add(inttostr(FObjs));
  end;
  if FDown>1 then begin
    Fparam.Add('--downsample');
    Fparam.Add(inttostr(FDown));
  end;
  Fparam.Add('--no-plots');
  if FOtherOptions<>'' then begin
    str:=TStringList.Create;
    SplitRec(FOtherOptions,' ',str);
    for i:=0 to str.Count-1 do Fparam.Add(str[i]);
    str.Free;
  end;
  Fparam.Add(FInFile);
  {$endif}
  Start;
 end;
end
else if FResolver=ResolverElbrus then begin
  FElbrusFile:=ExtractFileName(FInFile);
  {$ifdef mswindows}
    FElbrusDir:=FElbrusFolder;
  {$else}
    FElbrusDir:=FElbrusUnixpath;
  {$endif}
  DeleteFileUTF8(slash(FElbrusDir)+'elbrus.pos');
  DeleteFileUTF8(slash(FElbrusDir)+'elbrus.sta');
  Copyfile(FInFile,slash(FElbrusDir)+FElbrusFile,[cffOverwriteFile]);
  Start;
end
else if FResolver=ResolverPlateSolve then begin
  {$ifdef mswindows}
    Fcmd:=slash(FPlateSolveFolder)+'PlateSolve2.exe';
  {$else}
    Fcmd:='wine';
    Fparam.Add(slash(FPlateSolveFolder)+'PlateSolve2.exe');
  {$endif}
  if (Fra<>NullCoord)and(Fde<>NullCoord) then begin
    buf:=FloatToStr(Fra*deg2rad)+','+
          FloatToStr(Fde*deg2rad)+','+
          FloatToStr(FXsize*deg2rad*0.98)+','+  //PlateSolve2 has problems with solving some images if dimensions are a fraction too large.
          FloatToStr(FYsize*deg2rad*0.98)+','+
          '999,'+
          FInFile+','+
          IntToStr(PlateSolveWait);
  end
  else begin
    buf:=FInFile;
  end;
  Fparam.Add(buf);
  apmfile:=ChangeFileExt(FInFile,'.apm');
  wcsfile:=ChangeFileExt(FInFile,'.wcs');
  DeleteFileUTF8(apmfile);
  DeleteFileUTF8(wcsfile);
  Start;
end
else if FResolver=ResolverPlateSolve3 then begin
  {$ifdef mswindows}
    Fcmd:=FPlateSolve3Cmd;
  {$else}
    Fcmd:='wine';
    Fparam.Add(FPlateSolve3Cmd);
  {$endif}
  if (Fra<>NullCoord)and(Fde<>NullCoord) then begin
    Fparam.Add(FInFile);
    Fparam.Add(FloatToStr(Fra*deg2rad));
    Fparam.Add(FloatToStr(Fde*deg2rad));
    Fparam.Add(FloatToStr(FXsize*deg2rad));
    Fparam.Add(FloatToStr(FYsize*deg2rad));
  end
  else begin
    Fparam.Add(FInFile);
  end;
  ps3file:=ExtractFileNameWithoutExt(FInFile)+'_PS3.txt';
  wcsfile:=ChangeFileExt(FInFile,'.wcs');
  DeleteFileUTF8(ps3file);
  DeleteFileUTF8(wcsfile);
  Start;
end
else if FResolver=ResolverAstap then begin
  {$ifdef mswindows}
    Fcmd:=slash(FASTAPFolder)+'astap.exe';
  {$else}
    Fcmd:=slash(FASTAPFolder)+'astap';
  {$endif}
  Fparam.Add('-log');
  Fparam.Add('-z');
  Fparam.Add(inttostr(FASTAPdownsample));
  Fparam.Add('-r');
  if (Fra<>NullCoord)and(Fde<>NullCoord) then
    Fparam.Add(inttostr(FASTAPSearchRadius))
  else
    Fparam.Add('180');  // no coordinates, try full sky search
  Fparam.Add('-f');
  Fparam.Add(FInFile);
  FAstapLogFile:=ChangeFileExt(FInFile,'.log');
  DeleteFileUTF8(FAstapLogFile);
  wcsfile:=ChangeFileExt(FInFile,'.wcs');
  DeleteFileUTF8(wcsfile);
  Start;
end
else if FResolver=ResolverNone then begin
  Start;
end;
end;

procedure TAstrometry_engine.Execute;
const READ_BYTES = 65536;
var n: LongInt;
    f: file;
    buf,err,warn: string;
    logok: boolean;
    cbuf: array[0..READ_BYTES] of char;
    ft,fl: TextFile;
    fn,imgdir,txt: string;
    i,nside,available: integer;
    endtime: double;
    mem,mwcs: TMemoryStream;
begin
try
err:='';
FKilled:=false;
if (FResolver=ResolverAstrometryNet)or Fretry then begin
  cbuf:='';
  if (FLogFile<>'') then begin
    AssignFile(f,FLogFile);
    rewrite(f,1);
    buf:=Fcmd;
    for i:=0 to Fparam.Count-1 do buf:=buf+' '+Fparam[i];
    buf:=buf+CRLF;
    cbuf:=buf;
    BlockWrite(f,cbuf,Length(buf));
    logok:=true;
  end
  else
    logok:=false;
  if FAstrometryPath<>'' then begin
    process.Environment.Add('PATH='+GetEnvironmentVariable('PATH')+':'+FAstrometryPath);
  end else begin
    process.Environment.Add('PATH='+GetEnvironmentVariable('PATH'));
  end;
  process.Executable:=Fcmd;
  process.Parameters:=Fparam;
  process.Environment.Add('ccdcielpython='+PythonCmd);
  process.Environment.Add('ccdcielscriptdir='+ScriptsDir);
  process.Environment.Add('ccdcielapikey='+config.GetValue('/Astrometry/AstOnlineKey',''));
  if not FUseWSL then process.Options:=[poUsePipes,poStderrToOutPut];
  {$ifdef mswindows}
  process.Environment.Add('SYSTEMROOT=C:\Windows');
  process.ShowWindow:=swoHIDE;
  {$endif}
  endtime:=now+FTimeout/secperday;
  try
  process.Execute;
  while process.Running do begin
    if logok and (process.Output<>nil) then begin
      available:=process.Output.NumBytesAvailable;
      if available>0 then begin
        available:=min(available,READ_BYTES);
        n := process.Output.Read(cbuf, available);
        if n>=0 then BlockWrite(f,cbuf,n);
      end;
    end;
    if now>endtime then begin
       Stop;
       FKilled:=false;
       err:=rsTimeout+'!';
       if logok then begin
         buf:=rsTimeout+'!';
         cbuf:=buf;
         BlockWrite(f,cbuf,Length(buf));
       end;
       break;
    end;
    sleep(100);
  end;
  Fresult:=process.ExitStatus;
  if (logok)and(Fresult<>127)and(process.Output<>nil) then repeat
    available:=process.Output.NumBytesAvailable;
    if available>0 then begin
      available:=min(available,READ_BYTES);
      n := process.Output.Read(cbuf, available);
      if n>=0 then BlockWrite(f,cbuf,n);
    end;
  until (n<=0)or(available<=0)or(process.Output=nil);
  if (Fresult<>0)and(err='') then
     err:=Format(rsErrorResult, [IntToStr(Fresult)]);
  if logok and (Fresult<>0) then begin
     buf:=Format(rsErrorResult, [IntToStr(Fresult)]);
     cbuf:=buf;
     BlockWrite(f,cbuf,Length(buf));
  end;
  except
     Fresult:=1;
     err:=Format(rsErrorStartin, [Fcmd]);
     if logok then begin
       buf:=Format(rsErrorStartin, [Fcmd]);
       cbuf:=buf;
       BlockWrite(f,cbuf,Length(buf));
     end;
  end;
  process.Free;
  process:=TProcessUTF8.Create(nil);
  if logok then CloseFile(f);
  // merge wcs result
  if (Fresult=0)and(not FileExistsUTF8(FOutFile))and(FileExistsUTF8(wcsfile)) then begin
    Ftmpfits:=TFits.Create(nil);
    mem:=TMemoryStream.Create;
    try
    mem.LoadFromFile(FInFile);
    Ftmpfits.Stream:=mem;
    mwcs:=TMemoryStream.Create;
    mwcs.LoadFromFile(wcsfile);
    Ftmpfits.Header.NewWCS(mwcs);
    Ftmpfits.SaveToFile(FOutFile);
    finally
      Ftmpfits.Free;
      mwcs.Free;
    end;
  end;
  PostMessage(MsgHandle, LM_CCDCIEL, M_AstrometryDone, PtrInt(strnew(PChar(err))));
end
else if FResolver=ResolverElbrus then begin
  fn:=slash(FElbrusDir)+'elbrus.txt';
  imgdir:=trim(FElbrusFolder);
  if copy(imgdir,length(imgdir),1)<>'\' then imgdir:=imgdir+'\';
  AssignFile(ft,fn);
  rewrite(ft);
  write(ft,'1.- Elbrus command file'+crlf);    // CR+LF also on Linux
  write(ft,'2.- commands from CCDciel'+crlf);
  write(ft,'**SET imagePath'+crlf);
  write(ft,imgdir+FElbrusFile+crlf);
  write(ft,'**SET searchingCoordinatesFrom'+crlf);
  write(ft,'1'+crlf);
  write(ft,'**EXE analyze'+crlf);
  write(ft,'space'+crlf);
  CloseFile(ft);
  endtime:=now+FTimeout/secperday;
  repeat
    sleep(500);
  until FileExistsUTF8(slash(FElbrusDir)+'elbrus.sta') or (now>endtime);
  if (now>endtime) then err:=rsTimeout+'!';
  sleep(1000);
  if (FLogFile<>'') then begin
    if FileExistsUTF8(slash(FElbrusDir)+'elbrus.sta') then begin
       AssignFile(ft,slash(FElbrusDir)+'elbrus.sta');
       AssignFile(fl,FLogFile);
       Reset(ft);
       Rewrite(fl);
       repeat
         ReadLn(ft,txt);
         txt:=StringReplace(txt,#$b0,'d',[rfReplaceAll]);
         writeln(fl,txt);
       until eof(ft);
       CloseFile(ft);
       CloseFile(fl);
    end;
  end;
  if FileExistsUTF8(slash(FElbrusDir)+'elbrus.pos') then begin
    AssignFile(ft,slash(FElbrusDir)+'elbrus.pos');
    reset(ft);
    ReadLn(ft,txt);
    CloseFile(ft);
    nside:=StrToIntDef(copy(txt,1,3),0);
    if nside>=10 then begin
      Copyfile(slash(FElbrusDir)+FElbrusFile,FOutFile,[cffOverwriteFile]);
      fn:=ChangeFileExt(FInFile,'.solved');
      AssignFile(ft,fn);
      rewrite(ft);
      write(ft,' ');
      CloseFile(ft);
    end;
  end else begin
    if (FLogFile<>'') then begin
      AssignFile(ft,FLogFile);
      if FileExistsUTF8(slash(FElbrusDir)+'elbrus.sta') then
        Append(ft)
      else
        Rewrite(ft);
      WriteLn(ft, rsTimeout+'!');
      WriteLn(ft,'No response from Elbrus after 10 seconds.');
      WriteLn(ft,'Is Elbrus running and waiting for messages?');
      CloseFile(ft);
    end;
  end;
  PostMessage(MsgHandle, LM_CCDCIEL, M_AstrometryDone, PtrInt(strnew(PChar(err))));
end
else if FResolver=ResolverPlateSolve then begin
  process.Executable:=Fcmd;
  process.Parameters:=Fparam;
  endtime:=now+FTimeout/secperday;
  AssignFile(ft,FLogFile);
  Rewrite(ft);
  WriteLn(ft,Fcmd);
  for i:=0 to Fparam.Count-1 do
     WriteLn(ft,Fparam[i]);
  CloseFile(ft);
  try
  process.Execute;
  while process.Running do begin
    if now>endtime then begin
       err:=rsTimeout+'!';
       Stop;
       FKilled:=false;
       break;
    end;
    sleep(100);
  end;
  Fresult:=process.ExitStatus;
  except
     on E: Exception do begin
       Fresult:=1;
       err:='Fail to start Platesolve2:'+E.Message;
       AssignFile(ft,FLogFile);
       Append(ft);
       WriteLn(ft,'Fail to start Platesolve2:');
       WriteLn(ft,E.Message);
       CloseFile(ft);
     end;
  end;
  process.Free;
  process:=TProcessUTF8.Create(nil);
  // merge apm result
  if (Fresult=0)and(FileExistsUTF8(apmfile)) then begin
    if (FLogFile<>'') then begin
      AssignFile(ft,FLogFile);
      Append(ft);
      AssignFile(fl,apmfile);
      Reset(fl);
      WriteLn(ft,'Platesolve2 result:');
      while not eof(fl) do begin
        ReadLn(fl,buf);
        writeln(ft,buf);
      end;
      CloseFile(ft);
      CloseFile(fl);
    end;
    if Apm2Wcs then begin
      Ftmpfits:=TFits.Create(nil);
      mem:=TMemoryStream.Create;
      try
      mem.LoadFromFile(FInFile);
      Ftmpfits.Stream:=mem;
      mwcs:=TMemoryStream.Create;
      mwcs.LoadFromFile(wcsfile);
      Ftmpfits.Header.NewWCS(mwcs);
      Ftmpfits.SaveToFile(FOutFile);
      finally
        Ftmpfits.Free;
        mwcs.Free;
      end;
    end
    else begin
      if FFallback and (not Fretry) and (not FKilled) then begin
        PostMessage(MsgHandle, LM_CCDCIEL, M_AstrometryMsg, PtrInt( strnew(PChar(err+crlf+rsRetryWithAst))));
        FRetryEngine:=TAstrometry_engine.Create;
        FRetryEngine.Assign(self);
        FRetryEngine.Fallback:=false;
        FRetryEngine.Fretry:=true;
        FRetryEngine.Resolve;
        repeat
          sleep(100);
        until FRetryEngine.Finished;
        exit;
      end;
    end;
  end;
  PostMessage(MsgHandle, LM_CCDCIEL, M_AstrometryDone, PtrInt(strnew(PChar(err))));
end
else if FResolver=ResolverPlateSolve3 then begin
  process.Executable:=Fcmd;
  process.Parameters:=Fparam;
  endtime:=now+FTimeout/secperday;
  AssignFile(ft,FLogFile);
  Rewrite(ft);
  WriteLn(ft,Fcmd);
  for i:=0 to Fparam.Count-1 do
     WriteLn(ft,Fparam[i]);
  CloseFile(ft);
  try
  process.Execute;
  while process.Running do begin
    if now>endtime then begin
       err:=rsTimeout+'!';
       Stop;
       FKilled:=false;
       break;
    end;
    sleep(100);
  end;
  Fresult:=process.ExitStatus;
  except
     on E: Exception do begin
       Fresult:=1;
       err:='Fail to start Platesolve3:'+E.Message;
       AssignFile(ft,FLogFile);
       Append(ft);
       WriteLn(ft,'Fail to start Platesolve3:');
       WriteLn(ft,E.Message);
       CloseFile(ft);
     end;
  end;
  process.Free;
  process:=TProcessUTF8.Create(nil);
  // merge result
  if (Fresult=0)and(FileExistsUTF8(ps3file)) then begin
    if (FLogFile<>'') then begin
      AssignFile(ft,FLogFile);
      Append(ft);
      AssignFile(fl,ps3file);
      Reset(fl);
      WriteLn(ft,'Platesolve3 result:');
      while not eof(fl) do begin
        ReadLn(fl,buf);
        writeln(ft,buf);
      end;
      CloseFile(ft);
      CloseFile(fl);
    end;
    if PS3Wcs then begin
      Ftmpfits:=TFits.Create(nil);
      mem:=TMemoryStream.Create;
      try
      mem.LoadFromFile(FInFile);
      Ftmpfits.Stream:=mem;
      mwcs:=TMemoryStream.Create;
      mwcs.LoadFromFile(wcsfile);
      Ftmpfits.Header.NewWCS(mwcs);
      Ftmpfits.SaveToFile(FOutFile);
      finally
        Ftmpfits.Free;
        mwcs.Free;
      end;
    end
    else begin
      if FFallback and (not Fretry) and (not FKilled) then begin
        PostMessage(MsgHandle, LM_CCDCIEL, M_AstrometryMsg, PtrInt( strnew(PChar(err+crlf+rsRetryWithAst))));
        FRetryEngine:=TAstrometry_engine.Create;
        FRetryEngine.Assign(self);
        FRetryEngine.Fallback:=false;
        FRetryEngine.Fretry:=true;
        FRetryEngine.Resolve;
        repeat
          sleep(100);
        until FRetryEngine.Finished;
        exit;
      end;
    end;
  end;
  PostMessage(MsgHandle, LM_CCDCIEL, M_AstrometryDone, PtrInt(strnew(PChar(err))));
end
else if FResolver=ResolverAstap then begin
  process.Executable:=Fcmd;
  process.Parameters:=Fparam;
  endtime:=now+FTimeout/secperday;
  try
  process.Execute;
  while process.Running do begin
    if now>endtime then begin
       err:=rsTimeout+'!';
       Stop;
       FKilled:=false;
       break;
    end;
    sleep(100);
  end;
  Fresult:=process.ExitStatus;
  Fcode:=process.ExitCode;
  if (Fcode=0)and(Fresult<256) then Fcode:=Fresult;
  if (Fcode<>0)and(err='') then begin
     err:=AstapErr(Fcode);
  end;
  except
     on E: Exception do begin
       Fresult:=99;
       err:='Fail to start Astap:'+E.Message;
     end;
  end;
  if (FLogFile<>'')and(FAstapLogFile<>'')and(FLogFile<>FAstapLogFile)and FileExistsUTF8(FAstapLogFile) then begin
    CopyFile(FAstapLogFile,FLogFile,[cffOverwriteFile]);
  end;
  process.Free;
  process:=TProcessUTF8.Create(nil);
  // merge wcs result
  if (Fcode=0)and(FileExistsUTF8(wcsfile)) then begin
    Ftmpfits:=TFits.Create(nil);
    mem:=TMemoryStream.Create;
    try
    mem.LoadFromFile(FInFile);
    Ftmpfits.Stream:=mem;
    mwcs:=TMemoryStream.Create;
    AstapWcs(warn);
    if warn<>'' then begin
      err:=err+' '+warn;
      AssignFile(ft,FLogFile);
      Append(ft);
      WriteLn(ft,warn);
      CloseFile(ft);
    end;
    mwcs.LoadFromFile(wcsfile);
    Ftmpfits.Header.NewWCS(mwcs);
    Ftmpfits.SaveToFile(FOutFile);
    finally
      Ftmpfits.Free;
      mwcs.Free;
    end;
  end
  else begin
    if FFallback and (not Fretry) and (not FKilled) then begin
      PostMessage(MsgHandle, LM_CCDCIEL, M_AstrometryMsg, PtrInt( strnew(PChar(err+crlf+rsRetryWithAst))));
      FRetryEngine:=TAstrometry_engine.Create;
      FRetryEngine.Assign(self);
      FRetryEngine.Fallback:=false;
      FRetryEngine.Fretry:=true;
      FRetryEngine.Resolve;
      repeat
        sleep(100);
      until FRetryEngine.Finished;
      exit;
    end;
  end;
  PostMessage(MsgHandle, LM_CCDCIEL, M_AstrometryDone, PtrInt(strnew(PChar(err))));
end
else if FResolver=ResolverNone then begin
  err:=rsNoResolverCo;
  if (FLogFile<>'') then begin
    AssignFile(ft,FLogFile);
    rewrite(ft);
    WriteLn(ft,rsNoResolverCo);
    CloseFile(ft);
  end;
  PostMessage(MsgHandle, LM_CCDCIEL, M_AstrometryDone, PtrInt(strnew(PChar(err))));
end;
except
  on E: Exception do begin
    Fresult:=99;
    err:='Unexpected error: '+E.Message+crlf+err;
    PostMessage(MsgHandle, LM_CCDCIEL, M_AstrometryDone, PtrInt(strnew(PChar(err))));
  end;
end;
end;

function TAstrometry_engine.PS3Wcs:boolean;
var
  i : integer;
  f : textfile;
  line1, line2,line3,line4,line5 :string;
  hdr: THeaderBlock;
  fwcs: file of THeaderBlock;
  ra_radians,dec_radians,scale_radian,pixel_size,crota1,crota2,cdelt1,cdelt2:double;
  cd1_1,cd1_2,cd2_1,cd2_2: double;
  cdsign:  array [1..4] of double;
  List: TStrings;

begin
  result:=false;
  AssignFile(f,ps3file);
  // Reopen the file for reading
  Reset(f);
  readln(f,line1);
  readln(f,line2);
  readln(f,line3);
  readln(f,line4);
  readln(f,line5);
  closefile(f);
  if line1='True' then {line1 valid solution}
  begin
    List := TStringList.Create;
    try
       list.StrictDelimiter:=true;{only accept comma's}
       {now do line2}
       List.Clear;
       ExtractStrings([','], [], PChar(line2),List);
       if list.count<=2 then {commas between value, DOT as decimal separator}
       begin
         ra_radians:=strtofloat(list[0]);
         dec_radians:=strtofloat(list[1]);
       end
       else
       begin {commas between value, COMMA as decimal separator}
         ra_radians:=strtofloat(list[0]+'.'+list[1]);
         dec_radians:=strtofloat(list[2]+'.'+list[3]);
       end;
       {now do line3}
       List.Clear;
       ExtractStrings([','], [], PChar(line3),List);
       if list.count<=2 then {commas between value, DOT as decimal separator}
       begin
         scale_radian:=strtofloat(list[0]);
         crota2:=strtofloat(list[1]);
       end
       else
       begin {commas between value, COMMA as decimal separator}
         scale_radian:=strtofloat(list[0]+'.'+list[1]);
         crota2:=strtofloat(list[2]+'.'+list[3]);
       end;
       {line5}
       List.Clear;
       ExtractStrings([','], [], PChar(line5),List);
       if list.count<=8 then {commas between value, DOT as decimal separator}
       begin
         // a transformation matrix of unknow scale but with the right sign (inverted).
         cdsign[1] := sgn(-strtofloat(list[0]));
         cdsign[2] := sgn(-strtofloat(list[1]));
         cdsign[3] := sgn(-strtofloat(list[2]));
         cdsign[4] := sgn(-strtofloat(list[3]));
       end
       else
       begin  {commas between value, COMMA as decimal separator}
         cdsign[1] := sgn(-strtofloat(list[0]+'.'+list[1]));
         cdsign[2] := sgn(-strtofloat(list[2]+'.'+list[3]));
         cdsign[3] := sgn(-strtofloat(list[4]+'.'+list[5]));
         cdsign[4] := sgn(-strtofloat(list[6]+'.'+list[7]));
       end;
    finally
      List.Free;
    end;

    pixel_size:=3600*rad2deg/scale_radian;
    cdelt1:=pixel_size/3600;
    cdelt2:=pixel_size/3600;
    crota2:=deg2rad*(180-crota2);
    crota1:=crota2;
    // old_to_new_WCS
    cd1_1:=cdsign[1]*abs(cdelt1*cos(crota1));
    cd1_2:=cdsign[1]*abs(cdelt2*sin(crota1));
    cd2_1:=cdsign[1]*abs(cdelt1*sin(crota2));
    cd2_2:=cdsign[1]*abs(cdelt2*cos(crota2));

    // write header
    for i:=1 to 36 do
      hdr[i]:=blank80;
    hdr[1] := 'COMMENT   Solved by Platesolve 3 '+line4+blank80;
    hdr[2] := 'CTYPE1  = '+#39+'RA---TAN'+#39+'           / first parameter RA , projection TANgential'+blank80;
    hdr[3] := 'CTYPE2  = '+#39+'DEC--TAN'+#39+'           / second parameter DEC, projection TANgential'+blank80;
    hdr[4] := 'CUNIT1  = '+#39+'deg '+#39+'               / Unit of coordinate '+blank80;
    hdr[5] := 'CRPIX1  = '+Format('%20.10g',[ 0.5+Fiwidth/2])+' / X of reference pixel '+blank80;
    hdr[6] := 'CRPIX2  = '+Format('%20.10g',[ 0.5+Fiheight/2])+' / Y of reference pixel '+blank80;
    hdr[7] := 'CRVAL1  = '+Format('%20.13e',[ra_radians*rad2deg])+' / RA of reference pixel (deg) '+blank80;
    hdr[8] := 'CRVAL2  = '+Format('%20.13e',[dec_radians*rad2deg])+' / DEC of reference pixel (deg) '+blank80;
    // do not write CDELTi and CROTAi, the CD_ matrix is sufficient
    hdr[13] :='CD1_1   = '+Format('%20.13e',[cd1_1])+ ' / CD matrix to convert (x,y) to (Ra, Dec) '+blank80;
    hdr[14] :='CD1_2   = '+Format('%20.13e',[cd1_2])+ ' / CD matrix to convert (x,y) to (Ra, Dec) '+blank80;
    hdr[15] :='CD2_1   = '+Format('%20.13e',[cd2_1])+ ' / CD matrix to convert (x,y) to (Ra, Dec) '+blank80;
    hdr[16] :='CD2_2   = '+Format('%20.13e',[cd2_2])+ ' / CD matrix to convert (x,y) to (Ra, Dec) '+blank80;
    hdr[17] :='END'+blank80;

    // write wcs file
    AssignFile(fwcs,wcsfile);
    Rewrite(fwcs,1);
    BlockWrite(fwcs,hdr,sizeof(THeaderBlock));
    CloseFile(fwcs);

    // mark as solved
    AssignFile(f,ChangeFileExt(FInFile,'.solved'));
    rewrite(f);
    write(f,' ');
    CloseFile(f);
    result:=true;
  end
  else begin
    result:=false;
  end;
end;

function TAstrometry_engine.Apm2Wcs:boolean;
// communicated by Han Kleijn
var
  i,sign : integer;
  f : textfile;
  line1, line2,line3 :string;
  hdr: THeaderBlock;
  fwcs: file of THeaderBlock;
  ra_radians,dec_radians,pixel_size,crota1,crota2,cdelt1,cdelt2:double;
  cd1_1,cd1_2,cd2_1,cd2_2: double;
  flipped: boolean;
  List: TStrings;

begin
  result:=false;
  AssignFile(f,apmfile);
  // Reopen the file for reading
  Reset(f);
  while not Eof(f) do
  begin
    readln(f,line1);
    readln(f,line2);
    readln(f,line3);
  end;
  closefile(f);
  if length(line3)=20 then {valid solution}
  begin
    List := TStringList.Create;
    try
       list.StrictDelimiter:=true;{only accept comma's}
       {now do line1}
       List.Clear;
       ExtractStrings([','], [], PChar(line1),List);
       if list.count<=3 then {commas between value, DOT as decimal separator}
       begin
         ra_radians:=strtofloat(list[0]);
         dec_radians:=strtofloat(list[1]);
       end
       else
       begin {commas between value, COMMA as decimal separator}
         ra_radians:=strtofloat(list[0]+'.'+list[1]);
         dec_radians:=strtofloat(list[2]+'.'+list[3]);
       end;
       {now do line2}
       List.Clear;
       ExtractStrings([','], [], PChar(line2),List);
       if list.count<=5 then {commas between value, DOT as decimal separator}
       begin
         pixel_size:=strtofloat(list[0]);
         crota2:=strtofloat(list[1]);
         flipped:=pos('-',list[2])=0;{from yx ratio. In apm file yx ratio is reported inverse compared with PlateSolve2 window}
       end
       else
       begin {commas between value, COMMA as decimal separator}
         pixel_size:=strtofloat(list[0]+'.'+list[1]);
         crota2:=strtofloat(list[2]+'.'+list[3]);
         flipped:=pos('-',list[4])=0;{from yx ratio. In apm file yx ratio is reported inverse compared with PlateSolve2 window, Reading y/x ratio is difficult with comma separators. It could be written as "1" or "-0,9999" or "1,0011". So line2 could contain 7 or 8 comma's!! Therefore check only for "-"}
       end;
    finally
      List.Free;
    end;{try}

    if flipped then
    begin
     cdelt1:=-pixel_size/3600;
     cdelt2:=+pixel_size/3600;
    end
    else
    begin
      cdelt1:=+pixel_size/3600;
      cdelt2:=+pixel_size/3600;
      crota2:=180-crota2; {Non-standard angle reporting by Platesolve2.}
    end;
    crota1:=crota2; { coordinate grid is not skewed= > crota1=crota2}

    // old_to_new_WCS
    cd1_1:=cdelt1*cos(crota1*pi/180);
    if cdelt1>=0 then sign:=+1 else sign:=-1;
    cd1_2:=abs(cdelt2)*sign*sin(crota1*pi/180);
    if cdelt2>=0 then sign:=+1 else sign:=-1;
    cd2_1:=-abs(cdelt1)*sign*sin(crota2*pi/180);
    cd2_2:= cdelt2*cos(crota2*pi/180);

    // write header
    for i:=1 to 36 do
      hdr[i]:=blank80;
    hdr[1] := 'COMMENT   Solved by Platesolve 2'+blank80;
    hdr[2] := 'CTYPE1  = '+#39+'RA---TAN'+#39+'           / first parameter RA , projection TANgential'+blank80;
    hdr[3] := 'CTYPE2  = '+#39+'DEC--TAN'+#39+'           / second parameter DEC, projection TANgential'+blank80;
    hdr[4] := 'CUNIT1  = '+#39+'deg '+#39+'               / Unit of coordinate '+blank80;
    hdr[5] := 'CRPIX1  = '+Format('%20.10g',[ 0.5+Fiwidth/2])+' / X of reference pixel '+blank80;
    hdr[6] := 'CRPIX2  = '+Format('%20.10g',[ 0.5+Fiheight/2])+' / Y of reference pixel '+blank80;
    hdr[7] := 'CRVAL1  = '+Format('%20.10g',[ra_radians*rad2deg])+' / RA of reference pixel (deg) '+blank80;
    hdr[8] := 'CRVAL2  = '+Format('%20.10g',[dec_radians*rad2deg])+' / DEC of reference pixel (deg) '+blank80;
    hdr[9] := 'CDELT1  = '+Format('%20.10g',[cdelt1])+' / X pixel size (deg) '+blank80;
    hdr[10] :='CDELT2  = '+Format('%20.10g',[cdelt2])+' / Y pixel size (deg) '+blank80;
    hdr[11] :='CROTA1  = '+Format('%20.10g',[crota1])+' / Image twist of X axis (deg) '+blank80;
    hdr[12] :='CROTA2  = '+Format('%20.10g',[crota2])+' / Image twist of Y axis (deg) '+blank80;
    hdr[13] :='CD1_1   = '+Format('%20.10g',[cd1_1])+ ' / CD matrix to convert (x,y) to (Ra, Dec) '+blank80;
    hdr[14] :='CD1_2   = '+Format('%20.10g',[cd1_2])+ ' / CD matrix to convert (x,y) to (Ra, Dec) '+blank80;
    hdr[15] :='CD2_1   = '+Format('%20.10g',[cd2_1])+ ' / CD matrix to convert (x,y) to (Ra, Dec) '+blank80;
    hdr[16] :='CD2_2   = '+Format('%20.10g',[cd2_2])+ ' / CD matrix to convert (x,y) to (Ra, Dec) '+blank80;
    hdr[17] :='END'+blank80;

    // write wcs file
    AssignFile(fwcs,wcsfile);
    Rewrite(fwcs,1);
    BlockWrite(fwcs,hdr,sizeof(THeaderBlock));
    CloseFile(fwcs);

    // mark as solved
    AssignFile(f,ChangeFileExt(FInFile,'.solved'));
    rewrite(f);
    write(f,' ');
    CloseFile(f);
    result:=true;
  end
  else begin
    result:=false;
  end;
end;

procedure TAstrometry_engine.AstapWcs(out warn: string);
// Process ASTAP .wcs result
var
  i,n : integer;
  ft : textfile;
  mem: TMemoryStream;
  hl:array[1..80] of char;
  buf:string;
begin
    warn:='';
    mem:=TMemoryStream.Create;
    AssignFile(ft,wcsfile);
    reset(ft);
    n:=0;
    repeat
      ReadLn(ft,buf);
      hl:=copy(buf+blank80,1,80);
      if copy(hl,1,9)='WARNING =' then warn:=warn+trim(StringReplace(copy(hl,10,70),'''','',[rfReplaceAll]))+' ';
      mem.Write(hl,80);
      inc(n);
    until eof(ft);
    CloseFile(ft);
    hl:=blank80;
    for i:=0 to (36-(n div 36)) do mem.Write(hl,80);
    mem.Position:=0;
    mem.SaveToFile(wcsfile);
    mem.free;

    // mark as solved
    AssignFile(ft,ChangeFileExt(FInFile,'.solved'));
    rewrite(ft);
    write(ft,' ');
    CloseFile(ft);
end;

function TAstrometry_engine.AstapErr(errnum:integer):string;
begin
  case errnum of
     0: result:='No errors.';
     1: result:='No solution.';
     2: result:='Not enough stars detected.';
    16: result:='Error reading image file.';
    32: result:='No star database found.';
    33: result:='Error reading star database.';
    else result:='Unknow error: '+inttostr(errnum);
  end;
end;

end.

