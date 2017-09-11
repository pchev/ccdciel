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
  LCLIntf, math, UTF8Process, process, FileUtil, strutils, Classes, SysUtils;

type
TAstrometry_engine = class(TThread)
   private
     FInFile, FOutFile, FLogFile, FElbrusFile, FElbrusDir, FElbrusFolder, FElbrusUnixpath, FCygwinPath, wcsfile, apmfile : string;
     FPlateSolveFolder: string;
     Fscalelow,Fscalehigh,Fra,Fde,Fradius,FTimeout,FXsize,FYsize: double;
     FObjs,FDown,FResolver,FPlateSolveWait,Fiwidth,Fiheight: integer;
     Fplot: boolean;
     Fresult:integer;
     Fcmd: string;
     FOtherOptions: string;
     FUseScript:Boolean;
     FCustomScript: string;
     Fparam: TStringList;
     process: TProcessUTF8;
     Ftmpfits: TFits;
   protected
     procedure Execute; override;
     procedure Apm2Wcs;
   public
     constructor Create;
     destructor Destroy; override;
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
     property plot: boolean read Fplot write Fplot;
     property OtherOptions: string read FOtherOptions write FOtherOptions;
     property UseScript: boolean read FUseScript write FUseScript;
     property CustomScript: string read FCustomScript write FCustomScript;
     property result: integer read Fresult;
     property cmd: string read Fcmd write Fcmd;
     property param: TStringList read Fparam write Fparam;
     property CygwinPath: string read FCygwinPath write FCygwinPath;
     property ElbrusFolder: string read FElbrusFolder write FElbrusFolder;
     property ElbrusUnixpath: string read FElbrusUnixpath write FElbrusUnixpath;
     property PlateSolveFolder: string read FPlateSolveFolder write FPlateSolveFolder;
     property PlateSolveWait:integer read FPlateSolveWait write FPlateSolveWait;
end;

implementation
uses LazFileUtils;

constructor TAstrometry_engine.Create;
begin
  inherited create(true);
  FInFile:='';
  FOutFile:='';
  Fcmd:='';
  Fplot:=false;
  FOtherOptions:='';
  FUseScript:=false;
  FCustomScript:='';
  Fra:=NullCoord;
  Fde:=NullCoord;
  Fradius:=NullCoord;
  FResolver:=ResolverAstrometryNet;
  FObjs:=0;
  FDown:=0;
  Fscalelow:=0;
  Fscalehigh:=0;
  FPlateSolveWait:=0;
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

procedure TAstrometry_engine.Stop;
{$ifdef unix}
const maxchild=100;
var hnd: Thandle;
    childs: array [0..maxchild] of THandle;
    i,childnum:integer;
    resp: Tstringlist;
{$else}
var  Kcmd,buf: string;
     Kparam: TStringList;
     Kprocess: TProcessUTF8;
{$endif}
begin
if FResolver=ResolverAstrometryNet then begin
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
{$else}
  Kparam:=TStringList.Create;
  Kprocess:=TProcessUTF8.Create(nil);
  try
  Kcmd:=slash(Fcygwinpath)+slash('bin')+'bash.exe';
  Kparam.Clear;
  Kparam.Add('--login');
  Kparam.Add('-c');
  Kparam.Add('"ps aux|grep backend| awk ''{print $1}'' | xargs -n1 kill "');
  Kprocess.Executable:=Kcmd;
  Kprocess.Parameters:=Kparam;
  Kprocess.ShowWindow:=swoHIDE;
  Kprocess.Execute;
  Kparam.Clear;
  Kparam.Add('--login');
  Kparam.Add('-c');
  Kparam.Add('"ps aux|grep solve-field| awk ''{print $1}'' | xargs -n1 kill "');
  Kprocess.Parameters:=Kparam;
  Kprocess.Execute;
  finally
    Kprocess.Free;
    Kparam.Free;
  end;
  if (process<>nil) and process.Running then process.Active:=false;
{$endif}
end;
end;

procedure TAstrometry_engine.Resolve;
var str: TStringList;
    i: integer;
    buf: string;
begin
if FResolver=ResolverAstrometryNet then begin
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
   if not Fplot then begin
      buf:=buf+' --no-plots';
   end;
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
  Fcmd:=slash(Fcygwinpath)+slash('bin')+'bash.exe';
  Fparam.Add('--login');
  Fparam.Add('-c');
  buf:='"';
  buf:=buf+' solve-field ';
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
  if not Fplot then begin
     buf:=buf+' --no-plots ';
  end;
  if FOtherOptions<>'' then begin
    str:=TStringList.Create;
    SplitRec(FOtherOptions,' ',str);
    for i:=0 to str.Count-1 do buf:=buf+blank+str[i];
    str.Free;
  end;
  buf:=buf+' ""'+StringReplace(FInFile,'\','/',[rfReplaceAll])+'""'+blank;
  Fparam.Add(buf+'"');
  {$else}
  Fcmd:='solve-field';
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
  if not Fplot then begin
     Fparam.Add('--no-plots');
  end;
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
  buf:=FloatToStr(Fra*deg2rad)+','+
       FloatToStr(Fde*deg2rad)+','+
       FloatToStr(FXsize*deg2rad)+','+
       FloatToStr(FYsize*deg2rad)+','+
       '999,'+
       FInFile+','+
       IntToStr(PlateSolveWait);
  Fparam.Add(buf);
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
    buf: string;
    logok: boolean;
    cbuf: array[0..READ_BYTES] of char;
    ft,fl: TextFile;
    fn,imgdir,txt: string;
    i,nside,available: integer;
    endtime: double;
    mem: TMemoryStream;
begin
if FResolver=ResolverAstrometryNet then begin
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
  process.Executable:=Fcmd;
  process.Parameters:=Fparam;
  process.Options:=[poUsePipes,poStderrToOutPut];
  {$ifdef mswindows}
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
       if logok then begin
         buf:='Timeout!';
         cbuf:=buf;
         BlockWrite(f,cbuf,Length(buf));
       end;
    end;
    sleep(100);
  end;
  Fresult:=process.ExitStatus;
  if (logok)and(Fresult<>127)and(process.Output<>nil) then repeat
    n := process.Output.Read(cbuf, READ_BYTES);
    if n>=0 then BlockWrite(f,cbuf,n);
  until (n<=0)or(process.Output=nil);
  process.Free;
  process:=TProcessUTF8.Create(nil);
  except
     Fresult:=1;
     if logok then begin
       buf:='Error starting command: '+Fcmd;
       cbuf:=buf;
       BlockWrite(f,cbuf,Length(buf));
     end;
  end;
  if logok then CloseFile(f);
  // merge wcs result
  if (Fresult=0)and(not FileExistsUTF8(FOutFile))and(FileExistsUTF8(wcsfile)) then begin
    Ftmpfits:=TFits.Create(nil);
    mem:=TMemoryStream.Create;
    try
    mem.LoadFromFile(FInFile);
    Ftmpfits.Stream:=mem;
    mem.clear;
    mem.LoadFromFile(wcsfile);
    Ftmpfits.Header.NewWCS(mem);
    Ftmpfits.SaveToFile(FOutFile);
    except
      Ftmpfits.Free;
      mem.Free;
    end;
  end;
  PostMessage(MsgHandle, LM_CCDCIEL, M_AstrometryDone, 0);
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
      WriteLn(ft,'Timeout!');
      WriteLn(ft,'No response from Elbrus after 10 seconds.');
      WriteLn(ft,'Is Elbrus running and waiting for messages?');
      CloseFile(ft);
    end;
  end;
  PostMessage(MsgHandle, LM_CCDCIEL, M_AstrometryDone, 0);
end
else if FResolver=ResolverPlateSolve then begin
  process.Executable:=Fcmd;
  process.Parameters:=Fparam;
  try
  process.Execute;
  process.WaitOnExit;
  Fresult:=process.ExitStatus;
  process.Free;
  process:=TProcessUTF8.Create(nil);
  except
     Fresult:=1;
  end;
  // merge apm result
  apmfile:=ChangeFileExt(FInFile,'.apm');
  wcsfile:=ChangeFileExt(FInFile,'.wcs');
  if (Fresult=0)and(FileExistsUTF8(apmfile)) then begin
    Apm2Wcs;
    Ftmpfits:=TFits.Create(nil);
    mem:=TMemoryStream.Create;
    try
    mem.LoadFromFile(FInFile);
    Ftmpfits.Stream:=mem;
    mem.clear;
    mem.LoadFromFile(wcsfile);
    Ftmpfits.Header.NewWCS(mem);
    Ftmpfits.SaveToFile(FOutFile);
    except
      Ftmpfits.Free;
      mem.Free;
    end;
  end;
  PostMessage(MsgHandle, LM_CCDCIEL, M_AstrometryDone, 0);
end
else if FResolver=ResolverNone then begin
  if (FLogFile<>'') then begin
    AssignFile(ft,FLogFile);
    rewrite(ft);
    WriteLn(ft,'No astrometry resolver configured!');
    CloseFile(ft);
  end;
  PostMessage(MsgHandle, LM_CCDCIEL, M_AstrometryDone, 0);
end;
end;

procedure TAstrometry_engine.Apm2Wcs;
// comunicated by Han Kleijn
// simplified as ccdciel force decimal separator to .
var
   i,pos1,pos2,pos3,sign : integer;
   f     : textfile;
   line1, line2,line3 :string;
   hdr: THeaderBlock;
   fwcs: file of THeaderBlock;
   ra_radians,dec_radians,pixel_size,crota1,crota2,yx_ratio,cdelt1,cdelt2:double;
   cd1_1,cd1_2,cd2_1,cd2_2: double;
begin
  assign(f,apmfile);
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
    pos1:=posex(',',line1,1);
    pos2:=posex(',',line1,pos1+1);

    ra_radians:=strtofloat(copy(line1,1,pos1-1));
    dec_radians:=strtofloat(copy(line1,pos1+1,pos2-pos1-1));


    pos1:=posex(',',line2,1);
    pos2:=posex(',',line2,pos1+1);
    pos3:=posex(',',line2,pos2+1);

    pixel_size:=strtofloat(copy(line2,1,pos1-1));
    crota2:=strtofloat(copy(line2,pos1+1,pos2-pos1-1));
    yx_ratio:=strtofloat(copy(line2,pos2+1,pos3-pos2-1));{if positive flipped}

    cdelt1:=-pixel_size/3600;
    {yx ration is apm file is inverse compared with platesolve window}
    if yx_ratio>0 then
    begin
     cdelt1:=-pixel_size/3600;
     cdelt2:=+pixel_size/3600;
    end
    else
    begin
      cdelt1:=+pixel_size/3600;
      cdelt2:=+pixel_size/3600;
      crota2:=180-crota2; {very strange angle reporting in Platesolve2 ?? took me whole day to fixs this}
    end;
    crota1:=crota2;

    // old_to_new_WCS

    cd1_1:=cdelt1*cos(crota2*pi/180); {note 2013 should be crota1 if skewed}
    if cdelt1>=0 then sign:=+1 else sign:=-1;
    cd1_2:=abs(cdelt2)*sign*sin(crota2*pi/180);{note 2013 should be crota1 if skewed}
    if cdelt2>=0 then sign:=+1 else sign:=-1;
    cd2_1:=-abs(cdelt1)*sign*sin(crota2*pi/180);
    cd2_2:= cdelt2*cos(crota2*pi/180);

    // write header
    for i:=1 to 36 do
       hdr[i]:=blank80;
    hdr[1] := 'COMMENT    Solved by Platesolve 2'+blank80;
    hdr[2] := 'CTYPE1  = '+#39+'RA---TAN'+#39+'           / first parameter RA  ,  projection TANgential'+blank80;
    hdr[3] := 'CTYPE2  = '+#39+'DEC--TAN'+#39+'           / second parameter DEC,  projection TANgential'+blank80;
    hdr[4] := 'CUNIT1  = '+#39+'deg     '+#39+'           / Unit of coordinate                          '+blank80;
    hdr[5] := 'CRPIX1  = '+FormatFloat(e6,1+Fiwidth/2)+'        / X of reference pixel                  '+blank80;
    hdr[6] := 'CRPIX2  = '+FormatFloat(e6,1+Fiheight/2)+'        / Y of reference pixel                 '+blank80;
    hdr[7] := 'CRVAL1  = '+FormatFloat(e6,ra_radians*rad2deg)+'        / RA of reference pixel (deg)    '+blank80;
    hdr[8] := 'CRVAL2  = '+FormatFloat(e6,dec_radians*rad2deg)+'        / DEC of reference pixel (deg)  '+blank80;
    hdr[9] := 'CDELT1  = '+FormatFloat(e6,cdelt1)+'        / X pixel size (deg)                         '+blank80;
    hdr[10] := 'CDELT2  = '+FormatFloat(e6,cdelt2)+'        / Y pixel size (deg)                         '+blank80;
    hdr[11] := 'CROTA1  = '+FormatFloat(e6,crota1)+'        / Image twist of X axis        (deg)        '+blank80;
    hdr[12] := 'CROTA2  = '+FormatFloat(e6,crota2)+'        / Image twist of Y axis        (deg)        '+blank80;
    hdr[13] := 'CD1_1   = '+FormatFloat(e6,cd1_1)+'        / CD matrix to convert (x,y) to (Ra, Dec)    '+blank80;
    hdr[14] := 'CD1_2   = '+FormatFloat(e6,cd1_2)+'        / CD matrix to convert (x,y) to (Ra, Dec)    '+blank80;
    hdr[15] := 'CD2_1   = '+FormatFloat(e6,cd2_1)+'        / CD matrix to convert (x,y) to (Ra, Dec)    '+blank80;
    hdr[16] := 'CD2_2   = '+FormatFloat(e6,cd2_2)+'        / CD matrix to convert (x,y) to (Ra, Dec)    '+blank80;
    hdr[17] := 'END'+blank80;

    // write wcs file
    assign(fwcs,wcsfile);
    Rewrite(fwcs,1);
    BlockWrite(fwcs,hdr,sizeof(THeaderBlock));
    CloseFile(fwcs);

    // mark as solved
    AssignFile(f,ChangeFileExt(FInFile,'.solved'));
    rewrite(f);
    write(f,' ');
    CloseFile(f);
  end;


end;

end.

