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

uses  u_global, u_utils,
  {$ifdef unix}
  Unix, BaseUnix,
  {$endif}
  LCLIntf, math, UTF8Process, process, FileUtil, Classes, SysUtils;

type
TAstrometry_engine = class(TThread)
   private
     FInFile, FOutFile, FLogFile, FElbrusFile, FElbrusDir, FElbrusFolder, FElbrusUnixpath, FCygwinPath : string;
     Fscalelow,Fscalehigh,Fra,Fde,Fradius,FTimeout: double;
     FObjs,FDown,FResolver: integer;
     Fplot: boolean;
     Fresult:integer;
     Fcmd: string;
     FOtherOptions: string;
     FUseScript:Boolean;
     FCustomScript: string;
     Fparam: TStringList;
     process: TProcessUTF8;
   protected
     procedure Execute; override;
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
 if FUseScript then begin
   Fcmd:=FCustomScript;
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
   Fparam.Add(FOutFile);
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
  buf:=buf+' --new-fits ';
  buf:=buf+'""'+StringReplace(FOutFile,'\','/',[rfReplaceAll])+'""'+blank;
  buf:=buf+'""'+StringReplace(FInFile,'\','/',[rfReplaceAll])+'""'+blank;
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
  Fparam.Add('--new-fits');
  Fparam.Add(FOutFile);
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
  PostMessage(MsgHandle, LM_CCDCIEL, M_AstrometryDone, 0);
  //Synchronize(@SyncCmdTerminate);
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

end.

