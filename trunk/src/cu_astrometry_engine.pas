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
  UTF8Process, process, FileUtil, Classes, SysUtils;

type
TAstrometry_engine = class(TThread)
   private
     FInFile, FOutFile, FLogFile, FElbrusFile, FElbrusDir, FElbrusFolder, FElbrusUnixpath : string;
     Fscalelow,Fscalehigh,Fra,Fde,Fradius: double;
     FObjs,FDown,FResolver: integer;
     Fplot: boolean;
     Fresult:integer;
     Fcmd: string;
     FOtherOptions: string;
     Fparam: TStringList;
     process: TProcessUTF8;
     FCmdTerminate: TNotifyEvent;
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
     property objs: integer read FObjs write FObjs;
     property downsample: integer read FDown write FDown;
     property plot: boolean read Fplot write Fplot;
     property OtherOptions: string read FOtherOptions write FOtherOptions;
     property result: integer read Fresult;
     property cmd: string read Fcmd write Fcmd;
     property param: TStringList read Fparam write Fparam;
     property ElbrusFolder: string read FElbrusFolder write FElbrusFolder;
     property ElbrusUnixpath: string read FElbrusUnixpath write FElbrusUnixpath;
     property onCmdTerminate: TNotifyEvent read FCmdTerminate write FCmdTerminate;
end;

implementation

constructor TAstrometry_engine.Create;
begin
  FInFile:='';
  FOutFile:='';
  Fcmd:='';
  Fplot:=false;
  FOtherOptions:='';
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
  inherited create(true);
end;

destructor TAstrometry_engine.Destroy;
begin
  process.Free;
end;

procedure TAstrometry_engine.Stop;
{$ifdef unix}
const maxchild=100;
var hnd: Thandle;
    childs: array [0..maxchild] of THandle;
    i,childnum:integer;
    resp: Tstringlist;
{$endif}
begin
if FResolver=ResolverAstrometryNet then begin
{$ifdef unix}
  // Kill all child process
  if process.Running then begin
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
  if process.Running then process.Active:=false;
{$endif}
end;
end;

procedure TAstrometry_engine.Resolve;
var str: TStringList;
    i: integer;
begin
if FResolver=ResolverAstrometryNet then begin
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
  Start;
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
end;
end;

procedure TAstrometry_engine.Execute;
const READ_BYTES = 2048;
var n: LongInt;
    f: file;
    buf: string;
    logok: boolean;
    cbuf: array[0..READ_BYTES] of char;
    ft,fl: TextFile;
    fn,imgdir,txt: string;
    i,nside: integer;
    timeout: double;
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
  timeout:=now+30/secperday;
  process.Execute;
  while process.Running do begin
    if logok and (process.Output<>nil) then begin
      n := process.Output.Read(cbuf, READ_BYTES);
      if n>=0 then BlockWrite(f,cbuf,n);
    end;
    if now>timeout then begin
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
  if logok then CloseFile(f);
  if Assigned(FCmdTerminate) then FCmdTerminate(self);
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
  timeout:=now+10/secperday;
  repeat
    sleep(500);
  until FileExistsUTF8(slash(FElbrusDir)+'elbrus.sta') or (now>timeout);
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
  if Assigned(FCmdTerminate) then FCmdTerminate(self);
end;
end;

end.

