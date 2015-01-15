unit cu_astrometry;

{$mode objfpc}{$H+}

{
Copyright (C) 2015 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

interface

uses  u_global, u_utils,
  {$ifdef unix}
  Unix, BaseUnix,
  {$endif}
  UTF8Process, process, Classes, SysUtils;

type
TAstrometry = class(TThread)
   private
     FInFile, FOutFile, FLogFile: string;
     Fscalelow,Fscalehigh,Fra,Fde,Fradius: double;
     FObjs,FDown: integer;
     Fplot: boolean;
     Fresult:integer;
     Fcmd: string;
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
     property result: integer read Fresult;
     property cmd: string read Fcmd write Fcmd;
     property param: TStringList read Fparam write Fparam;
     property onCmdTerminate: TNotifyEvent read FCmdTerminate write FCmdTerminate;
end;

implementation

constructor TAstrometry.Create;
begin
  FInFile:='';
  FOutFile:='';
  Fcmd:='';
  Fplot:=false;
  Fra:=NullCoord;
  Fde:=NullCoord;
  Fradius:=NullCoord;
  FObjs:=0;
  FDown:=0;
  Fscalelow:=0;
  Fscalehigh:=0;
  Fparam:=TStringList.Create;
  process:=TProcessUTF8.Create(nil);
  FreeOnTerminate:=true;
  inherited create(true);
end;

destructor TAstrometry.Destroy;
begin
  process.Free;
end;

procedure TAstrometry.Stop;
{$ifdef unix}
const maxchild=100;
var hnd: Thandle;
    childs: array [0..maxchild] of THandle;
    i,childnum:integer;
    resp: Tstringlist;
{$endif}
begin
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

procedure TAstrometry.Resolve;
begin
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
  Fparam.Add('--new-fits');
  Fparam.Add(FOutFile);
  Fparam.Add(FInFile);
  Start;
end;

procedure TAstrometry.Execute;
const READ_BYTES = 2048;
var n: LongInt;
    f: file;
    logok: boolean;
    cbuf: array[0..READ_BYTES] of char;
begin
  cbuf:='';
  if (LogFile<>'') then begin
    AssignFile(f,FLogFile);
    rewrite(f,1);
    logok:=true;
  end
  else
    logok:=false;
  process.Executable:=Fcmd;
  process.Parameters:=Fparam;
  process.Options:=[poUsePipes,poStderrToOutPut];
  process.Execute;
  while process.Running do begin
    if logok and (process.Output<>nil) then begin
      n := process.Output.Read(cbuf, READ_BYTES);
      if n>=0 then BlockWrite(f,cbuf,n);
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
end;

end.

