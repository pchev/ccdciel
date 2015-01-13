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

uses
  UTF8Process, process, Classes, SysUtils;

type
TAstrometry = class(TThread)
   private
     FInFile, FOutFile, FLogFile: string;
     Fscalelow,Fscalehigh,Fra,Fde,Fradius: double;
     Fresult:integer;
     Fcmd: string;
     Fparam: TStringList;
     process: TProcessUTF8;
     FCmdTerminate: TNotifyEvent;
     procedure Execute; override;
   public
     constructor Create;
     destructor Destroy; override;
     procedure Resolve;
     property InFile: string read FInFile write FInFile;
     property OutFile: string read FOutFile write FOutFile;
     property LogFile: string read FLogFile write FLogFile;
     property scalelow: double read Fscalelow write Fscalelow;
     property scalehigh: double read Fscalehigh write Fscalehigh;
     property ra: double read Fra write Fra;
     property de: double read Fde write Fde;
     property radius: double read Fradius write Fradius;
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
  Fparam:=TStringList.Create;
  process:=TProcessUTF8.Create(nil);
  FreeOnTerminate:=true;
  inherited create(true);
end;

destructor TAstrometry.Destroy;
begin
  process.Free;
end;

procedure TAstrometry.Resolve;
begin
  Fcmd:='solve-field';
  Fparam.Add('--overwrite');
 // Fparam.Add('--no-plots');
  Fparam.Add('--index-xyls');
  Fparam.Add('none');
  Fparam.Add('--scale-low');
  Fparam.Add(FloatToStr(Fscalelow));
  Fparam.Add('--scale-high');
  Fparam.Add(FloatToStr(Fscalehigh));
  Fparam.Add('--scale-units');
  Fparam.Add('arcsecperpix');
  Fparam.Add('--ra');
  Fparam.Add(FloatToStr(Fra));
  Fparam.Add('--dec');
  Fparam.Add(FloatToStr(Fde));
  Fparam.Add('--radius');
  Fparam.Add(FloatToStr(Fradius));
  Fparam.Add('--rdls');
  Fparam.Add('none');
  Fparam.Add('--corr');
  Fparam.Add('none');
  Fparam.Add('--match');
  Fparam.Add('none');
  Fparam.Add('--new-fits');
  Fparam.Add(FOutFile);
  Fparam.Add(FInFile);
  Start;
end;

procedure TAstrometry.Execute;
var l: TStringList;
begin
  process.Executable:=Fcmd;
  process.Parameters:=Fparam;
  process.Options:=[poUsePipes,poStderrToOutPut];
  process.Execute;
  process.WaitOnExit;
  Fresult:=process.ExitStatus;
  if (LogFile<>'')and(process.Output<>nil) then begin
    l:=TStringList.Create;
    l.LoadFromStream(process.Output);
    l.SaveToFile(FLogFile);
    l.Free;
  end;
  if Assigned(FCmdTerminate) then FCmdTerminate(self);
end;

end.

