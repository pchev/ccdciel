unit u_libraw;

{$mode objfpc}{$H+}

interface

uses   dynlibs, u_global, u_utils,
  Classes, SysUtils;

type
    TRawInfo = record
      rawwidth: integer;
      rawheight: integer;
      imgwidth: integer;
      imgheight: integer;
      topmargin: integer;
      leftmargin: integer;
      bayerpattern: array [0..3] of Char;
      bitmap: pointer;
    end;
    TRawBitmap = array of smallint;
    TLoadRaw = function(rawinput: Pchar; inputsize: integer):integer;  cdecl;
    TCloseRaw = function():integer;  cdecl;
    TGetRawInfo = function(var info:TRawInfo):integer;  cdecl;
    TGetRawBitmap = function(var bitmap: pointer):integer;  cdecl;
    TGetRawErrorMsg = procedure(err:integer; msg: Pchar); cdecl;

Procedure Load_Libraw;

var libraw: TLibHandle;
    LoadRaw: TLoadRaw;
    CloseRaw: TCloseRaw;
    GetRawInfo: TGetRawInfo;
    GetRawBitmap: TGetRawBitmap;
    GetRawErrorMsg: TGetRawErrorMsg;
    DcrawCmd: string;

const
  rawext='.bay,.bmq,.cr3,.cr2,.crw,.cs1,.dc2,.dcr,.dng,.erf,.fff,.hdr,.k25,.kdc,.mdc,.mos,.mrw,.nef,.orf,.pef,.pxn,.raf,.raw,.rdc,.sr2,.srf,.x3f,.arw,.3fr,.cine,.ia,.kc2,.mef,.nrw,.qtk,.rw2,.sti,.rwl,.srw,';
  {$ifdef mswindows}
  librawname='libpasraw.dll';
  dcrawname='dcraw.exe';
  {$endif}
  {$ifdef linux}
  librawname='libpasraw.so.1';
  dcrawname='dcraw';
  {$endif}
  {$ifdef darwin}
  librawname='libpasraw.dylib';
  dcrawname='dcraw';
  {$endif}

implementation

Procedure Load_Libraw;
begin
  try
  libraw := LoadLibrary(librawname);
  except
  end;
  try
  if libraw <> 0 then  begin
    DcrawCmd := '';
    LoadRaw := TLoadRaw(GetProcAddress(libraw, 'loadraw'));
    CloseRaw := TCloseRaw(GetProcAddress(libraw, 'closeraw'));
    GetRawInfo := TGetRawInfo(GetProcAddress(libraw, 'getinfo'));
    GetRawErrorMsg := TGetRawErrorMsg(GetProcAddress(libraw, 'geterrormsg'));
  end;
  if (LoadRaw=nil)or(GetRawInfo=nil)or(CloseRaw=nil) then
     libraw:=0;
  except
  end;
  try
  if libraw=0 then begin
    {$ifdef mswindows}
    DcrawCmd:=slash(Appdir)+dcrawname;
    if not fileexists(DcrawCmd) then DcrawCmd:='';
    {$endif}
    {$ifdef linux}
    DcrawCmd:='/usr/bin/'+dcrawname;
    if not fileexists(DcrawCmd) then begin
       DcrawCmd:='/usr/local/bin/'+dcrawname;
       if not fileexists(DcrawCmd) then begin
          DcrawCmd:=ExpandFileName('~/bin/'+dcrawname);
          if not fileexists(DcrawCmd) then DcrawCmd:='';
       end;
    end;
    {$endif}
    {$ifdef darwin}
    DcrawCmd:=slash(AppDir)+'ccdciel.app/Contents/MacOS/'+dcrawname;
    if not fileexists(DcrawCmd) then
       DcrawCmd:='';
    {$endif}
  end;
  except
  end;
end;

initialization
  libraw := 0;

finalization
  try
  LoadRaw := nil;
  CloseRaw := nil;
  GetRawInfo := nil;
  GetRawErrorMsg := nil;
  if libraw <> 0 then FreeLibrary(libraw);
  except
  end;

end.

