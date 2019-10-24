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

var libraw: integer;
    LoadRaw: TLoadRaw;
    CloseRaw: TCloseRaw;
    GetRawInfo: TGetRawInfo;
    GetRawBitmap: TGetRawBitmap;
    GetRawErrorMsg: TGetRawErrorMsg;
    DcrawCmd: string;

const
  rawext='.bay,.bmq,.cr2,.crw,.cs1,.dc2,.dcr,.dng,.erf,.fff,.hdr,.k25,.kdc,.mdc,.mos,.mrw,.nef,.orf,.pef,.pxn,.raf,.raw,.rdc,.sr2,.srf,.x3f,.arw,.3fr,.cine,.ia,.kc2,.mef,.nrw,.qtk,.rw2,.sti,.rwl,.srw,';
  {$ifdef mswindows}
  libname='libpasraw.dll';
  {$endif}
  {$ifdef linux}
  libname='libpasraw.so.1';
  {$endif}
  {$ifdef darwin}
  libname='libpasraw.dylib';
  {$endif}

implementation

Procedure Load_Libraw;
begin
  try
  libraw := LoadLibrary(libname);
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
    DcrawCmd:=slash(Appdir)+'dcraw.exe';
    if not fileexists(DcrawCmd) then DcrawCmd:='';
    {$endif}
    {$ifdef linux}
    DcrawCmd:='/usr/bin/dcraw';
    if not fileexists(DcrawCmd) then begin
       DcrawCmd:='/usr/local/bin/dcraw';
       if not fileexists(DcrawCmd) then begin
          DcrawCmd:=ExpandFileName('~/bin/dcraw');
          if not fileexists(DcrawCmd) then DcrawCmd:='';
       end;
    end;
    {$endif}
    {$ifdef darwin}
    DcrawCmd:=slash(AppDir)+'ccdciel.app/Contents/MacOS/dcraw';
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

