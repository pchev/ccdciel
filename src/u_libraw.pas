unit u_libraw;

{$mode objfpc}{$H+}

interface

uses   dynlibs, u_global,
  {$ifdef mswindows}
  u_utils,
  {$endif}
  {$ifdef darwin}
  u_utils,
  {$endif}
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
      bitmap: pointer;                    // last element for version 1
    end;

    TRawInfo2 = record
      version: integer;                   // start of version 2
      camera: array [0..79] of Char;
      timestamp: {$ifdef cpu64}int64{$else}integer{$endif};
      isospeed: integer;
      shutter: single;
      aperture: single;
      focal_len: single;
      colors: integer;
      rmult: single;
      gmult: single;
      bmult: single;
      temperature: single;
    end;

    TRawBitmap = array of smallint;
    TLoadRaw = function(rawinput: Pchar; inputsize: integer):integer;  cdecl;
    TCloseRaw = function():integer;  cdecl;
    TGetRawInfo = function(var info:TRawInfo):integer;  cdecl;
    TGetRawInfo2 = function(var info:TRawInfo2):integer;  cdecl;
    TGetRawBitmap = function(var bitmap: pointer):integer;  cdecl;
    TGetRawErrorMsg = procedure(err:integer; msg: Pchar); cdecl;

Procedure Load_Libraw;

var libraw: TLibHandle;
    LoadRaw: TLoadRaw;
    CloseRaw: TCloseRaw;
    GetRawInfo: TGetRawInfo;
    GetRawInfo2: TGetRawInfo2;
    GetRawBitmap: TGetRawBitmap;
    GetRawErrorMsg: TGetRawErrorMsg;
    DcrawCmd, RawIdCmd, RawUnpCmd, Exiv2Cmd: string;

const
  rawext='.bay,.bmq,.cr3,.cr2,.crw,.cs1,.dc2,.dcr,.dng,.erf,.fff,.hdr,.k25,.kdc,.mdc,.mos,.mrw,.nef,.orf,.pef,.pxn,.raf,.raw,.rdc,.sr2,.srf,.x3f,.arw,.3fr,.cine,.ia,.kc2,.mef,.nrw,.qtk,.rw2,.sti,.rwl,.srw,';
  {$ifdef mswindows}
  librawname='libpasraw.dll';
  dcrawname='dcraw.exe';
  RawIdName='raw-identify.exe';
  RawUnpName='unprocessed_raw.exe';
  exiv2name='exiv2.exe';
  {$endif}
  {$ifdef linux}
  librawname='libpasraw.so.1';
  dcrawname='dcraw';
  RawIdName='raw-identify';
  RawUnpName='unprocessed_raw';
  exiv2name='exiv2';
  {$endif}
  {$ifdef darwin}
  librawname='libpasraw.dylib';
  dcrawname='dcraw';
  RawIdName='raw-identify';
  RawUnpName='unprocessed_raw';
  exiv2name='exiv2';
  {$endif}

implementation

Procedure Load_Libraw;
begin
  // try libraw
  try
  libraw := LoadLibrary(librawname);
  except
  end;
  try
  if libraw <> 0 then  begin
    DcrawCmd := '';
    RawIdCmd := '';
    RawUnpCmd:= '';
    LoadRaw := TLoadRaw(GetProcAddress(libraw, 'loadraw'));
    CloseRaw := TCloseRaw(GetProcAddress(libraw, 'closeraw'));
    GetRawInfo := TGetRawInfo(GetProcAddress(libraw, 'getinfo'));
    GetRawInfo2 := TGetRawInfo2(GetProcAddress(libraw, 'getinfo2'));
    GetRawErrorMsg := TGetRawErrorMsg(GetProcAddress(libraw, 'geterrormsg'));
  end;
  if (LoadRaw=nil)or(GetRawInfo=nil)or(CloseRaw=nil) then
     libraw:=0;
  except
  end;
  try
  // try libraw tools
  if libraw=0 then begin
    {$ifdef mswindows}
    RawIdCmd:=slash(Appdir)+RawIdName;
    if not fileexists(RawIdCmd) then RawIdCmd:='';
    RawUnpCmd:=slash(Appdir)+RawUnpName;
    if not fileexists(RawUnpCmd) then RawUnpCmd:='';
    {$endif}
    {$ifdef linux}
    RawIdCmd:='/usr/bin/'+RawIdName;
    if not fileexists(RawIdCmd) then begin
      RawIdCmd:='/usr/lib/libraw/'+RawIdName;
      if not fileexists(RawIdCmd) then begin
         RawIdCmd:='/usr/local/bin/'+RawIdName;
         if not fileexists(RawIdCmd) then begin
            RawIdCmd:=ExpandFileName('~/bin/'+RawIdName);
            if not fileexists(RawIdCmd) then RawIdCmd:='';
         end;
      end;
    end;
    RawUnpCmd:='/usr/bin/'+RawUnpName;
    if not fileexists(RawUnpCmd) then begin
      RawUnpCmd:='/usr/lib/libraw/'+RawUnpName;
      if not fileexists(RawUnpCmd) then begin
         RawUnpCmd:='/usr/local/bin/'+RawUnpName;
         if not fileexists(RawUnpCmd) then begin
            RawUnpCmd:=ExpandFileName('~/bin/'+RawUnpName);
            if not fileexists(RawUnpCmd) then RawIdCmd:='';
         end;
      end;
    end;
    {$endif}
    {$ifdef darwin}
    RawIdCmd:=slash(AppDir)+'ccdciel.app/Contents/MacOS/'+RawIdName;
    if not fileexists(RawIdCmd) then RawIdCmd:='';
    RawUnpCmd:=slash(AppDir)+'ccdciel.app/Contents/MacOS/'+RawUnpName;
    if not fileexists(RawUnpCmd) then RawUnpCmd:='';
    {$endif}
    if RawUnpCmd='' then RawIdCmd:='';
    if RawIdCmd='' then RawUnpCmd:='';
  end;
  except
  end;
  // try dcraw
  try
  if (libraw=0)and((RawUnpCmd='')or(RawIdCmd='')) then begin
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
  // exiv2
  {$ifdef mswindows}
  Exiv2Cmd:=slash(Appdir)+exiv2name;
  if not fileexists(Exiv2Cmd) then Exiv2Cmd:='';
  {$endif}
  {$ifdef linux}
  Exiv2Cmd:='/usr/bin/'+exiv2name;
  if not fileexists(Exiv2Cmd) then begin
     Exiv2Cmd:='/usr/local/bin/'+exiv2name;
     if not fileexists(Exiv2Cmd) then begin
        Exiv2Cmd:=ExpandFileName('~/bin/'+exiv2name);
        if not fileexists(Exiv2Cmd) then Exiv2Cmd:='';
     end;
  end;
  {$endif}
  {$ifdef darwin}
  Exiv2Cmd:=slash(AppDir)+'ccdciel.app/Contents/MacOS/'+exiv2name;
  if not fileexists(Exiv2Cmd) then Exiv2Cmd:='';
  {$endif}

end;

initialization
  libraw := 0;

finalization
  try
  LoadRaw := nil;
  CloseRaw := nil;
  GetRawInfo := nil;
  GetRawInfo2 := nil;
  GetRawErrorMsg := nil;
  if libraw <> 0 then FreeLibrary(libraw);
  except
  end;

end.

