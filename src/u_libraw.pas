unit u_libraw;

{$mode objfpc}{$H+}

interface

uses   dynlibs,
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

var libraw: integer;
    LoadRaw: TLoadRaw;
    CloseRaw: TCloseRaw;
    GetRawInfo: TGetRawInfo;
    GetRawBitmap: TGetRawBitmap;
    GetRawErrorMsg: TGetRawErrorMsg;

const
  rawext='.bay,.bmq,.cr2,.crw,.cs1,.dc2,.dcr,.dng,.erf,.fff,.hdr,.k25,.kdc,.mdc,.mos,.mrw,.nef,.orf,.pef,.pxn,.raf,.raw,.rdc,.sr2,.srf,.x3f,.arw,.3fr,.cine,.ia,.kc2,.mef,.nrw,.qtk,.rw2,.sti,.rwl,.srw,';

implementation

initialization
  libraw := LoadLibrary('libpasraw.so.1');
  if libraw <> 0 then  begin
    LoadRaw := TLoadRaw(GetProcAddress(libraw, 'loadraw'));
    CloseRaw := TCloseRaw(GetProcAddress(libraw, 'closeraw'));
    GetRawInfo := TGetRawInfo(GetProcAddress(libraw, 'getinfo'));
    GetRawErrorMsg := TGetRawErrorMsg(GetProcAddress(libraw, 'geterrormsg'));
  end;

finalization
  LoadRaw := nil;
  CloseRaw := nil;
  GetRawInfo := nil;
  GetRawErrorMsg := nil;
  if libraw <> 0 then FreeLibrary(libraw);

end.

