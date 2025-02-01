unit u_utils;

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

{$mode delphi}{$H+}
interface

uses u_global, u_refraction, u_ephem,
     {$ifdef mswindows}
       Windows, registry, ActiveX, comobj, variants,
     {$endif}
     {$ifdef unix}
       unix,baseunix,
     {$endif}
     smtpsend, synautil, cu_tcpclient,
     process, Classes, LCLType, FileUtil, ComCtrls, MTPCPU,
     Math, SysUtils, Forms, Menus, ActnList, Controls, StdCtrls, Graphics;

function InvertF32(X : LongWord) : Single;
function InvertF64(X : Int64) : Double;
Procedure FormPos(form : Tform; x,y : integer);
Function FormEntry(aOwner:TComponent; lbl,defaultstr:string):string;
Function FormEntryCB(aOwner:TComponent; val:Tstrings; lbl,defaultstr:string):string;
function wordspace(str: string): string;
function words(str,sep : string; p,n : integer; isep:char=blank) : string;
procedure SplitRec(buf,sep:string; var arg: TStringList);
procedure Splitarg(buf, sep: string; var arg: TStringList);
Procedure SplitCmd(S : String; List : TStringList);
function Slash(nom : string) : string;
Function sgn(x:Double):Double ;
procedure SetMenuAccelerator(Amenu: TMenuItem; level: integer; var AccelList: array of string);
function ScriptListCompare(List: TStringList; Index1, Index2: Integer): Integer;
Function SXToStr(de: Double) : string;
Function RAToStr(ar: Double) : string;
Function DEToStr(de: Double) : string;
Function RAToStrB(ar: Double) : string;
Function DEToStrB(de: Double) : string;
Function ARToStr4(ar: Double; f: string; out d,m,s : string) : string;
Function StrToAR(dms : string) : double;
Function StrToDE(dms : string) : double;
Function ARToStr3(ar: Double) : string;
Function Str3ToAR(dms : string) : double;
Function DEToStr3(de: Double) : string;
Function Str3ToDE(dms : string) : double;
function DEToStrShort(de: double; digits: integer = 1): string;
function TimToStr(tim: double; sep: string = ':'; showsec: boolean = True): string;
procedure Str2RaDec(txt: string; out ra,de: double);
procedure ExecNoWait(cmd: string; title:string=''; hide: boolean=true);
Function ExecProcess(cmd: string; output: TStringList; ShowConsole:boolean=false): integer;
Function ExecProcessMem(cmd: string; output: TMemoryStream; out err: string; ShowConsole:boolean=false): integer;
Function ExecuteFile(const FileName: string): integer;
procedure Wait(wt:single=5);
function GetCdCPort:string;
function  Rmod(x,y:Double):Double;
function  to180(x:Extended):Extended;
function  to360(x:Extended):Extended;
function  RoundFloat(x:Double;prec:integer):Double;
function IsNumber(n : string) : boolean;
Function PadZeros(x : string ; l :integer) : string;
function DateTimetoJD(date: Tdatetime): double;
function DateTimetoJD0(date: Tdatetime): double;
function Jd(annee,mois,jour :INTEGER; Heure:double):double;
PROCEDURE Djd(jd:Double;OUT annee,mois,jour:INTEGER; OUT Heure:double);
function isodate(a,m,d : integer) : string;
function DateIso2DateTime(dt: string): double;
function jddate(jd: double) : string;
PROCEDURE PrecessionFK5(ti,tf : double; VAR ari,dei : double);  // Lieske 77
procedure PrecessionEcl(ti, tf: double; var l, b: double);
function PositionAngle(ac, dc, ar, de: double): double;
function AngularDistance(ar1,de1,ar2,de2 : Double) : Double;
function PlaneDistance(x1,y1,x2,y2: double): double;
procedure InitObservatory;
procedure Paralaxe(SideralTime,dist,ar1,de1 : double; var ar,de,q: double; jdcoord,jdnow: double);
function SidTim(jd0,ut,long : double; eqeq: double=0): double;
Function CurrentSidTim: double;
Function SidTimT(t:TDateTime): double;
PROCEDURE Eq2Hz(HH,DE : double ; out A,h : double; method: smallint = 2);
Procedure Hz2Eq(A,h : double; out hh,de : double; method: smallint = 2);
procedure ApparentToObserved(ra,de: double; out obsra,obsde: double);
procedure ObservedToApparent(obsra,obsde: double; out ra,de: double);
function AirMass(h: double): double;
function atmospheric_absorption(airmass: double):double;
Procedure cmdEq2Hz(ra,de : double ; out a,h : double);
Procedure cmdHz2Eq(a,h : double; out ra,de : double);
procedure GuiderScreen2Fits(x,y: integer; FlipVert: boolean; out xx,yy:integer);
procedure FinderScreen2Fits(x,y: integer;  FlipVert: boolean; out xx,yy:integer);
procedure Screen2Fits(x,y: integer;  FlipHorz,FlipVert: boolean; out xx,yy:integer);
procedure FinderFits2Screen(x,y: integer; FlipVert: boolean; out xx,yy: integer);
procedure Fits2Screen(x,y: integer; FlipHorz,FlipVert: boolean; out xx,yy: integer);
procedure Screen2CCD(x,y: integer; FlipHorz,FlipVert: boolean; vflip:boolean; out xx,yy:integer);
procedure CCD2Screen(x,y: integer; FlipHorz,FlipVert: boolean; vflip:boolean; out xx,yy:integer);
procedure CircleIntersect(x0,y0,r,x1,y1: integer; out xr,yr: integer);
procedure ResetTrackBar(tb:TTrackBar);
procedure LeastSquares(data: array of TDouble2; out a,b,r: double);
procedure Time_Alt(jd, ar, de, h: double; out hp1, hp2: double);
function TwilightAstro(dt:TDateTime; out HMorning,HEvening:double):boolean;
procedure SecondsToWait(tnow,dt: TDateTime; forcenextday: boolean; out wt: Integer; out nextday:boolean);
procedure LoadHorizon(fname: string);
function ObjTransit(ra,de: double; out ht:double; out i:integer):boolean;
function ObjRise(ra,de: double; out hr:double; out i:integer):boolean;
function ObjSet(ra,de: double; out hs:double; out i:integer):boolean;
function  MoonRiseSet(dt:TDateTime; out moonrise,moonset:double):boolean;
function  DarkNight(t:Tdatetime): boolean;
function InTimeInterval(t,begint, endt: double; st: double=0.5): integer;
procedure sofa_PM(p: coordvector; var r: double);
procedure sofa_S2C(theta, phi: double; var c: coordvector);
procedure sofa_C2S(p: coordvector; var theta, phi: double);
procedure sofa_CP(p: coordvector; var c: coordvector);
procedure sofa_SXP(s: double; p: coordvector; var sp: coordvector);
procedure sofa_PN(p: coordvector; var r: double; var u: coordvector);
procedure sofa_PMP(a, b: coordvector; var amb: coordvector);
procedure sofa_PPP(a, b: coordvector; var apb: coordvector);
function sofa_PDP(a, b: coordvector): double;
procedure sofa_RXP(r: rotmatrix; p: coordvector; var rp: coordvector);
procedure sofa_TR(r: rotmatrix; var rt: rotmatrix);
procedure sofa_RXR(a, b: rotmatrix; var atb: rotmatrix);
procedure sofa_Ir(var r: rotmatrix);
procedure sofa_Rz(psi: double; var r: rotmatrix);
procedure sofa_Ry(theta: double; var r: rotmatrix);
procedure sofa_Rx(phi: double; var r: rotmatrix);
function ltp_Ecliptic(epj: double): double;
function DecryptStr(Str, Pwd: string): string;
function EncryptStr(Str, Pwd: string; Encode: boolean = True): string;
function hextostr(str: string): string;
function strtohex(str: string): string;
procedure InitCoord(jdnow: double=0);
procedure nutationme(j: double; var nutl, nuto: double);
procedure aberrationme(j: double; var abe, abp: double);
procedure apparent_equatorial(var ra, de: double);
procedure apparent_equatorialV(var p1: coordvector);
procedure mean_equatorial(var ra, de: double;doaberration:boolean=true;donutation:boolean=true);
procedure J2000ToApparent(var ra, de: double);
procedure ApparentToJ2000(var ra, de: double);
procedure MountToLocal(mountjd:double; var ra, de: double);
procedure LocalToMount(mountjd:double; var ra, de: double);
procedure MountToJ2000(mountjd:double; var ra, de: double);
procedure J2000ToMount(mountjd:double; var ra, de: double);
procedure quicksort(var list: array of double; lo,hi: integer);
function SMedian(list: array of double; leng: integer): double;
procedure SortFilterListInc(var list: TStringList);
procedure SortFilterListDec(var list: TStringList);
function SystemInformation: string;
function AscomVersion: string;
function IndiVersion: string;
function AstrometryVersion(resolver:integer; cygwinpath,cmdpath:string; usescript:boolean):string;
function TempDisplay(cf:integer; t:double):double;
function TempCelsius(cf:integer; t:double):double;
function GetThreadCount: integer;
function email(Subject,Msg:string):string;
function isLocalIP(ip:string): boolean;
function IsProgramRunning(pgm: string): boolean;
function StartProgram(pgm, path: string; param:string=''; allowmultiinstance: boolean=false): boolean;
procedure StopProgram(pgm: string);
function SafeFileName(fn:string): string;
function ValidateCustomHeader(key:string): boolean;
function pseudomodal(f:Tform):TModalResult;
function checkconnection(host,port: string):boolean;
procedure spcolor(w: double; out red,green,blue:integer);
function SimpleDeltaT(dt: Tdatetime): double;

implementation

uses u_translation, u_speech;

const
  GregorianStart=15821015;
  GregorianStartJD=2299161;

var
  dummy_ext : extended;

function InvertF32(X : LongWord) : Single;
var  P : PbyteArray;
     temp : LongWord;
begin
    P:=@X;
    if (P[0]=$7F)or(P[0]=$FF) then result:=0   // IEEE-754 NaN
    else begin
    temp:=(P[0] shl 24) or (P[1] shl 16) or (P[2] shl 8) or (P[3]);
    move(temp,result,4);
    end;
end;

function InvertF64(X : Int64) : Double;
var  P : PbyteArray;
     temp : Int64;
begin
    P:=@X;
    if (P[0]=$7F)or(P[0]=$FF) then result:=0   // IEEE-754 NaN
    else begin
    temp:=4294967296 * ((P[0] shl 24) or (P[1] shl 16) or (P[2] shl 8) or (P[3])) + ((P[4] shl 24) or (P[5] shl 16) or (P[6] shl 8) or (P[7]));
    move(temp,result,8);
    end;
end;

Procedure FormPos(form : Tform; x,y : integer);
begin
with Form do begin
  if width>(Screen.width-ScreenMargin) then width:=Screen.width-ScreenMargin;
  if x>ScreenMargin then left:=x
     else left:=ScreenMargin;
  if left+width>(Screen.Width-ScreenMargin) then left:=Screen.Width-width-ScreenMargin;
  if left<0 then left:=0;
  if y>ScreenMargin then top:=y
     else top:=ScreenMargin;
  if height>(Screen.height-ScreenMargin) then height:=Screen.height-ScreenMargin;
  if top+height>(Screen.height-ScreenMargin) then top:=Screen.height-height-ScreenMargin;
  if top<0 then top:=0;
end;
end;

Function FormEntry(aOwner:TComponent; lbl,defaultstr:string):string;
var f: TForm;
    l: Tlabel;
    e: Tedit;
    b,c: TButton;
begin
  f:=TForm.Create(aOwner);
  l:=TLabel.Create(f);
  e:=TEdit.Create(f);
  c:=TButton.Create(f);
  b:=TButton.Create(f);
  l.Caption:=lbl;
  l.Parent:=f;
  e.Constraints.MinWidth:=200;
  e.Text:=defaultstr;
  e.Parent:=f;
  c.Caption:=rsCancel;
  c.ModalResult:=mrCancel;
  c.Parent:=f;
  c.Cancel:=true;
  b.Caption:=rsOK;
  b.ModalResult:=mrOK;
  b.Parent:=f;
  b.Default:=true;
  f.ChildSizing.ControlsPerLine:=2;
  f.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
  f.ChildSizing.HorizontalSpacing:=8;
  f.AutoSize:=true;
  FormPos(f,mouse.CursorPos.X,mouse.CursorPos.Y);
  f.ShowModal;
  if f.ModalResult=mrOK then
    result:=e.Text
  else
    result:='';
  f.free;
end;

Function FormEntryCB(aOwner:TComponent; val:Tstrings; lbl,defaultstr:string):string;
var f: TForm;
    l: Tlabel;
    e: TComboBox;
    b,c: TButton;
begin
  f:=TForm.Create(aOwner);
  l:=TLabel.Create(f);
  e:=TComboBox.Create(f);
  c:=TButton.Create(f);
  b:=TButton.Create(f);
  l.Caption:=lbl;
  l.Parent:=f;
  e.Constraints.MinWidth:=200;
  e.Items.Assign(val);
  e.Text:=defaultstr;
  e.Parent:=f;
  c.Caption:=rsCancel;
  c.ModalResult:=mrCancel;
  c.Parent:=f;
  c.Cancel:=true;
  b.Caption:=rsOK;
  b.ModalResult:=mrOK;
  b.Parent:=f;
  b.Default:=true;
  f.ChildSizing.ControlsPerLine:=2;
  f.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
  f.ChildSizing.HorizontalSpacing:=8;
  f.AutoSize:=true;
  FormPos(f,mouse.CursorPos.X,mouse.CursorPos.Y);
  f.ShowModal;
  if f.ModalResult=mrOK then
    result:=e.Text
  else
    result:=defaultstr;
  f.free;
end;

function wordspace(str: string): string;
var
  i: integer;
  c: char;
begin
  c := blank;
  Result := '';
  for i := 1 to length(str) do
  begin
    if str[i] = blank then
    begin
      if c <> blank then
        Result := Result + str[i];
    end
    else
      Result := Result + str[i];
    c := str[i];
  end;
end;

function words(str,sep : string; p,n : integer; isep:char=blank) : string;
var     i,j : Integer;
begin
result:='';
str:=trim(str);
for i:=1 to p-1 do begin
 j:=pos(isep,str);
 if j=0 then j:=length(str)+1;
 str:=trim(copy(str,j+1,length(str)));
end;
for i:=1 to n do begin
 j:=pos(isep,str);
 if j=0 then j:=length(str)+1;
 result:=result+trim(copy(str,1,j-1))+sep;
 str:=trim(copy(str,j+1,length(str)));
end;
end;

procedure SplitRec(buf,sep:string; var arg: TStringList);
var i,l:integer;
begin
arg.clear;
l:=length(sep);
while pos(sep,buf)<>0 do begin
 for i:=1 to length(buf) do begin
  if copy(buf,i,l) = sep then begin
      arg.add(copy(buf,1,i-1));
      delete(buf,1,i-1+l);
      break;
  end;
 end;
end;
arg.add(buf);
end;

// same as SplitRec but remove empty strings
procedure Splitarg(buf, sep: string; var arg: TStringList);
var
  i, j, k, l: integer;
begin
  arg.Clear;
  l := length(sep);
  while copy(buf, 1, l) = sep do
    Delete(buf, 1, 1);
  while pos(sep, buf) <> 0 do
  begin
    for i := 1 to length(buf) do
    begin
      if copy(buf, i, l) = sep then
      begin
        if copy(buf, i + l, l) = sep then
          continue;
        if copy(buf, 1, 1) = '"' then
        begin
          j := length(buf);
          for k := 2 to length(buf) do
          begin
            if copy(buf, k, 1) = '"' then
            begin
              j := k;
              break;
            end;
          end;
          arg.Add(copy(buf, 2, j - 2));
          Delete(buf, 1, j);
          while copy(buf, 1, l) = sep do
            Delete(buf, 1, 1);
          break;
        end
        else
        begin
          arg.add(copy(buf, 1, i - 1));
          Delete(buf, 1, i - 1 + l);
          break;
        end;
      end;
    end;
  end;
  if buf > '' then
    arg.add(buf);
end;

Procedure SplitCmd(S : String; List : TStringList);
  Function GetNextWord : String;
  Const
    WhiteSpace = [' ',#9,#10,#13];
    Literals = ['"',''''];
  Var
    Wstart,wend : Integer;
    InLiteral : Boolean;
    LastLiteral : char;
  begin
    WStart:=1;
    While (WStart<=Length(S)) and (S[WStart] in WhiteSpace) do
      Inc(WStart);
    WEnd:=WStart;
    InLiteral:=False;
    LastLiteral:=#0;
    While (Wend<=Length(S)) and (Not (S[Wend] in WhiteSpace) or InLiteral) do
      begin
      if S[Wend] in Literals then
        If InLiteral then
          InLiteral:=Not (S[Wend]=LastLiteral)
        else
          begin
          InLiteral:=True;
          LastLiteral:=S[Wend];
          end;
       inc(wend);
       end;
     Result:=Copy(S,WStart,WEnd-WStart);
     if  (Length(Result) > 0)
     and (Result[1] = Result[Length(Result)]) // if 1st char = last char and..
     and (Result[1] in Literals) then // it's one of the literals, then
       Result:=Copy(Result, 2, Length(Result) - 2); //delete the 2 (but not others in it)
     While (WEnd<=Length(S)) and (S[Wend] in WhiteSpace) do
       inc(Wend);
     Delete(S,1,WEnd-1);
  end;
Var
  W : String;
begin
  While Length(S)>0 do
    begin
    W:=GetNextWord;
    If (W<>'') then
      List.Add(W);
    end;
end;

function Slash(nom : string) : string;
begin
result:=trim(nom);
if copy(result,length(result),1)<>PathDelim then result:=result+PathDelim;
end;

procedure SetMenuAccelerator(Amenu: TMenuItem; level: integer; var AccelList: array of string);
var k,p: integer;
    txt,c: string;
begin
  if level>MaxMenulevel then exit;
  txt:=StringReplace(Amenu.Caption,'&','',[rfReplaceAll]);
  if (txt<>'')and(txt<>'-') then begin
    p:=1;
    c:=UpperCase(copy(txt,p,1));
    while (pos(c,AccelList[level])>0)or(c<'A')or(c>'Z') do begin
      inc(p);
      if p>=length(txt) then begin
         p:=1;
         c:=UpperCase(copy(txt,p,1));
         break;
      end;
      c:=UpperCase(copy(txt,p,1));
    end;
    if Amenu.Action<>nil
      then  TAction(Amenu.Action).Caption:=copy(txt,1,p-1)+'&'+copy(txt,p,999)
      else Amenu.Caption:=copy(txt,1,p-1)+'&'+copy(txt,p,999);
    AccelList[level]:=AccelList[level]+c;
  end;
  AccelList[level+1]:='';
  for k:=0 to Amenu.Count-1 do begin
     SetMenuAccelerator(Amenu[k],level+1,AccelList);
  end;
end;

Function sgn(x:Double):Double ;
begin
// sign function with zero positive
if x<0 then
   sgn:= -1
else
   sgn:=  1 ;
end ;

function ScriptListCompare(List: TStringList; Index1, Index2: Integer): Integer;
var buf1,buf2: string;
const ff=#$ff;
begin
  buf1:=List[Index1];
  buf2:=List[Index2];
  if copy(buf1,1,2)='T_' then buf1:=ff+buf1;
  if copy(buf2,1,2)='T_' then buf2:=ff+buf2;
  if  buf1=buf2 then result:=0
  else if  buf1>buf2 then result:=1
  else result:=-1;
end;

Function SXToStr(de: Double) : string;
var dd,min1,min,sec: Double;
    d,m,s : string;
begin
    dd:=Int(de);
    min1:=abs(de-dd)*60;
    if min1>=59.99166667 then begin
       dd:=dd+sgn(de);
       min1:=0.0;
    end;
    min:=Int(min1);
    sec:=(min1-min)*60;
    if sec>=59.5 then begin
       min:=min+1;
       sec:=0.0;
    end;
    str(abs(dd):2:0,d);
    if abs(dd)<10 then d:='0'+trim(d);
    if de<0 then d:='-'+d;
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    str(sec:2:0,s);
    if abs(sec)<9.5 then s:='0'+trim(s);
    result := d+':'+m+':'+s;
end;

Function RAToStr(ar: Double) : string;
var dd,min1,min,sec: Double;
    d,m,s : string;
begin
 if ar=NullCoord then
   result:='-'
 else begin
    dd:=Int(ar);
    min1:=abs(ar-dd)*60;
    if min1>=59.99166667 then begin
       dd:=dd+sgn(ar);
       if dd=24 then dd:=0;
       min1:=0.0;
    end;
    min:=Int(min1);
    sec:=(min1-min)*60;
    if sec>=59.5 then begin
       min:=min+1;
       sec:=0.0;
    end;
    str(dd:2:0,d);
    if abs(dd)<10 then d:='0'+trim(d);
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    str(sec:2:0,s);
    if abs(sec)<9.5 then s:='0'+trim(s);
    result := d+'h'+m+'m'+s+'s';
 end;
end;

Function DEToStr(de: Double) : string;
var dd,min1,min,sec: Double;
    d,m,s : string;
begin
 if de=NullCoord then
   result:='-'
 else begin
    dd:=Int(de);
    min1:=abs(de-dd)*60;
    if min1>=59.99166667 then begin
       dd:=dd+sgn(de);
       min1:=0.0;
    end;
    min:=Int(min1);
    sec:=(min1-min)*60;
    if sec>=59.5 then begin
       min:=min+1;
       sec:=0.0;
    end;
    str(abs(dd):2:0,d);
    if abs(dd)<10 then d:='0'+trim(d);
    if de<0 then d:='-'+d else d:='+'+d;
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    str(sec:2:0,s);
    if abs(sec)<9.5 then s:='0'+trim(s);
    result := d+'d'+m+'m'+s+'s';
 end;
end;

Function RAToStrB(ar: Double) : string;
var dd,min1,min,sec: Double;
    d,m,s : string;
begin
    dd:=Int(ar);
    min1:=abs(ar-dd)*60;
    if min1>=59.99166667 then begin
       dd:=dd+sgn(ar);
       if dd=24 then dd:=0;
       min1:=0.0;
    end;
    min:=Int(min1);
    sec:=(min1-min)*60;
    if sec>=59.5 then begin
       min:=min+1;
       sec:=0.0;
    end;
    str(dd:2:0,d);
    if abs(dd)<10 then d:='0'+trim(d);
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    str(sec:2:0,s);
    if abs(sec)<9.5 then s:='0'+trim(s);
    result := d+' '+m+' '+s;
end;

Function DEToStrB(de: Double) : string;
var dd,min1,min,sec: Double;
    d,m,s : string;
begin
    dd:=Int(de);
    min1:=abs(de-dd)*60;
    if min1>=59.99166667 then begin
       dd:=dd+sgn(de);
       min1:=0.0;
    end;
    min:=Int(min1);
    sec:=(min1-min)*60;
    if sec>=59.5 then begin
       min:=min+1;
       sec:=0.0;
    end;
    str(abs(dd):2:0,d);
    if abs(dd)<10 then d:='0'+trim(d);
    if de<0 then d:='-'+d else d:='+'+d;
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    str(sec:2:0,s);
    if abs(sec)<9.5 then s:='0'+trim(s);
    result := d+' '+m+' '+s;
end;

Function ARToStr4(ar: Double; f: string; out d,m,s : string) : string;
var dd,min1,min,sec: Double;
begin
    dd:=Int(ar);
    min1:=abs(ar-dd)*60;
    if min1>=59.99166667 then begin
       dd:=dd+sgn(ar);
       if dd=24 then dd:=0;
       min1:=0.0;
    end;
    min:=Int(min1);
    sec:=(min1-min)*60;
    if sec>=59.95 then begin
       min:=min+1;
       sec:=0.0;
    end;
    str(dd:2:0,d);
    if abs(dd)<10 then d:='0'+trim(d);
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    s:=FormatFloat(f,sec);
    result := d+'h'+m+'m'+s+'s';
end;

Function StrToAR(dms : string) : double;
type tseplist=array[1..3] of string;
var s,p : integer;
    t : string;
    sep: tseplist;
const
    sep1: tseplist = ('h','m','s');
    sep2: tseplist = (':',':',':');
    sep3: tseplist = (' ',' ',' ');
begin
try
if copy(dms,1,1)='-' then s:=-1 else s:=1;
sep:=sep1;
p:=pos(sep[1],dms);
if p=0 then begin
  sep:=sep2;
  p:=pos(sep[1],dms);
end;
if p=0 then begin
  sep:=sep3;
  p:=pos(sep[1],dms);
end;
if p=0 then
  result:=StrToFloatDef(trim(dms),NullCoord)
else begin
  t:=copy(dms,1,p-1); delete(dms,1,p);
  result:=StrToFloat(trim(t));
  p:=pos(sep[2],dms);
  if p=0 then
    result:=result+ s * StrToFloat(trim(dms)) / 60
  else begin
    t:=copy(dms,1,p-1); delete(dms,1,p);
    result:=result+ s * StrToFloat(trim(t)) / 60;
    p:=pos(sep[3],dms);
    if p=0 then
      t:=dms
    else
      t:=copy(dms,1,p-1);
    result:=result+ s * StrToFloat(trim(t)) / 3600;
  end;
end;
except
result:=NullCoord;
end;
end;

Function StrToDE(dms : string) : double;
type tseplist=array[1..3] of string;
var s,p,d1 : integer;
    t : string;
    sep: tseplist;
const
    sep1: tseplist = ('d','m','s');
    sep2: tseplist = ('°','''','"');
    sep3: tseplist = (#176,'''','"');
    sep4: tseplist = (':',':',':');
    sep5: tseplist = (' ',' ',' ');
begin
try
if copy(dms,1,1)='-' then s:=-1 else s:=1;
sep:=sep1;
d1:=length(sep[1])-1;
p:=pos(sep[1],dms);
if p=0 then begin
  sep:=sep2;
  d1:=length(sep[1])-1;
  p:=pos(sep[1],dms);
end;
if p=0 then begin
  sep:=sep3;
  d1:=length(sep[1])-1;
  p:=pos(sep[1],dms);
end;
if p=0 then begin
  sep:=sep4;
  d1:=length(sep[1])-1;
  p:=pos(sep[1],dms);
end;
if p=0 then begin
  sep:=sep5;
  d1:=length(sep[1])-1;
  p:=pos(sep[1],dms);
end;
if p=0 then
  result:=StrToFloatDef(trim(dms),NullCoord)
else begin
  t:=copy(dms,1,p-1); delete(dms,1,p+d1);
  result:=StrToFloat(trim(t));
  p:=pos(sep[2],dms);
  if p=0 then
    result:=result+ s * StrToFloat(trim(dms)) / 60
  else begin
    t:=copy(dms,1,p-1); delete(dms,1,p);
    result:=result+ s * StrToFloat(trim(t)) / 60;
    p:=pos(sep[3],dms);
    if p=0 then
      t:=dms
    else
      t:=copy(dms,1,p-1);
    result:=result+ s * StrToFloat(trim(t)) / 3600;
  end;
end;
except
result:=NullCoord;
end;
end;

Function DEToStr3(de: Double) : string;
var dd,min1,min,sec: Double;
    d,m,s : string;
begin
    dd:=Int(de);
    min1:=abs(de-dd)*60;
    if min1>=59.99166667 then begin
       dd:=dd+sgn(de);
       min1:=0.0;
    end;
    min:=Int(min1);
    sec:=(min1-min)*60;
    if sec>=59.5 then begin
       min:=min+1;
       sec:=0.0;
    end;
    str(abs(dd):2:0,d);
    if abs(dd)<10 then d:='0'+trim(d);
    if de<0 then d:='-'+d else d:='+'+d;
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    str(sec:2:0,s);
    if abs(sec)<9.5 then s:='0'+trim(s);
    result := d+'d'+m+'m'+s+'s';
end;

Function ARToStr3(ar: Double) : string;
var dd,min1,min,sec: Double;
    d,m,s : string;
begin
    dd:=Int(ar);
    min1:=abs(ar-dd)*60;
    if min1>=59.99166667 then begin
       dd:=dd+sgn(ar);
       if dd=24 then dd:=0;
       min1:=0.0;
    end;
    min:=Int(min1);
    sec:=(min1-min)*60;
    if sec>=59.5 then begin
       min:=min+1;
       sec:=0.0;
    end;
    str(dd:2:0,d);
    if abs(dd)<10 then d:='0'+trim(d);
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    str(sec:2:0,s);
    if abs(sec)<9.5 then s:='0'+trim(s);
    result := d+'h'+m+'m'+s+'s';
end;

function Str3ToAR(dms : string): double;
type tseplist=array[1..3] of string;
var s,p : integer;
    t : string;
    sep: tseplist;
const
    sep1: tseplist = ('h','m','s');
    sep2: tseplist = (':',':',':');
    sep3: tseplist = (' ',' ',' ');
begin
try
  if copy(dms,1,1)='-' then s:=-1 else s:=1;
  sep:=sep1;
  p:=pos(sep[1],dms);
  if p=0 then begin
    sep:=sep2;
    p:=pos(sep[1],dms);
  end;
  if p=0 then begin
    sep:=sep3;
    p:=pos(sep[1],dms);
  end;
  if p=0 then
    result:=StrToFloatDef(trim(dms),-9999)
  else begin
    t:=copy(dms,1,p-1); delete(dms,1,p);
    result:=StrToFloatDef(trim(t),0);
    p:=pos(sep[2],dms);
    if p=0 then
      result:=result+ s * StrToFloatDef(trim(dms),0) / 60
    else begin
      t:=copy(dms,1,p-1); delete(dms,1,p);
      result:=result+ s * StrToFloatDef(trim(t),0) / 60;
      dms:=StringReplace(dms,' ','',[rfReplaceAll]);
      p:=pos(sep[3],dms);
      if p=0 then
        t:=dms
      else
        t:=copy(dms,1,p-1);
      result:=result+ s * StrToFloatDef(trim(t),0) / 3600;
    end;
  end;
except
result:=-9999;
end;
end;

function Str3ToDE(dms : string): double;
type tseplist=array[1..3] of string;
var s,p,d1 : integer;
    t : string;
    sep: tseplist;
const
    sep1: tseplist = ('d','m','s');
    sep2: tseplist = ('°','''','"');
    sep3: tseplist = (#176,'''','"');
    sep4: tseplist = (':',':',':');
    sep5: tseplist = (' ',' ',' ');
begin
try
  dms:=StringReplace(dms,ldeg,'d',[]);
  dms:=StringReplace(dms,lmin,'m',[]);
  dms:=StringReplace(dms,lsec,'s',[]);
  if copy(dms,1,1)='-' then s:=-1 else s:=1;
  sep:=sep1;
  d1:=length(sep[1])-1;
  p:=pos(sep[1],dms);
  if p=0 then begin
    sep:=sep2;
    d1:=length(sep[1])-1;
    p:=pos(sep[1],dms);
  end;
  if p=0 then begin
    sep:=sep3;
    d1:=length(sep[1])-1;
    p:=pos(sep[1],dms);
  end;
  if p=0 then begin
    sep:=sep4;
    d1:=length(sep[1])-1;
    p:=pos(sep[1],dms);
  end;
  if p=0 then begin
    sep:=sep5;
    d1:=length(sep[1])-1;
    p:=pos(sep[1],dms);
  end;
  if p=0 then
    result:=StrToFloatDef(trim(dms),-9999)
  else begin
    t:=copy(dms,1,p-1); delete(dms,1,p+d1);
    result:=StrToFloatDef(trim(t),0);
    p:=pos(sep[2],dms);
    if p=0 then
      result:=result+ s * StrToFloatDef(trim(dms),0) / 60
    else begin
      t:=copy(dms,1,p-1); delete(dms,1,p);
      result:=result+ s * StrToFloatDef(trim(t),0) / 60;
      p:=pos(sep[3],dms);
      if p=0 then
        t:=dms
      else
        t:=copy(dms,1,p-1);
      result:=result+ s * StrToFloatDef(trim(t),0) / 3600;
    end;
  end;
except
 result:=-9999;
end;
end;

function DEToStrShort(de: double; digits: integer = 1): string;
var
  dd, min1, min, sec: double;
  sg, d, m, s: string;
begin
  if de >= 0 then
    sg := ''
  else
    sg := '-';
  de := abs(de);
  dd := Int(de);
  min1 := abs(de - dd) * 60;
  if min1 >= 59.99166667 then
  begin
    dd := dd + sgn(de);
    min1 := 0.0;
  end;
  min := Int(min1);
  sec := (min1 - min) * 60;
  if sec >= 59.5 then
  begin
    min := min + 1;
    sec := 0.0;
  end;
  str(abs(dd): 2: 0, d);
  if abs(dd) < 10 then
    d := '0' + trim(d);
  if de < 0 then
    d := '-' + d
  else
    d := '+' + d;
  str(min: 2: 0, m);
  if abs(min) < 10 then
    m := '0' + trim(m);
  str(sec: 2: digits, s);
  if abs(sec) < 9.5 then
    s := '0' + trim(s);
  Result := sg;
  if dd <> 0 then
    Result := Result + d + ldeg;
  if min <> 0 then
    Result := Result + m + lmin;
  Result := Result + s + lsec;
end;

procedure ExecNoWait(cmd: string; title:string=''; hide: boolean=true);
{$ifdef unix}
begin
 fpSystem(cmd+' &');
end;
{$endif}
{$ifdef mswindows}
var
   bchExec: array[0..1024] of char;
   pchEXEC: Pchar;
   si: TStartupInfo;
   pi: TProcessInformation;
begin
   pchExec := @bchExec;
   StrPCopy(pchExec,cmd);
   FillChar(si{%H-},sizeof(si),0);
   FillChar(pi{%H-},sizeof(pi),0);
   si.dwFlags:=STARTF_USESHOWWINDOW;
   if title<>'' then si.lpTitle:=Pchar(title);
   if hide then si.wShowWindow:=SW_SHOWMINIMIZED
           else si.wShowWindow:=SW_SHOWNORMAL;
   si.cb := sizeof(si);
   try
     CreateProcess(Nil,pchExec,Nil,Nil,false,CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, Nil,Nil,si,pi);
    except;
    end;
end;
{$endif}

Function ExecProcessMem(cmd: string; output: TMemoryStream; out err: string; ShowConsole:boolean=false): integer;
var
  P: TProcess;
  param: TStringList;
  n,i: LongInt;
  BytesRead: LongInt;
  cerr: array[0..1024]of char;
begin
P := TProcess.Create(nil);
param:=TStringList.Create;
result:=1;
err:='';
cerr:='';
try
  BytesRead := 0;
  SplitCmd(cmd,param);
  cmd:= param[0];
  param.Delete(0);
  P.Executable:=cmd;
  P.Parameters:=param;
  if ShowConsole then begin
     P.ShowWindow:=swoShowNormal;
     P.StartupOptions:=[suoUseShowWindow];
  end else begin
     P.ShowWindow:=swoHIDE;
  end;
  if output<>nil then P.Options := [poUsePipes];
  P.Execute;
  while P.Running do begin
    if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
    if (output<>nil) and (P.Output<>nil) then begin
      i:=P.Output.NumBytesAvailable;
      if i>0 then begin
        output.SetSize(BytesRead + i);
        n := P.Output.Read((output.Memory + BytesRead)^, i);
        if n > 0 then inc(BytesRead, n);
      end;
    end;
    if (P.Stderr<>nil) then begin
      i:=P.Stderr.NumBytesAvailable;
      if i>0 then begin
        n:=P.Stderr.Read(cerr, i);
        err:=err+trim(cerr);
      end;
    end;
  end;
  result:=P.ExitStatus;
  if (output<>nil) and (result<>127)and(P.Output<>nil) then repeat
    i:=P.Output.NumBytesAvailable;
    if i>0 then begin
      output.SetSize(BytesRead + i);
      n := P.Output.Read((output.Memory + BytesRead)^, i);
      if n > 0 then inc(BytesRead, n);
    end;
  until (n<=0)or(i<=0)or(P.Output=nil);
  if (P.Stderr<>nil) then begin
    i:=P.Stderr.NumBytesAvailable;
    if i>0 then begin
      n:=P.Stderr.Read(cerr, i);
      err:=err+trim(cerr);
    end;
  end;
  P.Free;
  param.Free;
except
  on E: Exception do begin
    result:=-1;
    err:=err+E.Message;
    P.Free;
    param.Free;
  end;
end;
end;

Function ExecProcess(cmd: string; output: TStringList; ShowConsole:boolean=false): integer;
var
  M: TMemoryStream;
  P: TProcess;
  param: TStringList;
  n,s: LongInt;
  BytesRead: LongInt;
begin
M := TMemoryStream.Create;
P := TProcess.Create(nil);
param:=TStringList.Create;
result:=1;
try
  BytesRead := 0;
  SplitCmd(cmd,param);
  cmd:= param[0];
  param.Delete(0);
  P.Executable:=cmd;
  P.Parameters:=param;
  if ShowConsole then begin
     P.ShowWindow:=swoShowNormal;
     P.StartupOptions:=[suoUseShowWindow];
  end else begin
     P.ShowWindow:=swoHIDE;
  end;
  if output<>nil then P.Options := [poUsePipes, poStdErrToOutPut];
  P.Execute;
  while P.Running do begin
    if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
    if (output<>nil) and (P.Output<>nil) then begin
      s:=P.Output.NumBytesAvailable;
      if s>0 then begin
        M.SetSize(BytesRead + s);
        n := P.Output.Read((M.Memory + BytesRead)^, s);
        if n > 0 then inc(BytesRead, n);
      end;
    end;
  end;
  result:=P.ExitStatus;
  if (output<>nil) and (result<>127)and(P.Output<>nil) then repeat
    s:=P.Output.NumBytesAvailable;
    if s>0 then begin
      M.SetSize(BytesRead + s);
      n := P.Output.Read((M.Memory + BytesRead)^, s);
      if n > 0 then Inc(BytesRead, n);
    end;
  until (n<=0)or(s<=0)or(P.Output=nil);
  if (output<>nil) then begin
    M.SetSize(BytesRead);
    output.LoadFromStream(M);
  end;
  P.Free;
  M.Free;
  param.Free;
except
  on E: Exception do begin
    result:=-1;
    if (output<>nil) then output.add(E.Message);
    P.Free;
    M.Free;
    param.Free;
  end;
end;
end;

{$ifdef unix}
function ExecFork(cmd:string;p1:string='';p2:string='';p3:string='';p4:string='';p5:string=''):integer;
var
  parg: array[1..7] of PChar;
begin
  result := fpFork;
  if result = 0 then
  begin
    parg[1] := Pchar(cmd);
    if p1='' then parg[2]:=nil else parg[2] := PChar(p1);
    if p2='' then parg[3]:=nil else parg[3] := PChar(p2);
    if p3='' then parg[4]:=nil else parg[4] := PChar(p3);
    if p4='' then parg[5]:=nil else parg[5] := PChar(p4);
    if p5='' then parg[6]:=nil else parg[6] := PChar(p5);
    parg[7] := nil;
    if fpExecVP(cmd,PPChar(@parg[1])) = -1 then
    begin
      //writetrace('Could not launch '+cmd);
    end;
  end;
end;
{$endif}

Function ExecuteFile(const FileName: string): integer;
{$ifdef mswindows}
var
  zFileName, zParams, zDir: array[0..255] of Char;
begin
  //writetrace('Try to launch: '+FileName);
  Result := ShellExecute(Application.MainForm.Handle, nil, StrPCopy(zFileName, FileName),
                         StrPCopy(zParams, ''), StrPCopy(zDir, ''), SW_SHOWNOACTIVATE);
{$endif}
{$ifdef unix}
var cmd,p1,p2,p3,p4: string;
begin
  cmd:=trim(words(OpenFileCMD,blank,1,1));
  p1:=trim(words(OpenFileCMD,blank,2,1));
  p2:=trim(words(OpenFileCMD,blank,3,1));
  p3:=trim(words(OpenFileCMD,blank,4,1));
  p4:=trim(words(OpenFileCMD,blank,5,1));
  if p1='' then result:=ExecFork(cmd,FileName)
  else if p2='' then result:=ExecFork(cmd,p1,FileName)
  else if p3='' then result:=ExecFork(cmd,p1,p2,FileName)
  else if p4='' then result:=ExecFork(cmd,p1,p2,p3,FileName)
  else result:=ExecFork(cmd,p1,p2,p3,p4,FileName);
{$endif}
end;


procedure Wait(wt:single=5);
var endt: TDateTime;
begin
  endt:=now+wt/secperday;
  while now<endt do begin
    Sleep(100);
    if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
  end;
end;

function GetCdCPort:string;
var
{$ifdef mswindows}
Registry1: TRegistry;
{$else}
   f: textfile;
   fn: string;
{$endif}
begin
result:='3292';
{$ifdef mswindows}
  Registry1 := TRegistry.Create;
  with Registry1 do begin
    if Openkey('Software\Astro_PC\Ciel\Status',false) then begin
      if ValueExists('TcpPort') then result:=ReadString('TcpPort');
      CloseKey;
    end;
  end;
  Registry1.Free;
{$else}
  {$ifdef darwin}
   fn:=ExpandFileName('~/Library/Application Support/skychart/tmp/tcpport');
  {$else}
   fn:=ExpandFileName('~/.skychart/tmp/tcpport');
  {$endif}
  if FileExists(fn) then begin
    AssignFile(f,fn);
    Reset(f);
    read(f,result);
    CloseFile(f);
  end;
{$endif}
end;

function  Rmod(x,y:Double):Double;
BEGIN
    Rmod := x - Int(x/y) * y ;
END  ;

function to360(x:Extended):Extended;
begin
 result:=rmod(x+3600000000,360);
end;

function to180(x:Extended):Extended;
begin
 result:=rmod(x+3600000000,360);
 if result>180 then result:=result-360;
end;

function RoundFloat(x:Double;prec:integer):Double;
begin
  result:=round(x*prec)/prec;
end;

function IsNumber(n : string) : boolean;
begin
result:=TextToFloat(PChar(n),Dummy_ext);
end;

Function PadZeros(x : string ; l :integer) : string;
const zero = '000000000000';
var p : integer;
begin
x:=trim(x);
p:=l-length(x);
result:=copy(zero,1,p)+x;
end;

function DateTimetoJD(date: Tdatetime): double;
var Year, Month, Day: Word;
begin
DecodeDate(Date, Year, Month, Day);
result:=jd(Year,Month,Day,frac(date)*24);
end;

function DateTimetoJD0(date: Tdatetime): double;
var Year, Month, Day: Word;
begin
DecodeDate(Date, Year, Month, Day);
result:=jd(Year,Month,Day,0);
end;

function Jd(annee,mois,jour :INTEGER; Heure:double):double;
var u,u0,u1,u2 : double;
	gregorian : boolean;
begin
if annee*10000+mois*100+jour >= GregorianStart then gregorian:=true else gregorian:=false;
u:=annee;
if mois<3 then u:=u-1;
u0:=u+4712;
u1:=mois+1;
if u1<4 then u1:=u1+12;
result:=floor(u0*365.25)+floor(30.6*u1+0.000001)+jour+heure/24-63.5;
if gregorian then begin
   u2:=floor(abs(u)/100)-floor(abs(u)/400);
   if u<0 then u2:=-u2;
   result:=result-u2+2;
   if (u<0)and((u/100)=floor(u/100))and((u/400)<>floor(u/400)) then result:=result-1;
end;
end;

PROCEDURE Djd(jd:Double;OUT annee,mois,jour:INTEGER; OUT Heure:double);
var u0,u1,u2,u3,u4 : double;
	gregorian : boolean;
begin
u0:=jd+0.5;
if int(u0)>=GregorianStartJD then gregorian:=true else gregorian:=false;
u0:=jd+32082.5;
if gregorian then begin
   u1:=u0+floor(u0/36525)-floor(u0/146100)-38;
   if jd>=1830691.5 then u1:=u1+1;
   u0:=u0+floor(u1/36525)-floor(u1/146100)-38;
end;
u2:=floor(u0+123);
u3:=floor((u2-122.2)/365.25);
u4:=floor((u2-floor(365.25*u3))/30.6001);
mois:=round(u4-1);
if mois>12 then mois:=mois-12;
jour:=round(u2-floor(365.25*u3)-floor(30.6001*u4));
annee:=round(u3+floor((u4-2)/12)-4800);
heure:=(jd-floor(jd+0.5)+0.5)*24;
end;

function isodate(a,m,d : integer) : string;
begin
result:=padzeros(inttostr(a),4)+'-'+padzeros(inttostr(m),2)+'-'+padzeros(inttostr(d),2);
end;

function DateIso2DateTime(dt: string): double;
var
  sy, y, m, d, p: integer;
  h: double;
begin
  Result := 0;
  sy := 1;
  h := 0;
  dt := trim(dt);
  if length(dt) > 2 then
  begin
    if dt[1] = '-' then
    begin
      sy := -1;
      Delete(dt, 1, 1);
    end;
    if dt[1] = '+' then
    begin
      sy := 1;
      Delete(dt, 1, 1);
    end;
  end;
  p := pos('-', dt);
  if p = 0 then
    exit;
  y := sy * StrToInt(trim(copy(dt, 1, p - 1)));
  dt := copy(dt, p + 1, 999);
  p := pos('-', dt);
  if p = 0 then
    exit;
  m := StrToInt(trim(copy(dt, 1, p - 1)));
  dt := copy(dt, p + 1, 999);
  p := pos('T', dt);
  if p = 0 then
    p := pos(' ', dt);
  if p = 0 then
    d := StrToInt(trim(dt))     // no time part
  else
  begin
    d := StrToInt(trim(copy(dt, 1, p - 1)));
    h := StrToTime(trim(copy(dt,p+1,99)),':');
  end;
  result := EncodeDate(y, m, d) + h;
end;

function jddate(jd: double) : string;
var a,m,d : integer;
    h:double;
begin
djd(jd,a,m,d,h);
result:=isodate(a,m,d);
end;

PROCEDURE PrecessionFK5(ti,tf : double; VAR ari,dei : double);  // Lieske 77
var i1,i2,i3,i4,i5,i6,i7 : double ;
   BEGIN
   // simplified precession
   if abs(ti-tf)<0.01 then exit;
      I1:=(TI-2451545.0)/36525 ;
      I2:=(TF-TI)/36525;
      I3:=deg2rad*((2306.2181+1.39656*i1-1.39e-4*i1*i1)*i2+(0.30188-3.44e-4*i1)*i2*i2+1.7998e-2*i2*i2*i2)/3600 ;
      I4:=deg2rad*((2306.2181+1.39656*i1-1.39e-4*i1*i1)*i2+(1.09468+6.6e-5*i1)*i2*i2+1.8203e-2*i2*i2*i2)/3600 ;
      I5:=deg2rad*((2004.3109-0.85330*i1-2.17e-4*i1*i1)*i2-(0.42665+2.17e-4*i1)*i2*i2-4.1833e-2*i2*i2*i2)/3600 ;
      I6:=COS(DEI)*SIN(ARI+I3) ;
      I7:=COS(I5)*COS(DEI)*COS(ARI+I3)-SIN(I5)*SIN(DEI) ;
      i1:=(SIN(I5)*COS(DEI)*COS(ARI+I3)+COS(I5)*SIN(DEI));
      if i1>1 then i1:=1;
      if i1<-1 then i1:=-1;
      DEI:=ArcSIN(i1);
      ARI:=ARCTAN2(I6,I7) ;
      ARI:=ARI+I4;
      ARI:=RMOD(ARI+pi2,pi2);
   END  ;

procedure PrecessionEcl(ti, tf: double; var l, b: double);
var
  i1, i2, i3, i4, i5, i6, i7, i8: double;
begin
  i1 := (ti - 2451545.0) / 36525;
  i2 := (tf - ti) / 36525;
  i3 := deg2rad * (((47.0029 - 0.06603 * i1 + 0.000598 * i1 * i1) *
    i2 + (-0.03302 + 0.000598 * i1) * i2 * i2 + 0.000060 * i2 * i2 * i2) / 3600);
  i4 := deg2rad * ((174.876384 * 3600 + 3289.4789 * i1 + 0.60622 *
    i1 * i1 - (869.8089 + 0.50491 * i1) * i2 + 0.03536 * i2 * i2) / 3600);
  i5 := deg2rad * (((5029.0966 + 2.22226 * i1 - 0.000042 * i1 * i1) *
    i2 + (1.11113 - 0.000042 * i1) * i2 * i2 - 0.000006 * i2 * i2 * i2) / 3600);
  i6 := cos(i3) * cos(b) * sin(i4 - l) - sin(i3) * sin(b);
  i7 := cos(b) * cos(i4 - l);
  i8 := cos(i3) * sin(b) + sin(i3) * cos(b) * sin(i4 - l);
  l := i5 + i4 - arctan2(i6, i7);
  b := arcsin(i8);
  l := rmod(l + pi2, pi2);
end;

function PositionAngle(ac, dc, ar, de: double): double;
var
  hh, s1, s2, s3, c1, c2, c3: extended;
begin
  hh := (ac - ar);
  sincos(dc, s1, c1);
  sincos(de, s2, c2);
  sincos(hh, s3, c3);
  Result := rmod(pi2 + pi + arctan2((c2 * s3), (-c1 * s2 + s1 * c2 * c3)), pi2);
end;

Function AngularDistance(ar1,de1,ar2,de2 : Double) : Double;
var s1,s2,c1,c2,c3: extended;
begin
s1:=0;s2:=0;c1:=0;c2:=0;
try
if (ar1=ar2) and (de1=de2) then result:=0.0
else begin
    sincos(de1,s1,c1);
    sincos(de2,s2,c2);
    c3:=(s1*s2)+(c1*c2*cos((ar1-ar2)));
    if abs(c3)<=1 then
       result:=arccos(c3)
    else
       result:=pi2;
end;
except
  result:=pi2;
end;
end;

function PlaneDistance(x1,y1,x2,y2: double): double;
begin
try
  result:=sqrt(sqr(x1-x2)+sqr(y1-y2));
except
  result:=0;
end;
end;

procedure InitObservatory;
var
  u, p: double;
const
  ratio = 0.99664719;
  H0    = 6378140.0;
begin
  p := degtorad(ObsLatitude);
  u := arctan(ratio * tan(p));
  ObsRoSinPhi := ratio * sin(u) + (ObsElevation / H0) * sin(p);
  ObsRoCosPhi := cos(u) + (ObsElevation / H0) * cos(p);
end;

procedure Paralaxe(SideralTime,dist,ar1,de1 : double; var ar,de,q: double; jdcoord,jdnow: double);
var
   sinpi,H,a,b,d : double;
const
     desinpi = 4.26345151e-5;
begin
// AR, DE may be standard epoch but paralaxe is to be computed with coordinates of the date.
PrecessionFK5(jdcoord,jdnow,ar1,de1);
H:=(SideralTime-ar1);
//rde:=de1;
sinpi:=desinpi/dist;
a := cos(de1)*sin(H);
b := cos(de1)*cos(H)-ObsRoCosPhi*sinpi;
d := sin(de1)-ObsRoSinPhi*sinpi;
q := sqrt(a*a+b*b+d*d);
ar:=SideralTime-arctan2(a,b);
de:=arcsin(d/q);
PrecessionFK5(jdnow,jdcoord,ar,de);
end;

function SidTim(jd0,ut,long : double; eqeq: double=0): double;
VAR t,te: double;
BEGIN
t:=(jd0-2451545.0)/36525;
te:=100.46061837 + 36000.770053608*t + 0.000387933*t*t - t*t*t/38710000;
te:=te+rad2deg*eqeq;
result := deg2rad*Rmod(te - long + 1.00273790935*ut*15 + 360,360);
END ;

PROCEDURE Eq2Hz(HH,DE : double ; out A,h : double; method: smallint = 2);
var l1,d1,h1,sh : double;
BEGIN
// simplified function without pole motion and diurnal aberration
l1:=deg2rad*ObsLatitude;
d1:=DE;
h1:=HH;
sh := sin(l1)*sin(d1)+cos(l1)*cos(d1)*cos(h1);
if abs(sh)<1 then
 h:=arcsin(sh)
else
 h:=sgn(sh)*pi/2;
A:= arctan2(sin(h1),cos(h1)*sin(l1)-tan(d1)*cos(l1));
A:=Rmod(A+pi2,pi2);
Refraction(h,true,method);
END ;

Procedure Hz2Eq(A,h : double; out hh,de : double; method: smallint = 2);
var l1,a1,h1,sd : double;
BEGIN
// simplified function without pole motion and diurnal aberration
Refraction(h,false,method);
l1:=deg2rad*ObsLatitude;
a1:=A;
h1:=h;
sd:=sin(l1)*sin(h1)-cos(l1)*cos(h1)*cos(a1);
if abs(sd)<1 then
de:= arcsin(sd)
else
  h:=sgn(sd)*pi/2;
hh:= arctan2(sin(a1),cos(a1)*sin(l1)+tan(h1)*cos(l1));
hh:=Rmod(hh+pi2,pi2);
END ;

procedure ApparentToObserved(ra,de: double; out obsra,obsde: double);
var a,h,ha,st: double;
begin
  st:=CurrentSidTim;
  ha:=st - ra;
  Eq2HZ(ha, de, a, h, 2);     // with refraction
  Hz2Eq(a, h, ha, obsde, 0);  // geometric
  obsra:=rmod(st - ha + pi2, pi2);
end;

procedure ObservedToApparent(obsra,obsde: double; out ra,de: double);
var a,h,ha,st: double;
begin
  st:=CurrentSidTim;
  ha:=st - obsra;
  Eq2HZ(ha, obsde, a, h, 0);  // geometric
  Hz2Eq(a, h, ha, de, 2);     // with refraction
  ra:=rmod(st - ha + pi2, pi2);
end;

Function CurrentSidTim: double;
var jd0,CurTime: double;
    Year, Month, Day: Word;
begin
DecodeDate(now, Year, Month, Day);
CurTime:=frac(now)*24;
jd0:=jd(Year,Month,Day,0);
result:=Sidtim(jd0,CurTime-ObsTimeZone,ObsLongitude);
end;

Function SidTimT(t:TDateTime): double;
var jd0,CurTime: double;
    Year, Month, Day: Word;
begin
DecodeDate(t, Year, Month, Day);
CurTime:=frac(t)*24;
jd0:=jd(Year,Month,Day,0);
result:=Sidtim(jd0,CurTime-ObsTimeZone,ObsLongitude);
end;

Procedure cmdEq2Hz(ra,de : double ; out a,h : double);
var CurSt: double;
begin
CurST:=CurrentSidTim;
Eq2Hz(CurSt-deg2rad*15*ra,deg2rad*de,a,h) ;
a:=rad2deg*rmod(a+pi,pi2);
h:=rad2deg*h;
end;

Procedure cmdHz2Eq(a,h : double; out ra,de : double);
var CurSt: double;
begin
CurST:=CurrentSidTim;
a:=rmod(deg2rad*a-pi,pi2);
Hz2Eq(a,deg2rad*h,ra,de);
ra:=rad2deg*Rmod(CurST-ra+pi2,pi2)/15;
de:=rad2deg*de;
end;

function AirMass(h: double): double;
begin
if h>0 then begin
  try
  // Pickering, 2002
  result := 1 / sin(deg2rad * (h + (244 / (165 + 47 * h ** 1.1))));
  except
   result:=-1;
  end;
end
else
  result:=-1;
end;

function atmospheric_absorption(airmass: double):double;{magnitudes}
{The Extinction, Scattering, Absorption due to the atmosphere expressed in magnitudes.
 Reference http://www.icq.eps.harvard.edu/ICQExtinct.html
 see also https://www.skyandtelescope.com/astronomy-resources/transparency-and-atmospheric-extinction/}
 var
  a_ozon,a_ray,a_aer : double;
begin
  a_ozon:=airmass*0.016; {Schaefer's (1992) value Aoz =0.016 magnitudes per air mass for the small ozone component contributing to atmospheric extinction.}
  a_ray:=airmass*0.1451; {Rayleigh scattering by air molecules. Expressed in magnitudes}
  a_aer:=airmass*0.120; {Extinction due to aerosol scattering is due to particulates including dust, water droplets and manmade pollutants. Expressed in magnitudes}
  result:=a_ozon+a_ray+a_aer;{Total extinction, scattering, absorption due to the atmosphere expressed in magnitudes}
end;

procedure GuiderScreen2Fits(x,y: integer; FlipVert: boolean; out xx,yy:integer);
begin
try
  if FlipVert then y:=ScrGuideHeigth-y;
  if GuideImgZoom=0  then begin
     xx:=trunc(((x/GuideImgScale0)-GuideOrigX)/GuideImgPixRatio);
     yy:=trunc((y/GuideImgScale0)-GuideOrigY);
  end
  else begin
     xx:=trunc(((x/GuideImgZoom)-GuideOrigX)/GuideImgPixRatio);
     yy:=trunc((y/GuideImgZoom)-GuideOrigY);
  end;
except
  xx:=-1;
  yy:=-1;
end;
end;

procedure FinderScreen2Fits(x,y: integer; FlipVert: boolean; out xx,yy:integer);
begin
try
  if FlipVert then y:=ScrFinderHeigth-y;
  if FinderImgZoom=0  then begin
     xx:=trunc(((x/FinderImgScale0)-FinderOrigX)/FinderImgPixRatio);
     yy:=trunc((y/FinderImgScale0)-FinderOrigY);
  end
  else begin
     xx:=trunc(((x/FinderImgZoom)-FinderOrigX)/FinderImgPixRatio);
     yy:=trunc((y/FinderImgZoom)-FinderOrigY);
  end;
except
  xx:=-1;
  yy:=-1;
end;
end;

procedure Screen2Fits(x,y: integer; FlipHorz,FlipVert: boolean; out xx,yy:integer);
begin
try
  if FlipHorz then x:=ScrWidth-x;
  if FlipVert then y:=ScrHeigth-y;
  if ImgZoom=0  then begin
     if ImgPixRatio>=1 then begin
       xx:=trunc(((x/ImgScale0)-OrigX)/ImgPixRatio);
       yy:=trunc((y/ImgScale0)-OrigY);
     end
     else begin
       xx:=trunc((x/ImgScale0)-OrigX);
       yy:=trunc(((y/ImgScale0)-OrigY)*ImgPixRatio);
     end;
  end
  else begin
     if ImgPixRatio>=1 then begin
       xx:=trunc(((x/ImgZoom)-OrigX)/ImgPixRatio);
       yy:=trunc((y/ImgZoom)-OrigY);
     end
     else begin
       xx:=trunc((x/ImgZoom)-OrigX);
       yy:=trunc(((y/ImgZoom)-OrigY)*ImgPixRatio);
     end;
  end;
except
  xx:=-1;
  yy:=-1;
end;
end;

procedure Fits2Screen(x,y: integer; FlipHorz,FlipVert: boolean; out xx,yy: integer);
begin
try
  if ImgZoom=0 then begin
    if ImgPixRatio>=1 then begin
      xx:=round((x * ImgPixRatio+OrigX )* ImgScale0);
      yy:=round((y+OrigY )* ImgScale0);
    end
    else begin
      xx:=round((x+OrigX )* ImgScale0);
      yy:=round((y/ImgPixRatio+OrigY )* ImgScale0);
    end;
  end
  else begin
    if ImgPixRatio>=1 then begin
      xx:=round(((x+0.5) * ImgPixRatio+OrigX)*ImgZoom);
      yy:=round((y+0.5+OrigY)*ImgZoom);
    end
    else begin
      xx:=round((x+OrigX )* ImgZoom);
      yy:=round((y/ImgPixRatio+OrigY )* ImgZoom);
    end;
  end;
  if FlipHorz then xx:=ScrWidth-xx;
  if FlipVert then yy:=ScrHeigth-yy;
except
  xx:=-1;
  yy:=-1;
end;
end;

procedure FinderFits2Screen(x,y: integer; FlipVert: boolean; out xx,yy: integer);
begin
try
  if FinderImgZoom=0 then begin
    if FinderImgPixRatio>=1 then begin
      xx:=round((x * FinderImgPixRatio+FinderOrigX )* FinderImgScale0);
      yy:=round((y+FinderOrigY )* FinderImgScale0);
    end
    else begin
      xx:=round((x+FinderOrigX )* FinderImgScale0);
      yy:=round((y/FinderImgPixRatio+FinderOrigY )* FinderImgScale0);
    end;
  end
  else begin
    if FinderImgPixRatio>=1 then begin
      xx:=round(((x+0.5) * FinderImgPixRatio+FinderOrigX)*FinderImgZoom);
      yy:=round((y+0.5+FinderOrigY)*FinderImgZoom);
    end
    else begin
      xx:=round((x+FinderOrigX )* FinderImgZoom);
      yy:=round((y/FinderImgPixRatio+FinderOrigY )* FinderImgZoom);
    end;
  end;
  if FlipVert then yy:=ScrFinderHeigth-yy;
except
  xx:=-1;
  yy:=-1;
end;
end;

procedure Screen2CCD(x,y: integer; FlipHorz,FlipVert: boolean; vflip:boolean; out xx,yy:integer);
begin
  if FlipHorz then x:=ScrWidth-x;
  if FlipVert then y:=ScrHeigth-y;
  if ImgZoom=0 then  begin
    xx:=trunc((x/ImgScale0)-OrigX);
    if vflip then yy:=trunc(img_Height-(y/ImgScale0)+OrigY)
             else yy:=trunc((y/ImgScale0)-OrigY);
  end
  else begin
    xx:=trunc((x/ImgZoom)-OrigX);
    if vflip then yy:=trunc(img_Height-(y/ImgZoom)+OrigY)
             else yy:=trunc((y/ImgZoom)-OrigY);
  end;
  xx:=xx+ImgFrameX;
  yy:=yy+ImgFrameY;
end;

procedure CCD2Screen(x,y: integer; FlipHorz,FlipVert: boolean; vflip:boolean; out xx,yy:integer);
begin
try
  if ImgZoom=0 then begin
    xx:=round((x+OrigX) * ImgScale0);
    if vflip then yy:=round((img_Height-y+OrigY) * ImgScale0)
             else yy:=round((y +OrigY)* ImgScale0);
  end
  else begin
    xx:=round((x+OrigX)*ImgZoom);
    if vflip then yy:=round((img_Height-y+OrigY)*ImgZoom)
             else yy:=round((y+OrigY)*ImgZoom);
  end;
  if FlipHorz then xx:=ScrWidth-xx;
  if FlipVert then yy:=ScrHeigth-yy;
except
  xx:=-1;
  yy:=-1;
end;
end;

procedure CircleIntersect(x0,y0,r,x1,y1: integer; out xr,yr: integer);
var m,s,c:double;
begin
// intersection of line from (x0,y0), the center of the circle of radius r, to the point (y1,y1)
  m:=arctan2(y1-y0,x1-x0);
  sincos(m,s,c);
  xr:=round(x0+r*c);
  yr:=round(y0+r*s);
end;

procedure ResetTrackBar(tb:TTrackBar);
begin
  tb.min:=0;
  tb.position:=0;
  tb.max:=maxint;
end;

procedure LeastSquares(data: array of TDouble2; out a,b,r: double);
// https://en.wikipedia.org/wiki/Simple_linear_regression
// -> y = a*x + b
var
 X,Y,Sx,Sy,SumX,SumY,SumX2,SumY2,SumXY: double;
 n, i: Integer;
begin
 n:=Length(data);
 SumX:=0.0;
 SumY:=0.0;
 SumX2:=0.0;
 SumY2:=0.0;
 SumXY:=0.0;
 for i:=0 to n-1 do begin
   X:=data[i,1];
   Y:=data[i,2];
   SumX:=SumX+X;
   SumY:=SumY+Y;
   SumX2:=SumX2+X*X;
   SumY2:=SumY2+Y*Y;
   SumXY:=SumXY+X*Y;
 end;
 if (n*SumX2=SumX*SumX) or (n*SumY2=SumY*SumY) then begin
   a:=0;
   b:=0;
   r:=0;
 end else begin
   a:=((n*SumXY)-(SumX*SumY))/((n*SumX2)-(SumX*SumX));
   b:=(SumY-a*SumX)/n;
   Sx:=sqrt(Sumx2-sqr(SumX)/n);
   Sy:=Sqrt(Sumy2-sqr(SumY)/n);
   r:=(Sumxy-Sumx*SumY/n)/(Sx*sy);
 end;
end;

procedure Time_Alt(jd, ar, de, h: double; out hp1, hp2: double);
(*
   jd       :  date julienne desiree a 0H TU
   ar       :  ascension droite  radiant
   de       :  declinaison
   h        :  hauteur sur l'horizon   degres
               crepuscule nautique h=-12
               crepuscule astronomique h=-18
   hp1      :  heure matin
   hp2      :  heure soir
 *)
var
  hh, st, st0: double;
begin
  hh := (sin(deg2rad * h) - sin(deg2rad * ObsLatitude) * sin(de)) /
    (cos(deg2rad * ObsLatitude) * cos(de));
  if abs(hh) <= 1 then
  begin
    hh := arccos(hh);
    st0 := rad2deg * sidtim(jd, 0.0, ObsLongitude) / 15;
    st := rad2deg * (ar - hh) / 15;
    hp1 := rmod((st - st0) + 24, 24) / 1.002737908;
    st := rad2deg * (ar + hh) / 15;
    hp2 := rmod((st - st0) + 24, 24) / 1.002737908;
  end
  else
  begin
    if hh > 0 then
    begin
      hp1 := -99;      // never above H
      hp2 := -99;
    end
    else
    begin
      hp1 := 99;      // always above H
      hp2 := 99;
    end;
  end;
end;

function TwilightAstro(dt:TDateTime; out HMorning,HEvening:double):boolean;
var jd0,sra,sde,hp1,hp2: double;
    Year, Month, Day: Word;
begin
 DecodeDate(dt, Year, Month, Day);
 jd0:=jd(Year,Month,Day,0);
 Sun(jd0+0.5,sra,sde);
 Time_Alt(jd0, sra, sde, -18, hp1, hp2);
 if hp1<-90 then      // polar night
 begin
   HMorning:=0;
   HEvening:=24;
   result:=false;
 end
 else if hp1>90 then // polar day
 begin
    HMorning:=hp1;
    HEvening:=hp2;
    result:=false;
 end
 else begin          // day and night
   HMorning:=rmod(hp1+ObsTimeZone+24,24);
   HEvening:=rmod(hp2+ObsTimeZone+24,24);
   result:=true;
 end;
end;

procedure SecondsToWait(tnow,dt: TDateTime; forcenextday: boolean; out wt: Integer; out nextday:boolean);
var endt,nowt,nowd: TDateTime;
begin
  endt:=dt;
  nowd:=tnow;
  nowt:=frac(nowd);
  nowd:=trunc(nowd);
  nextday:=false;
  if (nowt>endt)and(forcenextday or (abs(nowt-endt)>0.5)) then begin
    endt:=nowd+1+endt;
    nextday:=true;
  end
  else begin
    endt:=nowd+endt;
  end;
  wt:=round((endt-tnow)*secperday);
end;

procedure LoadHorizon(fname: string);
var
  de, d0, d1, d2: single;
  i, i1, i2: integer;
  f: textfile;
  buf: string;
begin
  HorizonMax := musec;
  HorizonMin := pid2;
  for i := 0 to 361 do
    horizonlist[i] := 0;
  if fileexists(fname) then
  begin
    i1 := 0;
    i2 := 0;
    d1 := 0;
    d0 := 0;
    try
      Filemode := 0;
      assignfile(f, fname);
      reset(f);
      // get first point
      repeat
        readln(f, buf)
      until EOF(f) or ((trim(buf) <> '') and (buf[1] <> '#'));
      if (trim(buf) = '') or (buf[1] = '#') then
        exit;
      i1 := round(StrToFloat(trim(words(buf, blank, 1, 1))));
      d1 := strtofloat(trim(words(buf, blank, 2, 1)));
      if d1 > 90 then
        d1 := 90;
      if d1 < 0 then
        d1 := 0;
      if i1 <> 0 then
      begin
        reset(f);
        i1 := 0;
        d1 := 0;
      end;
      i2 := 0;
      d0 := d1;
      // process each point
      while (not EOF(f)) and (i2 < 359) do
      begin
        repeat
          readln(f, buf)
        until EOF(f) or ((trim(buf) <> '') and (buf[1] <> '#'));
        if (trim(buf) = '') or (buf[1] = '#') then
          break;
        i2 := round(StrToFloat(trim(words(buf, blank, 1, 1))));
        d2 := strtofloat(trim(words(buf, blank, 2, 1)));
        if i2 > 359 then
          i2 := 359;
        if i1 >= i2 then
          continue;
        if d2 > 90 then
          d2 := 90;
        if d2 < 0 then
          d2 := 0;
        for i := i1 to i2 do
        begin
          de := deg2rad * (d1 + (i - i1) * (d2 - d1) / (i2 - i1));
          horizonlist[i + 1] := de;
          HorizonMax := max(HorizonMax, de);
          HorizonMin := min(HorizonMin, de);
        end;
        i1 := i2;
        d1 := d2;
      end;

    finally
      closefile(f);
      // fill last point
      if i2 < 359 then
      begin
        for i := i1 to 359 do
        begin
          de := deg2rad * (d1 + (i - i1) * (d0 - d1) / (359 - i1));
          horizonlist[i + 1] := de;
          HorizonMax := max(HorizonMax, de);
          HorizonMin := min(HorizonMin, de);
        end;
      end;
      horizonlist[0]   := horizonlist[1];
      horizonlist[361] := horizonlist[1];
    end;
  end;
end;

procedure RiseTime(jd0, ar, de, alt: double; out hr, azr: double; out irc: integer );
var
  hoo, hs0, chh0, hh0, m0, m1,  a0: double;
  hsg, hl, h, dm, longref: double;
begin
  hoo := alt;
  Refraction(hoo, False);
  hoo := rad2deg * hoo;
  longref := -ObsTimeZone * 15;
  hs0 := sidtim(jd0, -ObsTimeZone, longref);
  chh0 := (sin(deg2rad * hoo) - sin(deg2rad * ObsLatitude) * sin(de)) /
    (cos(deg2rad * ObsLatitude) * cos(de));
  if abs(chh0) <= 1 then
  begin
    hh0 := arccos(chh0);
    m0 := (ar + deg2rad * ObsLongitude - deg2rad * longref - hs0) / pi2;
    m1 := m0 - hh0 / pi2;
    while m1 < 0 do
      m1 := m1 + 1;
    while m1 > 1 do
      m1 := m1 - 1;
    // rise
    hsg := hs0 + deg2rad * 360.985647 * m1;
    hl := hsg - deg2rad * Obslongitude + deg2rad * longref - ar;
    h := rad2deg * (arcsin(sin(deg2rad * Obslatitude) * sin(de) +
      cos(deg2rad * Obslatitude) * cos(de) * cos(hl)));
    dm := (h - hoo) / (360 * cos(de) * cos(deg2rad * Obslatitude) * sin(hl));
    hr := (m1 + dm) * 24;
    // azimuth
    a0 := arctan2(sin(hh0), cos(hh0) * sin(deg2rad * Obslatitude) -
      tan(de) * cos(deg2rad * Obslatitude));
    azr := pi2 - a0;
    irc := 0;
  end
  else
  begin
    hr := 0;
    azr := 0;
    if sgn(de) = sgn(ObsLatitude) then
      irc := 1  (* circumpolar *)
    else
      irc := 2; (* invisible *)
  end;
end;

procedure SetTime(jd0, ar, de, alt: double; out hs, azs: double; out irc: integer );
var
  hoo, hs0, chh0, hh0, m0, m2, a0: double;
  hsg, hl, h, dm, longref: double;
begin
  hoo := alt;
  Refraction(hoo, False);
  hoo := rad2deg * hoo;
  longref := -ObsTimeZone * 15;
  hs0 := sidtim(jd0, -ObsTimeZone, longref);
  chh0 := (sin(deg2rad * hoo) - sin(deg2rad * ObsLatitude) * sin(de)) /
    (cos(deg2rad * ObsLatitude) * cos(de));
  if abs(chh0) <= 1 then
  begin
    hh0 := arccos(chh0);
    m0 := (ar + deg2rad * ObsLongitude - deg2rad * longref - hs0) / pi2;
    m2 := m0 + hh0 / pi2;
    while m2 < 0 do
      m2 := m2 + 1;
    while m2 > 1 do
      m2 := m2 - 1;
    // set
    hsg := hs0 + deg2rad * 360.985647 * m2;
    hl := hsg - deg2rad * Obslongitude + deg2rad * longref - ar;
    h := rad2deg * (arcsin(sin(deg2rad * Obslatitude) * sin(de) +
      cos(deg2rad * Obslatitude) * cos(de) * cos(hl)));
    dm := (h - hoo) / (360 * cos(de) * cos(deg2rad * Obslatitude) * sin(hl));
    hs := (m2 + dm) * 24;
    // azimuth
    a0 := arctan2(sin(hh0), cos(hh0) * sin(deg2rad * Obslatitude) -
      tan(de) * cos(deg2rad * Obslatitude));
    azs := a0;
    irc := 0;
  end
  else
  begin
    hs := 0;
    azs := 0;
    if sgn(de) = sgn(ObsLatitude) then
      irc := 1  (* circumpolar *)
    else
      irc := 2; (* invisible *)
  end;
end;

procedure TransitTime(jd0, ar, de: double; out ht: double; out irc: integer );
var
  hoo, hs0, chh0,  m0: double;
  hsg, hl, dm, longref: double;
begin
  hoo := 0;
  Refraction(hoo, False);
  hoo := rad2deg * hoo;
  longref := -ObsTimeZone * 15;
  hs0 := sidtim(jd0, -ObsTimeZone, longref);
  chh0 := (sin(deg2rad * hoo) - sin(deg2rad * ObsLatitude) * sin(de)) /
    (cos(deg2rad * ObsLatitude) * cos(de));
  if abs(chh0) <= 1 then
  begin
    m0 := (ar + deg2rad * ObsLongitude - deg2rad * longref - hs0) / pi2;
    while m0 < 0 do
      m0 := m0 + 1;
    while m0 > 1 do
      m0 := m0 - 1;
    // transit
    hsg := hs0 + deg2rad * 360.985647 * m0;
    hl := hsg - deg2rad * Obslongitude + deg2rad * longref - ar;
    dm := -(hl / pi2);
    ht := rmod((m0 + dm) * 24 + 24, 24);
    if (ht < 10) and (m0 > 0.6) then
      ht := ht + 24;
    if (ht > 14) and (m0 < 0.4) then
      ht := ht - 24;
    irc := 0;
  end
  else
  begin
    if sgn(de) = sgn(ObsLatitude) then
    begin
      m0 := (ar + deg2rad * ObsLongitude - hs0) / pi2;     (* circumpolar *)
      if m0 < 0 then
        m0 := m0 + 1;
      if m0 > 1 then
        m0 := m0 - 1;
      hsg := hs0 + deg2rad * 360.985647 * m0;
      hl := hsg - deg2rad * ObsLongitude - ar;
      dm := -(hl / pi2);
      ht := rmod((m0 + dm) * 24 + ObsTimeZone + 24, 24);
      irc := 1;
    end
    else
    begin
      ht := 0;      (* invisible *)
      irc := 2;
    end;
  end;
end;

function ObjTransit(ra,de: double; out ht:double; out i:integer):boolean;
var jd0: double;
begin
  result:=false;
  jd0:=DateTimetoJD0(now);
  TransitTime(jd0,ra*15*deg2rad,de*deg2rad,ht,i);
  result:=i<2;
end;

function ObjRise(ra,de: double; out hr: double; out i:integer):boolean;
var jd0,azr,hhr,hht,a,h,ch,st: double;
    aa: integer;
begin
  result:=false;
  jd0:=DateTimetoJD0(now);
  h:=ElevationMin*deg2rad;
  RiseTime(jd0,ra*15*deg2rad,de*deg2rad,h,hhr,azr,i);
  if i=1 then begin
    // circumpolar, look for minimal altitude
    TransitTime(jd0,ra*15*deg2rad,de*deg2rad,hht,i);
    hhr:=rmod(hht+12,24);
    azr:=pi;
    i:=0;
  end;
  if i=0 then begin
    aa:=round(rmod(azr + pi, pi2)*rad2deg);
    if (aa<0)or(aa>360) then exit;
    ch:=horizonlist[aa];
    while h<ch do begin
     hhr:=hhr+(1/60);
     st:=SidTim(jd0,hhr-ObsTimeZone,ObsLongitude);
     Eq2Hz(st-ra*15*deg2rad,de*deg2rad,a,h);
     aa:=round(rmod(a + pi, pi2)*rad2deg);
     if aa=360 then aa:=0;
     if (aa>270)or(aa<0)or(aa>360) then exit;
     ch:=horizonlist[aa];
    end;
    hr:=rmod(hhr+24,24);
    result:=true;
  end;
end;

function ObjSet(ra,de: double; out hs:double; out i:integer):boolean;
var jd0,azs,hhs,hht,a,h,ch,st: double;
    aa: integer;
begin
  result:=false;
  jd0:=DateTimetoJD0(now);
  h:=ElevationMin*deg2rad;
  SetTime(jd0,ra*15*deg2rad,de*deg2rad,h,hhs,azs,i);
  if i=1 then begin
    // circumpolar, look for minimal altitude
    TransitTime(jd0,ra*15*deg2rad,de*deg2rad,hht,i);
    hhs:=rmod(hht+12,24);
    azs:=pi;
    i:=0;
  end;
  if i=0 then begin
    aa:=round(rmod(azs + pi, pi2)*rad2deg);
    if (aa<0)or(aa>360) then exit;
    ch:=horizonlist[aa];
    while h<ch do begin
     hhs:=hhs-(1/60);
     st:=SidTim(jd0,hhs-ObsTimeZone,ObsLongitude);
     Eq2Hz(st-ra*15*deg2rad,de*deg2rad,a,h);
     aa:=round(rmod(a + pi, pi2)*rad2deg);
     if aa=0 then aa:=360;
     if (aa<90)or(aa<0)or(aa>360) then exit;
     ch:=horizonlist[aa];
    end;
    hs:=rmod(hhs+24,24);
    result:=true;
  end;
end;

procedure InitCoord(jdnow: double=0);
var dy,dm,dd: word;
    se,ce,sra,sde: double;
begin
  if jdnow=0 then begin
    DecodeDate(now,dy,dm,dd);
    jdtoday:=jd(dy,dm,dd,0);
  end else begin
    jdtoday:=jdnow;
  end;
  deltaT:=SimpleDeltaT(now);
  // nutation
  nutationme(jdtoday,nutl,nuto);
  // Obliquity of the ecliptic
  ecl:=ltp_Ecliptic(jdtoday) + nuto;
  // nutation matrix
  sincos(ecl, se, ce);
  NutMAT[1, 1] := 1;
  NutMAT[1, 2] := -ce * nutl;
  NutMAT[1, 3] := -se * nutl;
  NutMAT[2, 1] := ce * nutl;
  NutMAT[2, 2] := 1;
  NutMAT[2, 3] := -nuto;
  NutMAT[3, 1] := se * nutl;
  NutMAT[3, 2] := nuto;
  NutMAT[3, 3] := 1;
  // sun longitude
  SunEcl(jdtoday,sunl,sunb);
  PrecessionEcl(jd2000, jdtoday, sunl, sunb);
  // aberration
  aberrationMe(jdtoday, abe, abp);

end;

procedure nutationme(j: double; var nutl, nuto: double);
var
  t, om, me, mas, mam, al: double;
const
  minjdnut = 2378496.5; // 1800   //limit for nutation calculation using Meeus function
  maxjdnut = 2524593.5; // 2200
begin
  if (j > minjdnut) and (j < maxjdnut) then
  begin
    // use this function only if cu_planet.nutation cannot get nutation from JPL ephemeris
    t := (j - jd2000) / 36525;
    // high precision. using meeus91 table 21.A
    //longitude of the asc.node of the Moon's mean orbit on the ecliptic
    om := deg2rad * (125.04452 - 1934.136261 * t + 0.0020708 * t *
      t + t * t * t / 4.5e+5);
    //mean elongation of the Moon from Sun
    me := deg2rad * (297.85036 + 445267.11148 * t - 0.0019142 * t *
      t + t * t * t / 189474);
    //mean anomaly of the Sun (Earth)
    mas := deg2rad * (357.52772 + 35999.05034 * t - 1.603e-4 * t * t - t * t * t / 3e+5);
    //mean anomaly of the Moon
    mam := deg2rad * (134.96298 + 477198.867398 * t + 0.0086972 * t *
      t + t * t * t / 56250);
    //Moon's argument of latitude
    al := deg2rad * (93.27191 + 483202.017538 * t - 0.0036825 * t *
      t + t * t * t / 327270);
    //periodic terms for the nutation in longitude.The unit is 0".0001.
    nutl := secarc * ((-171996 - 174.2 * t) * sin(1 * om) +
      (-13187 - 1.6 * t) * sin(-2 * me + 2 * al + 2 * om) +
      (-2274 - 0.2 * t) * sin(2 * al + 2 * om) + (2062 + 0.2 * t) *
      sin(2 * om) + (1426 - 3.4 * t) * sin(1 * mas) + (712 + 0.1 * t) *
      sin(1 * mam) + (-517 + 1.2 * t) * sin(-2 * me + 1 * mas + 2 * al + 2 * om) +
      (-386 - 0.4 * t) * sin(2 * al + 1 * om) - 301 * sin(1 * mam + 2 * al + 2 * om) +
      (217 - 0.5 * t) * sin(-2 * me - 1 * mas + 2 * al + 2 * om) -
      158 * sin(-2 * me + 1 * mam) + (129 + 0.1 * t) *
      sin(-2 * me + 2 * al + 1 * om) + 123 * sin(-1 * mam + 2 * al + 2 * om) +
      63 * sin(2 * me) + (63 + 0.1 * t) * sin(1 * mam + 1 * om) - 59 *
      sin(2 * me - 1 * mam + 2 * al + 2 * om) + (-58 - 0.1 * t) *
      sin(-1 * mam + 1 * om) - 51 * sin(1 * mam + 2 * al + 1 * om) + 48 *
      sin(-2 * me + 2 * mam) + 46 * sin(-2 * mam + 2 * al + 1 * om) -
      38 * sin(2 * me + 2 * al + 2 * om) - 31 * sin(2 * mam + 2 * al + 2 * om) +
      29 * sin(2 * mam) + 29 * sin(-2 * me + 1 * mam + 2 * al + 2 * om) +
      26 * sin(2 * al) - 22 * sin(-2 * me + 2 * al) + 21 *
      sin(-1 * mam + 2 * al + 1 * om) + (17 - 0.1 * t) * sin(2 * mas) +
      16 * sin(2 * me - 1 * mam + 1 * om) - 16 * sin(-2 * me + 2 *
      mas + 2 * al + 2 * om) - 15 * sin(1 * mas + 1 * om) - 13 *
      sin(-2 * me + 1 * mam + 1 * om) - 12 * sin(-1 * mas + 1 * om) +
      11 * sin(2 * mam - 2 * al) - 10 * sin(2 * me - 1 * mam + 2 * al + 1 * om) -
      8 * sin(2 * me + 1 * mam + 2 * al + 2 * om) + 7 *
      sin(1 * mas + 2 * al + 2 * om) - 7 * sin(-2 * me + 1 * mas + 1 * mam) -
      7 * sin(-1 * mas + 2 * al + 2 * om) - 7 * sin(2 * me + 2 * al + 1 * om) +
      6 * sin(2 * me + 1 * mam) + 6 * sin(-2 * me + 2 * mam + 2 * al + 2 * om) +
      6 * sin(-2 * me + 1 * mam + 2 * al + 1 * om) - 6 *
      sin(2 * me - 2 * mam + 1 * om) - 6 * sin(2 * me + 1 * om) + 5 *
      sin(-1 * mas + 1 * mam) - 5 * sin(-2 * me - 1 * mas + 2 * al + 1 * om) -
      5 * sin(-2 * me + 1 * om) - 5 * sin(2 * mam + 2 * al + 1 * om) +
      4 * sin(-2 * me + 2 * mam + 1 * om) + 4 * sin(-2 * me + 1 *
      mas + 2 * al + 1 * om) + 4 * sin(1 * mam - 2 * al) - 4 *
      sin(-1 * me + 1 * mam) - 4 * sin(-2 * me + 1 * mas) - 4 *
      sin(1 * me) + 3 * sin(1 * mam + 2 * al) - 3 * sin(-2 * mam + 2 * al + 2 * om) -
      3 * sin(-1 * me - 1 * mas + 1 * mam) - 3 * sin(1 * mas + 1 * mam) -
      3 * sin(-1 * mas + 1 * mam + 2 * al + 2 * om) - 3 *
      sin(2 * me - 1 * mas - 1 * mam + 2 * al + 2 * om) - 3 * sin(
      3 * mam + 2 * al + 2 * om) - 3 * sin(2 * me - 1 * mas + 2 * al + 2 * om));
    nutl := nutl * 0.0001;
    // periodic terms for the nutation in obliquity
    nuto := secarc * ((92025 + 8.9 * t) * cos(1 * om) + (5736 - 3.1 * t) *
      cos(-2 * me + 2 * al + 2 * om) + (977 - 0.5 * t) * cos(2 * al + 2 * om) +
      (-895 + 0.5 * t) * cos(2 * om) + (54 - 0.1 * t) * cos(1 * mas) -
      7 * cos(1 * mam) + (224 - 0.6 * t) * cos(-2 * me + 1 * mas + 2 * al + 2 * om) +
      200 * cos(2 * al + 1 * om) + (129 - 0.1 * t) * cos(1 * mam + 2 * al + 2 * om) +
      (-95 + 0.3 * t) * cos(-2 * me + -1 * mas + 2 * al + 2 * om) -
      70 * cos(-2 * me + 2 * al + 1 * om) - 53 * cos(-1 * mam + 2 * al + 2 * om) -
      33 * cos(1 * mam + 1 * om) + 26 * cos(2 * me + -1 * mam + 2 * al + 2 * om) +
      32 * cos(-1 * mam + 1 * om) + 27 * cos(1 * mam + 2 * al + 1 * om) -
      24 * cos(-2 * mam + 2 * al + 1 * om) + 16 * cos(2 * me + 2 * al + 2 * om) +
      13 * cos(2 * mam + 2 * al + 2 * om) - 12 * cos(-2 * me + 1 *
      mam + 2 * al + 2 * om) - 10 * cos(-1 * mam + 2 * al + 1 * om) -
      8 * cos(2 * me - 1 * mam + 1 * om) + 7 * cos(-2 * me + 2 * mas + 2 * al + 2 * om) +
      9 * cos(1 * mas + 1 * om) + 7 * cos(-2 * me + 1 * mam + 1 * om) + 6 *
      cos(-1 * mas + 1 * om) + 5 * cos(2 * me - 1 * mam + 2 * al + 1 * om) +
      3 * cos(2 * me + 1 * mam + 2 * al + 2 * om) - 3 *
      cos(1 * mas + 2 * al + 2 * om) + 3 * cos(-1 * mas + 2 * al + 2 * om) +
      3 * cos(2 * me + 2 * al + 1 * om) - 3 * cos(-2 * me + 2 * mam + 2 * al + 2 * om) -
      3 * cos(-2 * me + 1 * mam + 2 * al + 1 * om) + 3 *
      cos(2 * me - 2 * mam + 1 * om) + 3 * cos(2 * me + 1 * om) + 3 *
      cos(-2 * me - 1 * mas + 2 * al + 1 * om) + 3 * cos(-2 * me + 1 * om) +
      3 * cos(2 * mam + 2 * al + 1 * om));
    nuto := nuto * 0.0001;
  end
  else
  begin
    nutl := 0;
    nuto := 0;
  end;
end;

function ltp_Ecliptic(epj: double): double;
  // Obliquity of the ecliptic
  // Using equation 9, table 3.
  // Only the obliquity is computed here but the term in longitude are kept for clarity.
const
  npol = 4;
  nper = 10;
  // Polynomials
  pepol: array[1..npol, 1..2] of double = (
    (+8134.017132, 84028.206305),
    (+5043.0520035, +0.3624445),
    (-0.00710733, -0.00004039),
    (+0.000000271, -0.000000110));
  // Periodics
  peper: array[1..5, 1..nper] of double = (
    (409.90, 396.15, 537.22, 402.90, 417.15, 288.92, 4043.00, 306.00, 277.00, 203.00),
    (-6908.287473, -3198.706291, 1453.674527, -857.748557,
    1173.231614, -156.981465, 371.836550, -216.619040, 193.691479, 11.891524),
    (753.872780, -247.805823, 379.471484, -53.880558, -90.109153,
    -353.600190, -63.115353, -28.248187, 17.703387, 38.911307),
    (-2845.175469, 449.844989, -1255.915323, 886.736783, 418.887514,
    997.912441, -240.979710, 76.541307, -36.788069, -170.964086),
    (-1704.720302, -862.308358, 447.832178, -889.571909, 190.402846,
    -56.564991, -296.222622, -75.859952, 67.473503, 3.014055));
var
  as2r, d2pi, t, e, w, a, s, c: extended;
  i: integer;
begin
  d2pi := pi2;
  //Arcseconds to radians
  as2r := secarc;
  // Centuries since J2000.
  t := (epj - jd2000) / 36525;
  //p:=0;
  e := 0;
  // Periodic terms.
  for i := 1 to nper do
  begin
    W := D2PI * T;
    A := W / peper[1, I];
    sincos(A, S, C);
    //   p := p + C*peper[2,I] + S*peper[4,I];
    e := e + C * peper[3, I] + S * peper[5, I];
  end;
  //Polynomial terms.
  W := 1;
  for i := 1 to npol do
  begin
    //  p := p + pepol[I,1]*W;
    e := e + pepol[I, 2] * W;
    W := W * T;
  end;
  // in radiant.
  //p := p*AS2R;
  Result := e * AS2R;
end;

procedure aberrationme(j: double; var abe, abp: double);
var
  t: double;
const
  minjdabe = 2378496.5; // 1800 limit for aberration calculation using Meeus function
  maxjdabe = 2524593.5; // 2200
begin
  if (j > minjdabe) and (j < maxjdabe) then
  begin
    t := (j - jd2000) / 36525;
    abe := 0.016708617 - 4.2037e-5 * t - 1.236e-7 * t * t;
    abp := deg2rad * (102.93735 + 1.71953 * t + 4.6e-4 * t * t);
  end
  else
  begin
    abe := 0;
    abp := 0;
  end;
end;

{$WARN 5057 off : Local variable "$1" does not seem to be initialized}
procedure apparent_equatorialV(var p1: coordvector);
var
  ra, de, da, dd: double;
  cra, sra, cde, sde, ce, se, te, cp, sp, cls, sls: extended;
  p2: coordvector;
begin
  // simplified version without aberration relativistics term and without sun light deflection
  // nutation
  if (nutl <> 0) or (nuto <> 0) then
  begin
    // rotate using nutation matrix
    sofa_RXP(NutMAT, p1, p2);
    sofa_CP(p2, p1);
  end;
  //aberration
  if (abp <> 0) or (abe <> 0) then
  begin
      sofa_C2S(p1, ra, de);
      //meeus91 22.3
      sincos(ra, sra, cra);
      sincos(de, sde, cde);
      sincos(ecl, se, ce);
      sincos(sunl, sls, cls);
      sincos(abp, sp, cp);
      te := tan(ecl);
      da := -abek * (cra * cls * ce + sra * sls) / cde + abe *
        abek * (cra * cp * ce + sra * sp) / cde;
      dd := -abek * (cls * ce * (te * cde - sra * sde) + cra * sde * sls) +
        abe * abek * (cp * ce * (te * cde - sra * sde) + cra * sde * sp);
      ra := ra + da;
      de := de + dd;
      sofa_S2C(ra, de, p1);
  end;
end;

procedure apparent_equatorial(var ra, de: double);
var
  p: coordvector;
begin
  sofa_S2C(ra, de, p);
  apparent_equatorialV(p);
  sofa_c2s(p, ra, de);
  ra := rmod(ra + pi2, pi2);
end;

procedure mean_equatorial(var ra, de: double;doaberration:boolean=true;donutation:boolean=true);
var
  da, dd: double;
  cra, sra, cde, sde, ce, se, te, cp, sp, cls, sls: extended;
  p1, p2: coordvector;
  NutMATR: rotmatrix;
begin
  // simplified version without aberration relativistics term and without sun light deflection
  sofa_S2C(ra, de, p1);
  //aberration
  if doaberration and ((abp <> 0) or (abe <> 0)) then
  begin
      sofa_C2S(p1, ra, de);
      //meeus91 22.3
      sincos(ra, sra, cra);
      sincos(de, sde, cde);
      sincos(ecl, se, ce);
      sincos(sunl, sls, cls);
      sincos(abp, sp, cp);
      te := tan(ecl);
      da := -abek * (cra * cls * ce + sra * sls) / cde + abe *
        abek * (cra * cp * ce + sra * sp) / cde;
      dd := -abek * (cls * ce * (te * cde - sra * sde) + cra * sde * sls) +
        abe * abek * (cp * ce * (te * cde - sra * sde) + cra * sde * sp);
      ra := ra - da;
      de := de - dd;
      sofa_S2C(ra, de, p1);
  end;
  // nutation
  if donutation and ((nutl <> 0) or (nuto <> 0)) then
  begin
    // rotate using transposed nutation matrix
    sofa_TR(NutMAT, NutMATR);
    sofa_RXP(NutMATR, p1, p2);
    sofa_CP(p2, p1);
  end;
  sofa_C2S(p1, ra, de);
  ra := rmod(ra + pi2, pi2);
end;

procedure J2000ToApparent(var ra, de: double);
begin
PrecessionFK5(jd2000,jdtoday,ra,de);
apparent_equatorial(ra,de);
end;

procedure ApparentToJ2000(var ra, de: double);
begin
mean_equatorial(ra,de);
PrecessionFK5(jdtoday,jd2000,ra,de);
end;

procedure MountToLocal(mountjd:double; var ra, de: double);
begin
if abs(mountjd-jdtoday)<0.1 then exit;
ra:=deg2rad*ra*15;
de:=deg2rad*de;
PrecessionFK5(mountjd,jdtoday,ra,de);
apparent_equatorial(ra,de);
ra:=rad2deg*ra/15;
de:=rad2deg*de;
end;

procedure LocalToMount(mountjd:double; var ra, de: double);
begin
if abs(mountjd-jdtoday)<0.1 then exit;
ra:=deg2rad*ra*15;
de:=deg2rad*de;
mean_equatorial(ra,de);
PrecessionFK5(jdtoday,mountjd,ra,de);
ra:=rad2deg*ra/15;
de:=rad2deg*de;
end;

procedure MountToJ2000(mountjd:double; var ra, de: double);
begin
if abs(mountjd-jd2000)<0.1 then exit;
ra:=deg2rad*ra*15;
de:=deg2rad*de;
mean_equatorial(ra,de);
PrecessionFK5(mountjd,jd2000,ra,de);
ra:=rad2deg*ra/15;
de:=rad2deg*de;
end;

procedure J2000ToMount(mountjd:double; var ra, de: double);
begin
if abs(mountjd-jd2000)<0.1 then exit;
ra:=deg2rad*ra*15;
de:=deg2rad*de;
PrecessionFK5(jd2000,mountjd,ra,de);
apparent_equatorial(ra,de);
ra:=rad2deg*ra/15;
de:=rad2deg*de;
end;

function TempDisplay(cf:integer; t:double):double;
begin
if cf=0 then
   result:=t
else
   result:= t*9/5+32; // C to F
end;

function TempCelsius(cf:integer; t:double):double;
begin
if cf=0 then
   result:=t
else
   result:= (t-32)*5/9; // F to C
end;

////// Required functions adapted from the SOFA library

procedure sofa_PXP(a, b: coordvector; var axb: coordvector);
// p-vector outer (=vector=cross) product.
var
  xa, ya, za, xb, yb, zb: double;
begin
  XA := A[1];
  YA := A[2];
  ZA := A[3];
  XB := B[1];
  YB := B[2];
  ZB := B[3];
  AXB[1] := YA * ZB - ZA * YB;
  AXB[2] := ZA * XB - XA * ZB;
  AXB[3] := XA * YB - YA * XB;
end;

procedure sofa_PM(p: coordvector; var r: double);
// Modulus of p-vector.
var
  i: integer;
  w, c: double;
begin
  W := 0;
  for i := 1 to 3 do
  begin
    C := P[I];
    W := W + C * C;
  end;
  R := SQRT(W);
end;

procedure sofa_ZP(var p: coordvector);
// Zero a p-vector.
var
  i: integer;
begin
  for i := 1 to 3 do
    p[i] := 0;
end;

procedure sofa_SXP(s: double; p: coordvector; var sp: coordvector);
//  Multiply a p-vector by a scalar.
var
  i: integer;
begin
  for i := 1 to 3 do
    sp[i] := s * p[i];
end;

procedure sofa_PMP(a, b: coordvector; var amb: coordvector);
//  P-vector subtraction.
var
  i: integer;
begin
  for i := 1 to 3 do
    amb[i] := a[i] - b[i];
end;

procedure sofa_PPP(a, b: coordvector; var apb: coordvector);
//  P-vector addition.
var
  i: integer;
begin
  for i := 1 to 3 do
    apb[i] := a[i] + b[i];
end;

procedure sofa_PN(p: coordvector; var r: double; var u: coordvector);
// Convert a p-vector into modulus and unit vector.
var
  w: double;
begin
  // Obtain the modulus and test for zero.
  sofa_PM(P, W);
  if (W = 0) then
    //  Null vector.
    sofa_ZP(U)
  else
    //  Unit vector.
    sofa_SXP(1 / W, P, U);
  //  Return the modulus.
  R := W;
end;

procedure sofa_S2C(theta, phi: double; var c: coordvector);
// Convert spherical coordinates to Cartesian.
// THETA    d         longitude angle (radians)
// PHI      d         latitude angle (radians)
var
  sa, ca, sd, cd: extended;
begin
  sincos(theta, sa, ca);
  sincos(phi, sd, cd);
  c[1] := ca * cd;
  c[2] := sa * cd;
  c[3] := sd;
end;

procedure sofa_c2s(p: coordvector; var theta, phi: double);
// P-vector to spherical coordinates.
// THETA    d         longitude angle (radians)
// PHI      d         latitude angle (radians)
var
  x, y, z, d2: double;
begin
  X := P[1];
  Y := P[2];
  Z := P[3];
  D2 := X * X + Y * Y;
  if (D2 = 0) then
    theta := 0
  else
    theta := arctan2(Y, X);
  if (Z = 0) then
    phi := 0
  else
    phi := arctan2(Z, SQRT(D2));
end;

procedure sofa_cp(p: coordvector; var c: coordvector);
// Copy a p-vector.
var
  i: integer;
begin
  for i := 1 to 3 do
    c[i] := p[i];
end;

function sofa_PDP(a, b: coordvector): double;
  // p-vector inner (=scalar=dot) product.
begin
  Result := a[1] * b[1] + a[2] * b[2] + a[3] * b[3];
end;

procedure sofa_cr(r: rotmatrix; var c: rotmatrix);
// Copy an r-matrix.
var
  i, j: integer;
begin
  for j := 1 to 3 do
    for i := 1 to 3 do
      c[j, i] := r[j, i];
end;

procedure sofa_rxp(r: rotmatrix; p: coordvector; var rp: coordvector);
// Multiply a p-vector by an r-matrix.
var
  w: double;
  wrp: coordvector;
  i, j: integer;
begin
  // Matrix R * vector P.
  for j := 1 to 3 do
  begin
    W := 0;
    for i := 1 to 3 do
    begin
      W := W + R[J, I] * P[I];
    end; //i
    WRP[J] := W;
  end; //j
  // Return the result.
  sofa_CP(WRP, RP);
end;

procedure sofa_tr(r: rotmatrix; var rt: rotmatrix);
// Transpose an r-matrix.
var
  wm: rotmatrix;
  i, j: integer;
begin
  for i := 1 to 3 do
  begin
    for j := 1 to 3 do
    begin
      wm[i, j] := r[j, i];
    end;
  end;
  sofa_cr(wm, rt);
end;

procedure sofa_rxr(a, b: rotmatrix; var atb: rotmatrix);
// Multiply two r-matrices.
var
  i, j, k: integer;
  w: double;
  wm: rotmatrix;
begin
  for i := 1 to 3 do
  begin
    for j := 1 to 3 do
    begin
      W := 0;
      for k := 1 to 3 do
      begin
        W := W + A[I, K] * B[K, J];
      end; //k
      WM[I, J] := W;
    end; //j
  end; //i
  sofa_CR(WM, ATB);
end;

procedure sofa_Zr(var r: rotmatrix);
// Initialize an r-matrix to the null matrix.
var
  i, j: integer;
begin
  for i := 1 to 3 do
    for j := 1 to 3 do
      r[i, j] := 0;
end;

procedure sofa_Ir(var r: rotmatrix);
//   Initialize an r-matrix to the identity matrix.
begin
  sofa_Zr(r);
  r[1, 1] := 1.0;
  r[2, 2] := 1.0;
  r[3, 3] := 1.0;
end;

procedure sofa_Rz(psi: double; var r: rotmatrix);
//  Rotate an r-matrix about the z-axis.
var
  s, c: extended;
  a, w: rotmatrix;
begin
  // Matrix representing new rotation.
  sincos(psi, s, c);
  sofa_Ir(a);
  a[1, 1] := c;
  a[2, 1] := -s;
  a[1, 2] := s;
  a[2, 2] := c;
  // Rotate.
  sofa_Rxr(a, r, w);
  // Return result.
  sofa_Cr(w, r);
end;

procedure sofa_Ry(theta: double; var r: rotmatrix);
//  Rotate an r-matrix about the y-axis.
var
  s, c: extended;
  a, w: rotmatrix;
begin
  // Matrix representing new rotation.
  sincos(theta, s, c);
  sofa_Ir(a);
  a[1, 1] := c;
  a[3, 1] := s;
  a[1, 3] := -s;
  a[3, 3] := c;
  // Rotate.
  sofa_Rxr(a, r, w);
  // Return result.
  sofa_Cr(w, r);
end;

procedure sofa_Rx(phi: double; var r: rotmatrix);
//  Rotate an r-matrix about the x-axis.
var
  s, c: extended;
  a, w: rotmatrix;
begin
  // Matrix representing new rotation.
  sincos(phi, s, c);
  sofa_Ir(a);
  a[2, 2] := c;
  a[3, 2] := -s;
  a[2, 3] := s;
  a[3, 3] := c;
  // Rotate.
  sofa_Rxr(a, r, w);
  // Return result.
  sofa_Cr(w, r);
end;
{$WARN 5057 on : Local variable "$1" does not seem to be initialized}

function RotateBits(C: char; Bits: integer): char;
var
  SI: word;
begin
  Bits := Bits mod 8;
  // Are we shifting left?
  if Bits < 0 then
  begin
    // Put the data on the right half of a Word (2 bytes)
    SI := word(C);
    //      SI := MakeWord(Byte(C),0);
    // Now shift it left the appropriate number of bits
    SI := SI shl Abs(Bits);
  end
  else
  begin
    // Put the data on the left half of a Word (2 bytes)
    SI := word(C) shl 8;
    //      SI := MakeWord(0,Byte(C));
    // Now shift it right the appropriate number of bits
    SI := SI shr Abs(Bits);
  end;
  // Finally, Swap the bytes
  SI := Swap(SI);
  // And OR the two halves together
  SI := Lo(SI) or Hi(SI);
  Result := Chr(SI);
end;

function EncryptStr(Str, Pwd: string; Encode: boolean = True): string;
var
  a, PwdChk, Direction, ShiftVal, PasswordDigit: integer;
begin
  if str='' then begin
    Result:=str;
    exit;
  end;

  if Encode then
    str := str + '               ';

  PasswordDigit := 1;
  PwdChk := 0;

  for a := 1 to Length(Pwd) do
    Inc(PwdChk, Ord(Pwd[a]));
  Result := Str;

  if Encode then
    Direction := -1
  else
    Direction := 1;
  for a := 1 to Length(Result) do
  begin
    if Length(Pwd) = 0 then
      ShiftVal := a
    else
      ShiftVal := Ord(Pwd[PasswordDigit]);

    if Odd(A) then
      Result[A] := RotateBits(Result[A], -Direction * (ShiftVal + PwdChk))
    else
      Result[A] := RotateBits(Result[A], Direction * (ShiftVal + PwdChk));

    Inc(PasswordDigit);

    if PasswordDigit > Length(Pwd) then
      PasswordDigit := 1;
  end;
end;

function DecryptStr(Str, Pwd: string): string;
begin
  Result := trim(EncryptStr(Str, Pwd, False));
end;

function strtohex(str: string): string;
var
  i: integer;
begin

  Result := '';

  if str = '' then
    exit;

  for i := 1 to length(str) do
    Result := Result + inttohex(Ord(str[i]), 2);
end;


function hextostr(str: string): string;
var
  i, k: integer;
begin

  Result := '';

  if str = '' then
    exit;

  for i := 0 to (length(str) - 1) div 2 do
  begin

    k := strtointdef('$' + str[2 * i + 1] + str[2 * i + 2], -1);

    if k > 0 then
      Result := Result + char(k)
    else
    begin
      Result := str;   // if not numeric default to the input string
      break;
    end;

  end;

end;

procedure quicksort(var list: array of double; lo,hi: integer);{ Fast quick sort. Sorts elements in the array list with indices between lo and hi}
  procedure sort ( left, right : integer); {processing takes place in the sort procedure which executes itself recursively.}
  var
    i, j       : integer;
    tmp, pivot : double;    { tmp & pivot are the same type as the elements of array }
  begin
    i:=left;
    j:=right;
    pivot := list[(left + right) div 2];
    repeat
      while pivot > list[i] do inc(i);
      while pivot < list[j] do dec(j);
      if i<=j Then Begin
        tmp:=list[i]; list[i]:=list[j]; list[j]:=tmp; {swap}
        dec(j); inc(i);
      end;
    until i>j;
    if left<j then sort(left,j);
    if i<right then sort(i,right);
  end;
begin {quicksort};
  sort(lo,hi);
end;

function SMedian(list: array of double; leng: integer): double;{get median of an array of double. Taken from CCDciel code but slightly modified}
var
  mid : integer;
begin
 if leng=0 then result:=nan
 else
   if leng=1 then result:=list[0]
   else
   begin
     quickSort(list,0,leng-1);
     mid := (leng-1) div 2; //(high(list) - low(list)) div 2;
     if Odd(leng) then
     begin
       if leng<=3 then  result:=list[mid]
       else
       begin
         result:=(list[mid-1]+list[mid]+list[mid+1])/3;
       end;
     end
     else
     result:=(list[mid]+list[mid+1])/2;
  end;
end;

procedure SortFilterListInc(var list: TStringList);
var sorted: boolean;
    tmpexp: double;
    tmpname:string;
    j,n: integer;
begin
repeat
  sorted := True;
  n := list.Count;
  for j := 1 to n-1 do
  begin
    if TFilterExp(list.Objects[j - 1]).ExpFact > TFilterExp(list.Objects[j]).ExpFact then
    begin
      tmpname := list[j - 1];
      tmpexp := TFilterExp(list.Objects[j - 1]).ExpFact;
      list.Move(j,j - 1);
      list[j] := tmpname;
      TFilterExp(list.Objects[j]).ExpFact:=tmpexp;
      sorted := False;
    end;
  end;
until sorted;
end;

procedure SortFilterListDec(var list: TStringList);
var sorted: boolean;
    tmpexp: double;
    tmpname:string;
    j,n: integer;
begin
repeat
  sorted := True;
  n := list.Count;
  for j := 1 to n-1 do
  begin
    if TFilterExp(list.Objects[j - 1]).ExpFact < TFilterExp(list.Objects[j]).ExpFact then
    begin
      tmpname := list[j - 1];
      tmpexp := TFilterExp(list.Objects[j - 1]).ExpFact;
      list.Move(j,j - 1);
      list[j] := tmpname;
      TFilterExp(list.Objects[j]).ExpFact:=tmpexp;
      sorted := False;
    end;
  end;
until sorted;
end;

function MoonRiseSet(dt:TDateTime; out moonrise,moonset:double):boolean;
var jd0,jd1,jd2,ra,de,phase,illum,hp1,hp2: double;
    Year, Month, Day: Word;
begin
 // Approximate time the moon do light the sky
 DecodeDate(dt, Year, Month, Day);
 jd0:=jd(Year,Month,Day,0);
 Moon(jd0,ra,de,phase,illum);
 if illum<0.15 then begin  // small moon to ignore
  moonrise:=0;
  moonset:=24;
  result:=false;
  exit;
 end;
 Time_Alt(jd0, ra, de, 0, hp1, hp2);
 if hp1<-90 then      // No moon rise
 begin
   moonrise:=0;
   moonset:=24;
   result:=false;
 end
 else if hp1>90 then // No moon set
 begin
    moonrise:=0;
    moonset:=24;
    result:=true;
 end
 else begin          // rise and set
   jd1:=jd0+hp1/24;
   jd2:=jd0+hp2/24;
   Moon(jd1,ra,de,phase,illum);
   Time_Alt(jd0, ra, de, 0, hp1, hp2);
   moonrise:=rmod(hp1+ObsTimeZone+24,24);
   Moon(jd2,ra,de,phase,illum);
   Time_Alt(jd0, ra, de, 0, hp1, hp2);
   moonset:=rmod(hp2+ObsTimeZone+24,24);
   result:=true;
 end;
end;

function DarkNight(t:Tdatetime): boolean;
var st,jdn,ra,de,phase,illum,a,h: double;
begin
  st:=SidTimT(t);
  jdn:=DateTimetoJD(t);
  Sun(jdn,ra,de);
  Eq2Hz(st-ra,de,a,h) ;
  h:=rad2deg*h;
  // sun below astro-twilight
  result:=(h<=-18);
  if not result then exit;
  Moon(jdn,ra,de,phase,illum);
  Eq2Hz(st-ra,de,a,h) ;
  h:=rad2deg*h;
  // moon small or set
  result:=result and ((h<0) or (illum<0.15));
end;

function InTimeInterval(t,begint, endt: double; st: double=0.5): integer;
// st=start time = pivot for next day
// t between begin and end : result = 0
// t before begin : result = -1
// t after end    : result = 1
begin
  result:=0;
  if (begint<0)and(endt<0) then exit;  // must return 0 when interval is not set
  if abs(begint-endt)<(1/secperday) then exit; // full 24h interval
  if begint<0 then begint:=0.5;        // default to noon
  if endt<0   then endt:=0.5;          // default to noon
  if t<st then
     t:=t+1;
  if endt<begint then
     endt:=endt+1;
  if (endt<st) and (begint<st) then begin
     endt:=endt+1;
     begint:=begint+1;
  end;
  if t<begint then
     result:=-1;
  if t>endt   then
     result:=1;
end;

{$ifdef mswindows}
function  GetWin32_Info:string;
var
  objWMIService : OLEVariant;
  colItems      : OLEVariant;
  colItem       : OLEVariant;
  oEnum         : IEnumvariant;
  iValue        : LongWord;

  function GetWMIObject(const objectName: String): IDispatch;
  var
  {$if (fpc_version>3) or (fpc_release>0)}
    chEaten: ULONG;
  {$else}
    chEaten: PULONG;
  {$endif}
    BindCtx: IBindCtx;
    Moniker: IMoniker;
  begin
    OleCheck(CreateBindCtx(0, bindCtx));
    OleCheck(MkParseDisplayName(BindCtx, StringToOleStr(objectName), chEaten, Moniker));
    OleCheck(Moniker.BindToObject(BindCtx, nil, IDispatch, Result));
  end;

begin
  result:='unknown';
  try
  objWMIService := GetWMIObject('winmgmts:\\localhost\root\cimv2');
  colItems      := objWMIService.ExecQuery('SELECT * FROM Win32_OperatingSystem','WQL',0);
  oEnum         := IUnknown(colItems._NewEnum) as IEnumVariant;
  if oEnum.Next(1, colItem, iValue) = 0 then begin
  Result:=colItem.Caption+' '+colItem.Version+' '+colItem.OSArchitecture;
  end;
  except
  end;
end;
{$endif}

{$ifdef unix}
function GetKernel_Info: string;
var P: TProcess;

  function ExecParam(Param: String): String;
  var s: LongWord;
  begin
    P.Parameters[0]:= '-' + Param;
    P.Execute;
    s:=P.Output.NumBytesAvailable;
    SetLength(Result, s);
    if s>0 then P.Output.Read(Result[1], s);
    While (Length(Result) > 0) And (Result[Length(Result)] In [#8..#13,#32]) Do
      SetLength(Result, Length(Result) - 1);
  end;

begin
  result:='unknown';
  try
  P:= TProcess.Create(Nil);
  P.Options:= [poWaitOnExit, poUsePipes];
  P.Executable:= 'uname';
  P.Parameters.Add('');
  result:='Kernel: '+ ExecParam('s')+' ';
  result:=result+ ExecParam('r')+' ';
  result:=result+ ExecParam('m');
  P.Free;
  except
  end;
end;
{$endif}

{$ifdef linux}
function GetLinux_Info: string;
var P: TProcess;

  function ExecParam(Param: String): String;
  var s: LongWord;
  begin
    P.Parameters[0]:= '-' + Param;
    P.Execute;
    s:=P.Output.NumBytesAvailable;
    SetLength(Result, s);
    if s>0 then P.Output.Read(Result[1], s);
    While (Length(Result) > 0) And (Result[Length(Result)] In [#8..#13,#32]) Do
      SetLength(Result, Length(Result) - 1);
  end;

begin
  result:='unknown';
  try
  P:= TProcess.Create(Nil);
  P.Options:= [poWaitOnExit, poUsePipes];
  P.Executable:= 'lsb_release';
  P.Parameters.Add('');
  result:='System: '+ ExecParam('is')+' ';
  result:=result+ ExecParam('rs');
  P.Free;
  except
  end;
end;
{$endif}

{$ifdef darwin}
function GetMac_Info: string;
var P: TProcess;

  function ExecParam(Param: String): String;
  var s: LongWord;
  begin
    P.Parameters[0]:= '-' + Param;
    P.Execute;
    s:=P.Output.NumBytesAvailable;
    SetLength(Result, s);
    if s>0 then P.Output.Read(Result[1], s);
    While (Length(Result) > 0) And (Result[Length(Result)] In [#8..#13,#32]) Do
      SetLength(Result, Length(Result) - 1);
  end;

begin
  try
  P:= TProcess.Create(Nil);
  P.Options:= [poWaitOnExit, poUsePipes];
  P.Executable:= 'sw_vers';
  P.Parameters.Add('');
  result:='System: '+ ExecParam('productName')+' ';
  result:=result+ ExecParam('productVersion')+' ';
  result:=result+ ExecParam('buildVersion');
  P.Free;
  except
  end;
end;
{$endif}

function SystemInformation: string;
begin
result:='Unknown system version';
{$ifdef mswindows}
try
result:=GetWin32_Info;
except
result:='Windows '+inttostr(Win32Platform)+' '+inttostr(Win32MajorVersion)+'.'+inttostr(Win32MinorVersion)+'.'+inttostr(Win32BuildNumber);
end;
{$endif}
{$ifdef linux}
try
result:=GetLinux_Info+', '+GetKernel_Info;
except
result:='Linux';
end;
{$endif}
{$ifdef darwin}
try
result:=GetMac_Info+', '+GetKernel_Info;
except
result:='MacOS';
end;
{$endif}
end;

function AscomVersion: string;
{$ifdef mswindows}
var v: Variant;
{$endif}
begin
result:='Unknown ASCOM version';
{$ifdef mswindows}
try
v:=CreateOleObject('ASCOM.Utilities.Util');
result:='ASCOM Platform '+v.PlatformVersion;
result:=result+', '+inttostr(v.MajorVersion)+'.'+inttostr(v.MinorVersion);
result:=result+'.'+inttostr(v.ServicePack);
result:=result+'.'+inttostr(v.BuildNumber);
v:=Unassigned;
except
end;
{$endif}
end;

function IndiVersion: string;
var r: Tstringlist;
    i: integer;
begin
result:='';
try
r:=Tstringlist.Create;
ExecProcess('indiserver -version',r);  // add invalid option to be sure in the future the server will not start even without parameters
for i:=0 to r.count-1 do begin
  if copy(r[i],1,13)='INDI Library:' then
    result:=result+'Local '+r[i];
  if copy(r[i],1,5)='Code ' then
    result:=result+', '+r[i];
end;
r.free;
except
end;
end;

function AstrometryVersion(resolver:integer; cygwinpath,cmdpath:string; usescript:boolean):string;
var P: TProcess;
    i,s: integer;
    endtime: double;
    buf: String;
begin
result:='unknown';
try
if (resolver=ResolverAstrometryNet) and (not usescript) then begin
  P:= TProcess.Create(Nil);
  P.Options:=[poUsePipes,poStderrToOutPut];
  {$ifdef mswindows}
  P.Executable:=slash(cygwinpath)+slash('bin')+'bash.exe';
  P.Parameters.Add('--login');
  P.Parameters.Add('-c');
  buf:='"';
  buf:=buf+' solve-field ';
  buf:=buf+' --help "';
  P.Parameters.Add(buf);
  {$else}
  if cmdpath='' then
    P.Executable:='solve-field'
  else begin
    P.Executable:=slash(cmdpath)+'solve-field';
    P.Environment.Add('PATH='+GetEnvironmentVariable('PATH')+':'+cmdpath)
  end;
  P.Parameters.Add('--help');
  {$endif}
  P.ShowWindow:=swoHIDE;
  endtime:=now+2/secperday;
  P.Execute;
  while P.Running do begin
    if now>endtime then begin
       P.Terminate(1);
    end;
    sleep(100);
  end;
  s:=P.Output.NumBytesAvailable;
  SetLength(buf, s);
  if s>0 then P.Output.Read(buf[1], s);
  P.Free;
  i:=pos('Revision',buf);
  if i<=0 then exit;
  delete(buf,1,i+8);
  i:=pos(',',buf);
  if i<=0 then exit;
  result:=trim(copy(buf,1,i-1));
  i:=StrToIntDef(result,0);
  if i>1000 then result:='0.38'; // some old windows version use the revision number instead of the version
end;
except
end;
end;

function GetThreadCount: integer;
begin
  Result := GetSystemThreadCount;
end;


function email(Subject,Msg:string):string;
var MailData: TStringList;
    SMTP: TSMTPSend;
    s, t, error: string;
    ok: boolean;
begin
error:='';
SMTP:=TSMTPSend.Create;
MailData:=TStringList.Create;
try
  MailData.Add('From: ' + MailFrom);
  MailData.Add('To: ' + MailTo);
  MailData.Add('Date: ' + Rfc822DateTime(now));
  MailData.Add('Subject: [CCDciel] ' + Subject);
  MailData.Add('X-mailer: CCDciel');
  MailData.Add('');
  MailData.Add(Msg);
  MailData.Add('');
  SMTP.UserName:=SMTPUser;
  SMTP.Password:=SMTPPasswd;
  SMTP.TargetHost:=SMTPHost;
  SMTP.TargetPort:=SMTPPort;
  SMTP.AutoTLS:=true;
  SMTP.FullSSL:=SMTPSSLTLS;
  SMTP.Sock.ConnectionTimeout:=10000;
  SMTP.Timeout:=10000;
  if SMTP.Login then
  begin
    if SMTP.MailFrom(GetEmailAddr(MailFrom), Length(MailData.Text)) then
    begin
      s := MailTo;
      repeat
        t := GetEmailAddr(Trim(FetchEx(s, ',', '"')));
        if t <> '' then
          OK := SMTP.MailTo(t);
        if not OK then begin
          error:='SMTP MailTo error: '+smtp.ResultString;
          Break;
        end;
      until s = '';
      if OK then begin
        OK := SMTP.MailData(MailData);
        if not OK then
          error:='SMTP MailData error: '+smtp.ResultString;
      end;
    end
    else begin
      error:='SMTP MailFrom error: '+smtp.ResultString;
    end;
    SMTP.Logout;
  end
  else begin
    error:='SMTP login error: '+smtp.ResultString+'; '+smtp.Sock.LastErrorDesc;
  end;
finally
  SMTP.Free;
  MailData.Free;
  result:=error;
end;
if VoiceEmail then speak(Subject+' . '+Msg);
end;

function isLocalIP(ip:string): boolean;
var ipstr:Tstringlist;
begin
  result:=false;
  ipstr:=Tstringlist.Create;
  try
  SplitRec(ip,'.',ipstr);
  if ipstr[0]='127' then
    result:=true
  else if ipstr[0]='10' then
    result:=true
  else if (ipstr[0]='192')and(ipstr[1]='168') then
    result:=true
  else if (ipstr[0]='172')and(ipstr[1]>='16')and(ipstr[1]<='31') then
    result:=true;
  finally
    ipstr.Free;
  end;
end;

function IsProgramRunning(pgm: string): boolean;
var proclist: TStringList;
    i:integer;
begin
  result:=false;
  try
  proclist:=TStringList.Create;
  try
  {$ifdef mswindows}
     ExecProcess('wmic process get name',proclist);
  {$else}
     ExecProcess('ps -e -o comm',proclist);
     {$ifdef darwin}
       for i:=0 to proclist.Count-1 do proclist[i]:=ExtractFileName(proclist[i]);
     {$endif}
  {$endif}
  for i:=0 to proclist.Count-1 do begin
    if proclist[i]=pgm then begin
      result:=true;
      break;
    end;
  end;
  finally
    proclist.Free;
  end;
  except
  end;
end;

function StartProgram(pgm, path: string; param:string=''; allowmultiinstance: boolean=false): boolean;
begin
  if allowmultiinstance or (not IsProgramRunning(pgm)) then begin
    if path<>'' then pgm:='"'+slash(path)+pgm+'"';
    if param<>'' then pgm:=pgm+' '+param;
    ExecNoWait(pgm,'',false);
    result:=true;
  end
  else
    result:=false;
end;

procedure StopProgram(pgm: string);
begin
{$ifdef mswindows}
  ExecNoWait('taskkill /IM '+pgm);
{$else}
  ExecNoWait('pkill '+pgm);
{$endif}
end;

function SafeFileName(fn:string): string;
begin
result:=StringReplace(fn,'*','',[rfReplaceAll]);
result:=StringReplace(result,'/','',[rfReplaceAll]);
result:=StringReplace(result,'\','',[rfReplaceAll]);
result:=StringReplace(result,':','',[rfReplaceAll]);
end;

function ValidateCustomHeader(key:string): boolean;
var i: integer;
    c: char;
begin
  // check key is a valid custom FITS header
  result:=false;
  // check length
  if (length(key)=0)or(length(key)>8) then
    exit;
  // check not a restricted keyword
  for i:=1 to NRestrictKey do begin
    if key=RestrictKey[i] then
      exit;
  end;
  // check valid character
  for i:=1 to length(key) do begin
    c:=key[i];
    if not(((c>='0')and((c<='9')))or((c>='A')and((c<='Z')))or(c='-')or(c='_')) then
       exit;
  end;
  result:=true;
end;

function TimToStr(tim: double; sep: string = ':'; showsec: boolean = True): string;
var
  dd, min1, min, sec: double;
  d, m, s: string;
begin
  dd := Int(tim);
  min1 := abs(tim - dd) * 60;
  if min1 >= 59.99166667 then
  begin
    dd := dd + sgn(tim);
    min1 := 0.0;
  end;
  min := Int(min1);
  sec := (min1 - min) * 60;
  if sec >= 59.5 then
  begin
    min := min + 1;
    sec := 0.0;
  end;
  str(abs(dd): 2: 0, d);
  if abs(dd) < 10 then
    d := '0' + trim(d);
  str(min: 2: 0, m);
  if abs(min) < 10 then
    m := '0' + trim(m);
  str(sec: 2: 0, s);
  if abs(sec) < 9.5 then
    s := '0' + trim(s);
  if showsec then
    Result := d + sep + m + sep + s
  else
    Result := d + sep + m;
end;

function NumericStr(txt:string):string;
var i,n: integer;
    c: char;
begin
  // keep only numeric character, keep space between number
  // 'RA center: 20h32m53s.10' -> '20 32 53 .10'
  result:='';
  for i:=1 to length(txt) do begin
    c:=txt[i];
    if ((c>='0')and(c<='9'))or(c='.')or(c='+')or(c='-') then
      result:=result+c
    else
      result:=result+' ';
  end;
  result:=wordspace(result);
end;

procedure Str2RaDec(txt: string; out ra,de: double);
var buf,buf1,buf2: string;
    i,p,s: integer;
    lst: TStringList;
begin
//  Format example:
//  RA: 02h53m29.7s DE:+16°11'49.5"
//  11h22m33.3s +30°10'20"
//  +30°10'20" 11h22m33.3s
//  11:22:33.3 +30:10:20
//  11 22 33.3 +30 10 20
//  11:22 +30:10
//  11 22 +30 10
//  11.5 -30.1
//  11 30 -30.1

  lst:=TStringList.Create;
  try
  txt:=trim(NumericStr(txt));
  buf1:='';
  buf2:='';
  Splitarg(txt,' ',lst);
  if lst.Count=2 then begin // ra dec in two words
    buf1:=trim(lst[0]);
    buf2:=trim(lst[1]);
    if (copy(buf1,1,1)='+')or(copy(buf1,1,1)='-') then begin // dec first
      buf:=buf1;
      buf1:=buf2;
      buf2:=buf;
    end;
  end
  else begin
    s:=-1;
    for i:=1 to lst.Count-1 do begin  // skip first word, this cannot work with dec first
       if (copy(lst[i],1,1)='+') or (copy(lst[i],1,1)='-') then s:=i; // word with sign is first of dec
    end;
    if s<0 then
      s:=lst.Count div 2; // no sign, assume same number of word for ra and dec
    for i:=0 to s-1 do
      buf1:=buf1+' '+lst[i];
    for i:=s to lst.Count-1 do
      buf2:=buf2+' '+lst[i];
  end;

  ra:=Str3ToAR(trim(buf1));
  de:=Str3ToDE(trim(buf2));

  finally
   lst.free
  end;
end;

function pseudomodal(f:Tform):TModalResult;
var fstyle: TFormStyle;
begin
  // show form in modal like mode that allow to change the cursor
  f.ModalResult:=mrNone;
  fstyle:=f.FormStyle;
  try
  f.FormStyle:=fsStayOnTop;
  f.Show;
  while (f.visible)and(f.ModalResult=mrNone) do begin
    sleep(100);
    Application.ProcessMessages;
  end;
  result:=f.ModalResult;
  if result=mrNone then result:=mrCancel;
  finally
   f.Hide;
   f.FormStyle:=fstyle;
  end;
end;

function checkconnection(host,port: string):boolean;
var tcpclient:TTCPClient;
begin
result:=false;
tcpclient:=TTCPClient.Create;
try
 tcpclient.TargetHost:=host;
 tcpclient.TargetPort:=port;
 tcpclient.Timeout := 100;
 if tcpclient.Connect then begin
    result:=true;
 end;
 tcpclient.Disconnect;
 tcpclient.Free;
except
 result:=false;
end;
end;

procedure spcolor(w: double; out red,green,blue:integer);
var r,g,b: double;
begin
   if (w<3800) then begin
       r := 1.0;
       g := 0.0;
       b := 1.0;
   end
   else if((w >= 3800) and (w<4400)) then begin
       r := -(w - 4400) / (4400 - 3800);
       g := 0.0;
       b := 1.0;
   end
   else if((w >= 4400) and (w<4900)) then begin
       r := 0.0;
       g := (w - 4400) / (4900 - 4400);
       b := 1.0;
   end
   else if((w >= 4900) and (w<5100)) then begin
       r := 0.0;
       g := 1.0;
       b := -(w - 5100) / (5100 - 4900);
   end
   else if((w >= 5100) and (w<5800)) then begin
       r := (w - 5100) / (5800 - 5100);
       g := 1.0;
       b := 0.0;
   end
   else if((w >= 5800) and (w<6450)) then begin
       r := 1.0;
       g := -(w - 6450) / (6450 - 5800);
       b := 0.0;
   end
   else begin
       r := 1.0;
       g := 0.0;
       b := 0.0;
   end;
   red := round(r * 255);
   green := round(g * 255);
   blue := round(b * 255);
end;

function SimpleDeltaT(dt: Tdatetime): double;
var year,month,day: word;
    y,u,t: double;
begin
  DecodeDate(dt,year,month,day);
  y := year + (month - 1) / 12 + (day - 1) / 365.25;
  case year of
    2017..2025:
    begin
      Result := 69.18;
    end;
    2026..2049:
    begin
      t := y - 2000;
      Result := (58.23 + t * (0.32217 + t * (0.005589)));
    end;
    2050..2149:
    begin
      u := (y - 1820) / 100;
      t := 2150 - y;
      Result := (-20 + 32 * u * u - 0.5788 * t);
    end;
  end;
end;

end.

