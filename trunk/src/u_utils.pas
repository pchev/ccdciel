unit u_utils;

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

{$mode delphi}{$H+}

interface

uses u_global,
     {$ifdef mswindows}
       Windows, registry,
     {$endif}
     {$ifdef unix}
       unix,
     {$endif}
     process, SysUtils, Classes, LCLType, FileUtil,
     Math, Forms, Controls, StdCtrls, Graphics;

function InvertF32(X : LongWord) : Single;
function InvertF64(X : Int64) : Double;
Procedure FormPos(form : Tform; x,y : integer);
Function FormEntry(aOwner:TComponent; lbl,defaultstr:string):string;
function words(str,sep : string; p,n : integer; isep:char=blank) : string;
Procedure SplitCmd(S : String; List : TStringList);
function Slash(nom : string) : string;
Function sgn(x:Double):Double ;
Function SXToStr(de: Double) : string;
Function RAToStr(ar: Double) : string;
Function DEToStr(de: Double) : string;
Function RAToStrB(ar: Double) : string;
Function DEToStrB(de: Double) : string;
Function ARToStr4(ar: Double; f: string; var d,m,s : string) : string;
Function StrToAR(dms : string) : double;
Function StrToDE(dms : string) : double;
procedure ExecNoWait(cmd: string; title:string=''; hide: boolean=true);
Function ExecProcess(cmd: string; output: TStringList; ShowConsole:boolean=false): integer;
function GetCdCPort:string;
function  Rmod(x,y:Double):Double;
function Jd(annee,mois,jour :INTEGER; Heure:double):double;
PROCEDURE Djd(jd:Double;VAR annee,mois,jour:INTEGER; VAR Heure:double);
PROCEDURE PrecessionFK5(ti,tf : double; VAR ari,dei : double);  // Lieske 77

implementation

const
  GregorianStart=15821015;
  GregorianStartJD=2299161;

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
const margin=60; //minimal distance from screen border
begin
with Form do begin
  if x>margin then left:=x
     else left:=margin;
  if left+width>(Screen.Width-margin) then left:=Screen.Width-width-margin;
  if left<0 then left:=0;
  if y>margin then top:=y
     else top:=margin;
  if top+height>(Screen.height-margin) then top:=Screen.height-height-margin;
  if top<0 then top:=0;
end;
end;

Function FormEntry(aOwner:TComponent; lbl,defaultstr:string):string;
var f: TForm;
    l: Tlabel;
    e: Tedit;
    b: TButton;
    i: integer;
    t: TTarget;
begin
  f:=TForm.Create(aOwner);
  l:=TLabel.Create(f);
  e:=TEdit.Create(f);
  b:=TButton.Create(f);
  l.Caption:=lbl;
  l.Parent:=f;
  e.Text:=defaultstr;
  e.Parent:=f;
  b.Caption:='OK';
  b.ModalResult:=mrOK;
  b.Parent:=f;
  f.ChildSizing.ControlsPerLine:=2;
  f.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
  f.AutoSize:=true;
  FormPos(f,mouse.CursorPos.X,mouse.CursorPos.Y);
  f.ShowModal;
  if f.ModalResult=mrOK then
    result:=e.Text
  else
    result:=defaultstr;
  f.free;
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

Function sgn(x:Double):Double ;
begin
// sign function with zero positive
if x<0 then
   sgn:= -1
else
   sgn:=  1 ;
end ;

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
    dd:=Int(ar);
    min1:=abs(ar-dd)*60;
    if min1>=59.999166667 then begin
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
    str(dd:3:0,d);
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    str(sec:2:0,s);
    if abs(sec)<9.95 then s:='0'+trim(s);
    result := d+'h'+m+'m'+s+'s';
end;

Function DEToStr(de: Double) : string;
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

Function RAToStrB(ar: Double) : string;
var dd,min1,min,sec: Double;
    d,m,s : string;
begin
    dd:=Int(ar);
    min1:=abs(ar-dd)*60;
    if min1>=59.999166667 then begin
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
    str(dd:3:0,d);
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    str(sec:2:0,s);
    if abs(sec)<9.95 then s:='0'+trim(s);
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

Function ARToStr4(ar: Double; f: string; var d,m,s : string) : string;
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
var s,p : integer;
    t : string;
begin
try
dms:=StringReplace(dms,blank,'0',[rfReplaceAll]);
if copy(dms,1,1)='-' then s:=-1 else s:=1;
p:=pos('h',dms);
if p=0 then
  result:=StrToFloatDef(dms,0)
else begin
  t:=copy(dms,1,p-1); delete(dms,1,p);
  result:=StrToIntDef(t,0);
  p:=pos('m',dms);
  t:=copy(dms,1,p-1); delete(dms,1,p);
  result:=result+ s * StrToIntDef(t,0) / 60;
  p:=pos('s',dms);
  t:=copy(dms,1,p-1);
  result:=result+ s * StrToFloatDef(t,0) / 3600;
end;
except
result:=0;
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
begin
try
dms:=StringReplace(dms,blank,'0',[rfReplaceAll]);
if copy(dms,1,1)='-' then s:=-1 else s:=1;
sep:=sep1;
d1:=length(sep[1])-1;
p:=pos(sep[1],dms);
if p=0 then begin
  sep:=sep2;
  d1:=length(sep[1])-1;
  p:=pos(sep[1],dms);
end;
if p=0 then
  result:=StrToFloatDef(dms,0)
else begin
t:=copy(dms,1,p-1); delete(dms,1,p+d1);
result:=StrToIntDef(t,0);
p:=pos(sep[2],dms);
t:=copy(dms,1,p-1); delete(dms,1,p);
result:=result+ s * StrToIntDef(t,0) / 60;
p:=pos(sep[3],dms);
t:=copy(dms,1,p-1);
result:=result+ s * StrToFloatDef(t,0) / 3600;
end;
except
result:=0;
end;
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
   FillChar(si,sizeof(si),0);
   FillChar(pi,sizeof(pi),0);
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

Function ExecProcess(cmd: string; output: TStringList; ShowConsole:boolean=false): integer;
const READ_BYTES = 2048;
var
  M: TMemoryStream;
  P: TProcess;
  param: TStringList;
  n: LongInt;
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
  P.Options := [poUsePipes, poStdErrToOutPut];
  P.Execute;
  while P.Running do begin
    Application.ProcessMessages;
    if P.Output<>nil then begin
      M.SetSize(BytesRead + READ_BYTES);
      n := P.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
      if n > 0 then inc(BytesRead, n);
    end;
  end;
  result:=P.ExitStatus;
  if (result<>127)and(P.Output<>nil) then repeat
    M.SetSize(BytesRead + READ_BYTES);
    n := P.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0
    then begin
      Inc(BytesRead, n);
    end;
  until (n<=0)or(P.Output=nil);
  M.SetSize(BytesRead);
  output.LoadFromStream(M);
  P.Free;
  M.Free;
  param.Free;
except
  on E: Exception do begin
    result:=-1;
    output.add(E.Message);
    P.Free;
    M.Free;
    param.Free;
  end;
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

PROCEDURE Djd(jd:Double;VAR annee,mois,jour:INTEGER; VAR Heure:double);
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

PROCEDURE PrecessionFK5(ti,tf : double; VAR ari,dei : double);  // Lieske 77
var i1,i2,i3,i4,i5,i6,i7 : double ;
   BEGIN
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

end.

