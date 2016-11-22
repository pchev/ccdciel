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

uses u_global,
     {$ifdef mswindows}
       Windows, registry,
     {$endif}
     {$ifdef unix}
       unix,baseunix,
     {$endif}
     process, Classes, LCLType, FileUtil, ComCtrls,
     Math, SysUtils, Forms, Menus, ActnList, Controls, StdCtrls, Graphics;



function InvertF32(X : LongWord) : Single;
function InvertF64(X : Int64) : Double;
Procedure FormPos(form : Tform; x,y : integer);
Function FormEntry(aOwner:TComponent; lbl,defaultstr:string):string;
function words(str,sep : string; p,n : integer; isep:char=blank) : string;
procedure SplitRec(buf,sep:string; var arg: TStringList);
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
procedure ExecNoWait(cmd: string; title:string=''; hide: boolean=true);
Function ExecProcess(cmd: string; output: TStringList; ShowConsole:boolean=false): integer;
Function ExecuteFile(const FileName: string): integer;
procedure Wait(wt:integer=5);
function GetCdCPort:string;
function  Rmod(x,y:Double):Double;
function IsNumber(n : string) : boolean;
Function PadZeros(x : string ; l :integer) : string;
function DateTimetoJD(date: Tdatetime): double;
function Jd(annee,mois,jour :INTEGER; Heure:double):double;
PROCEDURE Djd(jd:Double;OUT annee,mois,jour:INTEGER; OUT Heure:double);
function isodate(a,m,d : integer) : string;
function jddate(jd: double) : string;
PROCEDURE PrecessionFK5(ti,tf : double; VAR ari,dei : double);  // Lieske 77
function AngularDistance(ar1,de1,ar2,de2 : Double) : Double;
function SidTim(jd0,ut,long : double; eqeq: double=0): double;
Procedure Refraction(var h : double; flag:boolean);
PROCEDURE Eq2Hz(HH,DE : double ; out A,h : double);
Procedure Hz2Eq(A,h : double; out hh,de : double);
Procedure cmdEq2Hz(ra,de : double ; var a,h : double);
Procedure cmdHz2Eq(a,h : double; var ra,de : double);
procedure Screen2Fits(x,y: integer; out xx,yy:integer);
procedure Fits2Screen(x,y: integer; out xx,yy: integer);
procedure Screen2CCD(x,y: integer; vflip:boolean; out xx,yy:integer);
procedure CCD2Screen(x,y: integer; vflip:boolean; out xx,yy:integer);
procedure ResetTrackBar(tb:TTrackBar);
procedure LeastSquares(data: array of TDouble2; out a,b,r: double);

implementation

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
  b.Default:=true;
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
var s,p : integer;
    t : string;
begin
try
dms:=StringReplace(dms,blank,'0',[rfReplaceAll]);
if copy(dms,1,1)='-' then s:=-1 else s:=1;
p:=pos('h',dms);
if p=0 then
  result:=StrToFloatDef(dms,NullCoord)
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
if p=0 then begin
  sep:=sep3;
  d1:=length(sep[1])-1;
  p:=pos(sep[1],dms);
end;
if p=0 then
  result:=StrToFloatDef(dms,NullCoord)
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

Function Str3ToAR(dms : string) : double;
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

Function Str3ToDE(dms : string) : double;
var s,p : integer;
    t : string;
begin
try
dms:=StringReplace(dms,blank,'0',[rfReplaceAll]);
if copy(dms,1,1)='-' then s:=-1 else s:=1;
p:=pos('d',dms);
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
  if output<>nil then P.Options := [poUsePipes, poStdErrToOutPut];
  P.Execute;
  while P.Running do begin
    Application.ProcessMessages;
    if (output<>nil) and (P.Output<>nil) then begin
      M.SetSize(BytesRead + READ_BYTES);
      n := P.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
      if n > 0 then inc(BytesRead, n);
    end;
  end;
  result:=P.ExitStatus;
  if (output<>nil) and (result<>127)and(P.Output<>nil) then repeat
    M.SetSize(BytesRead + READ_BYTES);
    n := P.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0
    then begin
      Inc(BytesRead, n);
    end;
  until (n<=0)or(P.Output=nil);
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


procedure Wait(wt:integer=5);
var endt: TDateTime;
begin
  endt:=now+wt/secperday;
  while now<endt do begin
    Sleep(100);
    Application.ProcessMessages;
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

function SidTim(jd0,ut,long : double; eqeq: double=0): double;
VAR t,te: double;
BEGIN
t:=(jd0-2451545.0)/36525;
te:=100.46061837 + 36000.770053608*t + 0.000387933*t*t - t*t*t/38710000;
te:=te+rad2deg*eqeq;
result := deg2rad*Rmod(te - long + 1.00273790935*ut*15 + 360,360);
END ;

Procedure Refraction(var h : double; flag:boolean);
var h1,R : double;
begin
{ Bennett 2010, meeus91 15.3, 15.4 }
if flag then begin   // true -> apparent
     h1:=rad2deg*h;
     if h1>-1 then begin
        R:=cotan(deg2rad*(h1+9.48/(h1+4.8)));
        R:=R-0.06*sin(deg2rad*(14.7*R+13));
        h:=min(pid2,h);
     end
      else h:=h;
end
else begin      // apparent -> true
     h1:=rad2deg*h;
     if h1>-0.347259404573 then begin
        R:=cotan(deg2rad*(0.99914*h1+(7.31/(h1+4.4))));
        R:=R-0.06*sin(deg2rad*(14.7*R+13));
        h:=min(pid2,h)
     end
      else h:=h;
end;
end;

PROCEDURE Eq2Hz(HH,DE : double ; out A,h : double);
var l1,d1,h1,sh : double;
BEGIN
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
Refraction(h,true);
END ;

Procedure Hz2Eq(A,h : double; out hh,de : double);
var l1,a1,h1,sd : double;
BEGIN
Refraction(h,false);
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

Procedure cmdEq2Hz(ra,de : double ; var a,h : double);
var jd0,CurSt,CurTime: double;
    Year, Month, Day: Word;
begin
DecodeDate(now, Year, Month, Day);
CurTime:=frac(now)*24;
jd0:=jd(Year,Month,Day,0);
CurST:=Sidtim(jd0,CurTime-ObsTimeZone,ObsLongitude);
Eq2Hz(CurSt-deg2rad*15*ra,deg2rad*de,a,h) ;
a:=rad2deg*rmod(a+pi,pi2);
h:=rad2deg*h;
end;

Procedure cmdHz2Eq(a,h : double; var ra,de : double);
var jd0,CurSt,CurTime: double;
    Year, Month, Day: Word;
begin
DecodeDate(now, Year, Month, Day);
CurTime:=frac(now)*24;
jd0:=jd(Year,Month,Day,0);
CurST:=Sidtim(jd0,CurTime-ObsTimeZone,ObsLongitude);
a:=rmod(deg2rad*a-pi,pi2);
Hz2Eq(a,deg2rad*h,ra,de);
ra:=rad2deg*Rmod(CurST-ra+pi2,pi2)/15;
de:=rad2deg*de;
end;


procedure Screen2Fits(x,y: integer; out xx,yy:integer);
begin
try
  if ImgZoom=0.5 then begin
     xx:=(x * 2)-OrigX;
     yy:=(y * 2)-OrigY;
  end else if ImgZoom=1 then begin
      xx:=x-OrigX;
      yy:=y-OrigY;
  end else if ImgZoom=2 then begin
     xx:=(x div 2)-OrigX;
     yy:=(y div 2)-OrigY;
  end else  begin
     xx:=trunc(x/ImgScale0);
     yy:=trunc(y/ImgScale0);
  end;
except
  xx:=-1;
  yy:=-1;
end;
end;

procedure Fits2Screen(x,y: integer; out xx,yy: integer);
begin
try
  if ImgZoom=0 then begin
    xx:=round(x * ImgScale0);
    yy:=round(y * ImgScale0);
  end
  else if ImgZoom=0.5 then begin
    xx:=(x+OrigX) div 2;
    yy:=(y+OrigY) div 2;
  end
  else if ImgZoom=1 then begin
    xx:=x+OrigX;
    yy:=y+OrigY;
  end
  else if ImgZoom=2 then begin
    xx:=2*(x+OrigX);
    yy:=2*(y+OrigY);
  end;
except
  xx:=-1;
  yy:=-1;
end;
end;

procedure Screen2CCD(x,y: integer; vflip:boolean; out xx,yy:integer);
begin
   if ImgZoom=0.5 then begin
     xx:=(x * 2)-OrigX;
     if vflip then yy:=img_Height-(y*2)+OrigY
              else yy:=(y*2)-OrigY;
   end else if ImgZoom=1 then begin
     xx:=x-OrigX;
     if vflip then yy:=img_Height-y+OrigY
              else yy:=y-OrigY;
   end else if ImgZoom=2 then begin
     xx:=(x div 2)-OrigX;
     if vflip then yy:=img_Height-(y div 2)+OrigY
              else yy:=(y div 2)-OrigY;
   end else  begin
     xx:=trunc(x/ImgScale0);
     if vflip then yy:=trunc(img_Height-(y/ImgScale0))
              else yy:=trunc(y/ImgScale0);
   end;
   xx:=xx+ImgFrameX;
   yy:=yy+ImgFrameY;
end;

procedure CCD2Screen(x,y: integer; vflip:boolean; out xx,yy:integer);
begin
try
  if ImgZoom=0 then begin
    xx:=round(x * ImgScale0);
    if vflip then yy:=round((img_Height-y) * ImgScale0)
             else yy:=round(y * ImgScale0);
  end
  else if ImgZoom=0.5 then begin
    xx:=(x+OrigX) div 2;
    if vflip then yy:=(img_Height-y+OrigY) div 2
             else yy:=(y+OrigY) div 2;
  end
  else if ImgZoom=1 then begin
    xx:=x+OrigX;
    if vflip then yy:=img_Height-y+OrigY
             else yy:=y+OrigY;
  end
  else if ImgZoom=2 then begin
    xx:=2*(x+OrigX);
    if vflip then yy:=(img_Height-y+OrigY) * 2
             else yy:=2*(y+OrigY);
  end;
except
  xx:=-1;
  yy:=-1;
end;
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


end.

