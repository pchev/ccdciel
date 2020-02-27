unit u_speech;

{
Copyright (C) 2016 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

{$mode objfpc}{$H+}

interface

uses
 {$ifdef mswindows}
  ComObj, Variants,
 {$endif}
 {$ifdef unix}
   unix,
 {$endif}
  u_utils, u_global, u_translation,
  Classes, Forms, SysUtils;

procedure InitSpeak;
procedure speak(Text: string);

var
  SPError: integer;
  SPErrorMsg: string;

implementation

var
  SpVoice: variant;
  spLang: string = '';
  LockSpeak: boolean;

{$ifdef mswindows}
procedure speak(Text: string);
var  SavedCW: word;
const SVSFlagsAsync = 1;
      SVSFIsXML = 8;
begin
  try
    SPError:=0;
    SPErrorMsg:='';
    if VarIsEmpty(SpVoice) then
      SpVoice := CreateOleObject('SAPI.SpVoice');
    if not VarIsEmpty(SpVoice) then
    begin
      SavedCW := Get8087CW;
      try
        Set8087CW(SavedCW or $4);
        text:='<speak version="1.0" xmlns="http://www.w3.org/2001/10/synthesis" xml:lang="'+lang+'">'+text+'</speak>';
        SpVoice.Speak(WideString(Text), SVSFlagsAsync+SVSFIsXML);
      finally
        Set8087CW(SavedCW);
      end;
    end;
  except
    on E:exception do begin
       SPError:=1;
       SPErrorMsg:=E.Message;
       end;
  end;
end;

{$endif}

{$if defined(linux) or defined(freebsd)}
procedure GetLang;
var
  ll: TStringList;
  sl, buf, buf1: string;
  i, p: integer;
begin
  spLang := 'en';
  sl := Lang;
  p := pos('-', sl);
  if p > 0 then
    sl := copy(sl, 1, p - 1);
  ll := TStringList.Create;
  try
    SPError:=ExecProcess('espeak --voices', ll);
    if SPError=0 then begin
    for i := 0 to ll.Count - 1 do
    begin
      buf := words(ll[i], '', 2, 1);
      if buf = Lang then
      begin
        spLang := buf;
        break;
      end;
      if buf = sl then
      begin
        spLang := buf;
        break;
      end;
      p := pos('-', buf);
      if p > 0 then
      begin
        buf1 := copy(buf, 1, p - 1);
        if buf1 = sl then
        begin
          spLang := buf;
        end;
      end;
    end;
    end
    else begin
      SPErrorMsg:=format(rsError,[IntToStr(SPError)]);
      if ll.Count>0 then SPErrorMsg:=SPErrorMsg+', '+ll[0];
    end;
  finally
    ll.Free;
  end;
end;

procedure speak(Text: string);
var opt: string;
    timeout: double;
begin
 timeout:=now+15/secperday;
 while LockSpeak and (now<timeout) do begin
    sleep(100);
    if (GetCurrentThreadId=MainThreadID) then CheckSynchronize;
 end;
 try
  LockSpeak:=true;
  SPError:=0;
  SPErrorMsg:='';
  if splang = '' then
    GetLang;
  opt:='';
  if pos('.',Text)>0 then begin
    Text:=StringReplace(Text,'.',', <break time="1000ms"/> ',[rfReplaceAll]);
    opt:=opt+' -m ';
  end;
  SPError:=fpSystem('espeak -s 140 -v ' + spLang + opt + ' "' + LowerCase(Text) + '"');
 finally
   LockSpeak:=false;
 end;
end;

{$endif}

{$ifdef darwin}
procedure speak(Text: string);
var timeout: double;
begin
 timeout:=now+15/secperday;
 while LockSpeak and (now<timeout) do begin
    sleep(100);
    if (GetCurrentThreadId=MainThreadID) then CheckSynchronize;
 end;
 try
  LockSpeak:=true;
  SPError:=0;
  SPErrorMsg:='';
  SPError:=fpSystem('osascript -e ''say "' + Text + '"''');
  finally
    LockSpeak:=false;
  end;
end;

{$endif}

procedure InitSpeak;
begin
  spLang := '';
  SpVoice := Unassigned;
  LockSpeak:= false;
end;

end.
