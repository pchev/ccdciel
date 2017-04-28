program focus_star;

{$mode objfpc}{$H+}

uses 
  Classes,
  SysUtils;
  
var f,f4,f5,f6,f7,f8:textfile;
    buf,proxy,ccdm,mag,ra,de,hip: string;
    m: integer;
    bv:double;

begin
  assignfile(f,'hip_main.dat');
  reset(f);
  assignfile(f4,'focus_star_4');
  rewrite(f4);
  assignfile(f5,'focus_star_5');
  rewrite(f5);
  assignfile(f6,'focus_star_6');
  rewrite(f6);
  assignfile(f7,'focus_star_7');
  rewrite(f7);
  assignfile(f8,'focus_star_8');
  rewrite(f8);
  repeat
     readln(f,buf);
     hip:=copy(buf,9,6);
     proxy:=trim(copy(buf,16,1));
     mag:=copy(buf,42,5);
     ra:=copy(buf,52,12);
     de:=copy(buf,65,12);
     bv:=StrToFloatDef(copy(buf,246,6),99);
     ccdm:=trim(copy(buf,328,10));
     m:=trunc(StrToFloatDef(mag,0));
     if (proxy='')and(ccdm='')and(bv>-0.1)and(bv<1.3) then begin
       buf:=hip+' '+ra+' '+de+' '+mag;
       case m of
         4: writeln(f4,buf);
         5: writeln(f5,buf);
         6: writeln(f6,buf);
         7: writeln(f7,buf);
         8: writeln(f8,buf);
       end;
     end;
  until eof(f);
  CloseFile(f);
  CloseFile(f4);
  CloseFile(f5);
  CloseFile(f6);
  CloseFile(f7);
  CloseFile(f8);
end.

