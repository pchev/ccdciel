unit u_annotation; {deep sky annotation of the image}
{ From ASTAP unit_deepsky with modification for CCDciel environment}
{$mode delphi}
{Copyright (C) 2018 by Han Kleijn, www.hnsky.org
 email: han.k.. at...hnsky.org

{This program is free software: you can redistribute it and/or modify
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

interface
uses  u_global, u_utils, cu_fits, UScaleDPI,
  Classes, SysUtils,strutils, math,graphics;

procedure plot_deepsky(f: TFits; cnv: TCanvas; cnvheight: integer);{plot the deep sky object on the image}
procedure load_deep;{load the deepsky database once. If loaded no action}
procedure read_deepsky(searchmode:char; telescope_ra,telescope_dec, cos_telescope_dec {cos(telescope_dec},fov : double; out ra2,dec2,length2,width2,pa : double);{deepsky database search}

var
  deepstring       : Tstrings;
  linepos          : integer;
  naam2,naam3,naam4: string;

implementation

procedure load_deep;{load the deepsky database once. If loaded no action}
begin
  if deepstring.count<10 then {load deepsky data base}
  begin
    with deepstring do
    begin
       try
       LoadFromFile(slash(DataDir)+slash('dso')+'deep_sky.csv');{load deep sky data from file }
       except;
         clear;
         globalmsg('Deep sky data base not found. Please try to reinstall the program.');
         beep;
       end;
    end;
  end;
end;


//http://fastcode.sourceforge.net/
//function ValLong_JOH_PAS_4_c(Value: Integer): string;
function Valint32(const s; out code: Integer): Longint;{fast val function, about 4 x faster}
var
  Digit: Integer;
  Neg, Hex, Valid: Boolean;
  P: PChar;
begin
  Code := 0;
  P := Pointer(S);
  if not Assigned(P) then
    begin
      Result := 0;
      inc(Code);
      Exit;
    end;
  Neg   := False;
  Hex   := False;
  Valid := False;
  while P^ = ' ' do
    Inc(P);
  if P^ in ['+', '-'] then
    begin
      Neg := (P^ = '-');
      inc(P);
    end;
  if P^ = '$' then
    begin
      inc(P);
      Hex := True;
    end
  else
    begin
      if P^ = '0' then
        begin
          inc(P);
          Valid := True;
        end;
      if Upcase(P^) = 'X' then
        begin
          Hex := True;
          inc(P);
        end;
    end;
  Result := 0;
  if Hex then
    begin
      Valid := False;
      while True do
        begin
          case P^ of
            '0'..'9': Digit := Ord(P^) - Ord('0');
            'a'..'f': Digit := Ord(P^) - Ord('a') + 10;
            'A'..'F': Digit := Ord(P^) - Ord('A') + 10;
            else      Break;
          end;
          if (Result < 0) or (Result > $0FFFFFFF) then
            Break;
          Result := (Result shl 4) + Digit;
          Valid := True;
          inc(P);
        end;
    end
  else
    begin
      while True do
        begin
          if not (P^ in ['0'..'9']) then
            break;
          if Result > (MaxInt div 10) then
            break;
          Result := (Result * 10) + Ord(P^) - Ord('0');
          Valid := True;
          inc(P);
        end;
      if Result < 0 then {Possible Overflow}
        if (Cardinal(Result) <> $80000000) or (not neg) then
          begin {Min(LongInt) = $80000000 is a Valid Result}
            Dec(P);
            Valid := False;
          end;
    end;
  if Neg then
    Result := -Result;
  if (not Valid) or (P^ <> #0) then
    Code := P-@S+1;
end;


procedure read_deepsky(searchmode:char; telescope_ra,telescope_dec, cos_telescope_dec {cos(telescope_dec},fov : double; out ra2,dec2,length2,width2,pa : double);{deepsky database search}
var
  x,z,y      : integer;
  fout,fout2, backsl1, backsl2,length_regel : integer;
  regel, data1      :  string;
  delta_ra : double;
  p2,p1: pchar;
begin
  repeat {until fout is 0}
    if linepos>=deepstring.count then
      begin
        linepos:=$FFFFFF;{mark as completed}
        exit;
      end;
    regel:=deepstring.strings[linepos]; {using regel,is faster then deepstring.strings[linepos]}
    inc(linepos);
    x:=1; z:=0; y:=0;

    P1 := Pointer(REGEL);
    length_regel:=length(regel);

    repeat
      {fast replacement for y:=posEx(',',regel,y+1); if y=0 then} {last field?}  {y:=length(regel)+1;}   {new fast routine nov 2015, use posEx rather then pos in Delphi}
      while ((y<length_regel) and (p1^<>',')) do
             begin inc(y); inc(p1,1) end;
      inc(y); inc(p1,1);

      {fast replacement for data1:=copy(regel,x,y-x);}
      SetLength(data1, y-x);
      if y<>x then {not empthy 2018}
      begin
        P2 := Pointer(regel);
        inc(P2, X-1);
        move(P2^,data1[1], y-x);

        while ((length(data1)>1) and (data1[length(data1)]=' ')) do {remove spaces in the end since VAL( can't cope with them}
                                      delete(data1,length(data1),1);
      end;{not empthy}
      x:=y;
      inc(z); {new data field}

      case z of 1:
                     ra2:=valint32(data1,fout)*pi*2/864000;{10*60*60*24, so RA 00:00 00.1=1}
                          {valint32 takes 1 ms instead of 4ms}

                2: begin
                     dec2:=valint32(data1,fout)*pi*0.5/324000;{60*60*90, so DEC 00:00 01=1}
                     delta_ra:=abs(ra2-telescope_ra); if delta_ra>pi then delta_ra:=pi*2-delta_ra;

                     if ((searchmode<>'T') and                                                        {if searchmode is 'T' then full database search else within FOV}
                         ( sqr( delta_ra*cos_telescope_dec)  + sqr(dec2-telescope_dec)> sqr(fov)  ) ) {calculate angular distance and skip when outside FOV}
                           then  fout:=99; {if true then outside screen,go to next line}
                   end;
                3: begin
                     naam2:='';{for case data1='';}
                     naam3:='';
                     naam4:='';
                     while (data1[1]=' ') do delete(data1,1,1); {remove spaces in front of the name, in practice faster then trimleft}
                     backsl1:=pos('/',data1);
                     if backsl1=0 then naam2:=data1
                     else
                     begin
                       naam2:=copy(data1,1,backsl1-1);
                       backsl2:=posEX('/',data1,backsl1+2);     { could also use LastDelimiter}
                       if backsl2=0 then naam3:=copy(data1,backsl1+1,length(data1)-backsl1+1)
                       else
                       begin
                         naam3:=copy(data1,backsl1+1,backsl2-backsl1-1);
                         naam4:=copy(data1,backsl2+1,length(data1)-backsl2+1);
                       end;
                     end;
                   end;
                4: begin
                      val(data1,length2,fout2);{accept floating points}
                   end;{go to next object}
                5: begin
                     val(data1,width2,fout2);{accept floating points}
                   end;
                6: begin val(data1,pa,fout2);{accept floating points}
                         if fout2<>0 then pa:=999;end;
                         {orientation 0 komt ook voor daarom if not know=empthy equals 999}
       end;
       inc(x);
    until ((z>=6) or (fout<>0));
  until fout=0;  {repeat until no errors}
end;


procedure plot_glx(dc:tcanvas;x9,y9,diameter,ratio {ratio width/length},orientation:double); {draw oval or galaxy}
var   glx :array[0..127 {nr}+1] of tpoint;
      i,nr           : integer;
      r, sin_ori,cos_ori              : double;
begin
   if diameter<10 then nr:=22
   else if diameter<20 then nr:=44
   else nr:=127;

  if abs(ratio)<0.00001 then ratio:=0.00001;{show ring always also when it is flat}
   for i:=0 to nr+1 do
   begin
     r:=sqrt(sqr(diameter*ratio)/(1.00000000000001-(1-sqr(ratio))*sqr(cos(-pi*i*2/(nr))))); {radius ellips}
      sincos(orientation+pi*i*2/nr, sin_ori, cos_ori);
     glx[i].x:=round(x9    +r * cos_ori );
     glx[i].y:=round(y9    +r * sin_ori );
   end;
   dc.polygon(glx,nr+1);
end;

procedure rotate(rot,x,y :double;out  x2,y2:double);{rotate a vector point, angle seen from y-axis, counter clockwise}
var
  sin_rot, cos_rot :double;
begin
  sincos(rot, sin_rot, cos_rot);
  x2:=x * + sin_rot + y*cos_rot;
  y2:=x * - cos_rot + y*sin_rot;{SEE PRISMA WIS VADEMECUM page 68}
end;


{ transformation of equatorial coordinates into CCD pixel coordinates for optical projection, rigid method}
{ ra0,dec0: right ascension and declination of the optical axis}
{ ra,dec:   right ascension and declination}
{ xx,yy :   CCD coordinates}
{ cdelt:    CCD scale in arcsec per pixel}
procedure equatorial_standard(ra0,dec0,ra,dec, cdelt : double; out xx,yy: double);
var dv,sin_dec0,cos_dec0,sin_dec ,cos_dec,sin_deltaRA,cos_deltaRA: double;
begin
  sincos(dec0  ,sin_dec0 ,cos_dec0);
  sincos(dec   ,sin_dec  ,cos_dec );
  sincos(ra-ra0, sin_deltaRA,cos_deltaRA);
  dv  := (cos_dec0 * cos_dec * cos_deltaRA + sin_dec0 * sin_dec) / (3600*180/pi)*cdelt; {/ (3600*180/pi)*cdelt, factor for conversion standard coordinates to CCD pixels}
  xx := - cos_dec *sin_deltaRA / dv;{tangent of the angle in RA}
  yy := -(sin_dec0 * cos_dec * cos_deltaRA - cos_dec0 * sin_dec) / dv;  {tangent of the angle in DEC}
end;

procedure plot_deepsky(f: TFits; cnv: TCanvas; cnvheight: integer);{plot the deep sky object on the image}
var
  fitsX, fitsY : double;
  dra,ddec,delta,gamma,
  telescope_ra,telescope_dec,cos_telescope_dec,fov,ra2,dec2,
  length1,width1,pa,xx,yy,x,y,len,flipped,
  crpix1,crpix2,cd1_1,cd1_2,cd2_1,cd2_2,cdelt1,cdelt2,crota1,crota2,ra0,dec0 :double;
  width2, height2,h,sign: integer;
  name: string;
  new_to_old_WCS: boolean;
begin
  if ((f<>nil) and (f.HeaderInfo.solved)) then
  begin
    width2:=f.HeaderInfo.naxis1;
    height2:=f.HeaderInfo.naxis2;
    fitsx:=width2/2;{for case crpix1 is not in the middle}
    fitsy:=height2/2;
    if not f.Header.Valueof('CRPIX1',crpix1) then exit;
    if not f.Header.Valueof('CRPIX2',crpix2) then exit;
    if not f.Header.Valueof('CRVAL1',ra0) then exit;
    if not f.Header.Valueof('CRVAL2',dec0) then exit;
    if not f.Header.Valueof('CD1_1',cd1_1) then exit;
    if not f.Header.Valueof('CD1_2',cd1_2) then exit;
    if not f.Header.Valueof('CD2_1',cd2_1) then exit;
    if not f.Header.Valueof('CD2_2',cd2_2) then exit;
    new_to_old_WCS:=false;
    if not f.Header.Valueof('CDELT1',cdelt1) then new_to_old_WCS:=true;
    if not f.Header.Valueof('CDELT2',cdelt2) then new_to_old_WCS:=true;
    if not f.Header.Valueof('CROTA2',crota2) then new_to_old_WCS:=true;

    ra0:=deg2rad*ra0;
    dec0:=deg2rad*dec0;

    if new_to_old_WCS then begin
      if (cd1_1*cd2_2-cd1_2*cd2_1)>=0 then sign:=+1 else sign:=-1;
      cdelt1:=sqrt(sqr(cd1_1)+sqr(cd2_1))*sign;
      cdelt2:=sqrt(sqr(cd1_2)+sqr(cd2_2));
      crota1:= arctan2(sign*cd1_2,cd2_2);
      crota2:= arctan2(sign*cd1_1,cd2_1)-pi/2;
      crota1:= crota1*180/pi;
      crota2:= crota2*180/pi;
    end;

    dRa :=(cd1_1*(fitsx-crpix1)+cd1_2*(fitsy-crpix2))*pi/180;
    dDec:=(cd2_1*(fitsx-crpix1)+cd2_2*(fitsy-crpix2))*pi/180;
    delta:=cos(dec0)-dDec*sin(dec0);
    gamma:=sqrt(dRa*dRa+delta*delta);
    telescope_ra:=ra0+arctan(Dra/delta);
    telescope_dec:=arctan((sin(dec0)+dDec*cos(dec0))/gamma);

    cos_telescope_dec:=cos(telescope_dec);
    fov:=1.5*sqrt(sqr(0.5*width2*cdelt1)+sqr(0.5*height2*cdelt2))*pi/180; {field of view with 50% extra}
    linepos:=0;
    if cdelt1*cdelt2>0 then flipped:=-1 {n-s or e-w flipped} else flipped:=1;

    h:=cnvheight;
    if h=0 then h:=height2;
    cnv.Pen.width := round(1+height2/h);{thickness lines}
    cnv.pen.color:=clyellow;
    cnv.font.size:=round(DoScaleX(10)*height2/h);{adapt font to image dimensions}
    cnv.brush.Style:=bsClear;
    cnv.font.color:=clyellow;

    repeat
      read_deepsky('S',telescope_ra,telescope_dec, cos_telescope_dec {cos(telescope_dec},fov,{var} ra2,dec2,length1,width1,pa);{deepsky database search}


     equatorial_standard(telescope_ra,telescope_dec,ra2,dec2,1, {var} xx,yy); {xx,yy in arc seconds}
     xx:=xx/(cdelt1*3600);{convert arc seconds to pixels}
     yy:=yy/(cdelt2*3600);
     rotate((90-crota2)*pi/180,xx,yy,x,y);{rotate to screen orientation}

     if ((x<=width2/2+500) and (y<=height2/2+500)) then {within image1 with some overlap}
     begin

       x:=fitsX-x;
       y:=fitsY-y;

       if naam3='' then name:=naam2
       else
       if naam4='' then name:=naam2+'/'+naam3
       else
       name:=naam2+'/'+naam3+'/'+naam4;

       cnv.textout(round(x),round(y),name);

       if width1=0 then begin width1:=length1;pa:=999;end;
       len:=length1/(cdelt2*60*10*2); {Length in pixels}
       if PA<>999 then
         plot_glx(cnv,x,y,len,width1/length1,-(pa*flipped-90+crota2)*pi/180) {draw oval or galaxy}
       else
         cnv.ellipse(round(x-len),round(y-len),round(x+len),round(y+len));{circel}
     end;

    until linepos>=$FFFFFF;{end of database}

  end;
end;{plot deep_sky}


end.

