unit u_annotation; {deep sky annotation of the image}
{ From ASTAP unit_deepsky with modification for CCDciel environment}
{$mode delphi}
{Copyright (C) 2018,2022 Han Kleijn (www.hnsky.org) and Patrick Chevalley.
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

{$RANGECHECKS OFF} // prevent range check error in VALINT32() for the header rows

interface
uses  u_global, u_utils, cu_fits, UScaleDPI,
  Classes, SysUtils,strutils, math,graphics;

procedure search_deepsky(f: TFits);{search the deep sky object on the image}
procedure plot_deepsky(cnv: TCanvas; cnvwidth,cnvheight: integer; FlipHor,FlipVer: boolean);{plot the deep sky object on the image}
procedure load_deep;{load the deepsky database once. If loaded no action}
procedure load_hyperleda;{load the HyperLeda database once. If loaded no action }
procedure read_deepsky(searchmode:char; telescope_ra,telescope_dec, cos_telescope_dec {cos(telescope_dec},fov : double; out ra2,dec2,length2,width2,pa : double);{deepsky database search}
function find_object(var objname : string; var ra0,dec0,length0,width0,pa : double): boolean; {find object in database}

var
  deepstring       : Tstrings;
  linepos          : integer;
  naam2,naam3,naam4: string;

implementation

type
   drawobject=record
               x,y: integer;
               length1,width1,pa,rot: double;
               name: shortstring;
              end;

var
   objlist: array[0..500] of drawobject;
   NumObj: integer;
   CurrentCdelt2: double;


procedure load_deep;{load the deepsky database once. If loaded no action}
begin
if ((deepstring.count<10) or (deepstring.count>=50000) {hyperleda loaded}) then {load deepsky data base}
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

procedure load_hyperleda;{load the HyperLeda database once. If loaded no action}
begin
if deepstring.count<50000 then {too small for HyperLeda. Empthy or normal database loaded. Replace by large HyperLeda}
  begin
    with deepstring do
    begin
       try
       LoadFromFile(slash(DataDir)+slash('dso')+'hyperleda.csv');{load deep sky data from file }
       except;
         clear;
         globalmsg('HyperLeda database not found. Install HyperLeda according instruction.');
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
                     if length(data1)>0 then {if no name skip this part}
                     begin
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
                   end;
                4: begin
                      val(data1,length2,fout2);{accept floating points}
                   end;{go to next object}
                5: begin
                     val(data1,width2,fout2);{accept floating points}
                   end;
                6: begin val(data1,pa,fout2);{accept floating points}
                         if fout2<>0 then pa:=999;end;
                         {orientation 0 is possible, therefore unknown=empthy equals 999}
       end;
       inc(x);
    until ((z>=6) or (fout<>0));
  until fout=0;  {repeat until no errors}
end;


procedure plot_glx(dc:tcanvas;x9,y9,diameter,ratio {ratio width/length},orientation:double); {draw oval or galaxy. If orientation is zero, galaxy is vertical.}
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
     glx[i].x:=round(x9    +r * sin_ori );
     glx[i].y:=round(y9    +r * cos_ori );
   end;
   dc.polygon(glx,nr+1);
end;


procedure search_deepsky(f: TFits);{search the deep sky object on the image}
var
  fitsX, fitsY, dra,ddec,delta,gamma, telescope_ra,telescope_dec,cos_telescope_dec,fov,ra2,dec2, length1,width1,
  pa,flipped,  crpix1,crpix2,cd1_1,cd1_2,cd2_1,cd2_2,cdelt1,cdelt2, crota2,ra0,dec0, delta_ra,det,
  SIN_dec_ref,COS_dec_ref,SIN_dec_new,COS_dec_new,SIN_delta_ra,COS_delta_ra,hh : double;
  width2, height2,sign: integer;
  new_to_old_WCS: boolean;
  x,y  : integer;
begin
  NumObj:=0;
  if ((f<>nil) and (f.HeaderInfo.solved)) then
  begin
    width2:=f.HeaderInfo.naxis1;
    height2:=f.HeaderInfo.naxis2;
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
      //crota1:= arctan2(sign*cd1_2,cd2_2);{not required}
      crota2:= arctan2(sign*cd1_1,cd2_1)-pi/2;
      //crota1:= crota1*180/pi; {not required}
      crota2:= crota2*180/pi;
    end;

    CurrentCdelt2:=cdelt2;


    {6. Passage (x,y) -> (RA,DEC) to find RA0,DEC0 for middle of the image. See http://alain.klotz.free.fr/audela/libtt/astm1-fr.htm}
    {for case crpix1,crpix2 are not in the middle}
    fitsX:=(width2+1)/2;{range 1..width, if range 1,2,3,4  then middle is 2.5=(4+1)/2 }
    fitsY:=(height2+1)/2;
    dRa :=(cd1_1*(fitsX-crpix1)+cd1_2*(fitsY-crpix2))*pi/180;
    dDec:=(cd2_1*(fitsX-crpix1)+cd2_2*(fitsY-crpix2))*pi/180;
    delta:=cos(dec0)-dDec*sin(dec0);
    gamma:=sqrt(dRa*dRa+delta*delta);
    telescope_ra:=ra0+arctan(Dra/delta);{position of the middle of the image}
    telescope_dec:=arctan((sin(dec0)+dDec*cos(dec0))/gamma);

    cos_telescope_dec:=cos(telescope_dec);
    fov:=1.5*sqrt(sqr(0.5*width2*cdelt1)+sqr(0.5*height2*cdelt2))*pi/180; {field of view with 50% extra}
    linepos:=2;
    if cdelt1*cdelt2>0 then flipped:=-1 {n-s or e-w flipped} else flipped:=1;

    sincos(dec0,SIN_dec_ref,COS_dec_ref);{do this in advance since it is for each pixel the same}

    repeat
      read_deepsky('S',telescope_ra,telescope_dec, cos_telescope_dec {cos(telescope_dec},fov,{var} ra2,dec2,length1,width1,pa);{deepsky database search}

      {5. Conversion (RA,DEC) -> (x,y). See http://alain.klotz.free.fr/audela/libtt/astm1-fr.htm}
      sincos(dec2,SIN_dec_new,COS_dec_new);{sincos is faster then seperate sin and cos functions}
      delta_ra:=ra2-ra0;
      sincos(delta_ra,SIN_delta_ra,COS_delta_ra);
      HH := SIN_dec_new*sin_dec_ref + COS_dec_new*COS_dec_ref*COS_delta_ra;
      dRA := (COS_dec_new*SIN_delta_ra / HH)*180/pi;
      dDEC:= ((SIN_dec_new*COS_dec_ref - COS_dec_new*SIN_dec_ref*COS_delta_ra ) / HH)*180/pi;
      det:=CD2_2*CD1_1 - CD1_2*CD2_1;
      fitsX:= +crpix1 - (CD1_2*dDEC - CD2_2*dRA) / det;{1..width2}
      fitsY:= +crpix2 + (CD1_1*dDEC - CD2_1*dRA) / det;{1..height2}
      x:=round(fitsX-1);{0..width2-1}
      y:=round(fitsY-1);{0..height2-1}

      if ((x>-0.25*width2) and (x<=1.25*width2) and (y>-0.25*height2) and (y<=1.25*height2)) then {within image1 with 25% overlap}
      begin
        y:=(height2-1)-y;

        objlist[NumObj].x:=x;
        objlist[NumObj].y:=y;
        objlist[NumObj].length1:=length1;
        objlist[NumObj].width1:=width1;
        objlist[NumObj].pa:=pa;
        objlist[NumObj].rot:=(pa*flipped+crota2);
        if naam2='' then objlist[NumObj].name:=''
        else
        if naam3='' then objlist[NumObj].name:=naam2
        else
        if naam4='' then objlist[NumObj].name:=naam2+'/'+naam3
        else
        objlist[NumObj].name:=naam2+'/'+naam3+'/'+naam4;

        inc(NumObj);

       end;
     until (NumObj>=10)or(linepos>=$FFFFFF);{maximum 500 objects or end of database}
  end;
end;{search deep_sky}

procedure plot_deepsky(cnv: TCanvas; cnvwidth,cnvheight: integer; FlipHor,FlipVer: boolean);{plot the deep sky object found by search_deepsky}
type
  textarea = record
     x1,y1,x2,y2 : integer;
  end;
var
  length1,width1,
  pa,rot,z,len,cdelt2 : double;
  width2, height2: integer;
  name: string;
  text_dimensions  : array of textarea;
  k,i,text_counter,th,tw,x1,y1,x2,y2,x,y  : integer;
  overlap  :boolean;
begin
  if NumObj>0 then
  begin
    width2:=cnvwidth;
    height2:=cnvheight;
    cdelt2:=CurrentCdelt2;

    cnv.Pen.width :=1; {thickness lines, fixed} // max(1,round(height2/h));{thickness lines}
    cnv.pen.color:=clyellow;

    cnv.font.size:=DoScaleX(10); {fixed}

    cnv.brush.Style:=bsClear;
    cnv.font.color:=clyellow;

    text_counter:=0;
    setlength(text_dimensions,200);

    for k:=0 to NumObj-1 do begin

        x:=objlist[k].x;
        y:=objlist[k].y;
        length1:=objlist[k].length1;
        width1:=objlist[k].width1;
        pa:=objlist[k].pa;
        rot:=objlist[k].rot;
        name:=objlist[k].name;

        Fits2Screen(x,y,FlipHor,FlipVer,x,y);

        {Plot deepsky text labels on an empthy text space.}
        { 1) If the center of the deepsky object is outside the image then don't plot text}
        { 2) If the text space is occupied, then move the text down. If the text crosses the bottom then use the original text position.}
        { 3) If the text crosses the right side of the image then move the text to the left.}
        { 4) If the text is moved in y then connect the text to the deepsky object with a vertical line.}
        if ( (x>=0) and (x<=width2) and (y>=0) and (y<=height2) and (name<>'') ) then {plot only text if center object is visible and has a name}
        begin

          {get text dimensions}
          th:=cnv.textheight(name);
          tw:=cnv.textwidth(name);
          x1:=x;
          y1:=y;
          x2:=x + tw;
          y2:=y + th ;

          if ((x1<=width2) and (x2>width2)) then begin x1:=x1-(x2-width2);x2:=width2;end; {if text is beyond right side, move left}

          if text_counter>0 then {find free space in y for text}
          begin
            repeat {find free text area}
              overlap:=false;
              i:=0;
              repeat {test overlap}
                if ( ((x1>=text_dimensions[i].x1) and (x1<=text_dimensions[i].x2) and (y1>=text_dimensions[i].y1) and (y1<=text_dimensions[i].y2)) {left top overlap} or
                     ((x2>=text_dimensions[i].x1) and (x2<=text_dimensions[i].x2) and (y1>=text_dimensions[i].y1) and (y1<=text_dimensions[i].y2)) {right top overlap} or
                     ((x1>=text_dimensions[i].x1) and (x1<=text_dimensions[i].x2) and (y2>=text_dimensions[i].y1) and (y2<=text_dimensions[i].y2)) {left bottom overlap} or
                     ((x2>=text_dimensions[i].x1) and (x2<=text_dimensions[i].x2) and (y2>=text_dimensions[i].y1) and (y2<=text_dimensions[i].y2)) {right bottom overlap} or

                     ((text_dimensions[i].x1>=x1) and (text_dimensions[i].x1<=x2) and (text_dimensions[i].y1>=y1) and (text_dimensions[i].y1<=y2)) {two corners of text_dimensions[i] within text} or
                     ((text_dimensions[i].x2>=x1) and (text_dimensions[i].x2<=x2) and (text_dimensions[i].y2>=y1) and (text_dimensions[i].y2<=y2)) {two corners of text_dimensions[i] within text}
                   ) then
                begin
                  overlap:=true; {text overlaps an existing text}
                  y1:=y1+(th div 3);{try to shift text one third of the text height down}
                  y2:=y2+(th div 3);
                  if y2>=height2 then {no space left, use original position}
                     begin
                       y1:=y;
                       y2:=y +th ;
                       overlap:=false;{stop searching}
                       i:=$FFFFFFF;{stop searching}
                     end;
                end;
                inc(i);
              until ((i>=text_counter) or (overlap) );{until all tested or found overlap}
            until overlap=false;{continue till no overlap}
          end;

          text_dimensions[text_counter].x1:=x1;{store text dimensions}
          text_dimensions[text_counter].y1:=y1;
          text_dimensions[text_counter].x2:=x2;
          text_dimensions[text_counter].y2:=y2;

          if y1<>y then {there was textual overlap, draw line down}
          begin
            cnv.moveto(x,round(y+th/4));
            cnv.lineto(x,y1);
          end;
          cnv.textout(x1,y1,name);
          inc(text_counter);
          if text_counter>=length(text_dimensions) then setlength(text_dimensions,text_counter+200);{increase size dynamic array}
        end;{centre object visible}

        if width1=0 then begin width1:=length1;pa:=999;end;
        z:=ImgZoom;
        if z=0 then z:=ImgScale0;
        len:=z*length1/(abs(cdelt2)*60*10*2); {Length in pixels}
        if len<=2 then {too small to plot an elipse or circle, just plot four dots}
        begin {tiny object marking}
          cnv.pixels[x-2,y+2]:=clyellow;
          cnv.pixels[x+2,y+2]:=clyellow;
          cnv.pixels[x-2,y-2]:=clyellow;
          cnv.pixels[x+2,y-2]:=clyellow;
        end {tiny object marking}
        else
        begin {normal plot}
          if PA<>999 then begin
            if FlipHor then rot:=-rot;
            if FlipVer then rot:=-rot;
            plot_glx(cnv,x,y,len,width1/length1,rot*pi/180) {draw oval or galaxy}
          end
          else
            cnv.ellipse(round(x-len),round(y-len),round(x+1+len),round(y+1+len));{circle, the y+1,x+1 are essential to center the circle(ellipse) at the middle of a pixel. Otherwise center is 0.5,0.5 pixel wrong in x, y}
        end;{normal plot}

    end; // numobj loop
    text_dimensions:=nil;{remove used memory}
  end;
end;{plot deep_sky}

function find_object(var objname : string; var ra0,dec0,length0,width0,pa : double): boolean; {find object in database}
begin
  result:=false;
  if length(objname)>1 then {Object name length should be two or longer}
  begin
    objname:=uppercase(objname);
    load_deep;{Load the deepsky database once. If already loaded, no action}
    linepos:=2;{Set pointer to the beginning}
    repeat
      read_deepsky('T' {full database search} ,0 {ra},0 {dec},1 {cos(telescope_dec)},2*pi{fov},{var} ra0,dec0,length0,width0,pa);{Deepsky database search}
      if ((objname=uppercase(naam2)) or (objname=uppercase(naam3)) or (objname=uppercase(naam4))) then
      begin
        result:=true;
        if naam3='' then objname:=naam2 {Add one object name only}
         else
         objname:=naam2+'_'+naam3;{Add two object names}
        linepos:=$FFFFFF; {Stop searching}
     end;
    until linepos>=$FFFFFF;{Found object or end of database}
  end;
end;



end.

