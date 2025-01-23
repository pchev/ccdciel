unit u_ephem;

{
Copyright (C) 2025 Patrick Chevalley

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

{$mode ObjFPC}{$H+}

interface

uses u_global, math,
  Classes, SysUtils;

function Open_de(dir: string): boolean;
procedure Close_de;
procedure Moon(jdlt : double; out ra,de,phase,illum : double);
procedure Sun(jdlt : double; out ra, de: double);
procedure SunEcl(jdlt : double; out l, b: double);
Procedure SunLowprec(jdn:double; out ra,de,l:double);
procedure MoonLowprec(jdn:double; out ra,de,phase,illum:double);

var
  de_folder, de_filename: string;
  de_type, de_jdcheck: integer;
  de_jdstart, de_jdend: double;

implementation

uses u_utils, uDE;

type
  Array_5D = array [0..5] of double;

const
  nJPL_DE = 11;
  JPL_DE: array [1..nJPL_DE] of integer = (440, 441, 430, 431, 423, 421, 422, 405, 406, 403, 200);

function load_de(t: double): boolean;
var
  i: integer;
begin
  if (t > de_jdstart) and (t < de_jdend) then
  begin
    // a file is already loaded for this date
    Result := (de_type <> 0);
  end
  else if trunc(t) = de_jdcheck then
  begin
    // we know that no file match this date
    Result := (de_type <> 0);
  end
  else
  begin
    // search a file for the date
    Result := False;
    de_type := 0;
    de_jdcheck := trunc(t);
    de_jdstart := MaxInt;
    de_jdend := -MaxInt;
    for i := 1 to nJPL_DE do
    begin
      if load_de_file(t, de_folder, JPL_DE[i], de_filename, de_jdstart, de_jdend)
      then
      begin
        Result := True;
        de_type := JPL_DE[i];
        break;
      end;
    end;
  end;
end;

function Open_de(dir: string): boolean;
begin
  de_folder:=dir;
  result:=load_de(DateTimetoJD(now));
end;

procedure Close_de;
begin
 close_de_file;
end;

procedure Barycenter(jdtt: double; var x, y, z: double);
var
  planet_arr: Array_5D;
begin
  Calc_Planet_de(jdtt, 12, planet_arr, True, 3, False);
  x := planet_arr[0];
  y := planet_arr[1];
  z := planet_arr[2];
end;

procedure SunRect(jdtt: double; out x, y, z: double);
var
  planet_arr: Array_5D;
  i: integer;
begin
  Calc_Planet_de(jdtt, 11, planet_arr, True, 3, False);
  x := planet_arr[0];
  y := planet_arr[1];
  z := planet_arr[2];
end;

procedure MoonGeocentric(jdtt : double; out alpha,delta,dist,dkm,diam,phase,illum : double);
{
	jdtt      :  julian date DT
	alpha   :  Moon RA J2000
        delta   :  Moon DEC J2000
	dist    :  Earth-Moon distance UA
	dkm     :  Earth-Moon distance Km
	diam    :  Apparent diameter (arcseconds)
	phase   :  Phase angle  (degree)
	illum	:  Illuminated percentage
}
var
   q : double;
   t,sm,mm,md : double;
   w: array[1..3] of double;
   planet_arr, sun_arr: Array_5D;
   i: integer;
   pp: double;
begin
  Barycenter(jdtt, sun_arr[0], sun_arr[1], sun_arr[2]);
  Calc_Planet_de(jdtt, 10, planet_arr, True, 3, False);
  dist := sqrt(planet_arr[0] * planet_arr[0] + planet_arr[1] * planet_arr[1] + planet_arr[2] * planet_arr[2]);
  jdtt := jdtt - tlight * dist;
  Calc_Planet_de(jdtt, 10, planet_arr, True, 12, False);
  for i := 0 to 2 do
    w[i + 1] := planet_arr[i] + sun_arr[i];
  alpha := arctan2(w[2], w[1]);
  if (alpha < 0) then
    alpha := alpha + 2 * pi;
  pp := sqrt(w[1] * w[1] + w[2] * w[2]);
  delta := arctan(w[3] / pp);
  dkm := dist * km_au;
  diam:=2*358482800/dkm;
  t:=(jdtt-2415020)/36525;  { meeus 15.1 }
  sm:=degtorad(358.475833+35999.0498*t-0.000150*t*t-0.0000033*t*t*t);  {meeus 30. }
  mm:=degtorad(296.104608+477198.8491*t+0.009192*t*t+0.0000144*t*t*t);
  md:=rmod(350.737486+445267.1142*t-0.001436*t*t+0.0000019*t*t*t,360);
  phase:=180-md ;     { meeus 31.4 }
  md:=degtorad(md);
  phase:=rmod(phase-6.289*sin(mm)+2.100*sin(sm)-1.274*sin(2*md-mm)-0.658*sin(2*md)-0.214*sin(2*mm)-0.112*sin(md)+360,360);
  illum:=(1+cos(degtorad(phase)))/2;
end;

procedure Moon(jdlt : double; out ra,de,phase,illum : double);
var jdut,jdtt,jd0,st0: double;
    dist,dkm,diam,q,h:double;
    y, m, d: integer;
begin
  jdut:=jdlt-ObsTimeZone/24;
  Djd(jdut,y, m, d, h);
  jd0:=jd(y, m, d, 0);
  st0:=SidTim(jd0, h, ObsLongitude);
  jdtt:=jdut+deltaT/3600/24;
  if load_de(jdtt) then begin
    MoonGeocentric(jdtt,ra,de,dist,dkm,diam,phase,illum);
    Paralaxe(st0, dist, ra, de, ra, de, q, jd2000, jdut);
  end
  else begin
   MoonLowprec(jdut,ra,de,phase,illum);
  end;
end;

procedure Sun(jdlt : double; out ra, de: double);
var
  jdut,jdtt, x, y, z, qr: double;
begin
  jdut:=jdlt-ObsTimeZone/24;
  jdtt:=jdut+deltaT/3600/24;
  if load_de(jdtt) then begin
    SunRect(jdtt, x, y, z);
    ra := arctan2(y, x);
    if (ra < 0) then
      ra := ra + pi2;
    qr := sqrt(x * x + y * y);
    if qr <> 0 then
      de := arctan(z / qr);
    PrecessionFK5(jd2000,jdtoday,ra,de);
  end
  else begin
  SunLowprec(jdut,ra,de,x);
  end;
end;

procedure SunEcl(jdlt : double; out l, b: double);
const
  sineps2k = 0.39777699580107953216;
  coseps2k = 0.917482131494378454;
var
  jdut,jdtt,x1, y1, z1, x, y, z, qr: double;
begin
  jdut:=jdlt-ObsTimeZone/24;
  jdtt:=jdut+deltaT/3600/24;
  if load_de(jdtt) then begin
    SunRect(jdtt, x1, y1, z1);
    // rotate equatorial to ecliptic
    x := x1;
    y := coseps2k * y1 + sineps2k * z1;
    z := -sineps2k * y1 + coseps2k * z1;
    // convert to polar
    l := arctan2(y, x);
    if (l < 0) then
      l := l + 2 * pi;
    qr := sqrt(x * x + y * y);
    if qr <> 0 then
      b := arctan(z / qr);
  end
  else begin
    SunLowprec(jdut,x,y,l);
    b:=0;
  end
end;

Procedure SunLowprec(jdn:double; out ra,de,l:double);
var d,ecl,q,g,r,xs,ys,xe,ye,ze: double;
begin
//Approximate Sun position
d :=jdn-jd2000;
// obliquity of the ecliptic
ecl := deg2rad * (23.439 - 0.00000036 * d);
// mean anomaly
g := deg2rad * rmod(357.529 + 0.98560028 * d,360);
// mean longitude
q := deg2rad * rmod(280.459 + 0.98564736 * d,360);
// geocentric apparent ecliptic longitude
l := q + deg2rad * (1.915 * sin(g) + 0.020 * sin(2*g));
// Sun distance
r := 1.00014 - 0.01671 * cos(g) - 0.00014 * cos(2*g);
// ecliptic rectangular geocentric coordinates
xs := r * cos(l);
ys := r * sin(l);
// equatorial rectangular geocentric coordinates
xe := xs;
ye := ys * cos(ecl);
ze := ys * sin(ecl);
// Sun Right Ascension and Declination
ra := arctan2( ye, xe );
de := arctan2( ze, sqrt(xe*xe+ye*ye) );
end;

procedure MoonLowprec(jdn:double; out ra,de,phase,illum:double);
var d,LE,M,F,l,b,e : double;
    t,sm,mm,md: double;
begin
//Very approximate Moon position
d :=jdn-jd2000;
// ecliptic longitude
LE := deg2rad * (218.316 + 13.176396 * d - 3.63258E-8 * d*d);
// mean anomaly
M := deg2rad * (134.963 + 13.064993 * d + 2.46324E-7 * d*d);
// mean distance
F := deg2rad * (93.272 + 13.229350 * d - 9.31666E-8 * d*d);
// longitude
l := LE + deg2rad * 6.289 * sin(M);
// latitude
b := deg2rad * 5.128 * sin(F);
// obliquity of the Earth
e := deg2rad * (23.4397 - 0.00000036 * d);
// Moon Right Ascension and Declination
ra:=arctan2(sin(l) * cos(e) - tan(b) * sin(e), cos(l));
de:=sin(sin(b) * cos(e) + cos(b) * sin(e) * sin(l));
ra:=rmod(ra+pi2,pi2);
// phase
t:=(jdn-2415020)/36525;  { meeus 15.1 }
sm:=degtorad(358.475833+35999.0498*t-0.000150*t*t-0.0000033*t*t*t);  {meeus 30. }
mm:=degtorad(296.104608+477198.8491*t+0.009192*t*t+0.0000144*t*t*t);
md:=rmod(350.737486+445267.1142*t-0.001436*t*t+0.0000019*t*t*t,360);
phase:=180-md ;     { meeus 31.4 }
md:=degtorad(md);
phase:=rmod(phase-6.289*sin(mm)+2.100*sin(sm)-1.274*sin(2*md-mm)-0.658*sin(2*md)-0.214*sin(2*mm)-0.112*sin(md)+360,360);
illum:=(1+cos(degtorad(phase)))/2;
end;

initialization
de_type := 0;
de_jdcheck := MaxInt;
de_jdstart := MaxInt;
de_jdend := -MaxInt;


end.

