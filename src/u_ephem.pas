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

// libplan404
const
{$ifdef linux}
      lib404   = 'libpasplan404.so.1';
{$endif}
{$ifdef darwin}
      lib404   = 'libplan404.dylib';
{$endif}
{$ifdef mswindows}
      lib404 = 'libplan404.dll';
{$endif}

type
     TPlanetData = record
        JD : double;
         l : double ;
         b : double ;
         r : double ;
         x : double ;
         y : double ;
         z : double ;
         ipla : integer;
     end;
     PPlanetData = ^TPlanetData;
     TPlan404=Function( pla : PPlanetData):integer; cdecl;

var Plan404 : TPlan404;
    Plan404lib: TLibHandle;


procedure Load_Plan404;
procedure Moon(jdlt : double; out ra,de,phase,illum : double);
procedure Sun(jdlt : double; out ra, de: double);
procedure SunEcl(jdlt : double; out l, b: double);

implementation

uses u_utils;

procedure Load_Plan404;
begin
  Plan404    := nil;
  Plan404lib := LoadLibrary(lib404);
  if Plan404lib <> 0 then
  begin
    Plan404 := TPlan404(GetProcAddress(Plan404lib, 'Plan404'));
  end;
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
   p :TPlanetData;
   q : double;
   t,sm,mm,md : double;
begin
  jdtt:=jdtt-(1.27/3600/24); // mean lighttime
  p.JD:=jdtt;
  p.ipla:=11;
  Plan404(addr(p));
  dist:=sqrt(p.x*p.x+p.y*p.y+p.z*p.z);
  alpha:=arctan2(p.y,p.x);
  if (alpha<0) then alpha:=alpha+pi2;
  q:=sqrt(p.x*p.x+p.y*p.y);
  delta:=arctan(p.z/q);
  // revert aberration to move position to solar system barycenter as with jpl eph
  mean_equatorial(alpha,delta,true,false);
  // plan404 give equinox of the date for the moon.
  PrecessionFK5(jdtt,jd2000,alpha,delta);
  dkm:=dist*km_au;
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
  MoonGeocentric(jdtt,ra,de,dist,dkm,diam,phase,illum);
  Paralaxe(st0, dist, ra, de, ra, de, q, jd2000, jdut);
end;

procedure SunRect(jdtt: double; out x, y, z: double);
var
  p: TPlanetData;
begin
  p.ipla := 3;
  p.JD := jdtt;
  Plan404(addr(p));
  x := -p.x;
  y := -p.y;
  z := -p.z;
end;

procedure Sun(jdlt : double; out ra, de: double);
var
  jdut,jdtt, x, y, z, qr: double;
begin
  jdut:=jdlt-ObsTimeZone/24;
  jdtt:=jdut+deltaT/3600/24;
  SunRect(jdtt, x, y, z);
  ra := arctan2(y, x);
  if (ra < 0) then
    ra := ra + pi2;
  qr := sqrt(x * x + y * y);
  if qr <> 0 then
    de := arctan(z / qr);
  PrecessionFK5(jd2000,jdtoday,ra,de);
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
end;


end.

