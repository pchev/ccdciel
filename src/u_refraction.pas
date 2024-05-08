unit u_refraction;

{
Copyright (C) 2002 Patrick Chevalley

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
{
 Projection functions

 Portion by Patrick Wallace, RAL Space, UK
}
{$mode objfpc}{$H+}
interface

uses
  u_global, Math, SysUtils, Graphics;

procedure Refraction(var h: double; flag: boolean; method: smallint = 2);
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
procedure sla_REFCO(HM, TDK, PMB, RH, WL, PHI, TLR, EPS: double;
  var REFA, REFB: double);
procedure sla_REFRO(ZOBS, HM, TDK, PMB, RH, WL, PHI, TLR, EPS: double; var REF: double);
procedure sla_REFCOQ(TDK, PMB, RH, WL: double; var REFA, REFB: double);
procedure sla_REFZ(ZU, REFA, REFB: double; var ZR: double);
procedure sla_GEOC(P, H: double; var R, Z: double);

implementation

const RefractionWavelength = 0.55;

procedure Refraction(var h: double; flag: boolean; method: smallint = 2);
const
  ZBREAK = 0.242535625;
var
  h1, R, DZD: double;
  I: integer;
begin
  if flag then
  begin   // true -> apparent
    case method of
      0:
      begin  { no refraction for testing}
        h := h;
      end;
      1:
      begin  { Bennett 2010, meeus91 15.3, 15.4 }
        h1 := rad2deg * h;
        if h1 > -1 then
        begin
          R := cotan(deg2rad * (h1 + 9.48 / (h1 + 4.8)));
          R := R - 0.06 * sin(deg2rad * (14.7 * R + 13));
          h := double(MinValue([pid2, h + deg2rad * (R) / 60]));
        end
        else
          h := h + deg2rad * 0.64658062088 * (h1 + 90) / 89;
      end;
      2:
      begin  { slalib }
        if (rad2deg * h) > -1 then
        begin
          h1 := pid2 - h;
          sla_REFCO(ObsElevation, 273.15 + ObsTemperature, ObsPressure,
                    ObsHumidity, RefractionWavelength, deg2rad * ObsLatitude, ObsTlr/1000,
                    1E-8, ObsRefA, ObsRefb);
          sla_REFZ(h1, ObsRefA, ObsRefB, h);
          if (COS(h) < ZBREAK) then
          begin // from aopqk.f
            I := 1;
            DZD := 1E1;
            while (ABS(DZD) > 1E-10) and (I <= 10) do
            begin
              // Compute refraction using current estimate of observed ZD
              sla_REFRO(h, ObsElevation, 273 + ObsTemperature,
                ObsPressure, ObsHumidity/100, 0.55, deg2rad * ObsLatitude,
                ObsTlr/1000, 1E-8, R);
              // Remaining discrepancy
              DZD := h + R - h1;
              // Update the estimate
              h := h - DZD;
              // Increment the iteration counter
              I := I + 1;
            end;
          end;
          h := pid2 - h;
        end
        else
          h := h + deg2rad * 0.64658062088 * (rad2deg * h + 90) / 89;
      end;
    end;
  end
  else
  begin      // apparent -> true
    case method of
      0:
      begin  { no refraction for testing}
        h := h;
      end;
      1:
      begin   { Bennett 2010, meeus91 15.3, 15.4 }
        h1 := rad2deg * h;
        if h1 > -0.347259404573 then
        begin
          R := cotan(deg2rad * (0.99914 * h1 + (7.31 / (h1 + 4.4))));
          R := R - 0.06 * sin(deg2rad * (14.7 * R + 13));
          h := double(MinValue([pid2, h - deg2rad * (R) / 60]));
        end
        else
          h := h - deg2rad * 0.65705159 * (h1 + 90) / 89.64658;
      end;
      2:
      begin   { slalib }
        if (rad2deg * h) > -0.327565049146 then
        begin
          h1 := pid2 - h;
          sla_REFRO(h1, ObsElevation, 273 + ObsTemperature,
            ObsPressure, ObsHumidity/100, 0.55, deg2rad * ObsLatitude, ObsTlr/1000, 1E-8, R);
          h1 := h1 + R;
          h := pid2 - h1;
        end
        else
          h := h - deg2rad * 0.65705159 * (rad2deg * h + 90) / 89.64658;
      end;
    end;
  end;
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


/////////////////////////////////////////
// Refraction from slalib
// P.T.Wallace   Starlink
// License: GPL
// converted from Fortran to Pascal
/////////////////////////////////////////

procedure sla__ATMS(RT, TT, DNT, GAMAL, R: double; var DN, RDNDR: double);
//  Internal routine used by REFRO

//  Refractive index and derivative with respect to height for the
//  stratosphere.

//  Given:
//    RT      d    height of tropopause from centre of the Earth (metre)
//    TT      d    temperature at the tropopause (K)
//    DNT     d    refractive index at the tropopause
//    GAMAL   d    constant of the atmospheric model = G*MD/R
//    R       d    current distance from the centre of the Earth (metre)

//  Returned:
//    DN      d    refractive index at R
//    RDNDR   d    R * rate the refractive index is changing at R

var
  B, W: double;

begin
  B := GAMAL / TT;
  W := (DNT - 1) * EXP(-B * (R - RT));
  DN := 1 + W;
  RDNDR := -R * B * W;
end;

procedure sla__ATMT(R0, T0, ALPHA, GAMM2, DELM2, C1, C2, C3, C4, C5, C6, R: double;
  var T, DN, RDNDR: double);
//  Internal routine used by REFRO

//  Refractive index and derivative with respect to height for the
//  troposphere.

//  Given:
//    R0      d    height of observer from centre of the Earth (metre)
//    T0      d    temperature at the observer (K)
//    ALPHA   d    alpha          )
//    GAMM2   d    gamma minus 2  ) see HMNAO paper
//    DELM2   d    delta minus 2  )
//    C1      d    useful term  )
//    C2      d    useful term  )
//    C3      d    useful term  ) see source
//    C4      d    useful term  ) of sla_REFRO
//    C5      d    useful term  )
//    C6      d    useful term  )
//    R       d    current distance from the centre of the Earth (metre)

//  Returned:
//    T       d    temperature at R (K)
//    DN      d    refractive index at R
//    RDNDR   d    R * rate the refractive index is changing at R

//  Note that in the optical case C5 and C6 are zero.

var
  TT0, TT0GM2, TT0DM2: double;

begin
  T := MAX(MIN(T0 - ALPHA * (R - R0), 320), 100);
  TT0 := T / T0;
  TT0GM2 := TT0 ** GAMM2;
  TT0DM2 := TT0 ** DELM2;
  DN := 1 + (C1 * TT0GM2 - (C2 - C5 / T) * TT0DM2) * TT0;
  RDNDR := R * (-C3 * TT0GM2 + (C4 - C6 / TT0) * TT0DM2);
end;

function  Rmod(x,y:Double):Double;
BEGIN
    Rmod := x - Int(x/y) * y ;
END  ;

procedure sla_REFRO(ZOBS, HM, TDK, PMB, RH, WL, PHI, TLR, EPS: double; var REF: double);
//  Atmospheric refraction for radio and optical/IR wavelengths.

//  Given:
//    ZOBS    d  observed zenith distance of the source (radian)
//    HM      d  height of the observer above sea level (metre)
//    TDK     d  ambient temperature at the observer (K)
//    PMB     d  pressure at the observer (millibar)
//    RH      d  relative humidity at the observer (range 0-1)
//    WL      d  effective wavelength of the source (micrometre)
//    PHI     d  latitude of the observer (radian, astronomical)
//    TLR     d  temperature lapse rate in the troposphere (K/metre)
//    EPS     d  precision required to terminate iteration (radian)

//  Returned:
//    REF     d  refraction: in vacuo ZD minus observed ZD (radian)



//  Fixed parameters

const
  //  93 degrees in radians
  D93 = 1.623156204;
  //  Universal gas constant
  GCR = 8314.32;
  //  Molecular weight of dry air
  DMD = 28.9644;
  //  Molecular weight of water vapour
  DMW = 18.0152;
  //  Mean Earth radius (metre)
  S = 6378120.0;
  //  Exponent of temperature dependence of water vapour pressure
  DELTA = 18.36;
  //  Height of tropopause (metre)
  HT = 11000.0;
  //  Upper limit for refractive effects (metre)
  HS = 80000.0;
  //  Numerical integration: maximum number of strips.
  ISMAX = 16384;

var
  IS1, K, N, I, J: integer;
  OPTIC, LOOP: boolean;
  ZOBS1, ZOBS2, HMOK, TDKOK, PMBOK, RHOK, WLOK, ALPHA, TOL, WLSQ,
  GB, A, GAMAL, GAMMA, GAMM2, DELM2, TDC, PSAT, PWO, W, C1, C2, C3,
  C4, C5, C6, R0, TEMPO, DN0, RDNDR0, SK0, F0, RT, TT, DNT, RDNDRT,
  SINE, ZT, FT, DNTS, RDNDRP, ZTS, FTS, RS, DNS, RDNDRS, ZS, FS,
  REFOLD, Z0, ZRANGE, FB, FF, FO, FE, H, R, SZ, RG, DR, TG, DN, RDNDR,
  T, F, REFP, REFT: double;

  //  Normalize angle into range +/- pi  (double precision)
  function sla_DRANGE(ANGLE: double): double;
  const
    DPI = 3.141592653589793238462643;
    D2PI = 6.283185307179586476925287;
  begin
    sla_DRANGE := RMOD(ANGLE, D2PI);
    if (ABS(sla_DRANGE) >= DPI) then
      sla_DRANGE := sla_DRANGE - D2PI * SIGN(ANGLE);
  end;

  //  The refraction integrand
  function REFI(DN, RDNDR: double): double;
  begin
    REFI := RDNDR / (DN + RDNDR);
  end;

begin
  //  Transform ZOBS into the normal range.
  ZOBS1 := sla_DRANGE(ZOBS);
  ZOBS2 := MIN(ABS(ZOBS1), D93);

  //  Keep other arguments within safe bounds.
  HMOK := MIN(MAX(HM, -1E3), HS);
  TDKOK := MIN(MAX(TDK, 100), 500);
  PMBOK := MIN(MAX(PMB, 0), 10000);
  RHOK := MIN(MAX(RH, 0), 1);
  WLOK := MAX(WL, 0.1);
  ALPHA := MIN(MAX(ABS(TLR), 0.001), 0.01);

  //  Tolerance for iteration.
  TOL := MIN(MAX(ABS(EPS), 1E-12), 0.1) / 2;

  //  Decide whether optical/IR or radio case - switch at 100 microns.
  OPTIC := (WLOK <= 100);

  //  Set up model atmosphere parameters defined at the observer.
  WLSQ := WLOK * WLOK;
  GB := 9.784 * (1 - 0.0026 * COS(PHI + PHI) - 0.00000028 * HMOK);
  if (OPTIC) then
    A := (287.6155 + (1.62887 + 0.01360 / WLSQ) / WLSQ) * 273.15E-6 / 1013.25
  else
    A := 77.6890E-6;
  GAMAL := (GB * DMD) / GCR;
  GAMMA := GAMAL / ALPHA;
  GAMM2 := GAMMA - 2;
  DELM2 := DELTA - 2;
  TDC := TDKOK - 273.15;
  PSAT := 10 ** ((0.7859 + 0.03477 * TDC) / (1 + 0.00412 * TDC)) *
    (1 + PMBOK * (4.5E-6 + 6E-10 * TDC * TDC));
  if (PMBOK > 0) then
    PWO := RHOK * PSAT / (1 - (1 - RHOK) * PSAT / PMBOK)
  else
    PWO := 0;
  W := PWO * (1 - DMW / DMD) * GAMMA / (DELTA - GAMMA);
  C1 := A * (PMBOK + W) / TDKOK;
  if (OPTIC) then
    C2 := (A * W + 11.2684E-6 * PWO) / TDKOK
  else
    C2 := (A * W + 6.3938E-6 * PWO) / TDKOK;
  C3 := (GAMMA - 1) * ALPHA * C1 / TDKOK;
  C4 := (DELTA - 1) * ALPHA * C2 / TDKOK;
  if (OPTIC) then
  begin
    C5 := 0;
    C6 := 0;
  end
  else
  begin
    C5 := 375463E-6 * PWO / TDKOK;
    C6 := C5 * DELM2 * ALPHA / (TDKOK * TDKOK);
  end;

  //  Conditions at the observer.
  R0 := S + HMOK;
  sla__ATMT(R0, TDKOK, ALPHA, GAMM2, DELM2, C1, C2, C3, C4, C5, C6,
    R0, TEMPO, DN0, RDNDR0);
  SK0 := DN0 * R0 * SIN(ZOBS2);
  F0 := REFI(DN0, RDNDR0);

  //  Conditions in the troposphere at the tropopause.
  RT := S + MAX(HT, HMOK);
  sla__ATMT(R0, TDKOK, ALPHA, GAMM2, DELM2, C1, C2, C3, C4, C5, C6, RT, TT, DNT, RDNDRT);
  SINE := SK0 / (RT * DNT);
  ZT := ArcTAN2(SINE, SQRT(MAX(1 - SINE * SINE, 0)));
  FT := REFI(DNT, RDNDRT);

  //  Conditions in the stratosphere at the tropopause.
  sla__ATMS(RT, TT, DNT, GAMAL, RT, DNTS, RDNDRP);
  SINE := SK0 / (RT * DNTS);
  ZTS := ArcTAN2(SINE, SQRT(MAX(1 - SINE * SINE, 0)));
  FTS := REFI(DNTS, RDNDRP);

  //  Conditions at the stratosphere limit.
  RS := S + HS;
  sla__ATMS(RT, TT, DNT, GAMAL, RS, DNS, RDNDRS);
  SINE := SK0 / (RS * DNS);
  ZS := ArcTAN2(SINE, SQRT(MAX(1 - SINE * SINE, 0)));
  FS := REFI(DNS, RDNDRS);

  //  Variable initialization to avoid compiler warning.
  REFT := 0;

  //  Integrate the refraction integral in two parts;  first in the
  //  troposphere (K=1), then in the stratosphere (K=2).

  for K := 1 to 2 do
  begin

    //     Initialize previous refraction to ensure at least two iterations.
    REFOLD := 1;

    //     Start off with 8 strips.
    IS1 := 8;

    //     Start Z, Z range, and start and end values.
    if (K = 1) then
    begin
      Z0 := ZOBS2;
      ZRANGE := ZT - Z0;
      FB := F0;
      FF := FT;
    end
    else
    begin
      Z0 := ZTS;
      ZRANGE := ZS - Z0;
      FB := FTS;
      FF := FS;
    end;

    //     Sums of odd and even values.
    FO := 0;
    FE := 0;

    //     First time through the loop we have to do every point.
    N := 1;

    //     Start of iteration loop (terminates at specified precision).
    LOOP := True;
    while LOOP do
    begin

      //        Strip width.
      H := ZRANGE / IS1;

      //        Initialize distance from Earth centre for quadrature pass.
      if (K = 1) then
        R := R0
      else
        R := RT;

      //        One pass (no need to compute evens after first time).
      I := 1;
      while I < IS1 do
      begin  // replace for loop with variable increment

        //           Sine of observed zenith distance.
        SZ := SIN(Z0 + H * I);

        //           Find R (to the nearest metre, maximum four iterations).
        if (SZ > 1E-20) then
        begin
          W := SK0 / SZ;
          RG := R;
          DR := 1E6;
          J := 0;
          while ((ABS(DR) > 1) and (J < 4)) do
          begin
            J := J + 1;
            if (K = 1) then
              sla__ATMT(R0, TDKOK, ALPHA, GAMM2, DELM2, C1, C2,
                C3, C4, C5, C6, RG, TG, DN, RDNDR)
            else
              sla__ATMS(RT, TT, DNT, GAMAL, RG, DN, RDNDR);
            DR := (RG * DN - W) / (DN + RDNDR);
            RG := RG - DR;
          end; // while
          R := RG;
        end;

        //           Find the refractive index and integrand at R.
        if (K = 1) then
          sla__ATMT(R0, TDKOK, ALPHA, GAMM2, DELM2, C1, C2, C3, C4,
            C5, C6, R, T, DN, RDNDR)
        else
          sla__ATMS(RT, TT, DNT, GAMAL, R, DN, RDNDR);
        F := REFI(DN, RDNDR);

        //           Accumulate odd and (first time only) even values.
        if ((N = 1) and ((I mod 2) = 0)) then
          FE := FE + F
        else
          FO := FO + F;

        Inc(I, N);
      end; // while I // for I

      //        Evaluate the integrand using Simpson's Rule.
      REFP := H * (FB + 4 * FO + 2 * FE + FF) / 3;

      //        Has the required precision been achieved (or can't be)?
      if ((ABS(REFP - REFOLD) > TOL) and (IS1 < ISMAX)) then
      begin

        //           No: prepare for next iteration.

        //           Save current value for convergence test.
        REFOLD := REFP;

        //           Double the number of strips.
        IS1 := IS1 + IS1;

        //           Sum of all current values = sum of next pass's even values.
        FE := FE + FO;

        //           Prepare for new odd values.
        FO := 0;

        //           Skip even values next time.
        N := 2;
      end
      else
      begin

        //           Yes: save troposphere component and terminate the loop.
        if (K = 1) then
          REFT := REFP;
        LOOP := False;
      end; // IF
    end; // WHILE LOOP
  end; // for K

  //  Result.
  REF := REFT + REFP;
  if (ZOBS1 < 0) then
    REF := -REF;

end;


procedure sla_REFCO(HM, TDK, PMB, RH, WL, PHI, TLR, EPS: double;
  var REFA, REFB: double);
//  Determine the constants A and B in the atmospheric refraction
//  model dZ = A tan Z + B tan**3 Z.

//  Z is the "observed" zenith distance (i.e. affected by refraction)
//  and dZ is what to add to Z to give the "topocentric" (i.e. in vacuo)
//  zenith distance.

//  Given:
//    HM      d     height of the observer above sea level (metre)
//    TDK     d     ambient temperature at the observer (K)
//    PMB     d     pressure at the observer (millibar)
//    RH      d     relative humidity at the observer (range 0-1)
//    WL      d     effective wavelength of the source (micrometre)
//    PHI     d     latitude of the observer (radian, astronomical)
//    TLR     d     temperature lapse rate in the troposphere (K/metre)
//    EPS     d     precision required to terminate iteration (radian)

//  Returned:
//    REFA    d     tan Z coefficient (radian)
//    REFB    d     tan**3 Z coefficient (radian)

var
  R1, R2: double;

  //  Sample zenith distances: arctan(1) and arctan(4)
const
  ATN1 = 0.7853981633974483;
  ATN4 = 1.325817663668033;

begin
  //  Determine refraction for the two sample zenith distances
  sla_REFRO(ATN1, HM, TDK, PMB, RH, WL, PHI, TLR, EPS, R1);
  sla_REFRO(ATN4, HM, TDK, PMB, RH, WL, PHI, TLR, EPS, R2);

  //  Solve for refraction constants
  REFA := (64 * R1 - R2) / 60;
  REFB := (R2 - 4 * R1) / 60;

end;

procedure sla_REFCOQ(TDK, PMB, RH, WL: double; var REFA, REFB: double);

//  Given:
//    TDK      d      ambient temperature at the observer (K)
//    PMB      d      pressure at the observer (millibar)
//    RH       d      relative humidity at the observer (range 0-1)
//    WL       d      effective wavelength of the source (micrometre)

//  Returned:
//    REFA     d      tan Z coefficient (radian)
//    REFB     d      tan**3 Z coefficient (radian)

var
  OPTIC: boolean;
  T, P, R, W, TDC, PS, PW, WLSQ, GAMMA, BETA: double;

begin
  //  Decide whether optical/IR or radio case:  switch at 100 microns.
  OPTIC := (WL <= 100);

  //  Restrict parameters to safe values.
  T := MIN(MAX(TDK, 100), 500);
  P := MIN(MAX(PMB, 0), 10000);
  R := MIN(MAX(RH, 0), 1);
  W := MIN(MAX(WL, 0.1), 1E6);

  //  Water vapour pressure at the observer.
  if (P > 0) then
  begin
    TDC := T - 273.15;
    PS := 10 ** ((0.7859 + 0.03477 * TDC) / (1 + 0.00412 * TDC)) *
      (1 + P * (4.5E-6 + 6E-10 * TDC * TDC));
    PW := R * PS / (1 - (1 - R) * PS / P);
  end
  else
  begin
    PW := 0;
  end;

  //  Refractive index minus 1 at the observer.
  if OPTIC then
  begin
    WLSQ := W * W;
    GAMMA := ((77.53484E-6 + (4.39108E-7 + 3.666E-9 / WLSQ) / WLSQ) *
      P - 11.2684E-6 * PW) / T;
  end
  else
  begin
    GAMMA := (77.6890E-6 * P - (6.3938E-6 - 0.375463E0 / T) * PW) / T;
  end;

  //  Formula for beta adapted from Stone, with empirical adjustments.
  BETA := 4.4474E-6 * T;
  if (not OPTIC) then
    BETA := BETA - 0.0074 * PW * BETA;

  //  Refraction constants from Green.
  REFA := GAMMA * (1 - BETA);
  REFB := -GAMMA * (BETA - GAMMA / 2);

end;

procedure sla_REFZ(ZU, REFA, REFB: double; var ZR: double);

//  Given:
//    ZU    dp    unrefracted zenith distance of the source (radian)
//    REFA  dp    tan Z coefficient (radian)
//    REFB  dp    tan**3 Z coefficient (radian)

//  Returned:
//    ZR    dp    refracted zenith distance (radian)

const
  //  Radians to degrees
  R2D = 57.29577951308232;

  //  Largest usable ZD (deg)
  D93 = 93.0;

  //  Coefficients for high ZD model (used beyond ZD 83 deg)
  C1 = +0.55445;
  C2 = -0.01133;
  C3 = +0.00202;
  C4 = +0.28385;
  C5 = +0.02390;

  //  ZD at which one model hands over to the other (radians)
  Z83 = 83.0 / R2D;

  //  High-ZD-model prediction (deg) for that point
  REF83 = (C1 + C2 * 7 + C3 * 49) / (1 + C4 * 7 + C5 * 49);

var
  ZU1, ZL, S, C, T, TSQ, TCU, REF, E, E2: double;

begin
  //  Perform calculations for ZU or 83 deg, whichever is smaller
  ZU1 := MIN(ZU, Z83);

  //  Functions of ZD
  ZL := ZU1;
  S := SIN(ZL);
  C := COS(ZL);
  T := S / C;
  TSQ := T * T;
  TCU := T * TSQ;

  //  Refracted ZD (mathematically to better than 1 mas at 70 deg)
  ZL := ZL - (REFA * T + REFB * TCU) / (1 + (REFA + 3 * REFB * TSQ) / (C * C));

  //  Further iteration
  S := SIN(ZL);
  C := COS(ZL);
  T := S / C;
  TSQ := T * T;
  TCU := T * TSQ;
  REF := ZU1 - ZL + (ZL - ZU1 + REFA * T + REFB * TCU) /
    (1 + (REFA + 3 * REFB * TSQ) / (C * C));

  // Special handling for large ZU
  if (ZU > ZU1) then
  begin
    E := 90 - MIN(D93, ZU * R2D);
    E2 := E * E;
    REF := (REF / REF83) * (C1 + C2 * E + C3 * E2) / (1 + C4 * E + C5 * E2);
  end;

  //  Return refracted ZD
  ZR := ZU - REF;

end;

procedure sla_GEOC(P, H: double; var R, Z: double);

// Convert geodetic position to geocentric (double precision)

// Given:
// P dp latitude (geodetic, radians)
// H dp height above reference spheroid (geodetic, metres)

// Returned:
// R dp distance from Earth axis (AU)
// Z dp distance from plane of Earth equator (AU)

const
  // Earth equatorial radius (metres)
  A0 = 6378140;
  // Reference spheroid flattening factor and useful function
  F = 1 / 298.257;
  B = (1 - F) * (1 - F);
  // Astronomical unit in metres
  AU = 1.49597870E11;

var
  SP, CP, C, S: double;
begin
  // Geodetic to geocentric conversion
  SP := SIN(P);
  CP := COS(P);
  C := 1 / SQRT(CP * CP + B * SP * SP);
  S := B * C;
  R := (A0 * C + H) * CP / AU;
  Z := (A0 * S + H) * SP / AU;
end;

end.
