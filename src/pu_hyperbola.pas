unit pu_hyperbola; {Hyperbola modeling of the star disk size in HFD as function of the telescope focuser position. Purpose is finding the best focuser position at the hyperbola minimum.}
{
Copyright (C) 2018, 2026 Patrick Chevalley & Han Kleijn (author)

http://www.ap-i.net
h@ap-i.net

http://www.hnsky.org

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
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  u_global;


procedure find_best_hyperbola_fit(var data:array of TDouble2 ;data_length:integer;out p,a,b, lowest_error : double; out iteration_cycles: integer); {input data[n,1]=position,data[n,2]=hfd, output: bestfocusposition=p, a, b of hyperbola}
function hfd_calc(position,perfectfocusposition,a,b:double) :double; {calculate HFD from position and perfectfocusposition using hyperbola parameters}
function steps_to_focus(hfd,a,b:double) :double; {calculates focuser steps to perfect focus from HFD and hyperbola parameters}



implementation


function hfd_calc(position,perfectfocusposition,a,b:double) :double; {calculate HFD from position and perfectfocusposition using hyperbola parameters}
{The HFD (half flux diameter) of the imaged star disk as function of the focuser position can be described as hyperbola}
{a,b are the hyperbola parameters, a is the lowest HFD value at focus position, the asymptote y:= +-x*a/b} {rev1}
{A vertical hyperbola is defined as: sqr(y/a)-sqr(x/b)=1}
var
  x : double;
begin
  x:=perfectfocusposition - position;
  result:=a*sqrt(1+sqr(x/b));//The y or hfd value
end;


function steps_to_focus(hfd,a,b:double) :double; {calculates focuser steps to perfect focus from HFD and hyperbola parameters}
{The HFD (half flux diameter) of the imaged star disk as function of the focuser position can be described as hyperbola. The y-axis is the HFD. The x-axis the offset from the perfect focus position }
{a,b are the hyperbola parameters, a is the lowest HFD value at focus position, the asymptote y:= +-x*a/b} {rev1}
{A vertical hyperbola is defined as: sqr(y/a)-sqr(x/b)=1}
{Note using the HFD there are two solutions, either left or right side of the hyperbola}
var
  k : double;
begin
  k:=hfd/a;
  if k<1 then k:=1;{prevent run time errors}
  result:=b*sqrt(sqr(k)-1); //calculate x position of the vertical hyperbola
end;


function mean_error_hyperbola(var data: array of TDouble2 {pos, hfd};data_length:integer; perfectfocusposition,a,b : double): double;{calculates total averaged error between measured V-curve and hyperbola}
var
  i : integer;
  hfd_simulation, total_error,error : double;
begin
  total_error:=0;
  for i:=0 to data_length-1 do
  begin
    hfd_simulation:=hfd_calc(data[i,1],perfectfocusposition,a,b);{y or HFD error}
    total_error := total_error + abs(hfd_simulation - data[i,2]) / (0.1+data[i,2]); //linear error, 0.1 protects against zero measurements and high points have a tiny amount of weigth
  end;
  result:=(total_error)/data_length;{scale to average HFD error per point}
end;


procedure find_best_hyperbola_fit(var data:array of TDouble2 ;data_length:integer;out p,a,b, lowest_error : double; out iteration_cycles: integer); {input data[n,1]=position,data[n,2]=hfd, output: bestfocusposition=p, a, b of hyperbola}
                                                                                                                                                {lowest_error: mean HFD error after curve fitting, iteration_cycles: how many cycles where used for the curve fitting}
{The input data array should contain several focuser positions with there corresponding HFD (star disk size).}
{The routine will try to find the best hyperbola curve fit using the Nelder-Mead simplex method.}
{The focuser position p at the hyperbola minimum is the expected best focuser position.}
{
  Nelder-Mead overview (downhill simplex method)
  ~~~~~~~~~~~~~~~~~~~~
  The three parameters to optimise are p (best focus position), a (HFD at focus) and b (hyperbola width).
  They form a point in 3-D parameter space.  Nelder-Mead maintains a "simplex": a set of 4 such points
  (one more than the number of dimensions).  In each iteration the worst point is replaced by a better
  one found by reflecting/expanding/contracting the simplex.  No derivatives are needed; only the error
  function is evaluated.  Typical convergence: a few hundred evaluations
  See also https://en.wikipedia.org/wiki/Nelder%E2%80%93Mead_method

  Simplex vertices are stored as:
    sv[0..3, 0] = p  (best focus position)
    sv[0..3, 1] = a  (hyperbola minimum HFD)
    sv[0..3, 2] = b  (hyperbola width parameter)
    sf[0..3]      = error value for each vertex
}
const
  MAX_ITER   = 500;   {maximum number of Nelder-Mead iterations}
  NM_DIMS    = 3;     {number of parameters: p, a, b}
  NM_VERTS   = 4;     {vertices in simplex = NM_DIMS + 1}
  {Nelder-Mead reflection coefficients (standard values)}
  ALPHA      = 1.0;   {reflection}
  BETA       = 0.5;   {contraction}
  GAMMA      = 2.0;   {expansion}
  SIGMA      = 0.5;   {shrink}

var
  i, j, iter,
  idx_best, idx_worst, idx_2nd_worst                                       : integer;
  highest_hfd, lowest_hfd, highest_hfd_position,  lowest_hfd_position,
  f_reflect, f_expand, f_contract                                          : double;
  sv  : array[0..NM_VERTS-1, 0..NM_DIMS-1] of double; {simplex vertices}
  sf  : array[0..NM_VERTS-1] of double;               {error at each vertex}
  centroid, reflect, expand, contract    : array[0..NM_DIMS-1] of double;

  {-- Helper: evaluate error for a vertex stored as array[0..2] --}
  function vertex_error(const v: array of double): double;
  begin
    {Guard against unphysical parameter values}
    if (v[1] <= 0) or (v[2] <= 0) then
      result := 1E99
    else
      result := mean_error_hyperbola(data, data_length, v[0], v[1], v[2]);
  end;

begin
  lowest_error := 1E99;

  {------------------------------------------------------------------
    STEP 1 – find good starting values
  ------------------------------------------------------------------}
  highest_hfd          := 0;
  lowest_hfd           := 1E99;
  highest_hfd_position := 0;
  lowest_hfd_position  := 0;

  for i := 0 to data_length - 1 do
  begin
    if data[i,2] > highest_hfd then
    begin
      highest_hfd          := data[i,2];
      highest_hfd_position := data[i,1];
    end;
    if (data[i,2] < lowest_hfd) and (data[i,2] > 0.1) {avoid zeros} then
    begin
      lowest_hfd          := data[i,2];
      lowest_hfd_position := data[i,1];
    end;
  end;
  if highest_hfd_position < lowest_hfd_position then
    highest_hfd_position := (lowest_hfd_position - highest_hfd_position) + lowest_hfd_position; {go up always}

  {Initial estimates for a, b, p}
  a := lowest_hfd;                                                                               {a is near the HFD value at focus}
  b := (highest_hfd_position - lowest_hfd_position) / sqrt(-1 + sqr(highest_hfd / a));          {rev2}
  p := lowest_hfd_position;

  {------------------------------------------------------------------
    STEP 2 – build the initial simplex
    One vertex is the starting estimate; the other three are offset
    along each parameter axis by a fraction of the initial scale.
    Using 20% of the initial a and b values and the p search range
    as step sizes gives a simplex that covers the relevant region
    without being so large that it starts outside valid parameter space.
  ------------------------------------------------------------------}
  {Vertex 0: the initial estimate itself}
  sv[0,0] := p;  sv[0,1] := a;  sv[0,2] := b;
  {Vertex 1: p shifted by the estimated half-width of the V-curve}
  sv[1,0] := p + (highest_hfd_position - lowest_hfd_position);
  sv[1,1] := a;
  sv[1,2] := b;
  {Vertex 2: a shifted by 20%}
  sv[2,0] := p;
  sv[2,1] := a * 1.2;
  sv[2,2] := b;
  {Vertex 3: b shifted by 20%}
  sv[3,0] := p;
  sv[3,1] := a;
  sv[3,2] := b * 1.2;

  {Evaluate error at each initial vertex}
  for i := 0 to NM_VERTS - 1 do
    sf[i] := vertex_error(sv[i]);

  {------------------------------------------------------------------
    STEP 3 – Nelder-Mead iteration loop, https://en.wikipedia.org/wiki/Nelder%E2%80%93Mead_method
  ------------------------------------------------------------------}
  iteration_cycles := 0;

  for iter := 1 to MAX_ITER do
  begin
    {--- 3a: sort vertices so [0]=best, [NM_VERTS-1]=worst ---}
    {Simple insertion sort on 4 elements}
    for i := 1 to NM_VERTS - 1 do
    begin
      for j := i downto 1 do
      begin
        if sf[j] < sf[j-1] then
        begin
          {swap errors}
          f_reflect := sf[j];  sf[j] := sf[j-1];  sf[j-1] := f_reflect;
          {swap vertices}
          f_expand  := sv[j,0]; sv[j,0] := sv[j-1,0]; sv[j-1,0] := f_expand;
          f_expand  := sv[j,1]; sv[j,1] := sv[j-1,1]; sv[j-1,1] := f_expand;
          f_expand  := sv[j,2]; sv[j,2] := sv[j-1,2]; sv[j-1,2] := f_expand;
        end;
      end;
    end;

    idx_best     := 0;
    idx_worst    := NM_VERTS - 1;
    idx_2nd_worst:= NM_VERTS - 2;

    {--- 3b: convergence check – stop when simplex is tiny ---}
    if (sf[idx_worst] - sf[idx_best]) < 1E-5 then
      break; //curve fitting completed

    {--- 3c: compute centroid of all vertices except the worst ---}
    for j := 0 to NM_DIMS - 1 do
    begin
      centroid[j] := 0;
      for i := 0 to NM_VERTS - 2 do   {skip idx_worst = last after sort}
        centroid[j] := centroid[j] + sv[i,j];
      centroid[j] := centroid[j] / (NM_VERTS - 1);// The centroid should be the average of all vertices except the worst
    end;

    {--- 3d: reflection ---}
    for j := 0 to NM_DIMS - 1 do
      reflect[j] := centroid[j] + ALPHA * (centroid[j] - sv[idx_worst,j]); //With ALPHA = 1.0 this means reflect is exactly as far from the centroid as W is, but on the opposite side. You are "flipping" the worst point across the centroid, hoping to land somewhere better — moving away from the bad region.
    f_reflect := vertex_error(reflect); //calculate error for new point

    if (f_reflect < sf[idx_best]) then
    begin {--- 3e: expansion (reflection improved best, try going further) ---}
      for j := 0 to NM_DIMS - 1 do
        expand[j] := centroid[j] + GAMMA * (reflect[j] - centroid[j]);
      f_expand := vertex_error(expand);
      if f_expand < f_reflect then
      begin
        {accept expansion}
        for j := 0 to NM_DIMS - 1 do sv[idx_worst,j] := expand[j];
        sf[idx_worst] := f_expand;
      end
      else
      begin
        {accept reflection}
        for j := 0 to NM_DIMS - 1 do sv[idx_worst,j] := reflect[j];
        sf[idx_worst] := f_reflect;
      end;
    end
    else if f_reflect < sf[idx_2nd_worst] then
    begin   {--- reflection is not best but better than second-worst: accept it ---}
      for j := 0 to NM_DIMS - 1 do sv[idx_worst,j] := reflect[j];
      sf[idx_worst] := f_reflect;
    end
    else
    begin {--- 3f: contraction ---}
      if f_reflect < sf[idx_worst] then
      begin
        {outside contraction}
        for j := 0 to NM_DIMS - 1 do
          contract[j] := centroid[j] + BETA * (reflect[j] - centroid[j]);
      end
      else
      begin {inside contraction}
        for j := 0 to NM_DIMS - 1 do
          contract[j] := centroid[j] + BETA * (sv[idx_worst,j] - centroid[j]);
      end;
      f_contract := vertex_error(contract);

      if f_contract < sf[idx_worst] then
      begin
        {accept contraction}
        for j := 0 to NM_DIMS - 1 do sv[idx_worst,j] := contract[j];
        sf[idx_worst] := f_contract;
      end
      else
      begin
        {--- 3g: shrink – pull all vertices toward the best ---}
        for i := 1 to NM_VERTS - 1 do
        begin
          for j := 0 to NM_DIMS - 1 do
            sv[i,j] := sv[idx_best,j] + SIGMA * (sv[i,j] - sv[idx_best,j]);
          sf[i] := vertex_error(sv[i]);
        end;
      end;
    end;

    inc(iteration_cycles);
  end; {Nelder-Mead loop}

  {------------------------------------------------------------------
    STEP 4 – return the best vertex found
  ------------------------------------------------------------------}
  {Find the vertex with the lowest error after the loop}
  idx_best := 0;
  for i := 1 to NM_VERTS - 1 do
    if sf[i] < sf[idx_best] then idx_best := i;

  p             := sv[idx_best, 0];
  a             := sv[idx_best, 1];
  b             := sv[idx_best, 2];
  lowest_error  := sf[idx_best];
end;


end.

