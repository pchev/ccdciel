unit pu_hyperbola; {Hyperbola modeling of telescope focuser. Star disk size as function of focuser position}

{
Copyright (C) 2018 Patrick Chevalley & Han Kleijn

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
  math, u_global;

procedure find_best_hyperbola_fit(data: array of TDouble2;data_length:integer;var p,a,b: double); {input data[1,n]=position,data[2,n]=hfd, output: bestfocusposition=p, a, b of hyperbola}
function hfd_calc(position,perfectfocusposition,a,b:double) :double; {calculate HFD from position and perfectfocusposition using hyperbola parameters}
function steps_to_focus(hfd,a,b:double) :double; {calculates focuser steps to perfect focus from HFD and hyperbola parameters}

var
 p_hyp,a_hyp,b_hyp: double;
 v_curve:array of TDouble2;

 iteration_cycles :integer; {how many cycle where used for curve fitting}
 lowest_error : double; {sum of sqr % HFD error after curve fitting}

implementation

function hfd_calc(position,perfectfocusposition,a,b:double) :double; {calculate HFD from position and perfectfocusposition using hyperbola parameters}
{The HFD (half value diameter) of the imaged star disk as function of the focuser position can be described as hyperbola}
{a,b are the hyperbola parameters, a is the lowest HFD value at focus position, the asymptote y:= +-x*b/a}
{A hyperbola is defined as: }
{x=b*sinh(t)                }
{y=a*cosh(t)                }
{Using the arccosh and arsinh functions it is possible to inverse}
{above calculations and convert x=>t and t->y or y->t and t->x}
var
   x,t : double;
begin
   x:=perfectfocusposition - position;
   t:=arsinh(x/b);{calculate t-position in hyperbola}
   result:=a*cosh(t);{convert t-position to y/hfd value}
end;

function steps_to_focus(hfd,a,b:double) :double; {calculates focuser steps to perfect focus from HFD and hyperbola parameters}
{The HFD (half value diameter) of the imaged star disk as function of the focuser position can be described as hyperbola}
{a,b are the hyperbola parameters, a is the lowest HFD value at focus position, the asymptote y:= +-x*b/a}
{A hyperbola is defined as: }
{x=b*sinh(t)                }
{y=a*cosh(t)                }
{Using the arccosh and arsinh functions it is possible to inverse}
{above calculations and convert x=>t and t->y or y->t and t->x}

{Note using the HFD there are two solutions, either left or right side of the hyperbola}
var
   x,t : double;
begin
   t:=arcosh(hfd/a);{calculate t-position in hyperbola}
   x:=b*sinh(t);{convert t-position to x}
   result:=x;{steps to focus}
end;

function scaled_error_hyperbola(data: array of TDouble2;data_length:integer; perfectfocusposition,a,b: double): double;{calculates total squared error between V-curve array data and hyperbola}
var
 n, i: Integer;
 hdf, hfd_simulation, total_error : double;
begin
 // n:=Length(data);
  n:=data_length;

  total_error:=0;
  for i:=0 to n-1 do
  begin
     hfd_simulation:=hfd_calc(data[i,1],perfectfocusposition,a,b);{y or HFD error}
     total_error:=total_error+ sqr((hfd_simulation-data[i,2])/data[i,2]); {sqr the SCALED DIFFERENCE}
   end;
  result:=sqrt(total_error);
end;

procedure find_best_hyperbola_fit(data: array of TDouble2;data_length:integer;var p,a,b: double); {input data[1,n]=position,data[2,n]=hfd, output: bestfocusposition=p, a, b of hyperbola}
var
   i,n  :integer;
   error1, old_error, p_range,a_range, b_range, slope, highest_hfd, lowest_hfd,
   highest_position, lowest_position,a1,b1,p1,a0,b0,p0  :double;
begin
 lowest_error:=1E99;
// n:=Length(data);
 n:=data_length;

 highest_hfd:=0;
 lowest_hfd:=1E99;
 for i:=0 to n-1 do {find start values for hyperbola loop}
 begin
   if data[i,2]>highest_hfd then
   begin
     highest_hfd:=data[i,2];
     highest_position:=data[i,1];
   end;
   if data[i,2]<lowest_hfd then
   begin
     lowest_hfd:=data[i,2];
     lowest_position:=data[i,1];
   end;
 end;
 if  highest_position<lowest_position then  highest_position:=(lowest_position- highest_position)+lowest_position;{go up always}
 slope:=(highest_position- lowest_position)/(highest_hfd-lowest_hfd); {could contain an error if bottom part of curve}
 iteration_cycles:=0;

 {get good starting values}
 a:=lowest_hfd;{a is near the HFD value}
 b:=a*slope;{b/a=slope of the asymptote of hyperbola,y:= +-x*b/a. b/a:=delta_HFD/delta_steps}
 p:=lowest_position;

 {set starting test range}
 a_range:=a;
 b_range:=b;
 p_range:=(highest_position-lowest_position);{large steps since slope could contain some error}

 repeat
   p0:=p;
   b0:=b;
   a0:=a;

   a_range:=a_range*0.5;{reduce scan range by 50%}
   b_range:=b_range*0.5;
   p_range:=p_range*0.5;

   p1:=p0 - p_range;{start value}
   while p1<=p0 + p_range do {position loop}
   begin
     a1:=a0 - a_range;{start value}
     while a1<=a0 + a_range do {a loop}
     begin
       b1:=b0 - b_range;{start value}
       while b1 <= b0 + b_range do {b loop}
       begin
         error1:=scaled_error_hyperbola(data, data_length,p1,a1,b1);
         if error1<lowest_error  then
         begin{better position found}
           old_error:=lowest_error;
           lowest_error:=error1;
           a:=a1;{best value up to now}
           b:=b1;
           p:=p1;
         end;
         b1:=b1 + b_range*0.1;{do 20 steps within range, many steps guarantees convergence}
       end;{b loop}
       a1:=a1 + a_range*0.1;{do 20 steps within range}
     end;{a loop}
     p1:=p1 + p_range*0.1;{do 20 steps within range}
   end;{position loop}
   inc(iteration_cycles);
 until (  (old_error-lowest_error<0.0001) {lowest error almost reached}
       or (lowest_error<=0.0001)          {perfect result}
       or (iteration_cycles>=30));         {most likely convergence problem}
end;


end.

