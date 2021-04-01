unit pu_image_sharpness;
{Measurement of image sharpness for astronomical images of the Moon, Sun}
{Resulting value is used for autofocus of Moon and Sun.}
{The routine applies the Root mean Square on the differences between the minimum and maximum value of each 2x2x4(RGGB) pixel combination of the image.}
{The result is reversed and scaled such that the final result is roughly identical to the star HFD measurement.}

{Copyright (C) 2020 by Han Kleijn www.hnsky.org and Patrick Chevalley http://www.ap-i.net, pch@ap-i.net }

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

uses
  Classes, SysUtils;

type
  image_array = array of array of array of single;

function image_sharpness(img: image_array): double;{measure the sharpeness of an image. Result is reversed and scaled to be roughly identical to a HFD measurment. So value decreases with sharpness}

implementation


function image_sharpness(img: image_array): double;{measure the sharpeness of an image. Result is reversed and scaled to be roughly identical to a HFD measurment. So value decreases with sharpness}
var
  w,h, i,j:integer;
  maxA,maxB,minA,minB,v1,v2,minimum,maximum,average : double;
begin
//  nrcolor:=length(img);{nr colours}
  h:=length(img[0,0]);{length}
  w:=length(img[0]);{width}

  result:=0;
  average:=0;

 {for OSC and mono images}
  for i:=0 to (w-4) div 4 do  {step in width}
  for j:=0 to (h-4) div 4 do  {step 4 pixels in height}
  begin {process 16 pixels. Test 2x2x(R+G+G+B) pixels}
    v1:=(img[0,i*4   ,j*4]+
         img[0,i*4+1 ,j*4]+
         img[0,i*4   ,j*4+1]+
         img[0,i*4+1 ,j*4+1]);{Sum of R+G+G+B}
    v2:=(img[0,i*4+2  ,j*4]+
         img[0,i*4+1+2,j*4]+
         img[0,i*4+2  ,j*4+1]+
         img[0,i*4+1+2,j*4+1]);{Sum of R+G+G+B}

    if v1>v2 then begin maxA:=v1; minA:=v2; end else begin maxA:=v2; minA:=v1; end;{find the minimum and maximum values of the two bottom (R+G+G+B) combinations}

    v1:=(img[0,i*4   ,j*4+2]+
         img[0,i*4+1 ,j*4+2]+
         img[0,i*4   ,j*4+1+2]+
         img[0,i*4+1 ,j*4+1+2]);{Sum of R+G+G+B}
    v2:=(img[0,i*4+2  ,j*4+2]+
         img[0,i*4+1+2,j*4+2]+
         img[0,i*4+2  ,j*4+1+2]+
         img[0,i*4+1+2,j*4+1+2]);{Sum of R+G+G+B}
    if v1>v2 then begin maxB:=v1; minB:=v2; end else begin maxB:=v2; minB:=v1; end; ;{find the minimum and maximum values of the two top (R+G+G+B)combinations}

    if minA<minB then minimum:=minA else minimum:=minB;
    if maxA>maxB then maximum:=maxA else maximum:=maxB;

    result:=result+sqr(maximum-minimum);{square the local slope in the 2x2x(R+G+G+B) test area and add to the total}
    average:=average+(maximum+minimum);
  end;

  result:=sqrt(result/(w*h));{slope value, highest value is the sharpest image}
  average:=average/(w*h);{calculate average pixel value}
  result:=4*average/result;{turn the curve upside down and scale simular as HFD values. A lower value indicates a sharper image}
end;


end.

