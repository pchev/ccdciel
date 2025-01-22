unit pu_image_sharpness;
{Measurement of image sharpness for astronomical images of the Moon, Sun}
{Resulting value is used for autofocus of Moon and Sun.}
{The routine applies the Root mean Square on the differences between the minimum and maximum value of each 2x2x4(RGGB) pixel combination of the image.}
{The result is reversed and scaled such that the final result is roughly identical to the star HFD measurement.}

{Copyright (C) 2025 by Han Kleijn www.hnsky.org and Patrick Chevalley http://www.ap-i.net, pch@ap-i.net }

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
  Classes, SysUtils, math;

type
  image_array = array of array of array of single;

function image_sharpness(img: image_array): double;// Measure the sharpeness of an image by gradient. The result is reversed and scaled to be roughly identical to a HFD curve So value decreases with sharpness

implementation


function image_sharpness(img: image_array): Double;// Measure the sharpeness of an image by gradient. The result is reversed and scaled to be roughly identical to a HFD curve So value decreases with sharpness
Var
  x, y, Width, Height         : Integer;
  Sharpness, Center, Up, Right: double;

begin
  Height:=length(img[0]);
  Width:=length(img[0,0]);

  Sharpness := 0;
  for y := 2 to Height - 3 do // Loop over the image, excluding edges where step size of 2 cannot be applied
  begin
    for x := 2 to Width - 3 do
    begin
      Center := Img[0, y, x];// Center pixel value
      Up :=Img[0, y + 2, x];// Neighboring pixel values with step size of 2. So compare red sensitive with red sensitive, green sensitive with green sensitive.
      Right := Img[0, y, x + 2];
      Sharpness:=Sharpness+sqr(up-center)+sqr(right-center);
    end;//for loop
  end;//for loop

  if Sharpness>0 then
    result := 100*width*height/sqrt(Sharpness) //turn the curve upside down and scale simular as HFD values. A lower value indicates a sharper image
  else
    result:=9999;//dummy value for case sharpness is zero
end;

end.

