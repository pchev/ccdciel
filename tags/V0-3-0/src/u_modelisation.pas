unit u_modelisation;

{$mode objfpc}{$H+}

{
Copyright (C) 2003 Philippe Martinole
Copyright (C) 2015 Patrick Chevalley

http://www.teleauto.org/
philippe.martinole@teleauto.org

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
{
 simplified to use only 16bit data
 2015 Patrick Chevalley pch@ap-i.net
}

interface

uses Classes, SysUtils;

const
  MaxArray       = 100; // Dimension maxi pour les matrices de calcul
  // Modélisation d'étoile
  TGauss         = 1;
  TGaussEllipse  = 2;
  TMoffat        = 3;
  TMoffatEllipse = 4;

  LowPrecision   = 1;
  MeanPrecision  = 2;
  HighPrecision  = 3;

  LowSelect   = 1;
  MeanSelect  = 2;
  HighSelect  = 3;

type

  DoubleArray=array [1..MaxArray,1..MaxArray] of Double;
  DoubleArrayRow=array [1..MaxArray] of Double;
  IntegerArrayRow=array [1..MaxArray] of Integer;
  TLigDouble=array[1..999999] of Double;
  PLigDouble=^TLigDouble;

  TPSF=record
    X,Y,DX,DY,Sigma,SigmaX,SigmaY,Angle,Alpha,Delta,DAlpha,DDelta:Double;
    IntensiteMax,Flux,DFlux,Mag,aMoffat,bMoffat:Double;
    Catalogue:Integer;
    end;

  ErrorMath         = class(exception);
  ErrorModelisation = class(exception);

  Tiw16 = array of array of word;
  TPiw16 = ^Tiw16;

procedure ModeliseEtoile(Img:TPiw16; Larg:integer; TypeModele,Precision,Select:Byte;Degre:Byte;var PSF:TPSF);
function  mrqmin(var Img:TPiw16; Larg:integer;
                 var a:DoubleArrayRow;
                 ma:Integer;
                 var covar,alpha:DoubleArray;
                 var chisq,alamda:Double;
                 TypeModele:Byte;
                 Degre:Byte;
                 Precision:Byte):Boolean;

implementation

/////////////////////////////////////////////////
//   fonctions copiée de u_math
/////////////////////////////////////////////////

function PuissanceDouble(Base,Exposant:Double):Double;
begin
   if Exposant=0 then Result:=1
   else
      begin
      if Base<0 then raise ErrorMath.Create('Base négative');
      Result:=Exp(Exposant*Ln(Base))
      end
end;

function Gaussj(var a:DoubleArray; n:Integer; var b:DoubleArray; m:Integer):Boolean;
var
big,dum,pivinv:Double;
i,icol,irow,j,k,l,ll:Integer;
indxc,indxr,ipiv:IntegerArrayRow;
begin
Result:=True;
for j:=1 to n do ipiv[j]:=0;
for i:=1 to n do
   begin
   big:=0;
   for j:=1 to n do
      if ipiv[j]<>1 then
         for k:=1 to n do
            if ipiv[k]=0 then
               if abs(a[j,k])>=big then
                  begin
                  big:=abs(a[j,k]);
                  irow:=j;
                  icol:=k;
                  end
               else if ipiv[k]>1 then raise ErrorMath.Create('Matrice singulière');
   ipiv[icol]:=ipiv[icol]+1;
   if irow<>icol then
      begin
      for l:=1 to n do
         begin
         dum:=a[irow,l];
         a[irow,l]:=a[icol,l];
         a[icol,l]:=dum;
         end;
      for l:=1 to m do
         begin
         dum:=b[irow,l];
         b[irow,l]:=b[icol,l];
         b[icol,l]:=dum;
         end;
      end;
   indxr[i]:=irow;
   indxc[i]:=icol;
   if a[icol,icol]<1e-18 then begin Result:=False; Exit; end;
   pivinv:=1/a[icol,icol];
   a[icol,icol]:=1;
   for l:=1 to n do a[icol,l]:=a[icol,l]*pivinv;
   for l:=1 to m do b[icol,l]:=b[icol,l]*pivinv;
   for ll:=1 to n do
      if ll<>icol then
         begin
         dum:=a[ll,icol];
         a[ll,icol]:=0;
         for l:=1 to n do a[ll,l]:=a[ll,l]-a[icol,l]*dum;
         for l:=1 to m do b[ll,l]:=b[ll,l]-b[icol,l]*dum;
         end;
   end;
for l:=n downto 1 do
   if indxr[l]<>indxc[l] then
      for k:=1 to n do
         begin
         dum:=a[k,indxr[l]];
         a[k,indxr[l]]:=a[k,indxc[l]];
         a[k,indxc[l]]:=dum;
         end;
Result:=True;
end;

/////////////////////////////////////////////////

function  mrqmin(var Img:TPiw16;  Larg:integer;
                 var a:DoubleArrayRow;
                 ma:Integer;
                 var covar,alpha:DoubleArray;
                 var chisq,alamda:Double;
                 TypeModele:Byte;
                 Degre:Byte;
                 Precision:Byte):Boolean;
var
k,j:Integer;
atry:DoubleArrayRow;
oneda:DoubleArray;
MrqminBeta:DoubleArrayRow;
Mrqmin0chisq:Double;
da:DoubleArrayRow;
CoefAlamda:Double;
label fin;

function mrqcof(var Img:TPiw16;
                var a:DoubleArrayRow;
                var alpha:DoubleArray;
                var beta:DoubleArrayRow;
                var chisq:Double;
                TypeModele:Byte;
                Degre:Byte):Boolean;
var
i,j,k,l:integer;
zmod,wt,dz:Double;
dyda:DoubleArrayRow;

Tmp,Val:Double;
CosAngle,SinAngle,SigmaX2,SigmaY2:Double;
begin
Result:=True;
for j:=1 to ma do
   begin
   for k:=1 to j do alpha[j,k]:=0;
   beta[j]:=0;
   end;
chisq:=0;
case TypeModele of
   TMoffat:case Degre of
             0:begin
               for i:=0 to Larg-1 do
                  for l:=0 to Larg-1 do
                          begin
                          // A[1] fond du ciel
                          // A[2] intensite
                          // A[3] x
                          // A[4] y
                          // A[5] Coeff a de Moffat
                          // A[6] Coeff b de Moffat
                          zmod:=0;
                          Tmp:=1+((Sqr(i-A[3])+Sqr(l-A[4]))/Sqr(A[5]));
                          Val:=1/PuissanceDouble(Tmp,A[6]);
                          zmod:=zmod+a[1]+a[2]*Val;
                          dyda[1]:=1;
                          dyda[2]:=Val;
                          dyda[3]:=A[2]*Val*2*A[6]*(i-A[3])/(Sqr(A[5])*Tmp);
                          dyda[4]:=A[2]*Val*2*A[6]*(l-A[4])/(Sqr(A[5])*Tmp);
                          dyda[5]:=A[2]*Val*A[6]*((Sqr(i-A[3])+Sqr(l-A[4]))/(A[5]*A[5]*A[5]*Val));
                          dyda[6]:=-A[2]*Val*ln(Tmp);
                          dz:=Img^[i,l]-zmod;
                          for j:=1 to ma do
                             begin
                             wt:=dyda[j];
                             for k:=1 to j do
                                alpha[j,k]:=alpha[j,k]+wt*dyda[k];
                             beta[j]:=beta[j]+dz*wt;
                             end;
                          chisq:=chisq+dz*dz;
                          end;
                end;
              1:begin
               for i:=0 to Larg-1 do
                  for l:=0 to Larg-1 do
                          begin
                          // A[1] fond du ciel
                          // A[2] intensite
                          // A[3] x
                          // A[4] y
                          // A[5] Coeff a de Moffat
                          // A[6] Coeff b de Moffat
                          // A[7] coef x du polynome
                          // A[8] coef y du polynome
                          zmod:=0;
                          Tmp:=1+((Sqr(i-A[3])+Sqr(l-A[4]))/Sqr(A[5]));
                          Val:=1/PuissanceDouble(Tmp,A[6]);
                          zmod:=zmod+a[1]+a[7]*i+a[8]*l+a[2]*Val;
                          dyda[1]:=1;
                          dyda[2]:=Val;
                          dyda[3]:=A[2]*Val*2*A[6]*(i-A[3])/(Sqr(A[5])*Tmp);
                          dyda[4]:=A[2]*Val*2*A[6]*(l-A[4])/(Sqr(A[5])*Tmp);
                          dyda[5]:=A[2]*Val*A[6]*((Sqr(i-A[3])+Sqr(l-A[4]))/(A[5]*A[5]*A[5]*Val));
                          dyda[6]:=-A[2]*Val*ln(Tmp);
                          dyda[7]:=i;
                          dyda[8]:=l;
                          dz:=Img^[i,l]-zmod;
                          for j:=1 to ma do
                             begin
                             wt:=dyda[j];
                             for k:=1 to j do
                                alpha[j,k]:=alpha[j,k]+wt*dyda[k];
                             beta[j]:=beta[j]+dz*wt;
                             end;
                          chisq:=chisq+dz*dz;
                          end;
                end;
             2:begin
               for i:=0 to Larg-1 do
                  for l:=0 to Larg-1 do
                          begin
                          // A[1] fond du ciel
                          // A[2] intensite
                          // A[3] x
                          // A[4] y
                          // A[5] Coeff a de Moffat
                          // A[6] Coeff b de Moffat
                          // A[7] coef x du polynome
                          // A[8] coef y du polynome
                          // A[9] coef x2 du polynome
                          // A[10] coef xy du polynome
                          // A[11] coef y2 du polynome
                          zmod:=0;
                          Tmp:=1+((Sqr(i-A[3])+Sqr(l-A[4]))/Sqr(A[5]));
                          Val:=1/PuissanceDouble(Tmp,A[6]);
                          zmod:=zmod+a[1]+a[7]*i+a[8]*l+a[9]*i*i+a[10]*i*l+a[11]*l*l+a[2]*Val;
                          dyda[1]:=1;
                          dyda[2]:=Val;
                          dyda[3]:=A[2]*Val*2*A[6]*(i-A[3])/(Sqr(A[5])*Tmp);
                          dyda[4]:=A[2]*Val*2*A[6]*(l-A[4])/(Sqr(A[5])*Tmp);
                          dyda[5]:=A[2]*Val*A[6]*((Sqr(i-A[3])+Sqr(l-A[4]))/(A[5]*A[5]*A[5]*Val));
                          dyda[6]:=-A[2]*Val*ln(Tmp);
                          dyda[7]:=i;
                          dyda[8]:=l;
                          dyda[9]:=sqr(i);
                          dyda[10]:=i*l;
                          dyda[11]:=sqr(l);
                          dz:=Img^[i,l]-zmod;
                          for j:=1 to ma do
                             begin
                             wt:=dyda[j];
                             for k:=1 to j do
                                alpha[j,k]:=alpha[j,k]+wt*dyda[k];
                             beta[j]:=beta[j]+dz*wt;
                             end;
                          chisq:=chisq+dz*dz;
                          end;
               end;
             end;
   TGauss:case Degre of
             0:begin
               for i:=0 to Larg-1 do
                  for l:=0 to Larg-1 do
                          begin
                          // A[1] fond du ciel
                          // A[2] intensite
                          // A[3] x
                          // A[4] y
                          // A[5] sigma
                          zmod:=0;
                          SigmaX2:=Sqr(a[5]);
                          Tmp:=exp(-sqr((i-a[3])/a[5])/2)*exp(-sqr((l-a[4])/a[5])/2);
                          zmod:=zmod+a[1]+a[2]*Tmp;
                          dyda[1]:=1;
                          dyda[2]:=Tmp;
                          dyda[3]:=Tmp/SigmaX2*(i-a[3])*a[2];
                          dyda[4]:=Tmp/SigmaX2*(l-a[4])*a[2];
                          dyda[5]:=Tmp/(SigmaX2*a[5])*(sqr(i-a[3])+sqr(l-a[4]))*a[2];
                          dz:=Img^[i,l]-zmod;
                          for j:=1 to ma do
                             begin
                             wt:=dyda[j];
                             for k:=1 to j do
                                alpha[j,k]:=alpha[j,k]+wt*dyda[k];
                             beta[j]:=beta[j]+dz*wt;
                             end;
                          chisq:=chisq+dz*dz;
                          end;
               end;
             1:begin
               for i:=0 to Larg-1 do
                  for l:=0 to Larg-1 do
                          begin
                          // A[1] fond du ciel
                          // A[2] intensite
                          // A[3] x
                          // A[4] y
                          // A[5] sigma
                          // A[6] coef x du polynome
                          // A[7] coef y du polynome
                          zmod:=0;
                          SigmaX2:=Sqr(a[5]);
                          Tmp:=exp(-sqr((i-a[3])/a[5])/2)*exp(-sqr((l-a[4])/a[5])/2);
                          zmod:=zmod+a[1]+a[6]*i+a[7]*l+a[2]*Tmp;
                          dyda[1]:=1;
                          dyda[2]:=Tmp;
                          dyda[3]:=Tmp/SigmaX2*(i-a[3])*a[2];
                          dyda[4]:=Tmp/SigmaX2*(l-a[4])*a[2];
                          dyda[5]:=Tmp/(SigmaX2*a[5])*(sqr(i-a[3])+sqr(l-a[4]))*a[2];
                          dyda[6]:=i;
                          dyda[7]:=l;
                          dz:=Img^[i,l]-zmod;
                          for j:=1 to ma do
                             begin
                             wt:=dyda[j];
                             for k:=1 to j do
                                alpha[j,k]:=alpha[j,k]+wt*dyda[k];
                             beta[j]:=beta[j]+dz*wt;
                             end;
                          chisq:=chisq+dz*dz;
                          end;
               end;
             2:begin
               for i:=0 to Larg-1 do
                  for l:=0 to Larg-1 do
                          begin
                          // A[1] fond du ciel
                          // A[2] intensite
                          // A[3] x
                          // A[4] y
                          // A[5] sigma
                          // A[6] coef x du polynome
                          // A[7] coef y du polynome
                          // A[8] coef x2 du polynome
                          // A[9] coef xy du polynome
                          // A[10] coef y2 du polynome
                          zmod:=0;
                          SigmaX2:=Sqr(a[5]);
                          Tmp:=exp(-sqr((l-a[4])/a[5])/2)*exp(-sqr((i-a[3])/a[5])/2);
                          zmod:=zmod+a[1]+a[6]*i+a[7]*l+a[8]*i*i+a[9]*i*l+a[10]*l*l+a[2]*Tmp;
                          dyda[1]:=1;
                          dyda[2]:=Tmp;
                          dyda[3]:=(i-a[3])*a[2]*Tmp/SigmaX2;
                          dyda[4]:=(l-a[4])*a[2]*Tmp/SigmaX2;
                          dyda[5]:=(sqr(i-a[3])+sqr(l-a[4]))*a[2]*Tmp/(SigmaX2*a[5]);
                          dyda[6]:=i;
                          dyda[7]:=l;
                          dyda[8]:=sqr(i);
                          dyda[9]:=i*l;
                          dyda[10]:=sqr(l);
                          dz:=Img^[i,l]-zmod;
                          for j:=1 to ma do
                             begin
                             wt:=dyda[j];
                             for k:=1 to j do
                                alpha[j,k]:=alpha[j,k]+wt*dyda[k];
                             beta[j]:=beta[j]+dz*wt;
                             end;
                          chisq:=chisq+dz*dz;
                          end;
               end;
          end;
   TGaussEllipse:case Degre of
             0:begin
               for i:=0 to Larg-1 do
                  for l:=0 to Larg-1 do
                          begin
                          // A[1] fond du ciel
                          // A[2] intensite
                          // A[3] x
                          // A[4] y
                          // A[5] sigma x
                          // A[6] sigma y
                          // A[7] theta
                          zmod:=0;
                          CosAngle:=Cos(a[7]);
                          SinAngle:=Sin(a[7]);
                          SigmaX2:=Sqr(a[5]);
                          SigmaY2:=Sqr(a[6]);
                          Tmp:=exp(-sqr(-(i-a[3])*SinAngle+(l-a[4])*CosAngle)/(2*SigmaY2))*
                               exp(-sqr((i-a[3])*CosAngle+(l-a[4])*SinAngle)/(2*SigmaX2));
                          zmod:=zmod+a[1]+a[2]*Tmp;
                          dyda[1]:=1;
                          dyda[2]:=Tmp;
                          dyda[3]:=(-CosAngle*(-(i-a[3])*CosAngle-(l-a[4])*SinAngle)/SigmaX2
                                   +SinAngle*((i-a[3])*SinAngle-(l-a[4])*CosAngle)/SigmaY2)*a[2]*Tmp;
                          dyda[4]:=(-SinAngle*(-(i-a[3])*CosAngle-(l-a[4])*SinAngle)/SigmaX2
                                   -CosAngle*((i-a[3])*SinAngle-(l-a[4])*CosAngle)/SigmaY2)*a[2]*Tmp;
                          dyda[5]:=(sqr((i-a[3])*CosAngle+(l-a[4])*SinAngle)/(SigmaX2*a[5]))*a[2]*Tmp;
                          dyda[6]:=(sqr(-(i-a[3])*SinAngle+(l-a[4])*CosAngle)/(SigmaY2*a[6]))*a[2]*Tmp;
                          dyda[7]:=(-((i-a[3])*CosAngle+(l-a[4])*SinAngle)*(-(i-a[3])*SinAngle+(l-a[4])*CosAngle)/SigmaX2
                                    -(-(i-a[3])*SinAngle+(l-a[4])*CosAngle)*(-(i-a[3])*CosAngle-(l-a[4])*SinAngle)/SigmaY2)*a[2]*Tmp;

                          dz:=Img^[i,l]-zmod;
                          for j:=1 to ma do
                             begin
                             wt:=dyda[j];
                             for k:=1 to j do
                                alpha[j,k]:=alpha[j,k]+wt*dyda[k];
                             beta[j]:=beta[j]+dz*wt;
                             end;
                          chisq:=chisq+dz*dz;
                          end;
               end;
             1:begin
               for i:=0 to Larg-1 do
                  for l:=0 to Larg-1 do
                          begin
                          // A[1] fond du ciel
                          // A[2] intensite max
                          // A[3] x
                          // A[4] y
                          // A[5] sigma x
                          // A[6] sigma y
                          // A[7] theta
                          // A[8] coef x du polynome
                          // A[9] coef y du polynome
                          zmod:=0;
                          CosAngle:=Cos(a[7]);
                          SinAngle:=Sin(a[7]);
                          SigmaX2:=Sqr(a[5]);
                          SigmaY2:=Sqr(a[6]);
                          Tmp:=exp(-sqr(-(i-a[3])*SinAngle+(l-a[4])*CosAngle)/(2*SigmaY2))*
                               exp(-sqr((i-a[3])*CosAngle+(l-a[4])*SinAngle)/(2*SigmaX2));

                          zmod:=zmod+a[1]+a[8]*i+a[9]*l+a[2]*Tmp;
                          dyda[1]:=1;
                          dyda[2]:=Tmp;
                          dyda[3]:=(-CosAngle*(-(i-a[3])*CosAngle-(l-a[4])*SinAngle)/SigmaX2
                                    +SinAngle*((i-a[3])*SinAngle-(l-a[4])*CosAngle)/SigmaY2)*a[2]*Tmp;
                          dyda[4]:=(-SinAngle*(-(i-a[3])*CosAngle-(l-a[4])*SinAngle)/SigmaX2
                                    -CosAngle*((i-a[3])*SinAngle-(l-a[4])*CosAngle)/SigmaY2)*a[2]*Tmp;
                          dyda[5]:=(sqr((i-a[3])*CosAngle+(l-a[4])*SinAngle)/(SigmaX2*a[5]))*a[2]*Tmp;
                          dyda[6]:=(sqr(-(i-a[3])*SinAngle+(l-a[4])*CosAngle)/(SigmaY2*a[6]))*a[2]*Tmp;
                          dyda[7]:=(-( (i-a[3])*CosAngle+(l-a[4])*SinAngle)*(-(i-a[3])*SinAngle+(l-a[4])*CosAngle)/SigmaX2
                                    -(-(i-a[3])*SinAngle+(l-a[4])*CosAngle)*(-(i-a[3])*CosAngle-(l-a[4])*SinAngle)/SigmaY2)*a[2]*Tmp;
                          dyda[8]:=i;
                          dyda[9]:=l;

                          dz:=Img^[i,l]-zmod;
                          for j:=1 to ma do
                             begin
                             wt:=dyda[j];
                             for k:=1 to j do
                                alpha[j,k]:=alpha[j,k]+wt*dyda[k];
                             beta[j]:=beta[j]+dz*wt;
                             end;
                          chisq:=chisq+dz*dz;
                          end;
               end;
             2:begin
               for i:=0 to Larg-1 do
                  for l:=0 to Larg-1 do
                          begin
                          // A[1] fond du ciel
                          // A[2] intensite
                          // A[3] x
                          // A[4] y
                          // A[5] sigma x
                          // A[6] sigma y
                          // A[7] theta
                          // A[8] coef x du polynome
                          // A[9] coef y du polynome
                          // A[10] coef x2 du polynome
                          // A[11] coef xy du polynome
                          // A[12] coef y2 du polynome
                          zmod:=0;
                          CosAngle:=Cos(a[7]);
                          SinAngle:=Sin(a[7]);
                          SigmaX2:=Sqr(a[5]);
                          SigmaY2:=Sqr(a[6]);
                          Tmp:=exp(-sqr(-(i-a[3])*SinAngle+(l-a[4])*CosAngle)/(2*SigmaY2))*
                               exp(-sqr((i-a[3])*CosAngle+(l-a[4])*SinAngle)/(2*SigmaX2));

                          zmod:=zmod+a[1]+a[8]*i+a[9]*l+a[10]*i*i+a[11]*i*l+a[12]*l*l+a[2]*Tmp;
                          dyda[1]:=1;
                          dyda[2]:=Tmp;
                          dyda[3]:=(-CosAngle*(-(i-a[3])*CosAngle-(l-a[4])*SinAngle)/SigmaX2
                                    +SinAngle*((i-a[3])*SinAngle-(l-a[4])*CosAngle)/SigmaY2)*a[2]*Tmp;
                          dyda[4]:=(-SinAngle*(-(i-a[3])*CosAngle-(l-a[4])*SinAngle)/SigmaX2
                                    -CosAngle*((i-a[3])*SinAngle-(l-a[4])*CosAngle)/SigmaY2)*a[2]*Tmp;
                          dyda[5]:=(sqr((i-a[3])*CosAngle+(l-a[4])*SinAngle)/(SigmaX2*a[5]))*a[2]*Tmp;
                          dyda[6]:=(sqr(-(i-a[3])*SinAngle+(l-a[4])*CosAngle)/(SigmaY2*a[6]))*a[2]*Tmp;
                          dyda[7]:=(-((i-a[3])*CosAngle+(l-a[4])*SinAngle)*(-(i-a[3])*SinAngle+(l-a[4])*CosAngle)/SigmaX2
                                    -(-(i-a[3])*SinAngle+(l-a[4])*CosAngle)*(-(i-a[3])*CosAngle-(l-a[4])*SinAngle)/SigmaY2)*a[2]*Tmp;
                          dyda[8]:=i;
                          dyda[9]:=l;
                          dyda[10]:=sqr(i);
                          dyda[11]:=i*l;
                          dyda[12]:=sqr(l);

                          dz:=Img^[i,l]-zmod;
                          for j:=1 to ma do
                             begin
                             wt:=dyda[j];
                             for k:=1 to j do
                                alpha[j,k]:=alpha[j,k]+wt*dyda[k];
                             beta[j]:=beta[j]+dz*wt;
                             end;
                          chisq:=chisq+dz*dz;
                          end;
               end;
             end;
   end;
for j:=2 to ma do // Fill in the symmetric side
   for k:=1 to j-1 do alpha[k,j]:=alpha[j,k];
end;

begin
Result:=True;

Case Precision of
   LowPrecision: CoefAlamda:=5;
   MeanPrecision:CoefAlamda:=2;
   HighPrecision:CoefAlamda:=1;
   end;

if alamda<0 then
   begin
   alamda:=1;
   if not mrqcof(Img,a,alpha,MrqminBeta,chisq,TypeModele,Degre) then
      begin
      Result:=False;
      Exit;
      end;
   Mrqmin0chisq:=chisq;
   for j:=1 to ma do atry[j]:=a[j];
   end;

for j:=1 to ma do
   begin
   for k:=1 to ma do covar[j,k]:=alpha[j,k];
   covar[j,j]:=alpha[j,j]*(1+alamda);
   oneda[j,1]:=MrqminBeta[j];
   end;

if not gaussj(covar,ma,oneda,1) then
   begin
   Result:=False;
   Exit;
   end;

for j:=1 to ma do da[j]:=oneda[j,1];
if alamda=0 then goto fin;

for j:=1 to ma do atry[j]:=a[j]+da[j];

if not mrqcof(Img,atry,covar,da,chisq,TypeModele,Degre) then
   begin
   Result:=False;
   Exit;
   end;

if chisq<Mrqmin0chisq then
   begin
   alamda:=1/CoefAlamda*alamda;
   Mrqmin0chisq:=chisq;
   for j:=1 to ma do
      begin
      for k:=1 to ma do alpha[j,k]:=covar[j,k];
      MrqminBeta[j]:=da[j];
      a[j]:=atry[j];
      end;
   end
else
   begin
   alamda:=CoefAlamda*alamda;
   chisq:=Mrqmin0chisq;
   end;

fin:
end;

procedure ModeliseEtoile(Img:TPiw16; Larg:integer; TypeModele,Precision,Select:Byte;Degre:Byte;var PSF:TPSF);
var
x,y:Integer;
NoIteration,i,j,ma,MaxIter:Integer;
Covar,Alpha:DoubleArray;
Chisq,Alamda,MaxAlamda:Double;
a:DoubleArrayRow;
Fini:Boolean;
DFluxMax,FluxMax,DFluxMin,FluxMin,Temp:Double;
OK:Boolean;
ValMax,Valeur:Double;
NotInfini:Integer;
begin

PSF.X:=Larg div 2;
PSF.Y:=Larg div 2;

// On initialise x et y avec le maximum
ValMax:=0;
for j:=0 to Larg-1 do
   for i:=0 to Larg-1 do
           begin
           Valeur:=Img^[i,j];
           if Valeur>ValMax then
              begin
              ValMax:=Valeur;
              x:=i;
              y:=j;
              end;
           end;
if ValMax=0 then exit;

for i:=1 to MaxArray do
   for j:=1 to MaxArray do covar[i,j]:=0;
for i:=1 to MaxArray do
   for j:=1 to MaxArray do alpha[i,j]:=0;

case TypeModele of
   TMoffat:begin
          case Degre of
             0:begin
               ma:=6;
               // A[1] fond du ciel
               // A[2] intensite
               // A[3] x
               // A[4] y
               // A[5] Coeff a de Moffat
               // A[6] Coeff b de Moffat
                a[1]:=(Img^[0,0]+Img^[0,Larg-1]+Img^[Larg-1,0]+Img^[Larg-1,Larg-1])/4;
                a[2]:=Img^[x,y]-a[1];
                a[3]:=x;
                a[4]:=y;
                a[5]:=2;
                a[6]:=5;
               end;
             1:begin
               ma:=8;
               // A[1] fond du ciel
               // A[2] intensite
               // A[3] x
               // A[4] y
               // A[5] Coeff a de Moffat
               // A[6] Coeff b de Moffat
               // A[7] coef x du polynome
               // A[8] coef y du polynome
               a[1]:=(Img^[0,0]+Img^[0,Larg-1]+Img^[Larg-1,0]+Img^[Larg-1,Larg-1])/4;
               a[2]:=Img^[x,y]-a[1];
                a[3]:=x;
                a[4]:=y;
                a[5]:=2;
                a[6]:=5;
                a[7]:=0;
                a[8]:=0;
                end;
             2:begin
               ma:=11;
               // A[1] fond du ciel
               // A[2] intensite
               // A[3] x
               // A[4] y
               // A[5] Coeff a de Moffat
               // A[6] Coeff b de Moffat
               // A[7] coef x du polynome
               // A[8] coef y du polynome
               // A[9] coef x2 du polynome
               // A[10] coef xy du polynome
               // A[11] coef y2 du polynome
               a[1]:=(Img^[0,0]+Img^[0,Larg-1]+Img^[Larg-1,0]+Img^[Larg-1,Larg-1])/4;
               a[2]:=Img^[x,y]-a[1];
                a[3]:=x;
                a[4]:=y;
                a[5]:=2;
                a[6]:=5;
                a[7]:=0;
                a[8]:=0;
                a[9]:=0;
                a[10]:=0;
                a[11]:=0;
                end;
             end;
          end;
   TGauss:begin
          case Degre of
             0:begin
               // A[1] fond du ciel
               // A[2] intensite
               // A[3] x
               // A[4] y
               // A[5] sigma
               ma:=5;
               a[1]:=(Img^[0,0]+Img^[0,Larg-1]+Img^[Larg-1,0]+Img^[Larg-1,Larg-1])/4;
               a[2]:=Img^[x,y]-a[1];
                a[3]:=x;
                a[4]:=y;
                a[5]:=1;
               end;
             1:begin
               // A[1] fond du ciel
               // A[2] intensite
               // A[3] x
               // A[4] y
               // A[5] sigma
               // A[6] coef x du polynome
               // A[7] coef y du polynome
               ma:=7;
               a[1]:=(Img^[0,0]+Img^[0,Larg-1]+Img^[Larg-1,0]+Img^[Larg-1,Larg-1])/4;
               a[2]:=Img^[x,y]-a[1];
                a[3]:=x;
                a[4]:=y;
                a[5]:=1;
                a[6]:=0;
                a[7]:=0;
               end;
             2:begin
               // A[1] fond du ciel
               // A[2] intensite
               // A[3] x
               // A[4] y
               // A[5] sigma
               // A[6] coef x du polynome
               // A[7] coef y du polynome
               // A[8] coef x2 du polynome
               // A[9] coef xy du polynome
               // A[10] coef y2 du polynome
               ma:=10;
               a[1]:=(Img^[0,0]+Img^[0,Larg-1]+Img^[Larg-1,0]+Img^[Larg-1,Larg-1])/4;
               a[2]:=Img^[x,y]-a[1];
                a[3]:=x;
                a[4]:=y;
                a[5]:=1;
                a[6]:=0;
                a[7]:=0;
                a[8]:=0;
                a[9]:=0;
                a[10]:=0;
               end;
             end;
          end;
   TGaussEllipse:begin
                 case Degre of
                    0:begin
                      // A[1] fond du ciel
                      // A[2] intensite
                      // A[3] x
                      // A[4] y
                      // A[5] sigma x
                      // A[6] sigma y
                      // A[7] theta
                      ma:=7;
                      a[1]:=(Img^[0,0]+Img^[0,Larg-1]+Img^[Larg-1,0]+Img^[Larg-1,Larg-1])/4;
                      a[2]:=Img^[x,y]-a[1];
                       a[3]:=x; a[4]:=y;
                       a[5]:=1.5;
                       a[6]:=1.6;
                       a[7]:=0;
                      end;
                    1:begin
                      // A[1] fond du ciel
                      // A[2] intensite max
                      // A[3] x
                      // A[4] y
                      // A[5] sigma x
                      // A[6] sigma y
                      // A[7] theta
                      // A[8] coef x du polynome
                      // A[9] coef y du polynome
                      ma:=9;
                      a[1]:=(Img^[0,0]+Img^[0,Larg-1]+Img^[Larg-1,0]+Img^[Larg-1,Larg-1])/4;
                      a[2]:=Img^[x,y]-a[1];
                       a[3]:=x;
                       a[4]:=y;
                       a[5]:=1.5;
                       a[6]:=1.6;
                       a[7]:=0;
                       a[8]:=0;
                       a[9]:=0;
                      end;
                    2:begin
                      // A[1] fond du ciel
                      // A[2] intensite
                      // A[3] x
                      // A[4] y
                      // A[5] sigma x
                      // A[6] sigma y
                      // A[7] theta
                      // A[8] coef x du polynome
                      // A[9] coef y du polynome
                      // A[10] coef x2 du polynome
                      // A[11] coef xy du polynome
                      // A[12] coef y2 du polynome
                      ma:=12;
                      a[1]:=(Img^[0,0]+Img^[0,Larg-1]+Img^[Larg-1,0]+Img^[Larg-1,Larg-1])/4;
                      a[2]:=Img^[x,y]-a[1];
                       a[3]:=x;
                       a[4]:=y;
                       a[5]:=1.5;
                       a[6]:=1.6;
                       a[7]:=0;
                       a[8]:=0;
                       a[9]:=0;
                       a[10]:=0;
                       a[11]:=0;
                       a[12]:=0;
                  end
                    else raise ErrorModelisation.Create('Demande de modélisation impossible');
                    end
                 end;
   end;


Chisq:=1;
Alamda:=-1;
if mrqmin(Img,larg,a,ma,Covar,Alpha,Chisq,Alamda,TypeModele,Degre,Precision) then
   begin
   case Precision of
      LowPrecision: begin MaxIter:=5;  MaxAlamda:=1; end;
      MeanPrecision:begin MaxIter:=100; MaxAlamda:=1;    end;
      HighPrecision:begin MaxIter:=500; MaxAlamda:=100;  end;
      end;

   Fini:=False;
   NoIteration:=1;
   while not(Fini) and (NoIteration<=MaxIter) do
      begin
      if Alamda>MaxAlamda then Fini:=True;
      mrqmin(Img,larg,a,ma,Covar,Alpha,Chisq,Alamda,TypeModele,Degre,Precision);
      Inc(NoIteration);
      end;

   Alamda:=0;
   if mrqmin(Img,larg,a,ma,Covar,Alpha,Chisq,Alamda,TypeModele,Degre,Precision) then
      begin
      case TypeModele of
         TMoffat: begin
                  // A[1] fond du ciel
                  // A[2] intensite
                  // A[3] x
                  // A[4] y
                  // A[5] Coeff a de Moffat
                  // A[6] Coeff b de Moffat
                  PSF.X:=a[3];
                  PSF.DX:=Sqrt(Abs(Covar[3,3])*Chisq/Larg/Larg);
                  PSF.Y:=a[4];
                  PSF.DY:=Sqrt(Abs(Covar[4,4])*Chisq/Larg/Larg);
                  PSF.Sigma:=2*a[5]*sqrt(-1+(PuissanceDouble(2,1/(A[6]-1))));
                  PSF.Flux:=Pi*A[2]*Sqr(A[5])/(A[6]-1);
                  PSF.aMoffat:=A[5];
                  PSF.bMoffat:=A[6];

                  PSF.IntensiteMax:=a[2];
                  FluxMax:=Pi*(a[2]+Sqrt(Abs(Covar[2,2])*Chisq/Larg/Larg))
                     *sqr((a[5]+Sqrt(Abs(Covar[5,5])*Chisq/Larg/Larg)))/((a[6]-Sqrt(Abs(Covar[6,6])*Chisq/Larg/Larg))-1);
                  DFluxMax:=Abs(PSF.Flux-FluxMax);
                  FluxMin:=Pi*(a[2]-Sqrt(Abs(Covar[2,2])*Chisq/Larg/Larg))
                     *sqr((a[5]-Sqrt(Abs(Covar[5,5])*Chisq/Larg/Larg)))/((a[6]+Sqrt(Abs(Covar[6,6])*Chisq/Larg/Larg))-1);
                  DFluxMin:=Abs(PSF.Flux-FluxMin);
                  if DFluxMax>DFluxMin then PSF.DFlux:=DFluxMax else PSF.DFlux:=DFluxMin;
                  end;
         TGauss:begin
                // A[1] fond du ciel
                // A[2] intensite
                // A[3] x
                // A[4] y
                // A[5] sigma
                PSF.X:=a[3];
                PSF.DX:=Sqrt(Abs(Covar[3,3])*Chisq/Larg/Larg);
                PSF.Y:=a[4];
                PSF.DY:=Sqrt(Abs(Covar[4,4])*Chisq/Larg/Larg);
                PSF.Sigma:=a[5]*2*Sqrt(-2*ln(1/2));
                PSF.Flux:=a[2]*2*pi*sqr(a[5]);
                PSF.IntensiteMax:=a[2];
                DFluxMax:=Abs(PSF.Flux-(a[2]+Sqrt(Abs(Covar[2,2])*Chisq/Larg/Larg))
                   *2*pi*sqr((a[5]+Sqrt(Abs(Covar[5,5])*Chisq/Larg/Larg))));
                DFluxMin:=Abs(PSF.Flux-(a[2]-Sqrt(Abs(Covar[2,2])*Chisq/Larg/Larg))
                   *2*pi*sqr((a[5]-Sqrt(Abs(Covar[5,5])*Chisq/Larg/Larg))));
                if DFluxMax>DFluxMin then PSF.DFlux:=DFluxMax else PSF.DFlux:=DFluxMin;
                end;
         TGaussEllipse:
                  begin
                  // A[1] fond du ciel
                  // A[2] intensite
                  // A[3] x
                  // A[4] y
                  // A[5] sigma x
                  // A[6] sigma y
                  // A[7] theta
                  PSF.X:=a[3];
                  PSF.DX:=Sqrt(Abs(Covar[3,3])*Chisq/Larg/Larg);
                  PSF.Y:=a[4];
                  PSF.DY:=Sqrt(Abs(Covar[4,4])*Chisq/Larg/Larg);
                  PSF.SigmaX:=abs(a[5]*2*Sqrt(-2*ln(1/2)));
                  PSF.SigmaY:=abs(a[6]*2*Sqrt(-2*ln(1/2)));
                  PSF.Angle:=a[7];
                  if PSF.SigmaY>PSF.SigmaX then
                     begin
                     Temp:=PSF.SigmaY;
                     PSF.SigmaY:=PSF.SigmaX;
                     PSF.SigmaX:=Temp;
                     PSF.Angle:=PSF.Angle+pi/2;
                     end;
                  NotInfini:=1;
                  while (PSF.Angle<-pi/2) and (NotInfini<1000) do
                     begin
                     PSF.Angle:=PSF.Angle+pi;
                     Inc(NotInfini);
                     end;
                  NotInfini:=1;
                  while (PSF.Angle>pi/2) and (NotInfini<1000) do
                     begin
                     PSF.Angle:=PSF.Angle-pi;
                     Inc(NotInfini);
                     end;
                  PSF.IntensiteMax:=a[2];
                  PSF.Flux:=a[2]*2*pi*a[5]*a[6];
                  DFluxMax:=Abs(PSF.Flux-(a[2]+Sqrt(Abs(Covar[2,2])*Chisq/Larg/Larg))
                     *2*pi*(a[5]+Sqrt(Abs(Covar[5,5])*Chisq/Larg/Larg))*(a[6]+Sqrt(Abs(Covar[6,6])*Chisq/Larg/Larg)));
                  DFluxMin:=Abs(PSF.Flux-(a[2]-Sqrt(Abs(Covar[2,2])*Chisq/Larg/Larg))
                     *2*pi*(a[5]-Sqrt(Abs(Covar[5,5])*Chisq/Larg/Larg))*(a[6]-Sqrt(Abs(Covar[6,6])*Chisq/Larg/Larg)));
                  if DFluxMax>DFluxMin then PSF.DFlux:=DFluxMax else PSF.DFlux:=DFluxMin;
                  end;
            end;
      end
   else PSF.Flux:=-1;
   end
else PSF.Flux:=-1;

// Derniers tests

if PSF.Flux=0 then PSF.Flux:=-1;

OK:=True;
if (PSF.X<1) or (PSF.X>Larg)  then OK:=False;
if (PSF.Y<1) or (PSF.Y>Larg)  then OK:=False;
if PSF.DX>1 then OK:=False;
if PSF.DY>1 then OK:=False;

// On veut parfois avoir les modelisations imprecises meme en demandant
// une grande precision (ex : photometrie) donc test a faire après cette fonction !
if Select=HighSelect then
   begin
   if  PSF.dFlux*100/PSF.Flux>5 then OK:=False;
   end
else if Select=MeanSelect then
   begin
   if  PSF.dFlux*100/PSF.Flux>35 then OK:=False;
   end
else if  PSF.dFlux*100/PSF.Flux>75 then OK:=False;

case TypeModele of
   TGauss:if (PSF.Sigma<0.1) or (PSF.Sigma>Larg/2) then OK:=False;
   TGaussEllipse:if (PSF.SigmaX<0.1) or (PSF.SigmaY<0.1) then OK:=False;
   end;
if not OK then PSF.Flux:=-1;
end;


// Fonction de modelisation d'etoiles
// - Utiliser Precision=LowPrecision et TypeModele=TGauss et Degre=0
//   Pour plus de rapidite ( recalage d'images )
// - Utiliser Precision=MeanPrecision et TypeModele=TGaussEllipse et Degre=1
//   Pour plus de precision ( photometrie et astrometrie auto )
// - Utiliser Precision=HighPrecision et TypeModele=TGaussEllipse et Degre=1
//   pour des mesures tres precises mais ponctuelles
// - Augmenter NbSigmaDetect pour plus de rapidite
// - Diminuer NbSigmaDetect pour plus d'étoiles
// - DemiLarg est la demilargeur de la fenetre d'analyse
//   pour des etoiles 3<DemiLarg<15 generalement
//   on pourra faire des essais avec des valeurs supperieures pour des etoiles defocalisees

function mrqmin1D(var LigDouble:PLigDouble;
                  Max:Integer;
                  var a:DoubleArrayRow;
                  ma:Integer;
                  var covar,alpha:DoubleArray;
                  var chisq,alamda:Double):Boolean;
var
k,j:Integer;
atry:DoubleArrayRow;
oneda:DoubleArray;
MrqminBeta:DoubleArrayRow;
Mrqmin0chisq:Double;
da:DoubleArrayRow;
CoefAlamda:Double;
label fin;

function mrqcof(var LigDouble:PLigDouble;
                var a:DoubleArrayRow;
                var alpha:DoubleArray;
                var beta:DoubleArrayRow;
                var chisq:Double):Boolean;
var
j,k,l:integer;
zmod,wt,dz:Double;
dyda:DoubleArrayRow;

Tmp:Double;
SigmaX2:Double;
begin
Result:=True;
for j:=1 to ma do
   begin
   for k:=1 to j do alpha[j,k]:=0;
   beta[j]:=0;
   end;
chisq:=0;

for l:=1 to Max do
   begin
   // A[1] fond du ciel
   // A[2] intensite
   // A[3] x
   // A[4] sigma
   zmod:=0;
   SigmaX2:=Sqr(a[4]);
   Tmp:=exp(-sqr((l-a[3])/a[4])/2);
   zmod:=zmod+a[1]+a[2]*Tmp;
   dyda[1]:=1;
   dyda[2]:=Tmp;
   dyda[3]:=(l-a[3])*a[2]*Tmp/SigmaX2;
   dyda[4]:=sqr(l-a[3])*a[2]*Tmp/(SigmaX2*a[4]);
   dz:=LigDouble^[l]-zmod;
   for j:=1 to ma do
      begin
      wt:=dyda[j];
      for k:=1 to j do
         alpha[j,k]:=alpha[j,k]+wt*dyda[k];
      beta[j]:=beta[j]+dz*wt;
      end;
   chisq:=chisq+dz*dz;
   end;

for j:=2 to ma do
   for k:=1 to j-1 do alpha[k,j]:=alpha[j,k];
end;

begin
Result:=True;

CoefAlamda:=10;

if alamda<0 then
   begin
   alamda:=1;
   if not mrqcof(LigDouble,a,alpha,MrqminBeta,chisq) then
      begin
      Result:=False;
      Exit;
      end;
   Mrqmin0chisq:=chisq;
   for j:=1 to ma do atry[j]:=a[j];
   end;

for j:=1 to ma do
   begin
   for k:=1 to ma do covar[j,k]:=alpha[j,k];
   covar[j,j]:=alpha[j,j]*(1+alamda);
   oneda[j,1]:=MrqminBeta[j];
   end;

if not gaussj(covar,ma,oneda,1) then
   begin
   Result:=False;
   Exit;
   end;

for j:=1 to ma do da[j]:=oneda[j,1];
if alamda=0 then goto fin;

for j:=1 to ma do atry[j]:=a[j]+da[j];

if not mrqcof(LigDouble,atry,covar,da,chisq) then
   begin
   Result:=False;
   Exit;
   end;

if chisq<Mrqmin0chisq then
   begin
   alamda:=1/CoefAlamda*alamda;
   Mrqmin0chisq:=chisq;
   for j:=1 to ma do
      begin
      for k:=1 to ma do alpha[j,k]:=covar[j,k];
      MrqminBeta[j]:=da[j];
      a[j]:=atry[j];
      end;
   end
else
   begin
   alamda:=CoefAlamda*alamda;
   chisq:=Mrqmin0chisq;
   end;

fin:
end;

procedure Modelise1D(var LigDouble:PLigDouble;Max,x:Integer;var PSF:TPSF);
var
NoIteration,i,j,ma,MaxIter:Integer;
Covar,Alpha:DoubleArray;
Chisq,Alamda:Double;
a:DoubleArrayRow;
Fini:Boolean;
OK:Boolean;
ValMax,Valeur:Double;
begin
PSF.X:=x;

// On initialise x avec le maximum
ValMax:=0;
for i:=1 to Max do
   begin
   Valeur:=LigDouble^[i];
   if Valeur>ValMax then
      begin
      ValMax:=Valeur;
      x:=i;
      end;
   end;

for i:=1 to MaxArray do
   for j:=1 to MaxArray do covar[i,j]:=0;
for i:=1 to MaxArray do
   for j:=1 to MaxArray do alpha[i,j]:=0;

// A[1] fond du ciel
// A[2] intensite
// A[3] x
// A[4] sigma
ma:=4;
a[1]:=0;
a[2]:=LigDouble^[x];
a[3]:=x;
a[4]:=10;

Chisq:=1;
Alamda:=-1;
if mrqmin1D(LigDouble,Max,a,ma,Covar,Alpha,Chisq,Alamda) then
   begin
   MaxIter:=500;

   Fini:=False;
   NoIteration:=1;
   while not(Fini) and (NoIteration<=MaxIter) do
      begin
      if Alamda<1e-20 then Fini:=True;
      mrqmin1D(LigDouble,Max,a,ma,Covar,Alpha,Chisq,Alamda);
      Inc(NoIteration);
      end;

   Alamda:=0;
   if mrqmin1D(LigDouble,Max,a,ma,Covar,Alpha,Chisq,Alamda) then
      begin
      // A[1] fond du ciel
      // A[2] intensite
      // A[3] x
      // A[4] sigma
      PSF.X:=a[3];
      PSF.DX:=Sqrt(Abs(Covar[3,3])*Chisq/Max);
      PSF.Sigma:=a[4]*2*Sqrt(-2*ln(1/2));
      PSF.Flux:=a[2]*2*pi*sqr(a[4]);
      PSF.IntensiteMax:=a[2];
      end
   else PSF.Flux:=-1;
   end
else PSF.Flux:=-1;

// Derniers tests
OK:=True;
if (PSF.X<1) or (PSF.X>Max)  then OK:=False;
if PSF.DX>Max then OK:=False;
if  PSF.dFlux*100/PSF.Flux>75 then OK:=False;
if (PSF.Sigma<1) or (PSF.Sigma>Max/2) then OK:=False;

if not OK then PSF.Flux:=-1;
end;


end.
