unit cu_fits;

{
Copyright (C) 2005-2015 Patrick Chevalley

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

{$mode delphi}{$H+}

interface

uses SysUtils, Classes, FileUtil, u_utils, u_global,
  LazUTF8, Graphics,Math, FPImage, Controls, LCLType, Forms,
  StdCtrls, ExtCtrls, Buttons, IntfGraphics;

type

 TFitsInfo = record
            valid, solved: boolean;
            bitpix,naxis,naxis1,naxis2,naxis3 : integer;
            Frx,Fry,Frwidth,Frheight: integer;
            bzero,bscale,dmax,dmin,blank : double;
            equinox,ra,dec,crval1,crval2: double;
            pixsz1,pixsz2,pixratio: double;
            objects,ctype1,ctype2 : string;
            end;

 THeaderBlock = array[1..36,1..80] of char;

 Timai8 = array of array of array of byte; TPimai8 = ^Timai8;
 Timai16 = array of array of array of smallint; TPimai16 = ^Timai16;
 Timaw16 = array of array of array of word; TPimaw16 = ^Timaw16;
 Timai32 = array of array of array of longint; TPimai32 = ^Timai32;
 Timar32 = array of array of array of single; TPimar32 = ^Timar32;
 Timar64 = array of array of array of double; TPimar64 = ^Timar64;

 Titt = (ittlinear,ittramp,ittlog,ittsqrt);

 THistogram = array[0..255] of integer;

 TFitsHeader = class(TObject)
    private
      FRows:   TStringList;
      FKeys:   TStringList;
      FValues: TStringList;
      FComments:TStringList;
      Fvalid : boolean;
    public
      constructor Create;
      destructor  Destroy; override;
      procedure ClearHeader;
      function ReadHeader(ff:TMemoryStream): integer;
      function GetStream: TMemoryStream;
      function Indexof(key: string): integer;
      function Valueof(key: string; out val: string): boolean; overload;
      function Valueof(key: string; out val: integer): boolean; overload;
      function Valueof(key: string; out val: double): boolean; overload;
      function Valueof(key: string; out val: boolean): boolean; overload;
      function Add(key,val,comment: string; quotedval:boolean=true): integer; overload;
      function Add(key:string; val:integer; comment: string): integer; overload;
      function Add(key:string; val:double; comment: string): integer; overload;
      function Add(key:string; val:boolean; comment: string): integer; overload;
      function Insert(idx: integer; key,val,comment: string; quotedval:boolean=true):integer; overload;
      function Insert(idx: integer; key:string; val:integer; comment: string):integer; overload;
      function Insert(idx: integer; key:string; val:double; comment: string):integer; overload;
      function Insert(idx: integer; key:string; val:boolean; comment: string):integer; overload;
      procedure Delete(idx: integer);
      property Rows:   TStringList read FRows;
      property Keys:   TStringList read FKeys;
      property Values: TStringList read FValues;
      property Comments:TStringList read FComments;
 end;

const    maxl = 20000;

type
  TFits = class(TComponent)
  private
    // Original Fits file
    FStream : TMemoryStream;
    // Fits read buffers
    d8  : array[1..2880] of byte;
    d16 : array[1..1440] of word;
    d32 : array[1..720] of Longword;
    d64 : array[1..360] of Int64;
    // Original image data
    imai8 : Timai8;
    imai16 : Timai16;
    imai32 : Timai32;
    imar32 : Timar32;
    imar64 : Timar64;
    // 16bit image scaled min/max unsigned
    Fimage : Timaw16;
    // Fimage scaling factor
    FimageC, FimageMin : double;
    // Histogram of Fimage
    FHistogram: THistogram;
    // Fits header
    FHeader: TFitsHeader;
    // same as Fimage in TLazIntfImage format
    FIntfImg: TLazIntfImage;
    // Fits header values
    FFitsInfo : TFitsInfo;
    //
    n_axis,cur_axis,Fwidth,Fheight,Fhdr_end,colormode : Integer;
    FTitle : string;
    Fmean,Fsigma,Fdmin,Fdmax : double;
    FImgDmin, FImgDmax: Word;
    FImgFullRange: Boolean;
    Fbpm: TBpm;
    FBPMcount,FBPMnx,FBPMny,FBPMnax: integer;
    Fitt : Titt;
    emptybmp:Tbitmap;
    clog,csqrt:double;
    f_ViewHeaders: TForm;
    m_ViewHeaders: TMemo;
    p_ViewHeaders: TPanel;
    b_ViewHeaders: TButton;
    Procedure ViewHeadersClose(Sender: TObject; var CloseAction:TCloseAction);
    Procedure ViewHeadersBtnClose(Sender: TObject);
    procedure SetStream(value:TMemoryStream);
    function GetStream: TMemoryStream;
    procedure SetVideoStream(value:TMemoryStream);
    Procedure ReadFitsImage;
    Procedure GetImage;
    procedure ApplyBPM(bpm:TBpm; count:integer);
    function Citt(value: Word):Word;
    procedure SetImgFullRange(value: boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
     invertx, inverty : boolean;
     constructor Create(AOwner:TComponent); override;
     destructor  Destroy; override;
     Procedure ViewHeaders;
     Procedure LoadStream;
     procedure GetIntfImg;
     procedure GetFitsInfo;
     procedure GetBitmap(var imabmp:Tbitmap);
     procedure SaveToFile(fn: string);
     procedure SetBPM(value: TBpm; count,nx,ny,nax:integer);
     property IntfImg: TLazIntfImage read FIntfImg;
     property Title : string read FTitle write FTitle;
     Property HeaderInfo : TFitsInfo read FFitsInfo;
     property Header: TFitsHeader read FHeader write FHeader;
     Property Stream : TMemoryStream read GetStream write SetStream;
     Property VideoStream : TMemoryStream write SetVideoStream;
     property Histogram : THistogram read FHistogram;
     property ImgDmin : Word read FImgDmin write FImgDmin;
     property ImgDmax : Word read FImgDmax write FImgDmax;
     property itt : Titt read Fitt write Fitt;
     property image : Timaw16 read Fimage;
     property imageC : double read FimageC;
     property imageMin : double read FimageMin;
     property imageMean: double read Fmean;
     property imageSigma: double read Fsigma;
     property ImgFullRange: Boolean read FImgFullRange write SetImgFullRange;
  end;

implementation

//////////////////// TFitsHeader /////////////////////////

constructor TFitsHeader.Create;
begin
  inherited Create;
  FRows:=TStringList.Create;
  FComments:=TStringList.Create;
  FValues:=TStringList.Create;
  FKeys:=TStringList.Create;
  Fvalid:=false;
end;

destructor  TFitsHeader.Destroy;
begin
  FRows.Free;
  FComments.Free;
  FValues.Free;
  FKeys.Free;
  inherited Destroy;
end;

procedure TFitsHeader.ClearHeader;
begin
  Fvalid:=false;
  FRows.Clear;
  FKeys.Clear;
  FValues.Clear;
  FComments.Clear;
end;

function TFitsHeader.ReadHeader(ff:TMemoryStream): integer;
var   header : THeaderBlock;
      i,p1,p2,n : integer;
      eoh : boolean;
      row,keyword,value,comment,buf : string;
      P: PChar;
begin
ClearHeader;
eoh:=false;
ff.Position:=0;
header[1,1]:=chr(0);
repeat
   n:=ff.Read(header,sizeof(THeaderBlock));
   if n<>sizeof(THeaderBlock) then
      Break;
   for i:=1 to 36 do begin
      row:=header[i];
      if trim(row)='' then continue;
      p1:=pos('=',row);
      if p1=0 then p1:=9;
      p2:=pos('/',row);
      keyword:=trim(copy(row,1,p1-1));
      if p2>0 then begin
         value:=trim(copy(row,p1+1,p2-p1-1));
         comment:=trim(copy(row,p2,99));
      end else begin
         value:=trim(copy(row,p1+1,99));
         comment:='';
      end;
      if (keyword='SIMPLE') then
         if (copy(value,1,1)='T') then begin
           Fvalid:=true;
         end
         else begin
           Fvalid:=false;
           Break;
         end;
      if (keyword='END') then begin
         eoh:=true;
      end;
      P:=PChar(value);
      buf:=AnsiExtractQuotedStr(P,'''');
      if buf<>'' then value:=buf;
      FRows.add(row);
      FKeys.add(keyword);
      FValues.add(value);
      FComments.add(comment);
   end;
   if not Fvalid then begin
     Break;
   end;
until eoh;
result:=ff.position;
end;

function TFitsHeader.GetStream: TMemoryStream;
var i,c:integer;
    buf: array[0..79] of char;
begin
  result:=TMemoryStream.Create;
  for i:=0 to FRows.Count-1 do begin
    buf:=FRows[i];
    result.Write(buf,80);
  end;
  if (FRows.Count mod 36)>0 then begin
    buf:=b80;
    c:=36 - (FRows.Count mod 36);
    for i:=1 to c do result.Write(buf,80);
  end;
end;

function TFitsHeader.Indexof(key: string): integer;
begin
  result:=FKeys.IndexOf(key);
end;

function TFitsHeader.Valueof(key: string; out val: string): boolean; overload;
var k: integer;
begin
  val:='';
  k:=FKeys.IndexOf(key);
  result:=(k>=0);
  if result then val:=FValues[k];
end;

function TFitsHeader.Valueof(key: string; out val: integer): boolean; overload;
var k: integer;
begin
  val:=0;
  k:=FKeys.IndexOf(key);
  result:=(k>=0);
  if result then val:=StrToIntDef(FValues[k],0);
end;

function TFitsHeader.Valueof(key: string; out val: double): boolean; overload;
var k: integer;
begin
  val:=0;
  k:=FKeys.IndexOf(key);
  result:=(k>=0);
  if result then val:=StrToFloatDef(FValues[k],0);
end;

function TFitsHeader.Valueof(key: string; out val: boolean): boolean; overload;
var k: integer;
begin
  val:=false;
  k:=FKeys.IndexOf(key);
  result:=(k>=0);
  if result then val:=(FValues[k]='T');
end;

function TFitsHeader.Add(key,val,comment: string; quotedval:boolean=true): integer;
begin
 result:=Insert(-1,key,val,comment,quotedval);
end;

function TFitsHeader.Add(key:string; val:integer; comment: string): integer;
begin
 result:=Insert(-1,key,val,comment);
end;

function TFitsHeader.Add(key:string; val:double; comment: string): integer;
begin
 result:=Insert(-1,key,val,comment);
end;

function TFitsHeader.Add(key:string; val:boolean; comment: string): integer;
begin
 result:=Insert(-1,key,val,comment);
end;

function TFitsHeader.Insert(idx: integer; key,val,comment: string; quotedval:boolean=true): integer;
var row: string;
begin
 // The END keyword
 if (trim(key)='END') then begin
   row:=copy('END'+b80,1,80);
   val:='';
   comment:='';
 end
 // Comments with keyword
 else if (trim(key)='COMMENT') then begin
   val:=val+comment;
   comment:='';
   row:=Format('%0:-8s',[key])+
        Format('  %0:-70s',[val]);
 end
 // Comment without keyword
 else if (trim(key)='') then begin
   val:=val+comment;
   comment:='';
   row:=Format('          %0:-70s',[val]);
 end
 // Quoted string
 else if quotedval then begin
    row:=Format('%0:-8s',[key])+
         Format('= %0:-20s',[QuotedStr(val)])+
         Format(' / %0:-47s',[comment]);
 end
 // Other unquoted values
 else begin
    row:=Format('%0:-8s',[key])+
         Format('= %0:-20s',[val])+
         Format(' / %0:-47s',[comment]);
 end;
 if idx>=0 then begin
    FRows.Insert(idx,row);
    FKeys.Insert(idx,key);
    FValues.Insert(idx,val);
    FComments.Insert(idx,comment);
    result:=idx;
 end else begin
    result:=FRows.Add(row);
    FKeys.Add(key);
    FValues.Add(val);
    FComments.Add(comment);
 end;
end;

function TFitsHeader.Insert(idx: integer; key:string; val:integer; comment: string):integer;
var txt: string;
begin
  txt:=Format('%20d',[val]);
  result:=Insert(idx,key,txt,comment,false);
end;

function TFitsHeader.Insert(idx: integer; key:string; val:double; comment: string):integer;
var txt: string;
begin
  txt:=Format('%20.10g',[val]);
  result:=Insert(idx,key,txt,comment,false);
end;

function TFitsHeader.Insert(idx: integer; key:string; val:boolean; comment: string):integer;
var txt,v: string;
begin
  if val then v:='T' else v:='F';
  txt:=Format('%0:20s',[v]);
  result:=Insert(idx,key,txt,comment,false);
  if (not Fvalid)and(key='SIMPLE')and(val) then Fvalid:=true;
end;

procedure TFitsHeader.Delete(idx: integer);
begin
  FRows.Delete(idx);
  FKeys.Delete(idx);
  FValues.Delete(idx);
  FComments.Delete(idx);
end;


//////////////////// TFits /////////////////////////

constructor TFits.Create(AOwner:TComponent);
begin
inherited Create(AOwner);
Fitt:=ittlinear;
Fheight:=0;
Fwidth:=0;
ImgDmin:=0;
FBPMcount:=0;
ImgDmax:=MaxWord;
FImgFullRange:=false;
FFitsInfo.naxis1:=0;
FHeader:=TFitsHeader.Create;
FStream:=TMemoryStream.Create;
FIntfImg:=TLazIntfImage.Create(0,0);
emptybmp:=Tbitmap.Create;
emptybmp.SetSize(1,1);
clog:=MaxWord/ln(MaxWord);
csqrt:=MaxWord/sqrt(MaxWord);
end;

destructor  TFits.Destroy; 
begin
try
setlength(imar64,0,0,0);
setlength(imar32,0,0,0);
setlength(imai8,0,0,0);
setlength(imai16,0,0,0);
setlength(imai32,0,0,0);
setlength(Fimage,0,0,0);
FHeader.Free;
FStream.Free;
FIntfImg.Free;
emptybmp.Free;
inherited destroy;
except
//writeln('error destroy '+name);
end;
end;

procedure TFits.SetVideoStream(value:TMemoryStream);
begin
// other header previously set by caller
FFitsInfo.solved:=false;
cur_axis:=1;
setlength(imar64,0,0,0);
setlength(imar32,0,0,0);
setlength(imai8,0,0,0);
setlength(imai16,0,0,0);
setlength(imai32,0,0,0);
setlength(Fimage,0,0,0);
FStream.Clear;
FStream.Position:=0;
value.Position:=0;
FStream.CopyFrom(value,value.Size);
Fhdr_end:=0;
ReadFitsImage;
GetIntfImg;
end;

procedure TFits.SetStream(value:TMemoryStream);
begin
try
 FFitsInfo.valid:=false;
 FFitsInfo.solved:=false;
 cur_axis:=1;
 setlength(imar64,0,0,0);
 setlength(imar32,0,0,0);
 setlength(imai8,0,0,0);
 setlength(imai16,0,0,0);
 setlength(imai32,0,0,0);
 setlength(Fimage,0,0,0);
 FStream.Clear;
 FStream.Position:=0;
 value.Position:=0;
 FStream.CopyFrom(value,value.Size);
 Fhdr_end:=FHeader.ReadHeader(FStream);
 GetFitsInfo;
except
 FFitsInfo.valid:=false;
end;
end;

Procedure TFits.LoadStream;
begin
  if FFitsInfo.valid then begin
    ReadFitsImage;
    GetIntfImg;
  end;
end;

function TFits.GetStream: TMemoryStream;
begin
  result:=FHeader.GetStream;
  FStream.Position:=Fhdr_end;
  result.CopyFrom(FStream,FStream.Size-Fhdr_end);
end;

procedure TFits.SaveToFile(fn: string);
var mem: TMemoryStream;
begin
  mem:=GetStream;
  mem.SaveToFile(fn);
  mem.Free;
end;

Procedure TFits.ViewHeaders;
begin
f_ViewHeaders:=TForm.create(self);
f_ViewHeaders.OnClose:=ViewHeadersClose;
m_ViewHeaders:=Tmemo.create(f_ViewHeaders);
p_ViewHeaders:=TPanel.Create(f_ViewHeaders);
b_ViewHeaders:=Tbutton.Create(f_ViewHeaders);
f_ViewHeaders.Width:=650;
f_ViewHeaders.Height:=450;
p_ViewHeaders.Parent:=f_ViewHeaders;
p_ViewHeaders.Caption:='';
p_ViewHeaders.Height:=b_ViewHeaders.Height+8;
p_ViewHeaders.Align:=alBottom;
m_ViewHeaders.Parent:=f_ViewHeaders;
m_ViewHeaders.Align:=alClient;
m_ViewHeaders.font.Name:='courier';
m_ViewHeaders.ReadOnly:=true;
m_ViewHeaders.WordWrap:=false;
m_ViewHeaders.ScrollBars:=ssAutoBoth;
b_ViewHeaders.Parent:=p_ViewHeaders;
b_ViewHeaders.Caption:='Close';
b_ViewHeaders.Top:=4;
b_ViewHeaders.Left:=40;
b_ViewHeaders.Cancel:=true;
b_ViewHeaders.Default:=true;
b_ViewHeaders.OnClick:=ViewHeadersBtnClose;
m_ViewHeaders.Lines:=FHeader.Rows;
FormPos(f_ViewHeaders,mouse.CursorPos.X,mouse.CursorPos.Y);
if trim(FTitle)='' then
   f_ViewHeaders.Caption:='FITS header'
else
   f_ViewHeaders.Caption:=SysToUTF8(FTitle);
f_ViewHeaders.Show;
end;

Procedure TFits.ViewHeadersBtnClose(Sender: TObject);
begin
f_ViewHeaders.Close;
end;

Procedure TFits.ViewHeadersClose(Sender: TObject; var CloseAction:TCloseAction);
begin
CloseAction:=caFree;
end;

procedure TFits.GetFitsInfo;
var   i : integer;
      keyword,buf : string;
begin
with FFitsInfo do begin
 valid:=false; solved:=false; naxis1:=0 ; naxis2:=0 ; naxis3:=1; bitpix:=0 ; dmin:=0 ; dmax := 65535; blank:=0;
 bzero:=0 ; bscale:=1; equinox:=2000; ra:=NullCoord; dec:=NullCoord; crval1:=NullCoord; crval2:=NullCoord;
 objects:=''; ctype1:=''; ctype2:=''; pixsz1:=0; pixsz2:=0; pixratio:=1; Frx:=-1;Fry:=-1;Frwidth:=0;Frheight:=0;
 for i:=0 to FHeader.Rows.Count-1 do begin
    keyword:=trim(FHeader.Keys[i]);
    buf:=trim(FHeader.Values[i]);
    if (keyword='SIMPLE') then if (copy(buf,1,1)<>'T')
       then begin valid:=false;Break;end
       else begin valid:=true;end;
    if (keyword='BITPIX') then bitpix:=strtoint(buf);
    if (keyword='NAXIS')  then naxis:=strtoint(buf);
    if (keyword='NAXIS1') then naxis1:=strtoint(buf);
    if (keyword='NAXIS2') then naxis2:=strtoint(buf);
    if (keyword='NAXIS3') then naxis3:=strtoint(buf);
    if (keyword='BZERO') then bzero:=strtofloat(buf);
    if (keyword='BSCALE') then bscale:=strtofloat(buf);
    if (keyword='DATAMAX') then dmax:=strtofloat(buf);
    if (keyword='DATAMIN') then dmin:=strtofloat(buf);
    if (keyword='THRESH') then dmax:=strtofloat(buf);
    if (keyword='THRESL') then dmin:=strtofloat(buf);
    if (keyword='BLANK') then blank:=strtofloat(buf);
    if (keyword='XPIXSZ') then pixsz1:=strtofloat(buf);
    if (keyword='YPIXSZ') then pixsz2:=strtofloat(buf);
    if (keyword='FRAMEX') then Frx:=strtoint(buf);
    if (keyword='FRAMEY') then Fry:=strtoint(buf);
    if (keyword='FRAMEHGT') then Frheight:=strtoint(buf);
    if (keyword='FRAMEWDH') then Frwidth:=strtoint(buf);
    if (keyword='OBJECT') then objects:=trim(buf);
    if (keyword='RA') then ra:=StrToFloatDef(buf,NullCoord);
    if (keyword='DEC') then dec:=StrToFloatDef(buf,NullCoord);
    if (keyword='EQUINOX') then equinox:=StrToFloatDef(buf,2000);
    if (keyword='CTYPE1') then ctype1:=buf;
    if (keyword='CTYPE2') then ctype2:=buf;
    if (keyword='CRVAL1') then crval1:=strtofloat(buf);
    if (keyword='CRVAL2') then crval2:=strtofloat(buf);
    if (keyword='A_ORDER') then solved:=true; // polynomial present, the image must be astrometry solved.
 end;
 if (pixsz1<>0)and(pixsz2<>0) then pixratio:=pixsz1/pixsz2;
 // very crude coordinates to help astrometry if telescope is not available
 if ra=NullCoord then begin
   if (copy(ctype1,1,3)='RA-')and(crval1<>NullCoord) then
      ra:=crval1/15;
 end;
 if dec=NullCoord then begin
   if (copy(ctype2,1,4)='DEC-')and(crval2<>NullCoord) then
      dec:=crval2;
 end;
 colormode:=1;
 if (naxis=3)and(naxis1=3) then begin // contiguous color RGB
  naxis1:=naxis2;
  naxis2:=naxis3;
  naxis3:=3;
  colormode:=2;
 end;
 if (naxis=3)and(naxis1=4) then begin // contiguous color RGBA
  naxis1:=naxis2;
  naxis2:=naxis3;
  naxis3:=3;
  colormode:=3;
 end;
 if (naxis=3)and(naxis3=3) then n_axis:=3 else n_axis:=1;
end;
end;

Procedure TFits.ReadFitsImage;
var i,ii,j,npix,k,km,kk : integer;
    x,dmin,dmax : double;
    ni,sum,sum2 : extended;
begin
if FFitsInfo.naxis1=0 then exit;
dmin:=1.0E100;
dmax:=-1.0E100;
sum:=0; sum2:=0; ni:=0;
if n_axis=3 then cur_axis:=1
else begin
  cur_axis:=trunc(min(cur_axis,FFitsInfo.naxis3));
  cur_axis:=trunc(max(cur_axis,1));
end;
Fheight:=trunc(min(maxl,FFitsInfo.naxis2));
Fwidth:=trunc(min(maxl,FFitsInfo.naxis1));
FStream.Position:=0;
case FFitsInfo.bitpix of
  -64 : begin
        setlength(imar64,n_axis,Fheight,Fwidth);
        FStream.Seek(Fhdr_end+FFitsInfo.naxis2*FFitsInfo.naxis1*8*(cur_axis-1),soFromBeginning);
        end;
  -32 : begin
        setlength(imar32,n_axis,Fheight,Fwidth);
        FStream.Seek(Fhdr_end+FFitsInfo.naxis2*FFitsInfo.naxis1*4*(cur_axis-1),soFromBeginning);
        end;
    8 : begin
        setlength(imai8,n_axis,Fheight,Fwidth);
        FStream.Seek(Fhdr_end+FFitsInfo.naxis2*FFitsInfo.naxis1*(cur_axis-1),soFromBeginning);
        end;
   16 : begin
        setlength(imai16,n_axis,Fheight,Fwidth);
        FStream.Seek(Fhdr_end+FFitsInfo.naxis2*FFitsInfo.naxis1*2*(cur_axis-1),soFromBeginning);
        end;
   32 : begin
        setlength(imai32,n_axis,Fheight,Fwidth);
        FStream.Seek(Fhdr_end+FFitsInfo.naxis2*FFitsInfo.naxis1*4*(cur_axis-1),soFromBeginning);
        end;
end;
npix:=0;
case FFitsInfo.bitpix of
    -64:for k:=cur_axis-1 to cur_axis+n_axis-2 do begin
        for i:=0 to FFitsInfo.naxis2-1 do begin
         ii:=FFitsInfo.naxis2-1-i;
         for j := 0 to FFitsInfo.naxis1-1 do begin
           if (npix mod 360 = 0) then begin
             FStream.Read(d64,sizeof(d64));
             npix:=0;
           end;
           inc(npix);
           x:=InvertF64(d64[npix]);
           if x=FFitsInfo.blank then x:=0;
           if (ii<=maxl-1) and (j<=maxl-1) then imar64[k,ii,j] := x ;
           x:=FFitsInfo.bzero+FFitsInfo.bscale*x;
           dmin:=min(x,dmin);
           dmax:=max(x,dmax);
           sum:=sum+x;
           sum2:=sum2+x*x;
           ni:=ni+1;
          end;
         end;
         end;
    -32: for k:=cur_axis-1 to cur_axis+n_axis-2 do begin
        for i:=0 to FFitsInfo.naxis2-1 do begin
         ii:=FFitsInfo.naxis2-1-i;
         for j := 0 to FFitsInfo.naxis1-1 do begin
           if (npix mod 720 = 0) then begin
             FStream.Read(d32,sizeof(d32));
             npix:=0;
           end;
           inc(npix);
           x:=InvertF32(d32[npix]);
           if x=FFitsInfo.blank then x:=0;
           if (ii<=maxl-1) and (j<=maxl-1) then imar32[k,ii,j] := x ;
           x:=FFitsInfo.bzero+FFitsInfo.bscale*x;
           dmin:=min(x,dmin);
           dmax:=max(x,dmax);
           sum:=sum+x;
           sum2:=sum2+x*x;
           ni:=ni+1;
         end;
         end;
         end;
     8 : if colormode=1 then
        for k:=cur_axis-1 to cur_axis+n_axis-2 do begin
        for i:=0 to FFitsInfo.naxis2-1 do begin
         ii:=FFitsInfo.naxis2-1-i;
         for j := 0 to FFitsInfo.naxis1-1 do begin
           if (npix mod 2880 = 0) then begin
             FStream.Read(d8,sizeof(d8));
             npix:=0;
           end;
           inc(npix);
           x:=d8[npix];
           if x=FFitsInfo.blank then x:=0;
           if (ii<=maxl-1) and (j<=maxl-1) then imai8[k,ii,j] := round(x);
           x:=FFitsInfo.bzero+FFitsInfo.bscale*x;
           dmin:=min(x,dmin);
           dmax:=max(x,dmax);
           sum:=sum+x;
           sum2:=sum2+x*x;
           ni:=ni+1;
         end;
         end;
         end else begin
          kk:=0;
          if colormode=3 then begin  // output RGB from RGBA
             n_axis:=4;
             kk:=1;
          end;
          for i:=0 to FFitsInfo.naxis2-1 do begin
           ii:=FFitsInfo.naxis2-1-i;
           for j := 0 to FFitsInfo.naxis1-1 do begin
             for k:=cur_axis+n_axis-2 downto cur_axis-1 do begin
             if (npix mod 2880 = 0) then begin
               FStream.Read(d8,sizeof(d8));
               npix:=0;
             end;
             inc(npix);
             km:=k-kk;
             if km<0 then continue; // skip A
             x:=d8[npix];
             if x=FFitsInfo.blank then x:=0;
             if (ii<=maxl-1) and (j<=maxl-1) then imai8[km,ii,j] := round(x);
             x:=FFitsInfo.bzero+FFitsInfo.bscale*x;
             dmin:=min(x,dmin);
             dmax:=max(x,dmax);
             sum:=sum+x;
             sum2:=sum2+x*x;
             ni:=ni+1;
             end;
           end;
          end;
          if colormode=3 then n_axis:=3; // restore value
         end;

     16 : for k:=cur_axis-1 to cur_axis+n_axis-2 do begin
        for i:=0 to FFitsInfo.naxis2-1 do begin
         ii:=FFitsInfo.naxis2-1-i;
         for j := 0 to FFitsInfo.naxis1-1 do begin
           if (npix mod 1440 = 0) then begin
             FStream.Read(d16,sizeof(d16));
             npix:=0;
           end;
           inc(npix);
           x:=BEtoN(SmallInt(d16[npix]));
           if x=FFitsInfo.blank then x:=0;
           if (ii<=maxl-1) and (j<=maxl-1) then imai16[k,ii,j] := round(x);
           x:=FFitsInfo.bzero+FFitsInfo.bscale*x;
           dmin:=min(x,dmin);
           dmax:=max(x,dmax);
           sum:=sum+x;
           sum2:=sum2+x*x;
           ni:=ni+1;
         end;
         end;
         end;
     32 : for k:=cur_axis-1 to cur_axis+n_axis-2 do begin
        for i:=0 to FFitsInfo.naxis2-1 do begin
         ii:=FFitsInfo.naxis2-1-i;
         for j := 0 to FFitsInfo.naxis1-1 do begin
           if (npix mod 720 = 0) then begin
             FStream.Read(d32,sizeof(d32));
             npix:=0;
           end;
           inc(npix);
           x:=BEtoN(LongInt(d32[npix]));
           if x=FFitsInfo.blank then x:=0;
           if (ii<=maxl-1) and (j<=maxl-1) then imai32[k,ii,j] := round(x);
           x:=FFitsInfo.bzero+FFitsInfo.bscale*x;
           dmin:=min(x,dmin);
           dmax:=max(x,dmax);
           sum:=sum+x;
           sum2:=sum2+x*x;
           ni:=ni+1;
         end;
         end;
         end;
end;
Fmean:=sum/ni;
Fsigma:=sqrt( (sum2/ni)-(Fmean*Fmean) );
if dmin>=dmax then dmax:=dmin+1;
if (FFitsInfo.dmin=0)and(FFitsInfo.dmax=0) then begin
  if Fitt=ittramp then begin
     FFitsInfo.dmin:=max(dmin,Fmean-5*Fsigma);
     FFitsInfo.dmax:=min(dmax,Fmean+5*Fsigma);
  end else begin
     FFitsInfo.dmin:=dmin;
     FFitsInfo.dmax:=dmax;
  end;
end;
GetImage;
end;

procedure TFits.GetImage;
var i,j: integer;
    x : word;
    h: integer;
    xx: extended;
    c: double;
begin
if FImgFullRange then begin
  Fdmin:=0;
  if FFitsInfo.bitpix=8 then
    Fdmax:=MaxByte
  else
    Fdmax:=MaxWord;
end else begin
  Fdmin:=FFitsInfo.dmin;
  Fdmax:=FFitsInfo.dmax;
end;
setlength(Fimage,n_axis,Fheight,Fwidth);
for i:=0 to 255 do FHistogram[i]:=1; // minimum 1 to take the log
case FFitsInfo.bitpix of
     -64 : begin
           if Fdmax>Fdmin then
             c:=MaxWord/(Fdmax-Fdmin)
           else
             c:=1;
           for i:=0 to Fheight-1 do begin
           for j := 0 to Fwidth-1 do begin
               xx:=FFitsInfo.bzero+FFitsInfo.bscale*imar64[0,i,j];
               x:=trunc(max(0,min(MaxWord,(xx-Fdmin) * c )) );
               Fimage[0,i,j]:=x;
               if n_axis=3 then begin
                 h:=x;
                 xx:=FFitsInfo.bzero+FFitsInfo.bscale*imar64[1,i,j];
                 x:=trunc(max(0,min(MaxWord,(xx-Fdmin) * c )) );
                 Fimage[1,i,j]:=x;
                 h:=h+x;
                 xx:=FFitsInfo.bzero+FFitsInfo.bscale*imar64[2,i,j];
                 x:=trunc(max(0,min(MaxWord,(xx-Fdmin) * c )) );
                 Fimage[2,i,j]:=x;
                 x:=(h+x) div 3;
               end;
               inc(FHistogram[x div 256]);
           end;
           end;
           end;
     -32 : begin
           if Fdmax>Fdmin then
             c:=MaxWord/(Fdmax-Fdmin)
           else
             c:=1;
           for i:=0 to Fheight-1 do begin
           for j := 0 to Fwidth-1 do begin
               xx:=FFitsInfo.bzero+FFitsInfo.bscale*imar32[0,i,j];
               x:=trunc(max(0,min(MaxWord,(xx-Fdmin) * c )) );
               Fimage[0,i,j]:=x;
               if n_axis=3 then begin
                 h:=x;
                 xx:=FFitsInfo.bzero+FFitsInfo.bscale*imar32[1,i,j];
                 x:=trunc(max(0,min(MaxWord,(xx-Fdmin) * c )) );
                 Fimage[1,i,j]:=x;
                 h:=h+x;
                 xx:=FFitsInfo.bzero+FFitsInfo.bscale*imar32[2,i,j];
                 x:=trunc(max(0,min(MaxWord,(xx-Fdmin) * c )) );
                 Fimage[2,i,j]:=x;
                 x:=(h+x) div 3;
               end;
               inc(FHistogram[x div 256]);
           end;
           end;
           end;
       8 : begin
           if Fdmax>Fdmin then
             c:=MaxWord/(Fdmax-Fdmin)
           else
             c:=1;
           for i:=0 to Fheight-1 do begin
           for j := 0 to Fwidth-1 do begin
               xx:=FFitsInfo.bzero+FFitsInfo.bscale*imai8[0,i,j];
               x:=trunc(max(0,min(MaxWord,(xx-Fdmin) * c )) );
               Fimage[0,i,j]:=x;
               if n_axis=3 then begin
                 h:=x;
                 xx:=FFitsInfo.bzero+FFitsInfo.bscale*imai8[1,i,j];
                 x:=trunc(max(0,min(MaxWord,(xx-Fdmin) * c )) );
                 Fimage[1,i,j]:=x;
                 h:=h+x;
                 xx:=FFitsInfo.bzero+FFitsInfo.bscale*imai8[2,i,j];
                 x:=trunc(max(0,min(MaxWord,(xx-Fdmin) * c )) );
                 Fimage[2,i,j]:=x;
                 x:=(h+x) div 3;
               end;
               inc(FHistogram[x div 256]);
           end;
           end;
           end;
      16 : begin
           if Fdmax>Fdmin then
              c:=MaxWord/(Fdmax-Fdmin)
           else
              c:=1;
           for i:=0 to Fheight-1 do begin
           for j := 0 to Fwidth-1 do begin
               xx:=FFitsInfo.bzero+FFitsInfo.bscale*imai16[0,i,j];
               x:=trunc(max(0,min(MaxWord,(xx-Fdmin) * c )) );
               Fimage[0,i,j]:=x;
               if n_axis=3 then begin
                 h:=x;
                 xx:=FFitsInfo.bzero+FFitsInfo.bscale*imai16[1,i,j];
                 x:=trunc(max(0,min(MaxWord,(xx-Fdmin) * c )) );
                 Fimage[1,i,j]:=x;
                 h:=h+x;
                 xx:=FFitsInfo.bzero+FFitsInfo.bscale*imai16[2,i,j];
                 x:=trunc(max(0,min(MaxWord,(xx-Fdmin) * c )) );
                 Fimage[2,i,j]:=x;
                 x:=(h+x) div 3;
               end;
               inc(FHistogram[x div 256]);
           end;
           end;
           end;
      32 : begin
           if Fdmax>Fdmin then
             c:=MaxWord/(Fdmax-Fdmin)
           else
             c:=1;
           for i:=0 to Fheight-1 do begin
           for j := 0 to Fwidth-1 do begin
               xx:=FFitsInfo.bzero+FFitsInfo.bscale*imai32[0,i,j];
               x:=trunc(max(0,min(MaxWord,(xx-Fdmin) * c )) );
               Fimage[0,i,j]:=x;
               if n_axis=3 then begin
                 h:=x;
                 xx:=FFitsInfo.bzero+FFitsInfo.bscale*imai32[1,i,j];
                 x:=trunc(max(0,min(MaxWord,(xx-Fdmin) * c )) );
                 Fimage[1,i,j]:=x;
                 h:=h+x;
                 xx:=FFitsInfo.bzero+FFitsInfo.bscale*imai32[2,i,j];
                 x:=trunc(max(0,min(MaxWord,(xx-Fdmin) * c )) );
                 Fimage[2,i,j]:=x;
                 x:=(h+x) div 3;
               end;
               inc(FHistogram[x div 256]);
           end;
           end;
           end;
      end;
FimageC:=c;
FimageMin:=Fdmin;
ApplyBPM(Fbpm,FBPMcount);
end;

procedure TFits.ApplyBPM(bpm:TBpm; count:integer);
var i,x,y,x0,y0: integer;
begin
if (FBPMcount>0)and(FBPMnax=FFitsInfo.naxis) then begin
  if (FFitsInfo.Frwidth>0)and(FFitsInfo.Frheight>0)and(FFitsInfo.Frx>=0)and(FFitsInfo.Fry>=0) then begin
    x0:=FFitsInfo.Frx;
    y0:=FFitsInfo.Fry;
  end else begin
    x0:=0;
    y0:=0;
  end;
  for i:=1 to count do begin
    x:=bpm[i,1]-x0;
    y:=bpm[i,2]-y0;
    if (x>0)and(x<Fwidth-2)and(y>0)and(y<Fheight-2) then begin
      image[0,y,x]:=(image[0,y-1,x]+image[0,y+1,x]+image[0,y,x-1]+image[0,y,x+1]) div 4;
      if n_axis=3 then begin
        image[1,y,x]:=(image[1,y-1,x]+image[1,y+1,x]+image[1,y,x-1]+image[1,y,x+1]) div 4;
        image[2,y,x]:=(image[2,y-1,x]+image[2,y+1,x]+image[2,y,x-1]+image[2,y,x+1]) div 4;
      end;
    end;
  end;
end;
end;

procedure TFits.SetBPM(value: TBpm; count,nx,ny,nax:integer);
var i:integer;
begin
 for i:=1 to count do begin
    Fbpm[i,1]:=value[i,1];
    Fbpm[i,2]:=value[i,2];
 end;
 FBPMcount:=count;
 FBPMnx:=nx;
 FBPMny:=ny;
 FBPMnax:=nax;
end;

procedure TFits.SetImgFullRange(value: boolean);
begin
  FImgFullRange:=value;
  if (Fheight>0)and(Fwidth>0) then GetImage;
end;

function TFits.Citt(value: Word):Word;
begin
case Fitt of
ittlinear: begin
          // Linear
         result:=value;
         end;
ittramp: begin
          // Ramp
          result:=value;
         end;
ittlog:  begin
          // Log
          if value=0 then result:=0
          else result:=round(clog*ln(value));
          end;
ittsqrt: begin
          // sqrt
          if value=0 then result:=0
          else result:=round(csqrt*sqrt(value));
         end;
end;
end;

procedure TFits.GetIntfImg;
var i,j,row : integer;
    x : word;
    xx: extended;
    c: double;
    color: TFPColor;
begin
FIntfImg.LoadFromBitmap(emptybmp.Handle,0);
FIntfImg.SetSize(Fwidth,Fheight);
if FImgDmin>=FImgDmax then FImgDmax:=FImgDmin+1;
c:=MaxWord/(FImgDmax-FImgDmin);
color.alpha:=65535;
for i:=0 to Fheight-1 do begin
   if invertY then row:=Fheight-1-i
              else row:=i;
   for j := 0 to Fwidth-1 do begin
       xx:=Fimage[0,i,j];
       x:=trunc(max(0,min(MaxWord,(xx-FImgDmin) * c )) );
       color.red:=Citt(x);
       if n_axis=3 then begin
         xx:=Fimage[1,i,j];
         x:=trunc(max(0,min(MaxWord,(xx-FImgDmin) * c )) );
         color.green:=Citt(x);
         xx:=Fimage[2,i,j];
         x:=trunc(max(0,min(MaxWord,(xx-FImgDmin) * c )) );
         color.blue:=Citt(x);
       end else begin
         color.green:=color.red;
         color.blue:=color.red;
       end;
       if invertX then begin
          FIntfImg.Colors[Fwidth-j,row]:=color;
       end else begin
          FIntfImg.Colors[j,row]:=color;
       end;
   end;
end;
end;

procedure TFits.GetBitmap(var imabmp:Tbitmap);
var
    ImgHandle,ImgMaskHandle: HBitmap;
begin
imabmp.freeimage;
imabmp.height:=1;
imabmp.width:=1;
if FFitsInfo.naxis1>0 then begin
  FIntfImg.CreateBitmaps(ImgHandle,ImgMaskHandle,false);
  imabmp.freeimage;
  imabmp.SetHandles(ImgHandle,ImgMaskHandle);
end;
end;

end.
