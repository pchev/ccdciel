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

uses SysUtils, Classes, LazFileUtils, u_utils, u_global, BGRABitmap, BGRABitmapTypes,
  LazUTF8, Graphics,Math, FPImage, Controls, LCLType, Forms, Dialogs, u_translation,
  StdCtrls, ExtCtrls, Buttons, IntfGraphics;

type

 TFitsInfo = record
            valid, solved: boolean;
            bitpix,naxis,naxis1,naxis2,naxis3 : integer;
            Frx,Fry,Frwidth,Frheight,BinX,BinY: integer;
            bzero,bscale,dmax,dmin,blank : double;
            equinox,ra,dec,crval1,crval2: double;
            pixsz1,pixsz2,pixratio,focallen: double;
            objects,ctype1,ctype2 : string;
            end;

 THeaderBlock = array[1..36,1..80] of char;

 TStar = record
         x,y: double;
         hfd, fwhm: double;
         vmax, snr: double;
         end;
 TStarList = array of TStar;

 Timai8 = array of array of array of byte; TPimai8 = ^Timai8;
 Timai16 = array of array of array of smallint; TPimai16 = ^Timai16;
 Timaw16 = array of array of array of word; TPimaw16 = ^Timaw16;
 Timai32 = array of array of array of longint; TPimai32 = ^Timai32;
 Timar32 = array of array of array of single; TPimar32 = ^Timar32;
 Timar64 = array of array of array of double; TPimar64 = ^Timar64;

 THistogram = array[0..high(word)] of integer;

 TMathOperator = (moAdd,moSub,moMean,moMult,moDiv);

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
      procedure Assign(value: TFitsHeader);
      function ReadHeader(ff:TMemoryStream): integer;
      function NewWCS(ff:TMemoryStream): boolean;
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
    d16 : array[1..1440] of smallint;
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
    FimageC, FimageMin,FimageMax : double;
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
    FImgFullRange,FStreamValid: Boolean;
    Fbpm: TBpm;
    FBPMcount,FBPMnx,FBPMny,FBPMnax: integer;
    gamma_c : array[0..32768] of single; {prepared power values for gamma correction}
    FGamma: single;
    emptybmp:Tbitmap;
    FMarkOverflow: boolean;
    FMaxADU, FOverflow, FUnderflow: double;
    f_ViewHeaders: TForm;
    m_ViewHeaders: TMemo;
    p_ViewHeaders: TPanel;
    b_ViewHeaders: TButton;
    FStarList: TStarList;
    Procedure ViewHeadersClose(Sender: TObject; var CloseAction:TCloseAction);
    Procedure ViewHeadersBtnClose(Sender: TObject);
    procedure SetStream(value:TMemoryStream);
    function GetStream: TMemoryStream;
    procedure SetVideoStream(value:TMemoryStream);
    Procedure ReadFitsImage;
    Procedure WriteFitsImage;
    Procedure GetImage;
    function GammaCorr(value: Word):byte;
    procedure SetImgFullRange(value: boolean);
    function GetHasBPM: boolean;
    procedure SetGamma(value: single);
  protected
    { Protected declarations }
  public
    { Public declarations }
     constructor Create(AOwner:TComponent); override;
     destructor  Destroy; override;
     Procedure ViewHeaders;
     Procedure LoadStream;
     procedure GetFitsInfo;
     procedure GetBGRABitmap(var bgra: TBGRABitmap);
     procedure SaveToFile(fn: string);
     procedure LoadFromFile(fn:string);
     procedure SetBPM(value: TBpm; count,nx,ny,nax:integer);
     procedure ApplyBPM;
     procedure ClearImage;
     procedure Math(operand: TFits; MathOperator:TMathOperator; new: boolean=false);
     procedure Shift(dx,dy: double);
     procedure ShiftInteger(dx,dy: integer);
     procedure Bitpix8to16;
     function  SameFormat(f:TFits): boolean;
     function  double_star(ri, x,y : integer):boolean;
     function  value_subpixel(x1,y1:double):double;
     procedure FindBrightestPixel(x,y,s,starwindow2: integer; out xc,yc:integer; out vmax: double; accept_double: boolean=true);
     procedure FindStarPos(x,y,s: integer; out xc,yc,ri:integer; out vmax,bg,bg_standard_deviation: double);
     procedure GetHFD(x,y,ri: integer; bg,bg_standard_deviation: double; out xc,yc,hfd,star_fwhm,valmax,snr: double);
     procedure GetHFD2(x,y,s: integer; out xc,yc,bg,bg_standard_deviation,hfd,star_fwhm,valmax,snr: double);{han.k 2018-3-21}
     procedure GetStarList(rx,ry,s: integer);
     procedure MeasureStarList(s: integer; list: TArrayDouble2);
     procedure ClearStarList;
     property IntfImg: TLazIntfImage read FIntfImg;
     property Title : string read FTitle write FTitle;
     Property HeaderInfo : TFitsInfo read FFitsInfo;
     property Header: TFitsHeader read FHeader write FHeader;
     Property Stream : TMemoryStream read GetStream write SetStream;
     Property VideoStream : TMemoryStream write SetVideoStream;
     property Histogram : THistogram read FHistogram;
     property ImgDmin : Word read FImgDmin write FImgDmin;
     property ImgDmax : Word read FImgDmax write FImgDmax;
     property Gamma: single read FGamma write SetGamma;
     property image : Timaw16 read Fimage;
     property imageC : double read FimageC;
     property imageMin : double read FimageMin;
     property imageMax : double read FimageMax;
     property imageMean: double read Fmean;
     property imageSigma: double read Fsigma;
     property ImgFullRange: Boolean read FImgFullRange write SetImgFullRange;
     property MaxADU: double read FMaxADU write FMaxADU;
     property MarkOverflow: boolean read FMarkOverflow write FMarkOverflow;
     property Overflow: double read FOverflow write FOverflow;
     property Underflow: double read FUnderflow write FUnderflow;
     property hasBPM: boolean read GetHasBPM;
     property StarList: TStarList read FStarList;
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

procedure TFitsHeader.Assign(value: TFitsHeader);
begin
  ClearHeader;
  FRows.Assign(value.FRows);
  FKeys.Assign(value.FKeys);
  FValues.Assign(value.FValues);
  FComments.Assign(value.FComments);
  Fvalid:=value.Fvalid;
 end;

function TFitsHeader.NewWCS(ff:TMemoryStream): boolean;
var header : THeaderBlock;
    i,p1,p2,n : integer;
    eoh : boolean;
    row,keyword,value,comment,buf : string;
    P: PChar;
const excl1:array[0..18] of string=('CTYPE','WCSAXES','EQUINOX','LONPOLE','LATPOLE','CRVAL','CRPIX','CUNIT','CD','CDELT','A_','B_','AP_','BP_','PV','CROTA','END','IMAGEW','IMAGEH');
      excl2:array[0..3] of string=('SIMPLE','BITPIX','EXTEND','NAXIS');
  function IsKeywordIn(k:string; klist:array of string): boolean;
  var j: integer;
  begin
    result:=false;
    for j:=0 to Length(klist)-1 do begin
      if pos(klist[j],k)=1 then begin
        result:=true;
        break;
      end;
    end;
  end;

begin
 result:=false;
 if FKeys.Count>0 then begin
   // delete old wcs
   for i:=FKeys.Count-1 downto 0 do begin
     if IsKeywordIn(FKeys[i],excl1) then begin
        Delete(i);
     end;
   end;
   // load new wcs
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
         if not IsKeywordIn(keyword,excl2) then begin
           FRows.add(row);
           FKeys.add(keyword);
           FValues.add(value);
           FComments.add(comment);
         end;
      end;
      if not Fvalid then begin
        Break;
      end;
   until eoh;
 end;
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
  if result then val:=StrToIntDef(trim(FValues[k]),0);
end;

function TFitsHeader.Valueof(key: string; out val: double): boolean; overload;
var k: integer;
begin
  val:=0;
  k:=FKeys.IndexOf(key);
  result:=(k>=0);
  if result then val:=StrToFloatDef(trim(FValues[k]),0);
end;

function TFitsHeader.Valueof(key: string; out val: boolean): boolean; overload;
var k: integer;
begin
  val:=false;
  k:=FKeys.IndexOf(key);
  result:=(k>=0);
  if result then val:=(trim(FValues[k])='T');
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
Fheight:=0;
Fwidth:=0;
ImgDmin:=0;
FBPMcount:=0;
ImgDmax:=MaxWord;
FImgFullRange:=false;
FStreamValid:=false;
FMarkOverflow:=false;
FMaxADU:=MAXWORD;
FOverflow:=MAXWORD;
FUnderflow:=0;
FFitsInfo.valid:=false;
FFitsInfo.naxis1:=0;
FHeader:=TFitsHeader.Create;
FStream:=TMemoryStream.Create;
FIntfImg:=TLazIntfImage.Create(0,0);
emptybmp:=Tbitmap.Create;
emptybmp.SetSize(1,1);
SetGamma(1.0);
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
 FStreamValid:=true;
except
 FFitsInfo.valid:=false;
end;
end;

Procedure TFits.LoadStream;
begin
  if FFitsInfo.valid then begin
    ReadFitsImage;
  end;
end;

function TFits.GetStream: TMemoryStream;
begin
  if not FStreamValid then begin
    WriteFitsImage;
    FStreamValid:=true;
  end;
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

procedure TFits.LoadFromFile(fn:string);
var mem: TMemoryStream;
begin
if FileExistsUTF8(fn) then begin
 mem:=TMemoryStream.Create;
 try
   mem.LoadFromFile(fn);
   SetBPM(bpm,0,0,0,0);
   SetStream(mem);
   LoadStream;
 finally
   mem.free;
 end;
end
else begin
 ClearImage;
 ShowMessage(Format(rsFileNotFound, [fn]));
end;
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
b_ViewHeaders.Caption:=rsClose;
b_ViewHeaders.Top:=4;
b_ViewHeaders.Left:=40;
b_ViewHeaders.Cancel:=true;
b_ViewHeaders.Default:=true;
b_ViewHeaders.OnClick:=ViewHeadersBtnClose;
m_ViewHeaders.Lines:=FHeader.Rows;
FormPos(f_ViewHeaders,mouse.CursorPos.X,mouse.CursorPos.Y);
if trim(FTitle)='' then
   f_ViewHeaders.Caption:=rsFITSHeader
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
 valid:=false; solved:=false; naxis1:=0 ; naxis2:=0 ; naxis3:=1; bitpix:=0 ; dmin:=0 ; dmax := 0; blank:=0;
 bzero:=0 ; bscale:=1; equinox:=2000; ra:=NullCoord; dec:=NullCoord; crval1:=NullCoord; crval2:=NullCoord;
 objects:=''; ctype1:=''; ctype2:=''; pixsz1:=0; pixsz2:=0; pixratio:=1; Frx:=-1;Fry:=-1;Frwidth:=0;Frheight:=0;
 focallen:=0; BinX:=1; BinY:=1;
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
    if (keyword='FOCALLEN') then focallen:=strtofloat(buf);
    if (keyword='XPIXSZ') then pixsz1:=strtofloat(buf);
    if (keyword='YPIXSZ') then pixsz2:=strtofloat(buf);
    if (keyword='XBINNING') then BinX:=round(StrToFloat(buf));
    if (keyword='YBINNING') then BinY:=round(StrToFloat(buf));
    if (keyword='FRAMEX') then Frx:=round(StrToFloat(buf));
    if (keyword='FRAMEY') then Fry:=round(StrToFloat(buf));
    if (keyword='FRAMEHGT') then Frheight:=round(StrToFloat(buf));
    if (keyword='FRAMEWDH') then Frwidth:=round(StrToFloat(buf));
    if (keyword='OBJECT') then objects:=trim(buf);
    if (keyword='RA') then ra:=StrToFloatDef(buf,NullCoord);
    if (keyword='DEC') then dec:=StrToFloatDef(buf,NullCoord);
    if (keyword='EQUINOX') then equinox:=StrToFloatDef(buf,2000);
    if (keyword='CTYPE1') then ctype1:=buf;
    if (keyword='CTYPE2') then ctype2:=buf;
    if (keyword='CRVAL1') then crval1:=strtofloat(buf);
    if (keyword='CRVAL2') then crval2:=strtofloat(buf);
    if (keyword='A_ORDER') or
       (keyword='AMDX1') or
       (keyword='CD1_1')
        then solved:=true; // the image must be astrometry solved.
 end;
 if (pixsz1<>0)and(pixsz2<>0) then pixratio:=pixsz1/pixsz2;
 valid:=valid and (naxis>0); // do not process file without primary array
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
    x16,b16:smallint;
    x8,b8:byte;
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
b8:=round(FFitsInfo.blank);
b16:=round(FFitsInfo.blank);
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
           x8:=d8[npix];
           if x8=b8 then x8:=0;
           if (ii<=maxl-1) and (j<=maxl-1) then imai8[k,ii,j] := x8;
           x:=FFitsInfo.bzero+FFitsInfo.bscale*x8;
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
             x8:=d8[npix];
             if x8=b8 then x8:=0;
             if (ii<=maxl-1) and (j<=maxl-1) then imai8[km,ii,j] := x8;
             x:=FFitsInfo.bzero+FFitsInfo.bscale*x8;
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
           x16:=BEtoN(d16[npix]);
           if x16=b16 then x16:=0;
           if (ii<=maxl-1) and (j<=maxl-1) then imai16[k,ii,j] := x16;
           x:=FFitsInfo.bzero+FFitsInfo.bscale*x16;
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
FStreamValid:=true;
Fmean:=sum/ni;
Fsigma:=sqrt( (sum2/ni)-(Fmean*Fmean) );
if dmin>=dmax then begin
   dmax:=dmin+1;
   dmin:=dmin-1;
end;
if (FFitsInfo.dmin=0)and(FFitsInfo.dmax=0) then begin
  FFitsInfo.dmin:=dmin;
  FFitsInfo.dmax:=dmax;
end;
SetLength(FStarList,0); {reset object list}
GetImage;
end;

Procedure TFits.WriteFitsImage;
var hdrmem: TMemoryStream;
    i,j,k,ii,npix: integer;
    first:boolean;
begin
  hdrmem:=FHeader.GetStream;
  Fhdr_end:=hdrmem.Size;
  FStream.Clear;
  FStream.Position:=0;
  hdrmem.Position:=0;
  FStream.CopyFrom(hdrmem,Fhdr_end);
  hdrmem.Free;
  npix:=0;
  first:=true;
  case FFitsInfo.bitpix of
     8 : begin
          for k:=cur_axis-1 to cur_axis+n_axis-2 do begin
          for i:=0 to FFitsInfo.naxis2-1 do begin
           ii:=FFitsInfo.naxis2-1-i;
           for j := 0 to FFitsInfo.naxis1-1 do begin
             if (npix mod 1440 = 0) then begin
               if not first then FStream.Write(d8,sizeof(d8));
               FillWord(d8,sizeof(d8),0);
               npix:=0;
               first:=false;
             end;
             inc(npix);
             d8[npix]:=imai8[k,ii,j];
           end;
           end;
           end;
           if npix>0 then  FStream.Write(d8,sizeof(d8));
           end;
     16 : begin
          for k:=cur_axis-1 to cur_axis+n_axis-2 do begin
          for i:=0 to FFitsInfo.naxis2-1 do begin
           ii:=FFitsInfo.naxis2-1-i;
           for j := 0 to FFitsInfo.naxis1-1 do begin
             if (npix mod 1440 = 0) then begin
               if not first then FStream.Write(d16,sizeof(d16));
               FillWord(d16,sizeof(d16),0);
               npix:=0;
               first:=false;
             end;
             inc(npix);
             d16[npix]:=NtoBE(imai16[k,ii,j]);
           end;
           end;
           end;
           if npix>0 then  FStream.Write(d16,sizeof(d16));
           end;
     32 : begin
          for k:=cur_axis-1 to cur_axis+n_axis-2 do begin
          for i:=0 to FFitsInfo.naxis2-1 do begin
           ii:=FFitsInfo.naxis2-1-i;
           for j := 0 to FFitsInfo.naxis1-1 do begin
             if (npix mod 1440 = 0) then begin
               if not first then FStream.Write(d32,sizeof(d32));
               FillWord(d32,sizeof(d32),0);
               npix:=0;
               first:=false;
             end;
             inc(npix);
             d32[npix]:=NtoBE(imai32[k,ii,j]);
           end;
           end;
           end;
           if npix>0 then  FStream.Write(d32,sizeof(d32));
           end;
  end;
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
for i:=0 to high(word) do FHistogram[i]:=1; // minimum 1 to take the log
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
               inc(FHistogram[x]);
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
               inc(FHistogram[x]);
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
               inc(FHistogram[x]);
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
               inc(FHistogram[x]);
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
               inc(FHistogram[x]);
           end;
           end;
           end;
      end;
FimageC:=c;

FimageMin:=Fdmin;
FimageMax:=Fdmax;
if FimageMin<0 then FimageMin:=0;
end;

procedure TFits.ApplyBPM;
var i,x,y,x0,y0: integer;
begin
if (FBPMcount>0)and(FBPMnax=FFitsInfo.naxis) then begin
  if (FFitsInfo.Frwidth>0)and(FFitsInfo.Frheight>0)and(FFitsInfo.Frx>=0)and(FFitsInfo.Fry>=0) then begin
    x0:=FFitsInfo.Frx;
    y0:=FBPMny-FFitsInfo.Fry-FFitsInfo.Frheight;
  end else begin
    x0:=0;
    y0:=0;
  end;
  for i:=1 to FBPMcount do begin
    x:=Fbpm[i,1]-x0;
    y:=Fbpm[i,2]-y0;
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

function TFits.GetHasBPM: boolean;
begin
  result:=FBPMcount>0;
end;

procedure TFits.SetImgFullRange(value: boolean);
begin
  FImgFullRange:=value;
  if (Fheight>0)and(Fwidth>0) then GetImage;
end;

function TFits.GammaCorr(value: Word):byte;
begin
  // gamma_c is 0..1 of length 32768
  // value is 0..65535
  // result is 0..255
  result:=round(255*gamma_c[trunc(value/2)]);
end;

procedure TFits.SetGamma(value: single);
var
  i: integer;
begin
  if value<>FGamma then begin
    FGamma:=value;
    for i:=0 to 32768 do
      gamma_c[i]:=power(i/32768.0, gamma);
  end;
end;

procedure TFits.GetBGRABitmap(var bgra: TBGRABitmap);
var i,j : integer;
    x : word;
    xx,xxg,xxb: extended;
    c,overflow,underflow: double;
    p: PBGRAPixel;
    HighOverflow,LowOverflow: TBGRAPixel;
begin
HighOverflow:=ColorToBGRA(clFuchsia);
LowOverflow:=ColorToBGRA(clYellow);
overflow:=(FOverflow-FimageMin)*FimageC;
underflow:=(FUnderflow-FimageMin)*FimageC;
bgra.SetSize(Fwidth,Fheight);
if FImgDmin>=FImgDmax then FImgDmax:=FImgDmin+1;
c:=MaxWord/(FImgDmax-FImgDmin);
for i:=0 to Fheight-1 do begin
   p := bgra.Scanline[i];
   for j := 0 to Fwidth-1 do begin
       xx:=Fimage[0,i,j];
       x:=trunc(max(0,min(MaxWord,(xx-FImgDmin) * c )) );
       if n_axis=3 then begin
         // 3 chanel color image
         p^.red:=GammaCorr(x);
         xxg:=Fimage[1,i,j];
         x:=trunc(max(0,min(MaxWord,(xxg-FImgDmin) * c )) );
         p^.green:=GammaCorr(x);
         xxb:=Fimage[2,i,j];
         x:=trunc(max(0,min(MaxWord,(xxb-FImgDmin) * c )) );
         p^.blue:=GammaCorr(x);
         if FMarkOverflow then begin
           if maxvalue([xx,xxg,xxb])>=overflow then
             p^:=HighOverflow
           else if minvalue([xx,xxg,xxb])<=underflow then
             p^:=LowOverflow;
         end;
       end else begin
         // B/W image
         p^.red:=GammaCorr(x);
         p^.green:=p^.red;
         p^.blue:=p^.red;
         if FMarkOverflow then begin
           if xx>=overflow then
             p^:=HighOverflow
           else if xx<=underflow then
             p^:=LowOverflow;
         end;
       end;
       p^.alpha:=255;
       inc(p);
   end;
end;
bgra.InvalidateBitmap;
end;

procedure TFits.ClearImage;
begin
Fheight:=0;
Fwidth:=0;
FFitsInfo.naxis1:=0;
FFitsInfo.valid:=false;
FFitsInfo.solved:=false;
setlength(imar64,0,0,0);
setlength(imar32,0,0,0);
setlength(imai8,0,0,0);
setlength(imai16,0,0,0);
setlength(imai32,0,0,0);
setlength(Fimage,0,0,0);
FStream.Clear;
end;

function TFits.double_star(ri, x,y : integer):boolean;
// double star detection based difference bright_spot and center_of_gravity
var SumVal,SumValX,SumValY,val,vmax,bg, Xg, Yg: double;
     i,j : integer;
begin
  try
  // New background from corner values
  bg:=0;
  for i:=-ri+1 to ri do {calculate average background at the square boundaries of region of interest}
  begin
    bg:=bg+Fimage[0,y+ri,x+i];{top line, left to right}
    bg:=bg+Fimage[0,y+i,x+ri];{right line, top to bottom}
    bg:=bg+Fimage[0,y-ri,x-i];{bottom line, right to left}
    bg:=bg+Fimage[0,y-i,x-ri];{right line, bottom to top}
  end;
  bg:=bg/(8*ri);
  bg:=FimageMin+bg/FimageC;

  SumVal:=0;
  SumValX:=0;
  SumValY:=0;
  vmax:=0;
  for i:=-ri to ri do
    for j:=-ri to ri do
    begin
      val:=FimageMin+Fimage[0,y+j,x+i]/FimageC-bg;
      if val<0 then val:=0;
      if val>vmax then vmax:=val;
      SumVal:=SumVal+val;
      SumValX:=SumValX+val*(i);
     SumValY:=SumValY+val*(j);
    end;
  Xg:=SumValX/SumVal;
  Yg:=SumValY/SumVal;
  if ((Xg*Xg)+(Yg*Yg))>0.3 then result:=true {0.3 is experimental factor. Double star, too much unbalance between bright spot and centre of gravity}
    else
    result:=false;
  except
    on E: Exception do begin
        result:=true;
    end;
  end;
end;{double star detection}

function TFits.value_subpixel(x1,y1:double):double;
{calculate image pixel value on subpixel level}
// see: https://www.ap-i.net/mantis/file_download.php?file_id=817&type=bug
var
  x_trunc,y_trunc: integer;
  x_frac,y_frac : double;
begin
  try
  result:=0;
  x_trunc:=trunc(x1);
  y_trunc:=trunc(y1);
  if (x_trunc<=0) or (x_trunc>=(Fwidth-2)) or (y_trunc<=0) or (y_trunc>=(Fheight-2)) then exit;
  x_frac :=frac(x1);
  y_frac :=frac(y1);
  result:= Fimage[0,y_trunc ,x_trunc ] * (1-x_frac)*(1-y_frac);{pixel left top, 1}
  result:=result + Fimage[0,y_trunc ,x_trunc+1] * ( x_frac)*(1-y_frac);{pixel right top, 2}
  result:=result + Fimage[0,y_trunc+1,x_trunc ] * (1-x_frac)*( y_frac);{pixel left bottom, 3}
  result:=result + Fimage[0,y_trunc+1,x_trunc+1] * ( x_frac)*( y_frac);{pixel right bottom, 4}
  except
    on E: Exception do begin
        result:=0;
    end;
  end;
end;

procedure TFits.FindBrightestPixel(x,y,s,starwindow2: integer; out xc,yc:integer; out vmax: double; accept_double: boolean=true);
// brightest 3x3 pixels in area s*s centered on x,y
var i,j,rs,xm,ym: integer;
    bg,bg_average,bg_standard_deviation: double;
    val :double;
begin
 rs:= s div 2;
 if (x-rs)<3 then x:=rs+3;
 if (x+rs)>(Fwidth-3) then x:=Fwidth-rs-3;
 if (y-rs)<3 then y:=rs+3;
 if (y+rs)>(Fheight-3) then y:=Fheight-rs-3;

 vmax:=0;
 xm:=0;
 ym:=0;

 try

   // average background
  bg_average:=0;
  for i:=-rs+1 to rs do {calculate average background at the square boundaries of region of interest}
  begin
    bg_average:=bg_average+Fimage[0,y+rs,x+i];{top line, left to right}
    bg_average:=bg_average+Fimage[0,y+i,x+rs];{right line, top to bottom}
    bg_average:=bg_average+Fimage[0,y-rs,x-i];{bottom line, right to left}
    bg_average:=bg_average+Fimage[0,y-i,x-rs];{right line, bottom to top}
  end;
  bg_average:=bg_average/(8*rs);

  bg_standard_deviation:=0;
  for i:=-rs+1 to rs do {calculate standard deviation background at the square boundaries of region of interest}
  begin
    bg_standard_deviation:=bg_standard_deviation+sqr(bg_average-Fimage[0,y+rs,x+i]);{top line, left to right}
    bg_standard_deviation:=bg_standard_deviation+sqr(bg_average-Fimage[0,y+i,x+rs]);{right line, top to bottom}
    bg_standard_deviation:=bg_standard_deviation+sqr(bg_average-Fimage[0,y-rs,x-i]);{bottom line, right to left}
    bg_standard_deviation:=bg_standard_deviation+sqr(bg_average-Fimage[0,y-i,x-rs]);{left line, bottom to top}
  end;
  bg_standard_deviation:=sqrt(0.0001+bg_standard_deviation/(8*rs))/FimageC;

  bg:=FimageMin+bg_average/FimageC;

 // try with double star exclusion
 for i:=-rs to rs do
   for j:=-rs to rs do begin
     val:=(Fimage[0,y+j-1 ,x+i-1]+Fimage[0,y+j-1 ,x+i]+Fimage[0,y+j-1 ,x+i+1]+
           Fimage[0,y+j ,x+i-1]+Fimage[0,y+j ,x+i]+Fimage[0,y+j ,x+i+1]+
           Fimage[0,y+j+1 ,x+i-1]+Fimage[0,y+j+1 ,x+i]+Fimage[0,y+j+1 ,x+i+1])/9;

     Val:=FimageMin+Val/FimageC-bg;
     // huge performance improvement by checking only the pixels above the noise
     if (val>((5*bg_standard_deviation))) and (Val>vmax) then
     begin
       if double_star(starwindow2, x+i,y+j)=false then
       begin
         vmax:=Val;
         xm:=i;
         ym:=j;
       end;
     end;
 end;

 if accept_double then begin
 // if we not find anything repeat with only max value
 if vmax=0 then
   for i:=-rs to rs do
     for j:=-rs to rs do begin
       val:=(Fimage[0,y+j-1 ,x+i-1]+Fimage[0,y+j-1 ,x+i]+Fimage[0,y+j-1 ,x+i+1]+
             Fimage[0,y+j ,x+i-1]+Fimage[0,y+j ,x+i]+Fimage[0,y+j ,x+i+1]+
             Fimage[0,y+j+1 ,x+i-1]+Fimage[0,y+j+1 ,x+i]+Fimage[0,y+j+1 ,x+i+1])/9;

       Val:=FimageMin+Val/FimageC;
       if Val>vmax then
       begin
         vmax:=Val;
         xm:=i;
         ym:=j;
       end;
   end;
 end;

 xc:=x+xm;
 yc:=y+ym;

 except
   on E: Exception do begin
       vmax:=0;
   end;
 end;

end;


procedure TFits.FindStarPos(x,y,s: integer; out xc,yc,ri:integer; out vmax,bg,bg_standard_deviation: double);
// center of gravity in area s*s centered on x,y
const
    max_ri=100;
var i,j,rs: integer;
    SumVal,SumValX,SumValY: double;
    val,xg,yg:double;
    distance :integer;
    bg_average : double;
    distance_histogram : array [0..max_ri] of integer;
    HistStart: boolean;
begin

  vmax:=0;
  bg:=0;
  rs:=s div 2;
  if (x-s)<1 then x:=s+1;
  if (x+s)>(Fwidth-1) then x:=Fwidth-s-1;
  if (y-s)<1 then y:=s+1;
  if (y+s)>(Fheight-1) then y:=Fheight-s-1;

  try

  // average background
  bg_average:=0;
  for i:=-rs+1 to rs do {calculate average background at the square boundaries of region of interest}
  begin
    bg_average:=bg_average+Fimage[0,y+rs,x+i];{top line, left to right}
    bg_average:=bg_average+Fimage[0,y+i,x+rs];{right line, top to bottom}
    bg_average:=bg_average+Fimage[0,y-rs,x-i];{bottom line, right to left}
    bg_average:=bg_average+Fimage[0,y-i,x-rs];{right line, bottom to top}
  end;
  bg_average:=bg_average/(8*rs);

  bg_standard_deviation:=0;
  for i:=-rs+1 to rs do {calculate standard deviation background at the square boundaries of region of interest}
  begin
    bg_standard_deviation:=bg_standard_deviation+sqr(bg_average-Fimage[0,y+rs,x+i]);{top line, left to right}
    bg_standard_deviation:=bg_standard_deviation+sqr(bg_average-Fimage[0,y+i,x+rs]);{right line, top to bottom}
    bg_standard_deviation:=bg_standard_deviation+sqr(bg_average-Fimage[0,y-rs,x-i]);{bottom line, right to left}
    bg_standard_deviation:=bg_standard_deviation+sqr(bg_average-Fimage[0,y-i,x-rs]);{left line, bottom to top}
  end;
  bg_standard_deviation:=sqrt(0.0001+bg_standard_deviation/(8*rs))/FimageC;

  bg:=FimageMin+bg_average/FimageC;

  // Get center of gravity whithin star detection box
  SumVal:=0;
  SumValX:=0;
  SumValY:=0;
  vmax:=0;
  for i:=-rs to rs do
   for j:=-rs to rs do begin
     val:=FimageMin+Fimage[0,y+j,x+i]/FimageC-bg;
     if val>((3*bg_standard_deviation)) then  {>3 * sd should be signal }
     begin
       if val>vmax then vmax:=val;
       SumVal:=SumVal+val;
       SumValX:=SumValX+val*(i);
       SumValY:=SumValY+val*(j);
     end;
   end;

  if sumval=0 then
  begin
    ri:=3;
    exit;
  end;

  Xg:=SumValX/SumVal;
  Yg:=SumValY/SumVal;
  xc:=round(x+Xg);
  yc:=round(y+Yg);

 // Get diameter of signal shape above the noise level. Find maximum distance of pixel with signal from the center of gravity. This works for donut shapes.

 for i:=0 to max_ri do distance_histogram[i]:=0;{clear histogram of pixel distances}

 for i:=-rs to rs do begin
   for j:=-rs to rs do begin
     val:=FimageMin+Fimage[0,yc+j,xc+i]/FimageC-bg;
     if val>((3*bg_standard_deviation)) then {>3 * sd should be signal }
     begin
       distance:=round((sqrt(1+ i*i + j*j )));{distance from gravity center }
       if distance<=max_ri then distance_histogram[distance]:=distance_histogram[distance]+1;{build distance histogram}
     end;
   end;
  end;

 ri:=0;
 HistStart:=false;
 repeat
    inc(ri);
    if distance_histogram[ri]>0 then {continue until we found a value>0, center of reflector ring can be black}
       HistStart:=true;
 until ((ri>=max_ri) or (HistStart and (distance_histogram[ri]=0)));{find a distance where there is no pixel illuminated, so the border of the star image of interest}

 inc(ri,2);

 if ri=0 then ri:=rs;
 if ri<3 then ri:=3;

 except
   on E: Exception do begin
       vmax:=0;
   end;
 end;
end;



procedure TFits.GetHFD(x,y,ri: integer; bg,bg_standard_deviation: double; out xc,yc,hfd,star_fwhm,valmax,snr: double);
var i,j: integer;
    SumVal,SumValX,SumValY,SumValR: double;
    Xg,Yg: double;
    r:double;
    val, pixel_counter: double;
begin
// x,y must be the star center, ri the radius of interest, bg the mean image value computed by FindStarPos
hfd:=-1;
star_fwhm:=-1;
if ri<=0 then exit;

try
// Get center of star gravity whithin radius of interest
SumVal:=0;
SumValX:=0;
SumValY:=0;
valmax:=0;
for i:=-ri to ri do
 for j:=-ri to ri do begin
   val:=FimageMin+Fimage[0,y+j,x+i]/FimageC-bg;
   if val>((3*bg_standard_deviation)) then  {3 * sd should be signal }
   begin
     if val>valmax then valmax:=val;
     SumVal:=SumVal+val;
     SumValX:=SumValX+val*(i);
     SumValY:=SumValY+val*(j);
   end;
 end;
if SumVal=0 then begin snr:=0; exit; end;{no values, abort. star_fwhm is already -1}
Xg:=SumValX/SumVal;
Yg:=SumValY/SumVal;
xc:=x+Xg;
yc:=y+Yg;

// Get HFD
SumVal:=0;
SumValR:=0;
pixel_counter:=0;
if valmax>1 then
   snr:=valmax/sqrt(valmax+2*bg)
else
  snr:=valmax*MAXWORD/sqrt(valmax*MAXWORD+2*bg*MAXWORD);
if snr>3 then
begin  // Get HFD using the aproximation routine assuming that HFD line divides the star in equal portions of gravity:
  for i:=-ri to ri do  {Make steps of one pixel in the region of interest}
    for j:=-ri to ri do
    begin
      Val:=FimageMin+value_subpixel(xc+i,yc+j)/FimageC-bg; {The calculated center of gravity is a floating point position and can be anyware, so calculate pixel values on sub-pixel level}
      if val>((3*bg_standard_deviation)) then {3 * sd should be signal}
      begin
        r:=sqrt(i*i+j*j);
        SumVal:=SumVal+Val;
        SumValR:=SumValR+Val*r;
        if val>=valmax*0.5 then pixel_counter:=pixel_counter+1;{how many pixels are above half maximum for FWHM}
      end;
    end;
    Sumval:=Sumval+0.00001;{prevent divide by zero}
    hfd:=2*SumValR/SumVal;
    hfd:=max(0.7,hfd); // minimum value for a star size of 1 pixel
    star_fwhm:=2*sqrt(pixel_counter/pi);{The surface is calculated by counting pixels above half max. The diameter of that surface called FWHM is then 2*sqrt(surface/pi) }
end;

except
  on E: Exception do begin
    hfd:=-1;
    star_fwhm:=-1;
  end;
end;
end;

procedure TFits.GetHFD2(x,y,s: integer; out xc,yc,bg,bg_standard_deviation,hfd,star_fwhm,valmax,snr: double);
// x,y, s, test location x,y and box size s x s
// xc,yc, center of gravity
// bg, background value
// bf_standard_deviation, standard deviation of background
// hfd, Half Flux Diameter of star disk
// star_fwhm, Full Width Half Maximum of star disk
// valmax, maximum value of brightest pixel in final test box.
// SNR, signal noise ratio
const
    max_ri=100;
var i,j,rs,distance,counter,ri, distance_top_value, illuminated_pixels: integer;
    SumVal,SumValX,SumValY,SumvalR,val,xg,yg,bg_average,
    pixel_counter,r, val_00,val_01,val_10,val_11,af :double;
    distance_histogram : array [0..max_ri] of integer;
    HistStart,asymmetry : boolean;
begin

  valmax:=0;
  bg:=0;
  snr:=0;
  valmax:=0;
  hfd:=-1;
  star_fwhm:=-1;

  rs:=s div 2;
  if (x-s)<1+4 then x:=s+1+4;
  if (x+s)>(Fwidth-1-4) then x:=Fwidth-s-1-4;
  if (y-s)<1+4 then y:=s+1+4;
  if (y+s)>(Fheight-1-4) then y:=Fheight-s-1-4;

  try
  // average background
  counter:=0;
  bg_average:=0;
  for i:=-rs-4 to rs+4 do {calculate mean at square boundaries of detection box}
  for j:=-rs-4 to rs+4 do
  begin
    if ( (abs(i)>rs) and (abs(j)>rs) ) then {measure only outside the box}
    begin
      bg_average:=bg_average+Fimage[0,y+i,x+j];
      inc(counter)
    end;
  end;
  bg_average:=bg_average/counter; {mean value background}
  bg:=bg_average;

  counter:=0;
  bg_standard_deviation:=0;
  for i:=-rs-4 to rs+4 do {calculate standard deviation background at the square boundaries of detection box}
    for j:=-rs-4 to rs+4 do
    begin
      if ( (abs(i)>rs) and (abs(j)>rs) ) then {measure only outside the box}
      begin
          bg_standard_deviation:=bg_standard_deviation+sqr(bg_average-Fimage[0,y+i,x+j]);
          inc(counter)
      end;
  end;
  bg_standard_deviation:=sqrt(0.0001+bg_standard_deviation/(counter)); {standard deviation in background}

  bg:=bg_average;

  repeat {## reduce box size till symmetry to remove stars}
    // Get center of gravity whithin star detection box and count signal pixels
    SumVal:=0;
    SumValX:=0;
    SumValY:=0;
    valmax:=0;
    for i:=-rs to rs do
    for j:=-rs to rs do
    begin
      val:=Fimage[0,y+j,x+i]-bg;
      if val>(3.5)*bg_standard_deviation then {just above noise level. }
      begin
        if val>valmax then valmax:=val;
        SumVal:=SumVal+val;
        SumValX:=SumValX+val*(i);
        SumValY:=SumValY+val*(j);
      end;
    end;
    if sumval<=15*bg_standard_deviation then exit; {no star found, too noisy}
    Xg:=SumValX/SumVal;
    Yg:=SumValY/SumVal;
    xc:=(x+Xg);
    yc:=(y+Yg);
   {center of star gravity found}

    if ((xc-rs<=1) or (xc+rs>=Fwidth-2) or (yc-rs<=1) or (yc+rs>=Fheight-2) ) then begin exit;end;{prevent runtime errors near sides of images}

   // Check for asymmetry. Are we testing a group of stars or a defocused star?
    val_00:=0;val_01:=0;val_10:=0;val_11:=0;
    for i:=0 to max_ri do distance_histogram[i]:=0;{clear histogram}

    for i:=-rs to 0 do begin
      for j:=-rs to 0 do begin
        val_00:=val_00+ value_subpixel(xc+i,yc+j)-bg; {value top left}
        val_01:=val_01+ value_subpixel(xc+i,yc-j)-bg; {value bottom left}
        val_10:=val_10+ value_subpixel(xc-i,yc+j)-bg; {value top right}
        val_11:=val_11+ value_subpixel(xc-i,yc-j)-bg; {value bottom right}
      end;
    end;
    af:=0.30; {## asymmetry factor. 1=is allow only prefect symmetrical, 0.000001=off}
              {0.30 make focusing to work with bad seeing}

    asymmetry:=( (val_00<af*val_11) or (val_00>val_11/af) or {diagonal asymmetry} {has asymmetry, ovals are NO LONGER accepted}
                 (val_01<af*val_10) or (val_01>val_10/af) or {diagonal asymmetry}
                 (val_00<af*val_10) or (val_00>val_10/af) or {east west asymmetry1}
                 (val_01<af*val_11) or (val_01>val_11/af) or {east west asymmetry2}
                 (val_00<af*val_01) or (val_00>val_01/af) or {north south asymmetry1}
                 (val_10<af*val_11) or (val_10>val_11/af));  {north south asymmetry2}

    if asymmetry then dec(rs,2); {try a smaller window to exclude nearby stars}
    if rs<4 then exit; {try to reduce box up to rs=4 equals 8x8 box else exit}
  until asymmetry=false; {loop and reduce box size until asymmetry is gone or exit if box is too small}

 // Get diameter of signal shape above the noise level. Find maximum distance of pixel with signal from the center of gravity. This works for donut shapes.
 for i:=0 to max_ri do distance_histogram[i]:=0;{clear histogram of pixel distances}
 for i:=-rs to rs do begin
   for j:=-rs to rs do begin
     Val:=value_subpixel(xc+i,yc+j)-bg;{##}
     if val>((3*bg_standard_deviation)) then {>3 * sd should be signal }
     begin
       distance:=round((sqrt(i*i + j*j )));{distance from star gravity center }
       if distance<=max_ri then distance_histogram[distance]:=distance_histogram[distance]+1;{build distance histogram}
     end;
   end;
  end;

 ri:=-1; {will start from distance 0}
 distance_top_value:=0;
 HistStart:=false;
 illuminated_pixels:=0;
 repeat
    inc(ri);
    illuminated_pixels:=illuminated_pixels+distance_histogram[ri];
    if distance_histogram[ri]>0 then HistStart:=true;{continue until we found a value>0, center of defocused star image can be black having a central obstruction in the telescope}
    if distance_top_value<distance_histogram[ri] then distance_top_value:=distance_histogram[ri]; {this should be 2*pi*ri if it is nice defocused star disk}
  until ((ri>=max_ri) or (ri>=rs){##} or (HistStart and (distance_histogram[ri]<=0.1*distance_top_value {##drop-off detection})));{find a distance where there is no pixel illuminated, so the border of the star image of interest}

  if ri>=rs then {star is larger then box, abort} exit; {hfd:=-1}
  if (ri>2)and(illuminated_pixels<0.35*sqr(ri+ri-2)){35% surface} then {not a star disk but stars, abort} exit; {hfd:=-1}
  if ri<3 then ri:=3; {Minimum 6+1 x 6+1 pixel box}

  // Get HFD using the aproximation routine assuming that HFD line divides the star in equal portions of gravity:
  SumVal:=0;
  SumValR:=0;
  pixel_counter:=0;

  snr:=valmax/bg_standard_deviation;{how much times above background noise}

  for i:=-ri to ri do {Make steps of one pixel}
    for j:=-ri to ri do
    begin
      Val:=value_subpixel(xc+i,yc+j)-bg;{The calculated center of gravity is a floating point position and can be anyware, so calculate pixel values on sub-pixel level}
      r:=sqrt(i*i+j*j);{Distance from star gravity center}
      SumVal:=SumVal+Val;{Sumval will be star total flux value}
      SumValR:=SumValR+Val*r; {Method Kazuhisa Miyashita, see notes of HFD calculation method}
      if val>=valmax*0.5 then pixel_counter:=pixel_counter+1;{How many pixels are above half maximum for FWHM}
    end;
  Sumval:=Sumval+0.00001;{prevent divide by zero}
  hfd:=2*SumValR/SumVal;
  hfd:=max(0.7,hfd); // minimum value for a star size of 1 pixel
  star_fwhm:=2*sqrt(pixel_counter/pi);{The surface is calculated by counting pixels above half max. The diameter of that surface called FWHM is then 2*sqrt(surface/pi) }

{==========Notes on HFD calculation method=================
  https://en.wikipedia.org/wiki/Half_flux_diameter
  http://www005.upp.so-net.ne.jp/k_miyash/occ02/halffluxdiameter/halffluxdiameter_en.html       by Kazuhisa Miyashita. No sub-pixel calculation
  https://www.lost-infinity.com/night-sky-image-processing-part-6-measuring-the-half-flux-diameter-hfd-of-a-star-a-simple-c-implementation/
  http://www.ccdware.com/Files/ITS%20Paper.pdf     See page 10, HFD Measurement Algorithm

  HFD, Half Flux Diameter is defined as: The diameter of circle where total flux value of pixels inside is equal to the outside pixel's.
  HFR, half flux radius:=0.5*HFD
  The pixel_flux:=pixel_value - background.

  The approximation routine assumes that the HFD line divides the star in equal portions of gravity:
      sum(pixel_flux * (distance_from_the_centroid - HFR))=0
  This can be rewritten as
     sum(pixel_flux * distance_from_the_centroid) - sum(pixel_values * (HFR))=0
     or
     HFR:=sum(pixel_flux * distance_from_the_centroid))/sum(pixel_flux)
     HFD:=2*HFR

  This is not an exact method but a very efficient routine. Numerical checking with an a highly oversampled artificial Gaussian shaped star indicates the following:

  Perfect two dimensional Gaussian shape with σ=1:   Numerical HFD=2.3548*σ                     Approximation 2.5066, an offset of +6.4%
  Homogeneous disk of a single value  :              Numerical HFD:=disk_diameter/sqrt(2)       Approximation disk_diameter/1.5, an offset of -6.1%

  The approximation routine is robust and efficient.

  Since the number of pixels illuminated is small and the calculated center of star gravity is not at the center of an pixel, above summation should be calculated on sub-pixel level (as used here)
  or the image should be re-sampled to a higher resolution.

  A sufficient signal to noise is required to have valid HFD value due to background noise.

  Note that for perfect Gaussian shape both the HFD and FWHM are at the same 2.3548 σ.
  }


   {=============Notes on FWHM:=====================
      1)	Determine the background level by the averaging the boarder pixels.
      2)	Calculate the standard deviation of the background.

          Signal is anything 3 * standard deviation above background

      3)	Determine the maximum signal level of region of interest.
      4)	Count pixels which are equal or above half maximum level.
      5)	Use the pixel count as area and calculate the diameter of that area  as diameter:=2 *sqrt(count/pi).}


 except
   on E: Exception do begin
     hfd:=-1;
     star_fwhm:=-1;
   end;
 end;


end;{gethfd2}

procedure TFits.ClearStarList;
begin
  SetLength(FStarList,0);
end;

procedure TFits.GetStarList(rx,ry,s: integer);
var
 fitsX,fitsY,fx,fy,nhfd,i,j,size: integer;
 hfd1,star_fwhm,vmax,bg,bgdev,xc,yc,snr: double;
 marginx,marginy,overlap: integer;
 img_temp: Timai8;
begin

overlap:=round(s/3); // large overlap to have more chance to measure a big dot as a single piece
s:=round(2*s/3);     // keep original window size after adding overlap

nhfd:=0;{set counters at zero}
SetLength(FStarList,1000);{allocate initial size}

marginx:=(FWidth-rx)div 2 div s;
marginy:=(Fheight-ry)div 2 div s;

SetLength(img_temp,1,FWidth,FHeight); {array to check for duplicate}
for j:=0 to Fheight-1 do
   for i:=0 to FWidth-1 do
      img_temp[0,i,j]:=0;  {mark as not surveyed}

for fy:=marginy to ((FHeight) div s)-marginy do { move test box with stepsize rs around}
 begin
   fitsY:=fy*s;
   for fx:=marginx to ((FWidth) div s)-marginx do
   begin
     fitsX:=fx*s;

     GetHFD2(fitsX,fitsY,s+overlap,xc,yc,bg,bgdev,hfd1,star_fwhm,vmax,snr);{2018-3-21, calculate HFD}

     {scale the result as GetHFD2 work with internal 16 bit values}
     vmax:=vmax/FimageC+FimageMin;
     bg:=bg/FimageC+FimageMin;
     bgdev:=bgdev/FimageC;

     {check valid hfd }
     if ((hfd1>0)and (Undersampled or (hfd1>0.8)))
        and (hfd1<99)
        and (img_temp[0,round(xc),round(yc)]=0)  {area not surveyed}
        and (snr>AutofocusMinSNR)  {minimal star detection level}
        and (vmax<(MaxADU-2*bg))   {new bright star but not saturated}
     then
     begin
       inc(nhfd);
       if nhfd>=Length(FStarList) then
          SetLength(FStarList,nhfd+1000);  {get more space to store values}
       FStarList[nhfd-1].x:=xc;
       FStarList[nhfd-1].y:=yc;
       FStarList[nhfd-1].hfd:=hfd1;
       FStarList[nhfd-1].fwhm:=star_fwhm;
       FStarList[nhfd-1].snr:=snr;
       FStarList[nhfd-1].vmax:=vmax;

       size:=round(2*hfd1);
       for j:=round(yc)-size to round(yc)+size do {mark the whole star area as surveyed}
          for i:=round(xc)-size to round(xc)+size do
             img_temp[0,i,j]:=1;

     end;
   end;
 end;
 SetLength(FStarList,nhfd);  {set length to new number of elements}
 SetLength(img_temp,0,0,0);
end;

procedure TFits.MeasureStarList(s: integer; list: TArrayDouble2);
var
 fitsX,fitsY,nhfd,i: integer;
 hfd1,star_fwhm,vmax,bg,bgdev,xc,yc,snr: double;
begin

nhfd:=0;{set counters at zero}
SetLength(FStarList,1000);{allocate initial size}

for i:=0 to Length(list)-1 do
 begin
   fitsX:=round(list[i,1]);
   fitsY:=round(list[i,2]);
   hfd1:=-1;
   star_fwhm:=-1;

   GetHFD2(fitsX,fitsY,s,xc,yc,bg,bgdev,hfd1,star_fwhm,vmax,snr);

   {check valid hfd, snr}
   if (((hfd1>0)and(Undersampled or (hfd1>0.8))) and (hfd1<99) and (snr>3)) then
    begin
       inc(nhfd);
       if nhfd>=Length(FStarList) then
          SetLength(FStarList,nhfd+1000);  {get more space to store values}
       FStarList[nhfd-1].x:=xc;
       FStarList[nhfd-1].y:=yc;
       FStarList[nhfd-1].hfd:=hfd1;
       FStarList[nhfd-1].fwhm:=star_fwhm;
       FStarList[nhfd-1].snr:=snr;
       FStarList[nhfd-1].vmax:=vmax;
    end;
 end;
 SetLength(FStarList,nhfd);  {set length to new number of elements}
end;

function TFits.SameFormat(f:TFits): boolean;
begin
 result := f.FFitsInfo.valid and
           (f.FFitsInfo.bitpix = FFitsInfo.bitpix)  and
           (f.FFitsInfo.naxis  = FFitsInfo.naxis )  and
           (f.FFitsInfo.naxis1 = FFitsInfo.naxis1 ) and
           (f.FFitsInfo.naxis2 = FFitsInfo.naxis2 ) and
           (f.FFitsInfo.naxis3 = FFitsInfo.naxis3 ) and
           (f.FFitsInfo.bzero  = FFitsInfo.bzero )  and
           (f.FFitsInfo.bscale = FFitsInfo.bscale );
end;

procedure TFits.Bitpix8to16;
var i,j,k,ii: integer;
    x: smallint;
begin
 if FFitsInfo.bitpix = 8 then begin
   setlength(imai16,n_axis,Fheight,Fwidth);
   for k:=cur_axis-1 to cur_axis+n_axis-2 do begin
     for i:=0 to FFitsInfo.naxis2-1 do begin
      ii:=FFitsInfo.naxis2-1-i;
      for j := 0 to FFitsInfo.naxis1-1 do begin
        x:=-32767+imai8[k,ii,j];
        imai16[k,ii,j]:=x;
      end;
     end;
   end;
 end;
 FFitsInfo.bitpix:=16;
 FFitsInfo.bscale:=1;
 FFitsInfo.bzero:=32768;
 i:=FHeader.Indexof('BITPIX');
 if i>=0 then FHeader.Delete(i);
 FHeader.Insert(i,'BITPIX',16,'');
 i:=FHeader.Indexof('BSCALE');
 if i>=0 then FHeader.Delete(i);
 FHeader.Insert(i,'BSCALE',1,'');
 i:=FHeader.Indexof('BZERO');
 if i>=0 then FHeader.Delete(i);
 FHeader.Insert(i,'BZERO',32768,'');
 setlength(imai8,0,0,0);
 WriteFitsImage;
end;

procedure TFits.Math(operand: TFits; MathOperator:TMathOperator; new: boolean=false);
var i,j,k,ii: integer;
    x,y,dmin,dmax : double;
    ni,sum,sum2 : extended;

begin
 if new or (Fheight=0)or(Fwidth=0)then begin  // first frame, just store the operand
   SetStream(operand.Stream);
   LoadStream;
 end
 else begin  // do operation
    dmin:=1.0E100;
    dmax:=-1.0E100;
    sum:=0; sum2:=0; ni:=0;
    for k:=cur_axis-1 to cur_axis+n_axis-2 do begin
      for i:=0 to FFitsInfo.naxis2-1 do begin
       ii:=FFitsInfo.naxis2-1-i;
       for j := 0 to FFitsInfo.naxis1-1 do begin
         case FFitsInfo.bitpix of
          -64 : begin
                x:=FFitsInfo.bzero+FFitsInfo.bscale*imar64[k,ii,j];
                y:=FFitsInfo.bzero+FFitsInfo.bscale*operand.imar64[k,ii,j];
                end;
          -32 : begin
                x:=FFitsInfo.bzero+FFitsInfo.bscale*imar32[k,ii,j];
                y:=FFitsInfo.bzero+FFitsInfo.bscale*operand.imar32[k,ii,j];
                end;
            8 : begin
                x:=FFitsInfo.bzero+FFitsInfo.bscale*imai8[k,ii,j];
                y:=FFitsInfo.bzero+FFitsInfo.bscale*operand.imai8[k,ii,j];
                end;
           16 : begin
                x:=FFitsInfo.bzero+FFitsInfo.bscale*imai16[k,ii,j];
                y:=FFitsInfo.bzero+FFitsInfo.bscale*operand.imai16[k,ii,j];
                end;
           32 : begin
                x:=FFitsInfo.bzero+FFitsInfo.bscale*imai32[k,ii,j];
                y:=FFitsInfo.bzero+FFitsInfo.bscale*operand.imai32[k,ii,j];
                end;
         end;
         case MathOperator of
           moAdd: x:=x+y;
           moSub: x:=x-y;
           moMean: x:=(x+y)/2;
           moMult: x:=x*y;
           moDiv : x:=x/y;
         end;
         case FFitsInfo.bitpix of
          -64 : imar64[k,ii,j] := x/FFitsInfo.bscale - FFitsInfo.bzero;
          -32 : imar32[k,ii,j] := x/FFitsInfo.bscale - FFitsInfo.bzero;
            8 : imai8[k,ii,j] := max(min(round(x/FFitsInfo.bscale - FFitsInfo.bzero),MAXBYTE),0);
           16 : imai16[k,ii,j] := max(min(round(x/FFitsInfo.bscale - FFitsInfo.bzero),maxSmallint),-maxSmallint);
           32 : imai32[k,ii,j] := max(min(round(x/FFitsInfo.bscale - FFitsInfo.bzero),maxLongint),-maxLongint);
         end;
         dmin:=min(x,dmin);
         dmax:=max(x,dmax);
         sum:=sum+x;
         sum2:=sum2+x*x;
         ni:=ni+1;
       end;
      end;
    end;
    FStreamValid:=false;
    Fmean:=sum/ni;
    Fsigma:=sqrt( (sum2/ni)-(Fmean*Fmean) );
    if dmin>=dmax then dmax:=dmin+1;
    FFitsInfo.dmin:=dmin;
    FFitsInfo.dmax:=dmax;
    GetImage;
 end;
end;

procedure TFits.Shift(dx,dy: double);
begin
  // for now use integer shift, next is to try with value_subpixel()
  ShiftInteger(round(dx),round(dy));
end;

procedure TFits.ShiftInteger(dx,dy: integer);
var imgshift: TFits;
    i,ii,j,k,x,y: integer;
begin
  imgshift:=TFits.Create(nil);
  imgshift.SetStream(FStream);
  imgshift.LoadStream;
  for k:=cur_axis-1 to cur_axis+n_axis-2 do begin
    for i:=0 to FFitsInfo.naxis2-1 do begin
     ii:=FFitsInfo.naxis2-1-i;
     for j := 0 to FFitsInfo.naxis1-1 do begin
       x:=j-dx;
       y:=ii-dy;
       if (x>0)and(x<FFitsInfo.naxis1)and(y>0)and(y<FFitsInfo.naxis2) then begin
         case FFitsInfo.bitpix of
          -64 : begin
                imgshift.imar64[k,ii,j]:=imar64[k,y,x];
                end;
          -32 : begin
                imgshift.imar32[k,ii,j]:=imar32[k,y,x];
                end;
            8 : begin
                imgshift.imai8[k,ii,j]:=imai8[k,y,x];
                end;
           16 : begin
                imgshift.imai16[k,ii,j]:=imai16[k,y,x];
                end;
           32 : begin
                imgshift.imai32[k,ii,j]:=imai32[k,y,x];
                end;
         end;
       end
       else begin
        case FFitsInfo.bitpix of
         -64 : begin
               imgshift.imar64[k,ii,j]:=-FFitsInfo.bzero;
               end;
         -32 : begin
               imgshift.imar32[k,ii,j]:=-FFitsInfo.bzero;
               end;
           8 : begin
               imgshift.imai8[k,ii,j]:=0;
               end;
          16 : begin
               imgshift.imai16[k,ii,j]:=-maxSmallint;
               end;
          32 : begin
               imgshift.imai32[k,ii,j]:=-maxLongint;
               end;
        end;
       end;
     end;
    end;
  end;
  imgshift.FStreamValid:=false;
  SetStream(imgshift.Stream);
  LoadStream;
  GetImage;
  imgshift.Free;
end;

end.
