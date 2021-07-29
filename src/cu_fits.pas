unit cu_fits;

{
Copyright (C) 2005-2020 Patrick Chevalley

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

//{$define debug_raw}

interface

uses SysUtils, Classes, LazFileUtils, u_utils, u_global, BGRABitmap, BGRABitmapTypes, ExpandedBitmap,
  GraphType,  FPReadJPEG, LazSysUtils, u_libraw, dateutils,
  LazUTF8, Graphics,Math, FPImage, Controls, LCLType, Dialogs, u_translation, IntfGraphics;

type

 TFitsInfo = record
            valid, solved, floatingpoint: boolean;
            bitpix,naxis,naxis1,naxis2,naxis3 : integer;
            Frx,Fry,Frwidth,Frheight,BinX,BinY: integer;
            bzero,bscale,dmax,dmin,blank : double;
            bayerpattern,roworder: string;
            bayeroffsetx, bayeroffsety: integer;
            rmult,gmult,bmult: double;
            equinox,ra,dec,crval1,crval2: double;
            pixsz1,pixsz2,pixratio,focallen,scale: double;
            exptime,airmass: double;
            objects,ctype1,ctype2 : string;
            frametype: TFrameType;
            procedure Assign(Source:TFitsInfo);
            end;

 THeaderBlock = array[1..36,1..80] of char;

 TStar = record
         x,y: double;
         hfd, fwhm: double;
         vmax, snr, bg: double;
         end;
 TStarList = array of TStar;

 Timabyte = array of array of array of byte;
 Timafloat = array of array of array of single;

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
         maxcropsizex = 3000;
         maxcropsizey = 2000;
         bottomup = 'BOTTOM-UP';
         topdown = 'TOP-DOWN';

type

  TFits = class(TComponent)
  private
    // Original Fits file
    FStream : TMemoryStream;
    // Fits read buffers
    d8  : array[1..2880] of byte;
    d16 : array[1..1440] of smallint;
    d32 : array[1..720] of Longword;
    // Original image data
    FUseRawImage: boolean;
    Frawimage: Timafloat;
    // 16bit image scaled min/max, unsigned, debayered
    Fimage : Timafloat;
    // Fimage scaling factor
    FimageC, FimageMin,FimageMax : double;
    FimageScaled: boolean;
    FimageDebayer : boolean;
    // Histogram of Fimage
    FHistogram: THistogram;
    // Fits header
    FHeader: TFitsHeader;
    // same as Fimage in TLazIntfImage format
    FIntfImg: TLazIntfImage;
    // Fits header values
    FFitsInfo : TFitsInfo;
    //
    n_plane,Fwidth,Fheight,Fhdr_end,colormode,Fpreview_axis : Integer;
    FTitle : string;
    Fmean,Fsigma,FFlatLevel : double;
    FVisuMin, FVisuMax: Word;
    FStreamValid,FImageValid: Boolean;
    Fbpm: TBpm;
    FBPMcount,FBPMnx,FBPMny,FBPMnax: integer;
    gamma_c : array[0..32768] of single; {prepared power values for gamma correction}
    FGamma: single;
    emptybmp:Tbitmap;
    FMarkOverflow,FDisableBayer: boolean;
    FMaxADU, FOverflow, FUnderflow: double;
    FInvert: boolean;
    FStarList: TStarList;
    FDark: TFits;
    FDarkOn: boolean;
    FDarkProcess, FBPMProcess, FPrivateDark: boolean;
    FonMsg: TNotifyMsg;
    ReadFitsCS : TRTLCriticalSection;
    procedure msg(txt: string; level:integer=3);
    procedure SetStream(value:TMemoryStream);
    function GetStream: TMemoryStream;
    procedure SetVideoStream(value:TMemoryStream);
    Procedure GetFlatLevel;
    Procedure ReadFitsImage;
    Procedure WriteFitsImage;
    function GammaCorr(value: Word):byte;
    function GetHasBPM: boolean;
    procedure ApplyBPM;
    procedure ApplyDark;
    procedure SetGamma(value: single);
    function GetBayerMode: TBayerMode;
    procedure GetBayerBgColor(t:TBayerMode; rmult,gmult,bmult:double; out r,g,b: single);
  protected
    { Protected declarations }
  public
    { Public declarations }
     constructor Create(AOwner:TComponent); override;
     destructor  Destroy; override;
     function  GetStatistics: string;
     Procedure LoadStream;
     Procedure MeasureFlatLevel;
     Procedure LoadRGB;
     procedure ClearFitsInfo;
     procedure GetFitsInfo;
     procedure CreateImage(info: TFitsInfo; hdr:TFitsHeader);
     procedure BayerInterpolation(t:TBayerMode; rmult,gmult,bmult:double; rbg,gbg,bbg:single; pix1,pix2,pix3,pix4,pix5,pix6,pix7,pix8,pix9:single; row,col:integer; out pixr,pixg,pixb:single); inline;
     Procedure Debayer;
     procedure GetExpBitmap(var bgra: TExpandedBitmap);
     procedure GetBGRABitmap(var bgra: TBGRABitmap);
     procedure SaveToBitmap(fn: string);
     procedure SaveToFile(fn: string; pack: boolean=false);
     procedure LoadFromFile(fn:string);
     procedure SetBPM(value: TBpm; count,nx,ny,nax:integer);
     procedure FreeDark;
     procedure ClearImage;
     procedure Math(operand: TFits; MathOperator:TMathOperator; new: boolean=false);
     procedure Shift(dx,dy: double);
     procedure ShiftInteger(dx,dy: integer);
     procedure Bitpix8to16;
     function  SameFormat(f:TFits): boolean;
     function  double_star(ri, x,y : integer):boolean;
     function  value_subpixel(x1,y1:double):double;
     procedure FindBrightestPixel(x,y,s,starwindow2: integer; out xc,yc:integer; out vmax: double; accept_double: boolean=true);
     procedure FindStarPos(x,y,s: integer; out xc,yc,ri:integer; out vmax,bg,sd: double);
     procedure GetHFD2(x,y,s: integer; out xc,yc,bg,sd,hfd,star_fwhm,valmax,snr,flux: double; strict_saturation: boolean=true);{han.k 2018-3-21}
     procedure GetStarList(rx,ry,s: integer);
     procedure MeasureStarList(s: integer; list: TArrayDouble2);
     procedure ClearStarList;
     procedure LoadDark(fn: string);
     property IntfImg: TLazIntfImage read FIntfImg;
     property Title : string read FTitle write FTitle;
     Property HeaderInfo : TFitsInfo read FFitsInfo;
     property Header: TFitsHeader read FHeader write FHeader;
     Property Stream : TMemoryStream read GetStream write SetStream;
     Property VideoStream : TMemoryStream write SetVideoStream;
     property Histogram : THistogram read FHistogram;
     property VisuMin : Word read FVisuMin write FVisuMin;
     property VisuMax : Word read FVisuMax write FVisuMax;
     property Gamma: single read FGamma write SetGamma;
     property ImageValid: boolean read FImageValid;
     property image : Timafloat read Fimage;
     property imageC : double read FimageC;
     property imageMin : double read FimageMin;
     property imageMax : double read FimageMax;
     property imageMean: double read Fmean;
     property imageSigma: double read Fsigma;
     property imageFlatLevel: double read FFlatLevel;
     property preview_axis: integer read Fpreview_axis;
     property MaxADU: double read FMaxADU write FMaxADU;
     property Invert: boolean read FInvert write FInvert;
     property MarkOverflow: boolean read FMarkOverflow write FMarkOverflow;
     property Overflow: double read FOverflow write FOverflow;
     property Underflow: double read FUnderflow write FUnderflow;
     property DisableBayer: boolean read FDisableBayer write FDisableBayer;
     property hasBPM: boolean read GetHasBPM;
     property BPMProcess: boolean read FBPMProcess;
     property StarList: TStarList read FStarList;
     property DarkProcess: boolean read FDarkProcess;
     property DarkOn: boolean read FDarkOn write FDarkOn;
     property DarkFrame: TFits read FDark write FDark;
     property PrivateDark: boolean read FPrivateDark write FPrivateDark;
     property onMsg: TNotifyMsg read FonMsg write FonMsg;
  end;

  TReadFits = class(TThread)
  public
    working: boolean;
    num, id: integer;
    fits: TFits;
    hist: THistogram;
    dmin,dmax : double;
    ni,sum,sum2 : extended;
    procedure Execute; override;
    constructor Create(CreateSuspended: boolean);
  end;

  TDebayerImage = class(TThread)
  public
    working: boolean;
    num, id: integer;
    fits: TFits;
    hist: THistogram;
    rmult,gmult,bmult: double;
    rbg,gbg,bbg: single;
    t: TBayerMode;
    procedure Execute; override;
    constructor Create(CreateSuspended: boolean);
  end;

  TGetBgraThread = class(TThread)
    public
      working: boolean;
      num, id: integer;
      fits: TFits;
      bgra: TBGRABitmap;
      HighOverflow,LowOverflow: TBGRAPixel;
      c,overflow,underflow: double;
      vmin: word;
      procedure Execute; override;
      constructor Create(CreateSuspended: boolean);
    end;

   TGetExpThread = class(TThread)
    public
      working: boolean;
      num, id: integer;
      minv,c: double;
      fits: TFits;
      bgra: TExpandedBitmap;
      procedure Execute; override;
      constructor Create(CreateSuspended: boolean);
    end;

    TGetStarList = class(TThread)
    public
      working: boolean;
      num, id: integer;
      fits: TFits;
      StarList: TStarList;
      rx,ry,overlap,s: integer;
      img_temp: Timabyte;
      procedure Execute; override;
      constructor Create(CreateSuspended: boolean);
    end;



  procedure PictureToFits(pict:TMemoryStream; ext: string; var ImgStream:TMemoryStream; flip:boolean=false;pix:double=-1;piy:double=-1;binx:integer=-1;biny:integer=-1;bayer:string='';rmult:string='';gmult:string='';bmult:string='';origin:string='';exifkey:TStringList=nil;exifvalue:TStringList=nil);
  procedure RawToFits(raw:TMemoryStream; ext: string; var ImgStream:TMemoryStream; out rmsg:string; pix:double=-1;piy:double=-1;binx:integer=-1;biny:integer=-1; flip:boolean=false);
  function PackFits(unpackedfilename,packedfilename: string; out rmsg:string):integer;
  function UnpackFits(packedfilename: string; var ImgStream:TMemoryStream; out rmsg:string):integer;

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
    i,p1,p2,n,ii : integer;
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
         p1:=9;
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
           if (keyword<>'')and(keyword<>'COMMENT')and(keyword<>'HISTORY') then
             ii:=FKeys.IndexOf(keyword)
           else
             ii:=-1;
           if ii<0 then begin
             FRows.add(row);
             FKeys.add(keyword);
             FValues.add(value);
             FComments.add(comment);
           end
           else begin
             FRows[ii]:=row;
             FKeys[ii]:=keyword;
             FValues[ii]:=value;
             FComments[ii]:=comment;
           end;
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
      p1:=9;
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

function CleanASCII(txt: string):string;
var i: integer;
begin
result:='';
for i:=1 to length(txt) do begin
  if (txt[i]>=#32)and(txt[i]<=#126) then
    result:=result+txt[i]
  else
    result:=result+blank;
end;
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
    ii: integer;
begin
 val:=CleanASCII(val);
 comment:=CleanASCII(comment);
 // The END keyword
 if (trim(key)='END') then begin
   row:=copy('END'+b80,1,80);
   val:='';
   comment:='';
 end
 // hierarch
 else if (trim(key)='HIERARCH') then begin
    row:=Format('%0:-8s',[key])+
         Format(' %0:-70s',[val]);
    if comment>'' then
       row:=row+Format(' / %0:-47s',[comment])
    else
       row:=row+b80;
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
         Format('= %0:-20s',[QuotedStr(val)]);
         if comment>'' then
            row:=row+Format(' / %0:-47s',[comment])
         else
            row:=row+b80;
 end
 // Other unquoted values
 else begin
    row:=Format('%0:-8s',[key])+
         Format('= %0:-20s',[val]);
         if comment>'' then
            row:=row+Format(' / %0:-47s',[comment])
         else
            row:=row+b80;
 end;
 row:=copy(row,1,80);
 // Search for existing key
 if (key<>'')and(key<>'COMMENT')and(key<>'HISTORY')and(key<>'HIERARCH') then
   ii:=FKeys.IndexOf(key)
 else
   ii:=-1;
 if ii>=0 then begin
    // replace existing key
    FRows[ii]:=row;
    FKeys[ii]:=key;
    FValues[ii]:=val;
    FComments[ii]:=comment;
    result:=ii;
 end
 else if idx>=0 then begin
    // insert key at position
    FRows.Insert(idx,row);
    FKeys.Insert(idx,key);
    FValues.Insert(idx,val);
    FComments.Insert(idx,comment);
    result:=idx;
 end else begin
    // add key at end
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

//////////////////// TFitsInfo /////////////////////////

procedure TFitsInfo.Assign(Source:TFitsInfo);
begin
  valid := Source.valid ;
  solved := Source.solved ;
  floatingpoint := Source.floatingpoint ;
  bitpix := Source.bitpix ;
  naxis := Source.naxis ;
  naxis1 := Source.naxis1 ;
  naxis2 := Source.naxis2 ;
  naxis3 := Source.naxis3 ;
  Frx := Source.Frx ;
  Fry := Source.Fry ;
  Frwidth := Source.Frwidth ;
  Frheight := Source.Frheight ;
  BinX := Source.BinX ;
  BinY := Source.BinY ;
  bzero := Source.bzero ;
  bscale := Source.bscale ;
  dmax := Source.dmax ;
  dmin := Source.dmin ;
  blank := Source.blank ;
  bayerpattern := Source.bayerpattern ;
  roworder := Source.roworder ;
  bayeroffsetx := Source.bayeroffsetx ;
  bayeroffsety := Source.bayeroffsety ;
  rmult := Source.rmult ;
  gmult := Source.gmult ;
  bmult := Source.bmult ;
  equinox := Source.equinox ;
  ra := Source.ra ;
  dec := Source.dec ;
  crval1 := Source.crval1 ;
  crval2 := Source.crval2 ;
  pixsz1 := Source.pixsz1 ;
  pixsz2 := Source.pixsz2 ;
  pixratio := Source.pixratio ;
  focallen := Source.focallen ;
  scale := Source.scale ;
  exptime := Source.exptime ;
  airmass := Source.airmass ;
  objects := Source.objects ;
  ctype1 := Source.ctype1 ;
  ctype2 := Source.ctype2 ;
  frametype := Source.frametype;
end;

//////////////////// TReadFits /////////////////////////

constructor TReadFits.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := False;
  inherited Create(CreateSuspended);
  working := True;
end;

procedure TReadFits.Execute;
var i,ii,j,npix,k,km,kk,streaminc : integer;
    streamstart,startline, endline, xs,ys: integer;
    x16,b16:smallint;
    x8,b8:byte;
    x : double;
    d8  : array[1..2880] of byte;
    d16 : array[1..1440] of smallint;
    d32 : array[1..720] of Longword;
    d64 : array[1..360] of Int64;
begin
// image size
xs:= fits.Fwidth;
ys:= fits.FHeight;
// height per thread
i := ys div num;
// this thread range
startline := id * i;
if id = (num - 1) then
  endline := ys - 1
else
  endline := (id + 1) * i - 1;
// start position of this range
streamstart:=fits.fhdr_end+round(xs*startline*abs(fits.FFitsInfo.bitpix/8));
dmin:=1.0E100;
dmax:=-1.0E100;
sum:=0; sum2:=0; ni:=0;
FillByte(hist,sizeof(THistogram),0);
npix:=0;
streaminc:=0;
b8:=round(fits.FFitsInfo.blank);
b16:=round(fits.FFitsInfo.blank);
try
case fits.FFitsInfo.bitpix of
    -64:for k:=0 to fits.n_plane-1 do begin
        for i:=startline to endline do begin
         ii:=ys-1-i;
         for j := 0 to xs-1 do begin
           if (npix mod 360 = 0) then begin
             EnterCriticalSection(fits.ReadFitsCS);
             try
             fits.FStream.Position:=streamstart+streaminc*sizeof(d64);
             fits.FStream.Read(d64,sizeof(d64));
             finally
               LeaveCriticalSection(fits.ReadFitsCS);
             end;
             inc(streaminc);
             npix:=0;
           end;
           inc(npix);
           x:=InvertF64(d64[npix]);
           if x=fits.FFitsInfo.blank then x:=0;
           x:=fits.FFitsInfo.bzero+fits.FFitsInfo.bscale*x;
           fits.Frawimage[k,ii,j] := x ;
           dmin:=min(x,dmin);
           dmax:=max(x,dmax);
           sum:=sum+x;
           sum2:=sum2+x*x;
           ni:=ni+1;
          end;
         end;
         end;
    -32: for k:=0 to fits.n_plane-1 do begin
        for i:=startline to endline do begin
         ii:=ys-1-i;
         for j := 0 to xs-1 do begin
           if (npix mod 720 = 0) then begin
             EnterCriticalSection(fits.ReadFitsCS);
             try
             fits.FStream.Position:=streamstart+streaminc*sizeof(d32);
             fits.FStream.Read(d32,sizeof(d32));
             finally
               LeaveCriticalSection(fits.ReadFitsCS);
             end;
             inc(streaminc);
             npix:=0;
           end;
           inc(npix);
           x:=InvertF32(d32[npix]);
           if x=fits.FFitsInfo.blank then x:=0;
           x:=fits.FFitsInfo.bzero+fits.FFitsInfo.bscale*x;
           fits.Frawimage[k,ii,j] := x ;
           dmin:=min(x,dmin);
           dmax:=max(x,dmax);
           sum:=sum+x;
           sum2:=sum2+x*x;
           ni:=ni+1;
         end;
         end;
         end;
     8 : if fits.colormode=1 then
        for k:=0 to fits.n_plane-1 do begin
        for i:=startline to endline do begin
         ii:=ys-1-i;
         for j := 0 to xs-1 do begin
           if (npix mod 2880 = 0) then begin
             EnterCriticalSection(fits.ReadFitsCS);
             try
             fits.FStream.Position:=streamstart+streaminc*sizeof(d8);
             fits.FStream.Read(d8,sizeof(d8));
             finally
               LeaveCriticalSection(fits.ReadFitsCS);
             end;
             inc(streaminc);
             npix:=0;
           end;
           inc(npix);
           x8:=d8[npix];
           if x8=b8 then x8:=0;
           x:=fits.FFitsInfo.bzero+fits.FFitsInfo.bscale*x8;
           if fits.FUseRawImage then fits.Frawimage[k,ii,j] := x;
           fits.Fimage[k,ii,j] := x;
           inc(hist[round(max(0,min(maxword,x)))]);
           dmin:=min(x,dmin);
           dmax:=max(x,dmax);
           sum:=sum+x;
           sum2:=sum2+x*x;
           ni:=ni+1;
         end;
         end;
         end else begin
          kk:=0;
          if fits.colormode=3 then begin  // output RGB from RGBA
             fits.n_plane:=4;
             kk:=1;
          end;
          for i:=startline to endline do begin
           ii:=ys-1-i;
           for j := 0 to xs-1 do begin
             for k:=fits.n_plane-1 downto 0 do begin
             if (npix mod 2880 = 0) then begin
               EnterCriticalSection(fits.ReadFitsCS);
               try
               fits.FStream.Position:=streamstart+streaminc*sizeof(d8);
               fits.FStream.Read(d8,sizeof(d8));
               finally
                 LeaveCriticalSection(fits.ReadFitsCS);
               end;
               inc(streaminc);
               npix:=0;
             end;
             inc(npix);
             km:=k-kk;
             if km<0 then continue; // skip A
             x8:=d8[npix];
             if x8=b8 then x8:=0;
             x:=fits.FFitsInfo.bzero+fits.FFitsInfo.bscale*x8;
             if fits.FUseRawImage then fits.Frawimage[km,ii,j] := x;
             fits.Fimage[km,ii,j] := x;
             inc(hist[round(max(0,min(maxword,x)))]);
             dmin:=min(x,dmin);
             dmax:=max(x,dmax);
             sum:=sum+x;
             sum2:=sum2+x*x;
             ni:=ni+1;
             end;
           end;
          end;
          if fits.colormode=3 then fits.n_plane:=3; // restore value
         end;

     16 : for k:=0 to fits.n_plane-1 do begin
        for i:=startline to endline do begin
         ii:=ys-1-i;
         for j := 0 to xs-1 do begin
           if (npix mod 1440 = 0) then begin
             EnterCriticalSection(fits.ReadFitsCS);
             try
             fits.FStream.Position:=streamstart+streaminc*sizeof(d16);
             fits.FStream.Read(d16,sizeof(d16));
             finally
               LeaveCriticalSection(fits.ReadFitsCS);
             end;
             inc(streaminc);
             npix:=0;
           end;
           inc(npix);
           x16:=BEtoN(d16[npix]);
           if x16=b16 then x16:=0;
           x:=fits.FFitsInfo.bzero+fits.FFitsInfo.bscale*x16;
           if fits.FUseRawImage then fits.Frawimage[k,ii,j] := x;
           fits.Fimage[k,ii,j] := x;
           inc(hist[round(max(0,min(maxword,x)))]);
           dmin:=min(x,dmin);
           dmax:=max(x,dmax);
           sum:=sum+x;
           sum2:=sum2+x*x;
           ni:=ni+1;
         end;
         end;
         end;
     32 : for k:=0 to fits.n_plane-1 do begin
        for i:=startline to endline do begin
         ii:=ys-1-i;;
         for j := 0 to xs-1 do begin
           if (npix mod 720 = 0) then begin
             EnterCriticalSection(fits.ReadFitsCS);
             try
             fits.FStream.Position:=streamstart+streaminc*sizeof(d32);
             fits.FStream.Read(d32,sizeof(d32));
             finally
               LeaveCriticalSection(fits.ReadFitsCS);
             end;
             inc(streaminc);
             npix:=0;
           end;
           inc(npix);
           x:=BEtoN(LongInt(d32[npix]));
           if x=fits.FFitsInfo.blank then x:=0;
           x:=fits.FFitsInfo.bzero+fits.FFitsInfo.bscale*x;
           fits.Frawimage[k,ii,j] := x;
           dmin:=min(x,dmin);
           dmax:=max(x,dmax);
           sum:=sum+x;
           sum2:=sum2+x*x;
           ni:=ni+1;
         end;
         end;
         end;
end;
finally
working := False;
end;
end;

//////////////////// TDebayerImage /////////////////////////

constructor TDebayerImage.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := False;
  inherited Create(CreateSuspended);
  working := True;
end;

procedure TDebayerImage.Execute;
var
  i, j, startline, endline, xs,ys: integer;
  i1,i2,i3,j1,j2,j3 : integer;
  pixr,pixg,pixb:single;
begin
xs:= fits.Fwidth;
ys:= fits.FHeight;
i := ys div num;
startline := id * i;
if id = (num - 1) then
  endline := ys - 1
else
  endline := (id + 1) * i - 1;
FillByte(hist,sizeof(THistogram),0);
// process the rows range for this thread
for i:=startline to endline do begin
  i1:=max(i-1,0);
  i2:=i;
  i3:= min(i+1,ys-1);
  for j := 0 to xs-1 do begin
     j1:=max(j-1,0);
     j2:=j;
     j3:=min(j+1,xs-1);
     fits.BayerInterpolation(t,rmult,gmult,bmult,rbg,gbg,bbg,
          fits.Frawimage[0,i1,j1],fits.Frawimage[0,i1,j2],fits.Frawimage[0,i1,j3],
          fits.Frawimage[0,i2,j1],fits.Frawimage[0,i2,j2],fits.Frawimage[0,i2,j3],
          fits.Frawimage[0,i3,j1],fits.Frawimage[0,i3,j2],fits.Frawimage[0,i3,j3],
          i,j,pixr,pixg,pixb);
     inc(hist[round(pixr)]);
     inc(hist[round(pixg)]);
     inc(hist[round(pixb)]);
     fits.Fimage[0,i,j]:=pixr;
     fits.Fimage[1,i,j]:=pixg;
     fits.Fimage[2,i,j]:=pixb;
  end;
end;
working := False;
end;

//////////////////// TGetBgraThread /////////////////////////

constructor TGetBgraThread.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
  working := True;
end;

procedure TGetBgraThread.Execute;
var
  i, j, startline, endline, xs,ys: integer;
  x : word;
  xx,xxg,xxb: extended;
  p: PBGRAPixel;

begin
xs:= fits.Fwidth;
ys:= fits.FHeight;
i := ys div num;
startline := id * i;
if id = (num - 1) then
  endline := ys - 1
else
  endline := (id + 1) * i - 1;
// process the rows range for this thread
for i:=startline to endline do begin
   p := bgra.Scanline[i];
   for j := 0 to xs-1 do begin
       if fits.preview_axis=3 then begin
         // 3 chanel color image
         xx:=fits.Fimage[0,i,j];
         x:=round(max(0,min(MaxWord,(xx-vmin) * c )) );
         p^.red:=fits.GammaCorr(x);
         xxg:=fits.Fimage[1,i,j];
         x:=round(max(0,min(MaxWord,(xxg-vmin) * c )) );
         p^.green:=fits.GammaCorr(x);
         xxb:=fits.Fimage[2,i,j];
         x:=round(max(0,min(MaxWord,(xxb-vmin) * c )) );
         p^.blue:=fits.GammaCorr(x);
         if fits.MarkOverflow then begin
           if maxvalue([xx,xxg,xxb])>=overflow then
             p^:=HighOverflow
           else if minvalue([xx,xxg,xxb])<=underflow then
             p^:=LowOverflow;
         end;
       end else begin
           // B/W image
           xx:=fits.Fimage[0,i,j];
           x:=round(max(0,min(MaxWord,(xx-vmin) * c )) );
           p^.red:=fits.GammaCorr(x);
           p^.green:=p^.red;
           p^.blue:=p^.red;
           if fits.MarkOverflow then begin
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
working := False;
end;

//////////////////// TGetExpThread /////////////////////////

constructor TGetExpThread.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
  working := True;
  minv:=0;
  c:=1;
end;

procedure TGetExpThread.Execute;
var
  i, j, startline, endline, xs,ys: integer;
  x : word;
  xx,xxg,xxb: extended;
  p: PExpandedPixel;

begin
xs:= fits.Fwidth;
ys:= fits.FHeight;
i := ys div num;
startline := id * i;
if id = (num - 1) then
  endline := ys - 1
else
  endline := (id + 1) * i - 1;
// process the rows range for this thread
for i:=startline to endline do begin
   p := bgra.Scanline[i];
   for j := 0 to xs-1 do begin
       if fits.preview_axis=3 then begin
         // 3 chanel color image
         xx:=(fits.Fimage[0,i,j]-minv)*c;
         x:=round(max(0,min(MaxWord,xx)) );
         p^.red:=x;
         xxg:=(fits.Fimage[1,i,j]-minv)*c;
         x:=round(max(0,min(MaxWord,xxg)) );
         p^.green:=x;
         xxb:=(fits.Fimage[2,i,j]-minv)*c;
         x:=round(max(0,min(MaxWord,xxb)) );
         p^.blue:=x;
       end else begin
           // B/W image
           xx:=(fits.Fimage[0,i,j]-minv)*c;
           x:=round(max(0,min(MaxWord,xx)));
           p^.red:=x;
           p^.green:=x;
           p^.blue:=x;
       end;
       p^.alpha:=65535;
       inc(p);
   end;
end;
working := False;
end;

//////////////////// TGetStarList /////////////////////////

constructor TGetStarList.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := false;
  inherited Create(CreateSuspended);
  working := True;
end;

procedure TGetStarList.Execute;
var
  i, j, starty, endy, ss, xs,ys: integer;
  fitsX,fitsY,fx,fy,nhfd,marginx,marginy,xci,yci,n,m,radius,sqr_radius: integer;
  hfd1,star_fwhm,vmax,bg,bgdev,xc,yc,snr,flux: double;
begin
  xs:= fits.Fwidth;
  ys:= fits.FHeight;
  // step size
  ss := ry div num div s;
  marginx:=(xs-rx)div 2 div s;
  marginy:=(ys-ry)div 2 div s;
  // range for current thread
  starty := marginy + id * ss;
  if id=(num-1) then
    endy := ((ys) div s)-marginy
  else
    endy := starty + ss;

  nhfd:=0;{set counters at zero}
  SetLength(StarList,1000);{allocate initial size}
  // process the rows range for this thread
  for fy:=starty to endy do { move test box with stepsize rs around}
   begin
     fitsY:=fy*s;
     for fx:=marginx to ((xs) div s)-marginx do
     begin
       fitsX:=fx*s;

       fits.GetHFD2(fitsX,fitsY,s+overlap,xc,yc,bg,bgdev,hfd1,star_fwhm,vmax,snr,flux,false);{2018-3-21, calculate HFD}

       {check valid hfd }
       if ((hfd1>0)and (Undersampled or (hfd1>0.7)))
          and (hfd1<99)
          and (img_temp[0,round(xc),round(yc)]=0)  {area not surveyed}
          and (snr>AutofocusMinSNR)  {minimal star detection level, also detect saturation}
       then
       begin
         inc(nhfd);
         if nhfd>=Length(StarList) then
            SetLength(StarList,nhfd+1000);  {get more space to store values}
         StarList[nhfd-1].x:=xc;
         StarList[nhfd-1].y:=yc;
         StarList[nhfd-1].hfd:=hfd1;
         StarList[nhfd-1].fwhm:=star_fwhm;
         StarList[nhfd-1].snr:=snr;
         StarList[nhfd-1].vmax:=vmax;
         StarList[nhfd-1].bg:=bg;
         radius:=round(3.0*hfd1);{for marking star area. A value between 2.5*hfd and 3.5*hfd gives same performance. Note in practice a star PSF has larger wings then predicted by a Gaussian function}
         sqr_radius:=sqr(radius);
         xci:=round(xc);{star center as integer}
         yci:=round(yc);
         for n:=-radius to +radius do {mark the whole circular star area as occupied to prevent double detection's}
           for m:=-radius to +radius do
           begin
             j:=n+yci;
             i:=m+xci;
             if ((j>=0) and (i>=0) and (j<ys) and (i<xs) and (sqr(m)+sqr(n)<=sqr_radius)) then
              img_temp[0,i,j]:=1;
           end;

      end;
     end;
   end;
   SetLength(StarList,nhfd);  {set length to new number of elements}
   working:=false;
end;

//////////////////// TFits /////////////////////////

constructor TFits.Create(AOwner:TComponent);
begin
inherited Create(AOwner);
Fheight:=0;
Fwidth:=0;
FVisuMin:=0;
FVisuMax:=MaxWord;
FimageMin:=0;
FimageMax:=0;
Fmean:=0;
Fsigma:=0;
FBPMcount:=0;
FBPMProcess:=false;
FDarkProcess:=false;
FDarkOn:=false;
FPrivateDark:=false;
FStreamValid:=false;
FImageValid:=false;
FMarkOverflow:=false;
FDisableBayer:=false;
FMaxADU:=MAXWORD;
FOverflow:=MAXWORD;
FUnderflow:=0;
FInvert:=false;
ClearFitsInfo;
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
setlength(Frawimage,0,0,0);
setlength(Fimage,0,0,0);
FHeader.Free;
if FStream<>nil then FreeAndNil(FStream);
FIntfImg.Free;
emptybmp.Free;
FreeDark;
inherited destroy;
except
//writeln('error destroy '+name);
end;
end;

procedure TFits.msg(txt: string; level:integer=3);
begin
 if Assigned(FonMsg) then FonMsg('FITS: '+txt,level);
end;

procedure TFits.SetVideoStream(value:TMemoryStream);
var buf: array[0..80] of char;
begin
FImageValid:=false;
setlength(Frawimage,0,0,0);
setlength(Fimage,0,0,0);
if FStream<>nil then FreeAndNil(FStream);
FStream:=value;
FStream.Position:=0;
FStream.Read(buf,80);
FStream.Position:=0;
if copy(buf,1,6)='SIMPLE' then begin
  // read header from stream
  Fhdr_end:=FHeader.ReadHeader(FStream);
  GetFitsInfo;
end
else begin
  // header already set by caller, stream contain only image data
  Fhdr_end:=0;
end;
ReadFitsImage;
end;

procedure TFits.SetStream(value:TMemoryStream);
begin
try
 FImageValid:=false;
 ClearFitsInfo;
 if FStream<>nil then FreeAndNil(FStream);
 FStream:=value;
 Fhdr_end:=FHeader.ReadHeader(FStream);
 FStreamValid:=true;
except
 ClearFitsInfo;
end;
end;

Procedure TFits.LoadStream;
begin
  GetFitsInfo;
  if FFitsInfo.valid then begin
    if FFitsInfo.frametype=FLAT then GetFlatLevel;
    ReadFitsImage;
  end;
end;

Procedure TFits.MeasureFlatLevel;
begin
  GetFitsInfo;
  if FFitsInfo.valid then begin
    GetFlatLevel;
  end;
end;

Procedure TFits.LoadRGB;
var i: integer;
begin
// reload stream from debayered image data
  if preview_axis=3 then begin
    // set new header
    FHeader.Insert(-1,'NAXIS',3,'');
    i:=FHeader.Indexof('NAXIS2')+1;
    FHeader.Insert(i,'NAXIS3',3,'');
    i:=FHeader.Indexof('BAYERPAT');
    if i>0 then FHeader.Delete(i);
    i:=FHeader.Indexof('XBAYROFF');
    if i>0 then FHeader.Delete(i);
    i:=FHeader.Indexof('YBAYROFF');
    if i>0 then FHeader.Delete(i);
    i:=FHeader.Indexof('MULT_R');
    if i>0 then FHeader.Delete(i);
    i:=FHeader.Indexof('MULT_G');
    if i>0 then FHeader.Delete(i);
    i:=FHeader.Indexof('MULT_B');
    if i>0 then FHeader.Delete(i);
    // reload header
    GetFitsInfo;
    // reload stream
    WriteFitsImage;
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

procedure TFits.SaveToFile(fn: string; pack: boolean=false);
var mem: TMemoryStream;
    tmpf,rmsg: string;
    i: integer;
begin
  mem:=GetStream;
  if pack then begin
    tmpf:=slash(TmpDir)+'tmppack.fits';
    mem.SaveToFile(tmpf);
    i:=PackFits(tmpf,fn+'.fz',rmsg);
    if i<>0 then begin
      msg('fpack error '+inttostr(i)+': '+rmsg,1);
      msg('Saving file without compression',1);
      mem.SaveToFile(fn);
    end;
  end
  else begin
    mem.SaveToFile(fn);
  end;
  mem.Free;
end;

procedure TFits.LoadFromFile(fn:string);
var mem: TMemoryStream;
    pack: boolean;
    rmsg: string;
    i: integer;
begin
if FileExistsUTF8(fn) then begin
   mem:=TMemoryStream.Create;
   pack:=uppercase(ExtractFileExt(fn))='.FZ';
   if pack then begin
     i:=UnpackFits(fn,mem,rmsg);
     if i<>0 then begin
       ClearImage;
       msg('funpack error '+inttostr(i)+': '+rmsg,1);
       exit;
     end;
   end
   else
     mem.LoadFromFile(fn);
   SetBPM(bpm,0,0,0,0);
   FDarkOn:=false;
   SetStream(mem);
   LoadStream;
end
else begin
 ClearImage;
 msg(Format(rsFileNotFound, [fn]),1);
end;
end;

function TFits.GetStatistics: string;
var ff: string;
    x: double;
    i,maxh,maxp,sz,sz2,npx,median:integer;
begin
  if FFitsInfo.valid then begin
    if FFitsInfo.bitpix>0 then ff:=f0 else ff:=f6;
    sz:=Fwidth*Fheight;
    sz2:=sz div 2;
    result:=rsImageStatist+crlf;
    result:=Format(rsPixelCount, [result, blank+IntToStr(sz)+crlf]);
    // min, max
    result:=result+rsMin2+blank+FormatFloat(ff,FFitsInfo.dmin)+crlf;
    result:=result+rsMax+blank+FormatFloat(ff,FFitsInfo.dmax)+crlf;
    // mode, median
    median:=0; maxh:=0;  npx:=0; maxp:=0;
    for i:=0 to high(word) do begin
      npx:=npx+FHistogram[i];
      if (median=0) and (npx>sz2) then
          median:=i;
      if FHistogram[i]>maxh then begin
          maxh:=FHistogram[i];
          maxp:=i;
      end;
    end;
    if maxh>0 then begin
      x:= FimageMin+maxp/FimageC;
      result:=result+rsMode+blank+FormatFloat(ff, x)+crlf;
    end;
    if median>0 then begin
      x:= FimageMin+median/FimageC;
      result:=Format(rsMedian, [result, blank+FormatFloat(ff, x)+crlf]);
    end;
    // mean
    result:=result+rsMean+blank+FormatFloat(f1, Fmean)+crlf;
    // sigma
    result:=result+rsStdDev+blank+FormatFloat(f1, Fsigma)+crlf;
  end
  else
    result:='';
end;

procedure TFits.ClearFitsInfo;
begin
with FFitsInfo do begin
   valid:=false; solved:=false; floatingpoint:=false;
   bitpix:=0; naxis:=0; naxis1:=0; naxis2:=0; naxis3:=1;
   Frx:=-1; Fry:=-1; Frwidth:=0; Frheight:=0; BinX:=1; BinY:=1;
   bzero:=0; bscale:=1; dmax:=0; dmin:=0; blank:=0;
   bayerpattern:=''; roworder:='';
   bayeroffsetx:=0; bayeroffsety:=0;
   rmult:=0; gmult:=0; bmult:=0;
   equinox:=2000; ra:=NullCoord; dec:=NullCoord; crval1:=NullCoord; crval2:=NullCoord;
   pixsz1:=0; pixsz2:=0; pixratio:=1; focallen:=0; scale:=0;
   exptime:=0; airmass:=0;
   objects:=''; ctype1:=''; ctype2:='';
   frametype:=LIGHT;
end;
end;

procedure TFits.GetFitsInfo;
var   i : integer;
      keyword,buf : string;
begin
 ClearFitsInfo;
 with FFitsInfo do begin
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
    if (keyword='EXPTIME') then exptime:=strtofloat(buf);
    if (keyword='XPIXSZ') then pixsz1:=strtofloat(buf);
    if (keyword='YPIXSZ') then pixsz2:=strtofloat(buf);
    if (keyword='XBINNING') then BinX:=round(StrToFloat(buf));
    if (keyword='YBINNING') then BinY:=round(StrToFloat(buf));
    if (keyword='FRAMEX') then Frx:=round(StrToFloat(buf));
    if (keyword='FRAMEY') then Fry:=round(StrToFloat(buf));
    if (keyword='FRAMEHGT') then Frheight:=round(StrToFloat(buf));
    if (keyword='FRAMEWDH') then Frwidth:=round(StrToFloat(buf));
    if (keyword='BAYERPAT') then bayerpattern:=trim(buf);
    if (keyword='ROWORDER') then roworder:=trim(buf);
    if (keyword='XBAYROFF') then bayeroffsetx:=round(StrToFloat(buf));
    if (keyword='YBAYROFF') then bayeroffsety:=round(StrToFloat(buf));
    if (keyword='MULT_R') then rmult:=strtofloat(buf);
    if (keyword='MULT_G') then gmult:=strtofloat(buf);
    if (keyword='MULT_B') then bmult:=strtofloat(buf);
    if (keyword='AIRMASS') then airmass:=strtofloat(buf);
    if (keyword='OBJECT') then objects:=trim(buf);
    if (keyword='RA') then ra:=StrToFloatDef(buf,NullCoord);
    if (keyword='DEC') then dec:=StrToFloatDef(buf,NullCoord);
    if (keyword='EQUINOX') then equinox:=StrToFloatDef(buf,2000);
    if (keyword='CTYPE1') then ctype1:=buf;
    if (keyword='CTYPE2') then ctype2:=buf;
    if (keyword='CRVAL1') then crval1:=strtofloat(buf);
    if (keyword='CRVAL2') then crval2:=strtofloat(buf);
    if (keyword='SCALE')  then scale:=strtofloat(buf);
    if (scale=0) and (keyword='SECPIX1')then scale:=strtofloat(buf);
    if (keyword='FRAME')or(keyword='IMAGETYP') then frametype:=Str2Frametype(buf);
    if (keyword='A_ORDER') or
       (keyword='AMDX1') or
       (keyword='CD1_1')
        then solved:=true; // the image must be astrometry solved.
 end;
 if (pixsz1<>0)and(pixsz2<>0) then pixratio:=pixsz1/pixsz2;
 valid:=valid and (naxis>0); // do not process file without primary array
 if not valid then exit;
 floatingpoint:=bitpix<0;
 // very crude coordinates to help astrometry if telescope is not available
 if ra=NullCoord then begin
   if (copy(ctype1,1,3)='RA-')and(crval1<>NullCoord) then
      ra:=crval1/15;
 end;
 if dec=NullCoord then begin
   if (copy(ctype2,1,4)='DEC-')and(crval2<>NullCoord) then
      dec:=crval2;
 end;
 // remove unsupported ASCOM SensorType for debayering. Correct SensorType is still written in header for further processing
 if (bayerpattern='CMYG') or
    (bayerpattern='CMYG2') or
    (bayerpattern='LRGB')
    then
      bayerpattern:='UNSUPPORTED';
 // default roworder support most windows capture software and to not break bayerpattern from version before 0.9.72
 if roworder='' then roworder:=topdown;
 // set color image type
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
 if (naxis=3)and(naxis3=3) then n_plane:=3 else n_plane:=1;
end;
end;

procedure TFits.CreateImage(info: TFitsInfo; hdr:TFitsHeader);
begin
 FFitsInfo.Assign(info);
 FHeader.Assign(hdr);
 // set color image type
 colormode:=1;
 with FFitsInfo do begin
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
   if (naxis=3)and(naxis3=3) then n_plane:=3 else n_plane:=1;
 end;
 Fheight:=FFitsInfo.naxis2;
 Fwidth :=FFitsInfo.naxis1;
 setlength(Fimage,n_plane,Fheight,Fwidth);
end;

Procedure TFits.GetFlatLevel;
var sum,flatlimit : extended;
    i,j,w,h : integer;
    startline,endline,startcol,cropsizex,cropsizey,rowlen: integer;
    x16,b16:smallint;
    x : double;
    d16 : array[1..maxcropsizex] of smallint;
begin
// measure central part of image for flat level
FFlatLevel:=0;
{$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'MeasureFitsImage');{$endif}
if FFitsInfo.naxis1=0 then exit;
if (FFitsInfo.naxis1>maxl)or(FFitsInfo.naxis2>maxl) then
  raise exception.Create(Format('Image too big! limit is currently %dx%d %sPlease open an issue to request an extension.',[maxl,maxl,crlf]));
// only 16 bit B/W images
if (FFitsInfo.bitpix<>16)or(n_plane<>1) then
   exit;
// image size
w:=FFitsInfo.naxis1;
h:=FFitsInfo.naxis2;
// size of central crop
cropsizex:=min(maxcropsizex,w div 2);
cropsizey:=min(maxcropsizey,h div 2);
startline := (h-cropsizey) div 2;
endline := startline+cropsizey;
startcol := (w-cropsizex) div 2;
rowlen:=sizeof(SmallInt)*cropsizex;
b16:=round(FFitsInfo.blank);
FillByte(FHistogram,sizeof(THistogram),0);
// fill histogram
for i:=startline to endline do begin
   FStream.Position:=fhdr_end+(w*i+startcol)*sizeof(SmallInt);
   FStream.Read(d16,rowlen);
   for j := 1 to cropsizex do begin
     x16:=BEtoN(d16[j]);
     if x16=b16 then x16:=0;
     x:=FFitsInfo.bzero+FFitsInfo.bscale*x16;
     inc(FHistogram[round(max(0,min(maxword,x)))]);
   end;
end;
// get level at 90%
flatlimit:=0.9*cropsizex*cropsizey;
sum:=0;
for i:=0 to high(word) do begin
  sum:=sum+FHistogram[i];
  if sum>=flatlimit then begin
    FFlatLevel:=i;
    break;
  end;
end;
end;

Procedure TFits.ReadFitsImage;
var i,j,k : integer;
    dmin,dmax : double;
    ni,sum,sum2 : extended;
    working, timingout: boolean;
    timelimit: TDateTime;
    thread: array[0..15] of TReadFits;
    tc,timeout: integer;
begin
{$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'ReadFitsImage');{$endif}
FImageValid:=false;
if FFitsInfo.naxis1=0 then exit;
FDarkProcess:=false;
FBPMProcess:=false;
if (FFitsInfo.naxis1>maxl)or(FFitsInfo.naxis2>maxl) then
  raise exception.Create(Format('Image too big! limit is currently %dx%d %sPlease open an issue to request an extension.',[maxl,maxl,crlf]));
Fheight:=FFitsInfo.naxis2;
Fwidth :=FFitsInfo.naxis1;
// do not scale 8 or 16 bit images
FimageScaled:=(FFitsInfo.bscale<>1)or((FFitsInfo.bitpix<>16)and(FFitsInfo.bitpix<>8));
// debayer supported for this image
FimageDebayer:=BayerColor and (n_plane=1) and (not FDisableBayer) and (GetBayerMode<>bayerUnsupported) and (not FimageScaled);
// fill raw image only if debayer or scaled
FUseRawImage:=FimageScaled or FimageDebayer;
if FUseRawImage then
  setlength(Frawimage,n_plane,Fheight,Fwidth)
else
  setlength(Frawimage,0,0,0);
// initialize image
setlength(Fimage,n_plane,Fheight,Fwidth);
FillByte(FHistogram,sizeof(THistogram),0);
dmin:=1.0E100;
dmax:=-1.0E100;
sum:=0; sum2:=0; ni:=0;

// number of thread
if n_plane>1 then
  tc := 1
else begin
  tc := max(1,min(16, MaxThreadCount)); // based on number of core
  tc := max(1,min(tc,Fheight div 100)); // do not split the image too much
end;
InitCriticalSection(ReadFitsCS);
// start thread
for i := 0 to tc - 1 do
begin
  thread[i] := TReadFits.Create(True);
  thread[i].fits := self;
  thread[i].num := tc;
  thread[i].id := i;
  thread[i].Start;
end;
// wait complete
timeout:=60;
timelimit := now + timeout / secperday;
repeat
  sleep(100);
  working := False;
  for i := 0 to tc - 1 do
    working := working or thread[i].working;
  timingout := (now > timelimit);
until (not working) or timingout;
// total statistics and histogram
for i:=0 to tc - 1 do begin
  dmin:=min(thread[i].dmin,dmin);
  dmax:=max(thread[i].dmax,dmax);
  sum:=sum+thread[i].sum;
  sum2:=sum2+thread[i].sum2;
  ni:=ni+thread[i].ni;
  for j:=0 to high(word) do begin
     FHistogram[j]:=FHistogram[j]+thread[i].hist[j];
  end;
end;
// cleanup
DoneCriticalSection(ReadFitsCS);
for i := 0 to tc - 1 do begin
  thread[i].Free;
end;

FStreamValid:=true;
Fmean:=sum/ni;
Fsigma:=sqrt( (sum2/ni)-(Fmean*Fmean) );
if dmin>=dmax then begin
   if dmin=0 then
     dmax:=dmin+1  // black if all 0
   else
     dmin:=dmax-1; // white if all same value
end;
if (FFitsInfo.dmin=0)and(FFitsInfo.dmax=0) then begin  // do not replace existing header data range
  FFitsInfo.dmin:=dmin;
  FFitsInfo.dmax:=dmax;
end;
SetLength(FStarList,0); {reset object list}
{$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'GetImage');{$endif}
fpreview_axis:=FFitsInfo.naxis3;
ApplyDark;
if FimageScaled then begin
  FimageMin:=FFitsInfo.dmin;
  FimageMax:=FFitsInfo.dmax;
end
else begin
  FimageMin:=0;
  if FFitsInfo.bitpix=8 then
    FimageMax:=MaxByte
  else
    FimageMax:=MaxWord;
end;
if FimageMax>FimageMin then
  FimageC:=MaxWord/(FimageMax-FimageMin)
else
  FimageC:=1;
if FimageMin<0 then FimageMin:=0;
ApplyBPM;
if FimageDebayer then begin
   Debayer;
end
else begin
  if FimageScaled then begin
    FillByte(FHistogram,sizeof(THistogram),0);
    for k:=0 to n_plane-1 do begin
       for i:=0 to FFitsInfo.naxis2-1 do begin
         for j := 0 to FFitsInfo.naxis1-1 do begin
            Fimage[k,i,j]:=(Frawimage[k,i,j]-FimageMin)*FimageC;
            inc(FHistogram[round(max(0,min(maxword,Fimage[k,i,j])))])
         end;
       end;
    end;
  end;
end;
FUseRawImage:=false;
setlength(Frawimage,0,0,0);
{$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'GetImage end');{$endif}
FImageValid:=true;
end;

Procedure TFits.WriteFitsImage;
var hdrmem: TMemoryStream;
    i,j,k,ii,npix: integer;
    x:double;
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
          for k:=0 to n_plane-1 do begin
          for i:=0 to FFitsInfo.naxis2-1 do begin
           ii:=FFitsInfo.naxis2-1-i;
           for j := 0 to FFitsInfo.naxis1-1 do begin
             if (npix mod 1440 = 0) then begin
               if not first then FStream.Write(d8,sizeof(d8));
               FillByte(d8,sizeof(d8),0);
               npix:=0;
               first:=false;
             end;
             inc(npix);
             x:=max(min(round((Fimage[k,ii,j]-FFitsInfo.bzero)/FFitsInfo.bscale),MAXBYTE),0);
             d8[npix]:=byte(round(x));
           end;
           end;
           end;
           if npix>0 then  FStream.Write(d8,sizeof(d8));
           end;
     16 : begin
          for k:=0 to n_plane-1 do begin
          for i:=0 to FFitsInfo.naxis2-1 do begin
           ii:=FFitsInfo.naxis2-1-i;
           for j := 0 to FFitsInfo.naxis1-1 do begin
             if (npix mod 1440 = 0) then begin
               if not first then FStream.Write(d16,sizeof(d16));
               FillByte(d16,sizeof(d16),0);
               npix:=0;
               first:=false;
             end;
             inc(npix);
             x:=max(min(round((Fimage[k,ii,j]-FFitsInfo.bzero)/FFitsInfo.bscale),maxSmallint),-maxSmallint-1);
             d16[npix]:=NtoBE(smallint(round(x)));
           end;
           end;
           end;
           if npix>0 then  FStream.Write(d16,sizeof(d16));
           end;
     32 : begin
          for k:=0 to n_plane-1 do begin
          for i:=0 to FFitsInfo.naxis2-1 do begin
           ii:=FFitsInfo.naxis2-1-i;
           for j := 0 to FFitsInfo.naxis1-1 do begin
             if (npix mod 1440 = 0) then begin
               if not first then FStream.Write(d32,sizeof(d32));
               FillByte(d32,sizeof(d32),0);
               npix:=0;
               first:=false;
             end;
             inc(npix);
             x:= max(min(round((Fimage[k,ii,j]-FFitsInfo.bzero)/FFitsInfo.bscale),maxLongint),-maxLongint-1);
             d32[npix]:=NtoBE(Longword(round(x)));
           end;
           end;
           end;
           if npix>0 then  FStream.Write(d32,sizeof(d32));
           end;
  end;
end;

procedure TFits.Debayer;
var i,j: integer;
    working, timingout: boolean;
    timelimit: TDateTime;
    thread: array[0..15] of TDebayerImage;
    tc,timeout: integer;
    rmult,gmult,bmult,mx: double;
    offsety: integer;
    rbg,gbg,bbg,bgm: single;
    t: TBayerMode;
begin
  t:=GetBayerMode;
  rmult:=0; gmult:=0; bmult:=0; rbg:=0; gbg:=0; bbg:=0;
  fpreview_axis:=3;
  if (BalanceFromCamera)and(FFitsInfo.rmult>0)and(FFitsInfo.gmult>0)and(FFitsInfo.bmult>0) then begin
   rmult:=FFitsInfo.rmult;
   gmult:=FFitsInfo.gmult;
   bmult:=FFitsInfo.bmult;
  end else begin
   mx:=minvalue([RedBalance,GreenBalance,BlueBalance]);
   rmult:=RedBalance/mx;
   gmult:=GreenBalance/mx;
   bmult:=BlueBalance/mx;
  end;
  if ((FFitsInfo.bayeroffsetx mod 2) = 1) and (not odd(Fwidth)) then begin
   case t of
     bayerGR: t:=bayerRG;
     bayerRG: t:=bayerGR;
     bayerBG: t:=bayerGB;
     bayerGB: t:=bayerBG;
   end;
  end;
  offsety:=FFitsInfo.bayeroffsety;
  if FFitsInfo.roworder<>bottomup then offsety:=(offsety+1) mod 2;
  if ((offsety mod 2) = 1) and (not odd(Fheight)) then begin
   case t of
     bayerGR: t:=bayerBG;
     bayerRG: t:=bayerGB;
     bayerBG: t:=bayerGR;
     bayerGB: t:=bayerRG;
   end;
  end;
  setlength(Fimage,preview_axis,Fheight,Fwidth);
  FillByte(FHistogram,sizeof(THistogram),0);

  if BGneutralization then begin
    GetBayerBgColor(t,rmult,gmult,bmult,rbg,gbg,bbg);
    bgm:=MaxValue([rbg,gbg,bbg]);
    rbg:=rbg-bgm;
    gbg:=gbg-bgm;
    bbg:=bbg-bgm;
  end;

  thread[0]:=nil;
  // number of thread
   tc := max(1,min(16, MaxThreadCount)); // based on number of core
   tc := max(1,min(tc,Fheight div 100)); // do not split the image too much
  // start thread
  for i := 0 to tc - 1 do
  begin
    thread[i] := TDebayerImage.Create(True);
    thread[i].fits := self;
    thread[i].num := tc;
    thread[i].id := i;
    thread[i].rmult := rmult;
    thread[i].gmult := gmult;
    thread[i].bmult := bmult;
    thread[i].rbg := rbg;
    thread[i].gbg := gbg;
    thread[i].bbg := bbg;
    thread[i].t := t;
    thread[i].Start;
  end;
  // wait complete
  timeout:=60;
  timelimit := now + timeout / secperday;
  repeat
    sleep(100);
    working := False;
    for i := 0 to tc - 1 do
      working := working or thread[i].working;
    timingout := (now > timelimit);
  until (not working) or timingout;
  // total histogram
  for i:=0 to tc - 1 do begin
    for j:=0 to high(word) do begin
       FHistogram[j]:=FHistogram[j]+thread[i].hist[j];
    end;
  end;
  // cleanup
  for i := 0 to tc - 1 do thread[i].Free;
end;

procedure TFits.LoadDark(fn: string);
begin
  FreeDark;
  FDark:=TFits.Create(nil);
  FDark.onMsg:=FonMsg;
  PrivateDark:=true;
  FDark.LoadFromFile(fn);
end;

procedure TFits.FreeDark;
begin
  if FPrivateDark and (FDark<>nil) then FreeAndNil(FDark);
end;

procedure TFits.ApplyDark;
begin
if (FDarkOn)and(FDark<>nil)and(SameFormat(FDark))
   then begin
    {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'apply dark');{$endif}
     Math(FDark,moSub);
     FDarkProcess:=true;
     FHeader.Insert( FHeader.Indexof('END'),'COMMENT','Dark substracted','');
   end;
end;

procedure TFits.ApplyBPM;
var i,x,y,x0,y0: integer;
begin
if (FBPMcount>0)and(FBPMnax=FFitsInfo.naxis) then begin
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'apply BPM');{$endif}
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
      if FUseRawImage then
        Frawimage[0,y,x]:=(Frawimage[0,y-1,x]+Frawimage[0,y+1,x]+Frawimage[0,y,x-1]+Frawimage[0,y,x+1]) / 4
      else
        Fimage[0,y,x]:=(Fimage[0,y-1,x]+Fimage[0,y+1,x]+Fimage[0,y,x-1]+Fimage[0,y,x+1]) / 4;
      if n_plane=3 then begin
        if FUseRawImage then begin
          Frawimage[1,y,x]:=(Frawimage[1,y-1,x]+Frawimage[1,y+1,x]+Frawimage[1,y,x-1]+Frawimage[1,y,x+1]) / 4;
          Frawimage[2,y,x]:=(Frawimage[2,y-1,x]+Frawimage[2,y+1,x]+Frawimage[2,y,x-1]+Frawimage[2,y,x+1]) / 4;
        end
        else begin
          Fimage[1,y,x]:=(Fimage[1,y-1,x]+Fimage[1,y+1,x]+Fimage[1,y,x-1]+Fimage[1,y,x+1]) / 4;
          Fimage[2,y,x]:=(Fimage[2,y-1,x]+Fimage[2,y+1,x]+Fimage[2,y,x-1]+Fimage[2,y,x+1]) / 4;
        end;
      end;
    end;
  end;
  FBPMProcess:=true;
  FHeader.Insert( FHeader.Indexof('END'),'COMMENT','Corrected with Bap Pixel Map','');
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

function TFits.GammaCorr(value: Word):byte;
begin
  // gamma_c is 0..1 of length 32768
  // value is 0..65535
  // result is 0..255
  result:=round(255*gamma_c[trunc(value/2)]);
  if FInvert then result:=255-result;
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

function TFits.GetBayerMode: TBayerMode;
var buf: string;
begin
  if DefaultBayerMode=bayerCamera then begin
    buf:=copy(HeaderInfo.bayerpattern,1,2);
    // use value from header
    if buf='GR' then result:=bayerGR
    else if buf='RG' then result:=bayerRG
    else if buf='T'  then result:=bayerRG  //some software can use a boolean to indicate a bayered image
    else if buf=''   then result:=bayerRG  //this is the most probable case if a user request auto debayer with a camera that not report it's sensor type
    else if buf='BG' then result:=bayerBG
    else if buf='GB' then result:=bayerGB
    else if buf='UN' then result:=bayerUnsupported
    else
      result:=bayerUnsupported;
  end
  else begin
    // use default configured value
    result:=DefaultBayerMode;
  end;
end;

procedure TFits.GetBayerBgColor(t:TBayerMode; rmult,gmult,bmult:double; out r,g,b: single);
var i,j,xs,ys,row,col,pix,thr: integer;
    sr,sg,sb: double;
    nr,ng,nb: integer;
const subsample=5;
begin
 r:=0;
 b:=0;
 g:=0;
 if (FFitsInfo.bitpix<>8)and(FFitsInfo.bitpix<>16) then exit;
 sr:=0; sg:=0; sb:=0;
 nr:=0; ng:=0; nb:=0;
 xs:= Fwidth div subsample;
 ys:= FHeight div subsample;
 thr:=round(Fmean+5*Fsigma);
 for i:=0 to ys-1 do begin
   row:=subsample*i;
   for j:=0 to xs-1 do begin
     col:=subsample*j;
     pix:=round( max(0,min(MaxWord,Frawimage[0,subsample*i,subsample*j])) );
     if pix<thr then begin
     if not odd(row) then begin //ligne paire
        if not odd(col) then begin //colonne paire et ligne paire
          case t of
          bayerGR: begin inc(ng); sg:=sg+round(gmult*pix); end;
          bayerRG: begin inc(nr); sr:=sr+round(rmult*pix); end;
          bayerBG: begin inc(nb); sb:=sb+round(bmult*pix); end;
          bayerGB: begin inc(ng); sg:=sg+round(gmult*pix); end;
          end;
        end
        else begin //colonne impaire et ligne paire
          case t of
          bayerGR: begin inc(nr); sr:=sr+round(rmult*pix); end;
          bayerRG: begin inc(ng); sg:=sg+round(gmult*pix); end;
          bayerBG: begin inc(ng); sg:=sg+round(gmult*pix); end;
          bayerGB: begin inc(nb); sb:=sb+round(bmult*pix); end;
          end;
        end;
     end
     else begin //ligne impaire
        if not odd(col) then begin //colonne paire et ligne impaire
          case t of
          bayerGR: begin inc(nb); sb:=sb+round(bmult*pix); end;
          bayerRG: begin inc(ng); sg:=sg+round(gmult*pix); end;
          bayerBG: begin inc(ng); sg:=sg+round(gmult*pix); end;
          bayerGB: begin inc(nr); sr:=sr+round(rmult*pix); end;
          end;
        end
        else begin //colonne impaire et ligne impaire
          case t of
          bayerGR: begin inc(ng); sg:=sg+round(gmult*pix); end;
          bayerRG: begin inc(nb); sb:=sb+round(bmult*pix); end;
          bayerBG: begin inc(nr); sr:=sr+round(rmult*pix); end;
          bayerGB: begin inc(ng); sg:=sg+round(gmult*pix); end;
          end;
       end;
     end;
     end;
   end;
 end;
 if nr>0 then r:=sr/nr;
 if ng>0 then g:=sg/ng;
 if nb>0 then b:=sb/nb;
end;

procedure TFits.BayerInterpolation(t:TBayerMode; rmult,gmult,bmult:double; rbg,gbg,bbg:single; pix1,pix2,pix3,pix4,pix5,pix6,pix7,pix8,pix9:single; row,col:integer; out pixr,pixg,pixb:single); inline;
var r,g,b: double;
begin
   if not odd(row) then begin //ligne paire
      if not odd(col) then begin //colonne paire et ligne paire
        case t of
        bayerGR: begin
            r:= round(rmult*(pix4+pix6)/2)-rbg;
            g:=round(gmult*pix5)-gbg;
            b:= round(bmult*(pix2+pix8)/2)-bbg;
           end;
        bayerRG: begin
            r:=round(rmult*pix5)-rbg;
            g:= round(gmult*(pix2+pix4+pix6+pix8)/4)-gbg;
            b:= round(bmult*(pix1+pix3+pix7+pix9)/4)-bbg;
           end;
        bayerBG: begin
            r:= round(rmult*(pix1+pix3+pix7+pix9)/4)-rbg;
            g:= round(gmult*(pix2+pix4+pix6+pix8)/4)-gbg;
            b:=round(bmult*pix5)-bbg;
           end;
        bayerGB: begin
            r:= round(rmult*(pix2+pix8)/2)-rbg;
            g:=round(gmult*pix5)-gbg;
            b:= round(bmult*(pix4+pix6)/2)-bbg;
           end;
        else begin
            r:=0; g:=0; b:=0;
           end;
        end;
      end
      else begin //colonne impaire et ligne paire
       case t of
       bayerGR: begin
           r:=round(rmult*pix5)-rbg;
           g:= round(gmult*(pix2+pix4+pix6+pix8)/4)-gbg;
           b:= round(bmult*(pix1+pix3+pix7+pix9)/4)-bbg;
          end;
       bayerRG: begin
           r:= round(rmult*(pix4+pix6)/2)-rbg;
           g:=round(gmult*pix5)-gbg;
           b:=round(bmult*(pix2+pix8)/2)-bbg;
          end;
       bayerBG: begin
           r:=round(rmult*(pix2+pix8)/2)-rbg;
           g:=round(gmult*pix5)-gbg;
           b:= round(bmult*(pix4+pix6)/2)-bbg;
          end;
       bayerGB: begin
           r:= round(rmult*(pix1+pix3+pix7+pix9)/4)-rbg;
           g:= round(gmult*(pix2+pix4+pix6+pix8)/4)-gbg;
           b:=round(bmult*pix5)-bbg;
          end;
       else begin
           r:=0; g:=0; b:=0;
          end;
       end;
     end;
   end
   else begin //ligne impaire
     if not odd(col) then begin //colonne paire et ligne impaire
       case t of
       bayerGR: begin
           r:= round(rmult*(pix1+pix3+pix7+pix9)/4)-rbg;
           g:= round(gmult*(pix2+pix4+pix6+pix8)/4)-gbg;
           b:=round(bmult*pix5)-bbg;
          end;
       bayerRG: begin
           r:= round(rmult*(pix2+pix8)/2)-rbg;
           g:=round(gmult*pix5)-gbg;
           b:=round(bmult*(pix4+pix6)/2)-bbg;
          end;
       bayerBG: begin
           r:= round(rmult*(pix4+pix6)/2)-rbg;
           g:=round(gmult*pix5)-gbg;
           b:=round(bmult*(pix2+pix8)/2)-bbg;
          end;
       bayerGB: begin
           r:=round(rmult*pix5)-rbg;
           g:= round(gmult*(pix2+pix4+pix6+pix8)/4)-gbg;
           b:= round(bmult*(pix1+pix3+pix7+pix9)/4)-bbg;
          end;
       else begin
           r:=0; g:=0; b:=0;
          end;
       end;
    end
    else begin //colonne impaire et ligne impaire
       case t of
       bayerGR: begin
           r:= round(rmult*(pix2+pix8)/2)-rbg;
           g:= round(gmult*pix5)-gbg;
           b:= round(bmult*(pix4+pix6)/2)-bbg;
          end;
       bayerRG: begin
           r:= round(rmult*(pix1+pix3+pix7+pix9)/4)-rbg;
           g:= round(gmult*(pix2+pix4+pix6+pix8)/4)-gbg;
           b:=round(bmult*pix5)-bbg;
          end;
       bayerBG: begin
           r:= round(rmult*pix5)-rbg;
           g:= round(gmult*(pix2+pix4+pix6+pix8)/4)-gbg;
           b:= round(bmult*(pix1+pix3+pix7+pix9)/4)-bbg;
          end;
       bayerGB: begin
           r:= round(rmult*(pix4+pix6)/2)-rbg;
           g:= round(gmult*pix5)-gbg;
           b:= round(bmult*(pix2+pix8)/2)-bbg;
          end;
       else begin
           r:=0; g:=0; b:=0;
          end;
       end;
     end;
   end;
   pixr:=max(0,min(MAXWORD,r));
   pixg:=max(0,min(MAXWORD,g));
   pixb:= max(0,min(MAXWORD,b));
end;

procedure TFits.GetExpBitmap(var bgra: TExpandedBitmap);
// get linear 16bit bitmap
var i: integer;
    working, timingout: boolean;
    timelimit: TDateTime;
    thread: array[0..15] of TGetExpThread;
    tc,timeout: integer;
begin
bgra.SetSize(Fwidth,Fheight);
thread[0]:=nil;
// number of thread
 tc := max(1,min(16, MaxThreadCount)); // based on number of core
 tc := max(1,min(tc,Fheight div 100)); // do not split the image too much
// start thread
for i := 0 to tc - 1 do
begin
  thread[i] := TGetExpThread.Create(True);
  thread[i].fits := self;
  thread[i].num := tc;
  thread[i].id := i;
  thread[i].bgra := bgra;
  if FimageScaled then begin
    thread[i].minv := FimageMin;
    thread[i].c := FimageC;
  end;
  thread[i].Start;
end;
// wait complete
timeout:=60;
timelimit := now + timeout / secperday;
repeat
  sleep(100);
  working := False;
  for i := 0 to tc - 1 do
    working := working or thread[i].working;
  timingout := (now > timelimit);
until (not working) or timingout;
// refresh image
bgra.InvalidateBitmap;
end;

procedure TFits.GetBGRABitmap(var bgra: TBGRABitmap);
// get stretched 8bit bitmap
var i : integer;
    HighOverflow,LowOverflow: TBGRAPixel;
    c,overflow,underflow: double;
    working, timingout: boolean;
    timelimit: TDateTime;
    thread: array[0..15] of TGetBgraThread;
    tc,timeout: integer;
begin
  HighOverflow:=ColorToBGRA(clFuchsia);
  LowOverflow:=ColorToBGRA(clYellow);
  overflow:=(FOverflow-FimageMin)*FimageC;
  underflow:=(FUnderflow-FimageMin)*FimageC;
  bgra.SetSize(Fwidth,Fheight);
  if FVisumin>=FVisuMax then begin
    if FVisuMin=0 then
      FVisuMax:=FVisuMin+1
    else
      FVisuMin:=FVisuMax-1
  end;
  c:=MaxWord/(FVisuMax-FVisumin);
  thread[0]:=nil;
  // number of thread
   tc := max(1,min(16, MaxThreadCount)); // based on number of core
   tc := max(1,min(tc,Fheight div 100)); // do not split the image too much
  // start thread
  for i := 0 to tc - 1 do
  begin
    thread[i] := TGetBgraThread.Create(True);
    thread[i].fits := self;
    thread[i].num := tc;
    thread[i].id := i;
    thread[i].HighOverflow := HighOverflow;
    thread[i].LowOverflow := LowOverflow;
    thread[i].overflow := overflow;
    thread[i].underflow := underflow;
    thread[i].bgra := bgra;
    thread[i].vmin := FVisuMin;
    thread[i].c := c;
    thread[i].Start;
  end;
  // wait complete
  timeout:=60;
  timelimit := now + timeout / secperday;
  repeat
    sleep(100);
    working := False;
    for i := 0 to tc - 1 do
      working := working or thread[i].working;
    timingout := (now > timelimit);
  until (not working) or timingout;
  // refresh image
  bgra.InvalidateBitmap;
end;

procedure TFits.SaveToBitmap(fn: string);
var expbmp: TExpandedBitmap;
    bgra: TBGRABitmap;
    ext: string;
begin
  ext:=uppercase(ExtractFileExt(fn));
  if (ext='.PNG')or(ext='.TIF')or(ext='.TIFF') then begin
    // save 16 bit linear image
    expbmp:=TExpandedBitmap.Create;
    GetExpBitmap(expbmp);
    expbmp.SaveToFile(fn);
    expbmp.Free;
  end
  else begin
    //save 8 bit stretched image
    bgra:=TBGRABitmap.Create;
    GetBGRABitmap(bgra);
    bgra.SaveToFile(fn);
    bgra.Free;
  end;
end;

procedure TFits.ClearImage;
begin
FImageValid:=false;
Fheight:=0;
Fwidth:=0;
ClearFitsInfo;
setlength(Frawimage,0,0,0);
setlength(Fimage,0,0,0);
FStream.Clear;
end;

procedure calculate_bg_sd(Fimage: Timafloat; x,y,rs,wd :integer; var bg,sd : double);{version 2021-03-26. calculate background and standard deviation for an annulus at position x,y with innner radius rs+1 and outer radius rs+1+wd. wd should be 1 or larger}
const maxbg=2000;
var
  counter,i,j,r1_square,r2_square,r2,distance : integer;
  mad_bg  : double;
  background : array [0..maxbg] of double; {fixed size array for fast execution}

begin
  r1_square:=rs*rs;;{square radius}
  r2:=rs+wd;{outer radius annulus}
  r2_square:=r2*r2;

  sd:=99999999999;
  bg:=0;
  try
  counter:=0;
  for i:=-r2 to r2 do {calculate the mean outside the the detection area}
  for j:=-r2 to r2 do
  begin
    distance:=i*i+j*j; {working with sqr(distance) is faster then applying sqrt}
    if ((distance>r1_square) and (distance<=r2_square)) then {annulus, circular area outside rs, typical one pixel wide}
    begin
      background[counter]:=Fimage[0,y+i,x+j];
      if counter>=maxbg then break;
      inc(counter);
    end;
  end;

  bg:=Smedian2(background,counter);
  for i:=0 to counter-1 do background[i]:=abs(background[i] - bg);{fill background with offsets}
  mad_bg:=Smedian2(background,counter); //median absolute deviation (MAD)
  sd:=mad_bg*1.4826; {Conversion from mad to sd. See https://en.wikipedia.org/wiki/Median_absolute_deviation}
  sd:=max(sd,0.1); {add some value for images with zero noise background. This will prevent that background is seen as a star. E.g. some jpg processed by nova.astrometry.net}
  except
    {should not happen, sd=99999999999}
  end;
end;

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

function TFits.double_star(ri, x,y : integer):boolean;
// double star detection based difference bright_spot and center_of_gravity
var SumVal,SumValX,SumValY,val,vmax,bg,sd, Xg, Yg: double;
     i,j : integer;
begin

  try
    calculate_bg_sd(Fimage,x,y,ri,4,bg,sd); {calculate background and standard deviation for position x,y around box 2rs x 2rs. }

    SumVal:=0;
    SumValX:=0;
    SumValY:=0;
    vmax:=0;
    for i:=-ri to ri do
      for j:=-ri to ri do
      begin
        val:=Fimage[0,y+j,x+i]-bg;
        if val<0 then val:=0;
        if val>vmax then vmax:=val;
        SumVal:=SumVal+val;
        SumValX:=SumValX+val*(i);
        SumValY:=SumValY+val*(j);
      end;
    Xg:=SumValX/SumVal;
    Yg:=SumValY/SumVal;
    // if ((Xg*Xg)+(Yg*Yg))>0.3 then result:=true {0.3 is experimental factor. Double star, too much unbalance between bright spot and centre of gravity}
    if ((Xg*Xg)+(Yg*Yg))>sqr(4) then result:=true {Center of gravity is 4 or more pixels from brightest position x, y. Assume double star visible}
    else
      result:=false;
  except
    on E: Exception do begin
        result:=true;
    end;
  end;
end;{double star detection}

procedure TFits.FindBrightestPixel(x,y,s,starwindow2: integer; out xc,yc:integer; out vmax: double; accept_double: boolean=true);
// brightest 3x3 pixels in area s*s centered on x,y
var i,j,rs,xm,ym: integer;
    bg,sd: double;
    val :double;
const
    wd =4; {wd is the width of the area outside box rs used for calculating the mean value of the background}

begin
 rs:= s div 2;{box size is s x s pixels}
 if (x-rs)<(3+wd) then x:=rs+(3+wd); {stay 3 pixels away from the sides. wd is the width of the area outside box 2rs x 2rs used for calculating the mean value of the background}
 if (x+rs)>(Fwidth-(3+wd)) then x:=Fwidth-rs-(3+wd);
 if (y-rs)<(3+wd) then y:=rs+(3+wd);
 if (y+rs)>(Fheight-(3+wd)) then y:=Fheight-rs-(3+wd);

 vmax:=0;
 xm:=0;
 ym:=0;

 try
   // average background
  calculate_bg_sd(Fimage,x,y,rs,wd {4},bg,sd); {calculate background and standard deviation for position x,y around box 2rs x 2rs. }

 // try with double star exclusion
 for i:=-rs to rs do
   for j:=-rs to rs do begin
     val:=(Fimage[0,y+j-1 ,x+i-1]+Fimage[0,y+j-1 ,x+i]+Fimage[0,y+j-1 ,x+i+1]+
           Fimage[0,y+j ,x+i-1]+Fimage[0,y+j ,x+i]+Fimage[0,y+j ,x+i+1]+
           Fimage[0,y+j+1 ,x+i-1]+Fimage[0,y+j+1 ,x+i]+Fimage[0,y+j+1 ,x+i+1])/9;

     Val:=Val-bg;
     // huge performance improvement by checking only the pixels above the noise
     if (val>((5*sd))) and (Val>vmax) then
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

procedure TFits.FindStarPos(x,y,s: integer; out xc,yc,ri:integer; out vmax,bg, sd: double);
// center of gravity in area s*s centered on x,y
const
    max_ri=100;
var i,j,rs, distance :integer;
    SumVal,SumValX,SumValY, val,xg,yg : double;
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
    calculate_bg_sd(Fimage,x,y,rs,4,bg,sd); {calculate background and standard deviation for position x,y around box rs x rs. }

    // Get center of gravity whithin star detection box
    SumVal:=0;
    SumValX:=0;
    SumValY:=0;
    vmax:=0;
    for i:=-rs to rs do
     for j:=-rs to rs do begin
       val:=Fimage[0,y+j,x+i]-bg;
       if val>((3*sd)) then  {>3 * sd should be signal }
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
       val:=Fimage[0,yc+j,xc+i]-bg;
       if val>((3*sd)) then {>3 * sd should be signal }
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

procedure TFits.GetHFD2(x,y,s: integer; out xc,yc,bg,sd,hfd,star_fwhm,valmax,snr,flux: double; strict_saturation: boolean=true);
// x,y, s, test location x,y and box size s x s
// xc,yc, center of gravity
// bg, background value
// bf_standard_deviation, standard deviation of background
// hfd, Half Flux Diameter of star disk
// star_fwhm, Full Width Half Maximum of star disk
// valmax, maximum value of brightest pixel in final test box.
// SNR, signal noise ratio
// flux, the total star signal
// fluxsnr, the signal noise ratio on the total flux
const
    max_ri=100;
var i,j,rs,distance,ri, distance_top_value, illuminated_pixels, saturated_counter, max_saturated : integer;
    valsaturation:double;
    SumVal,SumValX,SumValY,SumvalR,val,xg,yg, pixel_counter,r, val_00,val_01,val_10,val_11,af,
    faintA,faintB, brightA,brightB,faintest,brightest : double;
    distance_histogram  : array [0..max_ri] of integer;
    HistStart,asymmetry : boolean;
begin
  valmax:=0;
  bg:=0;
  snr:=0;
  valmax:=0;
  hfd:=-1;
  star_fwhm:=-1;
  flux:=-1;

  if strict_saturation then
     max_saturated:=0
  else
     max_saturated:=5;

  rs:=s div 2;
  if rs>max_ri then rs:=max_ri; {protection against run time error}

  if (x-s)<1+4 then x:=s+1+4;
  if (x+s)>(Fwidth-1-4) then x:=Fwidth-s-1-4;
  if (y-s)<1+4 then y:=s+1+4;
  if (y+s)>(Fheight-1-4) then y:=Fheight-s-1-4;

  try
    calculate_bg_sd(Fimage,x,y,rs,4,bg,sd); {calculate background and standard deviation for position x,y around box rs x rs. }

    repeat {## reduce box size till symmetry to remove stars}
      // Get center of gravity whithin star detection box and count signal pixels
      SumVal:=0;
      SumValX:=0;
      SumValY:=0;
      valmax:=0;
      saturated_counter:=0;
      if FFitsInfo.floatingpoint then
        valsaturation:=MaxDouble
      else
        valsaturation:=MaxADU-1-bg;
      for i:=-rs to rs do
      for j:=-rs to rs do
      begin
        val:=Fimage[0,y+j,x+i]-bg;
        if val>(3.5)*sd then {just above noise level. }
        begin
          if val>=valsaturation then inc(saturated_counter);
          if val>valmax then valmax:=val;
          SumVal:=SumVal+val;
          SumValX:=SumValX+val*(i);
          SumValY:=SumValY+val*(j);
        end;
      end;
      if sumval<=15*sd then exit; {no star found, too noisy}
      Xg:=SumValX/SumVal;
      Yg:=SumValY/SumVal;
      xc:=(x+Xg);
      yc:=(y+Yg);
     {center of star gravity found}

      if ((xc-rs<=1) or (xc+rs>=Fwidth-2) or (yc-rs<=1) or (yc+rs>=Fheight-2) ) then begin exit;end;{prevent runtime errors near sides of images}

     // Check for asymmetry. Are we testing a group of stars or a defocused star?
      val_00:=0;val_01:=0;val_10:=0;val_11:=0;

      for i:=rs downto 1 do begin
        for j:=rs downto 1 do begin
          val_00:=val_00+ value_subpixel(xc+i-0.5,yc+j-0.5)-bg;
          val_01:=val_01+ value_subpixel(xc+i-0.5,yc-j+0.5)-bg;
          val_10:=val_10+ value_subpixel(xc-i+0.5,yc+j-0.5)-bg;
          val_11:=val_11+ value_subpixel(xc-i+0.5,yc-j+0.5)-bg;
        end;
      end;

      af:=0.30; {## asymmetry factor. 1=is allow only prefect symmetrical, 0.000001=off}
                {0.30 make focusing to work with bad seeing}

      {check for asymmetry of detected star using the four quadrants}
      if val_00<val_01  then begin faintA:=val_00; brightA:=val_01; end else begin faintA:=val_01; brightA:=val_00; end;
      if val_10<val_11  then begin faintB:=val_10; brightB:=val_11; end else begin faintB:=val_11; brightB:=val_10; end;
      if faintA<faintB  then faintest:=faintA else faintest:=faintB;{find faintest quadrant}
      if brightA>brightB  then brightest:=brightA else brightest:=brightB;{find brightest quadrant}
      asymmetry:=(brightest*af>=faintest); {if true then detected star has asymmetry, ovals/galaxies or double stars will not be accepted}

      if asymmetry then dec(rs,2); {try a smaller window to exclude nearby stars}
      if rs<4 then exit; {try to reduce box up to rs=4 equals 8x8 box else exit}
    until asymmetry=false; {loop and reduce box size until asymmetry is gone or exit if box is too small}

    if (not Undersampled) then   {check on single hot pixels}
    for i:=-1 to +1 do
      for j:=-1 to +1 do begin
        val:=Fimage[0,round(yc)+j,round(xc)+i]-bg; {no subpixel calculation here}
        if val>0.5*sumval then exit;
      end;

    // Get diameter of star above the noise level.
    for i:=0 to rs do distance_histogram[i]:=0;{clear histogram of pixel distances}

    for i:=-rs to rs do begin
      for j:=-rs to rs do begin
        distance:=round((sqrt(i*i + j*j )));{distance from star gravity center }
        if distance<=rs then {build histogram for circel with radius rs}
        begin
          Val:=value_subpixel(xc+i,yc+j)-bg;
          if val>((3*sd)) then {>3 * sd should be signal }
            distance_histogram[distance]:=distance_histogram[distance]+1;{build distance histogram}
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
    until ((ri>=rs) or (HistStart and (distance_histogram[ri]<=0.1*distance_top_value {drop-off detection})));{find a distance where there is no pixel illuminated, so the border of the star image of interest}

    if ri>=rs then {star is equal or larger then box, abort} exit; {hfd:=-1}
    if (ri>2)and(illuminated_pixels<0.35*sqr(ri+ri-2)){35% surface} then {not a star disk but stars, abort} exit; {hfd:=-1}

    if ri<3 then  {small star image}
    begin
     if ((not Undersampled) and (distance_histogram[1]<3)) then
        exit; // reject single hot pixel if less then 3 pixels are detected around the center of gravity
     ri:=3; {Minimum 6+1 x 6+1 pixel box}
    end;

    // Get HFD using the aproximation routine assuming that HFD line divides the star in equal portions of gravity:
    SumVal:=0;
    SumValR:=0;
    pixel_counter:=0;

    for i:=-ri to ri do {Make steps of one pixel}
      for j:=-ri to ri do
      begin
        Val:=value_subpixel(xc+i,yc+j)-bg;{The calculated center of gravity is a floating point position and can be anyware, so calculate pixel values on sub-pixel level}
        r:=sqrt(i*i+j*j);{Distance from star gravity center}
        SumVal:=SumVal+Val;{Sumval will be star total flux value}
        SumValR:=SumValR+Val*r; {Method Kazuhisa Miyashita, see notes of HFD calculation method}
        if val>=valmax*0.5 then pixel_counter:=pixel_counter+1;{How many pixels are above half maximum for FWHM}
      end;
    if Sumval<0.00001 then Sumval:=0.00001;{prevent divide by zero}
    hfd:=2*SumValR/SumVal;
    hfd:=max(0.7,hfd); // minimum value for a star size of 1 pixel
    star_fwhm:=2*sqrt(pixel_counter/pi);{The surface is calculated by counting pixels above half max. The diameter of that surface called FWHM is then 2*sqrt(surface/pi) }
    if (SumVal>0.00001)and(saturated_counter<=max_saturated) then begin
      flux:=Sumval;
      snr:=flux/sqrt(flux +sqr(ri)*pi*sqr(sd)); {For both bright stars (shot-noise limited) or skybackground limited situations
                                                                       snr:=signal/sqrt(signal + r*r*pi* SKYsignal) equals snr:=flux/sqrt(flux + r*r*pi* sd^2).}
    end else begin
      flux:=-1;
      snr:=0;
    end;


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
 i,j,n,nhfd: integer;
 overlap: integer;
 img_temp: Timabyte;
 working, timingout: boolean;
 timelimit: TDateTime;
 thread: array[0..15] of TGetStarList;
 tc,timeout: integer;
begin
  overlap:=max(8,round(s/3)); // large overlap to have more chance to measure a big dot as a single piece
  s:=max(4,s-overlap);        // keep original window size after adding overlap
  SetLength(img_temp,1,FWidth,FHeight); {array to check for duplicate}
  for j:=0 to Fheight-1 do
     for i:=0 to FWidth-1 do
        img_temp[0,i,j]:=0;  {mark as not surveyed}
  thread[0]:=nil;
  // number of thread
   tc := max(1,min(16, MaxThreadCount)); // based on number of core
   tc := max(1,min(tc,Fheight div (100+2*s))); // do not split the image too much
  // start thread
  for i := 0 to tc - 1 do
  begin
    thread[i] := TGetStarList.Create(true);
    thread[i].fits := self;
    thread[i].num := tc;
    thread[i].id := i;
    thread[i].rx := rx;
    thread[i].ry := ry;
    thread[i].overlap := overlap;
    thread[i].s := s;
    thread[i].img_temp := img_temp;
    thread[i].Start;
  end;
  // wait complete
  timeout:=60;
  timelimit := now + timeout / secperday;
  repeat
    sleep(100);
    working := False;
    for i := 0 to tc - 1 do
      working := working or thread[i].working;
    timingout := (now > timelimit);
  until (not working) or timingout;
  SetLength(img_temp,0,0,0);
  // copy result
  nhfd:=0;
  for i:=0 to tc - 1 do
    nhfd:=nhfd+Length(thread[i].StarList);
  SetLength(FStarList,nhfd);
  n:=0;
  for i:=0 to tc - 1 do begin
    for j:=0 to Length(thread[i].StarList)-1 do begin
       FStarList[n]:=thread[i].StarList[j];
       inc(n);
    end;
  end;
  // cleanup
  for i:=0 to tc - 1 do SetLength(thread[i].StarList,0);
  for i := 0 to tc - 1 do thread[i].Free;
end;

procedure TFits.MeasureStarList(s: integer; list: TArrayDouble2);
var
 fitsX,fitsY,nhfd,i: integer;
 hfd1,star_fwhm,vmax,bg,bgdev,xc,yc,snr,flux: double;
begin

nhfd:=0;{set counters at zero}
SetLength(FStarList,1000);{allocate initial size}

for i:=0 to Length(list)-1 do
 begin
   fitsX:=round(list[i,1]);
   fitsY:=round(list[i,2]);
   hfd1:=-1;
   star_fwhm:=-1;

   GetHFD2(fitsX,fitsY,s,xc,yc,bg,bgdev,hfd1,star_fwhm,vmax,snr,flux,false);

   {check valid hfd, snr}
   if ((hfd1>0)and (Undersampled or (hfd1>0.7)))
      and (hfd1<99)
      and (snr>AutofocusMinSNR)  {minimal star detection level, also detect saturation}
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
       FStarList[nhfd-1].bg:=bg;
       // new position to correct image drift
       list[i,1]:=xc;
       list[i,2]:=yc;
    end;
 end;
 SetLength(FStarList,nhfd);  {set length to new number of elements}
end;

function TFits.SameFormat(f:TFits): boolean;
begin
 result := (f<>nil) and f.FFitsInfo.valid and
           (f.FFitsInfo.bitpix = FFitsInfo.bitpix)  and
           (f.FFitsInfo.naxis  = FFitsInfo.naxis )  and
           (f.FFitsInfo.naxis1 = FFitsInfo.naxis1 ) and
           (f.FFitsInfo.naxis2 = FFitsInfo.naxis2 ) and
           (f.FFitsInfo.naxis3 = FFitsInfo.naxis3 ) and
           (f.FFitsInfo.bzero  = FFitsInfo.bzero )  and
           (f.FFitsInfo.roworder = FFitsInfo.roworder ) and
           (f.FFitsInfo.bscale = FFitsInfo.bscale );
end;

procedure TFits.Bitpix8to16;
var i: integer;
begin
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
 WriteFitsImage;
end;

procedure TFits.Math(operand: TFits; MathOperator:TMathOperator; new: boolean=false);
var i,j,k,ii,nax,naxo,ko: integer;
    x,y,dmin,dmax,minoffset : double;
    ni,sum,sum2 : extended;
    m: TMemoryStream;
begin
 if new or (Fheight=0)or(Fwidth=0)then begin  // first frame, just store the operand
    m:=operand.Stream;
    SetStream(m);
    LoadStream;
 end
 else begin  // do operation
    dmin:=1.0E100;
    dmax:=-1.0E100;
    sum:=0; sum2:=0; ni:=0;
    FillByte(FHistogram,sizeof(THistogram),0);
    minoffset:=operand.FFitsInfo.dmin-FFitsInfo.dmin;
    if FUseRawImage then
      nax:=n_plane
    else
      nax:=Fpreview_axis;
    if operand.FUseRawImage then
      naxo:=operand.n_plane
    else
      naxo:=operand.Fpreview_axis;
    for k:=0 to nax-1 do begin
      ko:=min(k,naxo-1);
      for i:=0 to FFitsInfo.naxis2-1 do begin
       ii:=FFitsInfo.naxis2-1-i;
       for j := 0 to FFitsInfo.naxis1-1 do begin
         if FUseRawImage then begin
           x:=Frawimage[k,ii,j];
         end
         else begin
           x:=Fimage[k,ii,j];
         end;
         if operand.FUseRawImage then begin
           y:=operand.Frawimage[ko,ii,j];
         end
         else begin
           y:=operand.Fimage[ko,ii,j];
         end;
         case MathOperator of
           moAdd: x:=x+y;
           moSub: x:=x-y+minoffset;
           moMean: x:=(x+y)/2;
           moMult: x:=x*y;
           moDiv : x:=x/y;
         end;
         if FUseRawImage then
           Frawimage[k,ii,j] := x
         else
           Fimage[k,ii,j] := x;
         inc(FHistogram[round(max(0,min(maxword,x)))]);
         dmin:=min(x,dmin);
         dmax:=max(x,dmax);
         sum:=sum+x;
         sum2:=sum2+x*x;
         ni:=ni+1;
       end;
      end;
    end;
    FStreamValid:=false;
    Fmean:=(sum/ni);
    Fsigma:=sqrt((sum2/ni)-((sum/ni)*(sum/ni)));
    if dmin>=dmax then dmax:=dmin+1;
    FFitsInfo.dmin:=dmin;
    FFitsInfo.dmax:=dmax;
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
    m: TMemoryStream;
begin
  imgshift:=TFits.Create(nil);
  imgshift.onMsg:=onMsg;
  imgshift.CreateImage(FFitsInfo,FHeader);
  for k:=0 to n_plane-1 do begin
    for i:=0 to FFitsInfo.naxis2-1 do begin
     ii:=FFitsInfo.naxis2-1-i;
     for j := 0 to FFitsInfo.naxis1-1 do begin
       x:=j-dx;
       y:=ii-dy;
       if (x>0)and(x<FFitsInfo.naxis1)and(y>0)and(y<FFitsInfo.naxis2) then begin
         imgshift.Fimage[k,ii,j]:=Fimage[k,y,x];
       end
       else begin
        imgshift.Fimage[k,ii,j]:=0;
       end;
     end;
    end;
  end;
  imgshift.FStreamValid:=false;
  m:=imgshift.Stream;
  SetStream(m);
  LoadStream;
  imgshift.Free;
end;

procedure PictureToFits(pict:TMemoryStream; ext: string; var ImgStream:TMemoryStream; flip:boolean=false;pix:double=-1;piy:double=-1;binx:integer=-1;biny:integer=-1;bayer:string='';rmult:string='';gmult:string='';bmult:string='';origin:string='';exifkey:TStringList=nil;exifvalue:TStringList=nil);
var img:TLazIntfImage;
    lRawImage: TRawImage;
    i,j,c,w,h,x,y,naxis: integer;
    ii: smallint;
    b: array[0..2880]of char;
    hdr: TFitsHeader;
    hdrmem: TMemoryStream;
    RedStream,GreenStream,BlueStream: TMemoryStream;
    htyp,hext: string;
    ReaderClass: TFPCustomImageReaderClass;
   Reader: TFPCustomImageReader;
begin
 // define raw image data
 lRawImage.Init;
 with lRawImage.Description do begin
  // Set format 48bit R16G16B16
  Format := ricfRGBA;
  Depth := 48; // used bits per pixel
  Width := 0;
  Height := 0;
  BitOrder := riboBitsInOrder;
  ByteOrder := riboLSBFirst;
  LineOrder := riloTopToBottom;
  BitsPerPixel := 48; // bits per pixel. can be greater than Depth.
  LineEnd := rileDWordBoundary;
  RedPrec := 16; // red precision. bits for red
  RedShift := 0;
  GreenPrec := 16;
  GreenShift := 16; // bitshift. Direction: from least to most significant
  BluePrec := 16;
  BlueShift:=32;
 end;
 // create resources
 lRawImage.CreateData(false);
 hdr:=TFitsHeader.Create;
 ImgStream.Clear;
 ImgStream.Position:=0;
 img:=TLazIntfImage.Create(0,0);
 ext:=uppercase(ext);
 try
   // set image data
   img.SetRawImage(lRawImage);
   // search a reader for the given fileext
   for i:=0 to ImageHandlers.Count-1 do begin
     htyp:=ImageHandlers.TypeNames[i];
     hext:=uppercase(ImageHandlers.Extensions[htyp]);
     if (pos(ext,hext)>0) then begin
       // load image from file, it use the correct reader from fileext
       ReaderClass:=ImageHandlers.ImageReader[htyp];
       Reader:=ReaderClass.Create;
       try
       pict.Position:=0;
       // load image from file, using the reader
       img.LoadFromStream(pict,Reader);
       reader.free;
       break;
       except
         // not the right reader, continue to try another
         reader.free;
       end;
     end;
   end;
   w:=img.Width;
   h:=img.Height;
   if (h=0)or(w=0) then begin
     exit;
   end;
   // detect BW or color
   naxis:=2;
   for i:=0 to (h-1)div 10 do begin
      y:=10*i;
      for j:=0 to (w-1)div 10 do begin
        x:=10*j;
        if (img.Colors[x,y].red <> img.Colors[x,y].green)or(img.Colors[x,y].red <> img.Colors[x,y].blue) then begin
           naxis:=3;
           break;
        end;
      end;
      if naxis=3 then break;
   end;
   // create fits header
   hdr.ClearHeader;
   hdr.Add('SIMPLE',true,'file does conform to FITS standard');
   hdr.Add('BITPIX',16,'number of bits per data pixel');
   hdr.Add('NAXIS',naxis,'number of data axes');
   hdr.Add('NAXIS1',w ,'length of data axis 1');
   hdr.Add('NAXIS2',h ,'length of data axis 2');
   if naxis=3 then hdr.Add('NAXIS3',3 ,'length of data axis 3');
   hdr.Add('BZERO',32768,'offset data range to that of unsigned short');
   hdr.Add('BSCALE',1,'default scaling factor');
   if flip then
     hdr.Add('ROWORDER',bottomup,'Order of the rows in image array')
   else
     hdr.Add('ROWORDER',topdown,'Order of the rows in image array');
   if pix>0 then hdr.Add('PIXSIZE1',pix ,'Pixel Size 1 (microns)');
   if piy>0 then hdr.Add('PIXSIZE2',piy ,'Pixel Size 2 (microns)');
   if binx>0 then hdr.Add('XBINNING',binx ,'Binning factor in width');
   if biny>0 then hdr.Add('YBINNING',biny ,'Binning factor in height');
   if bayer<>'' then begin
     hdr.Add('XBAYROFF',0,'X offset of Bayer array');
     hdr.Add('YBAYROFF',0,'Y offset of Bayer array');
     hdr.Add('BAYERPAT',bayer,'CFA Bayer pattern');
     if rmult<>'' then hdr.Add('MULT_R',rmult,'R multiplier');
     if gmult<>'' then hdr.Add('MULT_G',gmult,'G multiplier');
     if bmult<>'' then hdr.Add('MULT_B',bmult,'B multiplier');
   end;
   hdr.Add('DATE',FormatDateTime(dateisoshort,NowUTC),'Date data written');
   hdr.Add('SWCREATE','CCDciel '+ccdciel_version+'-'+RevisionStr,'');
   if (exifkey<>nil)and(exifvalue<>nil)and(exifkey.Count>0) then begin
     for i:=0 to exifkey.Count-1 do begin
        hdr.Add('HIERARCH',StringReplace(exifkey[i],'.',' ',[rfReplaceAll])+' = '''+exifvalue[i]+'''','');
     end;
   end;
   if origin='' then
     hdr.Add('COMMENT','Converted from '+ext,'')
   else
     hdr.Add('COMMENT','Converted from camera RAW by '+origin,'');
   hdr.Add('END','','');
   hdrmem:=hdr.GetStream;
   try
     // put header in stream
     ImgStream.position:=0;
     hdrmem.Position:=0;
     ImgStream.CopyFrom(hdrmem,hdrmem.Size);
   finally
     hdrmem.Free;
   end;
   // load image
   if naxis=2 then begin
     // BW image
     for i:=0 to h-1 do begin
        if flip then y:=h-1-i
                else y:=i;
        for j:=0 to w-1 do begin
          ii:=img.Colors[j,y].red-32768;
          ii:=NtoBE(ii);
          ImgStream.Write(ii,sizeof(smallint));
        end;
     end;
   end
   else begin
     // Color image
     // put data in stream by color
     RedStream:=TMemoryStream.Create;
     GreenStream:=TMemoryStream.Create;
     BlueStream:=TMemoryStream.Create;
     try
     for i:=0 to h-1 do begin
        if flip then y:=h-1-i
                else y:=i;
        for j:=0 to w-1 do begin
          ii:=img.Colors[j,y].red-32768;
          ii:=NtoBE(ii);
          RedStream.Write(ii,sizeof(smallint));
          ii:=img.Colors[j,y].green-32768;
          ii:=NtoBE(ii);
          GreenStream.Write(ii,sizeof(smallint));
          ii:=img.Colors[j,y].blue-32768;
          ii:=NtoBE(ii);
          BlueStream.Write(ii,sizeof(smallint));
        end;
     end;
     // put the 3 color plane in image stream
     RedStream.Position:=0;
     ImgStream.CopyFrom(RedStream,RedStream.Size);
     GreenStream.Position:=0;
     ImgStream.CopyFrom(GreenStream,GreenStream.Size);
     BlueStream.Position:=0;
     ImgStream.CopyFrom(BlueStream,BlueStream.Size);
     finally
       RedStream.Free;
       GreenStream.Free;
       BlueStream.Free;
     end;
   end;
   // fill to fits buffer size
   b:='';
   c:=ImgStream.Size mod 2880;
   if c>0 then begin
     c:=2880-c;
     FillChar(b,c,0);
     ImgStream.Write(b,c);
   end;
 finally
   // Free resources
   hdr.Free;
   img.free;
 end;
end;

procedure GetExif(raw:TMemoryStream; ext:string; exifkey,exifvalue:TStringList);
var cmd,fn,k,v: string;
    r: Tstringlist;
    i,j,n: integer;
    ok: boolean;
begin
 if trim(ext)='' then ext:='.raw';
 fn:=slash(TmpDir)+'exiftmp'+ext;
 ok:=false;
 if Exiv2Cmd<>'' then begin
   r:=Tstringlist.Create;
   try
   raw.SaveToFile(fn);
   cmd:=Exiv2Cmd+' -PEkt '+fn;
   n:=ExecProcess(cmd,r);
   if (n=0)and(r.Count>0) then begin
     ok:=true;
     for i:=0 to r.Count-1 do begin
       j:=pos(' ',r[i]);
       if j>0 then begin
         k:=trim(copy(r[i],1,j));
         v:=trim(copy(r[i],j,999));
         if (length(k+v)<65)and(v<>'(Binary value suppressed)') then begin
           exifkey.Add(k);
           exifvalue.Add(v);
         end;
       end;
     end;
   end;
   finally
     r.free;
   end;
 end;
 if (not ok) and (ExifToolCmd<>'') then begin
   r:=Tstringlist.Create;
   try
   raw.SaveToFile(fn);
   cmd:=ExifToolCmd+' -m -G:2 -s2 '+fn;
   n:=ExecProcess(cmd,r);
   if (n=0)and(r.Count>0) then begin
     for i:=0 to r.Count-1 do begin
       j:=pos(': ',r[i]);
       if j>0 then begin
         k:=trim(copy(r[i],1,j));
         k:=StringReplace(k,'[','',[]);
         k:=StringReplace(k,']','.',[]);
         k:=StringReplace(k,':','.',[rfReplaceAll]);
         k:=StringReplace(k,' ','',[rfReplaceAll]);
         v:=trim(copy(r[i],j+2,999));
         v:=StringReplace(v,'; ',';',[rfReplaceAll]);
         if (length(k+v)<65)and(pos('(Binary data',v)=0) then begin
           exifkey.Add(k);
           exifvalue.Add(v);
         end;
       end;
     end;
   end;
   finally
     r.free;
   end;
 end;
end;

procedure RawToFits(raw:TMemoryStream; ext: string; var ImgStream:TMemoryStream; out rmsg:string; pix:double=-1;piy:double=-1;binx:integer=-1;biny:integer=-1; flip:boolean=false);
var i,ii,j,n,x,c: integer;
    xs,ys,xmax,ymax: integer;
    rawinfo:TRawInfo;
    rawinfo2:TRawInfo2;
    buf: array of char;
    msg: array[0..1024] of char;
    pmsg: PChar;
    xx: SmallInt;
    hdr: TFitsHeader;
    hdrmem: TMemoryStream;
    b: array[0..2880]of char;
    rawf,tiff,cmdi,cmdu,txt,bayerpattern: string;
    outr: TStringList;
    rmult,gmult,bmult: string;
    infook: boolean;
    exifkey,exifvalue: TStringList;
begin
rmsg:='';
try
exifkey:=TStringList.Create;
exifvalue:=TStringList.Create;
if WantExif then begin
  GetExif(raw,ext,exifkey,exifvalue);
end;
if libraw<>0 then begin  // Use libraw directly
  try
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'Copy raw buffer');{$endif}
  i:=raw.Size;
  SetLength(buf,i+1);
  raw.Position:=0;
  raw.Read(buf[0],i);
  except
    rmsg:='Error loading file';
    exit;
  end;
  try
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'libraw LoadRaw');{$endif}
  n:=LoadRaw(@buf[0],i);
  SetLength(buf,0);
  if n<>0 then begin
    pmsg:=@msg;
    GetRawErrorMsg(n,pmsg);
    rmsg:=msg;
    exit;
  end;
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'GetRawInfo');{$endif}
  rawinfo.bitmap:=nil;
  n:=GetRawInfo(rawinfo);
  if (n<>0) or (rawinfo.bitmap=nil) then begin
   rmsg:='GetRawInfo error';
   exit;
  end;
  rawinfo2.version:=3;
  infook:=false;
  if @GetRawInfo2<>nil then begin
     n:=GetRawInfo2(rawinfo2);
     infook:=(n=0);
  end;
  xs:=rawinfo.leftmargin;
  ys:=rawinfo.topmargin;
  xmax:=xs+rawinfo.imgwidth;
  ymax:=ys+rawinfo.imgheight;
  if (xmax>rawinfo.rawwidth)or(ymax>rawinfo.rawheight) then begin
    rmsg:='Inconsistant image size: leftmargin='+inttostr(rawinfo.leftmargin)+'topmargin='+inttostr(rawinfo.topmargin)+
          'imgwidth='+inttostr(rawinfo.imgwidth)+'imgheight='+inttostr(rawinfo.imgheight)+
          'rawwidth='+inttostr(rawinfo.rawwidth)+'rawheight='+inttostr(rawinfo.rawheight);
    exit;
  end;
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'Create FITS header');{$endif}
  hdr:=TFitsHeader.Create;
  hdr.ClearHeader;
  hdr.Add('SIMPLE',true,'file does conform to FITS standard');
  hdr.Add('BITPIX',16,'number of bits per data pixel');
  hdr.Add('NAXIS',2,'number of data axes');
  hdr.Add('NAXIS1',rawinfo.imgwidth ,'length of data axis 1');
  hdr.Add('NAXIS2',rawinfo.imgheight ,'length of data axis 2');
  hdr.Add('BZERO',32768,'offset data range to that of unsigned short');
  hdr.Add('BSCALE',1,'default scaling factor');
  if flip then
    hdr.Add('ROWORDER',bottomup,'Order of the rows in image array')
  else
    hdr.Add('ROWORDER',topdown,'Order of the rows in image array');
  if pix>0 then hdr.Add('PIXSIZE1',pix ,'Pixel Size 1 (microns)');
  if piy>0 then hdr.Add('PIXSIZE2',piy ,'Pixel Size 2 (microns)');
  if binx>0 then hdr.Add('XBINNING',binx ,'Binning factor in width');
  if biny>0 then hdr.Add('YBINNING',biny ,'Binning factor in height');
  if infook and (rawinfo2.version>1) then begin
    txt:=copy(trim(rawinfo2.camera),1,40);
    if txt<>'' then hdr.Add('CAMERA', txt ,'Camera model');
  end;
  if infook and (rawinfo2.version>=3) and (rawinfo2.temperature>-273) then hdr.Add('CCD-TEMP',rawinfo2.temperature ,'CCD temperature (Celsius)');
  if infook and (rawinfo2.version>1) and (rawinfo2.focal_len>0) then hdr.Add('FOCALLEN',rawinfo2.focal_len ,'Camera focal length');
  if infook and (rawinfo2.version>1) and (rawinfo2.aperture>0) then hdr.Add('F_STOP',round(10*rawinfo2.aperture)/10 ,'Camera F-stop');
  if infook and (rawinfo2.version>1) and (rawinfo2.isospeed>0) then hdr.Add('ISOSPEED',rawinfo2.isospeed ,'Camera ISO speed');
  if infook and (rawinfo2.version>1) and (rawinfo2.shutter>0) then hdr.Add('SHUTTER',rawinfo2.shutter ,'Camera shutter');
  if infook and (rawinfo2.version>1) and (rawinfo2.timestamp>0) then hdr.Add('DATE-OBS',FormatDateTime(dateisoshort,UnixToDateTime(rawinfo2.timestamp)) ,'Camera timestamp');
  hdr.Add('XBAYROFF',0,'X offset of Bayer array');
  hdr.Add('YBAYROFF',0,'Y offset of Bayer array');
  hdr.Add('BAYERPAT',rawinfo.bayerpattern,'CFA Bayer pattern');
  if infook and (rawinfo2.version>1) and (rawinfo2.colors=3) then begin
    hdr.Add('MULT_R',rawinfo2.rmult,'R multiplier');
    hdr.Add('MULT_G',rawinfo2.gmult,'G multiplier');
    hdr.Add('MULT_B',rawinfo2.bmult,'B multiplier');
  end;
  hdr.Add('DATE',FormatDateTime(dateisoshort,NowUTC),'Date data written');
  hdr.Add('SWCREATE','CCDciel '+ccdciel_version+'-'+RevisionStr,'');
  if exifkey.Count>0 then begin
    for i:=0 to exifkey.Count-1 do begin
       hdr.Add('HIERARCH',StringReplace(exifkey[i],'.',' ',[rfReplaceAll])+' = '''+exifvalue[i]+'''','');
    end;
  end;
  hdr.Add('COMMENT','Converted from camera RAW by libraw','');
  hdr.Add('END','','');
  hdrmem:=hdr.GetStream;
  try
    // put header in stream
    ImgStream.position:=0;
    hdrmem.Position:=0;
    ImgStream.CopyFrom(hdrmem,hdrmem.Size);
  finally
    hdrmem.Free;
  end;
  hdr.Free;
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'Copy data to FITS');{$endif}
  for i:=ys to ymax-1 do begin
    if flip then
      ii:=ymax-1-i
    else
      ii:=i;
    for j:=xs to xmax-1 do begin
      {$RANGECHECKS OFF} x:=TRawBitmap(rawinfo.bitmap)[ii*(rawinfo.rawwidth)+j];
      if x>0 then
         xx:=x-32768
      else
         xx:=-32768;
      xx:=NtoBE(xx);
      ImgStream.Write(xx,sizeof(smallint));
    end;
  end;
  b:='';
  c:=ImgStream.Size mod 2880;
  if c>0 then begin
    c:=2880-c;
    FillChar(b,c,0);
    ImgStream.Write(b,c);
  end;
  CloseRaw();
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'RawToFITS end');{$endif}
  except
    rmsg:='Error converting raw file';
  end;
end
else if RawUnpCmd<>'' then begin  // try libraw tools
 try
 rawf:=slash(TmpDir)+'tmp.raw';
 tiff:=slash(TmpDir)+'tmp.raw.tiff';
 DeleteFile(tiff);
 cmdi:=RawIdCmd+' -v '+rawf;
 cmdu:=RawUnpCmd+' -T '+rawf;
 raw.Position:=0;
 raw.SaveToFile(rawf);
 raw.clear;
 outr:=TStringList.Create;
 if ExecProcess(cmdi,outr)<>0 then begin
   exit;
 end;
 for i:=0 to outr.Count-1 do begin
    if copy(outr[i],1,15)='Filter pattern:' then begin
      txt:=outr[i];
      Delete(txt,1,16);
      txt:=trim(txt);
      bayerpattern:=copy(txt,1,4); // Filter pattern: RGGBRGGBRGGBRGGB
    end;
    if copy(outr[i],1,24)='Derived D65 multipliers:' then begin
      txt:=outr[i];
      Delete(txt,1,25);
      txt:=trim(txt);
      rmult:=words(txt,' ',1,1);
      gmult:=words(txt,' ',2,1);
      bmult:=words(txt,' ',3,1);
    end;
 end;
 if ExecProcess(cmdu,outr)<>0 then begin
   exit;
 end;
 raw.LoadFromFile(tiff);
 PictureToFits(raw,'tiff',ImgStream,flip,pix,piy,binx,biny,bayerpattern,rmult,gmult,bmult,'LibRaw tools',exifkey,exifvalue);
 outr.Free;
 except
   rmsg:='Error converting raw file';
 end; end
else if DcrawCmd<>'' then begin  // try dcraw command line
  try
  rawf:=slash(TmpDir)+'tmp.raw';
  tiff:=slash(TmpDir)+'tmp.tiff';
  DeleteFile(tiff);
  cmdi:=DcrawCmd+' -i -t 0 -v '+rawf;
  cmdu:=DcrawCmd+' -D -4 -t 0 -T '+rawf;
  raw.Position:=0;
  raw.SaveToFile(rawf);
  raw.clear;
  outr:=TStringList.Create;
  if ExecProcess(cmdi,outr)<>0 then begin
    exit;
  end;
  for i:=0 to outr.Count-1 do begin
     if copy(outr[i],1,15)='Filter pattern:' then begin
       txt:=outr[i];
       Delete(txt,1,16);
       txt:=trim(txt);
       bayerpattern:=copy(txt,1,2)+copy(txt,4,2); // Filter pattern: RG/GB
       break;
     end;
  end;
  if ExecProcess(cmdu,outr)<>0 then begin
    exit;
  end;
  raw.LoadFromFile(tiff);
  PictureToFits(raw,'tiff',ImgStream,flip,pix,piy,binx,biny,bayerpattern,'','','','dcraw',exifkey,exifvalue);
  outr.Free;
  except
    rmsg:='Error converting raw file';
  end;
end
else begin
  rmsg:='No RAW decoder found!';
end;
finally
  exifkey.Free;
  exifvalue.Free;
end;
end;

function PackFits(unpackedfilename,packedfilename: string; out rmsg:string):integer;
var
  j: integer;
  outstr:Tstringlist;
begin
 try
   outstr:=TStringList.Create;
   rmsg:='';
   result:=ExecProcess(fpackcmd+' -O '+packedfilename+' -D -Y '+unpackedfilename,outstr);
   if result<>0 then begin
     for j:=0 to outstr.Count-1 do rmsg:=rmsg+crlf+outstr[j];
   end;
   outstr.Free;
 except
   on E: Exception do begin
     result:=-1;
     rmsg:=E.Message;
   end;
 end;
end;

function UnpackFits(packedfilename: string; var ImgStream:TMemoryStream; out rmsg:string):integer;
{$ifdef mswindows}
var
  j: integer;
  tmpfo: string;
  outstr:Tstringlist;
{$endif}
begin
 try
   ImgStream.Clear;
   {$ifdef mswindows}
   // funpack -S do not work correctly on Windows
   tmpfo:=slash(TmpDir)+'tmpunpack.fits';
   outstr:=TStringList.Create;
   rmsg:='';
   result:=ExecProcess(funpackcmd+' -O '+tmpfo+' -D '+packedfilename,outstr);
   if result=0 then begin
     ImgStream.LoadFromFile(tmpfo);
     DeleteFile(tmpfo);
   end
   else begin
     for j:=0 to outstr.Count-1 do rmsg:=rmsg+crlf+outstr[j];
   end;
   outstr.Free;
   {$else}
   result:=ExecProcessMem(funpackcmd+' -S '+packedfilename,ImgStream,rmsg);
   {$endif}
 except
   on E: Exception do begin
     result:=-1;
     rmsg:=E.Message;
   end;
 end;
end;

end.
