unit unit_ricecomp_fits;

{
Copyright (C) 2026 Patrick Chevalley

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

{==============================================================================
  Native reading and writing of RICE_1 tile-compressed FITS files (.fz, the
  fpack format) for CCDciel, replacing the external fpack / funpack utilities.

  This unit provides two stream-based entry points that are drop-in
  replacements for the ExecProcess-based UnpackFits / PackFits in cu_fits.pas:

    NativeUnpackFits(InStream, OutStream, rmsg): integer
        InStream  : a full .fz file (compressed BINTABLE FITS) in memory
        OutStream : receives a normal, uncompressed FITS image stream,
                    byte-structured exactly like funpack output would be, so
                    the rest of CCDciel's loading pipeline is unchanged.
        result    : 0 on success, non-zero on error (rmsg gives the reason).

    NativePackFits(InStream, OutStream, rmsg): integer
        InStream  : a normal uncompressed 16-bit-integer FITS image stream
        OutStream : receives a RICE_1 tile-compressed .fz FITS stream
                    (one image row per tile), byte-compatible with CFITSIO so
                    funpack / astropy can read it.
        result    : 0 on success, non-zero on error.

  Reading supports original images with ZBITPIX = 16 (integer) and ZBITPIX =
  -32 (32-bit float, with SUBTRACTIVE_DITHER_1/2 or NONE quantisation).
  Writing supports 16-bit integer mono images only (Rice is lossless only for
  integers - the intended use is many short sub-exposures).

  All the tricky per-tile decoding, dithering and scaling logic lives in
  unit_ricecomp (rice_decode_tiles / rice_encode_rows); this unit only parses
  the BINTABLE structure, fills the parameter record, and (de)serialises the
  uncompressed image to / from a plain FITS stream.

==============================================================================}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, unit_ricecomp;

{ Decode a compressed .fz FITS stream (InStream) into a normal uncompressed
  FITS image stream (OutStream). Returns 0 on success. }
function NativeUnpackFits(InStream, OutStream: TStream; out rmsg: string): integer;

{ Compress a normal uncompressed 16-bit-integer FITS image stream (InStream)
  into a RICE_1 .fz FITS stream (OutStream). Returns 0 on success. }
function NativePackFits(InStream, OutStream: TStream; out rmsg: string): integer;

{ Read ONLY the header of a tile-compressed .fz file, without decompressing any
  pixel data. The compressed image's descriptive keywords (WCS, RA/DEC, XPIXSZ,
  FOCALLEN, EXPTIME, ...) live in the BINTABLE extension header of the second
  HDU as ordinary ASCII cards, so they are simply copied out.

  OutStream receives a synthetic, uncompressed PRIMARY header (terminated by END
  and padded to 2880) describing the ORIGINAL image, so it can be parsed by the
  normal FITS header reader with no special handling, e.g.

      NativeReadFzHeader(ins, mem, rmsg);
      FHeader.ReadHeader(mem);

  The Z-prefixed geometry keywords are translated back to their normal names
  (ZBITPIX->BITPIX, ZNAXIS->NAXIS, ZNAXISn->NAXISn), because NAXIS1/NAXIS2 in
  the BINTABLE header describe the table, not the image. Compression-specific
  and table-structural keywords are dropped. No pixel data is read, so this is
  fast regardless of image size. Returns 0 on success. }
function NativeReadFzHeader(InStream, OutStream: TStream; out rmsg: string): integer;

{ True if the stream looks like a tile-compressed image (ZIMAGE = T in the
  first extension header). Cheap sniff used to decide the code path. }
function StreamIsTileCompressed(InStream: TStream): boolean;

implementation

const
  FITS_BLOCK = 2880;
  CARD_LEN   = 80;

type
  { minimal parsed FITS header: a list of raw 80-char cards plus quick lookup }
  TCardList = record
    cards : TStringList;   { raw 80-char cards, END excluded }
  end;

{------------------------------------------------------------------------------
  Small header helpers
------------------------------------------------------------------------------}

function CardKeyword(const card: string): string;
begin
  result := trim(copy(card, 1, 8));
end;

function CardRawValue(const card: string): string;
{ text between column 11 and the comment slash (respecting quoted strings) }
var
  i, n: integer;
  inq: boolean;
  s: string;
begin
  result := '';
  if length(card) < 10 then exit;
  if copy(card, 9, 2) <> '= ' then exit;   { not a valued card }
  s := copy(card, 11, length(card) - 10);
  inq := false;
  n := length(s);
  for i := 1 to n do
  begin
    if s[i] = '''' then inq := not inq;
    if (s[i] = '/') and (not inq) then
    begin
      result := trim(copy(s, 1, i - 1));
      exit;
    end;
  end;
  result := trim(s);
end;

function CardStringValue(const card: string): string;
{ value of a string card, quotes stripped, trailing spaces trimmed }
var
  v: string;
  p: PChar;
begin
  v := CardRawValue(card);
  if (length(v) >= 1) and (v[1] = '''') then
  begin
    p := PChar(v);
    result := AnsiExtractQuotedStr(p, '''');
    result := TrimRight(result);
  end
  else
    result := v;
end;

procedure HdrClear(var h: TCardList);
begin
  if h.cards = nil then h.cards := TStringList.Create
  else h.cards.Clear;
end;

procedure HdrFree(var h: TCardList);
begin
  if h.cards <> nil then FreeAndNil(h.cards);
end;

function HdrIndex(const h: TCardList; const key: string): integer;
var
  i: integer;
  k: string;
begin
  result := -1;
  k := UpperCase(trim(key));
  for i := 0 to h.cards.Count - 1 do
    if UpperCase(CardKeyword(h.cards[i])) = k then
    begin
      result := i;
      exit;
    end;
end;

function HdrGetStr(const h: TCardList; const key: string; out val: string): boolean;
var
  i: integer;
begin
  i := HdrIndex(h, key);
  result := i >= 0;
  if result then val := CardStringValue(h.cards[i]) else val := '';
end;

function HdrGetInt(const h: TCardList; const key: string; out val: integer): boolean;
var
  s: string;
begin
  result := HdrGetStr(h, key, s);
  if result then result := TryStrToInt(trim(s), val);
  if not result then val := 0;
end;

function HdrGetDbl(const h: TCardList; const key: string; out val: double): boolean;
var
  s: string;
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  result := HdrGetStr(h, key, s);
  if result then result := TryStrToFloat(trim(s), val, fs);
  if not result then val := 0;
end;

{ Read one FITS header (sequence of 2880 blocks up to END) from Stream at its
  current position. On return Stream is positioned at the first byte after the
  header (i.e. start of the data unit). Returns false on truncation. }
function ReadOneHeader(Stream: TStream; var h: TCardList): boolean;
var
  block: array[0..FITS_BLOCK - 1] of char;
  i, n: integer;
  card: string;
  ended: boolean;
begin
  HdrClear(h);
  ended := false;
  repeat
    n := Stream.Read(block, FITS_BLOCK);
    if n <> FITS_BLOCK then exit(false);
    for i := 0 to 35 do
    begin
      SetString(card, PChar(@block[i * CARD_LEN]), CARD_LEN);
      if UpperCase(CardKeyword(card)) = 'END' then
      begin
        ended := true;
        break;
      end;
      h.cards.Add(card);
    end;
  until ended;
  result := true;
end;

{------------------------------------------------------------------------------
  Card formatting for writing
------------------------------------------------------------------------------}

function PadCard(const s: string): string;
begin
  result := copy(s + StringOfChar(' ', CARD_LEN), 1, CARD_LEN);
end;

function MakeLogicalCard(const key: string; value: boolean; const comment: string): string;
var
  v, s: string;
begin
  if value then v := 'T' else v := 'F';
  s := copy(UpperCase(key) + '        ', 1, 8) + '= ' + StringOfChar(' ', 19) + v;
  { right-justify the value into columns 11..30 }
  s := copy(UpperCase(key) + '        ', 1, 8) + '= ' +
       copy(StringOfChar(' ', 20) + v, length(StringOfChar(' ', 20) + v) - 19, 20);
  if comment <> '' then s := s + ' / ' + comment;
  result := PadCard(s);
end;

function MakeIntCard(const key: string; value: int64; const comment: string): string;
var
  s, vs: string;
begin
  vs := IntToStr(value);
  s := copy(UpperCase(key) + '        ', 1, 8) + '= ' +
       copy(StringOfChar(' ', 20) + vs, length(StringOfChar(' ', 20) + vs) - 19, 20);
  if comment <> '' then s := s + ' / ' + comment;
  result := PadCard(s);
end;

function MakeFloatCard(const key: string; value: double; const comment: string): string;
var
  s, vs: string;
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  if value = round(value) then
    vs := FloatToStr(value, fs)
  else
    vs := FloatToStrF(value, ffGeneral, 15, 0, fs);
  if pos('.', vs) = 0 then vs := vs + '.0';
  s := copy(UpperCase(key) + '        ', 1, 8) + '= ' +
       copy(StringOfChar(' ', 20) + vs, length(StringOfChar(' ', 20) + vs) - 19, 20);
  if comment <> '' then s := s + ' / ' + comment;
  result := PadCard(s);
end;

function MakeStrCard(const key: string; const value, comment: string): string;
var
  s, vq: string;
begin
  vq := '''' + value + '''';
  { FITS string values are at least 8 chars inside the quotes }
  while length(vq) < 10 do vq := copy(vq, 1, length(vq) - 1) + ' ''';
  s := copy(UpperCase(key) + '        ', 1, 8) + '= ' + vq;
  if comment <> '' then s := s + ' / ' + comment;
  result := PadCard(s);
end;

{ Big-endian helpers for the BINTABLE }
procedure PutBE32(Stream: TStream; v: longword);
var b: array[0..3] of byte;
begin
  b[0] := (v shr 24) and $ff; b[1] := (v shr 16) and $ff;
  b[2] := (v shr 8) and $ff;  b[3] := v and $ff;
  Stream.Write(b, 4);
end;

{------------------------------------------------------------------------------
  Column parsing for the compressed BINTABLE.
  Returns byte offset of each named column within a row, or -1 if absent, and
  the total row width in bytes (which must equal NAXIS1).
------------------------------------------------------------------------------}

function TFormWidthBytes(const tform: string): integer;
{ width in bytes of one BINTABLE column given its TFORM string }
var
  s, letters, digits: string;
  i, rep: integer;
  ch: char;
  isP: boolean;
begin
  s := UpperCase(trim(tform));
  { variable-length array descriptor: 1PB(...), 1PJ(...), 1QB(...) etc.
    1P -> 8-byte descriptor (2x int32), 1Q -> 16-byte descriptor (2x int64) }
  isP := false;
  i := pos('P', s);
  if i > 0 then
  begin
    exit(8);
  end;
  i := pos('Q', s);
  if i > 0 then
  begin
    exit(16);
  end;
  { fixed column: leading repeat count then a type letter }
  digits := '';
  letters := '';
  for i := 1 to length(s) do
  begin
    ch := s[i];
    if (ch >= '0') and (ch <= '9') then digits := digits + ch
    else begin letters := ch; break; end;
  end;
  if digits = '' then rep := 1 else rep := StrToIntDef(digits, 1);
  case letters of
    'L','X','B','A': result := rep * 1;
    'I':             result := rep * 2;
    'J','E':         result := rep * 4;
    'K','D','C':     result := rep * 8;
    'M':             result := rep * 16;
    else             result := rep * 1;
  end;
  if isP then ;
end;

{------------------------------------------------------------------------------
  Build the CFITSIO 10000-entry random dither table (Park-Miller LCG).
------------------------------------------------------------------------------}

procedure BuildDitherTable(out tbl: array of single);
const
  A = 16807;
  M = 2147483647;
  Q = 127773;
  R = 2836;
var
  seed, i, hi: integer;
  temp: double;
begin
  { CFITSIO fits_init_randoms: seed = 1, fill all 10000 entries directly,
    no warm-up discard, values clamped just below 1.0 }
  seed := 1;
  for i := 0 to 10000 - 1 do
  begin
    hi := seed div Q;
    seed := A * (seed - hi * Q) - R * hi;
    if seed < 0 then seed := seed + M;
    temp := seed / M;
    if temp > 0.9999999 then temp := 0.9999999;
    tbl[i] := temp;
  end;
end;

{------------------------------------------------------------------------------
  StreamIsTileCompressed
------------------------------------------------------------------------------}

function StreamIsTileCompressed(InStream: TStream): boolean;
var
  h: TCardList;
  save: int64;
  s: string;
begin
  result := false;
  h.cards := nil;
  HdrClear(h);
  save := InStream.Position;
  try
    InStream.Position := 0;
    { primary header }
    if not ReadOneHeader(InStream, h) then exit;
    { skip primary data (NAXIS should be 0 for compressed files) }
    { read extension header }
    if not ReadOneHeader(InStream, h) then exit;
    if HdrGetStr(h, 'ZIMAGE', s) then
      result := (UpperCase(trim(s)) = 'T') or (UpperCase(trim(s)) = 'TRUE');
    if not result then
    begin
      { ZIMAGE may be logical without quotes; CardStringValue returns 'T' }
      if HdrGetStr(h, 'ZIMAGE', s) then result := UpperCase(trim(s)) = 'T';
    end;
  finally
    InStream.Position := save;
    HdrFree(h);
  end;
end;

{------------------------------------------------------------------------------
  Write a full uncompressed FITS image stream from a decoded float image.
  bitpix: 16 or -32. For bitpix=16 we write BSCALE/BZERO to reproduce the
  original unsigned scaling; for -32 we write raw floats (BZERO=0,BSCALE=1).
------------------------------------------------------------------------------}

procedure WriteUncompressedFits(OutStream: TStream;
  const img: Timage_array; width, height, naxis3, bitpix: integer;
  bscale, bzero: double; const extraCards: TStringList);
var
  hdr: TStringList;
  i, x, y, z, pad: integer;
  card: string;
  buf: string;
  fs: TFormatSettings;
  bcount: int64;
  wbe16: word;
  ibe: longword;
  fbe: longword;
  fval: single;
  ival: integer;
  zero: byte;
begin
  fs := DefaultFormatSettings; fs.DecimalSeparator := '.';
  hdr := TStringList.Create;
  try
    hdr.Add(PadCard('SIMPLE  =                    T / file conforms to FITS standard'));
    hdr.Add(MakeIntCard('BITPIX', bitpix, 'number of bits per data pixel'));
    if naxis3 > 1 then
      hdr.Add(MakeIntCard('NAXIS', 3, 'number of data axes'))
    else
      hdr.Add(MakeIntCard('NAXIS', 2, 'number of data axes'));
    hdr.Add(MakeIntCard('NAXIS1', width, 'length of data axis 1'));
    hdr.Add(MakeIntCard('NAXIS2', height, 'length of data axis 2'));
    if naxis3 > 1 then
      hdr.Add(MakeIntCard('NAXIS3', naxis3, 'length of data axis 3'));
    if bitpix = 16 then
    begin
      hdr.Add(MakeFloatCard('BZERO', bzero, 'offset data range to that of unsigned short'));
      hdr.Add(MakeFloatCard('BSCALE', bscale, 'default scaling factor'));
    end;
    { copy extra (WCS/instrument/history) cards, skipping structural ones }
    if extraCards <> nil then
      for i := 0 to extraCards.Count - 1 do
        hdr.Add(extraCards[i]);

    { write header padded to 2880 }
    for i := 0 to hdr.Count - 1 do
    begin
      card := hdr[i];
      OutStream.Write(card[1], CARD_LEN);
    end;
    card := PadCard('END');
    OutStream.Write(card[1], CARD_LEN);
    pad := (FITS_BLOCK - ((hdr.Count + 1) * CARD_LEN) mod FITS_BLOCK) mod FITS_BLOCK;
    card := StringOfChar(' ', CARD_LEN);
    i := 0;
    while i < pad do
    begin
      OutStream.Write(card[1], min(CARD_LEN, pad - i));
      inc(i, CARD_LEN);
    end;
  finally
    hdr.Free;
  end;

  { write pixel data, big-endian }
  bcount := 0;
  if bitpix = 16 then
  begin
    for z := 0 to naxis3 - 1 do
      for y := 0 to height - 1 do
        for x := 0 to width - 1 do
        begin
          { physical -> stored signed16: stored = (phys - bzero)/bscale }
          ival := round((img[z, y, x] - bzero) / bscale);
          if ival < -32768 then ival := -32768;
          if ival > 32767 then ival := 32767;
          wbe16 := word(smallint(ival));
          wbe16 := ((wbe16 shr 8) and $ff) or ((wbe16 shl 8) and $ff00);
          OutStream.Write(wbe16, 2);
          inc(bcount, 2);
        end;
  end
  else { bitpix = -32 }
  begin
    for z := 0 to naxis3 - 1 do
      for y := 0 to height - 1 do
        for x := 0 to width - 1 do
        begin
          fval := img[z, y, x];
          fbe := PLongword(@fval)^;
          ibe := ((fbe shr 24) and $ff) or ((fbe shr 8) and $ff00) or
                 ((fbe shl 8) and $ff0000) or ((fbe shl 24) and $ff000000);
          OutStream.Write(ibe, 4);
          inc(bcount, 4);
        end;
  end;

  { pad data unit to 2880 }
  zero := 0;
  pad := (FITS_BLOCK - (bcount mod FITS_BLOCK)) mod FITS_BLOCK;
  for i := 1 to pad do OutStream.Write(zero, 1);
end;

{------------------------------------------------------------------------------
  NativeUnpackFits
------------------------------------------------------------------------------}

function NativeUnpackFits(InStream, OutStream: TStream; out rmsg: string): integer;
var
  hp, hx: TCardList;
  s, zcmptype, zquantiz: string;
  znaxis, zbitpix, znaxis1, znaxis2, znaxis3: integer;
  ztile1, ztile2, ztile3: integer;
  bnaxis1, bnaxis2: integer;   { BINTABLE own geometry }
  pcount, tfields: integer;
  blocksize, bytepix: integer;
  zdither0: integer;
  bscale, bzero: double;
  i, k, coloff, w, idx: integer;
  tform, ttype: string;
  offComp, offGzip, offZscale, offZzero, offZblank: integer;
  colWidths: array of integer;
  colNames: array of string;
  sumWidth: integer;
  tableBytes, heapBytes: integer;
  tableBuf, heapBuf: PByte;
  dataStart: int64;
  p: Trice_decode_params;
  ditherTbl: array of single;
  img: Timage_array;
  outMax, outMin: single;
  errGzip, errDecode, errRange: boolean;
  errTile: integer;
  errMsg: string;
  extraCards: TStringList;
  outBitpix: integer;
  zval, zname: string;
  d: double;
  fitsFloat: boolean;
  gzipWarned: boolean;
begin
  result := 1;
  rmsg := '';
  hp.cards := nil; hx.cards := nil;
  tableBuf := nil; heapBuf := nil;
  extraCards := TStringList.Create;
  HdrClear(hp); HdrClear(hx);
  try
    InStream.Position := 0;
    if not ReadOneHeader(InStream, hp) then
    begin rmsg := 'cannot read primary header'; exit; end;
    { primary should be NAXIS=0; if it has data, skip it (defensive) }
    if HdrGetInt(hp, 'NAXIS', i) and (i > 0) then
    begin
      rmsg := 'unexpected primary data in compressed file'; exit;
    end;
    if not ReadOneHeader(InStream, hx) then
    begin rmsg := 'cannot read compressed BINTABLE header'; exit; end;

    { must be a compressed image }
    if not (HdrGetStr(hx, 'ZIMAGE', s) and (UpperCase(trim(s)) = 'T')) then
    begin rmsg := 'not a tile-compressed image (ZIMAGE<>T)'; exit; end;

    if not HdrGetStr(hx, 'ZCMPTYPE', zcmptype) then zcmptype := '';
    zcmptype := UpperCase(trim(zcmptype));
    if zcmptype <> 'RICE_1' then
    begin
      rmsg := 'unsupported ZCMPTYPE ''' + zcmptype + ''' (only RICE_1 supported)';
      exit;
    end;

    { image geometry from Z* keywords }
    HdrGetInt(hx, 'ZBITPIX', zbitpix);
    HdrGetInt(hx, 'ZNAXIS', znaxis);
    HdrGetInt(hx, 'ZNAXIS1', znaxis1);
    HdrGetInt(hx, 'ZNAXIS2', znaxis2);
    znaxis3 := 1;
    if znaxis >= 3 then HdrGetInt(hx, 'ZNAXIS3', znaxis3);
    if znaxis3 < 1 then znaxis3 := 1;

    HdrGetInt(hx, 'ZTILE1', ztile1);
    if not HdrGetInt(hx, 'ZTILE2', ztile2) then ztile2 := 1;
    if not HdrGetInt(hx, 'ZTILE3', ztile3) then ztile3 := 1;
    if ztile1 <= 0 then ztile1 := znaxis1;
    if ztile2 <= 0 then ztile2 := 1;
    if ztile3 <= 0 then ztile3 := 1;

    { BINTABLE own geometry - snapshot BEFORE overwriting with image dims }
    HdrGetInt(hx, 'NAXIS1', bnaxis1);
    HdrGetInt(hx, 'NAXIS2', bnaxis2);
    HdrGetInt(hx, 'PCOUNT', pcount);
    HdrGetInt(hx, 'TFIELDS', tfields);

    { BLOCKSIZE / BYTEPIX from ZNAMEn / ZVALn pairs }
    blocksize := 32;
    bytepix := abs(zbitpix) div 8;
    if bytepix < 1 then bytepix := 2;
    i := 1;
    while HdrGetStr(hx, 'ZNAME' + IntToStr(i), zname) do
    begin
      zname := UpperCase(trim(zname));
      if HdrGetStr(hx, 'ZVAL' + IntToStr(i), zval) then
      begin
        if zname = 'BLOCKSIZE' then blocksize := StrToIntDef(trim(zval), 32);
        if zname = 'BYTEPIX' then bytepix := StrToIntDef(trim(zval), bytepix);
      end;
      inc(i);
      if i > 100 then break;
    end;

    { quantisation for float images }
    if not HdrGetStr(hx, 'ZQUANTIZ', zquantiz) then zquantiz := 'NONE';
    zquantiz := UpperCase(trim(zquantiz));
    if not HdrGetInt(hx, 'ZDITHER0', zdither0) then zdither0 := 0;

    { BSCALE/BZERO come from the header under their NORMAL names (no ZBSCALE) }
    if not HdrGetDbl(hx, 'BSCALE', bscale) then bscale := 1.0;
    if not HdrGetDbl(hx, 'BZERO', bzero) then bzero := 0.0;

    { parse columns to find byte offsets in a row }
    offComp := -1; offGzip := -1; offZscale := -1; offZzero := -1; offZblank := -1;
    SetLength(colWidths, tfields);
    SetLength(colNames, tfields);
    coloff := 0;
    for i := 1 to tfields do
    begin
      if not HdrGetStr(hx, 'TFORM' + IntToStr(i), tform) then
      begin rmsg := 'missing TFORM' + IntToStr(i); exit; end;
      if not HdrGetStr(hx, 'TTYPE' + IntToStr(i), ttype) then ttype := '';
      ttype := UpperCase(trim(ttype));
      w := TFormWidthBytes(tform);
      colWidths[i - 1] := w;
      colNames[i - 1] := ttype;
      if ttype = 'COMPRESSED_DATA' then offComp := coloff
      else if ttype = 'GZIP_COMPRESSED_DATA' then offGzip := coloff
      else if ttype = 'ZSCALE' then offZscale := coloff
      else if ttype = 'ZZERO' then offZzero := coloff
      else if ttype = 'ZBLANK' then offZblank := coloff;
      inc(coloff, w);
    end;
    sumWidth := coloff;

    if offComp < 0 then
    begin rmsg := 'no COMPRESSED_DATA column'; exit; end;

    { cross-check row width }
    if sumWidth <> bnaxis1 then
    begin
      rmsg := Format('row width mismatch: columns sum %d <> NAXIS1 %d', [sumWidth, bnaxis1]);
      exit;
    end;

    { read table + heap }
    dataStart := InStream.Position;
    tableBytes := bnaxis1 * bnaxis2;
    heapBytes := pcount;
    if tableBytes < 0 then begin rmsg := 'bad table size'; exit; end;
    GetMem(tableBuf, max(1, tableBytes));
    if InStream.Read(tableBuf^, tableBytes) <> tableBytes then
    begin rmsg := 'truncated table'; exit; end;
    if heapBytes > 0 then
    begin
      GetMem(heapBuf, heapBytes + 16); { pad for decoder end-of-block check }
      if InStream.Read(heapBuf^, heapBytes) <> heapBytes then
      begin rmsg := 'truncated heap'; exit; end;
    end
    else
      heapBuf := nil;

    fitsFloat := (zbitpix = -32) or (zbitpix = -64);

    { build dither table if needed }
    p.dither_active := (zquantiz = 'SUBTRACTIVE_DITHER_1') or (zquantiz = 'SUBTRACTIVE_DITHER_2');
    p.dither_is_2 := (zquantiz = 'SUBTRACTIVE_DITHER_2');
    p.zquantiz_is_none := (zquantiz = 'NONE') or (not fitsFloat);
    if p.dither_active then
    begin
      SetLength(ditherTbl, 10000);
      BuildDitherTable(ditherTbl);
      p.dither_table_ptr := @ditherTbl[0];
    end
    else
      p.dither_table_ptr := nil;

    { fill remaining params }
    p.table_buffer := tableBuf;
    p.heap_buffer := heapBuf;
    p.heap_size := heapBytes;
    p.table_rowwidth := bnaxis1;
    p.table_rows := bnaxis2;

    p.tiles_x := (znaxis1 + ztile1 - 1) div ztile1;
    p.tiles_y := (znaxis2 + ztile2 - 1) div ztile2;
    if znaxis3 > 1 then
      p.tiles_z := (znaxis3 + ztile3 - 1) div ztile3
    else
      p.tiles_z := 1;
    p.total_tiles := p.tiles_x * p.tiles_y * p.tiles_z;

    p.ztile1_val := ztile1; p.ztile2_val := ztile2; p.ztile3_val := ztile3;
    p.znaxis1_val := znaxis1; p.znaxis2_val := znaxis2; p.znaxis3_val := znaxis3;
    p.img_width := znaxis1; p.img_height := znaxis2; p.img_naxis3 := znaxis3;

    p.off_comp := offComp; p.off_gzip := offGzip;
    p.off_zscale := offZscale; p.off_zzero := offZzero; p.off_zblank := offZblank;

    p.bytepix_val := bytepix; p.blocksize_val := blocksize;
    p.zdither0_val := zdither0;

    { for float without per-tile ZSCALE columns, header-level ZSCALE/ZZERO
      (rare); default to identity, per-tile columns override }
    if not HdrGetDbl(hx, 'ZSCALE', p.zscale_val) then p.zscale_val := 1.0;
    if not HdrGetDbl(hx, 'ZZERO', p.zzero_val) then p.zzero_val := 0.0;
    if not HdrGetInt(hx, 'ZBLANK', p.zblank_val) then p.zblank_val := 0;
    p.zblank_present := (offZblank >= 0) or (HdrIndex(hx, 'ZBLANK') >= 0);

    p.img_bscale := bscale; p.img_bzero := bzero;

    { fast path only valid for lossless 16-bit full-width single-row tiles }
    p.fastpath_possible := (not fitsFloat) and (bytepix = 2) and
                           (ztile1 = znaxis1) and (ztile2 = 1) and (znaxis3 = 1) and
                           p.zquantiz_is_none;

    { allocate target image }
    SetLength(img, znaxis3, znaxis2, znaxis1);

    { decode }
    rice_decode_tiles(img, p, outMax, outMin, errGzip, errDecode, errRange, errTile, errMsg);

    if errDecode then
    begin
      rmsg := Format('rice decode error on tile %d: %s', [errTile, errMsg]);
      exit;
    end;
    if errRange then
    begin
      rmsg := Format('heap descriptor out of range on tile %d', [errTile]);
      exit;
    end;
    gzipWarned := errGzip;

    { choose output bitpix: keep original ZBITPIX }
    if fitsFloat then outBitpix := -32 else outBitpix := 16;

    { collect header cards to carry over (WCS/instrument/history), skipping
      compression + structural keywords }
    for i := 0 to hx.cards.Count - 1 do
    begin
      k := HdrIndex(hx, ''); { unused }
      s := UpperCase(CardKeyword(hx.cards[i]));
      if (s = 'XTENSION') or (s = 'BITPIX') or (s = 'NAXIS') or
         (s = 'NAXIS1') or (s = 'NAXIS2') or (s = 'NAXIS3') or
         (s = 'PCOUNT') or (s = 'GCOUNT') or (s = 'TFIELDS') or
         (s = 'BSCALE') or (s = 'BZERO') or (s = 'END') or (s = 'SIMPLE') or
         (s = 'EXTEND') then continue;
      if (copy(s, 1, 5) = 'TTYPE') or (copy(s, 1, 5) = 'TFORM') or
         (copy(s, 1, 5) = 'TSCAL') or (copy(s, 1, 5) = 'TZERO') or
         (copy(s, 1, 1) = 'Z') or (copy(s, 1, 4) = 'ZVAL') then continue;
      extraCards.Add(hx.cards[i]);
    end;

    WriteUncompressedFits(OutStream, img, znaxis1, znaxis2, znaxis3, outBitpix,
      bscale, bzero, extraCards);

    if gzipWarned then
      rmsg := 'note: some tiles used GZIP compression (not supported), left blank';

    result := 0;
  finally
    if tableBuf <> nil then FreeMem(tableBuf);
    if heapBuf <> nil then FreeMem(heapBuf);
    extraCards.Free;
    HdrFree(hp); HdrFree(hx);
  end;
end;

{------------------------------------------------------------------------------
  NativeReadFzHeader - header only, no pixel decompression
------------------------------------------------------------------------------}

function NativeReadFzHeader(InStream, OutStream: TStream; out rmsg: string): integer;
var
  hp, hx: TCardList;
  hdr: TStringList;
  s, key, zcmptype: string;
  znaxis, zbitpix, zn: integer;
  i, pad: integer;
  card: string;
begin
  result := 1;
  rmsg := '';
  hp.cards := nil; hx.cards := nil;
  HdrClear(hp); HdrClear(hx);
  hdr := TStringList.Create;
  try
    InStream.Position := 0;
    { first HDU: empty primary stub (SIMPLE=T, NAXIS=0, no data unit) }
    if not ReadOneHeader(InStream, hp) then
    begin rmsg := 'cannot read primary header'; exit; end;
    if HdrGetInt(hp, 'NAXIS', i) and (i > 0) then
    begin rmsg := 'unexpected primary data in compressed file'; exit; end;

    { second HDU: the BINTABLE extension holding the compressed image.
      Its header is plain ASCII and carries the original image's keywords. }
    if not ReadOneHeader(InStream, hx) then
    begin rmsg := 'cannot read compressed BINTABLE header'; exit; end;

    if not (HdrGetStr(hx, 'ZIMAGE', s) and (UpperCase(trim(s)) = 'T')) then
    begin rmsg := 'not a tile-compressed image (ZIMAGE<>T)'; exit; end;

    if not HdrGetStr(hx, 'ZCMPTYPE', zcmptype) then zcmptype := '';
    zcmptype := UpperCase(trim(zcmptype));
    if zcmptype <> 'RICE_1' then
    begin
      rmsg := 'unsupported ZCMPTYPE ''' + zcmptype + ''' (only RICE_1 supported)';
      exit;
    end;

    { original image geometry comes from the Z* keywords, NOT from the
      BINTABLE's own NAXIS1/NAXIS2 (which describe the table rows). }
    if not HdrGetInt(hx, 'ZBITPIX', zbitpix) then zbitpix := 16;
    if not HdrGetInt(hx, 'ZNAXIS', znaxis) then znaxis := 2;

    hdr.Add(PadCard('SIMPLE  =                    T / file conforms to FITS standard'));
    hdr.Add(MakeIntCard('BITPIX', zbitpix, 'number of bits per data pixel'));
    hdr.Add(MakeIntCard('NAXIS', znaxis, 'number of data axes'));
    for i := 1 to znaxis do
    begin
      if not HdrGetInt(hx, 'ZNAXIS' + IntToStr(i), zn) then zn := 0;
      hdr.Add(MakeIntCard('NAXIS' + IntToStr(i), zn, 'length of data axis ' + IntToStr(i)));
    end;

    { copy every descriptive keyword through unchanged: WCS (CRVALn, CRPIXn,
      CDELTn, CTYPEn, CDi_j), RA/DEC/OBJCTRA/OBJCTDEC, SECPIX, XPIXSZ/YPIXSZ,
      FOCALLEN, EXPTIME, INSTRUME, TELESCOP, DATE-OBS, OBJECT, HISTORY, ...
      while dropping table-structural and compression-specific cards. }
    for i := 0 to hx.cards.Count - 1 do
    begin
      key := UpperCase(CardKeyword(hx.cards[i]));
      if (key = 'XTENSION') or (key = 'BITPIX') or (key = 'NAXIS') or
         (key = 'NAXIS1') or (key = 'NAXIS2') or (key = 'NAXIS3') or
         (key = 'PCOUNT') or (key = 'GCOUNT') or (key = 'TFIELDS') or
         (key = 'SIMPLE') or (key = 'EXTEND') or (key = 'END') then continue;
      { BINTABLE column descriptors }
      if (copy(key, 1, 5) = 'TTYPE') or (copy(key, 1, 5) = 'TFORM') or
         (copy(key, 1, 5) = 'TSCAL') or (copy(key, 1, 5) = 'TZERO') or
         (copy(key, 1, 5) = 'TUNIT') or (copy(key, 1, 5) = 'TDIM') or
         (copy(key, 1, 4) = 'THEA') then continue;
      { all compression keywords (ZIMAGE, ZCMPTYPE, ZBITPIX, ZNAXISn, ZTILEn,
        ZNAMEn, ZVALn, ZQUANTIZ, ZDITHER0, ZSIMPLE, ZEXTEND, ZSCALE, ZZERO...) }
      if copy(key, 1, 1) = 'Z' then continue;
      hdr.Add(hx.cards[i]);
    end;

    { emit as a normal primary header: cards + END, padded to 2880 }
    for i := 0 to hdr.Count - 1 do
    begin
      card := hdr[i];
      OutStream.Write(card[1], CARD_LEN);
    end;
    card := PadCard('END');
    OutStream.Write(card[1], CARD_LEN);
    pad := (FITS_BLOCK - (((hdr.Count + 1) * CARD_LEN) mod FITS_BLOCK)) mod FITS_BLOCK;
    if pad > 0 then
    begin
      card := StringOfChar(' ', pad);
      OutStream.Write(card[1], pad);
    end;

    OutStream.Position := 0;
    result := 0;
  finally
    hdr.Free;
    HdrFree(hp); HdrFree(hx);
  end;
end;

{------------------------------------------------------------------------------
  NativePackFits - compress a 16-bit integer FITS image into RICE_1 .fz
------------------------------------------------------------------------------}

function NativePackFits(InStream, OutStream: TStream; out rmsg: string): integer;
var
  h: TCardList;
  bitpix, naxis, naxis1, naxis2, naxis3, i, x, y, pad: integer;
  bscale, bzero: double;
  img: Timage_array;
  dataStart: int64;
  rowbuf: array of byte;
  raw16: word;
  ival: smallint;
  fval: single;
  ibe: longword;
  tile_data: array of PByte;
  tile_len: array of integer;
  ok: boolean;
  errRow: integer;
  errMsg: string;
  hdr: TStringList;
  card: string;
  totalHeap: int64;
  cumOffset: longword;
  extraCards: TStringList;
  s: string;
  zero: byte;
  dataUnitBytes: int64;
  rd: longword;
begin
  result := 1;
  rmsg := '';
  h.cards := nil;
  HdrClear(h);
  extraCards := TStringList.Create;
  tile_data := nil;
  try
    InStream.Position := 0;
    if not ReadOneHeader(InStream, h) then
    begin rmsg := 'cannot read FITS header'; exit; end;

    if not HdrGetInt(h, 'BITPIX', bitpix) then
    begin rmsg := 'no BITPIX'; exit; end;
    if bitpix <> 16 then
    begin rmsg := 'native Rice compression supports only BITPIX=16'; exit; end;

    HdrGetInt(h, 'NAXIS', naxis);
    HdrGetInt(h, 'NAXIS1', naxis1);
    HdrGetInt(h, 'NAXIS2', naxis2);
    naxis3 := 1;
    if naxis >= 3 then HdrGetInt(h, 'NAXIS3', naxis3);
    if naxis3 < 1 then naxis3 := 1;
    if naxis3 <> 1 then
    begin rmsg := 'native Rice compression supports only mono (NAXIS3=1)'; exit; end;
    if (naxis1 <= 0) or (naxis2 <= 0) then
    begin rmsg := 'bad image dimensions'; exit; end;

    if not HdrGetDbl(h, 'BSCALE', bscale) then bscale := 1.0;
    if not HdrGetDbl(h, 'BZERO', bzero) then bzero := 0.0;

    { read image into Timage_array as physical float values }
    dataStart := InStream.Position;
    SetLength(img, 1, naxis2, naxis1);
    SetLength(rowbuf, naxis1 * 2);
    for y := 0 to naxis2 - 1 do
    begin
      if InStream.Read(rowbuf[0], naxis1 * 2) <> naxis1 * 2 then
      begin rmsg := 'truncated image data'; exit; end;
      for x := 0 to naxis1 - 1 do
      begin
        { big-endian signed16 -> physical }
        raw16 := (word(rowbuf[x * 2]) shl 8) or word(rowbuf[x * 2 + 1]);
        ival := smallint(raw16);
        img[0, y, x] := ival * bscale + bzero;
      end;
    end;

    { carry over non-structural header cards }
    for i := 0 to h.cards.Count - 1 do
    begin
      s := UpperCase(CardKeyword(h.cards[i]));
      if (s = 'SIMPLE') or (s = 'BITPIX') or (s = 'NAXIS') or
         (s = 'NAXIS1') or (s = 'NAXIS2') or (s = 'NAXIS3') or
         (s = 'EXTEND') or (s = 'BSCALE') or (s = 'BZERO') or
         (s = 'PCOUNT') or (s = 'GCOUNT') or (s = 'XTENSION') or (s = 'END') then
        continue;
      if (copy(s, 1, 1) = 'Z') and
         ((s = 'ZIMAGE') or (s = 'ZCMPTYPE') or (copy(s, 1, 5) = 'ZNAXI') or
          (copy(s, 1, 5) = 'ZTILE') or (copy(s, 1, 4) = 'ZVAL') or
          (copy(s, 1, 5) = 'ZNAME') or (s = 'ZBITPIX') or (s = 'ZQUANTIZ') or
          (s = 'ZDITHER0') or (s = 'ZSIMPLE') or (s = 'ZEXTEND')) then
        continue;
      extraCards.Add(h.cards[i]);
    end;

    { encode all rows }
    SetLength(tile_data, naxis2);
    SetLength(tile_len, naxis2);
    for i := 0 to naxis2 - 1 do begin tile_data[i] := nil; tile_len[i] := 0; end;

    rice_encode_rows(img, naxis1, naxis2, 32, tile_data, tile_len, ok, errRow, errMsg);
    if not ok then
    begin
      rmsg := Format('rice encode error on row %d: %s', [errRow, errMsg]);
      exit;
    end;

    totalHeap := 0;
    for i := 0 to naxis2 - 1 do inc(totalHeap, tile_len[i]);

    { ---- write primary HDU (empty) ---- }
    hdr := TStringList.Create;
    try
      hdr.Add(PadCard('SIMPLE  =                    T / file conforms to FITS standard'));
      hdr.Add(MakeIntCard('BITPIX', 8, 'array data type'));
      hdr.Add(MakeIntCard('NAXIS', 0, 'number of array dimensions'));
      hdr.Add(MakeLogicalCard('EXTEND', true, ''));
      for i := 0 to hdr.Count - 1 do
      begin card := hdr[i]; OutStream.Write(card[1], CARD_LEN); end;
      card := PadCard('END'); OutStream.Write(card[1], CARD_LEN);
      pad := (FITS_BLOCK - (((hdr.Count + 1) * CARD_LEN) mod FITS_BLOCK)) mod FITS_BLOCK;
      card := StringOfChar(' ', pad);
      if pad > 0 then OutStream.Write(card[1], pad);
    finally
      hdr.Free;
    end;

    { ---- write BINTABLE header ---- }
    hdr := TStringList.Create;
    try
      hdr.Add(MakeStrCard('XTENSION', 'BINTABLE', 'binary table extension'));
      hdr.Add(MakeIntCard('BITPIX', 8, '8-bit bytes'));
      hdr.Add(MakeIntCard('NAXIS', 2, '2-dimensional binary table'));
      hdr.Add(MakeIntCard('NAXIS1', 8, 'width of table in bytes'));
      hdr.Add(MakeIntCard('NAXIS2', naxis2, 'number of rows in table'));
      hdr.Add(MakeIntCard('PCOUNT', totalHeap, 'size of special data area'));
      hdr.Add(MakeIntCard('GCOUNT', 1, 'one data group'));
      hdr.Add(MakeIntCard('TFIELDS', 1, 'number of fields in each row'));
      hdr.Add(MakeStrCard('TTYPE1', 'COMPRESSED_DATA', 'label for field 1'));
      hdr.Add(MakeStrCard('TFORM1', '1PB(0)', 'data format of field: variable length'));
      hdr.Add(MakeLogicalCard('ZIMAGE', true, 'extension contains compressed image'));
      hdr.Add(MakeLogicalCard('ZSIMPLE', true, 'original image had SIMPLE = T'));
      hdr.Add(MakeIntCard('ZBITPIX', 16, 'original image had BITPIX = 16'));
      hdr.Add(MakeIntCard('ZNAXIS', 2, 'original image had NAXIS = 2'));
      hdr.Add(MakeIntCard('ZNAXIS1', naxis1, 'original image axis 1'));
      hdr.Add(MakeIntCard('ZNAXIS2', naxis2, 'original image axis 2'));
      hdr.Add(MakeIntCard('ZTILE1', naxis1, 'size of tiles axis 1'));
      hdr.Add(MakeIntCard('ZTILE2', 1, 'size of tiles axis 2'));
      hdr.Add(MakeStrCard('ZCMPTYPE', 'RICE_1', 'compression algorithm'));
      hdr.Add(MakeStrCard('ZNAME1', 'BLOCKSIZE', 'compression block size'));
      hdr.Add(MakeIntCard('ZVAL1', 32, 'pixels per block'));
      hdr.Add(MakeStrCard('ZNAME2', 'BYTEPIX', 'bytes per pixel'));
      hdr.Add(MakeIntCard('ZVAL2', 2, '16-bit integers'));
      hdr.Add(MakeFloatCard('BZERO', 32768.0, 'offset for unsigned short'));
      hdr.Add(MakeFloatCard('BSCALE', 1.0, 'default scaling'));
      { carry over WCS / instrument / history cards }
      for i := 0 to extraCards.Count - 1 do hdr.Add(extraCards[i]);

      for i := 0 to hdr.Count - 1 do
      begin card := hdr[i]; OutStream.Write(card[1], CARD_LEN); end;
      card := PadCard('END'); OutStream.Write(card[1], CARD_LEN);
      pad := (FITS_BLOCK - (((hdr.Count + 1) * CARD_LEN) mod FITS_BLOCK)) mod FITS_BLOCK;
      card := StringOfChar(' ', pad);
      if pad > 0 then OutStream.Write(card[1], pad);
    finally
      hdr.Free;
    end;

    { ---- write table rows: (nelem, offset) big-endian per row ---- }
    cumOffset := 0;
    for i := 0 to naxis2 - 1 do
    begin
      PutBE32(OutStream, longword(tile_len[i]));
      PutBE32(OutStream, cumOffset);
      inc(cumOffset, longword(tile_len[i]));
    end;

    { ---- write heap: the tile bytes ---- }
    for i := 0 to naxis2 - 1 do
      if (tile_len[i] > 0) and (tile_data[i] <> nil) then
        OutStream.Write(tile_data[i]^, tile_len[i]);

    { pad the data unit (table + heap) to 2880 }
    dataUnitBytes := int64(naxis2) * 8 + totalHeap;
    pad := (FITS_BLOCK - (dataUnitBytes mod FITS_BLOCK)) mod FITS_BLOCK;
    zero := 0;
    for i := 1 to pad do OutStream.Write(zero, 1);

    result := 0;
  finally
    if tile_data <> nil then
      for i := 0 to High(tile_data) do
        if tile_data[i] <> nil then FreeMem(tile_data[i]);
    extraCards.Free;
    HdrFree(h);
  end;
end;

end.
