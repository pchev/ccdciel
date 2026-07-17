unit cu_tiff;

{This version is shared and now part of CCDCiel. Copyright (C) 2026 Patrick Chevalley}

{Writes and reads uncompressed or compressed tiff files from an to an image array
This unit can be called ASTAP-TIFF since it is developped for the ASTAP program
Copyright 2018, 2026 by Han Kleijn, www.hnsky.org
email: han.k.. at...hnsky.org}

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

{2026-7: Multi-threaded Deflate compression and decompression.
         Compressed files are now written as multiple strips (one strip per CPU core),
         each strip an independent Deflate stream compressed in its own thread.
         Reading of multi-strip Deflate files is decompressed in parallel as well.
         Single-strip files (older ASTAP files) still read fine but sequentially.
         Fixed: flip_H previously had no effect (buffer index equalled source index).}


interface

type
  Timg_array = array of array of array of Single;//Same as Timage_array in unit astap_main but different name to avoid name conflict.

function save_tiff_new(img: Timg_array; var filen2: string;  description: ansistring; bitpix: integer; flip_H, flip_V, overwrite: boolean; compressionlevel: integer): boolean; //save to TIFF file, compressionlevel 0..3 equals clnone,clfastest, cldefault, clmax

 {16 bit procedures.}
function save_tiff_16(img: Timg_array; filen2, description: ansistring; flip_H, flip_V: boolean; compressionlevel: integer): boolean; //save to 16 bit gray scale TIFF file, compressionlevel 0..3 equals clnone,clfastest, cldefault, clmax
function save_tiff_48(img: Timg_array; filen2, description: ansistring; flip_H, flip_V: boolean; compressionlevel: integer): boolean; //save to 48=3x16 color TIFF file, compressionlevel 0..3 equals clnone,clfastest, cldefault, clmax

{32 bit procedures}
function save_tiff_32(img: Timg_array; filen2, description: ansistring; flip_H, flip_V: boolean; compressionlevel: integer): boolean; //save to 32 bit float gray scale TIFF file, compressionlevel 0..3 equals clnone,clfastest, cldefault, clmax
function save_tiff_96(img: Timg_array; filen2, description: ansistring; flip_H, flip_V: boolean; compressionlevel: integer): boolean; //save to 96=3x32 color TIFF file, compressionlevel 0..3 equals clnone,clfastest, cldefault, clmax

{Reading procedures}
procedure read_tiff(filen: string; var img: Timg_array; out description: string; out bitspersample: word; out measured_max : double; out theresult: boolean); //no compression, lzw compressed, zip compressed, 8,16,24,32,48,64,96 bit


implementation

uses
  SysUtils, Classes, dialogs, math,
  zstream, //for ZIP compression/decompression
  {$ifndef mswindows}
  unit_mtpcpu, //GetSystemThreadCount
  {$endif}
  FPReadTiff;//for LZW decompression


function get_thread_count: integer; //nr of logical cores
begin
  {$ifdef mswindows}
  result := System.CPUCount;//works in Windows and Linux virtual machine but not in native Linux or Darwin
  {$else} {unix}
  result := GetSystemThreadCount;
  {$endif}
  if result < 1 then result := 1;
end;



type
  TDirEntry = record
    _Tag: Word;
    _Type: Word;
    _Count: LongInt;
    _Value: LongInt;
  end;

const
  SoftwareName = 'ASTAP' + #0; {GIMP like to have this #0}

  { TIFF File Header: }
  TifHeader: array[0..7] of Byte = (
    $49, $49,                 { Intel byte order }
    $2a, $00,                 { TIFF version (42) }
    $08, $00, $00, $00 );     { Pointer to the first directory. Will be updated later }

  size16 = 16;
  NoOfDirsBW16: array[0..1] of Byte = (size16, $00); { Number of tags within the directory }
  DirectoryBW16: array[0..size16 - 1] of TDirEntry = (
    (_Tag: $00FE; _Type: $0004; _Count: $00000001; _Value: $00000000), {0 NewSubFile: Image with full solution (0) }
    (_Tag: $0100; _Type: $0003; _Count: $00000001; _Value: $00000000), {1 ImageWidth:      Value will be set later }
    (_Tag: $0101; _Type: $0003; _Count: $00000001; _Value: $00000000), {2 ImageLength:     Value will be set later }
    (_Tag: $0102; _Type: $0003; _Count: $00000001; _Value: $00000010), {3 BitsPerSample $10=16 ,no address         }
    (_Tag: $0103; _Type: $0003; _Count: $00000001; _Value: $00000001), {4 Compression:     No compression          }
    (_Tag: $0106; _Type: $0003; _Count: $00000001; _Value: $00000001), {5 PhotometricInterpretation: 1 = BlackIsZero.}
    (_Tag: $010E; _Type: $0002; _Count: $0000000A; _Value: $00000000), {6  Image Description. _Count will be updated later  }
    (_Tag: $0111; _Type: $0004; _Count: $00000001; _Value: $00000000), {7 StripOffsets: Ptr to the adress of the image data }
    (_Tag: $0115; _Type: $0003; _Count: $00000001; _Value: $00000001), {8 SamplesPerPixels: 1                      }
    (_Tag: $0116; _Type: $0004; _Count: $00000001; _Value: $00000000), {9 RowsPerStrip: Value will be set later    }
    (_Tag: $0117; _Type: $0004; _Count: $00000001; _Value: $00000000), {10 StripByteCounts: xs*ys bytes pro strip   }
    (_Tag: $011A; _Type: $0005; _Count: $00000001; _Value: $00000000), {11 X-Resolution: Adresse                    }
    (_Tag: $011B; _Type: $0005; _Count: $00000001; _Value: $00000000), {12 Y-Resolution: (Adresse)                  }
    (_Tag: $0128; _Type: $0003; _Count: $00000001; _Value: $00000002), {13 Resolution Unit: (2)= Unit ZOLL          }
    (_Tag: $0131; _Type: $0002; _Count: $0000000A; _Value: $00000000), {14 Software:                                }
    (_Tag: $0153; _Type: $0003; _Count: $00000001; _Value: $00000001)); {15 Sampleformat  integer=1                  }


  size32 = 16;
  NoOfDirsBW32: array[0..1] of Byte = (size32, $00); { Number of tags within the directory }
  DirectoryBW32: array[0..size32 - 1] of TDirEntry = (
    (_Tag: $00FE; _Type: $0004; _Count: $00000001; _Value: $00000000), {0 NewSubFile: Image with full solution (0) }
    (_Tag: $0100; _Type: $0003; _Count: $00000001; _Value: $00000000), {1 ImageWidth:      Value will be set later }
    (_Tag: $0101; _Type: $0003; _Count: $00000001; _Value: $00000000), {2 ImageLength:     Value will be set later }
    (_Tag: $0102; _Type: $0003; _Count: $00000001; _Value: $00000020), {3 BitsPerSample $20=32 ,no address         }
    (_Tag: $0103; _Type: $0003; _Count: $00000001; _Value: $00000001), {4 Compression:     No compression          }
    (_Tag: $0106; _Type: $0003; _Count: $00000001; _Value: $00000001), {5 PhotometricInterpretation[0, 1], 1 = BlackIsZero.}
    (_Tag: $010E; _Type: $0002; _Count: $0000000A; _Value: $00000000), {6  Image Description. _Count will be updated later  }
    (_Tag: $0111; _Type: $0004; _Count: $00000001; _Value: $00000000), {7 StripOffsets: Ptr to the adress of the image data }
    (_Tag: $0115; _Type: $0003; _Count: $00000001; _Value: $00000001), {8 SamplesPerPixels: 1                      }
    (_Tag: $0116; _Type: $0004; _Count: $00000001; _Value: $00000000), {9 RowsPerStrip: Value will be set later    }
    (_Tag: $0117; _Type: $0004; _Count: $00000001; _Value: $00000000), {10 StripByteCounts: xs*ys bytes pro strip   }
    (_Tag: $011A; _Type: $0005; _Count: $00000001; _Value: $00000000), {11 X-Resolution: Adresse                   }
    (_Tag: $011B; _Type: $0005; _Count: $00000001; _Value: $00000000), {12 Y-Resolution: (Adresse)                 }
    (_Tag: $0128; _Type: $0003; _Count: $00000001; _Value: $00000002), {13 Resolution Unit: (2)= Unit ZOLL         }
    (_Tag: $0131; _Type: $0002; _Count: $0000000A; _Value: $00000000), {14 Software:                               }
    (_Tag: $0153; _Type: $0003; _Count: $00000001; _Value: $00000003)); {15 Sampleformat  float=3                   }


  size48 = 17;
  NoOfDirsRGB48: array[0..1] of Byte = (size48, $00); { Number of tags within the directory }
  DirectoryRGB48: array[0..size48 - 1] of TDirEntry = (
    (_Tag: $00FE; _Type: $0004; _Count: $00000001; _Value: $00000000), {0 NewSubFile:      Image with full solution (0) }
    (_Tag: $0100; _Type: $0003; _Count: $00000001; _Value: $00000000), {1 ImageWidth:      Value will be set later      }
    (_Tag: $0101; _Type: $0003; _Count: $00000001; _Value: $00000000), {2 ImageLength:     Value will be set later      }
    (_Tag: $0102; _Type: $0003; _Count: $00000003; _Value: $00000000), {3 BitsPerSample address will be written later   }
    (_Tag: $0103; _Type: $0003; _Count: $00000001; _Value: $00000001), {4 Compression:     No compression               }
    (_Tag: $0106; _Type: $0003; _Count: $00000001; _Value: $00000002), {5 PhotometricInterpretation: 2 = colour         }
    (_Tag: $010E; _Type: $0002; _Count: $0000000A; _Value: $00000000), {6  Image Description._Count will be updated later   }
    (_Tag: $0111; _Type: $0004; _Count: $00000001; _Value: $00000000), {7 StripOffsets: Ptr to the adress of the image data }
    (_Tag: $0115; _Type: $0003; _Count: $00000001; _Value: $00000003), {8 SamplesPerPixels: 3                         }
    (_Tag: $0116; _Type: $0004; _Count: $00000001; _Value: $00000000), {9 RowsPerStrip: Value will be set later         }
    (_Tag: $0117; _Type: $0004; _Count: $00000001; _Value: $00000000), {10  StripByteCounts: xs*ys bytes pro strip  }
    (_Tag: $011A; _Type: $0005; _Count: $00000001; _Value: $00000000), {11 X-Resolution: Adresse                   }
    (_Tag: $011B; _Type: $0005; _Count: $00000001; _Value: $00000000), {12 Y-Resolution: (Adresse)                 }
    (_Tag: $011C; _Type: $0003; _Count: $00000001; _Value: $00000001), {13 PlanarConfiguration: Pixel data will be stored continous       }
    (_Tag: $0128; _Type: $0003; _Count: $00000001; _Value: $00000002), {14 Resolution Unit: (2)= Unit ZOLL         }
    (_Tag: $0131; _Type: $0002; _Count: $0000000A; _Value: $00000000), {15 Software                                }
    (_Tag: $0153; _Type: $0003; _Count: $00000001; _Value: $00000001)); {16 Sampleformat  integer=1                 }


  size96 = 17;
  NoOfDirsRGB96: array[0..1] of Byte = (size96, $00); { Number of tags within the directory }
  DirectoryRGB96: array[0..size96 - 1] of TDirEntry = (
    (_Tag: $00FE; _Type: $0004; _Count: $00000001; _Value: $00000000), {0 NewSubFile:      Image with full solution (0) }
    (_Tag: $0100; _Type: $0003; _Count: $00000001; _Value: $00000000), {1 ImageWidth:      Value will be set later      }
    (_Tag: $0101; _Type: $0003; _Count: $00000001; _Value: $00000000), {2 ImageLength:     Value will be set later      }
    (_Tag: $0102; _Type: $0003; _Count: $00000003; _Value: $00000000), {3 BitsPerSample address will be written later   }
    (_Tag: $0103; _Type: $0003; _Count: $00000001; _Value: $00000001), {4 Compression:     No compression               }
    (_Tag: $0106; _Type: $0003; _Count: $00000001; _Value: $00000002), {5 PhotometricInterpretation:  2 = colour        }
    (_Tag: $010E; _Type: $0002; _Count: $0000000A; _Value: $00000000), {6  Image Description._Count will be updated later   }
    (_Tag: $0111; _Type: $0004; _Count: $00000001; _Value: $00000000), {7 StripOffsets: Ptr to the adress of the image data }
    (_Tag: $0115; _Type: $0003; _Count: $00000001; _Value: $00000003), {8 SamplesPerPixels: 3                           }
    (_Tag: $0116; _Type: $0004; _Count: $00000001; _Value: $00000000), {9 RowsPerStrip: Value will be set later         }
    (_Tag: $0117; _Type: $0004; _Count: $00000001; _Value: $00000000), {10 StripByteCounts: xs*ys bytes pro strip        }
    (_Tag: $011A; _Type: $0005; _Count: $00000001; _Value: $00000000), {11 X-Resolution: Adresse                        }
    (_Tag: $011B; _Type: $0005; _Count: $00000001; _Value: $00000000), {12 Y-Resolution: (Adresse)                      }
    (_Tag: $011C; _Type: $0003; _Count: $00000001; _Value: $00000001), {13 PlanarConfiguration: Pixel data will be stored continous          }
    (_Tag: $0128; _Type: $0003; _Count: $00000001; _Value: $00000002), {14 Resolution Unit: (2)= Unit ZOLL              }
    (_Tag: $0131; _Type: $0002; _Count: $0000000A; _Value: $00000000), {15 Software                                     }
    (_Tag: $0153; _Type: $0003; _Count: $00000001; _Value: $00000003)); {16 Sampleformat  float=3                        }


  NullString: array[0..3] of Byte = ($00, $00, $00, $00);
  X_Res_Value: array[0..7] of Byte = ($6D, $03, $00, $00, $0A, $00, $00, $00); { Value for X-Resolution: 87,7 Pixel/Zoll (SONY SCREEN) }
  Y_Res_Value: array[0..7] of Byte = ($6D, $03, $00, $00, $0A, $00, $00, $00); { Value for Y-Resolution: 87,7 Pixel/Zoll }


  BitsPerSample48: array[0..2] of Word = ($0010, $0010, $0010); {8 or 16=$10}
  BitsPerSample96: array[0..2] of Word = ($0020, $0020, $0020); {8 or 16=$10}


{ ═══════════════════════════════════════════════════════════════════════════
  Shared row-packing and multi-threaded strip compression
  ═══════════════════════════════════════════════════════════════════════════ }
type
  TTiffFormat = (tfGray16, tfGray32, tfRGB48, tfRGB96);
  TStripStreams = array of TMemoryStream;

const
  bytes_per_pixel: array[TTiffFormat] of integer = (2, 4, 6, 12);


procedure fill_tiff_row(const img: Timg_array; fmt: TTiffFormat; k, width2: integer;
                        flip_H: boolean; var buf: array of byte);
{Pack one image row (source row k) into buf in TIFF little-endian byte order.
 Note buffer position is j, source position is m, so flip_H really flips.}
var
  j, m: integer;
  dum: double;

  function clamp16(v: double): word; inline;
  begin
    if v > $FFFF then v := $FFFF;
    if v < 0 then v := 0;
    result := word(round(v));
  end;

begin
  for j := 0 to width2 - 1 do
  begin
    if flip_H then m := width2 - 1 - j else m := j;
    case fmt of
      tfGray16:
        PWord(@buf[j * 2])^ := NtoLE(clamp16(img[0, k, m])); {explicit little-endian, safe on all platforms}
      tfGray32:
        PSingle(@buf[j * 4])^ := img[0, k, m] / 65535; {write float in range 0..1}
      tfRGB48:
        begin
          PWord(@buf[j * 6    ])^ := NtoLE(clamp16(img[0, k, m])); {red}
          PWord(@buf[j * 6 + 2])^ := NtoLE(clamp16(img[1, k, m])); {green}
          PWord(@buf[j * 6 + 4])^ := NtoLE(clamp16(img[2, k, m])); {blue}
        end;
      tfRGB96:
        begin
          PSingle(@buf[j * 12    ])^ := img[0, k, m] / 65535; {write floats in range 0..1}
          PSingle(@buf[j * 12 + 4])^ := img[1, k, m] / 65535;
          PSingle(@buf[j * 12 + 8])^ := img[2, k, m] / 65535;
        end;
    end;
  end;
end;


type
  TStripCompressThread = class(TThread)
  private
    Fimg: Timg_array; {dynamic array assignment only copies the reference, data is shared}
    Ffmt: TTiffFormat;
    FflipH, FflipV: boolean;
    Flevel: TCompressionLevel;
    Fwidth, Fheight, FstripRows, Fstrip: integer;
    Fstrips: ^TStripStreams; {each thread writes only its own index, so no locking required}
  protected
    procedure Execute; override;
  public
    constructor Create(strip: integer; const img: Timg_array; fmt: TTiffFormat;
                       flip_H, flip_V: boolean; level: TCompressionLevel;
                       width2, height2, strip_rows: integer; var strips: TStripStreams);
  end;


constructor TStripCompressThread.Create(strip: integer; const img: Timg_array; fmt: TTiffFormat;
                   flip_H, flip_V: boolean; level: TCompressionLevel;
                   width2, height2, strip_rows: integer; var strips: TStripStreams);
begin
  inherited Create(True); {create suspended}
  FreeOnTerminate := False;
  Fstrip := strip;
  Fimg := img; {reference copy, no pixel data is copied}
  Ffmt := fmt;
  FflipH := flip_H;
  FflipV := flip_V;
  Flevel := level;
  Fwidth := width2;
  Fheight := height2;
  FstripRows := strip_rows;
  Fstrips := @strips;
end;


procedure TStripCompressThread.Execute;
var
  r, k, row0, row1: integer;
  cs: TCompressionStream;
  buf: array of byte;
begin
  SetLength(buf, Fwidth * bytes_per_pixel[Ffmt]);
  Fstrips^[Fstrip] := TMemoryStream.Create;
  cs := TCompressionStream.Create(Flevel, Fstrips^[Fstrip]); {each thread has its own zlib stream, zlib is re-entrant}
  try
    row0 := Fstrip * FstripRows;                       {first file row of this strip}
    row1 := min(row0 + FstripRows, Fheight) - 1;       {last file row of this strip}
    for r := row0 to row1 do
    begin
      if FflipV = false then k := Fheight - 1 - r else k := r; {reverse fits down to counting}
      fill_tiff_row(Fimg, Ffmt, k, Fwidth, FflipH, buf);
      cs.WriteBuffer(buf[0], length(buf));
    end;
  finally
    cs.Free; {flushes remaining data to the memory stream}
  end;
end;


function compress_image_strips(const img: Timg_array; fmt: TTiffFormat;
             flip_H, flip_V: boolean; level: TCompressionLevel;
             width2, height2: integer;
             out strip_rows: integer; out strips: TStripStreams): integer;
{Compress the image as N independent Deflate strips, one thread per strip.
 Returns the number of strips. Caller must free strips[] after writing them.}
var
  thread_count, num_strips, i: integer;
  threads: array of TStripCompressThread;
begin
  thread_count := min(get_thread_count, height2);
  strip_rows := (height2 + thread_count - 1) div thread_count; {ceil, all strips except last have strip_rows rows as TIFF requires}
  num_strips := (height2 + strip_rows - 1) div strip_rows;

  SetLength(strips, num_strips);
  SetLength(threads, num_strips);

  for i := 0 to num_strips - 1 do
  begin
    threads[i] := TStripCompressThread.Create(i, img, fmt, flip_H, flip_V, level,
                                              width2, height2, strip_rows, strips);
    threads[i].Start;
  end;
  for i := 0 to num_strips - 1 do
  begin
    threads[i].WaitFor;
    threads[i].Free;
  end;
  result := num_strips;
end;

function save_tiff_new(img: Timg_array; var filen2: string; description: ansistring; bitpix: integer; flip_H, flip_V, overwrite: boolean; compressionlevel: integer): boolean; //save to TIFF file, compressionlevel 0..3 equals clnone,clfastest, cldefault, clmax
var                                                                                                                                                                            //new file name will be returned
  extn: string;
begin
  result:=false;//assume failure
  extn:=lowercase(extractfileExt(filen2));
  if ((extn='.tmp') or (extn='.tif') or (extn='.tiff'))=false then // not a special .tmp file for secure tiff save, not .TIF or .TIFF
    filen2 := ChangeFileExt(filen2, '.tif');

  if ((overwrite=false) and (fileexists(filen2))) = true then
    if MessageDlg('Existing file ' + filen2 + ' Overwrite?', mtConfirmation, [mbYes, mbNo], 0) <> 6 {mbYes} then
      Exit;

  if length(img)=1 then //monochrome
  begin
    if bitpix<=16 then //8 or 16 bit origin
      result:=save_tiff_16(img,filen2, description,flip_H, flip_V,compressionlevel) {save to 16 bit gray scale TIFF file }
    else  //32 bit origin
      result:=save_tiff_32(img,filen2, description,flip_H, flip_V,compressionlevel) {save to 32 bit gray scale TIFF file }
  end
  else
  begin  //colour
    if bitpix<=16 then //8 or 16 bit origin
      result:=save_tiff_48(img,filen2, description,flip_H, flip_V,compressionlevel) {save to 48=3x16 color TIFF file }
    else //32 bit origin
      result:=save_tiff_96(img,filen2, description,flip_H, flip_V,compressionlevel) {save to 96=3x32 color TIFF file }
  end;
end;


function save_tiff_16(img: Timg_array; filen2, description: ansistring; flip_H, flip_V: boolean; compressionlevel: integer): boolean; {save to 16 bit grascale TIFF file }
var
  OffsetXRes: LongInt;
  OffsetYRes: LongInt;
  OffsetDescrip: Longint;
  OffsetSoftware: LongInt;
  OffsetStrip: LongInt;
  OffsetDir: LongInt;
  OffsetStripArrays: LongInt;
  thefile: tfilestream;
  i, k, width2, height2: integer;
  tiffbuffer: array of byte;
  strips: TStripStreams;      {compressed strips, one per thread}
  strip_rows, num_strips: integer;
  strip_offs, strip_sizes: array of LongInt;
begin
  result := false;
  num_strips := 1;

  width2 := length(img[0, 0]); {width}
  height2 := length(img[0]);  {height}
  description := description + #0; {GIMP is complaining about this #0}

  try
    thefile := tfilestream.Create(filen2, fmcreate);
  except
    thefile.free;
    exit;
  end;

  Directorybw16[1]._Value := LongInt(width2);          { Image Width }
  Directorybw16[2]._Value := LongInt(height2);         { Image Height }

  Directorybw16[9]._Value := LongInt(height2);         { RowsPerStrip, single strip default }

  Directorybw16[06]._count := LongInt(length(description));   { Length Description}
  Directorybw16[14]._count := LongInt(length(softwarename));   { Length software}

  {restore possibly modified counts from a previous multi-strip save (typed constants persist!)}
  Directorybw16[7]._Count  := 1;
  Directorybw16[10]._Count := 1;

  { Handle Compression Tag and Data Preparation }
  if TCompressionLevel(compressionlevel)<>clnone then
  begin
    Directorybw16[4]._Value := 8; {Tag 0x0103: Compression = 8 (Deflate)}

    {Compress in parallel to memory, one Deflate strip per CPU core}
    num_strips := compress_image_strips(img, tfGray16, flip_H, flip_V,
                    TCompressionLevel(compressionlevel), width2, height2, strip_rows, strips);

    Directorybw16[9]._Value := LongInt(strip_rows);    { RowsPerStrip }
    if num_strips > 1 then
    begin
      Directorybw16[7]._Count  := num_strips;  { StripOffsets becomes an array, _Value set later to its file offset }
      Directorybw16[10]._Count := num_strips;  { StripByteCounts idem }
    end
    else
      Directorybw16[10]._Value := LongInt(strips[0].Size); { Strip Byte Counts = Compressed Size }
  end
  else
  begin
    Directorybw16[4]._Value := 1; {Tag 0x0103: Compression = 1 (None)}
    Directorybw16[10]._Value := LongInt(2 * width2 * height2); { Strip Byte Counts }
  end;


  { Write TIFF - File }
  { ------------------------------------------- }
  { Write Header }
  OffsetDir := sizeof(TifHeader) + sizeof(X_Res_Value) + sizeof(Y_Res_Value) + length(description) + length(SoftwareName); {where is the IFD directory}
  if num_strips > 1 then
    OffsetDir := OffsetDir + 8 * num_strips; {StripOffsets + StripByteCounts arrays stored before the IFD}
  move(offsetdir, tifheader[4], 4); { Pointer to the first directory.}

  thefile.writebuffer(TifHeader, sizeof(TifHeader));

  OffsetXRes := thefile.Position;
  thefile.writebuffer(X_Res_Value, sizeof(X_Res_Value));

  OffsetYRes := thefile.Position;
  thefile.writebuffer(Y_Res_Value, sizeof(Y_Res_Value));

  OffsetDescrip := thefile.Position;
  thefile.writebuffer(description[1], length(description));

  OffsetSoftware := thefile.Position;
  thefile.writebuffer(SoftwareName[1], length(SoftwareName));

  OffsetStrip := OffsetDir + sizeof(NoOfDirsBW16) + sizeof(Directorybw16) + sizeof(NullString); {location of first strip data}

  if num_strips > 1 then
  begin {write StripOffsets and StripByteCounts arrays just before the IFD}
    SetLength(strip_offs, num_strips);
    SetLength(strip_sizes, num_strips);
    strip_offs[0] := OffsetStrip;
    strip_sizes[0] := LongInt(strips[0].Size);
    for i := 1 to num_strips - 1 do
    begin
      strip_sizes[i] := LongInt(strips[i].Size);
      strip_offs[i] := strip_offs[i-1] + strip_sizes[i-1]; {strips are written back to back}
    end;
    OffsetStripArrays := thefile.Position;
    thefile.writebuffer(strip_offs[0], 4 * num_strips);
    thefile.writebuffer(strip_sizes[0], 4 * num_strips);

    Directorybw16[7]._Value := OffsetStripArrays;                   { StripOffsets array location }
    Directorybw16[10]._Value := OffsetStripArrays + 4 * num_strips; { StripByteCounts array location }
  end
  else
    Directorybw16[7]._Value := OffsetStrip;          { StripOffset, location of start image data}

  { Set Offset - Adresses into Directory }
  Directorybw16[11]._Value := OffsetXRes;          { X-Resolution  }
  Directorybw16[12]._Value := OffsetYRes;          { Y-Resolution  }

  Directorybw16[14]._Value := OffsetSoftware;      { Software      }
  Directorybw16[06]._Value := OffsetDescrip;       { Description   }


  { Write IFD Directory }
  thefile.writebuffer(NoOfDirsBW16, sizeof(NoOfDirsBW16)); {number of directory entries}
  thefile.writebuffer(Directorybw16, sizeof(Directorybw16));
  thefile.writebuffer(NullString, sizeof(NullString));


  { Write Image Data }
  if TCompressionLevel(compressionlevel)<>clnone then
  begin
    {Write the compressed strips from memory, back to back}
    for i := 0 to num_strips - 1 do
    begin
      strips[i].Position := 0;
      thefile.CopyFrom(strips[i], strips[i].Size);
      strips[i].Free;
    end;
  end
  else
  begin
    {Standard uncompressed write loop}
    setlength(tiffbuffer, width2 * 2);
    for i := 0 to height2 - 1 do
    begin
      if flip_V = false then k := height2 - 1 - i else k := i; {reverse fits down to counting}
      fill_tiff_row(img, tfGray16, k, width2, flip_H, tiffbuffer);
      thefile.writebuffer(tiffbuffer[0], width2 * 2); {works only for byte arrays}
    end;
  end;

  thefile.free;
  result := true;
end;

function save_tiff_32(img: Timg_array; filen2, description: ansistring; flip_H, flip_V: boolean; compressionlevel: integer): boolean; {save to 32 bit float gray scale TIFF file }
var
  OffsetXRes: LongInt;
  OffsetYRes: LongInt;
  OffsetSoftware: LongInt;
  OffsetDescrip: LongInt;
  OffsetStrip: LongInt;
  OffsetDir: LongInt;
  OffsetStripArrays: LongInt;
  thefile: tfilestream;
  i, k, width2, height2: integer;
  tiffbuffer: array of byte;
  strips: TStripStreams;
  strip_rows, num_strips: integer;
  strip_offs, strip_sizes: array of LongInt;
begin
  result := false;
  num_strips := 1;

  width2 := length(img[0, 0]); {width}
  height2 := length(img[0]);  {height}
  description := description + #0; {GIMP is complaining about this #0}

  try
    thefile := tfilestream.Create(filen2, fmcreate);
  except
    thefile.free;
    exit;
  end;

  Directorybw32[1]._Value := LongInt(width2);       { Image Width }
  Directorybw32[2]._Value := LongInt(Height2);      { Image Height }

  Directorybw32[9]._Value := LongInt(Height2);      { RowsPerStrip, single strip default }

  Directorybw32[06]._count := LongInt(length(description));   { Length Description}
  Directorybw32[14]._count := LongInt(length(softwarename));   { Length software}

  Directorybw32[7]._Count  := 1; {restore possibly modified counts, typed constants persist}
  Directorybw32[10]._Count := 1;

  if TCompressionLevel(compressionlevel)<>clnone then
  begin
    Directorybw32[4]._Value := 8; {Deflate Compression}

    num_strips := compress_image_strips(img, tfGray32, flip_H, flip_V,
                    TCompressionLevel(compressionlevel), width2, height2, strip_rows, strips);

    Directorybw32[9]._Value := LongInt(strip_rows);    { RowsPerStrip }
    if num_strips > 1 then
    begin
      Directorybw32[7]._Count  := num_strips;
      Directorybw32[10]._Count := num_strips;
    end
    else
      Directorybw32[10]._Value := LongInt(strips[0].Size);
  end
  else
  begin
    Directorybw32[4]._Value := 1; {No Compression}
    Directorybw32[10]._Value := LongInt(4 * width2 * Height2); { Strip Byte Counts }
  end;

  { Write TIFF -  }
  { ------------------------------------------- }
  { Write Header }
  OffsetDir := sizeof(TifHeader) + sizeof(X_Res_Value) + sizeof(Y_Res_Value) + length(description) + length(SoftwareName); {where is the IFD directory}
  if num_strips > 1 then
    OffsetDir := OffsetDir + 8 * num_strips;
  move(offsetdir, tifheader[4], 4); { Pointer to the first directory.}
  thefile.writebuffer(TifHeader, sizeof(TifHeader));

  OffsetXRes := thefile.Position;
  thefile.writebuffer(X_Res_Value, sizeof(X_Res_Value));

  OffsetYRes := thefile.Position;
  thefile.writebuffer(Y_Res_Value, sizeof(Y_Res_Value));

  OffsetDescrip := thefile.Position;
  thefile.writebuffer(description[1], length(description));

  OffsetSoftware := thefile.Position;
  thefile.writebuffer(SoftwareName[1], length(SoftwareName));

  OffsetStrip := OffsetDir + sizeof(NoOfDirsBW32) + sizeof(Directorybw32) + sizeof(NullString);

  if num_strips > 1 then
  begin
    SetLength(strip_offs, num_strips);
    SetLength(strip_sizes, num_strips);
    strip_offs[0] := OffsetStrip;
    strip_sizes[0] := LongInt(strips[0].Size);
    for i := 1 to num_strips - 1 do
    begin
      strip_sizes[i] := LongInt(strips[i].Size);
      strip_offs[i] := strip_offs[i-1] + strip_sizes[i-1];
    end;
    OffsetStripArrays := thefile.Position;
    thefile.writebuffer(strip_offs[0], 4 * num_strips);
    thefile.writebuffer(strip_sizes[0], 4 * num_strips);

    Directorybw32[7]._Value := OffsetStripArrays;
    Directorybw32[10]._Value := OffsetStripArrays + 4 * num_strips;
  end
  else
    Directorybw32[7]._Value := OffsetStrip;        { StripOffset, location of start image data}

  { Set Offset - Adresses into Directory }
  Directorybw32[11]._Value := OffsetXRes;         { X-Resolution  }
  Directorybw32[12]._Value := OffsetYRes;         { Y-Resolution  }
  Directorybw32[14]._Value := OffsetSoftware;     { Software      }
  Directorybw32[06]._Value := OffsetDescrip;      { Description   }

  { Write IFD Directory }
  thefile.writebuffer(NoOfDirsBW32, sizeof(NoOfDirsBW32)); {number of directory entries}
  thefile.writebuffer(Directorybw32, sizeof(Directorybw32));
  thefile.writebuffer(NullString, sizeof(NullString));

  { Write Image Data }
  if TCompressionLevel(compressionlevel)<>clnone then
  begin
    for i := 0 to num_strips - 1 do
    begin
      strips[i].Position := 0;
      thefile.CopyFrom(strips[i], strips[i].Size);
      strips[i].Free;
    end;
  end
  else
  begin
    setlength(tiffbuffer, width2 * 4);
    for i := 0 to Height2 - 1 do
    begin
      if flip_V = false then k := height2 - 1 - i else k := i; {reverse fits down to counting}
      fill_tiff_row(img, tfGray32, k, width2, flip_H, tiffbuffer);
      thefile.writebuffer(tiffbuffer[0], width2 * 4); {works only for byte arrays}
    end;
  end;

  thefile.free;
  result := true;
end;


function save_tiff_48(img: Timg_array; filen2, description: ansistring; flip_H, flip_V : boolean; compressionlevel: integer): boolean; {save to 48=3x16 color TIFF file}
var
  OffsetXRes: LongInt;
  OffsetYRes: LongInt;
  OffsetDescrip: Longint;
  OffsetSoftware: LongInt;
  OffsetStrip: LongInt;
  OffsetDir: LongInt;
  OffsetBitsPerSample: LongInt;
  OffsetStripArrays: LongInt;
  thefile: tfilestream;
  i, k, width2, height2: integer;
  tiffbuffer: array of byte;
  strips: TStripStreams;
  strip_rows, num_strips: integer;
  strip_offs, strip_sizes: array of LongInt;
begin
  result := false;
  num_strips := 1;
  try
    thefile := tfilestream.Create(filen2, fmcreate);
  except
    thefile.free;
    exit;
  end;

  width2 := length(img[0, 0]); {width}
  height2 := length(img[0]); {height}
  description := description + #0; {GIMP is complaining about this}

  Directoryrgb48[1]._Value := LongInt(width2);       { Image Width }
  Directoryrgb48[2]._Value := LongInt(height2);      { Image Height }
  Directoryrgb48[9]._Value := LongInt(height2);      { RowsPerStrip, single strip default }

  Directoryrgb48[06]._count := LongInt(length(description));   { Length Description}
  Directoryrgb48[15]._count := LongInt(length(softwarename));  { Length software}

  Directoryrgb48[7]._Count  := 1; {restore possibly modified counts, typed constants persist}
  Directoryrgb48[10]._Count := 1;

  { Handle Compression Tag and Data Preparation }
  if TCompressionLevel(compressionlevel)<>clnone then
  begin
    Directoryrgb48[4]._Value := 8; {Tag 0x0103: Compression = 8 (Deflate)}

    num_strips := compress_image_strips(img, tfRGB48, flip_H, flip_V,
                    TCompressionLevel(compressionlevel), width2, height2, strip_rows, strips);

    Directoryrgb48[9]._Value := LongInt(strip_rows);    { RowsPerStrip }
    if num_strips > 1 then
    begin
      Directoryrgb48[7]._Count  := num_strips;
      Directoryrgb48[10]._Count := num_strips;
    end
    else
      Directoryrgb48[10]._Value := LongInt(strips[0].Size); { Strip Byte Counts = Compressed Size }
  end
  else
  begin
    Directoryrgb48[4]._Value := 1; {Tag 0x0103: Compression = 1 (None)}
    Directoryrgb48[10]._Value := LongInt(2 * 3 * width2 * height2); { Strip Byte Counts }
  end;

  { Write TIFF - File for Image with RGB-Values }
  { ------------------------------------------- }
  { Write Header }
  OffsetDir := sizeof(TifHeader) + sizeof(X_Res_Value) + sizeof(Y_Res_Value) + sizeof(BitsPerSample48) + length(description) + length(SoftwareName); {where is the IFD directory}
  if num_strips > 1 then
    OffsetDir := OffsetDir + 8 * num_strips;
  move(offsetdir, tifheader[4], 4); { Pointer to the first directory.}
  thefile.writebuffer(TifHeader, sizeof(TifHeader));

  OffsetXRes := thefile.Position;
  thefile.writebuffer(X_Res_Value, sizeof(X_Res_Value));

  OffsetYRes := thefile.Position;
  thefile.writebuffer(Y_Res_Value, sizeof(Y_Res_Value));

  OffsetBitsPerSample := Thefile.Position; {where is sample located}
  Thefile.writebuffer(BitsPerSample48, sizeof(BitsPerSample48));

  OffsetDescrip := thefile.Position;
  thefile.writebuffer(description[1], length(description));

  OffsetSoftware := thefile.Position;
  thefile.writebuffer(SoftwareName[1], length(SoftwareName));

  OffsetStrip := OffsetDir + sizeof(NoOfDirsRGB48) + sizeof(DirectoryRGB48) + sizeof(NullString);

  if num_strips > 1 then
  begin
    SetLength(strip_offs, num_strips);
    SetLength(strip_sizes, num_strips);
    strip_offs[0] := OffsetStrip;
    strip_sizes[0] := LongInt(strips[0].Size);
    for i := 1 to num_strips - 1 do
    begin
      strip_sizes[i] := LongInt(strips[i].Size);
      strip_offs[i] := strip_offs[i-1] + strip_sizes[i-1];
    end;
    OffsetStripArrays := thefile.Position;
    thefile.writebuffer(strip_offs[0], 4 * num_strips);
    thefile.writebuffer(strip_sizes[0], 4 * num_strips);

    Directoryrgb48[7]._Value := OffsetStripArrays;
    Directoryrgb48[10]._Value := OffsetStripArrays + 4 * num_strips;
  end
  else
    Directoryrgb48[7]._Value := OffsetStrip;          { StripOffset, location of start image data}

  { Set Offset - Adresses into Directory }
  DirectoryRGB48[3]._Value := OffsetBitsPerSample;   { BitsPerSample location containing 1000 1000 1000  (16,16,16)}
  Directoryrgb48[11]._Value := OffsetXRes;          { X-Resolution  }
  Directoryrgb48[12]._Value := OffsetYRes;          { Y-Resolution  }
  Directoryrgb48[15]._Value := OffsetSoftware;      { Software      }
  Directoryrgb48[06]._Value := OffsetDescrip;       { Description   }

  { Write Directory }
  thefile.writebuffer(NoOfDirsRGB48, sizeof(NoOfDirsRGB48)); {number of directory entries}
  thefile.writebuffer(Directoryrgb48, sizeof(Directoryrgb48));
  thefile.writebuffer(NullString, sizeof(NullString));

  { Write Image Data }
  if TCompressionLevel(compressionlevel)<>clnone then
  begin
    for i := 0 to num_strips - 1 do
    begin
      strips[i].Position := 0;
      thefile.CopyFrom(strips[i], strips[i].Size);
      strips[i].Free;
    end;
  end
  else
  begin
    setlength(tiffbuffer, width2 * 6);
    for i := 0 to height2 - 1 do
    begin
      if flip_V = false then k := height2 - 1 - i else k := i; {reverse fits down to counting}
      fill_tiff_row(img, tfRGB48, k, width2, flip_H, tiffbuffer);
      thefile.writebuffer(tiffbuffer[0], width2 * 6); {works only for byte arrays}
    end;
  end;

  thefile.free;
  result := true;
end;


function save_tiff_96(img: Timg_array; filen2, description: ansistring; flip_H, flip_V: boolean; compressionlevel: integer): boolean; {save to 96=3x32 color TIFF file }
var
  OffsetXRes: LongInt;
  OffsetYRes: LongInt;
  OffsetDescrip: Longint;
  OffsetSoftware: LongInt;
  OffsetStrip: LongInt;
  OffsetDir: LongInt;
  OffsetBitsPerSample: LongInt;
  OffsetStripArrays: LongInt;
  thefile: tfilestream;
  i, k, width2, height2: integer;
  tiffbuffer: array of byte;
  strips: TStripStreams;
  strip_rows, num_strips: integer;
  strip_offs, strip_sizes: array of LongInt;
begin
  result := false;
  num_strips := 1;
  try
    thefile := tfilestream.Create(filen2, fmcreate);
  except
    thefile.free;
    exit;
  end;

  width2 := length(img[0, 0]); {width}
  height2 := length(img[0]); {height}

  description := description + #0; {GIMP is complaining about this}

  Directoryrgb96[1]._Value := LongInt(width2);       { Image Width }
  Directoryrgb96[2]._Value := LongInt(Height2);      { Image Height }
  Directoryrgb96[9]._Value := LongInt(Height2);      { RowsPerStrip, single strip default }

  Directoryrgb96[06]._count := LongInt(length(description));   { Length Description}
  Directoryrgb96[15]._count := LongInt(length(softwarename));  { Length software}

  Directoryrgb96[7]._Count  := 1; {restore possibly modified counts, typed constants persist}
  Directoryrgb96[10]._Count := 1;

  if TCompressionLevel(compressionlevel)<>clnone then
  begin
    Directoryrgb96[4]._Value := 8; {Deflate Compression}

    num_strips := compress_image_strips(img, tfRGB96, flip_H, flip_V,
                    TCompressionLevel(compressionlevel), width2, height2, strip_rows, strips);

    Directoryrgb96[9]._Value := LongInt(strip_rows);    { RowsPerStrip }
    if num_strips > 1 then
    begin
      Directoryrgb96[7]._Count  := num_strips;
      Directoryrgb96[10]._Count := num_strips;
    end
    else
      Directoryrgb96[10]._Value := LongInt(strips[0].Size);
  end
  else
  begin
    Directoryrgb96[4]._Value := 1; {No Compression}
    Directoryrgb96[10]._Value := LongInt(4 * 3 * width2 * Height2); { Strip Byte Counts }
  end;

  { Write TIFF - File for Image with RGB-Values }
  { ------------------------------------------- }
  { Write Header }
  OffsetDir := sizeof(TifHeader) + sizeof(X_Res_Value) + sizeof(Y_Res_Value) + sizeof(BitsPerSample96) + length(description) + length(SoftwareName); {where is the IFD directory}
  if num_strips > 1 then
    OffsetDir := OffsetDir + 8 * num_strips;
  move(offsetdir, tifheader[4], 4); { Pointer to the first directory.}

  thefile.writebuffer(TifHeader, sizeof(TifHeader));

  OffsetXRes := thefile.Position;
  thefile.writebuffer(X_Res_Value, sizeof(X_Res_Value));

  OffsetYRes := thefile.Position;
  thefile.writebuffer(Y_Res_Value, sizeof(Y_Res_Value));

  OffsetBitsPerSample := Thefile.Position; {where is sample located}
  Thefile.writebuffer(BitsPerSample96, sizeof(BitsPerSample96));

  OffsetDescrip := thefile.Position;
  thefile.writebuffer(description[1], length(description));

  OffsetSoftware := thefile.Position;
  thefile.writebuffer(SoftwareName[1], length(SoftwareName));

  OffsetStrip := OffsetDir + sizeof(NoOfDirsRGB96) + sizeof(DirectoryRGB96) + sizeof(NullString);

  if num_strips > 1 then
  begin
    SetLength(strip_offs, num_strips);
    SetLength(strip_sizes, num_strips);
    strip_offs[0] := OffsetStrip;
    strip_sizes[0] := LongInt(strips[0].Size);
    for i := 1 to num_strips - 1 do
    begin
      strip_sizes[i] := LongInt(strips[i].Size);
      strip_offs[i] := strip_offs[i-1] + strip_sizes[i-1];
    end;
    OffsetStripArrays := thefile.Position;
    thefile.writebuffer(strip_offs[0], 4 * num_strips);
    thefile.writebuffer(strip_sizes[0], 4 * num_strips);

    Directoryrgb96[7]._Value := OffsetStripArrays;
    Directoryrgb96[10]._Value := OffsetStripArrays + 4 * num_strips;
  end
  else
    Directoryrgb96[7]._Value := OffsetStrip;          { StripOffset, location of start image data}

  { Set Offset - Adresses into Directory }
  DirectoryRGB96[3]._Value := OffsetBitsPerSample;   { BitsPerSample location containing 2000 2000 2000  (32,32,32)}
  Directoryrgb96[11]._Value := OffsetXRes;          { X-Resolution  }
  Directoryrgb96[12]._Value := OffsetYRes;          { Y-Resolution  }
  Directoryrgb96[15]._Value := OffsetSoftware;      { Software      }
  Directoryrgb96[06]._Value := OffsetDescrip;       { Description   }

  { Write Directory }
  thefile.writebuffer(NoOfDirsRGB96, sizeof(NoOfDirsRGB96)); {number of directory entries}
  thefile.writebuffer(Directoryrgb96, sizeof(Directoryrgb96));
  thefile.writebuffer(NullString, sizeof(NullString));

  { Write Image Data }
  if TCompressionLevel(compressionlevel)<>clnone then
  begin
    for i := 0 to num_strips - 1 do
    begin
      strips[i].Position := 0;
      thefile.CopyFrom(strips[i], strips[i].Size);
      strips[i].Free;
    end;
  end
  else
  begin
    setlength(tiffbuffer, width2 * 12); //4*3 bytes for one pixel
    for i := 0 to Height2 - 1 do
    begin
      if flip_V = false then k := height2 - 1 - i else k := i; {reverse fits down to counting}
      fill_tiff_row(img, tfRGB96, k, width2, flip_H, tiffbuffer);
      thefile.writebuffer(tiffbuffer[0], width2 * 12); {works only for byte arrays}
    end;
  end;
  thefile.free;
  result := true;
end;

{ -------------------------------------------------------------------------
  Generic TIFF Read Function (Big Endian support, threaded Deflate decompression)
  ------------------------------------------------------------------------- }

{ ── byte-swap helpers ─────────────────────────────────────────────────── }
function SwapWord(v: Word): Word; inline;
begin
  Result := ((v and $00FF) shl 8) or ((v and $FF00) shr 8);
end;

function SwapLong(v: LongWord): LongWord; inline;
begin
  Result := ((v and $000000FF) shl 24) or
            ((v and $0000FF00) shl  8) or
            ((v and $00FF0000) shr  8) or
            ((v and $FF000000) shr 24);
end;


{ ── predictor, standalone so both the sequential path and the worker threads can use it ── }
procedure apply_predictor(var tiffbuffer: array of byte; row_size: integer;
            samplesperpixel: integer; bitspersample, predictor: word; big_endian: boolean);
var
  p16     : PWord;
  jj, b   : Integer;
  rowBytes: Integer;
  temp    : array of Byte;
  srcPtr  : PByte;
begin
  if (predictor <> 2) and (predictor <> 3) then Exit;
  rowBytes := row_size;

  if predictor = 2 then
  begin
    if bitspersample = 8 then
    begin
      for jj := samplesperpixel to rowBytes - 1 do
        tiffbuffer[jj] := Byte(tiffbuffer[jj] + tiffbuffer[jj - samplesperpixel]);
    end
    else if bitspersample = 16 then
    begin
      if not big_endian then
      begin
        // Little-endian: words are already in host order, add in place
        for jj := samplesperpixel to (rowBytes div 2) - 1 do
        begin
          p16  := PWord(@tiffbuffer[jj * 2]);
          p16^ := Word(p16^ + PWord(@tiffbuffer[(jj - samplesperpixel) * 2])^);
        end;
      end
      else
      begin
        // Big-endian: swap to host order, add, swap back
        for jj := samplesperpixel to (rowBytes div 2) - 1 do
        begin
          p16  := PWord(@tiffbuffer[jj * 2]);
          p16^ := SwapWord(
                    Word(SwapWord(p16^) +
                         SwapWord(PWord(@tiffbuffer[(jj - samplesperpixel) * 2])^)));
        end;
      end;
    end;
  end
  else if predictor = 3 then  { floating-point predictor }
  begin
    { (a) undo per-plane byte delta }
    for jj := samplesperpixel to rowBytes - 1 do
      tiffbuffer[jj] := Byte(tiffbuffer[jj] + tiffbuffer[jj - samplesperpixel]);

    { (b) undo byte-plane reordering. See original comments: we always
      reconstruct in little-endian host order (byte[0] = LSB). }
    SetLength(temp, rowBytes);
    Move(tiffbuffer[0], temp[0], rowBytes);
    srcPtr := @temp[0];
    for jj := 0 to (rowBytes div 4) - 1 do
    begin
      tiffbuffer[jj * 4 + 3] := srcPtr[jj];
      tiffbuffer[jj * 4 + 2] := srcPtr[jj +     rowBytes div 4];
      tiffbuffer[jj * 4 + 1] := srcPtr[jj + 2 * (rowBytes div 4)];
      tiffbuffer[jj * 4 + 0] := srcPtr[jj + 3 * (rowBytes div 4)];
    end;
    if big_endian then
    begin
      jj := 0;
      while jj < rowBytes do
      begin
        { swap bytes 0↔3 and 1↔2 }
        b := tiffbuffer[jj];   tiffbuffer[jj]   := tiffbuffer[jj+3]; tiffbuffer[jj+3] := b;
        b := tiffbuffer[jj+1]; tiffbuffer[jj+1] := tiffbuffer[jj+2]; tiffbuffer[jj+2] := b;
        Inc(jj, 4);
      end;
    end;
  end;
end;


{ ── copy one decompressed/predictor-corrected row into img, standalone ── }
procedure copy_row_to_img(var img: Timg_array; const tiffbuffer: array of byte;
            row_inv, height, width: integer; samplesperpixel: integer;
            bitspersample: word; is_float, big_endian: boolean; var measured_max: double);
var
  mm, row : integer;
  raw16   : Word;
  rawf    : LongWord;
  fval    : Single;
begin
  row := height - row_inv - 1;   { flip vertical (FITS convention) }

  if samplesperpixel = 1 then
  begin
    if bitspersample = 8 then
      for mm := 0 to width - 1 do
        img[0, row, mm] := tiffbuffer[mm] * 257
    else if bitspersample = 16 then
    begin
      for mm := 0 to width - 1 do
      begin
        raw16 := PWord(@tiffbuffer[mm * 2])^;
        if big_endian then raw16 := SwapWord(raw16);
        img[0, row, mm] := raw16;
      end;
    end
    else if is_float then
    begin
      for mm := 0 to width - 1 do
      begin
        rawf := PLongWord(@tiffbuffer[mm * 4])^;
        if big_endian then rawf := SwapLong(rawf);
        fval := PSingle(@rawf)^ * 65535;
        measured_max := max(fval, measured_max);
        img[0, row, mm] := fval;
      end;
    end;
  end
  else  { samplesperpixel = 3 }
  begin
    if bitspersample = 8 then
    begin
      for mm := 0 to width - 1 do
      begin
        img[0, row, mm] := tiffbuffer[mm*3]   * 257;
        img[1, row, mm] := tiffbuffer[mm*3+1] * 257;
        img[2, row, mm] := tiffbuffer[mm*3+2] * 257;
      end;
    end
    else if bitspersample = 16 then
    begin
      for mm := 0 to width - 1 do
      begin
        raw16 := PWord(@tiffbuffer[mm*6])^;
        if big_endian then raw16 := SwapWord(raw16);
        img[0, row, mm] := raw16;

        raw16 := PWord(@tiffbuffer[mm*6+2])^;
        if big_endian then raw16 := SwapWord(raw16);
        img[1, row, mm] := raw16;

        raw16 := PWord(@tiffbuffer[mm*6+4])^;
        if big_endian then raw16 := SwapWord(raw16);
        img[2, row, mm] := raw16;
      end;
    end
    else if is_float then
    begin
      for mm := 0 to width - 1 do
      begin
        rawf := PLongWord(@tiffbuffer[mm*12])^;
        if big_endian then rawf := SwapLong(rawf);
        fval := PSingle(@rawf)^ * 65535;
        measured_max := max(fval, measured_max);
        img[0, row, mm] := fval;

        rawf := PLongWord(@tiffbuffer[mm*12+4])^;
        if big_endian then rawf := SwapLong(rawf);
        img[1, row, mm] := PSingle(@rawf)^ * 65535;

        rawf := PLongWord(@tiffbuffer[mm*12+8])^;
        if big_endian then rawf := SwapLong(rawf);
        img[2, row, mm] := PSingle(@rawf)^ * 65535;
      end;
    end;
  end;
end;


{ ── worker thread decompressing a range of Deflate strips ─────────────── }
type
  TCompressedStrips = array of TBytes;

  TStripDecompressThread = class(TThread)
  private
    Fimg            : Timg_array;       {reference copy, shares underlying data with caller}
    Fdata           : TCompressedStrips; {reference copy}
    FfirstStrip, FlastStrip : integer;
    FrowsPerStrip, Fheight, Fwidth, FrowSize : integer;
    FsamplesPP      : integer;
    Fbits, Fpredictor : word;
    FisFloat, FbigEndian : boolean;
  public
    max_found : double;  {per-thread measured_max, combined by the caller}
    error_msg : string;  {threads must never call MessageDlg, report instead}
    constructor Create(firstStrip, lastStrip: integer; var img: Timg_array;
                       var data: TCompressedStrips;
                       rowsPerStrip, height, width, rowSize, samplesPP: integer;
                       bits, predictor: word; isFloat, bigEndian: boolean);
  protected
    procedure Execute; override;
  end;


constructor TStripDecompressThread.Create(firstStrip, lastStrip: integer; var img: Timg_array;
                   var data: TCompressedStrips;
                   rowsPerStrip, height, width, rowSize, samplesPP: integer;
                   bits, predictor: word; isFloat, bigEndian: boolean);
begin
  inherited Create(True); {create suspended}
  FreeOnTerminate := False;
  FfirstStrip := firstStrip;
  FlastStrip := lastStrip;
  Fimg := img;   {reference copy, no pixel data is copied}
  Fdata := data;
  FrowsPerStrip := rowsPerStrip;
  Fheight := height;
  Fwidth := width;
  FrowSize := rowSize;
  FsamplesPP := samplesPP;
  Fbits := bits;
  Fpredictor := predictor;
  FisFloat := isFloat;
  FbigEndian := bigEndian;
  max_found := 0;
  error_msg := '';
end;


procedure TStripDecompressThread.Execute;
var
  s, r, row0, rows_in_strip: integer;
  mem_str: TMemoryStream;
  decomp_str: TDecompressionStream;
  buf: array of byte;
begin
  SetLength(buf, FrowSize);
  try
    for s := FfirstStrip to FlastStrip do
    begin
      row0 := s * FrowsPerStrip;
      if row0 >= Fheight then Break;
      rows_in_strip := min(FrowsPerStrip, Fheight - row0);

      mem_str := TMemoryStream.Create;
      try
        mem_str.WriteBuffer(Fdata[s][0], length(Fdata[s]));
        mem_str.Position := 0;
        decomp_str := TDecompressionStream.Create(mem_str);
        try
          for r := 0 to rows_in_strip - 1 do
          begin
            decomp_str.ReadBuffer(buf[0], FrowSize);
            apply_predictor(buf, FrowSize, FsamplesPP, Fbits, Fpredictor, FbigEndian);
            copy_row_to_img(Fimg, buf, row0 + r, Fheight, Fwidth, FsamplesPP,
                            Fbits, FisFloat, FbigEndian, max_found);
          end;
        finally
          decomp_str.Free;
        end;
      finally
        mem_str.Free;
      end;
    end;
  except
    on E: Exception do
      error_msg := 'Deflate strip decompression failed: ' + E.Message;
  end;
end;

procedure read_tiff(filen: string; var img: Timg_array; out description: string;
  out bitspersample: word; out measured_max: double; out theresult: boolean);
var
  fs              : TFileStream;
  tiffbuffer      : array of Byte;
  header          : array[0..7] of byte;
  ifd_offset      : LongInt;
  num_entries     : Word;
  i, k            : integer;
  entry           : TDirEntry;
  width, height   : LongInt;
  compression     : word;
  samplesperpixel : word;
  rowsperstrip    : LongInt;
  strip_offsets   : array of LongInt;
  strip_bytecounts: array of LongInt;
  num_strips      : integer;
  current_row     : integer;
  rows_in_strip   : integer;
  mem_str         : TMemoryStream;
  decomp_str      : TDecompressionStream;
  saved_pos       : Int64;
  row_size        : integer;
  w               : word;
  bits_array      : array[0..2] of word;
  is_float        : boolean;
  sample_format   : word;
  planar_config   : word;
  desc_len        : integer;
  desc_offset     : LongInt;
  predictor       : Word;
  lzwOut          : PByte;
  lzwLen          : PtrInt;
  lzwPos          : PtrInt;
  scalefactor     : single;
  big_endian      : boolean;   { TRUE when file uses MM / big-endian byte order }
  j, m            : integer;
  comp_strips     : TCompressedStrips;               {for threaded decompression}
  dthreads        : array of TStripDecompressThread;
  thread_count, strips_per_thread, s0, s1 : integer;
  thread_error    : string;

  { Conditionally swap a Word / LongWord depending on big_endian flag }
  function FixW(v: Word): Word; inline;
  begin
    if big_endian then Result := SwapWord(v) else Result := v;
  end;

  function FixL(v: LongWord): LongWord; inline;
  begin
    if big_endian then Result := SwapLong(v) else Result:=v;
  end;

  procedure ApplyPredictor; inline; {thin wrappers keeping the original sequential code readable}
  begin
    apply_predictor(tiffbuffer, row_size, samplesperpixel, bitspersample, predictor, big_endian);
  end;

  procedure CopyRowToImg(row_inv: integer); inline;
  begin
    copy_row_to_img(img, tiffbuffer, row_inv, height, width, samplesperpixel,
                    bitspersample, is_float, big_endian, measured_max);
  end;

{ ── main body ─────────────────────────────────────────────────────────────── }
begin
  theResult       := False;
  description     := '';
  bitspersample   := 0;
  samplesperpixel := 1;
  is_float        := False;
  sample_format   := 1;
  planar_config   := 1;
  rowsperstrip    := MaxLongInt;
  desc_len        := 0;
  desc_offset     := 0;
  predictor       := 1;
  compression     := 1;
  width           := 0;
  height          := 0;
  measured_max    := 0;
  big_endian      := False;

  if not FileExists(filen) then
  begin
    MessageDlg('File not found: ' + filen, mtError, [mbOK], 0);
    Exit;
  end;

  try
    fs := TFileStream.Create(filen, fmOpenRead or fmShareDenyWrite);
  except
    description := 'Cannot open file: ' + filen;
    Exit;
  end;

  try
    fs.ReadBuffer(header, 8);

    { ── byte-order detection ── }
    if (header[0] = $49) and (header[1] = $49) then   //73, 73
      big_endian := False                        { II = little-endian }
    else if (header[0] = $4D) and (header[1] = $4D) then  //77
      big_endian := True                         { MM = big-endian    }
    else
    begin
      description := 'Unknown TIFF byte-order marker.';
      Exit;
    end;

    { ── magic number ── }
    if big_endian then
    begin
      if (header[2] <> $00) or (header[3] <> $2A) then
      begin
        description := 'Invalid TIFF identifier.';
        Exit;
      end;
    end
    else
    begin
      if (header[2] <> $2A) or (header[3] <> $00) then
      begin
        description := 'Invalid TIFF identifier.';
        Exit;
      end;
    end;

    { ── IFD offset ── }
    Move(header[4], ifd_offset, 4);
    if big_endian then ifd_offset := LongInt(SwapLong(LongWord(ifd_offset)));

    fs.Seek(ifd_offset, soBeginning);
    fs.ReadBuffer(num_entries, 2);
    if big_endian then num_entries := SwapWord(num_entries);

    for i := 0 to num_entries - 1 do
    begin
      fs.ReadBuffer(entry, SizeOf(TDirEntry));

      if big_endian then
      begin
        entry._Tag   := SwapWord  (entry._Tag);
        entry._Type  := SwapWord  (entry._Type);
        entry._Count := SwapLong  (entry._Count);
        entry._Value := SwapLong  (entry._Value);
        { For SHORT (type=3) values stored in the value field the value is
          left-justified in big-endian files. Shift it down. }
        if (entry._Type = 3) and (entry._Count = 1) then
          entry._Value := entry._Value shr 16;
      end;

      case entry._Tag of
        $0100: width            := entry._Value;
        $0101: height           := entry._Value;
        $0102: begin
                 if entry._Count = 1 then
                   bitspersample := entry._Value
                 else
                 begin
                   saved_pos := fs.Position;
                   fs.Seek(entry._Value, soBeginning);
                   fs.ReadBuffer(bits_array[0], entry._Count * 2);
                   if big_endian then
                     bits_array[0] := SwapWord(bits_array[0]);
                   bitspersample := bits_array[0];
                   fs.Seek(saved_pos, soFromBeginning);
                 end;
               end;
        $0103: begin
                 compression := entry._Value;
                 //if compression = 32946 then compression := 8; {old-style Deflate tag, identical zlib data}
               end;
        $010E: begin
                 desc_len := entry._Count;
                 if desc_len > 0 then
                 begin
                   if desc_len <= 4 then
                   begin
                     SetLength(description, desc_len);
                     Move(entry._Value, description[1], desc_len);
                   end
                   else
                     desc_offset := entry._Value;
                 end;
               end;
        $0111: begin
                 SetLength(strip_offsets, entry._Count);
                 if entry._Count = 1 then
                   strip_offsets[0] := entry._Value
                 else
                 begin
                   saved_pos := fs.Position;
                   fs.Seek(entry._Value, soBeginning);
                   if entry._Type = 3 then
                     for k := 0 to entry._Count - 1 do
                     begin
                       fs.ReadBuffer(w, 2);
                       if big_endian then w := SwapWord(w);
                       strip_offsets[k] := w;
                     end
                   else
                   begin
                     fs.ReadBuffer(strip_offsets[0], entry._Count * 4);
                     if big_endian then
                       for k := 0 to entry._Count - 1 do
                         strip_offsets[k] :=
                           LongInt(SwapLong(LongWord(strip_offsets[k])));
                   end;
                   fs.Seek(saved_pos, soFromBeginning);
                 end;
               end;
        $0115: samplesperpixel  := entry._Value;
        $0116: rowsperstrip     := entry._Value;
        $0117: begin
                 SetLength(strip_bytecounts, entry._Count);
                 if entry._Count = 1 then
                   strip_bytecounts[0] := entry._Value
                 else
                 begin
                   saved_pos := fs.Position;
                   fs.Seek(entry._Value, soBeginning);
                   if entry._Type = 3 then
                     for k := 0 to entry._Count - 1 do
                     begin
                       fs.ReadBuffer(w, 2);
                       if big_endian then w := SwapWord(w);
                       strip_bytecounts[k] := w;
                     end
                   else
                   begin
                     fs.ReadBuffer(strip_bytecounts[0], entry._Count * 4);
                     if big_endian then
                       for k := 0 to entry._Count - 1 do
                         strip_bytecounts[k] :=
                           LongInt(SwapLong(LongWord(strip_bytecounts[k])));
                   end;
                   fs.Seek(saved_pos, soFromBeginning);
                 end;
               end;
        $011C: planar_config    := entry._Value;
        $013D: predictor        := entry._Value;
        $0153: begin
                 if entry._Count = 1 then
                   sample_format := entry._Value
                 else
                 begin
                   saved_pos := fs.Position;
                   fs.Seek(entry._Value, soBeginning);
                   fs.ReadBuffer(sample_format, 2);
                   if big_endian then sample_format := SwapWord(sample_format);
                   fs.Seek(saved_pos, soFromBeginning);
                 end;
               end;
      end;
    end;

    if (desc_len > 4) and (desc_offset > 0) then
    begin
      saved_pos := fs.Position;
      fs.Seek(desc_offset, soBeginning);
      SetLength(description, desc_len);
      fs.ReadBuffer(description[1], desc_len);
      fs.Seek(saved_pos, soFromBeginning);
    end;
    if (Length(description) > 0) and
       (description[Length(description)] = #0) then
      SetLength(description, Length(description) - 1);

    if (width = 0) or (height = 0) then Exit;

    if (compression <> 1) and (compression <> 8) and (compression <> 5) then
    begin
      description := 'Unsupported compression: ' + IntToStr(compression);
      Exit;
    end;
    if planar_config <> 1 then
    begin
      description := 'Unsupported planar configuration.';
      Exit;
    end;

    if sample_format = 3 then is_float := True;

    if samplesperpixel = 1 then
    begin
      if      bitspersample = 8  then row_size := width
      else if bitspersample = 16 then row_size := width * 2
      else if bitspersample = 32 then row_size := width * 4
      else Exit;
    end
    else if samplesperpixel = 3 then
    begin
      if      bitspersample = 8  then row_size := width * 3
      else if bitspersample = 16 then row_size := width * 6
      else if bitspersample = 32 then row_size := width * 12
      else Exit;
    end
    else Exit;

    SetLength(tiffbuffer, row_size);
    if samplesperpixel = 1 then
      SetLength(img, 1, height, width)
    else
      SetLength(img, 3, height, width);

    num_strips  := Length(strip_offsets);
    current_row := 0;

    { ── threaded path: Deflate compressed, multiple strips ──────────────
      Each strip is an independent zlib stream and each strip fills its own
      disjoint block of img rows, so no locking is needed. The compressed
      data is first read sequentially (I/O), then decompressed in parallel. }
    if (compression = 8) and (num_strips > 1) and
       (Length(strip_bytecounts) >= num_strips) then
    begin
      SetLength(comp_strips, num_strips);
      for i := 0 to num_strips - 1 do
      begin
        SetLength(comp_strips[i], strip_bytecounts[i]);
        fs.Seek(strip_offsets[i], soBeginning);
        fs.ReadBuffer(comp_strips[i][0], strip_bytecounts[i]);
      end;

      thread_count := min(get_thread_count, num_strips);
      strips_per_thread := (num_strips + thread_count - 1) div thread_count;
      thread_count := (num_strips + strips_per_thread - 1) div strips_per_thread;
      SetLength(dthreads, thread_count);

      for i := 0 to thread_count - 1 do
      begin
        s0 := i * strips_per_thread;
        s1 := min(s0 + strips_per_thread, num_strips) - 1;
        dthreads[i] := TStripDecompressThread.Create(s0, s1, img, comp_strips,
                         rowsperstrip, height, width, row_size, samplesperpixel,
                         bitspersample, predictor, is_float, big_endian);
        dthreads[i].Start;
      end;

      thread_error := '';
      for i := 0 to thread_count - 1 do
      begin
        dthreads[i].WaitFor;
        measured_max := max(measured_max, dthreads[i].max_found);
        if (thread_error = '') and (dthreads[i].error_msg <> '') then
          thread_error := dthreads[i].error_msg;
        dthreads[i].Free;
      end;

      if thread_error <> '' then
      begin
        description := thread_error;
        Exit;
      end;
    end
    else {sequential paths: uncompressed, LZW, or single strip Deflate}
    for i := 0 to num_strips - 1 do
    begin
      if current_row >= height then Break;
      rows_in_strip := height - current_row;
      if rowsperstrip < rows_in_strip then rows_in_strip := rowsperstrip;
      if rows_in_strip <= 0 then Break;

      fs.Seek(strip_offsets[i], soBeginning);

      if compression = 1 then
      begin
        for k := 0 to rows_in_strip - 1 do
        begin
          fs.ReadBuffer(tiffbuffer[0], row_size);
          ApplyPredictor;
          CopyRowToImg(current_row + k);
        end;
      end
      else if compression = 8 then
      begin
        mem_str := TMemoryStream.Create;
        try
          mem_str.SetSize(strip_bytecounts[i]);
          fs.ReadBuffer(mem_str.Memory^, strip_bytecounts[i]);
          mem_str.Position := 0;
          decomp_str := TDecompressionStream.Create(mem_str);
          try
            for k := 0 to rows_in_strip - 1 do
            begin
              decomp_str.ReadBuffer(tiffbuffer[0], row_size);
              ApplyPredictor;
              CopyRowToImg(current_row + k);
            end;
          finally
            decomp_str.Free;
          end;
        finally
          mem_str.Free;
        end;
      end
      else if compression = 5 then
      begin
        mem_str := TMemoryStream.Create;
        try
          mem_str.SetSize(strip_bytecounts[i]);
          fs.ReadBuffer(mem_str.Memory^, strip_bytecounts[i]);
          lzwOut := nil;
          lzwLen := 0;
          try
            DecompressLZW(mem_str.Memory, strip_bytecounts[i], lzwOut, lzwLen);
            if lzwLen < rows_in_strip * row_size then
            begin
              MessageDlg(Format('LZW failed: expected %d got %d bytes',
                [rows_in_strip * row_size, lzwLen]), mtError, [mbOK], 0);
              Break;
            end;
            lzwPos := 0;
            for k := 0 to rows_in_strip - 1 do
            begin
              System.Move(lzwOut[lzwPos], tiffbuffer[0], row_size);
              Inc(lzwPos, row_size);
              ApplyPredictor;
              CopyRowToImg(current_row + k);
            end;
          finally
            if lzwOut <> nil then ReAllocMem(lzwOut, 0);
          end;
        finally
          mem_str.Free;
        end;
      end;

      current_row := current_row + rows_in_strip;
    end;

    if measured_max > 65535 * 1.5 then
    begin
      scalefactor := 65535 / measured_max;
      for k := 0 to length(img) - 1 do
        for j := 0 to length(img[0]) - 1 do
          for m := 0 to length(img[0,0]) - 1 do
            img[k,j,m] := img[k,j,m] * scalefactor;
      measured_max := 65535;
    end;
    if measured_max = 0 then measured_max := 65535;

    theResult := True;
  finally
    fs.Free;
  end;
end;


end.
