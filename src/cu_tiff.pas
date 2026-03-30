unit cu_tiff;

{This version is shared and now part of CCDCiel. Copyright (C) 2026 Patrick Chevalley}

{Writes and reads uncompressed or compressed tiff files from an to an image array
This unit can be called ASTAP-TIFF since it is developped for the ASTAP program
Copyright 2018, 2026 by Han Kleijn, www.hnsky.org
email: han.k.. at...hnsky.org}

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
  FPReadTiff;//for LZW decompression



type
  TDirEntry = record
    _Tag: Word;
    _Type: Word;
    _Count: LongInt;
    _Value: LongInt;
  end;

const
  SoftwareName = 'CCDCIEL' + #0; {GIMP like to have this #0}

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
  thefile: tfilestream;
  i, j, k, m, width2, height2: integer;
  dum: double;
  memstream: TMemoryStream; {For compression}
  compstream: TCompressionStream; {For compression}
  tiffbuffer: array of byte;

        procedure fill_tiffbuffer; inline;
        var
          j: integer;
        begin
          for j := 0 to width2 - 1 do
          begin
            if flip_H then m := width2 - 1 - j else m := j;
            dum := img[0, k, m];
            if dum > $FFFF then dum := $FFFF;
            if dum < 0 then dum := 0;
            PWord(@tiffbuffer[m * 2])^ := NtoLE(round(dum)); {explicit little-endian, safe on all platforms}
          end;
        end;

begin
  result := false;

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

  Directorybw16[9]._Value := LongInt(height2);         { Image Height }

  Directorybw16[06]._count := LongInt(length(description));   { Length Description}
  Directorybw16[14]._count := LongInt(length(softwarename));   { Length software}

  setlength(tiffbuffer,width2*2);

  { Handle Compression Tag and Data Preparation }
  if TCompressionLevel(compressionlevel)<>clnone then
  begin
    Directorybw16[4]._Value := 8; {Tag 0x0103: Compression = 8 (Deflate)}

    {We must compress to memory first to know the final size for the IFD header}
    memstream := TMemoryStream.Create;
    compstream := TCompressionStream.Create(Tcompressionlevel(compressionlevel), memstream); //compression clnone,clfastest, cldefault, clmax

    { Write Image Data to Compression Stream }
    for i := 0 to height2 - 1 do
    begin
      if flip_V = false then k := height2 - 1 - i else k := i; {reverse fits down to counting}
      fill_tiffbuffer;
      compstream.WriteBuffer(tiffbuffer[0], width2 * 2);
    end;

    compstream.Free; {Must free compression stream to flush all data to memstream}
    Directorybw16[10]._Value := LongInt(memstream.Size); { Strip Byte Counts = Compressed Size }
  end
  else
  begin
    Directorybw16[4]._Value := 1; {Tag 0x0103: Compression = 1 (None)}
    Directorybw16[10]._Value := LongInt(2 * width2 * height2); { Strip Byte Counts }
  end;


  { Write TIFF - File for Image with RGB-Values }
  { ------------------------------------------- }
  { Write Header }
  OffsetDir := sizeof(TifHeader) + sizeof(X_Res_Value) + sizeof(Y_Res_Value) + length(description) + length(SoftwareName); {where is the IFD directory}
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

  OffsetStrip := OffsetDir + sizeof(NoOfDirsBW16) + sizeof(Directorybw16) + sizeof(NullString);

  { Set Offset - Adresses into Directory }
  Directorybw16[7]._Value := OffsetStrip;          { StripOffset, location of start image data}
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
    {Write the compressed buffer from memory}
    memstream.Position := 0; {Rewind memory stream}
    thefile.CopyFrom(memstream, memstream.Size);
    memstream.Free;
  end
  else
  begin
    {Standard uncompressed write loop}
    for i := 0 to height2 - 1 do
    begin
      if flip_V = false then k := height2 - 1 - i else k := i; {reverse fits down to counting}
      fill_tiffbuffer;
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
  thefile: tfilestream;
  i, j, k, m, width2, height2: integer;
  memstream: TMemoryStream;
  compstream: TCompressionStream;
  tiffbuffer: array of byte;
begin
  result := false;

  //colours2:=length(img);{nr colours}
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

  Directorybw32[9]._Value := LongInt(Height2);      { Image Height }

  Directorybw32[06]._count := LongInt(length(description));   { Length Description}
  Directorybw32[14]._count := LongInt(length(softwarename));   { Length software}

  setlength(tiffbuffer,width2*4);

  if TCompressionLevel(compressionlevel)<>clnone then
  begin
    Directorybw32[4]._Value := 8; {Deflate Compression}

    memstream := TMemoryStream.Create;
    compstream := TCompressionStream.Create(Tcompressionlevel(compressionlevel), memstream); //compression clnone,clfastest, cldefault, clmax

    { Prepare Data }
    for i := 0 to Height2 - 1 do
    begin
      if flip_V = false then k := height2 - 1 - i else k := i; {reverse fits down to counting}
      for j := 0 to width2 - 1 do
      begin
        if flip_H = true then m := width2 - 1 - j else m := j;
        PSingle(@tiffbuffer[m * 4])^:= img[0, k, m] / 65535; {write float in range 0..1}
      end;
      compstream.WriteBuffer(tiffbuffer[0], width2 * 4);
    end;

    compstream.Free;
    Directorybw32[10]._Value := LongInt(memstream.Size);
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

  { Set Offset - Adresses into Directory }
  Directorybw32[7]._Value := OffsetStrip;        { StripOffset, location of start image data}
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
    memstream.Position := 0;
    thefile.CopyFrom(memstream, memstream.Size);
    memstream.Free;
  end
  else
  begin
    for i := 0 to Height2 - 1 do
    begin
      if flip_V = false then k := height2 - 1 - i else k := i; {reverse fits down to counting}
      for j := 0 to width2 - 1 do
      begin
        if flip_H = true then m := width2 - 1 - j else m := j;
        PSingle(@tiffbuffer[m * 4])^:= img[0, k, m] / 65535; {write floats in range 0..1}
      end;
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
  thefile: tfilestream;
  i, j, k, m, width2, height2: integer;
  dum: double;
  dummy: word;
  memstream: TMemoryStream;
  compstream: TCompressionStream;
  tiffbuffer: array of byte;

        procedure fill_tiffbuffer; inline;
        var
          j,base: integer;
        begin
          for j := 0 to width2 - 1 do
          begin
            if flip_H then m := width2 - 1 - j else m := j;
            base := m * 6;  {each pixel occupies 6 bytes: 3 channels * 2 bytes}

            dum := img[0, k, m];  {red channel}
            if dum > $FFFF then dum := $FFFF;
            if dum < 0 then dum := 0;
            PWord(@tiffbuffer[base])^ := NtoLE(word(round(dum)));

            dum := img[1, k, m];  {green channel}
            if dum > $FFFF then dum := $FFFF;
            if dum < 0 then dum := 0;
            PWord(@tiffbuffer[base + 2])^ := NtoLE(word(round(dum)));

            dum := img[2, k, m];  {blue channel}
            if dum > $FFFF then dum := $FFFF;
            if dum < 0 then dum := 0;
            PWord(@tiffbuffer[base + 4])^ := NtoLE(word(round(dum)));
          end;
        end;



begin
  result := false;
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
  Directoryrgb48[9]._Value := LongInt(height2);      { Image Height }

  Directoryrgb48[06]._count := LongInt(length(description));   { Length Description}
  Directoryrgb48[15]._count := LongInt(length(softwarename));  { Length software}

  setlength(tiffbuffer,width2*2*3);

  { Handle Compression Tag and Data Preparation }
  if TCompressionLevel(compressionlevel)<>clnone then
  begin
    Directoryrgb48[4]._Value := 8; {Tag 0x0103: Compression = 8 (Deflate)}

    memstream := TMemoryStream.Create;
    compstream := TCompressionStream.Create(Tcompressionlevel(compressionlevel), memstream); //compression clnone,clfastest, cldefault, clmax

    { Write Image Data to Compression Stream }
    for i := 0 to height2 - 1 do
    begin
      if flip_V = false then k := height2 - 1 - i else k := i; {reverse fits down to counting}
      fill_tiffbuffer;
      compstream.WriteBuffer(tiffbuffer[0], width2 * 6);
    end;

    compstream.Free;
    Directoryrgb48[10]._Value := LongInt(memstream.Size); { Strip Byte Counts = Compressed Size }
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

  { Set Offset - Adresses into Directory }
  DirectoryRGB48[3]._Value := OffsetBitsPerSample;   { BitsPerSample location containing 1000 1000 1000  (16,16,16)}
  Directoryrgb48[7]._Value := OffsetStrip;          { StripOffset, location of start image data}
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
    memstream.Position := 0;
    thefile.CopyFrom(memstream, memstream.Size);
    memstream.Free;
  end
  else
  begin
    for i := 0 to height2 - 1 do
    begin
      if flip_V = false then k := height2 - 1 - i else k := i; {reverse fits down to counting}
      fill_tiffbuffer;
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
  tiffbuffer: array of byte;

var
  thefile: tfilestream;
  i, j, k, m, width2, height2: integer;

  buf32: single;
  buffer: array[0..3] of byte absolute buf32;
  memstream: TMemoryStream;
  compstream: TCompressionStream;
begin
  result := false;
  try
    thefile := tfilestream.Create(filen2, fmcreate);
  except
    thefile.free;
    exit;
  end;

  //colours2:=length(img);{nr colours}
  width2 := length(img[0, 0]); {width}
  height2 := length(img[0]); {height}

  description := description + #0; {GIMP is complaining about this}

  Directoryrgb96[1]._Value := LongInt(width2);       { Image Width }
  Directoryrgb96[2]._Value := LongInt(Height2);      { Image Height }
  Directoryrgb96[9]._Value := LongInt(Height2);      { Image Height }

  Directoryrgb96[06]._count := LongInt(length(description));   { Length Description}
  Directoryrgb96[15]._count := LongInt(length(softwarename));  { Length software}

  setlength(tiffbuffer,width2*12);//4*3 bytes for one pixel

  if TCompressionLevel(compressionlevel)<>clnone then
  begin
    Directoryrgb96[4]._Value := 8; {Deflate Compression}

    memstream := TMemoryStream.Create;
    compstream := TCompressionStream.Create(Tcompressionlevel(compressionlevel), memstream); //compression clnone,clfastest, cldefault, clmax

    for i := 0 to Height2 - 1 do
    begin
      if flip_V = false then k := height2 - 1 - i else k := i; {reverse fits down to counting}
      for j := 0 to width2 - 1 do
      begin
        if flip_H = true then m := width2 - 1 - j else m := j;
        PSingle(@tiffbuffer[m * 12  ])^:= img[0, k, m] / 65535; {write floats in range 0..1}
        PSingle(@tiffbuffer[m * 12+4])^:= img[1, k, m] / 65535; {write floats in range 0..1}
        PSingle(@tiffbuffer[m * 12+8])^:= img[2, k, m] / 65535; {write floats in range 0..1}
      end;
      compstream.WriteBuffer(tiffbuffer[0], width2 * 12);
    end;

    compstream.Free;
    Directoryrgb96[10]._Value := LongInt(memstream.Size);
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

  { Set Offset - Adresses into Directory }
  DirectoryRGB96[3]._Value := OffsetBitsPerSample;   { BitsPerSample location containing 1000 1000 1000  (16,16,16)}
  Directoryrgb96[7]._Value := OffsetStrip;          { StripOffset, location of start image data}
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
    memstream.Position := 0;
    thefile.CopyFrom(memstream, memstream.Size);
    memstream.Free;
  end
  else
  begin
    for i := 0 to Height2 - 1 do
    begin
      if flip_V = false then k := height2 - 1 - i else k := i; {reverse fits down to counting}
      for j := 0 to width2 - 1 do
      begin
        if flip_H = true then m := width2 - 1 - j else m := j;

        PSingle(@tiffbuffer[m * 12  ])^:= img[0, k, m] / 65535; {write floats in range 0..1}
        PSingle(@tiffbuffer[m * 12+4])^:= img[1, k, m] / 65535; {write floats in range 0..1}
        PSingle(@tiffbuffer[m * 12+8])^:= img[2, k, m] / 65535; {write floats in range 0..1}
      end;
      thefile.writebuffer(tiffbuffer[0], width2 * 12); {works only for byte arrays}
    end;
  end;
  thefile.free;
  result := true;
end;


{ -------------------------------------------------------------------------
  Generic TIFF Read Function (Updated for Big Endian support)
  ------------------------------------------------------------------------- }
procedure read_tiff(filen: string; var img: Timg_array; out description: string;
  out bitspersample: word; out measured_max: double; out theresult: boolean);
var
  fs              : TFileStream;
  tiffbuffer      : array of Byte;
  header          : array[0..7] of byte;
  ifd_offset      : LongInt;
  num_entries     : Word;
  i, j, k, m     : integer;
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
  stride          : integer;
  curr16, prev16, sum16: integer;
  lzwOut          : PByte;
  lzwLen          : PtrInt;
  lzwPos          : PtrInt;
  pixel_value, scalefactor: single;
  big_endian      : boolean;   { TRUE when file uses MM / big-endian byte order }

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

  { Conditionally swap a Word / LongWord depending on big_endian flag }
  function FixW(v: Word): Word; inline;
  begin
    if big_endian then Result := SwapWord(v) else Result := v;
  end;

  function FixL(v: LongWord): LongWord; inline;
  begin
    if big_endian then Result := SwapLong(v) else Result:=v;
  end;

  { ── predictor ─────────────────────────────────────────────────────────── }
  procedure ApplyPredictor;
  var
    p16     : PWord;
    s, jj, b: Integer;
    rowBytes: Integer;
    numComps: Integer;
    temp    : array of Byte;
    srcPtr  : PByte;
  begin
    if (predictor <> 2) and (predictor <> 3) then Exit;
    rowBytes := row_size;
    numComps := samplesperpixel;

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

      { (b) undo byte-plane reordering.
        Little-endian TIFF lays out planes as [byte3|byte2|byte1|byte0]
        (MSB plane first) and the float bytes map as:
            tiffbuffer[j*4+3] = MSB, …, tiffbuffer[j*4+0] = LSB
        Big-endian TIFF lays out planes the same way but the IEEE 754 float
        on disk is also big-endian, so byte[0] is the MSB.  We still want
        host-native floats at the end; CopyRowToImg will handle the final
        byte-swap, so here we always reconstruct in little-endian host order
        (i.e. byte[0] = LSB) regardless of file endianness. }
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
      { After the plane reassembly above the floats are in little-endian byte
        order inside tiffbuffer.  If the file is big-endian we need to swap
        each 4-byte group so that CopyRowToImg (which calls PSingle directly
        and then optionally swaps) works uniformly. }
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

  { ── copy one decompressed/predictor-corrected row into img ────────────── }
  procedure CopyRowToImg(row_inv: integer);
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
    if (header[0] = $49) and (header[1] = $49) then
      big_endian := False                        { II = little-endian }
    else if (header[0] = $4D) and (header[1] = $4D) then
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

      { Byte-swap all IFD entry fields when big-endian.
        TDirEntry is assumed to have the layout:
          _Tag   : Word;
          _Type  : Word;
          _Count : LongWord;
          _Value : LongWord;   (or offset if data doesn't fit in 4 bytes) }
      if big_endian then
      begin
        entry._Tag   := SwapWord  (entry._Tag);
        entry._Type  := SwapWord  (entry._Type);
        entry._Count := SwapLong  (entry._Count);
        entry._Value := SwapLong  (entry._Value);
        { For SHORT (type=3) values stored in the value field the value is
          left-justified in big-endian files, i.e. the actual word is in the
          high 16 bits of _Value after the swap above.  Shift it down. }
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
        $0103: compression      := entry._Value;
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
          for i := 0 to length(img[0,0]) - 1 do
            img[k,j,i] := img[k,j,i] * scalefactor;
      measured_max := 65535;
    end;
    if measured_max = 0 then measured_max := 65535;

    theResult := True;
  finally
    fs.Free;
  end;
end;


end.
