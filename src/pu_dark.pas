unit pu_dark;

{$mode ObjFPC}{$H+}

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


interface

uses u_global, u_translation, u_utils, cu_camera, cu_fits, math, cu_waitthread,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Spin, StdCtrls, ComCtrls;

type

  { Tf_dark }

  Tf_dark = class(TForm)
    btnStart: TButton;
    btnCancel: TButton;
    cbMultiExp: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Status: TLabel;
    ProgressBarStack: TProgressBar;
    ProgressBarCount: TProgressBar;
    MaxExposure: TSpinEdit;
    Stackcount: TSpinEdit;
    procedure btnCancelClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure cbMultiExpChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TotalChange(Sender: TObject);
  private
    Fcamera: T_camera;
    saveendexposure: TNotifyEvent;
    Ffn: string;
    Frunning: boolean;
    nloop,curloop,nstack,curstack: integer;
    maxexp,curexp,sq2: double;
    FGain,FOffset,FBin: integer;
    curft: TFrameType;
    fstack: TFits;
    fdark: TMemoryStream;
    procedure EndExposure(Sender: TObject);
    procedure NextExposure(data: PtrInt);
  public
    procedure SetLang;
    property camera: T_camera read Fcamera write Fcamera;
    property Gain: integer read FGain write FGain;
    property Offset: integer read FOffset write FOffset;
    property Binning: integer read FBin write FBin;
    property filename: string read Ffn write Ffn;
  end;

var
  f_dark: Tf_dark;

implementation

{$R *.lfm}

{ Tf_dark }

procedure Tf_dark.FormCreate(Sender: TObject);
begin
  Frunning:=false;
  sq2:=sqrt(2);
  FGain:=0;
  FOffset:=0;
  SetLang;
end;

procedure Tf_dark.SetLang;
begin
  Caption:=rsDark;
  Label1.Caption:=rsMaxExposure;
  Label2.Caption:=rsStackingCoun;
  Label3.Caption:=rsCoverTheCame2;
  cbMultiExp.Caption:=rsMultipleDark;
end;

procedure Tf_dark.FormShow(Sender: TObject);
begin
  saveendexposure:=Fcamera.onEndControlExposure;
  ProgressBarStack.Position:=ProgressBarStack.Min;
  ProgressBarCount.Position:=ProgressBarCount.Min;
  Label4.Caption:='';
  if camera.hasGain then
    Label4.Caption:=rsGain+': '+IntToStr(FGain);
  if camera.hasOffset then
    Label4.Caption:=Label4.Caption+'; '+rsOffset2+': '+IntToStr(FOffset);
  TotalChange(self);
end;

procedure Tf_dark.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Fcamera.onEndControlExposure:=saveendexposure;
  try
  if fstack<>nil then fstack.free;
  if fdark<>nil then fdark.free;
  except
  end;
end;

procedure Tf_dark.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Frunning then CanClose:=false;
end;

procedure Tf_dark.cbMultiExpChange(Sender: TObject);
begin
  if cbMultiExp.Checked then
    Label1.Caption:=rsMaxExposure
  else
    Label1.Caption:=rsExposureTime2;
  TotalChange(self);
end;

procedure Tf_dark.btnStartClick(Sender: TObject);
begin
  Frunning:=true;
  maxexp:=MaxExposure.Value;
  if cbMultiExp.Checked then begin
    nloop:=trunc(LogN(sq2,maxexp))+1;
    curloop:=-1;
    curexp:=0;
    curft:=BIAS;
  end
  else begin
    nloop:=-1;
    curloop:=-1;
    curexp:=maxexp;
    curft:=DARK;
  end;
  nstack:=Stackcount.Value;
  ProgressBarCount.Min:=-2;
  ProgressBarCount.Max:=nloop;
  ProgressBarCount.Position:=-1;
  fdark:=TMemoryStream.Create;
  Fcamera.onEndControlExposure:=@EndExposure;
  ProgressBarStack.Min:=-1;
  ProgressBarStack.Max:=nstack;
  ProgressBarStack.Position:=0;
  status.Caption:=rsExposure+' '+FormatFloat(f1,curexp)+' '+rsSeconds;
  fstack:=TFits.Create(self);
  curstack:=1;
  if not Fcamera.ControlExposure(curexp,FBin,FBin,curft,ReadoutModeCapture,FGain,FOffset,true) then begin
     Frunning:=false;
     EndExposure(self);
  end;
end;

procedure Tf_dark.EndExposure(Sender: TObject);
begin
  WaitExecute(1000,@NextExposure,0);
end;

procedure Tf_dark.NextExposure(data: PtrInt);
var fd: TMemoryStream;
    k: integer;
begin
try
  if Frunning and Fcamera.ControlExposureOK then begin
    fstack.Math(Fcamera.Fits,moRunMean,curstack=1,curstack);
    inc(curstack);
    if curstack<=nstack then begin
      ProgressBarStack.Position:=curstack;
      if not Fcamera.ControlExposure(curexp,FBin,FBin,curft,ReadoutModeCapture,FGain,FOffset,true) then begin
        Frunning:=false;
        EndExposure(self);
      end;
      exit;
    end
    else begin
      if curloop<0 then begin
        k:=fstack.Header.Replace('EXTEND',True,5);
        fstack.Header.Insert(k+1,'NEXTEND',nloop+1,'Number of extension');
      end else begin
        fstack.Header.Delete('SIMPLE');
        fstack.Header.Delete('EXTEND');
        fstack.Header.Insert(0,'XTENSION','IMAGE   ','Image extension');
        fstack.Header.Insert(5,'PCOUNT',0,'');
        fstack.Header.Insert(6,'GCOUNT',1,'');
        fstack.Header.Insert(7,'EXTNAME','Exp: '+FormatFloat(f1,curexp),'extension name');
      end;
      fd:=fstack.Stream;
      fd.Position:=0;
      fdark.CopyFrom(fd,fd.Size);
      FreeAndNil(fstack);
      fd.free;
      inc(curloop);
      if curloop<=nloop then begin
        ProgressBarCount.Position:=curloop;
        if curloop=nloop then  begin
          curexp:=MaxExposure.Value;
          curft:=DARK;
        end
        else begin
          curexp:=sq2**curloop;
          curft:=DARK;
        end;
        ProgressBarStack.Position:=0;
        status.Caption:=rsExposure+' '+FormatFloat(f1,curexp)+' '+rsSeconds;
        fstack:=TFits.Create(self);
        curstack:=1;
        Fcamera.ControlExposure(curexp,FBin,FBin,curft,ReadoutModeCapture,FGain,FOffset,true);
        exit;
      end
      else begin
        fdark.SaveToFile(Ffn);
        status.Caption:=format(rsSaved,[rsDark]);
        ProgressBarStack.Position:=ProgressBarStack.Max;
        ProgressBarCount.Position:=ProgressBarCount.Max;
        Frunning:=false;
        FreeAndNil(fdark);
        wait(2);
        ModalResult:=mrOK;
        exit;
      end;
    end;
  end
  else begin
    raise exception.Create(rsExposureFail);
  end;
except
  on e: Exception do begin
    Frunning:=false;
    status.Caption:=e.Message;
    ProgressBarStack.Position:=ProgressBarStack.Min;
    ProgressBarCount.Position:=ProgressBarCount.Min;
    try
    if fstack<>nil then FreeAndNil(fstack);
    if fdark<>nil then FreeAndNil(fdark);
    except
    end;
  end;
end;
end;

procedure Tf_dark.TotalChange(Sender: TObject);
var i,j,k,n: integer;
    tot,texp: double;
begin
  if cbMultiExp.Checked then begin
    n:=trunc(LogN(sq2,MaxExposure.Value))+1;
  end
  else begin
    n:=-1;
  end;
  tot:=0;
  k:=2; // extra time for processing and waiting
  for i:=-1 to n do begin
    if cbMultiExp.Checked then begin
      if i<0 then
        texp:=k
      else if i=n then
        texp:=MaxExposure.Value+k
      else
        texp:=sq2**i+k;
    end
    else begin
      texp:=MaxExposure.Value+k;
    end;
    for j:=1 to Stackcount.Value do begin
      tot:=tot+texp;
    end;
  end;
  status.Caption:=rsTotalExecuti+': '+TimToStr(tot/3600);
end;

procedure Tf_dark.btnCancelClick(Sender: TObject);
begin
  if Frunning then begin
    Fcamera.AbortExposure;
    Frunning:=false;
  end
  else begin
    ModalResult:=mrCancel;
  end;
end;

end.

