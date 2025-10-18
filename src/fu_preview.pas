unit fu_preview;

{$mode objfpc}{$H+}

{
Copyright (C) 2015 Patrick Chevalley

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

uses u_global, u_utils, Graphics, UScaleDPI, cu_camera, u_translation, u_hints, indiapi,
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, SpinEx, Menus;

type

  { Tf_preview }

  Tf_preview = class(TFrame)
    BtnPreview: TButton;
    BtnLoop: TButton;
    CheckBoxAstrometry: TCheckBox;
    image_inspection1: TCheckBox;
    StackPreview: TCheckBox;
    PreprocessPreview: TCheckBox;
    Fnumber: TComboBox;
    ISObox: TComboBox;
    Label3: TLabel;
    LabelAstrometry: TLabel;
    LabelOffset: TLabel;
    LabelGain: TLabel;
    Panel6: TPanel;
    Panel5: TPanel;
    Panel7: TPanel;
    PanelOffset: TPanel;
    PanelFnumber: TPanel;
    PanelGain: TPanel;
    GainEdit: TSpinEditEx;
    OffsetEdit: TSpinEditEx;
    ExpTime: TComboBox;
    Binning: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    led: TShape;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Title: TLabel;
    procedure BtnLoopClick(Sender: TObject);
    procedure BtnPreviewClick(Sender: TObject);
    procedure CheckBoxAstrometryClick(Sender: TObject);
    procedure ExpTimeKeyPress(Sender: TObject; var Key: char);
    procedure StackPreviewClick(Sender: TObject);
  private
    { private declarations }
    Fcamera: T_camera;
    Frunning,FLoop: boolean;
    FonMsg: TNotifyMsg;
    FonResetStack: TNotifyEvent;
    FonStartExposure: TNotifyEvent;
    FonAbortExposure: TNotifyEvent;
    procedure msg(txt:string; level:integer);
    function GetExposure:double;
    procedure SetExposure(value:double);
    function GetBinning: integer;
    procedure SetBinning(value:integer);
    function GetGain:integer;
    procedure SetGain(value:integer);
    function GetOffset:integer;
    procedure SetOffset(value:integer);
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    procedure SetTitleColor;
    procedure Stop;
    property Running: boolean read Frunning write Frunning;
    property Camera: T_camera read Fcamera write Fcamera;
    property Loop: boolean read FLoop write FLoop;
    property Exposure: double read GetExposure write SetExposure;
    property Gain: integer read GetGain write SetGain;
    property Offset: integer read GetOffset write SetOffset;
    property Bin: integer read GetBinning write SetBinning;
    property onResetStack: TNotifyEvent read FonResetStack write FonResetStack;
    property onStartExposure: TNotifyEvent read FonStartExposure write FonStartExposure;
    property onAbortExposure: TNotifyEvent read FonAbortExposure write FonAbortExposure;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
  end;

implementation

{$R *.lfm}

{ Tf_preview }

constructor Tf_preview.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Panel1.ChildSizing.LeftRightSpacing:=8;
 Panel1.ChildSizing.VerticalSpacing:=4;
 {$endif}
 ScaleDPI(Self);
 Frunning:=false;
 SetLang;
 led.Canvas.AntialiasingMode:=amOn;
end;

destructor  Tf_preview.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_preview.SetTitleColor;
begin
  Title.Color:=InterfaceColor[TitleColor,1];
  Title.Font.Color:=InterfaceColor[TitleColor,2];
  Title.Font.Style:=[fsBold];
end;

procedure Tf_preview.SetLang;
begin
  Title.Caption:=rsPreview;
  Label1.Caption:=rsExposure;
  LabelGain.Caption:=rsGain;
  Label2.Caption:=rsBinning;
  Label3.Caption:=rsFStop;
  BtnPreview.Caption:=rsPreview;
  CheckBoxAstrometry.Caption:=rsAstrometry;
  image_inspection1.caption:=rsImageInspect;
  StackPreview.Caption:=rsStack;
  PreprocessPreview.Caption:=rsPreprocessin;
  BtnLoop.Caption:=rsLoop;
  ExpTime.Hint:=rsExposureTime;
  ISObox.Hint:=rsCameraISO;
  GainEdit.Hint:=rsCameraGain;
  Binning.Hint:=rsCameraBinnin;
  BtnPreview.Hint:=rsStartOnePrev;
  BtnLoop.Hint:=rsLoopPreviewE;
  StackPreview.Hint:=Format(rsStackThePrev, [crlf]);
  led.Hint:='';
end;

procedure Tf_preview.msg(txt:string; level:integer);
begin
 if assigned(FonMsg) then FonMsg(txt,level);
end;

procedure Tf_preview.BtnPreviewClick(Sender: TObject);
begin
  if FLoop then exit;
  Frunning:=not Frunning;
  FLoop:=False;
  EarlyNextExposure:=false;
  if Frunning then begin
     if Assigned(FonStartExposure) then FonStartExposure(self);
     if Frunning then begin
        Msg(rsStartSingleP,2);
     end else begin
        Msg(rsCannotStartP,0);
     end;
  end else begin
     msg(rsStopPreview,2);
     ExpectedStop:=true;
     if Assigned(FonAbortExposure) then FonAbortExposure(self);
  end;
end;

procedure Tf_preview.CheckBoxAstrometryClick(Sender: TObject);
begin
  if CheckBoxAstrometry.Checked then
    LabelAstrometry.Caption:='Apparent: -'+crlf+'J2000   : -'+crlf+'PA      : -'+crlf
  else
    LabelAstrometry.Caption:='';
end;

procedure Tf_preview.ExpTimeKeyPress(Sender: TObject; var Key: char);
begin
  // Only accept positive decimal value
  // inspired from TCustomFloatSpinEdit.KeyPress
  inherited KeyPress(Key);
  if (Key in ['.',',']) then Key := DefaultFormatSettings.Decimalseparator;
  if not (Key in ['0'..'9', DefaultFormatSettings.DecimalSeparator,'+',#8,#9,^C,^X,^V,^Z]) then Key := #0;
end;

procedure Tf_preview.StackPreviewClick(Sender: TObject);
begin
  PreprocessPreview.Enabled:=not StackPreview.Checked;
end;

procedure Tf_preview.BtnLoopClick(Sender: TObject);
begin
  Frunning:=not Frunning;
  if Frunning then begin
     if StackPreview.Checked and Assigned(FonResetStack) then FonResetStack(self);
     FLoop:=True;
     EarlyNextExposure:=ConfigExpEarlyStart and (Exposure>1);
     if Assigned(FonStartExposure) then FonStartExposure(self);
     if Frunning then begin
        CancelAutofocus:=false;
        led.Brush.Color:=clLime;
        BtnLoop.Caption:=rsStopLoop;
        Msg(rsStartPreview,2);
     end else begin
        FLoop:=False;
        EarlyNextExposure:=false;
        Msg(rsCannotStartP2,0);
     end;
  end else begin
     EarlyNextExposure:=false;
     ExpectedStop:=true;
     if Assigned(FonAbortExposure) then FonAbortExposure(self);
     led.Brush.Color:=clGray;
     BtnLoop.Caption:=rsLoop;
     FLoop:=False;
     Msg(rsStopPreviewL,2);
  end;
end;

procedure Tf_preview.Stop;
begin
  Frunning:=false;
  FLoop:=false;
  EarlyNextExposure:=false;
  led.Brush.Color:=clGray;
  BtnLoop.Caption:=rsLoop;
end;

function Tf_preview.GetExposure:double;
begin
  result:=StrToFloatDef(ExpTime.Text,-1);
end;

procedure Tf_preview.SetExposure(value:double);
begin
  ExpTime.Text:=FormatFloat(f9v,value);
end;

function Tf_preview.GetBinning: integer;
var buf:string;
    p:integer;
begin
  p:=pos('x',Binning.Text);
  if p>0 then begin
     buf:=trim(copy(Binning.Text,1,p-1));
     result:=StrToIntDef(buf,1);
  end
  else result:=1;
end;

procedure Tf_preview.SetBinning(value:integer);
var buf:String;
    i: integer;
begin
  buf:=IntToStr(value)+'x'+IntToStr(value);
  i:=Binning.Items.IndexOf(buf);
  if i>=0 then Binning.ItemIndex:=i;
end;

function Tf_preview.GetGain:integer;
begin
  if hasGainISO then
    result:=ISObox.ItemIndex
  else
    result:=GainEdit.Value;
end;

procedure Tf_preview.SetGain(value:integer);
begin
  if hasGainISO then begin
    if (value>=0)and(value<ISObox.Items.Count) then
      ISObox.ItemIndex:=value
  end
  else begin
    GainEdit.Value:=value;
  end;
end;

function Tf_preview.GetOffset:integer;
begin
  result:=OffsetEdit.Value;
end;

procedure Tf_preview.SetOffset(value:integer);
begin
  OffsetEdit.Value:=value;
end;

end.

