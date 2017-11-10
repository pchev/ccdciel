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

uses u_global, u_utils, Graphics, UScaleDPI, cu_camera, indiapi,
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls;

type

  { Tf_preview }

  Tf_preview = class(TFrame)
    BtnPreview: TButton;
    BtnLoop: TButton;
    Panel5: TPanel;
    StackPreview: TCheckBox;
    ExpTime: TComboBox;
    Binning: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    led: TShape;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    StaticText1: TStaticText;
    procedure BtnLoopClick(Sender: TObject);
    procedure BtnPreviewClick(Sender: TObject);
  private
    { private declarations }
    Fcamera: T_camera;
    Frunning,FLoop: boolean;
    FonMsg: TNotifyMsg;
    FonResetStack: TNotifyEvent;
    FonStartExposure: TNotifyEvent;
    FonAbortExposure: TNotifyEvent;
    FonEndControlExposure: TNotifyEvent;
    WaitExposure, ControlExposureOK: boolean;
    procedure EndExposure(Sender: TObject);
    procedure msg(txt:string);
    function GetExposure:double;
    procedure SetExposure(value:double);
    function GetBinning: integer;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Stop;
    function ControlExposure(exp:double; binx,biny: integer):boolean;
    property Running: boolean read Frunning write Frunning;
    property Camera: T_camera read Fcamera write Fcamera;
    property Loop: boolean read FLoop write FLoop;
    property Exposure: double read GetExposure write SetExposure;
    property Bin: integer read GetBinning;
    property onResetStack: TNotifyEvent read FonResetStack write FonResetStack;
    property onStartExposure: TNotifyEvent read FonStartExposure write FonStartExposure;
    property onAbortExposure: TNotifyEvent read FonAbortExposure write FonAbortExposure;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onEndControlExposure: TNotifyEvent read FonEndControlExposure write FonEndControlExposure;
  end;

implementation

{$R *.lfm}

{ Tf_preview }

constructor Tf_preview.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 ScaleDPI(Self);
 Frunning:=false;
end;

destructor  Tf_preview.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_preview.msg(txt:string);
begin
 if assigned(FonMsg) then FonMsg(txt);
end;

procedure Tf_preview.BtnPreviewClick(Sender: TObject);
begin
  if FLoop then exit;
  Frunning:=not Frunning;
  FLoop:=False;
  if Frunning then begin
     if Assigned(FonStartExposure) then FonStartExposure(self);
     if Frunning then begin
        Msg('Start single preview');
     end else begin
        Msg('Cannot start preview now');
     end;
  end else begin
     if Assigned(FonAbortExposure) then FonAbortExposure(self);
  end;
end;

procedure Tf_preview.BtnLoopClick(Sender: TObject);
begin
  Frunning:=not Frunning;
  if Frunning then begin
     if StackPreview.Checked and Assigned(FonResetStack) then FonResetStack(self);
     if Assigned(FonStartExposure) then FonStartExposure(self);
     if Frunning then begin
        led.Brush.Color:=clLime;
        BtnLoop.Caption:='Stop Loop';
        FLoop:=True;
        Msg('Start preview loop');
     end else begin
        FLoop:=False;
        Msg('Cannot start preview loop now');
     end;
  end else begin
     if Assigned(FonAbortExposure) then FonAbortExposure(self);
     led.Brush.Color:=clGray;
     BtnLoop.Caption:='Loop';
     FLoop:=False;
     Msg('Stop preview loop');
  end;
end;

procedure Tf_preview.Stop;
begin
  Frunning:=false;
  FLoop:=false;
  WaitExposure:=false;
  led.Brush.Color:=clGray;
  BtnLoop.Caption:='Loop';
end;

function Tf_preview.GetExposure:double;
begin
  result:=StrToFloatDef(ExpTime.Text,-1);
end;

procedure Tf_preview.SetExposure(value:double);
begin
  if value>=10 then
    ExpTime.Text:=FormatFloat(f0,value)
  else if value>=1 then
      ExpTime.Text:=FormatFloat(f1,value)
  else if value>=0.1 then
      ExpTime.Text:=FormatFloat(f2,value)
  else if value>=0.01 then
      ExpTime.Text:=FormatFloat(f3,value)
  else ExpTime.Text:=FormatFloat(f4,value);
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

function Tf_preview.ControlExposure(exp:double; binx,biny: integer):boolean;
var SaveonNewImage: TNotifyEvent;
    savebinx,savebiny: integer;
    endt: TDateTime;
begin
result:=false;
if Camera.Status=devConnected then begin
  msg('Take control exposure for '+FormatFloat(f1,exp)+' seconds');
  SaveonNewImage:=Camera.onNewImage;
  savebinx:=Camera.BinX;
  savebiny:=Camera.BinY;
  Camera.onNewImage:=@EndExposure;
  if (binx<>savebinx)or(biny<>savebiny) then Camera.SetBinning(binx,biny);
  WaitExposure:=true;
  ControlExposureOK:=false;
  camera.AddFrames:=false;
  Camera.StartExposure(exp);
  endt:=now+60/secperday;
  while WaitExposure and(now<endt) and (not CancelAutofocus) do begin
    Sleep(100);
    Application.ProcessMessages;
  end;
  result:=ControlExposureOK;
  Camera.onNewImage:=SaveonNewImage;
  if (binx<>savebinx)or(biny<>savebiny) then Camera.SetBinning(savebinx,savebiny);
  if result and Assigned(SaveonNewImage) then SaveonNewImage(self);
  Wait(1);
end;
end;

procedure Tf_preview.EndExposure(Sender: TObject);
begin
  ControlExposureOK:=true;
  WaitExposure:=false;
  if assigned(FonEndControlExposure) then FonEndControlExposure(self);
end;

end.

