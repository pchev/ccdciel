unit fu_capture;

{$mode objfpc}{$H+}

{
Copyright (C) 2015 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

interface

uses u_global, Graphics,
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls;

type

  { Tf_capture }

  Tf_capture = class(TFrame)
    Binning: TComboBox;
    BtnStart: TButton;
    FrameType: TComboBox;
    ExpTime: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    SeqNum: TEdit;
    Fname: TEdit;
    StaticText1: TStaticText;
    procedure BtnStartClick(Sender: TObject);
    procedure FrameEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FrameResize(Sender: TObject);
  private
    { private declarations }
    FSeqCount: integer;
    Frunning: boolean;
    FonMsg: TNotifyMsg;
    FonStartExposure: TNotifyEvent;
    FonAbortExposure: TNotifyEvent;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Stop;
    property Running: boolean read Frunning;
    property SeqCount: Integer read FSeqCount write FSeqCount;
    property onStartExposure: TNotifyEvent read FonStartExposure write FonStartExposure;
    property onAbortExposure: TNotifyEvent read FonAbortExposure write FonAbortExposure;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
end;

implementation

{$R *.lfm}

{ Tf_capture }

constructor Tf_capture.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 Frunning:=false;
end;

destructor  Tf_capture.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_capture.BtnStartClick(Sender: TObject);
begin
  Frunning:=not Frunning;
  if Frunning then begin
    FSeqCount:=1;
    if Assigned(FonStartExposure) then FonStartExposure(self);
  end;
  if (not Frunning) and Assigned(FonAbortExposure) then FonAbortExposure(self);
  if Frunning then begin
    BtnStart.Font.Color:=clGreen;
    if Assigned(FonMsg) then FonMsg('Start capture');
  end else begin
    BtnStart.Font.Color:=clDefault;
    if Assigned(FonMsg) then FonMsg('Stop capture');
  end;
end;

procedure Tf_capture.FrameEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if Target is TPanel then begin
     if TPanel(Target).Width>TPanel(Target).Height then begin
        Panel1.ChildSizing.ControlsPerLine:=2;
        Panel1.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
     end else begin
         Panel1.ChildSizing.ControlsPerLine:=99;
         Panel1.ChildSizing.Layout:=cclTopToBottomThenLeftToRight;
     end;
  end;
end;

procedure Tf_capture.FrameResize(Sender: TObject);
begin
  if Parent is TPanel then begin
     if TPanel(Parent).Width>TPanel(Parent).Height then begin
        Panel1.ChildSizing.ControlsPerLine:=2;
        Panel1.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
     end else begin
         Panel1.ChildSizing.ControlsPerLine:=99;
         Panel1.ChildSizing.Layout:=cclTopToBottomThenLeftToRight;
     end;
  end;
end;

procedure Tf_capture.Stop;
begin
  Frunning:=false;
  BtnStart.Font.Color:=clDefault;
end;

end.

