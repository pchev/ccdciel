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

uses u_global, Graphics, UScaleDPI,
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls;

type

  { Tf_preview }

  Tf_preview = class(TFrame)
    BtnPreview: TButton;
    BtnLoop: TButton;
    ExpTime: TComboBox;
    Binning: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    StaticText1: TStaticText;
    procedure BtnLoopClick(Sender: TObject);
    procedure BtnPreviewClick(Sender: TObject);
  private
    { private declarations }
    Frunning,FLoop: boolean;
    FonMsg: TNotifyMsg;
    FonStartExposure: TNotifyEvent;
    FonAbortExposure: TNotifyEvent;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Stop;
    property Running: boolean read Frunning write Frunning;
    property Loop: boolean read FLoop write FLoop;
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
 ScaleDPI(Self);
 Frunning:=false;
end;

destructor  Tf_preview.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_preview.BtnPreviewClick(Sender: TObject);
begin
  if FLoop then exit;
  Frunning:=not Frunning;
  FLoop:=False;
  if Frunning then begin
     if Assigned(FonStartExposure) then FonStartExposure(self);
     if Frunning then begin
        if Assigned(FonMsg) then FonMsg('Start single preview');
     end else begin
        if Assigned(FonMsg) then FonMsg('Cannot start preview now');
     end;
  end else begin
     if Assigned(FonAbortExposure) then FonAbortExposure(self);
  end;
end;

procedure Tf_preview.BtnLoopClick(Sender: TObject);
begin
  Frunning:=not Frunning;
  if Frunning then begin
     if Assigned(FonStartExposure) then FonStartExposure(self);
     if Frunning then begin
        BtnLoop.Font.Color:=clGreen;
        BtnLoop.Caption:='Stop Loop';
        FLoop:=True;
        if Assigned(FonMsg) then FonMsg('Start preview loop');
     end else begin
        FLoop:=False;
        if Assigned(FonMsg) then FonMsg('Cannot start preview now');
     end;
  end else begin
     if Assigned(FonAbortExposure) then FonAbortExposure(self);
     BtnLoop.Font.Color:=clDefault;
     BtnLoop.Caption:='Loop';
     FLoop:=False;
     if Assigned(FonMsg) then FonMsg('Stop preview loop');
  end;
end;

procedure Tf_preview.Stop;
begin
  Frunning:=false;
  FLoop:=false;
  BtnLoop.Font.Color:=clDefault;
  BtnLoop.Caption:='Loop';
end;

end.

