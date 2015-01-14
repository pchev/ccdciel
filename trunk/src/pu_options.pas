unit pu_options;

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

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ExtCtrls;

type

  { Tf_option }

  Tf_option = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CaptureDir: TDirectoryEdit;
    Plot: TCheckBox;
    Downsample: TEdit;
    SourcesLimit: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    Tolerance: TEdit;
    Label7: TLabel;
    MinRadius: TEdit;
    Label6: TLabel;
    PixelSizeFromCamera: TCheckBox;
    FocaleFromTelescope: TCheckBox;
    PixelSize: TEdit;
    Focale: TEdit;
    FocusWindow: TEdit;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Logtofile: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label14: TLabel;
    Panel1: TPanel;
    RaDecFromTelescope: TRadioButton;
    RaDecFromFits: TRadioButton;
    StarWindow: TEdit;
    procedure FocaleFromTelescopeChange(Sender: TObject);
    procedure PixelSizeFromCameraChange(Sender: TObject);
  private
    { private declarations }
    FGetPixelSize, FGetFocale: TNotifyEvent;
  public
    { public declarations }
    property onGetPixelSize : TNotifyEvent read FGetPixelSize write FGetPixelSize;
    property onGetFocale : TNotifyEvent read FGetFocale write FGetFocale;
  end;

var
  f_option: Tf_option;

implementation

{$R *.lfm}

{ Tf_option }

procedure Tf_option.PixelSizeFromCameraChange(Sender: TObject);
begin
  PixelSize.Enabled:=not PixelSizeFromCamera.Checked;
  if (not PixelSize.Enabled) and (assigned(FGetPixelSize)) then
      FGetPixelSize(self);
end;

procedure Tf_option.FocaleFromTelescopeChange(Sender: TObject);
begin
  Focale.Enabled:=not FocaleFromTelescope.Checked;
  if (not Focale.Enabled) and (assigned(FGetFocale)) then
      FGetFocale(self);
end;

end.

