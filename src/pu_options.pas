unit pu_options;

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

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ExtCtrls, ComCtrls;

type

  { Tf_option }

  Tf_option = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CaptureDir: TDirectoryEdit;
    DitherRAonly: TCheckBox;
    Label23: TLabel;
    SettlePixel: TEdit;
    SettleMinTime: TEdit;
    SettleMaxTime: TEdit;
    FileFiltername: TCheckBox;
    FileDate: TCheckBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    Label18: TLabel;
    Label19: TLabel;
    FileObjname: TCheckBox;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    DitherPixel: TEdit;
    SubfolderStep: TCheckBox;
    SubfolderSequence: TCheckBox;
    SubfolderObjname: TCheckBox;
    SubfolderFrametype: TCheckBox;
    PHDhostname: TEdit;
    PHDport: TEdit;
    ElbrusFolder: TEdit;
    ElbrusUnixpath: TEdit;
    GroupBox3: TGroupBox;
    Label12: TLabel;
    Label13: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Notebook1: TNotebook;
    ObserverName: TEdit;
    ObservatoryName: TEdit;
    Page1: TPage;
    Page2: TPage;
    AutoguiderBox: TRadioGroup;
    TabSheet5: TTabSheet;
    TelescopeName: TEdit;
    GroupBox4: TGroupBox;
    elbrus: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label5: TLabel;
    PageControl1: TPageControl;
    Plot: TCheckBox;
    Downsample: TEdit;
    ResolverBox: TRadioGroup;
    SourcesLimit: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Tolerance: TEdit;
    Label7: TLabel;
    MinRadius: TEdit;
    Label6: TLabel;
    PixelSizeFromCamera: TCheckBox;
    FocaleFromTelescope: TCheckBox;
    PixelSize: TEdit;
    Focale: TEdit;
    FocusWindow: TEdit;
    astrometrynet: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Logtofile: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label14: TLabel;
    Panel1: TPanel;
    StarWindow: TEdit;
    procedure FocaleFromTelescopeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PixelSizeFromCameraChange(Sender: TObject);
    procedure ResolverBoxClick(Sender: TObject);
  private
    { private declarations }
    FGetPixelSize, FGetFocale: TNotifyEvent;
    function GetResolver: integer;
    procedure SetResolver(value:integer);
  public
    { public declarations }
    property Resolver: integer read GetResolver write SetResolver;
    property onGetPixelSize : TNotifyEvent read FGetPixelSize write FGetPixelSize;
    property onGetFocale : TNotifyEvent read FGetFocale write FGetFocale;
  end;

var
  f_option: Tf_option;

implementation

{$R *.lfm}

{ Tf_option }


procedure Tf_option.FormCreate(Sender: TObject);
begin
  {$ifdef mswindows}
    ElbrusUnixpath.Visible:=false;
    Label13.Visible:=false;
  {$endif}
  PageControl1.ActivePageIndex:=0;
end;

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

function Tf_option.GetResolver: integer;
begin
  result:=ResolverBox.ItemIndex;
end;

procedure Tf_option.SetResolver(value:integer);
begin
  if (value<0)or(value>1) then exit;
  ResolverBox.ItemIndex:=value;
  ResolverBoxClick(nil);
end;

procedure Tf_option.ResolverBoxClick(Sender: TObject);
begin
  Notebook1.PageIndex:=ResolverBox.ItemIndex;
end;


end.

