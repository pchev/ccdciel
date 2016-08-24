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

uses u_utils,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, enhedits;

type

  { Tf_option }

  Tf_option = class(TForm)
    ButtonDir: TButton;
    BayerMode: TComboBox;
    DebayerPreview: TCheckBox;
    CheckBoxLocalCdc: TCheckBox;
    CalibrationDelay: TEdit;
    AstrometryTimeout: TEdit;
    CygwinPath: TEdit;
    CaptureDir: TEdit;
    GroupBox8: TGroupBox;
    GroupBox9: TGroupBox;
    hemis: TComboBox;
    Label33: TLabel;
    CygwinPanel: TPanel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    latdeg: TFloatEdit;
    LatitudeGroup: TGroupBox;
    latmin: TLongEdit;
    latsec: TFloatEdit;
    long: TComboBox;
    longdeg: TFloatEdit;
    LongitudeGroup: TGroupBox;
    longmin: TLongEdit;
    longsec: TFloatEdit;
    RefColor: TRadioGroup;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SlewPrec: TEdit;
    SlewRetry: TEdit;
    SlewExp: TEdit;
    SlewBin: TEdit;
    GroupBox7: TGroupBox;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    OtherOptions: TEdit;
    GroupBoxSamp: TGroupBox;
    GroupBoxSkychart: TGroupBox;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Notebook2: TNotebook;
    PageSkychart: TPage;
    PageSamp: TPage;
    PanelRemoteCdc: TPanel;
    CdChostname: TEdit;
    CdCport: TEdit;
    PlanetariumBox: TRadioGroup;
    Button1: TButton;
    Button2: TButton;
    DitherRAonly: TCheckBox;
    Label23: TLabel;
    PrecSlewBox: TRadioGroup;
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
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
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
    RefTreshold: TTrackBar;
    procedure ButtonDirClick(Sender: TObject);
    procedure CheckBoxLocalCdcChange(Sender: TObject);
    procedure FocaleFromTelescopeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure latChange(Sender: TObject);
    procedure longChange(Sender: TObject);
    procedure PixelSizeFromCameraChange(Sender: TObject);
    procedure PlanetariumBoxClick(Sender: TObject);
    procedure ResolverBoxClick(Sender: TObject);
  private
    { private declarations }
    FGetPixelSize, FGetFocale: TNotifyEvent;
    Flatitude, Flongitude: double;
    Lockchange: boolean;
    function GetResolver: integer;
    procedure SetResolver(value:integer);
    procedure SetLatitude(value:double);
    procedure SetLongitude(value:double);
  public
    { public declarations }
    property Resolver: integer read GetResolver write SetResolver;
    property Latitude: double read Flatitude write SetLatitude;
    property Longitude: double read Flongitude write SetLongitude;
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
    CygwinPanel.Visible:=true;
    ElbrusUnixpath.Visible:=false;
    Label13.Visible:=false;
  {$endif}
  Lockchange:=false;
  PageControl1.ActivePageIndex:=0;
end;

procedure Tf_option.latChange(Sender: TObject);
begin
  if LockChange then exit;
  if frac(latdeg.Value)>0 then
    Flatitude:=latdeg.value
  else
    Flatitude:=latdeg.value+latmin.value/60+latsec.value/3600;
  if hemis.Itemindex>0 then Flatitude:=-Flatitude;
end;

procedure Tf_option.longChange(Sender: TObject);
begin
  if LockChange then exit;
  if frac(longdeg.Value)>0 then
     Flongitude:=longdeg.value
  else
     Flongitude:=longdeg.value+longmin.value/60+longsec.value/3600;
  if long.Itemindex>0 then Flongitude:=-Flongitude;
end;

procedure Tf_option.SetLatitude(value:double);
var d,m,s : string;
begin
try
  LockChange:=true;
  Flatitude:=value;
  ArToStr4(abs(value),'0.0',d,m,s);
  latdeg.Text:=d;
  latmin.Text:=m;
  latsec.Text:=s;
  if value>=0 then hemis.Itemindex:=0
              else hemis.Itemindex:=1;
finally
  LockChange:=false;
end;
end;

procedure Tf_option.SetLongitude(value:double);
var d,m,s : string;
begin
try
  LockChange:=true;
  Flongitude:=value;
  ArToStr4(abs(value),'0.0',d,m,s);
  longdeg.Text:=d;
  longmin.Text:=m;
  longsec.Text:=s;
  if value>=0 then long.Itemindex:=0
                   else long.Itemindex:=1;
finally
  LockChange:=false;
end;
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

procedure Tf_option.PlanetariumBoxClick(Sender: TObject);
begin
  Notebook2.PageIndex:=PlanetariumBox.ItemIndex;
end;

procedure Tf_option.CheckBoxLocalCdcChange(Sender: TObject);
begin
  if CheckBoxLocalCdc.Checked then begin
    CdChostname.Text:='localhost';
    CdCport.Text:='';
    PanelRemoteCdc.Visible:=false;
  end else begin
    PanelRemoteCdc.Visible:=true;
  end;
end;

procedure Tf_option.ButtonDirClick(Sender: TObject);
begin
SelectDirectoryDialog1.InitialDir:=CaptureDir.text;
SelectDirectoryDialog1.FileName:=CaptureDir.text;
if SelectDirectoryDialog1.Execute then CaptureDir.text:=SelectDirectoryDialog1.FileName;
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

