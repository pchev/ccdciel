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

uses u_utils, u_global, UScaleDPI, u_translation,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Grids, EditBtn, Spin, enhedits;

type

  { Tf_option }

  Tf_option = class(TForm)
    BtnDisableDelay: TButton;
    BtnDisableFocuserTemp: TButton;
    BtnFolderDefault: TButton;
    BtnFileDefault: TButton;
    AutofocusMultistar: TGroupBox;
    BtnDisableAutofocusTemp: TButton;
    AutofocusTemp: TFloatSpinEdit;
    FilenameSep: TComboBox;
    Debug_Ascom: TCheckBox;
    GroupBox11: TGroupBox;
    Label119: TLabel;
    Label120: TLabel;
    Label121: TLabel;
    Label122: TLabel;
    Label123: TLabel;
    MeridianFlipStopSlaving: TCheckBox;
    Label113: TLabel;
    Label114: TLabel;
    Label115: TLabel;
    Label116: TLabel;
    Label117: TLabel;
    Label118: TLabel;
    PageDitherOnly: TPage;
    Panel2: TPanel;
    Panel3: TPanel;
    DitherWaitTime: TSpinEdit;
    TemperatureScale: TRadioGroup;
    ReadOutCapture: TComboBox;
    FloatSpinEditMa10: TFloatSpinEdit;
    FloatSpinEditMa11: TFloatSpinEdit;
    FloatSpinEditMa12: TFloatSpinEdit;
    FloatSpinEditMa13: TFloatSpinEdit;
    FloatSpinEditMa8: TFloatSpinEdit;
    FloatSpinEditMa9: TFloatSpinEdit;
    FloatSpinEditMi10: TFloatSpinEdit;
    FloatSpinEditMi11: TFloatSpinEdit;
    FloatSpinEditMi12: TFloatSpinEdit;
    FloatSpinEditMi13: TFloatSpinEdit;
    FloatSpinEditMi8: TFloatSpinEdit;
    FloatSpinEditMi9: TFloatSpinEdit;
    GroupBoxReadOut: TGroupBox;
    Label111: TLabel;
    Label112: TLabel;
    LabelMa10: TLabel;
    LabelMa11: TLabel;
    LabelMa12: TLabel;
    LabelMa13: TLabel;
    LabelMa8: TLabel;
    LabelMa9: TLabel;
    LabelMi10: TLabel;
    LabelMi11: TLabel;
    LabelMi12: TLabel;
    LabelMi13: TLabel;
    LabelMi8: TLabel;
    LabelMi9: TLabel;
    PanelW10: TPanel;
    PanelW11: TPanel;
    PanelW12: TPanel;
    PanelW13: TPanel;
    PanelW8: TPanel;
    PanelW9: TPanel;
    ReadOutPreview: TComboBox;
    ReadOutFocus: TComboBox;
    ReadOutAstrometry: TComboBox;
    SafetyActions: TStringGrid;
    ScrollBoxWeather: TScrollBox;
    WeatherRestartDelay: TSpinEdit;
    TabSheet14: TTabSheet;
    UseW1: TCheckBox;
    DomeFlatSetLight: TCheckBox;
    CygwinPath: TDirectoryEdit;
    ASTAPFolder: TDirectoryEdit;
    DomeFlatSetLightON: TEdit;
    DomeFlatSetLightOFF: TEdit;
    FloatSpinEditMa2: TFloatSpinEdit;
    FloatSpinEditMa3: TFloatSpinEdit;
    FloatSpinEditMa4: TFloatSpinEdit;
    FloatSpinEditMa5: TFloatSpinEdit;
    FloatSpinEditMa6: TFloatSpinEdit;
    FloatSpinEditMa7: TFloatSpinEdit;
    FloatSpinEditMi1: TFloatSpinEdit;
    FloatSpinEditMa1: TFloatSpinEdit;
    FloatSpinEditMi2: TFloatSpinEdit;
    FloatSpinEditMi3: TFloatSpinEdit;
    FloatSpinEditMi4: TFloatSpinEdit;
    FloatSpinEditMi5: TFloatSpinEdit;
    FloatSpinEditMi6: TFloatSpinEdit;
    FloatSpinEditMi7: TFloatSpinEdit;
    Label108: TLabel;
    LabelMa2: TLabel;
    LabelMa3: TLabel;
    LabelMa4: TLabel;
    LabelMa5: TLabel;
    LabelMa6: TLabel;
    LabelMa7: TLabel;
    LabelMi2: TLabel;
    LabelMi3: TLabel;
    LabelMi4: TLabel;
    LabelMi5: TLabel;
    LabelMi6: TLabel;
    LabelMi7: TLabel;
    LabelMi1: TLabel;
    LabelMa1: TLabel;
    ObsElev: TFloatSpinEdit;
    Label106: TLabel;
    Label110: TLabel;
    PanelW2: TPanel;
    PanelW3: TPanel;
    PanelW4: TPanel;
    PanelW5: TPanel;
    PanelW6: TPanel;
    PanelW7: TPanel;
    PanelW1: TPanel;
    PlatesolveFolder: TDirectoryEdit;
    DomeFlatTelescopeSlew: TCheckBox;
    DomeFlatTelescopeAz: TFloatSpinEdit;
    DomeFlatTelescopeAlt: TFloatSpinEdit;
    DomeBox: TGroupBox;
    Label102: TLabel;
    Label103: TLabel;
    Label104: TLabel;
    Label105: TLabel;
    Label107: TLabel;
    Label109: TLabel;
    Label52: TLabel;
    Label56: TLabel;
    Label61: TLabel;
    LabelMultistarWarning: TLabel;
    MaxAduFromCamera: TCheckBox;
    ElevationMin: TFloatSpinEdit;
    ClippingLow: TFloatSpinEdit;
    ClippingHigh: TFloatSpinEdit;
    BPMsigma: TFloatSpinEdit;
    CameraAutoCoolTemp: TFloatSpinEdit;
    FlatMinExp: TFloatSpinEdit;
    FlatMaxExp: TFloatSpinEdit;
    FlatLevelMin: TSpinEdit;
    FlatLevelMax: TSpinEdit;
    AutofocusExposure: TFloatSpinEdit;
    AutofocusTolerance: TFloatSpinEdit;
    AutofocusMinSNR: TFloatSpinEdit;
    AutofocusStartHFD: TFloatSpinEdit;
    AutofocusNearHFD: TFloatSpinEdit;
    AutofocusPrecisionSlew: TFloatSpinEdit;
    Downsample: TSpinEdit;
    DitherPixel: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    GroupBox20: TGroupBox;
    Label19: TLabel;
    Label3: TLabel;
    Page5: TPage;
    PageGuiderNone: TPage;
    PanelFocusStar: TPanel;
    PixelSize: TFloatSpinEdit;
    PixelSizeFromCamera: TCheckBox;
    AutofocusSlew: TRadioButton;
    AutofocusInPlace: TRadioButton;
    astap: TGroupBox;
    SettlePixel: TFloatSpinEdit;
    SlewExp: TFloatSpinEdit;
    SlewPrec: TFloatSpinEdit;
    SourcesLimit: TSpinEdit;
    PlatesolveWait: TSpinEdit;
    SlewRetry: TSpinEdit;
    SlewBin: TSpinEdit;
    SlewDelay: TSpinEdit;
    MinutesPastMeridian: TSpinEdit;
    MinutesPastMeridianMin: TSpinEdit;
    MeridianFlipPauseTimeout: TSpinEdit;
    SettleMinTime: TSpinEdit;
    SettleMaxTime: TSpinEdit;
    CalibrationDelay: TSpinEdit;
    MaxAdu: TSpinEdit;
    ASTAPSearchRadius: TSpinEdit;
    ASTAPdownsample: TSpinEdit;
    StarLostCancel: TSpinEdit;
    StarLostRestart: TSpinEdit;
    TabSheet13: TTabSheet;
    Tolerance: TFloatSpinEdit;
    MaxRadius: TFloatSpinEdit;
    Focale: TFloatSpinEdit;
    FocuserTempCoeff: TFloatSpinEdit;
    FocusWindow: TSpinEdit;
    FocuserBacklash: TSpinEdit;
    FocuserDelay: TSpinEdit;
    AutofocusBinning: TSpinEdit;
    AutofocusNearNum: TSpinEdit;
    AutofocusSlippageOffset: TSpinEdit;
    AutofocusDynamicNumPoint: TSpinEdit;
    AutofocusDynamicMovement: TSpinEdit;
    AutofocusMaxSpeed: TSpinEdit;
    AutofocusMinSpeed: TSpinEdit;
    AstrometryTimeout: TSpinEdit;
    StarWindow: TSpinEdit;
    TemperatureSlope: TFloatSpinEdit;
    Languages: TComboBox;
    Label18: TLabel;
    Labelmsg: TLabel;
    Undersampled: TCheckBox;
    UseW10: TCheckBox;
    UseW11: TCheckBox;
    UseW12: TCheckBox;
    UseW13: TCheckBox;
    UseW7: TCheckBox;
    UseW6: TCheckBox;
    UseW5: TCheckBox;
    UseW4: TCheckBox;
    UseW3: TCheckBox;
    UseW2: TCheckBox;
    UseW8: TCheckBox;
    UseW9: TCheckBox;
    VideoPreviewRate: TSpinEdit;
    UseTcpServer: TCheckBox;
    FocuserBacklashActive: TCheckBox;
    FocuserBacklashDirection: TComboBox;
    GroupBox19: TGroupBox;
    Label100: TLabel;
    Label101: TLabel;
    Label37: TLabel;
    FileFolderOpt: TPageControl;
    FolderOpt: TTabSheet;
    FileOpt: TTabSheet;
    TmpDirDefault: TButton;
    ButtonDir: TButton;
    BayerMode: TComboBox;
    Autofocusmode: TRadioGroup;
    AstUseScript: TCheckBox;
    AstCustScript: TEdit;
    ButtonTempDir: TButton;
    CameraAutoCool: TCheckBox;
    AutofocusSlippageCorrection: TCheckBox;
    TempDir: TEdit;
    FlatAutoExposure: TCheckBox;
    FileOptions: TStringGrid;
    FlatExposureBox: TGroupBox;
    Label91: TLabel;
    Label92: TLabel;
    Label93: TLabel;
    Label94: TLabel;
    Label95: TLabel;
    Label96: TLabel;
    FlatType: TRadioGroup;
    Label97: TLabel;
    Label98: TLabel;
    Label99: TLabel;
    StackShow: TCheckBox;
    GroupBox16: TGroupBox;
    GroupBox17: TGroupBox;
    Label81: TLabel;
    Label82: TLabel;
    Label83: TLabel;
    Label84: TLabel;
    Label85: TLabel;
    Label86: TLabel;
    Label87: TLabel;
    Label88: TLabel;
    Label89: TLabel;
    Label90: TLabel;
    Notebook3: TNotebook;
    PageLinGuider: TPage;
    PagePHD: TPage;
    LinGuiderSocket: TEdit;
    LinGuiderHostname: TEdit;
    LinGuiderPort: TEdit;
    Label79: TLabel;
    Label80: TLabel;
    Label78: TLabel;
    platesolve: TGroupBox;
    HorizonFile: TFileNameEdit;
    Label76: TLabel;
    Label77: TLabel;
    MeridianFlipAutofocus: TCheckBox;
    MeridianFlipCalibrate: TCheckBox;
    Page4: TPage;
    PageHNSKY: TPage;
    rbLinUnixSocket: TRadioButton;
    rbLinTCP: TRadioButton;
    FolderOptions: TStringGrid;
    TabSheet12: TTabSheet;
    TemperatureSlopeActive: TCheckBox;
    GroupBox14: TGroupBox;
    GroupBox15: TGroupBox;
    Label74: TLabel;
    Label75: TLabel;
    PanelTemperatureSlope: TPanel;
    GroupBox13: TGroupBox;
    Label70: TLabel;
    Label71: TLabel;
    Label72: TLabel;
    Label73: TLabel;
    Label67: TLabel;
    Label68: TLabel;
    Label69: TLabel;
    SlewFilter: TComboBox;
    FocusStarMag: TComboBox;
    GroupBox10: TGroupBox;
    GroupCorrection: TGroupBox;
    AutofocusSlewStar: TGroupBox;
    GroupBox12: TGroupBox;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    AutofocusNotebook: TNotebook;
    Label59: TLabel;
    Label60: TLabel;
    Label62: TLabel;
    Label63: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    PageDynamic: TPage;
    PageNone: TPage;
    PageVcurve: TPage;
    PageIterative: TPage;
    PanelNearFocus: TPanel;
    PanelAutofocus: TPanel;
    AutofocusMoveDirIn: TRadioButton;
    AutofocusMoveDirOut: TRadioButton;
    FilterList: TStringGrid;
    TabSheet10: TTabSheet;
    RedBalance: TTrackBar;
    GreenBalance: TTrackBar;
    BlueBalance: TTrackBar;
    TabSheet11: TTabSheet;
    StackGroup: TGroupBox;
    VideoGroup: TGroupBox;
    Label44: TLabel;
    Label45: TLabel;
    MeridianWarning: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    MeridianFlipPauseBefore: TCheckBox;
    DebayerPreview: TCheckBox;
    CheckBoxLocalCdc: TCheckBox;
    CaptureDir: TEdit;
    MeridianFlipPauseAfter: TCheckBox;
    GroupBox8: TGroupBox;
    GroupBox9: TGroupBox;
    hemis: TComboBox;
    Label33: TLabel;
    CygwinPanel: TPanel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    latdeg: TFloatEdit;
    LatitudeGroup: TGroupBox;
    latmin: TLongEdit;
    latsec: TFloatEdit;
    long: TComboBox;
    longdeg: TFloatEdit;
    LongitudeGroup: TGroupBox;
    longmin: TLongEdit;
    longsec: TFloatEdit;
    MeridianOption: TRadioGroup;
    MeridianFlipPanel: TPanel;
    Page3: TPage;
    RefColor: TRadioGroup;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    GroupBox7: TGroupBox;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    OtherOptions: TEdit;
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
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
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
    TabSheet9: TTabSheet;
    TelescopeName: TEdit;
    GroupBox4: TGroupBox;
    elbrus: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label5: TLabel;
    PageControl1: TPageControl;
    Plot: TCheckBox;
    ResolverBox: TRadioGroup;
    Label8: TLabel;
    Label9: TLabel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Label7: TLabel;
    Label6: TLabel;
    FocaleFromTelescope: TCheckBox;
    astrometrynet: TGroupBox;
    Label2: TLabel;
    Label4: TLabel;
    Logtofile: TCheckBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label14: TLabel;
    Panel1: TPanel;
    RefTreshold: TTrackBar;
    procedure AutofocusmodeClick(Sender: TObject);
    procedure AutoguiderBoxClick(Sender: TObject);
    procedure BtnDisableAutofocusTempClick(Sender: TObject);
    procedure BtnDisableDelayClick(Sender: TObject);
    procedure BtnDisableFocuserTempClick(Sender: TObject);
    procedure BtnFileDefaultClick(Sender: TObject);
    procedure BtnFolderDefaultClick(Sender: TObject);
    procedure ButtonTempDirClick(Sender: TObject);
    procedure ChangeAutofocusInPlace(Sender: TObject);
    procedure CheckFocuserDirection(Sender: TObject);
    procedure CheckFocusWindow(Sender: TObject);
    procedure CheckStartNearHFD(Sender: TObject);
    procedure ButtonDirClick(Sender: TObject);
    procedure CheckBoxLocalCdcChange(Sender: TObject);
    procedure FlatTypeClick(Sender: TObject);
    procedure FocaleFromTelescopeChange(Sender: TObject);
    procedure FileOrFolderOptionsClick(Sender: TObject);
    procedure FileOrFolderOptionsColRowMoved(Sender: TObject; IsColumn: Boolean;
      sIndex, tIndex: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LanguagesChange(Sender: TObject);
    procedure latChange(Sender: TObject);
    procedure longChange(Sender: TObject);
    procedure MaxAduFromCameraChange(Sender: TObject);
    procedure MeridianOptionClick(Sender: TObject);
    procedure MinutesPastMeridianChange(Sender: TObject);
    procedure MinutesPastMeridianMinChange(Sender: TObject);
    procedure PixelSizeFromCameraChange(Sender: TObject);
    procedure PlanetariumBoxClick(Sender: TObject);
    procedure rbLinSocketChange(Sender: TObject);
    procedure ResolverBoxClick(Sender: TObject);
    procedure SafetyActionsSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
    procedure SafetyActionsValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
    procedure TempDirChange(Sender: TObject);
    procedure TemperatureScaleClick(Sender: TObject);
    procedure TemperatureSlopeActiveClick(Sender: TObject);
    procedure TmpDirDefaultClick(Sender: TObject);
  private
    { private declarations }
    FGetMaxADU, FGetPixelSize, FGetFocale: TNotifyEvent;
    Flatitude, Flongitude: double;
    Lockchange: boolean;
    SaveTemperatureSlope: double;
    procedure msg(txt:string);
    function GetResolver: integer;
    procedure SetResolver(value:integer);
    procedure SetLatitude(value:double);
    procedure SetLongitude(value:double);
    procedure SetLinGuiderUseUnixSocket(value: boolean);
    function  GetLinGuiderUseUnixSocket: boolean;
    procedure FileOrFolderOptionsRenumber(G: TStringGrid);
    procedure Setlang;
  public
    { public declarations }
    LockTemp: Boolean;
    property Resolver: integer read GetResolver write SetResolver;
    property Latitude: double read Flatitude write SetLatitude;
    property Longitude: double read Flongitude write SetLongitude;
    property LinGuiderUseUnixSocket: boolean read GetLinGuiderUseUnixSocket write SetLinGuiderUseUnixSocket;
    property onGetMaxADU : TNotifyEvent read FGetMaxADU write FGetMaxADU;
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
  ScaleDPI(Self);
  Setlang;
  Lockchange:=false;
  LockTemp:=false;
  SafetyActions.RowCount:=SafetyActionNum+1;
  PageControl1.ActivePageIndex:=0;
end;

procedure Tf_option.FormShow(Sender: TObject);
begin
  SaveTemperatureSlope:=TemperatureSlope.Value;
  f_option.TemperatureSlopeActive.Checked:=(f_option.TemperatureSlope.Value<>0);
  FileOrFolderOptionsRenumber(FolderOptions);
  FileOrFolderOptionsRenumber(FileOptions);
  ChangeAutofocusInPlace(nil);
  TemperatureScaleClick(nil);
end;

procedure Tf_option.Setlang;
begin
  Caption := rsOptions2;
  Button1.Caption := rsOK;
  Button2.Caption := rsCancel;
  TabSheet1.Caption := rsFiles;
  Label1.Caption := rsCaptureFolde;
  Logtofile.Caption := rsLogAllMessag;
  Label99.Caption := rsTemporaryFol;
  TmpDirDefault.Caption := rsDefault;
  FileOpt.Caption := rsFileNameOpti;
  BtnFileDefault.Caption := rsDefault;
  label123.Caption := rsSeparator;
  FolderOpt.Caption := rsFolderNameOp;
  BtnFolderDefault.Caption := rsDefault;
  UseTcpServer.Caption := rsAllowToGetPr;
  Label18.Caption := rsLanguage;
  TabSheet2.Caption := rsObservatory;
  Label5.Caption := rsObserverName;
  Label10.Caption := rsObservatoryN;
  Label11.Caption := rsTelescopeNam;
  Label35.Caption := rsLatitude;
  Label36.Caption := rsLongitude;
  Label108.Caption := rsElevation;
  Label76.Caption := rsHorizonProfi;
  Label77.Caption := rsMinimumObser;
  TabSheet8.Caption := rsPreview;
  GroupBox8.Caption := rsColorPreview;
  Label38.Caption := rsBayerMatrixP;
  DebayerPreview.Caption := rsDebayerThePr;
  GroupBox9.Caption := rsReferenceIma;
  Label39.Caption := rsTreshold;
  VideoGroup.Caption := rsVideo;
  Label45.Caption := rsVideoPreview;
  GroupBox10.Caption := rsBadPixelsDet;
  Label62.Caption := rsBadPixelThre;
  Label63.Caption := rsSigma;
  StackGroup.Caption := rsPreviewStack;
  StackShow.Caption := rsShowPreviewS;
  GroupBox19.Caption := rsClippingIndi;
  Label37.Caption := rsShadowADU;
  Label100.Caption := rsHighlightADU;
  TabSheet11.Caption := rsCamera;
  GroupBox1.Caption := rsCCDTemperatu;
  GroupBox14.Caption := rsAutomaticCoo;
  CameraAutoCool.Caption := rsCoolDownWhen;
  TemperatureScale.Caption:=rsTemperatureS;
  Label75.Caption := rsDegree;
  GroupBox15.Caption := rsMaximumTempe;
  TemperatureSlopeActive.Caption := rsLimitTempera;
  Label74.Caption := rsDegreesPerMi;
  GroupBox20.Caption:=rsSensorsPrope;
  label19.Caption:=rsMaximumADU;
  MaxAduFromCamera.Caption:=rsFromCameraDr;
  GroupBoxReadOut.Caption:=rsReadoutModes;
  Label112.Caption:=rsCapture;
  Label113.Caption:=rsPreview;
  Label114.Caption:=rsFocus;
  Label115.Caption:=rsAstrometry;
  TabSheet12.Caption := rsFlat;
  FlatType.Caption := rsSequenceAuto;
  FlatExposureBox.Caption := rsFlatAutoExpo;
  FlatAutoExposure.Caption := rsUseFlatAutom;
  Label91.Caption := rsExposureTime2;
  Label92.Caption := rsMax;
  Label93.Caption := rsMin2;
  Label94.Caption := rsFlatImageMea;
  Label95.Caption := rsMin2;
  Label96.Caption := rsMax;
  Label97.Caption := rsAutomaticFla;
  DomeBox.Caption:=rsDomePanel;
  DomeFlatTelescopeSlew.Caption:=rsSlewTelescop2;
  Label61.Caption:=rsTelescopeAzi;
  Label102.Caption:=rsTelescopeEle;
  Label103.Caption:=rsDegree;
  Label104.Caption:=rsDegree;
  DomeFlatSetLight.Caption:=rsUseExternalC;
  Label106.Caption:=rsLightON;
  Label110.Caption:=rsLightOFF;
  TabSheet10.Caption := rsFocus;
  GroupBox2.Caption := rsStarProfile;
  Label14.Caption := rsStarDetectio;
  Label2.Caption := rsFocusWindowS;
  Undersampled.Caption:=rsSystemIsUnde;
  GroupCorrection.Caption := rsFocuserCorre;
  Label98.Caption := rsStabilizatio;
  BtnDisableDelay.Caption := rsDisable;
  FocuserBacklashActive.Caption := rsBacklashComp;
  GroupBox12.Caption := rsFilterOffset;
  GroupBox16.Caption := rsFocuserTempe3;
  Label84.Caption := rsTemperatureC;
  Label119.Caption := rsRunAutoFocus;
  BtnDisableFocuserTemp.Caption := rsDisable;
  BtnDisableAutofocusTemp.Caption := rsDisable;
  TabSheet3.Caption := rsAutofocus;
  Autofocusmode.Caption := rsAutofocusMet;
  Label49.Caption := rsExposureTime2;
  Label51.Caption := rsSeconds;
  Label58.Caption := rsMoveDirectio;
  AutofocusMoveDirIn.Caption := rsIn;
  AutofocusMoveDirOut.Caption := rsOut;
  Label82.Caption := rsStartFocus;
  AutofocusSlippageCorrection.Caption := rsSlippageCorr;
  Label88.Caption := rsEstimatedSli;
  Label64.Caption := rsNumberOfDyna;
  Label65.Caption := rsMovementBetw;
  Label46.Caption := rsInitialMovem;
  Label47.Caption := rsFinalMovemen;
  Label48.Caption := rsNearFocus;
  Label57.Caption := rsNumberOfExpo;
  Label50.Caption := rsBinning;
  Label54.Caption := rsAutofocusTol;
  Label81.Caption := rsMinSNR;
  Label52.Caption:=rsDefaultBehav;
  AutofocusSlew.Caption:=rsSlewToFocusS2;
  AutofocusInPlace.Caption:=rsStayInPlace;
  AutofocusSlewStar.Caption := rsFocusStarSel;
  Label53.Caption := rsSlewToANearS;
  Label85.Caption := rsSlewWithAPre;
  Label86.Caption := rsArcmin;
  AutofocusMultistar.Caption := rsFocusStarSel;
  label56.Caption := rsDetectMultip;
  Label101.Caption := rsTheMenuFileF;
  TabSheet4.Caption := rsAstrometry;
  GroupBox4.Caption := rsAstrometryOp;
  FocaleFromTelescope.Caption := rsFromTelescop;
  Label4.Caption := rsFocaleLength;
  Label3.Caption := rsPixelSize;
  PixelSizeFromCamera.Caption := rsFromCameraDr;
  ResolverBox.Caption := rsSoftware;
  Label33.Caption := rsTimeout;
  Label6.Caption := rsMaximumSearc;
  Label7.Caption := rsScaleToleran;
  Label8.Caption := rsDownsample;
  Label9.Caption := rsMaximumSourc;
  Plot.Caption := rsCreatePlotOf;
  Label27.Caption := rsOtherOptions;
  Label34.Caption := rsCygwinPath;
  AstUseScript.Caption := rsUseCustomScr;
  Label12.Caption := rsElbrusImages;
  Label13.Caption := rsImagesFolder;
  Label15.Caption := Format(rsBeforeYouCan, [#10]);
  Label44.Caption := rsManyFunction;
  Label78.Caption := rsProgramFolde;
  Label79.Caption := rsWaitAfterSol;
  Label80.Caption := rsSeconds;
  Label105.Caption := rsProgramFolde;
  Label109.Caption := rsMaximumSearc;
  Label107.Caption := rsDownsample;
  TabSheet7.Caption := rsSlewing;
  GroupBox7.Caption := rsPrecisionSle;
  Label28.Caption := rsTargetPrecis;
  Label29.Caption := rsMaximumNumbe;
  Label30.Caption := rsExposureTime3;
  Label31.Caption := rsBinning;
  Label32.Caption := rsControlExpos;
  PrecSlewBox.Caption := rsCorrectionMe;
  Label66.Caption := rsFilter;
  Label87.Caption := rsDelayAfterTe;
  TabSheet9.Caption := rsMeridian;
  MeridianOption.Caption := rsOnMeridianCr;
  Label40.Caption := rsCanTrackPast;
  Label41.Caption := rsMinutes;
  MeridianFlipPauseBefore.Caption := rsPauseBeforeM;
  MeridianFlipPauseAfter.Caption := rsPauseAfterMe;
  Label42.Caption := rsTimeout;
  Label43.Caption := rsMinutes;
  Label59.Caption := rsNoFlipUntilP;
  Label60.Caption := rsMinutes;
  MeridianFlipAutofocus.Caption := rsAutofocusAft;
  MeridianFlipCalibrate.Caption := rsCalibrateAut;
  MeridianFlipStopSlaving.Caption:=rsSuspendDomeS;
  TabSheet5.Caption := rsAutoGuiding;
  AutoguiderBox.Caption := rsSoftware;
  Label120.Caption:= rsNoAutoGuidin+crlf+rsSetTheMeanDi;
  GroupBox5.Caption := rsDithering;
  Label23.Caption := rsPixels;
  Label122.Caption:='';
  DitherRAonly.Caption := rsRAOnly;
  GroupBox11.Caption := rsSettleTolera;
  Label121.Caption:=rsWaitTime;
  GroupBox6.Caption := rsSettleTolera;
  Label20.Caption := rsPixels;
  Label21.Caption := rsMinTime;
  Label22.Caption := rsTimeout;
  Label26.Caption := Format(rsCalibrationD, [#10]);
  GroupBox13.Caption := rsStarLostReco;
  Label70.Caption := rsS;
  Label71.Caption := rsRestartAfter;
  Label72.Caption := rsS;
  Label73.Caption := rsAbortAfter;
  Label16.Caption := rsServer;
  Label17.Caption := rsPort;
  Label89.Caption := rsServer;
  Label90.Caption := rsPort;
  TabSheet6.Caption := rsPlanetarium;
  PlanetariumBox.Caption := rsSoftware;
  CheckBoxLocalCdc.Caption := rsSkychartOnLo;
  Label25.Caption := rsPort;
  Label24.Caption := rsServer;
  hemis.Items[0]:=rsNorth;
  hemis.Items[1]:=rsSouth;
  long.Items[0]:=rsWest;
  long.Items[1]:=rsEast;
  RefColor.Items[0]:=rsRed;
  RefColor.Items[1]:=rsGreen;
  RefColor.Items[2]:=rsBlue;
  FocuserBacklashDirection.Items[0]:=rsIn;
  FocuserBacklashDirection.Items[1]:=rsOut;
  FlatType.Items[0]:=rsNone2;
  FlatType.Items[1]:=rsTwilightSkyF;
  FlatType.Items[2]:=rsDomePanel;
  Autofocusmode.items[0]:=rsVCurve;
  Autofocusmode.items[1]:=rsDynamic;
  Autofocusmode.items[2]:=rsIterative;
  Autofocusmode.items[3]:=rsNone2;
  ResolverBox.Items[2]:=rsNone2;
  PrecSlewBox.Items[0]:=rsMountSync;
  PrecSlewBox.Items[1]:=rsPointingOffs;
  MeridianOption.Items[0]:=rsDoNothing;
  MeridianOption.Items[1]:=rsAutomaticFli;
  MeridianOption.Items[2]:=rsAbort;
  AutoguiderBox.Items[2]:=rsNone2;
  AutoguiderBox.Items[3]:=rsDitherOnly;
  TabSheet13.Caption := rsWeatherStati;
  Label118.Caption:=rsPauseSequenc;
  Label116.Caption:=rsRestartAfter2;
  Label117.Caption:=rsMinutes;
  UseW1.Caption:=rsCloudCover;
  UseW2.Caption:=rsDewPoint;
  UseW3.Caption:=rsHumidity;
  UseW4.Caption:=rsPressure;
  UseW5.Caption:=rsRainRate;
  UseW6.Caption:=rsSkyBrightnes;
  UseW7.Caption:=rsSkyQuality;
  UseW8.Caption:=rsSkyTemperatu;
  UseW9.Caption:=rsStarFWHM;
  UseW10.Caption:=rsTemperature;
  UseW11.Caption:=rsWindDirectio;
  UseW12.Caption:=rsWindGust;
  UseW13.Caption:=rsWindSpeed;
  LabelMi1.Caption:=rsMinimum;
  LabelMi2.Caption:=rsMinimum;
  LabelMi3.Caption:=rsMinimum;
  LabelMi4.Caption:=rsMinimum;
  LabelMi5.Caption:=rsMinimum;
  LabelMi6.Caption:=rsMinimum;
  LabelMi7.Caption:=rsMinimum;
  LabelMi8.Caption:=rsMinimum;
  LabelMi9.Caption:=rsMinimum;
  LabelMi10.Caption:=rsMinimum;
  LabelMi11.Caption:=rsMinimum;
  LabelMi12.Caption:=rsMinimum;
  LabelMi13.Caption:=rsMinimum;
  LabelMa1.Caption:=rsMaximum;
  LabelMa2.Caption:=rsMaximum;
  LabelMa3.Caption:=rsMaximum;
  LabelMa4.Caption:=rsMaximum;
  LabelMa5.Caption:=rsMaximum;
  LabelMa6.Caption:=rsMaximum;
  LabelMa7.Caption:=rsMaximum;
  LabelMa8.Caption:=rsMaximum;
  LabelMa9.Caption:=rsMaximum;
  LabelMa10.Caption:=rsMaximum;
  LabelMa11.Caption:=rsMaximum;
  LabelMa12.Caption:=rsMaximum;
  LabelMa13.Caption:=rsMaximum;
  TabSheet14.Caption := rsSafetyMonito;
  label111.Caption:=rsTheFollowing;
  SafetyActions.Columns[0].Title.Caption:=rsAction;
  SafetyActions.Columns[1].Title.Caption:=rsParameter;
end;

procedure Tf_option.LanguagesChange(Sender: TObject);
begin
   msg(rsTheProgramNe);
end;

procedure Tf_option.msg(txt:string);
begin
 if txt<>'' then Beep;
 Labelmsg.Caption:=txt;
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

procedure Tf_option.MeridianOptionClick(Sender: TObject);
begin
  MeridianFlipPanel.Visible:=(MeridianOption.ItemIndex=1);
end;

procedure Tf_option.MinutesPastMeridianChange(Sender: TObject);
begin
  MinutesPastMeridianMin.MaxValue:=MinutesPastMeridian.Value-1;
end;

procedure Tf_option.MinutesPastMeridianMinChange(Sender: TObject);
begin
  MinutesPastMeridian.MinValue:=MinutesPastMeridianMin.Value+1;
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


procedure Tf_option.MaxAduFromCameraChange(Sender: TObject);
begin
  MaxADU.Enabled:=not MaxADUFromCamera.Checked;
  if (not MaxADU.Enabled) and (assigned(FGetMaxADU)) then
      FGetMaxADU(self);
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

procedure Tf_option.FlatTypeClick(Sender: TObject);
begin
  FlatExposureBox.Visible:=FlatType.ItemIndex>0;
  DomeBox.Visible:=FlatType.ItemIndex=2;
end;

procedure Tf_option.ButtonDirClick(Sender: TObject);
begin
SelectDirectoryDialog1.InitialDir:=CaptureDir.text;
SelectDirectoryDialog1.FileName:=CaptureDir.text;
if SelectDirectoryDialog1.Execute then CaptureDir.text:=SelectDirectoryDialog1.FileName;
end;


procedure Tf_option.ButtonTempDirClick(Sender: TObject);
begin
SelectDirectoryDialog1.InitialDir:=TempDir.text;
SelectDirectoryDialog1.FileName:=TempDir.text;
if SelectDirectoryDialog1.Execute then TempDir.text:=SelectDirectoryDialog1.FileName;
end;

procedure Tf_option.ChangeAutofocusInPlace(Sender: TObject);
begin
  AutofocusMultistar.Visible:=AutofocusInPlace.Checked;
  AutofocusSlewStar.Visible:=not AutofocusMultistar.Visible;
  if AutofocusInPlace.Checked and (Autofocusmode.ItemIndex=0) then
     LabelMultistarWarning.Caption:=rsItIsSuggestT
  else
    LabelMultistarWarning.Caption:='';
end;

procedure Tf_option.CheckFocuserDirection(Sender: TObject);
begin
  if PanelAutofocus.Visible then
  begin
     { force the backlash in the same direction as the autofocus}
     if AutofocusMoveDirIn.Checked then
       FocuserBacklashDirection.ItemIndex:=0
     else
       FocuserBacklashDirection.ItemIndex:=1;
     FocuserBacklashDirection.Enabled:=false;
  end
  else
  begin
     {autofocus is disabled, let select the direction}
     FocuserBacklashDirection.Enabled:=true;
  end;
end;

procedure Tf_option.TmpDirDefaultClick(Sender: TObject);
begin
 TempDir.text:=slash(ConfigDir)+'tmp';
end;

procedure Tf_option.FileOrFolderOptionsClick(Sender: TObject);
begin
  with Sender as TStringGrid do begin
    if Col=1 then begin
     if Cells[Col,Row]='1' then
        Cells[Col,Row]:='0'
     else
        Cells[Col,Row]:='1'
    end;
  end;
end;

procedure Tf_option.FileOrFolderOptionsRenumber(G: TStringGrid);
var i: integer;
begin
  with G do begin
    for i:=0 to RowCount-1 do begin
      Cells[0,i]:=IntToStr(i+1);
    end;
  end;
end;

procedure Tf_option.FileOrFolderOptionsColRowMoved(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  FileOrFolderOptionsRenumber(TStringGrid(Sender));
end;

procedure Tf_option.AutofocusmodeClick(Sender: TObject);
begin
  AutofocusNotebook.PageIndex:=Autofocusmode.ItemIndex;
  PanelAutofocus.Visible:=(Autofocusmode.ItemIndex<3);
  PanelFocusStar.Visible:=PanelAutofocus.Visible;
  PanelNearFocus.Visible:=true;
  CheckFocuserDirection(Sender);
  if AutofocusInPlace.Checked and (Autofocusmode.ItemIndex=0) then
     LabelMultistarWarning.Caption:=rsItIsSuggestT
  else
    LabelMultistarWarning.Caption:='';
end;

procedure Tf_option.AutoguiderBoxClick(Sender: TObject);
begin
  Notebook3.PageIndex:=AutoguiderBox.ItemIndex;
  groupbox5.Visible:=(AutoguiderBox.ItemIndex<2)or(AutoguiderBox.ItemIndex=3);
  if (AutoguiderBox.ItemIndex<2) then begin
    Label23.Caption:=rsPixels;
    Label122.Caption:='';
  end
  else begin
    Label23.Caption:=rsPulseDuratio;
    Label122.Caption:=rsS;
  end;
  GroupBox11.Visible:=(AutoguiderBox.ItemIndex=3);
  groupbox6.Visible:=(AutoguiderBox.ItemIndex=0);
  groupbox13.Visible:=(AutoguiderBox.ItemIndex=0);
  DitherRAonly.Visible:=(AutoguiderBox.ItemIndex=0)or(AutoguiderBox.ItemIndex=3);
end;

procedure Tf_option.BtnDisableDelayClick(Sender: TObject);
begin
  FocuserDelay.Value:=0;
end;

procedure Tf_option.BtnDisableFocuserTempClick(Sender: TObject);
begin
  FocuserTempCoeff.Value:=0.0;
end;

procedure Tf_option.BtnDisableAutofocusTempClick(Sender: TObject);
begin
  AutofocusTemp.Value:=0.0;
end;

procedure Tf_option.BtnFileDefaultClick(Sender: TObject);
var i:integer;
begin
  FilenameSep.ItemIndex:=0;
  for i:=0 to FileNameCount-1 do begin
    if i in [0,1,5] then
      FileOptions.Cells[1,i]:='1'
    else
      FileOptions.Cells[1,i]:='0';
    FileOptions.Cells[2,i]:=FilenameName[i];
  end;
end;

procedure Tf_option.BtnFolderDefaultClick(Sender: TObject);
var i:integer;
begin
  for i:=0 to SubDirCount-1 do begin
    FolderOptions.Cells[1,i]:='0';
    FolderOptions.Cells[2,i]:=SubDirName[i];
  end;
end;

procedure Tf_option.CheckFocusWindow(Sender: TObject);
var a,b: integer;
begin
  msg('');
  a:=StarWindow.Value;
  b:=FocusWindow.Value;
  if (4*a)>b then msg(rsFocusWindowM);
end;

procedure Tf_option.CheckStartNearHFD(Sender: TObject);
var a,b: double;
begin
  msg('');
  a:=AutofocusStartHFD.Value;
  b:=AutofocusNearHFD.Value;
  if a<=b then msg(rsNearHFDMustB);
end;

function Tf_option.GetResolver: integer;
begin
  result:=ResolverBox.ItemIndex;
end;

procedure Tf_option.SetResolver(value:integer);
begin
  if (value<0)or(value>(ResolverBox.Items.Count-1)) then exit;
  ResolverBox.ItemIndex:=value;
  ResolverBoxClick(nil);
end;

procedure Tf_option.ResolverBoxClick(Sender: TObject);
begin
  Notebook1.PageIndex:=ResolverBox.ItemIndex;
end;

procedure Tf_option.SafetyActionsSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
const delim='"'; comma=',';
begin
  if aCol=1 then begin
    if (Editor is TCustomComboBox) then
      with Editor as TCustomComboBox do begin
        Items.CommaText:= delim + SafetyActionName[0] + delim + comma +
                          delim + SafetyActionName[1] + delim + comma +
                          delim + SafetyActionName[2] + delim + comma +
                          delim + SafetyActionName[3] + delim + comma +
                          delim + SafetyActionName[4] + delim + comma +
                          delim + SafetyActionName[5] + delim + comma +
                          delim + SafetyActionName[6] + delim + comma +
                          delim + SafetyActionName[7] + delim + comma +
                          delim + SafetyActionName[8] + delim + comma +
                          delim + SafetyActionName[9] + delim + comma +
                          delim + SafetyActionName[10] + delim + comma +
                          delim + SafetyActionName[11] + delim + comma +
                          delim + SafetyActionName[12] + delim;
      end;
  end;
end;

procedure Tf_option.SafetyActionsValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
var i: integer;
    ok: boolean;
begin
 if aCol=1 then begin
   ok:=false;
   for i:=0 to ord(high(TSafetyAction)) do begin
     if SafetyActionName[i]=NewValue then begin
        ok:=true;
        break;
     end;
   end;
   if not ok then NewValue:=OldValue;
 end;
end;

procedure Tf_option.TempDirChange(Sender: TObject);
{$ifdef mswindows}var c: char;{$endif}
begin
{$ifdef mswindows}
  Labelmsg.Caption:='';
  for c in TempDir.Text do begin
    if (c<#32)or(c>#127) then begin
      Labelmsg.Caption:=rsTemporaryFol2;
      break;
    end;
  end;
{$endif}
end;

procedure Tf_option.TemperatureScaleClick(Sender: TObject);
begin
  if LockTemp then exit;
  if  TemperatureScale.ItemIndex=0 then begin
    Label74.Caption:=format(rsDegreesPerMi,['C']);
    Label75.Caption:=rsDegree+blank+'C';
    Label84.Caption:=format(rsTemperatureC,['C']);
    Label119.Caption := format(rsRunAutoFocus,['C']);
    if sender<>nil then begin
       FocuserTempCoeff.Value:=FocuserTempCoeff.Value*5/9;
       AutofocusTemp.Value:=AutofocusTemp.Value*5/9;
       if TemperatureSlopeActive.Checked then TemperatureSlope.Value:=TemperatureSlope.Value*5/9;
       if CameraAutoCool.Checked then CameraAutoCoolTemp.Value:=TempCelsius(1,CameraAutoCoolTemp.Value);
    end;
  end
  else begin
    Label74.Caption:=format(rsDegreesPerMi,['F']);
    Label75.Caption:=rsDegree+blank+'F';
    Label84.Caption:=format(rsTemperatureC,['F']);
    Label119.Caption := format(rsRunAutoFocus,['F']);
    if sender<>nil then begin
      FocuserTempCoeff.Value:=FocuserTempCoeff.Value*9/5;
      AutofocusTemp.Value:=AutofocusTemp.Value*9/5;
      if TemperatureSlopeActive.Checked then TemperatureSlope.Value:=TemperatureSlope.Value*9/5;
      if CameraAutoCool.Checked then CameraAutoCoolTemp.Value:=TempDisplay(1,CameraAutoCoolTemp.Value);
    end;
  end;
end;

procedure Tf_option.TemperatureSlopeActiveClick(Sender: TObject);
begin
  if TemperatureSlopeActive.Checked then begin
     TemperatureSlope.Value:=SaveTemperatureSlope;
     PanelTemperatureSlope.Visible:=true;
  end
  else begin
     TemperatureSlope.Value:=0;
     PanelTemperatureSlope.Visible:=false;
  end;
end;

procedure Tf_option.SetLinGuiderUseUnixSocket(value: boolean);
begin
{$ifdef mswindows}
value:=false;
{$endif}
rbLinUnixSocket.Checked:=value;
rbLinTCP.Checked:=not value;
end;

function Tf_option.GetLinGuiderUseUnixSocket: boolean;
begin
result:=rbLinUnixSocket.Checked;
end;

procedure Tf_option.rbLinSocketChange(Sender: TObject);
begin
{$ifdef mswindows}
   if rbLinUnixSocket.Checked then begin
      rbLinUnixSocket.Checked:=false;
      rbLinTCP.Checked:=true;
   end;
{$endif}
end;

end.

