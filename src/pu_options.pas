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

uses u_utils, u_global, UScaleDPI, u_hints, u_translation, u_speech, u_ccdconfig,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LCLType,
  StdCtrls, ExtCtrls, ComCtrls, Grids, EditBtn, CheckLst, Buttons, Spin, enhedits, Types;

type

  { Tf_option }

  Tf_option = class(TForm)
    BtnDelHdr: TButton;
    ButtonSeqDir: TButton;
    ButtonLogDir: TButton;
    CdCPath: TEdit;
    Panel8: TPanel;
    ShowVideo: TCheckBox;
    Label160: TLabel;
    Panel28: TPanel;
    LogDir: TEdit;
    LogDirDefault: TButton;
    StackUseFlat: TCheckBox;
    InstrumentName: TEdit;
    GuideDriftAbort: TCheckBox;
    FitsExt: TComboBox;
    Label155: TLabel;
    Label156: TLabel;
    Label157: TLabel;
    Label158: TLabel;
    Label159: TLabel;
    LabelFlatWarning: TLabel;
    PageInternal: TPage;
    PanelFlatPositionAltAz: TPanel;
    DomeFlatPosition: TRadioGroup;
    SaveFormat: TRadioGroup;
    GuideDriftAbortNum: TSpinEdit;
    StackDebayer: TCheckBox;
    StackUseDark: TCheckBox;
    FileStackFloat: TCheckBox;
    ImageCursor: TComboBox;
    GroupBox33: TGroupBox;
    Label152: TLabel;
    Label153: TLabel;
    Label154: TLabel;
    StackOperation: TRadioGroup;
    Panel27: TPanel;
    StackAlign: TCheckBox;
    StackGroup: TGroupBox;
    SaveStack: TCheckBox;
    StackShow: TCheckBox;
    SeqDir: TEdit;
    SeqDirDefault: TButton;
    HeaderOpt: TTabSheet;
    CustomHeader: TStringGrid;
    UseReadoutMode: TCheckBox;
    Page6: TPage;
    PHDpath: TEdit;
    AzimuthOrigin: TRadioGroup;
    StartPHD: TCheckBox;
    HNSKYPath: TEdit;
    Label151: TLabel;
    SAMPPath: TEdit;
    Label68: TLabel;
    Label69: TLabel;
    StartCdC: TCheckBox;
    EarlyDither: TCheckBox;
    Label150: TLabel;
    Label67: TLabel;
    Panel26: TPanel;
    PanelLocalCdC: TPanel;
    PythonCmd: TEdit;
    GroupBox31: TGroupBox;
    GroupBox32: TPanel;
    hemis: TComboBox;
    Label124: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    latdeg: TFloatEdit;
    LatitudeGroup: TPanel;
    latmin: TLongEdit;
    latsec: TFloatEdit;
    long: TComboBox;
    longdeg: TFloatEdit;
    LongitudeGroup: TPanel;
    longmin: TLongEdit;
    longsec: TFloatEdit;
    Panel22: TPanel;
    Panel23: TPanel;
    Panel24: TPanel;
    Panel25: TPanel;
    StartHNSKY: TCheckBox;
    StartSAMP: TCheckBox;
    TCPIPportDefault: TButton;
    SlewGainEdit: TSpinEdit;
    SlewISObox: TComboBox;
    SlewOffsetEdit: TSpinEdit;
    AutofocusPlanetMovement: TSpinEdit;
    AutofocusPlanetNumPoint: TSpinEdit;
    AutofocusPauseGuider: TCheckBox;
    BtnDisableDelay: TButton;
    BtnDisableFocuserTemp: TButton;
    BtnFolderDefault: TButton;
    BtnFileDefault: TButton;
    AutofocusMultistar: TGroupBox;
    BtnMaxDriftDisable: TButton;
    BtnDisableStarLost: TButton;
    ASTAPadvanced: TButton;
    AutofocusGainEdit: TSpinEdit;
    AutofocusISObox: TComboBox;
    LabelGain: TLabel;
    LabelGain1: TLabel;
    LabelOffset: TLabel;
    LabelOffset1: TLabel;
    ObservatoryDBDelete: TButton;
    ButtonVoiceTest: TButton;
    ButtonVoiceAll: TButton;
    ButtonVoiceNone: TButton;
    AutofocusMultiStarCenter: TCheckBox;
    BGneutralization: TCheckBox;
    Label14: TLabel;
    ObservatoryDB: TComboBox;
    LongitudeError: TLabel;
    AutofocusOffsetEdit: TSpinEdit;
    PagePlaNone: TPage;
    Panel21: TPanel;
    AutofocusPanelGain: TPanel;
    SlewPanelGain: TPanel;
    PanelLeft: TPanel;
    AutofocusPanelOffset: TPanel;
    SlewPanelOffset: TPanel;
    SlewPrec: TFloatSpinEdit;
    SlewRetry: TSpinEdit;
    PythonDefault: TButton;
    TCPIPport: TSpinEdit;
    WantExif: TCheckBox;
    LabelTestVoice: TLabel;
    LowQualityDisplay: TCheckBox;
    NotDisplayCapture: TCheckBox;
    CheckGroupVoice: TCheckGroup;
    GroupBox27: TGroupBox;
    GroupBox28: TGroupBox;
    GroupBox29: TGroupBox;
    GroupBox30: TGroupBox;
    GroupBoxFocus: TGroupBox;
    GroupBoxMeasurement: TGroupBox;
    GuideDriftCancelExposure: TCheckBox;
    GuideDriftRestartDelay: TSpinEdit;
    Label143: TLabel;
    Label144: TLabel;
    GuideDriftMax: TFloatSpinEdit;
    GroupBoxDrift: TGroupBox;
    Label145: TLabel;
    Label146: TLabel;
    Label147: TLabel;
    Label148: TLabel;
    Label149: TLabel;
    Label920: TLabel;
    BtnDisableAutofocusTemp: TButton;
    AutofocusTemp: TFloatSpinEdit;
    Label1119: TLabel;
    ButtonTestEmail: TButton;
    ButtonNotificationAll: TButton;
    ButtonNotificationNone: TButton;
    ButtonHelp: TButton;
    CbShowHints: TCheckBox;
    AstrometryPathPanel: TPanel;
    AstrometryPath: TDirectoryEdit;
    BalanceFromCamera: TCheckBox;
    AstrometryFallback: TCheckBox;
    AutofocusExpTime: TComboBox;
    Label141: TLabel;
    Label142: TLabel;
    PageControl2: TPageControl;
    PagePlanet: TPage;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    Panel20: TPanel;
    Panel7: TPanel;
    Panel9: TPanel;
    smtp_ssltls: TCheckBox;
    FilePack: TCheckBox;
    EmailCondition: TCheckListBox;
    GroupBox25: TGroupBox;
    GroupBox26: TGroupBox;
    Label140: TLabel;
    Panel6: TPanel;
    smtp_host: TEdit;
    smtp_port: TEdit;
    smtp_user: TEdit;
    smtp_pass: TEdit;
    mail_from: TEdit;
    mail_to: TEdit;
    GroupBox24: TGroupBox;
    Label134: TLabel;
    Label135: TLabel;
    Label136: TLabel;
    Label137: TLabel;
    Label138: TLabel;
    Label139: TLabel;
    SaveBitmapFormat: TComboBox;
    SaveBitmap: TCheckBox;
    MeasureNewImage: TCheckBox;
    FocusStarMagAdjust: TCheckBox;
    CheckRecenterTarget: TCheckBox;
    Label130: TLabel;
    Label131: TLabel;
    Label132: TLabel;
    Label133: TLabel;
    RecenterTargetDistance: TFloatSpinEdit;
    GroupBox22: TGroupBox;
    GroupBox23: TGroupBox;
    Label127: TLabel;
    Label128: TLabel;
    AutofocusPeriod: TSpinEdit;
    Label129: TLabel;
    BtnShowPass: TSpeedButton;
    StarLostCancelExposure: TSpinEdit;
    PagePref: TTabSheet;
    PageNotification: TTabSheet;
    PageSequence: TTabSheet;
    PagePerformance: TTabSheet;
    TabSheetVoice: TTabSheet;
    TabSheetEmail: TTabSheet;
    UseFileSequenceWidth: TCheckBox;
    ExpEarlyStart: TCheckBox;
    DomeNoSafetyCheck: TCheckBox;
    DomeSlaveToMount: TCheckBox;
    DomeCloseActions: TStringGrid;
    CanSetGain: TCheckBox;
    FilenameSep: TComboBox;
    Debug_msg: TCheckBox;
    GroupBox11: TGroupBox;
    GroupBox18: TGroupBox;
    GroupBox21: TGroupBox;
    Label119: TLabel;
    Label120: TLabel;
    Label121: TLabel;
    Label122: TLabel;
    Label123: TLabel;
    Label125: TLabel;
    Label126: TLabel;
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
    Panel4: TPanel;
    DomeOpenActions: TStringGrid;
    DomeActionWait: TSpinEdit;
    Panel5: TPanel;
    FileSequenceWidth: TSpinEdit;
    PageDome: TTabSheet;
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
    ScrollBox1: TScrollBox;
    ScrollBoxWeather: TScrollBox;
    WeatherRestartDelay: TSpinEdit;
    PageSafety: TTabSheet;
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
    SourcesLimit: TSpinEdit;
    PlatesolveWait: TSpinEdit;
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
    PageWeather: TTabSheet;
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
    PageFlat: TTabSheet;
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
    LabelR: TLabel;
    LabelG: TLabel;
    LabelB: TLabel;
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
    PageFocus: TTabSheet;
    RedBalance: TTrackBar;
    GreenBalance: TTrackBar;
    BlueBalance: TTrackBar;
    PageCamera: TTabSheet;
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
    Label33: TLabel;
    CygwinPanel: TPanel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    MeridianOption: TRadioGroup;
    MeridianFlipPanel: TPanel;
    Page3: TPage;
    RefColor: TRadioGroup;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    GroupBox7: TGroupBox;
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
    PageGuide: TTabSheet;
    PagePlanetarium: TTabSheet;
    PageSlew: TTabSheet;
    PagePreview: TTabSheet;
    PageMeridian: TTabSheet;
    TelescopeName: TEdit;
    GroupBox4: TGroupBox;
    elbrus: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label5: TLabel;
    PageControl1: TPageControl;
    ResolverBox: TRadioGroup;
    Label8: TLabel;
    Label9: TLabel;
    PageFile: TTabSheet;
    PageObs: TTabSheet;
    PageAutofocus: TTabSheet;
    PageAstrometry: TTabSheet;
    Label7: TLabel;
    Label6: TLabel;
    FocaleFromTelescope: TCheckBox;
    astrometrynet: TGroupBox;
    Label2: TLabel;
    Label4: TLabel;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Panel1: TPanel;
    RefTreshold: TTrackBar;
    procedure ASTAPadvancedClick(Sender: TObject);
    procedure AstUseScriptClick(Sender: TObject);
    procedure AutofocusExpTimeChange(Sender: TObject);
    procedure AutofocusmodeClick(Sender: TObject);
    procedure AutoguiderBoxClick(Sender: TObject);
    procedure BtnDelHdrClick(Sender: TObject);
    procedure BtnDisableAutofocusTempClick(Sender: TObject);
    procedure BtnDisableDelayClick(Sender: TObject);
    procedure BtnDisableFocuserTempClick(Sender: TObject);
    procedure BtnDisableStarLostClick(Sender: TObject);
    procedure BtnFileDefaultClick(Sender: TObject);
    procedure BtnFolderDefaultClick(Sender: TObject);
    procedure BtnMaxDriftDisableClick(Sender: TObject);
    procedure ButtonLogDirClick(Sender: TObject);
    procedure ButtonSeqDirClick(Sender: TObject);
    procedure DomeFlatPositionClick(Sender: TObject);
    procedure LogDirDefaultClick(Sender: TObject);
    procedure ObservatoryDBDeleteClick(Sender: TObject);
    procedure ButtonNotificationAllClick(Sender: TObject);
    procedure ButtonHelpClick(Sender: TObject);
    procedure ButtonNotificationNoneClick(Sender: TObject);
    procedure ButtonTempDirClick(Sender: TObject);
    procedure ButtonTestEmailClick(Sender: TObject);
    procedure ButtonVoiceAllClick(Sender: TObject);
    procedure ButtonVoiceNoneClick(Sender: TObject);
    procedure ButtonVoiceTestClick(Sender: TObject);
    procedure ChangeAutofocusInPlace(Sender: TObject);
    procedure CheckFocuserDirection(Sender: TObject);
    procedure CheckStartNearHFD(Sender: TObject);
    procedure ButtonDirClick(Sender: TObject);
    procedure CheckBoxLocalCdcChange(Sender: TObject);
    procedure DomeSlaveToMountChange(Sender: TObject);
    procedure ExpEarlyStartClick(Sender: TObject);
    procedure FlatTypeClick(Sender: TObject);
    procedure FocaleFromTelescopeChange(Sender: TObject);
    procedure FileOrFolderOptionsClick(Sender: TObject);
    procedure FileOrFolderOptionsColRowMoved(Sender: TObject; IsColumn: Boolean;
      sIndex, tIndex: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure latChange(Sender: TObject);
    procedure longChange(Sender: TObject);
    procedure MaxAduFromCameraChange(Sender: TObject);
    procedure MeridianOptionClick(Sender: TObject);
    procedure MinutesPastMeridianChange(Sender: TObject);
    procedure MinutesPastMeridianMinChange(Sender: TObject);
    procedure ObservatoryDBChange(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure PanelLeftMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PixelSizeFromCameraChange(Sender: TObject);
    procedure PlanetariumBoxClick(Sender: TObject);
    procedure SeqDirDefaultClick(Sender: TObject);
    procedure StackShowChange(Sender: TObject);
    procedure StackUseDarkFlatChange(Sender: TObject);
    procedure StartCdCChange(Sender: TObject);
    procedure StartHNSKYChange(Sender: TObject);
    procedure StartSAMPChange(Sender: TObject);
    procedure TCPIPportDefaultClick(Sender: TObject);
    procedure PythonDefaultClick(Sender: TObject);
    procedure rbLinSocketChange(Sender: TObject);
    procedure BalanceChange(Sender: TObject);
    procedure ResolverBoxClick(Sender: TObject);
    procedure SafetyActionsSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
    procedure SafetyActionsValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
    procedure SlewPrecChange(Sender: TObject);
    procedure BtnShowPassClick(Sender: TObject);
    procedure TempDirChange(Sender: TObject);
    procedure TemperatureScaleClick(Sender: TObject);
    procedure TemperatureSlopeActiveClick(Sender: TObject);
    procedure TmpDirDefaultClick(Sender: TObject);
    procedure UseFileSequenceWidthClick(Sender: TObject);
    procedure UseReadoutModeChange(Sender: TObject);
  private
    { private declarations }
    FGetMaxADU, FGetPixelSize, FGetFocale, FShowHelp: TNotifyEvent;
    Flatitude, Flongitude: double;
    FAutofocusExposure: double;
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
    procedure SetAutofocusExpTime(val: double);
    procedure SelectPage(Sender: TObject);
    procedure IncPage(Sender: TObject);
    procedure SelectNextPage(direction: integer);
    procedure CheckLongitude;
    function  GetAutoguiderType: integer;
    procedure SetAutoguiderType(value: integer);
  public
    { public declarations }
    LockTemp: Boolean;
    procedure Setlang;
    procedure SetAutofocusMode(value: TAutofocusMode);
    function  GetAutofocusMode: TAutofocusMode;
    procedure LoadObservatoryDB(defaultobs:string);
    procedure SaveObservatoryDB;
    property AutofocusExp: double read FAutofocusExposure write SetAutofocusExpTime;
    property Resolver: integer read GetResolver write SetResolver;
    property Latitude: double read Flatitude write SetLatitude;
    property Longitude: double read Flongitude write SetLongitude;
    property LinGuiderUseUnixSocket: boolean read GetLinGuiderUseUnixSocket write SetLinGuiderUseUnixSocket;
    property AutoguiderType: integer read GetAutoguiderType write SetAutoguiderType;
    property onGetMaxADU : TNotifyEvent read FGetMaxADU write FGetMaxADU;
    property onGetPixelSize : TNotifyEvent read FGetPixelSize write FGetPixelSize;
    property onGetFocale : TNotifyEvent read FGetFocale write FGetFocale;
    property onShowHelp: TNotifyEvent read FShowHelp write FShowHelp;
  end;

var
  f_option: Tf_option;

implementation

{$R *.lfm}

{ Tf_option }


procedure Tf_option.FormCreate(Sender: TObject);
var i: integer;
    b: TSpeedButton;
begin
  {$ifdef mswindows}
    CygwinPanel.Visible:=true;
    ElbrusUnixpath.Visible:=false;
    Label13.Visible:=false;
  {$else}
  AstrometryPathPanel.Visible:=true;
  {$endif}
  ScaleDPI(Self);
  Setlang;
  Lockchange:=false;
  LockTemp:=false;
  FilterList.RowCount:=MaxFilter;
  SafetyActions.RowCount:=SafetyActionNum+1;
  PageControl1.ActivePageIndex:=0;
  CustomHeader.RowCount:=MaxCustomHeaders+1;
  b:=TSpeedButton.Create(self);
  b.GroupIndex:=99876;
  b.AllowAllUp:=true;
  b.Constraints.MinHeight:=DoScaleY(24);
  b.Layout:=blGlyphBottom;
  b.Caption:='↑';
  b.tag:=1001;
  b.OnClick:=@IncPage;
  b.Parent:=PanelLeft;
  for i:=0 to PageControl1.PageCount-1 do begin
    b:=TSpeedButton.Create(self);
    b.GroupIndex:=99870;
    b.Constraints.MinHeight:=DoScaleY(24);
    b.Layout:=blGlyphBottom;
    b.Caption:=PageControl1.Pages[i].Caption;
    b.tag:=i;
    if i=0 then b.down:=true;
    b.OnClick:=@SelectPage;
    b.Parent:=PanelLeft;
  end;
  b:=TSpeedButton.Create(self);
  b.GroupIndex:=99877;
  b.AllowAllUp:=true;
  b.Constraints.MinHeight:=DoScaleY(24);
  b.Layout:=blGlyphBottom;
  b.Caption:='↓';
  b.tag:=1002;
  b.OnClick:=@IncPage;
  b.Parent:=PanelLeft;
end;

procedure Tf_option.SelectPage(Sender: TObject);
begin
  if sender is TSpeedButton then
     PageControl1.ActivePageIndex:=TSpeedButton(Sender).Tag;
end;

procedure Tf_option.SelectNextPage(direction: integer);
var i: integer;
begin
  if direction<0 then begin
    if PageControl1.ActivePageIndex<PageControl1.PageCount-1 then
       PageControl1.ActivePageIndex:=PageControl1.ActivePageIndex+1;
  end
  else begin
    if PageControl1.ActivePageIndex>0 then
       PageControl1.ActivePageIndex:=PageControl1.ActivePageIndex-1;
  end;
  for i:=0 to PanelLeft.ControlCount-1 do  begin
    if TSpeedButton(PanelLeft.Controls[i]).Tag=PageControl1.ActivePageIndex then
      TSpeedButton(PanelLeft.Controls[i]).Down:=True;
  end;
end;

procedure Tf_option.IncPage(Sender: TObject);
begin
  if sender is TSpeedButton then begin
    TSpeedButton(sender).Down:=false;
    if TSpeedButton(Sender).Tag=1002 then
       SelectNextPage(-1)
    else if TSpeedButton(Sender).Tag=1001 then
       SelectNextPage(1);
end;
end;

procedure Tf_option.PanelLeftMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  SelectNextPage(WheelDelta);
end;

procedure Tf_option.FormShow(Sender: TObject);
begin
  SaveTemperatureSlope:=TemperatureSlope.Value;
  f_option.TemperatureSlopeActive.Checked:=(f_option.TemperatureSlope.Value<>0);
  FileOrFolderOptionsRenumber(FolderOptions);
  FileOrFolderOptionsRenumber(FileOptions);
  ChangeAutofocusInPlace(nil);
  TemperatureScaleClick(nil);
  panel4.Visible:=DomeSlaveToMount.Checked;
  AutoguiderBoxClick(nil);
  ExpEarlyStartClick(nil);
  BalanceChange(nil);
  StartCdCChange(nil);
  StartHNSKYChange(nil);
  StartSAMPChange(nil);
  StackShowChange(nil);
end;

procedure Tf_option.Setlang;
var i: integer;
begin
  Caption := rsOptions2;
  Button1.Caption := rsOK;
  Button2.Caption := rsCancel;
  ButtonHelp.Caption:=rsHelp;
  PagePref.Caption:=format(rsPreferences,['']);
  PageFile.Caption := rsFiles;
  Label1.Caption := rsCaptureFolde;
  SeqDirDefault.Caption := rsDefault;
  Label152.Caption:=rsSequenceFold;
  Label99.Caption := rsTemporaryFol;
  TmpDirDefault.Caption := rsDefault;
  Label160.Caption := rsLogFolder;
  LogDirDefault.Caption := rsDefault;
  Label150.Caption := rsTCPIPServerP;
  TCPIPportDefault.Caption := rsDefault;
  Label124.Caption:=rsPythonComman;
  PythonDefault.Caption:=rsDefault;
  FileOpt.Caption := rsFileNameOpti;
  BtnFileDefault.Caption := rsDefault;
  label123.Caption := rsSeparator;
  FolderOpt.Caption := rsFolderNameOp;
  BtnFolderDefault.Caption := rsDefault;
  UseFileSequenceWidth.Caption:=rsFixedSequenc;
  FilePack.Caption:=rsCompressTheF;
  Label156.Caption:=rsFITSFileExte;
  SaveFormat.Caption:=rsFileFormat;
  SaveFormat.Items[1]:=rsTIFFWithFITS;
  WantExif.Caption:=rsAddFITSKeywo;
  Label18.Caption := rsLanguage;
  CbShowHints.Caption:=rsShowHints;
  Debug_msg.Caption:=rsVerboseDevic;
  SaveBitmap.Caption:=rsSaveAPNGFile;
  PageObs.Caption := rsObservatory;
  GroupBox27.Caption:=rsInformation;
  GroupBox28.Caption:=rsCoordinates;
  GroupBox29.Caption:=rsRiseSetCondi;
  Label14.Caption:=rsPresetList;
  ObservatoryDBDelete.Caption:=rsDelete;
  Label5.Caption := rsObserverName;
  Label10.Caption := rsObservatoryN;
  Label11.Caption := rsTelescopeNam;
  Label35.Caption := rsLatitude;
  Label36.Caption := rsLongitude;
  Label108.Caption := rsElevation;
  Label76.Caption := rsHorizonProfi;
  Label77.Caption := rsMinimumObser;
  AzimuthOrigin.Caption:=rsAzimuthOrigi;
  AzimuthOrigin.Items[0]:=rsNorth;
  AzimuthOrigin.Items[1]:=rsSouth;
  PageDome.Caption:=rsDome;
  DomeNoSafetyCheck.Caption:=rsAllowToOpenT;
  DomeSlaveToMount.Caption:=rsAutomaticall;
  Label125.Caption:=rsWaitTimeBetw;
  Label126.Caption:=rsSeconds;
  GroupBox18.Caption:=rsOpenDomeSequ;
  DomeOpenActions.Columns[0].Title.Caption:=rsAction;
  DomeOpenActions.Columns[0].PickList.Clear;
  DomeOpenActions.Columns[0].PickList.Add(DomeOpenActionName[0]);
  DomeOpenActions.Columns[0].PickList.Add(DomeOpenActionName[1]);
  DomeOpenActions.Columns[0].PickList.Add(DomeOpenActionName[2]);
  DomeOpenActions.Columns[0].PickList.Add(DomeOpenActionName[3]);
  DomeOpenActions.Columns[0].PickList.Add(DomeOpenActionName[4]);
  DomeOpenActions.Columns[0].PickList.Add(DomeOpenActionName[5]);
  GroupBox21.Caption:=rsCloseDomeSeq;
  DomeCloseActions.Columns[0].Title.Caption:=rsAction;
  DomeCloseActions.Columns[0].PickList.Clear;
  DomeCloseActions.Columns[0].PickList.Add(DomeCloseActionName[0]);
  DomeCloseActions.Columns[0].PickList.Add(DomeCloseActionName[1]);
  DomeCloseActions.Columns[0].PickList.Add(DomeCloseActionName[2]);
  DomeCloseActions.Columns[0].PickList.Add(DomeCloseActionName[3]);
  DomeCloseActions.Columns[0].PickList.Add(DomeCloseActionName[4]);
  DomeCloseActions.Columns[0].PickList.Add(DomeCloseActionName[5]);
  PagePreview.Caption := rsPreview;
  GroupBox8.Caption := rsColorPreview;
  Label38.Caption := rsBayerMatrixP;
  DebayerPreview.Caption := rsDebayerThePr;
  BayerMode.Items[4]:=rsAutomatic;
  BalanceFromCamera.Caption:=rsUseDSLRColor;
  BGneutralization.Caption:=rsBackgroundNe;
  GroupBox26.Caption:=rsColorBalance;
  GroupBox9.Caption := rsReferenceIma;
  Label39.Caption := rsTreshold;
  VideoGroup.Caption := rsVideo;
  ShowVideo.Caption := rsShowVideoCap;
  Label45.Caption := rsVideoPreview;
  GroupBox10.Caption := rsBadPixelsDet;
  Label62.Caption := rsBadPixelThre;
  Label63.Caption := rsSigma;
  GroupBox33.Caption:=rsImageCursor;
  Label153.Caption:=rsSelectTheCur;
  Label154.Caption:='('+rsTheProgramNe+')';
  ImageCursor.Items[0]:=rsSmallCross;
  ImageCursor.Items[1]:=rsBigCross;
  ImageCursor.Items[2]:=rsSystemCross;
  StackGroup.Caption := rsShortExposur;
  StackShow.Caption := rsShowStacking;
  SaveStack.Caption := rsSaveIndividu;
  StackAlign.Caption:= rsStarAlignmen;
  FileStackFloat.Caption:=rsFloatingPoin;
  StackUseDark.Caption:=rsApplyDarkAnd;
  StackUseFlat.Caption:=rsApplyFlat;
  StackUseFlat.Hint:=rsFlatProcessi;
  StackDebayer.Caption:=rsDebayer;
  StackOperation.Items[0]:=rsAddImages;
  StackOperation.Items[1]:=rsMeanOfImages;
  GroupBox19.Caption := rsClippingIndi;
  Label37.Caption := rsShadowADU;
  Label100.Caption := rsHighlightADU;
  PageCamera.Caption := rsCamera;
  GroupBox1.Caption := rsSensorTemperatu;
  GroupBox14.Caption := rsAutomaticCoo;
  CameraAutoCool.Caption := rsCoolDownWhen;
  TemperatureScale.Caption:=rsTemperatureS;
  TemperatureScale.Items[0]:=rsCelsius;
  TemperatureScale.Items[1]:=rsFahrenheit;
  Label75.Caption := rsDegree;
  GroupBox15.Caption := rsMaximumTempe;
  TemperatureSlopeActive.Caption := rsLimitTempera;
  Label74.Caption := rsDegreesPerMi;
  GroupBox20.Caption:=rsSensorsPrope;
  label19.Caption:=rsMaximumADU;
  MaxAduFromCamera.Caption:=rsFromCameraDr;
  GroupBox31.Caption:=rsGain+' / '+rsOffset2;
  CanSetGain.Caption:=rsManageTheGai;
  GroupBoxReadOut.Caption:=rsReadoutModes;
  UseReadoutMode.Caption:=rsUseSpecificR;
  Label112.Caption:=rsCapture;
  Label113.Caption:=rsPreview;
  Label114.Caption:=rsFocus;
  Label115.Caption:=rsAstrometry;
  PageSequence.Caption:=rsSequence;
  ExpEarlyStart.Caption:=rsStartNewExpo;
  Label147.Caption:=rsGlobalAction;
  GroupBoxMeasurement.Caption:=rsMeasurementO;
  MeasureNewImage.Caption:=rsAutomaticHFD;
  GroupBox23.Caption:=rsRecenterSequ;
  CheckRecenterTarget.Caption:=rsRunAstrometr;
  Label130.Caption:=rsRecenterIfTh;
  Label131.Caption:=rsArcmin;
  Label149.Caption:=rsLimitedTo15x;
  GroupBoxFocus.Caption:=rsPeriodicAuto;
  Label148.Caption:=rsThisOptionsA;
  GroupBox16.Caption := rsFocuserTempe3;
  Label84.Caption := rsTemperatureC;
  Label119.Caption := rsRunAutoFocus;
  AutofocusTemp.Hint := rsAutofocusIfT;
  BtnDisableFocuserTemp.Caption := rsDisable;
  BtnDisableAutofocusTemp.Caption := rsDisable;
  GroupBox22.Caption:=rsPeriodicAuto;
  Label127.Caption:=rsAutomaticall2;
  Label128.Caption:=rsMinutes;
  PageFlat.Caption := rsFlat;
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
  DomeFlatPosition.Caption:=rsPosition;
  DomeFlatPosition.Items[0]:=rsAlt+'/'+rsAz;
  DomeFlatPosition.Items[1]:=rsPark;
  Label61.Caption:=rsTelescopeAzi;
  Label102.Caption:=rsTelescopeEle;
  Label103.Caption:=rsDegree;
  Label104.Caption:=rsDegree;
  DomeFlatSetLight.Caption:=rsUseExternalC;
  Label106.Caption:=rsLightON;
  Label110.Caption:=rsLightOFF;
  PageFocus.Caption := rsFocus;
  GroupBox2.Caption := rsStarProfile;
  Label2.Caption := rsFocusWindowS;
  Undersampled.Caption:=rsSystemIsUnde;
  GroupCorrection.Caption := rsFocuserCorre;
  Label98.Caption := rsStabilizatio;
  BtnDisableDelay.Caption := rsDisable;
  FocuserBacklashActive.Caption := rsBacklashComp;
  GroupBox12.Caption := rsFilterOffset;
  PageAutofocus.Caption := rsAutofocus;
  Autofocusmode.Caption := rsAutofocusMet;
  Label49.Caption := rsExposureTime2;
  Label51.Caption := rsSeconds;
  LabelGain.Caption:=rsGain;
  LabelOffset.Caption:=rsOffset2;
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
  Label67.Caption := rsWhenPerformi;
  Label52.Caption:=rsDefaultBehav;
  Label141.Caption:=rsNumberOfDyna;
  Label142.Caption:=rsMovementBetw;
  AutofocusSlew.Caption:=rsSlewToFocusS2;
  AutofocusInPlace.Caption:=rsStayInPlace;
  AutofocusSlewStar.Caption := rsFocusStarSel;
  Label53.Caption := rsSlewToANearS;
  Label85.Caption := rsSlewWithAPre;
  Label86.Caption := rsArcmin;
  FocusStarMagAdjust.Caption:=rsAllowToSelec;
  AutofocusMultistar.Caption := rsFocusStarSel;
  label56.Caption := rsDetectMultip;
  AutofocusPauseGuider.Caption:=rsPauseGuiding;
  AutofocusMultiStarCenter.Caption:=rsUseOnlyStarN;
  Label101.Caption := rsTheMenuFileF;
  PageAstrometry.Caption := rsAstrometry;
  GroupBox4.Caption := rsAstrometryOp;
  FocaleFromTelescope.Caption := rsFromTelescop;
  Label4.Caption := rsFocaleLength;
  Label132.Caption:='[mm]';
  Label3.Caption := rsPixelSize;
  PixelSizeFromCamera.Caption := rsFromCameraDr;
  ResolverBox.Caption := rsSoftware;
  Label33.Caption := rsTimeout;
  Label133.Caption:=rsSeconds2;
  Label6.Caption := rsMaximumSearc;
  Label7.Caption := rsScaleToleran;
  Label8.Caption := rsDownsample;
  Label9.Caption := rsMaximumSourc;
  Label27.Caption := rsOtherOptions;
  Label34.Caption := rsCygwinPath;
  AstUseScript.Caption := rsUseCustomScr;
  label129.Caption := rsCommandPath;
  AstrometryFallback.Caption:=rsUseAsFallbac;
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
  ASTAPadvanced.Caption:=rsAdvancedSett;
  PageSlew.Caption := rsSlewing;
  GroupBox7.Caption := rsPrecisionSle;
  Label28.Caption := rsTargetPrecis;
  Label29.Caption := rsMaximumNumbe;
  Label30.Caption := rsExposureTime3;
  LabelGain1.Caption:=rsGain;
  LabelOffset1.Caption:=rsOffset2;
  Label31.Caption := rsBinning;
  Label32.Caption := rsControlExpos;
  PrecSlewBox.Caption := rsCorrectionMe;
  Label66.Caption := rsFilter;
  Label87.Caption := rsDelayAfterTe;
  PageMeridian.Caption := rsMeridian;
  MeridianOption.Caption := rsOnMeridianCr;
  Label40.Caption := rsCanTrackPast;
  Label41.Caption := rsMinutes;
  Label158.Caption:= '';
  MeridianFlipPauseBefore.Caption := rsPauseBeforeM;
  MeridianFlipPauseAfter.Caption := rsPauseAfterMe;
  Label42.Caption := rsTimeout;
  Label43.Caption := rsMinutes;
  Label59.Caption := rsNoFlipUntilP;
  Label60.Caption := rsMinutes;
  MeridianFlipAutofocus.Caption := rsAutofocusAft;
  MeridianFlipCalibrate.Caption := rsCalibrateAut;
  MeridianFlipStopSlaving.Caption:=rsSuspendDomeS;
  PageGuide.Caption := rsAutoGuiding;
  AutoguiderBox.Caption := rsSoftware;
  Label155.Caption:= rsToUseTheInte;
  Label120.Caption:= rsNoAutoGuidin+crlf+rsSetTheMeanDi;
  GroupBox5.Caption := rsDithering;
  Label23.Caption := rsPixels;
  EarlyDither.Caption:=rsDitherWhileD;
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
  Label145.Caption:=rsCancelExposu2;
  Label146.Caption:=rsCount;
  Label70.Caption := rsS;
  Label71.Caption := rsRestartAfter;
  Label72.Caption := rsS;
  Label73.Caption := rsAbortAfter;
  BtnDisableStarLost.Caption:=rsDisable;
  Label16.Caption := rsServer;
  Label17.Caption := rsPort;
  Label89.Caption := rsServer;
  Label90.Caption := rsPort;
  StartPHD.Caption := Format(rsStartS, ['PHD2']);
  GroupBoxDrift.Caption:=rsGuidingDrift;
  Label1119.Caption:=rsPixel;
  Label920.Caption:=rsMaximumDrift;
  GuideDriftCancelExposure.Caption:=rsCancelAndRes;
  Label143.Caption:=rsRestartDelay;
  Label144.Caption:=rsS;
  BtnMaxDriftDisable.Caption:=rsDisable;
  GuideDriftAbort.Caption:=rsAbortAfterRe;
  Label157.Caption:=rsTimeInARow;
  PagePlanetarium.Caption := rsPlanetarium;
  PlanetariumBox.Caption := rsSoftware;
  CheckBoxLocalCdc.Caption := rsSkychartOnLo;
  StartCdC.Caption := Format(rsStartS,['Skychart']);
  StartHNSKY.Caption := Format(rsStartS,['HNSKY']);
  StartSAMP.Caption := Format(rsStartS,['SAMP']);
  label68.Caption := rsProgramPath;
  label69.Caption := rsProgramPath;
  label151.Caption := rsProgramPath;
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
  Autofocusmode.items[0]:=rsDynamic;
  Autofocusmode.items[1]:=rsVCurve;
  Autofocusmode.items[2]:=rsIterative;
  Autofocusmode.items[3]:=rsPlanet;
  Autofocusmode.items[4]:=rsNone2;
  ResolverBox.Items[2]:=rsNone2;
  PrecSlewBox.Items[0]:=rsMountSync;
  PrecSlewBox.Items[1]:=rsPointingOffs;
  MeridianOption.Items[0]:=rsDoNothing;
  MeridianOption.Items[1]:=rsAutomaticFli;
  MeridianOption.Items[2]:=rsAbort;
  AutoguiderBox.Items[1]:=rsInternal;
  AutoguiderBox.Items[3]:=rsDitherOnly;
  AutoguiderBox.Items[4]:=rsNone2;
  PageWeather.Caption := rsWeatherStati;
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
  PageSafety.Caption := rsSafetyMonito;
  label111.Caption:=rsTheFollowing;
  SafetyActions.Columns[0].Title.Caption:=rsAction;
  SafetyActions.Columns[1].Title.Caption:=rsParameter;
  PagePerformance.Caption:=rsPerformance;
  GroupBox30.Caption:=rsDisplaySpeed;
  LowQualityDisplay.Caption:=rsLowQualityIm;
  NotDisplayCapture.Caption:=rsDoNotDisplay;
  PageNotification.Caption := rsNotification;
  Label140.Caption:=rsPleaseSeeThe;
  GroupBox24.Caption:=rsEmailConfigu;
  TabSheetEmail.Caption:=rsEmailConfigu;
  Label134.Caption:=rsSMTPServerAd;
  Label135.Caption:=rsSMTPServerPo;
  Label136.Caption:=rsUserName;
  Label137.Caption:=rsPassword;
  Label138.Caption:=rsFromEmailAdd;
  Label139.Caption:=rsDestinationE;
  smtp_ssltls.Caption:=rsSecureSSLTLS;
  ButtonTestEmail.Caption:=rsSendTestEmai;
  GroupBox25.Caption:=rsEmailOn;
  EmailCondition.Items[0]:=rsSequenceNorm;
  EmailCondition.Items[1]:=rsSequenceAbno;
  EmailCondition.Items[2]:=rsAutoguiderEr;
  EmailCondition.Items[3]:=rsAutofocusErr2;
  EmailCondition.Items[4]:=rsMeridianFlip8;
  EmailCondition.Items[5]:=rsTargetInitia2;
  CheckGroupVoice.Items[0]:=rsInteractionD;
  CheckGroupVoice.Items[1]:=rsSequenceStep;
  CheckGroupVoice.Items[2]:=rsErrorMessage;
  CheckGroupVoice.Items[3]:=rsEmailNotific;
  ButtonNotificationAll.Caption:=rsAll;
  ButtonNotificationNone.Caption:=rsNone2;
  TabSheetVoice.Caption:=rsVoiceConfigu;
  CheckGroupVoice.Caption:=rsVoiceConfigu;
  ButtonVoiceAll.Caption:=rsAll;
  ButtonVoiceNone.Caption:=rsNone2;
  ButtonVoiceTest.Caption:=rsTest;
  //Hint
  CaptureDir.Hint:=rsTheBaseFolde;
  TempDir.Hint:=rsATemporaryDi;
  FileOpt.Hint:=rsSelectTheInf;
  FolderOpt.Hint:=rsSelectTheInf2;
  ObserverName.Hint:=rsTheObserverN;
  ObservatoryName.Hint:=rsTheObservato;
  TelescopeName.Hint:=rsTheTelescopeNameForI;
  HorizonFile.Hint:=rsTheHorizonPr;
  ElevationMin.Hint:=rsTheMinimalOb;
  CanSetGain.Hint:=rsIfNotChecked;
  FocuserBacklashDirection.Hint:=Format(rsTheDirection, [crlf]);
  FocuserBacklashActive.Hint:=Format(rsActivateBack, [crlf]);
  AutofocusMoveDirIn.Hint:=rsThePreferedF;
  AutofocusMoveDirOut.Hint:=rsThePreferedF;
  Autofocusmode.Hint:='- '+Format(rsUseDynamic2,[crlf+blank,crlf+blank])+crlf+'- '+rsUseVcurveWi2+crlf+'- '+Format(rsUseIterati2,[crlf+blank]);
  AutofocusSlippageCorrection.Hint:=Format(rsTryToCorrect, [crlf]);
  FocusStarMag.Hint:=Format(rsTheMagnitude, [crlf]);
  AutofocusPauseGuider.Hint:=rsBeSureToPaus;
  AstrometryPath.Hint := rsLetBlankForD;
  ElbrusFolder.Hint:=rsTheElbrusIma;
  ElbrusUnixpath.Hint:=rsTheUnixPathE;
  CheckRecenterTarget.Hint:=Format(rsActiveOnlyIf, [crlf]);

  for i:=0 to PanelLeft.ControlCount-1 do begin
    if PanelLeft.Controls[i] is TSpeedButton then
       with PanelLeft.Controls[i] as TSpeedButton do begin
          if Tag<PageControl1.PageCount then
            Caption:=PageControl1.Pages[Tag].Caption;
       end;
  end;

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
  if longdeg.Value=180 then longdeg.Text:='180';
  if frac(longdeg.Value)>0 then
     Flongitude:=longdeg.value
  else
     Flongitude:=longdeg.value+longmin.value/60+longsec.value/3600;
  if long.Itemindex>0 then Flongitude:=-Flongitude;
  Flongitude:=to180(Flongitude);
  CheckLongitude;
end;

procedure Tf_option.MeridianOptionClick(Sender: TObject);
begin
  MeridianFlipPanel.Visible:=(MeridianOption.ItemIndex=1);
end;

procedure Tf_option.MinutesPastMeridianChange(Sender: TObject);
begin
  MinutesPastMeridianMin.MaxValue:=MinutesPastMeridian.Value-1;
  label41.Caption:=rsMinutes+' ('+FormatFloat(f1,15*MinutesPastMeridian.Value/60)+'°)';
end;

procedure Tf_option.MinutesPastMeridianMinChange(Sender: TObject);
begin
  MinutesPastMeridian.MinValue:=MinutesPastMeridianMin.Value+1;
  label60.Caption:=rsMinutes+' ('+FormatFloat(f1,15*MinutesPastMeridianMin.Value/60)+'°)';
  if MinutesPastMeridianMin.Value<0 then
     Label158.Caption:=rsWarningDoNot
  else
     Label158.Caption:='';
end;

procedure Tf_option.LoadObservatoryDB(defaultobs:string);
var obsdb: TCCDconfig;
    obsname: string;
    i,n,k,o: integer;
begin
  obsdb:=TCCDConfig.Create(self);
  obsdb.Filename:=slash(ConfigDir)+'Observatory.cfg';
  ObservatoryDB.Clear;
  n:=obsdb.GetValue('/NumObservatory',0);
  o:=-1;
  for i:=1 to n do begin
    obsname:=obsdb.GetValue('/Obs_'+inttostr(i)+'/ObservatoryName','');
    k:=ObservatoryDB.Items.Add(obsname);
    if obsname=defaultobs then o:=k;
  end;
  if o>=0 then ObservatoryDB.ItemIndex:=o;
  obsdb.Free;
end;


procedure Tf_option.SaveObservatoryDB;
var obsdb: TCCDconfig;
    obsname, obskey: string;
    i,n: integer;
begin
try
  obsname:=f_option.ObservatoryName.Text;
  obskey:='';
  obsdb:=TCCDConfig.Create(self);
  obsdb.Filename:=slash(ConfigDir)+'Observatory.cfg';
  n:=obsdb.GetValue('/NumObservatory',0);
  for i:=1 to n do begin
    if obsname=obsdb.GetValue('/Obs_'+inttostr(i)+'/ObservatoryName','') then begin
      obskey:='Obs_'+inttostr(i);
      break;
    end;
  end;
  if obskey='' then begin
    n:=n+1;
    obsdb.SetValue('/NumObservatory',n);
    obskey:='Obs_'+inttostr(n);
  end;
  obsdb.SetValue('/'+obskey+'/ObservatoryName',obsname);
  obsdb.SetValue('/'+obskey+'/ObservatoryLatitude',f_option.Latitude);
  obsdb.SetValue('/'+obskey+'/ObservatoryLongitude',f_option.Longitude);
  obsdb.SetValue('/'+obskey+'/ObservatoryElevation',f_option.ObsElev.Value);
  obsdb.SetValue('/'+obskey+'/ObserverName',f_option.ObserverName.Text);
  obsdb.SetValue('/'+obskey+'/TelescopeName',f_option.TelescopeName.Text);
  obsdb.SetValue('/'+obskey+'/HorizonFile',f_option.HorizonFile.FileName);
  obsdb.SetValue('/'+obskey+'/ElevationMin',f_option.ElevationMin.Value);
  obsdb.Flush;
  obsdb.Free;
  except
    on E: Exception do msg('Error saving observatory database: '+ E.Message);
  end;
end;

procedure Tf_option.ObservatoryDBChange(Sender: TObject);
var obsdb: TCCDconfig;
    obskey: string;
    i: integer;
begin
 if ObservatoryDB.Text<>'' then begin
   obsdb:=TCCDConfig.Create(self);
   obsdb.Filename:=slash(ConfigDir)+'Observatory.cfg';
   i:=ObservatoryDB.ItemIndex+1;
   obskey:='Obs_'+inttostr(i);
   ObservatoryName.Text:=obsdb.GetValue('/'+obskey+'/ObservatoryName',ObservatoryName.Text);
   Latitude:=obsdb.GetValue('/'+obskey+'/ObservatoryLatitude',Latitude);
   Longitude:=obsdb.GetValue('/'+obskey+'/ObservatoryLongitude',Longitude);
   ObsElev.Value:=obsdb.GetValue('/'+obskey+'/ObservatoryElevation',ObsElev.Value);
   ObserverName.Text:=obsdb.GetValue('/'+obskey+'/ObserverName',ObserverName.Text);
   TelescopeName.Text:=obsdb.GetValue('/'+obskey+'/TelescopeName',TelescopeName.Text);
   HorizonFile.FileName:=obsdb.GetValue('/'+obskey+'/HorizonFile',HorizonFile.FileName);
   ElevationMin.Value:=obsdb.GetValue('/'+obskey+'/ElevationMin',ElevationMin.Value);
   obsdb.Free;
 end;
end;

procedure Tf_option.ObservatoryDBDeleteClick(Sender: TObject);
var obsdb: TCCDconfig;
    obskey,v1,v2,v3,v4,v5,v6,v7,v8: string;
    i,k,n,d: integer;
begin
try
 d:=ObservatoryDB.ItemIndex+1;
 if (d<1) then exit;
 obsdb:=TCCDConfig.Create(self);
 obsdb.Filename:=slash(ConfigDir)+'Observatory.cfg';
 n:=obsdb.GetValue('/NumObservatory',0);
 obsdb.DeletePath(UnicodeString('/Obs_'+inttostr(d)));
 k:=0;
 for i:=1 to n do begin
   obskey:='Obs_'+inttostr(i);
   v1:=obsdb.GetValue('/'+obskey+'/ObservatoryName','');
   if v1='' then continue;
   v2:=obsdb.GetValue('/'+obskey+'/ObservatoryLatitude','');
   v3:=obsdb.GetValue('/'+obskey+'/ObservatoryLongitude','');
   v4:=obsdb.GetValue('/'+obskey+'/ObservatoryElevation','');
   v5:=obsdb.GetValue('/'+obskey+'/ObserverName','');
   v6:=obsdb.GetValue('/'+obskey+'/TelescopeName','');
   v7:=obsdb.GetValue('/'+obskey+'/HorizonFile','');
   v8:=obsdb.GetValue('/'+obskey+'/ElevationMin','');
   obsdb.DeletePath(UnicodeString(obskey));
   inc(k);
   obskey:='Obs_'+inttostr(k);
   obsdb.SetValue('/'+obskey+'/ObservatoryName',v1);
   obsdb.SetValue('/'+obskey+'/ObservatoryLatitude',v2);
   obsdb.SetValue('/'+obskey+'/ObservatoryLongitude',v3);
   obsdb.SetValue('/'+obskey+'/ObservatoryElevation',v4);
   obsdb.SetValue('/'+obskey+'/ObserverName',v5);
   obsdb.SetValue('/'+obskey+'/TelescopeName',v6);
   obsdb.SetValue('/'+obskey+'/HorizonFile',v7);
   obsdb.SetValue('/'+obskey+'/ElevationMin',v8);
 end;
 obsdb.SetValue('/NumObservatory',k);
 obsdb.Flush;
 obsdb.Free;
 LoadObservatoryDB('');
 except
   on E: Exception do msg('Error saving observatory database: '+ E.Message);
 end;
end;

procedure Tf_option.PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
begin
  // prevent focus error on Gtk2
  ActiveControl:=Button1;
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
  CheckLongitude;
finally
  LockChange:=false;
end;
end;

procedure Tf_option.CheckLongitude;
begin
  if (ObsTimeZone<>0) and (abs(ObsTimeZone+FLongitude/15)>3) then begin
    LongitudeError.Caption:=Format(rsTheComputerT, [FormatFloat(f1, ObsTimeZone), FormatFloat(f1, abs(FLongitude))+blank+long.Text])+crlf+rsBeCarefulOft;
  end
  else
    LongitudeError.Caption:='';
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

procedure Tf_option.SeqDirDefaultClick(Sender: TObject);
begin
  SeqDir.text:=ConfigDir;
end;

procedure Tf_option.LogDirDefaultClick(Sender: TObject);
begin
  LogDir.text:=slash(ConfigDir)+'Log';
end;

procedure Tf_option.StackShowChange(Sender: TObject);
begin
  SaveStack.Enabled:=StackShow.Checked;
  StackAlign.Enabled:=StackShow.Checked;
  StackOperation.Enabled:=StackShow.Checked;
  FileStackFloat.Enabled:=StackShow.Checked;
  StackUseDark.Enabled:=StackShow.Checked;
  StackUseFlat.Enabled:=StackShow.Checked;
  StackDebayer.Enabled:=StackShow.Checked;
end;

procedure Tf_option.StackUseDarkFlatChange(Sender: TObject);
begin
  if not StackUseDark.Checked then StackUseFlat.Checked:=false;
end;

procedure Tf_option.StartCdCChange(Sender: TObject);
begin
  CdCPath.Enabled:=StartCdC.Checked;
end;

procedure Tf_option.StartHNSKYChange(Sender: TObject);
begin
 HNSKYPath.Enabled:=StartHNSKY.Checked;
end;

procedure Tf_option.StartSAMPChange(Sender: TObject);
begin
 SAMPPath.Enabled:=StartSAMP.Checked;
end;

procedure Tf_option.TCPIPportDefaultClick(Sender: TObject);
begin
  TCPIPport.Value:=3277;
end;

procedure Tf_option.PythonDefaultClick(Sender: TObject);
begin
  PythonCmd.Text:=defPython;
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
  PanelLocalCdC.Visible:=not PanelRemoteCdc.Visible;
end;

procedure Tf_option.DomeSlaveToMountChange(Sender: TObject);
begin
  panel4.Visible:=DomeSlaveToMount.Checked;
end;

procedure Tf_option.ExpEarlyStartClick(Sender: TObject);
begin
   GroupBoxMeasurement.Enabled:=ExpEarlyStart.Checked;
end;

procedure Tf_option.FlatTypeClick(Sender: TObject);
begin
  FlatExposureBox.Visible:=FlatType.ItemIndex>0;
  FlatLevelMax.Visible:=FlatType.ItemIndex=1;
  Label95.Visible:=FlatLevelMax.Visible;
  Label96.Visible:=FlatLevelMax.Visible;
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

procedure Tf_option.ButtonTestEmailClick(Sender: TObject);
var subject,txt,r: string;
    savehost,saveport,saveuser,savepass,savefrom,saveto: string;
    savessltls: boolean;
begin
 savehost := SMTPHost;
 saveport := SMTPPort;
 saveuser := SMTPUser;
 savepass := SMTPPasswd;
 savefrom := MailFrom;
 saveto   := MailTo;
 savessltls:=SMTPSSLTLS;
 try
 SMTPHost := smtp_host.Text;
 SMTPPort := smtp_port.Text;
 SMTPUser := smtp_user.Text;
 SMTPPasswd:= smtp_pass.Text;
 MailFrom := mail_from.Text;
 MailTo   := mail_to.Text;
 SMTPSSLTLS:=smtp_ssltls.Checked;
 subject:=rsTestEmailFro;
 txt:=rsTestEmailFro+CRLF+rsThisMessageC;
 r:=email(Subject,txt);
 if r='' then r:=rsEmailSentSuc;
 if pos('ssl3_get_record:wrong version number', r)>0 then r:=rsSSLTLSNotSup+crlf+r;
 ShowMessage(r);
 finally
   SMTPHost   := savehost;
   SMTPPort   := saveport;
   SMTPUser   := saveuser;
   SMTPPasswd := savepass;
   MailFrom   := savefrom;
   MailTo     := saveto;
   SMTPSSLTLS := savessltls;
 end;
end;

procedure Tf_option.ChangeAutofocusInPlace(Sender: TObject);
begin
  if (Autofocusmode.ItemIndex=3) then begin
    AutofocusInPlace.Checked:=true;
    AutofocusSlew.Checked:=false;
  end;
  AutofocusMultistar.Visible:=AutofocusInPlace.Checked;
  AutofocusSlewStar.Visible:=not AutofocusMultistar.Visible;
  PanelNearFocus.Visible:=AutofocusSlewStar.Visible;
  if AutofocusInPlace.Checked and (Autofocusmode.ItemIndex=1) then
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

procedure Tf_option.UseFileSequenceWidthClick(Sender: TObject);
begin
  FileSequenceWidth.Enabled:=UseFileSequenceWidth.Checked;
end;

procedure Tf_option.UseReadoutModeChange(Sender: TObject);
begin
  if ReadOutCapture.Items.Count=0 then UseReadoutMode.Checked:=false;
  ReadOutCapture.Enabled:=UseReadoutMode.Checked;
  ReadOutPreview.Enabled:=UseReadoutMode.Checked;
  ReadOutAstrometry.Enabled:=UseReadoutMode.Checked;
  ReadOutFocus.Enabled:=UseReadoutMode.Checked;
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

procedure Tf_option.SetAutofocusMode(value: TAutofocusMode);
begin
  case value of
  afVcurve    : Autofocusmode.ItemIndex:=1;
  afDynamic   : Autofocusmode.ItemIndex:=0;
  afIterative : Autofocusmode.ItemIndex:=2;
  afNone      : Autofocusmode.ItemIndex:=4;
  afPlanet    : Autofocusmode.ItemIndex:=3;
  end;
end;

function  Tf_option.GetAutofocusMode: TAutofocusMode;
begin
 case Autofocusmode.ItemIndex of
   0 : result:=afDynamic;
   1 : result:=afVcurve;
   2 : result:=afIterative;
   3 : result:=afPlanet;
   4 : result:=afNone;
 end;

end;

procedure Tf_option.AutofocusmodeClick(Sender: TObject);
begin
  AutofocusNotebook.PageIndex:=Autofocusmode.ItemIndex;
  PanelAutofocus.Visible:=(Autofocusmode.ItemIndex<4);
  PanelFocusStar.Visible:=PanelAutofocus.Visible;
  PanelNearFocus.Visible:=true;
  CheckFocuserDirection(Sender);
  ChangeAutofocusInPlace(Sender);
end;

procedure Tf_option.AstUseScriptClick(Sender: TObject);
begin
   AstCustScript.Visible:=AstUseScript.Checked;
end;

procedure Tf_option.ASTAPadvancedClick(Sender: TObject);
var cmd: string;
begin
 {$ifdef mswindows}
   cmd:=slash(ASTAPFolder.Directory)+'astap.exe -debug';
 {$else}
   cmd:=slash(ASTAPFolder.Directory)+'astap -debug';
 {$endif}
 ExecNoWait(cmd,'',false);
end;

procedure Tf_option.AutofocusExpTimeChange(Sender: TObject);
var x: double;
begin
  TComboBox(Sender).Text:=stringReplace(TComboBox(Sender).Text,',','.',[]);
  x:=StrToFloatDef(AutofocusExpTime.Text,-1);
  if x>0 then FAutofocusExposure:=x;
end;

procedure Tf_option.SetAutofocusExpTime(val: double);
begin
  FAutofocusExposure:=val;
  AutofocusExpTime.Text:=FormatFloat('0.####',val);
end;

procedure Tf_option.AutoguiderBoxClick(Sender: TObject);
var i: integer;
begin
  i:=GetAutoguiderType;
  Notebook3.PageIndex:=i;
  groupbox5.Visible:=(i<2)or(i=3)or(i=4);
  if (i<2)or(i=4) then begin
    Label23.Caption:=rsPixels;
    Label122.Caption:='';
  end
  else begin
    Label23.Caption:=rsPulseDuratio;
    Label122.Caption:=rsS;
  end;
  panel14.Visible:=(i<>2);
  GroupBox11.Visible:=(i=3);
  groupbox6.Visible:=(i=0)or(i=4);
  groupbox13.Visible:=(i=0);
  GroupBoxDrift.Visible:=(i=0);
  DitherRAonly.Visible:=(i=0)or(i=3)or(i=4);
  CalibrationDelay.Visible:=(i=0);
  label26.Visible:=(i=0);
  EarlyDither.Visible:=(i=0);
end;

procedure Tf_option.BtnDelHdrClick(Sender: TObject);
var aRow: integer;
begin
  aRow:=CustomHeader.Selection.Top;
  if (aRow>0)and(aRow<CustomHeader.RowCount) then begin
    CustomHeader.Cells[0,aRow]:='';
    CustomHeader.Cells[1,aRow]:='';
  end;
end;

function Tf_option.GetAutoguiderType: integer;
// agPHD,agLINGUIDER,agNONE,agDITHER,agINTERNAL
// PHD2 Internal Lin_Guider Dither only None
begin
 case AutoguiderBox.ItemIndex of
   0: result:=0; // PHD2
   1: result:=4; // Internal
   2: result:=1; // Linguider
   3: result:=3; // Dither
   4: result:=2; // None
 end;
end;

procedure Tf_option.SetAutoguiderType(value: integer);
begin
 case value of
   0: AutoguiderBox.ItemIndex:=0; // PHD2
   1: AutoguiderBox.ItemIndex:=2; // Linguider
   2: AutoguiderBox.ItemIndex:=4; // None
   3: AutoguiderBox.ItemIndex:=3; // Dither
   4: AutoguiderBox.ItemIndex:=1; // Internal
 end;
end;

procedure Tf_option.BtnDisableDelayClick(Sender: TObject);
begin
  FocuserDelay.Value:=0;
end;

procedure Tf_option.BtnDisableFocuserTempClick(Sender: TObject);
begin
  FocuserTempCoeff.Value:=0.0;
end;

procedure Tf_option.BtnDisableStarLostClick(Sender: TObject);
begin
  StarLostCancel.Value:=0;
  StarLostRestart.Value:=0;
  StarLostCancelExposure.Value:=0;
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

procedure Tf_option.BtnMaxDriftDisableClick(Sender: TObject);
begin
  GuideDriftMax.Value:=100;
end;

procedure Tf_option.ButtonLogDirClick(Sender: TObject);
begin
 SelectDirectoryDialog1.InitialDir:=LogDir.text;
 SelectDirectoryDialog1.FileName:=LogDir.text;
 if SelectDirectoryDialog1.Execute then LogDir.text:=SelectDirectoryDialog1.FileName;
end;

procedure Tf_option.ButtonSeqDirClick(Sender: TObject);
begin
 SelectDirectoryDialog1.InitialDir:=SeqDir.text;
 SelectDirectoryDialog1.FileName:=SeqDir.text;
 if SelectDirectoryDialog1.Execute then SeqDir.text:=SelectDirectoryDialog1.FileName;
end;

procedure Tf_option.DomeFlatPositionClick(Sender: TObject);
begin
  PanelFlatPositionAltAz.Visible:=(DomeFlatPosition.ItemIndex=0);
  if DomeFlatPosition.ItemIndex=1 then
    LabelFlatWarning.Caption:=rsWarningTheMo
  else
    LabelFlatWarning.Caption:='';
end;

procedure Tf_option.ButtonHelpClick(Sender: TObject);
begin
  if Assigned(FShowHelp) then FShowHelp(self);
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

procedure Tf_option.SlewPrecChange(Sender: TObject);
begin
   RecenterTargetDistance.MinValue:=1.5*SlewPrec.Value;
end;

procedure Tf_option.BtnShowPassClick(Sender: TObject);
begin
  if BtnShowPass.Down then
   smtp_pass.PasswordChar:=#0
  else
   smtp_pass.PasswordChar:='*';
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

procedure Tf_option.BalanceChange(Sender: TObject);
begin
  LabelR.Caption:='R '+FormatFloat(f2,RedBalance.Position/100);
  LabelG.Caption:='G '+FormatFloat(f2,GreenBalance.Position/100);
  LabelB.Caption:='B '+FormatFloat(f2,BlueBalance.Position/100);
end;

procedure Tf_option.ButtonNotificationNoneClick(Sender: TObject);
var i: integer;
begin
  for i:=0 to EmailCondition.Count-1 do begin
    EmailCondition.Checked[i]:=false;
  end;
end;

procedure Tf_option.ButtonNotificationAllClick(Sender: TObject);
var i: integer;
begin
  for i:=0 to EmailCondition.Count-1 do begin
    EmailCondition.Checked[i]:=true;
  end;
end;

procedure Tf_option.ButtonVoiceAllClick(Sender: TObject);
var i: integer;
begin
  for i:=0 to CheckGroupVoice.items.Count-1 do begin
    CheckGroupVoice.Checked[i]:=true;
  end;
end;

procedure Tf_option.ButtonVoiceNoneClick(Sender: TObject);
var i: integer;
begin
  for i:=0 to CheckGroupVoice.items.Count-1 do begin
    CheckGroupVoice.Checked[i]:=false;
  end;
end;

procedure Tf_option.ButtonVoiceTestClick(Sender: TObject);
begin
  LabelTestVoice.Caption:='';
  speak(rstest+' . '+format(rsNeedToWaitUn,['22:34:56']));
  if SPError<>0 then
    LabelTestVoice.Caption:=SPErrorMsg;
end;

end.

