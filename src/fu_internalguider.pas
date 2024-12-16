unit fu_internalguider;

{$mode objfpc}{$H+}

{
Copyright (C) 2022 Patrick Chevalley & Han Kleijn

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

uses   UScaleDPI, Dialogs, u_hints, u_translation, u_global, cu_camera, indiapi,
  Classes, SysUtils, FileUtil, Forms, Graphics, Controls, StdCtrls, ExtCtrls, Spin,
  math,LCLintf, ComCtrls, Buttons, Menus;


type
  trend_info=record ra,dec,racorr,deccorr : double; settling:boolean; end;//for internal guider
  xy_guiderlist =array of trend_info;

  dither_info=record raposition,decposition : double; end;//for internal guider
  dither_positionarray=array of dither_info;

type
  { Tf_internalguider }

  Tf_internalguider = class(TFrame)
    Backlash: TSpinEdit;
    BtnZoom05: TSpeedButton;
    BtnZoom1: TSpeedButton;
    BtnZoom2: TSpeedButton;
    BtnZoomAdjust: TSpeedButton;
    Button1: TButton;
    btnRefImage: TButton;
    ButtonLoop: TButton;
    ButtonSetLock: TSpeedButton;
    ButtonSetTemp: TButton;
    ButtonDark: TButton;
    ButtonCalibrate: TButton;
    ButtonGuide: TButton;
    ButtonStop: TButton;
    cbDrawSlit: TCheckBox;
    cbSpectro: TCheckBox;
    CalDate: TEdit;
    CalBinning: TEdit;
    CalRAspeed: TEdit;
    CalDECspeed: TEdit;
    CalDeclination: TEdit;
    CalIssue: TEdit;
    cbEnlargeImage: TCheckBox;
    CheckBoxBacklash: TCheckBox;
    CheckBoxTrackSolar1: TCheckBox;
    cbSlitList: TComboBox;
    disable_guiding1: TCheckBox;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    MenuItemAddSlit: TMenuItem;
    MenuItemDelSlit: TMenuItem;
    MenuSlitOffset: TButton;
    Panel12: TPanel;
    Panel13: TPanel;
    PanelAstrometryExposure: TPanel;
    PanelGuideStarOffset: TPanel;
    PopupMenuSlit: TPopupMenu;
    rgSpectroStrategy: TRadioGroup;
    Shape1: TShape;
    SlitOffsetX: TFloatSpinEdit;
    StarOffsetX: TFloatSpinEdit;
    SlitOffsetY: TFloatSpinEdit;
    framesize1: TComboBox;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Panel10: TPanel;
    GuideSpeedRA: TFloatSpinEdit;
    GuideSpeedDEC: TFloatSpinEdit;
    ForceGuideSpeed: TCheckBox;
    Cooler: TCheckBox;
    edOffsetX: TFloatSpinEdit;
    edOffsetY: TFloatSpinEdit;
    Exposure: TFloatSpinEdit;
    AstrometryExp: TFloatSpinEdit;
    GroupBox3: TGroupBox;
    GroupBoxSlit: TGroupBox;
    GroupBoxSearchArea: TGroupBox;
    GroupBoxLock: TGroupBox;
    GroupBoxOffset: TGroupBox;
    Label11: TLabel;
    Label20: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label30: TLabel;
    edRefX: TFloatSpinEdit;
    edRefY: TFloatSpinEdit;
    Label21: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label9: TLabel;
    LabelInfo4: TLabel;
    LabelInfo3: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    LabelInfo: TLabel;
    LabelInfo2: TLabel;
    LabelTemperature: TLabel;
    LabelStatusDec: TLabel;
    LabelStatusRA: TLabel;
    LongestPulse1: TSpinEdit;
    MenuItemDarkInfo: TMenuItem;
    MenuItemLoadDark: TMenuItem;
    GroupBox2: TGroupBox;
    Label10: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    LabelDark: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    label_estimate1: TLabel;
    measure_method2: TCheckBox;
    MenuItemClearDark: TMenuItem;
    MenuItemCaptureDark: TMenuItem;
    minHFD1: TFloatSpinEdit;
    minSNR1: TSpinEdit;
    pa1: TEdit;
    PageControl2: TPageControl;
    Panel11: TPanel;
    PanelSlitDrawing: TPanel;
    PanelImage: TPanel;
    PanelGuideSpeed: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PanelSpectro: TPanel;
    PanelTemperature: TPanel;
    PanelOffset: TPanel;
    PanelGain: TPanel;
    pier_side1: TEdit;
    PulseNorthDirection1: TEdit;
    PulseNotthDirection2: TEdit;
    pixelsize1: TEdit;
    PopupMenuDark: TPopupMenu;
    pulsegainEast1: TEdit;
    pulsegainNorth1: TEdit;
    pulsegainSouth1: TEdit;
    pulsegainWest1: TEdit;
    rgDitherMode: TRadioGroup;
    ra_hysteresis1: TSpinEdit;
    dec_hysteresis1: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    ra_gain1: TSpinEdit;
    scale1: TUpDown;
    Binning: TSpinEdit;
    Gain: TSpinEdit;
    Offset: TSpinEdit;
    edslitWinMin: TSpinEdit;
    edslitWinMax: TSpinEdit;
    InitialCalibrationStep: TSpinEdit;
    ShortestPulse1: TSpinEdit;
    StarOffsetY: TFloatSpinEdit;
    spSlitX: TSpinEdit;
    spSlitY: TSpinEdit;
    spSlitW: TSpinEdit;
    spSlitL: TSpinEdit;
    spSlitPA: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheetSpectro: TTabSheet;
    Temperature: TSpinEdit;
    TabSheetOptions: TTabSheet;
    TabSheetCamera: TTabSheet;
    Gamma: TTrackBar;
    Luminosity: TTrackBar;
    unitarcseconds1: TCheckBox;
    vpa_solar1: TFloatSpinEdit;
    v_solar1: TFloatSpinEdit;
    xy_trend1: TImage;
    xy_Panel1: TImage;
    Label3: TLabel;
    led: TShape;
    Panel2: TPanel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel3: TPanel;
    dec_gain1: TSpinEdit;
    TabSheetAdvanced: TTabSheet;
    TabSheetGuider: TTabSheet;
    Title: TLabel;
    procedure btnAddSlitOffsetClick(Sender: TObject);
    procedure btnDelSlitOffsetClick(Sender: TObject);
    procedure BtnZoom05Click(Sender: TObject);
    procedure BtnZoom1Click(Sender: TObject);
    procedure BtnZoom2Click(Sender: TObject);
    procedure BtnZoomAdjustClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnRefImageClick(Sender: TObject);
    procedure ButtonSetLockClick(Sender: TObject);
    procedure ButtonCalibrateClick(Sender: TObject);
    procedure ButtonDarkMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ButtonLoopClick(Sender: TObject);
    procedure ButtonGuideClick(Sender: TObject);
    procedure ButtonSetTempClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure cbDrawSlitChange(Sender: TObject);
    procedure cbSlitListChange(Sender: TObject);
    procedure cbSpectroChange(Sender: TObject);
    procedure cbEnlargeImageChange(Sender: TObject);
    procedure CheckBoxBacklashChange(Sender: TObject);
    procedure CheckBoxTrackSolar1Change(Sender: TObject);
    procedure CoolerClick(Sender: TObject);
    procedure dec_gain1Change(Sender: TObject);
    procedure dec_hysteresis1Change(Sender: TObject);
    procedure disable_guiding1Change(Sender: TObject);
    procedure ForceGuideSpeedChange(Sender: TObject);
    procedure ForceRedraw(Sender: TObject);
    procedure ExposureChange(Sender: TObject);
    procedure MenuSlitOffsetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LongestPulse1Change(Sender: TObject);
    procedure MenuItemCaptureDarkClick(Sender: TObject);
    procedure MenuItemClearDarkClick(Sender: TObject);
    procedure MenuItemDarkInfoClick(Sender: TObject);
    procedure MenuItemLoadDarkClick(Sender: TObject);
    procedure minHFD1Change(Sender: TObject);
    procedure minSNR1Change(Sender: TObject);
    procedure pa1Change(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure pier_side1Change(Sender: TObject);
    procedure pixelsize1Change(Sender: TObject);
    procedure pulsegainEast1Change(Sender: TObject);
    procedure pulsegainNorth1Change(Sender: TObject);
    procedure pulsegainSouth1Change(Sender: TObject);
    procedure pulsegainWest1Change(Sender: TObject);
    procedure ra_gain1Change(Sender: TObject);
    procedure ra_hysteresis1Change(Sender: TObject);
    procedure rgSpectroStrategySelectionChanged(Sender: TObject);
    procedure scale1Click(Sender: TObject; Button: TUDBtnType);
    procedure ShortestPulse1Change(Sender: TObject);
    procedure SlitOffsetChange(Sender: TObject);
    procedure vpa_solar1Change(Sender: TObject);
    procedure v_solar1Change(Sender: TObject);
  private
    { private declarations }
    thescale : double;
    FonStart, FonStop, FonCalibrate, FonCalibrateBacklash, FonLoop, FonRedraw,
    FonSpectroGuideChange,FonConfigureGuider,FonMeasureReferenceImage: TNotifyEvent;
    FonSetTemperature,FonSetCooler, FonCaptureDark, FonLoadDark, FonClearDark,FonDarkInfo, FonShowImage: TNotifyEvent;
    FonParameterChange: TNotifyStr;
    cur_minHFD,cur_minSNR,cur_Exposure,cur_vsolar,cur_vpasolar : double;
    cur_RAgain,cur_RA_hysteresis,cur_DECgain,cur_DEC_hysteresis,cur_LongestPulse,cur_shortestPulse: integer;
    cur_pa1,cur_pier_side1,cur_pixelsize1,cur_pulsegainEast1,cur_pulsegainNorth1,cur_pulsegainSouth1,
    cur_pulsegainWest1: string;
    cur_disable_guiding, cur_tracksolar, FForceMultiStar: boolean;
    FDrawSettingChange: boolean;
    FGuideLock, FGuideMultistar, FGuideAstrometry,FGuideStarOffset: boolean;
    FGuideLockNextX, FGuideLockNextY: integer;
    FonShowMessage: TNotifyMsg;
    Fcamera: T_camera;
    CurrentSlit: integer;

    procedure msg(txt:string; level: integer);
    procedure ShowMinMove;
    procedure SetLed (cl : tcolor);
    procedure SetRA_hysteresis(value:integer);
    function GetRA_hysteresis:integer;
    procedure SetDEC_hysteresis(value:integer);
    function GetDEC_hysteresis:integer;
    function GetRAgain:integer;
    procedure SetRAgain(value:integer);
    function GetDECgain:integer;
    procedure SetDECgain(value:integer);
    function GetpulsegainEastsetting:double;
    procedure SetpulsegainEastsetting(value:double);
    function GetpulsegainWestsetting:double;
    procedure SetpulsegainWestsetting(value:double);
    function GetpulsegainNorthsetting:double;
    procedure SetpulsegainNorthsetting(value:double);
    function GetpulsegainSouthsetting:double;
    procedure SetpulsegainSouthsetting(value:double);
    function Getpier_sidesetting:string;
    procedure Setpier_sidesetting(value:string);
    function GetPAsetting:double;
    procedure SetPixelSize(value:double);
    function GetPixelSize:double;

    procedure SetPulseDirectionNorth_1(value:string);
    function GetPulseDirectionNorth_1:string;
    procedure SetPulseDirectionNorth_2(value:string);
    function GetPulseDirectionNorth_2:string;

    procedure SetMinHFD(value:double);
    function GetMinHFD:double;
    procedure SetShortestPulse(value:integer);
    function GetShortestPulse:integer;
    procedure SetLongestPulse(value:integer);
    function GetLongestPulse:integer;
    procedure SetMinSNR(value:integer);
    function GetMinSNR:integer;
    procedure SetPAsetting(value:double);
    function Getdisableguiding: boolean;
    function GetUseArcSeconds: boolean;
    procedure SetUseArcSeconds(value:boolean);
    function GetScale:integer;
    procedure SetScale(value:integer);
    function GetFrameSize: integer;
    function GetSolartracking: Boolean;
    procedure SetSolarTracking(value: Boolean);
    procedure SetV_solar(value:double);
    function GetV_solar:double;
    procedure SetVPA_solar(value:double);
    function GetVPa_solar:double;
    procedure SetBacklash(value:integer);
    function GetBacklash:integer;
    procedure SetBacklashCompensation(value:Boolean);
    function GetBacklashCompensation:Boolean;
    procedure SetSpectro(value:Boolean);
    function GetSpectro:Boolean;
    procedure SetSpectroStrategy(value:TSpectroStrategy);
    function GetSpectroStrategy:TSpectroStrategy;
    procedure SetForceMultistar(value:Boolean);
    function GetForceMultistar:Boolean;
    function GetFGuideMultistar:Boolean;
    procedure SetGuideLock(value:Boolean);
    function GetGuideLock:Boolean;
    function GetSpectroAstrometry:Boolean;
    function GetSpectroAstrometryExposure:double;
    procedure SetSpectroAstrometryExposure(value:double);
    // slit reference position
    function GetRefX:double;
    procedure SetRefX(value:double);
    function GetRefY:double;
    procedure SetRefY(value:double);
    // slit current position including slit offset
    function GetSlitPosX:double;
    function GetSlitPosY:double;
    // guide position including slit and star offset
    function GetLockX:double;
    function GetLockY:double;
    function GetSearchWinMin:integer;
    procedure SetSearchWinMin(value:integer);
    function GetSearchWinMax:integer;
    procedure SetSearchWinMax(value:integer);
    function GetSlitX:integer;
    procedure SetSlitX(value:integer);
    function GetSlitY:integer;
    procedure SetSlitY(value:integer);
    function GetSlitW:integer;
    procedure SetSlitW(value:integer);
    function GetSlitL:integer;
    procedure SetSlitL(value:integer);
    function GetSlitPA:integer;
    procedure SetSlitPA(value:integer);
    function GetDrawSlit:Boolean;
    procedure SetDrawSlit(value:Boolean);
    function GetOffsetX:double;
    procedure SetOffsetX(value:double);
    function GetOffsetY:double;
    procedure SetOffsetY(value:double);
    procedure SetCameraStatus(status: string);
    function  GetCameraStatus: string;
    procedure SetInfo(status: string);
    function  GetInfo: string;
    procedure SetSpiralDither(value:Boolean);
    function GetSpiralDither:Boolean;

  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    procedure draw_xy(xy_trend :xy_guiderlist; dither_position :dither_positionarray);//draw XY points
    procedure draw_trend(xy_trend :xy_guiderlist);//draw trend
    procedure trend_message(message1,message2,message3 :string);//clear trend and place message
    function CalibrationIsValid(out detail: string): boolean;
    function Snapshot(exp: double; fn: string):boolean;
    function SaveFits(fn: string):boolean;
    procedure ClearSlitList;
    procedure ChangeSpectro;
    procedure ChangeSpectroStrategy;
    procedure CheckGuiderReferenceFile;
    property onShowMessage: TNotifyMsg read FonShowMessage write FonShowMessage;
    property Camera: T_camera read Fcamera write Fcamera;
    property onLoop: TNotifyEvent read FonLoop write FonLoop;
    property onStart: TNotifyEvent read FonStart write FonStart;
    property onStop: TNotifyEvent read FonStop write FonStop;
    property onSpectroGuideChange: TNotifyEvent read FonSpectroGuideChange write FonSpectroGuideChange;
    property onCalibrate: TNotifyEvent read FonCalibrate write FonCalibrate;
    property onCalibrateBacklash: TNotifyEvent read FonCalibrateBacklash write FonCalibrateBacklash;
    property onRedraw: TNotifyEvent read FonRedraw write FonRedraw;
    property onCaptureDark: TNotifyEvent read FonCaptureDark write FonCaptureDark;
    property onLoadDark: TNotifyEvent read FonLoadDark write FonLoadDark;
    property onClearDark: TNotifyEvent read FonClearDark write FonClearDark;
    property onDarkInfo: TNotifyEvent read FonDarkInfo write FonDarkInfo;
    property onParameterChange: TNotifyStr read FonParameterChange write FonParameterChange;
    property onSetTemperature: TNotifyEvent read FonSetTemperature write FonSetTemperature;
    property onSetCooler: TNotifyEvent read FonSetCooler write FonSetCooler;
    property onShowImage: TNotifyEvent read FonShowImage write FonShowImage;
    property RA_hysteresis: integer read GetRA_hysteresis write SetRA_hysteresis;
    property DEC_hysteresis: integer read GetDEC_hysteresis write SetDEC_hysteresis;
    property RAgain: integer read GetRAgain write SetRAgain;
    property DECgain: integer read GetDECgain write SetDECgain;
    property disable_guiding: boolean read Getdisableguiding;
    property use_arcsec: boolean read GetUseArcseconds write SetUseArcSeconds;

    property pulsegainEast: double read GetpulsegainEastsetting write SetpulsegainEastsetting; // movement in arcsec/second. Found by the calibration
    property pulsegainWest: double read GetpulsegainWestsetting write SetpulsegainWestsetting; // movement in arcsec/second. Found by the calibration
    property pulsegainNorth: double read GetpulsegainNorthsetting write SetpulsegainNorthsetting; // movement in arcsec/second. Found by the calibration
    property pulsegainSouth: double read GetpulsegainSouthsetting write SetpulsegainSouthsetting; // movement in arcsec/second. Found by the calibration
    property pixel_size: double read GetPixelSize write SetPixelSize; // scale in arcsec/pixel.
    property PulseNorthDirection_1: string read GetPulseDirectionNorth_1 write SetPulseDirectionNorth_1; // pulse direction at calibration side
    property PulseNorthDirection_2: string read GetPulseDirectionNorth_2 write SetPulseDirectionNorth_2; // pulse direction after flipping
    property minHFD: double read GetMinHFD write SetMinHFD;
    property ShortestPulse: integer read GetShortestPulse write SetShortestPulse; // minimum pulse duration. If below skip.
    property LongestPulse: integer read GetLongestPulse write SetLongestPulse;    // maximum pulse duration. Truncated if above.
    property MinSNR: integer read GetMinSNR write SetMinSNR;
    property pier_side: string read Getpier_sidesetting write Setpier_sidesetting; // movement in arcsec/second. Found by the calibration
    property PA : double read GetPAsetting write SetPAsetting;// Guider image orientation in radians. Found by the calibration
    property trend_scale: integer read Getscale write Setscale;
    property FrameSize: integer read GetFrameSize;
    property SolarTracking: Boolean read GetSolarTracking write SetSolarTracking;
    property v_solar: double read GetV_solar write SetV_solar;
    property vpa_solar: double read GetVPa_solar write SetVPA_solar;
    property DecBacklash: integer read GetBacklash write SetBacklash;
    property BacklashCompensation: Boolean read GetBacklashCompensation write SetBacklashCompensation;
    property SpectroFunctions: boolean read GetSpectro write SetSpectro; // single star slit guiding
    property SpectroStrategy:TSpectroStrategy read GetSpectroStrategy write SetSpectroStrategy;
    property GuideLock: boolean read GetGuideLock write SetGuideLock; // single star slit guiding
    property ForceGuideMultistar: boolean read GetFGuideMultistar; // force multistar after star lock
    property ForceMultistar: boolean read GetForceMultistar write SetForceMultistar; // multistar mode now required
    property SpectroAstrometry: boolean read GetSpectroAstrometry; // astrometry to find the target
    property SpectroAstrometryExposure: double read GetSpectroAstrometryExposure write SetSpectroAstrometryExposure; // exposure time for astrometry
    property GuideStarOffset: boolean read FGuideStarOffset;
    property RefX: double read GetRefX write SetRefX;    // slit reference position
    property RefY: double read GetRefY write SetRefY;
    property SlitPosX:double read GetSlitPosX;           // slit current position including slit offset
    property SlitPosY:double read GetSlitPosY;
    property LockX: double read GetLockX;                // guide position including slit and star offset
    property LockY: double read GetLockY;
    property OffsetX: double read GetOffsetX write SetOffsetX;  // guide offset relative to reference, used by dithering, solar tracking, spectro offset
    property OffsetY: double read GetOffsetY write SetOffsetY;
    property GuideLockNextX: integer read FGuideLockNextX write FGuideLockNextX;
    property GuideLockNextY: integer read FGuideLockNextY write FGuideLockNextY;
    property SearchWinMin: integer read GetSearchWinMin write SetSearchWinMin; // star search area
    property SearchWinMax: integer read GetSearchWinMax write SetSearchWinMax;
    property DrawSlit: boolean read GetDrawSlit write SetDrawSlit;
    property SlitX: integer read GetSlitX write SetSlitX; // slit position drawing
    property SlitY: integer read GetSlitY write SetSlitY;
    property SlitW: integer read GetSlitW write SetSlitW;
    property SlitL: integer read GetSlitL write SetSlitL;
    property SlitPA: integer read GetSlitPA write SetSlitPA;
    property DrawSettingChange: boolean read FDrawSettingChange write FDrawSettingChange;
    property Info: string read GetInfo write SetInfo;
    property CameraStatus: string read GetCameraStatus write SetCameraStatus;
    property SpiralDither: boolean read GetSpiralDither write SetSpiralDither;
    property onConfigureGuider: TNotifyEvent read FonConfigureGuider write FonConfigureGuider;
    property onMeasureReferenceImage: TNotifyEvent read FonMeasureReferenceImage write FonMeasureReferenceImage;

  end;

implementation

uses cu_mount;

{$R *.lfm}

{ Tf_internalguider }

constructor Tf_internalguider.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 {$endif}
 {$ifndef mswindows}
 Panel2.Color:=clDefault;
 Panel3.Color:=clDefault;
 Panel4.Color:=clDefault;
 Panel5.Color:=clDefault;
 Panel9.Color:=clDefault;
 PanelSpectro.Color:=clDefault;
 {$endif}
 PageControl1.ActivePageIndex:=0;
 ScaleDPI(Self);
 SetLang;
 LabelStatusRA.Caption:='';
 LabelStatusDec.Caption:='';
 LabelInfo.Caption:='';
 LabelInfo2.Caption:='';
 LabelInfo3.Caption:='';
 LabelInfo4.Caption:='';
 pier_side1.Text:='N/A';
 cur_minHFD:=minHFD;
 cur_minSNR:=minSNR;
 cur_pa1:=pa1.Text;
 cur_pier_side1:=pier_side1.Text;
 cur_pixelsize1:=pixelsize1.Text;
 cur_pulsegainEast1:=pulsegainEast1.Text;
 cur_pulsegainNorth1:=pulsegainNorth1.Text;
 cur_pulsegainSouth1:=pulsegainSouth1.Text;
 cur_pulsegainWest1:=pulsegainWest1.Text;
 cur_DECgain:=DECgain;
 cur_DEC_hysteresis:=DEC_hysteresis;
 cur_disable_guiding:=disable_guiding;
 cur_LongestPulse:=LongestPulse;
 cur_shortestPulse:=shortestPulse;
 cur_Exposure:=Exposure.value;
 cur_RAgain:=RAgain;
 cur_RA_hysteresis:=RA_hysteresis;
 cur_tracksolar:=SolarTracking;
 cur_vsolar:=v_solar1.Value;
 cur_vpasolar:=vpa_solar1.Value;
 FDrawSettingChange:=false;
 FGuideLockNextX:=-1;
 FGuideLockNextY:=-1;
 led.Canvas.AntialiasingMode:=amOn;
 FForceMultiStar:=false;

end;

destructor  Tf_internalguider.Destroy;
begin
 ClearSlitList;
 inherited Destroy;
end;


procedure Tf_internalguider.SetLang;
begin
  Title.Caption:=rsInternalGuid;
  TabSheetGuider.Caption:=rsGuider;
  ButtonGuide.Caption:=rsGuide;
  ButtonStop.Caption:=rsStop;
  ButtonCalibrate.Caption:=rsCalibrate;
  cbEnlargeImage.Caption:=rsEnlargeGuide;
  Label2.Caption:='α '+rsGain+':';
  Label3.Caption:='δ '+rsGain+':';
  Label1.Caption:=rsHysteresis+':';
  Label4.Caption:=rsHysteresis+':';
  TabSheetCamera.Caption:=rsCamera;
  ButtonLoop.Caption:=rsLoop;
  ButtonDark.Caption:=rsDark;
  MenuItemCaptureDark.Caption:=rsCreateFromCa;
  MenuItemLoadDark.Caption:=rsLoadDarkFile;
  MenuItemClearDark.Caption:=rsClearDarkFra;
  MenuItemDarkInfo.Caption:=rsViewHeader;
  Label10.Caption:=rsExposure;
  Label14.Caption:=rsBinning;
  Label15.Caption:=rsGain;
  Label16.Caption:=rsOffset2;
  Label21.Caption:=rsTemperature;
  ButtonSetTemp.Caption:=rsSet;
  Cooler.Caption:=rsCooler;
  Label17.Caption:=rsGamma;
  Label18.Caption:=rsLuminosity;
  Label19.Caption:=rsZoom;
  TabSheetOptions.Caption:=rsOptions2;
  TabSheetAdvanced.Caption:=rsAdvanced;
  GroupBox2.Caption:=rsCalibrationR;
  Label5.Caption:=rsCameraAngle+' [°]';
  Label6.Caption:=rsPulseGain+blank+rsEast+' [px*cos(δ)/sec]';
  Label7.Caption:=rsPulseGain+blank+rsWest+' [px*cos(δ)/sec]';
  Label8.Caption:=rsPulseGain+blank+rsNorth+' [px/sec]';
  Label13.Caption:=rsPulseGain+blank+rsSouth+' [px/sec]';
  Label12.Caption:=rsMeasuredAtEW;
  label_estimate1.Caption:=rsPixelScale+' ["/px]';
  Label22.Caption:=rsShortestGuid+' [ms]';
  label11.Caption:=rsLongestGuide+' [ms]';
  Label20.Caption:=rsMinimumMove;
  Label23.Caption:=rsMinimum2+' HFD';
  Label24.Caption:=rsMinimum2+' SNR';
  disable_guiding1.Caption:=rsDisableGuidi;
  Label9.Caption:=rsFrameSize;
  framesize1.Items[0]:=rsMax2;
  CheckBoxTrackSolar1.Caption:=rsActivateSola;
  Label25.Caption:=rsApparentMoti+' ["/min]';
  Label26.Caption:=rsApparentMoti2+' [°]';
  CheckBoxBacklash.Caption:=rsUseBacklashC;
  Label27.Caption:=rsDeclinationB;
  rgDitherMode.Caption:=rsDitherMode;
  rgDitherMode.Items[0]:=rsSpiral;
  rgDitherMode.Items[1]:=rsRandom;
  TabSheetSpectro.Caption:=rsSpectroscopy;
  cbSpectro.Caption:=rsActivateSpec;
  rgSpectroStrategy.Caption:=rsCenteringAnd;
  rgSpectroStrategy.Items[0]:=rsSingleStarUs;
  rgSpectroStrategy.Items[1]:=rsSingleStarBr;
  rgSpectroStrategy.Items[2]:=rsSingleStarWi;
  rgSpectroStrategy.Items[3]:=rsSingleStarWi2;
  rgSpectroStrategy.Items[4]:=rsMultistarWit;
  GroupBoxLock.Caption:=rsGuidePositio;
  label28.Caption:='X';
  label29.Caption:='Y';
  ButtonSetLock.Caption:=rsClickOnImage;
  Label54.Caption:=rsPerSlitOffse;
  Label53.Caption:=rsSlit;
  Label51.Caption:=rsOffset+' X';
  Label52.Caption:=rsOffset+' Y';
  Label55.Caption:=rsGuideStarOff;
  Label56.Caption:=rsOffset+' X';
  Label57.Caption:=rsOffset+' Y';
  MenuSlitOffset.Caption:=rsManage;
  MenuItemAddSlit.Caption:=rsAdd;
  MenuItemDelSlit.Caption:=rsDelete;
  label39.Caption:=rsAstrometryEx;
  GroupBoxOffset.Caption:=rsMultiStarGui;
  Label37.Caption:=rsGuideOffset+' X';
  Label38.Caption:=rsGuideOffset+' Y';
  GroupBoxSearchArea.Caption:=rsStarSearchAr;
  Label30.Caption:=rsGuideBoxSize;
  Label31.Caption:=rsMaximumSearc2;
  GroupBoxSlit.Caption:=rsSlitDrawing;
  cbDrawSlit.Caption:=rsDrawSlitArea;
  label32.Caption:=rsCenter+' X';
  label33.Caption:=rsCenter+' Y';
  label34.Caption:=rsWidth;
  label35.Caption:=rsLength;
  label36.Caption:=rsRotation;
  GroupBox3.Caption:=rsCalibrationO;
  label40.Caption:=rsInitialCalib+' [ms]';
  ForceGuideSpeed.Caption:=rsSetGuideSpee;
  Label41.Caption:=rsRA;
  Label42.Caption:=rsDec;
  measure_method2.Caption:=rsMeasurePixel;
  Label47.Caption:=rsLastCalibrat;
  Label43.Caption:=rsDate;
  Label44.Caption:=rsBinning;
  Label45.Caption:=rsRA+' '+rsSpeed;
  Label46.Caption:=rsDec+' '+rsSpeed;
  Label49.Caption:=rsDeclination;
  Label50.Caption:=rsIssue;
end;

function Tf_internalguider.Getdisableguiding:boolean;
begin
  result:=disable_guiding1.checked;
end;

function Tf_internalguider.GetUseArcSeconds:boolean;
begin
  result:=unitarcseconds1.checked;
end;

procedure Tf_internalguider.SetUseArcSeconds(value:boolean);
begin
  unitarcseconds1.checked:=value;
end;

function Tf_internalguider.GetRA_hysteresis:integer;
begin
  result:=ra_hysteresis1.Value;
end;

procedure Tf_internalguider.SetRA_hysteresis(value:integer);
begin
  ra_hysteresis1.Value:=value;
end;

function Tf_internalguider.GetDec_hysteresis:integer;
begin
  result:=dec_hysteresis1.Value;
end;

procedure Tf_internalguider.SetDec_hysteresis(value:integer);
begin
  dec_hysteresis1.Value:=value;
end;

function Tf_internalguider.GetpulsegainEASTsetting:double;
begin
  result:=strtofloat(pulsegainEAST1.text);
end;

procedure Tf_internalguider.SetpulsegainEastsetting(value:double);
begin
  pulsegainEAST1.text:=FormatFloat(f2v,value);
end;

function Tf_internalguider.GetpulsegainWestsetting:double;
begin
  result:=strtofloat(pulsegainWest1.text);
end;

procedure Tf_internalguider.SetpulsegainWestsetting(value:double);
begin
  pulsegainWest1.text:=FormatFloat(f2v,value);
end;

function Tf_internalguider.GetpulsegainNorthsetting:double;
begin
  result:=strtofloat(pulsegainNorth1.text);
end;

procedure Tf_internalguider.SetpulsegainNorthsetting(value:double);
begin
  pulsegainNorth1.text:=FormatFloat(f2v,value);
end;

function Tf_internalguider.GetpulsegainSouthsetting:double;
begin
  result:=strtofloat(pulsegainSouth1.text);
end;

procedure Tf_internalguider.SetpulsegainSouthsetting(value:double);
begin
  pulsegainSouth1.text:=FormatFloat(f2v,value);
end;

function Tf_internalguider.Getpier_sidesetting:string;
begin
  result:=pier_side1.text;
end;

procedure Tf_internalguider.SetPixelSize(value:double);
begin
  pixelsize1.text:=FormatFloat(f4v,value);
end;

function Tf_internalguider.GetPixelSize:double;
begin
  result:=strtofloat(pixelsize1.text);
end;

procedure Tf_internalguider.SetPulseDirectionNorth_1(value:string);
begin
  PulseNorthDirection1.text:=value;
end;

function Tf_internalguider.GetPulseDirectionNorth_1:string;
begin
  result:=PulseNorthDirection1.text;
end;

procedure Tf_internalguider.SetPulseDirectionNorth_2(value:string);
begin
  PulseNotthDirection2.text:=value;
end;

function Tf_internalguider.GetPulseDirectionNorth_2:string;
begin
  result:=PulseNotthDirection2.text;
end;


procedure Tf_internalguider.SetMinHFD(value:double);
begin
  MinHFD1.value:=value;
end;

function Tf_internalguider.GetMinHFD:double;
begin
  result:=MinHFD1.value;
end;


procedure Tf_internalguider.SetShortestPulse(value:integer);
begin
  ShortestPulse1.value:=value;
end;

function Tf_internalguider.GetShortestPulse:integer;
begin
  result:=ShortestPulse1.value;
end;

procedure Tf_internalguider.SetLongestPulse(value:integer);
begin
  LongestPulse1.value:=value;
end;

function Tf_internalguider.GetLongestPulse:integer;
begin
  result:=LongestPulse1.value;
end;

procedure Tf_internalguider.SetMinSNR(value:integer);
begin
  MinSNR1.value:=value;
end;

function Tf_internalguider.GetMinSNR:integer;
begin
  result:=MinSNR1.value;
end;

procedure Tf_internalguider.Setpier_sidesetting(value:string);
begin
  pier_side1.text:=value;
end;

function Tf_internalguider.GetPAsetting:double;
begin
  result:=strtofloat(PA1.text);
end;

procedure Tf_internalguider.SetPAsetting(value:double);
begin
  PA1.text:=FormatFloat(f1v,value);
end;

function Tf_internalguider.GetRAgain:integer;
begin
  result:=ra_gain1.Value;
end;

procedure Tf_internalguider.SetRAgain(value:integer);
begin
  ra_gain1.Value:=value;
end;

function Tf_internalguider.GetDECgain:integer;
begin
  result:=dec_gain1.Value;
end;

procedure Tf_internalguider.SetDECgain(value:integer);
begin
  dec_gain1.Value:=value;
end;

function Tf_internalguider.GetScale:integer;
begin
  result:=scale1.position;
end;

procedure Tf_internalguider.SetV_solar(value:double);
begin
  v_solar1.Value:=value;
end;

function Tf_internalguider.GetV_solar:double;
begin
  result:=v_solar1.Value;
end;

procedure Tf_internalguider.SetVPA_solar(value:double);
begin
  vpa_solar1.Value:=value;
end;

function Tf_internalguider.GetVPa_solar:double;
begin
  result:=vpa_solar1.Value;
end;

procedure Tf_internalguider.SetBacklash(value:integer);
begin
   Backlash.Value:=value;
end;

function Tf_internalguider.GetBacklash:integer;
begin
  result:=Backlash.Value;
end;

procedure Tf_internalguider.SetBacklashCompensation(value:Boolean);
begin
  CheckBoxBacklash.Checked:=value;
end;

function Tf_internalguider.GetBacklashCompensation:Boolean;
begin
  result:=CheckBoxBacklash.Checked;
end;

procedure Tf_internalguider.SetScale(value:integer);
begin
  scale1.position:=value;
  case value of 6: thescale:=0.25;
                5: thescale:=0.5;
                4: thescale:=1.0;
                3: thescale:=2.0;
                2: thescale:=4.0;
                1: thescale:=8.0;
                0: thescale:=16.0;
  end;

 draw_xy(nil,nil);//plot xy values
 draw_trend(nil);// plot trends

end;

procedure Tf_internalguider.SetLed(cl: tcolor);
begin
  led.Brush.Color:=cl;
end;

procedure Tf_internalguider.ButtonGuideClick(Sender: TObject);
begin
  setled(clgreen);
  if Assigned(FonStart) then FonStart(self);
end;

procedure Tf_internalguider.ButtonStopClick(Sender: TObject);
begin
  if Assigned(FonStop) then FonStop(self);
  setled(clGray);
end;

procedure Tf_internalguider.cbDrawSlitChange(Sender: TObject);
begin
  PanelSlitDrawing.Visible:=cbDrawSlit.Checked;
  ForceRedraw(nil);
end;

procedure Tf_internalguider.ChangeSpectro;
begin
  cbSpectroChange(nil);
end;

procedure Tf_internalguider.rgSpectroStrategySelectionChanged(Sender: TObject);
begin
  case rgSpectroStrategy.ItemIndex of
    0: begin // Single star, use the brightest star
        FGuideLock:=true;
        FGuideMultistar:=false;
        FGuideAstrometry:=false;
        FGuideStarOffset:=false;
        StarOffsetX.Value:=0;
        StarOffsetY.Value:=0;
        PageControl2.ActivePageIndex:=0;
        PanelAstrometryExposure.Visible:=false;
        PanelGuideStarOffset.Visible:=false;
       end;
    1: begin // Single star, use the brightest star, with offset
        FGuideLock:=true;
        FGuideMultistar:=false;
        FGuideAstrometry:=false;
        FGuideStarOffset:=true;
        PageControl2.ActivePageIndex:=0;
        PanelAstrometryExposure.Visible:=false;
        PanelGuideStarOffset.Visible:=true;
       end;
    2: begin // Single star, with guider astrometry
        FGuideLock:=true;
        FGuideMultistar:=false;
        FGuideAstrometry:=true;
        FGuideStarOffset:=false;
        StarOffsetX.Value:=0;
        StarOffsetY.Value:=0;
        PageControl2.ActivePageIndex:=0;
        PanelAstrometryExposure.Visible:=true;
        PanelGuideStarOffset.Visible:=false;
       end;
    3: begin // Single star with astrometry, then multistar
        FGuideLock:=true;
        FGuideMultistar:=true;
        FGuideAstrometry:=true;
        FGuideStarOffset:=false;
        StarOffsetX.Value:=0;
        StarOffsetY.Value:=0;
        PageControl2.ActivePageIndex:=0;
        PanelAstrometryExposure.Visible:=true;
        PanelGuideStarOffset.Visible:=false;
       end;
    4: begin // Multistar with guider astrometry
        FGuideLock:=false;
        FGuideMultistar:=false;
        FGuideAstrometry:=true;
        FGuideStarOffset:=false;
        StarOffsetX.Value:=0;
        StarOffsetY.Value:=0;
        PageControl2.ActivePageIndex:=1;
        PanelAstrometryExposure.Visible:=true;
        PanelGuideStarOffset.Visible:=false;
       end;
  end;
  FDrawSettingChange:=true;
  if Assigned(FonSpectroGuideChange) then FonSpectroGuideChange(self);
end;

procedure Tf_internalguider.ChangeSpectroStrategy;
begin
  rgSpectroStrategySelectionChanged(nil);
end;

procedure Tf_internalguider.cbSpectroChange(Sender: TObject);
begin
  PanelSpectro.Enabled:=cbSpectro.Checked;
end;


procedure Tf_internalguider.cbEnlargeImageChange(Sender: TObject);
begin
  if assigned(FonShowImage) then FonShowImage(self);
end;

procedure Tf_internalguider.CheckBoxBacklashChange(Sender: TObject);
begin
  Backlash.Enabled:=CheckBoxBacklash.Checked;
end;

procedure Tf_internalguider.CheckBoxTrackSolar1Change(Sender: TObject);
begin
  v_solar1.Enabled:=CheckBoxTrackSolar1.checked;
  vpa_solar1.enabled:=CheckBoxTrackSolar1.checked;
  if (cur_tracksolar<>SolarTracking) and Assigned(FonParameterChange) then FonParameterChange('SolarTracking = '+BoolToStr(SolarTracking,'true','false'));
  cur_tracksolar:=SolarTracking;
end;

procedure Tf_internalguider.ButtonSetTempClick(Sender: TObject);
begin
  if Assigned(FonSetTemperature) then FonSetTemperature(self);
end;

procedure Tf_internalguider.CoolerClick(Sender: TObject);
begin
  if Assigned(FonSetCooler) then FonSetCooler(self);
end;

procedure Tf_internalguider.ButtonDarkMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var p: TPoint;
begin
  p.x:=0;
  p.y:=ButtonDark.Height;
  p:=ButtonDark.ClientToScreen(p);
  PopupMenuDark.PopUp(p.x,p.y);
end;

procedure Tf_internalguider.MenuItemCaptureDarkClick(Sender: TObject);
begin
  if Assigned(FonCaptureDark) then FonCaptureDark(self);
end;

procedure Tf_internalguider.MenuItemLoadDarkClick(Sender: TObject);
begin
  if Assigned(FonLoadDark) then FonLoadDark(self);
end;

procedure Tf_internalguider.minHFD1Change(Sender: TObject);
begin
  if (cur_minHFD<>minHFD) and Assigned(FonParameterChange) then FonParameterChange('Minimum HFD setting = '+FormatFloat(f2,minHFD));
  cur_minHFD:=minHFD;
end;

procedure Tf_internalguider.minSNR1Change(Sender: TObject);
begin
  if (cur_minSNR<>minSNR) and Assigned(FonParameterChange) then FonParameterChange('Minimum SNR setting = '+FormatFloat(f2,minSNR));
  cur_minSNR:=minSNR;
end;

procedure Tf_internalguider.pa1Change(Sender: TObject);
begin
  if (cur_pa1<>pa1.Text) and Assigned(FonParameterChange) then FonParameterChange('Camera angle = '+pa1.Text);
  cur_pa1:=pa1.Text;
end;

procedure Tf_internalguider.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePageIndex=1 then
    PanelImage.Parent:=Panel4  // page camera
  else
    PanelImage.Parent:=Panel3; // page guider
end;

procedure Tf_internalguider.pier_side1Change(Sender: TObject);
begin
  if (cur_pier_side1<>pier_side1.Text) and Assigned(FonParameterChange) then FonParameterChange('Measured at = '+pier_side1.Text);
  cur_pier_side1:=pier_side1.Text;
end;

procedure Tf_internalguider.pixelsize1Change(Sender: TObject);
begin
  if (cur_pixelsize1<>pixelsize1.Text) and Assigned(FonParameterChange) then FonParameterChange('Pixel scale = '+pixelsize1.Text);
  ShowMinMove;
  cur_pixelsize1:=pixelsize1.Text;
end;

procedure Tf_internalguider.pulsegainEast1Change(Sender: TObject);
begin
  if (cur_pulsegainEast1<>pulsegainEast1.Text) and Assigned(FonParameterChange) then FonParameterChange('Pulse gain East = '+pulsegainEast1.Text);
  ShowMinMove;
  cur_pulsegainEast1:=pulsegainEast1.Text;
end;

procedure Tf_internalguider.pulsegainNorth1Change(Sender: TObject);
begin
  if (cur_pulsegainNorth1<>pulsegainNorth1.Text) and Assigned(FonParameterChange) then FonParameterChange('Pulse gain North = '+pulsegainNorth1.Text);
  cur_pulsegainNorth1:=pulsegainNorth1.Text;
end;

procedure Tf_internalguider.pulsegainSouth1Change(Sender: TObject);
begin
  if (cur_pulsegainSouth1<>pulsegainSouth1.Text) and Assigned(FonParameterChange) then FonParameterChange('Pulse gain South = '+pulsegainSouth1.Text);
  cur_pulsegainSouth1:=pulsegainSouth1.Text;
end;

procedure Tf_internalguider.pulsegainWest1Change(Sender: TObject);
begin
  if (cur_pulsegainWest1<>pulsegainWest1.Text) and Assigned(FonParameterChange) then FonParameterChange('Pulse gain West = '+pulsegainWest1.Text);
  cur_pulsegainWest1:=pulsegainWest1.Text;
end;

procedure Tf_internalguider.dec_gain1Change(Sender: TObject);
begin
  if (cur_DECgain<>DECgain) and Assigned(FonParameterChange) then FonParameterChange('DEC Gain = '+IntToStr(DECgain));
  cur_DECgain:=DECgain;
end;

procedure Tf_internalguider.dec_hysteresis1Change(Sender: TObject);
begin
  if (cur_DEC_hysteresis<>DEC_hysteresis) and Assigned(FonParameterChange) then FonParameterChange('DEC Hyst = '+IntToStr(DEC_hysteresis));
  cur_DEC_hysteresis:=DEC_hysteresis;
end;

procedure Tf_internalguider.disable_guiding1Change(Sender: TObject);
begin
  if (cur_disable_guiding<>disable_guiding) and Assigned(FonParameterChange) then FonParameterChange('MountGuidingEnabled = '+BoolToStr(not disable_guiding,'true','false'));
  cur_disable_guiding:=disable_guiding;
end;

procedure Tf_internalguider.ForceGuideSpeedChange(Sender: TObject);
begin
  PanelGuideSpeed.Enabled:=ForceGuideSpeed.Checked;
end;

procedure Tf_internalguider.ForceRedraw(Sender: TObject);
begin
  FDrawSettingChange:=true;
  if Assigned(FonRedraw) then FonRedraw(self);
end;

procedure Tf_internalguider.LongestPulse1Change(Sender: TObject);
begin
  if (cur_LongestPulse<>LongestPulse) and Assigned(FonParameterChange) then FonParameterChange('Max RA duration = '+IntToStr(LongestPulse)+', Max DEC duration = '+IntToStr(LongestPulse));
  cur_LongestPulse:=LongestPulse;
end;

procedure Tf_internalguider.ShortestPulse1Change(Sender: TObject);
begin
 ShowMinMove;
 if (cur_shortestPulse<>shortestPulse) and Assigned(FonParameterChange) then FonParameterChange('Shortest guide pulse setting = '+IntToStr(shortestPulse));
 cur_shortestPulse:=shortestPulse;
end;

procedure Tf_internalguider.vpa_solar1Change(Sender: TObject);
begin
 if (cur_vpasolar<>vpa_solar1.Value) and Assigned(FonParameterChange) then FonParameterChange('Solar tracking PA = '+vpa_solar1.Text);
 cur_vpasolar:=vpa_solar1.Value;
end;

procedure Tf_internalguider.v_solar1Change(Sender: TObject);
begin
 if (cur_vsolar<>v_solar1.Value) and Assigned(FonParameterChange) then FonParameterChange('Solar tracking rate = '+v_solar1.Text);
 cur_vsolar:=v_solar1.Value;
end;

procedure Tf_internalguider.ExposureChange(Sender: TObject);
begin
  if (cur_Exposure<>Exposure.value) and Assigned(FonParameterChange) then FonParameterChange('Exposure = '+FormatFloat(f0,Exposure.value*1000)+' ms');
  cur_Exposure:=Exposure.value;
end;

procedure Tf_internalguider.ra_gain1Change(Sender: TObject);
begin
  if (cur_RAgain<>RAgain) and Assigned(FonParameterChange) then FonParameterChange('RA Gain = '+IntToStr(RAgain));
  cur_RAgain:=RAgain;
end;

procedure Tf_internalguider.ra_hysteresis1Change(Sender: TObject);
begin
  if (cur_RA_hysteresis<>RA_hysteresis) and Assigned(FonParameterChange) then FonParameterChange('RA Hyst = '+IntToStr(RA_hysteresis));
  cur_RA_hysteresis:=RA_hysteresis;
end;

procedure Tf_internalguider.MenuItemClearDarkClick(Sender: TObject);
begin
  if Assigned(FonClearDark) then FonClearDark(self);
end;

procedure Tf_internalguider.MenuItemDarkInfoClick(Sender: TObject);
begin
  if Assigned(FonDarkInfo) then FonDarkInfo(self);
end;

procedure Tf_internalguider.scale1Click(Sender: TObject; Button: TUDBtnType);
begin
  trend_scale:=scale1.position;//trigger a redraw trend
end;

procedure Tf_internalguider.ShowMinMove;
var px,asec: double;
begin
  px:=ShortestPulse*pulsegainEast/1000;
  asec:=px*pixel_size;
  label20.Caption:=rsMinimumMove+': '+FormatFloat(f2, px)+' '+rsPixel+', '+FormatFloat(f2, asec)+' "';
end;

procedure Tf_internalguider.ButtonCalibrateClick(Sender: TObject);
var txt: string;
    n: integer;
begin
 txt:= rsSelectACalib+#10+#10+rsOption1Calib+#10+#10+rsOption3Backl+#10+#10+rsOption4Cance;
 {$ifdef lclgtk2}
 // inverted button with GTK2
    n:=QuestionDlg (rsGuiderCalibr, txt, mtCustom,
          [23,rsCancel,'IsCancel',  22, rsBacklashCali, 20,rsCalibration],
          '');
 {$else}
   n:=QuestionDlg (rsGuiderCalibr, txt, mtCustom,
         [20,rsCalibration, 22, rsBacklashCali, 23,rsCancel,'IsCancel'],
         '');
 {$endif}
 case n of
      20:begin
         // ReverseDec:=false;
          setled(clYellow);
          if Assigned(FonCalibrate) then FonCalibrate(self);
         end;
      22:begin
          setled(clYellow);
          if Assigned(FonCalibrateBacklash) then FonCalibrateBacklash(self);
         end;

      23:exit;
   end;
end;

procedure Tf_internalguider.ButtonLoopClick(Sender: TObject);
begin
  if Assigned(FonLoop) then FonLoop(self);
end;




procedure Tf_internalguider.draw_xy(xy_trend :xy_guiderlist; dither_position :dither_positionarray);
var
 i,j,w2,h2,diameter, lenb,x,y,counter :integer;
 rms_ra,rms_dec,mean_ra,mean_dec,scale,scale2 :double;
 scaleunit : string;
 firstplot: boolean;
const
    len=2;
   procedure draw_plussign(canvas :tcanvas; x,y :integer);
   begin
     canvas.MoveTo(x-2,y);
     canvas.LineTo(x+2,y);
     canvas.MoveTo(x,y-2);
     canvas.LineTo(x,y+2);
   end;

begin
  with xy_panel1 do
  begin
    w2:= width div 2;
    h2:= height div 2;

    {clear}
    Canvas.Pen.width :=1;{thickness lines}
    Canvas.pen.color:=colorLightGray;
    canvas.font.color:=colorText;
    canvas.brush.color:=colorBg;
    canvas.rectangle(0,0, width,height);
    canvas.Brush.Style:=bsClear;{transparent style}
    canvas.moveto(0,h2);{cross}
    canvas.lineto(width,h2);{cross}

    canvas.moveto(w2,0);{cross}
    canvas.lineto(w2,height);

    diameter:=round(20);
    canvas.Ellipse(w2-diameter, h2-diameter, w2+diameter, h2+diameter);
    diameter:=round(40);
    canvas.Ellipse(w2-diameter, h2-diameter, w2+diameter, h2+diameter);
    diameter:=round(60);
    canvas.Ellipse(w2-diameter, h2-diameter, w2+diameter, h2+diameter);


    if ((use_arcsec=false) or (pixel_size=0)) then
    begin
      scale:=1/thescale;
      scale2:=1;
      scaleunit:=' px';
    end
    else
    begin
      scale:=pixel_size/thescale;
      scale2:=pixel_size;
      scaleunit:=' "';
   end;
   canvas.textout(w2,h2+20-10,FormatFloat(f1v,2*scale));
   canvas.textout(w2,h2+40-10,FormatFloat(f1v,4*scale));
   canvas.textout(w2,h2+60-10,FormatFloat(f1v,6*scale)+scaleunit);

   //draw xy graph
   lenb:=length(xy_trend)-1;
   counter:=0;
   rms_ra:=0;
   mean_ra:=0;
   mean_dec:=0;
   firstplot:=true;
   for i:=lenb downto 0 do
   begin
     if ((xy_trend[i].ra<1E99) and (xy_trend[i].settling=false)) then //valid trend data
     begin
       x:=w2+round(xy_trend[i].ra*10*thescale) ;
       y:=h2-round(xy_trend[i].dec*10*thescale);

       x:=min(max(0,x),width);
       y:=min(max(0,y),height);
       if i<6 then Canvas.pen.color:=colorRed else  Canvas.pen.color:=colorGray;

       if firstplot then canvas.moveto(x,y) else canvas.lineto(x,y);
       canvas.Ellipse(x-len,y-len,x+1+len,y+1+len);{circle, the y+1,x+1 are essential to center the circle(ellipse) at the middle of a pixel. Otherwise center is 0.5,0.5 pixel wrong in x, y}
       mean_ra:=mean_ra+xy_trend[i].ra;
       mean_dec:=mean_dec+xy_trend[i].dec;
       inc(counter);
       firstplot:=false;
     end;
   end;

   if counter>0 then // calculate the RMS error of the variation around the mean value;
    begin // report the RMS values
      mean_ra:=mean_ra/counter;//mean or average value
      mean_dec:=mean_dec/counter;
      rms_ra:=0;
      rms_dec:=0;
      for i:=0 to counter-1 do
      begin
        rms_ra:=rms_ra+sqr(xy_trend[i].ra-mean_ra);
        rms_dec:=rms_dec+sqr(xy_trend[i].dec-mean_dec);
      end;
      rms_ra:=sqrt(rms_ra/counter);
      rms_dec:=sqrt(rms_dec/counter);

      canvas.textout(1,h2,'α  '+FormatFloat(f1v,scale2*rms_ra));
      canvas.textout(w2+2,2,'δ  '+FormatFloat(f1v,scale2*rms_dec));
    end;

    //show dithering
    Canvas.pen.color:=colorGreen;
    for i:=length(dither_position)-1 downto 0 do
    begin
      if dither_position[i].raposition<1E99 then //valid data
      begin
        x:=w2+round(dither_position[i].raposition*10*thescale) ;
        y:=h2-round(dither_position[i].decposition*10*thescale);

        x:=min(max(0,x),width);
        y:=min(max(0,y),height);
        draw_plussign(canvas,x,y);
      end;
    end;
  end;//xy panel
end;

procedure Tf_internalguider.trend_message(message1,message2,message3 :string);//clear trend and place message
begin
 with xy_trend1 do
 begin
   canvas.brush.color:=colorBg;
   canvas.rectangle(0,0, width,height);
   canvas.font.color:=colorText;
   canvas.textout(5,10,message1);
   canvas.textout(5,35,message2);
   canvas.textout(5,60,message3);
 end;
end;


procedure Tf_internalguider.draw_trend(xy_trend :xy_guiderlist);//draw trend.
var
 i, h2, lenb, x, y, counter :integer;
 scale: double;
 scaleunit : string;

begin
 with xy_trend1 do
 begin
   h2:= height div 2;

   {clear}
   Canvas.pen.color:=colorLightGray;
   canvas.font.color:=colorText;
   canvas.brush.color:=colorBg;
   canvas.rectangle(0,0, width,height);
   canvas.Brush.Style:=bsClear;{transparent style}


   //mark dither phase as grey background
   Canvas.pen.color:= colorLightGray;
   Canvas.Pen.width :=6;{thickness lines}
   Canvas.pen.style:=psSolid;
   lenb:=length(xy_trend)-1;
   counter:=lenb;
   for i:=lenb downto 0  do
   begin
     if xy_trend[i].ra<1E99 then //valid data
     begin
       x:=width-counter*((width-5) div lenb)-15;
       dec(counter);

       if xy_trend[i].settling then
       begin
         canvas.moveto(x-2,0);
         canvas.lineto(x-2,height);
       end;
     end;
   end;


   Canvas.Pen.width :=1;{thickness lines}

   for i:=-2 to 2 do
   begin
     canvas.moveto(0,h2+I*20);{lines}
     canvas.lineto(width,h2+i*20);{lines}
   end;

   if ((use_arcsec=false) or (pixel_size=0)) then
   begin
      scale:=1/thescale;
      scaleunit:=' px';
   end
   else
   begin
      scale:=pixel_size/thescale;
      scaleunit:=' "';
   end;
   canvas.textout(0,h2+20-10,FormatFloat(f1v,-2*scale));
   canvas.textout(0,h2+40-10,FormatFloat(f1v,-4*scale));
   canvas.textout(0,h2-20-10,'+'+FormatFloat(f1v,+2*scale));
   canvas.textout(0,h2-40-10,'+'+FormatFloat(f1v,+4*scale)+scaleunit);



   Canvas.Pen.width :=2;{thickness lines}
   lenb:=length(xy_trend)-1;
   Canvas.pen.color:=colorBlue;
   canvas.font.color:=colorBlue;
   canvas.textout(5,0,'α');


   //draw DEC trend
   Canvas.pen.color:=colorRed;
   canvas.font.color:=colorRed;
   canvas.textout(15,0,'δ');
   counter:=lenb;
   for i:=lenb downto 0  do
   begin
     if xy_trend[i].ra<1E99 then //valid data
     begin
       y:=h2-round(xy_trend[i].dec*10*thescale);
       y:=min(max(0,y),height);
       x:=width-counter*((width-5) div lenb)-15;
       if counter<>lenb then canvas.lineto(x,y) else canvas.moveto(x,y);
       dec(counter);
     end;
   end;

   //drawDEC  corrective action
   Canvas.pen.color:=colorRed;
   canvas.font.color:=colorText;
   counter:=lenb;
   for i:=lenb downto 0  do
   begin
     if xy_trend[i].ra<1E99 then //valid data
     begin
       y:=h2-round(xy_trend[i].deccorr*10*thescale) ;
       y:=min(max(0,y),height);
       x:=width-counter*((width-5) div lenb)-15;
       canvas.moveto(x-1,h2);{one pixel behind to allow both RA and DEC action to be drawn}
       canvas.lineto(x-1,y);
       dec(counter);
     end;
   end;

   //draw RA trend
   Canvas.Pen.width :=2;{thickness lines}
   Canvas.pen.color:=colorBlue;
   counter:=lenb;
   for i:=lenb downto 0  do
   begin
     if xy_trend[i].ra<1E99 then //valid data
     begin
       y:=h2-round(xy_trend[i].ra*10*thescale) ;
       y:=min(max(0,y),height);
       x:=width-counter*((width-5) div lenb)-15;
       if counter<>lenb then canvas.lineto(x,y) else canvas.moveto(x,y);
       dec(counter);
     end;
   end;

   //draw RA corrective action
   counter:=lenb;
   for i:=lenb downto 0  do
   begin
     if xy_trend[i].ra<1E99 then //valid data
     begin
       y:=h2-round(xy_trend[i].racorr*10*thescale) ;
       y:=min(max(0,y),height);
       x:=width-counter*((width-5) div lenb)-15;
       canvas.moveto(x+1,h2);{one pixel before to allow both RA and DEC action to be drawn}
       canvas.lineto(x+1,y);
       dec(counter);
     end;
   end;
 end;
end;

procedure Tf_internalguider.BtnZoomAdjustClick(Sender: TObject);
begin
  GuideImgZoom:=0;
  if Assigned(FonRedraw) then FonRedraw(self);
end;

procedure Tf_internalguider.Button1Click(Sender: TObject);
begin
  if Assigned(FonConfigureGuider) then FonConfigureGuider(self);
end;

procedure Tf_internalguider.ButtonSetLockClick(Sender: TObject);
begin
  InternalGuiderSetLockPosition:=true;
  ButtonSetLock.Down:=true;
end;

procedure Tf_internalguider.BtnZoom05Click(Sender: TObject);
begin
  GuideImgZoom:=0.5;
  if Assigned(FonRedraw) then FonRedraw(self);
end;

procedure Tf_internalguider.BtnZoom1Click(Sender: TObject);
begin
  GuideImgZoom:=1;
  if Assigned(FonRedraw) then FonRedraw(self);
end;

procedure Tf_internalguider.BtnZoom2Click(Sender: TObject);
begin
  GuideImgZoom:=2;
  if Assigned(FonRedraw) then FonRedraw(self);
end;

function Tf_internalguider.GetFrameSize: integer;
var
  err, v : integer;
begin
  val(framesize1.text,v,err);
  if err<>0 then result:=999999 else result:=v;
end;

function Tf_internalguider.GetSolarTracking: boolean;
begin
  result:=CheckBoxTrackSolar1.Checked;
end;

procedure Tf_internalguider.SetSolarTracking(value: Boolean);
begin
  CheckBoxTrackSolar1.Checked:=value;
end;

procedure Tf_internalguider.SetSpectro(value:Boolean);
begin
  cbSpectro.Checked:=value;
end;

function Tf_internalguider.GetSpectro:Boolean;
begin
  result:=cbSpectro.Checked;
end;

procedure Tf_internalguider.SetSpectroStrategy(value:TSpectroStrategy);
begin
  rgSpectroStrategy.ItemIndex:=ord(value);
end;

function Tf_internalguider.GetSpectroStrategy:TSpectroStrategy;
begin
  result:=TSpectroStrategy(rgSpectroStrategy.ItemIndex);
end;

procedure Tf_internalguider.SetGuideLock(value:Boolean);
begin
  FGuideLock:=value;
end;

function Tf_internalguider.GetGuideLock:Boolean;
begin
  result:=FGuideLock;
end;

function Tf_internalguider.GetSpectroAstrometry:Boolean;
begin
 result:=FGuideAstrometry;
end;

function Tf_internalguider.GetSpectroAstrometryExposure:double;
begin
  result:=AstrometryExp.Value;
end;

procedure Tf_internalguider.SetSpectroAstrometryExposure(value:double);
begin
  AstrometryExp.Value:=value;
end;

function Tf_internalguider.GetFGuideMultistar:Boolean;
begin
  result:=FGuideMultistar;
end;

procedure Tf_internalguider.SetForceMultistar(value:Boolean);
begin
  FForceMultiStar:=value;
end;

function Tf_internalguider.GetForceMultistar:Boolean;
begin
  result:=FForceMultiStar;
end;

function Tf_internalguider.GetRefX:double;
begin
 result:=edRefX.value;
end;
procedure Tf_internalguider.SetRefX(value:double);
begin
 edRefX.value:=value;
end;
function Tf_internalguider.GetRefY:double;
begin
 result:=edRefY.value;
end;
procedure Tf_internalguider.SetRefY(value:double);
begin
 edRefY.value:=value;
end;

function Tf_internalguider.GetLockX:double;
begin
 result:=edRefX.value + SlitOffsetX.Value + StarOffsetX.Value;
end;

function Tf_internalguider.GetLockY:double;
begin
 result:=edRefY.value + SlitOffsetY.value + StarOffsetY.Value;
end;

function Tf_internalguider.GetSlitPosX:double;
begin
 result:=edRefX.value + SlitOffsetX.Value;
end;

function Tf_internalguider.GetSlitPosY:double;
begin
 result:=edRefY.value + SlitOffsetY.value;
end;

function Tf_internalguider.GetSearchWinMin:integer;
begin
 result:=edslitWinMin.value;
end;

procedure Tf_internalguider.SetSearchWinMin(value:integer);
begin
 edslitWinMin.value:=value;
end;

function Tf_internalguider.GetSearchWinMax:integer;
begin
 result:=edslitWinMax.value;
end;

procedure Tf_internalguider.SetSearchWinMax(value:integer);
begin
 edslitWinMax.value:=value;
end;

function Tf_internalguider.GetSlitX:integer;
begin
 result:=spSlitX.value;
end;

procedure Tf_internalguider.SetSlitX(value:integer);
begin
 spSlitX.value:=value;
end;

function Tf_internalguider.GetSlitY:integer;
begin
 result:=spSlitY.value;
end;

procedure Tf_internalguider.SetSlitY(value:integer);
begin
 spSlitY.value:=value;
end;

function Tf_internalguider.GetSlitW:integer;
begin
 result:=spSlitW.value;
end;

procedure Tf_internalguider.SetSlitW(value:integer);
begin
 spSlitW.value:=value;
end;

function Tf_internalguider.GetSlitL:integer;
begin
 result:=spSlitL.value;
end;

procedure Tf_internalguider.SetSlitL(value:integer);
begin
 spSlitL.value:=value;
end;

function Tf_internalguider.GetSlitPA:integer;
begin
 result:=spSlitPA.value;
end;

procedure Tf_internalguider.SetSlitPA(value:integer);
begin
 spSlitPA.value:=value;
end;

function Tf_internalguider.GetDrawSlit:Boolean;
begin
 result:=cbDrawSlit.Checked;
end;

procedure Tf_internalguider.SetDrawSlit(value:Boolean);
begin
 cbDrawSlit.Checked:=value;
end;

function Tf_internalguider.GetOffsetX:double;
begin
 result:=-edOffsetX.value;
end;

procedure Tf_internalguider.SetOffsetX(value:double);
begin
 if value=0 then
   edOffsetX.Value:=0
 else
   edOffsetX.Value:=-value;
end;

function Tf_internalguider.GetOffsetY:double;
begin
 result:=edOffsetY.value;
end;

procedure Tf_internalguider.SetOffsetY(value:double);
begin
 edOffsetY.Value:=value;
end;

function Tf_internalguider.CalibrationIsValid(out detail: string): boolean;
begin
  result:=true;
  detail:='';
  if CalDate.Text='' then begin
    result:=false;
    detail:='No calibration done with current program version.';
    exit;
  end;
  if CalBinning.Text<>IntToStr(Binning.Value) then begin
    result:=false;
    detail:='Binning change, ';
  end;
  if CalRAspeed.Text<>FormatFloat(f1,GuideSpeedRA.Value) then begin
    result:=false;
    detail:=detail+'RA guide rate change, ';
  end;
  if CalDECspeed.Text<>FormatFloat(f1,GuideSpeedDEC.Value) then begin
    result:=false;
    detail:=detail+'DEC guide rate change, ';
  end;
end;

procedure Tf_internalguider.SetCameraStatus(status: string);
begin
 LabelInfo3.Caption:=status;
 LabelInfo4.Caption:=status;
end;

function Tf_internalguider.GetCameraStatus: string;
begin
 result:=LabelInfo3.Caption;
end;

procedure Tf_internalguider.SetInfo(status: string);
begin
 LabelInfo.Caption:=status;
 LabelInfo2.Caption:=status;
end;

function Tf_internalguider.GetInfo: string;
begin
 result:=LabelInfo.Caption;
end;

procedure Tf_internalguider.msg(txt:string; level: integer);
begin
 if assigned(FonShowMessage) then FonShowMessage(rsGuideCamera+': '+txt,level);
end;

function Tf_internalguider.Snapshot(exp: double; fn: string): boolean;
var bin,sgain,soffset: integer;
begin
try
  if (Assigned(Fcamera)) and (FCamera.Status=devConnected) then begin
    if (not InternalguiderRunning) then begin
      sgain:=gain.Value;
      soffset:=Offset.Value;
      bin:=Binning.Value;
      msg(format(rsExposureS,[FormatFloat(f3,exp)])+blank+rsSeconds,3);
      if not Fcamera.ControlExposure(exp,bin,bin,LIGHT,ReadoutModeCapture,sgain,soffset,true) then begin
        msg(rsExposureFail,0);
        result:=false;
        exit;
      end;
      FCamera.Fits.SaveToFile(fn);
      result:=true;
    end
    else begin
      msg('Cannot take snapshot when the guider is running',1);
      result:=false;
    end;
  end
  else begin
    msg(rsSomeDefinedD,1);
    result:=false;
  end;
except
  result:=false;
end;
end;

function Tf_internalguider.SaveFits(fn: string):boolean;
begin
try
  if (Assigned(Fcamera)) and (FCamera.Status=devConnected) then begin
    if Fcamera.Fits.HeaderInfo.valid and Fcamera.Fits.ImageValid then begin
      FCamera.Fits.SaveToFile(fn);
      result:=true;
    end
    else begin
      msg('No guider image is available to save',1);
      result:=false;
    end;
  end
  else begin
    msg(rsSomeDefinedD,1);
    result:=false;
  end;
except
  result:=false;
end;
end;

procedure Tf_internalguider.SetSpiralDither(value:Boolean);
begin
  if value then rgDitherMode.ItemIndex:=0
           else rgDitherMode.ItemIndex:=1;
end;

function Tf_internalguider.GetSpiralDither:Boolean;
begin
  result:=rgDitherMode.ItemIndex=0;
end;

procedure Tf_internalguider.ClearSlitList;
var i: integer;
begin
  for i:=cbSlitList.Items.Count-1 downto 0 do begin
    if cbSlitList.Items.Objects[i]<>nil then cbSlitList.Items.Objects[i].Free;
    cbSlitList.Items.Delete(i);
  end;
end;

procedure Tf_internalguider.MenuSlitOffsetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var p: TPoint;
begin
  p.x:=0;
  p.y:=MenuSlitOffset.Height;
  p:=MenuSlitOffset.ClientToScreen(p);
  PopupMenuSlit.PopUp(p.x,p.y);
end;

procedure Tf_internalguider.btnAddSlitOffsetClick(Sender: TObject);
var so:TSlitOffset;
    i: integer;
    n: string;
begin
  n:=trim(cbSlitList.text);
  i:=cbSlitList.Items.IndexOf(n);
  if i>=0 then begin
    so:=TSlitOffset(cbSlitList.items.Objects[i]);
    so.x:=SlitOffsetX.Value;
    so.y:=SlitOffsetY.Value;
  end
  else begin
    so:=TSlitOffset.Create;
    so.slitname:=n;
    so.x:=SlitOffsetX.Value;
    so.y:=SlitOffsetY.Value;
    i:=cbSlitList.Items.AddObject(so.slitname,so);
  end;
  cbSlitList.ItemIndex:=i;
  CurrentSlit:=i;
end;

procedure Tf_internalguider.SlitOffsetChange(Sender: TObject);
var so:TSlitOffset;
    i: integer;
begin
  i:=cbSlitList.ItemIndex;
  if (i>=0) then begin
    so:=TSlitOffset(cbSlitList.Items.Objects[i]);
    so.x:=SlitOffsetX.Value;
    so.y:=SlitOffsetY.Value;
  end;
  ForceRedraw(Sender);
end;

procedure Tf_internalguider.btnDelSlitOffsetClick(Sender: TObject);
var i: integer;
begin
 i:=CurrentSlit;
 if (i>=0)and(i<cbSlitList.Items.Count) then begin
   if cbSlitList.Items.Objects[i]<>nil then cbSlitList.Items.Objects[i].Free;
   cbSlitList.Items.Delete(i);
   cbSlitList.ItemIndex:=i-1;
   cbSlitListChange(Sender);
 end;
end;

procedure Tf_internalguider.cbSlitListChange(Sender: TObject);
var so:TSlitOffset;
    i: integer;
begin
  i:=cbSlitList.ItemIndex;
  if (i>=0)and(i<cbSlitList.Items.Count) then begin
    CurrentSlit:=i;
    so:=TSlitOffset(cbSlitList.Items.Objects[CurrentSlit]);
    cbSlitList.text := so.slitname;
    SlitOffsetX.Value := so.x;
    SlitOffsetY.Value := so.y;
  end;
end;

procedure Tf_internalguider.btnRefImageClick(Sender: TObject);
begin
  if assigned(FonMeasureReferenceImage) then FonMeasureReferenceImage(self);
  CheckGuiderReferenceFile;
end;

procedure Tf_internalguider.CheckGuiderReferenceFile;
begin
  if FileExists(ConfigGuiderReferenceFile) then
    Shape1.Brush.Color:=clLime
  else
    Shape1.Brush.Color:=clRed;
end;

end.

