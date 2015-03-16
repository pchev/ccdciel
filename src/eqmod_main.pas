unit eqmod_main;

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

uses eqmod_int, eqmod_setup, pu_indigui, u_utils,
  u_ccdconfig, XMLConf, DOM,
  Classes, SysUtils, FileUtil, Forms, Controls, LCLType,
  Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, ComCtrls;

type

  { Tf_eqmod }

  Tf_eqmod = class(TForm)
    BtnPark: TSpeedButton;
    BtnINDIgui: TButton;
    BtnSaveAlign: TSpeedButton;
    BtnSaveIndiSettings: TSpeedButton;
    BtnSaveSite2: TSpeedButton;
    BtnLoadAlign: TSpeedButton;
    Label19: TLabel;
    SyncModeCombo: TComboBox;
    AlignModeCombo: TComboBox;
    DeltaRa: TEdit;
    DeltaDe: TEdit;
    ALT: TEdit;
    BtnClearAlignment: TSpeedButton;
    BtnClearDelta: TSpeedButton;
    BtnTrackSidereal: TSpeedButton;
    BtnTrackLunar: TSpeedButton;
    BtnTrackStop: TSpeedButton;
    BtnTrackSolar: TSpeedButton;
    BtnTrackCustom: TSpeedButton;
    AlignPoints: TEdit;
    AlignTriangles: TEdit;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    SiteName: TComboBox;
    LatNS: TComboBox;
    LongWE: TComboBox;
    LatDeg: TEdit;
    LatMin: TEdit;
    LatSec: TEdit;
    LongDeg: TEdit;
    LongMin: TEdit;
    LongSec: TEdit;
    Elevation: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label9: TLabel;
    Panel4: TPanel;
    PanelCustTrack: TPanel;
    ReverseDec: TCheckBox;
    SlewPreset: TComboBox;
    GroupBox11: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    led: TShape;
    PanelSlewRate: TPanel;
    PierSide: TEdit;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    msg: TMemo;
    Notebook1: TNotebook;
    Page1: TPage;
    Page2: TPage;
    BtnUnPark: TSpeedButton;
    RArate: TTrackBar;
    DErate: TTrackBar;
    IndiSetup: TSpeedButton;
    BtnSetTrackRate: TSpeedButton;
    BtnSaveSite: TSpeedButton;
    SetSite: TSpeedButton;
    TrackDEC: TEdit;
    TRackRA: TEdit;
    RA: TEdit;
    GroupBox1: TGroupBox;
    Panel1: TPanel;
    Panel2: TPanel;
    DE: TEdit;
    AZ: TEdit;
    LST: TEdit;
    SpeedButton1: TSpeedButton;
    SetupBtn: TSpeedButton;
    BtnNorth: TSpeedButton;
    BtnStop: TSpeedButton;
    BtnEast: TSpeedButton;
    BtnWest: TSpeedButton;
    BtnSouth: TSpeedButton;
    StaticText1: TStaticText;
    TopPanel: TPanel;
    IndiBtn: TPanel;
    TitlePanel: TPanel;
    procedure AlignModeComboChange(Sender: TObject);
    procedure BtnClearAlignmentClick(Sender: TObject);
    procedure BtnClearDeltaClick(Sender: TObject);
    procedure BtnEastMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnLoadAlignClick(Sender: TObject);
    procedure BtnNorthMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnParkClick(Sender: TObject);
    procedure BtnSaveAlignClick(Sender: TObject);
    procedure BtnSaveIndiSettingsClick(Sender: TObject);
    procedure BtnSaveSiteClick(Sender: TObject);
    procedure BtnSouthMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnTrackPaint(Sender: TObject);
    procedure ElevationChange(Sender: TObject);
    procedure LatChange(Sender: TObject);
    procedure LatKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LongChange(Sender: TObject);
    procedure LongKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure SetSiteClick(Sender: TObject);
    procedure SetTrackModeClick(Sender: TObject);
    procedure BtnWestMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnMotionMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnStopClick(Sender: TObject);
    procedure DErateChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IndiBtnClick(Sender: TObject);
    procedure IndiSetupClick(Sender: TObject);
    procedure RArateChange(Sender: TObject);
    procedure ReverseDecChange(Sender: TObject);
    procedure SetupBtnClick(Sender: TObject);
    procedure SiteNameChange(Sender: TObject);
    procedure SlewPresetChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure BtnSetTrackRateClick(Sender: TObject);
    procedure BtnUnParkClick(Sender: TObject);
    procedure SyncModeComboChange(Sender: TObject);
  private
    { private declarations }
    eqmod: T_indieqmod;
    f_indigui: Tf_indigui;
    config: TCCDconfig;
    configfile,indiserver,indiserverport,indidevice,indideviceport:string;
    ready, GUIready, indisimulation, obslock: boolean;
    TrackMode: integer;
    ObsLat, ObsLon, ObsElev: double;
    procedure ReadConfig;
    procedure Connect;
    procedure GUIdestroy(Sender: TObject);
    procedure eqmodDestroy(Sender: TObject);
    Procedure StatusChange(Sender: TObject);
    Procedure CoordChange(Sender: TObject);
    Procedure AltAZChange(Sender: TObject);
    procedure NewMessage(txt: string);
    Procedure LSTChange(Sender: TObject);
    Procedure PierSideChange(Sender: TObject);
    procedure RevDecChange(Sender: TObject);
    Procedure SlewSpeedChange(Sender: TObject);
    Procedure SlewModeChange(Sender: TObject);
    procedure FillSlewPreset;
    procedure SlewSpeedRange;
    procedure TrackModeChange(Sender: TObject);
    procedure TrackRateChange(Sender: TObject);
    procedure ParkChange(Sender: TObject);
    procedure GeoCoordChange(Sender: TObject);
    procedure ShowObsCoord;
    procedure AlignCountChange(Sender: TObject);
    procedure SyncDeltaChange(Sender: TObject);
    procedure FillSyncMode;
    procedure FillAlignMode;
    procedure SyncModeChange(Sender: TObject);
    procedure AlignModeChange(Sender: TObject);
  public
    { public declarations }
  end;

var
  f_eqmod: Tf_eqmod;

implementation

const
  clOrange=$1080EF;
  f1='0.0';
  f5='0.00000';

{$R *.lfm}

{ Tf_eqmod }

procedure Tf_eqmod.FormCreate(Sender: TObject);
begin
 DefaultFormatSettings.DecimalSeparator:='.';
 Notebook1.PageIndex:=0;
 configfile:=GetAppConfigFileUTF8(false,true,true);
 config:=TCCDconfig.Create(self);
 config.Filename:=configfile;
 ready:=false;
 GUIready:=false;
 obslock:=false;
 TrackMode:=-999;
 Width:=226;
end;

procedure Tf_eqmod.FormDestroy(Sender: TObject);
begin
  config.Free;
end;

procedure Tf_eqmod.FormShow(Sender: TObject);
begin
  ReadConfig;
  Connect;
end;

procedure Tf_eqmod.ReadConfig;
var n,i: integer;
  buf: string;
begin
 indiserver:=config.GetValue('/INDI/server','localhost');
 indiserverport:=config.GetValue('/INDI/serverport','7624');
 indidevice:=config.GetValue('/INDI/device','EQMod Mount');
 indideviceport:=config.GetValue('/INDI/deviceport','/dev/ttyUSB0');
 indisimulation:=config.GetValue('/INDI/simulation',false);
 SiteName.Clear;
 n:=config.GetValue('/Site/Number',0);
 for i:=1 to n do begin
   buf:=config.GetValue('/Site/Site'+inttostr(i)+'/SiteName','');
   if buf<>'' then begin
     SiteName.Items.Add(buf);
   end;
 end;
end;

procedure Tf_eqmod.Connect;
begin
 if not ready then begin
   eqmod:=T_indieqmod.create;
   eqmod.onDestroy:=@eqmodDestroy;
   eqmod.onMsg:=@NewMessage;
   eqmod.onCoordChange:=@CoordChange;
   eqmod.onRevDecChange:=@RevDecChange;
   eqmod.onAltAZChange:=@AltAZChange;
   eqmod.onStatusChange:=@StatusChange;
   eqmod.onLSTChange:=@LSTChange;
   eqmod.onPierSideChange:=@PierSideChange;
   eqmod.onSlewSpeedChange:=@SlewSpeedChange;
   eqmod.onSlewModeChange:=@SlewModeChange;
   eqmod.onTrackModeChange:=@TrackModeChange;
   eqmod.onTrackRateChange:=@TrackRateChange;
   eqmod.onParkChange:=@ParkChange;
   eqmod.onGeoCoordChange:=@GeoCoordChange;
   eqmod.onAlignCountChange:=@AlignCountChange;
   eqmod.onSyncDeltaChange:=@SyncDeltaChange;
   eqmod.onSyncModeChange:=@SyncModeChange;
   eqmod.onAlignmentModeChange:=@AlignModeChange;
   if indiserver<>'' then eqmod.indiserver:=indiserver;
   if indiserverport<>'' then eqmod.indiserverport:=indiserverport;
   if indidevice<>'' then eqmod.indidevice:=indidevice;
   eqmod.indideviceport:=indideviceport;
   eqmod.simulation:=indisimulation;
   eqmod.Connect;
   ready:=true;
 end;
end;

procedure Tf_eqmod.IndiSetupClick(Sender: TObject);
begin
 if not ready then begin
   f_eqmodsetup:=Tf_eqmodsetup.Create(self);
   f_eqmodsetup.Server.Text:=config.GetValue('/INDI/server','localhost');
   f_eqmodsetup.Serverport.Text:=config.GetValue('/INDI/serverport','7624');
   f_eqmodsetup.Port.Text:=config.GetValue('/INDI/deviceport','/dev/ttyUSB0');
   f_eqmodsetup.Sim.Checked:=config.GetValue('/INDI/simulation',false);
   f_eqmodsetup.ShowModal;
   if f_eqmodsetup.ModalResult=mrOK then begin
      config.SetValue('/INDI/server',f_eqmodsetup.Server.Text);
      config.SetValue('/INDI/serverport',f_eqmodsetup.Serverport.Text);
      config.SetValue('/INDI/deviceport',f_eqmodsetup.Port.Text);
      config.SetValue('/INDI/simulation',f_eqmodsetup.Sim.Checked);
      config.Flush;
      Connect;
   end;
 end;
end;

procedure Tf_eqmod.IndiBtnClick(Sender: TObject);
begin
 if not GUIready then begin
    f_indigui:=Tf_indigui.Create(self);
    f_indigui.onDestroy:=@GUIdestroy;
    f_indigui.IndiServer:=indiserver;
    f_indigui.IndiPort:=indiserverport;
    f_indigui.IndiDevice:=indidevice;
    GUIready:=true;
 end;
 f_indigui.Show;
end;

procedure Tf_eqmod.BtnSaveIndiSettingsClick(Sender: TObject);
begin
  eqmod.SaveConfig;
end;

procedure Tf_eqmod.GUIdestroy(Sender: TObject);
begin
  GUIready:=false;
end;

procedure Tf_eqmod.SetupBtnClick(Sender: TObject);
begin
 if setupbtn.Caption='>>>' then begin
   setupbtn.Caption:='<<<';
   panel2.Visible:=true;
 end else begin
   setupbtn.Caption:='>>>';
   panel2.Visible:=false;
 end;
 AutoSize:=false;
 AutoSize:=true;
end;

procedure Tf_eqmod.SpeedButton1Click(Sender: TObject);
var i:integer;
begin
 i:=Notebook1.PageIndex;
 inc(i);
 if i>=Notebook1.PageCount then i:=0;
 Notebook1.PageIndex:=i;
end;

procedure Tf_eqmod.eqmodDestroy(Sender: TObject);
begin
 ready:=false;
end;

procedure Tf_eqmod.NewMessage(txt: string);
begin
 if txt<>'' then begin
  if msg.Lines.Count>100 then msg.Lines.Delete(0);
  msg.Lines.Add(FormatDateTime('hh:nn:ss',now)+':'+txt);
  msg.SelStart:=msg.GetTextLen-1;
  msg.SelLength:=0;
  msg.ScrollBy(0,msg.Lines.Count);
 { if LogToFile then begin
    WriteLog(msg);
  end;  }
 end;
end;

Procedure Tf_eqmod.StatusChange(Sender: TObject);
begin
case eqmod.Status of
  devDisconnected:begin
                      led.Brush.Color:=clRed;
                      ready:=false;
                  end;
  devConnecting:  begin
                      NewMessage('Connecting mount...');
                      led.Brush.Color:=clOrange;
                   end;
  devConnected:   begin
                      NewMessage('Mount connected');
                      led.Brush.Color:=clGreen;
                      CoordChange(Sender);
                      LSTChange(Sender);
                      PierSideChange(Sender);
                      FillSlewPreset;
                      FillSyncMode;
                      FillAlignMode;
                      SlewModeChange(Sender);
                      SlewSpeedRange;
                      RevDecChange(Sender);
                      SlewSpeedChange(Sender);
                      SlewPresetChange(Sender);
                      TrackModeChange(Sender);
                      TrackRateChange(Sender);
                      ParkChange(Sender);
                      GeoCoordChange(Sender);
                      AlignCountChange(Sender);
                      SyncDeltaChange(Sender);
                      SyncModeChange(Sender);
                      AlignModeChange(Sender);
                   end;
end;
end;

//////////////////  Mount position box ////////////////////////

Procedure Tf_eqmod.CoordChange(Sender: TObject);
begin
 RA.Text:=SXToStr(eqmod.RA);
 DE.Text:=SXToStr(eqmod.Dec);
end;

Procedure Tf_eqmod.AltAzChange(Sender: TObject);
begin
 AZ.Text:=SXToStr(eqmod.AZ);
 ALT.Text:=SXToStr(eqmod.ALT);
end;

Procedure Tf_eqmod.LSTChange(Sender: TObject);
begin
 LST.Text:=SXToStr(eqmod.LST);
end;

Procedure Tf_eqmod.PierSideChange(Sender: TObject);
begin
  PierSide.Text:=eqmod.PierSideLbl;
  TrackModeChange(Sender);
end;

//////////////////  Slew control box ////////////////////////

procedure Tf_eqmod.BtnNorthMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  eqmod.MotionNorth;
end;

procedure Tf_eqmod.BtnSouthMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  eqmod.MotionSouth;
end;

procedure Tf_eqmod.BtnEastMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  eqmod.MotionEast;
end;

procedure Tf_eqmod.BtnWestMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  eqmod.MotionWest;
end;

procedure Tf_eqmod.BtnMotionMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  eqmod.MotionStop;
end;

procedure Tf_eqmod.BtnStopClick(Sender: TObject);
begin
  eqmod.MotionStop;
end;

procedure Tf_eqmod.RArateChange(Sender: TObject);
begin
  eqmod.RASlewSpeed:=RArate.Position;
end;

procedure Tf_eqmod.DErateChange(Sender: TObject);
begin
  eqmod.DESlewSpeed:=DErate.Position;
end;

procedure Tf_eqmod.RevDecChange(Sender: TObject);
begin
  ReverseDec.Checked:=eqmod.ReverseDec;
end;

procedure Tf_eqmod.ReverseDecChange(Sender: TObject);
begin
  eqmod.ReverseDec:=ReverseDec.Checked;
end;

procedure Tf_eqmod.SlewSpeedRange;
var min,max,step: integer;
    range:TNumRange;
begin
  // RA
  range:=eqmod.RASlewSpeedRange;
  min:=trunc(range.min);
  if min<1 then min:=1;
  max:=trunc(range.max);
  step:=trunc(range.step);
  if step<1 then step:=1;
  RArate.Min:=min;
  RArate.Max:=max;
  RArate.LineSize:=step;
  RArate.PageSize:=(max-min) div 10;
  // DEC
  range:=eqmod.DESlewSpeedRange;
  min:=trunc(range.min);
  if min<1 then min:=1;
  max:=trunc(range.max);
  step:=trunc(range.step);
  if step<1 then step:=1;
  DErate.Min:=min;
  DErate.Max:=max;
  DErate.LineSize:=step;
  DErate.PageSize:=(max-min) div 10;
end;

Procedure Tf_eqmod.SlewSpeedChange(Sender: TObject);
begin
  RArate.Position:=eqmod.RASlewSpeed;
  DErate.Position:=eqmod.DESlewSpeed;
end;

procedure Tf_eqmod.FillSlewPreset;
begin
  SlewPreset.Clear;
  SlewPreset.Items.Assign(eqmod.SlewPreset);
end;

Procedure Tf_eqmod.SlewModeChange(Sender: TObject);
begin
  SlewPreset.ItemIndex:=eqmod.ActiveSlewPreset;
end;

procedure Tf_eqmod.SlewPresetChange(Sender: TObject);
begin
  PanelSlewRate.Visible:=(SlewPreset.ItemIndex=0);
  eqmod.ActiveSlewPreset:=SlewPreset.ItemIndex;
end;

//////////////////  Track rate box ////////////////////////

Procedure Tf_eqmod.TrackModeChange(Sender: TObject);
begin
 if eqmod.TrackMode<>TrackMode then begin
     TrackMode:=eqmod.TrackMode;
     BtnTrackStop.Down:=false;
     BtnTrackSidereal.Down:=false;
     BtnTrackLunar.Down:=false;
     BtnTrackSolar.Down:=false;
     BtnTrackCustom.Down:=false;
     PanelCustTrack.Visible:=false;
      case TrackMode of
        -1 : BtnTrackStop.Down:=true;
         0 : BtnTrackSidereal.Down:=true;
         1 : BtnTrackLunar.Down:=true;
         2 : BtnTrackSolar.Down:=true;
         3 : begin
             BtnTrackCustom.Down:=true;
             PanelCustTrack.Visible:=true;
             end;
      end;
 end;
end;

procedure Tf_eqmod.SetTrackModeClick(Sender: TObject);
begin
if sender is TSpeedButton then
  eqmod.TrackMode:=TSpeedButton(sender).tag;
  PanelCustTrack.Visible:=(TSpeedButton(sender).tag=3);
end;

procedure Tf_eqmod.BtnSetTrackRateClick(Sender: TObject);
var x,y: double;
begin
x:=StrToFloatDef(TrackRA.Text,-99999);
y:=StrToFloatDef(TrackDEC.Text,-99999);
if (x>-99999)and(y>=-99999) then begin
   eqmod.SetTrackRate(x,y);
end;
end;

Procedure Tf_eqmod.TrackRateChange(Sender: TObject);
begin
  if ActiveControl<>TRackRA then
     TRackRA.Text:=FormatFloat(f5,eqmod.RATrackRate);
  if ActiveControl<>TRackDEC then
     TRackDEC.Text:=FormatFloat(f5,eqmod.DETrackRate);
end;

procedure Tf_eqmod.BtnTrackPaint(Sender: TObject);
begin
with (Sender as TSpeedButton) do begin
  if Down then begin
    Canvas.Brush.Color:=clGray;
    Canvas.FillRect(0,0,Canvas.Width,Canvas.Height);
    Canvas.Draw(4,4,Glyph);
  end;
end;
end;

//////////////////  Park/Unpark box ////////////////////////

Procedure Tf_eqmod.ParkChange(Sender: TObject);
begin
// Park status is useles for now (version 1.0) returning always off for both switch

 { if eqmod.Park then begin
     BtnUnPark.Caption:='Unpark';
     LblPark.Caption:='Parked';
  end
  else begin
     BtnUnPark.Caption:='Park';
     LblPark.Caption:='Unparked';
  end;   }
end;

procedure Tf_eqmod.BtnParkClick(Sender: TObject);
begin
  if MessageDlg('Park the telescope now?',mtConfirmation,mbYesNo,0)=mrYes then
     eqmod.Park:=true;
end;

procedure Tf_eqmod.BtnUnParkClick(Sender: TObject);
begin
  eqmod.Park:=false;
end;


//////////////////  Site information box ////////////////////////

procedure Tf_eqmod.GeoCoordChange(Sender: TObject);
var i,n,s:integer;
    la,lo,el:double;
begin
  ObsLat:=eqmod.Latitude;
  ObsLon:=eqmod.Longitude;
  ObsElev:=eqmod.Elevation;
  ShowObsCoord;
  n:=config.GetValue('/Site/Number',0);
  for i:=1 to n do begin
    la:=config.GetValue('/Site/Site'+inttostr(i)+'/Latitude',0.0);
    lo:=config.GetValue('/Site/Site'+inttostr(i)+'/Longitude',0.0);
    el:=config.GetValue('/Site/Site'+inttostr(i)+'/Elevation',0.0);
    if (la=ObsLat)and(lo=ObsLon)and(el=ObsElev) then begin
      SiteName.Text:=config.GetValue('/Site/Site'+inttostr(i)+'/SiteName','');
      break;
    end;
  end;
end;

Procedure Tf_eqmod.ShowObsCoord;
var d,m,s : string;
    long: double;
begin
try
obslock:=true;
Elevation.Text:=IntToStr(round(ObsElev));
ArToStr4(abs(ObsLat),f1,d,m,s);
LatDeg.Text:=d;
LatMin.Text:=m;
LatSec.Text:=s;
if ObsLon<180 then
  long:=ObsLon
  else
  long:=ObsLon-360;
ArToStr4(abs(long),f1,d,m,s);
LongDeg.Text:=d;
LongMin.Text:=m;
LongSec.Text:=s;
if ObsLat>=0 then LatNS.Itemindex:=0
             else LatNS.Itemindex:=1;
if long>=0 then LongWE.Itemindex:=0
           else LongWE.Itemindex:=1;
finally
obslock:=false;
end;
end;

procedure Tf_eqmod.LatChange(Sender: TObject);
var d,m,s: double;
    n: integer;
begin
  if obslock then exit;
  val(LatDeg.Text,d,n);
  if n>0 then exit;
  val(LatMin.Text,m,n);
  if n>0 then exit;
  val(LatSec.Text,s,n);
  if n>0 then exit;
  if frac(d)>0 then
    ObsLat:=d
  else
    ObsLat:=d+m/60+s/3600;
  if LatNS.Itemindex>0 then ObsLat:=-ObsLat;
  ShowObsCoord;
end;

procedure Tf_eqmod.LatKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=VK_RETURN then LatChange(Sender);
end;

procedure Tf_eqmod.LongChange(Sender: TObject);
var d,m,s: double;
    n: integer;
begin
  if obslock then exit;
  val(LongDeg.Text,d,n);
  if n>0 then exit;
  val(LongMin.Text,m,n);
  if n>0 then exit;
  val(LongSec.Text,s,n);
  if n>0 then exit;
  if frac(d)>0 then
    ObsLon:=d
  else
    ObsLon:=d+m/60+s/3600;
  if LongWE.Itemindex>0 then ObsLon:=360-ObsLon;
  ShowObsCoord;
end;

procedure Tf_eqmod.LongKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=VK_RETURN then LongChange(Sender);
end;

procedure Tf_eqmod.ElevationChange(Sender: TObject);
var e: double;
    n: integer;
begin
  val(Elevation.Text,e,n);
  if n=0 then ObsElev:=e;
end;

procedure Tf_eqmod.SetSiteClick(Sender: TObject);
begin
  LatChange(Sender);
  LongChange(Sender);
  ElevationChange(Sender);
  eqmod.SetSite(ObsLat,ObsLon,ObsElev);
end;

procedure Tf_eqmod.BtnSaveSiteClick(Sender: TObject);
var i,n,s:integer;
    buf:string;
begin
LatChange(Sender);
LongChange(Sender);
ElevationChange(Sender);
s:=-1;
n:=config.GetValue('/Site/Number',0);
for i:=1 to n do begin
  buf:=config.GetValue('/Site/Site'+inttostr(i)+'/SiteName','');
  if buf=SiteName.Text then begin
    s:=i;
    break;
  end;
end;
if s<0 then begin
 s:=n+1;
 SiteName.Items.Add(SiteName.Text);
end;
config.SetValue('/Site/Site'+inttostr(s)+'/SiteName',SiteName.Text);
config.SetValue('/Site/Site'+inttostr(s)+'/Latitude',ObsLat);
config.SetValue('/Site/Site'+inttostr(s)+'/Longitude',ObsLon);
config.SetValue('/Site/Site'+inttostr(s)+'/Elevation',ObsElev);
config.SetValue('/Site/Number',s);
config.Flush;
end;

procedure Tf_eqmod.SiteNameChange(Sender: TObject);
var i,n,s:integer;
    buf:string;
begin
n:=config.GetValue('/Site/Number',0);
for i:=1 to n do begin
  buf:=config.GetValue('/Site/Site'+inttostr(i)+'/SiteName','');
  if buf=SiteName.Text then begin
    ObsLat:=config.GetValue('/Site/Site'+inttostr(i)+'/Latitude',0.0);
    ObsLon:=config.GetValue('/Site/Site'+inttostr(i)+'/Longitude',0.0);
    ObsElev:=config.GetValue('/Site/Site'+inttostr(i)+'/Elevation',0.0);
    ShowObsCoord;
  end;
end;
end;

//////////////////  Alignment / Sync  box ////////////////////////

procedure Tf_eqmod.AlignCountChange(Sender: TObject);
begin
 AlignPoints.Text:=inttostr(eqmod.PointCount);
 AlignTriangles.Text:=inttostr(eqmod.TriangleCount);
end;

procedure Tf_eqmod.BtnClearAlignmentClick(Sender: TObject);
begin
  if MessageDlg('Clear all the alignment points?',mtConfirmation,mbYesNo,0)=mrYes then begin
     eqmod.ClearAlignment;
  end;
end;

procedure Tf_eqmod.SyncDeltaChange(Sender: TObject);
begin
   DeltaRa.Text:=SXToStr(eqmod.DeltaRa);
   DeltaDe.Text:=SXToStr(eqmod.DeltaDe);
end;

procedure Tf_eqmod.BtnClearDeltaClick(Sender: TObject);
begin
  if MessageDlg('Clear Sync delta?',mtConfirmation,mbYesNo,0)=mrYes then begin
     eqmod.ClearSyncDelta;
  end;
end;

procedure Tf_eqmod.BtnSaveAlignClick(Sender: TObject);
var fn,site: string;
begin
  fn:=ExpandFileNameUTF8('~/.indi');
  site:=trim(SiteName.Text);
  ForceDirectoriesUTF8(fn);
  fn:=fn+'/AlignData'+site+'.xml';
  eqmod.SaveAlignment(fn);
end;

procedure Tf_eqmod.BtnLoadAlignClick(Sender: TObject);
var fn,site: string;
begin
  fn:=ExpandFileNameUTF8('~/.indi');
  site:=trim(SiteName.Text);
  fn:=fn+'/AlignData'+site+'.xml';
  eqmod.LoadAlignment(fn);
end;

procedure Tf_eqmod.FillSyncMode;
begin
SyncModeCombo.Clear;
SyncModeCombo.Items.Assign(eqmod.SyncMode);
end;

procedure Tf_eqmod.FillAlignMode;
begin
AlignModeCombo.Clear;
AlignModeCombo.Items.Assign(eqmod.AlignmentMode);
end;

procedure Tf_eqmod.SyncModeChange(Sender: TObject);
begin
  SyncModeCombo.ItemIndex:=eqmod.ActiveSyncMode;
end;

procedure Tf_eqmod.AlignModeChange(Sender: TObject);
begin
 AlignModeCombo.ItemIndex:=eqmod.ActiveAlignmentMode;
end;

procedure Tf_eqmod.SyncModeComboChange(Sender: TObject);
begin
  eqmod.ActiveSyncMode:=SyncModeCombo.ItemIndex;
end;

procedure Tf_eqmod.AlignModeComboChange(Sender: TObject);
begin
  eqmod.ActiveAlignmentMode:=AlignModeCombo.ItemIndex
end;


end.


