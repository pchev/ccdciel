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
  XMLConf, DOM, Classes, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, ComCtrls;

type

  { Tf_eqmod }

  Tf_eqmod = class(TForm)
    ALT: TEdit;
    BtnTrackSidereal: TSpeedButton;
    BtnTrackLunar: TSpeedButton;
    BtnTrackSolar: TSpeedButton;
    BtnTrackCustom: TSpeedButton;
    ReverseDec: TCheckBox;
    SlewPreset: TComboBox;
    GroupBox10: TGroupBox;
    GroupBox11: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    GroupBox9: TGroupBox;
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
    SpeedButton2: TSpeedButton;
    RArate: TTrackBar;
    DErate: TTrackBar;
    IndiSetup: TSpeedButton;
    TrackDEC: TEdit;
    TRackRA: TEdit;
    RA: TEdit;
    GroupBox1: TGroupBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
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
    BtnTrackStop: TSpeedButton;
    StaticText1: TStaticText;
    TopPanel: TPanel;
    IndiBtn: TPanel;
    TitlePanel: TPanel;
    procedure BtnEastMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnNorthMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnSouthMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
    procedure SlewPresetChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
    eqmod: T_indieqmod;
    f_indigui: Tf_indigui;
    config: TXMLConfig;
    configfile,indiserver,indiserverport,indidevice,indideviceport:string;
    ready, GUIready, indisimulation: boolean;
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
    Procedure TrackModeChange(Sender: TObject);
  public
    { public declarations }
  end;

var
  f_eqmod: Tf_eqmod;

implementation

const
  clOrange=$1080EF;

{$R *.lfm}

{ Tf_eqmod }

procedure Tf_eqmod.FormCreate(Sender: TObject);
begin
 Notebook1.PageIndex:=0;
 configfile:=GetAppConfigFileUTF8(false,true,true);
 config:=TXMLConfig.Create(self);
 config.Filename:=configfile;
 ready:=false;
 GUIready:=false;
end;

procedure Tf_eqmod.FormDestroy(Sender: TObject);
begin
  config.Free;
end;

procedure Tf_eqmod.FormShow(Sender: TObject);
begin
  Connect;
end;

procedure Tf_eqmod.Connect;
begin
 if not ready then begin
   indiserver:=config.GetValue('/INDI/server','localhost');
   indiserverport:=config.GetValue('/INDI/serverport','7624');
   indidevice:=config.GetValue('/INDI/device','EQMod Mount');
   indideviceport:=config.GetValue('/INDI/deviceport','/dev/ttyUSB0');
   indisimulation:=config.GetValue('/INDI/simulation',false);
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

procedure Tf_eqmod.GUIdestroy(Sender: TObject);
begin
  GUIready:=false;
end;

procedure Tf_eqmod.SetupBtnClick(Sender: TObject);
begin
 if setupbtn.Caption='>>>' then begin
   setupbtn.Caption:='<<<';
   panel2.Visible:=true;
   panel3.Visible:=true;
 end else begin
   setupbtn.Caption:='>>>';
   panel2.Visible:=false;
   panel3.Visible:=false;
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
                      SlewModeChange(Sender);
                      SlewSpeedRange;
                      RevDecChange(Sender);
                      SlewSpeedChange(Sender);
                      SlewPresetChange(Sender);
                      TrackModeChange(Sender);
                   end;
end;
end;

Procedure Tf_eqmod.CoordChange(Sender: TObject);
begin
 RA.Text:=RAToStr(eqmod.RA);
 DE.Text:=DEToStr(eqmod.Dec);
end;

Procedure Tf_eqmod.AltAzChange(Sender: TObject);
begin
 AZ.Text:=DEToStr(eqmod.AZ);
 ALT.Text:=DEToStr(eqmod.ALT);
end;

Procedure Tf_eqmod.LSTChange(Sender: TObject);
begin
 LST.Text:=RAToStr(eqmod.LST);
end;

Procedure Tf_eqmod.PierSideChange(Sender: TObject);
begin
  PierSide.Text:=eqmod.PierSideLbl;
end;

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

procedure Tf_eqmod.RArateChange(Sender: TObject);
begin
  eqmod.RASlewSpeed:=RArate.Position;
end;

procedure Tf_eqmod.DErateChange(Sender: TObject);
begin
  eqmod.DESlewSpeed:=DErate.Position;
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

Procedure Tf_eqmod.TrackModeChange(Sender: TObject);
begin
  case eqmod.TrackMode of
    -1 : BtnTrackStop.Down:=true;
     0 : BtnTrackSidereal.Down:=true;
     1 : BtnTrackLunar.Down:=true;
     2 : BtnTrackSolar.Down:=true;
     3 : BtnTrackCustom.Down:=true;
  end;
end;

procedure Tf_eqmod.SetTrackModeClick(Sender: TObject);
begin
if sender is TSpeedButton then
  eqmod.TrackMode:=TSpeedButton(sender).tag;
end;


end.

