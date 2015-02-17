unit eqmod_main;

{$mode objfpc}{$H+}

interface

uses eqmod_int, pu_indigui, u_utils,
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
    DECrate: TTrackBar;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IndiBtnClick(Sender: TObject);
    procedure SetupBtnClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
    eqmod: T_indieqmod;
    f_indigui: Tf_indigui;
    config: TXMLConfig;
    configfile,indiserver,indiserverport,indidevice,indideviceport:string;
    GUIready: boolean;
    procedure GUIdestroy(Sender: TObject);
    procedure eqmodDestroy(Sender: TObject);
    Procedure StatusChange(Sender: TObject);
    Procedure CoordChange(Sender: TObject);
    Procedure AltAZChange(Sender: TObject);
    procedure NewMessage(txt: string);
    Procedure LSTChange(Sender: TObject);
    Procedure PierSideChange(Sender: TObject);
  public
    { public declarations }
  end;

var
  f_eqmod: Tf_eqmod;

implementation

{$R *.lfm}

{ Tf_eqmod }

procedure Tf_eqmod.FormCreate(Sender: TObject);
begin
 Notebook1.PageIndex:=0;
 configfile:=GetAppConfigFileUTF8(false,true,true);
 config:=TXMLConfig.Create(self);
 config.Filename:=configfile;
 indiserver:=config.GetValue('/INDI/server','localhost');
 indiserverport:=config.GetValue('/INDI/serverport','7624');
 indidevice:=config.GetValue('/INDI/device','EQMod Mount');
 indideviceport:=config.GetValue('/INDI/deviceport','/dev/ttyUSB0');
 eqmod:=T_indieqmod.create;
 GUIready:=false;
end;

procedure Tf_eqmod.FormDestroy(Sender: TObject);
begin
  config.Free;
end;

procedure Tf_eqmod.FormShow(Sender: TObject);
begin
 eqmod.onDestroy:=@eqmodDestroy;
 eqmod.onMsg:=@NewMessage;
 eqmod.onCoordChange:=@CoordChange;
 eqmod.onAltAZChange:=@AltAZChange;
 eqmod.onStatusChange:=@StatusChange;
 eqmod.onLSTChange:=@LSTChange;
 eqmod.onPierSideChange:=@PierSideChange;
 if indiserver<>'' then eqmod.indiserver:=indiserver;
 if indiserverport<>'' then eqmod.indiserverport:=indiserverport;
 if indidevice<>'' then eqmod.indidevice:=indidevice;
 eqmod.indideviceport:=indideviceport;
 eqmod.Connect;
end;

procedure Tf_eqmod.IndiBtnClick(Sender: TObject);
begin
 if not GUIready then begin
    f_indigui:=Tf_indigui.Create(self);
    f_indigui.onDestroy:=@GUIdestroy;
    f_indigui.IndiServer:=indiserver;
    f_indigui.IndiPort:=indiserverport;
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
 Close;
end;

Procedure Tf_eqmod.StatusChange(Sender: TObject);
begin
case eqmod.Status of
  devDisconnected:begin
                      //f_devicesconnection.LabelMount.Font.Color:=clRed;
                  end;
  devConnecting:  begin
                      NewMessage('Connecting mount...');
                      //f_devicesconnection.LabelMount.Font.Color:=clOrange;
                   end;
  devConnected:   begin
                      NewMessage('Mount connected');
                      //f_devicesconnection.LabelMount.Font.Color:=clGreen;
                      CoordChange(Sender);
                      LSTChange(Sender);
                      PierSideChange(Sender);
                   end;
end;
//CheckConnectionStatus;
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


end.

