unit fu_autoguider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

type

  { Tf_autoguider }

  Tf_autoguider = class(TFrame)
    BtnConnect: TButton;
    BtnCal: TButton;
    BtnGuide: TButton;
    BtnDither: TButton;
    Panel1: TPanel;
    Status: TEdit;
    StaticText1: TStaticText;
    procedure BtnCalClick(Sender: TObject);
    procedure BtnConnectClick(Sender: TObject);
    procedure BtnDitherClick(Sender: TObject);
    procedure BtnGuideClick(Sender: TObject);
  private
    { private declarations }
    FonConnect,FonCalibrate,FonGuide,FonDither: TNotifyEvent;
  public
    { public declarations }
    property onConnect: TNotifyEvent read FonConnect write FonConnect;
    property onCalibrate: TNotifyEvent read FonCalibrate write FonCalibrate;
    property onGuide: TNotifyEvent read FonGuide write FonGuide;
    property onDither: TNotifyEvent read FonDither write FonDither;
  end;

implementation

{$R *.lfm}

{ Tf_autoguider }

procedure Tf_autoguider.BtnConnectClick(Sender: TObject);
begin
   if Assigned(FonConnect) then FonConnect(self);
end;

procedure Tf_autoguider.BtnDitherClick(Sender: TObject);
begin
   if Assigned(FonDither) then FonDither(self);
end;

procedure Tf_autoguider.BtnGuideClick(Sender: TObject);
begin
   if Assigned(FonGuide) then FonGuide(self);
end;

procedure Tf_autoguider.BtnCalClick(Sender: TObject);
begin
   if Assigned(FonCalibrate) then FonCalibrate(self);
end;

end.

