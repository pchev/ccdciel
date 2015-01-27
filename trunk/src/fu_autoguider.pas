unit fu_autoguider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

type

  { Tf_autoguider }

  Tf_autoguider = class(TFrame)
    BtnConnect: TButton;
    Panel1: TPanel;
    Status: TEdit;
    StaticText1: TStaticText;
    procedure BtnConnectClick(Sender: TObject);
  private
    { private declarations }
    FonConnect: TNotifyEvent;
  public
    { public declarations }
    property onConnect: TNotifyEvent read FonConnect write FonConnect;
  end;

implementation

{$R *.lfm}

{ Tf_autoguider }

procedure Tf_autoguider.BtnConnectClick(Sender: TObject);
begin
   if Assigned(FonConnect) then FonConnect(self);
end;

end.

