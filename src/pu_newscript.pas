unit pu_newscript;

{$mode objfpc}{$H+}

interface

uses  u_translation, pu_downloadscript,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { Tf_newscript }

  Tf_newscript = class(TForm)
    BtnCancel: TButton;
    BtnOK: TButton;
    ButtonDownload: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PanelScriptname: TPanel;
    PanelBottom: TPanel;
    PanelLanguage: TPanel;
    PanelDownload: TPanel;
    ScriptLanguage: TRadioGroup;
    procedure ButtonDownloadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ScriptLanguageClick(Sender: TObject);
  private
    FDownloaded: boolean;
    procedure SetLang;
  public
    property Downloaded: boolean read FDownloaded;
  end;

var
  f_newscript: Tf_newscript;

implementation

{$R *.lfm}

{ Tf_newscript }

procedure Tf_newscript.SetLang;
begin
  label1.caption:=rsNewScript;
  label2.Caption:=rsScriptLangua;
  BtnOK.Caption:=rsOK;
  BtnCancel.Caption:=rsCancel;
  Label3.Caption:=rsExampleAndTe;
  ButtonDownload.Caption:=rsDownload;
end;

procedure Tf_newscript.FormCreate(Sender: TObject);
begin
 SetLang;
end;

procedure Tf_newscript.FormShow(Sender: TObject);
begin
  FDownloaded:=false;
end;

procedure Tf_newscript.ButtonDownloadClick(Sender: TObject);
begin
  f_downloadscript:=Tf_downloadscript.Create(self);
  try
  f_downloadscript.ShowModal;
  if  f_downloadscript.ModalResult=mrOK  then begin
    edit1.Text:=f_downloadscript.Scriptname;
    FDownloaded:=true;
    ModalResult:=mrOK;
  end;
  finally
    f_downloadscript.Free;
  end;
end;

procedure Tf_newscript.ScriptLanguageClick(Sender: TObject);
begin
{$if defined(CPUARM) or defined(CPUAARCH64)}
  ScriptLanguage.ItemIndex:=1;  // Pascalscript do not work on ARM
{$endif}
end;

end.

