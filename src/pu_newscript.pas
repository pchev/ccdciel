unit pu_newscript;

{$mode objfpc}{$H+}

interface

uses  u_translation,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { Tf_newscript }

  Tf_newscript = class(TForm)
    BtnCancel: TButton;
    BtnOK: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    ScriptLanguage: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure ScriptLanguageClick(Sender: TObject);
  private
    procedure SetLang;
  public

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
end;

procedure Tf_newscript.FormCreate(Sender: TObject);
begin
 SetLang;
end;

procedure Tf_newscript.ScriptLanguageClick(Sender: TObject);
begin
{$if defined(CPUARM) or defined(CPUAARCH64)}
  ScriptLanguage.ItemIndex:=1;  // Pascalscript do not work on ARM
{$endif}
end;

end.

