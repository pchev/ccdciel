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
  ScriptLanguage.Caption:=rsScriptLangua;
  BtnOK.Caption:=rsOK;
  BtnCancel.Caption:=rsCancel;
end;

procedure Tf_newscript.FormCreate(Sender: TObject);
begin
 SetLang;
end;

end.

