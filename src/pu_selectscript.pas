unit pu_selectscript;

{$mode objfpc}{$H+}

interface

uses u_translation, UScaleDPI,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { Tf_selectscript }

  Tf_selectscript = class(TForm)
    BtnCancel: TButton;
    BtnOK: TButton;
    ComboBoxScript: TComboBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel5: TPanel;
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure SetLang;
    procedure Setscript(scn: string);
  end;

var
  f_selectscript: Tf_selectscript;

implementation

{$R *.lfm}

procedure Tf_selectscript.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self);
  SetLang;
end;

procedure Tf_selectscript.SetLang;
begin
  Caption:=rsScript;
  BtnOK.Caption:=rsOK;
  BtnCancel.Caption:=rsCancel;
end;

procedure Tf_selectscript.Setscript(scn: string);
var i: integer;
begin
  ComboBoxScript.ItemIndex:=0;
  for i:=0 to ComboBoxScript.Items.Count-1 do begin
    if ComboBoxScript.Items[i]=scn then begin
       ComboBoxScript.ItemIndex:=i;
       break;
    end;
  end;
end;

end.

