unit pu_msgtabs;

{$mode objfpc}{$H+}

interface

uses  UScaleDPI, u_translation,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls;

type

  { Tf_msgtabs }

  Tf_msgtabs = class(TForm)
    TabControl1: TTabControl;
    procedure FormCreate(Sender: TObject);

  private
    procedure SetLang;
  public
  end;

var
  f_msgtabs: Tf_msgtabs;

implementation

{$R *.lfm}

{ Tf_msgtabs }



procedure Tf_msgtabs.FormCreate(Sender: TObject);
begin
  {$ifdef lclcocoa}
  TabControl1.Height:=44;
  {$endif}
  ScaleDPI(Self);
  Setlang;
end;

procedure Tf_msgtabs.SetLang;
begin
  TabControl1.Tabs[0]:=rsSummary;
  TabControl1.Tabs[1]:=rsCommands;
  TabControl1.Tabs[2]:=rsDetails;
end;

end.

