unit pu_viewtext;

{$mode objfpc}{$H+}

interface

uses  UScaleDPI,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { Tf_viewtext }

  Tf_viewtext = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  f_viewtext: Tf_viewtext;

implementation

{$R *.lfm}

{ Tf_viewtext }

procedure Tf_viewtext.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self);
end;

end.

