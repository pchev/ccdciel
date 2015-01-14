unit pu_viewtext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { Tf_viewtext }

  Tf_viewtext = class(TForm)
    Memo1: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  f_viewtext: Tf_viewtext;

implementation

{$R *.lfm}

end.

