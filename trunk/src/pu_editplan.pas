unit pu_editplan;

{$mode objfpc}{$H+}

interface

uses u_ccdconfig, u_global, u_utils,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { Tf_EditPlan }

  Tf_EditPlan = class(TForm)
    BtnClose: TButton;
    BtnAddStep: TButton;
    Step: TComboBox;
    FrameType: TComboBox;
    Filter: TComboBox;
    Binning: TComboBox;
    Exposure: TEdit;
    Count: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    PlanName: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  f_EditPlan: Tf_EditPlan;

implementation

{$R *.lfm}

{ Tf_EditPlan }

procedure Tf_EditPlan.FormShow(Sender: TObject);
var pfile: TCCDconfig;
    fn,str: string;
    i:integer;
begin
  fn:=slash(ConfigDir)+PlanName.Caption+'.plan';
  if FileExistsUTF8(fn) then begin
    pfile:=TCCDconfig.Create(self);
    pfile.Filename:=fn;
    str:=pfile.GetValue('/FrameType','Light');
    i:=FrameType.Items.IndexOf(str);
    if i<0 then i:=0;
    FrameType.ItemIndex:=i;
    str:=pfile.GetValue('/Binning','1x1');
    i:=Binning.Items.IndexOf(str);
    if i<0 then i:=0;
    Binning.ItemIndex:=i;
    str:=pfile.GetValue('/Filter','');
    i:=Filter.Items.IndexOf(str);
    if i<0 then i:=0;
    Filter.ItemIndex:=i;
    Exposure.Text:=pfile.GetValue('/Exposure','1');
    Count.Text:=pfile.GetValue('/Count','1');
    pfile.Free;
  end;
end;

procedure Tf_EditPlan.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var pfile: TCCDconfig;
    fn,str: string;
    i:integer;
begin
  fn:=slash(ConfigDir)+PlanName.Caption+'.plan';
  pfile:=TCCDconfig.Create(self);
  pfile.Filename:=fn;
  pfile.Clear;
  pfile.SetValue('/FrameType',FrameType.Text);
  pfile.SetValue('/Binning',Binning.Text);
  pfile.SetValue('/Filter',Filter.Text);
  pfile.SetValue('/Exposure',Exposure.Text);
  pfile.SetValue('/Count',Count.Text);
  pfile.Flush;
  pfile.Free;
  ModalResult:=mrOK;
end;

end.

