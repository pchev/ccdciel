unit pu_scripteditor;

{$mode objfpc}{$H+}


{
Copyright (C) 2019 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. 

}

{
 Pascal script editor and debugger.
 The debugger is based on the example in lazarus/components/PascalScript/Samples/Debug/
}

interface

uses  Classes, SysUtils, FileUtil, SynEdit, UScaleDPI, u_translation, u_global, u_utils, pu_scriptengine,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, ActnList, StdActns, Buttons, ComCtrls,
  SynEditTypes, SynHighlighterPython;

type

  { Tf_scripteditor }

  Tf_scripteditor = class(TForm)
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    FontDialog1: TFontDialog;
    Label2: TLabel;
    Panel4: TPanel;
    params: TEdit;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    ImageList1: TImageList;
    DebugMemo: TMemo;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PanelDebug: TPanel;
    PopupMenu1: TPopupMenu;
    SynEdit1: TSynEdit;
    SynPythonSyn1: TSynPythonSyn;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure Button4Click(Sender: TObject);
    procedure ButtonRunClick(Sender: TObject);
    procedure ButtonStepIntoClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SynEdit1SpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
    procedure SynEdit1StatusChange(Sender: TObject; Changes: TSynStatusChanges);
  private
    { private declarations }
    FDebugResume: Boolean;
    FActiveLine: integer;
    FScriptName: string;
    procedure SetScriptName(value:string);
  public
    { public declarations }
    procedure SetLang;
    property ScriptName: string read FScriptName write SetScriptName;
  end;

var
  f_scripteditor: Tf_scripteditor;

implementation


{$R *.lfm}

procedure Tf_scripteditor.FormCreate(Sender: TObject);
begin
  SynEdit1.Font.Name:=EditorFontName;
  SynEdit1.Font.Size:=EditorFontSize;
  if EditorFontStyle>=0 then
    SynEdit1.Font.Style:=[TFontStyle(EditorFontStyle)]
  else
    SynEdit1.Font.Style:=[];
  ScaleDPI(Self);
  FDebugResume:=false;
  SetLang;
end;

procedure Tf_scripteditor.FormShow(Sender: TObject);
begin
FActiveLine := 0;
DebugMemo.Clear;
SynEdit1.Modified:=False;
ToolButton4.Visible:=true;
ToolButton4.Hint:='Debug';
SynEdit1.Highlighter:=SynPythonSyn1;
end;

procedure Tf_scripteditor.SetLang;
begin
  Caption:=rsScriptEditor;
  Button1.Caption:=rsSave;
  Button2.Caption:=rsCancel;
  ToolButton1.Hint:=rsRun;
  ToolButton3.Hint:=rsStop;
  ToolButton4.Hint:=rsStepInto;
  label2.Caption:=rsScriptArgume+':';
end;

procedure Tf_scripteditor.SetScriptName(value:string);
begin
  FScriptName:=value;
  Caption:=rsScriptEditor+': '+FScriptName;
end;

function GetErrorRowCol(const inStr: string): TPoint;
var
  Row:string;
  Col:string;
  p1,p2,p3:integer;
begin
  p1:=Pos('(',inStr);
  p2:=Pos(':',inStr);
  p3:=Pos(')',inStr);
  if (p1>0) and (p2>p1) and (p3>p2) then
  begin
    Row := Copy(inStr, p1+1,p2-p1-1);
    Col := Copy(inStr, p2+1,p3-p2-1);
    Result.X := StrToInt(Trim(Col));
    Result.Y := StrToInt(Trim(Row));
  end
  else
  begin
    Result.X := 1;
    Result.Y := 1;
  end
end;

procedure Tf_scripteditor.ButtonRunClick(Sender: TObject);
var i,n: integer;
    fn,args:string;
begin
  n:=1;
  fn:=slash(ConfigDir)+'tmpscript';
  SynEdit1.Lines.SaveToFile(fn);
  args:=trim(params.Text);
  DebugMemo.Clear;
  DebugMemo.Lines.Add('running...');
  Application.ProcessMessages;
  f_scriptengine.RunPython(PythonCmd, fn, slash(ScriptsDir),args,true,n);
  for i:=0 to f_scriptengine.PythonOutput[n].Count-1 do
     DebugMemo.Lines.Add(f_scriptengine.PythonOutput[n][i]);
  DebugMemo.Lines.Add('Exit code: '+inttostr(f_scriptengine.PythonResult[n]));
end;

procedure Tf_scripteditor.ButtonStepIntoClick(Sender: TObject);
var fn,args:string;
    n: integer;
begin
  fn:=slash(ConfigDir)+'tmpscript';
  SynEdit1.Lines.SaveToFile(fn);
  args:=trim(params.Text);
  DebugMemo.Clear;
  DebugMemo.Lines.Add('debugging...');
  Application.ProcessMessages;
  f_scriptengine.RunPython(PythonCmd, fn, slash(ScriptsDir),args,true,n,true);
  DebugMemo.Lines.Add('Exit code: '+inttostr(f_scriptengine.PythonResult[n]));
end;

procedure Tf_scripteditor.Button4Click(Sender: TObject);
begin
  FontDialog1.Font.Name:=EditorFontName;
  FontDialog1.Font.Size:=EditorFontSize;
  if EditorFontStyle>=0 then
    FontDialog1.Font.Style:=[TFontStyle(EditorFontStyle)]
  else
    FontDialog1.Font.Style:=[];
  if FontDialog1.Execute then begin
    EditorFontName:=FontDialog1.Font.Name;
    EditorFontSize:=FontDialog1.Font.Size;
    if FontDialog1.Font.Style=[] then
      EditorFontStyle:=-1;
    if fsBold in FontDialog1.Font.Style then
      EditorFontStyle:=ord(fsBold)
    else
      EditorFontStyle:=-1;
    SynEdit1.Font.Name:=EditorFontName;
    SynEdit1.Font.Size:=EditorFontSize;
    if EditorFontStyle>=0 then
      SynEdit1.Font.Style:=[TFontStyle(EditorFontStyle)]
    else
      SynEdit1.Font.Style:=[];
  end;
end;

procedure Tf_scripteditor.ButtonStopClick(Sender: TObject);
begin
  f_scriptengine.StopPython;
end;

procedure Tf_scripteditor.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
if SynEdit1.Modified and (ModalResult<>mrOK) then begin
   CanClose:=(MessageDlg('Abandon your changes?', mtConfirmation, mbYesNo, 0)=mrYes);
end;
end;

procedure Tf_scripteditor.SynEdit1SpecialLineColors(Sender: TObject;
  Line: integer; var Special: boolean; var FG, BG: TColor);
begin
  if Line = FActiveLine then
  begin
    Special := True;
    BG := clYellow;
    FG := clWindowText;
  end else Special := False;
end;

procedure Tf_scripteditor.SynEdit1StatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  label1.Caption := IntToStr(SynEdit1.CaretY)+':'+IntToStr(SynEdit1.CaretX)
end;

end.

