unit pu_pascaleditor;

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
  SynHighlighterPas, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, ActnList, StdActns, Buttons, ComCtrls, uPSComponent, uPSDebugger,
  uPSRuntime, SynEditMarks, SynEditTypes, SynHighlighterPython;

type

  { Tf_pascaleditor }

  Tf_pascaleditor = class(TForm)
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
    SynPasSyn1: TSynPasSyn;
    SynPythonSyn1: TSynPythonSyn;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure Button4Click(Sender: TObject);
    procedure ButtonPauseClick(Sender: TObject);
    procedure ButtonRunClick(Sender: TObject);
    procedure ButtonStepIntoClick(Sender: TObject);
    procedure ButtonStepOverClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SynEdit1GutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark);
    procedure SynEdit1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SynEdit1SpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
    procedure SynEdit1StatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure ButtonRemoveBreakpoints(Sender: TObject);
  private
    { private declarations }
    Fdbgscr: TPSScriptDebugger;
    FDebugResume: Boolean;
    FActiveLine: integer;
    FScriptName: string;
    FScriptType: TScriptType;
    procedure SetScriptName(value:string);
    procedure SetScriptType(value:TScriptType);
    procedure DebugLineInfo(Sender: TObject; const fn: String; Pos, Row, Col: Cardinal);
    procedure DebugBreakpoint(Sender: TObject; const fn: String; Pos, Row, Col: Cardinal);
    procedure DebugIdle(Sender: TObject);
    procedure Startdebug;
  public
    { public declarations }
    procedure SetLang;
    property DebugScript: TPSScriptDebugger read Fdbgscr write Fdbgscr;
    property ScriptName: string read FScriptName write SetScriptName;
    property ScriptType: TScriptType read FScriptType write SetScriptType;
  end;

var
  f_pascaleditor: Tf_pascaleditor;

implementation

const
  isRunningOrPaused = [isRunning, isPaused];

{$R *.lfm}

procedure Tf_pascaleditor.FormCreate(Sender: TObject);
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

procedure Tf_pascaleditor.FormShow(Sender: TObject);
begin
Fdbgscr.OnLineInfo:=@DebugLineInfo;
Fdbgscr.OnBreakpoint:=@DebugBreakpoint;
Fdbgscr.OnIdle:=@DebugIdle;
FActiveLine := 0;
DebugMemo.Clear;
SynEdit1.Modified:=False;
end;

procedure Tf_pascaleditor.SetLang;
begin
  Caption:=rsScriptEditor;
  Button1.Caption:=rsSave;
  Button2.Caption:=rsCancel;
  ToolButton1.Hint:=rsRun;
  ToolButton2.Hint:=rsPause;
  ToolButton3.Hint:=rsStop;
  ToolButton4.Hint:=rsStepInto;
  ToolButton5.Hint:=rsStepOver;
  ToolButton6.Hint:=rsRemoveAllBre;
  label2.Caption:=rsScriptArgume+':';
end;

procedure Tf_pascaleditor.SetScriptName(value:string);
begin
  if FScriptName<>value then ButtonRemoveBreakpoints(nil);
  FScriptName:=value;
  Caption:=rsScriptEditor+': '+FScriptName;
end;

procedure Tf_pascaleditor.SetScriptType(value:TScriptType);
begin
  FScriptType:=value;
  if FScriptType=stPascal then begin
    ToolButton2.Visible:=true;
    ToolButton4.Visible:=true;
    ToolButton5.Visible:=true;
    ToolButton6.Visible:=true;
    ToolButton4.Hint:=rsStepInto;
    SynEdit1.Highlighter:=SynPasSyn1;
  end
  else begin
    ToolButton2.Visible:=false;
    ToolButton4.Visible:=true;
    ToolButton5.Visible:=false;
    ToolButton6.Visible:=false;
    ToolButton4.Hint:='Debug';
    SynEdit1.Highlighter:=SynPythonSyn1;
  end;
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

procedure Tf_pascaleditor.Startdebug;
var i: integer;
   ok: boolean;
begin
DebugMemo.Clear;
Fdbgscr.Script.Assign(SynEdit1.Lines);
ok:=Fdbgscr.Compile;
if ok then begin
  DebugMemo.Lines.Add('running...');
  Application.ProcessMessages;
  Fdbgscr.Exec.DebugEnabled:=true;
  Fdbgscr.StepInto;
  Fdbgscr.Execute;
  FActiveLine := 0;
  SynEdit1.Refresh;
end else begin
  for i:=0 to Fdbgscr.CompilerMessageCount-1 do begin
     DebugMemo.Lines.Add('Error: '+ Fdbgscr.CompilerErrorToStr(i));
     if i=0 then begin
       SynEdit1.CaretXY := GetErrorRowCol(Fdbgscr.CompilerErrorToStr(0));
       FActiveLine:=SynEdit1.CaretY;
       SynEdit1.Refresh;
     end;
  end;
end;
end;

procedure Tf_pascaleditor.ButtonRunClick(Sender: TObject);
var i: integer;
    fn,args:string;
begin
if FScriptType=stPascal then begin
  if Fdbgscr.Running then
    FDebugResume:=true
  else
    Startdebug;
end
else if FScriptType=stPython then begin
  fn:=slash(ConfigDir)+'tmpscript';
  SynEdit1.Lines.SaveToFile(fn);
  args:=trim(params.Text);
  DebugMemo.Clear;
  DebugMemo.Lines.Add('running...');
  Application.ProcessMessages;
  f_scriptengine.RunPython(PythonCmd, fn, slash(ScriptsDir),args);
  for i:=0 to f_scriptengine.PythonOutput.Count-1 do
     DebugMemo.Lines.Add(f_scriptengine.PythonOutput[i]);
  DebugMemo.Lines.Add('Exit code: '+inttostr(f_scriptengine.PythonResult));
end;
end;

procedure Tf_pascaleditor.ButtonStepIntoClick(Sender: TObject);
var fn,args:string;
begin
if FScriptType=stPascal then begin
  if Fdbgscr.Exec.Status in isRunningOrPaused then
    Fdbgscr.StepInto
  else
  begin
    Startdebug;
  end;
end
else if FScriptType=stPython then begin
  fn:=slash(ConfigDir)+'tmpscript';
  SynEdit1.Lines.SaveToFile(fn);
  args:=trim(params.Text);
  DebugMemo.Clear;
  DebugMemo.Lines.Add('debugging...');
  Application.ProcessMessages;
  f_scriptengine.RunPython(PythonCmd, fn, slash(ScriptsDir),args,true);
  DebugMemo.Lines.Add('Exit code: '+inttostr(f_scriptengine.PythonResult));
end;
end;

procedure Tf_pascaleditor.ButtonPauseClick(Sender: TObject);
begin
if Fdbgscr.Exec.Status = isRunning then
  begin
  Fdbgscr.Pause;
  Fdbgscr.StepInto;
  end;
end;

procedure Tf_pascaleditor.Button4Click(Sender: TObject);
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

procedure Tf_pascaleditor.ButtonStepOverClick(Sender: TObject);
begin
if Fdbgscr.Exec.Status in isRunningOrPaused then
  Fdbgscr.StepOver
else
begin
  Startdebug;
end;
end;

procedure Tf_pascaleditor.ButtonStopClick(Sender: TObject);
begin
if FScriptType=stPascal then begin
  if Fdbgscr.Exec.Status in isRunningOrPaused then
    Fdbgscr.Stop;
end
else if FScriptType=stPython then begin
  f_scriptengine.StopPython;
end;
end;

procedure Tf_pascaleditor.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
if SynEdit1.Modified and (ModalResult<>mrOK) then begin
   CanClose:=(MessageDlg('Abandon your changes?', mtConfirmation, mbYesNo, 0)=mrYes);
end;
end;

procedure Tf_pascaleditor.ButtonRemoveBreakpoints(Sender: TObject);
var i,l: integer;
begin
  for i:=0 to SynEdit1.Marks.Count-1 do begin
    l:=SynEdit1.Marks[i].Line;
    if Fdbgscr.HasBreakPoint('',l) then Fdbgscr.ClearBreakPoint('',l);
    SynEdit1.Marks[i].Visible:=false;
  end;
end;

procedure Tf_pascaleditor.SynEdit1GutterClick(Sender: TObject; X, Y, Line: integer; mark: TSynEditMark);
var  m: TSynEditMark;
begin
if FScriptType=stPascal then begin
  if SynEdit1.Marks.Line[Line]<>nil then m:=SynEdit1.Marks.Line[Line][0] else m:=nil;
  if m=nil then begin
    m := TSynEditMark.Create(SynEdit1);
    m.Line := Line;
    m.ImageList := ImageList1;
    m.Visible := false;
    SynEdit1.Marks.Add(m);
  end;
  if m.Visible then begin
    m.Visible := false;
    if Fdbgscr.HasBreakPoint('',Line) then Fdbgscr.ClearBreakPoint('',Line);
  end else begin
    m.ImageIndex := 0;
    m.Visible := true;
    Fdbgscr.SetBreakPoint('',Line);
  end;
end;
end;

procedure Tf_pascaleditor.SynEdit1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var pt:Tpoint;
    str,val:string;
begin
if (FScriptType=stPascal) and (Fdbgscr.Exec.Status in isRunningOrPaused) then begin
  if SynEdit1.SelText<>'' then
    str:=SynEdit1.SelText
  else begin
    pt:=SynEdit1.PixelsToRowColumn(point(x,y));
    str:=SynEdit1.GetWordAtRowCol(pt);
  end;
  if str<>'' then begin
    val:=Fdbgscr.GetVarContents(str);
    DebugMemo.Lines.Add(str+' = '+val);
    DebugMemo.SelStart:=length(DebugMemo.Text);
  end;
end;
end;

procedure Tf_pascaleditor.SynEdit1SpecialLineColors(Sender: TObject;
  Line: integer; var Special: boolean; var FG, BG: TColor);
begin
  if (FScriptType=stPascal) and (Fdbgscr.Exec.Status in isRunningOrPaused) then begin
    if Fdbgscr.HasBreakPoint('', Line) then
    begin
      Special := True;
      if Line = FActiveLine then
      begin
        BG := clRed;
        FG := clWhite;
      end else
      begin
        BG := clWindow;
        FG := clWindowText;
      end;
    end else
    if Line = FActiveLine then
    begin
      Special := True;
      BG := clBlue;
      FG := clWhite;
    end else Special := False;
  end else if Line = FActiveLine then
  begin
    Special := True;
    BG := clYellow;
    FG := clWindowText;
  end else Special := False;
end;

procedure Tf_pascaleditor.SynEdit1StatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  label1.Caption := IntToStr(SynEdit1.CaretY)+':'+IntToStr(SynEdit1.CaretX)
end;

procedure Tf_pascaleditor.DebugLineInfo(Sender: TObject; const fn: String; Pos, Row, Col: Cardinal);
begin
  if (Fdbgscr.Exec.DebugMode <> dmRun) and (Fdbgscr.Exec.DebugMode <> dmStepOver) then
  begin
    FActiveLine := Row;
    if (FActiveLine < SynEdit1.TopLine +2) or (FActiveLine > SynEdit1.TopLine + SynEdit1.LinesInWindow -2) then
    begin
      SynEdit1.TopLine := FActiveLine - (SynEdit1.LinesInWindow div 2);
    end;
    SynEdit1.CaretY := FActiveLine;
    SynEdit1.CaretX := 1;
    SynEdit1.Refresh;
  end
  else
    Application.ProcessMessages;
end;

procedure Tf_pascaleditor.DebugBreakpoint(Sender: TObject; const fn: String; Pos, Row, Col: Cardinal);
begin
 FDebugResume:=false;
 Fdbgscr.Pause;
 DebugMemo.Lines.Add('breakpoint row:'+inttostr(row)+' col:'+inttostr(col));
 FActiveLine := Row;
 if (FActiveLine < SynEdit1.TopLine +2) or (FActiveLine > SynEdit1.TopLine + SynEdit1.LinesInWindow -2) then
 begin
   SynEdit1.TopLine := FActiveLine - (SynEdit1.LinesInWindow div 2);
 end;
 SynEdit1.CaretY := FActiveLine;
 SynEdit1.CaretX := 1;
 SynEdit1.Refresh;
 Application.ProcessMessages;
end;

procedure Tf_pascaleditor.DebugIdle(Sender: TObject);
begin
Application.ProcessMessages;
if FDebugResume then begin
   FDebugResume:=false;
   Fdbgscr.Resume;
   FActiveLine := 0;
   SynEdit1.Refresh;
end;
end;

end.

