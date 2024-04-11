unit fu_script;

{$mode objfpc}{$H+}

{
Copyright (C) 2016 Patrick Chevalley

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

interface

uses  u_global, u_utils, pu_scriptengine, pu_pascaleditor, UScaleDPI, u_translation,
  fu_capture, fu_preview, cu_mount, cu_camera, cu_autoguider, cu_astrometry, pu_newscript,
  LCLType, Classes, Dialogs, SysUtils, FileUtil, Forms, Graphics, Controls, StdCtrls, ExtCtrls;

type

  { Tf_script }

  Tf_script = class(TFrame)
    BtnRun: TButton;
    BtnEdit: TButton;
    BtnNew: TButton;
    BtnStop: TButton;
    BtnCopy: TButton;
    ButtonParam: TButton;
    ComboBoxScript: TComboBox;
    led: TShape;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    ScriptParam: TComboBox;
    Title: TLabel;
    procedure BtnCopyClick(Sender: TObject);
    procedure BtnScriptClick(Sender: TObject);
    procedure BtnRunClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure ButtonParamClick(Sender: TObject);
    procedure ComboBoxScriptChange(Sender: TObject);
    procedure ComboBoxScriptKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
    Fcamera: T_camera;
    Fcapture: Tf_capture;
    Fpreview: Tf_preview;
    Fmount: T_mount;
    Fautoguider: T_autoguider;
    Fastrometry: TAstrometry;
    FonMsg: TNotifyMsg;
    procedure msg(txt:string);
    function GetScriptList: TStrings;
    procedure AddMRU(txt:string);
    procedure SaveMRU;
    procedure LoadMRU;
    function  GetScriptName: string;
    procedure SetScriptName(value: string);
 public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    procedure LoadScriptList;
    procedure SetScriptList(sl:string);
    procedure RunStartupScript;
    procedure RunShutdownScript;
    procedure RunConnectedScript;
    procedure RunDisconnectedScript;
    property Camera: T_camera read Fcamera write Fcamera;
    property Preview: Tf_preview read Fpreview write Fpreview;
    property Capture: Tf_capture read Fcapture write Fcapture;
    property Mount: T_mount read Fmount write Fmount;
    property Autoguider: T_autoguider read Fautoguider write Fautoguider;
    property Astrometry: TAstrometry read Fastrometry write Fastrometry;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property ScriptList: TStrings read GetScriptList;
    property ScriptName: string read GetScriptName write SetScriptName;
  end;

implementation

{$R *.lfm}
uses LazFileUtils;

const MaxMRU=10;

{ Tf_script }

constructor Tf_script.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 Panel1.ChildSizing.LeftRightSpacing:=8;
 Panel1.ChildSizing.VerticalSpacing:=4;
 {$endif}
 ScaleDPI(Self);
 SetLang;
 led.Canvas.AntialiasingMode:=amOn;
end;

destructor  Tf_script.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_script.SetLang;
begin
  Title.Caption:=rsRunScript;
  BtnRun.Caption:=rsRun;
  BtnEdit.Caption:=rsEdit;
  BtnNew.Caption:=rsNew;
  BtnStop.Caption:=rsStop;
  BtnCopy.Caption:=rsCopy;
  ButtonParam.Hint:=rsScriptArgume;
  ScriptParam.Hint:=rsScriptArgume;
  if f_pascaleditor<>nil then f_pascaleditor.SetLang;
end;

procedure Tf_script.msg(txt:string);
begin
  if Assigned(FonMsg) then FonMsg(txt);
end;

procedure Tf_script.AddMRU(txt:string);
var i: integer;
begin
  i := ScriptParam.Items.IndexOf(txt);
  if (i < 0) and (ScriptParam.Items.Count >= MaxMRU) then
    i := MaxMRU - 1;
  if i >= 0 then
    ScriptParam.Items.Delete(i);
  ScriptParam.Items.Insert(0, txt);
  ScriptParam.ItemIndex:=ScriptParam.Items.Count-1;
  ScriptParam.ItemIndex := 0;
  SaveMRU;
end;

procedure Tf_script.SaveMRU;
var fn,sname: string;
    f: Textfile;
    i: integer;
begin
  i:=ComboBoxScript.ItemIndex;
  sname:=ComboBoxScript.Items[i];
  fn:=slash(ConfigDir)+'param_'+sname+'.lst';
  AssignFile(f,fn);
  Rewrite(f);
  for i:=0 to ScriptParam.Items.Count-1 do begin
    WriteLn(f,ScriptParam.Items[i]);
  end;
  CloseFile(f);
end;

procedure Tf_script.LoadMRU;
var fn,sname,buf: string;
    f: Textfile;
    i: integer;
begin
  i:=ComboBoxScript.ItemIndex;
  sname:=ComboBoxScript.Items[i];
  fn:=slash(ConfigDir)+'param_'+sname+'.lst';
  if FileExists(fn) then begin
    panel5.Visible:=true;
    ScriptParam.Clear;
    AssignFile(f,fn);
    Reset(f);
    repeat
      ReadLn(f,buf);
      ScriptParam.Items.Add(buf);
    until eof(f);
    CloseFile(f);
    ScriptParam.ItemIndex:=0;
  end
  else begin
    ScriptParam.Clear;
    panel5.Visible:=false;
  end;
end;

function  Tf_script.GetScriptName: string;
begin
  result:=ComboBoxScript.Text;
end;

procedure Tf_script.SetScriptName(value: string);
var i: integer;
begin
  i:=ComboBoxScript.Items.IndexOf(value);
  if i >= 0 then begin
    ComboBoxScript.ItemIndex:=i;
    LoadMRU;
  end
  else
    ComboBoxScript.Text:=value;
end;

procedure Tf_script.ComboBoxScriptChange(Sender: TObject);
begin
  LoadMRU;
end;

procedure Tf_script.RunStartupScript;
var path,sname: string;
begin
  // Always run user customized script, never try to run the distribution sample script
  path:=ScriptDir[1].path;
  sname:='startup';
  if FileExistsUTF8(slash(path)+sname+'.script') then begin
    f_scriptengine.RunScript(sname,path,'');
  end;
end;

procedure Tf_script.RunShutdownScript;
var path,sname: string;
begin
  // Always run user customized script, never try to run the distribution sample script
  path:=ScriptDir[1].path;
  sname:='shutdown';
  if FileExistsUTF8(slash(path)+sname+'.script') then begin
    f_scriptengine.RunScript(sname,path,'');
  end;
end;

procedure Tf_script.RunConnectedScript;
var path,sname: string;
begin
  // Always run user customized script, never try to run the distribution sample script
  path:=ScriptDir[1].path;
  sname:='connected';
  if FileExistsUTF8(slash(path)+sname+'.script') then begin
    f_scriptengine.RunScript(sname,path,'');
  end;
end;

procedure Tf_script.RunDisconnectedScript;
var path,sname: string;
begin
  // Always run user customized script, never try to run the distribution sample script
  path:=ScriptDir[1].path;
  sname:='disconnected';
  if FileExistsUTF8(slash(path)+sname+'.script') then begin
    f_scriptengine.RunScript(sname,path,'');
  end;
end;

procedure Tf_script.BtnRunClick(Sender: TObject);
var sname,args: string;
    scdir:TScriptDir;
    i: integer;
begin
  i:=ComboBoxScript.ItemIndex;
  if i>=0 then begin
    if f_scriptengine.ScriptRunning then begin
      msg(rsAnotherScrip);
    end else begin
      sname:=ComboBoxScript.Items[i];
      scdir:=TScriptDir(ComboBoxScript.Items.Objects[i]);
      if panel5.Visible then begin
        args:=trim(ScriptParam.text);
        AddMRU(args);
      end
      else
        args:='';
      if (sname='')or(scdir=nil) then exit;
      if not FileExistsUTF8(slash(scdir.path)+sname+'.script') then begin
        msg(Format(rsFileNotFound,[sname+'.script']));
        exit;
      end;
      f_scriptengine.RunScript(sname,scdir.path,args);
   end;
  end
  else msg(rsPleaseSelect);
end;

procedure Tf_script.BtnStopClick(Sender: TObject);
begin
  if f_scriptengine.ScriptRunning then begin
    f_scriptengine.StopScript;
  end
  else msg(rsNoScriptAreR);
end;

procedure Tf_script.ButtonParamClick(Sender: TObject);
begin
  panel5.Visible:=not panel5.Visible;
end;

procedure Tf_script.ComboBoxScriptKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var txt,fn: string;
    scdir:TScriptDir;
    i:integer;
begin
  if key=VK_DELETE then begin
   i:=ComboBoxScript.ItemIndex;
   if i<0 then exit;
   txt:=ComboBoxScript.Items[i];
   scdir:=TScriptDir(ComboBoxScript.Items.Objects[i]);
   if (txt='')or(scdir=nil) then exit;
   if scdir<>ScriptDir[1] then exit;
   fn:=scdir.path+txt+'.script';
   if MessageDlg(Format(rsDoYouWantToD, [fn]), mtConfirmation, mbYesNo, 0)=
     mrYes then begin
      DeleteFileUTF8(fn);
      LoadScriptList;
   end;
  end;
end;

procedure Tf_script.BtnCopyClick(Sender: TObject);
var txt,fn1,fn2: string;
    scdir:TScriptDir;
    i:integer;
begin
  i:=ComboBoxScript.ItemIndex;
  if i<0 then exit;
  txt:=ComboBoxScript.Items[i];
  scdir:=TScriptDir(ComboBoxScript.Items.Objects[i]);
  if (txt='')or(scdir=nil) then exit;
  fn1:=scdir.path+txt+'.script';
  txt:=FormEntry(self, rsCopyTo, '');
  if txt='' then exit;
  scdir:=ScriptDir[1];
  fn2:=scdir.path+txt+'.script';
  if FileExistsUTF8(fn2) then begin
     if MessageDlg(Format(rsScriptAlread, [fn2]), mtConfirmation, mbYesNo, 0)<>
       mrYes then
       exit;
  end;
  if CopyFile(fn1,fn2,false) then begin
     LoadScriptList;
     SetScriptList(txt);
     i:=ComboBoxScript.ItemIndex;
     if i<0 then exit;
     if txt=ComboBoxScript.Items[i] then
        BtnScriptClick(self);
  end;
end;

procedure Tf_script.BtnScriptClick(Sender: TObject);
var txt,fn: string;
    scdir:TScriptDir;
    i:integer;
    newscript: boolean;
    s: TStringList;
    ns: Tf_newscript;
    st: TScriptType;
begin
  newscript:=(Sender=BtnNew)or(ComboBoxScript.Text='');
  s:=TStringList.Create;
  if f_pascaleditor=nil then begin
     f_pascaleditor:=Tf_pascaleditor.Create(self);
     f_pascaleditor.DebugScript:=f_scriptengine.dbgscr;
  end;
  f_pascaleditor.ShowHint:=ShowHint;
  if newscript then begin
    s.Clear;
    ns:=Tf_newscript.Create(self);
    FormPos(ns,mouse.CursorPos.x,mouse.CursorPos.y);
    ns.ShowModal;
    if ns.ModalResult<>mrOK then exit;
    txt:=trim(ns.Edit1.text);
    if txt='' then exit;
    st:=TScriptType(ns.ScriptLanguage.ItemIndex+1);
    ns.free;
    scdir:=ScriptDir[1];
    if copy(txt,1,2)='T_' then delete(txt,1,2);
    fn:=scdir.path+txt+'.script';
    if FileExistsUTF8(fn) then begin
       if MessageDlg(Format(rsScriptAlread2, [fn]), mtConfirmation, mbYesNo, 0)=
         mrYes then
         s.LoadFromFile(fn)
       else
         exit;
    end
    else begin
      if st=stPascal then begin
        s.Add('{');
        s.Add('Pascal script for CCDciel');
        s.Add('see: https://www.ap-i.net/ccdciel/en/documentation/script_reference');
        s.Add('}');
        s.Add('begin');
        s.Add('');
        s.Add('end.');
      end
      else if st=stPython then begin
        s.Add('# Python program for CCDciel');
        s.Add('# see: https://www.ap-i.net/ccdciel/en/documentation/jsonrpc_reference');
        s.Add('');
        s.Add('from ccdciel import ccdciel');
        s.Add('');
      end;
    end;
    f_pascaleditor.ScriptName:=txt;
    f_pascaleditor.ScriptType:=st;
  end
  else begin
    i:=ComboBoxScript.ItemIndex;
    if i<0 then exit;
    txt:=ComboBoxScript.Items[i];
    scdir:=TScriptDir(ComboBoxScript.Items.Objects[i]);
    if (txt='')or(scdir=nil) then exit;
    fn:=scdir.path+txt+'.script';
    s.LoadFromFile(fn);
    f_pascaleditor.ScriptType:=f_scriptengine.ScriptType(fn);
    if scdir<>ScriptDir[1] then begin
       if copy(txt,1,2)='T_' then
          delete(txt,1,2)
       else begin
         if txt[1]<>'_' then txt:='_'+txt
       end;
       scdir:=ScriptDir[1];
       fn:=scdir.path+txt+'.script';
       newscript:=true;
       if FileExistsUTF8(fn) then begin
          if MessageDlg(Format(rsScriptAlread3, [fn]), mtConfirmation, mbYesNo, 0)<>mrYes then
            exit;
       end;
    end;
    f_pascaleditor.ScriptName:=txt;
  end;
  f_pascaleditor.SynEdit1.Lines.Assign(s);
  FormPos(f_pascaleditor,mouse.CursorPos.X,mouse.CursorPos.Y);
  f_pascaleditor.ShowModal;
  if f_pascaleditor.ModalResult=mrOK then begin
    s.Assign(f_pascaleditor.SynEdit1.Lines);
    s.SaveToFile(fn);
    if newscript then begin
     LoadScriptList;
     SetScriptList(f_pascaleditor.ScriptName);
    end;
  end;
  s.Free;
end;

procedure Tf_script.LoadScriptList;
var i,k: integer;
    fs : TSearchRec;
    s: TStringlist;
    scr: string;
begin
  s:=TStringlist.Create;
  ComboBoxScript.Clear;
  for k:=1 to MaxScriptDir do begin
    i:=FindFirstUTF8(ScriptDir[k].path+'*.script',0,fs);
    while i=0 do begin
      {$if defined(CPUARM) or defined(CPUAARCH64)}
      if f_scriptengine.ScriptType(ScriptDir[k].path+fs.name)<>stPascal then
      {$endif}
        begin
          scr:=ExtractFileNameOnly(fs.Name);
          if s.IndexOf(scr)<0 then
            s.AddObject(scr,ScriptDir[k]);
        end;
      i:=FindNextUTF8(fs);
    end;
    FindCloseUTF8(fs);
  end;
  s.CustomSort(@ScriptListCompare);
  ComboBoxScript.Items.Assign(s);
  ComboBoxScript.ItemIndex:=0;
  ComboBoxScriptChange(ComboBoxScript);
  s.Free;
end;

procedure Tf_script.SetScriptList(sl:string);
var i:integer;
begin
  if sl='' then exit;
  i:=ComboBoxScript.Items.IndexOf(sl);
  if i>=0 then ComboBoxScript.ItemIndex:=i;
  ComboBoxScriptChange(ComboBoxScript);
end;

function Tf_script.GetScriptList: TStrings;
begin
  result:=ComboBoxScript.Items;
end;

end.

