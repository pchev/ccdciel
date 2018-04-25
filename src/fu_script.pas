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
  fu_capture, fu_preview, cu_mount, cu_camera, cu_autoguider, cu_astrometry,
  LCLType, Classes, Dialogs, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

type

  { Tf_script }

  Tf_script = class(TFrame)
    BtnRun: TButton;
    BtnEdit: TButton;
    BtnNew: TButton;
    BtnStop: TButton;
    BtnCopy: TButton;
    ComboBoxScript: TComboBox;
    led: TShape;
    Panel1: TPanel;
    Title: TLabel;
    procedure BtnCopyClick(Sender: TObject);
    procedure BtnScriptClick(Sender: TObject);
    procedure BtnRunClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure ComboBoxScriptKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
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
    procedure SetLang;
 public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure LoadScriptList;
    procedure SetScriptList(sl:string);
    procedure RunStartupScript;
    procedure RunShutdownScript;
    property Camera: T_camera read Fcamera write Fcamera;
    property Preview: Tf_preview read Fpreview write Fpreview;
    property Capture: Tf_capture read Fcapture write Fcapture;
    property Mount: T_mount read Fmount write Fmount;
    property Autoguider: T_autoguider read Fautoguider write Fautoguider;
    property Astrometry: TAstrometry read Fastrometry write Fastrometry;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
  end;

implementation

{$R *.lfm}
uses LazFileUtils;

{ Tf_script }

constructor Tf_script.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 ScaleDPI(Self);
 SetLang;
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
end;

procedure Tf_script.msg(txt:string);
begin
  if Assigned(FonMsg) then FonMsg(txt);
end;

procedure Tf_script.RunStartupScript;
var path,sname: string;
begin
  path:=ScriptDir[1].path;
  sname:='startup';
  if FileExistsUTF8(slash(path)+sname+'.script') then begin
    f_scriptengine.RunScript(sname,path);
  end;
end;

procedure Tf_script.RunShutdownScript;
var path,sname: string;
begin
  path:=ScriptDir[1].path;
  sname:='shutdown';
  if FileExistsUTF8(slash(path)+sname+'.script') then begin
    f_scriptengine.RunScript(sname,path);
  end;
end;

procedure Tf_script.BtnRunClick(Sender: TObject);
var sname: string;
    scdir:TScriptDir;
    i: integer;
begin
  i:=ComboBoxScript.ItemIndex;
  if i>=0 then begin
    if f_scriptengine.scr.Running then begin
      msg(rsAnotherScrip);
    end else begin
      sname:=ComboBoxScript.Items[i];
      scdir:=TScriptDir(ComboBoxScript.Items.Objects[i]);
      if (sname='')or(scdir=nil) then exit;
      f_scriptengine.RunScript(sname,scdir.path);
   end;
  end
  else msg(rsPleaseSelect);
end;

procedure Tf_script.BtnStopClick(Sender: TObject);
begin
  if f_scriptengine.scr.Running then begin
    f_scriptengine.StopScript;
  end
  else msg(rsNoScriptAreR);
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
begin
  newscript:=(Sender=BtnNew)or(ComboBoxScript.Text='');
  s:=TStringList.Create;
  if f_pascaleditor=nil then begin
     f_pascaleditor:=Tf_pascaleditor.Create(self);
     f_pascaleditor.DebugScript:=f_scriptengine.dbgscr;
  end;
  if newscript then begin
    s.Clear;
    txt:=FormEntry(self, rsNewScript, '');
    if txt='' then exit;
    scdir:=ScriptDir[1];
    if copy(txt,1,2)='T_' then delete(txt,1,2);
    fn:=scdir.path+txt+'.script';
    if FileExistsUTF8(fn) then begin
       if MessageDlg(Format(rsScriptAlread2, [fn]), mtConfirmation, mbYesNo, 0)=
         mrYes then
         s.LoadFromFile(fn)
       else
         exit;
    end;
    f_pascaleditor.ScriptName:=txt;
  end
  else begin
    i:=ComboBoxScript.ItemIndex;
    if i<0 then exit;
    txt:=ComboBoxScript.Items[i];
    scdir:=TScriptDir(ComboBoxScript.Items.Objects[i]);
    if (txt='')or(scdir=nil) then exit;
    fn:=scdir.path+txt+'.script';
    s.LoadFromFile(fn);
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
          if MessageDlg(Format(rsScriptAlread3, [fn]), mtConfirmation, mbYesNo,
            0)<>mrYes then
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
begin
  s:=TStringlist.Create;
  ComboBoxScript.Clear;
  for k:=1 to MaxScriptDir do begin
    i:=FindFirstUTF8(ScriptDir[k].path+'*.script',0,fs);
    while i=0 do begin
      s.AddObject(ExtractFileNameOnly(fs.Name),ScriptDir[k]);
      i:=FindNextUTF8(fs);
    end;
    FindCloseUTF8(fs);
  end;
  s.CustomSort(@ScriptListCompare);
  ComboBoxScript.Items.Assign(s);
  ComboBoxScript.ItemIndex:=0;
  s.Free;
end;

procedure Tf_script.SetScriptList(sl:string);
var i:integer;
begin
  if sl='' then exit;
  i:=ComboBoxScript.Items.IndexOf(sl);
  if i>=0 then ComboBoxScript.ItemIndex:=i;
end;

end.

