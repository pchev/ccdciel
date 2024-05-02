unit pu_downloadscript;

{$mode ObjFPC}{$H+}

interface

uses u_global, u_utils, u_translation,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls, ExtCtrls, downloaddialog;

type

  { Tf_downloadscript }

  Tf_downloadscript = class(TForm)
    ButtonDownload: TButton;
    ButtonCancel: TButton;
    DownloadDialog1: TDownloadDialog;
    PanelBottom: TPanel;
    PanelTitle: TPanel;
    StringGrid1: TStringGrid;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonDownloadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FscriptName: string;
    procedure SetLang;
    procedure LoadScriptList;
    function DownloadScript(fn,dfn: string): boolean;
  public
    property Scriptname: string Read FscriptName;
  end;

var
  f_downloadscript: Tf_downloadscript;

implementation

{$R *.lfm}

{ Tf_downloadscript }

procedure Tf_downloadscript.FormCreate(Sender: TObject);
begin
  SetLang;
end;

procedure Tf_downloadscript.SetLang;
begin
  Caption:=rsDownload;
  PanelTitle.Caption:=rsSelectTheScr;
  StringGrid1.Columns[0].Title.Caption:=rsGroup;
  StringGrid1.Columns[1].Title.Caption:=rsScript;
  StringGrid1.Columns[2].Title.Caption:=rsDescription;
  ButtonCancel.Caption:=rsCancel;
  ButtonDownload.Caption:=rsDownload;
end;

procedure Tf_downloadscript.FormShow(Sender: TObject);
begin
  FscriptName:='';
  LoadScriptList;
end;

procedure Tf_downloadscript.LoadScriptList;
var f: textfile;
    fn,buf,msg: string;
    l: TStringList;
    ft: TDateTime;
    doDownload: boolean;
begin
  msg:='';
  fn:=slash(ConfigDir)+'script.list';
  doDownload:=true;
  if FileExists(fn) then begin
    if FileAge(fn,ft) then begin
      doDownload:=(ft<(now-1));
    end;
  end;
  if doDownload then begin
    DownloadDialog1.ConfirmDownload:=false;
    DownloadDialog1.QuickCancel:=FileExists(fn);
    DownloadDialog1.URL:=URL_SCRIPTLIST;
    DownloadDialog1.SaveToFile:=fn;
    DownloadDialog1.Execute;
    msg:=DownloadDialog1.ResponseText;
  end;
  StringGrid1.RowCount:=1;
  if FileExists(fn) then begin
    AssignFile(f,fn);
    Reset(f);
    l:=TStringList.Create;
    repeat
      ReadLn(f,buf);
      Splitarg(buf,';',l);
      StringGrid1.RowCount:=StringGrid1.RowCount+1;
      StringGrid1.Cells[0,StringGrid1.RowCount-1]:=l[0];
      StringGrid1.Cells[1,StringGrid1.RowCount-1]:=l[1];
      StringGrid1.Cells[2,StringGrid1.RowCount-1]:=l[2];
    until eof(f);
    CloseFile(f);
    l.Free;
  end
  else begin
    msg:='Error downloading file script.list'+crlf+msg;
    ShowMessage(msg);
  end;
end;

procedure Tf_downloadscript.ButtonDownloadClick(Sender: TObject);
var x: integer;
    fn,dfn: string;
begin
  FscriptName:='';
  x:=StringGrid1.Selection.Top;
  fn:=trim(StringGrid1.Cells[1,x])+'.script';
  dfn:=slash(ConfigDir)+fn;
  if FileExists(dfn) then begin
    if MessageDlg(format(rsScriptAlread,[fn]),mtConfirmation,mbYesNo,0)=mrNo
       then exit;
  end;
  if DownloadScript(fn,dfn) then  begin
    FscriptName:=trim(StringGrid1.Cells[1,x]);
    ModalResult:=mrOK;
  end;
end;

function Tf_downloadscript.DownloadScript(fn,dfn: string): boolean;
begin
  result:=false;
  DownloadDialog1.ConfirmDownload:=false;
  DownloadDialog1.QuickCancel:=false;
  DownloadDialog1.URL:=URL_SCRIPTDOWNLOAD+fn;
  DownloadDialog1.SaveToFile:=dfn;
  result:=DownloadDialog1.Execute;
  if not Result then ShowMessage(DownloadDialog1.ResponseText);
end;

procedure Tf_downloadscript.ButtonCancelClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;



end.

