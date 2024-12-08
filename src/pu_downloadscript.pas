unit pu_downloadscript;

{$mode ObjFPC}{$H+}

interface

uses u_global, u_utils, u_translation, zipper, LazFileUtils, FileUtil,
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
    procedure StringGrid1Resize(Sender: TObject);
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

procedure Tf_downloadscript.StringGrid1Resize(Sender: TObject);
begin
  with StringGrid1 do begin
    Columns[2].Width:=ClientWidth-Columns[0].Width-Columns[1].Width;
  end;
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
    fn,basefn,dfn,configfn: string;
    configexist: boolean;
    FUnZipper: TUnZipper;
begin
  FscriptName:='';
  x:=StringGrid1.Selection.Top;
  fn:=trim(StringGrid1.Cells[1,x]);
  basefn:=ExtractFileNameOnly(fn);
  if uppercase(ExtractFileExt(fn))='.PKG' then begin
    fn:=basefn+'.zip';
    dfn:=slash(ConfigDir)+fn;
  end
  else begin
    fn:=basefn+'.script';
    dfn:=slash(ConfigDir)+fn;
    if FileExists(dfn) then begin
      if MessageDlg(format(rsScriptAlread,[fn]),mtConfirmation,mbYesNo,0)=mrNo
        then exit;
  end;
  end;
  if DownloadScript(fn,dfn) then  begin
    if uppercase(ExtractFileExt(fn))='.ZIP' then begin
      try
      configexist:=false;
      configfn:=slash(ConfigDir)+trim(basefn)+'_config.script';
      if FileExists(configfn) then begin
        CopyFile(configfn,configfn+'.backup',[cffOverwriteFile]);
        configexist:=true;
      end;
      FUnZipper:=TUnZipper.Create;
      FUnZipper.FileName:=dfn;
      FUnZipper.OutputPath:=ConfigDir;
      FUnZipper.UseUTF8:=True;
      FUnZipper.Examine;
      FUnZipper.UnZipAllFiles;
      if configexist then begin
        DeleteFile(configfn+'.dist');
        RenameFile(configfn,configfn+'.dist');
        CopyFile(configfn+'.backup',configfn,[cffOverwriteFile]);
      end;
      FscriptName:=trim(basefn)+'_config';
      if not FileExists(slash(ConfigDir)+FscriptName+'.script') then
        FscriptName:='';
      ModalResult:=mrOK;
      except
         on E: Exception do
         begin
           ShowMessage('Unzip error : '+E.Message)
         end;
      end;
      FUnZipper.Free;
    end
    else begin
      FscriptName:=trim(StringGrid1.Cells[1,x]);
      ModalResult:=mrOK;
    end;
  end;
end;

function Tf_downloadscript.DownloadScript(fn,dfn: string): boolean;
begin
  result:=false;
  DownloadDialog1.ConfirmDownload:=false;
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

