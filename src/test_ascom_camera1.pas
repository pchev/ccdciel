unit test_ascom_camera1;

{$mode objfpc}{$H+}

interface

uses cu_camera, cu_ascomcamera, cu_fits, Variants, comobj, BGRABitmap,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    f: TFits;
    bmp:TBGRABitmap;
    camera: T_ascomcamera;
    ImgCount: integer;
    Stoploop:boolean;
    procedure CameraNewImage(Sender: TObject);
    procedure CameraNewImageAsync(Data: PtrInt);
    procedure msg(txt:string; level:integer);
  public

  end;

const crlf=chr(10)+chr(13);

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  f:=TFits.Create(nil);
  bmp:=TBGRABitmap.Create;
  camera:=T_ascomcamera.Create(nil);
  camera.fits:=f;
  camera.onNewImage:=@CameraNewImage;
  camera.onMsg:=@msg;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  camera.Disconnect;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  camera.free;
  f.free;
  bmp.free;
end;

function MemoryInfo: string;
var  HeapStatus:THeapStatus;
begin
HeapStatus:=GetHeapStatus;
result:='Heap status:'+crlf+
        'TotalAddrSpace:'+Inttostr((HeapStatus.TotalAddrSpace div 1024))+'kb'+crlf+
        'TotalAllocated:'+Inttostr((HeapStatus.TotalAllocated div 1024))+'kb'+crlf+
        'TotalFree:'+Inttostr((HeapStatus.TotalFree div 1024))+'kb';
end;

procedure TForm1.msg(txt:string; level:integer);
begin
  memo1.Lines.Add(txt);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  V: variant;
  dev: WideString;
begin
  dev:=edit1.Text;
  V := CreateOleObject('ASCOM.Utilities.Chooser');
  V.DeviceType:='Camera';
  dev:=widestring(V.Choose(dev));
  V:=Unassigned;
  edit1.Text:=dev;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ImgCount:=0;
  Stoploop:=false;
  camera.Connect(edit1.Text);
  CameraNewImage(nil);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Stoploop:=true;
end;

procedure TForm1.CameraNewImage(Sender: TObject);
begin
 Application.QueueAsyncCall(@CameraNewImageAsync,0);
end;

procedure TForm1.CameraNewImageAsync(Data: PtrInt);
begin
  f.GetBGRABitmap(bmp);
  inc(ImgCount);
  label1.caption:='image count='+inttostr(ImgCount);
  label2.caption:=MemoryInfo;
  if not stoploop then
    camera.StartExposure(0.1)
  else
    camera.Disconnect;
end;

end.

