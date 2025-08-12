unit fu_profile;

{$mode ObjFPC}{$H+}

{
Copyright (C) 2025 Patrick Chevalley

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

uses UScaleDPI, u_translation, u_global, u_utils, cu_fits, math,
  Buttons, TAGraph, TAFuncSeries, TASeries, TASources, TAChartUtils, TATransformations, Graphics,
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls;

type

  { Tf_profile }

  Tf_profile = class(TFrame)
    BtnPinProfile: TSpeedButton;
    BtnSpectraProfile: TSpeedButton;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1LinearAxisTransform1: TLinearAxisTransform;
    Panel1: TPanel;
    Panel3: TPanel;
    ProfileChart: TChart;
    ProfileChartLine: TLineSeries;
    SpectraSource: TListChartSource;
    SpectraProfileMethod: TComboBox;
    Title: TLabel;
    procedure BtnPinProfileClick(Sender: TObject);
    procedure BtnSpectraProfileClick(Sender: TObject);
    procedure SpectraProfileMethodChange(Sender: TObject);
  private
    FSpectraX, FSpectraY, FSpectraWidth, FSpectraHeight: integer;
    FFits: TFits;
    FonSelectionChange: TNotifyEvent;
    procedure SetSpectralProfile(value:boolean);
    function GetSpectralProfile: boolean;
    procedure PanelProfileClose(Sender: TObject; var CloseAction: TCloseAction);
  public
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    procedure SetSpectra(x,y,w,h: integer; f: TFits);
    procedure ShowSpectraProfile(f: TFits);
    property SpectraX: integer read FSpectraX;
    property SpectraY: integer read FSpectraY;
    property SpectraWidth: integer read FSpectraWidth;
    property SpectraHeight: integer read FSpectraHeight;
    property SpectraProfile: boolean read GetSpectralProfile write SetSpectralProfile;
    property onSelectionChange: TNotifyEvent read FonSelectionChange write FonSelectionChange;

  end;

implementation

{$R *.lfm}

constructor Tf_profile.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 Panel1.ChildSizing.LeftRightSpacing:=8;
 {$endif}
 ScaleDPI(Self);
 SetLang;
 FSpectraHeight:=0;
 FSpectraWidth:=0;
end;

destructor  Tf_profile.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_profile.SetLang;
begin
  Title.Caption:=rsSpectralProf;
  BtnSpectraProfile.Caption:=rsSelectArea;
end;

procedure Tf_profile.SetSpectralProfile(value:boolean);
begin
  BtnSpectraProfile.Down:=value;
  panel1.visible:=BtnSpectraProfile.Down;
  if not value then begin
    FSpectraHeight:=0;
    FSpectraWidth:=0;
  end;
  if Assigned(FonSelectionChange) then FonSelectionChange(self);
end;

function Tf_profile.GetSpectralProfile: boolean;
begin
   result:=BtnSpectraProfile.Down;
end;


procedure Tf_profile.BtnSpectraProfileClick(Sender: TObject);
begin
  panel1.Visible:=BtnSpectraProfile.Down;
  if Assigned(FonSelectionChange) then FonSelectionChange(self);
end;

procedure Tf_profile.PanelProfileClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  panel1.visible:=BtnSpectraProfile.Down;
  config.SetValue('/SpectraProfile/ProfileX',TForm(Sender).Left);
  config.SetValue('/SpectraProfile/ProfileY',TForm(Sender).Top);
  config.SetValue('/SpectraProfile/ProfileW',TForm(Sender).Width);
  config.SetValue('/SpectraProfile/ProfileH',TForm(Sender).Height);
  CloseAction:=caFree;
  SpectraProfileMethod.Parent:=Panel3;
  ProfileChart.Parent:=Panel1;
  ProfileChart.AxisList.Axes[0].Visible:=true;
  ProfileChart.AxisList.Axes[0].Title.Visible:=false;
  ProfileChart.AxisList.Axes[0].Marks.Format:='%0:.9gK';
  ProfileChart.AxisList.Axes[0].Transformations:=ChartAxisTransformations1;
  ProfileChart.AxisList.Axes[1].Visible:=false;
  ProfileChartLine.LinePen.Width:=1;
end;

procedure Tf_profile.BtnPinProfileClick(Sender: TObject);
var f: TForm;
    p: TPanel;
    x,y,w,h: integer;
begin
  if ProfileChart.Parent=Panel1 then begin
   // detach the graph
   x:=config.GetValue('/SpectraProfile/ProfileX',-1);
   y:=config.GetValue('/SpectraProfile/ProfileY',-1);
   w:=config.GetValue('/SpectraProfile/ProfileW',-1);
   h:=config.GetValue('/SpectraProfile/ProfileH',-1);
   f:=TForm.Create(self);
   f.FormStyle:=fsStayOnTop;
   f.OnClose:=@PanelProfileClose;
   if w>0 then
     f.Width:=w
   else
     f.Width:=DoScaleX(400);
   if h>0 then
     f.Height:=h
   else
     f.Height:=DoScaleY(350);
   f.Caption:=rsProfile2;
   p:=TPanel.Create(f);
   p.Align:=alBottom;
   p.Height:=SpectraProfileMethod.Height+SpectraProfileMethod.Top+4;
   p.Parent:=f;
   SpectraProfileMethod.Parent:=p;
   ProfileChart.Parent:=f;
   ProfileChart.Align:=alClient;
   ProfileChart.AxisList.Axes[0].Visible:=true;
   ProfileChart.AxisList.Axes[0].Title.Caption:='ADU';
   ProfileChart.AxisList.Axes[0].Title.Visible:=true;
   ProfileChart.AxisList.Axes[0].Marks.Format:='%0:.9g';
   ProfileChart.AxisList.Axes[0].Transformations:=nil;
   ProfileChart.AxisList.Axes[1].Visible:=true;
   ProfileChart.AxisList.Axes[1].Title.Caption:='X pixel';
   ProfileChart.AxisList.Axes[1].Title.Visible:=true;
   ProfileChartLine.LinePen.Width:=1;
   panel1.Visible:=false;
   if (x>0)and(y>0) then begin
     f.Left:=x;
     f.Top:=y;
   end
   else
     FormPos(f,mouse.CursorPos.x,mouse.CursorPos.y);
   f.Show;
  end
  else if ProfileChart.Parent is TForm then begin
   // close form and dock the graph
   TForm(ProfileChart.Parent).Close;
  end;

end;

procedure Tf_profile.SetSpectra(x,y,w,h: integer; f: TFits);
begin
  FSpectraWidth:=max(0,w);
  FSpectraHeight:=max(0,h);
  FSpectraX:=max(0,min(x,f.HeaderInfo.naxis1-1));
  FSpectraY:=max(0,min(y,f.HeaderInfo.naxis2-1));
  ProfileChart.AxisList.Axes[0].Visible:=true;
  ShowSpectraProfile(f);
end;

procedure Tf_profile.ShowSpectraProfile(f: TFits);
var i,j,n,r,g,b:integer;
    value,wmin,wmax: double;
    hasBPM:boolean;
begin
 FFits:=f;
 if not FFits.BPMProcess then begin
   // apply BPM
   hasBPM:=FFits.hasBPM;
   if not hasBPM then
     FFits.SetBPM(bpm,bpmNum,bpmX,bpmY,bpmAxis);
   FFits.LoadStream;
   FFits.UpdateStream;
   if not hasBPM then
     FFits.SetBPM(bpm,0,0,0,0);
 end;
 wmin:=FFits.HeaderInfo.wavemin;
 wmax:=FFits.HeaderInfo.wavemax;
 if ColorizeSpectra and (wmin<>NullCoord) and (wmax<>NullCoord) then begin
   ProfileChart.BackColor:=clBlack;
   ProfileChartLine.ColorEach:=ceLineAfter;
 end
 else begin
   ProfileChart.BackColor:=clWindow;
   ProfileChartLine.ColorEach:=ceNone;
 end;
 SpectraSource.Clear;
 n:=SpectraProfileMethod.ItemIndex;
 for i:=FSpectraX to FSpectraX+FSpectraWidth do begin
   value:=0;
   case n of
   0: begin
        for j:=FSpectraY to FSpectraY+FSpectraHeight do begin
          value:=value+f.imageMin+f.image[0,j,i]/f.imageC;
        end;
        value:=value/(FSpectraHeight+1);
      end;
   1: begin
        for j:=FSpectraY to FSpectraY+FSpectraHeight do begin
          value:=max(value,f.imageMin+f.image[0,j,i]/f.imageC);
        end;
      end;
   end;
   if ColorizeSpectra and (wmin<>NullCoord) and (wmax<>NullCoord) then begin
     spcolor(wmin+(wmax-wmin)*i/FFits.HeaderInfo.naxis1,r,g,b);
     SpectraSource.Add(i,value,'',RGBToColor(r,g,b));
   end
   else begin
     SpectraSource.Add(i,value);
   end;
 end;
end;

procedure Tf_profile.SpectraProfileMethodChange(Sender: TObject);
begin
  if SpectraProfile and (FFits<>nil) and(FSpectraHeight>0) then
    ShowSpectraProfile(FFits);
end;


end.

