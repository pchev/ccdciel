unit pu_sensoranalysis;

{$mode ObjFPC}{$H+}

{
Copyright (C) 2023 Patrick Chevalley & Han Kleijn

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

uses u_translation, u_global, cu_fits, cu_camera, UScaleDPI, math,u_utils,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Spin, Grids, TAGraph, TASeries, TARadialSeries;


type
   Tlightinfo = record
       gain      : integer; //camera gain
       exposure,
       median_light_adu,
       sigma_light_adu,
       read_noise_e,
       gain_e,             //gain e-/adu
       fullwell_capacity_e: double;
   end;

type
   xy_list = array[0..1,0..30] of double;


type
  { Tf_sensoranalysis }
  Tf_sensoranalysis = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ButtonClose1: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart2LineSeries1: TLineSeries;
    Chart3: TChart;
    Chart2: TChart;
    Chart3LineSeries1: TLineSeries;
    Chart4: TChart;
    Chart4LineSeries1: TLineSeries;
    Chart5: TChart;
    Chart5LineSeries1: TLineSeries;
    Chart5LineSeries2: TLineSeries;
    Chart6: TChart;
    Chart6BarSeries1: TBarSeries;
    exposuremax1: TLabel;
    exposuremin1: TLabel;
    Gain3: TSpinEdit;
    Gain4: TSpinEdit;
    Gain5: TSpinEdit;
    Gain6: TSpinEdit;
    Gain7: TSpinEdit;
    Gain8: TSpinEdit;
    dark_current_test_duration1: TSpinEdit;
    GroupBox1: TGroupBox;
    Label10: TLabel;
    Gain2: TSpinEdit;
    Label11: TLabel;
    Label12: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LabelFullwellcapacity1: TLabel;
    LabelMaxAdu1: TLabel;
    LabelTemperature1: TLabel;
    LabelTemperature2: TLabel;
    PageControl1: TPageControl;
    RadioButton1: TRadioButton;
    lin1: TRadioButton;
    lin2: TRadioButton;
    lin3: TRadioButton;
    lin4: TRadioButton;
    lin5: TRadioButton;
    lin6: TRadioButton;
    lin7: TRadioButton;
    lin8: TRadioButton;
    RadioButton18: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    RadioButton9: TRadioButton;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    repeats1: TSpinEdit;
    StepButton1: TButton;
    Instructions: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Gain1: TSpinEdit;
    Offset1: TSpinEdit;
    StringGrid1: TStringGrid;
    Linearity: TTabSheet;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure StepButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TabSheet4Show(Sender: TObject);
  private
    FFits: TFits;
    w,h,bit_depth,nrgainsteps : integer;
    biaslevel,flux_adu, sigma_dark_adu,exposure,sat_level_adu,exposure_min : double;
    stoploop: boolean;
    measurements : array of Tlightinfo;
    Fcamera: T_camera;
    FonShowMessage: TNotifyMsg;
    procedure msg(txt:string; level: integer=3);
    procedure SetROI;
    function Takeimage(exp:double; typeof {FLAT, DARK, LIGHT, BIAS}: TFrameType): boolean;
    function stdev(img1,img2: Timafloat): double;//find standard deviation of a single image using two images and avoid pixel-to-pixel variations in the sensitivity of the CCD, known as the flat field effect.
    procedure median_and_stdev(exp: double;nrframes :integer; typeof {FLAT, DARK, LIGHT, BIAS}: TFrameType; out  the_stdev, the_median : double);//calculate stdev and median using several exposures
    function median(img1 : Timafloat): double;//median value of an image
    function median_of_median(exp: double;nrframes :integer;typeof {FLAT, DARK, LIGHT, BIAS}: TFrameType): double;//median of medians

    function bitdepth(img1 : Timafloat): integer;//find the bit depth of the data
    function max(img1 : Timafloat): single;//max value of an image
    procedure update_temperature_reading;
    procedure draw_linearity_line(xylist : xy_list; nr : integer);
  public
    property Fits: TFits read FFits write FFits;
    property Camera: T_camera read Fcamera write Fcamera;
    property onShowMessage: TNotifyMsg read FonShowMessage write FonShowMessage;
  end;

var
  f_sensoranalysis: Tf_sensoranalysis;

implementation

{$R *.lfm}


var
   step : integer;

{ Tf_sensoranalysis }


{Basic formula

 σ [e-] = sqrt ( (readout_noise*readout_noise) +
                 (nr_thermal_electrons) +
                 (nr_photo_electrons) ) (formula 1)

Therefore for a dark
             σ[e-]:=readout_noise (1a)

For a light with a strong signal the readout and thermal noise becomes insignificant:
             σ[e-]:=sqrt(nr_photo_electrons)
             σ[e-]:=sqrt(flux[e-]) (formula 1b)

Gain can be applied bot on signal and noise
        dark_current[e]:=dark_current[adu]*gain[e-/adu]      (formula 5)
        σ_dark[e-]:=sqrt(dark_current[e])



--------------------------------------------------
Step 1 Measure median dark value

     median_dark

Step 2 Measure σ[adu] of a dark
     σ_dark[adu]:=STDEV(dark1-dark2)/sqrt(2) (formula 2)

Step 3 Calculate gain by exposing a light using a flat panel:

     flux[adu]:=median_px_value - median_dark_value
     σ_light[adu]:=STDEV(light1-light2)/sqrt(2)

     σ_light[e-]:=sqrt(flux[e-]) (formula 1b)
     sqr(σ_light[e-]):=flux[e-]
     sqr(σ_light[adu]*gain[e-/adu]):=flux[adu]*gain[e-/adu]
     sqr(σ_light[adu])*sqr(gain[e-/adu]):=flux[adu]*gain[e-/adu]
     gain[e-/adu]:=flux[adu]/sqr( σ_light[adu]) (formula 3)

Step 4, Calculate read_noise[e-]

     read_noise[e-]:=σ_dark[adu] * gain[e-/adu]


Step 5 Find saturation level sat_level[adu].

Expose long using a flat panel and measure sat_level[adu]


Step 6 Calculate full well capacity

  FullWell_capacity[e-]:=sat_level[adu]*gain[e-/adu]


Step 7a Calculate dark current method Δadu

       measure Δ adu over 100 seconds
       Δ e- = Δ adu * gain[e-/adu]
       dark current: Δ e-/100 [e-/(sec*pixel)]

Step 7b Calculate dark current method Δσ

      measure noise at beginning and after 100 seconds
      σ_begin[e]:= σ_begin[adu]*gain[e-/adu]
      σ_end[e]:= σ_end[adu]*gain[e-/adu]

      sqr(σ_end[e])=sqr(σ_dark_current[e]+sqr(σ_read_noise[e])
      sqr(σ_end[adu]*gain[e-/adu])=sqr(σ_dark_current[e]+sqr(σ_read_noise[adu]*gain[e-/adu])
      sqr(σ_end[adu]*gain[e-/adu])=dark_current[e-] + sqr(σ_read_noise[adu]*gain[e-/adu])
      dark_current[e-]:=sqr(σ_end[adu]*gain[e-/adu]) - sqr(σ_read_noise[adu]*gain[e-/adu])    (formula 4)


Note a correction for gain in the driver should be applied. E.g. ASI1600 12 bit sensor ouput is increased from 0..4096 to 0..65535. A additional gain factor of 16.
}



procedure Tf_sensoranalysis.msg(txt:string; level: integer=3);
begin
 if assigned(FonShowMessage) then FonShowMessage('Sensor analysis: '+txt,level);
end;


procedure Tf_sensoranalysis.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self);
end;


procedure Tf_sensoranalysis.FormShow(Sender: TObject);
begin
  exposure_min:=math.min(0.01, math.max(camera.ExposureRange.min,0.0));//protect againt 9999 or -9999 values

  Gain1.enabled:=camera.CanSetGain;
  Gain2.enabled:=camera.CanSetGain;
  Gain3.enabled:=camera.CanSetGain;
  Gain4.enabled:=camera.CanSetGain;
  Gain5.enabled:=camera.CanSetGain;
  Gain6.enabled:=camera.CanSetGain;
  Gain7.enabled:=camera.CanSetGain;
  Gain8.enabled:=camera.CanSetGain;

  Gain1.MaxValue:=camera.GainMax;
  Gain1.MinValue:=camera.GainMin;
  Gain2.MaxValue:=camera.GainMax;
  Gain2.MinValue:=camera.GainMin;
  Gain3.MaxValue:=camera.GainMax;
  Gain3.MinValue:=camera.GainMin;
  Gain4.MaxValue:=camera.GainMax;
  Gain4.MinValue:=camera.GainMin;
  Gain5.MaxValue:=camera.GainMax;
  Gain5.MinValue:=camera.GainMin;
  Gain6.MaxValue:=camera.GainMax;
  Gain6.MinValue:=camera.GainMin;
  Gain7.MaxValue:=camera.GainMax;
  Gain7.MinValue:=camera.GainMin;
  Gain8.MaxValue:=camera.GainMax;
  Gain8.MinValue:=camera.GainMin;

  Gain1.value:=camera.GainMin;

  if camera.GainMin<>0 then
  begin
    Gain2.value:=round(2*camera.GainMin);
    Gain3.value:=round(3*camera.GainMin);
    Gain4.value:=round(5*camera.GainMin);
  end
  else
  begin
    Gain2.value:=round(0.01*camera.GainMax);
    Gain3.value:=round(0.02*camera.GainMax);
    Gain4.value:=round(0.04*camera.GainMax);
  end;

  Gain5.value:=round(0.1*camera.GainMax);
  Gain6.value:=round(0.2*camera.GainMax);
  Gain7.value:=round(0.4*camera.GainMax);
  Gain8.value:=camera.GainMax;

  Offset1.enabled:=camera.hasOffset;
  Offset1.MaxValue:=camera.OffsetMax;
  Offset1.MinValue:=camera.OffsetMin;
  if Offset1.enabled then
    Offset1.Value:=camera.Offset;

  chart1.Title.text.setStrings('Linearity      ('+camera.ccdname+')');
  chart2.Title.text.setStrings('Gain in e-/adu      ('+camera.ccdname+')');
  chart3.Title.text.setStrings('Read noise      ('+camera.ccdname+')');
  chart4.Title.text.setStrings('Full well capacity of each pixel in e-      ('+camera.ccdname+')');
  chart5.Title.text.setStrings('Dark current and total noise      ('+camera.ccdname+')');

  StepButton1.caption:='Start';
  Instructions.Lines.text:='Camera sensor analyses'+#10+
                           #10+
                           'Your camera has to be attached to a telescope or lens.'+#10+
                           'First you have to place a flat panel or substitute.'+#10+
                           'In the second step darks will be made so you will need to cover the telescope.'+#10+
                           #10+
                           'If flat panel is placed then press "Take lights" to start the test.';

  update_temperature_reading;

  StepButton1.enabled:=true;
  StepButton1.caption:='Take lights';
  step:=0;
end;


procedure Tf_sensoranalysis.update_temperature_reading;
begin
  LabelTemperature1.Caption:=FormatFloat(f2,camera.Temperature);
  LabelTemperature2.Caption:='🌡  '+FormatFloat(f2,camera.Temperature)+' °C';
end;


procedure Tf_sensoranalysis.TabSheet4Show(Sender: TObject);
begin
  exposuremin1.caption:=floattostrF(camera.ExposureRange.min,FFfixed,0,6);
  exposuremax1.caption:=floattostrF(camera.ExposureRange.max,FFfixed,0,0);
  LabelFullwellcapacity1.Caption:=FormatFloat(f0,camera.FullWellCapacity);
  LabelMaxAdu1.Caption:=FormatFloat(f0,camera.MaxADU);
end;


procedure Tf_sensoranalysis.ButtonCloseClick(Sender: TObject);
begin
  stoploop:=true;
  close;
end;


procedure Tf_sensoranalysis.Button1Click(Sender: TObject);
begin
  stringgrid1.selection:=rect(0,0,5,99);
  stringgrid1.CopyToClipboard;
end;


procedure Tf_sensoranalysis.Button2Click(Sender: TObject);
begin
  stoploop:=true;
  Instructions.Lines.add('Abort pressed. Will stop soon.');
end;


procedure Tf_sensoranalysis.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  measurements:=nil;
end;


procedure Tf_sensoranalysis.SetROI;
var sx,sy: integer;
begin
  w:=200;
  h:=200;
  sx:=round(camera.MaxX-w) div 2;
  sy:=round(camera.MaxY-h) div 2;
  camera.SetFrame(sx,sy,w,h);
end;


function Tf_sensoranalysis.Takeimage(exp: double; typeof {FLAT, DARK, LIGHT, BIAS}: TFrameType): boolean;
var bin,poffset: integer;
begin
  SetROI;
  fits.SetBPM(bpm,0,0,0,0);
  fits.DarkOn:=false;
  bin:=1;
  poffset:=Offset1.Value;
  msg(copy(FrameName[ord(typeof)],1,6)+'exposure='+FormatFloat(f3,exp)+' binning='+inttostr(bin));
  if not camera.ControlExposure(exp,bin,bin,typeof,ReadoutModeCapture,gain,poffset) then begin
      msg(rsExposureFail,1);
      result:=false;
      exit;
  end;
  result:=true;
end;


function  Tf_sensoranalysis.stdev(img1,img2: Timafloat): double;//find standard deviation of a single image using two images and avoid pixel-to-pixel variations in the sensitivity of the CCD, known as the flat field effect.
var
   i,j    : integer;
begin  //stdev
  result:=0;
  for i:=0 to w-1 do
  for j:=0 to h-1 do
  begin
    result:=result+sqr(img1[0,j,i]-img2[0,j,i]);
  end;
  result:=sqrt(result/(w*h))/sqrt(2);
end;


function  Tf_sensoranalysis.median(img1 : Timafloat): double;//median value of an image
var
   i,j,counter : integer;
   median_array             : array of double;
begin
  setlength(median_array,w*h);
  counter:=0;
  for i:=0 to w-1 do
  for j:=0 to h-1 do
  begin
    median_array[counter]:=img1[0,j,i]; {fill array with sampling data. Smedian will be applied later}
    inc(counter);
  end;
  result:=smedian(median_array,counter);
  median_array:=nil;
end;


function  Tf_sensoranalysis.max(img1 : Timafloat): single;//max value of an image
var
   i,j    : integer;
begin
  result:=0;
  for i:=0 to w-1 do
  for j:=0 to h-1 do
  begin
    result:=math.max(result,img1[0,j,i]);
  end;
end;


function  Tf_sensoranalysis.median_of_median(exp: double;nrframes :integer;typeof {FLAT, DARK, LIGHT, BIAS}: TFrameType): double;//median of medians
label 999;
var
  i         : integer;
  valuesM   : array of double;
begin
  result:=0;
  result:=0;
  setlength(valuesM,nrframes);
  for i:=0 to nrframes-1 do
  begin
    if Takeimage(exp,typeof) then
      valuesM[i]:=median(Ffits.image)
    else
     goto 999;
    if stoploop then goto 999;
  end;
  result:=smedian(valuesM,nrframes);//median of the median values
999:
  valuesM:=nil;
end;


function  Tf_sensoranalysis.bitdepth(img1 : Timafloat): integer;//find the bit depth of the data
var
   histogram : array of integer;
   i,j,step,minstep   : integer;
begin
  setlength(histogram,65536);
  for i:=0 to 65535 do histogram[i]:=0;

  result:=0;
  for i:=0 to w-1 do
  for j:=0 to h-1 do
  begin
    inc(histogram[round(img1[0,j,i])],1);
  end;

  minstep:=99999;
  step:=99999;
  for i:=0 to 65535 do
  begin
    if histogram[i]<>0 then
    begin
      minstep:=math.min(minstep,step);
      step:=1;
    end
    else
      inc(step);
  end;
  minstep:=minstep;
  if minstep>1 then
    result:=16-round(sqrt(minstep))
  else
    result:=16;

  histogram:=nil;
end;


procedure Tf_sensoranalysis.median_and_stdev(exp: double;nrframes :integer; typeof {FLAT, DARK, LIGHT, BIAS}: TFrameType; out  the_stdev, the_median : double);//calculate stdev and median using several exposures
   label 999;
var
   i         : integer;
   image3    : Timafloat;
   valuesST,valuesM   : array of double;
begin
  the_median:=0;
  setlength(valuesST,nrframes);
  setlength(valuesM,nrframes);
  for i:=0 to nrframes-1 do
  begin
    if Takeimage(exp,typeof) then
    begin
       image3:=Ffits.image;
       setlength(image3,1,h,w); //duplicate
       if Takeimage(exp,typeof) then
         valuesST[i]:=stdev(image3,Ffits.image) //calculate standard deviation
       else
       begin
         the_stdev:=0;
         goto 999;
       end;
       valuesM[i]:=(median(image3)+median(Ffits.image))/2;
    end
    else
    begin
      the_stdev:=0;
      goto 999;
    end;
    if stoploop then goto 999;
  end;
  the_stdev:=smedian(valuesST,nrframes);
  the_median:=smedian(valuesM,nrframes);//median of the median values
999:
  image3:=nil;
  valuesST:=nil;
  valuesM:=nil;
end;


procedure trendline(xylist: xy_list; len{length xy_list} : integer; out  slope, intercept:double); //find linear trendline Y = magnitude_slope*X + intercept
var                                                         //idea from https://stackoverflow.com/questions/43224/how-do-i-calculate-a-trendline-for-a-graph
  sumX,sumX2,sumY, sumXY,median,mad  : double;
  count, i                           : integer;
begin
  count:=0;
  sumX:=0;
  sumX2:=0;
  sumY:=0;
  sumXY:=0;

  for i:=0 to  len-1 do
  begin
    inc(count);
    sumX:=sumX+xylist[0,i]; //sum X= sum exposure
    sumX2:=sumx2+sqr(xylist[0,i]);
    sumY:=sumY+xylist[1,i]; //sum Y= sum adu's;
    sumXY:=sumXY+xylist[0,i]*xylist[1,i];
  end;

  Slope:=(count*sumXY - sumX*sumY) / (count*sumX2 - sqr(sumX));   // b = (n*Σ(xy) - ΣxΣy) / (n*Σ(x^2) - (Σx)^2)
  Intercept:= (sumY - Slope * sumX)/count;                        // a = (Σy - bΣx)/n
end;


procedure Tf_sensoranalysis.draw_linearity_line(xylist : xy_list; nr : integer);
var
  i : integer;
  slope, intercept,err,maxerr : double;
  mess : string;
begin
  trendline(xylist,10, slope, intercept);//find trendline for 0..90% is position 0..9

  maxerr:=0;
  for i:=0 to nr do
  begin
    err:=(xylist[1,i] - (xylist[0,i]*slope+intercept))*100/65535;
    Chart1LineSeries1.addxy(xylist[0,i],xylist[1,i],floattostrF(err,FFfixed,0,2)+'%');
    {Now we have to make sure that the labels are displayed. For this purpose, the TChartSeries which is an ancestor of TLineSeries has a property Marks.
    In the sub-properties you find the option Style which is set to smsNone by default, meaning that no labels are displayed.
    You see in the object inspector that there is a variety of information than can be displayed in the marks, but you'll need here the option smsLabel which shows the text of the ChartDataItems.}
    if i<=9 then maxerr:=math.max(maxerr,abs(err));
  end;
  mess:=floattostrF(maxerr,FFfixed,0,3)+'%';
  Instructions.Lines.add('Max linearity error is ' +mess+ ' in range [0..90%]');
  chart1.Title.text.setStrings('Linearity. Maximum error ' +mess+ ' in range [0..90%]. Gain is '+inttostr(gain));
end;


procedure Tf_sensoranalysis.StepButton1Click(Sender: TObject);
var
  saturationlevel,correction,stepexposure,themedian, oldthemedian,median_dark_adu,sigma_light_adu,exposure_lin,sigma_dark_adu2,median_dark_adu2,
  dark_current_adu,dark_current_es, total_noise,total_noise_e,read_noise2_e,dark_current2_es             : double;
  i,gainstep,nr                                                                                          : integer;
  message                                                                                                : string;
  Save_Cursor:TCursor;
  xylist : xy_list;
const
  level7=0.7;// 70% saturation for testing.
     procedure prepare_stop;
     begin
       StepButton1.caption:='Restart';
       StepButton1.enabled:=true;
       Screen.Cursor:=Save_Cursor;
       step:=0;
     end;

begin
  stoploop:=false;

  if camera.CanSetGain=false then
  begin
     Instructions.Lines.add(#10+#10+'Fatal failure!! Can not set gain. Allow setting the camera gain in Preference, Camera');
     exit;
  end;
   //Chart6BarSeries1.addxy(0,20,' test' );
   //Chart6BarSeries1.addxy(01,20,' test' );
   //Chart1LineSeries1.addxy(0.5,4000,'test');
   //Chart1LineSeries1.addxy(1,4000,'test');

  Save_Cursor := Screen.Cursor;
  Screen.Cursor := crHourglass; { Show hourglass cursor }

  case step of
  0: begin //taking lights

      StepButton1.enabled:=false;
      StepButton1.caption:='.....';
      Instructions.Lines.add('Working on the LIGHTS....'+#10);

      // find bias level
      exposure:=0.0;
      biaslevel:=median_of_median(exposure,repeats1.value,BIAS {FLAT, DARK, LIGHT, BIAS});//find best median
      Instructions.Lines.add('Bias level: '+floattostrF(biaslevel,FFfixed,0,0));


      nrgainsteps:=8-1;
      setlength(measurements,nrgainsteps+1);
      measurements[0].gain:=Gain1.value;
      measurements[1].gain:=Gain2.value;
      measurements[2].gain:=Gain3.value;
      measurements[3].gain:=Gain4.value;
      measurements[4].gain:=Gain5.value;
      measurements[5].gain:=Gain6.value;
      measurements[6].gain:=Gain7.value;
      measurements[7].gain:=Gain8.value;

      gainstep:=0;

      while gainstep<=nrgainsteps do  //do all gains
      begin
        update_temperature_reading;
        gain:=measurements[gainstep].gain;
        if gainstep=0 then exposure:=0.1
        else
        begin
          if measurements[gainstep-1].gain>0 then // extrapolating from gain 0 does not work
          exposure:= measurements[gainstep-1].exposure * measurements[gainstep-1].gain/measurements[gainstep].gain;//assuming gain is linear
        end;

        Instructions.Lines.add('Testing gain: '+inttostr(gain));
        for i:=0 to 9 do
        begin
          if Takeimage(exposure,FLAT) then
          begin
            measurements[gainstep].median_light_adu:= median(Ffits.image);
            saturationlevel:=(measurements[gainstep].median_light_adu-biaslevel)/camera.MaxADU;

            Instructions.Lines.add('Trying to find the exposure time for 70% saturation. Exposure time: '+floattostrF(exposure,FFfixed,0,3)+', saturation level: '+floattostrF(saturationlevel*100,FFfixed,0,0 )+'%');
            if (((Ffits.imagemean-biaslevel)>0.65*camera.MaxADU) and ((Ffits.imagemean-biaslevel)<0.75*camera.MaxADU)) then break;//exposure is good
            exposure:=min(30,exposure*level7/saturationlevel);//try to get 70% exposed. level7=0.7
           end;
          if stoploop then
          begin
            prepare_stop;
            exit;
          end;
          if (exposure>=30) then
            break;
        end; //for loop
        if ((i=9) or (exposure>=30)) then
        begin
          Instructions.Lines.add('Abort. Can not find a correct exposure time. Check flat panel'+#10);
          prepare_stop;
          exit;
        end;

        //store information
        measurements[gainstep].gain:=gain;
        measurements[gainstep].exposure:=exposure;

        median_and_stdev(exposure,repeats1.value,FLAT,{out}measurements[gainstep].sigma_light_adu,measurements[gainstep].median_light_adu);//measure median value and stdev


        if gainstep=0 then
        begin
          bit_depth:=bitdepth(Ffits.image);
          Instructions.Lines.add('Bit depth image is: '+inttostr(bit_depth));
        end;

        if (  ((gainstep=0) and (lin1.checked)) or
              ((gainstep=1) and (lin2.checked)) or
              ((gainstep=2) and (lin3.checked)) or
              ((gainstep=3) and (lin4.checked)) or
              ((gainstep=4) and (lin5.checked)) or
              ((gainstep=5) and (lin6.checked)) or
              ((gainstep=6) and (lin7.checked)) or
              ((gainstep=7) and (lin8.checked)) ) then
        begin
          Instructions.Lines.add('Testing linearity.'+#10);
          stepexposure:=exposure/(level7*10);//exposure difference to get 10% more level

          oldthemedian:=0;
          for nr:=0 to 30 do  //Saturate the image
          begin
            if stoploop then begin prepare_stop; exit end;
            exposure_lin:=math.max(exposure_min,stepexposure*nr);//about 10%. Minimum 1 ms should be possible for all cameras

            median_and_stdev(exposure_lin,repeats1.value,FLAT,{out}sigma_light_adu,themedian);//measure median value and stdev
            StringGrid1.InsertRowWithValues(stringgrid1.rowcount,[inttostr(nr), floattostrF(measurements[gainstep].Gain,FFfixed,0,0),'','','',floattostrF(exposure_lin,FFfixed,0,3),floattostrF((themedian),FFfixed,0,0),
                                              floattostrF((themedian-oldthemedian)*100/65535,FFfixed,0,3),floattostrF(sigma_light_adu,FFfixed,0,1)]);
            StringGrid1.Row :=stringgrid1.rowcount;//scroll

            if (themedian-oldthemedian)<0.1 then break; //saturation reached
            oldthemedian:=themedian;
            xylist[0,nr]:=exposure_lin;
            xylist[1,nr]:=themedian;
            if stoploop then begin prepare_stop; exit; end;
          end;//for loop

          draw_linearity_line( xylist,nr-1);
        end;

        if gainstep=8-1 then //find sat level
        begin
          if Takeimage(3*exposure,FLAT) then  //expose at 4*70%
          sat_level_adu:=max(Ffits.image);
          Instructions.Lines.add('Test saturation level. Exposure time: '+floattostrF(3*exposure,FFfixed,0,3)+', saturation level: '+floattostrF(sat_level_adu,FFfixed,0,0 )+' adu');
        end;

        inc(gainstep);

      end; //while

      Instructions.Lines.add(#10+
                            'Place the cap on the telescope for making darks.'+#10+#10+
                            'If ready press button "Take darks"');
      StepButton1.caption:='■■■ Take darks ■■■';
      StepButton1.enabled:=true;
      step:=1;
    end;


  1: begin  //taking darks and final process
      StepButton1.enabled:=false;
      StepButton1.caption:='.....';
      gainstep:=0;
      while gainstep<=nrgainsteps do  //do all gains
      begin
        update_temperature_reading;

        if gainstep=0 then Instructions.Lines.add('Working on the DARKS....');
        exposure:=measurements[gainstep].Exposure;
        gain:=measurements[gainstep].gain;

        if (( gainstep=0) and (Takeimage(exposure,DARK))) then //First dark is ignored since in some cameras (Touptek) the pedestal value could be higher in the first dark after a bright flat exposure')
        begin  //so this dark has two functions. 1) Skip first invalid dark and 2) test if flat panel is removed.
          themedian:=median(Ffits.image);
          Instructions.Lines.add('Took dark with gain '+floattostrF( gain,FFfixed,0,0)+ ' and exposure ' +floattostrF(measurements[gainstep].exposure,FFfixed,0,3)+' to remove persistent charge.  Median value '+floattostrF(themedian,FFfixed,0,3));

          if themedian>0.5*measurements[0].median_light_adu then
          begin
            Instructions.Lines.add('Flat panel is still present. Remove panel, place the telescope cap and press again "Take darks".');
            StepButton1.enabled:=true;
            StepButton1.caption:='■■■ Take darks ■■■';
            Screen.Cursor:=Save_Cursor;
            exit;
          end;
          Chart6BarSeries1.addxy(gainstep-1,themedian,' Gain '+inttostr(gain)+', ' );

        end;
        Instructions.Lines.add('Taking dark(s) with gain '+floattostrF( gain,FFfixed,0,0)+ ' and exposure ' +floattostrF(measurements[gainstep].exposure,FFfixed,0,3));
        median_and_stdev(measurements[gainstep].exposure,repeats1.value,DARK,{out}sigma_dark_adu,median_dark_adu);//as stdev but do it nrframes times and report median value as result

        Chart6BarSeries1.addxy(gainstep,median_dark_adu,' Gain '+inttostr(gain)+', ');

        flux_adu:=measurements[gainstep].median_light_adu-median_dark_adu;//calculate median flux value of one pixel

        //corrections for gain in the camera driver. E.g. for 12 bit ASI1600 with an output range of 0..65535 the correction factor is 16
        if bit_depth<>16 then
          correction:=round(sat_level_adu/power(2,bit_depth))
        else
        correction:=1;

        measurements[gainstep].gain_e{e-/adu}:=(flux_adu/correction)/sqr(measurements[gainstep].sigma_light_adu/correction);

        measurements[gainstep].read_noise_e{e-}:=(sigma_dark_adu/correction) * measurements[gainstep].gain_e;
        measurements[gainstep].FullWell_capacity_e:=(sat_level_adu/correction)*measurements[gainstep].gain_e;

        StringGrid1.InsertRowWithValues(stringgrid1.rowcount,[inttostr(gainstep+1), floattostrF(measurements[gainstep].Gain,FFfixed,0,0),floattostrF(measurements[gainstep].gain_e,FFfixed,0,3),
                                                              floattostrF(measurements[gainstep].read_noise_e,FFfixed,0,3),floattostrF(measurements[gainstep].FullWell_capacity_e,FFfixed,0,0),
                                                              floattostrF(measurements[gainstep].exposure,FFfixed,0,3),floattostrF(measurements[gainstep].median_light_adu,FFfixed,0,0)+'-'+floattostrF(median_dark_adu,FFfixed,0,0)]);
        StringGrid1.Row :=stringgrid1.rowcount;//scroll

        Chart2LineSeries1.addxy(measurements[gainstep].Gain,measurements[gainstep].gain_e);
        Chart3LineSeries1.addxy(measurements[gainstep].Gain,measurements[gainstep].read_noise_e);
        Chart4LineSeries1.addxy(measurements[gainstep].Gain,measurements[gainstep].FullWell_capacity_e);

        inc(gainstep);
      end;//while


      Instructions.Lines.add('Testing dark current');

      if radiobutton9.checked=false then
      begin
        if radiobutton1.checked then gainstep:=0 else
        if radiobutton2.checked then gainstep:=1 else
        if radiobutton3.checked then gainstep:=2 else
        if radiobutton4.checked then gainstep:=3 else
        if radiobutton5.checked then gainstep:=4 else
        if radiobutton6.checked then gainstep:=5 else
        if radiobutton7.checked then gainstep:=6 else
        if radiobutton8.checked then gainstep:=7 else
        if radiobutton1.checked then gainstep:=8;

        gain:=measurements[gainstep].gain; //camera gain

        //take an exposure of 1.01 seconds to avoid weird variations in the first second.
        median_and_stdev(1.01,repeats1.value,DARK,{out}sigma_dark_adu,median_dark_adu);//as stdev but do it nrframes times and report median value as result
        //take an exposure of value+1 seconds.
        median_and_stdev(dark_current_test_duration1.value+1.01,repeats1.value,DARK,{out}sigma_dark_adu2,median_dark_adu2);//as stdev but do it nrframes times and report median value as result

        read_noise2_e:=sigma_dark_adu*measurements[gainstep].gain_e/correction; // read noise after 1.1 seconds
        total_noise_e:=sigma_dark_adu2*measurements[gainstep].gain_e/correction;// total noise after long exposure noise[e-]:=noise_adu * gain_e

        Instructions.Lines.add('Noise after 1.01 sec: '+floattostrF(read_noise2_e,FFfixed,0,2)+ '[e-]');
        Instructions.Lines.add('Noise after '+inttostr(dark_current_test_duration1.value+1)+' sec:  '+floattostrF(total_noise_e,FFfixed,0,2)+ '[e-]');

        dark_current_adu:=median_dark_adu2-median_dark_adu;
        dark_current_es:=dark_current_adu*measurements[gainstep].gain_e/(dark_current_test_duration1.value*correction); //dark current in e-/(sec*pixel)
        Instructions.Lines.add('Δadu: '+floattostrF(dark_current_adu,FFfixed,0,0));
        Instructions.Lines.add('Dark current method Δadu: '+floattostrF(dark_current_es,FFfixed,0,4)+ ' [e-/(sec*pixel)]');

        dark_current2_es:=(sqr(total_noise_e) - sqr(read_noise2_e))/dark_current_test_duration1.value; // dark current in e-/(sec*pixel)
        Instructions.Lines.add('Dark current method Δσ: '+floattostrF(dark_current2_es,FFfixed,0,4)+ ' [e-/(sec*pixel)]');

        if dark_current_adu>2 then //at least 2 adu
        begin
          //Plot method Δadu
          for i:=0 to dark_current_test_duration1.value do
          begin
            total_noise:=sqrt(sqr(measurements[gainstep].read_noise_e)+i*dark_current_es);
            Chart5LineSeries1.addxy(i,total_noise);//total noise:=sqrt(sqr(readnoise)+sqr(dark_noise))==> total noise:=sqrt(sqr(readnoise)+dark current)
          end;

          //Plot method Δσ
          //dark_current_adu[e-]:=sqr(σ_end[adu]*gain[e-/adu]) - sqr(σ_read_noise[adu]*gain[e-/adu])    (formula 4)
          Chart5LineSeries2.addxy(0,read_noise2_e);//read noise
          for i:=0 to dark_current_test_duration1.value  do
          begin
            total_noise:=sqrt(i*dark_current2_es+sqr(read_noise2_e)); //formula 4
            Chart5LineSeries2.addxy(i,total_noise);
          end;
          message:='Dark current '+floattostrF(dark_current2_es,FFfixed,0,5)+' [e-/(sec*pixel)] at'+floattostrF(camera.temperature,FFfixed,0,1)  +'° Celsius. Gain '+inttostr(gain)+'. ('+camera.ccdname+')';
        end
        else
        message:=message+#10+'WARNING. Too short exposure time. Only '+floattostrF(dark_current_adu,FFfixed,0,1)+' adu difference. Set exposure time longer.';
        Instructions.Lines.add(message);

        chart5.Title.text.setStrings(message);
        StringGrid1.InsertRowWithValues(stringgrid1.rowcount,['',message]);
        StringGrid1.Row :=stringgrid1.rowcount;//scroll
      end;

      update_temperature_reading;
      Instructions.Lines.add(#10+#10+'Finished.');
      StepButton1.enabled:=true;
      StepButton1.caption:='Restart';
      step:=0;

    end;
  end;//case

  StringGrid1.InsertRowWithValues(stringgrid1.rowcount,['']);//space line
  Screen.Cursor:=Save_Cursor;
end;

end.

