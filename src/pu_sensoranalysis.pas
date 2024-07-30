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

uses u_translation, u_global, cu_fits, cu_camera, UScaleDPI, Math, u_utils,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Spin, Grids, TAGraph, TASeries, TARadialSeries;

type
  Tlightinfo = record
    gain: integer;   //camera gain
    exposure,
    median_light_adu,
    sd_RTN_light_adu,
    sd_light_adu,
    readnoise_RTN_e,      //read noise plus random telegraph noise
    readnoise_e,
    gain_e,               //gain e-/adu
    fw_capacity_e    : double;//full well capacity
  end;

type
  xy_list = array[0..1, 0..30] of double;


type
  { Tf_sensoranalysis }
  Tf_sensoranalysis = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ButtonClose1: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart2: TChart;
    Chart2LineSeries1: TLineSeries;
    Chart3: TChart;
    Chart3LineSeries1: TLineSeries;
    Chart3LineSeries2: TLineSeries;
    Chart4: TChart;
    Chart4LineSeries1: TLineSeries;
    Chart5: TChart;
    Chart5BarSeries1: TBarSeries;
    Chart6: TChart;
    Chart6LineSeries1: TLineSeries;
    Chart6LineSeries2: TLineSeries;
    Chart6LineSeries3: TLineSeries;
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
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Gain2: TSpinEdit;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    bitsperpixel1: TLabel;
    Label14: TLabel;
    darkcurrent1: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    gain_used1: TLabel;
    Label17: TLabel;
    sensor_type1: TLabel;
    Label4: TLabel;
    model1: TLabel;
    temperature_sensor2: TLabel;
    rtn_rms1: TLabel;
    rtn1: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelFullwellcapacity1: TLabel;
    LabelMaxAdu1: TLabel;
    temperature_sensor1: TLabel;
    Linearity: TTabSheet;
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
    w, h, bit_depth, nrgainsteps: integer;
    biaslevel, flux_adu, sd_dark_adu, sd_RTN_dark_adu, exposure, sat_level_adu, exposure_min: double;
    stoploop: boolean;
    bayerpatt: integer;
    measurements: array of Tlightinfo;
    Fcamera: T_camera;
    FonShowMessage: TNotifyMsg;
    procedure msg(txt: string; level: integer = 3);
    procedure InstructionsAdd(txt: string);
    procedure SetROI;
    function Takeimage(exp: double; typeof {FLAT, DARK, LIGHT, BIAS}: TFrameType): boolean;
    function median(img: Timafloat): double;//median value of an image
    function median_of_median(exp: double; nrframes: integer; typeof {FLAT, DARK, LIGHT, BIAS}: TFrameType): double;//median of medians

    function bitdepth(img1: Timafloat): integer;//find the bit depth of the data
    function max(img: Timafloat): single;//max value of an image
    function getbayer: integer;//find out which pixels are green sensitive
    function extract_pixel(img : Timafloat; j,i :integer): double;//extra a pixel value from image. In case of OSC image report -999 if not a green pixel.
    procedure measure_rtn(sd,mean:  double; img : Timafloat; out rtn_perc, rtn_rms :double);{calculate the rtn (hotpixels) part and the RMS value of it}
    procedure stdev2(img1, img2: Timafloat; out  sd0, sd,rtn,rtn_rms: double); //find standard deviation of a single image using two images and avoid pixel-to-pixel variations in the sensitivity of the CCD, known as the flat field effect.
    procedure median_and_stdev(exp: double; nrframes: integer;  typeof {FLAT, DARK, LIGHT, BIAS}: TFrameType;out the_stdev0, the_stdev, the_median, the_RTN, the_RTNrms: double);//calculate stdev and median using several exposures

    procedure update_temperature_reading;
    procedure draw_linearity_line(xylist: xy_list; nr: integer);

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
  step: integer;

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



procedure Tf_sensoranalysis.msg(txt: string; level: integer = 3);
begin
  if assigned(FonShowMessage) then FonShowMessage('Sensor analysis: ' + txt, level);
end;

procedure Tf_sensoranalysis.InstructionsAdd(txt: string);
begin
  Instructions.Lines.Add(txt);
  // on Linux the memo do not go automatically to the last line
  Instructions.SelStart := Instructions.GetTextLen - 1;
  Instructions.SelLength := 0;
end;

procedure Tf_sensoranalysis.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self);
  Chart1LineSeries1.SeriesColor:=colorBlue;
  Chart1LineSeries1.LinePen.Color:=colorBlue;
  Chart2LineSeries1.SeriesColor:=colorBlue;
  Chart2LineSeries1.LinePen.Color:=colorBlue;
  Chart3LineSeries1.SeriesColor:=colorBlue;
  Chart3LineSeries1.LinePen.Color:=colorBlue;
  Chart3LineSeries2.SeriesColor:=colorRed;
  Chart3LineSeries2.LinePen.Color:=colorRed;
  Chart4LineSeries1.SeriesColor:=colorBlue;
  Chart4LineSeries1.LinePen.Color:=colorBlue;
  Chart5BarSeries1.SeriesColor:=colorGray;
  Chart5BarSeries1.BarBrush.Color:=colorGray;
  Chart6LineSeries1.SeriesColor:=colorGreen;
  Chart6LineSeries1.LinePen.Color:=colorGreen;
  Chart6LineSeries2.SeriesColor:=colorBlue;
  Chart6LineSeries2.LinePen.Color:=colorBlue;
  Chart6LineSeries3.SeriesColor:=colorRed;
  Chart6LineSeries3.LinePen.Color:=colorRed;
end;


procedure Tf_sensoranalysis.FormShow(Sender: TObject);
begin
  exposure_min := Math.min(0.01, Math.max(camera.ExposureRange.min, 0.0));
  //protect againt 9999 or -9999 values

  Gain1.Enabled := camera.CanSetGain;
  Gain2.Enabled := camera.CanSetGain;
  Gain3.Enabled := camera.CanSetGain;
  Gain4.Enabled := camera.CanSetGain;
  Gain5.Enabled := camera.CanSetGain;
  Gain6.Enabled := camera.CanSetGain;
  Gain7.Enabled := camera.CanSetGain;
  Gain8.Enabled := camera.CanSetGain;

  Gain1.MaxValue := camera.GainMax;
  Gain1.MinValue := camera.GainMin;
  Gain2.MaxValue := camera.GainMax;
  Gain2.MinValue := camera.GainMin;
  Gain3.MaxValue := camera.GainMax;
  Gain3.MinValue := camera.GainMin;
  Gain4.MaxValue := camera.GainMax;
  Gain4.MinValue := camera.GainMin;
  Gain5.MaxValue := camera.GainMax;
  Gain5.MinValue := camera.GainMin;
  Gain6.MaxValue := camera.GainMax;
  Gain6.MinValue := camera.GainMin;
  Gain7.MaxValue := camera.GainMax;
  Gain7.MinValue := camera.GainMin;
  Gain8.MaxValue := camera.GainMax;
  Gain8.MinValue := camera.GainMin;

  Gain1.Value := camera.GainMin;

  if camera.GainMin <> 0 then
  begin
    Gain2.Value := round(2 * camera.GainMin);
    Gain3.Value := round(3 * camera.GainMin);
    Gain4.Value := round(5 * camera.GainMin);
  end
  else
  begin
    Gain2.Value := round(0.01 * camera.GainMax);
    Gain3.Value := round(0.02 * camera.GainMax);
    Gain4.Value := round(0.04 * camera.GainMax);
  end;

  Gain5.Value := round(0.1 * camera.GainMax);
  Gain6.Value := round(0.2 * camera.GainMax);
  Gain7.Value := round(0.4 * camera.GainMax);
  Gain8.Value := camera.GainMax;

  Offset1.Enabled := camera.hasOffset;
  Offset1.MaxValue := camera.OffsetMax;
  Offset1.MinValue := camera.OffsetMin;
  if Offset1.Enabled then
    Offset1.Value := camera.Offset;

  chart1.Title.Text.setStrings('Linearity      (' + camera.ccdname + ')');
  chart2.Title.Text.setStrings('Gain in e-/adu      (' + camera.ccdname + ')');
  chart3.Title.Text.setStrings('Read noise and Random Telegraph Noise     (' + camera.ccdname + ')');
  chart4.Title.Text.setStrings('Full well capacity of each pixel in e-      (' +  camera.ccdname + ')');
  Chart6.Title.Text.setStrings('Dark current and total noise      (' + camera.ccdname + ')');

  StepButton1.Caption := 'Start';
  Instructions.Lines.Text := 'Camera sensor analyses' + crlf +
    crlf +
    'Your camera has to be attached to a telescope or lens.' + crlf +
    'First you have to place a flat panel or substitute.' + crlf +
    'In the second step darks will be made so you will need to cover the telescope.'
    +
    crlf + crlf +
    'If flat panel is placed then press "Take lights" to start the test.';

  model1.caption:=camera.ccdname;//report camera name in info tab
  update_temperature_reading;

  StepButton1.Enabled := True;
  StepButton1.Caption := 'Take lights';
  step := 0;
end;


procedure Tf_sensoranalysis.update_temperature_reading;
begin
  temperature_sensor2.caption:=FormatFloat(f2, camera.Temperature) + ' °C';
  temperature_sensor1.Caption := '🌡  ' + temperature_sensor2.caption;
end;


procedure Tf_sensoranalysis.TabSheet4Show(Sender: TObject);
begin
  exposuremin1.Caption := FormatFloat(f6,camera.ExposureRange.min);
  exposuremax1.Caption := FormatFloat(f0,camera.ExposureRange.max);
  LabelFullwellcapacity1.Caption := FormatFloat(f0, camera.FullWellCapacity);
  LabelMaxAdu1.Caption := FormatFloat(f0, camera.MaxADU);
end;


procedure Tf_sensoranalysis.ButtonCloseClick(Sender: TObject);
begin
  stoploop := True;
  Close;
end;


procedure Tf_sensoranalysis.Button1Click(Sender: TObject);
begin
  stringgrid1.selection := rect(0, 0, 5, 99);
  stringgrid1.CopyToClipboard;
end;


procedure Tf_sensoranalysis.Button2Click(Sender: TObject);
begin
  stoploop := True;
  InstructionsAdd('Abort pressed. Will stop soon.');
end;


procedure Tf_sensoranalysis.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  measurements := nil;
end;


procedure Tf_sensoranalysis.SetROI;
var
  sx, sy: integer;
begin
  w := 250;
  h := 250;
  sx := round(camera.MaxX - w) div 2;
  sy := round(camera.MaxY - h) div 2;
  camera.SetFrame(sx, sy, w, h);
end;


function Tf_sensoranalysis.Takeimage(exp: double;
  typeof {FLAT, DARK, LIGHT, BIAS}: TFrameType): boolean;
var
  bin, poffset: integer;
begin
  SetROI;
  fits.SetBPM(bpm, 0, 0, 0, 0);
  fits.DarkOn := False;
  bin := 1;
  poffset := Offset1.Value;
  // ensure exposure is in valid range
  if typeof<>BIAS then exp:=Math.max(exposure_min,exp);
  msg(copy(FrameName[Ord(typeof)], 1, 6) + 'exposure=' + FormatFloat(f3, exp) +
    ' binning=' + IntToStr(bin));
  if not camera.ControlExposure(exp, bin, bin, typeof, ReadoutModeCapture, gain, poffset) then
  begin
    msg(rsExposureFail, 1);
    Result := False;
    exit;
  end;
  // update image size to what the camera really return
  w:=Fits.HeaderInfo.naxis1;
  h:=Fits.HeaderInfo.naxis2;
  Result := True;
end;


function Tf_sensoranalysis.getbayer: integer;//find out which pixels are green sensitive
var
  buf: string;
begin
  buf := copy(Ffits.HeaderInfo.bayerpattern, 1, 2);
  // use value from header
  if buf = 'GR' then Result := 1
  else
  if buf = 'GB' then Result := 1
  else
  if buf = 'RG' then Result := 2
  else
  if buf = 'BG' then Result := 2
  else
    Result := 0; //mono sensor

  if Ffits.headerInfo.roworder <> bottomup then //flip pattern vertical
  begin
    if Result = 1 then Result := 2
    else
    if Result = 2 then Result := 1;
  end;

  if odd(Ffits.headerInfo.bayeroffsetX) <> odd(Ffits.headerInfo.bayeroffsetY) then
    // green pattern is flipped
  begin
    if Result = 1 then Result := 2
    else
    if Result = 2 then Result := 1;
  end;
end;


function Tf_sensoranalysis.extract_pixel(img : Timafloat; j,i :integer): double;//extra pixel value from image. In case of OSC image report -999 if not a green pixel.
begin
  if bayerpatt = 0 then // mono
    result := img[0, j, i]
  else //pattern GR or GB
  if ((bayerpatt = 1) and (((odd(i) = False) and (odd(j) = False)) or
                           ((odd(i) = True) and (odd(j) = True)))) then  // green one
    result:= img[0, j, i]
  else //pattern RG or BG
  if ((bayerpatt = 2) and (((odd(i) = True) and (odd(j) = False)) or
                           ((odd(i) = False) and (odd(j) = True)))) then  //green two
    result := img[0, j, i]
  else
  result:=1E99;//mark as invalid
end;


function Tf_sensoranalysis.median(img: Timafloat): double;
  //median value of an image, If OSC then green channel only
var
  i, j, counter: integer;
  value        : double;
  median_array: array of double;
begin
  setlength(median_array, w * h);
  counter := 0;

  for i := 0 to w - 1 do
    for j := 0 to h - 1 do
    begin
      value:=extract_pixel(img,j,i);
      if value<1E90 then
      begin
         median_array[counter] := value;
         Inc(counter);
      end;
    end;
  result := smedian(median_array, counter);
  median_array := nil;
end;


function Tf_sensoranalysis.max(img: Timafloat): single;//max value of an image
var
  i, j: integer;
begin
  Result := 0;
  for i := 0 to w - 1 do
    for j := 0 to h - 1 do
    begin
      Result := Math.max(Result, img[0, j, i]);
    end;
end;


function Tf_sensoranalysis.median_of_median(exp: double; nrframes: integer;
  typeof {FLAT, DARK, LIGHT, BIAS}: TFrameType): double;//median of medians
label
  999;
var
  i: integer;
  valuesM: array of double;
begin
  Result := 0;
  Result := 0;
  setlength(valuesM, nrframes);
  for i := 0 to nrframes - 1 do
  begin
    if Takeimage(exp, typeof) then
      valuesM[i] := median(Ffits.image)
    else
      goto 999;
    if stoploop then goto 999;
  end;
  Result := smedian(valuesM, nrframes);//median of the median values
  999:
    valuesM := nil;
end;


function Tf_sensoranalysis.bitdepth(img1: Timafloat): integer;
  //find the bit depth of the data
var
  histogram: array of integer;
  i, j, step, minstep: integer;
begin
  setlength(histogram, 65536);
  for i := 0 to 65535 do histogram[i] := 0;

  Result := 0;
  for i := 0 to w - 1 do
    for j := 0 to h - 1 do
    begin
      Inc(histogram[round(img1[0, j, i])], 1);
    end;

  minstep := 99999;
  step := 99999;
  for i := 0 to 65535 do
  begin
    if histogram[i] <> 0 then
    begin
      minstep := Math.min(minstep, step);
      step := 1;
    end
    else
      Inc(step);
  end;
  minstep := minstep;
  if minstep > 1 then
    Result := 16 - round(sqrt(minstep))
  else
    Result := 16;

  histogram := nil;
end;


procedure Tf_sensoranalysis.measure_rtn(sd,mean:  double; img : Timafloat; out rtn_perc, rtn_rms :double);{calculate the rtn (hotpixels) part and the RMS value of it}
var i,j,counter,counter2 : integer;
    value                : double;

begin
  rtn_rms:=0;
  counter:=0;
  counter2:=0;
  for i := 0 to w - 1 do
  for j := 0 to h - 1 do
  begin
    value:=abs(img[0,j,i]-mean);
    if value<=3*sd then {ignore outliers}
      inc(counter)
    else
    begin
      rtn_rms:=rtn_rms+sqr(value);
      inc(counter2);
    end;
  end;
  if counter2>0 then
    rtn_rms:=sqrt(rtn_rms/counter2)
  else
    rtn_rms:=0;
    rtn_perc:=100*(0.997 - counter/(counter+counter2));//Part effected by hot pixels. Within sigma 3.0,  99.73% remains.
end;


procedure Tf_sensoranalysis.stdev2(img1, img2: Timafloat; out  sd0, sd,rtn,rtn_rms: double); //find standard deviation of a single image using two images and avoid pixel-to-pixel variations in the sensitivity of the CCD, known as the flat field effect.
var
  i, j, counter, iterations: integer;
  mean, meanx, Value, sd_old: double;
  img3: Timafloat;
begin
  //calculate the difference between two images
  setlength(img3, 1, h, w);
  for i := 0 to w - 1 do
    for j := 0 to h - 1 do
      img3[0, j, i] := img1[0, j, i] - img2[0, j, i];

  sd := 99999;
  mean := 0;

  iterations := 0;
  repeat
    {mean}
    counter := 0;
    meanx := 0;
    for i := 0 to w - 1 do
      for j := 0 to h - 1 do
      begin
        value:=extract_pixel(img3,j,i);
        if value<1E90 then
        if ((iterations = 0) or (abs(Value - mean) <= 3 * sd)) then  {ignore outliers after first run}
        begin
          Inc(counter);
          meanx := meanx + Value; {mean}
        end;
      end;{filter outliers}
    if counter <> 0 then mean := meanx / counter {calculate the mean};

    {sd using sigma clip}
    sd_old := sd;
    counter := 0;
    for i := 0 to w - 1 do
      for j := 0 to h - 1 do
      begin
        value:=extract_pixel(img3,j,i);
        if value<1E90 then
        if ((iterations = 0) or (abs(Value - mean) <= 3 * sd_old)) then  {ignore outliers after first run}
        begin
          sd := sd + sqr(mean - Value);
          Inc(counter);
        end;
      end;
    if counter <> 0 then sd := sqrt(sd / counter);

    if iterations = 0 then sd0 := sd;//standard deviation without sigma clip
    Inc(iterations);
  until (((sd_old - sd) < 0.03 * sd) or (iterations >= 7)); {repeat until sd is stable or 7 iterations}

  measure_rtn(sd,mean,img3, {out} rtn, rtn_rms);{calculate the hotpixels percentage and RMS value of the DIFFERENCE. So use sd of the difference and not sd/sqrt(2)}

  sd0 := sd0 / sqrt(2); // Standard deviation. Corrected for combined noise of two images subtracted
  sd := sd / sqrt(2);   // Standard deviation using sigma clip. Correct for combined noise of two images subtracted
  img3 := nil;
end;


procedure Tf_sensoranalysis.median_and_stdev(exp: double; nrframes: integer;  typeof {FLAT, DARK, LIGHT, BIAS}: TFrameType;out the_stdev0, the_stdev, the_median, the_RTN, the_RTNrms: double);//calculate stdev and median using several exposures
label
  999;
var
  i: integer;
  image3: Timafloat;
  mean  : double;
  valuesSD0, valuesSD, valuesM,valuesRTN,valuesRTNrms: array of double;
begin
  the_median := 0;
  setlength(valuesSD0, nrframes);
  setlength(valuesSD, nrframes);
  setlength(valuesM, nrframes);
  setlength(valuesRTN, nrframes);
  setlength(valuesRTNrms, nrframes);
  for i := 0 to nrframes - 1 do
  begin
    if Takeimage(exp, typeof) then
    begin
      image3 := Ffits.image;
      setlength(image3, 1, h, w); //duplicate
      if Takeimage(exp, typeof) then
      begin
        stdev2(image3, Ffits.image, valuesSD0[i], valuesSD[i],valuesRTN[i],valuesRTNrms[i])  //calculate standard deviation
      end
      else
      begin
        the_stdev := 0;
        goto 999;
      end;
      valuesM[i] := (median(image3) + median(Ffits.image)) / 2;
    end
    else
    begin
      the_stdev := 0;
      goto 999;
    end;
    if stoploop then goto 999;
  end;
  the_stdev0:=smedian(valuesSD0, nrframes);
  the_stdev:=smedian(valuesSD, nrframes);
  the_median:=smedian(valuesM, nrframes);//median of the median values
  the_RTN:=smedian(valuesRTN, nrframes);//median of the median values
  the_RTNrms:=smedian(valuesRTNrms, nrframes);//median of the median values

  999:
    image3 := nil;
    valuesSD0 := nil;
    valuesSD := nil;
    valuesM := nil;
    valuesRTN:=nil;
    valuesRTNrms:=nil
end;


procedure trendline(xylist: xy_list; len{length xy_list}: integer;  out slope, intercept: double); //find linear trendline Y = magnitude_slope*X + intercept
var                                                                                                //idea from https://stackoverflow.com/questions/43224/how-do-i-calculate-a-trendline-for-a-graph
  sumX, sumX2, sumY, sumXY, median, mad: double;
  Count, i: integer;
begin
  Count := 0;
  sumX := 0;
  sumX2 := 0;
  sumY := 0;
  sumXY := 0;

  for i := 0 to len - 1 do
  begin
    Inc(Count);
    sumX := sumX + xylist[0, i]; //sum X= sum exposure
    sumX2 := sumx2 + sqr(xylist[0, i]);
    sumY := sumY + xylist[1, i]; //sum Y= sum adu's;
    sumXY := sumXY + xylist[0, i] * xylist[1, i];
  end;

  Slope := (Count * sumXY - sumX * sumY) / (Count * sumX2 - sqr(sumX));  // b = (n*Σ(xy) - ΣxΣy) / (n*Σ(x^2) - (Σx)^2)
  Intercept := (sumY - Slope * sumX) / Count;                            // a = (Σy - bΣx)/n
end;


procedure Tf_sensoranalysis.draw_linearity_line(xylist: xy_list; nr: integer);
var
  i: integer;
  slope, intercept, err, maxerr: double;
  mess: string;
begin
  trendline(xylist, 10, slope, intercept);//find trendline for 0..90% is position 0..9

  maxerr := 0;
  for i := 0 to nr do
  begin
    err := (xylist[1, i] - (xylist[0, i] * slope + intercept)) * 100 / 65535;
    Chart1LineSeries1.addxy(xylist[0, i], xylist[1, i], FormatFloat(f2,err) + '%');
    {Now we have to make sure that the labels are displayed. For this purpose, the TChartSeries which is an ancestor of TLineSeries has a property Marks.
    In the sub-properties you find the option Style which is set to smsNone by default, meaning that no labels are displayed.
    You see in the object inspector that there is a variety of information than can be displayed in the marks, but you'll need here the option smsLabel which shows the text of the ChartDataItems.}
    if i <= 9 then maxerr := Math.max(maxerr, abs(err));
  end;
  mess := FormatFloat(f3,maxerr) + '%';
  InstructionsAdd('Max linearity error is ' + mess + ' in range [0..90%]');
  chart1.Title.Text.setStrings('Linearity. Maximum error ' + mess + ' in range [0..90%]. Gain is ' + IntToStr(gain));
end;


procedure Tf_sensoranalysis.StepButton1Click(Sender: TObject);
var
  saturationlevel, correction, stepexposure, themedian,
  oldthemedian, median_dark_adu, sigma_light_adu, exposure_lin, sd_RTN_dark_adu2, sd_dark_adu2, median_dark_adu2, dark_current_adu, dark_current_es,
  total_noise, total_noise_e, readnoise2_e, dark_current2_e, dark_current2_es,  readnoise_RTN2_e, total_noise_RTN_e, dark_current_RTN2_es,RTN_perc,RTN_rms : double;
  i, gainstep, nr :  integer;
  message:  string;
  Save_Cursor: TCursor;
  xylist: xy_list;
const
  level7 = 0.7;// 70% saturation for testing.

  procedure prepare_stop;
  begin
    StepButton1.Caption := 'Restart';
    StepButton1.Enabled := True;
    Screen.Cursor := Save_Cursor;
    step := 0;
    InstructionsAdd('Read to restart');
  end;

begin
  stoploop := False;

  if camera.CanSetGain = False then
  begin
    InstructionsAdd(crlf + crlf +
      'Fatal failure!! Can not set gain. Allow setting the camera gain in Preference, Camera');
    exit;
  end;

  Save_Cursor := Screen.Cursor;
  Screen.Cursor := crHourglass; { Show hourglass cursor }

//  gain:=139;
//  median_and_stdev(1, repeats1.Value, DARK,{out}sd_RTN_dark_adu, sd_dark_adu, median_dark_adu,RTN_perc,RTN_rms);//take an exposure of 1.01 seconds to avoid weird variations in the first second.
//    RTN_rms:=RTN_rms* 1 / 16;//convert to e-
//    InstructionsAdd('Percentage of RTN found after '+ IntToStr(dark_current_test_duration1.Value + 1) + ' sec:  ' + floattostrF(RTN_perc, FFfixed, 0, 2)+'%, RMS value '+ floattostrF(RTN_rms, FFfixed, 0, 2)+
//    ' e-. Random Telegraph Noise is here defined as the number of pixels with a value more then 3 sigma above the Gaussian noise level in the difference betweem two darks.');
//  exit;



  case step of
    0:begin //taking lights
        Chart1LineSeries1.Clear;//clear old charts
        Chart2LineSeries1.Clear;//clear old charts
        Chart3LineSeries1.Clear;//clear old charts
        Chart3LineSeries2.Clear;//clear old charts
        Chart4LineSeries1.Clear;//clear old charts
        Chart5BarSeries1.Clear;//clear old charts
        Chart6LineSeries1.Clear;//clear old charts
        Chart6LineSeries2.Clear;//clear old charts
        Chart6LineSeries3.Clear;//clear old charts

        StepButton1.Enabled := False;
        StepButton1.Caption := '.....';
        InstructionsAdd('Working on the LIGHTS....' + crlf);

        // find bias level
        exposure := 0.0;
        biaslevel := median_of_median(exposure, repeats1.Value, BIAS {FLAT, DARK, LIGHT, BIAS}); //find best median
        InstructionsAdd('Bias level: ' + FormatFloat(f0,biaslevel));

        bayerpatt := getbayer;

        if bayerpatt = 0 then
        begin
          InstructionsAdd('Mono sensor detected');
          sensor_type1.caption:='mono';
        end
        else
        begin
          InstructionsAdd('Colour sensor detected. Will use only green for linearity test.' + crlf);
          sensor_type1.caption:='colour '+Ffits.HeaderInfo.bayerpattern;
        end;

        nrgainsteps := 8 - 1;
        setlength(measurements, nrgainsteps + 1);
        measurements[0].gain := Gain1.Value;
        measurements[1].gain := Gain2.Value;
        measurements[2].gain := Gain3.Value;
        measurements[3].gain := Gain4.Value;
        measurements[4].gain := Gain5.Value;
        measurements[5].gain := Gain6.Value;
        measurements[6].gain := Gain7.Value;
        measurements[7].gain := Gain8.Value;

        gainstep := 0;

        while gainstep <= nrgainsteps do  //do all gains
        begin
          update_temperature_reading;
          gain := measurements[gainstep].gain;
          if gainstep = 0 then exposure := 0.1
          else
          begin
            if measurements[gainstep - 1].gain > 0 then // extrapolating from gain 0 does not work
              exposure := measurements[gainstep - 1].exposure * measurements[gainstep - 1].gain / measurements[gainstep].gain;//assuming gain is linear
          end;

          InstructionsAdd('Testing gain: ' + IntToStr(gain));
          for i := 0 to 9 do
          begin
            if Takeimage(exposure, FLAT) then
            begin
              measurements[gainstep].median_light_adu := median(Ffits.image);
              saturationlevel:=(measurements[gainstep].median_light_adu - biaslevel) / camera.MaxADU;
              InstructionsAdd('Trying to find the exposure time for 70% saturation. Exposure time: ' + FormatFloat(f3,exposure) +
                              ', saturation level: ' + FormatFloat(f0, saturationlevel * 100) + '%');
              if ((saturationlevel> 0.65) and (saturationlevel < 0.75 )) then break;//exposure is good

              exposure := min(30, exposure * level7 / saturationlevel);   //try to get 70% exposed. level7=0.7
            end;
            if stoploop then
            begin
              prepare_stop;
              exit;
            end;
            if (exposure >= 30) then break;
          end; //for loop
          if ((i = 9) or (exposure >= 30)) then
          begin
            InstructionsAdd('Abort. Can not find a correct exposure time. Check flat panel' + crlf);
            prepare_stop;
            exit;
          end;

          //store information
          measurements[gainstep].gain := gain;
          measurements[gainstep].exposure := exposure;

          median_and_stdev(exposure, repeats1.Value, FLAT, {out}measurements[gainstep].sd_RTN_light_adu, measurements[gainstep].sd_light_adu, measurements[gainstep].median_light_adu,RTN_perc, RTN_rms); //measure median value and stdev0, stdev.

          if gainstep = 0 then
          begin
            bit_depth := bitdepth(Ffits.image);
            InstructionsAdd('Bit depth image is: ' + IntToStr(bit_depth));
            bitsperpixel1.caption:=IntToStr(bit_depth);
          end;

          if stoploop then
          begin
            prepare_stop;
            exit;
          end;

          if (((gainstep = 0) and (lin1.Checked)) or
            ((gainstep = 1) and (lin2.Checked)) or
            ((gainstep = 2) and (lin3.Checked)) or
            ((gainstep = 3) and (lin4.Checked)) or
            ((gainstep = 4) and (lin5.Checked)) or
            ((gainstep = 5) and (lin6.Checked)) or
            ((gainstep = 6) and (lin7.Checked)) or
            ((gainstep = 7) and (lin8.Checked))) then
          begin
            InstructionsAdd('Testing linearity.' + crlf);
            stepexposure := exposure / (level7 * 10);//exposure difference to get 10% more level

            oldthemedian := 0;
            for nr := 0 to 30 do  //Saturate the image
            begin
              if stoploop then
              begin
                prepare_stop;
                exit;
              end;
              exposure_lin := Math.max(exposure_min, stepexposure * nr); //about 10%. Minimum 1 ms should be possible for all cameras
              themedian := median_of_median(exposure_lin, repeats1.Value, FLAT); //median of medians


              StringGrid1.InsertRowWithValues(stringgrid1.rowcount,[IntToStr(nr), FormatFloat(f0,measurements[gainstep].Gain), '', '', '', '', FormatFloat(f3,exposure_lin), FormatFloat(f0, themedian), FormatFloat(f3, (themedian - oldthemedian) * 100 / 65535)]);
              StringGrid1.Row := stringgrid1.rowcount;//scroll

              if (themedian - oldthemedian) < 0.1 then break; //saturation reached
              oldthemedian := themedian;
              xylist[0, nr] := exposure_lin;
              xylist[1, nr] := themedian;
              if stoploop then
              begin
                prepare_stop;
                exit;
              end;
            end;//for loop

            draw_linearity_line(xylist, nr - 1);
          end;

          if gainstep = 8 - 1 then //find sat level
          begin
            if Takeimage(3 * exposure, FLAT) then  //expose at 4*70%
              sat_level_adu := max(Ffits.image);
            InstructionsAdd('Test saturation level. Exposure time: ' + FormatFloat(f3, 3 * exposure) + ', saturation level: ' + FormatFloat(f0, sat_level_adu) + ' adu');
          end;
          Inc(gainstep);
        end; //while

        InstructionsAdd(crlf +  'Place the cap on the telescope for making darks.' + crlf + crlf + 'If ready press button "Take darks"');
        StepButton1.Caption := '■■■ Take darks ■■■';
        StepButton1.Enabled := True;
        step := 1;
      end;


    1:begin  //taking darks and final process
        StepButton1.Enabled := False;
        StepButton1.Caption := '.....';
        gainstep := 0;
        while gainstep <= nrgainsteps do  //do all gains
        begin
          update_temperature_reading;

          if gainstep = 0 then InstructionsAdd('Working on the DARKS....');
          exposure := measurements[gainstep].Exposure;
          gain := measurements[gainstep].gain;

          if ((gainstep = 0) and (Takeimage(exposure, DARK))) then //First dark is ignored since in some cameras (Touptek) the pedestal value could be higher in the first dark after a bright flat exposure')
          begin  //so this dark has two functions. 1) Skip first invalid dark and 2) test if flat panel is removed.
            themedian := median(Ffits.image);
            InstructionsAdd('Took dark with gain ' + FormatFloat(f0, gain) + ' and exposure ' + FormatFloat(f3, measurements[gainstep].exposure) + ' to remove persistent charge.  Median value ' + FormatFloat(f3,themedian));

            if themedian > 0.5 * measurements[0].median_light_adu then
            begin
              InstructionsAdd(
                'Flat panel is still present. Remove panel, place the telescope cap and press again "Take darks".');
              StepButton1.Enabled := True;
              StepButton1.Caption := '■■■ Take darks ■■■';
              Screen.Cursor := Save_Cursor;
              exit;
            end;
            Chart5BarSeries1.addxy(gainstep - 1, themedian, ' Gain ' + IntToStr(gain) + ', ');

          end;
          InstructionsAdd('Taking dark(s) with gain ' + FormatFloat(f0, gain) + ' and exposure ' + FormatFloat(f3, measurements[gainstep].exposure));
          median_and_stdev(measurements[gainstep].exposure, repeats1.Value, DARK,{out}sd_RTN_dark_adu, sd_dark_adu, median_dark_adu,RTN_perc,RTN_rms);  //as stdev but do it nrframes times and report median value as result

          Chart5BarSeries1.addxy(gainstep, median_dark_adu, ' Gain ' + IntToStr(gain) + ', ');

          flux_adu := measurements[gainstep].median_light_adu - median_dark_adu; //calculate median flux value of one pixel

          //corrections for gain in the camera driver. E.g. for 12 bit ASI1600 with an output range of 0..65535 the correction factor is 16
          if bit_depth <> 16 then
            correction := round(sat_level_adu / power(2, bit_depth))
          else
            correction := 1;

          measurements[gainstep].gain_e{e-/adu} := (flux_adu / correction) / sqr(measurements[gainstep].sd_light_adu / correction);

          measurements[gainstep].readnoise_e{e-} := (sd_dark_adu / correction) * measurements[gainstep].gain_e;
          measurements[gainstep].readnoise_RTN_e{e-} :=(sd_RTN_dark_adu / correction) * measurements[gainstep].gain_e;
          measurements[gainstep].fw_capacity_e :=(sat_level_adu / correction) * measurements[gainstep].gain_e;

          StringGrid1.InsertRowWithValues(stringgrid1.rowcount,
            [IntToStr(gainstep + 1), FormatFloat(f0, measurements[gainstep].Gain),
            FormatFloat(f3, measurements[gainstep].gain_e),
            FormatFloat(f3, measurements[gainstep].readnoise_e),
            FormatFloat(f3, measurements[gainstep].readnoise_RTN_e),
            FormatFloat(f0, measurements[gainstep].fw_capacity_e),
            FormatFloat(f3, measurements[gainstep].exposure),
            FormatFloat(f0, measurements[gainstep].median_light_adu) + '-' + FormatFloat(f0, median_dark_adu)]);
          StringGrid1.Row := stringgrid1.rowcount;//scroll

          Chart2LineSeries1.addxy(measurements[gainstep].Gain, measurements[gainstep].gain_e);
          Chart3LineSeries1.addxy(measurements[gainstep].Gain, measurements[gainstep].readnoise_e);//read noise
          Chart3LineSeries2.addxy(measurements[gainstep].Gain, measurements[gainstep].readnoise_RTN_e);//read noise plus RTN
          Chart4LineSeries1.addxy(measurements[gainstep].Gain, measurements[gainstep].fw_capacity_e);//full well capacity

          Inc(gainstep);
        end;//while

        if radiobutton9.Checked = False then
        begin
          InstructionsAdd('Testing dark current');

          if radiobutton1.Checked then gainstep := 0
          else
          if radiobutton2.Checked then gainstep := 1
          else
          if radiobutton3.Checked then gainstep := 2
          else
          if radiobutton4.Checked then gainstep := 3
          else
          if radiobutton5.Checked then gainstep := 4
          else
          if radiobutton6.Checked then gainstep := 5
          else
          if radiobutton7.Checked then gainstep := 6
          else
          if radiobutton8.Checked then gainstep := 7
          else
          if radiobutton1.Checked then gainstep := 8;

          gain := measurements[gainstep].gain; //camera gain

          // one seoond test
          median_and_stdev(1.01, repeats1.Value, DARK,{out}sd_RTN_dark_adu, sd_dark_adu, median_dark_adu,RTN_perc,RTN_rms);//take an exposure of 1.01 seconds to avoid weird variations in the first second.
          RTN_rms:=RTN_rms* measurements[gainstep].gain_e / correction;//convert to e-
          rtn1.caption:= FormatFloat(f2, RTN_perc)+' %';
          rtn_rms1.caption:= FormatFloat(f2, RTN_rms)+' e-';
          gain_used1.Caption:=IntToStr(gain);
          InstructionsAdd('Percentage of RTN found after 1.01 sec: ' + rtn1.caption+', RMS value '+ rtn_rms1.caption);
          InstructionsAdd('Random Telegraph Noise is here defined as the number of pixels with a value more then 3 sigma above the Gaussian noise level in the difference betweem two darks.');

          // long duration test
          median_and_stdev(dark_current_test_duration1.Value + 1.01, repeats1.Value, DARK,{out}sd_RTN_dark_adu2, sd_dark_adu2, median_dark_adu2,RTN_perc,RTN_rms);  //take an exposure of value+1 seconds.
          RTN_rms:=RTN_rms* measurements[gainstep].gain_e / correction;//convert to e-
          InstructionsAdd('Percentage of RTN found after '+ IntToStr(dark_current_test_duration1.Value + 1) + ' sec:  ' + FormatFloat(f2,RTN_perc)+'%, RMS value '+ FormatFloat(f2,RTN_rms)+' e-.');
          InstructionsAdd('Random Telegraph Noise is here defined as the number of pixels with a value more then 3 sigma above the Gaussian noise level in the difference betweem two darks.');


          readnoise2_e := sd_dark_adu * measurements[gainstep].gain_e / correction; // read noise after 1.1 seconds        {1.08}
          readnoise_RTN2_e := sd_RTN_dark_adu * measurements[gainstep].gain_e / correction;  // read noise after 1.1 seconds including random telegraph noise  {1.59}
          total_noise_e := sd_dark_adu2 * measurements[gainstep].gain_e / correction; // total noise after long exposure noise[e-]:=noise_adu * gain_e     {1.63}
          total_noise_RTN_e := sd_RTN_dark_adu2 * measurements[gainstep].gain_e / correction; // total noise after long exposure noise[e-]:=noise_adu * gain_e. Including  including random telegraph noise {2.20}

          InstructionsAdd('Noise after 1.01 sec: ' + FormatFloat(f2, readnoise2_e) + '[e-]');
          InstructionsAdd('Noise after ' + IntToStr(dark_current_test_duration1.Value + 1) + ' sec:  ' + FormatFloat(f2, total_noise_e) + '[e-]');
          InstructionsAdd('Noise including RTN after ' + IntToStr(dark_current_test_duration1.Value + 1) + ' sec:  ' + FormatFloat(f2, total_noise_RTN_e) + '[e-]');

          dark_current_adu := median_dark_adu2 - median_dark_adu;//dark current in adu {4.5}
          dark_current_es := dark_current_adu * measurements[gainstep].gain_e / (dark_current_test_duration1.Value * correction); //dark current in e-/(sec*pixel)  {0.003}
          InstructionsAdd('Δadu after ' + IntToStr(dark_current_test_duration1.Value + 1) + ' sec:  ' + FormatFloat(f0, dark_current_adu));
          InstructionsAdd('Dark current method Δadu: ' + FormatFloat(f4, dark_current_es) + ' [e-/(sec*pixel)]');

          dark_current2_e  :=(sqr(total_noise_e) - sqr(readnoise2_e)); //total dark current after exposure time in e-
          dark_current2_es := dark_current2_e /dark_current_test_duration1.Value; // dark current in e-/(sec*pixel) {0.0037}
          dark_current_RTN2_es :=(sqr(total_noise_RTN_e) - sqr(readnoise_RTN2_e)) / dark_current_test_duration1.Value;// dark current in e-/(sec*pixel)  {0.0057}
          InstructionsAdd('Dark current method Δσ: ' + FormatFloat(f4, dark_current2_es) + ' [e-/(sec*pixel)]');

          if dark_current2_e > 0.5 then //at least 0.5 e-
          begin
            //Plot method Δadu
            for i := 0 to dark_current_test_duration1.Value do
            begin
              total_noise := sqrt(sqr(measurements[gainstep].readnoise_e) + i * dark_current_es);
              Chart6LineSeries1.addxy(i, total_noise); //total noise:=sqrt(sqr(readnoise)+sqr(dark_noise))==> total noise:=sqrt(sqr(readnoise)+dark current)
            end;

            //Plot method Δσ
            //dark_current_adu[e-]:=sqr(σ_end[adu]*gain[e-/adu]) - sqr(σ_read_noise[adu]*gain[e-/adu])    (formula 4)
            Chart6LineSeries2.addxy(0, readnoise2_e);//read noise
            for i := 0 to dark_current_test_duration1.Value do
            begin
              total_noise := sqrt(i * dark_current2_es + sqr(readnoise2_e)); //formula 4
              Chart6LineSeries2.addxy(i, total_noise);
              total_noise := sqrt(i * dark_current_RTN2_es + sqr(readnoise_RTN2_e)); //formula 4
              Chart6LineSeries3.addxy(i, total_noise);
            end;
            darkcurrent1.caption:= FormatFloat(f5, dark_current2_es) +' [e-/(sec*pixel)]';
            message := 'Dark current ' + darkcurrent1.caption+' at' + ' gain ' +gain_used1.Caption+'Temperature '+
                       temperature_sensor2.caption+ '. RTN is '+rtn1.caption+', RMS value of RTN is '+rtn_rms1.caption + '. (' + camera.ccdname + ')';
          end
          else
            message := message + crlf + 'WARNING. Too short exposure time. Only ' +  FormatFloat(f1, dark_current_adu) + ' e- difference. Set exposure time longer.';
          InstructionsAdd(message);

          Chart6.Title.Text.setStrings(message);
          StringGrid1.InsertRowWithValues(stringgrid1.rowcount, ['', message]);
          StringGrid1.Row := stringgrid1.rowcount;//scroll
        end;

        update_temperature_reading;
        InstructionsAdd(crlf + crlf + 'Test completed.');
        StepButton1.Enabled := True;
        StepButton1.Caption := 'Restart';
        step := 0;
      end;
  end;//case

  StringGrid1.InsertRowWithValues(stringgrid1.rowcount, ['']);//space line
  Screen.Cursor := Save_Cursor;
end;

end.
