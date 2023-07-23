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
       gain      : integer;
       exposure,
       mean_light_adu,
       sigma_light_adu  : double;
   end;

type

  { Tf_sensoranalysis }

  Tf_sensoranalysis = class(TForm)
    Button1: TButton;
    ButtonClose1: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    Chart1LineSeries3: TLineSeries;
    Chart1LineSeries4: TLineSeries;
    Chart1LineSeries5: TLineSeries;
    Chart1LineSeries6: TLineSeries;
    Chart1LineSeries7: TLineSeries;
    Chart1LineSeries8: TLineSeries;
    Chart2LineSeries1: TLineSeries;
    Chart3: TChart;
    Chart2: TChart;
    Chart3LineSeries1: TLineSeries;
    Chart3LineSeries2: TLineSeries;
    Chart4: TChart;
    Chart4LineSeries1: TLineSeries;
    exposuremax1: TLabel;
    exposuremin1: TLabel;
    Gain3: TSpinEdit;
    Gain4: TSpinEdit;
    Gain5: TSpinEdit;
    Gain6: TSpinEdit;
    Gain7: TSpinEdit;
    Gain8: TSpinEdit;
    GroupBox1: TGroupBox;
    Label10: TLabel;
    Gain2: TSpinEdit;
    Label11: TLabel;
    Label12: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    LabelFullwellcapacity1: TLabel;
    LabelMaxAdu1: TLabel;
    LabelTemperature1: TLabel;
    linearity_test2: TCheckBox;
    linearity_test3: TCheckBox;
    linearity_test4: TCheckBox;
    linearity_test5: TCheckBox;
    linearity_test6: TCheckBox;
    linearity_test7: TCheckBox;
    linearity_test8: TCheckBox;
    linearity_test1: TCheckBox;
    PageControl1: TPageControl;
    repeats1: TSpinEdit;
    StepButton1: TButton;
    Instruction: TMemo;
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
    procedure Button1Click(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure StepButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FFits: TFits;
    w,h,bit_depth,nrgainsteps : integer;
    biaslevel,read_noise_e,fullwell_capacity_e, flux_adu, sigma_dark_adu,gain_e,exposure,sat_level_adu,exposure_min : double;
    stoploop: boolean;
    lightinfo : array of Tlightinfo;
    Fcamera: T_camera;
    FonShowMessage: TNotifyMsg;
    procedure msg(txt:string; level: integer=3);
    procedure SetROI;
    function Takeimage(exp:double; typeof {FLAT, DARK, LIGHT, BIAS}: TFrameType): boolean;
    function stdev(img1,img2: Timafloat): double;//find standard deviation of a single image using two images and avoid pixel-to-pixel variations in the sensitivity of the CCD, known as the flat field effect.
    procedure mean_and_stdev(exp: double;nrframes :integer; typeof {FLAT, DARK, LIGHT, BIAS}: TFrameType; out  the_stdev, the_mean : double);//calculate stdev and mean using several exposures
    function mean(img1 : Timafloat): double;//mean value of an image
    function median_of_means(exp: double;nrframes :integer;typeof {FLAT, DARK, LIGHT, BIAS}: TFrameType): double;//median of means

    function bitdepth(img1 : Timafloat): integer;//find the bit depth of the data
    function max(img1 : Timafloat): single;//max value of an image
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

For a light with a strong signal:
             σ[e-]:=sqrt(nr_photo_electrons)
             σ[e-]:=sqrt(flux[e-]) (1b)

--------------------------------------------------
Step 1 Measure mean dark value

        mean_dark

Step 2 Measure σ[adu] of a dark
   σ_dark[adu]:=STDEV(dark1-dark2)/sqrt(2) (formula 2)

Step 3 Calculate gain by exposing a light using a flat panel:

     flux[adu]:=mean_px_value - mean_dark_value
     σ_light[adu]:=STDEV(light1-light2)/sqrt(2)

     σ_light[e-]:=sqrt(flux[e-]) (1b)
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

Note a correction for gain in the driver should be applied.E.g ASI1600 12 bit sensor ouput is increased from 0..4096 to 0..65535. A additonal gain factor of 16.
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
//  Exposure.Increment:=camera.ExposureRange.step;
  exposuremin1.caption:=floattostrF(camera.ExposureRange.min,FFfixed,0,6);
  exposure_min:=math.min(0.01, math.max(camera.ExposureRange.min,0.0));//protect againt 9999 or -9999 values
  exposuremax1.caption:=floattostrF(camera.ExposureRange.max,FFfixed,0,0);

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


{  Gain1.value:=139;
  Gain2.value:=139;
  Gain3.value:=139;
  Gain4.value:=139;
  Gain5.value:=139;
  Gain6.value:=139;
  Gain7.value:=139;
  Gain8.value:=139;}

  Offset1.enabled:=camera.hasGain;
  Offset1.MaxValue:=camera.OffsetMax;
  Offset1.MinValue:=camera.OffsetMin;

  chart1.Title.text.setStrings('Linearity      ('+camera.ccdname+')');
  chart2.Title.text.setStrings('e-/adu      ('+camera.ccdname+')');
  chart3.Title.text.setStrings('Read noise      ('+camera.ccdname+')');
  chart4.Title.text.setStrings('Full well      ('+camera.ccdname+')');


  LabelTemperature1.Caption:=FormatFloat(f2,camera.Temperature);
  LabelFullwellcapacity1.Caption:=FormatFloat(f0,camera.FullWellCapacity);
  LabelMaxAdu1.Caption:=FormatFloat(f0,camera.MaxADU);


  StepButton1.caption:='Start';
  Instruction.Lines.text:='Camera sensor analyses'+#10+
                           #10+
                           'Your camera has to be attached to a telescope or lens.'+#10+
                           'First you have to place a flat panel or use an evenly illuminated area to view.'+#10+
                           'In the second step darks will be made so you will need to cover the telescope.'+#10+
                           #10+
                           'If flat panel is placed then press "Take lights".';

  StepButton1.enabled:=true;
  StepButton1.caption:='Take lights';
  step:=0;

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

procedure Tf_sensoranalysis.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  lightinfo:=nil;
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


function  Tf_sensoranalysis.mean(img1 : Timafloat): double;//mean value of an image
var
   i,j    : integer;
begin
  result:=0;
  for i:=0 to w-1 do
  for j:=0 to h-1 do
  begin
    result:=result+img1[0,j,i];
  end;
  result:=result/(w*h);
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


function  Tf_sensoranalysis.median_of_means(exp: double;nrframes :integer;typeof {FLAT, DARK, LIGHT, BIAS}: TFrameType): double;//median of means
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
      valuesM[i]:=mean(Ffits.image)
    else
     goto 999;
  end;
  result:=smedian(valuesM,nrframes);//median of the mean values
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


procedure Tf_sensoranalysis.mean_and_stdev(exp: double;nrframes :integer; typeof {FLAT, DARK, LIGHT, BIAS}: TFrameType; out  the_stdev, the_mean : double);//calculate stdev and mean using several exposures
   label 999;
var
   i         : integer;
   image3    : Timafloat;
   valuesST,valuesM   : array of double;
begin
  the_mean:=0;
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
       valuesM[i]:=(mean(image3)+mean(Ffits.image))/2;
    end
    else
    begin
      the_stdev:=0;
      goto 999;
    end;
  end;
  the_stdev:=smedian(valuesST,nrframes);
  the_mean:=smedian(valuesM,nrframes);//median of the mean values
999:
  image3:=nil;
  valuesST:=nil;
  valuesM:=nil;
end;


procedure Tf_sensoranalysis.StepButton1Click(Sender: TObject);
var
    saturationlevel,correction,stepexposure,themean, oldthemean,mean_dark_adu,sigma_light_adu,exposure_lin : double;
    i,gainstep,nr                                                                                          : integer;
    Save_Cursor:TCursor;
const
     level7=0.7;

begin
  if camera.CanSetGain=false then
  begin
     Instruction.Lines.add(#10+#10+'Fatal failure!! Can not set gain. Allow setting the camera gain in Preference, Camera');
     exit;
  end;

  Save_Cursor := Screen.Cursor;
  Screen.Cursor := crHourglass; { Show hourglass cursor }

  case step of
  0: begin //taking lights

      StepButton1.enabled:=false;
      StepButton1.caption:='.....';
      Instruction.Lines.add('Working on the LIGHTS....'+#10);

      // find bias level
      exposure:=0.0;
      biaslevel:=median_of_means(exposure,repeats1.value,BIAS {FLAT, DARK, LIGHT, BIAS});//find best mean
      Instruction.Lines.add('Bias level: '+floattostrF(biaslevel,FFfixed,0,0));

      nrgainsteps:=8-1;
      setlength(lightinfo,nrgainsteps+1);
      lightinfo[0].gain:=Gain1.value;
      lightinfo[1].gain:=Gain2.value;
      lightinfo[2].gain:=Gain3.value;
      lightinfo[3].gain:=Gain4.value;
      lightinfo[4].gain:=Gain5.value;
      lightinfo[5].gain:=Gain6.value;
      lightinfo[6].gain:=Gain7.value;
      lightinfo[7].gain:=Gain8.value;

      Chart1LineSeries1.title:=inttostr(gain1.value);
      Chart1LineSeries2.title:=inttostr(gain2.value);
      Chart1LineSeries3.title:=inttostr(gain3.value);
      Chart1LineSeries4.title:=inttostr(gain4.value);
      Chart1LineSeries5.title:=inttostr(gain5.value);
      Chart1LineSeries6.title:=inttostr(gain6.value);
      Chart1LineSeries7.title:=inttostr(gain7.value);
      Chart1LineSeries8.title:=inttostr(gain8.value);

      gainstep:=0;
      while gainstep<=nrgainsteps do  //do all gains
      begin
        gain:=lightinfo[gainstep].gain;

        if gainstep=0 then exposure:=0.1
        else
        begin
          if lightinfo[gainstep-1].gain>0 then // extrapolating from gain 0 does not work
          exposure:= lightinfo[gainstep-1].exposure * lightinfo[gainstep-1].gain/lightinfo[gainstep].gain;//assuming gain is linear
        end;

        Instruction.Lines.add('Testing gain: '+inttostr(gain));
        for i:=0 to 9 do
        begin
          if Takeimage(exposure,FLAT) then
          begin
            lightinfo[gainstep].mean_light_adu:= mean(Ffits.image);
            saturationlevel:=(lightinfo[gainstep].mean_light_adu-biaslevel)/camera.MaxADU;

            Instruction.Lines.add('Trying to find the exposure time for 70% saturation. Exposure time: '+floattostrF(exposure,FFfixed,0,3)+', saturation level: '+floattostrF(saturationlevel*100,FFfixed,0,0 )+'%');
            if (((Ffits.imagemean--biaslevel)>0.65*camera.MaxADU) and ((Ffits.imagemean--biaslevel)<0.75*camera.MaxADU)) then break;//exposure is good
            exposure:=min(30,exposure*level7/saturationlevel);//try to get 70% exposed. level7=0.7
           end;
          if stoploop then
          begin
            Screen.Cursor:=Save_Cursor;
            exit;
          end;
          if (exposure>=30) then
            break;
        end; //for loop
        if ((i=9) or (exposure>=30)) then
        begin
          Instruction.Lines.add('Abort. Can not find a correct exposure time. Check flat panel'+#10);
          StepButton1.caption:='Restart';
          StepButton1.enabled:=true;
          step:=0;
          Screen.Cursor:=Save_Cursor;
          exit;
        end;

        //store information
        lightinfo[gainstep].gain:=gain;
        lightinfo[gainstep].exposure:=exposure;

        mean_and_stdev(exposure,repeats1.value,FLAT,{out}lightinfo[gainstep].sigma_light_adu,lightinfo[gainstep].mean_light_adu);//measure mean value and stdev


        if gainstep=0 then
        begin
          bit_depth:=bitdepth(Ffits.image);
          Instruction.Lines.add('Bit depth image is: '+inttostr(bit_depth));
        end;

        if (  ((gainstep=0) and (linearity_test1.checked)) or
              ((gainstep=1) and (linearity_test2.checked)) or
              ((gainstep=2) and (linearity_test3.checked)) or
              ((gainstep=3) and (linearity_test4.checked)) or
              ((gainstep=4) and (linearity_test5.checked)) or
              ((gainstep=5) and (linearity_test6.checked)) or
              ((gainstep=6) and (linearity_test7.checked)) or
              ((gainstep=7) and (linearity_test8.checked)) ) then
        begin
          Instruction.Lines.add('Testing linearity.'+#10);
          stepexposure:=exposure/(level7*10);//exposure difference to get 10% more level

          oldthemean:=0;
          for nr:=0 to 30 do  //Saturate the image
          begin
            if stoploop then begin Screen.Cursor:=Save_Cursor;  exit end;
            exposure_lin:=math.max(exposure_min,stepexposure*nr);//about 10%. Minimum 1 ms should be possible for all cameras

            mean_and_stdev(exposure_lin,repeats1.value,FLAT,{out}sigma_light_adu,themean);//measure mean value and stdev
            StringGrid1.InsertRowWithValues(stringgrid1.rowcount,[inttostr(nr), floattostrF(lightinfo[gainstep].Gain,FFfixed,0,0),'','','',floattostrF(exposure_lin,FFfixed,0,3),floattostrF((themean),FFfixed,0,0),
                                              floattostrF((themean-oldthemean)*100/65535,FFfixed,0,3),floattostrF(sigma_light_adu,FFfixed,0,1)]);
            StringGrid1.Row :=stringgrid1.rowcount;//scroll

            if (themean-oldthemean)<0.1 then break; //saturation reached
            oldthemean:=themean;

            case gainstep of 0: Chart1LineSeries1.addxy(exposure_lin,themean);
                             2: Chart1LineSeries2.addxy(exposure_lin,themean);
                             3: Chart1LineSeries3.addxy(exposure_lin,themean);
                             4: Chart1LineSeries4.addxy(exposure_lin,themean);
                             5: Chart1LineSeries5.addxy(exposure_lin,themean);
                             6: Chart1LineSeries6.addxy(exposure_lin,themean);
                             7: Chart1LineSeries7.addxy(exposure_lin,themean);
                             8: Chart1LineSeries8.addxy(exposure_lin,themean);
            end;//case


          end;//for loop
        end;

        if gainstep=8-1 then //find sat level
        begin
          if Takeimage(3*exposure,FLAT) then  //expose at 4*70%
          sat_level_adu:=max(Ffits.image);
          Instruction.Lines.add('Test saturation level. Exposure time: '+floattostrF(3*exposure,FFfixed,0,3)+', saturation level: '+floattostrF(sat_level_adu,FFfixed,0,0 )+' adu');
        end;

        inc(gainstep);

      end; //while

      Instruction.Lines.add(#10+
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
        if gainstep=0 then Instruction.Lines.add('Working on the DARKS....');
        exposure:=lightinfo[gainstep].Exposure;
        gain:=lightinfo[gainstep].gain;

        if (( gainstep<>0) or (Takeimage(exposure,DARK))) then //First dark is ignored since in some cameras (Touptek) the pedestal value could be higher in the first dark after a bright flat exposure')
        begin                                                  //so this dark has two functions. 1) Skip first invalid dark and 2) test if flat panel is removed.
          themean:=mean(Ffits.image);
          if themean>0.5*lightinfo[0].mean_light_adu then
          begin
            Instruction.Lines.add('Flat panel is still present. Remove panel, place the telescope cap and press again "Take darks".');
            StepButton1.enabled:=true;
            StepButton1.caption:='■■■ Take darks ■■■';
            Screen.Cursor:=Save_Cursor;
            exit;
          end;
        end;

        mean_and_stdev(lightinfo[gainstep].exposure,repeats1.value,DARK,{out}sigma_dark_adu,mean_dark_adu);//as stdev but do it nrframes times and report median value as result

        Instruction.Lines.add('First dark level: '+floattostrF( themean,FFfixed,0,0));//First bias could be different
        Instruction.Lines.add('Second dark level: '+floattostrF(mean_dark_adu,FFfixed,0,0));//First bias could be different

        flux_adu:=lightinfo[gainstep].mean_light_adu-mean_dark_adu;//calculate mean flux value of one pixel

        //corrections for Gain in the camera driver
        if bit_depth<>16 then
          correction:=round(sat_level_adu/power(2,bit_depth))
        else
        correction:=1;

        gain_e{e-/adu}:=(flux_adu/correction)/sqr(lightinfo[gainstep].sigma_light_adu/correction);

        read_noise_e{e-}:=(sigma_dark_adu/correction) * gain_e;
        FullWell_capacity_e:=(sat_level_adu/correction)*gain_e;

        StringGrid1.InsertRowWithValues(stringgrid1.rowcount,[inttostr(gainstep+1), floattostrF(lightinfo[gainstep].Gain,FFfixed,0,0),floattostrF(gain_e,FFfixed,0,3),floattostrF(read_noise_e,FFfixed,0,3),floattostrF(FullWell_capacity_e,FFfixed,0,0),
                                                              floattostrF(lightinfo[gainstep].exposure,FFfixed,0,3),floattostrF(lightinfo[gainstep].mean_light_adu,FFfixed,0,0)+'-'+floattostrF(mean_dark_adu,FFfixed,0,0)]);
        StringGrid1.Row :=stringgrid1.rowcount;//scroll

        Chart2LineSeries1.addxy(lightinfo[gainstep].Gain,gain_e);
        Chart3LineSeries1.addxy(lightinfo[gainstep].Gain,read_noise_e);
        Chart4LineSeries1.addxy(lightinfo[gainstep].Gain,FullWell_capacity_e);

        inc(gainstep);
      end;//while

      Instruction.Lines.add(#10+#10+'Finished.');
      StepButton1.enabled:=true;
      StepButton1.caption:='Restart';
      step:=0;

    end;
  end;//case

  StringGrid1.InsertRowWithValues(stringgrid1.rowcount,['']);//space line
  Screen.Cursor:=Save_Cursor;
end;

end.

