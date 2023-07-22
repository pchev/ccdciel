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
  ExtCtrls, ComCtrls, Spin, Grids;


type
   Tlightinfo = record
       gain      : integer;
       exposure,
       mean_light,
       sigma_light_adu  : double;
   end;

type

  { Tf_sensoranalysis }

  Tf_sensoranalysis = class(TForm)
    Button1: TButton;
    ButtonClose1: TButton;
    exposuremax1: TLabel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LabelFullwellcapacity1: TLabel;
    LabelMaxAdu1: TLabel;
    exposuremin1: TLabel;
    LabelTemperature1: TLabel;
    multiplier1: TFloatSpinEdit;
    Gain2: TSpinEdit;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    StepButton1: TButton;
    Instruction: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Gain1: TSpinEdit;
    Offset: TSpinEdit;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure StepButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FFits: TFits;
    w,h,bit_depth,nrgainsteps : integer;
    biaslevel,read_noise_e,fullwell_capacity_e, flux_adu, sigma_dark_adu,gain_e,exposure,sat_level_adu : double;
    stoploop: boolean;
    lightinfo : array of Tlightinfo;
    light1,dark1 :   Timafloat;
    Fcamera: T_camera;
    FonShowMessage: TNotifyMsg;
    procedure msg(txt:string; level: integer=3);
    procedure SetROI;
    function TakeLight: boolean;
    function TakeFlat: boolean;
    function TakeDark: boolean;
    function TakeBias: boolean;
    function stdev(img1,img2: Timafloat): double;//find standard deviation of a single image using two images and avoid pixel-to-pixel variations in the sensitivity of the CCD, known as the flat field effect.
    function mean(img1 : Timafloat): double;//mean value of an image
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
                       (nr_photo_electrons) ) (1)

Therefore for a dark
             σ[e-]:=readout_noise (1a)

For a light with a strong signal:
             σ[e-]:=sqrt(nr_photo_electrons)
             σ[e-]:=sqrt(flux[e-]) (1b)

--------------------------------------------------
Step 1 Measure mean dark value

        mean_dark

Step 2 Measure σ[adu] of a dark
   σ_dark[adu]:=STDEV(dark1-dark2)/sqrt(2) (2)

Step 3 Calculate gain by exposing a light using a flat panel:

     flux[adu]:=mean_px_value - mean_dark_value
     σ_light[adu]:=STDEV(light1-light2)/sqrt(2)

     σ_light[e-]:=sqrt(flux[e-]) (1b)
     sqr(σ_light[e-]):=flux[e-]
     sqr(σ_light[adu]*gain[e-/adu]):=flux[adu]*gain[e-/adu]
     sqr(σ_light[adu])*sqr(gain[e-/adu]):=flux[adu]*gain[e-/adu]
     gain[e-/adu]:=flux[adu]/sqr( σ_light[adu]) (3)
     Note correction for gain in the driver should be applied


Step 4, Calculate read_noise[e-]

    read_noise[e-]:=σ_dark[adu] * gain[e-/adu]


Step 5 Find saturation level sat_level[adu].

Expose long using a flat panel and measure sat_level[adu]


Step 6 Calculate full well capacity

  FullWell_capacity[e-]:=sat_level[adu]*gain[e-/adu]
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

//  if camera.GainMax=0 then //get camera info
//  begin
//    exposure:=0.0;
//    TakeLight;
//    wait(100);
//  end;

  exposuremin1.caption:=floattostrF(camera.ExposureRange.min,FFfixed,0,6);
  exposuremax1.caption:=floattostrF(camera.ExposureRange.max,FFfixed,0,0);

  Gain1.enabled:=camera.CanSetGain;

  Gain1.MaxValue:=camera.GainMax;
  Gain1.MinValue:=camera.GainMin;
  Gain2.MaxValue:=camera.GainMax;
  Gain2.MinValue:=camera.GainMin;

  Gain1.value:=camera.GainMin;
  Gain2.value:=camera.GainMax;

//  offset.Visible:=camera.hasGain;
  offset.MaxValue:=camera.OffsetMax;
  offset.MinValue:=camera.OffsetMin;

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

  if  camera.CanSetGain=false then
     Instruction.Lines.add('Warning !!. Can not set gain. Allow setting camera gain in Preference, Camera');

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
  dark1:=nil;
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

function Tf_sensoranalysis.TakeLight: boolean;
var bin,poffset: integer;
begin
  SetROI;
  fits.SetBPM(bpm,0,0,0,0);
  fits.DarkOn:=false;
  bin:=1;
  poffset:=Offset.Value;
  msg('Light exposure='+FormatFloat(f3,exposure)+' binning='+inttostr(bin));
  if not camera.ControlExposure(exposure,bin,bin,LIGHT,ReadoutModeCapture,gain,poffset) then begin
      msg(rsExposureFail,1);
      result:=false;
      exit;
  end;
  result:=true;
end;

function Tf_sensoranalysis.TakeFlat: boolean;
var bin,poffset: integer;
begin
  SetROI;
  fits.SetBPM(bpm,0,0,0,0);
  fits.DarkOn:=false;
  bin:=1;
  poffset:=Offset.Value;
  msg('Flat exposure='+FormatFloat(f3,exposure)+' binning='+inttostr(bin));
  if not camera.ControlExposure(exposure,bin,bin,FLAT,ReadoutModeCapture,gain,poffset) then begin
      msg(rsExposureFail,1);
      result:=false;
      exit;
  end;
  result:=true;
end;

function Tf_sensoranalysis.TakeDark: boolean;
var //exp:double;
    bin,poffset: integer;
begin
  SetROI;
  fits.SetBPM(bpm,0,0,0,0);
  fits.DarkOn:=false;
//  exp:=Exposure.Value;
  bin:=1;
  poffset:=Offset.Value;
  msg('Dark exposure='+FormatFloat(f3,exposure)+' binning='+inttostr(bin));
  if not camera.ControlExposure(exposure,bin,bin,DARK,ReadoutModeCapture,gain,poffset) then begin
      msg(rsExposureFail,1);
      result:=false;
      exit
  end;
  result:=true;
end;

function Tf_sensoranalysis.TakeBias: boolean;
var  bin,poffset: integer;
begin
  SetROI;
  fits.SetBPM(bpm,0,0,0,0);
  fits.DarkOn:=false;
  bin:=1;
  poffset:=Offset.Value;
  msg('Bias exposure='+FormatFloat(f3,exposure)+' binning='+inttostr(bin));
  if not camera.ControlExposure(exposure,bin,bin,BIAS,ReadoutModeCapture,gain,poffset) then begin
      msg(rsExposureFail,1);
      result:=false;
      exit
  end;
  result:=true;
end;

function  Tf_sensoranalysis.stdev(img1,img2: Timafloat): double;//find standard deviation of a single image using two images and avoid pixel-to-pixel variations in the sensitivity of the CCD, known as the flat field effect.
var
   i,j    : integer;
begin
  //stdev
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



procedure Tf_sensoranalysis.StepButton1Click(Sender: TObject);
var
    saturationlevel,correction,oldexposure,stepexposure,themean, oldthemean,dummy  : double;
    i,gainstep,nr                                                           : integer;
    Save_Cursor:TCursor;
begin
  Save_Cursor := Screen.Cursor;
  Screen.Cursor := crHourglass; { Show hourglass cursor }

  case step of
  0: begin //taking lights
      StepButton1.enabled:=false;
      StepButton1.caption:='.....';
      Instruction.Lines.add('Working on the LIGHTS....'+#10);

      // find bias level
      exposure:=0.0;
      if TakeBias then
      begin
        biaslevel:=Ffits.imagemean;
        Instruction.Lines.add('Bias level: '+floattostrF(biaslevel,FFfixed,0,0));
      end;


      nrgainsteps:=math.max(1, round( ln(Gain2.value/math.max(1,Gain1.value)) /ln(multiplier1.value))   );

      setlength(lightinfo,nrgainsteps+1);

      gainstep:=0;
      gain:=gain1.value;
      while gainstep<=nrgainsteps do  //do all gains
      begin

       if gain1.value<>0 then
          exposure:=0.1*gain1.value/gain //start low. From low is works better to increase then to saturate.
       else
          exposure:=0.1;

        Instruction.Lines.add('Testing gain: '+inttostr(gain));
        for i:=0 to 9 do
        begin
          if TakeFlat then
          begin
            saturationlevel:=Ffits.imagemean/camera.MaxADU;
            Instruction.Lines.add('Trying to find optimimum exposure time. Exposure time: '+floattostrF(exposure,FFfixed,0,3)+', saturation level: '+floattostrF(saturationlevel*100,FFfixed,0,0 )+'%');
            if ((Ffits.imagemean>0.55*camera.MaxADU) and (Ffits.imagemean<0.8*camera.MaxADU)) then break;//exposure is good
            exposure:=min(30,exposure*0.7/(saturationlevel));//try to get 70% exposed

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


        with Ffits do
        begin
          lightinfo[gainstep].mean_light:= mean(image);
          light1:=image;
          setlength(light1,1,h,w); //duplicate
          if TakeFlat then
            lightinfo[gainstep].sigma_light_adu:=stdev(light1,image); //calculate stdev in ADU
        end;

        if gainstep=0 then
        begin
          bit_depth:=bitdepth(light1);
          Instruction.Lines.add('Bit depth image is: '+inttostr(bit_depth));
          Instruction.Lines.add('Testing linearity.'+#10);
          oldexposure:=exposure;
          stepexposure:=exposure/7;   //Saturate the image

          oldthemean:=0;
          for nr:=0 to 30 do
          begin
            exposure:=MaxValue([0.01,stepexposure*nr]);//about 10%
            if TakeFlat then
            begin
              themean:=mean(Ffits.image);
              StringGrid1.InsertRowWithValues(stringgrid1.rowcount,[inttostr(nr), floattostrF(lightinfo[gainstep].Gain,FFfixed,0,0),'','','',floattostrF(exposure,FFfixed,0,3),floattostrF((themean),FFfixed,0,0), floattostrF((themean-oldthemean)*100/65535,FFfixed,0,3)]);
              StringGrid1.Row :=stringgrid1.rowcount;//scroll
            end;
            if (themean-oldthemean)<0.1 then break; //saturation reached
            oldthemean:=themean;
          end;
          exposure:=oldexposure;
          sat_level_adu:=max(Ffits.image);

        end;

        inc(gainstep);


       gain:=math.min(math.max(1,gain2.value),round(math.max(gain,1)*multiplier1.value));
      end; //while

      Instruction.Lines.add('Place the cap on the telescope for making darks.'+#10+
                            'If ready press button "Take darks"');
      StepButton1.caption:='Take darks';
      StepButton1.enabled:=true;
      step:=1;
    end;


  1: begin  //taking darks and final process

      gainstep:=0;
      while gainstep<=nrgainsteps do  //do all gains
      begin
        StepButton1.enabled:=false;
        StepButton1.caption:='.....';
        if gainstep=0 then Instruction.Lines.add('Working on the DARKS....');
        exposure:=lightinfo[gainstep].Exposure;
        gain:=lightinfo[gainstep].gain;

        if TakeDark then
        with Ffits do
        begin
          dark1:=image;
          setlength(dark1,1,h,w); //duplicate
          if TakeDark then
            sigma_dark_adu:=stdev(dark1,image); //calculate stdev in ADU
        end;



        flux_adu:=lightinfo[gainstep].mean_light-mean(dark1);//calculate mean flux value of one pixel

        //corrections for Gain in the camera driver
        if bit_depth<>16 then
          correction:=round(sat_level_adu/power(2,bit_depth))
        else
        correction:=1;

        gain_e{e-/adu}:=(flux_adu/correction)/sqr(lightinfo[gainstep].sigma_light_adu/correction);

        read_noise_e{e-}:=(sigma_dark_adu/correction) * gain_e;
        FullWell_capacity_e:=(sat_level_adu/correction)*gain_e;
        StringGrid1.InsertRowWithValues(stringgrid1.rowcount,[inttostr(gainstep+1), floattostrF(lightinfo[gainstep].Gain,FFfixed,0,0),floattostrF(gain_e,FFfixed,0,3),floattostrF(read_noise_e,FFfixed,0,3),floattostrF(FullWell_capacity_e,FFfixed,0,0),floattostrF(lightinfo[gainstep].exposure,FFfixed,0,3),floattostrF(lightinfo[gainstep].mean_light,FFfixed,0,0)]);
        StringGrid1.Row :=stringgrid1.rowcount;//scroll
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

