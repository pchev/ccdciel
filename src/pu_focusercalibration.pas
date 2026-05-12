unit pu_focusercalibration;

{$mode objfpc}{$H+}

{
Copyright (C) 2019 Patrick Chevalley

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

{
The principle of focuser calibration is the following:
- it not need to be started near the focus position, but this is faster if it is.
- detect stars in first image, if there is not retry by decreasing the required SNR.
- take 5 exposures at the same focuser position to measure the hfd fluctuation due to seeing.
- move the focuser by the initial step, measure hfd, increase step if the hfd change is too small.
- move the focuser and repeat measurement until the HFD is one more than the initial value.
- reverse direction to move in the direction of the minimal HFD position.
- once minimal HFD is reach continue until the HFD is two more than the minimal.
- compute the dynamic parameters from this measurement.
}
interface

uses u_global, cu_focuser, cu_camera, cu_waitthread, u_translation, u_utils,
  Classes, SysUtils, FileUtil, UScaleDPI, math,
  TASources, TAGraph, TASeries, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ValEdit, Spin, SpinEx, TACustomSeries;

type

  { Tf_focusercalibration }

  Tf_focusercalibration = class(TForm)
    btnNext: TButton;
    btnSave: TButton;
    spBin: TSpinEditEx;
    spExp: TFloatSpinEditEx;
    btnCancel: TButton;
    FitSource: TListChartSource;
    spGain: TSpinEditEx;
    ISObox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    LabelGain: TLabel;
    LabelOffset: TLabel;
    Notebook1: TNotebook;
    spOffset: TSpinEditEx;
    PageMeasure: TPage;
    PageResult: TPage;
    Panel1: TPanel;
    PanelExp: TPanel;
    PanelBinning: TPanel;
    PanelGain: TPanel;
    PanelOffset: TPanel;
    PtSource: TListChartSource;
    spStartStep: TSpinEdit;
    btnStart: TButton;
    ValueListEditor1: TValueListEditor;
    VcChart: TChart;
    VcChartPtL: TLineSeries;
    VcChartPtR: TLineSeries;
    VcChartRegL: TLineSeries;
    VcChartRegR: TLineSeries;
    procedure btnCancelClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
  private
    Ffocuser: T_focuser;
    Fcamera: T_camera;
    FonMsg: TNotifyMsg;
    saveendexposure: TNotifyEvent;
    TerminateCalibration: boolean;
    FAbsolute, FRunning, FFirstExp, SeeingFound,MinHfdFound, StepFound, Reverse, Direction: boolean;
    FExp,FSNR,FBacklash,seeing,hfddiff,maxhfd,minhfd,starthfd,curhfd,lasthfd: double;
    FBin,FGain,FOffset,numseeing,maxmeasurement,curmeasurement,startpos,minhfdpos,maxhfdpos,curpos,lastpos,minstep,maxstep,step,totstep  : integer;
    Fdynstep: integer;
    seeinglist: array of double;
    measurement: array of array[1..2] of double;
    procedure msg(txt:string);
    procedure EndExposure(Sender: TObject);
    procedure NextExposure(data: PtrInt);
    procedure FocIn(p:integer);
    procedure FocOut(p:integer);
    procedure ComputeResult;
    procedure Saveconfig;
  public
    procedure SetLang;
    procedure Progress(x,y: double);
    procedure CalibrationCancel(reason:string);
    property focuser: T_focuser read Ffocuser write Ffocuser;
    property camera: T_camera read Fcamera write Fcamera;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
  end;

var
  f_focusercalibration: Tf_focusercalibration;

implementation

{$R *.lfm}

{ Tf_focusercalibration }

procedure Tf_focusercalibration.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self);
  SetLang;
end;

procedure Tf_focusercalibration.SetLang;
begin
  Caption := rsFocuserCalib;
  label1.Caption:=rsExposure;
  label2.Caption:=rsBinning;
  label3.Caption:=rsStepSize;
  LabelGain.Caption:=rsGain;
  LabelOffset.Caption:=rsOffset2;
  ValueListEditor1.TitleCaptions[0] := rsParameter;
  ValueListEditor1.TitleCaptions[1] := rsValue;
  label8.Caption:=rsClickSaveToS;
  btnStart.Caption := rsStart;
  btnCancel.Caption := rsCancel;
  btnNext.Caption:=rsNext;
  btnSave.Caption:=rsSave;
end;

procedure Tf_focusercalibration.FormShow(Sender: TObject);
begin
  saveendexposure:=Fcamera.onEndControlExposure;
  if Ffocuser.BacklashActive then
    FBacklash:=Ffocuser.Backlash
  else
    FBacklash:=0;
  FRunning:=false;
  TerminateCalibration:=false;
  btnStart.Visible:=true;
  btnCancel.Visible:=true;
  btnNext.Visible:=False;
  btnSave.Visible:=False;
  Notebook1.PageIndex:=0;
end;

procedure Tf_focusercalibration.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  CanClose := not FRunning;
end;

procedure Tf_focusercalibration.CalibrationCancel(reason:string);
begin
  FRunning:=false;
  msg(reason);
  btnStart.Visible:=true;
end;

procedure Tf_focusercalibration.btnCancelClick(Sender: TObject);
begin
  msg(rsRequestToSto);
  if FRunning then
    TerminateCalibration:=true
  else
    ModalResult:=mrCancel;
end;

procedure Tf_focusercalibration.btnNextClick(Sender: TObject);
begin
  Notebook1.PageIndex:=1;
  btnNext.Visible:=false;
  btnSave.Visible:=true;
end;

procedure Tf_focusercalibration.btnSaveClick(Sender: TObject);
begin
  Saveconfig;
  ModalResult:=mrOK;
end;

procedure Tf_focusercalibration.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Fcamera.onEndControlExposure:=saveendexposure;
end;

procedure Tf_focusercalibration.msg(txt:string);
begin
 label4.Caption:=txt;
 if assigned(FonMsg) then FonMsg(rsFocuserCalib+': '+txt,3);
end;

procedure Tf_focusercalibration.btnStartClick(Sender: TObject);
begin
  // initialization
  FRunning:=true;
  TerminateCalibration:=false;
  msg(rsFocuserCalib3);
  PtSource.Clear;
  FitSource.Clear;
  FExp:=spExp.Value;
  FBin:=spBin.Value;
  FGain:=spGain.Value;
  FOffset:=spOffset.Value;
  SeeingFound:=false;
  MinHfdFound:=false;
  Reverse:=false;
  Direction:=FocusDirOut;
  StepFound:=false;
  FSNR:=30;
  hfddiff:=2;
  FFirstExp:=true;
  maxmeasurement:=100;
  curmeasurement:=0;
  SetLength(measurement,maxmeasurement);
  // get focuser state
  FAbsolute:=focuser.hasAbsolutePosition;
  if FAbsolute then begin
      minstep:=round(focuser.PositionRange.min);
      maxstep:=round(focuser.PositionRange.max);
      startpos:=focuser.Position
  end
  else begin
    if focuser.hasRelativePosition then begin
      minstep:=round(focuser.RelPositionRange.min);
      maxstep:=round(focuser.RelPositionRange.max);
      startpos:=0;
    end
    else begin
      CalibrationCancel(rsTheFocuserDo);
      exit;
    end;
  end;
  minstep:=max(minstep,2);
  curpos:=startpos;
  totstep:=0;
  step:=round(max(spStartStep.Value/1.5,minstep));
  // Start first exposure
  Fcamera.onEndControlExposure:=@EndExposure;
  if not Fcamera.ControlExposure(FExp,FBin,FBin,LIGHT,ReadoutModeFocus,FGain,FOffset,true) then begin
     CalibrationCancel(rsExposureFail);
  end
  else btnStart.Visible:=false;
end;

procedure Tf_focusercalibration.EndExposure(Sender: TObject);
begin
  WaitExecute(1000,@NextExposure,0);
end;

procedure Tf_focusercalibration.NextExposure(data: PtrInt);
var i,s,rx,ry,ns: integer;
  meanhfd,minseeing,maxseeing: double;
  hfdlist: array of double;
begin
try
  if TerminateCalibration then begin
    CalibrationCancel(rsStop);
    exit;
  end;
  if Frunning and Fcamera.ControlExposureOK then begin
    if FFirstExp then begin    // first exposure
      FFirstExp:=false;
      s:=20;
      rx:=img_Width-6*s;
      ry:=img_Height-6*s;
      repeat
        // search stars
        Fcamera.Fits.GetStarList(rx,ry,s,maxint,FSNR);
        ns:=Length(Fcamera.Fits.StarList);
        // if not found retry with a smaller snr
        if ns=0 then FSNR:=FSNR-3;
      until (ns>0)or(FSNR<=3);
      if ns>0 then begin
        // store star list for next exposures
        SetLength(hfdlist,ns);
        SetLength(AutofocusStarList,ns);
        for i:=0 to ns-1 do begin
            hfdlist[i]:=Fcamera.Fits.StarList[i].hfd;
            AutofocusStarList[i,1]:=Fcamera.Fits.StarList[i].x;
            AutofocusStarList[i,2]:=Fcamera.Fits.StarList[i].y;
        end;
        // compute median HFD
        meanhfd:=SMedian(hfdlist,ns);
        // initialize
        starthfd:=meanhfd;
        maxhfd:=starthfd;
        minhfd:=starthfd;
        curhfd:=starthfd;
        lasthfd:=starthfd;
        minhfdpos:=curpos;
        maxhfdpos:=curpos;
        lastpos:=curpos;
        inc(curmeasurement);
        measurement[curmeasurement-1,1]:=curpos;
        measurement[curmeasurement-1,2]:=curhfd;
        seeing:=0.1;
        numseeing:=1;
        SetLength(seeinglist,5);
        seeinglist[numseeing-1]:=meanhfd;
        msg(rsHFDFluctuati);
        // plot
        Progress(curpos,meanhfd);
        Fcamera.ControlExposure(FExp,FBin,FBin,LIGHT,ReadoutModeFocus,FGain,FOffset,true);
        exit;
      end
      else
         raise exception.create(rsNoStarDetect);
    end
    else if not SeeingFound then begin // process seeing measurement
      s:=20;
      Fcamera.Fits.MeasureStarList(s,AutofocusStarList);
      ns:=Length(Fcamera.Fits.StarList);
      if ns>0 then begin
         // compute median HFD
         inc(numseeing);
         SetLength(hfdlist,ns);
         for i:=0 to ns-1 do
             hfdlist[i]:=Fcamera.Fits.StarList[i].hfd;
         meanhfd:=SMedian(hfdlist,ns);
         seeinglist[numseeing-1]:=meanhfd;
         if numseeing=5 then begin
           minseeing:=999;
           maxseeing:=0;
           for i:=0 to 4 do begin
              minseeing:=min(minseeing,seeinglist[i]);
              maxseeing:=max(maxseeing,seeinglist[i]);
           end;
           seeing:=max(0.1,maxseeing-minseeing);
           msg(Format(rsHFDFluctuati2, [FormatFloat(f1, seeing)]));
           SeeingFound:=true;
         end;
         Progress(curpos,meanhfd);
         Fcamera.ControlExposure(FExp,FBin,FBin,LIGHT,ReadoutModeFocus,FGain,FOffset,true);
         exit;
      end
      else
         raise exception.create(rsNoStarDetect);
    end
    else begin          // process measurement exposures
      s:=20;
      Fcamera.Fits.MeasureStarList(s,AutofocusStarList);
      ns:=Length(Fcamera.Fits.StarList);
      if ns>0 then begin
         // compute median HFD
         SetLength(hfdlist,ns);
         for i:=0 to ns-1 do
             hfdlist[i]:=Fcamera.Fits.StarList[i].hfd;
         meanhfd:=SMedian(hfdlist,ns);
         curhfd:=meanhfd;
         // store measurement
         inc(curmeasurement);
         if curmeasurement>=maxmeasurement then raise exception.Create('Too many measurement without convergence');
         measurement[curmeasurement-1,1]:=curpos;
         measurement[curmeasurement-1,2]:=curhfd;
         // store maximum
         if MinHfdFound and (curhfd>maxhfd) then begin
           maxhfd:=curhfd;
           maxhfdpos:=curpos;
         end;
         // plot
         Progress(curpos,meanhfd);
         if MinHfdFound and((maxhfd-minhfd)>(hfddiff+seeing)) then begin
           // Calibration OK
           FRunning:=false;
           spStartStep.Value:=step;
           Label4.Caption:=rsFocuserCalib4;
           // Move focuser to min hfd point
           if FAbsolute then
             Ffocuser.Position:=minhfdpos
           else begin
             if totstep>0 then
               FocIn(totstep)
             else
               FocOut(abs(totstep));
           end;
           ComputeResult;
           btnNext.Visible:=true;
         end
         else begin
           // increase the step if the change is too small
           if (not StepFound)and(abs(curhfd-lasthfd)<(3*seeing)) then begin
             step:=round(1.5*step);
             if step>spStartStep.Value then
                msg(rsIncreaseStep+' '+inttostr(step));
           end
           else
             StepFound:=true;
           if (not MinHfdFound)and(not Reverse)and((curhfd-starthfd)>(1+seeing)) then begin
             // reverse direction when we are moving out of focus
             Direction:=not Direction;
             Reverse:=true;
             msg(rsReverseDirec);
           end
           else if (not MinHfdFound)and(Reverse)and((curhfd-lasthfd)>(2*seeing)) then begin
              // we reversed and now the hfd increase, this indicate we just pass the minimum
              minhfd:=999;
              for i:=0 to curmeasurement-1 do begin
                 if measurement[i,2]<minhfd then begin
                   minhfd:=measurement[i,2];
                   minhfdpos:=round(measurement[i,1]);
                 end;
              end;
              maxhfd:=curhfd;
              maxhfdpos:=curpos;
              MinHfdFound:=true;
              msg(rsMinimumHFDFo+' '+FormatFloat(f1, minhfd));
           end;
           if SeeingFound then begin
             // move focuser to new position
             lasthfd:=curhfd;
             lastpos:=curpos;
             if Direction=FocusDirOut then
               FocOut(step)
             else
               FocIn(step);
           end;
           // Start next exposure
           Fcamera.ControlExposure(FExp,FBin,FBin,LIGHT,ReadoutModeFocus,FGain,FOffset,true);
         end;
      end
      else
         raise exception.create(rsNoStarDetect);
    end;
  end
  else begin
    raise exception.Create(rsExposureFail);
  end;
except
  on e: Exception do begin
    CalibrationCancel(e.Message);
   end;
end;
end;

procedure Tf_focusercalibration.FocIn(p:integer);
begin
  if p>maxstep then p:=maxstep;
  if p<minstep then p:=minstep;
  if FAbsolute then begin
    curpos:=curpos-p;
    focuser.Position:=curpos;
  end
  else begin
    focuser.FocusIn;
    focuser.RelPosition:=p;
    curpos:=curpos-p;
    totstep:=totstep-p;
  end;
end;

procedure Tf_focusercalibration.FocOut(p:integer);
begin
  if p>maxstep then p:=maxstep;
  if p<minstep then p:=minstep;
  if FAbsolute then begin
    curpos:=curpos+p;
    focuser.Position:=curpos;
  end
  else begin
    focuser.FocusOut;
    focuser.RelPosition:=p;
    curpos:=curpos+p;
    totstep:=totstep+p;
  end;
end;

procedure Tf_focusercalibration.Progress(x,y: double);
begin
  PtSource.Add(x,y,'',clGreen);
  FitSource.Add(x,y);
end;

procedure Tf_focusercalibration.ComputeResult;
var  dpos: integer;
begin
  dpos:=abs(minhfdpos-maxhfdpos);
  Fdynstep:=round(dpos/3);
  ValueListEditor1.Clear;
  ValueListEditor1.InsertRow(rsExposure, FormatFloat(f3,FExp), true);
  ValueListEditor1.InsertRow(rsGain, inttostr(FGain), true);
  ValueListEditor1.InsertRow(rsOffset2, inttostr(FOffset), true);
  ValueListEditor1.InsertRow(rsBinning, inttostr(FBin), true);
  ValueListEditor1.InsertRow(rsMoveDirectio,rsOut,true);
  ValueListEditor1.InsertRow(rsAutofocusTol,FormatFloat(f1,2*minhfd),true);
  ValueListEditor1.InsertRow(rsMinSNR,FormatFloat(f1,FSNR),true);
  ValueListEditor1.InsertRow(rsNumberOfDyna,'7',true);
  ValueListEditor1.InsertRow(rsMovementBetw,inttostr(Fdynstep),true);
end;

procedure Tf_focusercalibration.Saveconfig;
begin
  try
  config.SetValue('/StarAnalysis/AutoFocusMode',ord(afDynamic));
  config.SetValue('/StarAnalysis/AutofocusExposure',FExp);
  config.SetValue('/StarAnalysis/AutofocusGain',FGain);
  config.SetValue('/StarAnalysis/AutofocusOffset',FOffset);
  config.SetValue('/StarAnalysis/AutofocusBinning',FBin);
  config.SetValue('/StarAnalysis/AutofocusMoveDir',FocusDirOut);
  config.SetValue('/StarAnalysis/AutofocusTolerance',2*minhfd);
  config.SetValue('/StarAnalysis/AutofocusMinSNR',FSNR);
  config.SetValue('/StarAnalysis/AutofocusDynamicNumPoint',7);
  config.SetValue('/StarAnalysis/AutofocusDynamicMovement',Fdynstep);
  config.SetValue('/StarAnalysis/AutofocusInPlace',True);
  config.SetValue('/StarAnalysis/AutofocusPlanetNumPoint',7);
  config.SetValue('/StarAnalysis/AutofocusPlanetMovement',Fdynstep);
  config.Flush;
  except
    on E: Exception do ShowMessage('Error saving focuser calibration: '+ E.Message);
  end;
end;

end.

