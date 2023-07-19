program ccdciel;

{$mode objfpc}{$H+}

{
Copyright (C) 2015 Patrick Chevalley

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

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  {$ifdef mswindows}
  windows,
  {$endif}
  Forms, pascalscript, tachartlazaruspkg, lazcontrols, sysutils, pu_main,
  fu_devicesconnection, fu_preview, fu_msg, u_utils, fu_visu, cu_indimount,
  fu_capture, pu_devicesetup, cu_ascomfocuser, cu_focuser, u_global,
  fu_starprofile, fu_filterwheel, fu_focuser, fu_script, cu_wheel, cu_mount,
  cu_indiwheel, cu_indifocuser, cu_indicamera, cu_fits, cu_camera,
  cu_ascomwheel, cu_ascommount, cu_ascomcamera, fu_ccdtemp, pu_options,
  fu_frame, cu_astrometry_engine, cu_planetarium_samp, pu_viewtext,
  cu_autoguider_phd, cu_tcpclient, fu_autoguider, fu_sequence, u_ccdconfig,
  pu_edittargets, cu_autoguider, cu_planetarium, fu_planetarium, cu_astrometry,
  pu_planetariuminfo, cu_planetarium_cdc, cu_targets, cu_plan, pu_pascaleditor,
  pu_scriptengine, fu_mount, enhedit, pu_pause, UScaleDPI, fu_video, pu_vcurve,
  cu_incamerawheel, cu_planetarium_hnsky, cu_indirotator, cu_ascomrotator,
  cu_rotator, fu_rotator, cu_autoguider_linguider, cu_watchdog, cu_indiwatchdog,
  pu_focusercalibration, cu_tcpserver, u_translation, pu_hyperbola,
  fu_magnifyer, u_annotation, cu_safety, cu_weather, cu_ascomsafety,
  cu_ascomweather, fu_weather, cu_indiweather, cu_indisafety, cu_dome,
  cu_ascomdome, cu_indidome, fu_dome, fu_safety, pu_about, pu_selectscript,
  cu_ascomrestcamera, cu_ascomrestwheel, cu_ascomrestweather,
  cu_ascomrestsafety, cu_ascomrestrotator, cu_ascomrestmount,
  cu_ascomrestfocuser, cu_ascomrestdome, cu_ascomrest, cu_autoguider_dither,
  cu_sequencefile, u_hints, pu_goto, pu_photometry, u_libraw, pu_polaralign,
  pu_keyboard, cu_alpacamanagement, pu_compute, cu_manualwheel, u_speech, pu_collimation,
  cu_ascomrestswitch, cu_ascomswitch, cu_indiswitch, cu_switch, fu_switchpage, pu_newscript,
  pu_polaralign2, fu_internalguider, cu_autoguider_internal, pu_handpad, fu_finder,
  pu_findercalibration, fu_switch, pu_sensoranalysis;

{$R *.res}

{$ifdef mswindows}
{$ifdef cpui386}
// allow to use more than 2GB of memory
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}
{$endif}
{$endif}
{$ifdef LCLGTK2}
var FArg,FSave: PPChar;
    i,nSave: integer;
{$endif}

begin
  (* // To stdout by default, uncomment to write to file
  {$ifdef USEHEAPTRC}
    {$ifdef mswindows}
      DeleteFile('C:\Temp\ccdciel_heap.trc');
      SetHeapTraceOutput('C:\Temp\ccdciel_heap.trc');
    {$else}
      DeleteFile('/tmp/ccdciel_heap.trc');
      SetHeapTraceOutput('/tmp/ccdciel_heap.trc');
    {$endif}
  {$endif}
  *)
  RequireDerivedFormResource := True;
  Application.Initialize;
  {$ifdef LCLGTK2}
    nSave:=argc;
    FSave:=argv;
    try
    argc:=argc+1;
    ReAllocMem(FArg, argc*SizeOf(PChar));
    argv := FArg;
    for i:=0 to nSave-1 do
      FArg[i]:=FSave[i];
    FArg[argc-1]:=PChar('--disableaccurateframe');  // Fix flashing window on startup
    except
      argc:=nSave;
      argv:=FSave;
    end;
  {$endif}
  Application.CreateForm(Tf_main, f_main);
  Application.CreateForm(Tf_setup, f_setup);
  Application.CreateForm(Tf_option, f_option);
  Application.CreateForm(Tf_planetariuminfo, f_planetariuminfo);
  Application.CreateForm(Tf_pause, f_pause);
  Application.CreateForm(Tf_focusercalibration, f_focusercalibration);
  Application.CreateForm(Tf_about, f_about);
  Application.CreateForm(Tf_goto, f_goto);
  Application.CreateForm(Tf_photometry, f_photometry);
  Application.CreateForm(Tf_polaralign, f_polaralign);
  Application.CreateForm(Tf_compute, f_compute);
  Application.CreateForm(Tf_collimation, f_collimation);
  Application.CreateForm(Tf_polaralign2, f_polaralign2);
  Application.CreateForm(Tf_handpad, f_handpad);
  Application.CreateForm(Tf_findercalibration, f_findercalibration);
  Application.Run;
  {$ifdef LCLGTK2}
  try
    ReAllocMem(FArg,0);
    argc:=0;
    argv:=nil;
  except
  end;
  {$endif}
end.

