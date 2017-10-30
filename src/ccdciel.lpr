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
  Forms, pascalscript, tachartlazaruspkg, sysutils, pu_main,
  fu_devicesconnection, fu_preview, fu_msg, u_utils, fu_visu, cu_indimount,
  fu_capture, pu_devicesetup, cu_ascomfocuser, cu_focuser, u_global,
  fu_starprofile, fu_filterwheel, fu_focuser, fu_script,
  cu_wheel, cu_mount, cu_indiwheel, cu_indifocuser, cu_indicamera, cu_fits,
  cu_camera, cu_ascomwheel, cu_ascommount, cu_ascomcamera, fu_ccdtemp,
  pu_options, fu_frame, cu_astrometry_engine, cu_planetarium_samp, pu_viewtext,
  cu_autoguider_phd, cu_tcpclient, fu_autoguider, fu_sequence, u_ccdconfig,
  pu_edittargets, pu_editplan, cu_autoguider, cu_planetarium, fu_planetarium,
  cu_astrometry, pu_planetariuminfo, cu_planetarium_cdc, cu_targets, cu_plan,
  pu_pascaleditor, pu_scriptengine, fu_mount, enhedit, pu_pause, UScaleDPI,
  fu_video, pu_vcurve, cu_incamerawheel, cu_planetarium_hnsky, cu_indirotator,
  cu_ascomrotator, cu_rotator, fu_rotator, cu_autoguider_linguider;

{$R *.res}

begin
  {$ifdef USEHEAPTRC}
  DeleteFile('/tmp/ccdciel_heap.trc');
  SetHeapTraceOutput('/tmp/ccdciel_heap.trc');
  {$endif}

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(Tf_main, f_main);
  Application.CreateForm(Tf_setup, f_setup);
  Application.CreateForm(Tf_option, f_option);
  Application.CreateForm(Tf_viewtext, f_viewtext);
  Application.CreateForm(Tf_EditPlan, f_EditPlan);
  Application.CreateForm(Tf_planetariuminfo, f_planetariuminfo);
  Application.CreateForm(Tf_pause, f_pause);
  Application.Run;
end.

