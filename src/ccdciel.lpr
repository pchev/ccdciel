program ccdciel;

{$mode objfpc}{$H+}

{
Copyright (C) 2015 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, sysutils, pu_main, fu_devicesconnection, fu_preview, fu_msg, u_utils,
  fu_visu, cu_indimount, fu_capture, pu_devicesetup, cu_ascomfocuser,
  cu_focuser, u_global, fu_starprofile, fu_filterwheel, fu_focuser, fu_mount,
  u_modelisation, cu_wheel, cu_mount, cu_indiwheel, cu_indifocuser,
  cu_indicamera, cu_fits, cu_camera, cu_ascomwheel, cu_ascommount,
  cu_ascomcamera, pu_filtername, fu_ccdtemp, pu_indigui, pu_options, fu_frame,
  cu_astrometry, cu_cdcclient, pu_viewtext;

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
  Application.CreateForm(Tf_filtername, f_filtername);
  Application.CreateForm(Tf_option, f_option);
  Application.CreateForm(Tf_viewtext, f_viewtext);
  Application.Run;
end.

