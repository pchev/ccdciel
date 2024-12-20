unit cu_onlinesearch;

{$mode objfpc}{$H+}

{
Copyright (C) 2024 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

interface

uses
  {$ifndef mswindows}
  netdb,
  {$endif}
  u_global,
  httpsend, blcksock, XMLRead, DOM,
  SysUtils, Classes;


function SearchOnline(n: string;out rname,resolv: string; out rra,rdec,rmagn: double; out magband:string): boolean;

implementation

var num,sesame_resolver,sesame_name,sesame_type,sesame_desc,sesame_band : string;
    sesame_magn,ra,de: double;

function LoadSesame(f:TStream): boolean;
var
  Doc: TXMLDocument;
  Node,Snode,Dnode: TDOMNode;
  k,v,a,buf,k1,v1: string;
begin

  result:=false;

  sesame_resolver:='';
  sesame_name:='';
  sesame_type:='';
  sesame_desc:='';
  sesame_band:='';
  sesame_magn:=NullCoord;

  try

  Doc := nil;

  ReadXMLFile( Doc, f);

  if Doc = nil then
    exit;

  try

    Node := Doc.DocumentElement.FindNode('Target');

    if Node = nil then
      exit;

    Node := Node.FindNode('Resolver');

    if Node = nil then
      exit;

    Snode := Node.Attributes.GetNamedItem('name');

    if Snode <> nil then
      sesame_resolver := string(Snode.TextContent);

    Node := Node.FirstChild;

    while Node <> nil do
    begin

      k := string(Node.NodeName);

      if k <> '#comment' then
      begin

        v := string(Node.TextContent);
        a := '';

        Dnode := Node.Attributes.Item[0];

        if Dnode <> nil then
           a := string(Dnode.NodeName) + '.' + string(Dnode.TextContent);

        buf :='';
        Dnode := Node.FirstChild;

        while Dnode <> nil do
        begin
          k1 := string(Dnode.NodeName);

          if k1 = '#text' then break;

          v1 := string(Dnode.TextContent);

          if (k = 'mag') and (k1 = 'v') then begin
            if (a = 'band.V')or((sesame_magn=NullCoord)and((a = 'band.B')or(a = 'band.R')or(a = 'band.G'))) then
            begin
               sesame_band:=a;
               sesame_magn:=StrToFloatDef(v1,NullCoord);
            end;
          end;

          buf := buf+k+'.'+a+'.'+k1+':'+v1+tab;
          Dnode := Dnode.NextSibling;
        end;

        if buf='' then sesame_desc:=sesame_desc+k+':'+v+tab
                  else sesame_desc:=sesame_desc+buf;

        if k = 'oname' then sesame_name := v;
        if k = 'otype' then sesame_type := v;

        if k ='jradeg' then
        begin
          ra := StrToFloatDef(v,NullCoord);
          if ra = NullCoord then
            exit;

          ra := deg2rad * ra;
        end;

        if k = 'jdedeg' then
        begin
          de := StrToFloatDef(v,NullCoord);
          if de = NullCoord then
            exit;

          de := deg2rad * de;
        end;

      end;

      Node := Node.NextSibling;

    end;

    if sesame_name = '' then
      sesame_name := num;

    result := true;

  finally
    Doc.Free;
  end;

  except
    result := false;
  end;

end;

function SearchOnline(n: string;out rname,resolv: string; out rra,rdec,rmagn: double; out magband:string): boolean;
var
  url,cat:string;
  http: THTTPSend;
  ts,tms: integer;
begin

  result:=false;

  cat:='SN';   // Simbad, then NED
  num:=n;

  url:='https://cds.unistra.fr/cgi-bin/nph-sesame';
  url:=url+'/-oxF/'+cat+'?'+trim(StringReplace(num,' ','%20',[rfReplaceAll]));
  http:=THTTPSend.Create;
  http.Sock.SocksIP:='';
  http.ProxyHost:='';

  http.Timeout:=2000;
  http.Sock.ConnectionTimeout:=2000;
  {$ifndef mswindows}
  ts:=netdb.TimeOutS;
  tms:=netdb.TimeOutMS;
  netdb.TimeOutS:=2;
  netdb.TimeOutMS:=0;
  {$endif}
  try

  if http.HTTPMethod('GET', url) and
    (
      (http.ResultCode=200) or
      (http.ResultCode=0)
    )
  then
  begin
    http.Document.SaveToFile('/tmp/aa');
     result:=LoadSesame(http.Document);
     if result then begin
       rra:=ra;
       rdec:=de;
       rmagn:=sesame_magn;
       rname:=sesame_type+' '+sesame_name;
       magband:=sesame_band;
       resolv:=sesame_resolver;
     end;
  end;

  finally
  {$ifndef mswindows}
  netdb.TimeOutS:=ts;
  netdb.TimeOutMS:=tms;
  {$endif}
  http.Free;
  end;
end;


end.
