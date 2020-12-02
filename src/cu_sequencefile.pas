unit cu_sequencefile;

{$mode objfpc}{$H+}

interface

uses u_ccdconfig,
     LazFileUtils, Classes, SysUtils;

type
  T_SequenceFile = Class(TComponent)
    private
      FItems: TCCDconfig;
      FCurrentName: string;
      procedure SetFilename(value: string);
      function GetFilename: string;
    public
      constructor Create(AOwner: TComponent);override;
      destructor  Destroy; override;
      procedure Clear;
      procedure ClearContent;
      procedure Save;
      property Filename: String read GetFilename write SetFilename;
      property Items: TCCDconfig read FItems write FItems;
      property CurrentName: string read FCurrentName;
  end;

implementation

constructor T_SequenceFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems:=TCCDconfig.Create(self);
  FCurrentName:='';
end;

destructor  T_SequenceFile.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure T_SequenceFile.SetFilename(value: string);
begin
  FItems.Filename:=value;
  FCurrentName:=ExtractFileNameOnly(value);
end;

function T_SequenceFile.GetFilename: string;
begin
  result:=FItems.Filename;
end;

procedure T_SequenceFile.Clear;
begin
  FItems.Filename:='';
  FItems.Clear;
  FCurrentName:='';
end;

procedure T_SequenceFile.ClearContent;
begin
  FItems.Clear;
end;

procedure T_SequenceFile.Save;
begin
  FItems.Flush;
end;

end.

