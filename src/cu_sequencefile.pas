unit cu_sequencefile;

{$mode objfpc}{$H+}

interface

uses u_ccdconfig,
  Classes, SysUtils;

type
  T_SequenceFile = Class(TComponent)
    private
      FConfig: TCCDconfig;
    public
      constructor Create(AOwner: TComponent);override;
      destructor  Destroy; override;
      property Config: TCCDconfig read FConfig write FConfig;
  end;

var
  SequenceFile: T_SequenceFile;

implementation

constructor T_SequenceFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConfig:=TCCDconfig.Create(self);
end;

destructor  T_SequenceFile.Destroy;
begin
  FConfig.Free;
  inherited Destroy;
end;

end.

