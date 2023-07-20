unit simpletimer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  CTimer,
  LResources;

type

  { TSimpleTimer }

  TSimpleTimer = class(TCustomControlTimer)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property Interval;
    property Enabled;
    property OnTimer;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I simpletimer_icon.lrs}
  RegisterComponents('Misc',[TSimpleTimer]);
end;

{ TSimpleTimer }

constructor TSimpleTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

end.
