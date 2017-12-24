unit u_extradios;

{$IFDEF FPC}
{$mode Delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
{$IFDEF FPC}
  LResources,
{$ELSE}
{$ENDIF}
   Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TExtRadioGroup }

  TExtRadioGroup = class(TRadioGroup)
  private
    FValues : TStrings;
    procedure SetValues(const AValue: TStrings);
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Values: TStrings read FValues write SetValues;

  end;

implementation

{------------------------------------------------------------------------------
  Method: TExtRadioGroup.SetValues
  Params:  avalue - Stringlist containing values optional result of radiobuttons
  Returns: Nothing

  Assign items from a stringlist.
 ------------------------------------------------------------------------------}
procedure TExtRadioGroup.SetValues(const AValue: TStrings);
begin
  if (AValue <> FValues) then
  begin
    FValues.Assign(AValue);
  end;
end;

constructor TExtRadioGroup.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FValues:=TStringList.Create;
end;

destructor TExtRadioGroup.Destroy;
begin
  inherited Destroy;
  FValues.Free;
end;

end.
