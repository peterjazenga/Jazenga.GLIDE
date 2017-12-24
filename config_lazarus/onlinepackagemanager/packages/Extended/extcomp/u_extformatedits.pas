{*********************************************************************}
{                                                                     }
{                                                                     }
{             TExtSearchEdit :                               }
{             Objet issu d'un TDBedit qui associe les         }
{             avantages de la DBEdit avec une recherche     }
{             Créateur : Matthieu Giroux                          }
{             31 Mars 2011                                            }
{                                                                     }
{                                                                     }
{*********************************************************************}

unit u_extformatedits;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I ..\DLCompilers.inc}

interface

uses Classes,
     u_framework_components,
     u_framework_dbcomponents,
     fonctions_string;

{$IFDEF VERSIONS}
const
    gVer_TExtAutoDBEdit : T_Version = ( Component : 'Composants Auto Edits' ;
                                          FileUnit : 'U_TExtAutoEdit' ;
                                          Owner : 'Matthieu Giroux' ;
                                          Comment : 'Searching in a dbedit.' ;
                                          BugsStory : '0.9.0.0 : In place not tested.';
                                          UnitType : 3 ;
                                          Major : 0 ; Minor : 9 ; Release : 0 ; Build : 0 );

{$ENDIF}

type

  { TExtFormatEdit }
  TExtFormatEdit = class(TFWEdit)
  private
    FNoAccent : Boolean;
    FModeFormat : TModeFormatText;
  {$IFDEF FPC}
  protected
    procedure TextChanged; override;
  {$ENDIF}
  public
    {$IFNDEF FPC}
    procedure Change; override;
    {$ENDIF}
    constructor Create ( Aowner : TComponent ); override;
  published
    property NoAccent : Boolean read FNoAccent write FNoAccent default False;
    property ModeFormat : TModeFormatText read FModeFormat write FModeFormat default mftNone;
  end;

  { TExtFormatDBEdit }
  TExtFormatDBEdit = class(TFWDBEdit)
  private
    FNoAccent : Boolean;
    FModeFormat : TModeFormatText;
  {$IFDEF FPC}
  protected
    procedure TextChanged; override;
  {$ENDIF}
  public
    {$IFNDEF FPC}
    procedure Change; override;
    {$ENDIF}
    constructor Create ( Aowner : TComponent ); override;
  published
    property NoAccent : Boolean read FNoAccent write FNoAccent default False;
    property ModeFormat : TModeFormatText read FModeFormat write FModeFormat default mftNone;
  end;

implementation


{ TExtFormatEdit }

{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetModified
  Params:  Value to set FModified to
  Returns: Nothing
 ------------------------------------------------------------------------------}

procedure TExtFormatEdit.{$IFDEF FPC}TextChanged{$ELSE}Change{$ENDIF};
var
  Temp: String;
  {$IFDEF FPC}
  CPos: TPoint;
  {$ENDIF}
  SStart, SLen: Integer;
begin
  //debugln('TCustomEdit.TextChanged ',DbgSName(Self));
  if FNoAccent or ( FModeFormat > mftNone ) Then
  begin
    // use a local variable to reduce amounts of widgetset calls
    Temp := Text;
    //check to see if the charcase should affect the text.
    p_FormatText(Temp,FModeFormat,FNoAccent);
    if (Temp <> Text) then
    begin
      {$IFDEF FPC}
      CPos := CaretPos;
      SStart := SelStart;
      SLen := SelLength;
      Text := Temp;
      SelStart  := SStart;
      SelLength := SLen;
      CaretPos  := CPos;
      {$ELSE}
      SStart := GetSelStart;
      SLen := GetSelLength;
      Text := Temp;
      SetSelStart ( SStart );
      SetSelLength ( SLen );
      {$ENDIF}
    end;
  end;
  Inherited;
end;



constructor TExtFormatEdit.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
  FNoAccent:=False;
  FModeFormat:=mftNone;
end;


{ TExtFormatDBEdit }

{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetModified
  Params:  Value to set FModified to
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TExtFormatDBEdit.{$IFDEF FPC}TextChanged{$ELSE}Change{$ENDIF};
var
  Temp: String;
  {$IFDEF FPC}
  CPos: TPoint;
  {$ENDIF}
  SStart, SLen: Integer;
begin
  //debugln('TCustomEdit.TextChanged ',DbgSName(Self));
  if FNoAccent or ( FModeFormat > mftNone ) Then
  begin
    // use a local variable to reduce amounts of widgetset calls
    Temp := Text;
    //check to see if the charcase should affect the text.
    p_FormatText(Temp,FModeFormat,FNoAccent);
    if (Temp <> Text) then
    begin
      {$IFDEF FPC}
      CPos := CaretPos;
      SStart := SelStart;
      SLen := SelLength;
      Text := Temp;
      SelStart  := SStart;
      SelLength := SLen;
      CaretPos  := CPos;
      {$ELSE}
      SStart := GetSelStart;
      SLen := GetSelLength;
      Text := Temp;
      SetSelStart ( SStart );
      SetSelLength ( SLen );
      {$ENDIF}
    end;
  end;
  Inherited;
end;


constructor TExtFormatDBEdit.Create(Aowner: TComponent);
begin
  Inherited;
  FNoAccent:=False;
  FModeFormat:=mftNone;
end;


{$IFDEF VERSIONS}
initialization
  // Versioning
  p_ConcatVersion ( gVer_TExtAutoDBEdit );
{$ENDIF}
end.
