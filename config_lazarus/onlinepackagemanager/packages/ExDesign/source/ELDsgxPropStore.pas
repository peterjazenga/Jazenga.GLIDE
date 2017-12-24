{**********************************************************************
 Package pl_ExDesign
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit ELDsgxPropStore;

{$MODE Delphi}

interface

uses
  Classes, Dialogs, Forms, SysUtils, TypInfo, IniFiles;

type
  TStoreTarget = (stIniFile);

  TplPropertiesStore = class(TComponent)
  private
    FStoredProps: TStrings;
    FRegKey: string;
    FIniFile: string;
    FStoreTarget: TStoreTarget;
    procedure SetStoredProps(const Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadSettings;
    procedure SaveSettings;
  published
    property StoredProps: TStrings read FStoredProps write SetStoredProps;
    property RegKey: string read FRegKey write FRegKey;
    property IniFile: string read FIniFile write FIniFile;
    property StoreTarget: TStoreTarget read FStoreTarget write FStoreTarget;
  end;


implementation

{ TplPropertiesStore }

constructor TplPropertiesStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStoreTarget := stIniFile;
  FIniFile := 'settings.ini';
  FStoredProps := TStringList.Create;
end;

destructor TplPropertiesStore.Destroy;
begin
  FStoredProps.Free;
  inherited Destroy;
end;

procedure TplPropertiesStore.SetStoredProps(const Value: TStrings);
begin
  FStoredProps.Assign(Value);
end;

procedure TplPropertiesStore.LoadSettings;
var
  x, i: integer;
  s: string;
  comp: TComponent;
  CompName, PropName, PropValue: string;
  Ini: TIniFile;
  t: TStringList;

begin

  if (FStoredProps.Count = 0) then
    Exit;

  t := TStringList.Create;

  case FStoreTarget of
    stIniFile:
    begin
      Ini := TIniFile.Create(ExtractFilePath(Application.ExeName) + FIniFile);
      Ini.ReadSection(Owner.Name, t);
    end;
  end;

  //................................

  for x := 0 to t.Count - 1 do
  begin
    s := t[x];
    if (s <> '') then
    begin
      i := Pos('.', s);
      CompName := Copy(s, 1, i - 1);
      if (CompName = '') then
        comp := Owner
      else
        comp := Owner.FindComponent(CompName);
      if Assigned(comp) then
      begin
        try
          PropName := Copy(s, i + 1, Length(s));

          case FStoreTarget of
            stIniFile:
            begin
              PropValue := Ini.ReadString(Owner.Name, s, '');
            end;
          end;

          if (PropValue <> '') then
            SetPropValue(comp, PropName, PropValue);
        except
          // Hide Exception
        end;
      end;
    end;
  end;

  //.................................

  case FStoreTarget of
    stIniFile:
    begin
      Ini.Free;
    end;
  end;

  t.Free;

end;

procedure TplPropertiesStore.SaveSettings;
var
  x, i: integer;
  s: string;
  comp: TComponent;
  CompName, PropName, PropValue: string;
  Ini: TIniFile;
  t: TStringList;

begin

  if (FStoredProps.Count = 0) then
    Exit;

  case FStoreTarget of
    stIniFile:
    begin
      Ini := TIniFile.Create(ExtractFilePath(Application.ExeName) + FIniFile);
      Ini.EraseSection(Owner.Name);
    end;
  end;
  //................................

  for x := 0 to FStoredProps.Count - 1 do
  begin
    s := Trim(FStoredProps[x]);
    if (s <> '') then
    begin
      i := Pos('.', s);
      CompName := Copy(s, 1, i - 1);
      if (CompName = '') then
        comp := Owner
      else
        comp := Owner.FindComponent(CompName);
      if Assigned(comp) then
      begin
        try
          if (CompName <> '') then
            CompName := CompName + '.';
          PropName := Copy(s, i + 1, Length(s));
          PropValue := GetPropValue(comp, PropName);

          case FStoreTarget of
            stIniFile:
            begin
              Ini.WriteString(Owner.Name, CompName + PropName, PropValue);
            end;
          end;

        except
          // Hide Exception
        end;
      end;
    end;
  end;
  //................................

  case FStoreTarget of
    stIniFile:
    begin
      Ini.Free;
    end;
  end;

end;

end.
