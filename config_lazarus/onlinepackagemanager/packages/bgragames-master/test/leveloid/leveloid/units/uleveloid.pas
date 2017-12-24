unit uleveloid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, bgTileMap, bgTileMapGL;

type

  { TLeveloidEditor }

  TLeveloidEditor = class(TBGTileMap)
  private
    FScript: TStringList;
    FNextLevel: string;
    procedure SetFNextLevel(AValue: string);
    procedure SetFScript(AValue: TStringList);
  public
    constructor Create(FileName: string);
    destructor Destroy; override;
    procedure LoadScript(FileName: string);
  published
    property Script: TStringList read FScript write SetFScript;
    property NextLevel: string read FNextLevel write SetFNextLevel;
  end;

  { TLeveloid }

  TLeveloid = class(TBGTileMapGL)
  private
    FScript: TStringList;
    FNextLevel: string;
    procedure SetFNextLevel(AValue: string);
    procedure SetFScript(AValue: TStringList);
  public
    constructor Create(FileName: string);
    destructor Destroy; override;
    procedure LoadScript(FileName: string);
  published
    property Script: TStringList read FScript write SetFScript;
    property NextLevel: string read FNextLevel write SetFNextLevel;
  end;

implementation

{ TLeveloidEditor }

procedure TLeveloidEditor.SetFScript(AValue: TStringList);
begin
  if FScript = AValue then
    Exit;
  FScript := AValue;
end;

procedure TLeveloidEditor.SetFNextLevel(AValue: string);
begin
  if FNextLevel = AValue then
    Exit;
  FNextLevel := AValue;
end;

constructor TLeveloidEditor.Create(FileName: string);
begin
  inherited Create(FileName);
  FScript := TStringList.Create;
  LoadScript(FileName);
end;

destructor TLeveloidEditor.Destroy;
begin
  FScript.Free;
  inherited Destroy;
end;

procedure TLeveloidEditor.LoadScript(FileName: string);
var
  ini: TMemIniFile;
  f: string;
begin
  ini := TMemIniFile.Create(FileName);

  f := ini.ReadString('Leveloid', 'Script', '');
  if f <> '' then
    FScript.LoadFromFile(f)
  else
    FScript.Add('begin end.');

  FNextLevel := ini.ReadString('Leveloid', 'NextLevel', '');

  ini.Free;
end;

{ TLeveloid }

procedure TLeveloid.SetFScript(AValue: TStringList);
begin
  if FScript = AValue then
    Exit;
  FScript := AValue;
end;

procedure TLeveloid.SetFNextLevel(AValue: string);
begin
  if FNextLevel = AValue then
    Exit;
  FNextLevel := AValue;
end;

constructor TLeveloid.Create(FileName: string);
begin
  inherited Create(FileName);
  FScript := TStringList.Create;
  LoadScript(FileName);
end;

destructor TLeveloid.Destroy;
begin
  FScript.Free;
  inherited Destroy;
end;

procedure TLeveloid.LoadScript(FileName: string);
var
  ini: TMemIniFile;
  f: string;
begin
  ini := TMemIniFile.Create(FileName);

  f := ini.ReadString('Leveloid', 'Script', '');
  if f <> '' then
    FScript.LoadFromFile(f)
  else
    FScript.Add('begin end.');

  FNextLevel := ini.ReadString('Leveloid', 'NextLevel', '');

  ini.Free;
end;

end.

