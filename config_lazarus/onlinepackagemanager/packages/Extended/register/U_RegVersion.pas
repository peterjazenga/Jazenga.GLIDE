unit U_RegVersion;
{
Unité             U_RegisterIni
Unité créant un projet form
Classes :
TF_McFormMainIniModule : Module créant une form
TF_McFormMainIniExpert : Expert enregistrant le module dans les nouveaux projets
Rédigé par Matthieu Giroux le 1/12/2003
}

interface

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

uses
{$IFDEF PROPEDITS}
  PropEdits,ComponentEditors, dbpropedits,
{$ELSE}
  DesignEditors, DesignIntf,
{$ENDIF}
  Forms, SysUtils ;

type
  TVersionProperty = class ( TStringProperty )
    function GetAttributes: TPropertyAttributes; override;
    function AboutDialog: Boolean;
    procedure Edit; override ;
  End ;

implementation

uses fonctions_version ;

{ TVersionProperty }

function TVersionProperty.AboutDialog: Boolean;
begin
  Result := fb_AfficheApropos ( True, 'A Propos ', '' );
end;

procedure TVersionProperty.Edit;
begin
  AboutDialog ;
end;

function TVersionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog,paReadOnly];
end;


end.
