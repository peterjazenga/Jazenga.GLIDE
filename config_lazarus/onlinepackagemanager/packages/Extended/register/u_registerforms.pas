unit u_registerforms;
{
Unité             U_RegisterIni
Unité créant un projet form
Classes :
TF_FormMainIniModule : Module créant une form
TF_FormMainIniExpert : Expert enregistrant le module dans les nouveaux projets
Rédigé par Matthieu Giroux le 1/12/2003
}

interface

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

uses
{$IFNDEF FPC}
  DesignEditors, Windows,
{$ENDIF}
{$IFNDEF OOPEXPERTS}
  ESBaseForm,
{$ENDIF}
  Forms, SysUtils ;

procedure Register ;

{$IFNDEF OOPEXPERTS}
const
  CST_AUTEUR = 'Matthieu Giroux'; //auteur du projet
  // Commentaire du projet
  CST_COMMENTAIRE = 'Form utilisant un fichier INI pour initialiser son descendant et ADO';
  CST_EXPERTNOM = 'Form INI'; // Nom du projet
  CST_FORMANCETRE = 'F_FormMainIni' ; // Ancêtre de la form
  CST_FORMUNIT = 'U_FormMainIni' ; // Unité de l'ancêtre
  CST_NOUVEAUMENU = 'FrameWork' ; // onglet du projet dans les nouveau projets
  CST_FORMIDSTRING = 'Microcelt.TF_FormMainIni' ; // Identifiant du projet

  // Source de la form descendante
  sIniFormSource =
    'unit %0:s;'                                                                 + crlf +
                                                                                  crlf +
    'interface'                                                                  + crlf +
                                                                                  crlf +
    'uses'                                                                      + crlf +
    '  Windows, Messages, SysUtils, Classes, Graphics, Controls,'               + crlf +
    '  Forms, Dialogs, %3:s;'                                                    + crlf +
                                                                                  crlf +
    'type'                                                                      + crlf +
    '  T%1:s = class(T%2:s)'                                                    + crlf +
    '  private'                                                                  + crlf +
    '    { Private declarations }'                                                + crlf +
    '  public'                                                                  + crlf +
    '    { Public declarations }'                                               + crlf +
    '// Utiliser cette procédure pour gérer automatiquement la lecture INI'     + crlf +
    '    procedure p_InitialisationParamIni; override;'                         + crlf +
    '// Utiliser cette procédure pour gérer automatiquement la sauvegarde INI'  + crlf +
    '    procedure p_SauvegardeParamIni; override;'                             + crlf +
    '// Utiliser cette procédure après la sauvegarde INI pour TB97'             + crlf +
    '    procedure p_ApresSauvegardeParamIni; override;'                        + crlf +
    '// Utiliser cette procédure pour gérer la déconnexion'                     + crlf +
    '    procedure p_PbConnexion; override;'                                    + crlf +
    '// Utiliser cette procédure pour gérer la connexion'                       + crlf +
    '    procedure p_Connectee; override;'                                      + crlf +
    '// Utiliser cette procédure pour mettre à jour les touches spéciales'      + crlf +
    '    procedure p_SortieMajNumScroll ( const ab_MajEnfoncee    ,'            + crlf +
    '                                           ab_NumEnfoncee    ,'            + crlf +
    '                                           ab_ScrollEnfoncee : boolean );override;' + crlf +
    '  end;'                                                                     + crlf +
                                                                                  crlf +
    'var'                                                                        + crlf +
    '  %1:s: T%1:s;'                                                            + crlf +
                                                                                  crlf +
    'implementation'                                                            + crlf +
                                                                                  crlf +
    '{$R *.DFM}'                                                                 + crlf +
                                                                                  crlf +
    '// gestion automatique de la lecture INI'                                  + crlf +
    'procedure T%1:s.p_InitialisationParamIni;'                                 + crlf +
    'begin'                                                                     + crlf +
    '//Placer ici le Code initialisation INI'                                   + crlf +
    'End ;'                                                                     + crlf +
                                                                                  crlf +
    '// gestion automatique de la sauvegarde INI'                               + crlf +
    'procedure T%1:s.p_SauvegardeParamIni;'                                     + crlf +
    'begin'                                                                     + crlf +
    '//Placer ici le Code sauvegarde INI'                                       + crlf +
    'End ;'                                                                     + crlf +
                                                                                  crlf +
    '// gestion de la sauvegarde INI pour TB97'                                 + crlf +
    'procedure T%1:s.p_ApresSauvegardeParamIni;'                                + crlf +
    'begin'                                                                     + crlf +
    '//Placer ici le Code sauvegarde INI'                                       + crlf +
    'End ;'                                                                     + crlf +
                                                                                  crlf +
    '// gestion de la déconnexion'                                              + crlf +
    'procedure T%1:s.p_PbConnexion;'                                            + crlf +
    'begin'                                                                     + crlf +
    '//Placer ici le Code d''affichage de déconnexion'                          + crlf +
    'End ;'                                                                     + crlf +
                                                                                  crlf +
    '// gestion de la connexion'                                                + crlf +
    'procedure T%1:s.p_Connectee;'                                              + crlf +
    'begin'                                                                     + crlf +
    '//Placer ici le Code d''affichage de connexion'                            + crlf +
    'End ;'                                                                     + crlf +
                                                                                  crlf +
    '// gestion des touches spéciales'                                          + crlf +
    '// ab_MajEnfoncee : Touche majuscule'                                      + crlf +
    '// ab_NumEnfoncee : Touche chiffres dans le clavier numérique'             + crlf +
    '// ab_ScrollEnfoncee : Touche arrêt défilement à effet bascule'            + crlf +
    'procedure T%1:s.p_SortieMajNumScroll ( const ab_MajEnfoncee    ,'          + crlf +
    '                                             ab_NumEnfoncee    ,'          + crlf +
    '                                             ab_ScrollEnfoncee : boolean );'+ crlf +
    'begin'                                                                     + crlf +
    '//Placer ici le Code de mise à jour des touches spéciales'                 + crlf +
    'End ;'                                                                     + crlf +
                                                                                  crlf +
    'end.'                                                                       + crlf;

{ TComponentProperty
  The default editor for TComponents.  It does not allow editing of the
  properties of the component.  It allow the user to set the value of this
  property to point to a component in the same form that is type compatible
  with the property being edited (e.g. the ActiveControl property). }

type
  { TF_FormMainIniModule : Module créant une form
 }
  TF_FormMainIniModule = class(TESBaseFormCreator)
  public
  // Récupère l'unité créée
    function GetFormAncestorUnitName: string; override;
  // Récupère l'ancêtre form créé
    function GetAncestorName: string; override;
    // Source de la nouvelle form
    function GetSourceCode: string; override;
    // Retourne le nom de l'interface ( ne sais pas l'utiliser )
    function GetIntfName: string; override;
    // Retourne la source de l'interface ( ne sais pas l'utiliser )
    function NewIntfSource(const UnitIdent, FormIdent,
    AncestorIdent: string): string; override;
  end; { TF_FormMainIniModule }

{ TF_FormMainIniExpert : Expert enregistrant le module dans les nouveaux projets }
  TF_FormMainIniExpert = class(TESBaseCustomFormExpert)
  public
// Récupère le menu dans nouveau
    function GetPage: string; override ;
// Récupère la classe form
    function GetFormCreatorClass: TESBaseFormCreatorClass; override;
// Récupère le nom du créateur
    function GetAuthor: string; override;
// Récupère les commentaires du projet
    function GetComment: string; override;
// Récupère l'ID info
    function GetIDString: string; override;
// Récupère le nom du projet
    function GetName: string; override;
// Récupère l'Icône
    function GetGlyph: HICON; override;
  end; { TF_FormMainIniExpert }

{$ENDIF}

implementation

uses
{$IFNDEF FPC}
  ToolIntf,  EditIntf, DesignIntf, ExptIntf,
{$ELSE}
  custforms, unite_messages,
{$ENDIF}
  Classes,  U_FormMainIni, U_FormAdapt;

{$IFNDEF OOPEXPERTS}
{
procedure Register ;
begin // Enregistre le nouvel expert de projet
  // Procédures à garder pour peut-être plus tard ( utilisation actuelle d'unités dépréciées)
  }
{  Params.Style := WS_CHILD or WS_DLGFRAME or WS_VISIBLE or DS_CONTROL;
  Params.WindowClass :=
  Initialisation des paramètres

// paramètrage de la création dans la palette des projets
  WindowClass.style := WS_DISABLED ;
  WindowClass.Cursor := crDefault ;
  WindowClass.lpszMenuName := 'FrameWork' ;
  WindowClass.lpszClassName := 'TF_FormMainIni' ;
  WindowClass.hCursor := LoadCursor(0, idc_Arrow);
  WindowClass.hIcon := LoadIcon(0, IDI_APPLICATION);
  WindowClass.hbrBackground := HBrush(Color_Window);
  WindowClass.lpfnWndProc := @DefWindowProc ;
  WindowClass.cbClsExtra := 0;
  WindowClass.cbWndExtra := 0;

// paramètrage de la création dans l'inspecteur d'objet
  Params.Caption := 'F_McFormMainIni' ;
  Params.X := 0 ;
  Params.Y := 0 ;
  Params.Height := 600 ;
  Params.Height := 400 ;
  Params.Style := WS_DISABLED ;
  Params.WndHandle := ParentWindow ;
  Params.WindowClass := g_ParametresForm ;
  Params.WinClassName := 'TF_FormMainIni' ;
}
//   UnRegisterClass ( TF_FormMainIni );
{   S := TResourceStream.Create(HInstance, 'INI', RT_RCDATA);
   try
     if Supports(BorlandIDEServices, IOTAServices, Services) then
     begin
       TargetFile := Services.GetBinDirectory + 'INI.res';
       if not FileExists(TargetFile) then
         S.SaveToFile(TargetFile);
     end;
   finally
     S.Free;
   end;
end;   }

function TF_FormMainIniModule.GetIntfName: string;
begin
  Result:= '';
end;

function TF_FormMainIniModule.NewIntfSource(const UnitIdent, FormIdent,
  AncestorIdent: string): string;
begin
  Result:= '';
end;

function TF_FormMainIniModule.GetSourceCode: string;
// This is a virtual method. Descendant form creators can override to return
// a different set of source code or to add special comments to the beginning or
// end of file. The source code format should be in a form similar to
// sBasicFormSource, above. This source should use the four format strings
// below:
//
//         %0:s            Name of unit being created (e.g., "Unit1").
//        %1:s            Name of form being created (e.g., "Form1").
//         %2:s            Name of form's ancestor class (e.g, "TMyCustomForm")
//        %3:s            Name of unit containing the ancestor form.
//
// Note: If you override this method and use the Format command to format the
// string, be aware that Format has an internal limitation of 1024 bytes. If
// your generated code exceeds this length, you should break it up into smaller
// segments and concatenate them together once formatted.
begin
  result := sIniFormSource ;
end;    { GetSourceCode }

{ TF_FormMainIniModule }

  // Récupère l'ancêtre form créé
function TF_FormMainIniModule.GetAncestorName: string;
begin
  result := CST_FORMANCETRE ; // Descends from TESNewForm
end; { GetAncestorName }

  // Récupère l'unité créée
function TF_FormMainIniModule.GetFormAncestorUnitName: string;
begin
  result := CST_FORMUNIT ;
end; { GetFormAncestorUnitName }

{ TF_FormMainIniExpert }

// Récupère le menu dans nouveau
function TF_FormMainIniExpert.GetPage: string;
begin
  result := CST_NOUVEAUMENU ;
end;    { GetPage }

// Récupère la classe form
function TF_FormMainIniExpert.GetFormCreatorClass: TESBaseFormCreatorClass;
begin
  result := TF_FormMainIniModule;
end; { GetFormCreator }

// Récupère le nom du créateur
function TF_FormMainIniExpert.GetAuthor: string;
begin
  result := CST_AUTEUR;
end; { GetAuthor }

// Récupère les commentaires du projet
function TF_FormMainIniExpert.GetComment: string;
begin
  result := CST_COMMENTAIRE;
end; { GetComment }

// Récupère l'Icône
function TF_FormMainIniExpert.GetGlyph: HICON;
begin
  result := LoadIcon(0, idi_Application);
end; { GetGlyph }

// Récupère l'ID info
function TF_FormMainIniExpert.GetIDString: string;
begin
  result := CST_FORMIDSTRING;
end; { GetIDString }

// Récupère le nom du projet
function TF_FormMainIniExpert.GetName: string;
begin
  result := CST_EXPERTNOM;
end; { GetName }
{$ENDIF}


procedure Register ;
begin // Enregistre le nouvel expert de projet
// Un register libère automatiquement la variable à la suppression
{$IFDEF FPC}
  RegisterCustomForm ( TF_FormMainIni, 'lazextcomponents' );
  RegisterCustomForm ( TF_FormAdapt, 'lazextcomponents' );
{$ELSE}
  RegisterCustomModule ( TF_FormMainIni, TCustomModule );
  RegisterCustomModule ( TF_FormAdapt, TCustomModule );
//   RegisterPropertyEditor ( TypeInfo ( TADOConnection ), TADOConnection, 'Connection', TProviderProperty );
//   RegisterPropertyEditor(TypeInfo(WideString), TADOConnection, 'Provider', TProviderProperty);
//   RegisterPropertyEditor(TypeInfo(WideString), TADOConnection, 'ConnectionString', TConnectionStringProperty);
//   RegisterComponentEditor(TADOConnection, TADOConnectionEditor);
{$IFNDEF OOPEXPERTS}
   RegisterLibraryExpert(TF_FormMainIniExpert.Create);
{$ENDIF}
{$ENDIF}
end;

end.
