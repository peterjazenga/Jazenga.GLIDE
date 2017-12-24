// unité contenant des fonctions de traitements de chaine
unit type_string;

interface

uses
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  Classes;

{$IFDEF VERSIONS}
const
    gVer_type_string : T_Version = ( Component : 'Ressources chaînes' ; FileUnit : 'fonctions_string' ;
                        	     Owner : 'Matthieu Giroux' ;
                        	     Comment : 'String types.' ;
                        	     BugsStory :'Version 1.0.0.0 : Creating structure for delphi.';
                        	     UnitType : 1 ;
                        	     Major : 1 ; Minor : 0 ; Release : 0 ; Build :  0);
{$ENDIF}

implementation

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_type_string );
{$ENDIF}
end.

