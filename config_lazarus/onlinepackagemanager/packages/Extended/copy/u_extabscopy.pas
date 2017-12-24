unit u_extabscopy;
{
Composant TAbsFileCopy

Développé par:
Matthieu GIROUX

Composant abstrait permettant de copier un fichier ou de le traduire.
Compatible Linux


Version actuelle: 1.0

Mises à jour:
}
{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface


{$I ..\dlcompilers.inc}
{$I ..\extends.inc}

uses
  SysUtils,
{$IFDEF FPC}
  unit_messagescopy,
{$ELSE}
  fonctions_system,
  unit_messagescopy_delphi,
{$ENDIF}
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  Classes ;

{$IFDEF VERSIONS}
  const
    gVer_TExtAbsCopy : T_Version = ( Component : 'Composant AbsFileCopy' ;
                                               FileUnit : 'U_ExtAbsCopy' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'Composant de copie multi-platformes.' ;
                                               BugsStory : '0.9.0.0 : En place testée.';
                                               UnitType : 3 ;
                                               Major : 0 ; Minor : 9 ; Release : 0 ; Build : 0 );

{$ENDIF}


type
    TECopyEvent = procedure(Sender : Tobject; const BytesCopied,BytesTotal : cardinal) of object;
    TESimpleCopyOption = ( cpUseFilter, cpCreateBackup, cpDestinationIsFile, cpCreateDestination, cpEraseDestination );
    TEChangeDirectoryEvent = procedure(Sender : Tobject; const NewDirectory, DestinationDirectory : String ) of object;
    TECopyErrorEvent = procedure(Sender : Tobject; const ErrorCode : Integer ; var ErrorMessage : String ; var ContinueCopy : Boolean ) of object;
    TECopyFinishEvent = procedure(Sender : Tobject; const ASource, ADestination : String ; const Errors : Integer ) of object;
    TEReturnEvent = procedure(Sender : Tobject; var Continue : Boolean ) of object;

    IFileCopyComponent = interface
             function  BeforeCopy : Boolean ;
             { Déclarations protégées }
             function InternalDefaultCopyFile  ( const as_Source, as_Destination : String ):Boolean;
             procedure InternalFinish ( const as_Source, as_Destination : String );
             Procedure CopySourceToDestination;
             function CreateDestination ( const as_Destination : String ): Boolean;
           End ;

    { TAbsFileCopy }

    TAbsFileCopy = class(TComponent)
           private
             FOnFailure : TECopyErrorEvent ;
             FErrors     : Integer ;
           protected
             function PrepareFileSourceAndDestination ( const as_Source : String ; var as_Destination : String ; const ab_AppendFile, ab_CreateBackup : Boolean ): Integer; virtual;
             function IsCopyOk ( const ai_Error : Integer ; as_Message : String ):Boolean; virtual ;
           published
             property Errors : integer read FErrors write FErrors;
             property OnFailure : TECopyErrorEvent read FOnFailure write FOnFailure;
         End ;

implementation

uses fonctions_file,
  {$IFDEF FPC}
     LazFileUtils,
  {$ENDIF}
     Forms, Dialogs, Controls ;

{ TAbsFileCopy }

function TAbsFileCopy.PrepareFileSourceAndDestination ( const as_Source : String ; var as_Destination : String ; const ab_AppendFile, ab_CreateBackup : Boolean ): Integer;
var ls_FileName, ls_FileExt : String ;
    lsr_data : TSearchRec ;
    li_pos : Integer ;
    ls_Destination : String ;
begin
  Result := 0 ;
  ls_Destination := as_Destination ;
  If fileexists(ls_Destination)
   then
    Begin
      ls_FileExt  := '';
      ls_FileName := '';
      FindFirstUTF8( ls_Destination,faanyfile,lsr_data);
      if ( ab_CreateBackup )
       Then
        try
          p_FileNameDivision ( lsr_data.Name, ls_FileName, ls_FileExt );
          li_pos := 0 ;
          while FileExists ( as_Destination ) do
           Begin
              inc ( li_pos );
              as_Destination := ExtractFilePath ( ls_Destination ) + DirectorySeparator + ls_FileName + '-' + IntToStr ( li_pos ) + ls_FileExt ;
           End
        Except
          Result := -1 ;
          IsCopyOk ( Result, as_Destination );
        End
       Else
        try
          DeleteFile(as_Destination);
        Except
          Result := CST_COPYFILES_ERROR_FILE_DELETE ;
          IsCopyOk ( Result, GS_COPYFILES_ERROR_FILE_DELETE + as_Destination );
        End ;
      FindCloseUTF8(lsr_data);
    End ;
end;


function TAbsFileCopy.IsCopyOk ( const ai_Error : Integer ; as_Message : String ):Boolean;
begin
  Result := True ;
  if  ( ai_Error <> 0 ) then
    Begin
      inc ( FErrors );
      if assigned ( FOnFailure ) then
        Begin
            FOnFailure ( Self, ai_Error, as_Message, Result );
        End ;
    End ;
End ;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_TExtAbsCopy );
{$ENDIF}
end.

