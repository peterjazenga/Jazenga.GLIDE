{*********************************************************************}
{                                                                     }
{                                                                     }
{             Matthieu Giroux                                         }
{             TDBGroupView :                                          }
{             Composant de groupes                                    }
{              et affectation avec chargements itératifs des données  }
{             22 Décembre 2006                                        }
{                                                                     }
{                                                                     }
{*********************************************************************}

unit U_GroupView;

{$IFDEF FPC}
{$mode Delphi}
{$ELSE}
{$R *.res}
{$ENDIF}

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

interface
// Gestion des groupes avec deux de ces composants et des boutons
// Chaque composant de gestion de groupe a sa propriété
// Datasource : Le Datasource à afficher dans la liste avec un paramètre dans le query
// DatasourceOwner : Le DataSource des groupes
// créé par Matthieu Giroux en Mars 2004

// 29-9-2004 : abandon complété dans la gestion panier (basket cancel completed)


uses
{$IFDEF FPC}
  LCLIntf, LCLType, SQLDB, lresources,
{$ELSE}
  Windows, DBTables, JvListView,
{$ENDIF}
    SysUtils, Classes, Graphics, Controls,
     Forms, Dialogs, Db,
     {$IFDEF EADO}
      ADODB,
     {$ENDIF}
     {$IFDEF DBEXPRESS}
     SQLExpr,
     {$ENDIF}
{$IFDEF DELPHI_9_UP}
     WideStrings ,
{$ENDIF}
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
     SyncObjs, U_DBListView,
     ComCtrls, fonctions_variant,
     fonctions_string,
     fonctions_proprietes;

{$IFDEF VERSIONS}
const
    gVer_TDBGroupView : T_Version = (Component : 'Composant TDBGroupView' ;
                                     FileUnit : 'U_GroupView' ;
                                     Owner : 'Matthieu Giroux' ;
                                     Comment : 'TDBListView avec gestion de groupes et affectation.' ;
                                     BugsStory :  '1.1.2.0 : SQL consts, testing 1N and NN.' +
                                                  '1.1.1.1 : IBX 2.' +
                                                  '1.1.1.0 : Multiple keys.' +
                                                  '1.1.0.3 : Init buttons to disabled.' +
                                                  '1.1.0.2 : Removing IsImplementorOf.' +
                                                  '1.1.0.1 : testing with SuperForm.' +
                                                  '1.1.0.0 : traducing methods and variables to english.' +
                                                  '1.0.1.2 : Creating methods from recording action.' +
                                                  '1.0.1.1 : Ajouts de la gestion DBExpress et BDE.' +
                                                  '1.0.1.0 : Gestion pour tous Datasources testée.' +
                                                  '1.0.0.0 : Gestion de groupe avec gestion de l''ADO non testée.';
                                     UnitType : 3 ;
                                     Major : 1 ; Minor : 1 ; Release : 2 ; Build : 0 );
{$ENDIF}

type

  EBasketAllEvent  = function  ( const Sender: TObject; var   Result : String ; const GetNullRecords, GetCurrentGroup : Boolean ):Boolean of object;
  EStringEvent  = function  ( const Sender: TObject ):String of object;
// Première déclaration
  TDBGroupView = class ;


    IFWFormVerify = interface
     ['{693AE27F-98C1-8E6D-E54F-FE57A16057E5}']
     procedure Modifying;
     procedure VerifyModifying;
     end;
  // Gestion de groupe

  { TDBGroupView }

  TDBGroupView = class(TDBListView)
   private
    ge_AddFields,
    ge_addFieldsValues : EStringEvent;
    gb_CaseInSensitive: Boolean;
{$IFDEF DELPHI_9_UP}
    gwst_SQLCommand,
    gwst_SQLSource,
    gwst_SQLQuery,
    gwst_SQLQuery2 : TWideStrings ;
{$ENDIF}
    {$IFNDEF FPC}
    ge_OnselectItem : TLVSelectItemEvent;
    ResInstance : THandle;
    {$ENDIF}
    gstl_SQLCommand,
    gstl_SQLSource,
    gstl_SQLQuery,
    gstl_SQLQuery2 : TStrings ;
    gstl_params : TStrings ;

    gprs_ParamSource : TParams ;
    {$IFDEF EADO}
    gprt_ParameterSource : TParameters ;
    {$ENDIF}

    ds_DatasourceQuery   ,
    ds_DatasourceQuery2  ,
    gds_Query1           ,
    gds_Query2           : TDataSource ;
    gdat_DataSetQuery : TComponent ;
    gws_RecordValue ,
    gws_Oldfilter ,
    gws_Filter : String ;
    ge_BasketGetAll : EBasketAllEvent ;
    gb_Oldfiltered,
    gb_Open,
    gb_Record    ,
    gb_Filtered : Boolean ;
    // Première fois de ce composant : Utilisé dans loaded
    function  fds_GetDatasourceQuery : TDataSource;
    function  fds_GetDatasourceQuery2 : TDataSource;
    function  fds_GetDatasetQuery : TComponent;
    function ft_GetValueList(var at_List: tt_TableauxStrings;
      const alsi_Item: TListItem): tt_ValeurStrings;
    procedure p_DatasetNotify;
    procedure p_SetDataSourceQuery ( const a_Value: TDataSource );
    procedure p_SetDatasetQuery ( const a_Value: TComponent );
    procedure p_SetDataSourceQuery2(const a_Value: TDataSource);
    procedure p_SetFilter   ( Value : String );
    procedure p_SetFiltered ( Value : Boolean    );
    procedure p_SetDataSourceOwner ( const a_Value: TDataSource );
    function  fds_GetDatasourceOwner : TdataSource ;
    procedure p_groupeMouseDownDisableEnableFleche ( const aLSV_groupe : TListView ; const abt_item : TControl );
    procedure p_SetTotalList  ( const a_Value: TWinControl );
    procedure p_SetBtnList  ( const a_Value: TWinControl );
    procedure p_SetAutreTotal  ( const a_Value: TWinControl );
    procedure p_SetOtherListBtn  ( const a_Value : TWinControl );
    procedure p_setBtnBasket      ( const a_Value : TWinControl );
    procedure p_SetOtherList  ( const a_Value: TDBGroupView );
    procedure p_setEnregistre  ( const a_Value: TWinControl );
    procedure p_setAbandonne   ( const a_Value: TWinControl );
    procedure p_setGroupKey   ( const a_Value: String );
    procedure p_setUnitsField  ( const a_Value: String );
    procedure p_SetGroupField ( const a_Value: String );
    procedure p_SetGroupTable ( const a_Value: String );
    procedure p_SetImageSupprime ( const a_Value: Integer );
    procedure p_SetImageInsere ( const a_Value: Integer );
    function fb_ErreurBtnTotalIn: Boolean;
    function fb_ErreurBtnIn: Boolean;
    function fb_ErreurBtnOut: Boolean;
    function fb_ErreurBtnTotalOut: Boolean;
    function fb_ValideBoutons: Boolean ;
    function fvar_CanPutPlus(const aadoq_Dataset, aadoq_Query: TDataset;
                                 const asi_ItemsSelected: TListItem ): Variant;
    function fi_FindList   ( var at_List : tt_TableauxStrings ; const alsi_Item : TListItem ) : Integer ;
    function fi_SupprimeItem ( var at_List : tt_TableauxStrings ; const alsi_Item : TListItem ) : Integer ;
    function fws_GetExistingFilter: String;
    function fb_ValueToValid(const afie_ChampTest: TField): Boolean;
   protected
    gds_Querysource      : TDataSource ;
    gdat_QuerySource     : TDataset ;
    gdat_Query1          ,
    gdat_Query2          : TDataset;

    // Groupe en cours
    gvar_WorkingOriginKey,
    gvar_WorkingGroup : Variant ;
    // lien vers le Datasource mettant à jour le composant
    gdl_DataLinkOwner : TUltimListViewDatalink;
    // champ de groupe dans la table d'association des groupes
    // Ou clé étrangère vers la table des groupes
    // Clé du Datasource des informations du groupe
    gstl_GroupKey     ,
    gstl_UnitsDatasource,
    gstl_GroupField     : TStrings ;
    gs_GroupField ,
    // champ unité dans la table d'association des groupes
    // Ou clé primaire de la table liée
    gs_UnitsFieldValues,
    gs_UnitsField   ,
    // Sort sauvegardé du query
    gs_SortQuery    ,
    // Table des groupes
    gs_TableOwner ,
    // Clé primaire de la table des groupes
    gs_GroupKey    ,
    // Table d'association NN des groupes
    gs_GroupTable  : String;
    // Propriété "est Datasource de List d'inclusion"
    gb_EstPrincipale : Boolean;
    // anciens evènements sur click des boutons
    ge_RecordedEvent   ,
    ge_FilterEvent       ,
    ge_DatasetDestroy,
    ge_DatasetParam,
    ge_QueryAll           ,
    ge_CancelledEvent    : TDatasetNotifyEvent ;
    ge_RecordError   : TDataSetErrorEvent ;
    ge_OnDatasetValidate : TNotifyEvent;
    ge_BasketClick       ,
    ge_RecordClick   ,
    ge_CancelClick    ,
    ge_TotalListClick   ,
    ge_BtnInvertClick    ,
    ge_ListClick        : TNotifyEvent ;
     /////////////////////////
     // Propriétés boutons  //
      /////////////////////////
       // Basket
    gBT_Basket       ,
    gBT_Optional       ,
    // Enregistre : évènement si principale
    gBT_Record   ,
    // abandonne : évènement si principale
    gBT_Cancel    ,
    // Ajoute tout dans la liste : évènement
    gBT_TotalList   ,
    // Ajoute dans la liste : évènement
    gBT_List        ,
    // Inversion des deux listes
    gbt_Exchange      ,
    // supprime de la liste
    gBT_Other        ,
    // supprime tout de la liste
    gBT_OtherTotal   : TWinControl ;
    // Propriété Image d'Insertion
    gi_ImageInsere   ,
    // Propriété Image de suppression
    gi_ImageSupprime : Integer ;
    // Propriété autre liste
    galv_OtherList  : TDBGroupView ;
    gb_SelfOpen ,
    gb_NoScroll      ,
    // Basket
    gb_Basket        : Boolean ;
    function ffi_LocateImageItem(const AItem: TListItem): TField; override;
    function fs_GetFieldsValuesInsert: String; virtual;
    function fs_GetFieldsInsert: String; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function fb_BeginOpen: Boolean; virtual;
    procedure p_SetHintString ( const awin_Control : TWincontrol ; const as_Hint : String );
    procedure p_SetClickEvent ( const awin_Control : TWincontrol ; const as_procedure : String ; const AOldEvent : TNotifyEvent );
    function fb_ExecuteQueryNotLinkedNNGroupSourceSimilar: Boolean; virtual;
    function fb_ExecuteQueryNotLinkedNNGroupSourceDifferent: Boolean; virtual;
    function fb_ExecuteQuery1N: Boolean; virtual;
    function fb_ExecuteQueryNotLinkedNNGroupSourceDifferentTotal: boolean; virtual;
    function fb_ExecuteQueryNotLinkedNNGroupSourceDifferentTotalOut: boolean; virtual;
    function fb_ExecuteQueryNotLinkedNNGroupSourceSimilarTotal: boolean; virtual;
    function fb_ExecuteQueryNotLinkedNNGroupSourceSimilarTotalOut: boolean; virtual;
    function fb_ExecuteQueryShowAllGroup ( const astl_KeysListOut: tt_TableauxStrings ): boolean; virtual;
    function fb_ExecuteQueryLinkedAllSelect: boolean; virtual;
    function fb_ExecuteQueryLinkedAllSelectTotal: boolean; virtual;
    procedure p_SetDatasources; virtual;
    function  fb_OpenParamsQuery : Boolean; virtual;
    Procedure p_OpenQuery; virtual;
    procedure p_SetButtonsOther(const ab_Value: Boolean);  protected
    procedure p_SetListImages ; virtual;
    procedure p_AssignColumnsSubitems; override;
    procedure p_ListLoaded; override;
    procedure EditingChanged; override;
    Procedure p_DataSetChanged; override;
    procedure p_SetDataSourceGroup( CONST a_Value: TDataSource); override;
    procedure p_LocateInit; virtual;
    procedure p_LocateRestore; virtual;
    procedure p_UndoRecord; virtual;
    procedure p_AddOriginKey ( const avar_Add : variant );virtual;
    procedure p_ReinitialisePasTout; override;
    function  fb_Locate ( const avar_Records : Variant ): Boolean ;
    procedure p_UpdateButtons ( const ai_ItemsAjoutes : Integer ); override;
    procedure p_DesactiveGrille; virtual;
    procedure p_VerifieModifications; virtual;
    function  fb_CanAdd          ( const adat_Dataset : TDataset ; const ab_AddItemPlus : Boolean) : Boolean ; override;
    function  fb_ChangeEtatItem       ( const adat_Dataset : TDataset ; const ab_AddItemPlus : Boolean ) : Boolean ; override;
    function  fb_AddRecords ( const adat_Dataset : TDataset ; const ab_InsereCles : Boolean ) : Boolean ; override;
    function  fb_SetList : Boolean ; override;
    function  fb_CanSort  : Boolean ; override;
    procedure DoSelectItem ( llsi_ItemsSelected : TListItem; ab_selected : Boolean ); {$IFNDEF FPC}virtual{$ELSE}override{$ENDIF};
    procedure LoadList; virtual;
    Procedure DataLinkClosed; virtual;
    procedure DblClick ; override;
    procedure DragOver ( aobj_Source : Tobject; ai_X, ai_Y : Integer ; ads_Etat : TDragState ; var ab_Accepte : Boolean ); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown( abt_Bouton : TMouseButton ; ass_EtatShift : TShiftState ; ai_x, ai_y : Integer ); override;
    procedure p_PostDataSourceOwner; virtual;
    procedure p_CreateSQL; virtual;
    procedure DataLinkScrolled ( const adat_Dataset : TDataSet ); override;

   public
    gb_OptionTotalList,
    gb_TotalListReal    : Boolean ;
    // Clés hors de la liste
    // Clés exclues de cette liste : l'autre liste a les clés incluses
    gt_KeyOwners     : tt_TableauxVarOption ;
    gstl_KeysListOut : tt_TableauxStrings ;

    procedure DataLinkLoadList ; override;
    constructor Create ( acom_owner : TComponent ); override;
    destructor Destroy ; override;
    Procedure p_AddSyncronousRecords; override;
    procedure p_Reinitialise ; override;
    procedure DragDrop ( aobj_Source : Tobject; ai_X, ai_Y : Integer ); override;
    procedure p_TransfertTotal ; virtual;
    procedure p_VideTotalList ; virtual;
    procedure Refresh ; override;
    property AllList    : Boolean read gb_OptionTotalList ;
    property  DataFieldsUnit : TStrings read gstl_UnitsDatasource;
   published
    procedure p_Cancel  ( Sender : TObject ); virtual;
    procedure p_Record ( Sender : TObject ); virtual;
    procedure p_VideBasket ( Sender : TObject ); virtual;
    procedure p_ClickTransfertTotal ( Sender : TObject ); virtual;
    procedure p_ClickTransfert      ( Sender : TObject ); virtual;
    procedure p_InvertClick ( Sender : TObject ); virtual;

    function ft_AddValue ( const alsi_ItemsSelected : TListItem ): tt_ValeurStrings;

    // Table de l'association des groupes
    property DataTableGroup : String read gs_GroupTable write p_setGroupTable;
    // Table du Datasource des groupes édités
    property DataTableOwner : String read gs_TableOwner write gs_TableOwner;
    // Datasource d'un query
    property DatasourceQuery : TDataSource read fds_GetDatasourceQuery write p_SetDataSourceQuery;
    // Datasource d'un deuxième query
    property DataSourceQuery2 : TDataSource read fds_GetDatasourceQuery2 write p_SetDataSourceQuery2;
    property DatasetQuery : TComponent read fds_GetDatasetQuery write p_SetDatasetQuery;
    // Datasource des groupes édités
    property DataSourceOwner : TDataSource read fds_GetDatasourceOwner write p_SetDataSourceOwner;
    // clé du query
    // du Datasource des groupes édités
    property DataKeyOwner : String read gs_GroupKey write p_setGroupKey;
    // field des unités de l'association des groupes
    property DataFieldUnit : String read gs_UnitsField write p_setUnitsField;
    // field des groupes de l'association des groupes
    property DataFieldGroup : String read gs_GroupField write p_setGroupField;
    // La liste est-elle la liste principale : liste d'inclusion
    property DataListPrimary : Boolean read gb_EstPrincipale write gb_EstPrincipale default True;
    // Bouton de transfert total de la liste
    property ButtonTotalIn : TWinControl read gBT_TotalList write p_setTotalList ;
    // Bouton de transfert de la liste
    property ButtonIn : TWinControl read gBT_List write p_setBtnList ;
    // Bouton de transfert entre deux listes
    property ButtonExchange : TWinControl read gbt_Exchange write gbt_Exchange ;
    // Bouton de transfert total de l'autre liste
    property ButtonTotalOut : TWinControl read gBT_OtherTotal write p_setAutretotal ;
    // Bouton de transfert de l'autre liste
    property ButtonOut: TWinControl read gBT_Other write p_setOtherListBtn ;
    // autre liste : liste complémentaire et obligatoire
    property DataListOpposite: TDBGroupView read galv_OtherList write p_setOtherList ;
    // Bouton d'enregistrement
    property ButtonRecord: TWinControl read gBT_Record write p_setEnregistre ;
    // Bouton d'enregistrement
    property ButtonOption: TWinControl read gBT_Optional write gBT_Optional ;
    // Bouton d'annulation de la composition
    property ButtonCancel: TWinControl read gBT_Cancel write p_setAbandonne ;
    // image ajoute de Imagelist
    property DataImgInsert : Integer read gi_ImageInsere write p_setImageInsere default 1;
    // image enlève de Imagelist
    property DataImgDelete : Integer read gi_ImageSupprime write p_SetImageSupprime default 0;
    // Basket
    property ButtonBasket : TWinControl read gBT_Basket write p_setBtnBasket ;
    // Récupération du trie des colonnes
    property DataSensitiveBug : Boolean read gb_CaseInSensitive write gb_CaseInSensitive default True ;

    //EVènements
    property OnDataRecorded : TDatasetNotifyEvent read ge_RecordedEvent write ge_RecordedEvent ;
    property OnDataCanceled : TDatasetNotifyEvent read ge_CancelledEvent write ge_CancelledEvent ;
    property OnDataRecordError : TDatasetErrorEvent read ge_RecordError write ge_RecordError ;
    property OnDataFilter : TDatasetNotifyEvent read ge_FilterEvent write ge_FilterEvent ;
    property OnDataAllQuery : TDatasetNotifyEvent read ge_QueryAll write ge_QueryAll ;

    // Filtrage SQL
    property DataAllFilter   : String read gws_Filter write p_SetFilter ;
    property DataAllFiltered : Boolean read gb_Filtered write p_SetFiltered ;
    property DataRecordValue : String read gws_RecordValue write gws_RecordValue ;
    property OnDataAllWhereBasket : EBasketAllEvent read ge_BasketGetAll write ge_BasketGetAll ;
    property DataKeyUnit;
    property OnDataAddFieldsInsert : EStringEvent read ge_AddFields write ge_AddFields;
    property OnDataAddFieldsValues : EStringEvent read ge_addFieldsValues write ge_addFieldsValues;
    property OnDatasetParam : TDataSetNotifyEvent read ge_DatasetParam write ge_DatasetParam;
    property OnDatasetDestroy : TDataSetNotifyEvent read ge_DatasetDestroy write ge_DatasetDestroy;
    property OnDatasetValidated : TNotifyEvent read ge_OnDatasetValidate write ge_OnDatasetValidate;
   end;

function fb_WaitForLoadingFirstFetch : Boolean ;
function fb_BuildWhereBasket ( const aalv_Primary: TDBGroupView ; var as_Result : String ; const ab_GetNull, ab_GetCurrent : Boolean ): Boolean; overload;
function fb_BuildWhereBasket ( const aalv_Primary: TDBGroupView ; var as_Result : String ; const ab_GetNull, ab_GetCurrent, ab_Order : Boolean ): Boolean; overload;

  // Message de confirmation d'enregistrement avant le tri
const
     // nombre par défaut de pages à charger
     CST_GROUPE_PAGES_CHARGER = 3 ;
     CST_GROUPE_COULEUR_FOCUS = clSkyBlue ;
     CST_GROUPE_TRANS_TOTAL   = 1 ;
     CST_GROUPE_TRANS_SIMPLE  = 0 ;
     CST_GROUPE_TRANS_RETOUR   = 2 ;
     CST_GROUPE_TRANS_DESTI   = 1 ;
     CST_GROUPE_TRANS_EXCLU   = 0 ;


var gcol_CouleurFocus : TColor = CST_GROUPE_COULEUR_FOCUS ;
    gcol_CouleurLignePaire   : Tcolor = clInfoBk ;
    gcol_CouleurLigneImpaire : Tcolor = clWhite  ;
    // Evènement centralisé de syncho du mode asynchrone
    ge_GroupFetchLoading : TEvent = Nil ;
    gim_GroupViewImageList : TImageList = Nil;

implementation

uses TypInfo,
  Variants,  ExtCtrls,  fonctions_erreurs,
  {$IFDEF FPC}
  unite_messages,
  {$ELSE}
  unite_messages_delphi,
  {$ENDIF}
     fonctions_db, fonctions_dbobjects ;

// non Utilisé : On change de groupe dans DataSetChanged
{Procedure TUltimListViewDatalink.DataSetScrolled(Distance: Integer);
Begin
  inherited ;
End;
 }
// Utilisé : On a supprimé un groupe
procedure TDBGroupView.p_DataSetChanged;
Begin
  inherited ;
  // sur le datasource des groupes
  if not gb_SelfOpen
  and assigned ( gdl_DataLink.DataSet )
   Then
    Begin
       // Si on est en consultation ( suppression n'est pas une édition )
      if ( Datasource.DataSet.State = dsBrowse )
      {$IFDEF EADO}
      and ( not ( Datasource.DataSet is TCustomADODataset ) or not ( eoAsyncExecute in ( Datasource.DataSet as TCustomADODataset ).ExecuteOptions ) or not ( stFetching in ( Datasource.DataSet as TCustomADODataset ).RecordsetState ))
      {$ENDIF}
      Then
         // Mise A Jour de la liste
        LoadList ;

    End ;
End;

procedure TDBGroupView.p_OpenQuery;
Begin
   // Filtrage
   // Gestion non N-N
   with gdl_DataLink.DataSet do
     Begin
       if ((  gs_GroupTable <> DataTableUnit )
       // Ou principale
            or  gb_EstPrincipale )
        // le champ groupe existe-t-il
       and assigned ( gdl_DataLink.DataSet.FindField ( gs_GroupField ))
       and assigned ( gdl_DataLinkOwner.DataSet )
        Then
          // Filtrage
          try
             // pour filtrer le dataset doit être actif
            gb_SelfOpen := True ;
            if ( State = dsInactive ) Then
                Active := True ;
            gb_SelfOpen := False ;

            // Filtrage
            if ((gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ) is TStringField )
            or (gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ) is TBlobField )
            or ( gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ).DataType IN CST_DELPHI_FIELD_STRING ))
             Then Filter := gs_GroupField + ' = ''' + fs_stringdbQuote ( gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).AsString ) + ''''
             Else Filter := gs_GroupField + ' = '   +                  ( gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).AsString ) ;
             // Activation du filtrage
            Filtered := True ;
          Except
            gb_SelfOpen := False ;
          End
         Else
          try
            // Sinon mise à jour des données
            gb_SelfOpen := True ;
            if ( State = dsInactive )
             Then
               Open ;
          finally
            gb_SelfOpen := False ;
          End ;
     End;
End;

function TDBGroupView.fb_OpenParamsQuery: Boolean;
Begin
  Result := False;
  if ( assigned ( gprs_ParamSource ) and ( gprs_ParamSource.Count > 0 ))
    {$IFDEF EADO}
  or ( assigned (  gprt_ParameterSource ) and ( gprt_ParameterSource.Count > 0 ))
    {$ENDIF}
    Then
      with gdl_DataLink.DataSet do
        Begin
          If  assigned ( gdl_DataLinkOwner.DataSet )
          and gdl_DataLinkOwner.DataSet.Active Then
            try
              gb_SelfOpen := True ;
              Close ;
              // Alors Affectation du premier paramètre en tant que paramètre du groupe
              if assigned ( gprs_ParamSource ) Then
                Begin
                  gprs_ParamSource.Items [ 0 ].DataType := gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).DataType;
                  if gdl_DataLinkOwner.DataSet.IsEmpty
                  or gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).IsNull
                   then
                    Begin
                      if ( gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ) is TNumericField )
                       then gprs_ParamSource.Items [ 0 ].Value    := 0
                       else gprs_ParamSource.Items [ 0 ].Value    := Null;
                    End
                  else gprs_ParamSource.Items [ 0 ].Value    := gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).Value
                End
        {$IFDEF EADO}
               else
                 Begin
                  gprt_ParameterSource.BeginUpdate;
                  gprt_ParameterSource.Items [ 0 ].DataType := gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).DataType;

                  if gdl_DataLinkOwner.DataSet.IsEmpty
                  or gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).IsNull
                  then
                    Begin
                      if ( gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ) is TNumericField ) then
                       gprt_ParameterSource.Items [ 0 ].Value    := 0
                      else
                       gprt_ParameterSource.Items [ 0 ].Value    := ' ';
                    End
                  else
                   Begin
                     gprt_ParameterSource.Items [ 0 ].Value    := gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).Value
                   End;
                  gprt_ParameterSource.EndUpdate;
                 End;
        {$ENDIF};
           // Gestion de relations N N
              if ( not gb_Basket )
        /// Ou sur le groupview principal contenant la liste des enregistrements
              or gb_EstPrincipale
              // Ou vide
              or (      ( assigned ( gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ))
                    and ( gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ).IsNull)))
              or (      ( assigned ( gdl_DataLink.DataSet.FindField ( gs_GroupField ))
                    and ( gdl_DataLink.DataSet.FindField ( gs_GroupField ).IsNull)))
                Then
                  if ( State = dsInactive ) Then
                    Begin
                      Open ;
                      Result := True;
                    End ;
              gb_SelfOpen := False ;
            Except
               gb_SelfOpen := False ;
            End;
        End;
End;

procedure TDBGroupView.DataLinkClosed;
begin
  {$IFDEF EADO}
  p_SetUnFetch;
  {$ENDIF}
  if not gb_SelfOpen Then
    gb_AllLoaded := True ;
end;

// Non utilisé
{Procedure TUltimListViewDatalink.RecordChanged(Field: TField);
Begin
  inherited ;
//  glst_GroupView.DataLinkRecordChanged(Field);
End;
 }



// Fonction : fb_WaitForLoadingFirstFetch
// Mode asynchrone : Attente d'un chargement d'items dans la liste
// A appeler avant de créer l'évènement ge_GroupFetchLoading et après tout ça mettre p_AddSyncronousRecords
// Retour : Dataset actif ou pas
Function  fb_WaitForLoadingFirstFetch : Boolean ;
Begin
  While assigned ( ge_GroupFetchLoading ) and ( ge_GroupFetchLoading.WaitFor ( 100 ) = wrSignaled ) do
     Begin
       Application.ProcessMessages ;
       Sleep ( 100 );

     End ;
  Result := True ;
End ;




// Récupère le datasource du Query
function TDBGroupView.fds_GetDatasourceQuery: TDataSource;
begin
  Result := ds_DataSourceQuery ;
end;

// Récupère le datasource du Query
function TDBGroupView.fds_GetDatasourceQuery2: TDataSource;
begin
  Result := ds_DatasourceQuery2 ;
end;

// Récupère le datasource du Query
function TDBGroupView.fds_GetDatasetQuery: TComponent;
begin
  Result := gdat_DataSetQuery ;
end;

// Affectation du composant dans la propriété DataSourceQuery
// test si n'existe pas
// Mise à jour du nom de table
// a_Value : Le datasource
procedure TDBGroupView.p_SetDataSourceQuery ( const a_Value: TDataSource );
var lobj_SQL : TObject ;
begin
{$IFNDEF FPC}
  ReferenceInterface ( DataSourceQuery, opRemove ); //Gestion de la destruction
{$ENDIF}
  if ds_DatasourceQuery <> a_Value then
  begin
    ds_DatasourceQuery := a_Value ; /// affectation
  end;
{$IFNDEF FPC}
  ReferenceInterface ( DataSourceQuery, opInsert ); //Gestion de la destruction
{$ENDIF}
  if  not assigned ( ds_DatasourceQuery ) Then
    Begin
      gstl_SQLQuery := nil ;
{$IFDEF DELPHI_9_UP}
      gwst_SQLQuery := nil ;
{$ENDIF}
    End
  else
  if  not ( csDesigning in ComponentState )
  and assigned ( ds_DatasourceQuery.Dataset ) Then
    Begin
      if not fb_GetSQLStrings ( ds_DataSourceQuery.Dataset, gstl_SQLQuery{$IFDEF DELPHI_9_UP}, gwst_SQLQuery {$ENDIF})
       Then
        ds_DatasourceQuery := nil;
    End ;

end;

// Affectation du composant dans la propriété DataSourceQuery
// test si n'existe pas
// Mise à jour du nom de table
// a_Value : Le datasource
procedure TDBGroupView.p_SetDataSourceQuery2 ( const a_Value: TDataSource );
var lobj_SQL : TObject ;
begin
{$IFNDEF FPC}
  ReferenceInterface ( DataSourceQuery, opRemove ); //Gestion de la destruction
{$ENDIF}
  if ds_DatasourceQuery2 <> a_Value then
  begin
    ds_DatasourceQuery2 := a_Value ; /// affectation
  end;
{$IFNDEF FPC}
  ReferenceInterface ( DataSourceQuery, opInsert ); //Gestion de la destruction
{$ENDIF}
  if  not assigned ( ds_DatasourceQuery2 ) Then
    Begin
      gstl_SQLQuery2 := nil ;
{$IFDEF DELPHI_9_UP}
      gwst_SQLQuery2 := nil ;
{$ENDIF}
    End
  else
  if  not ( csDesigning in ComponentState )
  and assigned ( ds_DatasourceQuery2.Dataset ) Then
    Begin
      if not fb_GetSQLStrings ( ds_DatasourceQuery2.Dataset, gstl_SQLQuery2{$IFDEF DELPHI_9_UP}, gwst_SQLQuery2 {$ENDIF})
       Then
        ds_DatasourceQuery2 := nil;
    End ;

end;


// Affectation du composant dans la propriété DataSourceQuery2
// test si n'existe pas
// Mise à jour du nom de table
// a_Value : Le datasource
procedure TDBGroupView.p_SetDatasetQuery ( const a_Value: TComponent );
begin
{$IFNDEF FPC}
  ReferenceInterface ( DataSourceQuery2, opRemove ); //Gestion de la destruction
{$ENDIF}
  if gdat_DataSetQuery <> a_Value then
  begin
    gdat_DataSetQuery := a_Value ; /// affectation
  end;
{$IFNDEF FPC}
  ReferenceInterface ( DataSourceQuery2, opInsert ); //Gestion de la destruction
{$ENDIF}
  if  not assigned ( gdat_DataSetQuery ) Then
    Begin
      gstl_SQLCommand := nil ;
{$IFDEF DELPHI_9_UP}
      gwst_SQLCommand := nil ;
{$ENDIF}
    End
  else
  if ( gdat_DataSetQuery <> nil )
  and not ( csDesigning in ComponentState )
  and assigned ( gdat_DataSetQuery ) Then
    Begin
      if not fb_GetSQLStrings ( gdat_DataSetQuery, gstl_SQLCommand{$IFDEF DELPHI_9_UP}, gwst_SQLCommand {$ENDIF})
       Then
         gdat_DataSetQuery := nil;
    End ;
end;

// Affectation du composant dans la propriété DataSource
// test si n'existe pas
// Mise à jour des paramètres et du code SQL
// a_Value : Le datasource
procedure TDBGroupView.p_SetDataSourceGroup ( const a_Value: TDataSource );
var old_Value: TDataSource ;
begin
  old_Value:= gdl_DataLink.Datasource ;
  inherited p_SetDataSourceGroup ( a_Value );
  if  not assigned ( gdl_DataLink.DataSet ) Then
    Begin
      // On a besoin du dataset
      gdl_DataLink.Datasource := nil;
      gstl_SQLSource := nil ;
{$IFDEF DELPHI_9_UP}
      gwst_SQLSource := nil ;
{$ENDIF}
    End
  else if ( old_Value <> a_Value )
   Then
    Begin
      fb_GetSQLStrings ( gdl_DataLink.DataSet, gstl_SQLSource{$IFDEF DELPHI_9_UP}, gwst_SQLSource {$ENDIF});
      fb_GetParamsDataset (gdl_DataLink.DataSet, gprs_ParamSource, gstl_params {$IFDEF EADO}, gprt_ParameterSource {$ENDIF});
    End ;
end;

// Evènement click colonne pour le tri
// alsc_colonne : la colonne à trier
procedure TDBGroupView.p_AssignColumnsSubitems;
var
   li_i, li_j : Integer;
begin
  if  ( assigned ( gstl_FieldsList ))
  and ( assigned ( gstl_UnitsDatasource ))
  and ( assigned ( gstl_KeyDataSource ))
    Then
      Begin
        if gstl_KeyDataSource.Count > 0
         Then
          SetLength ( gt_ColonneCle, gstl_UnitsDatasource.Count );
        for li_i := 0 to high ( gt_ColonneCle ) do
          gt_ColonneCle [ li_i ] := -1  ;
        // Gestion des subitems de la ListView
        for li_j := 0 to gstl_UnitsDatasource.Count - 1 do
         for li_i := 0 to gstl_FieldsList.Count - 1 do
           if gstl_KeyDataSource [ li_j + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ] = gstl_FieldsList [ li_i ] Then
             Begin
              gt_ColonneCle [ li_j ] := li_i ;
              Break;
             end;
        li_j := 0 ;
        for li_i := 0 to high ( gt_ColonneCle ) do
          if gt_ColonneCle [ li_i ] < 0 Then
            Begin
              gt_ColonneCle [ li_i ] := gstl_FieldsList.Count + li_j ;
              inc ( li_j );
            End ;
      End ;
End ;

procedure TDBGroupView.p_CreateSQL;
var li_i : Integer ;
{$IFDEF DELPHI_9_UP}
    ls_Query : String ;
{$ELSE}
    ls_Query : String ;
{$ENDIF}
    ls_Sort : String;
begin
  if not assigned ( ge_QueryAll )
  and assigned ( gdl_DataLink.DataSet )
  and assigned ( gstl_FieldsList )
  and not ( gdl_DataLink.DataSet.Active )
  and (( assigned ( gstl_SQLSource ) and ( trim ( gstl_SQLSource.Text ) = '' ))
 {$IFDEF DELPHI_9_UP}
  or ( assigned ( gwst_SQLSource) and ( trim ( gwst_SQLSource.Text ) = '' )){$ENDIF})
   Then
    Begin
      // Création de la sélection ( SELECT ) du query de Datasource
      ls_Query := CST_SQL_SELECT ;
      if ( gstl_FieldsList.Count > 0 ) then
        Begin
          AppendStr ( ls_Query, gs_TableSource + CST_FIELD_DECLARE_SEPARATOR +gstl_FieldsList [ 0 ] );
            for li_i := 1 to gstl_FieldsList.Count - 1 do
              Begin
                AppendStr ( ls_Query, ', ' + gs_TableSource + CST_FIELD_DECLARE_SEPARATOR + gstl_FieldsList [ li_i ] );
              End ;
          if assigned ( gstl_UnitsDatasource ) Then
            if gt_ColonneCle [ 0 ] >= gstl_FieldsList.Count Then
              for li_i := 0 to gstl_UnitsDatasource.Count - 1 do
                Begin
                  AppendStr ( ls_Query, ', ' + gs_TableSource + CST_FIELD_DECLARE_SEPARATOR + gstl_KeyDataSource [ li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ] );
  //                DataFieldsDisplay:=DataFieldsDisplay+FieldDelimiter+gstl_KeyDataSource [ li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ];
                End ;
        End ;
      if gs_FieldImage > '' Then
         AppendStr(ls_Query, ', ' + gs_TableSource + CST_FIELD_DECLARE_SEPARATOR + gs_FieldImage );
      AppendStr(ls_Query,#13#10);
        if gb_EstPrincipale Then
          Begin
            if ( gs_GroupTable = gs_TableSource ) Then
              Begin
                AppendStr ( ls_Query, CST_SQL_FROM + gs_TableSource );
                AppendStr ( ls_Query, CST_SQL_WHERE + gs_GroupField + ' = :GroupKey' +#13#10 );
              End
            Else
              Begin
                  // Propriété DatasourceGroupTable : gs_GroupTable
                AppendStr ( ls_Query, CST_SQL_FROM + gs_GroupTable + ' , ' + gs_TableSource  );
                AppendStr ( ls_Query, CST_SQL_WHERE + '(' + gs_TableSource + CST_FIELD_DECLARE_SEPARATOR + gstl_KeyDataSource [ gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ] + ' = ' + gs_GroupTable + '.' + gs_UnitsField +')');
                AppendStr ( ls_Query, CST_SQL_AND + gs_GroupTable + CST_FIELD_DECLARE_SEPARATOR + gs_GroupField + ' = :GroupKey' +#13#10 );
              End
          End
        Else
          if ( gs_GroupTable = gs_TableSource ) Then
            Begin
                AppendStr ( ls_Query, 'FROM ' + gs_TableSource );
              AppendStr ( ls_Query, CST_SQL_WHERE + gs_GroupField + CST_SQL_IS + CST_SQL_NULL +#13#10 );
            End
          Else
            Begin
              AppendStr ( ls_Query, CST_SQL_FROM + gs_TableSource  );
              for li_i := 0 to gstl_UnitsDatasource.Count - 1 do
                Begin
                  if li_i = 0
                   Then AppendStr(ls_Query,CST_SQL_WHERE + '(')
                   Else AppendStr(ls_Query,CST_SQL_AND);
                  AppendStr ( ls_Query,  gstl_KeyDataSource [ li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ] +
                                         ' NOT IN ( SELECT ' + gstl_UnitsDatasource [ li_i ] + CST_SQL_FROM + gs_GroupTable );
                  AppendStr ( ls_Query,  CST_SQL_WHERE +  gs_GroupField + ' = :GroupKey' + ' )' +#13#10 );
                  if li_i = gstl_UnitsDatasource.Count - 1
                   Then AppendStr(ls_Query,')'+#13#10);
                End ;
            End ;
          if not assigned ( GetPropInfo ( gdl_DataLink.DataSet, 'Sort' ))
          and ( SortColumn > -1 )
          and ( SortColumn < gstl_FieldsList.Count )
           Then
            Begin
             ls_Sort := fs_PrepareSorting;
             if ls_Sort > '' Then
               AppendStr(ls_Query,CST_SQL_ORDER_BY+ls_Sort);
            End;
          if Assigned ( gstl_SQLSource ) then
           with gstl_SQLSource do
            Begin
             Text := ls_Query ;
            End
{$IFDEF DELPHI_9_UP}
          else if Assigned ( gwst_SQLSource ) then
            gwst_SQLSource.Text := ls_Query
{$ENDIF};
          if assigned ( gprs_ParamSource     )
          and ( gprs_ParamSource.Count = 0 ) Then
            Begin
              gprs_ParamSource.Clear ;
              gprs_ParamSource.Add ;
              gprs_ParamSource.Items [ 0 ].Name := 'GroupKey' ;
              gprs_ParamSource.Items [ 0 ].ParamType := ptInput;
            End ;
{$IFDEF EADO}
          if assigned ( gprt_ParameterSource )
          and ( gprt_ParameterSource.Count = 0 )
           Then
            Begin
              gprt_ParameterSource.Clear ;
              gprt_ParameterSource.Add ;
              gprt_ParameterSource.Items [ 0 ].Name := 'GroupKey' ;
              gprt_ParameterSource.Items [ 0 ].Direction := pdInput;
              gprt_ParameterSource.Items [ 0 ].Attributes := [paSigned,paNullable];
            End ;
{$ENDIF}
   End ;
//  ShowMessage(ls_Query);
End ;

procedure TDBGroupView.p_DatasetNotify;
  procedure p_OnDatasetNotify ( const ADataset : TDataSet );
   Begin
     if assigned ( ADataset ) Then
      ge_DatasetParam(ADataset);
   end;
Begin
  if Assigned(ge_DatasetParam) Then
    Begin
     p_OnDatasetNotify(gdat_Query1);
     p_OnDatasetNotify(gdat_Query2);
     p_OnDatasetNotify(gdat_QuerySource);
     p_OnDatasetNotify(gdat_DatasetRefreshOnError);
    end;

end;

procedure TDBGroupView.p_SetDatasources ;
var
  ldat_Dataset : TDataset ;
begin
  if not ( csDesigning in ComponentState )
  and  ( not assigned ( ds_DatasourceQuery )
    or   not assigned ( gdat_DataSetQuery )
    or (  not assigned ( gdl_DataLink     .DataSet )and ( gb_EstPrincipale or ( gs_GroupTable <> gs_TableSource )))
    or   not assigned ( gdl_DataLinkOwner.DataSet ))
  and  (    assigned ( gdl_DataLink     .DataSet )
         or assigned ( gdl_DataLinkOwner.DataSet )) Then
    Begin
      if assigned ( gdl_DataLinkOwner.DataSet ) Then
        ldat_Dataset := gdl_DataLinkOwner.DataSet
       else
        ldat_Dataset := gdl_DataLink.DataSet;
      if (   not assigned ( gdl_DataLink     .DataSet )
          or not assigned ( gdl_DataLinkOwner.DataSet ))
      and not assigned ( gds_Querysource ) Then
        Begin
          gdat_QuerySource := fdat_CloneDatasetWithoutSQL ( ldat_Dataset, Self );
          gdat_QuerySource.Name:='gat_QuerySource' + Self.Name ;
          gds_Querysource := TDataSource.Create ( Owner ) ;
          gds_Querysource.Name:='gds_QuerySource' + Self.Name ;
          gds_Querysource.DataSet := gdat_QuerySource ;
        End ;

       if not assigned ( ds_DatasourceQuery )
       and not assigned ( gds_Query1 ) Then
        Begin
          gdat_Query1 := fdat_CloneDatasetWithoutSQL ( ldat_Dataset, Self );
          gdat_Query1.Name:='gat_Query1'+ Self.Name ;
          gds_Query1 := TDataSource.Create ( Owner ) ;
          gds_Query1.Name:='gds_Query1' + Self.Name ;
          gds_Query1.DataSet := gdat_Query1 ;
          DatasourceQuery := gds_Query1 ;
        end ;
       if not assigned ( ds_DataSourceQuery2 )
       and not assigned ( gds_Query2 ) Then
         Begin
           gdat_Query2 := fdat_CloneDatasetWithoutSQL ( ldat_Dataset, Self, True );
           gdat_Query2.Name:='gat_Query2' + Self.Name ;
           gds_Query2 := TDataSource.Create ( Owner ) ;
           gds_Query2.Name:='gds_Query2' + Self.Name ;
           gds_Query2.DataSet := gdat_Query2 ;
           DatasourceQuery2 := gds_Query2 ;
         end ;
       if not assigned ( gdat_DataSetQuery )  Then
         Begin
           DatasetQuery := fdat_CloneDatasetWithoutSQL ( ldat_Dataset, Self, True );
           gdat_DataSetQuery.Name:='gat_QuerySQL' + Self.Name ;
         end ;
      if not assigned ( gdl_DataLink.DataSet )
      and ( gb_EstPrincipale or ( gs_GroupTable <> gs_TableSource )) Then
       Begin
        Datasource := gds_Querysource ;
       end
      else
        if not assigned ( gdl_DataLinkOwner.DataSet ) Then
         Begin
          DataSourceOwner := gds_Querysource ;
         end ;
      p_DatasetNotify;
    End ;
End;

// Initialisation des images d'état
procedure TDBGroupView.p_SetListImages ;
var lbmp_Image : TBitmap ;
Begin
  if not assigned ( {$IFDEF FPC}SmallImages{$ELSE}StateImages{$ENDIF} ) Then
    Begin
      if not assigned ( gim_GroupViewImageList )
      Then
        Begin
          gim_GroupViewImageList := TImageList.Create(nil);
          gim_GroupViewImageList.BkColor := clBlack;
          gim_GroupViewImageList.BlendColor := clBlack;
        End;
      lbmp_Image := TBitmap.Create;
      {$IFNDEF FPC}
      ResInstance:= FindResourceHInstance(HInstance);
      {$ENDIF}
      {$IFDEF FPC}
      lbmp_Image.LoadFromLazarusResource( 'GROUPVIEW_MINUS' );
      {$ELSE}
      lbmp_Image.LoadFromResourceName(ResInstance,'GROUPVIEW_MINUS');
      {$ENDIF}
      gim_GroupViewImageList.AddMasked(lbmp_Image, lbmp_Image.TransparentColor );
      {$IFDEF FPC}
      lbmp_Image.LoadFromLazarusResource( 'GROUPVIEW_PLUS' );
      {$ELSE}
      lbmp_Image.LoadFromResourceName(ResInstance,'GROUPVIEW_PLUS');
      {$ENDIF}
      gim_GroupViewImageList.AddMasked(lbmp_Image, lbmp_Image.TransparentColor );
      {$IFNDEF FPC}
      lbmp_Image.Dormant ;
      {$ENDIF}
      lbmp_Image.FreeImage ;
      lbmp_Image.Handle := 0 ;
      {$IFDEF FPC}SmallImages{$ELSE}StateImages{$ENDIF} := gim_GroupViewImageList;
    End;
End;

procedure TDBGroupView.p_SetHintString ( const awin_Control : TWincontrol ; const as_Hint : String );
var lvar_Valeur : Variant ;
Begin
  if assigned ( awin_Control )
   Then
    Begin
      if awin_Control.Hint = '' Then
        Begin
          if  IspublishedProp ( awin_Control, 'ParentShowHint' )
          and PropIsType      ( awin_Control, 'OnEnter', tkEnumeration ) Then
            lvar_Valeur := GetPropValue   (  awin_Control, 'ParentShowHint' );
          if  ( lvar_Valeur <> Null )
          and ( TVarData( lvar_Valeur ).VType = 256 ) Then
            SetPropValue   (  awin_Control, 'ParentShowHint', False );
          awin_Control.ShowHint := True ;
          awin_Control.Hint := as_Hint ;
        End ;
    End ;
end;

procedure  TDBGroupView.p_SetClickEvent ( const awin_Control : TWincontrol ; const as_procedure : String ; const AOldEvent : TNotifyEvent );
var lmet_MethodeDistribuee : TMethod ;
Begin
  if assigned ( awin_Control )
   Then
    Begin
      // affectation de la méthode du nouvel évènement
      lmet_MethodeDistribuee .Data := Self ;
      lmet_MethodeDistribuee.Code := MethodAddress ( as_procedure );
      // Default : Not enabled
      awin_Control.Enabled:=False;
      // récupération et Affectation
      if  IsPublishedProp ( awin_Control, 'OnClick'           )
      and PropIsType      ( awin_Control, 'OnClick', tkMethod )
       Then
        try
          ge_RecordClick := TNotifyEvent ( GetMethodProp ( awin_Control, 'OnClick' ));
          SetMethodProp ( awin_Control, 'OnClick', lmet_MethodeDistribuee );
        Except
        End ;
    End ;
End;
// Initialisation des variables en fonction des propriétés
procedure TDBGroupView.p_ListLoaded ;
// Méthode évènement
{$IFNDEF FPC}
var
  lmet_MethodeDistribueeSelect : TMethod ;
  {$ENDIF}

begin
  {$IFNDEF FPC}
  ge_Onselectitem := OnSelectItem;
  lmet_MethodeDistribueeSelect .Data := Self ;
  lmet_MethodeDistribueeSelect.Code := MethodAddress ( 'DoSelectItem' );
  OnSelectItem := TLVSelectItemEvent ( lmet_MethodeDistribueeSelect );
  p_SetListImages ;
  {$ENDIF}
  // Si on est en exécution
  if assigned ( gBt_Basket )
  and ( gs_GroupTable = gs_TableSource ) Then
    gb_Basket := True
  Else
    gb_Basket := False ;

  p_SetDatasources;
  p_CreateSQL;

  // Initialisation du Basket si il y a un Basket
  If Hint = ''
   Then
    Begin
      if gb_EstPrincipale
       Then Hint := GS_GROUP_INCLUDE_LIST
       Else Hint := GS_GROUP_EXCLUDE_LIST ;
    End ;
  p_ChampsVersListe ( gstl_GroupKey, gs_GroupKey, FieldDelimiter );
  p_ChampsVersListe ( gstl_GroupField, gs_GroupField, FieldDelimiter );

  // Est-ce la liste d'enregistrement et d'abandon : liste principale
  if gb_EstPrincipale
   Then
    Begin

    // Affectation de l'évènement onclick d'enregistre
      p_SetClickEvent ( gBt_Record  , 'p_Record'     , ge_RecordClick    );
      // Affectation de l'évènement onclick d'inversion
      p_SetClickEvent ( gbt_Exchange, 'p_InvertClick', ge_BtnInvertClick );
      // Affectation de l'évènement onclick d'abandonne
      p_SetClickEvent ( gBT_Cancel  , 'p_Cancel'     , ge_CancelClick );
      p_SetClickEvent ( gBT_Basket  , 'p_VideBasket' , ge_BasketClick );
      p_SetHintString ( gBt_Basket, GS_GROUPE_RETOUR_ORIGINE );
      p_SetHintString ( gBT_TotalList, GS_GROUPE_TOUT_INCLURE );
      p_SetHintString ( gBT_List, GS_GROUPE_INCLURE );
    End
  // fin de l'affectation des évènements de la liste principale
  Else
   begin
    p_SetHintString ( gBT_TotalList, GS_GROUPE_TOUT_EXCLURE );
    p_SetHintString ( gBT_List, GS_GROUPE_EXCLURE );
   End;

   SetLength(gstl_KeysListOut,1);
   SetLength(gstl_KeysListOut[0],gstl_UnitsDatasource.Count);

    // Affectation de l'évènement onclick du bouton de la liste
  p_SetClickEvent ( gBT_List      , 'p_ClickTransfert'      , ge_ListClick );
  // Affectation de l'évènement onclick du bouton total de la liste
  p_SetClickEvent ( gBT_TotalList , 'p_ClickTransfertTotal' , ge_TotalListClick );
  // Fin de l'affectation des propriétés au premier chargement
  DataLinkScrolled(gdl_DataLinkOwner.DataSet);
End;

procedure TDBGroupView.DataLinkLoadList;
begin
  if gb_Basket and gb_EstPrincipale Then
    gb_Basket := True;
  // et si il est actif
  if assigned ( gdl_DataLinkOwner.DataSet )
  and not gb_Open
  and not gb_LoadList
  and not ( gdl_DataLinkOwner.DataSet.BOF and gdl_DataLinkOwner.DataSet.EOF )
    Then
      try
        // Alors on met à jour la liste
        {$IFDEF EADO}
        p_SetFetchLoaded ;
        {$ENDIF}
        gb_Open := True ;
        LoadList ;
      finally
        gb_Open := False ;
      End;

End;


 ///////////////////////////////////////////////////////////////
// TDBGroupView                                           //
//////////////////////////////////////////////////////////////
// Affectation du composant dans la propriété DataSourceOwner
// test si n'existe pas
procedure TDBGroupView.p_SetDataSourceOwner ( const a_Value: TDataSource );
var ls_Table : String;
begin
{$IFNDEF FPC}
  ReferenceInterface ( DataSourceOwner, opRemove ); // Gestion de la destruction
{$ENDIF}
  // Affectation
  if gdl_DataLinkOwner.DataSource <> a_Value then
  begin
    gdl_DataLinkOwner.DataSource := a_Value ;
  end;
{$IFNDEF FPC}
  ReferenceInterface ( DataSourceOwner, opInsert ); // Gestion de la destruction
{$ENDIF}

   // Récupération de la table
  // Y-a-t-il un dataset
  if assigned ( gdl_DataLinkOwner.DataSet ) Then
    Begin
      // Récupération de la connexion
      if HasLoad
      and not ( csCreating in ControlState ) then
        Begin
          p_SetDatasources;
          DataLinkScrolled(gdl_DataLinkOwner.DataSet);
        End;
     ls_Table := trim (fs_getComponentProperty ( gdl_DataLinkOwner.DataSet, 'TableName' ));
     if ls_Table <> '' Then
       gs_TableOwner := ls_Table;
    end;
end;
// Erreur si mauvais bouton BtnTotalIn affecté
// Résultat :  le message d'erreur a été affiché
function TDBGroupView.fb_ErreurBtnTotalIn : Boolean ;
Begin
  Result := False ;
  if  assigned ( ButtonTotalIn     )
  and     ( csDesigning in ComponentState )
  and not ( csLoading   in ComponentState )
  and assigned ( galv_OtherList )
  and ( ButtonTotalIn = galv_OtherList.ButtonTotalIn )
   Then
    Begin
      ShowMessage ( GS_GROUPE_MAUVAIS_BOUTONS );
      Result := True ;
    End ;
End ;

// Erreur si mauvais bouton BtnIn affecté
// Résultat :  le message d'erreur a été affiché
function TDBGroupView.fb_ErreurBtnIn : Boolean ;
Begin
  Result := False ;
  if  assigned ( ButtonIn     )
  and     ( csDesigning in ComponentState )
  and not ( csLoading   in ComponentState )
  and assigned ( galv_OtherList )
  and ( ButtonIn = galv_OtherList.ButtonIn )
   Then
    Begin
      ShowMessage ( GS_GROUPE_MAUVAIS_BOUTONS );
      Result := True ;
    End ;
End ;

// Erreur si mauvais bouton BtnTotalOut affecté
// Résultat :  le message d'erreur a été affiché
function TDBGroupView.fb_ErreurBtnTotalOut : Boolean ;
Begin
  Result := False ;
  if  assigned ( ButtonTotalOut     )
  and     ( csDesigning in ComponentState )
  and not ( csLoading   in ComponentState )
  and assigned ( galv_OtherList )
  and ( ButtonTotalOut = galv_OtherList.ButtonTotalOut )
   Then
    Begin
      ShowMessage ( GS_GROUPE_MAUVAIS_BOUTONS );
      Result := True ;
    End ;
End ;

// Erreur si mauvais bouton BtnOut affecté
// Résultat :  le message d'erreur a été affiché
function TDBGroupView.fb_ErreurBtnOut : Boolean ;
Begin
  Result := False ;
  if  assigned ( ButtonOut     )
  and     ( csDesigning in ComponentState )
  and not ( csLoading   in ComponentState )
  and assigned ( galv_OtherList )
  and ( ButtonOut = galv_OtherList.ButtonOut )
   Then
    Begin
      ShowMessage ( GS_GROUPE_MAUVAIS_BOUTONS );
      Result := True ;
    End ;
End ;

// Cherche un item : Utiliser plutôt le locate du dataset
// as_TexteItem : Item principal ou clé à trouver
// Résultat : numéro de l'item ou -1
{function TDBGroupView.fi_FindItem ( const avar_TexteItem : Variant ; const ab_FirstPlusMinorOption : Byte ) : Integer ;
// Compteur
var li_i , li_j : Integer ;
    lb_Trouve : Boolean ;
Begin
// Rien n'est encore trouvé
  Result := -1 ;
  // Parcourt des items principaux
  for li_i := 0 to Items.Count - 1 do
    Begin
      case ab_FirstPlusMinorOption of
        1 : if not ( Items [ li_i ].StateIndex in [ gi_ImageInsere, gi_ImageSupprime ]) Then
              Exit ;
        2 : if not ( Items [ li_i ].StateIndex in [ gi_ImageInsere, gi_ImageSupprime ]) Then
              Continue ;
      End ;
      lb_Trouve := False ;
      for li_j := 0 to high ( gt_ColonneCle ) do
      // Si on le trouve
        if (     ( gt_ColonneCle [ 0 ]    <= 0    )
             and ( VarCompareValue ( Items [ li_i ].Caption, avar_TexteItem ) = vrEqual ))
        or (     ( gt_ColonneCle [ 0 ]    > 0    )
             and ( VarCompareValue ( Items [ li_i ].SubItems [ gt_ColonneCle [ 0 ] - 1 ], avar_TexteItem ) = vrEqual ))
         Then
          Begin
            // Retourne le compteur
            lb_Trouve := True ;
          End ;
      if lb_Trouve Then
        Begin
          Result := li_i ;
          // C'est fini
          Break ;
        End ;
    End ;
End ;
}
// Affectation du composant dans la propriété ButtonTotalIn
// test si n'existe pas
procedure TDBGroupView.p_SetTotalList  ( const a_Value: TWinControl );
begin
{$IFNDEF FPC}
  ReferenceInterface ( ButtonTotalIn, opRemove ); // Gestion de la destruction
{$ENDIF}
  // Affectation
  if gBt_TotalList <> a_Value then
  begin
    gBt_TotalList := a_Value ;
  end;
{$IFNDEF FPC}
  ReferenceInterface ( ButtonTotalIn, opInsert ); // Gestion de la destruction
{$ENDIF}
  fb_ErreurBtnTotalIn ;
end;

// Affectation du composant dans la propriété ButtonIn
// test si n'existe pas
procedure TDBGroupView.p_SetBtnList  ( const a_Value: TWinControl );
begin
{$IFNDEF FPC}
  ReferenceInterface ( ButtonIn, opRemove ); // Gestion de la destruction
{$ENDIF}
  // Affectation
  if gBt_List <> a_Value then
  begin
    gBt_List := a_Value ;
  end;
{$IFNDEF FPC}
  ReferenceInterface ( ButtonIn, opInsert ); // Gestion de la destruction
{$ENDIF}
  fb_ErreurBtnIn ;
end;

// Affectation du composant dans la propriété ButtonTotalOut
// test si n'existe pas
procedure TDBGroupView.p_SetAutreTotal  ( const a_Value: TWinControl );
begin
{$IFNDEF FPC}
  ReferenceInterface ( ButtonTotalOut, opRemove ); // Gestion de la destruction
{$ENDIF}
  // Affectation
  if gBt_OtherTotal <> a_Value then
  begin
    gBt_OtherTotal := a_Value ;
  end;
{$IFNDEF FPC}
  ReferenceInterface ( ButtonTotalOut, opInsert ); // Gestion de la destruction
{$ENDIF}
  fb_ErreurBtnTotalOut ;
end;

// Affectation du composant dans la propriété ButtonOut
// test si n'existe pas
procedure TDBGroupView.p_SetOtherListBtn  ( const a_Value: TWinControl );
begin
{$IFNDEF FPC}
  ReferenceInterface ( ButtonOut, opRemove ); // Gestion de la destruction
{$ENDIF}
  // Affectation
  if gBt_Other <> a_Value
   then
    begin
      gBt_Other := a_Value ;
    end;
{$IFNDEF FPC}
  ReferenceInterface ( ButtonOut, opInsert ); // Gestion de la destruction
{$ENDIF}
  fb_ErreurBtnTotalOut ;
end;

procedure TDBGroupView.p_setBtnBasket(const a_Value: TWinControl);
begin
{$IFNDEF FPC}
  ReferenceInterface ( ButtonBasket, opRemove ); // Gestion de la destruction
{$ENDIF}
  // Affectation
  if gBt_Basket <> a_Value
   then
    begin
      gBt_Basket := a_Value ;
    end;
{$IFNDEF FPC}
  ReferenceInterface ( ButtonBasket, opInsert ); // Gestion de la destruction
{$ENDIF}
  if assigned ( gBt_Basket )
  and     ( csDesigning in ComponentState )
  and not ( csLoading   in ComponentState )
   Then
    gb_MontreTout := True ;
end;

// Affectation du composant dans la propriété DataOtherList
// test si n'existe pas
procedure TDBGroupView.p_SetOtherList  ( const a_Value: TDBGroupView );
begin
{$IFNDEF FPC}
  ReferenceInterface ( DataListOpposite, opRemove ); // Gestion de la destruction
{$ENDIF}
  // Affectation
  if  ( galv_OtherList <> a_Value )
  // La liste doit ne pas être elle même
  and ( a_Value         <> Self    )
   then
    begin
      galv_OtherList := a_Value ;
    end;
{$IFNDEF FPC}
  ReferenceInterface ( DataListOpposite, opInsert ); // Gestion de la destruction
{$ENDIF}
  if assigned ( galv_OtherList )
  and not fb_ErreurBtnTotalOut
  and not fb_ErreurBtnTotalIn
  and not fb_ErreurBtnOut
   Then
    fb_ErreurBtnIn ;
end;

// Affectation du composant dans la propriété ButtonRecord
// test si n'existe pas
procedure TDBGroupView.p_setEnregistre(const a_Value: TWinControl);
begin
{$IFNDEF FPC}
  ReferenceInterface ( ButtonRecord, opRemove ); // Gestion de la destruction
{$ENDIF}
  // Affectation
  if gBt_Record <> a_Value
   then
    begin
      gBt_Record := a_Value ;
    end;
{$IFNDEF FPC}
  ReferenceInterface ( ButtonRecord, opInsert ); // Gestion de la destruction
{$ENDIF}
end;

// Affectation du composant dans la propriété ButtonCancel
// test si n'existe pas
procedure TDBGroupView.p_setAbandonne(const a_Value: TWinControl);
begin
{$IFNDEF FPC}
  ReferenceInterface ( ButtonCancel, opRemove ); // Gestion de la destruction
{$ENDIF}
  // Affectation
  if gBt_Cancel <> a_Value
   then
    begin
      gBt_Cancel := a_Value ;
    end;
{$IFNDEF FPC}
  ReferenceInterface ( ButtonCancel, opInsert ); // Gestion de la destruction
{$ENDIF}
end;
// TRim sur la chaîne à l'affectation de gs_GroupKey
// a_Value : valeur avec peut-être des espaces
procedure TDBGroupView.p_setGroupKey(const a_Value: String);
begin
  // Affectation
  if gs_GroupKey <> Trim ( a_Value )
   then
    begin
      gs_GroupKey := Trim ( a_Value );
    end;
end;
// TRim sur la chaîne à l'affectation de gs_UnitsField
// a_Value : valeur avec peut-être des espaces
procedure TDBGroupView.p_setUnitsField(const a_Value: String);
begin
  // Affectation
  if gs_UnitsField <> Trim ( a_Value )
   then
    begin
     gs_UnitsField := Trim ( a_Value ) ;
     if ( trim (gs_UnitsField) > '' )
      Then
       Begin
         p_ChampsVersListe ( gstl_UnitsDatasource, trim(gs_UnitsField), FieldDelimiter );
         gs_UnitsFieldValues:= fs_RemplaceChar( gs_UnitsField, FieldDelimiter, ';' );
       End;
    end;
end;
// TRim sur la chaîne à l'affectation de gs_GroupField
// a_Value : valeur avec peut-être des espaces
procedure TDBGroupView.p_SetGroupField ( const a_Value: String );
begin
  // Affectation
  if gs_GroupField <> Trim ( a_Value )
   then
    begin
      gs_GroupField := Trim ( a_Value );
    end;
end;
// Teste si la valeur de l'image d'insertion est égale à -1
// a_Value : Valeur à tester
procedure TDBGroupView.p_SetImageInsere(const a_Value: Integer);
begin
  if a_Value <> -1
   Then
     gi_ImageInsere := a_Value ;

end;

// Teste si la valeur de l'image de suppression est égale à -1
// a_Value : Valeur à tester
procedure TDBGroupView.p_SetImageSupprime(const a_Value: Integer);
begin
  if a_Value <> -1
   Then
     gi_ImageSupprime := a_Value ;

end;

// TRim sur la chaîne à l'affectation de gs_GroupTable
// a_Value : valeur avec peut-être des espaces
procedure TDBGroupView.p_SetGroupTable ( const a_Value: String );
begin
  // Affectation
  if gs_GroupTable <> Trim ( a_Value )
   then
    begin
      gs_GroupTable := Trim ( a_Value );
    end;
end;

function TDBGroupView.fs_GetFieldsValuesInsert : String;
begin
  if Assigned(ge_addFieldsValues)
   Then Result := ge_addFieldsValues ( Self )
   Else Result := '';
End;

function TDBGroupView.fs_GetFieldsInsert : String;
begin
  if Assigned(ge_addFields)
   Then Result := ge_AddFields ( Self )
   Else Result := '';
End;

{ TDBGroupView }

// Création du composant
// acom_owner : Le propriétaire de la liste
constructor TDBGroupView.Create(acom_owner: TComponent);

begin
  // héritage de la création
  inherited create ( acom_owner );

  gdl_DataLinkOwner := TUltimListViewDatalink.Create ( Self );
  gstl_UnitsDatasource := TStringList.Create;
  gb_SelfOpen := False ;
  gds_QuerySource := nil ;
  gds_Query1 := nil ;
  gds_Query2 := nil ;
  ds_DatasourceQuery := nil ;
  gdat_DataSetQuery := Nil ;
  gdat_Query2 := nil;
  gdat_Query1 := nil;
  gdat_QuerySource := nil;
  gdat_DatasetRefreshOnError := nil;
  gstl_SQLCommand := nil ;
  gstl_SQLQuery := nil ;
  gstl_SQLSource := nil ;
  ge_addFieldsValues := nil;
  ge_addFields := nil;
{$IFDEF DELPHI_9_UP}
  gwst_SQLCommand := nil ;
  gwst_SQLSource := nil ;
  gwst_SQLQuery := nil ;
{$ENDIF DELPHI_9_UP}
  gstl_GroupKey     := nil ;
  gstl_GroupField   := nil ;
  gstl_params       := nil ;

  // initialisation
  gb_NoScroll := False ;
  gb_Open     := False ;
//  gb_ToutUneFois := True ;
//  gadoc_maj_groupe  .Mode := cmWrite ;
//  lw_NombrePages            := 0 ;
  gvar_WorkingGroup        := Null ;
  gb_CaseInSensitive        := True ;
  gb_EstPrincipale           := True ;
  gb_AllLoaded              := False ;
  gb_AllFetched             := True ;
  gb_OptionTotalList              := False ;
  gb_TotalListReal          := False ;
//  gb_TrieAsc                := True ;
  gb_Record             := False ;
  gi_ImageInsere            := 1;
  gi_ImageSupprime          := 0;
  // Aide
  ShowHint := True ;
end;

// Destruction du composant
destructor TDBGroupView.Destroy;
  procedure p_OnDestroyDataset ( const ADataset : TDataSet );
   Begin
     if assigned ( ADataset ) Then
      ge_DatasetDestroy(ADataset);
   end;

begin
  galv_OtherList := nil;
  gdl_DataLink.DataSource:=nil;
  gdl_DataLinkOwner.DataSource:=nil;
  if Assigned(ge_DatasetDestroy) Then
   Begin
    p_OnDestroyDataset ( gdat_Query1 );
    p_OnDestroyDataset ( gdat_Query2 );
    p_OnDestroyDataset ( gdat_QuerySource );
    p_OnDestroyDataset ( gdat_DatasetRefreshOnError );
   end;

  // Liste des clés : à libérer
  Finalize ( gstl_KeysListOut  );
  Finalize ( gt_KeyOwners      );
  inherited;
  // Libération du lien de données des groupes
  gstl_UnitsDatasource.Destroy;

  // Libération des listes de champs
  if assigned ( gstl_GroupKey     ) Then  gstl_GroupKey    .Free ;
  if assigned ( gstl_GroupField   ) Then  gstl_GroupField  .Free ;
  gstl_GroupKey   := nil ;
  gstl_GroupField := nil ;
//  gadoc_maj_groupe  est affecté à la liste : pas besoin de le détruire ;
  // Libération des composants utilisés dans les propriétés
end;

// Suppression des composants détruits
// AComponent : Le composant à détruire
// Operation  : Opération à effectuer : Suppression ou ajout
procedure TDBGroupView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  // Procédure héritée
  inherited Notification(AComponent, Operation);
   ////////////////////////////////////////////
  // Désaffectation des composants détruits //
 ////////////////////////////////////////////
  if ( Operation <> opRemove )
  or ( csDestroying in ComponentState ) Then
    Exit;

  if  ( AComponent = DataListOpposite ) then DataListOpposite := nil;
  if  ( AComponent = DataSourceOwner  ) then DataSourceOwner  := nil;
  if  ( AComponent = DataSourceQuery2 ) then DataSourceQuery2 := nil;
  if  ( AComponent = DatasourceQuery  ) then DatasourceQuery  := nil;
  if  ( AComponent = DatasetQuery     ) then DatasetQuery     := nil;
  if  ( AComponent = ButtonTotalIn    ) then ButtonTotalIn    := nil;
  if  ( AComponent = ButtonIn         ) then ButtonIn         := nil;
  if  ( AComponent = ButtonBasket     ) then ButtonBasket     := nil;
  if  ( AComponent = ButtonTotalOut   ) then ButtonTotalOut   := nil;
  if  ( AComponent = ButtonOut        ) then ButtonOut        := nil;
  if  ( AComponent = ButtonRecord     ) then ButtonRecord     := nil;
  if  ( AComponent = ButtonCancel     ) then ButtonCancel     := nil;
end;

// Le groupe a changé : méthode virtuelle
procedure TDBGroupView.DataLinkScrolled ( const adat_Dataset : TDataSet );
Begin
  if adat_Dataset <> gdl_DataLinkOwner.DataSet Then Exit;
  if gb_Basket and gb_EstPrincipale Then
   gb_LoadList:=False;
  DataLinkLoadList;
end;

procedure TDBGroupView.LoadList;
var lb_LoadList : Boolean ;
begin
  if (csDestroying In ComponentState) Then
    Exit ;
  lb_LoadList := True ;
  if csDesigning in ComponentState Then
    Begin
      P_Reinitialise ;
      Exit ;
    End ;
  if assigned  ( BeforeDataScroll )
  and assigned ( gdl_DataLink.DataSource )
  and assigned ( gdl_DataLink.DataSet ) then
    BeforeDataScroll ( Self, gdl_DataLink.DataSet, lb_LoadList );

  if not lb_LoadList
   Then
    Begin
      p_Reinitialise ;
      Exit ;
    End ;
  if not gb_Basket
  and assigned ( gdl_DataLinkOwner.DataSet )
  and gdl_DataLinkOwner.DataSet.IsEmpty Then
    Begin
      p_Reinitialise ;
      p_VerifieModifications ;
      Exit ;
    End ;
  // Basket : il faut enregistrer ou abandonner avant de changer d'enregistrements
 if  gb_Basket
 and gb_NoScroll
 and gb_EstPrincipale
 and assigned ( galv_OtherList ) Then
 // Y-a-t-il des clés à enregistrer
   Begin
     // La clé de destination n'est plus celle en cours et on a affecté avant
     if gvar_KeyDestination <> gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).Value Then
       Begin
         /// message d'Abandon
         MessageDlg ( GS_GROUPE_ABANDON , mtWarning, [mbOK], 0 );
         // retour
         gdl_DataLinkOwner.DataSet.Locate ( gs_GroupKey, gvar_KeyDestination, [] );
         /// Abandon
         Exit ;
       End
      Else
        Begin
         /// Abandon : on est revenu sur la bonne clé
          Exit ;
        End ;
   End ;
 // On remet à jour la liste
 if  not gb_Record
 // Gestion du Basket : on ne vide pas le Basket
 and ( not gb_Basket Or
       not assigned ( gdl_DataLinkOwner.DataSet )
        or  gb_EstPrincipale )
  Then
   Begin
    if assigned ( galv_OtherList ) Then
       galv_OtherList.gb_TotalListReal := False ;
     Refresh ;
   End ;
  if assigned  ( AfterDataScroll )
  and assigned ( gdl_DataLink.DataSet ) then
    AfterDataScroll ( gdl_DataLink.DataSet );
end;

// Insertion d'un item : appelle fb_InsereEnregistremnts
// Résultat      : A-t-on changé l'état
procedure TDBGroupView.p_UndoRecord;
var lb_VerifieModifs : Boolean ;
Begin
  lb_VerifieModifs := False ;
  // Mise à zéro des boutons
  if assigned ( gBt_Cancel )
  and gBt_Cancel .enabled Then
    Begin
      gBt_Cancel .enabled := False;
      lb_VerifieModifs := True ;
    End ;
  if assigned ( gBT_Optional )
  and gBT_Optional .enabled Then
    Begin
      gBT_Optional .enabled := False;
      lb_VerifieModifs := True ;
    End ;
  if assigned ( gBt_Record )
  and gBt_Record.enabled Then
    Begin
      gBt_Record.enabled := False;
      lb_VerifieModifs := True ;
    End ;
  if lb_VerifieModifs Then
    p_VerifieModifications ;
End ;
// Insertion d'un item : appelle fb_InsereEnregistremnts
// Résultat      : A-t-on changé l'état
function TDBGroupView.fb_SetList:Boolean;
Begin
    // Gestion normale
    if not gb_AllSelect or gb_MontreTout
    or ( gb_Basket and not gb_OptionTotalList )
     Then
      with gdl_DataLink.DataSet do
      Begin
        // A-t-on changé d'enregistrement
        if ( gvar_WorkingGroup = null  )
        or not assigned ( gdl_DataLinkOwner.DataSet )
        or ( gvar_WorkingGroup <> gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).Value )
         Then
          begin
            p_UndoRecord ;
            p_SetButtonsOther ( False );
            // Gestion du tri
            fs_SortDataset ( gdl_DataLink.DataSet );
            // gestion des paramètres d'un query éventuel
            // Par défaut si c'est un query et du'il y a des paramètres
            // Alors Affectation du premier paramètre en tant que paramètre de la clé du groupe
            if assigned ( ge_FilterEvent ) Then
              Begin
                ge_FilterEvent ( gdl_DataLink.DataSet );
              End
            else
              if not fb_OpenParamsQuery
                // Si ce n'est pas un query ou si c'est un query sans paramètre
               Then
                 p_OpenQuery;
              // Assignation du sort
            if Active
            and assigned ( gdl_DataLinkOwner.DataSet ) Then
              Begin
                try
                  gb_SelfOpen := True ;
                   // Si tout a été transféré
                   if  gb_OptionTotalList
                   and gb_AllSelect
                   and not gb_MontreTout
                    Then
                     Begin
                       // Assignation du sort du query
                       if fb_AssignSort ( ds_DatasourceQuery.DataSet, gstl_FieldsList, SortColumn ) Then
                         First ;
                     End
                    Else
                      // Sinon assignation au sort du datasource.dataset
                      if fb_AssignSort ( gdl_DataLink.DataSet, gstl_FieldsList, SortColumn ) Then
                        First ;

                finally
                  gb_SelfOpen := False ;
                End ;
                gvar_WorkingGroup := gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).Value ;
                gb_AllLoaded := False ;
              End ;
          End
         Else
           //  la liste est en train d'être parcourue
          Begin
            // Aller à l'enregistrement
            if ( gbm_DernierEnregistrement <> {$IFDEF WITH_TBOOKMARK}nil{$ELSE}''{$ENDIF} )
             Then
              Begin
                  // L'enregistrement peut avoir été effacé
                try
                // aller au bookmark
                  Bookmark := gbm_DernierEnregistrement ;
                  // Plus besoin du bookmark
                except
                end ;
              End ;
          End ;
      End ;
      // Gestion tranfert total dans la liste des enregistrements
//       Else
     // Si premier parcourt des enregistrements du tranfert total dans la liste
{        if gb_ToutUneFois
       Then
        Begin
          if assigned ( gBt_Cancel )
          and gBt_Cancel .enabled Then
            Begin
              gBt_Cancel .enabled := False;
              lb_VerifieModifs := True ;
            End ;
          if assigned ( gBt_Record )
          and gBt_Record.enabled Then
            Begin
              gBt_Record.enabled := False;
              lb_VerifieModifs := True ;
            End ;
          if assigned ( gBT_Optional )
          and gBT_Optional.enabled Then
            Begin
              gBT_Optional.enabled := False;
              lb_VerifieModifs := True ;
            End ;
//            gb_AllLoaded     := False ; // Rien n'est chargé et il faut charger des enregistrements
//            gbm_DernierEnregistrement := '';  // mise à nil du bookm
          // tri

//            fb_Sort ( gi_ColonneTrie )
        End
       else
        Begin                           }
          /// Si pas la première fois alors on va au bookmark du dernier enregistrement non ajouté
//          End ;
//      gb_ToutUneFois   := False ;  /// Plus de premier parcourt
    if  gb_EstPrincipale
    and gb_Basket
    and assigned ( gdl_DataLinkOwner.DataSet ) Then
      gvar_WorkingOriginKey := gdl_DataLinkOwner.DataSet.FieldValues [ gs_GroupKey ];
        // Si gestion de transfert total dans la liste
    if gb_AllSelect
    and not gb_MontreTout
    and ( not gb_Basket or gb_OptionTotalList )
      // Ajoute à partir de tous les enregistrements que l'on paut ajouter
     Then
      Begin
        Result := True ;
        if not gb_AllLoaded
        and ( gbm_DernierEnregistrement <> {$IFDEF WITH_TBOOKMARK}nil{$ELSE}''{$ENDIF} )
         Then
          try
          // Aller à l'enregistrement
            ds_DatasourceQuery.DataSet.Bookmark := gbm_DernierEnregistrement ;
          except
          End;
        {$IFDEF EADO}
        if assigned ( gstl_SQLQuery )
        and ( ds_DatasourceQuery.DataSet.State <> dsInactive ) Then
        {$ENDIF}
          fb_AddRecords ( ds_DatasourceQuery.DataSet, False )
      End
     // Sinon ajoute à partir du dataset en cours
     Else
      Begin
        if not gb_AllLoaded
        and ( gbm_DernierEnregistrement <> {$IFDEF WITH_TBOOKMARK}nil{$ELSE}''{$ENDIF} )
         Then
          try
          // Aller à l'enregistrement
            gdl_DataLink.DataSet.Bookmark := gbm_DernierEnregistrement ;
          except
          End
         Else
          fb_Sort;
        Result := fb_AddRecords ( gdl_DataLink.DataSet, False );
      End ;
  // Si l'état des enregistrements a changé
  if Result
   Then
    Begin
    // On a tout transféré
      if gb_AllSelect
      and not gb_MontreTout
       Then
        Begin
        // Si liste de transfert où il faut tout mettre
        /// Donc la liste sans les clés d'exclusion
          if not ( gb_OptionTotalList )
           Then
            Begin
              // Activation du bouton de transfert total
              if assigned ( gBt_TotalList )
              and Self.Enabled
               Then
                gBt_TotalList.Enabled := True ;
            End
           Else
        // Si liste de transfert tout a été transféré
        /// Donc la liste avec les clés d'exclusion
            Begin
              // désactivation du bouton de transfert total
              if assigned ( gBt_TotalList )
               Then
                gBt_TotalList.Enabled := False ;
            End ;
        End ;
        // Activation des boutons d'enregistrement et d'abandon
       if fb_ValideBoutons Then
         p_DesactiveGrille;
       // Fin de la mise à jour pour enregistrements transférés
    End;   
End ;

// Mise à jour automatique : ajoute des items sur scroll ou quand on l'appelle
procedure TDBGroupView.p_AddSyncronousRecords;
begin
  // propriétés obligatoires à renseigner
  if not assigned ( gdl_DataLinkOwner.DataSet )
  or gb_SelfOpen
  or ( gs_GroupKey = '' )
  or not assigned ( gdl_DataLinkOwner.DataSet.FindField ( gstl_GroupKey [ 0 ] ))
//  or not ( ds_Groupes.DataSet is TCustomADODataset )
   Then
     Exit ;

  inherited ;
  // si il y a un bouton de total
   /// alors si il y a des items
    If Items.Count > 0
      // activation du bouton de transfert
     then
       begin
         p_SetButtonsOther ( True );
       End
     // si il n'y en a pas alors pas d'activation
     else
       Begin
         p_SetButtonsOther ( False );
       End ;
end;

// Peut-on ajouter un item dans la liste ?
// adat_Dataset  : le dataset en cours
function TDBGroupView.fb_CanAdd  ( const adat_Dataset : TDataset ; const ab_AddItemPlus : Boolean )  : Boolean ;
// Item trouvé ou non
//var
//    li_Trouve : Integer ;
//var ls_Compare : String ;
 var ls_ChampTest : String ;
Begin
  // on peut ajouter par défaut
  Result := True ;
  ls_ChampTest := gstl_KeyDataSource[gstl_KeyDataSource.Count-1];


  // Si on trouve l'enregistrement de groupe dans la liste d'exclusion
  if not gb_Basket
  and ( fi_findInListVariant ( gstl_KeysListOut, adat_Dataset.FieldValues [ ls_ChampTest ], False ) <> -1 ) Then
     // on continue
    Begin
      Result := False ;
      Exit ;
    End ;
    // si on n'a pas déplacé le total sur la sélection d'origine
    // on si on ajoute des items
  if ab_AddItemPlus Then
    Begin
      if ( fi_findInListVariant ( galv_OtherList.gstl_KeysListOut, adat_Dataset.FieldValues [ ls_ChampTest ], False ) <> -1 ) Then
         // on continue
        Begin
          Result := True ;
          fb_ValideBoutons ;
          Exit ;
        End
      Else
        Begin
          Result := False ;
          Exit ;
        End ;

    End
  Else
    // Gestion Basket
    if ( gb_EstPrincipale and gb_Basket ) Then
      Begin
        // Si la clé n'a pas été ajoutée totalement
        if  ( fi_findInListVarBool ( gt_OriginKey, gvar_WorkingOriginKey, False, True, [CST_GROUPE_TRANS_SIMPLE] ) <> -1 ) Then
          Begin
            // alors certaines clés ont été ajoutées dans la liste d'exclusion de la liste principale
            if ( fi_findInListVariant        ( gstl_KeysListOut, adat_Dataset.FieldValues [ ls_ChampTest ], False ) <> -1 ) Then
              Begin
                Result := False ;
                Exit ;
              End ;
          End
        // Si la clé a été ajoutée totalement
        else
          if not gb_OptionTotalList
          and ( fi_findInListVarBool ( gt_OriginKey, gvar_WorkingOriginKey, False, True, [CST_GROUPE_TRANS_TOTAL] ) <> -1 ) Then
               // on continue
            Begin
              // alors certaines clés peuvent avoir été enlevées après l'ajout total
              Result := False ;
                  // si la clé est exclue de la liste
              if ( fi_findInListVariant        ( galv_OtherList.gstl_KeysListOut, adat_Dataset.FieldValues [ ls_ChampTest ], False ) <> -1 ) Then
                // on ne l'ajoute pas
                Begin
                  Result := True ;
                  Exit ;
                End ;
            End ;
      End ;

End ;

// Lorsqu'on déplace tout dans la liste : gestion des états
// adat_Dataset  : le dataset en cours
function TDBGroupView.fb_ChangeEtatItem ( const adat_Dataset : TDataset ; const ab_AddItemPlus : Boolean )  : Boolean ;
Begin
  // Par défaut : pas de changement d'état
  Result := False ;
  if trim ( gs_UnitsKey ) = ''
   Then
    Exit ;
  // Si gestion de transfert total
//  if gb_AllSelect
//   Then
 if not assigned ( galv_OtherList )
 or (( gws_RecordValue = '' ) and ( galv_OtherList.gws_RecordValue = '' )) Then
   Begin
    // Si est principale
    if gb_EstPrincipale
    Then
     Begin
      // Si on ne localise pas l'enregistrement en cours dans le dataset d'inclusion
      if ab_AddItemPlus Then
       Begin
        if ( gb_CaseInSensitive     and not fb_Locate (  adat_Dataset.FieldValues [ gstl_KeyDataSource [ gstl_KeyDataSource.Count - gstl_UnitsDataSource.Count ]] ))
        or ( not gb_CaseInSensitive and not gdl_DataLink.DataSet.Locate ( gstl_KeyDataSource [ gstl_KeyDataSource.Count - gstl_UnitsDataSource.Count ], adat_Dataset.FieldValues [ gstl_KeyDataSource [ gstl_KeyDataSource.Count - gstl_UnitsDataSource.Count ] ], [] ))
         Then
          Begin
           // C'est qu'il est à insérer
           gVG_ListItem.ImageIndex := gi_ImageInsere ;
           Result := True ;
          End ;
       End ;
     End
    Else
     // pour changer l'état de l'item en effacement il faut pas de Basket
    if not gb_Basket
    and assigned ( gdl_DataLink.Datasource ) then
  // Si n'est pas principale
      Begin
        // Si on ne localise pas l'enregistrement en cours dans le dataset d'exclusion
       if ab_AddItemPlus Then
        Begin
         if ( gb_CaseInSensitive     and not fb_Locate (  adat_Dataset.FieldValues [ gstl_KeyDataSource [ gstl_KeyDataSource.Count - gstl_UnitsDataSource.Count ] ] ))
         or ( not gb_CaseInSensitive and not gdl_DataLink.DataSet.Locate ( gstl_KeyDataSource [ gstl_KeyDataSource.Count - gstl_UnitsDataSource.Count ], adat_Dataset.FieldValues [ gstl_KeyDataSource [ gstl_KeyDataSource.Count - gstl_UnitsDataSource.Count ] ], [] ))
          Then
           Begin
            // C'est qu'il est à supprimer
            gVG_ListItem.ImageIndex := gi_ImageSupprime ;
            Result := True ;
           End ;
        End ;
      End ;
    End
 Else
 Begin
  if gws_RecordValue = '' Then
   Begin
    if not adat_Dataset.FieldByName ( gs_GroupField ).IsNull Then
     if gb_EstPrincipale Then
      Begin
       // C'est qu'il est à insérer
       gVG_ListItem.ImageIndex := gi_ImageInsere ;
       Result := True ;
      End
     Else
      Begin
        // C'est qu'il est à supprimer
        gVG_ListItem.ImageIndex := gi_ImageSupprime ;
        Result := True ;
      End ;
    End
  else
   Begin
    if fb_ValueToValid ( adat_Dataset.FieldByName ( gs_GroupField )) Then
     if gb_EstPrincipale Then
      Begin
       // C'est qu'il est à insérer
       gVG_ListItem.ImageIndex := gi_ImageInsere ;
       Result := True ;
      End
     Else
      Begin
      // C'est qu'il est à supprimer
       gVG_ListItem.ImageIndex := gi_ImageSupprime ;
       Result := True ;
      End ;
    End ;
  End ;
End ;

//////////////////////////////////////////////////////////////////////////////////
// Fonction : fb_IsChangeValue
// Description : La valeur en cours est-elle changée par rapport à la liste
//////////////////////////////////////////////////////////////////////////////////
function TDBGroupView.fb_ValueToValid ( const afie_ChampTest : TField )  : Boolean ;
Begin
  if  ( afie_ChampTest.DataType = ftBoolean ) Then
    Result := ( afie_ChampTest.AsBoolean <> StrToBool ( gws_RecordValue ))
  Else
    if  VarIsNumeric ( afie_ChampTest.AsVariant ) Then
      Result := ( afie_ChampTest.AsFloat <> StrToFloat ( gws_RecordValue ))
    Else
      Result := ( afie_ChampTest.AsString  <> gws_RecordValue )
End ;

// Insère un enregistrement dans la liste : surchargée
// adat_Dataset  : le dataset en cours
// Résultat      : A-t-on changé l'état
function TDBGroupView.fb_AddRecords ( const adat_Dataset : TDataset ; const ab_InsereCles : Boolean ) : Boolean;
var lb_CaseInsensitive : Boolean ;
begin
   // Par défaut : pas d'item changé
  Result := False ;
  // PAs de liste associée : on quitte
  if not assigned ( galv_OtherList )
   Then
    Exit ;
  lb_CaseInsensitive := gb_CaseInSensitive and assigned ( gdl_DataLink.DataSet ) and ( ab_InsereCles or gb_AllSelect ) and ( not assigned ( galv_OtherList ) or ( gws_RecordValue = '' ) or ( galv_OtherList.gws_RecordValue = '' ));
  if lb_CaseInSensitive Then
    p_LocateInit;
   // héritage
  try
    Result := inherited;
  finally
    if lb_CaseInSensitive Then
      p_LocateRestore;
  End ;
End ;
{
function TDBGroupView.fds_GetDatasourceGroupe: TdataSource;
begin
  Result := gdl_DataLink.Datasource ;
end;
}
// récupère le datasource des groupes
function TDBGroupView.fds_GetDatasourceOwner: TdataSource;
begin
  Result := nil ;
  if assigned ( gdl_DataLinkOwner )
   then
    Result := gdl_DataLinkOwner.DataSource ;
end;

/// Démarre l'ouverture du group view
// Retourne le fait de quitter la procédure principale
function TDBGroupView.fb_BeginOpen:Boolean;
var ls_SQLCommands ,
    ls_Fields     : String;
begin
 Result := True;
 if gs_TableOwner <> '' Then
 with  ds_DatasourceQuery2.Dataset do
   Begin
     try
       Close ;
       ls_Fields := CST_SQL_ALL;
       if ((gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ) is TStringField )
       or ( gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ) is TMemoField )
       or ( gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ).DataType IN CST_DELPHI_FIELD_STRING )) Then
         begin
           ls_SQLCommands := CST_SQL_SELECT+ls_Fields+CST_SQL_FROM + gs_TableOwner + CST_SQL_WHERE + gs_GroupKey + '=''' + fs_stringDbQuote ( gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ).AsString ) + '''' ;
         End
       Else
         ls_SQLCommands := CST_SQL_SELECT+ls_Fields+CST_SQL_FROM + gs_TableOwner + CST_SQL_WHERE + gs_GroupKey + '='   +                   gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ).AsString ;
       if Assigned ( gstl_SQLQuery2 ) then
         Begin
           gstl_SQLQuery2.Text := ls_SQLCommands ;
           {$IFDEF DELPHI_9_UP}
         End
        else
         Begin
           gwst_SQLQuery.Text := ls_SQLCommands ;
           {$ENDIF}
         End;
     finally
     End ;
     try
       Open ;
       if IsEmpty Then
       Begin
         if MessageDlg ( GS_METTRE_A_JOUR_FICHE, mtWarning, [mbYes,mbNo], 0) = mrYes Then
            fb_RefreshDataset ( gdl_DataLinkOwner.DataSet, True );
         Result := False;
       End ;
     except
       ShowMessage ( GS_PAS_GROUPES + ' ( ' + gs_TableOwner + ', ' + gs_GroupKey + ' )'  );
       Result := False;
     End ;
    End;
end;

// Evènement onclick du bouton vide Basket
// Sender : obligatoire
procedure TDBGroupView.p_VideBasket ( Sender : TObject );
//var //ls_Compare : String ;
//    li_i       : Integer ;
Begin
  if assigned ( ge_BasketClick ) Then
    ge_BasketClick ( Sender );
  if gb_Basket
   Then
    Begin
      Finalize ( gstl_KeysListOut );
      SetLength(gstl_KeysListOut,gstl_UnitsDatasource.count);
      Finalize ( gt_KeyOwners     );
      Finalize ( gt_OriginKey    );
//      gb_AllSelect := False ;
      p_Reinitialise ;
      if Assigned ( galv_OtherList ) Then
        Begin
          Finalize ( galv_OtherList.gt_OriginKey    );
          Finalize ( galv_OtherList.gstl_KeysListOut );
          Finalize ( galv_OtherList.gt_KeyOwners );
          galv_OtherList.Refresh ;
        End ;
      p_AddRecords ;
    End ;
  if assigned ( gBt_Basket )
   Then
    gBt_Basket.Enabled := False ;
End ;
// function TDBGroupView.fb_ExecuteQueryLinkedAllSelectTotal
// execute the query in N-N relationship when different group and owner table

function TDBGroupView.fb_ExecuteQueryNotLinkedNNGroupSourceDifferent : Boolean;
var ls_TexteSQL, ls_SQLCommands : String;
    li_i : Integer ;
 Begin
  Result := False;
  ls_TexteSQL := '';
   if ((gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ) is TStringField )
   or (gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ) is TBlobField )
   or ( gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ).DataType IN CST_DELPHI_FIELD_STRING ))
      Then AppendStr ( ls_SQLCommands, '(''' + gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).AsString + ''',' )
      Else AppendStr ( ls_SQLCommands, '('   + gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).AsString +   ',' );
  // Il faut ajouter les champs avec l'état ajoute si on est sur la liste en dehors du tout
  if( not gb_AllSelect or not gb_OptionTotalList or gb_MontreTout )
  and  assigned ( galv_OtherList )
  and fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0, ls_TexteSQL, galv_OtherList.gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[gstl_KeyDataSource [ gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ]]) Then
   with  gdat_DataSetQuery do
     Begin
      ls_SQLCommands := 'INSERT INTO ' + gs_GroupTable + ' ( ' + gs_GroupField + fs_GetFieldsInsert ;
      for li_i := 0 to gstl_UnitsDatasource.Count - 1 do
        AppendStr ( ls_SQLCommands, ', ' + gstl_UnitsDatasource [ li_i ] );
      AppendStr ( ls_SQLCommands, ' ) '+#10+' SELECT ') ;
        // lien groupe
      if ((gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ) is TStringField )
      or (gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ) is TBlobField )
      or ( gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ).DataType IN CST_DELPHI_FIELD_STRING ))
         Then AppendStr ( ls_SQLCommands, ' ''' + gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).AsString + '''' + fs_GetFieldsValuesInsert + ',' )
         Else AppendStr ( ls_SQLCommands, ' '   + gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).AsString +        fs_GetFieldsValuesInsert + ',' );
        // lien unité
        AppendStr ( ls_SQLCommands,  gstl_KeyDataSource [ gstl_KeyDataSource.Count - gstl_UnitsDataSource.Count ] + CST_SQL_FROM + gs_TableSource + #10+CST_SQL_WHERE + '('
                                  + gstl_KeyDataSource [ gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ] + ' IN ( ' + ls_TexteSQL + ')');
        for li_i := 1 to gstl_UnitsDatasource.Count - 1 do
          if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0, ls_TexteSQL, galv_OtherList.gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[gstl_KeyDataSource [ li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ]]) Then
            AppendStr ( ls_SQLCommands, 'AND (' + gstl_KeyDataSource [ li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ] + ' IN ( ' + ls_TexteSQL + ')' );
        AppendStr( ls_SQLCommands, ')'+#13#10);
         // Il se peut que quelqu'un ait effacé une unité
    	AppendStr( ls_SQLCommands,  #10+CST_SQL_AND + 'NOT EXISTS ( SELECT '  + gs_UnitsField +  CST_SQL_FROM + gs_GroupTable + CST_SQL_WHERE + gs_GroupTable + '.' + gs_UnitsField + '=' + gs_TableSource + '.' + gstl_KeyDataSource [ gstl_KeyDataSource.Count - gstl_UnitsDataSource.Count ]
      + CST_SQL_AND + gs_GroupField + '=');
      if ((gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ) is TStringField )
      or (gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ) is TBlobField )
      or ( gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ).DataType IN CST_DELPHI_FIELD_STRING ))
         Then AppendStr ( ls_SQLCommands,  ' ''' + gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).AsString + ''')' )
         Else AppendStr ( ls_SQLCommands,  ' '   + gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).AsString +   ')' );
      p_ExecuteSQLQuery ( gdat_DataSetQuery, ls_SQLCommands );
      Result := True;
    End ;
  // Suppression Autre liste non principale
  if  assigned ( galv_OtherList )
  and not      ( galv_OtherList.DataListPrimary )
  and assigned ( galv_OtherList.Datasource )
  and assigned ( galv_OtherList.Datasource.DataSet )
  // Il faut supprimer les champs avec l'état supprime si on est sur la liste en dehors du tout
  and ( not gb_AllSelect or not galv_OtherList.gb_OptionTotalList or gb_MontreTout )
  and fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0,
                          ls_TexteSQL, gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[
                          gstl_KeyDataSource [ gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ]]) Then
    with gdat_DataSetQuery do
      Begin
        ls_SQLCommands := CST_SQL_DELETE + CST_SQL_FROM + gs_GroupTable + CST_SQL_WHERE + gs_GroupField + ' =  ' ;
        // lien groupe
        if ((gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ) is TStringField )
        or (gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ) is TBlobField )
        or ( gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ).DataType IN CST_DELPHI_FIELD_STRING ))
         Then AppendStr ( ls_SQLCommands,  ' ''' + gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).AsString + '''' )
         Else AppendStr ( ls_SQLCommands,  ' '   + gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).AsString +   '' );
         AppendStr ( ls_SQLCommands,  CST_SQL_AND + '(' + gstl_UnitsDatasource [ 0 ] + ' IN ( ' + ls_TexteSQL + ')');
         for li_i := 1 to gstl_UnitsDatasource.Count - 1 do
           if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0, ls_TexteSQL, gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[gstl_KeyDataSource [ li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ]]) Then
             AppendStr ( ls_SQLCommands, 'AND (' + gstl_UnitsDatasource [ li_i ] + ' IN ( ' + ls_TexteSQL + ')' );
         AppendStr ( ls_SQLCommands, ')'+#13#10);
          // lien groupe
        p_ExecuteSQLQuery ( gdat_DataSetQuery, ls_SQLCommands );
        Result := True;
      End ;
End;

// function TDBGroupView.fb_ExecuteQueryLinkedAllSelectTotal
// execute the query in 1-N relationship
function TDBGroupView.fb_ExecuteQuery1N : Boolean;
var ls_TexteSQL, ls_SQLCommands : String;
Begin
  ls_TexteSQL := '';
  with gdl_DataLinkOwner.DataSet do
   Begin
     Result := False;
     // Mise à jour en une seule : la requête se met en place avant les boucles
     if  assigned ( FindField ( gs_GroupField ))
      Then ls_SQLCommands :=  CST_SQL_UPDATE + gs_TableOwner  + ' SET '
      Else ls_SQLCommands :=  CST_SQL_UPDATE + gs_GroupTable + ' SET ' ;
     // affecter le lien groupe ( affectation ou groupe )
     if ((FindField ( gs_GroupKey ) is TStringField )
     or (FindField ( gs_GroupKey ) is TBlobField )
     or ( FindField ( gs_GroupKey ).DataType IN CST_DELPHI_FIELD_STRING ))
      Then AppendStr ( ls_SQLCommands,  gs_GroupField + '=''' + VarToStr ( gvar_KeyDestination ) + '''' )
      Else AppendStr ( ls_SQLCommands,  gs_GroupField + '='   + VarToStr ( gvar_KeyDestination )        );
     if fb_BuildWhereBasket ( Self, ls_TexteSQL, assigned ( galv_OtherList.gdl_DataLinkOwner.DataSet ), False ) Then
       Begin
         AppendStr ( ls_SQLCommands,  ls_TexteSQL );
         p_ExecuteSQLQuery ( gdat_DataSetQuery, ls_SQLCommands );
         Result := True;
       End ;
   end;
End;

// function TDBGroupView.fb_ExecuteQueryLinkedAllSelectTotal
// execute the query in N-N relationship when similar group and owner table
function TDBGroupView.fb_ExecuteQueryNotLinkedNNGroupSourceSimilar : Boolean;
var ls_TexteSQL, ls_SQLCommands : String;
    li_i : Integer;
Begin
  Result := False;
  ls_TexteSQL := '';
  // Il faut ajouter les champs avec l'état ajoute si on est sur la liste en dehors du tout
  if( not gb_AllSelect or not gb_OptionTotalList or gb_MontreTout )
  and  assigned ( galv_OtherList )
  and fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0,
                          ls_TexteSQL, galv_OtherList.gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[
                          gstl_KeyDataSource [ gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ]]) Then
   with gdl_DataLinkOwner.DataSet do
    Begin
      // Propriété DatasourceGroupTable : gs_GroupTable
      ls_SQLCommands := CST_SQL_UPDATE + gs_GroupTable + ' SET ' ;
      // lien groupe
      if ((FindField ( gs_GroupKey ) is TStringField )
      or (FindField ( gs_GroupKey ) is TBlobField )
      or ( FindField ( gs_GroupKey ).DataType IN CST_DELPHI_FIELD_STRING ))
       Then AppendStr ( ls_SQLCommands,  gs_GroupField + '=''' + FieldByName ( gs_GroupKey  ).AsString + '''' )
       Else AppendStr ( ls_SQLCommands,  gs_GroupField + '='   + FieldByName ( gs_GroupKey  ).AsString        );
      // lien unité
      AppendStr ( ls_SQLCommands,  CST_SQL_WHERE + '((' + gstl_UnitsDatasource [ 0 ] + ' IN (' + ls_TexteSQL + ')' );
      for li_i := 1 to gstl_UnitsDatasource.Count - 1 do
        if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0,
                               ls_TexteSQL, galv_OtherList.gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[
                               gstl_KeyDataSource [ li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ]]) Then
          AppendStr ( ls_SQLCommands, 'AND (' + gstl_UnitsDatasource [ li_i ] + ' IN ( ' + ls_TexteSQL + ')' );
      AppendStr ( ls_SQLCommands, ')'+#13#10);
      p_ExecuteSQLQuery ( gdat_DataSetQuery, ls_SQLCommands );
      Result := True;
    end;

  // Suppression Autre liste non principale
  if  assigned ( galv_OtherList )
  and not      ( galv_OtherList.DataListPrimary )
  and assigned ( galv_OtherList.Datasource )
  and assigned ( galv_OtherList.Datasource.DataSet )
  // Il faut supprimer les champs avec l'état supprime si on est sur la liste en dehors du tout
  and ( not gb_AllSelect or not galv_OtherList.gb_OptionTotalList or gb_MontreTout )
  and fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0,
                          ls_TexteSQL, gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[
                          gstl_KeyDataSource [ gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ]]) Then
    with gdat_DataSetQuery do
      begin
        // Propriété DatasourceGroupTable : gs_GroupTable
        ls_SQLCommands :=  CST_SQL_UPDATE + gs_GroupTable + ' SET ' ;
        // lien groupe à null
        AppendStr ( ls_SQLCommands,  gs_GroupField + ' =' + CST_SQL_NULL  );
        // lien unité
        AppendStr ( ls_SQLCommands,  CST_SQL_WHERE + '((' + gstl_UnitsDatasource [ 0 ] + ' IN (' + ls_TexteSQL + ')' );
        for li_i := 1 to gstl_UnitsDatasource.Count - 1 do
          if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0,
                                 ls_TexteSQL, gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[
                                 gstl_UnitsDatasource [ li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count]]) Then
            AppendStr ( ls_SQLCommands, 'AND (' + gstl_UnitsDatasource [ li_i ] + ' IN ( ' + ls_TexteSQL + ')' );
        AppendStr ( ls_SQLCommands, ')'+#13#10);
        p_ExecuteSQLQuery ( gdat_DataSetQuery, ls_SQLCommands );
        Result := True;
      end;
End ;

// function TDBGroupView.fb_ExecuteQueryLinkedAllSelectTotal
// execute the query in N-N relationship when different group and owner table and  when is total
function TDBGroupView.fb_ExecuteQueryNotLinkedNNGroupSourceDifferentTotal: boolean;
var ls_TexteSQL, ls_SQLCommands : String;
    li_i, li_j : Integer;
Begin
  Result := False ;
  ls_TexteSQL := '';
  with gdl_DataLinkOwner.DataSet do
    Begin
      // Propriété DatasourceGroupTable : gs_GroupTable
      ls_SQLCommands :=  'INSERT INTO ' + gs_GroupTable + ' ( ' + gs_GroupField + fs_GetFieldsInsert +', ' + gs_UnitsField + ' ) ' + ' select ' ;
      // lien groupe à insérer globalement
      if ((FindField ( gs_GroupKey ) is TStringField )
      or (FindField ( gs_GroupKey ) is TBlobField )
      or ( FindField ( gs_GroupKey ).DataType IN CST_DELPHI_FIELD_STRING ))
       Then AppendStr ( ls_SQLCommands,  '''' + fs_StringDbQuote ( FieldByName ( gs_GroupKey  ).AsString  ) + '''' + fs_GetFieldsValuesInsert )
       Else AppendStr ( ls_SQLCommands,                             FieldByName ( gs_GroupKey  ).AsString          + fs_GetFieldsValuesInsert );
              // lien des unités à ajouter globalement
      AppendStr ( ls_SQLCommands,  ', ' + gstl_KeyDataSource [ gstl_KeyDataSource.Count - gstl_UnitsDataSource.Count ] + CST_SQL_FROM + gs_TableSource + #13#10
                    // Champ unité déjà dans le grouê
                     + CST_SQL_WHERE );
      for li_i := 0 to gstl_UnitsDatasource.Count - 1 do
        Begin
         if li_i > 0 Then
           AppendStr(ls_SQLCommands, CST_SQL_AND);
         AppendStr(ls_SQLCommands, '(' + gstl_KeyDataSource [ li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ]  +' NOT IN ( SELECT ' + gs_GroupTable + '.' + gstl_UnitsDatasource [ li_i ] + CST_SQL_FROM + gs_TableSource + ',' + gs_GroupTable   );
         AppendStr(ls_SQLCommands, CST_SQL_WHERE + '(' );
        for li_j := 0 to gstl_UnitsDatasource.Count - 1 do
         Begin
          if li_j > 0 Then
            AppendStr(ls_SQLCommands, CST_SQL_AND);
          AppendStr(ls_SQLCommands, '(' + gs_GroupTable + '.' + gstl_UnitsDatasource [ li_j ] + '=' + gs_TableSource + '.' + gstl_KeyDataSource [ li_j + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ] +')');
         end;
         if ((FindField ( gs_GroupKey ) is TStringField )
         or (FindField ( gs_GroupKey ) is TBlobField )
         or ( FindField ( gs_GroupKey ).DataType IN CST_DELPHI_FIELD_STRING ))
                 // lien groupe déjà dans le groupe
          then AppendStr ( ls_SQLCommands,  CST_SQL_AND + gs_GroupTable + '.' + gs_GroupField + ' = ''' + fs_stringDbQuote ( FindField ( gs_GroupKey ).AsString ) + '''' )
          Else AppendStr ( ls_SQLCommands,  CST_SQL_AND + gs_GroupTable + '.' + gs_GroupField + ' = '   +                    FindField ( gs_GroupKey ).AsString    );
        end;
       AppendStr(ls_SQLCommands, ')))' + #13#10);
       // et liste d'exclusion car il y a peut-être eu un transfert dans l'autre sens
      if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0,
                             ls_TexteSQL, gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[gstl_KeyDataSource[
                             gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count]])
       Then
        Begin
         AppendStr ( ls_SQLCommands,  CST_SQL_AND + '(' + gs_TableSource + '.' + gstl_KeyDataSource [  gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ] + ' NOT IN (' + ls_TexteSQL + ')' );
         for li_i := 1 to gstl_UnitsDatasource.Count - 1 do
           if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0,
                                  ls_TexteSQL, gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[
                                  gstl_KeyDataSource [ li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count]]) Then
             AppendStr ( ls_SQLCommands, 'AND (' + gs_TableSource + '.' + gstl_KeyDataSource [ li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ] + ' NOT IN ( ' + ls_TexteSQL + ')' );
         AppendStr ( ls_SQLCommands, ')'+#13#10);
        end;
      // exécution
      AppendStr ( ls_SQLCommands,  fws_GetExistingFilter );
      p_ExecuteSQLQuery ( gdat_DataSetQuery, ls_SQLCommands );
      Result := True ;

    end;
end;

// function TDBGroupView.fb_ExecuteQueryLinkedAllSelectTotal
// execute the query when shows all
function TDBGroupView.fb_ExecuteQueryShowAllGroup(
  const astl_KeysListOut: tt_TableauxStrings): boolean;
var ls_TexteSQL, ls_SQLCommands : String;
    li_i : Integer;
Begin
  Result := False;
  ls_TexteSQL := '';
  if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0, ls_TexteSQL, astl_KeysListOut, gdl_DataLink.DataSet.FieldValues[gstl_KeyDataSource[gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count]]) Then
   with gdat_DataSetQuery do
    begin
      // Propriété DatasourceGroupTable : gs_GroupTable
      ls_SQLCommands := CST_SQL_UPDATE + gs_GroupTable + ' SET ' ;
      // valeur à affecter uniquement
      if gws_RecordValue = ''
       Then
      // lien groupe à null
        AppendStr ( ls_SQLCommands,  gs_GroupField + '=NULL' )
       Else
        if ((gdl_DataLink.DataSet.FindField ( gs_GroupField ) is TStringField )
        or (gdl_DataLink.DataSet.FindField ( gs_GroupField ) is TMemoField )
        or ( gdl_DataLink.DataSet.FindField ( gs_GroupField ).DataType IN CST_DELPHI_FIELD_STRING ))
         Then AppendStr ( ls_SQLCommands,  gs_GroupField + '=''' + gws_RecordValue + '''' )
         Else AppendStr ( ls_SQLCommands,  gs_GroupField + '='   + gws_RecordValue        );
      // lien unité
      AppendStr ( ls_SQLCommands,  CST_SQL_WHERE + '(' + gstl_UnitsDatasource [ 0 ] + ' IN (' + ls_TexteSQL + ')' );
      for li_i := 1 to gstl_UnitsDatasource.Count - 1 do
        if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0, ls_TexteSQL, astl_KeysListOut, gdl_DataLink.DataSet.FieldValues[gstl_KeyDataSource [ li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ]]) Then
          AppendStr ( ls_SQLCommands, CST_SQL_AND + '(' + gstl_UnitsDatasource [ li_i ] + ' IN ( ' + ls_TexteSQL + ')' );
      AppendStr ( ls_SQLCommands, ')'+#13#10);
      p_ExecuteSQLQuery ( ds_DataSourceQuery2.DataSet, ls_SQLCommands );
      Result := True ;
    end;
end;

// function TDBGroupView.fb_ExecuteQueryLinkedAllSelectTotal
// execute the query in N-N relationship when different group and owner table and  when is total out
function TDBGroupView.fb_ExecuteQueryNotLinkedNNGroupSourceDifferentTotalOut: boolean;
var ls_TexteSQL, ls_SQLCommands : String;
    li_i : Integer;
Begin
  Result:=False;
  ls_TexteSQL := '';
  if  assigned ( galv_OtherList )
  and not      ( galv_OtherList.DataListPrimary )
  and          ( galv_OtherList.gb_OptionTotalList    )
   Then
    with gdl_DataLinkOwner.DataSet do
     Begin
      // Propriété DatasourceGroupTable : gs_GroupTable
      ls_SQLCommands := CST_SQL_DELETE + CST_SQL_FROM + gs_GroupTable + CST_SQL_WHERE ;
      // lien groupe à insérer globalement
      if ((FindField ( gs_GroupKey ) is TStringField )
      or (FindField ( gs_GroupKey ) is TBlobField )
      or ( FindField ( gs_GroupKey ).DataType IN CST_DELPHI_FIELD_STRING ))
       Then AppendStr ( ls_SQLCommands,  gs_GroupField + ' =  ''' + fs_stringDbQuote ( FieldByName ( gs_GroupKey ).AsString ) + '''' )
       Else AppendStr ( ls_SQLCommands,  gs_GroupField + ' =  '   + FieldByName ( gs_GroupKey ).AsString );
        ///  sauf la liste d'excusion car il y a peut-être eu un transfert vars l'autre liste
        if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0,
                               ls_TexteSQL, galv_OtherList.gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[
                               gstl_KeyDataSource [ gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ]])
         Then
          Begin
            AppendStr ( ls_SQLCommands,  'and (' + gstl_UnitsDatasource [ 0 ] + ' NOT IN (' +  ls_TexteSQL + ')' );
            for li_i := 1 to gstl_UnitsDatasource.Count - 1 do
              if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0,
                                     ls_TexteSQL, galv_OtherList.gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[
                                     gstl_KeyDataSource [ li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ]]) Then
                AppendStr ( ls_SQLCommands, 'AND (' + gstl_UnitsDatasource [ li_i ] + ' NOT IN ( ' + ls_TexteSQL + ')' );
            AppendStr ( ls_SQLCommands, ')'+#13#10);
           end;
      //                  Showmessage ( CommandText );
      AppendStr ( ls_SQLCommands,  fws_GetExistingFilter );
      p_ExecuteSQLQuery ( gdat_DataSetQuery, ls_SQLCommands );
      Result := True ;
     End ;
End;

// function TDBGroupView.fb_ExecuteQueryLinkedAllSelectTotal
// execute the query in N-N relationship when similar group and owner table and  when is total
function TDBGroupView.fb_ExecuteQueryNotLinkedNNGroupSourceSimilarTotal: boolean;
var ls_TexteSQL, ls_SQLCommands : String;
    li_i : Integer;
Begin
  Result := False;
  ls_TexteSQL := '';
  with gdl_DataLinkOwner.DataSet do
    Begin
      // Propriété DatasourceGroupTable : gs_GroupTable
      ls_SQLCommands := CST_SQL_UPDATE + gs_GroupTable + ' SET ' ;
      // lien groupe à insérer globalement
      if ((FindField ( gs_GroupKey ) is TStringField )
      or (FindField ( gs_GroupKey ) is TBlobField )
      or ( FindField ( gs_GroupKey ).DataType IN CST_DELPHI_FIELD_STRING ))
       Then AppendStr ( ls_SQLCommands,  gs_GroupField + '=''' + fs_stringDbQuote ( FieldByName ( gs_GroupKey  ).AsString ) + '''' )
       Else AppendStr ( ls_SQLCommands,  gs_GroupField + '='   + FieldByName ( gs_GroupKey  ).AsString );
      // Le groupe doit être null
      AppendStr ( ls_SQLCommands,  CST_SQL_WHERE + gs_GroupField + CST_SQL_IS + CST_SQL_NULL );
      // A garder
               // L'unité doit être hors de liste d'exclusion car transfert possible dans l'autre liste
      if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0,
                             ls_TexteSQL, gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[
                             gstl_KeyDataSource [ gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ]])
       Then
        Begin
          AppendStr ( ls_SQLCommands,  CST_SQL_AND + '(' + gstl_UnitsDatasource [ 0 ] + ' NOT IN (' + ls_TexteSQL + ')' );
          for li_i := 1 to gstl_UnitsDatasource.Count - 1 do
            if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, li_i +gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0,
                                   ls_TexteSQL, gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[
                                   gstl_KeyDataSource [ li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count]]) Then
              AppendStr ( ls_SQLCommands, 'AND (' + gstl_UnitsDatasource [ li_i ] + ' NOT IN ( ' + ls_TexteSQL + ')' );
          AppendStr ( ls_SQLCommands, ')'+#13#10);
        end;
      //                  Showmessage ( CommandText );
      AppendStr ( ls_SQLCommands,  fws_GetExistingFilter );
      p_ExecuteSQLQuery ( gdat_DataSetQuery, ls_SQLCommands );
      Result := True ;
    End
end;
// function TDBGroupView.fb_ExecuteQueryLinkedAllSelectTotal
// execute the query in N-N relationship when similar group and owner table and  when is total out
function TDBGroupView.fb_ExecuteQueryNotLinkedNNGroupSourceSimilarTotalOut: boolean;
var ls_TexteSQL, ls_SQLCommands : String;
    li_i : Integer;
Begin
  Result := False;
  ls_TexteSQL := '';
  with gdl_DataLinkOwner.DataSet do
    Begin
    // Propriété DatasourceGroupTable : gs_GroupTable
      ls_SQLCommands := CST_SQL_UPDATE + gs_GroupTable + ' SET ' ;
      AppendStr ( ls_SQLCommands,  gs_GroupField + ' =' + CST_SQL_NULL  );
      // lien groupe à supprimer
      if ((FindField ( gs_GroupKey ) is TStringField )
      or (FindField ( gs_GroupKey ) is TBlobField )
      or ( FindField ( gs_GroupKey ).DataType IN CST_DELPHI_FIELD_STRING ))
       Then AppendStr ( ls_SQLCommands,  CST_SQL_WHERE + gs_GroupField + '=''' + fs_stringDbQuote ( FieldByName ( gs_GroupKey  ).AsString ) + '''' )
       Else AppendStr ( ls_SQLCommands,  CST_SQL_WHERE + gs_GroupField + '='   + FieldByName ( gs_GroupKey  ).AsString );
      // L'unité doit être hors de liste d'exclusion car transfert possible dans l'autre liste
      if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0, ls_TexteSQL,
                             galv_OtherList.gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[gstl_UnitsDatasource [ 0 ]])
       Then
        Begin
          AppendStr ( ls_SQLCommands,  CST_SQL_AND + gstl_UnitsDatasource [ 0 ] + ' NOT IN (' + ls_TexteSQL + ')' );
          for li_i := 1 to gstl_UnitsDatasource.Count - 1 do
            if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0, ls_TexteSQL,
                                   galv_OtherList.gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[gstl_UnitsDatasource [ li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ]]) Then
              AppendStr ( ls_SQLCommands, 'AND (' + gstl_UnitsDatasource [ li_i ] + ' NOT IN ( ' + ls_TexteSQL + ')' );
          AppendStr ( ls_SQLCommands, ')'+#13#10);
        end;
      // exécution
      AppendStr ( ls_SQLCommands,  fws_GetExistingFilter );
      p_ExecuteSQLQuery ( gdat_DataSetQuery, ls_SQLCommands );
      Result := True ;
    end;
End;

// function TDBGroupView.fb_ExecuteQueryLinkedAllSelectTotal
// execute the query when linked to other group view and all selected and is total
function TDBGroupView.fb_ExecuteQueryLinkedAllSelectTotal : boolean;
var ls_TexteSQL, ls_SQLCommands : String;
    li_i : Integer;
Begin
  Result := False;
  ls_TexteSQL := '';
  with gdat_DataSetQuery do
   begin
    // Propriété DatasourceGroupTable : gs_GroupTable
    ls_SQLCommands := CST_SQL_UPDATE + gs_GroupTable + ' SET ' ;
    // lien groupe à modifier globalement
    if gws_RecordValue = ''
     Then
      // lien groupe à null
      AppendStr ( ls_SQLCommands,  gs_GroupField + '=NULL' )
     Else
      if ((gdl_DataLink.DataSet.FindField ( gs_GroupField ) is TStringField )
      or (gdl_DataLink.DataSet.FindField ( gs_GroupField ) is TMemoField )
      or ( gdl_DataLink.DataSet.FindField ( gs_GroupField ).DataType IN CST_DELPHI_FIELD_STRING ))
       Then AppendStr ( ls_SQLCommands,  gs_GroupField + '=''' + gws_RecordValue + '''' )
       Else AppendStr ( ls_SQLCommands,  gs_GroupField + '='   + gws_RecordValue        );
    // Le groupe doit être null
    if galv_OtherList.gws_RecordValue = ''
     Then
    // lien groupe à null
      AppendStr ( ls_SQLCommands,  CST_SQL_WHERE + gs_GroupField + CST_SQL_IS + CST_SQL_NULL )
     Else
      if ((gdl_DataLink.DataSet.FindField ( gs_GroupField ) is TStringField )
      or (gdl_DataLink.DataSet.FindField ( gs_GroupField ) is TMemoField )
      or ( gdl_DataLink.DataSet.FindField ( gs_GroupField ).DataType IN CST_DELPHI_FIELD_STRING ))
       Then AppendStr ( ls_SQLCommands,  CST_SQL_WHERE + gs_GroupField + '=''' + galv_OtherList.gws_RecordValue + '''' )
       Else AppendStr ( ls_SQLCommands,  CST_SQL_WHERE + gs_GroupField + '='   + galv_OtherList.gws_RecordValue        );
     // L'unité doit être hors de liste d'exclusion car transfert possible dans l'autre liste
    if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0, ls_TexteSQL,
                           gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[gstl_UnitsDatasource [ 0 ]])
     Then
      Begin
        AppendStr ( ls_SQLCommands, CST_SQL_AND + '(' + gstl_UnitsDatasource [ 0 ] + ' NOT IN (' + ls_TexteSQL + ')' );
        for li_i := 1 to gstl_UnitsDatasource.Count - 1 do
            if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0, ls_TexteSQL,
                                   gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[gstl_UnitsDatasource [ li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ]]) Then
              AppendStr ( ls_SQLCommands, 'AND (' + gstl_UnitsDatasource [ li_i ] + ' NOT IN ( ' + ls_TexteSQL + ')' );
          AppendStr ( ls_SQLCommands, ')'+#13#10);
      end;
    //                  Showmessage ( CommandText );
    AppendStr ( ls_SQLCommands,  fws_GetExistingFilter );
    p_ExecuteSQLQuery ( gdat_DataSetQuery, ls_SQLCommands );
    Result := True ;
  End;
end;
// function TDBGroupView.fb_ExecuteQueryLinkedAllSelect
// execute the query when linked to other group view and all selected
function TDBGroupView.fb_ExecuteQueryLinkedAllSelect : boolean;
var ls_TexteSQL, ls_SQLCommands : String;
    li_i : Integer;
Begin
  Result := False;
  ls_TexteSQL := '';
  with gdat_DataSetQuery do
   begin
    // Propriété DatasourceGroupTable : gs_GroupTable
    ls_SQLCommands := CST_SQL_UPDATE + gs_GroupTable + ' SET ' ;
      // lien groupe à modifier globalement
    if galv_OtherList.gws_RecordValue = ''
     Then
    // lien groupe à null
      AppendStr ( ls_SQLCommands,  gs_GroupField + '=NULL' )
     Else
      if ((gdl_DataLink.DataSet.FindField ( gs_GroupField ) is TStringField )
      or (gdl_DataLink.DataSet.FindField ( gs_GroupField ) is TMemoField )
      or ( gdl_DataLink.DataSet.FindField ( gs_GroupField ).DataType IN CST_DELPHI_FIELD_STRING ))
       Then AppendStr ( ls_SQLCommands,  gs_GroupField + '=''' + galv_OtherList.gws_RecordValue + '''' )
       Else AppendStr ( ls_SQLCommands,  gs_GroupField + '='   + galv_OtherList.gws_RecordValue        );
      // lien groupe des listes à modifier uniquement
    if gws_RecordValue = '' Then
     // lien groupe à null
     AppendStr ( ls_SQLCommands,  CST_SQL_WHERE + gs_GroupField + CST_SQL_IS + CST_SQL_NULL)
    Else
    if ((gdl_DataLink.DataSet.FindField ( gs_GroupField ) is TStringField )
    or (gdl_DataLink.DataSet.FindField ( gs_GroupField ) is TMemoField )
    or ( gdl_DataLink.DataSet.FindField ( gs_GroupField ).DataType IN CST_DELPHI_FIELD_STRING ))
     Then AppendStr ( ls_SQLCommands,  CST_SQL_WHERE + gs_GroupField + '=''' + gws_RecordValue + '''' )
     Else AppendStr ( ls_SQLCommands,  CST_SQL_WHERE + gs_GroupField + '='   + gws_RecordValue        );
    if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0, ls_TexteSQL,
                           galv_OtherList.gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[gstl_KeyDataSource [ gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ]])
    Then
     Begin
       AppendStr ( ls_SQLCommands,  CST_SQL_AND + gstl_UnitsDatasource [ 0 ] + ' NOT IN (' + ls_TexteSQL + ')' );
       for li_i := 1 to gstl_UnitsDatasource.Count - 1 do
            if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0, ls_TexteSQL,
                                   galv_OtherList.gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[gstl_KeyDataSource [ li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ]]) Then
              AppendStr ( ls_SQLCommands, 'AND (' + gstl_UnitsDatasource [ li_i ] + ' NOT IN ( ' + ls_TexteSQL + ')' );
       AppendStr ( ls_SQLCommands, ')'+#13#10);
     end;
    // exécution
     AppendStr ( ls_SQLCommands,  fws_GetExistingFilter );
    p_ExecuteSQLQuery ( gdat_DataSetQuery, ls_SQLCommands );
    Result := True ;
   end;
End ;
// Evènement enregistre du bouton enregistrer
// Sender : obligatoire
procedure TDBGroupView.p_Record ( Sender : TObject );
var lb_Executee    : Boolean ;
    lda_Action     : TDataAction ;

Begin
  // ancien évènement
  if assigned ( ge_RecordClick )
   Then
    ge_RecordClick ( Sender );
   lb_Executee := False ;
/// Variables obligatoires
  if not assigned ( gdl_DataLink.DataSet )
  or (gs_UnitsKey = '')
  or not assigned ( gdl_DataLink.DataSet.FindField ( gstl_KeyDataSource [ gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ] ))
  or not assigned ( ds_DatasourceQuery )
  or not assigned ( gdat_DataSetQuery )
  or ( not assigned ( gstl_SQLQuery)
 {$IFDEF DELPHI_9_UP}
  and not assigned ( gwst_SQLQuery)
 {$ENDIF}
 )
  or ( not assigned ( gstl_SQLCommand)
 {$IFDEF DELPHI_9_UP}
  and not assigned ( gwst_SQLCommand)
 {$ENDIF}
 )
  or ( gstl_KeyDataSource.Count = 0 )
   Then
    Exit ;

  gb_Record := True ;
  if assigned ( galv_OtherList )
   Then
    galv_OtherList.gb_Record := True ;
    // Mise à jour avant validation de la composition du groupe
  try
    p_PostDataSourceOwner;
  finally
    if assigned ( galv_OtherList )
     Then
      galv_OtherList.gb_Record := False ;
    gb_Record := False ;
  End ;

  // On a peut-être abandonné l'enregistrement de datasourceOwner
  if  assigned ( gdl_DataLinkOwner.DataSet )
  // Sur edition des groupes
  and ( gdl_DataLinkOwner.DataSet.State in [dsInsert,dsEdit] )
   Then
     // Alors on abandonne
    Exit ;

   try
    if  assigned ( gdl_DataLinkOwner.DataSet )
    and assigned ( gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ))
    and (   not assigned ( galv_OtherList )
    or (( gws_RecordValue = '' ) and ( galv_OtherList.gws_RecordValue = '' ))) Then
      /// gestion d'affectation ou gestion de groupe
      Begin
        if not fb_BeginOpen Then
         Exit;

        if not gb_Basket Then
          if ( gs_GroupTable <> gs_TableSource )
          // Insertion 1N 1N avec les items
           Then
            lb_Executee := fb_ExecuteQueryNotLinkedNNGroupSourceDifferent
           else
            lb_Executee := fb_ExecuteQueryNotLinkedNNGroupSourceSimilar;

        // La gestion par Basket a sa propre et unique requête
        if  gb_Basket
        and assigned ( galv_OtherList ) Then
          fb_ExecuteQuery1N
        Else
          if gb_AllSelect
          and not gb_MontreTout
           Then
            if (  gs_GroupTable <> gs_TableSource ) Then
            // Gestion 1N 1N avec les exclusions
            Begin
              if gb_OptionTotalList
               Then
                Begin
                  lb_Executee := fb_ExecuteQueryNotLinkedNNGroupSourceDifferentTotal;
                End
               Else
                 lb_Executee:=fb_ExecuteQueryNotLinkedNNGroupSourceDifferentTotalOut;
            End
           else
        if gb_OptionTotalList
        and not gb_MontreTout Then
         lb_Executee:=fb_ExecuteQueryNotLinkedNNGroupSourceSimilarTotal
        Else
         Begin
          if  not gb_Basket
          and assigned ( galv_OtherList )
          and  ( galv_OtherList.gb_OptionTotalList )
          Then
            lb_Executee:=fb_ExecuteQueryNotLinkedNNGroupSourceSimilarTotalOut;
        End ;
     End
    Else
    // Pas de Datasourceowner donc mode enregistrement sans possibilité de se déplacer sur les données
    // Donc il faut une valeur à affecter à la place des données du scroll
     Begin
      // Il faut ajouter les champs avec l'état ajoute si on est sur la liste en dehors du tout
      if( not gb_AllSelect or not gb_OptionTotalList or gb_MontreTout )
       Then
         lb_Executee:= fb_ExecuteQueryShowAllGroup ( gstl_KeysListOut );
      // Suppression Autre liste non principale
      if  not      ( galv_OtherList.DataListPrimary )
      and assigned ( galv_OtherList.Datasource )
      and assigned ( galv_OtherList.Datasource.DataSet )
      // Il faut supprimer les champs avec l'état supprime si on est sur la liste en dehors du tout
      and ( not gb_AllSelect or not galv_OtherList.gb_OptionTotalList or gb_MontreTout )
       Then
         lb_Executee:= fb_ExecuteQueryShowAllGroup ( galv_OtherList.gstl_KeysListOut );

      if gb_AllSelect
      and not gb_MontreTout
       Then
         if gb_OptionTotalList
          Then
            lb_Executee:=fb_ExecuteQueryLinkedAllSelectTotal
          Else
            if  not gb_Basket
            and  ( galv_OtherList.gb_OptionTotalList )
             Then
               lb_Executee:=fb_ExecuteQueryLinkedAllSelect;
    End ;
    if Assigned(ge_OnDatasetValidate) Then
     Begin
      ge_OnDatasetValidate ( gdat_DataSetQuery );
     end;
  Except
    on E: Exception do
     Begin
      if assigned ( ge_RecordError )
      and ( E is EDataBaseError )
       Then
        Begin
         lda_Action := daFail ;
         ge_RecordError ( gdl_DataLink.DataSet, ( E as EDataBaseError ), lda_Action );
         if lda_Action = daRetry Then
          p_Record ( Sender );
        End
       Else
        f_GereException( e, gdl_DataLink.DataSet );
     End ;
  End ;
  // C'est un évènement alors on abandonne car enregistrement effectué
  if assigned ( Self )
   Then
    p_Cancel ( nil );
  if   lb_Executee
  and assigned ( ge_RecordedEvent )
   Then
    ge_RecordedEvent ( gdl_DataLink.DataSet );
End ;

function fb_BuildWhereBasket ( const aalv_Primary : TDBGroupView ; var as_Result : String ; const ab_GetNull, ab_GetCurrent : Boolean ) : Boolean ;
Begin
  Result := fb_BuildWhereBasket ( aalv_Primary, as_Result, ab_GetNull, ab_GetCurrent, False );
End ;
// Fonction : fb_BuildWhereBasket
// Description : Construction de la clause where du Basket
// aalv_Primary : Liste principale
// aws_Result   : La requête si elle existe
// Résultat     : existence de la requête
function fb_BuildWhereBasket ( const aalv_Primary : TDBGroupView ; var as_Result : String ; const ab_GetNull, ab_GetCurrent, ab_order : Boolean ) : Boolean ;
var //lb_PremiereFois ,
    lb_Parentheses  : Boolean ;
    ls_Tri : String ;
    li_i, li_j : Integer ;
    ls_TexteSQL : String ;
    lvar_KeyItems : Variant ;
    gb_in : Boolean;
Begin
  if assigned ( aalv_Primary.ge_BasketGetAll ) Then
    Begin
      Result := aalv_Primary.ge_BasketGetAll ( aalv_Primary, as_Result, ab_GetNull, ab_GetCurrent );
      Exit ;
    End ;
  ls_TexteSQL := '';
  Result := False ;
  as_Result := '' ;
//  lb_PremiereFois := True ;
  with aalv_Primary do
    Begin
     with galv_OtherList do
      for li_i := 0 to high ( gt_KeyOwners ) do
        if  ( gstl_KeysListOut [ 0, li_i ] > '' ) Then
         Begin
          gb_in := not gb_EstPrincipale or ( galv_OtherList.gt_KeyOwners [ li_i ].i_Option <= 1 );
          Break;
         End;
{      if gb_OptionTotalList Then
        for li_i := 0 to high ( gstl_KeysListOut ) do
          if  ( gt_KeyOwners [ li_i ].as_Key <> Null ) Then
            if ( gb_EstPrincipale or ( gt_KeyOwners [ li_i ].i_Option in [CST_GROUPE_TRANS_EXCLU,CST_GROUPE_TRANS_RETOUR] )) Then
              fi_AddListe ( lt_Outs , gstl_KeysListOut [ li_i ], False ) ;}
      // Affectation aux clés d'exclusion de l'autre liste
      if gdl_DataLink.Active
      and gb_in
      and fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0, ls_TexteSQL,
                              galv_OtherList.gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[gstl_KeyDataSource[gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count]])
        Then
          begin
//            lvar_KeyItems := galv_OtherList.gstl_KeysListOut [ li_i ] ;
            // Propriété DatasourceGroupTable : gs_GroupTable
            // lien unité
            if Result
             Then AppendStr ( as_Result, CST_SQL_OR + '(' )
             Else AppendStr ( as_Result, CST_SQL_WHERE + '(( ' );
            Result := True ;
//            lb_PremiereFois := False ;
            AppendStr ( as_Result, gstl_UnitsDatasource [ 0 ] + ' IN (' + ls_TexteSQL +')'  );
            for li_i := 1 to gstl_UnitsDatasource.Count - 1 do
                 if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0, ls_TexteSQL,
                                        galv_OtherList.gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[gstl_KeyDataSource [ li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ]]) Then
                   AppendStr ( as_Result, 'AND (' + gstl_UnitsDatasource [ li_i ] + ' NOT IN ( ' + ls_TexteSQL + ')' );
            AppendStr ( as_Result, ')'+#13#10);
          end;
      lb_Parentheses := False ;
      if ab_GetCurrent
      and assigned ( gdl_DataLinkOwner.DataSet )
      and assigned ( gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey )) Then
       with gdl_DataLinkOwner.DataSet do
        Begin
          if Result
           Then AppendStr ( as_Result, CST_SQL_OR + '(('  )
           Else AppendStr ( as_Result, CST_SQL_WHERE + '((( ' );
          lb_Parentheses := True ;
          Result := True ;
          if ((FindField ( gs_GroupKey ) is TStringField )
          or (FindField ( gs_GroupKey ) is TBlobField )
          or ( FindField ( gs_GroupKey ).DataType IN CST_DELPHI_FIELD_STRING ))
           Then AppendStr ( as_Result, gs_GroupField + ' =''' + FindField ( gs_GroupKey ).AsString + '''' )
           Else AppendStr ( as_Result, gs_GroupField + ' =' + FindField ( gs_GroupKey ).AsString );
        End ;
      // mode Affectation : Affectation des groupements entièrement déplacés et des unités d'exclusion
      if gb_OptionTotalList Then
       //other list
       with galv_OtherList.gdl_DataLinkOwner.DataSet do
        for li_i := 0 to high ( gt_OriginKey ) do
         if ( gt_OriginKey [ li_i ].i_Option = CST_GROUPE_TRANS_TOTAL )
         and ( gt_OriginKey [ li_i ].as_Key [0]> '' ) Then
           Begin
            lvar_KeyItems := gt_OriginKey [ li_i ].as_Key ;
            // lien groupe à null
            if Result Then
              Begin
                if lb_Parentheses
                 Then AppendStr ( as_Result, CST_SQL_OR  )
                 Else AppendStr ( as_Result, CST_SQL_OR + '(( ' );
              End
            Else
              AppendStr ( as_Result, CST_SQL_WHERE + '(((' );
  //          lb_PremiereFois := False ;
            lb_Parentheses  := True ;
            Result := True ;
            if gb_EstPrincipale Then
              Begin
                if ((gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ) is TStringField )
                or (gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ) is TBlobField )
                or ( gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ).DataType IN CST_DELPHI_FIELD_STRING ))
                 Then AppendStr ( as_Result, gs_GroupField + '=''' + lvar_KeyItems + '''' )
                 Else AppendStr ( as_Result, gs_GroupField + '='   + lvar_KeyItems        )
              End
              Else
                if ((FindField ( galv_OtherList.gs_GroupKey ) is TStringField )
                or (FindField ( galv_OtherList.gs_GroupKey ) is TBlobField )
                or (FindField ( galv_OtherList.gs_GroupKey ).DataType IN CST_DELPHI_FIELD_STRING ))
                 Then AppendStr ( as_Result, gs_GroupField + '=''' + lvar_KeyItems + '''' )
                 Else AppendStr ( as_Result, gs_GroupField + '='   + lvar_KeyItems        );
           End ;
      if ab_GetNull and (( not gb_EstPrincipale and assigned ( gdl_DataLink.DataSet ))
                        or ( gb_EstPrincipale and assigned ( galv_OtherList.gdl_DataLink.DataSet ))) Then
        Begin
//          lb_PremiereFois := False ;
          // lien groupe à null
          if Result Then
            Begin
              if lb_Parentheses
               Then AppendStr ( as_Result, CST_SQL_OR    )
               Else AppendStr ( as_Result, CST_SQL_OR + '(( ' );
            End
          Else
            AppendStr ( as_Result, CST_SQL_WHERE + '(((' );
//          lb_PremiereFois := False ;
          lb_Parentheses  := True ;
          Result := True ;
          AppendStr ( as_Result, gs_GroupField + CST_SQL_IS + CST_SQL_NULL );
        End ;
      if lb_Parentheses Then
        // Déplacement total effectué au moins une fois alors ...
        // ... Affectation des unités d'exclusion
        Begin
          AppendStr ( as_Result, ')' );

          if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0, ls_TexteSQL,
                                 galv_OtherList.gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[gstl_Keydatasource[gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count]] )
           Then
            Begin
              AppendStr ( as_Result, CST_SQL_AND + '(' + gstl_UnitsDatasource [ 0 ] + ' NOT IN (' + ls_TexteSQL + ')' );
              for li_i := 1 to gstl_UnitsDatasource.Count - 1 do
                   if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0, ls_TexteSQL,
                                          gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[gstl_KeyDataSource [ li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ]]) Then
                     AppendStr ( as_Result, 'AND (' + gstl_UnitsDatasource [ li_i ] + ' NOT IN ( ' + ls_TexteSQL + ')' );
              AppendStr ( as_Result, ')'+#13#10);
            end;

          if not gb_in
          and fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0, ls_TexteSQL,
                                  galv_OtherList.gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[gstl_Keydatasource[gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count]] )
           Then
             Begin
              AppendStr ( as_Result, CST_SQL_AND + '(' + gstl_UnitsDatasource [ 0 ] + ' NOT IN (' + ls_TexteSQL + ')' );
              for li_i := 1 to gstl_UnitsDatasource.Count - 1 do
                   if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0, ls_TexteSQL,
                                          gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[gstl_KeyDataSource [ li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count ]]) Then
                     AppendStr ( as_Result, 'AND (' + gstl_UnitsDatasource [ li_i ] + ' NOT IN ( ' + ls_TexteSQL + ')' );
              AppendStr ( as_Result, ')'+#13#10);
             end;

          AppendStr ( as_Result, ')' );
        End ;
      If Result Then
        AppendStr ( as_Result, fws_GetExistingFilter + ')' );
      if ( ab_Order )
      and Result Then
        Begin
          ls_Tri := fs_PrepareSorting ;
          if ls_Tri <> '' Then
            AppendStr ( as_Result, ' ' +  CST_SQL_ORDER_BY + ' ' + fs_PrepareSorting );
        End ;
    End ;
//  showMessage ( as_Result );
End ;

// Evènement abandonne du bouton abandonner
// Sender : obligatoire
procedure TDBGroupView.p_Cancel(Sender: TObject);
  // Si Basket : on ne pas tout effacer
var //lb_VerifieModifs ,
    lb_EffaceTout : Boolean ;
    li_Modifie    ,
    li_i          : Integer ;
//    lvar_KeyCompare : Variant ;

Begin
  // Par défaut on efface tout
  gb_NoScroll := False ;
  lb_EffaceTout := True ;
  // Ancien évènement
  if assigned ( ge_CancelClick )
  and assigned ( Sender )
   Then
    ge_CancelClick ( Sender );
    // Gestion Basket : doit-on vraiment abandonner ?
  if  gb_Basket
  and gb_EstPrincipale
  and ( assigned ( Sender ) or ( galv_OtherList.Items.Count > 0 )) Then
  // Basket non vide
    Begin
      // sinon c'est qu'on a enregistré : on n'efface pas tout
      lb_EffaceTout := False ;
    End ;
   // On peut utiliser le Basket : réinitialisation complète
  if lb_EffaceTout Then
    Begin
      Finalize ( gt_OriginKey );
      //  gstl_KeysListOut : clés du Basket
      Finalize ( gstl_KeysListOut );
      Finalize ( gt_KeyOwners     );
    End ;
  // Réinitialise aussi l'autre liste
  if assigned ( galv_OtherList )
   Then
    Begin
      if lb_EffaceTout
       Then
        Begin
          Finalize ( galv_OtherList.gt_OriginKey     );
          Finalize ( galv_OtherList.gt_KeyOwners      );
          Finalize ( galv_OtherList.gstl_KeysListOut  );
          galv_OtherList.p_Reinitialise ;
          // Gestion Basket : liste secondaire vide à la réinitialisation
          if not gb_Basket Then
            galv_OtherList.p_AddRecords ;
        End
      // MAJ du 29-9-2004
      else
        if assigned ( Sender ) Then
          Begin
            //Réinitialisation si on n'a pas enregistré
            galv_OtherList.gb_OptionTotalList := gb_AllSelect ;
            galv_OtherList.gb_TotalListReal := gb_AllSelect ;
            gb_OptionTotalList := False ;
            gb_TotalListReal := False ;
            galv_OtherList.gb_AllSelect := gb_AllSelect ;
            gb_AllLoaded := False ;
            galv_OtherList.gb_AllLoaded := False ;

            // Clés à enlver lorsqu'on a renvoyé des éléments dans le Basket et qu'ils sont dans un transfert total
            for li_i := 0 to high ( gt_KeyOwners ) do
              if  ( gstl_KeysListOut [ 0 ] [li_i]> '' )
              and ( fi_findInListVarBool ( gt_OriginKey, gt_KeyOwners     [ li_i ].as_Key, False, True, [1] ) <> -1 ) Then
                Begin
                  gstl_KeysListOut [0] [ li_i ] := '' ;
                  gt_KeyOwners     [ li_i ].as_Key [0] := '' ;
                End ;

            // abandon : on replace les enregistrements à enregistrer dans le Basket
            with galv_OtherList do // other list
            for li_i := 0 to high ( galv_OtherList.gt_KeyOwners ) do
              if  ( gstl_KeysListOut [0] [ li_i ] > '' )
              and ( gt_KeyOwners     [ li_i ].i_Option = CST_GROUPE_TRANS_DESTI  ) Then
                Begin
                  if fi_AddListe ( gstl_KeysListOut, galv_OtherList.gstl_KeysListOut, li_i, True  ) <> - 1  Then
                    Begin
                      li_Modifie := fi_AddListe  ( gt_KeyOwners     , galv_OtherList.gt_KeyOwners     [ li_i ].as_Key, False );
                      gt_KeyOwners [ li_Modifie ].i_Option := CST_GROUPE_TRANS_EXCLU ;
                    End ;
                  galv_OtherList.gstl_KeysListOut [0] [ li_i ] := '' ;
                  galv_OtherList.gt_KeyOwners     [ li_i ].as_Key [ 0 ] := '' ;
                End ;

            // Les transferts sont effectués on met à jour le Basket
            galv_OtherList.Refresh ;
            if assigned ( gBt_Basket ) Then
                gBt_Basket.Enabled := Self.Enabled and assigned ( gstl_SQLQuery )
                                                   and ds_DatasourceQuery.DataSet.Active
                                                   and not ds_DatasourceQuery.DataSet.IsEmpty ;
        End
      Else
       with galv_OtherList do // other list
        Begin
          // rafraichissement après enregistrement : On réinitialise ce qui doit être enregistré
            for li_i := 0 to high ( gt_KeyOwners ) do
              if  ( gstl_KeysListOut [0] [ li_i ] > '' )
              and ( gt_KeyOwners      [ li_i ].i_Option = CST_GROUPE_TRANS_DESTI  ) Then
                Begin
                  gstl_KeysListOut [0] [ li_i ] := '' ;
                  gt_KeyOwners     [ li_i ].as_Key [ 0 ] := '' ;
                End ;
          Refresh ;
        End ;
    End ;
  // Réinitialisation sans sauvegarder
  if lb_EffaceTout Then
    p_Reinitialise
  Else
    Begin
      // on efface les items
      p_ReinitialisePasTout ;
//      gb_AllSelect := False ;
      // On va ajouter à nouveau des enregistrements
      gvar_WorkingGroup := Null ;
      gb_OptionTotalList := False ;
      gb_TotalListReal := False ;
    End ;
  // ajoute à nouveau les enregistrements
//  if not gb_Basket Then
  p_AddRecords ;
  p_UndoRecord;
  if assigned ( ge_CancelledEvent )
  and assigned ( Sender )
   Then
    ge_CancelledEvent ( gdl_DataLink.DataSet );

End ;

// Réinitialisation : Appelée pour recharger
procedure TDBGroupView.p_Reinitialise ;
Begin
  if  not assigned ( gdl_DataLinkOwner.DataSet )
  and assigned ( gdl_DataLink.DataSet )
  and not gb_Open
  and not ( csDestroying in Componentstate )
    Then
      try
        fb_RefreshDataset ( gdl_DataLink.DataSet, False );
      finally
      End ;
  // héritage de la réinitilisation
  inherited ;
  // Initialisation
  gb_LoadList := False ;
  gvar_WorkingGroup := Null ;

  if gb_AllSelect
  and not gb_MontreTout
   Then
    Begin
      gb_AllSelect := False ;
      // On a peut-être trié le query dans la liste où tout a été transféré
      if  gb_OptionTotalList
      and assigned ( gdl_DataLink.Datasource         )
      and assigned ( gdl_DataLink.DataSet )
       Then
         fb_Sort;
    End ;

  if  gb_Basket
  and gb_EstPrincipale
  and assigned ( galv_OtherList ) Then
    Begin
      if ( galv_OtherList.Items.Count = 0 )
      and assigned ( gBt_Basket ) Then
        gBt_Basket.Enabled := False ;
    End ;

  if not gb_Basket Then
    Finalize ( gstl_KeysListOut );
  gb_TotalListReal := False ;
  gb_OptionTotalList := False ;
End ;


//////////////////////////////////////////////////////////////////////////////////
// Fonction : fvar_CanPutPlus
// Description : Test si on peut mettre un plus à l'enregistrement en affectation
//////////////////////////////////////////////////////////////////////////////////

function TDBGroupView.fvar_CanPutPlus ( const aadoq_Dataset, aadoq_Query : TDataset ; const asi_ItemsSelected : TListItem ): Variant ;
var
  ls_CodeCherche : tt_ValeurStrings;
  li_i : Integer;
///  ls_Compare         : String ;
Begin
  Result := Null ;
  if gb_Basket Then
   Begin
    if ( high ( gt_OriginKey ) >= 0 ) Then
     Begin
      SetLength(ls_CodeCherche,High(gt_ColonneCle)+1);
      for li_i := 0 to high ( gt_ColonneCle ) do
        Begin
          if ( gt_ColonneCle [ li_i ] = 0   ) Then
            Begin
              ls_CodeCherche [ li_i ] := asi_ItemsSelected.Caption ;
            End
          Else
            Begin
              ls_CodeCherche [ li_i ] := asi_ItemsSelected.SubItems [ gt_colonneCle [ li_i ] - 1 ];
            End ;
        End;
      aadoq_Query.Filter := fs_createFilter(aadoq_Dataset,gstl_KeyDataSource,gstl_KeyDataSource,ls_CodeCherche);

      try
        if aadoq_Query.RecordCount > 0 Then
          Begin
            Result := aadoq_Query.FieldByName ( gs_GroupField ).Value ;
          End ;
      finally
      End ;
     end;
   end;
End ;


// Utilisé : On a changé d'état
// Gestion des mises à jour de la clé primaire des groupes
procedure TDBGroupView.EditingChanged;
begin
  inherited ;

  // Et sur le datasource des groupes
  if assigned ( Datasource )
  and assigned ( Datasource.DataSet )
   Then
    Begin
      // On est repassé en consultation des données : rafraichissement de l'enregistrement en cours
       // Si on est passé en insertion ou revenu en consultation
      if     ( Datasource.DataSet.State = dsInsert )
//           or ( Datasource.DataSet.State = dsBrowse ))
       Then
         // Mise A Jour de la liste
        LoadList ;

    End ;
end;


// Supprime un item d'un tableau
// at_List  : Tableau
// alsi_Item : Item de la liste
function TDBGroupView.fi_SupprimeItem(var at_List: tt_TableauxStrings;
  const alsi_Item: TListItem): Integer;
// Index à supprimer
var li_i : Integer;
Begin
  // On cherche si la colonne clé est affichée
  Result := fi_findList ( at_List, alsi_Item );
  if Result > -1
   Then
    for li_i := 0 to high ( at_list ) do
     // On efface l'indice
      at_List [ li_i, Result ] := '' ;
End ;

function TDBGroupView.ft_GetValueList ( var at_List : tt_TableauxStrings ; const alsi_Item : TListItem ) : tt_ValeurStrings ;
var li_i, li_j : Integer;
Begin
  // On cherche si la colonne clé est affichée
  SetLength(Result,high(gt_ColonneCle)+1);
  for li_j := 0 to high ( at_List ) do
   Begin
     for li_i := 0 to high ( at_List[li_j] ) do
      if gt_ColonneCle [ li_j ] = 0
        Then Result [ li_j ] := alsi_Item.Caption
        Else Result [ li_j ] := alsi_Item.SubItems [ gt_ColonneCle [ li_j ] - 1 ];
   end;
End ;
function TDBGroupView.fi_FindList(var at_List: tt_TableauxStrings;
  const alsi_Item: TListItem): Integer;
Begin
  // On cherche si la colonne clé est affichée
  Result := fi_findInListArray ( at_List, ft_GetValueList (at_List, alsi_Item), False );
End ;

function TDBGroupView.ffi_LocateImageItem(const AItem : TListItem) : TField;
var ls_cle : String;
begin
  Result := nil;
  ls_cle := fs_GetItemKey(AItem);

  if ls_cle > '' Then
    Begin
      if galv_OtherList.Datasource.DataSet.Locate(gstl_KeyDataSource [ gstl_KeyDataSource.Count-1],ls_cle,[])
       Then Result:=galv_OtherList.Datasource.DataSet.FindField(gs_FieldImage)
       Else Result:=Datasource.DataSet.FindField(gs_FieldImage);
    End
   Else
    Begin
      Datasource.DataSet.Locate(gstl_KeyDataSource [ gstl_KeyDataSource.Count-1],ls_cle,[]);
      Result:=Datasource.DataSet.FindField(gs_FieldImage);
    end;
End;

// Evènement transférer dans cette liste
// Sender : obligatoire
procedure TDBGroupView.p_ClickTransfert(Sender: TObject);
var
  li_i, li_j , li_add,
  li_TotalSelection : integer;
  llsi_OldItemSelected : TListItem;
  llsi_ItemsSelected : TListItem;
  // pour l'instant ola dévalidation sert au Basket
  lb_ValideBoutons   ,
  lb_DeValideBoutons ,
  lb_DesactiveGrille ,
  lb_VerifieModifs   ,
  lb_VarIsString     ,
  lb_Continue        : Boolean ;
  lvar_Add        ,
  lvar_AddTempo   : Variant ;
  lBitmap : TBitmap ;
  lt_Add : tt_ValeurStrings;
  AField : TField;
  lws_SQL            : String ;
const
    lits_stateSelected = {$IFDEF FPC}[lisSelected]{$ELSE} [isSelected]{$ENDIF};
begin
  if not assigned ( gBt_List )
  or ( not assigned ( gstl_SQLCommand)
 {$IFDEF DELPHI_9_UP}
  and not assigned ( gwst_SQLCommand)
 {$ENDIF}
 )
   Then
    Exit ;
  lb_DesactiveGrille := False ;
  lb_VerifieModifs   := False ;
    // Items sélectionnés


  // On n' pas encore trouvé de plus ni de moins pour valider l'enregistrement
  lb_ValideBoutons   := False ;
  lb_DeValideBoutons := False ;

  /// Enregistrement des groupes avant transfert : peut annuler cet évènement
//  p_PostDataSourceOwner;
  // Ne fonctionne qu'en complémentarité
  if not assigned ( galv_OtherList )
//  or not assigned ( gdl_DataLinkOwner )
//  or not assigned ( gdl_DataLinkOwner.DataSet )
  // Revérification : y-a-t-il maintenant des items sélectionnés
  or ( galv_OtherList.SelCount = 0 )
   then
    exit;

  lws_SQL := '';

  if gb_Basket Then
    Begin
      if gb_EstPrincipale Then
        Begin
          // Préparation mise à jour des plus et des clés d'origine
          if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, lws_SQL, gt_OriginKey, False, [] ) Then
            with ds_DataSourceQuery2 do
                try
                  DataSet.Close ;
                  lws_SQL := CST_SQL_SELECT + gs_GroupField + ',' + gs_UnitsField + CST_SQL_FROM + gs_GroupTable + CST_SQL_WHERE + gs_GroupField + ' IN (' + lws_SQL + ')' ;
                  p_OpenSQLQuery ( DataSet, lws_SQL );
                Except
                  on e: Exception do
                    f_GereException ( e, DataSet )
                End ;

          lvar_Add    := gdl_DataLinkOwner.DataSet.FieldValues [ gs_GroupKey ] ;
          lb_VarIsString := ((gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ) is TStringField )
                            or (gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ) is TBlobField )
                            or ( gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ).DataType IN CST_DELPHI_FIELD_STRING ));

          // Mise à jour des clés d'origine
//          if assigned ( lrec_Recordments ) Then
            for li_i := 0 to high ( gt_OriginKey ) do
              if  ( gt_OriginKey [ li_i ].as_Key [ 0 ] > '' ) Then
                with ds_DataSourceQuery2.DataSet do
                  Begin
                    Filter := fs_createFilter(ds_DataSourceQuery2.DataSet, gstl_GroupField, gstl_GroupField, gt_OriginKey [ li_i ].as_Key );
                    if RecordCount = 0 Then
                      Begin
                        gt_OriginKey [ li_i ].as_Key [ 0 ] := '' ;
                        galv_OtherList.gt_OriginKey [ li_i ].as_Key [0]:= '' ;
                      End ;
                  End ;

        End
      Else
        Begin
          lvar_Add    := galv_OtherList.gdl_DataLinkOwner.DataSet.FieldValues [ galv_OtherList.gs_GroupKey ];
        End ;
      if gb_OptionTotalList Then
        Begin
          gb_TotalListReal := fi_findInListVarBool ( gt_OriginKey, lvar_Add, False, True, [CST_GROUPE_TRANS_TOTAL]) <> -1;
        End ;
      p_AddOriginKey ( lvar_Add );
      // Eff
    End ;

  // Deuxième compteur : pour récupérer l'item suivant
  li_j := galv_OtherList.Selected.Index;
  // nombre d'éléments à ajouter
  li_TotalSelection := galv_OtherList.SelCount - 1;
  // Plus rapide avec Items.BeginUpdate et Items.EndUpdate
    // Items sélectionnés
  {$IFDEF FPC}
  BeginUpdate ;
  galv_OtherList.BeginUpdate ;
  {$ELSE}
  Items.BeginUpdate ;
  galv_OtherList.Items.BeginUpdate ;
  {$ENDIF}
  llsi_ItemsSelected := galv_OtherList.Selected;
  if DataImgField > '' Then
    lBitmap:= TBitmap.Create;
  if assigned ( galv_OtherList.gdl_DataLink.DataSet ) Then
    galv_OtherList.gdl_DataLink.DataSet.DisableControls;
  if assigned ( gdl_DataLink.DataSet ) Then
    gdl_DataLink.DataSet.DisableControls;
  try

    for li_i := 0 to li_TotalSelection do
      Begin
        begin
        // Insertion tout en haut
          gVG_ListItem := Items.Insert(0);
          //Transfert
          gVG_ListItem.Caption := llsi_ItemsSelected.Caption;
          gVG_ListItem.SubItems.addStrings ( llsi_ItemsSelected.SubItems );
          li_add := -1;
          lb_Continue := True ;
          if  not gb_Basket or not gb_EstPrincipale  Then
           /// Ajoute dans l'autre liste d'exclusion
            Begin
              // L'item se trouve soit dans les sous-items soit dans le caption de l'item
              lt_Add := ft_AddValue ( llsi_ItemsSelected );
              li_add := fi_AddListe ( galv_OtherList.gstl_KeysListOut , lt_Add, True );
              if gb_Basket
              and ( li_Add > -1 ) Then
                Begin
                  li_Add := fi_AddListe ( galv_OtherList.gT_KeyOwners     ,  gt_OriginKey [ high ( gt_OriginKey )].as_Key, False );
                  galv_OtherList.gT_KeyOwners [ li_Add ].i_Option := CST_GROUPE_TRANS_EXCLU ;
                End ;
            End
           Else
           //Gestion type Basket
            Begin

              li_Add := -1 ;
              lvar_AddTempo := fvar_CanPutPlus ( gdl_DataLink.DataSet, ds_DatasourceQuery2.DataSet, llsi_ItemsSelected );
              lb_Continue := ( VarCompareValue ( lvar_AddTempo, lvar_Add ) <> vrEqual );
              if lb_Continue or gb_AllSelect Then
               Begin
                lt_Add := ft_AddValue ( llsi_ItemsSelected );
                li_add := fi_AddListe ( galv_OtherList.gstl_KeysListOut , lt_Add, True );
               end;
              if gb_Basket
              and ( li_Add > - 1 ) Then
                Begin
                  if lvar_AddTempo <> Null Then
                    fi_AddListe ( galv_OtherList.gT_KeyOwners, lvar_AddTempo, False );
                  with galv_OtherList.gT_KeyOwners [ li_Add ] do
                    if gb_EstPrincipale and lb_Continue
                     Then i_Option := CST_GROUPE_TRANS_DESTI
                    Else
                      if gb_EstPrincipale
                       Then i_Option := CST_GROUPE_TRANS_RETOUR
                       Else i_Option := CST_GROUPE_TRANS_EXCLU ;
                End ;
            End ;

          AField := nil;

          if ( DataImgField > '' )
           Then
             Begin
              if high(gt_ColonneCle) > 0
               Then
                Begin
                 if gdl_DataLink.DataSet.Locate(gs_UnitsKeyFieldValues,lt_Add,[])
                  Then AField := gdl_DataLink.DataSet.FindField(DataImgField)
                  Else if galv_OtherList.gdl_DataLink.DataSet.Locate(gs_UnitsKeyFieldValues,lt_Add,[])
                  Then AField := galv_OtherList.gdl_DataLink.DataSet.FindField(DataImgField)
                End
                 Else if gdl_DataLink.DataSet.Locate(gs_UnitsKeyFieldValues,lt_Add[0],[])
                  Then AField := gdl_DataLink.DataSet.FindField(DataImgField)
                  Else if galv_OtherList.gdl_DataLink.DataSet.Locate(gs_UnitsKeyFieldValues,lt_Add[0],[])
                  Then AField := galv_OtherList.gdl_DataLink.DataSet.FindField(DataImgField);
              AddImageItem ( lBitmap, gVG_ListItem );
              if llsi_ItemsSelected.ImageIndex > -1 Then
               with galv_OtherList, llsi_ItemsSelected do
                 Begin
                   if Assigned(LargeImages)
                   and (LargeImages.Count > ImageIndex) Then
                    LargeImages.Delete(ImageIndex);
                   if Assigned(SmallImages)
                   and (SmallImages.Count > ImageIndex) Then
                    SmallImages.Delete(ImageIndex);
                 end;
             end;

          li_Add := fi_SupprimeItem ( gstl_KeysListOut, llsi_ItemsSelected );
          lb_DeValideBoutons :=  ( li_Add > -1 ) or ( gb_Basket and (( gb_EstPrincipale and gb_OptionTotalList ) or not assigned ( galv_OtherList ) or ( not gb_EstPrincipale  and galv_OtherList.gb_OptionTotalList ))) ;
   {       if gb_Basket
          and ( li_Add > -1 ) Then
           gt_KeyOwners [ li_Add ].as_Key [ 0 ] := '' ;
    }
            // si principale
          if assigned ( AField )
           Then lb_ValideBoutons        := True
           Else
            if gb_EstPrincipale
             Then
              Begin
              // alors état inséré
                if not gb_Basket
            // Plus tard gérer les clés en les enlevant complètement
                and ( llsi_ItemsSelected.{$IFDEF FPC}ImageIndex{$ELSE}StateIndex{$ENDIF} <> gi_ImageSupprime )
                 Then
                   Begin
                     gb_NoScroll := True ;
                     gVG_ListItem.{$IFDEF FPC}ImageIndex{$ELSE}StateIndex{$ENDIF} := gi_ImageInsere ;
                     lb_ValideBoutons        := True ;
                   End
                 Else
                  Begin
                    if (     gb_Basket
                         and ( lb_Continue )) Then
                      Begin
                        gb_NoScroll := True ;
                        gVG_ListItem.{$IFDEF FPC}ImageIndex{$ELSE}StateIndex{$ENDIF} := gi_ImageInsere ;
                        // On valide les boutons d'enregistrement
                        lb_ValideBoutons := True ;
                      End ;
                  End ;
              End
             Else
             // sinon état supprimé
              if not gb_Basket
            // Plus tard gérer les clés en les enlevant complètement
              and ( llsi_ItemsSelected.{$IFDEF FPC}ImageIndex{$ELSE}StateIndex{$ENDIF} <> gi_ImageInsere ) Then
                Begin
                  gVG_ListItem.{$IFDEF FPC}ImageIndex{$ELSE}StateIndex{$ENDIF} := gi_ImageSupprime ;
                   // On valide les boutons d'enregistrement
                  lb_ValideBoutons := True ;
                End;
          // Pas d'état insertion ou suppression : on supprime de la liste d'inclusion
          if not Assigned ( AField )
          and not gb_AllSelect
          and ( gVG_ListItem.{$IFDEF FPC}ImageIndex{$ELSE}StateIndex{$ENDIF} = -1 )
          and not gb_Basket Then
            fi_SupprimeItem ( galv_OtherList.gstl_KeysListOut, llsi_ItemsSelected );

          llsi_OldItemSelected := llsi_ItemsSelected;
          // Prochain item
          llsi_ItemsSelected := galv_OtherList.GetNextItem ( llsi_ItemsSelected{$IFDEF FPC}, sdBelow{$ENDIF} , lits_stateSelected);
          llsi_OldItemSelected.Delete;
          if llsi_ItemsSelected = nil Then
             Break;
        end;
      End ;
    // fin de la mise à jour
  finally
    with galv_OtherList do
     if assigned ( gdl_DataLink.DataSet ) Then
       try
         gb_Open := True;
         gdl_DataLink.DataSet.EnableControls;
       Finally
         gb_Open := False;
       End;
    if assigned ( gdl_DataLink.DataSet ) Then
      try
        gb_Open := True;
        gdl_DataLink.DataSet.EnableControls;
      Finally
        gb_Open := False;
      End;
    if DataImgField > '' Then
      Begin
        {$IFNDEF FPC}
        lBitmap.Dormant;
        {$ENDIF}
        lBitmap.Destroy;
      End;
    {$IFDEF FPC}
    EndUpdate ;
    galv_OtherList.EndUpdate ;
    {$ELSE}
    Items.EndUpdate ;
    galv_OtherList.Items.EndUpdate ;
    {$ENDIF}
    Invalidate ;
    galv_OtherList.Invalidate ;
  End ;

//  Repaint ;
  // Dévalide les boutons si il faut dévalider
  if lb_DeValideBoutons
  and not lb_ValideBoutons  Then
    if gb_Basket
    and  assigned ( galv_OtherList ) Then
      Begin
        if  not gb_EstPrincipale Then
          Begin
            // scrute les clés ajoutées une à une
            if not gb_AllSelect or not galv_OtherList.gb_OptionTotalList Then
              Begin
              for li_i := high ( gt_KeyOwners ) downto 0 do
                if  ( gstl_KeysListOut [0] [ li_i ] <> '' )
                and (( gt_KeyOwners [ li_i ].as_Key [0]= '' ) or ( VarCompareValue ( gt_KeyOwners [ li_i ].as_Key, galv_OtherList.gvar_KeyDestination ) <> vrEqual ))
                and ( gt_KeyOwners [ li_i ].i_Option = CST_GROUPE_TRANS_DESTI ) Then
                  Begin
                    lb_DeValideBoutons := False ;
                    Break ;
                  End ;
              End
            Else
              Begin
                if galv_OtherList.Items.Count > 0 Then
                  Begin
                    lb_DeValideBoutons := False ;
                  End
                Else
                  galv_OtherList.gb_OptionTotalList := False ;
              End ;
          End
        Else
          if not gb_AllSelect or not gb_OptionTotalList Then
            Begin
              with galv_OtherList do
                for li_i := high ( gt_KeyOwners ) downto 0 do
                  if  ( gstl_KeysListOut [0] [ li_i ] <> '' )
                  // Comparaison de variants
                  and (( gt_KeyOwners [ li_i ].as_Key [0]= '' ) or ( VarCompareValue ( gt_KeyOwners [ li_i ].as_Key, gvar_KeyDestination ) <> vrEqual ))
                  and ( gt_KeyOwners [ li_i ].i_Option = CST_GROUPE_TRANS_DESTI ) Then
                    Begin
                      lb_DeValideBoutons := False ;
                      Break ;
                    End ;
            End
          Else
            Begin
              if Items.Count > 0 Then
                Begin
                  lb_DeValideBoutons := False ;
                End
              Else
                gb_OptionTotalList := False ;
            End ;
      End
    Else
      Begin
        // Plus tard gérer les clés en les enlevant complètement
        if gb_AllSelect
        or fb_SettedList (               gstl_KeysListOut )
        or fb_SettedList ( galv_OtherList.gstl_KeysListOut ) Then
          Begin
            lb_DeValideBoutons := False ;
          End
      End
    Else
      lb_DeValideBoutons := False ;

  if  gb_EstPrincipale
  and gb_AllSelect
  and ( galv_OtherList.Items.Count = 0 ) Then
    Begin
      if not lb_ValideBoutons
      and ( lb_DeValideBoutons
           or ( assigned ( gBt_Record ) and not gBt_Record.Enabled )
           or ( assigned ( gBT_Cancel ) and not gBT_Cancel.Enabled )
           or ( not  assigned ( gBT_Record ) and  not assigned ( gBT_Cancel ) and assigned ( gBT_Optional ) and not gBT_Optional.Enabled )) Then
        Begin
          Finalize ( gt_OriginKey );
          Finalize ( gstl_KeysListOut );
          Finalize ( gt_KeyOwners );
          Finalize ( galv_OtherList.gt_KeyOwners );
          Finalize ( galv_OtherList.gstl_KeysListOut );
          Finalize ( galv_OtherList.gt_OriginKey );
        End
      Else
        for li_i := 0 to high ( gt_OriginKey ) do
          if gt_OriginKey [ li_i ].i_Option = 1 Then
            Begin
              gt_OriginKey [ li_i ].i_Option := 0 ;
              galv_OtherList.gt_OriginKey [ li_i ].i_Option := 0 ;
            End ;
      gb_OptionTotalList := False ;
      galv_OtherList.gb_OptionTotalList := False ;
    End ;

  if lb_DeValideBoutons Then
    Begin
      gb_NoScroll := False ;
      galv_OtherList.gb_NoScroll := False ;
    End ;

  // On peut enregistrer et abandonner ?
  if assigned ( gBT_Optional ) Then
    if ( lb_ValideBoutons ) Then
      Begin
        if not gBT_Optional.enabled Then
          Begin
            if Self.Enabled Then
              gBT_Optional.enabled := True;
            lb_DesactiveGrille       := True ;
          End ;
      End
    Else
      if  lb_DeValideBoutons
      and gBT_Optional.enabled Then
          Begin
            gBT_Optional.enabled := False;
            lb_VerifieModifs := True ;
          End ;
  if assigned ( gBt_Record ) Then
  // On a un plus ou un moins
    if ( lb_ValideBoutons ) Then
      Begin
        if not gBt_Record.Enabled Then
          Begin
            if Self.Enabled Then
              gBt_Record  .Enabled := True ;
            lb_DesactiveGrille       := True ;
          End ;
      End
    Else
      if  lb_DeValideBoutons
      and gBt_Record.Enabled Then
        Begin
          gBt_Record.Enabled := False ;
          lb_VerifieModifs := True ;
        End ;
  if assigned ( gBt_Cancel ) Then
  // On a un plus ou un moins
    if ( lb_ValideBoutons ) Then
      Begin
        if not gBt_Cancel.Enabled Then
          Begin
            if Self.Enabled Then
              gBt_Cancel.Enabled := True ;
            lb_DesactiveGrille := True ;
          End ;
      End
    Else
      if lb_DeValideBoutons
      and gBt_Cancel.Enabled Then
        Begin
          gBt_Cancel.Enabled := False ;
          lb_VerifieModifs := True ;
        End ;
  if assigned ( gBt_OtherTotal )
  and Self.Enabled Then
    gBt_OtherTotal.Enabled := True ;
  // Gestion du bouton Basket
  if  gb_Basket Then
  if  not gb_EstPrincipale Then
    Begin
      if ( Items.Count > 0 ) Then
        Begin
          if Self.Enabled Then
           gBt_Basket.Enabled := True ;
        End
       Else
         gBt_Basket.Enabled := False ;
    End
  Else
    Begin
      if ( galv_OtherList.Items.Count > 0 ) Then
        Begin
          if Self.Enabled Then
           gBt_Basket.Enabled := True ;
        End
       Else
         gBt_Basket.Enabled := False ;
    End ;
  if assigned ( gBt_List ) Then
    gBT_List.Enabled := False ;
  if ( galv_OtherList.Items.Count = 0 )
  and assigned ( gBt_TotalList ) Then
    gBt_TotalList.Enabled := False ;
  if lb_DesactiveGrille Then
    p_DesactiveGrille ;
  if lb_VerifieModifs
   Then
    p_VerifieModifications ;
  // Ancien évènement
  if assigned ( ge_ListClick )
  and assigned ( Sender )
   Then
    ge_ListClick ( Sender );

end;
//////////////////////////////////////////////////////////////////////////////////
// Procédure : p_AddOriginKey
// Description : Ajoute une clé d'origine de transfert
// paramètres :
// avar_Add : La clé à ajouter dans la liste des clés d'origine
//////////////////////////////////////////////////////////////////////////////////
procedure TDBGroupView.p_AddOriginKey ( const avar_Add : variant );
var li_Trouve, li_i : Integer ;
    lb_Test : Boolean ;
Begin
  if gb_EstPrincipale Then
    // transfert vers la liste principale
    Begin
      // nouvelle destination : on n'ajoute pas la clé mais on affecte la destination
      gvar_KeyDestination := gdl_DataLinkOwner.DataSet.FieldValues [ gs_GroupKey ];
    End
  Else
    Begin
      // La clé a peut-être été déjà ajoutée
      li_Trouve := fi_findInListVarBool ( gt_OriginKey, avar_Add, False, False, [] );
      // pas trouvé
      if li_Trouve = -1 Then
       with gt_OriginKey [ li_Trouve ] do
        Begin
          // On ajoute
          SetLength ( gt_OriginKey, high ( gt_OriginKey ) + 2 );
          SetLength ( galv_OtherList.gt_OriginKey, high ( galv_OtherList.gt_OriginKey ) + 2 );
          li_Trouve := high ( gt_OriginKey );
          CopyArrayVariantToArrayString (avar_Add, gt_OriginKey [ li_Trouve ].as_Key);
          CopyArrayVariantToArrayString (avar_Add, galv_OtherList.gt_OriginKey [ li_Trouve ].as_Key );
          lb_Test := gb_TotalListReal ;
        End
      Else
        // sinon on affectera un transfert total par défaut
        lb_Test := gb_TotalListReal or ( gt_OriginKey [ li_Trouve ].i_Option > CST_GROUPE_TRANS_SIMPLE ) ;
      if lb_Test Then
        // transfert total
        Begin
          gt_OriginKey [ li_Trouve ].i_Option := CST_GROUPE_TRANS_TOTAL ;
          galv_OtherList.gt_OriginKey [ li_Trouve ].i_Option := CST_GROUPE_TRANS_TOTAL ;
        End
      Else
        // transfert classique
        Begin
          gt_OriginKey [ li_Trouve ].i_Option := CST_GROUPE_TRANS_SIMPLE ;
          galv_OtherList.gt_OriginKey [ li_Trouve ].i_Option := CST_GROUPE_TRANS_SIMPLE ;
        End ;
    End ;
End ;
// Evènement tout transférer dans cette liste
// Sender : obligatoire
procedure TDBGroupView.p_ClickTransfertTotal(Sender: TObject);
// Tableau pour échange
//var lt_TableauTempo : tt_Tableau ;
var lb_DesactiveGrille : Boolean ;
begin
  lb_DesactiveGrille := False ;

  // Il est possible qu'on ai juste à faire un transfert total item par item
  if  assigned ( galv_OtherList )
  and assigned ( Sender )
    // On montre tout dans la deux listes : tous les items sont présents
  and (  ( gb_MontreTout  and galv_OtherList.gb_MontreTout )
    // Gestion Basket : une seule liste est à considérer
      or ( gb_Basket and ((( gb_AllLoaded or gb_EstPrincipale )  and galv_OtherList.gb_AllLoaded ) or ( gb_EstPrincipale and ( fi_findInListVarBool( gt_OriginKey, Null, False, True, [1]) = -1 )))))
   Then
    Begin
      // sélection de tous les items
      galv_OtherList.SelectAll ;
      // transfert standard
      p_ClickTransfert ( nil );
      // evènement du bouton
      if assigned ( ge_TotalListClick ) Then
        ge_TotalListClick ( Sender );
      Exit ;
    End ;
  if  assigned ( galv_OtherList )
  // Test inutile mais on ne sait jamais
  and  ( galv_OtherList.Items.Count > 0 )
   Then
    Begin
      // Ajoute tous les enregistrements dans la liste
      p_TransfertTotal ;
        // On vide l'autre liste
      galv_OtherList.p_VideTotalList ;
        /// Le transfert de tout dans la liste ne fonctionne qu'avec l'autre liste
      if gb_Basket
      and gB_AllSelect Then
        Begin
          if not gb_EstPrincipale Then
            Begin
              if fb_SettedList ( gstl_KeysListOut )
              or fb_SettedOriginalList ( gt_OriginKey, True, 1 ) Then
                Begin
                  if assigned ( gbt_Basket ) then
                    Begin
                      if Self.Enabled Then
                       gBt_Basket.Enabled := True ;
                    End
                End ;
            End
          Else
            if  assigned ( galv_OtherList )
            and (    fb_SettedList         ( galv_OtherList.gstl_KeysListOut )
                  or fb_SettedOriginalList ( galv_OtherList.gt_OriginKey, True, 1 )) Then
                Begin
                  if assigned ( gbt_Basket ) then
                    Begin
                      if Self.Enabled Then
                       gBt_Basket.Enabled := True ;
                    End
                End ;
        End ;
     if  ( not gb_Basket or gb_EstPrincipale ) Then
       Begin
          // On peut enregistrer et abandonner
          if assigned ( gBt_Record )
          and not gBt_Record.Enabled Then
            Begin
              gBt_Record.Enabled := True ;
              lb_DesactiveGrille := True ;
            End ;
          if assigned ( gBT_Optional )
          and not gBT_Optional.Enabled Then
            Begin
              gBT_Optional.Enabled := True ;
              lb_DesactiveGrille := True ;
            End ;
          if assigned ( gBt_Cancel )
          and not gBt_Cancel.Enabled  Then
            Begin
              gBt_Cancel.Enabled := True ;
              lb_DesactiveGrille := True ;
            End ;
        End ;
    End ;
  if assigned ( gBt_TotalList )
   Then
    gBt_TotalList.Enabled := False ;
  if assigned ( gBt_List )
   Then
    gBt_List.Enabled := False ;
  if lb_DesactiveGrille Then
    p_DesactiveGrille ;
  //  ancien évènement
  if assigned ( ge_TotalListClick )
   Then
    ge_TotalListClick ( Sender );
end;

// Sur edition des groupes on enregistre
procedure TDBGroupView.p_PostDataSourceOwner;
//var lbkm_GardeLeBonEnregistrement : TBookmark ;
begin
  if  assigned ( gdl_DataLinkOwner )
  and assigned ( gdl_DataLinkOwner.DataSet )
  // Sur edition des groupes
  and (   ( gdl_DataLinkOwner.DataSet.State = dsEdit   )
       or ( gdl_DataLinkOwner.DataSet.State = dsInsert ))
   Then
    Begin
      // on enregistre
      gdl_DataLinkOwner.DataSet.Post ;
    End ;
End ;


// gestion de l'Evènement p_ClickTransfertTotal : tout transférer dans cette liste
procedure TDBGroupView.p_TransfertTotal;
var ls_TexteSQL, ls_Fields : String ;
    li_i, li_Modifie : Integer ;
    lvar_Key : Variant ;
begin
   // Post du datasource des groupes si édition
//  p_PostDataSourceOwner;
  // Réinitialise avant de tout rajouter
  // Pas de reqêteur : on quite
  if not assigned ( gstl_SQLQuery ) Then
    Exit ;

  p_Reinitialise ;


  if gb_Basket
  and assigned ( galv_OtherList ) Then
    Begin
      gvar_WorkingGroup := Null ;
      gb_LoadList        := False ;
      if gb_EstPrincipale
       Then lvar_Key := gdl_DataLinkOwner.DataSet.FieldValues [ gs_GroupKey ]
       Else lvar_Key := galv_OtherList.gdl_DataLinkOwner.DataSet.FieldValues [ galv_OtherList.gs_GroupKey ];
      if  ( lvar_Key <> Null ) Then
        for li_i := 0 to high ( galv_OtherList.gstl_KeysListOut ) do
          if  ( galv_OtherList.gt_KeyOwners [ li_i ].as_Key [0] > '' )
          and (( VarCompareValue ( galv_OtherList.gt_KeyOwners [ li_i ].as_Key, lvar_Key ) = vrEqual )
                or ( gb_EstPrincipale and ( galv_OtherList.gt_KeyOwners [ li_i ].i_Option = CST_GROUPE_TRANS_DESTI ))) Then
            Begin
              galv_OtherList.gstl_KeysListOut [0] [ li_i ] := '' ;
              galv_OtherList.gt_KeyOwners     [ li_i ].as_Key [0] := '' ;
            End ;
        for li_i := 0 to high ( gt_KeyOwners ) do
          if  ( gstl_KeysListOut [0] [ li_i ] > '' )
          and ( fi_findInListVarBool ( gt_OriginKey, gt_KeyOwners [ li_i ].as_Key, False, True, [CST_GROUPE_TRANS_SIMPLE] ) <> -1 ) Then
//          and ( galv_OtherList.fi_FindItem ( gstl_KeysListOut [ li_i ] ) <> -1  ) Then
            Begin
              if fi_AddListe  ( galv_OtherList.gstl_KeysListOut, gstl_KeysListOut, li_i, True ) <> - 1  Then
                Begin
                  li_Modifie := fi_AddListe  ( galv_OtherList.gt_KeyOwners    , gt_KeyOwners [ li_i ].as_Key, False );
                  if gb_EstPrincipale Then
                    Begin
                      galv_OtherList.gt_KeyOwners [ li_Modifie ].i_Option := CST_GROUPE_TRANS_DESTI ;
                    End
                  Else
                    galv_OtherList.gt_KeyOwners [ li_Modifie ].i_Option := CST_GROUPE_TRANS_EXCLU ;
                End ;
              gstl_KeysListOut [0] [ li_i ] := '' ;
              gt_KeyOwners     [ li_i ].as_Key [0] := '' ;
            End ;
      if gb_EstPrincipale Then
        Begin
          li_Modifie := fi_findInListVarBool ( gt_OriginKey, lvar_Key, False, True, [1] );
          if li_Modifie <> -1 Then
            Begin
              gt_OriginKey [ li_Modifie ].as_Key [0] := '' ;
              galv_OtherList.gt_OriginKey [ li_Modifie ].as_Key [0] := '' ;
              if  not fb_SettedList ( galv_OtherList.gstl_KeysListOut )
              and not fb_SettedList ( gstl_KeysListOut ) Then
                Begin
                  Refresh ;
                  Exit ;
                End ;
            End ;
        End
      Else
        Begin
           // Mise à zéro des clés d'exclusion de la liste qui vont peut-être être transférée
          gb_OptionTotalList := True ;
          gb_TotalListReal := True ;
          p_AddOriginKey ( lvar_Key );
        End ;
    End ;
      // fermeture du query
  // on va tout transférer virtuellement dans la liste : pointe sur tous les enregistrements
  gb_AllSelect := True ;

   // Mise à zéro des clés d'exclusion de la liste qui vont peut-être être transférée
  gb_OptionTotalList := True ;
  gb_TotalListReal := True ;
  ls_TexteSQL := '';
  ds_DatasourceQuery.DataSet.Close();
  if assigned ( ge_QueryAll ) Then
    ge_QueryAll ( ds_DatasourceQuery.DataSet )
  Else
    if gb_Basket Then
      Begin
        if not gb_EstPrincipale Then
          gb_NoScroll := False ;
        gstl_SQLQuery.BeginUpdate ;
        ls_Fields := CST_SQL_ALL;
        for li_i := 0 to gstl_FieldsList.Count - 1 do
          if li_i = 0
           then ls_Fields:=gstl_FieldsList[li_i]
           else AppendStr(ls_Fields,','+gstl_FieldsList[li_i]);
        if gs_FieldImage > '' Then
           AppendStr(ls_Fields, ', ' + gs_TableSource + CST_FIELD_DECLARE_SEPARATOR + gs_FieldImage );
        gstl_SQLQuery.Text := CST_SQL_SELECT + CST_SQL_ALL + CST_SQL_FROM + gs_GroupTable ;
        if   fb_BuildWhereBasket ( Self, ls_TexteSQL, True, True, True ) Then
          Begin
            gstl_SQLQuery.Add ( ls_TexteSQL );
          End ;
        gstl_SQLQuery.EndUpdate ;
      End
    Else
      if not assigned ( galv_OtherList )
      or (( gws_RecordValue = '' ) and ( galv_OtherList.gws_RecordValue = '' )) Then
        Begin
          //Mise à jour du query en fonction des propriétés
          if ( not gb_Basket ) // Si assoce NN
          or ( gs_GroupTable = '' )
          or ( gs_TableSource   = '' )
           Then
            Begin
              // On sélectionne tous les enregistrements
              gstl_SQLQuery.BeginUpdate ;
              gstl_SQLQuery.Clear ;
              gstl_SQLQuery.Add( CST_SQL_SELECT + CST_SQL_ALL + CST_SQL_FROM + gs_TableSource  );
              gstl_SQLQuery.EndUpdate ;
            End
          Else
           // Sinon on sélectionne les champs de ce groupe et ceux à null
            Begin
              gstl_SQLQuery.BeginUpdate ;
              gstl_SQLQuery.Clear ;
              gstl_SQLQuery.Add( CST_SQL_SELECT+ gs_TableSource+CST_FIELD_DECLARE_SEPARATOR +CST_SQL_ALL + CST_SQL_FROM + gs_GroupTable + CST_TABLE_SEPARATOR+ gs_TableSource   );
              for li_i := 0 to gstl_GroupField.Count - 1 do
                if li_i = 0 Then
                  gstl_SQLQuery.Add( 'WHERE (' + gs_GroupTable + CST_FIELD_DECLARE_SEPARATOR + gstl_GroupField [ li_i ] + CST_SQL_IS + CST_SQL_NULL   )
                Else
                  gstl_SQLQuery.Add( 'AND ' + gs_GroupTable + CST_FIELD_DECLARE_SEPARATOR + gstl_GroupField [ li_i ] + CST_SQL_IS + CST_SQL_NULL   );
              gstl_SQLQuery.Add( ')' );
              if gb_Basket then
                begin
                  if gb_EstPrincipale then
                    if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0, ls_TexteSQL, galv_OtherList.gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[gstl_KeyDatasource[gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count]] )
                     Then
                      Begin
                        gstl_SQLQuery.Add( CST_SQL_OR + '(' + gs_GroupTable + CST_FIELD_DECLARE_SEPARATOR + gstl_UnitsDatasource [ 0 ] + ' IN (' + ls_TexteSQL + ')' );
                        for li_i := 0 to gstl_UnitsDatasource.Count - 1 do
                          if fb_TableauVersSQL ( gdl_DataLink.DataSet, gstl_KeyDataSource, li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count, 0, ls_TexteSQL, galv_OtherList.gstl_KeysListOut, gdl_DataLink.DataSet.FieldValues[gstl_KeyDatasource[li_i + gstl_KeyDataSource.Count - gstl_UnitsDatasource.Count]] ) Then
                           gstl_SQLQuery.Add( CST_SQL_AND + '(' + gs_GroupTable + CST_FIELD_DECLARE_SEPARATOR + gstl_UnitsDatasource [ li_i ] + ' IN (' + ls_TexteSQL + '))' );
                        gstl_SQLQuery.Add( ')'+#10) ;
                      End;
                  gstl_SQLQuery.Add ( CST_SQL_OR );
                  fb_SetMultipleFieldToQuery ( gstl_GroupField, gstl_SQLQuery, gt_OriginKey, gdl_DataLink.DataSet, True , gs_GroupTable );
{                  for li_i := 0 to high ( gt_OriginKey ) do
                    if VarIsStr ( gt_OriginKey [ li_i ].as_Key ) Then
                      if ((gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ) is TStringField )
                      or (gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ) is TBlobField )
                      or ( gdl_DataLinkOwner.DataSet.FindField ( gs_GroupKey ).DataType IN CST_DELPHI_FIELD_STRING ))
                       Then
                        gstl_SQLQuery.Add ( CST_SQL_OR + gs_GroupField + '=''' + fs_stringDbQuote ( gt_OriginKey [ li_i ].as_Key ) + '''' )
                      Else
                        gstl_SQLQuery.Add ( CST_SQL_OR + gs_GroupField + '=' + fs_stringDbQuote ( gt_OriginKey [ li_i ].as_Key ));}
                End ;
              gstl_SQLQuery.Add( ')' );
              for li_i := 0 to gstl_GroupKey.Count - 1 do
                if  assigned ( gdl_DataLinkOwner )
                and assigned ( gdl_DataLinkOwner.DataSource )
                and assigned ( gdl_DataLinkOwner.DataSet )
                and assigned ( gdl_DataLinkOwner.DataSet.FindField ( gstl_GroupKey [ li_i ] ))
                 Then
                  Begin
                   /// Et les groupes à null
                    if li_i = 0 Then
                      gstl_SQLQuery.Add( 'OR (' + gs_GroupTable + CST_FIELD_DECLARE_SEPARATOR + gstl_GroupField [ li_i ]    )
                    Else
                      gstl_SQLQuery.Add( 'AND ' + gs_GroupTable + CST_FIELD_DECLARE_SEPARATOR + gstl_GroupField [ li_i ]    );
                    if ((gdl_DataLinkOwner.DataSet.FindField ( gstl_GroupKey [ li_i ] ) is TStringField )
                    or (gdl_DataLinkOwner.DataSet.FindField ( gstl_GroupKey [ li_i ] ) is TBlobField )
                    or ( gdl_DataLinkOwner.DataSet.FindField ( gstl_GroupKey [ li_i ] ).DataType IN CST_DELPHI_FIELD_STRING ))
                     Then gstl_SQLQuery.Add (  '=''' + fs_stringDbQuote ( gdl_DataLinkOwner.DataSet.FieldByName ( gstl_GroupKey [ li_i ] ).AsString ) + '''' )
                     Else gstl_SQLQuery.Add (  '='   +                  ( gdl_DataLinkOwner.DataSet.FieldByName ( gstl_GroupKey [ li_i ] ).AsString )        );
                   if li_i = gstl_GroupKey.Count - 1 Then
                      gstl_SQLQuery.Add( ')' );
                  End  ;
              gstl_SQLQuery.Add ( fws_GetExistingFilter );

              gstl_SQLQuery.EndUpdate ;
            End ;
        End
      Else
        Begin
          // On sélectionne tous les enregistrements
          gstl_SQLQuery.BeginUpdate ;
          gstl_SQLQuery.Clear ;
          gstl_SQLQuery.Add( CST_SQL_SELECT+ gs_TableSource+CST_FIELD_DECLARE_SEPARATOR +CST_SQL_ALL + CST_SQL_FROM + gs_GroupTable + CST_TABLE_SEPARATOR+ gs_TableSource );
          gstl_SQLQuery.Add ( CST_SQL_WHERE + '(' );
          fb_SetMultipleFieldToQuery ( gstl_GroupField, gstl_SQLQuery, gws_RecordValue, gdl_DataLink.DataSet, True , gs_GroupTable  );
          {
          if ( gws_RecordValue = '' ) Then
            for li_i := 0 to gstl_GroupField.Count - 1 do
              if li_i = 0 Then
                gstl_SQLQuery.Add( 'WHERE ((' + gstl_GroupField [ li_i ] + CST_SQL_IS + CST_SQL_NULL   )
              Else
                gstl_SQLQuery.Add( 'AND ' + gstl_GroupField [ li_i ] + CST_SQL_IS + CST_SQL_NULL   )
          Else
            for li_i := 0 to gstl_GroupField.Count - 1 do
                if ((gdl_DataLink.DataSet.FindField ( gstl_GroupField [ li_i ] ) is TStringField )
                or (gdl_DataLink.DataSet.FindField ( gstl_GroupField [ li_i ] ) is TMemoField )
                or ( gdl_DataLink.DataSet.FindField ( gstl_GroupField [ li_i ] ).DataType IN CST_DELPHI_FIELD_STRING ))
                  Begin
                    if li_i = 0 Then
                      gstl_SQLQuery.Add( CST_SQL_WHERE + '((' + gs_GroupField + '=''' + gws_RecordValue + '''' )
                    Else
                      gstl_SQLQuery.Add ( CST_SQL_AND + gs_GroupField + '=''' + gws_RecordValue + '''' )
                  End
                Else
                  Begin
                    if li_i = 0 Then
                      gstl_SQLQuery.Add( CST_SQL_WHERE + '((' + gs_GroupField + '=' + gws_RecordValue )
                    Else
                      gstl_SQLQuery.Add ( CST_SQL_AND + gs_GroupField + '=' + gws_RecordValue )
                  End ;
          gstl_SQLQuery.Add( ')' );}
          gstl_SQLQuery.Add( 'OR ' );
          fb_SetMultipleFieldToQuery ( gstl_GroupField, gstl_SQLQuery, galv_OtherList.gws_RecordValue, gdl_DataLink.DataSet );
          gstl_SQLQuery.Add( '))' );
          gstl_SQLQuery.Add ( fws_GetExistingFilter );
          gstl_SQLQuery.EndUpdate ;
        End ;
    // Ouverture
  try
    ds_DatasourceQuery.DataSet.Open ;
    gb_AllLoaded := ds_DatasourceQuery.DataSet.IsEmpty ;
    if  gb_AllLoaded Then
      Begin
        gb_OptionTotalList := False ;
        Finalize ( gt_OriginKey );
        Finalize ( gt_KeyOwners  );
        Finalize ( gstl_KeysListOut );
        Finalize ( galv_OtherList.gt_OriginKey );
        Finalize ( galv_OtherList.gt_KeyOwners  );
        Finalize ( galv_OtherList.gstl_KeysListOut );
        galv_OtherList.gb_OptionTotalList := False ;
      End ;
  Except
  End ;
  if not ds_DatasourceQuery.DataSet.Active Then
    Exit ;
 gb_Open := True;
    // Ajoute les enregistrements dans la liste
  try
    p_AddRecords ;
  finally
     gb_Open := False;
  end;
end;

// gestion de l'Evènement transférer dans l'autre liste
procedure TDBGroupView.p_VideTotalList;
begin
  // Initialisation
  p_Reinitialise ;
  // On a tout transféré
  gb_AllSelect := True ;

  // On a tout transféré dans l'autre liste
  gb_AllLoaded := True ;
  if gb_Basket
  and gb_EstPrincipale Then
    p_UndoRecord;
end;

// peut-on trier les items ? : Gestion des changements sur la liste avant de trier
// Résultat : Va-t-on quitter l'évènement de tri sur click
// résultat : on peut trier ou pas
function TDBGroupView.fb_CanSort  : Boolean ;
Begin
  // par défaut : on peut trier
  Result := True ;
End ;

{
function TDBGroupView.CustomDrawItem(alsi_Item: TListItem;
  acds_Etat: TCustomDrawState; acds_Etape: TCustomDrawStage) : Boolean ;
begin
  if gb_CouleursLignes
   Then
    p_groupeCustomDrawItem( Self, alsi_Item );
  Result := inherited CustomDrawItem ( alsi_Item, acds_Etat, acds_Etape );
  if gb_CouleursLignes
   Then
    p_groupeCustomDrawItem( Self, alsi_Item );
end;

procedure TDBGroupView.DrawItem(alsi_Item: TListItem;
  arec_Rectangle : TRect; aods_Etat: TOwnerDrawState) ;
begin

  if gb_CouleursLignes
   Then
    p_groupeCustomDrawItem( Self, alsi_Item );

  inherited DrawItem ( alsi_Item, aRec_Rectangle, aods_Etat );
  if gb_CouleursLignes
   Then
    p_groupeCustomDrawItem( Self, alsi_Item );
end;
}
// Evènement double clicke
// Transfert dans l'autre liste
procedure TDBGroupView.DblClick;
begin
  inherited;
  if ( csDesigning in ComponentState ) Then
    Exit ;
  // Appel du transfert dasn l'autre liste
  if assigned ( galv_OtherList )
   Then
    galv_OtherList.p_ClickTransfert ( galv_OtherList );
end;

// Evènement sur déplacement de liste : méthode dynamique dans tlistview
// valide l'objet déplacé sur la liste
// aobj_Source : objet déplacé sur la liste
// ai_X, ai_Y  : Positions obligatoires
// ads_Etat    : obligatoire
// Résultat :
// ab_Accepte  : Accepte l'objet ou non
procedure TDBGroupView.DragOver(aobj_Source: Tobject; ai_X,
  ai_Y: Integer; ads_Etat: TDragState; var ab_Accepte: Boolean);
begin
  inherited;
  if ( csDesigning in ComponentState ) Then
    Exit ;
  // appel de la fonction de test de mc_fonctions_groupes
  if assigned ( galv_OtherList )
   Then
    ab_Accepte := ( aobj_Source = galv_OtherList );
//    p_groupeDragOver ( aobj_Source, ab_Accepte, galv_OtherList );
end;

// Evènement Drag and drop surchargé
// aobj_Source : la liste à partir d'où on transfert
// ai_X, ai_Y  : Position obligatoire
procedure TDBGroupView.DragDrop(aobj_Source: Tobject; ai_X,
  ai_Y: Integer);
begin
  inherited;
  if ( csDesigning in ComponentState ) Then
    Exit ;
    // Correspond à un transfert simple : onclick du bouton transfert simple
  p_ClickTransfert ( Self );
end;

// Evènement sur clicke: méthode dynamique dans tlistview
// Un item est sélectionné : Mise à jour des boutons
// abt_Bouton : bouton clické obligatoire
// ass_EtatShift : état obligatoire
// ai_x, ai_y    : Position obligatoire
procedure TDBGroupView.MouseDown(abt_Bouton: TMouseButton;
  ass_EtatShift: TShiftState; ai_x, ai_y: Integer);
begin
  inherited;
  if ( csDesigning in ComponentState ) Then
    Exit ;
  // Appel du test de sélection et de mise à jour du bouton
  if assigned ( gBt_Other )
   Then
    p_groupeMouseDownDisableEnableFleche ( Self, gBt_Other );
end;

procedure TDBGroupView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if ( csDesigning in ComponentState ) Then
    Exit ;
  // Appel du transfert dasn l'autre liste
  if assigned ( galv_OtherList )
  and ( Key in [VK_LEFT, VK_RIGHT ])
   Then
    galv_OtherList.p_ClickTransfert ( galv_OtherList );
end;


// Sélection d'un item : méthode statique
// ai_index : Le numéro de l'item sélectionné obligatoire
procedure TDBGroupView.DoSelectItem ( llsi_ItemsSelected : TListItem; ab_selected : Boolean );
begin
   // Héritage
 {$IFNDEF FPC}
  if assigned ( ge_onselectItem ) then
    ge_onselectItem ( Self, llsi_ItemsSelected, ab_selected );
 {$ELSE}
  inherited DoSelectItem ( llsi_ItemsSelected, ab_selected );
 {$ENDIF}
  //  Appel de la procédure de mc_fonctions_groupes
  // active le bouton si il y a des items
  if assigned ( gBt_Other )
   Then
    p_groupeMouseDownDisableEnableFleche ( Self, gBt_Other );
end;

//////////////////////////////////////////////////////////////////////////////
// Fonction    : fb_ValideBoutons
// Description : Active les boutons
// Retour      : Boutons activés ou pas
//////////////////////////////////////////////////////////////////////////////
function TDBGroupView.fb_ValideBoutons : Boolean;
begin
  Result := False ;
  if ( gb_EstPrincipale or not gb_Basket ) Then
    Begin
      if assigned ( gBt_Cancel )
      and not gBt_Cancel.Enabled
       Then
         Begin
           if Self.Enabled Then
             gBt_Cancel .enabled := True;
           Result := True ;
         End ;
      if assigned ( gBt_Record )
      and not gBt_Record.Enabled
       Then
         Begin
           if Self.Enabled Then
             gBt_Record.enabled := True;
           Result := True ;
         End ;
      if assigned ( gBT_Optional )
      and not gBT_Optional.Enabled Then
        Begin
          if Self.Enabled Then
            gBT_Optional.Enabled := True ;
          Result := True ;
        End ;
    End
  Else
  if ( not gb_EstPrincipale and gb_Basket and gb_OptionTotalList ) Then
    Begin
      if assigned ( gBt_Cancel )
      and not gBt_Cancel.Enabled
       Then
         Begin
           gBt_Cancel .enabled := False;
         End ;
      if assigned ( gBt_Record )
      and not gBt_Record.Enabled
       Then
         Begin
           gBt_Record.enabled := False;
         End ;
      if assigned ( gBT_Optional )
      and not gBT_Optional.Enabled Then
        Begin
          gBT_Optional.Enabled := False;
        End ;
    End

end;

// APppelle p_verifieModifications de form dico
procedure TDBGroupView.p_VerifieModifications;
begin
  if Supports(Owner, IFWFormVerify) Then
    ( Owner as IFWFormVerify ).VerifyModifying ;
end;
// désactive la grille de u_mcformdico
procedure TDBGroupView.p_DesactiveGrille;
begin
  if Supports(Owner, IFWFormVerify) Then
    ( Owner as IFWFormVerify ).Modifying;
end;

//////////////////////////////////////////////////////////////////////////////
// Procédure   : p_UpdateButtons
// Description : Rafraîchissement des boutons
// ai_ItemsAjoutes : Nombre d'éléments ajoutés ( le items.count ne mache pas à tous les moments )
//////////////////////////////////////////////////////////////////////////////
procedure TDBGroupView.p_UpdateButtons(const ai_ItemsAjoutes: Integer);
begin
  if assigned ( gBt_OtherTotal )
   Then
    if ai_ItemsAjoutes > 0 Then
      Begin
       if Self.Enabled Then
         gBt_OtherTotal.Enabled := True ;
      End
    Else
     /// alors si il y a des items
      If ( Items.Count > 0 )
      and Self.Enabled Then
        // activation du bouton de transfert
       gBt_OtherTotal.Enabled := True
       // si il n'y en a pas alors pas d'activation
{       else
         Begin
           gBt_OtherTotal.Enabled := False;
           if assigned ( gBt_Other ) Then
             gBt_Other.Enabled := False;
         End ;}
end;

// Disable ou enable une flèche en fonction de la liste source
// aLSV_groupe    : La liste source
// abt_item   : Le bouton flèche simple
procedure TDBGroupView.p_groupeMouseDownDisableEnableFleche(
  const aLSV_groupe: TListView; const abt_item: TControl);
begin
  if ( aLSV_groupe.SelCount > 0)
   then
    Begin
      if assigned ( abt_item )
      and Self.Enabled Then
        abt_item.Enabled := True
    End
   else
    if assigned ( abt_item )
     Then
       abt_item.Enabled := False;

end;

//////////////////////////////////////////////////////////////////////////////
// Fonction    : fb_Locate
// Description : Recherche un enregistrement exact à partir de la clé de l'enregistrement
// avar_Records: Les enregistrements de la clé
// ab_InPrimary: Liste principale ou pas
// Retour      : Trouvé ou pas
//////////////////////////////////////////////////////////////////////////////
function TDBGroupView.fb_Locate(
  const avar_Records: Variant ): Boolean;
begin
  gdl_DataLink.DataSet.Filter := fs_createFilter( gdl_DataLink.DataSet, gstl_KeyDataSource, gstl_UnitsDatasource, avar_Records );
  gdl_DataLink.DataSet.Filtered := True ;
  Result := gdl_DataLink.DataSet.RecordCount > 0 ;
  p_LocateRestore;
end;

//////////////////////////////////////////////////////////////////////////////
// Procédure   : p_LocateInit
//////////////////////////////////////////////////////////////////////////////
procedure TDBGroupView.p_LocateInit;
//var ls_Condition,
//    ls_Tables : String ;
begin
  gws_Oldfilter := gdl_DataLink.DataSet.Filter ;
  gb_Oldfiltered := gdl_DataLink.DataSet.Filtered ;
{  ls_Condition := gs_UnitsField + '=' + gs_UnitsField + CST_SQL_AND ;
//  if ab_InPrimary Then
    ls_Condition := ls_Condition + gs_GroupField + '=' ;
//  Else
//    ls_Condition := ls_Condition + gs_GroupField + ' NOT LIKE ';
  if gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).DataType in CST_DELPHI_FIELD_STRING Then
    ls_Condition := ls_Condition + '''' + fs_stringDbQuote ( gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).AsString ) + ''''
  Else
    ls_Condition := ls_Condition + gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).AsString ;
  if trim ( gs_TableSource ) <> Trim ( gs_GroupTable ) Then
    ls_Tables := gs_TableSource + ', ' + gs_GroupTable
  Else
    ls_Tables := gs_TableSource ;
  fonctions_db.p_LocateInit ( gadoq_QueryLocate, ls_Tables, ls_Condition );}
end;

//////////////////////////////////////////////////////////////////////////////
// Fonction    : p_LocateRestore
// Description : Recherche un enregistrement exact à partir de la clé de l'enregistrement
// avar_Records: Les enregistrements de la clé
// ab_InPrimary: Liste principale ou pas
// Retour      : Trouvé ou pas
//////////////////////////////////////////////////////////////////////////////
procedure TDBGroupView.p_LocateRestore;
var
  {$IFDEF WITH_TBOOKMARK}
  lbkm_Bookmark : TBookmark ;
  {$ELSE}
  lbkm_Bookmark : TBookmarkStr ;
  {$ENDIF}

begin
  lbkm_Bookmark := gdl_DataLink.DataSet.Bookmark ;
  gdl_DataLink.DataSet.Filter := gws_Oldfilter ;
  gdl_DataLink.DataSet.Filtered := gb_Oldfiltered ;
  try
    gdl_DataLink.DataSet.Bookmark := lbkm_Bookmark ;
  Except
  End ;
{  ls_Condition := gs_UnitsField + '=' + gs_UnitsField + CST_SQL_AND ;
//  if ab_InPrimary Then
    ls_Condition := ls_Condition + gs_GroupField + '=' ;
//  Else
//    ls_Condition := ls_Condition + gs_GroupField + ' NOT LIKE ';
  if gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).DataType in CST_DELPHI_FIELD_STRING Then
    ls_Condition := ls_Condition + '''' + fs_stringDbQuote ( gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).AsString ) + ''''
  Else
    ls_Condition := ls_Condition + gdl_DataLinkOwner.DataSet.FieldByName ( gs_GroupKey ).AsString ;
  if trim ( gs_TableSource ) <> Trim ( gs_GroupTable ) Then
    ls_Tables := gs_TableSource + ', ' + gs_GroupTable
  Else
    ls_Tables := gs_TableSource ;
  mc_fonctions_db.p_LocateInit ( gadoq_QueryLocate, ls_Tables, ls_Condition );}
end;

// Evènement inversion de listes
// Sender : obligatoire
procedure TDBGroupView.p_InvertClick(Sender: TObject);
var lt_Tempo : tt_TableauxStrings ;
    li_i     : Integer ;
    lb_Tempo : Boolean;
begin

  // Transfert tout
  SelectAll;
  Finalize ( lt_Tempo );
  if assigned ( galv_OtherList ) Then
    galv_OtherList.SelectAll;
  if (SelCount > 0) then
    p_ClickTransfert ( Self );

  // Inversion des tableaux
  for li_i := 0 to high ( gt_KeyOwners ) do
    if gstl_KeysListOut [0] [ li_i ] > '' Then
      fi_AddListe( lt_Tempo, gstl_KeysListOut, li_i, False );

  Finalize ( gstl_KeysListOut );

  // Inversion des propriétés
  if assigned ( galv_OtherList ) Then
    Begin
      lb_Tempo := galv_OtherList.gb_AllLoaded ;
      galv_OtherList.gb_AllLoaded := gb_AllLoaded ;
      gb_AllLoaded := lb_tempo ;
      lb_Tempo := galv_OtherList.gb_AllFetched ;
      galv_OtherList.gb_AllFetched := gb_AllFetched ;
      gb_AllFetched := lb_tempo ;
      lb_Tempo := galv_OtherList.gb_AllSelect ;
      galv_OtherList.gb_AllSelect := gb_AllSelect ;
      gb_AllSelect := lb_tempo ;
      if (galv_OtherList.SelCount > 0) then
        galv_OtherList.p_ClickTransfert ( Self );

      // Inversion des tableaux

      for li_i := 0 to high ( galv_OtherList.gstl_KeysListOut ) do
        fi_AddListe( gstl_KeysListOut, galv_OtherList.gstl_KeysListOut, li_i, False );
      Finalize ( galv_OtherList.gstl_KeysListOut );
      for li_i := 0 to high ( lt_Tempo ) do
        fi_AddListe( galv_OtherList.gstl_KeysListOut, tt_ValeurStrings(lt_Tempo [ li_i ]), False );
    End ;

  // Inversion des tableaux
  Finalize ( lt_Tempo );

  for li_i := 0 to high ( gt_KeyOwners ) do
    if gt_KeyOwners [ li_i ].as_Key [0] > '' Then
      fi_AddListe( lt_Tempo, gt_KeyOwners [ li_i ].as_Key, False );

  Finalize ( gt_KeyOwners );

  // Inversion des tableaux
  if assigned ( galv_OtherList ) Then
    Begin
      for li_i := 0 to high ( galv_OtherList.gt_KeyOwners ) do
        fi_AddListe( gt_KeyOwners, galv_OtherList.gt_KeyOwners [ li_i ].as_Key, False );
      Finalize ( galv_OtherList.gt_KeyOwners );
      for li_i := 0 to high ( lt_Tempo ) do
        fi_AddListe( galv_OtherList.gt_KeyOwners, lt_Tempo [ li_i ], False );
    End ;

  if assigned ( ge_BtnInvertClick ) Then
    ge_BtnInvertClick ( Sender );

end;

function TDBGroupView.ft_AddValue ( const alsi_ItemsSelected : TListItem ): tt_ValeurStrings;
var li_j : Integer;
begin
  SetLength(Result,high(gt_ColonneCle)+1);
  for li_j := 0 to high ( gt_ColonneCle ) do
   Begin
       // L'item se trouve soit dans les sous-items soit dans le caption de l'item
       if  ( gt_ColonneCle [ li_j ] > alsi_ItemsSelected.SubItems.Count )
       Then Result [ li_j ] := alsi_ItemsSelected.SubItems [ alsi_ItemsSelected.SubItems.Count - 1 ]
       Else
        if gt_ColonneCle [ li_j ] = 0
          Then Result [ li_j ] := alsi_ItemsSelected.Caption
          Else Result [ li_j ] := alsi_ItemsSelected.SubItems [ gt_ColonneCle [ li_j ] - 1 ];
   End

end;

// Réinitialisation : Appelée pour recharger au tri
procedure TDBGroupView.p_ReinitialisePasTout ;
Begin
  // héritage de la réinitilisation
  gb_LoadList := False ;
  inherited ;
  p_SetButtonsOther ( False )
End ;

// Réinitialisation : Appelée pour recharger au tri
procedure TDBGroupView.p_SetButtonsOther ( const ab_Value : Boolean );
Begin
  // héritage de la réinitilisation
  if assigned ( gBT_OtherTotal ) Then
    Begin
      gBT_OtherTotal.Enabled := ab_value ;
    End ;

  if assigned ( gBT_Other ) Then
    Begin
      gBT_Other.Enabled := ab_value ;
    End ;
End ;

//////////////////////////////////////////////////////////////////////////////
// Fonction    : fws_GetExistingFilter
// Description : Renvoi un filtre SQL si les propriétés DBAllFilter et DBAllFiltered sont renseignés
// Retour      : Le filtre avec le and SQL
//////////////////////////////////////////////////////////////////////////////
function TDBGroupView.fws_GetExistingFilter : String ;

Begin
  if ( Trim ( gws_Filter ) = '' )
  or not gb_Filtered Then
    Result := ''
  Else
    Result := CST_SQL_AND + gws_Filter ;
End ;

////////////////////////////////////////////////////////////////////////////
// Procédure   : p_SetFilter
// Description : Affecte DBAllFilter
// Value       : Le filtre à affecter
//////////////////////////////////////////////////////////////////////////////
procedure TDBGroupView.p_SetFilter(Value: String);
begin
  if Value <> gws_Filter Then
    Begin
      gws_Filter := Value ;
      if  gb_Filtered
      and gb_AllSelect
      and ( gws_Filter = '' ) Then
        Refresh ;
    End ;
end;

//////////////////////////////////////////////////////////////////////////////
// Procédure   : p_SetFiltered
// Description : Affecte DBAllFiltered
// Value       : Active ou désactive le filtre
//////////////////////////////////////////////////////////////////////////////
procedure TDBGroupView.p_SetFiltered(Value: Boolean);
begin
  if Value <> gb_Filtered Then
    Begin
      gb_Filtered := Value ;
      if  gb_Filtered
      and gb_AllSelect
      and ( gws_Filter = '' ) Then
        Refresh ;
    End ;

end;

procedure TDBGroupView.Refresh;
var lb_PremiereFois : Boolean ;
    ls_TexteSQL   : String ;
begin
  ls_TexteSQL := '' ;
  if gb_Basket Then
    Begin
      if gb_EstPrincipale Then
        Begin
          gb_TotalListReal := False ;
          gb_OptionTotalList     := False ;
          p_ReinitialisePasTout ;
          gvar_WorkingGroup := Null ;
          gb_LoadList        := False ;
          p_AddRecords ;
        End
      Else
        if  assigned ( gstl_SQLQuery ) Then
          Begin
              // MAJ du 13-4-2005
            ds_DatasourceQuery.DataSet.Close ;
            gstl_SQLQuery.BeginUpdate ;
            gstl_SQLQuery.Clear ;
            // Sélectionne notre groupe
            gstl_SQLQuery.Add( CST_SQL_SELECT + CST_SQL_ALL + CST_SQL_FROM + gs_TableSource );
            lb_PremiereFois := True ;
            if fb_BuildWhereBasket ( Self, ls_TexteSQL, True, False, True ) Then
              Begin
                lb_PremiereFois := False ;
                gstl_SQLQuery.Add ( ls_TexteSQL );
              End ;
            gstl_SQLQuery.EndUpdate ;
            if not lb_PremiereFois Then
              Begin
                if not lb_PremiereFois Then
                  Begin
                    ds_DatasourceQuery.DataSet.Open ;
                    p_ReinitialisePasTout ;
                    gb_LoadList        := False ;
                    gvar_WorkingGroup := Null ;
                    p_AddSyncronousRecords;
                  End ;
              End ;
          End ;
    End
  Else
    try
      gb_Open := True ;
      p_Reinitialise ;
      p_AddRecords ;
    finally
      gb_Open := False ;
    End ;
End;

initialization
{$IFDEF FPC}
  {$I U_GroupView.lrs}
{$ENDIF}
{$IFDEF VERSIONS}
  p_ConcatVersion ( gVer_TDBGroupView );
{$ENDIF}
finalization
  gim_GroupViewImageList.Free;
end.
