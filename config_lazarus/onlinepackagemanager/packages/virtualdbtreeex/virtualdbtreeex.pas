unit virtualdbtreeex;

 {

 15-APR-2003 Gerard Vanderveken Ghia bvba
French : Modifications par Matthieu Giroux de MICROCELT le 05-2003
Composant TBaseVirtualTreeEx :
C'est une Gestion des colonnes dans les Arbres virtuels version données

 Transformation des champs clés en réels vers du variant :
 - Les double deviennent variant
 - Plus de traduction en un certain type
 - Récupération des valeurs variant et non des réels

 Héritage des champs et des évènements maintenant publiés sur le TBaseVirtualTreeEx
 09-2004 Bug ChangeAll à True qui ne marche pas
 09-2004
 Ajout de la propriété CheckFieldName : Chargement des checks à partir des données
 Gestion de ResultFieldName en booléen

 03-2004 Bug affectations CheckState branches non cochables
 09- 2004 : Si pas de clique sur le titre un click sur le titre ouvre ou ferme tout le noeud sélectionné

 10-2004 : Gestion de règles de sélection à partir des données

 01-12-2004 : Dans TVirtualTreeView un code gêne le click sur les descendant dans HandleMouseUp :
 Mettre en commentaire //    		ChangeCheckState(FCheckNode, FPendingCheckState);
 C'est un code qui se répète dans la méthode qui suit
        DoCheckClick(FCheckNode, FPendingCheckState);

  10-01-2005
  Propriétés NodeLines active et dboAutoBmp non active

  14-06-2005
    Pas de lien de sélection vers les parents sur les branches sans coche

  19-09-2005 : Intégrité avec le nouveau TVirtualTreeView
 10-2005 : Bug affectation de NodeLines
 11-2005 : Possibilité de définir des actions de revalidation complète ou partielle
           dboSpecialVTChecks par défaut
  9-9-2008 Compatibilité LAZARUS avec DELPHI
 English : Modifications by Matthiew GIROUX from MICROCELT at 5-2003

 Transformations of data columns :
- double becomes variant
- Traduction as variant
- Bringing all as variant
- inheriting of data columns and now published events by TBaseVirtualTreeEx
9-9-2008 : LAZARUS with DELPHI compatibility
 09-2005 : Intégrity with new TVirtualTreeView
08- 2004

Adding CheckfieldName : Loading Boolean Checks from data
ResultFieldName can use Booleans

10-2004 : Selecting rules from Database

 09- 2003 : if no click on title a click on title expands or collapse all of selected node

 21-2004 : In TVirtualTreeView a line perturbs the click on herited trees in HandleMouseUp :
 Put in comments this line in previous method //      	ChangeCheckState(FCheckNode, FPendingCheckState);
 It is a repetition of lin that follows where the same line is :
        DoCheckClick(FCheckNode, FPendingCheckState);

  2005-03 Bug affectations of CheckState on uncheckable nodes
  2005-01-10
  Proprerties NodeLines activated et dboAutoBmp not activated
 2005-11-27 : We can now define some actions with complete or partial revalidation
              dboSpecialVTChecks by default

 - Added stateimages and data loaded from the database.
   Usage:
   * property StateImgIdxField = datbase (integer) field with selection
   for imagelist from property StateImages.
   * property DBDataFieldNames holds list of databasefieldnames (separated by ; )
   These are put in a variant or variantarray at userdatarec.DBData.
   Works fine for normal data (numbers, dates, strings). Problems for int64.
   Don't overdue for memo's etc.
   To display the data: Add Header columns to the treeview (First column is reserved for the tree).
   Add an ongettext event like this:

   procedure TForm1.DBTreeview1GetText(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
    var CellText: WideString);
   var
    CellData: Variant;
    Data: PDBNodeData;
   begin
    Data := TVirtualDBTreeEx(Sender).GetDBNodeData(Node);
    CellData := Data.DBData;
    case Column of
    // -1,0:   // this is the tree, allready handled
      1:           // second column = first data
        begin
        	if VarIsArray( CellData ) then    // 1 or many fields
        	begin
        		CellText := CellData[0];        // select from array
        	end
        	else
        	begin
        		Celltext := Celldata;           // one and only
        	end;
        			// Eventually format here as you like
        end;
     // 2:         // etc
    end;
   end;

  Possible usefull ToDo's:
  * field properties selectable from dataset with adapted property editor
  * eventually autocreate columns as in dbgrid

 02-JAN-2002 C.S. Phua

  - Modified from work of Vadim Sedulin and Adem Baba. Renamed all VirtualDBTree
    components to VirtualDBTreeEx to preserve the work previously done. Can't
    find a way to make it a descendant of VirtualDBTree though.

  - Changes made:
    * Changed integer type ID and AParent ID to type Double. This is done because
      integer type might not be enough to hold a hugh database's records. With
      Double type, one can store a number greater than zero (>0) up to 15 digits
      without the loss of accuracy in ID field. This gives more room to play with
      ID, for instance, I need to do this in ID:-
      xxxxxyyyyyyyyyy where xxxxx is site ID, yyyyyyyyyy is record ID of each site.
      Note: It still works with integer field as ID and ParentID even the codes
      expecting a Double field.

    * Added an ImgIdxFieldName property that can be assigned an integer field. When
      an ImageList is specified, it will take the ImageList's image as Icon of
      VirtualTree nodes by indexing it with the field specified in ImgIdxFieldName
      property. I need this feature to show different icons of treenodes, based on
      the type of treenodes, i.e. I have field ID, ParentID, NType in my database
      to form a tree.

    * Modified sorting options to enable OnCompareNodes event for custom sorting
      options. The original default sorting codes will be used if user does not
      supply OnCompareNodes event codes.

    * Changed the type name TSimpleData to TDBNodeData and move it to Interface
      section so that user who uses the method GetDBNodeData can type cast the
      pointer returned to TDBNodeData and access its member. Added an integer member
      named ImgIdx in the structure too.

    * Fixed the TreeOptions issue (finally). Now VirtualDBTreeEx user can play with
      settings in TreeOptions :)

    * Lastly, spent 10 minutes to produce new .dcr for VirtualDBTreeEx based on
      .dcr of VirtualDBTree.


 22-OCT-2001 Vadim Sedulin
  - TBaseVirtualDBTree::GoTo TBaseVirtualDBTree.GoToRec
  - TBaseVirtualDBTree::Update TBaseVirtualDBTree.UpdateTree

 23-OCT-2001 Adem Baba
  - I haven't done much other than to reorganize the code so that it is now one
    unit as oppsed to two units it originally was. I believe this is justifiable
    since this code is about 2,000 lines, there is, IMHO, no need to split them.
    Just look at VirtualTrees's line count --easily over 20,000 lines in a single
    unit.

  - I have removed all comments from the original code since they were Cyrillic,
      and they would have lost a lot in translation especially since I do not
      know Russian at all (I am only assuming they were Russian) :-)

  - I have renamed TSimpleVirtualDBTree to TVirtualDBTree. I believe it reflects
    its purpose better.

  - I have also merged the code in TCustomSimpleVirtualDBTree into TBaseVirtualDBTree;
    since everything else is derived from TBaseVirtualDBTree.

  - I got rid of TCheckDataLink, in favor of TVirtualDBTreeDataLink (which is
    renamed from TVTDataLink). There was no need for two descendants of TDataLink.

  - Finally, I have renamed the resultant file VirtualDBTree.

  Things to do:
    - Check to see if we really need these classes separately:
      TCustomCheckVirtualDBTree and TCheckVirtualDBTree. It looks as if they should
      be merged into a single class.

    - DCRs must be designed for
        - TVirtualDBTree,
        - TDBCheckVirtualDBTree,
        - TCheckVirtualDBTree

    - A demo. A demo is badly needed. I hope someone does come along and do it,
      as I am simply hopeless with those things.
 }

Interface

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

Uses
{$IFDEF LCL}
        LCLProc, LCLType, LMessages, LCLIntf, LResources,
        Graphics, Forms, GraphType,
{$ELSE}
  Windows,
  ActiveX,
{$ENDIF}
  Classes,
  Controls,
  Messages,
  DB,
  Variants,
  VirtualTrees, ImgList;

  // Données qui permettent de créer les checks sur les branches à partir du champ CheckFieldName
Const RulerActionSelectAndAction = 0 ;
      RulerActionSelectOnly = 1 ;
      RulerActionSelectAndSelected   = 2 ;
      RulerActionSelectedOnly = 3 ;
      RulerActionUnSelectAndSelected = 4 ;
      RulerActionUnSelectOnly = 5 ;
      RulerActionUnselectAndUnSelected = RulerActionUnSelectOnly + 1 ;
      RulerActionUnselectedOnly    = RulerActionUnSelectOnly + 2 ;
      RulerActionSelectAndUnSelected =  RulerActionUnSelectOnly + 3 ;
      RulerActionMixOnly = 9 ;
      RulerActionMixAndSelects    = RulerActionMixOnly + 1 ;
      RulerActionMixAndSelected   = RulerActionMixOnly + 2 ;
      RulerActionMixAndUnSelected = RulerActionMixOnly + 3 ;
      RulerActionMixAndUnselects  = RulerActionMixOnly + 4 ;
      RulerActionSelectAndMixes   = RulerActionMixOnly + 5 ;
      RulerActionUnselectAndMixes = RulerActionMixOnly + 6 ;
      RulerActionValidateSelectAndAction = 16 ;
      RulerActionValidateSelectOnly 	        = RulerActionValidateSelectAndAction + 1 ;
      RulerActionValidateSelectAndSelected     = RulerActionValidateSelectAndAction + 2 ;
      RulerActionValidateSelectedOnly 	      = RulerActionValidateSelectAndAction + 3 ;
      RulerActionValidateUnSelectAndSelected   = RulerActionValidateSelectAndAction + 4 ;
      RulerActionValidateUnSelectOnly 	      = RulerActionValidateSelectAndAction + 5 ;
      RulerActionValidateUnselectAndUnSelected  = RulerActionValidateUnSelectOnly + 1 ;
      RulerActionValidateUnselectedOnly          = RulerActionValidateUnSelectOnly + 2 ;
      RulerActionValidateSelectAndUnSelected     = RulerActionValidateUnSelectOnly + 3 ;
      RulerActionValidateMixOnly 	        = RulerActionValidateSelectAndAction + 9 ;
      RulerActionValidateMixAndSelects    = RulerActionValidateMixOnly + 1 ;
      RulerActionValidateMixAndSelected   = RulerActionValidateMixOnly + 2 ;
      RulerActionValidateMixAndUnSelected = RulerActionValidateMixOnly + 3 ;
      RulerActionValidateMixAndUnselects  = RulerActionValidateMixOnly + 4 ;
      RulerActionValidateSelectAndMixes   = RulerActionValidateMixOnly + 5 ;
      RulerActionValidateUnselectAndMixes = RulerActionValidateMixOnly + 6 ;
      RulerActionUnValidateSelectAndAction = 32 ;
      RulerActionUnValidateSelectOnly 	        	= RulerActionUnValidateSelectAndAction + 1 ;
      RulerActionUnValidateSelectAndSelected       = RulerActionUnValidateSelectAndAction + 2 ;
      RulerActionUnValidateSelectedOnly   	      = RulerActionUnValidateSelectAndAction + 3 ;
      RulerActionUnValidateUnSelectAndSelected     = RulerActionUnValidateSelectAndAction + 4 ;
      RulerActionUnValidateUnSelectOnly           = RulerActionUnValidateSelectAndAction + 5 ;
      RulerActionUnValidateUnselectAndUnSelected  = RulerActionUnValidateUnSelectOnly + 1 ;
      RulerActionUnValidateUnselectedOnly          = RulerActionUnValidateUnSelectOnly + 2 ;
      RulerActionUnValidateSelectAndUnSelected    = RulerActionUnValidateUnSelectOnly + 3 ;
      RulerActionUnValidateMixOnly 	        = RulerActionUnValidateSelectAndAction + 9 ;
      RulerActionUnValidateMixAndSelects    = RulerActionUnValidateMixOnly + 1 ;
      RulerActionUnValidateMixAndSelected   = RulerActionUnValidateMixOnly + 2 ;
      RulerActionUnValidateMixAndUnSelected = RulerActionUnValidateMixOnly + 3 ;
      RulerActionUnValidateMixAndUnselects  = RulerActionUnValidateMixOnly + 4 ;
      RulerActionUnValidateSelectAndMixes   = RulerActionUnValidateMixOnly + 5 ;
      RulerActionUnValidateUnselectAndMixes = RulerActionUnValidateMixOnly + 6 ;
      RulerActionValidate = 48 ;
      RulerActionUnValidate = RulerActionUnValidateMixOnly + 8 ;
      RulerActionOnlyRevalidate = 243 ;
      RulerActionOnlyRevalidateCanCheckUnSelect = 245 ;
      RulerActionOnlyRevalidateCanCheckSelect = 246 ;
      RulerActionOnlyRevalidateUnSelectCantCheck = 247 ;
      RulerActionOnlyRevalidateSelectCantCheck = 248 ;
      RulerActionOnlyRevalidateUnSelectCanCheck = 249 ;
      RulerActionOnlyRevalidateSelectCanCheck = 250 ;
      RulerActionOnlyRevalidateSetCantCheck = 251 ;
      RulerActionOnlyRevalidateSetCanCheck = 252 ;
      RulerActionOnlyRevalidateUnSelect = 253 ;
      RulerActionOnlyRevalidateSelect = 254 ;
Type
  TDBVTOption = (
    dboAllowChecking,
    // gestion de règles exceptionnelles de checking autmatisées ou orientés
    dboAllowRules,
    dboAutoBmp,
    // gestion de règles de checking type données sur les branches
    dboSpecialDBChecks,
    // héritage du checking du VirtualTree sur les branches
    dboSpecialVTChecks,
    dboAlwaysRule,
    dboAllowStructureChange,
    dboAlwaysStructured,
    dboCheckChildren,
    dboCheckDBStructure,
    dboListView,
    dboParentStructure,
    dboPathStructure,
    dboReadOnly,
    dboShowChecks,
    dboTrackActive,
    dboTrackChanges,
    dboTrackCursor,
    dboViewAll,
    dboWriteLevel,
    dboWriteSecondary
    );
  TDBVTOptions = Set Of TDBVTOption;

  TDBVTStatus = (
    dbtsChanged,
    dbtsChecking,
    dbtsDataChanging,
    dbtsDataOpening,
    dbtsDragDrop,
    dbtsEditing,
    dbtsEmpty,
    dbtsInsert,
    dbtsStructured,
    dbtsToggleAll
    );
  TDBVTStatuses = Set Of TDBVTStatus;

  TDBVTChangeMode = (
    dbcmEdit,
    dbcmInsert,
    dbcmStructure
    );

  TDBVTGoToMode = (
    gtmFromFirst,
    gtmNext,
    gtmPrev
    );

  TDBVTNodeStatus = (
    dbnsDelete,
    dbnsEdit,
    dbnsInited,
    dbnsNew,
    dbnsNone,
    dbnsRefreshed
    );

  PDBVTData = ^TDBVTData;
  TDBVTData = Record
    ID: Variant;
    Level: Integer;
    Status: TDBVTNodeStatus;
    Parent: PVirtualNode;
  End;

  TBaseVirtualDBTreeEx = Class;
  TVirtualDBTreeExDataLink = Class;

  TVTDBOpenQueryEvent = Procedure(Sender: TBaseVirtualDBTreeEx; Var Allow: Boolean) Of Object;
  TVTDBWriteQueryEvent = Procedure(Sender: TBaseVirtualDBTreeEx; Node: PVirtualNode; Column: TColumnIndex; ChangeMode: TDBVTChangeMode; Var Allow: Boolean) Of Object;
  TVTNodeDataChangedEvent = Procedure(Sender: TBaseVirtualDBTreeEx; Node: PVirtualNode; Field: TField; Var UpdateNode: Boolean) Of Object;
  TVTNodeFromDBEvent = Procedure(Sender: TBaseVirtualDBTreeEx; Node: PVirtualNode) Of Object;
  TVTPathToDBEvent = Procedure(Sender: TBaseVirtualDBTreeEx; Var Path: String) Of Object;

  TVirtualDBTreeExDataLink = Class(TDataLink)
  Private
    FVirtualDBTreeEx: TBaseVirtualDBTreeEx;
  Public
    Constructor Create(ATree: TBaseVirtualDBTreeEx); Virtual;
  Protected
    Procedure ActiveChanged; Override;
    Procedure DataSetChanged; Override;
    Procedure DataSetScrolled(Distance: Integer); Override;
    Procedure EditingChanged; Override;
    Procedure RecordChanged(Field: TField); Override;
  End;

  TDBNodeData = Record
    RulerCondition : Variant ;
    RulerSelection : Variant ;
    RulerResult    : Variant ;
    CanCheck : Boolean ;
    RulingState : TCheckState ;
    Text: WideString;
    ImgIdx: Integer;
    StImgIdx: Integer;
    DBData: Variant;
  End;
  PDBNodeData = ^TDBNodeData;

  TBaseVirtualDBTreeEx = Class(TCustomVirtualStringTree)
  Private
    // Rajouté format de date
//    FDateTimeFormat : String ;
    FHitNode: PVirtualNode;                    // node which "captures" an check event
    FCurID: Variant;
    FDBDataSize: Integer;
    FDBOptions: TDBVTOptions;
    FDBStatus: TDBVTStatuses;
    FDataLink: TVirtualDBTreeExDataLink;
    FKeyField: TField;
    FCheckFieldName ,
    FKeyFieldName: String;
    FLevelField: TField;
    FLevelFieldName: String;
    FNodeLines : Word ;
    FMaxLevel: Integer;
    FRulerChecks : TVTChangeEvent ;
    FOnNodeDataChanged: TVTNodeDataChangedEvent;
    FOnOpeningDataSet: TVTDBOpenQueryEvent;
    FOnReadNodeFromDB: TVTNodeFromDBEvent;
    FOnReadPathFromDB: TVTPathToDBEvent;
    FOnWritePathToDB: TVTPathToDBEvent;
    FOnWritingDataSet: TVTDBWriteQueryEvent;
    FParentField: TFields;
    FParentFieldName: String;
    STLParentFields : TStringList ;
    FPathField: TField;
    FPathFieldName: String;
    FViewField: TField;
    FViewFieldName: String;
    FImgIdxField: TField;
    FImgIdxFieldName: String;
    FStImgIdxField: TField;
    FSTImgIdxFieldName: String;
    // Matthieu Giroux
    FFormatIfEmpty : Boolean ;
    FDBDataFieldNames: String;
    // MAJ 12-10-2004 : Règles Rules
    FDatasourceRuleSelect ,
    FDatasourceRules : TDatasource ;
//    FDatasourceFilterRuler ,
//    FDatasourceRulerFilter ,
    FRulerConditionField   ,
    FRulerSelectionField   ,
    FRulerActionField      : String ;
//    FRules                 : TStringList ;
    procedure WMLButtonUp(var Message: {$IFDEF LCL}TLMLButtonUp); message LM_LBUTTONUP;{$ELSE}TWMLButtonUp); message WM_LBUTTONUP;{$ENDIF}
    procedure WMLButtonDown(var Message: {$IFDEF LCL}TLMLButtonDown); message LM_LBUTTONDOWN;{$ELSE}TWMLButtonDown); message WM_LBUTTONDOWN;{$ENDIF}
    procedure WMKeyUp(var Message: {$IFDEF LCL}TLMKeyUp); message LM_KEYUP;{$ELSE}TWMKeyUp); message WM_KEYUP;{$ENDIF}
    procedure WMKeyDown(var Message: {$IFDEF LCL}TLMKeyDown); message LM_KEYDOWN;{$ELSE}TWMKeyDown); message WM_KEYDOWN;{$ENDIF}

//    procedure SetRules ( const Value : TstringList );
    Function GetDBNodeDataSize: Integer;
    Function GetDBOptions: TDBVTOptions;
    Function GetDBStatus: TDBVTStatuses;
    Function GetDataSource: TDataSource;
    Procedure RefreshListNode;
    Procedure RefreshNodeByParent;
    Procedure RefreshNodeByPath;
    Procedure SetDBNodeDataSize(Value: Integer);
    Procedure SetDBOptions(Value: TDBVTOptions);
    Procedure SetDataSource(Value: TDataSource);
    Procedure SetKeyFieldName(Const Value: String);
    Procedure SetLevelFieldName(Const Value: String);
    Procedure SetParentFieldName(Const Value: String);
    Procedure SetPathFieldName(Const Value: String);
    Procedure SetViewFieldName(Const Value: String);
    Procedure SetImgIdxFieldName(Const Value: String);
    Procedure SetStImgIdxFieldName(Const Value: String);
    Procedure SetDBDataFieldNames(Const Value: String);
    Function GetNodeText( Node: PVirtualNode): WideString; virtual ;
    function DBChangeCheckState(const Node: PVirtualNode; Value: TCheckState): Boolean;
    Procedure p_SetDatasourceRuler ( Value : TDatasource );
    Procedure p_SetDatasourceRuleSelect ( Value : TDatasource );
    function CanSelectData(const RulerSelection : Variant ;
      const Condition: Variant): Boolean;
    procedure SetCheckFieldValues(const Node: PVirtualNode;
      const ai_Selection: Integer; const ab_CanCheck: Boolean);
    procedure CheckANodeFromRule(const Node: PVirtualNode;
      const adbn_DBDataNode: PDBNodeData;
      const NewCheckState: TCheckState);
    procedure SetCanCheck(const Node: PVirtualNode;
      const adbn_DBDataNode: PDBNodeData; const ab_Valeur: Boolean);

  Protected
    // Matthieu Giroux
    FChangeCheckState,
    FChangeCheckIfCanCheck,
    FChangeCheckDontSelect,
    FChangeCheckDontCanCheck,
    FChangeCheckDontCantCheck,
    FChangeCheckDontUnSelect,
    FChangeCheckRevalidate,
    FChangeStateParent,
    FCheckEvent ,
    FCheckingRules : Boolean ;
    Procedure SetParentField; virtual;
    procedure ExecuteCheckEvent; virtual ;
    function DoChecks ( const Node: PVirtualNode ; const adbn_DBDataParent, adbn_DBDataNode: PDBNodeData ;  const Demand : Integer ): Boolean;
    function DoRuleCheckAllActions(const SourceNodeExcluded, Node: PVirtualNode;
      const adbn_DBData: PDBNodeData; Demand : Integer ): Boolean;
    function DoRuleCheckAction(const SourceNodeExcluded, Node: PVirtualNode;
      const adbn_DBData: PDBNodeData; Demand : Integer ; const Condition : Variant ): Boolean;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;

    Function CanEdit ( Node: PVirtualNode; Column: TColumnIndex): Boolean; Override;
    Function CanOpenDataSet: Boolean; Virtual;
    Function CanWriteToDataSet(const Node: PVirtualNode; const Column: TColumnIndex; const ChangeMode: TDBVTChangeMode): Boolean; Virtual;
    Function DoChecking ( Node: PVirtualNode; Var NewCheckState: TCheckState): Boolean; Override;
    Function FindChild(const Node: PVirtualNode; ID: Variant): PVirtualNode;
    Function FindNode(const Start: PVirtualNode; ID: Variant): PVirtualNode;
    Function HasVisibleChildren( Node: PVirtualNode): Boolean;
    Procedure DataLinkActiveChanged; Virtual;
    Procedure DataLinkChanged; Virtual;
    Procedure DataLinkEditingChanged; Virtual;
    Procedure DataLinkRecordChanged(Field: TField); Virtual;
    Procedure DataLinkScrolled; Virtual;
    Procedure DoChecked( Node: PVirtualNode); Override;
    Procedure DoCollapsed(Node: PVirtualNode); Override;
              {$IFDEF LCL}
              {$ELSE}
    Procedure DoDragDrop(Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; Var Effect: Integer; Mode: TDropMode); Override;
              {$ENDIF}
    Procedure DoEdit; Override;
    Procedure DoFocusChange(Node: PVirtualNode; Column: TColumnIndex); Override;
    Procedure DoFreeNode(Node: PVirtualNode); Override;
    Procedure DoInitNode(AParent, Node: PVirtualNode; Var InitStates: TVirtualNodeInitStates); Override;
    Procedure DoNodeDataChanged(const Node: PVirtualNode; const Field: TField; Var UpdateNode: Boolean); Virtual;
    Procedure DoNodeMoved(Node: PVirtualNode); Override;
    Procedure SetParentValue(const ParentID: Variant); virtual;
    Function  GetParentField(const Last : Integer ):TField; virtual;
    Procedure DoOpeningDataSet(Var Allow: Boolean); Virtual;
    Procedure DoReadNodeFromDB(const Node: PVirtualNode); Virtual;
    Procedure DoReadPathFromDB(Var APath: String); Virtual;
    Procedure DoWritePathToDB(Var APath: String); Virtual;
    Procedure DoWritingDataSet(const Node: PVirtualNode; const Column: TColumnIndex; const ChangeMode: TDBVTChangeMode; Var Allow: Boolean); Virtual;
    Procedure InitFields; Virtual;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); Override;
    Procedure ReadNodeFromDB(Node: PVirtualNode); Virtual;
    Procedure RefreshNode; Virtual;
    Procedure RefreshNodes;
    Procedure ResetFields; Virtual;
    Procedure SetFocusToNode(Node: PVirtualNode);
    Procedure ToggleListView;
    Procedure ToggleViewMode;
    function GetOptions: TStringTreeOptions;
    procedure SetOptions(const AValue: TStringTreeOptions);
    function GetOptionsClass: TTreeOptionsClass; override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
  {$IFDEF LCL}
  {$ELSE}
    Procedure OnDragOverHandler(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; Var Effect: Integer; Var Accept: Boolean);
  {$ENDIF}
      // Matthieu giroux
    procedure CheckField ( const Node : PVirtualNode );
    procedure Rule ( const Node: PVirtualNode; const NewCheckState: TCheckState); dynamic;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var AText: String); Override;
  Public
    function DoRuleAction ( const ExcludedSourceNode : PVirtualNode ; const SourceActions : PDBNodeData  ) : Boolean ;
    // Intégrité avec VirtualTreeView de 01-2005
    Function DoCancelEdit: Boolean; Override;
    Function DoEndEdit: Boolean; Override;

    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
      // Matthieu giroux : gestion de check in ordonnée
    procedure DoCheckClick( Node: PVirtualNode; NewCheckState: TCheckState); override ;
    Function GetDBNodeData(Node: PVirtualNode): Pointer;
    Function GoToRec(AID: Variant): Boolean; Overload;
    Procedure AddNode(AParent: PVirtualNode);
    Procedure CheckAllChildren(Node: PVirtualNode);
    Procedure CollapseAll;
    Procedure DeleteSelection;
    Procedure ExpandAll;
    Procedure GoToRec(AString: String; AMode: TDBVTGoToMode); Overload;
    Procedure UnCheckAll(Node: PVirtualNode; OnlyChildren: Boolean);
    Procedure UpdateTree;
    Property DBNodeDataSize: Integer Read GetDBNodeDataSize Write SetDBNodeDataSize;
    Property DBStatus: TDBVTStatuses Read GetDBStatus;
    Property KeyField: TField Read FKeyField;
    Property LevelField: TField Read FLevelField;
    Property OnNodeDataChanged: TVTNodeDataChangedEvent Read FOnNodeDataChanged Write FOnNodeDataChanged;
    Property OnReadNodeFromDB: TVTNodeFromDBEvent Read FOnReadNodeFromDB Write FOnReadNodeFromDB;
    Property ParentField: TFields Read FParentField;
    Property PathField: TField Read FPathField;
    Property ViewField: TField Read FViewField;
    Property ImgIdxField: TField Read FImgIdxField;
    Property StateImgIdxField: TField Read FStImgIdxField;
    property Canvas;
    Property CheckFieldName : String Read FCheckFieldName  Write FCheckFieldName;
//    Property Rules : TstringList Read FRules write SetRules ;
    property Ruling : Boolean read FCheckingRules ;
    property RuleApplied : Boolean read FCheckEvent ;
    property ChangeAll : Boolean read FChangeCheckState write FChangeCheckState ;
    property HotNode : PVirtualNode read FHitNode ;

  Published {Since all these properties are published for all descendants, we might as well publish them here and save whitespace}
    {$IFDEF COMPILER_5_UP}
    Property OnContextPopup;
    {$ENDIF COMPILER_5_UP}
    // property Action;
    Property Align;
    Property Alignment;
    Property Anchors;
    Property AnimationDuration;
    Property AutoExpandDelay;
    Property AutoScrollDelay;
    Property AutoScrollInterval;
    Property Background;
    Property BackgroundOffsetX;
    Property BackgroundOffsetY;
    Property DragHeight;
    Property DragImageKind;
    Property DragKind;
    Property DragMode;
    Property DragOperations;
    Property DragType;
    Property DragWidth;
    Property OnCreateDataObject;
    Property OnCreateDragManager;
    Property OnDragAllowed;
    property OnDragOver;
    Property OnDragDrop;
    Property OnHeaderDragged;
    Property OnHeaderDragging;
    Property OnGetUserClipboardFormats;
    Property OnRenderOLEData;
{$IFDEF LCL}
{$ELSE}
    Property BevelEdges;
    Property BevelInner;
    Property BevelKind;
    Property BevelOuter;
    Property BevelWidth;
    Property Ctl3D;
    Property HintAnimation;
    Property ParentCtl3D;
{$ENDIF}
    Property BiDiMode;
    Property BorderStyle;
    Property BorderWidth;
    Property ButtonFillMode;
    Property ButtonStyle;
    Property ChangeDelay;
    Property CheckImageKind;
    Property ClipboardFormats;
    Property Color;
    Property Colors;
    Property Constraints;
    Property CustomCheckImages;
    Property DBOptions: TDBVTOptions Read GetDBOptions Write SetDBOptions;
    Property DataSource: TDataSource Read GetDataSource Write SetDataSource;
    Property DefaultNodeHeight;
    Property DefaultPasteMode;
    Property DefaultText;
    property DragCursor;
    Property DrawSelectionMode;
    Property EditDelay;
    Property Enabled;
    Property Font;
    Property Header;
    Property HintMode;
    Property HotCursor;
    Property Images;
    Property IncrementalSearch;
    Property IncrementalSearchDirection;
    Property IncrementalSearchStart;
    Property IncrementalSearchTimeout;
    Property Indent;
    Property KeyFieldName: String Read FKeyFieldName Write SetKeyFieldName;
    Property LevelFieldName: String Read FLevelFieldName Write SetLevelFieldName;
    Property LineMode;
    Property LineStyle;
    Property Margin;
    Property NodeAlignment;
    Property NodeLines : Word read FNodeLines write FNodeLines stored True default 1 ;
    Property OnAfterCellPaint;
    Property OnAfterItemErase;
    Property OnAfterItemPaint;
    Property OnAfterPaint;
    Property OnBeforeCellPaint;
    Property OnBeforeItemErase;
    Property OnBeforeItemPaint;
    Property OnBeforePaint;
    Property OnChange;
    Property OnChecked;
    Property OnChecking;
    Property OnClick;
    Property OnCollapsed;
    Property OnCollapsing;
    Property OnColumnClick;
    Property OnColumnDblClick;
    Property OnColumnResize;
    Property OnCompareNodes;
    {$ifdef COMPILER_5_UP}
      property OnContextPopup;
    {$endif COMPILER_5_UP}
    Property OnCreateEditor;
    Property OnDblClick;
    Property OnEditCancelled;
    Property OnEdited;
    Property OnEditing;
    Property OnEndDock;
    Property OnEndDrag;
    Property OnEnter;
    Property OnExit;
    Property OnExpanded;
    Property OnExpanding;
    Property OnFocusChanged;
    Property OnFocusChanging;
    Property OnFreeNode;
    property OnGetCursor;
    property OnGetHeaderCursor;
    Property OnGetHelpContext;
    Property OnGetHint;
    Property OnGetImageIndex;
    Property OnGetLineStyle;
    Property OnGetPopupMenu;
    Property OnGetText;
    Property OnHeaderClick;
    Property OnHeaderDblClick;
    Property OnHeaderDraw;
    Property OnHeaderMouseDown;
    Property OnHeaderMouseMove;
    Property OnHeaderMouseUp;
    Property OnHotChange;
    Property OnIncrementalSearch;
    Property OnInitNode;
    Property OnKeyAction;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnLoadNode;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnNewText;
    property OnNodeCopied;
    property OnNodeCopying;
    property OnNodeMoved;
    Property OnOpeningDataSet: TVTDBOpenQueryEvent Read FOnOpeningDataSet Write FOnOpeningDataSet;
    Property OnPaintBackground;
    Property OnPaintText;
    Property OnReadPathFromDB: TVTPathToDBEvent Read FOnReadPathFromDB Write FOnReadPathFromDB;
    Property OnResetNode;
    Property OnResize;
    Property OnSaveNode;
    Property OnScroll;
    Property OnShortenString;
    Property OnStartDock;
    Property OnStartDrag;
    property OnStructureChange;
    Property OnUpdating;
    Property OnWritePathToDB: TVTPathToDBEvent Read FOnWritePathToDB Write FOnWritePathToDB;
    Property OnWritingDataSet: TVTDBWriteQueryEvent Read FOnWritingDataSet Write FOnWritingDataSet;
    Property ParentBiDiMode;
    Property ParentColor Default False;
    Property ParentFieldName: String Read FParentFieldName Write SetParentFieldName;
    Property ParentFont;
    Property ParentShowHint;
    Property PathFieldName: String Read FPathFieldName Write SetPathFieldName;
    Property PopupMenu;
    Property ScrollBarOptions;
    Property SelectionCurveRadius;
    Property ShowHint;
    Property StateImages;
    Property TabOrder;
    Property TabStop Default True;
    Property TextMargin;
    property TreeOptions: TStringTreeOptions read GetOptions write SetOptions;
    Property ViewFieldName: String Read FViewFieldName Write SetViewFieldName;
    Property ImgIdxFieldName: String Read FImgIdxFieldName Write SetImgIdxFieldName;
    Property StateImgIdxFieldName: String Read FStImgIdxFieldName Write SetStImgIdxFieldName;
    Property DBDataFieldNames: String Read FDBDataFieldNames Write SetDBDataFieldNames;
    Property Visible;
    Property WantTabs;
    // 2004-09 Matthieu Giroux
    Property FormatIfEmpty        : Boolean read FFormatIfEmpty write FFormatIfEmpty default False ;
    Property RulerConditionField  : String read FRulerConditionField write FRulerConditionField;
    Property RulerSelectionField  : String read FRulerSelectionField write FRulerSelectionField;
    Property RulerActionField     : String read FRulerActionField write FRulerActionField;
    Property RulerChecks          : TVTChangeEvent read FRulerChecks write FRulerChecks ;
    Property DatasourceRules		  : TDatasource read FDatasourceRules write p_SetDatasourceRuler ;
    Property DatasourceRuleSelect	: TDatasource read FDatasourceRuleSelect write p_SetDatasourceRuleSelect ;
    //rajouté
//    Property DateTimeFormat : String read FDateTimeFormat write FDateTimeFormat stored True ;
  End;

  TVirtualDBTreeEx = Class(TBaseVirtualDBTreeEx)
  Private
    Function GetNodeText( Node: PVirtualNode): WideString; override ;
    Procedure SetNodeText(Node: PVirtualNode; Const Value: WideString);
  Protected
    Function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; Override;
    Procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; const AText: String); Override;
    Procedure DoNodeDataChanged(const Node: PVirtualNode; const Field: TField; Var UpdateNode: Boolean); Override;
    Procedure DoReadNodeFromDB(const Node: PVirtualNode); Override;
    Procedure DoWritingDataSet(const Node: PVirtualNode; const Column: TColumnIndex; const ChangeMode: TDBVTChangeMode; Var Allow: Boolean); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Property Canvas;
    Property NodeText[Node: PVirtualNode]: WideString Read GetNodeText Write SetNodeText;
  Published
    // 2004-09 Matthieu Giroux
    Property OnReadNodeFromDB ;
    Property OnNodeDataChanged ;
  End;

  TCustomDBCheckVirtualDBTreeEx = Class(TBaseVirtualDBTreeEx)
  Private
    FCheckDataLink: TVirtualDBTreeExDataLink;
    FResultField: TField;
    FResultKeyFieldName,
    FResultFieldName: String;
    Function GetCheckDataSource: TDataSource;
    Procedure SetCheckDataSource ( const Value: TDataSource);
    Procedure SetResultFieldName ( Const Value: String);
    // 2004-09 Matthieu Giroux
    Function GetCheckList: TStringList;
  Protected
    Function DoChecking(Node: PVirtualNode; Var NewCheckState: TCheckState): Boolean; Override;
    Procedure CheckDataLinkActiveChanged; Virtual;
    Procedure DoChecked(Node: PVirtualNode); Override;
    Procedure DoOpeningDataSet(Var Allow: Boolean); Override;
    Procedure DoReadNodeFromDB(const Node: PVirtualNode); Override;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Property ResultField: TField Read FResultField;
    Property CheckDataSource: TDataSource Read GetCheckDataSource Write SetCheckDataSource;
    Property ResultFieldName: String Read FResultFieldName Write SetResultFieldName;
    Property ResultKeyFieldName: String Read FResultKeyFieldName Write FResultKeyFieldName;
    // 2004-09 Matthieu Giroux
    Property CheckList: TStringList Read GetCheckList;
//    property InsertOnCheck : Boolean Read FInsertOnCheck Write FInsertOnCheck ;
  End;

  TDBCheckVirtualDBTreeEx = Class(TCustomDBCheckVirtualDBTreeEx)
  Private
    Function GetNodeText(Node: PVirtualNode): WideString; override ;
    Procedure SetNodeText(Node: PVirtualNode; Const AValue: WideString);
  Protected
    Function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; Override;
//    Procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; Var Text: WideString); Override;
    Procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; const AText: String); Override;
    Procedure DoNodeDataChanged(const Node: PVirtualNode; const Field: TField; Var UpdateNode: Boolean); Override;
    Procedure DoReadNodeFromDB(const Node: PVirtualNode); Override;
    Procedure DoWritingDataSet(const Node: PVirtualNode; const Column: TColumnIndex; const ChangeMode: TDBVTChangeMode; Var Allow: Boolean); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Property Canvas;
    Property NodeText[Node: PVirtualNode]: WideString Read GetNodeText Write SetNodeText;
  Published
    Property CheckDataSource;
    Property ResultFieldName;
    Property ResultKeyFieldName;
    Property CheckFieldName ;
    Property OnReadNodeFromDB ;
    Property OnNodeDataChanged ;
  End;

  TCustomCheckVirtualDBTreeEx = Class(TBaseVirtualDBTreeEx)
  Private
    FList: TStringList;
    Function GetCheckList: TStringList;
    Procedure SetCheckList(Value: TStringList);
  Protected
    Procedure DoChecked(Node: PVirtualNode); Override;
    Procedure DoReadNodeFromDB(const Node: PVirtualNode); Override;
  Public
    procedure ClearChecks;
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Property CheckList: TStringList Read GetCheckList Write SetCheckList;
  End;

  TCheckVirtualDBTreeEx = Class(TCustomCheckVirtualDBTreeEx)
  Private
    Function GetNodeText(Node: PVirtualNode): WideString; override ;
    Procedure SetNodeText(Node: PVirtualNode; Const Value: WideString);
  Protected
    Function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; Override;
//    Procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; Var Text: WideString); Override;
    Procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; const AText: String); Override;
    Procedure DoNodeDataChanged(const Node: PVirtualNode; const Field: TField; Var UpdateNode: Boolean); Override;
    Procedure DoReadNodeFromDB(const Node: PVirtualNode); Override;
    Procedure DoWritingDataSet(const Node: PVirtualNode; const Column: TColumnIndex; const ChangeMode: TDBVTChangeMode; Var Allow: Boolean); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Property Canvas;
    Property NodeText[Node: PVirtualNode]: WideString Read GetNodeText Write SetNodeText;
  Published
    Property CheckFieldName ;
    Property OnReadNodeFromDB ;
    Property OnNodeDataChanged ;
  End;

Implementation

Uses
  SysUtils,
  Dialogs,
  Math;

const
  UnpressedState: array[TCheckState] of TCheckState = (
    csUncheckedNormal, csUncheckedNormal, csCheckedNormal, csCheckedNormal, csMixedNormal, csMixedNormal
  );

Type
  THackedTreeOptions = Class(TCustomVirtualTreeOptions);

  {------------------------------------------------------------------------------}

function StrTotStringlist(const str: string): TStringList;
 var
   tstr: TStringList;
 begin
   tstr := TStringList.Create;
   tstr.Text := StringReplace(str, ';', sLineBreak, [rfReplaceAll]);
  Result := tstr;
end;

Constructor TVirtualDBTreeExDataLink.Create(ATree: TBaseVirtualDBTreeEx);
Begin
  Inherited Create;
  FVirtualDBTreeEx := ATree;
End;

Procedure TVirtualDBTreeExDataLink.ActiveChanged;
Begin
  if (  Dataset.Active )
  and assigned ( Dataset.AfterOpen )
    Then Dataset.AfterOpen ( Dataset );
  FVirtualDBTreeEx.DataLinkActiveChanged ;
End;

Procedure TVirtualDBTreeExDataLink.DataSetScrolled(Distance: Integer);
Begin
  FVirtualDBTreeEx.DataLinkScrolled;
End;

Procedure TVirtualDBTreeExDataLink.DataSetChanged;
Begin
  FVirtualDBTreeEx.DataLinkChanged;
End;

Procedure TVirtualDBTreeExDataLink.RecordChanged(Field: TField);
Begin
  FVirtualDBTreeEx.DataLinkRecordChanged(Field);
End;

Procedure TVirtualDBTreeExDataLink.EditingChanged;
Begin
  FVirtualDBTreeEx.DataLinkEditingChanged;
End;

{------------------------------------------------------------------------------}

Constructor TBaseVirtualDBTreeEx.Create(AOwner: TComponent);
Begin
  FNodeLines := 1;
  STLParentFields := nil ;
  FFormatIfEmpty := False ;
  Inherited;
  FDBOptions := [dboCheckDBStructure, dboParentStructure, dboWriteLevel, dboAutoBmp, dboWriteSecondary, dboTrackActive, dboTrackChanges, dboTrackCursor, dboAlwaysStructured, dboViewAll];
  FHitNode := nil ;
  FCheckEvent := False ;
  FChangeStateParent := False ;
  FChangeCheckState := False ;
  FChangeCheckDontSelect := False ;
  FChangeCheckDontCancheck := False ;
  FChangeCheckDontCantcheck := False ;
  FChangeCheckDontUnSelect := False ;
  FChangeCheckIfCanCheck := False ;
  FChangeCheckRevalidate := False ;
  FCheckingRules := False ;
  FDataLink := TVirtualDBTreeExDataLink.Create(Self);
  FDBDataSize := sizeof(TDBVTData);
  NodeDataSize := FDBDataSize;
{$IFDEF LCL}
{$ELSE}
  OnDragOver := OnDragOverHandler;
{$ENDIF}
  with TStringTreeOptions(inherited TreeOptions) do
    begin
      PaintOptions := PaintOptions + [toShowHorzGridLines,toShowTreeLines, toShowVertGridLines,toShowRoot];
      MiscOptions := MiscOptions + [toGridExtensions];
    end;
End;

Destructor TBaseVirtualDBTreeEx.Destroy;
Begin
  STLParentFields.Free ;
  FDataLink.Free;
  Inherited;
End;

Procedure TBaseVirtualDBTreeEx.SetDataSource(Value: TDataSource);
Begin
  FDataLink.DataSource := Value;
  If Assigned(Value) Then Value.FreeNotification(Self);
End;

Function TBaseVirtualDBTreeEx.GetDataSource: TDataSource;
Begin
  Result := FDataLink.DataSource;
End;

Procedure TBaseVirtualDBTreeEx.Notification(AComponent: TComponent; Operation: TOperation);
Begin
  Inherited;
  If (Operation = opRemove) And Assigned(FDataLink) And (AComponent = DataSource) Then DataSource := Nil;
  If (Operation = opRemove) And Assigned(FDatasourceRules) And (AComponent = DatasourceRules) Then
    DatasourceRules := Nil;
  If (Operation = opRemove) And Assigned(FDatasourceRuleSelect) And (AComponent = DatasourceRuleSelect) Then
    DatasourceRuleSelect := Nil;
End;

Procedure TBaseVirtualDBTreeEx.SetKeyFieldName(Const Value: String);
Begin
  If FKeyFieldName <> Value Then Begin
    FKeyFieldName := Value;
    If dboTrackActive In FDBOptions Then DataLinkActiveChanged
    Else Begin
      FDBStatus := FDBStatus + [dbtsChanged];
      FKeyField := Nil;
      If FDataLink.Active And (FKeyFieldName <> '') Then FKeyField := FDataLink.DataSet.FieldByName(FPathFieldName);
    End;
  End;
End;

Procedure TBaseVirtualDBTreeEx.SetParentField;
var li_i : Integer ;
Begin
  if FDataLink.Active And (FParentFieldName <> '') Then
     Begin
       FParentField := TFields.Create(FDataLink.DataSet);
       if pos ( ';', FParentFieldName ) = 0 Then
         Begin
           FParentField.Add ( FDataLink.DataSet.FieldByName(FParentFieldName));
         End
       else
         Begin
           STLParentFields := StrTotStringlist ( FParentFieldName );
           for li_i := 0 to STLParentFields.Count - 1 do
            FParentField.Add ( FDataLink.DataSet.FieldByName(STLParentFields [ li_i ]));
         End;
     End;
End;

Procedure TBaseVirtualDBTreeEx.SetParentFieldName(Const Value: String);
var CreateFields : Boolean ;
Begin
  If (FParentFieldName <> Value) Then Begin
    FParentFieldName := Value;
    FParentField := Nil;
    STLParentFields.Free;
    STLParentFields := nil ;
    CreateFields := False;
    If (dboParentStructure In FDBOptions) And (dboTrackActive In FDBOptions) Then Begin
      If Not (dboListView In FDBOptions) Then DataLinkActiveChanged
      Else If (dboAlwaysStructured In FDBOptions) Then DataLinkActiveChanged
      Else Begin
        FDBStatus := FDBStatus + [dbtsStructured];
        CreateFields := True;
      End;
    End Else Begin
      If Not (dboTrackActive In FDBOptions) Then FDBStatus := FDBStatus + [dbtsChanged];
      CreateFields := True;
    End;
    If CreateFields Then SetParentField;
  End;
End;

Procedure TBaseVirtualDBTreeEx.SetPathFieldName(Const Value: String);
Begin
  If (FPathFieldName <> Value) Then Begin
    FPathFieldName := Value;
    FPathField := Nil;
    If (dboPathStructure In FDBOptions) And (dboTrackActive In FDBOptions) Then Begin
      If Not (dboListView In FDBOptions) Then DataLinkActiveChanged
      Else If (dboAlwaysStructured In FDBOptions) Then DataLinkActiveChanged
      Else Begin
        FDBStatus := FDBStatus + [dbtsStructured];
        If FDataLink.Active And (FPathFieldName <> '') Then FPathField := FDataLink.DataSet.FieldByName(FPathFieldName);
      End;
    End Else Begin
      If Not (dboTrackActive In FDBOptions) Then FDBStatus := FDBStatus + [dbtsChanged];
      If FDataLink.Active And (FPathFieldName <> '') Then FPathField := FDataLink.DataSet.FieldByName(FPathFieldName);
    End;
  End;
End;

Procedure TBaseVirtualDBTreeEx.SetLevelFieldName(Const Value: String);
Begin
  If (FLevelFieldName <> Value) Then Begin
    FLevelFieldName := Value;
    FLevelField := Nil;
    If FDataLink.Active And (FLevelFieldName <> '') Then FLevelField := FDataLink.DataSet.FieldByName(FLevelFieldName);
  End;
End;

Procedure TBaseVirtualDBTreeEx.DataLinkActiveChanged;
Begin
  If Not (csDesigning In ComponentState) Then Begin
    ResetFields;
    If (dboTrackActive In FDBOptions) Then Begin
      If FDataLink.Active
       Then
        InitFields;
      UpdateTree;
    End Else FDBStatus := FDBStatus + [dbtsChanged];
  End;
End;

Procedure TBaseVirtualDBTreeEx.DataLinkScrolled;
Var
  KeyID: Variant;
Begin
  If Not (csDesigning In ComponentState) Then Begin
    If Assigned(FKeyField) And Not (dbtsDataChanging In FDBStatus) And (dboTrackCursor In FDBOptions) Then Begin
      FDBStatus := FDBStatus + [dbtsDataChanging];
      KeyID := FKeyField.Value;
      If (KeyID <> Null) And (KeyID <> FCurID) Then SetFocusToNode(FindNode(Nil, KeyID));
      FDBStatus := FDBStatus - [dbtsDataChanging];
      If Not Assigned(FocusedNode) Then SetFocusToNode(GetFirst);
    End;
  End;
End;

Procedure TBaseVirtualDBTreeEx.DataLinkChanged;
Begin
  If Not (csDesigning In ComponentState) Then Begin
    If Not FDataLink.Editing And Not (dbtsDataChanging In FDBStatus) Then Begin
      If (dboTrackChanges In FDBOptions) Then RefreshNodes
      Else FDBStatus := FDBStatus + [dbtsChanged];
    End;
  End;
End;

Procedure TBaseVirtualDBTreeEx.DataLinkRecordChanged(Field: TField);
Var
  UpdateField: Boolean;
  Node: PVirtualNode;
Begin
  If Not (csDesigning In ComponentState) Then Begin
    If Assigned(Field) And (dboTrackChanges In FDBOptions) Then Begin
      UpdateField := False;
      If dboTrackCursor In FDBOptions Then Node := FocusedNode
      Else Node := FindNode(Nil, FKeyField.Value);
      If Assigned(Node) Then Begin
        DoNodeDataChanged(Node, Field, UpdateField);
        If UpdateField Then InvalidateNode(Node);
      End;
    End;
  End;
End;

Procedure TBaseVirtualDBTreeEx.DataLinkEditingChanged;
Var
  Data: PDBVTData;
  Node: PVirtualNode;
Begin
  If Not (csDesigning In ComponentState) Then Begin
    If FDataLink.Editing And Not (dbtsEditing In FDBStatus) And Not (dbtsInsert In FDBStatus) Then Begin
      If dboTrackChanges In FDBOptions Then Begin
        If (dboTrackCursor In FDBOptions) And (FDataLink.DataSet.State = dsEdit) Then Begin
          If Assigned(FocusedNode) And (dboTrackCursor In FDBOptions) Then Begin
            Data := GetNodeData(FocusedNode);
            Data^.Status := dbnsEdit;
          End;
        End Else If FDataLink.DataSet.State = dsInsert Then Begin
          Node := AddChild(Nil);
          ValidateNode(Node, False);
          Data := GetNodeData(Node);
          Data^.ID := Null;
        	Data^.Level := 0;
        	Data^.Parent := Nil;
          Data^.Status := dbnsInited;
          ReadNodeFromDB(Node);
          Data^.Status := dbnsNew;
          If (dboTrackCursor In FDBOptions) Then SetFocusToNode(Node);
        End;
      End Else FDBStatus := FDBStatus + [dbtsChanged];
    End;
    If Assigned(FocusedNode) And (dboTrackChanges In FDBOptions) And (dboTrackCursor In FDBOptions) Then InvalidateNode(FocusedNode);
  End;
End;

Procedure TBaseVirtualDBTreeEx.DoFocusChange(Node: PVirtualNode; Column: TColumnIndex);
Var
  Data: PDBVTData;
Begin
  If Assigned(Node) Then Begin
    Data := GetNodeData(Node);
    If Data^.ID <> FCurID Then Begin
      FCurID := Data^.ID;
      If (FCurID <> Null) And Not (dbtsDataChanging In FDBStatus) And (dboTrackCursor In FDBOptions) Then Begin
        FDBStatus := FDBStatus + [dbtsDataChanging];
        FDataLink.DataSet.Locate(FKeyFieldName, Data^.ID, []);
        FDBStatus := FDBStatus - [dbtsDataChanging];
      End;
    End;
  End;
  Inherited;
End;

Function TBaseVirtualDBTreeEx.CanEdit ( Node: PVirtualNode; Column: TColumnIndex): Boolean;
Begin
  If Not (dbtsEditing In FDBStatus) And Not (dbtsInsert In FDBStatus) Then Result := CanWriteToDataSet(Node, Column, dbcmEdit)
  Else Result := True;
End;

Procedure TBaseVirtualDBTreeEx.DoEdit;
Var
  Data: PDBVTData;
Begin
  Inherited;
  If IsEditing Then Begin
    Data := GetNodeData(FocusedNode);
    If Data^.Status = dbnsEdit Then FDBStatus := FDBStatus + [dbtsEditing]
    Else If Data^.Status = dbnsNew Then FDBStatus := FDBStatus + [dbtsInsert]
    Else If Not (dbtsInsert In FDBStatus) Then Begin
      FDBStatus := FDBStatus + [dbtsEditing];
      FDataLink.DataSet.Edit;
    End;
  End;
End;

Function TBaseVirtualDBTreeEx.DoEndEdit: Boolean;
Var
  Data: PDBVTData;
Begin
  Result := Inherited DoEndEdit;
  If Result Then Begin
    Data := GetNodeData(FocusedNode);
    Data^.Status := dbnsRefreshed;
    If (dbtsEditing In FDBStatus) Then Begin
      FDBStatus := FDBStatus - [dbtsEditing] + [dbtsDataChanging];
      If FDataLink.Editing Then FDataLink.DataSet.Post;
      FDBStatus := FDBStatus - [dbtsDataChanging];
    End Else If (dbtsInsert In FDBStatus) Then Begin
      FDBStatus := FDBStatus - [dbtsInsert] + [dbtsDataChanging];
      If FDataLink.Editing Then FDataLink.DataSet.Post;
      FDBStatus := FDBStatus - [dbtsDataChanging];
      Data^.ID := FKeyField.Value;
      FCurID := Data^.ID;
    End;
  End;
End;

Function TBaseVirtualDBTreeEx.DoCancelEdit: Boolean;
Var
  Data: PDBVTData;
Begin
  If dbtsInsert In FDBStatus Then Begin
    FDBStatus := FDBStatus - [dbtsInsert] + [dbtsDataChanging];
    If FDataLink.Editing Then FDataLink.DataSet.Cancel;
    FDBStatus := FDBStatus - [dbtsDataChanging];
    Result := Inherited DoCancelEdit;
    DeleteNode(FocusedNode);
    DataLinkScrolled;
    Exit;
  End Else If (dbtsEditing In FDBStatus) Then Begin
    FDBStatus := FDBStatus - [dbtsEditing] + [dbtsDataChanging];
    If FDataLink.Editing Then FDataLink.DataSet.Cancel;
    FDBStatus := FDBStatus - [dbtsDataChanging];
    Data := GetNodeData(FocusedNode);
    Data^.Status := dbnsRefreshed;
  End;
  Result := Inherited DoCancelEdit;
End;

Procedure TBaseVirtualDBTreeEx.DoCollapsed(Node: PVirtualNode);
Var
  Focus: PVirtualNode;
Begin
  If Assigned(Node) Then Begin
    If Assigned(FocusedNode) And HasAsParent(FocusedNode, Node) Then Begin
      Focus := Node;
      If Not Selected[Focus] Then Begin
        Focus := GetNextSibling(Node);
        While Assigned(Focus) And Not Selected[Focus] Do Focus := GetNextVisible(Focus);
        If Not Assigned(Focus) Then Begin
          Focus := GetPreviousVisible(Node);
          While Assigned(Focus) And Not Selected[Focus] Do Focus := GetPreviousVisible(Focus);
          If Not Assigned(Focus) Then Focus := Node;
        End;
      End;
      FocusedNode := Focus;
      Selected[Focus] := True;
    End;
    Focus := GetNextSibling(Node);
    If Not Assigned(Focus) Then Begin
      Focus := GetLastChild(Node);
      If Not Assigned(Focus) Then Focus := Node;
      Focus := GetNext(Focus);
    End;
    Node := GetNext(Node);
    While Node <> Focus Do Begin
      Selected[Node] := False;
      Node := GetNext(Node);
    End;
  End;
  Inherited;
End;

{$IFDEF LCL}
{$ELSE}
Procedure TBaseVirtualDBTreeEx.DoDragDrop(Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; Var Effect: Integer; Mode: TDropMode);
Var
  CanProcess: Boolean;
  Focus: PVirtualNode;
Begin
  Effect := DROPEFFECT_MOVE;
  If CanWriteToDataSet(DropTargetNode, 0, dbcmStructure) Then Begin
    CanProcess := True;
    If CanProcess Then Begin
      Focus := FocusedNode;
      BeginUpdate;
      FDataLink.DataSet.DisableControls;
      FDBStatus := FDBStatus + [dbtsDataChanging, dbtsDragDrop];
      ProcessDrop(DataObject, DropTargetNode, Effect, amAddChildLast);
      Effect := DROPEFFECT_LINK;
      FocusedNode := Nil;
      EndUpdate;
      FDataLink.DataSet.EnableControls;
      FDBStatus := FDBStatus - [dbtsDataChanging, dbtsDragDrop];
      FCurID := Null;
      FocusedNode := Focus;
    End Else Effect := DROPEFFECT_NONE;
  End Else Effect := DROPEFFECT_NONE;
  Inherited;
End;

Procedure TBaseVirtualDBTreeEx.OnDragOverHandler(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; Var Effect: Integer; Var Accept: Boolean);
Begin
  Accept := CanWriteToDataSet(DropTargetNode, 0, dbcmStructure);
End;

{$ENDIF}
Procedure TBaseVirtualDBTreeEx.SetParentValue(const ParentID: Variant);
Var
  i: Integer;
  Found : Boolean;
Begin
 if STLParentFields = nil Then
   FParentField.Fields [ 0 ].Value := ParentID
  Else
   Begin
    Found := False ;
    for i := 0 to STLParentFields.Count - 1 do
     if ( FParentField.Fields [ i ].IsNull ) Then
       Begin
         FParentField.Fields [ i ].Value := ParentID;
         Found := True ;
       End;
     if not Found Then
       FParentField.Fields [ 0 ].Value := ParentID;
   End;
End;

Function TBaseVirtualDBTreeEx.GetParentField(const Last : Integer ):TField;
Var
  i : Integer;
Begin
 Result := Nil ;
 if STLParentFields = nil Then
   Begin
     if Last < 0 Then
       Result := FParentField.Fields [ 0 ];
   End
  Else
   Begin
    for i := Last + 1 to STLParentFields.Count - 1 do
     if not FParentField.Fields [ i ].IsNull Then
      Begin
        Result := FParentField.Fields [ i ];
      End;
   End;
End;

Procedure TBaseVirtualDBTreeEx.DoNodeMoved( Node: PVirtualNode);
Var
  Data: PDBVTData;
  APath: String;
  AParent: PVirtualNode;
  ParentID: Variant;
  Level : Integer;
Begin
  If (dbtsDragDrop In FDBStatus) Then Begin
    ParentID := Null;
    Level := 0;
    AParent := Node^.Parent;
    If AParent <> RootNode Then Begin
      Data := GetNodeData(AParent);
      Level := Data^.Level + 1;
      ParentID := Data^.ID;
      If (dboPathStructure In FDBOptions) Or (dboWriteSecondary In FDBOptions) Then Begin
        APath := ParentID.Asstring;
        AParent := AParent^.Parent;
        While AParent <> RootNode Do Begin
        	Data := GetNodeData(AParent);
        	APath := Format('%d.%s', [Data^.ID, APath]);
        	AParent := AParent^.Parent;
        End;
      End;
    End;
    Data := GetNodeData(Node);
    Data^.Level := Level;
    FDataLink.DataSet.Locate(FKeyFieldName, Data^.ID, []);
    FDataLink.DataSet.Edit;
    If (dboPathStructure In FDBOptions) Or (dboWriteSecondary In FDBOptions) Then Begin
      DoWritePathToDB(APath);
      FPathField.Value := APath;
    End;
    If (dboParentStructure In FDBOptions) Or (dboWriteSecondary In FDBOptions) Then Begin
      SetParentValue ( ParentID );
    End;
    If (dboWriteLevel In FDBOptions) Then Begin
      FLevelField.AsInteger := Level;
    End;
    FDataLink.DataSet.Post;
    Inherited;
    Node := GetFirstChild(Node);
    While Assigned(Node) Do Begin
      DoNodeMoved(Node);
      Node := GetNextSibling(Node);
    End;
  End;
End;

Procedure TBaseVirtualDBTreeEx.DoFreeNode(Node: PVirtualNode);
Var
  Data: PDBVTData;
  DBData: PDBNodeData;
Begin
  Data := GetNodeData(Node);
  DBData := GetDBNodeData(Node);
  Data^.ID := Null ;
  DBData^.RulerCondition := Null ;
  DBData^.RulerSelection := Null ;
  DBData^.RulerResult    := Null ;
  If (Data^.Status = dbnsDelete) Then Begin
    If FDataLink.DataSet.Locate(FKeyFieldName, Data^.ID, []) Then FDataLink.DataSet.Delete;
  End;
  Inherited;
End;

Procedure TBaseVirtualDBTreeEx.SetFocusToNode(Node: PVirtualNode);
Begin
  If Assigned(FocusedNode) Then Selected[FocusedNode] := False;
  FocusedNode := Node;
  If Assigned(Node) Then Begin
    Selected[Node] := True;
    FullyVisible[Node] := True;
  End;
End;

Function TBaseVirtualDBTreeEx.FindChild(const Node: PVirtualNode; ID: Variant): PVirtualNode;
Var
  Data: PDBVTData;
Begin
  Result := GetFirstChild(Node);
  While Assigned(Result) Do Begin
    Data := GetNodeData(Result);
    If Data^.ID = ID Then break;
    Result := GetNextSibling(Result);
  End;
End;

Function TBaseVirtualDBTreeEx.FindNode(const Start: PVirtualNode; ID: Variant): PVirtualNode;
Var
  Data: PDBVTData;
Begin
  If Assigned(Start) Then Result := Start
  Else Result := GetFirst;
  While Assigned(Result) Do Begin
    Data := GetNodeData(Result);
    If Data^.ID = ID Then break;
    Result := GetNext(Result);
  End;
End;

Procedure TBaseVirtualDBTreeEx.GoToRec(AString: String; AMode: TDBVTGoToMode);
Var
  AText: String;
  Node: PVirtualNode;
  Column: TColumnIndex;
Begin
  EndEditNode;
  Column := Header.MainColumn;
  Case AMode Of
    gtmFromFirst: Begin
        Node := GetFirst;
        AMode := gtmNext;
      End;
    gtmNext: Node := GetNext(FocusedNode);
    gtmPrev: Node := GetPrevious(FocusedNode);
  Else Node := Nil;
  End;
  While Assigned(Node) Do Begin
    DoGetText(Node, Column, ttNormal, AText);
    If Pos(AString, AText) = 1 Then break;
    If AMode = gtmNext Then Node := GetNext(Node)
    Else Node := GetPrevious(Node);
  End;
  If Assigned(Node) Then SetFocusToNode(Node);
End;

Function TBaseVirtualDBTreeEx.GoToRec(AID: Variant): Boolean;
Var
  Node: PVirtualNode;
Begin
  Node := FindNode(Nil, AID);
  If Assigned(Node) Then SetFocusToNode(Node);
  Result := Node <> Nil;
End;

Procedure TBaseVirtualDBTreeEx.AddNode(AParent: PVirtualNode);
Var
  Level: Integer;
  ParentID: Variant;
  APath: String;
  Node: PVirtualNode;
  Data: PDBVTData;
Begin
  EndEditNode;
  If CanWriteToDataSet(AParent, 0, dbcmInsert) Then Begin
    FDBStatus := FDBStatus + [dbtsDataChanging];
    Node := AddChild(AParent);
    If (AParent = Nil) Or (AParent = RootNode) Then Begin
      Level := 0;
      ParentID := Null;
      APath := '';
    End Else Begin
      Data := GetNodeData(AParent);
      Level := Data^.Level + 1;
      ParentID := Data^.ID;
      If (dboPathStructure In FDBOptions) Or (dboWriteSecondary In FDBOptions) Then Begin
        APath := ParentID.AsString;
        AParent := AParent^.Parent;
        While AParent <> RootNode Do Begin
        	Data := GetNodeData(AParent);
        	APath := Format('%d.%s', [Data^.ID, APath]);
        	AParent := AParent^.Parent;
        End;
      End;
    End;
    Data := GetNodeData(Node);
    Data^.ID := Null;
    Data^.Level := Level;
    Data^.Parent := Nil;
    FDBStatus := FDBStatus + [dbtsInsert];
    FDataLink.DataSet.Insert;
    If Not (dboListView In FDBOptions) Then Begin
      If (dboPathStructure In FDBOptions) Or (dboWriteSecondary In FDBOptions) Then Begin
        DoWritePathToDB(APath);
        FPathField.Value := APath;
      End;
      If (dboParentStructure In FDBOptions) Or (dboWriteSecondary In FDBOptions) Then SetParentValue ( ParentID );
      If (dboWriteLevel In FDBOptions) Then FLevelField.AsInteger := Level;
    End;
    DoReadNodeFromDB(Node);
    FCurID := Null;
    SetFocusToNode(Node);
    FDBStatus := FDBStatus - [dbtsDataChanging];
    EditNode(Node, Header.MainColumn);
  End;
End;

Procedure TBaseVirtualDBTreeEx.DeleteSelection;
Var
  Data: PDBVTData;
  Node: PVirtualNode;
  Temp: PVirtualNode;
  Last: PVirtualNode;
  Focus: PVirtualNode;
Begin
  If Not (dbtsDataChanging In FDBStatus) And Assigned(FocusedNode) And (SelectedCount > 0) And Not (dboReadOnly In FDBOptions) And FDataLink.Active And Assigned(FKeyField) And Not FDataLink.ReadOnly Then Begin
    Node := GetFirst;
    Focus := FocusedNode;
    While Selected[Focus^.Parent] Do Focus := Focus^.Parent;
    Temp := Focus;
    Repeat
      Focus := GetNextSibling(Focus);
    Until Not Assigned(Focus) Or Not Selected[Focus];
    If Not Assigned(Focus) Then Begin
      Focus := Temp;
      Repeat
        Focus := GetPreviousSibling(Focus);
      Until Not Assigned(Focus) Or Not Selected[Focus];
      If Not Assigned(Focus) Then Focus := Temp^.Parent;
      If Focus = RootNode Then Focus := Nil;
    End;
    FDBStatus := FDBStatus + [dbtsDataChanging];
    BeginUpdate;
    FDataLink.DataSet.DisableControls;
    While Assigned(Node) Do Begin
      If Selected[Node] Then Begin
        Temp := Node;
        Last := GetNextSibling(Node);
        Repeat
          Data := GetNodeData(Temp);
          Data^.Status := dbnsDelete;
          Temp := GetNext(Temp);
        Until Temp = Last;
        If Not Assigned(Temp) And (Node^.Parent <> RootNode) Then Temp := GetNextSibling(Node^.Parent);
        DeleteNode(Node);
        Node := Temp;
      End Else Node := GetNextVisible(Node);
    End;
    FDataLink.DataSet.EnableControls;
    EndUpdate;
    FDBStatus := FDBStatus - [dbtsDataChanging];
    If Assigned(Focus) And (Focus <> RootNode) Then SetFocusToNode(Focus);
  End;
End;

Function TBaseVirtualDBTreeEx.GetDBNodeData(Node: PVirtualNode): Pointer;
Begin
  If Not Assigned(Node) Or (DBNodeDataSize = 0) Then Result := Nil
  Else Result := PChar(GetNodeData(Node)) + FDBDataSize;
End;

Procedure TBaseVirtualDBTreeEx.RefreshNodes;
Var
  Data: PDBVTData;
  Node: PVirtualNode;
  Temp: PVirtualNode;
  I: Integer;
Begin
  If Not (dbtsDataChanging In FDBStatus) And CanOpenDataSet Then Begin
    FDBStatus := FDBStatus + [dbtsDataChanging];
    BeginUpdate;
    FMaxLevel := 0;
    FCurID := Null;
    If (dboAlwaysStructured In FDBOptions) Then Begin
      If Not (dbtsStructured In FDBStatus) Then Clear
      Else If (dboListView In FDBOptions) Then Begin
        FDBOptions := FDBOptions - [dboListView];
        ToggleListView;
        FDBOptions := FDBOptions + [dboListView];
      End;
      FDBStatus := FDBStatus + [dbtsStructured];
    End Else Begin
      If (dboListView In FDBOptions) Then FDBStatus := FDBStatus - [dbtsStructured]
      Else Begin
        If Not (dbtsStructured In FDBStatus) Then Clear;
        FDBStatus := FDBStatus + [dbtsStructured];
      End;
    End;
    Temp := GetFirst;
    If Not Assigned(Temp) Then FDBStatus := FDBStatus + [dbtsEmpty];
    While Assigned(Temp) Do Begin
      Data := GetNodeData(Temp);
      If Data^.Status = dbnsRefreshed Then Data^.Status := dbnsNone;
      Temp := GetNext(Temp);
    End;
    FDataLink.DataSet.DisableControls;
    If Not FDataLink.Dataset.IsEmpty
     Then FCurID := FKeyField.Value;
    I := 0;
    While Not FDataLink.DataSet.EOF Do Begin
      RefreshNode;
      FDataLink.DataSet.Next;
      Inc(I);
    End;
    If (I > 0) Then FDataLink.DataSet.MoveBy(-I);
    I := 0;
    While Not (FDataLink.DataSet.Bof) Do Begin
      RefreshNode;
      FDataLink.DataSet.Prior;
      Inc(I);
    End;
    If (I > 0) Then FDataLink.DataSet.MoveBy(I);
    If (dboTrackCursor In FDBOptions) And Assigned(FocusedNode) Then Begin
      Selected[FocusedNode] := False;
      FocusedNode := Nil;
    End;
    Temp := GetFirst;
    While Assigned(Temp) Do Begin
      Data := GetNodeData(Temp);
      /////////////////////////// Modifs
      if Temp^.Parent = nil
       Then Temp^.Parent := RootNode ;
       ///////////////////////
      If (Data^.Status <> dbnsRefreshed) Then Begin
        Node := GetNextSibling(Temp);
        DeleteNode(Temp);
      End Else Begin
        If (dbtsStructured In FDBStatus) And (dboParentStructure In FDBOptions) Then Begin
        	Data^.Level := GetNodeLevel(Temp);
        	FMaxLevel := Max(FMaxLevel, Data^.Level);
        End;
        If (dboTrackCursor In FDBOptions) And Not Assigned(FocusedNode) And (Data^.ID = FCurID) Then Begin
        	Selected[Temp] := True;
        	FocusedNode := Temp;
        	FullyVisible[Temp] := True;
        End;
        Node := GetNext(Temp);
      End;
      Temp := Node;
    End;
    FDataLink.DataSet.EnableControls;
    FDBStatus := FDBStatus - [dbtsDataChanging, dbtsChanged, dbtsEmpty];
    If (dboAlwaysStructured In FDBOptions) And (dboListView In FDBOptions) Then ToggleListView;
    If Not (dboListView In FDBOptions) And Not (dboViewAll In FDBOptions) And Not (dbtsToggleAll In FDBStatus) Then ToggleViewMode;
    EndUpdate;
  End;
End;

Procedure TBaseVirtualDBTreeEx.RefreshNode;
Begin
  if   assigned ( FDatasourceRules )
  and assigned ( FDatasourceRules.DataSet ) Then
    Begin
      FDatasourceRules.DataSet.Close ;
      FDatasourceRules.DataSet.Open ;
    End ;
  if   assigned ( FDatasourceRuleSelect )
  and assigned ( FDatasourceRuleSelect.DataSet ) Then
    Begin
      FDatasourceRuleSelect.DataSet.Close ;
      FDatasourceRuleSelect.DataSet.Open ;
    End ;
  If Not (dbtsStructured In FDBStatus) Then RefreshListNode
  Else If (dboPathStructure In FDBOptions) Then RefreshNodeByPath
  Else RefreshNodeByParent
End;

Procedure TBaseVirtualDBTreeEx.RefreshListNode;
Var
  Data: PDBVTData;
  Node: PVirtualNode;
  ID: Variant;
Begin
  ID := FKeyField.Value;
  If dbtsEmpty In FDBStatus Then Node := Nil
    Else Node := FindChild(Nil, ID);
  If Not Assigned(Node) Then Begin
    Node := AddChild(Nil);
    ValidateNode(Node, False);
    Data := GetNodeData(Node);
    Data^.ID := ID;
    Data^.Parent := Nil;
    Data^.Status := dbnsInited;
  End;
  ReadNodeFromDB(Node);
End;

Procedure TBaseVirtualDBTreeEx.RefreshNodeByPath;
Var
  Pos: Integer;
  ID: Variant;
  Level: Integer;
  Node: PVirtualNode;
  Last: PVirtualNode;
  Data: PDBVTData;
  Temp: String;
  APath: String;
Begin
  Data := Nil;
  APath := FPathField.AsString;
  Last := RootNode;
  DoReadPathFromDB(APath);
  Temp := FKeyField.Value;
  If APath = '' Then APath := Temp
  Else APath := Format('%s.%s', [APath, Temp]);

  Repeat
    Node := Last;
    Pos := System.Pos('.', APath);
    If (Pos = 0) Then Begin
      Temp := APath;
      Pos := Length(APath);
    End Else Temp := Copy(APath, 1, Pos - 1);

    Try
      ID := Temp;
      Last := FindChild(Node, ID);
    Except
      Exit;
    End;

    If Assigned(Last) Then Delete(APath, 1, Pos);
  Until Not Assigned(Last) Or (APath = '');

  If APath = '' Then Begin
    Node := Last;
    Data := GetNodeData(Node);
  End Else Begin
    If (Node = RootNode) Then Level := -1
    Else Begin
      Data := GetNodeData(Node);
      Level := Data^.Level;
    End;

    Repeat
      Pos := System.Pos('.', APath);
      If (Pos = 0) Then Begin
        Temp := APath;
        Pos := Length(APath);
      End Else Temp := Copy(APath, 1, Pos - 1);

      Try
        ID := Temp;
        Node := AddChild(Node);
        ValidateNode(Node, False);
        Data := GetNodeData(Node);
        Inc(Level);
        Data^.ID := ID;
        Data^.Level := Level;
        Data^.Status := dbnsInited;
        Data^.Parent := Nil;
        Delete(APath, 1, Pos);
      Except
        Exit;
      End;
    Until APath = '';
  End;
  If Data <> Nil Then FMaxLevel := Max(FMaxLevel, Data^.Level);
  If Node <> Nil Then ReadNodeFromDB(Node);
End;

Procedure TBaseVirtualDBTreeEx.RefreshNodeByParent;
Var
  ID: Variant;
  ParentID: Variant;
  Data: PDBVTData;
  This: PVirtualNode;
  AParent: PVirtualNode;
  Temp: PVirtualNode;
  Created: Boolean;
  i : Integer ;
  Procedure AddNodeByParent;
  Begin
      If Not Assigned(AParent) Then Begin
        AParent := AddChild(Nil);
        ValidateNode(AParent, False);
        Data := GetNodeData(AParent);
        Data^.ID := ParentID;
        Data^.Status := dbnsInited;
        Data^.Parent := Nil;
      End;
      Created := True;
      If Assigned(This) Then Begin
        If (This^.Parent <> AParent) Then Begin
          If HasAsParent(AParent, This) Then Begin
            Data := GetNodeData(AParent);
            If (Data^.Status = dbnsRefreshed) Then Begin
              Exit;
            End;
            Temp := This;
            This := AParent;
            AParent := Temp;
            Data := GetNodeData(AParent);
            Data^.ID := ParentID;
          End Else Begin
            MoveTo(This, AParent, amAddChildLast, False);
          End;
        End;
      End Else Begin
        Created := False;
        This := AddChild(AParent);
        ValidateNode(This, False);
      End;
      Data := GetNodeData(This);
      Data^.ID := ID;
      If Not Created Then Data^.Status := dbnsInited;
      ReadNodeFromDB(This);
  End;
Begin
  i := -1 ;
  AParent := Nil;
  This := Nil;
  while ( GetParentField ( i ) <> nil )
  and   ( AParent = nil ) do
    Begin
      ID := FKeyField.Value;
      ParentID := GetParentField ( i ).Value;
      If (ID = Null) Then Begin
        Break;
      End;
      Temp := GetFirst;
      If (ParentID = ID) Then Begin
        ParentID := Null;
      End;
      While Assigned(Temp) And (Not Assigned(This) Or Not Assigned(AParent)) Do Begin
        Data := GetNodeData(Temp);
        If (Data^.ID = ID) Then Begin
          If (Data^.Status = dbnsRefreshed) Then Begin
            Break;
          End;
          This := Temp;
        End;
        If (Data^.ID = ParentID) And (ParentID <> Null)
         Then
           Begin
            AParent := Temp;
            if assigned ( This ) Then
              Break;
           End;
        Temp := GetNext(Temp);
      End;
      inc ( i );
    End;
  if AParent = nil then
    AParent := RootNode;

  AddNodeByParent;
End;

Procedure TBaseVirtualDBTreeEx.UpdateTree;
Begin
  Clear;
  RefreshNodes;
End;

Procedure TBaseVirtualDBTreeEx.ToggleListView;
Var
  Data: PDBVTData;
  Node: PVirtualNode;
  Temp: PVirtualNode;
Begin
  If dbtsDragDrop In FDBStatus Then Exit;
  BeginUpdate;
  If (dboListView In FDBOptions) Then Begin
    Node := GetFirst;
    While Assigned(Node) Do Begin
      Data := GetNodeData(Node);
      If (Node^.Parent <> RootNode) Then Begin
        Data^.Parent := Node^.Parent;
        Temp := GetNextSibling(Node);
        If Not Assigned(Temp) Then Begin
          Temp := GetLastChild(Node);
          If Assigned(Temp) Then Begin
            Temp := GetNext(Temp);
            If Not Assigned(Temp) Then Temp := GetNext(Node);
          End Else Temp := GetNext(Node);
        End;
        MoveTo(Node, RootNode, amAddChildLast, False);
        Node := Temp;
      End Else Node := GetNext(Node);
    End;
    If Not (dboViewAll In FDBOptions) Then ToggleViewMode;
  End Else Begin
    Node := GetFirst;
    While Assigned(Node) Do Begin
      Data := GetNodeData(Node);
      If Data^.Parent <> Nil Then Begin
        Temp := GetNextSibling(Node);
        MoveTo(Node, Data^.Parent, amAddChildLast, False);
        Data^.Parent := Nil;
        Node := Temp;
      End Else Node := GetNextSibling(Node);
    End;
    If Not (dboViewAll In FDBOptions) Then Begin
      FDBStatus := FDBStatus + [dbtsToggleAll];
      ToggleViewMode;
      FDBStatus := FDBStatus - [dbtsToggleAll];
    End;
  End;
  If Assigned(FocusedNode) Then FullyVisible[FocusedNode] := True;
  EndUpdate;
End;

Procedure TBaseVirtualDBTreeEx.ResetFields;
Begin
  FKeyField := Nil;
  FParentField := Nil;
  FPathField := Nil;
  FLevelField := Nil;
  FViewField := Nil;
  FImgIdxField := Nil;
  FStImgIdxField := Nil;
End;

Procedure TBaseVirtualDBTreeEx.InitFields;
var
  Pos: Integer;
//  Field: TField;
Begin
  If (FKeyFieldName <> '') Then FKeyField := FDataLink.DataSet.FieldByName(FKeyFieldName);
  If (FParentFieldName <> '') Then SetParentField;
  If (FPathFieldName <> '') Then FPathField := FDataLink.DataSet.FieldByName(FPathFieldName);
  If (FLevelFieldName <> '') Then FLevelField := FDataLink.DataSet.FieldByName(FLevelFieldName);
  If FViewFieldName <> '' Then FViewField := DataSource.DataSet.FieldByName(FViewFieldName);
  If FImgIdxFieldName <> '' Then FImgIdxField := DataSource.DataSet.FieldByName(FImgIdxFieldName);
  If FStImgIdxFieldName <> '' Then FStImgIdxField := DataSource.DataSet.FieldByName(FStImgIdxFieldName);
  If FDBDataFieldNames <> '' Then
  begin
    Pos := 1;
    while Pos <= Length(FDBDataFieldNames) do
    begin            // just checking existence
      DataSource.DataSet.FieldByName(ExtractFieldName(DBDataFieldNames, Pos));
    end;
  end;
End;

Procedure TBaseVirtualDBTreeEx.SetDBNodeDataSize(Value: Integer);
Begin
  If (Value <> DBNodeDataSize) And (Value >= 0) Then Begin
    NodeDataSize := FDBDataSize + Value;
    UpdateTree;
  End;
End;

Function TBaseVirtualDBTreeEx.GetDBNodeDataSize: Integer;
Begin
  Result := NodeDataSize - FDBDataSize;
End;


function TBaseVirtualDBTreeEx.GetNodeText(Node: PVirtualNode): WideString;
begin
  Result := DefaultText ;
end;

procedure TBaseVirtualDBTreeEx.CheckField(const Node: PVirtualNode);
begin
  if FDataLink.DataSet.FindField ( FCheckFieldName ) is TBooleanField Then
    Begin
      if FDataLink.DataSet.FindField ( FCheckFieldName ).AsBoolean Then
        CheckState[Node] := csCheckedNormal
      Else
        CheckState[Node] := csUncheckedNormal;
    End
  Else
    if FDataLink.DataSet.FindField ( FCheckFieldName ) is TIntegerField Then
      Begin
        case FDataLink.DataSet.FindField ( FCheckFieldName ).AsInteger of
        	0..10 : Begin
        				   SetCheckFieldValues ( Node, FDataLink.DataSet.FindField ( FCheckFieldName ).AsInteger, True );
        				 End ;
        	11..21 : Begin
        				     SetCheckFieldValues ( Node, FDataLink.DataSet.FindField ( FCheckFieldName ).AsInteger - 11 , False );
        				   End ;
        		End ;
      End
    Else
      if FDataLink.DataSet.FindField ( FCheckFieldName ).IsNull Then
        CheckState[Node] := csUncheckedNormal
      Else
        CheckState[Node] := csCheckedNormal ;


end;

procedure TBaseVirtualDBTreeEx.SetCheckFieldValues(const Node: PVirtualNode ; const ai_Selection : Integer ; const ab_CanCheck : Boolean );
var
  Data: PDBNodeData;
Begin
  FChangeCheckState := True ;
  case ai_Selection of
    0..2 : Begin
        		 CheckType [Node] := ctTriStateCheckBox ;
        		 if ai_Selection = 1 Then
        			 CheckState[Node] := csCheckedNormal
        		 Else
        			 if ai_Selection = 2 Then
        				 CheckState[Node] := csMixedNormal
        			 Else
        				 CheckState[Node] := csUncheckedNormal;
        	 End ;
    3..4 : Begin
        		 CheckType [Node] := ctRadioButton ;
        		 if ai_Selection = 4 Then
        			 CheckState[Node] := csCheckedNormal
        		 Else
        			 CheckState[Node] := csUncheckedNormal;
        	 End ;
    5..6 : Begin
        		 CheckType [Node] := ctButton ;
        		 if ai_Selection = 6 Then
        			 CheckState[Node] := csCheckedNormal
        		 Else
        			 CheckState[Node] := csUncheckedNormal;
        	 End ;
    7..8 : Begin
        		 CheckType [Node] := ctCheckBox ;
        		 if ai_Selection = 8 Then
        			 CheckState[Node] := csCheckedNormal
        		 Else
        			 CheckState[Node] := csUncheckedNormal;
        	 End ;
    9..10 : Begin
        		 CheckType [Node] := ctNone ;
        		 if ai_Selection = 10 Then
        			 CheckState[Node] := csCheckedNormal
        		 Else
        			 CheckState[Node] := csUncheckedNormal;
        	 End ;
      End ;
  IF Not ab_CanCheck Then
    Begin
      Data := GetDBNodeData(Node);
      Data^.CanCheck := ab_canCheck ;
    End ;
End ;

procedure TBaseVirtualDBTreeEx.DoHeaderClick(HitInfo: TVTHeaderHitInfo);
var Node: PVirtualNode ;
begin
  inherited ;
  if not Assigned ( OnHeaderClick )
  and ( HitInfo.Column = Header.MainColumn ) Then
  with HitInfo do
    Begin
      Node := GetFirstSelected ;
      while assigned ( Node ) do
        Begin
        	if Selected [ Node ] Then
        		Begin
        			if not Expanded [ Node ] Then
        				FullExpand   ( Node )
        			Else
        				FullCollapse ( Node );
        		End ;
        	Node := GetNextSelected ( Node );
        End ;
    End ;
end;

{
procedure TBaseVirtualDBTreeEx.SetRules(const Value: TstringList);
begin
  FRules.Assign ( Value );
end;
}
///////////////////////////////////////////////////////////////////////////////////////
// Procédure : CanSelectData
// Description : La sélection du noeud est-elle vérifiée sur la condition
// Paramètres  : RulerSelection : Les noms de sélection
//               Condition      : La condition en cours
///////////////////////////////////////////////////////////////////////////////////////
function TBaseVirtualDBTreeEx.CanSelectData ( const RulerSelection : Variant ; const Condition : Variant ) : Boolean;
var li_i : Integer ;
Begin
  Result := False ;
  if VarIsArray ( RulerSelection ) Then
    for li_i := 0 to VarArrayHighBound ( RulerSelection, 1 ) do
      Begin
        if  ( RulerSelection [ li_i ] <> Null )
        and ( VarCompareValue ( RulerSelection [ li_i ], Condition ) = vrEqual ) Then
        	Begin
        		Result := True ;
        		Break ;
        	End ;
      End
  Else
    if RulerSelection <> Null Then
      Result := VarCompareValue ( RulerSelection, Condition ) = vrEqual ;
End ;

///////////////////////////////////////////////////////////////////////////////////////
// Procédure : SetCanCheck
// Description : Mise à jour de Cancheck d'un noeud
// Paramètres  : Node : Le noeud en cours
//               adbn_DBDataNode : Les données du noeud AParent et du noeud en cours
///////////////////////////////////////////////////////////////////////////////////////
procedure TBaseVirtualDBTreeEx.SetCanCheck ( const Node: PVirtualNode ; const adbn_DBDataNode: PDBNodeData ; const ab_Valeur : Boolean );
begin
  if  ( adbn_DBDataNode^.CanCheck <> ab_Valeur )
  and ( not FChangeCheckDontCancheck  or not ab_Valeur )
  and ( not FChangeCheckDontCantcheck or     ab_Valeur ) Then
    Begin
      FChangeCheckState := True ;
      adbn_DBDataNode^.CanCheck := ab_Valeur ;
    End ;
End ;


///////////////////////////////////////////////////////////////////////////////////////
// Procédure : CheckANodeFromRule
// Description : checking d'un noeud et de son état en cours à partir de paramètres
// Paramètres  : Node : Le noeud en cours
//               adbn_DBDataNode : Les données du noeud en cours
//               NewCheckState : le check que l'on veut
///////////////////////////////////////////////////////////////////////////////////////
procedure TBaseVirtualDBTreeEx.CheckANodeFromRule ( const Node: PVirtualNode ; const adbn_DBDataNode : PDBNodeData ; const NewCheckState : TCheckState );
Begin
  if  ( FChangeCheckDontSelect )
  and ( NewCheckState in [ csCheckedNormal, csCheckedPressed ] ) Then
    Exit ;
  if  ( FChangeCheckDontUnSelect )
  and ( NewCheckState in [ csUnCheckedNormal, csUnCheckedPressed ] ) Then
    Exit ;
  if  ( FChangeCheckIfCanCheck )
  and not adbn_DBDataNode^.CanCheck Then
    Exit ;
  adbn_DBDataNode^.RulingState := NewCheckState ;
  if Self.CheckState [ Node ] <> NewCheckState Then
    Begin
      FChangeCheckState := True ;
      CheckState [ Node ] := NewCheckState ;
    End ;
End ;

///////////////////////////////////////////////////////////////////////////////////////
// Procédure : DoChecks
// Description : checking d'un noeud à partir de données
// Paramètres  : Node : Le noeud en cours
//               adbn_DBDataParent, adbn_DBDataNode : Les données du noeud AParent et du noeud en cours
//               Demand : le check que l'on veut en donnée
///////////////////////////////////////////////////////////////////////////////////////
function TBaseVirtualDBTreeEx.DoChecks ( const Node: PVirtualNode ; const adbn_DBDataParent, adbn_DBDataNode: PDBNodeData ; const Demand : Integer ):Boolean;
var lbyt_Demand : Byte ;
begin
  Result := True ;
  case Demand of
    // Sélection simple
    RulerActionSelectAndSelected..RulerActionSelectedOnly :
      Begin
        // toujours
        CheckANodeFromRule ( Node, adbn_DBDataNode, csCheckedNormal );
      End ;
    // Désélection simple
    RulerActionUnSelectAndUnSelected..RulerActionUnSelectedOnly :
      Begin
        CheckANodeFromRule ( Node, adbn_DBDataNode, csUnCheckedNormal );
      End ;
      // Sélection avec dévalidation
      RulerActionUnValidateSelectAndSelected
      ..RulerActionUnValidateSelectedOnly   : Begin
                                                CheckANodeFromRule ( Node, adbn_DBDataNode, csCheckedNormal );
                                                SetCanCheck ( Node, adbn_DBDataNode, False );
                                              End;
      // désélection avec dévalidation
      RulerActionUnValidateUnSelectAndUnSelected
      ..RulerActionUnValidateUnSelectedOnly : Begin
                                                CheckANodeFromRule ( Node, adbn_DBDataNode, csUnCheckedNormal );
                                                SetCanCheck ( Node, adbn_DBDataNode, False );
                                              End;
      // sélection sélectionnable
      RulerActionValidateUnSelectAndUnSelected
      ..RulerActionValidateUnSelectedOnly : Begin
                                              SetCanCheck ( Node, adbn_DBDataNode, True );
                                              CheckANodeFromRule ( Node, adbn_DBDataNode, csUnCheckedNormal );
                                            End;
      // désélection sélectionnable
      RulerActionValidateSelectAndSelected
      ..RulerActionValidateSelectedOnly : Begin
                                              SetCanCheck ( Node, adbn_DBDataNode, True );
                                              CheckANodeFromRule ( Node, adbn_DBDataNode, csCheckedNormal );
                                            End;
      // Sélectionnable
      RulerActionValidate   :  SetCanCheck ( Node, adbn_DBDataNode, True );
      // Non sélectionnable
      RulerActionUnValidate : SetCanCheck ( Node, adbn_DBDataNode, False );
      RulerActionOnlyRevalidate..RulerActionOnlyRevalidateSelect :
        if adbn_DBDataNode^.CanCheck
        and ( Node^.CheckState in [cscheckedNormal,csCheckedPressed])
        and ( Node^.CheckType <> ctNone ) Then
          try
            FChangeCheckRevalidate := True ;
            lbyt_Demand := Demand ;
            if lbyt_Demand in [ RulerActionOnlyRevalidateCanCheckUnSelect, RulerActionOnlyRevalidateUnSelectCanCheck, RulerActionOnlyRevalidateUnSelectCantCheck, RulerActionOnlyRevalidateUnSelect, RulerActionOnlyRevalidateSetCanCheck ] Then
              Begin
                FChangeCheckDontSelect := True ;
              End ;
            if lbyt_Demand in [ RulerActionOnlyRevalidateCanCheckSelect, RulerActionOnlyRevalidateSelectCanCheck, RulerActionOnlyRevalidateSelectCantCheck, RulerActionOnlyRevalidateSelect, RulerActionOnlyRevalidateSetCanCheck ] Then
              Begin
                FChangeCheckDontUnSelect := True ;
              End ;
            if lbyt_Demand in [ RulerActionOnlyRevalidateUnSelect, RulerActionOnlyRevalidateSelect, RulerActionOnlyRevalidateCanCheckSelect, RulerActionOnlyRevalidateCanCheckUnSelect ] Then
              Begin
                FChangeCheckIfCanCheck := True ;
              End ;
            if lbyt_Demand in [ RulerActionOnlyRevalidateSelect, RulerActionOnlyRevalidateUnSelect, RulerActionOnlyRevalidateCanCheckUnSelect, RulerActionOnlyRevalidateCanCheckSelect, RulerActionOnlyRevalidateSetCantCheck, RulerActionOnlyRevalidateUnSelectCanCheck, RulerActionOnlyRevalidateSetCanCheck ] Then
              Begin
                FChangeCheckDontCantCheck := True ;
              End ;
            if lbyt_Demand in [ RulerActionOnlyRevalidateUnSelectCantCheck,  RulerActionOnlyRevalidateUnSelect, RulerActionOnlyRevalidateSelect, RulerActionOnlyRevalidateCanCheckUnSelect, RulerActionOnlyRevalidateCanCheckSelect, RulerActionOnlyRevalidateSetCantCheck ] Then
              Begin
                FChangeCheckDontCanCheck := True ;
              End ;
            FCheckingRules := True ;
            FChangeCheckState   := True ;
            Result := DoRuleAction ( Node, adbn_DBDataNode );
          finally
            FChangeCheckDontSelect    := False ;
            FChangeCheckDontCantcheck := False ;
            FChangeCheckDontCancheck  := False ;
            FChangeCheckDontUnSelect  := False ;
            FChangeCheckIfCanCheck    := False ;
            FChangeCheckRevalidate    := False ;
          End
        Else
          Result := False ;

    Else
      Result := False ;
  End ;
end;

///////////////////////////////////////////////////////////////////////////////////////
// Procédure : DoCheckClick
// Description : checking d'un noeud et comportements spécifiques de branches
// Paramètres  : Node : Le noeud en cours
//               NewCheckState : le check que l'on veut
///////////////////////////////////////////////////////////////////////////////////////
procedure TBaseVirtualDBTreeEx.DoCheckClick( Node: PVirtualNode;
  NewCheckState: TCheckState);
begin
  BeginUpdate;
  Rule ( Node, NewCheckState );
  if ( dboSpecialDBChecks in FDBOptions ) Then
  // Gestion ordonnée de check in
    Begin
      if DBChangeCheckState(Node, NewCheckState) Then
        DoChecked(Node);
    End
  else
    if ( dboSpecialVTChecks in FDBOptions ) Then
    // Gestion héritée de check in
      Begin
        FChangeCheckState := True ;
        inherited DoCheckClick ( Node, NewCheckState );
      End
    Else
      Begin
        // Gestion de check in dans la fiche
        Node^.CheckState := NewCheckState ;
        DoChecked(Node);
      End ;
  EndUpdate;
end;

///////////////////////////////////////////////////////////////////////////////////////
// Procédure : DBChangeCheckState
// Description : checking d'un noeud et comportements spécifiques de branches ( multi-sélection de branches, branches à option, check in des parents )
// Paramètres  : Node  : Le noeud en cours
//               Value : le check que l'on veut
///////////////////////////////////////////////////////////////////////////////////////
function TBaseVirtualDBTreeEx.DBChangeCheckState(const Node: PVirtualNode;
  Value: TCheckState): Boolean;
var
  Run: PVirtualNode;
  lnd_Data : PDBNodeData ;
  CheckCount: Cardinal;

begin
  FChangeCheckState := True ;
  with Node^ do
  begin
    Include(States, vsChecking);

    // Do actions which are associated with the given check state.
    case CheckType of
      // Check state change with additional consequences from check states from the children.
      // Le mode trois état est utilisé avec les branches enfants
      ctTriStateCheckBox:
        begin
        	// propagate state down to the children
        	if toAutoTristateTracking in TreeOptions.AutoOptions then
        		Begin
        			case Value of
        				csUncheckedNormal:
        				  begin
        				    Run := FirstChild;
        				    CheckCount := 0;
        				    while Assigned(Run) do
        				    begin
        				      if Run^.CheckType <> ctNone then
        				      begin
        				        if not FChangeStateParent
        				        and ( Self.CheckState [ Run ] <> csUnCheckedNormal ) Then
        				        	Self.CheckState [ Run ] := csUnCheckedNormal ;
        				        // Check if the new child state was set successfully, otherwise we have to adjust the
        				        // node's new check state accordingly.
        				        if Run^.CheckState in [ cscheckedNormal, cscheckedPressed ] then
        				        	Inc(CheckCount);
        				      end;
        				      Run := Run^.NextSibling;
        				    end;
        				    if ( CheckCount > 0 ) Then
        				    // si au moins une branche enfant désélectionnée alors mode intermédiaire de sélectionné
        				      Value := csMixedNormal
        				    else
        				      // Sinon aucune branche n'est sélectionnée
        				      Value := csUnCheckedNormal;
        				  end;
        				csCheckedNormal:
        				  begin
        				    Run := FirstChild;
        				    CheckCount := 0;
        				    while Assigned(Run) do
        				    begin
        				      if Run^.CheckType <> ctNone then
        				      begin
{        				        if Run.CheckType <> ctRadioButton then
        				        	Begin
        				        		if Run.CheckState <> csCheckedNormal Then
        				        			Self.CheckState [ Run ] := csCheckedNormal ;
        				        	end
        				        Else
        				        	if Run.CheckState = csCheckedNormal Then
        				        		RadioButton := True ;}
        				        // Check if the new child state was set successfully, otherwise we have to adjust the
        				        // node's new check state accordingly.
        				        if Run^.CheckState in [ cscheckedNormal, cscheckedPressed ] then
        				        	Inc(CheckCount);
        				      end;
        				      Run := Run^.NextSibling;
        				    end;
        				    if  ( CheckCount > 0 ) Then
        				      // Des branches sont sélectionnées : on sélectionne la branche parente
        				      Value := csCheckedNormal
        				    else
        				    // si aucune branche enfant désélectionnée alors mode intermédiaire de sélectionné
        				      Value := csMixedNormal
        				  end;
        			end;

        		End ;
        end;
      // radio button check state change
      ctRadioButton:
        if not FChangeStateParent
        and ( Value = csCheckedNormal ) then
        begin
        	// Seul une branche à option avec un même AParent peut être sélectionnée
        	// Make sure only this node is checked.
        	Run := Parent^.FirstChild;
        	while Assigned(Run) do
        	begin
        		if Run^.CheckType = ctRadioButton then
        			Begin
        				if Self.CheckState [ Run ] <> csUnCheckedNormal Then
        				  Self.CheckState [ Run ] := csUncheckedNormal;
        			End;
        		Run := Run^.NextSibling;
        	end;
        	Value := csCheckedNormal;
        end ;
    end;

    // Propagate state up to the AParent.
    if not (vsInitialized in Parent^.States) then
      InitNode(Parent);
    if (toAutoTristateTracking in TreeOptions.AutoOptions) and ([vsChecking, vsDisabled] * Parent^.States = []) and
      (CheckType in [ctCheckBox, ctTriStateCheckBox]) and (Parent <> RootNode) and
      (Parent^.CheckType = ctTriStateCheckBox) then
      Result := CheckParentCheckState(Node, Value)
    else
      Result := True;

    if Result then
      CheckState := Value // Set new check state
    else
      CheckState := UnpressedState[CheckState]; // Reset dynmic check state.
    InvalidateNode(Node);
    if  ( CheckType <> ctNone )
    // sélection : on sélectionne le noeud AParent
    and (( Value = csCheckedNormal )
    // déselection : on vérifie l'intégrité sur l'ensemble des checks des noeuds
    or   ( Value = csUnCheckedNormal )) Then
      Begin
        Run := Parent;
        FChangeStateParent := True ;
        // Mode changement du AParent pour éviter les opérations inutiles
        while Run <> RootNode do
        	begin
        		if  ( Run^.CheckType <> ctNone ) then
        			// On peut sélectionner le noeud
        			Begin
        				// si c'est un noeud à trois état alors une autre règle a pu être appliquée : on revérifie cette règle sauvée
        				if  ( Run^.CheckType = ctTriStateCheckBox )
        				and ( Value = csUnCheckedNormal ) Then
        				  Begin
        				    // Check du noeud en fonction de la règle appliquée avant
        				    lnd_Data := PDBNodeData ( GetDBNodeData ( Run ));
        				    DBChangeCheckState( Run, lnd_Data^.RulingState );
        				  End
        				Else
        				  // check in du noeud AParent
        				  if ( Value = csCheckedNormal ) Then
        				    DBChangeCheckState( Run, Value );
        				FChangeStateParent := False ;
        				Break ;
        			End
        			// 14-06-2005 : Pas de lien de sélection vers les parents sur les branches sans coche
        			// Si pas de check possible alors les parents suivants ne sont pas liés
        		Else
        			Break ;
        		Run := Run^.Parent;
        	end;
        // Plus de mode changement du AParent
        FChangeStateParent := False ;
      End ;
    Exclude(States, vsChecking);
  end;
end;

///////////////////////////////////////////////////////////////////////////////////////
// Procédure : p_SetDatasourceRuler
// Description : Affecte le datasource des conditions
// Paramètres  : Value : le datasource à affecter
///////////////////////////////////////////////////////////////////////////////////////
procedure TBaseVirtualDBTreeEx.p_SetDatasourceRuler(Value: TDatasource);
begin
{$IFDEF LCL}
{$ELSE}
  ReferenceInterface ( DatasourceRules, opRemove );
{$ENDIF}
  If Value <> FDatasourceRules Then
    Begin
      FDatasourceRules := Value ;
    End ;
{$IFDEF LCL}
{$ELSE}
  ReferenceInterface ( DatasourceRules, opInsert );
{$ENDIF}
end;

///////////////////////////////////////////////////////////////////////////////////////
// Procédure : p_SetDatasourceRuleSelect
// Description : Affecte le datasource des sélections
// Paramètres  : Value : le datasource à affecter
///////////////////////////////////////////////////////////////////////////////////////
procedure TBaseVirtualDBTreeEx.p_SetDatasourceRuleSelect(Value: TDatasource);
begin
{$IFDEF LCL}
{$ELSE}
  ReferenceInterface ( DatasourceRuleSelect, opRemove );
{$ENDIF}
  If Value <> FDatasourceRuleSelect Then
    Begin
      FDatasourceRuleSelect := Value ;
    End ;
{$IFDEF LCL}
{$ELSE}
  ReferenceInterface ( DatasourceRuleSelect, opInsert );
{$ENDIF}
end;


///////////////////////////////////////////////////////////////////////////////////////
// Procédure : ExecuteCheckEvent
// Description : exécute l'évènement de check si les règles sont en place
///////////////////////////////////////////////////////////////////////////////////////
procedure TBaseVirtualDBTreeEx.ExecuteCheckEvent;
begin
  if assigned ( FRulerChecks )  Then
    Begin
      FChangeCheckState   := True ;
      FRulerChecks ( Self, FHitNode );
    End ;
  FCheckEvent := False ;
End ;
procedure TBaseVirtualDBTreeEx.WMLButtonUp(var Message: {$IFDEF LCL}TLMLButtonUp{$ELSE}TWMLButtonUp{$ENDIF});
begin
{$IFDEF LCL}
  inherited WMLButtonUp( Message );
{$ELSE}
  inherited;
{$ENDIF}
  if FChangeCheckState Then
    ExecuteCheckEvent;
  FChangeCheckState := False ;
end;
procedure TBaseVirtualDBTreeEx.WMLButtonDown(var Message: {$IFDEF LCL}TLMLButtonDown{$ELSE}TWMLButtonDown{$ENDIF});
var
  HitInfo: THitInfo;

begin
  GetHitTestInfoAt(Message.XPos, Message.YPos, True, HitInfo);
  FHitNode := HitInfo.HitNode ;
  FChangeCheckState := False ;
{$IFDEF LCL}
  inherited WMLButtonDown( Message );
{$ELSE}
  inherited;
{$ENDIF}
end;

procedure TBaseVirtualDBTreeEx.WMKeyUp(var Message: {$IFDEF LCL}TLMKeyUp{$ELSE}TWMKeyUp{$ENDIF});

begin
{$IFDEF LCL}
  inherited WMKeyUp( Message);
{$ELSE}
  inherited;
{$ENDIF}
  if FChangeCheckState Then
    ExecuteCheckEvent;
  FChangeCheckState := False ;
end;

procedure TBaseVirtualDBTreeEx.WMKeyDown(var Message: {$IFDEF LCL}TLMKeyDown{$ELSE}TWMKeyDown{$ENDIF});

begin
  FHitNode := FocusedNode ;
  FChangeCheckState := False ;
{$IFDEF LCL}
  inherited WMKeyDown( Message );
{$ELSE}
  inherited;
{$ENDIF}
end;


///////////////////////////////////////////////////////////////////////////////////////
// Procédure : Rule
// Description : génère une sélection à partir des règles de l'arbre
// Paramètres  : Node : le noeud affecté
//        			 NewCheckState : Ce que l'on veut affecter au noeud
///////////////////////////////////////////////////////////////////////////////////////
procedure TBaseVirtualDBTreeEx.Rule ( const Node: PVirtualNode; const NewCheckState: TCheckState);
var
  ldbn_DBData: PDBNodeData ;
begin
  if ( dboAllowRules in FDBOptions )
  // Il ne faut pas de règle démarrée en même temps
  and not FCheckingRules
  // Le règles sont-elle démarrées tout le temps ou uniquement sur sélection
  and (( dboAlwaysRule in FDBOptions ) or ( NewCheckState in [csCheckedNormal, csMixedNormal] )) Then
    try
      FCheckingRules := True ;
      FChangeCheckState   := True ;
      ldbn_DBData := GetDBNodeData ( Node );
      FCheckEvent := DoRuleAction ( Node, ldbn_DBData );
    finally
      FCheckingRules := False ;
    End ;

end;

Function TBaseVirtualDBTreeEx.GetDBStatus: TDBVTStatuses;
Begin
  Result := FDBStatus;
End;

Function TBaseVirtualDBTreeEx.GetDBOptions: TDBVTOptions;
Begin
  Result := FDBOptions;
End;

Procedure TBaseVirtualDBTreeEx.SetDBOptions(Value: TDBVTOptions);
Var
  ToBeSet, ToBeCleared: TDBVTOptions;
Begin
  EndEditNode;
  ToBeSet := Value - FDBOptions;
  ToBeCleared := FDBOptions - Value;
  FDBOptions := Value;

  if ( dboAlwaysRule In ToBeSet ) Then
      FDBOptions := FDBOptions + [dboAllowRules];
  If (dboTrackCursor In ToBeSet) Then Begin
    If Not (dboTrackActive In FDBOptions) Or Not (dboTrackChanges In FDBOptions) Then Begin
      FDBOptions := FDBOptions + [dboTrackActive, dboTrackChanges];
      FDBOptions := FDBOptions - [dboAllowChecking];
      If (dbtsChanged In FDBStatus) Then DataLinkActiveChanged;
    End Else DataLinkScrolled;
  End Else If (dboTrackChanges In ToBeSet) Then Begin
    If Not (dboTrackActive In FDBOptions) Then FDBOptions := FDBOptions + [dboTrackActive];
    If dbtsChanged In FDBStatus Then DataLinkActiveChanged;
  End Else If dboTrackActive In ToBeSet Then Begin
    If dbtsChanged In FDBStatus Then DataLinkActiveChanged;
  End Else If dboTrackActive In ToBeCleared Then Begin
    FDBOptions := FDBOptions - [dboTrackCursor, dboTrackChanges];
    FDBOptions := FDBOptions + [dboReadOnly];
  End Else If dboTrackChanges In ToBeCleared Then Begin
    FDBOptions := FDBOptions - [dboTrackCursor];
    FDBOptions := FDBOptions + [dboReadOnly];
  End Else If dboTrackCursor In ToBeCleared Then FDBOptions := FDBOptions + [dboReadOnly];

  If dboShowChecks In ToBeSet Then Begin
    If dboTrackCursor In FDBOptions Then Begin
      FDBOptions := FDBOptions - [dboShowChecks];
      FDBOptions := FDBOptions + [dboViewAll];
    End Else Begin
      BeginUpdate;
      THackedTreeOptions(TreeOptions).MiscOptions := THackedTreeOptions(TreeOptions).MiscOptions + [toCheckSupport];
      If Not (dboViewAll In FDBOptions) Then ToggleViewMode;
      EndUpdate;
    End;
  End Else If dboShowChecks In ToBeCleared Then Begin
    BeginUpdate;
    THackedTreeOptions(TreeOptions).MiscOptions := THackedTreeOptions(TreeOptions).MiscOptions - [toCheckSupport];
    If Not (dboViewAll In FDBOptions) Then Begin
      FDBOptions := FDBOptions + [dboViewAll];
      RefreshNodes;
    End;
    EndUpdate;
  End Else If dboViewAll In ToBeSet Then Begin
    If dboShowChecks In FDBOptions Then ToggleViewMode;
  End Else If dboViewAll In ToBeCleared Then Begin
    If dboShowChecks In FDBOptions Then ToggleViewMode
    Else FDBOptions := FDBOptions + [dboViewAll];
  End;

  If dboPathStructure In ToBeSet Then Begin
    FDBOptions := FDBOptions - [dboParentStructure];
    If dboTrackActive In FDBOptions Then UpdateTree;
  End Else If dboParentStructure In ToBeSet Then Begin
    FDBOptions := FDBOptions - [dboPathStructure];
    If dboTrackActive In FDBOptions Then UpdateTree;
  End Else If dboPathStructure In ToBeCleared Then Begin
    FDBOptions := FDBOptions + [dboParentStructure];
    If dboTrackActive In FDBOptions Then UpdateTree;
  End Else If dboParentStructure In ToBeCleared Then Begin
    FDBOptions := FDBOptions + [dboPathStructure];
    If dboTrackActive In FDBOptions Then UpdateTree;
  End;

  If dboAlwaysStructured In ToBeSet Then Begin
    If Not (dbtsStructured In FDBStatus) Then RefreshNodes;
  End Else If dboAlwaysStructured In ToBeCleared Then Begin
    If dboShowChecks In FDBOptions Then FDBOptions := FDBOptions + [dboAlwaysStructured];
  End;

  If dboListView In ToBeSet Then ToggleListView
  Else If dboListView In ToBeCleared Then Begin
    If dbtsStructured In FDBStatus Then ToggleListView
    Else RefreshNodes;
  End;
  If (dboReadOnly In ToBeCleared) And (Not (dboTrackCursor In FDBOptions) Or Not (dboTrackChanges In FDBOptions) Or Not (dboTrackActive In FDBOptions)) Then FDBOptions := FDBOptions + [dboReadOnly];
End;

Function TBaseVirtualDBTreeEx.CanOpenDataSet: Boolean;
Begin
  Result := (FKeyField <> Nil);
  If Result And (Not (dboListView In FDBOptions) Or (dboAlwaysStructured In FDBOptions)) Then Result := ((dboPathStructure In FDBOptions) And Assigned(FPathField)) Or ((dboParentStructure In FDBOptions) And Assigned(FParentField));
  If Result Then DoOpeningDataSet(Result);
End;

Procedure TBaseVirtualDBTreeEx.DoOpeningDataSet(Var Allow: Boolean);
Begin
  Allow := (FViewField <> Nil);
  If Allow Then Begin
    If Assigned(FOnOpeningDataSet) Then FOnOpeningDataSet(Self, Allow);
  End;
End;

Procedure TBaseVirtualDBTreeEx.DoNodeDataChanged(const Node: PVirtualNode; const Field: TField; Var UpdateNode: Boolean);
Begin
  If Assigned(FOnNodeDataChanged) Then FOnNodeDataChanged(Self, Node, Field, UpdateNode);
End;

Procedure TBaseVirtualDBTreeEx.DoReadPathFromDB(Var APath: String);
Begin
  If Assigned(FOnReadPathFromDB) Then FOnReadPathFromDB(Self, APath);
End;

Procedure TBaseVirtualDBTreeEx.DoWritePathToDB(Var APath: String);
Begin
  If Assigned(FOnWritePathToDB) Then FOnWritePathToDB(Self, APath);
End;

Procedure TBaseVirtualDBTreeEx.ReadNodeFromDB(Node: PVirtualNode);
Var
  Data: PDBVTData;
Begin
  Data := GetNodeData(Node);
  If (Data^.Status <> dbnsNone) And (Data^.Status <> dbnsRefreshed) Then DoReadNodeFromDB(Node);
  Data^.Status := dbnsRefreshed;
End;

///////////////////////////////////////////////////////////////////////////////////////
// Procédure : DoReadNodeFromDB
// Description : chargement des noeuds à partir des données
// Paramètres  : Node : le noeud affecté
///////////////////////////////////////////////////////////////////////////////////////
Procedure TBaseVirtualDBTreeEx.DoReadNodeFromDB(const Node: PVirtualNode);
var
  Data: PDBNodeData;
  RecordCount ,
  I: Integer;
  Fields: TList;
  as_Tempo : AnsiString ;
Begin
  // Initialisation : récupération de ce qui va être affecté
  Data := PDBNodeData(GetDBNodeData(Node));
  // On peut clicker sur le checking du noeud par défaut
  Data^.CanCheck := True ;
  // Chargement éventuel des règles exceptionnelles de sélections sur conditions avec dataset de règles
  if  assigned ( FDatasourceRules )
  and assigned ( FDatasourceRules.DataSet )
  and assigned ( FDatasourceRuleSelect )
  and assigned ( FDatasourceRuleSelect.DataSet )
  // règles autorisées
  and ( dboAllowRules in FDBOptions ) Then
    Begin
      // Peut-on charger les données ?
      if      FDatasourceRules.DataSet.Active
      and not FDatasourceRules.DataSet.IsEmpty Then
        Begin
        	// On va tout scruter
        	FDatasourceRules.DataSet.First ;
        	// Initialisation des variables
        	RecordCount := FDatasourceRules.DataSet.RecordCount ;
        	Data^.RulerCondition := VarArrayCreate ( [ 0, RecordCount ], varVariant );
        	Data^.RulerResult    := VarArrayCreate ( [ 0, RecordCount ], varInteger );
        	// Récupération des données pour les conditions sur le noeud
        	for i := 0 to RecordCount - 1 do
        		Begin
        			Data^.RulerCondition [ i ] := FDatasourceRules.DataSet.FieldByName ( FRulerConditionField ).Value ;
        			Data^.RulerResult    [ i ] := FDatasourceRules.DataSet.FieldByName ( FRulerActionField    ).AsInteger ;
        			// au suivant
        			FDatasourceRules.DataSet.Next ;
        		End ;
        End
      Else
      // rien alors variants des règles à null
        Begin
        	Data^.RulerCondition := Null ;
        	Data^.RulerResult    := Null ;
        End ;
      // Chargement éventuel des règles exceptionnels de sélection sur le noeud
      if      FDatasourceRuleSelect.DataSet.Active
      and not FDatasourceRuleSelect.DataSet.IsEmpty Then
        // chargement si on a des données
        Begin
        	FDatasourceRuleSelect.DataSet.First ;
        	RecordCount := FDatasourceRuleSelect.DataSet.RecordCount ;
        	Data^.RulerSelection := VarArrayCreate ( [ 0, RecordCount ], varVariant );
        	// Récupération des données pour les sélections sur le noeud
        	for i := 0 to RecordCount - 1 do
        		Begin
        			Data^.RulerSelection [ i ] := FDatasourceRuleSelect.DataSet.FieldByName ( FRulerSelectionField ).Value ;
        			FDatasourceRuleSelect.DataSet.Next ;
        		End ;
        End
      Else
        Data^.RulerSelection := Null ;
    End
  Else
    // Chargement éventuel des règles exceptionnelles de sélection sur condition dans les données de l'arbre
    Begin
      // Peut-on charger des règles ?
      if  ( FRulerSelectionField <> '' )
      and ( dboAllowRules in FDBOptions )
      and assigned ( Datasource.DataSet.FindField ( FRulerSelectionField ))
      and ( FRulerConditionField <> '' )
      and assigned ( Datasource.DataSet.FindField ( FRulerConditionField )) Then
        Begin
        	// Chargement de la condition sur le noeud
        	Data^.RulerCondition := Datasource.DataSet.FieldByName ( FRulerConditionField ).Value ;
        	// Les champs sélection et condition doivent être différents
        	if FRulerConditionField <> FRulerSelectionField Then
        	// Chargement de la sélection sur le noeud
        		Data^.RulerSelection := Datasource.DataSet.FieldByName ( FRulerSelectionField ).Value ;
        End
        //Pas de règles
      Else
        Begin
        	Data^.RulerCondition := Null;
        	Data^.RulerSelection := Null;
        End ;
      // Chargement de l'action à exécuter
      if ( FRulerActionField <> '' )
      and assigned ( Datasource.DataSet.FindField ( FRulerActionField ))
      and not ( Datasource.DataSet.FindField ( FRulerActionField ).IsNull )
      and ( Datasource.DataSet.FindField ( FRulerActionField ) is TIntegerField ) Then
        Data^.RulerResult := Datasource.DataSet.FieldByName ( FRulerActionField ).ASInteger
      Else
        // action par défaut si rien : Sélection
        Data^.RulerResult := RulerActionSelectAndAction ;
    End;
  // chargement des colonnes
  if FDBDataFieldNames <> '' then
    Begin
      Fields := TList.Create;
      try
        // récupération de la liste de champs
        Datasource.DataSet.GetFieldList(Fields, FDBDataFieldNames);
        Data^.DBData := VarArrayCreate([0, Fields.Count - 1], varVariant);
        for I := 0 to Fields.Count - 1 do
        	// Le champ est un champ de temps à formater
        	if  (( TField ( Fields [ I ] ).Value <> Null ) or FFormatIfEmpty ) Then
        	 if  ( TField         ( Fields [ I ] ) is TDateTimeField )
        	 and ( TDateTimeField ( Fields [ I ] ).DisplayFormat <> '' )
        		Then
        		 try
        			 DateTimeToString ( as_Tempo, ( TField ( Fields [ I ] ) as TDateTimeField ).DisplayFormat, TDateTimeField ( Fields [ I ] ).AsDateTime );
        			 Data^.DBData [I] := as_Tempo ;
        		 except
        		 End
        		Else
        		// Le champ est un champ numérique à formater
        		 if  (  TField ( Fields [ I ] ) is TNumericField )
        		 and (( TField ( Fields [ I ] ) as TNumericField ).DisplayFormat <> '' )
        			Then
        			 try
        				 as_Tempo := FormatFloat ( ( TField ( Fields [ I ] ) as TNumericField ).DisplayFormat, TNumericField ( Fields [ I ] ).AsFloat);
  //             Data^.DBData [I] := TField ( Fields [ I ]).AsString ; //as_Tempo ;
        				 Data^.DBData [I] := as_Tempo ;
        			 except
        			 End
        			Else
        			// Le champ n'est pas à formater
        			 Begin
        				Data^.DBData [I] := TField ( Fields[I] ).Value;
        			 End ;
      finally
        Fields.Free;
      end;
    End
  else
    Data^.DBData :=Null;

  if ImgIdxField <> nil then
    if ( ImgIdxField.DataType = ftBoolean )
     Then
      Begin
        if ImgIdxField.AsBoolean
         Then Data^.ImgIdx := 1
         Else Data^.ImgIdx := 0 ;
      End
     Else Data^.ImgIdx := ImgIdxField.Value
  else
    Data^.ImgIdx := -1;

  if StateImgIdxField <> nil then
    if ( StateImgIdxField.DataType = ftBoolean )
     Then
      Begin
        if StateImgIdxField.AsBoolean
         Then Data^.StImgIdx := 1
         Else Data^.StImgIdx := 0 ;
      End
     Else Data^.StImgIdx := StateImgIdxField.Value
  else
    Data^.StImgIdx := -1;

  If Assigned(FOnReadNodeFromDB) Then FOnReadNodeFromDB(Self, Node);

End;

Function TBaseVirtualDBTreeEx.CanWriteToDataSet(const Node: PVirtualNode; const Column: TColumnIndex; const ChangeMode: TDBVTChangeMode): Boolean;
var i : Integer ;
Begin
  Result := Not (dboReadOnly In FDBOptions) And Assigned(FKeyField) And FDataLink.DataSet.CanModify;
  If Result Then Begin
    If dboListView In FDBOptions Then Begin
      If (ChangeMode = dbcmStructure) Or (ChangeMode = dbcmInsert) Then Result := (Node = Nil) Or (Node = RootNode);
    End Else If (ChangeMode = dbcmStructure) Or (ChangeMode = dbcmInsert) Then Begin
      Result := False ;
      if (((dboPathStructure In FDBOptions) Or (dboWriteSecondary In FDBOptions)) And Assigned(FPathField) And FPathField.CanModify) Or (((dboParentStructure In FDBOptions) Or (dboWriteSecondary In FDBOptions)) And Assigned(FParentField) ) Then
        Begin
          if STLParentFields = nil  then
           Result := FParentField.Fields [ 0 ].CanModify
          Else
           for i := 0 to STLParentFields.Count - 1 do
             if FParentField.Fields [ i ].CanModify then
               Result := True;
        End;
      If Result And (dboWriteLevel In FDBOptions) Then Result := Assigned(FLevelField) And FLevelField.CanModify;
    End;
    If Result Then DoWritingDataSet(Node, Column, ChangeMode, Result);
  End;
End;

Procedure TBaseVirtualDBTreeEx.DoWritingDataSet(const Node: PVirtualNode; const Column: TColumnIndex; const ChangeMode: TDBVTChangeMode; Var Allow: Boolean);
Begin
  If (ChangeMode = dbcmEdit) And (Column = Header.MainColumn) Then Allow := FViewField.CanModify;
  If Allow Then Begin
    If Assigned(FOnWritingDataSet) Then FOnWritingDataSet(Self, Node, Column, ChangeMode, Allow);
  End;
End;

Function TBaseVirtualDBTreeEx.DoChecking(Node: PVirtualNode; Var NewCheckState: TCheckState): Boolean;
var
  ldbn_DBData: PDBNodeData ;
Begin
  Result := False ;
  ldbn_DBData := GetDBNodeData ( Node );
  // On veut checker une branche qui ne peut pas l'être : on ne check pas
  if  not ldbn_DBData^.CanCheck
  // Si on est mode mise en place de checkins en cascade -> on check
  and not FChangeCheckState Then
    Exit ;
  If dbtsDataChanging In FDBStatus Then Result := True
  Else If (dboShowChecks In FDBOptions) and (dboAllowChecking In FDBOptions) Then Result := Inherited DoChecking(Node, NewCheckState)
  Else Result := False;
End;

Procedure TBaseVirtualDBTreeEx.DoChecked(Node: PVirtualNode);
var
  ldbn_DBData: PDBNodeData ;
Begin
  ldbn_DBData := GetDBNodeData ( Node );
  if  not ldbn_DBData^.CanCheck
  and not FChangeCheckState Then
    Exit ;
  If Not (dbtsDataChanging In FDBStatus) Then Begin
    BeginUpdate;
    If CheckState[Node] = csCheckedNormal Then Begin
      If dboCheckChildren In FDBOptions Then CheckAllChildren(Node);
    End Else If Not (dboViewAll In FDBOptions) Then ToggleViewMode;
    If Not (dbtsChecking In FDBStatus) And (dboPathStructure In FDBOptions) Then RefreshNodes;
    EndUpdate;
    Inherited;
  End;
End;

///////////////////////////////////////////////////////////////////////////////////////
// Procédure récursive : DoRuleAction
// Description : Evènements avant checking, gestion de règles exceptionnelles de checking et de revalidation
// Paramètres  : RootNode : Le noeud d'appel sinon nil
//               Node : Le noeud en cours
//               adbn_DBData : Les données du noeud d'origine
//               Retour : On peut le checker
///////////////////////////////////////////////////////////////////////////////////////
function TBaseVirtualDBTreeEx.DoRuleAction ( const ExcludedSourceNode : PVirtualNode ; const SourceActions : PDBNodeData ) : Boolean ;
var
  li_i        : Integer ;
Begin
  Result := False ;
   if VarIsArray ( SourceActions^.RulerResult ) Then
    Begin
     // Première exécution : refaire ce qui est déjà exécuté
      if not FChangeCheckRevalidate Then
        for li_i := 0 to VarArrayHighBound ( SourceActions^.RulerResult, 1 ) do
            // On scrute les noms de condition après vérifications
          if  VarIsNumeric ( SourceActions^.RulerResult [ li_i ] )
          and (( Integer ( SourceActions^.RulerResult [ li_i ]) in [ RulerActionOnlyRevalidate..RulerActionOnlyRevalidateSelect ] ) ) Then
            if DoRuleCheckAllActions ( ExcludedSourceNode, GetFirst, SourceActions, SourceActions^.RulerResult [ li_i ]) Then
              Result := True ;
      // Deuxième exécution : Appliquer de nouvelles règles de check
      for li_i := 0 to VarArrayHighBound ( SourceActions^.RulerResult, 1 ) do
          // On scrute les noms de condition après vérifications
        if VarIsStr ( SourceActions^.RulerCondition [ li_i ])
        // résultat demandé ok ?
        and VarIsNumeric ( SourceActions^.RulerResult [ li_i ] )
        and ( Integer ( SourceActions^.RulerResult [ li_i ]) in [RulerActionSelectOnly..RulerActionUnValidate]) Then
          Begin
            if DoRuleCheckAction ( ExcludedSourceNode, GetFirst, SourceActions, SourceActions^.RulerResult [ li_i ], SourceActions^.RulerCondition [ li_i ] ) Then
              Result := True ;
          End ;
    End
  Else
    if  ( SourceActions^.RulerCondition <> Null )
    and VarIsNumeric ( SourceActions^.RulerResult ) Then
      Begin
       // Première exécution : refaire ce qui est déjà exécuté
        if not FChangeCheckRevalidate
        and  (   ( Integer ( SourceActions^.RulerResult) in [ RulerActionOnlyRevalidate..RulerActionOnlyRevalidateSelect ] ) ) Then
          if DoRuleCheckAllActions ( ExcludedSourceNode, GetFirst, SourceActions, SourceActions^.RulerResult ) Then
            Result := True ;
        // Deuxième exécution : Appliquer de nouvelles règles de check
        if ( Integer ( SourceActions^.RulerResult ) in [RulerActionSelectOnly..RulerActionUnValidate]) Then
          if DoRuleCheckAction ( ExcludedSourceNode, GetFirst, SourceActions, SourceActions^.RulerResult, SourceActions^.RulerCondition ) Then
            Result := True ;
      End ;
End ;
///////////////////////////////////////////////////////////////////////////////////////
// Procédure récursive : DoRuleCheckAction
// Description : Evènements avant checking, gestion de règles exceptionnelles de checking
// Paramètres  : SourceNodeExcluded : Le noeud d'appel sinon nil
//               Node : Le noeud en cours
//               adbn_DBData : Les données du noeud d'origine
//               Retour : On peut le checker
///////////////////////////////////////////////////////////////////////////////////////
function TBaseVirtualDBTreeEx.DoRuleCheckAction ( const SourceNodeExcluded, Node: PVirtualNode ; const adbn_DBData: PDBNodeData; Demand : Integer ; const Condition : Variant ) : Boolean ;
var
  ldbn_DBData : PDBNodeData ;
Begin
  Result := False ;
  // Les données data du noeud en cours sont récupérées
  ldbn_DBData := GetDBNodeData ( Node );
  // Etat en cours de modification : on avertit les autres noeuds impliqués que ce noeud va changer
  ldbn_DBData^.RulingState := Node^.CheckState ;
  // Si c'est le noeud en cours qui est aussi le noeud d'appel de l'action on ne le traite pas
  if ( SourceNodeExcluded <> Node )
    // On scrute les noms de sélection pour vérifier la condition
  and CanSelectData   ( ldbn_DBData^.RulerSelection, Condition ) Then
    // si la condition est vérifiée alors on check
    Begin
      Result := True ;
      DoChecks ( Node, adbn_DBData, ldbn_DBData, Demand );
    End ;
  // Passage aux noeuds suivants : c'est une procédure récursive
  if assigned ( Node^.NextSibling )
  and DoRuleCheckAction ( SourceNodeExcluded, Node^.NextSibling, adbn_DBData, Demand, Condition ) Then
    Result := True ;
  if assigned ( Node^.FirstChild )
  and DoRuleCheckAction ( SourceNodeExcluded, Node^.FirstChild, adbn_DBData, Demand, Condition ) Then
    Result := True ;
End;

///////////////////////////////////////////////////////////////////////////////////////
// Procédure récursive : DoRuleCheckAllActions
// Description : Evènements avant checking, gestion de règles exceptionnelles de revalidation
// Paramètres  : SourceNodeExcluded : Le noeud d'appel sinon nil
//               Node : Le noeud en cours
//               adbn_DBData : Les données du noeud d'origine
//               Retour : On peut le checker
///////////////////////////////////////////////////////////////////////////////////////
function TBaseVirtualDBTreeEx.DoRuleCheckAllActions ( const SourceNodeExcluded, Node: PVirtualNode ; const adbn_DBData: PDBNodeData ; Demand : Integer ) : Boolean ;
var
  ldbn_DBData : PDBNodeData ;
Begin
  Result := False ;
  // Si c'est le noeud en cours qui est aussi le noeud d'appel de l'action on quitte la fonction
  if FChangeCheckRevalidate Then
    Exit ;
  // Les données data du noeud en cours sont récupérées
  ldbn_DBData := GetDBNodeData ( Node );
  // Etat en cours de modification : on avertit les autres noeuds impliqués que ce noeud va changer
  ldbn_DBData^.RulingState := Node^.CheckState ;
  // Le noeud qui fait l'action n'a pas à être scruté
  if ( SourceNodeExcluded <> Node ) Then
    Begin
      // Poursuivre la récursivité car on a une action de trouvée
      if DoChecks ( Node, adbn_DBData, ldbn_DBData, Demand ) Then
        Result := True ;
    End ;
  // Passage aux noeuds suivants : c'est une procédure récursive
  if assigned ( Node^.NextSibling )
  and DoRuleCheckAllActions ( SourceNodeExcluded, Node^.NextSibling, adbn_DBData, Demand ) Then
    Result := True ;
  if assigned ( Node^.FirstChild )
  and DoRuleCheckAllActions ( SourceNodeExcluded, Node^.FirstChild, adbn_DBData, Demand ) Then
    Result := True ;
End;

Procedure TBaseVirtualDBTreeEx.CheckAllChildren(Node: PVirtualNode);
Begin
  If (dboShowChecks In FDBOptions) And (dboAllowChecking In FDBOptions) Then Begin
    FDBStatus := FDBStatus + [dbtsChecking];
    Node := GetFirstChild(Node);
    While Assigned(Node) Do Begin
      if CheckState[Node] <> csCheckedNormal Then
        CheckState[Node] := csCheckedNormal;
      Node := GetNextSibling(Node);
    End;
    FDBStatus := FDBStatus - [dbtsChecking];
  End;
End;

Procedure TBaseVirtualDBTreeEx.UnCheckAll(Node: PVirtualNode; OnlyChildren: Boolean);
Var
  AChanged: Boolean;
  Last: PVirtualNode;
Begin
  If (dboShowChecks In FDBOptions) And (dboAllowChecking In FDBOptions) Then Begin
    AChanged := False;
    Last := GetNextSibling(Node);
    If Not Assigned(Last) Then Begin
      Last := GetLastChild(Node);
      If Not Assigned(Last) Then Last := Node;
      Last := GetNext(Last);
    End;
    If OnlyChildren Then Node := GetNext(Node);
    While Node <> Last Do Begin
      If CheckState[Node] <> csUncheckedNormal Then Begin
        CheckState[Node] := csUncheckedNormal;
        If CheckState[Node] = csUncheckedNormal Then AChanged := True;
      End;
    End;
    If AChanged Then ToggleViewMode;
  End;
End;

Procedure TBaseVirtualDBTreeEx.DoInitNode(AParent: PVirtualNode; Node: PVirtualNode; Var InitStates: TVirtualNodeInitStates);
Begin
  NodeHeight [ Node ] := FNodeLines * DefaultNodeHeight ;
  if FNodeLines > 1 Then
    Include(InitStates, ivsMultiline);
  Inherited;
  Node^.CheckType := ctCheckBox;
  Node^.CheckState := csUncheckedNormal;
End;

Procedure TBaseVirtualDBTreeEx.ToggleViewMode;
Var
  Node: PVirtualNode;
Begin
  If Not (dbtsDataChanging In FDBStatus) And (dboShowChecks In FDBOptions) Then Begin
    BeginUpdate;
    If dboViewAll In FDBOptions Then RefreshNodes()
    Else Begin
      If dbtsToggleAll In FDBStatus Then RefreshNodes;
      Node := GetLastChild(RootNode);
      While Assigned(Node) And (Node <> RootNode) Do Begin
        If (CheckState[Node] <> csCheckedNormal) And Not Assigned(GetFirstChild(Node)) Then Begin
          DeleteNode(Node);
          If dboListView In FDBOptions Then FDBStatus := FDBStatus - [dbtsStructured];
        End;
        Node := GetPrevious(Node);
      End;
    End;
    EndUpdate;
  End;
End;

Function TBaseVirtualDBTreeEx.HasVisibleChildren(Node: PVirtualNode): Boolean;
Var
  Last: PVirtualNode;
Begin
  Result := False;
  If Assigned(Node) Then Begin
    Last := GetNextSibling(Node);
    If Not Assigned(Last) Then Begin
      Last := GetLastChild(Node);
      If Not Assigned(Last) Then Last := Node;
      Last := GetNext(Last);
    End;
    Node := GetNext(Node);
    While Node <> Last Do Begin
      If IsVisible[Node] Then Begin
        Result := True;
        Break;
      End;
      Node := GetNext(Node);
    End;
  End;
End;

Procedure TBaseVirtualDBTreeEx.ExpandAll;
Var
  Node: PVirtualNode;
Begin
  Node := GetFirst;
  BeginUpdate;
  While Assigned(Node) Do Begin
    Expanded[Node] := True;
    Node := GetNext(Node);
  End;
  EndUpdate;
End;

Procedure TBaseVirtualDBTreeEx.CollapseAll;
Var
  Node: PVirtualNode;
Begin
  Node := GetFirst;
  BeginUpdate;
  While Assigned(Node) Do Begin
    Expanded[Node] := False;
    Node := GetNext(Node);
  End;
  EndUpdate;
End;

Procedure TBaseVirtualDBTreeEx.SetViewFieldName(Const Value: String);
Begin
  If FViewFieldName <> Value Then Begin
    FViewField := Nil;
    FViewFieldName := Value;
    DataLinkActiveChanged;
  End;
End;

Procedure TBaseVirtualDBTreeEx.SetImgIdxFieldName(Const Value: String);
Begin
  If FImgIdxFieldName <> Value Then Begin
    FImgIdxField := Nil;
    FImgIdxFieldName := Value;
    DataLinkActiveChanged;
  End;
End;

Procedure TBaseVirtualDBTreeEx.SetStImgIdxFieldName(Const Value: String);
Begin
  If FStImgIdxFieldName <> Value Then Begin
    FStImgIdxField := Nil;
    FStImgIdxFieldName := Value;
    DataLinkActiveChanged;
  End;
End;

Procedure TBaseVirtualDBTreeEx.SetDBDataFieldNames(Const Value: String);
Begin
  If FDBDataFieldNames <> Value Then Begin
    FDBDataFieldNames := Value;
    DataLinkActiveChanged;
  End;
End;

function TBaseVirtualDBTreeEx.GetOptions: TStringTreeOptions;
begin
  Result := TStringTreeOptions(inherited TreeOptions);
end;

procedure TBaseVirtualDBTreeEx.SetOptions(const AValue: TStringTreeOptions);
begin
  inherited TreeOptions := AValue;
end;

function TBaseVirtualDBTreeEx.GetOptionsClass: TTreeOptionsClass;
begin
  Result := TStringTreeOptions;
end;

function TBaseVirtualDBTreeEx.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList;
begin

  if (Column = Header.MainColumn) then
  begin
    if (Kind = ikNormal) or (Kind = ikSelected) then
      Index := PDBNodeData(GetDBNodeData(Node))^.ImgIdx;
  end;
  if (Column = Header.MainColumn) then
  begin
    if (Kind = ikState) then
      Index := PDBNodeData(GetDBNodeData(Node))^.StImgIdx;
  end;

{$IFDEF LCL}
{$ELSE}
  Result :=
{$ENDIF}
  inherited DoGetImageIndex(Node, Kind, Column, Ghosted, Index);

  if Assigned(OnGetImageIndex) then
    OnGetImageIndex(self, Node, Kind, Column, Ghosted, Index);

end;

{------------------------------------------------------------------------------}

Constructor TVirtualDBTreeEx.Create(AOwner: TComponent);
Begin
  Inherited;
  DBNodeDataSize := sizeof(TDBNodeData);
End;

Procedure TVirtualDBTreeEx.DoReadNodeFromDB(const Node: PVirtualNode);
{var
  Data: PDBNodeData;}
Begin
//  Data := PDBNodeData(GetDBNodeData(Node));

  NodeText[Node] := FViewField.AsString;

  inherited ;


End;

Procedure TVirtualDBTreeEx.DoNodeDataChanged(const Node: PVirtualNode; const Field: TField; Var UpdateNode: Boolean);
var
  Data: PDBNodeData;
Begin
  If Field = FViewField Then Begin
    NodeText[Node] := Field.AsString;
    UpdateNode := True;
  End
  else if (Field = ImgIdxField) then begin
    Data := PDBNodeData(GetDBNodeData(Node));
    Data^.ImgIdx := Field.AsInteger;
    UpdateNode := true;
  End
  else if (Field = StateImgIdxField) then begin
    Data := PDBNodeData(GetDBNodeData(Node));
    Data^.StImgIdx := Field.AsInteger;
    UpdateNode := true;
  end;

End;

Procedure TBaseVirtualDBTreeEx.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; Var AText: String);
var
    CellData: Variant;
    Data: PDBNodeData;
Begin
  If Assigned(Node) And (Node <> RootNode)
   Then
    Begin
    // Y a-t-il des colonnes
      if  ( DBDataFieldNames <> '' )
      and ( Column <> Header.MainColumn ) // Une colonne maître est en fait le libellé
      and ( Column <  Header.Columns.Count ) // Test d'intégrité : un numéro colonne est toujours inférieure au total de colonnes
       Then
        begin
          Data := GetDBNodeData(Node);
          CellData := Data^.DBData;

        	if VarIsArray( CellData )// Il y a bien un tableau
        	 then    // 1 or many fields
        		begin
        			if  ( Column  > Header.MainColumn ) // N° de Colonne supérieur à la colonne maître
        			 Then // Alors on décale la sélection de la colonne qui n'a pas la colonne maître
        				Begin
        				  // La valeur est-elle dans les limites ?
        				  if  ( Column - 1 <= VarArrayHighBound ( CellData, 1 ))
        				  and ( Column - 1 >= VarArrayLowBound  ( CellData, 1 ))
        				  // La valeur ne doit pas être null
        				  and ( CellData [ Column - 1 ] <> Null )
        				   Then
        				    AText := CellData[Column-1] ;        // select from array
        				End
        			 else
        				  // La valeur est-elle dans les limites ?
        				// N° de Colonne inférieur à la colonne maître : PAs de décalage
        				if  ( Column <= VarArrayHighBound ( CellData, 1 ))
        				and ( Column >= VarArrayLowBound  ( CellData, 1 ))
        				  // La valeur ne doit pas être null
                and ( CellData [ Column ] <> Null )
                 Then
                  AText := CellData[Column];        // select from array
            end
           else
           // PAs de tableau
            if VarIsStr ( CellData  )
              and  (       ( Column  > Header.MainColumn ) // N° de Colonne supérieur à la colonne maître
                      and  ( Column  <=                1 ))
                or (       ( Column  < Header.MainColumn )
                      and  ( Column  <=                0 ))
             Then
              begin
                AText := Celldata;           // one and only
              end;
                    // Eventually format here as you like
        end
       Else
        If (Column = Header.MainColumn) and (TextType = ttNormal)
         Then
          AText := GetNodeText ( Node )
    end
   Else Inherited;
End;

Procedure TVirtualDBTreeEx.DoNewText(Node: PVirtualNode; Column: TColumnIndex; const AText: String);
Begin
  If Column = Header.MainColumn Then FViewField.Value := AText;
End;

Procedure TVirtualDBTreeEx.DoWritingDataSet(const Node: PVirtualNode; const Column: TColumnIndex; const ChangeMode: TDBVTChangeMode; Var Allow: Boolean);
Begin
  If ChangeMode = dbcmEdit Then Begin
    If Column = Header.MainColumn Then Inherited
    Else Allow := False;
  End;
End;

Function TVirtualDBTreeEx.DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer;
Begin
  Result := 0;
  if Assigned(OnCompareNodes) then
    OnCompareNodes(Self, Node1, Node2, Column, Result)
  else
    begin
      If Column = Header.MainColumn Then
        begin
          If NodeText[Node1] > NodeText[Node2] Then
            Result := 1
          Else
            Result := -1
        end;
    end;
End;

Procedure TVirtualDBTreeEx.SetNodeText(Node: PVirtualNode; Const Value: WideString);
Begin
  If Assigned(Node) Then PDBNodeData(GetDBNodeData(Node))^.Text := Value;
End;

Function TVirtualDBTreeEx.GetNodeText(Node: PVirtualNode): WideString;
var
  ls_Tempo : AnsiString ;
Begin
  If Assigned(Node) Then
   Begin
     if  (   FViewField is TdateTimeField )
     and ( ( FViewField as TdateTimeField ).DisplayFormat <> '' )
      Then
       Begin
         DateTimeToString ( ls_Tempo, ( FViewField as TdateTimeField ).DisplayFormat, StrToDateTime ( PDBNodeData(GetDBNodeData(Node))^.Text ));
         Result := ls_Tempo ;
       End
      Else Result := PDBNodeData(GetDBNodeData(Node))^.Text;
   End ;
End;

{------------------------------------------------------------------------------}

Constructor TCustomDBCheckVirtualDBTreeEx.Create(AOwner: TComponent);
Begin
  Inherited;
  FCheckDataLink := TVirtualDBTreeExDataLink.Create(Self);
  DBOptions := DBOptions - [dboTrackChanges];
  DBOptions := DBOptions + [dboShowChecks, dboAllowChecking, dboSpecialVTChecks ];
  FResultField := Nil;
End;

Destructor TCustomDBCheckVirtualDBTreeEx.Destroy;
Begin
  FCheckDataLink.Free;
  Inherited;
End;

Procedure TCustomDBCheckVirtualDBTreeEx.CheckDataLinkActiveChanged;
Begin
  If Not (csDesigning In ComponentState) Then Begin
    FResultField := Nil;
    If FCheckDataLink.Active Then Begin
      If FResultFieldName <> '' Then FResultField := FCheckDataLink.DataSet.FieldByName(FResultFieldName);
    End;
    UpdateTree;
  End;
End;

Procedure TCustomDBCheckVirtualDBTreeEx.DoOpeningDataSet(Var Allow: Boolean);
Begin
  If Assigned(FResultField)
  // Mise à jour matthieu giroux
  or assigned ( FDataLink.DataSet.FindField ( FCheckFieldName ))
   Then
    Inherited
  Else Allow := False;
End;

Procedure TCustomDBCheckVirtualDBTreeEx.SetCheckDataSource ( const Value: TDataSource);
Begin
  FCheckDataLink.DataSource := Value;
  If Assigned(Value) Then Value.FreeNotification(Self);
End;

Function TCustomDBCheckVirtualDBTreeEx.GetCheckDataSource: TDataSource;
Begin
  Result := FCheckDataLink.DataSource;
End;

Procedure TCustomDBCheckVirtualDBTreeEx.Notification(AComponent: TComponent; Operation: TOperation);
Begin
  Inherited;
  If (Operation = opRemove) And Assigned(FCheckDataLink) And (AComponent = CheckDataSource) Then CheckDataSource := Nil;
End;

Procedure TCustomDBCheckVirtualDBTreeEx.SetResultFieldName(Const Value: String);
Begin
  If FResultFieldName <> Value Then Begin
    FResultFieldName := Value;
    If FResultFieldName <> '' Then FResultField := FCheckDataLink.DataSet.FieldByName(FResultFieldName)
      Else FResultField := nil ;
    CheckDataLinkActiveChanged;
  End;
End;

Function TCustomDBCheckVirtualDBTreeEx.DoChecking(Node: PVirtualNode; Var NewCheckState: TCheckState): Boolean;
Begin
  If dbtsDataChanging In DBStatus Then Result := Inherited DoChecking(Node, NewCheckState)
  Else If ( Assigned(FResultField) And FResultField.CanModify ) or Assigned(FDataLink.DataSet.FindField ( FCheckFieldName )) Then Result := Inherited DoChecking(Node, NewCheckState)
  Else Result := False;
End;

Procedure TCustomDBCheckVirtualDBTreeEx.DoChecked(Node: PVirtualNode);
Var
  Data: PDBVTData;
Begin
  If Not (dbtsDataChanging In DBStatus) Then Begin
    Data := GetNodeData(Node);
    If CheckState[Node] = csCheckedNormal Then
    Begin
    // Mise à jour Matthieu Giroux
      if not ( FResultField is TBooleanField ) Then
        Begin
          FCheckDataLink.DataSet.Insert;
          FResultField.Value := Data^.ID;
          FCheckDataLink.DataSet.Post;
        End
      Else
        If FCheckDataLink.DataSet.Locate(FResultKeyFieldName, Data^.ID, []) Then
          Begin
            if not ( FCheckDataLink.DataSet.State in [dsInsert, dsEdit ]) Then
              FCheckDataLink.DataSet.Edit ;
            FResultField.Value := True ;
            FCheckDataLink.DataSet.Post;
          End ;
    End
    Else
    if not ( FResultField is TBooleanField ) Then
      Begin
        If FCheckDataLink.DataSet.Locate(FResultFieldName, Data^.ID, []) Then FCheckDataLink.DataSet.Delete;
      End
        Else If FCheckDataLink.DataSet.Locate(FResultKeyFieldName, Data^.ID, []) Then
          Begin
            if not ( FCheckDataLink.DataSet.State in [dsInsert, dsEdit ]) Then
              FCheckDataLink.DataSet.Edit ;
            FResultField.Value := False ;
            FCheckDataLink.DataSet.Post;
          End ;
  End;
  Inherited;
End;

Procedure TCustomDBCheckVirtualDBTreeEx.DoReadNodeFromDB(const Node: PVirtualNode);
Var
  Data: PDBVTData;
Begin
  Inherited;
  Data := GetNodeData(Node);
  if assigned ( FDataLink.DataSet.FindField ( FCheckFieldName )) Then
    Begin
      CheckField ( Node );
    End
  Else
    If assigned ( FCheckDataLink.DataSet )
    and assigned ( FCheckDataLink.DataSet.FindField ( FResultFieldName ))
    and ( FCheckDataLink.DataSet.Locate(FResultFieldName, Data^.ID, [])) Then CheckState[Node] := csCheckedNormal
    Else CheckState[Node] := csUncheckedNormal;
End;

{------------------------------------------------------------------------------}

Constructor TDBCheckVirtualDBTreeEx.Create(AOwner: TComponent);
Begin
  Inherited;
  DBNodeDataSize := sizeof(TDBNodeData);
End;

Procedure TDBCheckVirtualDBTreeEx.DoReadNodeFromDB(const Node: PVirtualNode);
//var
//  Data: PDBNodeData;
Begin
  NodeText[Node] := FViewField.AsString;
//  Data := PDBNodeData(GetDBNodeData(Node));
{  if ImgIdxField <> nil then
    Data^.ImgIdx := ImgIdxField.AsInteger
  else
    Data^.ImgIdx := -1;
  if StateImgIdxField <> nil then
    Data^.StImgIdx := StateImgIdxField.AsInteger
  else
    Data^.StImgIdx := -1;
 }
  Inherited;
End;

Procedure TDBCheckVirtualDBTreeEx.DoNodeDataChanged(const Node: PVirtualNode; const Field: TField; Var UpdateNode: Boolean);
var
  Data: PDBNodeData;
Begin
  If Field = FViewField Then Begin
    NodeText[Node] := Field.AsString;
    UpdateNode := True;
  End
  else if Field = ImgIdxField then begin
    Data := PDBNodeData(GetDBNodeData(Node));
    Data^.ImgIdx := Field.AsInteger;
    UpdateNode := True;
  End
  else if Field = StateImgIdxField then begin
    Data := PDBNodeData(GetDBNodeData(Node));
    Data^.StImgIdx := Field.AsInteger;
    UpdateNode := True;
  end;

End;
{
Procedure TDBCheckVirtualDBTreeEx.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; Var Text: WideString);
var
    CellData: Variant;
    Data: PDBNodeData;
Begin
  If Assigned(Node) And (Node <> RootNode)
   Then
    Begin
      if  ( DBDataFieldNames <> '' )
      and ( Column <> Header.MainColumn )
      and ( Column <  Header.Columns.Count )
       Then
        begin
          Data := GetDBNodeData(Node);
          CellData := Data^.DBData;

          if VarIsArray( CellData )
           then    // 1 or many fields
            begin
              if  ( Header.MainColumn < Column )
               Then
                Begin
                  if  ( Column - 1 <= VarArrayHighBound ( CellData, 1 ))
                  and ( Column - 1 >= VarArrayLowBound  ( CellData, 1 ))
                  and ( CellData [ Column - 1 ] <> Null )
                   Then
                    Text := CellData[Column-1] ;        // select from array
                End
               else
                if  ( Column <= VarArrayHighBound ( CellData, 1 ))
                and ( Column >= VarArrayLowBound  ( CellData, 1 ))
                and ( CellData [ Column ] <> Null )
                 Then
                  Text := CellData[Column];        // select from array
            end
           else
            if VarIsStr ( CellData  )
             Then
              begin
                text := Celldata;           // one and only
              end;
                    // Eventually format here as you like
        end
       Else
        If (Column = Header.MainColumn) and (TextType = ttNormal)
         Then
          Text := NodeText[Node]
    end
   Else Inherited;
End;
 }
Procedure TDBCheckVirtualDBTreeEx.DoNewText(Node: PVirtualNode; Column: TColumnIndex; const AText: String);
Begin
  If Column = Header.MainColumn Then FViewField.Value := AText;
End;

Procedure TDBCheckVirtualDBTreeEx.DoWritingDataSet(const Node: PVirtualNode; const Column: TColumnIndex; const ChangeMode: TDBVTChangeMode; Var Allow: Boolean);
Begin
  If ChangeMode = dbcmEdit Then Begin
    If Column = Header.MainColumn Then Inherited
    Else Allow := False;
  End;
End;

Function TDBCheckVirtualDBTreeEx.DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer;
Begin
//  If Column = Header.MainColumn Then If NodeText[Node1] > NodeText[Node2] Then Result := 1
//    Else Result := -1
//  Else Result := 0;
  Result := 0;
  if Assigned(OnCompareNodes) then
    OnCompareNodes(Self, Node1, Node2, Column, Result)
  else
    begin
      If Column = Header.MainColumn Then
        begin
          If NodeText[Node1] > NodeText[Node2] Then
            Result := 1
          Else
            Result := -1
        end;
    end;

End;

Procedure TDBCheckVirtualDBTreeEx.SetNodeText(Node: PVirtualNode; Const AValue: WideString);
Begin
  If Assigned(Node) Then PDBNodeData(GetDBNodeData(Node))^.Text := AValue;
End;

Function TDBCheckVirtualDBTreeEx.GetNodeText(Node: PVirtualNode): WideString;
Begin
  If Assigned(Node) Then Result := PDBNodeData(GetDBNodeData(Node))^.Text;
End;

{------------------------------------------------------------------------------}

Constructor TCustomCheckVirtualDBTreeEx.Create(AOwner: TComponent);
Begin
  Inherited;
  FList := TStringList.Create;
  FList.Sorted := True;
  DBOptions := DBOptions - [dboTrackChanges];
  DBOptions := DBOptions + [dboShowChecks, dboAllowChecking];
End;

Destructor TCustomCheckVirtualDBTreeEx.Destroy;
Begin
  FList.Free;
  Inherited;
End;

Function TCustomCheckVirtualDBTreeEx.GetCheckList: TStringList;
Begin
  Result := TStringList.Create;
  Result.Assign(FList);
End;

Procedure TCustomCheckVirtualDBTreeEx.SetCheckList(Value: TStringList);
Begin
  FList.Assign(Value);
  UpdateTree;
End;

Procedure TCustomCheckVirtualDBTreeEx.ClearChecks ;
Begin
  FList.Clear ;
  UpdateTree;
End;

Procedure TCustomCheckVirtualDBTreeEx.DoChecked(Node: PVirtualNode);
Var
  Data: PDBVTData;
  Index: Integer;
Begin
  If Not (dbtsDataChanging In DBStatus) Then Begin
    Data := GetNodeData(Node);
    If CheckState[Node] = csCheckedNormal Then FList.Add(Data^.ID)
    Else Begin
      Index := FList.IndexOf(Data^.ID);
      If Index <> -1 Then FList.Delete(Index);
    End;
  End;
  Inherited;
End;

Function TCustomDBCheckVirtualDBTreeEx.GetCheckList: TStringList;
Var
  Data: PDBVTData;
  Node: PVirtualNode;
Begin
  Result := TStringList.Create;
  Node := GetFirst;
  While Assigned(Node) Do Begin
    Data := GetNodeData(Node);
    If CheckState[Node] = csCheckedNormal Then Result.Add(Data^.ID);
    Node := GetNext(Node);
  End;
End;

Procedure TCustomCheckVirtualDBTreeEx.DoReadNodeFromDB(const Node: PVirtualNode);
Var
  Data: PDBVTData;
  Index: Integer;
Begin
  Inherited;
  Data := GetNodeData(Node);
  if assigned ( FDataLink.DataSet.FindField ( FCheckFieldName )) Then
    Begin
      CheckField ( Node );
    End
  Else
    if ( Data^.ID <> Null )
    and FList.Find(Data^.ID, Index)
     Then
      CheckState[Node] := csCheckedNormal
    Else CheckState[Node] := csUncheckedNormal;
End;

{------------------------------------------------------------------------------}

Constructor TCheckVirtualDBTreeEx.Create(AOwner: TComponent);
Begin
  Inherited;
  DBNodeDataSize := sizeof(TDBNodeData);
End;

Procedure TCheckVirtualDBTreeEx.DoReadNodeFromDB(const Node: PVirtualNode);
var
  Data: PDBNodeData;
Begin
  NodeText[Node] := FViewField.AsString;
  Data := PDBNodeData(GetDBNodeData(Node));
  if ImgIdxField <> nil then
    Data^.ImgIdx := ImgIdxField.AsInteger
  else
    Data^.ImgIdx := -1;
  if StateImgIdxField <> nil then
    Data^.StImgIdx := StateImgIdxField.AsInteger
  else
    Data^.StImgIdx := -1;


  Inherited;
End;

Procedure TCheckVirtualDBTreeEx.DoNodeDataChanged(const Node: PVirtualNode; const Field: TField; Var UpdateNode: Boolean);
var
  Data: PDBNodeData;
Begin
  If Field = FViewField Then Begin
    NodeText[Node] := Field.AsString;
    UpdateNode := True;
  End
  else if Field = ImgIdxField then begin
    Data := PDBNodeData(GetDBNodeData(Node));
    Data^.ImgIdx := Field.AsInteger;
    UpdateNode := True;
  End
  else if Field = StateImgIdxField then begin
    Data := PDBNodeData(GetDBNodeData(Node));
    Data^.StImgIdx := Field.AsInteger;
    UpdateNode := True;
  end;
End;
{
Procedure TCheckVirtualDBTreeEx.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; Var Text: WideString);
var
    CellData: Variant;
    Data: PDBNodeData;
Begin
  If Assigned(Node) And (Node <> RootNode)
   Then
    Begin
      if  ( DBDataFieldNames <> '' )
      and ( Column <> Header.MainColumn )
      and ( Column <  Header.Columns.Count )
       Then
        begin
        	Data := GetDBNodeData(Node);
        	CellData := Data^.DBData;

        	if VarIsArray( CellData )
        	 then    // 1 or many fields
        		begin
        			if  ( Header.MainColumn < Column )
        			 Then
        				Begin
        				  if  ( Column - 1 <= VarArrayHighBound ( CellData, 1 ))
        				  and ( Column - 1 >= VarArrayLowBound  ( CellData, 1 ))
        				  and ( CellData [ Column - 1 ] <> Null )
        				   Then
        				    Text := CellData[Column-1] ;        // select from array
        				End
        			 else
        				if  ( Column <= VarArrayHighBound ( CellData, 1 ))
        				and ( Column >= VarArrayLowBound  ( CellData, 1 ))
        				and ( CellData [ Column ] <> Null )
        				 Then
        				  Text := CellData[Column];        // select from array
        		end
        	 else
        		if VarIsStr ( CellData  )
        		 Then
        			begin
        				text := Celldata;           // one and only
        			end;
        				    // Eventually format here as you like
        end
       Else
        If (Column = Header.MainColumn) and (TextType = ttNormal)
         Then
        	Text := NodeText[Node]
    end
   Else Inherited;
End;
}
Procedure TCheckVirtualDBTreeEx.DoNewText(Node: PVirtualNode; Column: TColumnIndex; const AText: String);
Begin
  If Column = Header.MainColumn Then FViewField.Value := AText;
End;

Procedure TCheckVirtualDBTreeEx.DoWritingDataSet(const Node: PVirtualNode; const Column: TColumnIndex; const ChangeMode: TDBVTChangeMode; Var Allow: Boolean);
Begin
  If ChangeMode = dbcmEdit Then Begin
    If Column = Header.MainColumn Then Inherited
    Else Allow := False;
  End;
End;

Function TCheckVirtualDBTreeEx.DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer;
Begin
//  If Column = Header.MainColumn Then If NodeText[Node1] > NodeText[Node2] Then Result := 1
//    Else Result := -1
//  Else Result := 0;
  Result := 0;
  if Assigned(OnCompareNodes) then
    OnCompareNodes(Self, Node1, Node2, Column, Result)
  else
    begin
      If Column = Header.MainColumn Then
        begin
        	If NodeText[Node1] > NodeText[Node2] Then
        		Result := 1
        	Else
        		Result := -1
        end;
    end;
End;

Procedure TCheckVirtualDBTreeEx.SetNodeText(Node: PVirtualNode; Const Value: WideString);
Begin
  If Assigned(Node) Then PDBNodeData(GetDBNodeData(Node))^.Text := Value;
End;

Function TCheckVirtualDBTreeEx.GetNodeText(Node: PVirtualNode): WideString;
Begin
  Result := '';
  If Assigned(Node) And (Node <> RootNode) Then Result := PDBNodeData(GetDBNodeData(Node))^.Text;
End;

initialization
{$IFDEF LCL}
  {$i virtualdbtreeex.lrs}
{$ENDIF}

End.