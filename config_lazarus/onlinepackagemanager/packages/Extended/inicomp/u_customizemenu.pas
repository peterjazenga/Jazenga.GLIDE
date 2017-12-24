unit U_CustomizeMenu;


/////////////////////////////////////////////////////////////////////////////////
//  U_CustomizeMenu
// Author : Matthieu GIROUX www.liberlog.fr
/////////////////////////////////////////////////////////////////////////////////

{$IFDEF FPC}
{$mode delphi}
{$R *.lfm}
{$ELSE}
{$R *.DFM}
{$ENDIF}

interface

uses
  Classes, SysUtils,
{$IFDEF FPC}
  LazFileUtils, LResources,
{$ELSE}
  JvExControls,
{$ENDIF}
  Forms, Controls, Graphics, Dialogs,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  VirtualTrees, u_extmenucustomize,
  U_OnFormInfoIni, Menus, Buttons,
  ExtCtrls, StdCtrls;

{$IFDEF VERSIONS}
const
    gVer_F_CustomizeMenu : T_Version = ( Component : 'Fenêtre de F_CustomizeMenu' ;
       			                 FileUnit : 'U_CustomizeMenu' ;
       			                 Owner : 'Matthieu Giroux' ;
       			                 Comment : 'Fenêtre de personnalisation de menu.' ;
      			                 BugsStory : 'Version 0.9.0.0 : Adding Customized Menu.' + #13#10 ;
			                 UnitType : CST_TYPE_UNITE_FICHE ;
			                 Major : 0 ; Minor : 9 ; Release : 0 ; Build : 0 );
{$ENDIF}

type
  TMenuNode = Record
    Name, Title : String;
    ImageIndex : Integer;
  end;

  PCustMenuNode = ^TMenuNode ;

  { TF_CustomizeMenu }
  TF_CustomizeMenu = class(TForm)
    FWClose1: TSpeedButton;
    FWDelete: TSpeedButton;
    FWInsert: TSpeedButton;
    ch_ajouteravant: TCheckbox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Splitter1: TSplitter;
    vt_MainMenu: TVirtualStringTree;
    vt_MenuIni: TVirtualStringTree;
    OnFormInfoIni: TOnFormInfoIni;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FWClose1Click(Sender: TObject);
    procedure FWDeleteClick(Sender: TObject);
    procedure FWInsertClick(Sender: TObject);
    procedure vt_MainMenuClick(Sender: TObject);
    procedure vt_MainMenuGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vt_MainMenuGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: {$IFDEF FPC}String{$ELSE}WideString{$ENDIF});
    procedure vt_MainMenuInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure LoadMenuNode ( const ATree : TVirtualStringTree ; const AMenuItem : TMenuItem ; const ParentNode : PVirtualNode ; const aajouter, aSearchValidity : Boolean ); virtual;
    procedure vt_MenuIniClick(Sender: TObject);
  private
    gMenuItem : TMenuItem;
    FMenuCustomize : TExtMenuCustomize;
    procedure GetMenu(const AMenuItem: TMenuItem; var AMenuFound: TMenuItem;
      const MenuNameToFind: String);
    procedure p_ShowMenu(const ATree: TVirtualStringTree; const AMenu: TMenu ; const aSearchValidity : Boolean );
    procedure vt_MenuNodeChange(const AMenuItem: TMenuItem; const Sender: TVirtualStringTree);
    { private declarations }
  public
    procedure DoClose(var CloseAction: TCloseAction); override;
    destructor Destroy; override;
  published
    property MenuCustomize : TExtMenuCustomize read FMenuCustomize  write FMenuCustomize ;
    { public declarations }
  end;

var
  F_CustomizeMenu: TF_CustomizeMenu = nil;

implementation

uses fonctions_components;

{ TF_CustomizeMenu }

procedure TF_CustomizeMenu.vt_MainMenuInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var  CustomerRecord : PCustMenuNode;
begin
  CustomerRecord := Sender.GetNodeData(Node);
  Initialize(CustomerRecord^);
  if assigned ( gMenuItem ) Then
    Begin
      CustomerRecord^.Name  := gMenuItem.Name ;
      CustomerRecord^.Title := gMenuItem.Caption ;
      CustomerRecord^.ImageIndex:=gMenuItem.ImageIndex;
    end;
end;

procedure TF_CustomizeMenu.LoadMenuNode(const ATree : TVirtualStringTree ; const AMenuItem: TMenuItem; const ParentNode : PVirtualNode ; const aajouter, aSearchValidity : Boolean );
var lnod_ChildNode: PVirtualNode ;
  i : Integer;
begin
  if aajouter
  and ( not aSearchValidity or ( assigned ( AMenuItem.OnClick ) or assigned ( AMenuItem.Action ) or ( AMenuItem.Count > 0 ))) then
    Begin
      gMenuItem := AMenuItem;
      lnod_ChildNode := ATree.AddChild ( ParentNode );
      ATree.ValidateNode ( Lnod_ChildNode, False );
    end
   Else
    lnod_ChildNode:=nil;
  for i := 0 to AMenuItem.Count -1  do
    Begin
      LoadMenuNode( ATree, AMenuItem [ i ], lnod_ChildNode, True, aSearchValidity );
    End ;

end;

destructor TF_CustomizeMenu.Destroy;
begin
  inherited Destroy;
  F_CustomizeMenu := nil;
end;

procedure TF_CustomizeMenu.p_ShowMenu ( const ATree : TVirtualStringTree ; const AMenu : TMenu ; const aSearchValidity : Boolean );
begin
  if assigned ( AMenu ) Then
    with ATree do
      Begin
        BeginUpdate ;
        Clear;
        NodeDataSize := Sizeof(TMenuNode)+1;
        Images := AMenu.Images;
        EndUpdate;
        BeginUpdate ;
        LoadMenuNode( ATree, AMenu.Items, nil, False, aSearchValidity );
        EndUpdate;
      end;
end;

procedure TF_CustomizeMenu.FormShow(Sender: TObject);
begin
  p_ShowMenu ( vt_MainMenu, MenuCustomize.MainMenu, True );
  p_ShowMenu ( vt_MenuIni , MenuCustomize.MenuIni , False );
end;

procedure TF_CustomizeMenu.FWClose1Click(Sender: TObject);
begin
  Close;
end;

procedure TF_CustomizeMenu.FWDeleteClick(Sender: TObject);
var  CustomerRecord : PCustMenuNode;
     LMenuItem : TMenuItem ;
begin
  with vt_MenuIni do
  if assigned ( FocusedNode )
   Then
    Begin
      CustomerRecord := GetNodeData( FocusedNode );
      LMenuItem := nil;
      FWDelete.Enabled:=False;
      GetMenu( MenuCustomize.MenuIni.Items, LMenuItem, CustomerRecord^.Name );
      if assigned ( LMenuItem ) Then
        LMenuItem.Free;
      vt_MenuIni.DeleteNode(FocusedNode);
    End;
end;

procedure TF_CustomizeMenu.FWInsertClick(Sender: TObject);
var  CustomerRecord : PCustMenuNode;
     LMenuItem, LMenuToAdd, lmenuCloned : TMenuItem ;
     lnod_ChildNode : PVirtualNode;
     lnod_Focus : PVirtualNode;
begin
  with vt_MainMenu do
    if  assigned ( FocusedNode ) Then
      Begin
        LMenuItem  := nil;
        LMenuToAdd := nil;
        FWInsert.Enabled:=False;
        CustomerRecord := GetNodeData( FocusedNode );
        GetMenu( MenuCustomize.MainMenu.Items, LMenuToAdd, CustomerRecord^.Name );
        lmenuCloned := fmi_CloneMenuItem ( LMenuToAdd, MenuCustomize.MenuIni );
        if assigned ( vt_MenuIni.FocusedNode ) Then
           lnod_Focus := vt_MenuIni.RootNode.FirstChild
         else
           Begin
             MenuCustomize.MenuIni.Items.Add ( lmenuCloned );
             gMenuItem := LMenuToAdd;
             lnod_ChildNode := vt_MenuIni.AddChild ( nil );
             vt_MenuIni.ValidateNode ( Lnod_ChildNode, False );
             Exit;
           end;
        gMenuItem := lmenuCloned;
        CustomerRecord := GetNodeData( lnod_Focus );
        GetMenu( MenuCustomize.MenuIni.Items, LMenuItem, CustomerRecord^.Name );
        if ( ch_ajouteravant.Checked )
         Then
          Begin
            LMenuItem.Parent.Insert(LMenuItem.Parent.IndexOf(LMenuItem), lmenuCloned );
            Lnod_ChildNode := vt_MenuIni.InsertNode(lnod_Focus, amInsertBefore);
          end
         Else
           Begin
             if ( LMenuItem.Parent.IndexOf(LMenuItem) = LMenuItem.Parent.Count - 1 )
              Then LMenuItem.Parent.Add ( lmenuCloned )
              Else LMenuItem.Parent.Insert(LMenuItem.Parent.IndexOf(LMenuItem) + 1, lmenuCloned );
             Lnod_ChildNode := vt_MenuIni.InsertNode(lnod_Focus, amInsertAfter);
           end;
         vt_MenuIni.ValidateNode ( Lnod_ChildNode, False );
      End;
end;

procedure TF_CustomizeMenu.vt_MainMenuClick(Sender: TObject);
begin
  vt_MenuNodeChange ( MenuCustomize.MainMenu.Items, vt_MainMenu );
end;

procedure TF_CustomizeMenu.FormCreate(Sender: TObject);
begin
  gMenuItem := nil;
end;

procedure TF_CustomizeMenu.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
   closeaction := caFree;
end;

procedure TF_CustomizeMenu.vt_MenuIniClick(Sender: TObject);
begin
  vt_MenuNodeChange ( MenuCustomize.MenuIni.Items, vt_MenuIni);
end;


// procedure TF_CustomizeMenu.GetMenu
// AMenuItem : clicked MenuItem
procedure TF_CustomizeMenu.GetMenu(const AMenuItem: TMenuItem; var AMenuFound : TMenuItem; const MenuNameToFind : String);
var i : Integer;
begin
  with FWDelete do
    Begin
      if AMenuFound <> nil Then
        Exit;
      if MenuNameToFind = AMenuItem.Name Then
        Begin
          AMenuFound := AMenuItem;
          Exit;
        end;
      for i := 0 to AMenuItem.Count -1  do
        GetMenu ( AMenuItem [ i ], AMenuFound, MenuNameToFind );

    end;
end;

procedure TF_CustomizeMenu.vt_MenuNodeChange ( const AMenuItem: TMenuItem; const Sender: TVirtualStringTree );
var  CustomerRecord : PCustMenuNode;
     lmenuItem : TMenuItem;
begin
  with Sender do
  if assigned ( FocusedNode ) Then
    Begin
      lmenuItem := nil ;
      if assigned ( AMenuItem ) Then
        Begin
          CustomerRecord := GetNodeData( FocusedNode );
          GetMenu ( AMenuItem, lmenuItem, CustomerRecord^.Name );
        end;
      if Sender = vt_MainMenu Then
        Begin
          FWInsert.Enabled := (lmenuItem <> nil ) and (FocusedNode^.ChildCount=0);
        end
       else
         FWDelete.Enabled := lmenuItem <> nil;
    end;
end;

procedure TF_CustomizeMenu.vt_MainMenuGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var  CustomerRecord : PCustMenuNode;
begin
  CustomerRecord := Sender.GetNodeData(Node);
  ImageIndex := CustomerRecord^.ImageIndex;
end;

procedure TF_CustomizeMenu.vt_MainMenuGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: {$IFDEF FPC}String{$ELSE}WideString{$ENDIF});
var  CustomerRecord : PCustMenuNode;
begin
   CustomerRecord := Sender.GetNodeData(Node);
   CellText := StringReplace(CustomerRecord^.Title,'&','',[rfReplaceAll]);
end;



procedure TF_CustomizeMenu.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);
  CloseAction:=caFree;
end;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_F_CustomizeMenu );
{$ENDIF}
end.

