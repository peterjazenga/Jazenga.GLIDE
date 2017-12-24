{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit view_helper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Contnrs,
  pastree, pascal_parser_intf;

const
  BOOL_STR : array[Boolean] of Char = ('N','Y');

type

  ISymbolPainter = interface
    ['{C13B3547-F338-43D7-8A44-2F81CC34A188}']
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;
  end;
  
  TSearchDirection = ( sdForward, sdbackward );
  TDependencyKind = ( dkProperty, dkAncestor, dkAliasedTo, dkArrayItem );

  { TDependendyInfo }

  TDependendyInfo = class
  private
    FKind : TDependencyKind;
    FPropName : string;
    FUsingType : TPasType;
  public
    property UsingType : TPasType read FUsingType;
    property Kind : TDependencyKind read FKind;
    property PropName : string read FPropName;
  end;
  
  { The function takes ownership of the "AFoundItem" parameter }
  TOnFindDependencyFunc = function (AFoundItem : TDependendyInfo) : Boolean of object;
  
  function FindPainter(AObj : TPasElement) : ISymbolPainter ;
  function SearchItem(
    const AName : string;
          AParentNode,
          AStartFrom : TTreeNode;
    const ADirection : TSearchDirection
  ) : TTreeNode;
  function XsdGenerateSourceForObject(
    AObject : TPasElement;
    ASymbolTable : TwstPasTreeContainer
  ) : string;

  procedure DrawDependencies(
    AView : TTreeView;
    ADependencyList : TObjectList
  );
  procedure FindDependencies(
    AType : TPasType;
    ATree : TwstPasTreeContainer;
    ADestList : TObjectList
  );

  function SetCursorHourGlass():IInterface;

implementation
uses
  Controls, Forms
  {$IFNDEF FPC}, xmldom, wst_delphi_xml{$ELSE}, DOM, wst_fpc_xml{$ENDIF}
  , xsd_generator;

const
  IMG_TABLE   = 0;
  IMG_TYPES   = 1;
  //IMG_CONST   = 2;
  IMG_TYPE_DEF    = 4;
  IMG_INTF_DEF    = 5;
  //IMG_PROP_DEF    = 6;
  IMG_ENUM        = 6;
  IMG_CONST_ITEM  = 7;
  IMG_ENUM_ITEM   = 8;
  IMG_PROC_ITEM   = 9;
  IMG_BINDING_ITEM   = 10;

type

  { TCursorHolder }

  TCursorHolder = class(TInterfacedObject,IInterface)
  private
    FCursor : TCursor;
    FOldCursor : TCursor;
  public
    constructor Create(const ACursor : TCursor);
    destructor Destroy();override;
  end;

function SetCursorHourGlass():IInterface;
begin
  Result := TCursorHolder.Create(crHourGlass);
  Application.ProcessMessages();
end;

type

  { TXsdGenerator }

  TXsdGeneratorX = class(TXsdGenerator)
  public
    constructor Create(
      const ADocument : TDOMDocument;
      const AOptions : TGeneratorOptions;
            ASymbolTable : TwstPasTreeContainer
    );
  end;

{ TXsdGeneratorX }

constructor TXsdGeneratorX.Create(
  const ADocument : TDOMDocument;
  const AOptions : TGeneratorOptions;
        ASymbolTable : TwstPasTreeContainer
);
begin
  inherited Create(ADocument,AOptions);
  Prepare(ASymbolTable,ASymbolTable.CurrentModule);
end;

function XsdGenerateSourceForObject(
  AObject : TPasElement;
  ASymbolTable : TwstPasTreeContainer
) : string;
var
  xsdGenerator : IXsdGenerator;
  typeGenerator : IXsdTypeHandler;
  doc : TXMLDocument;
  locRes : string;
begin
  if ( AObject = nil ) or ( Length(Trim(AObject.Name)) = 0 ) then begin
    locRes := '';
  end else begin
    doc := CreateDoc();
    try
      xsdGenerator := TXsdGeneratorX.Create(doc,[],ASymbolTable);
      if GetXsdTypeHandlerRegistry().Find(AObject,xsdGenerator,typeGenerator) then begin
        typeGenerator.Generate(ASymbolTable,AObject,doc);
        locRes := NodeToBuffer(doc);
      end;
    finally
      ReleaseDomNode(doc);
    end;
  end;
  Result := locRes;
end;

procedure DrawDependencies(
  AView : TTreeView;
  ADependencyList : TObjectList
);

  function FindNode(AType : TPasType) : TTreeNode;
  var
    k : Integer;
    nl : TTreeNodes;
  begin
    Result := nil;
    nl := AView.Items;
    for k := 0 to Pred(nl.Count) do begin
      if ( TPasType(nl[k].Data) = AType ) then begin
        Result := nl[k];
      end;
    end;
  end;

var
  i : Integer;
  e : TDependendyInfo;
  n : TTreeNode;
  s : string;
begin
  AView.Items.BeginUpdate();
  try
    AView.Items.Clear();
    if ( ADependencyList.Count > 0 ) then begin
      for i := 0 to Pred(ADependencyList.Count) do begin
        e := TDependendyInfo(ADependencyList[i]);
        n := FindNode(e.UsingType);
        if ( n = nil ) then begin
          n := AView.Items.Add(nil,e.UsingType.Name);
          n.Data := e.UsingType;
        end;
        case e.Kind of
          dkAliasedTo : s := 'Alias';
          dkAncestor  : s := 'Ancestor type';
          dkArrayItem : s := 'Array Item type';
          dkProperty  : s := Format('Property = %s',[e.PropName]);
          else
            s := e.UsingType.Name;
        end;
        AView.Items.AddChild(n,s).Data := e;
      end;
    end;
  finally
    AView.Items.EndUpdate();
  end;
end;

procedure FindDependencies(
  AType : TPasType;
  ATree : TwstPasTreeContainer;
  ADestList : TObjectList
);
var
  depInfo : TDependendyInfo;
  extName : string;
  
  function IsEqualTo(A : TPasType) : Boolean;
  begin
    Result := ( A <> nil ) and
              ( ( A = AType ) or
                ( A.InheritsFrom(TPasUnresolvedTypeRef) and
                  ( ATree.GetExternalName(A) = extName )
                )
              );
  end;
  
  procedure ScanAliasType(AItem : TPasAliasType);
  begin
    if IsEqualTo(AItem.DestType) then begin
      depInfo := TDependendyInfo.Create();
      depInfo.FUsingType := AItem;
      depInfo.FKind := dkAliasedTo;
      ADestList.Add(depInfo);
    end;
  end;

  procedure ScanArrayType(AItem : TPasArrayType);{$IFDEF USE_INLINE}inline;{$ENDIF}
  begin
    if IsEqualTo(AItem.ElType) then begin
      depInfo := TDependendyInfo.Create();
      depInfo.FUsingType := AItem;
      depInfo.FKind := dkArrayItem;
      ADestList.Add(depInfo);
    end;
  end;
  
  procedure ScanClassType(AItem : TPasClassType);
  var
    k : Integer;
    pl : TList2;
    m : TPasElement;
    p : TPasProperty;
  begin
    if IsEqualTo(AItem.AncestorType) then begin
      depInfo := TDependendyInfo.Create();
      depInfo.FUsingType := AItem;
      depInfo.FKind := dkAncestor;
      ADestList.Add(depInfo);
    end;
    pl := AItem.Members;
    if ( pl.Count > 0 ) then begin
      for k := 0 to Pred(pl.Count) do begin
        m := TPasElement(pl[k]);
        if m.InheritsFrom(TPasProperty) then begin
          p := TPasProperty(m);
          if IsEqualTo(p.VarType) then begin
            depInfo := TDependendyInfo.Create();
            depInfo.FUsingType := AItem;
            depInfo.FKind := dkProperty;
            depInfo.FPropName := p.Name;
            ADestList.Add(depInfo);
          end;
        end;
      end;
    end;
  end;

  procedure ScanRecordType(AItem : TPasRecordType);
  var
    k : Integer;
    pl : TList2;
    m : TPasElement;
    p : TPasVariable;
  begin
    pl := AItem.Members;
    if ( pl.Count > 0 ) then begin
      for k := 0 to Pred(pl.Count) do begin
        m := TPasElement(pl[k]);
        if m.InheritsFrom(TPasVariable) then begin
          p := TPasVariable(m);
          if IsEqualTo(p.VarType) then begin
            depInfo := TDependendyInfo.Create();
            depInfo.FUsingType := AItem;
            depInfo.FKind := dkProperty;
            depInfo.FPropName := p.Name;
            ADestList.Add(depInfo);
          end;
        end;
      end;
    end;
  end;

var
  i, c : Integer;
  list : TList2;
  e : TPasType;
begin
  list := ATree.CurrentModule.InterfaceSection.Types;
  c := list.Count;
  if ( c > 0 ) then begin
    extName := ATree.GetExternalName(AType);
    for i := 0 to Pred(c) do begin
      depInfo := nil;
      e := TPasType(list[i]);
      if e.InheritsFrom(TPasAliasType) then begin
        ScanAliasType(TPasAliasType(e));
      end else if e.InheritsFrom(TPasAliasType) then begin
        ScanArrayType(TPasArrayType(e));
      end else if e.InheritsFrom(TPasClassType) then begin
        ScanClassType(TPasClassType(e));
      end else if e.InheritsFrom(TPasRecordType) then begin
        ScanRecordType(TPasRecordType(e));
      end else if e.InheritsFrom(TPasArrayType) then begin
        ScanArrayType(TPasArrayType(e));
      end;
    end;
  end;
end;

function AddChildNode(AParent: TTreeNode; const AText : string):TTreeNode ;
begin
  Result := AParent.TreeNodes.AddChild(AParent,AText);
  Result.ImageIndex := -1;
  Result.StateIndex := -1;
  Result.SelectedIndex := -1;
end;

function SearchItem(
  const AName : string;
        AParentNode,
        AStartFrom : TTreeNode;
  const ADirection : TSearchDirection
) : TTreeNode;
var
  e : TTreeNode;
  ok : Boolean;
  upperName : string;
begin
  ok := False;
  upperName := UpperCase(AName);
  if ( AStartFrom <> nil ) then
    e := AStartFrom
  else
    e := AParentNode.GetFirstChild();
  while ( e <> nil ) do begin
    if ( e.Data <> nil ) and ( Pos(upperName,UpperCase(TPasElement(e.Data).Name)) = 1 ) then begin
      ok := True;
      Break;
    end;
    if ( ADirection = sdForward ) then
      e := e.GetNext()
    else
      e := e.GetPrev();
  end;
  if ok then
    Result := e
  else
    Result := nil;
end;

{ TCursorHolder }

constructor TCursorHolder.Create(const ACursor: TCursor);
begin
  FCursor := ACursor;
  FOldCursor := Screen.Cursor;
  Screen.Cursor := FCursor;
end;

destructor TCursorHolder.Destroy();
begin
  Screen.Cursor := FOldCursor;
  inherited Destroy();
end;

type

  { TSymbolPainter }

  TSymbolPainter = class(TInterfacedObject,ISymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;virtual;abstract;
  public
    constructor Create();virtual;
    //class function CanHandle(AObjClass : TClass):Boolean;overload;virtual;abstract;
    class function CanHandle(AObj : TObject):Boolean;overload;virtual;abstract;
  end;
  
  TSymbolPainterClass = class of TSymbolPainter;
  
  { TPainterRegistry }

  TPainterRegistry = class
  private
    FList : TClassList;
  private
    function FindHanlderIndex(AObj : TObject):Integer;
  public
    constructor Create();
    destructor Destroy();override;
    procedure RegisterHandler(APainterClass : TSymbolPainterClass);
    function FindHandler(AObj : TObject; out AHandler : ISymbolPainter) : Boolean;
  end;

var
  FPainterRegistryInst : TPainterRegistry;
  
type

  { TAbstractSymbolPainter }

  TAbstractSymbolPainter = class(TSymbolPainter,ISymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    //class function CanHandle(AObjClass : TClass):Boolean;overload;override;
    class function CanHandle(AObj : TObject):Boolean;overload;override;
  end;
  
  { TPackagePainter }

  TPackagePainter = class(TAbstractSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;
  
  { TModulePainter }

  TModulePainter = class(TAbstractSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;

  { TAbstractConstantDefinitionPainter }

  TAbstractConstantDefinitionPainter = class(TAbstractSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;
  
  { TTypeSymbolPainter }

  TTypeSymbolPainter = class(TAbstractSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  end;

  { TAnyTypeDefinitionPainter }

  TAnyTypeDefinitionPainter = class(TTypeSymbolPainter)
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;

  { TTypeAliasDefinitionPainter }

  TTypeAliasDefinitionPainter = class(TTypeSymbolPainter)
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;

  { TEnumTypeDefinitionPainter }

  TEnumTypeDefinitionPainter = class(TTypeSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;

  { TClassTypeDefinitionPainter }

  TClassTypeDefinitionPainter = class(TTypeSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;

  { TRecordTypeDefinitionPainter }

  TRecordTypeDefinitionPainter = class(TTypeSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;
  
  { TArrayTypeDefinitionPainter }

  TArrayTypeDefinitionPainter = class(TTypeSymbolPainter)
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;
  
  { TMethodDefinitionPainter }

  TMethodDefinitionPainter = class(TTypeSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;
  
  { TInterfaceDefinitionPainter }

  TInterfaceDefinitionPainter = class(TTypeSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;

  { TPasNativeSimpleTypePainter }

  TPasNativeSimpleTypePainter = class(TTypeSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;
  
  { TBindingPainter }

  TBindingPainter = class(TAbstractSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;

{ TRecordTypeDefinitionPainter }

function TRecordTypeDefinitionPainter.Paint(
  AContainer : TwstPasTreeContainer;
  AObj: TPasElement;
  AParent: TTreeNode
): TTreeNode;
var
  locObj : TPasRecordType;
  locProp : TPasVariable;
  i : Integer;
  s : string;
begin
  locObj := AObj as TPasRecordType;
  Result := inherited Paint(AContainer,locObj, AParent);
  for i := 0 to Pred(locObj.Members.Count) do begin
    if TPasElement(locObj.Members[i]).InheritsFrom(TPasVariable) then begin
      locProp := TPasVariable(locObj.Members[i]);
      s := Format('%s : %s',[AContainer.GetExternalName(locProp),AContainer.GetExternalName(locProp.VarType)]);
      if AContainer.IsAttributeProperty(locProp) then begin
        s := s + ' ( Attribute )';
      end;
      AddChildNode(Result,s);
    end;
  end;
end;

class function TRecordTypeDefinitionPainter.CanHandle(AObj : TObject) : Boolean;
begin
  Result := inherited CanHandle(AObj) and AObj.InheritsFrom(TPasRecordType);
end;

{ TArrayTypeDefinitionPainter }

class function TArrayTypeDefinitionPainter.CanHandle(AObj : TObject) : Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and AObj.InheritsFrom(TPasArrayType);
end;

{ TBindingPainter }

function TBindingPainter.Paint(
  AContainer: TwstPasTreeContainer;
  AObj: TPasElement;
  AParent: TTreeNode
): TTreeNode;
var
  locObj : TwstBinding;
begin
  locObj := TwstBinding(AObj);
  Result := inherited Paint(AContainer, locObj, AParent);
  Result.ImageIndex := IMG_BINDING_ITEM;
  Result.StateIndex := IMG_BINDING_ITEM;
  Result.SelectedIndex := IMG_BINDING_ITEM;
    AddChildNode(Result,BindingStyleNames[locObj.BindingStyle]);
    AddChildNode(Result,locObj.Address);
end;

class function TBindingPainter.CanHandle(AObj: TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and AObj.InheritsFrom(TwstBinding);
end;

{ TPasNativeSimpleTypePainter }

function TPasNativeSimpleTypePainter.Paint(
  AContainer: TwstPasTreeContainer;
  AObj: TPasElement;
  AParent: TTreeNode
): TTreeNode;
var
  locObj : TPasNativeSimpleType;
  //boxeNode : TTreeNode;
begin
  locObj := TPasNativeSimpleType(AObj);
  Result := inherited Paint(AContainer, locObj, AParent);
  {if ( locObj.BoxedType <> nil ) then begin
    boxeNode := AddChildNode(Result,locObj.BoxedType.Name);
    boxeNode.Data := locObj.BoxedType;
    boxeNode.ImageIndex := -1;
    boxeNode.StateIndex := -1;
    boxeNode.SelectedIndex := -1;
  end;}
end;

class function TPasNativeSimpleTypePainter.CanHandle(AObj: TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and AObj.InheritsFrom(TPasNativeSimpleType);
end;
  
{ TModulePainter }

function TModulePainter.Paint(
  AContainer: TwstPasTreeContainer;
  AObj: TPasElement;
  AParent: TTreeNode
): TTreeNode;
var
  i , c: Integer;
  locObj : TPasModule;
  objPtr : ISymbolPainter;
  {constNode,} typNode, intfNode : TTreeNode;
  objItm : TPasElement;
  decList : TList2;
begin
  locObj := AObj as TPasModule;
  Result := inherited Paint(AContainer, locObj, AParent);
  Result.ImageIndex := IMG_TABLE;
  Result.StateIndex := IMG_TABLE;
  Result.SelectedIndex := IMG_TABLE;
  {constNode := AddChildNode(Result,'Const');
  constNode.ImageIndex := IMG_CONST;
  constNode.StateIndex := IMG_CONST;
  constNode.SelectedIndex := IMG_CONST;}
  typNode := AddChildNode(Result,'Type');
  typNode.ImageIndex := IMG_TYPES;
  typNode.StateIndex := IMG_TYPES;
  typNode.SelectedIndex := IMG_TYPES;
  intfNode := AddChildNode(Result,'Interface');
  decList := locObj.InterfaceSection.Declarations;
  c := decList.Count;
  for i := 0 to Pred(c) do begin
    objItm := TPasElement(decList[i]);
    objPtr := FindPainter(objItm) ;
    if Assigned(objPtr) then begin
      if objItm.InheritsFrom(TPasClassType) and ( TPasClassType(objItm).ObjKind = okInterface ) then
        objPtr.Paint(AContainer,objItm,intfNode)
      else
        objPtr.Paint(AContainer,objItm,typNode);
    end;
  end;
end;

class function TModulePainter.CanHandle(AObj: TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and AObj.InheritsFrom(TPasModule);
end;

{ TMethodDefinitionPainter }

function TMethodDefinitionPainter.Paint(
  AContainer : TwstPasTreeContainer;
  AObj: TPasElement;
  AParent: TTreeNode
): TTreeNode;
var
  j : Integer;
  ss : string;
  pmr : TPasArgument;
  locMthd : TPasProcedure;
  memberList : TList2;
begin
  locMthd := AObj as TPasProcedure;
  Result := AddChildNode(AParent,AContainer.GetExternalName(locMthd));
  Result.Data := locMthd;
  Result.ImageIndex := IMG_PROC_ITEM;
  Result.StateIndex := IMG_PROC_ITEM;
  Result.SelectedIndex := IMG_PROC_ITEM;
  memberList := locMthd.ProcType.Args;
  for j := 0 to Pred(memberList.Count) do begin
    pmr := TPasArgument(memberList[j]);
    ss := AccessNames[pmr.Access];
    if ( Length(ss) > 0 ) then
      ss := ss + ' ';
    ss := ss + AContainer.GetExternalName(pmr);
    AddChildNode(Result,ss);
  end;
  if locMthd.InheritsFrom(TPasFunction) then begin
    AddChildNode(
      Result,
      '>> ' + AContainer.GetExternalName(TPasFunctionType(locMthd.ProcType).ResultEl)
    );
  end;
end;

class function TMethodDefinitionPainter.CanHandle(AObj : TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and AObj.InheritsFrom(TPasProcedure);
end;

{ TInterfaceDefinitionPainter }

function TInterfaceDefinitionPainter.Paint(
  AContainer : TwstPasTreeContainer;
  AObj : TPasElement;
  AParent: TTreeNode
): TTreeNode;
var
  locObj : TPasClassType;
  locMthd : TPasProcedure;
  i : Integer;
  memberList : TList2;
  bindingsNode : TTreeNode;
  b : TwstBinding;
begin
  locObj := AObj as TPasClassType;
  Result := inherited Paint(AContainer, locObj, AParent);
  Result.ImageIndex := IMG_INTF_DEF;
  Result.StateIndex := IMG_INTF_DEF;
  Result.SelectedIndex := IMG_INTF_DEF;
  memberList := locObj.Members;
  for i := 0 to Pred(memberList.Count) do begin
    if TPasElement(memberList[i]).InheritsFrom(TPasProcedure) then begin
      locMthd := TPasProcedure(memberList[i]);
      FindPainter(locMthd).Paint(AContainer,locMthd,Result);
    end;
  end;
  i := 0;
  bindingsNode := AddChildNode(Result,'Bindings >');
    while True do begin
      b := AContainer.FindBinding(locObj,i);
      if ( b = nil ) then
        Break;
      Inc(i);
      FindPainter(b).Paint(AContainer,b,bindingsNode);
    end;
end;

class function TInterfaceDefinitionPainter.CanHandle(AObj : TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and
            AObj.InheritsFrom(TPasClassType) and
            ( TPasClassType(AObj).ObjKind = okInterface );
end;
  
{ TAbstractConstantDefinitionPainter }

function TAbstractConstantDefinitionPainter.Paint(
  AContainer : TwstPasTreeContainer;
  AObj: TPasElement;
  AParent: TTreeNode
): TTreeNode;
begin
  Result := inherited Paint(AContainer,AObj, AParent);
  Result.ImageIndex := IMG_CONST_ITEM;
  Result.StateIndex := IMG_CONST_ITEM;
  Result.SelectedIndex := IMG_CONST_ITEM;
end;

class function TAbstractConstantDefinitionPainter.CanHandle(AObj : TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and AObj.InheritsFrom(TPasConst);
end;
  
{ TTypeSymbolPainter }

function TTypeSymbolPainter.Paint(
  AContainer : TwstPasTreeContainer;
  AObj: TPasElement;
  AParent: TTreeNode
): TTreeNode;
begin
  Result := inherited Paint(AContainer, AObj, AParent);
  Result.ImageIndex := IMG_TYPE_DEF;
  Result.StateIndex := IMG_TYPE_DEF;
  Result.SelectedIndex := IMG_TYPE_DEF;
end;

{ TClassTypeDefinitionPainter }

function TClassTypeDefinitionPainter.Paint(
  AContainer : TwstPasTreeContainer;
  AObj: TPasElement;
  AParent: TTreeNode
): TTreeNode;
var
  locObj : TPasClassType;
  locProp : TPasProperty;
  i : Integer;
  s : string;
begin
  locObj := AObj as TPasClassType;
  Result := inherited Paint(AContainer,locObj, AParent);
  if Assigned(locObj.AncestorType) then begin
    Result.Text := Format('%s (%s)',[AContainer.GetExternalName(locObj),AContainer.GetExternalName(locObj.AncestorType)]);
  end;
  for i := 0 to Pred(locObj.Members.Count) do begin
    if TPasElement(locObj.Members[i]).InheritsFrom(TPasProperty) then begin
      locProp := TPasProperty(locObj.Members[i]);
      s := Format('%s : %s',[AContainer.GetExternalName(locProp),AContainer.GetExternalName(locProp.VarType)]);
      if AContainer.IsAttributeProperty(locProp) then begin
        s := s + ' ( Attribute )';
      end;
      AddChildNode(Result,s);
    end;
  end;
end;

class function TClassTypeDefinitionPainter.CanHandle(AObj : TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and
            ( AObj.InheritsFrom(TPasClassType) and ( TPasClassType(AObj).ObjKind = okClass ) ) and
            ( not AObj.InheritsFrom(TPasNativeClassType) );
end;
  
{ TEnumTypeDefinitionPainter }

function TEnumTypeDefinitionPainter.Paint(
  AContainer : TwstPasTreeContainer;
  AObj    : TPasElement;
  AParent : TTreeNode
): TTreeNode;
var
  locObj : TPasEnumType;
  locItem : TPasEnumValue;
  i : Integer;
  locNode : TTreeNode;
begin
  locObj := AObj as TPasEnumType;
  Result := inherited Paint(AContainer, locObj, AParent);
  Result.ImageIndex := IMG_ENUM;
  Result.StateIndex := IMG_ENUM;
  Result.SelectedIndex := IMG_ENUM;
  for i := 0 to Pred(locObj.Values.Count) do begin
    locItem := TPasEnumValue(locObj.Values[i]);
    locNode := AddChildNode(Result,AContainer.GetExternalName(locItem));
    locNode.ImageIndex := IMG_ENUM_ITEM;
    locNode.StateIndex := IMG_ENUM_ITEM;
    locNode.SelectedIndex := IMG_ENUM_ITEM;
  end;
end;

class function TEnumTypeDefinitionPainter.CanHandle(AObj : TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and AObj.InheritsFrom(TPasEnumType);
end;
  
  
{ TTypeAliasDefinitionPainter }

class function TTypeAliasDefinitionPainter.CanHandle(AObj : TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and AObj.InheritsFrom(TPasAliasType);
end;
  
{ TAnyTypeDefinitionPainter }

class function TAnyTypeDefinitionPainter.CanHandle(AObj : TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and AObj.InheritsFrom(TPasUnresolvedTypeRef);
end;
  
{ TPackagePainter }

function TPackagePainter.Paint(
  AContainer : TwstPasTreeContainer;
  AObj    : TPasElement;
  AParent : TTreeNode
):TTreeNode;
var
  objPtr : ISymbolPainter;
  i : Integer;
  locImportedNode : TTreeNode;
  locModule : TPasModule;
begin
  Result := AParent;
  objPtr := FindPainter(AContainer.CurrentModule) ;
  if Assigned(objPtr) then
    objPtr.Paint(AContainer,AContainer.CurrentModule,Result);
  if (AContainer.Package.Modules.Count > 1) then begin
    locImportedNode := nil;
    for i := 0 to AContainer.Package.Modules.Count - 1 do begin
      locModule := TPasModule(AContainer.Package.Modules[i]);
      if (locModule <> AContainer.CurrentModule) and
         not(locModule.InheritsFrom(TPasNativeModule))
      then begin
        if (locImportedNode = nil) then
          locImportedNode := AddChildNode(AParent,'Imported Schemas');
        objPtr.Paint(AContainer,locModule,locImportedNode);
      end;
    end;
  end;
end;

class function TPackagePainter.CanHandle(AObj : TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and AObj.InheritsFrom(TPasPackage);
end;

function FindPainter(AObj: TPasElement): ISymbolPainter;
begin
  Result := nil;
  if Assigned(AObj) then begin
    FPainterRegistryInst.FindHandler(AObj,Result);
  end;
end;

{ TPainterRegistry }

function TPainterRegistry.FindHanlderIndex(AObj: TObject): Integer;
var
  i : Integer;
begin
  for i := 0 to Pred(FList.Count) do begin
    if TSymbolPainterClass(FList[i]).CanHandle(AObj) then begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

constructor TPainterRegistry.Create();
begin
  FList := TClassList.Create();
end;

destructor TPainterRegistry.Destroy();
begin
  FreeAndNil(FList);
  inherited Destroy();
end;

procedure TPainterRegistry.RegisterHandler(APainterClass: TSymbolPainterClass);
begin
  if ( FList.IndexOf(APainterClass) = -1 ) then begin
    FList.Add(APainterClass);
  end;
end;

function TPainterRegistry.FindHandler(AObj: TObject; out AHandler: ISymbolPainter): Boolean;
var
  i : Integer;
begin
  AHandler := nil;
  i := FindHanlderIndex(AObj);
  Result := ( i >= 0 );
  if Result then begin
    AHandler := TSymbolPainterClass(FList[i]).Create();
  end;
end;

function TAbstractSymbolPainter.Paint(
  AContainer : TwstPasTreeContainer;
  AObj    : TPasElement;
  AParent : TTreeNode
):TTreeNode;
begin
  Assert(Assigned(AParent));
  if Assigned(AObj) then begin
    Result := AddChildNode(AParent,AContainer.GetExternalName(AObj));
    Result.Data := AObj;
    Result.ImageIndex := -1;
    Result.StateIndex := -1;
    Result.SelectedIndex := -1;
  end;
end;

class function TAbstractSymbolPainter.CanHandle(AObj : TObject): Boolean;
begin
  Result := Assigned(AObj) and AObj.InheritsFrom(TPasElement);
end;

{ TSymbolPainter }

constructor TSymbolPainter.Create();
begin

end;



initialization
  FPainterRegistryInst := TPainterRegistry.Create();
  FPainterRegistryInst.RegisterHandler(TPackagePainter);
  FPainterRegistryInst.RegisterHandler(TModulePainter);
  FPainterRegistryInst.RegisterHandler(TAnyTypeDefinitionPainter);
  FPainterRegistryInst.RegisterHandler(TTypeAliasDefinitionPainter);
  FPainterRegistryInst.RegisterHandler(TEnumTypeDefinitionPainter);
  FPainterRegistryInst.RegisterHandler(TClassTypeDefinitionPainter);
  //FPainterRegistryInst.RegisterHandler(TAbstractConstantDefinitionPainter);
  FPainterRegistryInst.RegisterHandler(TInterfaceDefinitionPainter);
  FPainterRegistryInst.RegisterHandler(TMethodDefinitionPainter);
  FPainterRegistryInst.RegisterHandler(TPasNativeSimpleTypePainter);
  FPainterRegistryInst.RegisterHandler(TBindingPainter);
  FPainterRegistryInst.RegisterHandler(TArrayTypeDefinitionPainter);
  FPainterRegistryInst.RegisterHandler(TRecordTypeDefinitionPainter);

finalization
  FreeAndNil(FPainterRegistryInst);
  
end.
