unit wst_delphi_xml;

interface
uses
  SysUtils, Classes, xmldom, XMLIntf;

const
  LineEnding = sLineBreak;

type

  TDOMNode = IDOMNode;
  TDOMNodeList = IDOMNodeList;
  TDOMNamedNodeMap  = IDOMNamedNodeMap;
  TDOMDocument = IDOMDocument;
  TXMLDocument = TDOMDocument;
  TDOMElement = IDOMElement;

  function FindNode(ANode : TDOMNode; const ANodeName : string):TDOMNode;
  function GetNodeItemsCount(const ANode : TDOMNode): Integer;
  function GetNodeListCount(ANodeList : TDOMNodeList) : Integer ;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function GetNodeListCount(ANodeList : TDOMNamedNodeMap) : Integer ;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure ReleaseDomNode(ADomNode : IInterface);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure ReleaseDomNode(var ADomNode : TXMLDocument);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}

  function CreateDoc() : TXMLDocument ;
  procedure WriteXML(Element: TDOMNode; const AFileName: String);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure WriteXML(Element: TDOMNode; AStream: TStream); overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function ReadXMLFile(AStream : TStream) : TXMLDocument;overload;
  procedure WriteXMLFile(doc: TXMLDocument; const AFileName: String); overload;
  procedure WriteXMLFile(ADoc : TXMLDocument; AStream : TStream);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure ReadXMLFile(out ADoc : TXMLDocument; AStream : TStream);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure ReadXMLFile(out ADoc: TXMLDocument; const AFilename: String);overload;
  function ReadXMLFile(const AFilename: String) :  TXMLDocument;overload;
  function NodeToBuffer(ANode : TDOMNode):string ;

  function FilterList(const ALIst : IDOMNodeList; const ANodeName : DOMString):IDOMNodeList;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function FilterList(const ANode : TDOMNode; const ANodeName : DOMString):IDOMNodeList;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function SelectSingleNode(
    const AXPathExpression : DOMString;
    const AContextNode     : TDOMNode;
    const AErrorIfMore     : Boolean
  ) : TDOMNode;

resourcestring
  SERR_XpathExpectingOneNode = 'Xpath expression expecting a single node while got %d node : %s.';

implementation
uses XmlDoc;

function FindNode(ANode : TDOMNode; const ANodeName : string):TDOMNode;
var
  i, c : Integer;
  lst : TDOMNodeList;
begin
  Result := nil;
  if ANode.hasChildNodes then begin
    lst := ANode.childNodes;
    c := lst.length;
    for i  := 0 to Pred(c) do begin
      if ( ANodeName = lst.item[i].nodeName ) then begin
        Result := lst[i];
        Break;
      end;
    end;
  end;
end;

procedure WriteXMLFile(doc: TXMLDocument; const AFileName: String);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    WriteXMLFile(doc, fs);
  finally
    fs.Free;
  end;
end;

procedure WriteXMLFile(ADoc : TXMLDocument; AStream : TStream);
begin
  (ADoc as IDOMPersist).saveToStream(AStream);
end;

procedure WriteXML(Element: TDOMNode; const AFileName: String);
begin
  WriteXMLFile(TXMLDocument(Element), AFileName);
end;

procedure WriteXML(Element: TDOMNode; AStream: TStream);
begin
  WriteXMLFile(TXMLDocument(Element), AStream);
end;

procedure ReadXMLFile(out ADoc : TXMLDocument; AStream : TStream);
begin
  ADoc := CreateDoc();
  (ADoc as IDOMPersist).loadFromStream(AStream);
end;

function ReadXMLFile(AStream : TStream) : TXMLDocument;
begin
  ReadXMLFile(Result,AStream);
end;

procedure ReadXMLFile(out ADoc: TXMLDocument; const AFilename: String);
var
  FileStream: TStream;
begin
  ADoc := nil;
  FileStream := TFileStream.Create(AFilename, fmOpenRead+fmShareDenyWrite);
  try
    ReadXMLFile(ADoc, FileStream);
  finally
    FileStream.Free;
  end;
end;

function ReadXMLFile(const AFilename: String) :  TXMLDocument;
begin
  ReadXMLFile(Result, AFilename);
end;

function GetNodeItemsCount(const ANode : TDOMNode): Integer;
begin
  if ANode.HasChildNodes then begin
    Result := ANode.childNodes.length;
  end else begin
    Result := 0;
  end;
end;

function GetNodeListCount(ANodeList : TDOMNodeList) : Integer ;overload;
begin
  Result := ANodeList.length;
end;

function GetNodeListCount(ANodeList : TDOMNamedNodeMap) : Integer ;overload;
begin
  Result := ANodeList.length;
end;

procedure ReleaseDomNode(ADomNode : IInterface);
begin
end;

procedure ReleaseDomNode(var ADomNode : TXMLDocument);
begin

end;

function CreateDoc() : TXMLDocument ;
var
  locDoc : IXMLDocument;
begin
  locDoc := XmlDoc.TXMLDocument.Create(nil);
  locDoc.Active := True;
  Result := locDoc.DOMDocument;
end;

function NodeToBuffer(ANode : TDOMNode):string ;
var
  locNodeEx : IDOMNodeEx;
begin
  if Supports(ANode,IDOMNodeEx,locNodeEx) then begin
    Result := locNodeEx.xml;
  end else begin
    raise Exception.Create('This Xml library do not provide "IDOMNodeEx" support.');
  end;
end;

type
  TDOMNodeSelectListImp = class(TInterfacedObject,IDOMNodeList)
  private
    FItemName : widestring;
    FInnerList : IDOMNodeList;
    FCount : Integer;
  private
    function internal_get_item(index: Integer): IDOMNode;
  protected
    function get_item(index: Integer): IDOMNode; safecall;
    function get_length: Integer; safecall;
  public
    constructor Create(
      const AInnerList : IDOMNodeList;
      const AItemName  : widestring
    );
  end;

function FilterList(const ALIst : IDOMNodeList; const ANodeName : DOMString):IDOMNodeList ;
begin
  Result := TDOMNodeSelectListImp.Create(ALIst,ANodeName);
end;

function FilterList(const ANode : TDOMNode; const ANodeName : DOMString):IDOMNodeList;
begin
  Result := FilterList(ANode.ChildNodes,ANodeName);
end;

function SelectSingleNode(
  const AXPathExpression : DOMString;
  const AContextNode     : TDOMNode;
  const AErrorIfMore     : Boolean
) : TDOMNode;
var
  locSelect : IDOMNodeSelect;
  ns : TDOMNodeList;
begin
  Result := nil;
  locSelect := AContextNode as IDOMNodeSelect;
  ns := locSelect.selectNodes(AXPathExpression);
  if ( ns <> nil ) and ( ns.length > 0 ) then begin
    if AErrorIfMore and ( ns.length > 1 ) then
      raise Exception.CreateFmt(SERR_XpathExpectingOneNode,[ns.length,AXPathExpression]);
    Result := ns[0];
  end;
end;

{ TDOMNodeSelectListImp }

constructor TDOMNodeSelectListImp.Create(
  const AInnerList: IDOMNodeList;
  const AItemName: widestring
);
begin
  Assert(AInnerList <> nil);
  FInnerList := AInnerList;
  FItemName := AItemName;
  FCount := -1;
end;

function TDOMNodeSelectListImp.get_item(index: Integer): IDOMNode;
begin
  Result := internal_get_item(index);
  if ( Result = nil ) then
    raise Exception.CreateFmt('Invalid item at %d.',[index]);
end;

function TDOMNodeSelectListImp.get_length() : Integer;
begin
  if ( FCount >= 0 ) then begin
    Result := FCount;
  end else begin
    FCount := 0;
    while Assigned(internal_get_item(FCount)) do begin
      Inc(FCount);
    end;
    Result := FCount;
  end;
end;

function TDOMNodeSelectListImp.internal_get_item(index: Integer): IDOMNode;
var
  i : Integer;
  crt : IDOMNode;
begin
  Result := nil;
  if ( FInnerList.length > 0 ) then begin
    i := -1;
    crt := FInnerList.item[0];
    while ( crt <> nil ) do begin
      if ( FItemName = crt.nodeName ) then begin
        Inc(i);
        if ( i = index ) then begin
          Result := crt;
          Break;
        end;
      end;
      crt := crt.nextSibling;
    end;
  end;
end;

end.
