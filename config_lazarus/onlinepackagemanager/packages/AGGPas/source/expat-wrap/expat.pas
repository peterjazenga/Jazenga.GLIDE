
unit Expat;

interface

{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

const

{$IFDEF XML_UNICODE}
  LIBPOSTFIX = 'w';
{$ELSE}
  LIBPOSTFIX = '';
{$ENDIF}

{$IFDEF LINUX }
  EXPATLIB = 'libexpat' + LIBPOSTFIX + '.so';

{$ELSE}
  EXPATLIB = 'libexpat' + LIBPOSTFIX + '.dll';

{$ENDIF}{milano}


{=   > EXPAT.H <=}

{+// }
{-Copyright (c) 1998, 1999, 2000 Thai Open Source Software Center Ltd }
{-See the file COPYING for copying permission. }
{=    }

// for compatibility with C enum :

{$MINENUMSIZE 4}

type
  TXMLParser = Pointer;
  PXMLParser = ^TXMLParser;
  PPXMLParser = ^PXMLParser;

  XML_Parser = TXMLParser;

{ The XML_Status enum gives the possible return values for several API functions. }
  XML_Status = (XML_STATUS_ERROR ,XML_STATUS_OK ,XML_STATUS_SUSPENDED );
  
{$IFDEF XML_UNICODE} {+// Information is UTF-16 encoded.*/ }
  TXMLChar = WideChar;
  PXMLChar = PWideChar;
  {$IFDEF XML_UNICODE_WCHAR_T}
  TXMLLChar = WideChar;
  PXMLLChar = PWideChar;
  {$ELSE}
  TXMLLChar = Char;
  PXMLLChar = PChar;
  {$ENDIF}
{$ELSE}              {+// Information is UTF-8 encoded.*/ }
  TXMLChar = Char;
  TXMLLChar = Char;
  PXMLChar = PChar;
  PXMLLChar = PChar;
{$ENDIF}
  //PXMLChar = ^TXMLChar;
  //PXMLLChar = ^TXMLLChar;

  TXMLContentType = (
    XML_CTYPE_ILLEGAL, //dummy, to make XML_CTYPE_EMPTY = 1
    XML_CTYPE_EMPTY,
    XML_CTYPE_ANY,
    XML_CTYPE_MIXED,
    XML_CTYPE_NAME,
    XML_CTYPE_CHOICE,
    XML_CTYPE_SEQ);

  TXMLContentQuant = (
    XML_CQUANT_NONE,
    XML_CQUANT_OPT,
    XML_CQUANT_REP,
    XML_CQUANT_PLUS);

  {+// If type == XML_CTYPE_EMPTY or XML_CTYPE_ANY, then quant will be }
  {-   XML_CQUANT_NONE, and the other fields will be zero or NULL. }
  {-   If type == XML_CTYPE_MIXED, then quant will be NONE or REP and }
  {-   numchildren will contain number of elements that may be mixed in }
  {-   and children point to an array of XMLContent cells that will be }
  {-   all of XML_CTYPE_NAME type with no quantification. }

  {-   If type == XML_CTYPE_NAME, then the name points to the name, and }
  {-   the numchildren field will be zero and children will be NULL. The }
  {-   quant fields indicates any quantifiers placed on the name. }

  {-   CHOICE and SEQ will have name NULL, the number of children in }
  {-   numchildren and children will point, recursively, to an array }
  {-   of XMLContent cells. }

  {-   The EMPTY, ANY, and MIXED types will only occur at top level. }
  {=    }
  PXMLContent = ^TXMLContent;
  PXMLContents = ^TXMLContents;
  TXMLCp = packed record
    Type_:      TXMLContentType;
    Quant:      TXMLContentQuant;
    Name:       PXMLChar;
    NumChildren:Cardinal;
    Children:   PXMLContents;
    end;
  TXMLContent = TXMLCp;
  TXMLContents = packed array[0.. (MaxInt div SizeOf(TXMLContent)) - 1] of TXMLContent;


  {+// This is called for an element declaration. See above for }
  {-   description of the model argument. It's the caller's responsibility }
  {-   to free model when finished with it. }
  {=    }
  TXMLElementDeclhandler = procedure(
    UserData: Pointer;
    const Name: PXMLChar;
    Model: PXMLContent); cdecl;

procedure XMLSetElementDeclhandler(
  Parser: TXMLParser;
  ElDecl: TXMLElementDeclHandler); cdecl;

type
  {+// }
  {-  The Attlist declaration handler is called for*each* attribute. So }
  {-  a single Attlist declaration with multiple attributes declared will }
  {-  generate multiple calls to this handler. The "default" parameter }
  {   may be NULL in the case of the "#IMPLIED" or "#REQUIRED" keyword. }
  {-  The "isrequired" parameter will be true and the default value will }
  {   be NULL in the case of "#REQUIRED". If "isrequired" is true and }
  {   default is non-NULL, then this is a "#FIXED" default. }
  {=    }
  TXMLAttlistDeclHandler = procedure(
    UserData: Pointer;
    const ElName: PXMLChar;
    const Attname, AttType, Dflt: PXMLChar;
    IsRequired: Integer); cdecl;

procedure XMLSetAttlistDeclHandler(
  Parser: TXMLParser;
  AttDecl: TXMLAttlistDeclHandler); cdecl;

type
  {+// The XML declaration handler is called for*both* XML declarations and }
  {-   text declarations. The way to distinguish is that the version parameter }
  {-   will be null for text declarations. The encoding parameter may be null }
  {-   for XML declarations. The standalone parameter will be -1, 0, or 1 }
  {-   indicating respectively that there was no standalone parameter in }
  {-   the declaration, that it was given as no, or that it was given as yes. }
  {=    }
  TXMLXmlDeclHandler = procedure(
    UserData: Pointer;
    const Version, Encoding: PXMLChar;
    Standalone: Integer); cdecl;

procedure XMLSetXmlDeclHandler(
  Parser: TXMLParser;
  XmlDecl: TXMLXMLdeclHandler); cdecl;

{+// Constructs a new Parser; encoding is the encoding specified by the external }
{=      protocol or null if there is none specified. }
function XML_ParserCreate(const Encoding: PXMLChar): TXMLParser; cdecl;

{+// Constructs a new Parser and namespace processor. Element type names }
{-   and attribute names that belong to a namespace will be expanded; }
{-   unprefixed attribute names are never expanded; unprefixed element type }
{-   names are expanded only if there is a default namespace. The expanded }
{-   name is the concatenation of the namespace URI, the namespace separator character, }
{-   and the local part of the name. If the namespace separator is '\0' then }
{-   the namespace URI and the local part will be concatenated without any }
{-   separator. When a namespace is not declared, the name and prefix will be }
{=      passed through without expansion. }
function XMLParserCreateNS(
  const Encoding: PXMLChar;
  NamespaceSeparator: TXMLChar): TXMLParser; cdecl;

{+// Constructs a new Parser using the memory management suit referred to }
{-   by memsuite. If memsuite is NULL, then use the standard library memory }
{-   suite. If namespaceSeparator is non-NULL it creates a Parser with }
{-   namespace processing as described above. The character pointed at }
{-   will serve as the namespace separator. }

{-   All further memory operations used for the created Parser will come from }
{-   the given suite. }
{=    }
type
  TMallocFcn = function(Size: Integer): Pointer; cdecl;
  TReallocFcn = function(Ptr: Pointer; Size: Integer): Pointer; cdecl;
  TFreeFcn = procedure(Ptr: Pointer); cdecl;

  PXMLMemoryHandlingSuite = ^TXMLMemoryHandlingSuite;
  TXMLMemoryHandlingSuite = packed record
    MallocFcn: TMallocFcn;
    ReallocFcn: TReallocFcn;
    FreeFcn: TFReeFcn;
    end;

function XMLParserCreateMM(
  const Encoding: PXMLChar;
  const MemSuite: PXMLMemoryHandlingSuite;
  const NamespaceSeparator: PXMLChar): TXMLParser; cdecl;

{+// Prepare a parser object to be re-used.  This is particularly }
{-   valuable when memory allocation overhead is disproportionatly high, }
{-   such as when a large number of small documnents need to be parsed. }
{-   All handlers are cleared from the parser, except for the }
{-   unknownEncodingHandler. The parser's external state is re-initialized }
{=   except for the values of ns, ns_triplets and useForeignDTD. }
(*function XMLParserReset(
  Parser: TXMLParser;
  const Encoding: PXMLChar): Integer;*){milano}

type
  {+// attrs is array of name/value pairs, terminated by 0; }
  {=      names and values are 0 terminated. }
  TAttrs = array[0..(MaxInt div SizeOf(PXMLChar)) - 1] of PXMLChar;
  PAttrs = ^TAttrs;

  TXMLStartElementHandler = procedure(
    UserData: Pointer;
    const Name: PXMLChar;
    const Atts: TAttrs); cdecl;

  TXMLEndElementHandler = procedure(
    UserData: Pointer;
    const Name: PXMLChar); cdecl;

  {+// s is not 0 terminated.*/ }
  TXMLCharacterDataHandler = procedure(
    UserData: Pointer;
    const S: PXMLChar;
    Len: integer); cdecl;

  {+// target and data are 0 terminated*/ }
  TXMLProcessingInstructionHandler = procedure(
    UserData: Pointer;
    const Target: PXMLChar;
    const Data: PXMLChar); cdecl;

  {+// data is 0 terminated*/ }
  TXMLCommentHandler = procedure(
    UserData: Pointer;
    const Data: PXMLChar); cdecl;

  TXMLStartCdataSectionHandler = procedure(UserData: Pointer); cdecl;
  TXMLEndCdataSectionHandler = procedure(UserData: Pointer); cdecl;

  {+// This is called for any characters in the XML document for }
  {-   which there is no applicable handler. This includes both }
  {-   characters that are part of markup which is of a kind that is }
  {-   not reported (comments, markup declarations), or characters }
  {-   that are part of a construct which could be reported but }
  {-   for which no handler has been supplied. The characters are passed }
  {-   exactly as they were in the XML document except that }
  {-   they will be encoded in UTF-8. Line boundaries are not normalized. }
  {-   Note that a byte order mark character is not passed to the default handler. }
  {-   There are no guarantees about how characters are divided between calls }
  {-   to the default handler: for example, a comment might be split between }
  {=      multiple calls. }
  TXMLDefaultHandler = procedure(
    UserData: Pointer;
    const S: PXMLChar;
    Len: Integer); cdecl;

  {+// This is called for the start of the DOCTYPE declaration, before }
  {=      any DTD or internal subset is parsed. }
  TXMLStartDoctypeDeclHandler = procedure(
    UserData: Pointer;
    const DoctypeName: PXMLChar;
    const SysId: PXMLChar;
    const PubId: PXMLChar;
    HasInternalSubset: Integer); cdecl;

  {+// This is called for the start of the DOCTYPE declaration when the }
  {=      closing > is encountered, but after processing any external subset. }
  TXMLEndDoctypeDeclHandler = procedure(UserData: Pointer); cdecl;

  {+// This is called for entity declarations. The is_parameter_entity }
  {-   argument will be non-zero if the entity is a parameter entity, zero }
  {-   otherwise. }

  {-   For internal entities (<!ENTITY foo "bar">), value will }
  {-   be non-null and systemId, publicID, and notationName will be null. }
  {-   The value string is NOT null terminated; the length is provided in }
  {-   the value_length argument. Since it is legal to have zero-length }
  {-   values, do not use this argument to test for internal entities. }

  {-   For external entities, value will be null and systemId will be non-null. }
  {-   The publicId argument will be null unless a public identifier was }
  {-   provided. The notationName argument will have a non-null value only }
  {-   for unparsed entity declarations. }
  {=    }
  TXMLEntityDeclHandler = procedure(
    UserData: Pointer;
    const EntityName: PXMLChar;
    IsParameterEntity: Integer;
    const Value: PXMLChar;
    ValueLength: Integer;
		const Base: PXMLChar;
    const SystemId: PXMLChar;
    const PublicId: PXMLChar;
    const NotationName: PXMLChar); cdecl;

procedure XMLSetEntityDeclHandler(
  Parser: TXMLParser;
  Handler: TXMLEntityDeclHandler); cdecl;

type
  {+// OBSOLETE -- OBSOLETE -- OBSOLETE }
  {-   This handler has been superceded by the EntityDeclHandler above. }
  {-   It is provided here for backward compatibility. }
  {-   This is called for a declaration of an unparsed (NDATA) }
  {-   entity. The base argument is whatever was set by XMLSetBase. }
  {-   The entityName, systemId and notationName arguments will never be null. }
  {=      The other arguments may be. }
  TXMLUnparsedEntityDeclHandler = procedure(
    UserData: Pointer;
    const EntityName: PXMLChar;
    const Base: PXMLChar;
    const SystemId: PXMLChar;
    const PublicId: PXMLChar;
    const NotationName: PXMLChar); cdecl;

  {+// This is called for a declaration of notation. }
  {-   The base argument is whatever was set by XMLSetBase. }
  {=      The notationName will never be null. The other arguments can be. }
  TXMLNotationDeclHandler = procedure(
    UserData: Pointer;
    const NotationName: PXMLChar;
    const Base: PXMLChar;
    const SystemId: PXMLChar;
    const PublicId: PXMLChar); cdecl;

  {+// When namespace processing is enabled, these are called once for }
  {-   each namespace declaration. The call to the start and end element }
  {-   handlers occur between the calls to the start and end namespace }
  {-   declaration handlers. For an xmlns attribute, prefix will be null. }
  {=      For an xmlns="" attribute, uri will be null. }
  TXMLStartNamespaceDeclHandler = procedure(
    UserData: pointer;
    const Prefix: PXMLChar;
    const Uri: PXMLChar); cdecl;

  TXMLEndNamespaceDeclHandler = procedure(
    UserData: Pointer;
    const Prefix: PXMLChar); cdecl;

  {+// This is called if the document is not standalone (it has an }
  {-   external subset or a reference to a parameter entity, but does not }
  {-   have standalone="yes"). If this handler returns 0, then processing }
  {-   will not continue, and the Parser will return a }
  {=      XML_ERROR_NOT_STANDALONE error. }
  TXMLNotStandaloneHandler = function(UserData: Pointer): Integer; cdecl;

  {+// This is called for a reference to an external parsed general entity. }
  {-   The referenced entity is not automatically parsed. }
  {-   The application can parse it immediately or later using }
  {-   XMLExternalEntityParserCreate. }
  {-   The Parser argument is the Parser parsing the entity containing the reference; }
  {-   it can be passed as the Parser argument to XMLExternalEntityParserCreate. }
  {-   The systemId argument is the system identifier as specified in the entity declaration; }
  {-   it will not be null. }
  {-   The base argument is the system identifier that should be used as the base for }
  {-   resolving systemId if systemId was relative; this is set by XMLSetBase; }
  {-   it may be null. }
  {-   The publicId argument is the public identifier as specified in the entity declaration, }
  {-   or null if none was specified; the whitespace in the public identifier }
  {-   will have been normalized as required by the XML spec. }
  {-   The context argument specifies the parsing context in the format }
  {-   expected by the context argument to }
  {-   XMLExternalEntityParserCreate; context is valid only until the handler }
  {-   returns, so if the referenced entity is to be parsed later, it must be copied. }
  {-   The handler should return 0 if processing should not continue because of }
  {-   a fatal error in the handling of the external entity. }
  {-   In this case the calling Parser will return an XML_ERROR_EXTERNAL_ENTITY_HANDLING }
  {-   error. }
  {=      Note that unlike other handlers the first argument is the Parser, not UserData. }
  TXMLExternalEntityRefHandler = function(
    Parser: TXMLParser;
    const Context: PXMLChar;
    const Base: PXMLChar;
    const SystemId: PXMLChar;
    const PublicId: PXMLChar): Integer; cdecl;

  {+// This is called in two situations: }
  {-   1) An entity reference is encountered for which no declaration }
  {-      has been read *and* this is not an error. }
  {-   2) An internal entity reference is read, but not expanded, because }
  {=      XML_SetDefaultHandler has been called. }
  TXMLSkippedEntityHandler = procedure(
    UserData: Pointer;
    const EntityName: PXMLChar;
    IsParameterEntity: Integer); cdecl;

  {+// This structure is filled in by the XMLUnknownEncodingHandler }
  {-   to provide information to the Parser about encodings that are unknown }
  {-   to the Parser. }
  {-   The map[b] member gives information about byte sequences }
  {-   whose first byte is b. }
  {-   If map[b] is c where c is >= 0, then b by itself encodes the Unicode scalar value c. }
  {-   If map[b] is -1, then the byte sequence is malformed. }
  {-   If map[b] is -n, where n >= 2, then b is the first byte of an n-byte }
  {-   sequence that encodes a single Unicode scalar value. }
  {-   The data member will be passed as the first argument to the convert function. }
  {-   The convert function is used to convert multibyte sequences; }
  {-   s will point to a n-byte sequence where map[(unsigned char)*s] == -n. }
  {-   The convert function must return the Unicode scalar value }
  {-   represented by this byte sequence or -1 if the byte sequence is malformed. }
  {-   The convert function may be null if the encoding is a single-byte encoding, }
  {-   that is if map[b] >= -1 for all bytes b. }
  {-   When the Parser is finished with the encoding, then if release is not null, }
  {-   it will call release passing it the data member; }
  {-   once release has been called, the convert function will not be called again. }

  {-   Expat places certain restrictions on the encodings that are supported }
  {-   using this mechanism. }

  {-   1. Every ASCII character that can appear in a well-formed XML document, }
  {-   other than the characters }

  (*   $@\^`{}~ *)

  {-   must be represented by a single byte, and that byte must be the }
  {-   same byte that represents that character in ASCII. }

  {-   2. No character may require more than 4 bytes to encode. }

  {-   3. All characters encoded must have Unicode scalar values <= 0xFFFF, }
  {-   (ie characters that would be encoded by surrogates in UTF-16 }
  {-   are not allowed). Note that this restriction doesn't apply to }
  {-   the built-in support for UTF-8 and UTF-16. }

  {-   4. No Unicode character may be encoded by more than one distinct sequence }
  {=   of bytes. }
  TConvertEncoding = function(Data: Pointer; S: Pchar): Integer; cdecl;
  TReleaseEncoding = procedure(Data: Pointer); cdecl;

  PXMLEncoding = ^TXMLEncoding;
  TXMLEncoding = packed record
    Map: array[0..255] of Integer;
    Data: Pointer;
    Convert: TConvertEncoding;
    Release: TReleaseEncoding;
    end;

  {+// This is called for an encoding that is unknown to the Parser. }
  {-   The encodingHandlerData argument is that which was passed as the }
  {-   second argument to XMLSetUnknownEncodingHandler. }
  {-   The name argument gives the name of the encoding as specified in }
  {-   the encoding declaration. }
  {-   If the callback can provide information about the encoding, }
  {-   it must fill in the XMLEncoding structure, and return 1. }
  {-   Otherwise it must return 0. }
  {-   If info does not describe a suitable encoding, }
  {=   then the Parser will return an XML_UNKNOWN_ENCODING error. }
  TXMLUnknownEncodingHandler = function(
    EncodingHandlerData: Pointer;
    const Name: PXMLChar;
    Info: PXMLEncoding): Integer; cdecl;

procedure XML_SetElementHandler(
  Parser: TXMLParser;
  Start: TXMLStartElementHandler;
  End_: TXMLEndElementHandler); cdecl;

procedure XMLSetStartElementhandler(
  Parser: TXMLParser;
  Handler: TXMLStartElementHandler); cdecl;

procedure XMLSetEndElementHandler(
  Parser: TXMLParser;
  Handler: TXMLEndElementHandler); cdecl;

procedure XML_SetCharacterDataHandler(
  Parser: TXMLParser;
  Handler: TXMLCharacterDataHandler); cdecl;

procedure XMLSetProcessingInstructionHandler(
  Parser: TXMLParser;
  Handler: TXMLProcessingInstructionHandler); cdecl;

procedure XMLSetCommentHandler(
  Parser: TXMLParser;
  Handler: TXMLCommentHandler); cdecl;

procedure XMLSetCdataSectionHandler(
  Parser: TXMLParser;
  Start: TXMLStartCdataSectionHandler;
  End_: TXMLEndCdataSectionHandler); cdecl;

procedure XMLSetStartCdataSectionHandler(
  Parser: TXMLParser;
  Start: TXMLStartCdataSectionHandler); cdecl;

procedure XMLSetEndCdataSectionHandler(
  Parser: TXMLParser;
  End_: TXMLEndCdataSectionHandler); cdecl;

{+// This sets the default handler and also inhibits expansion of internal entities. }
{-   The entity references will be passed to the default handler, or to the
{=   skipped entity handler, if one is set. }
procedure XMLSetdefaultHandler(
  Parser: TXMLParser;
  Handler: TXMLDefaultHandler); cdecl;

{+// This sets the default handler but does not inhibit expansion of internal entities. }
{=   The entity reference will not be passed to the default handler. }
procedure XMLSetDefaultHandlerExpand(
  Parser: TXMLParser;
  Handler: TXMLDefaultHandler); cdecl;

procedure XMLSetDoctypeDeclHandler(
  Parser: TXMLParser;
  Start: TXMLStartDoctypeDeclHandler;
  End_: TXMLEndDoctypeDeclHandler); cdecl;

procedure XMLSetStartDoctypeDeclHandler(
  Parser: TXMLParser;
  Start: TXMLStartDoctypeDeclHandler); cdecl;

procedure XMLSetEndDoctypeDeclHandler(
  Parser: TXMLParser;
  End_: TXMLEndDoctypeDeclHandler); cdecl;

procedure XMLSetUnparsedEntityDeclHandler(
  Parser: TXMLParser;
  Handler: TXMLUnparsedEntityDeclHandler); cdecl;

procedure XMLSetNotationDeclHandler(
  Parser: TXMLParser;
  Handler: TXMLNotationDeclHandler); cdecl;

procedure XMLSetNameSpaceDeclHandler(
  Parser: TXMLParser;
    Start: TXMLStartNamespaceDeclHandler;
    End_: TXMLEndNamespaceDeclHandler); cdecl;

procedure XMLSetStartNameSpaceDeclHandler(
  Parser: TXMLParser;
  Start: TXMLStartNamespaceDeclHandler); cdecl;

procedure XMLSetEndNameSpaceDeclHandler(
  Parser: TXMLParser;
  End_: TXMLEndNamespaceDeclHandler); cdecl;

procedure XMLSetNotstandaloneHandler(
  Parser: TXMLParser;
  Handler: TXMLNotStandaloneHandler); cdecl;

procedure XMLSetExternalEntityRefHandler(
  Parser: TXMLParser;
  Handler: TXMLExternalEntityRefHandler); cdecl;

{+// If a non-null value for arg is specified here, then it will be passed }
{-   as the first argument to the external entity ref handler instead }
{=   of the Parser object. }
procedure XMLSetExternalEntityRefHandlerArg(
  Parser: TXMLParser;
  Arg: Pointer); cdecl;

procedure XMLSetSkippedEntityHandler(
  Parser: TXMLParser;
  Handler: TXMLSkippedEntityHandler); cdecl;

{ EncodingHandlerData works like UserData }
procedure XMLSetUnknownEncodingHandler(
  Parser: TXMLPARSER;
  Handler: TXMLUnknownEncodingHandler;
  EncodingHandlerData: Pointer); cdecl;

{+// This can be called within a handler for a start element, end element, }
{-   processing instruction or character data. It causes the corresponding }
{=   markup to be passed to the default handler. }
procedure XMLDefaultCurrent(Parser: TXMLParser); cdecl;

{+// If do_nst is non-zero, and namespace processing is in effect, and }
{-   a name has a prefix (i.e. an explicit namespace qualifier) then }
{-   that name is returned as a triplet in a single }
{-   string separated by the separator character specified when the Parser }
{-   was created: URI + sep + local_name + sep + prefix. }

{-   If do_nst is zero, then namespace information is returned in the }
{-   default manner (URI + sep + local_name) whether or not the names }
{-   has a prefix. }
{=    }
procedure XMLSetReturnNSTriplet(
  Parser: TXMLParser;
  DoNst: Integer); cdecl;

{+// This value is passed as the UserData argument to callbacks.*/ }
procedure XML_SetUserData(
  Parser: TXMLParser;
  UserData: Pointer); cdecl;

{+// Returns the last value set by XMLSetUserData or null.*/ }
{    #define XMLGetUserData(Parser) (*(void **)(Parser)) }
function XMLGetUserData(Parser: TXMLParser): Pointer;

{+// This is equivalent to supplying an encoding argument }
{-   to XMLParserCreate. It must not be called after XMLParse }
{=   or XMLParseBuffer. }
function XMLSetEncoding(
  Parser: TXMLParser;
  const Encoding: PXMLChar): Integer; cdecl;

{+// If this function is called, then the Parser will be passed }
{-   as the first argument to callbacks instead of UserData. }
{=   The UserData will still be accessible using XMLGetUserData. }
procedure XMLUseParserAsHandlerArg(Parser: TXMLParser); cdecl;

{+// If useDTD == XML_TRUE is passed to this function, then the parser }
{-   will assume that there is an external subset, even if none is }
{-   specified in the document. In such a case the parser will call the }
{-   externalEntityRefHandler with a value of NULL for the systemId }
{-   argument (the publicId and context arguments will be NULL as well). }
{-   Note: If this function is called, then this must be done before }
{-     the first call to XML_Parse or XML_ParseBuffer, since it will }
{-     have no effect after that.  Returns }
{-     XML_ERROR_CANT_CHANGE_FEATURE_ONCE_PARSING. }
{-   Note: If the document does not have a DOCTYPE declaration at all, }
{-     then startDoctypeDeclHandler and endDoctypeDeclHandler will not }
{-     be called, despite an external subset being parsed. }
{-   Note: If XML_DTD is not defined when Expat is compiled, returns }
{=     XML_ERROR_FEATURE_REQUIRES_XML_DTD. }
procedure XMLUseForeignDTD(
  Parser: TXMLParser;
  useDTD: ByteBool); cdecl;

{+// Sets the base to be used for resolving relative URIs in system identifiers in }
{-   declarations. Resolving relative identifiers is left to the application: }
{-   this value will be passed through as the base argument to the }
{-   XMLExternalEntityRefHandler, XMLNotationDeclHandler }
{-   and XMLUnparsedEntityDeclHandler. The base argument will be copied. }
{=   Returns zero if out of memory, non-zero otherwise. }
function XMLSetBase(
  Parser: TXMLParser;
  const Base: PXMLChar): Integer; cdecl;

function XMLGetBase(Parser: TXMLParser): PXMLChar; cdecl;

{+// Returns the number of the attribute/value pairs passed in last call }
{-   to the XMLStartElementHandler that were specified in the start-tag }
{-   rather than defaulted. Each attribute/value pair counts as 2; thus }
{-   this correspondds to an index into the atts array passed to the }
{=   XMLStartElementHandler. }
function XMLGetSpecifiedAttributeCount(Parser: TXMLParser): Integer; cdecl;

{+// Returns the index of the ID attribute passed in the last call to }
{-   XMLStartElementHandler, or -1 if there is no ID attribute. Each }
{-   attribute/value pair counts as 2; thus this correspondds to an index }
{=   into the atts array passed to the XMLStartElementHandler. }
function XMLGetIdAttributeIndex(Parser: TXMLParser): Integer; cdecl;

{+// Parses some input. Returns 0 if a fatal error is detected. }
{-   The last call to XMLParse must have isFinal true; }
{=   len may be zero for this call (or any other). }
function XML_Parse(
  Parser: TXMLParser;
  const S: PChar;
  Len: Integer;
  IsFinal: Integer): XML_Status; cdecl;

function XMLGetBuffer(
  Parser: TXMLParser;
  Len: Integer): Pointer; cdecl;

function XMLParseBuffer(
  Parser: TXMLParser;
  Len: Integer;
  IsFinal: Integer): Integer; cdecl;

{+// Creates an XMLParser object that can parse an external general entity; }
{-   context is a '\0'-terminated string specifying the parse context; }
{-   encoding is a '\0'-terminated string giving the name of the externally specified encoding, }
{-   or null if there is no externally specified encoding. }
{-   The context string consists of a sequence of tokens separated by formfeeds (\f); }
{-   a token consisting of a name specifies that the general entity of the name }
{-   is open; a token of the form prefix=uri specifies the namespace for a particular }
{-   prefix; a token of the form =uri specifies the default namespace. }
{-   This can be called at any point after the first call to an ExternalEntityRefHandler }
{-   so longer as the Parser has not yet been freed. }
{-   The new Parser is completely independent and may safely be used in a separate thread. }
{-   The handlers and UserData are initialized from the Parser argument. }
{=   Returns 0 if out of memory. Otherwise returns a new XMLParser object. }
function XMLExternalEntityParserCreate(
  Parser: TXMLParser;
  const Context: PXMLChar;
  const Encoding: PXMLChar): TXMLParser; cdecl;

type
  TXMLParamEntityParsing = (
    XML_PARAM_ENTITY_PARSING_NEVER,
    XML_PARAM_ENTITY_PARSING_UNLESS_STANDALONE,
    XML_PARAM_ENTITY_PARSING_ALWAYS);

{+// Controls parsing of parameter entities (including the external DTD }
{-   subset). If parsing of parameter entities is enabled, then references }
{-   to external parameter entities (including the external DTD subset) }
{-   will be passed to the handler set with }
{-   XMLSetExternalEntityRefHandler. The context passed will be 0. }
{-   Unlike external general entities, external parameter entities can only }
{-   be parsed synchronously. If the external parameter entity is to be }
{-   parsed, it must be parsed during the call to the external entity ref }
{-   handler: the complete sequence of XMLExternalEntityParserCreate, }
{-   XMLParse/XMLParseBuffer and XMLParserFree calls must be made during }
{-   this call. After XMLExternalEntityParserCreate has been called to }
{-   create the Parser for the external parameter entity (context must be 0 }
{-   for this call), it is illegal to make any calls on the old Parser }
{-   until XMLParserFree has been called on the newly created Parser. If }
{-   the library has been compiled without support for parameter entity }
{-   parsing (ie without XML_DTD being defined), then }
{-   XMLSetParamEntityParsing will return 0 if parsing of parameter }
{=   entities is requested; otherwise it will return non-zero. }
function XMLSetParamEntityParsing(
  Parser: TXMLParser;
  Parsing: TXMLParamEntityParsing): Integer; cdecl;

type
  TXMLError = (
    XML_ERROR_NONE,
    XML_ERROR_NO_MEMORY,
    XML_ERROR_SYNTAX,
    XML_ERROR_NO_ELEMENTS,
    XML_ERROR_INVALID_TOKEN,
    XML_ERROR_UNCLOSED_TOKEN,
    XML_ERROR_PARTIAL_CHAR,
    XML_ERROR_TAG_MISMATCH,
    XML_ERROR_DUPLICATE_ATTRIBUTE,
    XML_ERROR_JUNK_AFTER_DOC_ELEMENT,
    XML_ERROR_PARAM_ENTITY_REF,
    XML_ERROR_UNDEFINED_ENTITY,
    XML_ERROR_RECURSIVE_ENTITY_REF,
    XML_ERROR_ASYNC_ENTITY,
    XML_ERROR_BAD_CHAR_REF,
    XML_ERROR_BINARY_ENTITY_REF,
    XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF,
    XML_ERROR_MISPLACED_XML_PI,
    XML_ERROR_UNKNOWN_ENCODING,
    XML_ERROR_INCORRECT_ENCODING,
    XML_ERROR_UNCLOSED_CDATA_SECTION,
    XML_ERROR_EXTERNAL_ENTITY_HANDLING,
    XML_ERROR_NOT_STANDALONE,
    XML_ERROR_UNEXPECTED_STATE);

  XML_Error = TXMLError;

{+// If XMLParse or XMLParseBuffer have returned 0, then XMLGetErrorCode  );
{=   returns information about the error. }
function XML_GetErrorCode(Parser: TXMLParser): TXMLError; cdecl;

{+// These functions return information about the current parse location.  );
{-   They may be called when XMLParse or XMLParseBuffer return 0; }
{-   in this case the location is the location of the character at which }
{-   the error was detected. }
{-   They may also be called from any other callback called to report }
{-   some parse event; in this the location is the location of the first }
{=   of the sequence of characters that generated the event. }
function XML_GetCurrentLineNumber(Parser: TXMLParser): Integer; cdecl;
function XMLGetCurrentColumnNumber(Parser: TXMLParser): Integer; cdecl;
function XMLGetCurrentByteIndex(Parser: TXMLParser): Longint; cdecl;

{+// Return the number of bytes in the current event. }
{=   Returns 0 if the event is in an internal entity. }
function XMLGetCurrentByteCount(Parser: TXMLParser): Integer; cdecl;

{+// If XML_CONTEXT_BYTES is defined, returns the input buffer, sets }
{-   the integer pointed to by offset to the offset within this buffer }
{-   of the current parse position, and sets the integer pointed to by size }
{-   to the size of this buffer (the number of input bytes). Otherwise }
{-   returns a null pointer. Also returns a null pointer if a parse isn't active. }

{-   NOTE: The character pointer returned should not be used outside }
{=   the handler that makes the call. }
function XMLGetInputContext(
  Parser: TXMLParser;
  var Offset: Integer;
  var Size: Integer): PChar; cdecl;

{+// For backwards compatibility with previous versions. }
// #define XMLGetErrorLineNumber XMLGetCurrentLineNumber
// #define XMLGetErrorColumnNumber XMLGetCurrentColumnNumber
// #define XMLGetErrorByteIndex XMLGetCurrentByteIndex

{+// Frees memory used by the Parser. }
procedure XML_ParserFree(Parser: TXMLParser); cdecl;

{+// Returns a string describing the error. }
function XML_ErrorString(Code: Integer): PXMLLChar; cdecl;

{+// Return a string containing the version number of this expat }
function XMLExpatVersion: PXMLLChar; cdecl;

type
  TXMLExpatVersion = packed record
    Major: Longint;
    Minor: Longint;
    Micro: Longint;
    end;

{+// Return a TXMLExpatVersion record with version details }
function XMLExpatVersionInfo: TXMLExpatVersion; cdecl;

type
  {+// Added in Expat 1.95.5. }
  TXMLFeature = (
    XML_FEATURE_END,
    XML_FEATURE_UNICODE,
    XML_FEATURE_UNICODE_WCHAR_T,
    XML_FEATURE_DTD,
    XML_FEATURE_CONTEXT_BYTES,
    XML_FEATURE_MIN_SIZE,
    XML_FEATURE_SIZEOF_XML_CHAR,
    XML_FEATURE_SIZEOF_XML_LCHAR);
  {+// Additional features must be added to the end of this enum. }

  PXMLFeatureInfo = ^TXMLFeatureInfo;
  TXMLFeatureInfo = packed record
    Feature: TXMLFeature;
    Name: PXMLLChar;
    Value: Longint
    end;

  PXMLFeatureList = ^TXMLFeatureList;
  TXMLFeatureList = packed array[0.. (MaxInt div SizeOf(TXMLFeatureInfo)) - 1]
    of TXMLFeatureInfo;

{+// Return a list feature macro descriptions }
function XMLGetFeatureList: PXMLFeatureList; cdecl;

{+// Frees the content model passed to the element declaration handler }
procedure XMLFreeContentModel(Parser: TXMLParser; Model: PXMLContent); cdecl;

{+// Exposing the memory handling functions used in Expat }
function XMLMemMalloc(
  Parser: TXMLParser;
  Size: Longword): Pointer; cdecl;
function XMLMemRealloc(
  Parser: TXMLParser;
  Ptr: Pointer;
  Size: Longword): Pointer; cdecl;
procedure XMLMemFree(
  Parser: TXMLParser;
  Ptr: Pointer); cdecl;


implementation

{ XML_GetUserData is defined as a macro in Expat:
    #define XML_GetUserData(Parser) (*(void **)(Parser))
  So we can't call an API function, but we can duplicate what it does  }
type
  PPointer = ^Pointer;

function XMLGetUserData(Parser: TXMLParser): Pointer;
  begin
  Result := PPointer(Parser)^;
  end;

procedure XMLDefaultCurrent;
  external EXPATLIB name 'XML_DefaultCurrent';
function  XML_ErrorString;
  external EXPATLIB name 'XML_ErrorString';
function  XMLExpatVersion;
  external EXPATLIB name 'XML_ExpatVersion';
function  XMLExpatVersionInfo;
  external EXPATLIB name 'XML_ExpatVersionInfo';
function  XMLExternalEntityParserCreate;
  external EXPATLIB name 'XML_ExternalEntityParserCreate';
function  XMLGetBase;
  external EXPATLIB name 'XML_GetBase';
function  XMLGetBuffer;
  external EXPATLIB name 'XML_GetBuffer';
function  XMLGetCurrentByteCount;
  external EXPATLIB name 'XML_GetCurrentByteCount';
function  XMLGetCurrentByteIndex;
  external EXPATLIB name 'XML_GetCurrentByteIndex';
function  XMLGetCurrentColumnNumber;
  external EXPATLIB name 'XML_GetCurrentColumnNumber';
function  XML_GetCurrentLineNumber;
  external EXPATLIB name 'XML_GetCurrentLineNumber';
function  XML_GetErrorCode;
  external EXPATLIB name 'XML_GetErrorCode';
function  XMLGetIdAttributeIndex;
  external EXPATLIB name 'XML_GetIdAttributeIndex';
function  XMLGetInputContext;
  external EXPATLIB name 'XML_GetInputContext';
function  XMLGetSpecifiedAttributeCount;
  external EXPATLIB name 'XML_GetSpecifiedAttributeCount';
function  XML_Parse;
  external EXPATLIB name 'XML_Parse';
function  XMLParseBuffer;
  external EXPATLIB name 'XML_ParseBuffer';
function  XML_ParserCreate;
  external EXPATLIB name 'XML_ParserCreate';
function  XMLParserCreateNS;
  external EXPATLIB name 'XML_ParserCreateNS';
function  XMLParserCreateMM;
  external EXPATLIB name 'XML_ParserCreate_MM';
procedure XML_ParserFree;
  external EXPATLIB name 'XML_ParserFree';
procedure XMLSetAttlistDeclHandler;
  external EXPATLIB name 'XML_SetAttlistDeclHandler';
function  XMLSetBase;
  external EXPATLIB name 'XML_SetBase';
procedure XMLSetCdataSectionHandler;
  external EXPATLIB name 'XML_SetCdataSectionHandler';
procedure XML_SetCharacterDataHandler;
  external EXPATLIB name 'XML_SetCharacterDataHandler';
procedure XMLSetCommentHandler;
  external EXPATLIB name 'XML_SetCommentHandler';
procedure XMLSetDefaultHandler;
  external EXPATLIB name 'XML_SetDefaultHandler';
procedure XMLSetDefaultHandlerExpand;
  external EXPATLIB name 'XML_SetDefaultHandlerExpand';
procedure XMLSetDoctypeDeclHandler;
  external EXPATLIB name 'XML_SetDoctypeDeclHandler';
procedure XMLSetElementDeclHandler;
  external EXPATLIB name 'XML_SetElementDeclHandler';
procedure XML_SetElementHandler;
  external EXPATLIB name 'XML_SetElementHandler';
function  XMLSetEncoding;
  external EXPATLIB name 'XML_SetEncoding';
procedure XMLSetEndCdataSectionHandler;
  external EXPATLIB name 'XML_SetEndCdataSectionHandler';
procedure XMLSetEndDoctypeDeclHandler;
  external EXPATLIB name 'XML_SetEndDoctypeDeclHandler';
procedure XMLSetEndElementHandler;
  external EXPATLIB name 'XML_SetEndElementHandler';
procedure XMLSetEndNamespaceDeclHandler;
  external EXPATLIB name 'XML_SetEndNamespaceDeclHandler';
procedure XMLSetEntityDeclHandler;
  external EXPATLIB name 'XML_SetEntityDeclHandler';
procedure XMLSetExternalEntityRefHandler;
  external EXPATLIB name 'XML_SetExternalEntityRefHandler';
procedure XMLSetExternalEntityRefHandlerArg;
  external EXPATLIB name 'XML_SetExternalEntityRefHandlerArg';
procedure XMLSetNamespaceDeclHandler;
  external EXPATLIB name 'XML_SetNamespaceDeclHandler';
procedure XMLSetNotStandaloneHandler;
  external EXPATLIB name 'XML_SetNotStandaloneHandler';
procedure XMLSetNotationDeclHandler;
  external EXPATLIB name 'XML_SetNotationDeclHandler';
function XMLSetParamEntityParsing;
  external EXPATLIB name 'XML_SetParamEntityParsing';
procedure XMLSetProcessingInstructionHandler;
  external EXPATLIB name 'XML_SetProcessingInstructionHandler';
procedure XMLSetReturnNSTriplet;
  external EXPATLIB name 'XML_SetReturnNSTriplet';
procedure XMLSetStartCdataSectionHandler;
  external EXPATLIB name 'XML_SetStartCdataSectionHandler';
procedure XMLSetStartDoctypeDeclHandler;
  external EXPATLIB name 'XML_SetStartDoctypeDeclHandler';
procedure XMLSetStartElementHandler;
  external EXPATLIB name 'XML_SetStartElementHandler';
procedure XMLSetStartNamespaceDeclHandler;
  external EXPATLIB name 'XML_SetStartNamespaceDeclHandler';
procedure XMLSetUnknownEncodingHandler;
  external EXPATLIB name 'XML_SetUnknownEncodingHandler';
procedure XMLSetUnparsedEntityDeclHandler;
  external EXPATLIB name 'XML_SetUnparsedEntityDeclHandler';
procedure XML_SetUserData;
  external EXPATLIB name 'XML_SetUserData';
procedure XMLSetXmlDeclHandler;
  external EXPATLIB name 'XML_SetXmlDeclHandler';
procedure XMLUseParserAsHandlerArg;
  external EXPATLIB name 'XML_UseParserAsHandlerArg';
// added with version 1.95.3
(*function XMLParserReset;
  external EXPATLIB name 'XML_ParserReset';*){milano}
procedure XMLSetSkippedEntityHandler;
  external EXPATLIB name 'XML_SetSkippedEntityHandler';
// added with version 1.95.5
function  XMLGetFeatureList;
  external EXPATLIB name 'XML_GetFeatureList';
procedure XMLUseForeignDTD;
  external EXPATLIB name 'XML_UseForeignDTD';
// added with version 1.95.6
procedure  XMLFreeContentModel;
  external EXPATLIB name 'XML_FreeContentModel';
function  XMLMemMalloc;
  external EXPATLIB name 'XML_MemMalloc';
function  XMLMemRealloc;
  external EXPATLIB name 'XML_MemRealloc';
procedure  XMLMemFree;
  external EXPATLIB name 'XML_MemFree';


end.
