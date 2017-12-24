{
    This file is part of the Web Service Toolkit
    Copyright (c) 2015 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$INCLUDE wst_global.inc}
unit generatorj;

interface

uses
  Classes, SysUtils,
  PasTree,
  pascal_parser_intf, source_utils, wst_types, generatorbase;

type

  { TInftGenerator }

  TInftGenerator = class(TBaseGenerator)
  private
    FStream : ISourceStream;
  private
    function GenerateIntfName(AIntf : TPasElement):string;
    function GenerateTypeText(AType : TPasType) : string;

    procedure GenerateIntfProcParamsTypes(AProc : TPasProcedure);
    procedure GenerateIntf(AIntf : TPasClassType);
    procedure GenerateIntfProxy(AIntf : TPasClassType);
    procedure GenerateClass(ASymbol : TPasClassType);
    procedure GenerateEnum(ASymbol : TPasEnumType);
    function GetDestUnitName():string;

    procedure GenerateEnums();
    procedure PrepareModule();
    procedure InternalExecute();
  public
    procedure Execute();override;
  end;

implementation
uses
  contnrs,
  parserutils, logger_intf;

{ TInftGenerator }

function TInftGenerator.GenerateIntfName(AIntf : TPasElement) : string;
begin
  Result := AIntf.Name;
end;

function TInftGenerator.GenerateTypeText(AType : TPasType) : string;
var
  t : TPasType;
begin
  t := GetUltimeType(AType);
  if not t.InheritsFrom(TPasArrayType) then begin
    Result := t.Name;
  end else begin
    t := GetUltimeType(TPasArrayType(t).ElType);
    Result := Format('java.util.List<%s>',[t.Name]);
  end;
end;

procedure TInftGenerator.GenerateIntfProcParamsTypes(AProc : TPasProcedure);
var
  locName : string;

  procedure WriteRequestConstructor();
  var
    prmCnt,k : Integer;
    prm : TPasArgument;
    prms : TList2;
  begin
    Indent();
    Write('public %s(',[locName]);
    prms := AProc.ProcType.Args;
    prmCnt := prms.Count;
    if ( prmCnt > 0 ) then begin
      IncIndent();
      for k := 0 to Pred(prmCnt) do begin
        prm := TPasArgument(prms[k]);
        if (k > 0 ) then
          Write(', ');
        NewLine();
        Indent();
        Write('%s %s',[GenerateTypeText(prm.ArgType),prm.Name]);
      end;
      DecIndent();
      NewLine();
      Indent();
    end;
    WriteLn('){');
    IncIndent();
      if (prmCnt = 0) then begin
        Indent();
        WriteLn('super("%s", null);',[AProc.Name]);
      end else begin
        Indent();
        Write('super("%s", new Object[] {',[AProc.Name]);
        for k := 0 to Pred(prmCnt) do begin
          prm := TPasArgument(prms[k]);
          if (k > 0 ) then
            Write(', ');
          Write(prm.Name);
        end;
        WriteLn('});');
      end;
    DecIndent();
    Indent();
    WriteLn('}');
  end;

  procedure WriteResponseFields();
  var
    pt : TPasType;
  begin
    if AProc.InheritsFrom(TPasFunction) then begin
      Indent();
      pt := GetUltimeType(TPasFunctionType(AProc.ProcType).ResultEl.ResultType);
      Write('public %s result',[GenerateTypeText(pt)]);
      if pt.InheritsFrom(TPasArrayType) then
        Write(' = new java.util.ArrayList<>()');
      WriteLn(';');
    end;
  end;

var
  locUnitName, s : string;
begin
  locUnitName := GetDestUnitName();
  locName := Format('%sInParamsEnv_Type',[AProc.Name]);
  s := locUnitName + PathDelim + locName+ '.java';
  FStream := SrcMngr.CreateItem(s);
  SetCurrentStream(FStream);
  WriteLn('package %s;',[locUnitName]);
  NewLine();
  WriteLn('public class %s extends wst.BaseRemoteRequest {',[locName]);
  IncIndent();
    WriteRequestConstructor();
  DecIndent();
  WriteLn('}');

  locName := Format('%sOutParamsEnv_Type',[AProc.Name]);
  s := locUnitName + PathDelim + locName+ '.java';
  FStream := SrcMngr.CreateItem(s);
  SetCurrentStream(FStream);
  WriteLn('package %s;',[locUnitName]);
  NewLine();
  WriteLn('public class %s extends wst.BaseRemoteResponse {',[locName]);
  IncIndent();
    WriteResponseFields();
  DecIndent();
  WriteLn('}');
end;

procedure TInftGenerator.GenerateIntf(AIntf : TPasClassType);
var
  locName : string;

  procedure WriteMethod(AMthd : TPasProcedure);
  var
    prmCnt,k : Integer;
    prm : TPasArgument;
    prms : TList2;
    pt : TPasType;
  begin
    Indent();
    Write('public ');
    prms := AMthd.ProcType.Args;
    prmCnt := prms.Count;
    if AMthd.InheritsFrom(TPasFunction) then begin
      pt := TPasFunctionType(AMthd.ProcType).ResultEl.ResultType;
      Write(GenerateTypeText(pt) + ' ');
    end else begin
      Write('void ');
    end;
    Write('%s(',[AMthd.Name]);

    if ( prmCnt > 0 ) then begin
      IncIndent();
      for k := 0 to Pred(prmCnt) do begin
        prm := TPasArgument(prms[k]);
        if (k > 0 ) then
          Write(', ');
        NewLine();
        Indent();
        Write('%s %s',[GenerateTypeText(prm.ArgType),prm.Name]);
      end;
      DecIndent();
      NewLine();
      Indent();
    end;

    WriteLn(') throws java.io.IOException, wst.RemoteException;');
  end;

  procedure WriteMethods();
  var
    k, kc : Integer;
    mbrs : TList2;
    elt : TPasElement;
  begin
    IncIndent();
      mbrs := AIntf.Members;
      kc := 0;
      for k := 0 to Pred(mbrs.Count) do begin
        elt := TPasElement(mbrs[k]);
        if elt.InheritsFrom(TPasProcedure) then begin
          if (kc > 0) then
            NewLine();
          kc := kc+1;
          WriteMethod(TPasProcedure(elt));
        end;
      end;
    DecIndent();
  end;

  procedure WriteMethodEnvs();
  var
    k : Integer;
    mbrs : TList2;
    elt : TPasElement;
  begin
    mbrs := AIntf.Members;
    for k := 0 to Pred(mbrs.Count) do begin
      elt := TPasElement(mbrs[k]);
      if elt.InheritsFrom(TPasProcedure) then
        GenerateIntfProcParamsTypes(TPasProcedure(elt));
    end;
  end;

var
  locUnitName, s : string;
begin
  locUnitName := GetDestUnitName();
  s := locUnitName + PathDelim + SymbolTable.GetExternalName(AIntf)+ '.java';
  FStream := SrcMngr.CreateItem(s);
  SetCurrentStream(FStream);
  WriteLn('package %s;',[locUnitName]);
  NewLine();
  locName := GenerateIntfName(AIntf);
  WriteLn('public interface %s {',[locName]);
  IncIndent();
    WriteMethods();
  DecIndent();
  WriteLn('}');
  WriteMethodEnvs();
end;

procedure TInftGenerator.GenerateIntfProxy(AIntf : TPasClassType);
var
  locProxyName : string;

  procedure WriteMethod(AMthd : TPasProcedure);
  var
    prmCnt,k : Integer;
    prm : TPasArgument;
    prms : TList2;
    s : string;
    pt : TPasType;
  begin
    Indent();
    Write('public ');
    prms := AMthd.ProcType.Args;
    prmCnt := prms.Count;
    if AMthd.InheritsFrom(TPasFunction) then begin
      pt := TPasFunctionType(AMthd.ProcType).ResultEl.ResultType;
      Write(GenerateTypeText(pt) + ' ');
    end else begin
      Write('void ');
    end;
    Write('%s(',[AMthd.Name]);

    if ( prmCnt > 0 ) then begin
      IncIndent();
      for k := 0 to Pred(prmCnt) do begin
        prm := TPasArgument(prms[k]);
        if (k > 0 ) then
          Write(', ');
        NewLine();
        Indent();
        Write('%s %s',[GenerateTypeText(prm.ArgType),prm.Name]);
      end;
      DecIndent();
      NewLine();
      Indent();
    end;
    WriteLn(') throws java.io.IOException, wst.RemoteException{');
    IncIndent();
      s := Format('%sInParamsEnv_Type',[AMthd.Name]);
      Indent();
      Write('%s request = new %s(',[s,s]);
      for k := 0 to Pred(prmCnt) do begin
        prm := TPasArgument(prms[k]);
        if (k > 0 ) then
          Write(', ');
        Write(prm.Name);
      end;
      WriteLn(');');
      s := Format('%sOutParamsEnv_Type',[AMthd.Name]);
      Indent();
      WriteLn('%s response = (%s)MakeCall(request, %s.class);',[s,s,s]);
      Indent();
      WriteLn('if (response.error != null){ ');
      IncIndent();
        Indent();
        WriteLn('throw new wst.RemoteException(response.error.message,response.error.code,response.error.name);');
      DecIndent();
      Indent();
      WriteLn('}');
      if AMthd.InheritsFrom(TPasFunction) then begin
        Indent();
        WriteLn('return response.result;');
      end;
    DecIndent();
    Indent();
    WriteLn('};');
  end;

  procedure WriteMethods();
  var
    k : Integer;
    mbrs : TList2;
    elt : TPasElement;
  begin
    IncIndent();
      mbrs := AIntf.Members;
      for k := 0 to Pred(mbrs.Count) do begin
        elt := TPasElement(mbrs[k]);
        if elt.InheritsFrom(TPasProcedure) then begin
          NewLine();
          WriteMethod(TPasProcedure(elt));
        end;
      end;
    DecIndent();
  end;

var
  locUnitName, s : string;
begin
  locUnitName := GetDestUnitName();
  locProxyName := ExtractserviceName(AIntf);
  locProxyName := Format('%sProxy',[locProxyName]);
  s := locUnitName + PathDelim + locProxyName + '.java';
  FStream := SrcMngr.CreateItem(s);
  SetCurrentStream(FStream);
  WriteLn('package %s;',[locUnitName]);
  NewLine();
  WriteLn('public class %s extends wst.BaseProxy implements %s {',[locProxyName,AIntf.Name]);
  IncIndent();
    Indent();
    WriteLn('public %s(String serviceAddress){',[locProxyName]);
    IncIndent();
      Indent();
      WriteLn('super(serviceAddress);');
    DecIndent();
    Indent();
    WriteLn('}');
  DecIndent();
  WriteMethods();
  WriteLn('}');
end;

procedure TInftGenerator.GenerateClass(ASymbol : TPasClassType);

  procedure WriteDec();
  var
    decBuffer, s : string;
    elt : TPasElement;
    ultimAnc, trueAncestor : TPasType;
  begin
    s := '';
    if Assigned(ASymbol.AncestorType) then begin
      trueAncestor := ASymbol.AncestorType;
      if trueAncestor.InheritsFrom(TPasUnresolvedTypeRef) then begin
        elt := SymbolTable.FindElement(SymbolTable.GetExternalName(trueAncestor));
        if (elt = nil) or (not elt.InheritsFrom(TPasType)) then
          trueAncestor := nil
        else
          trueAncestor := TPasType(elt);
      end;
      if (trueAncestor <> nil) then begin
        trueAncestor := GetUltimeType(trueAncestor);
        if trueAncestor.InheritsFrom(TPasNativeSimpleType) and
           Assigned(TPasNativeSimpleType(trueAncestor).ExtendableType)
        then begin
          trueAncestor := TPasNativeSimpleType(trueAncestor).ExtendableType;
        end;
        if not(trueAncestor.InheritsFrom(TPasNativeClassType)) or (trueAncestor.Name <> 'Object') then
          s := Format('%s',[trueAncestor.Name]);
      end;
    end;
    if IsStrEmpty(s) then
      decBuffer := ''
    else
      decBuffer := Format(' extends %s',[s]);
    Indent();
    WriteLn('public class %s%s {',[ASymbol.Name,decBuffer]);
  end;

  procedure WriteProperty(AProp : TPasProperty; AActualPropType : TPasType);
  var
    locLine : string;
    locType : TPasType;
    locIsArray : Boolean;
  begin
    locType := GetUltimeType(AActualPropType);
    locIsArray := SymbolTable.IsOfType(locType,TPasArrayType);
    if not locIsArray then begin
      locLine := Format('public %s %s',[GenerateTypeText(locType),SymbolTable.GetExternalName(AProp)]);
      if SymbolTable.IsOfType(locType,TPasClassType) then
        locLine := Format('%s = new %s()',[locLine,GenerateTypeText(locType)]);
      locLine := locLine+';';
    end else begin
      locType := GetUltimeType(TPasArrayType(locType).ElType);
      locLine := Format('public java.util.List<%s> %s = new java.util.ArrayList<>();',[GenerateTypeText(locType),SymbolTable.GetExternalName(AProp)]);
    end;
    Indent(); WriteLn(locLine);
  end;

  procedure WriteProperties();
  var
    k : Integer;
    p : TPasProperty;
    elt : TPasElement;
  begin
    Indent();
    IncIndent();
    for k := 0 to Pred(ASymbol.Members.Count) do begin
      elt := TPasElement(ASymbol.Members[k]);
      if elt.InheritsFrom(TPasProperty) then begin
        p := TPasProperty(elt);
        WriteProperty(p,FindActualType(p.VarType,SymbolTable));
      end;
    end;
    DecIndent();
  end;

begin
  try
    NewLine();
    WriteDec();
    WriteProperties();
    WriteLn('}');
  except
    on e : Exception do begin
      GetLogger.Log(mtError,'TInftGenerator.GenerateClass()=',[ASymbol.Name, ' ;; ', e.Message]);
      raise;
    end;
  end;
end;

procedure TInftGenerator.GenerateEnum(ASymbol : TPasEnumType);
var
  i : Integer;
  itm : TPasEnumValue;
  s : string;
begin
  NewLine();
  WriteLn('public enum %s {',[SymbolTable.GetExternalName(ASymbol)]);
  IncIndent();
    for i := 0 to Pred(ASymbol.Values.Count) do begin
      itm := TPasEnumValue(ASymbol.Values[i]);
      Indent();
      s := SymbolTable.GetExternalName(itm);
      if ( i > 0 ) then
        WriteLn(',%s',[s])
      else
        WriteLn('%s',[s]);
    end;
  DecIndent();
  WriteLn('}');
end;

function TInftGenerator.GetDestUnitName(): string;
begin
  Result := SymbolTable.CurrentModule.Name;
end;

procedure TInftGenerator.GenerateEnums();
var
  typeList : TList2;
  locUnitName, s : string;
  i, c : Integer;
  elt : TPasElement;
begin
  typeList := SymbolTable.CurrentModule.InterfaceSection.Declarations;
  c := typeList.Count;
  for i := 0 to c-1 do begin
    elt := TPasElement(typeList[i]);
    if elt.InheritsFrom(TPasEnumType) then begin
      locUnitName := GetDestUnitName();
      s := locUnitName + PathDelim + SymbolTable.GetExternalName(elt)+ '.java';
      FStream := SrcMngr.CreateItem(s);
      SetCurrentStream(FStream);
      WriteLn('package %s;',[locUnitName]);
      GenerateEnum(TPasEnumType(elt));
    end;
  end;
end;

procedure TInftGenerator.PrepareModule();
begin

end;

procedure TInftGenerator.InternalExecute();
var
  i, c, j, k : Integer;
  clssTyp : TPasClassType;
  gnrClssLst : TObjectList;
  objLst : TObjectList;
  typeList : TList2;
  elt : TPasElement;
  classAncestor : TPasElement;
  s, locUnitName : string;
begin
  GenerateEnums();

  objLst := nil;
  gnrClssLst := TObjectList.Create(False);
  try
    locUnitName := GetDestUnitName();
    typeList := SymbolTable.CurrentModule.InterfaceSection.Declarations;
    c := Pred(typeList.Count);

    objLst := TObjectList.Create();
    objLst.OwnsObjects := False;
    for i := 0 to c do begin
      elt := TPasElement(typeList[i]);
      if elt.InheritsFrom(TPasClassType) and ( TPasClassType(elt).ObjKind = okClass ) then begin
        clssTyp := TPasClassType(elt);
        if ( gnrClssLst.IndexOf(clssTyp) = -1 ) then begin
          objLst.Clear();
          while Assigned(clssTyp) and ( objLst.IndexOf(clssTyp) = -1 ) do begin
            objLst.Add(clssTyp);
            classAncestor := clssTyp.AncestorType;
            if Assigned(classAncestor) and classAncestor.InheritsFrom(TPasUnresolvedTypeRef) then begin
              classAncestor := SymbolTable.FindElement(SymbolTable.GetExternalName(classAncestor));
            end;
            if Assigned(classAncestor) and classAncestor.InheritsFrom(TPasClassType) then begin
              clssTyp := classAncestor as TPasClassType;
            end else begin
              clssTyp := nil;
            end;
          end;

          k := Pred(objLst.Count);
          for j := 0 to k do begin
            clssTyp := objLst[k-j] as TPasClassType;
            if ( gnrClssLst.IndexOf(clssTyp) = -1 ) then begin
              if ( SymbolTable.CurrentModule.InterfaceSection.Declarations.IndexOf(clssTyp) <> -1 ) then begin
                s := locUnitName + PathDelim + SymbolTable.GetExternalName(clssTyp)+ '.java';
                FStream := SrcMngr.CreateItem(s);
                SetCurrentStream(FStream);
                WriteLn('package %s;',[locUnitName]);
                GenerateClass(clssTyp);
                gnrClssLst.Add(clssTyp);
              end;
            end;
          end;
        end;
      end;
    end;

    for i := 0 to c do begin
      elt := TPasElement(typeList[i]);
      if elt.InheritsFrom(TPasClassType) and ( TPasClassType(elt).ObjKind = okInterface ) then begin
        GenerateIntf(TPasClassType(elt));
        GenerateIntfProxy(TPasClassType(elt));
      end;
    end;
  finally
    FreeAndNil(objLst);
    FreeAndNil(gnrClssLst);
  end;
end;

procedure TInftGenerator.Execute();
var
  oldCurrent, mdl : TPasModule;
  i : Integer;
  mdlList : TList2;
  oldCS : Boolean;
  oldNamesKinds : TElementNameKinds;
begin
  oldCS := SymbolTable.CaseSensitive;
  oldNamesKinds := SymbolTable.DefaultSearchNameKinds;
  oldCurrent := SymbolTable.CurrentModule;
  try
    SymbolTable.CaseSensitive := True;
    SymbolTable.DefaultSearchNameKinds := [elkDeclaredName];
    mdlList := SymbolTable.Package.Modules;
    for i := 0 to Pred(mdlList.Count) do begin
      mdl := TPasModule(mdlList[i]);
      if not mdl.InheritsFrom(TPasNativeModule) then begin
        SymbolTable.SetCurrentModule(mdl);
        PrepareModule();
        InternalExecute();
      end;
    end;
  finally
    SymbolTable.SetCurrentModule(oldCurrent);
    SymbolTable.CaseSensitive := oldCS;
    SymbolTable.DefaultSearchNameKinds := oldNamesKinds;
  end;
end;

end.

