unit OpenWirePinEditors;

{$MODE DELPHI}{$H+}

interface

uses
  LCLIntf, LMessages, LResources, PropEdits,
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, ComCtrls, StdCtrls, Buttons, Contnrs, OWPins, ExtCtrls,
  OWDesignTypes;

type
  IADesigner = TIDesigner;
  TAGetPropProc = TGetPropEditProc;
  TOWPropNameString = ShortString;
  TOWPropertyDesigner = TPropertyEditorHook;



type

  TOWEItemEntry = class
  protected
    FPin: TOWBasicPin;
    FConnectedToPin: TOWBasicPin;
    FConnectedAfterPin: TOWBasicPin;
    FItem: TListItem;

  protected
    procedure SetConnectedToPin(Value: TOWBasicPin);
    procedure SetConnectedAfterPin(Value: TOWBasicPin);

  public
    procedure Populate();
  public
    IsDispatcher: boolean;
    StateIndex: integer;
    PinConnectIdent: string;
    PinConnectName: string;
  public
    constructor Create(AItem: TListItem; APin: TOWBasicPin);
  public
    property ConnectedToPin: TOWBasicPin read FConnectedToPin write SetConnectedToPin;
    property ConnectedAfterPin: TOWBasicPin read FConnectedAfterPin write SetConnectedAfterPin;

  end;


type
  TOWPinEditorForm = class(TForm)
    ImageList1: TImageList;
    SourcesImageList: TImageList;
    Panel1: TPanel;
    TestButton: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    ListView: TListView;
    Panel4: TPanel;
    FormsComboBox: TComboBox;
    Label1: TLabel;
    Panel5: TPanel;
    LinksCountLabel: TLabel;
    StaticLabel: TLabel;
    SinksImageList: TImageList;
    AboutPanel: TPanel;
    Image1: TImage;
    AllPinsCheckBox: TCheckBox;
    LinkAllButton: TBitBtn;
    UnlinkAllButton: TBitBtn;
    RestoreButton: TBitBtn;
    OkButton: TBitBtn;
    CancelButton: TBitBtn;
    AfterPinButton: TBitBtn;
    procedure LinkAllButtonClick(Sender: TObject);
    procedure UnlinkAllButtonClick(Sender: TObject);
    procedure RestoreButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure ListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure FormCreate(Sender: TObject);
    procedure ListViewCompare(Sender: TObject; Item1, Item2: TListItem; Data: integer; var Compare: integer);
    procedure FormsComboBoxChange(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure ListViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure ListViewKeyPress(Sender: TObject; var Key: char);
    procedure ListViewDeletion(Sender: TObject; Item: TListItem);
    procedure AfterPinButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateLinksCount();
    procedure PopulateAll();
    procedure PopulateAllEntries();
    procedure PopulateForm(ARootComponent: TComponent);

    procedure PopulateSingleForm(CurrentRoot: TComponent; ARootComponent: TComponent; OnlyConnected: boolean;
      FullPath: boolean; FilterPins: boolean);
    procedure PopulateSingleFormEntries(CurrentRoot: TComponent; ARootComponent: TComponent; FullPath: boolean; FilterPins: boolean);

    procedure PopulateSingleFormInt(CurrentRoot: TComponent; ARootComponent: TComponent; OnlyConnected: boolean;
      FullPath: boolean; FilterPins: boolean);

    procedure FillFormsInfo();
    function RootFromName(RootName: string): TComponent;

    function EntryFromPin(Pin: TOWBasicPin): TOWEPinEntry;
    function EntryFromDispatcher(Dispatcher: TOWStateDispatcher): TOWEPinEntry;
    procedure ChangeState(Item: TListItem);

  private
    Designer: TOWPropertyDesigner;
    FSourcePin: TOWSourcePin;
    FSinkPin: TOWSinkPin;
    FMultiSinkPin: TOWMultiSinkPin;
    FPin: TOWPin;
    Root: TComponent;
    PinsList: TOWEPinsList;
    ListUpdating: boolean;

  private
    procedure OWMUpdate(var Message: TMessage); message OWM_UPDATE;
    procedure OWMUpdateInspector(var Message: TMessage); message OWMSG_UPDATE_INSPECTOR;

  public
    function ExecuteForSource(ADesigner: TOWPropertyDesigner; ASourcePin: TOWSourcePin): integer;
    function ExecuteForSink(ADesigner: TOWPropertyDesigner; ASinkPin: TOWSinkPin): integer;
    function ExecuteForEventSink(ADesigner: TOWPropertyDesigner; AEventSinkPin: TOWMultiSinkPin): integer;

  private
    ColumnToSort: integer;
    Direction: boolean;
    AllSelected: boolean;

  public
    function GetListItems(): TListItems;
    procedure ClearData();

  public
    property Items: TListItems read GetListItems;

  end;

type
  TOWBasicPropertyEditor = class(TPropertyEditor)
  protected
    function GetIntDesigner(): TOWPropertyDesigner;

  end;

  TOWBasicPinPropertyEditor = class(TOWBasicPropertyEditor)
  protected
    function GetPin(): TOWPin;

  public
    function GetValue(): string; override;
    procedure SetValue(const Value: string); override;

  end;

  TOWPinListPropertyEditor = class(TOWBasicPropertyEditor)
  public
    function GetAttributes(): TPropertyAttributes; override;
    function GetValue(): string; override;
    procedure GetProperties(Proc: TAGetPropProc); override;

  public
    procedure CheckRefresh();

  end;

  TOWPinListOwnerPropertyEditor = class(TOWPinListPropertyEditor)
  public
    function GetAttributes(): TPropertyAttributes; override;
    function GetValue(): string; override;
    procedure SetValue(const Value: string); override;

  end;

  TOWSourcePinPropertyEditor = class(TOWBasicPinPropertyEditor)
  public
    function GetAttributes(): TPropertyAttributes; override;
    procedure Edit(); override;

  end;

  TOWStatePinPropertyEditor = class(TOWBasicPinPropertyEditor)
  public
    function GetAttributes(): TPropertyAttributes; override;
    procedure Edit(); override;

  end;

  TOWBasicPinListPropertyEditor = class(TPropertyEditor)
  protected
    FName: string;
    FPin: TOWBasicPin;
    FPinListEditor: TOWPinListPropertyEditor;

  protected
    function GetIntDesigner(): TOWPropertyDesigner;

  public
    function GetName(): TOWPropNameString; override;

    procedure SetValue(const Value: string); override;
    function GetValue(): string; override;

  public
    function GetPin(): TOWBasicPin;

  public
    constructor CreateEx(ADesigner: TOWPropertyDesigner; APin: TOWBasicPin; AName: string; PinListEditor: TOWPinListPropertyEditor);
    destructor Destroy; override;

  end;

  TOWSourcePinListPropertyEditor = class(TOWBasicPinListPropertyEditor)
  public
    function GetAttributes(): TPropertyAttributes; override;
    procedure Edit(); override;
  end;

  TOWSinkPinPropertyEditor = class(TOWBasicPinPropertyEditor)
  public
    function GetAttributes(): TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure Edit(); override;

  end;

  TOWEventSinkPinPropertyEditor = class(TOWBasicPinPropertyEditor)
  public
    function GetAttributes(): TPropertyAttributes; override;
    procedure Edit(); override;

  end;

  TOWSinkPinListPropertyEditor = class(TOWBasicPinListPropertyEditor)
  public
    function GetAttributes(): TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure Edit(); override;

  end;

  TOWEventSinkPinListPropertyEditor = class(TOWBasicPinListPropertyEditor)
  public
    function GetAttributes(): TPropertyAttributes; override;
    procedure Edit(); override;
  end;

  TOWStatePinListPropertyEditor = class(TOWBasicPinListPropertyEditor)
  public
    function GetAttributes(): TPropertyAttributes; override;
    procedure Edit(); override;
  end;


implementation

{$R *.lfm}

uses Math, OWAboutFormUnit, OWStateEditors, OWAfterPinSelectFormUnit;

type
  TADesignerSelectionList = TPersistentSelectionList;


const
  SEPARATOR = '.';


var
  GOWPinEditorForm: TOWPinEditorForm;

type
  TOWExposedPin = class(TOWBasicPin);
  TOWExposedSinkPin = class(TOWSinkPin);
  TOWExposedSourcePin = class(TOWSourcePin);
  TOWExposedPinList = class(TOWPinList);

procedure GetSinkPinValueList(SinkPin: TOWSinkPin; List: TStrings);
begin
  if (SinkPin = nil) then
    Exit;

  OWGetPinsValueList(List, SinkPin, '.', []);
end;

procedure GetSinkPinValues(SinkPin: TOWSinkPin; Proc: TGetStrProc);
var
  Values: TStringList;
  I: integer;

begin
  Values := TStringList.Create;
  try
    GetSinkPinValueList(SinkPin, Values);
    Proc(GOWDISCONNECTED);
    for I := 0 to Values.Count - 1 do
      Proc(Values.Strings[i]);

    for I := 0 to OWGetDispatcherCount() - 1 do
    begin
      if (SinkPin.CanConnectToState(OWGetDispatcher(I))) then
        Proc(OWGetDispatcher(I).Name);

    end;

  except
  end;
  Values.Free;

end;

function OWGetPinValue(APin: TOWBasicPin; ADesigner: TOWPropertyDesigner): string;
begin
  try
    if (APin = nil) then
    begin
      Result := 'Refreshing ...';
      OWResetObjectInspector(ADesigner);
    end

    else
      Result := TOWExposedPin(APin).GetEditorString();

  except
    Result := '(Error)';

  end;

end;

function OWSetPinValue(Root: TComponent; APin: TOWBasicPin; const Value: string): boolean;
begin
  Result := False;
  if (APin = nil) then
    Exit;

  Result := TOWExposedPin(APin).SetEditorString(Root, Value);
end;

function LinkToDisplayString(ARootComponent: TComponent; LinkStr: string; FullPath: boolean; OwnRoot: boolean; var PinStr: string): string;
var
  StrFull: string;
  Str: string;
  iPos: integer;

begin
  if (not OwnRoot) then
  begin
    iPos := Pos('.', LinkStr);
    StrFull := LinkStr;
    Delete(StrFull, 1, iPos);
  end

  else
    StrFull := LinkStr;

  iPos := Pos('.', StrFull);
  Str := StrFull;
  Delete(Str, 1, iPos);
  PinStr := Str;
  Str := StrFull;
  iPos := Pos('.', Str);
  Delete(Str, iPos, 10000);
  if (FullPath) then
    if (ARootComponent <> nil) then
      Str := ARootComponent.Name + '.' + Str;

  Result := Str;
end;

function OWGetPinValueEx(APin: TOWBasicPin; ADesigner: TOWPropertyDesigner; ACurrentRoot: TComponent): string;
var
  OwnRoot1: boolean;
  FullSinkPath: boolean;
  Str: string;
  PinStr: string;

begin
  if (not APin.IsConnected()) then
  begin
    Result := '';
    Exit;
  end;

  Result := OWGetPinValue(APin, ADesigner);
  if (TOWExposedPin(APin).GetConnectedPinCount() = 1) then
  begin
    OwnRoot1 := (OWGetMainOwnerComponent(TOWExposedPin(APin).GetConnectedPin(0).Owner) = OWGetMainOwnerComponent(APin.Owner));
    FullSinkPath := (OWGetMainOwnerComponent(TOWExposedPin(APin).GetConnectedPin(0).Owner) <> ACurrentRoot);

    Str := LinkToDisplayString(OWGetMainOwnerComponent(TOWExposedPin(APin).GetConnectedPin(0).Owner), Result,
      FullSinkPath, OwnRoot1, PinStr);
    Result := Str + '.' + PinStr;
  end;

end;

function SinkPinEdit(Designer: TOWPropertyDesigner; SinkPin: TOWSinkPin): boolean;
var
  I: integer;
  OtherPin: TOWBasicPin;
  AfterPin: TOWBasicPin;
  Dispatcher: TOWStateDispatcher;
  SourceRoot: TComponent;
  SourceChanged: boolean;
  PinIdent: string;

begin
  Result := False;
  if (SinkPin = nil) then
    Exit;

  try
    if (GOWPinEditorForm.ExecuteForSink(Designer, SinkPin) = mrOk) then
    begin
      for I := 0 to GOWPinEditorForm.Items.Count - 1 do
      begin
        Dispatcher := nil;

        SourceChanged := False;
        //        if( GOWPinEditorForm.Items [ I ].SubItems.Objects[ 0 ] is TOWPin ) then
        OtherPin := TOWEItemEntry(GOWPinEditorForm.Items[I].Data).ConnectedToPin;
        // TOWPin( GOWPinEditorForm.Items[ I ].SubItems.Objects[ 0 ] );

        //        else if( GOWPinEditorForm.Items [ I ].SubItems.Objects[ 0 ] is TOWStateDispatcher ) then
        //          Dispatcher := TOWStateDispatcher( GOWPinEditorForm.Items [ I ].SubItems.Objects[ 0 ] );

        //        PinIdent := GOWPinEditorForm.Items[ I ].SubItems.Strings[ 4 ];
        PinIdent := TOWEItemEntry(GOWPinEditorForm.Items[I].Data).PinConnectIdent;
        if (GOWPinEditorForm.Items[I].StateIndex and 1 > 0) then
        begin
          if (Dispatcher <> nil) then
          begin
            if (not Dispatcher.IsConnectedTo(SinkPin)) then
              SourceChanged := True;

          end

          else if (OtherPin = nil) then
          begin
            if (not TOWExposedSinkPin(SinkPin).IsConnectedToPinName(PinIdent)) then
              SourceChanged := True;

          end

          else
          begin
            if (SinkPin.SourcePin <> OtherPin) then
              SourceChanged := True;

          end;
        end

        else
        begin
          if (Dispatcher <> nil) then
          begin
            if (Dispatcher.IsConnectedTo(SinkPin)) then
              SourceChanged := True;

          end

          else if (OtherPin = nil) then
            if (TOWExposedSinkPin(SinkPin).IsConnectedToPinName(PinIdent)) then
              SourceChanged := True

            else
            begin
              if (SinkPin.SourcePin = OtherPin) then
                SourceChanged := True;

            end;
        end;

        if (SourceChanged) then
        begin
          { TODO : Modify the forms connected trough the Dispatcher }


          Designer.Modified(SinkPin);

          if (OtherPin <> nil) then
          begin
            if (OtherPin is TOWPin) then
            begin
              SourceRoot := OWGetMainDesignOwner(TOWPin(OtherPin).Owner);
              if (SourceRoot is TCustomForm) then
                if (Assigned(TCustomForm(SourceRoot).Designer)) then
                  TCustomForm(SourceRoot).Designer.Modified();

            end;

          end;

          Break;
        end;

      end;

      SinkPin.Disconnect();
      for I := 0 to GOWPinEditorForm.Items.Count - 1 do
      begin
        Dispatcher := nil;

        OtherPin := TOWEItemEntry(GOWPinEditorForm.Items[I].Data).ConnectedToPin;
        AfterPin := TOWEItemEntry(GOWPinEditorForm.Items[I].Data).ConnectedAfterPin;

        if (GOWPinEditorForm.Items[I].StateIndex and 1 > 0) then
        begin
          if (Dispatcher <> nil) then
            SinkPin.ConnectToStateAfter(Dispatcher, AfterPin)

          else
          begin
            if (OtherPin is TOWStatePin) then
            begin
              if (TOWExposedPin(SinkPin).FDispatcher <> nil) then
                OtherPin.ConnectToStateAfter(TOWExposedPin(SinkPin).FDispatcher, AfterPin)

              else if (TOWEItemEntry(GOWPinEditorForm.Items[I].Data).IsDispatcher) then
                SinkPin.ConnectByStateAfter(OtherPin, AfterPin)

              else
                SinkPin.ConnectAfter(OtherPin, AfterPin);

            end

            else
              SinkPin.ConnectAfter(OtherPin, AfterPin);

          end;

        end;

        if (OtherPin <> nil) then
          TOWExposedPin(OtherPin).SetInEditor(False);

      end;

      Result := True;
    end;

  finally
    GOWPinEditorForm.ClearData();
  end;

end;

function EventSinkPinEdit(Designer: TOWPropertyDesigner; EventSinkPin: TOWMultiSinkPin): boolean;
var
  I: integer;
  OtherPin: TOWBasicPin;
  AfterPin: TOWBasicPin;
  Dispatcher: TOWStateDispatcher;
  SourceRoot: TComponent;
  SourceChanged: boolean;
  PinIdent: string;
  AEntry: TOWEItemEntry;

begin
  Result := False;
  if (EventSinkPin = nil) then
    Exit;

  try
    if (GOWPinEditorForm.ExecuteForEventSink(Designer, EventSinkPin) = mrOk) then
    begin
      for I := 0 to GOWPinEditorForm.Items.Count - 1 do
      begin
        Dispatcher := nil;

        SourceChanged := False;
        AEntry := TOWEItemEntry(GOWPinEditorForm.Items[I].Data);
        OtherPin := AEntry.ConnectedToPin;

        PinIdent := AEntry.PinConnectIdent;
        if (GOWPinEditorForm.Items[I].StateIndex and 1 > 0) then
        begin
          if (Dispatcher <> nil) then
          begin
            if (not Dispatcher.IsConnectedTo(EventSinkPin)) then
              SourceChanged := True;

          end

          else if (OtherPin = nil) then
          begin
            if (not TOWExposedSinkPin(EventSinkPin).IsConnectedToPinName(PinIdent)) then
              SourceChanged := True;

          end

          else
          begin
            if (not EventSinkPin.IsConnectedTo(OtherPin)) then
              SourceChanged := True;

          end;
        end

        else
        begin
          if (Dispatcher <> nil) then
          begin
            if (Dispatcher.IsConnectedTo(EventSinkPin)) then
              SourceChanged := True;

          end

          else if (OtherPin = nil) then
            if (TOWExposedSinkPin(EventSinkPin).IsConnectedToPinName(PinIdent)) then
              SourceChanged := True

            else
            begin
              if (EventSinkPin.IsConnectedTo(OtherPin)) then
                SourceChanged := True;

            end;
        end;

        if (SourceChanged) then
        begin
          { TODO : Modify the forms connected trough the Dispatcher }


          Designer.Modified(EventSinkPin);

          if (OtherPin <> nil) then
          begin
            if (OtherPin is TOWPin) then
            begin
              SourceRoot := OWGetMainDesignOwner(TOWPin(OtherPin).Owner);
              if (SourceRoot is TCustomForm) then
                if (Assigned(TCustomForm(SourceRoot).Designer)) then
                  TCustomForm(SourceRoot).Designer.Modified();

            end;

          end;

          Break;
        end;

      end;

      EventSinkPin.Disconnect();
      for I := 0 to GOWPinEditorForm.Items.Count - 1 do
      begin
        Dispatcher := nil;

        AEntry := TOWEItemEntry(GOWPinEditorForm.Items[I].Data);
        OtherPin := AEntry.ConnectedToPin;
        AfterPin := AEntry.ConnectedAfterPin;

        if (GOWPinEditorForm.Items[I].StateIndex and 1 > 0) then
        begin
          if (Dispatcher <> nil) then
            EventSinkPin.ConnectToStateAfter(Dispatcher, AfterPin)

          else
          begin
            if (OtherPin is TOWStatePin) then
            begin
              if (TOWExposedPin(EventSinkPin).FDispatcher <> nil) then
                OtherPin.ConnectToStateAfter(TOWExposedPin(EventSinkPin).FDispatcher, AfterPin)

              else if (AEntry.IsDispatcher) then
                EventSinkPin.ConnectByStateAfter(OtherPin, AfterPin)

              else
                EventSinkPin.ConnectAfter(OtherPin, AfterPin);

            end

            else
              EventSinkPin.ConnectAfter(OtherPin, AfterPin);

          end;

        end;

        if (OtherPin <> nil) then
          TOWExposedPin(OtherPin).SetInEditor(False);

      end;

      Result := True;
    end;

  finally
    GOWPinEditorForm.ClearData();
  end;

end;

function SourcePinEdit(Designer: TOWPropertyDesigner; SourcePin: TOWSourcePin): boolean;
var
  I: integer;
  //  SinkPin       : TOWSinkPin;
  OtherPin: TOWBasicPin;
  AfterPin: TOWBasicPin;
  SinkRoot: TComponent;
  SinkChanged: boolean;
  PinIdent: string;
  AEntry: TOWEItemEntry;

begin
  Result := False;

  if (SourcePin = nil) then
    Exit;

  try
    if (GOWPinEditorForm.ExecuteForSource(Designer, SourcePin) = mrOk) then
    begin
      for I := 0 to GOWPinEditorForm.Items.Count - 1 do
      begin
        SinkChanged := False;
        AEntry := TOWEItemEntry(GOWPinEditorForm.Items[I].Data);
        if (not (AEntry.ConnectedToPin is TOWPin)) then
          Continue;

        OtherPin := AEntry.ConnectedToPin;
        //        AfterPin := AEntry.ConnectedAfterPin;
        if (GOWPinEditorForm.Items[I].StateIndex and 1 > 0) then
        begin
          if (OtherPin = nil) then
          begin
            //            PinIdent := GOWPinEditorForm.Items[ I ].SubItems.Strings[ 4 ];
            PinIdent := AEntry.PinConnectIdent;
            if (not TOWExposedSourcePin(SourcePin).IsConnectedToPinName(PinIdent)) then
              SinkChanged := True;

          end

          else
          begin
            if (not SourcePin.IsConnectedTo(OtherPin)) then
              SinkChanged := True;

          end;
        end

        else
        begin
          if (SourcePin.IsConnectedTo(OtherPin)) then
            SinkChanged := True;

        end;

        if (SinkChanged) then
        begin
          if (OtherPin <> nil) then
          begin
            if (OtherPin is TOWPin) then
            begin
              SinkRoot := OWGetMainDesignOwner(TOWPin(OtherPin).Owner);
              if (SinkRoot is TCustomForm) then
                if (Assigned(TCustomForm(SinkRoot).Designer)) then
                  TCustomForm(SinkRoot).Designer.Modified();

            end;
          end;
        end;
      end;

      SourcePin.Disconnect();
      TOWExposedSourcePin(SourcePin).SetInEditor(True);
      for I := 0 to GOWPinEditorForm.Items.Count - 1 do
      begin
        AEntry := TOWEItemEntry(GOWPinEditorForm.Items[I].Data);
        OtherPin := AEntry.ConnectedToPin;
        AfterPin := AEntry.ConnectedAfterPin;
        if (GOWPinEditorForm.Items[I].StateIndex and 1 > 0) then
        begin

          if (OtherPin <> nil) then
            SourcePin.ConnectAfter(OtherPin, AfterPin);

        end;

        if (OtherPin <> nil) then
          TOWExposedPin(OtherPin).SetInEditor(False);

      end;

      Result := True;
    end;
    TOWExposedSourcePin(SourcePin).SetInEditor(False);

  finally
    GOWPinEditorForm.ClearData();
  end;

end;




function TOWSourcePinPropertyEditor.GetAttributes(): TPropertyAttributes;
begin
  Result := [paDialog];
  OWRequestDesignerRefresh();
end;

procedure TOWSourcePinPropertyEditor.Edit();
var
  SourcePin: TOWSourcePin;

begin
  OWRequestRefreshEx(GetIntDesigner());
  SourcePin := TOWSourcePin(GetPin());
  if (SourcePinEdit(GetIntDesigner(), SourcePin)) then
    Modified();

end;




function TOWStatePinPropertyEditor.GetAttributes(): TPropertyAttributes;
begin
  Result := [paDialog];
  OWRequestDesignerRefresh();
end;

procedure TOWStatePinPropertyEditor.Edit();
var
  StatePin: TOWStatePin;

begin
  OWRequestRefreshEx(GetIntDesigner());
  StatePin := TOWStatePin(GetPin());
  if (OWStatePinEdit(GetIntDesigner(), StatePin)) then
    Modified();

end;




constructor TOWBasicPinListPropertyEditor.CreateEx(ADesigner: TOWPropertyDesigner; APin: TOWBasicPin; AName: string;
  PinListEditor: TOWPinListPropertyEditor);
begin
  inherited Create(ADesigner, 1);
  FPin := APin;
  FName := AName;
  FPinListEditor := PinListEditor;
  TOWExposedPin(FPin).CurrentEditorPtr := @FPin;
end;

destructor TOWBasicPinListPropertyEditor.Destroy();
begin
  if (FPin <> nil) then
    TOWExposedPin(FPin).CurrentEditorPtr := nil;

  inherited;
end;

function TOWBasicPinListPropertyEditor.GetName(): TOWPropNameString;
begin
  Result := FName;
end;

function TOWBasicPinListPropertyEditor.GetIntDesigner(): TOWPropertyDesigner;
begin
  Result := PropertyHook;
end;

procedure TOWBasicPinListPropertyEditor.SetValue(const Value: string);
var
  APin: TOWBasicPin;

begin
  APin := GetPin();
  if (OWSetPinValue(nil, APin, Value)) then
    Modified();

end;

function TOWBasicPinListPropertyEditor.GetValue(): string;
var
  APin: TOWBasicPin;

begin
  APin := GetPin();
  OWRequestRefreshEx(GetIntDesigner());
  Result := OWGetPinValue(APin, GetIntDesigner());
  FPinListEditor.CheckRefresh();
end;

function TOWBasicPinListPropertyEditor.GetPin(): TOWBasicPin;
begin
  Result := FPin;
end;




function TOWSourcePinListPropertyEditor.GetAttributes(): TPropertyAttributes;
begin
  Result := [paDialog];
  OWRequestDesignerRefresh();
end;

procedure TOWSourcePinListPropertyEditor.Edit();
var
  SourcePin: TOWSourcePin;

begin
  SourcePin := TOWSourcePin(GetPin());
  if (SourcePinEdit(GetIntDesigner(), SourcePin)) then
    Modified();

end;




procedure TOWSinkPinPropertyEditor.Edit();
var
  SinkPin: TOWSinkPin;

begin
  OWRequestRefreshEx(GetIntDesigner());
  SinkPin := TOWSinkPin(GetPin());
  if (SinkPinEdit(GetIntDesigner(), SinkPin)) then
    Modified();

end;

function TOWSinkPinPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paDialog];
  OWRequestDesignerRefresh();
end;

procedure TOWSinkPinPropertyEditor.GetValues(Proc: TGetStrProc);
var
  SinkPin: TOWSinkPin;

begin
  SinkPin := TOWSinkPin(GetPin());
  GetSinkPinValues(SinkPin, Proc);
end;




function TOWEventSinkPinPropertyEditor.GetAttributes(): TPropertyAttributes;
begin
  Result := [paDialog];
  OWRequestDesignerRefresh();
end;

procedure TOWEventSinkPinPropertyEditor.Edit();
var
  EventSinkPin: TOWMultiSinkPin;

begin
  OWRequestRefreshEx(GetIntDesigner());
  EventSinkPin := TOWMultiSinkPin(GetPin());
  if (EventSinkPinEdit(GetIntDesigner(), EventSinkPin)) then
    Modified();

end;

procedure TOWSinkPinListPropertyEditor.Edit();
var
  SinkPin: TOWSinkPin;

begin
  SinkPin := TOWSinkPin(GetPin());
  OWRequestRefreshEx(GetIntDesigner());
  if (SinkPinEdit(GetIntDesigner(), SinkPin)) then
    Modified();

end;

function TOWSinkPinListPropertyEditor.GetAttributes(): TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paDialog];
  OWRequestDesignerRefresh();
end;

procedure TOWSinkPinListPropertyEditor.GetValues(Proc: TGetStrProc);
var
  SinkPin: TOWSinkPin;

begin
  SinkPin := TOWSinkPin(GetPin());
  GetSinkPinValues(SinkPin, Proc);
end;

procedure TOWEventSinkPinListPropertyEditor.Edit();
var
  SinkPin: TOWMultiSinkPin;

begin
  OWRequestRefreshEx(GetIntDesigner());
  SinkPin := TOWMultiSinkPin(GetPin());
  if (EventSinkPinEdit(GetIntDesigner(), SinkPin)) then
    Modified();

end;

function TOWEventSinkPinListPropertyEditor.GetAttributes(): TPropertyAttributes;
begin
  Result := [paDialog];
  OWRequestDesignerRefresh();
end;

procedure TOWStatePinListPropertyEditor.Edit();
var
  StatePin: TOWStatePin;

begin
  OWRequestRefreshEx(GetIntDesigner());
  StatePin := TOWStatePin(GetPin());
  if (OWStatePinEdit(GetIntDesigner(), StatePin)) then
    Modified();

end;

function TOWStatePinListPropertyEditor.GetAttributes(): TPropertyAttributes;
begin
  Result := [paDialog];
  OWRequestDesignerRefresh();
end;

procedure TOWPinEditorForm.LinkAllButtonClick(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to ListView.Items.Count - 1 do
    //    ListView.Items [ I ].Checked := True;
    ListView.Items[I].StateIndex := ((TOWEItemEntry(ListView.Items[I].Data).StateIndex) or 1);

end;

procedure TOWPinEditorForm.UnlinkAllButtonClick(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to ListView.Items.Count - 1 do
    ListView.Items[I].StateIndex := ((TOWEItemEntry(ListView.Items[I].Data).StateIndex) and 2);
  //    ListView.Items [ I ].Checked := False;

end;

procedure TOWPinEditorForm.RestoreButtonClick(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to ListView.Items.Count - 1 do
    ListView.Items[I].StateIndex := TImageIndex(TOWEItemEntry(ListView.Items[I].Data).StateIndex);

end;

procedure TOWPinEditorForm.FormShow(Sender: TObject);
begin
  FillFormsInfo();
  UpdateLinksCount();
end;

procedure TOWPinEditorForm.FillFormsInfo();
var
  FormNames: TOWModulesColection;
  I: integer;

begin
  FormNames := TOWModulesColection.Create();

  FormsComboBox.Items.Clear();
  if (FormNames.Count = 0) then
    FormNames.Add(Root.Name);

  for I := 0 to FormNames.Count - 1 do
  begin
    if (OWCanAccessRootFromName(Designer, FormNames.Strings[I])) then
    begin
      if (FormNames.Strings[I] = Root.Name) then
      begin
        FormsComboBox.Items.Add(FormNames.Strings[I] + '  (Current)');
        FormsComboBox.ItemIndex := FormsComboBox.Items.Count - 1;
      end

      else
        FormsComboBox.Items.Add(FormNames.Strings[I]);

    end;
  end;

  if (FormNames.Count > 1) then
    FormsComboBox.Items.Add('All forms');

  PopulateAllEntries();
  if (AllSelected) then
    FormsComboBox.ItemIndex := FormsComboBox.Items.Count - 1;

  FormsComboBoxChange(Self);

  FormNames.Free();
end;

procedure TOWPinEditorForm.PopulateAllEntries();
var
  I: integer;
  CurrentRoot: TComponent;

begin
  PinsList.Clear();
  for I := 0 to Max(FormsComboBox.Items.Count - 2, 0) do
  begin
    CurrentRoot := RootFromName(FormsComboBox.Items.Strings[I]);
    PopulateSingleFormEntries(nil, CurrentRoot, True, not AllPinsCheckBox.Checked);
  end;

  RootFromName(Root.Name);
end;

procedure TOWPinEditorForm.PopulateAll();
var
  I: integer;
  CurrentRoot: TComponent;

begin
  ClearData();
  for I := 0 to FormsComboBox.Items.Count - 2 do
  begin
    CurrentRoot := RootFromName(FormsComboBox.Items.Strings[I]);
    PopulateSingleForm(nil, CurrentRoot, False, True, not AllPinsCheckBox.Checked);
  end;

  RootFromName(Root.Name);
end;

procedure TOWPinEditorForm.PopulateForm(ARootComponent: TComponent);
var
  I: integer;
  CurrentRoot: TComponent;

begin
  ClearData();
  PopulateSingleForm(ARootComponent, ARootComponent, False, False, not AllPinsCheckBox.Checked);

  for I := 0 to FormsComboBox.Items.Count - 2 do
  begin
    CurrentRoot := RootFromName(FormsComboBox.Items.Strings[I]);
    if (ARootComponent <> CurrentRoot) then
      PopulateSingleForm(ARootComponent, CurrentRoot, True, True, not AllPinsCheckBox.Checked);

  end;

end;

procedure TOWPinEditorForm.PopulateSingleFormEntries(CurrentRoot: TComponent; ARootComponent: TComponent;
  FullPath: boolean; FilterPins: boolean);
var
  Values: TStringList;
  I: integer;
  OtherPin: TOWPin;
  Entry: TOWEPinEntry;
  iPos: integer;
  Str: string;
  OwnRoot: boolean;
  OwnDataModule: boolean;
  LinkName: string;
  PinStr: string;
  Dispatcher: TOWStateDispatcher;

begin
  Values := TStringList.Create;
  try
    OwnRoot := (ARootComponent = OWGetMainOwnerComponent(FPin.Owner));
    OwnDataModule := False;

    if (not OwnRoot) then
      OwnDataModule := (ARootComponent.Name = OWGetMainOwnerComponent(FPin.Owner).Name);

    for I := 0 to OWGetDispatcherCount() - 1 do
    begin
      Dispatcher := OWGetDispatcher(I);
      if (not FPin.CanConnectToState(Dispatcher)) then
        Continue;

      Entry := TOWEPinEntry.Create();
      PinsList.Add(Entry);
      Entry.Dispatcher := Dispatcher;
      Entry.PinName := Dispatcher.Name;
      Entry.Checked := FPin.IsConnectedToState(Dispatcher);
    end;

    OWGetPinValueList(ARootComponent, FPin, Values, FilterPins);
    for I := 0 to Values.Count - 1 do
    begin
      OtherPin := TOWPin(Values.Objects[I]);

      if (FSourcePin <> nil) then
      begin
        if ((not (OtherPin is TOWSinkPin)) and (not (OtherPin is TOWStatePin))) then
          Continue;

      end

      else
      if ((not (OtherPin is TOWSourcePin)) and (not (OtherPin is TOWStatePin))) then
        Continue;

      if (OtherPin is TOWStatePin) then
        if (OtherPin.IsConnected()) then
          Continue; // Those pins are represented by the Dispatchers.

      Entry := TOWEPinEntry.Create();
      PinsList.Add(Entry);

      Str := LinkToDisplayString(ARootComponent, Values.Strings[I], FullPath, OwnRoot, PinStr);
      Entry.PinName := PinStr;

      Entry.OwnerName := Str;
      Entry.Pin := OtherPin;
      if (not FPin.CanConnectTo(OtherPin)) then
      begin
        Entry.SavedChecked := False;
        Entry.Checked := False;
      end

      else
      begin
        LinkName := Values.Strings[I];
        if (OwnDataModule) then
        begin
          iPos := Pos('.', LinkName);
          Delete(LinkName, 1, iPos);
        end;

        Entry.SavedChecked := FPin.IsLinkedTo(LinkName);
        Entry.Checked := Entry.SavedChecked;
      end;
    end;

  finally
    Values.Free;

  end;

end;

procedure TOWPinEditorForm.PopulateSingleForm(CurrentRoot: TComponent; ARootComponent: TComponent; OnlyConnected: boolean;
  FullPath: boolean; FilterPins: boolean);
begin
  ListUpdating := True;
  PopulateSingleFormInt(CurrentRoot, ARootComponent, OnlyConnected, FullPath, FilterPins);
  ListUpdating := False;
end;

function GetLinkType(APin: TOWBasicPin): string;
var
  ASinkPin: TOWBasicSinkPin;

begin
  if (APin is TOWBasicSinkPin) then
    ASinkPin := TOWBasicSinkPin(APin)

  else
  begin
    Result := 'Unknown';
    Exit;
  end;

  //  if( IsEqualGUID( ASinkPin.DownStreamID, ASinkPin.UpStreamID )) then
  if (ASinkPin.DownStreamLinkName = ASinkPin.UpStreamLinkName) then
  begin
    if (ASinkPin.DownStreamLinkName <> '') then
      Result := 'Bi-directional ( ' + ASinkPin.DownStreamLinkName + ' )';

  end

  else
  begin
    if (ASinkPin.DownStreamLinkName <> '') then
      Result := 'Downstream ( ' + ASinkPin.DownStreamLinkName + ' )    ';

    if (ASinkPin.UpStreamLinkName <> '') then
      Result := Result + 'Upstream ( ' + ASinkPin.UpStreamLinkName + ' )';

  end;

end;

function TOWPinEditorForm.EntryFromPin(Pin: TOWBasicPin): TOWEPinEntry;
var
  I: integer;

begin
  Result := nil;
  for I := 0 to PinsList.Count - 1 do
    if (PinsList.Items[I].Pin = Pin) then
    begin
      Result := PinsList.Items[I];
      Break;
    end;

end;

function TOWPinEditorForm.EntryFromDispatcher(Dispatcher: TOWStateDispatcher): TOWEPinEntry;
var
  I: integer;

begin
  Result := nil;
  for I := 0 to PinsList.Count - 1 do
    if (PinsList.Items[I].Dispatcher = Dispatcher) then
    begin
      Result := PinsList.Items[I];
      Break;
    end;

end;

procedure TOWPinEditorForm.PopulateSingleFormInt(CurrentRoot: TComponent; ARootComponent: TComponent;
  OnlyConnected: boolean; FullPath: boolean; FilterPins: boolean);
var
  Values: TStringList;
  I: integer;
  J: integer;
  AOtherPin: TOWBasicPin;
  ASinkPin: TOWSinkPin;
  Item: TListItem;
  iPos: integer;
  Str: string;
  LinkedTo: string;
  LinkType: string;
  NotifyAfter: string;
  NotifyAfter1: string;
  OwnRoot: boolean;
  OwnDataModule: boolean;
  LinkName: string;
  PinStr: string;
  Dispatcher: TOWStateDispatcher;

  Entry: TOWEPinEntry;
  Linked: boolean;

  ItemDataEntry: TOWEItemEntry;

begin
  Items.BeginUpdate();
  Values := TStringList.Create;
  try
    if (ARootComponent = nil) then
      ARootComponent := OWGetMainOwnerComponent(FPin.Owner);

    OwnRoot := (ARootComponent = OWGetMainOwnerComponent(FPin.Owner));
    OwnDataModule := False;

    if (not OwnRoot) then
      OwnDataModule := (ARootComponent.Name = OWGetMainOwnerComponent(FPin.Owner).Name);

    for I := 0 to OWGetDispatcherCount() - 1 do
    begin
      Dispatcher := OWGetDispatcher(I);
      if (not FPin.CanConnectToState(Dispatcher)) then
        Continue;

      Entry := EntryFromDispatcher(Dispatcher);
      Item := Items.Add();


      Item.ImageIndex := -1;
      Item.Caption := Dispatcher.Name;

      ItemDataEntry := TOWEItemEntry.Create(Item, FPin);
      ItemDataEntry.IsDispatcher := True;
      Item.Data := ItemDataEntry;

      Item.SubItems.Add(Str);

      if (Dispatcher.Pins[0] = FPin) then
        ItemDataEntry.ConnectedToPin := Dispatcher.Pins[1]

      else
        ItemDataEntry.ConnectedToPin := Dispatcher.Pins[0];

      Item.StateIndex := Ord(Entry.Checked);

      if (Dispatcher.PinCount = 2) then
      begin
        if (Dispatcher.Pins[0] = FPin) then
          Item.SubItems.Add(Dispatcher.Pins[1].GetFullName(True))

        else
          Item.SubItems.Add(Dispatcher.Pins[0].GetFullName(True));

      end

      else
        Item.SubItems.Add(IntToStr(Dispatcher.PinCount - 1) + ' Pins');

      if (FSourcePin = nil) then
        LinkType := GetLinkType(FPin)

      else
      begin
        if (Dispatcher.PinCount = 0) then
          LinkType := 'Unknown'

        else
        begin
          LinkType := FSourcePin.GetConnectionName(Dispatcher.Pins[0]);
          for J := 1 to Dispatcher.PinCount - 1 do
          begin
            if (FSourcePin <> Dispatcher[J]) then
              if (LinkType <> FSourcePin.GetConnectionName(Dispatcher[J])) then
              begin
                LinkType := 'Multiple types';
                Break;
              end;

          end;
        end;
      end;

      Item.SubItems.Add(Dispatcher.GetAfterPinDisplayName(FPin));
      Item.SubItems.Add(LinkType);
      Item.SubItems.Add(Dispatcher.Name);
    end;

    OWGetPinValueList(ARootComponent, FPin, Values, FilterPins);
    for I := 0 to Values.Count - 1 do
    begin
      AOtherPin := TOWBasicPin(Values.Objects[I]);
      if (not (AOtherPin is TOWStatePin)) then
        if (FSourcePin = nil) then
        begin
          if (not (AOtherPin is TOWSourcePin)) then
            Continue;

        end

        else
        begin
          if (not (AOtherPin is TOWBasicSinkPin)) then
            Continue;

        end;

      if (AOtherPin is TOWStatePin) then
        if (AOtherPin.IsConnected()) then
          Continue; // Those pins are represented by the Dispatchers.

{
      if( OtherPin is TOWSourcePin ) then
        ASourcePin := TOWSourcePin( OtherPin )

      else
        ASourcePin := NIL;
}
      Entry := EntryFromPin(AOtherPin);
      if (OnlyConnected) then
      begin
        if (Entry <> nil) then
        begin
          if (not Entry.Checked) then
            Continue;

        end

        else
        if (not FMultiSinkPin.IsLinkedTo(Values.Strings[I])) then
          Continue;

      end;

      Item := Items.Add();
      ItemDataEntry := TOWEItemEntry.Create(Item, AOtherPin); //FPin );
      Item.Data := ItemDataEntry;

      if (AOtherPin.GetRoot() <> FPin.GetRoot()) then
        Item.ImageIndex := 3

      else
        Item.ImageIndex := -1;

      Str := LinkToDisplayString(ARootComponent, Values.Strings[I], FullPath, OwnRoot, PinStr);
      Item.Caption := PinStr;

      //      Item.SubItems.AddObject( Str, Values.Objects [ I ] );
      Item.SubItems.Add(Str);
      ItemDataEntry.ConnectedToPin := TOWBasicPin(Values.Objects[I]);
      if (FSourcePin = nil) then
      begin
        if (AOtherPin is TOWSourcePin) then
        begin
          if (FPin.IsConnectedTo(AOtherPin)) then
            for J := 0 to AOtherPin.ConnectedPinCount - 1 do
              if (FPin = AOtherPin.ConnectedPin[J]) then
              begin
                ItemDataEntry.ConnectedAfterPin := TOWBasicSinkPin(AOtherPin).AfterPins[J];
                Break;
              end;

        end;
      end

      else
      begin
        if (AOtherPin is TOWBasicSinkPin) then
        begin
          if (AOtherPin.IsConnectedTo(FPin)) then
            for J := 0 to FPin.ConnectedPinCount - 1 do
              if (AOtherPin = FPin.ConnectedPin[J]) then
              begin
                ItemDataEntry.ConnectedAfterPin := TOWBasicSinkPin(FPin).AfterPins[J];
                Break;
              end;

        end;
      end;

      if (not FPin.CanConnectTo(AOtherPin)) then
      begin
        Item.StateIndex := -1;
        ItemDataEntry.StateIndex := -1;
        //        Item.Data := Pointer( -1 );
      end

      else
      begin
        //        Item.Checked := ASourcePin.IsLinkedTo( FPin );
        LinkName := Values.Strings[I];
        if (OwnDataModule) then
        begin
          iPos := Pos('.', LinkName);
          Delete(LinkName, 1, iPos);
        end;

        Linked := TOWExposedPin(FPin).IsLinkedTo(LinkName);

        if (Entry <> nil) then
          Item.StateIndex := Ord(Entry.Checked)

        else
          Item.StateIndex := Ord(Linked);

        if (AOtherPin is TOWMultiSinkPin) then
          Item.StateIndex := Item.StateIndex or 2

        else if (AOtherPin is TOWStatePin) then
          Item.StateIndex := Item.StateIndex or 4;

        ItemDataEntry.StateIndex := Item.StateIndex;
        //        Item.Data := Pointer( Item.StateIndex );
        //        Item.StateIndex := Ord( Item.Checked );
      end;

      LinkType := '';
      NotifyAfter := '';
      LinkedTo := OWGetPinValueEx(AOtherPin, Designer, CurrentRoot);
      if (FSourcePin = nil) then
      begin
        NotifyAfter := AOtherPin.GetAfterPinDisplayName(FPin);
        if (AOtherPin.ConnectedPinCount > 0) then
          LinkType := GetLinkType(AOtherPin.ConnectedPin[0]);

      end

      else
      begin
        if (AOtherPin is TOWBasicSinkPin) then
        begin
          if (AOtherPin.ConnectedPinCount = 1) then
            NotifyAfter := TOWSourcePin(AOtherPin.ConnectedPin[0]).GetAfterPinDisplayName(TOWBasicSinkPin(AOtherPin));

          if (AOtherPin is TOWSinkPin) then
          begin
            ASinkPin := TOWSinkPin(AOtherPin);
            if (ASinkPin.SourcePin <> nil) then
            begin
              LinkType := GetLinkType(ASinkPin);
              NotifyAfter := TOWSourcePin(ASinkPin.SourcePin).GetAfterPinDisplayName(ASinkPin);
            end;
          end

          else
          begin
            if (AOtherPin.ConnectedPinCount > 0) then
            begin
              LinkType := GetLinkType(AOtherPin);
              for J := 0 to AOtherPin.ConnectedPinCount - 1 do
              begin
                NotifyAfter1 := TOWSourcePin(AOtherPin.ConnectedPin[J]).GetAfterPinDisplayName(TOWBasicSinkPin(AOtherPin));
                if (NotifyAfter1 <> '') then
                begin
                  if (NotifyAfter = '') then
                    NotifyAfter := '(' + NotifyAfter1

                  else
                    NotifyAfter := NotifyAfter + ', ' + NotifyAfter1;

                end;

              end;

              if (NotifyAfter <> '') then
                NotifyAfter := NotifyAfter + ')';

            end;

          end;
        end;
      end;

      Item.SubItems.Add(LinkedTo);
      Item.SubItems.Add(NotifyAfter);
      Item.SubItems.Add(LinkType);
      Item.SubItems.Add(Values.Strings[I]);
    end;

    for I := 0 to FPin.ConnectedPinCount - 1 do
    begin
      if (FPin.ConnectedPin[I].GetRoot() = nil) then
      begin
        Item := Items.Add();

        ItemDataEntry := TOWEItemEntry.Create(Item, FPin);
        Item.Data := ItemDataEntry;

        ItemDataEntry.ConnectedToPin := FPin.ConnectedPin[I];

        Item.ImageIndex := 2;
        Item.StateIndex := 1;
        ItemDataEntry.StateIndex := Item.StateIndex;

        Item.Caption := FPin.ConnectedPin[I].GetName();
        Item.SubItems.Add(FPin.ConnectedPin[I].GetOwnerName());
        Item.SubItems.Add(FPin.GetFullName(False));
      end;
    end;

  finally
    Values.Free;
    for I := 0 to Items.Count - 1 do
    begin
      if (GOWPinEditorForm.Items[I].StateIndex and 1 > 0) then
      begin
        AOtherPin := TOWEItemEntry(Items[I].Data).ConnectedToPin;
        TOWExposedPin(AOtherPin).SetInEditor(True);
      end;
    end;

    Items.EndUpdate();

  end;

end;

procedure TOWPinEditorForm.OWMUpdateInspector(var Message: TMessage);
var
  List: TADesignerSelectionList;
  Designer: IADesigner;

begin
  GOWInRefresh := False;
end;

procedure TOWPinEditorForm.OWMUpdate(var Message: TMessage);
begin
  OWLinkAwaitsLinkingAllForms();
end;

function TOWPinEditorForm.ExecuteForSource(ADesigner: TOWPropertyDesigner; ASourcePin: TOWSourcePin): integer;
begin
  Designer := ADesigner;
  FPin := ASourcePin;
  FSourcePin := ASourcePin;
  FSinkPin := nil;
  FMultiSinkPin := nil;

  Root := OWGetMainDesignOwner(ASourcePin.Owner);

  LinkAllButton.Visible := True;
  UnlinkAllButton.Visible := True;

  ListView.Columns[0].Caption := 'Sink pin';
  ListView.Columns[2].Caption := 'Connected to';
  Caption := 'Connections - Source Pin : ' + OWValueToString(FSourcePin, '.', False, False);

  ListView.StateImages := SourcesImageList;
  OWLinkAwaitsLinkingAllForms();
  Result := ShowModal();
end;

function TOWPinEditorForm.ExecuteForSink(ADesigner: TOWPropertyDesigner; ASinkPin: TOWSinkPin): integer;
begin
  Designer := ADesigner;
  FPin := ASinkPin;
  FSinkPin := ASinkPin;
  FSourcePin := nil;
  FMultiSinkPin := nil;

  Root := OWGetMainDesignOwner(ASinkPin.Owner);

  LinkAllButton.Visible := False;
  UnlinkAllButton.Visible := False;

  ListView.Columns[0].Caption := 'Source pin';
  ListView.Columns[2].Caption := 'Connections';
  Caption := 'Connections - Sink Pin : ' + OWValueToString(FSinkPin, '.', False, False);

  ListView.StateImages := SinksImageList;
  OWLinkAwaitsLinkingAllForms();
  Result := ShowModal();
end;

function TOWPinEditorForm.ExecuteForEventSink(ADesigner: TOWPropertyDesigner; AEventSinkPin: TOWMultiSinkPin): integer;
begin
  Designer := ADesigner;
  FPin := AEventSinkPin;
  FMultiSinkPin := AEventSinkPin;
  FSinkPin := nil;
  FSourcePin := nil;

  Root := OWGetMainDesignOwner(AEventSinkPin.Owner);

  LinkAllButton.Visible := True;
  UnlinkAllButton.Visible := True;

  ListView.Columns[0].Caption := 'Source pin';
  ListView.Columns[2].Caption := 'Connections';
  Caption := 'Connections - Event Sink Pin : ' + OWValueToString(FSinkPin, '.', False, False);

  ListView.StateImages := SourcesImageList;
  OWLinkAwaitsLinkingAllForms();
  Result := ShowModal();
end;

procedure TOWPinEditorForm.UpdateLinksCount();
var
  Counter: integer;
  I: integer;
  AEnabled: boolean;
begin
  Counter := 0;
  for I := 0 to ListView.Items.Count - 1 do
    if ((ListView.Items[i].ImageIndex and 1) > 0) then
      Inc(Counter);

  LinksCountLabel.Caption := IntToStr(Counter);
  AEnabled := (((FSinkPin <> nil) or (FMultiSinkPin <> nil)) and (ListView.ItemFocused <> nil) and (ListView.ItemFocused.Checked));
  if (AEnabled) then
    if (not (TOWEItemEntry(ListView.ItemFocused.Data).ConnectedToPin is TOWSourcePin)) then
      if (TOWEItemEntry(ListView.ItemFocused.Data).ConnectedToPin.ConnectedDispatcherCount = 0) then
        AEnabled := False;

  AfterPinButton.Enabled := AEnabled;
end;

procedure TOWPinEditorForm.ClearData();
begin
  ListView.Items.Clear();
end;

procedure TOWPinEditorForm.ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);

begin
  if (ListUpdating) then
    Exit;

  UpdateLinksCount();
end;

procedure TOWPinEditorForm.ListViewColumnClick(Sender: TObject; Column: TListColumn);
begin
  ListView.Columns[ColumnToSort].ImageIndex := -1;

  if (ColumnToSort = Column.Index) then
    Direction := not Direction

  else
  begin
    Direction := True;
    ColumnToSort := Column.Index;
  end;

  ListView.Columns[ColumnToSort].ImageIndex := integer(not Direction);
end;

procedure TOWPinEditorForm.FormCreate(Sender: TObject);
begin
  PinsList := TOWEPinsList.Create(True);
  ColumnToSort := 0;
  Direction := True;
  AllSelected := False;
end;

procedure TOWPinEditorForm.FormDestroy(Sender: TObject);
begin
  PinsList.Free();
  GOWPinEditorForm := nil;
end;

procedure TOWPinEditorForm.ListViewCompare(Sender: TObject; Item1, Item2: TListItem; Data: integer; var Compare: integer);
var
  DirMultiply: integer;
  ix: integer;

begin

  if (Direction) then
    DirMultiply := 1

  else
    DirMultiply := -1;

  if (ColumnToSort = 0) then
    Compare := DirMultiply * CompareText(Item1.Caption, Item2.Caption)

  else
  begin
    ix := ColumnToSort - 1;
    Compare := DirMultiply * CompareText(Item1.SubItems.Strings[ix], Item2.SubItems.Strings[ix]);
  end;

end;

function TOWPinEditorForm.GetListItems(): TListItems;
begin
  Result := ListView.Items;
end;




function TOWBasicPropertyEditor.GetIntDesigner(): TOWPropertyDesigner;
begin
  Result := PropertyHook;
end;




function TOWBasicPinPropertyEditor.GetPin(): TOWPin;
begin
  Result := TOWPin(GetOrdValue());
end;

procedure TOWBasicPinPropertyEditor.SetValue(const Value: string);
var
  APin: TOWPin;

begin
  inherited SetValue(Value);

  APin := GetPin();
  if (OWSetPinValue(nil, APin, Value)) then
    Modified();

end;

function TOWBasicPinPropertyEditor.GetValue(): string;
var
  APin: TOWPin;

begin
  APin := GetPin();
  OWRequestRefreshEx(GetIntDesigner());
  Result := OWGetPinValue(APin, GetIntDesigner());
end;




function TOWPinListPropertyEditor.GetAttributes(): TPropertyAttributes;
begin
  Result := [paSubProperties, paReadOnly, paVolatileSubProperties, paAutoUpdate];

  OWRequestDesignerRefresh();
end;

procedure TOWPinListPropertyEditor.CheckRefresh();
var
  Collection: TOWPinList;

begin
  Collection := TOWPinList(GetOrdValue());
  if (TOWExposedPinList(Collection).LastIndicatedCount <> Collection.Count) then
  begin
    TOWExposedPinList(Collection).LastIndicatedCount := Collection.Count;
    OWResetObjectInspector(GetIntDesigner());
  end;

end;

function TOWPinListPropertyEditor.GetValue(): string;
var
  Collection: TOWPinList;

begin
  OWRequestRefreshEx(GetIntDesigner());

  Collection := TOWPinList(GetOrdValue());
  if (TOWExposedPinList(Collection).LastIndicatedCount <> Collection.Count) then
  begin
    TOWExposedPinList(Collection).LastIndicatedCount := Collection.Count;
    OWResetObjectInspector(GetIntDesigner());
  end;

  if (Collection.Count = 0) then
  begin
    Result := '(Empty)';
    Exit;
  end;

  if (Collection.Count = 1) then
  begin
    Result := '1 Pin';
    Exit;
  end;

  Result := IntToStr(Collection.Count) + ' Pins';
end;

procedure TOWPinListPropertyEditor.GetProperties(Proc: TAGetPropProc);
var
  Collection: TOWPinList;
  I: integer;
  Pin: TOWBasicPin;
begin
  Collection := TOWPinList(GetOrdValue());

  TOWExposedPinList(Collection).LastIndicatedCount := Collection.Count;
  for I := 0 to Collection.Count - 1 do
  begin
    try
      Pin := Collection.Pins[I];
      if (Pin is TOWSourcePin) then
        Proc(TOWSourcePinListPropertyEditor.CreateEx(GetIntDesigner(), TOWSourcePin(Pin), Collection.Names[I], Self))

      else if (Pin is TOWSinkPin) then
        Proc(TOWSinkPinListPropertyEditor.CreateEx(GetIntDesigner(), TOWSinkPin(Pin), Collection.Names[I], Self))

      else if (Pin is TOWMultiSinkPin) then
        Proc(TOWEventSinkPinListPropertyEditor.CreateEx(GetIntDesigner(), TOWSinkPin(Pin), Collection.Names[I], Self))

      else if (Pin is TOWStatePin) then
        Proc(TOWStatePinListPropertyEditor.CreateEx(GetIntDesigner(), TOWSinkPin(Pin), Collection.Names[I], Self))

    finally
    end;

  end;

end;

function TOWPinListOwnerPropertyEditor.GetAttributes(): TPropertyAttributes;
begin
  Result := [paSubProperties, paVolatileSubProperties, paAutoUpdate];

  OWRequestDesignerRefresh();
end;

function TOWPinListOwnerPropertyEditor.GetValue(): string;
var
  Collection: TOWPinList;

begin
  OWRequestRefreshEx(GetIntDesigner());
  //  OWLinkAwaitsLinking( Designer.Form );
  Collection := TOWPinList(GetOrdValue());
  Result := IntToStr(Collection.Count);
end;

procedure TOWPinListOwnerPropertyEditor.SetValue(const Value: string);
var
  Collection: TOWPinList;

begin
  try
    Collection := TOWPinList(GetOrdValue());
    Collection.Count := StrToInt(Value);
    Modified();
    OWResetObjectInspector(GetIntDesigner());

  except;
  end;
end;

function TOWPinEditorForm.RootFromName(RootName: string): TComponent;
var
  RealName: string;
  iPos: integer;

  CurrentComp: TComponent;

begin
  RealName := RootName;
  iPos := Pos(' ', RealName);
  if (iPos > 0) then
    Delete(RealName, iPos, 10000);

  Result := Designer.GetComponent(RealName + SEPARATOR + RealName);
  if (Result = nil) then
    Result := Designer.GetComponent(RealName);

  CurrentComp := Result;
  if (CurrentComp <> nil) then
    while (CurrentComp.Owner <> nil) do
    begin
      if (Result is TDataModule) then
        Break;

      if (Result is TFrame) then
        Break;

      if ((CurrentComp.Owner is TCustomForm) or (CurrentComp.Owner is TFrame) or (CurrentComp.Owner is TDataModule)) then
        Result := CurrentComp.Owner;

      CurrentComp := CurrentComp.Owner;
    end;

  if (Result = nil) then
    Result := Root;

end;

procedure TOWPinEditorForm.FormsComboBoxChange(Sender: TObject);
var
  SelectedRoot: TComponent;
  FormName: string;

begin
  SelectedRoot := nil;
  AllSelected := False;
  if (FormsComboBox.Items.Count > 1) then
    if (FormsComboBox.ItemIndex = FormsComboBox.Items.Count - 1) then
    begin
      PopulateAll();
      AllSelected := True;
      Exit;
    end;

  if (FormsComboBox.ItemIndex <> -1) then
  begin
    FormName := FormsComboBox.Items.Strings[FormsComboBox.ItemIndex];
    SelectedRoot := RootFromName(FormName);
  end;

  PopulateForm(SelectedRoot);
end;

procedure TOWPinEditorForm.TestButtonClick(Sender: TObject);
begin
  //OWReportAwaitsLinking();
end;

procedure TOWPinEditorForm.Image1Click(Sender: TObject);
var
  AboutForm: TOWAboutForm;
begin
  AboutPanel.BevelInner := bvLowered;
  AboutForm := TOWAboutForm.Create(Self);
  AboutForm.ShowModal();
  AboutForm.Free();
  AboutPanel.BevelInner := bvRaised;
end;

procedure TOWPinEditorForm.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  AboutPanel.BevelInner := bvLowered;
end;

procedure TOWPinEditorForm.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  AboutPanel.BevelInner := bvRaised;
end;

procedure TOWPinEditorForm.ChangeState(Item: TListItem);
var
  I: integer;

begin
  if (Item = nil) then
    Exit;

  if (Item.StateIndex and 1 <> 0) then
    Item.StateIndex := Item.StateIndex and 2

  else
  begin
    if (FSinkPin <> nil) then
    begin
      if (Item.StateIndex < 2) then
      begin
        for I := 0 to ListView.Items.Count - 1 do
          if ((ListView.Items[I].StateIndex < 2) or (TOWEItemEntry(Item.Data).ConnectedToPin is TOWBasicPin)) then
            ListView.Items[I].StateIndex := ListView.Items[I].StateIndex and 2;

      end

      else
      begin
        for I := 0 to ListView.Items.Count - 1 do
          if (TOWEItemEntry(ListView.Items[I].Data).ConnectedToPin is TOWBasicPin) then
            if (ListView.Items[I].StateIndex < 2) then
              ListView.Items[I].StateIndex := 0;

      end;
    end;

    Item.StateIndex := Item.StateIndex or 1;
  end;

end;

procedure TOWPinEditorForm.ListViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  Item: TListItem;

begin
  if (htOnStateIcon in ListView.GetHitTestInfoAt(X, Y)) then
  begin
    Item := ListView.GetItemAt(X, Y);
    Item.Selected := True;
    Item.Focused := True;
    ChangeState(Item);
  end;

end;

procedure TOWPinEditorForm.ListViewKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = ' ') then
  begin
    ChangeState(ListView.ItemFocused);
    Key := #0;
  end;
end;

procedure TOWPinEditorForm.ListViewDeletion(Sender: TObject; Item: TListItem);
begin
  TOWEItemEntry(Item.Data).Free();
  Item.Data := nil;
end;

procedure TOWPinEditorForm.AfterPinButtonClick(Sender: TObject);
var
  AForm: TOWAfterPinSelectForm;
  AEntry: TOWEItemEntry;

begin
  AForm := TOWAfterPinSelectForm.Create(Self);
  AEntry := TOWEItemEntry(ListView.ItemFocused.Data);

  if (AEntry.ConnectedToPin is TOWSourcePin) then
    AForm.FillFromSourcePin(FPin, TOWSourcePin(AEntry.ConnectedToPin), AEntry.ConnectedAfterPin)

  else
  if (AEntry.ConnectedToPin.ConnectedDispatcherCount > 0) then
    AForm.FillFromDisparcher(FPin, AEntry.ConnectedToPin.ConnectedDispatcher[0], AEntry.ConnectedAfterPin);

  if (AForm.ShowModal() = mrOk) then
    AEntry.ConnectedAfterPin := AForm.GetSelectedPin();

  AForm.Free();
end;


constructor TOWEItemEntry.Create(AItem: TListItem; APin: TOWBasicPin);
begin
  inherited Create();
  FPin := APin;
  FItem := AItem;
  if (FItem = nil) then
    Exit;
end;

procedure TOWEItemEntry.SetConnectedToPin(Value: TOWBasicPin);
begin
  FConnectedToPin := Value;
  Populate();
end;

procedure TOWEItemEntry.SetConnectedAfterPin(Value: TOWBasicPin);
begin
  FConnectedAfterPin := Value;
  Populate();
end;

procedure TOWEItemEntry.Populate();
begin
  if (FItem = nil) then
    Exit;

  if (FItem.SubItems.Count < 3) then
    Exit;

  FItem.Caption := FPin.GetName();
  FItem.SubItems[0] := FPin.GetOwnerName();

  if (FConnectedAfterPin = nil) then
    FItem.SubItems[2] := ''

  else
    FItem.SubItems[2] := FConnectedAfterPin.GetFullName(FPin.GetRoot() <> FConnectedAfterPin.GetRoot());

end;

//==================================================

initialization

  GOWPinEditorForm := TOWPinEditorForm.Create(Application);
  GOWRefreshForm := GOWPinEditorForm;


finalization

  if (GOWPinEditorForm <> nil) then
    GOWPinEditorForm.Free();

  GOWPinEditorForm := nil;

end.
