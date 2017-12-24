unit RXHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  rxtoolbar;

type
  TToolbarButtonStyleCntrl = (tbrcNotChange, tbrcDropDown, tbrcDropDownExtra);

  PNavigateRec = ^TNavigateRec;
  TNavigateRec = packed record
    Name:string;
    Cond:string;
    Next:PNavigateRec;
  end;

  TOnNavigateEvent = procedure(Sender:TObject; const EventName, EventMacro:string) of object;
  { TRXHistory }

  TRXHistory = class(TComponent)
  private
    FButtonNext: string;
    FButtonPrior: string;
    FButtonStyle: TToolbarButtonStyleCntrl;
    FNextButton: TToolbarItem;
    FNextButtonName: string;
    FOnNavigateEvent: TOnNavigateEvent;
    FPriorButton: TToolbarItem;
    FPriorButtonName: string;
    FToolPanel: TToolPanel;
    function GetNextButtonName: string;
    function GetPriorButtonName: string;
    procedure SetButtonStyle(const AValue: TToolbarButtonStyleCntrl);
    procedure SetNextButtonName(const AValue: string);
    procedure SetPriorButtonName(const AValue: string);
    procedure SetToolPanel(const AValue: TToolPanel);
    function SetBtn(const ABtnName: string;var Button:TToolbarItem):boolean;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    property PriorButton:TToolbarItem read FPriorButton;
    property NextButton:TToolbarItem read FNextButton;
  published
    property ToolPanel:TToolPanel read FToolPanel write SetToolPanel;
    property PriorButtonName:string read GetPriorButtonName write SetPriorButtonName;
    property NextButtonName:string read GetNextButtonName write SetNextButtonName;
    property ButtonStyle:TToolbarButtonStyleCntrl read FButtonStyle write SetButtonStyle default tbrcNotChange;
    property OnNavigateEvent:TOnNavigateEvent read FOnNavigateEvent write FOnNavigateEvent;
  end;

procedure Register;

implementation
uses PropEdits, Componenteditors, TypInfo;

type

  { TTRXHistoryBtnNameProperty }

  TTRXHistoryBtnNameProperty = class(TStringPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{ TTRXHistoryBtnNameProperty }

function TTRXHistoryBtnNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=inherited GetAttributes;
  Result:=Result + [paValueList, paSortList, paMultiSelect];
end;

procedure TTRXHistoryBtnNameProperty.GetValues(Proc: TGetStrProc);
var
  ToolPanel:TToolPanel;
  i:integer;
begin
  ToolPanel := GetObjectProp(GetComponent(0), 'ToolPanel') as TToolPanel;
  if Assigned(ToolPanel) then
    for I := 0 to ToolPanel.Items.Count - 1 do
    begin
      if Assigned(ToolPanel.Items[i].Action) then
       Proc(ToolPanel.Items[i].Action.Name);
    end;
end;

procedure Register;
begin
  RegisterComponents('RX',[TRXHistory]);

  RegisterPropertyEditor(TypeInfo(string), TRXHistory, 'PriorButtonName', TTRXHistoryBtnNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TRXHistory, 'NextButtonName', TTRXHistoryBtnNameProperty);
end;

{ TRXHistory }

procedure TRXHistory.SetToolPanel(const AValue: TToolPanel);
begin
  if FToolPanel=AValue then exit;
  FToolPanel:=AValue;
end;

function TRXHistory.SetBtn(const ABtnName: string;var Button:TToolbarItem):boolean;
var
  i:integer;
begin
  Result:=false;
  if not Assigned(FToolPanel) then exit;
  Button:=FToolPanel.Items.ByActionName[ABtnName];
  Result:=Assigned(Button);
  if Result then
  begin
    case FButtonStyle of
      tbrcDropDown:Button.ButtonStyle:=tbrDropDown;
      tbrcDropDownExtra:Button.ButtonStyle:=tbrDropDownExtra;
    end;
  end;
end;

procedure TRXHistory.Loaded;
begin
  inherited Loaded;
  if not SetBtn(FNextButtonName, FNextButton) then
    FNextButtonName:='';
  if not SetBtn(FPriorButtonName, FPriorButton) then
    FPriorButtonName:='';
end;

constructor TRXHistory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtonStyle:=tbrcNotChange;
end;

procedure TRXHistory.SetNextButtonName(const AValue: string);
begin
  if FNextButtonName=AValue then exit;
  if csLoading in ComponentState then
    FNextButtonName:=AValue
  else
  begin
    if SetBtn(AValue, FNextButton) then
      FNextButtonName:=AValue
    else
      FNextButtonName:='';
  end;
end;

procedure TRXHistory.SetButtonStyle(const AValue: TToolbarButtonStyleCntrl);
begin
  if FButtonStyle=AValue then exit;
  FButtonStyle:=AValue;
end;

function TRXHistory.GetNextButtonName: string;
begin
  if Assigned(NextButton) and Assigned(NextButton.Action) then
    Result:=NextButton.Action.Name
  else
    Result:='';
end;

function TRXHistory.GetPriorButtonName: string;
begin
  if Assigned(PriorButton) and Assigned(PriorButton.Action) then
    Result:=PriorButton.Action.Name
  else
    Result:='';
end;

procedure TRXHistory.SetPriorButtonName(const AValue: string);
begin
  if FPriorButtonName=AValue then exit;
  if csLoading in ComponentState then
    FPriorButtonName:=AValue
  else
  begin
    if SetBtn(AValue, FPriorButton) then
      FPriorButtonName:=AValue
    else
      FPriorButtonName:='';
  end;
end;

end.
