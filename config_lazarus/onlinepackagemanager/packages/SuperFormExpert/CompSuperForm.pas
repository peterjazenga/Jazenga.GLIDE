unit CompSuperForm;

interface

{$IFDEF FPC}
{$Mode Delphi}
{$ENDIF}

uses
  Classes,
  Controls,
  extctrls,
  buttons,
  Forms;


type
  TFormMode = (sfmEdit, sfmConsult, sfmNew, sfmUnknown);

  TIncrustMode = (aicCenter, aicTopLeft, aicAllClient, aicAllHeight, aicCenterTop, aicAllWidth);

  TNotifyReceiveMessageEvent = procedure(sender: TObject; theMessage: string) of object;

  { TSuperBox }

  TSuperBox = class(TScrollBox)
  public
    procedure Resize ; override;
  End;

  { TSuperPanel }

  TSuperPanel = class(TPanel)
  public
    procedure Resize ; override;
  End;

  { TSuperForm }

  TSuperForm = class(TForm)

  private
    fFormMode: TFormMode;
    fOnFillFields: TNotifyEvent;
    fOnFillProperties: TNotifyEvent;
    fOnRefreshControls: TNotifyEvent;
    fAutoFillFieldsWhenShow: boolean;
    fIncrustMode: TIncrustMode;
    fAdaptParentSize: boolean;
    fOnReceiveMessage: TNotifyReceiveMessageEvent;
    fPanelParent: TWinControl;
    fFixedClientHeight: integer;
    fFixedClientWidth: integer;
    //		fCloseAction: TCloseAction;
    fOnFirstActivate: TNotifyEvent;
    fAlreadyPassedInFirstActivate: boolean;
    fAlreadyPassedInFirstShow: boolean;
    fOnShowFirstTime: TNotifyEvent;
    fOnPrepare: TNotifyEvent;
    fDescription: string;
{$IFNDEF FPC}
    fShowBevel: boolean;

    procedure SetShowBevel(value: boolean);
{$ENDIF}
    procedure Incrust(ParentControl: TWinControl);
    procedure SetcsDisplayDragImage;

  protected
    procedure CallEventFillFields; dynamic;
    procedure CallEventFillProperties; dynamic;
    procedure CallEventRefreshControls; dynamic;
    procedure CallEventFirstActivate; dynamic;
    procedure CallEventReceiveMessage(theMessage: string); dynamic;
    procedure CallEventShowFirstTime; dynamic;
    procedure CallEventPrepare; dynamic;
    procedure DoShow; override;

{$IFNDEF FPC}
    procedure Paint; override;
{$ENDIF}
    procedure Activate; override;

  public
    constructor Create(AOwner: TComponent); override;

    procedure DoFillFields;
    procedure DoFillProperties;
    procedure ShowIncrust(ParentControl: TWinControl);
    //		function  ShowIncrustModal(ParentControl: TPanel): integer;
    procedure HideIncrust;
    procedure AlignFormInParent(sender: TObject);
    procedure DoRefreshControls;
    procedure DoSendMessage(theComponent: TObject; theMessage: string); virtual;
    function ShowModal: Integer; override;
    procedure DoPrepare;

  published
    property FormMode: TFormMode read fFormMode write fFormMode;
    property OnFirstActivate: TNotifyEvent read fOnFirstActivate write fOnFirstActivate;
    property OnFillFields: TNotifyEvent read fOnFillFields write fOnFillFields;
    property OnFillProperties: TNotifyEvent read fOnFillProperties write fOnFillProperties;
    property OnRefreshControls: TNotifyEvent read fOnRefreshControls write fOnRefreshControls;
    property AutoFillFieldsWhenShow: boolean read fAutoFillFieldsWhenShow write fAutoFillFieldsWhenShow;
    property IncrustMode: TIncrustMode read fIncrustMode write fIncrustMode;
    property AdaptParentSize: boolean read fAdaptParentSize write fAdaptParentSize;
    property OnReceiveMessage: TNotifyReceiveMessageEvent read fOnReceiveMessage write fOnReceiveMessage;
    property PanelParent: TWinControl read fPanelParent;
{$IFNDEF FPC}
    property ShowBevel: boolean read fShowBevel write SetShowBevel;
{$ENDIF}
    property AlreadyPassedInFirstActivate: boolean read fAlreadyPassedInFirstActivate write fAlreadyPassedInFirstActivate;
    property OnShowFirstTime: TNotifyEvent read fOnShowFirstTime write fOnShowFirstTime;
    property OnPrepare: TNotifyEvent read fOnPrepare write fOnPrepare;
    property Description: string read fDescription write fDescription;

  end;


implementation

constructor TSuperForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

 //ControlStyle := ControlStyle - [csSetCaption, csAcceptsControls] + [csDesignInteractive];

  fFormMode := sfmEdit;
  //    fAutoFillFieldsWhenShow := true;
      //	fIncrustMode:=aicCenter;
  fAdaptParentSize := true;
  fPanelParent := nil;
  fFixedClientHeight := ClientHeight;
  fFixedClientWidth := ClientWidth;
  //	fShowBevel:=false;
  fAlreadyPassedInFirstActivate := false;
  fAlreadyPassedInFirstShow := false;
  //	fDescription:='';
end;


procedure TSuperForm.CallEventFillFields;
begin
  if assigned(fOnFillFields) then fOnFillFields(self);
end;


procedure TSuperForm.DoFillFields;
begin
  CallEventFillFields;
end;


procedure TSuperForm.CallEventFillProperties;
begin
  if assigned(fOnFillProperties) then fOnFillProperties(self);
end;

procedure TSuperForm.CallEventReceiveMessage(theMessage: string);
begin
  if assigned(fOnReceiveMessage) then fOnReceiveMessage(self, theMessage);
end;


procedure TSuperForm.DoFillProperties;
begin
  CallEventFillProperties;
end;


procedure TSuperForm.CallEventRefreshControls;
begin
  if assigned(fOnRefreshControls) then fOnRefreshControls(self);
end;


procedure TSuperForm.DoRefreshControls;
begin
  CallEventRefreshControls;
end;




procedure TSuperForm.DoShow;
begin
  SetcsDisplayDragImage;

  if (not fAlreadyPassedInFirstShow) then
    begin
      CallEventShowFirstTime; ;
      fAlreadyPassedInFirstShow := true;
    end;

  inherited;

  if fAutoFillFieldsWhenShow then CallEventFillFields;
end;


procedure TSuperForm.Incrust(ParentControl: TWinControl);
begin
  {
  case fIncrustMode of

   aicTopLeft:
    begin
    end;

   aicAllClient:
    begin
     BorderStyle:=bsNone;
    end;

   aicCenter:
begin
    end;

   aicAllHeight:
    begin
 BorderStyle:=bsSizeable;
    end;
  end;
   }
  BorderStyle := bsNone;


  parent := ParentControl;
  fPanelParent := ParentControl;

  {
  case fIncrustMode of
   aicCenter,aicTopLeft: Align:=alNone;
   aicAllClient: Align:=alClient;
end;
  }

  ClientHeight := fFixedClientHeight;
  ClientWidth := fFixedClientWidth;

  AlignFormInParent ( Self );

end;

procedure TSuperForm.ShowIncrust(ParentControl: TWinControl);
begin
  Updating;
  Incrust(ParentControl);
  Updated;
  Show;

  DoShow;
end;

{
function  TSuperForm.ShowIncrustModal(ParentControl: TPanel): integer;
begin
 Incrust(ParentControl);
//	fCloseAction:=caFree;
 Show;
end;
}

procedure TSuperForm.HideIncrust;
begin
  Hide;
end;


procedure TSuperForm.AlignFormInParent(sender: TObject);
begin
  case fIncrustMode of

    aicTopLeft:
      begin
        SetBounds ( 0, 0, Width, Height );
      end;

    aicAllClient:
      begin
        SetBounds ( 0, 0, fPanelParent.ClientWidth, fPanelParent.ClientHeight );
      end;

    aicCenter:
      begin
        SetBounds ( (fPanelParent.ClientWidth - ClientWidth) div 2, (fPanelParent.ClientHeight - ClientHeight) div 2, Width, Height );
      end;

    aicCenterTop:
      begin
        SetBounds ( 0, (fPanelParent.ClientWidth - ClientWidth) div 2, Width, Height );
      end;

    aicAllHeight:
      begin
        SetBounds ( (fPanelParent.ClientWidth - ClientWidth) div 2, 0, fFixedClientWidth, fPanelParent.Height - 10 );
      end;

    aicAllWidth:
      begin
        SetBounds ( 0, (fPanelParent.ClientHeight - ClientHeight) div 2, fPanelParent.ClientWidth, fFixedClientHeight );
        //height := fPanelParent.Height - 10;
      end;

  end;
end;


procedure TSuperForm.DoSendMessage(theComponent: TObject; theMessage: string);
begin
  if (theComponent is TSuperForm) then
    begin
      (theComponent as TSuperForm).CallEventReceiveMessage(theMessage);
    end;
end;


{$IFNDEF FPC}
procedure TSuperForm.SetShowBevel(value: boolean);
begin
  fShowBevel := value;
  repaint;
end;
{$ENDIF}

{$IFNDEF FPC}
procedure TSuperForm.Paint;
begin
  inherited paint;
  if fShowBevel then DrawButtonFace(Canvas, ClientRect, 1, bsNew, True, False, False);
end;
{$ENDIF}


procedure TSuperForm.Activate;
begin
  inherited Activate;

  if (not fAlreadyPassedInFirstActivate) then
    begin
      CallEventFirstActivate;
      fAlreadyPassedInFirstActivate := true;
    end;
end;

procedure TSuperForm.CallEventFirstActivate;
begin
  if assigned(fOnFirstActivate) then fOnFirstActivate(self);
end;

procedure TSuperForm.CallEventShowFirstTime;
begin
  if assigned(fOnShowFirstTime) then fOnShowFirstTime(self);
end;

function TSuperForm.ShowModal: Integer;
begin
  if (not fAlreadyPassedInFirstShow) then
    begin
      CallEventShowFirstTime; ;
      fAlreadyPassedInFirstShow := true;
    end;

  if fAutoFillFieldsWhenShow then CallEventFillFields;

  result := inherited ShowModal;
end;

procedure TSuperForm.DoPrepare;
begin
  CallEventPrepare;
end;

procedure TSuperForm.CallEventPrepare;
begin
  if assigned(fOnPrepare) then fOnPrepare(self);
end;


procedure TSuperForm.SetcsDisplayDragImage;
var
  n: integer;
begin
  ControlStyle := ControlStyle + [csDisplayDragImage];
  for n := 0 to ComponentCount - 1 do
    if (Components[n] is TControl)
      then TControl(Components[n]).ControlStyle := TControl(Components[n]).ControlStyle + [csDisplayDragImage];
end;

procedure AlignSuperForm ( const Control : TWinControl );
var li_i : Integer;
Begin
  with Control do
    Begin
      BeginUpdateBounds;
      for li_i := 0 to ControlCount - 1 do
        if Controls [ li_i ] is TSuperForm Then
          ( Controls [ li_i ] as TSuperForm ).AlignFormInParent( Controls [ li_i ]);
      EndUpdateBounds;
    end;

end;

{ TSuperBox }

procedure TSuperBox.Resize;
begin
  inherited Resize;
  AlignSuperForm ( Self );
end;

{ TSuperPanel }

procedure TSuperPanel.Resize;
begin
  inherited Resize;
  AlignSuperForm ( Self );
end;

end.


