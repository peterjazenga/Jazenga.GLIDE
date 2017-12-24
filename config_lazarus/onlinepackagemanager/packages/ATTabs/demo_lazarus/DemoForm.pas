unit DemoForm;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, LCLProc, LCLType, attabs;

type
  { TForm1 }
  TForm1 = class(TForm)
    btnAdd: TButton;
    btnDel: TButton;
    btnColor: TButton;
    btnLeft: TButton;
    btnRight: TButton;
    btnStress: TButton;
    btnToggleSpecial: TButton;
    chkMultiline: TCheckBox;
    chkMultilineBtm: TCheckBox;
    chkVarSize: TCheckBox;
    chkCenterCaption: TCheckBox;
    chkShowFlat: TCheckBox;
    comboLayout: TComboBox;
    comboIconPos: TComboBox;
    comboShowX: TComboBox;
    EditInfo: TEdit;
    chkShowPlus: TCheckBox;
    ImageList1: TImageList;
    Label1: TLabel;
    chkNums: TCheckBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    labStatus: TLabel;
    btnModify: TButton;
    Label2: TLabel;
    chkShowFullColor: TCheckBox;
    Label3: TLabel;
    procedure btnStressClick(Sender: TObject);
    procedure btnToggleSpecialClick(Sender: TObject);
    procedure chkCenterCaptionChange(Sender: TObject);
    procedure chkMultilineBtmChange(Sender: TObject);
    procedure chkMultilineChange(Sender: TObject);
    procedure chkNumsChange(Sender: TObject);
    procedure chkShowFlatChange(Sender: TObject);
    procedure chkShowFullColorChange(Sender: TObject);
    procedure chkShowPlusChange(Sender: TObject);
    procedure chkVarSizeChange(Sender: TObject);
    procedure comboIconPosChange(Sender: TObject);
    procedure comboLayoutChange(Sender: TObject);
    procedure comboShowXChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
    procedure btnColorClick(Sender: TObject);
    procedure btnLeftClick(Sender: TObject);
    procedure btnRightClick(Sender: TObject);
    procedure EditInfoChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnModifyClick(Sender: TObject);
  private
    { Private declarations }
    LockEdit: boolean;
    procedure TabClickUserButton(Sender: TObject; AIndex: integer);
    procedure TabCloseEvent(Sender: TObject; ATabIndex: Integer; var ACanClose,
      ACanContinue: boolean);
    procedure TabMove(Sender: TObject; NFrom, NTo: Integer);
    procedure TabClick(Sender: TObject);
    procedure TabPlusClick(Sender: TObject);
    procedure Tab2PlusClick(Sender: TObject);
    procedure TabClose(Sender: TObject; ATabIndex: Integer;
      var ACanClose, ACanContinie: boolean);
    procedure TabDrawAfter_Bottom(Sender: TObject;
      AType: TATTabElemType; ATabIndex: Integer;
      C: TCanvas; const ARect: TRect; var ACanDraw: boolean);
    procedure TabDrawAfter_Top(Sender: TObject;
      AType: TATTabElemType; ATabIndex: Integer;
      C: TCanvas; const ARect: TRect; var ACanDraw: boolean);
    procedure TabDrawBefore_Bottom(Sender: TObject;
      AType: TATTabElemType; ATabIndex: Integer;
      C: TCanvas; const ARect: TRect; var ACanDraw: boolean);
  public
    { Public declarations }
    t_top, t_fox, t_cust: TATTabs;
  end;

var
  Form1: TForm1;

implementation

uses
  Math, StrUtils;

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  //avoid drag on click arrows
  DragManager.DragImmediate:= false;

  t_top:= TATTabs.Create(Self);
  t_top.Parent:= Self;
  t_top.Align:= alTop;
  t_top.Font.Color:= $E0E0E0;
  t_top.OnTabClick:= TabClick;
  t_top.OnTabPlusClick:= TabPlusClick;
  t_top.OnTabClose:= TabClose;
  t_top.OnTabMove:= TabMove;
  t_top.OnTabClose:= TabCloseEvent;
  t_top.OnTabDrawAfter:= TabDrawAfter_Top;
  t_top.OnTabClickUserButton:=TabClickUserButton;
  t_top.OptMouseDoubleClickPlus:= true;
  t_top.OptShowXButtons:= atbxShowAll;
  t_top.Height:= 48;
  t_top.OptTabHeight:= 38;
  t_top.DragMode:= dmAutomatic; //enable drag-drop
  t_top.Images:= ImageList1;
  t_top.OptVarWidth:= true;

  t_top.AddTab(-1, 'Tab');
  t_top.AddTab(-1, 'Tab middle len', nil, false, clGreen, 1);
  t_top.AddTab(-1, 'Tab ____________', nil, false, clBlue, 2);
  t_top.AddTab(-1, 'I', nil, false, clNone, 0);
  t_top.AddTab(-1, 'I');
  t_top.AddTab(-1, 'I');
  t_top.AddTab(-1, 'I');
  t_top.AddTab(-1, 'I');

  t_cust:= TATTabs.Create(Self);
  t_cust.Parent:= Self;
  t_cust.Align:= alBottom;
  t_cust.Font.Color:= clNavy;
  t_cust.Font.Size:= 12;
  t_cust.Height:= 56;
  t_cust.OnTabDrawBefore:= TabDrawBefore_Bottom;
  t_cust.OnTabDrawAfter:= TabDrawAfter_Bottom;
  t_cust.ColorBg:= $F9EADB;
  t_cust.ColorBorderActive:= clLime;
  t_cust.ColorBorderPassive:= clFuchsia;

  t_cust.OptButtonLayout:= '<,>';
  t_cust.OptShowAngled:= true;
  t_cust.OptShowArrowsNear:= false;
  t_cust.OptTabHeight:= 30;
  t_cust.OptTabWidthNormal:= 170;
  t_cust.OptSpaceInitial:= 10;
  t_cust.OptSpaceBetweenTabs:= 4;
  t_cust.OptSpacer:= 20;
  t_cust.OptSpaceXSize:= 15;
  t_cust.OptSpaceXInner:= 3;
  t_cust.OptPosition:= atpBottom;

  t_cust.AddTab(-1, 'Owner-draw', nil, false, clNone);
  t_cust.AddTab(-1, 'Tab wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww', nil, false, clGreen);
  t_cust.AddTab(-1, 'Last');

  t_fox:= TATTabs.Create(Self);
  t_fox.Parent:= Self;
  t_fox.Align:= alBottom;
  t_fox.Font.Size:= 8;

  t_fox.Height:= 42;
  t_fox.OptButtonLayout:= '';
  t_fox.OptSpaceBetweenTabs:= 8;
  t_fox.OptSpaceInitial:= 20;
  t_fox.OptSpacer:= 4;
  t_fox.OptSpaceXSize:= 13;
  t_fox.OptMouseDragEnabled:= false;
  t_fox.OptShowAngled:= true;
  t_fox.OnTabPlusClick:= Tab2PlusClick;

  t_fox.Font.Color:= clBlack;
  t_fox.ColorBg:= $F9EADB;
  t_fox.ColorBorderActive:= $ACA196;
  t_fox.ColorBorderPassive:= $ACA196;
  t_fox.ColorTabActive:= $FCF5ED;
  t_fox.ColorTabPassive:= $E0D3C7;
  t_fox.ColorTabOver:= $F2E4D7;
  t_fox.ColorCloseBg:= clNone;
  t_fox.ColorCloseBgOver:= $D5C9BD;
  t_fox.ColorCloseBorderOver:= $B0B0B0;
  t_fox.ColorCloseX:= $7B6E60;
  t_fox.ColorArrow:= $5C5751;
  t_fox.ColorArrowOver:= t_fox.ColorArrow;

  t_fox.AddTab(-1, 'Firefox');
  t_fox.AddTab(-1, 'A tab _____________________________________________________', nil, false, clGreen);
  t_fox.AddTab(-1, 'Tab middle len', nil, false, clBlue);
end;

procedure TForm1.btnStressClick(Sender: TObject);
var
  i: integer;
begin
  for i:= t_top.TabCount-1 downto 1 do
    t_top.DeleteTab(i, false, false);
  for i:= 1 to 300 do
    t_top.AddTab(-1, IntToStr(i));
end;

procedure TForm1.btnToggleSpecialClick(Sender: TObject);
var
  d: TATTabData;
begin
  d:= t_top.GetTabData(t_top.tabIndex);
  if d=nil then Exit;

  d.TabSpecial:= not d.TabSpecial;
  if d.TabSpecial then
    d.TabFontStyle:= [fsItalic, fsBold]
  else
    d.TabFontStyle:= [];

  t_top.Invalidate;
end;

procedure TForm1.chkCenterCaptionChange(Sender: TObject);
begin
  if chkCenterCaption.Checked then
    t_top.OptCaptionAlignment:= taCenter
  else
    t_top.OptCaptionAlignment:= taLeftJustify;
  t_top.Invalidate;
end;

procedure TForm1.chkMultilineBtmChange(Sender: TObject);
begin
  t_cust.OptVarWidth:= chkMultilineBtm.Checked;
  t_cust.OptMultiline:= chkMultilineBtm.Checked;
  t_cust.Invalidate;
end;

procedure TForm1.chkMultilineChange(Sender: TObject);
begin
  t_top.OptMultiline:= chkMultiline.Checked;
  t_top.Invalidate;
end;

procedure TForm1.chkNumsChange(Sender: TObject);
begin
  t_cust.OptShowNumberPrefix:= IfThen(chkNums.Checked, '%d. ', '');
  t_cust.Invalidate;
end;

procedure TForm1.chkShowFlatChange(Sender: TObject);
begin
  t_top.OptShowFlat:= chkShowFlat.Checked;
  t_fox.OptShowFlat:= chkShowFlat.Checked;
  t_cust.OptShowFlat:= chkShowFlat.Checked;
  t_top.Invalidate;
  t_fox.Invalidate;
  t_cust.Invalidate;
end;

procedure TForm1.chkShowFullColorChange(Sender: TObject);
begin
  t_top.OptShowEntireColor:= chkShowFullColor.Checked;
  t_top.Invalidate;
end;

procedure TForm1.chkShowPlusChange(Sender: TObject);
begin
  t_top.OptShowPlusTab:= chkShowPlus.Checked;
  t_top.Invalidate;
end;

procedure TForm1.chkVarSizeChange(Sender: TObject);
begin
  t_top.OptVarWidth:= chkVarSize.Checked;
  t_top.Invalidate;
end;

procedure TForm1.comboIconPosChange(Sender: TObject);
begin
  t_top.OptIconPosition:= TATTabIconPosition(comboIconPos.ItemIndex);
  t_top.Invalidate;
end;

procedure TForm1.comboLayoutChange(Sender: TObject);
begin
  t_top.OptButtonLayout:= comboLayout.Text;
  t_top.OptShowArrowsNear:= Pos('<>', t_top.OptButtonLayout)>0;
  t_top.Invalidate;
end;

procedure TForm1.comboShowXChange(Sender: TObject);
begin
  t_top.OptShowXButtons:= TATTabShowClose(comboShowX.ItemIndex);
  t_top.Invalidate;
end;

procedure TForm1.btnAddClick(Sender: TObject);
begin
  t_top.AddTab(t_top.TabIndex+1, 'test '+StringOfChar('n', Random(20)), nil, false, Random(65000));
end;

procedure TForm1.btnDelClick(Sender: TObject);
begin
  t_top.DeleteTab(1, true, false);
end;

procedure TForm1.btnColorClick(Sender: TObject);
var
  d: TATTabData;
begin
  d:= t_top.GetTabData(t_top.tabindex);
  d.TabColor:= Random(60000);
  t_top.Invalidate;
end;

procedure TForm1.btnLeftClick(Sender: TObject);
begin
  t_top.tabIndex:= t_top.TabIndex-1;
end;

procedure TForm1.btnRightClick(Sender: TObject);
begin
  t_top.tabIndex:= t_top.TabIndex+1;
end;

procedure TForm1.TabClick(Sender: TObject);
var
  d: TATTabData;
begin
  d:= t_top.GetTabData(t_top.TabIndex);
  LockEdit:= true;
  if Assigned(d) then
    EditInfo.Text:= d.TabCaption
  else
    EditInfo.Text:= '';
  LockEdit:= false;
end;

procedure TForm1.TabPlusClick(Sender: TObject);
begin
  btnAdd.Click;
end;

procedure TForm1.Tab2PlusClick(Sender: TObject);
begin
  t_fox.AddTab(t_fox.TabIndex+1, 'test');
end;


procedure TForm1.TabClose(Sender: TObject; ATabIndex: Integer;
  var ACanClose, ACanContinie: boolean);
{
var
  d: TATTabData;
  s: string;
  }
begin
  {
  d:= (Sender as TATTabs).GetTabData(ATabIndex);
  if d=nil then Exit;
  s:= d.TabCaption;
  ACanClose:= Pos('Tab', s)>0;
  }
  ACanClose:= true;
end;

procedure TForm1.EditInfoChange(Sender: TObject);
var
  d: TATTabData;
begin
  d:= t_top.GetTabData(t_top.tabIndex);
  if d=nil then Exit;
  if LockEdit then Exit;

  d.TabCaption:= EditInfo.Text;
end;


procedure TForm1.FormShow(Sender: TObject);
begin
end;

procedure TForm1.TabDrawAfter_Bottom(Sender: TObject;
  AType: TATTabElemType; ATabIndex: Integer;
  C: TCanvas; const ARect: TRect; var ACanDraw: boolean);
begin
  if ATabIndex<0 then Exit;
  C.Font.Name:= 'Arial';
  C.Font.Size:= 9;
  C.Font.Color:= clBlue;
  C.TextOut((ARect.Left+ARect.Right) div 2 - 8, ARect.Top+1, Inttostr(ATabIndex));
end;

procedure TForm1.TabDrawAfter_Top(Sender: TObject; AType: TATTabElemType;
  ATabIndex: Integer; C: TCanvas; const ARect: TRect; var ACanDraw: boolean);
begin
  case AType of
    aeButtonUser,
    aeButtonUserOver:
      begin
        C.Font.Color:= clYellow;
        C.Brush.Color:= IfThen(AType=aeButtonUserOver, clRed, clBlue);
        C.TextOut(ARect.Left, ARect.Top+3, '_'+IntToStr(ATabIndex));
      end;
  end;
end;

procedure TForm1.TabDrawBefore_Bottom(Sender: TObject;
  AType: TATTabElemType; ATabIndex: Integer;
  C: TCanvas; const ARect: TRect; var ACanDraw: boolean);
var
  NColor: TColor;
  R: TRect;
begin
  case AType of
    {
    aeBackground:
    begin
      NColor:= C.Brush.Color;
      //C.Brush.Style:= bsFDiagonal;
      C.Brush.Color:= clNavy;
      C.FillRect(ARect);
      C.Brush.Color:= NColor;
      C.Brush.Style:= bsSolid;
      ACanDraw:= false;
    end;
    }
    aeTabIconX,
    aeTabIconXOver:
    begin
      NColor:= C.Pen.Color;
      C.Pen.Width:= 2;
      C.Pen.Color:= IfThen(AType=aeTabIconX, clLtGray, clNavy);
      R:= Rect(ARect.Left+2, ARect.Top+2, ARect.Right-2, ARect.Bottom-2);
      C.Rectangle(R);
      C.Pen.Color:= NColor;
      C.Pen.Width:= 1;
      ACanDraw:= false;
    end;
  end;  
end;

procedure TForm1.TabMove(Sender: TObject; NFrom, NTo: Integer);
var s: string;
begin
  if NFrom=-1 then s:= 'add at index '+IntToStr(NTo) else
    if NTo=-1 then s:= 'delete at index '+IntToStr(NFrom) else
      s:= Format('move from %d to %d', [NFrom, NTo]);
  labStatus.Caption:= 'Status: '+s;
end;

procedure TForm1.btnModifyClick(Sender: TObject);
var
  d: TATTabData;
begin
  d:= t_top.GetTabData(t_top.tabIndex);
  if d=nil then Exit;

  d.TabModified:= not d.TabModified;
  t_top.Invalidate;
end;

procedure TForm1.TabCloseEvent(Sender: TObject; ATabIndex: Integer;
  var ACanClose, ACanContinue: boolean);
begin
  ACanClose:= Application.MessageBox('Close this tab?', 'Demo',
    MB_OKCANCEL+MB_ICONQUESTION) = ID_OK;
end;

procedure TForm1.TabClickUserButton(Sender: TObject; AIndex: integer);
begin
  ShowMessage('User button '+IntToStr(AIndex));
end;

end.
