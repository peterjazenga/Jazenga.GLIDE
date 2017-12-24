
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplLCDAnimatorEditor;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Grids, Graphics, Menus,
  Dialogs, ExtCtrls, Messages,LCLtype,lresources,
  PropEdits,ComponentEditors, Spin, Buttons,
  TplLCDScreenUnit;

 type
   TLCDAnimatorEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function  GetVerb(Index: Integer): String; //override;
    function  GetVerbCount: Integer; override;
  end;

  TLCDAnimatorCodeEditorProperty = class(TPropertyEditor)
    procedure Edit; override;
    function  GetValue: String; //override;
    function  GetAttributes: TPropertyAttributes; override;
    private
  end;


  { TLCDAnimatorCodeEditorForm }

  TLCDAnimatorCodeEditorForm = class(TForm)
    ctLoad: TButton;
    ctSave: TButton;
    ctUpdate: TButton;
    CancelButton: TButton;
    Code: TListBox;
    EditLine: TEdit;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    OkButton: TButton;
    InsertButton: TButton;
    AddButton: TButton;
    HorzScrollButton: TButton;
    OpenDialog1: TOpenDialog;
    PanelEdit: TPanel;
    PanelTop: TPanel;
    PanelBottom: TPanel;
    ctValue: TSpinEdit;
    SaveDialog1: TSaveDialog;
    VertScrollButton: TButton;
    LineBeginningButton: TButton;
    LineEndingButton: TButton;
    InstructionEndingButton: TButton;
    SetIntensityButton: TButton;
    AnimationDelayButton: TButton;
    ResetDisplayButton: TButton;
    GotoLineButton: TButton;
    DeleteButton: TButton;
    CodeCellPUM: TPopupMenu;
    DeleteMenu: TMenuItem;
    DeleteAllMenu: TMenuItem;
    SynthaxPanel: TPanel;
    SynthaxLabel2: TLabel;
    SynthaxLabel1: TLabel;
    SynthaxLabel3: TLabel;
    InsertMenu: TMenuItem;
    AddMenu: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    SyntaxAnalysisMenu: TMenuItem;

    procedure ctLoadClick(Sender: TObject);
    procedure ctSaveClick(Sender: TObject);
    procedure ctUpdateClick(Sender: TObject);
    procedure CodeClick(Sender: TObject);
    procedure InsertLineClick(Sender: TObject);
    procedure DeleteLineClick(Sender: TObject);
    procedure DeleteAllClick(Sender: TObject);
    procedure AddLineClick(Sender: TObject);
    procedure SyntaxAnalysisMenuClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure xxButtonClick(Sender: TObject);
    procedure xxSampleClick(Sender: TObject);
  public
    Procedure UpdateEditLine;
  end;


var
  LCDAnimatorCodeEditorForm: TLCDAnimatorCodeEditorForm;

implementation

//================ TLCDAnimatorEditor ==================

function  TLCDAnimatorEditor.GetVerbCount: Integer;
begin
 Result := 1;
end;

function  TLCDAnimatorEditor.GetVerb(Index: Integer): String;
begin
  Result := 'LCDAnimator Code Editor';
end;

procedure TLCDAnimatorEditor.ExecuteVerb(Index: Integer);
var F: TLCDAnimatorCodeEditorForm;
    i: Byte;
    CA:TplLCDAnimator;
begin

  if (Component is TplLCDAnimator) then
    CA:=TplLCDAnimator(Component) else
    exit;

  F := TLCDAnimatorCodeEditorForm.Create(Application);
  F.Caption := 'TplLCDAnimator Code Editor - ' + CA.Name;

  try
    F.Code.Items.Text:=CA.Code.Text;


    if F.ShowModal = mrOK  //Display the Editor form
    then begin
           CA.Code.Text:=F.Code.Items.Text;
           try Designer.Modified; except end;
           end;

  finally
    F.Free;
  end;
end;

//======================TLCDAnimatorCodeEditorProperty ==============================

function TLCDAnimatorCodeEditorProperty.GetValue: String;
begin
  Result := '(TStrings)';
end;

function TLCDAnimatorCodeEditorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TLCDAnimatorCodeEditorProperty.Edit;
var F: TLCDAnimatorCodeEditorForm;
    CA:TplLCDAnimator;
    i: Integer;
begin
 // C := GetComponent(0);
  if (GetComponent(0) is TplLCDAnimator) then
    CA:=TplLCDAnimator(GetComponent(0)) else
    exit;

  //.....................................................
  F := TLCDAnimatorCodeEditorForm.Create(Application);
  F.Caption := 'TplLCDAnimator Code Editor - ' + CA.Name;

  try
   F.Code.Items.Text:=CA.Code.Text;


    if F.ShowModal = mrOK  //Display the Editor form
    then begin
           CA.Code.Text:=F.Code.Items.Text;
          // try Designer.Modified; except end;
           end;

  finally
    F.Free;
  end;
end;

//==================== TLCDAnimatorCodeEditorForm ==================================
Procedure TLCDAnimatorCodeEditorForm.UpdateEditLine;
  var i:integer;
 begin
  i:=Code.ItemIndex;
  if i>-1 then
  EditLine.Text:=Code.Items[i] else
  EditLine.Text:='';
 end;

procedure TLCDAnimatorCodeEditorForm.CodeClick(Sender: TObject);
begin
  UpdateEditLine;
end;

procedure TLCDAnimatorCodeEditorForm.ctUpdateClick(Sender: TObject);
  var i:integer;
 begin
  i:=Code.ItemIndex;
  if i=-1 then exit;
  Code.Items[i]:=EditLine.Text;
 end;

procedure TLCDAnimatorCodeEditorForm.ctLoadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
   code.Items.LoadFromFile(OpenDialog1.FileName);
end;

procedure TLCDAnimatorCodeEditorForm.ctSaveClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    code.Items.SaveToFile(SaveDialog1.FileName);
end;


procedure TLCDAnimatorCodeEditorForm.InsertLineClick(Sender: TObject);
 var i:integer;
begin
  i:=Code.ItemIndex;
  if i>-1 then Code.Items[i]:=Code.Items[i]+']';

  Code.Items.Insert(i,'[');
  Code.ItemIndex:=i;
  UpdateEditLine;
end;


procedure TLCDAnimatorCodeEditorForm.AddLineClick(Sender: TObject);
 var i:integer;
begin
 i:=Code.ItemIndex;
 if i>-1 then Code.Items[i]:=Code.Items[i]+']';

 Code.Items.Add('[');
 Code.ItemIndex:=Code.Count-1;
 UpdateEditLine;
end;


procedure TLCDAnimatorCodeEditorForm.DeleteLineClick(Sender: TObject);
 var i:integer;
begin
 i:=Code.ItemIndex;
 if i<0 then exit;

 Code.Items.Delete(i);

 if i=Code.Count then i:=i-1;
 Code.ItemIndex:=i;
 UpdateEditLine;
end;


procedure TLCDAnimatorCodeEditorForm.DeleteAllClick(Sender: TObject);
begin
 Code.Clear;
 UpdateEditLine;
end;

procedure TLCDAnimatorCodeEditorForm.xxButtonClick(Sender: TObject);
  var ss:string;
      i:integer;
 begin
 if Not (Sender is TButton) then exit;
 if Code.Count=0 then AddLineClick(nil);
 i:=Code.ItemIndex;

 if i=-1 then exit;

 ss:=Code.Items[i];

 if TButton(Sender).Tag=0 then
   ss:=ss+TButton(Sender).Caption else
   ss:=ss+TButton(Sender).Caption+IntToStr(ctValue.Value)+');';

 Code.Items[i]:=ss;
 UpdateEditLine;
 end;

procedure TLCDAnimatorCodeEditorForm.xxSampleClick(Sender: TObject);
 var i:integer;
begin
 if  NOT (Sender is TComponent) then exit;
 i:=TComponent(Sender).Tag;

 Case i of
  1:  //Cycling
    begin
     Code.Items.Add('[HorzScroll(2); ]');
     Code.Items.Add('[HorzScroll(1); VertScroll(1); ]');
     Code.Items.Add('[HorzScroll(1); VertScroll(1); ]');
     Code.Items.Add('[VertScroll(1); ]');
     Code.Items.Add('[VertScroll(1); ]');
     Code.Items.Add('[HorzScroll(-1); VertScroll(1); ]');
     Code.Items.Add('[HorzScroll(-1); VertScroll(1); ]');
     Code.Items.Add('[HorzScroll(-2); ]');
     Code.Items.Add('[HorzScroll(-2); ]');
     Code.Items.Add('[HorzScroll(-1); VertScroll(-1); ]');
     Code.Items.Add('[HorzScroll(-1); VertScroll(-1); ]');
     Code.Items.Add('[VertScroll(-1); ]');
     Code.Items.Add('[VertScroll(-1); ]');
     Code.Items.Add('[HorzScroll(1); VertScroll(-1); ]');
     Code.Items.Add('[HorzScroll(1); VertScroll(-1);]');
     Code.Items.Add('[HorzScroll(2); ]');
    end;
  2: // Flashing
    begin
     Code.Items.Add('[SetIntensity(-127); ]');
     Code.Items.Add('[SetIntensity(127); ]');
    end;
  3:// Gravity
    begin
     Code.Items.Add('[VertScroll(1); ]');
     Code.Items.Add('[VertScroll(1); ]');
     Code.Items.Add('[VertScroll(2); ]');
     Code.Items.Add('[VertScroll(3); ]');
     Code.Items.Add('[VertScroll(4); ]');
     Code.Items.Add('[VertScroll(-4); ]');
     Code.Items.Add('[VertScroll(-3); ]');
     Code.Items.Add('[VertScroll(-1); ]');
     Code.Items.Add('[VertScroll(1); ]');
     Code.Items.Add('[VertScroll(2); ]');
     Code.Items.Add('[VertScroll(2); ]');
     Code.Items.Add('[VertScroll(3); ]');
     Code.Items.Add('[VertScroll(-2); ]');
     Code.Items.Add('[VertScroll(-1); ]');
     Code.Items.Add('[VertScroll(1); ]');
     Code.Items.Add('[VertScroll(1); ]');
     Code.Items.Add('[VertScroll(1); ]');
     Code.Items.Add('[VertScroll(-1); ]');
     Code.Items.Add('[VertScroll(1); ]');
     Code.Items.Add('[VertScroll(0); ]');
     Code.Items.Add('[VertScroll(0); ]');
     Code.Items.Add('[VertScroll(0); ]');
    end;
  4://Waving
    begin
     Code.Items.Add('[VertScroll(1); HorzScroll(2); ]');
     Code.Items.Add('[VertScroll(2); HorzScroll(2); ]');
     Code.Items.Add('[VertScroll(3); HorzScroll(3); ]');
     Code.Items.Add('[VertScroll(2); HorzScroll(2); ]');
     Code.Items.Add('[VertScroll(1); HorzScroll(2); ]');
     Code.Items.Add('[HorzScroll(2); ]');
     Code.Items.Add('[VertScroll(-1); HorzScroll(2); ]');
     Code.Items.Add('[VertScroll(-2); HorzScroll(2); ]');
     Code.Items.Add('[VertScroll(-3); HorzScroll(3); ]');
     Code.Items.Add('[VertScroll(-2); HorzScroll(2); ]');
     Code.Items.Add('[VertScroll(-1); HorzScroll(2); ]');
     Code.Items.Add('[HorzScroll(2); ]');
    end;
 end;
end;


//===================================================================
function CodeSynthaxAnalysis(var Caption: String): TColor;
var n1, n2, n3: Byte;
begin
  CodeSynthaxAnalysis := clRed;
  Caption := Trim(Caption);
  n1 := NbOfThings(Caption, ';');
  n2 := NbOfThings(Caption, '(');
  n3 := NbOfThings(Caption, ')');

  if Caption = ''
  then begin
         CodeSynthaxAnalysis := clYellow;
         Caption := 'Nul line';
         end
  else if Copy(Caption, 1, 1) <> '['
       then Caption := 'Error of ['
       else if Copy(Caption, Length(Caption), 1) <> ']'
            then Caption := 'Error of ]'
            else if not((n1 = n2) and (n2 = n3))
                 then begin
                        if n2 <> n3 then Caption := 'Error of ( )'
                                    else if n1 <> n2 then  Caption := 'Error of ;';
                        end
                 else begin
                        CodeSynthaxAnalysis := clLime;
                        Caption := 'Ok'
                        end;

end;


procedure TLCDAnimatorCodeEditorForm.SyntaxAnalysisMenuClick( Sender: TObject);
begin
 { SyntaxAnalysisMenu.Checked := not SyntaxAnalysisMenu.Checked;

  if SyntaxAnalysisMenu.Checked then CodeStringGrid.ColWidths[0] := 70
                                else CodeStringGrid.ColWidths[0] := 45;
                                }
end;


procedure TLCDAnimatorCodeEditorForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var i, NbErr: Integer;
    str: String;
    tmr: TModalResult;
begin
  NbErr := 0;
  tmr := mrIgnore;
  {
  for i := 0 to CodeStringGrid.RowCount - 1
  do begin
       str := CodeStringGrid.Cells[1, i];
       if CodeSynthaxAnalysis(str) = clRed then Inc(NbErr);
       end;
             }
  if NbErr <> 0 then tmr := MessageDlg('Code synthax error(s) detected.' + #13 + #10 +
                                       #13 + #10 + 'Close Editor anyway?',
                                       mtWarning, [mbAbort, mbIgnore], 0);
  Tag := NbErr;

  if tmr = mrIgnore then CanClose := True
                    else CanClose := False;
end;

//=======================================================
initialization
  {$I TplLCDAnimatorEditor.lrs}

end.
