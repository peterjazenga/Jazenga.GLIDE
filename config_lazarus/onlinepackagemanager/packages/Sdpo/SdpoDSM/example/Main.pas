unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  SdpoDSM, StdCtrls;

type

  { TFMain }

  TFMain = class(TForm)
    DSMState1: TDSMState;
    DSMState2: TDSMState;
    DSMTransition1: TDSMTransition;
    DSMConnection1: TDSMConnection;
    DSMConnection2: TDSMConnection;
    DSMTransition3: TDSMTransition;
    DSMConnection3: TDSMConnection;
    DSMConnection4: TDSMConnection;
    DSMState3: TDSMState;
    DSMTransition2: TDSMTransition;
    BGo: TButton;
    Memo: TMemo;
    StateMachine: TDSMStateMachine;
    DSMTransition4: TDSMTransition;
    DSMConnection5: TDSMConnection;
    DSMConnection6: TDSMConnection;
    DSMConnection7: TDSMConnection;
    DSMTransition5: TDSMTransition;
    DSMConnection8: TDSMConnection;
    BEval: TButton;
    BCommit: TButton;
    BStepIt: TButton;
    DSMConnection9: TDSMConnection;
    DSMConnection10: TDSMConnection;
    procedure BGoClick(Sender: TObject);
    procedure DSMTransitionXClick(Sender: TObject);
    procedure BEvalClick(Sender: TObject);
    procedure DSMTransitionXFireTest(Sender: TObject;
      var IsActive: Boolean);
    procedure BCommitClick(Sender: TObject);
    procedure BStepItClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FMain: TFMain;

implementation

procedure TFMain.BGoClick(Sender: TObject);
var i,j: integer;
    link: TDSMConnection;
    txt: string;
begin
  StateMachine.Active:=true;
  memo.lines.add('<States>');
  for i:=0 to StateMachine.StatesCount-1 do begin
    memo.lines.add(' '+StateMachine.States[i].Name);
    for j:=0 to StateMachine.GetStateOutputsCount(i)-1 do begin
      link:=StateMachine.GetStateOutput(i,j);
      memo.lines.add(' - '+ Link.TransitionLink.Name +' ('+link.Name+')');
    end;
  end;
  memo.lines.add('<Transitions>');
  for i:=0 to StateMachine.TransitionsCount-1 do begin
    txt:='';
    if StateMachine.Transitions[i].ConnectionStatus=[itHasParent] then txt:=' (Hanging)';
    if StateMachine.Transitions[i].ConnectionStatus=[itHasChild] then txt:=' (Orfan)';
    memo.lines.add(' '+StateMachine.Transitions[i].Name+ txt);
    for j:=0 to StateMachine.GetTransitionOutputsCount(i)-1 do begin
      link:=StateMachine.GetTransitionOutput(i,j);
      memo.lines.add(' - '+ Link.StateLink.Name +' ('+link.Name+')');
    end;
  end;
end;

procedure TFMain.DSMTransitionXClick(Sender: TObject);
var Tra: TDSMTransition;
begin
  Tra:=Sender as TDSMTransition;
  if Tra.text='0' then Tra.text:='1' else Tra.text:='0';
end;

procedure TFMain.BEvalClick(Sender: TObject);
begin
  StateMachine.EvalNextState;
  Memo.Lines.add(StateMachine.NextState.StateName);
end;

procedure TFMain.DSMTransitionXFireTest(Sender: TObject;
  var IsActive: Boolean);
var Tra: TDSMTransition;
begin
  Tra:=Sender as TDSMTransition;
  isActive:= (Tra.Text='1');
end;

procedure TFMain.BCommitClick(Sender: TObject);
begin
  StateMachine.CommitNextState;
end;

procedure TFMain.BStepItClick(Sender: TObject);
begin
  StateMachine.StepIt;
end;

initialization
  {$I Main.lrs}

end.
