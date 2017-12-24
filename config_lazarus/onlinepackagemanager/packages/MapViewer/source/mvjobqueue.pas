{
  Multi thread Queue,witch can be used without multi-thread (c) 2014 ti_dic

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit mvJobQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,syncobjs,contnrs,forms;

const ALL_TASK_COMPLETED = -1;
      NO_MORE_TASK   = 0;

type
    TjobQueue = class;

    { TJob }

    TJob = Class
      private
        FLauncher : TObject;
        FCancelled : Boolean;
        FName: String;
      protected
        Queue : TJobQueue;

        procedure DoCancel;virtual;
        Procedure WaitForResultOf(aJob : TJob);
        Procedure EnterCriticalSection;
        procedure LeaveCriticalSection;

        //should be called inside critical section
        function pGetTask : integer;virtual;
        procedure pTaskStarted(aTask: integer);virtual;abstract;
        procedure pTaskEnded(aTask : integer;aExcept : Exception);virtual;abstract;
        property Launcher : TObject read FLauncher;
      public
        procedure ExecuteTask(aTask : integer;FromWaiting : boolean);virtual;abstract;
        function Running : boolean;virtual;abstract;
        procedure Cancel;
        property Cancelled : boolean read FCancelled;
        property Name : String read FName write FName;
     end;

    TJobArray = Array of TJob;

    { TjobQueue }

    TjobQueue = Class
      private
        FMainThreadId : TThreadID;
        FOnIdle: TNotifyEvent;
        waitings : TStringList;
        FNbThread : integer;
        TerminatedThread : integer;
        FSect : TCriticalSection;
        FEvent,TerminateEvent : TEvent;
        FUseThreads: boolean;
        Threads : TList;
        Jobs : TObjectList;
        procedure pJobCompleted(var aJob: TJob);
        procedure SetUseThreads(AValue: boolean);
        procedure ClearWaitings;
      protected
        Procedure InitThreads;
        Procedure FreeThreads;
        Procedure EnterCriticalSection;
        procedure LeaveCriticalSection;
        Procedure DoWaiting(E : Exception;TaskId : integer);

        //Should be called inside critical section
        procedure pAddWaiting(aJob : TJob;aTask : integer;JobId : String);
        procedure pTaskStarted(aJob : TJob;aTask : integer);
        procedure pTaskEnded(var aJob : TJob;aTask : integer;aExcept : Exception);
        function pGetJob(out TaskId : integer;out Restart : boolean) : TJob;
        function pFindJobByName(const aName : string;ByLauncher: TObject) : TJobArray;
        procedure pNotifyWaitings(aJob : TJob);
        Function IsMainThread : boolean;
      public
        constructor Create(NbThread : integer = 5);
        destructor Destroy;override;
        procedure QueueAsyncCall(const AMethod: TDataEvent; Data: PtrInt);
        procedure QueueSyncCall(const AMethod: TDataEvent; Data: PtrInt);
        property UseThreads : boolean read FUseThreads write SetUseThreads;
        Procedure AddJob(aJob : TJob;Launcher : TObject);
        function AddUniqueJob(aJob : TJob;Launcher : TObject) : boolean;
        function CancelAllJob(ByLauncher: TObject) : TJobArray;
        function CancelJobByName(aJobName : String;ByLauncher: TObject) : boolean;
        Procedure WaitForTerminate(const lstJob : TJobArray);
        Procedure WaitAllJobTerminated(ByLauncher: TObject);
        property OnIdle : TNotifyEvent read FOnIdle write FOnIdle;
    end;


implementation
const
  WAIT_TIME = 3000;
  TERMINATE_TIMEOUT = 1000;


Type

    { EWaiting }

    EWaiting = Class(Exception)
    private
      FLauncher : TJob;
      FNewJob : TJob;
    public
      constructor Create(launcher : TJob;NewJob : TJob);
    end;

    { TRestartTask }

    TRestartTask = Class(TJob)
      private
        FStarted : Boolean;
        FJob : TJob;
        FTask : integer;
      protected
        procedure DoCancel;override;
        procedure pTaskStarted(aTask: integer);override;
        procedure pTaskEnded(aTask : integer;aExcept : Exception);override;
        function pGetTask : integer;override;
      public
        constructor Create(aJob : TJob;aTask : integer);
        procedure ExecuteTask(aTask : integer;FromWaiting : boolean);override;
        function Running : boolean;override;
    end;

    { TQueueThread }

    TQueueThread = Class(TThread)
    private
      MyQueue : TJobqueue;
      function ProcessJob : boolean;
    public
      constructor Create(aQueue: TJobQueue);
      procedure Execute; override;
    end;

{ TRestartTask }

procedure TRestartTask.DoCancel;
begin
  FJob.Cancel;
end;

procedure TRestartTask.pTaskStarted(aTask: integer);
begin
  FStarted := true;
end;

procedure TRestartTask.pTaskEnded(aTask: integer; aExcept: Exception);
begin
  Queue.pTaskEnded(FJob,FTask,aExcept);
end;

function TRestartTask.pGetTask: integer;
begin
  if FStarted then
    Result:=inherited pGetTask
  else
    Result:=1;
end;

constructor TRestartTask.Create(aJob: TJob; aTask: integer);
begin
  FJob:=aJob;
  FTask:=aTask;
end;

procedure TRestartTask.ExecuteTask(aTask: integer; FromWaiting: boolean);
begin
  FJob.ExecuteTask(FTask,true);
end;

function TRestartTask.Running: boolean;
begin
  Result:=Fstarted;
end;

{ EWaiting }

constructor EWaiting.Create(launcher: TJob; NewJob: TJob);
begin
  FLauncher:=launcher;
  FNewJob:=NewJob;
end;

{ TQueueThread }

function TQueueThread.ProcessJob : boolean;
var aJob : TJob;
    TaskId : Integer;

    Procedure SetRes(e : Exception);
    Begin
      MyQueue.EnterCriticalSection;
      Try
        MyQueue.pTaskEnded(aJob,TaskId,nil);
      finally
        MyQueue.LeaveCriticalSection;
      end;
    end;
var RestartTask : boolean;
    SomeJob : Boolean;
begin
  Result:=false;
  Repeat
    SomeJob:=false;
    MyQueue.EnterCriticalSection;
    Try
      result:=result or (MyQueue.Jobs.Count>0);
       aJob:=MyQueue.pGetJob(TaskId,RestartTask);
       if Assigned(aJob) then
       Begin
          if TaskId=ALL_TASK_COMPLETED then
          begin
            MyQueue.pJobCompleted(aJob);
            SomeJob := true;
          end
          else
          Begin
            MyQueue.FEvent.ResetEvent;
            if not(RestartTask) then
              MyQueue.pTaskStarted(aJob,TaskId);
          end;
       end;
    finally
      MyQueue.LeaveCriticalSection;
    end;
    if Assigned(aJob) then
    Begin
      SomeJob:=true;
      Try
        aJob.ExecuteTask(TaskId,RestartTask);
        SetRes(nil);
      Except
        on e : Exception do
          if e.InheritsFrom(EWaiting) then
            MyQueue.DoWaiting(e,TaskId)
          else
            SetRes(e);
      end;
    end;
  until SomeJob=false;
end;

constructor TQueueThread.Create(aQueue: TJobQueue);
begin
  MyQueue := aQueue;
  inherited Create(False);
end;

procedure TQueueThread.Execute;
var wRes : TWaitResult;
begin
  while not Terminated do
  begin
    wRes:=MyQueue.FEvent.WaitFor(WAIT_TIME);
    if not(Terminated) then
    Begin
      if not(ProcessJob) then
        if wRes=wrTimeout then
           if Assigned(MyQueue.OnIdle) then
             MyQueue.OnIdle(self);
    end;
  end;
  MyQueue.EnterCriticalSection;
  Try
     inc(MyQueue.TerminatedThread);
     if Assigned(MyQueue.TerminateEvent) then
        if MyQueue.TerminatedThread=MyQueue.Threads.count then
                MyQueue.TerminateEvent.SetEvent;
  finally
    MyQueue.LeaveCriticalSection;
  end;
end;

{ TjobQueue }

procedure TjobQueue.SetUseThreads(AValue: boolean);
begin
  if FUseThreads=AValue then
     Exit;
  FUseThreads:=AValue;
  if Fusethreads then
    InitThreads
  else
    FreeThreads;
end;

procedure TjobQueue.ClearWaitings;
var i : integer;
begin
  For i:=0 to pred(Waitings.count) do
    Waitings.Objects[i].Free;
  Waitings.Clear;
end;

procedure TjobQueue.InitThreads;
var i : integer;
begin
  Jobs:=TObjectList.Create(true);
  Threads:=TObjectList.Create(true);
  FEvent:=TEvent.Create(nil,true,false,'');
  FSect:=TCriticalSection.Create;
  TerminatedThread := 0;
  For i:=1 to FNbThread do
      Threads.Add(TQueueThread.Create(self));
end;

procedure TjobQueue.FreeThreads;
var i : integer;
begin
  if Assigned(Threads) then
  Begin
    TerminateEvent := TEvent.Create(nil,false,false,'');
    Try
      FEvent.SetEvent;
      TerminatedThread:=0;
      For i:=0 to pred(Threads.Count) do
          TQueueThread(Threads[i]).Terminate;
      TerminateEvent.WaitFor(TERMINATE_TIMEOUT);
      FreeAndNil(FSect);
      FreeAndNil(FEvent);
      FreeAndNil(Threads);
    finally
      FreeAndNil(TerminateEvent);
    end;
    FreeAndNil(Jobs);
  end;
end;

procedure TjobQueue.EnterCriticalSection;
begin
  if Assigned(FSect) and UseThreads then
     FSect.Enter;
end;

procedure TjobQueue.LeaveCriticalSection;
begin
  if Assigned(FSect) and UseThreads then
     FSect.Leave;
end;

procedure TjobQueue.DoWaiting(E : Exception;TaskId : integer);
var we : EWaiting;
begin
   EnterCriticalSection;
   try
     we:=EWaiting(e);
     pAddWaiting(we.FLauncher,TaskId,we.FNewJob.Name);
     AddUniqueJob(we.FNewJob,we.FLauncher.FLauncher);
   finally
     LeaveCriticalSection;
   end;
end;

procedure TjobQueue.pAddWaiting(aJob: TJob; aTask: integer; JobId: String);
begin
  Waitings.AddObject(JobId,TRestartTask.Create(aJob,aTask));
end;

procedure TjobQueue.pTaskStarted(aJob: TJob; aTask: integer);
begin
  aJob.pTaskStarted(aTask);
end;

procedure TjobQueue.pJobCompleted(var aJob: TJob);
Begin
     pNotifyWaitings(aJob);
     if FuseThreads then
     Begin
       Jobs.Remove(aJob);
       aJob:=nil;
     end
     else
       FreeAndNil(aJob);
end;

procedure TjobQueue.pTaskEnded(var aJob: TJob; aTask: integer; aExcept: Exception);
begin
  aJob.pTaskEnded(aTask,aExcept);
  if (aJob.pGetTask=ALL_TASK_COMPLETED) then
  Begin
     pJobcompleted(aJob);
  end;
end;

function TjobQueue.pGetJob(out TaskId : integer;out Restart : boolean): TJob;
var iJob : integer;
    aJob : TJob;
begin
  Restart:=false;
  Result:=nil;
  For iJob:=0 to pred(Jobs.Count) do
  Begin
    aJob:=TJob(Jobs[iJob]);
    if aJob.InheritsFrom(TRestartTask) then
    Begin
      result:=TRestartTask(aJob).FJob;
      TaskId:=TRestartTask(aJob).FTask;
      Restart:=true;
      Jobs.Delete(iJob);
      Exit;
    end;
    TaskId:=aJob.pGetTask;
    if (TaskId>NO_MORE_TASK) or (TaskId=ALL_TASK_COMPLETED) then
    Begin
       Result:=aJob;
       Exit;
    end;
  end;
  if not(assigned(result)) then
     TaskId:=NO_MORE_TASK;
end;

function TjobQueue.pFindJobByName(const aName: string;ByLauncher: TObject): TJobArray;
var iRes,i : integer;
begin
  SetLength(result,Jobs.count);
  iRes:=0;
  For i:=0 to pred(Jobs.Count) do
  Begin
    if TJob(Jobs[i]).Name=aName then
    begin
       if (ByLauncher=nil) or (TJob(Jobs[i]).FLauncher=ByLauncher) then
       Begin
          Result[iRes]:=TJob(Jobs[i]);
          inc(iRes);
       end;
    end;
  end;
  SetLength(result,iRes);
end;

procedure TjobQueue.pNotifyWaitings(aJob: TJob);
var JobId : String;
    ObjRestart : TRestartTask;
    idx : integer;
begin
  JobId:=aJob.Name;
  Repeat
    idx:=waitings.IndexOf(JobId);
    if idx<>-1 then
    Begin
         ObjRestart:=TRestartTask(waitings.Objects[idx]);
         waitings.Delete(idx);
         Jobs.Add(ObjRestart);
    end;
  until idx=-1;
end;

function TjobQueue.IsMainThread: boolean;
begin
  Result:=GetCurrentThreadId=FMainThreadID;
end;

constructor TjobQueue.Create(NbThread: integer);
begin
  waitings:=TStringList.create;
  FNbThread:=NbThread;
  FMainThreadId:=GetCurrentThreadId;
end;

destructor TjobQueue.Destroy;
begin
  FreeThreads;
  ClearWaitings;
  FreeAndNil(Waitings);
  inherited;
end;

procedure TjobQueue.QueueAsyncCall(const AMethod: TDataEvent; Data: PtrInt);
begin
  if UseThreads then
     Application.QueueAsyncCall(aMethod,Data)
  else
     AMethod(Data);
end;


Type

{ TSyncCallData }

 TSyncCallData = Class
     private
       FMethod : TDataEvent;
       FData : PtrInt;
     public
       Constructor Create(AMethod : TDataEvent;AData : PtrInt);
       Procedure SyncCall;
     End;

{ TSyncCallData }

constructor TSyncCallData.Create(AMethod: TDataEvent; AData: PtrInt);
begin
  FMethod:=AMethod;
  FData:=AData;

end;

procedure TSyncCallData.SyncCall;
begin
  FMethod(FData);
end;

procedure TjobQueue.QueueSyncCall(const AMethod: TDataEvent; Data: PtrInt);
var tmp : TSyncCallData;
begin
  tmp := TSyncCallData.Create(AMethod,Data);
  Try
    TThread.Synchronize(nil,@tmp.SyncCall);
  finally
    tmp.free;
  end;
end;

procedure TjobQueue.AddJob(aJob: TJob;Launcher : TObject);
var TaskId : Integer;
    restart : boolean;
begin
  aJob.FLauncher:=Launcher;
  aJob.Queue:=self;
  if Usethreads then
  Begin
    EnterCriticalSection;
    Try
      Jobs.add(aJob);
    finally
      LeaveCriticalSection;
    end;
    FEvent.SetEvent;
  end
  Else
  Begin
    Try
      Repeat
        TaskId:=aJob.pGetTask;
        restart:=false;
        if TaskId>NO_MORE_TASK then
        Begin
          pTaskStarted(aJob,TaskId);
          Try
            aJob.ExecuteTask(TaskId,restart);
            pTaskEnded(aJob,TaskId,nil);
          except
            on e : Exception do
            Begin
               if not(e.InheritsFrom(EWaiting)) then
                 pTaskEnded(aJob,TaskId,e)
               else
                 DoWaiting(e,TaskId);
            end;
          end;
        end;
        if not(Assigned(aJob)) then
           TaskId:=ALL_TASK_COMPLETED;
      until TaskId=ALL_TASK_COMPLETED;
    finally
      aJob.Free;
    end;
  end;
end;

function TjobQueue.AddUniqueJob(aJob: TJob; Launcher: TObject): boolean;
var lst : TJobArray;
begin
  Result:=true;
  if FUseThreads then
  Begin
    aJob.Queue:=self;
    aJob.FLauncher:=Launcher;
    EnterCriticalSection;
    Try
      lst:=pFindJobByName(aJob.Name,Launcher);
      if length(lst)=0 then
        Jobs.add(aJob)
      else
        Result:=false;
    finally
      LeaveCriticalSection;
    end;
    FEvent.SetEvent;;
  end
  Else
    AddJob(aJob,Launcher);
end;

function TjobQueue.CancelAllJob(ByLauncher: TObject) : TJobArray;
var i,iJob : integer;
begin
  SetLength(Result,0);
  if FUseThreads then
  Begin
    EnterCriticalSection;
    Try
      SetLEngth(Result,Jobs.Count);
      iJob:=0;
      For i:=pred(Jobs.Count) downto 0 do
      Begin
          if (ByLauncher=nil) or (TJob(Jobs[i]).FLauncher=ByLauncher) then
          Begin
             TJob(Jobs[i]).Cancel;
             Result[iJob]:=TJob(Jobs[i]);
             iJob+=1;
          End;
      End;
      SetLength(Result,iJob);
    finally
      LeaveCriticalSection;
    end;
  end;
end;

function TjobQueue.CancelJobByName(aJobName: String;ByLauncher: TObject) : boolean;
var lst : TJobArray;
    i : integer;
begin
  Result:=false;
  if FUseThreads then
  Begin
    EnterCriticalSection;
    Try
      lst:=pFindJobByName(aJobName,ByLauncher);
      For i:=low(lst) to high(lst) do
      Begin
          result:=true;
          lst[i].Cancel;
      End;
    finally
      LeaveCriticalSection;
    end;
  end;
end;

procedure TjobQueue.WaitForTerminate(const lstJob: TJobArray);
var OneFound : Boolean;
    i : integer;
    mThread : Boolean;
    TimeOut : integer;
begin
  TimeOut:=0;
  mThread:=IsMainThread;
  if FUseThreads then
  Begin
     repeat
       OneFound:=False;
       EnterCriticalSection;
       Try
         For i:=low(lstJob) to high(lstJob) do
         Begin
           if Jobs.IndexOf(lstJob[i])<>-1 then
           Begin
             OneFound:=True;
             break;
           end;
         end;
       finally
         LeaveCriticalSection;
       end;
       if OneFound and (TimeOut>200) then
         Raise Exception.Create('TimeOut');
       if mThread then
         Application.ProcessMessages;
       if OneFound then
           Sleep(100);
       Inc(TimeOut);
     until not(OneFound);
  end;
end;

procedure TjobQueue.WaitAllJobTerminated(ByLauncher: TObject);
var OneFound : boolean;
    i : integer;
    TimeOut : integer;
    mThread : Boolean;

    Procedure CheckTimeOut;
    Begin
      if TimeOut>200 then
        Raise Exception.Create('TimeOut');
      if mThread then
         Application.ProcessMessages;
      sleep(100);
      inc(TimeOut);
    end;

begin
  TimeOut:=0;
  if FUseThreads then
  Begin
    mThread:=IsMainThread;
    if ByLauncher=nil then
    Begin
      While Jobs.Count>0 do
            CheckTimeOut;
    end
    else
    Begin
       repeat
       OneFound:=False;
       EnterCriticalSection;
       Try
         For i:=0 to pred(Jobs.Count) do
         Begin
           if TJob(Jobs[i]).FLauncher=ByLauncher then
           Begin
             OneFound:=True;
             break;
           end;
         end;
       finally
         LeaveCriticalSection;
       end;
       if OneFound then
         CheckTimeOut;
     until not(OneFound);
    end;
  end;
end;

{ TjobQueue }

procedure TJob.Cancel;
var lst : Array of TRestartTask;
    i,idx : integer;
begin
  Queue.EnterCriticalSection;
  Try
    FCancelled := true;
    if (Name<>'') and (Queue.waitings.count>0) then
    Begin
      SetLength(lst,0);
        Repeat
          idx:=Queue.waitings.IndexOf(Name);
          if idx<>-1 then
          Begin
            SetLength(lst,length(lst)+1);
            lst[high(lst)]:=TRestartTask(Queue.waitings.Objects[idx]);
            Queue.waitings.Delete(idx);
          end;
        until idx=-1;
      For i:=low(lst) to high(lst) do
      Begin
        lst[i].Cancel;
        lst[i].pTaskEnded(1,nil);
        lst[i].Free;
      end;
    end;
    DoCancel;
  finally
    Queue.LeaveCriticalSection;
  end;
end;

procedure TJob.DoCancel;
begin

end;

function TJob.pGetTask: integer;
begin
  result:=ALL_TASK_COMPLETED;
end;

procedure TJob.WaitForResultOf(aJob: TJob);
begin
    Raise EWaiting.Create(self,aJob);
end;

procedure TJob.EnterCriticalSection;
begin
  Queue.EnterCriticalSection;
end;

procedure TJob.LeaveCriticalSection;
begin
  Queue.LeaveCriticalSection;
end;

end.

