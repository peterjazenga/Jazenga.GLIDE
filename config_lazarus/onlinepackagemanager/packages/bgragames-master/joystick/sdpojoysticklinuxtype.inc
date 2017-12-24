type
  TSdpoJoystick = class;
  { TMyThread }

  TSdpoJoystickThread = class(TThread)
  private
    //fStatusText: string;
    count: integer;
    StopThread: boolean;
    procedure SetStatus;
  protected
    procedure Execute; override;
  public
    Owner: TSdpoJoystick;
    procedure Abort;
  end;

