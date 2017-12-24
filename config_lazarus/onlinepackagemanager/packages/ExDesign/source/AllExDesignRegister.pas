{**********************************************************************
                PilotLogic Software House.
  
 Package pl_ExDesign.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit AllExDesignRegister;

interface

uses
  Classes, TypInfo,
  lresources, lclintf,
  ELDsgxSurface,
  ELDsgxObjectInsp,
  ELMDIForm,
  ELMDIFormPanel,
  ELDsgxPropStore,
  ELSizeControl,
  OMultiPanel;


procedure Register;

implementation

{$R AllExDesignRegister.res}

procedure Register;
begin


  RegisterComponents('Extra Design', [ TplDesignSurface,
                                       TplDesignScrollBox,
                                       TplDesignPanel,
                                       TplSizeControl,
                                       TplObjectInspector,
                                       TplMDIForm,
                                       TplMDIFormsPanel,
                                       TplPropertiesStore,
                                       TOMultiPanel
                                       ]);

end;

end.


