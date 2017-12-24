{**********************************************************************
                PilotLogic Software House.

 Package pl_Graphics32.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit AllGR32Register;

interface

uses
  Classes, TypInfo,
  LCLIntf, LResources, LazIDEIntf, PropEdits, ComponentEditors;

procedure Register;

implementation

{$R AllGR32Register.res}

uses
  GR32,
  GR32_Dsgn_Color,
  GR32_Dsgn_Bitmap,
  GR32_Dsgn_Misc,
  GR32_Image,
  GR32_ExtImage,
  GR32_Layers,
  GR32_RangeBars,
  GR32_ColorPicker,
  G32_ProgressBar,
  GR32_Resamplers,
  GR32_Scrollbox,
  GR32_Panel32;

{ Registration }
procedure Register;
begin
  RegisterComponents('Graphics32 Base', [TPaintBox32,
                                    TImage32,
                                    TImgView32,
                                    TBitmap32ScrollBox,
                                    TBitmap32List,
                                    TRangeBar, TGaugeBar, TG32_ProgressBar,
                                    TSyntheticImage32,
                                    TGR32Panel,
                                    TColorPickerHS, 
                                    TColorPickerHSV, 
                                    TColorPickerGTK]);

  RegisterPropertyEditor(TypeInfo(TColor32),  nil, '', TColor32Property);
  RegisterPropertyEditor(TypeInfo(TBitmap32), nil, '', TBitmap32Property);

  RegisterComponentEditor(TCustomImage32, TImage32Editor);

  RegisterPropertyEditor(TypeInfo(string), TBitmap32, 'ResamplerClassName', nil);
  RegisterPropertyEditor(TypeInfo(TCustomResampler), TBitmap32, 'Resampler', TResamplerClassProperty);
  RegisterPropertyEditor(TypeInfo(string), TKernelResampler, 'KernelClassName', nil);
  RegisterPropertyEditor(TypeInfo(TCustomKernel), TKernelResampler, 'Kernel', TKernelClassProperty);
end;


end.
