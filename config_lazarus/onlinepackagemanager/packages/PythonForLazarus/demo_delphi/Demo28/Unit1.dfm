�
 TFORM1 0
  TPF0TForm1Form1Left� Top� WidthUHeight-VertScrollBar.Range� ActiveControlButton1CaptionDemo of PythonColorclBackgroundFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height	Font.NameMS Sans Serif
Font.Pitch
fpVariable
Font.Style OldCreateOrder	PixelsPerInch`
TextHeight 	TSplitter	Splitter1Left Top� WidthEHeightCursorcrVSplitAlignalTopColor	clBtnFaceParentColorExplicitWidthM  TMemoMemo1Left Top� WidthEHeightAlignalClientLines.Strings
import p4d%L = p4d.CreateTStringList(1, 2, 3, 4)print Lfor i in L:  print i, type(i)    i = iter(L)print itry:  while True:    print i.next()except StopIteration:  print "Done"    
print L[2]L[2] = int(L[2]) * 2print L[2], type(L[2])print L[ L.add(10) ] 
ScrollBarsssBothTabOrder  TPanelPanel1Left Top�WidthEHeight,AlignalBottom
BevelOuterbvNoneTabOrder  TButtonButton1LeftTopWidthsHeightCaptionExecute scriptTabOrder OnClickButton1Click  TButtonButton2Left� TopWidth[HeightCaptionLoad script...TabOrderOnClickButton2Click  TButtonButton3LeftTopWidthYHeightCaptionSave script...TabOrderOnClickButton3Click   TMemoMemo2Left Top WidthEHeight� AlignalTop
ScrollBarsssBothTabOrder  TPythonEnginePythonEngine1IOPythonGUIInputOutput1Left   TOpenDialogOpenDialog1
DefaultExt*.pyFilter0Python files|*.py|Text files|*.txt|All files|*.*TitleOpenLeft�   TSaveDialogSaveDialog1
DefaultExt*.pyFilter0Python files|*.py|Text files|*.txt|All files|*.*TitleSave AsLeft�   TPythonGUIInputOutputPythonGUIInputOutput1	UnicodeIO		RawOutputOutputMemo2Left@  TPythonTypeptStringListEnginePythonEngine1OnCreateptStringListCreateTypeNameTStringList	TypeFlagstpfHaveGetCharBuffertpfHaveSequenceIntpfHaveInplaceOpstpfHaveRichComparetpfHaveWeakRefstpfHaveItertpfHaveClasstpfBaseType PrefixCreateModulepmP4DServices.Basic	bsGetAttr	bsSetAttrbsReprbsStrbsIter Services.InplaceNumber Services.Number Services.SequencessLengthssItem	ssAssItem Services.Mapping Left@Top0  TPythonModulepmP4DEnginePythonEngine1
ModuleNamep4dErrors Left Top0  TPythonTypeptStringListIteratorEnginePythonEngine1OnCreateptStringListIteratorCreateTypeNameTStringListIteratorPrefixCreateModulepmP4DServices.Basic	bsGetAttr	bsSetAttrbsReprbsStrbsIter
bsIterNext Services.InplaceNumber Services.Number Services.Sequence Services.Mapping Left@TopP   