�
 TFORM1 0O  TPF0TForm1Form1Left�Top� Width HeightwCaptionDemo of PythonColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrder	OnClose	FormClosePixelsPerInch`
TextHeight TMemoMemo1Left Top WidthHeight)AlignalClientLines.Stringsimport threading
import systry:  count = int(sys.argv[1])except:  count = 3 for i in range(count):  print "**** Pass", i  threading._test()print "**** Done." 
ScrollBarsssBothTabOrder  TPanelPanel1Left Top)WidthHeight,AlignalBottom
BevelOuterbvNoneTabOrder  TButtonButton1LeftTopWidthsHeightCaptionExecute scriptTabOrder OnClickButton1Click  TButtonButton2Left� TopWidth[HeightCaptionLoad script...TabOrderOnClickButton2Click  TButtonButton3LeftTopWidthYHeightCaptionSave script...TabOrderOnClickButton3Click   TPythonEnginePythonEngine1IOPythonInputOutput1Left� Top0  TOpenDialogOpenDialog1
DefaultExt*.pyFilter0Python files|*.py|Text files|*.txt|All files|*.*Left�   TSaveDialogSaveDialog1
DefaultExt*.pyFilter0Python files|*.py|Text files|*.txt|All files|*.*Left�   TPythonInputOutputPythonInputOutput1
OnSendDataPythonInputOutput1SendData	UnicodeIO	RawOutputLeft� Top0   