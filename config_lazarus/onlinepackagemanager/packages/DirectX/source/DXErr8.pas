{******************************************************************************}
{*                                                                            *}
{*  Copyright (C) Microsoft Corporation.  All Rights Reserved.                *}
{*                                                                            *}
{*  File:       dxerr8.h                                                      *}
{*  Content:    DirectX Error Library Include File                            *}
{*                                                                            *}
{*  DirectX 8.x Delphi adaptation by Alexey Barkovoy                          *}
{*  E-Mail: directx@clootie.ru                                                *}
{*                                                                            *}
{*  Modified: 19-Jan-2004                                                     *}
{*                                                                            *}
{*  Latest version can be downloaded from:                                    *}
{*    http://clootie.ru                                                       *}
{*    http://sourceforge.net/projects/delphi-dx9sdk                           *}
{*                                                                            *}
{******************************************************************************}
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

{**********************************************************************
 Package pl_Win_DirectX.pkg
 CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}


unit DXErr8;

interface


uses
  Windows;

(*==========================================================================;
 *
 *
 *  File:   dxerr8.h
 *  Content:    DirectX Error Library Include File
 *
 ****************************************************************************)

//
//  DXGetErrorString8
//
//  Desc:  Converts an DirectX HRESULT to a string
//
//  Args:  HRESULT hr   Can be any error code from
//                      DPLAY D3D8 D3DX8 DMUSIC DSOUND
//
//  Return: Converted string
//

const
  //////////// DLL export definitions ///////////////////////////////////////
  dxerr8dll = 'dxerr81ab.dll';

function DXGetErrorString8A(hr: HRESULT): PAnsiChar; stdcall; external dxerr8dll;
function DXGetErrorString8W(hr: HRESULT): PWideChar; stdcall; external dxerr8dll;

function DXGetErrorString8(hr: HRESULT): PChar;  stdcall; external dxerr8dll
  name {$IFDEF UNICODE}'DXGetErrorString8W'{$ELSE}'DXGetErrorString8A'{$ENDIF};


function DXTraceA(strFile: PAnsiChar; dwLine: DWORD; hr: HRESULT; strMsg: PAnsiChar; bPopMsgBox: BOOL): HRESULT; stdcall; external dxerr8dll;
function DXTraceW(strFile: PWideChar; dwLine: DWORD; hr: HRESULT; strMsg: PWideChar; bPopMsgBox: BOOL): HRESULT; stdcall; external dxerr8dll;

function DXTrace(strFile: PChar; dwLine: DWORD; hr: HRESULT; strMsg: PChar; bPopMsgBox: BOOL): HRESULT; stdcall; external dxerr8dll
  name {$IFDEF UNICODE}'DXTraceW'{$ELSE}'DXTraceA'{$ENDIF};


implementation

end.
