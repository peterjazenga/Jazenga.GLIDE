﻿
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplLCDLineUnit_Char;

interface

procedure LoadChar();

var
  CharAr: array [0..255, 0..6] of integer;
  CharCod: integer;

implementation

//***************************************************

procedure AddChar(const Args: array of integer);
var
  x: integer;
begin
  for x := 0 to High(Args) do
    CharAr[CharCod, x] := Args[x];
end;
//***************************************************

procedure LoadChar();
var
  x: integer;
begin
  for x := 0 to 255 do
  begin
    CharCod := x;
    case x of
      0..32: AddChar([0, 0, 0, 0, 0, 0, 0]);
      33: AddChar([4, 4, 4, 4, 4, 0, 4]);
      34: AddChar([10, 10, 0, 0, 0, 0, 0]);
      35: AddChar([0, 10, 31, 10, 31, 10, 0]);
      36: AddChar([4, 15, 20, 14, 5, 30, 4]);
      37: AddChar([25, 26, 2, 4, 8, 11, 19]);
      38: AddChar([12, 18, 20, 8, 21, 18, 13]);
      39: AddChar([4, 4, 0, 0, 0, 0, 0]);
      40: AddChar([2, 4, 8, 8, 8, 4, 2]);
      41: AddChar([8, 4, 2, 2, 2, 4, 8]);
      42: AddChar([0, 4, 21, 14, 21, 4, 0]);
      43: AddChar([0, 4, 4, 31, 4, 4, 0]);
      44: AddChar([0, 0, 0, 0, 12, 4, 8]);
      45: AddChar([0, 0, 0, 14, 0, 0, 0]);
      46: AddChar([0, 0, 0, 0, 0, 12, 12]);
      47: AddChar([1, 1, 2, 4, 8, 16, 16]);
      48: AddChar([14, 17, 19, 21, 25, 17, 14]);
      49: AddChar([4, 12, 4, 4, 4, 4, 14]);
      50: AddChar([14, 17, 1, 2, 4, 8, 31]);
      51: AddChar([14, 17, 1, 6, 1, 17, 14]);
      52: AddChar([2, 6, 10, 18, 31, 2, 2]);
      53: AddChar([31, 16, 30, 1, 1, 17, 14]);
      54: AddChar([14, 17, 16, 30, 17, 17, 14]);
      55: AddChar([31, 1, 1, 2, 4, 4, 4]);
      56: AddChar([14, 17, 17, 14, 17, 17, 14]);
      57: AddChar([14, 17, 17, 15, 1, 17, 14]);
      58: AddChar([0, 12, 12, 0, 12, 12, 0]);
      59: AddChar([0, 12, 12, 0, 12, 4, 8]);
      60: AddChar([2, 4, 8, 16, 8, 4, 2]);
      61: AddChar([0, 0, 31, 0, 31, 0, 0]);
      62: AddChar([8, 4, 2, 1, 2, 4, 8]);
      63: AddChar([14, 17, 1, 2, 4, 0, 4]);
      64: AddChar([14, 17, 19, 21, 23, 16, 15]);
      65: AddChar([14, 17, 17, 31, 17, 17, 17]);
      66: AddChar([30, 17, 17, 30, 17, 17, 30]);
      67: AddChar([14, 17, 16, 16, 16, 17, 14]);
      68: AddChar([30, 17, 17, 17, 17, 17, 30]);
      69: AddChar([31, 16, 16, 30, 16, 16, 31]);
      70: AddChar([31, 16, 16, 30, 16, 16, 16]);
      71: AddChar([14, 17, 16, 19, 17, 17, 14]);
      72: AddChar([17, 17, 17, 31, 17, 17, 17]);
      73: AddChar([14, 4, 4, 4, 4, 4, 14]);
      74: AddChar([1, 1, 1, 1, 17, 17, 14]);
      75: AddChar([17, 18, 20, 24, 20, 18, 17]);
      76: AddChar([16, 16, 16, 16, 16, 16, 31]);
      77: AddChar([17, 27, 21, 21, 17, 17, 17]);
      78: AddChar([17, 25, 21, 19, 17, 17, 17]);
      79: AddChar([14, 17, 17, 17, 17, 17, 14]);
      80: AddChar([30, 17, 17, 30, 16, 16, 16]);
      81: AddChar([14, 17, 17, 17, 17, 14, 1]);
      82: AddChar([30, 17, 17, 30, 17, 17, 17]);
      83: AddChar([14, 17, 16, 14, 1, 17, 14]);
      84: AddChar([31, 4, 4, 4, 4, 4, 4]);
      85: AddChar([17, 17, 17, 17, 17, 17, 14]);
      86: AddChar([17, 17, 17, 17, 17, 10, 4]);
      87: AddChar([17, 17, 17, 17, 21, 27, 17]);
      88: AddChar([17, 10, 4, 4, 4, 10, 17]);
      89: AddChar([17, 17, 17, 10, 4, 4, 4]);
      90: AddChar([31, 1, 2, 4, 8, 16, 31]);
      91: AddChar([12, 8, 8, 8, 8, 8, 12]);
      92: AddChar([0, 16, 8, 4, 2, 1, 0]);
      93: AddChar([6, 2, 2, 2, 2, 2, 6]);
      94: AddChar([4, 10, 17, 0, 0, 0, 0]);
      95: AddChar([0, 0, 0, 0, 0, 0, 31]);
      96: AddChar([6, 4, 2, 0, 0, 0, 0]);
      97: AddChar([0, 0, 14, 1, 15, 17, 15]);
      98: AddChar([16, 16, 30, 17, 17, 17, 30]);
      99: AddChar([0, 0, 15, 16, 16, 16, 15]);
      100: AddChar([1, 1, 15, 17, 17, 17, 15]);
      101: AddChar([0, 0, 14, 17, 31, 16, 14]);
      102: AddChar([3, 4, 31, 4, 4, 4, 4]);
      103: AddChar([0, 0, 15, 17, 15, 1, 14]);
      104: AddChar([16, 16, 22, 25, 17, 17, 17]);
      105: AddChar([4, 0, 12, 4, 4, 4, 14]);
      106: AddChar([2, 0, 6, 2, 2, 18, 12]);
      107: AddChar([16, 16, 18, 20, 24, 20, 18]);
      108: AddChar([12, 4, 4, 4, 4, 4, 14]);
      109: AddChar([0, 0, 26, 21, 21, 21, 21]);
      110: AddChar([0, 0, 22, 25, 17, 17, 17]);
      111: AddChar([0, 0, 14, 17, 17, 17, 14]);
      112: AddChar([0, 0, 30, 17, 30, 16, 16]);
      113: AddChar([0, 0, 15, 17, 15, 1, 1]);
      114: AddChar([0, 0, 11, 12, 8, 8, 8]);
      115: AddChar([0, 0, 14, 16, 14, 1, 30]);
      116: AddChar([4, 4, 31, 4, 4, 4, 3]);
      117: AddChar([0, 0, 17, 17, 17, 19, 13]);
      118: AddChar([0, 0, 17, 17, 17, 10, 4]);
      119: AddChar([0, 0, 17, 17, 21, 21, 10]);
      120: AddChar([0, 0, 17, 10, 4, 10, 17]);
      121: AddChar([0, 0, 17, 17, 15, 1, 14]);
      122: AddChar([0, 0, 31, 2, 4, 8, 31]);
      123: AddChar([3, 4, 4, 8, 4, 4, 3]);
      124: AddChar([4, 4, 4, 4, 4, 4, 4]);
      125: AddChar([24, 4, 4, 2, 4, 4, 24]);
      126: AddChar([8, 21, 2, 0, 0, 0, 0]);
      127: AddChar([0, 0, 0, 0, 0, 0, 0]);
      128: AddChar([6, 9, 28, 8, 28, 9, 6]);
      129..160: AddChar([0, 0, 0, 0, 0, 0, 0]);
      161: AddChar([4, 0, 4, 4, 4, 4, 4]);
      162: AddChar([4, 15, 20, 20, 20, 15, 4]);
      163: AddChar([6, 9, 8, 30, 8, 8, 31]);
      164: AddChar([0, 17, 14, 17, 14, 17, 0]);
      165: AddChar([17, 10, 31, 4, 31, 4, 4]);
      166: AddChar([4, 4, 4, 0, 4, 4, 4]);
      167: AddChar([14, 17, 12, 19, 14, 1, 30]);
      168: AddChar([10, 0, 31, 16, 28, 16, 31]);
      169: AddChar([14, 17, 23, 25, 23, 17, 14]);
      170: AddChar([4, 2, 14, 10, 6, 0, 14]);
      171: AddChar([0, 0, 9, 18, 9, 0, 0]);
      172: AddChar([0, 0, 0, 14, 2, 0, 0]);
      173: AddChar([0, 0, 0, 14, 0, 0, 0]);
      174: AddChar([14, 17, 29, 27, 29, 27, 14]);
      175: AddChar([31, 0, 0, 0, 0, 0, 0]);
      176: AddChar([6, 9, 6, 0, 0, 0, 0]);
      177: AddChar([0, 4, 14, 4, 0, 14, 0]);
      178: AddChar([12, 2, 4, 8, 14, 0, 0]);
      179: AddChar([12, 2, 4, 2, 12, 0, 0]);
      180: AddChar([7, 6, 8, 0, 0, 0, 0]);
      181: AddChar([0, 17, 17, 17, 25, 22, 16]);
      182: AddChar([15, 29, 29, 13, 5, 5, 5]);
      183: AddChar([0, 0, 0, 4, 0, 0, 0]);
      184: AddChar([10, 0, 14, 17, 31, 16, 14]);
      185: AddChar([4, 12, 4, 4, 4, 0, 0]);
      186: AddChar([14, 10, 14, 0, 14, 0, 0]);
      187: AddChar([0, 0, 18, 9, 18, 0, 0]);
      188: AddChar([9, 10, 4, 11, 21, 7, 1]);
      189: AddChar([17, 18, 4, 14, 17, 2, 7]);
      190: AddChar([24, 9, 26, 5, 11, 23, 1]);
      191: AddChar([4, 0, 4, 8, 16, 17, 14]);
      // Κθπθλθφΰ
      192: AddChar([14, 17, 17, 31, 17, 17, 17]);
      193: AddChar([30, 16, 16, 30, 17, 17, 30]);
      194: AddChar([30, 17, 17, 30, 17, 17, 30]);
      195: AddChar([31, 16, 16, 16, 16, 16, 16]);
      196: AddChar([15, 5, 5, 9, 17, 31, 17]);
      197: AddChar([31, 16, 16, 31, 16, 16, 31]);
      198: AddChar([21, 21, 21, 14, 21, 21, 21]);
      199: AddChar([14, 17, 1, 6, 1, 17, 14]);
      200: AddChar([17, 17, 19, 21, 25, 17, 17]);
      201: AddChar([21, 17, 19, 21, 25, 17, 17]);
      202: AddChar([17, 18, 20, 24, 20, 18, 17]);
      203: AddChar([15, 5, 5, 5, 5, 21, 9]);
      204: AddChar([17, 27, 21, 21, 17, 17, 17]);
      205: AddChar([17, 17, 17, 31, 17, 17, 17]);
      206: AddChar([14, 17, 17, 17, 17, 17, 14]);
      207: AddChar([31, 17, 17, 17, 17, 17, 17]);
      208: AddChar([30, 17, 17, 30, 16, 16, 16]);
      209: AddChar([14, 17, 16, 16, 16, 17, 14]);
      210: AddChar([31, 4, 4, 4, 4, 4, 4]);
      211: AddChar([17, 17, 17, 10, 4, 8, 16]);
      212: AddChar([4, 14, 21, 21, 21, 14, 4]);
      213: AddChar([17, 17, 10, 4, 10, 17, 17]);
      214: AddChar([17, 17, 17, 17, 17, 31, 1]);
      215: AddChar([17, 17, 17, 15, 1, 1, 1]);
      216: AddChar([21, 21, 21, 21, 21, 21, 31]);
      217: AddChar([21, 21, 21, 21, 21, 31, 1]);
      218: AddChar([24, 8, 8, 14, 9, 9, 14]);
      219: AddChar([17, 17, 17, 25, 21, 21, 25]);
      220: AddChar([16, 16, 16, 30, 17, 17, 30]);
      221: AddChar([14, 17, 1, 7, 1, 17, 14]);
      222: AddChar([18, 21, 21, 29, 21, 21, 18]);
      223: AddChar([15, 17, 17, 15, 5, 9, 17]);
      224: AddChar([0, 0, 14, 1, 15, 17, 15]);
      225: AddChar([3, 12, 16, 30, 17, 17, 14]);
      226: AddChar([0, 0, 28, 18, 28, 18, 28]);
      227: AddChar([0, 0, 31, 17, 16, 16, 16]);
      228: AddChar([0, 0, 15, 5, 9, 31, 17]);
      229: AddChar([0, 0, 14, 17, 31, 16, 14]);
      230: AddChar([0, 0, 21, 21, 14, 21, 21]);
      231: AddChar([0, 0, 30, 1, 6, 1, 30]);
      232: AddChar([0, 0, 17, 19, 21, 25, 17]);
      233: AddChar([10, 4, 17, 19, 21, 25, 17]);
      234: AddChar([0, 0, 9, 10, 12, 10, 9]);
      235: AddChar([0, 0, 15, 5, 5, 21, 9]);
      236: AddChar([0, 0, 17, 27, 21, 17, 17]);
      237: AddChar([0, 0, 17, 17, 31, 17, 17]);
      238: AddChar([0, 0, 14, 17, 17, 17, 14]);
      239: AddChar([0, 0, 31, 17, 17, 17, 17]);
      240: AddChar([0, 0, 30, 17, 30, 16, 16]);
      241: AddChar([0, 0, 14, 16, 16, 17, 14]);
      242: AddChar([0, 0, 31, 4, 4, 4, 4]);
      243: AddChar([0, 0, 17, 17, 15, 1, 14]);
      244: AddChar([0, 4, 14, 21, 21, 14, 4]);
      245: AddChar([0, 0, 17, 10, 4, 10, 17]);
      246: AddChar([0, 0, 18, 18, 18, 31, 1]);
      247: AddChar([0, 0, 17, 17, 15, 1, 1]);
      248: AddChar([0, 0, 21, 21, 21, 21, 31]);
      249: AddChar([0, 0, 21, 21, 21, 31, 1]);
      250: AddChar([0, 0, 24, 8, 14, 9, 14]);
      251: AddChar([0, 0, 17, 17, 25, 21, 25]);
      252: AddChar([0, 0, 16, 16, 28, 18, 28]);
      253: AddChar([0, 0, 14, 17, 7, 17, 14]);
      254: AddChar([0, 0, 18, 21, 29, 21, 18]);
      255: AddChar([0, 0, 15, 17, 15, 5, 9]);
    end;   // case
  end;     // for x

end;
//***************************************************


end.
