{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}

unit Pas2PhpDefines;

{$INCLUDE pas2php.inc}

interface

uses
  Classes, SysUtils;

type

  TPas2PhpOption = (p2pIgnoreFileAge, p2pSrcOutNoHeader, p2pNoPascalOutput, p2pNoElementListTitles);

  TPas2PhpOptions = set of TPas2PhpOption;

type

  OChar = object
  const
    SingleQuote = char('''');
    DoubleQuote = char('"');
    Space = char(' ');
    Colon = char(':');
    SemiColon = char(';');
    Dollar = char('$');
    Amp = char('&');
  end;

  OProduct = object
  const
    Name = 'Pas2Php';
    Slogan = 'Converts Object Pascal to PHP';
    Website = 'http://www.wascal.net/?page=pas2php';
    Version = '0.9';
    NameVersion = Name + ' v' + Version;
    Copyright = 'Copyright (C) 2014-2016 Derek John Evans';
  end;

const

  GPas2PhpSilent = [p2pSrcOutNoHeader, p2pNoPascalOutput, p2pNoElementListTitles];
  GPas2PhpDefine = 'PAS2PHP';

  GOutputIndentCount = 2;

type

  OFileName = object
  const
    ExtPas = '.pas';
    ExtPhp = '.php';
    Config = 'pas2php.php';
    Include = 'pas2php_include\';
  end;

resourcestring

  GAbstractMembersNotSupported = 'Abstract members are not supported. (Use virtual with exceptions)';
  GArrayHighDoesntMatchValues = 'Array high bounds doesn''t match array values.';
  GArrayLowBoundMustBeZeroDot = 'Array low bound must be zero.';
  GBadPascalString = 'Bad Pascal String';
  GClasses = 'Classes';
  GConstants = 'Constants';
  GConvertedFromPascalBy = 'Converted From Pascal By';
  GElements = 'Elements';
  GExpectedTPrimitiveExprDot = 'Expected TPrimitiveExpr';
  GExpressions = 'Expressions';
  GFunctions = 'Functions';
  GIndexingConstantsNotSupportedDot = 'Indexing Constants is Not Supported.';
  GMembers = 'Members';
  GStaticArraysNotSupportedDot = 'Static Arrays are Not Supported (Use Dynamic or Variant Arrays).';
  GTypes = 'Types';
  GUnableToConvertPascalFileDot = 'Unable to Convert Pascal File.';
  GUnknownClassTypeDot = 'Unknown Class Type';
  GUnknownExprKindDot = 'Unknown Expression Kind.';
  GUnsupportedArrayRangeDot = 'Unsupported array range.';
  GUsesList = 'UsesList';
  GVariables = 'Variables';
  GWriteComment = 'WriteComment';
  GWritePhp = 'WritePhp';

implementation

end.
