{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit excompress;

{$warn 5023 off : no warning about unused units}
interface

uses
  AllExCompressRegister, compressbase, TplLzmaUnit, TplZlibUnit, 
  UBitTreeDecoder, UBitTreeEncoder, UBufferedFS, UCRC, ULZBinTree, 
  ULZInWindow, ULZMABase, ULZMABench, ULZMACommon, ULZMADecoder, ULZMAEncoder, 
  ULZOutWindow, URangeDecoder, URangeEncoder, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AllExCompressRegister', @AllExCompressRegister.Register);
end;

initialization
  RegisterPackage('excompress', @Register);
end.
