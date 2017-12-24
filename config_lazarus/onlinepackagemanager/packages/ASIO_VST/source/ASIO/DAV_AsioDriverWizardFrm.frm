object AsioDriverWizardForm: TAsioDriverWizardForm
  Left = 311
  Top = 246
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderStyle = bsDialog
  Caption = 'ASIO Driver Project Wizard'
  ClientHeight = 345
  ClientWidth = 432
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  object LbEffectName: TLabel
    Left = 190
    Top = 264
    Width = 82
    Height = 13
    Caption = 'Plugin unit name:'
  end
  object BlSeparator: TBevel
    Left = 0
    Top = 42
    Width = 432
    Height = 2
    Align = alTop
    Shape = bsBottomLine
  end
  object PnHeader: TPanel
    Left = 0
    Top = 0
    Width = 432
    Height = 42
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 0
    object ImageASIO: TImage
      Left = 3
      Top = 3
      Width = 32
      Height = 32
      AutoSize = True
      Picture.Data = {
        055449636F6E0000010003001010000001000800680500003600000020200000
        01000800A80800009E0500003030000001000800A80E0000460E000028000000
        1000000020000000010008000000000040010000000000000000000000000000
        00000000000000004E4E4C0076776C00747474006E6E6D008686860092949300
        655F5F006E6C6C007575750066666500797A7A00544F5000433D3D0062646400
        5D585D00716D70007A7869003D41AD00D6D8CE00290D1000B7B4B400B3AAAA00
        8C7E7E009F9A9A008D828300776A6B00756A6A005F525300756F6F00391D2300
        588B5600919E90003A3EE4002126DA0099907E007364650053434300E4E1E000
        AD8B8C009E989700817373006D636300140000009C9D9C00A1A6A60017000000
        4170380034B5300055557900262DE5004F3727008D848400D1D4D400ECF0F000
        16000000857A7B00695A5A00260D0D008C8A89009C9E9D000E00000048814200
        6BAC67007A79710094969500796F6E00DCE2E10071656500877A790066525400
        A09C9B0082797900AAACAC00544545004B3A3A00757171007C847D00A59BA300
        525251006B6867006D6E6D006C6B6A00787979007A7B7B007E807F0078777800
        737272006E706F00686768006B6B6A00686A6800616362005E5C5D0072737300
        FFFFFF0000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000000000505152535455565758595A
        5B5C5D5E40414243444546471814494A4B4C4D4E313214342B35143738143514
        3C3D3E3F2122231425261428291417142D2E2F30111213141516141419141B1C
        1D1E1F200002030405060708090A0B0C0D0E0F10000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008000000000000000
        00000000000000000000000080000000FFFF0000FFFF0000FFFF0000FFFF0000
        FFFF000028000000200000004000000001000800000000008004000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000000100000005000000080000000B000000100101001302030016040400
        190606001B0809001C0B0B001E0D0E002110110026121300291818002C1C1D00
        301F200032202100332122003423240034252500322627002E2828002B2A2900
        2B2A2A002C2B2A002D2C2C002F2E2D002F312D0030362E0030412D002E562B00
        2D6729002B7626002A7E25002D7A29002F6D2C00325E3000364C350038413700
        393C38003B3939003C3839003A383D0036354600323152002D2C610025257A00
        1D1E94001718AC001718C1001718D0001818D7001718DD001A1BDD001F20D800
        2223CF002526C5002B2BB5003030A30036358E003A377C003E3B7300433E6700
        49415C004D43520050444C0052454A00534549005447480055494800554B4900
        574D4A0059504D005B5450005D5652005F57540060585500625A5700645C5900
        665D5B00675F5C0068605E006A6160006E64650071686900756C6D00756E7000
        756F7100766F7300767074007570760074707700726F7A00706E7D006F6D7F00
        6C6B82006B6A860068678A0066668E00666690006A6A8E0071708A0078768700
        7B7985007D7B84007F7C8200817E8100837E8000857F7F008580800085818100
        8582820086848400888585008B8686008B8888008C8989008D8B8A008E8D9100
        8E8D97008F90A0008F91A3009093A4009194A20093969F0094989C0095999800
        959A9400959A9400969D9400979E9500989F9600999F97009A9F98009C9E9A00
        9D9E9A009D9D9B009E9D9C009F9F9C009FA19D00A0A29E00A0A29E00A1A29F00
        A2A1A000A3A2A100A3A3A200A4A3A300A4A4A400A5A4A500A5A5A500A6A5A600
        A6A6A600A7A7A700A7A8A700A8A9A800A8A9A800A9AAA800A9AAA800AAABA800
        AAABA900ABACA900ABADAA00ADADAA00AEAEAB00AFAFAD00B0B0AD00B1B0AE00
        B1B1AF00B1B1B000B2B2B100B2B2B100B2B3B100B3B3B200B4B4B200B5B4B300
        B6B5B400B7B5B500B7B6B500B8B6B600B9B6B500BBB6B500BCB5B500BDB5B600
        BFB6B600C0B6B700C1B6B700C2B7B800C2B7B800C1B8B800C0B8B800BFB9B900
        BFB9B900BFBAB900BEBAB900BEBBB900BEBBB900BEBCBA00BEBCBA00BEBCBA00
        BEBEBB00BFBEBB00BFBFBC00BFBFBD00C0C0BE00C1C0BF00C1C1C100C2C1C200
        C3C2C200C3C3C300C4C3C400C4C4C400C5C4C500C6C5C600C7C7C700C9C8C800
        CAC9CA00CBCACA00CCCBCB00CDCCCC00CDCDCD00CECDCD00CFCECE00CFCFCE00
        CFCFCE00D0CFCF00D1D0D000D1D1D100D2D2D100D2D3D200D2D4D300D3D4D400
        D4D5D500D5D6D600D7D7D700D7D8D800D8D9D800D9DADA00DBDBDB00DCDDDC00
        DDDEDE00DEDFDF00DFE0E000E1E2E100E3E4E400E4E6E500E6E8E700FFFFFF00
        EDEEEE00F0F1F000F3F4F3000505050505050505050505050505050505050505
        0505050505050505050505050505050505050505050505050505050505050505
        0505050505050505050505050505050505050505050505050505050505050505
        0505050505050505050505050505050505050505050505050505050505050505
        0505050505050505050505050505050505050505050505050505050505050505
        0505050505050505050505050505050505050505050505050505050505050505
        0505050505050505050505050505050505050505050505050505050505050505
        0505050505050505050505050505050505050505050505050505050505050505
        0505050505050505050505050505050505050505050505050505050505050505
        0505050505050505050505050505050505050505050505050505050505050505
        0505050505050505050505050505050505050505050505050505050505050505
        05050505050505050505050505C7858F99949CA5AEC7C9C7D4D3B8B2AAAEA797
        8E8E8B8E84857C7B7A7B70E305A5908576ACC5D5B47CA8B87973B8E3D6847BA0
        A3A376585159767B767560C705B56E630A79D6EC5409581918491285DF16128C
        B0550D30490D175D764F5AC7059C3A69492FB2C3130ADD78FDFF1812ED180FA5
        590951A397600912602728BB053C3A697B0A4B4A0A1FFDF8F59E120EEA1910B6
        190A70917C84100A54282792803A3A69D1189685097BFBEEC25B1671ED1512B2
        120A7A8C7C84150A4A282727053C3A69D15F48490BB8F48E8F78B2F7DA1412B8
        150A73917C84130A4E28278705903A68CDCD15124DF3E75172FEEFD2DC2F19B3
        5B0B55928E7B0D125D2728BC05BB6967B5D3580D75F4F37B1A5D2059E72F189D
        AA59124A56151459762859D005B7A4ABAEC7A978D7EDF0FB9C5A72D0DF7A71A6
        B3B27A534C5779857C7C70E00505050505050505050505050505050505050505
        0505050505050505050505050505050505050505050505050505050505050505
        0505050505050505050505050505050505050505050505050505050505050505
        0505050505050505050505050505050505050505050505050505050505050505
        0505050505050505050505050505050505050505050505050505050505050505
        0505050505050505050505050505050505050505050505050505050505050505
        0505050505050505050505050505050505050505050505050505050505050505
        0505050505050505050505050505050505050505050505050505050505050505
        0505050505050505050505050505050505050505050505050505050505050505
        0505050505050505050505050505050505050505050505050505050505050505
        0505050505050505050505050505050505050505050505050505050505050505
        050505050505050505050505FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8000000080000000
        8000000080000000800000000000000080000000800000008000000080000000
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF2800000030000000600000000100080000000000
        800A0000000000000000000000010000000000001D0203001E0505001C030400
        000000001C0508001E1011002202030022020500210506002503050026050600
        25020300250609002205090025090A00230B0C002A0406002B0609002B0B0B00
        2B090600290E1100261213002C1314002D1A1B002B171800310A0C0033141500
        331C1C0038191A002C1010002A1F2000361E2000342322003B2324003B2A2B00
        382929003C333300382B2C002B7826002E6E2A0025601D003D437E00442C2C00
        46272900443334004C3C3C0048363700553B3C00472F3100421F2300533C4000
        593D4D004D3F3F0042773E004D46450046594500544344005C4C4C0057474800
        5B4F510057514D005B55540059585700434377004D4D6B004665440047764700
        5967590056735400654A4B0063535300645B5B006B5C5C006856560075585A00
        6B4D4F0064645D00755F5F00646363006B6465006D6D6D006B6A6A0069696700
        6F767300736D6D007C6C6D0075656500766F710077726E00747272007B737400
        7E7E7D007C7A7A007A7977006B756C00645F65003F40610017169D0013148B00
        0E13A4001418AA003132B60035358F000D11D800090AE100060AEB00050BF500
        040CF9000A0BF6001715E9000F13F2002B2ECA002827E2003D479B00494A8D00
        4951A8007A7C86006E7591005A6396004C48F1006E6BEE001EA61B0026991C00
        2A8A1D002E892A00289D2300358E310024A41C0026A81B0027A622001F9F1D00
        428B3C0039A331007B807C006F826E006EBA6E00499949007D8286007E859200
        7BC57D008468690082747500827C7C0086797A00967879008B6F7000847F8200
        84827E009C7F8000828282008585850083848300868989008B8485008A8A8900
        8D8E8E008C8B8B0085868900848492008D908E008D9291008E929B00928C8D00
        97878600978F900093938E00919191009596950094939300969A99009A949500
        9C9798009A9A9A009C9C9B00989996009EA3A100A6888900A39C9D00A5979700
        B8969700B18D8E00A89EA100B99CA100A4A19D00A2A2A200A6A6A600A3A4A300
        ABA4A400AAAAAA00ADADAD00ABACAB00A7A9A900ACB0AF00ADB2B100B2ADAD00
        B4A6A700B1B1B100B6B6B600B4B3B300B6BABA00B9B7B700BAB3B300BAB9B900
        BDBDBD00BBBCBB00B8B8B700B3AEB0009CA09F009193F6009DA2E700BBC0BF00
        B1C3B100ACD4AB00A4D5A300BEC2C100BDC4C300BDEBC100C2BDBC00C5B7B500
        D9B9B900CEAAAA00CABEC000CFC9BE00C3C3C300C6CAC900C3CAC900CAC5C500
        CACACA00CECDCD00CBCCCC00C7C8C700CED2D200D2CDCD00D5C8C600D1D1D100
        D5D5D400D6DAD900D5DCDA00DAD5D500D9DAD900DCDCDC00D5D8D600D2D1CF00
        D4DBFA00DBEBDB00E2DBDB00E3D3CF00EFEFEF00E2E1E100F3F3F300FFFFFF00
        FDFEFE00FBFBFC00F3F9F900EDF1EF00DFE0E800030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        030303030303030303030303030303030303030303F8AEA6A4A8A8A8A8A7AEAE
        AAAEAEACADACACACADACACABABAAAEA7A7A7A8A8A7A5A5A4A5A6A69F9F9BA6A6
        A69B9BF803EB5C9CA2A6A7AEACB1B8B9BFBDC4C6C6C6C8C8C5C6C4BDBCB9B9AF
        ADAEA7A6A29C9A9A96965B5B5B5B5D5C5D5D51EB03EB96ACADADCFBFC1C6CDCB
        D2C7C7D2E3E3CBC6C8E1E0DFD6CDC0BDBCBCB9AFCFADA8935C5B97989A9D9797
        979559EB03EB96ACA8542A4FC0C5CCD6BA2E2A39AA3A211F2139ADE6E6A2302A
        4FBEBFB99B46211B1B16223C5D9697855D5B59EB03EB9A8A71591A1FB8CAD2E6
        8D060046A30A328F4D1D21ADE799000046C0BD8E1F0024595A380D0824549A43
        355054EB03EB746F679E46068EDFDFE6390612ABC246E3F8F046063BE59A0806
        46C6AB2000219AADAEA63801022E5C447A834CEB03EC656B67A1A41D22484848
        1A0B30E3F1E5F0F0E3380B30DFA20A0647CA4F06063BADACA8A85C1800144844
        7A7F35F103D0696B679EC2320C3A4832090B55F0F0F0EFDD552B122EE6A20A06
        47C52D000E4FADAEA8A69B22000136447A807DD4D1706A6B679EC58E0E8EEA56
        061DBCF0EFEEE9B34D301A93F19909064EC42A001154ADAEA8A69C2E02002E44
        7A807984D1706A6B679EC4BD2A32C82E002CE5F0EBDADBB68C39A4F2EB990A06
        4FC42C001254ADAEA8A69C2C00002E447F80798703D16D6B679EBDC5551B8D1A
        064FEFF5C994B3B0BBE2F0EBEA9909064FC739000847ADAEA8A6982200063844
        7F797EE703EE716B649EC4C5B11F1B1114ADF0F0B2454DC9F8F0DFE3EA990809
        4FCA8E0E0B34A8AEA6A75D16001447447F8241EE03EE96656489C4C4C5553238
        48DFF1EFC6568CDFEFCB8EC6E7AB3A3293C6B94F2E385CA7A79A47222A3D9244
        7A4251F103EB99A1729EBEBDCD9C171548E4EAEBF28F21303921169AEA991614
        4FC4BEB84717162D39220F173A5C965E42595DF103EB96ADCFBABFBEC6BC564F
        B1E4E4EAEBE4AA49323A5CC5E5AB3D3B5CBDBEB9B15C392C242C3A4F95989896
        959559F103EB5B9BA6A8AAACADB8B9BEBEC4C4C6C5CACCC8C4C6C6C4BCBCB9BA
        ADACAEA89BA6A29A929597955B5B5C5D5C5D59EF030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303030303030303030303030303
        0303030303030303030303030303030303030303FFFFFFFFFFFF0000FFFFFFFF
        FFFF0000FFFFFFFFFFFF0000FFFFFFFFFFFF0000FFFFFFFFFFFF0000FFFFFFFF
        FFFF0000FFFFFFFFFFFF0000FFFFFFFFFFFF0000FFFFFFFFFFFF0000FFFFFFFF
        FFFF0000FFFFFFFFFFFF0000FFFFFFFFFFFF0000FFFFFFFFFFFF0000FFFFFFFF
        FFFF0000FFFFFFFFFFFF0000FFFFFFFFFFFF0000800000000000000080000000
        0000000080000000000000008000000000000000800000000000000080000000
        0000000080000000000000008000000000000000000000000000000000000000
        0000000080000000000000008000000000000000800000000000000080000000
        0000000080000000000000008000000000000000FFFFFFFFFFFF0000FFFFFFFF
        FFFF0000FFFFFFFFFFFF0000FFFFFFFFFFFF0000FFFFFFFFFFFF0000FFFFFFFF
        FFFF0000FFFFFFFFFFFF0000FFFFFFFFFFFF0000FFFFFFFFFFFF0000FFFFFFFF
        FFFF0000FFFFFFFFFFFF0000FFFFFFFFFFFF0000FFFFFFFFFFFF0000FFFFFFFF
        FFFF0000FFFFFFFFFFFF0000FFFFFFFFFFFF0000}
    end
    object LbHeading: TLabel
      Left = 43
      Top = 3
      Width = 178
      Height = 13
      Caption = 'New ASIO Driver Project Wizard'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LbSubHeading: TLabel
      Left = 43
      Top = 19
      Width = 248
      Height = 13
      Caption = 'Create a new project for developing a ASIO Drivers'
    end
  end
  object PnControl: TPanel
    Left = 0
    Top = 306
    Width = 432
    Height = 39
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Bevel2: TBevel
      Left = 0
      Top = 0
      Width = 432
      Height = 2
      Align = alTop
      Shape = bsBottomLine
    end
    object btnFinish: TButton
      Left = 350
      Top = 7
      Width = 75
      Height = 25
      Hint = 'This is Finish, but not the End'
      Anchors = [akTop, akRight]
      Caption = '&Finish'
      Default = True
      ModalResult = 1
      TabOrder = 0
      Visible = False
    end
    object btnCancel: TButton
      Left = 3
      Top = 7
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object btnNext: TButton
      Left = 350
      Top = 7
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Next'
      Default = True
      TabOrder = 1
      OnClick = btnNextClick
    end
    object btnPrev: TButton
      Left = 270
      Top = 7
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Back'
      Enabled = False
      TabOrder = 3
      OnClick = btnPrevClick
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 44
    Width = 432
    Height = 262
    ActivePage = TSWelcome
    Align = alClient
    TabOrder = 2
    object TSWelcome: TTabSheet
      Caption = 'Welcome'
      object LbWelcomeTitle: TLabel
        Left = 24
        Top = 13
        Width = 200
        Height = 13
        Caption = 'Welcome to the ASIO Driver Wizard'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LbWelcomeInstructions4: TLabel
        Left = 24
        Top = 136
        Width = 220
        Height = 13
        Caption = 'Click "Next" to start creating your ASIO driver'
      end
      object LbWelcomeInstructions3: TLabel
        Left = 24
        Top = 104
        Width = 351
        Height = 26
        Caption = 
          'Once compiled, your ASIO driver can be used by and suitable appl' +
          'ication compatible with the Steinberg ASIO SDK'
        WordWrap = True
      end
      object LbWelcomeInstructions2: TLabel
        Left = 24
        Top = 72
        Width = 383
        Height = 26
        Caption = 
          'An ASIO driver is a .DLL project that contains a TASIOModule des' +
          'cendant class (the driver code itself) and, optionally, a Contro' +
          'l Panel form.'
        WordWrap = True
      end
      object LbWelcomeInstructions1: TLabel
        Left = 24
        Top = 40
        Width = 369
        Height = 26
        Caption = 
          'This wizard will guide you through the process of creating a new' +
          ' ASIO driver project.'
        WordWrap = True
      end
    end
    object TSDestination: TTabSheet
      Caption = 'Dest'
      ImageIndex = 4
      object LbDestinationTitle: TLabel
        Left = 24
        Top = 13
        Width = 254
        Height = 13
        Caption = 'Select a Destination Folder and Project Name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LBDesinationSelect: TLabel
        Left = 24
        Top = 40
        Width = 310
        Height = 13
        Caption = 
          'Select the folder where you would like the project to be created' +
          ':'
      end
      object LBProjectName: TLabel
        Left = 24
        Top = 93
        Width = 141
        Height = 13
        Caption = 'Enter a name for the project:'
      end
      object LbDpr: TLabel
        Left = 229
        Top = 115
        Width = 20
        Height = 13
        Caption = '.dpr'
      end
      object edtProjectPath: TEdit
        Left = 24
        Top = 59
        Width = 321
        Height = 21
        TabOrder = 0
      end
      object edtProjectName: TEdit
        Left = 24
        Top = 112
        Width = 201
        Height = 21
        TabOrder = 2
      end
      object btnBrowse: TButton
        Left = 351
        Top = 59
        Width = 65
        Height = 21
        Caption = 'Browse'
        TabOrder = 1
        OnClick = btnBrowseClick
      end
      object chkSaveWhenFinished: TCheckBox
        Left = 24
        Top = 152
        Width = 113
        Height = 17
        Caption = 'Save when finished'
        Checked = True
        State = cbChecked
        TabOrder = 3
        Visible = False
      end
    end
    object TSModule: TTabSheet
      Caption = 'Module'
      ImageIndex = 5
      object LbModuleTitle: TLabel
        Left = 24
        Top = 13
        Width = 167
        Height = 13
        Caption = 'Add a VSTModule Descendant'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LbModuleInstructions: TLabel
        Left = 24
        Top = 40
        Width = 376
        Height = 26
        Caption = 
          'Each ASIO driver needs to contain a ASIOModule descendant class.' +
          ' This class provides the basic interface for the ASIO driver.'
        WordWrap = True
      end
      object LbModuleName: TLabel
        Left = 24
        Top = 85
        Width = 291
        Height = 13
        Caption = 'Please enter a name for your ASIOModule descendant class:'
      end
      object LbModuleUnit: TLabel
        Left = 24
        Top = 139
        Width = 257
        Height = 13
        Caption = 'Please enter a name for the unit containing this class:'
      end
      object LbPas: TLabel
        Left = 303
        Top = 161
        Width = 21
        Height = 13
        Caption = '.pas'
      end
      object edtDriverFormName: TEdit
        Left = 24
        Top = 104
        Width = 321
        Height = 21
        TabOrder = 0
      end
      object edtDriverUnitName: TEdit
        Left = 24
        Top = 158
        Width = 273
        Height = 21
        TabOrder = 1
      end
    end
    object TSControlPanel: TTabSheet
      Caption = 'Control Panel'
      ImageIndex = 3
      object LbGUIFormTitle: TLabel
        Left = 24
        Top = 13
        Width = 142
        Height = 13
        Caption = 'Add a Control Panel Form'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LbGUIFormInstructions: TLabel
        Left = 24
        Top = 40
        Width = 392
        Height = 26
        Caption = 
          'An ASIO drivermay optionally include a control panel form to ena' +
          'ble the end-user to manipulate the settings of the driver.'
        WordWrap = True
      end
      object pnlEditorDetails: TPanel
        Left = 0
        Top = 106
        Width = 433
        Height = 105
        BevelOuter = bvNone
        TabOrder = 1
        object LbPasDfm: TLabel
          Left = 300
          Top = 79
          Width = 47
          Height = 13
          Caption = '.pas/.dfm'
        end
        object LbGUIFormUnit: TLabel
          Left = 22
          Top = 56
          Width = 255
          Height = 13
          Caption = 'Please enter a name for the unit containing the form:'
        end
        object lblEditorFormName: TLabel
          Left = 22
          Top = 3
          Width = 228
          Height = 13
          Caption = 'Please enter a name for the control panel form:'
        end
        object edtControlPanelUnitName: TEdit
          Left = 22
          Top = 75
          Width = 273
          Height = 21
          TabOrder = 1
        end
        object edtControlPanelFormName: TEdit
          Left = 22
          Top = 22
          Width = 321
          Height = 21
          TabOrder = 0
        end
      end
      object chkUseEditor: TCheckBox
        Left = 24
        Top = 83
        Width = 209
        Height = 17
        Caption = 'Include a Control Panel form in the project'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = chkUseEditorClick
      end
    end
    object TSNames: TTabSheet
      Caption = 'Names && GUID'
      ImageIndex = 6
      object LbNameTitle: TLabel
        Left = 24
        Top = 13
        Width = 140
        Height = 13
        Caption = 'Naming Your ASIO Driver'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LbNameInstructions: TLabel
        Left = 24
        Top = 40
        Width = 384
        Height = 26
        Caption = 
          'Choose a name for your ASIO Driver. Most host applications will ' +
          'generally make this name visible to the end-user.'
        WordWrap = True
      end
      object LbAsioDriverName: TLabel
        Left = 24
        Top = 79
        Width = 197
        Height = 13
        Caption = 'Please enter a name for the ASIO driver:'
      end
      object LbGUID: TLabel
        Left = 24
        Top = 177
        Width = 352
        Height = 13
        Caption = 
          'Please enter a unique GUID for the ASIO driver (leave blank to g' +
          'enerate)'
      end
      object Label1: TLabel
        Left = 24
        Top = 133
        Width = 370
        Height = 26
        Caption = 
          'To identify the ASIO driver a worldwide unique identifier is nec' +
          'essary. This is provided by the COM interface with a so called G' +
          'UID'
        WordWrap = True
      end
      object edtDriverName: TEdit
        Left = 24
        Top = 98
        Width = 352
        Height = 21
        TabOrder = 0
      end
      object edtGUID: TEdit
        Left = 24
        Top = 196
        Width = 352
        Height = 21
        TabOrder = 1
      end
    end
    object TSFinish: TTabSheet
      Caption = 'Finish'
      ImageIndex = 2
      object LbDone: TLabel
        Left = 24
        Top = 13
        Width = 32
        Height = 13
        Caption = 'Done!'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LbDoneInstruction: TLabel
        Left = 24
        Top = 40
        Width = 383
        Height = 26
        Caption = 
          'The Wizard is now ready to create your VST plugin project with t' +
          'he options you have selected.'
        WordWrap = True
      end
      object LbClickFinish: TLabel
        Left = 24
        Top = 95
        Width = 278
        Height = 13
        Caption = 'Click the "Finish" button to create your VST plugin project.'
      end
    end
  end
end
