{**********************************************************************
 Package pl_ExGeographic
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit flagcustomunit;
{

  This file is part of the TFlagCompontPackage
  Copyright (C) 2004- Seppo S Finland
  Licence: modifiedLGPL (Same as FreePascal)

}


interface
uses
  Controls,Graphics,Classes,LCLProc,LCLtype, types, ExtGraphics, graphmath,
  flagcomponentbaseunit,flagtype;


type

  { TFlag }

  TCustomFlag = class(TFlagBase)//(TGraphicControl)
  private

    FFlag: TFlagType;
//    Xleft,Yup,XRight,Ylow:integer;

    procedure DiagonalFlags;
    procedure SetFlag(Value: TFlagType);
    procedure OneColorFlag;
    procedure BiColorFlag;
    procedure TriColorHorizontal;
    procedure TriColorVertical;
    procedure BasicColorsFlags;
    procedure CrossFlag;
    procedure CoupedCrossFlag;
    procedure SpecialCrossFlag;
    procedure UnionJackFlags;
    procedure UnionJack(X1,Y1,X2,Y2:integer);
    procedure EU_Flag;
    procedure ChequeredFlags;
    procedure StripeFlags;
    procedure TriagleFlags;
    procedure BallFlags;
    procedure ShapeFlags;
    procedure StarFlags;
    procedure StarsAndStripeFlags;
    procedure CrescentFlags;
    procedure XSaltiresFlags;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure StyleChanged(Sender: TObject);
  published

  property Flag: TFlagType read FFLag write SetFlag;
  end;


implementation

const ColorskBlue=clNavy;// R=0 G=56 B=168

constructor TCustomFlag.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SetInitialBounds(0,0,99,66);
  ControlStyle := ControlStyle + [csReplicatable];
end;

destructor TCustomFlag.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomFlag.StyleChanged(Sender: TObject);
begin
  If (Parent <> nil) and Visible and Parent.HandleAllocated
  then
    Invalidate;
end;

procedure TCustomFlag.SetFlag(Value: TFlagType);
begin
  if FFlag <> Value then
  begin
    FFlag := Value;
    StyleChanged(Self);
  end;
end;

procedure TCustomFlag.Paint;
var
  PaintRect : TRect;
begin
   PaintRect := Rect(0,0,Width - 1, Height - 1);
   With PaintRect do begin
      Xright:=Right;
      XLeft:=Left;
      YLow:= Bottom;
      YUp:= Top;
    end;
  case FFlag of
    flagLibya,flagWhiteFlag,flagBlackFlag,flagBlueFlag,flagGreenFlag,
    flagHinduSindhi,flagLightBlueFlag,flagRedFlag,flagYellowFlag  :OneColorFlag;

    flagAssen,flagBavaria,flagCashoubs,flagFribourg,flagHabsburg,flagHague,
    flagHaiti,flagHoogeveen,flagHuancayo, flagIndonesia,flagKingdomTalossa,
    flagLuzern,flagMauren,flagMonaco,flagMoravia,flagNijmegen,
    flagPahang,flagParis,flagPerlis,flagPoland,
    flagRoermond,flagRome,flagSanMarino,flagSaxony,flagSneek,
    flagSolothurn,flagSteiermark,flagSwalmen,
    flagTicino,flagTriesen,flagTriesenberg,
    flagUkraine,flagWarsaw:BiColorFlag;

    flagAalsmeer,flagArgentina,flagAustria,flagArmenia,flagAtlantium,
    flagBandung,flagBelgium1789,flagBismarck,flagBorne,flagBrawandi,
    flagBudapest,flagBulgaria,flagBusoga,
    flagDagestan,flagDelft,flagDuchyNassau1806,
    flagEnschede,flagErzya,flagEstonia,flagGabon,flagGarifuna,flagGelderland,
    flagGerman,flagGroningenCity,flagGuainia,
    flagHelgoland,flagHuila,flagHungary,flagIpiales,flagJewishMerchant1453,
    flagKaliningrad,flagKareliaRussia,flagKarnten,flagKingdomRedonda,flagKomia,
    flagKuyavianPomerania,
    flagLatinMerchant1453,flagLatvia,flagLisse,flagLivonians,flagLithuania,
    flagluxemburg,
    flagMaldivesWarFlag,flagManizales,flagMeghalaya,flagMisiones,flagMoresnet,
    flagMunster,flagMuslimMerchant1453,
    flagNetherlands,flagNorthFrisia,flagNorthHolland,flagNorthRhineWestphalia,
    flagPanAfrican,flagPerak,flagPosen,flagPrinsvlag,
    flagRomanMerchant1453,flagRotterdam,flagRussia,
    flagSariego,flagSchleswigHolstein,flagSierraLeone,flagSpain,flagSpectre,
    flagSouthOssetia,flagSuchaBeskidzka,flagTorneovalley,flagTransdniestr,
    flagUnitedSpace,flagUral,flagVillavicencio,
    flagWestPrussia,flagYemen,flagZanzibar,flagZug:TriColorHorizontal;

    flagBelgium,flagCanaryIslands,flagChad,flagFrance, flagGuinea, flagIreland,
    flagItaly,flagIvoryCoast,flagMali,flagMars,flagNewfoundland,flagNigeria,
    flagPeru,flagRomania:TriColorVertical;

    flagAbuDhabi,flagAltai,flagAncapflag,flagAnfemflag,
    flagAnnam,flagAppingedam,flagAustAgder,
    flagBanskobystricky,flagBar,flagBenin,flagBilbao,flagBlackWhiteFlag,
    flagBotswana,flagBratislavsky,flagBremen,
    flagCatalonia,flagChagossians,flagColombia,flagColombiaNativePeoples,
    flagCostaRica,flagCrimea,flagCusco,
    flagDubai,flagDuiven,flagEdamVolendam,flagEindhoven,flagElBierzo,flagElOro,
    flagElx,flagEsztergom,
    flagGambia,flagGeldermalsen,flagGreenBlackFlag,
    flagGrenzmarkPosenWestPrussian,
    flagHarenskarpel,flagHeemstede,flagHeerde,flagHuanuco,
    flagKingdomTahiti,flagKosicky,flagKuwait,
    flagLaarbeek,flagLeeuwarden,flagLemsterland,flagLodzkie,
    flagMaasbree,flagMadagascar,flagManipur,flagMauritius,
    flagMecklenburgWesternPomerania,flagMeijel,flagMiccosoukee,
    flagMillingenAanDeRijn,
    flagNapo,flagNitriansky,flagNorthernNigeria,flagNorthumberland,flagNuenen,
    flagOcana,flagOldenburgCity,flagOpsterland,flagOss,
    flagPanSiberian,flagPastaza,flagPotosi,flagPresovsky,
    flagQuito,flagRajashtan,flagRedBlackFlag,flagReeuwijk,
    flagSantiagoDeCali,flagSharjah,flagSchellenberg,flagSchiedam,
    flagSchiermonnikoog,flagSlaskie,flagSverdlovsk,
    flagTallinn,flagTatarstan,flagTerAar,flagTetovo,flagThailand,flagTholen,
    flagTrenciansky,
    flagUnitedArabEmirates,flagUtrechtCity,flagUttarPradesh,
    flagVaduz,flagValkenburg,
    flagWestvoorne,flagWinterswijk,flagWojewodztwoPodlaskie,flagWoudenberg,
    flagWriezen,flagZachPomorskie,flagZilinsky,flagZurich:BasicColorsFlags;

    flagAalandIslands,flagBornholm,flagDenmark,flagDevon,flagDurham,
    flagEastKarelia,flagEngland,flagFaroeIslands,flagFinland,flagHighlands,
    flagIceland,flagIngria,flagKalmarUnion,flagMizoramIndia,
    flagNordicCelticFlag,flagNormandy,flagNorway,
    flagOldenburg,flagOrkney,flagPula,flagSaintDavid, flagSaintPiran,
    flagScanianCrossFlag,flagShetland,flagSmaland,flagSouthUist,flagSweden,
    flagVepsia,flagZomi,flagZwolle:CrossFlag;

    flagRedCross, flagSwitzerland, flagTonga:CoupedCrossFlag;

    flagAlabama,flagAtica,flagBaarn,flagBasque,flagBedum,flagBurgers,
    flagChristian,flagDominicanRepublic,flagGreece,flagGroningen,flagGuernsey,
    flagIndianapolis,flagIFRCflag,flagKatwijk,flagLevis,flagNeuchatel,flagSaar,
    flagSaguenay,flagSaintAlban,flagSchwyz,flagScillonianCross,flagScotland,
    flagTenerife,flagTracia,flagUtrecht,
    flagValdivia,flagVolyn,flagWallisAndFutuna:SpecialCrossFlag;

    flagAbejorral,flagAmbtMontfoort,flagArabRevolt,flagArapaho,
    flagBahamas, flagBahia,flagBahrain,
    flagChukotka,flagCongo,flagCzech,flagDruzepeople,flagJamaica,
    flagKashmirIndependent,flagKhabarovskKrai,flagMeerloWanssum,flagMerida,
    flagMuiden,flagNataliaRepublic,flagOxapampa,
    flagPalestine,flagPasto,flagQatar,flagRijnwaarden,
    flagSaintLucia,flagSaintMartin,flagSealand,flagSeychelles,
    flagSognOgFjordane,flagSudan,
    flagTanzania,flagTxita,flagYamagata:TriagleFlags;

    flagBarotseland,flagBatasuna,flagBattleFlagOfTheUSConfederacy,
    flagConfederateNationalFlagSince1865,flagEntreRios,
    flagGalicia,flagMississippi,flagMonterey,
    flagNorthernSchleswigGermanMinority,flagOmmen,flagPara,
    flagRheden,flagRoraima,flagSomiedo,flagStrasbourg,
    flagTrinidadAndTobago,flagVillaDeLeyva,flagVoortrekker:DiagonalFlags;
    
    flagAmstelveen,flagAmsterdam,flagBreda,flagBreukelen: XSaltiresFlags;
    
    flagAboriginal, flagBangladesh, flagBlackFlagWithOrangeCircle,
    flagChin,flagColorado,flagDukeOfCornwall, flagGreenland, flagHiroshima,
    flagInNizam,flagJapan, flagLaos, flagMacedonia,flagMaori,flagMizoram,
    flagNagano,flagNuevaEsparta,flagNiger,flagOceanCity,flagOkinawa,flagOsaka,
    flagPalau,FlagPalmyraAtoll,flagSakha,flagSami:BallFlags;

    flagComoros,flagDoesburg,flagFrenchTunisia,flagIraqiTurkmen,
    flagMakran,flagMalacca,flagMaldives,flagMauritania,flagMikmaq,
    flagObdam,flagPakistan,flagRedCrescent,
    flagTunisia,flagTurkey,flagTurkishRepublicOfNorthernCyprus,
    flagUmmAlQaiwain,flagUzbekistan,flagWesternSahara: CrescentFlags;
     
    flagAcardia,flagAcre,flagAlaska,flagAntiguaAndBarbuda,flagAruba,flagAtacama,
    flagAzerbaijan,flagBlueEstelada,flagBurkinaFaso, flagBurundi,
    flagCameroon,flagCapeVerde,flagCentralAfricanRepublic,
    flagChicago,flagChile,flagChina,flagChittagongHillTracts,flagCongoKinshasa,
    flagCuba,flagCuracao,
    flagDenison,flagDjibouti,flagDurhamNC,flagEsperanto,flagEwePeople,
    flagGagauzia,flagGambierIslands,flagGhana,flagGoias,flagGuayaquil,
    flagGuineaBissau,
    flagHonduras,flagJordan,flagLipkovo,flagMaastricht,flagMadrid,flagMaranhao,
    flagMicronesia,flagMilneBay,flagMonteria,flagNamibia,flagNauru,
    flagNetherlandsAntilles,flagNorthKorea,flagOgaden,flagPanama,flagPuertoRico,
    flagRedArmy,flagRedEstelada,flagRondonia,flagRwanda,
    flagSamoa,flagSaoTomeAndPrincipe,flagSenegal,flagSergipe,flagSFRYugoslavia,
    flagSolomonIslands,flagSomalia,flagSuriname,flagSyria,
    flagTaiwan,flagTenessee,flagTexas, flagTogo,
    flagVenezuela,flagVietnam,flagVojvodina,
    flagWashingtonDC,flagWestSomalia:StarFlags;

    flagAustralia,flagBlueEnsign,flagCookIslands,flagEnsignRoyalAirForce,
    flagHawaii,flagNewZealand,flagNiue,flagRedEnsign,flagRossDependency,
    flagTuvalu,flagUnitedKingdom,flagWhiteEnsign:UnionJackFlags;

    flagEuropeanUnion:EU_Flag;
    flagUSA:StarsAndStripeFlags;

    flagBlackWhiteChequeredFlag:ChequeredFlags;
    flagGoes,flagPiaui,flagSenyera,flagTrnavsky,
    flagWestPapua,flagYellowFlagWithRedStripes:StripeFlags;

    flagArizona,flagBarbados,flagBosniaHerzegovina,flagCanadian,flagDamietta,
    flagDelta,flagDenver,flagEthiopia,flagIndia,flagIsrael,flagJollyRoger,
    flagLombardy,flagMadeira,flagMariEl,flagMatoGrosso,flagMorocco,
    flagNagornoKarabakh,flagNato,flagNorthernCheyenne,flagPadania,
    flagPembrokeshire,flagPiemonte,flagRedCrystal,flagRedShieldOfDavid,flagSaba,
    flagSaintVincentAndTheGrenadines,flagSingapore,flagSlovakia,flagSouthAfrica,
    flagSouthKorea,flagTokyo,flagToronto,flagVenda,flagYamaguchiKen:ShapeFlags;
  end;
end;




procedure TCustomFlag.OneColorFlag;
var PaintColor:TColor;
begin
  RightRatioSize(3,2);
  case FFlag of
    flagLibya:PaintColor:=clGreen;
    flagWhiteFlag:PaintColor:=clWhite;
    flagBlackFlag:PaintColor:=clBlack;
    flagBlueFlag:PaintColor:=clBlue;
    flagGreenFlag:PaintColor:=clLime;
    flagHinduSindhi:PaintColor:=TColor($0080FF);
    flagLightBlueFlag:PaintColor:=clAqua;
    flagRedFlag:PaintColor:=clRed;
    flagYellowFlag:PaintColor:=clYellow;
  end;
  Rectangular(Xleft,Yup,XRight,Ylow,PaintColor);
end;
procedure TCustomFlag.BiColorFlag;
begin
  case FFlag of
    flagMonaco:RightRatioSize(5,4);
    flagPoland,flagWarsaw:RightRatioSize(8,5);
    flagMauren,flagTriesen,flagTriesenberg: RightRatioSize(5,3);
    flagFribourg,flagLuzern,flagParis,flagSolothurn,
      flagTicino: RightRatioSize(1,1);
    flagPahang,flagPerlis:RightRatioSize(2,1);
    flagSteiermark:RightRatioSize(450,300);
    else RightRatioSize(3,2);
  end;
  case FFlag of
    flagAssen     :BiColorFlagHorizontal(TColor($8c4821),clWhite);
    flagBavaria   :BiColorFlagHorizontal(clWhite,TColor($FF8000));
    flagCashoubs  :BiColorFlagHorizontal(clYellow,clBlack);
    flagFribourg  :BiColorFlagHorizontal(clBlack,clWhite);
    flagHaiti     :BiColorFlagHorizontal(clBlue, clRed);
    flagHabsburg  :BiColorFlagHorizontal(clBlack, clYellow);
    flagHague     :BiColorFlagHorizontal(clYellow,clGreen);
    flagHoogeveen :BiColorFlagHorizontal(clBlue,clWhite);
    flagHuancayo  :BiColorFlagVertical(TColor($a0a000), clWhite);
    flagIndonesia :BiColorFlagHorizontal(clRed, clWhite);
    flagKingdomTalossa:BiColorFlagHorizontal(TColor($195D09), TColor($100bE6));
    flagLuzern    :BiColorFlagHorizontal(clWhite,clBlue);
    flagMauren    :BiColorFlagHorizontal(clBlack,  TColor($16D0FC));
    flagMonaco    :BiColorFlagHorizontal(clRed, clWhite);
    flagMoravia   :BiColorFlagHorizontal(clYellow,clRed);
    flagNijmegen  :BiColorFlagHorizontal(clBlack,clRed);
    flagPahang    :BiColorFlagHorizontal(clWhite,clBlack);
    flagParis     :BiColorFlagVertical(clBlue, clRed);
    flagPerlis    :BiColorFlagHorizontal(TColor($00C0ff), clNavy);
    flagPoland    :BiColorFlagHorizontal(clWhite, clRed);
    flagRoermond  :BiColorFlagHorizontal(clWhite,TColor($8c4821));
    flagRome      :BiColorFlagVertical(clMaroon, TColor($00C0ff));
    flagSanMarino :BiColorFlagHorizontal(clWhite,clBlue);
    flagSaxony    :BiColorFlagHorizontal(clWhite,clGreen);
    flagSneek     :BiColorFlagVertical(clBlack, clYellow);
    flagSolothurn :BiColorFlagHorizontal(clRed, clWhite);
    flagSteiermark:BiColorFlagHorizontal(clWhite,TColor($006600));
    flagSwalmen   :BiColorFlagHorizontal(TColor($00f000),clRed);
    flagTicino    :BiColorFlagHorizontal(clRed,clBlue);
    flagTriesen   :BiColorFlagVertical(TColor($800000),clWhite);
    flagTriesenberg:BiColorFlagHorizontal(TColor($800000), TColor($16D0FC));
    flagUkraine   :BiColorFlagHorizontal(clBlue, clYellow);
    flagWarsaw    :BiColorFlagHorizontal(TColor($00d6ff), TColor($0011FF));
  end;
end;
procedure TCustomFlag.TriColorHorizontal;
begin
  case FFlag of
    flagBulgaria, flagluxemburg,flagMaldivesWarFlag,flagMisiones,flagMunster,
    flagNorthRhineWestphalia, flagGerman: RightRatioSize(5,3);
    flagArgentina, flagBandung,flagDagestan,flagKaliningrad,flagKomia,
    flagLatvia,flagLivonians,flagLithuania,flagPerak,flagSouthOssetia,
    flagTransdniestr: RightRatioSize(2,1);
    flagBudapest,flagZug:RightRatioSize(1,1);
    flagMeghalaya,flagRotterdam:RightRatioSize(450,300);
    else RightRatioSize(3,2);
  end;
  case FFlag of
    flagAalsmeer:TriColorFlagHorizontal(clRed, TColor($008000),clBlack);
    flagArgentina:TriColorFlagHorizontal(TColor($CDC123),clWhite,TColor($CDC123));
    flagArmenia:  TriColorFlagHorizontal(clRed, clBlue,TColor($00C0ff));{Orange}
    flagAtlantium:TriColorFlagHorizontal(clAqua,clYellow,TColor($00C0ff));
    flagAustria:    TriColorFlagHorizontal(clRed,   clWhite, clRed);
    flagBandung:TriColorFlagHorizontal( TColor($009a00),TColor($00fffe),
                 TColor($fe9a00));
    flagBelgium1789:TriColorFlagHorizontal(clRed,   clYellow,clBlack);
    flagBismarck:   TriColorFlagHorizontal(clBlack, clWhite, clRed);
    flagBorne:FiveColorFlag(clBlue,clBlue,clYellow,clBlue,clBlue);
    flagBrawandi:TriColorFlagHorizontal(clRed,TColor($00C0ff){Orange},clYellow);
    flagBudapest:TriColorFlagHorizontal(clRed,clYellow,TColor($ff006b));
    flagBulgaria:   TriColorFlagHorizontal(clWhite, clGreen, clRed);
    flagBusoga:   TriColorFlagHorizontal(clBlue, clYellow, clMaroon);
    flagDagestan:   TriColorFlagHorizontal(clGreen,TColor($ff9B37), clRed);
    flagDelft:      TriColorFlagHorizontal(clWhite,  clBlack, clWhite);
    flagDuchyNassau1806:
                 TriColorFlagHorizontal(TColor($0090f0),clBlue,TColor($0090f0));
    flagEnschede:   TriColorFlagHorizontal(clWhite,  clRed, clWhite);
    flagErzya:      TriColorFlagHorizontal(clWhite,  clRed, clBlack);
    flagEstonia:    TriColorFlagHorizontal(clBlue,  clBlack, clWhite);
    flagGabon:      TriColorFlagHorizontal(clGreen, clYellow,clBlue);
    flagGarifuna:   TriColorFlagHorizontal(clYellow,clWhite, clBlack);
    flagGelderland: TriColorFlagHorizontal(clBlue, clYellow, clBlack);
    flagGerman:     TriColorFlagHorizontal(clBlack, clRed,   clYellow);
    flagGroningenCity:TriColorFlagHorizontal(clWhite,TColor($459500),clWhite);
    flagGuainia:    TriColorFlagHorizontal(clYellow, clBlue, clGreen);
    flagHelgoland:TriColorFlagHorizontal(TColor($008000),TColor($0000cc),clWhite);
    flagHuila:      TriColorFlagHorizontal(clWhite,  clGreen,clYellow);
    flagHungary:    TriColorFlagHorizontal(clRed,   clWhite, clGreen);
    flagIpiales:    TriColorFlagHorizontal(clRed,   clGreen, clWhite);
    flagJewishMerchant1453:TriColorFlagHorizontal(clRed,clYellow,clRed);
    flagKaliningrad:TriColorFlagHorizontal(clGreen,   clWhite, clBlue);
    flagKareliaRussia:TriColorFlagHorizontal(clRed,TColor($FF8000), clGreen);
    flagKarnten:TriColorFlagHorizontal(TColor($00ccff),clRed,clWhite);
    flagKingdomRedonda:TriColorFlagHorizontal(
                        TColor($fda658),TColor($f01731c),TColor($f18436f));
    flagKomia:      TriColorFlagHorizontal(clBlue,  clGreen, clWhite);
    flagKuyavianPomerania:QuadColorFlag(clBlack,clWhite,clWhite,clRed);
    flagLatinMerchant1453:TriColorFlagHorizontal(clRed,clBlack,clRed);
    flagLatvia:FiveColorFlag(clMaroon,clMaroon,clWhite,clMaroon,clMaroon);
    flagLisse:TriColorFlagHorizontal(clYellow,clBlue,clYellow);
    flagLivonians:FiveColorFlag(clGreen,clGreen,clWhite,clBlue,clBlue);
    flagLithuania:  TriColorFlagHorizontal(clYellow,clGreen, clRed);
    flagLuxemburg:  TriColorFlagHorizontal(clRed,   clWhite, clBlue);
    flagMaldivesWarFlag:TriColorFlagHorizontal(clNavy,clWhite,clGreen);
    flagManizales:  TriColorFlagHorizontal(clWhite,  clGreen,clRed);
    flagMeghalaya: TriColorFlagHorizontal(TColor($0014e4),
                                TColor($fae61f),TColor($11eff7));
    flagMisiones: TriColorFlagHorizontal(TColor($1d25da),
                                TColor($b7001b),TColor($fafafa));
    flagMoresnet:   TriColorFlagHorizontal(clBlack, clWhite,  clBlue);
    flagMunster:TriColorFlagHorizontal(TColor($00f2ff),TColor($3340e3),clWhite);
    flagMuslimMerchant1453:TriColorFlagHorizontal(clRed,TColor($008000),clRed);
    flagNetherlands:TriColorFlagHorizontal(clRed,   clWhite, clBlue);
    flagNorthFrisia:TriColorFlagHorizontal(TColor($00d0ff),clRed,TColor($ff2d1c));
    flagNorthHolland:TriColorFlagHorizontal(clYellow,clRed,  clBlue);
    flagNorthRhineWestphalia:TriColorFlagHorizontal(clGreen, clWhite,clRed);
    flagPanAfrican: TriColorFlagHorizontal(clRed,clBlack, clGreen);
    flagPerak:      TriColorFlagHorizontal(clWhite,clYellow,clBlack);
    flagPosen:      TriColorFlagHorizontal(clWhite,clBlack,clWhite);
    flagPrinsvlag:TriColorFlagHorizontal(TColor($00C0ff),clWhite,clBlue);
    flagRomanMerchant1453:TriColorFlagHorizontal(clRed,clBlue,clRed);
    flagRotterdam:  TriColorFlagHorizontal(clGreen, clWhite,clGreen);
    flagRussia:     TriColorFlagHorizontal(clWhite, clBlue,  clRed);
    flagSariego    :TriColorFlagHorizontal(clGreen, clYellow, clBlue);
    flagSchleswigHolstein:TriColorFlagHorizontal(TColor($ad3900),clWhite,clRed);
    flagSierraLeone:TriColorFlagHorizontal(TColor($0071CF40),clWhite, clAqua);
    flagSpain:      QuadColorFlag(clRed,clYellow,clYellow,clRed);
    flagSpectre:    TriColorFlagHorizontal(clYellow,clBlack,clYellow);
    flagSouthOssetia:TriColorFlagHorizontal(clWhite,clRed,clYellow);
    flagSuchaBeskidzka:TriColorFlagHorizontal(clWhite,clRed,TColor($3f9200));
    flagTorneovalley:TriColorFlagHorizontal(clWhite, clBlue,clYellow);
    flagTransdniestr:FiveColorFlag(clRed,clRed,clGreen,clRed,clRed);
    flagUnitedSpace:TriColorFlagHorizontal(TColor($660000),clWhite,TColor($660000));
    flagUral:       TriColorFlagHorizontal(clBlack,clYellow,clGreen);
    flagVillavicencio:TriColorFlagHorizontal(clBlue,clGreen,clRed);
    flagWestPrussia:TriColorFlagHorizontal(clBlack,   clWhite, clBlack);
    flagYemen:      TriColorFlagHorizontal(clRed,   clWhite, clBlack);
    flagZanzibar:   TriColorFlagHorizontal(clNavy,clBlack,clGreen);
    flagZug:        TriColorFlagHorizontal(clWhite, clBlue, clWhite);
  end;
end;
procedure TCustomFlag.TriColorVertical;
begin
  case FFlag of
    flagBelgium:RightRatioSize(15,13);
    flagNewfoundland:RightRatioSize(2,1);
    else RightRatioSize(3,2);
  end;
  case FFlag of
    flagBelgium: TriColorFlagVertical(clBlack,clYellow,clRed);
    flagCanaryIslands: TriColorFlagVertical(clWhite,clBlue, clYellow);
    flagChad   : TriColorFlagVertical(clBlue, clYellow,clRed);
    flagFrance : TriColorFlagVertical(clNavy, clWhite, clRed);
    flagGuinea : TriColorFlagVertical(clRed,  clYellow,clGreen);
    flagIreland: TriColorFlagVertical(clGreen,clWhite,TColor($00C0ff){Orange});
    flagItaly  : TriColorFlagVertical(clGreen,clWhite, clRed);
    flagIvoryCoast:TriColorFlagVertical(TColor($00C0ff),clWhite, clGreen);
    flagNewfoundland:
                 TriColorFlagVertical(TColor($00Cc01),clWhite,TColor($cd9cfe));
    flagNigeria: TriColorFlagVertical(clGreen,clWhite, clGreen);
    flagMali   : TriColorFlagVertical(clGreen,clYellow,clRed);
    flagMars   : TriColorFlagVertical(clRed, clLime, clBlue);
    flagPeru   : TriColorFlagVertical(clRed,  clWhite, clRed);
    flagRomania: TriColorFlagVertical(clBlue, clYellow,clRed);
  end;
end;

procedure TCustomFlag.BasicColorsFlags;
var i,j,x:integer;
begin
  case FFlag of
    flagAnnam,flagZurich:RightRatioSize(1,1);
    flagBotswana,flagLodzkie:RightRatioSize(8,5);
    flagCostaRica,flagOldenburgCity,flagSchellenberg,
    flagVaduz,flagWriezen:RightRatioSize(5,3);
    flagAbuDhabi,flagBar,flagChagossians,flagCrimea,flagDubai,flagElOro,
    flagKuwait,flagMecklenburgWesternPomerania,flagSharjah,flagTallinn,
    flagTatarstan,flagTetovo,flagUnitedArabEmirates:RightRatioSize(2,1);
    flagAustAgder:RightRatioSize(375,300);
    flagAltai,flagRajashtan,flagUttarPradesh:RightRatioSize(450,300);
    flagNorthumberland:RightRatioSize(24,15);
    flagPotosi:RightRatioSize(110,75);
    else RightRatioSize(3,2);
  end;
  case FFlag of
    flagAbuDhabi:begin
        Rectangular(Xleft,YUp,XRight,Ylow,TColor($0000bf));
        Rectangular(Xleft,YUp,
          Xleft+(XRight-XLeft) div 3,YUp+(YLow-YUp)div 3,clWhite);
      end;
    flagAltai:begin
        Rectangular(Xleft,YUp,XRight,Ylow,clWhite);
        i:=YLow-YUp;
        Rectangular(Xleft,YUp+Muldiv(i,201,300),
          XRight,YUp+Muldiv(i,213,300),TColor($efae01));
        Rectangular(Xleft,YUp+Muldiv(i,225,300),XRight,YLow,TColor($efae01));
      end;
    flagAncapflag:TwoRightTriangleUpDownFlag(TColor($00fdfd),clBlack);
    flagAnfemflag:TwoRightTriangleUpDownFlag(TColor($ff42ff),clBlack);
    flagAnnam:begin
        x:=(XRight-XLeft) div 2;
        i:=(YLow-YUp) div 2;
        Rectangular(Xleft,YUp,XRight,Ylow,TColor($03f2f4));
        TriColorBasicVertical(clNavy, clWhite, clRed,XLeft,Yup,XLeft+x,YUp+i);
      end;
    flagAppingedam:begin
        BiColorFlagHorizontal(clYellow,clBlue);
        i:= (YLow-YUp)div 10;
        j:=YUp+(YLow-YUp)div 2;
        Rectangular(Xleft,j-i,XRight,j,TColor($2cc02c));
        Rectangular(Xleft,j,XRight,j+i,clRed);
      end;
    flagAustAgder:FiveColorFlag(clRed,clYellow,clRed,clYellow,clRed);
    flagBanskobystricky:QuadColor2SFlag(ColorskBlue,clWhite,clWhite,clRed);
    flagBar:begin
        TriColorFlagHorizontal(TColor($833029),TColor($5a7529),TColor($833029));
        i:=MulDiv(YLow-YUp,56,228);
        j:=MulDiv(YLow-YUp,86,228);
        Rectangular(Xleft,YUp+i,XRight,YUp+j,clYellow);
        Rectangular(Xleft,YLow-j,XRight,Ylow-i,clYellow);
      end;
    flagBenin:begin
        BiColorFlagHorizontal(clYellow,clRed);
        Rectangular(Xleft,Yup,((XRight-XLeft)div 3)+Xleft,YLow,clGreen);
      end;
    flagBilbao:begin
        Rectangular(Xleft,YUp,XRight,Ylow,clWhite);
        i:=(YLow-YUp) div 2;
        Rectangular(Xleft,YUp,Xleft+i,YUp+i,clRed);
      end;
    flagBlackWhiteFlag:TwoRightTriangleUpDownFlag(clBlack,clWhite);
    flagBotswana:begin
        Rectangular(Xleft,YUp,XRight,Ylow,TColor($00CDC123));
        Rectangular(Xleft, YUp +(YLow-YUp)div 20*7,
                    XRight,YLow-(YLow-YUp)div 20*7,clWhite);
        Rectangular(Xleft, YUp +(YLow-YUp)div 5*2 ,
                    XRight,YLow-(YLow-YUp)div 5*2,clBlack);
      end;
    flagBratislavsky:QuadColor2SFlag(clYellow,ColorskBlue,ColorskBlue,clWhite);
    flagBremen:begin
        EightColorFlag(clRed,clWhite,clRed,clWhite,clRed,clWhite,clRed,clWhite);
        j:=YLow-YUp;
        i:=j div 8;
        x:=XLeft+i;
        i:=x+i;
        Rectangular(x,yup,i,YUp+MulDiv(j,1,8),clWhite);
        Rectangular(x,YUp+MulDiv(j,1,8),i,YUp+MulDiv(j,2,8),clRed);
        Rectangular(x,YUp+MulDiv(j,2,8),i,YUp+MulDiv(j,3,8),clWhite);
        Rectangular(x,YUp+MulDiv(j,3,8),i,YUp+MulDiv(j,4,8),clRed);
        Rectangular(x,YUp+MulDiv(j,4,8),i,YUp+MulDiv(j,5,8),clWhite);
        Rectangular(x,YUp+MulDiv(j,5,8),i,YUp+MulDiv(j,6,8),clRed);
        Rectangular(x,YUp+MulDiv(j,6,8),i,YUp+MulDiv(j,7,8),clWhite);
        Rectangular(x,YUp+MulDiv(j,7,8),i,YLow,clRed);
      end;
    flagCatalonia:NineColorFlag(TColor($0ee8fb),TColor($0d00db),TColor($0ee8fb),
      TColor($0d00db),TColor($0ee8fb),TColor($0d00db),TColor($0ee8fb),
      TColor($0d00db),TColor($0ee8fb));
    flagChagossians:begin
        Rectangular(Xleft,YUp,XRight,Ylow,TColor($0060f0));
        i:=YLow-(YLow-YUp)div 4;
        Rectangular(Xleft, YUp +Muldiv(YLow-YUp,23,40),XRight,i,clBlack);
        Rectangular(Xleft, i, XRight,YLow,clBlue);
      end;
    flagColombia:QuadColorFlag(clYellow,clYellow,clBlue,clRed);
    flagColombiaNativePeoples: QuadColorFlag(clRed,clBlue,clWhite,clBlack);
    flagCostaRica:SixColorFlag(clBlue,clWhite,clRed,clRed,clWhite,clBlue);
    flagCrimea:SixColorFlag(TColor($b06f00),clWhite,clWhite,
        clWhite,clWhite,TColor($1d25da));
    flagCusco:SevenColorFlag(TColor($0000dd),TColor($0099ff),TColor($00ddff),
        TColor($66aa44),TColor($aa8833),TColor($994400),TColor($883366));
    flagDubai:begin
        i:=(XRight-XLeft) div 4;
        Rectangular(Xleft,YUp,XRight,Ylow,clWhite);
        Rectangular(Xleft+i,YUp,XRight,Ylow,TColor($0000bf));
      end;
    flagDuiven:begin
        BiColorFlagHorizontal(clWhite,clBlue);
        i:=xLeft+MulDiv(XRight-XLeft,150,450);
        Rectangular(Xleft,YUp,i,YUp +Muldiv(YLow-YUp,100,300),clRed);
        Rectangular(Xleft,YUp,i,YUp +Muldiv(YLow-YUp,50,300),clYellow);
      end;
    flagEdamVolendam:NineColorFlag(clRed,clRed,clRed,TColor($00c000),clWhite,
            TColor($3090ff),clBlack,clBlack,clBlack);
    flagEindhoven:begin
        FiveColorFlag(clRed,ClWhite,clRed,ClWhite,clRed);
        i:=MulDiv(XRight-XLeft,120,450);
        Rectangular(Xleft,YUp,Xleft+i,Ylow,clWhite);
        i:=MulDiv(XRight-XLeft,60,450);
        Rectangular(Xleft,YUp,Xleft+i,Ylow,clRed);
      end;
    flagElBierzo:TwoRightTriangleUpDownFlag(TColor($e4ffff),TColor($ef9c49));
    flagElOro:QuadColorFlagVertical(TColor($00a136),TColor($1cffff),
      TColor($1cffff),TColor($00a136));
    flagElx:begin
        NineColorFlag(TColor($0ee8fb),TColor($0d00db),TColor($0ee8fb),
           TColor($0d00db),TColor($0ee8fb),TColor($0d00db),TColor($0ee8fb),
           TColor($0d00db),TColor($0ee8fb));
        x:=XLeft + (XRight-XLeft) div 2;
        i:=YUp+(YLow-YUp) div 2;
        Rectangular(Xleft,YUp,x,i,TColor($282dd5));
        Rectangular(Xleft,i,x,YLow,TColor($904e00));
      end;
    flagEsztergom:NineColorFlag(clRed,clWhite,clRed,clWhite,clRed,clWhite,
           clRed,clWhite,clRed);
    flagGambia:begin
        TriColorFlagHorizontal(clRed,clWhite,clGreen);
        Rectangular(Xleft,YUp  +round((YLow-YUp)/ 60*24),
                    XRight,YLow-round((YLow-YUp)/ 60*24),clBlue);
      end;
    flagGeldermalsen:begin
        QuadColorFlag(clYellow,clRed,clYellow,clRed);
        i:=XLeft+MulDiv(XRight-XLeft,150,450);
        Rectangular(Xleft,YUp,i,YLow,clYellow);
        Rectangular(Xleft,YUp,i,YUp+(YLow-YUp) div 4,TColor($f86058));
        Rectangular(Xleft,YUp+(YLow-YUp) div 2,i,YLow-(YLow-YUp) div 4,clBlack);
      end;
    flagGreenBlackFlag:TwoRightTriangleUpDownFlag(clLime,clBlack);
    flagGrenzmarkPosenWestPrussian:begin
        BiColorFlagVertical(clBlack,clWhite);
        x:=(XRight-Xleft) div 2 + XLeft;
        i:=(YLow-YUp) div 3;
        Rectangular(Xleft,Yup+i,X,YLow-i,clWhite);
        Rectangular(X,Yup+i,XRight,YLow-i,clBlack);
      end;
    flagHarenskarpel:TwoPlusTwoColor(TColor($3030ff),TColor($ee3b32),
       TColor($ee3b32),TColor($3030ff));
    flagHeemstede:QuadColorFlag(clYellow,clRed,clYellow,clRed);
    flagHeerde:QuadColor2SFlag(TColor($f86058),clYellow,clWhite,clRed);
    flagHuanuco:
      TwoRightTriangleDownUp(Xleft,Yup,XRight,Ylow,TColor($ccffff),clGreen);
    flagKingdomTahiti:QuadColorFlag(TColor($2820ce),clWhite,
      clWhite,TColor($2820ce));
    flagKosicky:QuadColor2SFlag(clYellow,clRed,ColorskBlue,clYellow);
    flagKuwait:begin
        BiColorFlagHorizontal(clGreen,clRed);
        Triagle(XLeft,YUp,round(Xleft+(XRight-Xleft)/2.6),YLow,atRight,clBlack);
        Rectangular(Xleft+(XRight-XLeft)div 4,Yup+(YLow-YUp)div 3,
                    XRight,YLow-(YLow-YUp)div 3,clWhite);
      end;
    flagLaarbeek:begin
        TriColorFlagHorizontal(clBlue,clRed,TColor($00ce00));
        x:=YLow-YUp;
        j:=MulDiv(x,32,300);
        i:=YUp+MulDiv(x,82,300);
        Rectangular(Xleft,i,XRight,i+j,clWhite);
        i:=YUp+MulDiv(x,219,300);
        Rectangular(Xleft,i-j,XRight,i,clWhite);
    end;
    flagLeeuwarden:QuadColorFlag(clBlue,clYellow,clBlue,clYellow);
    flagLemsterland:SixColorFlag(clWhite,clRed,clYellow,clYellow,clRed,clWhite);
    flagLodzkie:FiveColorVertical(clRed,clYellow,clRed,clYellow,clRed);
    flagMaasbree:begin
        BiColorFlagVertical(TColor($ee3b23),TColor($3030ff));
        Rectangular(Xleft,Yup,XRight,YUp+(YLow-YUp) div 3,clYellow);
      end;
    flagMadagascar:begin
        BiColorFlagHorizontal(clRed,clGreen);
        Rectangular(Xleft,Yup,((XRight-XLeft)div 3)+Xleft,YLow,clWhite);
      end;
    flagManipur:QuadColorFlag(clYellow,clNavy,clRed,clGreen);
    flagMauritius:QuadColorFlag(clRed,clBlue,clYellow,clGreen);
    flagMecklenburgWesternPomerania:begin
        Rectangular(Xleft,YUp,XRight,Ylow,clWhite);
        i:=YLow-YUp;
        j:=MulDiv(i,120,450);
        Rectangular(Xleft,YUp,XRight,YUp+j,TColor($ff4d00));
        Rectangular(Xleft,YLow-j,XRight,YLow,clRed);
        j:=MulDiv(i,210,450);
        Rectangular(Xleft,YUp+j,XRight,YLow-j,clYellow);
      end;
    flagMeijel:begin
        BiColorFlagVertical(clBlue,clYellow);
        x:=XRight-Xleft;
        i:=Xleft+MulDiv(x,92,450);
        x:=Xleft+MulDiv(x,29,450);
        j:=(YLow-YUp) div 10;
        Rectangular(x,YUp+MulDiv(YLow-YUp,156,300),
          i,YUp+j+MulDiv(YLow-YUp,156,300),clYellow);
        Rectangular(x,YUp+MulDiv(YLow-YUp,201,300),
          i,YUp+j+MulDiv(YLow-YUp,201,300),clYellow);
        Rectangular(x,YUp+MulDiv(YLow-YUp,247,300),
          i,YUp+j+MulDiv(YLow-YUp,247,300),clYellow);
      end;
    flagMiccosoukee:QuadColorFlag(clWhite,clBlack,clRed,clYellow);
    flagMillingenAanDeRijn:QuadColorFlag(clYellow,clRed,clWhite,clBlack);
    flagNapo:SixColorFlag(TColor($26f8f8),TColor($26f8f8),TColor($26f8f8),
      TColor($f3fbfa),TColor($b32737),TColor($0022ef));
    flagNitriansky:QuadColor2SFlag(ColorskBlue,clYellow,clRed,ColorskBlue);
    flagNorthernNigeria:FiveColorFlag(clRed,clYellow,clBlack,clLime,clOlive);
    flagNorthumberland:begin
        Rectangular(Xleft,YUp,XRight,Ylow,clRed);
        x:=XRight-Xleft;
        i:=MulDiv(x,75,600);
        j:=MulDiv(YLow-YUp,150,375);
        Rectangular(Xleft,YUp,Xleft+i,YUp+j,clYellow);
        Rectangular(XRight,Ylow-j,XRight-i,Ylow,clYellow);
        x:=MulDiv(XRight-Xleft,150,600);
        Rectangular(Xleft+x,YUp,Xleft+x+i,YUp+j,clYellow);
        Rectangular(XRight-x,Ylow-j,XRight-x-i,Ylow,clYellow);
        x:=MulDiv(XRight-Xleft,300,600);
        Rectangular(Xleft+X,YUp,Xleft+X+i,YUp+j,clYellow);
        Rectangular(XRight-x,Ylow-j,XRight-x-i,Ylow,clYellow);
        x:=MulDiv(XRight-Xleft,450,600);
        Rectangular(Xleft+X,YUp,Xleft+X+i,YUp+j,clYellow);
        Rectangular(XRight-x,Ylow-j,XRight-x-i,Ylow,clYellow);
      end;
    flagNuenen:begin
        QuadColorFlag(clWhite,clBlack,clYellow,TColor($ff6000));
        Rectangular(Xleft,Yup,Xleft+(XRight-Xleft) div 6,YLow,clRed);
      end;
    flagOcana:TwoPlusTwoColor(clGreen,clWhite,clWhite,clGreen);
    flagOldenburgCity:FiveColorFlag(TColor($00ccff),TColor($2936e8),
       TColor($00ccff),TColor($2936e8),TColor($00ccff));
    flagOss:begin
        QuadColorFlag(clRed,clYellow,clWhite,TColor($ffce00));
        i := (XRight-Xleft) div 3;
        j := (YLow-YUp) div 2;
        Rectangular(Xleft,YUp,Xleft+i,YUp+j,clWhite);
        Rectangular(Xleft,Ylow-j,Xleft+i,Ylow,TColor($009c00));
      end;
    flagOpsterland:TwoPlusTwoColor(clRed,clWhite,clWhite,TColor($008000));
    flagPanSiberian:TwoRightTriangleDownUpFlag(TColor($008000),clWhite);
    flagPastaza:TwoPlusTwoColor(TColor($7e900e),TColor($2ff5f5),
       TColor($2ff5f5),TColor($7e900e));
    flagPotosi:TwoPlusTwoColor(clRed,clWhite,clWhite,clRed);
    flagPresovsky:SixColorFlag(clWhite,clWhite,clYellow,ColorskBlue,clRed,clRed);
    flagQuito:begin
        Rectangular(Xleft,YUp,XRight,Ylow,clRed);
        i:=(XRight-XLeft)div 6;
        Rectangular(Xleft,YUp,Xleft+i,Ylow,clBlue);
        Rectangular(XRight-i,YUp,XRight,Ylow,clBlue);
      end;
    flagRajashtan:FiveColorFlag(TColor($2f24db),TColor($14e7f5),TColor($13c414),
       TColor($fffff6),TColor($e4232f));
    flagRedBlackFlag:TwoRightTriangleUpDownFlag(clRed,clBlack);
    flagReeuwijk:QuadColorFlag(clWhite,clRed,clYellow,clBlue);
    flagSantiagoDeCali:begin
        TriColorFlagHorizontal(TColor($dd0000),clWhite,TColor($00aa00));
        i:=MulDiv(YLow-YUp,34,600);
        j:=MulDiv(YLow-YUp,177,600);
        Rectangular(Xleft,Yup+j,XRight,yup+j+i,clRed);
        Rectangular(Xleft,ylow-j,XRight,ylow-j-i,clRed);
      end;
    flagSharjah:begin
        Rectangular(Xleft,YUp,XRight,Ylow,clWhite);
        i:=MulDiv(XRight-Xleft,100,800);
        j:=MulDiv(YLow-YUp,100,400);
        Rectangular(Xleft+i,YUp+j,XRight-i,Ylow-j,TColor($0000bf));
      end;
    flagSchellenberg:QuadColorFlag(clBlack,TColor($16D0FC),
      clBlack,TColor($16D0FC));
    flagSchiedam:SixColorFlag(clYellow,ClBlack,clYellow,ClBlack,
      clYellow,ClBlack);
    flagSchiermonnikoog:SevenColorFlag(clRed,ClWhite,TColor($ff2110),
      TColor($00ce00),clRed,ClWhite,TColor($ff2110));
    flagSlaskie:begin
        Rectangular(Xleft,YUp,XRight,Ylow,TColor($ff7f2a));
        i:=Muldiv(YLow-YUp,32,80);
        Rectangular(Xleft,YUp+i,XRight,YLow-i,TColor($0c0f0));
      end;
    flagSverdlovsk:begin
        Rectangular(Xleft,YUp,XRight,Ylow,clWhite);
        i:=YLow-YUp;
        Rectangular(Xleft,YUp+Muldiv(i,7,20),XRight,YUp+Muldiv(i,32,40),clBlue);
        Rectangular(Xleft,YUp+Muldiv(i,34,40),XRight,YLow,clGreen);
      end;
    flagTallinn: SixColorFlag(TColor($9C5700),clWhite,TColor($9C5700),clWhite,
      TColor($9C5700),clWhite);
    flagTatarstan:begin
        BiColorFlagHorizontal(clGreen, clRed);
        i :=MulDiv(YLow-YUp,7,15);
        Rectangular(Xleft,Yup+i,XRight,YLow-i,clWhite);
      end;
    flagTerAar:begin
        BiColorFlagVertical(clRed,clYellow);
        x:=XRight-Xleft;
        i:=MulDiv(x,1,6);
        Rectangular(Xleft+x div 2 + i,Yup,XRight-i,YLow,clRed);
      end;
    flagTetovo:SixColorFlag(clRed,clRed,clRed,ClBlack,ClBlack,clYellow);
    flagThailand:SixColorFlag(clRed,clWhite,clBlue,clBlue,clWhite,clRed);
    flagTholen:NineColorFlag(clRed,clWhite,clBlue,clYellow,clYellow,clYellow,
       clRed,clWhite,clBlue);
    flagTrenciansky:SixColorFlag(ColorskBlue,clWhite,clWhite,clRed,
      clRed,ColorskBlue);
    flagUnitedArabEmirates:begin
        TriColorFlagHorizontal(clGreen,clWhite,clBlack);
        Rectangular(Xleft,Yup,((XRight-XLeft)div 5)+Xleft,YLow,clRed);
      end;
    flagUtrechtCity:TwoRightTriangleDownUpFlag(clWhite,clRed);
    flagUttarPradesh:begin
        Rectangular(Xleft,YUp,XRight,Ylow,TColor($1513dd));
        i:=YLow-YUp;
        Rectangular(Xleft,YUp+Muldiv(i,195,300),XRight,Ylow,TColor($157700));
      end;
    flagVaduz:QuadColorFlag(clRed,clWhite,clRed,clRed);
    flagValkenburg:TwoRightTriangleDownUpFlag(clYellow,clRed);
    flagWestvoorne:SixColorFlag(clRed,ClWhite,TColor($008000),TColor($008000),
      clYellow,ClBlack);
    flagWinterswijk:TwoPlusTwoColor(TColor($fe0000),clYellow,
      clWhite,TColor($fe0000));
    flagWojewodztwoPodlaskie:QuadColorFlag(clWhite,clRed,clYellow,clBlue);
    flagWoudenberg:begin
        TriColorFlagHorizontal(clYellow,clBlack,clYellow);
        i:=MulDiv(YLow-YUp,4,9);
        Rectangular(Xleft,Yup+i,XRight,YLow-i,clRed);
      end;
    flagWriezen:TwoPlusTwoColor(clYellow,clBlue,clBlue,clYellow);
    flagZachPomorskie:begin
        Rectangular(Xleft,YUp,XRight,Ylow,clWhite);
        i:=Muldiv(XRight-Xleft,42,120);
        Rectangular(Xleft+i,YUp,XRight-i,YLow,clRed);
    end;
    flagZilinsky:QuadColor2SFlag(clYellow,ColorskBlue,clGreen,clRed);
    flagZurich:TwoRightTriangleDownUpFlag(clWhite,TColor($e57300));
  end;
end;


procedure TCustomFlag.CrossFlag;
begin
  case FFlag of
    flagBornholm:CrossBasic(clRed,clGreen,37,28,12,4,12);
    flagDenmark:CrossBasic(clRed,clWhite,37,28,12,4,12);
    flagEngland:CrossBasic(clWhite,clRed,75,45,32,9,18);
    flagFinland:CrossBasic(clWhite,clBlue,18,11,5,3,4);
    // Finland's blue = CMYK C 100 %, M 56 %, Y 0 %, K 18.5 %
    flagKalmarUnion:CrossBasic(clYellow,clRed,16,10,5,2,4);
    flagOldenburg:CrossBasic(clBlue,clRed,1000,625,312,125,250);
    flagPula:CrossBasic(TColor($009900),clYellow,480,240,116,56,92);
    flagSaintDavid:CrossBasic(clBlack,clYellow,75,45,32,9,18);
    flagSaintPiran:CrossBasic(clBlack,clWhite,75,45,32,9,18);
    flagScanianCrossFlag:CrossBasic(clRed,clYellow,16,10,5,2,4);
    flagSweden:CrossBasic(clBlue,clYellow,16,10,5,2,4);
    flagZwolle:CrossBasic(TColor($e90000),clWhite,90,60,36,18,21);

    flagAalandIslands:CrossBasic(clBlue,TColor($00C0ff),52,34,16,10,12);
    flagDevon        :CrossBasic(clGreen, clBlack,256,144,110,36,54);
    flagDurham       :CrossBasic(clBlack,clWhite,900,450,375,150,150);
    flagEastKarelia  :CrossBasic(clGreen, clRed,  36,22,10,6,8);
    flagFaroeIslands :CrossBasic(clWhite, clBlue, 24,16, 6,4,6);
    flagHighlands    :CrossBasic(TColor($c67200),clWhite,1100,800,300,200,300);
    flagIceland      :CrossBasic(clBlue,  clWhite,25,18, 7,4,7);
    flagIngria       :CrossBasic(clYellow,clRed,  36,22,10,6,8);
    flagMizoramIndia:CrossBasic(TColor($ee3618),clWhite,1000,728,232,226,268);
    flagNordicCelticFlag:CrossBasic(TColor($009900),clWhite,1000,600,272,134,234);
    flagNormandy     :CrossBasic(clRed,clYellow,900,570,212,142,212);
    flagNorway       :CrossBasic(clRed,   clWhite,24,16, 6,4,6);
    flagOrkney       :CrossBasic(clRed, clYellow,256,185,70,45,70);
    flagShetland     :CrossBasic(TColor($cc6500),clWhite,900,600,300,120,240);
    flagSmaland      :CrossBasic(clGreen,clWhite,1000,600,300,140,230);
    flagSouthUist:CrossBasic(TColor($008000),TColor($00f3ff),900,653,245,165,245);
    flagVepsia       :CrossBasic(clGreen, clRed,  36,22,10,6,8);
    flagZomi   :CrossBasic(TColor($2129da),TColor($00f3ff),900,600,350,200,200);
  end;
  case FFlag of   {AddCross}
    flagAalandIslands:CrossPaint(clRed,   52, 34,19,4,15);
    flagDevon        :CrossPaint(clWhite,256,144,115,26,59);
    flagDurham       :CrossPaint(clRed,  900,450,405,90,180);
    flagEastKarelia  :CrossPaint(clBlack, 36, 22,11,4, 9);
    flagFaroeIslands :CrossPaint(clRed,   24, 16, 7,2, 7);
    flagHighlands    :CrossPaint(TColor($009900),1100,800,350,100,350);
    flagIceland      :CrossPaint(clRed,   25, 18, 8,2, 8);
    flagIngria       :CrossPaint(clBlue,  36, 22,11,4, 9);
    flagMizoramIndia:CrossPaint(TColor($140deb),1000,728,275,150,305);
    flagNordicCelticFlag:CrossPaint(TColor($00777e8),1000,600,304,66,266);
    flagNormandy     :CrossPaint(clRed,  900,570,250,68,250);
    flagNorway       :CrossPaint(clBlue,  24, 16, 7,2, 7);
    flagOrkney       :CrossPaint(clBlue, 256,185,81,23,81);
    flagSmaland      :CrossPaint(clRed, 1000,600,320,100,250);
    flagSouthUist:CrossPaint(TColor($663300),900,653,286,82,286);
    flagVepsia       :CrossPaint(clBlue,  36, 22,11,4, 9);
    flagZomi      :CrossPaint(TColor($429900), 900,600,400,100,250);
  end;
end;
procedure TCustomFlag.CoupedCrossFlag;
var BaseColor,CrossColor,AddColor:TColor;
    XRect,YRect:integer;
    AddRectangle:boolean;
    PlusRect: TRect;
begin
  AddRectangle:=false;
  with PlusRect
    do begin
      case FFlag of
        flagRedCross:begin
            BaseColor :=clWhite;
            CrossColor:=clRed;
            RightRatioSize(3,2);
            Left:=XLeft+MulDiv(XRight-XLeft,75,300);
            Right:=XRight-MulDiv(XRight-XLeft,75,300);
            Top:=YUp+MulDiv(YLow-YUp,25,200);
            Bottom:=YLow-MulDiv(YLow-YUp,25,200);
          end;
        flagSwitzerland:begin
            BaseColor :=clRed;
            CrossColor:=clWhite;
            RightRatioSize(1,1);
            Left:=XLeft+(XRight-XLeft) div 5;
            Right:=XLeft+MulDiv(XRight-XLeft,1+3, 5);
            Top:=YUp+(YLow-YUp)  div 5;
            Bottom:=YUp+MulDiv(YLow-YUp,1+3, 5);
          end;
        flagTonga:begin
            BaseColor :=clRed;
            CrossColor:=clRed;
            AddColor:=clWhite;
            RightRatioSize(3,2);
            Left:=XLeft+MulDiv(XRight-XLeft,2,21);
            Right:=XLeft+MulDiv(XRight-XLeft,2+3,21);
            Top:=YUp+(YLow-YUp)  div 16;
            Bottom:=YUp+MulDiv(YLow-YUp,7,16);
            AddRectangle:=true;
            XRect:=XLeft+(XRight-XLeft) div 3;
            YRect:=YUp+(YLow-YUp) div 2 ;
          end;
      end;
  end;
  with canvas do
     begin
       Pen.color:=BaseColor;
       Brush.color:=BaseColor;
       Rectangle(XLeft,Yup,XRight,Ylow);
       if AddRectangle  //flagTonga
          then begin
            Pen.color:=AddColor;
            Brush.color:=AddColor;
            Rectangle(XLeft,Yup,XRect,YRect);
          end;
       Pen.color:=CrossColor;
       Brush.color:=CrossColor;
       PaintPlus(Canvas,PlusRect);
     end;
end;
procedure TCustomFlag.SpecialCrossFlag;
var X,Y,x1,x2,y1,y2:integer;
    dx,dy,wd,ht,dg:Real;
    PaintRect:TRect;
    cp:tpoint;
begin
  if FFlag = flagAlabama
    then begin
      RightRatioSize(3,2);
      Rectangular(XLeft,YUp,XRight,YLow,clWhite);
      x1:=MulDiv(XRight-XLeft,90,900);
      y1:=MulDiv(YLow-YUp,60,600);
      PaintXDiagonals(XLeft,YUp,XRight,YLow,x1,y1,TColor($2100b1));
    end;
  if FFlag = flagAtica
    then begin
      RightRatioSize(5,3);
      Rectangular(XLeft,YUp,XRight,YLow,TColor($0e08dd));
      x:=XRight-XLeft;
      y:=YLow-Yup;
      x1:=MulDiv(x,58,1000);
      y1:=MulDiv(y,58,600);
      Rectangular(XLeft+x1,YUp+y1,XRight-x1,YLow-y1,TColor($05fdfd));
      y2:=MulDiv(y,73,600);
      x2:=MulDiv(x,73,1000);
      if x1>=x2 then x2:=x2+1;
      if y1>=y2 then y2:=y2+1;
      Rectangular(XLeft+x2,YUp+y2,XRight-x2,YLow-y2,TColor($d51c23));
      x1:=XLeft+MulDiv(x,323,1000);
      x2:=XLeft+MulDiv(x,672,1000);
      y1:=YUp+MulDiv(y,252,600);
      y2:=YUp+MulDiv(y,338,600);
      CrossBasicPaint(clWhite,x1,YUp+MulDiv(y,127,600),x2,YUp+MulDiv(y,482,600),
        XLeft+MulDiv(x,445,1000),XLeft+MulDiv(x,542,1000),y1,y2);
    end;
  if FFlag = flagBaarn
    then begin
      RightRatioSize(3,2);
      TriColorFlagVertical(clBlue, clYellow, clBlue);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      x1:=MulDiv(x,117,900);
      x2:=MulDiv(x,25,900);
      CrossBasicPaint(clBlue,cp.x-X1,cp.y-x1,cp.x+X1,cp.Y+x1,
        cp.x-X2,cp.x+x2,cp.y-x2,cp.y+x2);
    end;
  if FFlag = flagBasque
    then begin
      RightRatioSize(25,14);
      Rectangular(XLeft,YUp,XRight,YLow,clRed);
      wd:=XRight-XLeft; ht:=YLow-YUp;
      dg:=sqrt(wd*wd+ht*ht);
      dx:=dg/12; dy:=dg/24;
      PaintXDiagonals(XLeft,YUp,XRight,YLow,round(dx),round(dy),clGreen);
      CrossPaint(clWhite,250,140,115,20,60);
    end;
  if fflag = flagBedum
    then begin
      RightRatioSize(3,2);
      TriColorFlagVertical(TColor($ff6600), clWhite, clWhite);
      CrossPaint(clBlack,900,600,200,200,200);
      CrossPaint(clYellow,900,600,225,150,225);
    end;
  if FFlag = flagBurgers
    then begin
      RightRatioSize(1000,636);
      Rectangular(XLeft,YUp,XRight,YLow,TColor($8b4621));
      x1:=MulDiv(XRight-XLeft,110,1000);
      y1:=MulDiv(YLow-YUp,90,636);
      PaintXDiagonals(XLeft,YUp,XRight,YLow,x1,y1,clWhite);
      x1:=MulDiv(XRight-XLeft,70,1000);
      y1:=MulDiv(YLow-YUp,70,636);
      PaintXDiagonals(XLeft,YUp,XRight,YLow,x1,y1,TColor($281cae));
    end;
  if fflag = flagChristian
    then begin
      RightRatioSize(600,400);
      Rectangular(XLeft,YUp,XRight,YLow,clWhite);
      x:=XRight-XLeft;
      y:=YLow-Yup;
      Rectangular(Xleft,Yup,MulDiv(X,254,600)+Xleft,
      Yup+MulDiv(Y,216,400),TColor($680028));
      CrossBasicPaint(TColor($0A30BF),Xleft+MulDiv(x,74,600),
      Yup+MulDiv(y,36,400),XLeft+MulDiv(x,180,600),Yup+MulDiv(y,180,400),
      XLeft+MulDiv(x,114,600),XLeft+MulDiv(x,138,600),
      Yup+MulDiv(y,76,400),Yup+MulDiv(y,100,400));
    end;
  if FFlag= flagDominicanRepublic
    then begin
       CrossBasic(clRed,clWhite,30,20,13,4,8);
       X:=round((XRight-Xleft)/30*13);
       Y:=round((YLow-YUp)/20*8);
       Rectangular(XLeft,Yup,Xleft+X,YUp+Y,clNavy);
       Rectangular(XRight-X,YLow-Y,XRight,YLow,clNavy);
    end;
   if FFlag = flagGreece
     then begin
       RightRatioSize(5,3);
       NineColorFlag(clBlue,clWhite,clBlue,clWhite,clBlue,clWhite,clBlue,
         clWhite,clBlue);
       y:=YLow-YUp;
       x:=XLeft+(XRight-XLeft) div 3;
       x1:=XLeft+MulDiv(x-XLeft, 2, 5) ;
       x2:=XLeft+MulDiv(x-XLeft, 2+1, 5);
       Rectangular(XLeft,YUp,x,Yup+MulDiv(y,5,9),clBlue);
       CrossBasicPaint(clWhite,XLeft,YUp,x,Yup+MulDiv(y,5,9),x1,x2,
       Yup+MulDiv(y,2,9),Yup+MulDiv(y,3,9));
     end;
   if FFlag = flagGroningen
    then begin
      RightRatioSize(3,2);
      TwoPlusTwoColor(clRed,clBlue,clBlue,clRed);
      CrossPaint(clWhite,900,600,340,220,190);
      CrossPaint(TColor($00cc00),900,600,410,80,260);
    end;
  if FFlag = flagGuernsey
    then begin
      CrossBasic(clWhite,clRed,36,24,15,6,9);
      PaintRect:=Rect(XLeft+MulDiv((XRight-Xleft),16,36),
        YUp+MulDiv((YLow-YUp),3,24), XRight-MulDiv((XRight-Xleft),16,36),
        YLow-MulDiv((YLow-YUp),3,24));
      Canvas.Pen.color:=clWhite;
      Canvas.Brush.color:=clWhite;
      PaintBigI(canvas,PaintRect, 0.0);
      PaintRect:=Rect(XLeft+MulDiv((XRight-Xleft),9,36),
        YUp+MulDiv((YLow-YUp),10,24), XRight-MulDiv((XRight-Xleft),9,36),
        YLow-MulDiv((YLow-YUp),10,24));
      PaintBigI(canvas,PaintRect, pi/2);
    end;
  if FFlag = flagIndianapolis
    then begin
       RightRatioSize(1000,602);
       CrossBasic(clNavy,clWhite,1000,602,450,100,251);
       CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
       Ball(cp.x,cp.y,MulDiv(x,140,1000),clWhite);
       Ball(cp.x,cp.y,MulDiv(x,80,1000),clRed);
       FivePointStarBasic(cp.x,cp.y,MulDiv(x,67,1000),clWhite);
     end;
  if FFlag = flagIFRCflag
    then begin
      RightRatioSize(900,600);
      Rectangular(XLeft,YUp,XRight,YLow,clRed);
      x:=XRight-Xleft;
      y:=MulDiv(x,5,900);
      if y<1 then y:=1;
      Rectangular(XLeft+y,YUp+y,XRight-y,YLow-y,clWhite);
      y:=YLow-YUp;
      CrossBasicPaint(clRed,XLeft+MulDiv(x,104,1000),YUp+MulDiv(y,119,600),
        XLeft+MulDiv(x,455,900),YUp+MulDiv(y,479,600),XLeft+MulDiv(x,225,900),
        XLeft+MulDiv(x,323,900),YUp+MulDiv(y,250,600),YUp+MulDiv(y,348,600));
      y:=YUp+(YLow-YUp) div 2;
      Ball(XLeft+MulDiv(x,673,900),y,MulDiv(x,182,900),clRed);
      Ball(XLeft+MulDiv(x,725,900),y,MulDiv(x,134,900),clWhite);
    end;
  if FFlag = flagKatwijk
    then begin
      RightRatioSize(3,2);
      Rectangular(XLeft,YUp,XRight,YLow,clWhite);
      x1:=MulDiv(XRight-XLeft,115,900);
      y1:=MulDiv(YLow-YUp,77,600);
      PaintXDiagonals(XLeft,YUp,XRight,YLow,x1,y1,clBlue);
    end;
  if FFlag = flagLevis
    then begin
      RightRatioSize(3,2);
      TwoPlusTwoColor(clBlack,TColor($4a738f),TColor($4a738f),clBlack);
      CrossPaint(clWhite,600,400,250,100,150);
    end;
  if FFlag = flagNeuchatel
    then begin
      RightRatioSize(1,1);
      TriColorFlagVertical(TColor($008000),clWhite, clRed);
      x:=YLow-YUp;
      x1:=MulDiv(x,295,1000);
      y1:=MulDiv(x,37,1000);
      x2:=MulDiv(x,184,1000);
      y2:=MulDiv(x,148,1000);
      CrossBasicPaint(clWhite,XRight-x1,YUp+y1,XRight-y1,YUp+x1,
        XRight-X2,XRight-y2,YUp+y2,YUp+x2);
    end;
  if FFlag = flagSaar
    then begin
      RightRatioSize(3,2);
      Rectangular(XLeft,YUp,XRight,YLow,TColor($2b2bef));
      x:=XRight-XLeft;
      Rectangular(XLeft,YUp,XLeft+MulDiv(x,200,600),YLow,TColor($8c1c0c));
      CrossPaint(clWhite,600,400,200,80,160);
    end;
  if FFlag = flagSaguenay
    then begin
      RightRatioSize(3,2);
      BiColorFlagHorizontal(clGreen,clYellow);
      CrossPaint(clRed,114,76,42,30,23);
      CrossPaint(clGray,114,76,48,18,29);
    end;
  if FFlag = flagSaintAlban
    then begin
      RightRatioSize(5,3);
      Rectangular(XLeft,YUp,XRight,YLow,TColor($c57200));
      x:=XRight-XLeft;
      y:=YLow-YUp;
      x1:=MulDiv(x,117,1000);
      y1:=MulDiv(y,70,600);
      PaintXDiagonals(XLeft,YUp,XRight,YLow,x1,y1,clYellow);
    end;
  if FFlag = flagSchwyz
    then begin
      RightRatioSize(1,1);
      Rectangular(XLeft,YUp,XRight,YLow,clRed);
      x:=YLow-YUp;
      x1:=MulDiv(x,24,500);
      y1:=MulDiv(x,190,500);
      x2:=MulDiv(x,95,500);
      y2:=MulDiv(x,118,500);
      CrossBasicPaint(clWhite,XLeft+x1,YUp+x1,XLeft+y1,YUp+y1,
        XLeft+X2,XLeft+y2,YUp+x2,YUp+y2);
    end;
  if FFlag = flagScillonianCross
    then begin
      RightRatioSize(2,1);
      BiColorFlagHorizontal(TColor($009aff),TColor($9c3000) );
      x:=XRight-XLeft;
      y:=YLow-YUp;
      CrossBasicPaint(clWhite,XLeft,YUp,XRight,YLow, XLeft+MulDiv(x,450,1000),
        XLeft+MulDiv(x,550,1000),YUp+MulDiv(y,200,500),YUp+MulDiv(y,300,500));
      x1:=XLeft+MulDiv(x,812,1000);
      x2:=XLeft+MulDiv(x,849,1000);
      FivePointStar(x1,YUp+MulDiv(y,29,500),x2,YUp+MulDiv(y,63,500),clWhite);
      x1:=XLeft+MulDiv(x,831,1000);
      x2:=XLeft+MulDiv(x,873,1000);
      FivePointStar(x1,YUp+MulDiv(y,142,500),x2,YUp+MulDiv(y,181,500),clWhite);
      x1:=XLeft+MulDiv(x,845,1000);
      x2:=XLeft+MulDiv(x,895,1000);
      FivePointStar(x1,YUp+MulDiv(y,7,500),x2,YUp+MulDiv(y,55,500),clWhite);
      x1:=XLeft+MulDiv(x,867,1000);
      x2:=XLeft+MulDiv(x,952,1000);
      FivePointStar(x1,YUp+MulDiv(y,69,500),x2,YUp+MulDiv(y,148,500),clWhite);
      x1:=XLeft+MulDiv(x,912,1000);
      x2:=XLeft+MulDiv(x,953,1000);
      FivePointStar(x1,YUp+MulDiv(y,8,500),x2,YUp+MulDiv(y,47,500),clWhite);
    end;
  if FFlag = flagScotland
    then begin
      RightRatioSize(5,3);
      Rectangular(XLeft,YUp,XRight,YLow,clBlue);
      wd:=XRight-XLeft; ht:=YLow-YUp;
      dg:=sqrt(wd*wd+ht*ht);
      x1:=round(dg/10); y1:=round(dg/20);
      PaintXDiagonals(XLeft,YUp,XRight,YLow,x1,y1,clWhite);
    end;
  if FFlag = flagTenerife
    then begin
      RightRatioSize(5,3);
      Rectangular(XLeft,YUp,XRight,YLow,TColor($720e2b));
      x1:=MulDiv(XRight-XLeft,85,1000);
      y1:=MulDiv(YLow-YUp,84,600);
      PaintXDiagonals(XLeft,YUp,XRight,YLow,x1,y1,clWhite);
    end;
  if FFlag = flagTracia
    then begin
      RightRatioSize(5,3);
      Rectangular(XLeft,YUp,XRight,YLow,TColor($bd0815));
      x:=XRight-XLeft;
      y:=YLow-YUp;
      x1:=XLeft+MulDiv(x,388,1000);
      y1:=YUp+MulDiv(y,210,600);
      y2:=YUp+MulDiv(y,395,600);
      x2:=XLeft+MulDiv(x,368,1000);
      if x1<=x2+1 then x1:=x2+2;
      Rectangular(x1,y1,XRight,y2,clWhite);
      x1:=XLeft+MulDiv(x,18,1000);
      CrossBasicPaint(clWhite,x1,YUp+MulDiv(y,125,600),x2,YUp+MulDiv(y,482,600),
        XLeft+MulDiv(x,101,1000),XLeft+MulDiv(x,285,1000),y1,y2);
    end;
  if fflag = flagUtrecht
    then begin
      RightRatioSize(3,2);
      BiColorFlagHorizontal(clWhite, clRed);
      x:=XRight-XLeft;
      y:=YLow-YUp;
      x1:=XLeft+MulDiv(x,205,900);
      y1:=YUp+MulDiv(y,195,600);
      Rectangular(XLeft,YUp,x1,y1,clRed);
      x2:=XLeft+MulDiv(x,78,900);
      x:=XLeft+MulDiv(x,125,900);
      y2:=YUp+MulDiv(y,70,600);
      y:=YUp+MulDiv(y,125,600);
      CrossBasicPaint(clWhite,XLeft,YUp, X1,Y1,X2,x,y2,y);
    end;
 if FFlag = flagValdivia
    then begin
      RightRatioSize(3,2);
      Rectangular(XLeft,YUp,XRight,YLow,clWhite);
      x1:=MulDiv(XRight-XLeft,40,900);
      y1:=MulDiv(YLow-YUp,28,600);
      PaintXDiagonals(XLeft,YUp,XRight,YLow,x1,y1,clRed);
    end;
  if FFlag = flagVolyn
    then begin
      CrossBasic(clRed,clWhite,1000,720,279,155,280);
      x:=XRight-Xleft;
      y:=YLow-YUp;
      x1:=MulDiv(x,107,1000);
      x2:=MulDiv(x,171,1000);
      y1:=MulDiv(y,22,720);
      y2:=MulDiv(y,185,720);
      Triagle(XLeft+x1,YUp+y1,Xleft+x2,YUp+y2,atDown,clWhite);
      y1:=MulDiv(y,92,720);
      y2:=MulDiv(y,259,720);
      Triagle(XLeft+x1,YUp+y1,Xleft+x2,YUp+y2,atUp,clWhite);
      x1:=MulDiv(x,20,1000);
      x2:=MulDiv(x,182,1000);
      y1:=MulDiv(y,108,720);
      y2:=MulDiv(y,173,720);
      Triagle(XLeft+x1,YUp+y1,Xleft+x2,YUp+y2,atRight,clWhite);
      x1:=MulDiv(x,95,1000);
      x2:=MulDiv(x,257,1000);
      Triagle(XLeft+x1,YUp+y1,Xleft+x2,YUp+y2,atLeft,clWhite);
    end;
  if FFlag = flagWallisAndFutuna
    then begin
      RightRatioSize(3,2);
      Rectangular(XLeft,YUp,XRight,YLow,clRed);
      x:=XRight-Xleft;
      y:=YLow-YUp;
      Rectangular(XLeft,YUp,
        XLeft+MulDiv(x,242,600),YUp+MulDiv(y,161,400),clWhite);
      TriColorBasicVertical(clNavy, clWhite, clRed,XLeft,Yup,
        XLeft+MulDiv(x,237,600),YUp+MulDiv(y,157,400));
      x1:=MulDiv(x,372,600);
      x2:=MulDiv(x,468,600);
      y1:=MulDiv(y,135,400);
      y2:=MulDiv(y,48,400);
      Triagle(XLeft+x1,YUp+y1,Xleft+x2,YUp+y1+y2,atDown,clWhite);
      y1:=MulDiv(y,215,400);
      Triagle(XLeft+x1,YUp+y1,Xleft+x2,YUp+y1+y2,atUp,clWhite);
      x1:=MulDiv(x,355,600);
      x2:=MulDiv(x,50,600);
      y1:=MulDiv(y,152,400);
      y2:=MulDiv(y,246,400);
      Triagle(XLeft+x1,YUp+y1,Xleft+x1+x2,YUp+y2,atRight,clWhite);
      x1:=MulDiv(x,436,600);
      Triagle(XLeft+x1,YUp+y1,Xleft+x1+x2,YUp+y2,atLeft,clWhite);
    end;
end;

procedure TCustomFlag.UnionJack(X1,Y1,X2,Y2:integer);
var CrossX1,CrossX2,CrossY1,CrossY2,x,y:integer; dg:Real;
begin
   Rectangular(X1,Y1,X2,Y2,TColor($660000){TColor($7d2400)});
   x:=X2-X1; y:=Y2-Y1;
   dg:=sqrt(x*x+y*y);
   PaintDiagonals(X1,Y1,X2,Y2,dg/10,dg/20);
   CrossX1:=X1+MulDiv(x, 25, 60) ;
   CrossX2:=X1+MulDiv(x, 25+10, 60);
   CrossY1:=Y1+MulDiv(y, 10, 30) ;
   CrossY2:=Y1+MulDiv(y, 10+10, 30);
   CrossBasicPaint(clWhite,X1,Y1,X2,Y2, CrossX1,CrossX2,CrossY1,CrossY2);
   CrossX1:=X1+MulDiv(x, 27,   60);
   CrossX2:=X1+MulDiv(x, 27+6, 60);
   CrossY1:=Y1+MulDiv(y, 12,   30);
   CrossY2:=Y1+MulDiv(y, 12+6, 30);
   CrossBasicPaint(clRed,X1,Y1,X2,Y2, CrossX1,CrossX2,CrossY1,CrossY2);
end;

procedure TCustomFlag.UnionJackFlags;
var  x,y,r,h,i,j,w:integer;
    cp:TPoint;
    
  procedure CookStars(cx,cy,r:integer);
  var sr,x1,y1,i,dx,dy:integer;
     theta,dth:real;
  begin
    sr:=round(r/44*9);
    theta:=Pi/2;
    dth:=Pi/7.5;
    for i:=1 to 15 do
      begin
        theta:=theta+dth;
        dx:=Round(r*cos(theta)); dy:=Round(r*sin(theta));
        FivePointStarBasicAngle(cx+dx,cy-dy,sr,2*pi*i/15,clWhite);
      end;
  end;
  procedure NewZealandStars;
  var unitX,unitY,alfa,sini,kosini,difX,difY:Real;
  begin
       FivePointStarEdge(XLeft+x div 4*3,YUp+y div 5,y*7 div 120,clWhite,clRed);
       FivePointStarEdge(XLeft+x div 4*3,YLow-y div 5,
                   y div 15,clWhite,clRed);
       unitX:=x/120; unitY:=y/60;
       alfa:=8*Pi/180;
       sini:=sin(alfa); kosini:=cos(alfa);
       difX:=(30+14*kosini)*unitX;
       difY:=(24+14*sini)*unitY;
       FivePointStarEdge(xRight-Round(difX),
         YUp+Round(difY),y*7 div 120,clWhite,clRed);
       difX:=(30-12*kosini)*unitX;
       difY:=(24-12*sini)*unitY;
       FivePointStarEdge(xRight-Round(difX),
         YUp+Round(difY),y div 20,clWhite,clRed);
  end;
begin
  RightRatioSize(2,1);
  CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
  case FFlag of
    flagUnitedKingdom: UnionJack(XLeft,YUp,XRight,YLow);
    flagAustralia:begin
      Rectangular(XLeft,YUp,XRight,YLow,TColor($660000));
      UnionJack(XLeft,YUp,cp.x,cp.y);
      r:=MulDiv(y,9,60);
      SevenPointStar(XLeft+MulDiv(x,3,12),YUp+MulDiv(y,45,60),r,clWhite);
      r:=MulDiv(y,429,6000);
      SevenPointStar(XLeft+MulDiv(x,75,120),YUp+MulDiv(y,2628,6000),r,clWhite);
      SevenPointStar(XLeft+MulDiv(x,90,120),YUp+MulDiv(y,10,60),r,clWhite);
      SevenPointStar(XLeft+MulDiv(x,90,120),Ylow-MulDiv(y,10,60),r,clWhite);
      SevenPointStar(XLeft+MulDiv(x,10332,12000),YUp+MulDiv(y,222,600),r,clWhite);
      FivePointStar(XLeft+MulDiv(x,935,1200),YUp+MulDiv(y,3002,6000),
        XLeft+MulDiv(x,985,1200),YUp+MulDiv(y,3502,6000),clWhite,0.0);
     end;
     flagBlueEnsign:begin
       Rectangular(XLeft,YUp,XRight,YLow,TColor($660000));
       UnionJack(XLeft,YUp,cp.x,cp.y);
     end;
     flagCookIslands:begin
       Rectangular(XLeft,YUp,XRight,YLow,TColor($660000));
       UnionJack(XLeft,YUp,cp.x,cp.y);
       CookStars(XLeft+MulDiv(x,3,4),cp.y,MulDiv(y,22,60));
     end;
     flagEnsignRoyalAirForce:begin
       Rectangular(XLeft,YUp,XRight,YLow,TColor($a5844a));
       UnionJack(XLeft,YUp,cp.x,cp.y);
       i:=XLeft+MulDiv(x,900,1200);
       Ball(i,cp.y,MulDiv(y,230,600),TColor($660000));
       Ball(i,cp.y,MulDiv(y,139,600),clWhite);
       Ball(i,cp.y,MulDiv(y,47,600),clRed);
     end;
     flagHawaii:begin
       EightColorFlag(clWhite,clRed,TColor($660000),
       clWhite,clRed,TColor($660000),clWhite,clRed);
       UnionJack(XLeft,YUp,cp.x,cp.y);
     end;
     flagNewZealand: begin
       Rectangular(XLeft,YUp,XRight,YLow,TColor($660000));
       UnionJack(XLeft,YUp,cp.x,cp.y);
       NewZealandStars;
     end;
     flagNiue:begin
       Rectangular(XLeft,YUp,XRight,YLow,clYellow);
       UnionJack(XLeft,YUp,cp.x,cp.y);
       w:=MulDiv(x,94,2000);
       h:=MulDiv(Y,90,1000);
       i:=MulDiv(x,160,2000);
       j:=MulDiv(Y,205,1000);
       FivePointStar(XLeft+i, YUp+j,XLeft+i+w, YUp+j+h, clYellow);
       i:=MulDiv(x,744,2000);
       FivePointStar(XLeft+i, YUp+j,XLeft+i+w, YUp+j+h, clYellow);
       i:=MulDiv(x,452,2000);
       j:=MulDiv(Y,38,1000);
       FivePointStar(XLeft+i, YUp+j,XLeft+i+w, YUp+j+h, clYellow);
       j:=MulDiv(Y,373,1000);
       FivePointStar(XLeft+i, YUp+j,XLeft+i+w, YUp+j+h, clYellow);
       r:=MulDiv(x,83,2000);
       i:=MulDiv(x,500,2000)+XLeft;
       j:=MulDiv(Y,250,1000)+YUp;
       Ball(i,j,r,clNavy);
       FivePointStarBasic(i,j,r,clYellow);
     end;
     flagRedEnsign:begin
       Rectangular(XLeft,YUp,XRight,YLow,clRed);
       UnionJack(XLeft,YUp,cp.x,cp.y);
     end;
     flagRossDependency:begin
       Rectangular(XLeft,YUp,XRight,YLow,TColor($e59966));
       Rectangular(XLeft,YLow-MulDiv(y,225-20,1500),XRight,YLow,clWhite);
       UnionJack(XLeft,YUp,cp.x,cp.y);
       NewZealandStars;
     end;
     flagTuvalu: begin
       Rectangular(XLeft,YUp,XRight,YLow,TColor($b19700));
       UnionJack(XLeft,YUp,cp.x,cp.y);
       w:=MulDiv(x,187,2000);
       j:=MulDiv(Y,806,1000);
       h:=MulDiv(Y,166,1000);
       FivePointStar(cp.x, YUp+j,cp.x+w, YUp+j+h ,clYellow);
       i:=MulDiv(x,1591,2000);
       j:=MulDiv(Y,552,1000);
       FivePointStar(XLeft+i, YUp+j,XLeft+i+w, YUp+j+h, clYellow);
       i:=MulDiv(x,1768,2000);
       j:=MulDiv(Y,438,1000);
       FivePointStar(XLeft+i, YUp+j,XLeft+i+w, YUp+j+h, clYellow);
       j:=MulDiv(Y,44,1000);
       FivePointStar(XLeft+i, YUp+j,XLeft+i+w, YUp+j+h, clYellow);
       i:=MulDiv(x,1206,2000);
       w:=MulDiv(x,186,2000);
       j:=MulDiv(Y,772,1000);
       h:=MulDiv(Y,168,1000);
       FivePointStar(XLeft+i, YUp+j,XLeft+i+w, YUp+h+j ,clYellow,45*Pi/180);
       j:=MulDiv(Y,569,1000);
       FivePointStar(XLeft+i, YUp+j,XLeft+i+w, YUp+h+j ,clYellow,45*Pi/180);
       i:=MulDiv(x,1416,2000);
       j:=MulDiv(Y,721,1000);
       FivePointStar(XLeft+i, YUp+j,XLeft+i+w, YUp+h+j ,clYellow,45*Pi/180);
       j:=MulDiv(Y,280,1000);
       FivePointStar(XLeft+i, YUp+j,XLeft+i+w, YUp+h+j ,clYellow,45*Pi/180);
       i:=MulDiv(x,1593,2000);
       j:=MulDiv(Y,219,1000);
       FivePointStar(XLeft+i, YUp+j,XLeft+i+w, YUp+h+j ,clYellow,45*Pi/180);
     end;
     flagWhiteEnsign:begin
       Rectangular(XLeft,YUp,XRight,YLow,clWhite);
       w:=XLeft+MulDiv(x,560,1200);
       h:=YUp+MulDiv(Y,260,600);
       UnionJack(XLeft,YUp,w,h);
       i:=MulDiv(y,80,600);
       CrossBasicPaint(clRed,XLeft,YUp,XRight,YLow,w,w+i,h,h+i);
     end;
  end;
end;

procedure TCustomFlag.EU_Flag;
var unitX,unitY,alfa,sade:Real;
    i,isade,dy,x0,y0,hw,x1,x2,y2,y1:Integer;
    S:array[0..11] of TPoint;
begin
  RightRatioSize(3,2);
  x1:=XLeft;
  y1:=YUp;
  x2:=xRight;
  y2:=YLow;
  Rectangular(X1,Y1,X2,Y2,clBlue);
  unitX:=(x2-x1)/1.5;
  unitY:=(y2-y1)/1.0;
  alfa:=Pi/6;
  x0:=(x1+x2) div 2; y0:=(y1+y2) div 2;
  sade:=unitX/3; isade:=Round(sade);
  S[0].x:=x0+isade; S[0].y:=y0;
  S[6].x:=x0-isade; S[6].y:=y0;
  for i:=1 to 5 do
    begin
      S[i].x:=x0+Round(sade*cos(i*alfa));
      S[12-i].x:=S[i].x;
      dy:=Round(sade*sin(i*alfa));
      S[i].y:=y0-dy;
      S[12-i].y:=y0+dy;
    end;
  hw:=Round(unitY/18);
  for i:=0 to 11
    do FivePointStar(S[i].x-hw,S[i].y-hw,S[i].x+hw,S[i].y+hw,clYellow);
end;

procedure TCustomFlag.ChequeredFlags;
var w,h,i,j:Integer;
    x:array[0..6] of Integer;
    y:array[0..5] of Integer;
begin
  case FFlag of
    flagBlackWhiteChequeredFlag:begin
      RightRatioSize(6,5);
      Rectangular(XLeft,YUp,XRight,YLow,clWhite);
      w:=Round((XRight-Xleft)/6); h:=Round((YLow-YUp)/5);
      for i:=0 to 5 do
        begin
          x[i]:=XLeft+i*w;
          y[i]:=YUp+i*h;
        end;
      x[6]:=XRight;

      for i:=0 to 5 do
        for j:=0 to 4 do
          begin
            Case j of
              0,2,4: if (i=0) or (i=2) or (i=4)
                       then Rectangular(x[i],y[j],x[i+1],y[j+1],clBlack);
                1,3: if (i=1) or (i=3) or (i=5)
                       then Rectangular(x[i],y[j],x[i+1],y[j+1],clBlack);
            end;
          end;
    end;
  end;
end;

procedure TCustomFlag.StripeFlags;
var h,x1:integer;
begin
  case FFlag of
    flagGoes:begin
      RightRatioSize(3,2);
      StripeBase13(clRed,clWhite);
    end;
    flagPiaui:begin
      RightRatioSize(3,2);
      StripeBase13(TColor($4c9900),TColor($18e7f3));
      h:=round((YLow-Yup)/13*5)-1;
      Rectangular(XLeft,YUp,XLeft+h,YUp+h,TColor($802500));
      x1:= h div 2;
      FivePointStarBasic(XLeft+x1,YUp+x1,MulDiv(h,40,100),clWhite);
    end;
    flagSenyera:begin
      RightRatioSize(3,2);
      NineColorFlag(TColor($0ee8fb),TColor($0d00db),TColor($0ee8fb),
      TColor($0d00db),TColor($0ee8fb),TColor($0d00db),TColor($0ee8fb),
      TColor($0d00db),TColor($0ee8fb));
    end;
    flagTrnavsky:begin
        RightRatioSize(3,2);
        Rectangular(XLeft,YUp,XRight,YLow,ColorskBlue);
        h:=(YLow-YUp) div 9;
        Rectangular(XLeft, Yup+h*2,XRight,Yup+h*3,clYellow);
        Rectangular(XLeft, Yup+h*4,XRight,Yup+h*5,clYellow);
        Rectangular(XLeft, Yup+h*6,XRight,Yup+h*7,clYellow);
    end;
    flagWestPapua:begin
      RightRatioSize(3,2);
      StripeBase13(TColor($906000),clWhite);
      x1:=XLeft+MulDiv(XRight-Xleft,208,780);
      Rectangular(XLeft,YUp,X1,YLow,TColor($0000d0));
      x1:=XLeft+MulDiv(XRight-Xleft,104,780);
      h:=YLow-YUp;
      FivePointStarBasic(x1,YUp+h div 2,MulDiv(h,130,780),clWhite);
    end;
    flagYellowFlagWithRedStripes:begin
      RightRatioSize(7,5);
      Rectangular(XLeft,YUp,XRight,YLow,clYellow);
      Rectangular(XLeft+(XRight-Xleft) div 7,YUp,
                  XLeft+(XRight-Xleft) div 7*2,YLow,clRed);
      Rectangular(XLeft+(XRight-Xleft) div 7*3,YUp,
                  XRight-(XRight-Xleft) div 7*3,YLow,clRed);
      Rectangular(XRight-(XRight-Xleft) div 7,YUp,
                  XRight-(XRight-Xleft) div 7*2,YLow,clRed);
    end;
  end;
end;

procedure TCustomFlag.StarsAndStripeFlags;
var i,j,x2,y2,xSpace,ySpace,hw:integer;yHeight:real;
begin
  RightRatioSize(19,10);
  yHeight:=(YLow-Yup)/13;
  StripeBase13(clRed,clWhite);
  xSpace:=round((XRight-XLeft)/19*8);
  x2:=Xleft+xSpace;
  y2:=Yup+round(yHeight*7)-1;
  Rectangular(Xleft,Yup,x2,y2,clNavy);
  if xSpace > 26
    then begin
      xSpace:=(X2-XLeft)div 6;
      ySpace:=(y2-YUp)div 5;
      hw:=round(xSpace / 4.5);
      for j:=1 to 4
        do for i:=1 to 5
          do FivePointStar((XLeft+XSpace*i-hw),YUp+YSpace*j-hw,
                       (XLeft+XSpace*i+hw),YUp+YSpace*j+hw,clWhite);
      x2:=xSpace div 2+Xleft;
      y2:=ySpace div 2+YUp;
      for j:=0 to 4
        do for i:=0 to 5
         do FivePointStar((X2+XSpace*i-hw), Y2+YSpace*j-hw,
                       (X2+XSpace*i+hw), Y2+YSpace*j+hw,clWhite);
    end;
end;

procedure TCustomFlag.DiagonalFlags;
var  cp,Po1,Po2,Po3,Po4,Po5,Po6:TPoint;
     x,x1,x2,y,y1,y2:integer;
  procedure BattleFlagOfTheUSConfederacy(xL,yu,xr,yL:integer);
  var x1,y1,x2,y2:integer;
  begin
        Rectangular(XL,YU,XR,YL,TColor($300abf));
        CpXYCalc(Xl,Yu,XR,Yl,cp,x,y);
        x1:=MulDiv(x,165,1000);
        y1:=MulDiv(y,165,1000);
        PaintDiagonal(DDupDown,clWhite,Xl,Yu,XR,Yl, x1,y1);
        PaintDiagonal(DDDownUp,clWhite,Xl,Yu,XR,Yl, x1,y1);
        x1:=MulDiv(x,118,1000);
        y1:=MulDiv(y,118,1000);
        PaintDiagonal(DDupDown,TColor($682800),Xl,Yu,XR,Yl, x1,y1);
        PaintDiagonal(DDDownUp,TColor($682800),Xl,Yu,XR,Yl, x1,y1);
        x2:=MulDiv(x,66,1000);
        FivePointStarBasic(cp.x,cp.y,x2,clWhite);
        x1:=MulDiv(x,106,1000);
        y1:=MulDiv(y,106,1000);
        FivePointStarBasic(XL+x1,YU+y1,x2,clWhite);
        FivePointStarBasic(XR-x1,YU+y1,x2,clWhite);
        FivePointStarBasic(XL+x1,YL-y1,x2,clWhite);
        FivePointStarBasic(XR-x1,YL-y1,x2,clWhite);
        x1:=MulDiv(x,238,1000);
        y1:=MulDiv(y,238,1000);
        FivePointStarBasic(XL+x1,YU+y1,x2,clWhite);
        FivePointStarBasic(XR-x1,YU+y1,x2,clWhite);
        FivePointStarBasic(XL+x1,YL-y1,x2,clWhite);
        FivePointStarBasic(XR-x1,YL-y1,x2,clWhite);
        x1:=MulDiv(x,368,1000);
        y1:=MulDiv(y,368,1000);
        FivePointStarBasic(XL+x1,YU+y1,x2,clWhite);
        FivePointStarBasic(XR-x1,YU+y1,x2,clWhite);
        FivePointStarBasic(XL+x1,YL-y1,x2,clWhite);
        FivePointStarBasic(XR-x1,YL-y1,x2,clWhite);
  end;

begin
  case FFlag of
    flagBarotseland:DiagonalBicolorTriband(clRed,clWhite,900,600,150,150);
    flagBatasuna:begin
      RightRatioSize(3,2);
      Rectangular(XLeft,YUp,XRight,YLow,clWhite);
      x:=XRight-Xleft;
      y:=YLow-Yup;
      x1:=MulDiv(x,422,900);
      y1:=MulDiv(y,272,600);
      TwoRightTriangleDownUp(Xleft,Yup,XLeft+x1,Yup+y1,TColor($00a5ff),clRed);
      TwoRightTriangleUpDown(Xleft,YLow-y1,XLeft+x1,YLow,
        TColor($000080),TColor($800080));
      TwoRightTriangleUpDown(XRight-x1,Yup,XRight,Yup+y1,
        clYellow,clLime);
      TwoRightTriangleDownUp(XRight-x1,YLow-y1,XRight,YLow,
        TColor($008000),clBlue);
      x1:=MulDiv(x,50,900);
      y1:=MulDiv(y,33,600);
      PaintDiagonal(DDUpDown,clWhite,XLeft,YUp,XRight,YLow, x1,y1);
      PaintDiagonal(DDDownUp,clWhite,XLeft,YUp,XRight,YLow, x1,y1);
    end;
    flagBattleFlagOfTheUSConfederacy:begin
        RightRatioSize(500,500);
        BattleFlagOfTheUSConfederacy(Xleft,Yup,XRight,Ylow);
      end;
    flagConfederateNationalFlagSince1865:begin
        RightRatioSize(900,600);
        x:=XRight-Xleft;
        y:=YLow-Yup;
        x1:=MulDiv(y,420,600);
        y1:=MulDiv(y,360,600);
        x2:=MulDiv(x,660,900);
        Rectangular(XLeft,YUp,XLeft+x2,YLow,clWhite);
        Rectangular(XLeft+x2,YUp,XRight,YLow,TColor($300abf));
        BattleFlagOfTheUSConfederacy(Xleft,Yup,XLeft+x1,YUp+y1);
      end;
    flagEntreRios: DiagonalTricolorTriband(TColor($dbaa75),clWhite,
        TColor($dbaa75),TColor($3232b4),1800,900,300,225);
    flagGalicia: DiagonalBicolorTriband(clWhite,TColor($cc9900),900,600,135,90);
    flagMississippi:begin
        RightRatioSize(1000,600);
        TriColorFlagHorizontal(TColor($682800),clWhite,TColor($300abf));
        x:=XRight-Xleft;
        y:=YLow-Yup;
        x1:=MulDiv(y,390,600);
        x2:=MulDiv(y,400,600);
        y1:=MulDiv(y,390,600);
        y2:=MulDiv(y,400,600);
        if x2<=x1 then x1:=x2-1;
        if y2<=y1 then y1:=y2-1;
        Rectangular(XLeft,YUp,XLeft+x2,YUp+y2,clWhite);
        BattleFlagOfTheUSConfederacy(Xleft,Yup,XLeft+x1,YUp+y1);
    end;
    flagMonterey:begin
        RightRatioSize(3,2);
        TwoPlusTwoColor(clWhite,TColor($366f00),TColor($366f00),clWhite);
        CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
        x1:=MulDiv(x,90,900);
        y1:=MulDiv(y,60,600);
        PaintDiagonal(DDupDown,clBlack,XLeft,YUp,cp.x,cp.y, x1,y1);
        PaintDiagonal(DDDownUp,clYellow,cp.x,Yup,XRight,cp.y, x1,y1);
        PaintDiagonal(DDDownUp,clYellow,XLeft,cp.y,cp.x,Ylow, x1,y1);
        PaintDiagonal(DDupDown,clBlack,cp.x,cp.y,XRight,Ylow, x1,y1);
      end;
    flagNorthernSchleswigGermanMinority:DiagonalTricolorTriband3(
        TColor($0ae6ff),TColor($f7f1fd),TColor($942600),900,600,216,66);
    flagOmmen:
      DiagonalTricolorTriband3(clWhite,clBlue,clYellow,900,600,200,150);
    flagPara:begin
        DiagonalBicolorTriband(clRed,clWhite,1000,664,200,102);
        CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
        FivePointStarBasic(cp.x, cp.y,MulDiv(x,90,1000),TColor($d87400));
      end;
    flagRheden:
      DiagonalTricolorTriband3(clRed,clYellow,TColor($ff3000),450,300,150,100);
    flagRoraima:begin
        DiagonalTricolorTriband2(TColor($ffa500),clWhite,TColor($008200),
          900,600,230,150);
        y:=YLow-Yup;
        y1:=YUp+MulDiv(y,480,600);
        y2:=YUp+MulDiv(y,510,600);
        if y2<=y1 then y2:=y1+1;
        Rectangular(XLeft,Y1,XRight,Y2,clRed);
        y2:=YUp+MulDiv(y,75,600);
        x1:=MulDiv(x,236,900);
        FivePointStar(XLeft+x1,y2,XRight-x1,y1,TColor($00c3f8));
      end;
    flagSomiedo:
      DiagonalBicolorTriband(TColor($00cc00),TColor($ff6600),900,600,240,160);
    flagStrasbourg:DiagonalBicolorTriband(clWhite,clRed,900,600,190,105);
    flagTrinidadAndTobago:begin
      RightRatioSize(1000,600);
      Rectangular(XLeft,YUp,XRight,YLow,clRed);
      x:=XRight-Xleft;
      x1:=MulDiv(x,306,1000);
      Po1:=Point(XLeft,YUp);
      Po2:=Point(XLeft+x1,YUp);
      Po3:=Point(XRight,YLow);
      Po4:=Point(XRight-x1,YLow);
      FourCornered(Po1,Po2,Po3,Po4,clWhite);
      x1:=MulDiv(x,53,1000);
      x2:=MulDiv(x,200,1000);
      Po1:=Point(XLeft+x1,YUp);
      Po2:=Point(XLeft+x1+x2,YUp);
      Po3:=Point(XRight-x1,YLow);
      Po4:=Point(XRight-x1-x2,YLow);
      FourCornered(Po1,Po2,Po3,Po4,clBlack);
    end;
    flagVillaDeLeyva:
      DiagonalTricolorTriband2(clRed,clBlack,TColor($009900),900,600,240,90);
    flagVoortrekker:begin
      RightRatioSize(3,2);
      Rectangular(XLeft,YUp,XRight,YLow,TColor($990000));
      x1:=MulDiv(XRight-Xleft,1,9);
      y1:=YUp+MulDiv(YLow-Yup,67,600);
      PaintXDiagonals(XLeft,YUp,XRight,YLow,x1,y1,TColor($0000cc));
    end;
  end;
end;

procedure TCustomFlag.XSaltiresFlags;
var x,y,x1,y1,x2,y2,w:integer; cp:TPoint;
begin
  RightRatioSize(3,2);
  CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
  case FFlag of
    flagAmstelveen:begin
      FiveColorFlag(clRed,clBlack,clRed,clBlack,clRed);
      w:=MulDiv(x,41,900);
      x1:=MulDiv(x,183,900);
      x2:=MulDiv(x,111,900);
      y1:=MulDiv(y,65,600);
      y2:=MulDiv(y,110,600);
      XSaltires(cp.x-x1-x2,cp.y-y1-y2,cp.x-x1,cp.y-y1,w,clWhite);
      XSaltires(cp.x+x1,cp.y-y1-y2,cp.x+x1+x2,cp.y-y1,w,clWhite);
      x1:=cp.x-x2 div 2;
      x2:=x1+x2;
      XSaltires(x1,cp.y-y1-y2,x2,cp.y-y1,w,clWhite);
      XSaltires(x1,cp.y+y1,x2,cp.y+y1+y2,w,clWhite);
    end;
    flagAmsterdam: begin
      TriColorFlagHorizontal(clRed,clBlack,clRed);
      w:=MulDiv(x,60,900);
      x1:=cp.x-MulDiv(x,175,900);
      x2:=MulDiv(x,170,900);
      y1:=MulDiv(y,83,600);
      y2:=cp.y+y1;
      y1:=cp.y-y1;
      XSaltires(x1-x2,y1,x1,y2,w,clWhite);
      x1:=cp.x+MulDiv(x,175,900);
      XSaltires(x1,y1,x1+x2,y2,w,clWhite);
      x1:=cp.x-MulDiv(x,175,1800);
      XSaltires(x1,y1,x1+x2,y2,w,clWhite);
    end;
    flagBreda: begin
      Rectangular(XLeft,YUp,XRight,YLow,TColor($1527ee));
      w:=MulDiv(x,48,900);
      x1:=cp.x-MulDiv(x,140,900);
      x2:=MulDiv(x,170,900);
      y2:=MulDiv(y,170,600);
      y1:=Yup+MulDiv(y,65,600);
      XSaltires(x1-x2,y1,x1,y1+y2,w,clWhite);
      x1:=cp.x+MulDiv(x,140,900);
      XSaltires(x1,y1,x1+x2,y1+y2,w,clWhite);
      x1:=cp.x-MulDiv(x,175,1800);
      y1:=Yup+MulDiv(y,355,600);
      XSaltires(x1,y1,x1+x2,y1+y2,w,clWhite);
    end;
    flagBreukelen: begin
      TriColorFlagHorizontal(TColor($ff3100),clYellow,clRed);
      w:=MulDiv(x,40,900);
      x1:=cp.x-MulDiv(x,188,900);
      x2:=MulDiv(x,160,900);
      y1:=MulDiv(y,80,600);
      y2:=cp.y+y1;
      y1:=cp.y-y1;
      XSaltires(x1-x2,y1,x1,y2,w,TColor($ff3100));
      x1:=cp.x-MulDiv(x,80,1800);
      XSaltires(x1,y1,x1+x2,y2,w,TColor($ff3100));
    end;
  end;
end;

procedure TCustomFlag.TriagleFlags;
const clCongoGreen=TColor($009E60);
var  i,x,x1,x2,y,y1,y2:integer;
  procedure QatarSerration(x0,x1,y1,x2,y2:integer;AColor:TColor);
  var  P:array[0..20] of TPoint;
      y,i:integer;
  begin
    y:=y2-y1;
    P[0]:=point(x0,y1);    P[1]:=Point(x1,y1);
    for i:=1 to 9 do
      begin
       P[i*2]:=Point(x2,y1+round(y/9*(i-1)+y/18));
       P[i*2+1]:=Point(x1,y1+round(y/9*i));
      end;
    P[20]:=point(x0,y2);    
    Canvas.Pen.color:=AColor;
    Canvas.Brush.color:=AColor;
    Canvas.Polygon(P);
  end;
begin
  case FFlag of
    flagAbejorral: begin
      RightRatioSize(2,1);
      BiColorFlagHorizontal(clYellow,clGreen);
      Triagle(XLeft,YUp,Xleft+(XRight-XLeft) div 4,YLow,atRight,clRed);
    end;
    flagAmbtMontfoort:begin
      RightRatioSize(3,2);
      SixColorFlag(clWhite,TColor($fe0000),clWhite,TColor($fe0000),
        clWhite,TColor($fe0000));
      x:=XRight-XLeft;
      y:=YLow-YUp;
      x1:=Xleft+x div 3;
      x2:=XRight-x div 3;
      y1:=y div 3;
      Triagle(XLeft,YUp,x1,YUp+y1,atLeft,clYellow);
      Triagle(X1,YUp+y1,x2,YLow-y1,atRight,clYellow);
      Triagle(XLeft,YLow-y1,x1,YLow,atLeft,clYellow);
    end;
    flagArabRevolt:begin
      RightRatioSize(3,2);
      TriColorFlagHorizontal(TColor($303030),TColor($306030),clWhite);
      Triagle(XLeft,YUp,Xleft+(XRight-Xleft)div 3,YLow,atRight,TColor($3030C0));
    end;
    flagArapaho:begin
      RightRatioSize(1000,602);
      Rectangular(XLeft,YUp,XRight,YLow,clWhite);
      y:=YLow-YUp;
      y1:=MulDiv(y,92,602);
      Rectangular(XLeft,YUp,XRight,YUp+y1,clRed);
      Rectangular(XLeft,YLow-y1,XRight,YLow,clRed);
      y1:=MulDiv(y,185,602);
      y2:=MulDiv(y,277,602);
      Rectangular(XLeft,YUp+y1,XRight,YUp+y2,clBlack);
      Rectangular(XLeft,YLow-y2,XRight,YLow-y1,clBlack);
      x:=XRight-Xleft;
      x1:=MulDiv(x,299,1000);
      Triagle(XLeft,YUp,Xleft+x1,YLow,atRight,clBlack);
      x2:=MulDiv(x,273,1000);
      y1:=MulDiv(y,27,602);
      Triagle(XLeft,YUp+y1,Xleft+x2,YLow-y1,atRight,clWhite);
      x1:=MulDiv(x,57,1000);
      x2:=MulDiv(x,165,1000);
      y1:=MulDiv(y,241,602);
      y2:=MulDiv(y,54,602);
      HalfBall(XLeft+x1,YUp+y1,XLeft+x2,YUp+y1+y2,atUp,clRed);
      HalfBall(XLeft+x1,YLow-y1-y2,XLeft+x2,YLow-y1,atDown,clBlack);
    end;
    flagBahamas: begin
      RightRatioSize(2,1);
      TriColorFlagHorizontal(TColor($00CDC123),clYellow,TColor($00CDC123));
      Triagle(XLeft,YUp,Xleft+(XRight-Xleft) div 5*2,YLow,atRight,clBlack);
    end;
    flagBahia:begin
      RightRatioSize(700,500);
      QuadColorFlag(clWhite,clRed,clWhite,clRed);
      y:=YLow-YUp;
      y1:=y div 2;
      x:=y1;
      Rectangular(XLeft,YUp,XLeft+x,YUp+y1,TColor($c8140a));
      x1:=MulDiv(x,26,250);
      x2:=MulDiv(x,224,250);
      y1:=MulDiv(y,35,500)+Yup;
      y2:=MulDiv(y,204,500)+Yup;
      Triagle(XLeft+x1,Y1,Xleft+x2,Y2,atUp,clWhite);
    end;
    flagBahrain: begin
      RightRatioSize(5,3);
      Rectangular(XLeft,YUp,XRight,YLow,clRed);
      x1:=Xleft+(XRight-Xleft) div 4;
      Rectangular(XLeft,YUp,x1,YLow,clWhite);
      x2:=round(Xleft+(XRight-Xleft)/100*35);
      Triagle(X1,YUp,x2,YUp+(YLow-YUp)div 5,atRight,clWhite);
      Triagle(X1,YUp+(YLow-YUp)div 5,x2,YUp+(YLow-YUp)div 5*2,atRight,clWhite);
      Triagle(X1,YUp+(YLow-YUp)div 5*2,x2,YLow-(YLow-YUp)div 5*2,atRight,clWhite);
      Triagle(X1,YLow-(YLow-YUp)div 5*2,x2,YLow-(YLow-YUp)div 5,atRight,clWhite);
      Triagle(X1,YLow-(YLow-YUp)div 5,x2,YLow,atRight,clWhite);
    end;
    flagChukotka:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,clBlue);
      x:=XRight-XLeft;
      Triagle(XLeft,YUp,Xleft+MulDiv(x,518,900),YLow,atRight,clWhite);
      x1:=XLeft+MulDiv(x,172,900);
      y:=YLow-YUp;
      y1:=Yup+ y div 2;
      y2:=MulDiv(y,75,600);
      Ball(x1,y1,MulDiv(y,124,600),TColor($00d7ff));
      Ball(x1,y1,y2,clBlue);
      x1:=XLeft+MulDiv(x,102,900);
      x2:=XLeft+MulDiv(x,241,900);
      i:=MulDiv(y,275,600);
      HalfBall(x1,y1-y2,x2,YUp+i,atUp,clWhite);
      HalfBall(x1,YLow-i,x2,Y1+y2,atDown,clRed);
    end;
    flagCongo:begin
      RightRatioSize(6,4);
      Rectangular(Xleft,Yup,XRight,Ylow,clYellow);
      x1:=XRight-XLeft;
      Triangular(XLeft,Yup,XLeft+MulDiv((x1),4,6),Ylow,pi,1.0,clCongoGreen);
      Triangular(XLeft+MulDiv((x1),2,6),Yup,XRight,Ylow,0.0,1.0,clRed);
    end;
    flagCzech: begin
      RightRatioSize(3,2);
      BiColorFlagHorizontal(clWhite,clRed);
      Triagle(XLeft,YUp,Xleft+(XRight-Xleft) div 2,YLow,atRight,clBlue);
    end;
    flagDruzepeople:begin
      RightRatioSize(3,2);
      QuadColorFlag(clRed,clYellow,clBlue,clWhite);
      Triagle(XLeft,YUp,Xleft+(XRight-Xleft)div 3,YLow,atRight,TColor($00CC00));
    end;
    flagJamaica:begin
      RightRatioSize(120,60);
      Rectangular(Xleft,Yup,XRight,Ylow,clYellow);
      x1:=XRight-XLeft;
      y1:=Ylow-Yup;
      x2:=DistCorner(x1,y1,y1,x1*5/120);
      y2:=DistCorner(x1,y1,x1,x1*5/120);
      y1:=y1 div 2;
      x1:=x1 div 2;
      Triagle(XLeft+x2,Yup, XRight-x2,Yup+y1-y2, atDown,clGreen);
      Triagle(XLeft+x2,YLow,XRight-x2,YLow-y1+y2,atUp,  clGreen);
      Triagle(XLeft,YUp+y2,XLeft+x1-x2,Ylow-y2,atRight,clBlack);
      Triagle(XRight,Ylow-y2,XRight-x1+x2,YUp+y2,atLeft,clBlack);
    end;
    flagKashmirIndependent:begin
      RightRatioSize(3,2);
      BiColorFlagHorizontal(TColor($00570c),TColor($0000e8));
      Triagle(XLeft,YUp,Xleft+(XRight-Xleft) div 2,YLow,atRight,clWhite);
    end;
    flagKhabarovskKrai:begin
      RightRatioSize(3,2);
      BiColorFlagHorizontal(clWhite,TColor($ff9900));
      Triagle(XLeft,YUp,Xleft+(XRight-Xleft) div 3,YLow,atRight,clGreen);
    end;
    flagMeerloWanssum:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,clWhite);
      y:=Ylow-Yup;
      x1:=Xleft+MulDiv(XRight-XLeft,150,450);
      y1:=y div 3;
      Triangular(XLeft,YUp,x1,YUp+y1,1.5*pi,0.5,clRed);
      Triangular(XLeft,YUp+y1,x1,YLow-y1,1.5*pi,0.5,clRed);
      Triangular(XLeft,YLow-y1,x1,YLow,1.5*pi,0.5,clRed);
    end;
    flagMerida:begin
      RightRatioSize(3,2);
      BiColorFlagVertical(TColor($46512b),TColor($ad7050));
      Triagle(XLeft,YUp,XRight,YLow,atUp,clWhite);
      x1:=XLeft+MulDiv(XRight-XLeft,450,900);
      y:=Ylow-Yup;
      y1:=YUp+MulDiv(y,320,600);
      FivePointStarBasic(x1,y1,MulDiv(y,145,600),TColor($2d0cba));
    end;
    flagMuiden:begin
      RightRatioSize(3,2);
      TriColorFlagHorizontal(clBlue,clWhite,clBlue);
      Triangular(XLeft,YUp,Xleft+(XRight-Xleft) div 2,YLow,1.5*pi,0.5,clYellow);
    end;
    flagNataliaRepublic:begin
      RightRatioSize(3,2);
      BiColorFlagHorizontal(clRed,clNavy);
      Triagle(XLeft,YUp,XRight,YLow,atLeft,clWhite);
    end;
    flagOxapampa: begin
      RightRatioSize(5,4);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($009900));
      Triagle(XLeft,YUp,XRight,YLow,atRight,clWhite);
      Triagle(XLeft,YUp,Xleft+MulDiv(XRight-Xleft,4,10),YLow,
        atRight,TColor($993300));
    end;
    flagPalestine: begin
      RightRatioSize(2,1);
      TriColorFlagHorizontal(clBlack,clWhite,clGreen);
      Triagle(XLeft,YUp,Xleft+(XRight-Xleft) div 5,YLow,atRight,clRed);
    end;
    flagPasto:begin
      RightRatioSize(2,1);
      QuadColorFlag(clRed,clNavy,clNavy,clRed);
      Triagle(XLeft,YUp,Xleft+(XRight-Xleft) div 4,YLow,atRight,clYellow);
    end;
    flagQatar:begin
      RightRatioSize(252,99);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($3d1970));
      x2:=XRight-XLeft;
      x1:=MulDiv(x2,72,252)+XLeft;
      QatarSerration(XLeft,x1,yup,XLeft+MulDiv(x2,90,252)+XLeft,Ylow,clwhite);
    end;
    flagRijnwaarden:begin
      RightRatioSize(3,2);
      TriColorFlagHorizontal(clYellow,clRed,clWhite);
      Triangular(XLeft,YUp,Xleft+(XRight-Xleft) div 2,YLow,1.5*pi,0.5,clBlue);
    end;
    flagSaintLucia:begin
      RightRatioSize(100,50);
      Rectangular(Xleft,Yup,XRight,Ylow,clBlue);
      x1:=XRight-XLeft;
      x2:=MulDiv(x1,32,100);
      y1:=YLow-YUp;
      y2:=YLow-MulDiv(y1,5,50);
      Triagle(XLeft+x2,YUp+MulDiv(y1,5,50),XRight-x2,Y2,atUp,clWhite);
      x1:=MulDiv(x1,34,100);
      Triagle(XLeft+x1,YUp+MulDiv(y1,10,50),XRight-x1,Y2,atUp,clBlack);
      Triagle(XLeft+x2,YUp+MulDiv(y1,25,50),XRight-x2,Y2,atUp,TColor($00C0ff));
    end;
    flagSaintMartin:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($800000));
      x1:=XRight-XLeft;
      y1:=YLow-YUp;
      x2:=MulDiv(x1,427,900);
      Rectangular(Xleft+x2,Yup,XRight-x2,Ylow,clWhite);
      Triagle(XLeft,YUp,XRight,YUp+MulDiv(y1,450,600),atDown,clWhite);
      x2:=MulDiv(x1,318,900);
      y2:=MulDiv(y1,274,600);
      Triagle(XLeft+x2,YUp+y2,XRight-x2,YUp+MulDiv(y1,399,600),atDown,clRed);
      x2:=MulDiv(x1,382,900);
      y2:=YUp+MulDiv(y1,179,600);
      y1:=YUp+MulDiv(y1,247,600);
      HalfBall(XLeft+x2,y2,XRight-x2,y1,atUp,TColor($00ddff));
    end;
    flagSealand:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,clWhite);
      x1:=XRight-XLeft;
      y1:=YLow-YUp;
      Triangular(XLeft,Yup,XLeft+MulDiv(x1,97,100),Yup+MulDiv(y1,93,100),
        pi,1.0,clRed);
      Triangular(XRight-MulDiv(x1,68,100),Ylow-MulDiv((Ylow-Yup),63,100),
        XRight,Ylow,0.0,1.0,clBlack);
    end;
    flagSeychelles:begin
      RightRatioSize(6,4);
      Rectangular(Xleft,Yup,XRight,Ylow,clRed);
      x1:=XRight-XLeft;
      Triangular(XLeft,Yup,XLeft+MulDiv(x1,4,6),Ylow,pi,1.0,clYellow);
      Triangular(XLeft,Yup,XLeft+MulDiv(x1,2,6),Ylow,pi,1.0,clNavy);
      Triangular(XLeft,Ylow-MulDiv((Ylow-Yup),2,3),XRight,Ylow,0.0,1.0,clWhite);
      Triangular(XLeft,Yup+MulDiv((Ylow-Yup),2,3),XRight,Ylow,0.0,1.0,clGreen);
    end;
    flagSognOgFjordane:begin
      RightRatioSize(6,4);
      y1:=MulDiv(Ylow-Yup,1,3);
      Rectangular(Xleft,Yup,XRight,Ylow,clWhite);
      Triagle(XLeft,YUp,XRight,Yup+y1,atRight,TColor($ff9933));
      Triagle(XLeft,YUp+y1,XRight,YLow-y1,atRight,TColor($ff9933));
      Triagle(XLeft,YLow-y1,XRight,YLow,atRight,TColor($ff9933));
    end;
    flagSudan: begin
      RightRatioSize(2,1);
      TriColorFlagHorizontal(clRed,clWhite,clBlack);
      Triagle(XLeft,YUp,Xleft+(XRight-Xleft) div 4,YLow,atRight,clGreen);
    end;
    flagTanzania:begin
      RightRatioSize(144,96);
      Rectangular(Xleft,Yup,XRight,Ylow,clBlack);
      x1:=XRight-XLeft;
      y1:=Ylow-Yup;
      Triangular(XLeft,Yup,XRight-DistCorner(x1,y1,y1,x1*13/144),
        Ylow-DistCorner(x1,y1,x1,x1*13/144),pi,1.0,clYellow);
      Triangular(XLeft,Yup,XRight-DistCorner(x1,y1,y1,x1*19/144),
        Ylow-DistCorner(x1,y1,x1,x1*19/144),pi,1.0,clGreen);
      Triangular(XLeft+DistCorner(x1,y1,y1,x1*13/144),
        Yup+DistCorner(x1,y1,x1,x1*13/144), XRight,Ylow,0.0,1.0,clYellow);
      Triangular(XLeft+DistCorner(x1,y1,y1,x1*19/144),
        Yup+DistCorner(x1,y1,x1,x1*19/144), XRight,Ylow,0.0,1.0,clBlue);
    end;
    flagTxita:begin
      RightRatioSize(3,2);
      BiColorFlagHorizontal(TColor($11bb0a),clRed);
      Triagle(XLeft,YUp,Xleft+(XRight-Xleft)div 2,YLow,atRight,clYellow);
    end;
    flagYamagata:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($bd6b00));
      x:=XRight-XLeft;
      y:=Ylow-Yup;
      x1:=MulDiv(x,173,900);
      x2:=MulDiv(x,180,900);
      y1:=YUp+MulDiv(y,206,600);
      y2:=YUp+MulDiv(y,390,600);
      i:=MulDiv(x,195,900);
      Triagle(XLeft+x1,Y1,XLeft+x1+i,Y2,atUp,clWhite);
      x1:=x1+x2;
      Triagle(XLeft+x1,Y1,XLeft+x1+i,Y2,atUp,clWhite);
      x1:=x1+x2;
      Triagle(XLeft+x1,Y1,XLeft+x1+i,Y2,atUp,clWhite);
      x1:=MulDiv(x,208,900) ;
      i:=MulDiv(x,57,900);
      y1:=YUp+MulDiv(y,343,600);
      if i>1 then begin
        Triagle(XLeft+x1,Y1,XLeft+x1+i,Y2,atUp,TColor($bd6b00));
        x1:=x1+x2;
        Triagle(XLeft+x1,Y1,XLeft+x1+i,Y2,atUp,TColor($bd6b00));
        x1:=x1+x2;
        Triagle(XLeft+x1,Y1,XLeft+x1+i,Y2,atUp,TColor($bd6b00));
      end;
    end;
  end;
end;

procedure TCustomFlag.BallFlags;
var YHeight:integer;
    xr,yr:real;
    x,y,x1,x2,x3,y1,y2:integer;
    cp:TPoint;
begin
  case FFlag of
    flagAboriginal:begin
      RightRatioSize(3,2);
      BiColorFlagHorizontal(clBlack,clRed);
      YHeight:=(YLow-YUp) div 2;
      Ball(XLeft+(XRight-XLeft) div 2,YUp+YHeight,YHeight div 2,clYellow);
    end;
    flagBangladesh:begin
      RightRatioSize(5,3);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($00336200));
      YHeight:=(YLow-YUp) div 2;
      Ball(XLeft+round((XRight-XLeft)/20*9),YUp+YHeight,YHeight div 3 * 2,clRed);
    end;
    flagBlackFlagWithOrangeCircle:begin
      RightRatioSize(4,3);
      Rectangular(Xleft,Yup,XRight,Ylow,clBlack);
      cp:=CenterPoint(Rect(Xleft,Yup,XRight,Ylow));
      Ball(cp.x,cp.y,MulDiv(257,YLow-YUp,750),TColor($0088ff));
    end;
    flagChin:begin
      RightRatioSize(3,2);
      BiColorFlagHorizontal(clWhite,clNavy);
      YHeight:=(YLow-YUp) div 2;
      Ball(XLeft+(XRight-XLeft) div 2,YUp+YHeight,YHeight div 2,clRed);
    end;
    flagColorado:begin
      RightRatioSize(3,2);
      YHeight:= (YLow-YUp);
      x1:=XLeft+(XRight-XLeft) div 3;
      TriColorFlagHorizontal(TColor($00CF713C),clWhite,TColor($00CF713C));
      y1:=MulDiv(YHeight,2,6);
      Ball(x1,YUp+YHeight div 2,y1,clRed);
      y1:=MulDiv(YHeight,15,40);
      Triagle(x1,YUp+y1,Xleft+MulDiv(XRight-XLeft,59,100),YLow-y1,atLeft,clWhite);
      Ball(x1,YUp+YHeight div 2,MulDiv(YHeight,1,6),clYellow);
    end;
    flagDukeOfCornwall:begin
      RightRatioSize(5,3);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      Rectangular(Xleft,Yup,XRight,Ylow,clBlack);
      y1:=YUp+MulDiv(y,92,600);
      y2:=MulDiv(y,37,600);
      Ball(cp.x,y1,y2,TColor($17d9f7));
      x1:=MulDiv(x,120,1000);
      Ball(cp.x-x1,y1,y2,TColor($17d9f7));
      Ball(cp.x+x1,y1,y2,TColor($17d9f7));
      x2:=MulDiv(x,240,1000);
      Ball(cp.x-x2,y1,y2,TColor($17d9f7));
      Ball(cp.x+x2,y1,y2,TColor($17d9f7));
      y1:=YUp+MulDiv(y,288,600);
      y2:=MulDiv(y,42,600);
      Ball(cp.x,y1,y2,TColor($17d9f7));
      Ball(cp.x-x1,y1,y2,TColor($17d9f7));
      Ball(cp.x+x1,y1,y2,TColor($17d9f7));
      y1:=YUp+MulDiv(y,186,600);
      y2:=MulDiv(y,39,600);
      x1:=MulDiv(x,60,1000);
      Ball(cp.x-x1,y1,y2,TColor($17d9f7));
      Ball(cp.x+x1,y1,y2,TColor($17d9f7));
      x2:=MulDiv(x,180,1000);
      Ball(cp.x-x2,y1,y2,TColor($17d9f7));
      Ball(cp.x+x2,y1,y2,TColor($17d9f7));
      y1:=YUp+MulDiv(y,395,600);
      y2:=MulDiv(y,45,600);
      x1:=MulDiv(x,61,1000);
      Ball(cp.x-x1,y1,y2,TColor($17d9f7));
      Ball(cp.x+x1,y1,y2,TColor($17d9f7));
      Ball(cp.x,YUp+MulDiv(y,500,600),MulDiv(y,47,600),TColor($17d9f7));
    end;
    flagGreenland:begin
      RightRatioSize(18,12);
      BiColorFlagHorizontal(clWhite,clRed);
      HalfBall(XLeft+round((XRight-XLeft)/18*3),YUp+(YLow-YUp)div 6,
        XLeft+round((XRight-XLeft)/18*11),YUp+(YLow-YUp)div 2,atUp,clRed);
      HalfBall(XLeft+round((XRight-XLeft)/18*3),YUp+(YLow-YUp)div 2,
        XLeft+round((XRight-XLeft)/18*11),YLow-(YLow-YUp)div 6,atDown,clWhite);
    end;
    flagHiroshima:begin
      RightRatioSize(3,2);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($4a00b5));
      Ball(cp.x,cp.y,MulDiv(y,185,600),clWhite);
      Ball(Xleft+MulDiv(y,500,600),Yup+MulDiv(y,270,600),
        MulDiv(y,145,600),TColor($4a00b5));
      Ball(Xleft+MulDiv(y,470,600),Yup+MulDiv(y,255,600),
        MulDiv(y,140,600),clWhite);
      Ball(Xleft+MulDiv(y,505,600),Yup+MulDiv(y,224,600),
        MulDiv(y,103,600),TColor($4a00b5));
    end;
    flagInNizam:begin
      RightRatioSize(450,300);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($00c4f8));
      Ball(cp.x,cp.y,MulDiv(y,72,300),clWhite);
    end;
    flagJapan:begin
      RightRatioSize(10,7);
      Rectangular(Xleft,Yup,XRight,Ylow,clWhite);
      YHeight:=(YLow-YUp) div 2;
      Ball(XLeft+(XRight-XLeft) div 2,YUp+YHeight,YHeight div 2,clRed);
    end;
    flagLaos:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,YUp,XRight,YLow,clRed);
      YHeight:=(YLow-YUp) div 4;
      Rectangular(Xleft,YUp+YHeight,XRight,YLow-YHeight,clBlue);
      YHeight:=(YLow-YUp) div 2;
      Ball(XLeft+(XRight-XLeft) div 2,YUp+YHeight,YHeight div 3,clWhite);
    end;
    flagMacedonia:begin
      RightRatioSize(40,20);
      Rectangular(Xleft,YUp,XRight,YLow,clYellow);
      x1:=(XRight-XLeft) div 2 + XLeft;
      Triagle(Xleft,YUp,x1,YLow,atRight,clRed);
      Triagle(XRight,YUp,x1,YLow,atLeft,clRed);
      y1:=MulDiv((YLow-YUp),8,20);
      Triagle(Xleft,YUp+y1,x1,YLow-y1,atRight,clYellow);
      Triagle(XRight,YUp+y1,x1,YLow-y1,atLeft,clYellow);
      x2:=MulDiv(XRight-XLeft,6,40);
      y2:=(YLow-YUp) div 2 + YUp;
      Triagle(Xleft+x2,YUp,XRight-x2,Y2,atDown,clRed);
      Triagle(Xleft+x2,Y2,XRight-x2,YLow,atUp,clRed);
      x2:=MulDiv(XRight-XLeft,18,40);
      Triagle(Xleft+x2,YUp,XRight-x2,Y2,atDown,clYellow);
      Triagle(Xleft+x2,Y2,XRight-x2,YLow,atUp,clYellow);
      Ball(x1,y2,MulDiv(XRight-XLeft,7,80),clRed);
      Ball(x1,y2,MulDiv(XRight-XLeft,6,80),clYellow);
    end;
    flagMaori:begin
      RightRatioSize(1080,600);
      x:=XRight-XLeft;
      y:=Ylow-Yup;
      x1:=MulDiv(x,300,1080);
      y2:=MulDiv(y,350,600);
      Rectangular(Xleft,YUp,XRight,YUp+y2,clBlack);
      x3:=MulDiv(y,460,600);
      Rectangular(Xleft,YUp+y2,XRight,YUp+x3,clWhite);
      Rectangular(Xleft,YUp+x3,XRight,YLow,clRed);
      y1:=MulDiv(y,80,600);
      Rectangular(Xleft,YUp+y1,XLeft+x1,YUp+MulDiv(y,190,600),clWhite);
      Rectangular(Xleft,YUp+MulDiv(y,190,600),XLeft+x1,YUp+x3,clRed);
      HalfBall(XLeft+x1,YUp+y1,Xleft+MulDiv(x,435,1080),YUp+y2,atRight,clWhite);
      HalfBall(XLeft+x1,YUp+MulDiv(y,190,600),Xleft+MulDiv(x,380,1080),YUp+y2,atRight,clRed);
      y1:=MulDiv(y,240,600);
      HalfBall(XLeft+x1,YUp+y1,Xleft+MulDiv(x,354,1080),YUp+y2,atRight,clWhite);
      HalfBall(XLeft+MulDiv(x,190,1080),YUp+y1,Xleft+x1,YUp+x3,atLeft,clWhite);
    end;
    flagMizoram:begin
      RightRatioSize(3,2);
      BiColorFlagHorizontal(clWhite,clNavy);
      YHeight:=(YLow-YUp) div 2;
      Ball(XLeft+(XRight-XLeft) div 2,YUp+YHeight,YHeight div 2,clRed);
    end;
    flagNagano:begin
      RightRatioSize(3,2);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($008cef));
      x3:=MulDiv(y,213,600);
      Ball(Xleft+MulDiv(x,320,900),cp.y,x3,clWhite);
      Ball(Xleft+MulDiv(x,320,900),cp.y,x3,clWhite);
      x2:=Xleft+MulDiv(x,215,900);
      y1:=MulDiv(y,225,600);
      HalfBall(X2,cp.y-y1,
        Xleft+MulDiv(x,445,900),cp.y+y1,atRight,TColor($008cef));
      y2:=MulDiv(y,185,600);
      HalfBall(X2,cp.y-y2,Xleft+MulDiv(x,397,900),cp.y+y2,atRight,clWhite);
      y2:=MulDiv(y,25,600);
      if y2<= 1 then y2:=1;
      Rectangular(Xleft,cp.y-y2,XRight,cp.y+y2,TColor($008cef));
    end;
    flagNuevaEsparta:begin
      RightRatioSize(3,2);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      QuadColorFlag(TColor($18d6ff),TColor($18d6ff),TColor($298c21),
        TColor($ad3901));
      y1:=YUp+MulDiv(y,150,600);
      x2:=Xleft+MulDiv(x,750,900);
      HalfBall(cp.x,y1,x2,cp.y,atUp,clWhite);
      y2:=YUp+MulDiv(y,375,600);
      x3:=MulDiv(x,58,900);
      FivePointStarBasic(XLeft+MulDiv(x,150,900),y2,x3,clWhite);
      FivePointStarBasic(cp.x,y2,x3,clWhite);
      FivePointStarBasic(x2,y2,x3,clWhite);
    end;
    flagNiger:begin
      RightRatioSize(3,2);
      TriColorFlagHorizontal(TColor($00007CE3),clWhite,clGreen);
      YHeight:=(YLow-YUp) div 2;
      Ball(XLeft+(XRight-XLeft)div 2,YUp+YHeight,YHeight div 4,TColor($00007CE3));
    end;
    flagOceanCity:begin
      RightRatioSize(1000,600);
      Rectangular(Xleft,Yup,XRight,Ylow,clWhite);
      x:=XRight-XLeft;
      y:=Ylow-Yup;
      x1:=MulDiv(x,470,1000);
      y1:=MulDiv(y,194,600);
      Rectangular(Xleft,Yup+y1,Xleft+x1,Ylow,TColor($ffa510));
      x3:=MulDiv(x,101,1000);
      x1:=MulDiv(x,720,1000);
      y2:=MulDiv(y,345,600);
      Ball(XLeft+x1,YUp+y2,MulDiv(x,150,1000),TColor($ffa510));
      x1:=MulDiv(x,440,1000);
      Ball(XLeft+x1,YUp+y2,MulDiv(x,150,1000),TColor($ffa510));
      y1:=MulDiv(y,485,600);
      Rectangular(Xleft,Yup+y1,XRight,Ylow,TColor($ffa510));
      x1:=MulDiv(x,800,1000);
      Rectangular(Xleft,Yup+y2,Xleft+x1,Ylow,TColor($ffa510));
      y1:=MulDiv(y,444,600);
      y2:=MulDiv(y,526,600);
      Triagle(XLeft+x1,YUp+y1,XRight,YUp+y2,atLeft,TColor($ffa510));
      x1:=MulDiv(x,413,1000);
      y1:=MulDiv(y,376,600);
      Ball(XLeft+x1,YUp+y1,x3,clWhite);
      y1:=MulDiv(y,380,600);
      x2:=MulDiv(x,763,1000);
      Ball(XLeft+x2,YUp+y1,x3,clWhite);
    end;
    flagOkinawa:begin
      RightRatioSize(3,2);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      Rectangular(Xleft,Yup,XRight,Ylow,clWhite);
      Ball(cp.x,cp.y,MulDiv(y,180,600),TColor($2611ce));
      Ball(cp.x,Yup+MulDiv(y,265,600),MulDiv(y,135,600),clWhite);
      Ball(cp.x,Yup+MulDiv(y,240,600),MulDiv(y,80,600),TColor($2611ce));
    end;
    flagOsaka:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,clNavy);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      y1:=MulDiv(y,485,1333);
      x3:=MulDiv(x,155,2000);
      Ball(cp.x,YUp+Y1,x3,clWhite);
      y2:=MulDiv(y,843,1333)+YUp;
      Ball(cp.x,Y2,x3,clWhite);
      x1:=MulDiv(x,315,2000);
      Ball(cp.x-x1,cp.y,x3,clWhite);
      Ball(cp.x+x1,cp.y,x3,clWhite);
      x2:=MulDiv(x,70,2000);
      if x>40 then
        begin
          Rectangular(cp.x-x2,Yup+y1,cp.x+x2,y2,clWhite);
          Rectangular(cp.x-x1,cp.y,cp.x+x1,cp.y+MulDiv(y,164,1333),clWhite);
        end;
      x2:=MulDiv(x,53,2000);
      Ball(cp.x-x1,cp.y,x2,clNavy);
      Ball(cp.x+x1,cp.y,x2,clNavy);
      Ball(cp.x,Y2,x2,clNavy);
      Ball(cp.x,YUp+Y1,x2,clNavy);
      x1:=MulDiv(x,105,2000);
      Ball(cp.x-x1,cp.y,x2,clNavy);
      Ball(cp.x+x1,cp.y,x2,clNavy);
      x1:=MulDiv(x,204,2000);
      Ball(cp.x-x1,y2,x2,clNavy);
      Ball(cp.x+x1,y2,x2,clNavy);
    end;
    flagPalau:begin
      RightRatioSize(8,5);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($00CFA500));
      YHeight:=(YLow-YUp) div 2;
      Ball(XLeft+(XRight-XLeft) div 8*3,YUp+YHeight,YHeight div 5*3,clYellow);
    end;
    FlagPalmyraAtoll:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($2611ce));
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      Ball(cp.x,YUp+MulDiv(y,250,600),MulDiv(y,225,600),TColor($16d1fc));
      Rectangular(Xleft,cp.y,XRight,Ylow,TColor($8c1008));
      Rectangular(Xleft,YUp+MulDiv(y,500,600),XRight,Ylow,TColor($16d1fc));
    end;
    flagSakha:begin
      RightRatioSize(600,300);
      Rectangular(Xleft,Yup,XRight,Ylow,clAqua);
      YHeight:=YLow-YUp;
      Ball(XLeft+ (XRight-XLeft) div 2,YUp+MulDiv(YHeight,113,300),
      MulDiv(YHeight,55,300),clWhite);
      y1:=YUp+MulDiv(YHeight,226,300);
      y2:=YUp+MulDiv(YHeight,244,300);
      Rectangular(Xleft,y1,XRight,Y2,clWhite);
      y1:=YUp+MulDiv(YHeight,262,300);
      Rectangular(Xleft,y2,XRight,Y1,clRed);
      Rectangular(Xleft,y1,XRight,YLow,clGreen);
    end;
    flagSami:begin
      RightRatioSize(404,300);
      xr:=(xright-xleft)/404;
      x1:=round(129*xr)+XLeft;
      Rectangular(xLeft,yup,x1,ylow,clRed);
      x2:=round(28*xr)+X1;
      Rectangular(x1,yup,x2,ylow,clGreen);
      x3:=round(28*xr)+X2;
      Rectangular(x2,yup,x3,ylow,clyellow);
      Rectangular(x3,yup,xright,ylow,clBlue);
      yr:=(YLow-YUp)/300;
      y1:=Yup+round(yr*54);
      y2:=YLow-round(yr*54);
      HalfBall(x2-round(96*xr),Y1,X2,Y2,atLeft,clBlue);
      HalfBall(X2,Y1,round(96*xr)+X2,Y2,atRight,clRed);
      y1:=Yup+round(yr*70);
      y2:=YLow-round(yr*70);
      HalfBall(x2-round(80*xr),Y1,X2,Y2,atLeft,clGreen);
      HalfBall(X2,Y1,round(80*xr)+X2,Y2,atRight,clyellow);
      y1:=Yup+round(yr*75.06);
      y2:=YLow-round(yr*75.06);
      HalfBall(x1-round(52*xr),Y1,X1,Y2,atLeft,clRed);
      HalfBall(X3,Y1,round(52*xr)+X3,Y2,atRight,clBlue);
    end;
  end;
end;

procedure TCustomFlag.StarFlags;
var i,j,h,hw,w,y,x:integer; cp:TPoint;
begin
  case FFlag of
    flagAcardia:begin
      RightRatioSize(3,2);
      TriColorFlagVertical(clNavy, clWhite, clRed);
      y:=YLow-YUp;
      x:=XRight-Xleft;
      i:=MulDiv(x,66,900);
      j:=MulDiv(y,51,600);
      h:=MulDiv(y,192,600);
      FivePointStar(XLeft+i,YUp+j,XLeft+MulDiv(x,222,900),YUp+h,clYellow);
    end;
    flagAcre:begin
      RightRatioSize(700,500);
      TwoRightTriangleUpDown(Xleft,Yup,XRight,Ylow,clYellow,TColor($4c9900));
      y:=YLow-YUp;
      x:=XRight-Xleft;
      i:=MulDiv(x,43,700);
      j:=MulDiv(y,40,500);
      h:=MulDiv(y,128,500);
      FivePointStar(XLeft+i,YUp+j,XLeft+MulDiv(x,136,700),YUp+h,clRed);
    end;
    flagAlaska:begin
      RightRatioSize(304,216);
      Rectangular(Xleft,Yup,XRight,Ylow,clNavy);
      x:=XRight-Xleft;
      y:=YLow-YUp;
      FivePointStar(XLeft+MulDiv(x,44,304),YUp+MulDiv(y,78,216),
        XLeft+MulDiv(x,60,304),YUp+MulDiv(y,94,216),clYellow);
      FivePointStar(XLeft+MulDiv(x,78,304),YUp+MulDiv(y,90,216),
        XLeft+MulDiv(x,92,304),YUp+MulDiv(y,106,216),clYellow);
      FivePointStar(XLeft+MulDiv(x,98,304),YUp+MulDiv(y,110,216),
        XLeft+MulDiv(x,114,304),YUp+MulDiv(y,126,216),clYellow);
      FivePointStar(XLeft+MulDiv(x,120,304),YUp+MulDiv(y,130,216),
        XLeft+MulDiv(x,136,304),YUp+MulDiv(y,146,216),clYellow);
      FivePointStar(XLeft+MulDiv(x,114,304),YUp+MulDiv(y,162,216),
        XLeft+MulDiv(x,130,304),YUp+MulDiv(y,178,216),clYellow);
      FivePointStar(XLeft+MulDiv(x,158,304),YUp+MulDiv(y,178,216),
        XLeft+MulDiv(x,174,304),YUp+MulDiv(y,194,216),clYellow);
      FivePointStar(XLeft+MulDiv(x,174,304),YUp+MulDiv(y,150,216),
        XLeft+MulDiv(x,190,304),YUp+MulDiv(y,166,216),clYellow);
      FivePointStar(XLeft+MulDiv(x,240,304),YUp+MulDiv(y,22,216),
        XLeft+MulDiv(x,282,304),YUp+MulDiv(y,62,216),clYellow);
    end;
    flagAntiguaAndBarbuda:begin
      RightRatioSize(138,92);
      Rectangular(Xleft,Yup,XRight,Ylow,clRed);
      Triagle(XLeft,YUp,XRight,YLow,atDown,clBlack);
      y:=MulDiv(YLow-Yup,36,92)+Yup;
      x:=(XRight-XLeft) div 2+XLeft;
      i:=MulDiv(YLow-Yup,30,92);
      PaintNPointStar(x,y,i,16,clYellow);
      x:=MulDiv(XRight-XLeft,105,552);
      Triagle(XLeft+x,y,XRight-x,YLow,atDown,clBlue);
      y:=MulDiv(YLow-Yup,56,92)+Yup;
      x:=MulDiv(XRight-XLeft,83,276);
      Triagle(XLeft+x,y,XRight-x,YLow,atDown,clWhite);
    end;
    flagAruba:begin
      RightRatioSize(54,36);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($00C67200));
      y:=MulDiv(YLow-Yup,26,36)+Yup;
      i:=MulDiv(YLow-Yup,2,36);
      Rectangular(Xleft,y-i,XRight,y,clYellow);
      y:=MulDiv(YLow-Yup,30,36)+Yup;
      Rectangular(Xleft,y-i,XRight,y,clYellow);
      y:=MulDiv(YLow-Yup,8,36)+Yup;
      x:=MulDiv(XRight-XLeft,8,54)+XLeft;
      i:=MulDiv(YLow-Yup,6,36);
      PaintNPointStar(x,y,i,4,clWhite);
      i:=MulDiv(YLow-Yup,5,36);
      PaintNPointStar(x,y,i,4,clRed);
    end;
    flagAtacama:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,clNavy);
      i:=(YLow-YUp)div 2;
      hw :=round(i/1.5);
      FivePointStar((XLeft+(XRight-XLeft) div 2)-hw, YUp+i-hw,
                    (XLeft+(XRight-XLeft) div 2)+hw, YUp+i+hw ,clYellow);
    end;
    flagAzerbaijan:begin
      RightRatioSize(240,120);
      TriColorFlagHorizontal(TColor($00B59900),clRed,clGreen);
      y:=(YLow-YUp)div 2+Yup;
      x:=XRight-XLeft;
      Ball(XLeft+MulDiv(x,114,240),Y,MulDiv(x,18,240),clWhite);
      Ball(XLeft+MulDiv(x,118,240),Y,MulDiv(x,15,240),clRed);
      PaintNPointStar(XLeft+MulDiv(x,134,240),y,MulDiv(x,10,240),8,clWhite);
    end;
    flagBlueEstelada,flagRedEstelada:begin
      RightRatioSize(3,2);
      NineColorFlag(TColor($0ee8fb),TColor($0d00db),TColor($0ee8fb),
        TColor($0d00db),TColor($0ee8fb),TColor($0d00db),TColor($0ee8fb),
        TColor($0d00db),TColor($0ee8fb));
      x:=XRight-XLeft;
      i:=XLeft+ MulDiv(x,300,900);
      hw:=MulDiv(x,85,900);
      x:=XLeft+ MulDiv(x,130,900);
      y:=(YLow-YUp)div 2+Yup;
      if fflag = flagRedEstelada then
         begin
           Triagle(XLeft,YUp,i,YLow,atRight,TColor($0ee8fb));
           FivePointStarBasic(x,y,hw,TColor($0d00db));
         end else begin
           Triagle(XLeft,YUp,i,YLow,atRight,TColor($b5570a));
           FivePointStarBasic(x,y,hw,clWhite);
         end;
    end;
    flagBurkinaFaso:begin
      RightRatioSize(3,2);
      BiColorFlagHorizontal(clRed,clGreen);
      FivePointStarBasic((XLeft+(XRight-XLeft) div 2), (YUp+(YLow-YUp)div 2),
                        ((YLow-YUp)div 5),clYellow);
    end;
    flagBurundi:begin
      RightRatioSize(3,2);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      Triagle(XLeft,Yup,cp.x,YLow,atRight,TColor($3ab51e));
      Triagle(cp.x,Yup,XRight,YLow,atLeft,TColor($3ab51e));
      Triagle(Xleft,Yup,XRight,cp.y,atDown,TColor($2611ce));
      Triagle(Xleft,cp.y,XRight,yLow,atUp,TColor($2611ce));
      i:=MulDiv(X,70,900);
      j:=MulDiv(Y,42,540);
      PaintXDiagonals(XLeft,YUp,XRight,YLow,i,j,clWhite);
      Ball(cp.x,cp.y, MulDiv(Y,307,1080),clWhite);
      h:=MulDiv(Y,71,1080);
      SixPointStarEdge(cp.x,cp.y-MulDiv(Y,81,540),h,
        TColor($3ab51e),TColor($2611ce));
      j:=Yup+MulDiv(Y,310,540);
      i:=MulDiv(X,69,900);
      SixPointStarEdge(cp.x-i,j,h,TColor($3ab51e),TColor($2611ce));
      SixPointStarEdge(cp.x+i,j,h,TColor($3ab51e),TColor($2611ce));
    end;
    flagCameroon:begin
      RightRatioSize(3,2);
      TriColorFlagVertical(clGreen,clRed,clYellow);
      FivePointStarBasic((XLeft+(XRight-XLeft) div 2), (YUp+(YLow-YUp)div 2),
                        ((YLow-YUp)div 5),clYellow);
    end;
    flagCapeVerde:begin
      RightRatioSize(17,10);
      Rectangular(Xleft,Yup,XRight,Ylow,clNavy);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      h:=MulDiv(y,3,36);
      Rectangular(Xleft,cp.y,XRight,cp.y+3*h,clwhite);
      Rectangular(Xleft,cp.y+h,XRight,cp.y+2*h,clRed);
      h:=MulDiv(y,48,527);
      w:=MulDiv(x,50,900);
      i:=MulDiv(x,312,900);
      j:=MulDiv(y,171,527);
      FivePointStar(XLeft+i,YUp+j,XLeft+i+w,YUp+j+h,clYellow);
      j:=MulDiv(y,435,527);
      FivePointStar(XLeft+i,YUp+j,XLeft+i+w,YUp+j+h,clYellow);
      i:=MulDiv(x,186,900);
      FivePointStar(XLeft+i,cp.y,XLeft+i+w,cp.y+h,clYellow);
      hw:=MulDiv(x,440,900);
      FivePointStar(XLeft+hw,cp.y,XLeft+hw+w,cp.y+h,clYellow);
      j:=MulDiv(y,344,527);
      FivePointStar(XLeft+i,YUp+j,XLeft+i+w,YUp+j+h,clYellow);
      FivePointStar(XLeft+hw,YUp+j,XLeft+hw+w,YUp+j+h,clYellow);
      j:=MulDiv(y,196,527);
      i:=MulDiv(x,235,900);
      FivePointStar(XLeft+i,YUp+j,XLeft+i+w,YUp+j+h,clYellow);
      hw:=MulDiv(x,390,900);
      FivePointStar(XLeft+hw,YUp+j,XLeft+hw+w,YUp+j+h,clYellow);
      j:=MulDiv(y,409,527);
      FivePointStar(XLeft+i,YUp+j,XLeft+i+w,YUp+j+h,clYellow);
      FivePointStar(XLeft+hw,YUp+j,XLeft+hw+w,YUp+j+h,clYellow);
    end;
    flagCentralAfricanRepublic:begin
      RightRatioSize(5,3);
      QuadColorFlag(clNavy,clWhite,clGreen,clYellow);
      i:=(XRight-XLeft)div 7*3;
      Rectangular(Xleft+i,Yup,XRight-i,YLow,clRed);
      FivePointStarBasic((XLeft+(XRight-XLeft) div 6), (YUp+(YLow-YUp)div 8),
                        ((YLow-YUp)div 10),clYellow);
    end;
    flagChicago:begin
      RightRatioSize(36,24);
      SixColorFlag(clWhite,clAqua,clWhite,clWhite,clAqua,clWhite);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      h:=MulDiv(y,17,136);
      SixPointStar(XLeft+Muldiv(x,53,256),cp.y,h,clRed);
      SixPointStar(XLeft+Muldiv(x,103,256),cp.y,h,clRed);
      SixPointStar(XLeft+Muldiv(x,153,256),cp.y,h,clRed);
      SixPointStar(XLeft+Muldiv(x,203,256),cp.y,h,clRed);
    end;
    flagChile:begin
      RightRatioSize(3,2);
      CantonBasic(clWhite,clRed,clBlue);
      x:=XRight-XLeft;
      hw:=(YLow-YUp)div 8;
      FivePointStar((XLeft+x div 6)-hw, (YUp+(YLow-YUp)div 4)-hw,
                    (XLeft+x div 6)+hw, (YUp+(YLow-YUp)div 4)+hw,clWhite);
    end;
    flagChina:begin
      RightRatioSize(300,200);
      Rectangular(Xleft,Yup,XRight,Ylow,clRed);
      x:=XRight-Xleft;
      y:=YLow-YUp;
      FivePointStar(XLeft+MulDiv(x,20,300),YUp+MulDiv(y,20,200),
        XLeft+MulDiv(x,80,300),YUp+MulDiv(y,75,200),clYellow);
      FivePointStar(XLeft+MulDiv(x,90,300),YUp+MulDiv(y,10,200),
        XLeft+MulDiv(x,110,300),YUp+MulDiv(y,30,200),clYellow,3);
      FivePointStar(XLeft+MulDiv(x,110,300),YUp+MulDiv(y,30,200),
        XLeft+MulDiv(x,130,300),YUp+MulDiv(y,50,200),clYellow,pi/2);
      FivePointStar(XLeft+MulDiv(x,110,300),YUp+MulDiv(y,60,200),
        XLeft+MulDiv(x,130,300),YUp+MulDiv(y,80,200),clYellow);
      FivePointStar(XLeft+MulDiv(x,90,300),YUp+MulDiv(y,80,200),
        XLeft+MulDiv(x,110,300),YUp+MulDiv(y,100,200),clYellow,1);
    end;
    flagChittagongHillTracts:begin
      RightRatioSize(3,2);
      BiColorFlagVertical(TColor($001bff),clWhite);
      x:=XRight-Xleft;
      y:=YLow-YUp;
      FivePointStar(XLeft+MulDiv(x,554,900),YUp+MulDiv(y,185,600),
        XLeft+MulDiv(x,794,900),YUp+MulDiv(y,413,600),clYellow);
    end;
    flagCongoKinshasa:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,clBlue);
      FivePointStarBasic((XLeft+(XRight-XLeft) div 2), (YUp+(YLow-YUp)div 2),
                        ((YLow-YUp)div 5),clYellow);
      for i:=1 to 6
        do FivePointStarBasic((XLeft+(XRight-XLeft) div 8),
           round(YUp+(YLow-YUp)/6.75*i), ((YLow-YUp)div 15),clYellow);
    end;
    flagCuba:begin
      RightRatioSize(2,1);
      FiveColorFlag(clBlue,clwhite,clBlue,clwhite,clBlue);
      Triagle(XLeft,YUp,Xleft+(XRight-Xleft) div 3,YLow,atRight,clRed);
      FivePointStarBasic((XLeft+(XRight-XLeft) div 8), (YUp+(YLow-YUp)div 2),
                        ((YLow-YUp)div 8),clWhite);
    end;
    flagCuracao:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,clNavy);
      x:=XRight-Xleft;
      y:=YLow-YUp;
      FivePointStar(XLeft+MulDiv(x,52,900),YUp+MulDiv(y,50,600),
        XLeft+MulDiv(x,147,900),YUp+MulDiv(y,140,600),clWhite);
      FivePointStar(XLeft+MulDiv(x,136,900),YUp+MulDiv(y,133,600),
        XLeft+MulDiv(x,262,900),YUp+MulDiv(y,252,600),clWhite);
      Rectangular(Xleft,Yup+MulDiv(y,374,600),XRight,
        Yup+MulDiv(y,448,600),clYellow);
    end;
    flagDenison:begin
      RightRatioSize(5,3);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      BiColorFlagHorizontal(clGreen,clWhite);
      h:=MulDiv(y,66,600);
      Rectangular(XLeft,cp.y-h,XRight,cp.y+h,clRed);
      w:=MulDiv(x,66,1000);
      Rectangular(cp.x-w,Yup,cp.x+w,Ylow,clBlack);
      FivePointStarBasic(cp.x,cp.y,MulDiv(x,55,1000),clWhite);
    end;
    flagDjibouti: begin
      RightRatioSize(7,4);
      BiColorFlagHorizontal(clBlue,clGreen);
      Triagle(XLeft,YUp,Xleft+(XRight-Xleft) div 7*3,YLow,atRight,clWhite);
      FivePointStarBasic((XLeft+(XRight-XLeft) div 6), (YUp+(YLow-YUp)div 2),
                        ((YLow-YUp)div 6),clRed);
    end;
    flagDurhamNC:begin
      RightRatioSize(1000,1240);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($46d0ff));
      x:=XRight-Xleft;
      y:=YLow-YUp;
      Rectangular(Xleft,Yup,XLeft+MulDiv(x,135,1000),Ylow,TColor($423eef));
      Rectangular(Xleft+MulDiv(x,270,1000),Yup,XRight,Ylow,TColor($8f5400));
      w:= MulDiv(X,111,1000);
      h:=MulDiv(y,106,1240);
      i:=XLeft+ MulDiv(X,334,1000);
      j:=Yup+MulDiv(y,728,1240);
      FivePointStar(i,j,i+w,j+h,clWhite);
      i:=XLeft+ MulDiv(X,478,1000);
      j:=Yup+MulDiv(y,682,1240);
      FivePointStar(i,j,i+w,j+h,clWhite);
      i:=XLeft+ MulDiv(X,508,1000);
      j:=Yup+MulDiv(y,810,1240);
      FivePointStar(i,j,i+w,j+h,clWhite);
      i:=XLeft+ MulDiv(X,612,1000);
      j:=Yup+MulDiv(y,605,1240);
      FivePointStar(i,j,i+w,j+h,clWhite);
      i:=XLeft+ MulDiv(X,637,1000);
      j:=Yup+MulDiv(y,745,1240);
      FivePointStar(i,j,i+w,j+h,clWhite);
      i:=XLeft+ MulDiv(X,711,1000);
      j:=Yup+MulDiv(y,509,1240);
      FivePointStar(i,j,i+w,j+h,clWhite);
      i:=XLeft+ MulDiv(X,747,1000);
      j:=Yup+MulDiv(y,646,1240);
      FivePointStar(i,j,i+w,j+h,clWhite);
    end;
    flagEsperanto:begin
      RightRatioSize(3,2);
      CantonBasic(clGreen,clGreen,clWhite);
      x:=XRight-XLeft;
      hw:=(YLow-YUp)div 8;
      FivePointStar((XLeft+x div 6)-hw, (YUp+(YLow-YUp)div 4)-hw,
                    (XLeft+x div 6)+hw, (YUp+(YLow-YUp)div 4)+hw,clGreen);
    end;
    flagEwePeople:begin
      RightRatioSize(3,2);
      TriColorFlagHorizontal(clGreen,clRed,clGreen);
      cp:=CenterPoint(Rect(Xleft,Yup,XRight,Ylow));
      y:=Yup+MulDiv(YLow-Yup,272,680);
      h:=Yup+MulDiv(YLow-Yup,411,680);
      x:=MulDiv(XRight-XLeft,127,512);
      w:=MulDiv(XRight-XLeft,73,512);
      i:=MulDiv(XRight-XLeft,73,1024);
      FivePointStar(cp.x-x-w, y,cp.x-x, h ,clYellow,0.0);
      FivePointStar(cp.x-i,y,cp.x-i+w, h ,clYellow,0.0);
      FivePointStar(cp.x+x, y,cp.x+x+w, h ,clYellow,0.0);
    end;
    flagGagauzia:begin
      RightRatioSize(2,1);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($a31d00));
      x:=XRight-Xleft;
      y:=YLow-YUp;
      Rectangular(Xleft,Yup+MulDiv(Y,300,500),XRight,Ylow,clWhite);
      Rectangular(Xleft,Yup+MulDiv(Y,400,500),XRight,Ylow,TColor($0000e8));
      i:=XLeft+ MulDiv(X,149,1000);
      j:=MulDiv(y,40,500);
      FivePointStarBasic(i,YUp+MulDiv(y,70,500),j,TColor($36deff));
      FivePointStarBasic(i,YUp+MulDiv(y,219,500),j,TColor($36deff));
      i:=XLeft+ MulDiv(X,276,1000);
      FivePointStarBasic(i,YUp+MulDiv(y,143,500),j,TColor($36deff));
    end;
    flagGambierIslands:begin
      RightRatioSize(3,2);
      TriColorFlagHorizontal(clWhite,clNavy,clWhite);
      cp:=CenterPoint(Rect(Xleft,Yup,XRight,Ylow));
      hw:=Muldiv(YLow-YUp, 160,600);
      i:=hw div 2;
      FivePointStar(cp.x-i,cp.y-i,cp.x-i+hw,cp.y-i+hw,clWhite);
      i:=MulDiv(XRight-XLeft,18,900);
      j:=Muldiv(YLow-YUp, 19,600);
      FivePointStar(XLeft+i,YUp+j,XLeft+i+hw,YUp+j+hw,clNavy);
      FivePointStar(XRight-i-hw,YUp+j,XRight-i,YUp+j+hw,clNavy);
      FivePointStar(XRight-i-hw,YLow-j-hw,XRight-i,YLow-j,clNavy);
      FivePointStar(XLeft+i,YLow-j-hw,XLeft+i+hw,YLow-j,clNavy);
    end;
    flagGhana:begin
      RightRatioSize(3,2);
      TriColorFlagHorizontal(clRed,clYellow,clGreen);
      FivePointStarBasic((XLeft+(XRight-XLeft) div 2), (YUp+(YLow-YUp)div 2),
                        ((YLow-YUp)div 6),clBlack);
    end;
    flagGoias:begin
      RightRatioSize(140,100);
      EightColorFlag(TColor($4c9900),TColor($18e7f3),
        TColor($4c9900),TColor($18e7f3),TColor($4c9900),TColor($18e7f3),
        TColor($4c9900),TColor($18e7f3));
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      j:=cp.y-(y div 8);
      i:=MulDiv(x,500,1400);
      Rectangular(Xleft,Yup,XLeft+i,j,TColor($802500));
      cp:=CenterPoint(Rect(Xleft,Yup,XLeft+i,j));
      j:=j-YUp;
      FivePointStarBasic(cp.x,cp.y,MulDiv(j,114,750),clWhite);
      x:= MulDiv(i,300,1000);
      y:= MulDiv(j,180,750);
      i:=MulDiv(j,153,750);
      FivePointStarBasic(cp.x-x,cp.y-y,i,clWhite);
      FivePointStarBasic(cp.x-x,cp.y+y,i,clWhite);
      FivePointStarBasic(cp.x+x,cp.y-y,i,clWhite);
      FivePointStarBasic(cp.x+x,cp.y+y,i,clWhite);
    end;
    flagGuayaquil:begin
      RightRatioSize(2,1);
      fiveColorFlag(TColor($e9b400),clWhite,TColor($e9b400),
        clWhite,TColor($e9b400));
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      w := MulDiv(x,94,1000);
      h := YUp+MulDiv(y,205,500);
      j := YUp+MulDiv(y,293,500);
      i := MulDiv(x,118,1000);
      FivePointStar(cp.x-i-w,h,cp.x-i,j,clWhite);
      FivePointStar(cp.x+i,h,cp.x+i+w,j,clWhite);
      i := cp.x-w div 2;
      FivePointStar(i,h,i+w,j,clWhite);
    end;
    flagGuineaBissau:begin
      RightRatioSize(2,1);
      BiColorFlagHorizontal(clYellow,clGreen);
      Rectangular(Xleft,Yup,((XRight-XLeft)div 3)+Xleft,YLow,clRed);
      FivePointStarBasic((XLeft+(XRight-XLeft) div 6), (YUp+(YLow-YUp)div 2),
                        ((YLow-YUp)div 5),clBlack);
    end;
    flagHonduras:begin
      RightRatioSize(2,1);
      TriColorFlagHorizontal(clBlue,clWhite,clBlue);
      FivePointStarBasic((XLeft+(XRight-XLeft) div 2), (YUp+(YLow-YUp)div 2),
                        ((YLow-YUp)div 12),clBlue);
      i:=round((YLow-YUp)/12*5);
      FivePointStarBasic(XLeft+round((XRight-XLeft)/5*2), (YUp+i),
                        ((YLow-YUp)div 12),clBlue);
      FivePointStarBasic(XLeft+round((XRight-XLeft)/5*2), (YLow-i),
                        ((YLow-YUp)div 12),clBlue);
      FivePointStarBasic(XLeft+round((XRight-XLeft)/5*3), (YUp+i),
                        ((YLow-YUp)div 12),clBlue);
      FivePointStarBasic(XLeft+round((XRight-XLeft)/5*3), (YLow-i),
                        ((YLow-YUp)div 12),clBlue);
    end;
    flagJordan: begin
      RightRatioSize(2,1);
      TriColorFlagHorizontal(clBlack,clWhite,clGreen);
      x:=XRight-XLeft;
      Triagle(XLeft,YUp,Xleft+x div 2,YLow,atRight,clRed);
      SevenPointStar(XLeft+MulDiv(x,12,84),YUp+(YLow-Yup)div 2,MulDiv(x,3,84),clWhite);
    end;
    flagLipkovo:begin
      RightRatioSize(2,1);
      FiveColorFlag(clGreen,clwhite,clGreen,clwhite,clGreen);
      Rectangular(Xleft,Yup,(XRight-XLeft) div 4+XLeft,Ylow,clRed);
      x:=(XRight-XLeft) div 2;
      y:=(YLow-YUp)div 2+YUp;
      Triagle(XLeft,YUp,Xleft+x,y,atDown,clBlue);
      Triagle(XLeft,Y,Xleft+x,YLow,atUp,clBlue);
      i:=(YLow-YUp)div 5;
      x:=MulDiv(XRight-XLeft,1,16);
      y:=y - (i div 2);
      FivePointStar(XLeft+x,y,XLeft+x+i,y+i,clWhite,0.0);
    end;
    flagMaastricht:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,clRed);
      x:=XRight-XLeft;
      y:=Ylow-Yup;
      FivePointStar(XLeft+MulDiv(x,110,750),YUp+MulDiv(y,90,500),
        XLeft+MulDiv(x,425,750),YUp+MulDiv(y,390,500),clWhite, 0.0);
    end;
    flagMadrid:begin
      RightRatioSize(600,400);
      Rectangular(Xleft,Yup,XRight,Ylow,clRed);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      j:=MulDiv(x,34,600);
      h:=MulDiv(y,68,400);
      w:=MulDiv(x,42,600);
      hw:=cp.x-j*3-w;
      j:=j*2;
      for i:= 1 to 3 do
        begin
          FivePointStar(hw,cp.y,hw+j,cp.y+h,clWhite,0.0);
          hw:=hw+w+j;
        end;
      hw:=cp.x-j*2-w-w div 2;
      y:=cp.y-h+MulDiv(y,10,400);
      for i:= 1 to 4 do
        begin
          FivePointStar(hw,y,hw+j,y+h,clWhite,0.0);
          hw:=hw+w+j;
        end;
    end;
    flagMaranhao:begin
      RightRatioSize(3,2);
      NineColorFlag(clRed,clWhite,clBlack,clWhite,clRed,clWhite,clBlack,
        clWhite,clRed);
      y:=YLow-Yup;
      x:=XRight-XLeft;
      Rectangular(Xleft,Yup,Xleft+MulDiv(x,3,9),Yup+y div 2,TColor($960019));
      FivePointStarBasic(Xleft+MulDiv(x,15,90),Yup+MulDiv(y,15,60),
        MulDiv(y,128,600),clWhite);

    end;
    flagMicronesia:begin
      RightRatioSize(38,20);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($00DDB275));
      y:=YLow-Yup;
      x:=XRight-XLeft;
      FivePointStar(XLeft+MulDiv(x,19-2,38),YUp+MulDiv(y,4-2,20),
        XLeft+MulDiv(x,19+2,38),YUp+MulDiv(y,4+2,20),clWhite, 0.0);
      FivePointStar(XLeft+MulDiv(x,19-2,38),YLow-MulDiv(y,4+2,20),
        XLeft+MulDiv(x,19+2,38),YLow-MulDiv(y,4-2,20),clWhite, 1.9);
      FivePointStar(XLeft+MulDiv(x,13-2,38),YUp+MulDiv(y,10-2,20),
        XLeft+MulDiv(x,13+2,38),YLow-MulDiv(y,10-2,20),clWhite, pi/2);
      FivePointStar(XLeft+MulDiv(x,25-2,38),YUp+MulDiv(y,10-2,20),
        XLeft+MulDiv(x,25+2,38),YLow-MulDiv(y,10-2,20),clWhite, 1.0);
    end;
    flagMilneBay:begin
      RightRatioSize(3,2);
      x:=XRight-XLeft;
      y:=Ylow-Yup;
      Rectangular(Xleft,Yup,XRight,Ylow,clWhite);
      Rectangular(Xleft,Yup,Xleft+MulDiv(x,127,900),Ylow,TColor($005522));
      TwoRightTriangleDownUp(Xleft+MulDiv(x,256,900),Yup,XRight,Ylow,
        TColor($000080),TColor($800000));
      FivePointStarBasic(Xleft+MulDiv(x,609,900),Yup+MulDiv(y,96,600),
        MulDiv(y,60,600),TColor($00ccff));
    end;
    flagMonteria:begin
      RightRatioSize(3,2);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      TwoPlusTwoColor(clRed,clWhite,clWhite,clNavy);
      FivePointStarBasic(cp.x,cp.y,MulDiv(y,150,600),clYellow);
    end;
    flagNamibia:begin
      RightRatioSize(180,120);
      Rectangular(Xleft,Yup,XRight,Ylow,clRed);
      x:=XRight-XLeft;
      y:=Ylow-Yup;
      Triangular(XLeft,Yup,XRight-DistCorner(x,y,y,x*15/180),
        Ylow-DistCorner(x,y,x,x*15/180),pi,1.0,clWhite);
      Triangular(XLeft,Yup,XRight-DistCorner(x,y,y,x*20/180),
        Ylow-DistCorner(x,y,x,x*20/180),pi,1.0,clNavy);
      Triangular(XLeft+DistCorner(x,y,y,x*15/180),
        Yup+DistCorner(x,y,x,x*15/180), XRight,Ylow,0.0,1.0,clWhite);
      Triangular(XLeft+DistCorner(x,y,y,x*20/180),
        Yup+DistCorner(x,y,x,x*20/180),XRight,Ylow,0.0,1.0,clGreen);
      i:=MulDiv(YLow-Yup,20,120);
      y:=MulDiv(YLow-Yup,33,120)+YUp;
      x:=MulDiv(XRight-XLeft,36,180)+XLeft;
      PaintNPointStar(x,y,i,12,clYellow);
      Ball(x,y,MulDiv(YLow-Yup,12,120),clNavy);
      Ball(x,y,MulDiv(YLow-Yup,10,120),clYellow);
    end;
    flagNauru:begin
      RightRatioSize(48,24);
      Rectangular(Xleft,Yup,XRight,Ylow,clNavy);
      y:=MulDiv(YLow-Yup,11,24);
      Rectangular(Xleft,Yup+y,XRight,Ylow-y,clYellow);
      i:=MulDiv(YLow-Yup,4,24);
      PaintNPointStar(Xleft+(XRight-XLeft) div 4,Ylow-y+i,i,12,clWhite);
    end;
    flagNetherlandsAntilles:begin
      RightRatioSize(54,36);
      TriColorFlagHorizontal(clWhite,clBlue,clWhite);
      i:=MulDiv(XRight-XLeft,6,54);
      y:=(YLow-Yup) div 3;
      x:=(XRight-XLeft) div 2+XLeft;
      Rectangular(X-i,Yup,X+i,Yup+y,clRed);
      Rectangular(X-i,YLow-y,X+i,Ylow,clRed);
      i:=MulDiv(XRight-XLeft,3,108);
      y:=MulDiv(YLow-Yup,14,36);
      FivePointStar(x-i, YUp+y-i,x+i,YUp+y+i,clWhite);
      x:=MulDiv(XRight-XLeft,196,540);
      y:=MulDiv(YLow-Yup,17,36);
      FivePointStar(XLeft+x-i, YUp+y-i,XLeft+x+i,YUp+y+i,clWhite);
      FivePointStar(XRight-x-i, YUp+y-i,XRight-x+i,YUp+y+i,clWhite);
      x:=MulDiv(XRight-XLeft,239,540);
      y:=MulDiv(YLow-Yup,214,360);
      FivePointStar(XLeft+x-i, YUp+y-i,XLeft+x+i,YUp+y+i,clWhite);
      FivePointStar(XRight-x-i, YUp+y-i,XRight-x+i,YUp+y+i,clWhite);
    end;
    flagNorthKorea:begin
      RightRatioSize(2,1);
      Rectangular(Xleft,Yup,XRight,Ylow,clBlue);
      i:=round((YLow-YUp)/(12+2+44+2+12)*12);
      Rectangular(Xleft,Yup+i,XRight,Ylow-i,clWhite);
      i:=round((YLow-YUp)/(12+2+44+2+12)*14);
      Rectangular(Xleft,Yup+i,XRight,Ylow-i,clRed);
      i:=round((YLow-YUp)/(12+2+44+2+12)*32/2);
      Ball(XLeft+round((XRight-XLeft)/(48+96)*48),YUp+(YLow-YUp)div 2,
        i,clWhite);
      i:=round((YLow-YUp)/(12+2+44+2+12)*30/2);
      FivePointStarBasic(XLeft+round((XRight-XLeft)/(48+96)*48),
        YUp+(YLow-YUp)div 2,i,clRed);
    end;
    flagOgaden:begin
      RightRatioSize(3,2);
      TriColorFlagHorizontal(TColor($008100),TColor($804000),TColor($0000fe));
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      FivePointStarBasic(cp.x,cp.y,MulDiv(y,84,600),clWhite);
    end;
    flagPanama:begin
      RightRatioSize(3,2);
      TwoPlusTwoColor(clWhite,clRed,clBlue,clWhite);
      FivePointStarBasic(XLeft+(XRight-XLeft) div 4, (YUp+(YLow-YUp)div 4),
                        ((YLow-YUp)div 8),clBlue);
      FivePointStarBasic(XRight-(XRight-XLeft) div 4, (YLow-(YLow-YUp)div 4),
                        ((YLow-YUp)div 8),clRed);
    end;
    flagPuertoRico:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,clRed);
      Rectangular(Xleft,Yup+(YLow-Yup)div 5,XRight,Yup+(YLow-Yup)div 5*2,clwhite);
      Rectangular(Xleft,YLow-(YLow-Yup)div 5*2,XRight,YLow-(YLow-Yup)div 5,clwhite);
      Triagle(XLeft,YUp,Xleft+(XRight-Xleft) div 3,YLow,atRight,clBlue);
      FivePointStarBasic((XLeft+(XRight-XLeft) div 8), (YUp+(YLow-YUp)div 2),
                        ((YLow-YUp)div 8),clWhite);
    end;
    flagRedArmy:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,clRed);
      FivePointStar(XLeft+MulDiv(XRight-XLeft,120,450),
        YUp+MulDiv(YLow-YUp,50,300),XLeft+MulDiv(XRight-XLeft,330,450),
        YUp+MulDiv(YLow-YUp,250,300),clYellow);
      FivePointStar(XLeft+MulDiv(XRight-XLeft,159,450),
        YUp+MulDiv(YLow-YUp,89,300),XLeft+MulDiv(XRight-XLeft,291,450),
        YUp+MulDiv(YLow-YUp,218,300),clRed);
    end;
    flagRondonia:begin
      RightRatioSize(1000,700);
      BiColorFlagHorizontal(TColor($802500),TColor($18e7f3));
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      Triagle(Xleft,cp.y,XRight,YLow,atUp,TColor($4c9900));
      FivePointStarBasic(cp.x,YUp+MulDiv(y,300,700),MulDiv(y,126,700),clWhite);
    end;
    flagRwanda:begin
      RightRatioSize(3,2);
      QuadColorFlag(TColor($00C98400),TColor($00C98400),clYellow,clGreen);
      y:=MulDiv(YLow-YUp,68,232)+YUp;
      x:=XLeft+MulDiv(XRight-XLeft,320,390);
      PaintNPointStar(x,y,MulDiv(XRight-XLeft,42,390),24,clYellow);
      Ball(x,y,MulDiv(XRight-XLeft,13,390),clBlue);
      Ball(x,y,MulDiv(XRight-XLeft,11,390),clYellow);
    end;
    flagSamoa:begin
      RightRatioSize(288,144);
      Rectangular(Xleft,Yup,XRight,Ylow,clRed);
      y:=MulDiv(YLow-Yup,72,144);
      x:=MulDiv(XRight-XLeft,144,288);
      Rectangular(Xleft,Yup,XLeft+x,YUp+y,clNavy);
      x:=MulDiv(XRight-XLeft,42,288)+XLeft;
      y:=MulDiv(YLow-Yup,21,144)+YUp;
      hw:=MulDiv(XRight-XLeft,16,288);
      FivePointStar(X, y,X+hw,Y+hw ,clWhite);
      x:=MulDiv(XRight-XLeft,64,288)+XLeft;
      y:=MulDiv(YLow-Yup,3,144)+YUp;
      FivePointStar(X, y,X+hw,Y+hw ,clWhite);
      x:=MulDiv(XRight-XLeft,87,288)+XLeft;
      y:=MulDiv(YLow-Yup,18,144)+YUp;
      hw:=MulDiv(XRight-XLeft,15,288);
      FivePointStar(X, y,X+hw,Y+hw ,clWhite);
      x:=MulDiv(XRight-XLeft,62,288)+XLeft;
      y:=MulDiv(YLow-Yup,49,144)+YUp;
      hw:=MulDiv(XRight-XLeft,20,288);
      FivePointStar(X, y,X+hw,Y+hw ,clWhite);
      x:=MulDiv(XRight-XLeft,80,288)+XLeft;
      y:=MulDiv(YLow-Yup,36,144)+YUp;
      hw:=MulDiv(XRight-XLeft,10,288);
      FivePointStar(X, y,X+hw,Y+hw ,clWhite);
    end;
    flagSaoTomeAndPrincipe:begin
      RightRatioSize(2,1);
      Rectangular(Xleft,Yup,XRight,Ylow,clGreen);
      Rectangular(Xleft,Yup+(YLow-Yup)div 4,XRight,YLow-(YLow-Yup)div 4,clYellow);
      Triagle(XLeft,YUp,Xleft+(XRight-Xleft) div 4,YLow,atRight,clRed);
      FivePointStarBasic((XLeft+(XRight-XLeft) div 2), (YUp+(YLow-YUp)div 2),
                        ((YLow-YUp)div 6),clBlack);
      FivePointStarBasic((XRight-(XRight-XLeft) div 4), (YUp+(YLow-YUp)div 2),
                        ((YLow-YUp)div 6),clBlack);
    end;
    flagSenegal:begin
      RightRatioSize(3,2);
      TriColorFlagVertical(clGreen,clYellow,clRed);
      FivePointStarBasic((XLeft+(XRight-XLeft) div 2), (YUp+(YLow-YUp)div 2),
                        ((XRight-XLeft)div 12),clGreen);
    end;
    flagSergipe:begin
      RightRatioSize(1000,700);
      QuadColorFlag(TColor($3f9200),TColor($00c3f8),
        TColor($3f9200),TColor($00c3f8));
      x:=XRight-XLeft;
      y:=Ylow-Yup;
      i:=y div 2;
      Rectangular(Xleft,Yup,Xleft+i,Yup+i,TColor($6f1628));
      j:=y div 4;
      FivePointStarBasic(XLeft+j,YUp+j,MulDiv(i,80,350),clWhite);
      hw:= MulDiv(i,40,350);
      j:=MulDiv(i,60,350);
      FivePointStarBasic(XLeft+j,YUp+j,hw,clWhite);
      FivePointStarBasic(XLeft+i-j,YUp+j,hw,clWhite);
      FivePointStarBasic(XLeft+j,YUp+i-j,hw,clWhite);
      FivePointStarBasic(XLeft+i-j,YUp+i-j,hw,clWhite);
    end;
    flagSFRYugoslavia:begin
      RightRatioSize(2,1);
      TriColorFlagHorizontal(TColor($8d3300),clWhite,TColor($0000de));
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      if x>200 then
        begin
          FivePointStarBasic(cp.x,cp.y,MulDiv(y,167,500),TColor($15d1fc));
          FivePointStarBasic(cp.x,cp.y,MulDiv(y,150,500),TColor($0000de));
       end
       else FivePointStarEdge(cp.x,cp.y,MulDiv(y,167,500),
         TColor($15d1fc),TColor($0000de));
    end;
    flagSolomonIslands:begin
      RightRatioSize(400,200);
      Rectangular(Xleft,Yup,XRight,YLow,clYellow);
      x:=XRight-XLeft;
      y:=Ylow-Yup;
      Triangular(XLeft,Yup,XRight-DistCorner(x,y,y,x*9/400),
        Ylow-DistCorner(x,y,x,x*9/400),pi,1.0,clBlue);
      Triangular(XLeft+DistCorner(x,y,y,x*9/400),Yup+DistCorner(x,y,x,x*9/400),
        XRight,Ylow,0.0,1.0,clGreen);
      x:=MulDiv(XRight-XLeft,70,400)+XLeft;
      y:=MulDiv(YLow-Yup,60,200)+YUp;
      hw:=MulDiv(XRight-XLeft,10,400);
      FivePointStar(X-hw, y-hw,X+hw,Y+hw ,clWhite);
      y:=MulDiv(YLow-Yup,38,200)+YUp;
      i:=MulDiv(XRight-XLeft,64,400);
      x:=MulDiv(XRight-XLeft,38,400)+XLeft;
      FivePointStar(X-hw, y-hw,X+hw,Y+hw ,clWhite);
      FivePointStar(X-hw+i, y-hw,X+hw+i,Y+hw ,clWhite);
      y:=MulDiv(YLow-Yup,82,200)+YUp;
      FivePointStar(X-hw, y-hw,X+hw,Y+hw ,clWhite);
      FivePointStar(X-hw+i, y-hw,X+hw+i,Y+hw ,clWhite);
    end;
    flagSomalia:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($00CF7938));
      FivePointStarBasic((XLeft+(XRight-XLeft) div 2), (YUp+(YLow-YUp)div 2),
                         ((YLow-YUp)div 4),clYellow);
    end;
    flagSuriname:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,YUp,XRight,Ylow,clGreen);
      i:=round((YLow-YUp)/62*12);
      Rectangular(Xleft, YUp +i, XRight,YLow-i,clWhite);
      i:=round((YLow-YUp)/62*18);
      Rectangular(Xleft, YUp +i, XRight,YLow-i,clRed);
      FivePointStarBasic(XLeft+(XRight-XLeft) div 2, YUp+(YLow-YUp)div 2,
                          i div 2,clYellow);
    end;
    flagSyria:begin
      RightRatioSize(3,2);
      TriColorFlagHorizontal(clRed,clWhite,clBlack);
      FivePointStarBasic(XLeft+(XRight-XLeft) div 3, YUp+(YLow-YUp)div 2,
                        (YLow-YUp)div 8,clGreen);
      FivePointStarBasic(XRight-(XRight-XLeft) div 3,YUp+(YLow-YUp)div 2,
                        (YLow-YUp)div 8,clGreen);
    end;
    flagTaiwan:begin
      RightRatioSize(240,160);
      Rectangular(Xleft,Yup,XRight,Ylow,clRed);
      y:=MulDiv(YLow-Yup,80,160);
      x:=MulDiv(XRight-XLeft,120,240);
      Rectangular(Xleft,Yup,XLeft+x,YUp+y,clNavy);
      i:=MulDiv(YLow-Yup,30,160);
      y:=MulDiv(YLow-Yup,40,160)+YUp;
      x:=MulDiv(XRight-XLeft,60,240)+XLeft;
      PaintNPointStar(x,y,i,12,clWhite);
      Ball(x,y,MulDiv(YLow-Yup,17,160),clNavy);
      Ball(x,y,MulDiv(YLow-Yup,15,160),clWhite);
    end;
    flagTenessee:begin
      RightRatioSize(100,60);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($2121e7));
      i:=XRight-MulDiv(x,75,1000);
      j:=XRight-MulDiv(x,63,1000);
      if j>= XRight then j:=j-1;
      if i>= j then i:=i-1;
      Rectangular(i,Yup,j,Ylow,clWhite);
      Rectangular(j,Yup,XRight,Ylow,TColor($7c3c00));
      cp.x:=Xleft+MulDiv(x,463,1000);
      i:=MulDiv(x,163,1000);
      j:=MulDiv(x,149,1000);
      if j>=i then i:=i+1;
      Ball(cp.x,cp.y,i,clWhite);
      Ball(cp.x,cp.y,j,TColor($7c3c00));
      FivePointStar(XLeft+MulDiv(x,342,1000),YUp+MulDiv(y,173,600),
        XLeft+MulDiv(x,474,1000),YUp+MulDiv(y,303,600),clWhite,6/180*pi);
      FivePointStar(XLeft+MulDiv(x,380,1000),YUp+MulDiv(y,307,600),
        XLeft+MulDiv(x,510,1000),YUp+MulDiv(y,441,600),clWhite,53/180*pi);
      FivePointStar(XLeft+MulDiv(x,468,1000),YUp+MulDiv(y,225,600),
        XLeft+MulDiv(x,603,1000),YUp+MulDiv(y,355,600),clWhite,31/180*pi);
    end;
    flagTexas:begin
      RightRatioSize(3,2);
      BiColorFlagHorizontal(clWhite,clRed);
      Rectangular(Xleft,Yup,((XRight-XLeft)div 3)+Xleft,YLow,clBlue);
      hw:=(YLow-YUp)div 5;
      FivePointStar((XLeft+(XRight-XLeft) div 6)-hw, (YUp+(YLow-YUp)div 2)-hw,
             (XLeft+(XRight-XLeft) div 6)+hw, (YUp+(YLow-YUp)div 2)+hw,clWhite);
    end;
    flagTogo:begin
      RightRatioSize(5,3);
      FiveColorFlag(clGreen,clYellow,clGreen,clYellow,clGreen);
      Rectangular(Xleft,Yup,((XRight-XLeft)div 3)+Xleft,
        YLow-MulDiv(Ylow-YUp,2,5),clRed);
      FivePointStarBasic((XLeft+(XRight-XLeft) div 6),
        round(YUp+(YLow-YUp)/ 10 * 3), ((YLow-YUp)div 8),clWhite);
    end;
    flagVenezuela:begin
      RightRatioSize(900,600);
      TriColorFlagHorizontal(clYellow,clBlue,clRed);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      w:=MulDiv(x,57,900);
      i:=MulDiv(x,252,900);
      j:=MulDiv(Y,326,600);
      h:=MulDiv(Y,380,600);
      FivePointStar(XLeft+i, YUp+j,XLeft+i+w, YUp+h ,clYellow,70*Pi/180);
      i:=MulDiv(x,590,900);
      FivePointStar(XLeft+i, YUp+j,XLeft+i+w, YUp+h ,clYellow,-70*Pi/180);
      w:=MulDiv(x,55,900);
      i:=MulDiv(x,286,900);
      j:=MulDiv(Y,275,600);
      h:=MulDiv(Y,332,600);
      FivePointStar(XLeft+i, YUp+j,XLeft+i+w, YUp+h ,clYellow,50*Pi/180);
      i:=MulDiv(x,557,900);
      FivePointStar(XLeft+i, YUp+j,XLeft+i+w, YUp+h ,clYellow,-50*Pi/180);
      w:=MulDiv(x,56,900);
      i:=MulDiv(x,330,900);
      j:=MulDiv(Y,237,600);
      h:=MulDiv(Y,292,600);
      FivePointStar(XLeft+i, YUp+j,XLeft+i+w, YUp+h ,clYellow,30*Pi/180);
      i:=MulDiv(x,512,900);
      FivePointStar(XLeft+i, YUp+j,XLeft+i+w, YUp+h ,clYellow,-30*Pi/180);
      w:=MulDiv(x,56,900);
      i:=MulDiv(x,389,900);
      j:=MulDiv(Y,212,600);
      h:=MulDiv(Y,267,600);
      FivePointStar(XLeft+i, YUp+j,XLeft+i+w, YUp+h ,clYellow,10*Pi/180);
      i:=MulDiv(x,454,900);
      FivePointStar(XLeft+i, YUp+j,XLeft+i+w, YUp+h ,clYellow,-10*Pi/180);
    end;
    flagVietnam:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,clRed);
      i:=(YLow-YUp)div 2;
      hw :=i div 2;
      FivePointStar((XLeft+(XRight-XLeft) div 2)-hw, YUp+i-hw,
                    (XLeft+(XRight-XLeft) div 2)+hw, YUp+i+hw ,clYellow);
    end;
    flagVojvodina:begin
      RightRatioSize(2,1);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($990000));
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      h:=MulDiv(Y,50,500);
      Rectangular(Xleft,Yup,XRight,YUp+h,clRed);
      Rectangular(Xleft,Ylow-h,XRight,Ylow,clWhite);
      i:=MulDiv(x,150,1000);
      j:=MulDiv(Y,24,500);
      h:=MulDiv(Y,121,500);
      FivePointStar(cp.X-i,cp.y-j,cp.x, cp.y+h ,clYellow,-pi/2);
      FivePointStar(cp.X,cp.y-j,cp.x+i, cp.y+h ,clYellow,pi/2);
      i:=MulDiv(x,76,1000);
      j:=MulDiv(Y,171,500);
      h:=MulDiv(Y,29,500);
      FivePointStar(cp.X-i,cp.y-j,cp.x+i, cp.y-h ,clYellow);
    end;
    flagWashingtonDC:begin
      RightRatioSize(512,256);
      Rectangular(Xleft,Yup,XRight,Ylow,clWhite);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      Rectangular(Xleft,cp.y-MulDiv(y,51*2,512),XRight,cp.y,clRed);
      Rectangular(Xleft,cp.y+MulDiv(y,51,512),XRight,
        cp.y+MulDiv(y,51*3,512),clRed);
      w:=MulDiv(x,48,512);
      i:=cp.x-(w div 2);
      h:=MulDiv(y,46,256);
      hw:=Yup+MulDiv(y,13,256);
      FivePointStar(i, hw,i+w, hw+h ,clRed,0.0);
      i:=i-MulDiv(x,80,512);
      FivePointStar(i-w, hw,i, hw+h ,clRed,0.0);
      i:=cp.x-(w div 2)+w+MulDiv(x,80,512);
      FivePointStar(i, hw,i+w, hw+h ,clRed,0.0);
    end;
    flagWestSomalia:begin
      RightRatioSize(3,2);
      BiColorFlagVertical(clBlack,clRed);
      i:=(YLow-YUp)div 2;
      hw :=round(i/1.5);
      FivePointStar((XLeft+(XRight-XLeft) div 2)-hw, YUp+i-hw,
                    (XLeft+(XRight-XLeft) div 2)+hw, YUp+i+hw ,clWhite);
    end;
  end;
end;

procedure TCustomFlag.CrescentFlags ;
var r:real;i,x,y,dy,dx:integer;
   cp:TPoint;
  procedure CalcPakistanCP(x1,y1,x2,y2,d:integer; var cpx,cpy:integer);
  var s,e,k,dx,dy:double;
  begin
    dx:=x2-x1;
    dy:=y2-y1;
    s := sqrt((dx*dx)+(dy*dy));
    e := s-d;
    k := e/s;
    cpx := x2-Round(k*dx);
    cpy := y1+Round(k*dy);
  end;
  procedure TunisiaBaseFlag;
  var dx,dy,x,y:integer;
  begin
      RightRatioSize(120,80);
      Rectangular(Xleft,Yup,XRight,Ylow,clRed);
      dy:=YLow-Yup;
      y:=Yup+dy div 2;
      dx:=XRight-XLeft;
      x:=XLeft+dx div 2;
      Ball(x,y,dy div 4,clWhite);
      Ball(x,y,MulDiv(dy,15,80) ,clRed);
      Ball(XLeft+MulDiv(dx,64,120),y,MulDiv(dy,12,80) ,clWhite);
      FivePointStar(XLeft+MulDiv(dx,64-9,120),YUp+MulDiv(dy,40-9,80),
        XLeft+MulDiv(dx,64+9,120),YUp+MulDiv(dy,40+9,80),clRed, pi/2);
  end;
  procedure TurkeyBase(x1,y1,x2,y2:integer;bcl,cl1:TColor);
  var dx,dy,x,y:integer;
  begin
    Rectangular(x1,y1,x2,y2,bcl);
    dx:=X2-X1;
    dy:=Y2-Y1;
    y:=Y1+dy div 2;
    x:=X1+MulDiv(dx,120,360);
    Ball(x,y,MulDiv(dy,60,240),cl1);
    Ball(X1+MulDiv(dx,135,360),y,MulDiv(dy,54,240),bcl);
    FivePointStar(X1+MulDiv(dx,161,360),YUp+MulDiv(dy,120-30,240),
      X1+MulDiv(dx,161+60,360),YUp+MulDiv(dy,120+30,240),cl1, pi/2);
  end;
begin
  case FFlag of
    flagComoros:begin
      RightRatioSize(3,2);
      QuadColorFlag(clYellow,clWhite,clRed,clBlue);
      Triagle(XLeft,YUp,Xleft+(XRight-Xleft) div 2,YLow,atRight,clGreen);
      i:=(YLow-YUp)div 4;
      HalfBall(XLeft+(XRight-XLeft)div 20,YUp+(YLow-YUp)div 4,
              XLeft+(XRight-XLeft)div 20+i ,YLow-(YLow-YUp)div 4,atLeft,clWhite);
      HalfBall(XLeft+(XRight-XLeft)div 20+(YLow-YUp)div 12,YUp+(YLow-YUp)div 4,
              XLeft+(XRight-XLeft)div 20+i ,YLow-(YLow-YUp)div 4,atLeft,clGreen);
      i:=(YLow-YUp)div 21;
      r:=(YLow-YUp)/60;
      FivePointStarBasic((XLeft+(XRight-XLeft) div 4),YUp+Round(r*21),
                        i,clWhite);
      FivePointStarBasic((XLeft+(XRight-XLeft) div 4),YUp+Round(r*27),
                        i,clWhite);
      FivePointStarBasic((XLeft+(XRight-XLeft) div 4),YUp+Round(r*33),
                        i,clWhite);
      FivePointStarBasic((XLeft+(XRight-XLeft) div 4),YUp+Round(r*39),
                        i,clWhite);

    end;
    flagDoesburg:begin
      RightRatioSize(3,2);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      QuadColorFlag(clWhite, clRed, clRed, clWhite);
      Ball(XLeft+MulDiv(x,150,540),cp.y,MulDiv(y,78,360) ,clYellow);
      Ball(XLeft+MulDiv(x,123,540),cp.y,MulDiv(y,60,360) ,clRed);
    end;
    flagFrenchTunisia:begin
      TunisiaBaseFlag;
      y:=YLow-Yup;
      x:=XRight-XLeft;
      dx:=MulDiv(x,223,900);
      dy:=MulDiv(y,149,600);
      x:=MulDiv(x,220,900);
      y:=MulDiv(y,146,600);
      if x+1>=dx then dx := x + 1;
      if y+1>=dy then dy := y + 1;
      Rectangular(XLeft,YUp,XLeft+dx,YUp+dy,clWhite);
      TriColorBasicVertical(clNavy, clWhite, clRed,XLeft,Yup,XLeft+x,YUp+y);
    end;
    flagIraqiTurkmen:begin
      RightRatioSize(360,240);
      TurkeyBase(Xleft,Yup,XRight,Ylow,clWhite,TColor($fbc863));
      y:=YLow-Yup;
      dy:=MulDiv(y,60,600);
      Rectangular(Xleft,Yup+dy,XRight,Yup+dy+dy,TColor($fbc863));
      Rectangular(Xleft,YLow-dy-dy,XRight,YLow-dy,TColor($fbc863));
    end;
    flagMakran:begin
      RightRatioSize(1024,682);
      x:=XRight-XLeft;
      y:=YLow-Yup;
      TriColorFlagHorizontal(TColor($009900), clRed, clBlue);
      dy:=MulDiv(y,114,682);
      Ball(XLeft+MulDiv(x,768,1024),dy,MulDiv(y,92,682) ,clWhite);
      Ball(XLeft+MulDiv(x,803,1024),dy,MulDiv(y,77,682) ,TColor($009900));
      FivePointStarBasic(XLeft+MulDiv(x,820,1024),dy,MulDiv(y,50,682),clWhite);
    end;
    flagMalacca:begin
      RightRatioSize(2,1);
      BiColorFlagHorizontal(TColor($0b21A9),clWhite);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      Rectangular(Xleft,Yup,cp.x,cp.y,TColor($4d0c0e));
      dy:=YUp+ y div 4;
      i:=MulDiv(y,90,500);
      Ball(XLeft+MulDiv(x,190,1000),dy,i ,TColor($2cd7ed));
      Ball(XLeft+MulDiv(x,230,1000),dy,i ,TColor($4d0c0e));
      FivePointStar(XLeft+MulDiv(x,179,1000),YUp+MulDiv(y,62,500),XLeft+
        MulDiv(x,295,1000),YUp+MulDiv(y,184,500),TColor($2cd7ed),64/180*pi);
    end;
    flagMaldives:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,clRed);
      i:=(YLow-YUp)div 4;
      Rectangular(Xleft+i,Yup+i,XRight-i,Ylow-i,clLime);
      i:=(YLow-YUp)div 6;
      Ball(XLeft+(XRight-XLeft)div 2+i div 2,YUp+(YLow-YUp)div 2,i ,clWhite);
      Ball(XLeft+(XRight-XLeft)div 2+i,YUp+(YLow-YUp)div 2,i,clLime);
    end;
    flagMauritania:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,clGreen);
      r:=(YLow-YUp)/48;
      Ball(XLeft+(XRight-XLeft)div 2,YUp+Round(r*18),Round(r*31/2),clYellow);
      Ball(XLeft+(XRight-XLeft)div 2,YUp+Round(r*14),Round(r*30/2),clGreen);
      FivePointStarBasic((XLeft+(XRight-XLeft) div 2),YUp+Round(r*14),
                        round(r*6),clYellow);
    end;
    flagMikmaq:begin
      RightRatioSize(2,1);
      Rectangular(Xleft,Yup,XRight,Ylow,clWhite);
      x:=XRight-XLeft;
      y:=YLow-Yup;
      dx:=XLeft+MulDiv(x,108,1000);
      i:=MulDiv(y,79,500);
      Ball(dx,YUp+MulDiv(y,396,500),i,clRed);
      Ball(dx,YUp+MulDiv(y,362,500),i,clWhite);
      i:=MulDiv(y,30,500);
      dy:=MulDiv(y,200,500);
      CrossBasicPaint(clRed,Xleft+i,YUp+i,XRight-i,YLow-i,
        XLeft+MulDiv(x,218,1000),XLeft+MulDiv(x,317,1000),YUp+dy,YLow-dy);
      FivePointStar(XLeft+MulDiv(x,19,1000),YUp+MulDiv(y,15,500),
        XLeft+MulDiv(x,194,1000),YUp+MulDiv(y,170,500),clRed, pi/6);
    end;
    flagObdam:begin
      RightRatioSize(3,2);
      TriColorFlagVertical(clYellow, clRed, clRed);
      x:=XRight-XLeft;
      y:=YLow-Yup;
      dx:=XLeft+MulDiv(x,231,450);
      dy:=YUp+MulDiv(y,66,300);
      i:=MulDiv(y,62,300);
      Ball(dx,dy,i ,clYellow);
      dy:=YUp+MulDiv(y,212,300);
      Ball(dx,dy,i ,clYellow);
      dy:=YUp+MulDiv(y,53,300);
      i:=MulDiv(y,53,300);
      Ball(dx,dy,i ,clRed);
      dy:=YUp+MulDiv(y,199,300);
      Ball(dx,dy,i ,clRed);
      dx:=XLeft+MulDiv(x,371,450);
      dy:=YUp+MulDiv(y,137,300);
      i:=MulDiv(y,62,300);
      Ball(dx,dy,i ,clYellow);
      dy:=YUp+MulDiv(y,124,300);
      i:=MulDiv(y,53,300);
      Ball(dx,dy,i ,clRed);
    end;
    flagPakistan:begin
      RightRatioSize(360,240);
      Rectangular(Xleft,Yup,XRight,Ylow,clWhite);
      dx:=XRight-XLeft;
      dy:=YLow-Yup;
      x:=dx div 4;
      y:=Yup+dy div 2;
      Rectangular(Xleft+x,Yup,XRight,Ylow,clGreen);
      x:=XLeft+MulDiv(dx,360-136,360);
      Ball(x,y,MulDiv(dy,72,240),clWhite);
      i:=MulDiv(dy,24,240);
      CalcPakistanCP(x,yup,xright,y,i,dx,dy);
      Ball(dx,dy,MulDiv(YLow-Yup,66,240),clGreen);
      CalcPakistanCP(x,yup,xright,y,MulDiv(YLow-Yup,48,240),dx,dy);
      FivePointStar(dx-i,dy-i,dx+i,dy+i,clWhite, pi/5);
    end;
    flagRedCrescent:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,clWhite);
      cp:=CenterPoint(Rect(Xleft,Yup,XRight,Ylow));
      i:=MulDiv(YLow-YUp,90,200);
      Ball(cp.x,cp.y,i ,clRed);
      Ball(cp.x+MulDiv(i,25,80),cp.y,MulDiv(i,62,80),clWhite);
    end;
    flagTunisia:TunisiaBaseFlag;
    flagTurkey:begin
      RightRatioSize(360,240);
      TurkeyBase(Xleft,Yup,XRight,Ylow,clRed,clWhite);
    end;
    flagTurkishRepublicOfNorthernCyprus:begin
      RightRatioSize(360,240);
      TurkeyBase(Xleft,Yup,XRight,Ylow,clWhite,clRed);
      y:=YLow-Yup;
      dy:=MulDiv(y,60,600);
      Rectangular(Xleft,Yup+dy,XRight,Yup+dy+dy,clRed);
      Rectangular(Xleft,YLow-dy-dy,XRight,YLow-dy,clRed);
    end;
    flagUmmAlQaiwain:begin
      RightRatioSize(2,1);
      Rectangular(Xleft,Yup,XRight,Ylow,clWhite);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      Rectangular(Xleft+x Div 6,Yup,XRight,Ylow,TColor($0000bf));
      Ball(cp.x,cp.y,MulDiv(y,150,450),clWhite);
      Ball(Xleft+MulDiv(x,340,900),cp.y,MulDiv(y,185,450),TColor($0000bf));
      FivePointStarBasic(cp.x,cp.y,MulDiv(y,83,500),clWhite);
    end;
    flagUzbekistan:begin
      RightRatioSize(2,1);
      BiColorFlagHorizontal(TColor($00CFA141), clLime);
      i:=round((YLow-YUp)/250*80);
      Rectangular(Xleft,Yup+i,XRight,Ylow-i,clRed);
      i:=round((YLow-YUp)/250*85) ;
      Rectangular(Xleft,Yup+i,XRight,Ylow-i,clWhite);
      r:=(XRight-XLeft)/500;
      HalfBall(XLeft+round(r*40),YUp+round(r*10),
              XLeft+round(r*75) ,YUp+round(r*70),atLeft,clWhite);
      HalfBall(XLeft+round(r*47),YUp+round(r*11),
              XLeft+round(r*75) ,YUp+round(r*69),atLeft,TColor($00CFA141));
      for i:=1 to 5
        do FivePointStar(XLeft+round(r*(70+i*24)),YUp+Round(r*58),
                      XLeft+round(r*(70+i*24+12)),YUp+Round(r*70),clWhite);
      for i:=1 to 4
        do FivePointStar(XLeft+round(r*(70+24+i*24)),YUp+Round(r*34),
                      XLeft+round(r*(70+24+i*24+12)),YUp+Round(r*46),clWhite);
      for i:=1 to 3
        do FivePointStar(XLeft+round(r*(70+48+i*24)),YUp+Round(r*10),
                      XLeft+round(r*(70+48+i*24+12)),YUp+Round(r*24),clWhite);
    end;
    flagWesternSahara:begin
      RightRatioSize(2,1);
      TriColorFlagHorizontal(clBlack, clWhite, clGreen);
      Triagle(XLeft,YUp,Xleft+MulDiv((XRight-Xleft),168,504),YLow,atRight,clRed);
      y:=YUp+(YLow-YUp) div 2;
      Ball(XLeft+MulDiv((XRight-Xleft),252,504),y,
        MulDiv((XRight-Xleft),30,504),clRed);
      Ball(XLeft+MulDiv((XRight-Xleft),262,504),y,
        MulDiv((XRight-Xleft),30,504),clWhite);
      y:=YLow-YUp;
      FivePointStar(XLeft+MulDiv((XRight-Xleft),257-20,504),YUp+MulDiv(y,106,252),
        XLeft+MulDiv((XRight-Xleft),257+20,504),YUp+MulDiv(y,146,252),clRed);
    end;
  end;
end;

procedure TCustomFlag.ShapeFlags;
  procedure CalcDxDy(var dx1,dy1,dx2,dy2:Integer; e,d,wDIVs,hDIVs:Extended);
  var ax,ay,bx,by:Integer;
  begin
    ax:=Round(wDIVs*e); ay:=Round(hDIVs*e);
    bx:=Round(hDIVs*d); by:=Round(wDIVs*d);
    dx1:=ax+bx; dy1:=by-ay;
    dx2:=ax-bx; dy2:=-by-ay;
  end;
  procedure DrawCenterCircles(canvas:TCanvas; cx,cy,dx,dy,r0,r1:Integer;
                            t:Extended);
  var sx,sy:Integer;
  begin
    with canvas do
      begin
        pen.Color:=clBlue; Brush.color:=clBlue;
        Ellipse(cx-r0,cy-r0,cx+r0,cy+r0);
        pen.Color:=clRed; Brush.color:=clRed;
        sx:=Round(r0*cos(t)); sy:=Round(r0*sin(t));
        Chord(cx-r0,cy-r0,cx+r0,cy+r0,cx+sx,cy+sy,cx-sx,cy-sy);

        sx:=cx-dx; sy:=cy-dy;
        pen.Color:=clRed; Brush.color:=clRed;
        Ellipse(sx-r1,sy-r1,sx+r1,sy+r1);

        sx:=cx+dx; sy:=cy+dy;
        pen.Color:=clBlue; Brush.color:=clBlue;
        Ellipse(sx-r1,sy-r1,sx+r1,sy+r1);
      end;
  end;
  procedure SKDrawCenterCircles(cp:TPoint;w,h:integer; s,sc:extended);
  var t,r,k:extended;
      dx,dy,r1,r0:integer;
  begin
    r0:=Round(sc*24); t:=h/w;
    r:=sc*12; r1:=Round(r);
    k:=r/s; dx:=Round(k*w); dy:=Round(k*h);
    DrawCenterCircles(canvas,cp.x, cp.y,dx,dy,r0,r1,t);
  end;
  procedure SKDrawBox(x1,y1,x4,y4,x2,y2,x3,y3:integer;Color:TColor);
  var po1,po2,po3,po4:TPoint;
  begin
    Po1:=Point(x1,y1); Po4:=Point(x4,y4);
    Po2:=Point(x2,y2); Po3:=Point(x3,y3);
    FourCornered(Po1,Po2,Po3,Po4,Color);
  end;
  procedure SKDrawTopRightBox(cpx,cpy,ax1,ay1,ax2,ay2,bx1,by1,bx2,by2:integer;
     Color:TColor);
  begin
    SKDrawBox(cpx+ax1,cpy+ay1,cpx+ax2,cpy+ay2,
              cpx+bx1,cpy+by1,cpx+bx2,cpy+by2,Color);
  end;
  procedure SKDrawLowLeftBox(cpx,cpy,ax1,ay1,ax2,ay2,bx1,by1,bx2,by2:integer;
     Color:TColor);
  begin
    SKDrawBox(cpx-ax1,cpy-ay1,cpx-ax2,cpy-ay2,
              cpx-bx1,cpy-by1,cpx-bx2,cpy-by2,Color);
  end;
  procedure SKDrawLowRightBox(cpx,cpy,ax1,ay1,ax2,ay2,bx1,by1,bx2,by2:integer;
     Color:TColor);
  begin
    SKDrawBox(cpx+ax1,cpy-ay1,cpx+ax2,cpy-ay2,
              cpx+bx1,cpy-by1,cpx+bx2,cpy-by2,Color);
  end;
  procedure SKDrawFourBox(cpx,cpy,ax1,ay1,ax2,ay2,bx1,by1,bx2,by2:integer;
     Color:TColor);
  begin
    SKDrawTopRightBox(cpx,cpy,ax1,ay1,ax2,ay2,bx1,by1,bx2,by2,Color);
    SKDrawBox(cpx-ax1,cpy+ay1,cpx-ax2,cpy+ay2,
              cpx-bx1,cpy+by1,cpx-bx2,cpy+by2,Color);
    SKDrawLowLeftBox(cpx,cpy,ax1,ay1,ax2,ay2,bx1,by1,bx2,by2,Color);
    SKDrawLowRightBox(cpx,cpy,ax1,ay1,ax2,ay2,bx1,by1,bx2,by2,Color);
  end;
  procedure PaintIndiaStar(Canvas:TCanvas; cx,cy,r,r2:integer;StarColor:TColor);
  var fii,dfii:Extended;
    P:array [0..47] of TPoint;
    i,x,y:Integer;
  begin
    dfii:=Pi/24;
    for i:=0 to 47 do
      begin
        fii:=i*dfii;
        if odd(i) then
          begin
            x:=cx+Round(r2*cos(fii));
            y:=cy-Round(r2*sin(fii));
          end
        else
          begin
            x:=cx+Round(r*cos(fii));
            y:=cy-Round(r*sin(fii));
        end;
        P[i]:=Point(x,y);
      end;
    Canvas.Pen.color:=StarColor;
    Canvas.Brush.color:=StarColor;
    Canvas.Polygon(P);
  end;
  procedure PaintSetOfCircles(cx,cy,r:integer;Color:TColor);
  var fii,dfii,shfii:Extended;
    i,x,y,ir:Integer;
  begin
    ir:= round( r/13 );
    if ir > 1 then
      begin
        dfii:=Pi/12; shfii:=Pi/24;
        for i:=0 to 23 do
          begin
            fii:=shfii+i*dfii;
            x:=cx+Round(r*cos(fii));
            y:=cy-Round(r*sin(fii));
            Ball(x,y,ir,Color);
          end;
      end;
  end;
 procedure RecCross(cp:TPoint;ax1,ay1,ax2,ay2,bx1,by1,bx2,by2:integer);
 var po1,po2,po3,po4:tpoint;
 begin
      Po1:=Point(cp.x-ax1,cp.y-ay1);
      Po2:=Point(cp.x-ax2,cp.y-ay2);
      Po3:=Point(cp.x-bx1,cp.y-by1);
      po4:=Point(cp.x-bx2,cp.y-by2);
      FourCornered(Po1,Po2,Po3,Po4,clWhite);
      Po1:=Point(cp.x+ax1,cp.y-ay1);
      Po2:=Point(cp.x+ax2,cp.y-ay2);
      Po3:=Point(cp.x+bx1,cp.y-by1);
      po4:=Point(cp.x+bx2,cp.y-by2);
      FourCornered(Po1,Po2,Po3,Po4,clWhite);
      Po1:=Point(cp.x+ax1,cp.y+ay1);
      Po2:=Point(cp.x+ax2,cp.y+ay2);
      Po3:=Point(cp.x+bx1,cp.y+by1);
      po4:=Point(cp.x+bx2,cp.y+by2);
      FourCornered(Po1,Po2,Po3,Po4,clWhite);
      Po1:=Point(cp.x-ax1,cp.y+ay1);
      Po2:=Point(cp.x-ax2,cp.y+ay2);
      Po3:=Point(cp.x-bx1,cp.y+by1);
      po4:=Point(cp.x-bx2,cp.y+by2);
      FourCornered(Po1,Po2,Po3,Po4,clWhite);
 end;
 procedure CanadianMaplePaint(x1,y1,x2,y2:integer);
 var PaintRect : TRect;
 begin
   Canvas.Pen.color:=clRed;
   Canvas.Brush.color:=clRed;
   PaintRect:=Rect(x1,y1,x2,y2);
   PaintCanadianMaple(Canvas,PaintRect,0.0);
 end;
  procedure EthiopiaStar(cpx,cpy,r:integer);
    type TAu1=array[1..8] of TPoint;
         TAu2=array[1..4] of TPoint;
    const
      ESP1:TAu1=((X:0;y:-100),(x:22;y:-32),(x:74;y:-32),(x:64;y:-23),
       (x:-13;y:-23),(x:-10;y:-32),(x:13;y:-32),(x:-5;y:-87));
      ESP2:TAu1=((X:-95;y:-32),(x:-22;y:-32),(x:-7;y:-81),(x:-3;y:-67),
       (x:-27;y:4),(x:-32;y:0),(x:-26;y:-23),(x:-84;y:-23));
      ESP3:TAu1=((X:-80;y:-19),(x:-66;y:-19),(x:-5;y:25),(x:-12;y:31),
       (x:-32;y:17),(x:-48;y:71),(x:-59;y:79),(x:-38;y:13));
      ESP4:TAu1=((X:-40;y:56),(x:23;y:10),(x:25;y:19),(x:7;y:32),
       (x:52;y:67),(x:56;y:79),(x:0;y:38),(x:-43;y:69));
      ESP5:TAu1=((X:16;y:-19),(x:25;y:-19),(x:32;y:2),(x:79;y:-32),
       (x:94;y:-32),(x:35;y:11),(x:51;y:60),(x:40;y:53));
      E2T1:TAu2=((x:-59;y:-83),(x:-68;y:-82),(x:-30;y:-37),(x:-27;y:-39));
      E2T2:TAu2=((x:29;y:-37),(x:26;y:-39),(x:58;y:-83),(x:61;y:-80));
      E2T3:TAu2=((x:44;y:15),(x:45;y:12),(x:95;y:29),(x:94;y:33));
      E2T4:TAu2=((x:-3;y:47),(x:3;y:47),(x:3;y:100),(x:-3;y:100));
      E2T5:TAu2=((x:-44;y:16),(x:-46;y:13),(x:-97;y:29),(x:-97;y:32));
    procedure ReadPoints(t:integer;var ATAu1:Tau1;Var AA2:TAu2);
    begin
      Case t of
       1: begin ATAu1:=ESP1; AA2:=E2T1; end;
       2: begin ATAu1:=ESP2; AA2:=E2T2; end;
       3: begin ATAu1:=ESP3; AA2:=E2T3; end;
       4: begin ATAu1:=ESP4; AA2:=E2T4; end;
       5: begin ATAu1:=ESP5; AA2:=E2T5; end;
      end;
    end;
    procedure Scale(cpx,cpy,r:integer;var AA1:TAu1;Var AA2:TAu2);
    var i:integer;
    begin
      for i:= 1 to 8 do
        AA1[i]:=point(MulDiv(AA1[i].x,r,100)+cpx,MulDiv(AA1[i].y,r,100)+cpy);
      for i:= 1 to 4 do
        AA2[i]:=point(MulDiv(AA2[i].x,r,100)+cpx,MulDiv(AA2[i].y,r,100)+cpy);
    end;
    Var i:integer; ETAu:TAu1; EA2:TAu2;
  begin
    if r> 10 then begin
      for i:= 1 to 5 do
        begin
          ReadPoints(i,ETAu,EA2);
          Scale(cpx,cpy,r,ETAu,EA2);
          EightCornered(ETAu[1],ETAu[2],ETAu[3],ETAu[4],ETAu[5],ETAu[6],
            ETAu[7],ETAu[8],TColor($16ddf9));
          FourCornered(EA2[1],EA2[2],EA2[3],EA2[4],TColor($16ddf9));
        end;
     end else
       FivePointStarBasic(cpx,cpy,r,TColor($16ddf9));
  end;
  procedure CoatOfSlovak(RT1:TRect);

      procedure box(RT:TRect;col:TColor);
      var
        P1,P2,P3,P4:tpoint;
        y1,y2,x3:integer;
      begin
        p1 :=CenterPoint(RT);
        y1 := RT.Top + MulDiv(RT.Bottom-RT.Top,150-133,150);
        y2 := RT.Top + MulDiv(RT.Bottom-RT.Top,50,150);
        x3 := MulDiv(RT.Right-RT.Left,2,105);
        p1.x := RT.Left;
        p2 := point(RT.Right,p1.y);
        p3 := point(RT.Right,y2);
        p4 := point(RT.Left,y2);
        FourCornered(p1,p2,p3,p4,col);
        p1 := point(RT.Left+x3,y1);
        p2 := point(RT.Right-x3,y1);
        p3 := point(RT.Right,y2);
        p4 := point(RT.Left,y2);
        FourCornered(p1,p2,p3,p4,col);
      end;
      procedure base(RT:TRect;col:TColor;sy:integer);
      var
        p1:TPoint;
        x1,y1,x2,y2,sx,ex,ey,r//,ry
        :integer;
      begin
        p1:=CenterPoint(RT);
        X2:=RT.Right;
        r:=MulDiv(RT.Right-RT.Left,105-23,105);
        X1:=RT.Right-r*2;
        y1:=RT.Bottom-MulDiv(RT.Bottom-RT.Top,150-133+58,150)-r;
        y2:=y1+r*2;
        sx := p1.x;
        ex:=RT.Right;
        ey := RT.Top;
        canvas.Brush.Color:=col; canvas.Pen.Color:=col;
        Canvas.Pie(x1,y1,x2,y2,sx,sy,ex,ey);
        X1:=RT.Left;
        ex:=RT.Left;
        X2:=RT.Left+r*2;
        Canvas.Pie(x1,y1,x2,y2,ex,ey,sx,sy);
        box(RT,col);
      end;
      procedure SlovakCross(RT:TRect);
      var   h,w,x1,y1,x2,y2,x3,y3,x4,y4:integer;
      begin
        h:=RT.Bottom-RT.Top;
        w:=RT.Right-RT.Left;
        if w > 49
          then begin
            x3 := MulDiv(w,20,105);
            x1 := RT.Left+x3;
            x2 := RT.Right-x3;
            y2 := RT.Top + MulDiv(h,150-102+10,150);
            y1 := RT.Top + MulDiv(h,150-64,150);
            Rectangular(x1,y1,x2,y2,clWhite);

            x4 := MulDiv(w,47,105);
            x1 := RT.Left+x4;
            x2 := RT.Right-x4;
            y2 := RT.Top + MulDiv(h,150-52,150);
            Rectangular(x1,y1,x2,y2,clWhite);

            y2 := RT.Top + MulDiv(h,150-102+10,150);
            x3 := MulDiv(RT.Right-RT.Left,25,105);
            x1 := RT.Left+x3;
            x2 := RT.Right-x3;
            y1 := RT.Top + MulDiv(h,150-129,150);
            Rectangular(x1,y1,x2,y2,clWhite);

            x3 := MulDiv(w,5,105);
            x1 := RT.Left+x3;
            x2 := RT.Left+x4;
            y1 := RT.Top + MulDiv(h,150-73,150);
            y2 := RT.Top + MulDiv(h,150-52,150);
            HalfBall(x1,y1,x2,y2,atLeft,clRed);
            x1 := RT.Right-x4;
            x2 := RT.Right-x3;
            HalfBall(x1,y1,x2,y2,atRight,clRed);

            y1 := RT.Top + MulDiv(h,150-96,150);
            y2 := RT.Top + MulDiv(h,150-84,150);
            HalfBall(x1,y1,x2,y2,atRight,clRed);
            x1 := RT.Left+x3;
            x2 := RT.Left+x4;
            HalfBall(x1,y1,x2,y2,atLeft,clRed);

            x4 := MulDiv(w,45,105);
            x2 := RT.Left+x4;
            y1 := RT.Top + MulDiv(h,150-124,150);
            y2 := RT.Top + MulDiv(h,150-108,150);
            HalfBall(x1,y1,x2,y2,atLeft,clRed);
            x1 := RT.Right-x4;
            x2 := RT.Right-x3;
            HalfBall(x1,y1,x2,y2,atRight,clRed);

            y1 := RT.Top + MulDiv(h,150-127,150);
            x4 := MulDiv(w,47,105);
            x3 := MulDiv(w,42,105);
            HalfBall(RT.Left+x3,y1,RT.Left+x4,y2,atUp,clRed);
            HalfBall(RT.Right-x4,y1,RT.Right-x3,y2,atUp,clRed);

            x3 := MulDiv(w,35,105);
            x1 := RT.Left+x3;
            x2 := RT.Right-x3;
            y1 := RT.Top + MulDiv(h,150-133,150);
            y2 := RT.Top + MulDiv(h,150-126,150);
            HalfBall(x1,y1,x2,y2,atDown,clRed);

            x3 := MulDiv(w,45,105);
            x4 := MulDiv(w,20,105);
            x2 := RT.Left+x3;
            x1 := RT.Left+x4;
            y2 := RT.Top + MulDiv(h,150-116,150);
            Rectangular(x1,y1,x2,y2,clRed);
            x1 := RT.Right-x4;
            x2 := RT.Right-x3;
            Rectangular(x1,y1,x2,y2,clRed);

            x4 := MulDiv(w,28,105);
            x3 := MulDiv(w,25,105);
            x1 := RT.Left+x3;
            x2 := RT.Left+x4;
            y1 := RT.Top + MulDiv(h,150-102+10,150);
            y2 := RT.Top + MulDiv(h,150-102-10,150);
            HalfBall(x1,y1,x2,y2,atRight,clRed);
            x1 := RT.Right-x4;
            x2 := RT.Right-x3;
            HalfBall(x1,y1,x2,y2,atLeft,clRed);

            x4 := MulDiv(w,22,105);
            x3 := MulDiv(w,20,105);
            x1 := RT.Left+x3;
            x2 := RT.Left+x4;
            y1 := RT.Top + MulDiv(h,150-78+10,150);
            y2 := RT.Top + MulDiv(h,150-78-10,150);
            HalfBall(x1,y1,x2,y2,atRight,clRed);
            x1 := RT.Right-x4;
            x2 := RT.Right-x3;
            HalfBall(x1,y1,x2,y2,atLeft,clRed);
          end else begin // small cross
            x3 := MulDiv(w,5,105);
            x4 := MulDiv(w,35,105);
            x1 := RT.Left+w div 2;
            x2 := RT.Right-x4;
            y1 := RT.Top + MulDiv(h,150-124,150);
            y2 := RT.Top + MulDiv(h,150-52,150);
            y3 := RT.Top + MulDiv(h,150-102+5,150);
            y4 := RT.Top + MulDiv(h,150-102-5,150);
            CrossBasicPaint(clWhite,RT.Left+x4,y1,RT.Right-x4,y2,x1-x3,x1+x3,y3,y4);
            x4 := MulDiv(w,28,105);
            y3 := RT.Top + MulDiv(h,150-78+5,150);
            y4 := RT.Top + MulDiv(h,150-78-5,150);
            CrossBasicPaint(clWhite,RT.Left+x4,y1,RT.Right-x4,y2,x1-x3,x1+x3,y3,y4);
          end;
      end;
      procedure smallCoat(RT:TRect);
      var  Po1,Po2,Po3,Po4,Po5,Po6,Po7:TPoint ;
           x,y,y1:integer;
      begin
        Po1:=point(RT.Left,RT.Top);
        Po2:=point(RT.Right,RT.Top);
        y:= RT.Bottom-RT.Top;
        y1:= MulDiv(y,63,133);
        Po3:=point(RT.Right,RT.Top+y1);
        Po7:=point(RT.Left,RT.Top+y1);
        y1:= MulDiv(y,100,133);
        x:= MulDiv(RT.Right-RT.Left,10,105);
        Po4:=point(RT.Right-x,RT.Top+y1);
        Po6:=point(RT.Left+x,RT.Top+y1);
        Po5:=point(RT.Left+(RT.Right-RT.Left)div 2,RT.Bottom);
        SevenCornered2(Po1,Po2,Po3,Po4,Po5,Po6,Po7,clWhite,clRed);
        Po1:=point(Po6.X+1,Po6.Y);
        Po2:=point(Po5.X,Po5.Y-1);
        Po3:=point(Po4.X-1,Po4.Y);
        Po4:=point(Po5.X,RT.Top+MulDiv(y,80,133));
        FourCornered(Po1,Po2,Po3,Po4,ColorskBlue);
        SlovakCross(RT);
      end;
    var
        RT,RT2:TRect;
        P1,P2,P3{,P4}:tpoint;

        r,x,y,x1,y1,x2,y2,x3,sy,x4,ey:integer;
    begin
      if (RT1.Right-RT1.Left)>41 then
        begin
      RT2 := RT1;
      RT2.bottom:=RT2.bottom-10;

      p1:=CenterPoint(RT2);
      y1:=muldiv(RT2.Bottom-RT2.Top,75,155);
      x1:=MulDiv(RT2.Right-RT2.Left,5,105);
      RT:=Rect(RT2.Left+x1,p1.y-y1,RT2.Right-x1,p1.y+y1);
      X2:=RT.Right;
      r:=MulDiv(RT.Right-RT.Left,105-23,105);
      X1:=RT.Right-r*2;
      y1:=RT.Bottom-MulDiv(RT.Bottom-RT.Top,150-133+58,150)-r;
      y2:=y1+r*2;
      x3 := p1.x;
      x4 := p1.x;
      sy := RT.Bottom;
      ey := RT.Top;

      base(RT2,clWhite,RT.Bottom+10);
      base(RT,clRed,RT.Bottom);

      canvas.Brush.Color:=ColorskBlue; canvas.Pen.Color:=ColorskBlue;
      X1:=RT.Left;
      X2:=RT.Left+r*2;


      y := RT.Bottom-MulDiv(RT.Bottom-RT.Top,33,150);
      x := RT.Right-MulDiv(RT.Right-RT.Left,91,105);
      Canvas.Pie(x1,y1,x2,y2,x,y,x3,sy);

      x := RT.Left+MulDiv(RT.Right-RT.Left,90,105);
      X1:=RT.Right-r*2;
      X2:=RT.Right;

      Canvas.Pie(x1,y1,x2,y2,x3,sy,x,y);

      x3 := MulDiv(RT.Right-RT.Left,20,105);
      x1 := RT.Left+x3;
      x2 := RT.Right-x3;
      y1 := RT.Top + MulDiv(RT.Bottom-RT.Top,50,150);
      y2 := RT.Top + MulDiv(RT.Bottom-RT.Top,110,150);
      Rectangular(x1,y1,x2,y2,clRed);
      SlovakCross(RT);
      y1:=RT.Bottom-RT.Top;
      Ball(p1.x,RT.Top + MulDiv(y1,114,150),MulDiv(y1,17,150),ColorskBlue);

      canvas.Brush.Color:=ColorskBlue; canvas.Pen.Color:=ColorskBlue;
      P2.x:=MulDiv(RT.Right-RT.Left,27,105);
      P3.x:=MulDiv(RT.Right-RT.Left,12,105);
      x1:=p1.x+P2.x-P3.x;
      x2:=p1.x+P2.x+P3.x;
      y1:=RT.Top+MulDiv(RT.Bottom-RT.Top,105,150);
      y2:=RT.Top+MulDiv(RT.Bottom-RT.Top,129,150);
      Canvas.Pie(x1,y1,x2,y2,x3,y2,x1,(y1+y2)div 2);
      x1:=p1.x-P2.x-P3.x;
      x2:=p1.x-P2.x+P3.x;
      Canvas.Pie(x1,y1,x2,y2,x3,y2,x1,(y1+y2)div 2);
        end
        else smallcoat(RT1);
    end;

var PaintRect : TRect;
    x,y,hw, h,w:integer;
    po1,po2,po3,po4,Po5,po6,po7,cp:TPoint;
    ax1,ay1,ax2,ay2,bx1,by1,bx2,by2:Integer;
    e,s,sc,d,wDIVs,hDIVs:Extended;
begin
  case FFlag of
    flagArizona:begin
      RightRatioSize(900,600);
      BiColorFlagHorizontal(TColor($300abf),TColor($682800));
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      ay1:=MulDiv(y,63,600);
      ay2:=MulDiv(y,188,600);
      po1:=point(XLeft, YUp+ay1);
      po2:=point(XLeft, YUp+ay2);
      TrianglePoints(Po1,Po2,cp,TColor($00d7fe));
      po1:=point(XRight, YUp+ay1);
      po2:=point(XRight, YUp+ay2);
      TrianglePoints(Po1,Po2,cp,TColor($00d7fe));
      po1:=point(XLeft+MulDiv(x,110,900), Yup);
      po2:=point(XLeft+MulDiv(x,242,900), Yup);
      TrianglePoints(Po1,Po2,cp,TColor($00d7fe));
      po1:=point(XLeft+MulDiv(x,336,900), Yup);
      po2:=point(XLeft+MulDiv(x,413,900), Yup);
      TrianglePoints(Po1,Po2,cp,TColor($00d7fe));
      po1:=point(XLeft+MulDiv(x,486,900), Yup);
      po2:=point(XLeft+MulDiv(x,563,900), Yup);
      TrianglePoints(Po1,Po2,cp,TColor($00d7fe));
      po1:=point(XLeft+MulDiv(x,656,900), Yup);
      po2:=point(XLeft+MulDiv(x,788,900), Yup);
      TrianglePoints(Po1,Po2,cp,TColor($00d7fe));
      FivePointStarBasic(cp.x,cp.y,MulDiv(x,150,900),TColor($175cce));
    end;
    flagBarbados:begin
      RightRatioSize(6,4);
      TriColorFlagVertical(clNavy,clYellow,clNavy);
      Canvas.Pen.color:=clBlack;
      Canvas.Brush.color:=clBlack;
      cp:=CenterPoint(Rect(Xleft,Yup,XRight,Ylow));
      x:=MulDiv((XRight-XLeft)div 2,18,60);
      y:=MulDiv((YLow-Yup) div 2,16,32);
      PaintRect:=Rect(cp.x-x,cp.y-y,cp.x+x,cp.y+y);
      PaintBarbadosTrident(Canvas,PaintRect,0.0);
    end;
    flagBosniaHerzegovina:begin
      RightRatioSize(200,100);
      Rectangular(Xleft,Yup,XRight,Ylow,clNavy);
      x:=XRight-XLeft;
      y:=YLow-YUp;
      Triangular(XLeft+MulDiv(x,53,200),Yup,XLeft+MulDiv(x,153,200),
      YLow, pi/2,1.0,clYellow);
      for h:= 1 to 7 do
        begin
          hw:=h*125;
          FivePointStar(XLeft+MulDiv(x,253+hw,2000),YUp+MulDiv(y,-106+hw,1000),
          XLeft+MulDiv(x,434+hw,2000),YUp+MulDiv(y,66+hw,1000),clYellow);
       end;
      po1:=point(XLeft+MulDiv(x,309,2000), Yup);
      po2:=point(XLeft+MulDiv(x,288,2000), YUp+MulDiv(y,66,1000));
      po3:=point(XLeft+MulDiv(x,344,2000), YUp+MulDiv(y,25,1000));
      po4:=point(XLeft+MulDiv(x,400,2000), YUp+MulDiv(y,66,1000));
      po5:=point(XLeft+MulDiv(x,378,2000), Yup);
      FiveCornered(po1,po2,po3,po4,po5,clYellow);
      po1:=point(XLeft+MulDiv(x,1344,2000), YUp+MulDiv(y,894,1000));
      po2:=point(XLeft+MulDiv(x,1322,2000), YUp+MulDiv(y,959,1000));
      po3:=point(XLeft+MulDiv(x,1253,2000), YUp+MulDiv(y,959,1000));
      po4:=point(XLeft+MulDiv(x,1309,2000), Ylow);
      po5:=point(XLeft+MulDiv(x,1378,2000), YLow);
      po6:=point(XLeft+MulDiv(x,1434,2000), YUp+MulDiv(y,959,1000));
      po7:=point(XLeft+MulDiv(x,1377,2000), YUp+MulDiv(y,959,1000));
      SevenCornered(po1,po2,po3,po4,po5,po6,po7,clYellow);
    end;
    flagCanadian:begin
      RightRatioSize(2,1);
      QuadColorFlagVertical(clRed,clWhite,clWhite,clRed);
      cp:=CenterPoint(Rect(Xleft,Yup,XRight,Ylow));
      x:=MulDiv((XRight-XLeft)div 2,25,64);
      y:=MulDiv((YLow-Yup) div 2,27,32);
      CanadianMaplePaint(cp.x-x,cp.y-y,cp.x+x,cp.y+y);
    end;
    flagDamietta:begin
      RightRatioSize(326,217);
      Rectangular(XLeft,YUp,XRight,YLow,TColor($943100));
      x:=XRight-Xleft;
      y:=YLow-YUp;
      ax1:=Xleft+MulDiv(x,168,326);
      ay1:=Yup+MulDiv(y,44,217);
      ax2:=MulDiv(x,76,326);
      ay2:=Yup+MulDiv(y,151,217);
      po1:=point(ax1,ay1);
      po2:=point(Xleft+ax2,ay2);
      TrianglePoints(po1,po2,point(ax1,ay2),clWhite);
      bx1:=MulDiv(x,179,326);
      by1:=MulDiv(y,85,217);
      po1:=point(Xleft+bx1,Yup+by1);
      po3:=point(Xleft+MulDiv(x,237,326),po2.y);
      TrianglePoints(po1,point(po1.x,po2.y),po3,clWhite);
      Line(ax1+1,ay1,ax1+1,ay2,TColor($943100));
      ax1:=MulDiv(x,263,326);
      ay1:=Yup+MulDiv(y,174,217);
      ax2:=MulDiv(x,244,326);
      ay2:=MulDiv(y,159,217);
      po1:=point(Xleft+ax1,ay1);
      po2:=point(Xleft+ax2,Yup+ay2);
      TrianglePoints(po1,po2,point(Xleft+ax2,ay1),clYellow);
      bx1:=Xleft+MulDiv(x,65,326);
      by1:=yup+MulDiv(y,157,217);
      po2:=point(Xleft+MulDiv(x,82,326),ay1);
      bx2:=Xleft+MulDiv(x,241,326);
      po3:=point(bx2,ay1);
      po4:=point(bx2,by1);
      FourCornered(point(bx1,by1),po2,po3,po4,clYellow);
      Line(bx1,by1-1,bx2,by1-1,TColor($943100));
    end;
    flagDelta:begin
      RightRatioSize(5,3);
      Rectangular(XLeft,YUp,XRight,YLow,TColor($009600));
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      ax1:=MulDiv(x,238,1000);
      ay1:=MulDiv(y,210,600);
      ay2:=MulDiv(y,112,600);
      bx1:=MulDiv(x,192,1000);
      by1:=MulDiv(y,175,600);
      po1:=point(cp.x-MulDiv(x,29,1000), cp.y-ay1);
      po2:=point(cp.x+MulDiv(x,39,1000), cp.y-ay1);
      po3:=point(cp.x+ax1, cp.y+ay2);
      po4:=point(cp.x+bx1, cp.y+by1);
      po5:=point(cp.x-MulDiv(x,180,1000), cp.y+by1);
      po6:=point(cp.x-MulDiv(x,232,1000), cp.y+ay2);
      SixCornered(po1,po2,po3,po4,po5,po6,clYellow);
      by2:=MulDiv(y,104,600);
      po1:=point(cp.x+MulDiv(x,4,1000), cp.y-MulDiv(y,132,600));
      po2:=point(cp.x+MulDiv(x,140,1000), cp.y+by2);
      po3:=point(cp.x-MulDiv(x,124,1000), cp.y+by2);
      TrianglePoints(po1,po2,po3,TColor($009600));
      Ball(XLeft+MulDiv(x,508,1000),YUp+MulDiv(y,329,600),
        MulDiv(y,65,600),clYellow);
    end;
    flagDenver:begin
      RightRatioSize(5,3);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      ay1:=Yup+MulDiv(y,470,600);
      Rectangular(Xleft,Yup,XRight,ay1,TColor($6f1628));
      Ball(cp.x,YUp+MulDiv(y,166,600),MulDiv(x,108,1000),clYellow);
      po1:=point(XLeft, YUp+MulDiv(y,357,600));
      ax1:=MulDiv(x,200,1000);
      po2:=point(XLeft+ax1, YUp+MulDiv(y,150,600));
      po3:=point(cp.x, YUp+MulDiv(y,470,600));
      po4:=point(XRight-ax1, Po2.y);
      po5:=point(XRight, Po1.y);
      po6:=point(XRight, YLow);
      po7:=point(XLeft, YLow);
      SevenCornered(po1,po2,po3,po4,po5,po6,po7,clWhite);
      po1.y:= YUp+MulDiv(y,468,600);
      po2.y:= YUp+MulDiv(y,264,600);
      po3.y:= Ylow;
      po4.y:= Po2.y;
      po5.y:= Po1.y;
      SevenCornered(po1,po2,po3,po4,po5,po6,po7,clRed);
    end;
    flagEthiopia:begin
      RightRatioSize(2,1);
      TriColorFlagHorizontal(TColor($3f6b00),TColor($16ddf9),TColor($283de2));
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      Ball(cp.x,cp.y,MulDiv(y,126,500),TColor($dba500));
      EthiopiaStar(cp.x,cp.y,MulDiv(y,100,500));
    end;
    flagIndia:begin
      RightRatioSize(3,2);
      TriColorFlagHorizontal(TColor($0066f5), clWhite,TColor($518700));
      hw:=MulDiv((Ylow-Yup),90,600);
      cp:=CenterPoint(Rect(Xleft,Yup,XRight,Ylow));
      Ball(cp.x,cp.y,hw,TColor($594400));
      h:=MulDiv(hw,180,200);
      Ball(cp.x,cp.y,h,clWhite);
      PaintSetOfCircles(cp.x,cp.y,h,TColor($594400));
      PaintIndiaStar(Canvas,cp.x,cp.y,h,h div 3,TColor($594400));
      Ball(cp.x,cp.y,MulDiv(hw,48,200),clWhite);
      Ball(cp.x,cp.y,MulDiv(hw,32,200),TColor($594400));
    end;
    flagIsrael:begin
      RightRatioSize(220,160);
      Rectangular(Xleft,Yup,XRight,Ylow,clWhite);
      y:=YLow-YUp;
      x:=MulDiv(y,15,160);
      y:=MulDiv(y,40,160);
      Rectangular(Xleft,YUp+x,XRight,Yup+y,clBlue);
      Rectangular(Xleft,YLow-y,XRight,YLow-x,clBlue);
      cp:=CenterPoint(Rect(Xleft,Yup,XRight,Ylow));
      y:=MulDiv(YLow-YUp,33,160);
      x:=round(y*sqrt(3)/2);
      if y>6
        then StarOfDavid(cp.x-x,cp.y-y,cp.x+x,cp.y+y,clBlue,clWhite)
        else LineStarOfDavid(cp.x-x,cp.y-y,cp.x+x,cp.y+y,clBlue);
    end;
    flagJollyRoger:begin
      RightRatioSize(3,2);
      Rectangular(XLeft,YUp,XRight,YLow,clBlack);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      ax1:=MulDiv(x,125,960);
      ay1:=cp.y-MulDiv(y,190,600);
      HalfBall(cp.x-ax1,cp.y-MulDiv(y,251,600),cp.x+ax1,ay1,atUp,clWhite);
      ax2:=MulDiv(x,100,960);
      ay2:=cp.y-MulDiv(y,42,600);
      bx1:=MulDiv(x,45,960);
      by1:=cp.y-MulDiv(y,20,600);
      bx2:=MulDiv(x,40,960);
      by2:=cp.y-MulDiv(y,4,600);
      po1:=point(cp.X+ax2, ay2);
      po2:=point(cp.x+ax1, ay1);
      po3:=point(cp.X-ax1, ay1);
      po4:=point(cp.X-ax2, ay2);
      po5:=point(cp.X-bx1, by1);
      po6:=point(cp.X+bx1, by1);
      SixCornered(po1,po2,po3,po4,po5,po6,clWhite);
      ax1:=MulDiv(x,145,960);
      ay1:=cp.y-MulDiv(y,220,600);
      ax2:=MulDiv(x,85,960);
      ay2:=cp.y-MulDiv(y,83,600);
      HalfBall(cp.x-ax1,ay1,cp.x-ax2,ay2,atLeft,clWhite);
      HalfBall(cp.x+ax2,ay1,cp.x+ax1,ay2,atRight,clWhite);


      ax1:=MulDiv(x,73,960);
      ay1:=cp.y-MulDiv(y,13,600);
      HalfBall(cp.x-ax1,ay1,cp.x+ax1,cp.y+MulDiv(y,40,600),atDown,clWhite);
      ax1:=MulDiv(x,59,960);
      HalfBall(cp.x-ax1,ay1,cp.x+ax1,cp.y+MulDiv(y,20,600),atDown,clBlack);
      ax1:=MulDiv(x,40,960);
      ay1:=cp.y+MulDiv(y,33,600);
      HalfBall(cp.x-ax1,ay1,cp.x+ax1,cp.y+MulDiv(y,55,600),atDown,clWhite);

      po1:=point(cp.X+bx1, by1);
      po2:=point(cp.X-bx1, by1);
      po3:=point(cp.X-bx2, by2);
      po5:=point(cp.X+bx2, by2);
      FiveCornered(po1,po2,po3,cp,po5,clWhite);
      ax1:=MulDiv(x,55,960);
      ay1:=cp.y-MulDiv(y,105,600);
      hw:=MulDiv(y,35,600);
      ball(cp.x-ax1,ay1,hw,clBlack);
      ball(cp.x+ax1,ay1,hw,clBlack);

      
      if y >20
        then begin
          ax1:=MulDiv(x,10,960);
          ay1:=cp.y-MulDiv(y,45,600);
          hw:=MulDiv(y,15,600);
          ball(cp.x-ax1,ay1,hw,clBlack);
          ball(cp.x+ax1,ay1,hw,clBlack);
        end;
      ax2:=MulDiv(x,25,960);
      ay2:=cp.y-MulDiv(y,45,600);
      po1:=point(cp.X+ax2, ay2);
      po2:=point(cp.X-ax2, ay2);
      trianglepoints( po1,po2,point(cp.x,cp.y-MulDiv(y,75,600)),clBlack);
      
      ax1:=MulDiv(x,150,960);
      ay1:=cp.y+MulDiv(y,75,600);
      hw:=MulDiv(y,30,600);
      ball(cp.x-ax1,ay1,hw,clWhite);
      ball(cp.x+ax1,ay1,hw,clWhite);
      ax2:=MulDiv(x,165,960);
      ay2:=cp.y+MulDiv(y,100,600);
      hw:=MulDiv(y,30,600);
      ball(cp.x-ax2,ay2,hw,clWhite);
      ball(cp.x+ax2,ay2,hw,clWhite);
      bx1:=MulDiv(x,125,960);
      by1:=cp.y+MulDiv(y,230,600);
      hw:=MulDiv(y,31,600);
      ball(cp.x-bx1,by1,hw,clWhite);
      ball(cp.x+bx1,by1,hw,clWhite);
      bx2:=MulDiv(x,150,960);
      by2:=cp.y+MulDiv(y,205,600);
      hw:=MulDiv(y,32,600);
      ball(cp.x-bx2,by2,hw,clWhite);
      ball(cp.x+bx2,by2,hw,clWhite);
      Po1:=Point(cp.x-ax1,ay1);
      Po2:=Point(cp.x-ax2,ay2);
      Po3:=Point(cp.x+bx1,by1);
      po4:=Point(cp.x+bx2,by2);
      FourCornered(Po1,Po2,Po3,Po4,clWhite);
      Po1:=Point(cp.x+ax1,ay1);
      Po2:=Point(cp.x+ax2,ay2);
      Po3:=Point(cp.x-bx1,by1);
      po4:=Point(cp.x-bx2,by2);
      FourCornered(Po1,Po2,Po3,Po4,clWhite);
    end;
    flagLombardy:begin
      RightRatioSize(3,2);
      Rectangular(XLeft,YUp,XRight,YLow,TColor($40a000));
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      hw:=MulDiv(y,75,600);
      ax1:=MulDiv(y,150,600);
      ay1:=MulDiv(y,60,600);
      ball(cp.x-ax1,cp.y-ay1,hw,clWhite);
      ball(cp.x+ax1,cp.y+ay1,hw,clWhite);
      ball(cp.x+ay1,cp.y-ax1,hw,clWhite);
      ball(cp.x-ay1,cp.y+ax1,hw,clWhite);
      Po1:=Point(cp.x-ax1,cp.y-ay1);
      Po2:=Point(cp.x+ay1,cp.y-ax1);
      po3:=Point(cp.x+ax1,cp.y+ay1);
      Po4:=Point(cp.x-ay1,cp.y+ax1);
      FourCornered(Po1,Po2,Po3,Po4,clWhite);
      hw:=MulDiv(y,40,600);
      ax1:=MulDiv(y,95,600);
      ay1:=MulDiv(y,45,600);
      ball(cp.x+ax1,cp.y-ay1,hw,TColor($40a000));
      ball(cp.x-ax1,cp.y+ay1,hw,TColor($40a000));
      ball(cp.x-ay1,cp.y-ax1,hw,TColor($40a000));
      ball(cp.x+ay1,cp.y+ax1,hw,TColor($40a000));
    end;
    flagMadeira:begin
      RightRatioSize(3,2);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      TriColorFlagVertical(TColor($cf1800),clYellow,TColor($cf1800));
      ax1:=MulDiv(y,138,600);
      ay1:=MulDiv(y,80,600);
      ax2:=MulDiv(y,112,600);
      ay2:=MulDiv(y,40,600);
      Po1:=Point(cp.x-ax1,cp.y-ay1);
      Po2:=Point(cp.x-ax1,cp.y+ay1);
      Po3:=Point(cp.x-ax2,cp.y+ay2);
      po4:=Point(cp.x-ax2,cp.y-ay2);
      FourCornered(Po1,Po2,Po3,Po4,TColor($1600ff));
      Po1:=Point(cp.x+ax1,cp.y-ay1);
      Po2:=Point(cp.x+ax1,cp.y+ay1);
      Po3:=Point(cp.x+ax2,cp.y+ay2);
      po4:=Point(cp.x+ax2,cp.y-ay2);
      FourCornered(Po1,Po2,Po3,Po4,TColor($1600ff));
      Po1:=Point(cp.x-ay1,cp.y-ax1);
      Po2:=Point(cp.x+ay1,cp.y-ax1);
      Po3:=Point(cp.x+ay2,cp.y-ax2);
      po4:=Point(cp.x-ay2,cp.y-ax2);
      FourCornered(Po1,Po2,Po3,Po4,TColor($1600ff));
      Po1:=Point(cp.x-ay1,cp.y+ax1);
      Po2:=Point(cp.x+ay1,cp.y+ax1);
      Po3:=Point(cp.x+ay2,cp.y+ax2);
      po4:=Point(cp.x-ay2,cp.y+ax2);
      FourCornered(Po1,Po2,Po3,Po4,TColor($1600ff));
      CrossBasicPaint(TColor($1600ff),cp.x-ax1,cp.y-ax1,cp.x+ax1,cp.y+ax1,
        cp.x-ay2,cp.X+ay2,cp.y-ay2,cp.y+ay2 );
      ay2:=MulDiv(y,13,600);
      CrossBasicPaint(clWhite,cp.x-ax2,cp.y-ax2,cp.x+ax2,cp.y+ax2,
        cp.x-ay2,cp.X+ay2,cp.y-ay2,cp.y+ay2 );
    end;
    flagMariEl:begin
      RightRatioSize(900,600);
      Rectangular(XLeft,YUp,XRight,YLow,clWhite);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      ay1:=MulDiv(y,180,600);
      Rectangular(XLeft,YUp,XRight,YUp+ay1,TColor($bc7500));
      Rectangular(XLeft,Ylow-ay1,XRight,Ylow,TColor($2824b3));
      ax1:=MulDiv(y,48,600);
      ay1:=MulDiv(y,99,600);
      ax2:=MulDiv(y,100,600);
      ay2:=MulDiv(y,45,600);
      Po1:=Point(cp.x-ax1,cp.y-ay1);
      Po2:=Point(cp.x+ax2,cp.y+ay2);
      Po3:=Point(cp.x+ax1,cp.y+ay1);
      po4:=Point(cp.x-ax2,cp.y-ay2);
      FourCornered(Po1,Po2,Po3,Po4,TColor($2824b3));
      Po1:=Point(cp.x+ax1,cp.y-ay1);
      Po2:=Point(cp.x-ax2,cp.y+ay2);
      Po3:=Point(cp.x-ax1,cp.y+ay1);
      po4:=Point(cp.x+ax2,cp.y-ay2);
      FourCornered(Po1,Po2,Po3,Po4,TColor($2824b3));
      ax1:=MulDiv(y,38,600);
      Po1:=Point(cp.x-ax1,cp.y);
      Po2:=Point(cp.x,cp.y+ax1);
      Po3:=Point(cp.x+ax1,cp.y);
      po4:=Point(cp.x,cp.y-ax1);
      FourCornered(Po1,Po2,Po3,Po4,clWhite);
      ax1:=MulDiv(y,20,600);
      Po1:=Point(cp.x-ax1,cp.y);
      Po2:=Point(cp.x,cp.y+ax1);
      Po3:=Point(cp.x+ax1,cp.y);
      po4:=Point(cp.x,cp.y-ax1);
      FourCornered(Po1,Po2,Po3,Po4,TColor($2824b3));
      ax1:=MulDiv(y,70,600);
      ay1:=MulDiv(y,75,600);
      ax2:=MulDiv(y,77,600);
      ay2:=MulDiv(y,68,600);
      bx1:=MulDiv(y,38,600);
      by1:=MulDiv(y,30,600);
      bx2:=MulDiv(y,31,600);
      by2:=MulDiv(y,37,600);
      RecCross(cp,ax1,ay1,ax2,ay2,bx1,by1,bx2,by2);
      ax1:=MulDiv(y,8,600);
      ay1:=MulDiv(y,44,600);
      ax2:=MulDiv(y,15,600);
      ay2:=MulDiv(y,52,600);
      bx1:=MulDiv(y,55,600);
      by1:=MulDiv(y,14,600);
      bx2:=MulDiv(y,46,600);
      by2:=MulDiv(y,7,600);
      RecCross(cp,ax1,ay1,ax2,ay2,bx1,by1,bx2,by2);
      if x>50 then begin
        ax1:=MulDiv(y,77,600);
        ay1:=MulDiv(y,52,600);
        ax2:=MulDiv(y,47,600);
        ay2:=MulDiv(y,22,600);
        bx1:=MulDiv(y,55,600);
        by1:=MulDiv(y,14,600);
        bx2:=MulDiv(y,84,600);
        by2:=MulDiv(y,44,600);
        RecCross(cp,ax1,ay1,ax2,ay2,bx1,by1,bx2,by2);
        ax1:=MulDiv(y,47,600);
        ay1:=MulDiv(y,83,600);
        ax2:=MulDiv(y,15,600);
        ay2:=MulDiv(y,52,600);
        bx1:=MulDiv(y,23,600);
        by1:=MulDiv(y,45,600);
        bx2:=MulDiv(y,54,600);
        by2:=MulDiv(y,75,600);
        RecCross(cp,ax1,ay1,ax2,ay2,bx1,by1,bx2,by2);
      end;
    end;
    flagMatoGrosso:begin
      RightRatioSize(1000,700);
      Rectangular(XLeft,YUp,XRight,YLow,TColor($6f1628));
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      ax1:=MulDiv(y,415,700);
      ay1:=MulDiv(y,265,700);
      Po1:=Point(cp.x-ax1,cp.y);
      Po2:=Point(cp.x,cp.y-ay1);
      po3:=Point(cp.x+ax1,cp.y);
      Po4:=Point(cp.x,cp.y+ay1);
      FourCornered(Po1,Po2,Po3,Po4,clWhite);
      hw:=MulDiv(y,175,700);
      ball(cp.x,cp.y,hw,TColor($3f9200));
      FivePointStarBasic(cp.x,cp.y,hw,TColor($00c3f8));
    end;
    flagMorocco:begin
      RightRatioSize(900,600);
      Rectangular(XLeft,YUp,XRight,YLow,TColor($3131de));
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      ax1:=MulDiv(x,106,900);
      ay1:=MulDiv(y,36,600);
      ax2:=MulDiv(x,59,900);
      ay2:=MulDiv(y,20,600);
      Po1:=Point(cp.x-ax1,cp.y-ay1);
      Po2:=Point(cp.x+ax1,cp.y-ay1);
      po3:=Point(cp.x+ax2,cp.y-ay2);
      Po4:=Point(cp.x-ax2,cp.y-ay2);
      FourCornered(Po1,Po2,Po3,Po4,TColor($008400));
      bx1:=MulDiv(x,66,900);
      by1:=MulDiv(y,90,600);
      bx2:=MulDiv(x,35,900);
      by2:=MulDiv(y,47,600);
      Po1:=Point(cp.x-bx1,cp.y+by1);
      Po4:=Point(cp.x-bx2,cp.y+by2);
      FourCornered(Po1,Po2,Po3,Po4,TColor($008400));
      Po1:=Point(cp.x-ax1,cp.y-ay1);
      Po2:=Point(cp.x+bx1,cp.y+by1);
      po3:=Point(cp.x+bx2,cp.y+by2);
      Po4:=Point(cp.x-ax2,cp.y-ay2);
      FourCornered(Po1,Po2,Po3,Po4,TColor($008400));
      h  :=MulDiv(y,114,600);
      hw :=MulDiv(y,60,600);
      Po1:=Point(cp.x,cp.y-h);
      Po4:=Point(cp.x,cp.y-hw);
      FourCornered(Po1,Po2,Po3,Po4,TColor($008400));
      Po2:=Point(cp.x-bx1,cp.y+by1);
      po3:=Point(cp.x-bx2,cp.y+by2);
      FourCornered(Po1,Po2,Po3,Po4,TColor($008400));
    end;
    flagNagornoKarabakh:begin
      RightRatioSize(360,180);
      TriColorFlagHorizontal(clRed,clBlue,TColor($00C0ff){Orange});
      x:=XRight-Xleft;
      y:=YLow-YUp;
      ax1:=MulDiv(x,27*2,360);
      ay1:=MulDiv(y,20,180);
      Rectangular(XRight-ax1,Yup,XRight,Yup+ay1,clWhite);
      Rectangular(XRight-ax1,YLow-ay1,XRight,YLow,clWhite);
      ax2:=MulDiv(x,27,360);
      Rectangular(XRight-ax2*3,Yup+ay1,XRight-ax2,Yup+ay1*2,clWhite);
      Rectangular(XRight-ax2*3,YLow-ay1*2,XRight-ax2,YLow-ay1,clWhite);
      ay2:=MulDiv(y,1,3);
      bx1:=MulDiv(x,27*4,360);
      Rectangular(XRight-bx1,Yup+ay1*2,XRight-ax1,Yup+ay2,clWhite);
      Rectangular(XRight-bx1,YLow-ay2,XRight-ax1,YLow-ay1*2,clWhite);
      bx2:=MulDiv(x,27*5,360);
      by1:=MulDiv(y,4,9);
      Rectangular(XRight-bx2,Yup+ay2,XRight-ax2*3,Yup+by1,clWhite);
      Rectangular(XRight-bx2,YLow-by1,XRight-ax2*3,YLow-ay2,clWhite);
      Rectangular(XRight-bx2-ax2,YUp+by1,XRight-bx1,YLow-by1,clWhite);
    end;
    flagNato:begin
      RightRatioSize(1000,750);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($7d2400));
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      hw:=MulDiv(x,147,1000);
      Ball(cp.x,cp.y,hw,clWhite);
      h:=MulDiv(x,137,1000);
      if hw <= h then dec(h);
      Ball(cp.x,cp.y,h,TColor($7d2400));
      if y> 20 then
        begin
          h:=MulDiv(y,75,750);
          hw:=MulDiv(x,217,1000);
          Triagle(cp.x-hw,cp.y-h,cp.x,cp.y+h,atLeft,TColor($7d2400));
          Triagle(cp.x,cp.y-h,cp.x+hw,cp.y+h,atRight,TColor($7d2400));
          Triagle(cp.x-h,cp.y-hw,cp.x+h,cp.y,atUp,TColor($7d2400));
          Triagle(cp.x-h,cp.y,cp.x+h,cp.y+hw,atDown,TColor($7d2400));
        end;
      by1:=MulDiv(y,370,750);
      ax1:=MulDiv(x,75,1000);
      ax2:=MulDiv(x,287,1000);
      Rectangular(Xleft+ax1,Yup+by1,Xleft+ax2,Ylow-by1,clWhite);
      Rectangular(XRight-ax2,Yup+by1,XRight-ax1,Ylow-by1,clWhite);
      ax1:=MulDiv(x,495,1000);
      by1:=MulDiv(y,75,750);
      by2:=MulDiv(y,162,750);
      Rectangular(Xleft+ax1,Yup+by1,XRight-ax1,Yup+by2,clWhite);
      Rectangular(Xleft+ax1,YLow-by2,XRight-ax1,YLow-by1,clWhite);
      ay1:=MulDiv(y,34,750);
      by1:=MulDiv(y,6,750);
      bx1:=MulDiv(x,169,1000);
      by2:=MulDiv(y,47,750);
      bx2:=MulDiv(x,42,1000);
      ay2:=MulDiv(y,186,750);
      po1:=point(cp.x,cp.y);
      po2:=point(cp.x-ay1, cp.y-ay1);
      po3:=point(cp.x-bx1, cp.y-by1);
      po4:=point(cp.x-bx2, cp.y-by2);
      po5:=point(cp.x, cp.y-ay2);
      FiveCornered(po1,po2,po3,po4,po5,clWhite);
      po2:=point(cp.x+ay1, cp.y+ay1);
      po3:=point(cp.x+bx1, cp.y+by1);
      po4:=point(cp.x+bx2, cp.y+by2);
      po5:=point(cp.x, cp.y+ay2);
      FiveCornered(po1,po2,po3,po4,po5,clWhite);
      po2:=point(cp.x-ay1, cp.y+ay1);
      po3:=point(cp.x-by1, cp.y+bx1);
      po4:=point(cp.x-by2, cp.y+bx2);
      po5:=point(cp.x-ay2, cp.y);
      FiveCornered(po1,po2,po3,po4,po5,clWhite);
      po2:=point(cp.x+ay1, cp.y-ay1);
      po3:=point(cp.x+by1, cp.y-bx1);
      po4:=point(cp.x+by2, cp.y-bx2);
      po5:=point(cp.x+ay2, cp.y);
      FiveCornered(po1,po2,po3,po4,po5,clWhite);
    end;
    flagNorthernCheyenne:begin
      RightRatioSize(1000,500);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($ff5300));
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      hw:=MulDiv(x,195,1000);
      ax2:=MulDiv(x,167,1000);
      Po1:=Point(cp.x-hw,cp.y-ax2);
      Po2:=Point(cp.x-ax2,cp.y-hw);
      po3:=Point(cp.x+hw,cp.y+ax2);
      Po4:=Point(cp.x+ax2,cp.y+hw);
      FourCornered(Po1,Po2,Po3,Po4,clWhite);
      Po1:=Point(cp.x+hw,cp.y-ax2);
      Po2:=Point(cp.x+ax2,cp.y-hw);
      po3:=Point(cp.x-hw,cp.y+ax2);
      Po4:=Point(cp.x-ax2,cp.y+hw);
      FourCornered(Po1,Po2,Po3,Po4,clWhite);
      bx1:=MulDiv(x,305,1000);
      Po1:=Point(XLeft+bx1,cp.y);
      Po2:=Point(cp.x,cp.y-hw);
      Po3:=Point(XRight-bx1,cp.y);
      Po4:=Point(cp.x,cp.y+hw);
      FourCornered(Po1,Po2,Po3,Po4,clWhite);
      hw:=MulDiv(x,139,1000);
      Po1:=Point(cp.x-hw,cp.y);
      Po2:=Point(cp.x,cp.y-hw);
      po3:=Point(cp.x+hw,cp.y);
      Po4:=Point(cp.x,cp.y+hw);
      FourCornered(Po1,Po2,Po3,Po4,TColor($ff5300));
    end;
    flagPadania:begin
      RightRatioSize(1000,500);
      Rectangular(Xleft,Yup,XRight,Ylow,clWhite);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      hw:=MulDiv(y,197,500);
      ball(cp.x,cp.y,hw,TColor($008000));
      hw:=MulDiv(y,161,500);
      ball(cp.x,cp.y,hw,clWhite);
      SunRay(cp, hw,TColor($008000));
    end;
    flagPembrokeshire:begin
      CrossBasic(TColor($e12e37),clYellow,100,60,44,12,24);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      po1:=point(cp.x, YUp+MulDiv(y,155,600));
      po2:=point(XLeft+MulDiv(x,354,1000), YUp+MulDiv(y,258,600));
      po3:=point(XLeft+MulDiv(x,410,1000), YUp+MulDiv(y,423,600));
      po4:=point(XRight-MulDiv(x,410,1000), YUp+MulDiv(y,423,600));
      po5:=point(XRight-MulDiv(x,354,1000), YUp+MulDiv(y,258,600));
      FiveCornered(Po1,Po2,Po3,Po4,Po5,clGreen);
      ay1:=YUp+MulDiv(y,178,600);
      po1:=point(cp.x,ay1 );
      ay2:=YUp+MulDiv(y,263,600);
      ax2:=MulDiv(x,377,1000);
      po2:=point(XLeft+ax2, ay2);
      bx1:=MulDiv(x,395,1000);
      by1:=YUp+MulDiv(y,320,600);
      po3:=point(XLeft+bx1, by1);
      po4:=point(cp.x, by1);
      FourCornered(Po1,Po2,Po3,Po4,clRed);
      po1:=point(cp.x,ay1 );
      po2:=point(XRight-ax2, ay2);
      po3:=point(XRight-bx1, by1);
      po4:=point(cp.x, by1);
      FourCornered(Po1,Po2,Po3,Po4,clWhite);
      bx2:=MulDiv(x,423,1000);
      by2:=YUp+MulDiv(y,405,600);
      po1:=point(cp.x,by1 );
      po2:=point(XLeft+bx1, by1);
      po3:=point(XLeft+bx2, by2);
      po4:=point(cp.x, by2);
      FourCornered(Po1,Po2,Po3,Po4,clWhite);
      po2:=point(XRight-bx1, by1);
      po3:=point(XRight-bx2, by2);
      FourCornered(Po1,Po2,Po3,Po4,clRed);
    end;
    flagPiemonte:begin
      RightRatioSize(3,2);
      Rectangular(XLeft,YUp,XRight,YLow,TColor($7c3c0c));
      x:=XRight-XLeft;
      y:=YLow-YUp;
      by2:=MulDiv(y,40,600);
      if by2<1 then by2:=1;
      ax1:=XLeft+by2;
      ax2:=XRight-by2;
      ay1:=Yup+by2;
      ay2:=Ylow-by2;
      Rectangular(ax1,ay1,ax2,ay2,clRed);
      CrossBasicPaint(clWhite,ax1,ay1,ax2,ay2,XLeft+MulDiv(x,385,900),
        XLeft+MulDiv(x,515,900),Yup+MulDiv(y,24,60),yUp+MulDiv(y,36,60));
      ay1:=Yup+MulDiv(y,80,600);
      ay2:=Yup+MulDiv(y,105,600);
      by1:=Yup+MulDiv(y,149,600);
      ax1:=XLeft+MulDiv(x,295,900);
      po1:=point(XLeft+MulDiv(x,270,900),ay1 );
      po2:=point(XLeft+MulDiv(x,246,900),by1 );
      po3:=point(XLeft+MulDiv(x,320,900),by1);
      po4:=point(ax1, ay2);
      po5:=point(ax1, ay1);
      FiveCornered(Po1,Po2,Po3,Po4,po5,TColor($7c3c0c));
      ax2:=XLeft+MulDiv(x,605,900);
      Rectangular(ax1,ay1,ax2,ay2,TColor($7c3c0c));
      po1.x:=XLeft+MulDiv(x,630,900);
      po2.x:=XLeft+MulDiv(x,654,900);
      po3.x:=XLeft+MulDiv(x,583,900);
      po4.x:=ax2;
      po5.x:=ax2;
      FiveCornered(Po1,Po2,Po3,Po4,po5,TColor($7c3c0c));
      po1:=point(XLeft+MulDiv(x,434,900),ay2 );
      po2.x:=XLeft+MulDiv(x,412,900);
      po3.x:=XLeft+MulDiv(x,488,900);
      po4.x:=XLeft+MulDiv(x,470,900);
      FourCornered(Po1,Po2,Po3,Po4,TColor($7c3c0c));
    end;
    flagRedCrystal:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,clWhite);
      cp:=CenterPoint(Rect(Xleft,Yup,XRight,Ylow));
      x:=MulDiv(XRight-XLeft,1,4);
      Diamond(cp.x+x,cp.x,cp.x-x,cp.y-x,cp.y,cp.y+x, clRed);
      x:=x div 2;
      Diamond(cp.x+x,cp.x,cp.x-x,cp.y-x,cp.y,cp.y+x, clWhite);
    end;
    flagRedShieldOfDavid:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,clWhite);
      cp:=CenterPoint(Rect(Xleft,Yup,XRight,Ylow));
      y:=MulDiv(YLow-YUp,45,106);
      x:=round(y*sqrt(3)/2);
      if y>10
        then StarOfDavid(cp.x-x,cp.y-y,cp.x+x,cp.y+y,clRed,clWhite)
        else LineStarOfDavid(cp.x-x,cp.y-y,cp.x+x,cp.y+y,clRed);
    end;
    flagSaba:begin
      RightRatioSize(3,2);
      BiColorFlagHorizontal(clRed,clBlue);
      cp:=CenterPoint(Rect(Xleft,Yup,XRight,Ylow));
      y:=(YLow-YUp) div 4;
      Diamond(XLeft,cp.x,XRight, Yup,cp.y,YLow, clWhite);
      FivePointStar(cp.x-y,cp.y-y,cp.x+y,cp.y+y,clYellow, 0.0);
    end;
    flagSaintVincentAndTheGrenadines:begin
      RightRatioSize(72,48);
      QuadColorFlagVertical(clBlue,clYellow,clYellow,clGreen);
      x:=XRight-XLeft;
      y:=YLow-YUp;
      Diamond(MulDiv(x,27,72)+XLeft,MulDiv(x,31,72)+XLeft,MulDiv(x,35,72)+XLeft,
        MulDiv(y,16,48)+Yup,y div 2+YUp,MulDiv(y,32,48)+Yup, clGreen);
      Diamond(MulDiv(x,37,72)+XLeft,MulDiv(x,41,72)+XLeft,MulDiv(x,45,72)+XLeft,
        MulDiv(y,16,48)+Yup,y div 2+YUp,MulDiv(y,32,48)+Yup, clGreen);
      Diamond(MulDiv(x,32,72)+XLeft,MulDiv(x,36,72)+XLeft,MulDiv(x,40,72)+XLeft,
        MulDiv(y,26,48)+Yup,MulDiv(y,34,48)+Yup,MulDiv(y,42,48)+Yup, clGreen);
    end;
    flagSingapore:begin
      RightRatioSize(4320,2880);
      BiColorFlagHorizontal(clRed,clWhite);
      x:=XRight-XLeft;
      y:=MulDiv(x,720,4320)+Yup;
      Ball(XLeft+MulDiv(x,910,4320),Y,MulDiv(x,530,4320),clWhite);
      Ball(XLeft+MulDiv(x,1200,4320),Y,MulDiv(x,580,4320),clRed);
      PaintRect:=Rect(XLeft+MulDiv(x,910,4320),y-MulDiv(x,304,4320),
        XLeft+MulDiv(x,910+304+128+128,4320),y+MulDiv(x,304,4320));
      CalculatePentagonPoints(PaintRect,Po1,Po2,Po3,Po4,Po5);
      hw:=MulDiv(x,128,4320);
      FivePointStar(Po1.X-hw, Po1.y-hw,Po1.X+hw,Po1.Y+hw ,clWhite);
      FivePointStar(Po2.X-hw, Po2.y-hw,Po2.X+hw,Po2.Y+hw ,clWhite);
      FivePointStar(Po3.X-hw, Po3.y-hw,Po3.X+hw,Po3.Y+hw ,clWhite);
      FivePointStar(Po4.X-hw, Po4.y-hw,Po4.X+hw,Po4.Y+hw ,clWhite);
      FivePointStar(Po5.X-hw, Po5.y-hw,Po5.X+hw,Po5.Y+hw ,clWhite);
    end;
    flagSlovakia:begin
      RightRatioSize(3,2);
      TriColorFlagHorizontal(clwhite,ColorskBlue, clRed);
      w:=MulDiv((XRight-Xleft),129,900);
      h:=w*165 div 115;
      x:=Xleft + MulDiv((XRight-Xleft),270,900);
      y:=Yup + MulDiv((Ylow-Yup),144,300);
      CoatOfSlovak(Rect(x-w,y-h,x+w,y+h));
    end;
    flagSouthAfrica:begin
      RightRatioSize(45,30);
      Rectangular(Xleft,Yup,XRight,Ylow,clGreen);
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      s:=sqrt(x*x+y*y)/2;
      Po1.x:=XLeft+DistCorner(X,Y,Y,X*3/45); Po1.y:=YLow;
      Po2.x:=XRight; Po2.y:=Ylow;
      po3.x:=XRight; Po3.y:=MulDiv(y,18,30)+Yup;
      Po4.x:=XLeft+ round((s+x*2)/5);
      Po4.y:=Po3.y;
      FourCornered(Po1,Po2,Po3,Po4,clWhite);
      Po1.y:=YUp;Po2.y:=Po1.Y;Po3.y:=yUp+MulDiv(y,12,30);Po4.y:=Po3.y;
      FourCornered(Po1,Po2,Po3,Po4,clWhite);
      Po1.x:=XLeft+DistCorner(X,Y,Y,X*5/45);
      Po4.x:=XLeft+ round((s+x)/3);
      Po3.y:=yUp+MulDiv(y,10,30);Po4.y:=Po3.y;
      FourCornered(Po1,Po2,Po3,Po4,clRed);
      Po1.y:=YLow;Po2.y:=Po1.Y;Po3.y:=yUp+MulDiv(y,20,30);Po4.y:=Po3.y;
      FourCornered(Po1,Po2,Po3,Po4,clBlue);
      y:=DistCorner(X,Y,x,3/45*x);
      Triagle(XLeft,YUp+y,cp.x-round(s/5),YLow-y,atRight,clYellow);
      y:=DistCorner(X,YLow-YUp,x,10/90*x);
      Triagle(XLeft,YUp+y,cp.x-round(s/3),YLow-y,atRight,clBlack);
    end;
    flagSouthKorea:begin
      RightRatioSize(144,96);
      Rectangular(Xleft,Yup,XRight,Ylow,clWhite);
      w:=XRight-Xleft; h:=Ylow-Yup; s:=sqrt(w*w+h*h); sc:=w/144;
      cp:=CenterPoint(Rect(Xleft,Yup,XRight,Ylow));

      SKDrawCenterCircles(cp,w,h,s,sc);
      
      e:=sc*36; d:=sc*12; wDIVs:=w/s; hDIVs:=h/s;
      CalcDxDy(ax1,ay1,ax2,ay2,e,d,wDIVs,hDIVs);
      e:=sc*52;
      CalcDxDy(bx1,by1,bx2,by2,e,d,wDIVs,hDIVs);
      SKDrawFourBox(cp.x,cp.y,ax1,ay1,ax2,ay2,bx1,by1,bx2,by2,clBlack);
      
      e:=sc*40;
      CalcDxDy(ax1,ay1,ax2,ay2,e,d,wDIVs,hDIVs);
      e:=sc*42;
      CalcDxDy(bx1,by1,bx2,by2,e,d,wDIVs,hDIVs);
      SKDrawFourBox(cp.x,cp.y,ax1,ay1,ax2,ay2,bx1,by1,bx2,by2,clWhite);

      e:=sc*46;
      CalcDxDy(ax1,ay1,ax2,ay2,e,d,wDIVs,hDIVs);
      e:=sc*48;
      CalcDxDy(bx1,by1,bx2,by2,e,d,wDIVs,hDIVs);
      SKDrawFourBox(cp.x,cp.y,ax1,ay1,ax2,ay2,bx1,by1,bx2,by2,clWhite);

      e:=sc*36; d:=sc*1;
      CalcDxDy(ax1,ay1,ax2,ay2,e,d,wDIVs,hDIVs);
      e:=sc*52;
      CalcDxDy(bx1,by1,bx2,by2,e,d,wDIVs,hDIVs);
      SKDrawLowRightBox(cp.x,cp.y,ax1,ay1,ax2,ay2,bx1,by1,bx2,by2,clWhite);
      e:=sc*40;
      CalcDxDy(bx1,by1,bx2,by2,e,d,wDIVs,hDIVs);
      SKDrawTopRightBox(cp.x,cp.y,ax1,ay1,ax2,ay2,bx1,by1,bx2,by2,clWhite);
      e:=sc*48;
      CalcDxDy(ax1,ay1,ax2,ay2,e,d,wDIVs,hDIVs);
      e:=sc*52;
      CalcDxDy(bx1,by1,bx2,by2,e,d,wDIVs,hDIVs);
      SKDrawTopRightBox(cp.x,cp.y,ax1,ay1,ax2,ay2,bx1,by1,bx2,by2,clWhite);
      e:=sc*42;
      CalcDxDy(ax1,ay1,ax2,ay2,e,d,wDIVs,hDIVs);
      e:=sc*46;
      CalcDxDy(bx1,by1,bx2,by2,e,d,wDIVs,hDIVs);
      SKDrawLowLeftBox(cp.x,cp.y,ax1,ay1,ax2,ay2,bx1,by1,bx2,by2,clWhite);
    end;
    flagTokyo:begin
      RightRatioSize(900,600);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($4a005a));
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      ay1:=MulDiv(y,183,600);
      ay2:=MulDiv(y,155,600);
      ax1:=MulDiv(x,21,900);
      ax2:=MulDiv(x,42,900);
      po1:=point(cp.x, cp.y-ay1);
      po2:=point(cp.X-ax1, cp.y-ay2);
      po3:=point(cp.X-ax1, cp.y+ay2);
      po4:=point(cp.x,cp.y+ay1);
      po5:=point(cp.X-ax1+ax2, cp.y+ay2);
      po6:=point(cp.X-ax1+ax2, cp.y-ay2);;
      SixCornered(po1,po2,po3,po4,po5,po6,clWhite);
      ay1:=MulDiv(y,90,600);
      by1:=MulDiv(y,60,600);
      by2:=MulDiv(y,95,600);
      ax1:=MulDiv(x,159,900);
      bx1:=MulDiv(x,147,900);
      bx2:=MulDiv(x,126,900);
      po1:=point(cp.x-ax1, cp.y-ay1);
      po2:=point(cp.X-bx1, cp.y-by1);
      po3:=point(cp.X+bx2, cp.y+by2);
      po4:=point(cp.X+ax1, cp.y+ay1);
      po5:=point(cp.X+bx1, cp.y+by1);
      po6:=point(cp.X-bx2, cp.y-by2);;
      SixCornered(po1,po2,po3,po4,po5,po6,clWhite);
      po1:=point(cp.x-ax1, cp.y+ay1);
      po2:=point(cp.X-bx1, cp.y+by1);
      po3:=point(cp.X+bx2, cp.y-by2);
      po4:=point(cp.X+ax1, cp.y-ay1);
      po5:=point(cp.X+bx1, cp.y-by1);
      po6:=point(cp.X-bx2, cp.y+by2);;
      SixCornered(po1,po2,po3,po4,po5,po6,clWhite);
      Ball(cp.x,cp.y,MulDiv(y,96,600),clWhite);
      Ball(cp.x,cp.y,MulDiv(y,65,600),TColor($4a005a));
      Ball(cp.x,cp.y,MulDiv(y,32,600),clWhite);
    end;
    flagToronto:begin
      RightRatioSize(2,1);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($904427));
      y:=YLow-YUp;
      h:=YUp+MulDiv(y,131,500);
      Rectangular(Xleft,Yup,XRight,h,clWhite);
      x:=XRight-XLeft;
      Rectangular(Xleft+MulDiv(x,265,1000),h,
        Xleft+MulDiv(x,536,1000),Ylow,clWhite);
      HalfBall(XLeft+MulDiv(x,100,1000),h,
        XRight,YUp+MulDiv(y,188,500),atDown,clWhite);
      h:=YUp+MulDiv(y,146,500);
      w:=XLeft+MulDiv(x,530,1000);
      Rectangular(Xleft,Yup,w,h,clWhite);
      HalfBall(XLeft,h,w,YUp+MulDiv(y,187,500),atDown,clWhite);
      w:=XLeft+MulDiv(x,734,1000);
      h:=YUp+MulDiv(y,50,500);
      HalfBall(XLeft,h,w,YUp+MulDiv(y,97,500),atDown,TColor($904427));
      Rectangular(Xleft,Yup,w,h,TColor($904427));
      h:=YUp+MulDiv(y,35,500);
      Rectangular(Xleft,Yup,XRight,h,TColor($904427));
      HalfBall(XLeft+MulDiv(x,100,1000),h,
        XRight,YUp+MulDiv(y,97,500),atDown,TColor($904427));
      Rectangular(Xleft+MulDiv(x,367,1000),Yup,
        Xleft+MulDiv(x,434,1000),YUp+MulDiv(y,350,500),TColor($904427));
      CanadianMaplePaint(Xleft+MulDiv(x,295,1000),YUp+MulDiv(y,305,500),
        Xleft+MulDiv(x,505,1000),YLow);
    end;
    flagVenda:begin
      RightRatioSize(3,2);
      x:=XRight-XLeft;
      TriColorFlagHorizontal(TColor($339900),TColor($2be9fc),TColor($003366));
      Rectangular(Xleft,Yup,XLeft+MulDiv(x,200,900),YLow,TColor($996633));
      y:=YLow-YUp;
      ay1:=YUp+MulDiv(y,228,600);
      ay2:=YUp+MulDiv(y,371,600);
      po1:=point(XLeft+MulDiv(x,438,900),ay1 );
      po2:=point(XLeft+MulDiv(x,484,900),ay1);
      po3:=point(XLeft+MulDiv(x,551,900), YUp+MulDiv(y,329,600));
      po4:=point(XLeft+MulDiv(x,621,900),ay1);
      po5:=point(XLeft+MulDiv(x,672,900),ay1);
      po6:=point(XLeft+MulDiv(x,566,900),ay2);
      po7:=point(XLeft+MulDiv(x,535,900),ay2);
      SevenCornered(po1,po2,po3,po4,po5,po6,po7,TColor($003366));
    end;
    flagYamaguchiKen:begin
      RightRatioSize(3,2);
      Rectangular(Xleft,Yup,XRight,Ylow,TColor($343c68));
      CpXYCalc(Xleft,Yup,XRight,Ylow,cp,x,y);
      Ball(cp.x,cp.y,MulDiv(y,170,600),clWhite);
      ay1:=yup+MulDiv(y,270,600);
      by1:=ay1+MulDiv(y,144,600);
      bx1:=MulDiv(x,182,900);
      bx2:=MulDiv(x,336,900);
      po1:=point(XLeft+bx1,ay1 );
      po2:=point(XLeft+bx2,by1 );
      po3:=point(XRight-bx2,by1);
      po4:=point(XRight-bx1, ay1);
      FourCornered(Po1,Po2,Po3,Po4,clWhite);
      Ball(cp.x,cp.y,MulDiv(y,100,600),TColor($343c68));
      ax2:=MulDiv(x,400,900);
      bx1:=Xleft+ax2;
      bx2:=XRight-ax2;
      TrianglePoints(point(cp.x,ay1),point(bx1,by1),point(bx2,by1),clWhite);
    end;
  end;
end;


end.
