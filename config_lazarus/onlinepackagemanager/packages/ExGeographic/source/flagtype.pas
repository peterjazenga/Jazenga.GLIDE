{
    This file is part of the TFlagCompontPackage


  Copyright (C) 2004- Seppo S Finland
  Licence: modifiedLGPL (Same as FreePascal)

}

{**********************************************************************
 Package pl_ExGeographic
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit flagtype;

{$mode objfpc}{$H+}

interface

type
  TFlagType = (
    flagAalandIslands,flagAalsmeer,flagAbejorral,flagAboriginal,flagAbuDhabi,
    flagAcardia,flagAcre,flagAlabama,flagAlaska,flagAltai,flagAmbtMontfoort,
    flagAmstelveen,flagAmsterdam,flagAncapflag,flagAnfemflag,flagAnnam,
    flagAntiguaAndBarbuda,flagAppingedam,flagArabRevolt,flagArapaho,
    flagArgentina,flagArizona,flagArmenia,flagAruba,flagAssen,flagAtacama,
    flagAtica,flagAtlantium,flagAustAgder,flagAustralia,flagAustria,
    flagAzerbaijan,
    flagBaarn,flagBahamas,flagBahia,flagBahrain,flagBandung,flagBangladesh,
    flagBanskobystricky,flagBar,flagBarbados,flagBarotseland,flagBasque,
    flagBatasuna,flagBattleFlagOfTheUSConfederacy,flagBavaria,flagBedum,
    flagBelgium,flagBelgium1789,flagBenin,flagBilbao, flagBismarck,
    flagBlackFlag,flagBlackFlagWithOrangeCircle,flagBlackWhiteFlag,
    flagBlackWhiteChequeredFlag,flagBlueEnsign,flagBlueEstelada,flagBlueFlag,
    flagBorne,flagBornholm,flagBosniaHerzegovina,flagBotswana,flagBratislavsky,
    flagBrawandi,flagBreda,flagBremen,flagBreukelen,flagBudapest,flagBulgaria,
    flagBurgers,flagBurkinaFaso,flagBurundi,flagBusoga,
    flagCameroon,flagCanadian,flagCanaryIslands,flagCapeVerde,
    flagCashoubs,flagCatalonia,flagCentralAfricanRepublic,flagChagossians,
    flagChicago,flagChad,flagChin,flagChina,flagChile,flagChittagongHillTracts,
    flagChristian,flagChukotka,flagColombia,flagColombiaNativePeoples,
    flagColorado,flagComoros,flagConfederateNationalFlagSince1865,flagCongo,
    flagCongoKinshasa,flagCookIslands,flagCostaRica,flagCrimea,flagCuba,
    flagCuracao,flagCusco,flagCzech,
    flagDagestan,flagDamietta,flagDelft,flagDelta,flagDenison,flagDenmark,
    flagDenver,flagDevon,flagDjibouti,flagDoesburg,flagDominicanRepublic,
    flagDruzepeople,flagDubai,flagDuchyNassau1806,flagDuiven,
    flagDukeOfCornwall,flagDurham,flagDurhamNC,
    flagEastKarelia,flagEdamVolendam,flagEindhoven,flagElBierzo,flagElOro,
    flagElx,flagEngland,flagEnschede,flagEnsignRoyalAirForce,flagEntreRios,
    flagErzya,flagEsperanto,flagEstonia,flagEsztergom,flagEthiopia,
    flagEuropeanUnion,flagEwePeople,
    flagFaroeIslands,flagFinland,flagFrance,flagFrenchTunisia,flagFribourg,
    flagGabon, flagGagauzia,flagGambia,flagGambierIslands,flagGalicia,
    flagGarifuna,flagGelderland,flagGeldermalsen,flagGerman,flagGhana,flagGoes,
    flagGoias,flagGreece,flagGreenBlackFlag,flagGreenFlag,flagGreenland,
    flagGrenzmarkPosenWestPrussian,flagGroningen,flagGroningenCity,flagGuainia,
    flagGuayaquil,flagGuernsey,flagGuinea,flagGuineaBissau,
    flagHabsburg,flagHague,flagHaiti,flagHarenskarpel,flagHawaii,flagHeemstede,
    flagHeerde,flagHelgoland,flagHighlands,flagHinduSindhi,flagHiroshima,
    flagHonduras,flagHoogeveen,flagHuancayo,flagHuanuco,flagHuila,flagHungary,
    flagIceland,flagIFRCflag,flagIndia,flagIndianapolis,flagIndonesia,
    flagIngria,flagInNizam,flagIpiales,flagIraqiTurkmen,flagIreland,flagIsrael,
    flagItaly,flagIvoryCoast,
    flagJamaica,flagJapan,flagJewishMerchant1453,flagJollyRoger,flagJordan,
    flagKaliningrad,flagKalmarUnion,flagKareliaRussia,flagKarnten,
    flagKashmirIndependent,flagKatwijk,flagKhabarovskKrai,flagKingdomRedonda,
    flagKingdomTahiti,flagKingdomTalossa,flagKomia,flagKosicky,flagKuwait,
    flagKuyavianPomerania,
    flagLaarbeek,flagLaos,flagLatinMerchant1453,flagLatvia,flagLeeuwarden,
    flagLemsterland,flagLevis,flagLibya,flagLipkovo,flagLisse,flagLithuania,
    flagLivonians,flagLodzkie,flagLombardy,flagLuxemburg,flagLightBlueFlag,
    flagLuzern,
    flagMaasbree,flagMaastricht,flagMacedonia,flagMadagascar,flagMadeira,
    flagMadrid,flagMakran,flagMalacca,flagMaldives,flagMaldivesWarFlag,flagMali,
    flagManipur,flagManizales,flagMaori,flagMaranhao,flagMariEl,flagMars,
    flagMatoGrosso,flagMauritania,flagMauren,flagMauritius,
    flagMecklenburgWesternPomerania,flagMeerloWanssum,flagMeghalaya,flagMeijel,
    flagMerida,flagMiccosoukee,flagMicronesia,flagMikmaq,flagMillingenAanDeRijn,
    flagMilneBay,flagMisiones,flagMississippi,flagMizoram,flagMizoramIndia,
    flagMonaco,flagMonterey,flagMonteria,flagMoravia,flagMoresnet,flagMorocco,
    flagMuiden,flagMunster,flagMuslimMerchant1453,
    flagNagano,flagNagornoKarabakh,flagNamibia,flagNapo,flagNataliaRepublic,
    flagNato,flagNauru,flagNetherlands,flagNetherlandsAntilles,flagNeuchatel,
    flagNuevaEsparta,flagNewfoundland,flagNewZealand,flagNiger,flagNigeria,
    flagNijmegen,flagNitriansky,flagNiue,flagNordicCelticFlag,flagNormandy,
    flagNorthernSchleswigGermanMinority,flagNorthFrisia,flagNorthHolland,
    flagNorthKorea,flagNorthRhineWestphalia,flagNorthernCheyenne,
    flagNorthernNigeria,flagNorthumberland,flagNorway,flagNuenen,
    flagObdam,flagOcana,flagOceanCity,flagOgaden,flagOkinawa,flagOldenburg,
    flagOldenburgCity,flagOmmen,flagOpsterland,flagOrkney,flagOsaka,flagOss,
    flagOxapampa,
    flagPadania,flagPahang,flagPakistan,flagPalau,flagPalestine,
    FlagPalmyraAtoll,flagPanAfrican,flagPanama,flagPanSiberian,flagPara,
    flagParis,flagPastaza,flagPasto,flagPembrokeshire,flagPerak,flagPerlis,
    flagPeru,flagPiaui,flagPiemonte,flagPoland,flagPosen,flagPotosi,
    flagPresovsky,flagPrinsvlag,flagPuertoRico,flagPula,
    flagQatar,flagQuito,
    flagRajashtan,flagRedArmy,flagRedCrescent,flagRedBlackFlag,flagRedCross,
    flagRedCrystal,flagRedEnsign,flagRedEstelada,flagRedFlag,
    flagRedShieldOfDavid,flagReeuwijk,flagRheden,flagRijnwaarden,flagRoermond,
    flagRomanMerchant1453,flagRomania,flagRome,flagRondonia,flagRoraima,
    flagRossDependency,flagRotterdam,flagRussia,flagRwanda,
    flagSaar,flagSaba,flagSaguenay,flagSaintAlban,flagSaintDavid,flagSaintLucia,
    flagSaintMartin,flagSaintPiran,flagSaintVincentAndTheGrenadines,flagSakha,
    flagSami,flagSamoa,flagSanMarino,flagSantiagoDeCali,flagSaoTomeAndPrincipe,
    flagSariego,flagSaxony,flagScanianCrossFlag,flagSchellenberg,
    flagSchleswigHolstein,flagSchiedam,flagSchiermonnikoog,flagSchwyz,
    flagScillonianCross,flagScotland,flagSealand,flagSenegal,flagSenyera,
    flagSergipe,flagSeychelles,flagSFRYugoslavia,flagShetland,flagSharjah,
    flagSierraLeone,flagSingapore,
    flagSlaskie,flagSlovakia,flagSmaland,flagSneek,flagSolomonIslands,
    flagSolothurn,flagSomalia,flagSomiedo,flagSognOgFjordane,flagSouthAfrica,
    flagSouthKorea,flagSouthOssetia,flagSouthUist,flagSpain,flagSpectre,
    flagSteiermark,flagStrasbourg,flagSuchaBeskidzka,flagSudan,flagSuriname,
    flagSverdlovsk,flagSwalmen,flagSweden,flagSwitzerland,flagSyria,
    flagTaiwan,flagTallinn,flagTanzania,flagTatarstan,flagTenerife,flagTenessee,
    flagTerAar,flagTetovo,flagTexas,flagThailand,flagTholen,flagTicino,flagTogo,
    flagTokyo,flagTonga,flagTorneovalley,flagToronto,flagTracia,
    flagTransdniestr,flagTrenciansky,flagTriesen,flagTriesenberg,
    flagTrinidadAndTobago,flagTrnavsky,flagTunisia,flagTurkey,
    flagTurkishRepublicOfNorthernCyprus,flagTuvalu,flagTxita,
    flagUkraine,flagUmmAlQaiwain,flagUnitedArabEmirates,flagUnitedKingdom,
    flagUnitedSpace,flagUral,flagUSA,flagUtrecht,flagUtrechtCity,
    flagUttarPradesh,flagUzbekistan,
    flagVaduz,flagValkenburg,flagValdivia,flagVenda,flagVenezuela,flagVepsia,
    flagVietnam,flagVillaDeLeyva,flagVillavicencio,flagVojvodina,flagVolyn,
    flagVoortrekker,
    flagWallisAndFutuna,flagWarsaw,flagWashingtonDC,flagWesternSahara,
    flagWestPapua,flagWestPrussia,flagWestSomalia,flagWestvoorne,
    flagWhiteEnsign,flagWhiteFlag,flagWinterswijk,flagWojewodztwoPodlaskie,
    flagWoudenberg,flagWriezen,
    flagYamagata,flagYamaguchiKen,flagYellowFlag,flagYellowFlagWithRedStripes,
    flagYemen,
    flagZachPomorskie,flagZanzibar,flagZilinsky,flagZomi,flagZug,flagZurich,
    flagZwolle);
    
implementation

end.

