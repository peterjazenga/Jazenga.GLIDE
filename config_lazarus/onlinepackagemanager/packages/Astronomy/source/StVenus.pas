{**********************************************************************                   
 Package pl_Astronomy.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit StVenus;

interface

uses
  StAstro;

function ComputeVenus(JD : Double) : TStEclipticalCord;


implementation

function GetLongitude(Tau, Tau2, Tau3, Tau4, Tau5 : Double) : Double;
var
  L0, L1,
  L2, L3,
  L4, L5  : Double;
begin
  L0 := 3.17614666770 * cos(0.00000000000 +     0.00000000000 * Tau)
      + 0.01353968419 * cos(5.59313319620 + 10213.28554600000 * Tau)
      + 0.00089891645 * cos(5.30650048470 + 20426.57109200000 * Tau)
      + 0.00005477201 * cos(4.41630652530 +  7860.41939240000 * Tau)
      + 0.00003455732 * cos(2.69964470780 + 11790.62908900000 * Tau)
      + 0.00002372061 * cos(2.99377539570 +  3930.20969620000 * Tau)
      + 0.00001664069 * cos(4.25018935030 +  1577.34354240000 * Tau)
      + 0.00001438322 * cos(4.15745043960 +  9683.59458110000 * Tau)
      + 0.00001317108 * cos(5.18668219090 +    26.29831980000 * Tau)
      + 0.00001200521 * cos(6.15357115320 + 30639.85663900000 * Tau)
      + 0.00000769314 * cos(0.81629615911 +  9437.76293490000 * Tau)
      + 0.00000761380 * cos(1.95014702120 +   529.69096509000 * Tau)
      + 0.00000707676 * cos(1.06466707210 +   775.52261132000 * Tau)
      + 0.00000584836 * cos(3.99839884760 +   191.44826611000 * Tau)
      + 0.00000499915 * cos(4.12340210070 + 15720.83878500000 * Tau)
      + 0.00000429498 * cos(3.58642859750 + 19367.18916200000 * Tau)
      + 0.00000326967 * cos(5.67736583710 +  5507.55323870000 * Tau)
      + 0.00000326221 * cos(4.59056473100 + 10404.73381200000 * Tau)
      + 0.00000231937 * cos(3.16251057070 +  9153.90361600000 * Tau)
      + 0.00000179695 * cos(4.65337915580 +  1109.37855210000 * Tau)
      + 0.00000155464 * cos(5.57043888950 + 19651.04848100000 * Tau)
      + 0.00000128263 * cos(4.22604493740 +    20.77539549200 * Tau)
      + 0.00000127907 * cos(0.96209822685 +  5661.33204920000 * Tau)
      + 0.00000105547 * cos(1.53721191250 +   801.82093112000 * Tau);

  L1 := 10213.52943100000 * cos(0.00000000000 + 0.00000000000 * Tau)
      + 0.00095707712 * cos(2.46424448980 + 10213.28554600000 * Tau)
      + 0.00014444977 * cos(0.51624564679 + 20426.57109200000 * Tau)
      + 0.00000213374 * cos(1.79547929370 + 30639.85663900000 * Tau)
      + 0.00000173904 * cos(2.65535879440 +    26.29831980000 * Tau)
      + 0.00000151669 * cos(6.10635282370 +  1577.34354240000 * Tau)
      + 0.00000082233 * cos(5.70234133730 +   191.44826611000 * Tau)
      + 0.00000069734 * cos(2.68136034980 +  9437.76293490000 * Tau)
      + 0.00000052408 * cos(3.60013087660 +   775.52261132000 * Tau)
      + 0.00000038318 * cos(1.03379038030 +   529.69096509000 * Tau)
      + 0.00000029633 * cos(1.25056322350 +  5507.55323870000 * Tau)
      + 0.00000025056 * cos(6.10664792860 + 10404.73381200000 * Tau);

  L2 :=
      + 0.00054127076 * cos(0.00000000000 +     0.00000000000 * Tau)
      + 0.00003891460 * cos(0.34514360047 + 10213.28554600000 * Tau)
      + 0.00001337880 * cos(2.02011286080 + 20426.57109200000 * Tau)
      + 0.00000023836 * cos(2.04592119010 +    26.29831980000 * Tau)
      + 0.00000019331 * cos(3.53527371460 + 30639.85663900000 * Tau)
      + 0.00000009984 * cos(3.97130221100 +   775.52261132000 * Tau)
      + 0.00000007046 * cos(1.51962593410 +  1577.34354240000 * Tau)
      + 0.00000006014 * cos(0.99926757893 +   191.44826611000 * Tau);

  L3 :=
      + 0.00000135742 * cos(4.80389020990 + 10213.28554600000 * Tau)
      + 0.00000077846 * cos(3.66876371590 + 20426.57109200000 * Tau)
      + 0.00000026023 * cos(0.00000000000 +     0.00000000000 * Tau);

  L4 :=
      + 0.00000114016 * cos(3.14159265360 +     0.00000000000 * Tau)
      + 0.00000003209 * cos(5.20514170160 + 20426.57109200000 * Tau)
      + 0.00000001714 * cos(2.51099591710 + 10213.28554600000 * Tau);

  L5 := 0.00000000874 * cos(3.14159265360 +     0.00000000000 * Tau);
  Result := (L0 + L1*Tau + L2*Tau2 + L3*Tau3 + L4*Tau4 + L5*Tau5);
end;

{-------------------------------------------------------------------------}

function GetLatitude(Tau, Tau2, Tau3, Tau4, Tau5 : Double) : Double;
var
  B0, B1,
  B2, B3,
  B4, B5  : Double;
begin
  B0 := 0.05923638472 * cos(0.26702775813 + 10213.28554600000 * Tau)
      + 0.00040107978 * cos(1.14737178110 + 20426.57109200000 * Tau)
      + 0.00032814918 * cos(3.14159265360 +     0.00000000000 * Tau)
      + 0.00001011392 * cos(1.08946123020 + 30639.85663900000 * Tau)
      + 0.00000149458 * cos(6.25390296070 + 18073.70493900000 * Tau)
      + 0.00000137788 * cos(0.86020146523 +  1577.34354240000 * Tau)
      + 0.00000129973 * cos(3.67152483650 +  9437.76293490000 * Tau)
      + 0.00000119507 * cos(3.70468812800 +  2352.86615380000 * Tau)
      + 0.00000107971 * cos(4.53903677650 + 22003.91463500000 * Tau);

  B1 := 0.00513347602 * cos(1.80364310800 + 10213.28554600000 * Tau)
      + 0.00004380100 * cos(3.38615711590 + 20426.57109200000 * Tau)
      + 0.00000199162 * cos(0.00000000000 +     0.00000000000 * Tau)
      + 0.00000196586 * cos(2.53001197490 + 30639.85663900000 * Tau);

  B2 := 0.00022377665 * cos(3.38509143880 + 10213.28554600000 * Tau)
      + 0.00000281739 * cos(0.00000000000 +     0.00000000000 * Tau)
      + 0.00000173164 * cos(5.25563766920 + 20426.57109200000 * Tau)
      + 0.00000026945 * cos(3.87040891570 + 30639.85663900000 * Tau);

  B3 := 0.00000646671 * cos(4.99166565280 + 10213.28554600000 * Tau)
      + 0.00000019952 * cos(3.14159265360 +     0.00000000000 * Tau)
      + 0.00000005540 * cos(0.77376923951 + 20426.57109200000 * Tau)
      + 0.00000002526 * cos(5.44493763020 + 30639.85663900000 * Tau);

  B4 := 0.00000014102 * cos(0.31537190181 + 10213.28554600000 * Tau);

  B5 := 0.00000000000;
  Result := (B0 + B1*Tau + B2*Tau2 + B3*Tau3 + B4*Tau4 + B5*Tau5);
end;

{-------------------------------------------------------------------------}

function GetRadiusVector(Tau, Tau2, Tau3, Tau4, Tau5 : Double) : Double;
var
  R0, R1,
  R2, R3,
  R4, R5  : Double;
begin
  R0 := 0.72334820905 * cos(0.00000000000 +     0.00000000000 * Tau)
      + 0.00489824185 * cos(4.02151832270 + 10213.28554600000 * Tau)
      + 0.00001658058 * cos(4.90206728010 + 20426.57109200000 * Tau)
      + 0.00001632093 * cos(2.84548851890 +  7860.41939240000 * Tau)
      + 0.00001378048 * cos(1.12846590600 + 11790.62908900000 * Tau)
      + 0.00000498399 * cos(2.58682187720 +  9683.59458110000 * Tau)
      + 0.00000373958 * cos(1.42314837060 +  3930.20969620000 * Tau)
      + 0.00000263616 * cos(5.52938185920 +  9437.76293490000 * Tau)
      + 0.00000237455 * cos(2.55135903980 + 15720.83878500000 * Tau)
      + 0.00000221983 * cos(2.01346776770 + 19367.18916200000 * Tau)
      + 0.00000125896 * cos(2.72769833560 +  1577.34354240000 * Tau)
      + 0.00000119467 * cos(3.01975365260 + 10404.73381200000 * Tau);

  R1 :=
      + 0.00034551039 * cos(0.89198710598 + 10213.28554600000 * Tau)
      + 0.00000234203 * cos(1.77224942710 + 20426.57109200000 * Tau)
      + 0.00000233998 * cos(3.14159265360 +     0.00000000000 * Tau);

  R2 :=
      + 0.00001406587 * cos(5.06366395190 + 10213.28554600000 * Tau)
      + 0.00000015529 * cos(5.47321687980 + 20426.57109200000 * Tau)
      + 0.00000013059 * cos(0.00000000000 +     0.00000000000 * Tau);

  R3 :=
      + 0.00000049582 * cos(3.22263554520 + 10213.28554600000 * Tau);

  R4 :=
      + 0.00000000573 * cos(0.92229697820 + 10213.28554600000 * Tau);

  R5 := 0.00000000000;
  Result := (R0 + R1*Tau + R2*Tau2 + R3*Tau3 + R4*Tau4 + R5*Tau5);
end;

{-------------------------------------------------------------------------}

function ComputeVenus(JD : Double) : TStEclipticalCord;
var
  Tau,
  Tau2,
  Tau3,
  Tau4,
  Tau5      : Double;
begin
  Tau  := (JD - 2451545.0) / 365250.0;
  Tau2 := sqr(Tau);
  Tau3 := Tau * Tau2;
  Tau4 := sqr(Tau2);
  Tau5 := Tau2 * Tau3;

  Result.L0 := GetLongitude(Tau, Tau2, Tau3, Tau4, Tau5);
  Result.B0 := GetLatitude(Tau, Tau2, Tau3, Tau4, Tau5);
  Result.R0 := GetRadiusVector(Tau, Tau2, Tau3, Tau4, Tau5);
end;


end.
