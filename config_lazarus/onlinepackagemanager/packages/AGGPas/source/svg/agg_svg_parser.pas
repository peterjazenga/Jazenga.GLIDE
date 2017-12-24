
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit  agg_svg_parser;

interface

{$DEFINE EXPAT_WRAPPER }
{$I agg_mode.inc }

uses

  //...................
  agg_file_utils,
  agg_platform_support,
  agg_pixelmap,
  //...................

  SysUtils,
  agg_basics,
  agg_color,
  agg_svg_path_tokenizer,
  agg_svg_path_renderer,
  agg_svg_exception,
  agg_trans_affine,
  agg_math_stroke,
  expat;

const
  buf_size = 512;

type
  parser_ptr = ^parser;

  parser = object
    m_path: path_renderer_ptr;
    m_tokenizer: path_tokenizer;

    m_buf, m_title: char_ptr;

    m_title_len: unsigned;
    m_title_flag, m_path_flag: boolean;
    m_attr_name, m_attr_value: char_ptr;

    m_attr_name_len, m_attr_name_aloc, m_attr_value_len, m_attr_value_aloc: unsigned;

    constructor Construct(path: path_renderer_ptr);
    destructor Destruct;

    procedure parse(fname: shortstring);
    function title: char_ptr;

    // XML event handlers
    procedure parse_attr(attr: char_ptr_ptr); overload;
    procedure parse_path(attr: char_ptr_ptr);
    procedure parse_poly(attr: char_ptr_ptr; close_flag: boolean);
    procedure parse_rect(attr: char_ptr_ptr);
    procedure parse_line(attr: char_ptr_ptr);
    procedure parse_style(str: agg_basics.char_ptr);
    procedure parse_transform(str: agg_basics.char_ptr);

    function parse_matrix(str: agg_basics.char_ptr): unsigned;
    function parse_translate(str: agg_basics.char_ptr): unsigned;
    function parse_rotate(str: agg_basics.char_ptr): unsigned;
    function parse_scale(str: agg_basics.char_ptr): unsigned;
    function parse_skew_x(str: agg_basics.char_ptr): unsigned;
    function parse_skew_y(str: agg_basics.char_ptr): unsigned;

    function parse_attr(Name, Value: agg_basics.char_ptr): boolean; overload;
    function parse_name_value(nv_start, nv_end: agg_basics.char_ptr): boolean;

    procedure copy_name(start, end_: agg_basics.char_ptr);
    procedure copy_value(start, end_: agg_basics.char_ptr);

  end;

{ GLOBAL PROCEDURES }
procedure start_element(Data: pointer; el: char_ptr; attr: char_ptr_ptr); {$IFDEF EXPAT_WRAPPER } cdecl; {$ENDIF }
procedure end_element(Data: pointer; el: char_ptr); {$IFDEF EXPAT_WRAPPER } cdecl; {$ENDIF }
procedure content(Data: pointer; s: char_ptr; len: int); {$IFDEF EXPAT_WRAPPER } cdecl; {$ENDIF }


implementation

{ LOCAL VARIABLES & CONSTANTS }
type
  named_color_ptr = ^named_color;

  named_color = record
    Name: array[0..21] of char;

    r, g, b, a: int8u;

  end;

const
  colors_num = 148;

  colors: array[0..colors_num - 1] of named_color =
    ((Name: 'aliceblue'; r: 240; g: 248; b: 255; a: 255),
    (Name: 'antiquewhite'; r: 250; g: 235; b: 215; a: 255),
    (Name: 'aqua'; r: 0; g: 255; b: 255; a: 255),
    (Name: 'aquamarine'; r: 127; g: 255; b: 212; a: 255),
    (Name: 'azure'; r: 240; g: 255; b: 255; a: 255),
    (Name: 'beige'; r: 245; g: 245; b: 220; a: 255),
    (Name: 'bisque'; r: 255; g: 228; b: 196; a: 255),
    (Name: 'black'; r: 0; g: 0; b: 0; a: 255),
    (Name: 'blanchedalmond'; r: 255; g: 235; b: 205; a: 255),
    (Name: 'blue'; r: 0; g: 0; b: 255; a: 255),
    (Name: 'blueviolet'; r: 138; g: 43; b: 226; a: 255),
    (Name: 'brown'; r: 165; g: 42; b: 42; a: 255),
    (Name: 'burlywood'; r: 222; g: 184; b: 135; a: 255),
    (Name: 'cadetblue'; r: 95; g: 158; b: 160; a: 255),
    (Name: 'chartreuse'; r: 127; g: 255; b: 0; a: 255),
    (Name: 'chocolate'; r: 210; g: 105; b: 30; a: 255),
    (Name: 'coral'; r: 255; g: 127; b: 80; a: 255),
    (Name: 'cornflowerblue'; r: 100; g: 149; b: 237; a: 255),
    (Name: 'cornsilk'; r: 255; g: 248; b: 220; a: 255),
    (Name: 'crimson'; r: 220; g: 20; b: 60; a: 255),
    (Name: 'cyan'; r: 0; g: 255; b: 255; a: 255),
    (Name: 'darkblue'; r: 0; g: 0; b: 139; a: 255),
    (Name: 'darkcyan'; r: 0; g: 139; b: 139; a: 255),
    (Name: 'darkgoldenrod'; r: 184; g: 134; b: 11; a: 255),
    (Name: 'darkgray'; r: 169; g: 169; b: 169; a: 255),
    (Name: 'darkgreen'; r: 0; g: 100; b: 0; a: 255),
    (Name: 'darkgrey'; r: 169; g: 169; b: 169; a: 255),
    (Name: 'darkkhaki'; r: 189; g: 183; b: 107; a: 255),
    (Name: 'darkmagenta'; r: 139; g: 0; b: 139; a: 255),
    (Name: 'darkolivegreen'; r: 85; g: 107; b: 47; a: 255),
    (Name: 'darkorange'; r: 255; g: 140; b: 0; a: 255),
    (Name: 'darkorchid'; r: 153; g: 50; b: 204; a: 255),
    (Name: 'darkred'; r: 139; g: 0; b: 0; a: 255),
    (Name: 'darksalmon'; r: 233; g: 150; b: 122; a: 255),
    (Name: 'darkseagreen'; r: 143; g: 188; b: 143; a: 255),
    (Name: 'darkslateblue'; r: 72; g: 61; b: 139; a: 255),
    (Name: 'darkslategray'; r: 47; g: 79; b: 79; a: 255),
    (Name: 'darkslategrey'; r: 47; g: 79; b: 79; a: 255),
    (Name: 'darkturquoise'; r: 0; g: 206; b: 209; a: 255),
    (Name: 'darkviolet'; r: 148; g: 0; b: 211; a: 255),
    (Name: 'deeppink'; r: 255; g: 20; b: 147; a: 255),
    (Name: 'deepskyblue'; r: 0; g: 191; b: 255; a: 255),
    (Name: 'dimgray'; r: 105; g: 105; b: 105; a: 255),
    (Name: 'dimgrey'; r: 105; g: 105; b: 105; a: 255),
    (Name: 'dodgerblue'; r: 30; g: 144; b: 255; a: 255),
    (Name: 'firebrick'; r: 178; g: 34; b: 34; a: 255),
    (Name: 'floralwhite'; r: 255; g: 250; b: 240; a: 255),
    (Name: 'forestgreen'; r: 34; g: 139; b: 34; a: 255),
    (Name: 'fuchsia'; r: 255; g: 0; b: 255; a: 255),
    (Name: 'gainsboro'; r: 220; g: 220; b: 220; a: 255),
    (Name: 'ghostwhite'; r: 248; g: 248; b: 255; a: 255),
    (Name: 'gold'; r: 255; g: 215; b: 0; a: 255),
    (Name: 'goldenrod'; r: 218; g: 165; b: 32; a: 255),
    (Name: 'gray'; r: 128; g: 128; b: 128; a: 255),
    (Name: 'green'; r: 0; g: 128; b: 0; a: 255),
    (Name: 'greenyellow'; r: 173; g: 255; b: 47; a: 255),
    (Name: 'grey'; r: 128; g: 128; b: 128; a: 255),
    (Name: 'honeydew'; r: 240; g: 255; b: 240; a: 255),
    (Name: 'hotpink'; r: 255; g: 105; b: 180; a: 255),
    (Name: 'indianred'; r: 205; g: 92; b: 92; a: 255),
    (Name: 'indigo'; r: 75; g: 0; b: 130; a: 255),
    (Name: 'ivory'; r: 255; g: 255; b: 240; a: 255),
    (Name: 'khaki'; r: 240; g: 230; b: 140; a: 255),
    (Name: 'lavender'; r: 230; g: 230; b: 250; a: 255),
    (Name: 'lavenderblush'; r: 255; g: 240; b: 245; a: 255),
    (Name: 'lawngreen'; r: 124; g: 252; b: 0; a: 255),
    (Name: 'lemonchiffon'; r: 255; g: 250; b: 205; a: 255),
    (Name: 'lightblue'; r: 173; g: 216; b: 230; a: 255),
    (Name: 'lightcoral'; r: 240; g: 128; b: 128; a: 255),
    (Name: 'lightcyan'; r: 224; g: 255; b: 255; a: 255),
    (Name: 'lightgoldenrodyellow'; r: 250; g: 250; b: 210; a: 255),
    (Name: 'lightgray'; r: 211; g: 211; b: 211; a: 255),
    (Name: 'lightgreen'; r: 144; g: 238; b: 144; a: 255),
    (Name: 'lightgrey'; r: 211; g: 211; b: 211; a: 255),
    (Name: 'lightpink'; r: 255; g: 182; b: 193; a: 255),
    (Name: 'lightsalmon'; r: 255; g: 160; b: 122; a: 255),
    (Name: 'lightseagreen'; r: 32; g: 178; b: 170; a: 255),
    (Name: 'lightskyblue'; r: 135; g: 206; b: 250; a: 255),
    (Name: 'lightslategray'; r: 119; g: 136; b: 153; a: 255),
    (Name: 'lightslategrey'; r: 119; g: 136; b: 153; a: 255),
    (Name: 'lightsteelblue'; r: 176; g: 196; b: 222; a: 255),
    (Name: 'lightyellow'; r: 255; g: 255; b: 224; a: 255),
    (Name: 'lime'; r: 0; g: 255; b: 0; a: 255),
    (Name: 'limegreen'; r: 50; g: 205; b: 50; a: 255),
    (Name: 'linen'; r: 250; g: 240; b: 230; a: 255),
    (Name: 'magenta'; r: 255; g: 0; b: 255; a: 255),
    (Name: 'maroon'; r: 128; g: 0; b: 0; a: 255),
    (Name: 'mediumaquamarine'; r: 102; g: 205; b: 170; a: 255),
    (Name: 'mediumblue'; r: 0; g: 0; b: 205; a: 255),
    (Name: 'mediumorchid'; r: 186; g: 85; b: 211; a: 255),
    (Name: 'mediumpurple'; r: 147; g: 112; b: 219; a: 255),
    (Name: 'mediumseagreen'; r: 60; g: 179; b: 113; a: 255),
    (Name: 'mediumslateblue'; r: 123; g: 104; b: 238; a: 255),
    (Name: 'mediumspringgreen'; r: 0; g: 250; b: 154; a: 255),
    (Name: 'mediumturquoise'; r: 72; g: 209; b: 204; a: 255),
    (Name: 'mediumvioletred'; r: 199; g: 21; b: 133; a: 255),
    (Name: 'midnightblue'; r: 25; g: 25; b: 112; a: 255),
    (Name: 'mintcream'; r: 245; g: 255; b: 250; a: 255),
    (Name: 'mistyrose'; r: 255; g: 228; b: 225; a: 255),
    (Name: 'moccasin'; r: 255; g: 228; b: 181; a: 255),
    (Name: 'navajowhite'; r: 255; g: 222; b: 173; a: 255),
    (Name: 'navy'; r: 0; g: 0; b: 128; a: 255),
    (Name: 'oldlace'; r: 253; g: 245; b: 230; a: 255),
    (Name: 'olive'; r: 128; g: 128; b: 0; a: 255),
    (Name: 'olivedrab'; r: 107; g: 142; b: 35; a: 255),
    (Name: 'orange'; r: 255; g: 165; b: 0; a: 255),
    (Name: 'orangered'; r: 255; g: 69; b: 0; a: 255),
    (Name: 'orchid'; r: 218; g: 112; b: 214; a: 255),
    (Name: 'palegoldenrod'; r: 238; g: 232; b: 170; a: 255),
    (Name: 'palegreen'; r: 152; g: 251; b: 152; a: 255),
    (Name: 'paleturquoise'; r: 175; g: 238; b: 238; a: 255),
    (Name: 'palevioletred'; r: 219; g: 112; b: 147; a: 255),
    (Name: 'papayawhip'; r: 255; g: 239; b: 213; a: 255),
    (Name: 'peachpuff'; r: 255; g: 218; b: 185; a: 255),
    (Name: 'peru'; r: 205; g: 133; b: 63; a: 255),
    (Name: 'pink'; r: 255; g: 192; b: 203; a: 255),
    (Name: 'plum'; r: 221; g: 160; b: 221; a: 255),
    (Name: 'powderblue'; r: 176; g: 224; b: 230; a: 255),
    (Name: 'purple'; r: 128; g: 0; b: 128; a: 255),
    (Name: 'red'; r: 255; g: 0; b: 0; a: 255),
    (Name: 'rosybrown'; r: 188; g: 143; b: 143; a: 255),
    (Name: 'royalblue'; r: 65; g: 105; b: 225; a: 255),
    (Name: 'saddlebrown'; r: 139; g: 69; b: 19; a: 255),
    (Name: 'salmon'; r: 250; g: 128; b: 114; a: 255),
    (Name: 'sandybrown'; r: 244; g: 164; b: 96; a: 255),
    (Name: 'seagreen'; r: 46; g: 139; b: 87; a: 255),
    (Name: 'seashell'; r: 255; g: 245; b: 238; a: 255),
    (Name: 'sienna'; r: 160; g: 82; b: 45; a: 255),
    (Name: 'silver'; r: 192; g: 192; b: 192; a: 255),
    (Name: 'skyblue'; r: 135; g: 206; b: 235; a: 255),
    (Name: 'slateblue'; r: 106; g: 90; b: 205; a: 255),
    (Name: 'slategray'; r: 112; g: 128; b: 144; a: 255),
    (Name: 'slategrey'; r: 112; g: 128; b: 144; a: 255),
    (Name: 'snow'; r: 255; g: 250; b: 250; a: 255),
    (Name: 'springgreen'; r: 0; g: 255; b: 127; a: 255),
    (Name: 'steelblue'; r: 70; g: 130; b: 180; a: 255),
    (Name: 'tan'; r: 210; g: 180; b: 140; a: 255),
    (Name: 'teal'; r: 0; g: 128; b: 128; a: 255),
    (Name: 'thistle'; r: 216; g: 191; b: 216; a: 255),
    (Name: 'tomato'; r: 255; g: 99; b: 71; a: 255),
    (Name: 'turquoise'; r: 64; g: 224; b: 208; a: 255),
    (Name: 'violet'; r: 238; g: 130; b: 238; a: 255),
    (Name: 'wheat'; r: 245; g: 222; b: 179; a: 255),
    (Name: 'white'; r: 255; g: 255; b: 255; a: 255),
    (Name: 'whitesmoke'; r: 245; g: 245; b: 245; a: 255),
    (Name: 'yellow'; r: 255; g: 255; b: 0; a: 255),
    (Name: 'yellowgreen'; r: 154; g: 205; b: 50; a: 255),
    (Name: 'zzzzzzzzzzz'; r: 0; g: 0; b: 0; a: 0));

  pageEqHigh: shortstring =
    #1#2#3#4#5#6#7#8#9#10#11#12#13#14#15#16 + #17#18#19#20#21#22#23#24#25#26#27#28#29#30#31#32 +
    #33#34#35#36#37#38#39#40#41#42#43#44#45#46#47#48 + #49#50#51#52#53#54#55#56#57#58#59#60#61#62#63#64 +
    #65#66#67#68#69#70#71#72#73#74#75#76#77#78#79#80 + #81#82#83#84#85#86#87#88#89#90#91#92#93#94#95#96 +
    #65#66#67#68#69#70#71#72#73#74#75#76#77#78#79#80 + #81#82#83#84#85#86#87#88#89#90#123#124#125#126#127#128 +
    #129#130#131#132#133#134#135#136#137#138#139#140#141#142#143#144 + #145#146#147#148#149#150#151#152#153#154#155#156#157#158#159#160 +
    #161#162#163#164#165#166#167#168#169#170#171#172#173#174#175#176 + #177#178#179#180#181#182#183#184#185#186#187#188#189#190#191#192 +
    #193#194#195#196#197#198#199#200#201#202#203#204#205#206#207#208 + #209#210#211#212#213#214#215#216#217#218#219#220#221#222#223#224 +
    #225#226#227#228#229#230#231#232#233#234#235#236#237#238#239#240 + #241#242#243#244#245#246#247#248#249#250#251#252#253#254#255;

{ UNIT IMPLEMENTATION }
{ START_ELEMENT }
procedure start_element(Data: pointer; el: char_ptr; attr: char_ptr_ptr); {$IFDEF EXPAT_WRAPPER } cdecl; {$ENDIF }
var
  this: parser_ptr;

begin
  this := parser_ptr(Data);

  if StrComp(PChar(el), 'title') = 0 then
    this^.m_title_flag := True
  else
  if StrComp(PChar(el), 'g') = 0 then
  begin
    this^.m_path^.push_attr;
    this^.parse_attr(attr);

  end
  else
  if StrComp(PChar(el), 'path') = 0 then
  begin
    if this^.m_path_flag then
      raise svg_exception.Construct(PChar('start_element: Nested path'));

    this^.m_path^.begin_path;
    this^.parse_path(attr);
    this^.m_path^.end_path;

    this^.m_path_flag := True;

  end
  else
  if StrComp(PChar(el), 'rect') = 0 then
    this^.parse_rect(attr)
  else
  if StrComp(PChar(el), 'line') = 0 then
    this^.parse_line(attr)
  else
  if StrComp(PChar(el), 'polyline') = 0 then
    this^.parse_poly(attr, False)
  else
  if StrComp(PChar(el), 'polygon') = 0 then
    this^.parse_poly(attr, True);

  //else
  // if StrComp(PChar(el ) ,'<OTHER_ELEMENTS>' ) = 0 then
  //  begin
  //  end
  //...

end;

{ END_ELEMENT }
procedure end_element(Data: pointer; el: char_ptr); {$IFDEF EXPAT_WRAPPER } cdecl; {$ENDIF }
var
  this: parser_ptr;

begin
  this := parser_ptr(Data);

  if StrComp(PChar(el), 'title') = 0 then
    this^.m_title_flag := False
  else
  if StrComp(PChar(el), 'g') = 0 then
    this^.m_path^.pop_attr
  else
  if StrComp(PChar(el), 'path') = 0 then
    this^.m_path_flag := False;

  //else
  // if StrComp(PChar(el ) ,'<OTHER_ELEMENTS>' ) = 0 then
  //  begin
  //  end
  // ...

end;

{ CONTENT }
procedure content(Data: pointer; s: char_ptr; len: int); {$IFDEF EXPAT_WRAPPER } cdecl; {$ENDIF }
var
  this: parser_ptr;

begin
  this := parser_ptr(Data);

  // m_title_flag signals that the <title> tag is being parsed now.
  // The following code concatenates the pieces of content of the <title> tag.
  if this^.m_title_flag then
  begin
    if len + this^.m_title_len > 255 then
      len := 255 - this^.m_title_len;

    if len > 0 then
    begin
      move(
        s^,
        char_ptr(ptrcomp(this^.m_title) + this^.m_title_len)^,
        len);

      Inc(this^.m_title_len, len);

      char_ptr(ptrcomp(this^.m_title) + this^.m_title_len)^ := #0;

    end;

  end;

end;

{ hex_unsigned }
function hex_unsigned(hexstr: agg_basics.char_ptr): unsigned;

  function xyint(x, y: integer): integer;
  var
    f: integer;
    m: boolean;

  begin
    m := False;

    if y < 0 then
    begin
      y := y * -1;
      m := True;

    end;

    Result := x;

    if y > 1 then
      for f := 1 to y - 1 do
        Result := Result * x;

    if m then
      Result := Result * -1;

  end;

var
  h: shortstring;
  fcb: byte;
  yps, mul, num: unsigned;

label
  Err, Esc;

const
  hex: string[16] = '0123456789ABCDEF';

begin
  h := '';

  while hexstr^ <> #0 do
  begin
    h := h + pageEqHigh[byte(hexstr^)];

    Inc(ptrcomp(hexstr));

  end;

  if length(h) > 0 then
  begin
    case h[length(h)] of
      '0'..'9', 'A'..'F':
      else
        goto Err;

    end;

    num := pos(h[length(h)], hex) - 1;
    yps := 2;
    mul := xyint(4, yps);

    if length(h) > 1 then
      for fcb := length(h) - 1 downto 1 do
      begin
        case h[fcb] of
          '0'..'9', 'A'..'F':
          else
            goto Err;

        end;

        Inc(num, (pos(h[fcb], hex) - 1) * mul);
        Inc(yps, 2);

        mul := xyint(4, yps);

      end;

    goto Esc;

  end;

  Err:
    num := 0;

  Esc:
    Result := num;

end;

{ parse_color }
function parse_color(str: agg_basics.char_ptr): aggclr;
var
  u: unsigned;
  p: named_color_ptr;
  m: shortstring;

begin
  while str^ = ' ' do
    Inc(ptrcomp(str));

  if str^ = '#' then
  begin
    Inc(ptrcomp(str));

    u := hex_unsigned(str);

    Result.Construct(rgb8_packed(u));

  end
  else
  begin
    p := nil;

    for u := 0 to colors_num - 1 do
      if StrComp(colors[u].Name, PChar(str)) = 0 then
      begin
        p := @colors[u];

        break;

      end;

    if p = nil then
    begin
      m := 'parse_color: Invalid color name ' + StrPas(PChar(str)) + #0;

      raise svg_exception.Construct(PChar(@m[1]));

    end;

    Result.ConstrInt(p^.r, p^.g, p^.b, p^.a);

  end;

end;

{ parse_double }
function parse_double(str: agg_basics.char_ptr): double;
begin
  while str^ = ' ' do
    Inc(ptrcomp(str));

  Result := get_double(pointer(PChar(str)));

end;

{ islower }
function islower(ch: char): boolean;
begin
  case ch of
    #97..#122:
      Result := True;

    else
      Result := False;

  end;

end;

{ is_numeric }
function is_numeric(ch: char): boolean;
begin
  Result := Pos(ch, '0123456789+-.eE') <> 0;

end;

{ parse_transform_args }
function parse_transform_args(str: agg_basics.char_ptr; args: double_ptr; max_na: unsigned; na: unsigned_ptr): unsigned;
var
  ptr, end_: agg_basics.char_ptr;

begin
  na^ := 0;
  ptr := str;

  while (ptr^ <> #0) and (ptr^ <> '(') do
    Inc(ptrcomp(ptr));

  if ptr^ = #0 then
    raise svg_exception.Construct(PChar('parse_transform_args: Invalid syntax'));

  end_ := ptr;

  while (end_^ <> #0) and (end_^ <> ')') do
    Inc(ptrcomp(end_));

  if end_^ = #0 then
    raise svg_exception.Construct(PChar('parse_transform_args: Invalid syntax'));

  while ptrcomp(ptr) < ptrcomp(end_) do
    if is_numeric(ptr^) then
    begin
      if na^ >= max_na then
        raise svg_exception.Construct(PChar('parse_transform_args: Too many arguments'));

      double_ptr(ptrcomp(args) + na^ * sizeof(double))^ := get_double(ptr);

      Inc(na^);

      while (ptrcomp(ptr) < ptrcomp(end_)) and is_numeric(ptr^) do
        Inc(ptrcomp(ptr));

    end
    else
      Inc(ptrcomp(ptr));

  Result := unsigned(ptrcomp(end_) - ptrcomp(str));

end;

{ CONSTRUCT }
constructor parser.Construct(path: path_renderer_ptr);
begin
  m_path := path;

  m_tokenizer.Construct;

  agg_getmem(pointer(m_buf), buf_size);
  agg_getmem(pointer(m_title), 256);

  m_title_len := 0;
  m_title_flag := False;
  m_path_flag := False;

  m_attr_name_aloc := 128;
  m_attr_value_aloc := 1024;

  agg_getmem(pointer(m_attr_name), m_attr_name_aloc);
  agg_getmem(pointer(m_attr_value), m_attr_value_aloc);

  m_attr_name_len := 127;
  m_attr_value_len := 1023;

  m_title^ := #0;

end;

{ DESTRUCT }
destructor parser.Destruct;
begin
  agg_freemem(pointer(m_attr_value), m_attr_value_aloc);
  agg_freemem(pointer(m_attr_name), m_attr_name_aloc);
  agg_freemem(pointer(m_title), 256);
  agg_freemem(pointer(m_buf), buf_size);

end;

{ PARSE }
procedure parser.parse(fname: shortstring);
var
  msg: array[0..1023] of char;

  p: XML_Parser;
  af: api_file;
  ts: char_ptr;

  done: boolean;
  len: int;

begin
  p := XML_ParserCreate(nil);

  if p = nil then
    raise svg_exception.Construct(PChar('Couldn''t allocate memory for parser'));

  XML_SetUserData(p, @self);
  XML_SetElementHandler(p, XML_StartElementHandler(@start_element), XML_EndElementHandler(@end_element));
  XML_SetCharacterDataHandler(p, XML_CharacterDataHandler(@content));

  fname := fname + #0;

  Dec(byte(fname[0]));

  if not api_open_file(af, fname) then
  begin
    sprintf(@msg[0], 'Couldn''t open file %s', unsigned(@fname[1]));

    XML_ParserFree(p);

    raise svg_exception.Construct(PChar(@msg[0]));

  end;

  done := False;

  repeat
    api_read_file(af, m_buf, buf_size, len);

    done := len < buf_size;

    if XML_Parse(p, pointer(m_buf), len, int(done)) = XML_STATUS_ERROR then
    begin
      api_close_file(af);
      XML_ParserFree(p);

      sprintf(@msg[0],
        '%s at line ',
        unsigned(XML_ErrorString(XML_GetErrorCode(p))));

      sprintf(@msg[StrLen(msg)],
        '%d'#13,
        XML_GetCurrentLineNumber(p));

      raise svg_exception.Construct(PChar(@msg[0]));

    end;

  until done;

  api_close_file(af);
  XML_ParserFree(p);

  ts := m_title;

  while ts^ <> #0 do
  begin
    if byte(ts^) < byte(' ') then
      ts^ := ' ';

    Inc(ptrcomp(ts));

  end;

end;

{ TITLE }
function parser.title: char_ptr;
begin
  Result := m_title;

end;

{ PARSE_ATTR }
procedure parser.parse_attr(attr: char_ptr_ptr);
var
  i: int;

begin
  i := 0;

  while char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^ <> nil do
  begin
    if StrComp(PChar(char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^), 'style') = 0 then
      parse_style(agg_basics.char_ptr_ptr(ptrcomp(attr) + (i + 1) * sizeof(char_ptr))^)
    else
      parse_attr(
        agg_basics.char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^,
        agg_basics.char_ptr_ptr(ptrcomp(attr) + (i + 1) * sizeof(char_ptr))^);

    Inc(i, 2);

  end;

end;

{ PARSE_PATH }
procedure parser.parse_path(attr: char_ptr_ptr);
var
  i: int;

  tmp: array[0..3] of agg_basics.char_ptr;

begin
  i := 0;

  while char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^ <> nil do
  begin
    // The <path> tag can consist of the path itself ("d=")
    // as well as of other parameters like "style=", "transform=", etc.
    // In the last case we simply rely on the function of parsing
    // attributes (see 'else' branch).
    if StrComp(PChar(char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^), 'd') = 0 then
    begin
      m_tokenizer.set_path_str(agg_basics.char_ptr_ptr(ptrcomp(attr) + (i + 1) * sizeof(char_ptr))^);

      m_path^.parse_path(@m_tokenizer);

    end
    else
    begin
      // Create a temporary single pair "name-value" in order
      // to avoid multiple calls for the same attribute.
      tmp[0] := agg_basics.char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^;
      tmp[1] := agg_basics.char_ptr_ptr(ptrcomp(attr) + (i + 1) * sizeof(char_ptr))^;
      tmp[2] := nil;
      tmp[3] := nil;

      parse_attr(@tmp);

    end;

    Inc(i, 2);

  end;

end;

{ PARSE_POLY }
procedure parser.parse_poly(attr: char_ptr_ptr; close_flag: boolean);
var
  i: int;

  x, y: double;

begin
  x := 0.0;
  y := 0.0;

  m_path^.begin_path;

  i := 0;

  while char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^ <> nil do
  begin
    if not parse_attr(agg_basics.char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^,
      agg_basics.char_ptr_ptr(ptrcomp(attr) + (i + 1) * sizeof(char_ptr))^) then
      if StrComp(PChar(agg_basics.char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^), 'points') = 0 then
      begin
        m_tokenizer.set_path_str(agg_basics.char_ptr_ptr(ptrcomp(attr) + (i + 1) * sizeof(char_ptr))^);

        if not m_tokenizer.Next then
          raise svg_exception.Construct(PChar('parse_poly: Too few coordinates'));

        x := m_tokenizer.last_number;

        if not m_tokenizer.Next then
          raise svg_exception.Construct(PChar('parse_poly: Too few coordinates'));

        y := m_tokenizer.last_number;

        m_path^.move_to(x, y);

        while m_tokenizer.Next do
        begin
          x := m_tokenizer.last_number;

          if not m_tokenizer.Next then
            raise svg_exception.Construct(PChar('parse_poly: Odd number of coordinates'));

          y := m_tokenizer.last_number;

          m_path^.line_to(x, y);

        end;

      end;

    Inc(i, 2);

  end;

  m_path^.end_path;

end;

{ PARSE_RECT }
procedure parser.parse_rect(attr: char_ptr_ptr);
var
  i: int;

  x, y, w, h: double;

begin
  x := 0.0;
  y := 0.0;
  w := 0.0;
  h := 0.0;

  m_path^.begin_path;

  i := 0;

  while char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^ <> nil do
  begin
    if not parse_attr(agg_basics.char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^,
      agg_basics.char_ptr_ptr(ptrcomp(attr) + (i + 1) * sizeof(char_ptr))^) then
    begin
      if StrComp(PChar(char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^), 'x') = 0 then
        x := parse_double(agg_basics.char_ptr_ptr(ptrcomp(attr) + (i + 1) * sizeof(char_ptr))^);

      if StrComp(PChar(char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^), 'y') = 0 then
        y := parse_double(agg_basics.char_ptr_ptr(ptrcomp(attr) + (i + 1) * sizeof(char_ptr))^);

      if StrComp(PChar(char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^), 'width') = 0 then
        w := parse_double(agg_basics.char_ptr_ptr(ptrcomp(attr) + (i + 1) * sizeof(char_ptr))^);

      if StrComp(PChar(char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^), 'height') = 0 then
        h := parse_double(agg_basics.char_ptr_ptr(ptrcomp(attr) + (i + 1) * sizeof(char_ptr))^);

      // rx - to be implemented
      // ry - to be implemented

    end;

    Inc(i, 2);

  end;

  if (w <> 0.0) and (h <> 0.0) then
  begin
    if w < 0.0 then
      raise svg_exception.Construct(PChar('parse_rect: Invalid width: '));

    if h < 0.0 then
      raise svg_exception.Construct(PChar('parse_rect: Invalid height: '));

    m_path^.move_to(x, y);
    m_path^.line_to(x + w, y);
    m_path^.line_to(x + w, y + h);
    m_path^.line_to(x, y + h);
    m_path^.close_subpath;

  end;

  m_path^.end_path;

end;

{ PARSE_LINE }
procedure parser.parse_line(attr: char_ptr_ptr);
var
  i: int;

  x1, y1, x2, y2: double;

begin
  x1 := 0.0;
  y1 := 0.0;
  x2 := 0.0;
  y2 := 0.0;

  m_path^.begin_path;

  i := 0;

  while char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^ <> nil do
  begin
    if not parse_attr(agg_basics.char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^,
      agg_basics.char_ptr_ptr(ptrcomp(attr) + (i + 1) * sizeof(char_ptr))^) then
    begin
      if StrComp(PChar(char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^), 'x1') = 0 then
        x1 := parse_double(agg_basics.char_ptr_ptr(ptrcomp(attr) + (i + 1) * sizeof(char_ptr))^);

      if StrComp(PChar(char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^), 'y1') = 0 then
        y1 := parse_double(agg_basics.char_ptr_ptr(ptrcomp(attr) + (i + 1) * sizeof(char_ptr))^);

      if StrComp(PChar(char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^), 'x2') = 0 then
        x2 := parse_double(agg_basics.char_ptr_ptr(ptrcomp(attr) + (i + 1) * sizeof(char_ptr))^);

      if StrComp(PChar(char_ptr_ptr(ptrcomp(attr) + i * sizeof(char_ptr))^), 'y2') = 0 then
        y2 := parse_double(agg_basics.char_ptr_ptr(ptrcomp(attr) + (i + 1) * sizeof(char_ptr))^);

    end;

    Inc(i, 2);

  end;

  m_path^.move_to(x1, y1);
  m_path^.line_to(x2, y2);
  m_path^.end_path;

end;

{ PARSE_STYLE }
procedure parser.parse_style(str: agg_basics.char_ptr);
var
  nv_start, nv_end: agg_basics.char_ptr;

begin
  while str^ <> #0 do
  begin
    // Left Trim
    while (str^ <> #0) and (str^ = ' ') do
      Inc(ptrcomp(str));

    nv_start := str;

    while (str^ <> #0) and (str^ <> ';') do
      Inc(ptrcomp(str));

    nv_end := str;

    // Right Trim
    while (ptrcomp(nv_end) > ptrcomp(nv_start)) and ((nv_end^ = ';') or (nv_end^ = ' ')) do
      Dec(ptrcomp(nv_end));

    Inc(ptrcomp(nv_end));

    parse_name_value(nv_start, nv_end);

    if str^ <> #0 then
      Inc(ptrcomp(str));

  end;

end;

{ PARSE_TRANSFORM }
procedure parser.parse_transform(str: agg_basics.char_ptr);
begin
  while str^ <> #0 do
  begin
    if islower(str^) then
      if StrLComp(PChar(str), 'matrix', 6) = 0 then
        Inc(ptrcomp(str), parse_matrix(str))
      else
      if StrLComp(PChar(str), 'translate', 9) = 0 then
        Inc(ptrcomp(str), parse_translate(str))
      else
      if StrLComp(PChar(str), 'rotate', 6) = 0 then
        Inc(ptrcomp(str), parse_rotate(str))
      else
      if StrLComp(PChar(str), 'scale', 5) = 0 then
        Inc(ptrcomp(str), parse_scale(str))
      else
      if StrLComp(PChar(str), 'skewX', 5) = 0 then
        Inc(ptrcomp(str), parse_skew_x(str))
      else
      if StrLComp(PChar(str), 'skewY', 5) = 0 then
        Inc(ptrcomp(str), parse_skew_y(str))
      else
        Inc(ptrcomp(str))

    else
      Inc(ptrcomp(str));

  end;

end;

{ PARSE_MATRIX }
function parser.parse_matrix(str: agg_basics.char_ptr): unsigned;
var
  args: array[0..5] of double;

  na, len: unsigned;

  ta: trans_affine;

begin
  na := 0;
  len := parse_transform_args(str, @args, 6, @na);

  if na <> 6 then
    raise svg_exception.Construct(PChar('parse_matrix: Invalid number of arguments'));

  ta.Construct(args[0], args[1], args[2], args[3], args[4], args[5]);

  m_path^.transform^.premultiply(@ta);

  Result := len;

end;

{ PARSE_TRANSLATE }
function parser.parse_translate(str: agg_basics.char_ptr): unsigned;
var
  args: array[0..1] of double;

  na, len: unsigned;

  tat: trans_affine_translation;

begin
  na := 0;
  len := parse_transform_args(str, @args, 2, @na);

  if na = 1 then
    args[1] := 0.0;

  tat.Construct(args[0], args[1]);

  m_path^.transform^.premultiply(@tat);

  Result := len;

end;

{ PARSE_ROTATE }
function parser.parse_rotate(str: agg_basics.char_ptr): unsigned;
var
  args: array[0..2] of double;

  na, len: unsigned;

  tar: trans_affine_rotation;

  tat, t: trans_affine_translation;

begin
  na := 0;
  len := parse_transform_args(str, @args, 3, @na);

  if na = 1 then
  begin
    tar.Construct(deg2rad(args[0]));

    m_path^.transform^.premultiply(@tar);

  end
  else
  if na = 3 then
  begin
    t.Construct(-args[1], -args[2]);

    tar.Construct(deg2rad(args[0]));
    tat.Construct(args[1], args[2]);

    t.multiply(@tar);
    t.multiply(@tat);

    m_path^.transform^.premultiply(@t);

  end
  else
    raise svg_exception.Construct(PChar('parse_rotate: Invalid number of arguments'));

  Result := len;

end;

{ PARSE_SCALE }
function parser.parse_scale(str: agg_basics.char_ptr): unsigned;
var
  args: array[0..1] of double;

  na, len: unsigned;

  tas: trans_affine_scaling;

begin
  na := 0;
  len := parse_transform_args(str, @args, 2, @na);

  if na = 1 then
    args[1] := args[0];

  tas.Construct(args[0], args[1]);

  m_path^.transform^.premultiply(@tas);

  Result := len;

end;

{ PARSE_SKEW_X }
function parser.parse_skew_x(str: agg_basics.char_ptr): unsigned;
var
  arg: double;

  na, len: unsigned;

  tas: trans_affine_skewing;

begin
  na := 0;
  len := parse_transform_args(str, @arg, 1, @na);

  tas.Construct(deg2rad(arg), 0.0);

  m_path^.transform^.premultiply(@tas);

  Result := len;

end;

{ PARSE_SKEW_Y }
function parser.parse_skew_y(str: agg_basics.char_ptr): unsigned;
var
  arg: double;

  na, len: unsigned;

  tas: trans_affine_skewing;

begin
  na := 0;
  len := parse_transform_args(str, @arg, 1, @na);

  tas.Construct(0.0, deg2rad(arg));

  m_path^.transform^.premultiply(@tas);

  Result := len;

end;

{ PARSE_ATTR }
function parser.parse_attr(Name, Value: agg_basics.char_ptr): boolean;
var
  clr: aggclr;

begin
  Result := True;

  if StrComp(PChar(Name), 'style') = 0 then
    parse_style(Value)
  else
  if StrComp(PChar(Name), 'fill') = 0 then
    if StrComp(PChar(Value), 'none') = 0 then
      m_path^.fill_none
    else
    begin
      clr := parse_color(Value);

      m_path^.fill(@clr);

    end
  else
  if StrComp(PChar(Name), 'fill-opacity') = 0 then
    m_path^.fill_opacity(parse_double(Value))
  else
  if StrComp(PChar(Name), 'stroke') = 0 then
    if StrComp(PChar(Value), 'none') = 0 then
      m_path^.stroke_none
    else
    begin
      clr := parse_color(Value);

      m_path^.stroke(@clr);

    end
  else
  if StrComp(PChar(Name), 'stroke-width') = 0 then
    m_path^.stroke_width(parse_double(Value))
  else
  if StrComp(PChar(Name), 'stroke-linecap') = 0 then
  begin
    if StrComp(PChar(Value), 'butt') = 0 then
      m_path^.line_cap(butt_cap)
    else
    if StrComp(PChar(Value), 'round') = 0 then
      m_path^.line_cap(round_cap)
    else
    if StrComp(PChar(Value), 'square') = 0 then
      m_path^.line_cap(square_cap);

  end
  else
  if StrComp(PChar(Name), 'stroke-linejoin') = 0 then
  begin
    if StrComp(PChar(Value), 'miter') = 0 then
      m_path^.line_join(miter_join)
    else
    if StrComp(PChar(Value), 'round') = 0 then
      m_path^.line_join(round_join)
    else
    if StrComp(PChar(Value), 'bevel') = 0 then
      m_path^.line_join(bevel_join);

  end
  else
  if StrComp(PChar(Name), 'stroke-miterlimit') = 0 then
    m_path^.miter_limit(parse_double(Value))
  else
  if StrComp(PChar(Name), 'stroke-opacity') = 0 then
    m_path^.stroke_opacity(parse_double(Value))
  else
  if StrComp(PChar(Name), 'transform') = 0 then
    parse_transform(Value)

  //else
  // if StrComp(PChar(el ) ,'<OTHER_ATTRIBUTES>' ) = 0 then
  //  begin
  //  end
  // ...

  else
    Result := False;

end;

{ PARSE_NAME_VALUE }
function parser.parse_name_value(nv_start, nv_end: agg_basics.char_ptr): boolean;
var
  str, val: agg_basics.char_ptr;

begin
  str := nv_start;

  while (ptrcomp(str) < ptrcomp(nv_end)) and (str^ <> ':') do
    Inc(ptrcomp(str));

  val := str;

  // Right Trim
  while (ptrcomp(str) > ptrcomp(nv_start)) and ((str^ = ':') or (str^ = ' ')) do
    Dec(ptrcomp(str));

  Inc(ptrcomp(str));

  copy_name(nv_start, str);

  while (ptrcomp(val) < ptrcomp(nv_end)) and ((val^ = ':') or (val^ = ' ')) do
    Inc(ptrcomp(val));

  copy_value(val, nv_end);

  Result := parse_attr(agg_basics.char_ptr(m_attr_name), agg_basics.char_ptr(m_attr_value));

end;

{ COPY_NAME }
procedure parser.copy_name(start, end_: agg_basics.char_ptr);
var
  len: unsigned;

begin
  len := ptrcomp(end_) - ptrcomp(start);

  if (m_attr_name_len = 0) or (len > m_attr_name_len) then
  begin
    agg_freemem(pointer(m_attr_name), m_attr_name_aloc);

    m_attr_name_aloc := len + 1;

    agg_freemem(pointer(m_attr_name), m_attr_name_aloc);

    m_attr_name_len := len;

  end;

  if len <> 0 then
    move(start^, m_attr_name^, len);

  char_ptr(ptrcomp(m_attr_name) + len)^ := #0;

end;

{ COPY_VALUE }
procedure parser.copy_value(start, end_: agg_basics.char_ptr);
var
  len: unsigned;

begin
  len := ptrcomp(end_) - ptrcomp(start);

  if (m_attr_value_len = 0) or (len > m_attr_value_len) then
  begin
    agg_freemem(pointer(m_attr_value), m_attr_value_aloc);

    m_attr_value_aloc := len + 1;

    agg_getmem(pointer(m_attr_value), m_attr_value_aloc);

    m_attr_value_len := len;

  end;

  if len <> 0 then
    move(start^, m_attr_value^, len);

  char_ptr(ptrcomp(m_attr_value) + len)^ := #0;

end;

end.

