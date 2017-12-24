
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit agg_gamma_lut;

interface

{$I agg_mode.inc }

uses
  Math,
  agg_basics,
  agg_pixfmt;

type

  gamma_lut = object(gamma)
    gamma_shift, gamma_size, gamma_mask, hi_res_shift, hi_res_size, hi_res_mask, HiResT, LoResT: unsigned;
    m_gamma: double;
    m_dir_gamma, m_inv_gamma: int8u_ptr;
    constructor Construct_(GammaShift: unsigned = 8; HiResShift: unsigned = 8);
    constructor Construct(g: double; GammaShift: unsigned = 8; HiResShift: unsigned = 8);
    destructor Destruct;
    procedure gamma_(g: double);
    function _gamma: double;
    function dir(v: unsigned): unsigned; virtual;
    function inv(v: unsigned): unsigned; virtual;
  end;

implementation

constructor gamma_lut.Construct_(GammaShift: unsigned = 8; HiResShift: unsigned = 8);
var
  i: unsigned;

begin
  gamma_shift := GammaShift;
  gamma_size := 1 shl gamma_shift;
  gamma_mask := gamma_size - 1;

  hi_res_shift := HiResShift;
  hi_res_size := 1 shl hi_res_shift;
  hi_res_mask := hi_res_size - 1;

  HiResT := hi_res_shift div 8;
  LoResT := gamma_shift div 8;

  agg_getmem(pointer(m_dir_gamma), gamma_size * HiResT);
  agg_getmem(pointer(m_inv_gamma), hi_res_size * LoResT);

  // dir_gamma
  for i := 0 to gamma_size - 1 do
    try
      case HiResT of
        1:
          int8u_ptr(ptrcomp(m_dir_gamma) + i * HiResT)^ :=
            int8u(i shl (hi_res_shift - gamma_shift));

        2:
          int16u_ptr(ptrcomp(m_dir_gamma) + i * HiResT)^ :=
            int16u(i shl (hi_res_shift - gamma_shift));

        4:
          int32u_ptr(ptrcomp(m_dir_gamma) + i * HiResT)^ :=
            int32u(i shl (hi_res_shift - gamma_shift));

      end;
    except
    end;

  // inv_gamma
  for i := 0 to hi_res_size - 1 do
    try
      case LoResT of
        1:
          int8u_ptr(ptrcomp(m_inv_gamma) + i * LoResT)^ :=
            int8u(i shr (hi_res_shift - gamma_shift));

        2:
          int16u_ptr(ptrcomp(m_inv_gamma) + i * LoResT)^ :=
            int16u(i shr (hi_res_shift - gamma_shift));

        4:
          int32u_ptr(ptrcomp(m_inv_gamma) + i * LoResT)^ :=
            int32u(i shr (hi_res_shift - gamma_shift));

      end;
    except
    end;

end;

{ CONSTRUCT }
constructor gamma_lut.Construct(g: double; GammaShift: unsigned = 8; HiResShift: unsigned = 8);
begin
  gamma_shift := GammaShift;
  gamma_size := 1 shl gamma_shift;
  gamma_mask := gamma_size - 1;

  hi_res_shift := HiResShift;
  hi_res_size := 1 shl hi_res_shift;
  hi_res_mask := hi_res_size - 1;

  HiResT := hi_res_shift div 8;
  LoResT := gamma_shift div 8;

  m_gamma := 1;

  agg_getmem(pointer(m_dir_gamma), gamma_size * HiResT);
  agg_getmem(pointer(m_inv_gamma), hi_res_size * LoResT);

  gamma_(g);

end;

{ DESTRUCT }
destructor gamma_lut.Destruct;
begin
  agg_freemem(pointer(m_dir_gamma), gamma_size * HiResT);
  agg_freemem(pointer(m_inv_gamma), hi_res_size * LoResT);

end;

{ GAMMA_ }
procedure gamma_lut.gamma_(g: double);
var
  i: unsigned;

  inv_g: double;

begin
  m_gamma := g;

  // dir_gamma
  for i := 0 to gamma_size - 1 do
    try
      case HiResT of
        1:
          int8u_ptr(ptrcomp(m_dir_gamma) + i * HiResT)^ :=
            int8u(trunc(Power(i / gamma_mask, m_gamma) * hi_res_mask + 0.5));

        2:
          int16u_ptr(ptrcomp(m_dir_gamma) + i * HiResT)^ :=
            int16u(trunc(Power(i / gamma_mask, m_gamma) * hi_res_mask + 0.5));

        4:
          int32u_ptr(ptrcomp(m_dir_gamma) + i * HiResT)^ :=
            int32u(trunc(Power(i / gamma_mask, m_gamma) * hi_res_mask + 0.5));

      end;
    except
    end;

  // inv_gamma
  if g = 0 then
    fillchar(m_inv_gamma^, hi_res_size * LoResT, 0)

  else
  begin
    inv_g := 1 / g;

    for i := 0 to hi_res_size - 1 do
      try
        case LoResT of
          1:
            int8u_ptr(ptrcomp(m_inv_gamma) + i * LoResT)^ :=
              int8u(trunc(Power(i / hi_res_mask, inv_g) * gamma_mask + 0.5));

          2:
            int16u_ptr(ptrcomp(m_inv_gamma) + i * LoResT)^ :=
              int16u(trunc(Power(i / hi_res_mask, inv_g) * gamma_mask + 0.5));

          4:
            int32u_ptr(ptrcomp(m_inv_gamma) + i * LoResT)^ :=
              int32u(trunc(Power(i / hi_res_mask, inv_g) * gamma_mask + 0.5));

        end;
      except
      end;

  end;

end;

{ _GAMMA }
function gamma_lut._gamma: double;
begin
  Result := m_gamma;

end;

{ DIR }
function gamma_lut.dir(v: unsigned): unsigned;
begin
  case HiResT of
    1:
      Result := int8u_ptr(ptrcomp(m_dir_gamma) + v * HiResT)^;

    2:
      Result := int16u_ptr(ptrcomp(m_dir_gamma) + v * HiResT)^;

    4:
      Result := int32u_ptr(ptrcomp(m_dir_gamma) + v * HiResT)^;

    else
      Result := 0;

  end;

end;

{ INV }
function gamma_lut.inv(v: unsigned): unsigned;
begin
  case LoResT of
    1:
      Result := int8u_ptr(ptrcomp(m_inv_gamma) + v * LoResT)^;

    2:
      Result := int16u_ptr(ptrcomp(m_inv_gamma) + v * LoResT)^;

    4:
      Result := int32u_ptr(ptrcomp(m_inv_gamma) + v * LoResT)^;

    else
      Result := 0;

  end;

end;

end.









