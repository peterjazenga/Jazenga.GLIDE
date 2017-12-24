
{**********************************************************************
 Package pl_AGGPas
 is a modification of
 Anti-Grain Geometry 2D Graphics Library (http://www.aggpas.org/)
 for CodeTyphon Studio (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}
unit
agg_span_interpolator_adaptor;

interface

{$I agg_mode.inc }

uses
  agg_basics,
  agg_span_interpolator_linear,
  agg_trans_affine;

type
  distortion_ptr = ^distortion;

  distortion = object
    procedure calculate(x, y: int_ptr); virtual; abstract;

  end;

  span_interpolator_adaptor = object(span_interpolator_linear)
    m_distortion: distortion_ptr;

    constructor Construct; overload;
    constructor Construct(trans: trans_affine_ptr; dist: distortion_ptr); overload;
    constructor Construct(trans: trans_affine_ptr; dist: distortion_ptr;
      x, y: double; len: unsigned); overload;

    function _distortion: distortion_ptr;
    procedure distortion_(dist: distortion_ptr);

    procedure coordinates(x, y: int_ptr); virtual;

  end;

implementation

constructor span_interpolator_adaptor.Construct;
begin
  inherited Construct;

  m_distortion := nil;

end;

{ CONSTRUCT }
constructor span_interpolator_adaptor.Construct(trans: trans_affine_ptr; dist: distortion_ptr);
begin
  inherited Construct(trans);

  m_distortion := dist;

end;

{ CONSTRUCT }
constructor span_interpolator_adaptor.Construct(trans: trans_affine_ptr; dist: distortion_ptr;
  x, y: double; len: unsigned);
begin
  inherited Construct(trans, x, y, len);

  m_distortion := dist;

end;

{ _DISTORTION }
function span_interpolator_adaptor._distortion: distortion_ptr;
begin
  Result := m_distortion;

end;

{ DISTORTION_ }
procedure span_interpolator_adaptor.distortion_(dist: distortion_ptr);
begin
  m_distortion := dist;

end;

{ COORDINATES }
procedure span_interpolator_adaptor.coordinates(x, y: int_ptr);
begin
  inherited coordinates(x, y);

  m_distortion^.calculate(x, y);

end;

end.




