{**********************************************************************
 Package pl_OpenGL.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit ctOpenGLES1x;

interface

uses
  LCLIntf, LCLType, LMessages,ctypes,
  math, FileUtil,sysutils, dynlibs;


function  InitGLES11 : Boolean;

Const
{$IFDEF MSWINDOWS}
  convertedlib = 'libgles_cm.dll';
{$ENDIF}

{$IFDEF WINCE}
  convertedlib = 'libGLES_CM.dll';
{$ENDIF}

{$IFDEF UNIX}
  convertedlib = 'libgles_cm.so';
{$ENDIF}


Type
GLenum = Cardinal;
GLboolean = Byte;
GLbitfield = Cardinal;
GLbyte = Shortint;
GLshort = Smallint;
GLint = Integer;
GLsizei = Integer;
GLubyte = Byte;
GLushort = Word;
GLuint = Cardinal;
GLfloat = Single;
GLclampf = Single;
GLintptrARB = Integer;
GLsizeiptrARB = Integer;
GLfixed = Integer;
GLclampx = Integer;
GLvoid = Pointer;
GLExtern = Pointer;

pGLenum= ^GLenum;
pGLboolean= ^GLboolean;
pGLbitfield= ^GLbitfield;
pGLbyte= ^GLbyte;
pGLshort= ^GLshort;
pGLint= ^GLint;
pGLsizei= ^GLsizei;
pGLubyte= ^GLubyte;
pGLushort= ^GLushort;
pGLuint= ^GLuint;
pGLfloat= ^GLfloat;
pGLclampf= ^GLclampf;
pGLintptrARB= ^GLintptrARB;
pGLsizeiptrARB= ^GLsizeiptrARB;
pGLfixed= ^GLfixed;
pGLclampx= ^GLclampx;

//EGL
EGLint = Integer;
EGLBoolean = Cardinal;
EGLenum = Cardinal;
EGLDisplay = EGLint;
EGLConfig = EGLint;
EGLSurface = type pointer;
EGLCOntext = type pointer;
EGLClientBuffer = type pointer;

EGLNativeDisplayType = HDC;
EGLNativeWindowType  = HWND;
EGLNativePixmapType  = HBITMAP;

PEGLint = ^EGLint;
PEGLConfig = ^EGLConfig;
PEGLBoolean = ^EGLBoolean;
PEGLenum = ^EGLenum;
PEGLDisplay = ^EGLDisplay;


Const
GL_OES_VERSION_1_0 = 1;
GL_OES_read_format = 1;
GL_OES_compressed_paletted_texture = 1;
GL_DEPTH_BUFFER_BIT = $00000100;
GL_STENCIL_BUFFER_BIT = $00000400;
GL_COLOR_BUFFER_BIT = $00004000;
GL_FALSE = 0;
GL_TRUE = 1;
GL_POINTS = $0000;
GL_LINES = $0001;
GL_LINE_LOOP = $0002;
GL_LINE_STRIP = $0003;
GL_TRIANGLES = $0004;
GL_TRIANGLE_STRIP = $0005;
GL_TRIANGLE_FAN = $0006;
GL_NEVER = $0200;
GL_LESS = $0201;
GL_EQUAL = $0202;
GL_LEQUAL = $0203;
GL_GREATER = $0204;
GL_NOTEQUAL = $0205;
GL_GEQUAL = $0206;
GL_ALWAYS = $0207;
GL_ZERO = 0;
GL_ONE = 1;
GL_SRC_COLOR = $0300;
GL_ONE_MINUS_SRC_COLOR = $0301;
GL_SRC_ALPHA = $0302;
GL_ONE_MINUS_SRC_ALPHA = $0303;
GL_DST_ALPHA = $0304;
GL_ONE_MINUS_DST_ALPHA = $0305;
GL_DST_COLOR = $0306;
GL_ONE_MINUS_DST_COLOR = $0307;
GL_SRC_ALPHA_SATURATE = $0308;
GL_FRONT = $0404;
GL_BACK = $0405;
GL_FRONT_AND_BACK = $0408;
GL_FOG = $0B60;
GL_LIGHTING = $0B50;
GL_TEXTURE_2D = $0DE1;
GL_CULL_FACE = $0B44;
GL_ALPHA_TEST = $0BC0;
GL_BLEND = $0BE2;
GL_COLOR_LOGIC_OP = $0BF2;
GL_DITHER = $0BD0;
GL_STENCIL_TEST = $0B90;
GL_DEPTH_TEST = $0B71;
GL_POINT_SMOOTH = $0B10;
GL_LINE_SMOOTH = $0B20;
GL_SCISSOR_TEST = $0C11;
GL_COLOR_MATERIAL = $0B57;
GL_NORMALIZE = $0BA1;
GL_RESCALE_NORMAL = $803A;
GL_POLYGON_OFFSET_FILL = $8037;
GL_VERTEX_ARRAY = $8074;
GL_NORMAL_ARRAY = $8075;
GL_COLOR_ARRAY = $8076;
GL_TEXTURE_COORD_ARRAY = $8078;
GL_MULTISAMPLE = $809D;
GL_SAMPLE_ALPHA_TO_COVERAGE = $809E;
GL_SAMPLE_ALPHA_TO_ONE = $809F;
GL_SAMPLE_COVERAGE = $80A0;
GL_NO_ERROR = 0;
GL_INVALID_ENUM = $0500;
GL_INVALID_VALUE = $0501;
GL_INVALID_OPERATION = $0502;
GL_STACK_OVERFLOW = $0503;
GL_STACK_UNDERFLOW = $0504;
GL_OUT_OF_MEMORY = $0505;
GL_EXP = $0800;
GL_EXP2 = $0801;
GL_FOG_DENSITY = $0B62;
GL_FOG_START = $0B63;
GL_FOG_END = $0B64;
GL_FOG_MODE = $0B65;
GL_FOG_COLOR = $0B66;
GL_CW = $0900;
GL_CCW = $0901;
GL_SMOOTH_POINT_SIZE_RANGE = $0B12;
GL_SMOOTH_LINE_WIDTH_RANGE = $0B22;
GL_ALIASED_POINT_SIZE_RANGE = $846D;
GL_ALIASED_LINE_WIDTH_RANGE = $846E;
GL_IMPLEMENTATION_COLOR_READ_TYPE_OES = $8B9A;
GL_IMPLEMENTATION_COLOR_READ_FORMAT_OES = $8B9B;
GL_MAX_LIGHTS = $0D31;
GL_MAX_TEXTURE_SIZE = $0D33;
GL_MAX_MODELVIEW_STACK_DEPTH = $0D36;
GL_MAX_PROJECTION_STACK_DEPTH = $0D38;
GL_MAX_TEXTURE_STACK_DEPTH = $0D39;
GL_MAX_VIEWPORT_DIMS = $0D3A;
GL_MAX_ELEMENTS_VERTICES = $80E8;
GL_MAX_ELEMENTS_INDICES = $80E9;
GL_MAX_TEXTURE_UNITS = $84E2;
GL_NUM_COMPRESSED_TEXTURE_FORMATS = $86A2;
GL_COMPRESSED_TEXTURE_FORMATS = $86A3;
GL_SUBPIXEL_BITS = $0D50;
GL_RED_BITS = $0D52;
GL_GREEN_BITS = $0D53;
GL_BLUE_BITS = $0D54;
GL_ALPHA_BITS = $0D55;
GL_DEPTH_BITS = $0D56;
GL_STENCIL_BITS = $0D57;
GL_DONT_CARE = $1100;
GL_FASTEST = $1101;
GL_NICEST = $1102;
GL_PERSPECTIVE_CORRECTION_HINT = $0C50;
GL_POINT_SMOOTH_HINT = $0C51;
GL_LINE_SMOOTH_HINT = $0C52;
GL_POLYGON_SMOOTH_HINT = $0C53;
GL_FOG_HINT = $0C54;
GL_LIGHT_MODEL_AMBIENT = $0B53;
GL_LIGHT_MODEL_TWO_SIDE = $0B52;
GL_AMBIENT = $1200;
GL_DIFFUSE = $1201;
GL_SPECULAR = $1202;
GL_POSITION = $1203;
GL_SPOT_DIRECTION = $1204;
GL_SPOT_EXPONENT = $1205;
GL_SPOT_CUTOFF = $1206;
GL_CONSTANT_ATTENUATION = $1207;
GL_LINEAR_ATTENUATION = $1208;
GL_QUADRATIC_ATTENUATION = $1209;
GL_BYTE = $1400;
GL_UNSIGNED_BYTE = $1401;
GL_SHORT = $1402;
GL_UNSIGNED_SHORT = $1403;
GL_FLOAT = $1406;
GL_FIXED = $140C;
GL_CLEAR = $1500;
GL_AND = $1501;
GL_AND_REVERSE = $1502;
GL_COPY = $1503;
GL_AND_INVERTED = $1504;
GL_NOOP = $1505;
GL_XOR = $1506;
GL_OR = $1507;
GL_NOR = $1508;
GL_EQUIV = $1509;
GL_INVERT = $150A;
GL_OR_REVERSE = $150B;
GL_COPY_INVERTED = $150C;
GL_OR_INVERTED = $150D;
GL_NAND = $150E;
GL_SET = $150F;
GL_EMISSION = $1600;
GL_SHININESS = $1601;
GL_AMBIENT_AND_DIFFUSE = $1602;
GL_MODELVIEW = $1700;
GL_PROJECTION = $1701;
GL_TEXTURE = $1702;
GL_ALPHA = $1906;
GL_RGB = $1907;
GL_RGBA = $1908;
GL_LUMINANCE = $1909;
GL_LUMINANCE_ALPHA = $190A;
GL_UNPACK_ALIGNMENT = $0CF5;
GL_PACK_ALIGNMENT = $0D05;
GL_UNSIGNED_SHORT_4_4_4_4 = $8033;
GL_UNSIGNED_SHORT_5_5_5_1 = $8034;
GL_UNSIGNED_SHORT_5_6_5 = $8363;
GL_FLAT = $1D00;
GL_SMOOTH = $1D01;
GL_KEEP = $1E00;
GL_REPLACE = $1E01;
GL_INCR = $1E02;
GL_DECR = $1E03;
GL_VENDOR = $1F00;
GL_RENDERER = $1F01;
GL_VERSION = $1F02;
GL_EXTENSIONS = $1F03;
GL_MODULATE = $2100;
GL_DECAL = $2101;
GL_ADD = $0104;
GL_TEXTURE_ENV_MODE = $2200;
GL_TEXTURE_ENV_COLOR = $2201;
GL_TEXTURE_ENV = $2300;
GL_NEAREST = $2600;
GL_LINEAR = $2601;
GL_NEAREST_MIPMAP_NEAREST = $2700;
GL_LINEAR_MIPMAP_NEAREST = $2701;
GL_NEAREST_MIPMAP_LINEAR = $2702;
GL_LINEAR_MIPMAP_LINEAR = $2703;
GL_TEXTURE_MAG_FILTER = $2800;
GL_TEXTURE_MIN_FILTER = $2801;
GL_TEXTURE_WRAP_S = $2802;
GL_TEXTURE_WRAP_T = $2803;
GL_TEXTURE0 = $84C0;
GL_TEXTURE1 = $84C1;
GL_TEXTURE2 = $84C2;
GL_TEXTURE3 = $84C3;
GL_TEXTURE4 = $84C4;
GL_TEXTURE5 = $84C5;
GL_TEXTURE6 = $84C6;
GL_TEXTURE7 = $84C7;
GL_TEXTURE8 = $84C8;
GL_TEXTURE9 = $84C9;
GL_TEXTURE10 = $84CA;
GL_TEXTURE11 = $84CB;
GL_TEXTURE12 = $84CC;
GL_TEXTURE13 = $84CD;
GL_TEXTURE14 = $84CE;
GL_TEXTURE15 = $84CF;
GL_TEXTURE16 = $84D0;
GL_TEXTURE17 = $84D1;
GL_TEXTURE18 = $84D2;
GL_TEXTURE19 = $84D3;
GL_TEXTURE20 = $84D4;
GL_TEXTURE21 = $84D5;
GL_TEXTURE22 = $84D6;
GL_TEXTURE23 = $84D7;
GL_TEXTURE24 = $84D8;
GL_TEXTURE25 = $84D9;
GL_TEXTURE26 = $84DA;
GL_TEXTURE27 = $84DB;
GL_TEXTURE28 = $84DC;
GL_TEXTURE29 = $84DD;
GL_TEXTURE30 = $84DE;
GL_TEXTURE31 = $84DF;
GL_REPEAT = $2901;
GL_CLAMP_TO_EDGE = $812F;
GL_PALETTE4_RGB8_OES = $8B90;
GL_PALETTE4_RGBA8_OES = $8B91;
GL_PALETTE4_R5_G6_B5_OES = $8B92;
GL_PALETTE4_RGBA4_OES = $8B93;
GL_PALETTE4_RGB5_A1_OES = $8B94;
GL_PALETTE8_RGB8_OES = $8B95;
GL_PALETTE8_RGBA8_OES = $8B96;
GL_PALETTE8_R5_G6_B5_OES = $8B97;
GL_PALETTE8_RGBA4_OES = $8B98;
GL_PALETTE8_RGB5_A1_OES = $8B99;
GL_LIGHT0 = $4000;
GL_LIGHT1 = $4001;
GL_LIGHT2 = $4002;
GL_LIGHT3 = $4003;
GL_LIGHT4 = $4004;
GL_LIGHT5 = $4005;
GL_LIGHT6 = $4006;
GL_LIGHT7 = $4007;

//EGL
EGL_VERSION_1_0 = 1;
EGL_VERSION_1_1 = 1;
EGL_VERSION_1_2 = 1;
EGL_VERSION_1_3 = 1;
EGL_FALSE = 0;
EGL_TRUE = 1;
EGL_SUCCESS = $3000;
EGL_NOT_INITIALIZED = $3001;
EGL_BAD_ACCESS = $3002;
EGL_BAD_ALLOC = $3003;
EGL_BAD_ATTRIBUTE = $3004;
EGL_BAD_CONFIG = $3005;
EGL_BAD_CONTEXT = $3006;
EGL_BAD_CURRENT_SURFACE = $3007;
EGL_BAD_DISPLAY = $3008;
EGL_BAD_MATCH = $3009;
EGL_BAD_NATIVE_PIXMAP = $300A;
EGL_BAD_NATIVE_WINDOW = $300B;
EGL_BAD_PARAMETER = $300C;
EGL_BAD_SURFACE = $300D;
EGL_CONTEXT_LOST = $300E;
EGL_BUFFER_SIZE = $3020;
EGL_ALPHA_SIZE = $3021;
EGL_BLUE_SIZE = $3022;
EGL_GREEN_SIZE = $3023;
EGL_RED_SIZE = $3024;
EGL_DEPTH_SIZE = $3025;
EGL_STENCIL_SIZE = $3026;
EGL_CONFIG_CAVEAT = $3027;
EGL_CONFIG_ID = $3028;
EGL_LEVEL = $3029;
EGL_MAX_PBUFFER_HEIGHT = $302A;
EGL_MAX_PBUFFER_PIXELS = $302B;
EGL_MAX_PBUFFER_WIDTH = $302C;
EGL_NATIVE_RENDERABLE = $302D;
EGL_NATIVE_VISUAL_ID = $302E;
EGL_NATIVE_VISUAL_TYPE = $302F;
EGL_PRESERVED_RESOURCES = $3030;
EGL_SAMPLES = $3031;
EGL_SAMPLE_BUFFERS = $3032;
EGL_SURFACE_TYPE = $3033;
EGL_TRANSPARENT_TYPE = $3034;
EGL_TRANSPARENT_BLUE_VALUE = $3035;
EGL_TRANSPARENT_GREEN_VALUE = $3036;
EGL_TRANSPARENT_RED_VALUE = $3037;
EGL_NONE = $3038;
EGL_BIND_TO_TEXTURE_RGB = $3039;
EGL_BIND_TO_TEXTURE_RGBA = $303A;
EGL_MIN_SWAP_INTERVAL = $303B;
EGL_MAX_SWAP_INTERVAL = $303C;
EGL_LUMINANCE_SIZE = $303D;
EGL_ALPHA_MASK_SIZE = $303E;
EGL_COLOR_BUFFER_TYPE = $303F;
EGL_RENDERABLE_TYPE = $3040;
EGL_MATCH_NATIVE_PIXMAP = $3041;
EGL_SLOW_CONFIG = $3050;
EGL_NON_CONFORMANT_CONFIG = $3051;
EGL_TRANSPARENT_RGB = $3052;
EGL_RGB_BUFFER = $308E;
EGL_LUMINANCE_BUFFER = $308F;
EGL_NO_TEXTURE = $305C;
EGL_TEXTURE_RGB = $305D;
EGL_TEXTURE_RGBA = $305E;
EGL_TEXTURE_2D = $305F;
EGL_PBUFFER_BIT = $01;
EGL_PIXMAP_BIT = $02;
EGL_WINDOW_BIT = $04;
EGL_OPENGL_ES_BIT = $01;
EGL_OPENVG_BIT = $02;
EGL_OPENGL_ES2_BIT = $04;
EGL_VENDOR = $3053;
EGL_VERSION = $3054;
EGL_EXTENSIONS = $3055;
EGL_CLIENT_APIS = $308D;
EGL_HEIGHT = $3056;
EGL_WIDTH = $3057;
EGL_LARGEST_PBUFFER = $3058;
EGL_TEXTURE_FORMAT = $3080;
EGL_TEXTURE_TARGET = $3081;
EGL_MIPMAP_TEXTURE = $3082;
EGL_MIPMAP_LEVEL = $3083;
EGL_RENDER_BUFFER = $3086;
EGL_COLORSPACE = $3087;
EGL_ALPHA_FORMAT = $3088;
EGL_HORIZONTAL_RESOLUTION = $3090;
EGL_VERTICAL_RESOLUTION = $3091;
EGL_PIXEL_ASPECT_RATIO = $3092;
EGL_SWAP_BEHAVIOR = $3093;
EGL_BACK_BUFFER = $3084;
EGL_SINGLE_BUFFER = $3085;
EGL_COLORSPACE_sRGB = $3089;
EGL_COLORSPACE_LINEAR = $308A;
EGL_ALPHA_FORMAT_NONPRE = $308B;
EGL_ALPHA_FORMAT_PRE = $308C;
EGL_DISPLAY_SCALING = 10000;
EGL_BUFFER_PRESERVED = $3094;
EGL_BUFFER_DESTROYED = $3095;
EGL_OPENVG_IMAGE = $3096;
EGL_CONTEXT_CLIENT_TYPE = $3097;
EGL_CONTEXT_CLIENT_VERSION = $3098;
EGL_OPENGL_ES_API = $30A0;
EGL_OPENVG_API = $30A1;
EGL_DRAW = $3059;
EGL_READ = $305A;
EGL_CORE_NATIVE_ENGINE = $305B;

EGL_DEFAULT_DISPLAY = 0;
EGL_NO_DISPLAY      = 0;
EGL_NO_CONTEXT      = nil;
EGL_NO_SURFACE      = nil;

VAR

 glActiveTexture: procedure(texture: GLenum); stdcall;
 glAlphaFunc: procedure(func: GLenum ; ref: GLclampf); stdcall;
 glAlphaFuncx: procedure(func: GLenum ; ref: GLclampx); stdcall;
 glBindTexture: procedure(target: GLenum ; texture: GLuint); stdcall;
 glBlendFunc: procedure(sfactor: GLenum ; dfactor: GLenum); stdcall;
 glClear: procedure(mask: GLbitfield); stdcall;
 glClearColor: procedure(red: GLclampf ; green: GLclampf ; blue: GLclampf ; alpha: GLclampf); stdcall;
 glClearColorx: procedure(red: GLclampx ; green: GLclampx ; blue: GLclampx ; alpha: GLclampx); stdcall;
 glClearDepthf: procedure(depth: GLclampf); stdcall;
 glClearDepthx: procedure(depth: GLclampx); stdcall;
 glClearStencil: procedure(s: GLint); stdcall;
 glClientActiveTexture: procedure(texture: GLenum); stdcall;
 glColor4f: procedure(red: GLfloat ; green: GLfloat ; blue: GLfloat ; alpha: GLfloat); stdcall;
 glColor4x: procedure(red: GLfixed ; green: GLfixed ; blue: GLfixed ; alpha: GLfixed); stdcall;
 glColorMask: procedure(red: GLboolean ; green: GLboolean ; blue: GLboolean ; alpha: GLboolean); stdcall;
 glColorPointer: procedure(size: GLint ; atype: GLenum ; stride: GLsizei ; const data : GLextern); stdcall;
 glCompressedTexImage2D: procedure(target: GLenum ; level: GLint ; internalformat: GLenum ; width: GLsizei ; height: GLsizei ; border: GLint ; imageSize: GLsizei ; const data : GLextern); stdcall;
 glCompressedTexSubImage2D: procedure(target: GLenum ; level: GLint ; xoffset: GLint ; yoffset: GLint ; width: GLsizei ; height: GLsizei ; format: GLenum ; imageSize: GLsizei ; const data : GLextern); stdcall;
 glCopyTexImage2D: procedure(target: GLenum ; level: GLint ; internalformat: GLenum ; x: GLint ; y: GLint ; width: GLsizei ; height: GLsizei ; border: GLint); stdcall;
 glCopyTexSubImage2D: procedure(target: GLenum ; level: GLint ; xoffset: GLint ; yoffset: GLint ; x: GLint ; y: GLint ; width: GLsizei ; height: GLsizei); stdcall;
 glCullFace: procedure(mode: GLenum); stdcall;
 glDeleteTextures: procedure(n: GLsizei ; const textures : PGLuint); stdcall;
 glDepthFunc: procedure(func: GLenum); stdcall;
 glDepthMask: procedure(flag: GLboolean); stdcall;
 glDepthRangef: procedure(zNear: GLclampf ; zFar: GLclampf); stdcall;
 glDepthRangex: procedure(zNear: GLclampx ; zFar: GLclampx); stdcall;
 glDisable: procedure(cap: GLenum); stdcall;
 glDisableClientState: procedure(farray: GLenum); stdcall;
 glDrawArrays: procedure(mode: GLenum ; first: GLint ; count: GLsizei); stdcall;
 glDrawElements: procedure(mode: GLenum ; count: GLsizei ; atype: GLenum ; const indices : GLextern); stdcall;
 glEnable: procedure(cap: GLenum); stdcall;
 glEnableClientState: procedure(farray: GLenum); stdcall;
 glFinish: procedure( ); stdcall;
 glFlush: procedure( ); stdcall;
 glFogf: procedure(pname: GLenum ; param: GLfloat); stdcall;
 glFogfv: procedure(pname: GLenum ; const params : PGLfloat); stdcall;
 glFogx: procedure(pname: GLenum ; param: GLfixed); stdcall;
 glFogxv: procedure(pname: GLenum ; const params : PGLfixed); stdcall;
 glFrontFace: procedure(mode: GLenum); stdcall;
 glFrustumf: procedure(left: GLfloat ; right: GLfloat ; bottom: GLfloat ; top: GLfloat ; zNear: GLfloat ; zFar: GLfloat); stdcall;
 glFrustumx: procedure(left: GLfixed ; right: GLfixed ; bottom: GLfixed ; top: GLfixed ; zNear: GLfixed ; zFar: GLfixed); stdcall;
 glGenTextures: procedure(n: GLsizei ; textures: PGLuint); stdcall;
 glGetError: function( ):GLenum; stdcall;
 glGetIntegerv: procedure(pname: GLenum ; params: PGLint); stdcall;
 glHint: procedure(target: GLenum ; mode: GLenum); stdcall;
 glLightModelf: procedure(pname: GLenum ; param: GLfloat); stdcall;
 glLightModelfv: procedure(pname: GLenum ; const params: PGLfloat); stdcall;
 glLightModelx: procedure(pname: GLenum ; param: GLfixed); stdcall;
 glLightModelxv: procedure(pname: GLenum ; const params: PGLfixed); stdcall;
 glLightf: procedure(light: GLenum ; pname: GLenum ; param: GLfloat); stdcall;
 glLightfv: procedure(light: GLenum ; pname: GLenum ; const params : PGLfloat); stdcall;
 glLightx: procedure(light: GLenum ; pname: GLenum ; param: GLfixed); stdcall;
 glLightxv: procedure(light: GLenum ; pname: GLenum ; const params: PGLfixed); stdcall;
 glLineWidth: procedure(width: GLfloat); stdcall;
 glLineWidthx: procedure(width: GLfixed); stdcall;
 glLoadIdentity: procedure( ); stdcall;
 glLoadMatrixf: procedure(const m: PGLfloat); stdcall;
 glLoadMatrixx: procedure(const m: PGLfixed); stdcall;
 glLogicOp: procedure(opcode: GLenum); stdcall;
 glMaterialf: procedure(face: GLenum ; pname: GLenum ; param: GLfloat); stdcall;
 glMaterialfv: procedure(face: GLenum ; pname: GLenum ; const params: PGLfloat); stdcall;
 glMaterialx: procedure(face: GLenum ; pname: GLenum ; param: GLfixed); stdcall;
 glMaterialxv: procedure(face: GLenum ; pname: GLenum ; const params: PGLfixed); stdcall;
 glMatrixMode: procedure(mode: GLenum); stdcall;
 glMultMatrixf: procedure(const m: PGLfloat); stdcall;
 glMultMatrixx: procedure(const m: PGLfixed); stdcall;
 glMultiTexCoord4f: procedure(target: GLenum ; s: GLfloat ; t: GLfloat ; r: GLfloat ; q: GLfloat); stdcall;
 glMultiTexCoord4x: procedure(target: GLenum ; s: GLfixed ; t: GLfixed ; r: GLfixed ; q: GLfixed); stdcall;
 glNormal3f: procedure(nx: GLfloat ; ny: GLfloat ; nz: GLfloat); stdcall;
 glNormal3x: procedure(nx: GLfixed ; ny: GLfixed ; nz: GLfixed); stdcall;
 glNormalPointer: procedure(atype: GLenum ; stride: GLsizei ; const data : GLextern); stdcall;
 glOrthof: procedure(left: GLfloat ; right: GLfloat ; bottom: GLfloat ; top: GLfloat ; zNear: GLfloat ; zFar: GLfloat); stdcall;
 glOrthox: procedure(left: GLfixed ; right: GLfixed ; bottom: GLfixed ; top: GLfixed ; zNear: GLfixed ; zFar: GLfixed); stdcall;
 glPixelStorei: procedure(pname: GLenum ; param: GLint); stdcall;
 glPointSize: procedure(size: GLfloat); stdcall;
 glPointSizex: procedure(size: GLfixed); stdcall;
 glPolygonOffset: procedure(factor: GLfloat ; units: GLfloat); stdcall;
 glPolygonOffsetx: procedure(factor: GLfixed ; units: GLfixed); stdcall;
 glPopMatrix: procedure( ); stdcall;
 glPushMatrix: procedure( ); stdcall;
 glReadPixels: procedure(x: GLint ; y: GLint ; width: GLsizei ; height: GLsizei ; format: GLenum ; atype: GLenum ; pixels : GLextern); stdcall;
 glRotatef: procedure(angle: GLfloat ; x: GLfloat ; y: GLfloat ; z: GLfloat); stdcall;
 glRotatex: procedure(angle: GLfixed ; x: GLfixed ; y: GLfixed ; z: GLfixed); stdcall;
 glSampleCoverage: procedure(value: GLclampf ; invert: GLboolean); stdcall;
 glSampleCoveragex: procedure(value: GLclampx ; invert: GLboolean); stdcall;
 glScalef: procedure(x: GLfloat ; y: GLfloat ; z: GLfloat); stdcall;
 glScalex: procedure(x: GLfixed ; y: GLfixed ; z: GLfixed); stdcall;
 glScissor: procedure(x: GLint ; y: GLint ; width: GLsizei ; height: GLsizei); stdcall;
 glShadeModel: procedure(mode: GLenum); stdcall;
 glStencilFunc: procedure(func: GLenum ; ref: GLint ; mask: GLuint); stdcall;
 glStencilMask: procedure(mask: GLuint); stdcall;
 glStencilOp: procedure(fail: GLenum ; zfail: GLenum ; zpass: GLenum); stdcall;
 glTexCoordPointer: procedure(size: GLint ; atype: GLenum ; stride: GLsizei ; const data: GLextern); stdcall;
 glTexEnvf: procedure(target: GLenum ; pname: GLenum ; param: GLfloat); stdcall;
 glTexEnvfv: procedure(target: GLenum ; pname: GLenum ; const params: PGLfloat); stdcall;
 glTexEnvx: procedure(target: GLenum ; pname: GLenum ; param: GLfixed); stdcall;
 glTexEnvxv: procedure(target: GLenum ; pname: GLenum ; const params: PGLfixed); stdcall;
 glTexImage2D: procedure(target: GLenum ; level: GLint ; internalformat: GLint ; width: GLsizei ; height: GLsizei ; border: GLint ; format: GLenum ; atype: GLenum ; const pixels: GLextern); stdcall;
 glTexParameterf: procedure(target: GLenum ; pname: GLenum ; param: GLfloat); stdcall;
 glTexParameterx: procedure(target: GLenum ; pname: GLenum ; param: GLfixed); stdcall;
 glTexSubImage2D: procedure(target: GLenum ; level: GLint ; xoffset: GLint ; yoffset: GLint ; width: GLsizei ; height: GLsizei ; format: GLenum ; atype: GLenum ; const pixels: GLextern); stdcall;
 glTranslatef: procedure(x: GLfloat ; y: GLfloat ; z: GLfloat); stdcall;
 glTranslatex: procedure(x: GLfixed ; y: GLfixed ; z: GLfixed); stdcall;
 glVertexPointer: procedure(size: GLint ; atype: GLenum ; stride: GLsizei ; const data : GLextern); stdcall;
 glViewport: procedure(x: GLint ; y: GLint ; width: GLsizei ; height: GLsizei); stdcall;
 glDeleteFramebuffers : procedure(n: GLsizei; const framebuffers: PGLuint); stdcall;
 glGetString : function(name: GLenum): PAnsiChar; stdcall;
 glTexEnvi : procedure(target: GLenum; pname: GLenum; param: GLint); stdcall;
 glTexParameteri : procedure(target: GLenum; pname: GLenum; param: GLint); stdcall;


//EGL
VAR
 eglGetProcAddress : function( name: PAnsiChar ) : Pointer; stdcall;
 eglGetError: function():EGLint; stdcall;
 eglGetDisplay: function(const display_id: EGLNativeDisplayType):EGLDisplay; stdcall;
 eglInitialize: function(dpy: EGLDisplay ; major: PEGLint ; minor: PEGLint):EGLBoolean; stdcall;
 eglTerminate: function(dpy: EGLDisplay):EGLBoolean; stdcall;
 eglGetConfigs: function(dpy: EGLDisplay ; configs: PEGLConfig ; config_size: EGLint ; num_config: PEGLint):EGLBoolean; stdcall;
 eglChooseConfig: function(dpy: EGLDisplay ; const attrib_list: PEGLint ; configs: PEGLConfig ; config_size: EGLint ; num_config: PEGLint):EGLBoolean; stdcall;
 eglGetConfigAttrib: function(dpy: EGLDisplay ; config: EGLConfig ; attribute: EGLint ; value: PEGLint):EGLBoolean; stdcall;
 eglCreateWindowSurface: function(dpy: EGLDisplay ; config: EGLConfig ; win: EGLNativeWindowType ; const attrib_list: PEGLint):EGLSurface; stdcall;
 eglCreatePbufferSurface: function(dpy: EGLDisplay ; config: EGLConfig ; const attrib_list: PEGLint):EGLSurface; stdcall;
 eglCreatePixmapSurface: function(dpy: EGLDisplay ; config: EGLConfig ; pixmap: EGLNativePixmapType ; const attrib_list: PEGLint):EGLSurface; stdcall;
 eglDestroySurface: function(dpy: EGLDisplay ; surface: EGLSurface):EGLBoolean; stdcall;
 eglQuerySurface: function(dpy: EGLDisplay ; surface: EGLSurface ; attribute: EGLint ; value: PEGLint):EGLBoolean; stdcall;
 eglBindAPI: function(api: EGLenum):EGLBoolean; stdcall;
 eglQueryAPI: function( ):EGLenum; stdcall;
 eglWaitClient: function( ):EGLBoolean; stdcall;
 eglReleaseThread: function( ):EGLBoolean; stdcall;
 eglCreatePbufferFromClientBuffer: function(dpy: EGLDisplay ; buftype: EGLenum ; buffer: EGLClientBuffer ; config: EGLConfig ; const attrib_list: EGLint):EGLSurface; stdcall;
 eglSurfaceAttrib: function(dpy: EGLDisplay ; surface: EGLSurface ; attribute: EGLint ; value: EGLint):EGLBoolean; stdcall;
 eglBindTexImage: function(dpy: EGLDisplay ; surface: EGLSurface ; buffer: EGLint):EGLBoolean; stdcall;
 eglReleaseTexImage: function(dpy: EGLDisplay ; surface: EGLSurface ; buffer: EGLint):EGLBoolean; stdcall;
 eglSwapInterval: function(dpy: EGLDisplay ; interval: EGLint):EGLBoolean; stdcall;
 eglCreateContext: function(dpy: EGLDisplay ; config: EGLConfig ; share_context: EGLContext ; const attrib_list: PEGLint):EGLContext; stdcall;
 eglDestroyContext: function(dpy: EGLDisplay ; ctx: EGLContext):EGLBoolean; stdcall;
 eglMakeCurrent: function(dpy: EGLDisplay ; draw: EGLSurface ; read: EGLSurface ; ctx: EGLContext):EGLBoolean; stdcall;
 eglGetCurrentContext: function( ):EGLContext; stdcall;
 eglGetCurrentSurface: function(readdraw: EGLint):EGLSurface; stdcall;
 eglGetCurrentDisplay: function( ):EGLDisplay; stdcall;
 eglQueryContext: function(dpy: EGLDisplay ; ctx: EGLContext ; attribute: EGLint ; value: PEGLint):EGLBoolean; stdcall;
 eglWaitGL: function( ):EGLBoolean; stdcall;
 eglWaitNative: function(engine: EGLint):EGLBoolean; stdcall;
 eglSwapBuffers: function(dpy: EGLDisplay ; surface: EGLSurface):EGLBoolean; stdcall;
 eglCopyBuffers: function(dpy: EGLDisplay ; surface: EGLSurface ; target: EGLNativePixmapType):EGLBoolean; stdcall;


//INIT

function InitLibrary(libname: string): boolean;

implementation


var
  CONV_LibHandle: THandle = 0;

function convLoadLibrary(Name: PChar): THandle;
begin

  CONV_LibHandle :=0;
  Result := LoadLibrary(Name);

end;

function convFreeLibrary(LibHandle: THandle): Boolean;
begin
  if LibHandle = 0 then
    Result := False
  else
    Result := FreeLibrary(LibHandle);
end;

function convGetProcAddress(ProcName: string; LibHandle: THandle = 0): Pointer;
begin

if LibHandle = 0 then
    LibHandle := CONV_LibHandle;

if LibHandle <> 0 then
 begin

  Result := GetProcAddress(LibHandle, Pchar(ProcName));
  if Result=nil then
  Result := GetProcAddress(LibHandle, Pchar(ProcName+'OES'));

end;

end;

function InitLibrary(libname: string): boolean;
begin
  result := false;

  // free opened libraries
  if CONV_LibHandle <> 0 then  convFreeLibrary(CONV_LibHandle);

  // load library
  CONV_LibHandle := convLoadLibrary(PChar(LibName));

if CONV_LibHandle = 0 then exit;

 Pointer(glActiveTexture) := ConvGetProcAddress('glActiveTexture');
 Pointer(glAlphaFunc) := ConvGetProcAddress('glAlphaFunc');
 Pointer(glAlphaFuncx) := ConvGetProcAddress('glAlphaFuncx');
 Pointer(glBindTexture) := ConvGetProcAddress('glBindTexture');
 Pointer(glBlendFunc) := ConvGetProcAddress('glBlendFunc');
 Pointer(glClear) := ConvGetProcAddress('glClear');
 Pointer(glClearColor) := ConvGetProcAddress('glClearColor');
 Pointer(glClearColorx) := ConvGetProcAddress('glClearColorx');
 Pointer(glClearDepthf) := ConvGetProcAddress('glClearDepthf');
 Pointer(glClearDepthx) := ConvGetProcAddress('glClearDepthx');
 Pointer(glClearStencil) := ConvGetProcAddress('glClearStencil');
 Pointer(glClientActiveTexture) := ConvGetProcAddress('glClientActiveTexture');
 Pointer(glColor4f) := ConvGetProcAddress('glColor4f');
 Pointer(glColor4x) := ConvGetProcAddress('glColor4x');
 Pointer(glColorMask) := ConvGetProcAddress('glColorMask');
 Pointer(glColorPointer) := ConvGetProcAddress('glColorPointer');
 Pointer(glCompressedTexImage2D) := ConvGetProcAddress('glCompressedTexImage2D');
 Pointer(glCompressedTexSubImage2D) := ConvGetProcAddress('glCompressedTexSubImage2D');
 Pointer(glCopyTexImage2D) := ConvGetProcAddress('glCopyTexImage2D');
 Pointer(glCopyTexSubImage2D) := ConvGetProcAddress('glCopyTexSubImage2D');
 Pointer(glCullFace) := ConvGetProcAddress('glCullFace');
 Pointer(glDeleteTextures) := ConvGetProcAddress('glDeleteTextures');
 Pointer(glDepthFunc) := ConvGetProcAddress('glDepthFunc');
 Pointer(glDepthMask) := ConvGetProcAddress('glDepthMask');
 Pointer(glDepthRangef) := ConvGetProcAddress('glDepthRangef');
 Pointer(glDepthRangex) := ConvGetProcAddress('glDepthRangex');
 Pointer(glDisable) := ConvGetProcAddress('glDisable');
 Pointer(glDisableClientState) := ConvGetProcAddress('glDisableClientState');
 Pointer(glDrawArrays) := ConvGetProcAddress('glDrawArrays');
 Pointer(glDrawElements) := ConvGetProcAddress('glDrawElements');
 Pointer(glEnable) := ConvGetProcAddress('glEnable');
 Pointer(glEnableClientState) := ConvGetProcAddress('glEnableClientState');
 Pointer(glFinish) := ConvGetProcAddress('glFinish');
 Pointer(glFlush) := ConvGetProcAddress('glFlush');
 Pointer(glFogf) := ConvGetProcAddress('glFogf');
 Pointer(glFogfv) := ConvGetProcAddress('glFogfv');
 Pointer(glFogx) := ConvGetProcAddress('glFogx');
 Pointer(glFogxv) := ConvGetProcAddress('glFogxv');
 Pointer(glFrontFace) := ConvGetProcAddress('glFrontFace');
 Pointer(glFrustumf) := ConvGetProcAddress('glFrustumf');
 Pointer(glFrustumx) := ConvGetProcAddress('glFrustumx');
 Pointer(glGenTextures) := ConvGetProcAddress('glGenTextures');
 Pointer(glGetError) := ConvGetProcAddress('glGetError');
 Pointer(glGetIntegerv) := ConvGetProcAddress('glGetIntegerv');
 Pointer(glHint) := ConvGetProcAddress('glHint');
 Pointer(glLightModelf) := ConvGetProcAddress('glLightModelf');
 Pointer(glLightModelfv) := ConvGetProcAddress('glLightModelfv');
 Pointer(glLightModelx) := ConvGetProcAddress('glLightModelx');
 Pointer(glLightModelxv) := ConvGetProcAddress('glLightModelxv');
 Pointer(glLightf) := ConvGetProcAddress('glLightf');
 Pointer(glLightfv) := ConvGetProcAddress('glLightfv');
 Pointer(glLightx) := ConvGetProcAddress('glLightx');
 Pointer(glLightxv) := ConvGetProcAddress('glLightxv');
 Pointer(glLineWidth) := ConvGetProcAddress('glLineWidth');
 Pointer(glLineWidthx) := ConvGetProcAddress('glLineWidthx');
 Pointer(glLoadIdentity) := ConvGetProcAddress('glLoadIdentity');
 Pointer(glLoadMatrixf) := ConvGetProcAddress('glLoadMatrixf');
 Pointer(glLoadMatrixx) := ConvGetProcAddress('glLoadMatrixx');
 Pointer(glLogicOp) := ConvGetProcAddress('glLogicOp');
 Pointer(glMaterialf) := ConvGetProcAddress('glMaterialf');
 Pointer(glMaterialfv) := ConvGetProcAddress('glMaterialfv');
 Pointer(glMaterialx) := ConvGetProcAddress('glMaterialx');
 Pointer(glMaterialxv) := ConvGetProcAddress('glMaterialxv');
 Pointer(glMatrixMode) := ConvGetProcAddress('glMatrixMode');
 Pointer(glMultMatrixf) := ConvGetProcAddress('glMultMatrixf');
 Pointer(glMultMatrixx) := ConvGetProcAddress('glMultMatrixx');
 Pointer(glMultiTexCoord4f) := ConvGetProcAddress('glMultiTexCoord4f');
 Pointer(glMultiTexCoord4x) := ConvGetProcAddress('glMultiTexCoord4x');
 Pointer(glNormal3f) := ConvGetProcAddress('glNormal3f');
 Pointer(glNormal3x) := ConvGetProcAddress('glNormal3x');
 Pointer(glNormalPointer) := ConvGetProcAddress('glNormalPointer');
 Pointer(glOrthof) := ConvGetProcAddress('glOrthof');
 Pointer(glOrthox) := ConvGetProcAddress('glOrthox');
 Pointer(glPixelStorei) := ConvGetProcAddress('glPixelStorei');
 Pointer(glPointSize) := ConvGetProcAddress('glPointSize');
 Pointer(glPointSizex) := ConvGetProcAddress('glPointSizex');
 Pointer(glPolygonOffset) := ConvGetProcAddress('glPolygonOffset');
 Pointer(glPolygonOffsetx) := ConvGetProcAddress('glPolygonOffsetx');
 Pointer(glPopMatrix) := ConvGetProcAddress('glPopMatrix');
 Pointer(glPushMatrix) := ConvGetProcAddress('glPushMatrix');
 Pointer(glReadPixels) := ConvGetProcAddress('glReadPixels');
 Pointer(glRotatef) := ConvGetProcAddress('glRotatef');
 Pointer(glRotatex) := ConvGetProcAddress('glRotatex');
 Pointer(glSampleCoverage) := ConvGetProcAddress('glSampleCoverage');
 Pointer(glSampleCoveragex) := ConvGetProcAddress('glSampleCoveragex');
 Pointer(glScalef) := ConvGetProcAddress('glScalef');
 Pointer(glScalex) := ConvGetProcAddress('glScalex');
 Pointer(glScissor) := ConvGetProcAddress('glScissor');
 Pointer(glShadeModel) := ConvGetProcAddress('glShadeModel');
 Pointer(glStencilFunc) := ConvGetProcAddress('glStencilFunc');
 Pointer(glStencilMask) := ConvGetProcAddress('glStencilMask');
 Pointer(glStencilOp) := ConvGetProcAddress('glStencilOp');
 Pointer(glTexCoordPointer) := ConvGetProcAddress('glTexCoordPointer');
 Pointer(glTexEnvf) := ConvGetProcAddress('glTexEnvf');
 Pointer(glTexEnvfv) := ConvGetProcAddress('glTexEnvfv');
 Pointer(glTexEnvx) := ConvGetProcAddress('glTexEnvx');
 Pointer(glTexEnvxv) := ConvGetProcAddress('glTexEnvxv');
 Pointer(glTexImage2D) := ConvGetProcAddress('glTexImage2D');
 Pointer(glTexParameterf) := ConvGetProcAddress('glTexParameterf');
 Pointer(glTexParameterx) := ConvGetProcAddress('glTexParameterx');
 Pointer(glTexSubImage2D) := ConvGetProcAddress('glTexSubImage2D');
 Pointer(glTranslatef) := ConvGetProcAddress('glTranslatef');
 Pointer(glTranslatex) := ConvGetProcAddress('glTranslatex');
 Pointer(glVertexPointer) := ConvGetProcAddress('glVertexPointer');
 Pointer(glViewport) := ConvGetProcAddress('glViewport');
 Pointer(glDeleteFramebuffers) := ConvGetProcAddress('glDeleteFramebuffers');
 Pointer(glGetString) := ConvGetProcAddress('glGetString');
 Pointer(glTexEnvi) := ConvGetProcAddress('glTexEnvi');
 Pointer(glTexParameteri) := ConvGetProcAddress('glTexParameteri');

  //EGL
 Pointer(eglGetProcAddress) := ConvGetProcAddress('eglGetProcAddress');
 Pointer(eglGetError) := ConvGetProcAddress('eglGetError');
 Pointer(eglGetDisplay) := ConvGetProcAddress('eglGetDisplay');
 Pointer(eglInitialize) := ConvGetProcAddress('eglInitialize');
 Pointer(eglTerminate) := ConvGetProcAddress('eglTerminate');
 Pointer(eglGetConfigs) := ConvGetProcAddress('eglGetConfigs');
 Pointer(eglChooseConfig) := ConvGetProcAddress('eglChooseConfig');
 Pointer(eglGetConfigAttrib) := ConvGetProcAddress('eglGetConfigAttrib');
 Pointer(eglCreateWindowSurface) := ConvGetProcAddress('eglCreateWindowSurface');
 Pointer(eglCreatePbufferSurface) := ConvGetProcAddress('eglCreatePbufferSurface');
 Pointer(eglCreatePixmapSurface) := ConvGetProcAddress('eglCreatePixmapSurface');
 Pointer(eglDestroySurface) := ConvGetProcAddress('eglDestroySurface');
 Pointer(eglQuerySurface) := ConvGetProcAddress('eglQuerySurface');
 Pointer(eglBindAPI) := ConvGetProcAddress('eglBindAPI');
 Pointer(eglQueryAPI) := ConvGetProcAddress('eglQueryAPI');
 Pointer(eglWaitClient) := ConvGetProcAddress('eglWaitClient');
 Pointer(eglReleaseThread) := ConvGetProcAddress('eglReleaseThread');
 Pointer(eglCreatePbufferFromClientBuffer) := ConvGetProcAddress('eglCreatePbufferFromClientBuffer');
 Pointer(eglSurfaceAttrib) := ConvGetProcAddress('eglSurfaceAttrib');
 Pointer(eglBindTexImage) := ConvGetProcAddress('eglBindTexImage');
 Pointer(eglReleaseTexImage) := ConvGetProcAddress('eglReleaseTexImage');
 Pointer(eglSwapInterval) := ConvGetProcAddress('eglSwapInterval');
 Pointer(eglCreateContext) := ConvGetProcAddress('eglCreateContext');
 Pointer(eglDestroyContext) := ConvGetProcAddress('eglDestroyContext');
 Pointer(eglMakeCurrent) := ConvGetProcAddress('eglMakeCurrent');
 Pointer(eglGetCurrentContext) := ConvGetProcAddress('eglGetCurrentContext');
 Pointer(eglGetCurrentSurface) := ConvGetProcAddress('eglGetCurrentSurface');
 Pointer(eglGetCurrentDisplay) := ConvGetProcAddress('eglGetCurrentDisplay');
 Pointer(eglQueryContext) := ConvGetProcAddress('eglQueryContext');
 Pointer(eglWaitGL) := ConvGetProcAddress('eglWaitGL');
 Pointer(eglWaitNative) := ConvGetProcAddress('eglWaitNative');
 Pointer(eglSwapBuffers) := ConvGetProcAddress('eglSwapBuffers');
 Pointer(eglCopyBuffers) := ConvGetProcAddress('eglCopyBuffers');


 result := true;

end;


function InitGLES11 : Boolean;
begin
    InitLibrary(convertedlib);

    Result := Assigned( eglGetDisplay ) and Assigned( eglInitialize ) and Assigned( eglTerminate ) and Assigned( eglChooseConfig ) and
              Assigned( eglCreateWindowSurface ) and Assigned( eglDestroySurface ) and Assigned( eglCreateContext ) and Assigned( eglDestroyContext ) and
              Assigned( eglMakeCurrent ) and Assigned( eglSwapBuffers );
end;

procedure FreeGLES11;
begin

  if NOT CONV_LibHandle=0 Then
    convFreeLibrary(CONV_LibHandle);

end;

end.

