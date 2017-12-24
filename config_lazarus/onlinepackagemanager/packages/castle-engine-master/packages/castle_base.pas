{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit castle_base;

{$warn 5023 off : no warning about unused units}
interface

uses
  Castle3D, CastleBoxes, CastleCameras, CastleConvexHull, CastleCubeMaps, 
  CastleFrustum, CastleGeometryArrays, CastleNURBS, CastleInternalOctree, 
  CastleQuaternions, CastleRays, CastleSectors, CastleSpaceFillingCurves, 
  CastleSphereSampling, CastleSphericalHarmonics, CastleTriangles, 
  CastleTriangulate, CastleGLBoxes, CastleGLShadowVolumes, CastleALUtils, 
  CastleEFX, CastleOgg, CastleOpenAL, CastleSoundAllocator, CastleSoundEngine, 
  CastleSoundFile, CastleVorbisCodec, CastleVorbisDecoder, CastleVorbisFile, 
  CastleApplicationProperties, CastleClassUtils, CastleColors, CastleConfig, 
  CastleDynLib, CastleFileFilters, CastleFilesUtils, CastleFindFiles, 
  CastleGenericLists, CastleGzioInternal, CastleInterfaces, CastleLog, 
  CastleMessaging, CastleParameters, CastleProgress, CastleProgressConsole, 
  CastleRandom, CastleRecentFiles, CastleRectangles, CastleShaders, 
  CastleStreamUtils, CastleStringUtils, CastleTimeUtils, CastleUnicode, 
  CastleUtils, CastleVectors, CastleWarnings, CastleXMLCfgInternal, 
  CastleXMLConfig, CastleXMLUtils, CastleZLib, CastleZStream, CastleGLVersion, 
  CastleNoise, CastleScript, CastleScriptArrays, CastleScriptCoreFunctions, 
  CastleScriptImages, CastleScriptLexer, CastleScriptParser, 
  CastleScriptVectors, CastleScriptXML, CastleCurves, CastleFont2Pascal, 
  CastleFreeType, CastleFreeTypeH, CastleFtFont, 
  CastleTextureFont_DejaVuSans_10, CastleTextureFont_DejaVuSansMono_18, 
  CastleTextureFont_DejaVuSansMonoBold_15, CastleTextureFont_DjvMono_20, 
  CastleTextureFont_DjvMonoB_20, CastleTextureFont_DjvMonoBO_20, 
  CastleTextureFont_DjvMonoO_20, CastleTextureFont_DjvSans_20, 
  CastleTextureFont_DjvSansB_20, CastleTextureFont_DjvSansBO_20, 
  CastleTextureFont_DjvSansO_20, CastleTextureFont_DjvSerif_20, 
  CastleTextureFont_DjvSerifB_20, CastleTextureFont_DjvSerifBI_20, 
  CastleTextureFont_DjvSerifI_20, CastleTextureFontData, CastleFontFamily, 
  CastleFonts, Castle2DSceneManager, CastleCreatures, CastleGameNotifications, 
  CastleItems, CastleLevels, CastlePlayer, CastleResources, 
  CastleSceneManager, CastleCompositeImage, CastleFPWritePNG, CastleImages, 
  CastlePng, CastleTextureImages, CastleVideos, CastleGLImages, 
  CastleGLShaders, CastleGLUtils, CastleScreenEffects, CastleDataURI, 
  CastleDownload, CastleURIUtils, CastleAds, CastleAnalytics, CastleGiftiz, 
  CastleGooglePlayGames, CastleHelpshift, CastleInAppPurchases, 
  CastleOpenDocument, CastleInputs, CastleJoysticks, CastleKeysMouse, 
  CastleTiledMap, CastleUIControls, CastleControls, CastleControlsImages, 
  CastleFlashEffect, CastleGLContainer, CastleInspectorControl, 
  CastleNotifications, CastleOnScreenMenu, CastleInternalPk3DConnexion, 
  CastleArraysGenerator, CastleInternalNodeInterpolator, 
  CastleInternalNormals, CastleMaterialProperties, CastleRayTracer, 
  CastleRenderingCamera, CastleSceneCore, CastleShapeInternalShadowVolumes, 
  CastleInternalShapeOctree, CastleShapes, CastleTerrain, 
  CastleInternalTriangleOctree, CastleBackground, CastleGLCubeMaps, 
  CastlePrecalculatedAnimation, CastleRenderer, CastleRendererInternalLights, 
  CastleRendererInternalShader, CastleRendererInternalTextureEnv, CastleScene, 
  CastleSceneInternalBlending, CastleSceneInternalOcclusion, 
  CastleSceneInternalShape, CastleShapeInternalRenderShadowVolumes, 
  X3DCameraUtils, X3DCastleScript, X3DFields, X3DLexer, X3DLoad, 
  X3DLoadInternal3DS, X3DLoadInternalCollada, X3DLoadInternalGEO, 
  X3DLoadInternalMD3, X3DLoadInternalOBJ, X3DLoadInternalSpine, 
  X3DLoadInternalUtils, X3DNodes, X3DShadowMaps, X3DTime, X3DTriangles, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CastleXMLConfig', @CastleXMLConfig.Register);
  RegisterUnit('CastleSceneManager', @CastleSceneManager.Register);
  RegisterUnit('CastleControls', @CastleControls.Register);
  RegisterUnit('CastleOnScreenMenu', @CastleOnScreenMenu.Register);
  RegisterUnit('CastlePrecalculatedAnimation', 
    @CastlePrecalculatedAnimation.Register);
  RegisterUnit('CastleScene', @CastleScene.Register);
end;

initialization
  RegisterPackage('castle_base', @Register);
end.
