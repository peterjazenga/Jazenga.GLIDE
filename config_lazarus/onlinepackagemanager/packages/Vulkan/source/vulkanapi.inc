{**********************************************************************
 Package pl_Vulkan
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}


{$IFDEF Win32}
  {$DEFINE VK_WIN}
  {$DEFINE VK_WIN32}
  {$DEFINE VK_USE_PLATFORM_WIN32_KHR}
{$ENDIF}

{$IFDEF Win64}
  {$DEFINE VK_WIN}
  {$DEFINE VK_WIN64}
  {$DEFINE VK_USE_PLATFORM_WIN32_KHR}
{$ENDIF}


{$IFDEF Android}
  {$DEFINE VK_USE_PLATFORM_ANDROID_KHR}
  {$DEFINE VulkanUseAndroidUnits}
{$ENDIF}

{$IFDEF UNIX}
  {$DEFINE VK_CDECL}

  {$DEFINE VK_USE_PLATFORM_XLIB_KHR}
  {$DEFINE VulkanUseXLIBUnits}
  {$DEFINE XLIB}

  {.$DEFINE VK_USE_PLATFORM_XCB_KHR)}
  {.$DEFINE (VulkanUseXCBUnits}
  {.$DEFINE XCB}

  {.$DEFINE VK_USE_PLATFORM_WAYLAND_KHR}
  {.$DEFINE VulkanUseWaylandUnits}
  {.$DEFINE Wayland}

  {.$DEFINE VK_USE_PLATFORM_MIR_KHR}
  {.$DEFINE VulkanUseMirUnits}
  {.$DEFINE Mir}

  {.$DEFINE VK_USE_PLATFORM_ANDROID_KHR}
  {.$DEFINE VulkanUseAndroidUnits}
  {.$DEFINE Android}
{$ENDIF}

{$IFDEF DARWIN}
  {$DEFINE VK_CDECL}
{$ENDIF}

{$IF DEFINED(CPU64)}
  {$DEFINE VK_CPU64}
{$ELSEIF DEFINED(CPU32)}
  {$DEFINE VK_CPU32}
{$ELSEIF DEFINED(CPU16)}
  {$DEFINE VK_CPU16}
{$ENDIF}

