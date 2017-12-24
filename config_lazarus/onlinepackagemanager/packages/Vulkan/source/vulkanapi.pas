{***********************************************************************************
                       PilotLogic Software House.

Package pl_Vulkan
This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
************************************************************************************}
{
  This header generated from the Khronos Vulkan XML API Registry.
}

unit vulkanapi;

{$mode objfpc}{$H+}

{$IFNDEF WINDOWS}
    {$LINKLIB c}
{$ENDIF}
{$MINENUMSIZE 4}
{$PACKSET 4}

{$I vulkanapi.inc}

interface

uses {$if defined(Windows)}
      Windows,
     {$elseif defined(Unix)}
      BaseUnix,UnixType,dl,
     {$ifend}
     {$if defined(XLIB) and defined(VulkanUseXLIBUnits)}x,xlib,{$ifend}
     {$if defined(XCB) and defined(VulkanUseXCBUnits)}xcb,{$ifend}
     {$if defined(Mir) and defined(VulkanUseMirUnits)}Mir,{$ifend}
     {$if defined(Wayland) and defined(VulkanUseWaylandUnits)}Wayland,{$ifend}
     {$if defined(Android) and defined(VulkanUseAndroidUnits)}Android,{$ifend}
     SysUtils,
     dynlibs;

Const
{$IFDEF VK_USE_PLATFORM_ANDROID_KHR}
  VK_DEFAULT_LIB_NAME = 'libvulkan.so';
{$ENDIF}
{$IFDEF VK_USE_PLATFORM_MIR_KHR}
  VK_DEFAULT_LIB_NAME = 'libvulkan.so';
{$ENDIF}
{$IFDEF VK_USE_PLATFORM_WAYLAND_KHR}
  VK_DEFAULT_LIB_NAME = 'libvulkan.so.1';
{$ENDIF}
{$IFDEF VK_USE_PLATFORM_WIN32_KHR}
  VK_DEFAULT_LIB_NAME = 'vulkan-1.dll';
{$ENDIF}
{$IFDEF VK_USE_PLATFORM_XLIB_KHR}
  VK_DEFAULT_LIB_NAME = 'libvulkan.so';
{$ENDIF}
{$IFDEF VK_USE_PLATFORM_XCB_KHR}
  VK_DEFAULT_LIB_NAME = 'libvulkan.so';
{$ENDIF}

type
     TVkHandle = Pointer;

     TVkInt8=Int8;
     PPVkInt8=^PVkInt8;
     PVkInt8=^TVkInt8;

     TVkUInt8=UInt8;
     PPVkUInt8=^PVkUInt8;
     PVkUInt8=^TVkUInt8;

     TVkInt16=Int16;
     PPVkInt16=^PVkInt16;
     PVkInt16=^TVkInt16;

     TVkUInt16=UInt16;
     PPVkUInt16=^PVkUInt16;
     PVkUInt16=^TVkUInt16;

     TVkInt32=Int32;
     PPVkInt32=^PVkInt32;
     PVkInt32=^TVkInt32;

     TVkUInt32=UInt32;
     PPVkUInt32=^PVkUInt32;
     PVkUInt32=^TVkUInt32;

     TVkInt64=Int64;
     PPVkInt64=^PVkInt64;
     PVkInt64=^TVkInt64;

     TVkUInt64=UInt64;
     PPVkUInt64=^PVkUInt64;
     PVkUInt64=^TVkUInt64;

     TVkChar=AnsiChar;
     PPVkChar=^PVkChar;
     PVkChar=PAnsiChar;

     TVkPointer=Pointer;
     PPVkPointer=^PVkPointer;
     PVkPointer=^TVkPointer;

     PPVkVoid=^PVkVoid;
     PVkVoid=Pointer;

     TVkHalfFloat=TVkUInt16;
     PPVkHalfFloat=^PVkHalfFloat;
     PVkHalfFloat=^TVkHalfFloat;

     TVkFloat=Single;
     PPVkFloat=^PVkFloat;
     PVkFloat=^TVkFloat;

     TVkDouble=Double;
     PPVkDouble=^PVkDouble;
     PVkDouble=^TVkDouble;

     PPVkPtrUInt=^PVkPtrUInt;
     PPVkPtrInt=^PVkPtrInt;
     PVkPtrUInt=^TVkPtrUInt;
     PVkPtrInt=^TVkPtrInt;

     TVkPtrUInt=PtrUInt;
     TVkPtrInt=PtrInt;

     TVkSizeUInt=TVkPtrUInt;
     PPVkSizeUInt=^PVkSizeUInt;
     PVkSizeUInt=^TVkSizeUInt;

     PPVkSizeInt=^PVkSizeInt;
     PVkSizeInt=^TVkSizeInt;
     TVkSizeInt=TVkPtrInt;

     TVkSize=TVkPtrUInt;
     PPVkSize=^PVkSizeUInt;
     PVkSize=^TVkSizeUInt;

     PPVkPtrDiff=^PVkPtrDiff;
     PVkPtrDiff=^TVkPtrDiff;
     TVkPtrDiff=TVkPtrInt;

     PPVkCharString=^PVkCharString;
     PVkCharString=^TVkCharString;
     TVkCharString=AnsiString;

     TVkVersion = TVkInt32;

{$ifdef Android}
     PPVkAndroidANativeWindow=^PVkAndroidANativeWindow;
     PVkAndroidANativeWindow={$ifdef VulkanUseAndroidUnits}PANativeWindow{$else}TVkPointer{$endif};
{$endif}

{$ifdef Mir}
     PPVkMirConnection=^PVkMirConnection;
     PVkMirConnection={$ifdef VulkanUseMirUnits}PMirConnection{$else}TVkPointer{$endif};

     PPVkMirSurface=^PVkMirSurface;
     PVkMirSurface={$ifdef VulkanUseMirUnits}PMirSurface{$else}TVkPointer{$endif};
{$endif}

{$ifdef Wayland}
     PPVkWaylandDisplay=^PVkWaylandDisplay;
     PVkWaylandDisplay={$ifdef VulkanUseWaylandUnits}Pwl_display{$else}TVkPointer{$endif};

     PPVkWaylandSurface=^PVkWaylandSurface;
     PVkWaylandSurface={$ifdef VulkanUseWaylandUnits}Pwl_surface{$else}TVkPointer{$endif};
{$endif}

{$ifdef XCB}
     PPVkXCBConnection=^PVkXCBConnection;
     PVkXCBConnection={$ifdef VulkanUseXCBUnits}Pxcb_connection_t{$else}TVkPointer{$endif};

     PPVkXCBVisualID=^PVkXCBVisualID;
     PVkXCBVisualID={$ifdef VulkanUseXCBUnits}Pxcb_visualid_t{$else}^TVkXCBVisualID{$endif};
     TVkXCBVisualID={$if defined(VulkanUseXCBUnits)}Pxcb_visualid_t{$elseif defined(CPU64)}TVkUInt64{$else}TVKUInt32{$ifend};

     PPVkXCBWindow=^PVkXCBWindow;
     PVkXCBWindow={$ifdef VulkanUseXCBUnits}Pxcb_window_t{$else}^TVkXCBWindow{$endif};
     TVkXCBWindow={$if defined(VulkanUseXCBUnits)}Txcb_window_t{$elseif defined(CPU64)}TVkUInt64{$else}TVKUInt32{$ifend};
{$endif}

{$ifdef XLIB}
     PPVkXLIBDisplay=^PVkXLIBDisplay;
     PVkXLIBDisplay={$ifdef VulkanUseXLIBUnits}PDisplay{$else}TVkPointer{$endif};
     {$ifdef VulkanUseXLIBUnits}TVkXLIBDisplay=TDisplay;{$endif}

     PPVkXLIBVisualID=^PVkXLIBVisualID;
     PVkXLIBVisualID={$ifdef VulkanUseXLIBUnits}PVisualID{$else}^TVkXLIBVisualID{$endif};
     TVkXLIBVisualID={$if defined(VulkanUseXLIBUnits)}TVisualID{$elseif defined(CPU64)}TVkUInt64{$else}TVKUInt32{$ifend};

     PPVkXLIBWindow=^PVkXLIBWindow;
     PVkXLIBWindow={$ifdef VulkanUseXLIBUnits}PWindow{$else}^TVkXLIBWindow{$endif};
     TVkXLIBWindow={$if defined(VulkanUseXLIBUnits)}TWindow{$elseif defined(CPU64)}TVkUInt64{$else}TVKUInt32{$ifend};
{$endif}

//========== CONST ===================
const
      VK_NULL_HANDLE=0;
      VK_NULL_INSTANCE=0;
      VK_INVALID_NDP_HANDLE=0;
      VK_INVALID_HANDLE=0;

      VK_API_VERSION=(1 shl 22) or (0 shl 12) or (0 shl 0);

      VK_API_VERSION_1_0=(1 shl 22) or (0 shl 12) or (0 shl 0);

      VK_HEADER_VERSION=57;

      VK_MAX_PHYSICAL_DEVICE_NAME_SIZE=256;
      VK_UUID_SIZE=16;
      VK_LUID_SIZE_KHR=8;
      VK_MAX_EXTENSION_NAME_SIZE=256;
      VK_MAX_DESCRIPTION_SIZE=256;
      VK_MAX_MEMORY_TYPES=32;
      VK_MAX_MEMORY_HEAPS=16;                                                    //< The maximum number of unique memory heaps, each of which supporting 1 or more memory types
      VK_LOD_CLAMP_NONE=1000.0;
      VK_REMAINING_MIP_LEVELS=TVkUInt32($ffffffff);
      VK_REMAINING_ARRAY_LAYERS=TVkUInt32($ffffffff);
      VK_WHOLE_SIZE=TVkUInt64($ffffffffffffffff);
      VK_ATTACHMENT_UNUSED=TVkUInt32($ffffffff);
      VK_TRUE=1;
      VK_FALSE=0;
      VK_QUEUE_FAMILY_IGNORED=TVkUInt32($ffffffff);
      VK_QUEUE_FAMILY_EXTERNAL_KHR=0;
      VK_SUBPASS_EXTERNAL=TVkUInt32($ffffffff);
      VK_MAX_DEVICE_GROUP_SIZE_KHX=32;
      VK_KHR_SURFACE_SPEC_VERSION=25;
      VK_KHR_SURFACE_EXTENSION_NAME='VK_KHR_surface';
      VK_KHR_SWAPCHAIN_SPEC_VERSION=68;
      VK_KHR_SWAPCHAIN_EXTENSION_NAME='VK_KHR_swapchain';
      VK_KHR_DISPLAY_SPEC_VERSION=21;
      VK_KHR_DISPLAY_EXTENSION_NAME='VK_KHR_display';
      VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION=9;
      VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME='VK_KHR_display_swapchain';
      VK_KHR_XLIB_SURFACE_SPEC_VERSION=6;
      VK_KHR_XLIB_SURFACE_EXTENSION_NAME='VK_KHR_xlib_surface';
      VK_KHR_XCB_SURFACE_SPEC_VERSION=6;
      VK_KHR_XCB_SURFACE_EXTENSION_NAME='VK_KHR_xcb_surface';
      VK_KHR_WAYLAND_SURFACE_SPEC_VERSION=6;
      VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME='VK_KHR_wayland_surface';
      VK_KHR_MIR_SURFACE_SPEC_VERSION=4;
      VK_KHR_MIR_SURFACE_EXTENSION_NAME='VK_KHR_mir_surface';
      VK_KHR_ANDROID_SURFACE_SPEC_VERSION=6;
      VK_KHR_ANDROID_SURFACE_EXTENSION_NAME='VK_KHR_android_surface';
      VK_KHR_WIN32_SURFACE_SPEC_VERSION=6;
      VK_KHR_WIN32_SURFACE_EXTENSION_NAME='VK_KHR_win32_surface';
      VK_ANDROID_NATIVE_BUFFER_SPEC_VERSION=4;
      VK_ANDROID_NATIVE_BUFFER_NUMBER=11;
      VK_ANDROID_NATIVE_BUFFER_NAME='VK_ANDROID_native_buffer';
      VK_EXT_DEBUG_REPORT_SPEC_VERSION=8;
      VK_EXT_DEBUG_REPORT_EXTENSION_NAME='VK_EXT_debug_report';
      VK_NV_GLSL_SHADER_SPEC_VERSION=1;
      VK_NV_GLSL_SHADER_EXTENSION_NAME='VK_NV_glsl_shader';
      VK_EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION=1;
      VK_EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME='VK_EXT_depth_range_unrestricted';
      VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION=1;
      VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME='VK_KHR_sampler_mirror_clamp_to_edge';
      VK_IMG_FILTER_CUBIC_SPEC_VERSION=1;
      VK_IMG_FILTER_CUBIC_EXTENSION_NAME='VK_IMG_filter_cubic';
      VK_AMD_EXTENSION_17_SPEC_VERSION=0;
      VK_AMD_EXTENSION_17_EXTENSION_NAME='VK_AMD_extension_17';
      VK_AMD_EXTENSION_18_SPEC_VERSION=0;
      VK_AMD_EXTENSION_18_EXTENSION_NAME='VK_AMD_extension_18';
      VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION=1;
      VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME='VK_AMD_rasterization_order';
      VK_AMD_EXTENSION_20_SPEC_VERSION=0;
      VK_AMD_EXTENSION_20_EXTENSION_NAME='VK_AMD_extension_20';
      VK_AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION=1;
      VK_AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME='VK_AMD_shader_trinary_minmax';
      VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_SPEC_VERSION=1;
      VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME='VK_AMD_shader_explicit_vertex_parameter';
      VK_EXT_DEBUG_MARKER_SPEC_VERSION=4;
      VK_EXT_DEBUG_MARKER_EXTENSION_NAME='VK_EXT_debug_marker';
      VK_AMD_EXTENSION_24_SPEC_VERSION=0;
      VK_AMD_EXTENSION_24_EXTENSION_NAME='VK_AMD_extension_24';
      VK_AMD_EXTENSION_25_SPEC_VERSION=0;
      VK_AMD_EXTENSION_25_EXTENSION_NAME='VK_AMD_extension_25';
      VK_AMD_GCN_SHADER_SPEC_VERSION=1;
      VK_AMD_GCN_SHADER_EXTENSION_NAME='VK_AMD_gcn_shader';
      VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION=1;
      VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME='VK_NV_dedicated_allocation';
      VK_EXT_EXTENSION_28_SPEC_VERSION=0;
      VK_EXT_EXTENSION_28_EXTENSION_NAME='VK_NV_extension_28';
      VK_NVX_EXTENSION_29_SPEC_VERSION=0;
      VK_NVX_EXTENSION_29_EXTENSION_NAME='VK_NVX_extension_29';
      VK_NVX_EXTENSION_30_SPEC_VERSION=0;
      VK_NVX_EXTENSION_30_EXTENSION_NAME='VK_NVX_extension_30';
      VK_NVX_EXTENSION_31_SPEC_VERSION=0;
      VK_NVX_EXTENSION_31_EXTENSION_NAME='VK_NVX_extension_31';
      VK_AMD_EXTENSION_32_SPEC_VERSION=0;
      VK_AMD_EXTENSION_32_EXTENSION_NAME='VK_AMD_extension_32';
      VK_AMD_EXTENSION_33_SPEC_VERSION=0;
      VK_AMD_EXTENSION_33_EXTENSION_NAME='VK_AMD_extension_33';
      VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION=1;
      VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME='VK_AMD_draw_indirect_count';
      VK_AMD_EXTENSION_35_SPEC_VERSION=0;
      VK_AMD_EXTENSION_35_EXTENSION_NAME='VK_AMD_extension_35';
      VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION=1;
      VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME='VK_AMD_negative_viewport_height';
      VK_AMD_GPU_SHADER_HALF_FLOAT_SPEC_VERSION=1;
      VK_AMD_GPU_SHADER_HALF_FLOAT_EXTENSION_NAME='VK_AMD_gpu_shader_half_float';
      VK_AMD_SHADER_BALLOT_SPEC_VERSION=1;
      VK_AMD_SHADER_BALLOT_EXTENSION_NAME='VK_AMD_shader_ballot';
      VK_AMD_EXTENSION_39_SPEC_VERSION=0;
      VK_AMD_EXTENSION_39_EXTENSION_NAME='VK_AMD_extension_39';
      VK_AMD_EXTENSION_40_SPEC_VERSION=0;
      VK_AMD_EXTENSION_40_EXTENSION_NAME='VK_AMD_extension_40';
      VK_AMD_EXTENSION_41_SPEC_VERSION=0;
      VK_AMD_EXTENSION_41_EXTENSION_NAME='VK_AMD_extension_41';
      VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION=1;
      VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME='VK_AMD_texture_gather_bias_lod';
      VK_AMD_EXTENSION_43_SPEC_VERSION=0;
      VK_AMD_EXTENSION_43_EXTENSION_NAME='VK_AMD_extension_43';
      VK_AMD_EXTENSION_44_SPEC_VERSION=0;
      VK_AMD_EXTENSION_44_EXTENSION_NAME='VK_AMD_extension_44';
      VK_AMD_EXTENSION_45_SPEC_VERSION=0;
      VK_AMD_EXTENSION_45_EXTENSION_NAME='VK_AMD_extension_45';
      VK_AMD_EXTENSION_46_SPEC_VERSION=0;
      VK_AMD_EXTENSION_46_EXTENSION_NAME='VK_AMD_extension_46';
      VK_AMD_EXTENSION_47_SPEC_VERSION=0;
      VK_AMD_EXTENSION_47_EXTENSION_NAME='VK_AMD_extension_47';
      VK_NVX_EXTENSION_48_SPEC_VERSION=0;
      VK_NVX_EXTENSION_48_EXTENSION_NAME='VK_NVX_extension_48';
      VK_GOOGLE_EXTENSION_49_SPEC_VERSION=0;
      VK_GOOGLE_EXTENSION_49_EXTENSION_NAME='VK_GOOGLE_extension_49';
      VK_GOOGLE_EXTENSION_50_SPEC_VERSION=0;
      VK_GOOGLE_EXTENSION_50_EXTENSION_NAME='VK_GOOGLE_extension_50';
      VK_NVX_EXTENSION_51_SPEC_VERSION=0;
      VK_NVX_EXTENSION_51_EXTENSION_NAME='VK_NVX_extension_51';
      VK_NVX_EXTENSION_52_SPEC_VERSION=0;
      VK_NVX_EXTENSION_52_EXTENSION_NAME='VK_NVX_extension_52';
      VK_NV_EXTENSION_53_SPEC_VERSION=0;
      VK_NV_EXTENSION_53_EXTENSION_NAME='VK_NV_extension_53';
      VK_KHX_MULTIVIEW_SPEC_VERSION=1;
      VK_KHX_MULTIVIEW_EXTENSION_NAME='VK_KHX_multiview';
      VK_IMG_FORMAT_PVRTC_SPEC_VERSION=1;
      VK_IMG_FORMAT_PVRTC_EXTENSION_NAME='VK_IMG_format_pvrtc';
      VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION=1;
      VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME='VK_NV_external_memory_capabilities';
      VK_NV_EXTERNAL_MEMORY_SPEC_VERSION=1;
      VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME='VK_NV_external_memory';
      VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION=1;
      VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME='VK_NV_external_memory_win32';
      VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION=1;
      VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME='VK_NV_win32_keyed_mutex';
      VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION=1;
      VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME='VK_KHR_get_physical_device_properties2';
      VK_KHX_DEVICE_GROUP_SPEC_VERSION=1;
      VK_KHX_DEVICE_GROUP_EXTENSION_NAME='VK_KHX_device_group';
      VK_EXT_VALIDATION_FLAGS_SPEC_VERSION=1;
      VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME='VK_EXT_validation_flags';
      VK_NN_VI_SURFACE_SPEC_VERSION=1;
      VK_NN_VI_SURFACE_EXTENSION_NAME='VK_NN_vi_surface';
      VK_KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION=1;
      VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME='VK_KHR_shader_draw_parameters';
      VK_EXT_SHADER_SUBGROUP_BALLOT_SPEC_VERSION=1;
      VK_EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME='VK_EXT_shader_subgroup_ballot';
      VK_EXT_SHADER_SUBGROUP_VOTE_SPEC_VERSION=1;
      VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME='VK_EXT_shader_subgroup_vote';
      VK_ARM_EXTENSION_01_SPEC_VERSION=0;
      VK_ARM_EXTENSION_01_EXTENSION_NAME='VK_ARM_extension_01';
      VK_ARM_EXTENSION_02_SPEC_VERSION=0;
      VK_ARM_EXTENSION_02_EXTENSION_NAME='VK_ARM_extension_02';
      VK_IMG_EXTENSION_69_SPEC_VERSION=0;
      VK_IMG_EXTENSION_69_EXTENSION_NAME='VK_IMG_extension_69';
      VK_KHR_MAINTENANCE1_SPEC_VERSION=1;
      VK_KHR_MAINTENANCE1_EXTENSION_NAME='VK_KHR_maintenance1';
      VK_KHX_DEVICE_GROUP_CREATION_SPEC_VERSION=1;
      VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME='VK_KHX_device_group_creation';
      VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION=1;
      VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME='VK_KHR_external_memory_capabilities';
      VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION=1;
      VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME='VK_KHR_external_memory';
      VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION=1;
      VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME='VK_KHR_external_memory_win32';
      VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION=1;
      VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME='VK_KHR_external_memory_fd';
      VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION=1;
      VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME='VK_KHR_win32_keyed_mutex';
      VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION=1;
      VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME='VK_KHR_external_semaphore_capabilities';
      VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION=1;
      VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME='VK_KHR_external_semaphore';
      VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION=1;
      VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME='VK_KHR_external_semaphore_win32';
      VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION=1;
      VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME='VK_KHR_external_semaphore_fd';
      VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION=1;
      VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME='VK_KHR_push_descriptor';
      VK_KHR_EXTENSION_82_SPEC_VERSION=0;
      VK_KHR_EXTENSION_82_EXTENSION_NAME='VK_KHR_extension_82';
      VK_KHR_EXTENSION_83_SPEC_VERSION=0;
      VK_KHR_EXTENSION_83_EXTENSION_NAME='VK_KHR_extension_83';
      VK_KHR_16BIT_STORAGE_SPEC_VERSION=1;
      VK_KHR_16BIT_STORAGE_EXTENSION_NAME='VK_KHR_16bit_storage';
      VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION=1;
      VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME='VK_KHR_incremental_present';
      VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION=1;
      VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME='VK_KHR_descriptor_update_template';
      VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION=1;
      VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME='VK_NVX_device_generated_commands';
      VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION=1;
      VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME='VK_NV_clip_space_w_scaling';
      VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION=1;
      VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME='VK_EXT_direct_mode_display';
      VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION=1;
      VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME='VK_EXT_acquire_xlib_display';
      VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION=1;
      VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME='VK_EXT_display_surface_counter';
      VK_EXT_DISPLAY_CONTROL_SPEC_VERSION=1;
      VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME='VK_EXT_display_control';
      VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION=1;
      VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME='VK_GOOGLE_display_timing';
      VK_KHR_EXTENSION_94_SPEC_VERSION=0;
      VK_KHR_EXTENSION_94_EXTENSION_NAME='VK_KHR_extension_94';
      VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION=1;
      VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME='VK_NV_sample_mask_override_coverage';
      VK_NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION=1;
      VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME='VK_NV_geometry_shader_passthrough';
      VK_NV_VIEWPORT_ARRAY2_SPEC_VERSION=1;
      VK_NV_VIEWPORT_ARRAY2_EXTENSION_NAME='VK_NV_viewport_array2';
      VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION=1;
      VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME='VK_NVX_multiview_per_view_attributes';
      VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION=1;
      VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME='VK_NV_viewport_swizzle';
      VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION=1;
      VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME='VK_EXT_discard_rectangles';
      VK_NV_EXTENSION_101_SPEC_VERSION=0;
      VK_NV_EXTENSION_101_EXTENSION_NAME='VK_NV_extension_101';
      VK_NV_EXTENSION_102_SPEC_VERSION=0;
      VK_NV_EXTENSION_102_EXTENSION_NAME='VK_NV_extension_102';
      VK_NV_EXTENSION_103_SPEC_VERSION=0;
      VK_NV_EXTENSION_103_EXTENSION_NAME='VK_NV_extension_103';
      VK_NV_EXTENSION_104_SPEC_VERSION=0;
      VK_NV_EXTENSION_104_EXTENSION_NAME='VK_NV_extension_104';
      VK_EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION=3;
      VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME='VK_EXT_swapchain_colorspace';
      VK_EXT_HDR_METADATA_SPEC_VERSION=1;
      VK_EXT_HDR_METADATA_EXTENSION_NAME='VK_EXT_hdr_metadata';
      VK_IMG_EXTENSION_107_SPEC_VERSION=0;
      VK_IMG_EXTENSION_107_EXTENSION_NAME='VK_IMG_extension_107';
      VK_IMG_EXTENSION_108_SPEC_VERSION=0;
      VK_IMG_EXTENSION_108_EXTENSION_NAME='VK_IMG_extension_108';
      VK_IMG_EXTENSION_109_SPEC_VERSION=0;
      VK_IMG_EXTENSION_109_EXTENSION_NAME='VK_IMG_extension_109';
      VK_IMG_EXTENSION_110_SPEC_VERSION=0;
      VK_IMG_EXTENSION_110_EXTENSION_NAME='VK_IMG_extension_110';
      VK_IMG_EXTENSION_111_SPEC_VERSION=0;
      VK_IMG_EXTENSION_111_EXTENSION_NAME='VK_IMG_extension_111';
      VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION=1;
      VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME='VK_KHR_shared_presentable_image';
      VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION=1;
      VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME='VK_KHR_external_fence_capabilities';
      VK_KHR_EXTERNAL_FENCE_SPEC_VERSION=1;
      VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME='VK_KHR_external_fence';
      VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION=1;
      VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME='VK_KHR_external_fence_win32';
      VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION=1;
      VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME='VK_KHR_external_fence_fd';
      VK_KHR_EXTENSION_117_SPEC_VERSION=0;
      VK_KHR_EXTENSION_117_EXTENSION_NAME='VK_KHR_extension_117';
      VK_KHR_EXTENSION_118_SPEC_VERSION=0;
      VK_KHR_EXTENSION_118_EXTENSION_NAME='VK_KHR_extension_118';
      VK_KHR_EXTENSION_119_SPEC_VERSION=0;
      VK_KHR_EXTENSION_119_EXTENSION_NAME='VK_KHR_extension_119';
      VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION=1;
      VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME='VK_KHR_get_surface_capabilities2';
      VK_KHR_VARIABLE_POINTERS_SPEC_VERSION=1;
      VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME='VK_KHR_variable_pointers';
      VK_KHR_EXTENSION_122_SPEC_VERSION=0;
      VK_KHR_EXTENSION_122_EXTENSION_NAME='VK_KHR_extension_122';
      VK_MVK_IOS_SURFACE_SPEC_VERSION=2;
      VK_MVK_IOS_SURFACE_EXTENSION_NAME='VK_MVK_ios_surface';
      VK_MVK_MACOS_SURFACE_SPEC_VERSION=2;
      VK_MVK_MACOS_SURFACE_EXTENSION_NAME='VK_MVK_macos_surface';
      VK_MVK_MOLTENVK_SPEC_VERSION=0;
      VK_MVK_MOLTENVK_EXTENSION_NAME='VK_MVK_moltenvk';
      VK_MESA_EXTENSION_126_SPEC_VERSION=0;
      VK_MESA_EXTENSION_126_EXTENSION_NAME='VK_MESA_extension_126';
      VK_MESA_EXTENSION_127_SPEC_VERSION=0;
      VK_MESA_EXTENSION_127_EXTENSION_NAME='VK_MESA_extension_127';
      VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION=1;
      VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME='VK_KHR_dedicated_allocation';
      VK_KHR_EXTENSION_129_SPEC_VERSION=0;
      VK_KHR_EXTENSION_129_EXTENSION_NAME='VK_EXT_extension_129';
      VK_KHR_EXTENSION_130_SPEC_VERSION=0;
      VK_KHR_EXTENSION_130_EXTENSION_NAME='VK_KHR_extension_130';
      VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION=1;
      VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME='VK_EXT_sampler_filter_minmax';
      VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION=1;
      VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME='VK_KHR_storage_buffer_storage_class';
      VK_AMD_GPU_SHADER_INT16_SPEC_VERSION=1;
      VK_AMD_GPU_SHADER_INT16_EXTENSION_NAME='VK_AMD_gpu_shader_int16';
      VK_AMD_EXTENSION_134_SPEC_VERSION=0;
      VK_AMD_EXTENSION_134_EXTENSION_NAME='VK_AMD_extension_134';
      VK_AMD_EXTENSION_135_SPEC_VERSION=0;
      VK_AMD_EXTENSION_135_EXTENSION_NAME='VK_AMD_extension_135';
      VK_AMD_EXTENSION_136_SPEC_VERSION=0;
      VK_AMD_EXTENSION_136_EXTENSION_NAME='VK_AMD_extension_136';
      VK_AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION=1;
      VK_AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME='VK_AMD_mixed_attachment_samples';
      VK_AMD_EXTENSION_138_SPEC_VERSION=0;
      VK_AMD_EXTENSION_138_EXTENSION_NAME='VK_AMD_extension_138';
      VK_AMD_EXTENSION_139_SPEC_VERSION=0;
      VK_AMD_EXTENSION_139_EXTENSION_NAME='VK_AMD_extension_139';
      VK_AMD_EXTENSION_140_SPEC_VERSION=0;
      VK_AMD_EXTENSION_140_EXTENSION_NAME='VK_AMD_extension_140';
      VK_AMD_EXTENSION_141_SPEC_VERSION=0;
      VK_AMD_EXTENSION_141_EXTENSION_NAME='VK_AMD_extension_141';
      VK_AMD_EXTENSION_142_SPEC_VERSION=0;
      VK_AMD_EXTENSION_142_EXTENSION_NAME='VK_AMD_extension_142';
      VK_AMD_EXTENSION_143_SPEC_VERSION=0;
      VK_AMD_EXTENSION_143_EXTENSION_NAME='VK_AMD_extension_143';
      VK_AMD_EXTENSION_144_SPEC_VERSION=0;
      VK_AMD_EXTENSION_144_EXTENSION_NAME='VK_AMD_extension_144';
      VK_KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION=1;
      VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME='VK_KHR_relaxed_block_layout';
      VK_KHR_extension_146_SPEC_VERSION=0;
      VK_KHR_extension_146_EXTENSION_NAME='VK_KHR_extension_146';
      VK_KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION=1;
      VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME='VK_KHR_get_memory_requirements2';
      VK_KHR_EXTENSION_148_SPEC_VERSION=0;
      VK_KHR_EXTENSION_148_EXTENSION_NAME='VK_EXT_extension_148';
      VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION=2;
      VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME='VK_EXT_blend_operation_advanced';
      VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION=1;
      VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME='VK_NV_fragment_coverage_to_color';
      VK_NV_EXTENSION_151_SPEC_VERSION=0;
      VK_NV_EXTENSION_151_EXTENSION_NAME='VK_NV_extension_151';
      VK_NV_EXTENSION_152_SPEC_VERSION=0;
      VK_NV_EXTENSION_152_EXTENSION_NAME='VK_NV_extension_152';
      VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION=1;
      VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME='VK_NV_framebuffer_mixed_samples';
      VK_NV_FILL_RECTANGLE_SPEC_VERSION=1;
      VK_NV_FILL_RECTANGLE_EXTENSION_NAME='VK_NV_fill_rectangle';
      VK_NV_EXTENSION_155_SPEC_VERSION=0;
      VK_NV_EXTENSION_155_EXTENSION_NAME='VK_NV_extension_155';
      VK_EXT_POST_DEPTH_COVERAGE_SPEC_VERSION=1;
      VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME='VK_EXT_post_depth_coverage';
      VK_KHR_EXTENSION_157_SPEC_VERSION=0;
      VK_KHR_EXTENSION_157_EXTENSION_NAME='VK_KHR_extension_157';
      VK_KHR_EXTENSION_158_SPEC_VERSION=0;
      VK_KHR_EXTENSION_158_EXTENSION_NAME='VK_KHR_extension_158';
      VK_EXT_EXTENSION_159_SPEC_VERSION=0;
      VK_EXT_EXTENSION_159_EXTENSION_NAME='VK_EXT_extension_159';
      VK_EXT_EXTENSION_160_SPEC_VERSION=0;
      VK_EXT_EXTENSION_160_EXTENSION_NAME='VK_EXT_extension_160';
      VK_EXT_EXTENSION_161_SPEC_VERSION=0;
      VK_EXT_EXTENSION_161_EXTENSION_NAME='VK_EXT_extension_161';
      VK_EXT_EXTENSION_162_SPEC_VERSION=0;
      VK_EXT_EXTENSION_162_EXTENSION_NAME='VK_EXT_extension_162';

//========== TYPES ===================
type
     TVkDispatchableHandle=TVkPtrInt;
     PPVkDispatchableHandle=^PVkDispatchableHandle;
     PVkDispatchableHandle=^TVkDispatchableHandle;

     TVkNonDispatchableHandle=TVkUInt64;
     PPVkNonDispatchableHandle=^PVkNonDispatchableHandle;
     PVkNonDispatchableHandle=^TVkNonDispatchableHandle;

     TVkEnum=TVkInt32;
     PPVkEnum=^PVkEnum;
     PVkEnum=^TVkEnum;

{$ifdef Windows}
     TVkHINSTANCE=TVkPtrUInt;
     PPVkHINSTANCE=^PVkHINSTANCE;
     PVkHINSTANCE=^TVkHINSTANCE;

     TVkHWND=HWND;
     PPVkHWND=^PVkHWND;
     PVkHWND=^TVkHWND;
{$endif}

     TVkSampleMask=TVkUInt32;
     PPVkSampleMask=^PVkSampleMask;
     PVkSampleMask=^TVkSampleMask;

     TVkBool32=TVkUInt32;
     PPVkBool32=^PVkBool32;
     PVkBool32=^TVkBool32;

     TVkFlags=TVkUInt32;
     PPVkFlags=^PVkFlags;
     PVkFlags=^TVkFlags;

     TVkDeviceSize=TVkUInt64;
     PPVkDeviceSize=^PVkDeviceSize;
     PVkDeviceSize=^TVkDeviceSize;

     TVkFramebufferCreateFlags=TVkFlags;
     PPVkFramebufferCreateFlags=^PVkFramebufferCreateFlags;
     PVkFramebufferCreateFlags=^TVkFramebufferCreateFlags;

     TVkQueryPoolCreateFlags=TVkFlags;
     PPVkQueryPoolCreateFlags=^PVkQueryPoolCreateFlags;
     PVkQueryPoolCreateFlags=^TVkQueryPoolCreateFlags;

     TVkRenderPassCreateFlags=TVkFlags;
     PPVkRenderPassCreateFlags=^PVkRenderPassCreateFlags;
     PVkRenderPassCreateFlags=^TVkRenderPassCreateFlags;

     TVkSamplerCreateFlags=TVkFlags;
     PPVkSamplerCreateFlags=^PVkSamplerCreateFlags;
     PVkSamplerCreateFlags=^TVkSamplerCreateFlags;

     TVkPipelineLayoutCreateFlags=TVkFlags;
     PPVkPipelineLayoutCreateFlags=^PVkPipelineLayoutCreateFlags;
     PVkPipelineLayoutCreateFlags=^TVkPipelineLayoutCreateFlags;

     TVkPipelineCacheCreateFlags=TVkFlags;
     PPVkPipelineCacheCreateFlags=^PVkPipelineCacheCreateFlags;
     PVkPipelineCacheCreateFlags=^TVkPipelineCacheCreateFlags;

     TVkPipelineDepthStencilStateCreateFlags=TVkFlags;
     PPVkPipelineDepthStencilStateCreateFlags=^PVkPipelineDepthStencilStateCreateFlags;
     PVkPipelineDepthStencilStateCreateFlags=^TVkPipelineDepthStencilStateCreateFlags;

     TVkPipelineDynamicStateCreateFlags=TVkFlags;
     PPVkPipelineDynamicStateCreateFlags=^PVkPipelineDynamicStateCreateFlags;
     PVkPipelineDynamicStateCreateFlags=^TVkPipelineDynamicStateCreateFlags;

     TVkPipelineColorBlendStateCreateFlags=TVkFlags;
     PPVkPipelineColorBlendStateCreateFlags=^PVkPipelineColorBlendStateCreateFlags;
     PVkPipelineColorBlendStateCreateFlags=^TVkPipelineColorBlendStateCreateFlags;

     TVkPipelineMultisampleStateCreateFlags=TVkFlags;
     PPVkPipelineMultisampleStateCreateFlags=^PVkPipelineMultisampleStateCreateFlags;
     PVkPipelineMultisampleStateCreateFlags=^TVkPipelineMultisampleStateCreateFlags;

     TVkPipelineRasterizationStateCreateFlags=TVkFlags;
     PPVkPipelineRasterizationStateCreateFlags=^PVkPipelineRasterizationStateCreateFlags;
     PVkPipelineRasterizationStateCreateFlags=^TVkPipelineRasterizationStateCreateFlags;

     TVkPipelineViewportStateCreateFlags=TVkFlags;
     PPVkPipelineViewportStateCreateFlags=^PVkPipelineViewportStateCreateFlags;
     PVkPipelineViewportStateCreateFlags=^TVkPipelineViewportStateCreateFlags;

     TVkPipelineTessellationStateCreateFlags=TVkFlags;
     PPVkPipelineTessellationStateCreateFlags=^PVkPipelineTessellationStateCreateFlags;
     PVkPipelineTessellationStateCreateFlags=^TVkPipelineTessellationStateCreateFlags;

     TVkPipelineInputAssemblyStateCreateFlags=TVkFlags;
     PPVkPipelineInputAssemblyStateCreateFlags=^PVkPipelineInputAssemblyStateCreateFlags;
     PVkPipelineInputAssemblyStateCreateFlags=^TVkPipelineInputAssemblyStateCreateFlags;

     TVkPipelineVertexInputStateCreateFlags=TVkFlags;
     PPVkPipelineVertexInputStateCreateFlags=^PVkPipelineVertexInputStateCreateFlags;
     PVkPipelineVertexInputStateCreateFlags=^TVkPipelineVertexInputStateCreateFlags;

     TVkPipelineShaderStageCreateFlags=TVkFlags;
     PPVkPipelineShaderStageCreateFlags=^PVkPipelineShaderStageCreateFlags;
     PVkPipelineShaderStageCreateFlags=^TVkPipelineShaderStageCreateFlags;

     TVkBufferViewCreateFlags=TVkFlags;
     PPVkBufferViewCreateFlags=^PVkBufferViewCreateFlags;
     PVkBufferViewCreateFlags=^TVkBufferViewCreateFlags;

     TVkInstanceCreateFlags=TVkFlags;
     PPVkInstanceCreateFlags=^PVkInstanceCreateFlags;
     PVkInstanceCreateFlags=^TVkInstanceCreateFlags;

     TVkDeviceCreateFlags=TVkFlags;
     PPVkDeviceCreateFlags=^PVkDeviceCreateFlags;
     PVkDeviceCreateFlags=^TVkDeviceCreateFlags;

     TVkDeviceQueueCreateFlags=TVkFlags;
     PPVkDeviceQueueCreateFlags=^PVkDeviceQueueCreateFlags;
     PVkDeviceQueueCreateFlags=^TVkDeviceQueueCreateFlags;

     TVkImageViewCreateFlags=TVkFlags;
     PPVkImageViewCreateFlags=^PVkImageViewCreateFlags;
     PVkImageViewCreateFlags=^TVkImageViewCreateFlags;

     TVkSemaphoreCreateFlags=TVkFlags;
     PPVkSemaphoreCreateFlags=^PVkSemaphoreCreateFlags;
     PVkSemaphoreCreateFlags=^TVkSemaphoreCreateFlags;

     TVkShaderModuleCreateFlags=TVkFlags;
     PPVkShaderModuleCreateFlags=^PVkShaderModuleCreateFlags;
     PVkShaderModuleCreateFlags=^TVkShaderModuleCreateFlags;

     TVkEventCreateFlags=TVkFlags;
     PPVkEventCreateFlags=^PVkEventCreateFlags;
     PVkEventCreateFlags=^TVkEventCreateFlags;

     TVkMemoryMapFlags=TVkFlags;
     PPVkMemoryMapFlags=^PVkMemoryMapFlags;
     PVkMemoryMapFlags=^TVkMemoryMapFlags;

     TVkDescriptorPoolResetFlags=TVkFlags;
     PPVkDescriptorPoolResetFlags=^PVkDescriptorPoolResetFlags;
     PVkDescriptorPoolResetFlags=^TVkDescriptorPoolResetFlags;

     TVkDescriptorUpdateTemplateCreateFlagsKHR=TVkFlags;
     PPVkDescriptorUpdateTemplateCreateFlagsKHR=^PVkDescriptorUpdateTemplateCreateFlagsKHR;
     PVkDescriptorUpdateTemplateCreateFlagsKHR=^TVkDescriptorUpdateTemplateCreateFlagsKHR;

     TVkDisplayModeCreateFlagsKHR=TVkFlags;
     PPVkDisplayModeCreateFlagsKHR=^PVkDisplayModeCreateFlagsKHR;
     PVkDisplayModeCreateFlagsKHR=^TVkDisplayModeCreateFlagsKHR;

     TVkDisplaySurfaceCreateFlagsKHR=TVkFlags;
     PPVkDisplaySurfaceCreateFlagsKHR=^PVkDisplaySurfaceCreateFlagsKHR;
     PVkDisplaySurfaceCreateFlagsKHR=^TVkDisplaySurfaceCreateFlagsKHR;

     TVkAndroidSurfaceCreateFlagsKHR=TVkFlags;
     PPVkAndroidSurfaceCreateFlagsKHR=^PVkAndroidSurfaceCreateFlagsKHR;
     PVkAndroidSurfaceCreateFlagsKHR=^TVkAndroidSurfaceCreateFlagsKHR;

     TVkMirSurfaceCreateFlagsKHR=TVkFlags;
     PPVkMirSurfaceCreateFlagsKHR=^PVkMirSurfaceCreateFlagsKHR;
     PVkMirSurfaceCreateFlagsKHR=^TVkMirSurfaceCreateFlagsKHR;

     TVkViSurfaceCreateFlagsNN=TVkFlags;
     PPVkViSurfaceCreateFlagsNN=^PVkViSurfaceCreateFlagsNN;
     PVkViSurfaceCreateFlagsNN=^TVkViSurfaceCreateFlagsNN;

     TVkWaylandSurfaceCreateFlagsKHR=TVkFlags;
     PPVkWaylandSurfaceCreateFlagsKHR=^PVkWaylandSurfaceCreateFlagsKHR;
     PVkWaylandSurfaceCreateFlagsKHR=^TVkWaylandSurfaceCreateFlagsKHR;

     TVkWin32SurfaceCreateFlagsKHR=TVkFlags;
     PPVkWin32SurfaceCreateFlagsKHR=^PVkWin32SurfaceCreateFlagsKHR;
     PVkWin32SurfaceCreateFlagsKHR=^TVkWin32SurfaceCreateFlagsKHR;

     TVkXlibSurfaceCreateFlagsKHR=TVkFlags;
     PPVkXlibSurfaceCreateFlagsKHR=^PVkXlibSurfaceCreateFlagsKHR;
     PVkXlibSurfaceCreateFlagsKHR=^TVkXlibSurfaceCreateFlagsKHR;

     TVkXcbSurfaceCreateFlagsKHR=TVkFlags;
     PPVkXcbSurfaceCreateFlagsKHR=^PVkXcbSurfaceCreateFlagsKHR;
     PVkXcbSurfaceCreateFlagsKHR=^TVkXcbSurfaceCreateFlagsKHR;

     TVkIOSSurfaceCreateFlagsMVK=TVkFlags;
     PPVkIOSSurfaceCreateFlagsMVK=^PVkIOSSurfaceCreateFlagsMVK;
     PVkIOSSurfaceCreateFlagsMVK=^TVkIOSSurfaceCreateFlagsMVK;

     TVkMacOSSurfaceCreateFlagsMVK=TVkFlags;
     PPVkMacOSSurfaceCreateFlagsMVK=^PVkMacOSSurfaceCreateFlagsMVK;
     PVkMacOSSurfaceCreateFlagsMVK=^TVkMacOSSurfaceCreateFlagsMVK;

     TVkCommandPoolTrimFlagsKHR=TVkFlags;
     PPVkCommandPoolTrimFlagsKHR=^PVkCommandPoolTrimFlagsKHR;
     PVkCommandPoolTrimFlagsKHR=^TVkCommandPoolTrimFlagsKHR;

     TVkPipelineViewportSwizzleStateCreateFlagsNV=TVkFlags;
     PPVkPipelineViewportSwizzleStateCreateFlagsNV=^PVkPipelineViewportSwizzleStateCreateFlagsNV;
     PVkPipelineViewportSwizzleStateCreateFlagsNV=^TVkPipelineViewportSwizzleStateCreateFlagsNV;

     TVkPipelineDiscardRectangleStateCreateFlagsEXT=TVkFlags;
     PPVkPipelineDiscardRectangleStateCreateFlagsEXT=^PVkPipelineDiscardRectangleStateCreateFlagsEXT;
     PVkPipelineDiscardRectangleStateCreateFlagsEXT=^TVkPipelineDiscardRectangleStateCreateFlagsEXT;

     TVkPipelineCoverageToColorStateCreateFlagsNV=TVkFlags;
     PPVkPipelineCoverageToColorStateCreateFlagsNV=^PVkPipelineCoverageToColorStateCreateFlagsNV;
     PVkPipelineCoverageToColorStateCreateFlagsNV=^TVkPipelineCoverageToColorStateCreateFlagsNV;

     TVkPipelineCoverageModulationStateCreateFlagsNV=TVkFlags;
     PPVkPipelineCoverageModulationStateCreateFlagsNV=^PVkPipelineCoverageModulationStateCreateFlagsNV;
     PVkPipelineCoverageModulationStateCreateFlagsNV=^TVkPipelineCoverageModulationStateCreateFlagsNV;

     TVkInstance=TVkDispatchableHandle;
     PPVkInstance=^PVkInstance;
     PVkInstance=^TVkInstance;

     TVkPhysicalDevice=TVkDispatchableHandle;
     PPVkPhysicalDevice=^PVkPhysicalDevice;
     PVkPhysicalDevice=^TVkPhysicalDevice;

     TVkDevice=TVkDispatchableHandle;
     PPVkDevice=^PVkDevice;
     PVkDevice=^TVkDevice;

     TVkQueue=TVkDispatchableHandle;
     PPVkQueue=^PVkQueue;
     PVkQueue=^TVkQueue;

     TVkCommandBuffer=TVkDispatchableHandle;
     PPVkCommandBuffer=^PVkCommandBuffer;
     PVkCommandBuffer=^TVkCommandBuffer;

     TVkDeviceMemory=TVkNonDispatchableHandle;
     PPVkDeviceMemory=^PVkDeviceMemory;
     PVkDeviceMemory=^TVkDeviceMemory;

     TVkCommandPool=TVkNonDispatchableHandle;
     PPVkCommandPool=^PVkCommandPool;
     PVkCommandPool=^TVkCommandPool;

     TVkBuffer=TVkNonDispatchableHandle;
     PPVkBuffer=^PVkBuffer;
     PVkBuffer=^TVkBuffer;

     TVkBufferView=TVkNonDispatchableHandle;
     PPVkBufferView=^PVkBufferView;
     PVkBufferView=^TVkBufferView;

     TVkImage=TVkNonDispatchableHandle;
     PPVkImage=^PVkImage;
     PVkImage=^TVkImage;

     TVkImageView=TVkNonDispatchableHandle;
     PPVkImageView=^PVkImageView;
     PVkImageView=^TVkImageView;

     TVkShaderModule=TVkNonDispatchableHandle;
     PPVkShaderModule=^PVkShaderModule;
     PVkShaderModule=^TVkShaderModule;

     TVkPipeline=TVkNonDispatchableHandle;
     PPVkPipeline=^PVkPipeline;
     PVkPipeline=^TVkPipeline;

     TVkPipelineLayout=TVkNonDispatchableHandle;
     PPVkPipelineLayout=^PVkPipelineLayout;
     PVkPipelineLayout=^TVkPipelineLayout;

     TVkSampler=TVkNonDispatchableHandle;
     PPVkSampler=^PVkSampler;
     PVkSampler=^TVkSampler;

     TVkDescriptorSet=TVkNonDispatchableHandle;
     PPVkDescriptorSet=^PVkDescriptorSet;
     PVkDescriptorSet=^TVkDescriptorSet;

     TVkDescriptorSetLayout=TVkNonDispatchableHandle;
     PPVkDescriptorSetLayout=^PVkDescriptorSetLayout;
     PVkDescriptorSetLayout=^TVkDescriptorSetLayout;

     TVkDescriptorPool=TVkNonDispatchableHandle;
     PPVkDescriptorPool=^PVkDescriptorPool;
     PVkDescriptorPool=^TVkDescriptorPool;

     TVkFence=TVkNonDispatchableHandle;
     PPVkFence=^PVkFence;
     PVkFence=^TVkFence;

     TVkSemaphore=TVkNonDispatchableHandle;
     PPVkSemaphore=^PVkSemaphore;
     PVkSemaphore=^TVkSemaphore;

     TVkEvent=TVkNonDispatchableHandle;
     PPVkEvent=^PVkEvent;
     PVkEvent=^TVkEvent;

     TVkQueryPool=TVkNonDispatchableHandle;
     PPVkQueryPool=^PVkQueryPool;
     PVkQueryPool=^TVkQueryPool;

     TVkFramebuffer=TVkNonDispatchableHandle;
     PPVkFramebuffer=^PVkFramebuffer;
     PVkFramebuffer=^TVkFramebuffer;

     TVkRenderPass=TVkNonDispatchableHandle;
     PPVkRenderPass=^PVkRenderPass;
     PVkRenderPass=^TVkRenderPass;

     TVkPipelineCache=TVkNonDispatchableHandle;
     PPVkPipelineCache=^PVkPipelineCache;
     PVkPipelineCache=^TVkPipelineCache;

     TVkObjectTableNVX=TVkNonDispatchableHandle;
     PPVkObjectTableNVX=^PVkObjectTableNVX;
     PVkObjectTableNVX=^TVkObjectTableNVX;

     TVkIndirectCommandsLayoutNVX=TVkNonDispatchableHandle;
     PPVkIndirectCommandsLayoutNVX=^PVkIndirectCommandsLayoutNVX;
     PVkIndirectCommandsLayoutNVX=^TVkIndirectCommandsLayoutNVX;

     TVkDescriptorUpdateTemplateKHR=TVkNonDispatchableHandle;
     PPVkDescriptorUpdateTemplateKHR=^PVkDescriptorUpdateTemplateKHR;
     PVkDescriptorUpdateTemplateKHR=^TVkDescriptorUpdateTemplateKHR;

     TVkDisplayKHR=TVkNonDispatchableHandle;
     PPVkDisplayKHR=^PVkDisplayKHR;
     PVkDisplayKHR=^TVkDisplayKHR;

     TVkDisplayModeKHR=TVkNonDispatchableHandle;
     PPVkDisplayModeKHR=^PVkDisplayModeKHR;
     PVkDisplayModeKHR=^TVkDisplayModeKHR;

     TVkSurfaceKHR=TVkNonDispatchableHandle;
     PPVkSurfaceKHR=^PVkSurfaceKHR;
     PVkSurfaceKHR=^TVkSurfaceKHR;

     TVkSwapchainKHR=TVkNonDispatchableHandle;
     PPVkSwapchainKHR=^PVkSwapchainKHR;
     PVkSwapchainKHR=^TVkSwapchainKHR;

     TVkDebugReportCallbackEXT=TVkNonDispatchableHandle;
     PPVkDebugReportCallbackEXT=^PVkDebugReportCallbackEXT;
     PVkDebugReportCallbackEXT=^TVkDebugReportCallbackEXT;

     TVkImageLayout=
      (
       VK_IMAGE_LAYOUT_UNDEFINED=0,                                              // Implicit layout an image is when its contents are undefined due to various reasons (e.g. right after creation)
       VK_IMAGE_LAYOUT_GENERAL=1,                                                // General layout when image can be used for any kind of access
       VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL=2,                               // Optimal layout when image is only used for color attachment read/write
       VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL=3,                       // Optimal layout when image is only used for depth/stencil attachment read/write
       VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL=4,                        // Optimal layout when image is used for read only depth/stencil attachment and shader access
       VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL=5,                               // Optimal layout when image is used for read only shader access
       VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL=6,                                   // Optimal layout when image is used only as source of transfer operations
       VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL=7,                                   // Optimal layout when image is used only as destination of transfer operations
       VK_IMAGE_LAYOUT_PREINITIALIZED=8,                                         // Initial layout used when the data is populated by the CPU
       VK_IMAGE_LAYOUT_PRESENT_SRC_KHR=1000001002,
       VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR=1000111000
      );
     PPVkImageLayout=^PVkImageLayout;
     PVkImageLayout=^TVkImageLayout;

     TVkAttachmentLoadOp=
      (
       VK_ATTACHMENT_LOAD_OP_LOAD=0,
       VK_ATTACHMENT_LOAD_OP_CLEAR=1,
       VK_ATTACHMENT_LOAD_OP_DONT_CARE=2
      );
     PPVkAttachmentLoadOp=^PVkAttachmentLoadOp;
     PVkAttachmentLoadOp=^TVkAttachmentLoadOp;

     TVkAttachmentStoreOp=
      (
       VK_ATTACHMENT_STORE_OP_STORE=0,
       VK_ATTACHMENT_STORE_OP_DONT_CARE=1
      );
     PPVkAttachmentStoreOp=^PVkAttachmentStoreOp;
     PVkAttachmentStoreOp=^TVkAttachmentStoreOp;

     TVkImageType=
      (
       VK_IMAGE_TYPE_1D=0,
       VK_IMAGE_TYPE_2D=1,
       VK_IMAGE_TYPE_3D=2
      );
     PPVkImageType=^PVkImageType;
     PVkImageType=^TVkImageType;

     TVkImageTiling=
      (
       VK_IMAGE_TILING_OPTIMAL=0,
       VK_IMAGE_TILING_LINEAR=1
      );
     PPVkImageTiling=^PVkImageTiling;
     PVkImageTiling=^TVkImageTiling;

     TVkImageViewType=
      (
       VK_IMAGE_VIEW_TYPE_1D=0,
       VK_IMAGE_VIEW_TYPE_2D=1,
       VK_IMAGE_VIEW_TYPE_3D=2,
       VK_IMAGE_VIEW_TYPE_CUBE=3,
       VK_IMAGE_VIEW_TYPE_1D_ARRAY=4,
       VK_IMAGE_VIEW_TYPE_2D_ARRAY=5,
       VK_IMAGE_VIEW_TYPE_CUBE_ARRAY=6
      );
     PPVkImageViewType=^PVkImageViewType;
     PVkImageViewType=^TVkImageViewType;

     TVkCommandBufferLevel=
      (
       VK_COMMAND_BUFFER_LEVEL_PRIMARY=0,
       VK_COMMAND_BUFFER_LEVEL_SECONDARY=1
      );
     PPVkCommandBufferLevel=^PVkCommandBufferLevel;
     PVkCommandBufferLevel=^TVkCommandBufferLevel;

     TVkComponentSwizzle=
      (
       VK_COMPONENT_SWIZZLE_IDENTITY=0,
       VK_COMPONENT_SWIZZLE_ZERO=1,
       VK_COMPONENT_SWIZZLE_ONE=2,
       VK_COMPONENT_SWIZZLE_R=3,
       VK_COMPONENT_SWIZZLE_G=4,
       VK_COMPONENT_SWIZZLE_B=5,
       VK_COMPONENT_SWIZZLE_A=6
      );
     PPVkComponentSwizzle=^PVkComponentSwizzle;
     PVkComponentSwizzle=^TVkComponentSwizzle;

     TVkDescriptorType=
      (
       VK_DESCRIPTOR_TYPE_SAMPLER=0,
       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER=1,
       VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE=2,
       VK_DESCRIPTOR_TYPE_STORAGE_IMAGE=3,
       VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER=4,
       VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER=5,
       VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER=6,
       VK_DESCRIPTOR_TYPE_STORAGE_BUFFER=7,
       VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC=8,
       VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC=9,
       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT=10
      );
     PPVkDescriptorType=^PVkDescriptorType;
     PVkDescriptorType=^TVkDescriptorType;

     TVkQueryType=
      (
       VK_QUERY_TYPE_OCCLUSION=0,
       VK_QUERY_TYPE_PIPELINE_STATISTICS=1,                                      // Optional
       VK_QUERY_TYPE_TIMESTAMP=2
      );
     PPVkQueryType=^PVkQueryType;
     PVkQueryType=^TVkQueryType;

     TVkBorderColor=
      (
       VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK=0,
       VK_BORDER_COLOR_INT_TRANSPARENT_BLACK=1,
       VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK=2,
       VK_BORDER_COLOR_INT_OPAQUE_BLACK=3,
       VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE=4,
       VK_BORDER_COLOR_INT_OPAQUE_WHITE=5
      );
     PPVkBorderColor=^PVkBorderColor;
     PVkBorderColor=^TVkBorderColor;

     TVkPipelineBindPoint=
      (
       VK_PIPELINE_BIND_POINT_GRAPHICS=0,
       VK_PIPELINE_BIND_POINT_COMPUTE=1
      );
     PPVkPipelineBindPoint=^PVkPipelineBindPoint;
     PVkPipelineBindPoint=^TVkPipelineBindPoint;

     TVkPipelineCacheHeaderVersion=
      (
       VK_PIPELINE_CACHE_HEADER_VERSION_ONE=1
      );
     PPVkPipelineCacheHeaderVersion=^PVkPipelineCacheHeaderVersion;
     PVkPipelineCacheHeaderVersion=^TVkPipelineCacheHeaderVersion;

     TVkPrimitiveTopology=
      (
       VK_PRIMITIVE_TOPOLOGY_POINT_LIST=0,
       VK_PRIMITIVE_TOPOLOGY_LINE_LIST=1,
       VK_PRIMITIVE_TOPOLOGY_LINE_STRIP=2,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST=3,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP=4,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN=5,
       VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY=6,
       VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY=7,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY=8,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY=9,
       VK_PRIMITIVE_TOPOLOGY_PATCH_LIST=10
      );
     PPVkPrimitiveTopology=^PVkPrimitiveTopology;
     PVkPrimitiveTopology=^TVkPrimitiveTopology;

     TVkSharingMode=
      (
       VK_SHARING_MODE_EXCLUSIVE=0,
       VK_SHARING_MODE_CONCURRENT=1
      );
     PPVkSharingMode=^PVkSharingMode;
     PVkSharingMode=^TVkSharingMode;

     TVkIndexType=
      (
       VK_INDEX_TYPE_UINT16=0,
       VK_INDEX_TYPE_UINT32=1
      );
     PPVkIndexType=^PVkIndexType;
     PVkIndexType=^TVkIndexType;

     TVkFilter=
      (
       VK_FILTER_NEAREST=0,
       VK_FILTER_LINEAR=1,
       VK_FILTER_CUBIC_IMG=1000015000
      );
     PPVkFilter=^PVkFilter;
     PVkFilter=^TVkFilter;

     TVkSamplerMipmapMode=
      (
       VK_SAMPLER_MIPMAP_MODE_NEAREST=0,                                         // Choose nearest mip level
       VK_SAMPLER_MIPMAP_MODE_LINEAR=1                                           // Linear filter between mip levels
      );
     PPVkSamplerMipmapMode=^PVkSamplerMipmapMode;
     PVkSamplerMipmapMode=^TVkSamplerMipmapMode;

     TVkSamplerAddressMode=
      (
       VK_SAMPLER_ADDRESS_MODE_REPEAT=0,
       VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT=1,
       VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE=2,
       VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER=3,
       VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE=4
      );
     PPVkSamplerAddressMode=^PVkSamplerAddressMode;
     PVkSamplerAddressMode=^TVkSamplerAddressMode;

     TVkCompareOp=
      (
       VK_COMPARE_OP_NEVER=0,
       VK_COMPARE_OP_LESS=1,
       VK_COMPARE_OP_EQUAL=2,
       VK_COMPARE_OP_LESS_OR_EQUAL=3,
       VK_COMPARE_OP_GREATER=4,
       VK_COMPARE_OP_NOT_EQUAL=5,
       VK_COMPARE_OP_GREATER_OR_EQUAL=6,
       VK_COMPARE_OP_ALWAYS=7
      );
     PPVkCompareOp=^PVkCompareOp;
     PVkCompareOp=^TVkCompareOp;

     TVkPolygonMode=
      (
       VK_POLYGON_MODE_FILL=0,
       VK_POLYGON_MODE_LINE=1,
       VK_POLYGON_MODE_POINT=2,
       VK_POLYGON_MODE_FILL_RECTANGLE_NV=1000153000
      );
     PPVkPolygonMode=^PVkPolygonMode;
     PVkPolygonMode=^TVkPolygonMode;

     VkCullModeFlagBits_=
      (
       VK_CULL_MODE_NONE=0,
       VK_CULL_MODE_FRONT_BIT=1,
       VK_CULL_MODE_BACK_BIT=2,
       VK_CULL_MODE_FRONT_AND_BACK=3
      );
     TVkCullModeFlagBit=VkCullModeFlagBits_;
     PPVkCullModeFlagBit=^PVkCullModeFlagBit;
     PVkCullModeFlagBit=^TVkCullModeFlagBit;

     TVkCullModeFlagBits= Set of TVkCullModeFlagBit; // This is SET OF
     PPVkCullModeFlagBits=^PVkCullModeFlagBits;
     PVkCullModeFlagBits=^TVkCullModeFlagBits;

     TVkFrontFace=
      (
       VK_FRONT_FACE_COUNTER_CLOCKWISE=0,
       VK_FRONT_FACE_CLOCKWISE=1
      );
     PPVkFrontFace=^PVkFrontFace;
     PVkFrontFace=^TVkFrontFace;

     TVkBlendFactor=
      (
       VK_BLEND_FACTOR_ZERO=0,
       VK_BLEND_FACTOR_ONE=1,
       VK_BLEND_FACTOR_SRC_COLOR=2,
       VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR=3,
       VK_BLEND_FACTOR_DST_COLOR=4,
       VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR=5,
       VK_BLEND_FACTOR_SRC_ALPHA=6,
       VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA=7,
       VK_BLEND_FACTOR_DST_ALPHA=8,
       VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA=9,
       VK_BLEND_FACTOR_CONSTANT_COLOR=10,
       VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR=11,
       VK_BLEND_FACTOR_CONSTANT_ALPHA=12,
       VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA=13,
       VK_BLEND_FACTOR_SRC_ALPHA_SATURATE=14,
       VK_BLEND_FACTOR_SRC1_COLOR=15,
       VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR=16,
       VK_BLEND_FACTOR_SRC1_ALPHA=17,
       VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA=18
      );
     PPVkBlendFactor=^PVkBlendFactor;
     PVkBlendFactor=^TVkBlendFactor;

     TVkBlendOp=
      (
       VK_BLEND_OP_ADD=0,
       VK_BLEND_OP_SUBTRACT=1,
       VK_BLEND_OP_REVERSE_SUBTRACT=2,
       VK_BLEND_OP_MIN=3,
       VK_BLEND_OP_MAX=4,
       VK_BLEND_OP_ZERO_EXT=1000148000,
       VK_BLEND_OP_SRC_EXT=1000148001,
       VK_BLEND_OP_DST_EXT=1000148002,
       VK_BLEND_OP_SRC_OVER_EXT=1000148003,
       VK_BLEND_OP_DST_OVER_EXT=1000148004,
       VK_BLEND_OP_SRC_IN_EXT=1000148005,
       VK_BLEND_OP_DST_IN_EXT=1000148006,
       VK_BLEND_OP_SRC_OUT_EXT=1000148007,
       VK_BLEND_OP_DST_OUT_EXT=1000148008,
       VK_BLEND_OP_SRC_ATOP_EXT=1000148009,
       VK_BLEND_OP_DST_ATOP_EXT=1000148010,
       VK_BLEND_OP_XOR_EXT=1000148011,
       VK_BLEND_OP_MULTIPLY_EXT=1000148012,
       VK_BLEND_OP_SCREEN_EXT=1000148013,
       VK_BLEND_OP_OVERLAY_EXT=1000148014,
       VK_BLEND_OP_DARKEN_EXT=1000148015,
       VK_BLEND_OP_LIGHTEN_EXT=1000148016,
       VK_BLEND_OP_COLORDODGE_EXT=1000148017,
       VK_BLEND_OP_COLORBURN_EXT=1000148018,
       VK_BLEND_OP_HARDLIGHT_EXT=1000148019,
       VK_BLEND_OP_SOFTLIGHT_EXT=1000148020,
       VK_BLEND_OP_DIFFERENCE_EXT=1000148021,
       VK_BLEND_OP_EXCLUSION_EXT=1000148022,
       VK_BLEND_OP_INVERT_EXT=1000148023,
       VK_BLEND_OP_INVERT_RGB_EXT=1000148024,
       VK_BLEND_OP_LINEARDODGE_EXT=1000148025,
       VK_BLEND_OP_LINEARBURN_EXT=1000148026,
       VK_BLEND_OP_VIVIDLIGHT_EXT=1000148027,
       VK_BLEND_OP_LINEARLIGHT_EXT=1000148028,
       VK_BLEND_OP_PINLIGHT_EXT=1000148029,
       VK_BLEND_OP_HARDMIX_EXT=1000148030,
       VK_BLEND_OP_HSL_HUE_EXT=1000148031,
       VK_BLEND_OP_HSL_SATURATION_EXT=1000148032,
       VK_BLEND_OP_HSL_COLOR_EXT=1000148033,
       VK_BLEND_OP_HSL_LUMINOSITY_EXT=1000148034,
       VK_BLEND_OP_PLUS_EXT=1000148035,
       VK_BLEND_OP_PLUS_CLAMPED_EXT=1000148036,
       VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT=1000148037,
       VK_BLEND_OP_PLUS_DARKER_EXT=1000148038,
       VK_BLEND_OP_MINUS_EXT=1000148039,
       VK_BLEND_OP_MINUS_CLAMPED_EXT=1000148040,
       VK_BLEND_OP_CONTRAST_EXT=1000148041,
       VK_BLEND_OP_INVERT_OVG_EXT=1000148042,
       VK_BLEND_OP_RED_EXT=1000148043,
       VK_BLEND_OP_GREEN_EXT=1000148044,
       VK_BLEND_OP_BLUE_EXT=1000148045
      );
     PPVkBlendOp=^PVkBlendOp;
     PVkBlendOp=^TVkBlendOp;

     TVkStencilOp=
      (
       VK_STENCIL_OP_KEEP=0,
       VK_STENCIL_OP_ZERO=1,
       VK_STENCIL_OP_REPLACE=2,
       VK_STENCIL_OP_INCREMENT_AND_CLAMP=3,
       VK_STENCIL_OP_DECREMENT_AND_CLAMP=4,
       VK_STENCIL_OP_INVERT=5,
       VK_STENCIL_OP_INCREMENT_AND_WRAP=6,
       VK_STENCIL_OP_DECREMENT_AND_WRAP=7
      );
     PPVkStencilOp=^PVkStencilOp;
     PVkStencilOp=^TVkStencilOp;

     TVkLogicOp=
      (
       VK_LOGIC_OP_CLEAR=0,
       VK_LOGIC_OP_AND=1,
       VK_LOGIC_OP_AND_REVERSE=2,
       VK_LOGIC_OP_COPY=3,
       VK_LOGIC_OP_AND_INVERTED=4,
       VK_LOGIC_OP_NO_OP=5,
       VK_LOGIC_OP_XOR=6,
       VK_LOGIC_OP_OR=7,
       VK_LOGIC_OP_NOR=8,
       VK_LOGIC_OP_EQUIVALENT=9,
       VK_LOGIC_OP_INVERT=10,
       VK_LOGIC_OP_OR_REVERSE=11,
       VK_LOGIC_OP_COPY_INVERTED=12,
       VK_LOGIC_OP_OR_INVERTED=13,
       VK_LOGIC_OP_NAND=14,
       VK_LOGIC_OP_SET=15
      );
     PPVkLogicOp=^PVkLogicOp;
     PVkLogicOp=^TVkLogicOp;

     TVkInternalAllocationType=
      (
       VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE=0
      );
     PPVkInternalAllocationType=^PVkInternalAllocationType;
     PVkInternalAllocationType=^TVkInternalAllocationType;

     TVkSystemAllocationScope=
      (
       VK_SYSTEM_ALLOCATION_SCOPE_COMMAND=0,
       VK_SYSTEM_ALLOCATION_SCOPE_OBJECT=1,
       VK_SYSTEM_ALLOCATION_SCOPE_CACHE=2,
       VK_SYSTEM_ALLOCATION_SCOPE_DEVICE=3,
       VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE=4
      );
     PPVkSystemAllocationScope=^PVkSystemAllocationScope;
     PVkSystemAllocationScope=^TVkSystemAllocationScope;

     TVkPhysicalDeviceType=
      (
       VK_PHYSICAL_DEVICE_TYPE_OTHER=0,
       VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU=1,
       VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU=2,
       VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU=3,
       VK_PHYSICAL_DEVICE_TYPE_CPU=4
      );
     PPVkPhysicalDeviceType=^PVkPhysicalDeviceType;
     PVkPhysicalDeviceType=^TVkPhysicalDeviceType;

     TVkVertexInputRate=
      (
       VK_VERTEX_INPUT_RATE_VERTEX=0,
       VK_VERTEX_INPUT_RATE_INSTANCE=1
      );
     PPVkVertexInputRate=^PVkVertexInputRate;
     PVkVertexInputRate=^TVkVertexInputRate;

     TVkFormat=
      (
       VK_FORMAT_UNDEFINED=0,
       VK_FORMAT_R4G4_UNORM_PACK8=1,
       VK_FORMAT_R4G4B4A4_UNORM_PACK16=2,
       VK_FORMAT_B4G4R4A4_UNORM_PACK16=3,
       VK_FORMAT_R5G6B5_UNORM_PACK16=4,
       VK_FORMAT_B5G6R5_UNORM_PACK16=5,
       VK_FORMAT_R5G5B5A1_UNORM_PACK16=6,
       VK_FORMAT_B5G5R5A1_UNORM_PACK16=7,
       VK_FORMAT_A1R5G5B5_UNORM_PACK16=8,
       VK_FORMAT_R8_UNORM=9,
       VK_FORMAT_R8_SNORM=10,
       VK_FORMAT_R8_USCALED=11,
       VK_FORMAT_R8_SSCALED=12,
       VK_FORMAT_R8_UINT=13,
       VK_FORMAT_R8_SINT=14,
       VK_FORMAT_R8_SRGB=15,
       VK_FORMAT_R8G8_UNORM=16,
       VK_FORMAT_R8G8_SNORM=17,
       VK_FORMAT_R8G8_USCALED=18,
       VK_FORMAT_R8G8_SSCALED=19,
       VK_FORMAT_R8G8_UINT=20,
       VK_FORMAT_R8G8_SINT=21,
       VK_FORMAT_R8G8_SRGB=22,
       VK_FORMAT_R8G8B8_UNORM=23,
       VK_FORMAT_R8G8B8_SNORM=24,
       VK_FORMAT_R8G8B8_USCALED=25,
       VK_FORMAT_R8G8B8_SSCALED=26,
       VK_FORMAT_R8G8B8_UINT=27,
       VK_FORMAT_R8G8B8_SINT=28,
       VK_FORMAT_R8G8B8_SRGB=29,
       VK_FORMAT_B8G8R8_UNORM=30,
       VK_FORMAT_B8G8R8_SNORM=31,
       VK_FORMAT_B8G8R8_USCALED=32,
       VK_FORMAT_B8G8R8_SSCALED=33,
       VK_FORMAT_B8G8R8_UINT=34,
       VK_FORMAT_B8G8R8_SINT=35,
       VK_FORMAT_B8G8R8_SRGB=36,
       VK_FORMAT_R8G8B8A8_UNORM=37,
       VK_FORMAT_R8G8B8A8_SNORM=38,
       VK_FORMAT_R8G8B8A8_USCALED=39,
       VK_FORMAT_R8G8B8A8_SSCALED=40,
       VK_FORMAT_R8G8B8A8_UINT=41,
       VK_FORMAT_R8G8B8A8_SINT=42,
       VK_FORMAT_R8G8B8A8_SRGB=43,
       VK_FORMAT_B8G8R8A8_UNORM=44,
       VK_FORMAT_B8G8R8A8_SNORM=45,
       VK_FORMAT_B8G8R8A8_USCALED=46,
       VK_FORMAT_B8G8R8A8_SSCALED=47,
       VK_FORMAT_B8G8R8A8_UINT=48,
       VK_FORMAT_B8G8R8A8_SINT=49,
       VK_FORMAT_B8G8R8A8_SRGB=50,
       VK_FORMAT_A8B8G8R8_UNORM_PACK32=51,
       VK_FORMAT_A8B8G8R8_SNORM_PACK32=52,
       VK_FORMAT_A8B8G8R8_USCALED_PACK32=53,
       VK_FORMAT_A8B8G8R8_SSCALED_PACK32=54,
       VK_FORMAT_A8B8G8R8_UINT_PACK32=55,
       VK_FORMAT_A8B8G8R8_SINT_PACK32=56,
       VK_FORMAT_A8B8G8R8_SRGB_PACK32=57,
       VK_FORMAT_A2R10G10B10_UNORM_PACK32=58,
       VK_FORMAT_A2R10G10B10_SNORM_PACK32=59,
       VK_FORMAT_A2R10G10B10_USCALED_PACK32=60,
       VK_FORMAT_A2R10G10B10_SSCALED_PACK32=61,
       VK_FORMAT_A2R10G10B10_UINT_PACK32=62,
       VK_FORMAT_A2R10G10B10_SINT_PACK32=63,
       VK_FORMAT_A2B10G10R10_UNORM_PACK32=64,
       VK_FORMAT_A2B10G10R10_SNORM_PACK32=65,
       VK_FORMAT_A2B10G10R10_USCALED_PACK32=66,
       VK_FORMAT_A2B10G10R10_SSCALED_PACK32=67,
       VK_FORMAT_A2B10G10R10_UINT_PACK32=68,
       VK_FORMAT_A2B10G10R10_SINT_PACK32=69,
       VK_FORMAT_R16_UNORM=70,
       VK_FORMAT_R16_SNORM=71,
       VK_FORMAT_R16_USCALED=72,
       VK_FORMAT_R16_SSCALED=73,
       VK_FORMAT_R16_UINT=74,
       VK_FORMAT_R16_SINT=75,
       VK_FORMAT_R16_SFLOAT=76,
       VK_FORMAT_R16G16_UNORM=77,
       VK_FORMAT_R16G16_SNORM=78,
       VK_FORMAT_R16G16_USCALED=79,
       VK_FORMAT_R16G16_SSCALED=80,
       VK_FORMAT_R16G16_UINT=81,
       VK_FORMAT_R16G16_SINT=82,
       VK_FORMAT_R16G16_SFLOAT=83,
       VK_FORMAT_R16G16B16_UNORM=84,
       VK_FORMAT_R16G16B16_SNORM=85,
       VK_FORMAT_R16G16B16_USCALED=86,
       VK_FORMAT_R16G16B16_SSCALED=87,
       VK_FORMAT_R16G16B16_UINT=88,
       VK_FORMAT_R16G16B16_SINT=89,
       VK_FORMAT_R16G16B16_SFLOAT=90,
       VK_FORMAT_R16G16B16A16_UNORM=91,
       VK_FORMAT_R16G16B16A16_SNORM=92,
       VK_FORMAT_R16G16B16A16_USCALED=93,
       VK_FORMAT_R16G16B16A16_SSCALED=94,
       VK_FORMAT_R16G16B16A16_UINT=95,
       VK_FORMAT_R16G16B16A16_SINT=96,
       VK_FORMAT_R16G16B16A16_SFLOAT=97,
       VK_FORMAT_R32_UINT=98,
       VK_FORMAT_R32_SINT=99,
       VK_FORMAT_R32_SFLOAT=100,
       VK_FORMAT_R32G32_UINT=101,
       VK_FORMAT_R32G32_SINT=102,
       VK_FORMAT_R32G32_SFLOAT=103,
       VK_FORMAT_R32G32B32_UINT=104,
       VK_FORMAT_R32G32B32_SINT=105,
       VK_FORMAT_R32G32B32_SFLOAT=106,
       VK_FORMAT_R32G32B32A32_UINT=107,
       VK_FORMAT_R32G32B32A32_SINT=108,
       VK_FORMAT_R32G32B32A32_SFLOAT=109,
       VK_FORMAT_R64_UINT=110,
       VK_FORMAT_R64_SINT=111,
       VK_FORMAT_R64_SFLOAT=112,
       VK_FORMAT_R64G64_UINT=113,
       VK_FORMAT_R64G64_SINT=114,
       VK_FORMAT_R64G64_SFLOAT=115,
       VK_FORMAT_R64G64B64_UINT=116,
       VK_FORMAT_R64G64B64_SINT=117,
       VK_FORMAT_R64G64B64_SFLOAT=118,
       VK_FORMAT_R64G64B64A64_UINT=119,
       VK_FORMAT_R64G64B64A64_SINT=120,
       VK_FORMAT_R64G64B64A64_SFLOAT=121,
       VK_FORMAT_B10G11R11_UFLOAT_PACK32=122,
       VK_FORMAT_E5B9G9R9_UFLOAT_PACK32=123,
       VK_FORMAT_D16_UNORM=124,
       VK_FORMAT_X8_D24_UNORM_PACK32=125,
       VK_FORMAT_D32_SFLOAT=126,
       VK_FORMAT_S8_UINT=127,
       VK_FORMAT_D16_UNORM_S8_UINT=128,
       VK_FORMAT_D24_UNORM_S8_UINT=129,
       VK_FORMAT_D32_SFLOAT_S8_UINT=130,
       VK_FORMAT_BC1_RGB_UNORM_BLOCK=131,
       VK_FORMAT_BC1_RGB_SRGB_BLOCK=132,
       VK_FORMAT_BC1_RGBA_UNORM_BLOCK=133,
       VK_FORMAT_BC1_RGBA_SRGB_BLOCK=134,
       VK_FORMAT_BC2_UNORM_BLOCK=135,
       VK_FORMAT_BC2_SRGB_BLOCK=136,
       VK_FORMAT_BC3_UNORM_BLOCK=137,
       VK_FORMAT_BC3_SRGB_BLOCK=138,
       VK_FORMAT_BC4_UNORM_BLOCK=139,
       VK_FORMAT_BC4_SNORM_BLOCK=140,
       VK_FORMAT_BC5_UNORM_BLOCK=141,
       VK_FORMAT_BC5_SNORM_BLOCK=142,
       VK_FORMAT_BC6H_UFLOAT_BLOCK=143,
       VK_FORMAT_BC6H_SFLOAT_BLOCK=144,
       VK_FORMAT_BC7_UNORM_BLOCK=145,
       VK_FORMAT_BC7_SRGB_BLOCK=146,
       VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK=147,
       VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK=148,
       VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK=149,
       VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK=150,
       VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK=151,
       VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK=152,
       VK_FORMAT_EAC_R11_UNORM_BLOCK=153,
       VK_FORMAT_EAC_R11_SNORM_BLOCK=154,
       VK_FORMAT_EAC_R11G11_UNORM_BLOCK=155,
       VK_FORMAT_EAC_R11G11_SNORM_BLOCK=156,
       VK_FORMAT_ASTC_4x4_UNORM_BLOCK=157,
       VK_FORMAT_ASTC_4x4_SRGB_BLOCK=158,
       VK_FORMAT_ASTC_5x4_UNORM_BLOCK=159,
       VK_FORMAT_ASTC_5x4_SRGB_BLOCK=160,
       VK_FORMAT_ASTC_5x5_UNORM_BLOCK=161,
       VK_FORMAT_ASTC_5x5_SRGB_BLOCK=162,
       VK_FORMAT_ASTC_6x5_UNORM_BLOCK=163,
       VK_FORMAT_ASTC_6x5_SRGB_BLOCK=164,
       VK_FORMAT_ASTC_6x6_UNORM_BLOCK=165,
       VK_FORMAT_ASTC_6x6_SRGB_BLOCK=166,
       VK_FORMAT_ASTC_8x5_UNORM_BLOCK=167,
       VK_FORMAT_ASTC_8x5_SRGB_BLOCK=168,
       VK_FORMAT_ASTC_8x6_UNORM_BLOCK=169,
       VK_FORMAT_ASTC_8x6_SRGB_BLOCK=170,
       VK_FORMAT_ASTC_8x8_UNORM_BLOCK=171,
       VK_FORMAT_ASTC_8x8_SRGB_BLOCK=172,
       VK_FORMAT_ASTC_10x5_UNORM_BLOCK=173,
       VK_FORMAT_ASTC_10x5_SRGB_BLOCK=174,
       VK_FORMAT_ASTC_10x6_UNORM_BLOCK=175,
       VK_FORMAT_ASTC_10x6_SRGB_BLOCK=176,
       VK_FORMAT_ASTC_10x8_UNORM_BLOCK=177,
       VK_FORMAT_ASTC_10x8_SRGB_BLOCK=178,
       VK_FORMAT_ASTC_10x10_UNORM_BLOCK=179,
       VK_FORMAT_ASTC_10x10_SRGB_BLOCK=180,
       VK_FORMAT_ASTC_12x10_UNORM_BLOCK=181,
       VK_FORMAT_ASTC_12x10_SRGB_BLOCK=182,
       VK_FORMAT_ASTC_12x12_UNORM_BLOCK=183,
       VK_FORMAT_ASTC_12x12_SRGB_BLOCK=184,
       VK_FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG=1000054000,
       VK_FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG=1000054001,
       VK_FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG=1000054002,
       VK_FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG=1000054003,
       VK_FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG=1000054004,
       VK_FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG=1000054005,
       VK_FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG=1000054006,
       VK_FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG=1000054007
      );
     PPVkFormat=^PVkFormat;
     PVkFormat=^TVkFormat;

     TVkStructureType=
      (
       VK_STRUCTURE_TYPE_APPLICATION_INFO=0,
       VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO=1,
       VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO=2,
       VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO=3,
       VK_STRUCTURE_TYPE_SUBMIT_INFO=4,
       VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO=5,
       VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE=6,
       VK_STRUCTURE_TYPE_BIND_SPARSE_INFO=7,
       VK_STRUCTURE_TYPE_FENCE_CREATE_INFO=8,
       VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO=9,
       VK_STRUCTURE_TYPE_EVENT_CREATE_INFO=10,
       VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO=11,
       VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO=12,
       VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO=13,
       VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO=14,
       VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO=15,
       VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO=16,
       VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO=17,
       VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO=18,
       VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO=19,
       VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO=20,
       VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO=21,
       VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO=22,
       VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO=23,
       VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO=24,
       VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO=25,
       VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO=26,
       VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO=27,
       VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO=28,
       VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO=29,
       VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO=30,
       VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO=31,
       VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO=32,
       VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO=33,
       VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO=34,
       VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET=35,
       VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET=36,
       VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO=37,
       VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO=38,
       VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO=39,
       VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO=40,
       VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO=41,
       VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO=42,
       VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO=43,
       VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER=44,
       VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER=45,
       VK_STRUCTURE_TYPE_MEMORY_BARRIER=46,
       VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO=47,                         // Reserved for internal use by the loader, layers, and ICDs
       VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO=48,                           // Reserved for internal use by the loader, layers, and ICDs
       VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR=1000001000,
       VK_STRUCTURE_TYPE_PRESENT_INFO_KHR=1000001001,
       VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR=1000002000,
       VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR=1000002001,
       VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR=1000003000,
       VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR=1000004000,
       VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR=1000005000,
       VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR=1000006000,
       VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR=1000007000,
       VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR=1000008000,
       VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR=1000009000,
       VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT=1000011000,
       VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT=1000011000,
       VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD=1000018000,
       VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT=1000022000,
       VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT=1000022001,
       VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT=1000022002,
       VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV=1000026000,
       VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV=1000026001,
       VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV=1000026002,
       VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD=1000041000,
       VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHX=1000053000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHX=1000053001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHX=1000053002,
       VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV=1000056000,
       VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV=1000056001,
       VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV=1000057000,
       VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV=1000057001,
       VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV=1000058000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR=1000059000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR=1000059001,
       VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR=1000059002,
       VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR=1000059003,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR=1000059004,
       VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR=1000059005,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR=1000059006,
       VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR=1000059007,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR=1000059008,
       VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHX=1000060000,
       VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHX=1000060001,
       VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHX=1000060002,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHX=1000060003,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHX=1000060004,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHX=1000060005,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHX=1000060006,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHX=1000060007,
       VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHX=1000060008,
       VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHX=1000060009,
       VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHX=1000060010,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHX=1000060011,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHX=1000060012,
       VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT=1000061000,
       VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN=1000062000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHX=1000070000,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHX=1000070001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR=1000071000,
       VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR=1000071001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR=1000071002,
       VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR=1000071003,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR=1000071004,
       VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR=1000072000,
       VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR=1000072001,
       VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR=1000072002,
       VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR=1000073000,
       VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR=1000073001,
       VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR=1000073002,
       VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR=1000073003,
       VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR=1000074000,
       VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR=1000074001,
       VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR=1000074002,
       VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR=1000075000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHR=1000076000,
       VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHR=1000076001,
       VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR=1000077000,
       VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR=1000078000,
       VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR=1000078001,
       VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR=1000078002,
       VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR=1000078003,
       VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR=1000079000,
       VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR=1000079001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR=1000080000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR=1000083000,
       VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR=1000084000,
       VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR=1000085000,
       VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX=1000086000,
       VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX=1000086001,
       VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX=1000086002,
       VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX=1000086003,
       VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX=1000086004,
       VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX=1000086005,
       VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV=1000087000,
       VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT=1000090000,
       VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT=1000091000,
       VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT=1000091001,
       VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT=1000091002,
       VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT=1000091003,
       VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE=1000092000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX=1000097000,
       VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV=1000098000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT=1000099000,
       VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT=1000099001,
       VK_STRUCTURE_TYPE_HDR_METADATA_EXT=1000105000,
       VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR=1000111000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR=1000112000,
       VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR=1000112001,
       VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR=1000113000,
       VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR=1000114000,
       VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR=1000114001,
       VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR=1000114002,
       VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR=1000115000,
       VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR=1000115001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR=1000119000,
       VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR=1000119001,
       VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR=1000119002,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR=1000120000,
       VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK=1000122000,
       VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK=1000123000,
       VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR=1000127000,
       VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR=1000127001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT=1000130000,
       VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT=1000130001,
       VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR=1000146000,
       VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR=1000146001,
       VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR=1000146002,
       VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR=1000146003,
       VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR=1000146004,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT=1000148000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT=1000148001,
       VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT=1000148002,
       VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV=1000149000,
       VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV=1000152000
      );
     PPVkStructureType=^PVkStructureType;
     PVkStructureType=^TVkStructureType;

     TVkSubpassContents=
      (
       VK_SUBPASS_CONTENTS_INLINE=0,
       VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS=1
      );
     PPVkSubpassContents=^PVkSubpassContents;
     PVkSubpassContents=^TVkSubpassContents;

     TVkResult=
      (
       VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR=-1000072003,
       VK_ERROR_OUT_OF_POOL_MEMORY_KHR=-1000069000,
       VK_ERROR_INVALID_SHADER_NV=-1000012000,
       VK_ERROR_VALIDATION_FAILED_EXT=-1000011001,
       VK_ERROR_INCOMPATIBLE_DISPLAY_KHR=-1000003001,
       VK_ERROR_OUT_OF_DATE_KHR=-1000001004,
       VK_ERROR_NATIVE_WINDOW_IN_USE_KHR=-1000000001,
       VK_ERROR_SURFACE_LOST_KHR=-1000000000,
       VK_ERROR_FRAGMENTED_POOL=-12,                                             // A requested pool allocation has failed due to fragmentation of the pool's memory
       _UNUSED_START=-12,
       VK_ERROR_FORMAT_NOT_SUPPORTED=-11,                                        // Requested format is not supported on this device
       VK_ERROR_TOO_MANY_OBJECTS=-10,                                            // Too many objects of the type have already been created
       VK_ERROR_INCOMPATIBLE_DRIVER=-9,                                          // Unable to find a Vulkan driver
       VK_ERROR_FEATURE_NOT_PRESENT=-8,                                          // Requested feature is not available on this device
       VK_ERROR_EXTENSION_NOT_PRESENT=-7,                                        // Extension specified does not exist
       VK_ERROR_LAYER_NOT_PRESENT=-6,                                            // Layer specified does not exist
       VK_ERROR_MEMORY_MAP_FAILED=-5,                                            // Mapping of a memory object has failed
       VK_ERROR_DEVICE_LOST=-4,                                                  // The logical device has been lost. See <<devsandqueues-lost-device>>
       VK_ERROR_INITIALIZATION_FAILED=-3,                                        // Initialization of a object has failed
       VK_ERROR_OUT_OF_DEVICE_MEMORY=-2,                                         // A device memory allocation has failed
       VK_ERROR_OUT_OF_HOST_MEMORY=-1,                                           // A host memory allocation has failed
       VK_SUCCESS=0,                                                             // Command completed successfully
       VK_NOT_READY=1,                                                           // A fence or query has not yet completed
       VK_TIMEOUT=2,                                                             // A wait operation has not completed in the specified time
       VK_EVENT_SET=3,                                                           // An event is signaled
       VK_EVENT_RESET=4,                                                         // An event is unsignaled
       VK_INCOMPLETE=5,                                                          // A return array was too small for the result
       VK_SUBOPTIMAL_KHR=1000001003
      );
     PPVkResult=^PVkResult;
     PVkResult=^TVkResult;

     TVkDynamicState=
      (
       VK_DYNAMIC_STATE_VIEWPORT=0,
       VK_DYNAMIC_STATE_SCISSOR=1,
       VK_DYNAMIC_STATE_LINE_WIDTH=2,
       VK_DYNAMIC_STATE_DEPTH_BIAS=3,
       VK_DYNAMIC_STATE_BLEND_CONSTANTS=4,
       VK_DYNAMIC_STATE_DEPTH_BOUNDS=5,
       VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK=6,
       VK_DYNAMIC_STATE_STENCIL_WRITE_MASK=7,
       VK_DYNAMIC_STATE_STENCIL_REFERENCE=8,
       VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV=1000087000,
       VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT=1000099000
      );
     PPVkDynamicState=^PVkDynamicState;
     PVkDynamicState=^TVkDynamicState;

     TVkDescriptorUpdateTemplateTypeKHR=
      (
       VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR=0,                  // Create descriptor update template for descriptor set updates
       VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR=1                 // Create descriptor update template for pushed descriptor updates
      );
     PPVkDescriptorUpdateTemplateTypeKHR=^PVkDescriptorUpdateTemplateTypeKHR;
     PVkDescriptorUpdateTemplateTypeKHR=^TVkDescriptorUpdateTemplateTypeKHR;

     TVkObjectType=
      (
       VK_OBJECT_TYPE_UNKNOWN=0,
       VK_OBJECT_TYPE_INSTANCE=1,                                                // VkInstance
       VK_OBJECT_TYPE_PHYSICAL_DEVICE=2,                                         // VkPhysicalDevice
       VK_OBJECT_TYPE_DEVICE=3,                                                  // VkDevice
       VK_OBJECT_TYPE_QUEUE=4,                                                   // VkQueue
       VK_OBJECT_TYPE_SEMAPHORE=5,                                               // VkSemaphore
       VK_OBJECT_TYPE_COMMAND_BUFFER=6,                                          // VkCommandBuffer
       VK_OBJECT_TYPE_FENCE=7,                                                   // VkFence
       VK_OBJECT_TYPE_DEVICE_MEMORY=8,                                           // VkDeviceMemory
       VK_OBJECT_TYPE_BUFFER=9,                                                  // VkBuffer
       VK_OBJECT_TYPE_IMAGE=10,                                                  // VkImage
       VK_OBJECT_TYPE_EVENT=11,                                                  // VkEvent
       VK_OBJECT_TYPE_QUERY_POOL=12,                                             // VkQueryPool
       VK_OBJECT_TYPE_BUFFER_VIEW=13,                                            // VkBufferView
       VK_OBJECT_TYPE_IMAGE_VIEW=14,                                             // VkImageView
       VK_OBJECT_TYPE_SHADER_MODULE=15,                                          // VkShaderModule
       VK_OBJECT_TYPE_PIPELINE_CACHE=16,                                         // VkPipelineCache
       VK_OBJECT_TYPE_PIPELINE_LAYOUT=17,                                        // VkPipelineLayout
       VK_OBJECT_TYPE_RENDER_PASS=18,                                            // VkRenderPass
       VK_OBJECT_TYPE_PIPELINE=19,                                               // VkPipeline
       VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT=20,                                  // VkDescriptorSetLayout
       VK_OBJECT_TYPE_SAMPLER=21,                                                // VkSampler
       VK_OBJECT_TYPE_DESCRIPTOR_POOL=22,                                        // VkDescriptorPool
       VK_OBJECT_TYPE_DESCRIPTOR_SET=23,                                         // VkDescriptorSet
       VK_OBJECT_TYPE_FRAMEBUFFER=24,                                            // VkFramebuffer
       VK_OBJECT_TYPE_COMMAND_POOL=25,                                           // VkCommandPool
       VK_OBJECT_TYPE_SURFACE_KHR=1000000000,
       VK_OBJECT_TYPE_SWAPCHAIN_KHR=1000001000,
       VK_OBJECT_TYPE_DISPLAY_KHR=1000002000,
       VK_OBJECT_TYPE_DISPLAY_MODE_KHR=1000002001,
       VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT=1000011000,
       VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR=1000085000,
       VK_OBJECT_TYPE_OBJECT_TABLE_NVX=1000086000,
       VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX=1000086001
      );
     PPVkObjectType=^PVkObjectType;
     PVkObjectType=^TVkObjectType;

     VkQueueFlagBits_=
      (
       VK_QUEUE_GRAPHICS_BIT=0,                                                  // Queue supports graphics operations
       VK_QUEUE_COMPUTE_BIT=1,                                                   // Queue supports compute operations
       VK_QUEUE_TRANSFER_BIT=2,                                                  // Queue supports transfer operations
       VK_QUEUE_SPARSE_BINDING_BIT=3                                             // Queue supports sparse resource memory management operations
      );
     TVkQueueFlagBit=VkQueueFlagBits_;
     PPVkQueueFlagBit=^PVkQueueFlagBit;
     PVkQueueFlagBit=^TVkQueueFlagBit;

     TVkQueueFlagBits= Set of TVkQueueFlagBit; // This is SET OF
     PPVkQueueFlagBits=^PVkQueueFlagBits;
     PVkQueueFlagBits=^TVkQueueFlagBits;

     VkMemoryPropertyFlagBits_=
      (
       VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT=0,                                    // If otherwise stated, then allocate memory on device
       VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT=1,                                    // Memory is mappable by host
       VK_MEMORY_PROPERTY_HOST_COHERENT_BIT=2,                                   // Memory will have i/o coherency. If not set, application may need to use vkFlushMappedMemoryRanges and vkInvalidateMappedMemoryRanges to flush/invalidate host cache
       VK_MEMORY_PROPERTY_HOST_CACHED_BIT=3,                                     // Memory will be cached by the host
       VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT=4                                 // Memory may be allocated by the driver when it is required
      );
     TVkMemoryPropertyFlagBit=VkMemoryPropertyFlagBits_;
     PPVkMemoryPropertyFlagBit=^PVkMemoryPropertyFlagBit;
     PVkMemoryPropertyFlagBit=^TVkMemoryPropertyFlagBit;

     TVkMemoryPropertyFlagBits= Set of TVkMemoryPropertyFlagBit; // This is SET OF
     PPVkMemoryPropertyFlagBits=^PVkMemoryPropertyFlagBits;
     PVkMemoryPropertyFlagBits=^TVkMemoryPropertyFlagBits;

     VkMemoryHeapFlagBits_=
      (
       VK_MEMORY_HEAP_DEVICE_LOCAL_BIT=0,                                        // If set, heap represents device memory
       VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHX=1
      );
     TVkMemoryHeapFlagBit=VkMemoryHeapFlagBits_;
     PPVkMemoryHeapFlagBit=^PVkMemoryHeapFlagBit;
     PVkMemoryHeapFlagBit=^TVkMemoryHeapFlagBit;

     TVkMemoryHeapFlagBits= Set of TVkMemoryHeapFlagBit; // This is SET OF
     PPVkMemoryHeapFlagBits=^PVkMemoryHeapFlagBits;
     PVkMemoryHeapFlagBits=^TVkMemoryHeapFlagBits;

     VkAccessFlagBits_=
      (
       VK_ACCESS_INDIRECT_COMMAND_READ_BIT=0,                                    // Controls coherency of indirect command reads
       VK_ACCESS_INDEX_READ_BIT=1,                                               // Controls coherency of index reads
       VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT=2,                                    // Controls coherency of vertex attribute reads
       VK_ACCESS_UNIFORM_READ_BIT=3,                                             // Controls coherency of uniform buffer reads
       VK_ACCESS_INPUT_ATTACHMENT_READ_BIT=4,                                    // Controls coherency of input attachment reads
       VK_ACCESS_SHADER_READ_BIT=5,                                              // Controls coherency of shader reads
       VK_ACCESS_SHADER_WRITE_BIT=6,                                             // Controls coherency of shader writes
       VK_ACCESS_COLOR_ATTACHMENT_READ_BIT=7,                                    // Controls coherency of color attachment reads
       VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT=8,                                   // Controls coherency of color attachment writes
       VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT=9,                            // Controls coherency of depth/stencil attachment reads
       VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT=10,                          // Controls coherency of depth/stencil attachment writes
       VK_ACCESS_TRANSFER_READ_BIT=11,                                           // Controls coherency of transfer reads
       VK_ACCESS_TRANSFER_WRITE_BIT=12,                                          // Controls coherency of transfer writes
       VK_ACCESS_HOST_READ_BIT=13,                                               // Controls coherency of host reads
       VK_ACCESS_HOST_WRITE_BIT=14,                                              // Controls coherency of host writes
       VK_ACCESS_MEMORY_READ_BIT=15,                                             // Controls coherency of memory reads
       VK_ACCESS_MEMORY_WRITE_BIT=16,                                            // Controls coherency of memory writes
       VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX=17,
       VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX=18,
       VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT=19
      );
     TVkAccessFlagBit=VkAccessFlagBits_;
     PPVkAccessFlagBit=^PVkAccessFlagBit;
     PVkAccessFlagBit=^TVkAccessFlagBit;

     TVkAccessFlagBits= Set of TVkAccessFlagBit; // This is SET OF
     PPVkAccessFlagBits=^PVkAccessFlagBits;
     PVkAccessFlagBits=^TVkAccessFlagBits;

     VkBufferUsageFlagBits_=
      (
       VK_BUFFER_USAGE_TRANSFER_SRC_BIT=0,                                       // Can be used as a source of transfer operations
       VK_BUFFER_USAGE_TRANSFER_DST_BIT=1,                                       // Can be used as a destination of transfer operations
       VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT=2,                               // Can be used as TBO
       VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT=3,                               // Can be used as IBO
       VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT=4,                                     // Can be used as UBO
       VK_BUFFER_USAGE_STORAGE_BUFFER_BIT=5,                                     // Can be used as SSBO
       VK_BUFFER_USAGE_INDEX_BUFFER_BIT=6,                                       // Can be used as source of fixed-function index fetch (index buffer)
       VK_BUFFER_USAGE_VERTEX_BUFFER_BIT=7,                                      // Can be used as source of fixed-function vertex fetch (VBO)
       VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT=8                                     // Can be the source of indirect parameters (e.g. indirect buffer, parameter buffer)
      );
     TVkBufferUsageFlagBit=VkBufferUsageFlagBits_;
     PPVkBufferUsageFlagBit=^PVkBufferUsageFlagBit;
     PVkBufferUsageFlagBit=^TVkBufferUsageFlagBit;

     TVkBufferUsageFlagBits= Set of TVkBufferUsageFlagBit; // This is SET OF
     PPVkBufferUsageFlagBits=^PVkBufferUsageFlagBits;
     PVkBufferUsageFlagBits=^TVkBufferUsageFlagBits;

     VkBufferCreateFlagBits_=
      (
       VK_BUFFER_CREATE_SPARSE_BINDING_BIT=0,                                    // Buffer should support sparse backing
       VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT=1,                                  // Buffer should support sparse backing with partial residency
       VK_BUFFER_CREATE_SPARSE_ALIASED_BIT=2                                     // Buffer should support constent data access to physical memory ranges mapped into multiple locations of sparse buffers
      );
     TVkBufferCreateFlagBit=VkBufferCreateFlagBits_;
     PPVkBufferCreateFlagBit=^PVkBufferCreateFlagBit;
     PVkBufferCreateFlagBit=^TVkBufferCreateFlagBit;

     TVkBufferCreateFlagBits= Set of TVkBufferCreateFlagBit; // This is SET OF
     PPVkBufferCreateFlagBits=^PVkBufferCreateFlagBits;
     PVkBufferCreateFlagBits=^TVkBufferCreateFlagBits;

     VkShaderStageFlagBits_=
      (
       VK_SHADER_STAGE_VERTEX_BIT=0,
       VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT=1,
       VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT=2,
       VK_SHADER_STAGE_GEOMETRY_BIT=3,
       VK_SHADER_STAGE_FRAGMENT_BIT=4,
       VK_SHADER_STAGE_ALL_GRAPHICS=5,
       VK_SHADER_STAGE_COMPUTE_BIT=6,
       VK_SHADER_STAGE_ALL=7
      );
     TVkShaderStageFlagBit=VkShaderStageFlagBits_;
     PPVkShaderStageFlagBit=^PVkShaderStageFlagBit;
     PVkShaderStageFlagBit=^TVkShaderStageFlagBit;

     TVkShaderStageFlagBits= Set of TVkShaderStageFlagBit; // This is SET OF
     PPVkShaderStageFlagBits=^PVkShaderStageFlagBits;
     PVkShaderStageFlagBits=^TVkShaderStageFlagBits;

     VkImageUsageFlagBits_=
      (
       VK_IMAGE_USAGE_TRANSFER_SRC_BIT=0,                                        // Can be used as a source of transfer operations
       VK_IMAGE_USAGE_TRANSFER_DST_BIT=1,                                        // Can be used as a destination of transfer operations
       VK_IMAGE_USAGE_SAMPLED_BIT=2,                                             // Can be sampled from (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
       VK_IMAGE_USAGE_STORAGE_BIT=3,                                             // Can be used as storage image (STORAGE_IMAGE descriptor type)
       VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT=4,                                    // Can be used as framebuffer color attachment
       VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT=5,                            // Can be used as framebuffer depth/stencil attachment
       VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT=6,                                // Image data not needed outside of rendering
       VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT=7                                     // Can be used as framebuffer input attachment
      );
     TVkImageUsageFlagBit=VkImageUsageFlagBits_;
     PPVkImageUsageFlagBit=^PVkImageUsageFlagBit;
     PVkImageUsageFlagBit=^TVkImageUsageFlagBit;

     TVkImageUsageFlagBits= Set of TVkImageUsageFlagBit; // This is SET OF
     PPVkImageUsageFlagBits=^PVkImageUsageFlagBits;
     PVkImageUsageFlagBits=^TVkImageUsageFlagBits;

     VkImageCreateFlagBits_=
      (
       VK_IMAGE_CREATE_SPARSE_BINDING_BIT=0,                                     // Image should support sparse backing
       VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT=1,                                   // Image should support sparse backing with partial residency
       VK_IMAGE_CREATE_SPARSE_ALIASED_BIT=2,                                     // Image should support constent data access to physical memory ranges mapped into multiple locations of sparse images
       VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT=3,                                     // Allows image views to have different format than the base image
       VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT=4,                                    // Allows creating image views with cube type from the created image
       VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR=5,
       VK_IMAGE_CREATE_BIND_SFR_BIT_KHX=6
      );
     TVkImageCreateFlagBit=VkImageCreateFlagBits_;
     PPVkImageCreateFlagBit=^PVkImageCreateFlagBit;
     PVkImageCreateFlagBit=^TVkImageCreateFlagBit;

     TVkImageCreateFlagBits= Set of TVkImageCreateFlagBit; // This is SET OF
     PPVkImageCreateFlagBits=^PVkImageCreateFlagBits;
     PVkImageCreateFlagBits=^TVkImageCreateFlagBits;

     VkPipelineCreateFlagBits_=
      (
       VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT=0,
       VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT=1,
       VK_PIPELINE_CREATE_DERIVATIVE_BIT=2,
       VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHX=3,
       VK_PIPELINE_CREATE_DISPATCH_BASE_KHX=4
      );
     TVkPipelineCreateFlagBit=VkPipelineCreateFlagBits_;
     PPVkPipelineCreateFlagBit=^PVkPipelineCreateFlagBit;
     PVkPipelineCreateFlagBit=^TVkPipelineCreateFlagBit;

     TVkPipelineCreateFlagBits= Set of TVkPipelineCreateFlagBit; // This is SET OF
     PPVkPipelineCreateFlagBits=^PVkPipelineCreateFlagBits;
     PVkPipelineCreateFlagBits=^TVkPipelineCreateFlagBits;

     VkColorComponentFlagBits_=
      (
       VK_COLOR_COMPONENT_R_BIT=0,
       VK_COLOR_COMPONENT_G_BIT=1,
       VK_COLOR_COMPONENT_B_BIT=2,
       VK_COLOR_COMPONENT_A_BIT=3
      );
     TVkColorComponentFlagBit=VkColorComponentFlagBits_;
     PPVkColorComponentFlagBit=^PVkColorComponentFlagBit;
     PVkColorComponentFlagBit=^TVkColorComponentFlagBit;

     TVkColorComponentFlagBits= Set of TVkColorComponentFlagBit; // This is SET OF
     PPVkColorComponentFlagBits=^PVkColorComponentFlagBits;
     PVkColorComponentFlagBits=^TVkColorComponentFlagBits;

     VkFenceCreateFlagBits_=
      (
       VK_FENCE_CREATE_SIGNALED_BIT=0
      );
     TVkFenceCreateFlagBit=VkFenceCreateFlagBits_;
     PPVkFenceCreateFlagBit=^PVkFenceCreateFlagBit;
     PVkFenceCreateFlagBit=^TVkFenceCreateFlagBit;

     TVkFenceCreateFlagBits= Set of TVkFenceCreateFlagBit; // This is SET OF
     PPVkFenceCreateFlagBits=^PVkFenceCreateFlagBits;
     PVkFenceCreateFlagBits=^TVkFenceCreateFlagBits;

     VkFormatFeatureFlagBits_=
      (
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT=0,                                    // Format can be used for sampled images (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
       VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT=1,                                    // Format can be used for storage images (STORAGE_IMAGE descriptor type)
       VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT=2,                             // Format supports atomic operations in case it is used for storage images
       VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT=3,                             // Format can be used for uniform texel buffers (TBOs)
       VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT=4,                             // Format can be used for storage texel buffers (IBOs)
       VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT=5,                      // Format supports atomic operations in case it is used for storage texel buffers
       VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT=6,                                    // Format can be used for vertex buffers (VBOs)
       VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT=7,                                 // Format can be used for color attachment images
       VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT=8,                           // Format supports blending in case it is used for color attachment images
       VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT=9,                         // Format can be used for depth/stencil attachment images
       VK_FORMAT_FEATURE_BLIT_SRC_BIT=10,                                        // Format can be used as the source image of blits with vkCmdBlitImage
       VK_FORMAT_FEATURE_BLIT_DST_BIT=11,                                        // Format can be used as the destination image of blits with vkCmdBlitImage
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT=12,                     // Format can be filtered with VK_FILTER_LINEAR when being sampled
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG=13,
       VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR=14,
       VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR=15,
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT=16
      );
     TVkFormatFeatureFlagBit=VkFormatFeatureFlagBits_;
     PPVkFormatFeatureFlagBit=^PVkFormatFeatureFlagBit;
     PVkFormatFeatureFlagBit=^TVkFormatFeatureFlagBit;

     TVkFormatFeatureFlagBits= Set of TVkFormatFeatureFlagBit; // This is SET OF
     PPVkFormatFeatureFlagBits=^PVkFormatFeatureFlagBits;
     PVkFormatFeatureFlagBits=^TVkFormatFeatureFlagBits;

     VkQueryControlFlagBits_=
      (
       VK_QUERY_CONTROL_PRECISE_BIT=0                                            // Require precise results to be collected by the query
      );
     TVkQueryControlFlagBit=VkQueryControlFlagBits_;
     PPVkQueryControlFlagBit=^PVkQueryControlFlagBit;
     PVkQueryControlFlagBit=^TVkQueryControlFlagBit;

     TVkQueryControlFlagBits= Set of TVkQueryControlFlagBit; // This is SET OF
     PPVkQueryControlFlagBits=^PVkQueryControlFlagBits;
     PVkQueryControlFlagBits=^TVkQueryControlFlagBits;

     VkQueryResultFlagBits_=
      (
       VK_QUERY_RESULT_64_BIT=0,                                                 // Results of the queries are written to the destination buffer as 64-bit values
       VK_QUERY_RESULT_WAIT_BIT=1,                                               // Results of the queries are waited on before proceeding with the result copy
       VK_QUERY_RESULT_WITH_AVAILABILITY_BIT=2,                                  // Besides the results of the query, the availability of the results is also written
       VK_QUERY_RESULT_PARTIAL_BIT=3                                             // Copy the partial results of the query even if the final results are not available
      );
     TVkQueryResultFlagBit=VkQueryResultFlagBits_;
     PPVkQueryResultFlagBit=^PVkQueryResultFlagBit;
     PVkQueryResultFlagBit=^TVkQueryResultFlagBit;

     TVkQueryResultFlagBits= Set of TVkQueryResultFlagBit; // This is SET OF
     PPVkQueryResultFlagBits=^PVkQueryResultFlagBits;
     PVkQueryResultFlagBits=^TVkQueryResultFlagBits;

     VkCommandBufferUsageFlagBits_=
      (
       VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT=0,
       VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT=1,
       VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT=2                            // Command buffer may be submitted/executed more than once simultaneously
      );
     TVkCommandBufferUsageFlagBit=VkCommandBufferUsageFlagBits_;
     PPVkCommandBufferUsageFlagBit=^PVkCommandBufferUsageFlagBit;
     PVkCommandBufferUsageFlagBit=^TVkCommandBufferUsageFlagBit;

     TVkCommandBufferUsageFlagBits= Set of TVkCommandBufferUsageFlagBit; // This is SET OF
     PPVkCommandBufferUsageFlagBits=^PVkCommandBufferUsageFlagBits;
     PVkCommandBufferUsageFlagBits=^TVkCommandBufferUsageFlagBits;

     VkQueryPipelineStatisticFlagBits_=
      (
       VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT=0,                // Optional
       VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT=1,              // Optional
       VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT=2,              // Optional
       VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT=3,            // Optional
       VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT=4,             // Optional
       VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT=5,                   // Optional
       VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT=6,                    // Optional
       VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT=7,            // Optional
       VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT=8,    // Optional
       VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT=9, // Optional
       VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT=10             // Optional
      );
     TVkQueryPipelineStatisticFlagBit=VkQueryPipelineStatisticFlagBits_;
     PPVkQueryPipelineStatisticFlagBit=^PVkQueryPipelineStatisticFlagBit;
     PVkQueryPipelineStatisticFlagBit=^TVkQueryPipelineStatisticFlagBit;

     TVkQueryPipelineStatisticFlagBits= Set of TVkQueryPipelineStatisticFlagBit; // This is SET OF
     PPVkQueryPipelineStatisticFlagBits=^PVkQueryPipelineStatisticFlagBits;
     PVkQueryPipelineStatisticFlagBits=^TVkQueryPipelineStatisticFlagBits;

     VkImageAspectFlagBits_=
      (
       VK_IMAGE_ASPECT_COLOR_BIT=0,
       VK_IMAGE_ASPECT_DEPTH_BIT=1,
       VK_IMAGE_ASPECT_STENCIL_BIT=2,
       VK_IMAGE_ASPECT_METADATA_BIT=3
      );
     TVkImageAspectFlagBit=VkImageAspectFlagBits_;
     PPVkImageAspectFlagBit=^PVkImageAspectFlagBit;
     PVkImageAspectFlagBit=^TVkImageAspectFlagBit;

     TVkImageAspectFlagBits= Set of TVkImageAspectFlagBit; // This is SET OF
     PPVkImageAspectFlagBits=^PVkImageAspectFlagBits;
     PVkImageAspectFlagBits=^TVkImageAspectFlagBits;

     VkSparseImageFormatFlagBits_=
      (
       VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT=0,                              // Image uses a single mip tail region for all array layers
       VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT=1,                            // Image requires mip level dimensions to be an integer multiple of the sparse image block dimensions for non-tail mip levels.
       VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT=2                       // Image uses a non-standard sparse image block dimensions
      );
     TVkSparseImageFormatFlagBit=VkSparseImageFormatFlagBits_;
     PPVkSparseImageFormatFlagBit=^PVkSparseImageFormatFlagBit;
     PVkSparseImageFormatFlagBit=^TVkSparseImageFormatFlagBit;

     TVkSparseImageFormatFlagBits= Set of TVkSparseImageFormatFlagBit; // This is SET OF
     PPVkSparseImageFormatFlagBits=^PVkSparseImageFormatFlagBits;
     PVkSparseImageFormatFlagBits=^TVkSparseImageFormatFlagBits;

     VkSparseMemoryBindFlagBits_=
      (
       VK_SPARSE_MEMORY_BIND_METADATA_BIT=0                                      // Operation binds resource metadata to memory
      );
     TVkSparseMemoryBindFlagBit=VkSparseMemoryBindFlagBits_;
     PPVkSparseMemoryBindFlagBit=^PVkSparseMemoryBindFlagBit;
     PVkSparseMemoryBindFlagBit=^TVkSparseMemoryBindFlagBit;

     TVkSparseMemoryBindFlagBits= Set of TVkSparseMemoryBindFlagBit; // This is SET OF
     PPVkSparseMemoryBindFlagBits=^PVkSparseMemoryBindFlagBits;
     PVkSparseMemoryBindFlagBits=^TVkSparseMemoryBindFlagBits;

     VkPipelineStageFlagBits_=
      (
       VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT=0,                                      // Before subsequent commands are processed
       VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT=1,                                    // Draw/DispatchIndirect command fetch
       VK_PIPELINE_STAGE_VERTEX_INPUT_BIT=2,                                     // Vertex/index fetch
       VK_PIPELINE_STAGE_VERTEX_SHADER_BIT=3,                                    // Vertex shading
       VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT=4,                      // Tessellation control shading
       VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT=5,                   // Tessellation evaluation shading
       VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT=6,                                  // Geometry shading
       VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT=7,                                  // Fragment shading
       VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT=8,                             // Early fragment (depth and stencil) tests
       VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT=9,                              // Late fragment (depth and stencil) tests
       VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT=10,                         // Color attachment writes
       VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT=11,                                  // Compute shading
       VK_PIPELINE_STAGE_TRANSFER_BIT=12,                                        // Transfer/copy operations
       VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT=13,                                  // After previous commands have completed
       VK_PIPELINE_STAGE_HOST_BIT=14,                                            // Indicates host (CPU) is a source/sink of the dependency
       VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT=15,                                    // All stages of the graphics pipeline
       VK_PIPELINE_STAGE_ALL_COMMANDS_BIT=16,                                    // All stages supported on the queue
       VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX=17
      );
     TVkPipelineStageFlagBit=VkPipelineStageFlagBits_;
     PPVkPipelineStageFlagBit=^PVkPipelineStageFlagBit;
     PVkPipelineStageFlagBit=^TVkPipelineStageFlagBit;

     TVkPipelineStageFlagBits= Set of TVkPipelineStageFlagBit; // This is SET OF
     PPVkPipelineStageFlagBits=^PVkPipelineStageFlagBits;
     PVkPipelineStageFlagBits=^TVkPipelineStageFlagBits;

     VkCommandPoolCreateFlagBits_=
      (
       VK_COMMAND_POOL_CREATE_TRANSIENT_BIT=0,                                   // Command buffers have a short lifetime
       VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT=1                         // Command buffers may release their memory individually
      );
     TVkCommandPoolCreateFlagBit=VkCommandPoolCreateFlagBits_;
     PPVkCommandPoolCreateFlagBit=^PVkCommandPoolCreateFlagBit;
     PVkCommandPoolCreateFlagBit=^TVkCommandPoolCreateFlagBit;

     TVkCommandPoolCreateFlagBits= Set of TVkCommandPoolCreateFlagBit; // This is SET OF
     PPVkCommandPoolCreateFlagBits=^PVkCommandPoolCreateFlagBits;
     PVkCommandPoolCreateFlagBits=^TVkCommandPoolCreateFlagBits;

     VkCommandPoolResetFlagBits_=
      (
       VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT=0                             // Release resources owned by the pool
      );
     TVkCommandPoolResetFlagBit=VkCommandPoolResetFlagBits_;
     PPVkCommandPoolResetFlagBit=^PVkCommandPoolResetFlagBit;
     PVkCommandPoolResetFlagBit=^TVkCommandPoolResetFlagBit;

     TVkCommandPoolResetFlagBits= Set of TVkCommandPoolResetFlagBit; // This is SET OF
     PPVkCommandPoolResetFlagBits=^PVkCommandPoolResetFlagBits;
     PVkCommandPoolResetFlagBits=^TVkCommandPoolResetFlagBits;

     VkCommandBufferResetFlagBits_=
      (
       VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT=0                           // Release resources owned by the buffer
      );
     TVkCommandBufferResetFlagBit=VkCommandBufferResetFlagBits_;
     PPVkCommandBufferResetFlagBit=^PVkCommandBufferResetFlagBit;
     PVkCommandBufferResetFlagBit=^TVkCommandBufferResetFlagBit;

     TVkCommandBufferResetFlagBits= Set of TVkCommandBufferResetFlagBit; // This is SET OF
     PPVkCommandBufferResetFlagBits=^PVkCommandBufferResetFlagBits;
     PVkCommandBufferResetFlagBits=^TVkCommandBufferResetFlagBits;

     VkSampleCountFlagBits_=
      (
       VK_SAMPLE_COUNT_1_BIT=0,                                                  // Sample count 1 supported
       VK_SAMPLE_COUNT_2_BIT=1,                                                  // Sample count 2 supported
       VK_SAMPLE_COUNT_4_BIT=2,                                                  // Sample count 4 supported
       VK_SAMPLE_COUNT_8_BIT=3,                                                  // Sample count 8 supported
       VK_SAMPLE_COUNT_16_BIT=4,                                                 // Sample count 16 supported
       VK_SAMPLE_COUNT_32_BIT=5,                                                 // Sample count 32 supported
       VK_SAMPLE_COUNT_64_BIT=6                                                  // Sample count 64 supported
      );
     TVkSampleCountFlagBit=VkSampleCountFlagBits_;
     PPVkSampleCountFlagBit=^PVkSampleCountFlagBit;
     PVkSampleCountFlagBit=^TVkSampleCountFlagBit;

     TVkSampleCountFlagBits= Set of TVkSampleCountFlagBit; // This is SET OF
     PPVkSampleCountFlagBits=^PVkSampleCountFlagBits;
     PVkSampleCountFlagBits=^TVkSampleCountFlagBits;

     VkAttachmentDescriptionFlagBits_=
      (
       VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT=0                                 // The attachment may alias physical memory of another attachment in the same render pass
      );
     TVkAttachmentDescriptionFlagBit=VkAttachmentDescriptionFlagBits_;
     PPVkAttachmentDescriptionFlagBit=^PVkAttachmentDescriptionFlagBit;
     PVkAttachmentDescriptionFlagBit=^TVkAttachmentDescriptionFlagBit;

     TVkAttachmentDescriptionFlagBits= Set of TVkAttachmentDescriptionFlagBit; // This is SET OF
     PPVkAttachmentDescriptionFlagBits=^PVkAttachmentDescriptionFlagBits;
     PVkAttachmentDescriptionFlagBits=^TVkAttachmentDescriptionFlagBits;

     VkStencilFaceFlagBits_=
      (
       VK_STENCIL_FACE_FRONT_BIT=0,                                              // Front face
       VK_STENCIL_FACE_BACK_BIT=1,                                               // Back face
       VK_STENCIL_FRONT_AND_BACK=2                                               // Front and back faces
      );
     TVkStencilFaceFlagBit=VkStencilFaceFlagBits_;
     PPVkStencilFaceFlagBit=^PVkStencilFaceFlagBit;
     PVkStencilFaceFlagBit=^TVkStencilFaceFlagBit;

     TVkStencilFaceFlagBits= Set of TVkStencilFaceFlagBit; // This is SET OF
     PPVkStencilFaceFlagBits=^PVkStencilFaceFlagBits;
     PVkStencilFaceFlagBits=^TVkStencilFaceFlagBits;

     VkDescriptorPoolCreateFlagBits_=
      (
       VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT=0                       // Descriptor sets may be freed individually
      );
     TVkDescriptorPoolCreateFlagBit=VkDescriptorPoolCreateFlagBits_;
     PPVkDescriptorPoolCreateFlagBit=^PVkDescriptorPoolCreateFlagBit;
     PVkDescriptorPoolCreateFlagBit=^TVkDescriptorPoolCreateFlagBit;

     TVkDescriptorPoolCreateFlagBits= Set of TVkDescriptorPoolCreateFlagBit; // This is SET OF
     PPVkDescriptorPoolCreateFlagBits=^PVkDescriptorPoolCreateFlagBits;
     PVkDescriptorPoolCreateFlagBits=^TVkDescriptorPoolCreateFlagBits;

     VkDependencyFlagBits_=
      (
       VK_DEPENDENCY_BY_REGION_BIT=0,                                            // Dependency is per pixel region 
       VK_DEPENDENCY_VIEW_LOCAL_BIT_KHX=1,
       VK_DEPENDENCY_DEVICE_GROUP_BIT_KHX=2
      );
     TVkDependencyFlagBit=VkDependencyFlagBits_;
     PPVkDependencyFlagBit=^PVkDependencyFlagBit;
     PVkDependencyFlagBit=^TVkDependencyFlagBit;

     TVkDependencyFlagBits= Set of TVkDependencyFlagBit; // This is SET OF
     PPVkDependencyFlagBits=^PVkDependencyFlagBits;
     PVkDependencyFlagBits=^TVkDependencyFlagBits;

     TVkPresentModeKHR=
      (
       VK_PRESENT_MODE_IMMEDIATE_KHR=0,
       VK_PRESENT_MODE_MAILBOX_KHR=1,
       VK_PRESENT_MODE_FIFO_KHR=2,
       VK_PRESENT_MODE_FIFO_RELAXED_KHR=3,
       VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR=1000111000,
       VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR=1000111001
      );
     PPVkPresentModeKHR=^PVkPresentModeKHR;
     PVkPresentModeKHR=^TVkPresentModeKHR;

     TVkColorSpaceKHR=
      (
       VK_COLOR_SPACE_SRGB_NONLINEAR_KHR=0,
       VK_COLORSPACE_SRGB_NONLINEAR_KHR=0,
       VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT=1000104001,
       VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT=1000104002,
       VK_COLOR_SPACE_DCI_P3_LINEAR_EXT=1000104003,
       VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT=1000104004,
       VK_COLOR_SPACE_BT709_LINEAR_EXT=1000104005,
       VK_COLOR_SPACE_BT709_NONLINEAR_EXT=1000104006,
       VK_COLOR_SPACE_BT2020_LINEAR_EXT=1000104007,
       VK_COLOR_SPACE_HDR10_ST2084_EXT=1000104008,
       VK_COLOR_SPACE_DOLBYVISION_EXT=1000104009,
       VK_COLOR_SPACE_HDR10_HLG_EXT=1000104010,
       VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT=1000104011,
       VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT=1000104012,
       VK_COLOR_SPACE_PASS_THROUGH_EXT=1000104013,
       VK_COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT=1000104014
      );
     PPVkColorSpaceKHR=^PVkColorSpaceKHR;
     PVkColorSpaceKHR=^TVkColorSpaceKHR;

     VkDisplayPlaneAlphaFlagBitsKHR_=
      (
       VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR=0,
       VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR=1,
       VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR=2,
       VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR=3
      );
     TVkDisplayPlaneAlphaFlagBitsKH=VkDisplayPlaneAlphaFlagBitsKHR_;
     PPVkDisplayPlaneAlphaFlagBitsKH=^PVkDisplayPlaneAlphaFlagBitsKH;
     PVkDisplayPlaneAlphaFlagBitsKH=^TVkDisplayPlaneAlphaFlagBitsKH;

     TVkDisplayPlaneAlphaFlagBitsKHR= Set of TVkDisplayPlaneAlphaFlagBitsKH; // This is SET OF
     PPVkDisplayPlaneAlphaFlagBitsKHR=^PVkDisplayPlaneAlphaFlagBitsKHR;
     PVkDisplayPlaneAlphaFlagBitsKHR=^TVkDisplayPlaneAlphaFlagBitsKHR;

     VkCompositeAlphaFlagBitsKHR_=
      (
       VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR=0,
       VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR=1,
       VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR=2,
       VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR=3
      );
     TVkCompositeAlphaFlagBitsKH=VkCompositeAlphaFlagBitsKHR_;
     PPVkCompositeAlphaFlagBitsKH=^PVkCompositeAlphaFlagBitsKH;
     PVkCompositeAlphaFlagBitsKH=^TVkCompositeAlphaFlagBitsKH;

     TVkCompositeAlphaFlagBitsKHR= Set of TVkCompositeAlphaFlagBitsKH; // This is SET OF
     PPVkCompositeAlphaFlagBitsKHR=^PVkCompositeAlphaFlagBitsKHR;
     PVkCompositeAlphaFlagBitsKHR=^TVkCompositeAlphaFlagBitsKHR;

     VkSurfaceTransformFlagBitsKHR_=
      (
       VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR=0,
       VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR=1,
       VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR=2,
       VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR=3,
       VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR=4,
       VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR=5,
       VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR=6,
       VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR=7,
       VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR=8
      );
     TVkSurfaceTransformFlagBitsKH=VkSurfaceTransformFlagBitsKHR_;
     PPVkSurfaceTransformFlagBitsKH=^PVkSurfaceTransformFlagBitsKH;
     PVkSurfaceTransformFlagBitsKH=^TVkSurfaceTransformFlagBitsKH;

     TVkSurfaceTransformFlagBitsKHR= Set of TVkSurfaceTransformFlagBitsKH; // This is SET OF
     PPVkSurfaceTransformFlagBitsKHR=^PVkSurfaceTransformFlagBitsKHR;
     PVkSurfaceTransformFlagBitsKHR=^TVkSurfaceTransformFlagBitsKHR;

     VkDebugReportFlagBitsEXT_=
      (
       VK_DEBUG_REPORT_INFORMATION_BIT_EXT=0,
       VK_DEBUG_REPORT_WARNING_BIT_EXT=1,
       VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT=2,
       VK_DEBUG_REPORT_ERROR_BIT_EXT=3,
       VK_DEBUG_REPORT_DEBUG_BIT_EXT=4
      );
     TVkDebugReportFlagBitsEX=VkDebugReportFlagBitsEXT_;
     PPVkDebugReportFlagBitsEX=^PVkDebugReportFlagBitsEX;
     PVkDebugReportFlagBitsEX=^TVkDebugReportFlagBitsEX;

     TVkDebugReportFlagBitsEXT= Set of TVkDebugReportFlagBitsEX; // This is SET OF
     PPVkDebugReportFlagBitsEXT=^PVkDebugReportFlagBitsEXT;
     PVkDebugReportFlagBitsEXT=^TVkDebugReportFlagBitsEXT;

     TVkDebugReportObjectTypeEXT=
      (
       VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT=0,
       VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT=1,
       VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT=2,
       VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT=3,
       VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT=4,
       VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT=5,
       VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT=6,
       VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT=7,
       VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT=8,
       VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT=9,
       VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT=10,
       VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT=11,
       VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT=12,
       VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT=13,
       VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT=14,
       VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT=15,
       VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT=16,
       VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT=17,
       VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT=18,
       VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT=19,
       VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT=20,
       VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT=21,
       VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT=22,
       VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT=23,
       VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT=24,
       VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT=25,
       VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT=26,
       VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT=27,
       VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT=28,
       VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT=28,
       VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT=29,
       VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT=30,
       VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT=31,
       VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT=32,
       VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT=1000085000
      );
     PPVkDebugReportObjectTypeEXT=^PVkDebugReportObjectTypeEXT;
     PVkDebugReportObjectTypeEXT=^TVkDebugReportObjectTypeEXT;

     TVkRasterizationOrderAMD=
      (
       VK_RASTERIZATION_ORDER_STRICT_AMD=0,
       VK_RASTERIZATION_ORDER_RELAXED_AMD=1
      );
     PPVkRasterizationOrderAMD=^PVkRasterizationOrderAMD;
     PVkRasterizationOrderAMD=^TVkRasterizationOrderAMD;

     VkExternalMemoryHandleTypeFlagBitsNV_=
      (
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV=0,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV=1,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV=2,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV=3
      );
     TVkExternalMemoryHandleTypeFlagBitsN=VkExternalMemoryHandleTypeFlagBitsNV_;
     PPVkExternalMemoryHandleTypeFlagBitsN=^PVkExternalMemoryHandleTypeFlagBitsN;
     PVkExternalMemoryHandleTypeFlagBitsN=^TVkExternalMemoryHandleTypeFlagBitsN;

     TVkExternalMemoryHandleTypeFlagBitsNV= Set of TVkExternalMemoryHandleTypeFlagBitsN; // This is SET OF
     PPVkExternalMemoryHandleTypeFlagBitsNV=^PVkExternalMemoryHandleTypeFlagBitsNV;
     PVkExternalMemoryHandleTypeFlagBitsNV=^TVkExternalMemoryHandleTypeFlagBitsNV;

     VkExternalMemoryFeatureFlagBitsNV_=
      (
       VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV=0,
       VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV=1,
       VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV=2
      );
     TVkExternalMemoryFeatureFlagBitsN=VkExternalMemoryFeatureFlagBitsNV_;
     PPVkExternalMemoryFeatureFlagBitsN=^PVkExternalMemoryFeatureFlagBitsN;
     PVkExternalMemoryFeatureFlagBitsN=^TVkExternalMemoryFeatureFlagBitsN;

     TVkExternalMemoryFeatureFlagBitsNV= Set of TVkExternalMemoryFeatureFlagBitsN; // This is SET OF
     PPVkExternalMemoryFeatureFlagBitsNV=^PVkExternalMemoryFeatureFlagBitsNV;
     PVkExternalMemoryFeatureFlagBitsNV=^TVkExternalMemoryFeatureFlagBitsNV;

     TVkValidationCheckEXT=
      (
       VK_VALIDATION_CHECK_ALL_EXT=0,
       VK_VALIDATION_CHECK_SHADERS_EXT=1
      );
     PPVkValidationCheckEXT=^PVkValidationCheckEXT;
     PVkValidationCheckEXT=^TVkValidationCheckEXT;

     VkIndirectCommandsLayoutUsageFlagBitsNVX_=
      (
       VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX=0,
       VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX=1,
       VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX=2,
       VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX=3
      );
     TVkIndirectCommandsLayoutUsageFlagBitsNV=VkIndirectCommandsLayoutUsageFlagBitsNVX_;
     PPVkIndirectCommandsLayoutUsageFlagBitsNV=^PVkIndirectCommandsLayoutUsageFlagBitsNV;
     PVkIndirectCommandsLayoutUsageFlagBitsNV=^TVkIndirectCommandsLayoutUsageFlagBitsNV;

     TVkIndirectCommandsLayoutUsageFlagBitsNVX= Set of TVkIndirectCommandsLayoutUsageFlagBitsNV; // This is SET OF
     PPVkIndirectCommandsLayoutUsageFlagBitsNVX=^PVkIndirectCommandsLayoutUsageFlagBitsNVX;
     PVkIndirectCommandsLayoutUsageFlagBitsNVX=^TVkIndirectCommandsLayoutUsageFlagBitsNVX;

     VkObjectEntryUsageFlagBitsNVX_=
      (
       VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX=0,
       VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX=1
      );
     TVkObjectEntryUsageFlagBitsNV=VkObjectEntryUsageFlagBitsNVX_;
     PPVkObjectEntryUsageFlagBitsNV=^PVkObjectEntryUsageFlagBitsNV;
     PVkObjectEntryUsageFlagBitsNV=^TVkObjectEntryUsageFlagBitsNV;

     TVkObjectEntryUsageFlagBitsNVX= Set of TVkObjectEntryUsageFlagBitsNV; // This is SET OF
     PPVkObjectEntryUsageFlagBitsNVX=^PVkObjectEntryUsageFlagBitsNVX;
     PVkObjectEntryUsageFlagBitsNVX=^TVkObjectEntryUsageFlagBitsNVX;

     TVkIndirectCommandsTokenTypeNVX=
      (
       VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX=0,
       VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX=1,
       VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX=2,
       VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX=3,
       VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX=4,
       VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX=5,
       VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX=6,
       VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX=7
      );
     PPVkIndirectCommandsTokenTypeNVX=^PVkIndirectCommandsTokenTypeNVX;
     PVkIndirectCommandsTokenTypeNVX=^TVkIndirectCommandsTokenTypeNVX;

     TVkObjectEntryTypeNVX=
      (
       VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX=0,
       VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX=1,
       VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX=2,
       VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX=3,
       VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX=4
      );
     PPVkObjectEntryTypeNVX=^PVkObjectEntryTypeNVX;
     PVkObjectEntryTypeNVX=^TVkObjectEntryTypeNVX;

     VkDescriptorSetLayoutCreateFlagBits_=
      (
       VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR=0
      );
     TVkDescriptorSetLayoutCreateFlagBit=VkDescriptorSetLayoutCreateFlagBits_;
     PPVkDescriptorSetLayoutCreateFlagBit=^PVkDescriptorSetLayoutCreateFlagBit;
     PVkDescriptorSetLayoutCreateFlagBit=^TVkDescriptorSetLayoutCreateFlagBit;

     TVkDescriptorSetLayoutCreateFlagBits= Set of TVkDescriptorSetLayoutCreateFlagBit; // This is SET OF
     PPVkDescriptorSetLayoutCreateFlagBits=^PVkDescriptorSetLayoutCreateFlagBits;
     PVkDescriptorSetLayoutCreateFlagBits=^TVkDescriptorSetLayoutCreateFlagBits;

     VkExternalMemoryHandleTypeFlagBitsKHR_=
      (
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR=0,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR=1,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR=2,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR=3,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR=4,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR=5,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR=6
      );
     TVkExternalMemoryHandleTypeFlagBitsKH=VkExternalMemoryHandleTypeFlagBitsKHR_;
     PPVkExternalMemoryHandleTypeFlagBitsKH=^PVkExternalMemoryHandleTypeFlagBitsKH;
     PVkExternalMemoryHandleTypeFlagBitsKH=^TVkExternalMemoryHandleTypeFlagBitsKH;

     TVkExternalMemoryHandleTypeFlagBitsKHR= Set of TVkExternalMemoryHandleTypeFlagBitsKH; // This is SET OF
     PPVkExternalMemoryHandleTypeFlagBitsKHR=^PVkExternalMemoryHandleTypeFlagBitsKHR;
     PVkExternalMemoryHandleTypeFlagBitsKHR=^TVkExternalMemoryHandleTypeFlagBitsKHR;

     VkExternalMemoryFeatureFlagBitsKHR_=
      (
       VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR=0,
       VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR=1,
       VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR=2
      );
     TVkExternalMemoryFeatureFlagBitsKH=VkExternalMemoryFeatureFlagBitsKHR_;
     PPVkExternalMemoryFeatureFlagBitsKH=^PVkExternalMemoryFeatureFlagBitsKH;
     PVkExternalMemoryFeatureFlagBitsKH=^TVkExternalMemoryFeatureFlagBitsKH;

     TVkExternalMemoryFeatureFlagBitsKHR= Set of TVkExternalMemoryFeatureFlagBitsKH; // This is SET OF
     PPVkExternalMemoryFeatureFlagBitsKHR=^PVkExternalMemoryFeatureFlagBitsKHR;
     PVkExternalMemoryFeatureFlagBitsKHR=^TVkExternalMemoryFeatureFlagBitsKHR;

     VkExternalSemaphoreHandleTypeFlagBitsKHR_=
      (
       VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR=0,
       VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR=1,
       VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR=2,
       VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR=3,
       VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR=4
      );
     TVkExternalSemaphoreHandleTypeFlagBitsKH=VkExternalSemaphoreHandleTypeFlagBitsKHR_;
     PPVkExternalSemaphoreHandleTypeFlagBitsKH=^PVkExternalSemaphoreHandleTypeFlagBitsKH;
     PVkExternalSemaphoreHandleTypeFlagBitsKH=^TVkExternalSemaphoreHandleTypeFlagBitsKH;

     TVkExternalSemaphoreHandleTypeFlagBitsKHR= Set of TVkExternalSemaphoreHandleTypeFlagBitsKH; // This is SET OF
     PPVkExternalSemaphoreHandleTypeFlagBitsKHR=^PVkExternalSemaphoreHandleTypeFlagBitsKHR;
     PVkExternalSemaphoreHandleTypeFlagBitsKHR=^TVkExternalSemaphoreHandleTypeFlagBitsKHR;

     VkExternalSemaphoreFeatureFlagBitsKHR_=
      (
       VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR=0,
       VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR=1
      );
     TVkExternalSemaphoreFeatureFlagBitsKH=VkExternalSemaphoreFeatureFlagBitsKHR_;
     PPVkExternalSemaphoreFeatureFlagBitsKH=^PVkExternalSemaphoreFeatureFlagBitsKH;
     PVkExternalSemaphoreFeatureFlagBitsKH=^TVkExternalSemaphoreFeatureFlagBitsKH;

     TVkExternalSemaphoreFeatureFlagBitsKHR= Set of TVkExternalSemaphoreFeatureFlagBitsKH; // This is SET OF
     PPVkExternalSemaphoreFeatureFlagBitsKHR=^PVkExternalSemaphoreFeatureFlagBitsKHR;
     PVkExternalSemaphoreFeatureFlagBitsKHR=^TVkExternalSemaphoreFeatureFlagBitsKHR;

     VkSemaphoreImportFlagBitsKHR_=
      (
       VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR=0
      );
     TVkSemaphoreImportFlagBitsKH=VkSemaphoreImportFlagBitsKHR_;
     PPVkSemaphoreImportFlagBitsKH=^PVkSemaphoreImportFlagBitsKH;
     PVkSemaphoreImportFlagBitsKH=^TVkSemaphoreImportFlagBitsKH;

     TVkSemaphoreImportFlagBitsKHR= Set of TVkSemaphoreImportFlagBitsKH; // This is SET OF
     PPVkSemaphoreImportFlagBitsKHR=^PVkSemaphoreImportFlagBitsKHR;
     PVkSemaphoreImportFlagBitsKHR=^TVkSemaphoreImportFlagBitsKHR;

     VkExternalFenceHandleTypeFlagBitsKHR_=
      (
       VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR=0,
       VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR=1,
       VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR=2,
       VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR=3
      );
     TVkExternalFenceHandleTypeFlagBitsKH=VkExternalFenceHandleTypeFlagBitsKHR_;
     PPVkExternalFenceHandleTypeFlagBitsKH=^PVkExternalFenceHandleTypeFlagBitsKH;
     PVkExternalFenceHandleTypeFlagBitsKH=^TVkExternalFenceHandleTypeFlagBitsKH;

     TVkExternalFenceHandleTypeFlagBitsKHR= Set of TVkExternalFenceHandleTypeFlagBitsKH; // This is SET OF
     PPVkExternalFenceHandleTypeFlagBitsKHR=^PVkExternalFenceHandleTypeFlagBitsKHR;
     PVkExternalFenceHandleTypeFlagBitsKHR=^TVkExternalFenceHandleTypeFlagBitsKHR;

     VkExternalFenceFeatureFlagBitsKHR_=
      (
       VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR=0,
       VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR=1
      );
     TVkExternalFenceFeatureFlagBitsKH=VkExternalFenceFeatureFlagBitsKHR_;
     PPVkExternalFenceFeatureFlagBitsKH=^PVkExternalFenceFeatureFlagBitsKH;
     PVkExternalFenceFeatureFlagBitsKH=^TVkExternalFenceFeatureFlagBitsKH;

     TVkExternalFenceFeatureFlagBitsKHR= Set of TVkExternalFenceFeatureFlagBitsKH; // This is SET OF
     PPVkExternalFenceFeatureFlagBitsKHR=^PVkExternalFenceFeatureFlagBitsKHR;
     PVkExternalFenceFeatureFlagBitsKHR=^TVkExternalFenceFeatureFlagBitsKHR;

     VkFenceImportFlagBitsKHR_=
      (
       VK_FENCE_IMPORT_TEMPORARY_BIT_KHR=0
      );
     TVkFenceImportFlagBitsKH=VkFenceImportFlagBitsKHR_;
     PPVkFenceImportFlagBitsKH=^PVkFenceImportFlagBitsKH;
     PVkFenceImportFlagBitsKH=^TVkFenceImportFlagBitsKH;

     TVkFenceImportFlagBitsKHR= Set of TVkFenceImportFlagBitsKH; // This is SET OF
     PPVkFenceImportFlagBitsKHR=^PVkFenceImportFlagBitsKHR;
     PVkFenceImportFlagBitsKHR=^TVkFenceImportFlagBitsKHR;

     VkSurfaceCounterFlagBitsEXT_=
      (
       VK_SURFACE_COUNTER_VBLANK_EXT=0
      );
     TVkSurfaceCounterFlagBitsEX=VkSurfaceCounterFlagBitsEXT_;
     PPVkSurfaceCounterFlagBitsEX=^PVkSurfaceCounterFlagBitsEX;
     PVkSurfaceCounterFlagBitsEX=^TVkSurfaceCounterFlagBitsEX;

     TVkSurfaceCounterFlagBitsEXT= Set of TVkSurfaceCounterFlagBitsEX; // This is SET OF
     PPVkSurfaceCounterFlagBitsEXT=^PVkSurfaceCounterFlagBitsEXT;
     PVkSurfaceCounterFlagBitsEXT=^TVkSurfaceCounterFlagBitsEXT;

     TVkDisplayPowerStateEXT=
      (
       VK_DISPLAY_POWER_STATE_OFF_EXT=0,
       VK_DISPLAY_POWER_STATE_SUSPEND_EXT=1,
       VK_DISPLAY_POWER_STATE_ON_EXT=2
      );
     PPVkDisplayPowerStateEXT=^PVkDisplayPowerStateEXT;
     PVkDisplayPowerStateEXT=^TVkDisplayPowerStateEXT;

     TVkDeviceEventTypeEXT=
      (
       VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT=0
      );
     PPVkDeviceEventTypeEXT=^PVkDeviceEventTypeEXT;
     PVkDeviceEventTypeEXT=^TVkDeviceEventTypeEXT;

     TVkDisplayEventTypeEXT=
      (
       VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT=0
      );
     PPVkDisplayEventTypeEXT=^PVkDisplayEventTypeEXT;
     PVkDisplayEventTypeEXT=^TVkDisplayEventTypeEXT;

     VkPeerMemoryFeatureFlagBitsKHX_=
      (
       VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX=0,                                // Can read with vkCmdCopy commands
       VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX=1,                                // Can write with vkCmdCopy commands
       VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX=2,                             // Can read with any access type/command
       VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX=3                              // Can write with and access type/command
      );
     TVkPeerMemoryFeatureFlagBitsKH=VkPeerMemoryFeatureFlagBitsKHX_;
     PPVkPeerMemoryFeatureFlagBitsKH=^PVkPeerMemoryFeatureFlagBitsKH;
     PVkPeerMemoryFeatureFlagBitsKH=^TVkPeerMemoryFeatureFlagBitsKH;

     TVkPeerMemoryFeatureFlagBitsKHX= Set of TVkPeerMemoryFeatureFlagBitsKH; // This is SET OF
     PPVkPeerMemoryFeatureFlagBitsKHX=^PVkPeerMemoryFeatureFlagBitsKHX;
     PVkPeerMemoryFeatureFlagBitsKHX=^TVkPeerMemoryFeatureFlagBitsKHX;

     VkMemoryAllocateFlagBitsKHX_=
      (
       VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX=0                                  // Force allocation on specific devices
      );
     TVkMemoryAllocateFlagBitsKH=VkMemoryAllocateFlagBitsKHX_;
     PPVkMemoryAllocateFlagBitsKH=^PVkMemoryAllocateFlagBitsKH;
     PVkMemoryAllocateFlagBitsKH=^TVkMemoryAllocateFlagBitsKH;

     TVkMemoryAllocateFlagBitsKHX= Set of TVkMemoryAllocateFlagBitsKH; // This is SET OF
     PPVkMemoryAllocateFlagBitsKHX=^PVkMemoryAllocateFlagBitsKHX;
     PVkMemoryAllocateFlagBitsKHX=^TVkMemoryAllocateFlagBitsKHX;

     VkDeviceGroupPresentModeFlagBitsKHX_=
      (
       VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX=0,                             // Present from local memory
       VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX=1,                            // Present from remote memory
       VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX=2,                               // Present sum of local and/or remote memory
       VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX=3                 // Each physical device presents from local memory
      );
     TVkDeviceGroupPresentModeFlagBitsKH=VkDeviceGroupPresentModeFlagBitsKHX_;
     PPVkDeviceGroupPresentModeFlagBitsKH=^PVkDeviceGroupPresentModeFlagBitsKH;
     PVkDeviceGroupPresentModeFlagBitsKH=^TVkDeviceGroupPresentModeFlagBitsKH;

     TVkDeviceGroupPresentModeFlagBitsKHX= Set of TVkDeviceGroupPresentModeFlagBitsKH; // This is SET OF
     PPVkDeviceGroupPresentModeFlagBitsKHX=^PVkDeviceGroupPresentModeFlagBitsKHX;
     PVkDeviceGroupPresentModeFlagBitsKHX=^TVkDeviceGroupPresentModeFlagBitsKHX;

     VkSwapchainCreateFlagBitsKHR_=
      (
       VK_SWAPCHAIN_CREATE_BIND_SFR_BIT_KHX=0
      );
     TVkSwapchainCreateFlagBitsKH=VkSwapchainCreateFlagBitsKHR_;
     PPVkSwapchainCreateFlagBitsKH=^PVkSwapchainCreateFlagBitsKH;
     PVkSwapchainCreateFlagBitsKH=^TVkSwapchainCreateFlagBitsKH;

     TVkSwapchainCreateFlagBitsKHR= Set of TVkSwapchainCreateFlagBitsKH; // This is SET OF
     PPVkSwapchainCreateFlagBitsKHR=^PVkSwapchainCreateFlagBitsKHR;
     PVkSwapchainCreateFlagBitsKHR=^TVkSwapchainCreateFlagBitsKHR;

     TVkViewportCoordinateSwizzleNV=
      (
       VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV=0,
       VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV=1,
       VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV=2,
       VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV=3,
       VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV=4,
       VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV=5,
       VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV=6,
       VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV=7
      );
     PPVkViewportCoordinateSwizzleNV=^PVkViewportCoordinateSwizzleNV;
     PVkViewportCoordinateSwizzleNV=^TVkViewportCoordinateSwizzleNV;

     TVkDiscardRectangleModeEXT=
      (
       VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT=0,
       VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT=1
      );
     PPVkDiscardRectangleModeEXT=^PVkDiscardRectangleModeEXT;
     PVkDiscardRectangleModeEXT=^TVkDiscardRectangleModeEXT;

     VkSubpassDescriptionFlagBits_=
      (
       VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX=0,
       VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX=1
      );
     TVkSubpassDescriptionFlagBit=VkSubpassDescriptionFlagBits_;
     PPVkSubpassDescriptionFlagBit=^PVkSubpassDescriptionFlagBit;
     PVkSubpassDescriptionFlagBit=^TVkSubpassDescriptionFlagBit;

     TVkSubpassDescriptionFlagBits= Set of TVkSubpassDescriptionFlagBit; // This is SET OF
     PPVkSubpassDescriptionFlagBits=^PVkSubpassDescriptionFlagBits;
     PVkSubpassDescriptionFlagBits=^TVkSubpassDescriptionFlagBits;

     TVkSamplerReductionModeEXT=
      (
       VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT=0,
       VK_SAMPLER_REDUCTION_MODE_MIN_EXT=1,
       VK_SAMPLER_REDUCTION_MODE_MAX_EXT=2
      );
     PPVkSamplerReductionModeEXT=^PVkSamplerReductionModeEXT;
     PVkSamplerReductionModeEXT=^TVkSamplerReductionModeEXT;

     TVkBlendOverlapEXT=
      (
       VK_BLEND_OVERLAP_UNCORRELATED_EXT=0,
       VK_BLEND_OVERLAP_DISJOINT_EXT=1,
       VK_BLEND_OVERLAP_CONJOINT_EXT=2
      );
     PPVkBlendOverlapEXT=^PVkBlendOverlapEXT;
     PVkBlendOverlapEXT=^TVkBlendOverlapEXT;

     TVkCoverageModulationModeNV=
      (
       VK_COVERAGE_MODULATION_MODE_NONE_NV=0,
       VK_COVERAGE_MODULATION_MODE_RGB_NV=1,
       VK_COVERAGE_MODULATION_MODE_ALPHA_NV=2,
       VK_COVERAGE_MODULATION_MODE_RGBA_NV=3
      );
     PPVkCoverageModulationModeNV=^PVkCoverageModulationModeNV;
     PVkCoverageModulationModeNV=^TVkCoverageModulationModeNV;

     TVkDescriptorSetLayoutCreateFlags=TVkDescriptorSetLayoutCreateFlagBits;
     PPVkDescriptorSetLayoutCreateFlags=^PVkDescriptorSetLayoutCreateFlags;
     PVkDescriptorSetLayoutCreateFlags=^TVkDescriptorSetLayoutCreateFlags;

     TVkQueueFlags=TVkQueueFlagBits;
     PPVkQueueFlags=^PVkQueueFlags;
     PVkQueueFlags=^TVkQueueFlags;

     TVkMemoryPropertyFlags=TVkMemoryPropertyFlagBits;
     PPVkMemoryPropertyFlags=^PVkMemoryPropertyFlags;
     PVkMemoryPropertyFlags=^TVkMemoryPropertyFlags;

     TVkMemoryHeapFlags=TVkMemoryHeapFlagBits;
     PPVkMemoryHeapFlags=^PVkMemoryHeapFlags;
     PVkMemoryHeapFlags=^TVkMemoryHeapFlags;

     TVkAccessFlags=TVkAccessFlagBits;
     PPVkAccessFlags=^PVkAccessFlags;
     PVkAccessFlags=^TVkAccessFlags;

     TVkBufferUsageFlags=TVkBufferUsageFlagBits;
     PPVkBufferUsageFlags=^PVkBufferUsageFlags;
     PVkBufferUsageFlags=^TVkBufferUsageFlags;

     TVkBufferCreateFlags=TVkBufferCreateFlagBits;
     PPVkBufferCreateFlags=^PVkBufferCreateFlags;
     PVkBufferCreateFlags=^TVkBufferCreateFlags;

     TVkShaderStageFlags=TVkShaderStageFlagBits;
     PPVkShaderStageFlags=^PVkShaderStageFlags;
     PVkShaderStageFlags=^TVkShaderStageFlags;

     TVkImageUsageFlags=TVkImageUsageFlagBits;
     PPVkImageUsageFlags=^PVkImageUsageFlags;
     PVkImageUsageFlags=^TVkImageUsageFlags;

     TVkImageCreateFlags=TVkImageCreateFlagBits;
     PPVkImageCreateFlags=^PVkImageCreateFlags;
     PVkImageCreateFlags=^TVkImageCreateFlags;

     TVkPipelineCreateFlags=TVkPipelineCreateFlagBits;
     PPVkPipelineCreateFlags=^PVkPipelineCreateFlags;
     PVkPipelineCreateFlags=^TVkPipelineCreateFlags;

     TVkColorComponentFlags=TVkColorComponentFlagBits;
     PPVkColorComponentFlags=^PVkColorComponentFlags;
     PVkColorComponentFlags=^TVkColorComponentFlags;

     TVkFenceCreateFlags=TVkFenceCreateFlagBits;
     PPVkFenceCreateFlags=^PVkFenceCreateFlags;
     PVkFenceCreateFlags=^TVkFenceCreateFlags;

     TVkFormatFeatureFlags=TVkFormatFeatureFlagBits;
     PPVkFormatFeatureFlags=^PVkFormatFeatureFlags;
     PVkFormatFeatureFlags=^TVkFormatFeatureFlags;

     TVkQueryControlFlags=TVkQueryControlFlagBits;
     PPVkQueryControlFlags=^PVkQueryControlFlags;
     PVkQueryControlFlags=^TVkQueryControlFlags;

     TVkQueryResultFlags=TVkQueryResultFlagBits;
     PPVkQueryResultFlags=^PVkQueryResultFlags;
     PVkQueryResultFlags=^TVkQueryResultFlags;

     TVkCommandPoolCreateFlags=TVkCommandPoolCreateFlagBits;
     PPVkCommandPoolCreateFlags=^PVkCommandPoolCreateFlags;
     PVkCommandPoolCreateFlags=^TVkCommandPoolCreateFlags;

     TVkCommandPoolResetFlags=TVkCommandPoolResetFlagBits;
     PPVkCommandPoolResetFlags=^PVkCommandPoolResetFlags;
     PVkCommandPoolResetFlags=^TVkCommandPoolResetFlags;

     TVkCommandBufferResetFlags=TVkCommandBufferResetFlagBits;
     PPVkCommandBufferResetFlags=^PVkCommandBufferResetFlags;
     PVkCommandBufferResetFlags=^TVkCommandBufferResetFlags;

     TVkCommandBufferUsageFlags=TVkCommandBufferUsageFlagBits;
     PPVkCommandBufferUsageFlags=^PVkCommandBufferUsageFlags;
     PVkCommandBufferUsageFlags=^TVkCommandBufferUsageFlags;

     TVkQueryPipelineStatisticFlags=TVkQueryPipelineStatisticFlagBits;
     PPVkQueryPipelineStatisticFlags=^PVkQueryPipelineStatisticFlags;
     PVkQueryPipelineStatisticFlags=^TVkQueryPipelineStatisticFlags;

     TVkImageAspectFlags=TVkImageAspectFlagBits;
     PPVkImageAspectFlags=^PVkImageAspectFlags;
     PVkImageAspectFlags=^TVkImageAspectFlags;

     TVkSparseMemoryBindFlags=TVkSparseMemoryBindFlagBits;
     PPVkSparseMemoryBindFlags=^PVkSparseMemoryBindFlags;
     PVkSparseMemoryBindFlags=^TVkSparseMemoryBindFlags;

     TVkSparseImageFormatFlags=TVkSparseImageFormatFlagBits;
     PPVkSparseImageFormatFlags=^PVkSparseImageFormatFlags;
     PVkSparseImageFormatFlags=^TVkSparseImageFormatFlags;

     TVkSubpassDescriptionFlags=TVkSubpassDescriptionFlagBits;
     PPVkSubpassDescriptionFlags=^PVkSubpassDescriptionFlags;
     PVkSubpassDescriptionFlags=^TVkSubpassDescriptionFlags;

     TVkPipelineStageFlags=TVkPipelineStageFlagBits;
     PPVkPipelineStageFlags=^PVkPipelineStageFlags;
     PVkPipelineStageFlags=^TVkPipelineStageFlags;

     TVkSampleCountFlags=TVkSampleCountFlagBits;
     PPVkSampleCountFlags=^PVkSampleCountFlags;
     PVkSampleCountFlags=^TVkSampleCountFlags;

     TVkAttachmentDescriptionFlags=TVkAttachmentDescriptionFlagBits;
     PPVkAttachmentDescriptionFlags=^PVkAttachmentDescriptionFlags;
     PVkAttachmentDescriptionFlags=^TVkAttachmentDescriptionFlags;

     TVkStencilFaceFlags=TVkStencilFaceFlagBits;
     PPVkStencilFaceFlags=^PVkStencilFaceFlags;
     PVkStencilFaceFlags=^TVkStencilFaceFlags;

     TVkCullModeFlags=TVkCullModeFlagBits;
     PPVkCullModeFlags=^PVkCullModeFlags;
     PVkCullModeFlags=^TVkCullModeFlags;

     TVkDescriptorPoolCreateFlags=TVkDescriptorPoolCreateFlagBits;
     PPVkDescriptorPoolCreateFlags=^PVkDescriptorPoolCreateFlags;
     PVkDescriptorPoolCreateFlags=^TVkDescriptorPoolCreateFlags;

     TVkDependencyFlags=TVkDependencyFlagBits;
     PPVkDependencyFlags=^PVkDependencyFlags;
     PVkDependencyFlags=^TVkDependencyFlags;

     TVkIndirectCommandsLayoutUsageFlagsNVX=TVkIndirectCommandsLayoutUsageFlagBitsNVX;
     PPVkIndirectCommandsLayoutUsageFlagsNVX=^PVkIndirectCommandsLayoutUsageFlagsNVX;
     PVkIndirectCommandsLayoutUsageFlagsNVX=^TVkIndirectCommandsLayoutUsageFlagsNVX;

     TVkObjectEntryUsageFlagsNVX=TVkObjectEntryUsageFlagBitsNVX;
     PPVkObjectEntryUsageFlagsNVX=^PVkObjectEntryUsageFlagsNVX;
     PVkObjectEntryUsageFlagsNVX=^TVkObjectEntryUsageFlagsNVX;

     TVkCompositeAlphaFlagsKHR=TVkCompositeAlphaFlagBitsKHR;
     PPVkCompositeAlphaFlagsKHR=^PVkCompositeAlphaFlagsKHR;
     PVkCompositeAlphaFlagsKHR=^TVkCompositeAlphaFlagsKHR;

     TVkDisplayPlaneAlphaFlagsKHR=TVkDisplayPlaneAlphaFlagBitsKHR;
     PPVkDisplayPlaneAlphaFlagsKHR=^PVkDisplayPlaneAlphaFlagsKHR;
     PVkDisplayPlaneAlphaFlagsKHR=^TVkDisplayPlaneAlphaFlagsKHR;

     TVkSurfaceTransformFlagsKHR=TVkSurfaceTransformFlagBitsKHR;
     PPVkSurfaceTransformFlagsKHR=^PVkSurfaceTransformFlagsKHR;
     PVkSurfaceTransformFlagsKHR=^TVkSurfaceTransformFlagsKHR;

     TVkSwapchainCreateFlagsKHR=TVkSwapchainCreateFlagBitsKHR;
     PPVkSwapchainCreateFlagsKHR=^PVkSwapchainCreateFlagsKHR;
     PVkSwapchainCreateFlagsKHR=^TVkSwapchainCreateFlagsKHR;

     TVkPeerMemoryFeatureFlagsKHX=TVkPeerMemoryFeatureFlagBitsKHX;
     PPVkPeerMemoryFeatureFlagsKHX=^PVkPeerMemoryFeatureFlagsKHX;
     PVkPeerMemoryFeatureFlagsKHX=^TVkPeerMemoryFeatureFlagsKHX;

     TVkMemoryAllocateFlagsKHX=TVkMemoryAllocateFlagBitsKHX;
     PPVkMemoryAllocateFlagsKHX=^PVkMemoryAllocateFlagsKHX;
     PVkMemoryAllocateFlagsKHX=^TVkMemoryAllocateFlagsKHX;

     TVkDeviceGroupPresentModeFlagsKHX=TVkDeviceGroupPresentModeFlagBitsKHX;
     PPVkDeviceGroupPresentModeFlagsKHX=^PVkDeviceGroupPresentModeFlagsKHX;
     PVkDeviceGroupPresentModeFlagsKHX=^TVkDeviceGroupPresentModeFlagsKHX;

     TVkDebugReportFlagsEXT=TVkDebugReportFlagBitsEXT;
     PPVkDebugReportFlagsEXT=^PVkDebugReportFlagsEXT;
     PVkDebugReportFlagsEXT=^TVkDebugReportFlagsEXT;

     TVkExternalMemoryHandleTypeFlagsNV=TVkExternalMemoryHandleTypeFlagBitsNV;
     PPVkExternalMemoryHandleTypeFlagsNV=^PVkExternalMemoryHandleTypeFlagsNV;
     PVkExternalMemoryHandleTypeFlagsNV=^TVkExternalMemoryHandleTypeFlagsNV;

     TVkExternalMemoryFeatureFlagsNV=TVkExternalMemoryFeatureFlagBitsNV;
     PPVkExternalMemoryFeatureFlagsNV=^PVkExternalMemoryFeatureFlagsNV;
     PVkExternalMemoryFeatureFlagsNV=^TVkExternalMemoryFeatureFlagsNV;

     TVkExternalMemoryHandleTypeFlagsKHR=TVkExternalMemoryHandleTypeFlagBitsKHR;
     PPVkExternalMemoryHandleTypeFlagsKHR=^PVkExternalMemoryHandleTypeFlagsKHR;
     PVkExternalMemoryHandleTypeFlagsKHR=^TVkExternalMemoryHandleTypeFlagsKHR;

     TVkExternalMemoryFeatureFlagsKHR=TVkExternalMemoryFeatureFlagBitsKHR;
     PPVkExternalMemoryFeatureFlagsKHR=^PVkExternalMemoryFeatureFlagsKHR;
     PVkExternalMemoryFeatureFlagsKHR=^TVkExternalMemoryFeatureFlagsKHR;

     TVkExternalSemaphoreHandleTypeFlagsKHR=TVkExternalSemaphoreHandleTypeFlagBitsKHR;
     PPVkExternalSemaphoreHandleTypeFlagsKHR=^PVkExternalSemaphoreHandleTypeFlagsKHR;
     PVkExternalSemaphoreHandleTypeFlagsKHR=^TVkExternalSemaphoreHandleTypeFlagsKHR;

     TVkExternalSemaphoreFeatureFlagsKHR=TVkExternalSemaphoreFeatureFlagBitsKHR;
     PPVkExternalSemaphoreFeatureFlagsKHR=^PVkExternalSemaphoreFeatureFlagsKHR;
     PVkExternalSemaphoreFeatureFlagsKHR=^TVkExternalSemaphoreFeatureFlagsKHR;

     TVkSemaphoreImportFlagsKHR=TVkSemaphoreImportFlagBitsKHR;
     PPVkSemaphoreImportFlagsKHR=^PVkSemaphoreImportFlagsKHR;
     PVkSemaphoreImportFlagsKHR=^TVkSemaphoreImportFlagsKHR;

     TVkExternalFenceHandleTypeFlagsKHR=TVkExternalFenceHandleTypeFlagBitsKHR;
     PPVkExternalFenceHandleTypeFlagsKHR=^PVkExternalFenceHandleTypeFlagsKHR;
     PVkExternalFenceHandleTypeFlagsKHR=^TVkExternalFenceHandleTypeFlagsKHR;

     TVkExternalFenceFeatureFlagsKHR=TVkExternalFenceFeatureFlagBitsKHR;
     PPVkExternalFenceFeatureFlagsKHR=^PVkExternalFenceFeatureFlagsKHR;
     PVkExternalFenceFeatureFlagsKHR=^TVkExternalFenceFeatureFlagsKHR;

     TVkFenceImportFlagsKHR=TVkFenceImportFlagBitsKHR;
     PPVkFenceImportFlagsKHR=^PVkFenceImportFlagsKHR;
     PVkFenceImportFlagsKHR=^TVkFenceImportFlagsKHR;

     TVkSurfaceCounterFlagsEXT=TVkSurfaceCounterFlagBitsEXT;
     PPVkSurfaceCounterFlagsEXT=^PVkSurfaceCounterFlagsEXT;
     PVkSurfaceCounterFlagsEXT=^TVkSurfaceCounterFlagsEXT;

     PPPFN_vkInternalAllocationNotification=^PPFN_vkInternalAllocationNotification;
     PPFN_vkInternalAllocationNotification=^TPFN_vkInternalAllocationNotification;
     TPFN_vkInternalAllocationNotification=procedure(pUserData:PVkVoid;size:TVkSize;allocationType:TVkInternalAllocationType;allocationScope:TVkSystemAllocationScope); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     PPPFN_vkInternalFreeNotification=^PPFN_vkInternalFreeNotification;
     PPFN_vkInternalFreeNotification=^TPFN_vkInternalFreeNotification;
     TPFN_vkInternalFreeNotification=procedure(pUserData:PVkVoid;size:TVkSize;allocationType:TVkInternalAllocationType;allocationScope:TVkSystemAllocationScope); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     PPPFN_vkReallocationFunction=^PPFN_vkReallocationFunction;
     PPFN_vkReallocationFunction=^TPFN_vkReallocationFunction;
     TPFN_vkReallocationFunction=function(pUserData:PVkVoid;pOriginal:PVkVoid;size:TVkSize;alignment:TVkSize;allocationScope:TVkSystemAllocationScope):PVkVoid; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     PPPFN_vkAllocationFunction=^PPFN_vkAllocationFunction;
     PPFN_vkAllocationFunction=^TPFN_vkAllocationFunction;
     TPFN_vkAllocationFunction=function(pUserData:PVkVoid;size:TVkSize;alignment:TVkSize;allocationScope:TVkSystemAllocationScope):PVkVoid; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     PPPFN_vkFreeFunction=^PPFN_vkFreeFunction;
     PPFN_vkFreeFunction=^TPFN_vkFreeFunction;
     TPFN_vkFreeFunction=procedure(pUserData:PVkVoid;pMemory:PVkVoid); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     PPPFN_vkVoidFunction=^PPFN_vkVoidFunction;
     PPFN_vkVoidFunction=^TPFN_vkVoidFunction;
     TPFN_vkVoidFunction=procedure(); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     PPPFN_vkDebugReportCallbackEXT=^PPFN_vkDebugReportCallbackEXT;
     PPFN_vkDebugReportCallbackEXT=^TPFN_vkDebugReportCallbackEXT;
     TPFN_vkDebugReportCallbackEXT=function(flags:TVkDebugReportFlagsEXT;objectType:TVkDebugReportObjectTypeEXT;object_:TVkUInt64;location:TVkSize;messageCode:TVkInt32;const pLayerPrefix:PVkChar;const pMessage:PVkChar;pUserData:PVkVoid):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     PPVkOffset2D=^PVkOffset2D;
     PVkOffset2D=^TVkOffset2D;
     TVkOffset2D=record
       x:TVkInt32;
       y:TVkInt32;
     end;

     PPVkOffset3D=^PVkOffset3D;
     PVkOffset3D=^TVkOffset3D;
     TVkOffset3D=record
       x:TVkInt32;
       y:TVkInt32;
       z:TVkInt32;
     end;

     PPVkExtent2D=^PVkExtent2D;
     PVkExtent2D=^TVkExtent2D;
     TVkExtent2D=record
       width:TVkUInt32;
       height:TVkUInt32;
     end;

     PPVkExtent3D=^PVkExtent3D;
     PVkExtent3D=^TVkExtent3D;
     TVkExtent3D=record
       width:TVkUInt32;
       height:TVkUInt32;
       depth:TVkUInt32;
     end;

     PPVkViewport=^PVkViewport;
     PVkViewport=^TVkViewport;
     TVkViewport=record
       x:TVkFloat;
       y:TVkFloat;
       width:TVkFloat;
       height:TVkFloat;
       minDepth:TVkFloat;
       maxDepth:TVkFloat;
     end;

     PPVkRect2D=^PVkRect2D;
     PVkRect2D=^TVkRect2D;
     TVkRect2D=record
       offset:TVkOffset2D;
       extent:TVkExtent2D;
     end;

     PPVkRect3D=^PVkRect3D;
     PVkRect3D=^TVkRect3D;
     TVkRect3D=record
       offset:TVkOffset3D;
       extent:TVkExtent3D;
     end;

     PPVkClearRect=^PVkClearRect;
     PVkClearRect=^TVkClearRect;
     TVkClearRect=record
       rect:TVkRect2D;
       baseArrayLayer:TVkUInt32;
       layerCount:TVkUInt32;
     end;

     PPVkComponentMapping=^PVkComponentMapping;
     PVkComponentMapping=^TVkComponentMapping;
     TVkComponentMapping=record
       r:TVkComponentSwizzle;
       g:TVkComponentSwizzle;
       b:TVkComponentSwizzle;
       a:TVkComponentSwizzle;
     end;

     PPVkPhysicalDeviceSparseProperties=^PVkPhysicalDeviceSparseProperties;
     PVkPhysicalDeviceSparseProperties=^TVkPhysicalDeviceSparseProperties;
     TVkPhysicalDeviceSparseProperties=record
       residencyStandard2DBlockShape:TVkBool32;
       residencyStandard2DMultisampleBlockShape:TVkBool32;
       residencyStandard3DBlockShape:TVkBool32;
       residencyAlignedMipSize:TVkBool32;
       residencyNonResidentStrict:TVkBool32;
     end;

     PPVkExtensionProperties=^PVkExtensionProperties;
     PVkExtensionProperties=^TVkExtensionProperties;
     TVkExtensionProperties=record
       extensionName:array[0..VK_MAX_EXTENSION_NAME_SIZE-1] of TVkChar;
       specVersion:TVkUInt32;
     end;

     PPVkLayerProperties=^PVkLayerProperties;
     PVkLayerProperties=^TVkLayerProperties;
     TVkLayerProperties=record
       layerName:array[0..VK_MAX_EXTENSION_NAME_SIZE-1] of TVkChar;
       specVersion:TVkUInt32;
       implementationVersion:TVkUInt32;
       description:array[0..VK_MAX_DESCRIPTION_SIZE-1] of TVkChar;
     end;

     PPVkApplicationInfo=^PVkApplicationInfo;
     PVkApplicationInfo=^TVkApplicationInfo;
     TVkApplicationInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_APPLICATION_INFO
       pNext:PVkVoid;
       pApplicationName:PVkChar;
       applicationVersion:TVkUInt32;
       pEngineName:PVkChar;
       engineVersion:TVkUInt32;
       apiVersion:TVkUInt32;
     end;

     PPVkAllocationCallbacks=^PVkAllocationCallbacks;
     PVkAllocationCallbacks=^TVkAllocationCallbacks;
     TVkAllocationCallbacks=record
       pUserData:PVkVoid;
       pfnAllocation:TPFN_vkAllocationFunction;
       pfnReallocation:TPFN_vkReallocationFunction;
       pfnFree:TPFN_vkFreeFunction;
       pfnInternalAllocation:TPFN_vkInternalAllocationNotification;
       pfnInternalFree:TPFN_vkInternalFreeNotification;
     end;

     PPVkDeviceQueueCreateInfo=^PVkDeviceQueueCreateInfo;
     PVkDeviceQueueCreateInfo=^TVkDeviceQueueCreateInfo;
     TVkDeviceQueueCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkDeviceQueueCreateFlags;
       queueFamilyIndex:TVkUInt32;
       queueCount:TVkUInt32;
       pQueuePriorities:PVkFloat;
     end;

     PPVkPhysicalDeviceFeatures=^PVkPhysicalDeviceFeatures;
     PVkPhysicalDeviceFeatures=^TVkPhysicalDeviceFeatures;
     TVkPhysicalDeviceFeatures=record
       robustBufferAccess:TVkBool32;
       fullDrawIndexUint32:TVkBool32;
       imageCubeArray:TVkBool32;
       independentBlend:TVkBool32;
       geometryShader:TVkBool32;
       tessellationShader:TVkBool32;
       sampleRateShading:TVkBool32;
       dualSrcBlend:TVkBool32;
       logicOp:TVkBool32;
       multiDrawIndirect:TVkBool32;
       drawIndirectFirstInstance:TVkBool32;
       depthClamp:TVkBool32;
       depthBiasClamp:TVkBool32;
       fillModeNonSolid:TVkBool32;
       depthBounds:TVkBool32;
       wideLines:TVkBool32;
       largePoints:TVkBool32;
       alphaToOne:TVkBool32;
       multiViewport:TVkBool32;
       samplerAnisotropy:TVkBool32;
       textureCompressionETC2:TVkBool32;
       textureCompressionASTC_LDR:TVkBool32;
       textureCompressionBC:TVkBool32;
       occlusionQueryPrecise:TVkBool32;
       pipelineStatisticsQuery:TVkBool32;
       vertexPipelineStoresAndAtomics:TVkBool32;
       fragmentStoresAndAtomics:TVkBool32;
       shaderTessellationAndGeometryPointSize:TVkBool32;
       shaderImageGatherExtended:TVkBool32;
       shaderStorageImageExtendedFormats:TVkBool32;
       shaderStorageImageMultisample:TVkBool32;
       shaderStorageImageReadWithoutFormat:TVkBool32;
       shaderStorageImageWriteWithoutFormat:TVkBool32;
       shaderUniformBufferArrayDynamicIndexing:TVkBool32;
       shaderSampledImageArrayDynamicIndexing:TVkBool32;
       shaderStorageBufferArrayDynamicIndexing:TVkBool32;
       shaderStorageImageArrayDynamicIndexing:TVkBool32;
       shaderClipDistance:TVkBool32;
       shaderCullDistance:TVkBool32;
       shaderFloat64:TVkBool32;
       shaderInt64:TVkBool32;
       shaderInt16:TVkBool32;
       shaderResourceResidency:TVkBool32;
       shaderResourceMinLod:TVkBool32;
       sparseBinding:TVkBool32;
       sparseResidencyBuffer:TVkBool32;
       sparseResidencyImage2D:TVkBool32;
       sparseResidencyImage3D:TVkBool32;
       sparseResidency2Samples:TVkBool32;
       sparseResidency4Samples:TVkBool32;
       sparseResidency8Samples:TVkBool32;
       sparseResidency16Samples:TVkBool32;
       sparseResidencyAliased:TVkBool32;
       variableMultisampleRate:TVkBool32;
       inheritedQueries:TVkBool32;
     end;

     PPVkInstanceCreateInfo=^PVkInstanceCreateInfo;
     PVkInstanceCreateInfo=^TVkInstanceCreateInfo;
     TVkInstanceCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkInstanceCreateFlags;
       pApplicationInfo:PVkApplicationInfo;
       enabledLayerCount:TVkUInt32;
       ppEnabledLayerNames:PPVkChar;
       enabledExtensionCount:TVkUInt32;
       ppEnabledExtensionNames:PPVkChar;
     end;

     PPVkQueueFamilyProperties=^PVkQueueFamilyProperties;
     PVkQueueFamilyProperties=^TVkQueueFamilyProperties;
     TVkQueueFamilyProperties=record
       queueFlags:TVkQueueFlags;
       queueCount:TVkUInt32;
       timestampValidBits:TVkUInt32;
       minImageTransferGranularity:TVkExtent3D;
     end;

     PPVkMemoryType=^PVkMemoryType;
     PVkMemoryType=^TVkMemoryType;
     TVkMemoryType=record
       propertyFlags:TVkMemoryPropertyFlags;
       heapIndex:TVkUInt32;
     end;

     PPVkMemoryAllocateInfo=^PVkMemoryAllocateInfo;
     PVkMemoryAllocateInfo=^TVkMemoryAllocateInfo;
     TVkMemoryAllocateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
       pNext:PVkVoid;
       allocationSize:TVkDeviceSize;
       memoryTypeIndex:TVkUInt32;
     end;

     PPVkMemoryRequirements=^PVkMemoryRequirements;
     PVkMemoryRequirements=^TVkMemoryRequirements;
     TVkMemoryRequirements=record
       size:TVkDeviceSize;
       alignment:TVkDeviceSize;
       memoryTypeBits:TVkUInt32;
     end;

     PPVkSparseImageFormatProperties=^PVkSparseImageFormatProperties;
     PVkSparseImageFormatProperties=^TVkSparseImageFormatProperties;
     TVkSparseImageFormatProperties=record
       aspectMask:TVkImageAspectFlags;
       imageGranularity:TVkExtent3D;
       flags:TVkSparseImageFormatFlags;
     end;

     PPVkSparseImageMemoryRequirements=^PVkSparseImageMemoryRequirements;
     PVkSparseImageMemoryRequirements=^TVkSparseImageMemoryRequirements;
     TVkSparseImageMemoryRequirements=record
       formatProperties:TVkSparseImageFormatProperties;
       imageMipTailFirstLod:TVkUInt32;
       imageMipTailSize:TVkDeviceSize;
       imageMipTailOffset:TVkDeviceSize;
       imageMipTailStride:TVkDeviceSize;
     end;

     PPVkMemoryHeap=^PVkMemoryHeap;
     PVkMemoryHeap=^TVkMemoryHeap;
     TVkMemoryHeap=record
       size:TVkDeviceSize;
       flags:TVkMemoryHeapFlags;
     end;

     PPVkPhysicalDeviceMemoryProperties=^PVkPhysicalDeviceMemoryProperties;
     PVkPhysicalDeviceMemoryProperties=^TVkPhysicalDeviceMemoryProperties;
     TVkPhysicalDeviceMemoryProperties=record
       memoryTypeCount:TVkUInt32;
       memoryTypes:array[0..VK_MAX_MEMORY_TYPES-1] of TVkMemoryType;
       memoryHeapCount:TVkUInt32;
       memoryHeaps:array[0..VK_MAX_MEMORY_HEAPS-1] of TVkMemoryHeap;
     end;

     PPVkMappedMemoryRange=^PVkMappedMemoryRange;
     PVkMappedMemoryRange=^TVkMappedMemoryRange;
     TVkMappedMemoryRange=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
       pNext:PVkVoid;
       memory:TVkDeviceMemory;
       offset:TVkDeviceSize;
       size:TVkDeviceSize;
     end;

     PPVkFormatProperties=^PVkFormatProperties;
     PVkFormatProperties=^TVkFormatProperties;
     TVkFormatProperties=record
       linearTilingFeatures:TVkFormatFeatureFlags;
       optimalTilingFeatures:TVkFormatFeatureFlags;
       bufferFeatures:TVkFormatFeatureFlags;
     end;

     PPVkImageFormatProperties=^PVkImageFormatProperties;
     PVkImageFormatProperties=^TVkImageFormatProperties;
     TVkImageFormatProperties=record
       maxExtent:TVkExtent3D;
       maxMipLevels:TVkUInt32;
       maxArrayLayers:TVkUInt32;
       sampleCounts:TVkSampleCountFlags;
       maxResourceSize:TVkDeviceSize;
     end;

     PPVkDescriptorBufferInfo=^PVkDescriptorBufferInfo;
     PVkDescriptorBufferInfo=^TVkDescriptorBufferInfo;
     TVkDescriptorBufferInfo=record
       buffer:TVkBuffer;
       offset:TVkDeviceSize;
       range:TVkDeviceSize;
     end;

     PPVkDescriptorImageInfo=^PVkDescriptorImageInfo;
     PVkDescriptorImageInfo=^TVkDescriptorImageInfo;
     TVkDescriptorImageInfo=record
       sampler:TVkSampler;
       imageView:TVkImageView;
       imageLayout:TVkImageLayout;
     end;

     PPVkWriteDescriptorSet=^PVkWriteDescriptorSet;
     PVkWriteDescriptorSet=^TVkWriteDescriptorSet;
     TVkWriteDescriptorSet=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
       pNext:PVkVoid;
       dstSet:TVkDescriptorSet;
       dstBinding:TVkUInt32;
       dstArrayElement:TVkUInt32;
       descriptorCount:TVkUInt32;
       descriptorType:TVkDescriptorType;
       pImageInfo:PVkDescriptorImageInfo;
       pBufferInfo:PVkDescriptorBufferInfo;
       pTexelBufferView:PVkBufferView;
     end;

     PPVkCopyDescriptorSet=^PVkCopyDescriptorSet;
     PVkCopyDescriptorSet=^TVkCopyDescriptorSet;
     TVkCopyDescriptorSet=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET
       pNext:PVkVoid;
       srcSet:TVkDescriptorSet;
       srcBinding:TVkUInt32;
       srcArrayElement:TVkUInt32;
       dstSet:TVkDescriptorSet;
       dstBinding:TVkUInt32;
       dstArrayElement:TVkUInt32;
       descriptorCount:TVkUInt32;
     end;

     PPVkBufferCreateInfo=^PVkBufferCreateInfo;
     PVkBufferCreateInfo=^TVkBufferCreateInfo;
     TVkBufferCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkBufferCreateFlags;
       size:TVkDeviceSize;
       usage:TVkBufferUsageFlags;
       sharingMode:TVkSharingMode;
       queueFamilyIndexCount:TVkUInt32;
       pQueueFamilyIndices:PVkUInt32;
     end;

     PPVkBufferViewCreateInfo=^PVkBufferViewCreateInfo;
     PVkBufferViewCreateInfo=^TVkBufferViewCreateInfo;
     TVkBufferViewCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkBufferViewCreateFlags;
       buffer:TVkBuffer;
       format:TVkFormat;
       offset:TVkDeviceSize;
       range:TVkDeviceSize;
     end;

     PPVkImageSubresource=^PVkImageSubresource;
     PVkImageSubresource=^TVkImageSubresource;
     TVkImageSubresource=record
       aspectMask:TVkImageAspectFlags;
       mipLevel:TVkUInt32;
       arrayLayer:TVkUInt32;
     end;

     PPVkImageSubresourceLayers=^PVkImageSubresourceLayers;
     PVkImageSubresourceLayers=^TVkImageSubresourceLayers;
     TVkImageSubresourceLayers=record
       aspectMask:TVkImageAspectFlags;
       mipLevel:TVkUInt32;
       baseArrayLayer:TVkUInt32;
       layerCount:TVkUInt32;
     end;

     PPVkImageSubresourceRange=^PVkImageSubresourceRange;
     PVkImageSubresourceRange=^TVkImageSubresourceRange;
     TVkImageSubresourceRange=record
       aspectMask:TVkImageAspectFlags;
       baseMipLevel:TVkUInt32;
       levelCount:TVkUInt32;
       baseArrayLayer:TVkUInt32;
       layerCount:TVkUInt32;
     end;

     PPVkMemoryBarrier=^PVkMemoryBarrier;
     PVkMemoryBarrier=^TVkMemoryBarrier;
     TVkMemoryBarrier=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_MEMORY_BARRIER
       pNext:PVkVoid;
       srcAccessMask:TVkAccessFlags;
       dstAccessMask:TVkAccessFlags;
     end;

     PPVkBufferMemoryBarrier=^PVkBufferMemoryBarrier;
     PVkBufferMemoryBarrier=^TVkBufferMemoryBarrier;
     TVkBufferMemoryBarrier=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER
       pNext:PVkVoid;
       srcAccessMask:TVkAccessFlags;
       dstAccessMask:TVkAccessFlags;
       srcQueueFamilyIndex:TVkUInt32;
       dstQueueFamilyIndex:TVkUInt32;
       buffer:TVkBuffer;
       offset:TVkDeviceSize;
       size:TVkDeviceSize;
     end;

     PPVkImageMemoryBarrier=^PVkImageMemoryBarrier;
     PVkImageMemoryBarrier=^TVkImageMemoryBarrier;
     TVkImageMemoryBarrier=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
       pNext:PVkVoid;
       srcAccessMask:TVkAccessFlags;
       dstAccessMask:TVkAccessFlags;
       oldLayout:TVkImageLayout;
       newLayout:TVkImageLayout;
       srcQueueFamilyIndex:TVkUInt32;
       dstQueueFamilyIndex:TVkUInt32;
       image:TVkImage;
       subresourceRange:TVkImageSubresourceRange;
     end;

     PPVkImageCreateInfo=^PVkImageCreateInfo;
     PVkImageCreateInfo=^TVkImageCreateInfo;
     TVkImageCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkImageCreateFlags;
       imageType:TVkImageType;
       format:TVkFormat;
       extent:TVkExtent3D;
       mipLevels:TVkUInt32;
       arrayLayers:TVkUInt32;
       samples:TVkSampleCountFlagBits;
       tiling:TVkImageTiling;
       usage:TVkImageUsageFlags;
       sharingMode:TVkSharingMode;
       queueFamilyIndexCount:TVkUInt32;
       pQueueFamilyIndices:PVkUInt32;
       initialLayout:TVkImageLayout;
     end;

     PPVkSubresourceLayout=^PVkSubresourceLayout;
     PVkSubresourceLayout=^TVkSubresourceLayout;
     TVkSubresourceLayout=record
       offset:TVkDeviceSize;
       size:TVkDeviceSize;
       rowPitch:TVkDeviceSize;
       arrayPitch:TVkDeviceSize;
       depthPitch:TVkDeviceSize;
     end;

     PPVkImageViewCreateInfo=^PVkImageViewCreateInfo;
     PVkImageViewCreateInfo=^TVkImageViewCreateInfo;
     TVkImageViewCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkImageViewCreateFlags;
       image:TVkImage;
       viewType:TVkImageViewType;
       format:TVkFormat;
       components:TVkComponentMapping;
       subresourceRange:TVkImageSubresourceRange;
     end;

     PPVkBufferCopy=^PVkBufferCopy;
     PVkBufferCopy=^TVkBufferCopy;
     TVkBufferCopy=record
       srcOffset:TVkDeviceSize;
       dstOffset:TVkDeviceSize;
       size:TVkDeviceSize;
     end;

     PPVkSparseMemoryBind=^PVkSparseMemoryBind;
     PVkSparseMemoryBind=^TVkSparseMemoryBind;
     TVkSparseMemoryBind=record
       resourceOffset:TVkDeviceSize;
       size:TVkDeviceSize;
       memory:TVkDeviceMemory;
       memoryOffset:TVkDeviceSize;
       flags:TVkSparseMemoryBindFlags;
     end;

     PPVkSparseImageMemoryBind=^PVkSparseImageMemoryBind;
     PVkSparseImageMemoryBind=^TVkSparseImageMemoryBind;
     TVkSparseImageMemoryBind=record
       subresource:TVkImageSubresource;
       offset:TVkOffset3D;
       extent:TVkExtent3D;
       memory:TVkDeviceMemory;
       memoryOffset:TVkDeviceSize;
       flags:TVkSparseMemoryBindFlags;
     end;

     PPVkSparseBufferMemoryBindInfo=^PVkSparseBufferMemoryBindInfo;
     PVkSparseBufferMemoryBindInfo=^TVkSparseBufferMemoryBindInfo;
     TVkSparseBufferMemoryBindInfo=record
       buffer:TVkBuffer;
       bindCount:TVkUInt32;
       pBinds:PVkSparseMemoryBind;
     end;

     PPVkSparseImageOpaqueMemoryBindInfo=^PVkSparseImageOpaqueMemoryBindInfo;
     PVkSparseImageOpaqueMemoryBindInfo=^TVkSparseImageOpaqueMemoryBindInfo;
     TVkSparseImageOpaqueMemoryBindInfo=record
       image:TVkImage;
       bindCount:TVkUInt32;
       pBinds:PVkSparseMemoryBind;
     end;

     PPVkSparseImageMemoryBindInfo=^PVkSparseImageMemoryBindInfo;
     PVkSparseImageMemoryBindInfo=^TVkSparseImageMemoryBindInfo;
     TVkSparseImageMemoryBindInfo=record
       image:TVkImage;
       bindCount:TVkUInt32;
       pBinds:PVkSparseImageMemoryBind;
     end;

     PPVkBindSparseInfo=^PVkBindSparseInfo;
     PVkBindSparseInfo=^TVkBindSparseInfo;
     TVkBindSparseInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_BIND_SPARSE_INFO
       pNext:PVkVoid;
       waitSemaphoreCount:TVkUInt32;
       pWaitSemaphores:PVkSemaphore;
       bufferBindCount:TVkUInt32;
       pBufferBinds:PVkSparseBufferMemoryBindInfo;
       imageOpaqueBindCount:TVkUInt32;
       pImageOpaqueBinds:PVkSparseImageOpaqueMemoryBindInfo;
       imageBindCount:TVkUInt32;
       pImageBinds:PVkSparseImageMemoryBindInfo;
       signalSemaphoreCount:TVkUInt32;
       pSignalSemaphores:PVkSemaphore;
     end;

     PPVkImageCopy=^PVkImageCopy;
     PVkImageCopy=^TVkImageCopy;
     TVkImageCopy=record
       srcSubresource:TVkImageSubresourceLayers;
       srcOffset:TVkOffset3D;
       dstSubresource:TVkImageSubresourceLayers;
       dstOffset:TVkOffset3D;
       extent:TVkExtent3D;
     end;

     PPVkImageBlit=^PVkImageBlit;
     PVkImageBlit=^TVkImageBlit;
     TVkImageBlit=record
       srcSubresource:TVkImageSubresourceLayers;
       srcOffsets:array[0..1] of TVkOffset3D;
       dstSubresource:TVkImageSubresourceLayers;
       dstOffsets:array[0..1] of TVkOffset3D;
     end;

     PPVkBufferImageCopy=^PVkBufferImageCopy;
     PVkBufferImageCopy=^TVkBufferImageCopy;
     TVkBufferImageCopy=record
       bufferOffset:TVkDeviceSize;
       bufferRowLength:TVkUInt32;
       bufferImageHeight:TVkUInt32;
       imageSubresource:TVkImageSubresourceLayers;
       imageOffset:TVkOffset3D;
       imageExtent:TVkExtent3D;
     end;

     PPVkImageResolve=^PVkImageResolve;
     PVkImageResolve=^TVkImageResolve;
     TVkImageResolve=record
       srcSubresource:TVkImageSubresourceLayers;
       srcOffset:TVkOffset3D;
       dstSubresource:TVkImageSubresourceLayers;
       dstOffset:TVkOffset3D;
       extent:TVkExtent3D;
     end;

     PPVkShaderModuleCreateInfo=^PVkShaderModuleCreateInfo;
     PVkShaderModuleCreateInfo=^TVkShaderModuleCreateInfo;
     TVkShaderModuleCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkShaderModuleCreateFlags;
       codeSize:TVkSize;
       pCode:PVkUInt32;
     end;

     PPVkDescriptorSetLayoutBinding=^PVkDescriptorSetLayoutBinding;
     PVkDescriptorSetLayoutBinding=^TVkDescriptorSetLayoutBinding;
     TVkDescriptorSetLayoutBinding=record
       binding:TVkUInt32;
       descriptorType:TVkDescriptorType;
       descriptorCount:TVkUInt32;
       stageFlags:TVkShaderStageFlags;
       pImmutableSamplers:PVkSampler;
     end;

     PPVkDescriptorSetLayoutCreateInfo=^PVkDescriptorSetLayoutCreateInfo;
     PVkDescriptorSetLayoutCreateInfo=^TVkDescriptorSetLayoutCreateInfo;
     TVkDescriptorSetLayoutCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkDescriptorSetLayoutCreateFlags;
       bindingCount:TVkUInt32;
       pBindings:PVkDescriptorSetLayoutBinding;
     end;

     PPVkDescriptorPoolSize=^PVkDescriptorPoolSize;
     PVkDescriptorPoolSize=^TVkDescriptorPoolSize;
     TVkDescriptorPoolSize=record
       type_:TVkDescriptorType;
       descriptorCount:TVkUInt32;
     end;

     PPVkDescriptorPoolCreateInfo=^PVkDescriptorPoolCreateInfo;
     PVkDescriptorPoolCreateInfo=^TVkDescriptorPoolCreateInfo;
     TVkDescriptorPoolCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkDescriptorPoolCreateFlags;
       maxSets:TVkUInt32;
       poolSizeCount:TVkUInt32;
       pPoolSizes:PVkDescriptorPoolSize;
     end;

     PPVkDescriptorSetAllocateInfo=^PVkDescriptorSetAllocateInfo;
     PVkDescriptorSetAllocateInfo=^TVkDescriptorSetAllocateInfo;
     TVkDescriptorSetAllocateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
       pNext:PVkVoid;
       descriptorPool:TVkDescriptorPool;
       descriptorSetCount:TVkUInt32;
       pSetLayouts:PVkDescriptorSetLayout;
     end;

     PPVkSpecializationMapEntry=^PVkSpecializationMapEntry;
     PVkSpecializationMapEntry=^TVkSpecializationMapEntry;
     TVkSpecializationMapEntry=record
       constantID:TVkUInt32;
       offset:TVkUInt32;
       size:TVkSize;
     end;

     PPVkSpecializationInfo=^PVkSpecializationInfo;
     PVkSpecializationInfo=^TVkSpecializationInfo;
     TVkSpecializationInfo=record
       mapEntryCount:TVkUInt32;
       pMapEntries:PVkSpecializationMapEntry;
       dataSize:TVkSize;
       pData:PVkVoid;
     end;

     PPVkPipelineShaderStageCreateInfo=^PVkPipelineShaderStageCreateInfo;
     PVkPipelineShaderStageCreateInfo=^TVkPipelineShaderStageCreateInfo;
     TVkPipelineShaderStageCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkPipelineShaderStageCreateFlags;
       stage:TVkShaderStageFlagBits;
       module:TVkShaderModule;
       pName:PVkChar;
       pSpecializationInfo:PVkSpecializationInfo;
     end;

     PPVkComputePipelineCreateInfo=^PVkComputePipelineCreateInfo;
     PVkComputePipelineCreateInfo=^TVkComputePipelineCreateInfo;
     TVkComputePipelineCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkPipelineCreateFlags;
       stage:TVkPipelineShaderStageCreateInfo;
       layout:TVkPipelineLayout;
       basePipelineHandle:TVkPipeline;
       basePipelineIndex:TVkInt32;
     end;

     PPVkVertexInputBindingDescription=^PVkVertexInputBindingDescription;
     PVkVertexInputBindingDescription=^TVkVertexInputBindingDescription;
     TVkVertexInputBindingDescription=record
       binding:TVkUInt32;
       stride:TVkUInt32;
       inputRate:TVkVertexInputRate;
     end;

     PPVkVertexInputAttributeDescription=^PVkVertexInputAttributeDescription;
     PVkVertexInputAttributeDescription=^TVkVertexInputAttributeDescription;
     TVkVertexInputAttributeDescription=record
       location:TVkUInt32;
       binding:TVkUInt32;
       format:TVkFormat;
       offset:TVkUInt32;
     end;

     PPVkPipelineVertexInputStateCreateInfo=^PVkPipelineVertexInputStateCreateInfo;
     PVkPipelineVertexInputStateCreateInfo=^TVkPipelineVertexInputStateCreateInfo;
     TVkPipelineVertexInputStateCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkPipelineVertexInputStateCreateFlags;
       vertexBindingDescriptionCount:TVkUInt32;
       pVertexBindingDescriptions:PVkVertexInputBindingDescription;
       vertexAttributeDescriptionCount:TVkUInt32;
       pVertexAttributeDescriptions:PVkVertexInputAttributeDescription;
     end;

     PPVkPipelineInputAssemblyStateCreateInfo=^PVkPipelineInputAssemblyStateCreateInfo;
     PVkPipelineInputAssemblyStateCreateInfo=^TVkPipelineInputAssemblyStateCreateInfo;
     TVkPipelineInputAssemblyStateCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkPipelineInputAssemblyStateCreateFlags;
       topology:TVkPrimitiveTopology;
       primitiveRestartEnable:TVkBool32;
     end;

     PPVkPipelineTessellationStateCreateInfo=^PVkPipelineTessellationStateCreateInfo;
     PVkPipelineTessellationStateCreateInfo=^TVkPipelineTessellationStateCreateInfo;
     TVkPipelineTessellationStateCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkPipelineTessellationStateCreateFlags;
       patchControlPoints:TVkUInt32;
     end;

     PPVkPipelineViewportStateCreateInfo=^PVkPipelineViewportStateCreateInfo;
     PVkPipelineViewportStateCreateInfo=^TVkPipelineViewportStateCreateInfo;
     TVkPipelineViewportStateCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkPipelineViewportStateCreateFlags;
       viewportCount:TVkUInt32;
       pViewports:PVkViewport;
       scissorCount:TVkUInt32;
       pScissors:PVkRect2D;
     end;

     PPVkPipelineRasterizationStateCreateInfo=^PVkPipelineRasterizationStateCreateInfo;
     PVkPipelineRasterizationStateCreateInfo=^TVkPipelineRasterizationStateCreateInfo;
     TVkPipelineRasterizationStateCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkPipelineRasterizationStateCreateFlags;
       depthClampEnable:TVkBool32;
       rasterizerDiscardEnable:TVkBool32;
       polygonMode:TVkPolygonMode;
       cullMode:TVkCullModeFlags;
       frontFace:TVkFrontFace;
       depthBiasEnable:TVkBool32;
       depthBiasConstantFactor:TVkFloat;
       depthBiasClamp:TVkFloat;
       depthBiasSlopeFactor:TVkFloat;
       lineWidth:TVkFloat;
     end;

     PPVkPipelineMultisampleStateCreateInfo=^PVkPipelineMultisampleStateCreateInfo;
     PVkPipelineMultisampleStateCreateInfo=^TVkPipelineMultisampleStateCreateInfo;
     TVkPipelineMultisampleStateCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkPipelineMultisampleStateCreateFlags;
       rasterizationSamples:TVkSampleCountFlagBits;
       sampleShadingEnable:TVkBool32;
       minSampleShading:TVkFloat;
       pSampleMask:PVkSampleMask;
       alphaToCoverageEnable:TVkBool32;
       alphaToOneEnable:TVkBool32;
     end;

     PPVkPipelineColorBlendAttachmentState=^PVkPipelineColorBlendAttachmentState;
     PVkPipelineColorBlendAttachmentState=^TVkPipelineColorBlendAttachmentState;
     TVkPipelineColorBlendAttachmentState=record
       blendEnable:TVkBool32;
       srcColorBlendFactor:TVkBlendFactor;
       dstColorBlendFactor:TVkBlendFactor;
       colorBlendOp:TVkBlendOp;
       srcAlphaBlendFactor:TVkBlendFactor;
       dstAlphaBlendFactor:TVkBlendFactor;
       alphaBlendOp:TVkBlendOp;
       colorWriteMask:TVkColorComponentFlags;
     end;

     PPVkPipelineColorBlendStateCreateInfo=^PVkPipelineColorBlendStateCreateInfo;
     PVkPipelineColorBlendStateCreateInfo=^TVkPipelineColorBlendStateCreateInfo;
     TVkPipelineColorBlendStateCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkPipelineColorBlendStateCreateFlags;
       logicOpEnable:TVkBool32;
       logicOp:TVkLogicOp;
       attachmentCount:TVkUInt32;
       pAttachments:PVkPipelineColorBlendAttachmentState;
       blendConstants:array[0..3] of TVkFloat;
     end;

     PPVkPipelineDynamicStateCreateInfo=^PVkPipelineDynamicStateCreateInfo;
     PVkPipelineDynamicStateCreateInfo=^TVkPipelineDynamicStateCreateInfo;
     TVkPipelineDynamicStateCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkPipelineDynamicStateCreateFlags;
       dynamicStateCount:TVkUInt32;
       pDynamicStates:PVkDynamicState;
     end;

     PPVkStencilOpState=^PVkStencilOpState;
     PVkStencilOpState=^TVkStencilOpState;
     TVkStencilOpState=record
       failOp:TVkStencilOp;
       passOp:TVkStencilOp;
       depthFailOp:TVkStencilOp;
       compareOp:TVkCompareOp;
       compareMask:TVkUInt32;
       writeMask:TVkUInt32;
       reference:TVkUInt32;
     end;

     PPVkPipelineDepthStencilStateCreateInfo=^PVkPipelineDepthStencilStateCreateInfo;
     PVkPipelineDepthStencilStateCreateInfo=^TVkPipelineDepthStencilStateCreateInfo;
     TVkPipelineDepthStencilStateCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkPipelineDepthStencilStateCreateFlags;
       depthTestEnable:TVkBool32;
       depthWriteEnable:TVkBool32;
       depthCompareOp:TVkCompareOp;
       depthBoundsTestEnable:TVkBool32;
       stencilTestEnable:TVkBool32;
       front:TVkStencilOpState;
       back:TVkStencilOpState;
       minDepthBounds:TVkFloat;
       maxDepthBounds:TVkFloat;
     end;

     PPVkGraphicsPipelineCreateInfo=^PVkGraphicsPipelineCreateInfo;
     PVkGraphicsPipelineCreateInfo=^TVkGraphicsPipelineCreateInfo;
     TVkGraphicsPipelineCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkPipelineCreateFlags;
       stageCount:TVkUInt32;
       pStages:PVkPipelineShaderStageCreateInfo;
       pVertexInputState:PVkPipelineVertexInputStateCreateInfo;
       pInputAssemblyState:PVkPipelineInputAssemblyStateCreateInfo;
       pTessellationState:PVkPipelineTessellationStateCreateInfo;
       pViewportState:PVkPipelineViewportStateCreateInfo;
       pRasterizationState:PVkPipelineRasterizationStateCreateInfo;
       pMultisampleState:PVkPipelineMultisampleStateCreateInfo;
       pDepthStencilState:PVkPipelineDepthStencilStateCreateInfo;
       pColorBlendState:PVkPipelineColorBlendStateCreateInfo;
       pDynamicState:PVkPipelineDynamicStateCreateInfo;
       layout:TVkPipelineLayout;
       renderPass:TVkRenderPass;
       subpass:TVkUInt32;
       basePipelineHandle:TVkPipeline;
       basePipelineIndex:TVkInt32;
     end;

     PPVkPipelineCacheCreateInfo=^PVkPipelineCacheCreateInfo;
     PVkPipelineCacheCreateInfo=^TVkPipelineCacheCreateInfo;
     TVkPipelineCacheCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkPipelineCacheCreateFlags;
       initialDataSize:TVkSize;
       pInitialData:PVkVoid;
     end;

     PPVkPushConstantRange=^PVkPushConstantRange;
     PVkPushConstantRange=^TVkPushConstantRange;
     TVkPushConstantRange=record
       stageFlags:TVkShaderStageFlags;
       offset:TVkUInt32;
       size:TVkUInt32;
     end;

     PPVkPipelineLayoutCreateInfo=^PVkPipelineLayoutCreateInfo;
     PVkPipelineLayoutCreateInfo=^TVkPipelineLayoutCreateInfo;
     TVkPipelineLayoutCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkPipelineLayoutCreateFlags;
       setLayoutCount:TVkUInt32;
       pSetLayouts:PVkDescriptorSetLayout;
       pushConstantRangeCount:TVkUInt32;
       pPushConstantRanges:PVkPushConstantRange;
     end;

     PPVkSamplerCreateInfo=^PVkSamplerCreateInfo;
     PVkSamplerCreateInfo=^TVkSamplerCreateInfo;
     TVkSamplerCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkSamplerCreateFlags;
       magFilter:TVkFilter;
       minFilter:TVkFilter;
       mipmapMode:TVkSamplerMipmapMode;
       addressModeU:TVkSamplerAddressMode;
       addressModeV:TVkSamplerAddressMode;
       addressModeW:TVkSamplerAddressMode;
       mipLodBias:TVkFloat;
       anisotropyEnable:TVkBool32;
       maxAnisotropy:TVkFloat;
       compareEnable:TVkBool32;
       compareOp:TVkCompareOp;
       minLod:TVkFloat;
       maxLod:TVkFloat;
       borderColor:TVkBorderColor;
       unnormalizedCoordinates:TVkBool32;
     end;

     PPVkCommandPoolCreateInfo=^PVkCommandPoolCreateInfo;
     PVkCommandPoolCreateInfo=^TVkCommandPoolCreateInfo;
     TVkCommandPoolCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkCommandPoolCreateFlags;
       queueFamilyIndex:TVkUInt32;
     end;

     PPVkCommandBufferAllocateInfo=^PVkCommandBufferAllocateInfo;
     PVkCommandBufferAllocateInfo=^TVkCommandBufferAllocateInfo;
     TVkCommandBufferAllocateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
       pNext:PVkVoid;
       commandPool:TVkCommandPool;
       level:TVkCommandBufferLevel;
       commandBufferCount:TVkUInt32;
     end;

     PPVkCommandBufferInheritanceInfo=^PVkCommandBufferInheritanceInfo;
     PVkCommandBufferInheritanceInfo=^TVkCommandBufferInheritanceInfo;
     TVkCommandBufferInheritanceInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO
       pNext:PVkVoid;
       renderPass:TVkRenderPass;
       subpass:TVkUInt32;
       framebuffer:TVkFramebuffer;
       occlusionQueryEnable:TVkBool32;
       queryFlags:TVkQueryControlFlags;
       pipelineStatistics:TVkQueryPipelineStatisticFlags;
     end;

     PPVkCommandBufferBeginInfo=^PVkCommandBufferBeginInfo;
     PVkCommandBufferBeginInfo=^TVkCommandBufferBeginInfo;
     TVkCommandBufferBeginInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
       pNext:PVkVoid;
       flags:TVkCommandBufferUsageFlags;
       pInheritanceInfo:PVkCommandBufferInheritanceInfo;
     end;

     PPVkClearColorValue=^PVkClearColorValue;
     PVkClearColorValue=^TVkClearColorValue;
     TVkClearColorValue=record
      case longint of
       0:(
        float32:array[0..3] of TVkFloat;
       );
       1:(
        int32:array[0..3] of TVkInt32;
       );
       2:(
        uint32:array[0..3] of TVkUInt32;
       );
     end;

     PPVkClearDepthStencilValue=^PVkClearDepthStencilValue;
     PVkClearDepthStencilValue=^TVkClearDepthStencilValue;
     TVkClearDepthStencilValue=record
       depth:TVkFloat;
       stencil:TVkUInt32;
     end;

     PPVkClearValue=^PVkClearValue;
     PVkClearValue=^TVkClearValue;
     TVkClearValue=record
      case longint of
       0:(
        color:TVkClearColorValue;
       );
       1:(
        depthStencil:TVkClearDepthStencilValue;
       );
     end;

     PPVkRenderPassBeginInfo=^PVkRenderPassBeginInfo;
     PVkRenderPassBeginInfo=^TVkRenderPassBeginInfo;
     TVkRenderPassBeginInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
       pNext:PVkVoid;
       renderPass:TVkRenderPass;
       framebuffer:TVkFramebuffer;
       renderArea:TVkRect2D;
       clearValueCount:TVkUInt32;
       pClearValues:PVkClearValue;
     end;

     PPVkClearAttachment=^PVkClearAttachment;
     PVkClearAttachment=^TVkClearAttachment;
     TVkClearAttachment=record
       aspectMask:TVkImageAspectFlags;
       colorAttachment:TVkUInt32;
       clearValue:TVkClearValue;
     end;

     PPVkAttachmentDescription=^PVkAttachmentDescription;
     PVkAttachmentDescription=^TVkAttachmentDescription;
     TVkAttachmentDescription=record
       flags:TVkAttachmentDescriptionFlags;
       format:TVkFormat;
       samples:TVkSampleCountFlagBits;
       loadOp:TVkAttachmentLoadOp;
       storeOp:TVkAttachmentStoreOp;
       stencilLoadOp:TVkAttachmentLoadOp;
       stencilStoreOp:TVkAttachmentStoreOp;
       initialLayout:TVkImageLayout;
       finalLayout:TVkImageLayout;
     end;

     PPVkAttachmentReference=^PVkAttachmentReference;
     PVkAttachmentReference=^TVkAttachmentReference;
     TVkAttachmentReference=record
       attachment:TVkUInt32;
       layout:TVkImageLayout;
     end;

     PPVkSubpassDescription=^PVkSubpassDescription;
     PVkSubpassDescription=^TVkSubpassDescription;
     TVkSubpassDescription=record
       flags:TVkSubpassDescriptionFlags;
       pipelineBindPoint:TVkPipelineBindPoint;
       inputAttachmentCount:TVkUInt32;
       pInputAttachments:PVkAttachmentReference;
       colorAttachmentCount:TVkUInt32;
       pColorAttachments:PVkAttachmentReference;
       pResolveAttachments:PVkAttachmentReference;
       pDepthStencilAttachment:PVkAttachmentReference;
       preserveAttachmentCount:TVkUInt32;
       pPreserveAttachments:PVkUInt32;
     end;

     PPVkSubpassDependency=^PVkSubpassDependency;
     PVkSubpassDependency=^TVkSubpassDependency;
     TVkSubpassDependency=record
       srcSubpass:TVkUInt32;
       dstSubpass:TVkUInt32;
       srcStageMask:TVkPipelineStageFlags;
       dstStageMask:TVkPipelineStageFlags;
       srcAccessMask:TVkAccessFlags;
       dstAccessMask:TVkAccessFlags;
       dependencyFlags:TVkDependencyFlags;
     end;

     PPVkRenderPassCreateInfo=^PVkRenderPassCreateInfo;
     PVkRenderPassCreateInfo=^TVkRenderPassCreateInfo;
     TVkRenderPassCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkRenderPassCreateFlags;
       attachmentCount:TVkUInt32;
       pAttachments:PVkAttachmentDescription;
       subpassCount:TVkUInt32;
       pSubpasses:PVkSubpassDescription;
       dependencyCount:TVkUInt32;
       pDependencies:PVkSubpassDependency;
     end;

     PPVkEventCreateInfo=^PVkEventCreateInfo;
     PVkEventCreateInfo=^TVkEventCreateInfo;
     TVkEventCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_EVENT_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkEventCreateFlags;
     end;

     PPVkFenceCreateInfo=^PVkFenceCreateInfo;
     PVkFenceCreateInfo=^TVkFenceCreateInfo;
     TVkFenceCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkFenceCreateFlags;
     end;

     PPVkDeviceCreateInfo=^PVkDeviceCreateInfo;
     PVkDeviceCreateInfo=^TVkDeviceCreateInfo;
     TVkDeviceCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkDeviceCreateFlags;
       queueCreateInfoCount:TVkUInt32;
       pQueueCreateInfos:PVkDeviceQueueCreateInfo;
       enabledLayerCount:TVkUInt32;
       ppEnabledLayerNames:PPVkChar;
       enabledExtensionCount:TVkUInt32;
       ppEnabledExtensionNames:PPVkChar;
       pEnabledFeatures:PVkPhysicalDeviceFeatures;
     end;

     PPVkPhysicalDeviceLimits=^PVkPhysicalDeviceLimits;
     PVkPhysicalDeviceLimits=^TVkPhysicalDeviceLimits;
     TVkPhysicalDeviceLimits=record
       maxImageDimension1D:TVkUInt32;
       maxImageDimension2D:TVkUInt32;
       maxImageDimension3D:TVkUInt32;
       maxImageDimensionCube:TVkUInt32;
       maxImageArrayLayers:TVkUInt32;
       maxTexelBufferElements:TVkUInt32;
       maxUniformBufferRange:TVkUInt32;
       maxStorageBufferRange:TVkUInt32;
       maxPushConstantsSize:TVkUInt32;
       maxMemoryAllocationCount:TVkUInt32;
       maxSamplerAllocationCount:TVkUInt32;
       bufferImageGranularity:TVkDeviceSize;
       sparseAddressSpaceSize:TVkDeviceSize;
       maxBoundDescriptorSets:TVkUInt32;
       maxPerStageDescriptorSamplers:TVkUInt32;
       maxPerStageDescriptorUniformBuffers:TVkUInt32;
       maxPerStageDescriptorStorageBuffers:TVkUInt32;
       maxPerStageDescriptorSampledImages:TVkUInt32;
       maxPerStageDescriptorStorageImages:TVkUInt32;
       maxPerStageDescriptorInputAttachments:TVkUInt32;
       maxPerStageResources:TVkUInt32;
       maxDescriptorSetSamplers:TVkUInt32;
       maxDescriptorSetUniformBuffers:TVkUInt32;
       maxDescriptorSetUniformBuffersDynamic:TVkUInt32;
       maxDescriptorSetStorageBuffers:TVkUInt32;
       maxDescriptorSetStorageBuffersDynamic:TVkUInt32;
       maxDescriptorSetSampledImages:TVkUInt32;
       maxDescriptorSetStorageImages:TVkUInt32;
       maxDescriptorSetInputAttachments:TVkUInt32;
       maxVertexInputAttributes:TVkUInt32;
       maxVertexInputBindings:TVkUInt32;
       maxVertexInputAttributeOffset:TVkUInt32;
       maxVertexInputBindingStride:TVkUInt32;
       maxVertexOutputComponents:TVkUInt32;
       maxTessellationGenerationLevel:TVkUInt32;
       maxTessellationPatchSize:TVkUInt32;
       maxTessellationControlPerVertexInputComponents:TVkUInt32;
       maxTessellationControlPerVertexOutputComponents:TVkUInt32;
       maxTessellationControlPerPatchOutputComponents:TVkUInt32;
       maxTessellationControlTotalOutputComponents:TVkUInt32;
       maxTessellationEvaluationInputComponents:TVkUInt32;
       maxTessellationEvaluationOutputComponents:TVkUInt32;
       maxGeometryShaderInvocations:TVkUInt32;
       maxGeometryInputComponents:TVkUInt32;
       maxGeometryOutputComponents:TVkUInt32;
       maxGeometryOutputVertices:TVkUInt32;
       maxGeometryTotalOutputComponents:TVkUInt32;
       maxFragmentInputComponents:TVkUInt32;
       maxFragmentOutputAttachments:TVkUInt32;
       maxFragmentDualSrcAttachments:TVkUInt32;
       maxFragmentCombinedOutputResources:TVkUInt32;
       maxComputeSharedMemorySize:TVkUInt32;
       maxComputeWorkGroupCount:array[0..2] of TVkUInt32;
       maxComputeWorkGroupInvocations:TVkUInt32;
       maxComputeWorkGroupSize:array[0..2] of TVkUInt32;
       subPixelPrecisionBits:TVkUInt32;
       subTexelPrecisionBits:TVkUInt32;
       mipmapPrecisionBits:TVkUInt32;
       maxDrawIndexedIndexValue:TVkUInt32;
       maxDrawIndirectCount:TVkUInt32;
       maxSamplerLodBias:TVkFloat;
       maxSamplerAnisotropy:TVkFloat;
       maxViewports:TVkUInt32;
       maxViewportDimensions:array[0..1] of TVkUInt32;
       viewportBoundsRange:array[0..1] of TVkFloat;
       viewportSubPixelBits:TVkUInt32;
       minMemoryMapAlignment:TVkSize;
       minTexelBufferOffsetAlignment:TVkDeviceSize;
       minUniformBufferOffsetAlignment:TVkDeviceSize;
       minStorageBufferOffsetAlignment:TVkDeviceSize;
       minTexelOffset:TVkInt32;
       maxTexelOffset:TVkUInt32;
       minTexelGatherOffset:TVkInt32;
       maxTexelGatherOffset:TVkUInt32;
       minInterpolationOffset:TVkFloat;
       maxInterpolationOffset:TVkFloat;
       subPixelInterpolationOffsetBits:TVkUInt32;
       maxFramebufferWidth:TVkUInt32;
       maxFramebufferHeight:TVkUInt32;
       maxFramebufferLayers:TVkUInt32;
       framebufferColorSampleCounts:TVkSampleCountFlags;
       framebufferDepthSampleCounts:TVkSampleCountFlags;
       framebufferStencilSampleCounts:TVkSampleCountFlags;
       framebufferNoAttachmentsSampleCounts:TVkSampleCountFlags;
       maxColorAttachments:TVkUInt32;
       sampledImageColorSampleCounts:TVkSampleCountFlags;
       sampledImageIntegerSampleCounts:TVkSampleCountFlags;
       sampledImageDepthSampleCounts:TVkSampleCountFlags;
       sampledImageStencilSampleCounts:TVkSampleCountFlags;
       storageImageSampleCounts:TVkSampleCountFlags;
       maxSampleMaskWords:TVkUInt32;
       timestampComputeAndGraphics:TVkBool32;
       timestampPeriod:TVkFloat;
       maxClipDistances:TVkUInt32;
       maxCullDistances:TVkUInt32;
       maxCombinedClipAndCullDistances:TVkUInt32;
       discreteQueuePriorities:TVkUInt32;
       pointSizeRange:array[0..1] of TVkFloat;
       lineWidthRange:array[0..1] of TVkFloat;
       pointSizeGranularity:TVkFloat;
       lineWidthGranularity:TVkFloat;
       strictLines:TVkBool32;
       standardSampleLocations:TVkBool32;
       optimalBufferCopyOffsetAlignment:TVkDeviceSize;
       optimalBufferCopyRowPitchAlignment:TVkDeviceSize;
       nonCoherentAtomSize:TVkDeviceSize;
     end;

     PPVkPhysicalDeviceProperties=^PVkPhysicalDeviceProperties;
     PVkPhysicalDeviceProperties=^TVkPhysicalDeviceProperties;
     TVkPhysicalDeviceProperties=record
       apiVersion:TVkUInt32;
       driverVersion:TVkUInt32;
       vendorID:TVkUInt32;
       deviceID:TVkUInt32;
       deviceType:TVkPhysicalDeviceType;
       deviceName:array[0..VK_MAX_PHYSICAL_DEVICE_NAME_SIZE-1] of TVkChar;
       pipelineCacheUUID:array[0..VK_UUID_SIZE-1] of TVkUInt8;
       limits:TVkPhysicalDeviceLimits;
       sparseProperties:TVkPhysicalDeviceSparseProperties;
     end;

     PPVkSemaphoreCreateInfo=^PVkSemaphoreCreateInfo;
     PVkSemaphoreCreateInfo=^TVkSemaphoreCreateInfo;
     TVkSemaphoreCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkSemaphoreCreateFlags;
     end;

     PPVkQueryPoolCreateInfo=^PVkQueryPoolCreateInfo;
     PVkQueryPoolCreateInfo=^TVkQueryPoolCreateInfo;
     TVkQueryPoolCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkQueryPoolCreateFlags;
       queryType:TVkQueryType;
       queryCount:TVkUInt32;
       pipelineStatistics:TVkQueryPipelineStatisticFlags;
     end;

     PPVkFramebufferCreateInfo=^PVkFramebufferCreateInfo;
     PVkFramebufferCreateInfo=^TVkFramebufferCreateInfo;
     TVkFramebufferCreateInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
       pNext:PVkVoid;
       flags:TVkFramebufferCreateFlags;
       renderPass:TVkRenderPass;
       attachmentCount:TVkUInt32;
       pAttachments:PVkImageView;
       width:TVkUInt32;
       height:TVkUInt32;
       layers:TVkUInt32;
     end;

     PPVkDrawIndirectCommand=^PVkDrawIndirectCommand;
     PVkDrawIndirectCommand=^TVkDrawIndirectCommand;
     TVkDrawIndirectCommand=record
       vertexCount:TVkUInt32;
       instanceCount:TVkUInt32;
       firstVertex:TVkUInt32;
       firstInstance:TVkUInt32;
     end;

     PPVkDrawIndexedIndirectCommand=^PVkDrawIndexedIndirectCommand;
     PVkDrawIndexedIndirectCommand=^TVkDrawIndexedIndirectCommand;
     TVkDrawIndexedIndirectCommand=record
       indexCount:TVkUInt32;
       instanceCount:TVkUInt32;
       firstIndex:TVkUInt32;
       vertexOffset:TVkInt32;
       firstInstance:TVkUInt32;
     end;

     PPVkDispatchIndirectCommand=^PVkDispatchIndirectCommand;
     PVkDispatchIndirectCommand=^TVkDispatchIndirectCommand;
     TVkDispatchIndirectCommand=record
       x:TVkUInt32;
       y:TVkUInt32;
       z:TVkUInt32;
     end;

     PPVkSubmitInfo=^PVkSubmitInfo;
     PVkSubmitInfo=^TVkSubmitInfo;
     TVkSubmitInfo=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SUBMIT_INFO
       pNext:PVkVoid;
       waitSemaphoreCount:TVkUInt32;
       pWaitSemaphores:PVkSemaphore;
       pWaitDstStageMask:PVkPipelineStageFlags;
       commandBufferCount:TVkUInt32;
       pCommandBuffers:PVkCommandBuffer;
       signalSemaphoreCount:TVkUInt32;
       pSignalSemaphores:PVkSemaphore;
     end;

     PPVkDisplayPropertiesKHR=^PVkDisplayPropertiesKHR;
     PVkDisplayPropertiesKHR=^TVkDisplayPropertiesKHR;
     TVkDisplayPropertiesKHR=record
       display:TVkDisplayKHR;
       displayName:PVkChar;
       physicalDimensions:TVkExtent2D;
       physicalResolution:TVkExtent2D;
       supportedTransforms:TVkSurfaceTransformFlagsKHR;
       planeReorderPossible:TVkBool32;
       persistentContent:TVkBool32;
     end;

     PPVkDisplayPlanePropertiesKHR=^PVkDisplayPlanePropertiesKHR;
     PVkDisplayPlanePropertiesKHR=^TVkDisplayPlanePropertiesKHR;
     TVkDisplayPlanePropertiesKHR=record
       currentDisplay:TVkDisplayKHR;
       currentStackIndex:TVkUInt32;
     end;

     PPVkDisplayModeParametersKHR=^PVkDisplayModeParametersKHR;
     PVkDisplayModeParametersKHR=^TVkDisplayModeParametersKHR;
     TVkDisplayModeParametersKHR=record
       visibleRegion:TVkExtent2D;
       refreshRate:TVkUInt32;
     end;

     PPVkDisplayModePropertiesKHR=^PVkDisplayModePropertiesKHR;
     PVkDisplayModePropertiesKHR=^TVkDisplayModePropertiesKHR;
     TVkDisplayModePropertiesKHR=record
       displayMode:TVkDisplayModeKHR;
       parameters:TVkDisplayModeParametersKHR;
     end;

     PPVkDisplayModeCreateInfoKHR=^PVkDisplayModeCreateInfoKHR;
     PVkDisplayModeCreateInfoKHR=^TVkDisplayModeCreateInfoKHR;
     TVkDisplayModeCreateInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR
       pNext:PVkVoid;
       flags:TVkDisplayModeCreateFlagsKHR;
       parameters:TVkDisplayModeParametersKHR;
     end;

     PPVkDisplayPlaneCapabilitiesKHR=^PVkDisplayPlaneCapabilitiesKHR;
     PVkDisplayPlaneCapabilitiesKHR=^TVkDisplayPlaneCapabilitiesKHR;
     TVkDisplayPlaneCapabilitiesKHR=record
       supportedAlpha:TVkDisplayPlaneAlphaFlagsKHR;
       minSrcPosition:TVkOffset2D;
       maxSrcPosition:TVkOffset2D;
       minSrcExtent:TVkExtent2D;
       maxSrcExtent:TVkExtent2D;
       minDstPosition:TVkOffset2D;
       maxDstPosition:TVkOffset2D;
       minDstExtent:TVkExtent2D;
       maxDstExtent:TVkExtent2D;
     end;

     PPVkDisplaySurfaceCreateInfoKHR=^PVkDisplaySurfaceCreateInfoKHR;
     PVkDisplaySurfaceCreateInfoKHR=^TVkDisplaySurfaceCreateInfoKHR;
     TVkDisplaySurfaceCreateInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR
       pNext:PVkVoid;
       flags:TVkDisplaySurfaceCreateFlagsKHR;
       displayMode:TVkDisplayModeKHR;
       planeIndex:TVkUInt32;
       planeStackIndex:TVkUInt32;
       transform:TVkSurfaceTransformFlagBitsKHR;
       globalAlpha:TVkFloat;
       alphaMode:TVkDisplayPlaneAlphaFlagBitsKHR;
       imageExtent:TVkExtent2D;
     end;

     PPVkDisplayPresentInfoKHR=^PVkDisplayPresentInfoKHR;
     PVkDisplayPresentInfoKHR=^TVkDisplayPresentInfoKHR;
     TVkDisplayPresentInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR
       pNext:PVkVoid;
       srcRect:TVkRect2D;
       dstRect:TVkRect2D;
       persistent:TVkBool32;
     end;

     PPVkSurfaceCapabilitiesKHR=^PVkSurfaceCapabilitiesKHR;
     PVkSurfaceCapabilitiesKHR=^TVkSurfaceCapabilitiesKHR;
     TVkSurfaceCapabilitiesKHR=record
       minImageCount:TVkUInt32;
       maxImageCount:TVkUInt32;
       currentExtent:TVkExtent2D;
       minImageExtent:TVkExtent2D;
       maxImageExtent:TVkExtent2D;
       maxImageArrayLayers:TVkUInt32;
       supportedTransforms:TVkSurfaceTransformFlagsKHR;
       currentTransform:TVkSurfaceTransformFlagBitsKHR;
       supportedCompositeAlpha:TVkCompositeAlphaFlagsKHR;
       supportedUsageFlags:TVkImageUsageFlags;
     end;

{$ifdef Android}
     PPVkAndroidSurfaceCreateInfoKHR=^PVkAndroidSurfaceCreateInfoKHR;
     PVkAndroidSurfaceCreateInfoKHR=^TVkAndroidSurfaceCreateInfoKHR;
     TVkAndroidSurfaceCreateInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR
       pNext:PVkVoid;
       flags:TVkAndroidSurfaceCreateFlagsKHR;
       window:PVkAndroidANativeWindow;
     end;
{$endif}

{$ifdef Mir}
     PPVkMirSurfaceCreateInfoKHR=^PVkMirSurfaceCreateInfoKHR;
     PVkMirSurfaceCreateInfoKHR=^TVkMirSurfaceCreateInfoKHR;
     TVkMirSurfaceCreateInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR
       pNext:PVkVoid;
       flags:TVkMirSurfaceCreateFlagsKHR;
       connection:PVkMirConnection;
       mirSurface:PVkMirSurface;
     end;
{$endif}

     PPVkViSurfaceCreateInfoNN=^PVkViSurfaceCreateInfoNN;
     PVkViSurfaceCreateInfoNN=^TVkViSurfaceCreateInfoNN;
     TVkViSurfaceCreateInfoNN=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN
       pNext:PVkVoid;
       flags:TVkViSurfaceCreateFlagsNN;
       window:PVkVoid;
     end;

{$ifdef Wayland}
     PPVkWaylandSurfaceCreateInfoKHR=^PVkWaylandSurfaceCreateInfoKHR;
     PVkWaylandSurfaceCreateInfoKHR=^TVkWaylandSurfaceCreateInfoKHR;
     TVkWaylandSurfaceCreateInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR
       pNext:PVkVoid;
       flags:TVkWaylandSurfaceCreateFlagsKHR;
       display:PVkWaylandDisplay;
       surface:PVkWaylandSurface;
     end;
{$endif}

{$ifdef Windows}
     PPVkWin32SurfaceCreateInfoKHR=^PVkWin32SurfaceCreateInfoKHR;
     PVkWin32SurfaceCreateInfoKHR=^TVkWin32SurfaceCreateInfoKHR;
     TVkWin32SurfaceCreateInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR
       pNext:PVkVoid;
       flags:TVkWin32SurfaceCreateFlagsKHR;
       hinstance_:TVkHINSTANCE;
       hwnd_:TVkHWND;
     end;
{$endif}

{$ifdef XLIB}
     PPVkXlibSurfaceCreateInfoKHR=^PVkXlibSurfaceCreateInfoKHR;
     PVkXlibSurfaceCreateInfoKHR=^TVkXlibSurfaceCreateInfoKHR;
     TVkXlibSurfaceCreateInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR
       pNext:PVkVoid;
       flags:TVkXlibSurfaceCreateFlagsKHR;
       dpy:PVkXLIBDisplay;
       window:TVkXLIBWindow;
     end;
{$endif}

{$ifdef XCB}
     PPVkXcbSurfaceCreateInfoKHR=^PVkXcbSurfaceCreateInfoKHR;
     PVkXcbSurfaceCreateInfoKHR=^TVkXcbSurfaceCreateInfoKHR;
     TVkXcbSurfaceCreateInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR
       pNext:PVkVoid;
       flags:TVkXcbSurfaceCreateFlagsKHR;
       connection:PVkXCBConnection;
       window:TVkXCBWindow;
     end;
{$endif}

     PPVkSurfaceFormatKHR=^PVkSurfaceFormatKHR;
     PVkSurfaceFormatKHR=^TVkSurfaceFormatKHR;
     TVkSurfaceFormatKHR=record
       format:TVkFormat;
       colorSpace:TVkColorSpaceKHR;
     end;

     PPVkSwapchainCreateInfoKHR=^PVkSwapchainCreateInfoKHR;
     PVkSwapchainCreateInfoKHR=^TVkSwapchainCreateInfoKHR;
     TVkSwapchainCreateInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
       pNext:PVkVoid;
       flags:TVkSwapchainCreateFlagsKHR;
       surface:TVkSurfaceKHR;
       minImageCount:TVkUInt32;
       imageFormat:TVkFormat;
       imageColorSpace:TVkColorSpaceKHR;
       imageExtent:TVkExtent2D;
       imageArrayLayers:TVkUInt32;
       imageUsage:TVkImageUsageFlags;
       imageSharingMode:TVkSharingMode;
       queueFamilyIndexCount:TVkUInt32;
       pQueueFamilyIndices:PVkUInt32;
       preTransform:TVkSurfaceTransformFlagBitsKHR;
       compositeAlpha:TVkCompositeAlphaFlagBitsKHR;
       presentMode:TVkPresentModeKHR;
       clipped:TVkBool32;
       oldSwapchain:TVkSwapchainKHR;
     end;

     PPVkPresentInfoKHR=^PVkPresentInfoKHR;
     PVkPresentInfoKHR=^TVkPresentInfoKHR;
     TVkPresentInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
       pNext:PVkVoid;
       waitSemaphoreCount:TVkUInt32;
       pWaitSemaphores:PVkSemaphore;
       swapchainCount:TVkUInt32;
       pSwapchains:PVkSwapchainKHR;
       pImageIndices:PVkUInt32;
       pResults:PVkResult;
     end;

     PPVkDebugReportCallbackCreateInfoEXT=^PVkDebugReportCallbackCreateInfoEXT;
     PVkDebugReportCallbackCreateInfoEXT=^TVkDebugReportCallbackCreateInfoEXT;
     TVkDebugReportCallbackCreateInfoEXT=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
       pNext:PVkVoid;
       flags:TVkDebugReportFlagsEXT;
       pfnCallback:TPFN_vkDebugReportCallbackEXT;
       pUserData:PVkVoid;
     end;

     PPVkValidationFlagsEXT=^PVkValidationFlagsEXT;
     PVkValidationFlagsEXT=^TVkValidationFlagsEXT;
     TVkValidationFlagsEXT=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT
       pNext:PVkVoid;
       disabledValidationCheckCount:TVkUInt32;
       pDisabledValidationChecks:PVkValidationCheckEXT;
     end;

     PPVkPipelineRasterizationStateRasterizationOrderAMD=^PVkPipelineRasterizationStateRasterizationOrderAMD;
     PVkPipelineRasterizationStateRasterizationOrderAMD=^TVkPipelineRasterizationStateRasterizationOrderAMD;
     TVkPipelineRasterizationStateRasterizationOrderAMD=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD
       pNext:PVkVoid;
       rasterizationOrder:TVkRasterizationOrderAMD;
     end;

     PPVkDebugMarkerObjectNameInfoEXT=^PVkDebugMarkerObjectNameInfoEXT;
     PVkDebugMarkerObjectNameInfoEXT=^TVkDebugMarkerObjectNameInfoEXT;
     TVkDebugMarkerObjectNameInfoEXT=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT
       pNext:PVkVoid;
       objectType:TVkDebugReportObjectTypeEXT;
       object_:TVkUInt64;
       pObjectName:PVkChar;
     end;

     PPVkDebugMarkerObjectTagInfoEXT=^PVkDebugMarkerObjectTagInfoEXT;
     PVkDebugMarkerObjectTagInfoEXT=^TVkDebugMarkerObjectTagInfoEXT;
     TVkDebugMarkerObjectTagInfoEXT=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT
       pNext:PVkVoid;
       objectType:TVkDebugReportObjectTypeEXT;
       object_:TVkUInt64;
       tagName:TVkUInt64;
       tagSize:TVkSize;
       pTag:PVkVoid;
     end;

     PPVkDebugMarkerMarkerInfoEXT=^PVkDebugMarkerMarkerInfoEXT;
     PVkDebugMarkerMarkerInfoEXT=^TVkDebugMarkerMarkerInfoEXT;
     TVkDebugMarkerMarkerInfoEXT=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT
       pNext:PVkVoid;
       pMarkerName:PVkChar;
       color:array[0..3] of TVkFloat;
     end;

     PPVkDedicatedAllocationImageCreateInfoNV=^PVkDedicatedAllocationImageCreateInfoNV;
     PVkDedicatedAllocationImageCreateInfoNV=^TVkDedicatedAllocationImageCreateInfoNV;
     TVkDedicatedAllocationImageCreateInfoNV=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
       pNext:PVkVoid;
       dedicatedAllocation:TVkBool32;
     end;

     PPVkDedicatedAllocationBufferCreateInfoNV=^PVkDedicatedAllocationBufferCreateInfoNV;
     PVkDedicatedAllocationBufferCreateInfoNV=^TVkDedicatedAllocationBufferCreateInfoNV;
     TVkDedicatedAllocationBufferCreateInfoNV=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
       pNext:PVkVoid;
       dedicatedAllocation:TVkBool32;
     end;

     PPVkDedicatedAllocationMemoryAllocateInfoNV=^PVkDedicatedAllocationMemoryAllocateInfoNV;
     PVkDedicatedAllocationMemoryAllocateInfoNV=^TVkDedicatedAllocationMemoryAllocateInfoNV;
     TVkDedicatedAllocationMemoryAllocateInfoNV=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
       pNext:PVkVoid;
       image:TVkImage;
       buffer:TVkBuffer;
     end;

     PPVkExternalImageFormatPropertiesNV=^PVkExternalImageFormatPropertiesNV;
     PVkExternalImageFormatPropertiesNV=^TVkExternalImageFormatPropertiesNV;
     TVkExternalImageFormatPropertiesNV=record
       imageFormatProperties:TVkImageFormatProperties;
       externalMemoryFeatures:TVkExternalMemoryFeatureFlagsNV;
       exportFromImportedHandleTypes:TVkExternalMemoryHandleTypeFlagsNV;
       compatibleHandleTypes:TVkExternalMemoryHandleTypeFlagsNV;
     end;

     PPVkExternalMemoryImageCreateInfoNV=^PVkExternalMemoryImageCreateInfoNV;
     PVkExternalMemoryImageCreateInfoNV=^TVkExternalMemoryImageCreateInfoNV;
     TVkExternalMemoryImageCreateInfoNV=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV
       pNext:PVkVoid;
       handleTypes:TVkExternalMemoryHandleTypeFlagsNV;
     end;

     PPVkExportMemoryAllocateInfoNV=^PVkExportMemoryAllocateInfoNV;
     PVkExportMemoryAllocateInfoNV=^TVkExportMemoryAllocateInfoNV;
     TVkExportMemoryAllocateInfoNV=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV
       pNext:PVkVoid;
       handleTypes:TVkExternalMemoryHandleTypeFlagsNV;
     end;

     PPVkImportMemoryWin32HandleInfoNV=^PVkImportMemoryWin32HandleInfoNV;
     PVkImportMemoryWin32HandleInfoNV=^TVkImportMemoryWin32HandleInfoNV;
     TVkImportMemoryWin32HandleInfoNV=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV
       pNext:PVkVoid;
       handleType:TVkExternalMemoryHandleTypeFlagsNV;
       handle:THANDLE;
     end;

{$ifdef Windows}
     PPVkExportMemoryWin32HandleInfoNV=^PVkExportMemoryWin32HandleInfoNV;
     PVkExportMemoryWin32HandleInfoNV=^TVkExportMemoryWin32HandleInfoNV;
     TVkExportMemoryWin32HandleInfoNV=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV
       pNext:PVkVoid;
       pAttributes:PSecurityAttributes;
       dwAccess:TVkUInt32;
     end;
{$endif}

     PPVkWin32KeyedMutexAcquireReleaseInfoNV=^PVkWin32KeyedMutexAcquireReleaseInfoNV;
     PVkWin32KeyedMutexAcquireReleaseInfoNV=^TVkWin32KeyedMutexAcquireReleaseInfoNV;
     TVkWin32KeyedMutexAcquireReleaseInfoNV=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
       pNext:PVkVoid;
       acquireCount:TVkUInt32;
       pAcquireSyncs:PVkDeviceMemory;
       pAcquireKeys:PVkUInt64;
       pAcquireTimeoutMilliseconds:PVkUInt32;
       releaseCount:TVkUInt32;
       pReleaseSyncs:PVkDeviceMemory;
       pReleaseKeys:PVkUInt64;
     end;

     PPVkDeviceGeneratedCommandsFeaturesNVX=^PVkDeviceGeneratedCommandsFeaturesNVX;
     PVkDeviceGeneratedCommandsFeaturesNVX=^TVkDeviceGeneratedCommandsFeaturesNVX;
     TVkDeviceGeneratedCommandsFeaturesNVX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX
       pNext:PVkVoid;
       computeBindingPointSupport:TVkBool32;
     end;

     PPVkDeviceGeneratedCommandsLimitsNVX=^PVkDeviceGeneratedCommandsLimitsNVX;
     PVkDeviceGeneratedCommandsLimitsNVX=^TVkDeviceGeneratedCommandsLimitsNVX;
     TVkDeviceGeneratedCommandsLimitsNVX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX
       pNext:PVkVoid;
       maxIndirectCommandsLayoutTokenCount:TVkUInt32;
       maxObjectEntryCounts:TVkUInt32;
       minSequenceCountBufferOffsetAlignment:TVkUInt32;
       minSequenceIndexBufferOffsetAlignment:TVkUInt32;
       minCommandsTokenBufferOffsetAlignment:TVkUInt32;
     end;

     PPVkIndirectCommandsTokenNVX=^PVkIndirectCommandsTokenNVX;
     PVkIndirectCommandsTokenNVX=^TVkIndirectCommandsTokenNVX;
     TVkIndirectCommandsTokenNVX=record
       tokenType:TVkIndirectCommandsTokenTypeNVX;
       buffer:TVkBuffer;
       offset:TVkDeviceSize;
     end;

     PPVkIndirectCommandsLayoutTokenNVX=^PVkIndirectCommandsLayoutTokenNVX;
     PVkIndirectCommandsLayoutTokenNVX=^TVkIndirectCommandsLayoutTokenNVX;
     TVkIndirectCommandsLayoutTokenNVX=record
       tokenType:TVkIndirectCommandsTokenTypeNVX;
       bindingUnit:TVkUInt32;
       dynamicCount:TVkUInt32;
       divisor:TVkUInt32;
     end;

     PPVkIndirectCommandsLayoutCreateInfoNVX=^PVkIndirectCommandsLayoutCreateInfoNVX;
     PVkIndirectCommandsLayoutCreateInfoNVX=^TVkIndirectCommandsLayoutCreateInfoNVX;
     TVkIndirectCommandsLayoutCreateInfoNVX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX
       pNext:PVkVoid;
       pipelineBindPoint:TVkPipelineBindPoint;
       flags:TVkIndirectCommandsLayoutUsageFlagsNVX;
       tokenCount:TVkUInt32;
       pTokens:PVkIndirectCommandsLayoutTokenNVX;
     end;

     PPVkCmdProcessCommandsInfoNVX=^PVkCmdProcessCommandsInfoNVX;
     PVkCmdProcessCommandsInfoNVX=^TVkCmdProcessCommandsInfoNVX;
     TVkCmdProcessCommandsInfoNVX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX
       pNext:PVkVoid;
       objectTable:TVkObjectTableNVX;
       indirectCommandsLayout:TVkIndirectCommandsLayoutNVX;
       indirectCommandsTokenCount:TVkUInt32;
       pIndirectCommandsTokens:PVkIndirectCommandsTokenNVX;
       maxSequencesCount:TVkUInt32;
       targetCommandBuffer:TVkCommandBuffer;
       sequencesCountBuffer:TVkBuffer;
       sequencesCountOffset:TVkDeviceSize;
       sequencesIndexBuffer:TVkBuffer;
       sequencesIndexOffset:TVkDeviceSize;
     end;

     PPVkCmdReserveSpaceForCommandsInfoNVX=^PVkCmdReserveSpaceForCommandsInfoNVX;
     PVkCmdReserveSpaceForCommandsInfoNVX=^TVkCmdReserveSpaceForCommandsInfoNVX;
     TVkCmdReserveSpaceForCommandsInfoNVX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX
       pNext:PVkVoid;
       objectTable:TVkObjectTableNVX;
       indirectCommandsLayout:TVkIndirectCommandsLayoutNVX;
       maxSequencesCount:TVkUInt32;
     end;

     PPVkObjectTableCreateInfoNVX=^PVkObjectTableCreateInfoNVX;
     PVkObjectTableCreateInfoNVX=^TVkObjectTableCreateInfoNVX;
     TVkObjectTableCreateInfoNVX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX
       pNext:PVkVoid;
       objectCount:TVkUInt32;
       pObjectEntryTypes:PVkObjectEntryTypeNVX;
       pObjectEntryCounts:PVkUInt32;
       pObjectEntryUsageFlags:PVkObjectEntryUsageFlagsNVX;
       maxUniformBuffersPerDescriptor:TVkUInt32;
       maxStorageBuffersPerDescriptor:TVkUInt32;
       maxStorageImagesPerDescriptor:TVkUInt32;
       maxSampledImagesPerDescriptor:TVkUInt32;
       maxPipelineLayouts:TVkUInt32;
     end;

     PPVkObjectTableEntryNVX=^PVkObjectTableEntryNVX;
     PVkObjectTableEntryNVX=^TVkObjectTableEntryNVX;
     TVkObjectTableEntryNVX=record
       type_:TVkObjectEntryTypeNVX;
       flags:TVkObjectEntryUsageFlagsNVX;
     end;

     PPVkObjectTablePipelineEntryNVX=^PVkObjectTablePipelineEntryNVX;
     PVkObjectTablePipelineEntryNVX=^TVkObjectTablePipelineEntryNVX;
     TVkObjectTablePipelineEntryNVX=record
       type_:TVkObjectEntryTypeNVX;
       flags:TVkObjectEntryUsageFlagsNVX;
       pipeline:TVkPipeline;
     end;

     PPVkObjectTableDescriptorSetEntryNVX=^PVkObjectTableDescriptorSetEntryNVX;
     PVkObjectTableDescriptorSetEntryNVX=^TVkObjectTableDescriptorSetEntryNVX;
     TVkObjectTableDescriptorSetEntryNVX=record
       type_:TVkObjectEntryTypeNVX;
       flags:TVkObjectEntryUsageFlagsNVX;
       pipelineLayout:TVkPipelineLayout;
       descriptorSet:TVkDescriptorSet;
     end;

     PPVkObjectTableVertexBufferEntryNVX=^PVkObjectTableVertexBufferEntryNVX;
     PVkObjectTableVertexBufferEntryNVX=^TVkObjectTableVertexBufferEntryNVX;
     TVkObjectTableVertexBufferEntryNVX=record
       type_:TVkObjectEntryTypeNVX;
       flags:TVkObjectEntryUsageFlagsNVX;
       buffer:TVkBuffer;
     end;

     PPVkObjectTableIndexBufferEntryNVX=^PVkObjectTableIndexBufferEntryNVX;
     PVkObjectTableIndexBufferEntryNVX=^TVkObjectTableIndexBufferEntryNVX;
     TVkObjectTableIndexBufferEntryNVX=record
       type_:TVkObjectEntryTypeNVX;
       flags:TVkObjectEntryUsageFlagsNVX;
       buffer:TVkBuffer;
       indexType:TVkIndexType;
     end;

     PPVkObjectTablePushConstantEntryNVX=^PVkObjectTablePushConstantEntryNVX;
     PVkObjectTablePushConstantEntryNVX=^TVkObjectTablePushConstantEntryNVX;
     TVkObjectTablePushConstantEntryNVX=record
       type_:TVkObjectEntryTypeNVX;
       flags:TVkObjectEntryUsageFlagsNVX;
       pipelineLayout:TVkPipelineLayout;
       stageFlags:TVkShaderStageFlags;
     end;

     PPVkPhysicalDeviceFeatures2KHR=^PVkPhysicalDeviceFeatures2KHR;
     PVkPhysicalDeviceFeatures2KHR=^TVkPhysicalDeviceFeatures2KHR;
     TVkPhysicalDeviceFeatures2KHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR
       pNext:PVkVoid;
       features:TVkPhysicalDeviceFeatures;
     end;

     PPVkPhysicalDeviceProperties2KHR=^PVkPhysicalDeviceProperties2KHR;
     PVkPhysicalDeviceProperties2KHR=^TVkPhysicalDeviceProperties2KHR;
     TVkPhysicalDeviceProperties2KHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR
       pNext:PVkVoid;
       properties:TVkPhysicalDeviceProperties;
     end;

     PPVkFormatProperties2KHR=^PVkFormatProperties2KHR;
     PVkFormatProperties2KHR=^TVkFormatProperties2KHR;
     TVkFormatProperties2KHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR
       pNext:PVkVoid;
       formatProperties:TVkFormatProperties;
     end;

     PPVkImageFormatProperties2KHR=^PVkImageFormatProperties2KHR;
     PVkImageFormatProperties2KHR=^TVkImageFormatProperties2KHR;
     TVkImageFormatProperties2KHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR
       pNext:PVkVoid;
       imageFormatProperties:TVkImageFormatProperties;
     end;

     PPVkPhysicalDeviceImageFormatInfo2KHR=^PVkPhysicalDeviceImageFormatInfo2KHR;
     PVkPhysicalDeviceImageFormatInfo2KHR=^TVkPhysicalDeviceImageFormatInfo2KHR;
     TVkPhysicalDeviceImageFormatInfo2KHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR
       pNext:PVkVoid;
       format:TVkFormat;
       type_:TVkImageType;
       tiling:TVkImageTiling;
       usage:TVkImageUsageFlags;
       flags:TVkImageCreateFlags;
     end;

     PPVkQueueFamilyProperties2KHR=^PVkQueueFamilyProperties2KHR;
     PVkQueueFamilyProperties2KHR=^TVkQueueFamilyProperties2KHR;
     TVkQueueFamilyProperties2KHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR
       pNext:PVkVoid;
       queueFamilyProperties:TVkQueueFamilyProperties;
     end;

     PPVkPhysicalDeviceMemoryProperties2KHR=^PVkPhysicalDeviceMemoryProperties2KHR;
     PVkPhysicalDeviceMemoryProperties2KHR=^TVkPhysicalDeviceMemoryProperties2KHR;
     TVkPhysicalDeviceMemoryProperties2KHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR
       pNext:PVkVoid;
       memoryProperties:TVkPhysicalDeviceMemoryProperties;
     end;

     PPVkSparseImageFormatProperties2KHR=^PVkSparseImageFormatProperties2KHR;
     PVkSparseImageFormatProperties2KHR=^TVkSparseImageFormatProperties2KHR;
     TVkSparseImageFormatProperties2KHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR
       pNext:PVkVoid;
       properties:TVkSparseImageFormatProperties;
     end;

     PPVkPhysicalDeviceSparseImageFormatInfo2KHR=^PVkPhysicalDeviceSparseImageFormatInfo2KHR;
     PVkPhysicalDeviceSparseImageFormatInfo2KHR=^TVkPhysicalDeviceSparseImageFormatInfo2KHR;
     TVkPhysicalDeviceSparseImageFormatInfo2KHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR
       pNext:PVkVoid;
       format:TVkFormat;
       type_:TVkImageType;
       samples:TVkSampleCountFlagBits;
       usage:TVkImageUsageFlags;
       tiling:TVkImageTiling;
     end;

     PPVkPhysicalDevicePushDescriptorPropertiesKHR=^PVkPhysicalDevicePushDescriptorPropertiesKHR;
     PVkPhysicalDevicePushDescriptorPropertiesKHR=^TVkPhysicalDevicePushDescriptorPropertiesKHR;
     TVkPhysicalDevicePushDescriptorPropertiesKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
       pNext:PVkVoid;
       maxPushDescriptors:TVkUInt32;
     end;

     PPVkRectLayerKHR=^PVkRectLayerKHR;
     PVkRectLayerKHR=^TVkRectLayerKHR;
     TVkRectLayerKHR=record
       offset:TVkOffset2D;
       extent:TVkExtent2D;
       layer:TVkUInt32;
     end;

     PPVkPresentRegionKHR=^PVkPresentRegionKHR;
     PVkPresentRegionKHR=^TVkPresentRegionKHR;
     TVkPresentRegionKHR=record
       rectangleCount:TVkUInt32;
       pRectangles:PVkRectLayerKHR;
     end;

     PPVkPresentRegionsKHR=^PVkPresentRegionsKHR;
     PVkPresentRegionsKHR=^TVkPresentRegionsKHR;
     TVkPresentRegionsKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR
       pNext:PVkVoid;
       swapchainCount:TVkUInt32;
       pRegions:PVkPresentRegionKHR;
     end;

     PPVkPhysicalDeviceVariablePointerFeaturesKHR=^PVkPhysicalDeviceVariablePointerFeaturesKHR;
     PVkPhysicalDeviceVariablePointerFeaturesKHR=^TVkPhysicalDeviceVariablePointerFeaturesKHR;
     TVkPhysicalDeviceVariablePointerFeaturesKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR
       pNext:PVkVoid;
       variablePointersStorageBuffer:TVkBool32;
       variablePointers:TVkBool32;
     end;

     PPVkExternalMemoryPropertiesKHR=^PVkExternalMemoryPropertiesKHR;
     PVkExternalMemoryPropertiesKHR=^TVkExternalMemoryPropertiesKHR;
     TVkExternalMemoryPropertiesKHR=record
       externalMemoryFeatures:TVkExternalMemoryFeatureFlagsKHR;
       exportFromImportedHandleTypes:TVkExternalMemoryHandleTypeFlagsKHR;
       compatibleHandleTypes:TVkExternalMemoryHandleTypeFlagsKHR;
     end;

     PPVkPhysicalDeviceExternalImageFormatInfoKHR=^PVkPhysicalDeviceExternalImageFormatInfoKHR;
     PVkPhysicalDeviceExternalImageFormatInfoKHR=^TVkPhysicalDeviceExternalImageFormatInfoKHR;
     TVkPhysicalDeviceExternalImageFormatInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR
       pNext:PVkVoid;
       handleType:TVkExternalMemoryHandleTypeFlagBitsKHR;
     end;

     PPVkExternalImageFormatPropertiesKHR=^PVkExternalImageFormatPropertiesKHR;
     PVkExternalImageFormatPropertiesKHR=^TVkExternalImageFormatPropertiesKHR;
     TVkExternalImageFormatPropertiesKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR
       pNext:PVkVoid;
       externalMemoryProperties:TVkExternalMemoryPropertiesKHR;
     end;

     PPVkPhysicalDeviceExternalBufferInfoKHR=^PVkPhysicalDeviceExternalBufferInfoKHR;
     PVkPhysicalDeviceExternalBufferInfoKHR=^TVkPhysicalDeviceExternalBufferInfoKHR;
     TVkPhysicalDeviceExternalBufferInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR
       pNext:PVkVoid;
       flags:TVkBufferCreateFlags;
       usage:TVkBufferUsageFlags;
       handleType:TVkExternalMemoryHandleTypeFlagBitsKHR;
     end;

     PPVkExternalBufferPropertiesKHR=^PVkExternalBufferPropertiesKHR;
     PVkExternalBufferPropertiesKHR=^TVkExternalBufferPropertiesKHR;
     TVkExternalBufferPropertiesKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR
       pNext:PVkVoid;
       externalMemoryProperties:TVkExternalMemoryPropertiesKHR;
     end;

     PPVkPhysicalDeviceIDPropertiesKHR=^PVkPhysicalDeviceIDPropertiesKHR;
     PVkPhysicalDeviceIDPropertiesKHR=^TVkPhysicalDeviceIDPropertiesKHR;
     TVkPhysicalDeviceIDPropertiesKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR
       pNext:PVkVoid;
       deviceUUID:array[0..VK_UUID_SIZE-1] of TVkUInt8;
       driverUUID:array[0..VK_UUID_SIZE-1] of TVkUInt8;
       deviceLUID:array[0..VK_LUID_SIZE_KHR-1] of TVkUInt8;
       deviceNodeMask:TVkUInt32;
       deviceLUIDValid:TVkBool32;
     end;

     PPVkExternalMemoryImageCreateInfoKHR=^PVkExternalMemoryImageCreateInfoKHR;
     PVkExternalMemoryImageCreateInfoKHR=^TVkExternalMemoryImageCreateInfoKHR;
     TVkExternalMemoryImageCreateInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR
       pNext:PVkVoid;
       handleTypes:TVkExternalMemoryHandleTypeFlagsKHR;
     end;

     PPVkExternalMemoryBufferCreateInfoKHR=^PVkExternalMemoryBufferCreateInfoKHR;
     PVkExternalMemoryBufferCreateInfoKHR=^TVkExternalMemoryBufferCreateInfoKHR;
     TVkExternalMemoryBufferCreateInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR
       pNext:PVkVoid;
       handleTypes:TVkExternalMemoryHandleTypeFlagsKHR;
     end;

     PPVkExportMemoryAllocateInfoKHR=^PVkExportMemoryAllocateInfoKHR;
     PVkExportMemoryAllocateInfoKHR=^TVkExportMemoryAllocateInfoKHR;
     TVkExportMemoryAllocateInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR
       pNext:PVkVoid;
       handleTypes:TVkExternalMemoryHandleTypeFlagsKHR;
     end;

     PPVkImportMemoryWin32HandleInfoKHR=^PVkImportMemoryWin32HandleInfoKHR;
     PVkImportMemoryWin32HandleInfoKHR=^TVkImportMemoryWin32HandleInfoKHR;
     TVkImportMemoryWin32HandleInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR
       pNext:PVkVoid;
       handleType:TVkExternalMemoryHandleTypeFlagBitsKHR;
       handle:THANDLE;
       name:PWideChar;
     end;

{$ifdef Windows}
     PPVkExportMemoryWin32HandleInfoKHR=^PVkExportMemoryWin32HandleInfoKHR;
     PVkExportMemoryWin32HandleInfoKHR=^TVkExportMemoryWin32HandleInfoKHR;
     TVkExportMemoryWin32HandleInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR
       pNext:PVkVoid;
       pAttributes:PSecurityAttributes;
       dwAccess:TVkUInt32;
       name:PWideChar;
     end;
{$endif}

     PPVkMemoryWin32HandlePropertiesKHR=^PVkMemoryWin32HandlePropertiesKHR;
     PVkMemoryWin32HandlePropertiesKHR=^TVkMemoryWin32HandlePropertiesKHR;
     TVkMemoryWin32HandlePropertiesKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR
       pNext:PVkVoid;
       memoryTypeBits:TVkUInt32;
     end;

     PPVkMemoryGetWin32HandleInfoKHR=^PVkMemoryGetWin32HandleInfoKHR;
     PVkMemoryGetWin32HandleInfoKHR=^TVkMemoryGetWin32HandleInfoKHR;
     TVkMemoryGetWin32HandleInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR
       pNext:PVkVoid;
       memory:TVkDeviceMemory;
       handleType:TVkExternalMemoryHandleTypeFlagBitsKHR;
     end;

     PPVkImportMemoryFdInfoKHR=^PVkImportMemoryFdInfoKHR;
     PVkImportMemoryFdInfoKHR=^TVkImportMemoryFdInfoKHR;
     TVkImportMemoryFdInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR
       pNext:PVkVoid;
       handleType:TVkExternalMemoryHandleTypeFlagBitsKHR;
       fd:TVkInt32;
     end;

     PPVkMemoryFdPropertiesKHR=^PVkMemoryFdPropertiesKHR;
     PVkMemoryFdPropertiesKHR=^TVkMemoryFdPropertiesKHR;
     TVkMemoryFdPropertiesKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR
       pNext:PVkVoid;
       memoryTypeBits:TVkUInt32;
     end;

     PPVkMemoryGetFdInfoKHR=^PVkMemoryGetFdInfoKHR;
     PVkMemoryGetFdInfoKHR=^TVkMemoryGetFdInfoKHR;
     TVkMemoryGetFdInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR
       pNext:PVkVoid;
       memory:TVkDeviceMemory;
       handleType:TVkExternalMemoryHandleTypeFlagBitsKHR;
     end;

     PPVkWin32KeyedMutexAcquireReleaseInfoKHR=^PVkWin32KeyedMutexAcquireReleaseInfoKHR;
     PVkWin32KeyedMutexAcquireReleaseInfoKHR=^TVkWin32KeyedMutexAcquireReleaseInfoKHR;
     TVkWin32KeyedMutexAcquireReleaseInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR
       pNext:PVkVoid;
       acquireCount:TVkUInt32;
       pAcquireSyncs:PVkDeviceMemory;
       pAcquireKeys:PVkUInt64;
       pAcquireTimeouts:PVkUInt32;
       releaseCount:TVkUInt32;
       pReleaseSyncs:PVkDeviceMemory;
       pReleaseKeys:PVkUInt64;
     end;

     PPVkPhysicalDeviceExternalSemaphoreInfoKHR=^PVkPhysicalDeviceExternalSemaphoreInfoKHR;
     PVkPhysicalDeviceExternalSemaphoreInfoKHR=^TVkPhysicalDeviceExternalSemaphoreInfoKHR;
     TVkPhysicalDeviceExternalSemaphoreInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHR
       pNext:PVkVoid;
       handleType:TVkExternalSemaphoreHandleTypeFlagBitsKHR;
     end;

     PPVkExternalSemaphorePropertiesKHR=^PVkExternalSemaphorePropertiesKHR;
     PVkExternalSemaphorePropertiesKHR=^TVkExternalSemaphorePropertiesKHR;
     TVkExternalSemaphorePropertiesKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHR
       pNext:PVkVoid;
       exportFromImportedHandleTypes:TVkExternalSemaphoreHandleTypeFlagsKHR;
       compatibleHandleTypes:TVkExternalSemaphoreHandleTypeFlagsKHR;
       externalSemaphoreFeatures:TVkExternalSemaphoreFeatureFlagsKHR;
     end;

     PPVkExportSemaphoreCreateInfoKHR=^PVkExportSemaphoreCreateInfoKHR;
     PVkExportSemaphoreCreateInfoKHR=^TVkExportSemaphoreCreateInfoKHR;
     TVkExportSemaphoreCreateInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR
       pNext:PVkVoid;
       handleTypes:TVkExternalSemaphoreHandleTypeFlagsKHR;
     end;

     PPVkImportSemaphoreWin32HandleInfoKHR=^PVkImportSemaphoreWin32HandleInfoKHR;
     PVkImportSemaphoreWin32HandleInfoKHR=^TVkImportSemaphoreWin32HandleInfoKHR;
     TVkImportSemaphoreWin32HandleInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
       pNext:PVkVoid;
       semaphore:TVkSemaphore;
       flags:TVkSemaphoreImportFlagsKHR;
       handleType:TVkExternalSemaphoreHandleTypeFlagBitsKHR;
       handle:THANDLE;
       name:PWideChar;
     end;

{$ifdef Windows}
     PPVkExportSemaphoreWin32HandleInfoKHR=^PVkExportSemaphoreWin32HandleInfoKHR;
     PVkExportSemaphoreWin32HandleInfoKHR=^TVkExportSemaphoreWin32HandleInfoKHR;
     TVkExportSemaphoreWin32HandleInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
       pNext:PVkVoid;
       pAttributes:PSecurityAttributes;
       dwAccess:TVkUInt32;
       name:PWideChar;
     end;
{$endif}

     PPVkD3D12FenceSubmitInfoKHR=^PVkD3D12FenceSubmitInfoKHR;
     PVkD3D12FenceSubmitInfoKHR=^TVkD3D12FenceSubmitInfoKHR;
     TVkD3D12FenceSubmitInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR
       pNext:PVkVoid;
       waitSemaphoreValuesCount:TVkUInt32;
       pWaitSemaphoreValues:PVkUInt64;
       signalSemaphoreValuesCount:TVkUInt32;
       pSignalSemaphoreValues:PVkUInt64;
     end;

     PPVkSemaphoreGetWin32HandleInfoKHR=^PVkSemaphoreGetWin32HandleInfoKHR;
     PVkSemaphoreGetWin32HandleInfoKHR=^TVkSemaphoreGetWin32HandleInfoKHR;
     TVkSemaphoreGetWin32HandleInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR
       pNext:PVkVoid;
       semaphore:TVkSemaphore;
       handleType:TVkExternalSemaphoreHandleTypeFlagBitsKHR;
     end;

     PPVkImportSemaphoreFdInfoKHR=^PVkImportSemaphoreFdInfoKHR;
     PVkImportSemaphoreFdInfoKHR=^TVkImportSemaphoreFdInfoKHR;
     TVkImportSemaphoreFdInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR
       pNext:PVkVoid;
       semaphore:TVkSemaphore;
       flags:TVkSemaphoreImportFlagsKHR;
       handleType:TVkExternalSemaphoreHandleTypeFlagBitsKHR;
       fd:TVkInt32;
     end;

     PPVkSemaphoreGetFdInfoKHR=^PVkSemaphoreGetFdInfoKHR;
     PVkSemaphoreGetFdInfoKHR=^TVkSemaphoreGetFdInfoKHR;
     TVkSemaphoreGetFdInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR
       pNext:PVkVoid;
       semaphore:TVkSemaphore;
       handleType:TVkExternalSemaphoreHandleTypeFlagBitsKHR;
     end;

     PPVkPhysicalDeviceExternalFenceInfoKHR=^PVkPhysicalDeviceExternalFenceInfoKHR;
     PVkPhysicalDeviceExternalFenceInfoKHR=^TVkPhysicalDeviceExternalFenceInfoKHR;
     TVkPhysicalDeviceExternalFenceInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR
       pNext:PVkVoid;
       handleType:TVkExternalFenceHandleTypeFlagBitsKHR;
     end;

     PPVkExternalFencePropertiesKHR=^PVkExternalFencePropertiesKHR;
     PVkExternalFencePropertiesKHR=^TVkExternalFencePropertiesKHR;
     TVkExternalFencePropertiesKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR
       pNext:PVkVoid;
       exportFromImportedHandleTypes:TVkExternalFenceHandleTypeFlagsKHR;
       compatibleHandleTypes:TVkExternalFenceHandleTypeFlagsKHR;
       externalFenceFeatures:TVkExternalFenceFeatureFlagsKHR;
     end;

     PPVkExportFenceCreateInfoKHR=^PVkExportFenceCreateInfoKHR;
     PVkExportFenceCreateInfoKHR=^TVkExportFenceCreateInfoKHR;
     TVkExportFenceCreateInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR
       pNext:PVkVoid;
       handleTypes:TVkExternalFenceHandleTypeFlagsKHR;
     end;

     PPVkImportFenceWin32HandleInfoKHR=^PVkImportFenceWin32HandleInfoKHR;
     PVkImportFenceWin32HandleInfoKHR=^TVkImportFenceWin32HandleInfoKHR;
     TVkImportFenceWin32HandleInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR
       pNext:PVkVoid;
       fence:TVkFence;
       flags:TVkFenceImportFlagsKHR;
       handleType:TVkExternalFenceHandleTypeFlagBitsKHR;
       handle:THANDLE;
       name:PWideChar;
     end;

{$ifdef Windows}
     PPVkExportFenceWin32HandleInfoKHR=^PVkExportFenceWin32HandleInfoKHR;
     PVkExportFenceWin32HandleInfoKHR=^TVkExportFenceWin32HandleInfoKHR;
     TVkExportFenceWin32HandleInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR
       pNext:PVkVoid;
       pAttributes:PSecurityAttributes;
       dwAccess:TVkUInt32;
       name:PWideChar;
     end;
{$endif}

     PPVkFenceGetWin32HandleInfoKHR=^PVkFenceGetWin32HandleInfoKHR;
     PVkFenceGetWin32HandleInfoKHR=^TVkFenceGetWin32HandleInfoKHR;
     TVkFenceGetWin32HandleInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR
       pNext:PVkVoid;
       fence:TVkFence;
       handleType:TVkExternalFenceHandleTypeFlagBitsKHR;
     end;

     PPVkImportFenceFdInfoKHR=^PVkImportFenceFdInfoKHR;
     PVkImportFenceFdInfoKHR=^TVkImportFenceFdInfoKHR;
     TVkImportFenceFdInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR
       pNext:PVkVoid;
       fence:TVkFence;
       flags:TVkFenceImportFlagsKHR;
       handleType:TVkExternalFenceHandleTypeFlagBitsKHR;
       fd:TVkInt32;
     end;

     PPVkFenceGetFdInfoKHR=^PVkFenceGetFdInfoKHR;
     PVkFenceGetFdInfoKHR=^TVkFenceGetFdInfoKHR;
     TVkFenceGetFdInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR
       pNext:PVkVoid;
       fence:TVkFence;
       handleType:TVkExternalFenceHandleTypeFlagBitsKHR;
     end;

     PPVkPhysicalDeviceMultiviewFeaturesKHX=^PVkPhysicalDeviceMultiviewFeaturesKHX;
     PVkPhysicalDeviceMultiviewFeaturesKHX=^TVkPhysicalDeviceMultiviewFeaturesKHX;
     TVkPhysicalDeviceMultiviewFeaturesKHX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHX
       pNext:PVkVoid;
       multiview:TVkBool32;
       multiviewGeometryShader:TVkBool32;
       multiviewTessellationShader:TVkBool32;
     end;

     PPVkPhysicalDeviceMultiviewPropertiesKHX=^PVkPhysicalDeviceMultiviewPropertiesKHX;
     PVkPhysicalDeviceMultiviewPropertiesKHX=^TVkPhysicalDeviceMultiviewPropertiesKHX;
     TVkPhysicalDeviceMultiviewPropertiesKHX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHX
       pNext:PVkVoid;
       maxMultiviewViewCount:TVkUInt32;
       maxMultiviewInstanceIndex:TVkUInt32;
     end;

     PPVkRenderPassMultiviewCreateInfoKHX=^PVkRenderPassMultiviewCreateInfoKHX;
     PVkRenderPassMultiviewCreateInfoKHX=^TVkRenderPassMultiviewCreateInfoKHX;
     TVkRenderPassMultiviewCreateInfoKHX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHX
       pNext:PVkVoid;
       subpassCount:TVkUInt32;
       pViewMasks:PVkUInt32;
       dependencyCount:TVkUInt32;
       pViewOffsets:PVkInt32;
       correlationMaskCount:TVkUInt32;
       pCorrelationMasks:PVkUInt32;
     end;

     PPVkSurfaceCapabilities2EXT=^PVkSurfaceCapabilities2EXT;
     PVkSurfaceCapabilities2EXT=^TVkSurfaceCapabilities2EXT;
     TVkSurfaceCapabilities2EXT=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT
       pNext:PVkVoid;
       minImageCount:TVkUInt32;
       maxImageCount:TVkUInt32;
       currentExtent:TVkExtent2D;
       minImageExtent:TVkExtent2D;
       maxImageExtent:TVkExtent2D;
       maxImageArrayLayers:TVkUInt32;
       supportedTransforms:TVkSurfaceTransformFlagsKHR;
       currentTransform:TVkSurfaceTransformFlagBitsKHR;
       supportedCompositeAlpha:TVkCompositeAlphaFlagsKHR;
       supportedUsageFlags:TVkImageUsageFlags;
       supportedSurfaceCounters:TVkSurfaceCounterFlagsEXT;
     end;

     PPVkDisplayPowerInfoEXT=^PVkDisplayPowerInfoEXT;
     PVkDisplayPowerInfoEXT=^TVkDisplayPowerInfoEXT;
     TVkDisplayPowerInfoEXT=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT
       pNext:PVkVoid;
       powerState:TVkDisplayPowerStateEXT;
     end;

     PPVkDeviceEventInfoEXT=^PVkDeviceEventInfoEXT;
     PVkDeviceEventInfoEXT=^TVkDeviceEventInfoEXT;
     TVkDeviceEventInfoEXT=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT
       pNext:PVkVoid;
       deviceEvent:TVkDeviceEventTypeEXT;
     end;

     PPVkDisplayEventInfoEXT=^PVkDisplayEventInfoEXT;
     PVkDisplayEventInfoEXT=^TVkDisplayEventInfoEXT;
     TVkDisplayEventInfoEXT=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT
       pNext:PVkVoid;
       displayEvent:TVkDisplayEventTypeEXT;
     end;

     PPVkSwapchainCounterCreateInfoEXT=^PVkSwapchainCounterCreateInfoEXT;
     PVkSwapchainCounterCreateInfoEXT=^TVkSwapchainCounterCreateInfoEXT;
     TVkSwapchainCounterCreateInfoEXT=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT
       pNext:PVkVoid;
       surfaceCounters:TVkSurfaceCounterFlagsEXT;
     end;

     PPVkPhysicalDeviceGroupPropertiesKHX=^PVkPhysicalDeviceGroupPropertiesKHX;
     PVkPhysicalDeviceGroupPropertiesKHX=^TVkPhysicalDeviceGroupPropertiesKHX;
     TVkPhysicalDeviceGroupPropertiesKHX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHX
       pNext:PVkVoid;
       physicalDeviceCount:TVkUInt32;
       physicalDevices:array[0..VK_MAX_DEVICE_GROUP_SIZE_KHX-1] of TVkPhysicalDevice;
       subsetAllocation:TVkBool32;
     end;

     PPVkMemoryAllocateFlagsInfoKHX=^PVkMemoryAllocateFlagsInfoKHX;
     PVkMemoryAllocateFlagsInfoKHX=^TVkMemoryAllocateFlagsInfoKHX;
     TVkMemoryAllocateFlagsInfoKHX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHX
       pNext:PVkVoid;
       flags:TVkMemoryAllocateFlagsKHX;
       deviceMask:TVkUInt32;
     end;

     PPVkBindBufferMemoryInfoKHX=^PVkBindBufferMemoryInfoKHX;
     PVkBindBufferMemoryInfoKHX=^TVkBindBufferMemoryInfoKHX;
     TVkBindBufferMemoryInfoKHX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHX
       pNext:PVkVoid;
       buffer:TVkBuffer;
       memory:TVkDeviceMemory;
       memoryOffset:TVkDeviceSize;
       deviceIndexCount:TVkUInt32;
       pDeviceIndices:PVkUInt32;
     end;

     PPVkBindImageMemoryInfoKHX=^PVkBindImageMemoryInfoKHX;
     PVkBindImageMemoryInfoKHX=^TVkBindImageMemoryInfoKHX;
     TVkBindImageMemoryInfoKHX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHX
       pNext:PVkVoid;
       image:TVkImage;
       memory:TVkDeviceMemory;
       memoryOffset:TVkDeviceSize;
       deviceIndexCount:TVkUInt32;
       pDeviceIndices:PVkUInt32;
       SFRRectCount:TVkUInt32;
       pSFRRects:PVkRect2D;
     end;

     PPVkDeviceGroupRenderPassBeginInfoKHX=^PVkDeviceGroupRenderPassBeginInfoKHX;
     PVkDeviceGroupRenderPassBeginInfoKHX=^TVkDeviceGroupRenderPassBeginInfoKHX;
     TVkDeviceGroupRenderPassBeginInfoKHX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHX
       pNext:PVkVoid;
       deviceMask:TVkUInt32;
       deviceRenderAreaCount:TVkUInt32;
       pDeviceRenderAreas:PVkRect2D;
     end;

     PPVkDeviceGroupCommandBufferBeginInfoKHX=^PVkDeviceGroupCommandBufferBeginInfoKHX;
     PVkDeviceGroupCommandBufferBeginInfoKHX=^TVkDeviceGroupCommandBufferBeginInfoKHX;
     TVkDeviceGroupCommandBufferBeginInfoKHX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHX
       pNext:PVkVoid;
       deviceMask:TVkUInt32;
     end;

     PPVkDeviceGroupSubmitInfoKHX=^PVkDeviceGroupSubmitInfoKHX;
     PVkDeviceGroupSubmitInfoKHX=^TVkDeviceGroupSubmitInfoKHX;
     TVkDeviceGroupSubmitInfoKHX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHX
       pNext:PVkVoid;
       waitSemaphoreCount:TVkUInt32;
       pWaitSemaphoreDeviceIndices:PVkUInt32;
       commandBufferCount:TVkUInt32;
       pCommandBufferDeviceMasks:PVkUInt32;
       signalSemaphoreCount:TVkUInt32;
       pSignalSemaphoreDeviceIndices:PVkUInt32;
     end;

     PPVkDeviceGroupBindSparseInfoKHX=^PVkDeviceGroupBindSparseInfoKHX;
     PVkDeviceGroupBindSparseInfoKHX=^TVkDeviceGroupBindSparseInfoKHX;
     TVkDeviceGroupBindSparseInfoKHX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHX
       pNext:PVkVoid;
       resourceDeviceIndex:TVkUInt32;
       memoryDeviceIndex:TVkUInt32;
     end;

     PPVkDeviceGroupPresentCapabilitiesKHX=^PVkDeviceGroupPresentCapabilitiesKHX;
     PVkDeviceGroupPresentCapabilitiesKHX=^TVkDeviceGroupPresentCapabilitiesKHX;
     TVkDeviceGroupPresentCapabilitiesKHX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHX
       pNext:PVkVoid;
       presentMask:array[0..VK_MAX_DEVICE_GROUP_SIZE_KHX-1] of TVkUInt32;
       modes:TVkDeviceGroupPresentModeFlagsKHX;
     end;

     PPVkImageSwapchainCreateInfoKHX=^PVkImageSwapchainCreateInfoKHX;
     PVkImageSwapchainCreateInfoKHX=^TVkImageSwapchainCreateInfoKHX;
     TVkImageSwapchainCreateInfoKHX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHX
       pNext:PVkVoid;
       swapchain:TVkSwapchainKHR;
     end;

     PPVkBindImageMemorySwapchainInfoKHX=^PVkBindImageMemorySwapchainInfoKHX;
     PVkBindImageMemorySwapchainInfoKHX=^TVkBindImageMemorySwapchainInfoKHX;
     TVkBindImageMemorySwapchainInfoKHX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHX
       pNext:PVkVoid;
       swapchain:TVkSwapchainKHR;
       imageIndex:TVkUInt32;
     end;

     PPVkAcquireNextImageInfoKHX=^PVkAcquireNextImageInfoKHX;
     PVkAcquireNextImageInfoKHX=^TVkAcquireNextImageInfoKHX;
     TVkAcquireNextImageInfoKHX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHX
       pNext:PVkVoid;
       swapchain:TVkSwapchainKHR;
       timeout:TVkUInt64;
       semaphore:TVkSemaphore;
       fence:TVkFence;
       deviceMask:TVkUInt32;
     end;

     PPVkDeviceGroupPresentInfoKHX=^PVkDeviceGroupPresentInfoKHX;
     PVkDeviceGroupPresentInfoKHX=^TVkDeviceGroupPresentInfoKHX;
     TVkDeviceGroupPresentInfoKHX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHX
       pNext:PVkVoid;
       swapchainCount:TVkUInt32;
       pDeviceMasks:PVkUInt32;
       mode:TVkDeviceGroupPresentModeFlagBitsKHX;
     end;

     PPVkDeviceGroupDeviceCreateInfoKHX=^PVkDeviceGroupDeviceCreateInfoKHX;
     PVkDeviceGroupDeviceCreateInfoKHX=^TVkDeviceGroupDeviceCreateInfoKHX;
     TVkDeviceGroupDeviceCreateInfoKHX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHX
       pNext:PVkVoid;
       physicalDeviceCount:TVkUInt32;
       pPhysicalDevices:PVkPhysicalDevice;
     end;

     PPVkDeviceGroupSwapchainCreateInfoKHX=^PVkDeviceGroupSwapchainCreateInfoKHX;
     PVkDeviceGroupSwapchainCreateInfoKHX=^TVkDeviceGroupSwapchainCreateInfoKHX;
     TVkDeviceGroupSwapchainCreateInfoKHX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHX
       pNext:PVkVoid;
       modes:TVkDeviceGroupPresentModeFlagsKHX;
     end;

     PPVkDescriptorUpdateTemplateEntryKHR=^PVkDescriptorUpdateTemplateEntryKHR;
     PVkDescriptorUpdateTemplateEntryKHR=^TVkDescriptorUpdateTemplateEntryKHR;
     TVkDescriptorUpdateTemplateEntryKHR=record
       dstBinding:TVkUInt32;
       dstArrayElement:TVkUInt32;
       descriptorCount:TVkUInt32;
       descriptorType:TVkDescriptorType;
       offset:TVkSize;
       stride:TVkSize;
     end;

     PPVkDescriptorUpdateTemplateCreateInfoKHR=^PVkDescriptorUpdateTemplateCreateInfoKHR;
     PVkDescriptorUpdateTemplateCreateInfoKHR=^TVkDescriptorUpdateTemplateCreateInfoKHR;
     TVkDescriptorUpdateTemplateCreateInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR
       pNext:PVkVoid;
       flags:TVkDescriptorUpdateTemplateCreateFlagsKHR;
       descriptorUpdateEntryCount:TVkUInt32;
       pDescriptorUpdateEntries:PVkDescriptorUpdateTemplateEntryKHR;
       templateType:TVkDescriptorUpdateTemplateTypeKHR;
       descriptorSetLayout:TVkDescriptorSetLayout;
       pipelineBindPoint:TVkPipelineBindPoint;
       pipelineLayout:TVkPipelineLayout;
       set_:TVkUInt32;
     end;

     PPVkXYColorEXT=^PVkXYColorEXT;
     PVkXYColorEXT=^TVkXYColorEXT;
     TVkXYColorEXT=record
       x:TVkFloat;
       y:TVkFloat;
     end;

     PPVkHdrMetadataEXT=^PVkHdrMetadataEXT;
     PVkHdrMetadataEXT=^TVkHdrMetadataEXT;
     TVkHdrMetadataEXT=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_HDR_METADATA_EXT
       pNext:PVkVoid;
       displayPrimaryRed:TVkXYColorEXT;
       displayPrimaryGreen:TVkXYColorEXT;
       displayPrimaryBlue:TVkXYColorEXT;
       whitePoint:TVkXYColorEXT;
       maxLuminance:TVkFloat;
       minLuminance:TVkFloat;
       maxContentLightLevel:TVkFloat;
       maxFrameAverageLightLevel:TVkFloat;
     end;

     PPVkRefreshCycleDurationGOOGLE=^PVkRefreshCycleDurationGOOGLE;
     PVkRefreshCycleDurationGOOGLE=^TVkRefreshCycleDurationGOOGLE;
     TVkRefreshCycleDurationGOOGLE=record
       refreshDuration:TVkUInt64;
     end;

     PPVkPastPresentationTimingGOOGLE=^PVkPastPresentationTimingGOOGLE;
     PVkPastPresentationTimingGOOGLE=^TVkPastPresentationTimingGOOGLE;
     TVkPastPresentationTimingGOOGLE=record
       presentID:TVkUInt32;
       desiredPresentTime:TVkUInt64;
       actualPresentTime:TVkUInt64;
       earliestPresentTime:TVkUInt64;
       presentMargin:TVkUInt64;
     end;

     PPVkPresentTimeGOOGLE=^PVkPresentTimeGOOGLE;
     PVkPresentTimeGOOGLE=^TVkPresentTimeGOOGLE;
     TVkPresentTimeGOOGLE=record
       presentID:TVkUInt32;
       desiredPresentTime:TVkUInt64;
     end;

     PPVkPresentTimesInfoGOOGLE=^PVkPresentTimesInfoGOOGLE;
     PVkPresentTimesInfoGOOGLE=^TVkPresentTimesInfoGOOGLE;
     TVkPresentTimesInfoGOOGLE=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE
       pNext:PVkVoid;
       swapchainCount:TVkUInt32;
       pTimes:PVkPresentTimeGOOGLE;
     end;

{$ifdef MoltenVK_IOS}
     PPVkIOSSurfaceCreateInfoMVK=^PVkIOSSurfaceCreateInfoMVK;
     PVkIOSSurfaceCreateInfoMVK=^TVkIOSSurfaceCreateInfoMVK;
     TVkIOSSurfaceCreateInfoMVK=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK
       pNext:PVkVoid;
       flags:TVkIOSSurfaceCreateFlagsMVK;
       pView:PVkVoid;
     end;
{$endif}

{$ifdef MoltenVK_MacOS}
     PPVkMacOSSurfaceCreateInfoMVK=^PVkMacOSSurfaceCreateInfoMVK;
     PVkMacOSSurfaceCreateInfoMVK=^TVkMacOSSurfaceCreateInfoMVK;
     TVkMacOSSurfaceCreateInfoMVK=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK
       pNext:PVkVoid;
       flags:TVkMacOSSurfaceCreateFlagsMVK;
       pView:PVkVoid;
     end;
{$endif}

     PPVkViewportWScalingNV=^PVkViewportWScalingNV;
     PVkViewportWScalingNV=^TVkViewportWScalingNV;
     TVkViewportWScalingNV=record
       xcoeff:TVkFloat;
       ycoeff:TVkFloat;
     end;

     PPVkPipelineViewportWScalingStateCreateInfoNV=^PVkPipelineViewportWScalingStateCreateInfoNV;
     PVkPipelineViewportWScalingStateCreateInfoNV=^TVkPipelineViewportWScalingStateCreateInfoNV;
     TVkPipelineViewportWScalingStateCreateInfoNV=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
       pNext:PVkVoid;
       viewportWScalingEnable:TVkBool32;
       viewportCount:TVkUInt32;
       pViewportWScalings:PVkViewportWScalingNV;
     end;

     PPVkViewportSwizzleNV=^PVkViewportSwizzleNV;
     PVkViewportSwizzleNV=^TVkViewportSwizzleNV;
     TVkViewportSwizzleNV=record
       x:TVkViewportCoordinateSwizzleNV;
       y:TVkViewportCoordinateSwizzleNV;
       z:TVkViewportCoordinateSwizzleNV;
       w:TVkViewportCoordinateSwizzleNV;
     end;

     PPVkPipelineViewportSwizzleStateCreateInfoNV=^PVkPipelineViewportSwizzleStateCreateInfoNV;
     PVkPipelineViewportSwizzleStateCreateInfoNV=^TVkPipelineViewportSwizzleStateCreateInfoNV;
     TVkPipelineViewportSwizzleStateCreateInfoNV=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV
       pNext:PVkVoid;
       flags:TVkPipelineViewportSwizzleStateCreateFlagsNV;
       viewportCount:TVkUInt32;
       pViewportSwizzles:PVkViewportSwizzleNV;
     end;

     PPVkPhysicalDeviceDiscardRectanglePropertiesEXT=^PVkPhysicalDeviceDiscardRectanglePropertiesEXT;
     PVkPhysicalDeviceDiscardRectanglePropertiesEXT=^TVkPhysicalDeviceDiscardRectanglePropertiesEXT;
     TVkPhysicalDeviceDiscardRectanglePropertiesEXT=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT
       pNext:PVkVoid;
       maxDiscardRectangles:TVkUInt32;
     end;

     PPVkPipelineDiscardRectangleStateCreateInfoEXT=^PVkPipelineDiscardRectangleStateCreateInfoEXT;
     PVkPipelineDiscardRectangleStateCreateInfoEXT=^TVkPipelineDiscardRectangleStateCreateInfoEXT;
     TVkPipelineDiscardRectangleStateCreateInfoEXT=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT
       pNext:PVkVoid;
       flags:TVkPipelineDiscardRectangleStateCreateFlagsEXT;
       discardRectangleMode:TVkDiscardRectangleModeEXT;
       discardRectangleCount:TVkUInt32;
       pDiscardRectangles:PVkRect2D;
     end;

     PPVkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX=^PVkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX;
     PVkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX=^TVkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX;
     TVkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX
       pNext:PVkVoid;
       perViewPositionAllComponents:TVkBool32;
     end;

     PPVkPhysicalDeviceSurfaceInfo2KHR=^PVkPhysicalDeviceSurfaceInfo2KHR;
     PVkPhysicalDeviceSurfaceInfo2KHR=^TVkPhysicalDeviceSurfaceInfo2KHR;
     TVkPhysicalDeviceSurfaceInfo2KHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR
       pNext:PVkVoid;
       surface:TVkSurfaceKHR;
     end;

     PPVkSurfaceCapabilities2KHR=^PVkSurfaceCapabilities2KHR;
     PVkSurfaceCapabilities2KHR=^TVkSurfaceCapabilities2KHR;
     TVkSurfaceCapabilities2KHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR
       pNext:PVkVoid;
       surfaceCapabilities:TVkSurfaceCapabilitiesKHR;
     end;

     PPVkSurfaceFormat2KHR=^PVkSurfaceFormat2KHR;
     PVkSurfaceFormat2KHR=^TVkSurfaceFormat2KHR;
     TVkSurfaceFormat2KHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR
       pNext:PVkVoid;
       surfaceFormat:TVkSurfaceFormatKHR;
     end;

     PPVkSharedPresentSurfaceCapabilitiesKHR=^PVkSharedPresentSurfaceCapabilitiesKHR;
     PVkSharedPresentSurfaceCapabilitiesKHR=^TVkSharedPresentSurfaceCapabilitiesKHR;
     TVkSharedPresentSurfaceCapabilitiesKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR
       pNext:PVkVoid;
       sharedPresentSupportedUsageFlags:TVkImageUsageFlags;
     end;

     PPVkPhysicalDevice16BitStorageFeaturesKHR=^PVkPhysicalDevice16BitStorageFeaturesKHR;
     PVkPhysicalDevice16BitStorageFeaturesKHR=^TVkPhysicalDevice16BitStorageFeaturesKHR;
     TVkPhysicalDevice16BitStorageFeaturesKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR
       pNext:PVkVoid;
       storageBuffer16BitAccess:TVkBool32;
       uniformAndStorageBuffer16BitAccess:TVkBool32;
       storagePushConstant16:TVkBool32;
       storageInputOutput16:TVkBool32;
     end;

     PPVkBufferMemoryRequirementsInfo2KHR=^PVkBufferMemoryRequirementsInfo2KHR;
     PVkBufferMemoryRequirementsInfo2KHR=^TVkBufferMemoryRequirementsInfo2KHR;
     TVkBufferMemoryRequirementsInfo2KHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR
       pNext:PVkVoid;
       buffer:TVkBuffer;
     end;

     PPVkImageMemoryRequirementsInfo2KHR=^PVkImageMemoryRequirementsInfo2KHR;
     PVkImageMemoryRequirementsInfo2KHR=^TVkImageMemoryRequirementsInfo2KHR;
     TVkImageMemoryRequirementsInfo2KHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR
       pNext:PVkVoid;
       image:TVkImage;
     end;

     PPVkImageSparseMemoryRequirementsInfo2KHR=^PVkImageSparseMemoryRequirementsInfo2KHR;
     PVkImageSparseMemoryRequirementsInfo2KHR=^TVkImageSparseMemoryRequirementsInfo2KHR;
     TVkImageSparseMemoryRequirementsInfo2KHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR
       pNext:PVkVoid;
       image:TVkImage;
     end;

     PPVkMemoryRequirements2KHR=^PVkMemoryRequirements2KHR;
     PVkMemoryRequirements2KHR=^TVkMemoryRequirements2KHR;
     TVkMemoryRequirements2KHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR
       pNext:PVkVoid;
       memoryRequirements:TVkMemoryRequirements;
     end;

     PPVkSparseImageMemoryRequirements2KHR=^PVkSparseImageMemoryRequirements2KHR;
     PVkSparseImageMemoryRequirements2KHR=^TVkSparseImageMemoryRequirements2KHR;
     TVkSparseImageMemoryRequirements2KHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR
       pNext:PVkVoid;
       memoryRequirements:TVkSparseImageMemoryRequirements;
     end;

     PPVkMemoryDedicatedRequirementsKHR=^PVkMemoryDedicatedRequirementsKHR;
     PVkMemoryDedicatedRequirementsKHR=^TVkMemoryDedicatedRequirementsKHR;
     TVkMemoryDedicatedRequirementsKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR
       pNext:PVkVoid;
       prefersDedicatedAllocation:TVkBool32;
       requiresDedicatedAllocation:TVkBool32;
     end;

     PPVkMemoryDedicatedAllocateInfoKHR=^PVkMemoryDedicatedAllocateInfoKHR;
     PVkMemoryDedicatedAllocateInfoKHR=^TVkMemoryDedicatedAllocateInfoKHR;
     TVkMemoryDedicatedAllocateInfoKHR=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR
       pNext:PVkVoid;
       image:TVkImage;
       buffer:TVkBuffer;
     end;

     PPVkTextureLODGatherFormatPropertiesAMD=^PVkTextureLODGatherFormatPropertiesAMD;
     PVkTextureLODGatherFormatPropertiesAMD=^TVkTextureLODGatherFormatPropertiesAMD;
     TVkTextureLODGatherFormatPropertiesAMD=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD
       pNext:PVkVoid;
       supportsTextureGatherLODBiasAMD:TVkBool32;
     end;

     PPVkPipelineCoverageToColorStateCreateInfoNV=^PVkPipelineCoverageToColorStateCreateInfoNV;
     PVkPipelineCoverageToColorStateCreateInfoNV=^TVkPipelineCoverageToColorStateCreateInfoNV;
     TVkPipelineCoverageToColorStateCreateInfoNV=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV
       pNext:PVkVoid;
       flags:TVkPipelineCoverageToColorStateCreateFlagsNV;
       coverageToColorEnable:TVkBool32;
       coverageToColorLocation:TVkUInt32;
     end;

     PPVkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT=^PVkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT;
     PVkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT=^TVkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT;
     TVkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT
       pNext:PVkVoid;
       filterMinmaxSingleComponentFormats:TVkBool32;
       filterMinmaxImageComponentMapping:TVkBool32;
     end;

     PPVkSamplerReductionModeCreateInfoEXT=^PVkSamplerReductionModeCreateInfoEXT;
     PVkSamplerReductionModeCreateInfoEXT=^TVkSamplerReductionModeCreateInfoEXT;
     TVkSamplerReductionModeCreateInfoEXT=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT
       pNext:PVkVoid;
       reductionMode:TVkSamplerReductionModeEXT;
     end;

     PPVkPhysicalDeviceBlendOperationAdvancedFeaturesEXT=^PVkPhysicalDeviceBlendOperationAdvancedFeaturesEXT;
     PVkPhysicalDeviceBlendOperationAdvancedFeaturesEXT=^TVkPhysicalDeviceBlendOperationAdvancedFeaturesEXT;
     TVkPhysicalDeviceBlendOperationAdvancedFeaturesEXT=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT
       pNext:PVkVoid;
       advancedBlendCoherentOperations:TVkBool32;
     end;

     PPVkPhysicalDeviceBlendOperationAdvancedPropertiesEXT=^PVkPhysicalDeviceBlendOperationAdvancedPropertiesEXT;
     PVkPhysicalDeviceBlendOperationAdvancedPropertiesEXT=^TVkPhysicalDeviceBlendOperationAdvancedPropertiesEXT;
     TVkPhysicalDeviceBlendOperationAdvancedPropertiesEXT=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT
       pNext:PVkVoid;
       advancedBlendMaxColorAttachments:TVkUInt32;
       advancedBlendIndependentBlend:TVkBool32;
       advancedBlendNonPremultipliedSrcColor:TVkBool32;
       advancedBlendNonPremultipliedDstColor:TVkBool32;
       advancedBlendCorrelatedOverlap:TVkBool32;
       advancedBlendAllOperations:TVkBool32;
     end;

     PPVkPipelineColorBlendAdvancedStateCreateInfoEXT=^PVkPipelineColorBlendAdvancedStateCreateInfoEXT;
     PVkPipelineColorBlendAdvancedStateCreateInfoEXT=^TVkPipelineColorBlendAdvancedStateCreateInfoEXT;
     TVkPipelineColorBlendAdvancedStateCreateInfoEXT=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT
       pNext:PVkVoid;
       srcPremultiplied:TVkBool32;
       dstPremultiplied:TVkBool32;
       blendOverlap:TVkBlendOverlapEXT;
     end;

     PPVkPipelineCoverageModulationStateCreateInfoNV=^PVkPipelineCoverageModulationStateCreateInfoNV;
     PVkPipelineCoverageModulationStateCreateInfoNV=^TVkPipelineCoverageModulationStateCreateInfoNV;
     TVkPipelineCoverageModulationStateCreateInfoNV=record
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV
       pNext:PVkVoid;
       flags:TVkPipelineCoverageModulationStateCreateFlagsNV;
       coverageModulationMode:TVkCoverageModulationModeNV;
       coverageModulationTableEnable:TVkBool32;
       coverageModulationTableCount:TVkUInt32;
       pCoverageModulationTable:PVkFloat;
     end;


//========== Vulkan Functions Definitions ================ Total: 291

     TvkCreateInstance=function(const pCreateInfo:PVkInstanceCreateInfo;const pAllocator:PVkAllocationCallbacks;pInstance:PVkInstance):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyInstance=procedure(instance:TVkInstance;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkEnumeratePhysicalDevices=function(instance:TVkInstance;pPhysicalDeviceCount:PVkUInt32;pPhysicalDevices:PVkPhysicalDevice):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetDeviceProcAddr=function(device:TVkDevice;const pName:PVkChar):TPFN_vkVoidFunction; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetInstanceProcAddr=function(instance:TVkInstance;const pName:PVkChar):TPFN_vkVoidFunction; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceProperties=procedure(physicalDevice:TVkPhysicalDevice;pProperties:PVkPhysicalDeviceProperties); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceQueueFamilyProperties=procedure(physicalDevice:TVkPhysicalDevice;pQueueFamilyPropertyCount:PVkUInt32;pQueueFamilyProperties:PVkQueueFamilyProperties); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceMemoryProperties=procedure(physicalDevice:TVkPhysicalDevice;pMemoryProperties:PVkPhysicalDeviceMemoryProperties); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceFeatures=procedure(physicalDevice:TVkPhysicalDevice;pFeatures:PVkPhysicalDeviceFeatures); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceFormatProperties=procedure(physicalDevice:TVkPhysicalDevice;format:TVkFormat;pFormatProperties:PVkFormatProperties); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceImageFormatProperties=function(physicalDevice:TVkPhysicalDevice;format:TVkFormat;type_:TVkImageType;tiling:TVkImageTiling;usage:TVkImageUsageFlags;flags:TVkImageCreateFlags;pImageFormatProperties:PVkImageFormatProperties):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateDevice=function(physicalDevice:TVkPhysicalDevice;const pCreateInfo:PVkDeviceCreateInfo;const pAllocator:PVkAllocationCallbacks;pDevice:PVkDevice):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyDevice=procedure(device:TVkDevice;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkEnumerateInstanceLayerProperties=function(pPropertyCount:PVkUInt32;pProperties:PVkLayerProperties):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkEnumerateInstanceExtensionProperties=function(const pLayerName:PVkChar;pPropertyCount:PVkUInt32;pProperties:PVkExtensionProperties):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkEnumerateDeviceLayerProperties=function(physicalDevice:TVkPhysicalDevice;pPropertyCount:PVkUInt32;pProperties:PVkLayerProperties):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkEnumerateDeviceExtensionProperties=function(physicalDevice:TVkPhysicalDevice;const pLayerName:PVkChar;pPropertyCount:PVkUInt32;pProperties:PVkExtensionProperties):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetDeviceQueue=procedure(device:TVkDevice;queueFamilyIndex:TVkUInt32;queueIndex:TVkUInt32;pQueue:PVkQueue); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkQueueSubmit=function(queue:TVkQueue;submitCount:TVkUInt32;const pSubmits:PVkSubmitInfo;fence:TVkFence):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkQueueWaitIdle=function(queue:TVkQueue):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDeviceWaitIdle=function(device:TVkDevice):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkAllocateMemory=function(device:TVkDevice;const pAllocateInfo:PVkMemoryAllocateInfo;const pAllocator:PVkAllocationCallbacks;pMemory:PVkDeviceMemory):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkFreeMemory=procedure(device:TVkDevice;memory:TVkDeviceMemory;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkMapMemory=function(device:TVkDevice;memory:TVkDeviceMemory;offset:TVkDeviceSize;size:TVkDeviceSize;flags:TVkMemoryMapFlags;ppData:PPVkVoid):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkUnmapMemory=procedure(device:TVkDevice;memory:TVkDeviceMemory); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkFlushMappedMemoryRanges=function(device:TVkDevice;memoryRangeCount:TVkUInt32;const pMemoryRanges:PVkMappedMemoryRange):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkInvalidateMappedMemoryRanges=function(device:TVkDevice;memoryRangeCount:TVkUInt32;const pMemoryRanges:PVkMappedMemoryRange):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetDeviceMemoryCommitment=procedure(device:TVkDevice;memory:TVkDeviceMemory;pCommittedMemoryInBytes:PVkDeviceSize); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetBufferMemoryRequirements=procedure(device:TVkDevice;buffer:TVkBuffer;pMemoryRequirements:PVkMemoryRequirements); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkBindBufferMemory=function(device:TVkDevice;buffer:TVkBuffer;memory:TVkDeviceMemory;memoryOffset:TVkDeviceSize):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetImageMemoryRequirements=procedure(device:TVkDevice;image:TVkImage;pMemoryRequirements:PVkMemoryRequirements); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkBindImageMemory=function(device:TVkDevice;image:TVkImage;memory:TVkDeviceMemory;memoryOffset:TVkDeviceSize):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetImageSparseMemoryRequirements=procedure(device:TVkDevice;image:TVkImage;pSparseMemoryRequirementCount:PVkUInt32;pSparseMemoryRequirements:PVkSparseImageMemoryRequirements); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceSparseImageFormatProperties=procedure(physicalDevice:TVkPhysicalDevice;format:TVkFormat;type_:TVkImageType;samples:TVkSampleCountFlagBits;usage:TVkImageUsageFlags;tiling:TVkImageTiling;pPropertyCount:PVkUInt32;pProperties:PVkSparseImageFormatProperties); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkQueueBindSparse=function(queue:TVkQueue;bindInfoCount:TVkUInt32;const pBindInfo:PVkBindSparseInfo;fence:TVkFence):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateFence=function(device:TVkDevice;const pCreateInfo:PVkFenceCreateInfo;const pAllocator:PVkAllocationCallbacks;pFence:PVkFence):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyFence=procedure(device:TVkDevice;fence:TVkFence;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkResetFences=function(device:TVkDevice;fenceCount:TVkUInt32;const pFences:PVkFence):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetFenceStatus=function(device:TVkDevice;fence:TVkFence):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkWaitForFences=function(device:TVkDevice;fenceCount:TVkUInt32;const pFences:PVkFence;waitAll:TVkBool32;timeout:TVkUInt64):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateSemaphore=function(device:TVkDevice;const pCreateInfo:PVkSemaphoreCreateInfo;const pAllocator:PVkAllocationCallbacks;pSemaphore:PVkSemaphore):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroySemaphore=procedure(device:TVkDevice;semaphore:TVkSemaphore;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateEvent=function(device:TVkDevice;const pCreateInfo:PVkEventCreateInfo;const pAllocator:PVkAllocationCallbacks;pEvent:PVkEvent):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyEvent=procedure(device:TVkDevice;event:TVkEvent;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetEventStatus=function(device:TVkDevice;event:TVkEvent):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkSetEvent=function(device:TVkDevice;event:TVkEvent):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkResetEvent=function(device:TVkDevice;event:TVkEvent):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateQueryPool=function(device:TVkDevice;const pCreateInfo:PVkQueryPoolCreateInfo;const pAllocator:PVkAllocationCallbacks;pQueryPool:PVkQueryPool):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyQueryPool=procedure(device:TVkDevice;queryPool:TVkQueryPool;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetQueryPoolResults=function(device:TVkDevice;queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32;dataSize:TVkSize;pData:PVkVoid;stride:TVkDeviceSize;flags:TVkQueryResultFlags):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateBuffer=function(device:TVkDevice;const pCreateInfo:PVkBufferCreateInfo;const pAllocator:PVkAllocationCallbacks;pBuffer:PVkBuffer):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyBuffer=procedure(device:TVkDevice;buffer:TVkBuffer;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateBufferView=function(device:TVkDevice;const pCreateInfo:PVkBufferViewCreateInfo;const pAllocator:PVkAllocationCallbacks;pView:PVkBufferView):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyBufferView=procedure(device:TVkDevice;bufferView:TVkBufferView;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateImage=function(device:TVkDevice;const pCreateInfo:PVkImageCreateInfo;const pAllocator:PVkAllocationCallbacks;pImage:PVkImage):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyImage=procedure(device:TVkDevice;image:TVkImage;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetImageSubresourceLayout=procedure(device:TVkDevice;image:TVkImage;const pSubresource:PVkImageSubresource;pLayout:PVkSubresourceLayout); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateImageView=function(device:TVkDevice;const pCreateInfo:PVkImageViewCreateInfo;const pAllocator:PVkAllocationCallbacks;pView:PVkImageView):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyImageView=procedure(device:TVkDevice;imageView:TVkImageView;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateShaderModule=function(device:TVkDevice;const pCreateInfo:PVkShaderModuleCreateInfo;const pAllocator:PVkAllocationCallbacks;pShaderModule:PVkShaderModule):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyShaderModule=procedure(device:TVkDevice;shaderModule:TVkShaderModule;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreatePipelineCache=function(device:TVkDevice;const pCreateInfo:PVkPipelineCacheCreateInfo;const pAllocator:PVkAllocationCallbacks;pPipelineCache:PVkPipelineCache):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyPipelineCache=procedure(device:TVkDevice;pipelineCache:TVkPipelineCache;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPipelineCacheData=function(device:TVkDevice;pipelineCache:TVkPipelineCache;pDataSize:PVkSize;pData:PVkVoid):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkMergePipelineCaches=function(device:TVkDevice;dstCache:TVkPipelineCache;srcCacheCount:TVkUInt32;const pSrcCaches:PVkPipelineCache):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateGraphicsPipelines=function(device:TVkDevice;pipelineCache:TVkPipelineCache;createInfoCount:TVkUInt32;const pCreateInfos:PVkGraphicsPipelineCreateInfo;const pAllocator:PVkAllocationCallbacks;pPipelines:PVkPipeline):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateComputePipelines=function(device:TVkDevice;pipelineCache:TVkPipelineCache;createInfoCount:TVkUInt32;const pCreateInfos:PVkComputePipelineCreateInfo;const pAllocator:PVkAllocationCallbacks;pPipelines:PVkPipeline):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyPipeline=procedure(device:TVkDevice;pipeline:TVkPipeline;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreatePipelineLayout=function(device:TVkDevice;const pCreateInfo:PVkPipelineLayoutCreateInfo;const pAllocator:PVkAllocationCallbacks;pPipelineLayout:PVkPipelineLayout):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyPipelineLayout=procedure(device:TVkDevice;pipelineLayout:TVkPipelineLayout;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateSampler=function(device:TVkDevice;const pCreateInfo:PVkSamplerCreateInfo;const pAllocator:PVkAllocationCallbacks;pSampler:PVkSampler):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroySampler=procedure(device:TVkDevice;sampler:TVkSampler;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateDescriptorSetLayout=function(device:TVkDevice;const pCreateInfo:PVkDescriptorSetLayoutCreateInfo;const pAllocator:PVkAllocationCallbacks;pSetLayout:PVkDescriptorSetLayout):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyDescriptorSetLayout=procedure(device:TVkDevice;descriptorSetLayout:TVkDescriptorSetLayout;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateDescriptorPool=function(device:TVkDevice;const pCreateInfo:PVkDescriptorPoolCreateInfo;const pAllocator:PVkAllocationCallbacks;pDescriptorPool:PVkDescriptorPool):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyDescriptorPool=procedure(device:TVkDevice;descriptorPool:TVkDescriptorPool;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkResetDescriptorPool=function(device:TVkDevice;descriptorPool:TVkDescriptorPool;flags:TVkDescriptorPoolResetFlags):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkAllocateDescriptorSets=function(device:TVkDevice;const pAllocateInfo:PVkDescriptorSetAllocateInfo;pDescriptorSets:PVkDescriptorSet):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkFreeDescriptorSets=function(device:TVkDevice;descriptorPool:TVkDescriptorPool;descriptorSetCount:TVkUInt32;const pDescriptorSets:PVkDescriptorSet):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkUpdateDescriptorSets=procedure(device:TVkDevice;descriptorWriteCount:TVkUInt32;const pDescriptorWrites:PVkWriteDescriptorSet;descriptorCopyCount:TVkUInt32;const pDescriptorCopies:PVkCopyDescriptorSet); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateFramebuffer=function(device:TVkDevice;const pCreateInfo:PVkFramebufferCreateInfo;const pAllocator:PVkAllocationCallbacks;pFramebuffer:PVkFramebuffer):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyFramebuffer=procedure(device:TVkDevice;framebuffer:TVkFramebuffer;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateRenderPass=function(device:TVkDevice;const pCreateInfo:PVkRenderPassCreateInfo;const pAllocator:PVkAllocationCallbacks;pRenderPass:PVkRenderPass):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyRenderPass=procedure(device:TVkDevice;renderPass:TVkRenderPass;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetRenderAreaGranularity=procedure(device:TVkDevice;renderPass:TVkRenderPass;pGranularity:PVkExtent2D); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateCommandPool=function(device:TVkDevice;const pCreateInfo:PVkCommandPoolCreateInfo;const pAllocator:PVkAllocationCallbacks;pCommandPool:PVkCommandPool):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyCommandPool=procedure(device:TVkDevice;commandPool:TVkCommandPool;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkResetCommandPool=function(device:TVkDevice;commandPool:TVkCommandPool;flags:TVkCommandPoolResetFlags):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkAllocateCommandBuffers=function(device:TVkDevice;const pAllocateInfo:PVkCommandBufferAllocateInfo;pCommandBuffers:PVkCommandBuffer):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkFreeCommandBuffers=procedure(device:TVkDevice;commandPool:TVkCommandPool;commandBufferCount:TVkUInt32;const pCommandBuffers:PVkCommandBuffer); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkBeginCommandBuffer=function(commandBuffer:TVkCommandBuffer;const pBeginInfo:PVkCommandBufferBeginInfo):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkEndCommandBuffer=function(commandBuffer:TVkCommandBuffer):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkResetCommandBuffer=function(commandBuffer:TVkCommandBuffer;flags:TVkCommandBufferResetFlags):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdBindPipeline=procedure(commandBuffer:TVkCommandBuffer;pipelineBindPoint:TVkPipelineBindPoint;pipeline:TVkPipeline); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetViewport=procedure(commandBuffer:TVkCommandBuffer;firstViewport:TVkUInt32;viewportCount:TVkUInt32;const pViewports:PVkViewport); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetScissor=procedure(commandBuffer:TVkCommandBuffer;firstScissor:TVkUInt32;scissorCount:TVkUInt32;const pScissors:PVkRect2D); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetLineWidth=procedure(commandBuffer:TVkCommandBuffer;lineWidth:TVkFloat); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetDepthBias=procedure(commandBuffer:TVkCommandBuffer;depthBiasConstantFactor:TVkFloat;depthBiasClamp:TVkFloat;depthBiasSlopeFactor:TVkFloat); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetBlendConstants=procedure(commandBuffer:TVkCommandBuffer;const blendConstants:TVkFloat); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetDepthBounds=procedure(commandBuffer:TVkCommandBuffer;minDepthBounds:TVkFloat;maxDepthBounds:TVkFloat); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetStencilCompareMask=procedure(commandBuffer:TVkCommandBuffer;faceMask:TVkStencilFaceFlags;compareMask:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetStencilWriteMask=procedure(commandBuffer:TVkCommandBuffer;faceMask:TVkStencilFaceFlags;writeMask:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetStencilReference=procedure(commandBuffer:TVkCommandBuffer;faceMask:TVkStencilFaceFlags;reference:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdBindDescriptorSets=procedure(commandBuffer:TVkCommandBuffer;pipelineBindPoint:TVkPipelineBindPoint;layout:TVkPipelineLayout;firstSet:TVkUInt32;descriptorSetCount:TVkUInt32;const pDescriptorSets:PVkDescriptorSet;dynamicOffsetCount:TVkUInt32;const pDynamicOffsets:PVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdBindIndexBuffer=procedure(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;indexType:TVkIndexType); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdBindVertexBuffers=procedure(commandBuffer:TVkCommandBuffer;firstBinding:TVkUInt32;bindingCount:TVkUInt32;const pBuffers:PVkBuffer;const pOffsets:PVkDeviceSize); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdDraw=procedure(commandBuffer:TVkCommandBuffer;vertexCount:TVkUInt32;instanceCount:TVkUInt32;firstVertex:TVkUInt32;firstInstance:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdDrawIndexed=procedure(commandBuffer:TVkCommandBuffer;indexCount:TVkUInt32;instanceCount:TVkUInt32;firstIndex:TVkUInt32;vertexOffset:TVkInt32;firstInstance:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdDrawIndirect=procedure(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;drawCount:TVkUInt32;stride:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdDrawIndexedIndirect=procedure(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;drawCount:TVkUInt32;stride:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdDispatch=procedure(commandBuffer:TVkCommandBuffer;groupCountX:TVkUInt32;groupCountY:TVkUInt32;groupCountZ:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdDispatchIndirect=procedure(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdCopyBuffer=procedure(commandBuffer:TVkCommandBuffer;srcBuffer:TVkBuffer;dstBuffer:TVkBuffer;regionCount:TVkUInt32;const pRegions:PVkBufferCopy); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdCopyImage=procedure(commandBuffer:TVkCommandBuffer;srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageCopy); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdBlitImage=procedure(commandBuffer:TVkCommandBuffer;srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageBlit;filter:TVkFilter); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdCopyBufferToImage=procedure(commandBuffer:TVkCommandBuffer;srcBuffer:TVkBuffer;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkBufferImageCopy); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdCopyImageToBuffer=procedure(commandBuffer:TVkCommandBuffer;srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstBuffer:TVkBuffer;regionCount:TVkUInt32;const pRegions:PVkBufferImageCopy); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdUpdateBuffer=procedure(commandBuffer:TVkCommandBuffer;dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;dataSize:TVkDeviceSize;const pData:PVkVoid); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdFillBuffer=procedure(commandBuffer:TVkCommandBuffer;dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;size:TVkDeviceSize;data:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdClearColorImage=procedure(commandBuffer:TVkCommandBuffer;image:TVkImage;imageLayout:TVkImageLayout;const pColor:PVkClearColorValue;rangeCount:TVkUInt32;const pRanges:PVkImageSubresourceRange); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdClearDepthStencilImage=procedure(commandBuffer:TVkCommandBuffer;image:TVkImage;imageLayout:TVkImageLayout;const pDepthStencil:PVkClearDepthStencilValue;rangeCount:TVkUInt32;const pRanges:PVkImageSubresourceRange); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdClearAttachments=procedure(commandBuffer:TVkCommandBuffer;attachmentCount:TVkUInt32;const pAttachments:PVkClearAttachment;rectCount:TVkUInt32;const pRects:PVkClearRect); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdResolveImage=procedure(commandBuffer:TVkCommandBuffer;srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageResolve); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetEvent=procedure(commandBuffer:TVkCommandBuffer;event:TVkEvent;stageMask:TVkPipelineStageFlags); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdResetEvent=procedure(commandBuffer:TVkCommandBuffer;event:TVkEvent;stageMask:TVkPipelineStageFlags); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdWaitEvents=procedure(commandBuffer:TVkCommandBuffer;eventCount:TVkUInt32;const pEvents:PVkEvent;srcStageMask:TVkPipelineStageFlags;dstStageMask:TVkPipelineStageFlags;memoryBarrierCount:TVkUInt32;const pMemoryBarriers:PVkMemoryBarrier;bufferMemoryBarrierCount:TVkUInt32;const pBufferMemoryBarriers:PVkBufferMemoryBarrier;imageMemoryBarrierCount:TVkUInt32;const pImageMemoryBarriers:PVkImageMemoryBarrier); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdPipelineBarrier=procedure(commandBuffer:TVkCommandBuffer;srcStageMask:TVkPipelineStageFlags;dstStageMask:TVkPipelineStageFlags;dependencyFlags:TVkDependencyFlags;memoryBarrierCount:TVkUInt32;const pMemoryBarriers:PVkMemoryBarrier;bufferMemoryBarrierCount:TVkUInt32;const pBufferMemoryBarriers:PVkBufferMemoryBarrier;imageMemoryBarrierCount:TVkUInt32;const pImageMemoryBarriers:PVkImageMemoryBarrier); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdBeginQuery=procedure(commandBuffer:TVkCommandBuffer;queryPool:TVkQueryPool;query:TVkUInt32;flags:TVkQueryControlFlags); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdEndQuery=procedure(commandBuffer:TVkCommandBuffer;queryPool:TVkQueryPool;query:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdResetQueryPool=procedure(commandBuffer:TVkCommandBuffer;queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdWriteTimestamp=procedure(commandBuffer:TVkCommandBuffer;pipelineStage:TVkPipelineStageFlagBits;queryPool:TVkQueryPool;query:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdCopyQueryPoolResults=procedure(commandBuffer:TVkCommandBuffer;queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32;dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;stride:TVkDeviceSize;flags:TVkQueryResultFlags); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdPushConstants=procedure(commandBuffer:TVkCommandBuffer;layout:TVkPipelineLayout;stageFlags:TVkShaderStageFlags;offset:TVkUInt32;size:TVkUInt32;const pValues:PVkVoid); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdBeginRenderPass=procedure(commandBuffer:TVkCommandBuffer;const pRenderPassBegin:PVkRenderPassBeginInfo;contents:TVkSubpassContents); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdNextSubpass=procedure(commandBuffer:TVkCommandBuffer;contents:TVkSubpassContents); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdEndRenderPass=procedure(commandBuffer:TVkCommandBuffer); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdExecuteCommands=procedure(commandBuffer:TVkCommandBuffer;commandBufferCount:TVkUInt32;const pCommandBuffers:PVkCommandBuffer); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$ifdef Android}
     TvkCreateAndroidSurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkAndroidSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
     TvkGetPhysicalDeviceDisplayPropertiesKHR=function(physicalDevice:TVkPhysicalDevice;pPropertyCount:PVkUInt32;pProperties:PVkDisplayPropertiesKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceDisplayPlanePropertiesKHR=function(physicalDevice:TVkPhysicalDevice;pPropertyCount:PVkUInt32;pProperties:PVkDisplayPlanePropertiesKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetDisplayPlaneSupportedDisplaysKHR=function(physicalDevice:TVkPhysicalDevice;planeIndex:TVkUInt32;pDisplayCount:PVkUInt32;pDisplays:PVkDisplayKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetDisplayModePropertiesKHR=function(physicalDevice:TVkPhysicalDevice;display:TVkDisplayKHR;pPropertyCount:PVkUInt32;pProperties:PVkDisplayModePropertiesKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateDisplayModeKHR=function(physicalDevice:TVkPhysicalDevice;display:TVkDisplayKHR;const pCreateInfo:PVkDisplayModeCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pMode:PVkDisplayModeKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetDisplayPlaneCapabilitiesKHR=function(physicalDevice:TVkPhysicalDevice;mode:TVkDisplayModeKHR;planeIndex:TVkUInt32;pCapabilities:PVkDisplayPlaneCapabilitiesKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateDisplayPlaneSurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkDisplaySurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateSharedSwapchainsKHR=function(device:TVkDevice;swapchainCount:TVkUInt32;const pCreateInfos:PVkSwapchainCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSwapchains:PVkSwapchainKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$ifdef Mir}
     TvkCreateMirSurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkMirSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
{$ifdef Mir}
     TvkGetPhysicalDeviceMirPresentationSupportKHR=function(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;connection:PVkMirConnection):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
     TvkDestroySurfaceKHR=procedure(instance:TVkInstance;surface:TVkSurfaceKHR;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceSurfaceSupportKHR=function(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;surface:TVkSurfaceKHR;pSupported:PVkBool32):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceSurfaceCapabilitiesKHR=function(physicalDevice:TVkPhysicalDevice;surface:TVkSurfaceKHR;pSurfaceCapabilities:PVkSurfaceCapabilitiesKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceSurfaceFormatsKHR=function(physicalDevice:TVkPhysicalDevice;surface:TVkSurfaceKHR;pSurfaceFormatCount:PVkUInt32;pSurfaceFormats:PVkSurfaceFormatKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceSurfacePresentModesKHR=function(physicalDevice:TVkPhysicalDevice;surface:TVkSurfaceKHR;pPresentModeCount:PVkUInt32;pPresentModes:PVkPresentModeKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateSwapchainKHR=function(device:TVkDevice;const pCreateInfo:PVkSwapchainCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSwapchain:PVkSwapchainKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroySwapchainKHR=procedure(device:TVkDevice;swapchain:TVkSwapchainKHR;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetSwapchainImagesKHR=function(device:TVkDevice;swapchain:TVkSwapchainKHR;pSwapchainImageCount:PVkUInt32;pSwapchainImages:PVkImage):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkAcquireNextImageKHR=function(device:TVkDevice;swapchain:TVkSwapchainKHR;timeout:TVkUInt64;semaphore:TVkSemaphore;fence:TVkFence;pImageIndex:PVkUInt32):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkQueuePresentKHR=function(queue:TVkQueue;const pPresentInfo:PVkPresentInfoKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateViSurfaceNN=function(instance:TVkInstance;const pCreateInfo:PVkViSurfaceCreateInfoNN;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$ifdef Wayland}
     TvkCreateWaylandSurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkWaylandSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
{$ifdef Wayland}
     TvkGetPhysicalDeviceWaylandPresentationSupportKHR=function(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;display:PVkWaylandDisplay):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
{$ifdef Windows}
     TvkCreateWin32SurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkWin32SurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
{$ifdef Windows}
     TvkGetPhysicalDeviceWin32PresentationSupportKHR=function(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
{$ifdef XLIB}
     TvkCreateXlibSurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkXlibSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
{$ifdef XLIB}
     TvkGetPhysicalDeviceXlibPresentationSupportKHR=function(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;dpy:PVkXLIBDisplay;visualID:TVkXLIBVisualID):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
{$ifdef XCB}
     TvkCreateXcbSurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkXcbSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
{$ifdef XCB}
     TvkGetPhysicalDeviceXcbPresentationSupportKHR=function(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;connection:PVkXCBConnection;visual_id:TVkXCBVisualID):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
     TvkCreateDebugReportCallbackEXT=function(instance:TVkInstance;const pCreateInfo:PVkDebugReportCallbackCreateInfoEXT;const pAllocator:PVkAllocationCallbacks;pCallback:PVkDebugReportCallbackEXT):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyDebugReportCallbackEXT=procedure(instance:TVkInstance;callback:TVkDebugReportCallbackEXT;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDebugReportMessageEXT=procedure(instance:TVkInstance;flags:TVkDebugReportFlagsEXT;objectType:TVkDebugReportObjectTypeEXT;object_:TVkUInt64;location:TVkSize;messageCode:TVkInt32;const pLayerPrefix:PVkChar;const pMessage:PVkChar); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDebugMarkerSetObjectNameEXT=function(device:TVkDevice;const pNameInfo:PVkDebugMarkerObjectNameInfoEXT):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDebugMarkerSetObjectTagEXT=function(device:TVkDevice;const pTagInfo:PVkDebugMarkerObjectTagInfoEXT):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdDebugMarkerBeginEXT=procedure(commandBuffer:TVkCommandBuffer;const pMarkerInfo:PVkDebugMarkerMarkerInfoEXT); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdDebugMarkerEndEXT=procedure(commandBuffer:TVkCommandBuffer); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdDebugMarkerInsertEXT=procedure(commandBuffer:TVkCommandBuffer;const pMarkerInfo:PVkDebugMarkerMarkerInfoEXT); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceExternalImageFormatPropertiesNV=function(physicalDevice:TVkPhysicalDevice;format:TVkFormat;type_:TVkImageType;tiling:TVkImageTiling;usage:TVkImageUsageFlags;flags:TVkImageCreateFlags;externalHandleType:TVkExternalMemoryHandleTypeFlagsNV;pExternalImageFormatProperties:PVkExternalImageFormatPropertiesNV):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$ifdef Windows}
     TvkGetMemoryWin32HandleNV=function(device:TVkDevice;memory:TVkDeviceMemory;handleType:TVkExternalMemoryHandleTypeFlagsNV;pHandle:PHANDLE):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
     TvkCmdDrawIndirectCountAMD=procedure(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;countBuffer:TVkBuffer;countBufferOffset:TVkDeviceSize;maxDrawCount:TVkUInt32;stride:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdDrawIndexedIndirectCountAMD=procedure(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;countBuffer:TVkBuffer;countBufferOffset:TVkDeviceSize;maxDrawCount:TVkUInt32;stride:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdProcessCommandsNVX=procedure(commandBuffer:TVkCommandBuffer;const pProcessCommandsInfo:PVkCmdProcessCommandsInfoNVX); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdReserveSpaceForCommandsNVX=procedure(commandBuffer:TVkCommandBuffer;const pReserveSpaceInfo:PVkCmdReserveSpaceForCommandsInfoNVX); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateIndirectCommandsLayoutNVX=function(device:TVkDevice;const pCreateInfo:PVkIndirectCommandsLayoutCreateInfoNVX;const pAllocator:PVkAllocationCallbacks;pIndirectCommandsLayout:PVkIndirectCommandsLayoutNVX):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyIndirectCommandsLayoutNVX=procedure(device:TVkDevice;indirectCommandsLayout:TVkIndirectCommandsLayoutNVX;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateObjectTableNVX=function(device:TVkDevice;const pCreateInfo:PVkObjectTableCreateInfoNVX;const pAllocator:PVkAllocationCallbacks;pObjectTable:PVkObjectTableNVX):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyObjectTableNVX=procedure(device:TVkDevice;objectTable:TVkObjectTableNVX;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkRegisterObjectsNVX=function(device:TVkDevice;objectTable:TVkObjectTableNVX;objectCount:TVkUInt32;const ppObjectTableEntries:PPVkObjectTableEntryNVX;const pObjectIndices:PVkUInt32):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkUnregisterObjectsNVX=function(device:TVkDevice;objectTable:TVkObjectTableNVX;objectCount:TVkUInt32;const pObjectEntryTypes:PVkObjectEntryTypeNVX;const pObjectIndices:PVkUInt32):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceGeneratedCommandsPropertiesNVX=procedure(physicalDevice:TVkPhysicalDevice;pFeatures:PVkDeviceGeneratedCommandsFeaturesNVX;pLimits:PVkDeviceGeneratedCommandsLimitsNVX); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceFeatures2KHR=procedure(physicalDevice:TVkPhysicalDevice;pFeatures:PVkPhysicalDeviceFeatures2KHR); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceProperties2KHR=procedure(physicalDevice:TVkPhysicalDevice;pProperties:PVkPhysicalDeviceProperties2KHR); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceFormatProperties2KHR=procedure(physicalDevice:TVkPhysicalDevice;format:TVkFormat;pFormatProperties:PVkFormatProperties2KHR); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceImageFormatProperties2KHR=function(physicalDevice:TVkPhysicalDevice;const pImageFormatInfo:PVkPhysicalDeviceImageFormatInfo2KHR;pImageFormatProperties:PVkImageFormatProperties2KHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceQueueFamilyProperties2KHR=procedure(physicalDevice:TVkPhysicalDevice;pQueueFamilyPropertyCount:PVkUInt32;pQueueFamilyProperties:PVkQueueFamilyProperties2KHR); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceMemoryProperties2KHR=procedure(physicalDevice:TVkPhysicalDevice;pMemoryProperties:PVkPhysicalDeviceMemoryProperties2KHR); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceSparseImageFormatProperties2KHR=procedure(physicalDevice:TVkPhysicalDevice;const pFormatInfo:PVkPhysicalDeviceSparseImageFormatInfo2KHR;pPropertyCount:PVkUInt32;pProperties:PVkSparseImageFormatProperties2KHR); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdPushDescriptorSetKHR=procedure(commandBuffer:TVkCommandBuffer;pipelineBindPoint:TVkPipelineBindPoint;layout:TVkPipelineLayout;set_:TVkUInt32;descriptorWriteCount:TVkUInt32;const pDescriptorWrites:PVkWriteDescriptorSet); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkTrimCommandPoolKHR=procedure(device:TVkDevice;commandPool:TVkCommandPool;flags:TVkCommandPoolTrimFlagsKHR); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceExternalBufferPropertiesKHR=procedure(physicalDevice:TVkPhysicalDevice;const pExternalBufferInfo:PVkPhysicalDeviceExternalBufferInfoKHR;pExternalBufferProperties:PVkExternalBufferPropertiesKHR); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$ifdef Windows}
     TvkGetMemoryWin32HandleKHR=function(device:TVkDevice;const pGetWin32HandleInfo:PVkMemoryGetWin32HandleInfoKHR;pHandle:PHANDLE):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
{$ifdef Windows}
     TvkGetMemoryWin32HandlePropertiesKHR=function(device:TVkDevice;handleType:TVkExternalMemoryHandleTypeFlagBitsKHR;handle:THANDLE;pMemoryWin32HandleProperties:PVkMemoryWin32HandlePropertiesKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
     TvkGetMemoryFdKHR=function(device:TVkDevice;const pGetFdInfo:PVkMemoryGetFdInfoKHR;pFd:PVkInt32):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetMemoryFdPropertiesKHR=function(device:TVkDevice;handleType:TVkExternalMemoryHandleTypeFlagBitsKHR;fd:TVkInt32;pMemoryFdProperties:PVkMemoryFdPropertiesKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceExternalSemaphorePropertiesKHR=procedure(physicalDevice:TVkPhysicalDevice;const pExternalSemaphoreInfo:PVkPhysicalDeviceExternalSemaphoreInfoKHR;pExternalSemaphoreProperties:PVkExternalSemaphorePropertiesKHR); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$ifdef Windows}
     TvkGetSemaphoreWin32HandleKHR=function(device:TVkDevice;const pGetWin32HandleInfo:PVkSemaphoreGetWin32HandleInfoKHR;pHandle:PHANDLE):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
{$ifdef Windows}
     TvkImportSemaphoreWin32HandleKHR=function(device:TVkDevice;const pImportSemaphoreWin32HandleInfo:PVkImportSemaphoreWin32HandleInfoKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
     TvkGetSemaphoreFdKHR=function(device:TVkDevice;const pGetFdInfo:PVkSemaphoreGetFdInfoKHR;pFd:PVkInt32):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkImportSemaphoreFdKHR=function(device:TVkDevice;const pImportSemaphoreFdInfo:PVkImportSemaphoreFdInfoKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceExternalFencePropertiesKHR=procedure(physicalDevice:TVkPhysicalDevice;const pExternalFenceInfo:PVkPhysicalDeviceExternalFenceInfoKHR;pExternalFenceProperties:PVkExternalFencePropertiesKHR); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$ifdef Windows}
     TvkGetFenceWin32HandleKHR=function(device:TVkDevice;const pGetWin32HandleInfo:PVkFenceGetWin32HandleInfoKHR;pHandle:PHANDLE):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
{$ifdef Windows}
     TvkImportFenceWin32HandleKHR=function(device:TVkDevice;const pImportFenceWin32HandleInfo:PVkImportFenceWin32HandleInfoKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
     TvkGetFenceFdKHR=function(device:TVkDevice;const pGetFdInfo:PVkFenceGetFdInfoKHR;pFd:PVkInt32):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkImportFenceFdKHR=function(device:TVkDevice;const pImportFenceFdInfo:PVkImportFenceFdInfoKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkReleaseDisplayEXT=function(physicalDevice:TVkPhysicalDevice;display:TVkDisplayKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$ifdef XLIB}
     TvkAcquireXlibDisplayEXT=function(physicalDevice:TVkPhysicalDevice;dpy:PVkXLIBDisplay;display:TVkDisplayKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
{$ifdef RandR}
     TvkGetRandROutputDisplayEXT=function(physicalDevice:TVkPhysicalDevice;dpy:PVkXLIBDisplay;rrOutput:TRROutput;pDisplay:PVkDisplayKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
     TvkDisplayPowerControlEXT=function(device:TVkDevice;display:TVkDisplayKHR;const pDisplayPowerInfo:PVkDisplayPowerInfoEXT):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkRegisterDeviceEventEXT=function(device:TVkDevice;const pDeviceEventInfo:PVkDeviceEventInfoEXT;const pAllocator:PVkAllocationCallbacks;pFence:PVkFence):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkRegisterDisplayEventEXT=function(device:TVkDevice;display:TVkDisplayKHR;const pDisplayEventInfo:PVkDisplayEventInfoEXT;const pAllocator:PVkAllocationCallbacks;pFence:PVkFence):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetSwapchainCounterEXT=function(device:TVkDevice;swapchain:TVkSwapchainKHR;counter:TVkSurfaceCounterFlagBitsEXT;pCounterValue:PVkUInt64):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceSurfaceCapabilities2EXT=function(physicalDevice:TVkPhysicalDevice;surface:TVkSurfaceKHR;pSurfaceCapabilities:PVkSurfaceCapabilities2EXT):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkEnumeratePhysicalDeviceGroupsKHX=function(instance:TVkInstance;pPhysicalDeviceGroupCount:PVkUInt32;pPhysicalDeviceGroupProperties:PVkPhysicalDeviceGroupPropertiesKHX):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetDeviceGroupPeerMemoryFeaturesKHX=procedure(device:TVkDevice;heapIndex:TVkUInt32;localDeviceIndex:TVkUInt32;remoteDeviceIndex:TVkUInt32;pPeerMemoryFeatures:PVkPeerMemoryFeatureFlagsKHX); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkBindBufferMemory2KHX=function(device:TVkDevice;bindInfoCount:TVkUInt32;const pBindInfos:PVkBindBufferMemoryInfoKHX):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkBindImageMemory2KHX=function(device:TVkDevice;bindInfoCount:TVkUInt32;const pBindInfos:PVkBindImageMemoryInfoKHX):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetDeviceMaskKHX=procedure(commandBuffer:TVkCommandBuffer;deviceMask:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetDeviceGroupPresentCapabilitiesKHX=function(device:TVkDevice;pDeviceGroupPresentCapabilities:PVkDeviceGroupPresentCapabilitiesKHX):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetDeviceGroupSurfacePresentModesKHX=function(device:TVkDevice;surface:TVkSurfaceKHR;pModes:PVkDeviceGroupPresentModeFlagsKHX):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkAcquireNextImage2KHX=function(device:TVkDevice;const pAcquireInfo:PVkAcquireNextImageInfoKHX;pImageIndex:PVkUInt32):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdDispatchBaseKHX=procedure(commandBuffer:TVkCommandBuffer;baseGroupX:TVkUInt32;baseGroupY:TVkUInt32;baseGroupZ:TVkUInt32;groupCountX:TVkUInt32;groupCountY:TVkUInt32;groupCountZ:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDevicePresentRectanglesKHX=function(physicalDevice:TVkPhysicalDevice;surface:TVkSurfaceKHR;pRectCount:PVkUInt32;pRects:PVkRect2D):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCreateDescriptorUpdateTemplateKHR=function(device:TVkDevice;const pCreateInfo:PVkDescriptorUpdateTemplateCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pDescriptorUpdateTemplate:PVkDescriptorUpdateTemplateKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkDestroyDescriptorUpdateTemplateKHR=procedure(device:TVkDevice;descriptorUpdateTemplate:TVkDescriptorUpdateTemplateKHR;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkUpdateDescriptorSetWithTemplateKHR=procedure(device:TVkDevice;descriptorSet:TVkDescriptorSet;descriptorUpdateTemplate:TVkDescriptorUpdateTemplateKHR;const pData:PVkVoid); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdPushDescriptorSetWithTemplateKHR=procedure(commandBuffer:TVkCommandBuffer;descriptorUpdateTemplate:TVkDescriptorUpdateTemplateKHR;layout:TVkPipelineLayout;set_:TVkUInt32;const pData:PVkVoid); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkSetHdrMetadataEXT=procedure(device:TVkDevice;swapchainCount:TVkUInt32;const pSwapchains:PVkSwapchainKHR;const pMetadata:PVkHdrMetadataEXT); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetSwapchainStatusKHR=function(device:TVkDevice;swapchain:TVkSwapchainKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetRefreshCycleDurationGOOGLE=function(device:TVkDevice;swapchain:TVkSwapchainKHR;pDisplayTimingProperties:PVkRefreshCycleDurationGOOGLE):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPastPresentationTimingGOOGLE=function(device:TVkDevice;swapchain:TVkSwapchainKHR;pPresentationTimingCount:PVkUInt32;pPresentationTimings:PVkPastPresentationTimingGOOGLE):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$ifdef MoltenVK_IOS}
     TvkCreateIOSSurfaceMVK=function(instance:TVkInstance;const pCreateInfo:PVkIOSSurfaceCreateInfoMVK;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
{$ifdef MoltenVK_MacOS}
     TvkCreateMacOSSurfaceMVK=function(instance:TVkInstance;const pCreateInfo:PVkMacOSSurfaceCreateInfoMVK;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}
     TvkCmdSetViewportWScalingNV=procedure(commandBuffer:TVkCommandBuffer;firstViewport:TVkUInt32;viewportCount:TVkUInt32;const pViewportWScalings:PVkViewportWScalingNV); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetDiscardRectangleEXT=procedure(commandBuffer:TVkCommandBuffer;firstDiscardRectangle:TVkUInt32;discardRectangleCount:TVkUInt32;const pDiscardRectangles:PVkRect2D); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceSurfaceCapabilities2KHR=function(physicalDevice:TVkPhysicalDevice;const pSurfaceInfo:PVkPhysicalDeviceSurfaceInfo2KHR;pSurfaceCapabilities:PVkSurfaceCapabilities2KHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceSurfaceFormats2KHR=function(physicalDevice:TVkPhysicalDevice;const pSurfaceInfo:PVkPhysicalDeviceSurfaceInfo2KHR;pSurfaceFormatCount:PVkUInt32;pSurfaceFormats:PVkSurfaceFormat2KHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetBufferMemoryRequirements2KHR=procedure(device:TVkDevice;const pInfo:PVkBufferMemoryRequirementsInfo2KHR;pMemoryRequirements:PVkMemoryRequirements2KHR); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetImageMemoryRequirements2KHR=procedure(device:TVkDevice;const pInfo:PVkImageMemoryRequirementsInfo2KHR;pMemoryRequirements:PVkMemoryRequirements2KHR); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
     TvkGetImageSparseMemoryRequirements2KHR=procedure(device:TVkDevice;const pInfo:PVkImageSparseMemoryRequirementsInfo2KHR;pSparseMemoryRequirementCount:PVkUInt32;pSparseMemoryRequirements:PVkSparseImageMemoryRequirements2KHR); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}


//========== TVkInstanceFunctions =========================

type
  TVkInstanceFunctions = class
    aaInstance: TVkInstance;
      //............................... Total Instance Functions: 247
      vkDestroyInstance : TvkDestroyInstance;
      vkEnumeratePhysicalDevices : TvkEnumeratePhysicalDevices;
      vkGetDeviceProcAddr : TvkGetDeviceProcAddr;
      vkGetPhysicalDeviceProperties : TvkGetPhysicalDeviceProperties;
      vkGetPhysicalDeviceQueueFamilyProperties : TvkGetPhysicalDeviceQueueFamilyProperties;
      vkGetPhysicalDeviceMemoryProperties : TvkGetPhysicalDeviceMemoryProperties;
      vkGetPhysicalDeviceFeatures : TvkGetPhysicalDeviceFeatures;
      vkGetPhysicalDeviceFormatProperties : TvkGetPhysicalDeviceFormatProperties;
      vkGetPhysicalDeviceImageFormatProperties : TvkGetPhysicalDeviceImageFormatProperties;
      vkCreateDevice : TvkCreateDevice;
      vkDestroyDevice : TvkDestroyDevice;
      vkEnumerateDeviceLayerProperties : TvkEnumerateDeviceLayerProperties;
      vkEnumerateDeviceExtensionProperties : TvkEnumerateDeviceExtensionProperties;
      vkGetDeviceQueue : TvkGetDeviceQueue;
      vkQueueSubmit : TvkQueueSubmit;
      vkQueueWaitIdle : TvkQueueWaitIdle;
      vkDeviceWaitIdle : TvkDeviceWaitIdle;
      vkAllocateMemory : TvkAllocateMemory;
      vkFreeMemory : TvkFreeMemory;
      vkMapMemory : TvkMapMemory;
      vkUnmapMemory : TvkUnmapMemory;
      vkFlushMappedMemoryRanges : TvkFlushMappedMemoryRanges;
      vkInvalidateMappedMemoryRanges : TvkInvalidateMappedMemoryRanges;
      vkGetDeviceMemoryCommitment : TvkGetDeviceMemoryCommitment;
      vkGetBufferMemoryRequirements : TvkGetBufferMemoryRequirements;
      vkBindBufferMemory : TvkBindBufferMemory;
      vkGetImageMemoryRequirements : TvkGetImageMemoryRequirements;
      vkBindImageMemory : TvkBindImageMemory;
      vkGetImageSparseMemoryRequirements : TvkGetImageSparseMemoryRequirements;
      vkGetPhysicalDeviceSparseImageFormatProperties : TvkGetPhysicalDeviceSparseImageFormatProperties;
      vkQueueBindSparse : TvkQueueBindSparse;
      vkCreateFence : TvkCreateFence;
      vkDestroyFence : TvkDestroyFence;
      vkResetFences : TvkResetFences;
      vkGetFenceStatus : TvkGetFenceStatus;
      vkWaitForFences : TvkWaitForFences;
      vkCreateSemaphore : TvkCreateSemaphore;
      vkDestroySemaphore : TvkDestroySemaphore;
      vkCreateEvent : TvkCreateEvent;
      vkDestroyEvent : TvkDestroyEvent;
      vkGetEventStatus : TvkGetEventStatus;
      vkSetEvent : TvkSetEvent;
      vkResetEvent : TvkResetEvent;
      vkCreateQueryPool : TvkCreateQueryPool;
      vkDestroyQueryPool : TvkDestroyQueryPool;
      vkGetQueryPoolResults : TvkGetQueryPoolResults;
      vkCreateBuffer : TvkCreateBuffer;
      vkDestroyBuffer : TvkDestroyBuffer;
      vkCreateBufferView : TvkCreateBufferView;
      vkDestroyBufferView : TvkDestroyBufferView;
      vkCreateImage : TvkCreateImage;
      vkDestroyImage : TvkDestroyImage;
      vkGetImageSubresourceLayout : TvkGetImageSubresourceLayout;
      vkCreateImageView : TvkCreateImageView;
      vkDestroyImageView : TvkDestroyImageView;
      vkCreateShaderModule : TvkCreateShaderModule;
      vkDestroyShaderModule : TvkDestroyShaderModule;
      vkCreatePipelineCache : TvkCreatePipelineCache;
      vkDestroyPipelineCache : TvkDestroyPipelineCache;
      vkGetPipelineCacheData : TvkGetPipelineCacheData;
      vkMergePipelineCaches : TvkMergePipelineCaches;
      vkCreateGraphicsPipelines : TvkCreateGraphicsPipelines;
      vkCreateComputePipelines : TvkCreateComputePipelines;
      vkDestroyPipeline : TvkDestroyPipeline;
      vkCreatePipelineLayout : TvkCreatePipelineLayout;
      vkDestroyPipelineLayout : TvkDestroyPipelineLayout;
      vkCreateSampler : TvkCreateSampler;
      vkDestroySampler : TvkDestroySampler;
      vkCreateDescriptorSetLayout : TvkCreateDescriptorSetLayout;
      vkDestroyDescriptorSetLayout : TvkDestroyDescriptorSetLayout;
      vkCreateDescriptorPool : TvkCreateDescriptorPool;
      vkDestroyDescriptorPool : TvkDestroyDescriptorPool;
      vkResetDescriptorPool : TvkResetDescriptorPool;
      vkAllocateDescriptorSets : TvkAllocateDescriptorSets;
      vkFreeDescriptorSets : TvkFreeDescriptorSets;
      vkUpdateDescriptorSets : TvkUpdateDescriptorSets;
      vkCreateFramebuffer : TvkCreateFramebuffer;
      vkDestroyFramebuffer : TvkDestroyFramebuffer;
      vkCreateRenderPass : TvkCreateRenderPass;
      vkDestroyRenderPass : TvkDestroyRenderPass;
      vkGetRenderAreaGranularity : TvkGetRenderAreaGranularity;
      vkCreateCommandPool : TvkCreateCommandPool;
      vkDestroyCommandPool : TvkDestroyCommandPool;
      vkResetCommandPool : TvkResetCommandPool;
      vkAllocateCommandBuffers : TvkAllocateCommandBuffers;
      vkFreeCommandBuffers : TvkFreeCommandBuffers;
      vkBeginCommandBuffer : TvkBeginCommandBuffer;
      vkEndCommandBuffer : TvkEndCommandBuffer;
      vkResetCommandBuffer : TvkResetCommandBuffer;
      vkCmdBindPipeline : TvkCmdBindPipeline;
      vkCmdSetViewport : TvkCmdSetViewport;
      vkCmdSetScissor : TvkCmdSetScissor;
      vkCmdSetLineWidth : TvkCmdSetLineWidth;
      vkCmdSetDepthBias : TvkCmdSetDepthBias;
      vkCmdSetBlendConstants : TvkCmdSetBlendConstants;
      vkCmdSetDepthBounds : TvkCmdSetDepthBounds;
      vkCmdSetStencilCompareMask : TvkCmdSetStencilCompareMask;
      vkCmdSetStencilWriteMask : TvkCmdSetStencilWriteMask;
      vkCmdSetStencilReference : TvkCmdSetStencilReference;
      vkCmdBindDescriptorSets : TvkCmdBindDescriptorSets;
      vkCmdBindIndexBuffer : TvkCmdBindIndexBuffer;
      vkCmdBindVertexBuffers : TvkCmdBindVertexBuffers;
      vkCmdDraw : TvkCmdDraw;
      vkCmdDrawIndexed : TvkCmdDrawIndexed;
      vkCmdDrawIndirect : TvkCmdDrawIndirect;
      vkCmdDrawIndexedIndirect : TvkCmdDrawIndexedIndirect;
      vkCmdDispatch : TvkCmdDispatch;
      vkCmdDispatchIndirect : TvkCmdDispatchIndirect;
      vkCmdCopyBuffer : TvkCmdCopyBuffer;
      vkCmdCopyImage : TvkCmdCopyImage;
      vkCmdBlitImage : TvkCmdBlitImage;
      vkCmdCopyBufferToImage : TvkCmdCopyBufferToImage;
      vkCmdCopyImageToBuffer : TvkCmdCopyImageToBuffer;
      vkCmdUpdateBuffer : TvkCmdUpdateBuffer;
      vkCmdFillBuffer : TvkCmdFillBuffer;
      vkCmdClearColorImage : TvkCmdClearColorImage;
      vkCmdClearDepthStencilImage : TvkCmdClearDepthStencilImage;
      vkCmdClearAttachments : TvkCmdClearAttachments;
      vkCmdResolveImage : TvkCmdResolveImage;
      vkCmdSetEvent : TvkCmdSetEvent;
      vkCmdResetEvent : TvkCmdResetEvent;
      vkCmdWaitEvents : TvkCmdWaitEvents;
      vkCmdPipelineBarrier : TvkCmdPipelineBarrier;
      vkCmdBeginQuery : TvkCmdBeginQuery;
      vkCmdEndQuery : TvkCmdEndQuery;
      vkCmdResetQueryPool : TvkCmdResetQueryPool;
      vkCmdWriteTimestamp : TvkCmdWriteTimestamp;
      vkCmdCopyQueryPoolResults : TvkCmdCopyQueryPoolResults;
      vkCmdPushConstants : TvkCmdPushConstants;
      vkCmdBeginRenderPass : TvkCmdBeginRenderPass;
      vkCmdNextSubpass : TvkCmdNextSubpass;
      vkCmdEndRenderPass : TvkCmdEndRenderPass;
      vkCmdExecuteCommands : TvkCmdExecuteCommands;
     {$ifdef Android}
      vkCreateAndroidSurfaceKHR : TvkCreateAndroidSurfaceKHR;
     {$endif}
      vkGetPhysicalDeviceDisplayPropertiesKHR : TvkGetPhysicalDeviceDisplayPropertiesKHR;
      vkGetPhysicalDeviceDisplayPlanePropertiesKHR : TvkGetPhysicalDeviceDisplayPlanePropertiesKHR;
      vkGetDisplayPlaneSupportedDisplaysKHR : TvkGetDisplayPlaneSupportedDisplaysKHR;
      vkGetDisplayModePropertiesKHR : TvkGetDisplayModePropertiesKHR;
      vkCreateDisplayModeKHR : TvkCreateDisplayModeKHR;
      vkGetDisplayPlaneCapabilitiesKHR : TvkGetDisplayPlaneCapabilitiesKHR;
      vkCreateDisplayPlaneSurfaceKHR : TvkCreateDisplayPlaneSurfaceKHR;
      vkCreateSharedSwapchainsKHR : TvkCreateSharedSwapchainsKHR;
     {$ifdef Mir}
      vkCreateMirSurfaceKHR : TvkCreateMirSurfaceKHR;
     {$endif}
     {$ifdef Mir}
      vkGetPhysicalDeviceMirPresentationSupportKHR : TvkGetPhysicalDeviceMirPresentationSupportKHR;
     {$endif}
      vkDestroySurfaceKHR : TvkDestroySurfaceKHR;
      vkGetPhysicalDeviceSurfaceSupportKHR : TvkGetPhysicalDeviceSurfaceSupportKHR;
      vkGetPhysicalDeviceSurfaceCapabilitiesKHR : TvkGetPhysicalDeviceSurfaceCapabilitiesKHR;
      vkGetPhysicalDeviceSurfaceFormatsKHR : TvkGetPhysicalDeviceSurfaceFormatsKHR;
      vkGetPhysicalDeviceSurfacePresentModesKHR : TvkGetPhysicalDeviceSurfacePresentModesKHR;
      vkCreateSwapchainKHR : TvkCreateSwapchainKHR;
      vkDestroySwapchainKHR : TvkDestroySwapchainKHR;
      vkGetSwapchainImagesKHR : TvkGetSwapchainImagesKHR;
      vkAcquireNextImageKHR : TvkAcquireNextImageKHR;
      vkQueuePresentKHR : TvkQueuePresentKHR;
      vkCreateViSurfaceNN : TvkCreateViSurfaceNN;
     {$ifdef Wayland}
      vkCreateWaylandSurfaceKHR : TvkCreateWaylandSurfaceKHR;
     {$endif}
     {$ifdef Wayland}
      vkGetPhysicalDeviceWaylandPresentationSupportKHR : TvkGetPhysicalDeviceWaylandPresentationSupportKHR;
     {$endif}
     {$ifdef Windows}
      vkCreateWin32SurfaceKHR : TvkCreateWin32SurfaceKHR;
     {$endif}
     {$ifdef Windows}
      vkGetPhysicalDeviceWin32PresentationSupportKHR : TvkGetPhysicalDeviceWin32PresentationSupportKHR;
     {$endif}
     {$ifdef XLIB}
      vkCreateXlibSurfaceKHR : TvkCreateXlibSurfaceKHR;
     {$endif}
     {$ifdef XLIB}
      vkGetPhysicalDeviceXlibPresentationSupportKHR : TvkGetPhysicalDeviceXlibPresentationSupportKHR;
     {$endif}
     {$ifdef XCB}
      vkCreateXcbSurfaceKHR : TvkCreateXcbSurfaceKHR;
     {$endif}
     {$ifdef XCB}
      vkGetPhysicalDeviceXcbPresentationSupportKHR : TvkGetPhysicalDeviceXcbPresentationSupportKHR;
     {$endif}
      vkCreateDebugReportCallbackEXT : TvkCreateDebugReportCallbackEXT;
      vkDestroyDebugReportCallbackEXT : TvkDestroyDebugReportCallbackEXT;
      vkDebugReportMessageEXT : TvkDebugReportMessageEXT;
      vkDebugMarkerSetObjectNameEXT : TvkDebugMarkerSetObjectNameEXT;
      vkDebugMarkerSetObjectTagEXT : TvkDebugMarkerSetObjectTagEXT;
      vkCmdDebugMarkerBeginEXT : TvkCmdDebugMarkerBeginEXT;
      vkCmdDebugMarkerEndEXT : TvkCmdDebugMarkerEndEXT;
      vkCmdDebugMarkerInsertEXT : TvkCmdDebugMarkerInsertEXT;
      vkGetPhysicalDeviceExternalImageFormatPropertiesNV : TvkGetPhysicalDeviceExternalImageFormatPropertiesNV;
     {$ifdef Windows}
      vkGetMemoryWin32HandleNV : TvkGetMemoryWin32HandleNV;
     {$endif}
      vkCmdDrawIndirectCountAMD : TvkCmdDrawIndirectCountAMD;
      vkCmdDrawIndexedIndirectCountAMD : TvkCmdDrawIndexedIndirectCountAMD;
      vkCmdProcessCommandsNVX : TvkCmdProcessCommandsNVX;
      vkCmdReserveSpaceForCommandsNVX : TvkCmdReserveSpaceForCommandsNVX;
      vkCreateIndirectCommandsLayoutNVX : TvkCreateIndirectCommandsLayoutNVX;
      vkDestroyIndirectCommandsLayoutNVX : TvkDestroyIndirectCommandsLayoutNVX;
      vkCreateObjectTableNVX : TvkCreateObjectTableNVX;
      vkDestroyObjectTableNVX : TvkDestroyObjectTableNVX;
      vkRegisterObjectsNVX : TvkRegisterObjectsNVX;
      vkUnregisterObjectsNVX : TvkUnregisterObjectsNVX;
      vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX : TvkGetPhysicalDeviceGeneratedCommandsPropertiesNVX;
      vkGetPhysicalDeviceFeatures2KHR : TvkGetPhysicalDeviceFeatures2KHR;
      vkGetPhysicalDeviceProperties2KHR : TvkGetPhysicalDeviceProperties2KHR;
      vkGetPhysicalDeviceFormatProperties2KHR : TvkGetPhysicalDeviceFormatProperties2KHR;
      vkGetPhysicalDeviceImageFormatProperties2KHR : TvkGetPhysicalDeviceImageFormatProperties2KHR;
      vkGetPhysicalDeviceQueueFamilyProperties2KHR : TvkGetPhysicalDeviceQueueFamilyProperties2KHR;
      vkGetPhysicalDeviceMemoryProperties2KHR : TvkGetPhysicalDeviceMemoryProperties2KHR;
      vkGetPhysicalDeviceSparseImageFormatProperties2KHR : TvkGetPhysicalDeviceSparseImageFormatProperties2KHR;
      vkCmdPushDescriptorSetKHR : TvkCmdPushDescriptorSetKHR;
      vkTrimCommandPoolKHR : TvkTrimCommandPoolKHR;
      vkGetPhysicalDeviceExternalBufferPropertiesKHR : TvkGetPhysicalDeviceExternalBufferPropertiesKHR;
     {$ifdef Windows}
      vkGetMemoryWin32HandleKHR : TvkGetMemoryWin32HandleKHR;
     {$endif}
     {$ifdef Windows}
      vkGetMemoryWin32HandlePropertiesKHR : TvkGetMemoryWin32HandlePropertiesKHR;
     {$endif}
      vkGetMemoryFdKHR : TvkGetMemoryFdKHR;
      vkGetMemoryFdPropertiesKHR : TvkGetMemoryFdPropertiesKHR;
      vkGetPhysicalDeviceExternalSemaphorePropertiesKHR : TvkGetPhysicalDeviceExternalSemaphorePropertiesKHR;
     {$ifdef Windows}
      vkGetSemaphoreWin32HandleKHR : TvkGetSemaphoreWin32HandleKHR;
     {$endif}
     {$ifdef Windows}
      vkImportSemaphoreWin32HandleKHR : TvkImportSemaphoreWin32HandleKHR;
     {$endif}
      vkGetSemaphoreFdKHR : TvkGetSemaphoreFdKHR;
      vkImportSemaphoreFdKHR : TvkImportSemaphoreFdKHR;
      vkGetPhysicalDeviceExternalFencePropertiesKHR : TvkGetPhysicalDeviceExternalFencePropertiesKHR;
     {$ifdef Windows}
      vkGetFenceWin32HandleKHR : TvkGetFenceWin32HandleKHR;
     {$endif}
     {$ifdef Windows}
      vkImportFenceWin32HandleKHR : TvkImportFenceWin32HandleKHR;
     {$endif}
      vkGetFenceFdKHR : TvkGetFenceFdKHR;
      vkImportFenceFdKHR : TvkImportFenceFdKHR;
      vkReleaseDisplayEXT : TvkReleaseDisplayEXT;
     {$ifdef XLIB}
      vkAcquireXlibDisplayEXT : TvkAcquireXlibDisplayEXT;
     {$endif}
     {$ifdef RandR}
      vkGetRandROutputDisplayEXT : TvkGetRandROutputDisplayEXT;
     {$endif}
      vkDisplayPowerControlEXT : TvkDisplayPowerControlEXT;
      vkRegisterDeviceEventEXT : TvkRegisterDeviceEventEXT;
      vkRegisterDisplayEventEXT : TvkRegisterDisplayEventEXT;
      vkGetSwapchainCounterEXT : TvkGetSwapchainCounterEXT;
      vkGetPhysicalDeviceSurfaceCapabilities2EXT : TvkGetPhysicalDeviceSurfaceCapabilities2EXT;
      vkEnumeratePhysicalDeviceGroupsKHX : TvkEnumeratePhysicalDeviceGroupsKHX;
      vkGetDeviceGroupPeerMemoryFeaturesKHX : TvkGetDeviceGroupPeerMemoryFeaturesKHX;
      vkBindBufferMemory2KHX : TvkBindBufferMemory2KHX;
      vkBindImageMemory2KHX : TvkBindImageMemory2KHX;
      vkCmdSetDeviceMaskKHX : TvkCmdSetDeviceMaskKHX;
      vkGetDeviceGroupPresentCapabilitiesKHX : TvkGetDeviceGroupPresentCapabilitiesKHX;
      vkGetDeviceGroupSurfacePresentModesKHX : TvkGetDeviceGroupSurfacePresentModesKHX;
      vkAcquireNextImage2KHX : TvkAcquireNextImage2KHX;
      vkCmdDispatchBaseKHX : TvkCmdDispatchBaseKHX;
      vkGetPhysicalDevicePresentRectanglesKHX : TvkGetPhysicalDevicePresentRectanglesKHX;
      vkCreateDescriptorUpdateTemplateKHR : TvkCreateDescriptorUpdateTemplateKHR;
      vkDestroyDescriptorUpdateTemplateKHR : TvkDestroyDescriptorUpdateTemplateKHR;
      vkUpdateDescriptorSetWithTemplateKHR : TvkUpdateDescriptorSetWithTemplateKHR;
      vkCmdPushDescriptorSetWithTemplateKHR : TvkCmdPushDescriptorSetWithTemplateKHR;
      vkSetHdrMetadataEXT : TvkSetHdrMetadataEXT;
      vkGetSwapchainStatusKHR : TvkGetSwapchainStatusKHR;
      vkGetRefreshCycleDurationGOOGLE : TvkGetRefreshCycleDurationGOOGLE;
      vkGetPastPresentationTimingGOOGLE : TvkGetPastPresentationTimingGOOGLE;
     {$ifdef MoltenVK_IOS}
      vkCreateIOSSurfaceMVK : TvkCreateIOSSurfaceMVK;
     {$endif}
     {$ifdef MoltenVK_MacOS}
      vkCreateMacOSSurfaceMVK : TvkCreateMacOSSurfaceMVK;
     {$endif}
      vkCmdSetViewportWScalingNV : TvkCmdSetViewportWScalingNV;
      vkCmdSetDiscardRectangleEXT : TvkCmdSetDiscardRectangleEXT;
      vkGetPhysicalDeviceSurfaceCapabilities2KHR : TvkGetPhysicalDeviceSurfaceCapabilities2KHR;
      vkGetPhysicalDeviceSurfaceFormats2KHR : TvkGetPhysicalDeviceSurfaceFormats2KHR;
      vkGetBufferMemoryRequirements2KHR : TvkGetBufferMemoryRequirements2KHR;
      vkGetImageMemoryRequirements2KHR : TvkGetImageMemoryRequirements2KHR;
      vkGetImageSparseMemoryRequirements2KHR : TvkGetImageSparseMemoryRequirements2KHR;
  end;

//========== TVkDeviceFunctions =======================

type
  TVkDeviceFunctions = class
    aaDevice: TVkDevice;
      //............................... Total Device Functions: 182
      vkCreateInstance : TvkCreateInstance;
      vkDestroyInstance : TvkDestroyInstance;
      vkEnumeratePhysicalDevices : TvkEnumeratePhysicalDevices;
      vkGetDeviceProcAddr : TvkGetDeviceProcAddr;
      vkGetInstanceProcAddr : TvkGetInstanceProcAddr;
      vkGetPhysicalDeviceProperties : TvkGetPhysicalDeviceProperties;
      vkGetPhysicalDeviceQueueFamilyProperties : TvkGetPhysicalDeviceQueueFamilyProperties;
      vkGetPhysicalDeviceMemoryProperties : TvkGetPhysicalDeviceMemoryProperties;
      vkGetPhysicalDeviceFeatures : TvkGetPhysicalDeviceFeatures;
      vkGetPhysicalDeviceFormatProperties : TvkGetPhysicalDeviceFormatProperties;
      vkGetPhysicalDeviceImageFormatProperties : TvkGetPhysicalDeviceImageFormatProperties;
      vkCreateDevice : TvkCreateDevice;
      vkDestroyDevice : TvkDestroyDevice;
      vkEnumerateInstanceLayerProperties : TvkEnumerateInstanceLayerProperties;
      vkEnumerateInstanceExtensionProperties : TvkEnumerateInstanceExtensionProperties;
      vkEnumerateDeviceLayerProperties : TvkEnumerateDeviceLayerProperties;
      vkEnumerateDeviceExtensionProperties : TvkEnumerateDeviceExtensionProperties;
      vkGetDeviceQueue : TvkGetDeviceQueue;
      vkQueueSubmit : TvkQueueSubmit;
      vkQueueWaitIdle : TvkQueueWaitIdle;
      vkDeviceWaitIdle : TvkDeviceWaitIdle;
      vkAllocateMemory : TvkAllocateMemory;
      vkFreeMemory : TvkFreeMemory;
      vkMapMemory : TvkMapMemory;
      vkUnmapMemory : TvkUnmapMemory;
      vkFlushMappedMemoryRanges : TvkFlushMappedMemoryRanges;
      vkInvalidateMappedMemoryRanges : TvkInvalidateMappedMemoryRanges;
      vkGetDeviceMemoryCommitment : TvkGetDeviceMemoryCommitment;
      vkGetBufferMemoryRequirements : TvkGetBufferMemoryRequirements;
      vkBindBufferMemory : TvkBindBufferMemory;
      vkGetImageMemoryRequirements : TvkGetImageMemoryRequirements;
      vkBindImageMemory : TvkBindImageMemory;
      vkGetImageSparseMemoryRequirements : TvkGetImageSparseMemoryRequirements;
      vkGetPhysicalDeviceSparseImageFormatProperties : TvkGetPhysicalDeviceSparseImageFormatProperties;
      vkQueueBindSparse : TvkQueueBindSparse;
      vkCreateFence : TvkCreateFence;
      vkDestroyFence : TvkDestroyFence;
      vkResetFences : TvkResetFences;
      vkGetFenceStatus : TvkGetFenceStatus;
      vkWaitForFences : TvkWaitForFences;
      vkCreateSemaphore : TvkCreateSemaphore;
      vkDestroySemaphore : TvkDestroySemaphore;
      vkCreateEvent : TvkCreateEvent;
      vkDestroyEvent : TvkDestroyEvent;
      vkGetEventStatus : TvkGetEventStatus;
      vkSetEvent : TvkSetEvent;
      vkResetEvent : TvkResetEvent;
      vkCreateQueryPool : TvkCreateQueryPool;
      vkDestroyQueryPool : TvkDestroyQueryPool;
      vkGetQueryPoolResults : TvkGetQueryPoolResults;
      vkCreateBuffer : TvkCreateBuffer;
      vkDestroyBuffer : TvkDestroyBuffer;
      vkCreateBufferView : TvkCreateBufferView;
      vkDestroyBufferView : TvkDestroyBufferView;
      vkCreateImage : TvkCreateImage;
      vkDestroyImage : TvkDestroyImage;
      vkGetImageSubresourceLayout : TvkGetImageSubresourceLayout;
      vkCreateImageView : TvkCreateImageView;
      vkDestroyImageView : TvkDestroyImageView;
      vkCreateShaderModule : TvkCreateShaderModule;
      vkDestroyShaderModule : TvkDestroyShaderModule;
      vkCreatePipelineCache : TvkCreatePipelineCache;
      vkDestroyPipelineCache : TvkDestroyPipelineCache;
      vkGetPipelineCacheData : TvkGetPipelineCacheData;
      vkMergePipelineCaches : TvkMergePipelineCaches;
      vkCreateGraphicsPipelines : TvkCreateGraphicsPipelines;
      vkCreateComputePipelines : TvkCreateComputePipelines;
      vkDestroyPipeline : TvkDestroyPipeline;
      vkCreatePipelineLayout : TvkCreatePipelineLayout;
      vkDestroyPipelineLayout : TvkDestroyPipelineLayout;
      vkCreateSampler : TvkCreateSampler;
      vkDestroySampler : TvkDestroySampler;
      vkCreateDescriptorSetLayout : TvkCreateDescriptorSetLayout;
      vkDestroyDescriptorSetLayout : TvkDestroyDescriptorSetLayout;
      vkCreateDescriptorPool : TvkCreateDescriptorPool;
      vkDestroyDescriptorPool : TvkDestroyDescriptorPool;
      vkResetDescriptorPool : TvkResetDescriptorPool;
      vkAllocateDescriptorSets : TvkAllocateDescriptorSets;
      vkFreeDescriptorSets : TvkFreeDescriptorSets;
      vkUpdateDescriptorSets : TvkUpdateDescriptorSets;
      vkCreateFramebuffer : TvkCreateFramebuffer;
      vkDestroyFramebuffer : TvkDestroyFramebuffer;
      vkCreateRenderPass : TvkCreateRenderPass;
      vkDestroyRenderPass : TvkDestroyRenderPass;
      vkGetRenderAreaGranularity : TvkGetRenderAreaGranularity;
      vkCreateCommandPool : TvkCreateCommandPool;
      vkDestroyCommandPool : TvkDestroyCommandPool;
      vkResetCommandPool : TvkResetCommandPool;
      vkAllocateCommandBuffers : TvkAllocateCommandBuffers;
      vkFreeCommandBuffers : TvkFreeCommandBuffers;
      vkBeginCommandBuffer : TvkBeginCommandBuffer;
      vkEndCommandBuffer : TvkEndCommandBuffer;
      vkResetCommandBuffer : TvkResetCommandBuffer;
      vkCmdBindPipeline : TvkCmdBindPipeline;
      vkCmdSetViewport : TvkCmdSetViewport;
      vkCmdSetScissor : TvkCmdSetScissor;
      vkCmdSetLineWidth : TvkCmdSetLineWidth;
      vkCmdSetDepthBias : TvkCmdSetDepthBias;
      vkCmdSetBlendConstants : TvkCmdSetBlendConstants;
      vkCmdSetDepthBounds : TvkCmdSetDepthBounds;
      vkCmdSetStencilCompareMask : TvkCmdSetStencilCompareMask;
      vkCmdSetStencilWriteMask : TvkCmdSetStencilWriteMask;
      vkCmdSetStencilReference : TvkCmdSetStencilReference;
      vkCmdBindDescriptorSets : TvkCmdBindDescriptorSets;
      vkCmdBindIndexBuffer : TvkCmdBindIndexBuffer;
      vkCmdBindVertexBuffers : TvkCmdBindVertexBuffers;
      vkCmdDraw : TvkCmdDraw;
      vkCmdDrawIndexed : TvkCmdDrawIndexed;
      vkCmdDrawIndirect : TvkCmdDrawIndirect;
      vkCmdDrawIndexedIndirect : TvkCmdDrawIndexedIndirect;
      vkCmdDispatch : TvkCmdDispatch;
      vkCmdDispatchIndirect : TvkCmdDispatchIndirect;
      vkCmdCopyBuffer : TvkCmdCopyBuffer;
      vkCmdCopyImage : TvkCmdCopyImage;
      vkCmdBlitImage : TvkCmdBlitImage;
      vkCmdCopyBufferToImage : TvkCmdCopyBufferToImage;
      vkCmdCopyImageToBuffer : TvkCmdCopyImageToBuffer;
      vkCmdUpdateBuffer : TvkCmdUpdateBuffer;
      vkCmdFillBuffer : TvkCmdFillBuffer;
      vkCmdClearColorImage : TvkCmdClearColorImage;
      vkCmdClearDepthStencilImage : TvkCmdClearDepthStencilImage;
      vkCmdClearAttachments : TvkCmdClearAttachments;
      vkCmdResolveImage : TvkCmdResolveImage;
      vkCmdSetEvent : TvkCmdSetEvent;
      vkCmdResetEvent : TvkCmdResetEvent;
      vkCmdWaitEvents : TvkCmdWaitEvents;
      vkCmdPipelineBarrier : TvkCmdPipelineBarrier;
      vkCmdBeginQuery : TvkCmdBeginQuery;
      vkCmdEndQuery : TvkCmdEndQuery;
      vkCmdResetQueryPool : TvkCmdResetQueryPool;
      vkCmdWriteTimestamp : TvkCmdWriteTimestamp;
      vkCmdCopyQueryPoolResults : TvkCmdCopyQueryPoolResults;
      vkCmdPushConstants : TvkCmdPushConstants;
      vkCmdBeginRenderPass : TvkCmdBeginRenderPass;
      vkCmdNextSubpass : TvkCmdNextSubpass;
      vkCmdEndRenderPass : TvkCmdEndRenderPass;
      vkCmdExecuteCommands : TvkCmdExecuteCommands;
     {$ifdef Android}
      vkCreateAndroidSurfaceKHR : TvkCreateAndroidSurfaceKHR;
     {$endif}
      vkGetPhysicalDeviceDisplayPropertiesKHR : TvkGetPhysicalDeviceDisplayPropertiesKHR;
      vkGetPhysicalDeviceDisplayPlanePropertiesKHR : TvkGetPhysicalDeviceDisplayPlanePropertiesKHR;
      vkGetDisplayPlaneSupportedDisplaysKHR : TvkGetDisplayPlaneSupportedDisplaysKHR;
      vkGetDisplayModePropertiesKHR : TvkGetDisplayModePropertiesKHR;
      vkCreateDisplayModeKHR : TvkCreateDisplayModeKHR;
      vkGetDisplayPlaneCapabilitiesKHR : TvkGetDisplayPlaneCapabilitiesKHR;
      vkCreateDisplayPlaneSurfaceKHR : TvkCreateDisplayPlaneSurfaceKHR;
      vkCreateSharedSwapchainsKHR : TvkCreateSharedSwapchainsKHR;
     {$ifdef Mir}
      vkCreateMirSurfaceKHR : TvkCreateMirSurfaceKHR;
     {$endif}
     {$ifdef Mir}
      vkGetPhysicalDeviceMirPresentationSupportKHR : TvkGetPhysicalDeviceMirPresentationSupportKHR;
     {$endif}
      vkDestroySurfaceKHR : TvkDestroySurfaceKHR;
      vkGetPhysicalDeviceSurfaceSupportKHR : TvkGetPhysicalDeviceSurfaceSupportKHR;
      vkGetPhysicalDeviceSurfaceCapabilitiesKHR : TvkGetPhysicalDeviceSurfaceCapabilitiesKHR;
      vkGetPhysicalDeviceSurfaceFormatsKHR : TvkGetPhysicalDeviceSurfaceFormatsKHR;
      vkGetPhysicalDeviceSurfacePresentModesKHR : TvkGetPhysicalDeviceSurfacePresentModesKHR;
      vkCreateSwapchainKHR : TvkCreateSwapchainKHR;
      vkDestroySwapchainKHR : TvkDestroySwapchainKHR;
      vkGetSwapchainImagesKHR : TvkGetSwapchainImagesKHR;
      vkAcquireNextImageKHR : TvkAcquireNextImageKHR;
      vkQueuePresentKHR : TvkQueuePresentKHR;
      vkCreateViSurfaceNN : TvkCreateViSurfaceNN;
     {$ifdef Wayland}
      vkCreateWaylandSurfaceKHR : TvkCreateWaylandSurfaceKHR;
     {$endif}
     {$ifdef Wayland}
      vkGetPhysicalDeviceWaylandPresentationSupportKHR : TvkGetPhysicalDeviceWaylandPresentationSupportKHR;
     {$endif}
     {$ifdef Windows}
      vkCreateWin32SurfaceKHR : TvkCreateWin32SurfaceKHR;
     {$endif}
     {$ifdef Windows}
      vkGetPhysicalDeviceWin32PresentationSupportKHR : TvkGetPhysicalDeviceWin32PresentationSupportKHR;
     {$endif}
     {$ifdef XLIB}
      vkCreateXlibSurfaceKHR : TvkCreateXlibSurfaceKHR;
     {$endif}
     {$ifdef XLIB}
      vkGetPhysicalDeviceXlibPresentationSupportKHR : TvkGetPhysicalDeviceXlibPresentationSupportKHR;
     {$endif}
     {$ifdef XCB}
      vkCreateXcbSurfaceKHR : TvkCreateXcbSurfaceKHR;
     {$endif}
     {$ifdef XCB}
      vkGetPhysicalDeviceXcbPresentationSupportKHR : TvkGetPhysicalDeviceXcbPresentationSupportKHR;
     {$endif}
      vkCreateDebugReportCallbackEXT : TvkCreateDebugReportCallbackEXT;
      vkDestroyDebugReportCallbackEXT : TvkDestroyDebugReportCallbackEXT;
      vkDebugReportMessageEXT : TvkDebugReportMessageEXT;
      vkDebugMarkerSetObjectNameEXT : TvkDebugMarkerSetObjectNameEXT;
      vkDebugMarkerSetObjectTagEXT : TvkDebugMarkerSetObjectTagEXT;
      vkCmdDebugMarkerBeginEXT : TvkCmdDebugMarkerBeginEXT;
      vkCmdDebugMarkerEndEXT : TvkCmdDebugMarkerEndEXT;
      vkCmdDebugMarkerInsertEXT : TvkCmdDebugMarkerInsertEXT;
      vkGetPhysicalDeviceExternalImageFormatPropertiesNV : TvkGetPhysicalDeviceExternalImageFormatPropertiesNV;
     {$ifdef Windows}
      vkGetMemoryWin32HandleNV : TvkGetMemoryWin32HandleNV;
     {$endif}
      vkCmdDrawIndirectCountAMD : TvkCmdDrawIndirectCountAMD;
      vkCmdDrawIndexedIndirectCountAMD : TvkCmdDrawIndexedIndirectCountAMD;
      vkCmdProcessCommandsNVX : TvkCmdProcessCommandsNVX;
      vkCmdReserveSpaceForCommandsNVX : TvkCmdReserveSpaceForCommandsNVX;
      vkCreateIndirectCommandsLayoutNVX : TvkCreateIndirectCommandsLayoutNVX;
  end;


//================ Variables ==============================================
Var

    vkCreateInstance:TvkCreateInstance=nil;
    vkDestroyInstance:TvkDestroyInstance=nil;
    vkEnumeratePhysicalDevices:TvkEnumeratePhysicalDevices=nil;
    vkGetDeviceProcAddr:TvkGetDeviceProcAddr=nil;
    vkGetInstanceProcAddr:TvkGetInstanceProcAddr=nil;
    vkGetPhysicalDeviceProperties:TvkGetPhysicalDeviceProperties=nil;
    vkGetPhysicalDeviceQueueFamilyProperties:TvkGetPhysicalDeviceQueueFamilyProperties=nil;
    vkGetPhysicalDeviceMemoryProperties:TvkGetPhysicalDeviceMemoryProperties=nil;
    vkGetPhysicalDeviceFeatures:TvkGetPhysicalDeviceFeatures=nil;
    vkGetPhysicalDeviceFormatProperties:TvkGetPhysicalDeviceFormatProperties=nil;
    vkGetPhysicalDeviceImageFormatProperties:TvkGetPhysicalDeviceImageFormatProperties=nil;
    vkCreateDevice:TvkCreateDevice=nil;
    vkDestroyDevice:TvkDestroyDevice=nil;
    vkEnumerateInstanceLayerProperties:TvkEnumerateInstanceLayerProperties=nil;
    vkEnumerateInstanceExtensionProperties:TvkEnumerateInstanceExtensionProperties=nil;
    vkEnumerateDeviceLayerProperties:TvkEnumerateDeviceLayerProperties=nil;
    vkEnumerateDeviceExtensionProperties:TvkEnumerateDeviceExtensionProperties=nil;
    vkGetDeviceQueue:TvkGetDeviceQueue=nil;
    vkQueueSubmit:TvkQueueSubmit=nil;
    vkQueueWaitIdle:TvkQueueWaitIdle=nil;
    vkDeviceWaitIdle:TvkDeviceWaitIdle=nil;
    vkAllocateMemory:TvkAllocateMemory=nil;
    vkFreeMemory:TvkFreeMemory=nil;
    vkMapMemory:TvkMapMemory=nil;
    vkUnmapMemory:TvkUnmapMemory=nil;
    vkFlushMappedMemoryRanges:TvkFlushMappedMemoryRanges=nil;
    vkInvalidateMappedMemoryRanges:TvkInvalidateMappedMemoryRanges=nil;
    vkGetDeviceMemoryCommitment:TvkGetDeviceMemoryCommitment=nil;
    vkGetBufferMemoryRequirements:TvkGetBufferMemoryRequirements=nil;
    vkBindBufferMemory:TvkBindBufferMemory=nil;
    vkGetImageMemoryRequirements:TvkGetImageMemoryRequirements=nil;
    vkBindImageMemory:TvkBindImageMemory=nil;
    vkGetImageSparseMemoryRequirements:TvkGetImageSparseMemoryRequirements=nil;
    vkGetPhysicalDeviceSparseImageFormatProperties:TvkGetPhysicalDeviceSparseImageFormatProperties=nil;
    vkQueueBindSparse:TvkQueueBindSparse=nil;
    vkCreateFence:TvkCreateFence=nil;
    vkDestroyFence:TvkDestroyFence=nil;
    vkResetFences:TvkResetFences=nil;
    vkGetFenceStatus:TvkGetFenceStatus=nil;
    vkWaitForFences:TvkWaitForFences=nil;
    vkCreateSemaphore:TvkCreateSemaphore=nil;
    vkDestroySemaphore:TvkDestroySemaphore=nil;
    vkCreateEvent:TvkCreateEvent=nil;
    vkDestroyEvent:TvkDestroyEvent=nil;
    vkGetEventStatus:TvkGetEventStatus=nil;
    vkSetEvent:TvkSetEvent=nil;
    vkResetEvent:TvkResetEvent=nil;
    vkCreateQueryPool:TvkCreateQueryPool=nil;
    vkDestroyQueryPool:TvkDestroyQueryPool=nil;
    vkGetQueryPoolResults:TvkGetQueryPoolResults=nil;
    vkCreateBuffer:TvkCreateBuffer=nil;
    vkDestroyBuffer:TvkDestroyBuffer=nil;
    vkCreateBufferView:TvkCreateBufferView=nil;
    vkDestroyBufferView:TvkDestroyBufferView=nil;
    vkCreateImage:TvkCreateImage=nil;
    vkDestroyImage:TvkDestroyImage=nil;
    vkGetImageSubresourceLayout:TvkGetImageSubresourceLayout=nil;
    vkCreateImageView:TvkCreateImageView=nil;
    vkDestroyImageView:TvkDestroyImageView=nil;
    vkCreateShaderModule:TvkCreateShaderModule=nil;
    vkDestroyShaderModule:TvkDestroyShaderModule=nil;
    vkCreatePipelineCache:TvkCreatePipelineCache=nil;
    vkDestroyPipelineCache:TvkDestroyPipelineCache=nil;
    vkGetPipelineCacheData:TvkGetPipelineCacheData=nil;
    vkMergePipelineCaches:TvkMergePipelineCaches=nil;
    vkCreateGraphicsPipelines:TvkCreateGraphicsPipelines=nil;
    vkCreateComputePipelines:TvkCreateComputePipelines=nil;
    vkDestroyPipeline:TvkDestroyPipeline=nil;
    vkCreatePipelineLayout:TvkCreatePipelineLayout=nil;
    vkDestroyPipelineLayout:TvkDestroyPipelineLayout=nil;
    vkCreateSampler:TvkCreateSampler=nil;
    vkDestroySampler:TvkDestroySampler=nil;
    vkCreateDescriptorSetLayout:TvkCreateDescriptorSetLayout=nil;
    vkDestroyDescriptorSetLayout:TvkDestroyDescriptorSetLayout=nil;
    vkCreateDescriptorPool:TvkCreateDescriptorPool=nil;
    vkDestroyDescriptorPool:TvkDestroyDescriptorPool=nil;
    vkResetDescriptorPool:TvkResetDescriptorPool=nil;
    vkAllocateDescriptorSets:TvkAllocateDescriptorSets=nil;
    vkFreeDescriptorSets:TvkFreeDescriptorSets=nil;
    vkUpdateDescriptorSets:TvkUpdateDescriptorSets=nil;
    vkCreateFramebuffer:TvkCreateFramebuffer=nil;
    vkDestroyFramebuffer:TvkDestroyFramebuffer=nil;
    vkCreateRenderPass:TvkCreateRenderPass=nil;
    vkDestroyRenderPass:TvkDestroyRenderPass=nil;
    vkGetRenderAreaGranularity:TvkGetRenderAreaGranularity=nil;
    vkCreateCommandPool:TvkCreateCommandPool=nil;
    vkDestroyCommandPool:TvkDestroyCommandPool=nil;
    vkResetCommandPool:TvkResetCommandPool=nil;
    vkAllocateCommandBuffers:TvkAllocateCommandBuffers=nil;
    vkFreeCommandBuffers:TvkFreeCommandBuffers=nil;
    vkBeginCommandBuffer:TvkBeginCommandBuffer=nil;
    vkEndCommandBuffer:TvkEndCommandBuffer=nil;
    vkResetCommandBuffer:TvkResetCommandBuffer=nil;
    vkCmdBindPipeline:TvkCmdBindPipeline=nil;
    vkCmdSetViewport:TvkCmdSetViewport=nil;
    vkCmdSetScissor:TvkCmdSetScissor=nil;
    vkCmdSetLineWidth:TvkCmdSetLineWidth=nil;
    vkCmdSetDepthBias:TvkCmdSetDepthBias=nil;
    vkCmdSetBlendConstants:TvkCmdSetBlendConstants=nil;
    vkCmdSetDepthBounds:TvkCmdSetDepthBounds=nil;
    vkCmdSetStencilCompareMask:TvkCmdSetStencilCompareMask=nil;
    vkCmdSetStencilWriteMask:TvkCmdSetStencilWriteMask=nil;
    vkCmdSetStencilReference:TvkCmdSetStencilReference=nil;
    vkCmdBindDescriptorSets:TvkCmdBindDescriptorSets=nil;
    vkCmdBindIndexBuffer:TvkCmdBindIndexBuffer=nil;
    vkCmdBindVertexBuffers:TvkCmdBindVertexBuffers=nil;
    vkCmdDraw:TvkCmdDraw=nil;
    vkCmdDrawIndexed:TvkCmdDrawIndexed=nil;
    vkCmdDrawIndirect:TvkCmdDrawIndirect=nil;
    vkCmdDrawIndexedIndirect:TvkCmdDrawIndexedIndirect=nil;
    vkCmdDispatch:TvkCmdDispatch=nil;
    vkCmdDispatchIndirect:TvkCmdDispatchIndirect=nil;
    vkCmdCopyBuffer:TvkCmdCopyBuffer=nil;
    vkCmdCopyImage:TvkCmdCopyImage=nil;
    vkCmdBlitImage:TvkCmdBlitImage=nil;
    vkCmdCopyBufferToImage:TvkCmdCopyBufferToImage=nil;
    vkCmdCopyImageToBuffer:TvkCmdCopyImageToBuffer=nil;
    vkCmdUpdateBuffer:TvkCmdUpdateBuffer=nil;
    vkCmdFillBuffer:TvkCmdFillBuffer=nil;
    vkCmdClearColorImage:TvkCmdClearColorImage=nil;
    vkCmdClearDepthStencilImage:TvkCmdClearDepthStencilImage=nil;
    vkCmdClearAttachments:TvkCmdClearAttachments=nil;
    vkCmdResolveImage:TvkCmdResolveImage=nil;
    vkCmdSetEvent:TvkCmdSetEvent=nil;
    vkCmdResetEvent:TvkCmdResetEvent=nil;
    vkCmdWaitEvents:TvkCmdWaitEvents=nil;
    vkCmdPipelineBarrier:TvkCmdPipelineBarrier=nil;
    vkCmdBeginQuery:TvkCmdBeginQuery=nil;
    vkCmdEndQuery:TvkCmdEndQuery=nil;
    vkCmdResetQueryPool:TvkCmdResetQueryPool=nil;
    vkCmdWriteTimestamp:TvkCmdWriteTimestamp=nil;
    vkCmdCopyQueryPoolResults:TvkCmdCopyQueryPoolResults=nil;
    vkCmdPushConstants:TvkCmdPushConstants=nil;
    vkCmdBeginRenderPass:TvkCmdBeginRenderPass=nil;
    vkCmdNextSubpass:TvkCmdNextSubpass=nil;
    vkCmdEndRenderPass:TvkCmdEndRenderPass=nil;
    vkCmdExecuteCommands:TvkCmdExecuteCommands=nil;
{$ifdef Android}
    vkCreateAndroidSurfaceKHR:TvkCreateAndroidSurfaceKHR=nil;
{$endif}
    vkGetPhysicalDeviceDisplayPropertiesKHR:TvkGetPhysicalDeviceDisplayPropertiesKHR=nil;
    vkGetPhysicalDeviceDisplayPlanePropertiesKHR:TvkGetPhysicalDeviceDisplayPlanePropertiesKHR=nil;
    vkGetDisplayPlaneSupportedDisplaysKHR:TvkGetDisplayPlaneSupportedDisplaysKHR=nil;
    vkGetDisplayModePropertiesKHR:TvkGetDisplayModePropertiesKHR=nil;
    vkCreateDisplayModeKHR:TvkCreateDisplayModeKHR=nil;
    vkGetDisplayPlaneCapabilitiesKHR:TvkGetDisplayPlaneCapabilitiesKHR=nil;
    vkCreateDisplayPlaneSurfaceKHR:TvkCreateDisplayPlaneSurfaceKHR=nil;
    vkCreateSharedSwapchainsKHR:TvkCreateSharedSwapchainsKHR=nil;
{$ifdef Mir}
    vkCreateMirSurfaceKHR:TvkCreateMirSurfaceKHR=nil;
{$endif}
{$ifdef Mir}
    vkGetPhysicalDeviceMirPresentationSupportKHR:TvkGetPhysicalDeviceMirPresentationSupportKHR=nil;
{$endif}
    vkDestroySurfaceKHR:TvkDestroySurfaceKHR=nil;
    vkGetPhysicalDeviceSurfaceSupportKHR:TvkGetPhysicalDeviceSurfaceSupportKHR=nil;
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR:TvkGetPhysicalDeviceSurfaceCapabilitiesKHR=nil;
    vkGetPhysicalDeviceSurfaceFormatsKHR:TvkGetPhysicalDeviceSurfaceFormatsKHR=nil;
    vkGetPhysicalDeviceSurfacePresentModesKHR:TvkGetPhysicalDeviceSurfacePresentModesKHR=nil;
    vkCreateSwapchainKHR:TvkCreateSwapchainKHR=nil;
    vkDestroySwapchainKHR:TvkDestroySwapchainKHR=nil;
    vkGetSwapchainImagesKHR:TvkGetSwapchainImagesKHR=nil;
    vkAcquireNextImageKHR:TvkAcquireNextImageKHR=nil;
    vkQueuePresentKHR:TvkQueuePresentKHR=nil;
    vkCreateViSurfaceNN:TvkCreateViSurfaceNN=nil;
{$ifdef Wayland}
    vkCreateWaylandSurfaceKHR:TvkCreateWaylandSurfaceKHR=nil;
{$endif}
{$ifdef Wayland}
    vkGetPhysicalDeviceWaylandPresentationSupportKHR:TvkGetPhysicalDeviceWaylandPresentationSupportKHR=nil;
{$endif}
{$ifdef Windows}
    vkCreateWin32SurfaceKHR:TvkCreateWin32SurfaceKHR=nil;
{$endif}
{$ifdef Windows}
    vkGetPhysicalDeviceWin32PresentationSupportKHR:TvkGetPhysicalDeviceWin32PresentationSupportKHR=nil;
{$endif}
{$ifdef XLIB}
    vkCreateXlibSurfaceKHR:TvkCreateXlibSurfaceKHR=nil;
{$endif}
{$ifdef XLIB}
    vkGetPhysicalDeviceXlibPresentationSupportKHR:TvkGetPhysicalDeviceXlibPresentationSupportKHR=nil;
{$endif}
{$ifdef XCB}
    vkCreateXcbSurfaceKHR:TvkCreateXcbSurfaceKHR=nil;
{$endif}
{$ifdef XCB}
    vkGetPhysicalDeviceXcbPresentationSupportKHR:TvkGetPhysicalDeviceXcbPresentationSupportKHR=nil;
{$endif}
    vkCreateDebugReportCallbackEXT:TvkCreateDebugReportCallbackEXT=nil;
    vkDestroyDebugReportCallbackEXT:TvkDestroyDebugReportCallbackEXT=nil;
    vkDebugReportMessageEXT:TvkDebugReportMessageEXT=nil;
    vkDebugMarkerSetObjectNameEXT:TvkDebugMarkerSetObjectNameEXT=nil;
    vkDebugMarkerSetObjectTagEXT:TvkDebugMarkerSetObjectTagEXT=nil;
    vkCmdDebugMarkerBeginEXT:TvkCmdDebugMarkerBeginEXT=nil;
    vkCmdDebugMarkerEndEXT:TvkCmdDebugMarkerEndEXT=nil;
    vkCmdDebugMarkerInsertEXT:TvkCmdDebugMarkerInsertEXT=nil;
    vkGetPhysicalDeviceExternalImageFormatPropertiesNV:TvkGetPhysicalDeviceExternalImageFormatPropertiesNV=nil;
{$ifdef Windows}
    vkGetMemoryWin32HandleNV:TvkGetMemoryWin32HandleNV=nil;
{$endif}
    vkCmdDrawIndirectCountAMD:TvkCmdDrawIndirectCountAMD=nil;
    vkCmdDrawIndexedIndirectCountAMD:TvkCmdDrawIndexedIndirectCountAMD=nil;
    vkCmdProcessCommandsNVX:TvkCmdProcessCommandsNVX=nil;
    vkCmdReserveSpaceForCommandsNVX:TvkCmdReserveSpaceForCommandsNVX=nil;
    vkCreateIndirectCommandsLayoutNVX:TvkCreateIndirectCommandsLayoutNVX=nil;
    vkDestroyIndirectCommandsLayoutNVX:TvkDestroyIndirectCommandsLayoutNVX=nil;
    vkCreateObjectTableNVX:TvkCreateObjectTableNVX=nil;
    vkDestroyObjectTableNVX:TvkDestroyObjectTableNVX=nil;
    vkRegisterObjectsNVX:TvkRegisterObjectsNVX=nil;
    vkUnregisterObjectsNVX:TvkUnregisterObjectsNVX=nil;
    vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX:TvkGetPhysicalDeviceGeneratedCommandsPropertiesNVX=nil;
    vkGetPhysicalDeviceFeatures2KHR:TvkGetPhysicalDeviceFeatures2KHR=nil;
    vkGetPhysicalDeviceProperties2KHR:TvkGetPhysicalDeviceProperties2KHR=nil;
    vkGetPhysicalDeviceFormatProperties2KHR:TvkGetPhysicalDeviceFormatProperties2KHR=nil;
    vkGetPhysicalDeviceImageFormatProperties2KHR:TvkGetPhysicalDeviceImageFormatProperties2KHR=nil;
    vkGetPhysicalDeviceQueueFamilyProperties2KHR:TvkGetPhysicalDeviceQueueFamilyProperties2KHR=nil;
    vkGetPhysicalDeviceMemoryProperties2KHR:TvkGetPhysicalDeviceMemoryProperties2KHR=nil;
    vkGetPhysicalDeviceSparseImageFormatProperties2KHR:TvkGetPhysicalDeviceSparseImageFormatProperties2KHR=nil;
    vkCmdPushDescriptorSetKHR:TvkCmdPushDescriptorSetKHR=nil;
    vkTrimCommandPoolKHR:TvkTrimCommandPoolKHR=nil;
    vkGetPhysicalDeviceExternalBufferPropertiesKHR:TvkGetPhysicalDeviceExternalBufferPropertiesKHR=nil;
{$ifdef Windows}
    vkGetMemoryWin32HandleKHR:TvkGetMemoryWin32HandleKHR=nil;
{$endif}
{$ifdef Windows}
    vkGetMemoryWin32HandlePropertiesKHR:TvkGetMemoryWin32HandlePropertiesKHR=nil;
{$endif}
    vkGetMemoryFdKHR:TvkGetMemoryFdKHR=nil;
    vkGetMemoryFdPropertiesKHR:TvkGetMemoryFdPropertiesKHR=nil;
    vkGetPhysicalDeviceExternalSemaphorePropertiesKHR:TvkGetPhysicalDeviceExternalSemaphorePropertiesKHR=nil;
{$ifdef Windows}
    vkGetSemaphoreWin32HandleKHR:TvkGetSemaphoreWin32HandleKHR=nil;
{$endif}
{$ifdef Windows}
    vkImportSemaphoreWin32HandleKHR:TvkImportSemaphoreWin32HandleKHR=nil;
{$endif}
    vkGetSemaphoreFdKHR:TvkGetSemaphoreFdKHR=nil;
    vkImportSemaphoreFdKHR:TvkImportSemaphoreFdKHR=nil;
    vkGetPhysicalDeviceExternalFencePropertiesKHR:TvkGetPhysicalDeviceExternalFencePropertiesKHR=nil;
{$ifdef Windows}
    vkGetFenceWin32HandleKHR:TvkGetFenceWin32HandleKHR=nil;
{$endif}
{$ifdef Windows}
    vkImportFenceWin32HandleKHR:TvkImportFenceWin32HandleKHR=nil;
{$endif}
    vkGetFenceFdKHR:TvkGetFenceFdKHR=nil;
    vkImportFenceFdKHR:TvkImportFenceFdKHR=nil;
    vkReleaseDisplayEXT:TvkReleaseDisplayEXT=nil;
{$ifdef XLIB}
    vkAcquireXlibDisplayEXT:TvkAcquireXlibDisplayEXT=nil;
{$endif}
{$ifdef RandR}
    vkGetRandROutputDisplayEXT:TvkGetRandROutputDisplayEXT=nil;
{$endif}
    vkDisplayPowerControlEXT:TvkDisplayPowerControlEXT=nil;
    vkRegisterDeviceEventEXT:TvkRegisterDeviceEventEXT=nil;
    vkRegisterDisplayEventEXT:TvkRegisterDisplayEventEXT=nil;
    vkGetSwapchainCounterEXT:TvkGetSwapchainCounterEXT=nil;
    vkGetPhysicalDeviceSurfaceCapabilities2EXT:TvkGetPhysicalDeviceSurfaceCapabilities2EXT=nil;
    vkEnumeratePhysicalDeviceGroupsKHX:TvkEnumeratePhysicalDeviceGroupsKHX=nil;
    vkGetDeviceGroupPeerMemoryFeaturesKHX:TvkGetDeviceGroupPeerMemoryFeaturesKHX=nil;
    vkBindBufferMemory2KHX:TvkBindBufferMemory2KHX=nil;
    vkBindImageMemory2KHX:TvkBindImageMemory2KHX=nil;
    vkCmdSetDeviceMaskKHX:TvkCmdSetDeviceMaskKHX=nil;
    vkGetDeviceGroupPresentCapabilitiesKHX:TvkGetDeviceGroupPresentCapabilitiesKHX=nil;
    vkGetDeviceGroupSurfacePresentModesKHX:TvkGetDeviceGroupSurfacePresentModesKHX=nil;
    vkAcquireNextImage2KHX:TvkAcquireNextImage2KHX=nil;
    vkCmdDispatchBaseKHX:TvkCmdDispatchBaseKHX=nil;
    vkGetPhysicalDevicePresentRectanglesKHX:TvkGetPhysicalDevicePresentRectanglesKHX=nil;
    vkCreateDescriptorUpdateTemplateKHR:TvkCreateDescriptorUpdateTemplateKHR=nil;
    vkDestroyDescriptorUpdateTemplateKHR:TvkDestroyDescriptorUpdateTemplateKHR=nil;
    vkUpdateDescriptorSetWithTemplateKHR:TvkUpdateDescriptorSetWithTemplateKHR=nil;
    vkCmdPushDescriptorSetWithTemplateKHR:TvkCmdPushDescriptorSetWithTemplateKHR=nil;
    vkSetHdrMetadataEXT:TvkSetHdrMetadataEXT=nil;
    vkGetSwapchainStatusKHR:TvkGetSwapchainStatusKHR=nil;
    vkGetRefreshCycleDurationGOOGLE:TvkGetRefreshCycleDurationGOOGLE=nil;
    vkGetPastPresentationTimingGOOGLE:TvkGetPastPresentationTimingGOOGLE=nil;
{$ifdef MoltenVK_IOS}
    vkCreateIOSSurfaceMVK:TvkCreateIOSSurfaceMVK=nil;
{$endif}
{$ifdef MoltenVK_MacOS}
    vkCreateMacOSSurfaceMVK:TvkCreateMacOSSurfaceMVK=nil;
{$endif}
    vkCmdSetViewportWScalingNV:TvkCmdSetViewportWScalingNV=nil;
    vkCmdSetDiscardRectangleEXT:TvkCmdSetDiscardRectangleEXT=nil;
    vkGetPhysicalDeviceSurfaceCapabilities2KHR:TvkGetPhysicalDeviceSurfaceCapabilities2KHR=nil;
    vkGetPhysicalDeviceSurfaceFormats2KHR:TvkGetPhysicalDeviceSurfaceFormats2KHR=nil;
    vkGetBufferMemoryRequirements2KHR:TvkGetBufferMemoryRequirements2KHR=nil;
    vkGetImageMemoryRequirements2KHR:TvkGetImageMemoryRequirements2KHR=nil;
    vkGetImageSparseMemoryRequirements2KHR:TvkGetImageSparseMemoryRequirements2KHR=nil;

//...............................................
    vkOCreateInstance:TvkCreateInstance;
    vkOGetInstanceProcAddr:TvkGetInstanceProcAddr;
    vkOEnumerateInstanceExtensionProperties:TvkEnumerateInstanceExtensionProperties;
    vkOEnumerateInstanceLayerProperties:TvkEnumerateInstanceLayerProperties;

//================ Global Functions =========================================

function vkMakeVersion(const aMajor, aMinor, aPatch: Integer): TVkVersion; inline;
function vkGetVersionMajor(const aVersion: TVkVersion): Integer; inline;
function vkGetVersionMinor(const aVersion: TVkVersion): Integer; inline;
function vkGetVersionPatch(const aVersion: TVkVersion): Integer; inline;

function  vkAPIInitialize(const aLibName: String = VK_DEFAULT_LIB_NAME): Boolean;
procedure vkAPIFinalize;
function  vkoAPIInitialize(const aLibName: String = VK_DEFAULT_LIB_NAME): Boolean;
function  vkoLoadInstanceFunctions(const aInstance: TVkInstance): TVkInstanceFunctions;
function  vkoLoadDeviceFunctions(const aInstance: TVkInstance; const aDevice: TVkDevice): TVkDeviceFunctions;


implementation

const
 InvalidLibHandle = 0;

type
  TvkLibHandle = TLibHandle;

var
  vkLibHandle: TvkLibHandle = InvalidLibHandle;

function vkMakeVersion(const aMajor, aMinor, aPatch: Integer): TVkVersion;
begin
  result := (aMajor shl 22) or (aMinor shl 12) or aPatch;
end;

function vkGetVersionMajor(const aVersion: TVkVersion): Integer;
begin
  result := (aVersion shr 22);
end;

function vkGetVersionMinor(const aVersion: TVkVersion): Integer;
begin
  result := (aVersion shr 12) and $3FF;
end;
 
function vkGetVersionPatch(const aVersion: TVkVersion): Integer;
begin
  result := (aVersion and $FFF);
end;

function vkLoadLibrary(const aLibName: String): TvkLibHandle;
  var xLibName: String;
begin
  result:=0;
  xLibName:=aLibName;
  result := LoadLibrary(xLibName);
end;

function vkGetProcAddress(const aProcName: String; aLibHandle: TvkLibHandle = InvalidLibHandle): TPFN_vkVoidFunction;
begin
  result := nil;
  if (aLibHandle = InvalidLibHandle) then
    aLibHandle := vkLibHandle;

  result := TPFN_vkVoidFunction(GetProcAddress(aLibHandle, AnsiString(aProcName)));
end;

procedure vkFreeLibrary(var aLibHandle: TvkLibHandle);
begin
  if (aLibHandle <> InvalidLibHandle) then
      aLibHandle := InvalidLibHandle;
end;

function vkAPIInitialize(const aLibName: String = VK_DEFAULT_LIB_NAME): Boolean;
begin
 if vkLibHandle = InvalidLibHandle then
    vkLibHandle := vkLoadLibrary(aLibName);

  result := (vkLibHandle <> InvalidLibHandle);

  if not result then exit;

    //............................ Total Global Functions: 247
        vkCreateInstance := TvkCreateInstance(vkGetProcAddress('vkCreateInstance'));
        vkDestroyInstance := TvkDestroyInstance(vkGetProcAddress('vkDestroyInstance'));
        vkEnumeratePhysicalDevices := TvkEnumeratePhysicalDevices(vkGetProcAddress('vkEnumeratePhysicalDevices'));
        vkGetDeviceProcAddr := TvkGetDeviceProcAddr(vkGetProcAddress('vkGetDeviceProcAddr'));
        vkGetInstanceProcAddr := TvkGetInstanceProcAddr(vkGetProcAddress('vkGetInstanceProcAddr'));
        vkGetPhysicalDeviceProperties := TvkGetPhysicalDeviceProperties(vkGetProcAddress('vkGetPhysicalDeviceProperties'));
        vkGetPhysicalDeviceQueueFamilyProperties := TvkGetPhysicalDeviceQueueFamilyProperties(vkGetProcAddress('vkGetPhysicalDeviceQueueFamilyProperties'));
        vkGetPhysicalDeviceMemoryProperties := TvkGetPhysicalDeviceMemoryProperties(vkGetProcAddress('vkGetPhysicalDeviceMemoryProperties'));
        vkGetPhysicalDeviceFeatures := TvkGetPhysicalDeviceFeatures(vkGetProcAddress('vkGetPhysicalDeviceFeatures'));
        vkGetPhysicalDeviceFormatProperties := TvkGetPhysicalDeviceFormatProperties(vkGetProcAddress('vkGetPhysicalDeviceFormatProperties'));
        vkGetPhysicalDeviceImageFormatProperties := TvkGetPhysicalDeviceImageFormatProperties(vkGetProcAddress('vkGetPhysicalDeviceImageFormatProperties'));
        vkCreateDevice := TvkCreateDevice(vkGetProcAddress('vkCreateDevice'));
        vkDestroyDevice := TvkDestroyDevice(vkGetProcAddress('vkDestroyDevice'));
        vkEnumerateInstanceLayerProperties := TvkEnumerateInstanceLayerProperties(vkGetProcAddress('vkEnumerateInstanceLayerProperties'));
        vkEnumerateInstanceExtensionProperties := TvkEnumerateInstanceExtensionProperties(vkGetProcAddress('vkEnumerateInstanceExtensionProperties'));
        vkEnumerateDeviceLayerProperties := TvkEnumerateDeviceLayerProperties(vkGetProcAddress('vkEnumerateDeviceLayerProperties'));
        vkEnumerateDeviceExtensionProperties := TvkEnumerateDeviceExtensionProperties(vkGetProcAddress('vkEnumerateDeviceExtensionProperties'));
        vkGetDeviceQueue := TvkGetDeviceQueue(vkGetProcAddress('vkGetDeviceQueue'));
        vkQueueSubmit := TvkQueueSubmit(vkGetProcAddress('vkQueueSubmit'));
        vkQueueWaitIdle := TvkQueueWaitIdle(vkGetProcAddress('vkQueueWaitIdle'));
        vkDeviceWaitIdle := TvkDeviceWaitIdle(vkGetProcAddress('vkDeviceWaitIdle'));
        vkAllocateMemory := TvkAllocateMemory(vkGetProcAddress('vkAllocateMemory'));
        vkFreeMemory := TvkFreeMemory(vkGetProcAddress('vkFreeMemory'));
        vkMapMemory := TvkMapMemory(vkGetProcAddress('vkMapMemory'));
        vkUnmapMemory := TvkUnmapMemory(vkGetProcAddress('vkUnmapMemory'));
        vkFlushMappedMemoryRanges := TvkFlushMappedMemoryRanges(vkGetProcAddress('vkFlushMappedMemoryRanges'));
        vkInvalidateMappedMemoryRanges := TvkInvalidateMappedMemoryRanges(vkGetProcAddress('vkInvalidateMappedMemoryRanges'));
        vkGetDeviceMemoryCommitment := TvkGetDeviceMemoryCommitment(vkGetProcAddress('vkGetDeviceMemoryCommitment'));
        vkGetBufferMemoryRequirements := TvkGetBufferMemoryRequirements(vkGetProcAddress('vkGetBufferMemoryRequirements'));
        vkBindBufferMemory := TvkBindBufferMemory(vkGetProcAddress('vkBindBufferMemory'));
        vkGetImageMemoryRequirements := TvkGetImageMemoryRequirements(vkGetProcAddress('vkGetImageMemoryRequirements'));
        vkBindImageMemory := TvkBindImageMemory(vkGetProcAddress('vkBindImageMemory'));
        vkGetImageSparseMemoryRequirements := TvkGetImageSparseMemoryRequirements(vkGetProcAddress('vkGetImageSparseMemoryRequirements'));
        vkGetPhysicalDeviceSparseImageFormatProperties := TvkGetPhysicalDeviceSparseImageFormatProperties(vkGetProcAddress('vkGetPhysicalDeviceSparseImageFormatProperties'));
        vkQueueBindSparse := TvkQueueBindSparse(vkGetProcAddress('vkQueueBindSparse'));
        vkCreateFence := TvkCreateFence(vkGetProcAddress('vkCreateFence'));
        vkDestroyFence := TvkDestroyFence(vkGetProcAddress('vkDestroyFence'));
        vkResetFences := TvkResetFences(vkGetProcAddress('vkResetFences'));
        vkGetFenceStatus := TvkGetFenceStatus(vkGetProcAddress('vkGetFenceStatus'));
        vkWaitForFences := TvkWaitForFences(vkGetProcAddress('vkWaitForFences'));
        vkCreateSemaphore := TvkCreateSemaphore(vkGetProcAddress('vkCreateSemaphore'));
        vkDestroySemaphore := TvkDestroySemaphore(vkGetProcAddress('vkDestroySemaphore'));
        vkCreateEvent := TvkCreateEvent(vkGetProcAddress('vkCreateEvent'));
        vkDestroyEvent := TvkDestroyEvent(vkGetProcAddress('vkDestroyEvent'));
        vkGetEventStatus := TvkGetEventStatus(vkGetProcAddress('vkGetEventStatus'));
        vkSetEvent := TvkSetEvent(vkGetProcAddress('vkSetEvent'));
        vkResetEvent := TvkResetEvent(vkGetProcAddress('vkResetEvent'));
        vkCreateQueryPool := TvkCreateQueryPool(vkGetProcAddress('vkCreateQueryPool'));
        vkDestroyQueryPool := TvkDestroyQueryPool(vkGetProcAddress('vkDestroyQueryPool'));
        vkGetQueryPoolResults := TvkGetQueryPoolResults(vkGetProcAddress('vkGetQueryPoolResults'));
        vkCreateBuffer := TvkCreateBuffer(vkGetProcAddress('vkCreateBuffer'));
        vkDestroyBuffer := TvkDestroyBuffer(vkGetProcAddress('vkDestroyBuffer'));
        vkCreateBufferView := TvkCreateBufferView(vkGetProcAddress('vkCreateBufferView'));
        vkDestroyBufferView := TvkDestroyBufferView(vkGetProcAddress('vkDestroyBufferView'));
        vkCreateImage := TvkCreateImage(vkGetProcAddress('vkCreateImage'));
        vkDestroyImage := TvkDestroyImage(vkGetProcAddress('vkDestroyImage'));
        vkGetImageSubresourceLayout := TvkGetImageSubresourceLayout(vkGetProcAddress('vkGetImageSubresourceLayout'));
        vkCreateImageView := TvkCreateImageView(vkGetProcAddress('vkCreateImageView'));
        vkDestroyImageView := TvkDestroyImageView(vkGetProcAddress('vkDestroyImageView'));
        vkCreateShaderModule := TvkCreateShaderModule(vkGetProcAddress('vkCreateShaderModule'));
        vkDestroyShaderModule := TvkDestroyShaderModule(vkGetProcAddress('vkDestroyShaderModule'));
        vkCreatePipelineCache := TvkCreatePipelineCache(vkGetProcAddress('vkCreatePipelineCache'));
        vkDestroyPipelineCache := TvkDestroyPipelineCache(vkGetProcAddress('vkDestroyPipelineCache'));
        vkGetPipelineCacheData := TvkGetPipelineCacheData(vkGetProcAddress('vkGetPipelineCacheData'));
        vkMergePipelineCaches := TvkMergePipelineCaches(vkGetProcAddress('vkMergePipelineCaches'));
        vkCreateGraphicsPipelines := TvkCreateGraphicsPipelines(vkGetProcAddress('vkCreateGraphicsPipelines'));
        vkCreateComputePipelines := TvkCreateComputePipelines(vkGetProcAddress('vkCreateComputePipelines'));
        vkDestroyPipeline := TvkDestroyPipeline(vkGetProcAddress('vkDestroyPipeline'));
        vkCreatePipelineLayout := TvkCreatePipelineLayout(vkGetProcAddress('vkCreatePipelineLayout'));
        vkDestroyPipelineLayout := TvkDestroyPipelineLayout(vkGetProcAddress('vkDestroyPipelineLayout'));
        vkCreateSampler := TvkCreateSampler(vkGetProcAddress('vkCreateSampler'));
        vkDestroySampler := TvkDestroySampler(vkGetProcAddress('vkDestroySampler'));
        vkCreateDescriptorSetLayout := TvkCreateDescriptorSetLayout(vkGetProcAddress('vkCreateDescriptorSetLayout'));
        vkDestroyDescriptorSetLayout := TvkDestroyDescriptorSetLayout(vkGetProcAddress('vkDestroyDescriptorSetLayout'));
        vkCreateDescriptorPool := TvkCreateDescriptorPool(vkGetProcAddress('vkCreateDescriptorPool'));
        vkDestroyDescriptorPool := TvkDestroyDescriptorPool(vkGetProcAddress('vkDestroyDescriptorPool'));
        vkResetDescriptorPool := TvkResetDescriptorPool(vkGetProcAddress('vkResetDescriptorPool'));
        vkAllocateDescriptorSets := TvkAllocateDescriptorSets(vkGetProcAddress('vkAllocateDescriptorSets'));
        vkFreeDescriptorSets := TvkFreeDescriptorSets(vkGetProcAddress('vkFreeDescriptorSets'));
        vkUpdateDescriptorSets := TvkUpdateDescriptorSets(vkGetProcAddress('vkUpdateDescriptorSets'));
        vkCreateFramebuffer := TvkCreateFramebuffer(vkGetProcAddress('vkCreateFramebuffer'));
        vkDestroyFramebuffer := TvkDestroyFramebuffer(vkGetProcAddress('vkDestroyFramebuffer'));
        vkCreateRenderPass := TvkCreateRenderPass(vkGetProcAddress('vkCreateRenderPass'));
        vkDestroyRenderPass := TvkDestroyRenderPass(vkGetProcAddress('vkDestroyRenderPass'));
        vkGetRenderAreaGranularity := TvkGetRenderAreaGranularity(vkGetProcAddress('vkGetRenderAreaGranularity'));
        vkCreateCommandPool := TvkCreateCommandPool(vkGetProcAddress('vkCreateCommandPool'));
        vkDestroyCommandPool := TvkDestroyCommandPool(vkGetProcAddress('vkDestroyCommandPool'));
        vkResetCommandPool := TvkResetCommandPool(vkGetProcAddress('vkResetCommandPool'));
        vkAllocateCommandBuffers := TvkAllocateCommandBuffers(vkGetProcAddress('vkAllocateCommandBuffers'));
        vkFreeCommandBuffers := TvkFreeCommandBuffers(vkGetProcAddress('vkFreeCommandBuffers'));
        vkBeginCommandBuffer := TvkBeginCommandBuffer(vkGetProcAddress('vkBeginCommandBuffer'));
        vkEndCommandBuffer := TvkEndCommandBuffer(vkGetProcAddress('vkEndCommandBuffer'));
        vkResetCommandBuffer := TvkResetCommandBuffer(vkGetProcAddress('vkResetCommandBuffer'));
        vkCmdBindPipeline := TvkCmdBindPipeline(vkGetProcAddress('vkCmdBindPipeline'));
        vkCmdSetViewport := TvkCmdSetViewport(vkGetProcAddress('vkCmdSetViewport'));
        vkCmdSetScissor := TvkCmdSetScissor(vkGetProcAddress('vkCmdSetScissor'));
        vkCmdSetLineWidth := TvkCmdSetLineWidth(vkGetProcAddress('vkCmdSetLineWidth'));
        vkCmdSetDepthBias := TvkCmdSetDepthBias(vkGetProcAddress('vkCmdSetDepthBias'));
        vkCmdSetBlendConstants := TvkCmdSetBlendConstants(vkGetProcAddress('vkCmdSetBlendConstants'));
        vkCmdSetDepthBounds := TvkCmdSetDepthBounds(vkGetProcAddress('vkCmdSetDepthBounds'));
        vkCmdSetStencilCompareMask := TvkCmdSetStencilCompareMask(vkGetProcAddress('vkCmdSetStencilCompareMask'));
        vkCmdSetStencilWriteMask := TvkCmdSetStencilWriteMask(vkGetProcAddress('vkCmdSetStencilWriteMask'));
        vkCmdSetStencilReference := TvkCmdSetStencilReference(vkGetProcAddress('vkCmdSetStencilReference'));
        vkCmdBindDescriptorSets := TvkCmdBindDescriptorSets(vkGetProcAddress('vkCmdBindDescriptorSets'));
        vkCmdBindIndexBuffer := TvkCmdBindIndexBuffer(vkGetProcAddress('vkCmdBindIndexBuffer'));
        vkCmdBindVertexBuffers := TvkCmdBindVertexBuffers(vkGetProcAddress('vkCmdBindVertexBuffers'));
        vkCmdDraw := TvkCmdDraw(vkGetProcAddress('vkCmdDraw'));
        vkCmdDrawIndexed := TvkCmdDrawIndexed(vkGetProcAddress('vkCmdDrawIndexed'));
        vkCmdDrawIndirect := TvkCmdDrawIndirect(vkGetProcAddress('vkCmdDrawIndirect'));
        vkCmdDrawIndexedIndirect := TvkCmdDrawIndexedIndirect(vkGetProcAddress('vkCmdDrawIndexedIndirect'));
        vkCmdDispatch := TvkCmdDispatch(vkGetProcAddress('vkCmdDispatch'));
        vkCmdDispatchIndirect := TvkCmdDispatchIndirect(vkGetProcAddress('vkCmdDispatchIndirect'));
        vkCmdCopyBuffer := TvkCmdCopyBuffer(vkGetProcAddress('vkCmdCopyBuffer'));
        vkCmdCopyImage := TvkCmdCopyImage(vkGetProcAddress('vkCmdCopyImage'));
        vkCmdBlitImage := TvkCmdBlitImage(vkGetProcAddress('vkCmdBlitImage'));
        vkCmdCopyBufferToImage := TvkCmdCopyBufferToImage(vkGetProcAddress('vkCmdCopyBufferToImage'));
        vkCmdCopyImageToBuffer := TvkCmdCopyImageToBuffer(vkGetProcAddress('vkCmdCopyImageToBuffer'));
        vkCmdUpdateBuffer := TvkCmdUpdateBuffer(vkGetProcAddress('vkCmdUpdateBuffer'));
        vkCmdFillBuffer := TvkCmdFillBuffer(vkGetProcAddress('vkCmdFillBuffer'));
        vkCmdClearColorImage := TvkCmdClearColorImage(vkGetProcAddress('vkCmdClearColorImage'));
        vkCmdClearDepthStencilImage := TvkCmdClearDepthStencilImage(vkGetProcAddress('vkCmdClearDepthStencilImage'));
        vkCmdClearAttachments := TvkCmdClearAttachments(vkGetProcAddress('vkCmdClearAttachments'));
        vkCmdResolveImage := TvkCmdResolveImage(vkGetProcAddress('vkCmdResolveImage'));
        vkCmdSetEvent := TvkCmdSetEvent(vkGetProcAddress('vkCmdSetEvent'));
        vkCmdResetEvent := TvkCmdResetEvent(vkGetProcAddress('vkCmdResetEvent'));
        vkCmdWaitEvents := TvkCmdWaitEvents(vkGetProcAddress('vkCmdWaitEvents'));
        vkCmdPipelineBarrier := TvkCmdPipelineBarrier(vkGetProcAddress('vkCmdPipelineBarrier'));
        vkCmdBeginQuery := TvkCmdBeginQuery(vkGetProcAddress('vkCmdBeginQuery'));
        vkCmdEndQuery := TvkCmdEndQuery(vkGetProcAddress('vkCmdEndQuery'));
        vkCmdResetQueryPool := TvkCmdResetQueryPool(vkGetProcAddress('vkCmdResetQueryPool'));
        vkCmdWriteTimestamp := TvkCmdWriteTimestamp(vkGetProcAddress('vkCmdWriteTimestamp'));
        vkCmdCopyQueryPoolResults := TvkCmdCopyQueryPoolResults(vkGetProcAddress('vkCmdCopyQueryPoolResults'));
        vkCmdPushConstants := TvkCmdPushConstants(vkGetProcAddress('vkCmdPushConstants'));
        vkCmdBeginRenderPass := TvkCmdBeginRenderPass(vkGetProcAddress('vkCmdBeginRenderPass'));
        vkCmdNextSubpass := TvkCmdNextSubpass(vkGetProcAddress('vkCmdNextSubpass'));
        vkCmdEndRenderPass := TvkCmdEndRenderPass(vkGetProcAddress('vkCmdEndRenderPass'));
        vkCmdExecuteCommands := TvkCmdExecuteCommands(vkGetProcAddress('vkCmdExecuteCommands'));
       {$ifdef Android}
        vkCreateAndroidSurfaceKHR := TvkCreateAndroidSurfaceKHR(vkGetProcAddress('vkCreateAndroidSurfaceKHR'));
       {$endif}
        vkGetPhysicalDeviceDisplayPropertiesKHR := TvkGetPhysicalDeviceDisplayPropertiesKHR(vkGetProcAddress('vkGetPhysicalDeviceDisplayPropertiesKHR'));
        vkGetPhysicalDeviceDisplayPlanePropertiesKHR := TvkGetPhysicalDeviceDisplayPlanePropertiesKHR(vkGetProcAddress('vkGetPhysicalDeviceDisplayPlanePropertiesKHR'));
        vkGetDisplayPlaneSupportedDisplaysKHR := TvkGetDisplayPlaneSupportedDisplaysKHR(vkGetProcAddress('vkGetDisplayPlaneSupportedDisplaysKHR'));
        vkGetDisplayModePropertiesKHR := TvkGetDisplayModePropertiesKHR(vkGetProcAddress('vkGetDisplayModePropertiesKHR'));
        vkCreateDisplayModeKHR := TvkCreateDisplayModeKHR(vkGetProcAddress('vkCreateDisplayModeKHR'));
        vkGetDisplayPlaneCapabilitiesKHR := TvkGetDisplayPlaneCapabilitiesKHR(vkGetProcAddress('vkGetDisplayPlaneCapabilitiesKHR'));
        vkCreateDisplayPlaneSurfaceKHR := TvkCreateDisplayPlaneSurfaceKHR(vkGetProcAddress('vkCreateDisplayPlaneSurfaceKHR'));
        vkCreateSharedSwapchainsKHR := TvkCreateSharedSwapchainsKHR(vkGetProcAddress('vkCreateSharedSwapchainsKHR'));
       {$ifdef Mir}
        vkCreateMirSurfaceKHR := TvkCreateMirSurfaceKHR(vkGetProcAddress('vkCreateMirSurfaceKHR'));
       {$endif}
       {$ifdef Mir}
        vkGetPhysicalDeviceMirPresentationSupportKHR := TvkGetPhysicalDeviceMirPresentationSupportKHR(vkGetProcAddress('vkGetPhysicalDeviceMirPresentationSupportKHR'));
       {$endif}
        vkDestroySurfaceKHR := TvkDestroySurfaceKHR(vkGetProcAddress('vkDestroySurfaceKHR'));
        vkGetPhysicalDeviceSurfaceSupportKHR := TvkGetPhysicalDeviceSurfaceSupportKHR(vkGetProcAddress('vkGetPhysicalDeviceSurfaceSupportKHR'));
        vkGetPhysicalDeviceSurfaceCapabilitiesKHR := TvkGetPhysicalDeviceSurfaceCapabilitiesKHR(vkGetProcAddress('vkGetPhysicalDeviceSurfaceCapabilitiesKHR'));
        vkGetPhysicalDeviceSurfaceFormatsKHR := TvkGetPhysicalDeviceSurfaceFormatsKHR(vkGetProcAddress('vkGetPhysicalDeviceSurfaceFormatsKHR'));
        vkGetPhysicalDeviceSurfacePresentModesKHR := TvkGetPhysicalDeviceSurfacePresentModesKHR(vkGetProcAddress('vkGetPhysicalDeviceSurfacePresentModesKHR'));
        vkCreateSwapchainKHR := TvkCreateSwapchainKHR(vkGetProcAddress('vkCreateSwapchainKHR'));
        vkDestroySwapchainKHR := TvkDestroySwapchainKHR(vkGetProcAddress('vkDestroySwapchainKHR'));
        vkGetSwapchainImagesKHR := TvkGetSwapchainImagesKHR(vkGetProcAddress('vkGetSwapchainImagesKHR'));
        vkAcquireNextImageKHR := TvkAcquireNextImageKHR(vkGetProcAddress('vkAcquireNextImageKHR'));
        vkQueuePresentKHR := TvkQueuePresentKHR(vkGetProcAddress('vkQueuePresentKHR'));
        vkCreateViSurfaceNN := TvkCreateViSurfaceNN(vkGetProcAddress('vkCreateViSurfaceNN'));
       {$ifdef Wayland}
        vkCreateWaylandSurfaceKHR := TvkCreateWaylandSurfaceKHR(vkGetProcAddress('vkCreateWaylandSurfaceKHR'));
       {$endif}
       {$ifdef Wayland}
        vkGetPhysicalDeviceWaylandPresentationSupportKHR := TvkGetPhysicalDeviceWaylandPresentationSupportKHR(vkGetProcAddress('vkGetPhysicalDeviceWaylandPresentationSupportKHR'));
       {$endif}
       {$ifdef Windows}
        vkCreateWin32SurfaceKHR := TvkCreateWin32SurfaceKHR(vkGetProcAddress('vkCreateWin32SurfaceKHR'));
       {$endif}
       {$ifdef Windows}
        vkGetPhysicalDeviceWin32PresentationSupportKHR := TvkGetPhysicalDeviceWin32PresentationSupportKHR(vkGetProcAddress('vkGetPhysicalDeviceWin32PresentationSupportKHR'));
       {$endif}
       {$ifdef XLIB}
        vkCreateXlibSurfaceKHR := TvkCreateXlibSurfaceKHR(vkGetProcAddress('vkCreateXlibSurfaceKHR'));
       {$endif}
       {$ifdef XLIB}
        vkGetPhysicalDeviceXlibPresentationSupportKHR := TvkGetPhysicalDeviceXlibPresentationSupportKHR(vkGetProcAddress('vkGetPhysicalDeviceXlibPresentationSupportKHR'));
       {$endif}
       {$ifdef XCB}
        vkCreateXcbSurfaceKHR := TvkCreateXcbSurfaceKHR(vkGetProcAddress('vkCreateXcbSurfaceKHR'));
       {$endif}
       {$ifdef XCB}
        vkGetPhysicalDeviceXcbPresentationSupportKHR := TvkGetPhysicalDeviceXcbPresentationSupportKHR(vkGetProcAddress('vkGetPhysicalDeviceXcbPresentationSupportKHR'));
       {$endif}
        vkCreateDebugReportCallbackEXT := TvkCreateDebugReportCallbackEXT(vkGetProcAddress('vkCreateDebugReportCallbackEXT'));
        vkDestroyDebugReportCallbackEXT := TvkDestroyDebugReportCallbackEXT(vkGetProcAddress('vkDestroyDebugReportCallbackEXT'));
        vkDebugReportMessageEXT := TvkDebugReportMessageEXT(vkGetProcAddress('vkDebugReportMessageEXT'));
        vkDebugMarkerSetObjectNameEXT := TvkDebugMarkerSetObjectNameEXT(vkGetProcAddress('vkDebugMarkerSetObjectNameEXT'));
        vkDebugMarkerSetObjectTagEXT := TvkDebugMarkerSetObjectTagEXT(vkGetProcAddress('vkDebugMarkerSetObjectTagEXT'));
        vkCmdDebugMarkerBeginEXT := TvkCmdDebugMarkerBeginEXT(vkGetProcAddress('vkCmdDebugMarkerBeginEXT'));
        vkCmdDebugMarkerEndEXT := TvkCmdDebugMarkerEndEXT(vkGetProcAddress('vkCmdDebugMarkerEndEXT'));
        vkCmdDebugMarkerInsertEXT := TvkCmdDebugMarkerInsertEXT(vkGetProcAddress('vkCmdDebugMarkerInsertEXT'));
        vkGetPhysicalDeviceExternalImageFormatPropertiesNV := TvkGetPhysicalDeviceExternalImageFormatPropertiesNV(vkGetProcAddress('vkGetPhysicalDeviceExternalImageFormatPropertiesNV'));
       {$ifdef Windows}
        vkGetMemoryWin32HandleNV := TvkGetMemoryWin32HandleNV(vkGetProcAddress('vkGetMemoryWin32HandleNV'));
       {$endif}
        vkCmdDrawIndirectCountAMD := TvkCmdDrawIndirectCountAMD(vkGetProcAddress('vkCmdDrawIndirectCountAMD'));
        vkCmdDrawIndexedIndirectCountAMD := TvkCmdDrawIndexedIndirectCountAMD(vkGetProcAddress('vkCmdDrawIndexedIndirectCountAMD'));
        vkCmdProcessCommandsNVX := TvkCmdProcessCommandsNVX(vkGetProcAddress('vkCmdProcessCommandsNVX'));
        vkCmdReserveSpaceForCommandsNVX := TvkCmdReserveSpaceForCommandsNVX(vkGetProcAddress('vkCmdReserveSpaceForCommandsNVX'));
        vkCreateIndirectCommandsLayoutNVX := TvkCreateIndirectCommandsLayoutNVX(vkGetProcAddress('vkCreateIndirectCommandsLayoutNVX'));
        vkDestroyIndirectCommandsLayoutNVX := TvkDestroyIndirectCommandsLayoutNVX(vkGetProcAddress('vkDestroyIndirectCommandsLayoutNVX'));
        vkCreateObjectTableNVX := TvkCreateObjectTableNVX(vkGetProcAddress('vkCreateObjectTableNVX'));
        vkDestroyObjectTableNVX := TvkDestroyObjectTableNVX(vkGetProcAddress('vkDestroyObjectTableNVX'));
        vkRegisterObjectsNVX := TvkRegisterObjectsNVX(vkGetProcAddress('vkRegisterObjectsNVX'));
        vkUnregisterObjectsNVX := TvkUnregisterObjectsNVX(vkGetProcAddress('vkUnregisterObjectsNVX'));
        vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX := TvkGetPhysicalDeviceGeneratedCommandsPropertiesNVX(vkGetProcAddress('vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX'));
        vkGetPhysicalDeviceFeatures2KHR := TvkGetPhysicalDeviceFeatures2KHR(vkGetProcAddress('vkGetPhysicalDeviceFeatures2KHR'));
        vkGetPhysicalDeviceProperties2KHR := TvkGetPhysicalDeviceProperties2KHR(vkGetProcAddress('vkGetPhysicalDeviceProperties2KHR'));
        vkGetPhysicalDeviceFormatProperties2KHR := TvkGetPhysicalDeviceFormatProperties2KHR(vkGetProcAddress('vkGetPhysicalDeviceFormatProperties2KHR'));
        vkGetPhysicalDeviceImageFormatProperties2KHR := TvkGetPhysicalDeviceImageFormatProperties2KHR(vkGetProcAddress('vkGetPhysicalDeviceImageFormatProperties2KHR'));
        vkGetPhysicalDeviceQueueFamilyProperties2KHR := TvkGetPhysicalDeviceQueueFamilyProperties2KHR(vkGetProcAddress('vkGetPhysicalDeviceQueueFamilyProperties2KHR'));
        vkGetPhysicalDeviceMemoryProperties2KHR := TvkGetPhysicalDeviceMemoryProperties2KHR(vkGetProcAddress('vkGetPhysicalDeviceMemoryProperties2KHR'));
        vkGetPhysicalDeviceSparseImageFormatProperties2KHR := TvkGetPhysicalDeviceSparseImageFormatProperties2KHR(vkGetProcAddress('vkGetPhysicalDeviceSparseImageFormatProperties2KHR'));
        vkCmdPushDescriptorSetKHR := TvkCmdPushDescriptorSetKHR(vkGetProcAddress('vkCmdPushDescriptorSetKHR'));
        vkTrimCommandPoolKHR := TvkTrimCommandPoolKHR(vkGetProcAddress('vkTrimCommandPoolKHR'));
        vkGetPhysicalDeviceExternalBufferPropertiesKHR := TvkGetPhysicalDeviceExternalBufferPropertiesKHR(vkGetProcAddress('vkGetPhysicalDeviceExternalBufferPropertiesKHR'));
       {$ifdef Windows}
        vkGetMemoryWin32HandleKHR := TvkGetMemoryWin32HandleKHR(vkGetProcAddress('vkGetMemoryWin32HandleKHR'));
       {$endif}
       {$ifdef Windows}
        vkGetMemoryWin32HandlePropertiesKHR := TvkGetMemoryWin32HandlePropertiesKHR(vkGetProcAddress('vkGetMemoryWin32HandlePropertiesKHR'));
       {$endif}
        vkGetMemoryFdKHR := TvkGetMemoryFdKHR(vkGetProcAddress('vkGetMemoryFdKHR'));
        vkGetMemoryFdPropertiesKHR := TvkGetMemoryFdPropertiesKHR(vkGetProcAddress('vkGetMemoryFdPropertiesKHR'));
        vkGetPhysicalDeviceExternalSemaphorePropertiesKHR := TvkGetPhysicalDeviceExternalSemaphorePropertiesKHR(vkGetProcAddress('vkGetPhysicalDeviceExternalSemaphorePropertiesKHR'));
       {$ifdef Windows}
        vkGetSemaphoreWin32HandleKHR := TvkGetSemaphoreWin32HandleKHR(vkGetProcAddress('vkGetSemaphoreWin32HandleKHR'));
       {$endif}
       {$ifdef Windows}
        vkImportSemaphoreWin32HandleKHR := TvkImportSemaphoreWin32HandleKHR(vkGetProcAddress('vkImportSemaphoreWin32HandleKHR'));
       {$endif}
        vkGetSemaphoreFdKHR := TvkGetSemaphoreFdKHR(vkGetProcAddress('vkGetSemaphoreFdKHR'));
        vkImportSemaphoreFdKHR := TvkImportSemaphoreFdKHR(vkGetProcAddress('vkImportSemaphoreFdKHR'));
        vkGetPhysicalDeviceExternalFencePropertiesKHR := TvkGetPhysicalDeviceExternalFencePropertiesKHR(vkGetProcAddress('vkGetPhysicalDeviceExternalFencePropertiesKHR'));
       {$ifdef Windows}
        vkGetFenceWin32HandleKHR := TvkGetFenceWin32HandleKHR(vkGetProcAddress('vkGetFenceWin32HandleKHR'));
       {$endif}
       {$ifdef Windows}
        vkImportFenceWin32HandleKHR := TvkImportFenceWin32HandleKHR(vkGetProcAddress('vkImportFenceWin32HandleKHR'));
       {$endif}
        vkGetFenceFdKHR := TvkGetFenceFdKHR(vkGetProcAddress('vkGetFenceFdKHR'));
        vkImportFenceFdKHR := TvkImportFenceFdKHR(vkGetProcAddress('vkImportFenceFdKHR'));
        vkReleaseDisplayEXT := TvkReleaseDisplayEXT(vkGetProcAddress('vkReleaseDisplayEXT'));
       {$ifdef XLIB}
        vkAcquireXlibDisplayEXT := TvkAcquireXlibDisplayEXT(vkGetProcAddress('vkAcquireXlibDisplayEXT'));
       {$endif}
       {$ifdef RandR}
        vkGetRandROutputDisplayEXT := TvkGetRandROutputDisplayEXT(vkGetProcAddress('vkGetRandROutputDisplayEXT'));
       {$endif}
        vkDisplayPowerControlEXT := TvkDisplayPowerControlEXT(vkGetProcAddress('vkDisplayPowerControlEXT'));
        vkRegisterDeviceEventEXT := TvkRegisterDeviceEventEXT(vkGetProcAddress('vkRegisterDeviceEventEXT'));
        vkRegisterDisplayEventEXT := TvkRegisterDisplayEventEXT(vkGetProcAddress('vkRegisterDisplayEventEXT'));
        vkGetSwapchainCounterEXT := TvkGetSwapchainCounterEXT(vkGetProcAddress('vkGetSwapchainCounterEXT'));
        vkGetPhysicalDeviceSurfaceCapabilities2EXT := TvkGetPhysicalDeviceSurfaceCapabilities2EXT(vkGetProcAddress('vkGetPhysicalDeviceSurfaceCapabilities2EXT'));
        vkEnumeratePhysicalDeviceGroupsKHX := TvkEnumeratePhysicalDeviceGroupsKHX(vkGetProcAddress('vkEnumeratePhysicalDeviceGroupsKHX'));
        vkGetDeviceGroupPeerMemoryFeaturesKHX := TvkGetDeviceGroupPeerMemoryFeaturesKHX(vkGetProcAddress('vkGetDeviceGroupPeerMemoryFeaturesKHX'));
        vkBindBufferMemory2KHX := TvkBindBufferMemory2KHX(vkGetProcAddress('vkBindBufferMemory2KHX'));
        vkBindImageMemory2KHX := TvkBindImageMemory2KHX(vkGetProcAddress('vkBindImageMemory2KHX'));
        vkCmdSetDeviceMaskKHX := TvkCmdSetDeviceMaskKHX(vkGetProcAddress('vkCmdSetDeviceMaskKHX'));
        vkGetDeviceGroupPresentCapabilitiesKHX := TvkGetDeviceGroupPresentCapabilitiesKHX(vkGetProcAddress('vkGetDeviceGroupPresentCapabilitiesKHX'));
        vkGetDeviceGroupSurfacePresentModesKHX := TvkGetDeviceGroupSurfacePresentModesKHX(vkGetProcAddress('vkGetDeviceGroupSurfacePresentModesKHX'));
        vkAcquireNextImage2KHX := TvkAcquireNextImage2KHX(vkGetProcAddress('vkAcquireNextImage2KHX'));
        vkCmdDispatchBaseKHX := TvkCmdDispatchBaseKHX(vkGetProcAddress('vkCmdDispatchBaseKHX'));
        vkGetPhysicalDevicePresentRectanglesKHX := TvkGetPhysicalDevicePresentRectanglesKHX(vkGetProcAddress('vkGetPhysicalDevicePresentRectanglesKHX'));
        vkCreateDescriptorUpdateTemplateKHR := TvkCreateDescriptorUpdateTemplateKHR(vkGetProcAddress('vkCreateDescriptorUpdateTemplateKHR'));
        vkDestroyDescriptorUpdateTemplateKHR := TvkDestroyDescriptorUpdateTemplateKHR(vkGetProcAddress('vkDestroyDescriptorUpdateTemplateKHR'));
        vkUpdateDescriptorSetWithTemplateKHR := TvkUpdateDescriptorSetWithTemplateKHR(vkGetProcAddress('vkUpdateDescriptorSetWithTemplateKHR'));
        vkCmdPushDescriptorSetWithTemplateKHR := TvkCmdPushDescriptorSetWithTemplateKHR(vkGetProcAddress('vkCmdPushDescriptorSetWithTemplateKHR'));
        vkSetHdrMetadataEXT := TvkSetHdrMetadataEXT(vkGetProcAddress('vkSetHdrMetadataEXT'));
        vkGetSwapchainStatusKHR := TvkGetSwapchainStatusKHR(vkGetProcAddress('vkGetSwapchainStatusKHR'));
        vkGetRefreshCycleDurationGOOGLE := TvkGetRefreshCycleDurationGOOGLE(vkGetProcAddress('vkGetRefreshCycleDurationGOOGLE'));
        vkGetPastPresentationTimingGOOGLE := TvkGetPastPresentationTimingGOOGLE(vkGetProcAddress('vkGetPastPresentationTimingGOOGLE'));
       {$ifdef MoltenVK_IOS}
        vkCreateIOSSurfaceMVK := TvkCreateIOSSurfaceMVK(vkGetProcAddress('vkCreateIOSSurfaceMVK'));
       {$endif}
       {$ifdef MoltenVK_MacOS}
        vkCreateMacOSSurfaceMVK := TvkCreateMacOSSurfaceMVK(vkGetProcAddress('vkCreateMacOSSurfaceMVK'));
       {$endif}
        vkCmdSetViewportWScalingNV := TvkCmdSetViewportWScalingNV(vkGetProcAddress('vkCmdSetViewportWScalingNV'));
        vkCmdSetDiscardRectangleEXT := TvkCmdSetDiscardRectangleEXT(vkGetProcAddress('vkCmdSetDiscardRectangleEXT'));
        vkGetPhysicalDeviceSurfaceCapabilities2KHR := TvkGetPhysicalDeviceSurfaceCapabilities2KHR(vkGetProcAddress('vkGetPhysicalDeviceSurfaceCapabilities2KHR'));
        vkGetPhysicalDeviceSurfaceFormats2KHR := TvkGetPhysicalDeviceSurfaceFormats2KHR(vkGetProcAddress('vkGetPhysicalDeviceSurfaceFormats2KHR'));
        vkGetBufferMemoryRequirements2KHR := TvkGetBufferMemoryRequirements2KHR(vkGetProcAddress('vkGetBufferMemoryRequirements2KHR'));
        vkGetImageMemoryRequirements2KHR := TvkGetImageMemoryRequirements2KHR(vkGetProcAddress('vkGetImageMemoryRequirements2KHR'));
        vkGetImageSparseMemoryRequirements2KHR := TvkGetImageSparseMemoryRequirements2KHR(vkGetProcAddress('vkGetImageSparseMemoryRequirements2KHR'));

 vkoAPIInitialize(aLibName)

end;


function vkoAPIInitialize(const aLibName: String = VK_DEFAULT_LIB_NAME): Boolean;
begin
 if vkLibHandle = InvalidLibHandle then
    vkLibHandle := vkLoadLibrary(aLibName);

  result := (vkLibHandle <> InvalidLibHandle);

  if not result then exit;

//.... Only 4 Vulkan functions loaded as Global functions ....

  vkOCreateInstance:= TvkCreateInstance(vkGetProcAddress('vkCreateInstance'));
  vkOGetInstanceProcAddr:= TvkGetInstanceProcAddr(vkGetProcAddress('vkGetInstanceProcAddr'));
  vkOEnumerateInstanceExtensionProperties:= TvkEnumerateInstanceExtensionProperties(vkGetProcAddress('vkEnumerateInstanceExtensionProperties'));
  vkOEnumerateInstanceLayerProperties:= TvkEnumerateInstanceLayerProperties(vkGetProcAddress('vkEnumerateInstanceLayerProperties'));

  if vkCreateInstance=nil then vkCreateInstance:=vkOCreateInstance;
  if vkGetInstanceProcAddr=nil then vkGetInstanceProcAddr:=vkOGetInstanceProcAddr;
  if vkEnumerateInstanceExtensionProperties=nil then vkEnumerateInstanceExtensionProperties:=vkOEnumerateInstanceExtensionProperties;
  if vkEnumerateInstanceLayerProperties=nil then vkEnumerateInstanceLayerProperties:=vkOEnumerateInstanceLayerProperties;
end;

function vkoLoadInstanceFunctions(const aInstance: TVkInstance): TVkInstanceFunctions;
  //.....................
  function _GetProcAddress(const aName: String): TPFN_vkVoidFunction;
  begin
    result := vkOGetInstanceProcAddr(aInstance, PVkChar(aName));
  end;
  //.....................
begin
  result  := TVkInstanceFunctions.Create;
  try
    result.aaInstance := aInstance;
    with result do begin
    //............................ Total Instance Functions: 247
        vkCreateInstance := TvkCreateInstance(_GetProcAddress('vkCreateInstance'));
        vkDestroyInstance := TvkDestroyInstance(_GetProcAddress('vkDestroyInstance'));
        vkEnumeratePhysicalDevices := TvkEnumeratePhysicalDevices(_GetProcAddress('vkEnumeratePhysicalDevices'));
        vkGetDeviceProcAddr := TvkGetDeviceProcAddr(_GetProcAddress('vkGetDeviceProcAddr'));
        vkGetInstanceProcAddr := TvkGetInstanceProcAddr(_GetProcAddress('vkGetInstanceProcAddr'));
        vkGetPhysicalDeviceProperties := TvkGetPhysicalDeviceProperties(_GetProcAddress('vkGetPhysicalDeviceProperties'));
        vkGetPhysicalDeviceQueueFamilyProperties := TvkGetPhysicalDeviceQueueFamilyProperties(_GetProcAddress('vkGetPhysicalDeviceQueueFamilyProperties'));
        vkGetPhysicalDeviceMemoryProperties := TvkGetPhysicalDeviceMemoryProperties(_GetProcAddress('vkGetPhysicalDeviceMemoryProperties'));
        vkGetPhysicalDeviceFeatures := TvkGetPhysicalDeviceFeatures(_GetProcAddress('vkGetPhysicalDeviceFeatures'));
        vkGetPhysicalDeviceFormatProperties := TvkGetPhysicalDeviceFormatProperties(_GetProcAddress('vkGetPhysicalDeviceFormatProperties'));
        vkGetPhysicalDeviceImageFormatProperties := TvkGetPhysicalDeviceImageFormatProperties(_GetProcAddress('vkGetPhysicalDeviceImageFormatProperties'));
        vkCreateDevice := TvkCreateDevice(_GetProcAddress('vkCreateDevice'));
        vkDestroyDevice := TvkDestroyDevice(_GetProcAddress('vkDestroyDevice'));
        vkEnumerateInstanceLayerProperties := TvkEnumerateInstanceLayerProperties(_GetProcAddress('vkEnumerateInstanceLayerProperties'));
        vkEnumerateInstanceExtensionProperties := TvkEnumerateInstanceExtensionProperties(_GetProcAddress('vkEnumerateInstanceExtensionProperties'));
        vkEnumerateDeviceLayerProperties := TvkEnumerateDeviceLayerProperties(_GetProcAddress('vkEnumerateDeviceLayerProperties'));
        vkEnumerateDeviceExtensionProperties := TvkEnumerateDeviceExtensionProperties(_GetProcAddress('vkEnumerateDeviceExtensionProperties'));
        vkGetDeviceQueue := TvkGetDeviceQueue(_GetProcAddress('vkGetDeviceQueue'));
        vkQueueSubmit := TvkQueueSubmit(_GetProcAddress('vkQueueSubmit'));
        vkQueueWaitIdle := TvkQueueWaitIdle(_GetProcAddress('vkQueueWaitIdle'));
        vkDeviceWaitIdle := TvkDeviceWaitIdle(_GetProcAddress('vkDeviceWaitIdle'));
        vkAllocateMemory := TvkAllocateMemory(_GetProcAddress('vkAllocateMemory'));
        vkFreeMemory := TvkFreeMemory(_GetProcAddress('vkFreeMemory'));
        vkMapMemory := TvkMapMemory(_GetProcAddress('vkMapMemory'));
        vkUnmapMemory := TvkUnmapMemory(_GetProcAddress('vkUnmapMemory'));
        vkFlushMappedMemoryRanges := TvkFlushMappedMemoryRanges(_GetProcAddress('vkFlushMappedMemoryRanges'));
        vkInvalidateMappedMemoryRanges := TvkInvalidateMappedMemoryRanges(_GetProcAddress('vkInvalidateMappedMemoryRanges'));
        vkGetDeviceMemoryCommitment := TvkGetDeviceMemoryCommitment(_GetProcAddress('vkGetDeviceMemoryCommitment'));
        vkGetBufferMemoryRequirements := TvkGetBufferMemoryRequirements(_GetProcAddress('vkGetBufferMemoryRequirements'));
        vkBindBufferMemory := TvkBindBufferMemory(_GetProcAddress('vkBindBufferMemory'));
        vkGetImageMemoryRequirements := TvkGetImageMemoryRequirements(_GetProcAddress('vkGetImageMemoryRequirements'));
        vkBindImageMemory := TvkBindImageMemory(_GetProcAddress('vkBindImageMemory'));
        vkGetImageSparseMemoryRequirements := TvkGetImageSparseMemoryRequirements(_GetProcAddress('vkGetImageSparseMemoryRequirements'));
        vkGetPhysicalDeviceSparseImageFormatProperties := TvkGetPhysicalDeviceSparseImageFormatProperties(_GetProcAddress('vkGetPhysicalDeviceSparseImageFormatProperties'));
        vkQueueBindSparse := TvkQueueBindSparse(_GetProcAddress('vkQueueBindSparse'));
        vkCreateFence := TvkCreateFence(_GetProcAddress('vkCreateFence'));
        vkDestroyFence := TvkDestroyFence(_GetProcAddress('vkDestroyFence'));
        vkResetFences := TvkResetFences(_GetProcAddress('vkResetFences'));
        vkGetFenceStatus := TvkGetFenceStatus(_GetProcAddress('vkGetFenceStatus'));
        vkWaitForFences := TvkWaitForFences(_GetProcAddress('vkWaitForFences'));
        vkCreateSemaphore := TvkCreateSemaphore(_GetProcAddress('vkCreateSemaphore'));
        vkDestroySemaphore := TvkDestroySemaphore(_GetProcAddress('vkDestroySemaphore'));
        vkCreateEvent := TvkCreateEvent(_GetProcAddress('vkCreateEvent'));
        vkDestroyEvent := TvkDestroyEvent(_GetProcAddress('vkDestroyEvent'));
        vkGetEventStatus := TvkGetEventStatus(_GetProcAddress('vkGetEventStatus'));
        vkSetEvent := TvkSetEvent(_GetProcAddress('vkSetEvent'));
        vkResetEvent := TvkResetEvent(_GetProcAddress('vkResetEvent'));
        vkCreateQueryPool := TvkCreateQueryPool(_GetProcAddress('vkCreateQueryPool'));
        vkDestroyQueryPool := TvkDestroyQueryPool(_GetProcAddress('vkDestroyQueryPool'));
        vkGetQueryPoolResults := TvkGetQueryPoolResults(_GetProcAddress('vkGetQueryPoolResults'));
        vkCreateBuffer := TvkCreateBuffer(_GetProcAddress('vkCreateBuffer'));
        vkDestroyBuffer := TvkDestroyBuffer(_GetProcAddress('vkDestroyBuffer'));
        vkCreateBufferView := TvkCreateBufferView(_GetProcAddress('vkCreateBufferView'));
        vkDestroyBufferView := TvkDestroyBufferView(_GetProcAddress('vkDestroyBufferView'));
        vkCreateImage := TvkCreateImage(_GetProcAddress('vkCreateImage'));
        vkDestroyImage := TvkDestroyImage(_GetProcAddress('vkDestroyImage'));
        vkGetImageSubresourceLayout := TvkGetImageSubresourceLayout(_GetProcAddress('vkGetImageSubresourceLayout'));
        vkCreateImageView := TvkCreateImageView(_GetProcAddress('vkCreateImageView'));
        vkDestroyImageView := TvkDestroyImageView(_GetProcAddress('vkDestroyImageView'));
        vkCreateShaderModule := TvkCreateShaderModule(_GetProcAddress('vkCreateShaderModule'));
        vkDestroyShaderModule := TvkDestroyShaderModule(_GetProcAddress('vkDestroyShaderModule'));
        vkCreatePipelineCache := TvkCreatePipelineCache(_GetProcAddress('vkCreatePipelineCache'));
        vkDestroyPipelineCache := TvkDestroyPipelineCache(_GetProcAddress('vkDestroyPipelineCache'));
        vkGetPipelineCacheData := TvkGetPipelineCacheData(_GetProcAddress('vkGetPipelineCacheData'));
        vkMergePipelineCaches := TvkMergePipelineCaches(_GetProcAddress('vkMergePipelineCaches'));
        vkCreateGraphicsPipelines := TvkCreateGraphicsPipelines(_GetProcAddress('vkCreateGraphicsPipelines'));
        vkCreateComputePipelines := TvkCreateComputePipelines(_GetProcAddress('vkCreateComputePipelines'));
        vkDestroyPipeline := TvkDestroyPipeline(_GetProcAddress('vkDestroyPipeline'));
        vkCreatePipelineLayout := TvkCreatePipelineLayout(_GetProcAddress('vkCreatePipelineLayout'));
        vkDestroyPipelineLayout := TvkDestroyPipelineLayout(_GetProcAddress('vkDestroyPipelineLayout'));
        vkCreateSampler := TvkCreateSampler(_GetProcAddress('vkCreateSampler'));
        vkDestroySampler := TvkDestroySampler(_GetProcAddress('vkDestroySampler'));
        vkCreateDescriptorSetLayout := TvkCreateDescriptorSetLayout(_GetProcAddress('vkCreateDescriptorSetLayout'));
        vkDestroyDescriptorSetLayout := TvkDestroyDescriptorSetLayout(_GetProcAddress('vkDestroyDescriptorSetLayout'));
        vkCreateDescriptorPool := TvkCreateDescriptorPool(_GetProcAddress('vkCreateDescriptorPool'));
        vkDestroyDescriptorPool := TvkDestroyDescriptorPool(_GetProcAddress('vkDestroyDescriptorPool'));
        vkResetDescriptorPool := TvkResetDescriptorPool(_GetProcAddress('vkResetDescriptorPool'));
        vkAllocateDescriptorSets := TvkAllocateDescriptorSets(_GetProcAddress('vkAllocateDescriptorSets'));
        vkFreeDescriptorSets := TvkFreeDescriptorSets(_GetProcAddress('vkFreeDescriptorSets'));
        vkUpdateDescriptorSets := TvkUpdateDescriptorSets(_GetProcAddress('vkUpdateDescriptorSets'));
        vkCreateFramebuffer := TvkCreateFramebuffer(_GetProcAddress('vkCreateFramebuffer'));
        vkDestroyFramebuffer := TvkDestroyFramebuffer(_GetProcAddress('vkDestroyFramebuffer'));
        vkCreateRenderPass := TvkCreateRenderPass(_GetProcAddress('vkCreateRenderPass'));
        vkDestroyRenderPass := TvkDestroyRenderPass(_GetProcAddress('vkDestroyRenderPass'));
        vkGetRenderAreaGranularity := TvkGetRenderAreaGranularity(_GetProcAddress('vkGetRenderAreaGranularity'));
        vkCreateCommandPool := TvkCreateCommandPool(_GetProcAddress('vkCreateCommandPool'));
        vkDestroyCommandPool := TvkDestroyCommandPool(_GetProcAddress('vkDestroyCommandPool'));
        vkResetCommandPool := TvkResetCommandPool(_GetProcAddress('vkResetCommandPool'));
        vkAllocateCommandBuffers := TvkAllocateCommandBuffers(_GetProcAddress('vkAllocateCommandBuffers'));
        vkFreeCommandBuffers := TvkFreeCommandBuffers(_GetProcAddress('vkFreeCommandBuffers'));
        vkBeginCommandBuffer := TvkBeginCommandBuffer(_GetProcAddress('vkBeginCommandBuffer'));
        vkEndCommandBuffer := TvkEndCommandBuffer(_GetProcAddress('vkEndCommandBuffer'));
        vkResetCommandBuffer := TvkResetCommandBuffer(_GetProcAddress('vkResetCommandBuffer'));
        vkCmdBindPipeline := TvkCmdBindPipeline(_GetProcAddress('vkCmdBindPipeline'));
        vkCmdSetViewport := TvkCmdSetViewport(_GetProcAddress('vkCmdSetViewport'));
        vkCmdSetScissor := TvkCmdSetScissor(_GetProcAddress('vkCmdSetScissor'));
        vkCmdSetLineWidth := TvkCmdSetLineWidth(_GetProcAddress('vkCmdSetLineWidth'));
        vkCmdSetDepthBias := TvkCmdSetDepthBias(_GetProcAddress('vkCmdSetDepthBias'));
        vkCmdSetBlendConstants := TvkCmdSetBlendConstants(_GetProcAddress('vkCmdSetBlendConstants'));
        vkCmdSetDepthBounds := TvkCmdSetDepthBounds(_GetProcAddress('vkCmdSetDepthBounds'));
        vkCmdSetStencilCompareMask := TvkCmdSetStencilCompareMask(_GetProcAddress('vkCmdSetStencilCompareMask'));
        vkCmdSetStencilWriteMask := TvkCmdSetStencilWriteMask(_GetProcAddress('vkCmdSetStencilWriteMask'));
        vkCmdSetStencilReference := TvkCmdSetStencilReference(_GetProcAddress('vkCmdSetStencilReference'));
        vkCmdBindDescriptorSets := TvkCmdBindDescriptorSets(_GetProcAddress('vkCmdBindDescriptorSets'));
        vkCmdBindIndexBuffer := TvkCmdBindIndexBuffer(_GetProcAddress('vkCmdBindIndexBuffer'));
        vkCmdBindVertexBuffers := TvkCmdBindVertexBuffers(_GetProcAddress('vkCmdBindVertexBuffers'));
        vkCmdDraw := TvkCmdDraw(_GetProcAddress('vkCmdDraw'));
        vkCmdDrawIndexed := TvkCmdDrawIndexed(_GetProcAddress('vkCmdDrawIndexed'));
        vkCmdDrawIndirect := TvkCmdDrawIndirect(_GetProcAddress('vkCmdDrawIndirect'));
        vkCmdDrawIndexedIndirect := TvkCmdDrawIndexedIndirect(_GetProcAddress('vkCmdDrawIndexedIndirect'));
        vkCmdDispatch := TvkCmdDispatch(_GetProcAddress('vkCmdDispatch'));
        vkCmdDispatchIndirect := TvkCmdDispatchIndirect(_GetProcAddress('vkCmdDispatchIndirect'));
        vkCmdCopyBuffer := TvkCmdCopyBuffer(_GetProcAddress('vkCmdCopyBuffer'));
        vkCmdCopyImage := TvkCmdCopyImage(_GetProcAddress('vkCmdCopyImage'));
        vkCmdBlitImage := TvkCmdBlitImage(_GetProcAddress('vkCmdBlitImage'));
        vkCmdCopyBufferToImage := TvkCmdCopyBufferToImage(_GetProcAddress('vkCmdCopyBufferToImage'));
        vkCmdCopyImageToBuffer := TvkCmdCopyImageToBuffer(_GetProcAddress('vkCmdCopyImageToBuffer'));
        vkCmdUpdateBuffer := TvkCmdUpdateBuffer(_GetProcAddress('vkCmdUpdateBuffer'));
        vkCmdFillBuffer := TvkCmdFillBuffer(_GetProcAddress('vkCmdFillBuffer'));
        vkCmdClearColorImage := TvkCmdClearColorImage(_GetProcAddress('vkCmdClearColorImage'));
        vkCmdClearDepthStencilImage := TvkCmdClearDepthStencilImage(_GetProcAddress('vkCmdClearDepthStencilImage'));
        vkCmdClearAttachments := TvkCmdClearAttachments(_GetProcAddress('vkCmdClearAttachments'));
        vkCmdResolveImage := TvkCmdResolveImage(_GetProcAddress('vkCmdResolveImage'));
        vkCmdSetEvent := TvkCmdSetEvent(_GetProcAddress('vkCmdSetEvent'));
        vkCmdResetEvent := TvkCmdResetEvent(_GetProcAddress('vkCmdResetEvent'));
        vkCmdWaitEvents := TvkCmdWaitEvents(_GetProcAddress('vkCmdWaitEvents'));
        vkCmdPipelineBarrier := TvkCmdPipelineBarrier(_GetProcAddress('vkCmdPipelineBarrier'));
        vkCmdBeginQuery := TvkCmdBeginQuery(_GetProcAddress('vkCmdBeginQuery'));
        vkCmdEndQuery := TvkCmdEndQuery(_GetProcAddress('vkCmdEndQuery'));
        vkCmdResetQueryPool := TvkCmdResetQueryPool(_GetProcAddress('vkCmdResetQueryPool'));
        vkCmdWriteTimestamp := TvkCmdWriteTimestamp(_GetProcAddress('vkCmdWriteTimestamp'));
        vkCmdCopyQueryPoolResults := TvkCmdCopyQueryPoolResults(_GetProcAddress('vkCmdCopyQueryPoolResults'));
        vkCmdPushConstants := TvkCmdPushConstants(_GetProcAddress('vkCmdPushConstants'));
        vkCmdBeginRenderPass := TvkCmdBeginRenderPass(_GetProcAddress('vkCmdBeginRenderPass'));
        vkCmdNextSubpass := TvkCmdNextSubpass(_GetProcAddress('vkCmdNextSubpass'));
        vkCmdEndRenderPass := TvkCmdEndRenderPass(_GetProcAddress('vkCmdEndRenderPass'));
        vkCmdExecuteCommands := TvkCmdExecuteCommands(_GetProcAddress('vkCmdExecuteCommands'));
       {$ifdef Android}
        vkCreateAndroidSurfaceKHR := TvkCreateAndroidSurfaceKHR(_GetProcAddress('vkCreateAndroidSurfaceKHR'));
       {$endif}
        vkGetPhysicalDeviceDisplayPropertiesKHR := TvkGetPhysicalDeviceDisplayPropertiesKHR(_GetProcAddress('vkGetPhysicalDeviceDisplayPropertiesKHR'));
        vkGetPhysicalDeviceDisplayPlanePropertiesKHR := TvkGetPhysicalDeviceDisplayPlanePropertiesKHR(_GetProcAddress('vkGetPhysicalDeviceDisplayPlanePropertiesKHR'));
        vkGetDisplayPlaneSupportedDisplaysKHR := TvkGetDisplayPlaneSupportedDisplaysKHR(_GetProcAddress('vkGetDisplayPlaneSupportedDisplaysKHR'));
        vkGetDisplayModePropertiesKHR := TvkGetDisplayModePropertiesKHR(_GetProcAddress('vkGetDisplayModePropertiesKHR'));
        vkCreateDisplayModeKHR := TvkCreateDisplayModeKHR(_GetProcAddress('vkCreateDisplayModeKHR'));
        vkGetDisplayPlaneCapabilitiesKHR := TvkGetDisplayPlaneCapabilitiesKHR(_GetProcAddress('vkGetDisplayPlaneCapabilitiesKHR'));
        vkCreateDisplayPlaneSurfaceKHR := TvkCreateDisplayPlaneSurfaceKHR(_GetProcAddress('vkCreateDisplayPlaneSurfaceKHR'));
        vkCreateSharedSwapchainsKHR := TvkCreateSharedSwapchainsKHR(_GetProcAddress('vkCreateSharedSwapchainsKHR'));
       {$ifdef Mir}
        vkCreateMirSurfaceKHR := TvkCreateMirSurfaceKHR(_GetProcAddress('vkCreateMirSurfaceKHR'));
       {$endif}
       {$ifdef Mir}
        vkGetPhysicalDeviceMirPresentationSupportKHR := TvkGetPhysicalDeviceMirPresentationSupportKHR(_GetProcAddress('vkGetPhysicalDeviceMirPresentationSupportKHR'));
       {$endif}
        vkDestroySurfaceKHR := TvkDestroySurfaceKHR(_GetProcAddress('vkDestroySurfaceKHR'));
        vkGetPhysicalDeviceSurfaceSupportKHR := TvkGetPhysicalDeviceSurfaceSupportKHR(_GetProcAddress('vkGetPhysicalDeviceSurfaceSupportKHR'));
        vkGetPhysicalDeviceSurfaceCapabilitiesKHR := TvkGetPhysicalDeviceSurfaceCapabilitiesKHR(_GetProcAddress('vkGetPhysicalDeviceSurfaceCapabilitiesKHR'));
        vkGetPhysicalDeviceSurfaceFormatsKHR := TvkGetPhysicalDeviceSurfaceFormatsKHR(_GetProcAddress('vkGetPhysicalDeviceSurfaceFormatsKHR'));
        vkGetPhysicalDeviceSurfacePresentModesKHR := TvkGetPhysicalDeviceSurfacePresentModesKHR(_GetProcAddress('vkGetPhysicalDeviceSurfacePresentModesKHR'));
        vkCreateSwapchainKHR := TvkCreateSwapchainKHR(_GetProcAddress('vkCreateSwapchainKHR'));
        vkDestroySwapchainKHR := TvkDestroySwapchainKHR(_GetProcAddress('vkDestroySwapchainKHR'));
        vkGetSwapchainImagesKHR := TvkGetSwapchainImagesKHR(_GetProcAddress('vkGetSwapchainImagesKHR'));
        vkAcquireNextImageKHR := TvkAcquireNextImageKHR(_GetProcAddress('vkAcquireNextImageKHR'));
        vkQueuePresentKHR := TvkQueuePresentKHR(_GetProcAddress('vkQueuePresentKHR'));
        vkCreateViSurfaceNN := TvkCreateViSurfaceNN(_GetProcAddress('vkCreateViSurfaceNN'));
       {$ifdef Wayland}
        vkCreateWaylandSurfaceKHR := TvkCreateWaylandSurfaceKHR(_GetProcAddress('vkCreateWaylandSurfaceKHR'));
       {$endif}
       {$ifdef Wayland}
        vkGetPhysicalDeviceWaylandPresentationSupportKHR := TvkGetPhysicalDeviceWaylandPresentationSupportKHR(_GetProcAddress('vkGetPhysicalDeviceWaylandPresentationSupportKHR'));
       {$endif}
       {$ifdef Windows}
        vkCreateWin32SurfaceKHR := TvkCreateWin32SurfaceKHR(_GetProcAddress('vkCreateWin32SurfaceKHR'));
       {$endif}
       {$ifdef Windows}
        vkGetPhysicalDeviceWin32PresentationSupportKHR := TvkGetPhysicalDeviceWin32PresentationSupportKHR(_GetProcAddress('vkGetPhysicalDeviceWin32PresentationSupportKHR'));
       {$endif}
       {$ifdef XLIB}
        vkCreateXlibSurfaceKHR := TvkCreateXlibSurfaceKHR(_GetProcAddress('vkCreateXlibSurfaceKHR'));
       {$endif}
       {$ifdef XLIB}
        vkGetPhysicalDeviceXlibPresentationSupportKHR := TvkGetPhysicalDeviceXlibPresentationSupportKHR(_GetProcAddress('vkGetPhysicalDeviceXlibPresentationSupportKHR'));
       {$endif}
       {$ifdef XCB}
        vkCreateXcbSurfaceKHR := TvkCreateXcbSurfaceKHR(_GetProcAddress('vkCreateXcbSurfaceKHR'));
       {$endif}
       {$ifdef XCB}
        vkGetPhysicalDeviceXcbPresentationSupportKHR := TvkGetPhysicalDeviceXcbPresentationSupportKHR(_GetProcAddress('vkGetPhysicalDeviceXcbPresentationSupportKHR'));
       {$endif}
        vkCreateDebugReportCallbackEXT := TvkCreateDebugReportCallbackEXT(_GetProcAddress('vkCreateDebugReportCallbackEXT'));
        vkDestroyDebugReportCallbackEXT := TvkDestroyDebugReportCallbackEXT(_GetProcAddress('vkDestroyDebugReportCallbackEXT'));
        vkDebugReportMessageEXT := TvkDebugReportMessageEXT(_GetProcAddress('vkDebugReportMessageEXT'));
        vkDebugMarkerSetObjectNameEXT := TvkDebugMarkerSetObjectNameEXT(_GetProcAddress('vkDebugMarkerSetObjectNameEXT'));
        vkDebugMarkerSetObjectTagEXT := TvkDebugMarkerSetObjectTagEXT(_GetProcAddress('vkDebugMarkerSetObjectTagEXT'));
        vkCmdDebugMarkerBeginEXT := TvkCmdDebugMarkerBeginEXT(_GetProcAddress('vkCmdDebugMarkerBeginEXT'));
        vkCmdDebugMarkerEndEXT := TvkCmdDebugMarkerEndEXT(_GetProcAddress('vkCmdDebugMarkerEndEXT'));
        vkCmdDebugMarkerInsertEXT := TvkCmdDebugMarkerInsertEXT(_GetProcAddress('vkCmdDebugMarkerInsertEXT'));
        vkGetPhysicalDeviceExternalImageFormatPropertiesNV := TvkGetPhysicalDeviceExternalImageFormatPropertiesNV(_GetProcAddress('vkGetPhysicalDeviceExternalImageFormatPropertiesNV'));
       {$ifdef Windows}
        vkGetMemoryWin32HandleNV := TvkGetMemoryWin32HandleNV(_GetProcAddress('vkGetMemoryWin32HandleNV'));
       {$endif}
        vkCmdDrawIndirectCountAMD := TvkCmdDrawIndirectCountAMD(_GetProcAddress('vkCmdDrawIndirectCountAMD'));
        vkCmdDrawIndexedIndirectCountAMD := TvkCmdDrawIndexedIndirectCountAMD(_GetProcAddress('vkCmdDrawIndexedIndirectCountAMD'));
        vkCmdProcessCommandsNVX := TvkCmdProcessCommandsNVX(_GetProcAddress('vkCmdProcessCommandsNVX'));
        vkCmdReserveSpaceForCommandsNVX := TvkCmdReserveSpaceForCommandsNVX(_GetProcAddress('vkCmdReserveSpaceForCommandsNVX'));
        vkCreateIndirectCommandsLayoutNVX := TvkCreateIndirectCommandsLayoutNVX(_GetProcAddress('vkCreateIndirectCommandsLayoutNVX'));
        vkDestroyIndirectCommandsLayoutNVX := TvkDestroyIndirectCommandsLayoutNVX(_GetProcAddress('vkDestroyIndirectCommandsLayoutNVX'));
        vkCreateObjectTableNVX := TvkCreateObjectTableNVX(_GetProcAddress('vkCreateObjectTableNVX'));
        vkDestroyObjectTableNVX := TvkDestroyObjectTableNVX(_GetProcAddress('vkDestroyObjectTableNVX'));
        vkRegisterObjectsNVX := TvkRegisterObjectsNVX(_GetProcAddress('vkRegisterObjectsNVX'));
        vkUnregisterObjectsNVX := TvkUnregisterObjectsNVX(_GetProcAddress('vkUnregisterObjectsNVX'));
        vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX := TvkGetPhysicalDeviceGeneratedCommandsPropertiesNVX(_GetProcAddress('vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX'));
        vkGetPhysicalDeviceFeatures2KHR := TvkGetPhysicalDeviceFeatures2KHR(_GetProcAddress('vkGetPhysicalDeviceFeatures2KHR'));
        vkGetPhysicalDeviceProperties2KHR := TvkGetPhysicalDeviceProperties2KHR(_GetProcAddress('vkGetPhysicalDeviceProperties2KHR'));
        vkGetPhysicalDeviceFormatProperties2KHR := TvkGetPhysicalDeviceFormatProperties2KHR(_GetProcAddress('vkGetPhysicalDeviceFormatProperties2KHR'));
        vkGetPhysicalDeviceImageFormatProperties2KHR := TvkGetPhysicalDeviceImageFormatProperties2KHR(_GetProcAddress('vkGetPhysicalDeviceImageFormatProperties2KHR'));
        vkGetPhysicalDeviceQueueFamilyProperties2KHR := TvkGetPhysicalDeviceQueueFamilyProperties2KHR(_GetProcAddress('vkGetPhysicalDeviceQueueFamilyProperties2KHR'));
        vkGetPhysicalDeviceMemoryProperties2KHR := TvkGetPhysicalDeviceMemoryProperties2KHR(_GetProcAddress('vkGetPhysicalDeviceMemoryProperties2KHR'));
        vkGetPhysicalDeviceSparseImageFormatProperties2KHR := TvkGetPhysicalDeviceSparseImageFormatProperties2KHR(_GetProcAddress('vkGetPhysicalDeviceSparseImageFormatProperties2KHR'));
        vkCmdPushDescriptorSetKHR := TvkCmdPushDescriptorSetKHR(_GetProcAddress('vkCmdPushDescriptorSetKHR'));
        vkTrimCommandPoolKHR := TvkTrimCommandPoolKHR(_GetProcAddress('vkTrimCommandPoolKHR'));
        vkGetPhysicalDeviceExternalBufferPropertiesKHR := TvkGetPhysicalDeviceExternalBufferPropertiesKHR(_GetProcAddress('vkGetPhysicalDeviceExternalBufferPropertiesKHR'));
       {$ifdef Windows}
        vkGetMemoryWin32HandleKHR := TvkGetMemoryWin32HandleKHR(_GetProcAddress('vkGetMemoryWin32HandleKHR'));
       {$endif}
       {$ifdef Windows}
        vkGetMemoryWin32HandlePropertiesKHR := TvkGetMemoryWin32HandlePropertiesKHR(_GetProcAddress('vkGetMemoryWin32HandlePropertiesKHR'));
       {$endif}
        vkGetMemoryFdKHR := TvkGetMemoryFdKHR(_GetProcAddress('vkGetMemoryFdKHR'));
        vkGetMemoryFdPropertiesKHR := TvkGetMemoryFdPropertiesKHR(_GetProcAddress('vkGetMemoryFdPropertiesKHR'));
        vkGetPhysicalDeviceExternalSemaphorePropertiesKHR := TvkGetPhysicalDeviceExternalSemaphorePropertiesKHR(_GetProcAddress('vkGetPhysicalDeviceExternalSemaphorePropertiesKHR'));
       {$ifdef Windows}
        vkGetSemaphoreWin32HandleKHR := TvkGetSemaphoreWin32HandleKHR(_GetProcAddress('vkGetSemaphoreWin32HandleKHR'));
       {$endif}
       {$ifdef Windows}
        vkImportSemaphoreWin32HandleKHR := TvkImportSemaphoreWin32HandleKHR(_GetProcAddress('vkImportSemaphoreWin32HandleKHR'));
       {$endif}
        vkGetSemaphoreFdKHR := TvkGetSemaphoreFdKHR(_GetProcAddress('vkGetSemaphoreFdKHR'));
        vkImportSemaphoreFdKHR := TvkImportSemaphoreFdKHR(_GetProcAddress('vkImportSemaphoreFdKHR'));
        vkGetPhysicalDeviceExternalFencePropertiesKHR := TvkGetPhysicalDeviceExternalFencePropertiesKHR(_GetProcAddress('vkGetPhysicalDeviceExternalFencePropertiesKHR'));
       {$ifdef Windows}
        vkGetFenceWin32HandleKHR := TvkGetFenceWin32HandleKHR(_GetProcAddress('vkGetFenceWin32HandleKHR'));
       {$endif}
       {$ifdef Windows}
        vkImportFenceWin32HandleKHR := TvkImportFenceWin32HandleKHR(_GetProcAddress('vkImportFenceWin32HandleKHR'));
       {$endif}
        vkGetFenceFdKHR := TvkGetFenceFdKHR(_GetProcAddress('vkGetFenceFdKHR'));
        vkImportFenceFdKHR := TvkImportFenceFdKHR(_GetProcAddress('vkImportFenceFdKHR'));
        vkReleaseDisplayEXT := TvkReleaseDisplayEXT(_GetProcAddress('vkReleaseDisplayEXT'));
       {$ifdef XLIB}
        vkAcquireXlibDisplayEXT := TvkAcquireXlibDisplayEXT(_GetProcAddress('vkAcquireXlibDisplayEXT'));
       {$endif}
       {$ifdef RandR}
        vkGetRandROutputDisplayEXT := TvkGetRandROutputDisplayEXT(_GetProcAddress('vkGetRandROutputDisplayEXT'));
       {$endif}
        vkDisplayPowerControlEXT := TvkDisplayPowerControlEXT(_GetProcAddress('vkDisplayPowerControlEXT'));
        vkRegisterDeviceEventEXT := TvkRegisterDeviceEventEXT(_GetProcAddress('vkRegisterDeviceEventEXT'));
        vkRegisterDisplayEventEXT := TvkRegisterDisplayEventEXT(_GetProcAddress('vkRegisterDisplayEventEXT'));
        vkGetSwapchainCounterEXT := TvkGetSwapchainCounterEXT(_GetProcAddress('vkGetSwapchainCounterEXT'));
        vkGetPhysicalDeviceSurfaceCapabilities2EXT := TvkGetPhysicalDeviceSurfaceCapabilities2EXT(_GetProcAddress('vkGetPhysicalDeviceSurfaceCapabilities2EXT'));
        vkEnumeratePhysicalDeviceGroupsKHX := TvkEnumeratePhysicalDeviceGroupsKHX(_GetProcAddress('vkEnumeratePhysicalDeviceGroupsKHX'));
        vkGetDeviceGroupPeerMemoryFeaturesKHX := TvkGetDeviceGroupPeerMemoryFeaturesKHX(_GetProcAddress('vkGetDeviceGroupPeerMemoryFeaturesKHX'));
        vkBindBufferMemory2KHX := TvkBindBufferMemory2KHX(_GetProcAddress('vkBindBufferMemory2KHX'));
        vkBindImageMemory2KHX := TvkBindImageMemory2KHX(_GetProcAddress('vkBindImageMemory2KHX'));
        vkCmdSetDeviceMaskKHX := TvkCmdSetDeviceMaskKHX(_GetProcAddress('vkCmdSetDeviceMaskKHX'));
        vkGetDeviceGroupPresentCapabilitiesKHX := TvkGetDeviceGroupPresentCapabilitiesKHX(_GetProcAddress('vkGetDeviceGroupPresentCapabilitiesKHX'));
        vkGetDeviceGroupSurfacePresentModesKHX := TvkGetDeviceGroupSurfacePresentModesKHX(_GetProcAddress('vkGetDeviceGroupSurfacePresentModesKHX'));
        vkAcquireNextImage2KHX := TvkAcquireNextImage2KHX(_GetProcAddress('vkAcquireNextImage2KHX'));
        vkCmdDispatchBaseKHX := TvkCmdDispatchBaseKHX(_GetProcAddress('vkCmdDispatchBaseKHX'));
        vkGetPhysicalDevicePresentRectanglesKHX := TvkGetPhysicalDevicePresentRectanglesKHX(_GetProcAddress('vkGetPhysicalDevicePresentRectanglesKHX'));
        vkCreateDescriptorUpdateTemplateKHR := TvkCreateDescriptorUpdateTemplateKHR(_GetProcAddress('vkCreateDescriptorUpdateTemplateKHR'));
        vkDestroyDescriptorUpdateTemplateKHR := TvkDestroyDescriptorUpdateTemplateKHR(_GetProcAddress('vkDestroyDescriptorUpdateTemplateKHR'));
        vkUpdateDescriptorSetWithTemplateKHR := TvkUpdateDescriptorSetWithTemplateKHR(_GetProcAddress('vkUpdateDescriptorSetWithTemplateKHR'));
        vkCmdPushDescriptorSetWithTemplateKHR := TvkCmdPushDescriptorSetWithTemplateKHR(_GetProcAddress('vkCmdPushDescriptorSetWithTemplateKHR'));
        vkSetHdrMetadataEXT := TvkSetHdrMetadataEXT(_GetProcAddress('vkSetHdrMetadataEXT'));
        vkGetSwapchainStatusKHR := TvkGetSwapchainStatusKHR(_GetProcAddress('vkGetSwapchainStatusKHR'));
        vkGetRefreshCycleDurationGOOGLE := TvkGetRefreshCycleDurationGOOGLE(_GetProcAddress('vkGetRefreshCycleDurationGOOGLE'));
        vkGetPastPresentationTimingGOOGLE := TvkGetPastPresentationTimingGOOGLE(_GetProcAddress('vkGetPastPresentationTimingGOOGLE'));
       {$ifdef MoltenVK_IOS}
        vkCreateIOSSurfaceMVK := TvkCreateIOSSurfaceMVK(_GetProcAddress('vkCreateIOSSurfaceMVK'));
       {$endif}
       {$ifdef MoltenVK_MacOS}
        vkCreateMacOSSurfaceMVK := TvkCreateMacOSSurfaceMVK(_GetProcAddress('vkCreateMacOSSurfaceMVK'));
       {$endif}
        vkCmdSetViewportWScalingNV := TvkCmdSetViewportWScalingNV(_GetProcAddress('vkCmdSetViewportWScalingNV'));
        vkCmdSetDiscardRectangleEXT := TvkCmdSetDiscardRectangleEXT(_GetProcAddress('vkCmdSetDiscardRectangleEXT'));
        vkGetPhysicalDeviceSurfaceCapabilities2KHR := TvkGetPhysicalDeviceSurfaceCapabilities2KHR(_GetProcAddress('vkGetPhysicalDeviceSurfaceCapabilities2KHR'));
        vkGetPhysicalDeviceSurfaceFormats2KHR := TvkGetPhysicalDeviceSurfaceFormats2KHR(_GetProcAddress('vkGetPhysicalDeviceSurfaceFormats2KHR'));
        vkGetBufferMemoryRequirements2KHR := TvkGetBufferMemoryRequirements2KHR(_GetProcAddress('vkGetBufferMemoryRequirements2KHR'));
        vkGetImageMemoryRequirements2KHR := TvkGetImageMemoryRequirements2KHR(_GetProcAddress('vkGetImageMemoryRequirements2KHR'));
        vkGetImageSparseMemoryRequirements2KHR := TvkGetImageSparseMemoryRequirements2KHR(_GetProcAddress('vkGetImageSparseMemoryRequirements2KHR'));
    end;
  except
    FreeAndNil(result);
    raise;
  end;
end;


function vkoLoadDeviceFunctions(const aInstance: TVkInstance; const aDevice: TVkDevice): TVkDeviceFunctions;
var
  xxGetDeviceProcAddr: TvkGetDeviceProcAddr;

  function _GetProcAddress(const aName: String): TPFN_vkVoidFunction;
  begin
    result := xxGetDeviceProcAddr(aDevice, PVkChar(aName));
  end;

begin
  Pointer(xxGetDeviceProcAddr) := TvkGetDeviceProcAddr(vkGetInstanceProcAddr(aInstance, 'vkGetDeviceProcAddr'));
  result := TVkDeviceFunctions.Create;
  try
    result.aaDevice := aDevice;
    with result do begin
    //............................ Total Device Functions : 182
        vkCreateInstance := TvkCreateInstance(_GetProcAddress('vkCreateInstance'));
        vkDestroyInstance := TvkDestroyInstance(_GetProcAddress('vkDestroyInstance'));
        vkEnumeratePhysicalDevices := TvkEnumeratePhysicalDevices(_GetProcAddress('vkEnumeratePhysicalDevices'));
        vkGetDeviceProcAddr := TvkGetDeviceProcAddr(_GetProcAddress('vkGetDeviceProcAddr'));
        vkGetInstanceProcAddr := TvkGetInstanceProcAddr(_GetProcAddress('vkGetInstanceProcAddr'));
        vkGetPhysicalDeviceProperties := TvkGetPhysicalDeviceProperties(_GetProcAddress('vkGetPhysicalDeviceProperties'));
        vkGetPhysicalDeviceQueueFamilyProperties := TvkGetPhysicalDeviceQueueFamilyProperties(_GetProcAddress('vkGetPhysicalDeviceQueueFamilyProperties'));
        vkGetPhysicalDeviceMemoryProperties := TvkGetPhysicalDeviceMemoryProperties(_GetProcAddress('vkGetPhysicalDeviceMemoryProperties'));
        vkGetPhysicalDeviceFeatures := TvkGetPhysicalDeviceFeatures(_GetProcAddress('vkGetPhysicalDeviceFeatures'));
        vkGetPhysicalDeviceFormatProperties := TvkGetPhysicalDeviceFormatProperties(_GetProcAddress('vkGetPhysicalDeviceFormatProperties'));
        vkGetPhysicalDeviceImageFormatProperties := TvkGetPhysicalDeviceImageFormatProperties(_GetProcAddress('vkGetPhysicalDeviceImageFormatProperties'));
        vkCreateDevice := TvkCreateDevice(_GetProcAddress('vkCreateDevice'));
        vkDestroyDevice := TvkDestroyDevice(_GetProcAddress('vkDestroyDevice'));
        vkEnumerateInstanceLayerProperties := TvkEnumerateInstanceLayerProperties(_GetProcAddress('vkEnumerateInstanceLayerProperties'));
        vkEnumerateInstanceExtensionProperties := TvkEnumerateInstanceExtensionProperties(_GetProcAddress('vkEnumerateInstanceExtensionProperties'));
        vkEnumerateDeviceLayerProperties := TvkEnumerateDeviceLayerProperties(_GetProcAddress('vkEnumerateDeviceLayerProperties'));
        vkEnumerateDeviceExtensionProperties := TvkEnumerateDeviceExtensionProperties(_GetProcAddress('vkEnumerateDeviceExtensionProperties'));
        vkGetDeviceQueue := TvkGetDeviceQueue(_GetProcAddress('vkGetDeviceQueue'));
        vkQueueSubmit := TvkQueueSubmit(_GetProcAddress('vkQueueSubmit'));
        vkQueueWaitIdle := TvkQueueWaitIdle(_GetProcAddress('vkQueueWaitIdle'));
        vkDeviceWaitIdle := TvkDeviceWaitIdle(_GetProcAddress('vkDeviceWaitIdle'));
        vkAllocateMemory := TvkAllocateMemory(_GetProcAddress('vkAllocateMemory'));
        vkFreeMemory := TvkFreeMemory(_GetProcAddress('vkFreeMemory'));
        vkMapMemory := TvkMapMemory(_GetProcAddress('vkMapMemory'));
        vkUnmapMemory := TvkUnmapMemory(_GetProcAddress('vkUnmapMemory'));
        vkFlushMappedMemoryRanges := TvkFlushMappedMemoryRanges(_GetProcAddress('vkFlushMappedMemoryRanges'));
        vkInvalidateMappedMemoryRanges := TvkInvalidateMappedMemoryRanges(_GetProcAddress('vkInvalidateMappedMemoryRanges'));
        vkGetDeviceMemoryCommitment := TvkGetDeviceMemoryCommitment(_GetProcAddress('vkGetDeviceMemoryCommitment'));
        vkGetBufferMemoryRequirements := TvkGetBufferMemoryRequirements(_GetProcAddress('vkGetBufferMemoryRequirements'));
        vkBindBufferMemory := TvkBindBufferMemory(_GetProcAddress('vkBindBufferMemory'));
        vkGetImageMemoryRequirements := TvkGetImageMemoryRequirements(_GetProcAddress('vkGetImageMemoryRequirements'));
        vkBindImageMemory := TvkBindImageMemory(_GetProcAddress('vkBindImageMemory'));
        vkGetImageSparseMemoryRequirements := TvkGetImageSparseMemoryRequirements(_GetProcAddress('vkGetImageSparseMemoryRequirements'));
        vkGetPhysicalDeviceSparseImageFormatProperties := TvkGetPhysicalDeviceSparseImageFormatProperties(_GetProcAddress('vkGetPhysicalDeviceSparseImageFormatProperties'));
        vkQueueBindSparse := TvkQueueBindSparse(_GetProcAddress('vkQueueBindSparse'));
        vkCreateFence := TvkCreateFence(_GetProcAddress('vkCreateFence'));
        vkDestroyFence := TvkDestroyFence(_GetProcAddress('vkDestroyFence'));
        vkResetFences := TvkResetFences(_GetProcAddress('vkResetFences'));
        vkGetFenceStatus := TvkGetFenceStatus(_GetProcAddress('vkGetFenceStatus'));
        vkWaitForFences := TvkWaitForFences(_GetProcAddress('vkWaitForFences'));
        vkCreateSemaphore := TvkCreateSemaphore(_GetProcAddress('vkCreateSemaphore'));
        vkDestroySemaphore := TvkDestroySemaphore(_GetProcAddress('vkDestroySemaphore'));
        vkCreateEvent := TvkCreateEvent(_GetProcAddress('vkCreateEvent'));
        vkDestroyEvent := TvkDestroyEvent(_GetProcAddress('vkDestroyEvent'));
        vkGetEventStatus := TvkGetEventStatus(_GetProcAddress('vkGetEventStatus'));
        vkSetEvent := TvkSetEvent(_GetProcAddress('vkSetEvent'));
        vkResetEvent := TvkResetEvent(_GetProcAddress('vkResetEvent'));
        vkCreateQueryPool := TvkCreateQueryPool(_GetProcAddress('vkCreateQueryPool'));
        vkDestroyQueryPool := TvkDestroyQueryPool(_GetProcAddress('vkDestroyQueryPool'));
        vkGetQueryPoolResults := TvkGetQueryPoolResults(_GetProcAddress('vkGetQueryPoolResults'));
        vkCreateBuffer := TvkCreateBuffer(_GetProcAddress('vkCreateBuffer'));
        vkDestroyBuffer := TvkDestroyBuffer(_GetProcAddress('vkDestroyBuffer'));
        vkCreateBufferView := TvkCreateBufferView(_GetProcAddress('vkCreateBufferView'));
        vkDestroyBufferView := TvkDestroyBufferView(_GetProcAddress('vkDestroyBufferView'));
        vkCreateImage := TvkCreateImage(_GetProcAddress('vkCreateImage'));
        vkDestroyImage := TvkDestroyImage(_GetProcAddress('vkDestroyImage'));
        vkGetImageSubresourceLayout := TvkGetImageSubresourceLayout(_GetProcAddress('vkGetImageSubresourceLayout'));
        vkCreateImageView := TvkCreateImageView(_GetProcAddress('vkCreateImageView'));
        vkDestroyImageView := TvkDestroyImageView(_GetProcAddress('vkDestroyImageView'));
        vkCreateShaderModule := TvkCreateShaderModule(_GetProcAddress('vkCreateShaderModule'));
        vkDestroyShaderModule := TvkDestroyShaderModule(_GetProcAddress('vkDestroyShaderModule'));
        vkCreatePipelineCache := TvkCreatePipelineCache(_GetProcAddress('vkCreatePipelineCache'));
        vkDestroyPipelineCache := TvkDestroyPipelineCache(_GetProcAddress('vkDestroyPipelineCache'));
        vkGetPipelineCacheData := TvkGetPipelineCacheData(_GetProcAddress('vkGetPipelineCacheData'));
        vkMergePipelineCaches := TvkMergePipelineCaches(_GetProcAddress('vkMergePipelineCaches'));
        vkCreateGraphicsPipelines := TvkCreateGraphicsPipelines(_GetProcAddress('vkCreateGraphicsPipelines'));
        vkCreateComputePipelines := TvkCreateComputePipelines(_GetProcAddress('vkCreateComputePipelines'));
        vkDestroyPipeline := TvkDestroyPipeline(_GetProcAddress('vkDestroyPipeline'));
        vkCreatePipelineLayout := TvkCreatePipelineLayout(_GetProcAddress('vkCreatePipelineLayout'));
        vkDestroyPipelineLayout := TvkDestroyPipelineLayout(_GetProcAddress('vkDestroyPipelineLayout'));
        vkCreateSampler := TvkCreateSampler(_GetProcAddress('vkCreateSampler'));
        vkDestroySampler := TvkDestroySampler(_GetProcAddress('vkDestroySampler'));
        vkCreateDescriptorSetLayout := TvkCreateDescriptorSetLayout(_GetProcAddress('vkCreateDescriptorSetLayout'));
        vkDestroyDescriptorSetLayout := TvkDestroyDescriptorSetLayout(_GetProcAddress('vkDestroyDescriptorSetLayout'));
        vkCreateDescriptorPool := TvkCreateDescriptorPool(_GetProcAddress('vkCreateDescriptorPool'));
        vkDestroyDescriptorPool := TvkDestroyDescriptorPool(_GetProcAddress('vkDestroyDescriptorPool'));
        vkResetDescriptorPool := TvkResetDescriptorPool(_GetProcAddress('vkResetDescriptorPool'));
        vkAllocateDescriptorSets := TvkAllocateDescriptorSets(_GetProcAddress('vkAllocateDescriptorSets'));
        vkFreeDescriptorSets := TvkFreeDescriptorSets(_GetProcAddress('vkFreeDescriptorSets'));
        vkUpdateDescriptorSets := TvkUpdateDescriptorSets(_GetProcAddress('vkUpdateDescriptorSets'));
        vkCreateFramebuffer := TvkCreateFramebuffer(_GetProcAddress('vkCreateFramebuffer'));
        vkDestroyFramebuffer := TvkDestroyFramebuffer(_GetProcAddress('vkDestroyFramebuffer'));
        vkCreateRenderPass := TvkCreateRenderPass(_GetProcAddress('vkCreateRenderPass'));
        vkDestroyRenderPass := TvkDestroyRenderPass(_GetProcAddress('vkDestroyRenderPass'));
        vkGetRenderAreaGranularity := TvkGetRenderAreaGranularity(_GetProcAddress('vkGetRenderAreaGranularity'));
        vkCreateCommandPool := TvkCreateCommandPool(_GetProcAddress('vkCreateCommandPool'));
        vkDestroyCommandPool := TvkDestroyCommandPool(_GetProcAddress('vkDestroyCommandPool'));
        vkResetCommandPool := TvkResetCommandPool(_GetProcAddress('vkResetCommandPool'));
        vkAllocateCommandBuffers := TvkAllocateCommandBuffers(_GetProcAddress('vkAllocateCommandBuffers'));
        vkFreeCommandBuffers := TvkFreeCommandBuffers(_GetProcAddress('vkFreeCommandBuffers'));
        vkBeginCommandBuffer := TvkBeginCommandBuffer(_GetProcAddress('vkBeginCommandBuffer'));
        vkEndCommandBuffer := TvkEndCommandBuffer(_GetProcAddress('vkEndCommandBuffer'));
        vkResetCommandBuffer := TvkResetCommandBuffer(_GetProcAddress('vkResetCommandBuffer'));
        vkCmdBindPipeline := TvkCmdBindPipeline(_GetProcAddress('vkCmdBindPipeline'));
        vkCmdSetViewport := TvkCmdSetViewport(_GetProcAddress('vkCmdSetViewport'));
        vkCmdSetScissor := TvkCmdSetScissor(_GetProcAddress('vkCmdSetScissor'));
        vkCmdSetLineWidth := TvkCmdSetLineWidth(_GetProcAddress('vkCmdSetLineWidth'));
        vkCmdSetDepthBias := TvkCmdSetDepthBias(_GetProcAddress('vkCmdSetDepthBias'));
        vkCmdSetBlendConstants := TvkCmdSetBlendConstants(_GetProcAddress('vkCmdSetBlendConstants'));
        vkCmdSetDepthBounds := TvkCmdSetDepthBounds(_GetProcAddress('vkCmdSetDepthBounds'));
        vkCmdSetStencilCompareMask := TvkCmdSetStencilCompareMask(_GetProcAddress('vkCmdSetStencilCompareMask'));
        vkCmdSetStencilWriteMask := TvkCmdSetStencilWriteMask(_GetProcAddress('vkCmdSetStencilWriteMask'));
        vkCmdSetStencilReference := TvkCmdSetStencilReference(_GetProcAddress('vkCmdSetStencilReference'));
        vkCmdBindDescriptorSets := TvkCmdBindDescriptorSets(_GetProcAddress('vkCmdBindDescriptorSets'));
        vkCmdBindIndexBuffer := TvkCmdBindIndexBuffer(_GetProcAddress('vkCmdBindIndexBuffer'));
        vkCmdBindVertexBuffers := TvkCmdBindVertexBuffers(_GetProcAddress('vkCmdBindVertexBuffers'));
        vkCmdDraw := TvkCmdDraw(_GetProcAddress('vkCmdDraw'));
        vkCmdDrawIndexed := TvkCmdDrawIndexed(_GetProcAddress('vkCmdDrawIndexed'));
        vkCmdDrawIndirect := TvkCmdDrawIndirect(_GetProcAddress('vkCmdDrawIndirect'));
        vkCmdDrawIndexedIndirect := TvkCmdDrawIndexedIndirect(_GetProcAddress('vkCmdDrawIndexedIndirect'));
        vkCmdDispatch := TvkCmdDispatch(_GetProcAddress('vkCmdDispatch'));
        vkCmdDispatchIndirect := TvkCmdDispatchIndirect(_GetProcAddress('vkCmdDispatchIndirect'));
        vkCmdCopyBuffer := TvkCmdCopyBuffer(_GetProcAddress('vkCmdCopyBuffer'));
        vkCmdCopyImage := TvkCmdCopyImage(_GetProcAddress('vkCmdCopyImage'));
        vkCmdBlitImage := TvkCmdBlitImage(_GetProcAddress('vkCmdBlitImage'));
        vkCmdCopyBufferToImage := TvkCmdCopyBufferToImage(_GetProcAddress('vkCmdCopyBufferToImage'));
        vkCmdCopyImageToBuffer := TvkCmdCopyImageToBuffer(_GetProcAddress('vkCmdCopyImageToBuffer'));
        vkCmdUpdateBuffer := TvkCmdUpdateBuffer(_GetProcAddress('vkCmdUpdateBuffer'));
        vkCmdFillBuffer := TvkCmdFillBuffer(_GetProcAddress('vkCmdFillBuffer'));
        vkCmdClearColorImage := TvkCmdClearColorImage(_GetProcAddress('vkCmdClearColorImage'));
        vkCmdClearDepthStencilImage := TvkCmdClearDepthStencilImage(_GetProcAddress('vkCmdClearDepthStencilImage'));
        vkCmdClearAttachments := TvkCmdClearAttachments(_GetProcAddress('vkCmdClearAttachments'));
        vkCmdResolveImage := TvkCmdResolveImage(_GetProcAddress('vkCmdResolveImage'));
        vkCmdSetEvent := TvkCmdSetEvent(_GetProcAddress('vkCmdSetEvent'));
        vkCmdResetEvent := TvkCmdResetEvent(_GetProcAddress('vkCmdResetEvent'));
        vkCmdWaitEvents := TvkCmdWaitEvents(_GetProcAddress('vkCmdWaitEvents'));
        vkCmdPipelineBarrier := TvkCmdPipelineBarrier(_GetProcAddress('vkCmdPipelineBarrier'));
        vkCmdBeginQuery := TvkCmdBeginQuery(_GetProcAddress('vkCmdBeginQuery'));
        vkCmdEndQuery := TvkCmdEndQuery(_GetProcAddress('vkCmdEndQuery'));
        vkCmdResetQueryPool := TvkCmdResetQueryPool(_GetProcAddress('vkCmdResetQueryPool'));
        vkCmdWriteTimestamp := TvkCmdWriteTimestamp(_GetProcAddress('vkCmdWriteTimestamp'));
        vkCmdCopyQueryPoolResults := TvkCmdCopyQueryPoolResults(_GetProcAddress('vkCmdCopyQueryPoolResults'));
        vkCmdPushConstants := TvkCmdPushConstants(_GetProcAddress('vkCmdPushConstants'));
        vkCmdBeginRenderPass := TvkCmdBeginRenderPass(_GetProcAddress('vkCmdBeginRenderPass'));
        vkCmdNextSubpass := TvkCmdNextSubpass(_GetProcAddress('vkCmdNextSubpass'));
        vkCmdEndRenderPass := TvkCmdEndRenderPass(_GetProcAddress('vkCmdEndRenderPass'));
        vkCmdExecuteCommands := TvkCmdExecuteCommands(_GetProcAddress('vkCmdExecuteCommands'));
       {$ifdef Android}
        vkCreateAndroidSurfaceKHR := TvkCreateAndroidSurfaceKHR(_GetProcAddress('vkCreateAndroidSurfaceKHR'));
       {$endif}
        vkGetPhysicalDeviceDisplayPropertiesKHR := TvkGetPhysicalDeviceDisplayPropertiesKHR(_GetProcAddress('vkGetPhysicalDeviceDisplayPropertiesKHR'));
        vkGetPhysicalDeviceDisplayPlanePropertiesKHR := TvkGetPhysicalDeviceDisplayPlanePropertiesKHR(_GetProcAddress('vkGetPhysicalDeviceDisplayPlanePropertiesKHR'));
        vkGetDisplayPlaneSupportedDisplaysKHR := TvkGetDisplayPlaneSupportedDisplaysKHR(_GetProcAddress('vkGetDisplayPlaneSupportedDisplaysKHR'));
        vkGetDisplayModePropertiesKHR := TvkGetDisplayModePropertiesKHR(_GetProcAddress('vkGetDisplayModePropertiesKHR'));
        vkCreateDisplayModeKHR := TvkCreateDisplayModeKHR(_GetProcAddress('vkCreateDisplayModeKHR'));
        vkGetDisplayPlaneCapabilitiesKHR := TvkGetDisplayPlaneCapabilitiesKHR(_GetProcAddress('vkGetDisplayPlaneCapabilitiesKHR'));
        vkCreateDisplayPlaneSurfaceKHR := TvkCreateDisplayPlaneSurfaceKHR(_GetProcAddress('vkCreateDisplayPlaneSurfaceKHR'));
        vkCreateSharedSwapchainsKHR := TvkCreateSharedSwapchainsKHR(_GetProcAddress('vkCreateSharedSwapchainsKHR'));
       {$ifdef Mir}
        vkCreateMirSurfaceKHR := TvkCreateMirSurfaceKHR(_GetProcAddress('vkCreateMirSurfaceKHR'));
       {$endif}
       {$ifdef Mir}
        vkGetPhysicalDeviceMirPresentationSupportKHR := TvkGetPhysicalDeviceMirPresentationSupportKHR(_GetProcAddress('vkGetPhysicalDeviceMirPresentationSupportKHR'));
       {$endif}
        vkDestroySurfaceKHR := TvkDestroySurfaceKHR(_GetProcAddress('vkDestroySurfaceKHR'));
        vkGetPhysicalDeviceSurfaceSupportKHR := TvkGetPhysicalDeviceSurfaceSupportKHR(_GetProcAddress('vkGetPhysicalDeviceSurfaceSupportKHR'));
        vkGetPhysicalDeviceSurfaceCapabilitiesKHR := TvkGetPhysicalDeviceSurfaceCapabilitiesKHR(_GetProcAddress('vkGetPhysicalDeviceSurfaceCapabilitiesKHR'));
        vkGetPhysicalDeviceSurfaceFormatsKHR := TvkGetPhysicalDeviceSurfaceFormatsKHR(_GetProcAddress('vkGetPhysicalDeviceSurfaceFormatsKHR'));
        vkGetPhysicalDeviceSurfacePresentModesKHR := TvkGetPhysicalDeviceSurfacePresentModesKHR(_GetProcAddress('vkGetPhysicalDeviceSurfacePresentModesKHR'));
        vkCreateSwapchainKHR := TvkCreateSwapchainKHR(_GetProcAddress('vkCreateSwapchainKHR'));
        vkDestroySwapchainKHR := TvkDestroySwapchainKHR(_GetProcAddress('vkDestroySwapchainKHR'));
        vkGetSwapchainImagesKHR := TvkGetSwapchainImagesKHR(_GetProcAddress('vkGetSwapchainImagesKHR'));
        vkAcquireNextImageKHR := TvkAcquireNextImageKHR(_GetProcAddress('vkAcquireNextImageKHR'));
        vkQueuePresentKHR := TvkQueuePresentKHR(_GetProcAddress('vkQueuePresentKHR'));
        vkCreateViSurfaceNN := TvkCreateViSurfaceNN(_GetProcAddress('vkCreateViSurfaceNN'));
       {$ifdef Wayland}
        vkCreateWaylandSurfaceKHR := TvkCreateWaylandSurfaceKHR(_GetProcAddress('vkCreateWaylandSurfaceKHR'));
       {$endif}
       {$ifdef Wayland}
        vkGetPhysicalDeviceWaylandPresentationSupportKHR := TvkGetPhysicalDeviceWaylandPresentationSupportKHR(_GetProcAddress('vkGetPhysicalDeviceWaylandPresentationSupportKHR'));
       {$endif}
       {$ifdef Windows}
        vkCreateWin32SurfaceKHR := TvkCreateWin32SurfaceKHR(_GetProcAddress('vkCreateWin32SurfaceKHR'));
       {$endif}
       {$ifdef Windows}
        vkGetPhysicalDeviceWin32PresentationSupportKHR := TvkGetPhysicalDeviceWin32PresentationSupportKHR(_GetProcAddress('vkGetPhysicalDeviceWin32PresentationSupportKHR'));
       {$endif}
       {$ifdef XLIB}
        vkCreateXlibSurfaceKHR := TvkCreateXlibSurfaceKHR(_GetProcAddress('vkCreateXlibSurfaceKHR'));
       {$endif}
       {$ifdef XLIB}
        vkGetPhysicalDeviceXlibPresentationSupportKHR := TvkGetPhysicalDeviceXlibPresentationSupportKHR(_GetProcAddress('vkGetPhysicalDeviceXlibPresentationSupportKHR'));
       {$endif}
       {$ifdef XCB}
        vkCreateXcbSurfaceKHR := TvkCreateXcbSurfaceKHR(_GetProcAddress('vkCreateXcbSurfaceKHR'));
       {$endif}
       {$ifdef XCB}
        vkGetPhysicalDeviceXcbPresentationSupportKHR := TvkGetPhysicalDeviceXcbPresentationSupportKHR(_GetProcAddress('vkGetPhysicalDeviceXcbPresentationSupportKHR'));
       {$endif}
        vkCreateDebugReportCallbackEXT := TvkCreateDebugReportCallbackEXT(_GetProcAddress('vkCreateDebugReportCallbackEXT'));
        vkDestroyDebugReportCallbackEXT := TvkDestroyDebugReportCallbackEXT(_GetProcAddress('vkDestroyDebugReportCallbackEXT'));
        vkDebugReportMessageEXT := TvkDebugReportMessageEXT(_GetProcAddress('vkDebugReportMessageEXT'));
        vkDebugMarkerSetObjectNameEXT := TvkDebugMarkerSetObjectNameEXT(_GetProcAddress('vkDebugMarkerSetObjectNameEXT'));
        vkDebugMarkerSetObjectTagEXT := TvkDebugMarkerSetObjectTagEXT(_GetProcAddress('vkDebugMarkerSetObjectTagEXT'));
        vkCmdDebugMarkerBeginEXT := TvkCmdDebugMarkerBeginEXT(_GetProcAddress('vkCmdDebugMarkerBeginEXT'));
        vkCmdDebugMarkerEndEXT := TvkCmdDebugMarkerEndEXT(_GetProcAddress('vkCmdDebugMarkerEndEXT'));
        vkCmdDebugMarkerInsertEXT := TvkCmdDebugMarkerInsertEXT(_GetProcAddress('vkCmdDebugMarkerInsertEXT'));
        vkGetPhysicalDeviceExternalImageFormatPropertiesNV := TvkGetPhysicalDeviceExternalImageFormatPropertiesNV(_GetProcAddress('vkGetPhysicalDeviceExternalImageFormatPropertiesNV'));
       {$ifdef Windows}
        vkGetMemoryWin32HandleNV := TvkGetMemoryWin32HandleNV(_GetProcAddress('vkGetMemoryWin32HandleNV'));
       {$endif}
        vkCmdDrawIndirectCountAMD := TvkCmdDrawIndirectCountAMD(_GetProcAddress('vkCmdDrawIndirectCountAMD'));
        vkCmdDrawIndexedIndirectCountAMD := TvkCmdDrawIndexedIndirectCountAMD(_GetProcAddress('vkCmdDrawIndexedIndirectCountAMD'));
        vkCmdProcessCommandsNVX := TvkCmdProcessCommandsNVX(_GetProcAddress('vkCmdProcessCommandsNVX'));
        vkCmdReserveSpaceForCommandsNVX := TvkCmdReserveSpaceForCommandsNVX(_GetProcAddress('vkCmdReserveSpaceForCommandsNVX'));
        vkCreateIndirectCommandsLayoutNVX := TvkCreateIndirectCommandsLayoutNVX(_GetProcAddress('vkCreateIndirectCommandsLayoutNVX'));
    end;
  except
    FreeAndNil(result);
    raise;
  end;
end;

procedure vkAPIFinalize;
begin
  vkFreeLibrary(vkLibHandle);
end;

//============================================================

initialization

finalization
  vkAPIFinalize;

end.
