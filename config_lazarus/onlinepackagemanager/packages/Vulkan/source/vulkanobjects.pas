{**********************************************************************
                PilotLogic Software House.
  
Package pl_Vulkan
this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit vulkanobjects;

{$mode objfpc}{$H+}

{$IFNDEF WINDOWS}
    {$LINKLIB c}
{$ENDIF}
{$MINENUMSIZE 4}
{$PACKSET 4}

{$I vulkanapi.inc}

interface

uses
  Classes, SysUtils,typinfo, controls,
  {$IF DEFINED(VK_USE_PLATFORM_WIN32_KHR)}
   Windows,
  {$ENDIF}
  vulkanapi;

type
 TvkObj_Device=class;
 TvkObj_Instance=class;
 TvkObj_InstanceObjFactory=class;

 TvkObj_Exception=class(Exception);

 TvkObj_Object=class(TObject)
  end;

 TvkObj_Structure=class
  end;

 TvkObj_CommandHelper=class
  end;

 TvkObj_ErrorException=class(TvkObj_Exception)
  private
    fError:TVkResult;
  public
    property Error:TVkResult read fError;
    constructor Create(const msg:string; const aError:TVkResult);
  end;

 TvkObj_InvalidFuncPtrException=class(TvkObj_Exception)
  private
    fExtensionName:String;
    fFunctionName:String;
    fExtentionName:String;
  public
    property FunctionName:String read fFunctionName;
    property ExtensionName:String read fExtensionName;
    constructor Create(const aFunctionName:String; const aExtensionName:String='');
  end;

 TvkObj_Handle=class(TvkObj_Object)
  private
    fOwnsHandle:Boolean;
  public
    property OwnsHandle:Boolean read fOwnsHandle write fOwnsHandle;
    constructor Create;
  end;

 TvkObj_AllocHandle=class(TvkObj_Handle)
  private
    fAllocCallbacks:PVkAllocationCallbacks;
  protected
    procedure SetAllocCallbacks(const aAllocCallbacks:TVkAllocationCallbacks);
    procedure SetAllocCallbacks(const aAllocCallbacks:PVkAllocationCallbacks);
    procedure DisposeAllocCallbacks;     
    property AllocCallbacks:PVkAllocationCallbacks read fAllocCallbacks;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  generic TvkObj_ListBase<T>=class(TObject)
  private type
    TPointerType=^T;
    TArrayType=array of T;
  private
    fItems:TArrayType;
    function GetData:TPointerType;
    function GetItem(const aIndex:Integer):T;
    function GetLength:Integer;
    procedure SetLength(aValue:Integer);
  protected
    procedure InitItem(var aItem:T); virtual;
    procedure FreeItem(var aItem:T); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property Length:Integer read GetLength write SetLength;
    property Count:Integer read GetLength write SetLength;
    property Size:Integer read GetLength write SetLength;
    property PData:TPointerType read GetData;
    property Data:TArrayType read fItems;
    property Items[const aIndex:Integer]:T read GetItem; default;
  end;

  generic TvkObj_ObjList<T>=class(specialize TvkObj_ListBase<T>)
  protected
    procedure InitItem(var aItem:T); override;
    procedure FreeItem(var aItem:T); override;
  end;

  generic TvkObj_List<T>=class(specialize TvkObj_ListBase<T>)
  private
    procedure SetItem(const aIndex:Integer; aValue:T);
  public
    procedure SetData(const aData:TPointerType; const aCount:Integer);
    property Data:TArrayType read fItems write fItems;
    property Items[const aIndex:Integer]:T read GetItem write SetItem; default;
  end;

  PVkCharArr                             =array of PVkChar;
  TVkPhysicalDeviceArr                   =array of TVkPhysicalDevice;
  TVkQueueFamilyPropertiesArr            =array of TVkQueueFamilyProperties;
  TVkSparseImageFormatPropertiesArr      =array of TVkSparseImageFormatProperties;
  TVkSurfaceFormatArr                    =array of TVkSurfaceFormatKHR;
  TVkPresentModeArr                      =array of TVkPresentModeKHR;
  TVkDisplayPropertiesArr                =array of TVkDisplayPropertiesKHR;
  TVkDisplayPlanePropertiesArr           =array of TVkDisplayPlanePropertiesKHR;
  TVkDisplayArr                          =array of TVkDisplayKHR;
  TVkDisplayModePropertiesArr            =array of TVkDisplayModePropertiesKHR;
  TVkImageArr                            =array of TVkImage;
  TVkCommandBufferArr                    =array of TVkCommandBuffer;
  TVkDescriptorSetLayoutBindingArr       =array of TVkDescriptorSetLayoutBinding;
  TVkDescriptorSetLayoutArr              =array of TVkDescriptorSetLayout;
  TVkPushConstantRangeArr                =array of TVkPushConstantRange;
  TVkPipelineShaderStageCreateInfoArr    =array of TVkPipelineShaderStageCreateInfo;
  TVkPipelineVertexInputStateCreateInfoArr =array of TVkPipelineVertexInputStateCreateInfo;
  TVkAttachmentDescriptionArr            =array of TVkAttachmentDescription;
  TVkSubpassDescriptionArr               =array of TVkSubpassDescription;
  TVkSubpassDependencyArr                =array of TVkSubpassDependency;
  TVkAttachmentReferenceArr              =array of TVkAttachmentReference;
  TVkMemoryBarrierArr                    =array of TVkMemoryBarrier;
  TVkBufferMemoryBarrierArr              =array of TVkBufferMemoryBarrier;
  TVkImageMemoryBarrierArr               =array of TVkImageMemoryBarrier;
  TVkViewportArr                         =array of TVkViewport;
  TVkRect2DArr                           =array of TVkRect2D;
  TVkSampleMaskArr                       =array of TVkSampleMask;
  TVkVertexInputBindingDescriptionArr    =array of TVkVertexInputBindingDescription;
  TVkVertexInputAttributeDescriptionArr  =array of TVkVertexInputAttributeDescription;
  TVkPipelineColorBlendAttachmentStateArr=array of TVkPipelineColorBlendAttachmentState;
  TVkDynamicStateArr                     =array of TVkDynamicState;
  TVkDescriptorPoolSizeArr               =array of TVkDescriptorPoolSize;
  TVkDescriptorSetArr                    =array of TVkDescriptorSet;
  TVkDescriptorImageInfoArr              =array of TVkDescriptorImageInfo;
  TVkDescriptorBufferInfoArr             =array of TVkDescriptorBufferInfo;
  TVkClearValueArr                       =array of TVkClearValue;
  TVkResultArr                           =array of TVkResult;

  TvkInt32List                           =specialize TvkObj_List<TVkUInt32>;
  TVkFloatList                           =specialize TvkObj_List<TVkFloat>;
  TVkImageViewList                       =specialize TvkObj_List<TVkImageView>;
  TVkSamplerList                         =specialize TvkObj_List<TVkSampler>;
  TVkDescriptorSetLayoutList             =specialize TvkObj_List<TVkDescriptorSetLayout>;
  TVkSampleMaskList                      =specialize TvkObj_List<TVkSampleMask>;
  TVkDynamicStateList                    =specialize TvkObj_List<TVkDynamicState>;
  TVkBufferViewList                      =specialize TvkObj_List<TVkBufferView>;
  TVkClearValueList                      =specialize TvkObj_List<TVkClearValue>;

type

 TvkObj_AllocationHandler=class(TObject)
  protected
    function  AllocateMemory(const aSize:TVkSize; const aAlignment:TVkSize; const aScope:TVkSystemAllocationScope):PVkVoid; virtual;
    function  ReallocateMemory(const aOriginal:PVkVoid; const aSize:TVkSize; const aAlignment:TVkSize; const aScope:TVkSystemAllocationScope):PVkVoid; virtual;
    procedure FreeMemory(const aMemory:PVkVoid); virtual;
    procedure InternalAllocationNotification(const aSize:TVkSize; const aType:TVkInternalAllocationType; const aScope:TVkSystemAllocationScope);
    procedure InternalFreeNotification(const aSize:TVkSize; const aType:TVkInternalAllocationType; const aScope:TVkSystemAllocationScope);
  public
    function  GetStructure:TVkAllocationCallbacks;
  end;

 TvkObj_DeviceMemory=class(TvkObj_AllocHandle)
  private
    fHandle:TVkDeviceMemory;
    fDeviceFunctions:TVkDeviceFunctions;
    procedure CreateHandle(const aAllocInfo:TVkMemoryAllocateInfo);
  public
    property Handle:TVkDeviceMemory read fHandle;
    property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
    function Map(const aOffset:TVkDeviceSize; aSize:TVkDeviceSize; aFlags:TVkMemoryMapFlags):PVkVoid;
    procedure Unmap;
    constructor Create(const aAllocInfo:TVkMemoryAllocateInfo;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks);
    constructor Create(const aHandle:TVkDeviceMemory;
                       const aOwnsHandle:Boolean;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    destructor Destroy; override;
  end;


 TvkObj_Buffer=class(TvkObj_AllocHandle)
  private
    fHandle:TVkBuffer;
    fDeviceFunctions:TVkDeviceFunctions;
    fMemory:TVkDeviceMemory;
    fBufferMemory:TvkObj_DeviceMemory;
    procedure CreateHandle(const aCreateInfo:TVkBufferCreateInfo);
  public
    procedure BindMemory(const aMemory:TVkDeviceMemory; const aOffset:TVkDeviceSize);
    function GetMemoryRequirements:TVkMemoryRequirements;
    procedure AllocateAndBindBufferMemory(const aMemoryProperties:TVkPhysicalDeviceMemoryProperties;
                                          aAllocCallbacks:PVkAllocationCallbacks=nil);
    constructor Create(const aCreateInfo:TVkBufferCreateInfo;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    constructor Create(const aHandle:TVkBuffer;
                       const aOwnsHandle:Boolean;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    destructor Destroy; override;
    property Handle:TVkBuffer read fHandle;
    property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
    property Memory:TVkDeviceMemory read fMemory;
  end;


 TvkObj_Factory=class(TvkObj_Structure)
  private
    fAllocHandler:TvkObj_AllocationHandler;
    fOwnsAllocCallbacks:Boolean;
    fAllocCallbacks:PVkAllocationCallbacks;
    procedure FreeAllocCallbacks;
    procedure SetAllocCallbacks(aValue:PVkAllocationCallbacks);
    procedure SetAllocHandler(aValue:TvkObj_AllocationHandler);
  public
    destructor Destroy; override;
    property AllocHandler:TvkObj_AllocationHandler read fAllocHandler write SetAllocHandler;
    property AllocCallbacks:PVkAllocationCallbacks read fAllocCallbacks write SetAllocCallbacks;
  end;

   TvkObj_DeviceObjFactory=class(TvkObj_Factory)
    private
      fDeviceFunctions:TVkDeviceFunctions;
    public
      property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
      constructor Create(const aDevice:TvkObj_Device);
      constructor Create(const aDeviceFunctions:TVkDeviceFunctions);
    end;

 TvkObj_BufferFactory=class(TvkObj_DeviceObjFactory)
  private
    fFlags:TVkBufferCreateFlags;
    fSize:TVkDeviceSize;
    fUsage:TVkBufferUsageFlags;
    fSharingMode:TVkSharingMode;
    fQueueFamilyIndices:TvkInt32List;
  public               
    function  CreateBuffer:TvkObj_Buffer;
    function  GetStructure:TVkBufferCreateInfo;
    procedure SetStructure(const aData:TVkBufferCreateInfo);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;  
    property Flags:TVkBufferCreateFlags read fFlags write fFlags;
    property Size:TVkDeviceSize read fSize write fSize;
    property Usage:TVkBufferUsageFlags read fUsage write fUsage;
    property SharingMode:TVkSharingMode read fSharingMode write fSharingMode;
    property QueueFamilyIndices:TvkInt32List read fQueueFamilyIndices;
  end;


 TvkObj_BufferView=class(TvkObj_AllocHandle)
  private
    fHandle:TVkBufferView;
    fDeviceFunctions:TVkDeviceFunctions;
    procedure CreateHandle(const aCreateInfo:TVkBufferViewCreateInfo);
  public
    constructor Create(const aCreateInfo:TVkBufferViewCreateInfo;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    constructor Create(const aHandle:TVkBufferView;
                       const aOwnsHandle:Boolean;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    destructor Destroy; override;
    property Handle:TVkBufferView read fHandle;
    property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
  end;


 TvkObj_BufferViewFactory=class(TvkObj_DeviceObjFactory)
  private
    fFlags:TVkBufferViewCreateFlags;
    fBuffer:TVkBuffer;
    fFormat:TVkFormat;
    fOffset:TVkDeviceSize;
    fRange:TVkDeviceSize;
  public        
    function CreateBufferView:TvkObj_BufferView;
    function  GetStructure:TVkBufferViewCreateInfo;
    procedure SetStructure(const aData:TVkBufferViewCreateInfo);
    property Flags:TVkBufferViewCreateFlags read fFlags write fFlags;
    property Buffer:TVkBuffer read fBuffer write fBuffer;
    property Format:TVkFormat read fFormat write fFormat;
    property Offset:TVkDeviceSize read fOffset write fOffset;
    property Range:TVkDeviceSize read fRange write fRange;
  end;

 TvkObj_RenderPassBeginInfo=class(TvkObj_Structure)
  private
    fClearValues:TVkClearValueList;
  public
    RenderPass:TVkRenderPass;
    Framebuffer:TVkFramebuffer;
    RenderArea:TVkRect2D;
    constructor Create;
    destructor Destroy; override;
    function  GetStructure:TVkRenderPassBeginInfo;
    procedure SetStructure(const aData:TVkRenderPassBeginInfo);
    property ClearValues:TVkClearValueList read fClearValues;
  end;

 TvkObj_BeginRenderPassHelper=class(TvkObj_CommandHelper)
  private
    fRenderPassBeginInfo:TvkObj_RenderPassBeginInfo;
    fDeviceFunctions:TVkDeviceFunctions;
    fCommandBuffer:TVkCommandBuffer;
  public
    constructor Create(const aCommandBuffer:TVkCommandBuffer; const aDeviceFunctions:TVkDeviceFunctions);
    destructor Destroy; override;    
    procedure Execute(const aContents:TVkSubpassContents);
    property RenderPassBeginInfo:TvkObj_RenderPassBeginInfo read fRenderPassBeginInfo;
  end;

 TvkObj_CommandBuffer=class(TvkObj_Handle)
  private
    fHandle:TVkCommandBuffer;
    fCommandPool:TVkCommandPool;
    fDeviceFunctions:TVkDeviceFunctions;
  public    
    constructor Create(const aHandle:TVkCommandBuffer;
                       const aOwnsHandle:Boolean;
                       const aCommandPool:TVkCommandPool;
                       const aDeviceFunctions:TVkDeviceFunctions);
    destructor Destroy; override;
    procedure BeginCommand;
    procedure EndCommand;
    procedure PipelineBarrier(const aSrcStageMask:TVkPipelineStageFlags;
                              const aDstStageMask:TVkPipelineStageFlags;
                              const aDependencyFlags:TVkDependencyFlags;
                              const aMemoryBarriers:array of TVkMemoryBarrier;
                              const aBufferMemoryBarriers:array of TVkBufferMemoryBarrier;
                              const aImageMemoryBarriers:array of TVkImageMemoryBarrier);
    procedure SetViewport(const aFirstViewport:TVkUInt32; const aViewports:array of TVkViewport);
    procedure SetScissors(const aFirstScissor:TVkUInt32; const aScissorss:array of TVkRect2D);
    procedure BindDescriptorSet(const aBindPoint:TVkPipelineBindPoint;
                                const aLayout:TVkPipelineLayout;
                                const aFirstSet:TVkUInt32;
                                const aDescriptorSets:array of TVkDescriptorSet;
                                const aDynamicOffsets:array of TVkUInt32);
    procedure BindPipeline(const aBindPoint:TVkPipelineBindPoint; const aPipeline:TVkPipeline);
    procedure BindVertexBuffers(const aFirstBinding:TVkUInt32; const aBuffers:array of TVkBuffer; const aOffsets:array of TVkDeviceSize);
    procedure BindIndexBuffers(const aBuffer:TVkBuffer; const aOffset:TVkDeviceSize; const aIndexType:TVkIndexType);
    procedure DrawIndexed(const aIndexCount:TVkUInt32;
                          const aInstanceCount:TVkUInt32;
                          const aFirstIndex:TVkUInt32;
                          const aVertexOffset:TVkInt32;
                          const aFirstInstance:TVkUInt32);
    function  BeginRenderPass:TvkObj_BeginRenderPassHelper;
    procedure EndRenderPass;
    property Handle:TVkCommandBuffer read fHandle;
    property CommandPool:TVkCommandPool read fCommandPool;
    property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
  end;

 TvkObj_CommandPool=class(TvkObj_AllocHandle)
  private
    fHandle:TVkCommandPool;
    fDeviceFunctions:TVkDeviceFunctions;
    procedure CreateHandle(const aCreateInfo:TVkCommandPoolCreateInfo);
  public 
    constructor Create(const aCreateInfo:TVkCommandPoolCreateInfo;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    constructor Create(const aHandle:TVkCommandPool;
                       const aOwnsHandle:Boolean;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    destructor Destroy; override;
    function  AllocateCommandBuffer(const aLevel:TVkCommandBufferLevel):TVkCommandBuffer;
    function  AllocateCommandBuffers(const aLevel:TVkCommandBufferLevel; const aCount:Integer):TVkCommandBufferArr;
    procedure FreeCommandBuffer(const aBuffer:TVkCommandBuffer);
    procedure FreeCommandBuffers(const aBuffers:TVkCommandBufferArr);
    property Handle:TVkCommandPool read fHandle;
    property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
  end;

 TvkObj_CommandPoolFactory=class(TvkObj_DeviceObjFactory)
  private
    fFlags:TVkCommandPoolCreateFlags;
    fQueueFamilyIndex:TVkUInt32;
  public
    function  GetStructure:TVkCommandPoolCreateInfo;
    procedure SetStructure(const aData:TVkCommandPoolCreateInfo);
    function CreateCommandPool:TvkObj_CommandPool;
    property Flags:TVkCommandPoolCreateFlags read fFlags write fFlags;
    property QueueFamilyIndex:TVkUInt32 read fQueueFamilyIndex write fQueueFamilyIndex;
  end;

 TvkObj_DebugReporter=class(TvkObj_AllocHandle)
  private
    fHandle:TVkDebugReportCallbackEXT;
    fInstanceFunctions:TVkInstanceFunctions;
  protected
    function DebugMessage(aFlags:TVkDebugReportFlagsEXT;
                          aObjectType:TVkDebugReportObjectTypeEXT;
                          aObject:TVkUInt64;
                          aLocation:TVkSize;
                          aMessageCode:TVkInt32;
                          aLayerPrefix:String;
                          aMessage:String):Boolean; virtual;
  public
    constructor Create(const aInstanceFunctions:TVkInstanceFunctions; const aAllocHandler:TvkObj_AllocationHandler=nil);
    destructor Destroy; override;
    property Handle:TVkDebugReportCallbackEXT read fHandle;
  end;

 TvkObj_ConsoleDebugReporter=class(TvkObj_DebugReporter)
  protected
    function DebugMessage(aFlags:TVkDebugReportFlagsEXT;
                          aObjectType:TVkDebugReportObjectTypeEXT;
                          aObject:TVkUInt64;
                          aLocation:TVkSize;
                          aMessageCode:TVkInt32;
                          aLayerPrefix:String;
                          aMessage:String):Boolean; virtual;
  end;


 TvkObj_DescriptorPool=class(TvkObj_AllocHandle)
  private
    fHandle:TVkDescriptorPool;
    fDeviceFunctions:TVkDeviceFunctions;
    procedure CreateHandle(const aCreateInfo:TVkDescriptorPoolCreateInfo);
  public         
    constructor Create(const aCreateInfo:TVkDescriptorPoolCreateInfo;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    constructor Create(const aHandle:TVkDescriptorPool;
                       const aOwnsHandle:Boolean;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    destructor Destroy; override;
    function  AllocateDescriptorSet(const aDescriptorSetLayout:TVkDescriptorSetLayout):TVkDescriptorSet;
    function  AllocateDescriptorSets(const aDescriptorSetLayouts:array of TVkDescriptorSetLayout):TVkDescriptorSetArr;
    procedure FreeDescriptorSet(const aDescriptorSet:TVkDescriptorSet);
    procedure FreeDescriptorSets(const aDescriptorSets:TVkDescriptorSetArr);
    property Handle:TVkDescriptorPool read fHandle;
    property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
  end;

 TvkObj_DescriptorPoolSize=class(TvkObj_Structure)
    DescriptorType:TVkDescriptorType;
    DescriptorCount:TVkUInt32;
    function  GetStructure:TVkDescriptorPoolSize;
    procedure SetStructure(const aData:TVkDescriptorPoolSize);
  end;
 TvkObj_DescriptorPoolSizeList=specialize TvkObj_ObjList<TvkObj_DescriptorPoolSize>;


 TvkObj_DescriptorPoolFactory=class(TvkObj_DeviceObjFactory)
  private
    fPoolSizes:TvkObj_DescriptorPoolSizeList;
    fvkPoolSizes:TVkDescriptorPoolSizeArr;
  public
    Flags:TVkDescriptorPoolCreateFlags;
    MaxSets:TVkUInt32;
    function  GetStructure:TVkDescriptorPoolCreateInfo;
    procedure SetStructure(const aData:TVkDescriptorPoolCreateInfo);
    function  CreateDescriptorPool:TvkObj_DescriptorPool;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property PoolSizes:TvkObj_DescriptorPoolSizeList read fPoolSizes;
  end;


 TvkObj_DescriptorSet=class(TvkObj_Handle)
  private
    fHandle:TVkDescriptorSet;
    fDescriptorPool:TVkDescriptorPool;
    fDeviceFunctions:TVkDeviceFunctions;
  public
    constructor Create(const aHandle:TVkDescriptorSet;
                       const aOwnsHandle:Boolean;
                       const aDescriptorPool:TVkDescriptorPool;
                       const aDeviceFunctions:TVkDeviceFunctions);
    destructor Destroy; override;
    property Handle:TVkDescriptorSet read fHandle;
    property DescriptorPool:TVkDescriptorPool read fDescriptorPool;
    property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
  end;


 TvkObj_DescriptorSetLayout=class(TvkObj_AllocHandle)
  private
    fHandle:TVkDescriptorSetLayout;
    fDeviceFunctions:TVkDeviceFunctions;
    procedure CreateHandle(const aCreateInfo:TVkDescriptorSetLayoutCreateInfo);
  public
    constructor Create(const aCreateInfo:TVkDescriptorSetLayoutCreateInfo;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    constructor Create(const aHandle:TVkDescriptorSetLayout;
                       const aOwnsHandle:Boolean;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    destructor Destroy; override;
    property Handle:TVkDescriptorSetLayout read fHandle;
    property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
  end;


 TvkObj_DescriptorSetLayoutBinding=class(TvkObj_Structure)
  private
    fImmutableSamplers:TVkSamplerList;
  public
    Binding:TVkUInt32;
    DescriptorType:TVkDescriptorType;
    DescriptorCount:TVkUInt32;
    StageFlags:TVkShaderStageFlags;
    function  GetStructure:TVkDescriptorSetLayoutBinding;
    procedure SetStructure(const aData:TVkDescriptorSetLayoutBinding);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property ImmutableSamplers:TVkSamplerList read fImmutableSamplers;
  end;
 TvkObj_DescriptorSetLayoutBindingArr=specialize TvkObj_ObjList<TvkObj_DescriptorSetLayoutBinding>;


 TvkObj_DescriptorSetLayoutFactory=class(TvkObj_DeviceObjFactory)
  private
    fFlags:TVkDescriptorSetLayoutCreateFlags;
    fBindings:TvkObj_DescriptorSetLayoutBindingArr;
    fvkBindings:TVkDescriptorSetLayoutBindingArr;
  public
    function  GetStructure:TVkDescriptorSetLayoutCreateInfo;
    procedure SetStructure(const aData:TVkDescriptorSetLayoutCreateInfo);
    function CreateDescriptorSetLayout:TvkObj_DescriptorSetLayout;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Flags:TVkDescriptorSetLayoutCreateFlags read fFlags write fFlags;
    property Bindings:TvkObj_DescriptorSetLayoutBindingArr read fBindings;
  end;


 TvkObj_DescriptorImageInfo=class(TvkObj_Structure)
    Sampler:TVkSampler;
    ImageView:TVkImageView;
    ImageLayout:TVkImageLayout;
    function  GetStructure:TVkDescriptorImageInfo;
    procedure SetStructure(const aData:TVkDescriptorImageInfo);
  end;
 TvkObj_DescriptorImageInfoList=specialize TvkObj_ObjList<TvkObj_DescriptorImageInfo>;


 TvkObj_DescriptorBufferInfo=class(TvkObj_Structure)
    Buffer:TVkBuffer;
    Offset:TVkDeviceSize;
    Range:TVkDeviceSize;
    function  GetStructure:TVkDescriptorBufferInfo;
    procedure SetStructure(const aData:TVkDescriptorBufferInfo);
  end;
 TvkObj_DescriptorBufferInfoList=specialize TvkObj_ObjList<TvkObj_DescriptorBufferInfo>;


 TvkObj_WriteDescriptorSet=class(TvkObj_Structure)
  private
    fImageInfos:TvkObj_DescriptorImageInfoList;
    fBufferInfos:TvkObj_DescriptorBufferInfoList;
    fTexelBufferViews:TVkBufferViewList;
    fvkImageInfos:TVkDescriptorImageInfoArr;
    fvkBufferInfos:TVkDescriptorBufferInfoArr;
  public
    DstSet:TVkDescriptorSet;
    DstBinding:TVkUInt32;
    DstArrayElement:TVkUInt32;
    DescriptorType:TVkDescriptorType;
    function  GetStructure:TVkWriteDescriptorSet;
    procedure SetStructure(const aData:TVkWriteDescriptorSet);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property ImageInfos:TvkObj_DescriptorImageInfoList read fImageInfos;
    property BufferInfos:TvkObj_DescriptorBufferInfoList read fBufferInfos;
  end;
 TvkObj_WriteDescriptorSetList=specialize TvkObj_ObjList<TvkObj_WriteDescriptorSet>;


 TvkObj_CopyDescriptorSet=class(TvkObj_Structure)
  public
    SrcSet:TVkDescriptorSet;
    SrcBinding:TVkUInt32;
    SrcArrayElement:TVkUInt32;
    DstSet:TVkDescriptorSet;
    DstBinding:TVkUInt32;
    DstArrayElement:TVkUInt32;
    DescriptorCount:TVkUInt32;
    function  GetStructure:TVkCopyDescriptorSet;
    procedure SetStructure(const aData:TVkCopyDescriptorSet);
  end;
 TvkObj_CopyDescriptorSetList=specialize TvkObj_ObjList<TvkObj_CopyDescriptorSet>;


 TvkObj_UpdateDescriptorSetHelper=class(TvkObj_CommandHelper)
  private
    fDeviceFunctions:TVkDeviceFunctions;
    fWriteDescriptorSets:TvkObj_WriteDescriptorSetList;
    fCopyDescriptorSets:TvkObj_CopyDescriptorSetList;
  public  
    constructor Create(const aDeviceFunctions:TVkDeviceFunctions);
    destructor Destroy; override; 
    procedure Execute;
    property WriteDescriptorSets:TvkObj_WriteDescriptorSetList read fWriteDescriptorSets;
    property CopyDescriptorSets:TvkObj_CopyDescriptorSetList read fCopyDescriptorSets;
  end;

 TvkObj_Device=class(TvkObj_AllocHandle)
  private
    fHandle:TVkDevice;
    fInstanceFunctions:TVkInstanceFunctions;
    fDeviceFunctions:TVkDeviceFunctions;
    procedure CreateHandle(const aPhyDevice:TVkPhysicalDevice; const aCreateInfo:PVkDeviceCreateInfo);
  public  
    constructor Create(const aPhysicalDevice:TVkPhysicalDevice;
                       const aCreateInfo:TVkDeviceCreateInfo;
                       const aInstanceFunctions:TVkInstanceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    constructor Create(const aHandle:TVkDevice;
                       const aOwnsHandle:Boolean;
                       const aInstanceFunctions:TVkInstanceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    destructor Destroy; override;

    function GetQueue(const aQueueFamilyIndex:TVkUInt32; const aQueueIndex:TVkUInt32):TVkQueue;
    function AllocateMemory(const aSize:TVkUInt32;
                            const aMemoryTypeIndex:TVkUInt32;
                            aAllocCallbacks:PVkAllocationCallbacks=nil):TVkDeviceMemory;
    procedure FreeMemory(const aMemory:TVkDeviceMemory; aAllocCallbacks:PVkAllocationCallbacks=nil);
    procedure WaitIdle;
    function UpdateDescriptorSet:TvkObj_UpdateDescriptorSetHelper;
    property Handle:TVkDevice read fHandle;
    property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
  end;

 TvkObj_QueueCreateInfo=class(TvkObj_Structure)
  private
    fQueuePriorities:TVkFloatList;
  public
    Flags:TVkDeviceQueueCreateFlags;
    QueueFamilyIndex:TVkUInt32;

    function  GetStructure:TVkDeviceQueueCreateInfo;
    procedure SetStructure(const aData:TVkDeviceQueueCreateInfo);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;  
    property QueuePriorities:TVkFloatList read fQueuePriorities;
  end;
 TvkObj_QueueCreateInfoList=specialize TvkObj_ObjList<TvkObj_QueueCreateInfo>;


 TvkObj_PhysicalDeviceFeatures=class(TvkObj_Structure)
    RobustBufferAccess:TVkBool32;
    FullDrawIndexUint32:TVkBool32;
    ImageCubeArray:TVkBool32;
    IndependentBlend:TVkBool32;
    GeometryShader:TVkBool32;
    TessellationShader:TVkBool32;
    SampleRateShading:TVkBool32;
    DualSrcBlend:TVkBool32;
    LogicOp:TVkBool32;
    MultiDrawIndirect:TVkBool32;
    DrawIndirectFirstInstance:TVkBool32;
    DepthClamp:TVkBool32;
    DepthBiasClamp:TVkBool32;
    FillModeNonSolid:TVkBool32;
    DepthBounds:TVkBool32;
    WideLines:TVkBool32;
    LargePoints:TVkBool32;
    AlphaToOne:TVkBool32;
    MultiViewport:TVkBool32;
    SamplerAnisotropy:TVkBool32;
    TextureCompressionETC2:TVkBool32;
    TextureCompressionASTC_LDR:TVkBool32;
    TextureCompressionBC:TVkBool32;
    OcclusionQueryPrecise:TVkBool32;
    PipelineStatisticsQuery:TVkBool32;
    VertexPipelineStoresAndAtomics:TVkBool32;
    FragmentStoresAndAtomics:TVkBool32;
    ShaderTessellationAndGeometryPointSize:TVkBool32;
    ShaderImageGatherExtended:TVkBool32;
    ShaderStorageImageExtendedFormats:TVkBool32;
    ShaderStorageImageMultisample:TVkBool32;
    ShaderStorageImageReadWithoutFormat:TVkBool32;
    ShaderStorageImageWriteWithoutFormat:TVkBool32;
    ShaderUniformBufferArrayDynamicIndexing:TVkBool32;
    ShaderSampledImageArrayDynamicIndexing:TVkBool32;
    ShaderStorageBufferArrayDynamicIndexing:TVkBool32;
    ShaderStorageImageArrayDynamicIndexing:TVkBool32;
    ShaderClipDistance:TVkBool32;
    ShaderCullDistance:TVkBool32;
    ShaderFloat64:TVkBool32;
    ShaderInt64:TVkBool32;
    ShaderInt16:TVkBool32;
    ShaderResourceResidency:TVkBool32;
    ShaderResourceMinLod:TVkBool32;
    SparseBinding:TVkBool32;
    SparseResidencyBuffer:TVkBool32;
    SparseResidencyImage2D:TVkBool32;
    SparseResidencyImage3D:TVkBool32;
    SparseResidency2Samples:TVkBool32;
    SparseResidency4Samples:TVkBool32;
    SparseResidency8Samples:TVkBool32;
    SparseResidency16Samples:TVkBool32;
    SparseResidencyAliased:TVkBool32;
    VariableMultisampleRate:TVkBool32;
    InheritedQueries:TVkBool32;
    function  GetStructure:TVkPhysicalDeviceFeatures;
    procedure SetStructure(const aData:TVkPhysicalDeviceFeatures);
  end;


 TvkObj_InstanceObjFactory=class(TvkObj_Factory)
  private
    fInstanceFunctions:TVkInstanceFunctions;
  public
    constructor Create(const aInstance:TvkObj_Instance);
    constructor Create(const aInstanceFunctions:TVkInstanceFunctions); 
    property InstanceFunctions:TVkInstanceFunctions read fInstanceFunctions;
  end;

 TvkObj_DeviceFactory=class(TvkObj_InstanceObjFactory)
  private
    fEnabledFeatures:TvkObj_PhysicalDeviceFeatures;
    fFlags:TVkDeviceCreateFlags;
    fEnabledLayerNames:TStringList;
    fEnabledExtensionNames:TStringList;
    fQueueCreateInfos:TvkObj_QueueCreateInfoList;
    fvkEnabledFeatures:TVkPhysicalDeviceFeatures;
    fvkEnabledLayerNames:array of PVkChar;
    fvkEnabledExtensionNames:array of PVkChar;
    fvkQueueCreateInfos:array of TVkDeviceQueueCreateInfo;
    function GetEnabledLayerNames:TStrings;
    function GetEnabledExtensionNames:TStrings;
  public 
    function CreateDevice(const aPhysicalDevice:TVkPhysicalDevice):TvkObj_Device;
    function  GetStructure:TVkDeviceCreateInfo;
    procedure SetStructure(const aData:TVkDeviceCreateInfo);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;  
    property Flags:TVkDeviceCreateFlags read fFlags write fFlags;
    property EnabledLayerNames:TStrings read GetEnabledLayerNames;
    property EnabledExtensionNames:TStrings read GetEnabledExtensionNames;
    property EnabledFeatures:TvkObj_PhysicalDeviceFeatures read fEnabledFeatures;
    property QueueCreateInfos:TvkObj_QueueCreateInfoList read fQueueCreateInfos;
  end;


 TvkObj_FrameBuffer=class(TvkObj_AllocHandle)
  private
    fHandle:TVkFramebuffer;
    fDeviceFunctions:TVkDeviceFunctions;
    procedure CreateHandle(const aCreateInfo:TVkFramebufferCreateInfo);
  public
    constructor Create(const aCreateInfo:TVkFramebufferCreateInfo;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    constructor Create(const aHandle:TVkFramebuffer;
                       const aOwnsHandle:Boolean;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    destructor Destroy; override; 
    property Handle:TVkFramebuffer read fHandle;
    property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
  end;


 TvkObj_FrameBufferFactory=class(TvkObj_DeviceObjFactory)
  private
    fFlags:TVkFramebufferCreateFlags;
    fRenderPass:TVkRenderPass;
    fWidth:TVkUInt32;
    fHeight:TVkUInt32;
    fLayers:TVkUInt32;
    fAttachments:TVkImageViewList;
  public 
    function CreateFrameBuffer:TvkObj_FrameBuffer;
    function  GetStructure:TVkFramebufferCreateInfo;
    procedure SetStructure(const aData:TVkFramebufferCreateInfo);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Flags:TVkFramebufferCreateFlags read fFlags write fFlags;
    property RenderPass:TVkRenderPass read fRenderPass write fRenderPass;
    property Width:TVkUInt32 read fWidth write fWidth;
    property Height:TVkUInt32 read fHeight write fHeight;
    property Layers:TVkUInt32 read fLayers write fLayers;
    property Attachments:TVkImageViewList read fAttachments;
  end;


 TvkObj_Image=class(TvkObj_AllocHandle)
  private
    fHandle:TVkImage;
    fDeviceFunctions:TVkDeviceFunctions;
    fMemory:TVkDeviceMemory;
    fImageMemory:TvkObj_DeviceMemory;
    procedure CreateHandle(const aCreateInfo:TVkImageCreateInfo);
  public   
    constructor Create(const aCreateInfo:TVkImageCreateInfo;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    constructor Create(const aHandle:TVkImage;
                       const aOwnsHandle:Boolean;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    destructor Destroy; override;
    function GetMemoryBarrier(const aAspectMask:TVkImageAspectFlags;
                              const aOldLayout:TVkImageLayout;
                              const aNewLayout:TVkImageLayout):TVkImageMemoryBarrier;
    procedure BindMemory(const aMemory:TVkDeviceMemory;
                         const aOffset:TVkDeviceSize);
    function GetMemoryRequirements:TVkMemoryRequirements;
    procedure AllocateAndBindImageMemory(const aMemoryProperties:TVkPhysicalDeviceMemoryProperties;
                                         aAllocCallbacks:PVkAllocationCallbacks=nil);
    property Handle:TVkImage read fHandle;
    property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
    property Memory:TVkDeviceMemory read fMemory;
  end;

 TvkObj_ImageFactory=class(TvkObj_DeviceObjFactory)
  private
    fFlags:TVkImageCreateFlags;
    fImageType:TVkImageType;
    fFormat:TVkFormat;
    fExtent:TVkExtent3D;
    fMipLevels:TVkUInt32;
    fArrayLayers:TVkUInt32;
    fSamples:TVkSampleCountFlagBits;
    fTiling:TVkImageTiling;
    fUsage:TVkImageUsageFlags;
    fSharingMode:TVkSharingMode;
    fQueueFamilyIndices:TvkInt32List;
    fInitialLayout:TVkImageLayout;
  public          
    function CreateImage:TvkObj_Image;
    function  GetStructure:TVkImageCreateInfo;
    procedure SetStructure(const aData:TVkImageCreateInfo);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Flags:TVkImageCreateFlags read fFlags write fFlags;
    property ImageType:TVkImageType read fImageType write fImageType;
    property Format:TVkFormat read fFormat write fFormat;
    property Extent:TVkExtent3D read fExtent write fExtent;
    property MipLevels:TVkUInt32 read fMipLevels write fMipLevels;
    property ArrayLayers:TVkUInt32 read fArrayLayers write fArrayLayers;
    property Samples:TVkSampleCountFlagBits read fSamples write fSamples;
    property Tiling:TVkImageTiling read fTiling write fTiling;
    property Usage:TVkImageUsageFlags read fUsage write fUsage;
    property SharingMode:TVkSharingMode read fSharingMode write fSharingMode;
    property InitialLayout:TVkImageLayout read fInitialLayout write fInitialLayout;
    property QueueFamilyIndices:TvkInt32List read fQueueFamilyIndices;
  end;


 TvkObj_ImageView=class(TvkObj_AllocHandle)
  private
    fHandle:TVkImageView;
    fDeviceFunctions:TVkDeviceFunctions;
    procedure CreateHandle(const aCreateInfo:TVkImageViewCreateInfo);
  public
    constructor Create(const aCreateInfo:TVkImageViewCreateInfo;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    constructor Create(const aHandle:TVkImageView;
                       const aOwnsHandle:Boolean;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    destructor Destroy; override;
    property Handle:TVkImageView read fHandle;
    property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
  end;

 TvkObj_ImageSubresourceRange=class(TvkObj_Structure)
    AspectMask:TVkImageAspectFlags;
    BaseMipLevel:TVkUInt32;
    LevelCount:TVkUInt32;
    BaseArrayLayer:TVkUInt32;
    LayerCount:TVkUInt32;
    function  GetStructure:TVkImageSubresourceRange;
    procedure SetStructure(const aData:TVkImageSubresourceRange);
  end;

 TvkObj_ImageViewFactory=class(TvkObj_DeviceObjFactory)
  private
    fFlags:TVkImageViewCreateFlags;
    fImage:TVkImage;
    fViewType:TVkImageViewType;
    fFormat:TVkFormat;
    fComponents:TVkComponentMapping;
    fSubresourceRange:TvkObj_ImageSubresourceRange;
  public
    function  GetStructure:TVkImageViewCreateInfo;
    procedure SetStructure(const aData:TVkImageViewCreateInfo);
    function CreateImageView:TvkObj_ImageView;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Flags:TVkImageViewCreateFlags read fFlags  write fFlags;
    property Image:TVkImage read fImage  write fImage;
    property ViewType:TVkImageViewType read fViewType write fViewType;
    property Format:TVkFormat read fFormat write fFormat;
    property Components:TVkComponentMapping read fComponents write fComponents;
    property SubresourceRange:TvkObj_ImageSubresourceRange read fSubresourceRange;
  end;

 TvkObj_Instance=class(TvkObj_AllocHandle)
  private
    fHandle:TVkInstance;
    fPhysicalDevices:TVkPhysicalDeviceArr;
    fInstanceFunctions:TVkInstanceFunctions;
    procedure UpdatePhysicalDevices;
  protected
    procedure CreateHandle(const aCreateInfo:PVkInstanceCreateInfo);
  public
    constructor Create(const aCreateInfo:TVkInstanceCreateInfo;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    constructor Create(const aHandle:TVkInstance;
                       const aOwnsHandle:Boolean;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    destructor Destroy; override;     
    function GetPhysicalDevices:TVkPhysicalDeviceArr;
    property Handle:TVkInstance read fHandle;
    property InstanceFunctions:TVkInstanceFunctions read fInstanceFunctions;
  end;


 TvkObj_ApplicationInfo=class(TObject)
  public
    AppName:String;
    AppVersion:TVkVersion;
    EngineName:String;
    EngineVersion:TVkVersion;
    ApiVersion:TVkVersion;
    constructor Create;
    function  GetStructure:TVkApplicationInfo;
    procedure SetStructure(const aData:TVkApplicationInfo);
  end;

 TvkObj_InstanceFactory=class(TvkObj_Factory)
  private
    fFlags:TVkInstanceCreateFlags;
    fApplicationInfo:TvkObj_ApplicationInfo;
    fEnabledLayerNames:TStringList;
    fEnabledExtensionNames:TStringList;
    fvkApplicationInfo:TVkApplicationInfo;
    fvkEnabledLayerNames:PVkCharArr;
    fvkEnabledExtensionNames:PVkCharArr;
    function GetLayers:TStrings;
    function GetExtensions:TStrings;
  public    
    constructor Create;
    destructor Destroy; override;
    function  GetStructure:TVkInstanceCreateInfo;
    procedure SetStructure(const aData:TVkInstanceCreateInfo);
    function CreateInstance:TvkObj_Instance; 
    property Flags:TVkInstanceCreateFlags read fFlags;
    property ApplicationInfo:TvkObj_ApplicationInfo read fApplicationInfo;
    property EnabledLayerNames:TStrings read GetLayers;
    property EnabledExtensionNames:TStrings read GetExtensions;
  end;


 TvkObj_PhysicalDevice=class(TvkObj_Object)
  private
    fHandle:TVkPhysicalDevice;
    fInstanceFunctions:TVkInstanceFunctions;
  public
    constructor Create(const aHandle:TVkPhysicalDevice; const aInstanceFunctions:TVkInstanceFunctions);
    function GetFeatures:TVkPhysicalDeviceFeatures;
    function GetProperties:TVkPhysicalDeviceProperties;
    function GetMemoryProperties:TVkPhysicalDeviceMemoryProperties;
    function GetQueueFamilyProperties:TVkQueueFamilyPropertiesArr;
    function GetFormatProperties(const aFormat:TVkFormat):TVkFormatProperties;
    function GetImageFormatProperties(const aFormat:TVkFormat;
                                      const aImageType:TVkImageType;
                                      const aTiling:TVkImageTiling;
                                      const aUsage:TVkImageUsageFlags;
                                      const aFlags:TVkImageCreateFlags):TVkImageFormatProperties;
    function GetSparseImageFormatProperties(const aFormat:TVkFormat;
                                            const aImageType:TVkImageType;
                                            const aSamples:TVkSampleCountFlagBits;
                                            const aUsage:TVkImageUsageFlags;
                                            const aTiling:TVkImageTiling):TVkSparseImageFormatPropertiesArr;
    function GetSurfaceSupport(const aQueueFamilyIndex:TVkUInt32; const aSurface:TVkSurfaceKHR):Boolean;
    function GetSurfaceCapabilities(const aSurface:TVkSurfaceKHR):TVkSurfaceCapabilitiesKHR;
    function GetSurfaceFormats(const aSurface:TVkSurfaceKHR):TVkSurfaceFormatArr;
    function GetSurfacePresentModes(const aSurface:TVkSurfaceKHR):TVkPresentModeArr;
    function GetDisplayProperties:TVkDisplayPropertiesArr;
    function GetDisplayPlaneProperties:TVkDisplayPlanePropertiesArr;
    function GetDisplayPlaneSupportedDisplays(const aPlaneIndex:TVkUInt32):TVkDisplayArr;
    function GetDisplayModeProperties(const aDisplay:TVkDisplayKHR):TVkDisplayModePropertiesArr;
    property Handle:TVkPhysicalDevice read fHandle;
    property InstanceFunctions:TVkInstanceFunctions read fInstanceFunctions;
  end;

 TvkObj_Pipeline=class(TvkObj_AllocHandle)
  private
    fHandle:TVkPipeline;
    fDeviceFunctions:TVkDeviceFunctions;
  public
    constructor Create(const aHandle:TVkPipeline;
                       const aOwnsHandle:Boolean;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    destructor Destroy; override;   
    property Handle:TVkPipeline read fHandle;
    property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
  end;

 TvkObj_GraphicsPipeline=class(TvkObj_Pipeline)
  private
    procedure CreateHandle(const aCreateInfo:TVkGraphicsPipelineCreateInfo; const aPipelineCache:TVkPipelineCache);
  public
    constructor Create(const aCreateInfo:TVkGraphicsPipelineCreateInfo;
                       const aPipelineCache:TVkPipelineCache;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
  end;

 TvkObj_ComputePipeline=class(TvkObj_Pipeline)
  private
    procedure CreateHandle(const aCreateInfo:TVkComputePipelineCreateInfo; const aPipelineCache:TVkPipelineCache);
  public
    constructor Create(const aCreateInfo:TVkComputePipelineCreateInfo;
                       const aPipelineCache:TVkPipelineCache;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
  end;


 TvkObj_PipelineCache=class(TvkObj_AllocHandle)
  private
    fHandle:TVkPipelineCache;
    fDeviceFunctions:TVkDeviceFunctions;
    procedure CreateHandle(const aCreateInfo:TVkPipelineCacheCreateInfo);
  public
    constructor Create(const aCreateInfo:TVkPipelineCacheCreateInfo;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    constructor Create(const aHandle:TVkPipelineCache;
                       const aOwnsHandle:Boolean;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    destructor Destroy; override;   
    property Handle:TVkPipelineCache read fHandle;
    property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
  end;

 TvkObj_PipelineCacheFactory=class(TvkObj_DeviceObjFactory)
  private
    fFlags:TVkPipelineCacheCreateFlags;
    fInitialDataSize:TVkSize;
    fInitialData:PVkVoid;
  public
    procedure SetInitalData(const aData:PVkVoid; const aSize:TVkSize);
    function  GetStructure:TVkPipelineCacheCreateInfo;
    procedure SetStructure(const aData:TVkPipelineCacheCreateInfo);
    function CreatePipelineCache:TvkObj_PipelineCache; 
    property Flags:TVkPipelineCacheCreateFlags read fFlags write fFlags;
  end;

 TvkObj_PipelineShaderStageCreateInfo=class(TvkObj_Structure)
  public
    Flags:TVkPipelineShaderStageCreateFlags;
    Stage:TVkShaderStageFlagBits;
    Module:TVkShaderModule;
    Name:String;
    function  GetStructure:TVkPipelineShaderStageCreateInfo;
    procedure SetStructure(const aData:TVkPipelineShaderStageCreateInfo);
  end;
 TvkObj_PipelineShaderStageCreateInfoList=specialize TvkObj_ObjList<TvkObj_PipelineShaderStageCreateInfo>;


 TvkObj_VertexInputBindingDescription=class(TvkObj_Structure)
  public
    Binding:TVkUInt32;
    Stride:TVkUInt32;
    InputRate:TVkVertexInputRate;
    function  GetStructure:TVkVertexInputBindingDescription;
    procedure SetStructure(const aData:TVkVertexInputBindingDescription);
  end;
 TvkObj_VertexInputBindingDescriptionList=specialize TvkObj_ObjList<TvkObj_VertexInputBindingDescription>;

 TvkObj_VertexInputAttributeDescription=class(TvkObj_Structure)
  public
    Location:TVkUInt32;
    Binding:TVkUInt32;
    Format:TVkFormat;
    Offset:TVkUInt32;
    function  GetStructure:TVkVertexInputAttributeDescription;
    procedure SetStructure(const aData:TVkVertexInputAttributeDescription);
  end;
 TvkObj_VertexInputAttributeDescriptionList=specialize TvkObj_ObjList<TvkObj_VertexInputAttributeDescription>;


 TvkObj_PipelineVertexInputStateCreateInfo=class(TvkObj_Structure)
  private
    fFlags:TVkPipelineVertexInputStateCreateFlags;
    fVertexBindingDescriptions:TvkObj_VertexInputBindingDescriptionList;
    fVertexAttributeDescriptions:TvkObj_VertexInputAttributeDescriptionList;
    fvkVertexBindingDescriptions:TVkVertexInputBindingDescriptionArr;
    fvkVertexAttributeDescriptions:TVkVertexInputAttributeDescriptionArr;
  public  
    constructor Create;
    destructor Destroy; override;
    function  GetStructure:TVkPipelineVertexInputStateCreateInfo;
    procedure SetStructure(const aData:TVkPipelineVertexInputStateCreateInfo);
    property Flags:TVkPipelineVertexInputStateCreateFlags read fFlags write fFlags;
    property VertexBindingDescriptions:TvkObj_VertexInputBindingDescriptionList read fVertexBindingDescriptions;
    property VertexAttributeDescriptions:TvkObj_VertexInputAttributeDescriptionList read fVertexAttributeDescriptions;
  end;

 TvkObj_PipelineInputAssemblyStateCreateInfo=class(TvkObj_Structure)
    Flags:TVkPipelineInputAssemblyStateCreateFlags;
    Topology:TVkPrimitiveTopology;
    PrimitiveRestartEnable:Boolean;
    function  GetStructure:TVkPipelineInputAssemblyStateCreateInfo;
    procedure SetStructure(const aData:TVkPipelineInputAssemblyStateCreateInfo);
  end;

 TvkObj_PipelineTessellationStateCreateInfo=class(TvkObj_Structure)
  public
    Flags:TVkPipelineTessellationStateCreateFlags;
    PatchControlPoints:TVkUInt32;
    function  GetStructure:TVkPipelineTessellationStateCreateInfo;
    procedure SetStructure(const aData:TVkPipelineTessellationStateCreateInfo);
  end;

 TvkObj_Viewport=class(TvkObj_Structure)
    x:TVkFloat;
    y:TVkFloat;
    Width:TVkFloat;
    Height:TVkFloat;
    MinDepth:TVkFloat;
    MaxDepth:TVkFloat;
    function  GetStructure:TVkViewport;
    procedure SetStructure(const aData:TVkViewport);
  end;
 TvkObj_ViewportList=specialize TvkObj_ObjList<TvkObj_Viewport>;


 TvkObj_Rect2D=class(TvkObj_Structure)
    Offset:TVkOffset2D;
    Extent:TVkExtent2D;
    function  GetStructure:TVkRect2D;
    procedure SetStructure(const aData:TVkRect2D);
  end;
 TvkObj_Rect2DList=specialize TvkObj_ObjList<TvkObj_Rect2D>;


 TvkObj_PipelineViewportStateCreateInfo=class(TvkObj_Structure)
  private
    fFlags:TVkPipelineViewportStateCreateFlags;
    fViewports:TvkObj_ViewportList;
    fScissors:TvkObj_Rect2DList;
    fvkViewports:TVkViewportArr;
    fvkScissors:TVkRect2DArr;
  public  
    constructor Create;
    destructor Destroy; override;
    function  GetStructure:TVkPipelineViewportStateCreateInfo;
    procedure SetStructure(const aData:TVkPipelineViewportStateCreateInfo);
    property Flags:TVkPipelineViewportStateCreateFlags read fFlags write fFlags;
    property Viewports:TvkObj_ViewportList read fViewports;
    property Scissors:TvkObj_Rect2DList read fScissors;
  end;

 TvkObj_PipelineRasterizationStateCreateInfo=class(TvkObj_Structure)
    Flags:TVkPipelineRasterizationStateCreateFlags;
    DepthClampEnable:Boolean;
    RasterizerDiscardEnable:Boolean;
    PolygonMode:TVkPolygonMode;
    CullMode:TVkCullModeFlags;
    FrontFace:TVkFrontFace;
    DepthBiasEnable:Boolean;
    DepthBiasConstantFactor:TVkFloat;
    DepthBiasClamp:TVkFloat;
    DepthBiasSlopeFactor:TVkFloat;
    LineWidth:TVkFloat;
    function  GetStructure:TVkPipelineRasterizationStateCreateInfo;
    procedure SetStructure(const aData:TVkPipelineRasterizationStateCreateInfo);
  end;

 TvkObj_PipelineMultisampleStateCreateInfo=class(TvkObj_Structure)
  private
    fSampleMask:TVkSampleMaskList;
  public
    Flags:TVkPipelineMultisampleStateCreateFlags;
    RasterizationSamples:TVkSampleCountFlagBits;
    SampleShadingEnable:Boolean;
    MinSampleShading:TVkFloat;
    AlphaToCoverageEnable:Boolean;
    AlphaToOneEnable:Boolean;
    constructor Create;
    destructor Destroy; override;
    function  GetStructure:TVkPipelineMultisampleStateCreateInfo;
    procedure SetStructure(const aData:TVkPipelineMultisampleStateCreateInfo); 
    property SampleMask:TVkSampleMaskList read fSampleMask;
  end;

 TvkObj_PipelineDepthStencilStateCreateInfo=class(TvkObj_Structure)
  public
    Flags:TVkPipelineDepthStencilStateCreateFlags;
    DepthTestEnable:Boolean;
    DepthWriteEnable:Boolean;
    DepthCompareOp:TVkCompareOp;
    DepthBoundsTestEnable:Boolean;
    StencilTestEnable:Boolean;
    Front:TVkStencilOpState;
    Back:TVkStencilOpState;
    MinDepthBounds:TVkFloat;
    MaxDepthBounds:TVkFloat;
    function  GetStructure:TVkPipelineDepthStencilStateCreateInfo;
    procedure SetStructure(const aData:TVkPipelineDepthStencilStateCreateInfo);
  end;

 TvkObj_PipelineColorBlendAttachmentState=class(TvkObj_Structure)
  public
    BlendEnable:Boolean;
    SrcColorBlendFactor:TVkBlendFactor;
    DstColorBlendFactor:TVkBlendFactor;
    ColorBlendOp:TVkBlendOp;
    SrcAlphaBlendFactor:TVkBlendFactor;
    DstAlphaBlendFactor:TVkBlendFactor;
    AlphaBlendOp:TVkBlendOp;
    ColorWriteMask:TVkColorComponentFlags;
    function  GetStructure:TVkPipelineColorBlendAttachmentState;
    procedure SetStructure(const aData:TVkPipelineColorBlendAttachmentState);
  end;
 TvkObj_PipelineColorBlendAttachmentStateList=specialize TvkObj_ObjList<TvkObj_PipelineColorBlendAttachmentState>;


 TvkObj_PipelineColorBlendStateCreateInfo=class(TvkObj_Structure)
  private
    fAttachments:TvkObj_PipelineColorBlendAttachmentStateList;
    fvkAttachments:TVkPipelineColorBlendAttachmentStateArr;
  public
    Flags:TVkPipelineColorBlendStateCreateFlags;
    LogicOpEnable:Boolean;
    LogicOp:TVkLogicOp;
    BlendConstants:array[0..3] of TVkFloat; 
    constructor Create;
    destructor Destroy; override;
    function  GetStructure:TVkPipelineColorBlendStateCreateInfo;
    procedure SetStructure(const aData:TVkPipelineColorBlendStateCreateInfo);
    property Attachments:TvkObj_PipelineColorBlendAttachmentStateList read fAttachments;
  end;

 TvkObj_PipelineDynamicStateCreateInfo=class(TvkObj_Structure)
  private
    fDynamicStates:TVkDynamicStateList;
    fvkDynamicStates:TVkDynamicStateArr;
  public
    Flags:TVkPipelineDynamicStateCreateFlags; 
    constructor Create;
    destructor Destroy; override;
    function  GetStructure:TVkPipelineDynamicStateCreateInfo;
    procedure SetStructure(const aData:TVkPipelineDynamicStateCreateInfo);
    property DynamicStates:TVkDynamicStateList read fDynamicStates;
  end;

 TvkObj_PipelineFactory=class(TvkObj_DeviceObjFactory)
  private
    fFlags:TVkPipelineCreateFlags;
    fLayout:TVkPipelineLayout;
    fBasePipelineHandle:TVkPipeline;
    fBasePipelineIndex:TVkInt32;
  public
    property Flags:TVkPipelineCreateFlags read fFlags write fFlags;
    property Layout:TVkPipelineLayout read fLayout write fLayout;
    property BasePipelineHandle:TVkPipeline read fBasePipelineHandle write fBasePipelineHandle;
    property BasePipelineIndex:TVkInt32 read fBasePipelineIndex write fBasePipelineIndex;
  end;

 TvkObj_GraphicsPipelineFactory=class(TvkObj_PipelineFactory)
  private
    fRenderPass:TVkRenderPass;
    fSubpass:TVkUInt32;
    fStages:TvkObj_PipelineShaderStageCreateInfoList;
    fVertexInputState:TvkObj_PipelineVertexInputStateCreateInfo;
    fInputAssemblyState:TvkObj_PipelineInputAssemblyStateCreateInfo;
    fTessellationState:TvkObj_PipelineTessellationStateCreateInfo;
    fViewportState:TvkObj_PipelineViewportStateCreateInfo;
    fRasterizationState:TvkObj_PipelineRasterizationStateCreateInfo;
    fMultisampleState:TvkObj_PipelineMultisampleStateCreateInfo;
    fDepthStencilState:TvkObj_PipelineDepthStencilStateCreateInfo;
    fColorBlendState:TvkObj_PipelineColorBlendStateCreateInfo;
    fDynamicState:TvkObj_PipelineDynamicStateCreateInfo;
    fvkStages:TVkPipelineShaderStageCreateInfoArr;
    fvkVertexInputState:TVkPipelineVertexInputStateCreateInfo;
    fvkInputAssemblyState:TVkPipelineInputAssemblyStateCreateInfo;
    fvkTessellationState:TVkPipelineTessellationStateCreateInfo;
    fvkRasterizationState:TVkPipelineRasterizationStateCreateInfo;
    fvkColorBlendState:TVkPipelineColorBlendStateCreateInfo;
    fvkMultisampleState:TVkPipelineMultisampleStateCreateInfo;
    fvkViewportState:TVkPipelineViewportStateCreateInfo;
    fvkDepthStencilState:TVkPipelineDepthStencilStateCreateInfo;
    fvkDynamicState:TVkPipelineDynamicStateCreateInfo;
  public
    function  GetStructure:TVkGraphicsPipelineCreateInfo;
    procedure SetStructure(const aData:TVkGraphicsPipelineCreateInfo);
    function  CreateGraphicsPipeline(const aPipelineCache:TVkPipelineCache):TvkObj_GraphicsPipeline;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Stages:TvkObj_PipelineShaderStageCreateInfoList read fStages;
    property VertexInputState:TvkObj_PipelineVertexInputStateCreateInfo read fVertexInputState;
    property InputAssemblyState:TvkObj_PipelineInputAssemblyStateCreateInfo read fInputAssemblyState;
    property TessellationState:TvkObj_PipelineTessellationStateCreateInfo read fTessellationState;
    property ViewportState:TvkObj_PipelineViewportStateCreateInfo read fViewportState;
    property RasterizationState:TvkObj_PipelineRasterizationStateCreateInfo read fRasterizationState;
    property MultiSampleState:TvkObj_PipelineMultisampleStateCreateInfo read fMultiSampleState;
    property DepthStencilState:TvkObj_PipelineDepthStencilStateCreateInfo read fDepthStencilState;
    property ColorBlendState:TvkObj_PipelineColorBlendStateCreateInfo read fColorBlendState;
    property DynamicState:TvkObj_PipelineDynamicStateCreateInfo read fDynamicState; 
    property RenderPass:TVkRenderPass read fRenderPass write fRenderPass;
    property SubPass:TVkUInt32 read fSubPass write fSubPass;
  end;

 TvkObj_ComputePipelineFactory=class(TvkObj_PipelineFactory)
  end;

 TvkObj_PipelineLayout=class(TvkObj_AllocHandle)
  private
    fHandle:TVkPipelineLayout;
    fDeviceFunctions:TVkDeviceFunctions;
    procedure CreateHandle(const aCreateInfo:TVkPipelineLayoutCreateInfo);
  public
    constructor Create(const aCreateInfo:TVkPipelineLayoutCreateInfo;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    constructor Create(const aHandle:TVkPipelineLayout;
                       const aOwnsHandle:Boolean;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    destructor Destroy; override;  
    property Handle:TVkPipelineLayout read fHandle;
    property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
  end;


 TvkObj_PushConstantRange=class(TvkObj_Structure)
  public
    StageFlags:TVkShaderStageFlags;
    Offset:TVkUInt32;
    Size:TVkUInt32;
    function  GetStructure:TVkPushConstantRange;
    procedure SetStructure(const aData:TVkPushConstantRange);
  end;
 TvkObj_PushConstantRangeList=specialize TvkObj_ObjList<TvkObj_PushConstantRange>;


 TvkObj_PipelineLayoutFactory=class(TvkObj_DeviceObjFactory)
  private
    fFlags:TVkPipelineLayoutCreateFlags;
    fSetLayouts:TVkDescriptorSetLayoutList;
    fPushConstantRanges:TvkObj_PushConstantRangeList;
    fvkPushConstantRanges:TVkPushConstantRangeArr;
  public
    function  GetStructure:TVkPipelineLayoutCreateInfo;
    procedure SetStructure(const aData:TVkPipelineLayoutCreateInfo);
    function CreatePipelineLayout:TvkObj_PipelineLayout;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;  
    property Flags:TVkPipelineLayoutCreateFlags read fFlags write fFlags;
    property SetLayouts:TVkDescriptorSetLayoutList read fSetLayouts;
    property PushConstantRanges:TvkObj_PushConstantRangeList read fPushConstantRanges;
  end;

 TvkObj_Queue=class(TvkObj_Object)
  private
    fHandle:TVkQueue;
    fDeviceFunctions:TVkDeviceFunctions;
  public 
    constructor Create(const aHandle:TVkQueue; const aDeviceFunctions:TVkDeviceFunctions);
    procedure Submit(const aWaitSemaphores:array of TVkSemaphore;
                     const aWaitDstStageMask:array of TVkSemaphore;
                     const aCommandBuffers:array of TVkCommandBuffer;
                     const aSignalSemaphores:array of TVkSemaphore;
                     const aFence:TVkFence);
    function Present(const aWaitSemaphore:array of TVkSemaphore;
                     const aSwapChains:array of TVkSwapchainKHR;
                     const aImageIndices:array of TVkUInt32):TVkResultArr;
    procedure WaitIdle;
    property Handle:TVkQueue read fHandle;
    property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
  end;

 TvkObj_RenderPass=class(TvkObj_AllocHandle)
  private
    fHandle:TVkRenderPass;
    fDeviceFunctions:TVkDeviceFunctions;
    procedure CreateHandle(const aCreateInfo:TVkRenderPassCreateInfo);
  public
    constructor Create(const aCreateInfo:TVkRenderPassCreateInfo;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    constructor Create(const aHandle:TVkRenderPass;
                       const aOwnsHandle:Boolean;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    destructor Destroy; override; 
    property Handle:TVkRenderPass read fHandle;
    property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
  end;

 TvkObj_AttachmentDescription=class(TvkObj_Structure)
  public
    Flags:TVkAttachmentDescriptionFlags;
    Format:TVkFormat;
    Samples:TVkSampleCountFlagBits;
    LoadOp:TVkAttachmentLoadOp;
    StoreOp:TVkAttachmentStoreOp;
    StencilLoadOp:TVkAttachmentLoadOp;
    StencilStoreOp:TVkAttachmentStoreOp;
    InitialLayout:TVkImageLayout;
    FinalLayout:TVkImageLayout;
    function  GetStructure:TVkAttachmentDescription;
    procedure SetStructure(const aData:TVkAttachmentDescription);
  end;
 TvkObj_AttachmentDescriptionList=specialize TvkObj_ObjList<TvkObj_AttachmentDescription>;


 TvkObj_AttachmentReference=class(TvkObj_Structure)
  public
    Attachment:TVkUInt32;
    Layout:TVkImageLayout;
    function  GetStructure:TVkAttachmentReference;
    procedure SetStructure(const aData:TVkAttachmentReference);
  end;
 TvkObj_AttachmentReferenceList=specialize TvkObj_ObjList<TvkObj_AttachmentReference>;


 TvkObj_SubpassDescription=class(TvkObj_Structure)
  private
    fInputAttachments:TvkObj_AttachmentReferenceList;
    fColorAttachments:TvkObj_AttachmentReferenceList;
    fResolveAttachments:TvkObj_AttachmentReferenceList;
    fDepthStencilAttachments:TvkObj_AttachmentReferenceList;
    fPreserveAttachments:TvkInt32List;
    fvkInputAttachments:TVkAttachmentReferenceArr;
    fvkColorAttachments:TVkAttachmentReferenceArr;
    fvkResolveAttachments:TVkAttachmentReferenceArr;
    fvkDepthStencilAttachments:TVkAttachmentReferenceArr;
  public
    Flags:TVkSubpassDescriptionFlags;
    PipelineBindPoint:TVkPipelineBindPoint;
    function  GetStructure:TVkSubpassDescription;
    procedure SetStructure(const aData:TVkSubpassDescription);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property InputAttachments:TvkObj_AttachmentReferenceList read fInputAttachments;
    property ColorAttachments:TvkObj_AttachmentReferenceList read fColorAttachments;
    property ResolveAttachments:TvkObj_AttachmentReferenceList read fResolveAttachments;
    property DepthStencilAttachments:TvkObj_AttachmentReferenceList read fDepthStencilAttachments;
    property PreserveAttachments:TvkInt32List read fPreserveAttachments;
  end;
 TvkObj_SubpassDescriptionList=specialize TvkObj_ObjList<TvkObj_SubpassDescription>;

 TvkObj_SubpassDependency=class(TvkObj_Structure)
  public
    SrcSubpass:TVkUInt32;
    DstSubpass:TVkUInt32;
    SrcStageMask:TVkPipelineStageFlags;
    DstStageMask:TVkPipelineStageFlags;
    SrcAccessMask:TVkAccessFlags;
    DstAccessMask:TVkAccessFlags;
    DependencyFlags:TVkDependencyFlags;
    function  GetStructure:TVkSubpassDependency;
    procedure SetStructure(const aData:TVkSubpassDependency);
  end;
 TvkObj_SubpassDependencyList=specialize TvkObj_ObjList<TvkObj_SubpassDependency>;

 TvkObj_RenderPassFactory=class(TvkObj_DeviceObjFactory)
  private
    fFlags:TVkRenderPassCreateFlags;
    fAttachments:TvkObj_AttachmentDescriptionList;
    fSubpasses:TvkObj_SubpassDescriptionList;
    fDependencies:TvkObj_SubpassDependencyList;
    fvkAttachments:TVkAttachmentDescriptionArr;
    fvkSubpasses:TVkSubpassDescriptionArr;
    fvkDependencies:TVkSubpassDependencyArr;
  public
    function  GetStructure:TVkRenderPassCreateInfo;
    procedure SetStructure(const aData:TVkRenderPassCreateInfo);
    function  CreateRenderPass:TvkObj_RenderPass;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;  
    property Flags:TVkRenderPassCreateFlags read fFlags write fFlags;
    property Attachments:TvkObj_AttachmentDescriptionList read fAttachments;
    property Subpasses:TvkObj_SubpassDescriptionList read fSubpasses;
    property Dependencies:TvkObj_SubpassDependencyList read fDependencies;
  end;

 TvkObj_Semaphore=class(TvkObj_AllocHandle)
  private
    fHandle:TVkSemaphore;
    fDeviceFunctions:TVkDeviceFunctions;
    procedure CreateHandle(const aCreateInfo:TVkSemaphoreCreateInfo);
  public
    constructor Create(const aFlags:TVkSemaphoreCreateFlags;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    constructor Create(const aCreateInfo:TVkSemaphoreCreateInfo;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    constructor Create(const aHandle:TVkSemaphore;
                       const aOwnsHandle:Boolean;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    destructor Destroy; override;  
    property Handle:TVkSemaphore read fHandle;
    property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
  end;

 TvkObj_ShaderModule=class(TvkObj_AllocHandle)
  private
    fHandle:TVkShaderModule;
    fDeviceFunctions:TVkDeviceFunctions;
    procedure CreateHandle(const aCreateInfo:TVkShaderModuleCreateInfo);
  public
    constructor Create(const aCreateInfo:TVkShaderModuleCreateInfo;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    constructor Create(const aHandle:TVkShaderModule;
                       const aOwnsHandle:Boolean;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    destructor Destroy; override;   
    property Handle:TVkShaderModule read fHandle;
    property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
  end;


 TvkObj_ShaderModuleFactory=class(TvkObj_DeviceObjFactory)
  private
    fFlags:TVkShaderModuleCreateFlags;
    fShaderCode:PVkVoid;
    fShaderCodeSize:TVkSize;
    procedure ReallocShaderCode(const aSize:TVkSize);
    procedure SetGlslCode(const aCode:PVkVoid; const aSize:TVkSize; const aStage:TVkShaderStageFlags);
  public
    procedure SetShaderCode(const aCode:PVkVoid; const aSize:TVkSize);
    procedure LoadSpvCode(const aStream:TStream; aSize:TVkSize=0);
    procedure LoadSpvCode(const aFilename:String);
    procedure SetGlslCode(const aCode:String; const aStage:TVkShaderStageFlags);
    procedure LoadGlslCode(const aStream:TStream; const aStage:TVkShaderStageFlags; aSize:TVkSize=0);
    procedure LoadGlslCode(const aFilename:String; const aStage:TVkShaderStageFlags);
    function  GetStructure:TVkShaderModuleCreateInfo;
    procedure SetStructure(const aData:TVkShaderModuleCreateInfo);
    function  CreateShaderModule:TvkObj_ShaderModule;
    procedure BeforeDestruction; override; 
    property Flags:TVkShaderModuleCreateFlags read fFlags write fFlags;
  end;

 TvkObj_SwapChain=class(TvkObj_AllocHandle)
  private
    fHandle:TVkSwapchainKHR;
    fDeviceFunctions:TVkDeviceFunctions;
    procedure CreateHandle(const aCreateInfo:PVkSwapchainCreateInfoKHR);
  public 
    constructor Create(const aCreateInfo:TVkSwapchainCreateInfoKHR;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil); overload;
    constructor Create(const aHandle:TVkSwapchainKHR;
                       const aOwnsHandle:Boolean;
                       const aDeviceFunctions:TVkDeviceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil); overload;
    destructor Destroy; override;
    function GetImages:TVkImageArr;
    function AcquireNextImage(const aTimeout:TVkUInt64; const aSemaphore:TVkSemaphore; const aFence:TVkFence):TVkUInt32;
    property Handle:TVkSwapchainKHR read fHandle;
    property DeviceFunctions:TVkDeviceFunctions read fDeviceFunctions;
  end;


 TvkObj_SwapChainFactory=class(TvkObj_DeviceObjFactory)
  private
    fFlags:TVkSwapchainCreateFlagsKHR;
    fSurface:TVkSurfaceKHR;
    fMinImageCount:TVkUInt32;
    fImageFormat:TVkFormat;
    fImageColorSpace:TVkColorSpaceKHR;
    fImageExtent:TVkExtent2D;
    fImageArrayLayers:TVkUInt32;
    fImageUsage:TVkImageUsageFlags;
    fImageSharingMode:TVkSharingMode;
    fQueueFamilyIndices:TvkInt32List;
    fPreTransform:TVkSurfaceTransformFlagBitsKHR;
    fComposideAlpha:TVkCompositeAlphaFlagBitsKHR;
    fPresentMode:TVkPresentModeKHR;
    fClipped:Boolean;
    fOldSwapChain:TVkSwapchainKHR;
  public
    function  GetStructure:TVkSwapchainCreateInfoKHR;
    procedure SetStructure(const aData:TVkSwapchainCreateInfoKHR);
    function  CreateSwapchain:TvkObj_SwapChain;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;     
    property Flags:TVkSwapchainCreateFlagsKHR read fFlags  write fFlags;
    property Surface:TVkSurfaceKHR read fSurface write fSurface;
    property MinImageCount:TVkUInt32 read fMinImageCount write fMinImageCount;
    property ImageFormat:TVkFormat read fImageFormat write fImageFormat;
    property ImageColorSpace:TVkColorSpaceKHR read fImageColorSpace write fImageColorSpace;
    property ImageExtent:TVkExtent2D read fImageExtent write fImageExtent;
    property ImageArrayLayers:TVkUInt32 read fImageArrayLayers write fImageArrayLayers;
    property ImageUsage:TVkImageUsageFlags read fImageUsage write fImageUsage;
    property ImageSharingMode:TVkSharingMode read fImageSharingMode write fImageSharingMode;
    property PreTransform:TVkSurfaceTransformFlagBitsKHR read fPreTransform write fPreTransform;
    property ComposideAlpha:TVkCompositeAlphaFlagBitsKHR read fComposideAlpha write fComposideAlpha;
    property PresentMode:TVkPresentModeKHR read fPresentMode write fPresentMode;
    property Clipped:Boolean read fClipped write fClipped;
    property OldSwapChain:TVkSwapchainKHR read fOldSwapChain write fOldSwapChain;
    property QueueFamilyIndices:TvkInt32List read fQueueFamilyIndices;
  end;


TvkObj_Surface=class(TvkObj_AllocHandle)
  private
    fHandle:TVkSurfaceKHR;
    fInstanceFunctions:TVkInstanceFunctions;
    procedure CreateHandle(const aCreateInfo:Pointer);
  public
    constructor Create(const aHandle:TVkSurfaceKHR;
                       const aOwnsHandle:Boolean;
                       const aInstanceFunctions:TVkInstanceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
    destructor Destroy; override;
  {$IF DEFINED(VK_USE_PLATFORM_WIN32_KHR)}
    constructor Create(const aCreateInfo:TVkWin32SurfaceCreateInfoKHR;
                       const aInstanceFunctions:TVkInstanceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
  {$ELSEIF DEFINED(VK_USE_PLATFORM_XLIB_KHR)}
    constructor Create(const aCreateInfo:TVkXlibSurfaceCreateInfoKHR;
                       const aInstanceFunctions:TVkInstanceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
  {$ELSEIF DEFINED(VK_USE_PLATFORM_XCB_KHR)}
    constructor Create(const aCreateInfo:TVkXcbSurfaceCreateInfoKHR;
                       const aInstanceFunctions:TVkInstanceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
  {$ELSEIF DEFINED(VK_USE_PLATFORM_WAYLAND_KHR)}
    constructor Create(const aCreateInfo:TVkWaylandSurfaceCreateInfoKHR;
                       const aInstanceFunctions:TVkInstanceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
  {$ELSEIF DEFINED(VK_USE_PLATFORM_MIR_KHR)}
    constructor Create(const aCreateInfo:TVkMirSurfaceCreateInfoKHR;
                       const aInstanceFunctions:TVkInstanceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
  {$ELSEIF DEFINED(VK_USE_PLATFORM_ANDROID_KHR)}
    constructor Create(const aCreateInfo:TVkAndroidSurfaceCreateInfoKHR;
                       const aInstanceFunctions:TVkInstanceFunctions;
                       const aAllocCallbacks:PVkAllocationCallbacks=nil);
  {$ELSE}
    {$ERROR 'TvkObj_Surface=class, Unsupportet platform'}
  {$ENDIF}
    class function GetPlatformSurfaceExtensionName:String;
    property Handle:TVkSurfaceKHR read fHandle;
    property InstanceFunctions:TVkInstanceFunctions read fInstanceFunctions;
  end;


TvkObj_SurfaceFactory=class(TvkObj_InstanceObjFactory)
  private
    fFlags:TVkFlags;
    fWinControl:TWinControl;
    procedure SetWinControl(aValue:TWinControl);
  {$IF DEFINED(VK_USE_PLATFORM_WIN32_KHR)}
  private
    fHInstance:TVkHINSTANCE;
    fHwnd:TVkHWND;
    procedure SetHwnd(const aValue:TVkHWND);
  public
    function  GetStructure:TVkWin32SurfaceCreateInfoKHR;
    procedure SetStructure(const aData:TVkWin32SurfaceCreateInfoKHR);
    property HInstance:TVkHINSTANCE read fHInstance write fHInstance;
    property Hwnd:TVkHWND read fHwnd write SetHwnd;
  {$ELSEIF DEFINED(VK_USE_PLATFORM_XLIB_KHR)}
  private
    fdpy:PVkXLIBDisplay;
    fWindow:TVkXLIBWindow;
    procedure SetWindow(const aValue:TVkXLIBWindow);
  public
    function  GetStructure:TVkXlibSurfaceCreateInfoKHR;
    procedure SetStructure(const aData:TVkXlibSurfaceCreateInfoKHR);
    property Dpy:PVkXLIBDisplay read fdpy write fdpy;
    property Window:TVkXLIBWindow read fWindow write SetWindow;
  {$ELSEIF DEFINED(VK_USE_PLATFORM_XCB_KHR)}
  private
    fConnection:PVkXCBConnection;
    fWindow:TVkXCBWindow;
    procedure SetWindow(const aValue:TVkXCBWindow);
  public
    function  GetStructure:TVkXcbSurfaceCreateInfoKHR;
    procedure SetStructure(const aData:TVkXcbSurfaceCreateInfoKHR);
    property Connection:PVkXCBConnection read fConnection write fConnection;
    property Window:TVkXCBWindow read fWindow write SetWindow;
  {$ELSEIF DEFINED(VK_USE_PLATFORM_WAYLAND_KHR)}
  private
    fDisplay:PVkWaylandDisplay;
    fSurface:PVkWaylandSurface;
    procedure SetWindow(const aValue:PVkWaylandSurface);
  public
    function  GetStructure:TVkWaylandSurfaceCreateInfoKHR;
    procedure SetStructure(const aData:TVkWaylandSurfaceCreateInfoKHR);
    property Display:PVkWaylandDisplay read fDisplay write fDisplay;
    property Surface:PVkWaylandSurface read fSurface write SetWindow;
  {$ELSEIF DEFINED(VK_USE_PLATFORM_MIR_KHR)}
  private
    fConnection:PVkMirConnection;
    fMirSurface:PVkMirSurface;
    procedure SetWindow(const aValue:PVkMirSurface);
  public
    function  GetStructure:TVkMirSurfaceCreateInfoKHR;
    procedure SetStructure(const aData:TVkMirSurfaceCreateInfoKHR);
    property Connection:PVkMirConnection read fConnection write fConnection;
    property MirSurface:PVkMirSurface read fMirSurface write SetWindow;
  {$ELSEIF DEFINED(VK_USE_PLATFORM_ANDROID_KHR)}
  private
    fWindow:PVkAndroidANativeWindow;
    procedure SetWindow(const aValue:PVkAndroidANativeWindow);
  public
    function  GetStructure:TVkAndroidSurfaceCreateInfoKHR;
    procedure SetStructure(const aData:TVkAndroidSurfaceCreateInfoKHR);
    property Window:PVkAndroidANativeWindow read fWindow write SetWindow;
  {$ELSE}
    {$ERROR 'TvkObj_SurfaceFactory=class, Unsupportet platform'}
  {$ENDIF}
  public
    function CreateSurface:TvkObj_Surface;
    procedure AfterConstruction; override;
    property WinControl:TWinControl read fWinControl write SetWinControl;
    property Flags:TVkFlags read fFlags write fFlags;
  end;

//=======================================================

function vkoExtent2D(const X, Y:TVkUInt32):TVkExtent2D;
function vkoExtent3D(const X, Y, Z:TVkUInt32):TVkExtent3D;

function vkoMakeString(const aFeatures:TVkPhysicalDeviceFeatures; const aSeperator:String=sLineBreak):String;
function vkoMakeString(const aProperties:TVkPhysicalDeviceProperties; const aPrintLimits, aPrintSparseProps:Boolean):String;
function vkoMakeString(const aMemoryProperties:TVkPhysicalDeviceMemoryProperties):String;

function vkuGetMemoryTypeIndex(const aMemoryProperties:TVkPhysicalDeviceMemoryProperties;
                               aMemoryTypeBits:TVkUInt32;
                               const aProperty:TVkMemoryPropertyFlagBit):TVkUInt32;
//=======================================================

implementation

function vkoExtent2D(const X, Y:TVkUInt32):TVkExtent2D;
begin
  result.width:=x;
  result.height:=y;
end;

function vkoExtent3D(const X, Y, Z:TVkUInt32):TVkExtent3D;
begin
  result.width:=x;
  result.height:=y;
  result.depth:=z;
end;

function vkoMakeString(const aFeatures:TVkPhysicalDeviceFeatures; const aSeperator:String):String;

  procedure AddToResult(const b:TVkBool32; const aName:String);
  begin
    if (b=0) then
      exit;
    if (result <> '') then
      result:=result+aSeperator;
    result:=result+aName;
  end;

begin
  result:='';
  AddToResult(aFeatures.robustBufferAccess,                       'RobustBufferAccess');
  AddToResult(aFeatures.fullDrawIndexUint32,                      'FullDrawIndexUint32');
  AddToResult(aFeatures.imageCubeArray,                           'ImageCubeArray');
  AddToResult(aFeatures.independentBlend,                         'IndependentBlend');
  AddToResult(aFeatures.geometryShader,                           'GeometryShader');
  AddToResult(aFeatures.tessellationShader,                       'TessellationShader');
  AddToResult(aFeatures.sampleRateShading,                        'SampleRateShading');
  AddToResult(aFeatures.dualSrcBlend,                             'DualSrcBlend');
  AddToResult(aFeatures.logicOp,                                  'LogicOp');
  AddToResult(aFeatures.multiDrawIndirect,                        'MultiDrawIndirect');
  AddToResult(aFeatures.drawIndirectFirstInstance,                'DrawIndirectFirstInstance');
  AddToResult(aFeatures.depthClamp,                               'DepthClamp');
  AddToResult(aFeatures.depthBiasClamp,                           'DepthBiasClamp');
  AddToResult(aFeatures.fillModeNonSolid,                         'FillModeNonSolid');
  AddToResult(aFeatures.depthBounds,                              'DepthBounds');
  AddToResult(aFeatures.wideLines,                                'WideLines');
  AddToResult(aFeatures.largePoints,                              'LargePoints');
  AddToResult(aFeatures.alphaToOne,                               'AlphaToOne');
  AddToResult(aFeatures.multiViewport,                            'MultiViewport');
  AddToResult(aFeatures.samplerAnisotropy,                        'SamplerAnisotropy');
  AddToResult(aFeatures.textureCompressionETC2,                   'TextureCompressionETC2');
  AddToResult(aFeatures.textureCompressionASTC_LDR,               'TextureCompressionASTC_LDR');
  AddToResult(aFeatures.textureCompressionBC,                     'TextureCompressionBC');
  AddToResult(aFeatures.occlusionQueryPrecise,                    'OcclusionQueryPrecise');
  AddToResult(aFeatures.pipelineStatisticsQuery,                  'PipelineStatisticsQuery');
  AddToResult(aFeatures.vertexPipelineStoresAndAtomics,           'VertexPipelineStoresAndAtomics');
  AddToResult(aFeatures.fragmentStoresAndAtomics,                 'FragmentStoresAndAtomics');
  AddToResult(aFeatures.shaderTessellationAndGeometryPointSize,   'ShaderTessellationAndGeometryPointSize');
  AddToResult(aFeatures.shaderImageGatherExtended,                'ShaderImageGatherExtended');
  AddToResult(aFeatures.shaderStorageImageExtendedFormats,        'ShaderStorageImageExtendedFormats');
  AddToResult(aFeatures.shaderStorageImageMultisample,            'ShaderStorageImageMultisample');
  AddToResult(aFeatures.shaderStorageImageReadWithoutFormat,      'ShaderStorageImageReadWithoutFormat');
  AddToResult(aFeatures.shaderStorageImageWriteWithoutFormat,     'ShaderStorageImageWriteWithoutFormat');
  AddToResult(aFeatures.shaderUniformBufferArrayDynamicIndexing,  'ShaderUniformBufferArrayDynamicIndexing');
  AddToResult(aFeatures.shaderSampledImageArrayDynamicIndexing,   'ShaderSampledImageArrayDynamicIndexing');
  AddToResult(aFeatures.shaderStorageBufferArrayDynamicIndexing,  'ShaderStorageBufferArrayDynamicIndexing');
  AddToResult(aFeatures.shaderStorageImageArrayDynamicIndexing,   'ShaderStorageImageArrayDynamicIndexing');
  AddToResult(aFeatures.shaderClipDistance,                       'ShaderClipDistance');
  AddToResult(aFeatures.shaderCullDistance,                       'ShaderCullDistance');
  AddToResult(aFeatures.shaderFloat64,                            'ShaderFloat64');
  AddToResult(aFeatures.shaderInt64,                              'ShaderInt64');
  AddToResult(aFeatures.shaderInt16,                              'ShaderInt16');
  AddToResult(aFeatures.shaderResourceResidency,                  'ShaderResourceResidency');
  AddToResult(aFeatures.shaderResourceMinLod,                     'ShaderResourceMinLod');
  AddToResult(aFeatures.sparseBinding,                            'SparseBinding');
  AddToResult(aFeatures.sparseResidencyBuffer,                    'SparseResidencyBuffer');
  AddToResult(aFeatures.sparseResidencyImage2D,                   'SparseResidencyImage2D');
  AddToResult(aFeatures.sparseResidencyImage3D,                   'SparseResidencyImage3D');
  AddToResult(aFeatures.sparseResidency2Samples,                  'SparseResidency2Samples');
  AddToResult(aFeatures.sparseResidency4Samples,                  'SparseResidency4Samples');
  AddToResult(aFeatures.sparseResidency8Samples,                  'SparseResidency8Samples');
  AddToResult(aFeatures.sparseResidency16Samples,                 'SparseResidency16Samples');
  AddToResult(aFeatures.sparseResidencyAliased,                   'SparseResidencyAliased');
  AddToResult(aFeatures.variableMultisampleRate,                  'VariableMultisampleRate');
  AddToResult(aFeatures.inheritedQueries,                         'InheritedQueries');
end;

function vkoMakeString(const aProperties:TVkPhysicalDeviceProperties; const aPrintLimits, aPrintSparseProps:Boolean):String;

  function UUIDtoStr:String;
  var i:Integer;
  begin
    result:='';
    for i:=low(aProperties.pipelineCacheUUID) to High(aProperties.pipelineCacheUUID) do begin
      if (result <> '') then
        result:=result+':';
      result:=result+IntToHex(aProperties.pipelineCacheUUID[i], 2);
    end;
  end;

  function VKtoString(const i:Int64):String;
  begin
    result:=IntToStr(i);
  end;

  function VKtoString(const e:Extended):String;
  begin
    result:=Format('%.3f', [e]);
  end;

  function VKtoString(const arr:array of TVkUInt32):String;
  var i:Integer;
  begin
    result:='';
    for i:=low(arr) to high(arr) do begin
      if (i <> low(arr)) then
        result:=result+'; ';
      result:=result+VKtoString(arr[i]);
    end;
  end;

  function VKtoString(const arr:array of TVkFloat):String;
  var i:Integer;
  begin
    result:='';
    for i:=low(arr) to high(arr) do begin
      if (i <> low(arr)) then
        result:=result+'; ';
      result:=result+VKtoString(arr[i]);
    end;
  end;

  function VKtoString(const aType:TVkPhysicalDeviceType):String;
  begin
    case aType of
      VK_PHYSICAL_DEVICE_TYPE_OTHER:result:='Other';
      VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU:result:='IntegratedGPU';
      VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU:result:='DiscreteGPU';
      VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU:result:='VirtualGPU';
      VK_PHYSICAL_DEVICE_TYPE_CPU:result:='CPU';
    else
      result:='Unknown';
    end;
  end;

  function VKtoString(const aChar:PVkChar):String;
  begin
    result:=String(aChar);
  end;

begin
  result:=Format(
    'ApiVersion:%x (v%d.%d.%d)'+sLineBreak +
    'DriverVersion:%s'+sLineBreak +
    'VendorID:%s'+sLineBreak +
    'DeviceID:%s'+sLineBreak +
    'DeviceType:%s'+sLineBreak +
    'deviceName:%s'+sLineBreak +
    'PipelineCacheUUID:%s'+sLineBreak
  ,[
    aProperties.apiVersion, vkGetVersionMajor(aProperties.apiVersion), vkGetVersionMinor(aProperties.apiVersion), vkgetVersionPatch(aProperties.apiVersion),
    VKtoString(aProperties.driverVersion),
    VKtoString(aProperties.vendorID),
    VKtoString(aProperties.deviceID),
    VKtoString(aProperties.deviceType),
    VKtoString(@aProperties.deviceName[0]),
    UUIDtoStr
  ]);
  if aPrintLimits then begin
    result:=result+Format(
    'Limits:'+sLineBreak +
    '  MaxImageDimension1D:%s'+sLineBreak +
    '  MaxImageDimension2D:%s'+sLineBreak +
    '  MaxImageDimension3D:%s'+sLineBreak +
    '  MaxImageDimensionCube:%s'+sLineBreak +
    '  MaxImageArrayLayers:%s'+sLineBreak +
    '  MaxTexelBufferElements:%s'+sLineBreak +
    '  MaxUniformBufferRange:%s'+sLineBreak +
    '  MaxStorageBufferRange:%s'+sLineBreak +
    '  MaxPushConstantsSize:%s'+sLineBreak +
    '  MaxMemoryAllocationCount:%s'+sLineBreak +
    '  MaxSamplerAllocationCount:%s'+sLineBreak +
    '  BufferImageGranularity:%s'+sLineBreak +
    '  SparseAddressSpaceSize:%s'+sLineBreak +
    '  MaxBoundDescriptorSets:%s'+sLineBreak +
    '  MaxPerStageDescriptorSamplers:%s'+sLineBreak +
    '  MaxPerStageDescriptorUniformBuffers:%s'+sLineBreak +
    '  MaxPerStageDescriptorStorageBuffers:%s'+sLineBreak +
    '  MaxPerStageDescriptorSampledImages:%s'+sLineBreak +
    '  MaxPerStageDescriptorStorageImages:%s'+sLineBreak +
    '  MaxPerStageDescriptorInputAttachments:%s'+sLineBreak +
    '  MaxPerStageResources:%s'+sLineBreak +
    '  MaxDescriptorSetSamplers:%s'+sLineBreak +
    '  MaxDescriptorSetUniformBuffers:%s'+sLineBreak +
    '  MaxDescriptorSetUniformBuffersDynamic:%s'+sLineBreak +
    '  MaxDescriptorSetStorageBuffers:%s'+sLineBreak +
    '  MaxDescriptorSetStorageBuffersDynamic:%s'+sLineBreak +
    '  MaxDescriptorSetSampledImages:%s'+sLineBreak +
    '  MaxDescriptorSetStorageImages:%s'+sLineBreak +
    '  MaxDescriptorSetInputAttachments:%s'+sLineBreak +
    '  MaxVertexInputAttributes:%s'+sLineBreak +
    '  MaxVertexInputBindings:%s'+sLineBreak +
    '  MaxVertexInputAttributeOffset:%s'+sLineBreak +
    '  MaxVertexInputBindingStride:%s'+sLineBreak +
    '  MaxVertexOutputComponents:%s'+sLineBreak +
    '  MaxTessellationGenerationLevel:%s'+sLineBreak +
    '  MaxTessellationPatchSize:%s'+sLineBreak +
    '  MaxTessellationControlPerVertexInputComponents:%s'+sLineBreak +
    '  MaxTessellationControlPerVertexOutputComponents:%s'+sLineBreak +
    '  MaxTessellationControlPerPatchOutputComponents:%s'+sLineBreak +
    '  MaxTessellationControlTotalOutputComponents:%s'+sLineBreak +
    '  MaxTessellationEvaluationInputComponents:%s'+sLineBreak +
    '  MaxTessellationEvaluationOutputComponents:%s'+sLineBreak +
    '  MaxGeometryShaderInvocations:%s'+sLineBreak +
    '  MaxGeometryInputComponents:%s'+sLineBreak +
    '  MaxGeometryOutputComponents:%s'+sLineBreak +
    '  MaxGeometryOutputVertices:%s'+sLineBreak +
    '  MaxGeometryTotalOutputComponents:%s'+sLineBreak +
    '  MaxFragmentInputComponents:%s'+sLineBreak +
    '  MaxFragmentOutputAttachments:%s'+sLineBreak +
    '  MaxFragmentDualSrcAttachments:%s'+sLineBreak +
    '  MaxFragmentCombinedOutputResources:%s'+sLineBreak +
    '  MaxComputeSharedMemorySize:%s'+sLineBreak +
    '  MaxComputeWorkGroupCount:%s'+sLineBreak +
    '  MaxComputeWorkGroupInvocations:%s'+sLineBreak +
    '  MaxComputeWorkGroupSize:%s'+sLineBreak +
    '  SubPixelPrecisionBits:%s'+sLineBreak +
    '  SubTexelPrecisionBits:%s'+sLineBreak +
    '  MipmapPrecisionBits:%s'+sLineBreak +
    '  MaxDrawIndexedIndexValue:%s'+sLineBreak +
    '  MaxDrawIndirectCount:%s'+sLineBreak +
    '  MaxSamplerLodBias:%s'+sLineBreak +
    '  MaxSamplerAnisotropy:%s'+sLineBreak +
    '  MaxViewports:%s'+sLineBreak +
    '  MaxViewportDimensions:%s'+sLineBreak +
    '  ViewportBoundsRange:%s'+sLineBreak +
    '  ViewportSubPixelBits:%s'+sLineBreak +
    '  MinMemoryMapAlignment:%s'+sLineBreak +
    '  MinTexelBufferOffsetAlignment:%s'+sLineBreak +
    '  MinUniformBufferOffsetAlignment:%s'+sLineBreak +
    '  MinStorageBufferOffsetAlignment:%s'+sLineBreak +
    '  MinTexelOffset:%s'+sLineBreak +
    '  MaxTexelOffset:%s'+sLineBreak +
    '  MinTexelGatherOffset:%s'+sLineBreak +
    '  MaxTexelGatherOffset:%s'+sLineBreak +
    '  MinInterpolationOffset:%s'+sLineBreak +
    '  MaxInterpolationOffset:%s'+sLineBreak +
    '  SubPixelInterpolationOffsetBits:%s'+sLineBreak +
    '  MaxFramebufferWidth:%s'+sLineBreak +
    '  MaxFramebufferHeight:%s'+sLineBreak +
    '  MaxFramebufferLayers:%s'+sLineBreak +
    '  FramebufferColorSampleCounts:%s'+sLineBreak +
    '  FramebufferDepthSampleCounts:%s'+sLineBreak +
    '  FramebufferStencilSampleCounts:%s'+sLineBreak +
    '  FramebufferNoAttachmentsSampleCounts:%s'+sLineBreak +
    '  MaxColorAttachments:%s'+sLineBreak +
    '  SampledImageColorSampleCounts:%s'+sLineBreak +
    '  SampledImageIntegerSampleCounts:%s'+sLineBreak +
    '  SampledImageDepthSampleCounts:%s'+sLineBreak +
    '  SampledImageStencilSampleCounts:%s'+sLineBreak +
    '  StorageImageSampleCounts:%s'+sLineBreak +
    '  MaxSampleMaskWords:%s'+sLineBreak +
    '  TimestampComputeAndGraphics:%s'+sLineBreak +
    '  TimestampPeriod:%s'+sLineBreak +
    '  MaxClipDistances:%s'+sLineBreak +
    '  MaxCullDistances:%s'+sLineBreak +
    '  MaxCombinedClipAndCullDistances:%s'+sLineBreak +
    '  DiscreteQueuePriorities:%s'+sLineBreak +
    '  PointSizeRange:%s'+sLineBreak +
    '  LineWidthRange:%s'+sLineBreak +
    '  PointSizeGranularity:%s'+sLineBreak +
    '  LineWidthGranularity:%s'+sLineBreak +
    '  StrictLines:%s'+sLineBreak +
    '  StandardSampleLocations:%s'+sLineBreak +
    '  OptimalBufferCopyOffsetAlignment:%s'+sLineBreak +
    '  OptimalBufferCopyRowPitchAlignment:%s'+sLineBreak +
    '  NonCoherentAtomSize:%s'+sLineBreak
    ,[
      VKtoString(aProperties.limits.maxImageDimension1D),
      VKtoString(aProperties.limits.maxImageDimension2D),
      VKtoString(aProperties.limits.maxImageDimension3D),
      VKtoString(aProperties.limits.maxImageDimensionCube),
      VKtoString(aProperties.limits.maxImageArrayLayers),
      VKtoString(aProperties.limits.maxTexelBufferElements),
      VKtoString(aProperties.limits.maxUniformBufferRange),
      VKtoString(aProperties.limits.maxStorageBufferRange),
      VKtoString(aProperties.limits.maxPushConstantsSize),
      VKtoString(aProperties.limits.maxMemoryAllocationCount),
      VKtoString(aProperties.limits.maxSamplerAllocationCount),
      VKtoString(aProperties.limits.bufferImageGranularity),
      VKtoString(aProperties.limits.sparseAddressSpaceSize),
      VKtoString(aProperties.limits.maxBoundDescriptorSets),
      VKtoString(aProperties.limits.maxPerStageDescriptorSamplers),
      VKtoString(aProperties.limits.maxPerStageDescriptorUniformBuffers),
      VKtoString(aProperties.limits.maxPerStageDescriptorStorageBuffers),
      VKtoString(aProperties.limits.maxPerStageDescriptorSampledImages),
      VKtoString(aProperties.limits.maxPerStageDescriptorStorageImages),
      VKtoString(aProperties.limits.maxPerStageDescriptorInputAttachments),
      VKtoString(aProperties.limits.maxPerStageResources),
      VKtoString(aProperties.limits.maxDescriptorSetSamplers),
      VKtoString(aProperties.limits.maxDescriptorSetUniformBuffers),
      VKtoString(aProperties.limits.maxDescriptorSetUniformBuffersDynamic),
      VKtoString(aProperties.limits.maxDescriptorSetStorageBuffers),
      VKtoString(aProperties.limits.maxDescriptorSetStorageBuffersDynamic),
      VKtoString(aProperties.limits.maxDescriptorSetSampledImages),
      VKtoString(aProperties.limits.maxDescriptorSetStorageImages),
      VKtoString(aProperties.limits.maxDescriptorSetInputAttachments),
      VKtoString(aProperties.limits.maxVertexInputAttributes),
      VKtoString(aProperties.limits.maxVertexInputBindings),
      VKtoString(aProperties.limits.maxVertexInputAttributeOffset),
      VKtoString(aProperties.limits.maxVertexInputBindingStride),
      VKtoString(aProperties.limits.maxVertexOutputComponents),
      VKtoString(aProperties.limits.maxTessellationGenerationLevel),
      VKtoString(aProperties.limits.maxTessellationPatchSize),
      VKtoString(aProperties.limits.maxTessellationControlPerVertexInputComponents),
      VKtoString(aProperties.limits.maxTessellationControlPerVertexOutputComponents),
      VKtoString(aProperties.limits.maxTessellationControlPerPatchOutputComponents),
      VKtoString(aProperties.limits.maxTessellationControlTotalOutputComponents),
      VKtoString(aProperties.limits.maxTessellationEvaluationInputComponents),
      VKtoString(aProperties.limits.maxTessellationEvaluationOutputComponents),
      VKtoString(aProperties.limits.maxGeometryShaderInvocations),
      VKtoString(aProperties.limits.maxGeometryInputComponents),
      VKtoString(aProperties.limits.maxGeometryOutputComponents),
      VKtoString(aProperties.limits.maxGeometryOutputVertices),
      VKtoString(aProperties.limits.maxGeometryTotalOutputComponents),
      VKtoString(aProperties.limits.maxFragmentInputComponents),
      VKtoString(aProperties.limits.maxFragmentOutputAttachments),
      VKtoString(aProperties.limits.maxFragmentDualSrcAttachments),
      VKtoString(aProperties.limits.maxFragmentCombinedOutputResources),
      VKtoString(aProperties.limits.maxComputeSharedMemorySize),
      VKtoString(aProperties.limits.maxComputeWorkGroupCount),
      VKtoString(aProperties.limits.maxComputeWorkGroupInvocations),
      VKtoString(aProperties.limits.maxComputeWorkGroupSize),
      VKtoString(aProperties.limits.subPixelPrecisionBits),
      VKtoString(aProperties.limits.subTexelPrecisionBits),
      VKtoString(aProperties.limits.mipmapPrecisionBits),
      VKtoString(aProperties.limits.maxDrawIndexedIndexValue),
      VKtoString(aProperties.limits.maxDrawIndirectCount),
      VKtoString(aProperties.limits.maxSamplerLodBias),
      VKtoString(aProperties.limits.maxSamplerAnisotropy),
      VKtoString(aProperties.limits.maxViewports),
      VKtoString(aProperties.limits.maxViewportDimensions),
      VKtoString(aProperties.limits.viewportBoundsRange),
      VKtoString(aProperties.limits.viewportSubPixelBits),
      VKtoString(aProperties.limits.minMemoryMapAlignment),
      VKtoString(aProperties.limits.minTexelBufferOffsetAlignment),
      VKtoString(aProperties.limits.minUniformBufferOffsetAlignment),
      VKtoString(aProperties.limits.minStorageBufferOffsetAlignment),
      VKtoString(aProperties.limits.minTexelOffset),
      VKtoString(aProperties.limits.maxTexelOffset),
      VKtoString(aProperties.limits.minTexelGatherOffset),
      VKtoString(aProperties.limits.maxTexelGatherOffset),
      VKtoString(aProperties.limits.minInterpolationOffset),
      VKtoString(aProperties.limits.maxInterpolationOffset),
      VKtoString(aProperties.limits.subPixelInterpolationOffsetBits),
      VKtoString(aProperties.limits.maxFramebufferWidth),
      VKtoString(aProperties.limits.maxFramebufferHeight),
      VKtoString(aProperties.limits.maxFramebufferLayers),
      VKtoString(PVkUint32(@aProperties.limits.framebufferColorSampleCounts)^),
      VKtoString(PVkUint32(@aProperties.limits.framebufferDepthSampleCounts)^),
      VKtoString(PVkUint32(@aProperties.limits.framebufferStencilSampleCounts)^),
      VKtoString(PVkUint32(@aProperties.limits.framebufferNoAttachmentsSampleCounts)^),
      VKtoString(aProperties.limits.maxColorAttachments),
      VKtoString(PVkUint32(@aProperties.limits.sampledImageColorSampleCounts)^),
      VKtoString(PVkUint32(@aProperties.limits.sampledImageIntegerSampleCounts)^),
      VKtoString(PVkUint32(@aProperties.limits.sampledImageDepthSampleCounts)^),
      VKtoString(PVkUint32(@aProperties.limits.sampledImageStencilSampleCounts)^),
      VKtoString(PVkUint32(@aProperties.limits.storageImageSampleCounts)^),
      VKtoString(aProperties.limits.maxSampleMaskWords),
      VKtoString(aProperties.limits.timestampComputeAndGraphics),
      VKtoString(aProperties.limits.timestampPeriod),
      VKtoString(aProperties.limits.maxClipDistances),
      VKtoString(aProperties.limits.maxCullDistances),
      VKtoString(aProperties.limits.maxCombinedClipAndCullDistances),
      VKtoString(aProperties.limits.discreteQueuePriorities),
      VKtoString(aProperties.limits.pointSizeRange),
      VKtoString(aProperties.limits.lineWidthRange),
      VKtoString(aProperties.limits.pointSizeGranularity),
      VKtoString(aProperties.limits.lineWidthGranularity),
      VKtoString(aProperties.limits.strictLines),
      VKtoString(aProperties.limits.standardSampleLocations),
      VKtoString(aProperties.limits.optimalBufferCopyOffsetAlignment),
      VKtoString(aProperties.limits.optimalBufferCopyRowPitchAlignment),
      VKtoString(aProperties.limits.nonCoherentAtomSize)
    ]);
  end;
  if aPrintSparseProps then begin
    result:=result+Format(
      'SparseProperties:'+sLineBreak +
      '  ResidencyStandard2DBlockShape:%s'+sLineBreak +
      '  ResidencyStandard2DMultisampleBlockShape:%s'+sLineBreak +
      '  ResidencyStandard3DBlockShape:%s'+sLineBreak +
      '  ResidencyAlignedMipSize:%s'+sLineBreak +
      '  ResidencyNonResidentStrict:%s'+sLineBreak,
    [
      VKtoString(aProperties.sparseProperties.residencyStandard2DBlockShape),
      VKtoString(aProperties.sparseProperties.residencyStandard2DMultisampleBlockShape),
      VKtoString(aProperties.sparseProperties.residencyStandard3DBlockShape),
      VKtoString(aProperties.sparseProperties.residencyAlignedMipSize),
      VKtoString(aProperties.sparseProperties.residencyNonResidentStrict)
    ]);
  end;
end;

function vkoMakeString(const aMemoryProperties:TVkPhysicalDeviceMemoryProperties):String;
var i:Integer;
begin
  result:='Types:'+sLineBreak;
  for i:=0 to aMemoryProperties.memoryTypeCount-1 do begin
    result:=result+Format(
      '  Type %d:'+sLineBreak +
      '    PropertyFlags:0x%.16X'+sLineBreak +
      '    HeapIndex:%d (0x%2:.16X)'+sLineBreak,
      [i, PVkUint32(@aMemoryProperties.memoryTypes[i].propertyFlags)^, aMemoryProperties.memoryTypes[i].heapIndex]);
  end;
  result:=result+'Heaps:'+sLineBreak;
  for i:=0 to aMemoryProperties.memoryHeapCount-1 do begin
    result:=result+Format(
      '  Heap %d:'+sLineBreak +
      '    Size:%d (0x%1:.16X)'+sLineBreak +
      '    Flags:0x%.16X'+sLineBreak,
      [i, aMemoryProperties.memoryHeaps[i].size, PVkUint32(@aMemoryProperties.memoryHeaps[i].flags)^]);
  end;
end;

function vkuGetMemoryTypeIndex(const aMemoryProperties:TVkPhysicalDeviceMemoryProperties;
                               aMemoryTypeBits:TVkUInt32;
                               const aProperty:TVkMemoryPropertyFlagBit):TVkUInt32;
var
  i:Integer;
begin
  result:=0;
  for i:=0 to VK_MAX_MEMORY_TYPES-1 do begin
    if ((aMemoryTypeBits and 1)=1) and
       (aProperty in aMemoryProperties.memoryTypes[i].propertyFlags) then
    begin
      result:=i;
      break;
    end;
    aMemoryTypeBits:=aMemoryTypeBits shr 1;
  end;
end;

//-------------------------------------------------

function AllocateMemoryCallback(aUserData:PVkVoid;
                                aSize:TVkSize;
                                aAlignment:TVkSize;
                                aScope:TVkSystemAllocationScope):PVkVoid; {$IFDEF VK_CDECL}cdecl{$ELSE}stdcall{$ENDIF};
begin
  result:=TvkObj_AllocationHandler(aUserData).AllocateMemory(aSize, aAlignment, aScope);
end;

function ReallocateMemoryCallback(aUserData:PVkVoid;
                                  aOriginal:PVkVoid;
                                  aSize:TVkSize;
                                  aAlignment:TVkSize;
                                  aScope:TVkSystemAllocationScope):PVkVoid; {$IFDEF VK_CDECL}cdecl{$ELSE}stdcall{$ENDIF};
begin
  result:=TvkObj_AllocationHandler(aUserData).ReallocateMemory(aOriginal, aSize, aAlignment, aScope);
end;

procedure FreeMemoryCallback(aUserData:PVkVoid; aMemory:PVkVoid); {$IFDEF VK_CDECL}cdecl{$ELSE}stdcall{$ENDIF};
begin
 TvkObj_AllocationHandler(aUserData).FreeMemory(aMemory);
end;

procedure InternalAllocationCallback(aUserData:PVkVoid;
                                     aSize:TVkSize;
                                     aType:TVkInternalAllocationType;
                                     aScope:TVkSystemAllocationScope); {$IFDEF VK_CDECL}cdecl{$ELSE}stdcall{$ENDIF};
begin
 TvkObj_AllocationHandler(aUserData).InternalAllocationNotification(aSize, aType, aScope);
end;

procedure InternalFreeCallback(aUserData:PVkVoid;
                               aSize:TVkSize;
                               aType:TVkInternalAllocationType;
                               aScope:TVkSystemAllocationScope); {$IFDEF VK_CDECL}cdecl{$ELSE}stdcall{$ENDIF};
begin
 TvkObj_AllocationHandler(aUserData).InternalFreeNotification(aSize, aType, aScope);
end;

function vkuDebugReportCallback(flags:TVkDebugReportFlagsEXT;
                                objectType:TVkDebugReportObjectTypeEXT;
                                object_:TVkUInt64;
                                location:TVkSize;
                                messageCode:TVkInt32;
                                const pLayerPrefix:PVkChar;
                                const pMessage:PVkChar;
                                pUserData:PVkVoid):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
var ret:Boolean;
begin
  ret:=TvkObj_DebugReporter(pUserData).DebugMessage(
    flags,
    objectType,
    Object_,
    location,
    messageCode,
    String(pLayerPrefix),
    String(pMessage));
  if ret
    then result:=VK_TRUE
    else result:=VK_FALSE;
end;


//==================== TvkObj_ErrorException ==================================

constructor TvkObj_ErrorException.Create(const msg:string; const aError:TVkResult);
begin
  inherited Create(msg);
  fError:=aError;
end;

//============= TvkObj_InvalidFuncPtrException ==================================

constructor TvkObj_InvalidFuncPtrException.Create(const aFunctionName:String; const aExtensionName:String);
var s:String;
begin
  fFunctionName:=aFunctionName;
  fExtensionName:=aExtensionName;
  s:='did you load the vulkan core commands?';
  if (aExtensionName <> '') then
    s:='did you enabled/load the '+fExtensionName+' extension?';
  inherited Create('invalid function pointer:'+fFunctionName+'; '+s);
end;

//============= TvkObj_Handle ==================================
constructor TvkObj_Handle.Create;
begin
  inherited Create;
  fOwnsHandle:=true;
end;

//============= TvkObj_AllocHandle ==================================

procedure TvkObj_AllocHandle.SetAllocCallbacks(const aAllocCallbacks:TVkAllocationCallbacks);
begin
  if not Assigned(fAllocCallbacks) then
    new(fAllocCallbacks);
  fAllocCallbacks^:=aAllocCallbacks;
end;

procedure TvkObj_AllocHandle.SetAllocCallbacks(const aAllocCallbacks:PVkAllocationCallbacks);
begin
  if Assigned(aAllocCallbacks) then
    SetAllocCallbacks(aAllocCallbacks^);
end;

procedure TvkObj_AllocHandle.DisposeAllocCallbacks;
begin
  if not Assigned(fAllocCallbacks) then
    exit;
  Dispose(fAllocCallbacks);
  fAllocCallbacks:=nil;
end;

constructor TvkObj_AllocHandle.Create;
begin
  inherited Create;
  fAllocCallbacks:=nil;
end;

destructor TvkObj_AllocHandle.Destroy;
begin
  inherited Destroy;
  DisposeAllocCallbacks;
end;

//============= TvkObj_ListBase ==================================

function TvkObj_ListBase.GetItem(const aIndex:Integer):T;
begin
  if (aIndex < low(fItems)) or (aIndex>high(fItems)) then
    raise TvkObj_Exception.CreateFmt('index (%d) out of range (%d:%d)', [aIndex, Low(fItems), High(fItems)]);
  result:=fItems[aIndex];
end;

function TvkObj_ListBase.GetData:TPointerType;
begin
  result:=@fItems[0];
end;

function TvkObj_ListBase.GetLength:Integer;
begin
  result:=System.Length(fItems);
end;

procedure TvkObj_ListBase.SetLength(aValue:Integer);
var i:Integer;
begin
  if (aValue < 0) then
    aValue:=0;
  i:=System.Length(fItems);
  while (i>aValue) do begin
    FreeItem(fItems[i-1]);
    dec(i);
  end;
  i:=System.Length(fItems)+1;
  System.SetLength(fItems, aValue);
  while (i <= aValue) do begin
    InitItem(fItems[i-1]);
    inc(i);
  end;
end;

procedure TvkObj_ListBase.InitItem(var aItem:T);
begin
  FillByte(aItem, SizeOf(aItem), 0);
end;

procedure TvkObj_ListBase.FreeItem(var aItem:T);
begin
end;

constructor TvkObj_ListBase.Create;
begin
  inherited Create;
  System.SetLength(fItems, 0);
end;

destructor TvkObj_ListBase.Destroy;
begin
  SetLength(0);
  inherited Destroy;
end;

//============= TvkObj_List ==================================

procedure TvkObj_List.SetItem(const aIndex:Integer; aValue:T);
begin
  if (aIndex < low(fItems)) or (aIndex>high(fItems)) then
    raise TvkObj_Exception.CreateFmt('index (%d) out of range (%d:%d)', [aIndex, Low(fItems), High(fItems)]);
  fItems[aIndex]:=aValue;
end;

procedure TvkObj_List.SetData(const aData:TPointerType; const aCount:Integer);
var i:Integer;
begin
  if Assigned(aData) then begin
    SetLength(aCount);
    for i:=low(fItems) to high(fItems) do
      fItems[i]:=(aData+i)^;
  end else
    SetLength(0);
end;

//============= TvkObj_ObjList ==================================

procedure TvkObj_ObjList.InitItem(var aItem:T);
begin
  inherited InitItem(aItem);
  aItem:=T.Create;
end;

procedure TvkObj_ObjList.FreeItem(var aItem:T);
begin
  FreeAndNil(aItem);
  inherited FreeItem(aItem);
end;

//============= TvkObj_AllocationHandler ==================================

function TvkObj_AllocationHandler.AllocateMemory(const aSize:TVkSize; const aAlignment:TVkSize; const aScope:TVkSystemAllocationScope):PVkVoid;
begin
  result:=System.GetMemory(aSize);
end;

function TvkObj_AllocationHandler.ReallocateMemory(const aOriginal:PVkVoid; const aSize:TVkSize; const aAlignment:TVkSize; const aScope:TVkSystemAllocationScope):PVkVoid;
begin
  result:=System.ReAllocMemory(aOriginal, aSize);
end;

procedure TvkObj_AllocationHandler.FreeMemory(const aMemory:PVkVoid);
begin
  System.FreeMemory(aMemory);
end;

procedure TvkObj_AllocationHandler.InternalAllocationNotification(const aSize:TVkSize; const aType:TVkInternalAllocationType; const aScope:TVkSystemAllocationScope);
begin
   // ....
end;

procedure TvkObj_AllocationHandler.InternalFreeNotification(const aSize:TVkSize; const aType:TVkInternalAllocationType; const aScope:TVkSystemAllocationScope);
begin
   // ...
end;

function TvkObj_AllocationHandler.GetStructure:TVkAllocationCallbacks;
begin
  FillByte(result, SizeOf(result), 0);
  result.pUserData:=self;
  result.pfnAllocation:=@AllocateMemoryCallback;
  result.pfnReallocation:=@ReallocateMemoryCallback;
  result.pfnFree:=@FreeMemoryCallback;
  result.pfnInternalAllocation:=@InternalAllocationCallback;
  result.pfnInternalFree:=@InternalFreeCallback;
end;

//============= TvkObj_DeviceMemory ==================================     

constructor TvkObj_DeviceMemory.Create(const aAllocInfo:TVkMemoryAllocateInfo; const aDeviceFunctions:TVkDeviceFunctions;
  const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fDeviceFunctions:=aDeviceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(aAllocInfo);
end;

constructor TvkObj_DeviceMemory.Create(const aHandle:TVkDeviceMemory; const aOwnsHandle:Boolean;
  const aDeviceFunctions:TVkDeviceFunctions; const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=aHandle;
  fDeviceFunctions:=aDeviceFunctions;
  OwnsHandle:=aOwnsHandle;
  SetAllocCallbacks(aAllocCallbacks);
end;

destructor TvkObj_DeviceMemory.Destroy;
begin
  if OwnsHandle and (fHandle <> VK_INVALID_NDP_HANDLE) then
     fDeviceFunctions.vkFreeMemory(fDeviceFunctions.aaDevice, fHandle, AllocCallbacks);
  fHandle:=VK_INVALID_NDP_HANDLE;
  inherited Destroy;
end;

procedure TvkObj_DeviceMemory.CreateHandle(const aAllocInfo:TVkMemoryAllocateInfo);
var err:TVkResult;
begin
  if not Assigned(fDeviceFunctions.vkAllocateMemory) then
    raise TvkObj_InvalidFuncPtrException.Create('vkAllocateMemory');
  err:= fDeviceFunctions.vkAllocateMemory(fDeviceFunctions.aaDevice, @aAllocInfo, AllocCallbacks, @fHandle);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to allocate device memory', err);
end;

function TvkObj_DeviceMemory.Map(const aOffset:TVkDeviceSize; aSize:TVkDeviceSize; aFlags:TVkMemoryMapFlags):PVkVoid;
var err:TVkResult;
begin
  err:= fDeviceFunctions.vkMapMemory(fDeviceFunctions.aaDevice, fHandle, aOffset, aSize, aFlags, @result);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to map device memory', err);
end;

procedure TvkObj_DeviceMemory.Unmap;
begin
   fDeviceFunctions.vkUnmapMemory(fDeviceFunctions.aaDevice, fHandle);
end;

 //============= TvkObj_Buffer ================================== 

constructor TvkObj_Buffer.Create(const aCreateInfo:TVkBufferCreateInfo;
                                 const aDeviceFunctions:TVkDeviceFunctions;
                                 const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fDeviceFunctions:=aDeviceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(aCreateInfo);
end;

constructor TvkObj_Buffer.Create(const aHandle:TVkBuffer;
                                 const aOwnsHandle:Boolean;
                                 const aDeviceFunctions:TVkDeviceFunctions;
                                 const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=aHandle;
  fDeviceFunctions:=aDeviceFunctions;
  OwnsHandle:=aOwnsHandle;
  SetAllocCallbacks(aAllocCallbacks);
end;

destructor TvkObj_Buffer.Destroy;
begin
  if OwnsHandle and (fHandle <> VK_INVALID_NDP_HANDLE) then
     fDeviceFunctions.vkDestroyBuffer(fDeviceFunctions.aaDevice, fHandle, AllocCallbacks);
  fHandle:=VK_INVALID_NDP_HANDLE;
  FreeAndNil(fBufferMemory);
  inherited Destroy;
end;

procedure TvkObj_Buffer.CreateHandle(const aCreateInfo:TVkBufferCreateInfo);
var err:TVkResult;
begin
  err:= fDeviceFunctions.vkCreateBuffer(fDeviceFunctions.aaDevice, @aCreateInfo, AllocCallbacks, @fHandle);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to create buffer', err);
end;

procedure TvkObj_Buffer.BindMemory(const aMemory:TVkDeviceMemory; const aOffset:TVkDeviceSize);
var err:TVkResult;
begin
  err:= fDeviceFunctions.vkBindBufferMemory(fDeviceFunctions.aaDevice, fHandle, aMemory, aOffset);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to bind buffer memory', err);
  fMemory:=aMemory;
end;

function TvkObj_Buffer.GetMemoryRequirements:TVkMemoryRequirements;
begin
   fDeviceFunctions.vkGetBufferMemoryRequirements(fDeviceFunctions.aaDevice, fHandle, @result);
end;

procedure TvkObj_Buffer.AllocateAndBindBufferMemory(const aMemoryProperties:TVkPhysicalDeviceMemoryProperties;
                                                   aAllocCallbacks:PVkAllocationCallbacks);
var
  i:Integer;
  bits:TVkUInt32;
  MemReq:TVkMemoryRequirements;
  Mem:TVkDeviceMemory;
  MemAllocInfo:TVkMemoryAllocateInfo;
  err:TVkResult;
begin
  MemReq:=GetMemoryRequirements;

  FillByte(MemAllocInfo, SizeOf(MemAllocInfo), 0);
  MemAllocInfo.sType:=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
  MemAllocInfo.pNext:=nil;
  MemAllocInfo.allocationSize:=MemReq.size;
  MemAllocInfo.memoryTypeIndex:=vkuGetMemoryTypeIndex(
    aMemoryProperties,
    MemReq.memoryTypeBits,
    VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT);

  if not Assigned(aAllocCallbacks) then
    aAllocCallbacks:=AllocCallbacks;
  err:= fDeviceFunctions.vkAllocateMemory(fDeviceFunctions.aaDevice, @MemAllocInfo, aAllocCallbacks, @Mem);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to allocate buffer memory', err);
  fBufferMemory:=TvkObj_DeviceMemory.Create(Mem, true, fDeviceFunctions, aAllocCallbacks);
  BindMemory(fBufferMemory.Handle, 0);
end;

//============= TvkObj_BufferFactory ==================================

function TvkObj_BufferFactory.GetStructure:TVkBufferCreateInfo;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType :=VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
  result.pNext :=nil;
  result.flags :=fFlags;
  result.size:=fSize;
  result.usage :=fUsage;
  result.sharingMode:=fSharingMode;
  result.queueFamilyIndexCount:=fQueueFamilyIndices.Length;
  result.pQueueFamilyIndices:=fQueueFamilyIndices.PData;
end;

procedure TvkObj_BufferFactory.SetStructure(const aData:TVkBufferCreateInfo);
var i:Integer;
begin
  fFlags:=aData.flags;
  fSize:=aData.size;
  fUsage:=aData.usage;
  fSharingMode:=aData.sharingMode;
  fQueueFamilyIndices.SetData(aData.pQueueFamilyIndices, aData.queueFamilyIndexCount);
end;

function TvkObj_BufferFactory.CreateBuffer:TvkObj_Buffer;
begin
  result:=TvkObj_Buffer.Create(GetStructure, DeviceFunctions, AllocCallbacks);
end;

procedure TvkObj_BufferFactory.AfterConstruction;
begin
  inherited AfterConstruction;
  fQueueFamilyIndices:=TvkInt32List.Create;
end;

procedure TvkObj_BufferFactory.BeforeDestruction;
begin
  FreeAndNil(fQueueFamilyIndices);
  inherited BeforeDestruction;
end;

//============= TvkObj_BufferView ==================================

constructor TvkObj_BufferView.Create(const aCreateInfo:TVkBufferViewCreateInfo; const aDeviceFunctions:TVkDeviceFunctions;
  const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fDeviceFunctions:=aDeviceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(aCreateInfo);
end;

constructor TvkObj_BufferView.Create(const aHandle:TVkBufferView; const aOwnsHandle:Boolean;
  const aDeviceFunctions:TVkDeviceFunctions; const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=aHandle;
  fDeviceFunctions:=aDeviceFunctions;
  OwnsHandle:=aOwnsHandle;
  SetAllocCallbacks(aAllocCallbacks);
end;

destructor TvkObj_BufferView.Destroy;
begin
  if OwnsHandle and (fHandle <> VK_INVALID_NDP_HANDLE) then
     fDeviceFunctions.vkDestroyBufferView(fDeviceFunctions.aaDevice, fHandle, AllocCallbacks);
  fHandle:=VK_INVALID_NDP_HANDLE;
  inherited Destroy;
end;

procedure TvkObj_BufferView.CreateHandle(const aCreateInfo:TVkBufferViewCreateInfo);
var err:TVkResult;
begin
  err:= fDeviceFunctions.vkCreateBufferView(fDeviceFunctions.aaDevice, @aCreateInfo, AllocCallbacks, @fHandle);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to create buffer view', err);
end;

//============= TvkObj_BufferViewFactory ==================================

function TvkObj_BufferViewFactory.GetStructure:TVkBufferViewCreateInfo;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.buffer:=fBuffer;
  result.format:=fFormat;
  result.offset:=fOffset;
  result.range:=fRange;
end;

procedure TvkObj_BufferViewFactory.SetStructure(const aData:TVkBufferViewCreateInfo);
begin
  fFlags:=aData.flags;
  fBuffer:=aData.buffer;
  fFormat:=aData.format;
  fOffset:=aData.offset;
  fRange:=aData.range;
end;

function TvkObj_BufferViewFactory.CreateBufferView:TvkObj_BufferView;
begin
  result:=TvkObj_BufferView.Create(GetStructure, DeviceFunctions, AllocCallbacks);
end;

//============= TvkObj_RenderPassBeginInfo ==================================

function TvkObj_RenderPassBeginInfo.GetStructure:TVkRenderPassBeginInfo;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
  result.pNext:=nil;
  result.renderPass:=RenderPass;
  result.framebuffer:=Framebuffer;
  result.renderArea:=RenderArea;
  result.clearValueCount:=fClearValues.Length;
  result.pClearValues:=fClearValues.PData;
end;

procedure TvkObj_RenderPassBeginInfo.SetStructure(const aData:TVkRenderPassBeginInfo);
begin
  RenderPass:=aData.renderPass;
  Framebuffer:=aData.framebuffer;
  RenderArea:=aData.renderArea;
  ClearValues.SetData(aData.pClearValues, aData.clearValueCount);
end;

constructor TvkObj_RenderPassBeginInfo.Create;
begin
  inherited Create;
  fClearValues:=TVkClearValueList.Create;
end;

destructor TvkObj_RenderPassBeginInfo.Destroy;
begin
  FreeAndNil(fClearValues);
  inherited Destroy;
end;

//============= TvkObj_BeginRenderPassHelper ==================================

procedure TvkObj_BeginRenderPassHelper.Execute(const aContents:TVkSubpassContents);
var info:TVkRenderPassBeginInfo;
begin
  info:=fRenderPassBeginInfo.GetStructure;
   fDeviceFunctions.vkCmdBeginRenderPass(fCommandBuffer, @info, aContents);
end;

constructor TvkObj_BeginRenderPassHelper.Create(const aCommandBuffer:TVkCommandBuffer; const aDeviceFunctions:TVkDeviceFunctions);
begin
  inherited Create;
  fCommandBuffer:=aCommandBuffer;
  fDeviceFunctions:=aDeviceFunctions;
  fRenderPassBeginInfo:=TvkObj_RenderPassBeginInfo.Create;
end;

destructor TvkObj_BeginRenderPassHelper.Destroy;
begin
  FreeAndNil(fRenderPassBeginInfo);
  inherited Destroy;
end;

//============= TvkObj_CommandBuffer ==================================

procedure TvkObj_CommandBuffer.BeginCommand;
var
  Info:TVkCommandBufferBeginInfo;
  err:TVkResult;
begin
  FillByte(Info, SizeOf(Info), 0);
  Info.sType:=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
  Info.pNext:=nil;
  Info.flags:=[];
  Info.pInheritanceInfo:=nil;
  err:= fDeviceFunctions.vkBeginCommandBuffer(fHandle, @Info);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to begin command buffer', err);
end;

procedure TvkObj_CommandBuffer.EndCommand;
var err:TVkResult;
begin
  err:= fDeviceFunctions.vkEndCommandBuffer(fHandle);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to end command buffer', err);
end;

procedure TvkObj_CommandBuffer.PipelineBarrier(const aSrcStageMask:TVkPipelineStageFlags;
                                               const aDstStageMask:TVkPipelineStageFlags;
                                               const aDependencyFlags:TVkDependencyFlags;
                                               const aMemoryBarriers:array of TVkMemoryBarrier;
                                               const aBufferMemoryBarriers:array of TVkBufferMemoryBarrier;
                                               const aImageMemoryBarriers:array of TVkImageMemoryBarrier);
begin
  DeviceFunctions.vkCmdPipelineBarrier(
    fHandle,
    aSrcStageMask,
    aDstStageMask,
    aDependencyFlags,
    Length(aMemoryBarriers),        @aMemoryBarriers[0],
    Length(aBufferMemoryBarriers),  @aBufferMemoryBarriers[0],
    Length(aImageMemoryBarriers),   @aImageMemoryBarriers[0]);
end;

function TvkObj_CommandBuffer.BeginRenderPass:TvkObj_BeginRenderPassHelper;
begin
  result:=TvkObj_BeginRenderPassHelper.Create(fHandle, fDeviceFunctions);
end;

procedure TvkObj_CommandBuffer.EndRenderPass;
begin
   fDeviceFunctions.vkCmdEndRenderPass(fHandle);
end;

procedure TvkObj_CommandBuffer.SetViewport(const aFirstViewport:TVkUInt32; const aViewports:array of TVkViewport);
begin
   fDeviceFunctions.vkCmdSetViewport(
    fHandle,
    aFirstViewport,
    Length(aViewports),
    @aViewports[0]);
end;

procedure TvkObj_CommandBuffer.SetScissors(const aFirstScissor:TVkUInt32; const aScissorss:array of TVkRect2D);
begin
   fDeviceFunctions.vkCmdSetScissor(
    fHandle,
    aFirstScissor,
    Length(aScissorss),
    @aScissorss[0]);
end;

procedure TvkObj_CommandBuffer.BindDescriptorSet(const aBindPoint:TVkPipelineBindPoint;
                                                 const aLayout:TVkPipelineLayout;
                                                 const aFirstSet:TVkUInt32;
                                                 const aDescriptorSets:array of TVkDescriptorSet;
                                                 const aDynamicOffsets:array of TVkUInt32);
var
  DescSetCount, DynOffsetCount:Integer;
  DescSets:PVkDescriptorSet;
  DynOffsets:PVkUint32;
begin
  DescSetCount:=Length(aDescriptorSets);
  DynOffsetCount:=Length(aDynamicOffsets);
  if (DescSetCount>0)
    then DescSets:=@aDescriptorSets[0]
    else DescSets:=nil;
  if (DynOffsetCount>0)
    then DynOffsets:=@aDynamicOffsets[0]
    else DynOffsets:=nil;

   fDeviceFunctions.vkCmdBindDescriptorSets(
    fHandle,
    aBindPoint,
    aLayout,
    aFirstSet,
    DescSetCount,
    DescSets,
    DynOffsetCount,
    DynOffsets);
end;

procedure TvkObj_CommandBuffer.BindPipeline(const aBindPoint:TVkPipelineBindPoint; const aPipeline:TVkPipeline);
begin
   fDeviceFunctions.vkCmdBindPipeline(fHandle, aBindPoint, aPipeline);
end;

procedure TvkObj_CommandBuffer.BindVertexBuffers(const aFirstBinding:TVkUInt32;
                                                 const aBuffers:array of TVkBuffer;
                                                 const aOffsets:array of TVkDeviceSize);
begin
  if (Length(aBuffers) <> Length(aOffsets)) then
    raise TvkObj_Exception.Create('aBuffers and aOffsets must have the same number of elements');
   fDeviceFunctions.vkCmdBindVertexBuffers(
    fHandle,
    aFirstBinding,
    Length(aBuffers),
    @aBuffers[0],
    @aOffsets[0]);
end;

procedure TvkObj_CommandBuffer.BindIndexBuffers(const aBuffer:TVkBuffer; const aOffset:TVkDeviceSize;
  const aIndexType:TVkIndexType);
begin
   fDeviceFunctions.vkCmdBindIndexBuffer(
    fHandle,
    aBuffer,
    aOffset,
    aIndexType);
end;

procedure TvkObj_CommandBuffer.DrawIndexed(const aIndexCount:TVkUInt32; const aInstanceCount:TVkUInt32;
  const aFirstIndex:TVkUInt32; const aVertexOffset:TVkInt32; const aFirstInstance:TVkUInt32);
begin
   fDeviceFunctions.vkCmdDrawIndexed(
    fHandle,
    aIndexCount,
    aInstanceCount,
    aFirstIndex,
    aVertexOffset,
    aFirstInstance);
end;

constructor TvkObj_CommandBuffer.Create(const aHandle:TVkCommandBuffer; const aOwnsHandle:Boolean;
  const aCommandPool:TVkCommandPool; const aDeviceFunctions:TVkDeviceFunctions);
begin
  inherited Create;
  fHandle:=aHandle;
  fCommandPool:=aCommandPool;
  fDeviceFunctions:=aDeviceFunctions;
  OwnsHandle:=aOwnsHandle;
end;

destructor TvkObj_CommandBuffer.Destroy;
begin
  if OwnsHandle and (fHandle <> VK_INVALID_HANDLE) then
     fDeviceFunctions.vkFreeCommandBuffers(fDeviceFunctions.aaDevice, fCommandPool, 1, @fHandle);
  fHandle:=VK_INVALID_HANDLE;
  inherited Destroy;
end;

//============= TvkObj_CommandPool ==================================
constructor TvkObj_CommandPool.Create(const aCreateInfo:TVkCommandPoolCreateInfo;
                                      const aDeviceFunctions:TVkDeviceFunctions;
                                      const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fDeviceFunctions:=aDeviceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(aCreateInfo);
end;

constructor TvkObj_CommandPool.Create(const aHandle:TVkCommandPool;
                                      const aOwnsHandle:Boolean;
                                      const aDeviceFunctions:TVkDeviceFunctions;
                                      const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=aHandle;
  fDeviceFunctions:=aDeviceFunctions;
  OwnsHandle:=aOwnsHandle;
  SetAllocCallbacks(aAllocCallbacks);
end;

destructor TvkObj_CommandPool.Destroy;
begin
  if OwnsHandle and (fHandle <> VK_INVALID_NDP_HANDLE) then
     fDeviceFunctions.vkDestroyCommandPool(fDeviceFunctions.aaDevice, fHandle, AllocCallbacks);
  fHandle:=VK_INVALID_NDP_HANDLE;
  inherited Destroy;
end;

procedure TvkObj_CommandPool.CreateHandle(const aCreateInfo:TVkCommandPoolCreateInfo);
 var err:TVkResult;
begin
  err:= fDeviceFunctions.vkCreateCommandPool(fDeviceFunctions.aaDevice, @aCreateInfo, AllocCallbacks, @fHandle);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to create command pool', err);
end;

function TvkObj_CommandPool.AllocateCommandBuffer(const aLevel:TVkCommandBufferLevel):TVkCommandBuffer;
begin
  result:=AllocateCommandBuffers(aLevel, 1)[0];
end;

function TvkObj_CommandPool.AllocateCommandBuffers(const aLevel:TVkCommandBufferLevel; const aCount:Integer):TVkCommandBufferArr;
var
  AllocInfo:TVkCommandBufferAllocateInfo;
  err:TVkResult;
begin
  FillByte(AllocInfo, SizeOf(AllocInfo), 0);
  AllocInfo.sType:=VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
  AllocInfo.pNext:=nil;
  AllocInfo.commandPool:=fHandle;
  AllocInfo.level:=aLevel;
  AllocInfo.commandBufferCount:=aCount;
  SetLength(result, aCount);
  err:= fDeviceFunctions.vkAllocateCommandBuffers(fDeviceFunctions.aaDevice, @AllocInfo, @result[0]);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to create command buffers', err);
end;

procedure TvkObj_CommandPool.FreeCommandBuffer(const aBuffer:TVkCommandBuffer);
begin
  FreeCommandBuffers(TVkCommandBufferArr.Create(aBuffer));
end;

procedure TvkObj_CommandPool.FreeCommandBuffers(const aBuffers:TVkCommandBufferArr);
begin
   fDeviceFunctions.vkFreeCommandBuffers(fDeviceFunctions.aaDevice, fHandle, Length(aBuffers), @aBuffers[0]);
end;

function TvkObj_CommandPoolFactory.GetStructure:TVkCommandPoolCreateInfo;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.queueFamilyIndex:=fQueueFamilyIndex;
end;

procedure TvkObj_CommandPoolFactory.SetStructure(const aData:TVkCommandPoolCreateInfo);
begin
  fFlags:=aData.flags;
  fQueueFamilyIndex:=aData.queueFamilyIndex;
end;

function TvkObj_CommandPoolFactory.CreateCommandPool:TvkObj_CommandPool;
begin
  result:=TvkObj_CommandPool.Create(GetStructure, DeviceFunctions, AllocCallbacks);
end;

//============= TvkObj_DebugReporter ==================================

function TvkObj_DebugReporter.DebugMessage(aFlags:TVkDebugReportFlagsEXT; aObjectType:TVkDebugReportObjectTypeEXT;
                                          aObject:TVkUInt64; aLocation:TVkSize; aMessageCode:TVkInt32; aLayerPrefix:String; aMessage:String):Boolean;
begin
  result:=true;
end;

constructor TvkObj_DebugReporter.Create(const aInstanceFunctions:TVkInstanceFunctions; const aAllocHandler:TvkObj_AllocationHandler);
var
  CreateInfo:TVkDebugReportCallbackCreateInfoEXT;
  err:TVkResult;
begin
  inherited Create;
  if Assigned(aAllocHandler) then
    SetAllocCallbacks(aAllocHandler.GetStructure);
  fInstanceFunctions:=aInstanceFunctions;

  FillByte(CreateInfo, SizeOf(CreateInfo), 0);
  CreateInfo.sType:=VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT;
  CreateInfo.pNext:=nil;
  Pointer(CreateInfo.pfnCallback):=@vkuDebugReportCallback; //=== ct9999 ================
  CreateInfo.pUserData:=self;
  CreateInfo.flags:=[
    VK_DEBUG_REPORT_INFORMATION_BIT_EXT,
    VK_DEBUG_REPORT_WARNING_BIT_EXT,
    VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT,
    VK_DEBUG_REPORT_ERROR_BIT_EXT,
    VK_DEBUG_REPORT_DEBUG_BIT_EXT
  ];

 //if fInstanceFunctions.vkCreateDebugReportCallbackEXT=nil then exit;

  err:=fInstanceFunctions.vkCreateDebugReportCallbackEXT(fInstanceFunctions.aaInstance, @CreateInfo, AllocCallbacks, @fHandle);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to create debug report handle', err);
end;

destructor TvkObj_DebugReporter.Destroy;
begin
  if (fHandle <> VK_INVALID_NDP_HANDLE) then
    fInstanceFunctions.vkDestroyDebugReportCallbackEXT(fInstanceFunctions.aaInstance, fHandle, AllocCallbacks);
  fHandle:=VK_INVALID_NDP_HANDLE;
  inherited Destroy;
end;

//============= TvkObj_ConsoleDebugReporter ==================================

function TvkObj_ConsoleDebugReporter.DebugMessage(aFlags:TVkDebugReportFlagsEXT;
                                                  aObjectType:TVkDebugReportObjectTypeEXT;
                                                  aObject:TVkUInt64;
                                                  aLocation:TVkSize;
                                                  aMessageCode:TVkInt32;
                                                  aLayerPrefix:String;
                                                  aMessage:String):Boolean;
begin
  WriteLn(Format('    DBG - [%03d] %10s:%s', [ Integer(aFlags), aLayerPrefix, aMessage ]));
end;

//============= TvkObj_DescriptorPool ==================================

procedure TvkObj_DescriptorPool.CreateHandle(const aCreateInfo:TVkDescriptorPoolCreateInfo);
 var err:TVkResult;
begin
  if not Assigned(fDeviceFunctions.vkCreateDescriptorPool) then
    raise TvkObj_InvalidFuncPtrException.Create('vkCreateDescriptorPool');
  err:=fDeviceFunctions.vkCreateDescriptorPool(fDeviceFunctions.aaDevice, @aCreateInfo, AllocCallbacks, @fHandle);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to create descriptor pool', err);
end;

function TvkObj_DescriptorPool.AllocateDescriptorSet(const aDescriptorSetLayout:TVkDescriptorSetLayout):TVkDescriptorSet;
begin
  result:=AllocateDescriptorSets([ aDescriptorSetLayout ])[0];
end;

function TvkObj_DescriptorPool.AllocateDescriptorSets(const aDescriptorSetLayouts:array of TVkDescriptorSetLayout):TVkDescriptorSetArr;
var
  AllocInfo:TVkDescriptorSetAllocateInfo;
  err:TVkResult;
begin
  if not Assigned(fDeviceFunctions.vkAllocateDescriptorSets) then
    raise TvkObj_InvalidFuncPtrException.Create('vkAllocateDescriptorSets');
  FillByte(AllocInfo, SizeOf(AllocInfo), 0);
  AllocInfo.sType:=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
  AllocInfo.pNext:=nil;
  AllocInfo.descriptorPool:=fHandle;
  AllocInfo.descriptorSetCount:=Length(aDescriptorSetLayouts);
  AllocInfo.pSetLayouts:=@aDescriptorSetLayouts[0];
  SetLength(result, AllocInfo.descriptorSetCount);
  err:=fDeviceFunctions.vkAllocateDescriptorSets(fDeviceFunctions.aaDevice, @AllocInfo, @result[0]);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to allocate descriptor sets', err);
end;

procedure TvkObj_DescriptorPool.FreeDescriptorSet(const aDescriptorSet:TVkDescriptorSet);
begin
  FreeDescriptorSets(TVkDescriptorSetArr.Create(aDescriptorSet));
end;

procedure TvkObj_DescriptorPool.FreeDescriptorSets(const aDescriptorSets:TVkDescriptorSetArr);
var err:TVkResult;
begin
  if not Assigned(fDeviceFunctions.vkFreeDescriptorSets) then
    raise TvkObj_InvalidFuncPtrException.Create('vkFreeDescriptorSets');
  err:=fDeviceFunctions.vkFreeDescriptorSets(fDeviceFunctions.aaDevice, fHandle, Length(aDescriptorSets), @aDescriptorSets[0]);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to free descriptor sets', err);
end;

constructor TvkObj_DescriptorPool.Create(const aCreateInfo:TVkDescriptorPoolCreateInfo;
  const aDeviceFunctions:TVkDeviceFunctions; const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fDeviceFunctions:=aDeviceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(aCreateInfo);
end;

constructor TvkObj_DescriptorPool.Create(const aHandle:TVkDescriptorPool; const aOwnsHandle:Boolean;
  const aDeviceFunctions:TVkDeviceFunctions; const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=aHandle;
  fDeviceFunctions:=aDeviceFunctions;
  OwnsHandle:=aOwnsHandle;
  SetAllocCallbacks(aAllocCallbacks);
end;

destructor TvkObj_DescriptorPool.Destroy;
begin
  if OwnsHandle and (fHandle <> VK_INVALID_NDP_HANDLE) then
    fDeviceFunctions.vkDestroyDescriptorPool(fDeviceFunctions.aaDevice, fHandle, AllocCallbacks);
  fHandle:=VK_INVALID_NDP_HANDLE;
  inherited Destroy;
end;


function TvkObj_DescriptorPoolSize.GetStructure:TVkDescriptorPoolSize;
begin
  FillByte(result, SizeOf(result), 0);
  result.type_:=DescriptorType;
  result.descriptorCount:=DescriptorCount;
end;

procedure TvkObj_DescriptorPoolSize.SetStructure(const aData:TVkDescriptorPoolSize);
begin
  DescriptorType:=aData.type_;
  DescriptorCount:=aData.descriptorCount;
end;

function TvkObj_DescriptorPoolFactory.GetStructure:TVkDescriptorPoolCreateInfo;
var i:Integer;
begin
  SetLength(fvkPoolSizes, fPoolSizes.Length);
  for i:=low(fvkPoolSizes) to high(fvkPoolSizes) do
    fvkPoolSizes[i]:=fPoolSizes[i].GetStructure;

  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=Flags;
  result.maxSets:=MaxSets;
  result.poolSizeCount:=Length(fvkPoolSizes);
  result.pPoolSizes:=@fvkPoolSizes[0];
end;

procedure TvkObj_DescriptorPoolFactory.SetStructure(const aData:TVkDescriptorPoolCreateInfo);
var i:Integer;
begin
  if Assigned(aData.pPoolSizes) then begin
    fPoolSizes.Length:=aData.poolSizeCount;
    for i:=0 to fPoolSizes.Length-1 do
      fPoolSizes[i].SetStructure((aData.pPoolSizes+i)^);
  end;

  Flags:=aData.flags;
  MaxSets:=aData.maxSets;
end;

function TvkObj_DescriptorPoolFactory.CreateDescriptorPool:TvkObj_DescriptorPool;
begin
  result:=TvkObj_DescriptorPool.Create(GetStructure, DeviceFunctions, AllocCallbacks);
end;

procedure TvkObj_DescriptorPoolFactory.AfterConstruction;
begin
  inherited AfterConstruction;
  fPoolSizes:=TvkObj_DescriptorPoolSizeList.Create;
end;

procedure TvkObj_DescriptorPoolFactory.BeforeDestruction;
begin
  FreeAndNil(fPoolSizes);
  inherited BeforeDestruction;
end;

//============= TvkObj_DescriptorSet ==================================

constructor TvkObj_DescriptorSet.Create(const aHandle:TVkDescriptorSet;
                                        const aOwnsHandle:Boolean;
                                        const aDescriptorPool:TVkDescriptorPool;
                                        const aDeviceFunctions:TVkDeviceFunctions);
begin
  inherited Create;
  fHandle:=aHandle;
  OwnsHandle:=aOwnsHandle;
  fDescriptorPool:=aDescriptorPool;
  fDeviceFunctions:=aDeviceFunctions;
end;

destructor TvkObj_DescriptorSet.Destroy;
begin
  if (fHandle <> VK_INVALID_NDP_HANDLE) and OwnsHandle then
    fDeviceFunctions.vkFreeDescriptorSets(fDeviceFunctions.aaDevice, fDescriptorPool, 1, @fHandle);
  fHandle:=VK_INVALID_NDP_HANDLE;
  inherited Destroy;
end;


procedure TvkObj_DescriptorSetLayout.CreateHandle(const aCreateInfo:TVkDescriptorSetLayoutCreateInfo);
var err:TVkResult;
begin
  if not Assigned(fDeviceFunctions.vkCreateDescriptorSetLayout) then
    raise TvkObj_InvalidFuncPtrException.Create('vkCreateDescriptorSetLayout');
  err:=fDeviceFunctions.vkCreateDescriptorSetLayout(fDeviceFunctions.aaDevice, @aCreateInfo, AllocCallbacks, @fHandle);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to create descriptor set layout', err);
end;

constructor TvkObj_DescriptorSetLayout.Create(const aCreateInfo:TVkDescriptorSetLayoutCreateInfo;
  const aDeviceFunctions:TVkDeviceFunctions; const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fDeviceFunctions:=aDeviceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(aCreateInfo);
end;

constructor TvkObj_DescriptorSetLayout.Create(const aHandle:TVkDescriptorSetLayout; const aOwnsHandle:Boolean;
  const aDeviceFunctions:TVkDeviceFunctions; const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=aHandle;
  fDeviceFunctions:=aDeviceFunctions;
  OwnsHandle:=aOwnsHandle;
  SetAllocCallbacks(aAllocCallbacks);
end;

destructor TvkObj_DescriptorSetLayout.Destroy;
begin
  if OwnsHandle and (fHandle <> VK_INVALID_NDP_HANDLE) then
    fDeviceFunctions.vkDestroyDescriptorSetLayout(fDeviceFunctions.aaDevice, fHandle, AllocCallbacks);
  fHandle:=VK_INVALID_NDP_HANDLE;
  inherited Destroy;
end;


function TvkObj_DescriptorSetLayoutBinding.GetStructure:TVkDescriptorSetLayoutBinding;
begin
  FillByte(result, SizeOf(result), 0);
  result.binding:=Binding;
  result.descriptorType:=DescriptorType;
  result.descriptorCount:=DescriptorCount;
  result.stageFlags:=StageFlags;
  result.pImmutableSamplers:=fImmutableSamplers.PData;

  if (fImmutableSamplers.Length <> 0) and
     (fImmutableSamplers.Length <> DescriptorCount) then
     raise TvkObj_Exception.Create('''ImmutableSamplers'' must be either empty or contains ''DescriptorCount'' number of elements');
end;

procedure TvkObj_DescriptorSetLayoutBinding.SetStructure(const aData:TVkDescriptorSetLayoutBinding);
var i:Integer;
begin
  Binding:=aData.binding;
  DescriptorType:=aData.descriptorType;
  StageFlags:=aData.stageFlags;
  fImmutableSamplers.SetData(aData.pImmutableSamplers, aData.descriptorCount);
end;

procedure TvkObj_DescriptorSetLayoutBinding.AfterConstruction;
begin
  inherited AfterConstruction;
  fImmutableSamplers:=TVkSamplerList.Create;
end;

procedure TvkObj_DescriptorSetLayoutBinding.BeforeDestruction;
begin
  FreeAndNil(fImmutableSamplers);
  inherited BeforeDestruction;
end;

function TvkObj_DescriptorSetLayoutFactory.GetStructure:TVkDescriptorSetLayoutCreateInfo;
var i:Integer;
begin
  SetLength(fvkBindings, fBindings.Length);
  for i:=low(fvkBindings) to high(fvkBindings) do
    fvkBindings[i]:=fBindings[i].GetStructure;

  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.bindingCount:=Length(fvkBindings);
  result.pBindings:=@fvkBindings[0];
end;

procedure TvkObj_DescriptorSetLayoutFactory.SetStructure(const aData:TVkDescriptorSetLayoutCreateInfo);
var i:Integer;
begin
  fFlags:=aData.flags;
  fBindings.Length:=aData.bindingCount;
  for i:=0 to fBindings.Length do
    fBindings[i].SetStructure((aData.pBindings+i)^);
end;

function TvkObj_DescriptorSetLayoutFactory.CreateDescriptorSetLayout:TvkObj_DescriptorSetLayout;
begin
  result:=TvkObj_DescriptorSetLayout.Create(GetStructure, DeviceFunctions, AllocCallbacks);
end;

procedure TvkObj_DescriptorSetLayoutFactory.AfterConstruction;
begin
  inherited AfterConstruction;
  fBindings:=TvkObj_DescriptorSetLayoutBindingArr.Create;
end;

procedure TvkObj_DescriptorSetLayoutFactory.BeforeDestruction;
begin
  FreeAndNil(fBindings);
  inherited BeforeDestruction;
end;

//============= TvkObj_DescriptorImageInfo ==================================

function TvkObj_DescriptorImageInfo.GetStructure:TVkDescriptorImageInfo;
begin
  FillByte(result, SizeOf(result), 0);
  result.sampler:=Sampler;
  result.imageView:=ImageView;
  result.imageLayout:=ImageLayout;
end;

procedure TvkObj_DescriptorImageInfo.SetStructure(const aData:TVkDescriptorImageInfo);
begin
  Sampler:=aData.sampler;
  ImageView:=aData.imageView;
  ImageLayout:=aData.imageLayout;
end;

//============= TvkObj_DescriptorBufferInfo ==================================

function TvkObj_DescriptorBufferInfo.GetStructure:TVkDescriptorBufferInfo;
begin
  FillByte(result, SizeOf(result), 0);
  result.buffer:=Buffer;
  result.offset:=Offset;
  result.range:=Range;
end;

procedure TvkObj_DescriptorBufferInfo.SetStructure(const aData:TVkDescriptorBufferInfo);
begin
  Buffer:=aData.buffer;
  Offset:=aData.offset;
  Range:=aData.range;
end;

//============= TvkObj_WriteDescriptorSet ==================================

function TvkObj_WriteDescriptorSet.GetStructure:TVkWriteDescriptorSet;
 var c, i:Integer;
begin
  c:=fImageInfos.Length;
  if (c < fBufferInfos.Length) then
    c:=fBufferInfos.Length;
  if (c < fTexelBufferViews.Length) then
    c:=fTexelBufferViews.Length;

  SetLength(fvkImageInfos, fImageInfos.Length);
  if (fImageInfos.Length <> 0) and (fImageInfos.Length <> c) then
    raise TvkObj_Exception.Create('ImageInfos, BufferInfos and TexelBufferViews must be empty or must have the same size!');
  for i:=low(fvkImageInfos) to high(fvkImageInfos) do
    fvkImageInfos[i]:=fImageInfos[i].GetStructure;

  SetLength(fvkBufferInfos, fBufferInfos.Length);
  if (fBufferInfos.Length <> 0) and (fBufferInfos.Length <> c) then
    raise TvkObj_Exception.Create('ImageInfos, BufferInfos and TexelBufferViews must be empty or must have the same size!');
  for i:=low(fvkBufferInfos) to high(fvkBufferInfos) do
    fvkBufferInfos[i]:=fBufferInfos[i].GetStructure;

  if (fTexelBufferViews.Length <> 0) and (fTexelBufferViews.Length <> c) then
    raise TvkObj_Exception.Create('ImageInfos, BufferInfos and TexelBufferViews must be empty or must have the same size!');

  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
  result.pNext:=nil;
  result.dstSet:=DstSet;
  result.dstBinding:=DstBinding;
  result.dstArrayElement:=DstArrayElement;
  result.descriptorCount:=c;
  result.descriptorType:=DescriptorType;
  result.pImageInfo:=@fvkImageInfos[0];
  result.pBufferInfo:=@fvkBufferInfos[0];
  result.pTexelBufferView:=fTexelBufferViews.PData;
end;

procedure TvkObj_WriteDescriptorSet.SetStructure(const aData:TVkWriteDescriptorSet);
var i:Integer;
begin
  if Assigned(aData.pImageInfo) then begin
    fImageInfos.Length:=aData.descriptorCount;
    for i:=0 to fImageInfos.Length-1 do
      fImageInfos[i].SetStructure((aData.pImageInfo+i)^);
  end;

  if Assigned(aData.pBufferInfo) then begin
    fBufferInfos.Length:=aData.descriptorCount;
    for i:=0 to fBufferInfos.Length-1 do
      fBufferInfos[i].SetStructure((aData.pBufferInfo+i)^);
  end;

  fTexelBufferViews.SetData(aData.pTexelBufferView, aData.descriptorCount);

  DstSet:=aData.dstSet;
  DstBinding:=aData.dstBinding;
  DstArrayElement:=aData.dstArrayElement;
  DescriptorType:=aData.descriptorType;
end;

procedure TvkObj_WriteDescriptorSet.AfterConstruction;
begin
  inherited AfterConstruction;
  fImageInfos:=TvkObj_DescriptorImageInfoList.Create;
  fBufferInfos:=TvkObj_DescriptorBufferInfoList.Create;
  fTexelBufferViews:=TVkBufferViewList.Create;
end;

procedure TvkObj_WriteDescriptorSet.BeforeDestruction;
begin
  FreeAndNil(fImageInfos);
  FreeAndNil(fBufferInfos);
  FreeAndNil(fTexelBufferViews);
  inherited BeforeDestruction;
end;

//============= TvkObj_CopyDescriptorSet ==================================

function TvkObj_CopyDescriptorSet.GetStructure:TVkCopyDescriptorSet;
begin
  result.sType:=VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET;
  result.pNext:=nil;
  result.srcSet:=SrcSet;
  result.srcBinding:=SrcBinding;
  result.srcArrayElement:=SrcArrayElement;
  result.dstSet:=DstSet;
  result.dstBinding:=DstBinding;
  result.dstArrayElement:=DstArrayElement;
  result.descriptorCount:=DescriptorCount;
end;

procedure TvkObj_CopyDescriptorSet.SetStructure(const aData:TVkCopyDescriptorSet);
begin
  SrcSet:=aData.srcSet;
  SrcBinding:=aData.srcBinding;
  SrcArrayElement:=aData.srcArrayElement;
  DstSet:=aData.dstSet;
  DstBinding:=aData.dstBinding;
  DstArrayElement:=aData.dstArrayElement;
  DescriptorCount:=aData.descriptorCount;
end;

//============= TvkObj_UpdateDescriptorSetHelper ==================================

procedure TvkObj_UpdateDescriptorSetHelper.Execute;
var
  i:Integer;
  vkWriteDescriptorSets:array of TVkWriteDescriptorSet;
  vkCopyDescriptorSets:array of TVkCopyDescriptorSet;
begin
  SetLength(vkWriteDescriptorSets, fWriteDescriptorSets.Length);
  for i:=low(vkWriteDescriptorSets) to high(vkWriteDescriptorSets) do
    vkWriteDescriptorSets[i]:=fWriteDescriptorSets[i].GetStructure;

  SetLength(vkCopyDescriptorSets, fCopyDescriptorSets.Length);
  for i:=low(vkCopyDescriptorSets) to high(vkCopyDescriptorSets) do
    vkCopyDescriptorSets[i]:=fCopyDescriptorSets[i].GetStructure;

  fDeviceFunctions.vkUpdateDescriptorSets(
    fDeviceFunctions.aaDevice,
    Length(vkWriteDescriptorSets),
    @vkWriteDescriptorSets[0],
    Length(vkCopyDescriptorSets),
    @vkCopyDescriptorSets[0]);
end;

constructor TvkObj_UpdateDescriptorSetHelper.Create(const aDeviceFunctions:TVkDeviceFunctions);
begin
  inherited Create;
  fDeviceFunctions:=aDeviceFunctions;
  fWriteDescriptorSets:=TvkObj_WriteDescriptorSetList.Create;
  fCopyDescriptorSets:=TvkObj_CopyDescriptorSetList.Create;
end;

destructor TvkObj_UpdateDescriptorSetHelper.Destroy;
begin
  FreeAndNil(fCopyDescriptorSets);
  FreeAndNil(fWriteDescriptorSets);
  inherited Destroy;
end;

//============= TvkObj_Device ==================================

procedure TvkObj_Device.CreateHandle(const aPhyDevice:TVkPhysicalDevice; const aCreateInfo:PVkDeviceCreateInfo);
var err:TVkResult;
begin
  if not Assigned(fInstanceFunctions.vkCreateDevice) then
    raise TvkObj_InvalidFuncPtrException.Create('vkCreateDevice');
  err:=fInstanceFunctions.vkCreateDevice(aPhyDevice, aCreateInfo, AllocCallbacks, @fHandle);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to create device', err);
  fDeviceFunctions:=vkoLoadDeviceFunctions(fInstanceFunctions.aaInstance, fHandle);
  if not Assigned(fDeviceFunctions) then
    raise TvkObj_Exception.Create('unable to load device commands');
end;

function TvkObj_Device.GetQueue(const aQueueFamilyIndex:TVkUInt32; const aQueueIndex:TVkUInt32):TVkQueue;
begin
  if not Assigned(fInstanceFunctions.vkGetDeviceQueue) then
    raise TvkObj_InvalidFuncPtrException.Create('vkGetDeviceQueue');
  fDeviceFunctions.vkGetDeviceQueue(fHandle, aQueueFamilyIndex, aQueueIndex, @result);
  if (result=VK_INVALID_HANDLE) then
    raise TvkObj_Exception.Create('unable to get device queue');
end;

function TvkObj_Device.AllocateMemory(const aSize:TVkUInt32; const aMemoryTypeIndex:TVkUInt32;
  aAllocCallbacks:PVkAllocationCallbacks):TVkDeviceMemory;
var
  AllocInfo:TVkMemoryAllocateInfo;
  err:TVkResult;
begin
  FillByte(AllocInfo, SizeOf(AllocInfo), 0);
  AllocInfo.sType:=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
  AllocInfo.pNext:=nil;
  AllocInfo.allocationSize:=aSize;
  AllocInfo.memoryTypeIndex:=aMemoryTypeIndex;
  if not Assigned(aAllocCallbacks) then
    aAllocCallbacks:=AllocCallbacks;
  err:=fDeviceFunctions.vkAllocateMemory(fHandle, @AllocInfo, aAllocCallbacks, @result);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to allocate device memory', err);
end;

procedure TvkObj_Device.FreeMemory(const aMemory:TVkDeviceMemory; aAllocCallbacks:PVkAllocationCallbacks);
begin
  if not Assigned(aAllocCallbacks) then
    aAllocCallbacks:=AllocCallbacks;
  fDeviceFunctions.vkFreeMemory(fHandle, aMemory, aAllocCallbacks);
end;

procedure TvkObj_Device.WaitIdle;
var err:TVkResult;
begin
  err:=fDeviceFunctions.vkDeviceWaitIdle(fHandle);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to wait for device idle', err);
end;

function TvkObj_Device.UpdateDescriptorSet:TvkObj_UpdateDescriptorSetHelper;
begin
  result:=TvkObj_UpdateDescriptorSetHelper.Create(fDeviceFunctions);
end;

constructor TvkObj_Device.Create(const aPhysicalDevice:TVkPhysicalDevice; const aCreateInfo:TVkDeviceCreateInfo;
  const aInstanceFunctions:TVkInstanceFunctions; const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=0;
  fInstanceFunctions:=aInstanceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(aPhysicalDevice, @aCreateInfo);
end;

constructor TvkObj_Device.Create(const aHandle:TVkDevice; const aOwnsHandle:Boolean;
  const aInstanceFunctions:TVkInstanceFunctions; const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=aHandle;
  fInstanceFunctions:=aInstanceFunctions;
  OwnsHandle:=aOwnsHandle;
  SetAllocCallbacks(aAllocCallbacks);
end;

destructor TvkObj_Device.Destroy;
begin
  if OwnsHandle and (fHandle <> VK_INVALID_HANDLE) then
    fDeviceFunctions.vkDestroyDevice(fHandle, AllocCallbacks);
  fHandle:=VK_INVALID_HANDLE;
  FreeAndNil(fDeviceFunctions);
  inherited Destroy;
end;

function TvkObj_QueueCreateInfo.GetStructure:TVkDeviceQueueCreateInfo;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=Flags;
  result.queueFamilyIndex:=QueueFamilyIndex;
  result.queueCount:=fQueuePriorities.Length;
  result.pQueuePriorities:=fQueuePriorities.PData;
end;

procedure TvkObj_QueueCreateInfo.SetStructure(const aData:TVkDeviceQueueCreateInfo);
var i:Integer;
begin
  Flags:=aData.flags;
  QueueFamilyIndex:=aData.queueFamilyIndex;
  fQueuePriorities.Length:=aData.queueCount;
  for i:=0 to fQueuePriorities.Length-1 do
    fQueuePriorities[i]:=(aData.pQueuePriorities+i)^;
end;

procedure TvkObj_QueueCreateInfo.AfterConstruction;
begin
  inherited AfterConstruction;
  fQueuePriorities:=TVkFloatList.Create;
end;

procedure TvkObj_QueueCreateInfo.BeforeDestruction;
begin
  FreeAndNil(fQueuePriorities);
  inherited BeforeDestruction;
end;

function TvkObj_PhysicalDeviceFeatures.GetStructure:TVkPhysicalDeviceFeatures;
begin
  FillByte(result, SizeOf(result), 0);
  result.robustBufferAccess  :=RobustBufferAccess;
  result.fullDrawIndexUint32 :=FullDrawIndexUint32;
  result.imageCubeArray :=ImageCubeArray;
  result.independentBlend:=IndependentBlend;
  result.geometryShader :=GeometryShader;
  result.tessellationShader  :=TessellationShader;
  result.sampleRateShading:=SampleRateShading;
  result.dualSrcBlend:=DualSrcBlend;
  result.logicOp   :=LogicOp;
  result.multiDrawIndirect:=MultiDrawIndirect;
  result.drawIndirectFirstInstance:=DrawIndirectFirstInstance;
  result.depthClamp:=DepthClamp;
  result.depthBiasClamp :=DepthBiasClamp;
  result.fillModeNonSolid:=FillModeNonSolid;
  result.depthBounds:=DepthBounds;
  result.wideLines :=WideLines;
  result.largePoints:=LargePoints;
  result.alphaToOne:=AlphaToOne;
  result.multiViewport  :=MultiViewport;
  result.samplerAnisotropy:=SamplerAnisotropy;
  result.textureCompressionETC2:=TextureCompressionETC2;
  result.textureCompressionASTC_LDR:=TextureCompressionASTC_LDR;
  result.textureCompressionBC:=TextureCompressionBC;
  result.occlusionQueryPrecise:=OcclusionQueryPrecise;
  result.pipelineStatisticsQuery :=PipelineStatisticsQuery;
  result.vertexPipelineStoresAndAtomics:=VertexPipelineStoresAndAtomics;
  result.fragmentStoresAndAtomics:=FragmentStoresAndAtomics;
  result.shaderTessellationAndGeometryPointSize:=ShaderTessellationAndGeometryPointSize;
  result.shaderImageGatherExtended:=ShaderImageGatherExtended;
  result.shaderStorageImageExtendedFormats:=ShaderStorageImageExtendedFormats;
  result.shaderStorageImageMultisample:=ShaderStorageImageMultisample;
  result.shaderStorageImageReadWithoutFormat:=ShaderStorageImageReadWithoutFormat;
  result.shaderStorageImageWriteWithoutFormat:=ShaderStorageImageWriteWithoutFormat;
  result.shaderUniformBufferArrayDynamicIndexing:=ShaderUniformBufferArrayDynamicIndexing;
  result.shaderSampledImageArrayDynamicIndexing:=ShaderSampledImageArrayDynamicIndexing;
  result.shaderStorageBufferArrayDynamicIndexing:=ShaderStorageBufferArrayDynamicIndexing;
  result.shaderStorageImageArrayDynamicIndexing:=ShaderStorageImageArrayDynamicIndexing;
  result.shaderClipDistance  :=ShaderClipDistance;
  result.shaderCullDistance  :=ShaderCullDistance;
  result.shaderFloat64  :=ShaderFloat64;
  result.shaderInt64:=ShaderInt64;
  result.shaderInt16:=ShaderInt16;
  result.shaderResourceResidency :=ShaderResourceResidency;
  result.shaderResourceMinLod:=ShaderResourceMinLod;
  result.sparseBinding  :=SparseBinding;
  result.sparseResidencyBuffer:=SparseResidencyBuffer;
  result.sparseResidencyImage2D:=SparseResidencyImage2D;
  result.sparseResidencyImage3D:=SparseResidencyImage3D;
  result.sparseResidency2Samples :=SparseResidency2Samples;
  result.sparseResidency4Samples :=SparseResidency4Samples;
  result.sparseResidency8Samples :=SparseResidency8Samples;
  result.sparseResidency16Samples:=SparseResidency16Samples;
  result.sparseResidencyAliased:=SparseResidencyAliased;
  result.variableMultisampleRate :=VariableMultisampleRate;
  result.inheritedQueries:=InheritedQueries;
end;

procedure TvkObj_PhysicalDeviceFeatures.SetStructure(const aData:TVkPhysicalDeviceFeatures);
begin
  RobustBufferAccess  :=aData.robustBufferAccess;
  FullDrawIndexUint32 :=aData.fullDrawIndexUint32;
  ImageCubeArray :=aData.imageCubeArray;
  IndependentBlend:=aData.independentBlend;
  GeometryShader :=aData.geometryShader;
  TessellationShader  :=aData.tessellationShader;
  SampleRateShading:=aData.sampleRateShading;
  DualSrcBlend:=aData.dualSrcBlend;
  LogicOp   :=aData.logicOp;
  MultiDrawIndirect:=aData.multiDrawIndirect;
  DrawIndirectFirstInstance:=aData.drawIndirectFirstInstance;
  DepthClamp:=aData.depthClamp;
  DepthBiasClamp :=aData.depthBiasClamp;
  FillModeNonSolid:=aData.fillModeNonSolid;
  DepthBounds:=aData.depthBounds;
  WideLines :=aData.wideLines;
  LargePoints:=aData.largePoints;
  AlphaToOne:=aData.alphaToOne;
  MultiViewport  :=aData.multiViewport;
  SamplerAnisotropy:=aData.samplerAnisotropy;
  TextureCompressionETC2:=aData.textureCompressionETC2;
  TextureCompressionASTC_LDR:=aData.textureCompressionASTC_LDR;
  TextureCompressionBC:=aData.textureCompressionBC;
  OcclusionQueryPrecise:=aData.occlusionQueryPrecise;
  PipelineStatisticsQuery :=aData.pipelineStatisticsQuery;
  VertexPipelineStoresAndAtomics:=aData.vertexPipelineStoresAndAtomics;
  FragmentStoresAndAtomics:=aData.fragmentStoresAndAtomics;
  ShaderTessellationAndGeometryPointSize:=aData.shaderTessellationAndGeometryPointSize;
  ShaderImageGatherExtended:=aData.shaderImageGatherExtended;
  ShaderStorageImageExtendedFormats:=aData.shaderStorageImageExtendedFormats;
  ShaderStorageImageMultisample:=aData.shaderStorageImageMultisample;
  ShaderStorageImageReadWithoutFormat:=aData.shaderStorageImageReadWithoutFormat;
  ShaderStorageImageWriteWithoutFormat:=aData.shaderStorageImageWriteWithoutFormat;
  ShaderUniformBufferArrayDynamicIndexing:=aData.shaderUniformBufferArrayDynamicIndexing;
  ShaderSampledImageArrayDynamicIndexing:=aData.shaderSampledImageArrayDynamicIndexing;
  ShaderStorageBufferArrayDynamicIndexing:=aData.shaderStorageBufferArrayDynamicIndexing;
  ShaderStorageImageArrayDynamicIndexing:=aData.shaderStorageImageArrayDynamicIndexing;
  ShaderClipDistance  :=aData.shaderClipDistance;
  ShaderCullDistance  :=aData.shaderCullDistance;
  ShaderFloat64  :=aData.shaderFloat64;
  ShaderInt64:=aData.shaderInt64;
  ShaderInt16:=aData.shaderInt16;
  ShaderResourceResidency :=aData.shaderResourceResidency;
  ShaderResourceMinLod:=aData.shaderResourceMinLod;
  SparseBinding  :=aData.sparseBinding;
  SparseResidencyBuffer:=aData.sparseResidencyBuffer;
  SparseResidencyImage2D:=aData.sparseResidencyImage2D;
  SparseResidencyImage3D:=aData.sparseResidencyImage3D;
  SparseResidency2Samples :=aData.sparseResidency2Samples;
  SparseResidency4Samples :=aData.sparseResidency4Samples;
  SparseResidency8Samples :=aData.sparseResidency8Samples;
  SparseResidency16Samples:=aData.sparseResidency16Samples;
  SparseResidencyAliased:=aData.sparseResidencyAliased;
  VariableMultisampleRate :=aData.variableMultisampleRate;
  InheritedQueries:=aData.inheritedQueries;
end;

function TvkObj_DeviceFactory.GetEnabledLayerNames:TStrings;
begin
  result:=fEnabledLayerNames;
end;

function TvkObj_DeviceFactory.GetEnabledExtensionNames:TStrings;
begin
  result:=fEnabledExtensionNames;
end;

function TvkObj_DeviceFactory.GetStructure:TVkDeviceCreateInfo;
var i:Integer;
begin
  if (fQueueCreateInfos.Length <= 0) then
    raise TvkObj_Exception.Create('no queue create infos assigned');

  fvkEnabledFeatures:=fEnabledFeatures.GetStructure;

  SetLength(fvkEnabledLayerNames, fEnabledLayerNames.Count);
  for i:=0 to fEnabledLayerNames.Count-1 do
    fvkEnabledLayerNames[i]:=PVkChar(fEnabledLayerNames[i]);

  SetLength(fvkEnabledExtensionNames, fEnabledExtensionNames.Count);
  for i:=0 to fEnabledExtensionNames.Count-1 do
    fvkEnabledExtensionNames[i]:=PVkChar(fEnabledExtensionNames[i]);

  SetLength(fvkQueueCreateInfos, fQueueCreateInfos.Length);
  for i:=low(fvkQueueCreateInfos) to high(fvkQueueCreateInfos) do
    fvkQueueCreateInfos[i]:=fQueueCreateInfos[i].GetStructure;

  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.queueCreateInfoCount:=Length(fvkQueueCreateInfos);
  result.pQueueCreateInfos:=@fvkQueueCreateInfos[0];
  result.enabledLayerCount:=Length(fvkEnabledLayerNames);
  result.ppEnabledLayerNames:=@fvkEnabledLayerNames[0];
  result.enabledExtensionCount:=Length(fvkEnabledExtensionNames);
  result.ppEnabledExtensionNames:=@fvkEnabledExtensionNames[0];
  result.pEnabledFeatures:=@fvkEnabledFeatures;
end;

procedure TvkObj_DeviceFactory.SetStructure(const aData:TVkDeviceCreateInfo);
var i:Integer;
begin
  if Assigned(aData.pEnabledFeatures) then
    fEnabledFeatures.SetStructure(aData.pEnabledFeatures^);

  fEnabledLayerNames.Clear;
  for i:=0 to aData.enabledLayerCount-1 do
    fEnabledLayerNames.Add(aData.ppEnabledLayerNames[i]);

  fEnabledExtensionNames.Clear;
  for i:=0 to aData.enabledExtensionCount-1 do
    fEnabledExtensionNames.Add(aData.ppEnabledExtensionNames[i]);

  fQueueCreateInfos.Length:=aData.queueCreateInfoCount;
  for i:=0 to fQueueCreateInfos.Length-1 do
    fQueueCreateInfos[i].SetStructure((aData.pQueueCreateInfos+i)^);

  fFlags:=aData.flags;
end;

function TvkObj_DeviceFactory.CreateDevice(const aPhysicalDevice:TVkPhysicalDevice):TvkObj_Device;
begin
  result:=TvkObj_Device.Create(aPhysicalDevice, GetStructure, InstanceFunctions, AllocCallbacks);
end;

procedure TvkObj_DeviceFactory.AfterConstruction;
begin
  inherited AfterConstruction;
  fEnabledFeatures:=TvkObj_PhysicalDeviceFeatures.Create;
  fEnabledLayerNames:=TStringList.Create;
  fEnabledExtensionNames:=TStringList.Create;
  fQueueCreateInfos:=TvkObj_QueueCreateInfoList.Create;
end;

procedure TvkObj_DeviceFactory.BeforeDestruction;
begin
  FreeAndNil(fQueueCreateInfos);
  FreeAndNil(fEnabledLayerNames);
  FreeAndNil(fEnabledExtensionNames);
  FreeAndNil(fEnabledFeatures);
  inherited BeforeDestruction;
end;

procedure TvkObj_Factory.FreeAllocCallbacks;
begin
  if Assigned(fAllocCallbacks) and fOwnsAllocCallbacks then
    Dispose(fAllocCallbacks);
  fAllocCallbacks:=nil;
end;

procedure TvkObj_Factory.SetAllocCallbacks(aValue:PVkAllocationCallbacks);
begin
  fAllocHandler:=nil;
  FreeAllocCallbacks;
  fAllocCallbacks:=aValue;
  fOwnsAllocCallbacks:=false;
end;

procedure TvkObj_Factory.SetAllocHandler(aValue:TvkObj_AllocationHandler);
begin
  FreeAllocCallbacks;
  fAllocHandler:=aValue;
  if Assigned(fAllocHandler) then begin
    new(fAllocCallbacks);
    fOwnsAllocCallbacks:=true;
    fAllocCallbacks^:=fAllocHandler.GetStructure;
  end;
end;

destructor TvkObj_Factory.Destroy;
begin
  FreeAllocCallbacks;
  inherited Destroy;
end;

constructor TvkObj_InstanceObjFactory.Create(const aInstance:TvkObj_Instance);
begin
  Create(aInstance.InstanceFunctions);
end;

constructor TvkObj_InstanceObjFactory.Create(const aInstanceFunctions:TVkInstanceFunctions);
begin
  inherited Create;
  fInstanceFunctions:=aInstanceFunctions;
end;

constructor TvkObj_DeviceObjFactory.Create(const aDevice:TvkObj_Device);
begin
  Create(aDevice.DeviceFunctions);
end;

constructor TvkObj_DeviceObjFactory.Create(const aDeviceFunctions:TVkDeviceFunctions);
begin
  inherited Create;
  fDeviceFunctions:=aDeviceFunctions;
end;


procedure TvkObj_FrameBuffer.CreateHandle(const aCreateInfo:TVkFramebufferCreateInfo);
var err:TVkResult;
begin
  if not Assigned(fDeviceFunctions.vkCreateFramebuffer) then
    raise TvkObj_InvalidFuncPtrException.Create('vkCreateFramebuffer');
  err:=fDeviceFunctions.vkCreateFramebuffer(fDeviceFunctions.aaDevice, @aCreateInfo, AllocCallbacks, @fHandle);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to create frame buffer', err);
end;

constructor TvkObj_FrameBuffer.Create(const aCreateInfo:TVkFramebufferCreateInfo;
  const aDeviceFunctions:TVkDeviceFunctions; const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fDeviceFunctions:=aDeviceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(aCreateInfo);
end;

constructor TvkObj_FrameBuffer.Create(const aHandle:TVkFramebuffer; const aOwnsHandle:Boolean;
  const aDeviceFunctions:TVkDeviceFunctions; const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=aHandle;
  fDeviceFunctions:=aDeviceFunctions;
  OwnsHandle:=aOwnsHandle;
  SetAllocCallbacks(aAllocCallbacks);
end;

destructor TvkObj_FrameBuffer.Destroy;
begin
  if OwnsHandle and (fHandle <> VK_INVALID_NDP_HANDLE) then
    fDeviceFunctions.vkDestroyFramebuffer(fDeviceFunctions.aaDevice, fHandle, AllocCallbacks);
  fHandle:=VK_INVALID_NDP_HANDLE;
  inherited Destroy;
end;


function TvkObj_FrameBufferFactory.GetStructure:TVkFramebufferCreateInfo;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.renderPass:=fRenderPass;
  result.attachmentCount:=fAttachments.Length;
  result.pAttachments:=fAttachments.PData;
  result.width:=fWidth;
  result.height:=fHeight;
  result.layers:=fLayers;
end;

procedure TvkObj_FrameBufferFactory.SetStructure(const aData:TVkFramebufferCreateInfo);
var i:Integer;
begin
  fFlags:=aData.flags;
  fRenderPass:=aData.renderPass;
  fWidth:=aData.width;
  fHeight:=aData.height;
  fLayers:=aData.layers;
  fAttachments.SetData(aData.pAttachments, aData.attachmentCount);
end;

function TvkObj_FrameBufferFactory.CreateFrameBuffer:TvkObj_FrameBuffer;
begin
  result:=TvkObj_FrameBuffer.Create(GetStructure, DeviceFunctions, AllocCallbacks);
end;

procedure TvkObj_FrameBufferFactory.AfterConstruction;
begin
  inherited AfterConstruction;
  fAttachments:=TVkImageViewList.Create;
end;

procedure TvkObj_FrameBufferFactory.BeforeDestruction;
begin
  FreeAndNil(fAttachments);
  inherited BeforeDestruction;
end;


procedure TvkObj_Image.CreateHandle(const aCreateInfo:TVkImageCreateInfo);
var err:TVkResult;
begin
  err:=fDeviceFunctions.vkCreateImage(fDeviceFunctions.aaDevice, @aCreateInfo, AllocCallbacks, @fHandle);
  if (err <> VK_SUCCESS) then
   TvkObj_ErrorException.Create('unable to create image', err);
end;

function TvkObj_Image.GetMemoryBarrier(
  const aAspectMask:TVkImageAspectFlags;
  const aOldLayout:TVkImageLayout;
  const aNewLayout:TVkImageLayout):TVkImageMemoryBarrier;
var
  range:TVkImageSubresourceRange;
begin
  FillByte(range, SizeOf(range), 0);
  range.aspectMask:=aAspectMask;
  range.baseMipLevel:=0;
  range.levelCount:=1;
  range.baseArrayLayer:=0;
  range.layerCount:=1;

  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
  result.pNext:=nil;
  result.srcAccessMask:=[];
  result.dstAccessMask:=[];
  result.oldLayout:=aOldLayout;
  result.newLayout:=aNewLayout;
  result.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  result.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  result.image:=fHandle;
  result.subresourceRange:=range;

  case aOldLayout of
    // Undefined layout:
    //   Note:Only allowed as initial layout!
    //   Note:Make sure any writes to the image have been finished
    VK_IMAGE_LAYOUT_UNDEFINED:begin
      result.srcAccessMask:=[ VK_ACCESS_HOST_WRITE_BIT, VK_ACCESS_TRANSFER_WRITE_BIT ];
    end;

    // Old layout is color attachment:
    //   Note:Make sure any writes to the color buffer have been finished
    VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL:begin
      result.srcAccessMask:=[ VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT ];
    end;

    // Old layout is transfer source:
    //   Note:Make sure any reads from the image have been finished
    VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL:begin
      result.srcAccessMask:=[ VK_ACCESS_TRANSFER_READ_BIT ];
    end;

    // Old layout is shader read (sampler, input attachment):
    //   Note:Make sure any shader reads from the image have been finished
    VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL:begin
      result.srcAccessMask:=[ VK_ACCESS_SHADER_READ_BIT ];
    end;
  end;

  case aNewLayout of
    // New layout is transfer destination (copy, blit):
    //   Note:Make sure any copyies to the image have been finished
    VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL:begin
      result.dstAccessMask:=[ VK_ACCESS_TRANSFER_WRITE_BIT ];
    end;

    // New layout is transfer source (copy, blit):
    //   Note:Make sure any reads from and writes to the image have been finished
    VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL:begin
      result.srcAccessMask:=result.srcAccessMask+[ VK_ACCESS_TRANSFER_READ_BIT ];
      result.dstAccessMask:=[ VK_ACCESS_TRANSFER_READ_BIT ];
    end;

    // New layout is color attachment:
    //   Note:Make sure any writes to the color buffer hav been finished
    VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL:begin
      result.dstAccessMask:=[ VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT ];
      result.srcAccessMask:=[ VK_ACCESS_TRANSFER_READ_BIT ];
    end;

    // New layout is depth attachment:
    //   Note:Make sure any writes to depth/stencil buffer have been finished
    VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL:begin
      result.dstAccessMask:=result.dstAccessMask+[ VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT ];
    end;

    // New layout is shader read (sampler, input attachment):
    //   Note:Make sure any writes to the image have been finished
    VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL:begin
      result.srcAccessMask:=[ VK_ACCESS_HOST_WRITE_BIT, VK_ACCESS_TRANSFER_WRITE_BIT ];
      result.dstAccessMask:=[ VK_ACCESS_SHADER_READ_BIT ];
    end;
  end;
end;

procedure TvkObj_Image.BindMemory(const aMemory:TVkDeviceMemory; const aOffset:TVkDeviceSize);
var err:TVkResult;
begin
  err:=fDeviceFunctions.vkBindImageMemory(fDeviceFunctions.aaDevice, fHandle, aMemory, aOffset);
  if (err <> VK_SUCCESS) then
   TvkObj_ErrorException.Create('unable to bind image memory', err);
  fMemory:=aMemory;
end;

function TvkObj_Image.GetMemoryRequirements:TVkMemoryRequirements;
begin
  fDeviceFunctions.vkGetImageMemoryRequirements(fDeviceFunctions.aaDevice, fHandle, @result);
end;

procedure TvkObj_Image.AllocateAndBindImageMemory(const aMemoryProperties:TVkPhysicalDeviceMemoryProperties;
  aAllocCallbacks:PVkAllocationCallbacks);
var
  i:Integer;
  bits:TVkUInt32;
  MemReq:TVkMemoryRequirements;
  Mem:TVkDeviceMemory;
  MemAllocInfo:TVkMemoryAllocateInfo;
  err:TVkResult;
begin
  MemReq:=GetMemoryRequirements;

  FillByte(MemAllocInfo, SizeOf(MemAllocInfo), 0);
  MemAllocInfo.sType:=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
  MemAllocInfo.pNext:=nil;
  MemAllocInfo.allocationSize:=MemReq.size;
  MemAllocInfo.memoryTypeIndex:=vkuGetMemoryTypeIndex(
    aMemoryProperties,
    MemReq.memoryTypeBits,
    VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);;

  if not Assigned(aAllocCallbacks) then
    aAllocCallbacks:=AllocCallbacks;
  err:=fDeviceFunctions.vkAllocateMemory(fDeviceFunctions.aaDevice, @MemAllocInfo, aAllocCallbacks, @Mem);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to allocate image memory', err);
  fImageMemory:=TvkObj_DeviceMemory.Create(Mem, true, fDeviceFunctions, aAllocCallbacks);
  BindMemory(fImageMemory.Handle, 0);
end;

constructor TvkObj_Image.Create(const aCreateInfo:TVkImageCreateInfo; const aDeviceFunctions:TVkDeviceFunctions;
  const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fDeviceFunctions:=aDeviceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(aCreateInfo);
end;

constructor TvkObj_Image.Create(const aHandle:TVkImage; const aOwnsHandle:Boolean;
  const aDeviceFunctions:TVkDeviceFunctions; const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=aHandle;
  fDeviceFunctions:=aDeviceFunctions;
  OwnsHandle:=aOwnsHandle;
  SetAllocCallbacks(aAllocCallbacks);
end;

destructor TvkObj_Image.Destroy;
begin
  if OwnsHandle and (fHandle <> VK_INVALID_NDP_HANDLE) then
    fDeviceFunctions.vkDestroyImage(fDeviceFunctions.aaDevice, fHandle, AllocCallbacks);
  fHandle:=VK_INVALID_NDP_HANDLE;
  FreeAndNil(fImageMemory);
  inherited Destroy;
end;


function TvkObj_ImageFactory.GetStructure:TVkImageCreateInfo;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType :=VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
  result.pNext :=nil;
  result.flags :=fFlags;
  result.imageType:=fImageType;
  result.format:=fFormat;
  result.extent:=fExtent;
  result.mipLevels:=fMipLevels;
  result.arrayLayers:=fArrayLayers;
  result.samples:=fSamples;
  result.tiling:=fTiling;
  result.usage :=fUsage;
  result.sharingMode:=fSharingMode;
  result.queueFamilyIndexCount:=fQueueFamilyIndices.Length;
  result.pQueueFamilyIndices:=fQueueFamilyIndices.PData;
  result.initialLayout:=fInitialLayout;
end;

procedure TvkObj_ImageFactory.SetStructure(const aData:TVkImageCreateInfo);
var i:Integer;
begin
  fFlags:=aData.flags;
  fImageType:=aData.imageType;
  fFormat:=aData.format;
  fExtent:=aData.extent;
  fMipLevels:=aData.mipLevels;
  fArrayLayers:=aData.arrayLayers;
  fSamples:=aData.samples;
  fTiling:=aData.tiling;
  fUsage:=aData.usage;
  fSharingMode:=aData.sharingMode;
  fInitialLayout:=aData.initialLayout;

  fQueueFamilyIndices.SetData(aData.pQueueFamilyIndices, aData.queueFamilyIndexCount);
end;

function TvkObj_ImageFactory.CreateImage:TvkObj_Image;
begin
  result:=TvkObj_Image.Create(GetStructure, DeviceFunctions, AllocCallbacks);
end;

procedure TvkObj_ImageFactory.AfterConstruction;
begin
  inherited AfterConstruction;
  fQueueFamilyIndices:=TvkInt32List.Create;
end;

procedure TvkObj_ImageFactory.BeforeDestruction;
begin
  FreeAndNil(fQueueFamilyIndices);
  inherited BeforeDestruction;
end;


procedure TvkObj_ImageView.CreateHandle(const aCreateInfo:TVkImageViewCreateInfo);
var err:TVkResult;
begin
  if not Assigned(fDeviceFunctions.vkCreateImageView) then
    raise TvkObj_InvalidFuncPtrException.Create('vkCreateImageView');
  err:=fDeviceFunctions.vkCreateImageView(fDeviceFunctions.aaDevice, @aCreateInfo, AllocCallbacks, @fHandle);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to create image view', err);
end;

constructor TvkObj_ImageView.Create(const aCreateInfo:TVkImageViewCreateInfo; const aDeviceFunctions:TVkDeviceFunctions;
  const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fDeviceFunctions:=aDeviceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(aCreateInfo);
end;

constructor TvkObj_ImageView.Create(const aHandle:TVkImageView; const aOwnsHandle:Boolean;
  const aDeviceFunctions:TVkDeviceFunctions; const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=aHandle;
  fDeviceFunctions:=aDeviceFunctions;
  OwnsHandle:=aOwnsHandle;
  SetAllocCallbacks(aAllocCallbacks);
end;

destructor TvkObj_ImageView.Destroy;
begin
  if OwnsHandle and (fHandle <> VK_INVALID_NDP_HANDLE) then
    fDeviceFunctions.vkDestroyImageView(fDeviceFunctions.aaDevice, fHandle, AllocCallbacks);
  fHandle:=VK_INVALID_NDP_HANDLE;
  inherited Destroy;
end;


function TvkObj_ImageSubresourceRange.GetStructure:TVkImageSubresourceRange;
begin
  FillByte(result, SizeOf(result), 0);
  result.aspectMask:=AspectMask;
  result.baseMipLevel:=BaseMipLevel;
  result.levelCount:=LevelCount;
  result.baseArrayLayer:=BaseArrayLayer;
  result.layerCount:=LayerCount;
end;

procedure TvkObj_ImageSubresourceRange.SetStructure(const aData:TVkImageSubresourceRange);
begin
  AspectMask:=aData.aspectMask;
  BaseMipLevel:=aData.baseMipLevel;
  LevelCount:=aData.levelCount;
  BaseArrayLayer:=aData.baseArrayLayer;
  LayerCount:=aData.layerCount;
end;

function TvkObj_ImageViewFactory.GetStructure:TVkImageViewCreateInfo;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.image:=fImage;
  result.viewType:=fViewType;
  result.format:=fFormat;
  result.components:=fComponents;
  result.subresourceRange:=fSubresourceRange.GetStructure;
end;

procedure TvkObj_ImageViewFactory.SetStructure(const aData:TVkImageViewCreateInfo);
begin
  fFlags:=aData.flags;
  fImage:=aData.image;
  fViewType:=aData.viewType;
  fFormat:=aData.format;
  fComponents:=aData.components;
  fSubresourceRange.SetStructure(aData.subresourceRange);
end;

function TvkObj_ImageViewFactory.CreateImageView:TvkObj_ImageView;
begin
  result:=TvkObj_ImageView.Create(GetStructure, DeviceFunctions, AllocCallbacks);
end;

procedure TvkObj_ImageViewFactory.AfterConstruction;
begin
  inherited AfterConstruction;
  fSubresourceRange:=TvkObj_ImageSubresourceRange.Create;
end;

procedure TvkObj_ImageViewFactory.BeforeDestruction;
begin
  FreeAndNil(fSubresourceRange);
  inherited BeforeDestruction;
end;


procedure TvkObj_Instance.UpdatePhysicalDevices;
var
  c:TVkUInt32;
  err:TVkResult;
begin
  if (fPhysicalDevices <> nil) then
    exit;

  err:=InstanceFunctions.vkEnumeratePhysicalDevices(fHandle, @c, nil);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to get physical device number', err);
  SetLength(fPhysicalDevices, c);

  err:=InstanceFunctions.vkEnumeratePhysicalDevices(fHandle, @c, @fPhysicalDevices[0]);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to get physical devices', err);
end;

procedure TvkObj_Instance.CreateHandle(const aCreateInfo:PVkInstanceCreateInfo);
var e:TVkResult;
begin
  e:=vkCreateInstance(aCreateInfo, AllocCallbacks, @fHandle);
  if (e <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to create vulkan instance', e);
  fInstanceFunctions:=vkoLoadInstanceFunctions(fHandle);
  if not Assigned(InstanceFunctions) then
    raise TvkObj_Exception.Create('unable to load instance commands');
end;

function TvkObj_Instance.GetPhysicalDevices:TVkPhysicalDeviceArr;
begin
  UpdatePhysicalDevices;
  result:=fPhysicalDevices;
end;

constructor TvkObj_Instance.Create(const aCreateInfo:TVkInstanceCreateInfo; const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=VK_INVALID_HANDLE;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(@aCreateInfo);
end;

constructor TvkObj_Instance.Create(const aHandle:TVkInstance; const aOwnsHandle:Boolean; const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=aHandle;
  OwnsHandle:=aOwnsHandle;
  SetAllocCallbacks(aAllocCallbacks);
end;

destructor TvkObj_Instance.Destroy;
begin
  if OwnsHandle and (fHandle <> VK_INVALID_HANDLE) then
    InstanceFunctions.vkDestroyInstance(fHandle, AllocCallbacks);
  fHandle:=VK_INVALID_HANDLE;
  FreeAndNil(fInstanceFunctions);
  inherited Destroy;
end;


function TvkObj_ApplicationInfo.GetStructure:TVkApplicationInfo;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_APPLICATION_INFO;
  result.pNext:=nil;
  result.pApplicationName:=PVkChar(AppName);
  result.applicationVersion:=AppVersion;
  result.pEngineName:=PVkChar(EngineName);
  result.engineVersion:=EngineVersion;
  result.apiVersion:=ApiVersion;
end;

procedure TvkObj_ApplicationInfo.SetStructure(const aData:TVkApplicationInfo);
begin
  AppName:=aData.pApplicationName;
  AppVersion:=aData.applicationVersion;
  EngineName:=aData.pEngineName;
  EngineVersion:=aData.engineVersion;
  ApiVersion:=aData.apiVersion;
end;

constructor TvkObj_ApplicationInfo.Create;
begin
  AppName:=ExtractFileName(ParamStr(0));
  AppVersion:=0;
  EngineName:=ExtractFileName(ParamStr(0));
  EngineVersion:=0;
  ApiVersion:=VK_API_VERSION;
end;

function TvkObj_InstanceFactory.GetLayers:TStrings;
begin
  result:=fEnabledLayerNames;
end;

function TvkObj_InstanceFactory.GetExtensions:TStrings;
begin
  result:=fEnabledExtensionNames;
end;

function TvkObj_InstanceFactory.GetStructure:TVkInstanceCreateInfo;
var i:Integer;
begin
  SetLength(fvkEnabledLayerNames, fEnabledLayerNames.Count);
  for i:=low(fvkEnabledLayerNames) to high(fvkEnabledLayerNames) do
    fvkEnabledLayerNames[i]:=PVkChar(fEnabledLayerNames[i]);

  SetLength(fvkEnabledExtensionNames, fEnabledExtensionNames.Count);
  for i:=low(fvkEnabledExtensionNames) to high(fvkEnabledExtensionNames) do
    fvkEnabledExtensionNames[i]:=PVkChar(fEnabledExtensionNames[i]);

  fvkApplicationInfo:=fApplicationInfo.GetStructure;

  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.pApplicationInfo:=@fvkApplicationInfo;
  result.enabledLayerCount:=Length(fvkEnabledLayerNames);
  result.ppEnabledLayerNames:=@fvkEnabledLayerNames[0];
  result.enabledExtensionCount:=Length(fvkEnabledExtensionNames);
  result.ppEnabledExtensionNames:=@fvkEnabledExtensionNames[0];
end;

procedure TvkObj_InstanceFactory.SetStructure(const aData:TVkInstanceCreateInfo);
var i:Integer;
begin
  fFlags:=aData.flags;

  if Assigned(aData.pApplicationInfo) then
    fApplicationInfo.SetStructure(aData.pApplicationInfo^);

  fEnabledLayerNames.Clear;
  for i:=0 to aData.enabledLayerCount-1 do
    fEnabledLayerNames.Add(aData.ppEnabledLayerNames[i]);

  fEnabledExtensionNames.Clear;
  for i:=0 to aData.enabledExtensionCount-1 do
    fEnabledExtensionNames.Add(aData.ppEnabledExtensionNames[i]);
end;

function TvkObj_InstanceFactory.CreateInstance:TvkObj_Instance;
begin
  result:=TvkObj_Instance.Create(GetStructure, AllocCallbacks);
end;

constructor TvkObj_InstanceFactory.Create;
begin
  inherited Create;
  fEnabledLayerNames:=TStringList.Create;
  fEnabledExtensionNames:=TStringList.Create;
  fApplicationInfo:=TvkObj_ApplicationInfo.Create;
  fFlags:=0;
end;

destructor TvkObj_InstanceFactory.Destroy;
begin
  FreeAndNil(fApplicationInfo);
  FreeAndNil(fEnabledLayerNames);
  FreeAndNil(fEnabledExtensionNames);
  inherited Destroy;
end;


function TvkObj_PhysicalDevice.GetFeatures:TVkPhysicalDeviceFeatures;
begin
  if not Assigned(InstanceFunctions.vkGetPhysicalDeviceFeatures) then
    raise TvkObj_InvalidFuncPtrException.Create('vkGetPhysicalDeviceFeatures');
  InstanceFunctions.vkGetPhysicalDeviceFeatures(fHandle, @result);
end;

function TvkObj_PhysicalDevice.GetProperties:TVkPhysicalDeviceProperties;
begin
  if not Assigned(InstanceFunctions.vkGetPhysicalDeviceProperties) then
    raise TvkObj_InvalidFuncPtrException.Create('vkGetPhysicalDeviceProperties');
  InstanceFunctions.vkGetPhysicalDeviceProperties(fHandle, @result);
end;

function TvkObj_PhysicalDevice.GetMemoryProperties:TVkPhysicalDeviceMemoryProperties;
begin
  if not Assigned(InstanceFunctions.vkGetPhysicalDeviceMemoryProperties) then
    raise TvkObj_InvalidFuncPtrException.Create('vkGetPhysicalDeviceMemoryProperties');
  InstanceFunctions.vkGetPhysicalDeviceMemoryProperties(fHandle, @result);
end;

function TvkObj_PhysicalDevice.GetQueueFamilyProperties:TVkQueueFamilyPropertiesArr;
var c:TVkUInt32;
begin
  result:=nil;
  if not Assigned(InstanceFunctions.vkGetPhysicalDeviceQueueFamilyProperties) then
    raise TvkObj_InvalidFuncPtrException.Create('vkGetPhysicalDeviceQueueFamilyProperties');
  InstanceFunctions.vkGetPhysicalDeviceQueueFamilyProperties(fHandle, @c, nil);
  SetLength(result, c);
  InstanceFunctions.vkGetPhysicalDeviceQueueFamilyProperties(fHandle, @c, @result[0]);
end;

function TvkObj_PhysicalDevice.GetFormatProperties(const aFormat:TVkFormat):TVkFormatProperties;
begin
  if not Assigned(InstanceFunctions.vkGetPhysicalDeviceFormatProperties) then
    raise TvkObj_InvalidFuncPtrException.Create('vkGetPhysicalDeviceFormatProperties');
  InstanceFunctions.vkGetPhysicalDeviceFormatProperties(fHandle, aFormat, @result);
end;

function TvkObj_PhysicalDevice.GetImageFormatProperties(const aFormat:TVkFormat; const aImageType:TVkImageType;
  const aTiling:TVkImageTiling; const aUsage:TVkImageUsageFlags; const aFlags:TVkImageCreateFlags):TVkImageFormatProperties;
var err:TVkResult;
begin
  if not Assigned(InstanceFunctions.vkGetPhysicalDeviceImageFormatProperties) then
    raise TvkObj_InvalidFuncPtrException.Create('vkGetPhysicalDeviceImageFormatProperties');
  err:=InstanceFunctions.vkGetPhysicalDeviceImageFormatProperties(fHandle, aFormat, aImageType, aTiling, aUsage, aFlags, @result);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to get physical device image format properties', err);
end;

function TvkObj_PhysicalDevice.GetSparseImageFormatProperties(const aFormat:TVkFormat; const aImageType:TVkImageType;
  const aSamples:TVkSampleCountFlagBits; const aUsage:TVkImageUsageFlags; const aTiling:TVkImageTiling):TVkSparseImageFormatPropertiesArr;
var c:TVkUInt32;
begin
  result:=nil;
  if not Assigned(InstanceFunctions.vkGetPhysicalDeviceSparseImageFormatProperties) then
    raise TvkObj_InvalidFuncPtrException.Create('vkGetPhysicalDeviceSparseImageFormatProperties');
  InstanceFunctions.vkGetPhysicalDeviceSparseImageFormatProperties(fHandle, aFormat, aImageType, aSamples, aUsage, aTiling, @c, nil);
  SetLength(result, c);
  InstanceFunctions.vkGetPhysicalDeviceSparseImageFormatProperties(fHandle, aFormat, aImageType, aSamples, aUsage, aTiling, @c, @result[0]);
end;

function TvkObj_PhysicalDevice.GetSurfaceSupport(const aQueueFamilyIndex:TVkUInt32; const aSurface:TVkSurfaceKHR):Boolean;
var
  b:TVkBool32;
  err:TVkResult;
begin
  if not Assigned(InstanceFunctions.vkGetPhysicalDeviceSurfaceSupportKHR) then
    raise TvkObj_InvalidFuncPtrException.Create('vkGetPhysicalDeviceSurfaceSupportKHR', 'VK_KHR_surface');
  err:=InstanceFunctions.vkGetPhysicalDeviceSurfaceSupportKHR(fHandle, aQueueFamilyIndex, aSurface, @b);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to get physical device surface support', err);
  result:=(b <> 0);
end;

function TvkObj_PhysicalDevice.GetSurfaceCapabilities(const aSurface:TVkSurfaceKHR):TVkSurfaceCapabilitiesKHR;
var
  err:TVkResult;
begin
  if not Assigned(InstanceFunctions.vkGetPhysicalDeviceSurfaceCapabilitiesKHR) then
    raise TvkObj_InvalidFuncPtrException.Create('vkGetPhysicalDeviceSurfaceCapabilitiesKHR', 'VK_KHR_surface');
  err:=InstanceFunctions.vkGetPhysicalDeviceSurfaceCapabilitiesKHR(fHandle, aSurface, @result);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to get physical device surface capabilities', err);
end;

function TvkObj_PhysicalDevice.GetSurfaceFormats(const aSurface:TVkSurfaceKHR):TVkSurfaceFormatArr;
var c:TVkUInt32;
begin
  result:=nil;
  if not Assigned(InstanceFunctions.vkGetPhysicalDeviceSurfaceFormatsKHR) then
    raise TvkObj_InvalidFuncPtrException.Create('vkGetPhysicalDeviceSurfaceFormatsKHR', 'VK_KHR_surface');
  InstanceFunctions.vkGetPhysicalDeviceSurfaceFormatsKHR(fHandle, aSurface, @c, nil);
  SetLength(result, c);
  InstanceFunctions.vkGetPhysicalDeviceSurfaceFormatsKHR(fHandle, aSurface, @c, @result[0]);
end;

function TvkObj_PhysicalDevice.GetSurfacePresentModes(const aSurface:TVkSurfaceKHR):TVkPresentModeArr;
var
  c:TVkUInt32;
  err:TVkResult;
begin
  result:=nil;
  if not Assigned(InstanceFunctions.vkGetPhysicalDeviceSurfacePresentModesKHR) then
    raise TvkObj_InvalidFuncPtrException.Create('vkGetPhysicalDeviceSurfacePresentModesKHR', 'VK_KHR_surface');
  err:=InstanceFunctions.vkGetPhysicalDeviceSurfacePresentModesKHR(fHandle, aSurface, @c, nil);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to get physical device surface present mode count', err);
  SetLength(result, c);
  err:=InstanceFunctions.vkGetPhysicalDeviceSurfacePresentModesKHR(fHandle, aSurface, @c, @result[0]);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to get physical device surface present modes', err);
end;

function TvkObj_PhysicalDevice.GetDisplayProperties:TVkDisplayPropertiesArr;
var
  err:TVkResult;
  c:TVkUInt32;
begin
  result:=nil;
  if not Assigned(fInstanceFunctions.vkGetPhysicalDeviceDisplayPropertiesKHR) then
    raise TvkObj_InvalidFuncPtrException.Create('vkGetPhysicalDeviceDisplayPropertiesKHR', 'VK_KHR_display');
  err:=fInstanceFunctions.vkGetPhysicalDeviceDisplayPropertiesKHR(fHandle, @c, nil);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to get physical device display property count', err);
  SetLength(result, c);
  err:=fInstanceFunctions.vkGetPhysicalDeviceDisplayPropertiesKHR(fHandle, @c, @result[0]);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to get physical device display properties', err);
end;

function TvkObj_PhysicalDevice.GetDisplayPlaneProperties:TVkDisplayPlanePropertiesArr;
var
  err:TVkResult;
  c:TVkUInt32;
begin
  result:=nil;
  if not Assigned(fInstanceFunctions.vkGetPhysicalDeviceDisplayPlanePropertiesKHR) then
    raise TvkObj_InvalidFuncPtrException.Create('vkGetPhysicalDeviceDisplayPlanePropertiesKHR', 'VK_KHR_display');
  err:=fInstanceFunctions.vkGetPhysicalDeviceDisplayPlanePropertiesKHR(fHandle, @c, nil);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to get physical device display plane property count', err);
  SetLength(result, c);
  err:=fInstanceFunctions.vkGetPhysicalDeviceDisplayPlanePropertiesKHR(fHandle, @c, @result[0]);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to get physical device display plane properties', err);
end;

function TvkObj_PhysicalDevice.GetDisplayPlaneSupportedDisplays(const aPlaneIndex:TVkUInt32):TVkDisplayArr;
var
  err:TVkResult;
  c:TVkUInt32;
begin
  result:=nil;
  if not Assigned(fInstanceFunctions.vkGetDisplayPlaneSupportedDisplaysKHR) then
    raise TvkObj_InvalidFuncPtrException.Create('vkGetDisplayPlaneSupportedDisplaysKHR', 'VK_KHR_display');
  err:=fInstanceFunctions.vkGetDisplayPlaneSupportedDisplaysKHR(fHandle, aPlaneIndex, @c, nil);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to get supported display plane count', err);
  SetLength(result, c);
  err:=fInstanceFunctions.vkGetDisplayPlaneSupportedDisplaysKHR(fHandle, aPlaneIndex, @c, @result[0]);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to get supported display planes', err);
end;

function TvkObj_PhysicalDevice.GetDisplayModeProperties(const aDisplay:TVkDisplayKHR):TVkDisplayModePropertiesArr;
var
  err:TVkResult;
  c:TVkUInt32;
begin
  result:=nil;
  if not Assigned(fInstanceFunctions.vkGetDisplayModePropertiesKHR) then
    raise TvkObj_InvalidFuncPtrException.Create('vkGetDisplayModePropertiesKHR', 'VK_KHR_display');
  err:=fInstanceFunctions.vkGetDisplayModePropertiesKHR(fHandle, aDisplay, @c, nil);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to get display mode property count', err);
  SetLength(result, c);
  err:=fInstanceFunctions.vkGetDisplayModePropertiesKHR(fHandle, aDisplay, @c, @result[0]);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to get display mode properties', err);
end;

constructor TvkObj_PhysicalDevice.Create(const aHandle:TVkPhysicalDevice; const aInstanceFunctions:TVkInstanceFunctions);
begin
  if (aHandle=VK_INVALID_HANDLE) then
    raise TvkObj_Exception.Create('invalid physical device handle');
  if not Assigned(aInstanceFunctions) then
    raise TvkObj_Exception.Create('invalid instance commands');
  inherited Create;
  fHandle:=aHandle;
  fInstanceFunctions:=aInstanceFunctions;
end;


constructor TvkObj_Pipeline.Create(const aHandle:TVkPipeline; const aOwnsHandle:Boolean;
  const aDeviceFunctions:TVkDeviceFunctions; const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=aHandle;
  fDeviceFunctions:=aDeviceFunctions;
  OwnsHandle:=aOwnsHandle;
  SetAllocCallbacks(aAllocCallbacks);
end;

destructor TvkObj_Pipeline.Destroy;
begin
  if OwnsHandle and (fHandle <> VK_INVALID_NDP_HANDLE) then
    fDeviceFunctions.vkDestroyPipeline(fDeviceFunctions.aaDevice, fHandle, AllocCallbacks);
  fHandle:=VK_INVALID_NDP_HANDLE;
  inherited Destroy;
end;

procedure TvkObj_GraphicsPipeline.CreateHandle(const aCreateInfo:TVkGraphicsPipelineCreateInfo;
  const aPipelineCache:TVkPipelineCache);
var err:TVkResult;
begin
  if not Assigned(fDeviceFunctions.vkCreateGraphicsPipelines) then
    raise TvkObj_InvalidFuncPtrException.Create('vkCreateGraphicsPipelines');
  err:=fDeviceFunctions.vkCreateGraphicsPipelines(fDeviceFunctions.aaDevice, aPipelineCache, 1, @aCreateInfo, AllocCallbacks, @fHandle);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to create graphics pipeline', err);
end;

constructor TvkObj_GraphicsPipeline.Create(const aCreateInfo:TVkGraphicsPipelineCreateInfo;
  const aPipelineCache:TVkPipelineCache; const aDeviceFunctions:TVkDeviceFunctions;
  const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create(VK_INVALID_NDP_HANDLE, true, aDeviceFunctions, aAllocCallbacks);
  CreateHandle(aCreateInfo, aPipelineCache);
end;

procedure TvkObj_ComputePipeline.CreateHandle(const aCreateInfo:TVkComputePipelineCreateInfo;
  const aPipelineCache:TVkPipelineCache);
var err:TVkResult;
begin
  if not Assigned(fDeviceFunctions.vkCreateComputePipelines) then
    raise TvkObj_InvalidFuncPtrException.Create('vkCreateComputePipelines');
  err:=fDeviceFunctions.vkCreateComputePipelines(fDeviceFunctions.aaDevice, aPipelineCache, 1, @aCreateInfo, AllocCallbacks, @fHandle);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to create compute pipeline', err);
end;

constructor TvkObj_ComputePipeline.Create(const aCreateInfo:TVkComputePipelineCreateInfo;
  const aPipelineCache:TVkPipelineCache; const aDeviceFunctions:TVkDeviceFunctions;
  const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create(VK_INVALID_NDP_HANDLE, true, aDeviceFunctions, aAllocCallbacks);
  CreateHandle(aCreateInfo, aPipelineCache);
end;


procedure TvkObj_PipelineCache.CreateHandle(const aCreateInfo:TVkPipelineCacheCreateInfo);
var err:TVkResult;
begin
  err:=fDeviceFunctions.vkCreatePipelineCache(fDeviceFunctions.aaDevice, @aCreateInfo, AllocCallbacks, @fHandle);
  if (err <> VK_SUCCESS) then
   TvkObj_ErrorException.Create('unable to create pipeline cache', err);
end;

constructor TvkObj_PipelineCache.Create(const aCreateInfo:TVkPipelineCacheCreateInfo;
  const aDeviceFunctions:TVkDeviceFunctions; const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fDeviceFunctions:=aDeviceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(aCreateInfo);
end;

constructor TvkObj_PipelineCache.Create(const aHandle:TVkPipelineCache; const aOwnsHandle:Boolean;
  const aDeviceFunctions:TVkDeviceFunctions; const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=aHandle;
  fDeviceFunctions:=aDeviceFunctions;
  OwnsHandle:=aOwnsHandle;
  SetAllocCallbacks(aAllocCallbacks);
end;

destructor TvkObj_PipelineCache.Destroy;
begin
  if OwnsHandle and (fHandle <> VK_INVALID_NDP_HANDLE) then
    fDeviceFunctions.vkDestroyPipelineCache(fDeviceFunctions.aaDevice, fHandle, AllocCallbacks);
  fHandle:=VK_INVALID_NDP_HANDLE;
  inherited Destroy;
end;


procedure TvkObj_PipelineCacheFactory.SetInitalData(const aData:PVkVoid; const aSize:TVkSize);
begin
  fInitialData:=aData;
  fInitialDataSize:=aSize;
end;

function TvkObj_PipelineCacheFactory.GetStructure:TVkPipelineCacheCreateInfo;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.initialDataSize:=fInitialDataSize;
  result.pInitialData:=fInitialData;
end;

procedure TvkObj_PipelineCacheFactory.SetStructure(const aData:TVkPipelineCacheCreateInfo);
begin
  fFlags:=aData.flags;
  fInitialDataSize:=aData.initialDataSize;
  fInitialData:=aData.pInitialData;
end;

function TvkObj_PipelineCacheFactory.CreatePipelineCache:TvkObj_PipelineCache;
begin
  result:=TvkObj_PipelineCache.Create(GetStructure, DeviceFunctions, AllocCallbacks);
end;


function TvkObj_PipelineShaderStageCreateInfo.GetStructure:TVkPipelineShaderStageCreateInfo;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=Flags;
  result.stage:=Stage;
  result.module:=Module;
  result.pName:=PVkChar(Name);
  result.pSpecializationInfo:=nil; // TODO:implement SpecializationInfo
end;

procedure TvkObj_PipelineShaderStageCreateInfo.SetStructure(const aData:TVkPipelineShaderStageCreateInfo);
begin
  Flags:=aData.flags;
  Stage:=aData.stage;
  Module:=aData.module;
  if Assigned(aData.pName)
    then Name:=aData.pName
    else Name:='';
  // TODO:read SpecializationInfo
end;

function TvkObj_VertexInputBindingDescription.GetStructure:TVkVertexInputBindingDescription;
begin
  FillByte(result, SizeOf(result), 0);
  result.binding:=Binding;
  result.stride:=Stride;
  result.inputRate:=InputRate;
end;

procedure TvkObj_VertexInputBindingDescription.SetStructure(const aData:TVkVertexInputBindingDescription);
begin
  Binding:=aData.binding;
  Stride:=aData.stride;
  InputRate:=aData.inputRate;
end;

function TvkObj_VertexInputAttributeDescription.GetStructure:TVkVertexInputAttributeDescription;
begin
  FillByte(result, SizeOf(result), 0);
  result.location:=Location;
  result.binding:=Binding;
  result.format:=Format;
  result.offset:=Offset;
end;

procedure TvkObj_VertexInputAttributeDescription.SetStructure(const aData:TVkVertexInputAttributeDescription);
begin
  Location:=aData.location;
  Binding:=aData.binding;
  Format:=aData.format;
  Offset:=aData.offset;
end;

function TvkObj_PipelineVertexInputStateCreateInfo.GetStructure:TVkPipelineVertexInputStateCreateInfo;
var i:Integer;
begin
  SetLength(fvkVertexBindingDescriptions, fVertexBindingDescriptions.Length);
  for i:=low(fvkVertexBindingDescriptions) to high(fvkVertexBindingDescriptions) do
    fvkVertexBindingDescriptions[i]:=fVertexBindingDescriptions[i].GetStructure;

  SetLength(fvkVertexAttributeDescriptions, fVertexAttributeDescriptions.Length);
  for i:=low(fvkVertexAttributeDescriptions) to high(fvkVertexAttributeDescriptions) do
    fvkVertexAttributeDescriptions[i]:=fVertexAttributeDescriptions[i].GetStructure;

  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.vertexBindingDescriptionCount:=Length(fvkVertexBindingDescriptions);
  result.pVertexBindingDescriptions:=@fvkVertexBindingDescriptions[0];
  result.vertexAttributeDescriptionCount:=Length(fvkVertexAttributeDescriptions);
  result.pVertexAttributeDescriptions:=@fvkVertexAttributeDescriptions[0];
end;

procedure TvkObj_PipelineVertexInputStateCreateInfo.SetStructure(const aData:TVkPipelineVertexInputStateCreateInfo);
var i:Integer;
begin
  if Assigned(aData.pVertexAttributeDescriptions) then begin
    fVertexAttributeDescriptions.Length:=aData.vertexAttributeDescriptionCount;
    for i:=0 to fVertexAttributeDescriptions.Length-1 do
      fVertexAttributeDescriptions[i].SetStructure((aData.pVertexAttributeDescriptions+i)^);
  end;

  if Assigned(aData.pVertexBindingDescriptions) then begin
    fVertexBindingDescriptions.Length:=aData.vertexBindingDescriptionCount;
    for i:=0 to fVertexBindingDescriptions.Length-1 do
      fVertexBindingDescriptions[i].SetStructure((aData.pVertexBindingDescriptions+i)^);
  end;

  fFlags:=aData.flags;
end;

constructor TvkObj_PipelineVertexInputStateCreateInfo.Create;
begin
  inherited Create;
  fVertexAttributeDescriptions:=TvkObj_VertexInputAttributeDescriptionList.Create;
  fVertexBindingDescriptions:=TvkObj_VertexInputBindingDescriptionList.Create;
end;

destructor TvkObj_PipelineVertexInputStateCreateInfo.Destroy;
begin
  FreeAndNil(fVertexBindingDescriptions);
  FreeAndNil(fVertexAttributeDescriptions);
  inherited Destroy;
end;

function TvkObj_PipelineInputAssemblyStateCreateInfo.GetStructure:TVkPipelineInputAssemblyStateCreateInfo;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=Flags;
  result.topology:=Topology;
  if PrimitiveRestartEnable
    then result.primitiveRestartEnable:=VK_TRUE
    else result.primitiveRestartEnable:=VK_FALSE;
end;

procedure TvkObj_PipelineInputAssemblyStateCreateInfo.SetStructure(const aData:TVkPipelineInputAssemblyStateCreateInfo);
begin
  Flags:=aData.flags;
  Topology:=aData.topology;
  PrimitiveRestartEnable:=(aData.primitiveRestartEnable <> 0);
end;

function TvkObj_PipelineTessellationStateCreateInfo.GetStructure:TVkPipelineTessellationStateCreateInfo;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=Flags;
  result.patchControlPoints:=PatchControlPoints;
end;

procedure TvkObj_PipelineTessellationStateCreateInfo.SetStructure(const aData:TVkPipelineTessellationStateCreateInfo);
begin
  Flags:=aData.flags;
  PatchControlPoints:=aData.patchControlPoints;
end;

function TvkObj_Viewport.GetStructure:TVkViewport;
begin
  FillByte(result, SizeOf(result), 0);
  result.x:=x;
  result.y:=y;
  result.width:=Width;
  result.height:=Height;
  result.minDepth:=MinDepth;
  result.maxDepth:=MaxDepth;
end;

procedure TvkObj_Viewport.SetStructure(const aData:TVkViewport);
begin
  x:=aData.x;
  y:=aData.y;
  Width:=aData.width;
  Height:=aData.height;
  MinDepth:=aData.minDepth;
  MaxDepth:=aData.maxDepth;
end;

function TvkObj_Rect2D.GetStructure:TVkRect2D;
begin
  FillByte(result, SizeOf(result), 0);
  result.offset:=Offset;
  result.extent:=Extent;
end;

procedure TvkObj_Rect2D.SetStructure(const aData:TVkRect2D);
begin
  Offset:=aData.offset;
  Extent:=aData.extent;
end;

function TvkObj_PipelineViewportStateCreateInfo.GetStructure:TVkPipelineViewportStateCreateInfo;
var i:Integer;
begin
  SetLength(fvkViewports, fViewports.Length);
  for i:=low(fvkViewports) to high(fvkViewports) do
    fvkViewports[i]:=fViewports[i].GetStructure;

  SetLength(fvkScissors, fScissors.Length);
  for i:=low(fvkScissors) to high(fvkScissors) do
    fvkScissors[i]:=fScissors[i].GetStructure;

  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.viewportCount:=Length(fvkViewports);
  result.pViewports:=@fvkViewports[0];
  result.scissorCount:=Length(fvkScissors);
  result.pScissors:=@fvkScissors[0];
end;

procedure TvkObj_PipelineViewportStateCreateInfo.SetStructure(const aData:TVkPipelineViewportStateCreateInfo);
var i:Integer;
begin
  if Assigned(aData.pViewports) then begin
    fViewports.Length:=aData.viewportCount;
    for i:=0 to fViewports.Length-1 do
      fViewports[i].SetStructure((aData.pViewports+i)^);
  end;

  if Assigned(aData.pScissors) then begin
    fScissors.Length:=aData.scissorCount;
    for i:=0 to fScissors.Length-1 do
      fScissors[i].SetStructure((aData.pScissors+i)^);
  end;

  fFlags:=aData.flags;
end;

constructor TvkObj_PipelineViewportStateCreateInfo.Create;
begin
  inherited Create;
  fViewports:=TvkObj_ViewportList.Create;
  fScissors:=TvkObj_Rect2DList.Create;
end;

destructor TvkObj_PipelineViewportStateCreateInfo.Destroy;
begin
  FreeAndNil(fViewports);
  FreeAndNil(fScissors);
  inherited Destroy;
end;

function TvkObj_PipelineRasterizationStateCreateInfo.GetStructure:TVkPipelineRasterizationStateCreateInfo;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=Flags;
  result.polygonMode:=PolygonMode;
  result.cullMode :=CullMode;
  result.frontFace:=FrontFace;
  result.depthBiasConstantFactor:=DepthBiasConstantFactor;
  result.depthBiasClamp:=DepthBiasClamp;
  result.depthBiasSlopeFactor:=DepthBiasSlopeFactor;
  result.lineWidth:=LineWidth;

  if DepthClampEnable
    then result.depthClampEnable:=VK_TRUE
    else result.depthClampEnable:=VK_FALSE;
  if RasterizerDiscardEnable
    then result.rasterizerDiscardEnable:=VK_TRUE
    else result.rasterizerDiscardEnable:=VK_FALSE;
  if DepthBiasEnable
    then result.depthBiasEnable:=VK_TRUE
    else result.depthBiasEnable:=VK_FALSE;
end;

procedure TvkObj_PipelineRasterizationStateCreateInfo.SetStructure(const aData:TVkPipelineRasterizationStateCreateInfo);
begin
  Flags:=aData.flags;
  DepthClampEnable:=(aData.depthClampEnable <> 0);
  RasterizerDiscardEnable:=(aData.rasterizerDiscardEnable <> 0);
  PolygonMode:=aData.polygonMode;
  CullMode:=aData.cullMode;
  FrontFace:=aData.frontFace;
  DepthBiasEnable:=(aData.depthBiasEnable <> 0);
  DepthBiasConstantFactor:=aData.depthBiasConstantFactor;
  DepthBiasClamp:=aData.depthBiasClamp;
  DepthBiasSlopeFactor:=aData.depthBiasSlopeFactor;
  LineWidth:=aData.lineWidth;
end;

function TvkObj_PipelineMultisampleStateCreateInfo.GetStructure:TVkPipelineMultisampleStateCreateInfo;
var i:Integer;
begin
  if (fSampleMask.Length <> 0) then
    fSampleMask.Length:=32;

  FillByte(result, SizeOf(result), 0);
  result.sType :=VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
  result.pNext :=nil;
  result.flags :=Flags;
  result.rasterizationSamples:=RasterizationSamples;
  result.minSampleShading:=MinSampleShading;
  result.pSampleMask:=fSampleMask.PData;
  if SampleShadingEnable
    then result.sampleShadingEnable:=VK_TRUE
    else result.sampleShadingEnable:=VK_FALSE;
  if AlphaToCoverageEnable
    then result.alphaToCoverageEnable:=VK_TRUE
    else result.alphaToCoverageEnable:=VK_FALSE;
  if AlphaToOneEnable
    then result.alphaToOneEnable:=VK_TRUE
    else result.alphaToOneEnable:=VK_FALSE;
end;

procedure TvkObj_PipelineMultisampleStateCreateInfo.SetStructure(const aData:TVkPipelineMultisampleStateCreateInfo);
begin
  fSampleMask.SetData(aData.pSampleMask, 32);

  Flags :=aData.flags;
  RasterizationSamples:=aData.rasterizationSamples;
  SampleShadingEnable:=(aData.sampleShadingEnable <> 0);
  MinSampleShading:=aData.minSampleShading;
  AlphaToCoverageEnable:=(aData.alphaToCoverageEnable <> 0);
  AlphaToOneEnable:=(aData.alphaToOneEnable <> 0);
end;

constructor TvkObj_PipelineMultisampleStateCreateInfo.Create;
begin
  inherited Create;
  fSampleMask:=TVkSampleMaskList.Create;
end;

destructor TvkObj_PipelineMultisampleStateCreateInfo.Destroy;
begin
  FreeAndNil(fSampleMask);
  inherited Destroy;
end;

function TvkObj_PipelineDepthStencilStateCreateInfo.GetStructure:TVkPipelineDepthStencilStateCreateInfo;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=Flags;
  result.depthCompareOp:=DepthCompareOp;
  result.front:=Front;
  result.back:=Back;
  result.minDepthBounds:=MinDepthBounds;
  result.maxDepthBounds:=MaxDepthBounds;

  if DepthTestEnable
    then result.depthTestEnable:=VK_TRUE
    else result.depthTestEnable:=VK_FALSE;
  if DepthWriteEnable
    then result.depthWriteEnable:=VK_TRUE
    else result.depthWriteEnable:=VK_FALSE;
  if DepthBoundsTestEnable
    then result.depthBoundsTestEnable:=VK_TRUE
    else result.depthBoundsTestEnable:=VK_FALSE;
  if StencilTestEnable
    then result.stencilTestEnable:=VK_TRUE
    else result.stencilTestEnable:=VK_FALSE;
end;

procedure TvkObj_PipelineDepthStencilStateCreateInfo.SetStructure(const aData:TVkPipelineDepthStencilStateCreateInfo);
begin
  Flags := aData.flags;
  DepthTestEnable:=(aData.depthTestEnable <> 0);
  DepthWriteEnable:=(aData.depthWriteEnable <> 0);
  DepthCompareOp:= aData.depthCompareOp;
  DepthBoundsTestEnable:=(aData.depthBoundsTestEnable <> 0);
  StencilTestEnable:=(aData.stencilTestEnable <> 0);
  Front := aData.front;
  Back:= aData.back;
  MinDepthBounds:= aData.minDepthBounds;
  MaxDepthBounds:= aData.maxDepthBounds;
end;

function TvkObj_PipelineColorBlendAttachmentState.GetStructure:TVkPipelineColorBlendAttachmentState;
begin
  FillByte(result, SizeOf(result), 0);
  if BlendEnable
    then result.blendEnable:=VK_TRUE
    else result.blendEnable:=VK_FALSE;
  result.srcColorBlendFactor:=SrcColorBlendFactor;
  result.dstColorBlendFactor:=DstColorBlendFactor;
  result.colorBlendOp:=ColorBlendOp;
  result.srcAlphaBlendFactor:=SrcAlphaBlendFactor;
  result.dstAlphaBlendFactor:=DstAlphaBlendFactor;
  result.alphaBlendOp:=AlphaBlendOp;
  result.colorWriteMask:=ColorWriteMask;
end;

procedure TvkObj_PipelineColorBlendAttachmentState.SetStructure(const aData:TVkPipelineColorBlendAttachmentState);
begin
  BlendEnable:=(aData.blendEnable <> 0);
  SrcColorBlendFactor:= aData.srcColorBlendFactor;
  DstColorBlendFactor:= aData.dstColorBlendFactor;
  ColorBlendOp:= aData.colorBlendOp;
  SrcAlphaBlendFactor:= aData.srcAlphaBlendFactor;
  DstAlphaBlendFactor:= aData.dstAlphaBlendFactor;
  AlphaBlendOp:= aData.alphaBlendOp;
  ColorWriteMask:= aData.colorWriteMask;
end;

function TvkObj_PipelineColorBlendStateCreateInfo.GetStructure:TVkPipelineColorBlendStateCreateInfo;
var i:Integer;
begin
  SetLength(fvkAttachments, fAttachments.Length);
  for i:=low(fvkAttachments) to high(fvkAttachments) do
    fvkAttachments[i]:=fAttachments[i].GetStructure;

  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=Flags;
  result.logicOp:=LogicOp;
  result.attachmentCount:=Length(fvkAttachments);
  result.pAttachments:=@fvkAttachments[0];
  result.blendConstants:=BlendConstants;
  if LogicOpEnable
    then result.logicOpEnable:=VK_TRUE
    else result.logicOpEnable:=VK_FALSE;
end;

procedure TvkObj_PipelineColorBlendStateCreateInfo.SetStructure(const aData:TVkPipelineColorBlendStateCreateInfo);
var i:Integer;
begin
  if Assigned(aData.pAttachments) then begin
    fAttachments.Length:=aData.attachmentCount;
    for i:=0 to fAttachments.Length-1 do
      fAttachments[i].SetStructure((aData.pAttachments+i)^);
  end;

  Flags:= aData.flags;
  LogicOp:= aData.logicOp;
  LogicOpEnable:=(aData.logicOpEnable <> 0);
  BlendConstants:= aData.blendConstants;
end;

constructor TvkObj_PipelineColorBlendStateCreateInfo.Create;
begin
  inherited Create;
  fAttachments:=TvkObj_PipelineColorBlendAttachmentStateList.Create;
end;

destructor TvkObj_PipelineColorBlendStateCreateInfo.Destroy;
begin
  FreeAndNil(fAttachments);
  inherited Destroy;
end;

function TvkObj_PipelineDynamicStateCreateInfo.GetStructure:TVkPipelineDynamicStateCreateInfo;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=Flags;
  result.dynamicStateCount:=fDynamicStates.Length;
  result.pDynamicStates:=fDynamicStates.PData;
end;

procedure TvkObj_PipelineDynamicStateCreateInfo.SetStructure(const aData:TVkPipelineDynamicStateCreateInfo);
begin
  fDynamicStates.SetData(aData.pDynamicStates, aData.dynamicStateCount);
  Flags:=aData.flags;
end;

constructor TvkObj_PipelineDynamicStateCreateInfo.Create;
begin
  inherited Create;
  fDynamicStates:=TVkDynamicStateList.Create;
end;

destructor TvkObj_PipelineDynamicStateCreateInfo.Destroy;
begin
  FreeAndNil(fDynamicStates);
  inherited Destroy;
end;

function TvkObj_GraphicsPipelineFactory.GetStructure:TVkGraphicsPipelineCreateInfo;
var i:Integer;
begin
  SetLength(fvkStages, fStages.Length);
  for i:=low(fvkStages) to high(fvkStages) do
    fvkStages[i]:=fStages[i].GetStructure;

  fvkVertexInputState:=fVertexInputState.GetStructure;
  fvkInputAssemblyState:=fInputAssemblyState.GetStructure;
  fvkTessellationState:=fTessellationState.GetStructure;
  fvkViewportState:=fViewportState.GetStructure;
  fvkRasterizationState:=fRasterizationState.GetStructure;
  fvkMultisampleState:=fMultisampleState.GetStructure;
  fvkDepthStencilState:=fDepthStencilState.GetStructure;
  fvkColorBlendState:=fColorBlendState.GetStructure;
  fvkDynamicState:=fDynamicState.GetStructure;

  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.stageCount:=Length(fvkStages);
  result.pStages:=@fvkStages[0];
  result.pVertexInputState:=@fvkVertexInputState;
  result.pInputAssemblyState:=@fvkInputAssemblyState;
  result.pTessellationState:=@fvkTessellationState;
  result.pViewportState:=@fvkViewportState;
  result.pRasterizationState:=@fvkRasterizationState;
  result.pMultisampleState:=@fvkMultisampleState;
  result.pDepthStencilState:=@fvkDepthStencilState;
  result.pColorBlendState:=@fvkColorBlendState;
  result.pDynamicState:=@fvkDynamicState;
  result.layout:=fLayout;
  result.renderPass:=fRenderPass;
  result.subpass:=fSubPass;
  result.basePipelineHandle:=fBasePipelineHandle;
  result.basePipelineIndex:=fBasePipelineIndex;
end;

procedure TvkObj_GraphicsPipelineFactory.SetStructure(const aData:TVkGraphicsPipelineCreateInfo);
var i:Integer;
begin
  if Assigned(aData.pStages) then begin
    fStages.Length:=aData.stageCount;
    for i:=0 to aData.stageCount-1 do
      fStages[i].SetStructure((aData.pStages+i)^);
  end;

  fFlags:=aData.flags;
  fLayout:=aData.layout;
  fRenderPass:=aData.renderPass;
  fSubPass:=aData.subpass;
  fBasePipelineHandle:=aData.basePipelineHandle;
  fBasePipelineIndex:=aData.basePipelineIndex;
end;

function TvkObj_GraphicsPipelineFactory.CreateGraphicsPipeline(const aPipelineCache:TVkPipelineCache):TvkObj_GraphicsPipeline;
begin
  result:=TvkObj_GraphicsPipeline.Create(GetStructure, aPipelineCache, DeviceFunctions, AllocCallbacks);
end;

procedure TvkObj_GraphicsPipelineFactory.AfterConstruction;
begin
  inherited AfterConstruction;
  fStages:=TvkObj_PipelineShaderStageCreateInfoList.Create;
  fVertexInputState:=TvkObj_PipelineVertexInputStateCreateInfo.Create;
  fInputAssemblyState:=TvkObj_PipelineInputAssemblyStateCreateInfo.Create;
  fTessellationState:=TvkObj_PipelineTessellationStateCreateInfo.Create;
  fViewportState:=TvkObj_PipelineViewportStateCreateInfo.Create;
  fRasterizationState:=TvkObj_PipelineRasterizationStateCreateInfo.Create;
  fMultisampleState:=TvkObj_PipelineMultisampleStateCreateInfo.Create;
  fDepthStencilState:=TvkObj_PipelineDepthStencilStateCreateInfo.Create;
  fColorBlendState:=TvkObj_PipelineColorBlendStateCreateInfo.Create;
  fDynamicState:=TvkObj_PipelineDynamicStateCreateInfo.Create;
end;

procedure TvkObj_GraphicsPipelineFactory.BeforeDestruction;
begin
  FreeAndNil(fDynamicState);
  FreeAndNil(fColorBlendState);
  FreeAndNil(fDepthStencilState);
  FreeAndNil(fMultisampleState);
  FreeAndNil(fRasterizationState);
  FreeAndNil(fViewportState);
  FreeAndNil(fTessellationState);
  FreeAndNil(fInputAssemblyState);
  FreeAndNil(fVertexInputState);
  FreeAndNil(fStages);
  inherited BeforeDestruction;
end;


procedure TvkObj_PipelineLayout.CreateHandle(const aCreateInfo:TVkPipelineLayoutCreateInfo);
var err:TVkResult;
begin
  if not Assigned(fDeviceFunctions.vkCreatePipelineLayout) then
    raise TvkObj_InvalidFuncPtrException.Create('vkCreatePipelineLayout');
  err:=fDeviceFunctions.vkCreatePipelineLayout(fDeviceFunctions.aaDevice, @aCreateInfo, AllocCallbacks, @fHandle);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to create pipeline layout', err);
end;

constructor TvkObj_PipelineLayout.Create(const aCreateInfo:TVkPipelineLayoutCreateInfo;
  const aDeviceFunctions:TVkDeviceFunctions; const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fDeviceFunctions:=aDeviceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(aCreateInfo);
end;

constructor TvkObj_PipelineLayout.Create(const aHandle:TVkPipelineLayout; const aOwnsHandle:Boolean;
  const aDeviceFunctions:TVkDeviceFunctions; const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=aHandle;
  fDeviceFunctions:=aDeviceFunctions;
  OwnsHandle:=aOwnsHandle;
  SetAllocCallbacks(aAllocCallbacks);
end;

destructor TvkObj_PipelineLayout.Destroy;
begin
  if OwnsHandle and (fHandle <> VK_INVALID_NDP_HANDLE) then
    fDeviceFunctions.vkDestroyPipelineLayout(fDeviceFunctions.aaDevice, fHandle, AllocCallbacks);
  fHandle:=VK_INVALID_NDP_HANDLE;
  inherited Destroy;
end;


function TvkObj_PushConstantRange.GetStructure:TVkPushConstantRange;
begin
  FillByte(result, SizeOf(result), 0);
  result.stageFlags:=StageFlags;
  result.offset:=Offset;
  result.size:=Size;
end;

procedure TvkObj_PushConstantRange.SetStructure(const aData:TVkPushConstantRange);
begin
  StageFlags:=aData.stageFlags;
  Offset:=aData.offset;
  Size:=aData.size;
end;

function TvkObj_PipelineLayoutFactory.GetStructure:TVkPipelineLayoutCreateInfo;
var i:Integer;
begin
  SetLength(fvkPushConstantRanges, fPushConstantRanges.Length);
  for i:=low(fvkPushConstantRanges) to high(fvkPushConstantRanges) do
    fvkPushConstantRanges[i]:=fPushConstantRanges[i].GetStructure;

  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.setLayoutCount:=fSetLayouts.Length;
  result.pSetLayouts:=fSetLayouts.PData;
  result.pushConstantRangeCount:=Length(fvkPushConstantRanges);
  result.pPushConstantRanges:=@fvkPushConstantRanges[0];
end;

procedure TvkObj_PipelineLayoutFactory.SetStructure(const aData:TVkPipelineLayoutCreateInfo);
 var i:Integer;
begin
  fFlags:=aData.flags;
  fSetLayouts.SetData(aData.pSetLayouts, aData.setLayoutCount);
  fPushConstantRanges.Length:=aData.pushConstantRangeCount;
  for i:=0 to fPushConstantRanges.Length-1 do
    fPushConstantRanges[i].SetStructure((aData.pPushConstantRanges+i)^);
end;

function TvkObj_PipelineLayoutFactory.CreatePipelineLayout:TvkObj_PipelineLayout;
begin
  result:=TvkObj_PipelineLayout.Create(GetStructure, DeviceFunctions, AllocCallbacks);
end;

procedure TvkObj_PipelineLayoutFactory.AfterConstruction;
begin
  inherited AfterConstruction;
  fSetLayouts:=TVkDescriptorSetLayoutList.Create;
  fPushConstantRanges:=TvkObj_PushConstantRangeList.Create;
end;

procedure TvkObj_PipelineLayoutFactory.BeforeDestruction;
begin
  FreeAndNil(fPushConstantRanges);
  FreeAndNil(fSetLayouts);
  inherited BeforeDestruction;
end;

//============= TvkObj_Queue ==================================

constructor TvkObj_Queue.Create(const aHandle:TVkQueue; const aDeviceFunctions:TVkDeviceFunctions);
begin
  inherited Create;
  fHandle:=aHandle;
  fDeviceFunctions:=aDeviceFunctions;
end;

procedure TvkObj_Queue.Submit(const aWaitSemaphores:array of TVkSemaphore;
                              const aWaitDstStageMask:array of TVkSemaphore;
                              const aCommandBuffers:array of TVkCommandBuffer;
                              const aSignalSemaphores:array of TVkSemaphore;
                              const aFence:TVkFence);
var
  SubmitInfo:TVkSubmitInfo;
  err:TVkResult;
begin
  if     (Length(aWaitSemaphores)   <> Length(aWaitDstStageMask))
     and (Length(aWaitDstStageMask) <> 0) then
    raise TvkObj_Exception.Create('aWaitDstStageMask must have the same number of elements as aWaitSemaphores or must be empty');

  FillByte(SubmitInfo, SizeOf(SubmitInfo), 0);
  SubmitInfo.sType:=VK_STRUCTURE_TYPE_SUBMIT_INFO;
  SubmitInfo.pNext:=nil;
  SubmitInfo.waitSemaphoreCount:=Length(aWaitSemaphores);
  SubmitInfo.pWaitSemaphores:=@aWaitSemaphores[0];
  SubmitInfo.pWaitDstStageMask:=@aWaitDstStageMask[0];
  SubmitInfo.commandBufferCount:=Length(aCommandBuffers);
  SubmitInfo.pCommandBuffers:=@aCommandBuffers[0];
  SubmitInfo.signalSemaphoreCount:=Length(aSignalSemaphores);
  SubmitInfo.pSignalSemaphores:=@aSignalSemaphores[0];
  err:=fDeviceFunctions.vkQueueSubmit(fHandle, 1, @SubmitInfo, aFence);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to submit queue', err);
end;

function TvkObj_Queue.Present(const aWaitSemaphore:array of TVkSemaphore;
                              const aSwapChains:array of TVkSwapchainKHR;
                              const aImageIndices:array of TVkUInt32):TVkResultArr;
var
  info:TVkPresentInfoKHR;
  err:TVkResult;
begin
  if (Length(aSwapChains) <> Length(aImageIndices)) then
    raise TvkObj_Exception.Create('aSwapChains and aImageIndices must have the same number of elements');

  SetLength(result, Length(aSwapChains));
  FillByte(info, SizeOf(info), 0);
  info.sType:=VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
  info.pNext:=nil;
  info.waitSemaphoreCount:=Length(aWaitSemaphore);
  info.pWaitSemaphores:=@aWaitSemaphore[0];
  info.swapchainCount:=Length(aSwapChains);
  info.pSwapchains:=@aSwapChains[0];
  info.pImageIndices:=@aImageIndices[0];
  info.pResults:=@result[0];
  err:=fDeviceFunctions.vkQueuePresentKHR(fHandle, @info);

  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to present queue', err);
end;

procedure TvkObj_Queue.WaitIdle;
 var err:TVkResult;
begin
  err:=fDeviceFunctions.vkQueueWaitIdle(fHandle);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('error while waiting for queue', err);
end;

//============= TvkObj_RenderPass ==================================


constructor TvkObj_RenderPass.Create(const aCreateInfo:TVkRenderPassCreateInfo;
                                     const aDeviceFunctions:TVkDeviceFunctions;
                                     const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fDeviceFunctions:=aDeviceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(aCreateInfo);
end;

constructor TvkObj_RenderPass.Create(const aHandle:TVkRenderPass;
                                     const aOwnsHandle:Boolean;
                                     const aDeviceFunctions:TVkDeviceFunctions;
                                     const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=aHandle;
  fDeviceFunctions:=aDeviceFunctions;
  OwnsHandle:=aOwnsHandle;
  SetAllocCallbacks(aAllocCallbacks);
end;

destructor TvkObj_RenderPass.Destroy;
begin
  if OwnsHandle and (fHandle <> VK_INVALID_NDP_HANDLE) then
    fDeviceFunctions.vkDestroyRenderPass(fDeviceFunctions.aaDevice, fHandle, AllocCallbacks);
  fHandle:=VK_INVALID_NDP_HANDLE;
  inherited Destroy;
end;

procedure TvkObj_RenderPass.CreateHandle(const aCreateInfo:TVkRenderPassCreateInfo);
 var err:TVkResult;
begin
  if not Assigned(fDeviceFunctions.vkCreateRenderPass) then
    raise TvkObj_InvalidFuncPtrException.Create('vkCreateRenderPass');

  err:=fDeviceFunctions.vkCreateRenderPass(fDeviceFunctions.aaDevice, @aCreateInfo, AllocCallbacks, @fHandle);

  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to create render pass', err);
end;

//============= TvkObj_AttachmentDescription ==================================

function TvkObj_AttachmentDescription.GetStructure:TVkAttachmentDescription;
begin
  result.flags:=Flags;
  result.format:=Format;
  result.samples:=Samples;
  result.loadOp:=LoadOp;
  result.storeOp:=StoreOp;
  result.stencilLoadOp:=StencilLoadOp;
  result.stencilStoreOp:=StencilStoreOp;
  result.initialLayout:=InitialLayout;
  result.finalLayout:=FinalLayout;
end;

procedure TvkObj_AttachmentDescription.SetStructure(const aData:TVkAttachmentDescription);
begin
  Flags:=aData.flags;
  Format:=aData.format;
  Samples:=aData.samples;
  LoadOp:=aData.loadOp;
  StoreOp:=aData.storeOp;
  StencilLoadOp:=aData.stencilLoadOp;
  StencilStoreOp:=aData.stencilStoreOp;
  InitialLayout:=aData.initialLayout;
  FinalLayout:=aData.finalLayout;
end;

function TvkObj_AttachmentReference.GetStructure:TVkAttachmentReference;
begin
  FillByte(result, SizeOf(result), 0);
  result.attachment:=Attachment;
  result.layout:=Layout;
end;

procedure TvkObj_AttachmentReference.SetStructure(const aData:TVkAttachmentReference);
begin
  Attachment:=aData.attachment;
  Layout:=aData.layout;
end;

//============= TvkObj_SubpassDescription ==================================

function TvkObj_SubpassDescription.GetStructure:TVkSubpassDescription;
var i:Integer;
begin
  if (fResolveAttachments.Length <> 0) and
     (fResolveAttachments.Length <> fColorAttachments.Length) then
     raise TvkObj_Exception.Create('''ResolveAttachments'' must be empty or have the same size as ''ColorAttachments''');

  if (fDepthStencilAttachments.Length <> fColorAttachments.Length) then
     raise TvkObj_Exception.Create('''DepthStencilAttachments'' must have the same size as ''ColorAttachments''');

  SetLength(fvkInputAttachments, fInputAttachments.Length);
  for i:=low(fvkInputAttachments) to high(fvkInputAttachments) do
    fvkInputAttachments[i]:=fInputAttachments[i].GetStructure;

  SetLength(fvkColorAttachments, fColorAttachments.Length);
  for i:=low(fvkColorAttachments) to high(fvkColorAttachments) do
    fvkColorAttachments[i]:=fColorAttachments[i].GetStructure;

  SetLength(fvkResolveAttachments, fResolveAttachments.Length);
  for i:=low(fvkResolveAttachments) to high(fvkResolveAttachments) do
    fvkResolveAttachments[i]:=fResolveAttachments[i].GetStructure;

  SetLength(fvkDepthStencilAttachments, fDepthStencilAttachments.Length);
  for i:=low(fvkDepthStencilAttachments) to high(fvkDepthStencilAttachments) do
    fvkDepthStencilAttachments[i]:=fDepthStencilAttachments[i].GetStructure;

  result.flags:=Flags;
  result.pipelineBindPoint:=PipelineBindPoint;
  result.inputAttachmentCount:=Length(fvkInputAttachments);
  result.pInputAttachments:=@fvkInputAttachments[0];
  result.colorAttachmentCount:=Length(fvkColorAttachments);
  result.pColorAttachments:=@fvkColorAttachments[0];
  result.pResolveAttachments:=@fvkResolveAttachments[0];
  result.pDepthStencilAttachment:=@fvkDepthStencilAttachments[0];
  result.preserveAttachmentCount:=fPreserveAttachments.Length;
  result.pPreserveAttachments:=fPreserveAttachments.PData;
end;

procedure TvkObj_SubpassDescription.SetStructure(const aData:TVkSubpassDescription);
var i:Integer;
begin
  Flags:=aData.flags;
  PipelineBindPoint:=aData.pipelineBindPoint;

  if Assigned(aData.pInputAttachments) then begin
    fInputAttachments.Length:=aData.inputAttachmentCount;
    for i:=0 to aData.inputAttachmentCount-1 do
      fInputAttachments[i].SetStructure((aData.pInputAttachments+i)^);
  end else
    fInputAttachments.Length:=0;

  if Assigned(aData.pResolveAttachments) then begin
    fResolveAttachments.Length:=aData.colorAttachmentCount;
    for i:=0 to aData.colorAttachmentCount-1 do
      fResolveAttachments[i].SetStructure((aData.pResolveAttachments+i)^);
  end else
    fResolveAttachments.Length:=0;

  if Assigned(aData.pDepthStencilAttachment) then begin
    fDepthStencilAttachments.Length:=aData.colorAttachmentCount;
    for i:=0 to aData.colorAttachmentCount-1 do
      fDepthStencilAttachments[i].SetStructure((aData.pDepthStencilAttachment+i)^);
  end else
    fDepthStencilAttachments.Length:=0;

  fPreserveAttachments.SetData(aData.pPreserveAttachments, aData.preserveAttachmentCount);
end;

procedure TvkObj_SubpassDescription.AfterConstruction;
begin
  inherited AfterConstruction;
  fInputAttachments:=TvkObj_AttachmentReferenceList.Create;
  fColorAttachments:=TvkObj_AttachmentReferenceList.Create;
  fResolveAttachments:=TvkObj_AttachmentReferenceList.Create;
  fDepthStencilAttachments:=TvkObj_AttachmentReferenceList.Create;
  fPreserveAttachments:=TvkInt32List.Create;
end;

procedure TvkObj_SubpassDescription.BeforeDestruction;
begin
  FreeAndNil(fPreserveAttachments);
  FreeAndNil(fDepthStencilAttachments);
  FreeAndNil(fResolveAttachments);
  FreeAndNil(fColorAttachments);
  FreeAndNil(fInputAttachments);
  inherited BeforeDestruction;
end;

//============= TvkObj_SubpassDependency ==================================

function TvkObj_SubpassDependency.GetStructure:TVkSubpassDependency;
begin
  result.srcSubpass:=SrcSubpass;
  result.dstSubpass:=DstSubpass;
  result.srcStageMask:=SrcStageMask;
  result.dstStageMask:=DstStageMask;
  result.srcAccessMask:=SrcAccessMask;
  result.dstAccessMask:=DstAccessMask;
  result.dependencyFlags:=DependencyFlags;
end;

procedure TvkObj_SubpassDependency.SetStructure(const aData:TVkSubpassDependency);
begin
  SrcSubpass:=aData.srcSubpass;
  DstSubpass:=aData.dstSubpass;
  SrcStageMask:=aData.srcStageMask;
  DstStageMask:=aData.dstStageMask;
  SrcAccessMask:=aData.srcAccessMask;
  DstAccessMask:=aData.dstAccessMask;
  DependencyFlags:=aData.dependencyFlags;
end;

//============= TvkObj_RenderPassFactory ==================================

function TvkObj_RenderPassFactory.GetStructure:TVkRenderPassCreateInfo;
var i:Integer;
begin
  SetLength(fvkAttachments, fAttachments.Length);
  for i:=low(fvkAttachments) to high(fvkAttachments) do
    fvkAttachments[i]:=fAttachments[i].GetStructure;

  SetLength(fvkSubpasses, fSubpasses.Length);
  for i:=low(fvkSubpasses) to high(fvkSubpasses) do
    fvkSubpasses[i]:=fSubpasses[i].GetStructure;

  SetLength(fvkDependencies, fDependencies.Length);
  for i:=low(fvkDependencies) to high(fvkDependencies) do
    fvkDependencies[i]:=fDependencies[i].GetStructure;

  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.attachmentCount:=Length(fvkAttachments);
  result.pAttachments:=@fvkAttachments[0];
  result.subpassCount:=Length(fvkSubpasses);
  result.pSubpasses:=@fvkSubpasses[0];
  result.dependencyCount:=Length(fvkDependencies);
  result.pDependencies:=@fvkDependencies[0];
end;

procedure TvkObj_RenderPassFactory.SetStructure(const aData:TVkRenderPassCreateInfo);
var i:Integer;
begin
  fFlags:=aData.flags;

  fAttachments.Length:=aData.attachmentCount;
  for i:=0 to fAttachments.Length-1 do
    fAttachments[i].SetStructure((aData.pAttachments+i)^);

  fSubpasses.Length:=aData.subpassCount;
  for i:=0 to fSubpasses.Length-1 do
    fSubpasses[i].SetStructure((aData.pSubpasses+i)^);

  fDependencies.Length:=aData.dependencyCount;
  for i:=0 to fDependencies.Length-1 do
    fDependencies[i].SetStructure((aData.pDependencies+i)^);
end;

function TvkObj_RenderPassFactory.CreateRenderPass:TvkObj_RenderPass;
begin
  result:=TvkObj_RenderPass.Create(GetStructure, DeviceFunctions, AllocCallbacks);
end;

procedure TvkObj_RenderPassFactory.AfterConstruction;
begin
  inherited AfterConstruction;
  fAttachments:=TvkObj_AttachmentDescriptionList.Create;
  fSubpasses:=TvkObj_SubpassDescriptionList.Create;
  fDependencies:=TvkObj_SubpassDependencyList.Create;
end;

procedure TvkObj_RenderPassFactory.BeforeDestruction;
begin
  FreeAndNil(fDependencies);
  FreeAndNil(fSubpasses);
  FreeAndNil(fAttachments);
  inherited BeforeDestruction;
end;

//============= TvkObj_Semaphore ==================================

constructor TvkObj_Semaphore.Create(const aFlags:TVkSemaphoreCreateFlags; const aDeviceFunctions:TVkDeviceFunctions;
  const aAllocCallbacks:PVkAllocationCallbacks);
var CreateInfo:TVkSemaphoreCreateInfo;
begin
  FillByte(CreateInfo, SizeOf(CreateInfo), 0);
  CreateInfo.sType:=VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
  CreateInfo.pNext:=nil;
  CreateInfo.flags:=aFlags;
  Create(CreateInfo, aDeviceFunctions, aAllocCallbacks);
end;

constructor TvkObj_Semaphore.Create(const aCreateInfo:TVkSemaphoreCreateInfo; const aDeviceFunctions:TVkDeviceFunctions;
  const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fDeviceFunctions:=aDeviceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(aCreateInfo);
end;

constructor TvkObj_Semaphore.Create(const aHandle:TVkSemaphore; const aOwnsHandle:Boolean;
  const aDeviceFunctions:TVkDeviceFunctions; const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=aHandle;
  fDeviceFunctions:=aDeviceFunctions;
  OwnsHandle:=aOwnsHandle;
  SetAllocCallbacks(aAllocCallbacks);
end;

destructor TvkObj_Semaphore.Destroy;
begin
  if OwnsHandle and (fHandle <> VK_INVALID_NDP_HANDLE) then
    fDeviceFunctions.vkDestroySemaphore(fDeviceFunctions.aaDevice, fHandle, AllocCallbacks);
  fHandle:=VK_INVALID_NDP_HANDLE;
  inherited Destroy;
end;
         
procedure TvkObj_Semaphore.CreateHandle(const aCreateInfo:TVkSemaphoreCreateInfo);
var err:TVkResult;
begin
  if not Assigned(fDeviceFunctions.vkCreateSemaphore) then
    raise TvkObj_InvalidFuncPtrException.Create('vkCreateSemaphore');
  err:=fDeviceFunctions.vkCreateSemaphore(fDeviceFunctions.aaDevice, @aCreateInfo, AllocCallbacks, @fHandle);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to create semaphore', err);
end;

//============= TvkObj_ShaderModule ==================================

constructor TvkObj_ShaderModule.Create(const aCreateInfo:TVkShaderModuleCreateInfo;
                                       const aDeviceFunctions:TVkDeviceFunctions;
                                       const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fDeviceFunctions:=aDeviceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(aCreateInfo);
end;

constructor TvkObj_ShaderModule.Create(const aHandle:TVkShaderModule;
                                       const aOwnsHandle:Boolean;
                                       const aDeviceFunctions:TVkDeviceFunctions;
                                       const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=aHandle;
  fDeviceFunctions:=aDeviceFunctions;
  OwnsHandle:=aOwnsHandle;
  SetAllocCallbacks(aAllocCallbacks);
end;

destructor TvkObj_ShaderModule.Destroy;
begin
  if OwnsHandle and (fHandle <> VK_INVALID_NDP_HANDLE) then
    fDeviceFunctions.vkDestroyShaderModule(fDeviceFunctions.aaDevice, fHandle, AllocCallbacks);

  fHandle:=VK_INVALID_NDP_HANDLE;
  inherited Destroy;
end;

procedure TvkObj_ShaderModule.CreateHandle(const aCreateInfo:TVkShaderModuleCreateInfo);
 var err:TVkResult;
begin
  if not Assigned(fDeviceFunctions.vkCreateShaderModule) then
    raise TvkObj_InvalidFuncPtrException.Create('vkCreateShaderModule');

  err:=fDeviceFunctions.vkCreateShaderModule(fDeviceFunctions.aaDevice, @aCreateInfo, AllocCallbacks, @fHandle);

  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to create shader module', err);
end;

//============= TvkObj_ShaderModuleFactory ==================================

procedure TvkObj_ShaderModuleFactory.ReallocShaderCode(const aSize:TVkSize);
begin
  if Assigned(fShaderCode) then
    Freememory(fShaderCode);
  fShaderCodeSize:=aSize;
  if (fShaderCodeSize>0) then
    fShaderCode:=GetMemory(fShaderCodeSize);
end;

procedure TvkObj_ShaderModuleFactory.SetGlslCode(const aCode:PVkVoid; const aSize:TVkSize; const aStage:TVkShaderStageFlags);
 var p:PVkUint32;
begin
  ReallocShaderCode(3 * SizeOf(TVkUInt32)+aSize+1);
  p:=fShaderCode;
  (p+0)^:=$07230203;
  (p+1)^:=0;
  (p+2)^:=TVkUInt32(aStage);
  Move(aCode^, (p+3)^, aSize);
  PByte(fShaderCode+3 * SizeOf(TVkUInt32)+aSize)^:=0;
end;

procedure TvkObj_ShaderModuleFactory.SetShaderCode(const aCode:PVkVoid; const aSize:TVkSize);
begin
  ReallocShaderCode(aSize);
  Move(aCode^, fShaderCode^, fShaderCodeSize);
end;

procedure TvkObj_ShaderModuleFactory.LoadSpvCode(const aStream:TStream; aSize:TVkSize);
begin
  if (aSize=0) then
    aSize:=aStream.Size - aStream.Position;
  ReallocShaderCode(aSize);
  aStream.Read(fShaderCode^, aSize);
end;

procedure TvkObj_ShaderModuleFactory.LoadSpvCode(const aFilename:String);
var fs:TFileStream;
begin
  fs:=TFileStream.Create(aFilename, fmOpenRead);
  try
    LoadSpvCode(fs);
  finally
    FreeAndNil(fs);
  end;
end;

procedure TvkObj_ShaderModuleFactory.SetGlslCode(const aCode:String; const aStage:TVkShaderStageFlags);
begin
  SetGlslCode(PVkVoid(PAnsiChar(aCode)), Length(aCode), aStage);
end;

procedure TvkObj_ShaderModuleFactory.LoadGlslCode(const aStream:TStream; const aStage:TVkShaderStageFlags; aSize:TVkSize);
 var mem:PVkVoid;
begin
  if (aSize=0) then
    aSize:=aStream.Size - aStream.Position;
  mem:=GetMemory(aSize);
  try
    aStream.Read(mem^, aSize);
    SetGlslCode(mem, aSize, aStage);
  finally
    FreeMemory(mem);
  end;
end;

procedure TvkObj_ShaderModuleFactory.LoadGlslCode(const aFilename:String; const aStage:TVkShaderStageFlags);
 var fs:TFileStream;
begin
  fs:=TFileStream.Create(aFilename, fmOpenRead);
  try
    LoadGlslCode(fs, aStage);
  finally
    FreeAndNil(fs);
  end;
end;

function TvkObj_ShaderModuleFactory.GetStructure:TVkShaderModuleCreateInfo;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.codeSize:=fShaderCodeSize;
  result.pCode:=fShaderCode;
end;

procedure TvkObj_ShaderModuleFactory.SetStructure(const aData:TVkShaderModuleCreateInfo);
begin
  fFlags:=aData.flags;
  SetShaderCode(aData.pCode, aData.codeSize);
end;

function TvkObj_ShaderModuleFactory.CreateShaderModule:TvkObj_ShaderModule;
 var
  CreateInfo:TVkShaderModuleCreateInfo;
begin
  FillByte(CreateInfo, SizeOf(CreateInfo), 0);
  CreateInfo.sType:=VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
  CreateInfo.pNext:=nil;
  CreateInfo.flags:=fFlags;
  CreateInfo.codeSize:=fShaderCodeSize;
  CreateInfo.pCode:=fShaderCode;
  result:=TvkObj_ShaderModule.Create(CreateInfo, DeviceFunctions, AllocCallbacks);
end;

procedure TvkObj_ShaderModuleFactory.BeforeDestruction;
begin
  inherited BeforeDestruction;
  ReallocShaderCode(0);
end;

//============= TvkObj_SwapChain ==================================

constructor TvkObj_SwapChain.Create(const aCreateInfo:TVkSwapchainCreateInfoKHR;
                                    const aDeviceFunctions:TVkDeviceFunctions;
                                    const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fDeviceFunctions:=aDeviceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(@aCreateInfo);
end;

constructor TvkObj_SwapChain.Create(const aHandle:TVkSwapchainKHR;
                                    const aOwnsHandle:Boolean;
                                    const aDeviceFunctions:TVkDeviceFunctions;
                                    const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=aHandle;
  fDeviceFunctions:=aDeviceFunctions;
  OwnsHandle:=aOwnsHandle;
  SetAllocCallbacks(aAllocCallbacks);
end;

destructor TvkObj_SwapChain.Destroy;
begin
  if OwnsHandle and (fHandle <> VK_INVALID_NDP_HANDLE) then
    fDeviceFunctions.vkDestroySwapchainKHR(fDeviceFunctions.aaDevice, fHandle, AllocCallbacks);

  fHandle:=VK_INVALID_NDP_HANDLE;
  inherited Destroy;
end;

procedure TvkObj_SwapChain.CreateHandle(const aCreateInfo:PVkSwapchainCreateInfoKHR);
 var err:TVkResult;
begin
  if not Assigned(fDeviceFunctions.vkCreateSwapchainKHR) then
    raise TvkObj_Exception.Create('invalid function pointer:vkCreateSwapchainKHR; did you enabled the VK_KHR_swapchain extension?');
  err:=fDeviceFunctions.vkCreateSwapchainKHR(fDeviceFunctions.aaDevice, aCreateInfo, AllocCallbacks, @fHandle);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to create swap chain', err);
end;

function TvkObj_SwapChain.GetImages:TVkImageArr;
 var
  c:TVkUInt32;
  err:TVkResult;
begin
  if not Assigned(fDeviceFunctions.vkGetSwapchainImagesKHR) then
    raise TvkObj_InvalidFuncPtrException.Create('vkGetSwapchainImagesKHR', 'VK_KHR_swapchain');

  err:=fDeviceFunctions.vkGetSwapchainImagesKHR(fDeviceFunctions.aaDevice, fHandle, @c, nil);

  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to get swap chain image count', err);

  SetLength(result, c);
  err:=fDeviceFunctions.vkGetSwapchainImagesKHR(fDeviceFunctions.aaDevice, fHandle, @c, @result[0]);

  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to get swap chain images', err);
end;

function TvkObj_SwapChain.AcquireNextImage(const aTimeout:TVkUInt64; const aSemaphore:TVkSemaphore; const aFence:TVkFence):TVkUInt32;
 var err:TVkResult;
begin
  if not Assigned(fDeviceFunctions.vkAcquireNextImageKHR) then
    raise TvkObj_InvalidFuncPtrException.Create('vkAcquireNextImageKHR', 'VK_KHR_swapchain');
  err:=fDeviceFunctions.vkAcquireNextImageKHR(fDeviceFunctions.aaDevice, fHandle, aTimeout, aSemaphore, aFence, @result);
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to aquire next image', err);
end;

//============= TvkObj_SwapChainFactory ==================================

function TvkObj_SwapChainFactory.GetStructure:TVkSwapchainCreateInfoKHR;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.surface:=fSurface;
  result.minImageCount:=fMinImageCount;
  result.imageFormat:=fImageFormat;
  result.imageColorSpace:=fImageColorSpace;
  result.imageExtent:=fImageExtent;
  result.imageArrayLayers:=fImageArrayLayers;
  result.imageUsage:=fImageUsage;
  result.imageSharingMode:=fImageSharingMode;
  result.queueFamilyIndexCount:=fQueueFamilyIndices.Length;
  result.pQueueFamilyIndices:=fQueueFamilyIndices.PData;
  result.preTransform:=fPreTransform;
  result.compositeAlpha:=fComposideAlpha;
  result.presentMode:=fPresentMode;
  result.oldSwapchain:=fOldSwapChain;

  if fClipped
    then result.clipped:=VK_TRUE
    else result.clipped:=VK_FALSE;
end;

procedure TvkObj_SwapChainFactory.SetStructure(const aData:TVkSwapchainCreateInfoKHR);
begin
  fFlags:=aData.flags;
  fSurface:=aData.surface;
  fMinImageCount:=aData.minImageCount;
  fImageFormat:=aData.imageFormat;
  fImageColorSpace:=aData.imageColorSpace;
  fImageExtent:=aData.imageExtent;
  fImageArrayLayers:=aData.imageArrayLayers;
  fImageUsage:=aData.imageUsage;
  fImageSharingMode:=aData.imageSharingMode;
  fPreTransform:=aData.preTransform;
  fComposideAlpha:=aData.compositeAlpha;
  fPresentMode:=aData.presentMode;
  fOldSwapChain:=aData.oldSwapchain;
  fClipped:=(aData.clipped <> 0);

  fQueueFamilyIndices.SetData(aData.pQueueFamilyIndices, aData.queueFamilyIndexCount);
end;

function TvkObj_SwapChainFactory.CreateSwapchain:TvkObj_SwapChain;
begin
  result:=TvkObj_SwapChain.Create(GetStructure, DeviceFunctions, AllocCallbacks);
end;

procedure TvkObj_SwapChainFactory.AfterConstruction;
begin
  inherited AfterConstruction;
  fPresentMode:=VK_PRESENT_MODE_FIFO_KHR;
  fQueueFamilyIndices:=TvkInt32List.Create;
end;

procedure TvkObj_SwapChainFactory.BeforeDestruction;
begin
  FreeAndNil(fQueueFamilyIndices);
  inherited BeforeDestruction;
end;


//========================================================================
//========================================================================
//=============== TvkObj_Surface =========================================

constructor TvkObj_Surface.Create(const aHandle:TVkSurfaceKHR;
                                  const aOwnsHandle:Boolean;
                                  const aInstanceFunctions:TVkInstanceFunctions;
                                  const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fHandle:=aHandle;
  fInstanceFunctions:=aInstanceFunctions;
  OwnsHandle:=aOwnsHandle;
  SetAllocCallbacks(aAllocCallbacks);
end;

destructor TvkObj_Surface.Destroy;
begin
  if OwnsHandle and (fHandle <> VK_INVALID_NDP_HANDLE) then
    fInstanceFunctions.vkDestroySurfaceKHR(fInstanceFunctions.aaInstance, fHandle, AllocCallbacks);
  fHandle:=VK_INVALID_NDP_HANDLE;
  inherited Destroy;
end;

{$IF DEFINED(VK_USE_PLATFORM_WIN32_KHR)}
constructor TvkObj_Surface.Create(const aCreateInfo:TVkWin32SurfaceCreateInfoKHR;
                                  const aInstanceFunctions:TVkInstanceFunctions;
                                  const aAllocCallbacks:PVkAllocationCallbacks);
begin
  inherited Create;
  fInstanceFunctions:=aInstanceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(@aCreateInfo);
end;

{$ELSEIF DEFINED(VK_USE_PLATFORM_XLIB_KHR)}
constructor TvkObj_Surface.Create(const aCreateInfo:TVkXlibSurfaceCreateInfoKHR;
                                  const aInstanceFunctions:TVkInstanceFunctions;
                                  const aAllocCallbacks:PVkAllocationCallbacks=nil);
begin
  inherited Create;
  fInstanceFunctions:=aInstanceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(@aCreateInfo);
end;
{$ELSEIF DEFINED(VK_USE_PLATFORM_XCB_KHR)}
constructor TvkObj_Surface.Create(const aCreateInfo:TVkXcbSurfaceCreateInfoKHR;
                                  const aInstanceFunctions:TVkInstanceFunctions;
                                  const aAllocCallbacks:PVkAllocationCallbacks=nil);
begin
  inherited Create;
  fInstanceFunctions:=aInstanceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(@aCreateInfo);
end;
{$ELSEIF DEFINED(VK_USE_PLATFORM_WAYLAND_KHR)}
constructor TvkObj_Surface.Create(const aCreateInfo:TVkWaylandSurfaceCreateInfoKHR;
                                  const aInstanceFunctions:TVkInstanceFunctions;
                                  const aAllocCallbacks:PVkAllocationCallbacks=nil);
begin
  inherited Create;
  fInstanceFunctions:=aInstanceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(@aCreateInfo);
end;
{$ELSEIF DEFINED(VK_USE_PLATFORM_MIR_KHR)}
constructor TvkObj_Surface.Create(const aCreateInfo:TVkMirSurfaceCreateInfoKHR;
                                  const aInstanceFunctions:TVkInstanceFunctions;
                                  const aAllocCallbacks:PVkAllocationCallbacks=nil);
begin
  inherited Create;
  fInstanceFunctions:=aInstanceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(@aCreateInfo);
end;
{$ELSEIF DEFINED(VK_USE_PLATFORM_ANDROID_KHR)}
constructor TvkObj_Surface.Create(const aCreateInfo:TVkAndroidSurfaceCreateInfoKHR;
                                  const aInstanceFunctions:TVkInstanceFunctions;
                                  const aAllocCallbacks:PVkAllocationCallbacks=nil);
begin
  inherited Create;
  fInstanceFunctions:=aInstanceFunctions;
  SetAllocCallbacks(aAllocCallbacks);
  CreateHandle(@aCreateInfo);
end;

{$ENDIF}

procedure TvkObj_Surface.CreateHandle(const aCreateInfo:Pointer);
var err:TVkResult;
begin
{$IF DEFINED(VK_USE_PLATFORM_WIN32_KHR)}
  if not Assigned(fInstanceFunctions.vkCreateWin32SurfaceKHR) then
    raise TvkObj_InvalidFuncPtrException.Create('vkCreateWin32SurfaceKHR', 'VK_KHR_win32_surface');
  err:=fInstanceFunctions.vkCreateWin32SurfaceKHR(fInstanceFunctions.aaInstance, aCreateInfo, AllocCallbacks, @fHandle);
{$ENDIF}
  if (err <> VK_SUCCESS) then
    raise TvkObj_ErrorException.Create('unable to create surface', err);
end;


class function TvkObj_Surface.GetPlatformSurfaceExtensionName:String;
begin
{$IF DEFINED(VK_USE_PLATFORM_XLIB_KHR)}
  result:=VK_KHR_XLIB_SURFACE_EXTENSION_NAME;
{$ELSEIF DEFINED(VK_USE_PLATFORM_XCB_KHR)}
  result:=VK_KHR_XCB_SURFACE_EXTENSION_NAME;
{$ELSEIF DEFINED(VK_USE_PLATFORM_WAYLAND_KHR)}
  result:=VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME;
{$ELSEIF DEFINED(VK_USE_PLATFORM_MIR_KHR)}
  result:=VK_KHR_MIR_SURFACE_EXTENSION_NAME;
{$ELSEIF DEFINED(VK_USE_PLATFORM_ANDROID_KHR)}
  result:=VK_KHR_ANDROID_SURFACE_EXTENSION_NAME;
{$ELSEIF DEFINED(VK_USE_PLATFORM_WIN32_KHR)}
  result:=VK_KHR_WIN32_SURFACE_EXTENSION_NAME;
{$ELSE}
  {$ERROR 'unknown or unsupportet platform'}
{$ENDIF}
end;

//=============== TvkObj_SurfaceFactory =========================
procedure TvkObj_SurfaceFactory.SetWinControl(aValue:TWinControl);
begin
  if fWinControl=aValue then exit;

  fWinControl:=aValue;

  {$IF DEFINED(VK_USE_PLATFORM_WIN32_KHR)}
    fHwnd:=TVkHWND(fWinControl.Handle);
  {$ELSEIF DEFINED(VK_USE_PLATFORM_XLIB_KHR)}
    fWindow:=TVkXLIBWindow(fWinControl.Handle);
  {$ELSEIF DEFINED(VK_USE_PLATFORM_XCB_KHR)}
    fWindow:=TVkXCBWindow(fWinControl.Handle);
  {$ELSEIF DEFINED(VK_USE_PLATFORM_WAYLAND_KHR)}
    fSurface:=PVkWaylandSurface(fWinControl.Handle);
  {$ELSEIF DEFINED(VK_USE_PLATFORM_MIR_KHR)}
    fMirSurface:=PVkMirSurface(fWinControl.Handle);
  {$ELSEIF DEFINED(VK_USE_PLATFORM_ANDROID_KHR)}
    fWindow:=PVkAndroidANativeWindow(fWinControl.Handle);
  {$ELSE}
    {$ERROR 'TvkObj_SurfaceFactory.SetWinControl unknown or unsupported system'}
  {$ENDIF}
end;

function TvkObj_SurfaceFactory.CreateSurface:TvkObj_Surface;
begin
  result:=TvkObj_Surface.Create(GetStructure, InstanceFunctions, AllocCallbacks);
end;

procedure TvkObj_SurfaceFactory.AfterConstruction;
begin
  inherited AfterConstruction;
  {$IF DEFINED(VK_USE_PLATFORM_WIN32_KHR)}
    fHInstance:=TVkHINSTANCE(GetModuleHandle(nil));
  {$ELSEIF DEFINED(VK_USE_PLATFORM_XLIB_KHR)}
    //fdpy:=PVkXLIBDisplay(GetModuleHandle(nil));
  {$ENDIF}
  fFlags:=0;
end;

{$IF DEFINED(VK_USE_PLATFORM_WIN32_KHR)}
procedure TvkObj_SurfaceFactory.SetHwnd(const aValue:TVkHWND);
begin
  SetWinControl(nil);
  fHwnd:=aValue;
end;

function TvkObj_SurfaceFactory.GetStructure:TVkWin32SurfaceCreateInfoKHR;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.hinstance_:=fHInstance;
  result.hwnd_:=fHwnd;
end;

procedure TvkObj_SurfaceFactory.SetStructure(const aData:TVkWin32SurfaceCreateInfoKHR);
begin
  fFlags:=aData.flags;
  fHInstance:=aData.hinstance_;
  fHwnd:=aData.hwnd_;
end;
{$ELSEIF DEFINED(VK_USE_PLATFORM_XLIB_KHR)}
procedure TvkObj_SurfaceFactory.SetWindow(const aValue:TVkXLIBWindow);
begin
  SetWinControl(nil);
  fWindow:=aValue;
end;

function TvkObj_SurfaceFactory.GetStructure:TVkXlibSurfaceCreateInfoKHR;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.dpy:=fdpy;
  result.window:=fWindow;
end;

procedure TvkObj_SurfaceFactory.SetStructure(const aData:TVkXlibSurfaceCreateInfoKHR);
begin
  fFlags:=aData.flags;
  fdpy:=aData.dpy;
  fWindow:=aData.window;
end;
{$ELSEIF DEFINED(VK_USE_PLATFORM_XCB_KHR)}
procedure TvkObj_SurfaceFactory.SetWindow(const aValue:TVkXCBWindow);
begin
  SetWinControl(nil);
  fWindow:=aValue;
end;

function TvkObj_SurfaceFactory.GetStructure:TVkXcbSurfaceCreateInfoKHR;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.connection:= fConnection;
  result.window:=fWindow;
end;

procedure TvkObj_SurfaceFactory.SetStructure(const aData:TVkXcbSurfaceCreateInfoKHR);
begin
  fFlags:=aData.flags;
  fConnection:= aData.connection;
  fWindow:=aData.window;
end;
{$ELSEIF DEFINED(VK_USE_PLATFORM_WAYLAND_KHR)}
procedure TvkObj_SurfaceFactory.SetWindow(const aValue:PVkWaylandSurface);
begin
  SetWinControl(nil);
  fSurface:=aValue;
end;

function TvkObj_SurfaceFactory.GetStructure:TVkWaylandSurfaceCreateInfoKHR;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.display:=fDisplay;
  result.surface:=fSurface;
end;

procedure TvkObj_SurfaceFactory.SetStructure(const aData:TVkWaylandSurfaceCreateInfoKHR);
begin
  fFlags:=aData.flags;
  fdisplay:=aData.display;
  fSurface:=aData.Surface;
end;
{$ELSEIF DEFINED(VK_USE_PLATFORM_MIR_KHR)}
procedure TvkObj_SurfaceFactory.SetWindow(const aValue:PVkMirSurface);
begin
  SetWinControl(nil);
  fMirSurface:=aValue;
end;

function TvkObj_SurfaceFactory.GetStructure:TVkMirSurfaceCreateInfoKHR;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.connection:= fConnection;
  result.mirSurface:= fMirSurface;
end;

procedure TvkObj_SurfaceFactory.SetStructure(const aData:TVkMirSurfaceCreateInfoKHR);
begin
  fFlags:=aData.flags;
  fConnection:=aData.connection;
  fMirSurface:=aData.mirSurface;
end;
{$ELSEIF DEFINED(VK_USE_PLATFORM_ANDROID_KHR)}
procedure TvkObj_SurfaceFactory.SetWindow(const aValue:PVkAndroidANativeWindow);
begin
  SetWinControl(nil);
  fWindow:=aValue;
end;

function TvkObj_SurfaceFactory.GetStructure:TVkAndroidSurfaceCreateInfoKHR;
begin
  FillByte(result, SizeOf(result), 0);
  result.sType:=VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR;
  result.pNext:=nil;
  result.flags:=fFlags;
  result.window:=fWindow;
end;

procedure TvkObj_SurfaceFactory.SetStructure(const aData:TVkAndroidSurfaceCreateInfoKHR);
begin
  fFlags:=aData.flags;
  fWindow:=aData.Window;
end;
{$ENDIF}

end.


