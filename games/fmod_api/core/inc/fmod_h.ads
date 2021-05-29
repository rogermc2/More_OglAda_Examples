pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with fmod_common_h;
with Interfaces.C.Strings;
limited with fmod_codec_h;
limited with fmod_dsp_h;
limited with fmod_output_h;
with fmod_dsp_effects_h;
with Interfaces.C.Extensions;

package fmod_h is

  -- ========================================================================================  
  -- FMOD Core API - C header file.                                                            
  -- Copyright (c), Firelight Technologies Pty, Ltd. 2004-2021.                                
  --                                                                                           
  -- Use this header in conjunction with fmod_common.h (which contains all the constants /     
  -- callbacks) to develop using the C interface                                               
  --                                                                                           
  -- For more detail visit:                                                                    
  -- https://fmod.com/resources/documentation-api?version=2.0&page=core-api.html               
  -- ========================================================================================  
  --    FMOD global system functions (optional).
  -- 

   function FMOD_Memory_Initialize
     (poolmem : System.Address;
      poollen : int;
      useralloc : fmod_common_h.FMOD_MEMORY_ALLOC_CALLBACK;
      userrealloc : fmod_common_h.FMOD_MEMORY_REALLOC_CALLBACK;
      userfree : fmod_common_h.FMOD_MEMORY_FREE_CALLBACK;
      memtypeflags : fmod_common_h.FMOD_MEMORY_TYPE) return fmod_common_h.FMOD_RESULT  -- fmod.h:25
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Memory_Initialize";

   function FMOD_Memory_GetStats
     (currentalloced : access int;
      maxalloced : access int;
      blocking : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:26
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Memory_GetStats";

   function FMOD_Debug_Initialize
     (flags : fmod_common_h.FMOD_DEBUG_FLAGS;
      mode : fmod_common_h.FMOD_DEBUG_MODE;
      callback : fmod_common_h.FMOD_DEBUG_CALLBACK;
      filename : Interfaces.C.Strings.chars_ptr) return fmod_common_h.FMOD_RESULT  -- fmod.h:27
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Debug_Initialize";

   function FMOD_File_SetDiskBusy (busy : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:28
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_File_SetDiskBusy";

   function FMOD_File_GetDiskBusy (busy : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:29
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_File_GetDiskBusy";

   function FMOD_Thread_SetAttributes
     (c_type : fmod_common_h.FMOD_THREAD_TYPE;
      affinity : fmod_common_h.FMOD_THREAD_AFFINITY;
      priority : fmod_common_h.FMOD_THREAD_PRIORITY;
      stacksize : fmod_common_h.FMOD_THREAD_STACK_SIZE) return fmod_common_h.FMOD_RESULT  -- fmod.h:30
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Thread_SetAttributes";

  --    FMOD System factory functions.  Use this to create an FMOD System Instance.  below you will see FMOD_System_Init/Close to get started.
  -- 

   function FMOD_System_Create (c_system : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:35
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_Create";

   function FMOD_System_Release (c_system : access fmod_common_h.FMOD_SYSTEM) return fmod_common_h.FMOD_RESULT  -- fmod.h:36
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_Release";

  --    'System' API
  -- 

  -- Setup functions.  
   function FMOD_System_SetOutput (c_system : access fmod_common_h.FMOD_SYSTEM; output : fmod_common_h.FMOD_OUTPUTTYPE) return fmod_common_h.FMOD_RESULT  -- fmod.h:43
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_SetOutput";

   function FMOD_System_GetOutput (c_system : access fmod_common_h.FMOD_SYSTEM; output : access fmod_common_h.FMOD_OUTPUTTYPE) return fmod_common_h.FMOD_RESULT  -- fmod.h:44
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetOutput";

   function FMOD_System_GetNumDrivers (c_system : access fmod_common_h.FMOD_SYSTEM; numdrivers : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:45
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetNumDrivers";

   function FMOD_System_GetDriverInfo
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      id : int;
      name : Interfaces.C.Strings.chars_ptr;
      namelen : int;
      guid : access fmod_common_h.FMOD_GUID;
      systemrate : access int;
      speakermode : access fmod_common_h.FMOD_SPEAKERMODE;
      speakermodechannels : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:46
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetDriverInfo";

   function FMOD_System_SetDriver (c_system : access fmod_common_h.FMOD_SYSTEM; driver : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:47
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_SetDriver";

   function FMOD_System_GetDriver (c_system : access fmod_common_h.FMOD_SYSTEM; driver : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:48
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetDriver";

   function FMOD_System_SetSoftwareChannels (c_system : access fmod_common_h.FMOD_SYSTEM; numsoftwarechannels : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:49
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_SetSoftwareChannels";

   function FMOD_System_GetSoftwareChannels (c_system : access fmod_common_h.FMOD_SYSTEM; numsoftwarechannels : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:50
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetSoftwareChannels";

   function FMOD_System_SetSoftwareFormat
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      samplerate : int;
      speakermode : fmod_common_h.FMOD_SPEAKERMODE;
      numrawspeakers : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:51
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_SetSoftwareFormat";

   function FMOD_System_GetSoftwareFormat
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      samplerate : access int;
      speakermode : access fmod_common_h.FMOD_SPEAKERMODE;
      numrawspeakers : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:52
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetSoftwareFormat";

   function FMOD_System_SetDSPBufferSize
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      bufferlength : unsigned;
      numbuffers : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:53
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_SetDSPBufferSize";

   function FMOD_System_GetDSPBufferSize
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      bufferlength : access unsigned;
      numbuffers : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:54
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetDSPBufferSize";

   function FMOD_System_SetFileSystem
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      useropen : fmod_common_h.FMOD_FILE_OPEN_CALLBACK;
      userclose : fmod_common_h.FMOD_FILE_CLOSE_CALLBACK;
      userread : fmod_common_h.FMOD_FILE_READ_CALLBACK;
      userseek : fmod_common_h.FMOD_FILE_SEEK_CALLBACK;
      userasyncread : fmod_common_h.FMOD_FILE_ASYNCREAD_CALLBACK;
      userasynccancel : fmod_common_h.FMOD_FILE_ASYNCCANCEL_CALLBACK;
      blockalign : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:55
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_SetFileSystem";

   function FMOD_System_AttachFileSystem
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      useropen : fmod_common_h.FMOD_FILE_OPEN_CALLBACK;
      userclose : fmod_common_h.FMOD_FILE_CLOSE_CALLBACK;
      userread : fmod_common_h.FMOD_FILE_READ_CALLBACK;
      userseek : fmod_common_h.FMOD_FILE_SEEK_CALLBACK) return fmod_common_h.FMOD_RESULT  -- fmod.h:56
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_AttachFileSystem";

   function FMOD_System_SetAdvancedSettings (c_system : access fmod_common_h.FMOD_SYSTEM; settings : access fmod_common_h.FMOD_ADVANCEDSETTINGS) return fmod_common_h.FMOD_RESULT  -- fmod.h:57
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_SetAdvancedSettings";

   function FMOD_System_GetAdvancedSettings (c_system : access fmod_common_h.FMOD_SYSTEM; settings : access fmod_common_h.FMOD_ADVANCEDSETTINGS) return fmod_common_h.FMOD_RESULT  -- fmod.h:58
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetAdvancedSettings";

   function FMOD_System_SetCallback
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      callback : fmod_common_h.FMOD_SYSTEM_CALLBACK;
      callbackmask : fmod_common_h.FMOD_SYSTEM_CALLBACK_TYPE) return fmod_common_h.FMOD_RESULT  -- fmod.h:59
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_SetCallback";

  -- Plug-in support.  
   function FMOD_System_SetPluginPath (c_system : access fmod_common_h.FMOD_SYSTEM; path : Interfaces.C.Strings.chars_ptr) return fmod_common_h.FMOD_RESULT  -- fmod.h:62
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_SetPluginPath";

   function FMOD_System_LoadPlugin
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      filename : Interfaces.C.Strings.chars_ptr;
      handle : access unsigned;
      priority : unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.h:63
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_LoadPlugin";

   function FMOD_System_UnloadPlugin (c_system : access fmod_common_h.FMOD_SYSTEM; handle : unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.h:64
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_UnloadPlugin";

   function FMOD_System_GetNumNestedPlugins
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      handle : unsigned;
      count : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:65
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetNumNestedPlugins";

   function FMOD_System_GetNestedPlugin
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      handle : unsigned;
      index : int;
      nestedhandle : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.h:66
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetNestedPlugin";

   function FMOD_System_GetNumPlugins
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      plugintype : fmod_common_h.FMOD_PLUGINTYPE;
      numplugins : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:67
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetNumPlugins";

   function FMOD_System_GetPluginHandle
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      plugintype : fmod_common_h.FMOD_PLUGINTYPE;
      index : int;
      handle : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.h:68
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetPluginHandle";

   function FMOD_System_GetPluginInfo
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      handle : unsigned;
      plugintype : access fmod_common_h.FMOD_PLUGINTYPE;
      name : Interfaces.C.Strings.chars_ptr;
      namelen : int;
      version : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.h:69
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetPluginInfo";

   function FMOD_System_SetOutputByPlugin (c_system : access fmod_common_h.FMOD_SYSTEM; handle : unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.h:70
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_SetOutputByPlugin";

   function FMOD_System_GetOutputByPlugin (c_system : access fmod_common_h.FMOD_SYSTEM; handle : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.h:71
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetOutputByPlugin";

   function FMOD_System_CreateDSPByPlugin
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      handle : unsigned;
      dsp : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:72
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_CreateDSPByPlugin";

   function FMOD_System_GetDSPInfoByPlugin
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      handle : unsigned;
      description : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:73
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetDSPInfoByPlugin";

   function FMOD_System_RegisterCodec
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      description : access fmod_codec_h.FMOD_CODEC_DESCRIPTION;
      handle : access unsigned;
      priority : unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.h:74
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_RegisterCodec";

   function FMOD_System_RegisterDSP
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      description : access constant fmod_dsp_h.FMOD_DSP_DESCRIPTION;
      handle : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.h:75
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_RegisterDSP";

   function FMOD_System_RegisterOutput
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      description : access constant fmod_output_h.FMOD_OUTPUT_DESCRIPTION;
      handle : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.h:76
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_RegisterOutput";

  -- Init/Close.  
   function FMOD_System_Init
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      maxchannels : int;
      flags : fmod_common_h.FMOD_INITFLAGS;
      extradriverdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:79
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_Init";

   function FMOD_System_Close (c_system : access fmod_common_h.FMOD_SYSTEM) return fmod_common_h.FMOD_RESULT  -- fmod.h:80
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_Close";

  -- General post-init system functions.  
   function FMOD_System_Update (c_system : access fmod_common_h.FMOD_SYSTEM) return fmod_common_h.FMOD_RESULT  -- fmod.h:83
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_Update";

   function FMOD_System_SetSpeakerPosition
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      speaker : fmod_common_h.FMOD_SPEAKER;
      x : float;
      y : float;
      active : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:84
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_SetSpeakerPosition";

   function FMOD_System_GetSpeakerPosition
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      speaker : fmod_common_h.FMOD_SPEAKER;
      x : access float;
      y : access float;
      active : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:85
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetSpeakerPosition";

   function FMOD_System_SetStreamBufferSize
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      filebuffersize : unsigned;
      filebuffersizetype : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT  -- fmod.h:86
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_SetStreamBufferSize";

   function FMOD_System_GetStreamBufferSize
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      filebuffersize : access unsigned;
      filebuffersizetype : access fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT  -- fmod.h:87
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetStreamBufferSize";

   function FMOD_System_Set3DSettings
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      dopplerscale : float;
      distancefactor : float;
      rolloffscale : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:88
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_Set3DSettings";

   function FMOD_System_Get3DSettings
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      dopplerscale : access float;
      distancefactor : access float;
      rolloffscale : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:89
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_Get3DSettings";

   function FMOD_System_Set3DNumListeners (c_system : access fmod_common_h.FMOD_SYSTEM; numlisteners : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:90
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_Set3DNumListeners";

   function FMOD_System_Get3DNumListeners (c_system : access fmod_common_h.FMOD_SYSTEM; numlisteners : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:91
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_Get3DNumListeners";

   function FMOD_System_Set3DListenerAttributes
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      listener : int;
      pos : access constant fmod_common_h.FMOD_VECTOR;
      vel : access constant fmod_common_h.FMOD_VECTOR;
      forward : access constant fmod_common_h.FMOD_VECTOR;
      up : access constant fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.h:92
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_Set3DListenerAttributes";

   function FMOD_System_Get3DListenerAttributes
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      listener : int;
      pos : access fmod_common_h.FMOD_VECTOR;
      vel : access fmod_common_h.FMOD_VECTOR;
      forward : access fmod_common_h.FMOD_VECTOR;
      up : access fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.h:93
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_Get3DListenerAttributes";

   function FMOD_System_Set3DRolloffCallback (c_system : access fmod_common_h.FMOD_SYSTEM; callback : fmod_common_h.FMOD_3D_ROLLOFF_CALLBACK) return fmod_common_h.FMOD_RESULT  -- fmod.h:94
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_Set3DRolloffCallback";

   function FMOD_System_MixerSuspend (c_system : access fmod_common_h.FMOD_SYSTEM) return fmod_common_h.FMOD_RESULT  -- fmod.h:95
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_MixerSuspend";

   function FMOD_System_MixerResume (c_system : access fmod_common_h.FMOD_SYSTEM) return fmod_common_h.FMOD_RESULT  -- fmod.h:96
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_MixerResume";

   function FMOD_System_GetDefaultMixMatrix
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      sourcespeakermode : fmod_common_h.FMOD_SPEAKERMODE;
      targetspeakermode : fmod_common_h.FMOD_SPEAKERMODE;
      matrix : access float;
      matrixhop : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:97
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetDefaultMixMatrix";

   function FMOD_System_GetSpeakerModeChannels
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      mode : fmod_common_h.FMOD_SPEAKERMODE;
      channels : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:98
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetSpeakerModeChannels";

  -- System information functions.  
   function FMOD_System_GetVersion (c_system : access fmod_common_h.FMOD_SYSTEM; version : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.h:101
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetVersion";

   function FMOD_System_GetOutputHandle (c_system : access fmod_common_h.FMOD_SYSTEM; handle : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:102
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetOutputHandle";

   function FMOD_System_GetChannelsPlaying
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      channels : access int;
      realchannels : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:103
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetChannelsPlaying";

   function FMOD_System_GetCPUUsage
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      dsp : access float;
      stream : access float;
      geometry : access float;
      update : access float;
      total : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:104
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetCPUUsage";

   function FMOD_System_GetCPUUsageEx
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      convolutionThread1 : access float;
      convolutionThread2 : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:105
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetCPUUsageEx";

   function FMOD_System_GetFileUsage
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      sampleBytesRead : access Long_Long_Integer;
      streamBytesRead : access Long_Long_Integer;
      otherBytesRead : access Long_Long_Integer) return fmod_common_h.FMOD_RESULT  -- fmod.h:106
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetFileUsage";

  -- Sound/DSP/Channel/FX creation and retrieval.  
   function FMOD_System_CreateSound
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      name_or_data : Interfaces.C.Strings.chars_ptr;
      mode : fmod_common_h.FMOD_MODE;
      exinfo : access fmod_common_h.FMOD_CREATESOUNDEXINFO;
      sound : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:109
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_CreateSound";

   function FMOD_System_CreateStream
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      name_or_data : Interfaces.C.Strings.chars_ptr;
      mode : fmod_common_h.FMOD_MODE;
      exinfo : access fmod_common_h.FMOD_CREATESOUNDEXINFO;
      sound : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:110
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_CreateStream";

   function FMOD_System_CreateDSP
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      description : access constant fmod_dsp_h.FMOD_DSP_DESCRIPTION;
      dsp : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:111
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_CreateDSP";

   function FMOD_System_CreateDSPByType
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      c_type : fmod_dsp_effects_h.FMOD_DSP_TYPE;
      dsp : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:112
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_CreateDSPByType";

   function FMOD_System_CreateChannelGroup
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      name : Interfaces.C.Strings.chars_ptr;
      channelgroup : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:113
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_CreateChannelGroup";

   function FMOD_System_CreateSoundGroup
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      name : Interfaces.C.Strings.chars_ptr;
      soundgroup : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:114
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_CreateSoundGroup";

   function FMOD_System_CreateReverb3D (c_system : access fmod_common_h.FMOD_SYSTEM; reverb : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:115
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_CreateReverb3D";

   function FMOD_System_PlaySound
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      sound : access fmod_common_h.FMOD_SOUND;
      channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      paused : fmod_common_h.FMOD_BOOL;
      channel : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:116
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_PlaySound";

   function FMOD_System_PlayDSP
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      dsp : access fmod_common_h.FMOD_DSP;
      channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      paused : fmod_common_h.FMOD_BOOL;
      channel : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:117
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_PlayDSP";

   function FMOD_System_GetChannel
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      channelid : int;
      channel : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:118
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetChannel";

   function FMOD_System_GetDSPInfoByType
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      c_type : fmod_dsp_effects_h.FMOD_DSP_TYPE;
      description : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:119
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetDSPInfoByType";

   function FMOD_System_GetMasterChannelGroup (c_system : access fmod_common_h.FMOD_SYSTEM; channelgroup : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:120
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetMasterChannelGroup";

   function FMOD_System_GetMasterSoundGroup (c_system : access fmod_common_h.FMOD_SYSTEM; soundgroup : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:121
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetMasterSoundGroup";

  -- Routing to ports.  
   function FMOD_System_AttachChannelGroupToPort
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      portType : fmod_common_h.FMOD_PORT_TYPE;
      portIndex : fmod_common_h.FMOD_PORT_INDEX;
      channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      passThru : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:124
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_AttachChannelGroupToPort";

   function FMOD_System_DetachChannelGroupFromPort (c_system : access fmod_common_h.FMOD_SYSTEM; channelgroup : access fmod_common_h.FMOD_CHANNELGROUP) return fmod_common_h.FMOD_RESULT  -- fmod.h:125
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_DetachChannelGroupFromPort";

  -- Reverb API.  
   function FMOD_System_SetReverbProperties
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      instance : int;
      prop : access constant fmod_common_h.FMOD_REVERB_PROPERTIES) return fmod_common_h.FMOD_RESULT  -- fmod.h:128
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_SetReverbProperties";

   function FMOD_System_GetReverbProperties
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      instance : int;
      prop : access fmod_common_h.FMOD_REVERB_PROPERTIES) return fmod_common_h.FMOD_RESULT  -- fmod.h:129
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetReverbProperties";

  -- System level DSP functionality.  
   function FMOD_System_LockDSP (c_system : access fmod_common_h.FMOD_SYSTEM) return fmod_common_h.FMOD_RESULT  -- fmod.h:132
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_LockDSP";

   function FMOD_System_UnlockDSP (c_system : access fmod_common_h.FMOD_SYSTEM) return fmod_common_h.FMOD_RESULT  -- fmod.h:133
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_UnlockDSP";

  -- Recording API.  
   function FMOD_System_GetRecordNumDrivers
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      numdrivers : access int;
      numconnected : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:136
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetRecordNumDrivers";

   function FMOD_System_GetRecordDriverInfo
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      id : int;
      name : Interfaces.C.Strings.chars_ptr;
      namelen : int;
      guid : access fmod_common_h.FMOD_GUID;
      systemrate : access int;
      speakermode : access fmod_common_h.FMOD_SPEAKERMODE;
      speakermodechannels : access int;
      state : access fmod_common_h.FMOD_DRIVER_STATE) return fmod_common_h.FMOD_RESULT  -- fmod.h:137
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetRecordDriverInfo";

   function FMOD_System_GetRecordPosition
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      id : int;
      position : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.h:138
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetRecordPosition";

   function FMOD_System_RecordStart
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      id : int;
      sound : access fmod_common_h.FMOD_SOUND;
      c_loop : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:139
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_RecordStart";

   function FMOD_System_RecordStop (c_system : access fmod_common_h.FMOD_SYSTEM; id : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:140
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_RecordStop";

   function FMOD_System_IsRecording
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      id : int;
      recording : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:141
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_IsRecording";

  -- Geometry API.  
   function FMOD_System_CreateGeometry
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      maxpolygons : int;
      maxvertices : int;
      geometry : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:144
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_CreateGeometry";

   function FMOD_System_SetGeometrySettings (c_system : access fmod_common_h.FMOD_SYSTEM; maxworldsize : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:145
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_SetGeometrySettings";

   function FMOD_System_GetGeometrySettings (c_system : access fmod_common_h.FMOD_SYSTEM; maxworldsize : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:146
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetGeometrySettings";

   function FMOD_System_LoadGeometry
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      data : System.Address;
      datasize : int;
      geometry : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:147
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_LoadGeometry";

   function FMOD_System_GetGeometryOcclusion
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      listener : access constant fmod_common_h.FMOD_VECTOR;
      source : access constant fmod_common_h.FMOD_VECTOR;
      direct : access float;
      reverb : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:148
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetGeometryOcclusion";

  -- Network functions.  
   function FMOD_System_SetNetworkProxy (c_system : access fmod_common_h.FMOD_SYSTEM; proxy : Interfaces.C.Strings.chars_ptr) return fmod_common_h.FMOD_RESULT  -- fmod.h:151
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_SetNetworkProxy";

   function FMOD_System_GetNetworkProxy
     (c_system : access fmod_common_h.FMOD_SYSTEM;
      proxy : Interfaces.C.Strings.chars_ptr;
      proxylen : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:152
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetNetworkProxy";

   function FMOD_System_SetNetworkTimeout (c_system : access fmod_common_h.FMOD_SYSTEM; timeout : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:153
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_SetNetworkTimeout";

   function FMOD_System_GetNetworkTimeout (c_system : access fmod_common_h.FMOD_SYSTEM; timeout : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:154
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetNetworkTimeout";

  -- Userdata set/get.  
   function FMOD_System_SetUserData (c_system : access fmod_common_h.FMOD_SYSTEM; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:157
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_SetUserData";

   function FMOD_System_GetUserData (c_system : access fmod_common_h.FMOD_SYSTEM; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:158
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_System_GetUserData";

  -- Sound API
  -- 

   function FMOD_Sound_Release (sound : access fmod_common_h.FMOD_SOUND) return fmod_common_h.FMOD_RESULT  -- fmod.h:163
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_Release";

   function FMOD_Sound_GetSystemObject (sound : access fmod_common_h.FMOD_SOUND; c_system : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:164
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetSystemObject";

  --     Standard sound manipulation functions.
  -- 

   function FMOD_Sound_Lock
     (sound : access fmod_common_h.FMOD_SOUND;
      offset : unsigned;
      length : unsigned;
      ptr1 : System.Address;
      ptr2 : System.Address;
      len1 : access unsigned;
      len2 : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.h:170
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_Lock";

   function FMOD_Sound_Unlock
     (sound : access fmod_common_h.FMOD_SOUND;
      ptr1 : System.Address;
      ptr2 : System.Address;
      len1 : unsigned;
      len2 : unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.h:171
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_Unlock";

   function FMOD_Sound_SetDefaults
     (sound : access fmod_common_h.FMOD_SOUND;
      frequency : float;
      priority : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:172
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_SetDefaults";

   function FMOD_Sound_GetDefaults
     (sound : access fmod_common_h.FMOD_SOUND;
      frequency : access float;
      priority : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:173
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetDefaults";

   function FMOD_Sound_Set3DMinMaxDistance
     (sound : access fmod_common_h.FMOD_SOUND;
      min : float;
      max : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:174
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_Set3DMinMaxDistance";

   function FMOD_Sound_Get3DMinMaxDistance
     (sound : access fmod_common_h.FMOD_SOUND;
      min : access float;
      max : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:175
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_Get3DMinMaxDistance";

   function FMOD_Sound_Set3DConeSettings
     (sound : access fmod_common_h.FMOD_SOUND;
      insideconeangle : float;
      outsideconeangle : float;
      outsidevolume : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:176
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_Set3DConeSettings";

   function FMOD_Sound_Get3DConeSettings
     (sound : access fmod_common_h.FMOD_SOUND;
      insideconeangle : access float;
      outsideconeangle : access float;
      outsidevolume : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:177
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_Get3DConeSettings";

   function FMOD_Sound_Set3DCustomRolloff
     (sound : access fmod_common_h.FMOD_SOUND;
      points : access fmod_common_h.FMOD_VECTOR;
      numpoints : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:178
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_Set3DCustomRolloff";

   function FMOD_Sound_Get3DCustomRolloff
     (sound : access fmod_common_h.FMOD_SOUND;
      points : System.Address;
      numpoints : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:179
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_Get3DCustomRolloff";

   function FMOD_Sound_GetSubSound
     (sound : access fmod_common_h.FMOD_SOUND;
      index : int;
      subsound : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:180
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetSubSound";

   function FMOD_Sound_GetSubSoundParent (sound : access fmod_common_h.FMOD_SOUND; parentsound : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:181
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetSubSoundParent";

   function FMOD_Sound_GetName
     (sound : access fmod_common_h.FMOD_SOUND;
      name : Interfaces.C.Strings.chars_ptr;
      namelen : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:182
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetName";

   function FMOD_Sound_GetLength
     (sound : access fmod_common_h.FMOD_SOUND;
      length : access unsigned;
      lengthtype : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT  -- fmod.h:183
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetLength";

   function FMOD_Sound_GetFormat
     (sound : access fmod_common_h.FMOD_SOUND;
      c_type : access fmod_common_h.FMOD_SOUND_TYPE;
      format : access fmod_common_h.FMOD_SOUND_FORMAT;
      channels : access int;
      bits : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:184
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetFormat";

   function FMOD_Sound_GetNumSubSounds (sound : access fmod_common_h.FMOD_SOUND; numsubsounds : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:185
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetNumSubSounds";

   function FMOD_Sound_GetNumTags
     (sound : access fmod_common_h.FMOD_SOUND;
      numtags : access int;
      numtagsupdated : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:186
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetNumTags";

   function FMOD_Sound_GetTag
     (sound : access fmod_common_h.FMOD_SOUND;
      name : Interfaces.C.Strings.chars_ptr;
      index : int;
      tag : access fmod_common_h.FMOD_TAG) return fmod_common_h.FMOD_RESULT  -- fmod.h:187
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetTag";

   function FMOD_Sound_GetOpenState
     (sound : access fmod_common_h.FMOD_SOUND;
      openstate : access fmod_common_h.FMOD_OPENSTATE;
      percentbuffered : access unsigned;
      starving : access fmod_common_h.FMOD_BOOL;
      diskbusy : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:188
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetOpenState";

   function FMOD_Sound_ReadData
     (sound : access fmod_common_h.FMOD_SOUND;
      buffer : System.Address;
      length : unsigned;
      read : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.h:189
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_ReadData";

   function FMOD_Sound_SeekData (sound : access fmod_common_h.FMOD_SOUND; pcm : unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.h:190
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_SeekData";

   function FMOD_Sound_SetSoundGroup (sound : access fmod_common_h.FMOD_SOUND; soundgroup : access fmod_common_h.FMOD_SOUNDGROUP) return fmod_common_h.FMOD_RESULT  -- fmod.h:192
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_SetSoundGroup";

   function FMOD_Sound_GetSoundGroup (sound : access fmod_common_h.FMOD_SOUND; soundgroup : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:193
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetSoundGroup";

  --     Synchronization point API.  These points can come from markers embedded in wav files, and can also generate channel callbacks.
  -- 

   function FMOD_Sound_GetNumSyncPoints (sound : access fmod_common_h.FMOD_SOUND; numsyncpoints : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:199
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetNumSyncPoints";

   function FMOD_Sound_GetSyncPoint
     (sound : access fmod_common_h.FMOD_SOUND;
      index : int;
      point : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:200
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetSyncPoint";

   function FMOD_Sound_GetSyncPointInfo
     (sound : access fmod_common_h.FMOD_SOUND;
      point : access fmod_common_h.FMOD_SYNCPOINT;
      name : Interfaces.C.Strings.chars_ptr;
      namelen : int;
      offset : access unsigned;
      offsettype : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT  -- fmod.h:201
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetSyncPointInfo";

   function FMOD_Sound_AddSyncPoint
     (sound : access fmod_common_h.FMOD_SOUND;
      offset : unsigned;
      offsettype : fmod_common_h.FMOD_TIMEUNIT;
      name : Interfaces.C.Strings.chars_ptr;
      point : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:202
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_AddSyncPoint";

   function FMOD_Sound_DeleteSyncPoint (sound : access fmod_common_h.FMOD_SOUND; point : access fmod_common_h.FMOD_SYNCPOINT) return fmod_common_h.FMOD_RESULT  -- fmod.h:203
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_DeleteSyncPoint";

  --     Functions also in Channel class but here they are the 'default' to save having to change it in Channel all the time.
  -- 

   function FMOD_Sound_SetMode (sound : access fmod_common_h.FMOD_SOUND; mode : fmod_common_h.FMOD_MODE) return fmod_common_h.FMOD_RESULT  -- fmod.h:209
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_SetMode";

   function FMOD_Sound_GetMode (sound : access fmod_common_h.FMOD_SOUND; mode : access fmod_common_h.FMOD_MODE) return fmod_common_h.FMOD_RESULT  -- fmod.h:210
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetMode";

   function FMOD_Sound_SetLoopCount (sound : access fmod_common_h.FMOD_SOUND; loopcount : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:211
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_SetLoopCount";

   function FMOD_Sound_GetLoopCount (sound : access fmod_common_h.FMOD_SOUND; loopcount : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:212
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetLoopCount";

   function FMOD_Sound_SetLoopPoints
     (sound : access fmod_common_h.FMOD_SOUND;
      loopstart : unsigned;
      loopstarttype : fmod_common_h.FMOD_TIMEUNIT;
      loopend : unsigned;
      loopendtype : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT  -- fmod.h:213
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_SetLoopPoints";

   function FMOD_Sound_GetLoopPoints
     (sound : access fmod_common_h.FMOD_SOUND;
      loopstart : access unsigned;
      loopstarttype : fmod_common_h.FMOD_TIMEUNIT;
      loopend : access unsigned;
      loopendtype : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT  -- fmod.h:214
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetLoopPoints";

  --     For MOD/S3M/XM/IT/MID sequenced formats only.
  -- 

   function FMOD_Sound_GetMusicNumChannels (sound : access fmod_common_h.FMOD_SOUND; numchannels : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:220
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetMusicNumChannels";

   function FMOD_Sound_SetMusicChannelVolume
     (sound : access fmod_common_h.FMOD_SOUND;
      channel : int;
      volume : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:221
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_SetMusicChannelVolume";

   function FMOD_Sound_GetMusicChannelVolume
     (sound : access fmod_common_h.FMOD_SOUND;
      channel : int;
      volume : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:222
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetMusicChannelVolume";

   function FMOD_Sound_SetMusicSpeed (sound : access fmod_common_h.FMOD_SOUND; speed : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:223
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_SetMusicSpeed";

   function FMOD_Sound_GetMusicSpeed (sound : access fmod_common_h.FMOD_SOUND; speed : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:224
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetMusicSpeed";

  --     Userdata set/get.
  -- 

   function FMOD_Sound_SetUserData (sound : access fmod_common_h.FMOD_SOUND; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:230
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_SetUserData";

   function FMOD_Sound_GetUserData (sound : access fmod_common_h.FMOD_SOUND; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:231
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Sound_GetUserData";

  --    'Channel' API
  -- 

   function FMOD_Channel_GetSystemObject (channel : access fmod_common_h.FMOD_CHANNEL; c_system : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:237
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetSystemObject";

  --     General control functionality for Channels and ChannelGroups.
  -- 

   function FMOD_Channel_Stop (channel : access fmod_common_h.FMOD_CHANNEL) return fmod_common_h.FMOD_RESULT  -- fmod.h:243
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Stop";

   function FMOD_Channel_SetPaused (channel : access fmod_common_h.FMOD_CHANNEL; paused : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:244
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetPaused";

   function FMOD_Channel_GetPaused (channel : access fmod_common_h.FMOD_CHANNEL; paused : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:245
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetPaused";

   function FMOD_Channel_SetVolume (channel : access fmod_common_h.FMOD_CHANNEL; volume : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:246
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetVolume";

   function FMOD_Channel_GetVolume (channel : access fmod_common_h.FMOD_CHANNEL; volume : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:247
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetVolume";

   function FMOD_Channel_SetVolumeRamp (channel : access fmod_common_h.FMOD_CHANNEL; ramp : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:248
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetVolumeRamp";

   function FMOD_Channel_GetVolumeRamp (channel : access fmod_common_h.FMOD_CHANNEL; ramp : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:249
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetVolumeRamp";

   function FMOD_Channel_GetAudibility (channel : access fmod_common_h.FMOD_CHANNEL; audibility : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:250
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetAudibility";

   function FMOD_Channel_SetPitch (channel : access fmod_common_h.FMOD_CHANNEL; pitch : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:251
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetPitch";

   function FMOD_Channel_GetPitch (channel : access fmod_common_h.FMOD_CHANNEL; pitch : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:252
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetPitch";

   function FMOD_Channel_SetMute (channel : access fmod_common_h.FMOD_CHANNEL; mute : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:253
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetMute";

   function FMOD_Channel_GetMute (channel : access fmod_common_h.FMOD_CHANNEL; mute : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:254
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetMute";

   function FMOD_Channel_SetReverbProperties
     (channel : access fmod_common_h.FMOD_CHANNEL;
      instance : int;
      wet : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:255
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetReverbProperties";

   function FMOD_Channel_GetReverbProperties
     (channel : access fmod_common_h.FMOD_CHANNEL;
      instance : int;
      wet : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:256
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetReverbProperties";

   function FMOD_Channel_SetLowPassGain (channel : access fmod_common_h.FMOD_CHANNEL; gain : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:257
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetLowPassGain";

   function FMOD_Channel_GetLowPassGain (channel : access fmod_common_h.FMOD_CHANNEL; gain : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:258
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetLowPassGain";

   function FMOD_Channel_SetMode (channel : access fmod_common_h.FMOD_CHANNEL; mode : fmod_common_h.FMOD_MODE) return fmod_common_h.FMOD_RESULT  -- fmod.h:259
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetMode";

   function FMOD_Channel_GetMode (channel : access fmod_common_h.FMOD_CHANNEL; mode : access fmod_common_h.FMOD_MODE) return fmod_common_h.FMOD_RESULT  -- fmod.h:260
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetMode";

   function FMOD_Channel_SetCallback (channel : access fmod_common_h.FMOD_CHANNEL; callback : fmod_common_h.FMOD_CHANNELCONTROL_CALLBACK) return fmod_common_h.FMOD_RESULT  -- fmod.h:261
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetCallback";

   function FMOD_Channel_IsPlaying (channel : access fmod_common_h.FMOD_CHANNEL; isplaying : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:262
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_IsPlaying";

  --     Note all 'set' functions alter a final matrix, this is why the only get function is getMixMatrix, to avoid other get functions returning incorrect/obsolete values.
  -- 

   function FMOD_Channel_SetPan (channel : access fmod_common_h.FMOD_CHANNEL; pan : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:268
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetPan";

   function FMOD_Channel_SetMixLevelsOutput
     (channel : access fmod_common_h.FMOD_CHANNEL;
      frontleft : float;
      frontright : float;
      center : float;
      lfe : float;
      surroundleft : float;
      surroundright : float;
      backleft : float;
      backright : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:269
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetMixLevelsOutput";

   function FMOD_Channel_SetMixLevelsInput
     (channel : access fmod_common_h.FMOD_CHANNEL;
      levels : access float;
      numlevels : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:270
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetMixLevelsInput";

   function FMOD_Channel_SetMixMatrix
     (channel : access fmod_common_h.FMOD_CHANNEL;
      matrix : access float;
      outchannels : int;
      inchannels : int;
      inchannel_hop : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:271
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetMixMatrix";

   function FMOD_Channel_GetMixMatrix
     (channel : access fmod_common_h.FMOD_CHANNEL;
      matrix : access float;
      outchannels : access int;
      inchannels : access int;
      inchannel_hop : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:272
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetMixMatrix";

  --     Clock based functionality.
  -- 

   function FMOD_Channel_GetDSPClock
     (channel : access fmod_common_h.FMOD_CHANNEL;
      dspclock : access Extensions.unsigned_long_long;
      parentclock : access Extensions.unsigned_long_long) return fmod_common_h.FMOD_RESULT  -- fmod.h:278
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetDSPClock";

   function FMOD_Channel_SetDelay
     (channel : access fmod_common_h.FMOD_CHANNEL;
      dspclock_start : Extensions.unsigned_long_long;
      dspclock_end : Extensions.unsigned_long_long;
      stopchannels : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:279
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetDelay";

   function FMOD_Channel_GetDelay
     (channel : access fmod_common_h.FMOD_CHANNEL;
      dspclock_start : access Extensions.unsigned_long_long;
      dspclock_end : access Extensions.unsigned_long_long;
      stopchannels : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:280
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetDelay";

   function FMOD_Channel_AddFadePoint
     (channel : access fmod_common_h.FMOD_CHANNEL;
      dspclock : Extensions.unsigned_long_long;
      volume : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:281
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_AddFadePoint";

   function FMOD_Channel_SetFadePointRamp
     (channel : access fmod_common_h.FMOD_CHANNEL;
      dspclock : Extensions.unsigned_long_long;
      volume : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:282
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetFadePointRamp";

   function FMOD_Channel_RemoveFadePoints
     (channel : access fmod_common_h.FMOD_CHANNEL;
      dspclock_start : Extensions.unsigned_long_long;
      dspclock_end : Extensions.unsigned_long_long) return fmod_common_h.FMOD_RESULT  -- fmod.h:283
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_RemoveFadePoints";

   function FMOD_Channel_GetFadePoints
     (channel : access fmod_common_h.FMOD_CHANNEL;
      numpoints : access unsigned;
      point_dspclock : access Extensions.unsigned_long_long;
      point_volume : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:284
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetFadePoints";

  --     DSP effects.
  -- 

   function FMOD_Channel_GetDSP
     (channel : access fmod_common_h.FMOD_CHANNEL;
      index : int;
      dsp : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:290
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetDSP";

   function FMOD_Channel_AddDSP
     (channel : access fmod_common_h.FMOD_CHANNEL;
      index : int;
      dsp : access fmod_common_h.FMOD_DSP) return fmod_common_h.FMOD_RESULT  -- fmod.h:291
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_AddDSP";

   function FMOD_Channel_RemoveDSP (channel : access fmod_common_h.FMOD_CHANNEL; dsp : access fmod_common_h.FMOD_DSP) return fmod_common_h.FMOD_RESULT  -- fmod.h:292
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_RemoveDSP";

   function FMOD_Channel_GetNumDSPs (channel : access fmod_common_h.FMOD_CHANNEL; numdsps : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:293
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetNumDSPs";

   function FMOD_Channel_SetDSPIndex
     (channel : access fmod_common_h.FMOD_CHANNEL;
      dsp : access fmod_common_h.FMOD_DSP;
      index : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:294
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetDSPIndex";

   function FMOD_Channel_GetDSPIndex
     (channel : access fmod_common_h.FMOD_CHANNEL;
      dsp : access fmod_common_h.FMOD_DSP;
      index : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:295
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetDSPIndex";

  --     3D functionality.
  -- 

   function FMOD_Channel_Set3DAttributes
     (channel : access fmod_common_h.FMOD_CHANNEL;
      pos : access constant fmod_common_h.FMOD_VECTOR;
      vel : access constant fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.h:301
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Set3DAttributes";

   function FMOD_Channel_Get3DAttributes
     (channel : access fmod_common_h.FMOD_CHANNEL;
      pos : access fmod_common_h.FMOD_VECTOR;
      vel : access fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.h:302
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Get3DAttributes";

   function FMOD_Channel_Set3DMinMaxDistance
     (channel : access fmod_common_h.FMOD_CHANNEL;
      mindistance : float;
      maxdistance : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:303
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Set3DMinMaxDistance";

   function FMOD_Channel_Get3DMinMaxDistance
     (channel : access fmod_common_h.FMOD_CHANNEL;
      mindistance : access float;
      maxdistance : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:304
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Get3DMinMaxDistance";

   function FMOD_Channel_Set3DConeSettings
     (channel : access fmod_common_h.FMOD_CHANNEL;
      insideconeangle : float;
      outsideconeangle : float;
      outsidevolume : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:305
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Set3DConeSettings";

   function FMOD_Channel_Get3DConeSettings
     (channel : access fmod_common_h.FMOD_CHANNEL;
      insideconeangle : access float;
      outsideconeangle : access float;
      outsidevolume : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:306
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Get3DConeSettings";

   function FMOD_Channel_Set3DConeOrientation (channel : access fmod_common_h.FMOD_CHANNEL; orientation : access fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.h:307
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Set3DConeOrientation";

   function FMOD_Channel_Get3DConeOrientation (channel : access fmod_common_h.FMOD_CHANNEL; orientation : access fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.h:308
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Get3DConeOrientation";

   function FMOD_Channel_Set3DCustomRolloff
     (channel : access fmod_common_h.FMOD_CHANNEL;
      points : access fmod_common_h.FMOD_VECTOR;
      numpoints : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:309
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Set3DCustomRolloff";

   function FMOD_Channel_Get3DCustomRolloff
     (channel : access fmod_common_h.FMOD_CHANNEL;
      points : System.Address;
      numpoints : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:310
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Get3DCustomRolloff";

   function FMOD_Channel_Set3DOcclusion
     (channel : access fmod_common_h.FMOD_CHANNEL;
      directocclusion : float;
      reverbocclusion : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:311
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Set3DOcclusion";

   function FMOD_Channel_Get3DOcclusion
     (channel : access fmod_common_h.FMOD_CHANNEL;
      directocclusion : access float;
      reverbocclusion : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:312
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Get3DOcclusion";

   function FMOD_Channel_Set3DSpread (channel : access fmod_common_h.FMOD_CHANNEL; angle : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:313
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Set3DSpread";

   function FMOD_Channel_Get3DSpread (channel : access fmod_common_h.FMOD_CHANNEL; angle : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:314
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Get3DSpread";

   function FMOD_Channel_Set3DLevel (channel : access fmod_common_h.FMOD_CHANNEL; level : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:315
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Set3DLevel";

   function FMOD_Channel_Get3DLevel (channel : access fmod_common_h.FMOD_CHANNEL; level : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:316
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Get3DLevel";

   function FMOD_Channel_Set3DDopplerLevel (channel : access fmod_common_h.FMOD_CHANNEL; level : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:317
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Set3DDopplerLevel";

   function FMOD_Channel_Get3DDopplerLevel (channel : access fmod_common_h.FMOD_CHANNEL; level : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:318
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Get3DDopplerLevel";

   function FMOD_Channel_Set3DDistanceFilter
     (channel : access fmod_common_h.FMOD_CHANNEL;
      custom : fmod_common_h.FMOD_BOOL;
      customLevel : float;
      centerFreq : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:319
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Set3DDistanceFilter";

   function FMOD_Channel_Get3DDistanceFilter
     (channel : access fmod_common_h.FMOD_CHANNEL;
      custom : access fmod_common_h.FMOD_BOOL;
      customLevel : access float;
      centerFreq : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:320
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_Get3DDistanceFilter";

  --     Userdata set/get.
  -- 

   function FMOD_Channel_SetUserData (channel : access fmod_common_h.FMOD_CHANNEL; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:326
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetUserData";

   function FMOD_Channel_GetUserData (channel : access fmod_common_h.FMOD_CHANNEL; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:327
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetUserData";

  --     Channel specific control functionality.
  -- 

   function FMOD_Channel_SetFrequency (channel : access fmod_common_h.FMOD_CHANNEL; frequency : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:333
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetFrequency";

   function FMOD_Channel_GetFrequency (channel : access fmod_common_h.FMOD_CHANNEL; frequency : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:334
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetFrequency";

   function FMOD_Channel_SetPriority (channel : access fmod_common_h.FMOD_CHANNEL; priority : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:335
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetPriority";

   function FMOD_Channel_GetPriority (channel : access fmod_common_h.FMOD_CHANNEL; priority : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:336
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetPriority";

   function FMOD_Channel_SetPosition
     (channel : access fmod_common_h.FMOD_CHANNEL;
      position : unsigned;
      postype : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT  -- fmod.h:337
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetPosition";

   function FMOD_Channel_GetPosition
     (channel : access fmod_common_h.FMOD_CHANNEL;
      position : access unsigned;
      postype : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT  -- fmod.h:338
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetPosition";

   function FMOD_Channel_SetChannelGroup (channel : access fmod_common_h.FMOD_CHANNEL; channelgroup : access fmod_common_h.FMOD_CHANNELGROUP) return fmod_common_h.FMOD_RESULT  -- fmod.h:339
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetChannelGroup";

   function FMOD_Channel_GetChannelGroup (channel : access fmod_common_h.FMOD_CHANNEL; channelgroup : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:340
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetChannelGroup";

   function FMOD_Channel_SetLoopCount (channel : access fmod_common_h.FMOD_CHANNEL; loopcount : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:341
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetLoopCount";

   function FMOD_Channel_GetLoopCount (channel : access fmod_common_h.FMOD_CHANNEL; loopcount : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:342
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetLoopCount";

   function FMOD_Channel_SetLoopPoints
     (channel : access fmod_common_h.FMOD_CHANNEL;
      loopstart : unsigned;
      loopstarttype : fmod_common_h.FMOD_TIMEUNIT;
      loopend : unsigned;
      loopendtype : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT  -- fmod.h:343
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_SetLoopPoints";

   function FMOD_Channel_GetLoopPoints
     (channel : access fmod_common_h.FMOD_CHANNEL;
      loopstart : access unsigned;
      loopstarttype : fmod_common_h.FMOD_TIMEUNIT;
      loopend : access unsigned;
      loopendtype : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT  -- fmod.h:344
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetLoopPoints";

  --     Information only functions.
  -- 

   function FMOD_Channel_IsVirtual (channel : access fmod_common_h.FMOD_CHANNEL; isvirtual : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:350
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_IsVirtual";

   function FMOD_Channel_GetCurrentSound (channel : access fmod_common_h.FMOD_CHANNEL; sound : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:351
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetCurrentSound";

   function FMOD_Channel_GetIndex (channel : access fmod_common_h.FMOD_CHANNEL; index : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:352
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Channel_GetIndex";

  --    'ChannelGroup' API
  -- 

   function FMOD_ChannelGroup_GetSystemObject (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; c_system : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:358
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetSystemObject";

  --     General control functionality for Channels and ChannelGroups.
  -- 

   function FMOD_ChannelGroup_Stop (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP) return fmod_common_h.FMOD_RESULT  -- fmod.h:364
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Stop";

   function FMOD_ChannelGroup_SetPaused (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; paused : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:365
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_SetPaused";

   function FMOD_ChannelGroup_GetPaused (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; paused : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:366
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetPaused";

   function FMOD_ChannelGroup_SetVolume (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; volume : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:367
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_SetVolume";

   function FMOD_ChannelGroup_GetVolume (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; volume : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:368
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetVolume";

   function FMOD_ChannelGroup_SetVolumeRamp (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; ramp : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:369
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_SetVolumeRamp";

   function FMOD_ChannelGroup_GetVolumeRamp (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; ramp : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:370
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetVolumeRamp";

   function FMOD_ChannelGroup_GetAudibility (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; audibility : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:371
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetAudibility";

   function FMOD_ChannelGroup_SetPitch (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; pitch : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:372
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_SetPitch";

   function FMOD_ChannelGroup_GetPitch (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; pitch : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:373
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetPitch";

   function FMOD_ChannelGroup_SetMute (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; mute : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:374
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_SetMute";

   function FMOD_ChannelGroup_GetMute (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; mute : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:375
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetMute";

   function FMOD_ChannelGroup_SetReverbProperties
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      instance : int;
      wet : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:376
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_SetReverbProperties";

   function FMOD_ChannelGroup_GetReverbProperties
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      instance : int;
      wet : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:377
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetReverbProperties";

   function FMOD_ChannelGroup_SetLowPassGain (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; gain : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:378
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_SetLowPassGain";

   function FMOD_ChannelGroup_GetLowPassGain (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; gain : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:379
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetLowPassGain";

   function FMOD_ChannelGroup_SetMode (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; mode : fmod_common_h.FMOD_MODE) return fmod_common_h.FMOD_RESULT  -- fmod.h:380
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_SetMode";

   function FMOD_ChannelGroup_GetMode (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; mode : access fmod_common_h.FMOD_MODE) return fmod_common_h.FMOD_RESULT  -- fmod.h:381
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetMode";

   function FMOD_ChannelGroup_SetCallback (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; callback : fmod_common_h.FMOD_CHANNELCONTROL_CALLBACK) return fmod_common_h.FMOD_RESULT  -- fmod.h:382
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_SetCallback";

   function FMOD_ChannelGroup_IsPlaying (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; isplaying : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:383
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_IsPlaying";

  --     Note all 'set' functions alter a final matrix, this is why the only get function is getMixMatrix, to avoid other get functions returning incorrect/obsolete values.
  -- 

   function FMOD_ChannelGroup_SetPan (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; pan : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:389
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_SetPan";

   function FMOD_ChannelGroup_SetMixLevelsOutput
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      frontleft : float;
      frontright : float;
      center : float;
      lfe : float;
      surroundleft : float;
      surroundright : float;
      backleft : float;
      backright : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:390
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_SetMixLevelsOutput";

   function FMOD_ChannelGroup_SetMixLevelsInput
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      levels : access float;
      numlevels : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:391
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_SetMixLevelsInput";

   function FMOD_ChannelGroup_SetMixMatrix
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      matrix : access float;
      outchannels : int;
      inchannels : int;
      inchannel_hop : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:392
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_SetMixMatrix";

   function FMOD_ChannelGroup_GetMixMatrix
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      matrix : access float;
      outchannels : access int;
      inchannels : access int;
      inchannel_hop : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:393
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetMixMatrix";

  --     Clock based functionality.
  -- 

   function FMOD_ChannelGroup_GetDSPClock
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      dspclock : access Extensions.unsigned_long_long;
      parentclock : access Extensions.unsigned_long_long) return fmod_common_h.FMOD_RESULT  -- fmod.h:399
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetDSPClock";

   function FMOD_ChannelGroup_SetDelay
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      dspclock_start : Extensions.unsigned_long_long;
      dspclock_end : Extensions.unsigned_long_long;
      stopchannels : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:400
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_SetDelay";

   function FMOD_ChannelGroup_GetDelay
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      dspclock_start : access Extensions.unsigned_long_long;
      dspclock_end : access Extensions.unsigned_long_long;
      stopchannels : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:401
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetDelay";

   function FMOD_ChannelGroup_AddFadePoint
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      dspclock : Extensions.unsigned_long_long;
      volume : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:402
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_AddFadePoint";

   function FMOD_ChannelGroup_SetFadePointRamp
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      dspclock : Extensions.unsigned_long_long;
      volume : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:403
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_SetFadePointRamp";

   function FMOD_ChannelGroup_RemoveFadePoints
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      dspclock_start : Extensions.unsigned_long_long;
      dspclock_end : Extensions.unsigned_long_long) return fmod_common_h.FMOD_RESULT  -- fmod.h:404
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_RemoveFadePoints";

   function FMOD_ChannelGroup_GetFadePoints
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      numpoints : access unsigned;
      point_dspclock : access Extensions.unsigned_long_long;
      point_volume : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:405
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetFadePoints";

  --     DSP effects.
  -- 

   function FMOD_ChannelGroup_GetDSP
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      index : int;
      dsp : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:411
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetDSP";

   function FMOD_ChannelGroup_AddDSP
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      index : int;
      dsp : access fmod_common_h.FMOD_DSP) return fmod_common_h.FMOD_RESULT  -- fmod.h:412
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_AddDSP";

   function FMOD_ChannelGroup_RemoveDSP (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; dsp : access fmod_common_h.FMOD_DSP) return fmod_common_h.FMOD_RESULT  -- fmod.h:413
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_RemoveDSP";

   function FMOD_ChannelGroup_GetNumDSPs (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; numdsps : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:414
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetNumDSPs";

   function FMOD_ChannelGroup_SetDSPIndex
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      dsp : access fmod_common_h.FMOD_DSP;
      index : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:415
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_SetDSPIndex";

   function FMOD_ChannelGroup_GetDSPIndex
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      dsp : access fmod_common_h.FMOD_DSP;
      index : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:416
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetDSPIndex";

  --     3D functionality.
  -- 

   function FMOD_ChannelGroup_Set3DAttributes
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      pos : access constant fmod_common_h.FMOD_VECTOR;
      vel : access constant fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.h:422
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Set3DAttributes";

   function FMOD_ChannelGroup_Get3DAttributes
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      pos : access fmod_common_h.FMOD_VECTOR;
      vel : access fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.h:423
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Get3DAttributes";

   function FMOD_ChannelGroup_Set3DMinMaxDistance
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      mindistance : float;
      maxdistance : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:424
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Set3DMinMaxDistance";

   function FMOD_ChannelGroup_Get3DMinMaxDistance
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      mindistance : access float;
      maxdistance : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:425
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Get3DMinMaxDistance";

   function FMOD_ChannelGroup_Set3DConeSettings
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      insideconeangle : float;
      outsideconeangle : float;
      outsidevolume : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:426
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Set3DConeSettings";

   function FMOD_ChannelGroup_Get3DConeSettings
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      insideconeangle : access float;
      outsideconeangle : access float;
      outsidevolume : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:427
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Get3DConeSettings";

   function FMOD_ChannelGroup_Set3DConeOrientation (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; orientation : access fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.h:428
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Set3DConeOrientation";

   function FMOD_ChannelGroup_Get3DConeOrientation (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; orientation : access fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.h:429
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Get3DConeOrientation";

   function FMOD_ChannelGroup_Set3DCustomRolloff
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      points : access fmod_common_h.FMOD_VECTOR;
      numpoints : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:430
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Set3DCustomRolloff";

   function FMOD_ChannelGroup_Get3DCustomRolloff
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      points : System.Address;
      numpoints : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:431
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Get3DCustomRolloff";

   function FMOD_ChannelGroup_Set3DOcclusion
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      directocclusion : float;
      reverbocclusion : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:432
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Set3DOcclusion";

   function FMOD_ChannelGroup_Get3DOcclusion
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      directocclusion : access float;
      reverbocclusion : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:433
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Get3DOcclusion";

   function FMOD_ChannelGroup_Set3DSpread (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; angle : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:434
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Set3DSpread";

   function FMOD_ChannelGroup_Get3DSpread (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; angle : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:435
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Get3DSpread";

   function FMOD_ChannelGroup_Set3DLevel (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; level : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:436
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Set3DLevel";

   function FMOD_ChannelGroup_Get3DLevel (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; level : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:437
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Get3DLevel";

   function FMOD_ChannelGroup_Set3DDopplerLevel (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; level : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:438
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Set3DDopplerLevel";

   function FMOD_ChannelGroup_Get3DDopplerLevel (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; level : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:439
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Get3DDopplerLevel";

   function FMOD_ChannelGroup_Set3DDistanceFilter
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      custom : fmod_common_h.FMOD_BOOL;
      customLevel : float;
      centerFreq : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:440
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Set3DDistanceFilter";

   function FMOD_ChannelGroup_Get3DDistanceFilter
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      custom : access fmod_common_h.FMOD_BOOL;
      customLevel : access float;
      centerFreq : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:441
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Get3DDistanceFilter";

  --     Userdata set/get.
  -- 

   function FMOD_ChannelGroup_SetUserData (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:447
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_SetUserData";

   function FMOD_ChannelGroup_GetUserData (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:448
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetUserData";

   function FMOD_ChannelGroup_Release (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP) return fmod_common_h.FMOD_RESULT  -- fmod.h:450
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_Release";

  --     Nested channel groups.
  -- 

   function FMOD_ChannelGroup_AddGroup
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      group : access fmod_common_h.FMOD_CHANNELGROUP;
      propagatedspclock : fmod_common_h.FMOD_BOOL;
      connection : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:456
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_AddGroup";

   function FMOD_ChannelGroup_GetNumGroups (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; numgroups : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:457
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetNumGroups";

   function FMOD_ChannelGroup_GetGroup
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      index : int;
      group : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:458
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetGroup";

   function FMOD_ChannelGroup_GetParentGroup (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; group : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:459
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetParentGroup";

  --     Information only functions.
  -- 

   function FMOD_ChannelGroup_GetName
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      name : Interfaces.C.Strings.chars_ptr;
      namelen : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:465
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetName";

   function FMOD_ChannelGroup_GetNumChannels (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP; numchannels : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:466
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetNumChannels";

   function FMOD_ChannelGroup_GetChannel
     (channelgroup : access fmod_common_h.FMOD_CHANNELGROUP;
      index : int;
      channel : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:467
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_ChannelGroup_GetChannel";

  --    'SoundGroup' API
  -- 

   function FMOD_SoundGroup_Release (soundgroup : access fmod_common_h.FMOD_SOUNDGROUP) return fmod_common_h.FMOD_RESULT  -- fmod.h:473
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_SoundGroup_Release";

   function FMOD_SoundGroup_GetSystemObject (soundgroup : access fmod_common_h.FMOD_SOUNDGROUP; c_system : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:474
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_SoundGroup_GetSystemObject";

  --     SoundGroup control functions.
  -- 

   function FMOD_SoundGroup_SetMaxAudible (soundgroup : access fmod_common_h.FMOD_SOUNDGROUP; maxaudible : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:480
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_SoundGroup_SetMaxAudible";

   function FMOD_SoundGroup_GetMaxAudible (soundgroup : access fmod_common_h.FMOD_SOUNDGROUP; maxaudible : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:481
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_SoundGroup_GetMaxAudible";

   function FMOD_SoundGroup_SetMaxAudibleBehavior (soundgroup : access fmod_common_h.FMOD_SOUNDGROUP; behavior : fmod_common_h.FMOD_SOUNDGROUP_BEHAVIOR) return fmod_common_h.FMOD_RESULT  -- fmod.h:482
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_SoundGroup_SetMaxAudibleBehavior";

   function FMOD_SoundGroup_GetMaxAudibleBehavior (soundgroup : access fmod_common_h.FMOD_SOUNDGROUP; behavior : access fmod_common_h.FMOD_SOUNDGROUP_BEHAVIOR) return fmod_common_h.FMOD_RESULT  -- fmod.h:483
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_SoundGroup_GetMaxAudibleBehavior";

   function FMOD_SoundGroup_SetMuteFadeSpeed (soundgroup : access fmod_common_h.FMOD_SOUNDGROUP; speed : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:484
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_SoundGroup_SetMuteFadeSpeed";

   function FMOD_SoundGroup_GetMuteFadeSpeed (soundgroup : access fmod_common_h.FMOD_SOUNDGROUP; speed : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:485
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_SoundGroup_GetMuteFadeSpeed";

   function FMOD_SoundGroup_SetVolume (soundgroup : access fmod_common_h.FMOD_SOUNDGROUP; volume : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:486
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_SoundGroup_SetVolume";

   function FMOD_SoundGroup_GetVolume (soundgroup : access fmod_common_h.FMOD_SOUNDGROUP; volume : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:487
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_SoundGroup_GetVolume";

   function FMOD_SoundGroup_Stop (soundgroup : access fmod_common_h.FMOD_SOUNDGROUP) return fmod_common_h.FMOD_RESULT  -- fmod.h:488
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_SoundGroup_Stop";

  --     Information only functions.
  -- 

   function FMOD_SoundGroup_GetName
     (soundgroup : access fmod_common_h.FMOD_SOUNDGROUP;
      name : Interfaces.C.Strings.chars_ptr;
      namelen : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:494
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_SoundGroup_GetName";

   function FMOD_SoundGroup_GetNumSounds (soundgroup : access fmod_common_h.FMOD_SOUNDGROUP; numsounds : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:495
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_SoundGroup_GetNumSounds";

   function FMOD_SoundGroup_GetSound
     (soundgroup : access fmod_common_h.FMOD_SOUNDGROUP;
      index : int;
      sound : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:496
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_SoundGroup_GetSound";

   function FMOD_SoundGroup_GetNumPlaying (soundgroup : access fmod_common_h.FMOD_SOUNDGROUP; numplaying : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:497
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_SoundGroup_GetNumPlaying";

  --     Userdata set/get.
  -- 

   function FMOD_SoundGroup_SetUserData (soundgroup : access fmod_common_h.FMOD_SOUNDGROUP; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:503
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_SoundGroup_SetUserData";

   function FMOD_SoundGroup_GetUserData (soundgroup : access fmod_common_h.FMOD_SOUNDGROUP; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:504
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_SoundGroup_GetUserData";

  --    'DSP' API
  -- 

   function FMOD_DSP_Release (dsp : access fmod_common_h.FMOD_DSP) return fmod_common_h.FMOD_RESULT  -- fmod.h:510
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_Release";

   function FMOD_DSP_GetSystemObject (dsp : access fmod_common_h.FMOD_DSP; c_system : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:511
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetSystemObject";

  --     Connection / disconnection / input and output enumeration.
  -- 

   function FMOD_DSP_AddInput
     (dsp : access fmod_common_h.FMOD_DSP;
      input : access fmod_common_h.FMOD_DSP;
      connection : System.Address;
      c_type : fmod_common_h.FMOD_DSPCONNECTION_TYPE) return fmod_common_h.FMOD_RESULT  -- fmod.h:517
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_AddInput";

   function FMOD_DSP_DisconnectFrom
     (dsp : access fmod_common_h.FMOD_DSP;
      target : access fmod_common_h.FMOD_DSP;
      connection : access fmod_common_h.FMOD_DSPCONNECTION) return fmod_common_h.FMOD_RESULT  -- fmod.h:518
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_DisconnectFrom";

   function FMOD_DSP_DisconnectAll
     (dsp : access fmod_common_h.FMOD_DSP;
      inputs : fmod_common_h.FMOD_BOOL;
      outputs : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:519
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_DisconnectAll";

   function FMOD_DSP_GetNumInputs (dsp : access fmod_common_h.FMOD_DSP; numinputs : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:520
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetNumInputs";

   function FMOD_DSP_GetNumOutputs (dsp : access fmod_common_h.FMOD_DSP; numoutputs : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:521
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetNumOutputs";

   function FMOD_DSP_GetInput
     (dsp : access fmod_common_h.FMOD_DSP;
      index : int;
      input : System.Address;
      inputconnection : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:522
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetInput";

   function FMOD_DSP_GetOutput
     (dsp : access fmod_common_h.FMOD_DSP;
      index : int;
      output : System.Address;
      outputconnection : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:523
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetOutput";

  --     DSP unit control.
  -- 

   function FMOD_DSP_SetActive (dsp : access fmod_common_h.FMOD_DSP; active : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:529
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_SetActive";

   function FMOD_DSP_GetActive (dsp : access fmod_common_h.FMOD_DSP; active : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:530
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetActive";

   function FMOD_DSP_SetBypass (dsp : access fmod_common_h.FMOD_DSP; bypass : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:531
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_SetBypass";

   function FMOD_DSP_GetBypass (dsp : access fmod_common_h.FMOD_DSP; bypass : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:532
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetBypass";

   function FMOD_DSP_SetWetDryMix
     (dsp : access fmod_common_h.FMOD_DSP;
      prewet : float;
      postwet : float;
      dry : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:533
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_SetWetDryMix";

   function FMOD_DSP_GetWetDryMix
     (dsp : access fmod_common_h.FMOD_DSP;
      prewet : access float;
      postwet : access float;
      dry : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:534
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetWetDryMix";

   function FMOD_DSP_SetChannelFormat
     (dsp : access fmod_common_h.FMOD_DSP;
      channelmask : fmod_common_h.FMOD_CHANNELMASK;
      numchannels : int;
      source_speakermode : fmod_common_h.FMOD_SPEAKERMODE) return fmod_common_h.FMOD_RESULT  -- fmod.h:535
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_SetChannelFormat";

   function FMOD_DSP_GetChannelFormat
     (dsp : access fmod_common_h.FMOD_DSP;
      channelmask : access fmod_common_h.FMOD_CHANNELMASK;
      numchannels : access int;
      source_speakermode : access fmod_common_h.FMOD_SPEAKERMODE) return fmod_common_h.FMOD_RESULT  -- fmod.h:536
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetChannelFormat";

   function FMOD_DSP_GetOutputChannelFormat
     (dsp : access fmod_common_h.FMOD_DSP;
      inmask : fmod_common_h.FMOD_CHANNELMASK;
      inchannels : int;
      inspeakermode : fmod_common_h.FMOD_SPEAKERMODE;
      outmask : access fmod_common_h.FMOD_CHANNELMASK;
      outchannels : access int;
      outspeakermode : access fmod_common_h.FMOD_SPEAKERMODE) return fmod_common_h.FMOD_RESULT  -- fmod.h:537
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetOutputChannelFormat";

   function FMOD_DSP_Reset (dsp : access fmod_common_h.FMOD_DSP) return fmod_common_h.FMOD_RESULT  -- fmod.h:538
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_Reset";

  --     DSP parameter control.
  -- 

   function FMOD_DSP_SetParameterFloat
     (dsp : access fmod_common_h.FMOD_DSP;
      index : int;
      value : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:544
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_SetParameterFloat";

   function FMOD_DSP_SetParameterInt
     (dsp : access fmod_common_h.FMOD_DSP;
      index : int;
      value : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:545
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_SetParameterInt";

   function FMOD_DSP_SetParameterBool
     (dsp : access fmod_common_h.FMOD_DSP;
      index : int;
      value : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:546
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_SetParameterBool";

   function FMOD_DSP_SetParameterData
     (dsp : access fmod_common_h.FMOD_DSP;
      index : int;
      data : System.Address;
      length : unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.h:547
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_SetParameterData";

   function FMOD_DSP_GetParameterFloat
     (dsp : access fmod_common_h.FMOD_DSP;
      index : int;
      value : access float;
      valuestr : Interfaces.C.Strings.chars_ptr;
      valuestrlen : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:548
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetParameterFloat";

   function FMOD_DSP_GetParameterInt
     (dsp : access fmod_common_h.FMOD_DSP;
      index : int;
      value : access int;
      valuestr : Interfaces.C.Strings.chars_ptr;
      valuestrlen : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:549
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetParameterInt";

   function FMOD_DSP_GetParameterBool
     (dsp : access fmod_common_h.FMOD_DSP;
      index : int;
      value : access fmod_common_h.FMOD_BOOL;
      valuestr : Interfaces.C.Strings.chars_ptr;
      valuestrlen : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:550
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetParameterBool";

   function FMOD_DSP_GetParameterData
     (dsp : access fmod_common_h.FMOD_DSP;
      index : int;
      data : System.Address;
      length : access unsigned;
      valuestr : Interfaces.C.Strings.chars_ptr;
      valuestrlen : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:551
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetParameterData";

   function FMOD_DSP_GetNumParameters (dsp : access fmod_common_h.FMOD_DSP; numparams : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:552
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetNumParameters";

   function FMOD_DSP_GetParameterInfo
     (dsp : access fmod_common_h.FMOD_DSP;
      index : int;
      desc : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:553
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetParameterInfo";

   function FMOD_DSP_GetDataParameterIndex
     (dsp : access fmod_common_h.FMOD_DSP;
      datatype : int;
      index : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:554
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetDataParameterIndex";

   function FMOD_DSP_ShowConfigDialog
     (dsp : access fmod_common_h.FMOD_DSP;
      hwnd : System.Address;
      show : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:555
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_ShowConfigDialog";

  --     DSP attributes.
  -- 

   function FMOD_DSP_GetInfo
     (dsp : access fmod_common_h.FMOD_DSP;
      name : Interfaces.C.Strings.chars_ptr;
      version : access unsigned;
      channels : access int;
      configwidth : access int;
      configheight : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:561
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetInfo";

   function FMOD_DSP_GetType (dsp : access fmod_common_h.FMOD_DSP; c_type : access fmod_dsp_effects_h.FMOD_DSP_TYPE) return fmod_common_h.FMOD_RESULT  -- fmod.h:562
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetType";

   function FMOD_DSP_GetIdle (dsp : access fmod_common_h.FMOD_DSP; idle : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:563
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetIdle";

  --     Userdata set/get.
  -- 

   function FMOD_DSP_SetUserData (dsp : access fmod_common_h.FMOD_DSP; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:569
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_SetUserData";

   function FMOD_DSP_GetUserData (dsp : access fmod_common_h.FMOD_DSP; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:570
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetUserData";

  --     Metering.
  -- 

   function FMOD_DSP_SetMeteringEnabled
     (dsp : access fmod_common_h.FMOD_DSP;
      inputEnabled : fmod_common_h.FMOD_BOOL;
      outputEnabled : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:576
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_SetMeteringEnabled";

   function FMOD_DSP_GetMeteringEnabled
     (dsp : access fmod_common_h.FMOD_DSP;
      inputEnabled : access fmod_common_h.FMOD_BOOL;
      outputEnabled : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:577
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetMeteringEnabled";

   function FMOD_DSP_GetMeteringInfo
     (dsp : access fmod_common_h.FMOD_DSP;
      inputInfo : access fmod_dsp_h.FMOD_DSP_METERING_INFO;
      outputInfo : access fmod_dsp_h.FMOD_DSP_METERING_INFO) return fmod_common_h.FMOD_RESULT  -- fmod.h:578
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetMeteringInfo";

   function FMOD_DSP_GetCPUUsage
     (dsp : access fmod_common_h.FMOD_DSP;
      exclusive : access unsigned;
      inclusive : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.h:579
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSP_GetCPUUsage";

  --    'DSPConnection' API
  -- 

   function FMOD_DSPConnection_GetInput (dspconnection : access fmod_common_h.FMOD_DSPCONNECTION; input : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:585
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSPConnection_GetInput";

   function FMOD_DSPConnection_GetOutput (dspconnection : access fmod_common_h.FMOD_DSPCONNECTION; output : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:586
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSPConnection_GetOutput";

   function FMOD_DSPConnection_SetMix (dspconnection : access fmod_common_h.FMOD_DSPCONNECTION; volume : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:587
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSPConnection_SetMix";

   function FMOD_DSPConnection_GetMix (dspconnection : access fmod_common_h.FMOD_DSPCONNECTION; volume : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:588
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSPConnection_GetMix";

   function FMOD_DSPConnection_SetMixMatrix
     (dspconnection : access fmod_common_h.FMOD_DSPCONNECTION;
      matrix : access float;
      outchannels : int;
      inchannels : int;
      inchannel_hop : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:589
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSPConnection_SetMixMatrix";

   function FMOD_DSPConnection_GetMixMatrix
     (dspconnection : access fmod_common_h.FMOD_DSPCONNECTION;
      matrix : access float;
      outchannels : access int;
      inchannels : access int;
      inchannel_hop : int) return fmod_common_h.FMOD_RESULT  -- fmod.h:590
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSPConnection_GetMixMatrix";

   function FMOD_DSPConnection_GetType (dspconnection : access fmod_common_h.FMOD_DSPCONNECTION; c_type : access fmod_common_h.FMOD_DSPCONNECTION_TYPE) return fmod_common_h.FMOD_RESULT  -- fmod.h:591
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSPConnection_GetType";

  --     Userdata set/get.
  -- 

   function FMOD_DSPConnection_SetUserData (dspconnection : access fmod_common_h.FMOD_DSPCONNECTION; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:597
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSPConnection_SetUserData";

   function FMOD_DSPConnection_GetUserData (dspconnection : access fmod_common_h.FMOD_DSPCONNECTION; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:598
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_DSPConnection_GetUserData";

  --    'Geometry' API
  -- 

   function FMOD_Geometry_Release (geometry : access fmod_common_h.FMOD_GEOMETRY) return fmod_common_h.FMOD_RESULT  -- fmod.h:604
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Geometry_Release";

  --     Polygon manipulation.
  -- 

   function FMOD_Geometry_AddPolygon
     (geometry : access fmod_common_h.FMOD_GEOMETRY;
      directocclusion : float;
      reverbocclusion : float;
      doublesided : fmod_common_h.FMOD_BOOL;
      numvertices : int;
      vertices : access constant fmod_common_h.FMOD_VECTOR;
      polygonindex : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:610
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Geometry_AddPolygon";

   function FMOD_Geometry_GetNumPolygons (geometry : access fmod_common_h.FMOD_GEOMETRY; numpolygons : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:611
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Geometry_GetNumPolygons";

   function FMOD_Geometry_GetMaxPolygons
     (geometry : access fmod_common_h.FMOD_GEOMETRY;
      maxpolygons : access int;
      maxvertices : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:612
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Geometry_GetMaxPolygons";

   function FMOD_Geometry_GetPolygonNumVertices
     (geometry : access fmod_common_h.FMOD_GEOMETRY;
      index : int;
      numvertices : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:613
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Geometry_GetPolygonNumVertices";

   function FMOD_Geometry_SetPolygonVertex
     (geometry : access fmod_common_h.FMOD_GEOMETRY;
      index : int;
      vertexindex : int;
      vertex : access constant fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.h:614
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Geometry_SetPolygonVertex";

   function FMOD_Geometry_GetPolygonVertex
     (geometry : access fmod_common_h.FMOD_GEOMETRY;
      index : int;
      vertexindex : int;
      vertex : access fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.h:615
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Geometry_GetPolygonVertex";

   function FMOD_Geometry_SetPolygonAttributes
     (geometry : access fmod_common_h.FMOD_GEOMETRY;
      index : int;
      directocclusion : float;
      reverbocclusion : float;
      doublesided : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:616
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Geometry_SetPolygonAttributes";

   function FMOD_Geometry_GetPolygonAttributes
     (geometry : access fmod_common_h.FMOD_GEOMETRY;
      index : int;
      directocclusion : access float;
      reverbocclusion : access float;
      doublesided : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:617
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Geometry_GetPolygonAttributes";

  --     Object manipulation.
  -- 

   function FMOD_Geometry_SetActive (geometry : access fmod_common_h.FMOD_GEOMETRY; active : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:623
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Geometry_SetActive";

   function FMOD_Geometry_GetActive (geometry : access fmod_common_h.FMOD_GEOMETRY; active : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:624
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Geometry_GetActive";

   function FMOD_Geometry_SetRotation
     (geometry : access fmod_common_h.FMOD_GEOMETRY;
      forward : access constant fmod_common_h.FMOD_VECTOR;
      up : access constant fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.h:625
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Geometry_SetRotation";

   function FMOD_Geometry_GetRotation
     (geometry : access fmod_common_h.FMOD_GEOMETRY;
      forward : access fmod_common_h.FMOD_VECTOR;
      up : access fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.h:626
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Geometry_GetRotation";

   function FMOD_Geometry_SetPosition (geometry : access fmod_common_h.FMOD_GEOMETRY; position : access constant fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.h:627
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Geometry_SetPosition";

   function FMOD_Geometry_GetPosition (geometry : access fmod_common_h.FMOD_GEOMETRY; position : access fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.h:628
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Geometry_GetPosition";

   function FMOD_Geometry_SetScale (geometry : access fmod_common_h.FMOD_GEOMETRY; scale : access constant fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.h:629
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Geometry_SetScale";

   function FMOD_Geometry_GetScale (geometry : access fmod_common_h.FMOD_GEOMETRY; scale : access fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.h:630
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Geometry_GetScale";

   function FMOD_Geometry_Save
     (geometry : access fmod_common_h.FMOD_GEOMETRY;
      data : System.Address;
      datasize : access int) return fmod_common_h.FMOD_RESULT  -- fmod.h:631
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Geometry_Save";

  --     Userdata set/get.
  -- 

   function FMOD_Geometry_SetUserData (geometry : access fmod_common_h.FMOD_GEOMETRY; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:637
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Geometry_SetUserData";

   function FMOD_Geometry_GetUserData (geometry : access fmod_common_h.FMOD_GEOMETRY; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:638
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Geometry_GetUserData";

  --    'Reverb3D' API
  -- 

   function FMOD_Reverb3D_Release (reverb3d : access fmod_common_h.FMOD_REVERB3D) return fmod_common_h.FMOD_RESULT  -- fmod.h:644
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Reverb3D_Release";

  --     Reverb manipulation.
  -- 

   function FMOD_Reverb3D_Set3DAttributes
     (reverb3d : access fmod_common_h.FMOD_REVERB3D;
      position : access constant fmod_common_h.FMOD_VECTOR;
      mindistance : float;
      maxdistance : float) return fmod_common_h.FMOD_RESULT  -- fmod.h:650
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Reverb3D_Set3DAttributes";

   function FMOD_Reverb3D_Get3DAttributes
     (reverb3d : access fmod_common_h.FMOD_REVERB3D;
      position : access fmod_common_h.FMOD_VECTOR;
      mindistance : access float;
      maxdistance : access float) return fmod_common_h.FMOD_RESULT  -- fmod.h:651
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Reverb3D_Get3DAttributes";

   function FMOD_Reverb3D_SetProperties (reverb3d : access fmod_common_h.FMOD_REVERB3D; properties : access constant fmod_common_h.FMOD_REVERB_PROPERTIES) return fmod_common_h.FMOD_RESULT  -- fmod.h:652
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Reverb3D_SetProperties";

   function FMOD_Reverb3D_GetProperties (reverb3d : access fmod_common_h.FMOD_REVERB3D; properties : access fmod_common_h.FMOD_REVERB_PROPERTIES) return fmod_common_h.FMOD_RESULT  -- fmod.h:653
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Reverb3D_GetProperties";

   function FMOD_Reverb3D_SetActive (reverb3d : access fmod_common_h.FMOD_REVERB3D; active : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:654
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Reverb3D_SetActive";

   function FMOD_Reverb3D_GetActive (reverb3d : access fmod_common_h.FMOD_REVERB3D; active : access fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT  -- fmod.h:655
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Reverb3D_GetActive";

  --     Userdata set/get.
  -- 

   function FMOD_Reverb3D_SetUserData (reverb3d : access fmod_common_h.FMOD_REVERB3D; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:661
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Reverb3D_SetUserData";

   function FMOD_Reverb3D_GetUserData (reverb3d : access fmod_common_h.FMOD_REVERB3D; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.h:662
   with Import => True, 
        Convention => C, 
        External_Name => "FMOD_Reverb3D_GetUserData";

end fmod_h;
