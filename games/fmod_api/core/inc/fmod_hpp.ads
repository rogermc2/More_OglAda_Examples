pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with fmod_common_h;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
limited with fmod_codec_h;
limited with fmod_dsp_h;
limited with fmod_output_h;
with fmod_dsp_effects_h;

package fmod_hpp is

  -- ========================================================================================  
  -- FMOD Core API - C++ header file.                                                          
  -- Copyright (c), Firelight Technologies Pty, Ltd. 2004-2021.                                
  --                                                                                           
  -- Use this header in conjunction with fmod_common.h (which contains all the constants /     
  -- callbacks) to develop using the C++ language.                                             
  --                                                                                           
  -- For more detail visit:                                                                    
  -- https://fmod.com/resources/documentation-api?version=2.0&page=core-api.html               
  -- ========================================================================================  
  --    FMOD Namespace
  -- 

  --        FMOD global system functions (optional).
  --     

   function Memory_Initialize
     (poolmem : System.Address;
      poollen : int;
      useralloc : fmod_common_h.FMOD_MEMORY_ALLOC_CALLBACK;
      userrealloc : fmod_common_h.FMOD_MEMORY_REALLOC_CALLBACK;
      userfree : fmod_common_h.FMOD_MEMORY_FREE_CALLBACK;
      memtypeflags : fmod_common_h.FMOD_MEMORY_TYPE) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:36
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZN4FMOD17Memory_InitializeEPviPFS0_jjPKcEPFS0_S0_jjS2_EPFvS0_jS2_Ej";

   function Memory_GetStats
     (currentalloced : access int;
      maxalloced : access int;
      blocking : Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:37
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZN4FMOD15Memory_GetStatsEPiS0_b";

   function Debug_Initialize
     (flags : fmod_common_h.FMOD_DEBUG_FLAGS;
      mode : fmod_common_h.FMOD_DEBUG_MODE;
      callback : fmod_common_h.FMOD_DEBUG_CALLBACK;
      filename : Interfaces.C.Strings.chars_ptr) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:38
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZN4FMOD16Debug_InitializeEj15FMOD_DEBUG_MODEPF11FMOD_RESULTjPKciS3_S3_ES3_";

   function File_SetDiskBusy (busy : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:39
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZN4FMOD16File_SetDiskBusyEi";

   function File_GetDiskBusy (busy : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:40
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZN4FMOD16File_GetDiskBusyEPi";

   function Thread_SetAttributes
     (c_type : fmod_common_h.FMOD_THREAD_TYPE;
      affinity : fmod_common_h.FMOD_THREAD_AFFINITY;
      priority : fmod_common_h.FMOD_THREAD_PRIORITY;
      stacksize : fmod_common_h.FMOD_THREAD_STACK_SIZE) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:41
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZN4FMOD20Thread_SetAttributesE16FMOD_THREAD_TYPEyij";

  --        FMOD System factory functions.
  --     

   type System;
   function System_Create (the_c_system : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:46
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZN4FMOD13System_CreateEPPNS_6SystemE";

  --       'System' API
  --     

  -- Constructor made private so user cannot statically instance a System class.  System_Create must be used.
   package Class_System is
      type System is limited record
         null;
      end record
      with Import => True,
           Convention => CPP;

      function New_System return System;  -- fmod.hpp:56
      pragma CPP_Constructor (New_System, "_ZN4FMOD6SystemC1Ev");

      function release (this : access System) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:61
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System7releaseEv";

      function setOutput (this : access System; output : fmod_common_h.FMOD_OUTPUTTYPE) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:64
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System9setOutputE15FMOD_OUTPUTTYPE";

      function getOutput (this : access System; output : access fmod_common_h.FMOD_OUTPUTTYPE) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:65
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System9getOutputEP15FMOD_OUTPUTTYPE";

      function getNumDrivers (this : access System; numdrivers : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:66
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System13getNumDriversEPi";

      function getDriverInfo
        (this : access System;
         id : int;
         name : Interfaces.C.Strings.chars_ptr;
         namelen : int;
         guid : access fmod_common_h.FMOD_GUID;
         systemrate : access int;
         speakermode : access fmod_common_h.FMOD_SPEAKERMODE;
         speakermodechannels : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:67
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System13getDriverInfoEiPciP9FMOD_GUIDPiP16FMOD_SPEAKERMODES4_";

      function setDriver (this : access System; driver : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:68
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System9setDriverEi";

      function getDriver (this : access System; driver : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:69
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System9getDriverEPi";

      function setSoftwareChannels (this : access System; numsoftwarechannels : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:70
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System19setSoftwareChannelsEi";

      function getSoftwareChannels (this : access System; numsoftwarechannels : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:71
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System19getSoftwareChannelsEPi";

      function setSoftwareFormat
        (this : access System;
         samplerate : int;
         speakermode : fmod_common_h.FMOD_SPEAKERMODE;
         numrawspeakers : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:72
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System17setSoftwareFormatEi16FMOD_SPEAKERMODEi";

      function getSoftwareFormat
        (this : access System;
         samplerate : access int;
         speakermode : access fmod_common_h.FMOD_SPEAKERMODE;
         numrawspeakers : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:73
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System17getSoftwareFormatEPiP16FMOD_SPEAKERMODES1_";

      function setDSPBufferSize
        (this : access System;
         bufferlength : unsigned;
         numbuffers : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:74
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System16setDSPBufferSizeEji";

      function getDSPBufferSize
        (this : access System;
         bufferlength : access unsigned;
         numbuffers : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:75
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System16getDSPBufferSizeEPjPi";

      function setFileSystem
        (this : access System;
         useropen : fmod_common_h.FMOD_FILE_OPEN_CALLBACK;
         userclose : fmod_common_h.FMOD_FILE_CLOSE_CALLBACK;
         userread : fmod_common_h.FMOD_FILE_READ_CALLBACK;
         userseek : fmod_common_h.FMOD_FILE_SEEK_CALLBACK;
         userasyncread : fmod_common_h.FMOD_FILE_ASYNCREAD_CALLBACK;
         userasynccancel : fmod_common_h.FMOD_FILE_ASYNCCANCEL_CALLBACK;
         blockalign : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:76
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System13setFileSystemEPF11FMOD_RESULTPKcPjPPvS5_EPFS1_S5_S5_EPFS1_S5_S5_jS4_S5_EPFS1_S5_jS5_EPFS1_P18FMOD_ASYNCREADINFOS5_ESI_i";

      function attachFileSystem
        (this : access System;
         useropen : fmod_common_h.FMOD_FILE_OPEN_CALLBACK;
         userclose : fmod_common_h.FMOD_FILE_CLOSE_CALLBACK;
         userread : fmod_common_h.FMOD_FILE_READ_CALLBACK;
         userseek : fmod_common_h.FMOD_FILE_SEEK_CALLBACK) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:77
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System16attachFileSystemEPF11FMOD_RESULTPKcPjPPvS5_EPFS1_S5_S5_EPFS1_S5_S5_jS4_S5_EPFS1_S5_jS5_E";

      function setAdvancedSettings (this : access System; settings : access fmod_common_h.FMOD_ADVANCEDSETTINGS) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:78
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System19setAdvancedSettingsEP21FMOD_ADVANCEDSETTINGS";

      function getAdvancedSettings (this : access System; settings : access fmod_common_h.FMOD_ADVANCEDSETTINGS) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:79
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System19getAdvancedSettingsEP21FMOD_ADVANCEDSETTINGS";

      function setCallback
        (this : access System;
         callback : fmod_common_h.FMOD_SYSTEM_CALLBACK;
         callbackmask : fmod_common_h.FMOD_SYSTEM_CALLBACK_TYPE) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:80
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System11setCallbackEPF11FMOD_RESULTP11FMOD_SYSTEMjPvS4_S4_Ej";

      function setPluginPath (this : access System; path : Interfaces.C.Strings.chars_ptr) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:83
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System13setPluginPathEPKc";

      function loadPlugin
        (this : access System;
         filename : Interfaces.C.Strings.chars_ptr;
         handle : access unsigned;
         priority : unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:84
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System10loadPluginEPKcPjj";

      function unloadPlugin (this : access System; handle : unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:85
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System12unloadPluginEj";

      function getNumNestedPlugins
        (this : access System;
         handle : unsigned;
         count : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:86
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System19getNumNestedPluginsEjPi";

      function getNestedPlugin
        (this : access System;
         handle : unsigned;
         index : int;
         nestedhandle : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:87
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System15getNestedPluginEjiPj";

      function getNumPlugins
        (this : access System;
         plugintype : fmod_common_h.FMOD_PLUGINTYPE;
         numplugins : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:88
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System13getNumPluginsE15FMOD_PLUGINTYPEPi";

      function getPluginHandle
        (this : access System;
         plugintype : fmod_common_h.FMOD_PLUGINTYPE;
         index : int;
         handle : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:89
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System15getPluginHandleE15FMOD_PLUGINTYPEiPj";

      function getPluginInfo
        (this : access System;
         handle : unsigned;
         plugintype : access fmod_common_h.FMOD_PLUGINTYPE;
         name : Interfaces.C.Strings.chars_ptr;
         namelen : int;
         version : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:90
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System13getPluginInfoEjP15FMOD_PLUGINTYPEPciPj";

      function setOutputByPlugin (this : access System; handle : unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:91
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System17setOutputByPluginEj";

      function getOutputByPlugin (this : access System; handle : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:92
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System17getOutputByPluginEPj";

      function createDSPByPlugin
        (this : access System;
         handle : unsigned;
         the_dsp : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:93
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System17createDSPByPluginEjPPNS_3DSPE";

      function getDSPInfoByPlugin
        (this : access System;
         handle : unsigned;
         description : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:94
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System18getDSPInfoByPluginEjPPK20FMOD_DSP_DESCRIPTION";

      function registerCodec
        (this : access System;
         description : access fmod_codec_h.FMOD_CODEC_DESCRIPTION;
         handle : access unsigned;
         priority : unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:95
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System13registerCodecEP22FMOD_CODEC_DESCRIPTIONPjj";

      function registerDSP
        (this : access System;
         description : access constant fmod_dsp_h.FMOD_DSP_DESCRIPTION;
         handle : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:96
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System11registerDSPEPK20FMOD_DSP_DESCRIPTIONPj";

      function registerOutput
        (this : access System;
         description : access constant fmod_output_h.FMOD_OUTPUT_DESCRIPTION;
         handle : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:97
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System14registerOutputEPK23FMOD_OUTPUT_DESCRIPTIONPj";

      function init
        (this : access System;
         maxchannels : int;
         flags : fmod_common_h.FMOD_INITFLAGS;
         extradriverdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:100
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System4initEijPv";

      function close (this : access System) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:101
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System5closeEv";

      function update (this : access System) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:104
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System6updateEv";

      function setSpeakerPosition
        (this : access System;
         speaker : fmod_common_h.FMOD_SPEAKER;
         x : float;
         y : float;
         active : Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:106
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System18setSpeakerPositionE12FMOD_SPEAKERffb";

      function getSpeakerPosition
        (this : access System;
         speaker : fmod_common_h.FMOD_SPEAKER;
         x : access float;
         y : access float;
         active : access Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:107
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System18getSpeakerPositionE12FMOD_SPEAKERPfS2_Pb";

      function setStreamBufferSize
        (this : access System;
         filebuffersize : unsigned;
         filebuffersizetype : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:108
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System19setStreamBufferSizeEjj";

      function getStreamBufferSize
        (this : access System;
         filebuffersize : access unsigned;
         filebuffersizetype : access fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:109
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System19getStreamBufferSizeEPjS1_";

      function set3DSettings
        (this : access System;
         dopplerscale : float;
         distancefactor : float;
         rolloffscale : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:110
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System13set3DSettingsEfff";

      function get3DSettings
        (this : access System;
         dopplerscale : access float;
         distancefactor : access float;
         rolloffscale : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:111
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System13get3DSettingsEPfS1_S1_";

      function set3DNumListeners (this : access System; numlisteners : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:112
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System17set3DNumListenersEi";

      function get3DNumListeners (this : access System; numlisteners : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:113
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System17get3DNumListenersEPi";

      function set3DListenerAttributes
        (this : access System;
         listener : int;
         pos : access constant fmod_common_h.FMOD_VECTOR;
         vel : access constant fmod_common_h.FMOD_VECTOR;
         forward : access constant fmod_common_h.FMOD_VECTOR;
         up : access constant fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:114
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System23set3DListenerAttributesEiPK11FMOD_VECTORS3_S3_S3_";

      function get3DListenerAttributes
        (this : access System;
         listener : int;
         pos : access fmod_common_h.FMOD_VECTOR;
         vel : access fmod_common_h.FMOD_VECTOR;
         forward : access fmod_common_h.FMOD_VECTOR;
         up : access fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:115
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System23get3DListenerAttributesEiP11FMOD_VECTORS2_S2_S2_";

      function set3DRolloffCallback (this : access System; callback : fmod_common_h.FMOD_3D_ROLLOFF_CALLBACK) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:116
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System20set3DRolloffCallbackEPFfP19FMOD_CHANNELCONTROLfE";

      function mixerSuspend (this : access System) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:117
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System12mixerSuspendEv";

      function mixerResume (this : access System) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:118
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System11mixerResumeEv";

      function getDefaultMixMatrix
        (this : access System;
         sourcespeakermode : fmod_common_h.FMOD_SPEAKERMODE;
         targetspeakermode : fmod_common_h.FMOD_SPEAKERMODE;
         matrix : access float;
         matrixhop : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:119
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System19getDefaultMixMatrixE16FMOD_SPEAKERMODES1_Pfi";

      function getSpeakerModeChannels
        (this : access System;
         mode : fmod_common_h.FMOD_SPEAKERMODE;
         channels : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:120
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System22getSpeakerModeChannelsE16FMOD_SPEAKERMODEPi";

      function getVersion (this : access System; version : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:123
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System10getVersionEPj";

      function getOutputHandle (this : access System; handle : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:124
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System15getOutputHandleEPPv";

      function getChannelsPlaying
        (this : access System;
         channels : access int;
         realchannels : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:125
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System18getChannelsPlayingEPiS1_";

      function getCPUUsage
        (this : access System;
         dsp : access float;
         stream : access float;
         geometry : access float;
         update : access float;
         total : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:126
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System11getCPUUsageEPfS1_S1_S1_S1_";

      function getCPUUsageEx
        (this : access System;
         convolutionThread1 : access float;
         convolutionThread2 : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:127
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System13getCPUUsageExEPfS1_";

      function getFileUsage
        (this : access System;
         sampleBytesRead : access Long_Long_Integer;
         streamBytesRead : access Long_Long_Integer;
         otherBytesRead : access Long_Long_Integer) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:128
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System12getFileUsageEPxS1_S1_";

      function createSound
        (this : access System;
         name_or_data : Interfaces.C.Strings.chars_ptr;
         mode : fmod_common_h.FMOD_MODE;
         exinfo : access fmod_common_h.FMOD_CREATESOUNDEXINFO;
         the_sound : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:131
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System11createSoundEPKcjP22FMOD_CREATESOUNDEXINFOPPNS_5SoundE";

      function createStream
        (this : access System;
         name_or_data : Interfaces.C.Strings.chars_ptr;
         mode : fmod_common_h.FMOD_MODE;
         exinfo : access fmod_common_h.FMOD_CREATESOUNDEXINFO;
         the_sound : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:132
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System12createStreamEPKcjP22FMOD_CREATESOUNDEXINFOPPNS_5SoundE";

      function createDSP
        (this : access System;
         description : access constant fmod_dsp_h.FMOD_DSP_DESCRIPTION;
         the_dsp : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:133
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System9createDSPEPK20FMOD_DSP_DESCRIPTIONPPNS_3DSPE";

      function createDSPByType
        (this : access System;
         c_type : fmod_dsp_effects_h.FMOD_DSP_TYPE;
         the_dsp : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:134
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System15createDSPByTypeE13FMOD_DSP_TYPEPPNS_3DSPE";

      function createChannelGroup
        (this : access System;
         name : Interfaces.C.Strings.chars_ptr;
         the_channelgroup : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:135
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System18createChannelGroupEPKcPPNS_12ChannelGroupE";

      function createSoundGroup
        (this : access System;
         name : Interfaces.C.Strings.chars_ptr;
         the_soundgroup : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:136
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System16createSoundGroupEPKcPPNS_10SoundGroupE";

      function createReverb3D (this : access System; reverb : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:137
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System14createReverb3DEPPNS_8Reverb3DE";

      function playSound
        (this : access System;
         the_sound : access Sound;
         the_channelgroup : access ChannelGroup;
         paused : Extensions.bool;
         the_channel : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:139
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System9playSoundEPNS_5SoundEPNS_12ChannelGroupEbPPNS_7ChannelE";

      function playDSP
        (this : access System;
         the_dsp : access DSP;
         the_channelgroup : access ChannelGroup;
         paused : Extensions.bool;
         the_channel : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:140
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System7playDSPEPNS_3DSPEPNS_12ChannelGroupEbPPNS_7ChannelE";

      function getChannel
        (this : access System;
         channelid : int;
         the_channel : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:141
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System10getChannelEiPPNS_7ChannelE";

      function getDSPInfoByType
        (this : access System;
         c_type : fmod_dsp_effects_h.FMOD_DSP_TYPE;
         description : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:142
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System16getDSPInfoByTypeE13FMOD_DSP_TYPEPPK20FMOD_DSP_DESCRIPTION";

      function getMasterChannelGroup (this : access System; the_channelgroup : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:143
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System21getMasterChannelGroupEPPNS_12ChannelGroupE";

      function getMasterSoundGroup (this : access System; the_soundgroup : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:144
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System19getMasterSoundGroupEPPNS_10SoundGroupE";

      function attachChannelGroupToPort
        (this : access System;
         portType : fmod_common_h.FMOD_PORT_TYPE;
         portIndex : fmod_common_h.FMOD_PORT_INDEX;
         the_channelgroup : access ChannelGroup;
         passThru : Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:147
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System24attachChannelGroupToPortEjyPNS_12ChannelGroupEb";

      function detachChannelGroupFromPort (this : access System; the_channelgroup : access ChannelGroup) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:148
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System26detachChannelGroupFromPortEPNS_12ChannelGroupE";

      function setReverbProperties
        (this : access System;
         instance : int;
         prop : access constant fmod_common_h.FMOD_REVERB_PROPERTIES) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:151
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System19setReverbPropertiesEiPK22FMOD_REVERB_PROPERTIES";

      function getReverbProperties
        (this : access System;
         instance : int;
         prop : access fmod_common_h.FMOD_REVERB_PROPERTIES) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:152
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System19getReverbPropertiesEiP22FMOD_REVERB_PROPERTIES";

      function lockDSP (this : access System) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:155
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System7lockDSPEv";

      function unlockDSP (this : access System) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:156
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System9unlockDSPEv";

      function getRecordNumDrivers
        (this : access System;
         numdrivers : access int;
         numconnected : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:159
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System19getRecordNumDriversEPiS1_";

      function getRecordDriverInfo
        (this : access System;
         id : int;
         name : Interfaces.C.Strings.chars_ptr;
         namelen : int;
         guid : access fmod_common_h.FMOD_GUID;
         systemrate : access int;
         speakermode : access fmod_common_h.FMOD_SPEAKERMODE;
         speakermodechannels : access int;
         state : access fmod_common_h.FMOD_DRIVER_STATE) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:160
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System19getRecordDriverInfoEiPciP9FMOD_GUIDPiP16FMOD_SPEAKERMODES4_Pj";

      function getRecordPosition
        (this : access System;
         id : int;
         position : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:161
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System17getRecordPositionEiPj";

      function recordStart
        (this : access System;
         id : int;
         the_sound : access Sound;
         c_loop : Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:162
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System11recordStartEiPNS_5SoundEb";

      function recordStop (this : access System; id : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:163
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System10recordStopEi";

      function isRecording
        (this : access System;
         id : int;
         recording : access Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:164
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System11isRecordingEiPb";

      function createGeometry
        (this : access System;
         maxpolygons : int;
         maxvertices : int;
         the_geometry : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:167
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System14createGeometryEiiPPNS_8GeometryE";

      function setGeometrySettings (this : access System; maxworldsize : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:168
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System19setGeometrySettingsEf";

      function getGeometrySettings (this : access System; maxworldsize : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:169
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System19getGeometrySettingsEPf";

      function loadGeometry
        (this : access System;
         data : System.Address;
         datasize : int;
         the_geometry : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:170
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System12loadGeometryEPKviPPNS_8GeometryE";

      function getGeometryOcclusion
        (this : access System;
         listener : access constant fmod_common_h.FMOD_VECTOR;
         source : access constant fmod_common_h.FMOD_VECTOR;
         direct : access float;
         reverb : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:171
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System20getGeometryOcclusionEPK11FMOD_VECTORS3_PfS4_";

      function setNetworkProxy (this : access System; proxy : Interfaces.C.Strings.chars_ptr) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:174
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System15setNetworkProxyEPKc";

      function getNetworkProxy
        (this : access System;
         proxy : Interfaces.C.Strings.chars_ptr;
         proxylen : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:175
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System15getNetworkProxyEPci";

      function setNetworkTimeout (this : access System; timeout : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:176
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System17setNetworkTimeoutEi";

      function getNetworkTimeout (this : access System; timeout : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:177
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System17getNetworkTimeoutEPi";

      function setUserData (this : access System; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:180
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System11setUserDataEPv";

      function getUserData (this : access System; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:181
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD6System11getUserDataEPPv";
   end;
   use Class_System;
  -- Setup functions.
  -- Plug-in support.
  -- Init/Close.
  -- General post-init system functions.
  -- IMPORTANT! CALL THIS ONCE PER FRAME!  
  -- System information functions.
  -- Sound/DSP/Channel/FX creation and retrieval.
  -- Routing to ports.
  -- Reverb API.
  -- System level DSP functionality.
  -- Recording API.
  -- Geometry API.
  -- Network functions.
  -- Userdata set/get.
  --        'Sound' API
  --     

  -- Constructor made private so user cannot statically instance a Sound class.  Appropriate Sound creation or retrieval function must be used.
   package Class_Sound is
      type Sound is limited record
         null;
      end record
      with Import => True,
           Convention => CPP;

      function New_Sound return Sound;  -- fmod.hpp:192
      pragma CPP_Constructor (New_Sound, "_ZN4FMOD5SoundC1Ev");

      function release (this : access Sound) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:197
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound7releaseEv";

      function getSystemObject (this : access Sound; the_c_system : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:198
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound15getSystemObjectEPPNS_6SystemE";

      function lock
        (this : access Sound;
         offset : unsigned;
         length : unsigned;
         ptr1 : System.Address;
         ptr2 : System.Address;
         len1 : access unsigned;
         len2 : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:201
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound4lockEjjPPvS2_PjS3_";

      function unlock
        (this : access Sound;
         ptr1 : System.Address;
         ptr2 : System.Address;
         len1 : unsigned;
         len2 : unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:202
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound6unlockEPvS1_jj";

      function setDefaults
        (this : access Sound;
         frequency : float;
         priority : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:203
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound11setDefaultsEfi";

      function getDefaults
        (this : access Sound;
         frequency : access float;
         priority : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:204
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound11getDefaultsEPfPi";

      function set3DMinMaxDistance
        (this : access Sound;
         min : float;
         max : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:205
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound19set3DMinMaxDistanceEff";

      function get3DMinMaxDistance
        (this : access Sound;
         min : access float;
         max : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:206
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound19get3DMinMaxDistanceEPfS1_";

      function set3DConeSettings
        (this : access Sound;
         insideconeangle : float;
         outsideconeangle : float;
         outsidevolume : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:207
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound17set3DConeSettingsEfff";

      function get3DConeSettings
        (this : access Sound;
         insideconeangle : access float;
         outsideconeangle : access float;
         outsidevolume : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:208
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound17get3DConeSettingsEPfS1_S1_";

      function set3DCustomRolloff
        (this : access Sound;
         points : access fmod_common_h.FMOD_VECTOR;
         numpoints : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:209
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound18set3DCustomRolloffEP11FMOD_VECTORi";

      function get3DCustomRolloff
        (this : access Sound;
         points : System.Address;
         numpoints : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:210
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound18get3DCustomRolloffEPP11FMOD_VECTORPi";

      function getSubSound
        (this : access Sound;
         index : int;
         subsound : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:211
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound11getSubSoundEiPPS0_";

      function getSubSoundParent (this : access Sound; parentsound : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:212
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound17getSubSoundParentEPPS0_";

      function getName
        (this : access Sound;
         name : Interfaces.C.Strings.chars_ptr;
         namelen : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:213
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound7getNameEPci";

      function getLength
        (this : access Sound;
         length : access unsigned;
         lengthtype : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:214
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound9getLengthEPjj";

      function getFormat
        (this : access Sound;
         c_type : access fmod_common_h.FMOD_SOUND_TYPE;
         format : access fmod_common_h.FMOD_SOUND_FORMAT;
         channels : access int;
         bits : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:215
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound9getFormatEP15FMOD_SOUND_TYPEP17FMOD_SOUND_FORMATPiS5_";

      function getNumSubSounds (this : access Sound; numsubsounds : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:216
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound15getNumSubSoundsEPi";

      function getNumTags
        (this : access Sound;
         numtags : access int;
         numtagsupdated : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:217
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound10getNumTagsEPiS1_";

      function getTag
        (this : access Sound;
         name : Interfaces.C.Strings.chars_ptr;
         index : int;
         tag : access fmod_common_h.FMOD_TAG) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:218
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound6getTagEPKciP8FMOD_TAG";

      function getOpenState
        (this : access Sound;
         openstate : access fmod_common_h.FMOD_OPENSTATE;
         percentbuffered : access unsigned;
         starving : access Extensions.bool;
         diskbusy : access Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:219
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound12getOpenStateEP14FMOD_OPENSTATEPjPbS4_";

      function readData
        (this : access Sound;
         buffer : System.Address;
         length : unsigned;
         read : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:220
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound8readDataEPvjPj";

      function seekData (this : access Sound; pcm : unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:221
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound8seekDataEj";

      function setSoundGroup (this : access Sound; the_soundgroup : access SoundGroup) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:223
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound13setSoundGroupEPNS_10SoundGroupE";

      function getSoundGroup (this : access Sound; the_soundgroup : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:224
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound13getSoundGroupEPPNS_10SoundGroupE";

      function getNumSyncPoints (this : access Sound; numsyncpoints : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:227
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound16getNumSyncPointsEPi";

      function getSyncPoint
        (this : access Sound;
         index : int;
         point : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:228
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound12getSyncPointEiPP14FMOD_SYNCPOINT";

      function getSyncPointInfo
        (this : access Sound;
         point : access fmod_common_h.FMOD_SYNCPOINT;
         name : Interfaces.C.Strings.chars_ptr;
         namelen : int;
         offset : access unsigned;
         offsettype : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:229
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound16getSyncPointInfoEP14FMOD_SYNCPOINTPciPjj";

      function addSyncPoint
        (this : access Sound;
         offset : unsigned;
         offsettype : fmod_common_h.FMOD_TIMEUNIT;
         name : Interfaces.C.Strings.chars_ptr;
         point : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:230
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound12addSyncPointEjjPKcPP14FMOD_SYNCPOINT";

      function deleteSyncPoint (this : access Sound; point : access fmod_common_h.FMOD_SYNCPOINT) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:231
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound15deleteSyncPointEP14FMOD_SYNCPOINT";

      function setMode (this : access Sound; mode : fmod_common_h.FMOD_MODE) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:234
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound7setModeEj";

      function getMode (this : access Sound; mode : access fmod_common_h.FMOD_MODE) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:235
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound7getModeEPj";

      function setLoopCount (this : access Sound; loopcount : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:236
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound12setLoopCountEi";

      function getLoopCount (this : access Sound; loopcount : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:237
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound12getLoopCountEPi";

      function setLoopPoints
        (this : access Sound;
         loopstart : unsigned;
         loopstarttype : fmod_common_h.FMOD_TIMEUNIT;
         loopend : unsigned;
         loopendtype : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:238
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound13setLoopPointsEjjjj";

      function getLoopPoints
        (this : access Sound;
         loopstart : access unsigned;
         loopstarttype : fmod_common_h.FMOD_TIMEUNIT;
         loopend : access unsigned;
         loopendtype : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:239
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound13getLoopPointsEPjjS1_j";

      function getMusicNumChannels (this : access Sound; numchannels : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:242
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound19getMusicNumChannelsEPi";

      function setMusicChannelVolume
        (this : access Sound;
         channel : int;
         volume : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:243
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound21setMusicChannelVolumeEif";

      function getMusicChannelVolume
        (this : access Sound;
         channel : int;
         volume : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:244
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound21getMusicChannelVolumeEiPf";

      function setMusicSpeed (this : access Sound; speed : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:245
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound13setMusicSpeedEf";

      function getMusicSpeed (this : access Sound; speed : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:246
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound13getMusicSpeedEPf";

      function setUserData (this : access Sound; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:249
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound11setUserDataEPv";

      function getUserData (this : access Sound; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:250
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD5Sound11getUserDataEPPv";
   end;
   use Class_Sound;
  -- Standard sound manipulation functions.
  -- Synchronization point API.  These points can come from markers embedded in wav files, and can also generate channel callbacks.
  -- Functions also in Channel class but here they are the 'default' to save having to change it in Channel all the time.
  -- For MOD/S3M/XM/IT/MID sequenced formats only.
  -- Userdata set/get.
  --        'ChannelControl API'.   This is a base class for Channel and ChannelGroup so they can share the same functionality.  This cannot be used or instansiated explicitly.
  --     

  -- Constructor made private so user cannot statically instance a Control class.
   package Class_ChannelControl is
      type ChannelControl is limited record
         null;
      end record
      with Import => True,
           Convention => CPP;

      function New_ChannelControl return ChannelControl;  -- fmod.hpp:262
      pragma CPP_Constructor (New_ChannelControl, "_ZN4FMOD14ChannelControlC1Ev");

      function getSystemObject (this : access ChannelControl; the_c_system : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:267
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl15getSystemObjectEPPNS_6SystemE";

      function stop (this : access ChannelControl) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:270
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl4stopEv";

      function setPaused (this : access ChannelControl; paused : Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:271
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl9setPausedEb";

      function getPaused (this : access ChannelControl; paused : access Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:272
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl9getPausedEPb";

      function setVolume (this : access ChannelControl; volume : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:273
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl9setVolumeEf";

      function getVolume (this : access ChannelControl; volume : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:274
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl9getVolumeEPf";

      function setVolumeRamp (this : access ChannelControl; ramp : Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:275
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl13setVolumeRampEb";

      function getVolumeRamp (this : access ChannelControl; ramp : access Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:276
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl13getVolumeRampEPb";

      function getAudibility (this : access ChannelControl; audibility : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:277
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl13getAudibilityEPf";

      function setPitch (this : access ChannelControl; pitch : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:278
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl8setPitchEf";

      function getPitch (this : access ChannelControl; pitch : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:279
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl8getPitchEPf";

      function setMute (this : access ChannelControl; mute : Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:280
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl7setMuteEb";

      function getMute (this : access ChannelControl; mute : access Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:281
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl7getMuteEPb";

      function setReverbProperties
        (this : access ChannelControl;
         instance : int;
         wet : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:282
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl19setReverbPropertiesEif";

      function getReverbProperties
        (this : access ChannelControl;
         instance : int;
         wet : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:283
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl19getReverbPropertiesEiPf";

      function setLowPassGain (this : access ChannelControl; gain : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:284
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl14setLowPassGainEf";

      function getLowPassGain (this : access ChannelControl; gain : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:285
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl14getLowPassGainEPf";

      function setMode (this : access ChannelControl; mode : fmod_common_h.FMOD_MODE) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:286
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl7setModeEj";

      function getMode (this : access ChannelControl; mode : access fmod_common_h.FMOD_MODE) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:287
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl7getModeEPj";

      function setCallback (this : access ChannelControl; callback : fmod_common_h.FMOD_CHANNELCONTROL_CALLBACK) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:288
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl11setCallbackEPF11FMOD_RESULTP19FMOD_CHANNELCONTROL24FMOD_CHANNELCONTROL_TYPE33FMOD_CHANNELCONTROL_CALLBACK_TYPEPvS6_E";

      function isPlaying (this : access ChannelControl; isplaying : access Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:289
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl9isPlayingEPb";

      function setPan (this : access ChannelControl; pan : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:293
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl6setPanEf";

      function setMixLevelsOutput
        (this : access ChannelControl;
         frontleft : float;
         frontright : float;
         center : float;
         lfe : float;
         surroundleft : float;
         surroundright : float;
         backleft : float;
         backright : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:294
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl18setMixLevelsOutputEffffffff";

      function setMixLevelsInput
        (this : access ChannelControl;
         levels : access float;
         numlevels : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:295
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl17setMixLevelsInputEPfi";

      function setMixMatrix
        (this : access ChannelControl;
         matrix : access float;
         outchannels : int;
         inchannels : int;
         inchannel_hop : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:296
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl12setMixMatrixEPfiii";

      function getMixMatrix
        (this : access ChannelControl;
         matrix : access float;
         outchannels : access int;
         inchannels : access int;
         inchannel_hop : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:297
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl12getMixMatrixEPfPiS2_i";

      function getDSPClock
        (this : access ChannelControl;
         dspclock : access Extensions.unsigned_long_long;
         parentclock : access Extensions.unsigned_long_long) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:300
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl11getDSPClockEPyS1_";

      function setDelay
        (this : access ChannelControl;
         dspclock_start : Extensions.unsigned_long_long;
         dspclock_end : Extensions.unsigned_long_long;
         stopchannels : Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:301
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl8setDelayEyyb";

      function getDelay
        (this : access ChannelControl;
         dspclock_start : access Extensions.unsigned_long_long;
         dspclock_end : access Extensions.unsigned_long_long;
         stopchannels : access Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:302
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl8getDelayEPyS1_Pb";

      function addFadePoint
        (this : access ChannelControl;
         dspclock : Extensions.unsigned_long_long;
         volume : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:303
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl12addFadePointEyf";

      function setFadePointRamp
        (this : access ChannelControl;
         dspclock : Extensions.unsigned_long_long;
         volume : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:304
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl16setFadePointRampEyf";

      function removeFadePoints
        (this : access ChannelControl;
         dspclock_start : Extensions.unsigned_long_long;
         dspclock_end : Extensions.unsigned_long_long) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:305
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl16removeFadePointsEyy";

      function getFadePoints
        (this : access ChannelControl;
         numpoints : access unsigned;
         point_dspclock : access Extensions.unsigned_long_long;
         point_volume : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:306
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl13getFadePointsEPjPyPf";

      function getDSP
        (this : access ChannelControl;
         index : int;
         the_dsp : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:309
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl6getDSPEiPPNS_3DSPE";

      function addDSP
        (this : access ChannelControl;
         index : int;
         the_dsp : access DSP) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:310
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl6addDSPEiPNS_3DSPE";

      function removeDSP (this : access ChannelControl; the_dsp : access DSP) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:311
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl9removeDSPEPNS_3DSPE";

      function getNumDSPs (this : access ChannelControl; numdsps : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:312
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl10getNumDSPsEPi";

      function setDSPIndex
        (this : access ChannelControl;
         the_dsp : access DSP;
         index : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:313
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl11setDSPIndexEPNS_3DSPEi";

      function getDSPIndex
        (this : access ChannelControl;
         the_dsp : access DSP;
         index : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:314
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl11getDSPIndexEPNS_3DSPEPi";

      function set3DAttributes
        (this : access ChannelControl;
         pos : access constant fmod_common_h.FMOD_VECTOR;
         vel : access constant fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:317
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl15set3DAttributesEPK11FMOD_VECTORS3_";

      function get3DAttributes
        (this : access ChannelControl;
         pos : access fmod_common_h.FMOD_VECTOR;
         vel : access fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:318
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl15get3DAttributesEP11FMOD_VECTORS2_";

      function set3DMinMaxDistance
        (this : access ChannelControl;
         mindistance : float;
         maxdistance : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:319
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl19set3DMinMaxDistanceEff";

      function get3DMinMaxDistance
        (this : access ChannelControl;
         mindistance : access float;
         maxdistance : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:320
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl19get3DMinMaxDistanceEPfS1_";

      function set3DConeSettings
        (this : access ChannelControl;
         insideconeangle : float;
         outsideconeangle : float;
         outsidevolume : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:321
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl17set3DConeSettingsEfff";

      function get3DConeSettings
        (this : access ChannelControl;
         insideconeangle : access float;
         outsideconeangle : access float;
         outsidevolume : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:322
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl17get3DConeSettingsEPfS1_S1_";

      function set3DConeOrientation (this : access ChannelControl; orientation : access fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:323
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl20set3DConeOrientationEP11FMOD_VECTOR";

      function get3DConeOrientation (this : access ChannelControl; orientation : access fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:324
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl20get3DConeOrientationEP11FMOD_VECTOR";

      function set3DCustomRolloff
        (this : access ChannelControl;
         points : access fmod_common_h.FMOD_VECTOR;
         numpoints : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:325
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl18set3DCustomRolloffEP11FMOD_VECTORi";

      function get3DCustomRolloff
        (this : access ChannelControl;
         points : System.Address;
         numpoints : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:326
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl18get3DCustomRolloffEPP11FMOD_VECTORPi";

      function set3DOcclusion
        (this : access ChannelControl;
         directocclusion : float;
         reverbocclusion : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:327
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl14set3DOcclusionEff";

      function get3DOcclusion
        (this : access ChannelControl;
         directocclusion : access float;
         reverbocclusion : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:328
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl14get3DOcclusionEPfS1_";

      function set3DSpread (this : access ChannelControl; angle : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:329
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl11set3DSpreadEf";

      function get3DSpread (this : access ChannelControl; angle : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:330
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl11get3DSpreadEPf";

      function set3DLevel (this : access ChannelControl; level : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:331
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl10set3DLevelEf";

      function get3DLevel (this : access ChannelControl; level : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:332
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl10get3DLevelEPf";

      function set3DDopplerLevel (this : access ChannelControl; level : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:333
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl17set3DDopplerLevelEf";

      function get3DDopplerLevel (this : access ChannelControl; level : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:334
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl17get3DDopplerLevelEPf";

      function set3DDistanceFilter
        (this : access ChannelControl;
         custom : Extensions.bool;
         customLevel : float;
         centerFreq : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:335
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl19set3DDistanceFilterEbff";

      function get3DDistanceFilter
        (this : access ChannelControl;
         custom : access Extensions.bool;
         customLevel : access float;
         centerFreq : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:336
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl19get3DDistanceFilterEPbPfS2_";

      function setUserData (this : access ChannelControl; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:339
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl11setUserDataEPv";

      function getUserData (this : access ChannelControl; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:340
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD14ChannelControl11getUserDataEPPv";
   end;
   use Class_ChannelControl;
  -- General control functionality for Channels and ChannelGroups.
  -- Panning and level adjustment.
  -- Note all 'set' functions alter a final matrix, this is why the only get function is getMixMatrix, to avoid other get functions returning incorrect/obsolete values.
  -- Clock based functionality.
  -- DSP effects.
  -- 3D functionality.
  -- Userdata set/get.
  --        'Channel' API.
  --     

  -- Constructor made private so user cannot statically instance a Channel class.  Appropriate Channel creation or retrieval function must be used.
   package Class_Channel is
      type Channel is limited record
         null;
      end record
      with Import => True,
           Convention => CPP;

      function New_Channel return Channel;  -- fmod.hpp:351
      pragma CPP_Constructor (New_Channel, "_ZN4FMOD7ChannelC1Ev");

      function setFrequency (this : access Channel; frequency : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:357
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD7Channel12setFrequencyEf";

      function getFrequency (this : access Channel; frequency : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:358
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD7Channel12getFrequencyEPf";

      function setPriority (this : access Channel; priority : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:359
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD7Channel11setPriorityEi";

      function getPriority (this : access Channel; priority : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:360
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD7Channel11getPriorityEPi";

      function setPosition
        (this : access Channel;
         position : unsigned;
         postype : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:361
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD7Channel11setPositionEjj";

      function getPosition
        (this : access Channel;
         position : access unsigned;
         postype : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:362
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD7Channel11getPositionEPjj";

      function setChannelGroup (this : access Channel; the_channelgroup : access ChannelGroup) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:363
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD7Channel15setChannelGroupEPNS_12ChannelGroupE";

      function getChannelGroup (this : access Channel; the_channelgroup : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:364
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD7Channel15getChannelGroupEPPNS_12ChannelGroupE";

      function setLoopCount (this : access Channel; loopcount : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:365
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD7Channel12setLoopCountEi";

      function getLoopCount (this : access Channel; loopcount : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:366
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD7Channel12getLoopCountEPi";

      function setLoopPoints
        (this : access Channel;
         loopstart : unsigned;
         loopstarttype : fmod_common_h.FMOD_TIMEUNIT;
         loopend : unsigned;
         loopendtype : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:367
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD7Channel13setLoopPointsEjjjj";

      function getLoopPoints
        (this : access Channel;
         loopstart : access unsigned;
         loopstarttype : fmod_common_h.FMOD_TIMEUNIT;
         loopend : access unsigned;
         loopendtype : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:368
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD7Channel13getLoopPointsEPjjS1_j";

      function isVirtual (this : access Channel; isvirtual : access Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:371
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD7Channel9isVirtualEPb";

      function getCurrentSound (this : access Channel; the_sound : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:372
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD7Channel15getCurrentSoundEPPNS_5SoundE";

      function getIndex (this : access Channel; index : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:373
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD7Channel8getIndexEPi";
   end;
   use Class_Channel;
  -- Channel specific control functionality.
  -- Information only functions.
  --        'ChannelGroup' API
  --     

  -- Constructor made private so user cannot statically instance a ChannelGroup class.  Appropriate ChannelGroup creation or retrieval function must be used.
   package Class_ChannelGroup is
      type ChannelGroup is limited record
         null;
      end record
      with Import => True,
           Convention => CPP;

      function New_ChannelGroup return ChannelGroup;  -- fmod.hpp:384
      pragma CPP_Constructor (New_ChannelGroup, "_ZN4FMOD12ChannelGroupC1Ev");

      function release (this : access ChannelGroup) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:389
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD12ChannelGroup7releaseEv";

      function addGroup
        (this : access ChannelGroup;
         group : access ChannelGroup;
         propagatedspclock : Extensions.bool;
         connection : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:392
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD12ChannelGroup8addGroupEPS0_bPPNS_13DSPConnectionE";

      function getNumGroups (this : access ChannelGroup; numgroups : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:393
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD12ChannelGroup12getNumGroupsEPi";

      function getGroup
        (this : access ChannelGroup;
         index : int;
         group : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:394
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD12ChannelGroup8getGroupEiPPS0_";

      function getParentGroup (this : access ChannelGroup; group : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:395
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD12ChannelGroup14getParentGroupEPPS0_";

      function getName
        (this : access ChannelGroup;
         name : Interfaces.C.Strings.chars_ptr;
         namelen : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:398
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD12ChannelGroup7getNameEPci";

      function getNumChannels (this : access ChannelGroup; numchannels : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:399
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD12ChannelGroup14getNumChannelsEPi";

      function getChannel
        (this : access ChannelGroup;
         index : int;
         the_channel : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:400
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD12ChannelGroup10getChannelEiPPNS_7ChannelE";
   end;
   use Class_ChannelGroup;
  -- Nested channel groups.
  -- Information only functions.
  --        'SoundGroup' API
  --     

  -- Constructor made private so user cannot statically instance a SoundGroup class.  Appropriate SoundGroup creation or retrieval function must be used.
   package Class_SoundGroup is
      type SoundGroup is limited record
         null;
      end record
      with Import => True,
           Convention => CPP;

      function New_SoundGroup return SoundGroup;  -- fmod.hpp:411
      pragma CPP_Constructor (New_SoundGroup, "_ZN4FMOD10SoundGroupC1Ev");

      function release (this : access SoundGroup) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:416
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD10SoundGroup7releaseEv";

      function getSystemObject (this : access SoundGroup; the_c_system : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:417
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD10SoundGroup15getSystemObjectEPPNS_6SystemE";

      function setMaxAudible (this : access SoundGroup; maxaudible : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:420
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD10SoundGroup13setMaxAudibleEi";

      function getMaxAudible (this : access SoundGroup; maxaudible : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:421
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD10SoundGroup13getMaxAudibleEPi";

      function setMaxAudibleBehavior (this : access SoundGroup; behavior : fmod_common_h.FMOD_SOUNDGROUP_BEHAVIOR) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:422
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD10SoundGroup21setMaxAudibleBehaviorE24FMOD_SOUNDGROUP_BEHAVIOR";

      function getMaxAudibleBehavior (this : access SoundGroup; behavior : access fmod_common_h.FMOD_SOUNDGROUP_BEHAVIOR) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:423
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD10SoundGroup21getMaxAudibleBehaviorEP24FMOD_SOUNDGROUP_BEHAVIOR";

      function setMuteFadeSpeed (this : access SoundGroup; speed : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:424
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD10SoundGroup16setMuteFadeSpeedEf";

      function getMuteFadeSpeed (this : access SoundGroup; speed : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:425
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD10SoundGroup16getMuteFadeSpeedEPf";

      function setVolume (this : access SoundGroup; volume : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:426
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD10SoundGroup9setVolumeEf";

      function getVolume (this : access SoundGroup; volume : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:427
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD10SoundGroup9getVolumeEPf";

      function stop (this : access SoundGroup) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:428
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD10SoundGroup4stopEv";

      function getName
        (this : access SoundGroup;
         name : Interfaces.C.Strings.chars_ptr;
         namelen : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:431
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD10SoundGroup7getNameEPci";

      function getNumSounds (this : access SoundGroup; numsounds : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:432
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD10SoundGroup12getNumSoundsEPi";

      function getSound
        (this : access SoundGroup;
         index : int;
         the_sound : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:433
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD10SoundGroup8getSoundEiPPNS_5SoundE";

      function getNumPlaying (this : access SoundGroup; numplaying : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:434
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD10SoundGroup13getNumPlayingEPi";

      function setUserData (this : access SoundGroup; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:437
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD10SoundGroup11setUserDataEPv";

      function getUserData (this : access SoundGroup; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:438
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD10SoundGroup11getUserDataEPPv";
   end;
   use Class_SoundGroup;
  -- SoundGroup control functions.
  -- Information only functions.
  -- Userdata set/get.
  --        'DSP' API
  --     

  -- Constructor made private so user cannot statically instance a DSP class.  Appropriate DSP creation or retrieval function must be used.
   package Class_DSP is
      type DSP is limited record
         null;
      end record
      with Import => True,
           Convention => CPP;

      function New_DSP return DSP;  -- fmod.hpp:449
      pragma CPP_Constructor (New_DSP, "_ZN4FMOD3DSPC1Ev");

      function release (this : access DSP) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:454
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP7releaseEv";

      function getSystemObject (this : access DSP; the_c_system : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:455
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP15getSystemObjectEPPNS_6SystemE";

      function addInput
        (this : access DSP;
         input : access DSP;
         connection : System.Address;
         c_type : fmod_common_h.FMOD_DSPCONNECTION_TYPE) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:458
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP8addInputEPS0_PPNS_13DSPConnectionE23FMOD_DSPCONNECTION_TYPE";

      function disconnectFrom
        (this : access DSP;
         target : access DSP;
         connection : access DSPConnection) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:459
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP14disconnectFromEPS0_PNS_13DSPConnectionE";

      function disconnectAll
        (this : access DSP;
         inputs : Extensions.bool;
         outputs : Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:460
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP13disconnectAllEbb";

      function getNumInputs (this : access DSP; numinputs : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:461
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP12getNumInputsEPi";

      function getNumOutputs (this : access DSP; numoutputs : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:462
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP13getNumOutputsEPi";

      function getInput
        (this : access DSP;
         index : int;
         input : System.Address;
         inputconnection : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:463
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP8getInputEiPPS0_PPNS_13DSPConnectionE";

      function getOutput
        (this : access DSP;
         index : int;
         output : System.Address;
         outputconnection : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:464
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP9getOutputEiPPS0_PPNS_13DSPConnectionE";

      function setActive (this : access DSP; active : Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:467
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP9setActiveEb";

      function getActive (this : access DSP; active : access Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:468
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP9getActiveEPb";

      function setBypass (this : access DSP; bypass : Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:469
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP9setBypassEb";

      function getBypass (this : access DSP; bypass : access Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:470
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP9getBypassEPb";

      function setWetDryMix
        (this : access DSP;
         prewet : float;
         postwet : float;
         dry : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:471
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP12setWetDryMixEfff";

      function getWetDryMix
        (this : access DSP;
         prewet : access float;
         postwet : access float;
         dry : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:472
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP12getWetDryMixEPfS1_S1_";

      function setChannelFormat
        (this : access DSP;
         channelmask : fmod_common_h.FMOD_CHANNELMASK;
         numchannels : int;
         source_speakermode : fmod_common_h.FMOD_SPEAKERMODE) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:473
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP16setChannelFormatEji16FMOD_SPEAKERMODE";

      function getChannelFormat
        (this : access DSP;
         channelmask : access fmod_common_h.FMOD_CHANNELMASK;
         numchannels : access int;
         source_speakermode : access fmod_common_h.FMOD_SPEAKERMODE) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:474
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP16getChannelFormatEPjPiP16FMOD_SPEAKERMODE";

      function getOutputChannelFormat
        (this : access DSP;
         inmask : fmod_common_h.FMOD_CHANNELMASK;
         inchannels : int;
         inspeakermode : fmod_common_h.FMOD_SPEAKERMODE;
         outmask : access fmod_common_h.FMOD_CHANNELMASK;
         outchannels : access int;
         outspeakermode : access fmod_common_h.FMOD_SPEAKERMODE) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:475
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP22getOutputChannelFormatEji16FMOD_SPEAKERMODEPjPiPS1_";

      function reset (this : access DSP) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:476
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP5resetEv";

      function setParameterFloat
        (this : access DSP;
         index : int;
         value : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:479
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP17setParameterFloatEif";

      function setParameterInt
        (this : access DSP;
         index : int;
         value : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:480
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP15setParameterIntEii";

      function setParameterBool
        (this : access DSP;
         index : int;
         value : Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:481
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP16setParameterBoolEib";

      function setParameterData
        (this : access DSP;
         index : int;
         data : System.Address;
         length : unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:482
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP16setParameterDataEiPvj";

      function getParameterFloat
        (this : access DSP;
         index : int;
         value : access float;
         valuestr : Interfaces.C.Strings.chars_ptr;
         valuestrlen : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:483
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP17getParameterFloatEiPfPci";

      function getParameterInt
        (this : access DSP;
         index : int;
         value : access int;
         valuestr : Interfaces.C.Strings.chars_ptr;
         valuestrlen : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:484
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP15getParameterIntEiPiPci";

      function getParameterBool
        (this : access DSP;
         index : int;
         value : access Extensions.bool;
         valuestr : Interfaces.C.Strings.chars_ptr;
         valuestrlen : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:485
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP16getParameterBoolEiPbPci";

      function getParameterData
        (this : access DSP;
         index : int;
         data : System.Address;
         length : access unsigned;
         valuestr : Interfaces.C.Strings.chars_ptr;
         valuestrlen : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:486
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP16getParameterDataEiPPvPjPci";

      function getNumParameters (this : access DSP; numparams : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:487
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP16getNumParametersEPi";

      function getParameterInfo
        (this : access DSP;
         index : int;
         desc : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:488
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP16getParameterInfoEiPP23FMOD_DSP_PARAMETER_DESC";

      function getDataParameterIndex
        (this : access DSP;
         datatype : int;
         index : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:489
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP21getDataParameterIndexEiPi";

      function showConfigDialog
        (this : access DSP;
         hwnd : System.Address;
         show : Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:490
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP16showConfigDialogEPvb";

      function getInfo
        (this : access DSP;
         name : Interfaces.C.Strings.chars_ptr;
         version : access unsigned;
         channels : access int;
         configwidth : access int;
         configheight : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:493
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP7getInfoEPcPjPiS3_S3_";

      function getType (this : access DSP; c_type : access fmod_dsp_effects_h.FMOD_DSP_TYPE) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:494
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP7getTypeEP13FMOD_DSP_TYPE";

      function getIdle (this : access DSP; idle : access Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:495
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP7getIdleEPb";

      function setUserData (this : access DSP; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:498
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP11setUserDataEPv";

      function getUserData (this : access DSP; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:499
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP11getUserDataEPPv";

      function setMeteringEnabled
        (this : access DSP;
         inputEnabled : Extensions.bool;
         outputEnabled : Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:502
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP18setMeteringEnabledEbb";

      function getMeteringEnabled
        (this : access DSP;
         inputEnabled : access Extensions.bool;
         outputEnabled : access Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:503
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP18getMeteringEnabledEPbS1_";

      function getMeteringInfo
        (this : access DSP;
         inputInfo : access fmod_dsp_h.FMOD_DSP_METERING_INFO;
         outputInfo : access fmod_dsp_h.FMOD_DSP_METERING_INFO) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:504
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP15getMeteringInfoEP22FMOD_DSP_METERING_INFOS2_";

      function getCPUUsage
        (this : access DSP;
         exclusive : access unsigned;
         inclusive : access unsigned) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:505
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD3DSP11getCPUUsageEPjS1_";
   end;
   use Class_DSP;
  -- Connection / disconnection / input and output enumeration.
  -- DSP unit control.
  -- DSP parameter control.
  -- DSP attributes.
  -- Userdata set/get.
  -- Metering.
  --        'DSPConnection' API
  --     

  -- Constructor made private so user cannot statically instance a DSPConnection class.  Appropriate DSPConnection creation or retrieval function must be used.
   package Class_DSPConnection is
      type DSPConnection is limited record
         null;
      end record
      with Import => True,
           Convention => CPP;

      function New_DSPConnection return DSPConnection;  -- fmod.hpp:517
      pragma CPP_Constructor (New_DSPConnection, "_ZN4FMOD13DSPConnectionC1Ev");

      function getInput (this : access DSPConnection; input : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:522
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD13DSPConnection8getInputEPPNS_3DSPE";

      function getOutput (this : access DSPConnection; output : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:523
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD13DSPConnection9getOutputEPPNS_3DSPE";

      function setMix (this : access DSPConnection; volume : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:524
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD13DSPConnection6setMixEf";

      function getMix (this : access DSPConnection; volume : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:525
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD13DSPConnection6getMixEPf";

      function setMixMatrix
        (this : access DSPConnection;
         matrix : access float;
         outchannels : int;
         inchannels : int;
         inchannel_hop : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:526
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD13DSPConnection12setMixMatrixEPfiii";

      function getMixMatrix
        (this : access DSPConnection;
         matrix : access float;
         outchannels : access int;
         inchannels : access int;
         inchannel_hop : int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:527
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD13DSPConnection12getMixMatrixEPfPiS2_i";

      function getType (this : access DSPConnection; c_type : access fmod_common_h.FMOD_DSPCONNECTION_TYPE) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:528
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD13DSPConnection7getTypeEP23FMOD_DSPCONNECTION_TYPE";

      function setUserData (this : access DSPConnection; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:531
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD13DSPConnection11setUserDataEPv";

      function getUserData (this : access DSPConnection; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:532
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD13DSPConnection11getUserDataEPPv";
   end;
   use Class_DSPConnection;
  -- Userdata set/get.
  --        'Geometry' API
  --     

  -- Constructor made private so user cannot statically instance a Geometry class.  Appropriate Geometry creation or retrieval function must be used.
   package Class_Geometry is
      type Geometry is limited record
         null;
      end record
      with Import => True,
           Convention => CPP;

      function New_Geometry return Geometry;  -- fmod.hpp:544
      pragma CPP_Constructor (New_Geometry, "_ZN4FMOD8GeometryC1Ev");

      function release (this : access Geometry) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:549
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Geometry7releaseEv";

      function addPolygon
        (this : access Geometry;
         directocclusion : float;
         reverbocclusion : float;
         doublesided : Extensions.bool;
         numvertices : int;
         vertices : access constant fmod_common_h.FMOD_VECTOR;
         polygonindex : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:552
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Geometry10addPolygonEffbiPK11FMOD_VECTORPi";

      function getNumPolygons (this : access Geometry; numpolygons : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:553
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Geometry14getNumPolygonsEPi";

      function getMaxPolygons
        (this : access Geometry;
         maxpolygons : access int;
         maxvertices : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:554
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Geometry14getMaxPolygonsEPiS1_";

      function getPolygonNumVertices
        (this : access Geometry;
         index : int;
         numvertices : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:555
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Geometry21getPolygonNumVerticesEiPi";

      function setPolygonVertex
        (this : access Geometry;
         index : int;
         vertexindex : int;
         vertex : access constant fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:556
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Geometry16setPolygonVertexEiiPK11FMOD_VECTOR";

      function getPolygonVertex
        (this : access Geometry;
         index : int;
         vertexindex : int;
         vertex : access fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:557
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Geometry16getPolygonVertexEiiP11FMOD_VECTOR";

      function setPolygonAttributes
        (this : access Geometry;
         index : int;
         directocclusion : float;
         reverbocclusion : float;
         doublesided : Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:558
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Geometry20setPolygonAttributesEiffb";

      function getPolygonAttributes
        (this : access Geometry;
         index : int;
         directocclusion : access float;
         reverbocclusion : access float;
         doublesided : access Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:559
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Geometry20getPolygonAttributesEiPfS1_Pb";

      function setActive (this : access Geometry; active : Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:562
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Geometry9setActiveEb";

      function getActive (this : access Geometry; active : access Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:563
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Geometry9getActiveEPb";

      function setRotation
        (this : access Geometry;
         forward : access constant fmod_common_h.FMOD_VECTOR;
         up : access constant fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:564
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Geometry11setRotationEPK11FMOD_VECTORS3_";

      function getRotation
        (this : access Geometry;
         forward : access fmod_common_h.FMOD_VECTOR;
         up : access fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:565
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Geometry11getRotationEP11FMOD_VECTORS2_";

      function setPosition (this : access Geometry; position : access constant fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:566
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Geometry11setPositionEPK11FMOD_VECTOR";

      function getPosition (this : access Geometry; position : access fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:567
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Geometry11getPositionEP11FMOD_VECTOR";

      function setScale (this : access Geometry; scale : access constant fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:568
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Geometry8setScaleEPK11FMOD_VECTOR";

      function getScale (this : access Geometry; scale : access fmod_common_h.FMOD_VECTOR) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:569
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Geometry8getScaleEP11FMOD_VECTOR";

      function save
        (this : access Geometry;
         data : System.Address;
         datasize : access int) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:570
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Geometry4saveEPvPi";

      function setUserData (this : access Geometry; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:573
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Geometry11setUserDataEPv";

      function getUserData (this : access Geometry; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:574
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Geometry11getUserDataEPPv";
   end;
   use Class_Geometry;
  -- Polygon manipulation.
  -- Object manipulation.
  -- Userdata set/get.
  --        'Reverb' API
  --     

  -- Constructor made private so user cannot statically instance a Reverb3D class.  Appropriate Reverb creation or retrieval function must be used.
   package Class_Reverb3D is
      type Reverb3D is limited record
         null;
      end record
      with Import => True,
           Convention => CPP;

      function New_Reverb3D return Reverb3D;  -- fmod.hpp:586
      pragma CPP_Constructor (New_Reverb3D, "_ZN4FMOD8Reverb3DC1Ev");

      function release (this : access Reverb3D) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:591
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Reverb3D7releaseEv";

      function set3DAttributes
        (this : access Reverb3D;
         position : access constant fmod_common_h.FMOD_VECTOR;
         mindistance : float;
         maxdistance : float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:594
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Reverb3D15set3DAttributesEPK11FMOD_VECTORff";

      function get3DAttributes
        (this : access Reverb3D;
         position : access fmod_common_h.FMOD_VECTOR;
         mindistance : access float;
         maxdistance : access float) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:595
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Reverb3D15get3DAttributesEP11FMOD_VECTORPfS3_";

      function setProperties (this : access Reverb3D; properties : access constant fmod_common_h.FMOD_REVERB_PROPERTIES) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:596
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Reverb3D13setPropertiesEPK22FMOD_REVERB_PROPERTIES";

      function getProperties (this : access Reverb3D; properties : access fmod_common_h.FMOD_REVERB_PROPERTIES) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:597
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Reverb3D13getPropertiesEP22FMOD_REVERB_PROPERTIES";

      function setActive (this : access Reverb3D; active : Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:598
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Reverb3D9setActiveEb";

      function getActive (this : access Reverb3D; active : access Extensions.bool) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:599
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Reverb3D9getActiveEPb";

      function setUserData (this : access Reverb3D; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:602
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Reverb3D11setUserDataEPv";

      function getUserData (this : access Reverb3D; userdata : System.Address) return fmod_common_h.FMOD_RESULT  -- fmod.hpp:603
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN4FMOD8Reverb3D11getUserDataEPPv";
   end;
   use Class_Reverb3D;
  -- Reverb manipulation.
  -- Userdata set/get.
end fmod_hpp;
