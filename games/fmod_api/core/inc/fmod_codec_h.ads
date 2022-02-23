pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with fmod_common_h;
with System;
with Interfaces.C.Strings;

package fmod_codec_h is

   FMOD_CODEC_WAVEFORMAT_VERSION : constant := 3;  --  fmod_codec.h:25

  -- ========================================================================================  
  -- FMOD Core API - Codec development header file.                                            
  -- Copyright (c), Firelight Technologies Pty, Ltd. 2004-2021.                                
  --                                                                                           
  -- Use this header if you are wanting to develop your own file format plugin to use with     
  -- FMOD's codec system.  With this header you can make your own fileformat plugin that FMOD  
  -- can register and use.  See the documentation and examples on how to make a working        
  -- plugin.                                                                                   
  --                                                                                           
  -- For more detail visit:                                                                    
  -- https://fmod.com/resources/documentation-api?version=2.0&page=core-api.html               
  -- ========================================================================================  
  --    Codec types
  -- 

   type FMOD_CODEC_STATE;
   type FMOD_CODEC_WAVEFORMAT;
  --    Codec constants
  -- 

  --    Codec callbacks
  -- 

   type FMOD_CODEC_OPEN_CALLBACK is access function
        (arg1 : access FMOD_CODEC_STATE;
         arg2 : fmod_common_h.FMOD_MODE;
         arg3 : access fmod_common_h.FMOD_CREATESOUNDEXINFO) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_codec.h:30

   type FMOD_CODEC_CLOSE_CALLBACK is access function (arg1 : access FMOD_CODEC_STATE) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_codec.h:31

   type FMOD_CODEC_READ_CALLBACK is access function
        (arg1 : access FMOD_CODEC_STATE;
         arg2 : System.Address;
         arg3 : unsigned;
         arg4 : access unsigned) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_codec.h:32

   type FMOD_CODEC_GETLENGTH_CALLBACK is access function
        (arg1 : access FMOD_CODEC_STATE;
         arg2 : access unsigned;
         arg3 : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_codec.h:33

   type FMOD_CODEC_SETPOSITION_CALLBACK is access function
        (arg1 : access FMOD_CODEC_STATE;
         arg2 : int;
         arg3 : unsigned;
         arg4 : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_codec.h:34

   type FMOD_CODEC_GETPOSITION_CALLBACK is access function
        (arg1 : access FMOD_CODEC_STATE;
         arg2 : access unsigned;
         arg3 : fmod_common_h.FMOD_TIMEUNIT) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_codec.h:35

   type FMOD_CODEC_SOUNDCREATE_CALLBACK is access function
        (arg1 : access FMOD_CODEC_STATE;
         arg2 : int;
         arg3 : access fmod_common_h.FMOD_SOUND) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_codec.h:36

   type FMOD_CODEC_GETWAVEFORMAT_CALLBACK is access function
        (arg1 : access FMOD_CODEC_STATE;
         arg2 : int;
         arg3 : access FMOD_CODEC_WAVEFORMAT) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_codec.h:37

  --    Codec functions
  -- 

   type FMOD_CODEC_METADATA_FUNC is access function
        (arg1 : access FMOD_CODEC_STATE;
         arg2 : fmod_common_h.FMOD_TAGTYPE;
         arg3 : Interfaces.C.Strings.chars_ptr;
         arg4 : System.Address;
         arg5 : unsigned;
         arg6 : fmod_common_h.FMOD_TAGDATATYPE;
         arg7 : int) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_codec.h:42

  --    Codec structures
  -- 

   type FMOD_CODEC_DESCRIPTION is record
      name : Interfaces.C.Strings.chars_ptr;  -- fmod_codec.h:49
      version : aliased unsigned;  -- fmod_codec.h:50
      defaultasstream : aliased int;  -- fmod_codec.h:51
      timeunits : aliased fmod_common_h.FMOD_TIMEUNIT;  -- fmod_codec.h:52
      open : FMOD_CODEC_OPEN_CALLBACK;  -- fmod_codec.h:53
      close : FMOD_CODEC_CLOSE_CALLBACK;  -- fmod_codec.h:54
      read : FMOD_CODEC_READ_CALLBACK;  -- fmod_codec.h:55
      getlength : FMOD_CODEC_GETLENGTH_CALLBACK;  -- fmod_codec.h:56
      setposition : FMOD_CODEC_SETPOSITION_CALLBACK;  -- fmod_codec.h:57
      getposition : FMOD_CODEC_GETPOSITION_CALLBACK;  -- fmod_codec.h:58
      soundcreate : FMOD_CODEC_SOUNDCREATE_CALLBACK;  -- fmod_codec.h:59
      getwaveformat : FMOD_CODEC_GETWAVEFORMAT_CALLBACK;  -- fmod_codec.h:60
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_codec.h:47

   type FMOD_CODEC_WAVEFORMAT is record
      name : Interfaces.C.Strings.chars_ptr;  -- fmod_codec.h:65
      format : aliased fmod_common_h.FMOD_SOUND_FORMAT;  -- fmod_codec.h:66
      channels : aliased int;  -- fmod_codec.h:67
      frequency : aliased int;  -- fmod_codec.h:68
      lengthbytes : aliased unsigned;  -- fmod_codec.h:69
      lengthpcm : aliased unsigned;  -- fmod_codec.h:70
      pcmblocksize : aliased unsigned;  -- fmod_codec.h:71
      loopstart : aliased int;  -- fmod_codec.h:72
      loopend : aliased int;  -- fmod_codec.h:73
      mode : aliased fmod_common_h.FMOD_MODE;  -- fmod_codec.h:74
      channelmask : aliased fmod_common_h.FMOD_CHANNELMASK;  -- fmod_codec.h:75
      channelorder : aliased fmod_common_h.FMOD_CHANNELORDER;  -- fmod_codec.h:76
      peakvolume : aliased float;  -- fmod_codec.h:77
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_codec.h:63

   type FMOD_CODEC_STATE is record
      numsubsounds : aliased int;  -- fmod_codec.h:82
      waveformat : access FMOD_CODEC_WAVEFORMAT;  -- fmod_codec.h:83
      plugindata : System.Address;  -- fmod_codec.h:84
      filehandle : System.Address;  -- fmod_codec.h:86
      filesize : aliased unsigned;  -- fmod_codec.h:87
      fileread : fmod_common_h.FMOD_FILE_READ_CALLBACK;  -- fmod_codec.h:88
      fileseek : fmod_common_h.FMOD_FILE_SEEK_CALLBACK;  -- fmod_codec.h:89
      metadata : FMOD_CODEC_METADATA_FUNC;  -- fmod_codec.h:90
      waveformatversion : aliased int;  -- fmod_codec.h:92
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_codec.h:80

end fmod_codec_h;
