pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with fmod_common_h;
with Interfaces.C.Strings;
with System;

package fmod_output_h is

   FMOD_OUTPUT_PLUGIN_VERSION : constant := 4;  --  fmod_output.h:22

   FMOD_OUTPUT_METHOD_MIX_DIRECT : constant := 0;  --  fmod_output.h:25
   FMOD_OUTPUT_METHOD_POLLING : constant := 1;  --  fmod_output.h:26
   FMOD_OUTPUT_METHOD_MIX_BUFFERED : constant := 2;  --  fmod_output.h:27
   --  arg-macro: function FMOD_OUTPUT_READFROMMIXER (_state, _buffer, _length)
   --    return _state).readfrommixer(_state, _buffer, _length;
   --  arg-macro: function FMOD_OUTPUT_ALLOC (_state, _size, _align)
   --    return _state).alloc(_size, _align, __FILE__, __LINE__;
   --  arg-macro: function FMOD_OUTPUT_FREE (_state, _ptr)
   --    return _state).free(_ptr, __FILE__, __LINE__;
   --  unsupported macro: FMOD_OUTPUT_LOG(_state,_level,_location,_format,...) (_state)->log(_level, __FILE__, __LINE__, _location, _format, __VA_ARGS__)
   --  arg-macro: function FMOD_OUTPUT_COPYPORT (_state, _id, _buffer, _length)
   --    return _state).copyport(_state, _id, _buffer, _length;
   --  arg-macro: function FMOD_OUTPUT_REQUESTRESET (_state)
   --    return _state).requestreset(_state;

  -- ========================================================================================  
  -- FMOD Core API - output development header file.                                           
  -- Copyright (c), Firelight Technologies Pty, Ltd. 2004-2021.                                
  --                                                                                           
  -- Use this header if you are wanting to develop your own output plugin to use with          
  -- FMOD's output system.  With this header you can make your own output plugin that FMOD     
  -- can register and use.  See the documentation and examples on how to make a working        
  -- plugin.                                                                                   
  --                                                                                           
  -- For more detail visit:                                                                    
  -- https://fmod.com/resources/documentation-api?version=2.0&page=plugin-api-output.html      
  -- ========================================================================================  
   type FMOD_OUTPUT_STATE;
   type FMOD_OUTPUT_OBJECT3DINFO;
  --    Output constants
  -- 

   subtype FMOD_OUTPUT_METHOD is unsigned;  -- fmod_output.h:24

  --    Output callbacks
  -- 

   type FMOD_OUTPUT_GETNUMDRIVERS_CALLBACK is access function (arg1 : access FMOD_OUTPUT_STATE; arg2 : access int) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:32

   type FMOD_OUTPUT_GETDRIVERINFO_CALLBACK is access function
        (arg1 : access FMOD_OUTPUT_STATE;
         arg2 : int;
         arg3 : Interfaces.C.Strings.chars_ptr;
         arg4 : int;
         arg5 : access fmod_common_h.FMOD_GUID;
         arg6 : access int;
         arg7 : access fmod_common_h.FMOD_SPEAKERMODE;
         arg8 : access int) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:33

   type FMOD_OUTPUT_INIT_CALLBACK is access function
        (arg1 : access FMOD_OUTPUT_STATE;
         arg2 : int;
         arg3 : fmod_common_h.FMOD_INITFLAGS;
         arg4 : access int;
         arg5 : access fmod_common_h.FMOD_SPEAKERMODE;
         arg6 : access int;
         arg7 : access fmod_common_h.FMOD_SOUND_FORMAT;
         arg8 : int;
         arg9 : int;
         arg10 : System.Address) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:34

   type FMOD_OUTPUT_START_CALLBACK is access function (arg1 : access FMOD_OUTPUT_STATE) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:35

   type FMOD_OUTPUT_STOP_CALLBACK is access function (arg1 : access FMOD_OUTPUT_STATE) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:36

   type FMOD_OUTPUT_CLOSE_CALLBACK is access function (arg1 : access FMOD_OUTPUT_STATE) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:37

   type FMOD_OUTPUT_UPDATE_CALLBACK is access function (arg1 : access FMOD_OUTPUT_STATE) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:38

   type FMOD_OUTPUT_GETHANDLE_CALLBACK is access function (arg1 : access FMOD_OUTPUT_STATE; arg2 : System.Address) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:39

   type FMOD_OUTPUT_GETPOSITION_CALLBACK is access function (arg1 : access FMOD_OUTPUT_STATE; arg2 : access unsigned) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:40

   type FMOD_OUTPUT_LOCK_CALLBACK is access function
        (arg1 : access FMOD_OUTPUT_STATE;
         arg2 : unsigned;
         arg3 : unsigned;
         arg4 : System.Address;
         arg5 : System.Address;
         arg6 : access unsigned;
         arg7 : access unsigned) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:41

   type FMOD_OUTPUT_UNLOCK_CALLBACK is access function
        (arg1 : access FMOD_OUTPUT_STATE;
         arg2 : System.Address;
         arg3 : System.Address;
         arg4 : unsigned;
         arg5 : unsigned) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:42

   type FMOD_OUTPUT_MIXER_CALLBACK is access function (arg1 : access FMOD_OUTPUT_STATE) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:43

   type FMOD_OUTPUT_OBJECT3DGETINFO_CALLBACK is access function (arg1 : access FMOD_OUTPUT_STATE; arg2 : access int) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:44

   type FMOD_OUTPUT_OBJECT3DALLOC_CALLBACK is access function (arg1 : access FMOD_OUTPUT_STATE; arg2 : System.Address) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:45

   type FMOD_OUTPUT_OBJECT3DFREE_CALLBACK is access function (arg1 : access FMOD_OUTPUT_STATE; arg2 : System.Address) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:46

   type FMOD_OUTPUT_OBJECT3DUPDATE_CALLBACK is access function
        (arg1 : access FMOD_OUTPUT_STATE;
         arg2 : System.Address;
         arg3 : access constant FMOD_OUTPUT_OBJECT3DINFO) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:47

   type FMOD_OUTPUT_OPENPORT_CALLBACK is access function
        (arg1 : access FMOD_OUTPUT_STATE;
         arg2 : fmod_common_h.FMOD_PORT_TYPE;
         arg3 : fmod_common_h.FMOD_PORT_INDEX;
         arg4 : access int;
         arg5 : access int;
         arg6 : access int;
         arg7 : access fmod_common_h.FMOD_SOUND_FORMAT) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:48

   type FMOD_OUTPUT_CLOSEPORT_CALLBACK is access function (arg1 : access FMOD_OUTPUT_STATE; arg2 : int) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:49

   type FMOD_OUTPUT_DEVICELISTCHANGED_CALLBACK is access function (arg1 : access FMOD_OUTPUT_STATE) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:50

  --    Output functions
  -- 

   type FMOD_OUTPUT_READFROMMIXER_FUNC is access function
        (arg1 : access FMOD_OUTPUT_STATE;
         arg2 : System.Address;
         arg3 : unsigned) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:55

   type FMOD_OUTPUT_COPYPORT_FUNC is access function
        (arg1 : access FMOD_OUTPUT_STATE;
         arg2 : int;
         arg3 : System.Address;
         arg4 : unsigned) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:56

   type FMOD_OUTPUT_REQUESTRESET_FUNC is access function (arg1 : access FMOD_OUTPUT_STATE) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_output.h:57

   type FMOD_OUTPUT_ALLOC_FUNC is access function
        (arg1 : unsigned;
         arg2 : unsigned;
         arg3 : Interfaces.C.Strings.chars_ptr;
         arg4 : int) return System.Address
   with Convention => C;  -- fmod_output.h:58

   type FMOD_OUTPUT_FREE_FUNC is access procedure
        (arg1 : System.Address;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : int)
   with Convention => C;  -- fmod_output.h:59

   type FMOD_OUTPUT_LOG_FUNC is access procedure
        (arg1 : fmod_common_h.FMOD_DEBUG_FLAGS;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : int;
         arg4 : Interfaces.C.Strings.chars_ptr;
         arg5 : Interfaces.C.Strings.chars_ptr  -- , ...
         )
   with Convention => C;  -- fmod_output.h:60

  --    Output structures
  -- 

   type FMOD_OUTPUT_DESCRIPTION is record
      apiversion : aliased unsigned;  -- fmod_output.h:67
      name : Interfaces.C.Strings.chars_ptr;  -- fmod_output.h:68
      version : aliased unsigned;  -- fmod_output.h:69
      method : aliased FMOD_OUTPUT_METHOD;  -- fmod_output.h:70
      getnumdrivers : FMOD_OUTPUT_GETNUMDRIVERS_CALLBACK;  -- fmod_output.h:71
      getdriverinfo : FMOD_OUTPUT_GETDRIVERINFO_CALLBACK;  -- fmod_output.h:72
      init : FMOD_OUTPUT_INIT_CALLBACK;  -- fmod_output.h:73
      start : FMOD_OUTPUT_START_CALLBACK;  -- fmod_output.h:74
      stop : FMOD_OUTPUT_STOP_CALLBACK;  -- fmod_output.h:75
      close : FMOD_OUTPUT_CLOSE_CALLBACK;  -- fmod_output.h:76
      update : FMOD_OUTPUT_UPDATE_CALLBACK;  -- fmod_output.h:77
      gethandle : FMOD_OUTPUT_GETHANDLE_CALLBACK;  -- fmod_output.h:78
      getposition : FMOD_OUTPUT_GETPOSITION_CALLBACK;  -- fmod_output.h:79
      lock : FMOD_OUTPUT_LOCK_CALLBACK;  -- fmod_output.h:80
      unlock : FMOD_OUTPUT_UNLOCK_CALLBACK;  -- fmod_output.h:81
      mixer : FMOD_OUTPUT_MIXER_CALLBACK;  -- fmod_output.h:82
      object3dgetinfo : FMOD_OUTPUT_OBJECT3DGETINFO_CALLBACK;  -- fmod_output.h:83
      object3dalloc : FMOD_OUTPUT_OBJECT3DALLOC_CALLBACK;  -- fmod_output.h:84
      object3dfree : FMOD_OUTPUT_OBJECT3DFREE_CALLBACK;  -- fmod_output.h:85
      object3dupdate : FMOD_OUTPUT_OBJECT3DUPDATE_CALLBACK;  -- fmod_output.h:86
      openport : FMOD_OUTPUT_OPENPORT_CALLBACK;  -- fmod_output.h:87
      closeport : FMOD_OUTPUT_CLOSEPORT_CALLBACK;  -- fmod_output.h:88
      devicelistchanged : FMOD_OUTPUT_DEVICELISTCHANGED_CALLBACK;  -- fmod_output.h:89
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_output.h:65

   type FMOD_OUTPUT_STATE is record
      plugindata : System.Address;  -- fmod_output.h:94
      readfrommixer : FMOD_OUTPUT_READFROMMIXER_FUNC;  -- fmod_output.h:95
      alloc : FMOD_OUTPUT_ALLOC_FUNC;  -- fmod_output.h:96
      free : FMOD_OUTPUT_FREE_FUNC;  -- fmod_output.h:97
      log : FMOD_OUTPUT_LOG_FUNC;  -- fmod_output.h:98
      copyport : FMOD_OUTPUT_COPYPORT_FUNC;  -- fmod_output.h:99
      requestreset : FMOD_OUTPUT_REQUESTRESET_FUNC;  -- fmod_output.h:100
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_output.h:92

   type FMOD_OUTPUT_OBJECT3DINFO is record
      buffer : access float;  -- fmod_output.h:105
      bufferlength : aliased unsigned;  -- fmod_output.h:106
      position : aliased fmod_common_h.FMOD_VECTOR;  -- fmod_output.h:107
      gain : aliased float;  -- fmod_output.h:108
      spread : aliased float;  -- fmod_output.h:109
      priority : aliased float;  -- fmod_output.h:110
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_output.h:103

  --    Output macros
  -- 

end fmod_output_h;
