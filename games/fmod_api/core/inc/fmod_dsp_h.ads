pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with fmod_common_h;
with System;
with Interfaces.C.Strings;
with Interfaces.C.Extensions;
with fmod_dsp_effects_h;

package fmod_dsp_h is

   FMOD_PLUGIN_SDK_VERSION : constant := 110;  --  fmod_dsp.h:24
   FMOD_DSP_GETPARAM_VALUESTR_LENGTH : constant := 32;  --  fmod_dsp.h:25
   --  arg-macro: procedure FMOD_DSP_INIT_PARAMDESC_FLOAT (_paramstruct, _name, _label, _description, _min, _max, _defaultval)
   --    memset(and(_paramstruct), 0, sizeof(_paramstruct)); (_paramstruct).type := FMOD_DSP_PARAMETER_TYPE_FLOAT; strncpy((_paramstruct).name, _name, 15); strncpy((_paramstruct).label, _label, 15); (_paramstruct).description := _description; (_paramstruct).floatdesc.min := _min; (_paramstruct).floatdesc.max := _max; (_paramstruct).floatdesc.defaultval := _defaultval; (_paramstruct).floatdesc.mapping.type := FMOD_DSP_PARAMETER_FLOAT_MAPPING_TYPE_AUTO;
   --  arg-macro: procedure FMOD_DSP_INIT_PARAMDESC_FLOAT_WITH_MAPPING (_paramstruct, _name, _label, _description, _defaultval, _values, _positions)
   --    ; memset(and(_paramstruct), 0, sizeof(_paramstruct)); (_paramstruct).type := FMOD_DSP_PARAMETER_TYPE_FLOAT; strncpy((_paramstruct).name, _name , 15); strncpy((_paramstruct).label, _label, 15); (_paramstruct).description := _description; (_paramstruct).floatdesc.min := _values(0); (_paramstruct).floatdesc.max := _values(sizeof(_values) / sizeof(float) - 1); (_paramstruct).floatdesc.defaultval := _defaultval; (_paramstruct).floatdesc.mapping.type := FMOD_DSP_PARAMETER_FLOAT_MAPPING_TYPE_PIECEWISE_LINEAR; (_paramstruct).floatdesc.mapping.piecewiselinearmapping.numpoints := sizeof(_values) / sizeof(float); (_paramstruct).floatdesc.mapping.piecewiselinearmapping.pointparamvalues := _values; (_paramstruct).floatdesc.mapping.piecewiselinearmapping.pointpositions := _positions;
   --  arg-macro: procedure FMOD_DSP_INIT_PARAMDESC_INT (_paramstruct, _name, _label, _description, _min, _max, _defaultval, _goestoinf, _valuenames)
   --    memset(and(_paramstruct), 0, sizeof(_paramstruct)); (_paramstruct).type := FMOD_DSP_PARAMETER_TYPE_INT; strncpy((_paramstruct).name, _name , 15); strncpy((_paramstruct).label, _label, 15); (_paramstruct).description := _description; (_paramstruct).intdesc.min := _min; (_paramstruct).intdesc.max := _max; (_paramstruct).intdesc.defaultval := _defaultval; (_paramstruct).intdesc.goestoinf := _goestoinf; (_paramstruct).intdesc.valuenames := _valuenames;
   --  arg-macro: procedure FMOD_DSP_INIT_PARAMDESC_INT_ENUMERATED (_paramstruct, _name, _label, _description, _defaultval, _valuenames)
   --    memset(and(_paramstruct), 0, sizeof(_paramstruct)); (_paramstruct).type := FMOD_DSP_PARAMETER_TYPE_INT; strncpy((_paramstruct).name, _name , 15); strncpy((_paramstruct).label, _label, 15); (_paramstruct).description := _description; (_paramstruct).intdesc.min := 0; (_paramstruct).intdesc.max := sizeof(_valuenames) / sizeof(char*) - 1; (_paramstruct).intdesc.defaultval := _defaultval; (_paramstruct).intdesc.goestoinf := false; (_paramstruct).intdesc.valuenames := _valuenames;
   --  arg-macro: procedure FMOD_DSP_INIT_PARAMDESC_BOOL (_paramstruct, _name, _label, _description, _defaultval, _valuenames)
   --    memset(and(_paramstruct), 0, sizeof(_paramstruct)); (_paramstruct).type := FMOD_DSP_PARAMETER_TYPE_BOOL; strncpy((_paramstruct).name, _name , 15); strncpy((_paramstruct).label, _label, 15); (_paramstruct).description := _description; (_paramstruct).booldesc.defaultval := _defaultval; (_paramstruct).booldesc.valuenames := _valuenames;
   --  arg-macro: procedure FMOD_DSP_INIT_PARAMDESC_DATA (_paramstruct, _name, _label, _description, _datatype)
   --    memset(and(_paramstruct), 0, sizeof(_paramstruct)); (_paramstruct).type := FMOD_DSP_PARAMETER_TYPE_DATA; strncpy((_paramstruct).name, _name , 15); strncpy((_paramstruct).label, _label, 15); (_paramstruct).description := _description; (_paramstruct).datadesc.datatype := _datatype;
   --  arg-macro: function FMOD_DSP_ALLOC (_state, _size)
   --    return _state).functions.alloc(_size, FMOD_MEMORY_NORMAL, __FILE__;
   --  arg-macro: function FMOD_DSP_REALLOC (_state, _ptr, _size)
   --    return _state).functions.realloc(_ptr, _size, FMOD_MEMORY_NORMAL, __FILE__;
   --  arg-macro: function FMOD_DSP_FREE (_state, _ptr)
   --    return _state).functions.free(_ptr, FMOD_MEMORY_NORMAL, __FILE__;
   --  unsupported macro: FMOD_DSP_LOG(_state,_level,_location,_format,...) (_state)->functions->log(_level, __FILE__, __LINE__, _location, _format, __VA_ARGS__)
   --  arg-macro: function FMOD_DSP_GETSAMPLERATE (_state, _rate)
   --    return _state).functions.getsamplerate(_state, _rate;
   --  arg-macro: function FMOD_DSP_GETBLOCKSIZE (_state, _blocksize)
   --    return _state).functions.getblocksize(_state, _blocksize;
   --  arg-macro: function FMOD_DSP_GETSPEAKERMODE (_state, _speakermodemix, _speakermodeout)
   --    return _state).functions.getspeakermode(_state, _speakermodemix, _speakermodeout;
   --  arg-macro: function FMOD_DSP_GETCLOCK (_state, _clock, _offset, _length)
   --    return _state).functions.getclock(_state, _clock, _offset, _length;
   --  arg-macro: function FMOD_DSP_GETLISTENERATTRIBUTES (_state, _numlisteners, _attributes)
   --    return _state).functions.getlistenerattributes(_state, _numlisteners, _attributes;
   --  arg-macro: function FMOD_DSP_GETUSERDATA (_state, _userdata)
   --    return _state).functions.getuserdata(_state, _userdata;
   --  arg-macro: function FMOD_DSP_DFT_FFTREAL (_state, _size, _signal, _dft, _window, _signalhop)
   --    return _state).functions.dft.fftreal(_state, _size, _signal, _dft, _window, _signalhop;
   --  arg-macro: function FMOD_DSP_DFT_IFFTREAL (_state, _size, _dft, _signal, _window, _signalhop)
   --    return _state).functions.dft.inversefftreal(_state, _size, _dft, _signal, _window, _signalhop;
   --  arg-macro: function FMOD_DSP_PAN_SUMMONOMATRIX (_state, _sourcespeakermode, _lowfrequencygain, _overallgain, _matrix)
   --    return _state).functions.pan.summonomatrix(_state, _sourcespeakermode, _lowfrequencygain, _overallgain, _matrix;
   --  arg-macro: function FMOD_DSP_PAN_SUMSTEREOMATRIX (_state, _sourcespeakermode, _pan, _lowfrequencygain, _overallgain, _matrixhop, _matrix)
   --    return _state).functions.pan.sumstereomatrix(_state, _sourcespeakermode, _pan, _lowfrequencygain, _overallgain, _matrixhop, _matrix;
   --  arg-macro: function FMOD_DSP_PAN_SUMSURROUNDMATRIX (_state, _sourcespeakermode, _targetspeakermode, _direction, _extent, _rotation, _lowfrequencygain, _overallgain, _matrixhop, _matrix, _flags)
   --    return _state).functions.pan.sumsurroundmatrix(_state, _sourcespeakermode, _targetspeakermode, _direction, _extent, _rotation, _lowfrequencygain, _overallgain, _matrixhop, _matrix, _flags;
   --  arg-macro: function FMOD_DSP_PAN_SUMMONOTOSURROUNDMATRIX (_state, _targetspeakermode, _direction, _extent, _lowfrequencygain, _overallgain, _matrixhop, _matrix)
   --    return _state).functions.pan.summonotosurroundmatrix(_state, _targetspeakermode, _direction, _extent, _lowfrequencygain, _overallgain, _matrixhop, _matrix;
   --  arg-macro: function FMOD_DSP_PAN_SUMSTEREOTOSURROUNDMATRIX (_state, _targetspeakermode, _direction, _extent, _rotation, _lowfrequencygain, _overallgain, matrixhop, _matrix)
   --    return _state).functions.pan.sumstereotosurroundmatrix(_state, _targetspeakermode, _direction, _extent, _rotation, _lowfrequencygain, _overallgain, matrixhop, _matrix;
   --  arg-macro: function FMOD_DSP_PAN_GETROLLOFFGAIN (_state, _rolloff, _distance, _mindistance, _maxdistance, _gain)
   --    return _state).functions.pan.getrolloffgain(_state, _rolloff, _distance, _mindistance, _maxdistance, _gain;

  -- ========================================================================================  
  -- FMOD Core API - DSP header file.                                                          
  -- Copyright (c), Firelight Technologies Pty, Ltd. 2004-2021.                                
  --                                                                                           
  -- Use this header if you are wanting to develop your own DSP plugin to use with FMODs       
  -- dsp system.  With this header you can make your own DSP plugin that FMOD can              
  -- register and use.  See the documentation and examples on how to make a working plugin.    
  --                                                                                           
  -- For more detail visit:                                                                    
  -- https://fmod.com/resources/documentation-api?version=2.0&page=plugin-api-dsp.html         
  -- ========================================================================================= 
   type FMOD_DSP_STATE;
   type FMOD_DSP_BUFFER_ARRAY;
   type FMOD_COMPLEX;
  --    DSP Constants
  -- 

   type FMOD_DSP_PROCESS_OPERATION is 
     (FMOD_DSP_PROCESS_PERFORM,
      FMOD_DSP_PROCESS_QUERY)
   with Convention => C;  -- fmod_dsp.h:31

   subtype FMOD_DSP_PAN_SURROUND_FLAGS is unsigned;
   FMOD_DSP_PAN_SURROUND_DEFAULT : constant unsigned := 0;
   FMOD_DSP_PAN_SURROUND_ROTATION_NOT_BIASED : constant unsigned := 1;
   FMOD_DSP_PAN_SURROUND_FLAGS_FORCEINT : constant unsigned := 65536;  -- fmod_dsp.h:33

   subtype FMOD_DSP_PARAMETER_TYPE is unsigned;
   FMOD_DSP_PARAMETER_TYPE_FLOAT : constant unsigned := 0;
   FMOD_DSP_PARAMETER_TYPE_INT : constant unsigned := 1;
   FMOD_DSP_PARAMETER_TYPE_BOOL : constant unsigned := 2;
   FMOD_DSP_PARAMETER_TYPE_DATA : constant unsigned := 3;
   FMOD_DSP_PARAMETER_TYPE_MAX : constant unsigned := 4;
   FMOD_DSP_PARAMETER_TYPE_FORCEINT : constant unsigned := 65536;  -- fmod_dsp.h:50

   subtype FMOD_DSP_PARAMETER_FLOAT_MAPPING_TYPE is unsigned;
   FMOD_DSP_PARAMETER_FLOAT_MAPPING_TYPE_LINEAR : constant unsigned := 0;
   FMOD_DSP_PARAMETER_FLOAT_MAPPING_TYPE_AUTO : constant unsigned := 1;
   FMOD_DSP_PARAMETER_FLOAT_MAPPING_TYPE_PIECEWISE_LINEAR : constant unsigned := 2;
   FMOD_DSP_PARAMETER_FLOAT_MAPPING_TYPE_FORCEINT : constant unsigned := 65536;  -- fmod_dsp.h:59

   subtype FMOD_DSP_PARAMETER_DATA_TYPE is int;
   FMOD_DSP_PARAMETER_DATA_TYPE_USER : constant int := 0;
   FMOD_DSP_PARAMETER_DATA_TYPE_OVERALLGAIN : constant int := -1;
   FMOD_DSP_PARAMETER_DATA_TYPE_3DATTRIBUTES : constant int := -2;
   FMOD_DSP_PARAMETER_DATA_TYPE_SIDECHAIN : constant int := -3;
   FMOD_DSP_PARAMETER_DATA_TYPE_FFT : constant int := -4;
   FMOD_DSP_PARAMETER_DATA_TYPE_3DATTRIBUTES_MULTI : constant int := -5;  -- fmod_dsp.h:69

  --    DSP Callbacks
  -- 

   type FMOD_DSP_CREATE_CALLBACK is access function (arg1 : access FMOD_DSP_STATE) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:74

   type FMOD_DSP_RELEASE_CALLBACK is access function (arg1 : access FMOD_DSP_STATE) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:75

   type FMOD_DSP_RESET_CALLBACK is access function (arg1 : access FMOD_DSP_STATE) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:76

   type FMOD_DSP_READ_CALLBACK is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : access float;
         arg3 : access float;
         arg4 : unsigned;
         arg5 : int;
         arg6 : access int) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:77

   type FMOD_DSP_PROCESS_CALLBACK is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : unsigned;
         arg3 : access constant FMOD_DSP_BUFFER_ARRAY;
         arg4 : access FMOD_DSP_BUFFER_ARRAY;
         arg5 : fmod_common_h.FMOD_BOOL;
         arg6 : FMOD_DSP_PROCESS_OPERATION) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:78

   type FMOD_DSP_SETPOSITION_CALLBACK is access function (arg1 : access FMOD_DSP_STATE; arg2 : unsigned) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:79

   type FMOD_DSP_SHOULDIPROCESS_CALLBACK is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : fmod_common_h.FMOD_BOOL;
         arg3 : unsigned;
         arg4 : fmod_common_h.FMOD_CHANNELMASK;
         arg5 : int;
         arg6 : fmod_common_h.FMOD_SPEAKERMODE) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:80

   type FMOD_DSP_SETPARAM_FLOAT_CALLBACK is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : int;
         arg3 : float) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:81

   type FMOD_DSP_SETPARAM_INT_CALLBACK is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : int;
         arg3 : int) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:82

   type FMOD_DSP_SETPARAM_BOOL_CALLBACK is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : int;
         arg3 : fmod_common_h.FMOD_BOOL) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:83

   type FMOD_DSP_SETPARAM_DATA_CALLBACK is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : int;
         arg3 : System.Address;
         arg4 : unsigned) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:84

   type FMOD_DSP_GETPARAM_FLOAT_CALLBACK is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : int;
         arg3 : access float;
         arg4 : Interfaces.C.Strings.chars_ptr) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:85

   type FMOD_DSP_GETPARAM_INT_CALLBACK is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : int;
         arg3 : access int;
         arg4 : Interfaces.C.Strings.chars_ptr) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:86

   type FMOD_DSP_GETPARAM_BOOL_CALLBACK is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : int;
         arg3 : access fmod_common_h.FMOD_BOOL;
         arg4 : Interfaces.C.Strings.chars_ptr) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:87

   type FMOD_DSP_GETPARAM_DATA_CALLBACK is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : int;
         arg3 : System.Address;
         arg4 : access unsigned;
         arg5 : Interfaces.C.Strings.chars_ptr) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:88

   type FMOD_DSP_SYSTEM_REGISTER_CALLBACK is access function (arg1 : access FMOD_DSP_STATE) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:89

   type FMOD_DSP_SYSTEM_DEREGISTER_CALLBACK is access function (arg1 : access FMOD_DSP_STATE) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:90

   type FMOD_DSP_SYSTEM_MIX_CALLBACK is access function (arg1 : access FMOD_DSP_STATE; arg2 : int) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:91

  --    DSP Functions
  -- 

   type FMOD_DSP_ALLOC_FUNC is access function
        (arg1 : unsigned;
         arg2 : fmod_common_h.FMOD_MEMORY_TYPE;
         arg3 : Interfaces.C.Strings.chars_ptr) return System.Address
   with Convention => C;  -- fmod_dsp.h:96

   type FMOD_DSP_REALLOC_FUNC is access function
        (arg1 : System.Address;
         arg2 : unsigned;
         arg3 : fmod_common_h.FMOD_MEMORY_TYPE;
         arg4 : Interfaces.C.Strings.chars_ptr) return System.Address
   with Convention => C;  -- fmod_dsp.h:97

   type FMOD_DSP_FREE_FUNC is access procedure
        (arg1 : System.Address;
         arg2 : fmod_common_h.FMOD_MEMORY_TYPE;
         arg3 : Interfaces.C.Strings.chars_ptr)
   with Convention => C;  -- fmod_dsp.h:98

   type FMOD_DSP_LOG_FUNC is access procedure
        (arg1 : fmod_common_h.FMOD_DEBUG_FLAGS;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : int;
         arg4 : Interfaces.C.Strings.chars_ptr;
         arg5 : Interfaces.C.Strings.chars_ptr  -- , ...
         )
   with Convention => C;  -- fmod_dsp.h:99

   type FMOD_DSP_GETSAMPLERATE_FUNC is access function (arg1 : access FMOD_DSP_STATE; arg2 : access int) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:100

   type FMOD_DSP_GETBLOCKSIZE_FUNC is access function (arg1 : access FMOD_DSP_STATE; arg2 : access unsigned) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:101

   type FMOD_DSP_GETSPEAKERMODE_FUNC is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : access fmod_common_h.FMOD_SPEAKERMODE;
         arg3 : access fmod_common_h.FMOD_SPEAKERMODE) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:102

   type FMOD_DSP_GETCLOCK_FUNC is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : access Extensions.unsigned_long_long;
         arg3 : access unsigned;
         arg4 : access unsigned) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:103

   type FMOD_DSP_GETLISTENERATTRIBUTES_FUNC is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : access int;
         arg3 : access fmod_common_h.FMOD_3D_ATTRIBUTES) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:104

   type FMOD_DSP_GETUSERDATA_FUNC is access function (arg1 : access FMOD_DSP_STATE; arg2 : System.Address) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:105

   type FMOD_DSP_DFT_FFTREAL_FUNC is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : int;
         arg3 : access float;
         arg4 : access FMOD_COMPLEX;
         arg5 : access float;
         arg6 : int) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:106

   type FMOD_DSP_DFT_IFFTREAL_FUNC is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : int;
         arg3 : access constant FMOD_COMPLEX;
         arg4 : access float;
         arg5 : access float;
         arg6 : int) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:107

   type FMOD_DSP_PAN_SUMMONOMATRIX_FUNC is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : fmod_common_h.FMOD_SPEAKERMODE;
         arg3 : float;
         arg4 : float;
         arg5 : access float) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:108

   type FMOD_DSP_PAN_SUMSTEREOMATRIX_FUNC is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : fmod_common_h.FMOD_SPEAKERMODE;
         arg3 : float;
         arg4 : float;
         arg5 : float;
         arg6 : int;
         arg7 : access float) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:109

   type FMOD_DSP_PAN_SUMSURROUNDMATRIX_FUNC is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : fmod_common_h.FMOD_SPEAKERMODE;
         arg3 : fmod_common_h.FMOD_SPEAKERMODE;
         arg4 : float;
         arg5 : float;
         arg6 : float;
         arg7 : float;
         arg8 : float;
         arg9 : int;
         arg10 : access float;
         arg11 : FMOD_DSP_PAN_SURROUND_FLAGS) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:110

   type FMOD_DSP_PAN_SUMMONOTOSURROUNDMATRIX_FUNC is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : fmod_common_h.FMOD_SPEAKERMODE;
         arg3 : float;
         arg4 : float;
         arg5 : float;
         arg6 : float;
         arg7 : int;
         arg8 : access float) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:111

   type FMOD_DSP_PAN_SUMSTEREOTOSURROUNDMATRIX_FUNC is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : fmod_common_h.FMOD_SPEAKERMODE;
         arg3 : float;
         arg4 : float;
         arg5 : float;
         arg6 : float;
         arg7 : float;
         arg8 : int;
         arg9 : access float) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:112

   type FMOD_DSP_PAN_GETROLLOFFGAIN_FUNC is access function
        (arg1 : access FMOD_DSP_STATE;
         arg2 : fmod_dsp_effects_h.FMOD_DSP_PAN_3D_ROLLOFF_TYPE;
         arg3 : float;
         arg4 : float;
         arg5 : float;
         arg6 : access float) return fmod_common_h.FMOD_RESULT
   with Convention => C;  -- fmod_dsp.h:113

  --    DSP Structures
  -- 

   type FMOD_DSP_BUFFER_ARRAY is record
      numbuffers : aliased int;  -- fmod_dsp.h:120
      buffernumchannels : access int;  -- fmod_dsp.h:121
      bufferchannelmask : access fmod_common_h.FMOD_CHANNELMASK;  -- fmod_dsp.h:122
      buffers : System.Address;  -- fmod_dsp.h:123
      speakermode : aliased fmod_common_h.FMOD_SPEAKERMODE;  -- fmod_dsp.h:124
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_dsp.h:118

   type FMOD_COMPLEX is record
      real : aliased float;  -- fmod_dsp.h:129
      imag : aliased float;  -- fmod_dsp.h:130
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_dsp.h:127

   type FMOD_DSP_PARAMETER_FLOAT_MAPPING_PIECEWISE_LINEAR is record
      numpoints : aliased int;  -- fmod_dsp.h:135
      pointparamvalues : access float;  -- fmod_dsp.h:136
      pointpositions : access float;  -- fmod_dsp.h:137
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_dsp.h:133

   type FMOD_DSP_PARAMETER_FLOAT_MAPPING is record
      c_type : aliased FMOD_DSP_PARAMETER_FLOAT_MAPPING_TYPE;  -- fmod_dsp.h:142
      piecewiselinearmapping : aliased FMOD_DSP_PARAMETER_FLOAT_MAPPING_PIECEWISE_LINEAR;  -- fmod_dsp.h:143
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_dsp.h:140

   type FMOD_DSP_PARAMETER_DESC_FLOAT is record
      min : aliased float;  -- fmod_dsp.h:148
      max : aliased float;  -- fmod_dsp.h:149
      defaultval : aliased float;  -- fmod_dsp.h:150
      mapping : aliased FMOD_DSP_PARAMETER_FLOAT_MAPPING;  -- fmod_dsp.h:151
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_dsp.h:146

   type FMOD_DSP_PARAMETER_DESC_INT is record
      min : aliased int;  -- fmod_dsp.h:156
      max : aliased int;  -- fmod_dsp.h:157
      defaultval : aliased int;  -- fmod_dsp.h:158
      goestoinf : aliased fmod_common_h.FMOD_BOOL;  -- fmod_dsp.h:159
      valuenames : System.Address;  -- fmod_dsp.h:160
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_dsp.h:154

   type FMOD_DSP_PARAMETER_DESC_BOOL is record
      defaultval : aliased fmod_common_h.FMOD_BOOL;  -- fmod_dsp.h:165
      valuenames : System.Address;  -- fmod_dsp.h:166
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_dsp.h:163

   type FMOD_DSP_PARAMETER_DESC_DATA is record
      datatype : aliased int;  -- fmod_dsp.h:171
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_dsp.h:169

   type anon_42 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            floatdesc : aliased FMOD_DSP_PARAMETER_DESC_FLOAT;  -- fmod_dsp.h:183
         when 1 =>
            intdesc : aliased FMOD_DSP_PARAMETER_DESC_INT;  -- fmod_dsp.h:184
         when 2 =>
            booldesc : aliased FMOD_DSP_PARAMETER_DESC_BOOL;  -- fmod_dsp.h:185
         when others =>
            datadesc : aliased FMOD_DSP_PARAMETER_DESC_DATA;  -- fmod_dsp.h:186
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;
   subtype FMOD_DSP_PARAMETER_DESC_name_array is Interfaces.C.char_array (0 .. 15);
   subtype FMOD_DSP_PARAMETER_DESC_label_array is Interfaces.C.char_array (0 .. 15);
   type FMOD_DSP_PARAMETER_DESC is record
      c_type : aliased FMOD_DSP_PARAMETER_TYPE;  -- fmod_dsp.h:176
      name : aliased FMOD_DSP_PARAMETER_DESC_name_array;  -- fmod_dsp.h:177
      label : aliased FMOD_DSP_PARAMETER_DESC_label_array;  -- fmod_dsp.h:178
      description : Interfaces.C.Strings.chars_ptr;  -- fmod_dsp.h:179
      field_5 : aliased anon_42;
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_dsp.h:174

   type FMOD_DSP_PARAMETER_OVERALLGAIN is record
      linear_gain : aliased float;  -- fmod_dsp.h:192
      linear_gain_additive : aliased float;  -- fmod_dsp.h:193
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_dsp.h:190

   type FMOD_DSP_PARAMETER_3DATTRIBUTES is record
      relative : aliased fmod_common_h.FMOD_3D_ATTRIBUTES;  -- fmod_dsp.h:198
      absolute : aliased fmod_common_h.FMOD_3D_ATTRIBUTES;  -- fmod_dsp.h:199
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_dsp.h:196

   type FMOD_DSP_PARAMETER_3DATTRIBUTES_MULTI_relative_array is array (0 .. 7) of aliased fmod_common_h.FMOD_3D_ATTRIBUTES;
   type FMOD_DSP_PARAMETER_3DATTRIBUTES_MULTI_weight_array is array (0 .. 7) of aliased float;
   type FMOD_DSP_PARAMETER_3DATTRIBUTES_MULTI is record
      numlisteners : aliased int;  -- fmod_dsp.h:204
      relative : aliased FMOD_DSP_PARAMETER_3DATTRIBUTES_MULTI_relative_array;  -- fmod_dsp.h:205
      weight : aliased FMOD_DSP_PARAMETER_3DATTRIBUTES_MULTI_weight_array;  -- fmod_dsp.h:206
      absolute : aliased fmod_common_h.FMOD_3D_ATTRIBUTES;  -- fmod_dsp.h:207
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_dsp.h:202

   type FMOD_DSP_PARAMETER_SIDECHAIN is record
      sidechainenable : aliased fmod_common_h.FMOD_BOOL;  -- fmod_dsp.h:212
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_dsp.h:210

   type FMOD_DSP_PARAMETER_FFT_spectrum_array is array (0 .. 31) of access float;
   type FMOD_DSP_PARAMETER_FFT is record
      length : aliased int;  -- fmod_dsp.h:217
      numchannels : aliased int;  -- fmod_dsp.h:218
      spectrum : FMOD_DSP_PARAMETER_FFT_spectrum_array;  -- fmod_dsp.h:219
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_dsp.h:215

   subtype FMOD_DSP_DESCRIPTION_name_array is Interfaces.C.char_array (0 .. 31);
   type FMOD_DSP_DESCRIPTION is record
      pluginsdkversion : aliased unsigned;  -- fmod_dsp.h:224
      name : aliased FMOD_DSP_DESCRIPTION_name_array;  -- fmod_dsp.h:225
      version : aliased unsigned;  -- fmod_dsp.h:226
      numinputbuffers : aliased int;  -- fmod_dsp.h:227
      numoutputbuffers : aliased int;  -- fmod_dsp.h:228
      create : FMOD_DSP_CREATE_CALLBACK;  -- fmod_dsp.h:229
      release : FMOD_DSP_RELEASE_CALLBACK;  -- fmod_dsp.h:230
      reset : FMOD_DSP_RESET_CALLBACK;  -- fmod_dsp.h:231
      read : FMOD_DSP_READ_CALLBACK;  -- fmod_dsp.h:232
      process : FMOD_DSP_PROCESS_CALLBACK;  -- fmod_dsp.h:233
      setposition : FMOD_DSP_SETPOSITION_CALLBACK;  -- fmod_dsp.h:234
      numparameters : aliased int;  -- fmod_dsp.h:236
      paramdesc : System.Address;  -- fmod_dsp.h:237
      setparameterfloat : FMOD_DSP_SETPARAM_FLOAT_CALLBACK;  -- fmod_dsp.h:238
      setparameterint : FMOD_DSP_SETPARAM_INT_CALLBACK;  -- fmod_dsp.h:239
      setparameterbool : FMOD_DSP_SETPARAM_BOOL_CALLBACK;  -- fmod_dsp.h:240
      setparameterdata : FMOD_DSP_SETPARAM_DATA_CALLBACK;  -- fmod_dsp.h:241
      getparameterfloat : FMOD_DSP_GETPARAM_FLOAT_CALLBACK;  -- fmod_dsp.h:242
      getparameterint : FMOD_DSP_GETPARAM_INT_CALLBACK;  -- fmod_dsp.h:243
      getparameterbool : FMOD_DSP_GETPARAM_BOOL_CALLBACK;  -- fmod_dsp.h:244
      getparameterdata : FMOD_DSP_GETPARAM_DATA_CALLBACK;  -- fmod_dsp.h:245
      shouldiprocess : FMOD_DSP_SHOULDIPROCESS_CALLBACK;  -- fmod_dsp.h:246
      userdata : System.Address;  -- fmod_dsp.h:247
      sys_register : FMOD_DSP_SYSTEM_REGISTER_CALLBACK;  -- fmod_dsp.h:249
      sys_deregister : FMOD_DSP_SYSTEM_DEREGISTER_CALLBACK;  -- fmod_dsp.h:250
      sys_mix : FMOD_DSP_SYSTEM_MIX_CALLBACK;  -- fmod_dsp.h:251
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_dsp.h:222

   type FMOD_DSP_STATE_DFT_FUNCTIONS is record
      fftreal : FMOD_DSP_DFT_FFTREAL_FUNC;  -- fmod_dsp.h:257
      inversefftreal : FMOD_DSP_DFT_IFFTREAL_FUNC;  -- fmod_dsp.h:258
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_dsp.h:255

   type FMOD_DSP_STATE_PAN_FUNCTIONS is record
      summonomatrix : FMOD_DSP_PAN_SUMMONOMATRIX_FUNC;  -- fmod_dsp.h:263
      sumstereomatrix : FMOD_DSP_PAN_SUMSTEREOMATRIX_FUNC;  -- fmod_dsp.h:264
      sumsurroundmatrix : FMOD_DSP_PAN_SUMSURROUNDMATRIX_FUNC;  -- fmod_dsp.h:265
      summonotosurroundmatrix : FMOD_DSP_PAN_SUMMONOTOSURROUNDMATRIX_FUNC;  -- fmod_dsp.h:266
      sumstereotosurroundmatrix : FMOD_DSP_PAN_SUMSTEREOTOSURROUNDMATRIX_FUNC;  -- fmod_dsp.h:267
      getrolloffgain : FMOD_DSP_PAN_GETROLLOFFGAIN_FUNC;  -- fmod_dsp.h:268
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_dsp.h:261

   type FMOD_DSP_STATE_FUNCTIONS is record
      alloc : FMOD_DSP_ALLOC_FUNC;  -- fmod_dsp.h:273
      realloc : FMOD_DSP_REALLOC_FUNC;  -- fmod_dsp.h:274
      free : FMOD_DSP_FREE_FUNC;  -- fmod_dsp.h:275
      getsamplerate : FMOD_DSP_GETSAMPLERATE_FUNC;  -- fmod_dsp.h:276
      getblocksize : FMOD_DSP_GETBLOCKSIZE_FUNC;  -- fmod_dsp.h:277
      dft : access FMOD_DSP_STATE_DFT_FUNCTIONS;  -- fmod_dsp.h:278
      pan : access FMOD_DSP_STATE_PAN_FUNCTIONS;  -- fmod_dsp.h:279
      getspeakermode : FMOD_DSP_GETSPEAKERMODE_FUNC;  -- fmod_dsp.h:280
      getclock : FMOD_DSP_GETCLOCK_FUNC;  -- fmod_dsp.h:281
      getlistenerattributes : FMOD_DSP_GETLISTENERATTRIBUTES_FUNC;  -- fmod_dsp.h:282
      log : FMOD_DSP_LOG_FUNC;  -- fmod_dsp.h:283
      getuserdata : FMOD_DSP_GETUSERDATA_FUNC;  -- fmod_dsp.h:284
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_dsp.h:271

   type FMOD_DSP_STATE is record
      instance : System.Address;  -- fmod_dsp.h:289
      plugindata : System.Address;  -- fmod_dsp.h:290
      channelmask : aliased fmod_common_h.FMOD_CHANNELMASK;  -- fmod_dsp.h:291
      source_speakermode : aliased fmod_common_h.FMOD_SPEAKERMODE;  -- fmod_dsp.h:292
      sidechaindata : access float;  -- fmod_dsp.h:293
      sidechainchannels : aliased int;  -- fmod_dsp.h:294
      functions : access FMOD_DSP_STATE_FUNCTIONS;  -- fmod_dsp.h:295
      systemobject : aliased int;  -- fmod_dsp.h:296
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_dsp.h:287

   type FMOD_DSP_METERING_INFO_peaklevel_array is array (0 .. 31) of aliased float;
   type FMOD_DSP_METERING_INFO_rmslevel_array is array (0 .. 31) of aliased float;
   type FMOD_DSP_METERING_INFO is record
      numsamples : aliased int;  -- fmod_dsp.h:301
      peaklevel : aliased FMOD_DSP_METERING_INFO_peaklevel_array;  -- fmod_dsp.h:302
      rmslevel : aliased FMOD_DSP_METERING_INFO_rmslevel_array;  -- fmod_dsp.h:303
      numchannels : aliased short;  -- fmod_dsp.h:304
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_dsp.h:299

  --    DSP Macros
  -- 

end fmod_dsp_h;
