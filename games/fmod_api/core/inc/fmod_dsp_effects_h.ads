pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package fmod_dsp_effects_h is

  -- =============================================================================================================  
  -- FMOD Core API - Built-in effects header file.                                                                  
  -- Copyright (c), Firelight Technologies Pty, Ltd. 2004-2021.                                                     
  --                                                                                                                
  -- In this header you can find parameter structures for FMOD system registered DSP effects                        
  -- and generators.                                                                                                
  --                                                                                                                
  -- For more detail visit:                                                                                         
  -- https://fmod.com/resources/documentation-api?version=2.0&page=core-api-common-dsp-effects.html#fmod_dsp_type   
  -- =============================================================================================================  
  -- Makes sure this enum is signed 32bit.  
   subtype FMOD_DSP_TYPE is unsigned;
   FMOD_DSP_TYPE_UNKNOWN : constant unsigned := 0;
   FMOD_DSP_TYPE_MIXER : constant unsigned := 1;
   FMOD_DSP_TYPE_OSCILLATOR : constant unsigned := 2;
   FMOD_DSP_TYPE_LOWPASS : constant unsigned := 3;
   FMOD_DSP_TYPE_ITLOWPASS : constant unsigned := 4;
   FMOD_DSP_TYPE_HIGHPASS : constant unsigned := 5;
   FMOD_DSP_TYPE_ECHO : constant unsigned := 6;
   FMOD_DSP_TYPE_FADER : constant unsigned := 7;
   FMOD_DSP_TYPE_FLANGE : constant unsigned := 8;
   FMOD_DSP_TYPE_DISTORTION : constant unsigned := 9;
   FMOD_DSP_TYPE_NORMALIZE : constant unsigned := 10;
   FMOD_DSP_TYPE_LIMITER : constant unsigned := 11;
   FMOD_DSP_TYPE_PARAMEQ : constant unsigned := 12;
   FMOD_DSP_TYPE_PITCHSHIFT : constant unsigned := 13;
   FMOD_DSP_TYPE_CHORUS : constant unsigned := 14;
   FMOD_DSP_TYPE_VSTPLUGIN : constant unsigned := 15;
   FMOD_DSP_TYPE_WINAMPPLUGIN : constant unsigned := 16;
   FMOD_DSP_TYPE_ITECHO : constant unsigned := 17;
   FMOD_DSP_TYPE_COMPRESSOR : constant unsigned := 18;
   FMOD_DSP_TYPE_SFXREVERB : constant unsigned := 19;
   FMOD_DSP_TYPE_LOWPASS_SIMPLE : constant unsigned := 20;
   FMOD_DSP_TYPE_DELAY : constant unsigned := 21;
   FMOD_DSP_TYPE_TREMOLO : constant unsigned := 22;
   FMOD_DSP_TYPE_LADSPAPLUGIN : constant unsigned := 23;
   FMOD_DSP_TYPE_SEND : constant unsigned := 24;
   FMOD_DSP_TYPE_RETURN : constant unsigned := 25;
   FMOD_DSP_TYPE_HIGHPASS_SIMPLE : constant unsigned := 26;
   FMOD_DSP_TYPE_PAN : constant unsigned := 27;
   FMOD_DSP_TYPE_THREE_EQ : constant unsigned := 28;
   FMOD_DSP_TYPE_FFT : constant unsigned := 29;
   FMOD_DSP_TYPE_LOUDNESS_METER : constant unsigned := 30;
   FMOD_DSP_TYPE_ENVELOPEFOLLOWER : constant unsigned := 31;
   FMOD_DSP_TYPE_CONVOLUTIONREVERB : constant unsigned := 32;
   FMOD_DSP_TYPE_CHANNELMIX : constant unsigned := 33;
   FMOD_DSP_TYPE_TRANSCEIVER : constant unsigned := 34;
   FMOD_DSP_TYPE_OBJECTPAN : constant unsigned := 35;
   FMOD_DSP_TYPE_MULTIBAND_EQ : constant unsigned := 36;
   FMOD_DSP_TYPE_MAX : constant unsigned := 37;
   FMOD_DSP_TYPE_FORCEINT : constant unsigned := 65536;  -- fmod_dsp_effects.h:57

  --    ===================================================================================================
  --    FMOD built in effect parameters.  
  --    Use DSP::setParameter with these enums for the 'index' parameter.
  --    ===================================================================================================
  -- 

   type FMOD_DSP_OSCILLATOR is 
     (FMOD_DSP_OSCILLATOR_TYPE,
      FMOD_DSP_OSCILLATOR_RATE)
   with Convention => C;  -- fmod_dsp_effects.h:72

   type FMOD_DSP_LOWPASS is 
     (FMOD_DSP_LOWPASS_CUTOFF,
      FMOD_DSP_LOWPASS_RESONANCE)
   with Convention => C;  -- fmod_dsp_effects.h:79

   type FMOD_DSP_ITLOWPASS is 
     (FMOD_DSP_ITLOWPASS_CUTOFF,
      FMOD_DSP_ITLOWPASS_RESONANCE)
   with Convention => C;  -- fmod_dsp_effects.h:86

   type FMOD_DSP_HIGHPASS is 
     (FMOD_DSP_HIGHPASS_CUTOFF,
      FMOD_DSP_HIGHPASS_RESONANCE)
   with Convention => C;  -- fmod_dsp_effects.h:93

   type FMOD_DSP_ECHO is 
     (FMOD_DSP_ECHO_DELAY,
      FMOD_DSP_ECHO_FEEDBACK,
      FMOD_DSP_ECHO_DRYLEVEL,
      FMOD_DSP_ECHO_WETLEVEL)
   with Convention => C;  -- fmod_dsp_effects.h:102

   type FMOD_DSP_FADER is 
     (FMOD_DSP_FADER_GAIN,
      FMOD_DSP_FADER_OVERALL_GAIN)
   with Convention => C;  -- fmod_dsp_effects.h:105

   type FMOD_DSP_FLANGE is 
     (FMOD_DSP_FLANGE_MIX,
      FMOD_DSP_FLANGE_DEPTH,
      FMOD_DSP_FLANGE_RATE)
   with Convention => C;  -- fmod_dsp_effects.h:117

   type FMOD_DSP_DISTORTION is 
     (FMOD_DSP_DISTORTION_LEVEL)
   with Convention => C;  -- fmod_dsp_effects.h:123

   type FMOD_DSP_NORMALIZE is 
     (FMOD_DSP_NORMALIZE_FADETIME,
      FMOD_DSP_NORMALIZE_THRESHHOLD,
      FMOD_DSP_NORMALIZE_MAXAMP)
   with Convention => C;  -- fmod_dsp_effects.h:131

   type FMOD_DSP_LIMITER is 
     (FMOD_DSP_LIMITER_RELEASETIME,
      FMOD_DSP_LIMITER_CEILING,
      FMOD_DSP_LIMITER_MAXIMIZERGAIN,
      FMOD_DSP_LIMITER_MODE)
   with Convention => C;  -- fmod_dsp_effects.h:140

   type FMOD_DSP_PARAMEQ is 
     (FMOD_DSP_PARAMEQ_CENTER,
      FMOD_DSP_PARAMEQ_BANDWIDTH,
      FMOD_DSP_PARAMEQ_GAIN)
   with Convention => C;  -- fmod_dsp_effects.h:148

   type FMOD_DSP_MULTIBAND_EQ is 
     (FMOD_DSP_MULTIBAND_EQ_A_FILTER,
      FMOD_DSP_MULTIBAND_EQ_A_FREQUENCY,
      FMOD_DSP_MULTIBAND_EQ_A_Q,
      FMOD_DSP_MULTIBAND_EQ_A_GAIN,
      FMOD_DSP_MULTIBAND_EQ_B_FILTER,
      FMOD_DSP_MULTIBAND_EQ_B_FREQUENCY,
      FMOD_DSP_MULTIBAND_EQ_B_Q,
      FMOD_DSP_MULTIBAND_EQ_B_GAIN,
      FMOD_DSP_MULTIBAND_EQ_C_FILTER,
      FMOD_DSP_MULTIBAND_EQ_C_FREQUENCY,
      FMOD_DSP_MULTIBAND_EQ_C_Q,
      FMOD_DSP_MULTIBAND_EQ_C_GAIN,
      FMOD_DSP_MULTIBAND_EQ_D_FILTER,
      FMOD_DSP_MULTIBAND_EQ_D_FREQUENCY,
      FMOD_DSP_MULTIBAND_EQ_D_Q,
      FMOD_DSP_MULTIBAND_EQ_D_GAIN,
      FMOD_DSP_MULTIBAND_EQ_E_FILTER,
      FMOD_DSP_MULTIBAND_EQ_E_FREQUENCY,
      FMOD_DSP_MULTIBAND_EQ_E_Q,
      FMOD_DSP_MULTIBAND_EQ_E_GAIN)
   with Convention => C;  -- fmod_dsp_effects.h:151

   type FMOD_DSP_MULTIBAND_EQ_FILTER_TYPE is 
     (FMOD_DSP_MULTIBAND_EQ_FILTER_DISABLED,
      FMOD_DSP_MULTIBAND_EQ_FILTER_LOWPASS_12DB,
      FMOD_DSP_MULTIBAND_EQ_FILTER_LOWPASS_24DB,
      FMOD_DSP_MULTIBAND_EQ_FILTER_LOWPASS_48DB,
      FMOD_DSP_MULTIBAND_EQ_FILTER_HIGHPASS_12DB,
      FMOD_DSP_MULTIBAND_EQ_FILTER_HIGHPASS_24DB,
      FMOD_DSP_MULTIBAND_EQ_FILTER_HIGHPASS_48DB,
      FMOD_DSP_MULTIBAND_EQ_FILTER_LOWSHELF,
      FMOD_DSP_MULTIBAND_EQ_FILTER_HIGHSHELF,
      FMOD_DSP_MULTIBAND_EQ_FILTER_PEAKING,
      FMOD_DSP_MULTIBAND_EQ_FILTER_BANDPASS,
      FMOD_DSP_MULTIBAND_EQ_FILTER_NOTCH,
      FMOD_DSP_MULTIBAND_EQ_FILTER_ALLPASS)
   with Convention => C;  -- fmod_dsp_effects.h:176

   type FMOD_DSP_PITCHSHIFT is 
     (FMOD_DSP_PITCHSHIFT_PITCH,
      FMOD_DSP_PITCHSHIFT_FFTSIZE,
      FMOD_DSP_PITCHSHIFT_OVERLAP,
      FMOD_DSP_PITCHSHIFT_MAXCHANNELS)
   with Convention => C;  -- fmod_dsp_effects.h:200

   type FMOD_DSP_CHORUS is 
     (FMOD_DSP_CHORUS_MIX,
      FMOD_DSP_CHORUS_RATE,
      FMOD_DSP_CHORUS_DEPTH)
   with Convention => C;  -- fmod_dsp_effects.h:208

   type FMOD_DSP_ITECHO is 
     (FMOD_DSP_ITECHO_WETDRYMIX,
      FMOD_DSP_ITECHO_FEEDBACK,
      FMOD_DSP_ITECHO_LEFTDELAY,
      FMOD_DSP_ITECHO_RIGHTDELAY,
      FMOD_DSP_ITECHO_PANDELAY)
   with Convention => C;  -- fmod_dsp_effects.h:218

   type FMOD_DSP_COMPRESSOR is 
     (FMOD_DSP_COMPRESSOR_THRESHOLD,
      FMOD_DSP_COMPRESSOR_RATIO,
      FMOD_DSP_COMPRESSOR_ATTACK,
      FMOD_DSP_COMPRESSOR_RELEASE,
      FMOD_DSP_COMPRESSOR_GAINMAKEUP,
      FMOD_DSP_COMPRESSOR_USESIDECHAIN,
      FMOD_DSP_COMPRESSOR_LINKED)
   with Convention => C;  -- fmod_dsp_effects.h:229

   type FMOD_DSP_SFXREVERB is 
     (FMOD_DSP_SFXREVERB_DECAYTIME,
      FMOD_DSP_SFXREVERB_EARLYDELAY,
      FMOD_DSP_SFXREVERB_LATEDELAY,
      FMOD_DSP_SFXREVERB_HFREFERENCE,
      FMOD_DSP_SFXREVERB_HFDECAYRATIO,
      FMOD_DSP_SFXREVERB_DIFFUSION,
      FMOD_DSP_SFXREVERB_DENSITY,
      FMOD_DSP_SFXREVERB_LOWSHELFFREQUENCY,
      FMOD_DSP_SFXREVERB_LOWSHELFGAIN,
      FMOD_DSP_SFXREVERB_HIGHCUT,
      FMOD_DSP_SFXREVERB_EARLYLATEMIX,
      FMOD_DSP_SFXREVERB_WETLEVEL,
      FMOD_DSP_SFXREVERB_DRYLEVEL)
   with Convention => C;  -- fmod_dsp_effects.h:246

   type FMOD_DSP_LOWPASS_SIMPLE is 
     (FMOD_DSP_LOWPASS_SIMPLE_CUTOFF)
   with Convention => C;  -- fmod_dsp_effects.h:251

   type FMOD_DSP_DELAY is 
     (FMOD_DSP_DELAY_CH0,
      FMOD_DSP_DELAY_CH1,
      FMOD_DSP_DELAY_CH2,
      FMOD_DSP_DELAY_CH3,
      FMOD_DSP_DELAY_CH4,
      FMOD_DSP_DELAY_CH5,
      FMOD_DSP_DELAY_CH6,
      FMOD_DSP_DELAY_CH7,
      FMOD_DSP_DELAY_CH8,
      FMOD_DSP_DELAY_CH9,
      FMOD_DSP_DELAY_CH10,
      FMOD_DSP_DELAY_CH11,
      FMOD_DSP_DELAY_CH12,
      FMOD_DSP_DELAY_CH13,
      FMOD_DSP_DELAY_CH14,
      FMOD_DSP_DELAY_CH15,
      FMOD_DSP_DELAY_MAXDELAY)
   with Convention => C;  -- fmod_dsp_effects.h:273

   type FMOD_DSP_TREMOLO is 
     (FMOD_DSP_TREMOLO_FREQUENCY,
      FMOD_DSP_TREMOLO_DEPTH,
      FMOD_DSP_TREMOLO_SHAPE,
      FMOD_DSP_TREMOLO_SKEW,
      FMOD_DSP_TREMOLO_DUTY,
      FMOD_DSP_TREMOLO_SQUARE,
      FMOD_DSP_TREMOLO_PHASE,
      FMOD_DSP_TREMOLO_SPREAD)
   with Convention => C;  -- fmod_dsp_effects.h:286

   type FMOD_DSP_SEND is 
     (FMOD_DSP_SEND_RETURNID,
      FMOD_DSP_SEND_LEVEL)
   with Convention => C;  -- fmod_dsp_effects.h:293

   type FMOD_DSP_RETURN is 
     (FMOD_DSP_RETURN_ID,
      FMOD_DSP_RETURN_INPUT_SPEAKER_MODE)
   with Convention => C;  -- fmod_dsp_effects.h:300

   type FMOD_DSP_HIGHPASS_SIMPLE is 
     (FMOD_DSP_HIGHPASS_SIMPLE_CUTOFF)
   with Convention => C;  -- fmod_dsp_effects.h:306

   type FMOD_DSP_PAN_2D_STEREO_MODE_TYPE is 
     (FMOD_DSP_PAN_2D_STEREO_MODE_DISTRIBUTED,
      FMOD_DSP_PAN_2D_STEREO_MODE_DISCRETE)
   with Convention => C;  -- fmod_dsp_effects.h:313

   type FMOD_DSP_PAN_MODE_TYPE is 
     (FMOD_DSP_PAN_MODE_MONO,
      FMOD_DSP_PAN_MODE_STEREO,
      FMOD_DSP_PAN_MODE_SURROUND)
   with Convention => C;  -- fmod_dsp_effects.h:321

   type FMOD_DSP_PAN_3D_ROLLOFF_TYPE is 
     (FMOD_DSP_PAN_3D_ROLLOFF_LINEARSQUARED,
      FMOD_DSP_PAN_3D_ROLLOFF_LINEAR,
      FMOD_DSP_PAN_3D_ROLLOFF_INVERSE,
      FMOD_DSP_PAN_3D_ROLLOFF_INVERSETAPERED,
      FMOD_DSP_PAN_3D_ROLLOFF_CUSTOM)
   with Convention => C;  -- fmod_dsp_effects.h:331

   type FMOD_DSP_PAN_3D_EXTENT_MODE_TYPE is 
     (FMOD_DSP_PAN_3D_EXTENT_MODE_AUTO,
      FMOD_DSP_PAN_3D_EXTENT_MODE_USER,
      FMOD_DSP_PAN_3D_EXTENT_MODE_OFF)
   with Convention => C;  -- fmod_dsp_effects.h:339

   type FMOD_DSP_PAN is 
     (FMOD_DSP_PAN_MODE,
      FMOD_DSP_PAN_2D_STEREO_POSITION,
      FMOD_DSP_PAN_2D_DIRECTION,
      FMOD_DSP_PAN_2D_EXTENT,
      FMOD_DSP_PAN_2D_ROTATION,
      FMOD_DSP_PAN_2D_LFE_LEVEL,
      FMOD_DSP_PAN_2D_STEREO_MODE,
      FMOD_DSP_PAN_2D_STEREO_SEPARATION,
      FMOD_DSP_PAN_2D_STEREO_AXIS,
      FMOD_DSP_PAN_ENABLED_SPEAKERS,
      FMOD_DSP_PAN_3D_POSITION,
      FMOD_DSP_PAN_3D_ROLLOFF,
      FMOD_DSP_PAN_3D_MIN_DISTANCE,
      FMOD_DSP_PAN_3D_MAX_DISTANCE,
      FMOD_DSP_PAN_3D_EXTENT_MODE,
      FMOD_DSP_PAN_3D_SOUND_SIZE,
      FMOD_DSP_PAN_3D_MIN_EXTENT,
      FMOD_DSP_PAN_3D_PAN_BLEND,
      FMOD_DSP_PAN_LFE_UPMIX_ENABLED,
      FMOD_DSP_PAN_OVERALL_GAIN,
      FMOD_DSP_PAN_SURROUND_SPEAKER_MODE,
      FMOD_DSP_PAN_2D_HEIGHT_BLEND)
   with Convention => C;  -- fmod_dsp_effects.h:366

   type FMOD_DSP_THREE_EQ_CROSSOVERSLOPE_TYPE is 
     (FMOD_DSP_THREE_EQ_CROSSOVERSLOPE_12DB,
      FMOD_DSP_THREE_EQ_CROSSOVERSLOPE_24DB,
      FMOD_DSP_THREE_EQ_CROSSOVERSLOPE_48DB)
   with Convention => C;  -- fmod_dsp_effects.h:374

   type FMOD_DSP_THREE_EQ is 
     (FMOD_DSP_THREE_EQ_LOWGAIN,
      FMOD_DSP_THREE_EQ_MIDGAIN,
      FMOD_DSP_THREE_EQ_HIGHGAIN,
      FMOD_DSP_THREE_EQ_LOWCROSSOVER,
      FMOD_DSP_THREE_EQ_HIGHCROSSOVER,
      FMOD_DSP_THREE_EQ_CROSSOVERSLOPE)
   with Convention => C;  -- fmod_dsp_effects.h:385

   type FMOD_DSP_FFT_WINDOW is 
     (FMOD_DSP_FFT_WINDOW_RECT,
      FMOD_DSP_FFT_WINDOW_TRIANGLE,
      FMOD_DSP_FFT_WINDOW_HAMMING,
      FMOD_DSP_FFT_WINDOW_HANNING,
      FMOD_DSP_FFT_WINDOW_BLACKMAN,
      FMOD_DSP_FFT_WINDOW_BLACKMANHARRIS)
   with Convention => C;  -- fmod_dsp_effects.h:396

   type FMOD_DSP_FFT is 
     (FMOD_DSP_FFT_WINDOWSIZE,
      FMOD_DSP_FFT_WINDOWTYPE,
      FMOD_DSP_FFT_SPECTRUMDATA,
      FMOD_DSP_FFT_DOMINANT_FREQ)
   with Convention => C;  -- fmod_dsp_effects.h:405

   type FMOD_DSP_ENVELOPEFOLLOWER is 
     (FMOD_DSP_ENVELOPEFOLLOWER_ATTACK,
      FMOD_DSP_ENVELOPEFOLLOWER_RELEASE,
      FMOD_DSP_ENVELOPEFOLLOWER_ENVELOPE,
      FMOD_DSP_ENVELOPEFOLLOWER_USESIDECHAIN)
   with Convention => C;  -- fmod_dsp_effects.h:414

   type FMOD_DSP_CONVOLUTION_REVERB is 
     (FMOD_DSP_CONVOLUTION_REVERB_PARAM_IR,
      FMOD_DSP_CONVOLUTION_REVERB_PARAM_WET,
      FMOD_DSP_CONVOLUTION_REVERB_PARAM_DRY,
      FMOD_DSP_CONVOLUTION_REVERB_PARAM_LINKED)
   with Convention => C;  -- fmod_dsp_effects.h:422

   type FMOD_DSP_CHANNELMIX_OUTPUT is 
     (FMOD_DSP_CHANNELMIX_OUTPUT_DEFAULT,
      FMOD_DSP_CHANNELMIX_OUTPUT_ALLMONO,
      FMOD_DSP_CHANNELMIX_OUTPUT_ALLSTEREO,
      FMOD_DSP_CHANNELMIX_OUTPUT_ALLQUAD,
      FMOD_DSP_CHANNELMIX_OUTPUT_ALL5POINT1,
      FMOD_DSP_CHANNELMIX_OUTPUT_ALL7POINT1,
      FMOD_DSP_CHANNELMIX_OUTPUT_ALLLFE,
      FMOD_DSP_CHANNELMIX_OUTPUT_ALL7POINT1POINT4)
   with Convention => C;  -- fmod_dsp_effects.h:434

   type FMOD_DSP_CHANNELMIX is 
     (FMOD_DSP_CHANNELMIX_OUTPUTGROUPING,
      FMOD_DSP_CHANNELMIX_GAIN_CH0,
      FMOD_DSP_CHANNELMIX_GAIN_CH1,
      FMOD_DSP_CHANNELMIX_GAIN_CH2,
      FMOD_DSP_CHANNELMIX_GAIN_CH3,
      FMOD_DSP_CHANNELMIX_GAIN_CH4,
      FMOD_DSP_CHANNELMIX_GAIN_CH5,
      FMOD_DSP_CHANNELMIX_GAIN_CH6,
      FMOD_DSP_CHANNELMIX_GAIN_CH7,
      FMOD_DSP_CHANNELMIX_GAIN_CH8,
      FMOD_DSP_CHANNELMIX_GAIN_CH9,
      FMOD_DSP_CHANNELMIX_GAIN_CH10,
      FMOD_DSP_CHANNELMIX_GAIN_CH11,
      FMOD_DSP_CHANNELMIX_GAIN_CH12,
      FMOD_DSP_CHANNELMIX_GAIN_CH13,
      FMOD_DSP_CHANNELMIX_GAIN_CH14,
      FMOD_DSP_CHANNELMIX_GAIN_CH15,
      FMOD_DSP_CHANNELMIX_GAIN_CH16,
      FMOD_DSP_CHANNELMIX_GAIN_CH17,
      FMOD_DSP_CHANNELMIX_GAIN_CH18,
      FMOD_DSP_CHANNELMIX_GAIN_CH19,
      FMOD_DSP_CHANNELMIX_GAIN_CH20,
      FMOD_DSP_CHANNELMIX_GAIN_CH21,
      FMOD_DSP_CHANNELMIX_GAIN_CH22,
      FMOD_DSP_CHANNELMIX_GAIN_CH23,
      FMOD_DSP_CHANNELMIX_GAIN_CH24,
      FMOD_DSP_CHANNELMIX_GAIN_CH25,
      FMOD_DSP_CHANNELMIX_GAIN_CH26,
      FMOD_DSP_CHANNELMIX_GAIN_CH27,
      FMOD_DSP_CHANNELMIX_GAIN_CH28,
      FMOD_DSP_CHANNELMIX_GAIN_CH29,
      FMOD_DSP_CHANNELMIX_GAIN_CH30,
      FMOD_DSP_CHANNELMIX_GAIN_CH31,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH0,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH1,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH2,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH3,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH4,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH5,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH6,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH7,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH8,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH9,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH10,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH11,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH12,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH13,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH14,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH15,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH16,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH17,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH18,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH19,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH20,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH21,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH22,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH23,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH24,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH25,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH26,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH27,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH28,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH29,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH30,
      FMOD_DSP_CHANNELMIX_OUTPUT_CH31)
   with Convention => C;  -- fmod_dsp_effects.h:503

   subtype FMOD_DSP_TRANSCEIVER_SPEAKERMODE is int;
   FMOD_DSP_TRANSCEIVER_SPEAKERMODE_AUTO : constant int := -1;
   FMOD_DSP_TRANSCEIVER_SPEAKERMODE_MONO : constant int := 0;
   FMOD_DSP_TRANSCEIVER_SPEAKERMODE_STEREO : constant int := 1;
   FMOD_DSP_TRANSCEIVER_SPEAKERMODE_SURROUND : constant int := 2;  -- fmod_dsp_effects.h:511

   type FMOD_DSP_TRANSCEIVER is 
     (FMOD_DSP_TRANSCEIVER_TRANSMIT,
      FMOD_DSP_TRANSCEIVER_GAIN,
      FMOD_DSP_TRANSCEIVER_CHANNEL,
      FMOD_DSP_TRANSCEIVER_TRANSMITSPEAKERMODE)
   with Convention => C;  -- fmod_dsp_effects.h:520

   type FMOD_DSP_OBJECTPAN is 
     (FMOD_DSP_OBJECTPAN_3D_POSITION,
      FMOD_DSP_OBJECTPAN_3D_ROLLOFF,
      FMOD_DSP_OBJECTPAN_3D_MIN_DISTANCE,
      FMOD_DSP_OBJECTPAN_3D_MAX_DISTANCE,
      FMOD_DSP_OBJECTPAN_3D_EXTENT_MODE,
      FMOD_DSP_OBJECTPAN_3D_SOUND_SIZE,
      FMOD_DSP_OBJECTPAN_3D_MIN_EXTENT,
      FMOD_DSP_OBJECTPAN_OVERALL_GAIN,
      FMOD_DSP_OBJECTPAN_OUTPUTGAIN)
   with Convention => C;  -- fmod_dsp_effects.h:534

end fmod_dsp_effects_h;
