pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

package fmod_common_h is

   --  unsupported macro: F_EXPORT __attribute__((visibility("default")))
   --  unsupported macro: F_API F_CALL
   --  unsupported macro: F_CALLBACK F_CALL
   FMOD_VERSION : constant := 16#00020109#;  --  fmod_common.h:61

   FMOD_DEBUG_LEVEL_NONE : constant := 16#00000000#;  --  fmod_common.h:64
   FMOD_DEBUG_LEVEL_ERROR : constant := 16#00000001#;  --  fmod_common.h:65
   FMOD_DEBUG_LEVEL_WARNING : constant := 16#00000002#;  --  fmod_common.h:66
   FMOD_DEBUG_LEVEL_LOG : constant := 16#00000004#;  --  fmod_common.h:67
   FMOD_DEBUG_TYPE_MEMORY : constant := 16#00000100#;  --  fmod_common.h:68
   FMOD_DEBUG_TYPE_FILE : constant := 16#00000200#;  --  fmod_common.h:69
   FMOD_DEBUG_TYPE_CODEC : constant := 16#00000400#;  --  fmod_common.h:70
   FMOD_DEBUG_TYPE_TRACE : constant := 16#00000800#;  --  fmod_common.h:71
   FMOD_DEBUG_DISPLAY_TIMESTAMPS : constant := 16#00010000#;  --  fmod_common.h:72
   FMOD_DEBUG_DISPLAY_LINENUMBERS : constant := 16#00020000#;  --  fmod_common.h:73
   FMOD_DEBUG_DISPLAY_THREAD : constant := 16#00040000#;  --  fmod_common.h:74

   FMOD_MEMORY_NORMAL : constant := 16#00000000#;  --  fmod_common.h:77
   FMOD_MEMORY_STREAM_FILE : constant := 16#00000001#;  --  fmod_common.h:78
   FMOD_MEMORY_STREAM_DECODE : constant := 16#00000002#;  --  fmod_common.h:79
   FMOD_MEMORY_SAMPLEDATA : constant := 16#00000004#;  --  fmod_common.h:80
   FMOD_MEMORY_DSP_BUFFER : constant := 16#00000008#;  --  fmod_common.h:81
   FMOD_MEMORY_PLUGIN : constant := 16#00000010#;  --  fmod_common.h:82
   FMOD_MEMORY_PERSISTENT : constant := 16#00200000#;  --  fmod_common.h:83
   FMOD_MEMORY_ALL : constant := 16#FFFFFFFF#;  --  fmod_common.h:84

   FMOD_INIT_NORMAL : constant := 16#00000000#;  --  fmod_common.h:87
   FMOD_INIT_STREAM_FROM_UPDATE : constant := 16#00000001#;  --  fmod_common.h:88
   FMOD_INIT_MIX_FROM_UPDATE : constant := 16#00000002#;  --  fmod_common.h:89
   FMOD_INIT_3D_RIGHTHANDED : constant := 16#00000004#;  --  fmod_common.h:90
   FMOD_INIT_CHANNEL_LOWPASS : constant := 16#00000100#;  --  fmod_common.h:91
   FMOD_INIT_CHANNEL_DISTANCEFILTER : constant := 16#00000200#;  --  fmod_common.h:92
   FMOD_INIT_PROFILE_ENABLE : constant := 16#00010000#;  --  fmod_common.h:93
   FMOD_INIT_VOL0_BECOMES_VIRTUAL : constant := 16#00020000#;  --  fmod_common.h:94
   FMOD_INIT_GEOMETRY_USECLOSEST : constant := 16#00040000#;  --  fmod_common.h:95
   FMOD_INIT_PREFER_DOLBY_DOWNMIX : constant := 16#00080000#;  --  fmod_common.h:96
   FMOD_INIT_THREAD_UNSAFE : constant := 16#00100000#;  --  fmod_common.h:97
   FMOD_INIT_PROFILE_METER_ALL : constant := 16#00200000#;  --  fmod_common.h:98
   FMOD_INIT_MEMORY_TRACKING : constant := 16#00400000#;  --  fmod_common.h:99

   FMOD_DRIVER_STATE_CONNECTED : constant := 16#00000001#;  --  fmod_common.h:102
   FMOD_DRIVER_STATE_DEFAULT : constant := 16#00000002#;  --  fmod_common.h:103

   FMOD_TIMEUNIT_MS : constant := 16#00000001#;  --  fmod_common.h:106
   FMOD_TIMEUNIT_PCM : constant := 16#00000002#;  --  fmod_common.h:107
   FMOD_TIMEUNIT_PCMBYTES : constant := 16#00000004#;  --  fmod_common.h:108
   FMOD_TIMEUNIT_RAWBYTES : constant := 16#00000008#;  --  fmod_common.h:109
   FMOD_TIMEUNIT_PCMFRACTION : constant := 16#00000010#;  --  fmod_common.h:110
   FMOD_TIMEUNIT_MODORDER : constant := 16#00000100#;  --  fmod_common.h:111
   FMOD_TIMEUNIT_MODROW : constant := 16#00000200#;  --  fmod_common.h:112
   FMOD_TIMEUNIT_MODPATTERN : constant := 16#00000400#;  --  fmod_common.h:113

   FMOD_SYSTEM_CALLBACK_DEVICELISTCHANGED : constant := 16#00000001#;  --  fmod_common.h:116
   FMOD_SYSTEM_CALLBACK_DEVICELOST : constant := 16#00000002#;  --  fmod_common.h:117
   FMOD_SYSTEM_CALLBACK_MEMORYALLOCATIONFAILED : constant := 16#00000004#;  --  fmod_common.h:118
   FMOD_SYSTEM_CALLBACK_THREADCREATED : constant := 16#00000008#;  --  fmod_common.h:119
   FMOD_SYSTEM_CALLBACK_BADDSPCONNECTION : constant := 16#00000010#;  --  fmod_common.h:120
   FMOD_SYSTEM_CALLBACK_PREMIX : constant := 16#00000020#;  --  fmod_common.h:121
   FMOD_SYSTEM_CALLBACK_POSTMIX : constant := 16#00000040#;  --  fmod_common.h:122
   FMOD_SYSTEM_CALLBACK_ERROR : constant := 16#00000080#;  --  fmod_common.h:123
   FMOD_SYSTEM_CALLBACK_MIDMIX : constant := 16#00000100#;  --  fmod_common.h:124
   FMOD_SYSTEM_CALLBACK_THREADDESTROYED : constant := 16#00000200#;  --  fmod_common.h:125
   FMOD_SYSTEM_CALLBACK_PREUPDATE : constant := 16#00000400#;  --  fmod_common.h:126
   FMOD_SYSTEM_CALLBACK_POSTUPDATE : constant := 16#00000800#;  --  fmod_common.h:127
   FMOD_SYSTEM_CALLBACK_RECORDLISTCHANGED : constant := 16#00001000#;  --  fmod_common.h:128
   FMOD_SYSTEM_CALLBACK_BUFFEREDNOMIX : constant := 16#00002000#;  --  fmod_common.h:129
   FMOD_SYSTEM_CALLBACK_DEVICEREINITIALIZE : constant := 16#00004000#;  --  fmod_common.h:130
   FMOD_SYSTEM_CALLBACK_OUTPUTUNDERRUN : constant := 16#00008000#;  --  fmod_common.h:131
   FMOD_SYSTEM_CALLBACK_ALL : constant := 16#FFFFFFFF#;  --  fmod_common.h:132

   FMOD_DEFAULT : constant := 16#00000000#;  --  fmod_common.h:135
   FMOD_LOOP_OFF : constant := 16#00000001#;  --  fmod_common.h:136
   FMOD_LOOP_NORMAL : constant := 16#00000002#;  --  fmod_common.h:137
   FMOD_LOOP_BIDI : constant := 16#00000004#;  --  fmod_common.h:138
   FMOD_2D : constant := 16#00000008#;  --  fmod_common.h:139
   FMOD_3D : constant := 16#00000010#;  --  fmod_common.h:140
   FMOD_CREATESTREAM : constant := 16#00000080#;  --  fmod_common.h:141
   FMOD_CREATESAMPLE : constant := 16#00000100#;  --  fmod_common.h:142
   FMOD_CREATECOMPRESSEDSAMPLE : constant := 16#00000200#;  --  fmod_common.h:143
   FMOD_OPENUSER : constant := 16#00000400#;  --  fmod_common.h:144
   FMOD_OPENMEMORY : constant := 16#00000800#;  --  fmod_common.h:145
   FMOD_OPENMEMORY_POINT : constant := 16#10000000#;  --  fmod_common.h:146
   FMOD_OPENRAW : constant := 16#00001000#;  --  fmod_common.h:147
   FMOD_OPENONLY : constant := 16#00002000#;  --  fmod_common.h:148
   FMOD_ACCURATETIME : constant := 16#00004000#;  --  fmod_common.h:149
   FMOD_MPEGSEARCH : constant := 16#00008000#;  --  fmod_common.h:150
   FMOD_NONBLOCKING : constant := 16#00010000#;  --  fmod_common.h:151
   FMOD_UNIQUE : constant := 16#00020000#;  --  fmod_common.h:152
   FMOD_3D_HEADRELATIVE : constant := 16#00040000#;  --  fmod_common.h:153
   FMOD_3D_WORLDRELATIVE : constant := 16#00080000#;  --  fmod_common.h:154
   FMOD_3D_INVERSEROLLOFF : constant := 16#00100000#;  --  fmod_common.h:155
   FMOD_3D_LINEARROLLOFF : constant := 16#00200000#;  --  fmod_common.h:156
   FMOD_3D_LINEARSQUAREROLLOFF : constant := 16#00400000#;  --  fmod_common.h:157
   FMOD_3D_INVERSETAPEREDROLLOFF : constant := 16#00800000#;  --  fmod_common.h:158
   FMOD_3D_CUSTOMROLLOFF : constant := 16#04000000#;  --  fmod_common.h:159
   FMOD_3D_IGNOREGEOMETRY : constant := 16#40000000#;  --  fmod_common.h:160
   FMOD_IGNORETAGS : constant := 16#02000000#;  --  fmod_common.h:161
   FMOD_LOWMEM : constant := 16#08000000#;  --  fmod_common.h:162
   FMOD_VIRTUAL_PLAYFROMSTART : constant := 16#80000000#;  --  fmod_common.h:163

   FMOD_CHANNELMASK_FRONT_LEFT : constant := 16#00000001#;  --  fmod_common.h:166
   FMOD_CHANNELMASK_FRONT_RIGHT : constant := 16#00000002#;  --  fmod_common.h:167
   FMOD_CHANNELMASK_FRONT_CENTER : constant := 16#00000004#;  --  fmod_common.h:168
   FMOD_CHANNELMASK_LOW_FREQUENCY : constant := 16#00000008#;  --  fmod_common.h:169
   FMOD_CHANNELMASK_SURROUND_LEFT : constant := 16#00000010#;  --  fmod_common.h:170
   FMOD_CHANNELMASK_SURROUND_RIGHT : constant := 16#00000020#;  --  fmod_common.h:171
   FMOD_CHANNELMASK_BACK_LEFT : constant := 16#00000040#;  --  fmod_common.h:172
   FMOD_CHANNELMASK_BACK_RIGHT : constant := 16#00000080#;  --  fmod_common.h:173
   FMOD_CHANNELMASK_BACK_CENTER : constant := 16#00000100#;  --  fmod_common.h:174
   --  unsupported macro: FMOD_CHANNELMASK_MONO (FMOD_CHANNELMASK_FRONT_LEFT)
   --  unsupported macro: FMOD_CHANNELMASK_STEREO (FMOD_CHANNELMASK_FRONT_LEFT | FMOD_CHANNELMASK_FRONT_RIGHT)
   --  unsupported macro: FMOD_CHANNELMASK_LRC (FMOD_CHANNELMASK_FRONT_LEFT | FMOD_CHANNELMASK_FRONT_RIGHT | FMOD_CHANNELMASK_FRONT_CENTER)
   --  unsupported macro: FMOD_CHANNELMASK_QUAD (FMOD_CHANNELMASK_FRONT_LEFT | FMOD_CHANNELMASK_FRONT_RIGHT | FMOD_CHANNELMASK_SURROUND_LEFT | FMOD_CHANNELMASK_SURROUND_RIGHT)
   --  unsupported macro: FMOD_CHANNELMASK_SURROUND (FMOD_CHANNELMASK_FRONT_LEFT | FMOD_CHANNELMASK_FRONT_RIGHT | FMOD_CHANNELMASK_FRONT_CENTER | FMOD_CHANNELMASK_SURROUND_LEFT | FMOD_CHANNELMASK_SURROUND_RIGHT)
   --  unsupported macro: FMOD_CHANNELMASK_5POINT1 (FMOD_CHANNELMASK_FRONT_LEFT | FMOD_CHANNELMASK_FRONT_RIGHT | FMOD_CHANNELMASK_FRONT_CENTER | FMOD_CHANNELMASK_LOW_FREQUENCY | FMOD_CHANNELMASK_SURROUND_LEFT | FMOD_CHANNELMASK_SURROUND_RIGHT)
   --  unsupported macro: FMOD_CHANNELMASK_5POINT1_REARS (FMOD_CHANNELMASK_FRONT_LEFT | FMOD_CHANNELMASK_FRONT_RIGHT | FMOD_CHANNELMASK_FRONT_CENTER | FMOD_CHANNELMASK_LOW_FREQUENCY | FMOD_CHANNELMASK_BACK_LEFT | FMOD_CHANNELMASK_BACK_RIGHT)
   --  unsupported macro: FMOD_CHANNELMASK_7POINT0 (FMOD_CHANNELMASK_FRONT_LEFT | FMOD_CHANNELMASK_FRONT_RIGHT | FMOD_CHANNELMASK_FRONT_CENTER | FMOD_CHANNELMASK_SURROUND_LEFT | FMOD_CHANNELMASK_SURROUND_RIGHT | FMOD_CHANNELMASK_BACK_LEFT | FMOD_CHANNELMASK_BACK_RIGHT)
   --  unsupported macro: FMOD_CHANNELMASK_7POINT1 (FMOD_CHANNELMASK_FRONT_LEFT | FMOD_CHANNELMASK_FRONT_RIGHT | FMOD_CHANNELMASK_FRONT_CENTER | FMOD_CHANNELMASK_LOW_FREQUENCY | FMOD_CHANNELMASK_SURROUND_LEFT | FMOD_CHANNELMASK_SURROUND_RIGHT | FMOD_CHANNELMASK_BACK_LEFT | FMOD_CHANNELMASK_BACK_RIGHT)

   FMOD_THREAD_PRIORITY_PLATFORM_MIN : constant := (-32 * 1024);  --  fmod_common.h:187
   FMOD_THREAD_PRIORITY_PLATFORM_MAX : constant := ( 32 * 1024);  --  fmod_common.h:188
   --  unsupported macro: FMOD_THREAD_PRIORITY_DEFAULT (FMOD_THREAD_PRIORITY_PLATFORM_MIN - 1)
   --  unsupported macro: FMOD_THREAD_PRIORITY_LOW (FMOD_THREAD_PRIORITY_PLATFORM_MIN - 2)
   --  unsupported macro: FMOD_THREAD_PRIORITY_MEDIUM (FMOD_THREAD_PRIORITY_PLATFORM_MIN - 3)
   --  unsupported macro: FMOD_THREAD_PRIORITY_HIGH (FMOD_THREAD_PRIORITY_PLATFORM_MIN - 4)
   --  unsupported macro: FMOD_THREAD_PRIORITY_VERY_HIGH (FMOD_THREAD_PRIORITY_PLATFORM_MIN - 5)
   --  unsupported macro: FMOD_THREAD_PRIORITY_EXTREME (FMOD_THREAD_PRIORITY_PLATFORM_MIN - 6)
   --  unsupported macro: FMOD_THREAD_PRIORITY_CRITICAL (FMOD_THREAD_PRIORITY_PLATFORM_MIN - 7)
   --  unsupported macro: FMOD_THREAD_PRIORITY_MIXER FMOD_THREAD_PRIORITY_EXTREME
   --  unsupported macro: FMOD_THREAD_PRIORITY_FEEDER FMOD_THREAD_PRIORITY_CRITICAL
   --  unsupported macro: FMOD_THREAD_PRIORITY_STREAM FMOD_THREAD_PRIORITY_VERY_HIGH
   --  unsupported macro: FMOD_THREAD_PRIORITY_FILE FMOD_THREAD_PRIORITY_HIGH
   --  unsupported macro: FMOD_THREAD_PRIORITY_NONBLOCKING FMOD_THREAD_PRIORITY_HIGH
   --  unsupported macro: FMOD_THREAD_PRIORITY_RECORD FMOD_THREAD_PRIORITY_HIGH
   --  unsupported macro: FMOD_THREAD_PRIORITY_GEOMETRY FMOD_THREAD_PRIORITY_LOW
   --  unsupported macro: FMOD_THREAD_PRIORITY_PROFILER FMOD_THREAD_PRIORITY_MEDIUM
   --  unsupported macro: FMOD_THREAD_PRIORITY_STUDIO_UPDATE FMOD_THREAD_PRIORITY_MEDIUM
   --  unsupported macro: FMOD_THREAD_PRIORITY_STUDIO_LOAD_BANK FMOD_THREAD_PRIORITY_MEDIUM
   --  unsupported macro: FMOD_THREAD_PRIORITY_STUDIO_LOAD_SAMPLE FMOD_THREAD_PRIORITY_MEDIUM
   --  unsupported macro: FMOD_THREAD_PRIORITY_CONVOLUTION1 FMOD_THREAD_PRIORITY_VERY_HIGH
   --  unsupported macro: FMOD_THREAD_PRIORITY_CONVOLUTION2 FMOD_THREAD_PRIORITY_VERY_HIGH

   FMOD_THREAD_STACK_SIZE_DEFAULT : constant := 0;  --  fmod_common.h:213
   FMOD_THREAD_STACK_SIZE_MIXER : constant := (80 * 1024);  --  fmod_common.h:214
   FMOD_THREAD_STACK_SIZE_FEEDER : constant := (16 * 1024);  --  fmod_common.h:215
   FMOD_THREAD_STACK_SIZE_STREAM : constant := (96 * 1024);  --  fmod_common.h:216
   FMOD_THREAD_STACK_SIZE_FILE : constant := (48 * 1024);  --  fmod_common.h:217
   FMOD_THREAD_STACK_SIZE_NONBLOCKING : constant := (112 * 1024);  --  fmod_common.h:218
   FMOD_THREAD_STACK_SIZE_RECORD : constant := (16 * 1024);  --  fmod_common.h:219
   FMOD_THREAD_STACK_SIZE_GEOMETRY : constant := (48 * 1024);  --  fmod_common.h:220
   FMOD_THREAD_STACK_SIZE_PROFILER : constant := (128 * 1024);  --  fmod_common.h:221
   FMOD_THREAD_STACK_SIZE_STUDIO_UPDATE : constant := (96 * 1024);  --  fmod_common.h:222
   FMOD_THREAD_STACK_SIZE_STUDIO_LOAD_BANK : constant := (96 * 1024);  --  fmod_common.h:223
   FMOD_THREAD_STACK_SIZE_STUDIO_LOAD_SAMPLE : constant := (96 * 1024);  --  fmod_common.h:224
   FMOD_THREAD_STACK_SIZE_CONVOLUTION1 : constant := (16 * 1024);  --  fmod_common.h:225
   FMOD_THREAD_STACK_SIZE_CONVOLUTION2 : constant := (16 * 1024);  --  fmod_common.h:226

   FMOD_THREAD_AFFINITY_GROUP_DEFAULT : constant := 16#8000000000000000#;  --  fmod_common.h:230
   FMOD_THREAD_AFFINITY_GROUP_A : constant := 16#8000000000000001#;  --  fmod_common.h:231
   FMOD_THREAD_AFFINITY_GROUP_B : constant := 16#8000000000000002#;  --  fmod_common.h:232
   FMOD_THREAD_AFFINITY_GROUP_C : constant := 16#8000000000000003#;  --  fmod_common.h:233
   --  unsupported macro: FMOD_THREAD_AFFINITY_MIXER FMOD_THREAD_AFFINITY_GROUP_A
   --  unsupported macro: FMOD_THREAD_AFFINITY_FEEDER FMOD_THREAD_AFFINITY_GROUP_C
   --  unsupported macro: FMOD_THREAD_AFFINITY_STREAM FMOD_THREAD_AFFINITY_GROUP_C
   --  unsupported macro: FMOD_THREAD_AFFINITY_FILE FMOD_THREAD_AFFINITY_GROUP_C
   --  unsupported macro: FMOD_THREAD_AFFINITY_NONBLOCKING FMOD_THREAD_AFFINITY_GROUP_C
   --  unsupported macro: FMOD_THREAD_AFFINITY_RECORD FMOD_THREAD_AFFINITY_GROUP_C
   --  unsupported macro: FMOD_THREAD_AFFINITY_GEOMETRY FMOD_THREAD_AFFINITY_GROUP_C
   --  unsupported macro: FMOD_THREAD_AFFINITY_PROFILER FMOD_THREAD_AFFINITY_GROUP_C
   --  unsupported macro: FMOD_THREAD_AFFINITY_STUDIO_UPDATE FMOD_THREAD_AFFINITY_GROUP_B
   --  unsupported macro: FMOD_THREAD_AFFINITY_STUDIO_LOAD_BANK FMOD_THREAD_AFFINITY_GROUP_C
   --  unsupported macro: FMOD_THREAD_AFFINITY_STUDIO_LOAD_SAMPLE FMOD_THREAD_AFFINITY_GROUP_C
   --  unsupported macro: FMOD_THREAD_AFFINITY_CONVOLUTION1 FMOD_THREAD_AFFINITY_GROUP_C
   --  unsupported macro: FMOD_THREAD_AFFINITY_CONVOLUTION2 FMOD_THREAD_AFFINITY_GROUP_C

   FMOD_THREAD_AFFINITY_CORE_ALL : constant := 0;  --  fmod_common.h:250
   FMOD_THREAD_AFFINITY_CORE_0 : constant := (2 ** 0);  --  fmod_common.h:251
   FMOD_THREAD_AFFINITY_CORE_1 : constant := (2 ** 1);  --  fmod_common.h:252
   FMOD_THREAD_AFFINITY_CORE_2 : constant := (2 ** 2);  --  fmod_common.h:253
   FMOD_THREAD_AFFINITY_CORE_3 : constant := (2 ** 3);  --  fmod_common.h:254
   FMOD_THREAD_AFFINITY_CORE_4 : constant := (2 ** 4);  --  fmod_common.h:255
   FMOD_THREAD_AFFINITY_CORE_5 : constant := (2 ** 5);  --  fmod_common.h:256
   FMOD_THREAD_AFFINITY_CORE_6 : constant := (2 ** 6);  --  fmod_common.h:257
   FMOD_THREAD_AFFINITY_CORE_7 : constant := (2 ** 7);  --  fmod_common.h:258
   FMOD_THREAD_AFFINITY_CORE_8 : constant := (2 ** 8);  --  fmod_common.h:259
   FMOD_THREAD_AFFINITY_CORE_9 : constant := (2 ** 9);  --  fmod_common.h:260
   FMOD_THREAD_AFFINITY_CORE_10 : constant := (2 ** 10);  --  fmod_common.h:261
   FMOD_THREAD_AFFINITY_CORE_11 : constant := (2 ** 11);  --  fmod_common.h:262
   FMOD_THREAD_AFFINITY_CORE_12 : constant := (2 ** 12);  --  fmod_common.h:263
   FMOD_THREAD_AFFINITY_CORE_13 : constant := (2 ** 13);  --  fmod_common.h:264
   FMOD_THREAD_AFFINITY_CORE_14 : constant := (2 ** 14);  --  fmod_common.h:265
   FMOD_THREAD_AFFINITY_CORE_15 : constant := (2 ** 15);  --  fmod_common.h:266
   --  unsupported macro: FMOD_PRESET_OFF { 1000, 7, 11, 5000, 100, 100, 100, 250, 0, 20, 96, -80.0f }
   --  unsupported macro: FMOD_PRESET_GENERIC { 1500, 7, 11, 5000, 83, 100, 100, 250, 0, 14500, 96, -8.0f }
   --  unsupported macro: FMOD_PRESET_PADDEDCELL { 170, 1, 2, 5000, 10, 100, 100, 250, 0, 160, 84, -7.8f }
   --  unsupported macro: FMOD_PRESET_ROOM { 400, 2, 3, 5000, 83, 100, 100, 250, 0, 6050, 88, -9.4f }
   --  unsupported macro: FMOD_PRESET_BATHROOM { 1500, 7, 11, 5000, 54, 100, 60, 250, 0, 2900, 83, 0.5f }
   --  unsupported macro: FMOD_PRESET_LIVINGROOM { 500, 3, 4, 5000, 10, 100, 100, 250, 0, 160, 58, -19.0f }
   --  unsupported macro: FMOD_PRESET_STONEROOM { 2300, 12, 17, 5000, 64, 100, 100, 250, 0, 7800, 71, -8.5f }
   --  unsupported macro: FMOD_PRESET_AUDITORIUM { 4300, 20, 30, 5000, 59, 100, 100, 250, 0, 5850, 64, -11.7f }
   --  unsupported macro: FMOD_PRESET_CONCERTHALL { 3900, 20, 29, 5000, 70, 100, 100, 250, 0, 5650, 80, -9.8f }
   --  unsupported macro: FMOD_PRESET_CAVE { 2900, 15, 22, 5000, 100, 100, 100, 250, 0, 20000, 59, -11.3f }
   --  unsupported macro: FMOD_PRESET_ARENA { 7200, 20, 30, 5000, 33, 100, 100, 250, 0, 4500, 80, -9.6f }
   --  unsupported macro: FMOD_PRESET_HANGAR { 10000, 20, 30, 5000, 23, 100, 100, 250, 0, 3400, 72, -7.4f }
   --  unsupported macro: FMOD_PRESET_CARPETTEDHALLWAY { 300, 2, 30, 5000, 10, 100, 100, 250, 0, 500, 56, -24.0f }
   --  unsupported macro: FMOD_PRESET_HALLWAY { 1500, 7, 11, 5000, 59, 100, 100, 250, 0, 7800, 87, -5.5f }
   --  unsupported macro: FMOD_PRESET_STONECORRIDOR { 270, 13, 20, 5000, 79, 100, 100, 250, 0, 9000, 86, -6.0f }
   --  unsupported macro: FMOD_PRESET_ALLEY { 1500, 7, 11, 5000, 86, 100, 100, 250, 0, 8300, 80, -9.8f }
   --  unsupported macro: FMOD_PRESET_FOREST { 1500, 162, 88, 5000, 54, 79, 100, 250, 0, 760, 94, -12.3f }
   --  unsupported macro: FMOD_PRESET_CITY { 1500, 7, 11, 5000, 67, 50, 100, 250, 0, 4050, 66, -26.0f }
   --  unsupported macro: FMOD_PRESET_MOUNTAINS { 1500, 300, 100, 5000, 21, 27, 100, 250, 0, 1220, 82, -24.0f }
   --  unsupported macro: FMOD_PRESET_QUARRY { 1500, 61, 25, 5000, 83, 100, 100, 250, 0, 3400, 100, -5.0f }
   --  unsupported macro: FMOD_PRESET_PLAIN { 1500, 179, 100, 5000, 50, 21, 100, 250, 0, 1670, 65, -28.0f }
   --  unsupported macro: FMOD_PRESET_PARKINGLOT { 1700, 8, 12, 5000, 100, 100, 100, 250, 0, 20000, 56, -19.5f }
   --  unsupported macro: FMOD_PRESET_SEWERPIPE { 2800, 14, 21, 5000, 14, 80, 60, 250, 0, 3400, 66, 1.2f }
   --  unsupported macro: FMOD_PRESET_UNDERWATER { 1500, 7, 11, 5000, 10, 100, 100, 250, 0, 500, 92, 7.0f }

   FMOD_MAX_CHANNEL_WIDTH : constant := 32;  --  fmod_common.h:294
   FMOD_MAX_SYSTEMS : constant := 8;  --  fmod_common.h:295
   FMOD_MAX_LISTENERS : constant := 8;  --  fmod_common.h:296
   FMOD_REVERB_MAXINSTANCES : constant := 4;  --  fmod_common.h:297
   FMOD_PORT_INDEX_NONE : constant := 16#FFFFFFFFFFFFFFFF#;  --  fmod_common.h:298

  -- ========================================================================================  
  -- FMOD Core API - Common C/C++ header file.                                                 
  -- Copyright (c), Firelight Technologies Pty, Ltd. 2004-2021.                                
  --                                                                                           
  -- This header is included by fmod.hpp (C++ interface) and fmod.h (C interface)              
  --                                                                                           
  -- For more detail visit:                                                                    
  -- https://fmod.com/resources/documentation-api?version=2.0&page=core-api-common.html        
  -- ========================================================================================  
  --    Library import helpers
  -- 

  --    FMOD core types
  -- 

   subtype FMOD_BOOL is int;  -- fmod_common.h:41

   type FMOD_SYSTEM is null record;   -- incomplete struct

   type FMOD_SOUND is null record;   -- incomplete struct

   type FMOD_CHANNELCONTROL is null record;   -- incomplete struct

   type FMOD_CHANNEL is null record;   -- incomplete struct

   type FMOD_CHANNELGROUP is null record;   -- incomplete struct

   type FMOD_SOUNDGROUP is null record;   -- incomplete struct

   type FMOD_REVERB3D is null record;   -- incomplete struct

   type FMOD_DSP is null record;   -- incomplete struct

   type FMOD_DSPCONNECTION is null record;   -- incomplete struct

   type FMOD_POLYGON is null record;   -- incomplete struct

   type FMOD_GEOMETRY is null record;   -- incomplete struct

   type FMOD_SYNCPOINT is null record;   -- incomplete struct

   type FMOD_ASYNCREADINFO;
   subtype FMOD_PORT_TYPE is unsigned;  -- fmod_common.h:55

   subtype FMOD_PORT_INDEX is Extensions.unsigned_long_long;  -- fmod_common.h:56

  --    FMOD constants
  -- 

   subtype FMOD_DEBUG_FLAGS is unsigned;  -- fmod_common.h:63

   subtype FMOD_MEMORY_TYPE is unsigned;  -- fmod_common.h:76

   subtype FMOD_INITFLAGS is unsigned;  -- fmod_common.h:86

   subtype FMOD_DRIVER_STATE is unsigned;  -- fmod_common.h:101

   subtype FMOD_TIMEUNIT is unsigned;  -- fmod_common.h:105

   subtype FMOD_SYSTEM_CALLBACK_TYPE is unsigned;  -- fmod_common.h:115

   subtype FMOD_MODE is unsigned;  -- fmod_common.h:134

   subtype FMOD_CHANNELMASK is unsigned;  -- fmod_common.h:165

   subtype FMOD_THREAD_PRIORITY is int;  -- fmod_common.h:185

  -- Platform specific priority range  
  -- Platform agnostic priorities, maps internally to platform specific value  
  -- Thread defaults  
   subtype FMOD_THREAD_STACK_SIZE is unsigned;  -- fmod_common.h:212

   subtype FMOD_THREAD_AFFINITY is Extensions.unsigned_long_long;  -- fmod_common.h:228

  -- Platform agnostic thread groupings  
  -- Thread defaults  
  -- Core mask, valid up to 1 << 62  
  -- Preset for FMOD_REVERB_PROPERTIES  
   subtype FMOD_THREAD_TYPE is unsigned;
   FMOD_THREAD_TYPE_MIXER : constant unsigned := 0;
   FMOD_THREAD_TYPE_FEEDER : constant unsigned := 1;
   FMOD_THREAD_TYPE_STREAM : constant unsigned := 2;
   FMOD_THREAD_TYPE_FILE : constant unsigned := 3;
   FMOD_THREAD_TYPE_NONBLOCKING : constant unsigned := 4;
   FMOD_THREAD_TYPE_RECORD : constant unsigned := 5;
   FMOD_THREAD_TYPE_GEOMETRY : constant unsigned := 6;
   FMOD_THREAD_TYPE_PROFILER : constant unsigned := 7;
   FMOD_THREAD_TYPE_STUDIO_UPDATE : constant unsigned := 8;
   FMOD_THREAD_TYPE_STUDIO_LOAD_BANK : constant unsigned := 9;
   FMOD_THREAD_TYPE_STUDIO_LOAD_SAMPLE : constant unsigned := 10;
   FMOD_THREAD_TYPE_CONVOLUTION1 : constant unsigned := 11;
   FMOD_THREAD_TYPE_CONVOLUTION2 : constant unsigned := 12;
   FMOD_THREAD_TYPE_MAX : constant unsigned := 13;
   FMOD_THREAD_TYPE_FORCEINT : constant unsigned := 65536;  -- fmod_common.h:300

   subtype FMOD_RESULT is unsigned;
   FMOD_OK : constant unsigned := 0;
   FMOD_ERR_BADCOMMAND : constant unsigned := 1;
   FMOD_ERR_CHANNEL_ALLOC : constant unsigned := 2;
   FMOD_ERR_CHANNEL_STOLEN : constant unsigned := 3;
   FMOD_ERR_DMA : constant unsigned := 4;
   FMOD_ERR_DSP_CONNECTION : constant unsigned := 5;
   FMOD_ERR_DSP_DONTPROCESS : constant unsigned := 6;
   FMOD_ERR_DSP_FORMAT : constant unsigned := 7;
   FMOD_ERR_DSP_INUSE : constant unsigned := 8;
   FMOD_ERR_DSP_NOTFOUND : constant unsigned := 9;
   FMOD_ERR_DSP_RESERVED : constant unsigned := 10;
   FMOD_ERR_DSP_SILENCE : constant unsigned := 11;
   FMOD_ERR_DSP_TYPE : constant unsigned := 12;
   FMOD_ERR_FILE_BAD : constant unsigned := 13;
   FMOD_ERR_FILE_COULDNOTSEEK : constant unsigned := 14;
   FMOD_ERR_FILE_DISKEJECTED : constant unsigned := 15;
   FMOD_ERR_FILE_EOF : constant unsigned := 16;
   FMOD_ERR_FILE_ENDOFDATA : constant unsigned := 17;
   FMOD_ERR_FILE_NOTFOUND : constant unsigned := 18;
   FMOD_ERR_FORMAT : constant unsigned := 19;
   FMOD_ERR_HEADER_MISMATCH : constant unsigned := 20;
   FMOD_ERR_HTTP : constant unsigned := 21;
   FMOD_ERR_HTTP_ACCESS : constant unsigned := 22;
   FMOD_ERR_HTTP_PROXY_AUTH : constant unsigned := 23;
   FMOD_ERR_HTTP_SERVER_ERROR : constant unsigned := 24;
   FMOD_ERR_HTTP_TIMEOUT : constant unsigned := 25;
   FMOD_ERR_INITIALIZATION : constant unsigned := 26;
   FMOD_ERR_INITIALIZED : constant unsigned := 27;
   FMOD_ERR_INTERNAL : constant unsigned := 28;
   FMOD_ERR_INVALID_FLOAT : constant unsigned := 29;
   FMOD_ERR_INVALID_HANDLE : constant unsigned := 30;
   FMOD_ERR_INVALID_PARAM : constant unsigned := 31;
   FMOD_ERR_INVALID_POSITION : constant unsigned := 32;
   FMOD_ERR_INVALID_SPEAKER : constant unsigned := 33;
   FMOD_ERR_INVALID_SYNCPOINT : constant unsigned := 34;
   FMOD_ERR_INVALID_THREAD : constant unsigned := 35;
   FMOD_ERR_INVALID_VECTOR : constant unsigned := 36;
   FMOD_ERR_MAXAUDIBLE : constant unsigned := 37;
   FMOD_ERR_MEMORY : constant unsigned := 38;
   FMOD_ERR_MEMORY_CANTPOINT : constant unsigned := 39;
   FMOD_ERR_NEEDS3D : constant unsigned := 40;
   FMOD_ERR_NEEDSHARDWARE : constant unsigned := 41;
   FMOD_ERR_NET_CONNECT : constant unsigned := 42;
   FMOD_ERR_NET_SOCKET_ERROR : constant unsigned := 43;
   FMOD_ERR_NET_URL : constant unsigned := 44;
   FMOD_ERR_NET_WOULD_BLOCK : constant unsigned := 45;
   FMOD_ERR_NOTREADY : constant unsigned := 46;
   FMOD_ERR_OUTPUT_ALLOCATED : constant unsigned := 47;
   FMOD_ERR_OUTPUT_CREATEBUFFER : constant unsigned := 48;
   FMOD_ERR_OUTPUT_DRIVERCALL : constant unsigned := 49;
   FMOD_ERR_OUTPUT_FORMAT : constant unsigned := 50;
   FMOD_ERR_OUTPUT_INIT : constant unsigned := 51;
   FMOD_ERR_OUTPUT_NODRIVERS : constant unsigned := 52;
   FMOD_ERR_PLUGIN : constant unsigned := 53;
   FMOD_ERR_PLUGIN_MISSING : constant unsigned := 54;
   FMOD_ERR_PLUGIN_RESOURCE : constant unsigned := 55;
   FMOD_ERR_PLUGIN_VERSION : constant unsigned := 56;
   FMOD_ERR_RECORD : constant unsigned := 57;
   FMOD_ERR_REVERB_CHANNELGROUP : constant unsigned := 58;
   FMOD_ERR_REVERB_INSTANCE : constant unsigned := 59;
   FMOD_ERR_SUBSOUNDS : constant unsigned := 60;
   FMOD_ERR_SUBSOUND_ALLOCATED : constant unsigned := 61;
   FMOD_ERR_SUBSOUND_CANTMOVE : constant unsigned := 62;
   FMOD_ERR_TAGNOTFOUND : constant unsigned := 63;
   FMOD_ERR_TOOMANYCHANNELS : constant unsigned := 64;
   FMOD_ERR_TRUNCATED : constant unsigned := 65;
   FMOD_ERR_UNIMPLEMENTED : constant unsigned := 66;
   FMOD_ERR_UNINITIALIZED : constant unsigned := 67;
   FMOD_ERR_UNSUPPORTED : constant unsigned := 68;
   FMOD_ERR_VERSION : constant unsigned := 69;
   FMOD_ERR_EVENT_ALREADY_LOADED : constant unsigned := 70;
   FMOD_ERR_EVENT_LIVEUPDATE_BUSY : constant unsigned := 71;
   FMOD_ERR_EVENT_LIVEUPDATE_MISMATCH : constant unsigned := 72;
   FMOD_ERR_EVENT_LIVEUPDATE_TIMEOUT : constant unsigned := 73;
   FMOD_ERR_EVENT_NOTFOUND : constant unsigned := 74;
   FMOD_ERR_STUDIO_UNINITIALIZED : constant unsigned := 75;
   FMOD_ERR_STUDIO_NOT_LOADED : constant unsigned := 76;
   FMOD_ERR_INVALID_STRING : constant unsigned := 77;
   FMOD_ERR_ALREADY_LOCKED : constant unsigned := 78;
   FMOD_ERR_NOT_LOCKED : constant unsigned := 79;
   FMOD_ERR_RECORD_DISCONNECTED : constant unsigned := 80;
   FMOD_ERR_TOOMANYSAMPLES : constant unsigned := 81;
   FMOD_RESULT_FORCEINT : constant unsigned := 65536;  -- fmod_common.h:320

   subtype FMOD_CHANNELCONTROL_TYPE is unsigned;
   FMOD_CHANNELCONTROL_CHANNEL : constant unsigned := 0;
   FMOD_CHANNELCONTROL_CHANNELGROUP : constant unsigned := 1;
   FMOD_CHANNELCONTROL_MAX : constant unsigned := 2;
   FMOD_CHANNELCONTROL_FORCEINT : constant unsigned := 65536;  -- fmod_common.h:408

   subtype FMOD_OUTPUTTYPE is unsigned;
   FMOD_OUTPUTTYPE_AUTODETECT : constant unsigned := 0;
   FMOD_OUTPUTTYPE_UNKNOWN : constant unsigned := 1;
   FMOD_OUTPUTTYPE_NOSOUND : constant unsigned := 2;
   FMOD_OUTPUTTYPE_WAVWRITER : constant unsigned := 3;
   FMOD_OUTPUTTYPE_NOSOUND_NRT : constant unsigned := 4;
   FMOD_OUTPUTTYPE_WAVWRITER_NRT : constant unsigned := 5;
   FMOD_OUTPUTTYPE_WASAPI : constant unsigned := 6;
   FMOD_OUTPUTTYPE_ASIO : constant unsigned := 7;
   FMOD_OUTPUTTYPE_PULSEAUDIO : constant unsigned := 8;
   FMOD_OUTPUTTYPE_ALSA : constant unsigned := 9;
   FMOD_OUTPUTTYPE_COREAUDIO : constant unsigned := 10;
   FMOD_OUTPUTTYPE_AUDIOTRACK : constant unsigned := 11;
   FMOD_OUTPUTTYPE_OPENSL : constant unsigned := 12;
   FMOD_OUTPUTTYPE_AUDIOOUT : constant unsigned := 13;
   FMOD_OUTPUTTYPE_AUDIO3D : constant unsigned := 14;
   FMOD_OUTPUTTYPE_WEBAUDIO : constant unsigned := 15;
   FMOD_OUTPUTTYPE_NNAUDIO : constant unsigned := 16;
   FMOD_OUTPUTTYPE_WINSONIC : constant unsigned := 17;
   FMOD_OUTPUTTYPE_AAUDIO : constant unsigned := 18;
   FMOD_OUTPUTTYPE_MAX : constant unsigned := 19;
   FMOD_OUTPUTTYPE_FORCEINT : constant unsigned := 65536;  -- fmod_common.h:417

   subtype FMOD_DEBUG_MODE is unsigned;
   FMOD_DEBUG_MODE_TTY : constant unsigned := 0;
   FMOD_DEBUG_MODE_FILE : constant unsigned := 1;
   FMOD_DEBUG_MODE_CALLBACK : constant unsigned := 2;
   FMOD_DEBUG_MODE_FORCEINT : constant unsigned := 65536;  -- fmod_common.h:443

   subtype FMOD_SPEAKERMODE is unsigned;
   FMOD_SPEAKERMODE_DEFAULT : constant unsigned := 0;
   FMOD_SPEAKERMODE_RAW : constant unsigned := 1;
   FMOD_SPEAKERMODE_MONO : constant unsigned := 2;
   FMOD_SPEAKERMODE_STEREO : constant unsigned := 3;
   FMOD_SPEAKERMODE_QUAD : constant unsigned := 4;
   FMOD_SPEAKERMODE_SURROUND : constant unsigned := 5;
   FMOD_SPEAKERMODE_5POINT1 : constant unsigned := 6;
   FMOD_SPEAKERMODE_7POINT1 : constant unsigned := 7;
   FMOD_SPEAKERMODE_7POINT1POINT4 : constant unsigned := 8;
   FMOD_SPEAKERMODE_MAX : constant unsigned := 9;
   FMOD_SPEAKERMODE_FORCEINT : constant unsigned := 65536;  -- fmod_common.h:452

   subtype FMOD_SPEAKER is int;
   FMOD_SPEAKER_NONE : constant int := -1;
   FMOD_SPEAKER_FRONT_LEFT : constant int := 0;
   FMOD_SPEAKER_FRONT_RIGHT : constant int := 1;
   FMOD_SPEAKER_FRONT_CENTER : constant int := 2;
   FMOD_SPEAKER_LOW_FREQUENCY : constant int := 3;
   FMOD_SPEAKER_SURROUND_LEFT : constant int := 4;
   FMOD_SPEAKER_SURROUND_RIGHT : constant int := 5;
   FMOD_SPEAKER_BACK_LEFT : constant int := 6;
   FMOD_SPEAKER_BACK_RIGHT : constant int := 7;
   FMOD_SPEAKER_TOP_FRONT_LEFT : constant int := 8;
   FMOD_SPEAKER_TOP_FRONT_RIGHT : constant int := 9;
   FMOD_SPEAKER_TOP_BACK_LEFT : constant int := 10;
   FMOD_SPEAKER_TOP_BACK_RIGHT : constant int := 11;
   FMOD_SPEAKER_MAX : constant int := 12;
   FMOD_SPEAKER_FORCEINT : constant int := 65536;  -- fmod_common.h:468

   subtype FMOD_CHANNELORDER is unsigned;
   FMOD_CHANNELORDER_DEFAULT : constant unsigned := 0;
   FMOD_CHANNELORDER_WAVEFORMAT : constant unsigned := 1;
   FMOD_CHANNELORDER_PROTOOLS : constant unsigned := 2;
   FMOD_CHANNELORDER_ALLMONO : constant unsigned := 3;
   FMOD_CHANNELORDER_ALLSTEREO : constant unsigned := 4;
   FMOD_CHANNELORDER_ALSA : constant unsigned := 5;
   FMOD_CHANNELORDER_MAX : constant unsigned := 6;
   FMOD_CHANNELORDER_FORCEINT : constant unsigned := 65536;  -- fmod_common.h:488

   subtype FMOD_PLUGINTYPE is unsigned;
   FMOD_PLUGINTYPE_OUTPUT : constant unsigned := 0;
   FMOD_PLUGINTYPE_CODEC : constant unsigned := 1;
   FMOD_PLUGINTYPE_DSP : constant unsigned := 2;
   FMOD_PLUGINTYPE_MAX : constant unsigned := 3;
   FMOD_PLUGINTYPE_FORCEINT : constant unsigned := 65536;  -- fmod_common.h:501

   subtype FMOD_SOUND_TYPE is unsigned;
   FMOD_SOUND_TYPE_UNKNOWN : constant unsigned := 0;
   FMOD_SOUND_TYPE_AIFF : constant unsigned := 1;
   FMOD_SOUND_TYPE_ASF : constant unsigned := 2;
   FMOD_SOUND_TYPE_DLS : constant unsigned := 3;
   FMOD_SOUND_TYPE_FLAC : constant unsigned := 4;
   FMOD_SOUND_TYPE_FSB : constant unsigned := 5;
   FMOD_SOUND_TYPE_IT : constant unsigned := 6;
   FMOD_SOUND_TYPE_MIDI : constant unsigned := 7;
   FMOD_SOUND_TYPE_MOD : constant unsigned := 8;
   FMOD_SOUND_TYPE_MPEG : constant unsigned := 9;
   FMOD_SOUND_TYPE_OGGVORBIS : constant unsigned := 10;
   FMOD_SOUND_TYPE_PLAYLIST : constant unsigned := 11;
   FMOD_SOUND_TYPE_RAW : constant unsigned := 12;
   FMOD_SOUND_TYPE_S3M : constant unsigned := 13;
   FMOD_SOUND_TYPE_USER : constant unsigned := 14;
   FMOD_SOUND_TYPE_WAV : constant unsigned := 15;
   FMOD_SOUND_TYPE_XM : constant unsigned := 16;
   FMOD_SOUND_TYPE_XMA : constant unsigned := 17;
   FMOD_SOUND_TYPE_AUDIOQUEUE : constant unsigned := 18;
   FMOD_SOUND_TYPE_AT9 : constant unsigned := 19;
   FMOD_SOUND_TYPE_VORBIS : constant unsigned := 20;
   FMOD_SOUND_TYPE_MEDIA_FOUNDATION : constant unsigned := 21;
   FMOD_SOUND_TYPE_MEDIACODEC : constant unsigned := 22;
   FMOD_SOUND_TYPE_FADPCM : constant unsigned := 23;
   FMOD_SOUND_TYPE_OPUS : constant unsigned := 24;
   FMOD_SOUND_TYPE_MAX : constant unsigned := 25;
   FMOD_SOUND_TYPE_FORCEINT : constant unsigned := 65536;  -- fmod_common.h:511

   subtype FMOD_SOUND_FORMAT is unsigned;
   FMOD_SOUND_FORMAT_NONE : constant unsigned := 0;
   FMOD_SOUND_FORMAT_PCM8 : constant unsigned := 1;
   FMOD_SOUND_FORMAT_PCM16 : constant unsigned := 2;
   FMOD_SOUND_FORMAT_PCM24 : constant unsigned := 3;
   FMOD_SOUND_FORMAT_PCM32 : constant unsigned := 4;
   FMOD_SOUND_FORMAT_PCMFLOAT : constant unsigned := 5;
   FMOD_SOUND_FORMAT_BITSTREAM : constant unsigned := 6;
   FMOD_SOUND_FORMAT_MAX : constant unsigned := 7;
   FMOD_SOUND_FORMAT_FORCEINT : constant unsigned := 65536;  -- fmod_common.h:543

   subtype FMOD_OPENSTATE is unsigned;
   FMOD_OPENSTATE_READY : constant unsigned := 0;
   FMOD_OPENSTATE_LOADING : constant unsigned := 1;
   FMOD_OPENSTATE_ERROR : constant unsigned := 2;
   FMOD_OPENSTATE_CONNECTING : constant unsigned := 3;
   FMOD_OPENSTATE_BUFFERING : constant unsigned := 4;
   FMOD_OPENSTATE_SEEKING : constant unsigned := 5;
   FMOD_OPENSTATE_PLAYING : constant unsigned := 6;
   FMOD_OPENSTATE_SETPOSITION : constant unsigned := 7;
   FMOD_OPENSTATE_MAX : constant unsigned := 8;
   FMOD_OPENSTATE_FORCEINT : constant unsigned := 65536;  -- fmod_common.h:557

   subtype FMOD_SOUNDGROUP_BEHAVIOR is unsigned;
   FMOD_SOUNDGROUP_BEHAVIOR_FAIL : constant unsigned := 0;
   FMOD_SOUNDGROUP_BEHAVIOR_MUTE : constant unsigned := 1;
   FMOD_SOUNDGROUP_BEHAVIOR_STEALLOWEST : constant unsigned := 2;
   FMOD_SOUNDGROUP_BEHAVIOR_MAX : constant unsigned := 3;
   FMOD_SOUNDGROUP_BEHAVIOR_FORCEINT : constant unsigned := 65536;  -- fmod_common.h:572

   subtype FMOD_CHANNELCONTROL_CALLBACK_TYPE is unsigned;
   FMOD_CHANNELCONTROL_CALLBACK_END : constant unsigned := 0;
   FMOD_CHANNELCONTROL_CALLBACK_VIRTUALVOICE : constant unsigned := 1;
   FMOD_CHANNELCONTROL_CALLBACK_SYNCPOINT : constant unsigned := 2;
   FMOD_CHANNELCONTROL_CALLBACK_OCCLUSION : constant unsigned := 3;
   FMOD_CHANNELCONTROL_CALLBACK_MAX : constant unsigned := 4;
   FMOD_CHANNELCONTROL_CALLBACK_FORCEINT : constant unsigned := 65536;  -- fmod_common.h:582

   subtype FMOD_CHANNELCONTROL_DSP_INDEX is int;
   FMOD_CHANNELCONTROL_DSP_HEAD : constant int := -1;
   FMOD_CHANNELCONTROL_DSP_FADER : constant int := -2;
   FMOD_CHANNELCONTROL_DSP_TAIL : constant int := -3;
   FMOD_CHANNELCONTROL_DSP_FORCEINT : constant int := 65536;  -- fmod_common.h:593

   subtype FMOD_ERRORCALLBACK_INSTANCETYPE is unsigned;
   FMOD_ERRORCALLBACK_INSTANCETYPE_NONE : constant unsigned := 0;
   FMOD_ERRORCALLBACK_INSTANCETYPE_SYSTEM : constant unsigned := 1;
   FMOD_ERRORCALLBACK_INSTANCETYPE_CHANNEL : constant unsigned := 2;
   FMOD_ERRORCALLBACK_INSTANCETYPE_CHANNELGROUP : constant unsigned := 3;
   FMOD_ERRORCALLBACK_INSTANCETYPE_CHANNELCONTROL : constant unsigned := 4;
   FMOD_ERRORCALLBACK_INSTANCETYPE_SOUND : constant unsigned := 5;
   FMOD_ERRORCALLBACK_INSTANCETYPE_SOUNDGROUP : constant unsigned := 6;
   FMOD_ERRORCALLBACK_INSTANCETYPE_DSP : constant unsigned := 7;
   FMOD_ERRORCALLBACK_INSTANCETYPE_DSPCONNECTION : constant unsigned := 8;
   FMOD_ERRORCALLBACK_INSTANCETYPE_GEOMETRY : constant unsigned := 9;
   FMOD_ERRORCALLBACK_INSTANCETYPE_REVERB3D : constant unsigned := 10;
   FMOD_ERRORCALLBACK_INSTANCETYPE_STUDIO_SYSTEM : constant unsigned := 11;
   FMOD_ERRORCALLBACK_INSTANCETYPE_STUDIO_EVENTDESCRIPTION : constant unsigned := 12;
   FMOD_ERRORCALLBACK_INSTANCETYPE_STUDIO_EVENTINSTANCE : constant unsigned := 13;
   FMOD_ERRORCALLBACK_INSTANCETYPE_STUDIO_PARAMETERINSTANCE : constant unsigned := 14;
   FMOD_ERRORCALLBACK_INSTANCETYPE_STUDIO_BUS : constant unsigned := 15;
   FMOD_ERRORCALLBACK_INSTANCETYPE_STUDIO_VCA : constant unsigned := 16;
   FMOD_ERRORCALLBACK_INSTANCETYPE_STUDIO_BANK : constant unsigned := 17;
   FMOD_ERRORCALLBACK_INSTANCETYPE_STUDIO_COMMANDREPLAY : constant unsigned := 18;
   FMOD_ERRORCALLBACK_INSTANCETYPE_FORCEINT : constant unsigned := 65536;  -- fmod_common.h:602

   subtype FMOD_DSP_RESAMPLER is unsigned;
   FMOD_DSP_RESAMPLER_DEFAULT : constant unsigned := 0;
   FMOD_DSP_RESAMPLER_NOINTERP : constant unsigned := 1;
   FMOD_DSP_RESAMPLER_LINEAR : constant unsigned := 2;
   FMOD_DSP_RESAMPLER_CUBIC : constant unsigned := 3;
   FMOD_DSP_RESAMPLER_SPLINE : constant unsigned := 4;
   FMOD_DSP_RESAMPLER_MAX : constant unsigned := 5;
   FMOD_DSP_RESAMPLER_FORCEINT : constant unsigned := 65536;  -- fmod_common.h:627

   subtype FMOD_DSPCONNECTION_TYPE is unsigned;
   FMOD_DSPCONNECTION_TYPE_STANDARD : constant unsigned := 0;
   FMOD_DSPCONNECTION_TYPE_SIDECHAIN : constant unsigned := 1;
   FMOD_DSPCONNECTION_TYPE_SEND : constant unsigned := 2;
   FMOD_DSPCONNECTION_TYPE_SEND_SIDECHAIN : constant unsigned := 3;
   FMOD_DSPCONNECTION_TYPE_MAX : constant unsigned := 4;
   FMOD_DSPCONNECTION_TYPE_FORCEINT : constant unsigned := 65536;  -- fmod_common.h:639

   subtype FMOD_TAGTYPE is unsigned;
   FMOD_TAGTYPE_UNKNOWN : constant unsigned := 0;
   FMOD_TAGTYPE_ID3V1 : constant unsigned := 1;
   FMOD_TAGTYPE_ID3V2 : constant unsigned := 2;
   FMOD_TAGTYPE_VORBISCOMMENT : constant unsigned := 3;
   FMOD_TAGTYPE_SHOUTCAST : constant unsigned := 4;
   FMOD_TAGTYPE_ICECAST : constant unsigned := 5;
   FMOD_TAGTYPE_ASF : constant unsigned := 6;
   FMOD_TAGTYPE_MIDI : constant unsigned := 7;
   FMOD_TAGTYPE_PLAYLIST : constant unsigned := 8;
   FMOD_TAGTYPE_FMOD : constant unsigned := 9;
   FMOD_TAGTYPE_USER : constant unsigned := 10;
   FMOD_TAGTYPE_MAX : constant unsigned := 11;
   FMOD_TAGTYPE_FORCEINT : constant unsigned := 65536;  -- fmod_common.h:650

   subtype FMOD_TAGDATATYPE is unsigned;
   FMOD_TAGDATATYPE_BINARY : constant unsigned := 0;
   FMOD_TAGDATATYPE_INT : constant unsigned := 1;
   FMOD_TAGDATATYPE_FLOAT : constant unsigned := 2;
   FMOD_TAGDATATYPE_STRING : constant unsigned := 3;
   FMOD_TAGDATATYPE_STRING_UTF16 : constant unsigned := 4;
   FMOD_TAGDATATYPE_STRING_UTF16BE : constant unsigned := 5;
   FMOD_TAGDATATYPE_STRING_UTF8 : constant unsigned := 6;
   FMOD_TAGDATATYPE_MAX : constant unsigned := 7;
   FMOD_TAGDATATYPE_FORCEINT : constant unsigned := 65536;  -- fmod_common.h:668

  --    FMOD callbacks
  -- 

   type FMOD_DEBUG_CALLBACK is access function
        (arg1 : FMOD_DEBUG_FLAGS;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : int;
         arg4 : Interfaces.C.Strings.chars_ptr;
         arg5 : Interfaces.C.Strings.chars_ptr) return FMOD_RESULT
   with Convention => C;  -- fmod_common.h:685

   type FMOD_SYSTEM_CALLBACK is access function
        (arg1 : access FMOD_SYSTEM;
         arg2 : FMOD_SYSTEM_CALLBACK_TYPE;
         arg3 : System.Address;
         arg4 : System.Address;
         arg5 : System.Address) return FMOD_RESULT
   with Convention => C;  -- fmod_common.h:686

   type FMOD_CHANNELCONTROL_CALLBACK is access function
        (arg1 : access FMOD_CHANNELCONTROL;
         arg2 : FMOD_CHANNELCONTROL_TYPE;
         arg3 : FMOD_CHANNELCONTROL_CALLBACK_TYPE;
         arg4 : System.Address;
         arg5 : System.Address) return FMOD_RESULT
   with Convention => C;  -- fmod_common.h:687

   type FMOD_SOUND_NONBLOCK_CALLBACK is access function (arg1 : access FMOD_SOUND; arg2 : FMOD_RESULT) return FMOD_RESULT
   with Convention => C;  -- fmod_common.h:688

   type FMOD_SOUND_PCMREAD_CALLBACK is access function
        (arg1 : access FMOD_SOUND;
         arg2 : System.Address;
         arg3 : unsigned) return FMOD_RESULT
   with Convention => C;  -- fmod_common.h:689

   type FMOD_SOUND_PCMSETPOS_CALLBACK is access function
        (arg1 : access FMOD_SOUND;
         arg2 : int;
         arg3 : unsigned;
         arg4 : FMOD_TIMEUNIT) return FMOD_RESULT
   with Convention => C;  -- fmod_common.h:690

   type FMOD_FILE_OPEN_CALLBACK is access function
        (arg1 : Interfaces.C.Strings.chars_ptr;
         arg2 : access unsigned;
         arg3 : System.Address;
         arg4 : System.Address) return FMOD_RESULT
   with Convention => C;  -- fmod_common.h:691

   type FMOD_FILE_CLOSE_CALLBACK is access function (arg1 : System.Address; arg2 : System.Address) return FMOD_RESULT
   with Convention => C;  -- fmod_common.h:692

   type FMOD_FILE_READ_CALLBACK is access function
        (arg1 : System.Address;
         arg2 : System.Address;
         arg3 : unsigned;
         arg4 : access unsigned;
         arg5 : System.Address) return FMOD_RESULT
   with Convention => C;  -- fmod_common.h:693

   type FMOD_FILE_SEEK_CALLBACK is access function
        (arg1 : System.Address;
         arg2 : unsigned;
         arg3 : System.Address) return FMOD_RESULT
   with Convention => C;  -- fmod_common.h:694

   type FMOD_FILE_ASYNCREAD_CALLBACK is access function (arg1 : access FMOD_ASYNCREADINFO; arg2 : System.Address) return FMOD_RESULT
   with Convention => C;  -- fmod_common.h:695

   type FMOD_FILE_ASYNCCANCEL_CALLBACK is access function (arg1 : access FMOD_ASYNCREADINFO; arg2 : System.Address) return FMOD_RESULT
   with Convention => C;  -- fmod_common.h:696

   type FMOD_FILE_ASYNCDONE_FUNC is access procedure (arg1 : access FMOD_ASYNCREADINFO; arg2 : FMOD_RESULT)
   with Convention => C;  -- fmod_common.h:697

   type FMOD_MEMORY_ALLOC_CALLBACK is access function
        (arg1 : unsigned;
         arg2 : FMOD_MEMORY_TYPE;
         arg3 : Interfaces.C.Strings.chars_ptr) return System.Address
   with Convention => C;  -- fmod_common.h:698

   type FMOD_MEMORY_REALLOC_CALLBACK is access function
        (arg1 : System.Address;
         arg2 : unsigned;
         arg3 : FMOD_MEMORY_TYPE;
         arg4 : Interfaces.C.Strings.chars_ptr) return System.Address
   with Convention => C;  -- fmod_common.h:699

   type FMOD_MEMORY_FREE_CALLBACK is access procedure
        (arg1 : System.Address;
         arg2 : FMOD_MEMORY_TYPE;
         arg3 : Interfaces.C.Strings.chars_ptr)
   with Convention => C;  -- fmod_common.h:700

   type FMOD_3D_ROLLOFF_CALLBACK is access function (arg1 : access FMOD_CHANNELCONTROL; arg2 : float) return float
   with Convention => C;  -- fmod_common.h:701

  --    FMOD structs
  -- 

   type FMOD_ASYNCREADINFO is record
      handle : System.Address;  -- fmod_common.h:708
      offset : aliased unsigned;  -- fmod_common.h:709
      sizebytes : aliased unsigned;  -- fmod_common.h:710
      priority : aliased int;  -- fmod_common.h:711
      userdata : System.Address;  -- fmod_common.h:712
      buffer : System.Address;  -- fmod_common.h:713
      bytesread : aliased unsigned;  -- fmod_common.h:714
      done : FMOD_FILE_ASYNCDONE_FUNC;  -- fmod_common.h:715
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_common.h:706

   type FMOD_VECTOR is record
      x : aliased float;  -- fmod_common.h:720
      y : aliased float;  -- fmod_common.h:721
      z : aliased float;  -- fmod_common.h:722
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_common.h:718

   type FMOD_3D_ATTRIBUTES is record
      position : aliased FMOD_VECTOR;  -- fmod_common.h:727
      velocity : aliased FMOD_VECTOR;  -- fmod_common.h:728
      forward : aliased FMOD_VECTOR;  -- fmod_common.h:729
      up : aliased FMOD_VECTOR;  -- fmod_common.h:730
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_common.h:725

   type FMOD_GUID_Data4_array is array (0 .. 7) of aliased unsigned_char;
   type FMOD_GUID is record
      Data1 : aliased unsigned;  -- fmod_common.h:735
      Data2 : aliased unsigned_short;  -- fmod_common.h:736
      Data3 : aliased unsigned_short;  -- fmod_common.h:737
      Data4 : aliased FMOD_GUID_Data4_array;  -- fmod_common.h:738
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_common.h:733

   type FMOD_PLUGINLIST is record
      c_type : aliased FMOD_PLUGINTYPE;  -- fmod_common.h:743
      description : System.Address;  -- fmod_common.h:744
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_common.h:741

   type FMOD_ADVANCEDSETTINGS is record
      cbSize : aliased int;  -- fmod_common.h:749
      maxMPEGCodecs : aliased int;  -- fmod_common.h:750
      maxADPCMCodecs : aliased int;  -- fmod_common.h:751
      maxXMACodecs : aliased int;  -- fmod_common.h:752
      maxVorbisCodecs : aliased int;  -- fmod_common.h:753
      maxAT9Codecs : aliased int;  -- fmod_common.h:754
      maxFADPCMCodecs : aliased int;  -- fmod_common.h:755
      maxPCMCodecs : aliased int;  -- fmod_common.h:756
      ASIONumChannels : aliased int;  -- fmod_common.h:757
      ASIOChannelList : System.Address;  -- fmod_common.h:758
      ASIOSpeakerList : access FMOD_SPEAKER;  -- fmod_common.h:759
      vol0virtualvol : aliased float;  -- fmod_common.h:760
      defaultDecodeBufferSize : aliased unsigned;  -- fmod_common.h:761
      profilePort : aliased unsigned_short;  -- fmod_common.h:762
      geometryMaxFadeTime : aliased unsigned;  -- fmod_common.h:763
      distanceFilterCenterFreq : aliased float;  -- fmod_common.h:764
      reverb3Dinstance : aliased int;  -- fmod_common.h:765
      DSPBufferPoolSize : aliased int;  -- fmod_common.h:766
      resamplerMethod : aliased FMOD_DSP_RESAMPLER;  -- fmod_common.h:767
      randomSeed : aliased unsigned;  -- fmod_common.h:768
      maxConvolutionThreads : aliased int;  -- fmod_common.h:769
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_common.h:747

   type FMOD_TAG is record
      c_type : aliased FMOD_TAGTYPE;  -- fmod_common.h:774
      datatype : aliased FMOD_TAGDATATYPE;  -- fmod_common.h:775
      name : Interfaces.C.Strings.chars_ptr;  -- fmod_common.h:776
      data : System.Address;  -- fmod_common.h:777
      datalen : aliased unsigned;  -- fmod_common.h:778
      updated : aliased FMOD_BOOL;  -- fmod_common.h:779
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_common.h:772

   type FMOD_CREATESOUNDEXINFO is record
      cbsize : aliased int;  -- fmod_common.h:784
      length : aliased unsigned;  -- fmod_common.h:785
      fileoffset : aliased unsigned;  -- fmod_common.h:786
      numchannels : aliased int;  -- fmod_common.h:787
      defaultfrequency : aliased int;  -- fmod_common.h:788
      format : aliased FMOD_SOUND_FORMAT;  -- fmod_common.h:789
      decodebuffersize : aliased unsigned;  -- fmod_common.h:790
      initialsubsound : aliased int;  -- fmod_common.h:791
      numsubsounds : aliased int;  -- fmod_common.h:792
      inclusionlist : access int;  -- fmod_common.h:793
      inclusionlistnum : aliased int;  -- fmod_common.h:794
      pcmreadcallback : FMOD_SOUND_PCMREAD_CALLBACK;  -- fmod_common.h:795
      pcmsetposcallback : FMOD_SOUND_PCMSETPOS_CALLBACK;  -- fmod_common.h:796
      nonblockcallback : FMOD_SOUND_NONBLOCK_CALLBACK;  -- fmod_common.h:797
      dlsname : Interfaces.C.Strings.chars_ptr;  -- fmod_common.h:798
      encryptionkey : Interfaces.C.Strings.chars_ptr;  -- fmod_common.h:799
      maxpolyphony : aliased int;  -- fmod_common.h:800
      userdata : System.Address;  -- fmod_common.h:801
      suggestedsoundtype : aliased FMOD_SOUND_TYPE;  -- fmod_common.h:802
      fileuseropen : FMOD_FILE_OPEN_CALLBACK;  -- fmod_common.h:803
      fileuserclose : FMOD_FILE_CLOSE_CALLBACK;  -- fmod_common.h:804
      fileuserread : FMOD_FILE_READ_CALLBACK;  -- fmod_common.h:805
      fileuserseek : FMOD_FILE_SEEK_CALLBACK;  -- fmod_common.h:806
      fileuserasyncread : FMOD_FILE_ASYNCREAD_CALLBACK;  -- fmod_common.h:807
      fileuserasynccancel : FMOD_FILE_ASYNCCANCEL_CALLBACK;  -- fmod_common.h:808
      fileuserdata : System.Address;  -- fmod_common.h:809
      filebuffersize : aliased int;  -- fmod_common.h:810
      channelorder : aliased FMOD_CHANNELORDER;  -- fmod_common.h:811
      initialsoundgroup : access FMOD_SOUNDGROUP;  -- fmod_common.h:812
      initialseekposition : aliased unsigned;  -- fmod_common.h:813
      initialseekpostype : aliased FMOD_TIMEUNIT;  -- fmod_common.h:814
      ignoresetfilesystem : aliased int;  -- fmod_common.h:815
      audioqueuepolicy : aliased unsigned;  -- fmod_common.h:816
      minmidigranularity : aliased unsigned;  -- fmod_common.h:817
      nonblockthreadid : aliased int;  -- fmod_common.h:818
      fsbguid : access FMOD_GUID;  -- fmod_common.h:819
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_common.h:782

   type FMOD_REVERB_PROPERTIES is record
      DecayTime : aliased float;  -- fmod_common.h:824
      EarlyDelay : aliased float;  -- fmod_common.h:825
      LateDelay : aliased float;  -- fmod_common.h:826
      HFReference : aliased float;  -- fmod_common.h:827
      HFDecayRatio : aliased float;  -- fmod_common.h:828
      Diffusion : aliased float;  -- fmod_common.h:829
      Density : aliased float;  -- fmod_common.h:830
      LowShelfFrequency : aliased float;  -- fmod_common.h:831
      LowShelfGain : aliased float;  -- fmod_common.h:832
      HighCut : aliased float;  -- fmod_common.h:833
      EarlyLateMix : aliased float;  -- fmod_common.h:834
      WetLevel : aliased float;  -- fmod_common.h:835
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_common.h:822

   type FMOD_ERRORCALLBACK_INFO is record
      result : aliased FMOD_RESULT;  -- fmod_common.h:840
      instancetype : aliased FMOD_ERRORCALLBACK_INSTANCETYPE;  -- fmod_common.h:841
      instance : System.Address;  -- fmod_common.h:842
      functionname : Interfaces.C.Strings.chars_ptr;  -- fmod_common.h:843
      functionparams : Interfaces.C.Strings.chars_ptr;  -- fmod_common.h:844
   end record
   with Convention => C_Pass_By_Copy;  -- fmod_common.h:838

  --    FMOD optional headers for plugin development
  -- 

end fmod_common_h;
