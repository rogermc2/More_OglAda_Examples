pragma Ada_95;
pragma Warnings (Off);
with System;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: 6.1.0" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_hello" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#1c802f2a#;
   pragma Export (C, u00001, "helloB");
   u00002 : constant Version_32 := 16#b6df930e#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#337e9ce1#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#472fa979#;
   pragma Export (C, u00005, "ada__exceptionsB");
   u00006 : constant Version_32 := 16#a2017425#;
   pragma Export (C, u00006, "ada__exceptionsS");
   u00007 : constant Version_32 := 16#e947e6a9#;
   pragma Export (C, u00007, "ada__exceptions__last_chance_handlerB");
   u00008 : constant Version_32 := 16#41e5552e#;
   pragma Export (C, u00008, "ada__exceptions__last_chance_handlerS");
   u00009 : constant Version_32 := 16#c3282aa7#;
   pragma Export (C, u00009, "systemS");
   u00010 : constant Version_32 := 16#465d427a#;
   pragma Export (C, u00010, "system__soft_linksB");
   u00011 : constant Version_32 := 16#5dacf2f2#;
   pragma Export (C, u00011, "system__soft_linksS");
   u00012 : constant Version_32 := 16#b01dad17#;
   pragma Export (C, u00012, "system__parametersB");
   u00013 : constant Version_32 := 16#bd0227d8#;
   pragma Export (C, u00013, "system__parametersS");
   u00014 : constant Version_32 := 16#0f0cb66d#;
   pragma Export (C, u00014, "system__secondary_stackB");
   u00015 : constant Version_32 := 16#6849e5ce#;
   pragma Export (C, u00015, "system__secondary_stackS");
   u00016 : constant Version_32 := 16#39a03df9#;
   pragma Export (C, u00016, "system__storage_elementsB");
   u00017 : constant Version_32 := 16#eeeb60a3#;
   pragma Export (C, u00017, "system__storage_elementsS");
   u00018 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00018, "system__stack_checkingB");
   u00019 : constant Version_32 := 16#4d97414f#;
   pragma Export (C, u00019, "system__stack_checkingS");
   u00020 : constant Version_32 := 16#87a448ff#;
   pragma Export (C, u00020, "system__exception_tableB");
   u00021 : constant Version_32 := 16#9e8643e5#;
   pragma Export (C, u00021, "system__exception_tableS");
   u00022 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00022, "system__exceptionsB");
   u00023 : constant Version_32 := 16#ab4b4751#;
   pragma Export (C, u00023, "system__exceptionsS");
   u00024 : constant Version_32 := 16#4c9e814d#;
   pragma Export (C, u00024, "system__exceptions__machineS");
   u00025 : constant Version_32 := 16#aa0563fc#;
   pragma Export (C, u00025, "system__exceptions_debugB");
   u00026 : constant Version_32 := 16#bda2d363#;
   pragma Export (C, u00026, "system__exceptions_debugS");
   u00027 : constant Version_32 := 16#570325c8#;
   pragma Export (C, u00027, "system__img_intB");
   u00028 : constant Version_32 := 16#c1f3ca65#;
   pragma Export (C, u00028, "system__img_intS");
   u00029 : constant Version_32 := 16#39df8c17#;
   pragma Export (C, u00029, "system__tracebackB");
   u00030 : constant Version_32 := 16#9d0af463#;
   pragma Export (C, u00030, "system__tracebackS");
   u00031 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00031, "system__traceback_entriesB");
   u00032 : constant Version_32 := 16#c373dcd7#;
   pragma Export (C, u00032, "system__traceback_entriesS");
   u00033 : constant Version_32 := 16#6fd210f2#;
   pragma Export (C, u00033, "system__traceback__symbolicB");
   u00034 : constant Version_32 := 16#dd19f67a#;
   pragma Export (C, u00034, "system__traceback__symbolicS");
   u00035 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00035, "ada__exceptions__tracebackB");
   u00036 : constant Version_32 := 16#20245e75#;
   pragma Export (C, u00036, "ada__exceptions__tracebackS");
   u00037 : constant Version_32 := 16#57a37a42#;
   pragma Export (C, u00037, "system__address_imageB");
   u00038 : constant Version_32 := 16#62c4b79d#;
   pragma Export (C, u00038, "system__address_imageS");
   u00039 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00039, "system__wch_conB");
   u00040 : constant Version_32 := 16#d8550875#;
   pragma Export (C, u00040, "system__wch_conS");
   u00041 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00041, "system__wch_stwB");
   u00042 : constant Version_32 := 16#f5442474#;
   pragma Export (C, u00042, "system__wch_stwS");
   u00043 : constant Version_32 := 16#b96cfbef#;
   pragma Export (C, u00043, "system__wch_cnvB");
   u00044 : constant Version_32 := 16#d7e2b286#;
   pragma Export (C, u00044, "system__wch_cnvS");
   u00045 : constant Version_32 := 16#4be8ce1b#;
   pragma Export (C, u00045, "interfacesS");
   u00046 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00046, "system__wch_jisB");
   u00047 : constant Version_32 := 16#5792aba7#;
   pragma Export (C, u00047, "system__wch_jisS");
   u00048 : constant Version_32 := 16#920eada5#;
   pragma Export (C, u00048, "ada__tagsB");
   u00049 : constant Version_32 := 16#13ca27f3#;
   pragma Export (C, u00049, "ada__tagsS");
   u00050 : constant Version_32 := 16#c3335bfd#;
   pragma Export (C, u00050, "system__htableB");
   u00051 : constant Version_32 := 16#47ea994d#;
   pragma Export (C, u00051, "system__htableS");
   u00052 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00052, "system__string_hashB");
   u00053 : constant Version_32 := 16#e5b4f233#;
   pragma Export (C, u00053, "system__string_hashS");
   u00054 : constant Version_32 := 16#5e708e67#;
   pragma Export (C, u00054, "system__unsigned_typesS");
   u00055 : constant Version_32 := 16#06052bd0#;
   pragma Export (C, u00055, "system__val_lluB");
   u00056 : constant Version_32 := 16#2482d915#;
   pragma Export (C, u00056, "system__val_lluS");
   u00057 : constant Version_32 := 16#27b600b2#;
   pragma Export (C, u00057, "system__val_utilB");
   u00058 : constant Version_32 := 16#6f889c59#;
   pragma Export (C, u00058, "system__val_utilS");
   u00059 : constant Version_32 := 16#d1060688#;
   pragma Export (C, u00059, "system__case_utilB");
   u00060 : constant Version_32 := 16#e7214370#;
   pragma Export (C, u00060, "system__case_utilS");
   u00061 : constant Version_32 := 16#28f088c2#;
   pragma Export (C, u00061, "ada__text_ioB");
   u00062 : constant Version_32 := 16#2d7da68a#;
   pragma Export (C, u00062, "ada__text_ioS");
   u00063 : constant Version_32 := 16#10558b11#;
   pragma Export (C, u00063, "ada__streamsB");
   u00064 : constant Version_32 := 16#2e6701ab#;
   pragma Export (C, u00064, "ada__streamsS");
   u00065 : constant Version_32 := 16#db5c917c#;
   pragma Export (C, u00065, "ada__io_exceptionsS");
   u00066 : constant Version_32 := 16#84a27f0d#;
   pragma Export (C, u00066, "interfaces__c_streamsB");
   u00067 : constant Version_32 := 16#a06e9ee4#;
   pragma Export (C, u00067, "interfaces__c_streamsS");
   u00068 : constant Version_32 := 16#b3b9fca9#;
   pragma Export (C, u00068, "system__crtlS");
   u00069 : constant Version_32 := 16#f1dc49a7#;
   pragma Export (C, u00069, "system__file_ioB");
   u00070 : constant Version_32 := 16#6459cbc2#;
   pragma Export (C, u00070, "system__file_ioS");
   u00071 : constant Version_32 := 16#cf417de3#;
   pragma Export (C, u00071, "ada__finalizationS");
   u00072 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00072, "system__finalization_rootB");
   u00073 : constant Version_32 := 16#8cda5937#;
   pragma Export (C, u00073, "system__finalization_rootS");
   u00074 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00074, "interfaces__cB");
   u00075 : constant Version_32 := 16#61e3d2ff#;
   pragma Export (C, u00075, "interfaces__cS");
   u00076 : constant Version_32 := 16#197dc8cd#;
   pragma Export (C, u00076, "system__os_libB");
   u00077 : constant Version_32 := 16#dc0cac3f#;
   pragma Export (C, u00077, "system__os_libS");
   u00078 : constant Version_32 := 16#1a817b8e#;
   pragma Export (C, u00078, "system__stringsB");
   u00079 : constant Version_32 := 16#bd973bc1#;
   pragma Export (C, u00079, "system__stringsS");
   u00080 : constant Version_32 := 16#3eb7b00f#;
   pragma Export (C, u00080, "system__file_control_blockS");
   u00081 : constant Version_32 := 16#5763aff7#;
   pragma Export (C, u00081, "glfwB");
   u00082 : constant Version_32 := 16#08cde0a0#;
   pragma Export (C, u00082, "glfwS");
   u00083 : constant Version_32 := 16#08f43d5c#;
   pragma Export (C, u00083, "glB");
   u00084 : constant Version_32 := 16#e1ec44d8#;
   pragma Export (C, u00084, "glS");
   u00085 : constant Version_32 := 16#57acee79#;
   pragma Export (C, u00085, "gl__apiS");
   u00086 : constant Version_32 := 16#f37a77e2#;
   pragma Export (C, u00086, "gl__attributesB");
   u00087 : constant Version_32 := 16#fd529ba0#;
   pragma Export (C, u00087, "gl__attributesS");
   u00088 : constant Version_32 := 16#94ea0d7a#;
   pragma Export (C, u00088, "gl__api__doublesS");
   u00089 : constant Version_32 := 16#6d1a370b#;
   pragma Export (C, u00089, "gl__api__intsS");
   u00090 : constant Version_32 := 16#990190e4#;
   pragma Export (C, u00090, "gl__api__shortsS");
   u00091 : constant Version_32 := 16#46c912b6#;
   pragma Export (C, u00091, "gl__api__singlesS");
   u00092 : constant Version_32 := 16#9536c8bb#;
   pragma Export (C, u00092, "gl__api__uintsS");
   u00093 : constant Version_32 := 16#ffd2fcf9#;
   pragma Export (C, u00093, "gl__low_levelS");
   u00094 : constant Version_32 := 16#92f92955#;
   pragma Export (C, u00094, "gl__typesS");
   u00095 : constant Version_32 := 16#b4215e1d#;
   pragma Export (C, u00095, "gl__algebraB");
   u00096 : constant Version_32 := 16#03f7ab4d#;
   pragma Export (C, u00096, "gl__algebraS");
   u00097 : constant Version_32 := 16#971b5210#;
   pragma Export (C, u00097, "gl__matricesB");
   u00098 : constant Version_32 := 16#cb7cb65e#;
   pragma Export (C, u00098, "gl__matricesS");
   u00099 : constant Version_32 := 16#69e03752#;
   pragma Export (C, u00099, "gl__vectorsB");
   u00100 : constant Version_32 := 16#e7beab61#;
   pragma Export (C, u00100, "gl__vectorsS");
   u00101 : constant Version_32 := 16#1b9b80a1#;
   pragma Export (C, u00101, "interfaces__c__stringsB");
   u00102 : constant Version_32 := 16#603c1c44#;
   pragma Export (C, u00102, "interfaces__c__stringsS");
   u00103 : constant Version_32 := 16#426c2896#;
   pragma Export (C, u00103, "gl__blendingB");
   u00104 : constant Version_32 := 16#497aebb9#;
   pragma Export (C, u00104, "gl__blendingS");
   u00105 : constant Version_32 := 16#4eab72ce#;
   pragma Export (C, u00105, "gl__enumsS");
   u00106 : constant Version_32 := 16#5c0136c4#;
   pragma Export (C, u00106, "gl__togglesB");
   u00107 : constant Version_32 := 16#c697f1ab#;
   pragma Export (C, u00107, "gl__togglesS");
   u00108 : constant Version_32 := 16#269cbee4#;
   pragma Export (C, u00108, "gl__enums__getterS");
   u00109 : constant Version_32 := 16#af2a89de#;
   pragma Export (C, u00109, "gl__buffersB");
   u00110 : constant Version_32 := 16#65478fa6#;
   pragma Export (C, u00110, "gl__buffersS");
   u00111 : constant Version_32 := 16#5f7a2743#;
   pragma Export (C, u00111, "gl__low_level__enumsS");
   u00112 : constant Version_32 := 16#e258add1#;
   pragma Export (C, u00112, "gl__cullingB");
   u00113 : constant Version_32 := 16#d847364b#;
   pragma Export (C, u00113, "gl__cullingS");
   u00114 : constant Version_32 := 16#617b6e27#;
   pragma Export (C, u00114, "gl__types__colorsS");
   u00115 : constant Version_32 := 16#bee3cb08#;
   pragma Export (C, u00115, "gl__enums__texturesS");
   u00116 : constant Version_32 := 16#3411ce9d#;
   pragma Export (C, u00116, "gl__errorsB");
   u00117 : constant Version_32 := 16#fe0134aa#;
   pragma Export (C, u00117, "gl__errorsS");
   u00118 : constant Version_32 := 16#1748a37b#;
   pragma Export (C, u00118, "gl__fixedB");
   u00119 : constant Version_32 := 16#1cdfb9f3#;
   pragma Export (C, u00119, "gl__fixedS");
   u00120 : constant Version_32 := 16#a9c701a8#;
   pragma Export (C, u00120, "gl__fixed__lightingB");
   u00121 : constant Version_32 := 16#e76a1b6d#;
   pragma Export (C, u00121, "gl__fixed__lightingS");
   u00122 : constant Version_32 := 16#f4e1c091#;
   pragma Export (C, u00122, "system__stream_attributesB");
   u00123 : constant Version_32 := 16#221dd20d#;
   pragma Export (C, u00123, "system__stream_attributesS");
   u00124 : constant Version_32 := 16#6099f418#;
   pragma Export (C, u00124, "gl__fixed__texturesB");
   u00125 : constant Version_32 := 16#cf118dae#;
   pragma Export (C, u00125, "gl__fixed__texturesS");
   u00126 : constant Version_32 := 16#6a71ed8c#;
   pragma Export (C, u00126, "gl__helpersB");
   u00127 : constant Version_32 := 16#ee72da5f#;
   pragma Export (C, u00127, "gl__helpersS");
   u00128 : constant Version_32 := 16#f0452941#;
   pragma Export (C, u00128, "gl__framebufferB");
   u00129 : constant Version_32 := 16#80ee9f75#;
   pragma Export (C, u00129, "gl__framebufferS");
   u00130 : constant Version_32 := 16#ad0bc940#;
   pragma Export (C, u00130, "gl__pixelsB");
   u00131 : constant Version_32 := 16#d077c373#;
   pragma Export (C, u00131, "gl__pixelsS");
   u00132 : constant Version_32 := 16#7fddb2d6#;
   pragma Export (C, u00132, "gl__objectsB");
   u00133 : constant Version_32 := 16#ee14714a#;
   pragma Export (C, u00133, "gl__objectsS");
   u00134 : constant Version_32 := 16#dc6c750d#;
   pragma Export (C, u00134, "gl__objects__buffersB");
   u00135 : constant Version_32 := 16#b43eb420#;
   pragma Export (C, u00135, "gl__objects__buffersS");
   u00136 : constant Version_32 := 16#5e196e91#;
   pragma Export (C, u00136, "ada__containersS");
   u00137 : constant Version_32 := 16#c164a034#;
   pragma Export (C, u00137, "ada__containers__hash_tablesS");
   u00138 : constant Version_32 := 16#14d67c72#;
   pragma Export (C, u00138, "ada__containers__helpersB");
   u00139 : constant Version_32 := 16#4adfc5eb#;
   pragma Export (C, u00139, "ada__containers__helpersS");
   u00140 : constant Version_32 := 16#020a3f4d#;
   pragma Export (C, u00140, "system__atomic_countersB");
   u00141 : constant Version_32 := 16#7774072a#;
   pragma Export (C, u00141, "system__atomic_countersS");
   u00142 : constant Version_32 := 16#c24eaf4d#;
   pragma Export (C, u00142, "ada__containers__prime_numbersB");
   u00143 : constant Version_32 := 16#6d3af8ed#;
   pragma Export (C, u00143, "ada__containers__prime_numbersS");
   u00144 : constant Version_32 := 16#6abe5dbe#;
   pragma Export (C, u00144, "system__finalization_mastersB");
   u00145 : constant Version_32 := 16#98d4136d#;
   pragma Export (C, u00145, "system__finalization_mastersS");
   u00146 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00146, "system__img_boolB");
   u00147 : constant Version_32 := 16#36f15b4c#;
   pragma Export (C, u00147, "system__img_boolS");
   u00148 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00148, "system__ioB");
   u00149 : constant Version_32 := 16#5d6adde8#;
   pragma Export (C, u00149, "system__ioS");
   u00150 : constant Version_32 := 16#6d4d969a#;
   pragma Export (C, u00150, "system__storage_poolsB");
   u00151 : constant Version_32 := 16#e0c5b40a#;
   pragma Export (C, u00151, "system__storage_poolsS");
   u00152 : constant Version_32 := 16#d0432c8d#;
   pragma Export (C, u00152, "system__img_enum_newB");
   u00153 : constant Version_32 := 16#a2642c67#;
   pragma Export (C, u00153, "system__img_enum_newS");
   u00154 : constant Version_32 := 16#5a895de2#;
   pragma Export (C, u00154, "system__pool_globalB");
   u00155 : constant Version_32 := 16#7141203e#;
   pragma Export (C, u00155, "system__pool_globalS");
   u00156 : constant Version_32 := 16#58e7cff7#;
   pragma Export (C, u00156, "system__memoryB");
   u00157 : constant Version_32 := 16#9a554c93#;
   pragma Export (C, u00157, "system__memoryS");
   u00158 : constant Version_32 := 16#e78389d8#;
   pragma Export (C, u00158, "system__storage_pools__subpoolsB");
   u00159 : constant Version_32 := 16#cc5a1856#;
   pragma Export (C, u00159, "system__storage_pools__subpoolsS");
   u00160 : constant Version_32 := 16#9aad1ff1#;
   pragma Export (C, u00160, "system__storage_pools__subpools__finalizationB");
   u00161 : constant Version_32 := 16#fe2f4b3a#;
   pragma Export (C, u00161, "system__storage_pools__subpools__finalizationS");
   u00162 : constant Version_32 := 16#312d8fcd#;
   pragma Export (C, u00162, "system__strings__stream_opsB");
   u00163 : constant Version_32 := 16#55d4bd57#;
   pragma Export (C, u00163, "system__strings__stream_opsS");
   u00164 : constant Version_32 := 16#2e6a7c56#;
   pragma Export (C, u00164, "ada__streams__stream_ioB");
   u00165 : constant Version_32 := 16#31fc8e02#;
   pragma Export (C, u00165, "ada__streams__stream_ioS");
   u00166 : constant Version_32 := 16#5de653db#;
   pragma Export (C, u00166, "system__communicationB");
   u00167 : constant Version_32 := 16#da487f75#;
   pragma Export (C, u00167, "system__communicationS");
   u00168 : constant Version_32 := 16#eaf53e5a#;
   pragma Export (C, u00168, "gl__objects__framebuffersB");
   u00169 : constant Version_32 := 16#cd24f044#;
   pragma Export (C, u00169, "gl__objects__framebuffersS");
   u00170 : constant Version_32 := 16#53f3d104#;
   pragma Export (C, u00170, "gl__objects__renderbuffersB");
   u00171 : constant Version_32 := 16#22992df7#;
   pragma Export (C, u00171, "gl__objects__renderbuffersS");
   u00172 : constant Version_32 := 16#216c145b#;
   pragma Export (C, u00172, "gl__objects__texturesB");
   u00173 : constant Version_32 := 16#ae871266#;
   pragma Export (C, u00173, "gl__objects__texturesS");
   u00174 : constant Version_32 := 16#50266b69#;
   pragma Export (C, u00174, "gl__enums__indexesB");
   u00175 : constant Version_32 := 16#aa14aa16#;
   pragma Export (C, u00175, "gl__enums__indexesS");
   u00176 : constant Version_32 := 16#3bab65db#;
   pragma Export (C, u00176, "gl__objects__programsB");
   u00177 : constant Version_32 := 16#60931315#;
   pragma Export (C, u00177, "gl__objects__programsS");
   u00178 : constant Version_32 := 16#df249643#;
   pragma Export (C, u00178, "gl__objects__shadersB");
   u00179 : constant Version_32 := 16#99239fd2#;
   pragma Export (C, u00179, "gl__objects__shadersS");
   u00180 : constant Version_32 := 16#eb291c74#;
   pragma Export (C, u00180, "gl__objects__shaders__listsB");
   u00181 : constant Version_32 := 16#bde94a5f#;
   pragma Export (C, u00181, "gl__objects__shaders__listsS");
   u00182 : constant Version_32 := 16#b1650c94#;
   pragma Export (C, u00182, "gl__objects__listsB");
   u00183 : constant Version_32 := 16#0c2df2bb#;
   pragma Export (C, u00183, "gl__objects__listsS");
   u00184 : constant Version_32 := 16#bf41ae94#;
   pragma Export (C, u00184, "gl__uniformsB");
   u00185 : constant Version_32 := 16#4e3d1406#;
   pragma Export (C, u00185, "gl__uniformsS");
   u00186 : constant Version_32 := 16#432a36b9#;
   pragma Export (C, u00186, "gl__rasterizationB");
   u00187 : constant Version_32 := 16#f7e96e7f#;
   pragma Export (C, u00187, "gl__rasterizationS");
   u00188 : constant Version_32 := 16#1a665180#;
   pragma Export (C, u00188, "gl__load_function_pointersB");
   u00189 : constant Version_32 := 16#816531bf#;
   pragma Export (C, u00189, "gl__api__subprogram_referenceB");
   u00190 : constant Version_32 := 16#56008d3a#;
   pragma Export (C, u00190, "gl__api__subprogram_referenceS");
   u00191 : constant Version_32 := 16#ccf15963#;
   pragma Export (C, u00191, "gl__api__mac_os_xB");
   u00192 : constant Version_32 := 16#f3bfc4d3#;
   pragma Export (C, u00192, "gl__api__mac_os_xS");
   u00193 : constant Version_32 := 16#fd83e873#;
   pragma Export (C, u00193, "system__concat_2B");
   u00194 : constant Version_32 := 16#c188fd77#;
   pragma Export (C, u00194, "system__concat_2S");
   u00195 : constant Version_32 := 16#7776ea39#;
   pragma Export (C, u00195, "glfw__apiS");
   u00196 : constant Version_32 := 16#5413977e#;
   pragma Export (C, u00196, "glfw__enumsS");
   u00197 : constant Version_32 := 16#7bb3b8c8#;
   pragma Export (C, u00197, "glfw__errorsB");
   u00198 : constant Version_32 := 16#281e49f7#;
   pragma Export (C, u00198, "glfw__errorsS");
   u00199 : constant Version_32 := 16#df2c9043#;
   pragma Export (C, u00199, "glfw__inputB");
   u00200 : constant Version_32 := 16#b0449067#;
   pragma Export (C, u00200, "glfw__inputS");
   u00201 : constant Version_32 := 16#044cd027#;
   pragma Export (C, u00201, "glfw__input__joysticksB");
   u00202 : constant Version_32 := 16#3084f1e8#;
   pragma Export (C, u00202, "glfw__input__joysticksS");
   u00203 : constant Version_32 := 16#5c7d402f#;
   pragma Export (C, u00203, "glfw__input__keysS");
   u00204 : constant Version_32 := 16#9ef5cdaf#;
   pragma Export (C, u00204, "glfw__input__mouseS");
   u00205 : constant Version_32 := 16#7cfd3c38#;
   pragma Export (C, u00205, "glfw__monitorsB");
   u00206 : constant Version_32 := 16#159f7f27#;
   pragma Export (C, u00206, "glfw__monitorsS");
   u00207 : constant Version_32 := 16#339bb29e#;
   pragma Export (C, u00207, "glfw__windowsB");
   u00208 : constant Version_32 := 16#a0271349#;
   pragma Export (C, u00208, "glfw__windowsS");
   u00209 : constant Version_32 := 16#30409e7d#;
   pragma Export (C, u00209, "glfw__windows__contextB");
   u00210 : constant Version_32 := 16#fc7c069d#;
   pragma Export (C, u00210, "glfw__windows__contextS");
   u00211 : constant Version_32 := 16#64d38f39#;
   pragma Export (C, u00211, "initializeB");
   u00212 : constant Version_32 := 16#cbddc14c#;
   pragma Export (C, u00212, "initializeS");
   u00213 : constant Version_32 := 16#448c36d5#;
   pragma Export (C, u00213, "glfw__windows__hintsB");
   u00214 : constant Version_32 := 16#a7ea5d0b#;
   pragma Export (C, u00214, "glfw__windows__hintsS");
   u00215 : constant Version_32 := 16#88ec2306#;
   pragma Export (C, u00215, "utilitiesB");
   u00216 : constant Version_32 := 16#883d99ed#;
   pragma Export (C, u00216, "utilitiesS");
   u00217 : constant Version_32 := 16#af50e98f#;
   pragma Export (C, u00217, "ada__stringsS");
   u00218 : constant Version_32 := 16#f78329ae#;
   pragma Export (C, u00218, "ada__strings__unboundedB");
   u00219 : constant Version_32 := 16#4c956ffe#;
   pragma Export (C, u00219, "ada__strings__unboundedS");
   u00220 : constant Version_32 := 16#e5c7cf31#;
   pragma Export (C, u00220, "ada__strings__searchB");
   u00221 : constant Version_32 := 16#c1ab8667#;
   pragma Export (C, u00221, "ada__strings__searchS");
   u00222 : constant Version_32 := 16#e2ea8656#;
   pragma Export (C, u00222, "ada__strings__mapsB");
   u00223 : constant Version_32 := 16#1e526bec#;
   pragma Export (C, u00223, "ada__strings__mapsS");
   u00224 : constant Version_32 := 16#0d3c0e78#;
   pragma Export (C, u00224, "system__bit_opsB");
   u00225 : constant Version_32 := 16#0765e3a3#;
   pragma Export (C, u00225, "system__bit_opsS");
   u00226 : constant Version_32 := 16#12c24a43#;
   pragma Export (C, u00226, "ada__charactersS");
   u00227 : constant Version_32 := 16#4b7bb96a#;
   pragma Export (C, u00227, "ada__characters__latin_1S");
   u00228 : constant Version_32 := 16#5b9edcc4#;
   pragma Export (C, u00228, "system__compare_array_unsigned_8B");
   u00229 : constant Version_32 := 16#6a2b5b2a#;
   pragma Export (C, u00229, "system__compare_array_unsigned_8S");
   u00230 : constant Version_32 := 16#5f72f755#;
   pragma Export (C, u00230, "system__address_operationsB");
   u00231 : constant Version_32 := 16#d0249494#;
   pragma Export (C, u00231, "system__address_operationsS");
   u00232 : constant Version_32 := 16#11bca878#;
   pragma Export (C, u00232, "gl__contextB");
   u00233 : constant Version_32 := 16#1d72d829#;
   pragma Export (C, u00233, "gl__contextS");
   u00234 : constant Version_32 := 16#e5480ede#;
   pragma Export (C, u00234, "ada__strings__fixedB");
   u00235 : constant Version_32 := 16#a86b22b3#;
   pragma Export (C, u00235, "ada__strings__fixedS");
   u00236 : constant Version_32 := 16#2b70b149#;
   pragma Export (C, u00236, "system__concat_3B");
   u00237 : constant Version_32 := 16#c8587602#;
   pragma Export (C, u00237, "system__concat_3S");
   u00238 : constant Version_32 := 16#fd4a31c0#;
   pragma Export (C, u00238, "main_loopB");
   u00239 : constant Version_32 := 16#67da5fc8#;
   pragma Export (C, u00239, "main_loopS");
   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.atomic_counters%s
   --  system.atomic_counters%b
   --  system.case_util%s
   --  system.case_util%b
   --  system.htable%s
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.io%s
   --  system.io%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.os_lib%s
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  system.soft_links%s
   --  system.unsigned_types%s
   --  system.val_llu%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_llu%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_cnv%s
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  ada.exceptions.traceback%s
   --  system.address_image%s
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  system.compare_array_unsigned_8%s
   --  system.compare_array_unsigned_8%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.containers%s
   --  ada.containers.prime_numbers%s
   --  ada.containers.prime_numbers%b
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  ada.strings.maps%s
   --  ada.strings.fixed%s
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.tags%s
   --  ada.streams%s
   --  ada.streams%b
   --  interfaces.c%s
   --  interfaces.c.strings%s
   --  system.communication%s
   --  system.communication%b
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.exceptions.machine%s
   --  system.file_control_block%s
   --  ada.streams.stream_io%s
   --  system.file_io%s
   --  ada.streams.stream_io%b
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  ada.containers.helpers%s
   --  ada.containers.helpers%b
   --  ada.containers.hash_tables%s
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.storage_pools.subpools%s
   --  system.storage_pools.subpools.finalization%s
   --  system.storage_pools.subpools.finalization%b
   --  system.stream_attributes%s
   --  system.stream_attributes%b
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.secondary_stack%s
   --  system.storage_pools.subpools%b
   --  system.finalization_masters%b
   --  system.file_io%b
   --  interfaces.c.strings%b
   --  interfaces.c%b
   --  ada.tags%b
   --  ada.strings.fixed%b
   --  ada.strings.maps%b
   --  system.soft_links%b
   --  system.os_lib%b
   --  system.secondary_stack%b
   --  system.address_image%b
   --  ada.exceptions.traceback%b
   --  ada.strings.unbounded%s
   --  ada.strings.unbounded%b
   --  system.strings.stream_ops%s
   --  system.strings.stream_ops%b
   --  system.traceback%s
   --  system.traceback%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  ada.exceptions%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  gl%s
   --  gl.matrices%s
   --  gl.matrices%b
   --  gl.vectors%s
   --  gl.vectors%b
   --  gl.algebra%s
   --  gl.algebra%b
   --  gl.types%s
   --  gl.attributes%s
   --  gl.context%s
   --  gl.low_level%s
   --  gl.culling%s
   --  gl.errors%s
   --  gl.fixed%s
   --  gl.low_level.enums%s
   --  gl.objects%s
   --  gl.objects%b
   --  gl.objects.buffers%s
   --  gl.objects.lists%s
   --  gl.objects.lists%b
   --  gl.objects.shaders%s
   --  gl.objects.shaders.lists%s
   --  gl.objects.shaders.lists%b
   --  gl.pixels%s
   --  gl.objects.renderbuffers%s
   --  gl.rasterization%s
   --  gl.toggles%s
   --  gl.enums%s
   --  gl.enums.getter%s
   --  gl.enums.indexes%s
   --  gl.enums.textures%s
   --  gl.types.colors%s
   --  gl.buffers%s
   --  gl.blending%s
   --  gl.fixed.lighting%s
   --  gl.fixed.textures%s
   --  gl.framebuffer%s
   --  gl.helpers%s
   --  gl.helpers%b
   --  gl.objects.textures%s
   --  gl.objects.framebuffers%s
   --  gl.uniforms%s
   --  gl.objects.programs%s
   --  gl.api%s
   --  gl.objects.programs%b
   --  gl.objects.framebuffers%b
   --  gl.objects.textures%b
   --  gl.framebuffer%b
   --  gl.fixed.textures%b
   --  gl.blending%b
   --  gl.buffers%b
   --  gl.enums.indexes%b
   --  gl.toggles%b
   --  gl.rasterization%b
   --  gl.objects.renderbuffers%b
   --  gl.pixels%b
   --  gl.objects.shaders%b
   --  gl.objects.buffers%b
   --  gl.fixed%b
   --  gl.errors%b
   --  gl.culling%b
   --  gl.context%b
   --  gl.api.doubles%s
   --  gl.api.ints%s
   --  gl.api.mac_os_x%s
   --  gl.api.mac_os_x%b
   --  gl.api.shorts%s
   --  gl.api.singles%s
   --  gl.fixed.lighting%b
   --  gl.api.subprogram_reference%s
   --  gl.api.subprogram_reference%b
   --  gl.api.uints%s
   --  gl.uniforms%b
   --  gl.attributes%b
   --  gl.load_function_pointers%b
   --  gl%b
   --  glfw%s
   --  glfw.enums%s
   --  glfw.errors%s
   --  glfw.input%s
   --  glfw.input.joysticks%s
   --  glfw.input.keys%s
   --  glfw.input.mouse%s
   --  glfw.monitors%s
   --  glfw.windows%s
   --  glfw.windows.context%s
   --  glfw.api%s
   --  glfw.windows.context%b
   --  glfw.windows%b
   --  glfw.monitors%b
   --  glfw.input.joysticks%b
   --  glfw.input%b
   --  glfw.errors%b
   --  glfw%b
   --  glfw.windows.hints%s
   --  glfw.windows.hints%b
   --  initialize%s
   --  main_loop%s
   --  hello%b
   --  utilities%s
   --  utilities%b
   --  main_loop%b
   --  initialize%b
   --  END ELABORATION ORDER


end ada_main;
