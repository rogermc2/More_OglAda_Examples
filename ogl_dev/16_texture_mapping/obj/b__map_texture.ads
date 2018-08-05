pragma Warnings (Off);
pragma Ada_95;
with System;
with System.Parameters;
with System.Secondary_Stack;
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
                    "GNAT Version: Community 2018 (20180523-73)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_map_texture" & ASCII.NUL;
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
   u00001 : constant Version_32 := 16#3d60ee8f#;
   pragma Export (C, u00001, "map_textureB");
   u00002 : constant Version_32 := 16#050ff2f0#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#0f7d71d4#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#f8088b52#;
   pragma Export (C, u00005, "ada__exceptionsB");
   u00006 : constant Version_32 := 16#16307b94#;
   pragma Export (C, u00006, "ada__exceptionsS");
   u00007 : constant Version_32 := 16#5726abed#;
   pragma Export (C, u00007, "ada__exceptions__last_chance_handlerB");
   u00008 : constant Version_32 := 16#41e5552e#;
   pragma Export (C, u00008, "ada__exceptions__last_chance_handlerS");
   u00009 : constant Version_32 := 16#085b6ffb#;
   pragma Export (C, u00009, "systemS");
   u00010 : constant Version_32 := 16#ae860117#;
   pragma Export (C, u00010, "system__soft_linksB");
   u00011 : constant Version_32 := 16#4d58644d#;
   pragma Export (C, u00011, "system__soft_linksS");
   u00012 : constant Version_32 := 16#bd45c2cc#;
   pragma Export (C, u00012, "system__secondary_stackB");
   u00013 : constant Version_32 := 16#4dcf97e2#;
   pragma Export (C, u00013, "system__secondary_stackS");
   u00014 : constant Version_32 := 16#86dbf443#;
   pragma Export (C, u00014, "system__parametersB");
   u00015 : constant Version_32 := 16#40b73bd0#;
   pragma Export (C, u00015, "system__parametersS");
   u00016 : constant Version_32 := 16#ced09590#;
   pragma Export (C, u00016, "system__storage_elementsB");
   u00017 : constant Version_32 := 16#259825ff#;
   pragma Export (C, u00017, "system__storage_elementsS");
   u00018 : constant Version_32 := 16#75bf515c#;
   pragma Export (C, u00018, "system__soft_links__initializeB");
   u00019 : constant Version_32 := 16#5697fc2b#;
   pragma Export (C, u00019, "system__soft_links__initializeS");
   u00020 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00020, "system__stack_checkingB");
   u00021 : constant Version_32 := 16#86e40413#;
   pragma Export (C, u00021, "system__stack_checkingS");
   u00022 : constant Version_32 := 16#34742901#;
   pragma Export (C, u00022, "system__exception_tableB");
   u00023 : constant Version_32 := 16#55f506b9#;
   pragma Export (C, u00023, "system__exception_tableS");
   u00024 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00024, "system__exceptionsB");
   u00025 : constant Version_32 := 16#6038020d#;
   pragma Export (C, u00025, "system__exceptionsS");
   u00026 : constant Version_32 := 16#80916427#;
   pragma Export (C, u00026, "system__exceptions__machineB");
   u00027 : constant Version_32 := 16#3bad9081#;
   pragma Export (C, u00027, "system__exceptions__machineS");
   u00028 : constant Version_32 := 16#aa0563fc#;
   pragma Export (C, u00028, "system__exceptions_debugB");
   u00029 : constant Version_32 := 16#76d1963f#;
   pragma Export (C, u00029, "system__exceptions_debugS");
   u00030 : constant Version_32 := 16#6c2f8802#;
   pragma Export (C, u00030, "system__img_intB");
   u00031 : constant Version_32 := 16#0a808f39#;
   pragma Export (C, u00031, "system__img_intS");
   u00032 : constant Version_32 := 16#39df8c17#;
   pragma Export (C, u00032, "system__tracebackB");
   u00033 : constant Version_32 := 16#5679b13f#;
   pragma Export (C, u00033, "system__tracebackS");
   u00034 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00034, "system__traceback_entriesB");
   u00035 : constant Version_32 := 16#0800998b#;
   pragma Export (C, u00035, "system__traceback_entriesS");
   u00036 : constant Version_32 := 16#bb296fbb#;
   pragma Export (C, u00036, "system__traceback__symbolicB");
   u00037 : constant Version_32 := 16#c84061d1#;
   pragma Export (C, u00037, "system__traceback__symbolicS");
   u00038 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00038, "ada__exceptions__tracebackB");
   u00039 : constant Version_32 := 16#20245e75#;
   pragma Export (C, u00039, "ada__exceptions__tracebackS");
   u00040 : constant Version_32 := 16#a0d3d22b#;
   pragma Export (C, u00040, "system__address_imageB");
   u00041 : constant Version_32 := 16#a9b7f2c1#;
   pragma Export (C, u00041, "system__address_imageS");
   u00042 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00042, "system__wch_conB");
   u00043 : constant Version_32 := 16#13264d29#;
   pragma Export (C, u00043, "system__wch_conS");
   u00044 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00044, "system__wch_stwB");
   u00045 : constant Version_32 := 16#3e376128#;
   pragma Export (C, u00045, "system__wch_stwS");
   u00046 : constant Version_32 := 16#a831679c#;
   pragma Export (C, u00046, "system__wch_cnvB");
   u00047 : constant Version_32 := 16#1c91f7da#;
   pragma Export (C, u00047, "system__wch_cnvS");
   u00048 : constant Version_32 := 16#5ab55268#;
   pragma Export (C, u00048, "interfacesS");
   u00049 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00049, "system__wch_jisB");
   u00050 : constant Version_32 := 16#9ce1eefb#;
   pragma Export (C, u00050, "system__wch_jisS");
   u00051 : constant Version_32 := 16#d398a95f#;
   pragma Export (C, u00051, "ada__tagsB");
   u00052 : constant Version_32 := 16#12a0afb8#;
   pragma Export (C, u00052, "ada__tagsS");
   u00053 : constant Version_32 := 16#796f31f1#;
   pragma Export (C, u00053, "system__htableB");
   u00054 : constant Version_32 := 16#8c99dc11#;
   pragma Export (C, u00054, "system__htableS");
   u00055 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00055, "system__string_hashB");
   u00056 : constant Version_32 := 16#2ec7b76f#;
   pragma Export (C, u00056, "system__string_hashS");
   u00057 : constant Version_32 := 16#3cdd1378#;
   pragma Export (C, u00057, "system__unsigned_typesS");
   u00058 : constant Version_32 := 16#afdbf393#;
   pragma Export (C, u00058, "system__val_lluB");
   u00059 : constant Version_32 := 16#462f440a#;
   pragma Export (C, u00059, "system__val_lluS");
   u00060 : constant Version_32 := 16#269742a9#;
   pragma Export (C, u00060, "system__val_utilB");
   u00061 : constant Version_32 := 16#a4fbd905#;
   pragma Export (C, u00061, "system__val_utilS");
   u00062 : constant Version_32 := 16#ec4d5631#;
   pragma Export (C, u00062, "system__case_utilB");
   u00063 : constant Version_32 := 16#378ed9af#;
   pragma Export (C, u00063, "system__case_utilS");
   u00064 : constant Version_32 := 16#927a893f#;
   pragma Export (C, u00064, "ada__text_ioB");
   u00065 : constant Version_32 := 16#1ffab6e1#;
   pragma Export (C, u00065, "ada__text_ioS");
   u00066 : constant Version_32 := 16#10558b11#;
   pragma Export (C, u00066, "ada__streamsB");
   u00067 : constant Version_32 := 16#67e31212#;
   pragma Export (C, u00067, "ada__streamsS");
   u00068 : constant Version_32 := 16#92d882c5#;
   pragma Export (C, u00068, "ada__io_exceptionsS");
   u00069 : constant Version_32 := 16#73d2d764#;
   pragma Export (C, u00069, "interfaces__c_streamsB");
   u00070 : constant Version_32 := 16#b1330297#;
   pragma Export (C, u00070, "interfaces__c_streamsS");
   u00071 : constant Version_32 := 16#4e0ce0a1#;
   pragma Export (C, u00071, "system__crtlS");
   u00072 : constant Version_32 := 16#ec083f01#;
   pragma Export (C, u00072, "system__file_ioB");
   u00073 : constant Version_32 := 16#af2a8e9e#;
   pragma Export (C, u00073, "system__file_ioS");
   u00074 : constant Version_32 := 16#86c56e5a#;
   pragma Export (C, u00074, "ada__finalizationS");
   u00075 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00075, "system__finalization_rootB");
   u00076 : constant Version_32 := 16#47a91c6b#;
   pragma Export (C, u00076, "system__finalization_rootS");
   u00077 : constant Version_32 := 16#0f8892f9#;
   pragma Export (C, u00077, "system__os_libB");
   u00078 : constant Version_32 := 16#d8e681fb#;
   pragma Export (C, u00078, "system__os_libS");
   u00079 : constant Version_32 := 16#2a8e89ad#;
   pragma Export (C, u00079, "system__stringsB");
   u00080 : constant Version_32 := 16#684d436e#;
   pragma Export (C, u00080, "system__stringsS");
   u00081 : constant Version_32 := 16#f5c4f553#;
   pragma Export (C, u00081, "system__file_control_blockS");
   u00082 : constant Version_32 := 16#f8890a6a#;
   pragma Export (C, u00082, "glfwB");
   u00083 : constant Version_32 := 16#f9879c15#;
   pragma Export (C, u00083, "glfwS");
   u00084 : constant Version_32 := 16#625a77c7#;
   pragma Export (C, u00084, "glfw__apiS");
   u00085 : constant Version_32 := 16#910a3ded#;
   pragma Export (C, u00085, "glfw__enumsS");
   u00086 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00086, "interfaces__cB");
   u00087 : constant Version_32 := 16#467817d8#;
   pragma Export (C, u00087, "interfaces__cS");
   u00088 : constant Version_32 := 16#f6c96571#;
   pragma Export (C, u00088, "glfw__errorsB");
   u00089 : constant Version_32 := 16#d9543542#;
   pragma Export (C, u00089, "glfw__errorsS");
   u00090 : constant Version_32 := 16#27986d94#;
   pragma Export (C, u00090, "interfaces__c__stringsB");
   u00091 : constant Version_32 := 16#603c1c44#;
   pragma Export (C, u00091, "interfaces__c__stringsS");
   u00092 : constant Version_32 := 16#e4e25004#;
   pragma Export (C, u00092, "glfw__inputB");
   u00093 : constant Version_32 := 16#61865ea4#;
   pragma Export (C, u00093, "glfw__inputS");
   u00094 : constant Version_32 := 16#1f0aa216#;
   pragma Export (C, u00094, "glfw__input__joysticksB");
   u00095 : constant Version_32 := 16#245f95b8#;
   pragma Export (C, u00095, "glfw__input__joysticksS");
   u00096 : constant Version_32 := 16#039168f8#;
   pragma Export (C, u00096, "system__stream_attributesB");
   u00097 : constant Version_32 := 16#8bc30a4e#;
   pragma Export (C, u00097, "system__stream_attributesS");
   u00098 : constant Version_32 := 16#7cf5f259#;
   pragma Export (C, u00098, "glfw__input__keysS");
   u00099 : constant Version_32 := 16#be7d7fd9#;
   pragma Export (C, u00099, "glfw__input__mouseS");
   u00100 : constant Version_32 := 16#67bb4e09#;
   pragma Export (C, u00100, "glfw__monitorsB");
   u00101 : constant Version_32 := 16#2fa646ce#;
   pragma Export (C, u00101, "glfw__monitorsS");
   u00102 : constant Version_32 := 16#aa77b9a2#;
   pragma Export (C, u00102, "glfw__windowsB");
   u00103 : constant Version_32 := 16#4017245f#;
   pragma Export (C, u00103, "glfw__windowsS");
   u00104 : constant Version_32 := 16#a41dbb53#;
   pragma Export (C, u00104, "glB");
   u00105 : constant Version_32 := 16#e1ec44d8#;
   pragma Export (C, u00105, "glS");
   u00106 : constant Version_32 := 16#f1314054#;
   pragma Export (C, u00106, "gl__apiS");
   u00107 : constant Version_32 := 16#6ec6a3b0#;
   pragma Export (C, u00107, "gl__attributesB");
   u00108 : constant Version_32 := 16#fd529ba0#;
   pragma Export (C, u00108, "gl__attributesS");
   u00109 : constant Version_32 := 16#1ec013c7#;
   pragma Export (C, u00109, "gl__api__doublesS");
   u00110 : constant Version_32 := 16#fddcd518#;
   pragma Export (C, u00110, "gl__api__intsS");
   u00111 : constant Version_32 := 16#2ee86a00#;
   pragma Export (C, u00111, "gl__api__shortsS");
   u00112 : constant Version_32 := 16#3b5493b1#;
   pragma Export (C, u00112, "gl__api__singlesS");
   u00113 : constant Version_32 := 16#ad165146#;
   pragma Export (C, u00113, "gl__api__uintsS");
   u00114 : constant Version_32 := 16#36545a31#;
   pragma Export (C, u00114, "gl__low_levelS");
   u00115 : constant Version_32 := 16#a7374e6d#;
   pragma Export (C, u00115, "gl__typesS");
   u00116 : constant Version_32 := 16#72d4fd8a#;
   pragma Export (C, u00116, "gl__algebraB");
   u00117 : constant Version_32 := 16#4721c722#;
   pragma Export (C, u00117, "gl__algebraS");
   u00118 : constant Version_32 := 16#3024ea54#;
   pragma Export (C, u00118, "gl__matricesB");
   u00119 : constant Version_32 := 16#cb7cb65e#;
   pragma Export (C, u00119, "gl__matricesS");
   u00120 : constant Version_32 := 16#e17718a9#;
   pragma Export (C, u00120, "gl__vectorsB");
   u00121 : constant Version_32 := 16#96a6a036#;
   pragma Export (C, u00121, "gl__vectorsS");
   u00122 : constant Version_32 := 16#4fe37ab5#;
   pragma Export (C, u00122, "gl__blendingB");
   u00123 : constant Version_32 := 16#b4e140ae#;
   pragma Export (C, u00123, "gl__blendingS");
   u00124 : constant Version_32 := 16#0eb9ad4d#;
   pragma Export (C, u00124, "gl__enumsS");
   u00125 : constant Version_32 := 16#7551f971#;
   pragma Export (C, u00125, "gl__togglesB");
   u00126 : constant Version_32 := 16#10da7a30#;
   pragma Export (C, u00126, "gl__togglesS");
   u00127 : constant Version_32 := 16#543eef32#;
   pragma Export (C, u00127, "gl__enums__getterB");
   u00128 : constant Version_32 := 16#436c0351#;
   pragma Export (C, u00128, "gl__enums__getterS");
   u00129 : constant Version_32 := 16#55d57394#;
   pragma Export (C, u00129, "gl__buffersB");
   u00130 : constant Version_32 := 16#98dc24b1#;
   pragma Export (C, u00130, "gl__buffersS");
   u00131 : constant Version_32 := 16#a2e18c54#;
   pragma Export (C, u00131, "gl__low_level__enumsS");
   u00132 : constant Version_32 := 16#d724bfcd#;
   pragma Export (C, u00132, "gl__cullingB");
   u00133 : constant Version_32 := 16#25dc9d5c#;
   pragma Export (C, u00133, "gl__cullingS");
   u00134 : constant Version_32 := 16#617b6e27#;
   pragma Export (C, u00134, "gl__types__colorsS");
   u00135 : constant Version_32 := 16#d5273407#;
   pragma Export (C, u00135, "gl__enums__texturesS");
   u00136 : constant Version_32 := 16#77fdea34#;
   pragma Export (C, u00136, "gl__errorsB");
   u00137 : constant Version_32 := 16#039a9fbd#;
   pragma Export (C, u00137, "gl__errorsS");
   u00138 : constant Version_32 := 16#54a487d2#;
   pragma Export (C, u00138, "gl__fixedB");
   u00139 : constant Version_32 := 16#e14412e4#;
   pragma Export (C, u00139, "gl__fixedS");
   u00140 : constant Version_32 := 16#06aae245#;
   pragma Export (C, u00140, "gl__fixed__lightingB");
   u00141 : constant Version_32 := 16#dbfee30b#;
   pragma Export (C, u00141, "gl__fixed__lightingS");
   u00142 : constant Version_32 := 16#2375d0b1#;
   pragma Export (C, u00142, "gl__fixed__texturesB");
   u00143 : constant Version_32 := 16#328a26b9#;
   pragma Export (C, u00143, "gl__fixed__texturesS");
   u00144 : constant Version_32 := 16#6a71ed8c#;
   pragma Export (C, u00144, "gl__helpersB");
   u00145 : constant Version_32 := 16#13e97148#;
   pragma Export (C, u00145, "gl__helpersS");
   u00146 : constant Version_32 := 16#2a51ae4d#;
   pragma Export (C, u00146, "gl__framebufferB");
   u00147 : constant Version_32 := 16#e64a7501#;
   pragma Export (C, u00147, "gl__framebufferS");
   u00148 : constant Version_32 := 16#bd1514de#;
   pragma Export (C, u00148, "gl__pixelsB");
   u00149 : constant Version_32 := 16#307de7d9#;
   pragma Export (C, u00149, "gl__pixelsS");
   u00150 : constant Version_32 := 16#84b54a85#;
   pragma Export (C, u00150, "gl__objectsB");
   u00151 : constant Version_32 := 16#8652efc3#;
   pragma Export (C, u00151, "gl__objectsS");
   u00152 : constant Version_32 := 16#a469d0b1#;
   pragma Export (C, u00152, "gl__objects__buffersB");
   u00153 : constant Version_32 := 16#f70e520c#;
   pragma Export (C, u00153, "gl__objects__buffersS");
   u00154 : constant Version_32 := 16#179d7d28#;
   pragma Export (C, u00154, "ada__containersS");
   u00155 : constant Version_32 := 16#c164a034#;
   pragma Export (C, u00155, "ada__containers__hash_tablesS");
   u00156 : constant Version_32 := 16#bcec81df#;
   pragma Export (C, u00156, "ada__containers__helpersB");
   u00157 : constant Version_32 := 16#4adfc5eb#;
   pragma Export (C, u00157, "ada__containers__helpersS");
   u00158 : constant Version_32 := 16#020a3f4d#;
   pragma Export (C, u00158, "system__atomic_countersB");
   u00159 : constant Version_32 := 16#bc074276#;
   pragma Export (C, u00159, "system__atomic_countersS");
   u00160 : constant Version_32 := 16#c24eaf4d#;
   pragma Export (C, u00160, "ada__containers__prime_numbersB");
   u00161 : constant Version_32 := 16#6d3af8ed#;
   pragma Export (C, u00161, "ada__containers__prime_numbersS");
   u00162 : constant Version_32 := 16#d96e3c40#;
   pragma Export (C, u00162, "system__finalization_mastersB");
   u00163 : constant Version_32 := 16#53a75631#;
   pragma Export (C, u00163, "system__finalization_mastersS");
   u00164 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00164, "system__img_boolB");
   u00165 : constant Version_32 := 16#fd821e10#;
   pragma Export (C, u00165, "system__img_boolS");
   u00166 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00166, "system__ioB");
   u00167 : constant Version_32 := 16#961998b4#;
   pragma Export (C, u00167, "system__ioS");
   u00168 : constant Version_32 := 16#6d4d969a#;
   pragma Export (C, u00168, "system__storage_poolsB");
   u00169 : constant Version_32 := 16#2bb6f156#;
   pragma Export (C, u00169, "system__storage_poolsS");
   u00170 : constant Version_32 := 16#273384e4#;
   pragma Export (C, u00170, "system__img_enum_newB");
   u00171 : constant Version_32 := 16#6917693b#;
   pragma Export (C, u00171, "system__img_enum_newS");
   u00172 : constant Version_32 := 16#5a895de2#;
   pragma Export (C, u00172, "system__pool_globalB");
   u00173 : constant Version_32 := 16#7141203e#;
   pragma Export (C, u00173, "system__pool_globalS");
   u00174 : constant Version_32 := 16#2323a8af#;
   pragma Export (C, u00174, "system__memoryB");
   u00175 : constant Version_32 := 16#512609cf#;
   pragma Export (C, u00175, "system__memoryS");
   u00176 : constant Version_32 := 16#2e260032#;
   pragma Export (C, u00176, "system__storage_pools__subpoolsB");
   u00177 : constant Version_32 := 16#cc5a1856#;
   pragma Export (C, u00177, "system__storage_pools__subpoolsS");
   u00178 : constant Version_32 := 16#84042202#;
   pragma Export (C, u00178, "system__storage_pools__subpools__finalizationB");
   u00179 : constant Version_32 := 16#fe2f4b3a#;
   pragma Export (C, u00179, "system__storage_pools__subpools__finalizationS");
   u00180 : constant Version_32 := 16#86ecf8ab#;
   pragma Export (C, u00180, "system__strings__stream_opsB");
   u00181 : constant Version_32 := 16#ec029138#;
   pragma Export (C, u00181, "system__strings__stream_opsS");
   u00182 : constant Version_32 := 16#db0aa7dc#;
   pragma Export (C, u00182, "ada__streams__stream_ioB");
   u00183 : constant Version_32 := 16#55e6e4b0#;
   pragma Export (C, u00183, "ada__streams__stream_ioS");
   u00184 : constant Version_32 := 16#5de653db#;
   pragma Export (C, u00184, "system__communicationB");
   u00185 : constant Version_32 := 16#113b3a29#;
   pragma Export (C, u00185, "system__communicationS");
   u00186 : constant Version_32 := 16#54fe271d#;
   pragma Export (C, u00186, "gl__objects__framebuffersB");
   u00187 : constant Version_32 := 16#3052eeeb#;
   pragma Export (C, u00187, "gl__objects__framebuffersS");
   u00188 : constant Version_32 := 16#97d8f76f#;
   pragma Export (C, u00188, "gl__objects__renderbuffersB");
   u00189 : constant Version_32 := 16#8a360460#;
   pragma Export (C, u00189, "gl__objects__renderbuffersS");
   u00190 : constant Version_32 := 16#c9b68422#;
   pragma Export (C, u00190, "gl__objects__texturesB");
   u00191 : constant Version_32 := 16#59f19211#;
   pragma Export (C, u00191, "gl__objects__texturesS");
   u00192 : constant Version_32 := 16#ed58c24a#;
   pragma Export (C, u00192, "gl__enums__indexesB");
   u00193 : constant Version_32 := 16#7e965334#;
   pragma Export (C, u00193, "gl__enums__indexesS");
   u00194 : constant Version_32 := 16#f7d8ca0a#;
   pragma Export (C, u00194, "gl__objects__programsB");
   u00195 : constant Version_32 := 16#f51c4902#;
   pragma Export (C, u00195, "gl__objects__programsS");
   u00196 : constant Version_32 := 16#33afba0b#;
   pragma Export (C, u00196, "gl__objects__shadersB");
   u00197 : constant Version_32 := 16#ae71b85a#;
   pragma Export (C, u00197, "gl__objects__shadersS");
   u00198 : constant Version_32 := 16#eb291c74#;
   pragma Export (C, u00198, "gl__objects__shaders__listsB");
   u00199 : constant Version_32 := 16#e2fdf35e#;
   pragma Export (C, u00199, "gl__objects__shaders__listsS");
   u00200 : constant Version_32 := 16#b1650c94#;
   pragma Export (C, u00200, "gl__objects__listsB");
   u00201 : constant Version_32 := 16#99f0c725#;
   pragma Export (C, u00201, "gl__objects__listsS");
   u00202 : constant Version_32 := 16#b5bff27e#;
   pragma Export (C, u00202, "gl__uniformsB");
   u00203 : constant Version_32 := 16#4e3d1406#;
   pragma Export (C, u00203, "gl__uniformsS");
   u00204 : constant Version_32 := 16#4ea5649a#;
   pragma Export (C, u00204, "gl__rasterizationB");
   u00205 : constant Version_32 := 16#0a72c568#;
   pragma Export (C, u00205, "gl__rasterizationS");
   u00206 : constant Version_32 := 16#21d800a3#;
   pragma Export (C, u00206, "gl__load_function_pointersB");
   u00207 : constant Version_32 := 16#99fdb80e#;
   pragma Export (C, u00207, "gl__api__subprogram_referenceB");
   u00208 : constant Version_32 := 16#15eca993#;
   pragma Export (C, u00208, "gl__api__subprogram_referenceS");
   u00209 : constant Version_32 := 16#31192e77#;
   pragma Export (C, u00209, "gl__api__mac_os_xB");
   u00210 : constant Version_32 := 16#9f06ff00#;
   pragma Export (C, u00210, "gl__api__mac_os_xS");
   u00211 : constant Version_32 := 16#fd83e873#;
   pragma Export (C, u00211, "system__concat_2B");
   u00212 : constant Version_32 := 16#0afbb82b#;
   pragma Export (C, u00212, "system__concat_2S");
   u00213 : constant Version_32 := 16#ee837e72#;
   pragma Export (C, u00213, "glfw__windows__contextB");
   u00214 : constant Version_32 := 16#26750862#;
   pragma Export (C, u00214, "glfw__windows__contextS");
   u00215 : constant Version_32 := 16#2bdf45f9#;
   pragma Export (C, u00215, "initializeB");
   u00216 : constant Version_32 := 16#3b701138#;
   pragma Export (C, u00216, "initializeS");
   u00217 : constant Version_32 := 16#6b9992c2#;
   pragma Export (C, u00217, "glfw__windows__hintsB");
   u00218 : constant Version_32 := 16#7de353f4#;
   pragma Export (C, u00218, "glfw__windows__hintsS");
   u00219 : constant Version_32 := 16#09f28941#;
   pragma Export (C, u00219, "utilitiesB");
   u00220 : constant Version_32 := 16#498ed34d#;
   pragma Export (C, u00220, "utilitiesS");
   u00221 : constant Version_32 := 16#e6d4fa36#;
   pragma Export (C, u00221, "ada__stringsS");
   u00222 : constant Version_32 := 16#457fb2da#;
   pragma Export (C, u00222, "ada__strings__unboundedB");
   u00223 : constant Version_32 := 16#f39c7224#;
   pragma Export (C, u00223, "ada__strings__unboundedS");
   u00224 : constant Version_32 := 16#2eb48a6d#;
   pragma Export (C, u00224, "ada__strings__searchB");
   u00225 : constant Version_32 := 16#c1ab8667#;
   pragma Export (C, u00225, "ada__strings__searchS");
   u00226 : constant Version_32 := 16#96df1a3f#;
   pragma Export (C, u00226, "ada__strings__mapsB");
   u00227 : constant Version_32 := 16#1e526bec#;
   pragma Export (C, u00227, "ada__strings__mapsS");
   u00228 : constant Version_32 := 16#98e13b0e#;
   pragma Export (C, u00228, "system__bit_opsB");
   u00229 : constant Version_32 := 16#0765e3a3#;
   pragma Export (C, u00229, "system__bit_opsS");
   u00230 : constant Version_32 := 16#5b4659fa#;
   pragma Export (C, u00230, "ada__charactersS");
   u00231 : constant Version_32 := 16#4b7bb96a#;
   pragma Export (C, u00231, "ada__characters__latin_1S");
   u00232 : constant Version_32 := 16#acee74ad#;
   pragma Export (C, u00232, "system__compare_array_unsigned_8B");
   u00233 : constant Version_32 := 16#a1581e76#;
   pragma Export (C, u00233, "system__compare_array_unsigned_8S");
   u00234 : constant Version_32 := 16#a8025f3c#;
   pragma Export (C, u00234, "system__address_operationsB");
   u00235 : constant Version_32 := 16#1b57d1c8#;
   pragma Export (C, u00235, "system__address_operationsS");
   u00236 : constant Version_32 := 16#1c33fa5b#;
   pragma Export (C, u00236, "gl__contextB");
   u00237 : constant Version_32 := 16#a27bc5f3#;
   pragma Export (C, u00237, "gl__contextS");
   u00238 : constant Version_32 := 16#adb6d201#;
   pragma Export (C, u00238, "ada__strings__fixedB");
   u00239 : constant Version_32 := 16#a86b22b3#;
   pragma Export (C, u00239, "ada__strings__fixedS");
   u00240 : constant Version_32 := 16#2b70b149#;
   pragma Export (C, u00240, "system__concat_3B");
   u00241 : constant Version_32 := 16#032b335e#;
   pragma Export (C, u00241, "system__concat_3S");
   u00242 : constant Version_32 := 16#932a4690#;
   pragma Export (C, u00242, "system__concat_4B");
   u00243 : constant Version_32 := 16#763f44db#;
   pragma Export (C, u00243, "system__concat_4S");
   u00244 : constant Version_32 := 16#8aa4f090#;
   pragma Export (C, u00244, "system__img_realB");
   u00245 : constant Version_32 := 16#cff33e19#;
   pragma Export (C, u00245, "system__img_realS");
   u00246 : constant Version_32 := 16#0cccd408#;
   pragma Export (C, u00246, "system__fat_llfS");
   u00247 : constant Version_32 := 16#1b28662b#;
   pragma Export (C, u00247, "system__float_controlB");
   u00248 : constant Version_32 := 16#e8a72cc7#;
   pragma Export (C, u00248, "system__float_controlS");
   u00249 : constant Version_32 := 16#3e932977#;
   pragma Export (C, u00249, "system__img_lluB");
   u00250 : constant Version_32 := 16#751413bb#;
   pragma Export (C, u00250, "system__img_lluS");
   u00251 : constant Version_32 := 16#ec78c2bf#;
   pragma Export (C, u00251, "system__img_unsB");
   u00252 : constant Version_32 := 16#a3292f8f#;
   pragma Export (C, u00252, "system__img_unsS");
   u00253 : constant Version_32 := 16#582b098c#;
   pragma Export (C, u00253, "system__powten_tableS");
   u00254 : constant Version_32 := 16#65a7e602#;
   pragma Export (C, u00254, "mathsB");
   u00255 : constant Version_32 := 16#1df11686#;
   pragma Export (C, u00255, "mathsS");
   u00256 : constant Version_32 := 16#cd2959fb#;
   pragma Export (C, u00256, "ada__numericsS");
   u00257 : constant Version_32 := 16#e5114ee9#;
   pragma Export (C, u00257, "ada__numerics__auxB");
   u00258 : constant Version_32 := 16#9f6e24ed#;
   pragma Export (C, u00258, "ada__numerics__auxS");
   u00259 : constant Version_32 := 16#6533c8fa#;
   pragma Export (C, u00259, "system__machine_codeS");
   u00260 : constant Version_32 := 16#dd6d0ce5#;
   pragma Export (C, u00260, "quaternionsB");
   u00261 : constant Version_32 := 16#1f499d0b#;
   pragma Export (C, u00261, "quaternionsS");
   u00262 : constant Version_32 := 16#46b1f5ea#;
   pragma Export (C, u00262, "system__concat_8B");
   u00263 : constant Version_32 := 16#eb5c222c#;
   pragma Export (C, u00263, "system__concat_8S");
   u00264 : constant Version_32 := 16#46899fd1#;
   pragma Export (C, u00264, "system__concat_7B");
   u00265 : constant Version_32 := 16#f49c34e4#;
   pragma Export (C, u00265, "system__concat_7S");
   u00266 : constant Version_32 := 16#a83b7c85#;
   pragma Export (C, u00266, "system__concat_6B");
   u00267 : constant Version_32 := 16#da9c4249#;
   pragma Export (C, u00267, "system__concat_6S");
   u00268 : constant Version_32 := 16#608e2cd1#;
   pragma Export (C, u00268, "system__concat_5B");
   u00269 : constant Version_32 := 16#8f052cd5#;
   pragma Export (C, u00269, "system__concat_5S");
   u00270 : constant Version_32 := 16#b2a569d2#;
   pragma Export (C, u00270, "system__exn_llfB");
   u00271 : constant Version_32 := 16#b425d427#;
   pragma Export (C, u00271, "system__exn_llfS");
   u00272 : constant Version_32 := 16#502e73ef#;
   pragma Export (C, u00272, "system__fat_fltS");
   u00273 : constant Version_32 := 16#c2f33c3c#;
   pragma Export (C, u00273, "main_loopB");
   u00274 : constant Version_32 := 16#bdd35137#;
   pragma Export (C, u00274, "main_loopS");
   u00275 : constant Version_32 := 16#1f269880#;
   pragma Export (C, u00275, "buffersB");
   u00276 : constant Version_32 := 16#36eea01e#;
   pragma Export (C, u00276, "buffersS");
   u00277 : constant Version_32 := 16#d8c78d6a#;
   pragma Export (C, u00277, "gl__objects__textures__targetsB");
   u00278 : constant Version_32 := 16#f6ffc087#;
   pragma Export (C, u00278, "gl__objects__textures__targetsS");
   u00279 : constant Version_32 := 16#c8eda0b7#;
   pragma Export (C, u00279, "system__taskingB");
   u00280 : constant Version_32 := 16#904e70c9#;
   pragma Export (C, u00280, "system__taskingS");
   u00281 : constant Version_32 := 16#fde20231#;
   pragma Export (C, u00281, "system__task_primitivesS");
   u00282 : constant Version_32 := 16#4a005d03#;
   pragma Export (C, u00282, "system__os_interfaceB");
   u00283 : constant Version_32 := 16#55598295#;
   pragma Export (C, u00283, "system__os_interfaceS");
   u00284 : constant Version_32 := 16#1b8990a4#;
   pragma Export (C, u00284, "interfaces__c__extensionsS");
   u00285 : constant Version_32 := 16#54ea2fd2#;
   pragma Export (C, u00285, "system__os_constantsS");
   u00286 : constant Version_32 := 16#9690f83e#;
   pragma Export (C, u00286, "system__task_primitives__operationsB");
   u00287 : constant Version_32 := 16#bd0bc49c#;
   pragma Export (C, u00287, "system__task_primitives__operationsS");
   u00288 : constant Version_32 := 16#89b55e64#;
   pragma Export (C, u00288, "system__interrupt_managementB");
   u00289 : constant Version_32 := 16#1a73cd21#;
   pragma Export (C, u00289, "system__interrupt_managementS");
   u00290 : constant Version_32 := 16#f65595cf#;
   pragma Export (C, u00290, "system__multiprocessorsB");
   u00291 : constant Version_32 := 16#30f7f088#;
   pragma Export (C, u00291, "system__multiprocessorsS");
   u00292 : constant Version_32 := 16#2b2125d3#;
   pragma Export (C, u00292, "system__os_primitivesB");
   u00293 : constant Version_32 := 16#0fa60a0d#;
   pragma Export (C, u00293, "system__os_primitivesS");
   u00294 : constant Version_32 := 16#e0fce7f8#;
   pragma Export (C, u00294, "system__task_infoB");
   u00295 : constant Version_32 := 16#8841d2fa#;
   pragma Export (C, u00295, "system__task_infoS");
   u00296 : constant Version_32 := 16#1036f432#;
   pragma Export (C, u00296, "system__tasking__debugB");
   u00297 : constant Version_32 := 16#de1ac8b1#;
   pragma Export (C, u00297, "system__tasking__debugS");
   u00298 : constant Version_32 := 16#9dca6636#;
   pragma Export (C, u00298, "system__img_lliB");
   u00299 : constant Version_32 := 16#19143a2a#;
   pragma Export (C, u00299, "system__img_lliS");
   u00300 : constant Version_32 := 16#6ec3c867#;
   pragma Export (C, u00300, "system__stack_usageB");
   u00301 : constant Version_32 := 16#3a3ac346#;
   pragma Export (C, u00301, "system__stack_usageS");
   u00302 : constant Version_32 := 16#479ec0a5#;
   pragma Export (C, u00302, "gl__objects__textures__with_1d_loaderB");
   u00303 : constant Version_32 := 16#d32d8e5b#;
   pragma Export (C, u00303, "gl__objects__textures__with_1d_loaderS");
   u00304 : constant Version_32 := 16#c58365ab#;
   pragma Export (C, u00304, "gl__objects__textures__with_2d_loaderB");
   u00305 : constant Version_32 := 16#a46acc9c#;
   pragma Export (C, u00305, "gl__objects__textures__with_2d_loaderS");
   u00306 : constant Version_32 := 16#cef6390f#;
   pragma Export (C, u00306, "gl__objects__textures__with_3d_loaderB");
   u00307 : constant Version_32 := 16#1998073c#;
   pragma Export (C, u00307, "gl__objects__textures__with_3d_loaderS");
   u00308 : constant Version_32 := 16#aa9a2958#;
   pragma Export (C, u00308, "gl__objects__vertex_arraysB");
   u00309 : constant Version_32 := 16#1d5a67ec#;
   pragma Export (C, u00309, "gl__objects__vertex_arraysS");
   u00310 : constant Version_32 := 16#80bcaa24#;
   pragma Export (C, u00310, "gl__windowB");
   u00311 : constant Version_32 := 16#24a263f4#;
   pragma Export (C, u00311, "gl__windowS");
   u00312 : constant Version_32 := 16#e9e8503f#;
   pragma Export (C, u00312, "ogldev_cameraB");
   u00313 : constant Version_32 := 16#2f727283#;
   pragma Export (C, u00313, "ogldev_cameraS");
   u00314 : constant Version_32 := 16#1b606804#;
   pragma Export (C, u00314, "ogldev_mathB");
   u00315 : constant Version_32 := 16#bdcae620#;
   pragma Export (C, u00315, "ogldev_mathS");
   u00316 : constant Version_32 := 16#8225628b#;
   pragma Export (C, u00316, "ada__containers__red_black_treesS");
   u00317 : constant Version_32 := 16#9fb0398c#;
   pragma Export (C, u00317, "assimp_typesS");
   u00318 : constant Version_32 := 16#67b5cf8a#;
   pragma Export (C, u00318, "api_vectorsS");
   u00319 : constant Version_32 := 16#6ad167f4#;
   pragma Export (C, u00319, "matrix_4x4S");
   u00320 : constant Version_32 := 16#c67fa46b#;
   pragma Export (C, u00320, "ogldev_pipelineB");
   u00321 : constant Version_32 := 16#cd3f9bd9#;
   pragma Export (C, u00321, "ogldev_pipelineS");
   u00322 : constant Version_32 := 16#99a6679d#;
   pragma Export (C, u00322, "ogldev_textureB");
   u00323 : constant Version_32 := 16#960796fe#;
   pragma Export (C, u00323, "ogldev_textureS");
   u00324 : constant Version_32 := 16#622ef959#;
   pragma Export (C, u00324, "magick_blobB");
   u00325 : constant Version_32 := 16#4b00bb5e#;
   pragma Export (C, u00325, "magick_blobS");
   u00326 : constant Version_32 := 16#3bd4d6c1#;
   pragma Export (C, u00326, "magick_blob__apiS");
   u00327 : constant Version_32 := 16#95bf520f#;
   pragma Export (C, u00327, "blob_referenceS");
   u00328 : constant Version_32 := 16#8bfaf470#;
   pragma Export (C, u00328, "core_blobB");
   u00329 : constant Version_32 := 16#d1053d88#;
   pragma Export (C, u00329, "core_blobS");
   u00330 : constant Version_32 := 16#dbe9ed6e#;
   pragma Export (C, u00330, "magick_typeB");
   u00331 : constant Version_32 := 16#7785c8f7#;
   pragma Export (C, u00331, "magick_typeS");
   u00332 : constant Version_32 := 16#35df64f0#;
   pragma Export (C, u00332, "method_attributeS");
   u00333 : constant Version_32 := 16#53ad6155#;
   pragma Export (C, u00333, "semaphoreS");
   u00334 : constant Version_32 := 16#49b21dba#;
   pragma Export (C, u00334, "threadS");
   u00335 : constant Version_32 := 16#d389491e#;
   pragma Export (C, u00335, "sys_pthread_mutexS");
   u00336 : constant Version_32 := 16#69226a62#;
   pragma Export (C, u00336, "sys_pthread_typesS");
   u00337 : constant Version_32 := 16#78534528#;
   pragma Export (C, u00337, "streamS");
   u00338 : constant Version_32 := 16#f6d2357a#;
   pragma Export (C, u00338, "core_imageB");
   u00339 : constant Version_32 := 16#9cd5aa63#;
   pragma Export (C, u00339, "core_imageS");
   u00340 : constant Version_32 := 16#22ecdf37#;
   pragma Export (C, u00340, "cacheS");
   u00341 : constant Version_32 := 16#87ef8a9e#;
   pragma Export (C, u00341, "colourS");
   u00342 : constant Version_32 := 16#f63ada63#;
   pragma Export (C, u00342, "magick_exceptionB");
   u00343 : constant Version_32 := 16#5ac5357b#;
   pragma Export (C, u00343, "magick_exceptionS");
   u00344 : constant Version_32 := 16#7d87fa07#;
   pragma Export (C, u00344, "magick_pixelB");
   u00345 : constant Version_32 := 16#5e69ed76#;
   pragma Export (C, u00345, "magick_pixelS");
   u00346 : constant Version_32 := 16#db19c26b#;
   pragma Export (C, u00346, "colour_spaceS");
   u00347 : constant Version_32 := 16#e68156d9#;
   pragma Export (C, u00347, "compositeS");
   u00348 : constant Version_32 := 16#094fb960#;
   pragma Export (C, u00348, "compressS");
   u00349 : constant Version_32 := 16#642319d6#;
   pragma Export (C, u00349, "core_blob__apiS");
   u00350 : constant Version_32 := 16#12a77ece#;
   pragma Export (C, u00350, "std_ioS");
   u00351 : constant Version_32 := 16#d2264924#;
   pragma Export (C, u00351, "geometryS");
   u00352 : constant Version_32 := 16#e62db32d#;
   pragma Export (C, u00352, "layerS");
   u00353 : constant Version_32 := 16#95bae204#;
   pragma Export (C, u00353, "magick_profileS");
   u00354 : constant Version_32 := 16#439df4d2#;
   pragma Export (C, u00354, "methodS");
   u00355 : constant Version_32 := 16#cfe9dadb#;
   pragma Export (C, u00355, "monitorS");
   u00356 : constant Version_32 := 16#7d9e2ba3#;
   pragma Export (C, u00356, "quantumS");
   u00357 : constant Version_32 := 16#3b4f34f4#;
   pragma Export (C, u00357, "resampleS");
   u00358 : constant Version_32 := 16#14500f3d#;
   pragma Export (C, u00358, "timerS");
   u00359 : constant Version_32 := 16#ed186219#;
   pragma Export (C, u00359, "magick_imageB");
   u00360 : constant Version_32 := 16#4ab6a8e3#;
   pragma Export (C, u00360, "magick_imageS");
   u00361 : constant Version_32 := 16#45273d41#;
   pragma Export (C, u00361, "magick_image__apiS");
   u00362 : constant Version_32 := 16#df84ea6d#;
   pragma Export (C, u00362, "image_referenceS");
   u00363 : constant Version_32 := 16#948c2841#;
   pragma Export (C, u00363, "magick_optionsS");
   u00364 : constant Version_32 := 16#f4eb32b3#;
   pragma Export (C, u00364, "drawS");
   u00365 : constant Version_32 := 16#ff9d83ad#;
   pragma Export (C, u00365, "quantizeS");
   u00366 : constant Version_32 := 16#7f23b26e#;
   pragma Export (C, u00366, "ogldev_engine_commonS");
   u00367 : constant Version_32 := 16#8364a88f#;
   pragma Export (C, u00367, "program_loaderB");
   u00368 : constant Version_32 := 16#25171948#;
   pragma Export (C, u00368, "program_loaderS");
   u00369 : constant Version_32 := 16#e6195f7f#;
   pragma Export (C, u00369, "ada__directoriesB");
   u00370 : constant Version_32 := 16#9da5f6a3#;
   pragma Export (C, u00370, "ada__directoriesS");
   u00371 : constant Version_32 := 16#b8719323#;
   pragma Export (C, u00371, "ada__calendarB");
   u00372 : constant Version_32 := 16#41508869#;
   pragma Export (C, u00372, "ada__calendarS");
   u00373 : constant Version_32 := 16#95569f93#;
   pragma Export (C, u00373, "ada__calendar__formattingB");
   u00374 : constant Version_32 := 16#7ddaf16f#;
   pragma Export (C, u00374, "ada__calendar__formattingS");
   u00375 : constant Version_32 := 16#e3cca715#;
   pragma Export (C, u00375, "ada__calendar__time_zonesB");
   u00376 : constant Version_32 := 16#77b56b93#;
   pragma Export (C, u00376, "ada__calendar__time_zonesS");
   u00377 : constant Version_32 := 16#d763507a#;
   pragma Export (C, u00377, "system__val_intB");
   u00378 : constant Version_32 := 16#40fe45c4#;
   pragma Export (C, u00378, "system__val_intS");
   u00379 : constant Version_32 := 16#1d9142a4#;
   pragma Export (C, u00379, "system__val_unsB");
   u00380 : constant Version_32 := 16#2c75fe43#;
   pragma Export (C, u00380, "system__val_unsS");
   u00381 : constant Version_32 := 16#c2ca0511#;
   pragma Export (C, u00381, "system__val_realB");
   u00382 : constant Version_32 := 16#f67218ea#;
   pragma Export (C, u00382, "system__val_realS");
   u00383 : constant Version_32 := 16#8f637df8#;
   pragma Export (C, u00383, "ada__characters__handlingB");
   u00384 : constant Version_32 := 16#3b3f6154#;
   pragma Export (C, u00384, "ada__characters__handlingS");
   u00385 : constant Version_32 := 16#92f05f13#;
   pragma Export (C, u00385, "ada__strings__maps__constantsS");
   u00386 : constant Version_32 := 16#ab4ad33a#;
   pragma Export (C, u00386, "ada__directories__validityB");
   u00387 : constant Version_32 := 16#498b13d5#;
   pragma Export (C, u00387, "ada__directories__validityS");
   u00388 : constant Version_32 := 16#2f6496d9#;
   pragma Export (C, u00388, "system__file_attributesS");
   u00389 : constant Version_32 := 16#95f86c43#;
   pragma Export (C, u00389, "system__regexpB");
   u00390 : constant Version_32 := 16#2b69c837#;
   pragma Export (C, u00390, "system__regexpS");
   u00391 : constant Version_32 := 16#f1013930#;
   pragma Export (C, u00391, "gl__filesB");
   u00392 : constant Version_32 := 16#07708aff#;
   pragma Export (C, u00392, "gl__filesS");
   u00393 : constant Version_32 := 16#0460865d#;
   pragma Export (C, u00393, "system__direct_ioB");
   u00394 : constant Version_32 := 16#aa3a89c5#;
   pragma Export (C, u00394, "system__direct_ioS");

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
   --  system.exn_llf%s
   --  system.exn_llf%b
   --  system.float_control%s
   --  system.float_control%b
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.io%s
   --  system.io%b
   --  system.machine_code%s
   --  system.os_primitives%s
   --  system.os_primitives%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.powten_table%s
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.stack_usage%s
   --  system.stack_usage%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%s
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.task_info%s
   --  system.task_info%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.unsigned_types%s
   --  system.img_llu%s
   --  system.img_llu%b
   --  system.img_uns%s
   --  system.img_uns%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
   --  system.compare_array_unsigned_8%s
   --  system.compare_array_unsigned_8%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.concat_4%s
   --  system.concat_4%b
   --  system.concat_5%s
   --  system.concat_5%b
   --  system.concat_6%s
   --  system.concat_6%b
   --  system.concat_7%s
   --  system.concat_7%b
   --  system.concat_8%s
   --  system.concat_8%b
   --  system.traceback%s
   --  system.traceback%b
   --  system.wch_stw%s
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  ada.exceptions%s
   --  system.wch_stw%b
   --  ada.exceptions.traceback%s
   --  system.secondary_stack%s
   --  system.address_image%s
   --  system.soft_links%s
   --  system.exception_table%s
   --  system.exception_table%b
   --  system.exceptions%s
   --  system.exceptions%b
   --  ada.exceptions.last_chance_handler%s
   --  system.memory%s
   --  system.memory%b
   --  ada.exceptions.traceback%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  system.exceptions.machine%s
   --  system.exceptions.machine%b
   --  system.secondary_stack%b
   --  system.address_image%b
   --  system.soft_links.initialize%s
   --  system.soft_links.initialize%b
   --  system.soft_links%b
   --  ada.exceptions.last_chance_handler%b
   --  system.standard_library%b
   --  ada.exceptions%b
   --  ada.containers%s
   --  ada.containers.prime_numbers%s
   --  ada.containers.prime_numbers%b
   --  ada.io_exceptions%s
   --  ada.numerics%s
   --  ada.strings%s
   --  interfaces.c%s
   --  interfaces.c%b
   --  interfaces.c.extensions%s
   --  interfaces.c.strings%s
   --  interfaces.c.strings%b
   --  system.case_util%s
   --  system.case_util%b
   --  system.fat_flt%s
   --  system.fat_llf%s
   --  ada.numerics.aux%s
   --  ada.numerics.aux%b
   --  system.img_real%s
   --  system.img_real%b
   --  system.multiprocessors%s
   --  system.multiprocessors%b
   --  system.os_constants%s
   --  system.os_interface%s
   --  system.os_interface%b
   --  system.interrupt_management%s
   --  system.interrupt_management%b
   --  system.os_lib%s
   --  system.os_lib%b
   --  system.task_primitives%s
   --  system.tasking%s
   --  system.task_primitives.operations%s
   --  system.tasking.debug%s
   --  system.tasking%b
   --  system.task_primitives.operations%b
   --  system.tasking.debug%b
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_llu%s
   --  system.val_llu%b
   --  ada.tags%s
   --  ada.tags%b
   --  ada.streams%s
   --  ada.streams%b
   --  system.communication%s
   --  system.communication%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  ada.containers.helpers%s
   --  ada.containers.helpers%b
   --  ada.containers.hash_tables%s
   --  ada.containers.red_black_trees%s
   --  system.file_io%s
   --  system.file_io%b
   --  ada.streams.stream_io%s
   --  ada.streams.stream_io%b
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.finalization_masters%b
   --  system.storage_pools.subpools%s
   --  system.storage_pools.subpools.finalization%s
   --  system.storage_pools.subpools.finalization%b
   --  system.storage_pools.subpools%b
   --  system.stream_attributes%s
   --  system.stream_attributes%b
   --  system.val_real%s
   --  system.val_real%b
   --  system.val_uns%s
   --  system.val_uns%b
   --  system.val_int%s
   --  system.val_int%b
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.calendar.time_zones%s
   --  ada.calendar.time_zones%b
   --  ada.calendar.formatting%s
   --  ada.calendar.formatting%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  ada.strings.maps%s
   --  ada.strings.maps%b
   --  ada.strings.maps.constants%s
   --  ada.characters.handling%s
   --  ada.characters.handling%b
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.strings.fixed%s
   --  ada.strings.fixed%b
   --  ada.strings.unbounded%s
   --  ada.strings.unbounded%b
   --  system.direct_io%s
   --  system.direct_io%b
   --  system.file_attributes%s
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.regexp%s
   --  system.regexp%b
   --  ada.directories%s
   --  ada.directories.validity%s
   --  ada.directories.validity%b
   --  ada.directories%b
   --  system.strings.stream_ops%s
   --  system.strings.stream_ops%b
   --  gl%s
   --  gl.vectors%s
   --  gl.vectors%b
   --  gl.matrices%s
   --  gl.matrices%b
   --  gl.algebra%s
   --  gl.algebra%b
   --  gl.types%s
   --  gl.types.colors%s
   --  gl.low_level%s
   --  gl.low_level.enums%s
   --  gl.attributes%s
   --  gl.rasterization%s
   --  gl.uniforms%s
   --  gl.objects%s
   --  gl.objects.lists%s
   --  gl.objects.lists%b
   --  gl.objects%b
   --  gl.pixels%s
   --  gl.helpers%s
   --  gl.helpers%b
   --  gl.fixed%s
   --  gl.errors%s
   --  gl.culling%s
   --  gl.buffers%s
   --  gl.blending%s
   --  gl.toggles%s
   --  gl.enums%s
   --  gl.objects.shaders%s
   --  gl.objects.shaders.lists%s
   --  gl.objects.shaders.lists%b
   --  gl.objects.programs%s
   --  gl.objects.textures%s
   --  gl.objects.renderbuffers%s
   --  gl.objects.buffers%s
   --  gl.framebuffer%s
   --  gl.fixed.textures%s
   --  gl.fixed.lighting%s
   --  gl.enums.textures%s
   --  gl.enums.getter%s
   --  gl.enums.indexes%s
   --  gl.enums.indexes%b
   --  gl.objects.framebuffers%s
   --  gl.api%s
   --  gl.api.mac_os_x%s
   --  gl.api.mac_os_x%b
   --  gl.api.subprogram_reference%s
   --  gl.rasterization%b
   --  gl.objects.shaders%b
   --  gl.objects.textures%b
   --  gl.objects.framebuffers%b
   --  gl.objects.renderbuffers%b
   --  gl.objects.buffers%b
   --  gl.pixels%b
   --  gl.framebuffer%b
   --  gl.fixed.textures%b
   --  gl.fixed.lighting%b
   --  gl.fixed%b
   --  gl.errors%b
   --  gl.culling%b
   --  gl.buffers%b
   --  gl.blending%b
   --  gl.enums.getter%b
   --  gl.toggles%b
   --  gl.api.uints%s
   --  gl.api.singles%s
   --  gl.api.shorts%s
   --  gl.api.ints%s
   --  gl.api.doubles%s
   --  gl.objects.programs%b
   --  gl.api.subprogram_reference%b
   --  gl.load_function_pointers%b
   --  gl%b
   --  gl.uniforms%b
   --  gl.attributes%b
   --  gl.context%s
   --  gl.context%b
   --  gl.objects.textures.with_1d_loader%s
   --  gl.objects.textures.with_1d_loader%b
   --  gl.objects.textures.with_2d_loader%s
   --  gl.objects.textures.with_2d_loader%b
   --  gl.objects.textures.with_3d_loader%s
   --  gl.objects.textures.with_3d_loader%b
   --  gl.objects.textures.targets%s
   --  gl.objects.textures.targets%b
   --  api_vectors%s
   --  assimp_types%s
   --  cache%s
   --  colour_space%s
   --  composite%s
   --  compress%s
   --  gl.files%s
   --  gl.files%b
   --  gl.objects.vertex_arrays%s
   --  gl.objects.vertex_arrays%b
   --  gl.window%s
   --  gl.window%b
   --  glfw%s
   --  glfw.monitors%s
   --  glfw.input%s
   --  glfw.input.mouse%s
   --  glfw.input.keys%s
   --  glfw.errors%s
   --  glfw.enums%s
   --  glfw.windows%s
   --  glfw.windows.context%s
   --  glfw.input.joysticks%s
   --  glfw.api%s
   --  glfw.monitors%b
   --  glfw.input.joysticks%b
   --  glfw.input%b
   --  glfw.errors%b
   --  glfw%b
   --  glfw.windows%b
   --  glfw.windows.context%b
   --  glfw.windows.hints%s
   --  glfw.windows.hints%b
   --  layer%s
   --  magick_type%s
   --  magick_type%b
   --  geometry%s
   --  magick_pixel%s
   --  magick_pixel%b
   --  matrix_4x4%s
   --  method%s
   --  method_attribute%s
   --  magick_profile%s
   --  monitor%s
   --  ogldev_engine_common%s
   --  program_loader%s
   --  program_loader%b
   --  quantize%s
   --  quaternions%s
   --  quaternions%b
   --  maths%s
   --  maths%b
   --  ogldev_camera%s
   --  ogldev_camera%b
   --  ogldev_math%s
   --  ogldev_math%b
   --  ogldev_pipeline%s
   --  ogldev_pipeline%b
   --  resample%s
   --  std_io%s
   --  stream%s
   --  sys_pthread_types%s
   --  sys_pthread_mutex%s
   --  thread%s
   --  semaphore%s
   --  core_blob%s
   --  core_blob%b
   --  core_blob.api%s
   --  magick_blob%s
   --  blob_reference%s
   --  magick_blob.api%s
   --  magick_blob%b
   --  magick_exception%s
   --  magick_exception%b
   --  colour%s
   --  quantum%s
   --  timer%s
   --  core_image%s
   --  core_image%b
   --  draw%s
   --  magick_options%s
   --  image_reference%s
   --  magick_image%s
   --  magick_image.api%s
   --  magick_image%b
   --  ogldev_texture%s
   --  ogldev_texture%b
   --  utilities%s
   --  utilities%b
   --  buffers%s
   --  buffers%b
   --  initialize%s
   --  initialize%b
   --  main_loop%s
   --  main_loop%b
   --  map_texture%b
   --  END ELABORATION ORDER

end ada_main;
