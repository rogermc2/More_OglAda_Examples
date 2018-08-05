pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__transform_feedback.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__transform_feedback.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "system__exception_table_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__exceptions_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__secondary_stack_E");
   E151 : Short_Integer; pragma Import (Ada, E151, "ada__containers_E");
   E066 : Short_Integer; pragma Import (Ada, E066, "ada__io_exceptions_E");
   E253 : Short_Integer; pragma Import (Ada, E253, "ada__numerics_E");
   E218 : Short_Integer; pragma Import (Ada, E218, "ada__strings_E");
   E085 : Short_Integer; pragma Import (Ada, E085, "interfaces__c_E");
   E089 : Short_Integer; pragma Import (Ada, E089, "interfaces__c__strings_E");
   E076 : Short_Integer; pragma Import (Ada, E076, "system__os_lib_E");
   E050 : Short_Integer; pragma Import (Ada, E050, "ada__tags_E");
   E065 : Short_Integer; pragma Import (Ada, E065, "ada__streams_E");
   E079 : Short_Integer; pragma Import (Ada, E079, "system__file_control_block_E");
   E074 : Short_Integer; pragma Import (Ada, E074, "system__finalization_root_E");
   E072 : Short_Integer; pragma Import (Ada, E072, "ada__finalization_E");
   E071 : Short_Integer; pragma Import (Ada, E071, "system__file_io_E");
   E180 : Short_Integer; pragma Import (Ada, E180, "ada__streams__stream_io_E");
   E166 : Short_Integer; pragma Import (Ada, E166, "system__storage_pools_E");
   E160 : Short_Integer; pragma Import (Ada, E160, "system__finalization_masters_E");
   E174 : Short_Integer; pragma Import (Ada, E174, "system__storage_pools__subpools_E");
   E275 : Short_Integer; pragma Import (Ada, E275, "ada__calendar_E");
   E273 : Short_Integer; pragma Import (Ada, E273, "ada__calendar__delays_E");
   E301 : Short_Integer; pragma Import (Ada, E301, "ada__calendar__time_zones_E");
   E063 : Short_Integer; pragma Import (Ada, E063, "ada__text_io_E");
   E224 : Short_Integer; pragma Import (Ada, E224, "ada__strings__maps_E");
   E308 : Short_Integer; pragma Import (Ada, E308, "ada__strings__maps__constants_E");
   E220 : Short_Integer; pragma Import (Ada, E220, "ada__strings__unbounded_E");
   E318 : Short_Integer; pragma Import (Ada, E318, "system__direct_io_E");
   E170 : Short_Integer; pragma Import (Ada, E170, "system__pool_global_E");
   E285 : Short_Integer; pragma Import (Ada, E285, "system__random_seed_E");
   E314 : Short_Integer; pragma Import (Ada, E314, "system__regexp_E");
   E297 : Short_Integer; pragma Import (Ada, E297, "ada__directories_E");
   E103 : Short_Integer; pragma Import (Ada, E103, "gl_E");
   E113 : Short_Integer; pragma Import (Ada, E113, "gl__types_E");
   E131 : Short_Integer; pragma Import (Ada, E131, "gl__types__colors_E");
   E148 : Short_Integer; pragma Import (Ada, E148, "gl__objects_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "gl__errors_E");
   E194 : Short_Integer; pragma Import (Ada, E194, "gl__objects__shaders_E");
   E196 : Short_Integer; pragma Import (Ada, E196, "gl__objects__shaders__lists_E");
   E192 : Short_Integer; pragma Import (Ada, E192, "gl__objects__programs_E");
   E188 : Short_Integer; pragma Import (Ada, E188, "gl__objects__textures_E");
   E186 : Short_Integer; pragma Import (Ada, E186, "gl__objects__renderbuffers_E");
   E150 : Short_Integer; pragma Import (Ada, E150, "gl__objects__buffers_E");
   E138 : Short_Integer; pragma Import (Ada, E138, "gl__fixed__lighting_E");
   E184 : Short_Integer; pragma Import (Ada, E184, "gl__objects__framebuffers_E");
   E234 : Short_Integer; pragma Import (Ada, E234, "gl__context_E");
   E316 : Short_Integer; pragma Import (Ada, E316, "gl__files_E");
   E289 : Short_Integer; pragma Import (Ada, E289, "gl__objects__vertex_arrays_E");
   E291 : Short_Integer; pragma Import (Ada, E291, "gl__window_E");
   E081 : Short_Integer; pragma Import (Ada, E081, "glfw_E");
   E099 : Short_Integer; pragma Import (Ada, E099, "glfw__monitors_E");
   E091 : Short_Integer; pragma Import (Ada, E091, "glfw__input_E");
   E087 : Short_Integer; pragma Import (Ada, E087, "glfw__errors_E");
   E101 : Short_Integer; pragma Import (Ada, E101, "glfw__windows_E");
   E211 : Short_Integer; pragma Import (Ada, E211, "glfw__windows__context_E");
   E093 : Short_Integer; pragma Import (Ada, E093, "glfw__input__joysticks_E");
   E082 : Short_Integer; pragma Import (Ada, E082, "glfw__api_E");
   E215 : Short_Integer; pragma Import (Ada, E215, "glfw__windows__hints_E");
   E293 : Short_Integer; pragma Import (Ada, E293, "load_vb_object_E");
   E295 : Short_Integer; pragma Import (Ada, E295, "program_loader_E");
   E252 : Short_Integer; pragma Import (Ada, E252, "maths_E");
   E217 : Short_Integer; pragma Import (Ada, E217, "utilities_E");
   E213 : Short_Integer; pragma Import (Ada, E213, "initialize_E");
   E271 : Short_Integer; pragma Import (Ada, E271, "main_loop_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E293 := E293 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "load_vb_object__finalize_spec");
      begin
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "glfw__windows__finalize_body");
      begin
         E101 := E101 - 1;
         F2;
      end;
      E093 := E093 - 1;
      E099 := E099 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "glfw__input__joysticks__finalize_spec");
      begin
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "glfw__windows__finalize_spec");
      begin
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "glfw__monitors__finalize_spec");
      begin
         F5;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "gl__objects__vertex_arrays__finalize_body");
      begin
         E289 := E289 - 1;
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "gl__objects__vertex_arrays__finalize_spec");
      begin
         F7;
      end;
      E192 := E192 - 1;
      E138 := E138 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "gl__objects__buffers__finalize_body");
      begin
         E150 := E150 - 1;
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "gl__objects__renderbuffers__finalize_body");
      begin
         E186 := E186 - 1;
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "gl__objects__framebuffers__finalize_body");
      begin
         E184 := E184 - 1;
         F10;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "gl__objects__textures__finalize_body");
      begin
         E188 := E188 - 1;
         F11;
      end;
      E194 := E194 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "gl__objects__framebuffers__finalize_spec");
      begin
         F12;
      end;
      declare
         procedure F13;
         pragma Import (Ada, F13, "gl__fixed__lighting__finalize_spec");
      begin
         F13;
      end;
      declare
         procedure F14;
         pragma Import (Ada, F14, "gl__objects__buffers__finalize_spec");
      begin
         F14;
      end;
      declare
         procedure F15;
         pragma Import (Ada, F15, "gl__objects__renderbuffers__finalize_spec");
      begin
         F15;
      end;
      declare
         procedure F16;
         pragma Import (Ada, F16, "gl__objects__textures__finalize_spec");
      begin
         F16;
      end;
      declare
         procedure F17;
         pragma Import (Ada, F17, "gl__objects__programs__finalize_spec");
      begin
         F17;
      end;
      declare
         procedure F18;
         pragma Import (Ada, F18, "gl__objects__shaders__finalize_spec");
      begin
         F18;
      end;
      E297 := E297 - 1;
      declare
         procedure F19;
         pragma Import (Ada, F19, "ada__directories__finalize_spec");
      begin
         F19;
      end;
      E314 := E314 - 1;
      declare
         procedure F20;
         pragma Import (Ada, F20, "system__regexp__finalize_spec");
      begin
         F20;
      end;
      E170 := E170 - 1;
      declare
         procedure F21;
         pragma Import (Ada, F21, "system__pool_global__finalize_spec");
      begin
         F21;
      end;
      E318 := E318 - 1;
      declare
         procedure F22;
         pragma Import (Ada, F22, "system__direct_io__finalize_spec");
      begin
         F22;
      end;
      E220 := E220 - 1;
      declare
         procedure F23;
         pragma Import (Ada, F23, "ada__strings__unbounded__finalize_spec");
      begin
         F23;
      end;
      E063 := E063 - 1;
      declare
         procedure F24;
         pragma Import (Ada, F24, "ada__text_io__finalize_spec");
      begin
         F24;
      end;
      E174 := E174 - 1;
      declare
         procedure F25;
         pragma Import (Ada, F25, "system__storage_pools__subpools__finalize_spec");
      begin
         F25;
      end;
      E160 := E160 - 1;
      declare
         procedure F26;
         pragma Import (Ada, F26, "system__finalization_masters__finalize_spec");
      begin
         F26;
      end;
      E180 := E180 - 1;
      declare
         procedure F27;
         pragma Import (Ada, F27, "ada__streams__stream_io__finalize_spec");
      begin
         F27;
      end;
      declare
         procedure F28;
         pragma Import (Ada, F28, "system__file_io__finalize_body");
      begin
         E071 := E071 - 1;
         F28;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E021 := E021 + 1;
      System.Exceptions'Elab_Spec;
      E023 := E023 + 1;
      System.Soft_Links'Elab_Body;
      E011 := E011 + 1;
      System.Secondary_Stack'Elab_Body;
      E015 := E015 + 1;
      Ada.Containers'Elab_Spec;
      E151 := E151 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E066 := E066 + 1;
      Ada.Numerics'Elab_Spec;
      E253 := E253 + 1;
      Ada.Strings'Elab_Spec;
      E218 := E218 + 1;
      Interfaces.C'Elab_Spec;
      E085 := E085 + 1;
      Interfaces.C.Strings'Elab_Spec;
      E089 := E089 + 1;
      System.Os_Lib'Elab_Body;
      E076 := E076 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E050 := E050 + 1;
      Ada.Streams'Elab_Spec;
      E065 := E065 + 1;
      System.File_Control_Block'Elab_Spec;
      E079 := E079 + 1;
      System.Finalization_Root'Elab_Spec;
      E074 := E074 + 1;
      Ada.Finalization'Elab_Spec;
      E072 := E072 + 1;
      System.File_Io'Elab_Body;
      E071 := E071 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E180 := E180 + 1;
      System.Storage_Pools'Elab_Spec;
      E166 := E166 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E160 := E160 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E174 := E174 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E275 := E275 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E273 := E273 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E301 := E301 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E063 := E063 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E224 := E224 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E308 := E308 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E220 := E220 + 1;
      System.Direct_Io'Elab_Spec;
      E318 := E318 + 1;
      System.Pool_Global'Elab_Spec;
      E170 := E170 + 1;
      System.Random_Seed'Elab_Body;
      E285 := E285 + 1;
      System.Regexp'Elab_Spec;
      E314 := E314 + 1;
      Ada.Directories'Elab_Spec;
      Ada.Directories'Elab_Body;
      E297 := E297 + 1;
      GL'ELAB_SPEC;
      GL.TYPES'ELAB_SPEC;
      E113 := E113 + 1;
      GL.TYPES.COLORS'ELAB_SPEC;
      E131 := E131 + 1;
      GL.OBJECTS'ELAB_SPEC;
      GL.OBJECTS'ELAB_BODY;
      E148 := E148 + 1;
      GL.ERRORS'ELAB_SPEC;
      GL.OBJECTS.SHADERS'ELAB_SPEC;
      GL.OBJECTS.SHADERS.LISTS'ELAB_SPEC;
      E196 := E196 + 1;
      GL.OBJECTS.PROGRAMS'ELAB_SPEC;
      GL.OBJECTS.TEXTURES'ELAB_SPEC;
      GL.OBJECTS.RENDERBUFFERS'ELAB_SPEC;
      GL.OBJECTS.BUFFERS'ELAB_SPEC;
      GL.FIXED.LIGHTING'ELAB_SPEC;
      GL.OBJECTS.FRAMEBUFFERS'ELAB_SPEC;
      GL.OBJECTS.SHADERS'ELAB_BODY;
      E194 := E194 + 1;
      GL.OBJECTS.TEXTURES'ELAB_BODY;
      E188 := E188 + 1;
      GL.OBJECTS.FRAMEBUFFERS'ELAB_BODY;
      E184 := E184 + 1;
      GL.OBJECTS.RENDERBUFFERS'ELAB_BODY;
      E186 := E186 + 1;
      GL.OBJECTS.BUFFERS'ELAB_BODY;
      E150 := E150 + 1;
      GL.FIXED.LIGHTING'ELAB_BODY;
      E138 := E138 + 1;
      E134 := E134 + 1;
      GL.OBJECTS.PROGRAMS'ELAB_BODY;
      E192 := E192 + 1;
      E103 := E103 + 1;
      GL.CONTEXT'ELAB_SPEC;
      E234 := E234 + 1;
      E316 := E316 + 1;
      GL.OBJECTS.VERTEX_ARRAYS'ELAB_SPEC;
      GL.OBJECTS.VERTEX_ARRAYS'ELAB_BODY;
      E289 := E289 + 1;
      E291 := E291 + 1;
      Glfw'Elab_Spec;
      Glfw.Monitors'Elab_Spec;
      Glfw.Windows'Elab_Spec;
      Glfw.Input.Joysticks'Elab_Spec;
      Glfw.Api'Elab_Spec;
      E082 := E082 + 1;
      Glfw.Monitors'Elab_Body;
      E099 := E099 + 1;
      Glfw.Input.Joysticks'Elab_Body;
      E093 := E093 + 1;
      E091 := E091 + 1;
      E087 := E087 + 1;
      E081 := E081 + 1;
      Glfw.Windows'Elab_Body;
      E101 := E101 + 1;
      E211 := E211 + 1;
      E215 := E215 + 1;
      Load_Vb_Object'Elab_Spec;
      Load_Vb_Object'Elab_Body;
      E293 := E293 + 1;
      Program_Loader'Elab_Spec;
      E295 := E295 + 1;
      Maths'Elab_Spec;
      E252 := E252 + 1;
      E217 := E217 + 1;
      E213 := E213 + 1;
      E271 := E271 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_transform_feedback");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /Ada_Source/OpenGLAda/examples/redbook/03_feedback/obj/debug/main_loop.o
   --   /Ada_Source/OpenGLAda/examples/redbook/03_feedback/obj/debug/transform_feedback.o
   --   -L/Ada_Source/OpenGLAda/examples/redbook/03_feedback/obj/debug/
   --   -L/Ada_Source/OpenGLAda/examples/redbook/03_feedback/obj/debug/
   --   -L/Ada_Source/OpenGLAda/examples/common/lib/debug/
   --   -L/Ada_Source/OpenGLAda/lib/debug/
   --   -L/Ada_Source/OpenGLAda/FreeTypeAda/lib/debug/
   --   -L/usr/local/gnat/lib/gcc/x86_64-apple-darwin14.5.0/6.3.1/adalib/
   --   -static
   --   -lgnat
--  END Object file/option list   

end ada_main;
