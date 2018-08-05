pragma Ada_95;
pragma Warnings (Off);
pragma Source_File_Name (ada_main, Spec_File_Name => "b__hello.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__hello.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E077 : Short_Integer; pragma Import (Ada, E077, "system__os_lib_E");
   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "system__exception_table_E");
   E136 : Short_Integer; pragma Import (Ada, E136, "ada__containers_E");
   E065 : Short_Integer; pragma Import (Ada, E065, "ada__io_exceptions_E");
   E217 : Short_Integer; pragma Import (Ada, E217, "ada__strings_E");
   E223 : Short_Integer; pragma Import (Ada, E223, "ada__strings__maps_E");
   E049 : Short_Integer; pragma Import (Ada, E049, "ada__tags_E");
   E064 : Short_Integer; pragma Import (Ada, E064, "ada__streams_E");
   E075 : Short_Integer; pragma Import (Ada, E075, "interfaces__c_E");
   E102 : Short_Integer; pragma Import (Ada, E102, "interfaces__c__strings_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__exceptions_E");
   E080 : Short_Integer; pragma Import (Ada, E080, "system__file_control_block_E");
   E165 : Short_Integer; pragma Import (Ada, E165, "ada__streams__stream_io_E");
   E070 : Short_Integer; pragma Import (Ada, E070, "system__file_io_E");
   E073 : Short_Integer; pragma Import (Ada, E073, "system__finalization_root_E");
   E071 : Short_Integer; pragma Import (Ada, E071, "ada__finalization_E");
   E151 : Short_Integer; pragma Import (Ada, E151, "system__storage_pools_E");
   E145 : Short_Integer; pragma Import (Ada, E145, "system__finalization_masters_E");
   E159 : Short_Integer; pragma Import (Ada, E159, "system__storage_pools__subpools_E");
   E155 : Short_Integer; pragma Import (Ada, E155, "system__pool_global_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__secondary_stack_E");
   E219 : Short_Integer; pragma Import (Ada, E219, "ada__strings__unbounded_E");
   E062 : Short_Integer; pragma Import (Ada, E062, "ada__text_io_E");
   E084 : Short_Integer; pragma Import (Ada, E084, "gl_E");
   E094 : Short_Integer; pragma Import (Ada, E094, "gl__types_E");
   E233 : Short_Integer; pragma Import (Ada, E233, "gl__context_E");
   E117 : Short_Integer; pragma Import (Ada, E117, "gl__errors_E");
   E133 : Short_Integer; pragma Import (Ada, E133, "gl__objects_E");
   E135 : Short_Integer; pragma Import (Ada, E135, "gl__objects__buffers_E");
   E179 : Short_Integer; pragma Import (Ada, E179, "gl__objects__shaders_E");
   E181 : Short_Integer; pragma Import (Ada, E181, "gl__objects__shaders__lists_E");
   E171 : Short_Integer; pragma Import (Ada, E171, "gl__objects__renderbuffers_E");
   E114 : Short_Integer; pragma Import (Ada, E114, "gl__types__colors_E");
   E121 : Short_Integer; pragma Import (Ada, E121, "gl__fixed__lighting_E");
   E173 : Short_Integer; pragma Import (Ada, E173, "gl__objects__textures_E");
   E169 : Short_Integer; pragma Import (Ada, E169, "gl__objects__framebuffers_E");
   E177 : Short_Integer; pragma Import (Ada, E177, "gl__objects__programs_E");
   E082 : Short_Integer; pragma Import (Ada, E082, "glfw_E");
   E198 : Short_Integer; pragma Import (Ada, E198, "glfw__errors_E");
   E200 : Short_Integer; pragma Import (Ada, E200, "glfw__input_E");
   E202 : Short_Integer; pragma Import (Ada, E202, "glfw__input__joysticks_E");
   E206 : Short_Integer; pragma Import (Ada, E206, "glfw__monitors_E");
   E208 : Short_Integer; pragma Import (Ada, E208, "glfw__windows_E");
   E210 : Short_Integer; pragma Import (Ada, E210, "glfw__windows__context_E");
   E195 : Short_Integer; pragma Import (Ada, E195, "glfw__api_E");
   E214 : Short_Integer; pragma Import (Ada, E214, "glfw__windows__hints_E");
   E212 : Short_Integer; pragma Import (Ada, E212, "initialize_E");
   E239 : Short_Integer; pragma Import (Ada, E239, "main_loop_E");
   E216 : Short_Integer; pragma Import (Ada, E216, "utilities_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E202 := E202 - 1;
      E206 := E206 - 1;
      E208 := E208 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "glfw__windows__finalize_spec");
      begin
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "glfw__monitors__finalize_spec");
      begin
         F2;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "glfw__input__joysticks__finalize_spec");
      begin
         F3;
      end;
      E121 := E121 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "gl__objects__buffers__finalize_body");
      begin
         E135 := E135 - 1;
         F4;
      end;
      E179 := E179 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "gl__objects__renderbuffers__finalize_body");
      begin
         E171 := E171 - 1;
         F5;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "gl__objects__textures__finalize_body");
      begin
         E173 := E173 - 1;
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "gl__objects__framebuffers__finalize_body");
      begin
         E169 := E169 - 1;
         F7;
      end;
      E177 := E177 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "gl__objects__programs__finalize_spec");
      begin
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "gl__objects__framebuffers__finalize_spec");
      begin
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "gl__objects__textures__finalize_spec");
      begin
         F10;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "gl__fixed__lighting__finalize_spec");
      begin
         F11;
      end;
      declare
         procedure F12;
         pragma Import (Ada, F12, "gl__objects__renderbuffers__finalize_spec");
      begin
         F12;
      end;
      declare
         procedure F13;
         pragma Import (Ada, F13, "gl__objects__shaders__finalize_spec");
      begin
         F13;
      end;
      declare
         procedure F14;
         pragma Import (Ada, F14, "gl__objects__buffers__finalize_spec");
      begin
         F14;
      end;
      E062 := E062 - 1;
      declare
         procedure F15;
         pragma Import (Ada, F15, "ada__text_io__finalize_spec");
      begin
         F15;
      end;
      E219 := E219 - 1;
      declare
         procedure F16;
         pragma Import (Ada, F16, "ada__strings__unbounded__finalize_spec");
      begin
         F16;
      end;
      declare
         procedure F17;
         pragma Import (Ada, F17, "system__file_io__finalize_body");
      begin
         E070 := E070 - 1;
         F17;
      end;
      E145 := E145 - 1;
      E159 := E159 - 1;
      E155 := E155 - 1;
      declare
         procedure F18;
         pragma Import (Ada, F18, "system__pool_global__finalize_spec");
      begin
         F18;
      end;
      declare
         procedure F19;
         pragma Import (Ada, F19, "system__storage_pools__subpools__finalize_spec");
      begin
         F19;
      end;
      declare
         procedure F20;
         pragma Import (Ada, F20, "system__finalization_masters__finalize_spec");
      begin
         F20;
      end;
      E165 := E165 - 1;
      declare
         procedure F21;
         pragma Import (Ada, F21, "ada__streams__stream_io__finalize_spec");
      begin
         F21;
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
      Ada.Containers'Elab_Spec;
      E136 := E136 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E065 := E065 + 1;
      Ada.Strings'Elab_Spec;
      E217 := E217 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E064 := E064 + 1;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E023 := E023 + 1;
      System.File_Control_Block'Elab_Spec;
      E080 := E080 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E165 := E165 + 1;
      System.Finalization_Root'Elab_Spec;
      E073 := E073 + 1;
      Ada.Finalization'Elab_Spec;
      E071 := E071 + 1;
      System.Storage_Pools'Elab_Spec;
      E151 := E151 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Storage_Pools.Subpools'Elab_Spec;
      System.Pool_Global'Elab_Spec;
      E155 := E155 + 1;
      E159 := E159 + 1;
      System.Finalization_Masters'Elab_Body;
      E145 := E145 + 1;
      System.File_Io'Elab_Body;
      E070 := E070 + 1;
      E102 := E102 + 1;
      E075 := E075 + 1;
      Ada.Tags'Elab_Body;
      E049 := E049 + 1;
      E223 := E223 + 1;
      System.Soft_Links'Elab_Body;
      E011 := E011 + 1;
      System.Os_Lib'Elab_Body;
      E077 := E077 + 1;
      System.Secondary_Stack'Elab_Body;
      E015 := E015 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E219 := E219 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E062 := E062 + 1;
      GL'ELAB_SPEC;
      GL.TYPES'ELAB_SPEC;
      E094 := E094 + 1;
      GL.CONTEXT'ELAB_SPEC;
      GL.ERRORS'ELAB_SPEC;
      GL.OBJECTS'ELAB_SPEC;
      E133 := E133 + 1;
      GL.OBJECTS.BUFFERS'ELAB_SPEC;
      GL.OBJECTS.SHADERS'ELAB_SPEC;
      GL.OBJECTS.SHADERS.LISTS'ELAB_SPEC;
      E181 := E181 + 1;
      GL.OBJECTS.RENDERBUFFERS'ELAB_SPEC;
      GL.TYPES.COLORS'ELAB_SPEC;
      E114 := E114 + 1;
      GL.FIXED.LIGHTING'ELAB_SPEC;
      GL.OBJECTS.TEXTURES'ELAB_SPEC;
      GL.OBJECTS.FRAMEBUFFERS'ELAB_SPEC;
      GL.OBJECTS.PROGRAMS'ELAB_SPEC;
      E177 := E177 + 1;
      GL.OBJECTS.FRAMEBUFFERS'ELAB_BODY;
      E169 := E169 + 1;
      GL.OBJECTS.TEXTURES'ELAB_BODY;
      E173 := E173 + 1;
      GL.OBJECTS.RENDERBUFFERS'ELAB_BODY;
      E171 := E171 + 1;
      E179 := E179 + 1;
      GL.OBJECTS.BUFFERS'ELAB_BODY;
      E135 := E135 + 1;
      E117 := E117 + 1;
      E233 := E233 + 1;
      E121 := E121 + 1;
      E084 := E084 + 1;
      Glfw'Elab_Spec;
      Glfw.Input.Joysticks'Elab_Spec;
      Glfw.Monitors'Elab_Spec;
      Glfw.Windows'Elab_Spec;
      Glfw.Api'Elab_Spec;
      E195 := E195 + 1;
      E210 := E210 + 1;
      E208 := E208 + 1;
      E206 := E206 + 1;
      E202 := E202 + 1;
      E200 := E200 + 1;
      E198 := E198 + 1;
      E082 := E082 + 1;
      E214 := E214 + 1;
      E216 := E216 + 1;
      E239 := E239 + 1;
      E212 := E212 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_hello");

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
   --   /Ada_Source/OpenGLAda/examples/durian/hello/obj/hello.o
   --   /Ada_Source/OpenGLAda/examples/durian/hello/obj/utilities.o
   --   /Ada_Source/OpenGLAda/examples/durian/hello/obj/main_loop.o
   --   /Ada_Source/OpenGLAda/examples/durian/hello/obj/initialize.o
   --   -L/Ada_Source/OpenGLAda/examples/durian/hello/obj/
   --   -L/Ada_Source/OpenGLAda/examples/durian/hello/obj/
   --   -L/Ada_Source/OpenGLAda/lib/
   --   -L/opt/gcc-6.1.0/lib/gcc/x86_64-apple-darwin15/6.1.0/adalib/
   --   -static
   --   -lgnat
--  END Object file/option list   

end ada_main;
