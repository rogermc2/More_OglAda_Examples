pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__colour_vertices.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__colour_vertices.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__exception_table_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__exceptions_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__soft_links__initialize_E");
   E154 : Short_Integer; pragma Import (Ada, E154, "ada__containers_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "ada__io_exceptions_E");
   E248 : Short_Integer; pragma Import (Ada, E248, "ada__numerics_E");
   E217 : Short_Integer; pragma Import (Ada, E217, "ada__strings_E");
   E087 : Short_Integer; pragma Import (Ada, E087, "interfaces__c_E");
   E091 : Short_Integer; pragma Import (Ada, E091, "interfaces__c__strings_E");
   E078 : Short_Integer; pragma Import (Ada, E078, "system__os_lib_E");
   E052 : Short_Integer; pragma Import (Ada, E052, "ada__tags_E");
   E067 : Short_Integer; pragma Import (Ada, E067, "ada__streams_E");
   E081 : Short_Integer; pragma Import (Ada, E081, "system__file_control_block_E");
   E076 : Short_Integer; pragma Import (Ada, E076, "system__finalization_root_E");
   E074 : Short_Integer; pragma Import (Ada, E074, "ada__finalization_E");
   E073 : Short_Integer; pragma Import (Ada, E073, "system__file_io_E");
   E181 : Short_Integer; pragma Import (Ada, E181, "ada__streams__stream_io_E");
   E169 : Short_Integer; pragma Import (Ada, E169, "system__storage_pools_E");
   E163 : Short_Integer; pragma Import (Ada, E163, "system__finalization_masters_E");
   E175 : Short_Integer; pragma Import (Ada, E175, "system__storage_pools__subpools_E");
   E266 : Short_Integer; pragma Import (Ada, E266, "ada__calendar_E");
   E272 : Short_Integer; pragma Import (Ada, E272, "ada__calendar__time_zones_E");
   E065 : Short_Integer; pragma Import (Ada, E065, "ada__text_io_E");
   E223 : Short_Integer; pragma Import (Ada, E223, "ada__strings__maps_E");
   E281 : Short_Integer; pragma Import (Ada, E281, "ada__strings__maps__constants_E");
   E219 : Short_Integer; pragma Import (Ada, E219, "ada__strings__unbounded_E");
   E291 : Short_Integer; pragma Import (Ada, E291, "system__direct_io_E");
   E171 : Short_Integer; pragma Import (Ada, E171, "system__pool_global_E");
   E287 : Short_Integer; pragma Import (Ada, E287, "system__regexp_E");
   E264 : Short_Integer; pragma Import (Ada, E264, "ada__directories_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "gl_E");
   E115 : Short_Integer; pragma Import (Ada, E115, "gl__types_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "gl__types__colors_E");
   E151 : Short_Integer; pragma Import (Ada, E151, "gl__objects_E");
   E137 : Short_Integer; pragma Import (Ada, E137, "gl__errors_E");
   E195 : Short_Integer; pragma Import (Ada, E195, "gl__objects__shaders_E");
   E197 : Short_Integer; pragma Import (Ada, E197, "gl__objects__shaders__lists_E");
   E193 : Short_Integer; pragma Import (Ada, E193, "gl__objects__programs_E");
   E189 : Short_Integer; pragma Import (Ada, E189, "gl__objects__textures_E");
   E187 : Short_Integer; pragma Import (Ada, E187, "gl__objects__renderbuffers_E");
   E153 : Short_Integer; pragma Import (Ada, E153, "gl__objects__buffers_E");
   E141 : Short_Integer; pragma Import (Ada, E141, "gl__fixed__lighting_E");
   E185 : Short_Integer; pragma Import (Ada, E185, "gl__objects__framebuffers_E");
   E289 : Short_Integer; pragma Import (Ada, E289, "gl__files_E");
   E260 : Short_Integer; pragma Import (Ada, E260, "gl__objects__vertex_arrays_E");
   E083 : Short_Integer; pragma Import (Ada, E083, "glfw_E");
   E101 : Short_Integer; pragma Import (Ada, E101, "glfw__monitors_E");
   E093 : Short_Integer; pragma Import (Ada, E093, "glfw__input_E");
   E089 : Short_Integer; pragma Import (Ada, E089, "glfw__errors_E");
   E103 : Short_Integer; pragma Import (Ada, E103, "glfw__windows_E");
   E210 : Short_Integer; pragma Import (Ada, E210, "glfw__windows__context_E");
   E095 : Short_Integer; pragma Import (Ada, E095, "glfw__input__joysticks_E");
   E084 : Short_Integer; pragma Import (Ada, E084, "glfw__api_E");
   E214 : Short_Integer; pragma Import (Ada, E214, "glfw__windows__hints_E");
   E262 : Short_Integer; pragma Import (Ada, E262, "program_loader_E");
   E247 : Short_Integer; pragma Import (Ada, E247, "maths_E");
   E216 : Short_Integer; pragma Import (Ada, E216, "utilities_E");
   E212 : Short_Integer; pragma Import (Ada, E212, "initialize_E");
   E258 : Short_Integer; pragma Import (Ada, E258, "main_loop_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      declare
         procedure F1;
         pragma Import (Ada, F1, "glfw__windows__finalize_body");
      begin
         E103 := E103 - 1;
         F1;
      end;
      E095 := E095 - 1;
      E101 := E101 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "glfw__input__joysticks__finalize_spec");
      begin
         F2;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "glfw__windows__finalize_spec");
      begin
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "glfw__monitors__finalize_spec");
      begin
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "gl__objects__vertex_arrays__finalize_body");
      begin
         E260 := E260 - 1;
         F5;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "gl__objects__vertex_arrays__finalize_spec");
      begin
         F6;
      end;
      E193 := E193 - 1;
      E141 := E141 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "gl__objects__buffers__finalize_body");
      begin
         E153 := E153 - 1;
         F7;
      end;
      declare
         procedure F8;
         pragma Import (Ada, F8, "gl__objects__renderbuffers__finalize_body");
      begin
         E187 := E187 - 1;
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "gl__objects__framebuffers__finalize_body");
      begin
         E185 := E185 - 1;
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "gl__objects__textures__finalize_body");
      begin
         E189 := E189 - 1;
         F10;
      end;
      E195 := E195 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "gl__objects__framebuffers__finalize_spec");
      begin
         F11;
      end;
      declare
         procedure F12;
         pragma Import (Ada, F12, "gl__fixed__lighting__finalize_spec");
      begin
         F12;
      end;
      declare
         procedure F13;
         pragma Import (Ada, F13, "gl__objects__buffers__finalize_spec");
      begin
         F13;
      end;
      declare
         procedure F14;
         pragma Import (Ada, F14, "gl__objects__renderbuffers__finalize_spec");
      begin
         F14;
      end;
      declare
         procedure F15;
         pragma Import (Ada, F15, "gl__objects__textures__finalize_spec");
      begin
         F15;
      end;
      declare
         procedure F16;
         pragma Import (Ada, F16, "gl__objects__programs__finalize_spec");
      begin
         F16;
      end;
      declare
         procedure F17;
         pragma Import (Ada, F17, "gl__objects__shaders__finalize_spec");
      begin
         F17;
      end;
      E264 := E264 - 1;
      declare
         procedure F18;
         pragma Import (Ada, F18, "ada__directories__finalize_spec");
      begin
         F18;
      end;
      E287 := E287 - 1;
      declare
         procedure F19;
         pragma Import (Ada, F19, "system__regexp__finalize_spec");
      begin
         F19;
      end;
      E171 := E171 - 1;
      declare
         procedure F20;
         pragma Import (Ada, F20, "system__pool_global__finalize_spec");
      begin
         F20;
      end;
      E291 := E291 - 1;
      declare
         procedure F21;
         pragma Import (Ada, F21, "system__direct_io__finalize_spec");
      begin
         F21;
      end;
      E219 := E219 - 1;
      declare
         procedure F22;
         pragma Import (Ada, F22, "ada__strings__unbounded__finalize_spec");
      begin
         F22;
      end;
      E065 := E065 - 1;
      declare
         procedure F23;
         pragma Import (Ada, F23, "ada__text_io__finalize_spec");
      begin
         F23;
      end;
      E175 := E175 - 1;
      declare
         procedure F24;
         pragma Import (Ada, F24, "system__storage_pools__subpools__finalize_spec");
      begin
         F24;
      end;
      E163 := E163 - 1;
      declare
         procedure F25;
         pragma Import (Ada, F25, "system__finalization_masters__finalize_spec");
      begin
         F25;
      end;
      E181 := E181 - 1;
      declare
         procedure F26;
         pragma Import (Ada, F26, "ada__streams__stream_io__finalize_spec");
      begin
         F26;
      end;
      declare
         procedure F27;
         pragma Import (Ada, F27, "system__file_io__finalize_body");
      begin
         E073 := E073 - 1;
         F27;
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
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

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

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E023 := E023 + 1;
      System.Exceptions'Elab_Spec;
      E025 := E025 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E019 := E019 + 1;
      E011 := E011 + 1;
      Ada.Containers'Elab_Spec;
      E154 := E154 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E068 := E068 + 1;
      Ada.Numerics'Elab_Spec;
      E248 := E248 + 1;
      Ada.Strings'Elab_Spec;
      E217 := E217 + 1;
      Interfaces.C'Elab_Spec;
      E087 := E087 + 1;
      Interfaces.C.Strings'Elab_Spec;
      E091 := E091 + 1;
      System.Os_Lib'Elab_Body;
      E078 := E078 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E052 := E052 + 1;
      Ada.Streams'Elab_Spec;
      E067 := E067 + 1;
      System.File_Control_Block'Elab_Spec;
      E081 := E081 + 1;
      System.Finalization_Root'Elab_Spec;
      E076 := E076 + 1;
      Ada.Finalization'Elab_Spec;
      E074 := E074 + 1;
      System.File_Io'Elab_Body;
      E073 := E073 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E181 := E181 + 1;
      System.Storage_Pools'Elab_Spec;
      E169 := E169 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E163 := E163 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E175 := E175 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E266 := E266 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E272 := E272 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E065 := E065 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E223 := E223 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E281 := E281 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E219 := E219 + 1;
      System.Direct_Io'Elab_Spec;
      E291 := E291 + 1;
      System.Pool_Global'Elab_Spec;
      E171 := E171 + 1;
      System.Regexp'Elab_Spec;
      E287 := E287 + 1;
      Ada.Directories'Elab_Spec;
      Ada.Directories'Elab_Body;
      E264 := E264 + 1;
      GL'ELAB_SPEC;
      GL.TYPES'ELAB_SPEC;
      E115 := E115 + 1;
      GL.TYPES.COLORS'ELAB_SPEC;
      E134 := E134 + 1;
      GL.OBJECTS'ELAB_SPEC;
      GL.OBJECTS'ELAB_BODY;
      E151 := E151 + 1;
      GL.ERRORS'ELAB_SPEC;
      GL.OBJECTS.SHADERS'ELAB_SPEC;
      GL.OBJECTS.SHADERS.LISTS'ELAB_SPEC;
      E197 := E197 + 1;
      GL.OBJECTS.PROGRAMS'ELAB_SPEC;
      GL.OBJECTS.TEXTURES'ELAB_SPEC;
      GL.OBJECTS.RENDERBUFFERS'ELAB_SPEC;
      GL.OBJECTS.BUFFERS'ELAB_SPEC;
      GL.FIXED.LIGHTING'ELAB_SPEC;
      GL.OBJECTS.FRAMEBUFFERS'ELAB_SPEC;
      GL.OBJECTS.SHADERS'ELAB_BODY;
      E195 := E195 + 1;
      GL.OBJECTS.TEXTURES'ELAB_BODY;
      E189 := E189 + 1;
      GL.OBJECTS.FRAMEBUFFERS'ELAB_BODY;
      E185 := E185 + 1;
      GL.OBJECTS.RENDERBUFFERS'ELAB_BODY;
      E187 := E187 + 1;
      GL.OBJECTS.BUFFERS'ELAB_BODY;
      E153 := E153 + 1;
      GL.FIXED.LIGHTING'ELAB_BODY;
      E141 := E141 + 1;
      E137 := E137 + 1;
      GL.OBJECTS.PROGRAMS'ELAB_BODY;
      E193 := E193 + 1;
      E105 := E105 + 1;
      E289 := E289 + 1;
      GL.OBJECTS.VERTEX_ARRAYS'ELAB_SPEC;
      GL.OBJECTS.VERTEX_ARRAYS'ELAB_BODY;
      E260 := E260 + 1;
      Glfw'Elab_Spec;
      Glfw.Monitors'Elab_Spec;
      Glfw.Windows'Elab_Spec;
      Glfw.Input.Joysticks'Elab_Spec;
      Glfw.Api'Elab_Spec;
      E084 := E084 + 1;
      Glfw.Monitors'Elab_Body;
      E101 := E101 + 1;
      Glfw.Input.Joysticks'Elab_Body;
      E095 := E095 + 1;
      E093 := E093 + 1;
      E089 := E089 + 1;
      E083 := E083 + 1;
      Glfw.Windows'Elab_Body;
      E103 := E103 + 1;
      E210 := E210 + 1;
      E214 := E214 + 1;
      Program_Loader'Elab_Spec;
      E262 := E262 + 1;
      Maths'Elab_Spec;
      E247 := E247 + 1;
      E216 := E216 + 1;
      E212 := E212 + 1;
      E258 := E258 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_colour_vertices");

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
   --   /Ada_Source/OpenGLAda/examples/superbible/vertex_colouring/obj/main_loop.o
   --   /Ada_Source/OpenGLAda/examples/superbible/vertex_colouring/obj/colour_vertices.o
   --   -L/Ada_Source/OpenGLAda/examples/superbible/vertex_colouring/obj/
   --   -L/Ada_Source/OpenGLAda/examples/superbible/vertex_colouring/obj/
   --   -L/Ada_Source/OpenGLAda/examples/common/lib/
   --   -L/Ada_Source/OpenGLAda/lib/
   --   -L/Ada_Source/OpenGLAda/FreeTypeAda/lib/
   --   -L/opt/gnat/2018/lib/gcc/x86_64-apple-darwin16.7.0/7.3.1/adalib/
   --   -static
   --   -lgnat
--  END Object file/option list   

end ada_main;
