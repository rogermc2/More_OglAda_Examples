pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__tutorial_4.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__tutorial_4.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "system__exception_table_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__exceptions_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__secondary_stack_E");
   E137 : Short_Integer; pragma Import (Ada, E137, "ada__containers_E");
   E066 : Short_Integer; pragma Import (Ada, E066, "ada__io_exceptions_E");
   E251 : Short_Integer; pragma Import (Ada, E251, "ada__numerics_E");
   E216 : Short_Integer; pragma Import (Ada, E216, "ada__strings_E");
   E101 : Short_Integer; pragma Import (Ada, E101, "interfaces__c_E");
   E103 : Short_Integer; pragma Import (Ada, E103, "interfaces__c__strings_E");
   E076 : Short_Integer; pragma Import (Ada, E076, "system__os_lib_E");
   E050 : Short_Integer; pragma Import (Ada, E050, "ada__tags_E");
   E065 : Short_Integer; pragma Import (Ada, E065, "ada__streams_E");
   E079 : Short_Integer; pragma Import (Ada, E079, "system__file_control_block_E");
   E074 : Short_Integer; pragma Import (Ada, E074, "system__finalization_root_E");
   E072 : Short_Integer; pragma Import (Ada, E072, "ada__finalization_E");
   E071 : Short_Integer; pragma Import (Ada, E071, "system__file_io_E");
   E166 : Short_Integer; pragma Import (Ada, E166, "ada__streams__stream_io_E");
   E152 : Short_Integer; pragma Import (Ada, E152, "system__storage_pools_E");
   E146 : Short_Integer; pragma Import (Ada, E146, "system__finalization_masters_E");
   E160 : Short_Integer; pragma Import (Ada, E160, "system__storage_pools__subpools_E");
   E284 : Short_Integer; pragma Import (Ada, E284, "ada__calendar_E");
   E290 : Short_Integer; pragma Import (Ada, E290, "ada__calendar__time_zones_E");
   E063 : Short_Integer; pragma Import (Ada, E063, "ada__text_io_E");
   E222 : Short_Integer; pragma Import (Ada, E222, "ada__strings__maps_E");
   E299 : Short_Integer; pragma Import (Ada, E299, "ada__strings__maps__constants_E");
   E218 : Short_Integer; pragma Import (Ada, E218, "ada__strings__unbounded_E");
   E307 : Short_Integer; pragma Import (Ada, E307, "system__direct_io_E");
   E156 : Short_Integer; pragma Import (Ada, E156, "system__pool_global_E");
   E305 : Short_Integer; pragma Import (Ada, E305, "system__regexp_E");
   E282 : Short_Integer; pragma Import (Ada, E282, "ada__directories_E");
   E083 : Short_Integer; pragma Import (Ada, E083, "gl_E");
   E093 : Short_Integer; pragma Import (Ada, E093, "gl__types_E");
   E115 : Short_Integer; pragma Import (Ada, E115, "gl__types__colors_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "gl__objects_E");
   E118 : Short_Integer; pragma Import (Ada, E118, "gl__errors_E");
   E180 : Short_Integer; pragma Import (Ada, E180, "gl__objects__shaders_E");
   E182 : Short_Integer; pragma Import (Ada, E182, "gl__objects__shaders__lists_E");
   E178 : Short_Integer; pragma Import (Ada, E178, "gl__objects__programs_E");
   E174 : Short_Integer; pragma Import (Ada, E174, "gl__objects__textures_E");
   E172 : Short_Integer; pragma Import (Ada, E172, "gl__objects__renderbuffers_E");
   E136 : Short_Integer; pragma Import (Ada, E136, "gl__objects__buffers_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "gl__fixed__lighting_E");
   E170 : Short_Integer; pragma Import (Ada, E170, "gl__objects__framebuffers_E");
   E232 : Short_Integer; pragma Import (Ada, E232, "gl__context_E");
   E280 : Short_Integer; pragma Import (Ada, E280, "gl__files_E");
   E274 : Short_Integer; pragma Import (Ada, E274, "gl__objects__vertex_arrays_E");
   E276 : Short_Integer; pragma Import (Ada, E276, "gl__window_E");
   E081 : Short_Integer; pragma Import (Ada, E081, "glfw_E");
   E205 : Short_Integer; pragma Import (Ada, E205, "glfw__monitors_E");
   E199 : Short_Integer; pragma Import (Ada, E199, "glfw__input_E");
   E197 : Short_Integer; pragma Import (Ada, E197, "glfw__errors_E");
   E207 : Short_Integer; pragma Import (Ada, E207, "glfw__windows_E");
   E209 : Short_Integer; pragma Import (Ada, E209, "glfw__windows__context_E");
   E201 : Short_Integer; pragma Import (Ada, E201, "glfw__input__joysticks_E");
   E194 : Short_Integer; pragma Import (Ada, E194, "glfw__api_E");
   E213 : Short_Integer; pragma Import (Ada, E213, "glfw__windows__hints_E");
   E278 : Short_Integer; pragma Import (Ada, E278, "program_loader_E");
   E256 : Short_Integer; pragma Import (Ada, E256, "quaternions_E");
   E250 : Short_Integer; pragma Import (Ada, E250, "maths_E");
   E215 : Short_Integer; pragma Import (Ada, E215, "utilities_E");
   E211 : Short_Integer; pragma Import (Ada, E211, "initialize_E");
   E271 : Short_Integer; pragma Import (Ada, E271, "main_loop_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E201 := E201 - 1;
      E205 := E205 - 1;
      E207 := E207 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "glfw__input__joysticks__finalize_spec");
      begin
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "glfw__windows__finalize_spec");
      begin
         F2;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "glfw__monitors__finalize_spec");
      begin
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "gl__objects__vertex_arrays__finalize_body");
      begin
         E274 := E274 - 1;
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "gl__objects__vertex_arrays__finalize_spec");
      begin
         F5;
      end;
      E178 := E178 - 1;
      E122 := E122 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "gl__objects__buffers__finalize_body");
      begin
         E136 := E136 - 1;
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "gl__objects__renderbuffers__finalize_body");
      begin
         E172 := E172 - 1;
         F7;
      end;
      declare
         procedure F8;
         pragma Import (Ada, F8, "gl__objects__framebuffers__finalize_body");
      begin
         E170 := E170 - 1;
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "gl__objects__textures__finalize_body");
      begin
         E174 := E174 - 1;
         F9;
      end;
      E180 := E180 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "gl__objects__framebuffers__finalize_spec");
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
         pragma Import (Ada, F12, "gl__objects__buffers__finalize_spec");
      begin
         F12;
      end;
      declare
         procedure F13;
         pragma Import (Ada, F13, "gl__objects__renderbuffers__finalize_spec");
      begin
         F13;
      end;
      declare
         procedure F14;
         pragma Import (Ada, F14, "gl__objects__textures__finalize_spec");
      begin
         F14;
      end;
      declare
         procedure F15;
         pragma Import (Ada, F15, "gl__objects__programs__finalize_spec");
      begin
         F15;
      end;
      declare
         procedure F16;
         pragma Import (Ada, F16, "gl__objects__shaders__finalize_spec");
      begin
         F16;
      end;
      E282 := E282 - 1;
      declare
         procedure F17;
         pragma Import (Ada, F17, "ada__directories__finalize_spec");
      begin
         F17;
      end;
      E305 := E305 - 1;
      declare
         procedure F18;
         pragma Import (Ada, F18, "system__regexp__finalize_spec");
      begin
         F18;
      end;
      E156 := E156 - 1;
      declare
         procedure F19;
         pragma Import (Ada, F19, "system__pool_global__finalize_spec");
      begin
         F19;
      end;
      E307 := E307 - 1;
      declare
         procedure F20;
         pragma Import (Ada, F20, "system__direct_io__finalize_spec");
      begin
         F20;
      end;
      E218 := E218 - 1;
      declare
         procedure F21;
         pragma Import (Ada, F21, "ada__strings__unbounded__finalize_spec");
      begin
         F21;
      end;
      E063 := E063 - 1;
      declare
         procedure F22;
         pragma Import (Ada, F22, "ada__text_io__finalize_spec");
      begin
         F22;
      end;
      E160 := E160 - 1;
      declare
         procedure F23;
         pragma Import (Ada, F23, "system__storage_pools__subpools__finalize_spec");
      begin
         F23;
      end;
      E146 := E146 - 1;
      declare
         procedure F24;
         pragma Import (Ada, F24, "system__finalization_masters__finalize_spec");
      begin
         F24;
      end;
      E166 := E166 - 1;
      declare
         procedure F25;
         pragma Import (Ada, F25, "ada__streams__stream_io__finalize_spec");
      begin
         F25;
      end;
      declare
         procedure F26;
         pragma Import (Ada, F26, "system__file_io__finalize_body");
      begin
         E071 := E071 - 1;
         F26;
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
      E137 := E137 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E066 := E066 + 1;
      Ada.Numerics'Elab_Spec;
      E251 := E251 + 1;
      Ada.Strings'Elab_Spec;
      E216 := E216 + 1;
      Interfaces.C'Elab_Spec;
      E101 := E101 + 1;
      Interfaces.C.Strings'Elab_Spec;
      E103 := E103 + 1;
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
      E166 := E166 + 1;
      System.Storage_Pools'Elab_Spec;
      E152 := E152 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E146 := E146 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E160 := E160 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E284 := E284 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E290 := E290 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E063 := E063 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E222 := E222 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E299 := E299 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E218 := E218 + 1;
      System.Direct_Io'Elab_Spec;
      E307 := E307 + 1;
      System.Pool_Global'Elab_Spec;
      E156 := E156 + 1;
      System.Regexp'Elab_Spec;
      E305 := E305 + 1;
      Ada.Directories'Elab_Spec;
      Ada.Directories'Elab_Body;
      E282 := E282 + 1;
      GL'ELAB_SPEC;
      GL.TYPES'ELAB_SPEC;
      E093 := E093 + 1;
      GL.TYPES.COLORS'ELAB_SPEC;
      E115 := E115 + 1;
      GL.OBJECTS'ELAB_SPEC;
      GL.OBJECTS'ELAB_BODY;
      E134 := E134 + 1;
      GL.ERRORS'ELAB_SPEC;
      GL.OBJECTS.SHADERS'ELAB_SPEC;
      GL.OBJECTS.SHADERS.LISTS'ELAB_SPEC;
      E182 := E182 + 1;
      GL.OBJECTS.PROGRAMS'ELAB_SPEC;
      GL.OBJECTS.TEXTURES'ELAB_SPEC;
      GL.OBJECTS.RENDERBUFFERS'ELAB_SPEC;
      GL.OBJECTS.BUFFERS'ELAB_SPEC;
      GL.FIXED.LIGHTING'ELAB_SPEC;
      GL.OBJECTS.FRAMEBUFFERS'ELAB_SPEC;
      GL.OBJECTS.SHADERS'ELAB_BODY;
      E180 := E180 + 1;
      GL.OBJECTS.TEXTURES'ELAB_BODY;
      E174 := E174 + 1;
      GL.OBJECTS.FRAMEBUFFERS'ELAB_BODY;
      E170 := E170 + 1;
      GL.OBJECTS.RENDERBUFFERS'ELAB_BODY;
      E172 := E172 + 1;
      GL.OBJECTS.BUFFERS'ELAB_BODY;
      E136 := E136 + 1;
      GL.FIXED.LIGHTING'ELAB_BODY;
      E122 := E122 + 1;
      E118 := E118 + 1;
      GL.OBJECTS.PROGRAMS'ELAB_BODY;
      E178 := E178 + 1;
      E083 := E083 + 1;
      GL.CONTEXT'ELAB_SPEC;
      E232 := E232 + 1;
      E280 := E280 + 1;
      GL.OBJECTS.VERTEX_ARRAYS'ELAB_SPEC;
      GL.OBJECTS.VERTEX_ARRAYS'ELAB_BODY;
      E274 := E274 + 1;
      E276 := E276 + 1;
      Glfw'Elab_Spec;
      Glfw.Monitors'Elab_Spec;
      Glfw.Windows'Elab_Spec;
      Glfw.Input.Joysticks'Elab_Spec;
      Glfw.Api'Elab_Spec;
      E194 := E194 + 1;
      Glfw.Windows'Elab_Body;
      E207 := E207 + 1;
      Glfw.Monitors'Elab_Body;
      E205 := E205 + 1;
      Glfw.Input.Joysticks'Elab_Body;
      E201 := E201 + 1;
      E199 := E199 + 1;
      E197 := E197 + 1;
      E081 := E081 + 1;
      E209 := E209 + 1;
      E213 := E213 + 1;
      Program_Loader'Elab_Spec;
      E278 := E278 + 1;
      E256 := E256 + 1;
      Maths'Elab_Spec;
      E250 := E250 + 1;
      E215 := E215 + 1;
      E211 := E211 + 1;
      E271 := E271 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_tutorial_4");

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
   --   /Ada_Source/OpenGLAda/examples/ogl_tutorials/04_coloured_cube/obj/cube_data.o
   --   /Ada_Source/OpenGLAda/examples/ogl_tutorials/04_coloured_cube/obj/program_loader.o
   --   /Ada_Source/OpenGLAda/examples/ogl_tutorials/04_coloured_cube/obj/quaternions.o
   --   /Ada_Source/OpenGLAda/examples/ogl_tutorials/04_coloured_cube/obj/maths.o
   --   /Ada_Source/OpenGLAda/examples/ogl_tutorials/04_coloured_cube/obj/utilities.o
   --   /Ada_Source/OpenGLAda/examples/ogl_tutorials/04_coloured_cube/obj/initialize.o
   --   /Ada_Source/OpenGLAda/examples/ogl_tutorials/04_coloured_cube/obj/main_loop.o
   --   /Ada_Source/OpenGLAda/examples/ogl_tutorials/04_coloured_cube/obj/tutorial_4.o
   --   -L/Ada_Source/OpenGLAda/examples/ogl_tutorials/04_coloured_cube/obj/
   --   -L/Ada_Source/OpenGLAda/examples/ogl_tutorials/04_coloured_cube/obj/
   --   -L/Ada_Source/OpenGLAda/lib/
   --   -L/usr/local/gnat/lib/gcc/x86_64-apple-darwin14.5.0/6.3.1/adalib/
   --   -static
   --   -lgnat
--  END Object file/option list   

end ada_main;
