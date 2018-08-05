pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__model_loader.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__model_loader.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__exception_table_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__exceptions_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__soft_links__initialize_E");
   E154 : Short_Integer; pragma Import (Ada, E154, "ada__containers_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "ada__io_exceptions_E");
   E256 : Short_Integer; pragma Import (Ada, E256, "ada__numerics_E");
   E221 : Short_Integer; pragma Import (Ada, E221, "ada__strings_E");
   E087 : Short_Integer; pragma Import (Ada, E087, "interfaces__c_E");
   E091 : Short_Integer; pragma Import (Ada, E091, "interfaces__c__strings_E");
   E078 : Short_Integer; pragma Import (Ada, E078, "system__os_lib_E");
   E052 : Short_Integer; pragma Import (Ada, E052, "ada__tags_E");
   E067 : Short_Integer; pragma Import (Ada, E067, "ada__streams_E");
   E081 : Short_Integer; pragma Import (Ada, E081, "system__file_control_block_E");
   E076 : Short_Integer; pragma Import (Ada, E076, "system__finalization_root_E");
   E074 : Short_Integer; pragma Import (Ada, E074, "ada__finalization_E");
   E073 : Short_Integer; pragma Import (Ada, E073, "system__file_io_E");
   E183 : Short_Integer; pragma Import (Ada, E183, "ada__streams__stream_io_E");
   E169 : Short_Integer; pragma Import (Ada, E169, "system__storage_pools_E");
   E163 : Short_Integer; pragma Import (Ada, E163, "system__finalization_masters_E");
   E177 : Short_Integer; pragma Import (Ada, E177, "system__storage_pools__subpools_E");
   E319 : Short_Integer; pragma Import (Ada, E319, "ada__calendar_E");
   E323 : Short_Integer; pragma Import (Ada, E323, "ada__calendar__time_zones_E");
   E065 : Short_Integer; pragma Import (Ada, E065, "ada__text_io_E");
   E227 : Short_Integer; pragma Import (Ada, E227, "ada__strings__maps_E");
   E332 : Short_Integer; pragma Import (Ada, E332, "ada__strings__maps__constants_E");
   E223 : Short_Integer; pragma Import (Ada, E223, "ada__strings__unbounded_E");
   E341 : Short_Integer; pragma Import (Ada, E341, "system__direct_io_E");
   E173 : Short_Integer; pragma Import (Ada, E173, "system__pool_global_E");
   E337 : Short_Integer; pragma Import (Ada, E337, "system__regexp_E");
   E317 : Short_Integer; pragma Import (Ada, E317, "ada__directories_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "gl_E");
   E115 : Short_Integer; pragma Import (Ada, E115, "gl__types_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "gl__types__colors_E");
   E151 : Short_Integer; pragma Import (Ada, E151, "gl__objects_E");
   E137 : Short_Integer; pragma Import (Ada, E137, "gl__errors_E");
   E197 : Short_Integer; pragma Import (Ada, E197, "gl__objects__shaders_E");
   E199 : Short_Integer; pragma Import (Ada, E199, "gl__objects__shaders__lists_E");
   E195 : Short_Integer; pragma Import (Ada, E195, "gl__objects__programs_E");
   E191 : Short_Integer; pragma Import (Ada, E191, "gl__objects__textures_E");
   E189 : Short_Integer; pragma Import (Ada, E189, "gl__objects__renderbuffers_E");
   E153 : Short_Integer; pragma Import (Ada, E153, "gl__objects__buffers_E");
   E141 : Short_Integer; pragma Import (Ada, E141, "gl__fixed__lighting_E");
   E187 : Short_Integer; pragma Import (Ada, E187, "gl__objects__framebuffers_E");
   E278 : Short_Integer; pragma Import (Ada, E278, "gl__objects__textures__targets_E");
   E351 : Short_Integer; pragma Import (Ada, E351, "api_vectors_matrices_E");
   E339 : Short_Integer; pragma Import (Ada, E339, "gl__files_E");
   E309 : Short_Integer; pragma Import (Ada, E309, "gl__objects__vertex_arrays_E");
   E311 : Short_Integer; pragma Import (Ada, E311, "gl__window_E");
   E083 : Short_Integer; pragma Import (Ada, E083, "glfw_E");
   E101 : Short_Integer; pragma Import (Ada, E101, "glfw__monitors_E");
   E093 : Short_Integer; pragma Import (Ada, E093, "glfw__input_E");
   E089 : Short_Integer; pragma Import (Ada, E089, "glfw__errors_E");
   E103 : Short_Integer; pragma Import (Ada, E103, "glfw__windows_E");
   E214 : Short_Integer; pragma Import (Ada, E214, "glfw__windows__context_E");
   E095 : Short_Integer; pragma Import (Ada, E095, "glfw__input__joysticks_E");
   E084 : Short_Integer; pragma Import (Ada, E084, "glfw__api_E");
   E218 : Short_Integer; pragma Import (Ada, E218, "glfw__windows__hints_E");
   E364 : Short_Integer; pragma Import (Ada, E364, "magick_type_E");
   E378 : Short_Integer; pragma Import (Ada, E378, "magick_pixel_E");
   E345 : Short_Integer; pragma Import (Ada, E345, "ogldev_lights_common_E");
   E315 : Short_Integer; pragma Import (Ada, E315, "program_loader_E");
   E255 : Short_Integer; pragma Import (Ada, E255, "maths_E");
   E313 : Short_Integer; pragma Import (Ada, E313, "ogldev_basic_lighting_E");
   E347 : Short_Integer; pragma Import (Ada, E347, "ogldev_camera_E");
   E349 : Short_Integer; pragma Import (Ada, E349, "ogldev_math_E");
   E354 : Short_Integer; pragma Import (Ada, E354, "ogldev_pipeline_E");
   E362 : Short_Integer; pragma Import (Ada, E362, "core_blob_E");
   E358 : Short_Integer; pragma Import (Ada, E358, "magick_blob_E");
   E376 : Short_Integer; pragma Import (Ada, E376, "magick_exception_E");
   E372 : Short_Integer; pragma Import (Ada, E372, "core_image_E");
   E393 : Short_Integer; pragma Import (Ada, E393, "magick_image_E");
   E356 : Short_Integer; pragma Import (Ada, E356, "ogldev_texture_E");
   E220 : Short_Integer; pragma Import (Ada, E220, "utilities_E");
   E276 : Short_Integer; pragma Import (Ada, E276, "buffers_E");
   E216 : Short_Integer; pragma Import (Ada, E216, "initialize_E");
   E274 : Short_Integer; pragma Import (Ada, E274, "main_loop_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E358 := E358 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "magick_blob__finalize_spec");
      begin
         F1;
      end;
      E349 := E349 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "ogldev_math__finalize_spec");
      begin
         F2;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "glfw__windows__finalize_body");
      begin
         E103 := E103 - 1;
         F3;
      end;
      E095 := E095 - 1;
      E101 := E101 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "glfw__input__joysticks__finalize_spec");
      begin
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "glfw__windows__finalize_spec");
      begin
         F5;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "glfw__monitors__finalize_spec");
      begin
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "gl__objects__vertex_arrays__finalize_body");
      begin
         E309 := E309 - 1;
         F7;
      end;
      declare
         procedure F8;
         pragma Import (Ada, F8, "gl__objects__vertex_arrays__finalize_spec");
      begin
         F8;
      end;
      E278 := E278 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "gl__objects__textures__targets__finalize_spec");
      begin
         F9;
      end;
      E195 := E195 - 1;
      E141 := E141 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "gl__objects__buffers__finalize_body");
      begin
         E153 := E153 - 1;
         F10;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "gl__objects__renderbuffers__finalize_body");
      begin
         E189 := E189 - 1;
         F11;
      end;
      declare
         procedure F12;
         pragma Import (Ada, F12, "gl__objects__framebuffers__finalize_body");
      begin
         E187 := E187 - 1;
         F12;
      end;
      declare
         procedure F13;
         pragma Import (Ada, F13, "gl__objects__textures__finalize_body");
      begin
         E191 := E191 - 1;
         F13;
      end;
      E197 := E197 - 1;
      declare
         procedure F14;
         pragma Import (Ada, F14, "gl__objects__framebuffers__finalize_spec");
      begin
         F14;
      end;
      declare
         procedure F15;
         pragma Import (Ada, F15, "gl__fixed__lighting__finalize_spec");
      begin
         F15;
      end;
      declare
         procedure F16;
         pragma Import (Ada, F16, "gl__objects__buffers__finalize_spec");
      begin
         F16;
      end;
      declare
         procedure F17;
         pragma Import (Ada, F17, "gl__objects__renderbuffers__finalize_spec");
      begin
         F17;
      end;
      declare
         procedure F18;
         pragma Import (Ada, F18, "gl__objects__textures__finalize_spec");
      begin
         F18;
      end;
      declare
         procedure F19;
         pragma Import (Ada, F19, "gl__objects__programs__finalize_spec");
      begin
         F19;
      end;
      declare
         procedure F20;
         pragma Import (Ada, F20, "gl__objects__shaders__finalize_spec");
      begin
         F20;
      end;
      E317 := E317 - 1;
      declare
         procedure F21;
         pragma Import (Ada, F21, "ada__directories__finalize_spec");
      begin
         F21;
      end;
      E337 := E337 - 1;
      declare
         procedure F22;
         pragma Import (Ada, F22, "system__regexp__finalize_spec");
      begin
         F22;
      end;
      E173 := E173 - 1;
      declare
         procedure F23;
         pragma Import (Ada, F23, "system__pool_global__finalize_spec");
      begin
         F23;
      end;
      E341 := E341 - 1;
      declare
         procedure F24;
         pragma Import (Ada, F24, "system__direct_io__finalize_spec");
      begin
         F24;
      end;
      E223 := E223 - 1;
      declare
         procedure F25;
         pragma Import (Ada, F25, "ada__strings__unbounded__finalize_spec");
      begin
         F25;
      end;
      E065 := E065 - 1;
      declare
         procedure F26;
         pragma Import (Ada, F26, "ada__text_io__finalize_spec");
      begin
         F26;
      end;
      E177 := E177 - 1;
      declare
         procedure F27;
         pragma Import (Ada, F27, "system__storage_pools__subpools__finalize_spec");
      begin
         F27;
      end;
      E163 := E163 - 1;
      declare
         procedure F28;
         pragma Import (Ada, F28, "system__finalization_masters__finalize_spec");
      begin
         F28;
      end;
      E183 := E183 - 1;
      declare
         procedure F29;
         pragma Import (Ada, F29, "ada__streams__stream_io__finalize_spec");
      begin
         F29;
      end;
      declare
         procedure F30;
         pragma Import (Ada, F30, "system__file_io__finalize_body");
      begin
         E073 := E073 - 1;
         F30;
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
      E256 := E256 + 1;
      Ada.Strings'Elab_Spec;
      E221 := E221 + 1;
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
      E183 := E183 + 1;
      System.Storage_Pools'Elab_Spec;
      E169 := E169 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E163 := E163 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E177 := E177 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E319 := E319 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E323 := E323 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E065 := E065 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E227 := E227 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E332 := E332 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E223 := E223 + 1;
      System.Direct_Io'Elab_Spec;
      E341 := E341 + 1;
      System.Pool_Global'Elab_Spec;
      E173 := E173 + 1;
      System.Regexp'Elab_Spec;
      E337 := E337 + 1;
      Ada.Directories'Elab_Spec;
      Ada.Directories'Elab_Body;
      E317 := E317 + 1;
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
      E199 := E199 + 1;
      GL.OBJECTS.PROGRAMS'ELAB_SPEC;
      GL.OBJECTS.TEXTURES'ELAB_SPEC;
      GL.OBJECTS.RENDERBUFFERS'ELAB_SPEC;
      GL.OBJECTS.BUFFERS'ELAB_SPEC;
      GL.FIXED.LIGHTING'ELAB_SPEC;
      GL.OBJECTS.FRAMEBUFFERS'ELAB_SPEC;
      GL.OBJECTS.SHADERS'ELAB_BODY;
      E197 := E197 + 1;
      GL.OBJECTS.TEXTURES'ELAB_BODY;
      E191 := E191 + 1;
      GL.OBJECTS.FRAMEBUFFERS'ELAB_BODY;
      E187 := E187 + 1;
      GL.OBJECTS.RENDERBUFFERS'ELAB_BODY;
      E189 := E189 + 1;
      GL.OBJECTS.BUFFERS'ELAB_BODY;
      E153 := E153 + 1;
      GL.FIXED.LIGHTING'ELAB_BODY;
      E141 := E141 + 1;
      E137 := E137 + 1;
      GL.OBJECTS.PROGRAMS'ELAB_BODY;
      E195 := E195 + 1;
      E105 := E105 + 1;
      GL.OBJECTS.TEXTURES.TARGETS'ELAB_SPEC;
      GL.OBJECTS.TEXTURES.TARGETS'ELAB_BODY;
      E278 := E278 + 1;
      Api_Vectors_Matrices'Elab_Spec;
      E351 := E351 + 1;
      E339 := E339 + 1;
      GL.OBJECTS.VERTEX_ARRAYS'ELAB_SPEC;
      GL.OBJECTS.VERTEX_ARRAYS'ELAB_BODY;
      E309 := E309 + 1;
      E311 := E311 + 1;
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
      E214 := E214 + 1;
      E218 := E218 + 1;
      E364 := E364 + 1;
      Magick_Pixel'Elab_Spec;
      E378 := E378 + 1;
      E345 := E345 + 1;
      Program_Loader'Elab_Spec;
      E315 := E315 + 1;
      Maths'Elab_Spec;
      E255 := E255 + 1;
      E313 := E313 + 1;
      Ogldev_Camera'Elab_Body;
      E347 := E347 + 1;
      Ogldev_Math'Elab_Spec;
      E349 := E349 + 1;
      E354 := E354 + 1;
      Core_Blob'Elab_Spec;
      E362 := E362 + 1;
      Magick_Blob'Elab_Spec;
      Magick_Blob'Elab_Body;
      E358 := E358 + 1;
      E376 := E376 + 1;
      Core_Image'Elab_Spec;
      E372 := E372 + 1;
      Magick_Image'Elab_Spec;
      E393 := E393 + 1;
      Ogldev_Texture'Elab_Spec;
      E356 := E356 + 1;
      E220 := E220 + 1;
      E276 := E276 + 1;
      E216 := E216 + 1;
      E274 := E274 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_model_loader");

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
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/ant_tweak_bar.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/api_vectors_matrices.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/assimp_types.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/cache.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/colour_space.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/composite.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/compress.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/layer.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/magick_type.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/geometry.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/magick_pixel.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/method.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/method_attribute.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/magick_profile.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/monitor.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/ogldev_engine_common.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/ogldev_lights_common.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/quantize.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/ogldev_basic_lighting.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/ogldev_camera.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/ogldev_math.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/ogldev_pipeline.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/resample.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/std_io.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/stream.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/sys_pthread_types.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/sys_pthread_mutex.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/thread.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/semaphore.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/core_blob.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/core_blob-api.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/blob_reference.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/magick_blob-api.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/magick_blob.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/magick_exception.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/colour.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/quantum.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/timer.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/core_image.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/draw.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/magick_options.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/image_reference.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/magick_image-api.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/magick_image.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/ogldev_texture.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/buffers.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/main_loop.o
   --   /Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/model_loader.o
   --   -L/Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/
   --   -L/Ada_Source/OpenGLAda/examples/ogl_dev/22_loading_models/obj/
   --   -L/Ada_Source/OpenGLAda/examples/common/lib/
   --   -L/Ada_Source/OpenGLAda/lib/
   --   -L/Ada_Source/OpenGLAda/FreeTypeAda/lib/
   --   -L/opt/gnat/2018/lib/gcc/x86_64-apple-darwin16.7.0/7.3.1/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
--  END Object file/option list   

end ada_main;
