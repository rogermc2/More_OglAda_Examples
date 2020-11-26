
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Blending;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

with Buffers;
with Keyboard_Handler;
with Textures;

procedure Main_Loop (Main_Window : in out Input_Callback.Callback_Window) is
   use GL.Types;
   use GL.Uniforms;

   VAO             : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Shader_Program  : GL.Objects.Programs.Program;
   Coord_Attribute : GL.Attributes.Attribute;
   Offset_X_ID     : Uniform := -1;
   Scale_X_ID      : Uniform := -1;
   Sprite_ID       : Uniform := -1;
   Texture_ID      : Uniform := -1;
   theTexture      : GL.Objects.Textures.Texture;
   Res_Tex_Width   : Int := 15;
   Vertices_Buffer : GL.Objects.Buffers.Buffer;
   Offset_X        : Single := 0.0;
   Scale_X         : Single := 1.0;
   Mode            : Keyboard_Handler.Mode_Range := 0;

   Background      : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 0.0);

   --  ------------------------------------------------------------------------

   function Build_Shader_Program return Boolean is
      use GL.Attributes;
      use GL.Objects.Shaders;
      use GL.Uniforms;
      use Program_Loader;
      OK : Boolean := False;
   begin
      Shader_Program := Program_From
        ((Src ("src/shaders/graph_1.vert", Vertex_Shader),
         Src ("src/shaders/graph_1.frag", Fragment_Shader)));

      OK := GL.Objects.Programs.Link_Status (Shader_Program);
      if not OK then
         Put_Line ("Build_Shader_Program, Shader_Program Link failed");
         Put_Line (GL.Objects.Programs.Info_Log (Shader_Program));
      else
         GL.Objects.Programs.Use_Program (Shader_Program);
         Coord_Attribute := GL.Objects.Programs.Attrib_Location (Shader_Program, "coord2d");
         OK := Integer (Coord_Attribute) > -1;
         if OK then
            Offset_X_ID :=
              GL.Objects.Programs.Uniform_Location (Shader_Program, "offset_x");
            Scale_X_ID :=
              GL.Objects.Programs.Uniform_Location (Shader_Program, "scale_x");
            Sprite_ID :=
              GL.Objects.Programs.Uniform_Location (Shader_Program, "sprite");
            Texture_ID :=
              GL.Objects.Programs.Uniform_Location (Shader_Program, "mytexture");
            OK := Integer (Offset_X_ID) > -1 and Integer (Scale_X_ID) > -1 and
              Integer (Sprite_ID) > -1 and Integer (Texture_ID) > -1;
         end if;
      end if;
      if not OK then
         Put_Line ("Build_Shader_Program, Attributes initialization failed");
      end if;
      return OK;

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Build_Shader_Program.");
         raise;
   end Build_Shader_Program;

   --  ------------------------------------------------------------------------

   procedure Display (Window : in out Input_Callback.Callback_Window) is
      use GL.Objects.Vertex_Arrays;
      use GL.Types.Singles;
      use Maths;
      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Utilities.Clear_Background_Colour (Background);

      GL.Objects.Programs.Use_Program (Shader_Program);
      GL.Uniforms.Set_Int (Texture_ID, 0);
      GL.Uniforms.Set_Single (Offset_X_ID, Offset_X);
      GL.Uniforms.Set_Single (Scale_X_ID, Scale_X);

      GL.Objects.Buffers.Array_Buffer.Bind (Vertices_Buffer);

      GL.Attributes.Enable_Vertex_Attrib_Array (Coord_Attribute);
      GL.Attributes.Set_Vertex_Attrib_Pointer (Coord_Attribute, 2,
                                               Single_Type, False, 0, 0);
      case Mode is
         when 0 =>
            GL.Uniforms.Set_Single (Sprite_ID, 0.0);
            Draw_Arrays (Line_Strip, 0, 2000);
         when 1 =>
            GL.Uniforms.Set_Single (Sprite_ID, 1.0);
            Draw_Arrays (Points, 0, 2000);
         when 2 =>
            GL.Uniforms.Set_Single (Sprite_ID, Single (Res_Tex_Width));
            Draw_Arrays (Points, 0, 2000);
      end case;

      GL.Attributes.Disable_Vertex_Attrib_Array (Coord_Attribute);

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Display.");
         raise;
   end Display;

   --  ------------------------------------------------------------------------


   function  Init (Window : in out Input_Callback.Callback_Window) return Boolean is
      use GL.Blending;
      use GL.Toggles;
      Position         : constant GL.Types.Singles.Vector4 := (-6.0, -3.0, 3.0, 0.0);
      Local_View       : constant GL.Types.Single := 0.0;
      Local_Ambient    : constant GL.Types.Colors.Color := (0.2, 0.2, 0.2, 1.0);
      Window_Width     : Glfw.Size;
      Window_Height    : Glfw.Size;
      Result           : Boolean;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      Utilities.Clear_Background_Colour (Background);

      Result := Build_Shader_Program;
      if Result then
         Enable (Blend);
         Set_Blend_Func (Src_Alpha, One_Minus_Src_Alpha);
         Put_Line ("Main_Loop.Init Blend_Func set.");
--           Enable (Point_Sprite);
--           Put_Line ("Main_Loop.Init Point_Sprite enabled.");
         Enable (Vertex_Program_Point_Size);
         Put_Line ("Main_Loop.Init Vertex_Program_Point_Size enabled.");

         VAO.Initialize_Id;
         VAO.Bind;

         Buffers.Create_Vertex_Buffer (Vertices_Buffer);
         Textures.Load_Texture (theTexture);
      end if;
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Init.");
         raise;
   end Init;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean;
begin
   Running := Init (Main_Window);

   while Running loop
      Display (Main_Window);
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Running := Running and not
        (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and not Main_Window.Should_Close;
   end loop;

exception
   when others =>
      Put_Line ("An exception occurred in Main_Loop.");
      raise;
end Main_Loop;
