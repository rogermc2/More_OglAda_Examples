
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Blending;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
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
with Cube_Data;
with Textures;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;

   VAO                 : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Elements_Buffer     : GL.Objects.Buffers.Buffer;
   Tex_Coords_Buffer   : GL.Objects.Buffers.Buffer;
   Vertices_Buffer     : GL.Objects.Buffers.Buffer;
   Shader_Program      : GL.Objects.Programs.Program;
   Coord_3D_Attribute  : GL.Attributes.Attribute;
   Tex_Coord_Attribute : GL.Attributes.Attribute;
   MVP_ID              : GL.Uniforms.Uniform;
   Texture_ID          : GL.Uniforms.Uniform;
   Cube_Texture        : GL.Objects.Textures.Texture;
   Window_Width        : Glfw.Size;
   Window_Height       : Glfw.Size;

   Background          : constant GL.Types.Colors.Color := (0.7, 0.7, 0.7, 0.0);

   procedure Logic;

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
         Coord_3D_Attribute := GL.Objects.Programs.Attrib_Location
           (Shader_Program, "coord3d");
         Tex_Coord_Attribute := GL.Objects.Programs.Attrib_Location
           (Shader_Program, "texcoord");
         OK := Coord_3D_Attribute > -1 and Tex_Coord_Attribute > -1;
         if OK then
            MVP_ID :=
              GL.Objects.Programs.Uniform_Location (Shader_Program, "mvp");
            Texture_ID :=
              GL.Objects.Programs.Uniform_Location (Shader_Program, "mytexture");
            OK := MVP_ID > -1 and Texture_ID > -1;
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

   procedure Display (Window : in out Glfw.Windows.Window) is
      use GL.Objects.Textures;
      use GL.Objects.Textures.Targets;
      use GL.Types.Singles;
      use Maths;
      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Utilities.Clear_Colour_Buffer_And_Depth;

      GL.Objects.Programs.Use_Program (Shader_Program);
      Set_Active_Unit (0);
      GL.Uniforms.Set_Int (Texture_ID, 0);
      Texture_2D.Bind (Cube_Texture);

      GL.Attributes.Enable_Vertex_Attrib_Array (Coord_3D_Attribute);
      GL.Objects.Buffers.Array_Buffer.Bind (Vertices_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (Coord_3D_Attribute, 3,
                                               Single_Type, False, 0, 0);

      GL.Attributes.Disable_Vertex_Attrib_Array (Coord_3D_Attribute);

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Display.");
         raise;
   end Display;

   --  ------------------------------------------------------------------------

   function  Init (Window : in out Glfw.Windows.Window) return Boolean is
      use GL.Blending;
      use GL.Toggles;
      Position         : constant GL.Types.Singles.Vector4 := (-6.0, -3.0, 3.0, 0.0);
      Local_View       : constant GL.Types.Single := 0.0;
      Local_Ambient    : constant GL.Types.Colors.Color := (0.2, 0.2, 0.2, 1.0);
      Result           : Boolean;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      Utilities.Clear_Background_Colour (Background);

      Result := Build_Shader_Program;
      if Result then
         Enable (Blend);
         Enable (Depth_Test);
         Set_Blend_Func (Src_Alpha, One_Minus_Src_Alpha);

         VAO.Initialize_Id;
         VAO.Bind;

         Buffers.Create_Vertex_Buffer (Vertices_Buffer, Cube_Data.Vertices);
         Buffers.Create_Tex_Coords_Buffer (Tex_Coords_Buffer, Cube_Data.Texture_Coords);
         Buffers.Create_Elements_Buffer (Elements_Buffer, Cube_Data.Elements);
         Textures.Load_Texture (Cube_Texture, "src/res_texture.png");
         Logic;
      end if;
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Init.");
         raise;
   end Init;

   --  ------------------------------------------------------------------------

   procedure Logic is
      use Maths;

      use Singles;
      Angle      : constant Degree := Degree (Glfw.Time) / 1000.0 * Degree (15.0);
      Anim       : constant Matrix4 :=
                     Rotation_Matrix (3.0 * Angle, (1.0, 0.0, 0.0)) *
                     Rotation_Matrix (2.0 * Angle, (0.0, 1.0, 0.0)) *
                     Rotation_Matrix (4.0 * Angle, (0.0, 0.0, 1.0));
      Model       : constant Matrix4 := Translation_Matrix ((0.0, 0.0, -4.0));
      View        : Matrix4;
      Projection  : constant Matrix4 := Perspective_Matrix
        (45.0, Single (Window_Width) / Single (Window_Height), 0.1, 10.0);
      MVP         : Matrix4;
   begin
      Init_Lookat_Transform ((0.0, 2.0, 0.0), (0.0, 0.0, -4.0),
                             (0.0, 1.0, 0.0), View);
      MVP := Projection * View * Model * Anim;
      GL.Objects.Programs.Use_Program (Shader_Program);
      GL.Uniforms.Set_Single (MVP_ID, MVP);
   end Logic;

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
