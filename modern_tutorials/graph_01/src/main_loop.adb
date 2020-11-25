
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
with Graph_Data;
with Textures;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;

   Vertex_Array_Size  : GL.Types.Int;

   VAO                : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Shader_Program     : GL.Objects.Programs.Program;
   Coord_Attribute    : GL.Attributes.Attribute;
   Offset_X_ID        : GL.Uniforms.Uniform;
   Scale_X_ID         : GL.Uniforms.Uniform;
   Sprite_ID          : GL.Uniforms.Uniform;
   Texture_ID         : GL.Uniforms.Uniform;
   theTexture         : GL.Objects.Textures.Texture
   Vertices_Buffer    : GL.Objects.Buffers.Buffer;

   Vertices           : Graph_Data.Point_Data;

   Background         : constant GL.Types.Colors.Color := (0.7, 0.7, 0.7, 0.0);

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
         OK := Coord_Attribute > -1;
         if OK then
            Offset_X_ID :=
              GL.Objects.Programs.Uniform_Location (Shader_Program, "offset_x");
            Scale_X_ID :=
              GL.Objects.Programs.Uniform_Location (Shader_Program, "scale_x");
            Sprite_ID :=
              GL.Objects.Programs.Uniform_Location (Shader_Program, "sprite");
            Texture_ID :=
              GL.Objects.Programs.Uniform_Location (Shader_Program, "mytexture");
            OK := Offset_X_ID > -1 and Scale_X_ID > -1 and Sprite_ID > -1 and Texture_ID > -1;
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
      use GL.Types.Singles;
      use Maths;
      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
      Current_Time  : constant Glfw.Seconds := Glfw.Time;
      Angle         : Radian := 0.1 * Radian (Current_Time);  --  approx 15 degree per second
      Animation     : Singles.Matrix4 := Singles.Identity4;
      View          : Singles.Matrix4 := Singles.Identity4;
      Model         : Singles.Matrix4 := Singles.Identity4;
      Scale_Matrix  : Singles.Matrix4 := Singles.Identity4;
      Projection    : Singles.Matrix4 := Singles.Identity4;
      MVP_Matrix    : Singles.Matrix4 := Singles.Identity4;
      Offset        : Natural := 0;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Utilities.Clear_Colour_Buffer_And_Depth;
      Maths.Init_Lookat_Transform ((0.0, 0.0, 8.0), (0.0, 0.0, 0.0), (0.0, 1.0, 0.0), View);
      Animation := Translation_Matrix ((-0.5, 0.0, -1.5))  *
        Rotation_Matrix (Angle, (1.0, 0.0, 0.0)) *
          Rotation_Matrix (2.0 * Angle, (0.0, 1.0, 0.0)) *
            Rotation_Matrix (3.0 * Angle, (0.0, 0.0, 1.0)) * Animation;
      Projection := Perspective_Matrix (Degree (45.0),
                                        Single (Window_Width) / Single (Window_Height),
                                        0.1, 10.0);
      MVP_Matrix := Projection * View * Model * Animation;

      GL.Objects.Programs.Use_Program (Shader_Program);
      --          GL.Uniforms.Set_Single (MVP_Location, MVP_Matrix);

      GL.Attributes.Enable_Vertex_Attrib_Array (Coord_Attribute);
      GL.Objects.Buffers.Array_Buffer.Bind (Vertices_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (Coord_Attribute, 3,
                                               Single_Type, 0, 0);

      GL.Attributes.Disable_Vertex_Attrib_Array (Coord_Attribute);

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
         Enable (Point_Sprite);
         Enable (Vertex_Program_Point_Size);

         VAO.Initialize_Id;
         VAO.Bind;

         Buffers.Create_Vertex_Buffer (Vertices_Buffer, Vertices);
         Textures.Load_Texture (theTexture,
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