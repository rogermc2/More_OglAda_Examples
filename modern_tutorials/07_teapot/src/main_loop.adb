
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Culling;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
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
with Teapot_Data;
with MT_Teapot;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;

   Vertex_Array_Size  : GL.Types.Int := Teapot_Data.Num_Patchs * MT_Teapot.Res_U * MT_Teapot.Res_V;

   VAO                : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Shader_Program     : GL.Objects.Programs.Program;
   Coord_Attribute    : GL.Attributes.Attribute;
   Colour_Attribute   : GL.Attributes.Attribute;
   MVP_Location       : GL.Uniforms.Uniform;
   Vertices_Buffer    : GL.Objects.Buffers.Buffer;
   Colours_Buffer     : GL.Objects.Buffers.Buffer;
   Elements_Buffer    : GL.Objects.Buffers.Buffer;
   CP_Vertices_Buffer : GL.Objects.Buffers.Buffer;
   CP_Colours_Buffer  : GL.Objects.Buffers.Buffer;
   CP_Elements_Buffer : GL.Objects.Buffers.Buffer;

   Colours            : MT_Teapot.Colours_Array;
   Vertices           : MT_Teapot.Vertices_Array;
   Elements           : MT_Teapot.Element_Array;
   CP_Colours         : MT_Teapot.Teapot_CP_Colours;  --  For debugging
   CP_Elements        : Buffers.CP_Element_Array;     --  For debugging

   Background         : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 0.0);

   procedure Update_Lighting_Intensity (Window : in out Glfw.Windows.Window);

   --  ------------------------------------------------------------------------

   function Build_Shader_Program return Boolean is
      use GL.Attributes;
      use GL.Objects.Shaders;
      use GL.Uniforms;
      use Program_Loader;
      OK : Boolean := False;
   begin
      Shader_Program := Program_From
        ((Src ("src/shaders/teapot.vs", Vertex_Shader),
         Src ("src/shaders/teapot.fs", Fragment_Shader)));

      OK := GL.Objects.Programs.Link_Status (Shader_Program);
      if not OK then
         Put_Line ("Build_Shader_Program, Shader_Program Link failed");
         Put_Line (GL.Objects.Programs.Info_Log (Shader_Program));
      else
         GL.Objects.Programs.Use_Program (Shader_Program);
         Coord_Attribute := GL.Objects.Programs.Attrib_Location (Shader_Program, "coord3d");
         Colour_Attribute := GL.Objects.Programs.Attrib_Location (Shader_Program, "v_color");
         OK := Coord_Attribute >= 0 and Colour_Attribute >= 0;
         if OK then
            MVP_Location :=
              GL.Objects.Programs.Uniform_Location (Shader_Program, "mvp");
            OK := MVP_Location >= 0;
            if not OK then
               Put_Line ("Build_Shader_Program, MVP_Location initialization failed");
            end if;
         else
            Put_Line ("Build_Shader_Program, Attributes initialization failed");
         end if;
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
      use MAths;
      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
      Current_Time  : constant Glfw.Seconds := Glfw.Time;
      Angle         : Radian := 0.26 * Radian (Current_Time);  --  approx 15 degree per second
      Animation     : Singles.Matrix4 := Singles.Identity4;
      View          : Singles.Matrix4 := Singles.Identity4;
      Model         : Singles.Matrix4 := Singles.Identity4;
      Projection    : Singles.Matrix4 := Singles.Identity4;
      MVP_Matrix    : Singles.Matrix4 := Singles.Identity4;
      Offset        : Natural := 0;
   begin
      --        Update_Lighting_Intensity (Window);
      --           Put_Line ("Main_Loop.Init Lighting_Intensity updated.");
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Maths.Init_Lookat_Transform ((0.0, 0.0, 8.0), (1.0, 0.0, 0.0), (0.0, 1.0, 0.0), View);
      Animation := Translation_Matrix ((0.0, 0.0, -1.5)) *
        Rotation_Matrix (Angle, (1.0, 0.0, 0.0)) *
          Rotation_Matrix (2.0 * Angle, (0.0, 1.0, 0.0)) *
            Rotation_Matrix (3.0 * Angle, (0.0, 0.0, 1.0)) * Animation;
      Projection := Perspective_Matrix (Degree (45.0),
                                        Single (Window_Width) / Single (Window_Height),
                                        0.1, 10.0);
      MVP_Matrix := Projection * View * Model * Animation;

      GL.Objects.Programs.Use_Program (Shader_Program);
      GL.Uniforms.Set_Single (MVP_Location, MVP_Matrix);

      Utilities.Clear_Background_Colour_And_Depth (Background);
      GL.Attributes.Enable_Vertex_Attrib_Array (Coord_Attribute);
      GL.Objects.Buffers.Array_Buffer.Bind (Vertices_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (Coord_Attribute, 3, Single_Type, 0, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (Colour_Attribute);
      GL.Objects.Buffers.Array_Buffer.Bind (Colours_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (Colour_Attribute, 3, Single_Type, 0, 0);

      GL.Objects.Buffers.Element_Array_Buffer.Bind (Elements_Buffer);
      GL.Objects.Buffers.Draw_Elements (Triangles, 3, UShort_Type);

      --  Draw Control points
      GL.Objects.Buffers.Array_Buffer.Bind (CP_Vertices_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (Coord_Attribute, 3, Single_Type, 0, 0);
      GL.Objects.Buffers.Array_Buffer.Bind (CP_Colours_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (Colour_Attribute, 3, Single_Type, 0, 0);
      GL.Objects.Buffers.Element_Array_Buffer.Bind (CP_Elements_Buffer);
      for Patch_Num in Teapot_Data.Patchs'First .. Teapot_Data.Patchs'Last loop
         for index in 1 .. Teapot_Data.Order loop
            Offset := Offset + Natural (Teapot_Data.Order);
--              Put_Line ("Main_Loop.Display Drawing Patch " &
--                          GL.Types.Int'Image (Patch_Num) & "  " &
--                       GL.Types.Int'Image (index));
            GL.Objects.Buffers.Draw_Elements (Line_Loop, Teapot_Data.Order, UShort_Type, Offset);
         end loop;
      end loop;

      GL.Attributes.Disable_Vertex_Attrib_Array (Coord_Attribute);
      GL.Attributes.Disable_Vertex_Attrib_Array (Colour_Attribute);

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Display.");
         raise;
   end Display;

   --  ------------------------------------------------------------------------

   function  Init (Window : in out Glfw.Windows.Window) return Boolean is
      Ambient          : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 1.0);
      Diffuse          : constant GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);
      Specular         : constant GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);
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
         VAO.Initialize_Id;
         VAO.Bind;
         MT_Teapot.Build_Teapot (Vertices, Colours, Elements);
         Buffers.Create_Vertex_Buffer (Vertices_Buffer, Vertices);
         Buffers.Create_Colour_Buffer (Colours_Buffer, Colours);
         Buffers.Create_Elements_Buffer (Elements_Buffer, Elements);

         Buffers.Create_Vertex_Buffer (CP_Vertices_Buffer, Teapot_Data.CP_Vertices);
         Buffers.Create_Colour_Buffer (CP_Colours_Buffer, CP_Colours);
         Buffers.Create_Elements_Buffer (CP_Elements_Buffer, CP_Elements);  --  For debugging
      end if;
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Init.");
         raise;
   end Init;

   --  ------------------------------------------------------------------------

   procedure Update_Lighting_Intensity (Window : in out Glfw.Windows.Window) is
      use Glfw.Input;
   begin
      null;
      --        if Window'Access.Key_State (Keys.A) = Pressed then
      --           Light_Direction.Ambient_Intensity := Light_Direction.Ambient_Intensity + 0.05;
      --        elsif Window'Access.Key_State (Keys.S) = Pressed then
      --           Light_Direction.Ambient_Intensity := Light_Direction.Ambient_Intensity - 0.05;
      --        elsif Window'Access.Key_State (Keys.Z) = Pressed then
      --           Light_Direction.Diffuse_Intensity := Light_Direction.Diffuse_Intensity + 0.05;
      --        elsif Window'Access.Key_State (Keys.X) = Pressed then
      --           Light_Direction.Diffuse_Intensity := Light_Direction.Diffuse_Intensity  - 0.05;
      --        end if;
   end Update_Lighting_Intensity;

   --  -------------------------------------------------------------------------

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
