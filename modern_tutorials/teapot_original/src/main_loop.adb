
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
with Pascal_Teapot;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;

   type Mode is (Teapot, CP);
   Teapot_Mode : Mode := Teapot;

   VAO                : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Shader_Program     : GL.Objects.Programs.Program;
   Coord_Attribute    : GL.Attributes.Attribute;
--     Colour_Attribute   : GL.Attributes.Attribute;
   MVP_Location       : GL.Uniforms.Uniform;
   Vertices_Buffer    : GL.Objects.Buffers.Buffer;
   CP_Vertices_Buffer : GL.Objects.Buffers.Buffer;
--     Colours_Buffer     : GL.Objects.Buffers.Buffer;
--     CP_Colours_Buffer  : GL.Objects.Buffers.Buffer;
--     Colours            : Pascal_Teapot.Colours_Array;
--     CP_Colours         : Pascal_Teapot.CP_Colours_Array;   --  For debugging
 --     CP_Elements        : Pascal_Teapot.Patch_Element_Array;  --  For debugging

   CP_Count           : constant Int := Teapot_Data.Control_Points'Length;
   Patch_Count        : constant Int := Teapot_Data.Patchs'Length;
   Num_Steps          : constant Int := 10; -- 10;

   Patch_Array_Length : Int := 2 * (Num_Steps + 1) ** 2;
   Patch_Array        : Singles.Vector3_Array (1 .. Patch_Array_Length) :=
                          (others => (0.0, 0.0, 0.0));

   Teapot_Length      : constant Int
     := Int (Teapot_Data.Patchs'Length * Patch_Array_Length);
   theTeapot          : Singles.Vector3_Array (1 .. Teapot_Length) :=
                          (others => (0.0, 0.0, 0.0));

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
        ((Src ("src/shaders/teapot.vs", Vertex_Shader),
         Src ("src/shaders/teapot.fs", Fragment_Shader)));

      OK := GL.Objects.Programs.Link_Status (Shader_Program);
      if not OK then
         Put_Line ("Build_Shader_Program, Shader_Program Link failed");
         Put_Line (GL.Objects.Programs.Info_Log (Shader_Program));
      else
         GL.Objects.Programs.Use_Program (Shader_Program);
         Coord_Attribute := GL.Objects.Programs.Attrib_Location (Shader_Program, "coord3d");
--           Colour_Attribute := GL.Objects.Programs.Attrib_Location (Shader_Program, "v_color");
--           OK := Coord_Attribute >= 0 and Colour_Attribute >= 0;
         OK := Coord_Attribute >= 0;
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
      use Maths;
      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
      Current_Time  : constant Glfw.Seconds := Glfw.Time;
      Angle         : Radian := 0.26 * Radian (Current_Time);  --  approx 15 degree per second
      Animation     : Singles.Matrix4 := Singles.Identity4;
      View          : Singles.Matrix4 := Singles.Identity4;
      Model         : Singles.Matrix4 := Singles.Identity4;
      Scale_Matrix  : Singles.Matrix4 := Singles.Identity4;
      Projection    : Singles.Matrix4 := Singles.Identity4;
      MVP_Matrix    : Singles.Matrix4 := Singles.Identity4;
      Offset        : Natural := 0;
      Scale         : Single := 0.4;
      Line_Index      : Int := 0;
      Points_Per_Line : constant Int := Num_Steps + 1;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Utilities.Clear_Colour_Buffer_And_Depth;
--        Maths.Init_Lookat_Transform ((0.0, 0.0, -8.0), (0.0, 0.0, 0.0), (0.0, 1.0, 0.0), View);
--        Animation := Translation_Matrix ((-0.5, 0.0, -1.5)); --   *
--          Rotation_Matrix (Angle, (1.0, 0.0, 0.0)) *
--            Rotation_Matrix (2.0 * Angle, (0.0, 1.0, 0.0)) *
--              Rotation_Matrix (3.0 * Angle, (0.0, 0.0, 1.0)) * Animation;
      Projection := Perspective_Matrix (Degree (45.0),
                                        Single (Window_Width) / Single (Window_Height),
                                        -0.1, 100.0);
      Scale_Matrix := Maths.Scaling_Matrix (Scale);
--        MVP_Matrix := Projection * View * Model * Animation * Scale_Matrix;
        MVP_Matrix := Scale_Matrix;

      GL.Objects.Programs.Use_Program (Shader_Program);
      GL.Uniforms.Set_Single (MVP_Location, MVP_Matrix);

      GL.Attributes.Enable_Vertex_Attrib_Array (Coord_Attribute);
      GL.Objects.Buffers.Array_Buffer.Bind (Vertices_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer
        (Coord_Attribute, 3, Single_Type, 0, 0);

        if Teapot_Mode = Teapot then
--        GL.Objects.Buffers.Array_Buffer.Bind (CP_Colours_Buffer);
--  --        GL.Attributes.Set_Vertex_Attrib_Pointer (Colour_Attribute, 3, Single_Type, 0, 0);
         while Line_Index < Patch_Array_Length loop
            GL.Objects.Vertex_Arrays.Draw_Arrays
              (Line_Strip, Line_Index, Points_Per_Line);
            Line_Index := Line_Index + Points_Per_Line;
         end loop;
      else
            GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, 269);
      end if;

      GL.Attributes.Disable_Vertex_Attrib_Array (Coord_Attribute);
--        GL.Attributes.Disable_Vertex_Attrib_Array (Colour_Attribute);

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Display.");
         raise;
   end Display;

   --  ------------------------------------------------------------------------

   function  Init (Window : in out Glfw.Windows.Window) return Boolean is
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

        if Teapot_Mode = Teapot then
             GL.Toggles.Disable (GL.Toggles.Vertex_Program_Point_Size);
             Pascal_Teapot.Build_Teapot (Num_Steps, theTeapot);
--           Pascal_Teapot.Build_CP_Colours (CP_Colours);

            Put_Line ("Init Patch_Array_Length" &
               Int'Image (Patch_Array_Length));
             Buffers.Create_Vertex_Buffer (Vertices_Buffer, theTeapot);
--           Buffers.Create_Colour_Buffer (Colours_Buffer, Colours);

--        CP_Vertices_Buffer.Initialize_Id;
--        GL.Objects.Buffers.Array_Buffer.Bind (CP_Vertices_Buffer);
--        Utilities.Load_Singles_Buffer
--            (GL.Objects.Buffers.Array_Buffer, Teapot_Data.Control_Points2,
--             GL.Objects.Buffers.Static_Draw);
         else
            GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);
            Buffers.Create_Vertex_Buffer
                  (Vertices_Buffer, Teapot_Data.Control_Points);
         end if;
--           Buffers.Create_CP_Colour_Buffer (CP_Colours_Buffer, CP_Colours);
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
