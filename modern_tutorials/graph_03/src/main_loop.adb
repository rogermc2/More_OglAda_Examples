
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
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
with Keyboard_Handler;
with Scissor_API;

procedure Main_Loop (Main_Window : in out Input_Callback.Callback_Window) is
   use GL.Types;
   use GL.Uniforms;

   Border          : constant Single := 10.0;
   Tick_Size       : constant Single := 10.0;
   VAO             : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Shader_Program  : GL.Objects.Programs.Program;
   Colour_ID       : Uniform := -1;
   Transform_ID    : Uniform := -1;
   Data_VBO        : GL.Objects.Buffers.Buffer;
   Border_VBO      : GL.Objects.Buffers.Buffer;
   Status          : Keyboard_Handler.Status_Data;
   Offset_X        : Single := 0.0;
   Scale_X         : Single := 1.0;

   Background      : constant GL.Types.Colors.Color := (0.1, 0.1, 0.1, 0.0);
   Black           : constant Singles.Vector4 := (0.0, 0.0, 0.0, 1.0);
   Red             : constant Singles.Vector4 := (1.0, 0.0, 0.0, 1.0);


   procedure  Draw_X_Tick_Marks (Pixel_Y : Single);
   procedure  Draw_Y_Tick_Marks (Pixel_X : Single);
   function Viewport_Transform (Window : in out Input_Callback.Callback_Window;
      X, Y, Width, Height : Single;  Pixel_X, Pixel_Y : in out Single)
      return Singles.Matrix4;

   --  ------------------------------------------------------------------------

   function Build_Shader_Program return Boolean is
      use GL.Attributes;
      use GL.Objects.Shaders;
      use GL.Uniforms;
      use Program_Loader;
      OK : Boolean := False;
   begin
      Shader_Program := Program_From
        ((Src ("src/shaders/graph_3.vert", Vertex_Shader),
         Src ("src/shaders/graph_3.frag", Fragment_Shader)));

      OK := GL.Objects.Programs.Link_Status (Shader_Program);
      if not OK then
         Put_Line ("Build_Shader_Program, Shader_Program Link failed");
         Put_Line (GL.Objects.Programs.Info_Log (Shader_Program));
      else
         GL.Objects.Programs.Use_Program (Shader_Program);
         Transform_ID :=
           GL.Objects.Programs.Uniform_Location (Shader_Program, "transform");
         Colour_ID :=
           GL.Objects.Programs.Uniform_Location (Shader_Program, "colour");
         OK := Integer (Transform_ID) > -1 and Integer (Colour_ID) > -1;
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
      use GL.Objects.Buffers;
      use GL.Objects.Vertex_Arrays;
      use GL.Toggles;
      use GL.Types.Singles;
      use Maths;
      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
      Width         : Single;
      Height        : Single;
      Pixel_X       : Single := 0.0;
      Pixel_Y       : Single := 0.0;
      View          : Matrix4 := Identity4;
      Transform     : Matrix4 := Identity4;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));

      Width := Single (Window_Width);
      Height := Single (Window_Height);

      Utilities.Clear_Background_Colour (Background);
      Keyboard_Handler.Key_Down (Window, Status);

      View := Viewport_Transform (Window, Border + Tick_Size,
                                  Border + Tick_Size,
                                  Width - 2.0 * Border, Height - 2.0 * Border,
                                  Pixel_X, Pixel_Y);
      Scissor_API.Set_Scissor_Rectangle
        (Int (Border + Tick_Size), Int (Border + Tick_Size),
         GL.Types.Size (Width - 2.0 * Border - Tick_Size),
         GL.Types.Size (Height - 2.0 * Border - Tick_Size));
      Enable (Scissor_Test);

      Transform := Translation_Matrix ((Offset_X, 0.0, 0.0)) *
        Scaling_Matrix ((Scale_X, 1.0, 1.0));

      GL.Objects.Programs.Use_Program (Shader_Program);
      GL.Uniforms.Set_Single (Transform_ID, Transform);
      GL.Uniforms.Set_Single (Colour_ID, Red);

      GL.Objects.Buffers.Array_Buffer.Bind (Data_VBO);

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 2, Single_Type, False, 0, 0);
      Draw_Arrays (Line_Strip, 0, 2000);

      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));

      GL.Attributes.Disable_Vertex_Attrib_Array (0);
      Disable (Scissor_Test);

      --  Draw the borders
      Pixel_X := 0.0;
      Pixel_Y := 0.0;
      Transform := Viewport_Transform
        (Window, Border + Tick_Size, Border + Tick_Size,
         Width - 2.0 * Border - Tick_Size, Height - 2.0 * Border - Tick_Size,
         Pixel_X, Pixel_Y);
      GL.Uniforms.Set_Single (Transform_ID, Transform);
      GL.Uniforms.Set_Single (Colour_ID, Black);

      GL.Objects.Buffers.Array_Buffer.Bind (Border_VBO);

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 2, Single_Type, False, 0, 0);
      Draw_Arrays (Line_Loop, 0, 4);

      GL.Attributes.Disable_Vertex_Attrib_Array (0);
      Draw_Y_Tick_Marks (Pixel_X);
      Draw_X_Tick_Marks (Pixel_Y);

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Display.");
         raise;
   end Display;

   --  ------------------------------------------------------------------------

   procedure  Draw_X_Tick_Marks (Pixel_Y : Single) is
      use GL.Types.Singles;
      use GL.Objects.Buffers;
      use GL.Objects.Vertex_Arrays;
      use Maths.Single_Math_Functions;
      Tick_Spacing  : constant Single :=
                        0.1 * (10.0 ** (-Single'Floor (Log (Scale_X, 10.0))));
      Left          : constant Single := -1.0 / Scale_X - Offset_X;
      Right         : constant Single := 1.0 / Scale_X - Offset_X;
      Left_Index    : constant Int := Int (Single'Ceiling (Left / Tick_Spacing));
      Right_Index   : constant Int := Int (Single'Floor (Right / Tick_Spacing));
      Left_Margin   : constant Single := Single (Left_Index) * Tick_Spacing - Left;
      First_Tick    : constant Single := Left_Margin * Scale_X - 1.0;
      Num_Ticks     : Int := Right_Index - Left_Index + 1;
      Tick_Scale    : Single;
      X             : Single;
      Ticks         : Vector2_Array (1 .. 42) := (others => (0.0, 0.0));
   begin
      if Num_Ticks > 21 then
         Num_Ticks := 21;   --  should not happen
      end if;

      for index in Int range 0 .. Num_Ticks - 1 loop
         X := First_Tick + Single (index) * Tick_Spacing * Scale_X;
         if (index + Left_Index) mod 10 = 0 then
            Tick_Scale := 1.0;
         else
            Tick_Scale := 0.5;
         end if;
         Ticks (2 * index + 1) := (X, -1.0);
         Ticks (2 * index + 2) := (X, -1.0 - Tick_Size * Tick_Scale * Pixel_Y);
      end loop;

      Array_Buffer.Bind (Border_VBO);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Ticks, Dynamic_Draw);

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 2, Single_Type, False, 0, 0);
      Draw_Arrays (Lines, 0, 2 * Num_Ticks);

      GL.Attributes.Disable_Vertex_Attrib_Array (0);
   end Draw_X_Tick_Marks;

   --  ------------------------------------------------------------------------

   procedure  Draw_Y_Tick_Marks (Pixel_X : Single) is
      use GL.Types.Singles;
      use GL.Objects.Buffers;
      use GL.Objects.Vertex_Arrays;
      Ticks         : Vector2_Array (1 .. 42) := (others => (0.0, 0.0));
      Y             : Single;
      Tick_Scale    : Single;
      Tick_Size_Sq  : constant Single := Tick_Size ** 2;
      begin
      for index in Int range 0 .. 20 loop
         Y := 0.1 * Single (index - 1) - 1.0;
         if (index - 1) mod 10 = 0 then
            Tick_Scale := 1.0;
         else
            Tick_Scale := 0.5;
         end if;
         Ticks (2 * index + 1) := (-1.0, Y);
         Ticks (2 * index + 2) := (-1.0 - Tick_Size_Sq * Pixel_X , Y);
      end loop;

      Array_Buffer.Bind (Border_VBO);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Ticks, Dynamic_Draw);

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 2, Single_Type, False, 0, 0);
      Draw_Arrays (Lines, 0, 42);

      GL.Attributes.Disable_Vertex_Attrib_Array (0);
   end Draw_Y_Tick_Marks;

   --  ------------------------------------------------------------------------

   function  Init (Window : in out Input_Callback.Callback_Window) return Boolean is
      use GL.Toggles;
      Result  : Boolean;
   begin
      Utilities.Clear_Background_Colour (Background);

      Result := Build_Shader_Program;
      if Result then
         Enable (Vertex_Program_Point_Size);

         VAO.Initialize_Id;
         VAO.Bind;

         Buffers.Create_Vertex_Buffer (Data_VBO, Border_VBO);
      end if;
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Init.");
         raise;
   end Init;

   --  ------------------------------------------------------------------------

   function Viewport_Transform (Window : in out Input_Callback.Callback_Window;
                                X, Y, Width, Height : Single;
                                Pixel_X, Pixel_Y    : in out Single)
                                return Singles.Matrix4 is
      use Singles;
      use Maths;
      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
      FB_Width      : Single;
      FB_Height     : Single;
      Offset_X      : Single;
      Offset_Y      : Single;
      Scale_X       : Single;
      Scale_Y       : Single;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      FB_Width := Single (Window_Width);
      FB_Height := Single (Window_Height);
      Offset_X := (2.0 * X  + Width - FB_Width) / FB_Width;
      Offset_Y := (2.0 * Y + Height - FB_Height) / FB_Height;
      Scale_X := Width / FB_Width;
      Scale_Y := Height / FB_Height;

      if Pixel_X /= 0.0 then
         Pixel_X := 2.0 / FB_Width;
      end if;
      if Pixel_Y /= 0.0 then
         Pixel_Y := 2.0 / FB_Height;
      end if;

      return Scaling_Matrix ((Scale_X, Scale_Y, 1.0)) *
        Translation_Matrix ((Offset_X, Offset_Y, 0.0));

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Viewport_Transform.");
         raise;
         return Identity4;
   end Viewport_Transform;

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
