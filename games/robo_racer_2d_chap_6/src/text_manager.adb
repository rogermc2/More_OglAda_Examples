
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;

with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Toggles;
with GL.Uniforms;
with GL.Window;

with Maths;
with Program_Loader;
with Text_Management;

package body Text_Manager is

   Render_Text_Program   : GL.Objects.Programs.Program;
   Dimensions_ID         : GL.Uniforms.Uniform;
   Texture_ID            : GL.Uniforms.Uniform;
   MVP_Matrix_ID         : GL.Uniforms.Uniform;
   Colour_ID             : GL.Uniforms.Uniform;
   MVP_Matrix            : GL.Types.Singles.Matrix4;

   Font_File_1     : constant String := "../fonts/NotoSerif-Regular.ttf";

   --  ------------------------------------------------------------------------

   procedure Draw_Text (Window  : in out Input_Callback.Callback_Window;
                        Text : String; X, Y, R, G, B : GL.Types.Single) is
      use GL.Types;
      Scale         : constant GL.Types.Single := 0.2;
      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
   begin
      Window.Get_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Maths.Init_Orthographic_Transform (Single (Window_Height), 0.0, 0.0,
                                         Single (Window_Width), 0.1, -100.0,
                                         MVP_Matrix);

      Text_Management.Render_Text (Render_Text_Program, Text, X, Y,
                                   Scale, (R, G, B, 1.0), Texture_ID,
                                   MVP_Matrix_ID, Dimensions_ID, Colour_ID,
                                   MVP_Matrix);
   exception
      when anError : others =>
         Put_Line ("An exception occurred in Text_Manager.Draw_Text.");
         Put_Line (Exception_Information (anError));
         raise;
   end Draw_Text;

   --  -------------------------------------------------------------------------

--     procedure Render (Window  : in out Glfw.Windows.Window) is
--        use Ada.Strings.Unbounded;
--        use GL.Types;
--        Window_Width    : Glfw.Size;
--        Window_Height   : Glfw.Size;
--        Pos_X           : constant GL.Types.Single := 10.0;
--        Pos_Y           : constant GL.Types.Single := 150.0;
--        Scale_1         : constant GL.Types.Single := 0.4;
--        Scale_2         : constant GL.Types.Single := 0.2;
--        Scale_3         : constant GL.Types.Single := 0.3;
--        Text_List       : Text_Management.Text_Array (1 .. 2);
--     begin
--        Window.Get_Size (Window_Width, Window_Height);
--        GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
--                                GL.Types.Int (Window_Height));
--        Maths.Init_Orthographic_Transform (Single (Window_Height), 0.0, 0.0,
--                                           Single (Window_Width), 0.1, -100.0,
--                                           MVP_Matrix);
--        Text_Management.Render_Text (Render_Text_Program, "Hello", 300.0, Pos_Y + 250.0,
--                                     Scale_1, Text_Colour, Texture_ID, MVP_Matrix_ID,
--                                     Dimensions_ID, Colour_ID, MVP_Matrix);
--        Text_List (1) :=
--          (To_Unbounded_String ("1234567890 !@#$%^&*()_+=,./?;':""{}[]\|~`"),
--           Pos_X + 20.0, Pos_Y + 150.0, Scale_1, Text_Colour);
--        Text_List (2) :=
--          (To_Unbounded_String ("The Quick Brown Fox jumps over the zoo's Lazy Dog."),
--           Pos_X, Pos_Y, Scale_3, Text_Colour);
--        Text_Management.Render_Text (Render_Text_Program, Text_List, Texture_ID,
--                                     MVP_Matrix_ID, Dimensions_ID, Colour_ID, MVP_Matrix);
--        Text_Management.Render_Text (Render_Text_Program, "Hello again!", 300.0, 50.0,
--                                     Scale_2, Text_Colour, Texture_ID, MVP_Matrix_ID,
--                                     Dimensions_ID, Colour_ID, MVP_Matrix);
--     end Render;

   --  ------------------------------------------------------------------------

   procedure Initialize  (Window  : in out Input_Callback.Callback_Window) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use Program_Loader;

      Window_Width    : Glfw.Size;
      Window_Height   : Glfw.Size;
   begin
      Window.Get_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      GL.Toggles.Enable (GL.Toggles.Cull_Face);

      Render_Text_Program := Program_From
        ((Src ("src/shaders/text_vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/text_fragment_shader.glsl", Fragment_Shader)));
      Use_Program (Render_Text_Program);

      MVP_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Text_Program, "mvp_matrix");
      Texture_ID := GL.Objects.Programs.Uniform_Location
        (Render_Text_Program, "text_sampler");
      Colour_ID := GL.Objects.Programs.Uniform_Location
        (Render_Text_Program, "text_colour");
      Dimensions_ID := GL.Objects.Programs.Uniform_Location
        (Render_Text_Program, "dimensions");

      Text_Management.Setup (Font_File_1);

exception
      when anError : others =>
         Put_Line ("An exception occurred in Text_Manager.Draw_Text.");
         Put_Line (Exception_Information (anError));
         raise;

   end Initialize;

   --  ------------------------------------------------------------------------

end Text_Manager;
