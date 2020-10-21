
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;
with GL.Types;

with GL_Maths;

package MMenu_Initialization is

   MMenu_Exception : Exception;

   type Menu_String_Array is array (Integer range <>) of Unbounded_String;

   procedure Init (Cursor_VAO, Title_VAO , Menu_VAO : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object;
                   Enabled_Strings, Tex_Filter_Strings : in out Menu_String_Array;
                   Menu_Text, Graphics_Text, Graphic_Value_Text, Cal_KB_Text,
                   Cal_GP_Text, GP_Axis_Binding_Text, GP_Buttons_Binding_Text,
                   Audio_Text, Audio_Value_Text, Input_Text, Input_Value_Text,
                   Confirm_Quit_Text, KB_Binding_Text  : in out GL_Maths.Integer_Array;
                   Graphic_Value_Strings  : in out Menu_String_Array;
                   Title_Author_Text, Title_Buildstamp_Text : in out Integer;
                   Title_Shader_Program, Cursor_Shader_Program,
                   Credits_Shader_Program  : in out GL.Objects.Programs.Program;
                   Cursor_Point_Count, Title_Point_Count : in out Integer;
                   Position_Buffer, Texture_Buffer : in out GL.Objects.Buffers.Buffer;
                   Text_Background_Texture, Menu_Credits_Texture,
                   Title_Skull_Texture, Menu_Cursor_Texture : in out GL.Objects.Textures.Texture;
                   Title_M, Title_V : in out GL.Types.Singles.Matrix4);


end MMenu_Initialization;
