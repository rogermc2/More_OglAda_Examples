
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

   procedure Init1 (Menu_Text : in out GL_Maths.Integer_Array;
                   End_Story_Text :in out Integer;
                   Text_Background_Texture, Menu_Credits_Texture,
                   Title_Skull_Texture : in out GL.Objects.Textures.Texture);

   procedure Init_Audio_Value_Strings
     (Audio_Text, Audio_Value_Text : in out GL_Maths.Integer_Array);
   procedure Init_Credits
     (Credits_Shader_Program : in out GL.Objects.Programs.Program;
      Text_Background_Pos : in out GL.Types.Singles.Vector2);
   procedure Init_Cursor (Title_Mesh            : Integer;
                          Menu_Cursor_Texture   : in out GL.Objects.Textures.Texture;
                          Cursor_Shader_Program : in out GL.Objects.Programs.Program;
                          Cursor_VAO            : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object;
                          Cursor_Point_Count    : in out Integer);
   procedure Init_Graphic_Value_Strings (Enabled_Strings, Graphic_Value_Strings :
                                         in out Menu_String_Array);
   procedure Init_Graphic_Text
     (Graphics_Text, Graphic_Value_Text : in out GL_Maths.Integer_Array;
      Graphic_Value_Strings             : in out Menu_String_Array);
   procedure Init_Input_Text (Input_Text : in out GL_Maths.Integer_Array);
   procedure Init_Input_Actions
     (Cal_KB_Text, Cal_GP_Text, KB_Binding_Text, GP_Axis_Binding_Text,
      GP_Buttons_Binding_Text : in out GL_Maths.Integer_Array);
   procedure Init_Position_And_Texture_Buffers
     (Menu_VAO                        : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Position_Buffer, Texture_Buffer : in out GL.Objects.Buffers.Buffer);
   procedure Init_Quit_Text
     (Input_Value_Text, Confirm_Quit_Text : in out GL_Maths.Integer_Array;
      Enabled_Strings                     : in out Menu_String_Array);
   procedure Init_Title
     (Title_Mesh_ID                            : in out Integer;
      Title_Author_Text, Title_Buildstamp_Text : in out Integer;
      Title_M, Title_V                         : in out GL.Types.Singles.Matrix4;
      Title_Shader_Program                     : in out GL.Objects.Programs.Program;
      Title_VAO                                : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Title_Point_Count                        : in out Integer);
   procedure Init_Various (Input_Text : in out GL_Maths.Integer_Array;
                           Joy_Name   : String);


end MMenu_Initialization;
