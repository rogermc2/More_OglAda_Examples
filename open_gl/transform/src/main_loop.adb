
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Low_Level.Enums;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;
with Vertex_Data;

with Ogldev_Texture;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Background          : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 1.0);
   Shader_Program      : GL.Objects.Programs.Program;
   Vertex_Array        : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer       : GL.Objects.Buffers.Buffer;
   Element_Buffer      : GL.Objects.Buffers.Buffer;
   Textures            : array (1 .. 2) of Ogldev_Texture.Ogl_Texture;
   Position_Location   : GL.Attributes.Attribute;
   Colour_Location     : GL.Attributes.Attribute;
   Tex_Coord_Location  : GL.Attributes.Attribute;
   Projection_Location : GL.Uniforms.Uniform;
   Transform_Location  : GL.Uniforms.Uniform;
   View_Location       : GL.Uniforms.Uniform;
   Texture0_Location   : GL.Uniforms.Uniform;
   Texture1_Location   : GL.Uniforms.Uniform;
   Projection_Matrix   : GL.Types.Singles.Matrix4;
   View_Matrix         : GL.Types.Singles.Matrix4;

   --  ------------------------------------------------------------------------

   procedure Render is
      use GL.Types;
      use Maths;
      Current_Time    : constant Glfw.Seconds := Glfw.Time;
      Rotation_Axis   : constant GL.Types.Singles.Vector3 :=  (0.0, 0.0, 1.0);
      Rotation_Matrix : GL.Types.Singles.Matrix4;
   begin
      Utilities.Clear_Background_Colour (Background);
      Rotation_Matrix := Maths.Rotation_Matrix (Degree (0.3 * Single (Current_Time)), Rotation_Axis);
      Shader_Program.Use_Program;
      GL.Uniforms.Set_Single (Transform_Location, Rotation_Matrix);

      GL.Objects.Buffers.Draw_Elements (GL.Types.Triangles, 6, UInt_Type);
   exception
      when others =>
         Put_Line ("An exception occurred in Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Setup is
      use GL.Objects.Buffers;
      use GL.Objects.Shaders;
      use GL.Objects.Textures.Targets;
      use Program_Loader;
   begin
      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Utilities.Load_Vector8_Buffer
        (Array_Buffer, Vertex_Data.Vertices, Static_Draw);

      Element_Buffer.Initialize_Id;
      Element_Array_Buffer.Bind (Element_Buffer);
      Utilities.Load_Element_Buffer
        (Element_Array_Buffer, Vertex_Data.Elements, Static_Draw);

      Shader_Program := Program_From
        ((Src ("src/shaders/transform_vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/transform_fragment_shader.glsl", Fragment_Shader)));

      GL.Objects.Programs.Use_Program (Shader_Program);
      Position_Location := GL.Objects.Programs.Attrib_Location
        (Shader_Program, "position");
      Colour_Location := GL.Objects.Programs.Attrib_Location
        (Shader_Program, "colour");
      Tex_Coord_Location := GL.Objects.Programs.Attrib_Location
        (Shader_Program, "texcoord");

      Transform_Location := GL.Objects.Programs.Uniform_Location
        (Shader_Program, "trans");
      View_Location := GL.Objects.Programs.Uniform_Location
        (Shader_Program, "view");
      Projection_Location := GL.Objects.Programs.Uniform_Location
        (Shader_Program, "proj");
      Texture0_Location := GL.Objects.Programs.Uniform_Location
        (Shader_Program, "tex0");
      Texture1_Location := GL.Objects.Programs.Uniform_Location
        (Shader_Program, "tex1");

      Maths.Init_Lookat_Transform (Position => (1.2, 1.2, 1.2),
                                   Target   => (0.0, 0.0, 0.0),
                                   Up       => (0.0, 0.0, 1.0),
                                   Look_At  => View_Matrix);
      Maths.Init_Perspective_Transform (View_Angle => Maths.Degrees (45.0),
                                        Width      => 800.0,
                                        Height     => 600.0,
                                        Z_Near     => 1.0,
                                        Z_Far      => 10.0,
                                        Transform  => Projection_Matrix);

      GL.Uniforms.Set_Single (View_Location, View_Matrix);
      GL.Uniforms.Set_Single (Projection_Location, Projection_Matrix);

      GL.Objects.Textures.Set_Active_Unit (0);
--        for index in 1 .. 2 loop
--           Textures (index).Initialize_Id;
--           Texture_2D.Bind (Textures (index));
--        end loop;

         if  Ogldev_Texture.Init_Texture (Textures (1), GL.Low_Level.Enums.Texture_2D,
                                          "../content/sample.png") then
            Ogldev_Texture.Load (Textures (1));
         else
            Put_Line ("Main_Loop.Init sample.png failed to load");
         end if;
      GL.Uniforms.Set_Int (Texture0_Location, 0);
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_S
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_T

      GL.Objects.Textures.Set_Active_Unit (1);

         if  Ogldev_Texture.Init_Texture (Textures (2), GL.Low_Level.Enums.Texture_2D,
                                          "../content/sample2.png") then
            Ogldev_Texture.Load (Textures (2));
         else
            Put_Line ("Main_Loop.Init sample2.png failed to load");
         end if;
      GL.Uniforms.Set_Int (Texture1_Location, 1);
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge);
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge);

      GL.Attributes.Enable_Vertex_Attrib_Array (Position_Location);
      GL.Attributes.Set_Vertex_Attrib_Pointer
        (Position_Location, 2, GL.Types.Single_Type, False, 7, 0);

      GL.Attributes.Enable_Vertex_Attrib_Array (Colour_Location);
      GL.Attributes.Set_Vertex_Attrib_Pointer
        (Colour_Location, 3, GL.Types.Single_Type, False, 7, 2);

      GL.Attributes.Enable_Vertex_Attrib_Array (Tex_Coord_Location);
      GL.Attributes.Set_Vertex_Attrib_Pointer
        (Tex_Coord_Location, 2, GL.Types.Single_Type, False, 7, 5);

   exception
      when others =>
         Put_Line ("An exception occurred in Setup.");
         raise;
   end Setup;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
begin
   Setup;
   while Running loop
      Render;
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
