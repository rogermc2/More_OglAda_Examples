
with Ada.Numerics.Generic_Real_Arrays;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types.Colors;
with GL.Uniforms;

with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Windows;
with Glfw.Windows.Context;

with Controls;
with Load_DDS;
with Utilities;
--  with VBO_Indexer;

with Particle_System;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   package Real_Array_Functions is new
     Ada.Numerics.Generic_Real_Arrays (GL.Types.Single);

    Dark_Blue              : constant GL.Types.Colors.Color := (0.0, 0.0, 0.4, 0.0);
    White                  : constant GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);

    Vertices_Array_Object  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Element_Buffer         : GL.Objects.Buffers.Buffer;
   Vertex_Buffer_Size      : GL.Types.Long;

    Billboard_Buffer       : GL.Objects.Buffers.Buffer;
    Colour_Buffer          : GL.Objects.Buffers.Buffer;
    Positions_Buffer       : GL.Objects.Buffers.Buffer;
    Particle_Texture       : GL.Objects.Textures.Texture;

    Billboard_Program      : GL.Objects.Programs.Program;

    Last_Time              : GL.Types.Single := GL.Types.Single (Glfw.Time);
    Number_Of_Frames       : Integer := 0;

    function To_Matrix3  (aMatrix4 : GL.Types.Singles.Matrix4)
                          return GL.Types.Singles.Matrix3;

    --  ------------------------------------------------------------------------

   function Convert_To_Real (GL_Matrix : GL.Types.Singles.Matrix3)
                             return Real_Array_Functions.Real_Matrix is
     use Real_Array_Functions;
     Result : Real_Matrix (1 .. 3, 1 .. 3);
   begin
      for row in 1 .. 3 loop
         for col in 1 .. 3 loop
            Result (row, col) := GL_Matrix (GL.Index_Homogeneous'Enum_Val (row),
                                            GL.Index_Homogeneous'Enum_Val (col));
         end loop;
      end loop;
      return Result;
   end Convert_To_Real;

    --  ------------------------------------------------------------------------

    function Convert_To_Single  (R_Matrix : Real_Array_Functions.Real_Matrix)
                                 return GL.Types.Singles.Matrix3 is
     Result  : GL.Types.Singles.Matrix3;
   begin
      for row in 1 .. 3 loop
         for col in 1 .. 3 loop
            Result (GL.Index_Homogeneous'Enum_Val (row),
                    GL.Index_Homogeneous'Enum_Val (col)) := R_Matrix (row, col);
         end loop;
      end loop;
      return Result;
   end Convert_To_Single;

    --  ------------------------------------------------------------------------

   function Inverse (GL_Matrix : GL.Types.Singles.Matrix3)
                     return GL.Types.Singles.Matrix3 is
     use Real_Array_Functions;
     RI_Matrix : Real_Matrix (1 .. 3, 1 .. 3);
   begin
      RI_Matrix := Real_Array_Functions.Inverse (Convert_To_Real (GL_Matrix));
      return Convert_To_Single (RI_Matrix);
   end Inverse;

    --  ------------------------------------------------------------------------

    procedure Load_Buffers (Buffer_Size : out GL.Types.Long) is
        use GL.Objects.Buffers;
        use GL.Types;
        Vertex_Data        : constant Singles.Vector3_Array (1 .. 4) :=
                               ((-0.5, -0.5, 0.0),
                                (0.5, -0.5, 0.0),
                                (-0.5, 0.5, 0.0),
                                (0.5, 0.5, 0.0));
        Vertex_Data_Bytes  : constant Int := Vertex_Data'Size / 8;
   begin
      Buffer_Size := 4 * Long (Particle_System.Max_Particles * Vertex_Data_Bytes);
        Billboard_Buffer.Initialize_Id;
        Array_Buffer.Bind (Billboard_Buffer);
        Utilities.Load_Vertex_Buffer (Array_Buffer, Vertex_Data, Static_Draw);

        Positions_Buffer.Initialize_Id;
        Array_Buffer.Bind (Positions_Buffer);
        Allocate (Array_Buffer, Buffer_Size, Static_Draw);

        Colour_Buffer.Initialize_Id;
        Array_Buffer.Bind (Colour_Buffer);
        Allocate (Array_Buffer, Buffer_Size, Static_Draw);

    exception
        when others =>
            Put_Line ("An exception occurred in Load_Buffers.");
            raise;
    end Load_Buffers;

    --  ------------------------------------------------------------------------

    procedure Load_Matrices (Window  : in out Glfw.Windows.Window) is
        use GL.Types;
      use GL.Types.Singles;
      View_Matrix       : Matrix4;
      View_Matrix3      : Matrix3;
      VM_Inv            : Matrix3;
      Projection_Matrix : Matrix4;
      VP_Matrix         : Matrix4;
      Camera_Position   : Vector3;
    begin
      Controls.Compute_Matrices_From_Inputs (Window, Projection_Matrix, View_Matrix);
      View_Matrix3 := To_Matrix3 (View_Matrix);
      VP_Matrix :=  Projection_Matrix * View_Matrix;
      VM_Inv := Inverse (View_Matrix3);
      Camera_Position := (VM_Inv (GL.Z, GL.X), VM_Inv (GL.Z, GL.Y), VM_Inv (GL.Z, GL.Z));
      Particle_System.Set_IDs (VP_Matrix);

    exception
        when others =>
            Put_Line ("An exception occurred in Load_Matrices.");
            raise;
    end Load_Matrices;

   --  ------------------------------------------------------------------------

    procedure Render (Window : in out Glfw.Windows.Window) is
        use GL.Objects.Buffers;
        use GL.Types;
        Current_Time : constant Single := Single (Glfw.Time);
        Delta_Time   : constant Single := Last_Time - Current_Time;
    begin
      Utilities.Clear_Background_Colour_And_Depth (Dark_Blue);

      Particle_System.Update_Particles (Delta_Time);
      GL.Objects.Buffers.Array_Buffer.Bind (Positions_Buffer);
      GL.Objects.Buffers.Allocate (Array_Buffer, Vertex_Buffer_Size, Stream_Draw);

        Particle_System.Use_Program;
        Load_Matrices (Window);

        --  First attribute buffer : vertices
        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, True, 0, 0);
        --  Second attribute buffer : UVs
        GL.Attributes.Enable_Vertex_Attrib_Array (1);
        GL.Objects.Buffers.Array_Buffer.Bind (UVs_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, Single_Type, True, 0, 0);
        --  Third attribute buffer : normals
        GL.Attributes.Enable_Vertex_Attrib_Array (2);
        GL.Objects.Buffers.Array_Buffer.Bind (Normals_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer (2, 3, Single_Type, True, 0, 0);

        --  Index Buffer
        GL.Objects.Buffers.Element_Array_Buffer.Bind (Element_Buffer);

        --  Bind the texture in Texture Unit 0
        GL.Objects.Textures.Set_Active_Unit (0);
        GL.Objects.Textures.Targets.Texture_2D.Bind (UV_Map);
        GL.Uniforms.Set_Int (Texture_ID, 0);

        GL.Objects.Buffers.Draw_Elements (Triangles, Indices_Size, UInt_Type, 0);

        GL.Attributes.Disable_Vertex_Attrib_Array (0);
        GL.Attributes.Disable_Vertex_Attrib_Array (1);
        GL.Attributes.Disable_Vertex_Attrib_Array (2);
    exception
        when others =>
            Put_Line ("An exception occurred in Render.");
            raise;
    end Render;

    --  ------------------------------------------------------------------------

    procedure Setup (Window : in out Glfw.Windows.Window) is
        use GL.Types;
        use Glfw.Input;
        Window_Width    : constant Glfw.Size := 1024;
        Window_Height   : constant Glfw.Size := 768;

    begin
        Window.Set_Input_Toggle (Sticky_Keys, True);
        Window.Set_Cursor_Mode (Mouse.Disabled);

        Window'Access.Set_Size (Window_Width, Window_Height);
        Window'Access.Set_Cursor_Pos (Mouse.Coordinate (0.5 * Single (Window_Width)),
                                      Mouse.Coordinate (0.5 * Single (Window_Height)));
        Utilities.Clear_Background_Colour_And_Depth (Dark_Blue);

        GL.Toggles.Enable (GL.Toggles.Depth_Test);
        GL.Buffers.Set_Depth_Function (GL.Types.Less);

        Vertices_Array_Object.Initialize_Id;
      Vertices_Array_Object.Bind;

        Load_DDS ("src/textures/particle.DDS", Particle_Texture);

        Load_Buffers (Vertex_Buffer_Size);
        Last_Time := Single (Glfw.Time);
    exception
        when others =>
            Put_Line ("An exception occurred in Setup.");
            raise;
    end Setup;

    --  ------------------------------------------------------------------------

    function To_Matrix3  (aMatrix4 : GL.Types.Singles.Matrix4)
                          return GL.Types.Singles.Matrix3 is
     Result : GL.Types.Singles.Matrix3;
   begin
      for row in GL.X .. GL.Z loop
         for col in GL.X .. GL.Z loop
            Result (row, col) := aMatrix4 (row, col);
         end loop;
      end loop;
      return Result;
   end To_Matrix3;

    --  ------------------------------------------------------------------------

    use Glfw.Input;
    Running         : Boolean := True;
begin
    Setup (Main_Window);
    while Running loop
        Render (Main_Window);
        Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        Glfw.Input.Poll_Events;
        Running := Running and then
          not (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
        Running := Running and then not Main_Window.Should_Close;
    end loop;

exception
    when others =>
        Put_Line ("An exception occurred in Main_Loop.");
        raise;
end Main_Loop;
