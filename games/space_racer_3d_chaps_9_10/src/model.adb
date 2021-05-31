
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Programs;
with GL.Types;

with Load_Object_File;
with Maths;
with Utilities;

--  with Buffers_Manager;
with Shader_Manager_Model;

package body Model is
    use GL.Types;

    Model_Program  : GL.Objects.Programs.Program;

    --  ------------------------------------------------------------------------

    procedure Bind_Model_VAO (aModel : in out Model_Data) is
    begin
        aModel.Model_VAO.Bind;
    end Bind_Model_VAO;

    --  ------------------------------------------------------------------------

    procedure Initialize (aModel : in out Model_Data; File_Path : String;
                          Colour : GL.Types.Colors.Basic_Color) is
        use GL.Objects.Buffers;
        Vertex_Count : Int;
    begin
        Shader_Manager_Model.Init_Shaders (Model_Program);
        aModel.Model_Colour := Colour;
        aModel.Model_VAO.Initialize_Id;
        aModel.Model_VAO.Bind;

        Vertex_Count := Load_Object_File.Mesh_Size (File_Path);
        Put_Line ("Model.Initialize Vertex_Count: " &
                    Int'Image (Vertex_Count));
        declare
            Vertices         : Singles.Vector3_Array (1 .. Vertex_Count);
            UVs              : Singles.Vector2_Array (1 .. Vertex_Count);
            Normals          : Singles.Vector3_Array (1 .. Vertex_Count);
        begin
            Load_Object_File.Load_Object (File_Path, Vertices, UVs, Normals);
            for index in 1 .. Vertex_Count loop
                aModel.Vertices.Append (Vertices (index));
            end loop;

            aModel.Model_Vertex_Buffer.Initialize_Id;
            Array_Buffer.Bind (aModel.Model_Vertex_Buffer);
            Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);
            GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, False,
                                                     0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (0);
        end;

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Model.Initialize.");
            Put_Line (Exception_Information (anError));
            raise;
    end Initialize;

    --  ------------------------------------------------------------------------

    procedure Render (aModel : in out Model_Data) is
        use GL.Objects.Buffers;
        use Shader_Manager_Model;
        use Vertices_Package;
        Model_Matrix  : Matrix4 := Singles.Identity4;
        View_Matrix   : Matrix4 := Singles.Identity4;
        Num_Vertices  : constant Int := Int (aModel.Vertices.Length);
        Vertices      : Singles.Vector3_Array (1 .. Num_Vertices);
        Index         : Int := 0;
        V_Cursor      : Cursor := aModel.Vertices.First;
    begin
        if aModel.Is_Visible then
            --              Put_Line ("Model.Render Num_Vertices " & Int'Image (Num_Vertices));
            Maths.Init_Rotation_Transform
              (aModel.Base_Rotation, Model_Matrix);
            aModel.Model_VAO.Bind;
            while Has_Element (V_Cursor) loop
                Index := Index + 1;
                Vertices (Index) := Element (V_Cursor);
                Next (V_Cursor);
            end loop;

            if aModel.Is_Ship then
                Maths.Init_Rotation_Transform
                  (aModel.Base_Rotation, View_Matrix);
                View_Matrix := Maths.Scaling_Matrix ((1.0, 1.0, 1.0)) *
                  Maths.Translation_Matrix (aModel.Position) * View_Matrix;
            end if;

            if not aModel.Is_Ship then
                for v_index in Vertices'Range loop
                    Vertices (v_index) := Vertices (v_index) + aModel.Position;
                end loop;
            end if;
            Utilities.Print_GL_Array3 ("Model.Render.Vertices", Vertices);
            Array_Buffer.Bind (aModel.Model_Vertex_Buffer);
            Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);

            GL.Objects.Programs.Use_Program (Model_Program);
            Set_Colour (aModel.Model_Colour);
            Set_Model_Matrix (Model_Matrix);
            Set_View_Matrix (View_Matrix);

            GL.Objects.Vertex_Arrays.Draw_Arrays
              (Triangles, 0, Vertices'Length / 3);
        end if;

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Model.Render.");
            Put_Line (Exception_Information (anError));
            raise;
    end Render;

    --  ------------------------------------------------------------------------

    procedure Set_Base_Rotation (aModel   : in out Model_Data;
                                 Rotation : GL.Types.Singles.Vector3) is
    begin
        aModel.Base_Rotation := Rotation;
    end Set_Base_Rotation;

    --  ------------------------------------------------------------------------

    procedure Set_Is_Ship (aModel : in out Model_Data; State : Boolean) is
    begin
        aModel.Is_Ship := State;
    end Set_Is_Ship;

    --  ------------------------------------------------------------------------

    procedure Set_Perspective (Projection_Matrix : GL.Types.Singles.Matrix4) is
    begin
        GL.Objects.Programs.Use_Program (Model_Program);
        Shader_Manager_Model.Set_Projection_Matrix (Projection_Matrix);
    end Set_Perspective;

    --  ------------------------------------------------------------------------

    procedure Set_Position (aModel   : in out Model_Data;
                            Position : GL.Types.Singles.Vector3) is
    begin
        aModel.Position := Position;
    end Set_Position;

    --  ------------------------------------------------------------------------

    procedure Set_Velocity (aModel   : in out Model_Data;
                            Velocity : GL.Types.Single) is
    begin
        aModel.Velocity := Velocity;
    end Set_Velocity;

    --  ------------------------------------------------------------------------

    procedure Update (aModel : in out Model_Data; Delta_Time : Float) is
        use Maths;
        use Maths.Single_Math_Functions;
        Distance         : constant Single :=
                             aModel.Velocity * Single (Delta_Time);
        Target_Position  : Vector3 := aModel.Position;
        Delta_Position   : Vector3;
    begin
        Delta_Position (GL.X) := Cos (Target_Position (GL.Z)) * Distance;
        Delta_Position (GL.Y) := -Sin (Target_Position (GL.Z)) * Distance;
        Delta_Position (GL.Z) := Sin (Target_Position (GL.X)) * Distance;

        Target_Position := Target_Position + Delta_Position;
        aModel.Position := Target_Position;

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Model.Render.");
            Put_Line (Exception_Information (anError));
            raise;
    end Update;

    --  ------------------------------------------------------------------------

end Model;
