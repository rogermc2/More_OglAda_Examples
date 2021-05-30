
with GL.Attributes;
with GL.Objects.Programs;
with GL.Types;

with Load_Object_File2;
with Maths;
with Utilities;

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
        aModel.Model_Colour := Colour;
        aModel.Model_VAO.Initialize_Id;
        aModel.Model_VAO.Bind;

        Vertex_Count := Load_Object_File2.Mesh_Size (File_Path);
        declare
            Vertices         : Singles.Vector3_Array (1 .. Vertex_Count);
            UVs              : Singles.Vector2_Array (1 .. Vertex_Count);
            Normals          : Singles.Vector3_Array (1 .. Vertex_Count);
        begin
            Shader_Manager_Model.Init_Shaders (Model_Program);
            Load_Object_File2.Load_Object (File_Path, Vertices, UVs, Normals);
            for index in 1 .. Vertex_Count loop
                aModel.Vertices.Append (Vertices (index));
                aModel.Normals.Append (Normals (index));
                aModel.UVs.Append (UVs (index));
            end loop;

            aModel.Model_Vertex_Buffer.Initialize_Id;
            Array_Buffer.Bind (aModel.Model_Vertex_Buffer);
            Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);
            GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, False,
                                                     0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (0);

            aModel.Model_Normals_Buffer.Initialize_Id;
            Array_Buffer.Bind (aModel.Model_Normals_Buffer);
            Utilities.Load_Vertex_Buffer (Array_Buffer, Normals, Static_Draw);
            GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, Single_Type, False,
                                                     0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (1);

--              aModel.Model_UVs_Buffer.Initialize_Id;
--              Array_Buffer.Bind (aModel.Model_UVs_Buffer);
--              Utilities.Load_Vertex_Buffer (Array_Buffer, UVs, Static_Draw);
--              GL.Attributes.Set_Vertex_Attrib_Pointer (2, 2, Single_Type, False,
--                                                       0, 0);
--              GL.Attributes.Enable_Vertex_Attrib_Array (2);

        end;

    end Initialize;

    --  ------------------------------------------------------------------------

    procedure Render (aModel : in out Model_Data) is
        use GL.Objects.Buffers;
        use Shader_Manager_Model;
        use Vertices_Package;
--          Current_Position : Vector3 := aModel.Position;
--          Target_Rotation  : Vector3 := aModel.Heading_Rotation;
        Model_Matrix  : Matrix4 := Singles.Identity4;
        View_Matrix   : Matrix4 := Singles.Identity4;
        Num_Vertices  : constant Int := Int (aModel.Vertices.Length);
        Vertices      : Singles.Vector3_Array (1 .. Num_Vertices);
        Normals       : Singles.Vector3_Array (1 .. Num_Vertices);
        Index         : Int := 0;
        V_Cursor      : Cursor := aModel.Vertices.First;
        N_Cursor      : Cursor := aModel.Normals.First;
    begin
        if aModel.Is_Visible then
            Maths.Init_Rotation_Transform
              (aModel.Base_Rotation, Model_Matrix);
            aModel.Model_VAO.Bind;
            while Has_Element (V_Cursor) loop
                Index := Index + 1;
                Vertices (Index) := Element  (V_Cursor);
                Normals (Index) := Element  (N_Cursor);
                Next (V_Cursor);
                Next (N_Cursor);
            end loop;

            if aModel.Is_Ship then
                Maths.Init_Rotation_Transform
                  (aModel.Base_Rotation, View_Matrix);
                View_Matrix := Maths.Translation_Matrix (aModel.Position) *
                  View_Matrix;
            end if;

            if not aModel.Is_Ship then
                for v_index in Vertices'Range loop
                    Vertices (v_index) := Vertices (v_index) + aModel.Position;
                end loop;
            end if;

            GL.Attributes.Disable_Vertex_Attrib_Array (0);
            aModel.Model_Vertex_Buffer.Clear;
            Array_Buffer.Bind (aModel.Model_Vertex_Buffer);
            Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);
            GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, False,
                                                     0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (0);

            GL.Attributes.Disable_Vertex_Attrib_Array (1);
            aModel.Model_Normals_Buffer.Clear;
            Array_Buffer.Bind (aModel.Model_Normals_Buffer);
            Utilities.Load_Vertex_Buffer (Array_Buffer, Normals, Static_Draw);
            GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, Single_Type, False,
                                                     0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (1);

            GL.Objects.Programs.Use_Program (Model_Program);
            Set_Colour (aModel.Model_Colour);
            Set_Model_Matrix (Model_Matrix);
            Set_View_Matrix (View_Matrix);
        end if;
    end Render;

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

    end Update;

    --  ------------------------------------------------------------------------

end Model;
