
with Interfaces.C;
with Interfaces.C.Pointers;

with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Low_Level.Enums;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;

with Ogldev_Engine_Common;
with Ogldev_Texture;

with Billboard_Technique_27;

Package body Billboard_List is
    use GL.Types;
    Num_Rows : constant Int := 10;
    Num_Cols : constant Int := 10;

    type Position_Vector is array (1 .. Num_Cols) of aliased Singles.Vector3;
    pragma Convention (C, Position_Vector);

    type Positions_Array is array (Size range <>) of aliased Position_Vector;
    pragma Convention (C, Positions_Array);

    package Position_Vector_Pointers is new Interfaces.C.Pointers
      (Size, Position_Vector, Positions_Array, Position_Vector'(others => <>));

    procedure Load_Positions_Buffer is new GL.Objects.Buffers.Load_To_Buffer
      (Position_Vector_Pointers);

    Billboard_Technique : Billboard_Technique_27.Technique;
    VBO                 : GL.Objects.Buffers.Buffer;
    Billboard_Texture   : Ogldev_Texture.Ogl_Texture;

    --  ----------------------------------------------------------------------

    procedure Create_Position_Buffer is
        use GL.Objects.Buffers;
        Positions : Positions_Array (1 .. Num_Rows);
        Position  : Position_Vector;
    begin
        for Row in 1 .. Num_Rows loop
            for Col in 1 .. Num_Cols loop
                Position (Col) := (Single (Row), 0.0, Single (Col));
            end loop;
            Positions (Row) := Position;
        end loop;

        VBO.Initialize_Id;
        Array_Buffer.Bind (VBO);
        Load_Positions_Buffer (Array_Buffer, Positions, Static_Draw);
    end Create_Position_Buffer;

    --  ----------------------------------------------------------------------

    function Init (Tex_File_Name : String) return Boolean is
        OK : Boolean;
    begin
        OK := Ogldev_Texture.Init_Texture
          (Billboard_Texture, GL.Low_Level.Enums.Texture_2D, Tex_File_Name);
        if OK then
            Ogldev_Texture.Load (Billboard_Texture);
            Create_Position_Buffer;
        end if;
        OK := OK and Billboard_Technique_27.Init (Billboard_Technique);
        return OK;
    end Init;

    --  ----------------------------------------------------------------------

    procedure Render (View_Point_Matrix : GL.Types.Singles.Matrix4;
                      Camera_Position : GL.Types.Singles.Vector3) is
    begin
        Billboard_Technique_27.Use_Program (Billboard_Technique);
        Billboard_Technique_27.Set_VP_Location (Billboard_Technique, View_Point_Matrix);
        Billboard_Technique_27.Set_Camera_Position_Location (Billboard_Technique, Camera_Position);

        Ogldev_Texture.Bind (Billboard_Texture, Ogldev_Engine_Common.Colour_Texture_Unit);

        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        GL.Objects.Buffers.Array_Buffer.Bind (VBO);
        GL.Attributes.Set_Vertex_Attrib_Pointer
          (Index  => 0, Count => 3, Kind => Single_Type, Stride => 0, Offset => 0);

        GL.Objects.Vertex_Arrays.Draw_Arrays (GL.Types.Points, 0, Num_Rows * Num_Cols);

        GL.Attributes.Disable_Vertex_Attrib_Array (0);

    exception
        when others =>
            Put_Line ("An exception occurred in Billboard_List.Render.");
            raise;
    end Render;

    --  --------------------------------------------------------------------------

end Billboard_List;
