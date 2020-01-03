
with Ada.Containers.Vectors;
with Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

package body Sphere is

    Min_Sector_Count : constant Int := 3;
    Min_Stack_Count  : constant Int := 2;

    type Flat_Vertex is record
        X : Single := 0.0;
        Y : Single := 0.0;
        Z : Single := 0.0;
        S : Single := 0.0;
        T : Single := 0.0;
    end record;
    package Flat_Vertex_Package is new Ada.Containers.Vectors (Natural, Flat_Vertex);
    type Flat_Vertex_Vector is new Flat_Vertex_Package.Vector with null record;

--      package Face_Normal_Package is new Ada.Containers.Doubly_Linked_Lists (Single);
--      type Face_Normal_List is new Face_Normal_Package.List with null record;

    function Compute_Face_Normal (X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3 : Single)
                                  return Vertex;

    --   ----------------------------------------------------------------------

    procedure Build_Interleaved_Vertices (theSphere : in out Sphere) is
        use Vertex_Data_Package;
        use Tex_Coords_Package;
        use Maths;
        Vertex_Cursor    : Vertex_Data_Package.Cursor := theSphere.Vertices.First;
        Normals_Cursor   : Vertex_Data_Package.Cursor := theSphere.Normals.First;
        Tex_Cursor       : Tex_Coords_Package.Cursor := theSphere.Tex_Coords.First;
        I_Vec            : Interleaved_Vector;
    begin
        Reverse_Elements (theSphere.Interleaved_Vertices);
        for count in 1 .. Int (theSphere.Vertices.Length) loop
            I_Vec (X) := Element (Vertex_Cursor).X;
            I_Vec (Y) := Element (Vertex_Cursor).Y;
            I_Vec (Z) := Element (Vertex_Cursor).Z;
            Next (Vertex_Cursor);

            I_Vec (NX) := Element (Normals_Cursor).X;
            I_Vec (NY) := Element (Normals_Cursor).Y;
            I_Vec (NZ) := Element (Normals_Cursor).Z;
            Next (Normals_Cursor);

            I_Vec (U) := Element (Tex_Cursor).U;
            I_Vec (U) := Element (Tex_Cursor).V;
            Next (Tex_Cursor);
            theSphere.Interleaved_Vertices.Append (I_Vec);
        end loop;
    end Build_Interleaved_Vertices;

    --   ----------------------------------------------------------------------

    procedure Build_Vertices_Smooth (theSphere : in out Sphere) is
        use Maths.Single_Math_Functions;
        aVertex        : Vertex;
        XY             : Single;
        Normals        : Vertex;
        UV             : Tex_Coords;
        Inverse_Length : constant Single := 1.0 / theSphere.Radius;
        Stack_Step     : constant Single :=
                            Ada.Numerics.Pi / Single (theSphere.Stack_Count);
        Stack_Angle    : Single;
        Sector_Step    : constant Single :=
                           2.0 * Ada.Numerics.Pi / Single (theSphere.Sector_Count);
        Sector_Angle   : Single;
        k1             : UInt;
        k2             : UInt;
    begin
        for index in 1 .. theSphere.Stack_Count loop
            Stack_Angle := Ada.Numerics.Pi / 2.0 - Single (index - 1) * Stack_Step;
            XY := theSphere.Radius * Cos (Stack_Angle);
            aVertex.Z := theSphere.Radius * Sin (Stack_Angle);
            for index_2 in 1 .. theSphere.Sector_Count loop
                Sector_Angle := Single (index_2 - 1) * Sector_Step;
                aVertex.X := XY * Cos (Sector_Angle);
                aVertex.Y := XY * Sin (Sector_Angle);
                theSphere.Vertices.Append (aVertex);

                Normals.X := aVertex.X * Inverse_Length;
                Normals.Y := aVertex.Y * Inverse_Length;
                Normals.Z := aVertex.Z * Inverse_Length;
                theSphere.Normals.Append (Normals);

                UV.U := Single (index_2 - 1) / Single (theSphere.Sector_Count);
                UV.V := Single (index - 1) / Single (theSphere.Stack_Count);
                theSphere.Tex_Coords.Append (UV);
            end loop;
        end loop;

        for index in 1 .. theSphere.Stack_Count loop
            --  Two triangles per sector excluding 1st and last stacks
            k1 := UInt ((index - 1) * (theSphere.Sector_Count + 1));  --  beginning of current stack
            k2 := k1 + UInt (theSphere.Sector_Count + 1);             --  beginning of next stack
            for index_2 in 1 .. theSphere.Sector_Count loop
                if index /= 1 then
                    theSphere.Indices.Append ((k1, k2, k1 + 1));
                end if;
                if index /= theSphere.Sector_Count then
                    theSphere.Indices.Append ((k1 + 1, k2, k2 + 1));
                end if;
                --  Vertical lines for all stacks
                theSphere.Line_Indices.Append (k1);
                theSphere.Line_Indices.Append (k2);
                --  Horizontal lines except 1st stack
                if index /= 1 then
                    theSphere.Line_Indices.Append (k1);
                    theSphere.Line_Indices.Append (k1 + 1);
                end if;
                k1 := k1 + 1;
                k2 := k2 + 1;
            end loop;
        end loop;

        Build_Interleaved_Vertices (theSphere);
    end Build_Vertices_Smooth;

    --   ----------------------------------------------------------------------

    procedure Build_Vertices_Flat (theSphere : in out Sphere) is
        use Maths.Single_Math_Functions;
        use Flat_Vertex_Package;
        Flat_Vertices  : Flat_Vertex_Vector;
        aVertex        : Flat_Vertex;
        Vertex_1       : Flat_Vertex;
        Vertex_2       : Flat_Vertex;
        Vertex_3       : Flat_Vertex;
        Vertex_4       : Flat_Vertex;
        XY             : Single;
        Stack_Step     : constant Single :=
                            Ada.Numerics.Pi / Single (theSphere.Stack_Count);
        Stack_Angle    : Single;
        Sector_Step    : constant Single :=
                           2.0 * Ada.Numerics.Pi / Single (theSphere.Sector_Count);
        Sector_Angle   : Single;
        Face_Normal    : Vertex;
        Vertex_Index   : UInt := 0;
        V_Index_1      : Natural;
        V_Index_2      : Natural;
    begin
        --  compute all vertices first, each vertex contains (x,y,z,s,t) except normal
        for index in 1 .. theSphere.Stack_Count loop
            Stack_Angle := Ada.Numerics.Pi / 2.0 - Single (index - 1) * Stack_Step;
            XY := theSphere.Radius * Cos (Stack_Angle);
            aVertex.Z := theSphere.Radius * Sin (Stack_Angle);
            for index_2 in 1 .. theSphere.Sector_Count loop
                Sector_Angle := Single (index_2 - 1) * Sector_Step;
                aVertex.X := XY * Cos (Sector_Angle);
                aVertex.Y := XY * Sin (Sector_Angle);
                aVertex.S := Single (index_2 - 1) / Single (theSphere.Sector_Count);
                aVertex.T := Single (index - 1) / Single (theSphere.Stack_Count);
                Flat_Vertices.Append (aVertex);
            end loop;
        end loop;

        for index in Natural range 1 .. Natural (theSphere.Stack_Count - 1) loop
            V_Index_1 := (index - 1) * Natural (theSphere.Sector_Count);
            V_Index_2 := index * Natural (theSphere.Sector_Count);
            for index_2 in Natural range 1 .. Natural (theSphere.Sector_Count) loop
                Vertex_1 := Element (To_Cursor (Flat_Vertices, V_Index_1));
                Vertex_2 := Element (To_Cursor (Flat_Vertices, V_Index_2));
                Vertex_3 := Element (To_Cursor (Flat_Vertices, V_Index_1 + 1));
                Vertex_4 := Element (To_Cursor (Flat_Vertices, V_Index_2 + 1));

                --  If 1st stack and last stack, store only 1 triangle per sector
                --  otherwise, store 2 triangles (quad) per sector
                if index = 1 then
                    theSphere.Vertices.Append ((Vertex_1.X, Vertex_1.Y, Vertex_1.Z));
                    theSphere.Vertices.Append ((Vertex_2.X, Vertex_2.Y, Vertex_2.Z));
                    theSphere.Vertices.Append ((Vertex_4.X, Vertex_4.Y, Vertex_4.Z));
                    theSphere.Tex_Coords.Append ((Vertex_1.S, Vertex_1.T));
                    theSphere.Tex_Coords.Append ((Vertex_2.S, Vertex_2.T));
                    theSphere.Tex_Coords.Append ((Vertex_4.S, Vertex_4.T));

                    Face_Normal := Compute_Face_Normal
                      (Vertex_1.X, Vertex_1.Y, Vertex_1.Z,
                       Vertex_2.X, Vertex_2.Y, Vertex_2.Z,
                       Vertex_4.X, Vertex_4.Y, Vertex_4.Z);
                    --  same normals for 3 vertices
                    theSphere.Normals.Append (Face_Normal);
                    theSphere.Normals.Append (Face_Normal);
                    theSphere.Normals.Append (Face_Normal);

                    theSphere.Indices.Append ((Vertex_Index, Vertex_Index + 1,
                                              Vertex_Index + 2));

                    --  indices for line (first stack requires only vertical line)
                    theSphere.Line_Indices.Append (Vertex_Index);
                    theSphere.Line_Indices.Append (Vertex_Index + 1);

                    Vertex_Index := Vertex_Index + 3;
                elsif index = Integer (theSphere.Stack_Count) then
                    theSphere.Vertices.Append ((Vertex_1.X, Vertex_1.Y, Vertex_1.Z));
                    theSphere.Vertices.Append ((Vertex_2.X, Vertex_2.Y, Vertex_2.Z));
                    theSphere.Vertices.Append ((Vertex_3.X, Vertex_3.Y, Vertex_3.Z));
                    theSphere.Tex_Coords.Append ((Vertex_1.S, Vertex_1.T));
                    theSphere.Tex_Coords.Append ((Vertex_2.S, Vertex_2.T));
                    theSphere.Tex_Coords.Append ((Vertex_3.S, Vertex_3.T));

                    Face_Normal := Compute_Face_Normal
                      (Vertex_1.X, Vertex_1.Y, Vertex_1.Z,
                       Vertex_2.X, Vertex_2.Y, Vertex_2.Z,
                       Vertex_3.X, Vertex_3.Y, Vertex_3.Z);
                    --  same normals for 3 vertices
                    theSphere.Normals.Append (Face_Normal);
                    theSphere.Normals.Append (Face_Normal);
                    theSphere.Normals.Append (Face_Normal);

                    theSphere.Indices.Append ((Vertex_Index, Vertex_Index + 1,
                                              Vertex_Index + 2));
                    --  indices for line (last stack requires both vert/hori lines)
                    theSphere.Line_Indices.Append (Vertex_Index);
                    theSphere.Line_Indices.Append (Vertex_Index + 1);
                    theSphere.Line_Indices.Append (Vertex_Index);
                    theSphere.Line_Indices.Append (Vertex_Index + 2);

                    Vertex_Index := Vertex_Index + 3;
                else --  2 triangles for others
                    theSphere.Vertices.Append ((Vertex_1.X, Vertex_1.Y, Vertex_1.Z));
                    theSphere.Vertices.Append ((Vertex_2.X, Vertex_2.Y, Vertex_2.Z));
                    theSphere.Vertices.Append ((Vertex_3.X, Vertex_3.Y, Vertex_3.Z));
                    theSphere.Vertices.Append ((Vertex_4.X, Vertex_4.Y, Vertex_4.Z));
                    theSphere.Tex_Coords.Append ((Vertex_1.S, Vertex_1.T));
                    theSphere.Tex_Coords.Append ((Vertex_2.S, Vertex_2.T));
                    theSphere.Tex_Coords.Append ((Vertex_3.S, Vertex_3.T));
                    theSphere.Tex_Coords.Append ((Vertex_4.S, Vertex_4.T));

                    Face_Normal := Compute_Face_Normal
                      (Vertex_1.X, Vertex_1.Y, Vertex_1.Z,
                       Vertex_2.X, Vertex_2.Y, Vertex_2.Z,
                       Vertex_3.X, Vertex_3.Y, Vertex_3.Z);
                    --  same normals for 4 vertices
                    theSphere.Normals.Append (Face_Normal);
                    theSphere.Normals.Append (Face_Normal);
                    theSphere.Normals.Append (Face_Normal);
                    theSphere.Normals.Append (Face_Normal);

                    theSphere.Indices.Append ((Vertex_Index, Vertex_Index + 1,
                                              Vertex_Index + 2));
                    theSphere.Indices.Append ((Vertex_Index + 2, Vertex_Index + 1,
                                               Vertex_Index + 3));

                    theSphere.Line_Indices.Append (Vertex_Index);
                    theSphere.Line_Indices.Append (Vertex_Index + 1);
                    theSphere.Line_Indices.Append (Vertex_Index);
                    theSphere.Line_Indices.Append (Vertex_Index + 2);

                    Vertex_Index := Vertex_Index + 4;
                end if;
            end loop;
        end loop;

        Build_Interleaved_Vertices (theSphere);

    exception
        when others =>
            Put_Line ("An exception occurred in Sphere.Build_Vertices_Flat.");
            raise;

    end Build_Vertices_Flat;

    --   ----------------------------------------------------------------------

    function Compute_Face_Normal (X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3 : Single)
                                  return Vertex is
       use Maths.Single_Math_Functions;
       Normals    : Vertex;
       Epsilon    : constant Single := 0.000001;
       Ex1        : constant Single := X2 - X1;
       Ey1        : constant Single := Y2 - Y1;
       Ez1        : constant Single := Z2 - Z1;
       Ex2        : constant Single := X3 - X1;
       Ey2        : constant Single := Y3 - Y1;
       Ez2        : constant Single := Z3 - Z1;
       --  Cross products
       Nx         : constant Single := Ey1 * Ez2 - Ez1 * Ey2;
       Ny         : constant Single := Ez1 * Ex2 - Ex1 * Ez2;
       Nz         : constant Single := Ex1 * Ey2 - Ey1 * Ex2;
       Vec_Length : constant Single := Sqrt (Nx * Nx + Ny * Ny + Nz * Nz);
       Inv_Length : Single;
    begin
        if Vec_Length > Epsilon then
            Inv_Length := 1.0 / Vec_Length;
            Normals := (Inv_Length * Nx, Inv_Length * Ny, Inv_Length * Nz);
        end if;
        return Normals;

    exception
        when others =>
            Put_Line ("An exception occurred in Sphere.Compute_Face_Normal.");
            raise;
    end Compute_Face_Normal;

    --   ----------------------------------------------------------------------

    function Get_Indices (theSphere : Sphere) return Int_Array is
        use Ada.Containers;
        use Indices_Package;
        Index_Cursor : Indices_Package.Cursor :=
                          theSphere.Indices.First;
        S_Indices    : Int_Array
          (1 .. Int (3 * theSphere.Indices.Length));
        V_Indices    : Triangle_Indices;
        I_Index      : Int := 0;
    begin
        while Has_Element (Index_Cursor) loop
            V_Indices := (Element (Index_Cursor));
            I_Index := I_Index + 1;
            S_Indices (I_Index) := Int (V_Indices.Vertex_1);
            I_Index := I_Index + 1;
            S_Indices (I_Index) := Int (V_Indices.Vertex_2);
            I_Index := I_Index + 1;
            S_Indices (I_Index) := Int (V_Indices.Vertex_3);
            Next (Index_Cursor);
        end loop;
        return S_Indices;

    exception
        when others =>
            Put_Line ("An exception occurred in Sphere.Get_Indices.");
            raise;
    end Get_Indices;

    --   ----------------------------------------------------------------------

    function Get_Indices_Size (theSphere : Sphere) return Int is
    begin
        return 3 * Int (theSphere.Indices.Length);
    end Get_Indices_Size;

    --   ----------------------------------------------------------------------

    function Get_Interleaved_Size (theSphere : Sphere) return Int is
    begin
        return Int (theSphere.Interleaved_Vertices.Length);
    end Get_Interleaved_Size;

    --   ----------------------------------------------------------------------

    function Get_Interleaved_Stride return Int is
    begin
        return Int (Interleaved_Vector'Size / 8);
    end Get_Interleaved_Stride;

    --   ----------------------------------------------------------------------
    function Get_Interleaved_Vertices (theSphere : Sphere)
                                       return Maths.Vector8_Array is
        use Interleaved_Vertices_Package;
        use Maths;
        Vertex_Cursor : Interleaved_Vertices_Package.Cursor :=
                          theSphere.Interleaved_Vertices.First;
        Vertices      : Vector8_Array
          (1 .. Int (theSphere.Interleaved_Vertices.Length));
        V_Index       : Int := 0;
    begin
        while Has_Element (Vertex_Cursor) loop
            V_Index := V_Index + 1;
            Vertices (V_Index) := Vector8 (Element (Vertex_Cursor));
            Next (Vertex_Cursor);
        end loop;
        return Vertices;

    exception
        when others =>
            Put_Line ("An exception occurred in Sphere.Get_Interleaved_Vertices.");
            raise;
    end Get_Interleaved_Vertices;

    --   ----------------------------------------------------------------------

    procedure Init (theSphere : out Sphere; Radius : Single := 1.0;
                    Sector_Count : Int := 36; Stack_Count : Int := 18;
                    Smooth : Boolean := True) is
    begin
        theSphere.Radius := Radius;
        if Sector_Count < Min_Sector_Count then
            theSphere.Sector_Count := Min_Sector_Count;
        else
            theSphere.Sector_Count := Sector_Count;
        end if;
        if Stack_Count < Min_Stack_Count then
            theSphere.Sector_Count := Min_Stack_Count;
        else
            theSphere.Stack_Count := Stack_Count;
        end if;

        theSphere.Smooth := Smooth;
        if Smooth then
            Build_Vertices_Smooth (theSphere);
        else
            Build_Vertices_Flat (theSphere);
        end if;

    exception
        when others =>
            Put_Line ("An exception occurred in Sphere.Init.");
            raise;
    end Init;

    --   ----------------------------------------------------------------------

end Sphere;
