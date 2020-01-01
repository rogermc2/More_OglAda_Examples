
with Ada.Containers.Doubly_Linked_Lists;

with GL.Types; use GL.Types;

package Sphere is
    type Sphere is private;

    procedure Init (theSphere : out Sphere; Radius : Single := 1.0;
                    Sector_Count : Int := 36; Stack_Count : Int := 18;
                    Smooth : Boolean := True);

private
    type Vertex is record
        X : Single := 0.0;
        Y : Single := 0.0;
        Z : Single := 0.0;
    end record;
    package Vertex_Data_Package is new Ada.Containers.Doubly_Linked_Lists (Vertex);
    type Vertex_List is new Vertex_Data_Package.List with null record;

    type Indices is record
        Vertex_1 : UInt := 0;
        Vertex_2 : UInt := 0;
        Vertex_3 : UInt := 0;
    end record;
    package Indices_Package is new Ada.Containers.Doubly_Linked_Lists (Indices);
    type Indices_List is new Indices_Package.List with null record;

    package Line_Indices_Package is new Ada.Containers.Doubly_Linked_Lists (UInt);
    type Line_Indices_List is new Line_Indices_Package.List with null record;

    package Interleaved_Vertices_Package is new Ada.Containers.Doubly_Linked_Lists (Single);
    type Interleaved_Vertices_List is new Interleaved_Vertices_Package.List with null record;

    type Tex_Coords is record
        U : Single := 0.0;
        V : Single := 0.0;
    end record;

    package Tex_Coords_Package is new Ada.Containers.Doubly_Linked_Lists (Tex_Coords);
    type Tex_Coords_List is new Tex_Coords_Package.List with null record;

    type Vertex_Array is new  Singles.Vector3_Array;
    type Normals_Array is new  Singles.Vector3_Array;
    type Tex_Coords_Array is new  Singles.Vector3_Array;
    type Indices_Array is new  UInt_Array;
    type Line_Indices_Array is new  UInt_Array;
    type Sphere is record
        Radius               : Single := 1.0;
        Sector_Count         : Int := 36;
        Stack_Count          : Int := 18;
        Smooth               : Boolean := True;
        Interleaved_Stride   : Int := 0;
        Vertices             : Vertex_List;
        Interleaved_Vertices : Interleaved_Vertices_List;
        Normals              : Vertex_List;
        Tex_Coords           : Tex_Coords_List;
        Indices              : Indices_List;
        Line_Indices         : Line_Indices_List;
    end record;

end Sphere;
