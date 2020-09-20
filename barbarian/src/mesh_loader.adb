
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Buffers;

with Game_Utils;

package body Mesh_Loader is
    use GL.Types.Singles;
    type Animation is record
        null;
    end record;

    package Animations_Package is new Ada.Containers.Doubly_Linked_Lists (Animation);
    type Animations_List is new Animations_Package.List with null record;

    package Matrix4_Package is new Ada.Containers.Doubly_Linked_Lists (Singles.Matrix4);
    type Matrix4_List is new Matrix4_Package.List with null record;

    type Node_Children_Array is array (1 .. Max_Bones, 1 .. Max_Bones) of Integer;

    type Mesh is record
        File_Name              : Unbounded_String := To_Unbounded_String ("");
        VAO                    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        Shadow_VAO             : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        Point_Count            : Integer := 0;
        Vp_Count               : Integer := 0;
        Vn_Count               : Integer := 0;
        Vt_Count               : Integer := 0;
        Vtan_Count             : Integer := 0;
        Vb_Count               : Integer := 0;
        Bounding_Radius        : Float := 1.0;
        --  the skeleton hierarchy
        Root_Transform_Matrix  : Singles.Matrix4 := Singles.Identity4;
        Offset_Matrices        : Matrix4_List;
        Current_Bone_Matrices  : Matrix4_List;
        Anim_Node_Parents      : Matrix4_List;
        Anim_Node_Children     : Node_Children_Array := (others => (others => 0));
        Anim_Node_Num_Children : Int_Array (1 .. Max_Bones) := (others => 0);
        Anim_Node_Bone_Ids     : GL_Maths.Ints_List;
        Bone_Count             : Integer := 0;
        -- animations using the skeleton
        Animations             : Animations_List;
        Animation_Count        : Integer := 0;
        Points_Vbo             : GL.Objects.Buffers.Buffer;
        Normals_Vbo            : GL.Objects.Buffers.Buffer;
        Texcoords_Vbo          : GL.Objects.Buffers.Buffer;
        Bone_Ids_Vbo           : GL.Objects.Buffers.Buffer;
        Vtans_Vbo              : GL.Objects.Buffers.Buffer;
    end record;

    package Meshes_Package is new Ada.Containers.Vectors (Positive, Mesh);
    type Mesh_List is new Meshes_Package.Vector with null record;

    Loaded_Mesh_Count    : Integer := 0;
    Allocated_Mesh_Count : Integer := 128;
    Loaded_Meshes        : Mesh_List;

    --  ------------------------------------------------------------------------

    procedure Init is
    begin
        Loaded_Meshes.Clear;
        Loaded_Mesh_Count := 0;
        Allocated_Mesh_Count := 0;
    end Init;

    --  ------------------------------------------------------------------------

    procedure Load_Data (Input_File : File_Type; Vec : in out Vector3) is
        use Ada.Strings;
        Value : constant String := Get_Line (Input_File);
        Pos1  : constant Natural := Fixed.Index (Value, " ");
        Pos2  : Natural;
    begin
        Vec (GL.X) :=
          Single'Value (Value (1 .. Pos1 - 1));
        Pos2 :=
          Fixed.Index (Value (Pos1 .. Value'Last), " ");
        Vec (GL.Y) :=
          Single'Value (Value (Pos1 + 1 .. Pos2 - 1));
        Vec (GL.Z) :=
          Single'Value (Value (Pos2 + 1 .. Value'Last));
    end Load_Data;

    --  ------------------------------------------------------------------------

    function Load_Managed_Mesh (Mesh : String; Has_Vp, Has_Vn, Has_Vt,
                                Has_Vtangents, Has_bones : Boolean := False)
                                return Integer is
    begin
        return 0;
    end Load_Managed_Mesh;

    --  ------------------------------------------------------------------------

    function Load_Mesh_Data_Only (File_Name : String;
                                  Points : in out GL_Maths.Vector3_List;
                                  Tex_Coords : in out GL_Maths.Vector2_List;
                                  Normals : in out GL_Maths.Vector3_List;
                                  Point_Count : in out Integer)
                                  return Boolean is
        use Ada.Strings;
        Input_File : File_Type;
        Comps      : Integer := 0;
        Vec3       : Vector3;
    begin
        Point_Count := 0;
        Game_Utils.Game_Log ("Loaded_Mesh_Data_Only loading mesh data from: " &
                               File_Name);
        Open (Input_File, In_File, File_Name);
        while not End_Of_File (Input_File) loop
            declare
                aString    : constant String := Get_Line (Input_File);
            begin
                if aString (1 .. 1) = "@" then
                    if aString (2 .. 9) = "Anton's " then
                        null;
                    elsif aString (2 .. 12) = "vert_count " then
                        Point_Count := Integer'Value (aString (13 .. aString'Last));
                    elsif aString (2 .. 10) = "vp comps " then
                        Comps  := Integer'Value (aString (11 .. aString'Last));
                        if Comps * Point_Count > 0 then
                            for index in 1 .. Comps * Point_Count loop
                                Load_Data (Input_File, Vec3);
                                Points.Append (Vec3);
                            end loop;
                        end if;

                    elsif aString (2 .. 4) = "vn " then
                        Comps  := Integer'Value (aString (11 .. aString'Last));
                        if Comps * Point_Count > 0 then
                            for index in 1 .. Comps * Point_Count loop
                                Load_Data (Input_File, Vec3);
                                Normals.Append (Vec3);
                            end loop;
                        end if;

                    elsif aString (2 .. 4) = "vt " then
                        Comps := Integer'Value (aString (11 .. aString'Last));
                        if Comps * Point_Count > 0 then
                            for index in 1 .. Comps * Point_Count loop
                                declare
                                    Value : constant String := Get_Line (Input_File);
                                    Pos1  : constant Natural := Fixed.Index (Value, " ");
                                    Vec2  : Vector2;
                                begin
                                    Vec2 (GL.X) :=
                                      Single'Value (Value (1 .. Pos1 - 1));
                                    Vec2 (GL.Y) :=
                                      Single'Value (Value (Pos1 + 1 .. Value'Last));
                                    Tex_Coords.Append (Vec2);
                                end;  --  declare block
                            end loop;
                        end if;
                    end if;
                end if;
            end;  --  declare block
        end loop;
        Close (Input_File);
        return False;
    end Load_Mesh_Data_Only;

    --  ------------------------------------------------------------------------

    function Loaded_Mesh_VAO (Index : Integer;
                              VAO : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object)
                              return Boolean is
        use Meshes_Package;
        Curs  : Cursor := Loaded_Meshes.First;
        Found : Boolean := False;
    begin
        while Has_Element (Curs) and not Found loop
            Found := To_Index (Curs) = Index;
            if Found then
                VAO := Element (Curs).VAO;
            else
                Next (Curs);
            end if;
        end loop;

        return Found;
    end Loaded_Mesh_VAO;

    --  ------------------------------------------------------------------------

    function Point_Count (Index : Integer) return Integer is
        use Meshes_Package;
        Curs  : Cursor := Loaded_Meshes.First;
        Found : Boolean := False;
        Count : Integer := 0;
    begin
        while Has_Element (Curs) and not Found loop
            Found := To_Index (Curs) = Index;
            if Found then
                Count := Element (Curs).Point_Count;
            else
                Next (Curs);
            end if;
        end loop;

        return Count;
    end Point_Count;

    --  ------------------------------------------------------------------------

end Mesh_Loader;
