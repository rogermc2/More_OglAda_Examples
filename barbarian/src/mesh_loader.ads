
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;

with GL_Maths;
with GL_Utils;

package Mesh_Loader is

   type Animation is private;
   type Mesh_List is private;
   type Mesh is private;

   Max_Bones             : constant  Int := 32;
   Mesh_Loader_Exception : Exception;

   function Animation_Duration (Mesh_ID : Integer; Anim_ID : Positive)
                                 return Float;
   function Bone_Count (Index : Integer) return Integer;
   procedure Init;
   function Loaded_Mesh  (Index : Positive) return Mesh;
   function Loaded_Meshes return Mesh_List;
   function Loaded_Mesh_Animation
     (Mesh_ID,  Animation_ID : Integer; theAnimation : in out Animation) return Boolean;
   function Load_Managed_Mesh (Mesh_Name               : String;
                               Has_Vp, Has_Vn, Has_Vt,
                               Has_tangents, Has_Bones : Boolean := False)
                                return Integer;
   function Load_Mesh_Data_Only (File_Name   : String;
                                 Points      : in out GL_Maths.Vec3_List;
                                 Tex_Coords  : in out GL_Maths.Vec2_List;
                                 Normals     : in out GL_Maths.Vec3_List)
                                  return Boolean;
   function Loaded_Mesh_Bounding_Radius
     (Mesh_ID : Positive; Radius : in out Float) return Boolean;
   function Loaded_Mesh_Shadow_VAO (Mesh_ID : Integer;
                                    VAO     : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object)
                                     return Boolean;
   function Loaded_Mesh_Vertex_Count (Mesh_ID : Integer; Count : in out Natural)
                                       return Boolean;
   function Loaded_Mesh_VAO (Mesh_ID : Integer; VAO : in out
                               GL.Objects.Vertex_Arrays.Vertex_Array_Object)
                              return Boolean;
   function Point_Count (Index : Integer) return Integer;
   procedure Recurse_Animation_Tree
     (aMesh        : in out Mesh; Anim : in out Animation; Anim_Time : Float;
      My_Anim_Node : Integer; Parent_Matrix : Singles.Matrix4);

private

   type Node_Children_Array is array (1 .. Max_Bones, 1 .. Max_Bones) of Integer;

   type Tra_Anim_Key is record
      Tra  : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
      Time : Float := 0.0;
   end record;

   type Sca_Anim_Key is record
      Sca  : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
      Time : Float := 0.0;
   end record;

   type Rot_Anim_Key is record
      Rot  : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
      Time : Float := 0.0;
   end record;

   --  Channel_Data defines a channel of keys within an animation.
   --  All the keys in anim for a particular animation node,
   --  where an 'animation node' is a bone in the skeleton but doesn't
   --  necessarily have any vertices weighted to it
   --  i.e. can be an in-between joint
   type Channel_Data is record
      Tra_Keys       : Tra_Anim_Key;
      Tra_Keys_Count : Integer := 0;
      Sca_Keys       : Sca_Anim_Key;
      Sca_Keys_Count : Integer := 0;
      Rot_Keys       : Rot_Anim_Key;
      Rot_Keys_Count : Integer := 0;
   end record;

   package Channels_Package is new Ada.Containers.Vectors (Positive, Channel_Data);
   type Channel_List is new Channels_Package.Vector with null record;

   type Animation is record
      Name     : Unbounded_String := To_Unbounded_String ("");
      Duration : Float := 0.0;
      --  Order of channels corresponds to anim nodes in hierarchy
      Channels : Channel_List;
   end record;

   package Animations_Package is new Ada.Containers.Vectors (Positive, Animation);
   type Animations_List is new Animations_Package.Vector with null record;

   type Mesh is record
      File_Name              : Unbounded_String := To_Unbounded_String ("");
      VAO                    : GL.Objects.Vertex_Arrays.Vertex_Array_Object :=
                                 GL.Objects.Vertex_Arrays.Null_Array_Object;
      Shadow_VAO             : GL.Objects.Vertex_Arrays.Vertex_Array_Object :=
                                 GL.Objects.Vertex_Arrays.Null_Array_Object;
      Point_Count            : Integer := 0;
      Vp_Count               : Integer := 0;
      Vn_Count               : Integer := 0;
      Vt_Count               : Integer := 0;
      Vtan_Count             : Integer := 0;
      Vb_Count               : Integer := 0;
      Bounding_Radius        : Float := 1.0;
      --  the skeleton hierarchy
      Root_Transform_Matrix  : Singles.Matrix4 := Singles.Identity4;
      Offset_Matrices        : GL_Utils.Matrix4_List;
      Current_Bone_Matrices  : GL_Utils.Matrix4_List;
      Anim_Node_Parents      : GL_Utils.Matrix4_List;
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
      Bones_Vbo              : GL.Objects.Buffers.Buffer;
      Vtans_Vbo              : GL.Objects.Buffers.Buffer;
   end record;

   package Meshes_Package is new Ada.Containers.Vectors (Positive, Mesh);
   type Mesh_List is new Meshes_Package.Vector with null record;

end Mesh_Loader;
