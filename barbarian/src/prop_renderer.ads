
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Glfw;

with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;

with Maths;

with Depth_Skinned_Shader_Manager;
with GL_Maths;
with Manifold;
with Mesh_Loader;
with Properties_Manager;
with Prop_Renderer_Support;

package Prop_Renderer is

   package Prop_Indices_Package is new Ada.Containers.Vectors
     (Positive, GL.Types.Int);
   type Prop_Indices_List is new Prop_Indices_Package.Vector with null Record;

   type Props_In_Tiles_Array is array
     (1 .. Manifold.Max_Tile_Cols, 1 .. Manifold.Max_Tile_Cols) of
     Prop_Indices_List;

   Prop_Renderer_Exception : Exception;

   function Activate_Door (Property_Index : Positive) return Boolean;
   function Activate_Door_In_Tile
      (Map_U, Map_V : Int; Hand_Y_World_Pos : Single;
       Activator : Properties_Manager.Activator_Type) return Boolean;
   procedure Init;
   procedure Launch_Decap_Head (LHL_Type  : Positive;
                                World_Pos : Singles.Vector3);
   function Get_Num_Live_Mirrors return Int;
   function Get_Tile_Property_Indices (U, V : Positive) return Prop_Indices_List;
   function Get_Tile_Property_Index (U, V, Index : Positive) return Positive;
--     function Get_Script_Data (Script_Index : Positive)
--                               return Prop_Renderer_Support.Prop_Script;
   function Get_Script_Index (Prop_Index : Positive) return Positive;
   function Props_In_Tiles_Size (U, V : Integer) return Natural;
   procedure Render_Property (Prop_ID : Positive);
   procedure Render_Props_Around_Depth_Only (U, V, Tiles_Distance : Int);
   procedure Render_Props_Around_Split (U, V, Tiles_Distance : Int);
   procedure Reset_Properties;
   procedure Set_Ambient_Light_Level (Level : Singles.Vector3);
    procedure Set_Portal_Index (Index : Natural);
   procedure Splash_Particles_At (Pos : Singles.Vector3);
   function Sq_Dist_To_End_Level_Portal (Pos : Singles.Vector3) return Float;
   procedure Update_Dynamic_Lights (World_Pos, Diff, Specular : Singles.Vector3;
                                    Dist : Single);
   procedure Update_Properties (Seconds : Float);
   procedure Update_Props_In_Tiles_Index (U, V : Integer;
                                          Prop_Index : GL.Types.Int);
   procedure Update_Static_Lights_Uniforms;

private
    use Prop_Renderer_Support;

    Max_Decap_Types            : constant Integer :=  32;
    Max_Active_Decaps_Per_Type : constant Integer := 8;
    Max_Decap_Particles        : constant Integer := 4;
    Max_Mirrors                : constant Integer := 16;

    package Indicies_Package is new Ada.Containers.Vectors (Positive, Positive);
    type Indicies_List is new Indicies_Package.Vector with null Record;

    --  Animation and rendering
    Model_Matrix                  : Singles.Matrix4 := (others => (others => 0.0));
    Current_Bone_Transforms       : Singles.Matrix4_Array
      (1 .. Mesh_Loader.Max_Bones)  := (others => Singles.Identity4);
    Anim_Duration                 : Integer := 0;
    Anim_Elapsed_Time             : Integer := 0;
    Sprite_Duration               : Integer := 0;
    Delay_Countdown               : Integer := 0;
    --  Hack to stop decap head bouncing when stuck
    Bounce_Count                  : Integer := 0;
--      Scripts                       : Script_List;
    Active_Properties_A           : Indicies_List;
    Active_Properties_B           : Indicies_List;
    Curr_Active_Props_A           : Boolean := True;
    Basic_Render_List             : Indicies_List;
    Skinned_Render_List           : Indicies_List;
    Jav_Stand_Render_List         : Indicies_List;
    Portal_Render_List            : Indicies_List;
    Treasure_Render_List          : Indicies_List;
    Last_Head_Launched            : GL_Maths.Integer_Array (1 .. Max_Decap_Types);
    Props_In_Tiles                : Props_In_Tiles_Array;
    Head_Particles                : GL_Maths.Integer_Array (1 .. Max_Decap_Particles);
    Decap_Heads_Prop_Index        : constant array
      (1 .. Max_Decap_Types, 1 .. Max_Active_Decaps_Per_Type) of Integer
      := (others => (others => 0));
    Dust_Particles                : Integer := -1;
    Dust_Particlesb               : Integer := -1;
    Dust_Particlesc               : Integer := -1;
    Pot_Particles                 : Integer := -1;
    Mirror_Particles              : Integer := -1;
    Splash_Particles              : Integer := -1;

    --     Mirror_Indices              : array (1 .. Max_Mirrors) of Positive;
    Prop_Count                    : Natural := 0;
    Mirror_Count                  : Int := 0;
    Live_Mirror_Count             : Int := 0;
    Num_Types_Decap_Heads         : Int := 0;
    Last_Head_Particles_Used      : Integer := 0;
    Prop_Dyn_Light_Pos_Wor        : Singles.Vector3 := Maths.Vec3_0;
    Prop_Dyn_Light_Diff           : Singles.Vector3 := Maths.Vec3_0;
    Prop_Dyn_Light_Spec           : Singles.Vector3 := Maths.Vec3_0;
    Prop_Dyn_Light_Range          : Single := 1.0;
    Prop_Dyn_Light_Dirty          : Boolean := True;
    Prev_Time                     : Single := Single (Glfw.Time);

end Prop_Renderer;
