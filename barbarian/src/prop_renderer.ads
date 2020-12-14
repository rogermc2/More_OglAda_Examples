
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;

with Maths;

with Depth_Skinned_Shader_Manager;
with Manifold;
with Mesh_Loader;
with Prop_Renderer_Support;

package Prop_Renderer is

   type Activator_Type is (Prop_Activator_Player_State, Prop_Activator_Npc_State,
                           Prop_Activator_Prop_State);

   package Prop_Indices_Package is new Ada.Containers.Vectors
     (Positive, GL.Types.Int);
   type Prop_Indices_List is new Prop_Indices_Package.Vector with null Record;

   type Props_In_Tiles_Array is array
     (1 .. Manifold.Max_Tile_Cols, 1 .. Manifold.Max_Tile_Cols) of
     Prop_Indices_List;

   Prop_Renderer_Exception : Exception;

   procedure Activate_Door (Property_Index : Positive);
   procedure Delete_Script_Data (Script_Index : Positive);
   procedure Init;
   procedure Launch_Decap_Head (LHL_Type  : Positive;
                                World_Pos : Singles.Vector3);
   function Get_Num_Live_Mirrors return Int;
   function Get_Property_Indices (U, V : Positive) return Prop_Indices_List;
   function Get_Property_Index (U, V, Index : Positive) return Positive;
   function Get_Property_Data (Prop_Index : Positive)
                               return Prop_Renderer_Support.Property_Data;
   function Get_Script_Data (Script_Index : Positive)
                             return Prop_Renderer_Support.Prop_Script;
   function Get_Script_Index (Prop_Index : Positive) return Positive;
   procedure Render_Property (Prop_ID : Positive);
   procedure Render_Props_Around_Depth_Only (U, V, Tiles_Distance : Int);
   procedure Render_Props_Around_Split (U, V, Tiles_Distance : Int);
   procedure Reset_Properties;
   procedure Set_Ambient_Light_Level (Level : Singles.Vector3);
   procedure Splash_Particles_At (Pos : Singles.Vector3);
   procedure Update_Properties (Seconds : Float);
   procedure Update_Static_Lights_Uniforms;

end Prop_Renderer;
