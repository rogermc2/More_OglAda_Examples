
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

   Type Prop_Script is record
      -- Mesh/File Stuff
      File_Name             : Unbounded_String := To_Unbounded_String ("");
      Mesh_Index            : Positive;  -- Index Of Mesh In The Mesh Loader
      Outlines_Mesh_Index   : Positive;
      Smashed_Script_Index  : Positive;-- Script To Switch To Once It Has Been Smashed (Changes Prop Type Stuff)


      -- Draw Stuff
      -- ----------
      Vao                   : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      -- Opengl Shadow Mesh Vao
      Shadow_Vao            : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      -- Opengl Outline Mesh Vao
      Outlines_Vao          : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      -- # Vertices In Mesh
      Vertex_Count          : Int := 0;
      -- # Vertices In Outlines Mesh
      Outlines_Vertex_Count : Int := 0;
      -- Radius Of Visibility Sphere - Defined In Mesh File
      Bounding_Radius       : Single := 0.0;

      -- Sprite
      -- ------
      Sprite_Timer          : Float := 0.0;
      Sprite_Y_Offset       : Float := 0.0;
      Sprite_Map_Rows       : Integer := 0;
      Sprite_Map_Cols       : Integer := 0;
      Uses_Sprite           : Boolean := False;

      -- Textures
      -- --------
      Diffuse_Map_Id        : GL.Objects.Textures.Texture;
      Specular_Map_Id       : GL.Objects.Textures.Texture;
      Normal_Map_Id         : GL.Objects.Textures.Texture;
      Uses_Normal_Map       : Boolean := False;

      -- Special Rendering Modes
      -- -----------------------
      Casts_Shadow          : Boolean := False;
      Transparent           : Boolean := False;
      Draw_Outlines         : Boolean := False;

      -- General Stuff
      -- -------------
      Script_Type           : Prop_Renderer_Support.Property_Type;
      Scale                 : Singles.Vector3 := Maths.Vec3_0;

      -- Collision Shape
      -- ---------------
      -- Height Of Bounding Cylinder Or Box
      Height                 : Single := 0.0;
      -- Radius Of Bounding Cylinder Or Sphere
      Radius                 : Single := 0.0;
      -- Define 4 Of These X,Z Points As Alternative To Bounding Cylinder
      Box_Points             : Singles.Vector2_Array (1 .. 4);
      -- Used To Offset Origin Of Bounding Cylinder Shape And Visibility Sphere
      Origin                 : Singles.Vector3 := Maths.Vec3_0;
      Hole_Height            : Single := 0.0;
      Hole_Points            : Singles.Vector2_Array (1 .. 4);
      Has_Hole               : Boolean := False;

      -- Lights Attached To Prop
      -- -----------------------
      Lamp_Offset            : Singles.Vector3 := Maths.Vec3_0;
      Lamp_Diffuse           : Singles.Vector3 := Maths.Vec3_0;
      Lamp_Specular          : Singles.Vector3 := Maths.Vec3_0;
      Lamp_Range             : Float := 0.0;
      Has_Lamp               : Boolean := False;

      -- Particle Emitters Attached To Prop
      -- ----------------------------------
      Particles_Offset       : Singles.Vector3 := Maths.Vec3_0;
      Particle_Script_File_Name : Unbounded_String := To_Unbounded_String ("");
      Has_Particles           : Boolean := False;

      -- Triggers
      Character_Activated     : Boolean := False;
      Npc_Activated           : Boolean := False;
      Trigger_Only_Once       : Boolean := False;
      Hide_After_Triggering   : Boolean := False;

      -- Door Stuff
      Initial_Door_State      : Prop_Renderer_Support.Door_State :=
                                  Prop_Renderer_Support.Closed_State;
      Opening_Time_S          : Float := 0.0;
      Starts_Visible          : Boolean := True;

      -- Elevator Stuff
      Initial_Elevator_State     : Prop_Renderer_Support.Elevator_State;
      Elevator_Up_Duration       : Float := 0.0;
      Elevator_Down_Duration     : Float := 0.0;
      Elevator_Wait_Delay        : Float := 0.0;
      Elevator_Top_Height        : Float := 0.0;
      Elevator_Bottom_Height     : Float := 0.0;
      Elevator_Goes_Back_Up      : Boolean := False;
      Elevator_Goes_Back_Down    : Boolean := False;
      Elevator_Visible_At_Top    : Boolean := False;
      Elevator_Visible_At_Bottom : Boolean := False;
      Starts_At_Bottom           : Boolean := True;

      -- Value In Gold Coins Or Health Points
      Value                    : Integer := 0;
      -- Audio
      Sound_Activate_File_Name : Unbounded_String := To_Unbounded_String ("");
   end record;

   package Prop_Indices_Package is new Ada.Containers.Vectors
     (Positive, Positive);
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
   function Get_Script_Data (Script_Index : Positive) return Prop_Script;
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
