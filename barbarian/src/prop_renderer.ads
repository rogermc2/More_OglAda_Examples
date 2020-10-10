
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;

with Maths;

with Depth_Skinned_Shader_Manager;
with Mesh_Loader;

package Prop_Renderer is

   type Property_Type is (Generic_Prop_Prop, Boulder_Prop, Door_Prop,
                          Dart_Trap_Prop, Touch_Plate_Prop, Treasure_Prop,
                          Portal_Prop, Bridge_Prop, Pillar_Prop,
                          Decap_Head_Prop, Box_Prop, Mirror_Prop, Tavern_Prop,
                          Jav_Stand_Prop, Elevator_Prop, Diamond_Trigger_Prop,
                          Hammer_Prop, Anim_Loop_Prop, Food_Prop,
                          Windlass_Prop, Big_Box_Prop, End_Camera_Prop,
                          Pot_Prop);
   type Door_State is (Open_State, Closed_State,
                       Opening_State, Closing_State);

   type Elevator_State is (At_Top_State, At_Bottom_State, Going_Down_State,
                           Going_Up_State, Waiting_To_Go_Up_State,
                           Waiting_To_Go_Down_State);

   type Trap_State is (Trap_Primed_State, Trap_Reloading_State);

   type Activator_Type is (Prop_Activator_Player_State, Prop_Activator_Npc_State,
                           Prop_Activator_Prop_State);

   Prop_Renderer_Exception : Exception;

   procedure Init;
   function Update_Props (Seconds : Float) return Boolean;

private
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
      Vertex_Count          : Integer := 0;
      -- # Vertices In Outlines Mesh
      Outlines_Vertex_Count : Integer := 0;
      -- Radius Of Visibility Sphere - Defined In Mesh File
      Bounding_Radius       : Float := 0.0;

      -- Sprite
      -- ------
      Sprite_Timer          : Float := 0.0;
      Sprite_Y_Offset       : Float := 0.0;
      Sprite_Map_Rows       : Integer := 0;
      Sprite_Map_Cols       : Integer := 0;
      Uses_Sprite           : Boolean := False;

      -- Textures
      -- --------
      Diffuse_Map_Id        : Int := 0;
      Specular_Map_Id       : Int := 0;
      Normal_Map_Id         : Int := 0;
      Uses_Normal_Map       : Boolean := False;

      -- Special Rendering Modes
      -- -----------------------
      Casts_Shadow          : Boolean := False;
      Transparent           : Boolean := False;
      Draw_Outlines         : Boolean := False;

      -- General Stuff
      -- -------------
      Prop_Type             : Property_Type;
      Scale                 : Singles.Vector3 := Maths.Vec3_0;

      -- Collision Shape
      -- ---------------
      -- Height Of Bounding Cylinder Or Box
      Height                 : Float := 0.0;
      -- Radius Of Bounding Cylinder Or Sphere
      Radius                 : Float := 0.0;
      -- Define 4 Of These X,Z Points As Alternative To Bounding Cylinder
      Box_Points             : Singles.Vector2_Array (1 .. 4);
      -- Used To Offset Origin Of Bounding Cylinder Shape And Visibility Sphere
      Origin                 : Singles.Vector3 := Maths.Vec3_0;
      Hole_Height            : Float := 0.0;
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
      Initial_Door_State      : Door_State := Closed_State;
      Opening_Time_S          : Float := 0.0;
      Starts_Visible          : Boolean := True;

      -- Elevator Stuff
      Initial_Elevator_State     : Elevator_State;
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

   Type Property_Data Is Record
      -- Prop_Script To Use From Array Of Them (Properties For This Type Of Prop)
      Script_Index          : Positive := 1;

      -- Index Of Any Sprite Used In Sprite Renderer
      Sprite_Index          : Positive := 1;
      -- If Particles Attached This Is Index In Particle Renderer
      Particle_System_Index : Positive := 1;
      Boulder_Snd_Idx       : Positive := 1;

      -- Transmit And Recieve Codes For Events And Traps
      -- -----------------------------------------------
      Tx_Code                : Integer := 0;
      Rx_Code                : Integer := 0;

      -- Tile-Based Position And Facing
      -- ------------------------------
      -- 2D Tile Position
      Map_U                : Int := 0;
      Map_V                : Int := 0;
      -- In 2M Levels Above Ground
      Height_Level         : Integer := 0;
      -- Compass Facing
      Facing               : Character := 'N';

      -- Position And Facing In Meters And Degrees
      -- -----------------------------------------
      World_Pos           : Singles.Vector3 := Maths.Vec3_0;
      Quat                : Singles.Vector4 := Maths.Vec4_0;  --  Versor
      Heading_Deg         : Float := 0.0;
      Velocity            : Singles.Vector3 := Maths.Vec3_0;
      Origin_World        : Singles.Vector3 := Maths.Vec3_0;

      -- Animation And Rendering
      -- -----------------------
      Model_Mat               : Singles.Matrix4 := Singles.Identity4;
      Current_Bone_Transforms : Depth_Skinned_Shader_Manager.Bone_Matrices_Array
        := (others => Singles.Identity4);
      Anim_Duration           : Float := 0.0;
      Anim_Elapsed_Time       : Float := 0.0;
      Sprite_Duration         : Float := 0.0;
      Delay_Countdown         : Float := 0.0;
      -- Hack To Stop Decap Head Bouncing When Stuck
      Bounce_Count            : Integer := 0;

      -- Various States
      -- --------------
      Door                    : Door_State := Closed_State;
      Elevator                : Elevator_State := At_Bottom_State;
      Trap                    : Trap_State := Trap_Primed_State;
      Was_Triggered           : Boolean := False;
      Is_On_Ground            : Boolean := True;
      No_Save                 : Boolean := False;
      Is_Animating            : Boolean := False;
      Is_Visible              : Boolean := False;
      -- These Two Are For The Pillar As They Are Set At Diff Times Into Animation
      First_Doom_Tile_Set     : Boolean := False;
      Second_Doom_Tile_Set    : Boolean := False;
      -- Gold
      Was_Collected_By_Player : Boolean := False;
      -- Pot
      Was_Smashed             : Boolean := False;
   End Record;

end Prop_Renderer;
