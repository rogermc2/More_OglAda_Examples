
with GL.Types; use GL.Types;

with Maths;

with Depth_Skinned_Shader_Manager;

package Prop_Renderer_Support is

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
      Quat                : Maths.Single_Quaternion.Quaternion;  --  Versor
      Heading_Deg         : Maths.Degree := 0.0;
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

   procedure Set_Shaders (Property : Property_Data; Prop_Type : Property_Type;
                          Gold_Current : Integer);

end Prop_Renderer_Support;
