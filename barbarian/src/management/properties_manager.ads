
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types; use GL.Types;

with Maths;

with Mesh_Loader;

package Properties_Manager is

   type Prop_Type is (Generic_Prop, Boulder, Door, Dart_Trap, Touch_Plate,
                      Treasure, Portal, Bridge, Pillar, Decap_Head, Box,
                      Mirror, Tavern, Jav_Stand, Elevator, Hammer, Anim_Loop,
                      Food, Windlass, Big_Box, End_Camera, Pot);
   type Door_State is (Open, Closed, Opening, Closing);
   type Elevator_State is (At_Top, At_Bottom, Going_Down, Going_Up,
                           Waiting_To_Go_Up, Waiting_To_Go_Down);

   type Trap_State is (Trap_Primed, Trap_Reloading);

   type Activator_Type is (Prop_Activator_Player, Prop_Activator_Npc,
                           Prop_Activator_Prop);

   type Prop is private;

   Properties_Exception : Exception;

   procedure Load_Properties (Prop_File : File_Type);

private

   type Prop is record
      --  Prop_Script To Use From Array Of Them (Properties For This Type Of Prop)
      Script_Index : Integer := -1;

      --  Index Of Any Sprite Used In Sprite Renderer
      Sprite_Index          : Integer := 0;
      --  If Particles Attached This Is Index In Particle Renderer
      Particle_System_Index : Integer := 0;
      Boulder_Snd_Idx       : Integer := 0;

      --  Transmit And Recieve Codes For Events And Traps
      Tx_Code : Integer := -1;
      Rx_Code : Integer := -1;

      --  Tile-Based Position And Facing
      --  2D Tile Position
      Map_U        : Integer := 0;
      Map_V        : Integer := 0;
      --  In 2M Levels Above Ground
      Height_Level : Integer := 0;
      --  Compass Facing
      Facing       : Character := 'N';

      --  Position And Facing In Meters And Degrees
      --  -----------------------------------------
      World_Pos        : Singles.Vector3 := (0.0, 0.0, 0.0);
      Quat             : Maths.Single_Quaternion.Quaternion :=
                           Maths.New_Quaternion (0.0, (0.0, 1.0, 0.0));   --  Versor;
      Heading_Deg      : Maths.Degree := 0.0;
      Vel              : Singles.Vector3 := (0.0, 0.0, 0.0);
      Origin_Wor       : Singles.Vector3 := (0.0, 0.0, 0.0);

      --  Animation And Rendering
      --  -----------------------
      Model_Mat                    : Singles.Matrix4 := Singles.Identity4;
      Current_Bone_Transforms      : Singles.Matrix4_Array
        (1 .. Mesh_Loader.Max_Bones) := (others => Singles.Identity4);
      Anim_Duration                : Float := 0.0;
      Anim_Elapsed_Time            : Float := 0.0;
      Sprite_Duration              : Float := 0.0;
      Delay_Countdown              : Float := 0.0;
      --  Hack To Stop Decap Head Bouncing When Stuck
      Bounce_Count                 : Integer := 0;

      --  Various States
      --  --------------
      Door                    : Door_State := Closed;
      Elevator                : Elevator_State := At_Top;
      Trap                    : Trap_State := Trap_Primed;
      Was_Triggered           : Boolean := False;
      Is_On_Ground            : Boolean := False;
      No_Save                 : Boolean := False;
      Is_Animating            : Boolean := False;
      Is_Visible              : Boolean := True;
      --  These Two Are For The Pillar As They Are Set At Diff Times Into Animation
      First_Doom_Tile_Set     : Boolean := False;
      Second_Doom_Tile_Set    : Boolean := False;
      --  Gold : Boolean := False
      Was_Collected_By_Player : Boolean := False;
      --  Pot
      Was_Smashed             : Boolean := False;
   end record;

end Properties_Manager;
