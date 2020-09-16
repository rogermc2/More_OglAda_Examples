
with Ada.Streams.Stream_IO; use Ada.Streams;

with GL.Types;

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

    procedure Load_Properties (Input_Stream : Stream_IO.Stream_Access);
--                                 Stream_Index : Stream_IO.Count);

private
    type Prop is record
       --  Prop_Script To Use From Array Of Them (Properties For This Type Of Prop)
	Script_Index : Integer := 0;

	--  Index Of Any Sprite Used In Sprite Renderer
	Sprite_Index : Integer := 0;
	--  If Particles Attached This Is Index In Particle Renderer
	Particle_System_Index : Integer := 0;
	Boulder_Snd_Idx : Integer := 0;

	--  Transmit And Recieve Codes For Events And Traps
	Tx_Code : Integer := 0;
	Rx_Code : Integer := 0;

	--  Tile-Based Position And Facing
	--  2D Tile Position
	Map_U : Integer := 0;
	Map_V : Integer := 0;
	--  In 2M Levels Above Ground
	Height_Level : Integer := 0;
	--  Compass Facing
--  	Char  : Facing;

	--  Position And Facing In Meters And Degrees
	--  -----------------------------------------
	World_Pos        : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
--  	Versor Quat;
	Heading_Deg      : Float := 0.0;
	Vel              : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
	Origin_Wor       : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);

	--  Animation And Rendering
	--  -----------------------
	Model_Mat               : GL.Types.Singles.Matrix4 :=
                                    GL.Types.Singles.Identity4;
--  	Current_Bone_Transforms[Max_Bones];
	Anim_Duration           : Float := 0.0;
	Anim_Elapsed_Time       : Float := 0.0;
	Sprite_Duration         : Float := 0.0;
	Delay_Countdown         : Float := 0.0;
	--  Hack To Stop Decap Head Bouncing When Stuck
	Bounce_Count            : Integer := 0;

	--  Various States
	--  --------------
	Door                    : Door_State;
	Elevator                : Elevator_State;
	Trap                    : Trap_State;
	Was_Triggered           : Boolean := False;
	Is_On_Ground            : Boolean := False;
	No_Save                 : Boolean := False;
	Is_Animating            : Boolean := False;
	Is_Visible              : Boolean := False;
	--  These Two Are For The Pillar As They Are Set At Diff Times Into Animation
	First_Doom_Tile_Set     : Boolean := False;
	Second_Doom_Tile_Set    : Boolean := False;
	--  Gold : Boolean := False
	Was_Collected_By_Player : Boolean := False;
	--  Pot
	Was_Smashed             : Boolean := False;
    end record;

end Properties_Manager;
