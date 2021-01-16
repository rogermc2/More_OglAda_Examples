
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Vertex_Arrays;
with GL.Objects.Textures;
with GL.Types; use GL.Types;

with Maths;

with Event_Controller;
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
   type Prop_Script is private;

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

    type Points_Array is array (1 .. 4, 1 .. 2) of GL.Types.Single;
    type Prop_Script is record
    --  Mesh/File stuff
        File_Name             : Unbounded_String := To_Unbounded_String ("");
        --  Index of mesh in the mesh loader
        Mesh_Index            : Natural := 0;
        --  Same for the outlines version of the mesh
        Outlines_Mesh_Index   : Natural := 0;
        --  Script to switch to once it has been smashed (changes prop type stuff)
        Smashed_Script_Index  : Natural := 0;

        --  Draw stuff
        --  Opengl Vertex Array Object
        Vao                    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        --  Opengl shadow mesh Vao
        Shadow_Vao             : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        --  Opengl outline mesh Vao
        Outlines_Vao           : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        --  # Vertices In Mesh
        Vertex_Count           : Integer := 0;
        --  # Vertices in outlines mesh
        Outlines_Vertex_Count  : Integer := 0;
        --  Radius of visibility sphere - defined in mesh file
        Bounding_Radius        : Float := 0.0;

        --  Sprite
        Sprite_Timer     : float := 0.4;
        Sprite_Y_Offset  : Integer := 0;
        Sprite_Map_Rows  : Integer := 1;
        Sprite_Map_Cols  : Integer := 1;
        Uses_Sprite      : Boolean := False;

        --  Textures
        Diffuse_Map     : GL.Objects.Textures.Texture;
        Specular_Map    : GL.Objects.Textures.Texture;
        Normal_Map      : GL.Objects.Textures.Texture;
        Uses_Normal_Map : Boolean := False;

        --  Special rendering modes
        Casts_Shadow    : Boolean := True;
        Transparent     : Boolean := True;
        Draw_Outlines   : Boolean := True;

        --  General stuff
        --  -------------
        Prop_Kind : Prop_Type;
        Scale     : GL.Types.Singles.Vector3 := (1.0, 1.0, 1.0);

        --  Collision shape
        --  Height Of Bounding Cylinder Or Box
        Height      : Float := 0.0;
        --  Radius Of Bounding Cylinder Or Sphere
        Radius      : Float := 0.0;
        --  Define 4 Of These X,Z Points As Alternative To Bounding Cylinder
        Box_Points  : Points_Array;
        --  Used To Offset Origin Of Bounding Cylinder Shape And Visibility Sphere
        Origin      : GL.Types.Singles.Vector3 := Maths.Vec3_0;
        Hole_Height : Float := 0.0;
        Hole_Points : Points_Array;
        Has_Hole    : Boolean := False;

        --  Lights Attached To Prop
        --  -----------------------
        Lamp_Offset   : GL.Types.Singles.Vector3 := Maths.Vec3_0;
        Lamp_Diffuse  : GL.Types.Singles.Vector3 := Maths.Vec3_0;
        Lamp_Specular : GL.Types.Singles.Vector3 := Maths.Vec3_0;
        Lamp_Range    : Single := 0.0;
        Has_Lamp      : Boolean := False;

        --  Particle emitters attached to prop
        --  ----------------------------------
        Particles_Offset          : GL.Types.Singles.Vector3 := Maths.Vec3_0;
        Particle_Script_File_Name :  Unbounded_String := To_Unbounded_String ("");
        Has_Particles             : Boolean := False;

        --  Triggers
        Character_Activated   : Boolean := True;
        Npc_Activated         : Boolean := True;
        Trigger_Only_Once     : Boolean := False;
        Hide_After_Triggering : Boolean := False;

        --  Door stuff
        Initial_Door_State : Door_State := Closed;
        Opening_Time_S     : Integer := 0;
        Starts_Visible     : Boolean := True;
        Trap               : Trap_State := Trap_Primed;

        --Elevator stuff
        Initial_Elevator_State     : Elevator_State := At_Top;
        Elevator_Up_Duration       : float := 1.0;
        Elevator_Down_Duration     : float := 1.0;
        Elevator_Wait_Delay        : Integer := 0;
        Elevator_Top_Height        : float := 0.0;
        Elevator_Bottom_Height     : float := 0.0;
        Elevator_Goes_Back_Up      : Boolean := False;
        Elevator_Goes_Back_Down    : Boolean := False;
        Elevator_Visible_At_Top    : Boolean := True;
        Elevator_Visible_At_Bottom : Boolean := True;
        Starts_At_Bottom           : Boolean := False;

        --  Value in gold coins or health points
        Value : Integer := 0;

        --  Audio
        Sound_Activate_File_Name :  Unbounded_String := To_Unbounded_String ("");
    end record;  --  Prop_Script

end Properties_Manager;
