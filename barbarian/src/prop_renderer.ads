
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;

with Maths;

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

   procedure Init_Prop_Renderer;
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

end Prop_Renderer;
