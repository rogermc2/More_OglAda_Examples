
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Maths;

with GL.Objects.Vertex_Arrays;

with Character_Controller;
with Game_Utils;

package body Properties_Manager is

   type Points_Array is array (1 .. 4, 1 .. 2) of GL.Types.Single;

   type Prop_Script is record
      --  Mesh/File stuff
      File_Name             : Unbounded_String := To_Unbounded_String ("");
      --  Index of mesh in the mesh loader
      Mesh_Index            : GL.Types.UInt := 0;
      --  Same for the outlines version of the mesh
      Outlines_Mesh_Index   : GL.Types.UInt := 0;
      --  Script to switch to once it has been smashed (changes prop type stuff)
      Smashed_Script_Index  : Integer := 0;

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
      Diffuse_Map_Id  : GL.Types.Int := 0;
      Specular_Map_Id : GL.Types.Int := 0;
      Normal_Map_Id   : GL.Types.Int := 0;
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
      Lamp_Rang     : Float := 0.0;
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

   package Properties_Package is new Ada.Containers.Vectors
     (Positive, Prop);
   type Properties_List is new Properties_Package.Vector with null record;

   Properties   : Properties_List;
   Portal_Index : Integer := -1;

   procedure Set_Property_Defaults;

   -- -------------------------------------------------------------------------
   --  Height_level is the property's own height offset from the tile.
   --  Facing is the compass facing 'N' 'S' 'W' or 'E'.
   procedure Create_Prop_From_Script (Script_File                : String;
                                      Map_U, Map_V, Height_Level : Integer;
                                      Facing                     : Character; Tx, Rx : Integer) is

   begin
      Game_Utils.Game_Log ("Properties Manager creating property from script "
                           & Script_File);
      Set_Property_Defaults;
   end Create_Prop_From_Script;

   -- -------------------------------------------------------------------------
   --  read properties from an already open file
   procedure Load_Properties (Prop_File : File_Type) is
      use Ada.Strings;
      aLine          : String := Get_Line (Prop_File);
      PosL           : Natural := Fixed.Index (aLine, " ") + 1;
      PosR           : Natural;
      S_Length       : Integer := aLine'Length;
      Property_Count : Integer := 0;
      Script_File    : Unbounded_String;
      U              : Integer := 0;       --  map position
      V              : Integer := 0;
      Height         : Integer := 0;       --  map height level
      Facing         : Character := 'N';   --  compass facing
      Rx             : Integer := -1;      --  receive code
      Tx             : Integer := -1;      --  transmit code
   begin
      if Fixed.Index (aLine, "props ") = 0 then
         raise Properties_Exception with
           "Load_Properties, invalid format, ""props"" expected: " & aLine (1 .. PosL);
      end if;

      PosR := Fixed.Index (aLine (PosL .. S_Length), " ");

      Property_Count := Integer'Value (aLine (PosL .. PosR));
      Game_Utils.Game_Log ("Properties_Manager reading " &
                             Integer'Image (Property_Count) & " properties");
      Portal_Index := -1;
      Character_Controller.Gold_Current := 0;
      Character_Controller.Gold_Max := 0;
      Character_Controller.Total_Treasure_Found := 0;

      for index in 1 .. Property_Count loop
         declare
            Prop_Line : String := Get_Line (Prop_File);
         begin
            S_Length := Prop_Line'Length;
            PosL := Fixed.Index (Prop_Line, " ");
            Script_File := To_Unbounded_String (Prop_Line (1 .. PosL - 1));
            PosR := Fixed.Index (Prop_Line (PosL .. S_Length), ",");
            U := Integer'Value (Prop_Line (PosL + 1 .. PosR - 1));
            PosL := Fixed.Index (Prop_Line (PosR .. S_Length), " ");
            V := Integer'Value (Prop_Line (PosR + 1 .. PosL - 1));

            PosR := Fixed.Index (Prop_Line (PosL + 1 .. S_Length), " ");
            Height := Integer'Value (Prop_Line (PosL + 1 .. PosR - 1));
            PosL := Fixed.Index (Prop_Line (PosR + 1 .. S_Length), " ");
            Facing := Prop_Line (PosR + 1);

            PosR := Fixed.Index (Prop_Line (PosL + 1 .. S_Length), " ");
            Rx := Integer'Value (Prop_Line (PosL + 1 .. PosR - 1));
            Tx := Integer'Value (Prop_Line (PosR + 1 .. S_Length));
            --              Game_Utils.Game_Log ("Properties_Manager Script_File " &
            --                                  To_String (Script_File) & ", U: " &
            --                               Integer'Image (U) & ", V: " &
            --                               Integer'Image (V) & ", Height: " &
            --                               Integer'Image (Height) & ", Facing: " &
            --                               Facing & ", Rx: " &
            --                               Integer'Image (Rx) & ", Tx: " &
            --                               Integer'Image (Tx));
         end; --  declare block
         Create_Prop_From_Script (To_String (Script_File), U, V, Height,
                                  Facing, Tx, Rx);
      end loop;


   exception
      when anError : others =>
         Put_Line ("An exception occurred in Properties_Manager.Load_Properties!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
   end Load_Properties;

   --  ----------------------------------------------------------------------------

   procedure Set_Property_Defaults is

   begin
      null;
   end Set_Property_Defaults;

   -- --------------------------------------------------------------------------

end Properties_Manager;
