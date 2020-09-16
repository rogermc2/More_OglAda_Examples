
--  with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

--  with Maths;

--  with GL.Objects.Vertex_Arrays;

package body Properties_Manager is

--      type Points_Array is array (1 .. 4, 1 .. 2) of GL.Types.Single;

--      type Prop_Script is record
--      --  Mesh/File stuff
--          File_Name : Unbounded_String := To_Unbounded_String ("");
--          --  Index of mesh in the mesh loader
--          Mesh_Index  : GL.Types.UInt := 0;
--          --  Same for the outlines version of the mesh
--          Outlines_Mesh_Index  : GL.Types.UInt := 0;
--          --  Script to switch to once it has been smashed (changes prop type stuff)
--          Smashed_Script_Index  : Integer := 0;
--
--          --  Draw stuff
--          --  Opengl Vertex Array Object
--          Vao : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
--          --  Opengl shadow mesh Vao
--          Shadow_Vao : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
--          --  Opengl outline mesh Vao
--          Outlines_Vao : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
--          --  # Vertices In Mesh
--          Vertex_Count  : Integer := 0;
--          --  # Vertices in outlines mesh
--          Outlines_Vertex_Count  : Integer := 0;
--          --  Radius of visibility sphere - defined in mesh file
--          Bounding_Radius  : Float := 0.0;
--
--          --  Sprite
--          Sprite_Timer  : Integer := 0;
--          Sprite_Y_Offset : Integer := 0;
--          Sprite_Map_Rows  : Integer := 0;
--          Sprite_Map_Cols  : Integer := 0;
--          Uses_Sprite : Boolean := False;
--
--          --  Textures
--          Diffuse_Map_Id : GL.Types.Int := 0;
--          Specular_Map_Id : GL.Types.Int := 0;
--          Normal_Map_Id : GL.Types.Int := 0;
--          Uses_Normal_Map : Boolean := False;
--
--          --  Special rendering modes
--          Casts_Shadow : Boolean := False;
--          Transparent : Boolean := False;
--          Draw_Outlines : Boolean := False;
--
--          --  General stuff
--          --  -------------
--          Prop_Kind : Prop_Type;
--          Scale : GL.Types.Singles.Vector3 := Maths.Vec3_0;
--
--          --  Collision shape
--          --  Height Of Bounding Cylinder Or Box
--          Height : Float := 0.0;
--          --  Radius Of Bounding Cylinder Or Sphere
--          Radius : Float := 0.0;
--          --  Define 4 Of These X,Z Points As Alternative To Bounding Cylinder
--          Box_Points : Points_Array;
--          --  Used To Offset Origin Of Bounding Cylinder Shape And Visibility Sphere
--          Origin : GL.Types.Singles.Vector3 := Maths.Vec3_0;
--          Hole_Height : Float := 0.0;
--          Hole_Points : Points_Array;
--          Has_Hole : Boolean := False;
--
--          --  Lights Attached To Prop
--          --  -----------------------
--          Lamp_Offset : GL.Types.Singles.Vector3 := Maths.Vec3_0;
--          Lamp_Diffuse : GL.Types.Singles.Vector3 := Maths.Vec3_0;
--          Lamp_Specular : GL.Types.Singles.Vector3 := Maths.Vec3_0;
--          Lamp_Rang : Float := 0.0;
--          Has_Lamp : Boolean := False;
--
--          --  Particle emitters attached to prop
--          --  ----------------------------------
--          Particles_Offset : GL.Types.Singles.Vector3 := Maths.Vec3_0;
--          Particle_Script_File_Name :  Unbounded_String := To_Unbounded_String ("");
--          Has_Particles : Boolean := False;
--
--          --  Triggers
--          Character_Activated : Boolean := False;
--          Npc_Activated : Boolean := False;
--          Trigger_Only_Once : Boolean := False;
--          Hide_After_Triggering : Boolean := False;
--
--          --  Door stuff
--          Initial_Door_State : Door_State;
--          Opening_Time_S : Integer := 0;
--          Starts_Visible : Boolean := False;
--
--          --Elevator stuff
--          Initial_Elevator_State : Elevator_State;
--          Elevator_Up_Duration : Integer := 0;
--          Elevator_Down_Duration : Integer := 0;
--          Elevator_Wait_Delay : Integer := 0;
--          Elevator_Top_Height : float := 0.0;
--          Elevator_Bottom_Height : float := 0.0;
--          Elevator_Goes_Back_Up : Boolean := False;
--          Elevator_Goes_Back_Down : Boolean := False;
--          Elevator_Visible_At_Top : Boolean := False;
--          Elevator_Visible_At_Bottom : Boolean := False;
--          Starts_At_Bottom : Boolean := False;
--
--          --  Value in gold coins or health points
--          Value : Integer := 0;
--
--          --  Audio
--          Sound_Activate_File_Name :  Unbounded_String := To_Unbounded_String ("");
--      end record;
--
--      package Prop_Scripts_Package is new Ada.Containers.Vectors
--        (Positive, Unbounded_String);
--      type Prop_Scripts_List is new Prop_Scripts_Package.Vector with null record;

    -- -------------------------------------------------------------------------
    --  read properties from an already open file stream
    procedure Load_Properties (Input_Stream : Stream_IO.Stream_Access) is
--                                 Stream_Index : Stream_IO.Count) is
        aLine            : Unbounded_String;
    begin
        Unbounded_String'Read (Input_Stream, aLine);
        declare
--              use Stream_IO;
            aString        : constant String := To_String (aLine);
            Pos            : constant Natural := Index (aLine, " ");
            Pos_M1         : constant Natural := Pos - 1;
--              Val_Pos        : constant Stream_IO.Count :=
--                                 Stream_Index + Stream_IO.Count (Pos_P1);
--              S_Length       : constant Integer := aString'Length;
--              Property_Count : GL.Types.Int := 0;
        begin
            if aString (1 .. Pos_M1) = "props" then
                null;
--                  Property_Count := GL.Types.Int'Value (aString (Pos_P1 .. S_Length));
            else
                raise Properties_Exception with
                  "Properties_Manager.Load_Properties, invalid property: " &
                  aString (1 .. Pos_M1);
            end if;

            --              if aString (1 .. Pos_M1) = "max_initial_velocity" then
            --                  Set_Index (Input_File, Val_Pos);
            --                  Singles.Vector3'Read (Input_Stream, Max_Velocity); );
            --              end if;
        end;  --  declare block

        --          Unbounded_String'Read (Input_Stream, theMap.Music_Track);
        --          Unbounded_String'Read (Input_Stream, theMap.Hammer_Music_Track);

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Properties_Manager.Load_Properties!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Load_Properties;

    --  ----------------------------------------------------------------------------

end Properties_Manager;
