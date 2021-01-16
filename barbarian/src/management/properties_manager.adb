
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Maths;

with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;

with Audio;
with Batch_Manager;
with Character_Controller;
with Event_Controller;
with Game_Utils;
with Particle_System;
with Prop_Renderer;
with Prop_Renderer_Support;
with Sprite_Renderer;
with Tiles_Manager;

package body Properties_Manager is

    Max_Mirrors     : constant Integer := 16;
    Sprite_Y_Offset : constant Single := 0.125;

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

    package Properties_Package is new Ada.Containers.Vectors
      (Positive, Prop);
    type Properties_List is new Properties_Package.Vector with null record;

    package Properties_Script_Package is new Ada.Containers.Vectors
      (Positive, Prop_Script);
    type Properties_Script_List is new Properties_Script_Package.Vector with null record;

    Pillar_Bridge_Script_File : constant String := "pillar_bridge.script";
    Properties                : Properties_List;
    Prop_Scripts              : Properties_Script_List;
    Portal_Index              : Natural := 0;
    Pillar_Bridge_SI          : Natural := 0;
    Mirror_Indices            : array (1 .. Max_Mirrors) of Natural :=
                                  (others => 0);
    Mirror_Count              : Natural := 0;
    Live_Mirror_Count         : Natural := 0;
    End_Camera_Matrix         : Singles.Matrix4 := Singles.Identity4;
    End_Camera_Position       : Singles.Vector3 := Maths.Vec3_0;

    function Get_Index_Of_Prop_Script (Script_File : String) return Positive;
    function Load_Property_Script (File_Name : String; Index : out Positive)
                                   return Boolean;
    procedure Process_Script_Type (New_Props : in out Prop;
                                   aScript : Prop_Script;
                                   Rx_Kind  : in out Event_Controller.RX_Type;
                                   Rebalance  : in out Boolean);
    procedure Rebalance_Props_In (Map_U, Map_V : Integer);
    procedure Set_Up_Sprite (New_Props : in out Prop; aScript : Prop_Script);

    -- -------------------------------------------------------------------------

    function Do_Mesh (Mesh_Data : String; aScript : in out Prop_Script)
                      return Boolean is
        Full_Path    : Unbounded_String;
        Mesh_Index   : Positive;
        Managed_Mesh : Mesh_Loader.Mesh;
        Ok           : Boolean := False;
    begin
        Full_Path := To_Unbounded_String ("src/meshes/" & Mesh_Data);
        aScript.Mesh_Index := Mesh_Loader.Load_Managed_Mesh
          (To_String (Full_Path), True, True, True, True, True);
        Mesh_Index := aScript.Mesh_Index;
        Managed_Mesh := Mesh_Loader.Loaded_Mesh (Mesh_Index);
        OK := Mesh_Loader.Loaded_Mesh_VAO (Mesh_Index, aScript.Vao);
        if not OK then
            Put_Line ("Properties_Manager.Do_Mesh, failed to load VAO for "
                      & Mesh_Data);
        end if;
        if not Mesh_Loader.Loaded_Mesh_Bounding_Radius
          (Mesh_Index, aScript.Bounding_Radius) then
            Put_Line ("Properties_Manager.Do_Mesh, failed to load Bounding_Radius for "
                      & Mesh_Data);
            OK := False;
        end if;
        if not Mesh_Loader.Loaded_Mesh_Shadow_VAO
          (Mesh_Index, aScript.Shadow_Vao) then
            Put_Line ("Properties_Manager.Do_Mesh, failed to load Shadow_VAO for "
                      & Mesh_Data);
            OK := False;
        end if;
        if not Mesh_Loader.Loaded_Mesh_Vertex_Count
          (Mesh_Index, aScript.Vertex_Count) then
            Put_Line ("Properties_Manager.Do_Mesh, failed to load Vertex_Count for "
                      & Mesh_Data);
            OK := False;
        end if;
        if not OK then
            Put_Line ("Properties_Manager.Do_Mesh, failed to load "
                      & Mesh_Data);
        end if;
        return OK;
    end Do_Mesh;

    --  ----------------------------------------------------------------------------
    --  Height_level is the property's own height offset from the tile.
    --  Facing is the compass facing 'N' 'S' 'W' or 'E'.
    procedure Create_Prop_From_Script (Script_File : String;
                                       Map_U, Map_V : Int; Height_Level : Integer;
                                       Facing       : Character; Tx, Rx : Integer) is
        use Maths;
        use Singles;
        use Event_Controller;
        use Properties_Script_Package;
        New_Props     : Prop;
        Script_Index  : constant Positive := Get_Index_Of_Prop_Script (Script_File);
        aScript       : constant Prop_Script := Prop_Scripts.Element (Script_Index);
        Script_Type   : constant Prop_Type := aScript.Prop_Kind;
        Respect_Ramps : constant Boolean := Script_Type = Boulder;
        Start_Now     : Boolean := True;
        Always_Update : Boolean := False;
        Always_Draw   : Boolean := False;
        Rebalance     : Boolean := False;
        RX_Kind       : RX_Type := Rx_Invalid;
        Rot_Matrix    : Matrix4;
        Ros           : Vector3;
    begin
        Game_Utils.Game_Log ("Properties Manager Create_Prop_From_Script creating property from script index"
                             & Integer'Image (Script_Index));
        Game_Utils.Game_Log ("Properties Manager Create_Prop_From_Script Mesh_Index"
                             & Integer'Image (aScript.Mesh_Index));
        if Tiles_Manager.Is_Tile_Valid (Map_U, Map_V) then
            --           Game_Utils.Game_Log ("Properties Manager creating property from script "
            --                                & Script_File);
            New_Props.Script_Index := Script_Index;
            --        Set_Property_Defaults;   set by record defaults
            New_Props.Door := Closed;
            New_Props.Trap := Trap_Primed;
            for index in 1 .. Mesh_Loader.Max_Bones loop
                New_Props.Current_Bone_Transforms (index) := Singles.Identity4;
            end loop;

            New_Props.World_Pos (GL.X) := 2.0 * Single (Map_U);
            New_Props.World_Pos (GL.Z) := 2.0 * Single (Map_V);
            New_Props.Is_Visible := aScript.Starts_Visible;

            New_Props.World_Pos (GL.Y) :=
              Tiles_Manager.Get_Tile_Height (New_Props.World_Pos (GL.X),
                                             New_Props.World_Pos (GL.Z),
                                             False, Respect_Ramps);
            New_Props.World_Pos (GL.Y) :=
              New_Props.World_Pos (GL.Y) + Single (2 * Height_Level);
            --  Allow portcullis and its collision model to start up high
            New_Props.Elevator := aScript.Initial_Elevator_State;
            if Script_Type = Elevator and New_Props.Elevator = At_Top then
                New_Props.World_Pos (GL.Y) :=
                  New_Props.World_Pos (GL.Y) + Single (aScript.Elevator_Top_Height);
                New_Props.Is_Visible := aScript.Elevator_Visible_At_Top;
            end if;
            New_Props.Vel := Vec3_0;
            New_Props.Anim_Duration := 0.0;
            New_Props.Anim_Elapsed_Time := 0.0;
            New_Props.Sprite_Duration := 0.0;
            New_Props.Delay_Countdown := 0.0;
            New_Props.Facing := Facing;
            case Facing is
                when 'E' => New_Props.Heading_Deg := Maths.Degree (270.0);
                when 'S' => New_Props.Heading_Deg := Maths.Degree (180.0);
                when 'W' => New_Props.Heading_Deg := Maths.Degree (90.0);
                when others => New_Props.Heading_Deg := Maths.Degree (0.0);
            end case;
            New_Props.Map_U := Integer (Map_U);
            New_Props.Map_V := Integer (Map_V);
            New_Props.Tx_Code := Tx;
            New_Props.Rx_Code := Rx;
            New_Props.Script_Index := 0;
            New_Props.Height_Level := Height_Level;

            if aScript.Has_Particles then
                New_Props.Particle_System_Index :=
                  Particle_System.Create_Particle_System
                    (To_String (aScript.Particle_Script_File_Name),
                     True, False, False);
                --  rotate offset
                Rot_Matrix := Rotate_Y_Degree
                  (Identity4,  New_Props.Heading_Deg);
                Ros := To_Vector3 (Rot_Matrix * To_Vector4 (aScript.Particles_Offset));
                Particle_System.Set_Particle_System_Position
                  (New_Props.Particle_System_Index, New_Props.World_Pos + Ros);
            else
                New_Props.Particle_System_Index := 0;
            end if;

            New_Props.Was_Triggered := False;
            New_Props.Is_On_Ground := False;
            New_Props.No_Save := False;
            New_Props.Is_Animating := False;
            New_Props.First_Doom_Tile_Set := False;
            New_Props.Second_Doom_Tile_Set := False;
            New_Props.Was_Collected_By_Player := False;
            Game_Utils.Game_Log ("Properties Manager Create_Prop_From_Script Mesh_Index"
                                 & Integer'Image (aScript.Mesh_Index));
            Process_Script_Type (New_Props, aScript, RX_Kind, Rebalance);
            if aScript.Uses_Sprite then
                Set_Up_Sprite (New_Props, aScript);
            end if;
            if aScript.Has_Lamp then
                Batch_Manager.Add_Static_Light
                  (Map_U, Map_V, Height_Level, aScript.Lamp_Offset,
                   aScript.Lamp_Diffuse, aScript.Lamp_Specular, aScript.Lamp_Range);
            end if;
            if New_Props.Rx_Code /= 0 and RX_Kind /= Rx_Invalid then
                Event_Controller.Add_Receiver (New_Props.Rx_Code, RX_Kind,
                                               Properties.Last_Index);
            end if;
            Prop_Renderer.Update_Props_In_Tiles
              (New_Props.Map_U, New_Props.Map_V, Int (Properties.Last_Index));
            if Rebalance then
                Rebalance_Props_In (Integer (Map_U), Integer (Map_V));
            end if;
            Properties.Append (New_Props);
        end if;
    end Create_Prop_From_Script;

    -- -------------------------------------------------------------------------

    function Get_Index_Of_Prop_Script (Script_File : String) return Positive is
        use Properties_Script_Package;
        Curs    : Cursor := Prop_Scripts.First;
        aScript : Prop_Script;
        Found   : Boolean := False;
        Index   : integer := 0;
        OK      : Boolean := False;
    begin
        while Has_Element (Curs) and not Found loop
            aScript := Element (Curs);
            Found := aScript.File_Name = To_Unbounded_String (Script_File);
            if Found then
                Index := To_Index (Curs);
            else
                Next (Curs);
            end if;
        end loop;
        OK := Found;
        if not Found then
            OK := Load_Property_Script (Script_File, Index);
        end if;
        if not OK then
            Put_Line ("Properties_Manager.Get_Index_Of_Prop_Script failed to load: " &
                        Script_File);
        end if;
--          Put_Line ("Properties_Manager.Get_Index_Of_Prop_Script: Index: " &
--                      Integer'Image (Index));

        return Index;
    end Get_Index_Of_Prop_Script;

    -- -------------------------------------------------------------------------

    --  read properties from an already open file
    procedure Load_Properties (Prop_File : File_Type) is
        use Ada.Strings;
        aLine          : constant String := Get_Line (Prop_File);
        PosL           : Natural := Fixed.Index (aLine, " ") + 1;
        PosR           : Natural;
        S_Length       : Integer := aLine'Length;
        Property_Count : Integer := 0;
        Script_File    : Unbounded_String;
        U              : Int := 0;       --  map position
        V              : Int := 0;
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
        Portal_Index := 0;
        Character_Controller.Gold_Current := 0;
        Character_Controller.Gold_Max := 0;
        Character_Controller.Total_Treasure_Found := 0;

        for index in 1 .. Property_Count loop
            declare
                Prop_Line : constant String := Get_Line (Prop_File);
            begin
                S_Length := Prop_Line'Length;
                PosL := Fixed.Index (Prop_Line, " ");
                Script_File := To_Unbounded_String (Prop_Line (1 .. PosL - 1));
                PosR := Fixed.Index (Prop_Line (PosL .. S_Length), ",");
                U := Int'Value (Prop_Line (PosL + 1 .. PosR - 1));
                PosL := Fixed.Index (Prop_Line (PosR .. S_Length), " ");
                V := Int'Value (Prop_Line (PosR + 1 .. PosL - 1));

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

    function Load_Property_Script (File_Name : String; Index : out Positive)
                                   return Boolean is
        use Properties_Script_Package;
        With_Path           : constant String := "src/props/" & File_Name;
        Script_File         : File_Type;
        aScript             : Prop_Script;
        Box_Point_Count     : Integer := 0;
        Hole_Point_Count    : Integer := 0;
        Smashed_Script_File : Unbounded_String;
        Has_Smashed_Script  : constant Boolean := False;
        OK                  : Boolean := True;
    begin
        Open (Script_File, In_File, With_Path);
        Game_Utils.Game_Log ("Properties_Manager.Load_Property_Script, " &
                               With_Path & " opened.");
        aScript.File_Name := To_Unbounded_String (File_Name);

        while not End_Of_File (Script_File) loop
            declare
                aLine    : constant String := Get_Line (Script_File);
                S_Length : constant Integer := aLine'Length;
            begin
                if S_Length > 1 and then aLine (1) /= '#' then
                    if S_Length > 4 and then aLine (1 .. 5)  = "mesh:" then
                        OK := Do_Mesh (aLine (7 .. S_Length), aScript);
                    elsif S_Length > 13 and then
                      aLine (1 .. 14)  = "outlines_mesh:" then
                        null;
                    elsif S_Length > 14 and then
                      aLine (1 .. 15)  = "smashed_script:" then
                        null;
                    elsif S_Length > 11 and then
                      aLine (1 .. 12)  = "diffuse_map:" then
                        null;
                    elsif S_Length > 12 and then
                      aLine (1 .. 13)  = "specular_map:" then
                        null;
                    elsif S_Length > 10 and then
                      aLine (1 .. 11)  = "normal_map:" then
                        null;
                    elsif S_Length > 12 and then
                      aLine (1 .. 13)  = "casts_shadow:" then
                        null;
                    elsif S_Length > 13 and then
                      aLine (1 .. 14)  = "draw_outlines:" then
                        null;
                    elsif S_Length > 11 and then
                      aLine (1 .. 12)  = "transparent:" then
                        null;
                    elsif S_Length > 11 and then
                      aLine (1 .. 12)  = "starts_open:" then
                        null;
                    elsif S_Length > 14 and then aLine
                      (1 .. 15)  = "starts_visible:" then
                        null;
                    elsif S_Length > 14 and then
                      aLine (1 .. 15)  = "lamp_offset_pos" then
                        null;
                    elsif S_Length > 18 and then
                      aLine (1 .. 19)  = "lamp_diffuse_colour" then
                        null;
                    elsif S_Length > 19 and then
                      aLine (1 .. 20)  = "lamp_specular_colour" then
                        null;
                    elsif S_Length > 9 and then aLine (1 .. 10)  = "lamp_range" then
                        null;
                    elsif S_Length > 9 and then aLine (1 .. 10)  = "particles:" then
                        null;
                    elsif S_Length > 16 and then
                      aLine (1 .. 17)  = "particles_offset:" then
                        null;
                    elsif S_Length > 4 and then aLine (1 .. 5)  = "type:" then
                        null;
                    elsif S_Length > 5 and then
                      aLine (1 .. 6)  = "scale:" then
                        null;
                    elsif S_Length > 11 and then aLine (1 .. 12)  = "uses_sprite:" then
                        null;
                    elsif S_Length > 15 and then aLine (1 .. 16)  = "sprite_map_rows:" then
                        null;
                    elsif S_Length > 15 and then aLine (1 .. 16)  = "sprite_map_cols:" then
                        null;
                    elsif S_Length > 15 and then aLine (1 .. 16)  = "sprite_y_offset:" then
                        null;
                    elsif S_Length > 12 and then aLine (1 .. 13)  = "sprite_timer:" then
                        null;
                    elsif S_Length > 6 and then aLine (1 .. 7)  = "height:" then
                        null;
                    elsif S_Length > 6 and then aLine (1 .. 7)  = "radius:" then
                        null;
                    elsif S_Length > 6 and then aLine (1 .. 7)  = "origin:" then
                        null;
                    elsif S_Length > 17 and then aLine (1 .. 18)  = "trigger_only_once:" then
                        null;
                    elsif S_Length > 19 and then aLine (1 .. 20)  = "character_activated:" then
                        null;
                    elsif S_Length > 13 and then aLine (1 .. 14)  = "npc_activated:" then
                        null;
                    elsif S_Length > 21 and then
                      aLine (1 .. 22)  = "hide_after_triggering:" then
                        null;
                    elsif S_Length > 12 and then aLine (1 .. 13)  = "box_xz_point:" then
                        null;
                    elsif S_Length > 11 and then aLine (1 .. 12)  = "hole_height:" then
                        null;
                    elsif S_Length > 13 and then aLine (1 .. 14)  = "hole_xz_point:" then
                        null;
                    elsif S_Length > 16 and then aLine (1 .. 17)  = "setOpeningTime_s:" then
                        null;
                    elsif S_Length > 12 and then aLine (1 .. 13)  = "goes_back_up:" then
                        null;
                    elsif S_Length > 14 and then aLine (1 .. 15)  = "goes_back_down:" then
                        null;
                    elsif S_Length > 16 and then aLine (1 .. 17)  = "starts_at_bottom:" then
                        null;
                    elsif S_Length > 23 and then aLine (1 .. 24)  = "elevator_visible_at_top:" then
                        null;
                    elsif S_Length > 26 and then aLine (1 .. 27)  = "elevator_visible_at_bottom:" then
                        null;
                    elsif S_Length > 19 and then aLine (1 .. 20)  = "elevator_top_height:" then
                        null;
                    elsif S_Length > 22 and then aLine (1 .. 23)  = "elevator_bottom_height:" then
                        null;
                    elsif S_Length > 22 and then aLine (1 .. 23)  = "elevator_down_duration:" then
                        null;
                    elsif S_Length > 20 and then aLine (1 .. 21)  = "elevator_up_duration:" then
                        null;
                    elsif S_Length > 7 and then aLine (1 .. 8)  = "delay_s:" then
                        null;
                    elsif S_Length > 5 and then aLine (1 .. 6)  = "value:" then
                        null;
                    elsif S_Length > 14 and then aLine (1 .. 15)  = "sound_activate:" then
                        null;
                    else
                        OK := False;
                        Game_Utils.Game_Log ("Properties_Manager.Load_Property_Script, "
                                             & "invalid property in " & File_Name &
                                               ": " & aLine);
                    end if;
                end if;
            end;  --  declare block
        end loop;

        Close (Script_File);

        if OK then
            Prop_Scripts.Append (aScript);
            Index := Prop_Scripts.Last_Index;
        else
            Put_Line ("Properties_Manager.Load_Property_Script, failed to load"
                      & With_Path);
        end if;

        if Has_Smashed_Script then
            OK := Load_Property_Script
              (To_String (Smashed_Script_File), aScript.Smashed_Script_Index);
            Prop_Scripts.Replace_Element (Index, aScript);
            if not OK then
                Put_Line ("Properties_Manager.Load_Property_Script, failed to load" &
                            To_String (Smashed_Script_File));
            end if;
        end if;

        --        Game_Utils.Game_Log ("Properties_Manager.Load_Property_Script, script properties loaded");
        return OK;
    end Load_Property_Script;

    -- --------------------------------------------------------------------------

    procedure Process_Script_Type (New_Props : in out Prop; aScript : Prop_Script;
                                   Rx_Kind   : in out Event_Controller.RX_Type;
                                   Rebalance  : in out Boolean) is
        use Singles;
        use Event_Controller;
        use Maths;
        Script_Type : constant Prop_Type := aScript.Prop_Kind;
        Mesh_Index  : Positive;
        Duration    : Float := 0.0;
        Tim         : Float := 0.0;
        Rot_Matrix  : Matrix4 := Identity4;
        Target      : Vector3 := Vec3_0;
    begin
        if aScript.Mesh_Index < 1 then
            raise Properties_Exception with
              "Process_Script_Type called with invalid Mesh_Index: "
                  & Integer'Image (aScript.Mesh_Index);
        end if;

        Mesh_Index := aScript.Mesh_Index;
        New_Props.Door := aScript.Initial_Door_State;
        case Script_Type is
            when Boulder =>
                New_Props.World_Pos (GL.Y) :=
                  New_Props.World_Pos (GL.Y) + Single (aScript.Radius);
                New_Props.Is_On_Ground := True;
                Rx_Kind := Rx_Boulder;
                New_Props.Boulder_Snd_Idx :=
                  Audio.Create_Boulder_Sound (New_Props.World_Pos);
            when Door => Rx_Kind := Rx_Door;
            when Dart_Trap => Rx_Kind := Rx_Dart_Trap;
            when Treasure =>  Character_Controller.Gold_Max :=
                  Character_Controller.Gold_Max + aScript.Value;
            when Portal => Portal_Index := Properties.Last_Index;
            when Bridge => Rebalance := True;
            when Pillar =>
                Rebalance := True;
                if Pillar_Bridge_SI = 0 then
                    Pillar_Bridge_SI :=
                      Get_Index_Of_Prop_Script (Pillar_Bridge_Script_File);
                end if;
                if Pillar_Bridge_SI = 0 then
                    raise Properties_Exception with
                      "Process_Script_Type can't create_prop_from_script " &
                      Pillar_Bridge_Script_File;
                end if;
            when Mirror =>
                if Mirror_Count >= Max_Mirrors then
                    raise Properties_Exception with
                      "Process_Script_Type wants too many mirrors.";
                end if;
                Mirror_Count := Mirror_Count + 1;
                Mirror_Indices (Mirror_Count) := Properties.Last_Index;
                Live_Mirror_Count := Live_Mirror_Count + 1;
            when Elevator =>
                New_Props.Elevator := aScript.Initial_Elevator_State;
                Rx_Kind := Rx_Elevator;
            when Anim_Loop =>
                --  randomise starting time
                Duration := Mesh_Loader.Animation_Duration (Mesh_Index, 1);
                Tim := Float (Abs (Random_Float));
                New_Props.Anim_Elapsed_Time := Tim * Duration;
            when End_Camera =>
                End_Camera_Position := New_Props.World_Pos;
                Rot_Matrix :=
                  Rotate_Y_Degree (Identity4, New_Props.Heading_Deg);
                Target := End_Camera_Position +
                  To_Vector3 (Rot_Matrix * (0.0, 0.0, -1.0, 1.0));
                Init_Lookat_Transform
                  (End_Camera_Position, Target, (0.0, 1.0, 0.0),
                   End_Camera_Matrix);
            when others => null;
        end case;

        New_Props.Model_Mat := Translation_Matrix (New_Props.World_Pos) *
          Rotate_Y_Degree (Identity4, New_Props.Heading_Deg);

    end Process_Script_Type;

    -- --------------------------------------------------------------------------

    procedure Rebalance_Props_In (Map_U, Map_V : Integer) is
        use Prop_Renderer;
        use Prop_Renderer_Support;
        use Singles;
        Prop_Size     : constant Integer := Props_In_Tiles_Size (Map_U, Map_V);
        Prop_Index   : Integer;
        Prop         : Property_Data;
        Script_Index : Integer;
        Script       : Prop_Renderer_Support.Prop_Script;
        Script_Kind  : Property_Type;
        Sprite_Pos   : Vector3;
        Rot_Matrix   : Matrix4;
        Origin       : Vector4;
    begin
        if not Tiles_Manager.Is_Tile_Valid (Int (Map_U), Int (Map_V)) then
            raise Properties_Exception with
              "Rebalance_Props_In called with invalid Map_U, Map_V: "
              & Integer'Image (Map_U) & ", " & Integer'Image (Map_V);
        end if;

        For index in 1 .. Prop_Size loop
            Prop_Index := Get_Property_Index (Map_U, Map_V, index);
            Prop := Get_Property_Data (Prop_Index);
            Script_Index := Prop.Script_Index;
            Script := Get_Script_Data (Script_Index);
            Script_Kind := Script.Script_Type;

            Prop.World_Pos (GL.Y) := Tiles_Manager.Get_Tile_Height
              (Prop.World_Pos (GL.X), Prop.World_Pos (GL.Z), False, False) +
                Single (2 * Prop.Height_Level);
            if Script_Kind = Boulder_Prop then
                Prop.World_Pos (GL.Y) := Prop.World_Pos (GL.Y) + Script.Radius;
                Prop.Is_On_Ground := True;
            end if;
            if Script.Uses_Sprite then
                Sprite_Pos := Prop.World_Pos;
                Sprite_Pos (GL.Y) := Sprite_Pos (GL.Y) +
                  Single (Script.Sprite_Y_Offset) + Sprite_Y_Offset;
                Sprite_Renderer.Set_Sprite_Position
                  (Prop.Script_Index, Sprite_Pos);
            end if;
            Rot_Matrix := Maths.Rotate_Y_Degree (Identity4, Prop.Heading_Deg);
            Prop.Model_Matrix := Rot_Matrix * Maths.Translation_Matrix ((Prop.World_Pos));
            Origin := To_Vector4 (Script.Origin);
            Prop.Origin_World := To_Vector3 (Prop.Model_Matrix * Origin);
            Replace_Property (index, Prop);
        end loop;

    end Rebalance_Props_In;

    -- --------------------------------------------------------------------------

    procedure Set_Up_Sprite (New_Props : in out Prop; aScript : Prop_Script) is
        use Singles;
        use Sprite_Renderer;
        Diff_Map   : constant GL.Objects.Textures.Texture := aScript.Diffuse_Map;
        Spec_Map   : constant GL.Objects.Textures.Texture := aScript.Specular_Map;
        Rows       : constant Integer := aScript.Sprite_Map_Rows;
        Cols       : constant Integer := aScript.Sprite_Map_Cols;
        Y_Offset   : constant Single := Single (aScript.Sprite_Y_Offset);
        Sprite_Pos : Vector3 := New_Props.World_Pos;
    begin
        New_Props.Script_Index := Add_Sprite (Diff_Map, Spec_Map, Cols, Rows);
        Set_Sprite_Scale (New_Props.Sprite_Index, aScript.Scale);
        Sprite_Pos (GL.Y) := Sprite_Pos (GL.Y) + Y_Offset + Sprite_Y_Offset;
        Set_Sprite_Position (New_Props.Sprite_Index, Sprite_Pos);
        Set_Sprite_Heading (New_Props.Sprite_Index, New_Props.Heading_Deg);

    end Set_Up_Sprite;

    -- --------------------------------------------------------------------------

end Properties_Manager;
