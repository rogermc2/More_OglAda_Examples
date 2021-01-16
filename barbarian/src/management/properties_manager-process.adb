
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
with Texture_Manager;
with Tiles_Manager;

package body Properties_Manager.Process is

    Max_Mirrors     : constant Integer := 16;
    Sprite_Y_Offset : constant Single := 0.125;

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

    -- -------------------------------------------------------------------------

    procedure Do_Diffuse_Map (File_Name : String; aScript : in out Prop_Script) is
        Full_Path : constant String := "src/textures/" & File_Name;
    begin
        Texture_Manager.Load_Image_To_Texture (Full_Path, aScript.Diffuse_Map,
                                               True, True);
    end Do_Diffuse_Map;

    --  ----------------------------------------------------------------------------

    function Do_Mesh (File_Name : String; aScript : in out Prop_Script)
                      return Boolean is
        Full_Path    : constant String := "src/meshes/" & File_Name;
        Mesh_Index   : Positive;
        Managed_Mesh : Mesh_Loader.Mesh;
        Ok           : Boolean := False;
    begin
        aScript.Mesh_Index := Mesh_Loader.Load_Managed_Mesh
          (Full_Path, True, True, True, True, True);
        Mesh_Index := aScript.Mesh_Index;
        Managed_Mesh := Mesh_Loader.Loaded_Mesh (Mesh_Index);
        OK := Mesh_Loader.Loaded_Mesh_VAO (Mesh_Index, aScript.Vao);
        if not OK then
            Put_Line ("Properties_Manager.Do_Mesh, failed to load VAO for "
                      & File_Name);
        end if;
        if not Mesh_Loader.Loaded_Mesh_Bounding_Radius
          (Mesh_Index, aScript.Bounding_Radius) then
            Put_Line ("Properties_Manager.Do_Mesh, failed to load Bounding_Radius for "
                      & File_Name);
            OK := False;
        end if;
        if not Mesh_Loader.Loaded_Mesh_Shadow_VAO
          (Mesh_Index, aScript.Shadow_Vao) then
            Put_Line ("Properties_Manager.Do_Mesh, failed to load Shadow_VAO for "
                      & File_Name);
            OK := False;
        end if;
        if not Mesh_Loader.Loaded_Mesh_Vertex_Count
          (Mesh_Index, aScript.Vertex_Count) then
            Put_Line ("Properties_Manager.Do_Mesh, failed to load Vertex_Count for "
                      & File_Name);
            OK := False;
        end if;
        if not OK then
            Put_Line ("Properties_Manager.Do_Mesh, failed to load "
                      & File_Name);
        end if;
        return OK;
    end Do_Mesh;

    --  ----------------------------------------------------------------------------

    function Do_Outlines_Mesh (File_Name : String; aScript : in out Prop_Script)
                      return Boolean is
        Full_Path    : constant String := "src/meshes/" & File_Name;
        Mesh_Index   : Positive;
        Managed_Mesh : Mesh_Loader.Mesh;
        Ok           : Boolean := False;
    begin
        aScript.Outlines_Mesh_Index := Mesh_Loader.Load_Managed_Mesh
          (Full_Path, True, True, True, True, True);
        Mesh_Index := aScript.Outlines_Mesh_Index;
        Managed_Mesh := Mesh_Loader.Loaded_Mesh (Mesh_Index);
        OK := Mesh_Loader.Loaded_Mesh_VAO (Mesh_Index, aScript.Outlines_Vao);
        if not OK then
            Put_Line ("Properties_Manager.Do_Outlines_Mesh, failed to load outline VAO for "
                      & File_Name);
        end if;
        if not Mesh_Loader.Loaded_Mesh_Vertex_Count
          (Mesh_Index, aScript.Outlines_Vertex_Count) then
            Put_Line ("Properties_Manager.Do_Outlines_Mesh, failed to load outline Vertex_Count for "
                      & File_Name);
            OK := False;
        end if;
        if not OK then
            Put_Line ("Properties_Manager.Do_Outlines_Mesh, failed to load "
                      & File_Name);
        end if;
        return OK;
    end Do_Outlines_Mesh;

    --  ------------------------------------------------------------------------

    procedure Do_Normal_Map (File_Name : String; aScript : in out Prop_Script) is
        Full_Path : constant String := "src/textures/" & File_Name;
    begin
        Texture_Manager.Load_Image_To_Texture (Full_Path, aScript.Normal_Map,
                                               True, False);
    end Do_Normal_Map;

    --  ------------------------------------------------------------------------

    procedure Do_Starts_Open (State : String; aScript : in out Prop_Script) is
    begin
        if State = "1"then
            aScript.Initial_Door_State := Properties_Manager.Door_Open;
        else
            aScript.Initial_Door_State := Properties_Manager.Door_Closed;
        end if;
    end Do_Starts_Open;

    --  ------------------------------------------------------------------------

    procedure Do_Specular_Map (File_Name : String; aScript : in out Prop_Script) is
        Full_Path : constant String := "src/textures/" & File_Name;
    begin
        Texture_Manager.Load_Image_To_Texture (Full_Path, aScript.Specular_Map,
                                               True, True);
    end Do_Specular_Map;

    --  --------------------------------------------

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
                        OK := Do_Outlines_Mesh (aLine (16 .. S_Length), aScript);
                    elsif S_Length > 14 and then
                      aLine (1 .. 15)  = "smashed_script:" then
                        null;
                    elsif S_Length > 11 and then
                      aLine (1 .. 12)  = "diffuse_map:" then
                        Do_Diffuse_Map (aLine (14 .. S_Length), aScript);
                    elsif S_Length > 12 and then
                      aLine (1 .. 13)  = "specular_map:" then
                        Do_Specular_Map (aLine (15 .. S_Length), aScript);
                    elsif S_Length > 10 and then
                      aLine (1 .. 11)  = "normal_map:" then
                        Do_Normal_Map (aLine (13 .. S_Length), aScript);
                    elsif S_Length > 12 and then
                      aLine (1 .. 13)  = "casts_shadow:" then
                        aScript.Casts_Shadow := aLine (15 .. 15) /= "0";
                    elsif S_Length > 13 and then
                      aLine (1 .. 14)  = "draw_outlines:" then
                        aScript.Draw_Outlines := aLine (16 .. 16) /= "0";
                    elsif S_Length > 11 and then
                      aLine (1 .. 12)  = "transparent:" then
                        aScript.Transparent := aLine (14 .. 14) /= "0";
                    elsif S_Length > 11 and then
                      aLine (1 .. 12)  = "starts_open:" then
                        Do_Starts_Open (aLine (14 .. 14), aScript);
                    elsif S_Length > 14 and then aLine
                      (1 .. 15)  = "starts_visible:" then
                        aScript.Starts_Visible := aLine (17 .. 17) /= "0";
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

end Properties_Manager.Process;
