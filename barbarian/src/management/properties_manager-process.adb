
with Ada.Containers.Vectors;
with Ada.Exceptions; use Ada.Exceptions;
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

    Pillar_Bridge_Script_File : constant String := "pillar_bridge.script";
    Pillar_Bridge_SI          : Natural := 0;
    End_Camera_Matrix         : Singles.Matrix4 := Singles.Identity4;
    End_Camera_Position       : Singles.Vector3 := Maths.Vec3_0;

    -- -------------------------------------------------------------------------

    procedure Do_Diffuse_Map (File_Name : String;
                              aScript   : in out Prop_Script) is
        Full_Path : constant String := "src/textures/" & File_Name;
    begin
        --        Game_Utils.Game_Log ("Properties_Manager-Process.Do_Diffuse_Map calling Load_Image_To_Texture");
        Texture_Manager.Load_Image_To_Texture (Full_Path, aScript.Diffuse_Map_Id,
                                               True, True);
        --        Game_Utils.Game_Log
        --        ("Properties_Manager-Process.Do_Diffuse_Map loaded " & File_Name);
    end Do_Diffuse_Map;

    --  ------------------------------------------------------------------------

    procedure Do_Lamp_Offset (aLine : String; aScript : in out Prop_Script) is
        use Ada.Strings;
        S_Length : constant Integer := aLine'Length;
        Pos_1    : Natural := Fixed.Index (aLine, "(");
        Pos_2    : Natural := Fixed.Index (aLine, ",");
    begin
        if Pos_1 = 0 or Pos_2 = 0 then
            raise Properties_Exception with
              "Properties_Manager-Process.Do_Lamp_Offset invalid format: " &
              aLine;
        else
            aScript.Has_Lamp := True;
            aScript.Lamp_Offset (GL.X) :=
              Single'Value (aLine (Pos_1 + 1 .. Pos_2 - 1));
            Pos_1 := Pos_2 + 2;
            Pos_2 := Fixed.Index (aLine (Pos_1 + 1 .. S_Length), ",");
            aScript.Lamp_Offset (GL.Y) :=
              Single'Value (aLine (Pos_1 .. Pos_2 - 1));
            Pos_1 := Pos_2 + 2;
            Pos_2 := Fixed.Index (aLine (Pos_1 + 1 .. S_Length), ")");
            aScript.Lamp_Offset (GL.Z) :=
              Single'Value (aLine (Pos_1 .. Pos_2 - 1));
        end if;
    end Do_Lamp_Offset;

    --  ------------------------------------------------------------------------

    function Do_Mesh (File_Name : String; aScript : in out Prop_Script)
                     return Boolean is
        Full_Path    : constant String := "src/meshes/" & File_Name;
        Mesh_Index   : Positive;
        Managed_Mesh : Mesh_Loader.Mesh;
        Ok           : Boolean := False;
    begin
--          Game_Utils.Game_Log
--            ("Properties_Manager-Process.Do_Mesh, File " & File_Name);
        aScript.Mesh_Index := Mesh_Loader.Load_Managed_Mesh
          (Full_Path, True, True, True, True, True);
        Mesh_Index := aScript.Mesh_Index;
        Managed_Mesh := Mesh_Loader.Loaded_Mesh (Mesh_Index);
        --        Game_Utils.Game_Log
        --          ("Properties_Manager.Do_Mesh-Process, loading Mesh_VAO, Mesh_Index " &
        --             Integer'Image (Mesh_Index));
        OK := Mesh_Loader.Loaded_Mesh_VAO (Mesh_Index, aScript.Vao);
        if not OK then
            Put_Line ("Properties_Manager.Process.Do_Mesh, failed to load VAO for "
                      & File_Name);
        end if;

        if not Mesh_Loader.Loaded_Mesh_Bounding_Radius
          (Mesh_Index, Float (aScript.Bounding_Radius)) then
            Put_Line ("Properties_Manager.Process.Do_Mesh, failed to load Bounding_Radius for "
                      & File_Name);
            OK := False;
        end if;
        if not Mesh_Loader.Loaded_Mesh_Shadow_VAO
          (Mesh_Index, aScript.Shadow_Vao) then
            Put_Line ("Properties_Manager.Process.Do_Mesh, failed to load Shadow_VAO for "
                      & File_Name);
            OK := False;
        end if;
        if not Mesh_Loader.Loaded_Mesh_Vertex_Count
          (Mesh_Index, Integer (aScript.Vertex_Count)) then
            Put_Line ("Properties_Manager.Process.Do_Mesh, failed to load Vertex_Count for "
                      & File_Name);
            OK := False;
        end if;
        if not OK then
            Put_Line ("Properties_Manager.Process.Do_Mesh, failed to load "
                      & File_Name);
        end if;
        --        Game_Utils.Game_Log ("Properties_Manager-Process.Do_Mesh, has loaded " &
        --                               File_Name);
        return OK;

    exception
        when anError : Constraint_Error =>
            Put ("Properties_Manager-Process.Do_Mesh constraint error: ");
            Put_Line (Exception_Information (anError));
            raise;
        when anError :  others =>
            Put_Line ("An exception occurred in Properties_Manager-Process.Do_Mesh.");
            Put_Line (Exception_Information (anError));
            raise;

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
            Put_Line ("Properties_Manager-Process.Do_Outlines_Mesh, failed to load outline VAO for "
                      & File_Name);
        end if;
        if not Mesh_Loader.Loaded_Mesh_Vertex_Count
          (Mesh_Index, Integer (aScript.Outlines_Vertex_Count)) then
            Put_Line ("Properties_Manager-Process.Do_Outlines_Mesh, failed to load outline Vertex_Count for "
                      & File_Name);
            OK := False;
        end if;
        if not OK then
            Put_Line ("Properties_Manager-Process.Do_Outlines_Mesh, failed to load "
                      & File_Name);
        end if;
        return OK;
    end Do_Outlines_Mesh;

    --  ------------------------------------------------------------------------

    procedure Do_Normal_Map (File_Name : String; aScript : in out Prop_Script) is
        Full_Path : constant String := "src/textures/" & File_Name;
    begin
        Texture_Manager.Load_Image_To_Texture (Full_Path, aScript.Normal_Map_Id,
                                               True, False);
    end Do_Normal_Map;

    --  ------------------------------------------------------------------------

    procedure Do_Sprite (State : String; aScript : in out Prop_Script) is
    begin
        if State = "1"then
            aScript.Initial_Door_State := Open_State;
        else
            aScript.Initial_Door_State := Closed_State;
        end if;
    end Do_Sprite;

    --  ------------------------------------------------------------------------

    procedure Do_Specular_Map (File_Name : String;
                               aScript   : in out Prop_Script) is
        Full_Path : constant String := "src/textures/" & File_Name;
    begin
        Game_Utils.Game_Log ("Properties_Manager-Process.Do_Specular_Map, Specular_Map "
                             & File_Name);
        Game_Utils.Game_Log ("Properties_Manager-Process.Do_Specular_Map, Specular_Map already initialized "
                             & Boolean'Image (aScript.Specular_Map_Id.Initialized));
        Texture_Manager.Load_Image_To_Texture
          (Full_Path, aScript.Specular_Map_Id, True, True);
        Game_Utils.Game_Log ("Properties_Manager-Process.Do_Specular_Map, Specular_Map initialized "
                             & Boolean'Image (aScript.Specular_Map_Id.Initialized));
    end Do_Specular_Map;

    --  ------------------------------------------------------------------------

    procedure Do_Starts_Open (State : String; aScript : in out Prop_Script) is
    begin
        if State = "1"then
            aScript.Initial_Door_State := Open_State;
        else
            aScript.Initial_Door_State := Closed_State;
        end if;
    end Do_Starts_Open;

    --  ------------------------------------------------------------------------

    procedure Do_Type (Type_Code : String; aScript : in out Prop_Script) is
    begin
        if Type_Code = "generic" then
            aScript.Script_Type := Generic_Prop;
        elsif Type_Code = "boulder" then
            aScript.Script_Type := Boulder_Prop;
        elsif Type_Code = "decapitated_head" then
            aScript.Script_Type := Decap_Head_Prop;
        elsif Type_Code = "door" then
            aScript.Script_Type := Door_Prop;
        elsif Type_Code = "elevator" then
            aScript.Script_Type := Elevator_Prop;
        elsif Type_Code = "dart_shooter" then
            aScript.Script_Type := Dart_Trap_Prop;
        elsif Type_Code = "touch_plate" then
            aScript.Script_Type := Touch_Plate_Prop;
        elsif Type_Code = "treasure" then
            aScript.Script_Type := Treasure_Prop;
        elsif Type_Code = "portal" then
            aScript.Script_Type := Portal_Prop;
        elsif Type_Code = "bridge" then
            aScript.Script_Type := Bridge_Prop;
        elsif Type_Code = "pillar" then
            aScript.Script_Type := Pillar_Prop;
        elsif Type_Code = "box" then
            aScript.Script_Type := Box_Prop;
        elsif Type_Code = "mirror" then
            aScript.Script_Type := Mirror_Prop;
        elsif Type_Code = "tavern" then
            aScript.Script_Type := Tavern_Prop;
        elsif Type_Code = "javelin_stall" then
            aScript.Script_Type := Jav_Stand_Prop;
        elsif Type_Code = "diamond_trigger" then
            aScript.Script_Type := Diamond_Trigger_Prop;
        elsif Type_Code = "hammer" then
            aScript.Script_Type := Hammer_Prop;
        elsif Type_Code = "food" then
            aScript.Script_Type := Food_Prop;
        elsif Type_Code = "anim_loop" then
            aScript.Script_Type := Anim_Loop_Prop;
        elsif Type_Code = "windlass" then
            aScript.Script_Type := Windlass_Prop;
        elsif Type_Code = "big_box" then
            aScript.Script_Type := Big_Box_Prop;
        elsif Type_Code = "end_camera" then
            aScript.Script_Type := End_Camera_Prop;
        elsif Type_Code = "pot" then
            aScript.Script_Type := Pot_Prop;
        end if;
    end Do_Type;

    --  ------------------------------------------------------------------------

    function Do_Vec2 (aLine : String) return Singles.Vector2 is
        use Ada.Strings;
        S_Length  : constant Integer := aLine'First + aLine'Length - 1;
        Pos_1     : constant Natural := Fixed.Index (aLine, ",");
        theVector : Singles.Vector2;
    begin
        if Pos_1 = 0 then
            raise Properties_Exception with
              "Do_Vec2 invalid format: " & aLine;
        else
            theVector (GL.X) := Single'Value (aLine (aLine'First .. Pos_1 - 1));
            theVector (GL.Y) := Single'Value (aLine (Pos_1 + 2 .. S_Length));
        end if;
        return theVector;
    end Do_Vec2;

    --  ------------------------------------------------------------------------

    function Do_Vec3 (aLine : String) return Singles.Vector3 is
        use Ada.Strings;
        S_Length  : constant Integer := aLine'First + aLine'Length - 1;
        Pos_1     : Natural := Fixed.Index (aLine, "(");
        Pos_2     : Natural := Fixed.Index (aLine, ",");
        theVector : Singles.Vector3;
    begin
        if Pos_1 = 0 or Pos_2 = 0 then
            raise Properties_Exception with
              "Do_Vec3 invalid format: " & aLine;
        else
            theVector (GL.X) := Single'Value (aLine (Pos_1 + 1 .. Pos_2 - 1));
            Pos_1 := Pos_2 + 2;
            Pos_2 := Fixed.Index (aLine (Pos_1 .. S_Length), ",");
            theVector (GL.Y) := Single'Value (aLine (Pos_1 .. Pos_2 - 1));
            Pos_1 := Pos_2 + 2;
            Pos_2 := Fixed.Index (aLine (Pos_1 + 1 .. S_Length), ")");
            theVector (GL.Z) := Single'Value (aLine (Pos_1 .. Pos_2 - 1));
        end if;
        return theVector;
    end Do_Vec3;

    --  ------------------------------------------------------------------------

    function Get_Index_Of_Prop_Script (Script_File : String) return Natural is
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
--              Game_Utils.Game_Log
--                ("Properties_Manager.Process.Get_Index_Of_Prop_Script loading: " &
--                   Script_File);
            OK := Load_Property_Script (Script_File, Index);
            Game_Utils.Game_Log
                ("Properties_Manager.Process.Get_Index_Of_Prop_Script Diffuse and Specular_Map_Id.Initialized: " &
                   Boolean'Image (aScript.Diffuse_Map_Id.Initialized) & " " &
                   Boolean'Image (aScript.Specular_Map_Id.Initialized));
        end if;

        if not OK then
            Put_Line
              ("Properties_Manager.Process.Get_Index_Of_Prop_Script failed to load: " &
                 Script_File);
        end if;
--                Game_Utils.Game_Log
--                  ("Properties_Manager.Process.Get_Index_Of_Prop_Script done, Index: " &
--                     Integer'Image (Index));
        return Index;

    exception
        when anError : Constraint_Error =>
            Put
              ("Properties_Manager.Process.Get_Index_Of_Prop_Script constraint error: ");
            Put_Line (Exception_Information (anError));
            raise;
        when anError :  others =>
            Put_Line
              ("An exception occurred in Properties_Manager.Process.Get_Index_Of_Prop_Script.Load_Property_Script.");
            Put_Line (Exception_Information (anError));
            raise;
    end Get_Index_Of_Prop_Script;

    -- -------------------------------------------------------------------------

    function Load_Property_Script (File_Name : String; Prop_Index : out Positive)
                                  return Boolean is
        use Properties_Script_Package;
        With_Path           : constant String := "src/props/" & File_Name;
        Script_File         : File_Type;
        aScript             : Prop_Script;
        Point_2D            : Singles.Vector2;
        Box_Point_Count     : Int := 0;
        Hole_Point_Count    : Int := 0;
        Smashed_Script_File : Unbounded_String;
        Has_Smashed_Script  : Boolean := False;
        OK                  : Boolean := True;
    begin
        if File_Name'Length = 0 then
            raise Properties_Process_Exception with
              "Properties_Manager.Process.Load_Property_Script called with empty file name";
        end if;

        Open (Script_File, In_File, With_Path);
--          Put_Line ("Properties_Manager.Process.Load_Property_Script, " &
--                     With_Path & " opened.");
        aScript.File_Name := To_Unbounded_String (File_Name);

        while not End_Of_File (Script_File) loop
            declare
                aLine    : constant String := Get_Line (Script_File);
                S_Length : constant Integer := aLine'Length;
            begin
--                  Game_Utils.Game_Log ("Properties_Manager-Process.Load_Property_Script, " &
--                                       aLine);
                if S_Length > 1 and then aLine (1) /= '#' then
                    if S_Length > 4 and then aLine (1 .. 5)  = "mesh:" then
                        OK := Do_Mesh (aLine (7 .. S_Length), aScript);
                    elsif S_Length > 13 and then
                      aLine (1 .. 14)  = "outlines_mesh:" then
                        OK := Do_Outlines_Mesh (aLine (16 .. S_Length), aScript);
                    elsif S_Length > 14 and then
                      aLine (1 .. 15)  = "smashed_script:" then
                        Smashed_Script_File :=
                          To_Unbounded_String (aLine (17 .. S_Length));
                        --  Flag to load at end as prop script counter would
                        --  be wrong.
                        Has_Smashed_Script := True;
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
                        aScript.Has_Lamp := True;
                        aScript.Lamp_Offset := Do_Vec3  (aLine (17 .. S_Length));
                    elsif S_Length > 18 and then
                      aLine (1 .. 19)  = "lamp_diffuse_colour" then
                        aScript.Has_Lamp := True;
                        aScript.Lamp_Diffuse := Do_Vec3  (aLine (21 .. S_Length));
                    elsif S_Length > 19 and then
                      aLine (1 .. 20)  = "lamp_specular_colour" then
                        aScript.Has_Lamp := True;
                        aScript.Lamp_Specular := Do_Vec3  (aLine (22 .. S_Length));
                    elsif S_Length > 9 and then
                      aLine (1 .. 10)  = "lamp_range" then
                        aScript.Lamp_Range := Float'Value (aLine (12 .. S_Length));
                    elsif S_Length > 9 and then
                      aLine (1 .. 10)  = "particles:" then
                        aScript.Particle_Script_File_Name :=
                          To_Unbounded_String (aLine (12 .. S_Length));
                    elsif S_Length > 16 and then
                      aLine (1 .. 17)  = "particles_offset:" then
                        aScript.Particles_Offset := Do_Vec3 (aLine (19 .. S_Length));
                    elsif S_Length > 4 and then aLine (1 .. 5)  = "type:" then
                        Do_Type (aLine (7 .. S_Length), aScript);
                    elsif S_Length > 5 and then
                      aLine (1 .. 6)  = "scale:" then
                        aScript.Scale := Do_Vec3 (aLine (8 .. S_Length));
                    elsif S_Length > 11 and then
                      aLine (1 .. 12)  = "uses_sprite:" then
                        aScript.Uses_Sprite := aLine (14 .. 14) /= "0";
                    elsif S_Length > 15 and then
                      aLine (1 .. 16)  = "sprite_map_rows:" then
                        aScript.Sprite_Map_Rows :=
                          Integer'Value (aLine (18 .. S_Length));
                    elsif S_Length > 15 and then
                      aLine (1 .. 16)  = "sprite_map_cols:" then
                        aScript.Sprite_Map_Cols :=
                          Integer'Value (aLine (18 .. S_Length));
                    elsif S_Length > 15 and then
                      aLine (1 .. 16)  = "sprite_y_offset:" then
                        aScript.Sprite_Y_Offset :=
                          Float'Value (aLine (18 .. S_Length));
                    elsif S_Length > 12 and then
                      aLine (1 .. 13)  = "sprite_timer:" then
                        aScript.Sprite_Timer :=
                          Float'Value (aLine (15 .. S_Length));
                    elsif S_Length > 6 and then
                      aLine (1 .. 7)  = "height:" then
                        aScript.Height :=
                          Single'Value (aLine (9 .. S_Length));
                    elsif S_Length > 6 and then
                      aLine (1 .. 7)  = "radius:" then
                        aScript.Radius :=
                          Single'Value (aLine (9 .. S_Length));
                    elsif S_Length > 6 and then
                      aLine (1 .. 7)  = "origin:" then
                        aScript.Origin := Do_Vec3 (aLine (9 .. S_Length));
                    elsif S_Length > 17 and then
                      aLine (1 .. 18)  = "trigger_only_once:" then
                        aScript.Trigger_Only_Once := aLine (20 .. 20) /= "0";
                    elsif S_Length > 19 and then
                      aLine (1 .. 20)  = "character_activated:" then
                        aScript.Character_Activated := aLine (22 .. 22) /= "0";
                    elsif S_Length > 13 and then
                      aLine (1 .. 14)  = "npc_activated:" then
                        aScript.Npc_Activated := aLine (16 .. 16) /= "0";
                    elsif S_Length > 21 and then
                      aLine (1 .. 22)  = "hide_after_triggering:" then
                        aScript.Hide_After_Triggering := aLine (24 .. 24) /= "0";
                    elsif S_Length > 12 and then
                      aLine (1 .. 13)  = "box_xz_point:" then
                      if S_Length > 14 then
                        Point_2D := Do_Vec2 (aLine (15 .. S_Length));
                        Box_Point_Count := Box_Point_Count + 1;
                        aScript.Box_Points (Box_Point_Count) := Point_2D;
                      end if;
                    elsif S_Length > 11 and then
                      aLine (1 .. 12)  = "hole_height:" then
                        aScript.Hole_Height :=
                          Single'Value (aLine (14 .. S_Length));
                    elsif S_Length > 13 and then
                      (aLine (1 .. 14)  = "hole_xz_point:") then
                        if S_Length > 15 then
                            Point_2D := Do_Vec2 (aLine (15 .. S_Length));
                            Hole_Point_Count := Hole_Point_Count + 1;
                            aScript.Hole_Points (Hole_Point_Count) := Point_2D;
                        end if;
                    elsif S_Length > 16 and then
                      aLine (1 .. 17)  = "setOpeningTime_s:" then
                        aScript.Opening_Time_S :=
                          Float'Value (aLine (19 .. S_Length));
                    elsif S_Length > 12 and then
                      aLine (1 .. 13)  = "goes_back_up:" then
                        aScript.Elevator_Goes_Back_Up := aLine (15 .. 15) /= "0";
                    elsif S_Length > 14 and then
                      aLine (1 .. 15)  = "goes_back_down:" then
                        aScript.Elevator_Goes_Back_Down := aLine (15 .. 15) /= "0";
                    elsif S_Length > 16 and then
                      aLine (1 .. 17)  = "starts_at_bottom:" then
                        aScript.Starts_At_Bottom := aLine (19 .. 19) /= "0";
                    elsif S_Length > 23 and then
                      aLine (1 .. 24)  = "elevator_visible_at_top:" then
                        aScript.Elevator_Visible_At_Top := aLine (26 .. 26) /= "0";
                    elsif S_Length > 26 and then
                      aLine (1 .. 27)  = "elevator_visible_at_bottom:" then
                        aScript.Elevator_Visible_At_Bottom :=
                          aLine (29 .. 29) /= "0";
                    elsif S_Length > 19 and then
                      aLine (1 .. 20)  = "elevator_top_height:" then
                        aScript.Elevator_Top_Height :=
                          Float'Value (aLine (22 .. S_Length));
                    elsif S_Length > 22 and then
                      aLine (1 .. 23)  = "elevator_bottom_height:" then
                        aScript.Elevator_Bottom_Height :=
                          Float'Value (aLine (25 .. S_Length));
                    elsif S_Length > 22 and then
                      aLine (1 .. 23)  = "elevator_down_duration:" then
                        aScript.Elevator_Down_Duration :=
                          Float'Value (aLine (25 .. S_Length));
                    elsif S_Length > 20 and then
                      aLine (1 .. 21)  = "elevator_up_duration:" then
                        aScript.Elevator_Up_Duration :=
                          Float'Value (aLine (23 .. S_Length));
                    elsif S_Length > 7 and then
                      aLine (1 .. 8)  = "delay_s:" then
                        aScript.Elevator_Wait_Delay :=
                          Float'Value (aLine (10 .. S_Length));
                    elsif S_Length > 5 and then aLine (1 .. 6)  = "value:" then
                        aScript.Value :=
                          Integer'Value (aLine (8 .. S_Length));
                    elsif S_Length > 14 and then
                      aLine (1 .. 15)  = "sound_activate:" then
                        aScript.Sound_Activate_File_Name :=
                          To_Unbounded_String (aLine (17 .. S_Length));
                    else
                        OK := False;
                        raise Properties_Process_Exception with
                        "Properties_Manager.Load_Property_Script, "  &
                             "invalid property in " & File_Name & ": " & aLine;
                    end if;
                end if;
            end;  --  declare block
        end loop;

        Close (Script_File);

        if OK then
            Prop_Scripts.Append (aScript);
            Prop_Index := Prop_Scripts.Last_Index;
            --           Game_Utils.Game_Log ("Properties_Manager-Process.Load_Property_Script, loaded"
            --                                & With_Path & ", index " & Integer'Image (Prop_Index));
        else
            Put_Line ("Properties_Manager-Process.Load_Property_Script failed to load"
                      & With_Path);
        end if;

        if Has_Smashed_Script then
            OK := Load_Property_Script
              (To_String (Smashed_Script_File), aScript.Smashed_Script_Index);
            Prop_Scripts.Replace_Element (Prop_Index, aScript);
            if not OK then
                Put_Line ("Properties_Manager.Process.Load_Property_Script, failed to load " &
                            To_String (Smashed_Script_File));
            end if;
        end if;
        return OK;

    exception
        when anError : Constraint_Error =>
            Put ("Properties_Manager.Process.Load_Property_Script constraint error: ");
            Put_Line (Exception_Information (anError));
            raise;
        when anError :  others =>
            Put_Line ("An exception occurred in Properties_Manager.Process.Load_Property_Script.");
            Put_Line (Exception_Information (anError));
            raise;
    end Load_Property_Script;

    -- --------------------------------------------------------------------------

    procedure Process_Script_Type (New_Props  : in out Property_Data;
                                   aScript    : Prop_Script;
                                   Rx_Kind    : in out Event_Controller.RX_Type;
                                   Rebalance  : in out Boolean) is
        use Singles;
        use Event_Controller;
        use Maths;
        Script_Type : constant Property_Type := aScript.Script_Type;
        Mesh_Index  : Positive;
        Duration    : Float := 0.0;
        Tim         : Float := 0.0;
        Rot_Matrix  : Matrix4 := Identity4;
        Target      : Vector3 := Vec3_0;
    begin
        New_Props.Door_Position := aScript.Initial_Door_State;
        case Script_Type is
            when Boulder_Prop =>
                New_Props.World_Pos (GL.Y) :=
                  New_Props.World_Pos (GL.Y) + Single (aScript.Radius);
                New_Props.Is_On_Ground := True;
                Rx_Kind := Rx_Boulder;
                New_Props.Boulder_Snd_Idx :=
                  Audio.Create_Boulder_Sound (New_Props.World_Pos);
            when Door_Prop => Rx_Kind := Rx_Door;
            when Dart_Trap_Prop => Rx_Kind := Rx_Dart_Trap;
            when Treasure_Prop =>  Character_Controller.Set_Gold_Max
                  (Character_Controller.Gold_Max + aScript.Value);
            when Portal_Prop =>
                Prop_Renderer.Set_Portal_Index (Properties.Last_Index);
            when Bridge_Prop => Rebalance := True;
            when Pillar_Prop =>
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
            when Mirror_Prop =>
                if Mirror_Count >= Max_Mirrors then
                    raise Properties_Exception with
                      "Process_Script_Type wants too many mirrors.";
                end if;
                Mirror_Count := Mirror_Count + 1;
                Mirror_Indices (Mirror_Count) := Properties.Last_Index;
                Live_Mirror_Count := Live_Mirror_Count + 1;
            when Elevator_Prop =>
                New_Props.Elevator := aScript.Initial_Elevator_State;
                Rx_Kind := Rx_Elevator;
            when Anim_Loop_Prop =>
                if aScript.Mesh_Index < 1 then
                    raise Properties_Exception with
                      "Properties_Manager-Process Process_Script_Type Anim_Loop_Prop called with invalid Mesh_Index: "
                      & Integer'Image (aScript.Mesh_Index);
                end if;

                Mesh_Index := aScript.Mesh_Index;
                --  randomise starting time
                Duration := Mesh_Loader.Animation_Duration (Mesh_Index, 1);
                Tim := Float (Abs (Random_Float));
                New_Props.Anim_Elapsed_Time := Tim * Duration;
            when End_Camera_Prop =>
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

        New_Props.Model_Matrix := Translation_Matrix (New_Props.World_Pos) *
          Rotate_Y_Degree (Identity4, New_Props.Heading_Deg);

    exception
        when anError : Constraint_Error =>
            Put ("Properties_Manager-Process.Process_Script_Type constraint error: ");
            Put_Line (Exception_Information (anError));
            raise;
        when anError :  others =>
            Put_Line ("An exception occurred in Properties_Manager-Process.Process_Script_Type.");
            Put_Line (Exception_Information (anError));
            raise;
    end Process_Script_Type;

    -- --------------------------------------------------------------------------

end Properties_Manager.Process;
