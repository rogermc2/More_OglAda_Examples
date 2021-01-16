
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings.Fixed;

with Maths;

with Audio;
with Batch_Manager;
with Character_Controller;
with Event_Controller;
with Game_Utils;
with Particle_System;
with Properties_Manager.Process;
with Prop_Renderer;
with Prop_Renderer_Support;
with Sprite_Renderer;
with Tiles_Manager;

package body Properties_Manager is
    use Properties_Manager.Process;

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

    procedure Rebalance_Props_In (Map_U, Map_V : Integer);
    procedure Set_Up_Sprite (New_Props : in out Prop; aScript : Prop_Script);

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
            New_Props.Door := Door_Closed;
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
            if Script_Type = Elevator and
              New_Props.Elevator = Elevator_At_Top then
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
            if aScript.Mesh_Index < 1 then
                raise Properties_Exception with
                  "Create_Prop_From_Script called with invalid Mesh_Index"
                  & Integer'Image (aScript.Mesh_Index);
            end if;
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
