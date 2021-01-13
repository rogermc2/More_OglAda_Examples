
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;

with GL.Culling;
with GL.Objects.Programs;
with GL.Toggles;

with Audio;
with Batch_Manager;
with Camera;
with Character_Controller;
with Coins_Shader_Manager;
with Depth_Skinned_Shader_Manager;
with Event_Controller;
with Frustum;
with Game_Utils;
with GL_Maths;
with GL_Utils;
with Jav_Stand_Shader_Manager;
with Particle_System;
with Portal_Shader_Manager;
with Properties_Shader_Manager;
with Properties_Skinned_Shader_Manager;
with Prop_Renderer.Boulder;
with Prop_Renderer.Render;
with Settings;
with Shadows;
with Specs_Manager;
with Sprite_Renderer;
with Texture_Manager;
with Tiles_Manager;
with Transparency;

package body Prop_Renderer is
    use Prop_Renderer_Support;

    Boulder_Bounce_Sound_File : constant String
      := "EXPLOSION_Medium_Little_Debris_Burning_Tail_stereo.wav";
    Decap_Sound_File          : constant String := "GORE_Blade_Chop_mono.wav";
    Decap_Bounce_Sound_File   : constant String := "GORE_Stab_Splat_Only_mono.wav";
    Splash_Sound_File         : constant String := "SPLASH_Small_03_mono.wav";
    Pot_Smash_Sound           : constant String
      := "SHATTER_Glass_Medium_01_mono.wav";
    Mirror_Smash_Magic_Sound  : constant String
      := "MAGIC_SPELL_Scary_Raising_Reverse_Rain_Subtle_stereo.wav";  --  MAGIC_SPELL_Short_Fast_Burst_Quick_Fade_stereo.wav"
    All_Mirrors_Smashed_Sound : constant String
      := "MAGIC_SPELL_Passing_Tunnel_Sci-Fi_Fast_Rapid_Echo_stereo.wav";

    Head_Particles_File   : constant String := "blood_artery_jet.particles";
    Splash_Particles_File : constant String := "splash.particles";
    Dust_Particles_File   : constant String := "dust.particles";
    Pot_Particles_File    : constant String := "pot.particles";
    Mirror_Particles_File : constant String := "mirror.particles";

    Max_Scene_Lamp_Locs        : constant integer := 3;
    --  Up To 32 Types, With 8 Active Ones Of Each Type
    Max_Decap_Types            : constant Integer :=  32;
    Max_Active_Decaps_Per_Type : constant Integer := 8;
    Max_Decap_Particles        : constant Integer := 4;
    Max_Mirrors                : constant Integer := 16;

    package Property_Scripts_Package is new Ada.Containers.Vectors
      (Positive, Prop_Script);
    type Script_List is new Property_Scripts_Package.Vector with null Record;

    package Properties_Package is new Ada.Containers.Vectors
      (Positive, Prop_Renderer_Support.Property_Data);
    type Properties_List is new Properties_Package.Vector with null Record;

    package Indicies_Package is new Ada.Containers.Vectors (Positive, Positive);
    type Indicies_List is new Indicies_Package.Vector with null Record;

    --  Animation and rendering
    Model_Matrix                  : Singles.Matrix4 := (others => (others => 0.0));
    Current_Bone_Transforms       : Singles.Matrix4_Array
      (1 .. Mesh_Loader.Max_Bones)  := (others => Singles.Identity4);
    Anim_Duration                 : Integer := 0;
    Anim_Elapsed_Time             : Integer := 0;
    Sprite_Duration               : Integer := 0;
    Delay_Countdown               : Integer := 0;
    --  Hack to stop decap head bouncing when stuck
    Bounce_Count                  : Integer := 0;
    Scripts                       : Script_List;
    Properties                    : Properties_List;
    Active_Properties_A           : Indicies_List;
    Active_Properties_B           : Indicies_List;
    Curr_Active_Props_A           : Boolean := True;
    Basic_Render_List             : Indicies_List;
    Skinned_Render_List           : Indicies_List;
    Jav_Stand_Render_List         : Indicies_List;
    Portal_Render_List            : Indicies_List;
    Treasure_Render_List          : Indicies_List;
    Last_Head_Launched            : GL_Maths.Integer_Array (1 .. Max_Decap_Types);
    Props_In_Tiles                : Props_In_Tiles_Array;
    Head_Particles                : GL_Maths.Integer_Array (1 .. Max_Decap_Particles);
    Decap_Heads_Prop_Index        : constant array
      (1 .. Max_Decap_Types, 1 .. Max_Active_Decaps_Per_Type) of Integer
      := (others => (others => 0));
    Dust_Particles                : Integer := -1;
    Dust_Particlesb               : Integer := -1;
    Dust_Particlesc               : Integer := -1;
    Pot_Particles                 : Integer := -1;
    Mirror_Particles              : Integer := -1;
    Splash_Particles              : Integer := -1;

    --     Mirror_Indices              : array (1 .. Max_Mirrors) of Positive;
    Prop_Count                    : Natural := 0;
    Mirror_Count                  : Int := 0;
    Live_Mirror_Count             : Int := 0;
    Num_Types_Decap_Heads         : Int := 0;
    Last_Head_Particles_Used      : Integer := 0;
    Prop_Dyn_Light_Pos_Wor        : Singles.Vector3 := Maths.Vec3_0;
    Prop_Dyn_Light_Diff           : Singles.Vector3 := Maths.Vec3_0;
    Prop_Dyn_Light_Spec           : Singles.Vector3 := Maths.Vec3_0;
    Prop_Dyn_Light_Range          : Single := 1.0;
    Prop_Dyn_Light_Dirty          : Boolean := True;
    Prev_Time                     : Single := Single (Glfw.Time);

    function Activate_Elevator (Property_Index : Positive) return Boolean;
    procedure Activate_Property (Property_Index : Positive;
                                 Reactivating   : Boolean);
    procedure Update_Anim_Looped_Prop (Prop_Index : Positive; Seconds : Single);


    --  -------------------------------------------------------------------------
    --  return false if door already opened
    function Activate_Door (Property_Index : Positive) return Boolean is
        Property     : Property_Data := Properties.Element (Property_Index);
        Script_Index : constant Positive := Property.Script_Index;
        Mesh_Index   :  Positive;
        aScript      : constant Prop_Script := Scripts.Element (Script_Index);
        Result       : Boolean := False;
    begin
        Result := Property.Door_Position = aScript.Initial_Door_State;
        if Result then
            if aScript.Initial_Door_State = Open_State then
                Property.Door_Position := Closing_State;
            else
                Property.Door_Position := Opening_State;
            end if;
            Property.Is_Visible := True;
            Mesh_Index := aScript.Mesh_Index;
            Property.Anim_Duration := Mesh_Loader.Animation_Duration (Mesh_Index, 1);
            Property.Anim_Elapsed_Time := 0.0;
            Property.Is_Animating := True;
            Activate_Property (Property_Index, False);
            Properties.Replace_Element (Property_Index, Property);
            Audio.Play_Sound
              (To_String (aScript.Sound_Activate_File_Name), True);
        end if;
        return Result;
    end Activate_Door;

    --  -------------------------------------------------------------------------

    function Activate_Door_In_Tile
      (Map_U, Map_V : Int; Hand_Y_World_Pos : Single;
       Activator : Activator_Type) return Boolean is
        Tile_Props    : Prop_Indices_List;
        Property_I    : Int;
        Property      : Property_Data;
        Script_I      : Integer;
        Script        : Prop_Script;
        Handle_Height : Single;
        Diff          : Single;
        Continue      : Boolean := True;
        Result        : Boolean := False;
    begin
        if not Tiles_Manager.Is_Tile_Valid (Map_U, Map_V) then
            raise Prop_Renderer_Exception with
              "Prop_Renderer.Activate_Door_In_Tile, invalid tile indices: " &
              Int'Image (Map_U) & ", " & Int'Image (Map_V);
        end if;

        Tile_Props := Props_In_Tiles (Integer (Map_U), Integer (Map_V));
        for index in Tile_Props.First_Index .. Tile_Props.Last_Index loop
            if Continue then
                Property_I := Tile_Props.Element (index);
                Property := Properties.Element (Positive (Property_I));
                Script_I := Property.Script_Index;
                Script := Scripts.Element (Script_I);
                if (Script.Script_Type = Door_Prop or
                      Script.Script_Type = Elevator_Prop) and then
                  (Activator /= Prop_Activator_Player_State or
                     Script.Character_Activated) and then
                  (Activator /= Prop_Activator_Npc_State or
                     Script.Npc_Activated) then
                    Handle_Height := 1.0 + Property.World_Pos (GL.Y);
                    Diff := Abs (Handle_Height - Hand_Y_World_Pos);
                    if Diff <= 1.0 and then
                      Script.Script_Type = Elevator_Prop then
                        --  Activate_Elevator returns false if not
                        --  in a state to be activated
                        Result := Activate_Elevator (Positive (Property_I));
                        Continue := False;
                    end if;
                end if;
            end if;
            Result := Activate_Door (Integer (Property_I));
            Continue := False;
        end loop;
        return Result;
    end Activate_Door_In_Tile;

    --  -------------------------------------------------------------------------
    --   return false if not in a state to be activated
    function Activate_Elevator (Property_Index : Positive) return Boolean is
        Property     : Property_Data := Properties.Element (Property_Index);
        Script_Index : constant Positive := Property.Script_Index;
        aScript      : constant Prop_Script := Scripts.Element (Script_Index);
        Result       : Boolean := False;
    begin
        if Property.Elevator = At_Top_State then
            Property.Elevator := Going_Down_State;
            Property.Delay_Countdown := aScript.Elevator_Down_Duration;
            Result := True;
        elsif Property.Elevator = At_Bottom_State then
            Property.Elevator := Going_Up_State;
            Property.Delay_Countdown := aScript.Elevator_Up_Duration;
            Property.Is_Animating := True;
            Result := True;
        end if;
        if Result then
            Property.Is_Visible := True;
            Property.Anim_Elapsed_Time := 0.0;
            Properties.Replace_Element (Property_Index, Property);
            Activate_Property (Property_Index, False);
            Audio.Play_Sound
              (To_String (aScript.Sound_Activate_File_Name), True);
        end if;

        return Result;
    end Activate_Elevator;

    --  -------------------------------------------------------------------------

    procedure Activate_Property (Property_Index : Positive;
                                 Reactivating   : Boolean) is
        use Indicies_Package;
        Property       : Property_Data := Properties.Element (Property_Index);
        Script_Index   : constant Positive := Property.Script_Index;
        aScript        : constant Prop_Script := Scripts.Element (Script_Index);
        Prop_Type      : constant Property_Type := aScript.Script_Type;
        Double_Up      : Boolean := False;
        Active_Curs    : Cursor;
        Break          : Boolean := False;
    begin
        if not Property.Was_Triggered or Reactivating then
            if Prop_Type = Boulder_Prop then
                if not Audio.Start_Boulder_Sound (Property.Boulder_Snd_Idx) then
                    raise Prop_Renderer_Exception with
                      "Prop_Renderer.Activate_Property, push boulder in tile-audio.";
                end if;
            end if;
            Property.Was_Triggered := True;
            if Curr_Active_Props_A then
                Active_Curs := Active_Properties_B.First;
                while Has_Element (Active_Curs) and not Double_Up loop
                    Double_Up := Element (Active_Curs) = Property_Index;
                    Next (Active_Curs);
                end loop;
                if not Double_Up then
                    Active_Properties_B.Append (Property_Index);
                end if;

            else --  Active_Properties_B is current
                Active_Curs := Active_Properties_A.First;
                while Has_Element (Active_Curs) and not Double_Up loop
                    Double_Up := Element (Active_Curs) = Property_Index;
                    Next (Active_Curs);
                end loop;
                if not Double_Up then
                    Active_Properties_A.Append (Property_Index);
                end if;
            end if;
        end if;
    end Activate_Property;

    --  -------------------------------------------------------------------------

    procedure Delete_Script_Data (Script_Index : Positive) is
    begin
        Scripts.Delete (Script_Index);
    end Delete_Script_Data;

    --  -------------------------------------------------------------------------

    function Get_Num_Live_Mirrors return Int is
    begin
        return Live_Mirror_Count;
    end Get_Num_Live_Mirrors;

    --  -------------------------------------------------------------------------

    function Get_Property_Indices (U, V : Positive) return Prop_Indices_List is
    begin
        return Props_In_Tiles (U, V);
    end Get_Property_Indices;

    --  -------------------------------------------------------------------------

    function Get_Property_Index (U, V, Index : Positive) return Positive is
        use Prop_Indices_Package;
        Prop : constant Prop_Indices_List := Props_In_Tiles (U, V);
    begin
        return Positive (Prop.Element (Index));
    end Get_Property_Index;

    --  -------------------------------------------------------------------------

    function Get_Property_Data (Prop_Index : Positive)
                                return Prop_Renderer_Support.Property_Data is
    begin
        return Properties.Element (Prop_Index);
    end Get_Property_Data;

    --  -------------------------------------------------------------------------

    function Get_Script_Data (Script_Index : Positive)
                              return Prop_Renderer_Support.Prop_Script is
    begin
        return Scripts.Element (Script_Index);
    end Get_Script_Data;

    --  -------------------------------------------------------------------------

    function Get_Script_Index (Prop_Index : Positive) return Positive is
        Property : constant Property_Data := Properties.Element (Prop_Index);
    begin
        return Property.Script_Index;
    end Get_Script_Index;

    --  -------------------------------------------------------------------------

    procedure Init is
        Start_Now      : constant Boolean := False;
        Always_Update  : constant Boolean := True;
        Always_Draw    : constant Boolean := False;
    begin
        Game_Utils.Game_Log ("---INIT PROPS---");
        Scripts.Clear;
        Properties.Clear;
        Active_Properties_A.Clear;
        Active_Properties_B.Clear;
        Basic_Render_List.Clear;
        Skinned_Render_List.Clear;
        Jav_Stand_Render_List.Clear;
        Portal_Render_List.Clear;
        Treasure_Render_List.Clear;

        Properties_Shader_Manager.Load_Prop_Shaders;
        Game_Utils.Game_Log ("Prop_Shaders loaded");

        for index in 1 .. Max_Decap_Particles loop
            Head_Particles (index) := Particle_System.Create_Particle_System
              (Head_Particles_File, Start_Now, Always_Update, Always_Draw);
        end loop;

        Pot_Particles := Particle_System.Create_Particle_System
          (Pot_Particles_File, Start_Now, Always_Update, Always_Draw);
        Mirror_Particles := Particle_System.Create_Particle_System
          (Mirror_Particles_File, Start_Now, Always_Update, Always_Draw);
        Dust_Particles := Particle_System.Create_Particle_System
          (Dust_Particles_File, Start_Now, Always_Update, Always_Draw);
        Dust_Particlesb := Particle_System.Create_Particle_System
          (Dust_Particles_File, Start_Now, Always_Update, Always_Draw);
        Dust_Particlesc := Particle_System.Create_Particle_System
          (Dust_Particles_File, Start_Now, Always_Update, Always_Draw);
        Splash_Particles := Particle_System.Create_Particle_System
          (Splash_Particles_File, Start_Now, Always_Update, Always_Draw);

        Game_Utils.Game_Log ("---PROPS INITIALIZED---");
    end Init;

    --  -------------------------------------------------------------------------

    procedure Launch_Decap_Head (LHL_Type  : Positive;
                                 World_Pos : Singles.Vector3) is
        T_Index    : constant Positive := Last_Head_Launched (LHL_Type + 1) mod
          Max_Active_Decaps_Per_Type;
        Prop_Index : Positive;
        Prop       : Property_Data;
        Sys_Index  : Positive;
        Vel_X      : constant Single := Abs (Maths.Random_Float) * 7.0 - 3.5;
        Vel_Z      : constant Single := Abs (Maths.Random_Float) * 7.0 - 3.5;
        U          : Int;
        V          : Int;
    begin
        Last_Head_Launched (LHL_Type) := T_Index;
        Prop_Index := Decap_Heads_Prop_Index (LHL_Type, T_Index);
        if Prop_Index > Prop_Count then
            raise Prop_Renderer_Exception;
        end if;
        Prop.Bounce_Count := 0;
        Prop.Model_Matrix := Maths.Translation_Matrix (World_Pos);
        Prop.Quat := GL_Maths.Quat_From_Axis_Radian (0.0, 0.0, 1.0, 0.0);
        Prop.Velocity := (Vel_X, 10.0, Vel_Z);
        U := Int (0.5 * (World_Pos (GL.X) + 1.0));
        V := Int (0.5 * (World_Pos (GL.Z) + 1.0));
        Prop.Map_U := U;
        Prop.Map_V := V;
        Last_Head_Particles_Used := (Last_Head_Particles_Used + 1) mod
          Max_Decap_Particles + 1;
        Sys_Index := Head_Particles (Last_Head_Particles_Used);
        Particle_System.Set_Particle_System_Position (Sys_Index, World_Pos);
        Particle_System.Set_Particle_System_Heading (Sys_Index, 0.0);
        Particle_System.Start_Particle_System (Sys_Index);
        Prop.Particle_System_Index := Sys_Index;
        Prop.Was_Triggered := False;
        Prop.Is_On_Ground := False;
        Prop.Is_Visible := True;
        Props_In_Tiles (Integer (U), Integer (V)).Append (Int (Prop_Index));

        Properties.Replace_Element (Prop_Index, Prop);
        Activate_Property (Prop_Index, True);
        Audio.Play_Sound (Decap_Sound_File, True);

    end Launch_Decap_Head;

    --  -------------------------------------------------------------------------

    procedure Render_Property (Prop_ID : Positive) is
        use GL.Culling;
        use Prop_Indices_Package;
        use Properties_Shader_Manager;
        Curr_Time    : Single;
        Elapsed_Time : Single;
        Property     : Property_Data := Properties.Element (Prop_ID);
        Script_ID    : Positive := Property.Script_Index;
        aScript      : Prop_Script := Scripts.Element (Script_ID);
        SSI          : constant Natural := aScript.Smashed_Script_Index;
        Mesh_Index   : Integer;
        Bone_Count   : Integer;
        Prop_Type    : Property_Type;

    begin
        Prev_Time := Single (Glfw.Time);
        Curr_Time := Single (Glfw.Time);
        Elapsed_Time := Curr_Time - Prev_Time;
        if Property.Was_Smashed and SSI > 0 then
            Script_ID := Ssi;
        end if;
        aScript := Scripts.Element (Script_ID);
        Mesh_Index := aScript.Mesh_Index;
        Bone_Count := Mesh_Loader.Bone_Count (Mesh_Index);
        Prop_Type := aScript.Script_Type;
        if Prop_Type = Anim_Loop_Prop then
            Update_Anim_Looped_Prop (Prop_ID, Elapsed_Time);
        end if;

        GL_Utils.Bind_VAO (aScript.Vao);
        Texture_Manager.Bind_Texture (0, aScript.Diffuse_Map_Id);
        Texture_Manager.Bind_Texture (1, aScript.Specular_Map_Id);
        Texture_Manager.Bind_Texture (2, aScript.Normal_Map_Id);

        Set_Shaders (Property, Prop_Type, aScript,
                     Character_Controller.Gold_Current, Elapsed_Time);
        Properties.Replace_Element (Prop_ID, Property);

        if Settings.Render_OLS and aScript.Draw_Outlines then
            Set_Outline_Shaders (Prop_Type, aScript);
        end if;
        GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, aScript.Vertex_Count);

    end Render_Property;

    --  -------------------------------------------------------------------------

    function Push_Boulder_In_Tile (Map_U, Map_V : Int;
                                   Hand_Y_World_Pos : Single) return Boolean is
        Prop_IDs   : Prop_Indices_List;
        Prop_ID    : Integer;
        Property   : Property_Data;
        Script_ID  : Positive;
        Script     : Prop_Script;
        Dist       : Single;
        Result     : Boolean := False;
    begin
        if not Tiles_Manager.Is_Tile_Valid (Map_U, Map_V) then
            raise Prop_Renderer_Exception with
              "Prop_Renderer.Activate_Door_In_Tile, invalid tile indices: " &
              Int'Image (Map_U) & ", " & Int'Image (Map_V);
        end if;

        Prop_IDs := Props_In_Tiles (Integer (Map_U), Integer (Map_V));
        for index in Prop_IDs.First_Index .. Prop_IDs.Last_Index  loop
            if not Result then
                Prop_ID := Integer (Prop_IDs.Element (index));
                Property := Properties.Element (Prop_ID);
                Script_ID := Property.Script_Index;
                Script := Scripts.Element (Script_ID);
                if Script.Script_Type = Boulder_Prop then
                    Dist := Property.World_Pos (GL.Y) - Hand_Y_World_Pos;
                    Result := Abs (Dist) < 2.0;
                    if Result then
                        Activate_Property (Prop_ID, False);
                    end if;
                end if;
            end if;
        end loop;
        return Result;
    end Push_Boulder_In_Tile;

    --  -------------------------------------------------------------------------

    procedure Render_Props_Around_Depth_Only (U, V, Tiles_Distance : Int) is
        use Prop_Indices_Package;
        use GL.Objects.Vertex_Arrays;
        use GL.Toggles;
        use Maths;
        use Single_Math_Functions;
        use Singles;
        Left         : constant Int := Maths.Max_Int (0, V - Tiles_Distance) + 1;
        Right        : constant Int := Maths.Min_Int (Batch_Manager.Max_Cols - 1,
                                                      V + Tiles_Distance) + 1;
        Up           : constant Int := Maths.Max_Int (0, U - Tiles_Distance) + 1;
        Down         : constant Int := Maths.Min_Int (Batch_Manager.Max_Rows - 1,
                                                      U + Tiles_Distance) + 1;
        --  Diamond Bob
        Curr_Time    : constant Single := Single (Glfw.Time);
        Elapsed      : constant Single := Curr_Time - Prev_Time;
        Hdg_Dia      : constant Single := 20.0 * Elapsed;
        Hgt_Dia      : constant Single := 0.5 * Sin (2.0 * Curr_Time);
        Tile_Data    : Props_In_Tiles_Array;
        Prop_Indices : Prop_Indices_List;
        Property     : Property_Data;
        --        Props_Size   : Integer;
        Script_Index : Integer;
        Script_Type  : Property_Type;
        aScript      : Prop_Script;
        Ssi          : Natural;
        Mesh_Index   : Integer;
        Bone_Count   : Integer;
        Rot_Dia      : Singles.Matrix4;
        Trans_Dia    : Singles.Matrix4;
        Trans        : Singles.Vector3;
    begin
        Prev_Time := Curr_Time;
        Enable (Depth_Test);
        --        Game_Utils.Game_Log ("Prop_Renderer.Render_Props_Around_Depth_Only Left, Right: "
        --         & Int'Image (Left) & ", " & Int'Image (Right));
        --        Game_Utils.Game_Log ("Prop_Renderer.Render_Props_Around_Depth_Only Up, Down: "
        --         & Int'Image (Up) & ", " & Int'Image (Down));
        for vi in Left .. Right loop
            for ui in Up .. Down loop
                --                 Props_Size := Tile_Data'Size;
                Prop_Indices := Tile_Data (Integer (ui), Integer (vi));
                for Props_Index in Prop_Indices.First_Index .. Prop_Indices.Last_Index loop
                    Property := Properties.Element (Props_Index);
                    Script_Index := Property.Script_Index;
                    aScript := Scripts.Element (Script_Index);
                    Ssi := aScript.Smashed_Script_Index;
                    if Property.Was_Smashed and Ssi > 0 then
                        Script_Index := Ssi;
                    end if;

                    if (Property.Is_Visible or GL_Utils.Is_Edit_Mode) and
                      aScript.Casts_Shadow and not aScript.Uses_Sprite and
                      not aScript.Transparent and
                      Frustum.Is_Sphere_In_Frustum
                        (Property.Origin_World, aScript.Bounding_Radius) then
                        GL_Utils.Bind_VAO (aScript.Vao);
                        Script_Type := aScript.Script_Type;
                        if Script_Type = Door_Prop or
                          Script_Type = Pillar_Prop or
                          Script_Type = Anim_Loop_Prop or
                          Script_Type = Windlass_Prop then
                            Shadows.Set_Depth_Skinned_Model_Matrix
                              (Property.Model_Matrix);
                            Mesh_Index := aScript.Mesh_Index;
                            Bone_Count := Mesh_Loader.Bone_Count (Mesh_Index);
                            Shadows.Set_Depth_Skinned_Bone_Matrices
                              (Property.Current_Bone_Transforms);
                        elsif Script_Type = Diamond_Trigger_Prop then
                            Rot_Dia := Rotate_Y_Degree
                              (Singles.Identity4, Degree (Hdg_Dia) +
                                   Property.Heading_Deg);
                            Trans := Property.World_Pos;
                            Trans (GL.Y) := Trans (GL.Y)  + Hdg_Dia;
                            Trans_Dia := Translation_Matrix (Trans);
                            Shadows.Set_Depth_Model_Matrix (Trans_Dia * Rot_Dia);
                        else
                            Shadows.Set_Depth_Model_Matrix (Property.Model_Matrix);
                        end if;
                        Draw_Arrays (Triangles, 0, aScript.Vertex_Count);
                    end if;
                end loop;
            end loop;
        end loop;
    end Render_Props_Around_Depth_Only;

    --  -------------------------------------------------------------------------

    procedure Render_Props_Around_Split (U, V, Tiles_Distance : Int) is
        use Prop_Indices_Package;
        use GL.Objects.Vertex_Arrays;
        use GL.Toggles;
        use Maths;
        use Single_Math_Functions;
        use Singles;
        use Prop_Renderer.Render;
        use Transparency;
        Left         : constant Int := Maths.Max_Int (0, V - Tiles_Distance) + 1;
        Right        : constant Int := Maths.Min_Int (Batch_Manager.Max_Cols - 1,
                                                      V + Tiles_Distance) + 1;
        Up           : constant Int := Maths.Max_Int (0, U - Tiles_Distance) + 1;
        Down         : constant Int := Maths.Min_Int (Batch_Manager.Max_Rows - 1,
                                                      U + Tiles_Distance) + 1;
        Curr_Time    : constant Single := Single (Glfw.Time);
        Elapsed      : constant Single := Curr_Time - Prev_Time;
        Hdg_Dia      : constant Single := 20.0 * Elapsed;
        Hgt_Dia      : constant Single := 0.5 * Sin (2.0 * Curr_Time);
        Tile_Data    : Props_In_Tiles_Array;
        Prop_Indices : Prop_Indices_List;
        Property     : Property_Data;
        Script_Index : Integer;
        Prop_Type    : Property_Type;
        aScript      : Prop_Script;
        Ssi          : Integer;
        Prop_Size    : Integer;
        Sprite_Time  : Float;
        Curr_Sprite  : Positive;
    begin
        Basic_Render_List.Clear;
        Skinned_Render_List.Clear;
        Jav_Stand_Render_List.Clear;
        Portal_Render_List.Clear;
        Treasure_Render_List.Clear;

        Prev_Time := Curr_Time;
        Enable (Depth_Test);
        for vi in Left .. Right loop
            for ui in Up .. Down loop
                Prop_Indices := Tile_Data (Integer (ui), Integer (vi));
                for Props_Index in Prop_Indices.First_Index .. Prop_Indices.Last_Index loop
                    Property := Properties.Element (Props_Index);
                    Script_Index := Property.Script_Index;
                    aScript := Scripts.Element (Script_Index);
                    Ssi := aScript.Smashed_Script_Index;
                    if Property.Was_Smashed and Ssi >= 0 then
                        Script_Index := Ssi;
                    end if;
                    Prop_Type := aScript.Script_Type;

                    if Property.Is_Visible or GL_Utils.Is_Edit_Mode then
                        if aScript.Uses_Sprite then
                            Prop_Size := aScript.Sprite_Map_Rows *
                              aScript.Sprite_Map_Cols;
                            Sprite_Time := aScript.Sprite_Timer * Float (Prop_Size);
                            Property.Sprite_Duration := Property.Sprite_Duration +
                              Float (Elapsed);
                            if Property.Sprite_Duration > Sprite_Time then
                                Property.Sprite_Duration := Property.Sprite_Duration -
                                  Sprite_Time;
                            end if;

                            Curr_Sprite := Positive (Property.Sprite_Duration /
                                                       aScript.Sprite_Timer);
                            Sprite_Renderer.Set_Sprite_Current_Sprite
                              (Property.Sprite_Index, Curr_Sprite);
                            Properties.Replace_Element (Props_Index, Property);

                        elsif Frustum.Is_Sphere_In_Frustum
                          (Property.Origin_World, aScript.Bounding_Radius) and
                          aScript.Transparent then
                            Add_Transparency_Item
                              (Tr_Prop, Props_Index, Property.Origin_World,
                               aScript.Bounding_Radius);
                        else
                            if Prop_Type = Anim_Loop_Prop then
                                Update_Anim_Looped_Prop (Props_Index, Elapsed);
                            end if;
                            case Prop_Type is
                            when Door_Prop | Pillar_Prop | Anim_Loop_Prop |
                                 Windlass_Prop =>
                                Skinned_Render_List.Append (Props_Index);
                            when Elevator_Prop =>
                                Basic_Render_List.Append (Props_Index);
                            when Jav_Stand_Prop | Tavern_Prop |
                                 Diamond_Trigger_Prop =>
                                Jav_Stand_Render_List.Append (Props_Index);
                            when Portal_Prop =>
                                Portal_Render_List.Append (Props_Index);
                            when Treasure_Prop | Hammer_Prop | Food_Prop =>
                                Treasure_Render_List.Append (Props_Index);
                            when others =>
                                Basic_Render_List.Append (Props_Index);
                            end case;
                        end if;
                    end if;
                end loop;  --  end for p_in_t_i
            end loop;  --  end for ui
        end loop;  --  end for vi

        Render_Basic
          (Prop_Dyn_Light_Pos_Wor, Prop_Dyn_Light_Diff, Prop_Dyn_Light_Spec,
           Prop_Dyn_Light_Range, Prop_Dyn_Light_Dirty);
       Render_Skinned
          (Prop_Dyn_Light_Pos_Wor, Prop_Dyn_Light_Diff, Prop_Dyn_Light_Spec,
           Prop_Dyn_Light_Range, Prop_Dyn_Light_Dirty);
        Render_Javelin_Standard;
        Render_Portal;
        Render_Treasure;
        Prop_Dyn_Light_Dirty := False;

   exception
            when others =>
                Put_Line ("Prop_Renderer.Game_Support.Render_Props_Around_Split exception");
                raise;
    end Render_Props_Around_Split;

    --  -------------------------------------------------------------------------

    procedure Reset_Properties is
        use Prop_Indices_Package;
        -- Particle Systems
        Start_Now      : constant Boolean := False;
        Always_Update  : constant Boolean := True;
        Always_Draw    : constant Boolean := False;
        Prop_Indices   : Prop_Indices_List;
    begin
        for row in Props_In_Tiles'Range loop
            for col in Props_In_Tiles'Range (2) loop
                Props_In_Tiles (Integer (row), Integer (col)).Clear;
            end loop;
        end loop;

        Active_Properties_A.Clear;
        Active_Properties_B.Clear;
        Basic_Render_List.Clear;
        Skinned_Render_List.Clear;
        Jav_Stand_Render_List.Clear;
        Portal_Render_List.Clear;
        Treasure_Render_List.Clear;

        Prop_Count := 0;
        Num_Types_Decap_Heads := 0;
        Last_Head_Particles_Used := 0;
        Mirror_Count := 0;
        Live_Mirror_Count := 0;

        for index in 1 .. Max_Decap_Particles loop
            Head_Particles (index) := Particle_System.Create_Particle_System
              (Head_Particles_File, Start_Now, Always_Update, Always_Draw);
        end loop;

        Pot_Particles := Particle_System.Create_Particle_System
          (Pot_Particles_File, Start_Now, Always_Update, Always_Draw);
        Mirror_Particles := Particle_System.Create_Particle_System
          (Mirror_Particles_File, Start_Now, Always_Update, Always_Draw);
        Dust_Particles := Particle_System.Create_Particle_System
          (Dust_Particles_File, Start_Now, Always_Update, Always_Draw);
        Dust_Particlesb := Particle_System.Create_Particle_System
          (Dust_Particles_File, Start_Now, Always_Update, Always_Draw);
        Dust_Particlesc := Particle_System.Create_Particle_System
          (Dust_Particles_File, Start_Now, Always_Update, Always_Draw);
        Splash_Particles := Particle_System.Create_Particle_System
          (Splash_Particles_File, Start_Now, Always_Update, Always_Draw);

    end Reset_Properties;

    --  -------------------------------------------------------------------------

    procedure Set_Ambient_Light_Level (Level : Singles.Vector3) is
    begin
        GL.Objects.Programs.Use_Program (Properties_Shader_Manager.Prop_Shader);
        Properties_Shader_Manager.Set_L_A (Level);
        Properties_Skinned_Shader_Manager.Set_L_A (Level);
    end Set_Ambient_Light_Level;

    --  -------------------------------------------------------------------------

    procedure Splash_Particles_At (Pos : Singles.Vector3) is
    begin
        Audio.Play_Sound (Splash_Sound_File, True);
        Particle_System.Set_Particle_System_Position (Splash_Particles, Pos);
        Particle_System.Start_Particle_System (Splash_Particles);
    end Splash_Particles_At;

    --  -------------------------------------------------------------------------

    procedure Update_Anim_Looped_Prop (Prop_Index : Positive; Seconds : Single) is
    begin
        null;
    end Update_Anim_Looped_Prop;

    --  -------------------------------------------------------------------------

    procedure Update_Dart_Trap (Prop_Index : Positive; Seconds : Float) is
    begin
        null;
    end Update_Dart_Trap;

    --  -------------------------------------------------------------------------

    procedure Update_Door (Prop_Index : Positive; Seconds : Float) is
    begin
        null;
    end Update_Door;

    --  -------------------------------------------------------------------------

    procedure Update_Dynamic_Lights (World_Pos, Diff, Specular : Singles.Vector3;
                                     Dist : Single) is
    begin
        Prop_Dyn_Light_Pos_Wor := World_Pos;
        Prop_Dyn_Light_Diff := Diff;
        Prop_Dyn_Light_Spec := Specular;
        Prop_Dyn_Light_Range := Dist;
    end Update_Dynamic_Lights;

    --  -------------------------------------------------------------------------

    procedure Update_Elevator (Prop_Index : Positive; Seconds : Float) is
    begin
        null;
    end Update_Elevator;

    --  -------------------------------------------------------------------------

    procedure Update_Pillar (Prop_Index : Positive; Seconds : Float) is
    begin
        null;
    end Update_Pillar;

    --  -------------------------------------------------------------------------

    procedure Update_Decap_Head (Prop_Index : Positive; Seconds : Float) is
    begin
        null;
    end Update_Decap_Head;

    --  -------------------------------------------------------------------------

    procedure Update_Windlass (Prop_Index : Positive; Seconds : Float) is
    begin
        null;
    end Update_Windlass;

    --  -------------------------------------------------------------------------

    procedure Update_Properties (Seconds : Float) is

        procedure Update (Props : Indicies_List) is
            P_Index     : Positive;
            S_Index     : Positive;
            SS_Index    : Positive;
            Property    : Property_Data;
            aScript     : Prop_Script;
        begin
            for Index in Props.First_Index .. Props.Last_Index loop
                P_Index := Element (Props, Index);
                Property := Element (Properties, P_Index);
                S_Index := Property.Script_Index;
                aScript := Element (Scripts, S_Index);
                SS_Index := aScript.Smashed_Script_Index;
                if Property.Was_Smashed and SS_Index > 0 then
                    S_Index := SS_Index;
                end if;

                case aScript.Script_Type is
                when Boulder_Prop => Prop_Renderer.Boulder.Update_Boulder
                      (P_Index, aScript, Seconds);
                when Dart_Trap_Prop => Update_Dart_Trap (P_Index, Seconds);
                when Door_Prop => Update_Door (P_Index, Seconds);
                when Elevator_Prop => Update_Elevator (P_Index, Seconds);
                when Pillar_Prop => Update_Pillar (P_Index, Seconds);
                when Decap_Head_Prop => Update_Decap_Head (P_Index, Seconds);
                when Windlass_Prop => Update_Windlass (P_Index, Seconds);
                when others =>
                    raise Prop_Renderer_Exception with
                      ("Prop_Renderer.Update_Properties, unhandled property type.");
                end case;
            end loop;
        end Update;

    begin
        --  Swap lists to use/build around
        if Curr_Active_Props_A then
            Update (Active_Properties_A);
            Active_Properties_A.Clear;
        else
            Update (Active_Properties_B);
            Active_Properties_B.Clear;
        end if;

        Curr_Active_Props_A := not Curr_Active_Props_A;

    end Update_Properties;

    --  -------------------------------------------------------------------------

    procedure Update_Static_Lights_Uniforms is
        use Batch_Manager;
        use GL_Maths.Indices_Package;
        use Properties_Shader_Manager;
        Index     : Positive := Static_Lights.First_Index;
        aLight    : Static_Light_Data;
        Positions : Light_Array;
        Diffuse   : Light_Array;
        Specular  : Light_Array;
        Ranges    : Light_Range_Array;
    begin
        while Index <= Static_Lights.Last_Index loop
            aLight := Element (Static_Lights, Index);
            Positions (Int (Index)) := aLight.Position;
            Diffuse (Int (Index)) := aLight.Diffuse;
            Specular (Int (Index)) := aLight.Specular;
            Ranges (Int (Index)) := aLight.Light_Range;
            Index := Index + 1;
        end loop;

        GL.Objects.Programs.Use_Program (Properties_Shader_Manager.Prop_Shader);
        Properties_Shader_Manager.Set_Light_Pos (Positions);
        Properties_Shader_Manager.Set_Light_Diff (Diffuse);
        Properties_Shader_Manager.Set_Light_Spec (Specular);
        Properties_Shader_Manager.Set_Light_Range (Ranges);
        GL.Objects.Programs.Use_Program
          (Properties_Shader_Manager.Prop_Skinned_Shader);
        Properties_Skinned_Shader_Manager.Set_Light_Diff (Diffuse);
        Properties_Skinned_Shader_Manager.Set_Light_Pos (Positions);
        Properties_Skinned_Shader_Manager.Set_Light_Spec (Specular);
        Properties_Skinned_Shader_Manager.Set_Light_Range (Ranges);
    end Update_Static_Lights_Uniforms;

    --  ----------------------------------------------------------------------------

end Prop_Renderer;
