
with Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Culling;
with GL.Objects.Programs;
with GL.Toggles;

with Utilities;

with Audio;
with Batch_Manager;
with Camera;
with Character_Controller;
with Coins_Shader_Manager;
with Depth_Skinned_Shader_Manager;
with Event_Controller;
with FB_Effects;
with Frustum;
with Game_Utils;
with GL_Utils;
with GUI_Level_Chooser;
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
with Text;
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

   End_Camera_Matrix     : Singles.Matrix4 := Singles.Identity4;
   End_Camera_Position   : Singles.Vector3 := (0.0, 0.0, 0.0);
   Head_Particles_File   : constant String := "blood_artery_jet.particles";
   Splash_Particles_File : constant String := "splash.particles";
   Dust_Particles_File   : constant String := "dust.particles";
   Pot_Particles_File    : constant String := "pot.particles";
   Mirror_Particles_File : constant String := "mirror.particles";

   Max_Scene_Lamp_Locs        : constant integer := 3;
   --  Up To 32 Types, With 8 Active Ones Of Each Type

   Portal_Index               : Natural := 0;

   function Activate_Elevator (Property_Index : Positive) return Boolean;
   procedure Update_Anim_Looped_Prop (Prop_Index : Positive; Seconds : Single);
   procedure Update_Windlass (Prop_Index : Positive; Seconds : Float);

   --  -------------------------------------------------------------------------
   --  return false if door already opened
   function Activate_Door (Property_Index : Positive) return Boolean is
      Property     : Property_Data :=
                       Properties_Manager.Get_Property_Data (Property_Index);
      Script_Index : constant Positive := Property.Script_Index;
      Mesh_Index   :  Positive;
      aScript      : constant Prop_Script :=
                       Properties_Manager.Get_Script_Data (Script_Index);
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
         --              Properties.Replace_Element (Property_Index, Property);
         Audio.Play_Sound
           (To_String (aScript.Sound_Activate_File_Name), True);
      end if;
      return Result;
   end Activate_Door;

   --  -------------------------------------------------------------------------

   function Activate_Door_In_Tile
     (Map_U, Map_V : Int; Hand_Y_World_Pos : Single;
      Activator    : Properties_Manager.Activator_Type) return Boolean is
      use Properties_Manager;
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
      if not Tiles_Manager.Is_Tile_Valid ((Map_U, Map_V)) then
         raise Prop_Renderer_Exception with
           "Prop_Renderer.Activate_Door_In_Tile, invalid tile indices: " &
           Int'Image (Map_U) & ", " & Int'Image (Map_V);
      end if;

      Tile_Props := Props_In_Tiles (Integer (Map_U), Integer (Map_V));
      for index in Tile_Props.First_Index .. Tile_Props.Last_Index loop
         if Continue then
            Property_I := Tile_Props.Element (index);
            Property := Get_Property_Data (Positive (Property_I));
            Script_I := Property.Script_Index;
            Script := Properties_Manager.Get_Script_Data (Script_I);
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
      Property     : Property_Data :=
                       Properties_Manager.Get_Property_Data (Property_Index);
      Script_Index : constant Positive := Property.Script_Index;
      aScript      : constant Prop_Script :=
                       Properties_Manager.Get_Script_Data (Script_Index);
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
         Properties_Manager.Replace_Property (Property_Index, Property);
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
      Property       : Property_Data :=
                         Properties_Manager.Get_Property_Data (Property_Index);
      Script_Index   : constant Positive := Property.Script_Index;
      aScript        : constant Prop_Script :=
                         Properties_Manager.Get_Script_Data (Script_Index);
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

   function Check_Tile_For_Property
     (Map : Ints.Vector2; Prop_Type : Prop_Renderer_Support.Property_Type)
      return Boolean is
      use Prop_Indices_Package;
      Prop_Indices : Prop_Indices_List;
      Property     : Property_Data;
      aScript      : Prop_Script;
      P_Index      : Positive;
      Index        : Positive;
      Result       : Boolean := False;
   begin
      if not Tiles_Manager.Is_Tile_Valid (Map) then
         Utilities.Print_Vector
           ("Prop_Renderer.Check_Tile_For_Property, invalid tile indices: ",
            Map);
         raise Prop_Renderer_Exception;
      end if;
      Prop_Indices :=
        Props_In_Tiles (Integer (Map (GL.X)), Integer (Map (GL.Y)));
      if not Prop_Indices.Is_Empty then
         Index := Prop_Indices.First_Index;
         while index <= Prop_Indices.Last_Index and not Result loop
            P_Index := Positive (Prop_Indices.Element (index));
            Property :=
              Properties_Manager.Get_Property_Data (P_Index);
            aScript :=
              Properties_Manager.Get_Script_Data (Property.Script_Index);
            Result := aScript.Script_Type = Prop_Type;
            if Index /= Prop_Indices.Last_Index then
               Index := Index + 1;
            end if;
         end loop;
      end if;
      return Result;
   end Check_Tile_For_Property;

   --  ------------------------------------------------------------------------

   function Get_End_Camera_Position return Singles.Vector3 is
   begin
      return End_Camera_Position;
   end Get_End_Camera_Position;

   --  -------------------------------------------------------------------------

   function Get_End_Camera_Matrix return Singles.Matrix4 is
   begin
      return End_Camera_Matrix;
   end Get_End_Camera_Matrix;

   --  -------------------------------------------------------------------------

   function Get_Num_Live_Mirrors return Int is
   begin
      return Live_Mirror_Count;
   end Get_Num_Live_Mirrors;

   --  -------------------------------------------------------------------------

   function Get_Prop_Height (theProperty : Property_Data;
                             NW, SE      : Singles.Vector3) return Single is
      use Singles;
      use Maths;
      Script_Index    : constant Positive := theProperty.Script_Index;
      aScript         : Prop_Script :=
                          Properties_Manager.Get_Script_Data (Script_Index);
      Prop_Type       : constant Property_Type := aScript.Script_Type;
      Prop_Centre     : constant Vector3 := theProperty.World_Pos;
      Query_Centre    : constant Vector3 := NW + 0.5 * (SE - NW);
      Distance        : constant Vector3 :=
                          theProperty.World_Pos - Query_Centre;
      Sq_Dist         : Single;
      Radius          : Single;
      RP              : Vector4_Array (1 .. 4);
      Min_X           : Single := 0.0;
      Max_X           : Single := 0.0;
      Max_Y           : Single := 0.0;
      Min_Z           : Single := 0.0;
      Max_Z           : Single := 0.0;
      P               : Singles.Vector4;
      F               : Single;
      Continue        : Boolean := False;
      Height          : Single;
   begin
      if theProperty.Was_Smashed then
         aScript := Properties_Manager.Get_Script_Data
           (aScript.Smashed_Script_Index);
      end if;

      Sq_Dist := Distance (GL.X) ** 2 +  Distance (GL.Z) ** 2;
      Radius := aScript.Radius;
      if Radius > 0.0 and Sq_Dist > Radius ** 2 then
         Height := -100.0;
      else
         Height := aScript.Height;
         if aScript.Has_Hole then
            for index in Int range 1 .. 4 loop
               P := (aScript.Hole_Points (index) (GL.X), 0.0,
                     aScript.Hole_Points (index) (GL.Y), 1.0);
               RP (index) := theProperty.Model_Matrix * P;
               if index = 1 then
                  Min_X := RP (index) (GL.X);
                  Max_X := RP (index) (GL.X);
                  Min_Z := RP (index) (GL.Z);
                  Max_Z := RP (index) (GL.Z);
               else
                  Min_X := Maths.Min (Min_X, RP (index) (GL.X));
                  Max_X := Maths.Max (Max_X, RP (index) (GL.X));
                  Min_Z := Maths.Min (Min_Z, RP (index) (GL.Z));
                  Max_Z := Maths.Max (Max_Z, RP (index) (GL.Z));
               end if;
            end loop;

            Max_Y := aScript.Hole_Height + Prop_Centre (GL.Y);
            if NW (GL.X) >= Min_X and SE (GL.X) <= Max_X and
              NW (GL.Y) <= Max_Y and
              NW (GL.Z) >= Min_Z and SE (GL.Z) <= Max_Z then
               Height  := -100.0;
               Continue := False;
            end if;
         end if;  -- Has_Hole
      end if;  --  Radius

      if Continue then
         case Prop_Type is
            when Boulder_Prop =>
               F := 0.5 * Ada.Numerics.pi * Sq_Dist / Radius ** 2;
               Height := Radius * Maths.Single_Math_Functions.Cos (F) +
                 Prop_Centre (GL.Y);
               Continue := False;
            when Pillar_Prop =>
               if theProperty.Door_Position = Closed_State then
                  Height := Height + Prop_Centre (GL.Y);
               else
                  Height := Prop_Centre (GL.Y);
               end if;
               Continue := False;
            when Door_Prop | Elevator_Prop | Box_Prop | Mirror_Prop |
                 Windlass_Prop | Big_Box_Prop =>
               if Prop_Type = Door_Prop and then
                 theProperty.Door_Position = Open_State then
                  Height  := -100.0;
                  Continue := False;
               else
                  Min_X := 0.0;
                  Max_X := 0.0;
                  Min_Z := 0.0;
                  Max_Z := 0.0;
                  for index in Int range 1 .. 4 loop
                     P := (aScript.Hole_Points (index) (GL.X), 0.0,
                           aScript.Hole_Points (index) (GL.Y), 1.0);
                     if index = 1 then
                        Min_X := RP (index) (GL.X);
                        Max_X := RP (index) (GL.X);
                        Min_Z := RP (index) (GL.Z);
                        Max_Z := RP (index) (GL.Z);
                     else
                        Min_X := Maths.Min (Min_X, RP (index) (GL.X));
                        Max_X := Maths.Max (Max_X, RP (index) (GL.X));
                        Min_Z := Maths.Min (Min_Z, RP (index) (GL.Z));
                        Max_Z := Maths.Max (Max_Z, RP (index) (GL.Z));
                     end if;
                  end loop;
                  if Min_X > Max (NW (GL.X), SE (GL.X)) or
                    Max_X < Min (NW (GL.X), SE (GL.X)) or
                    Min_X > Max (NW (GL.Z), SE (GL.Z)) or
                    Max_X < Min (NW (GL.Z), SE (GL.Z)) then
                     Height  := -100.0;
                  else
                     Height := Height + Prop_Centre (GL.Y);
                  end if;
                  Continue := False;
               end if;
            when Bridge_Prop =>
               Height := theProperty.World_Pos (GL.Y);
               Continue := False;
            when Decap_Head_Prop | Tavern_Prop | Jav_Stand_Prop =>
               Height  := -100.0;
               Continue := False;
            when others => null;
         end case;
         if Radius <= 0.0 then
            Height := -100.0;
            Continue := False;
         end if;
      end if;

      if Continue then
         Height := Prop_Centre (GL.Y) + aScript.Height;
      end if;

      return Height;

   end Get_Prop_Height;

   --  -------------------------------------------------------------------------

   function Get_Prop_Height_Between (NW, SE : Singles.Vector3) return Single is
      use Singles;
      use Maths;
      use Properties_Manager;
      use Prop_Indices_Package;

      NW_U            : constant Integer := Integer (0.5 * (NW (GL.X) + 1.0));
      NW_V            : constant Integer := Integer (0.5 * (NW (GL.Z) + 1.0));
      SE_U            : constant Integer := Integer (0.5 * (SE (GL.X) + 1.0));
      SE_V            : constant Integer := Integer (0.5 * (SE (GL.Z) + 1.0));
      Left            : constant Integer := Min_Integer (NW_U, SE_U);
      Right           : constant Integer := Max_Integer (NW_U, SE_U);
      Top             : constant Integer := Min_Integer (NW_V, SE_V);
      Bottom          : constant Integer := Max_Integer (NW_V, SE_V);
      Head            : constant Single := Max (NW (GL.Y), SE (GL.Y));
      Max_Tile_Radius : constant Integer := 2;
      Up_Bound        : constant Integer :=
                          Max_Integer (Top - Max_Tile_Radius, 0);
      Down_Bound      : constant Integer :=
                          Min_Integer (Bottom + Max_Tile_Radius,
                                       Manifold.Max_Tile_Cols - 1);
      Left_Bound      : constant Integer :=
                          Max_Integer (Left - Max_Tile_Radius, 0);
      Right_Bound     : constant Integer :=
                          Max_Integer (Right + Max_Tile_Radius,
                                       Manifold.Max_Tile_Cols - 1);
      Prop_Indices    : Prop_Indices_List;
      Prop_Index      : Positive;
      aProp           : Property_Data;
      Script_Index    : Positive;
      aScript         : Prop_Script;
      Prop_Type       : Property_Type;
      Prop_Radius     : Single;
      Centre_Point    : Singles.Vector3;
      Dist            : Singles.Vector3;
      Sqdist          : Single;
      Prop_Sqrad      : Single;
      Zone_Rad        : Single;
      Zone_Sqrad      : Single;
      Continue        : Boolean := True;
      Height          : Single := -100.0;
   begin
      for v in Integer range Up_Bound .. Down_Bound loop
         for h in Integer range Left_Bound .. Right_Bound loop
            Prop_Indices := Props_In_Tiles (h, v);
            for index in Prop_Indices.First_Index ..
              Prop_Indices.Last_Index loop
               Prop_Index := Positive (Prop_Indices.Element (index));
               aProp := Get_Property_Data (Prop_Index);
               Script_Index := aProp.Script_Index;
               aScript := Get_Script_Data (Script_Index);
               if aProp.Was_Smashed then
                  Script_Index := aScript.Smashed_Script_Index;
                  aScript := Get_Script_Data (Script_Index);
               end if;
               Prop_Type := aScript.Script_Type;
               if Prop_Type /= Pot_Prop and Prop_Type /= Door_Prop and
                 Prop_Type /= Pillar_Prop then
                  if Prop_Type /= Big_Box_Prop then
                     Continue := v >= Top and v <= Bottom and
                       h >= Left and h <= Right;
                  end if;
                  Continue := Continue and
                    Head >= aProp.World_Pos (GL.Y);
                  if Continue then
                     Prop_Radius := aScript.Radius;
                     if Prop_Radius > 0.0 then
                        Centre_Point := 0.5 * (NW + SE);
                        Dist := Centre_Point - aProp.Origin_World;
                        Sqdist := Dist (GL.X) ** 2 + Dist (GL.Z) ** 2;
                        Prop_Sqrad := Prop_Radius * Prop_Radius;
                        --   also approximate box to a cylinder (circle)
                        Zone_Rad := Centre_Point (GL.X) - SE (GL.X);
                        Zone_Sqrad := Zone_Rad ** 2;
                     else
                        Height :=
                          Max (Height, Get_Prop_Height (aProp, NW, SE));
                     end if;
                  end if;
               end if;
            end loop;  --  end for property
         end loop; --  end for horiz
      end loop;  --  end for vert

      return Height;

   end Get_Prop_Height_Between;

   --  -------------------------------------------------------------------------

   function Get_Tile_Property_Indices (U, V : Positive) return Prop_Indices_List is
   begin
      return Props_In_Tiles (U, V);
   end Get_Tile_Property_Indices;

   --  -------------------------------------------------------------------------

   function Get_Tile_Property_Index (U, V, Index : Positive) return Positive is
      use Prop_Indices_Package;
      Prop : constant Prop_Indices_List := Props_In_Tiles (U, V);
   begin
      if not Properties_Manager.Index_Is_Valid (Index) then
         raise Prop_Renderer_Exception with
           "Prop_Renderer.Get_Tile_Property_Index, invalid property index: " &
           Positive'Image (Index);
      end if;
      return Positive (Prop.Element (Index));
   end Get_Tile_Property_Index;

   --  -------------------------------------------------------------------------

   --      function Get_Script_Data (Script_Index : Positive)
   --                                return Prop_Renderer_Support.Prop_Script is
   --      begin
   --          return Scripts.Element (Script_Index);
   --      end Get_Script_Data;

   --  -------------------------------------------------------------------------

   function Get_Script_Index (Prop_Index : Positive) return Positive is
      Property : constant Property_Data :=
                   Properties_Manager.Get_Property_Data (Prop_Index);
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

      Properties_Manager.Replace_Property (Prop_Index, Prop);
      Activate_Property (Prop_Index, True);
      Audio.Play_Sound (Decap_Sound_File, True);

   end Launch_Decap_Head;

   --  -------------------------------------------------------------------------

   procedure Do_Food (Prop      : in out Property_Data;
                      Prop_Type : in out Prop_Renderer_Support.Property_Type;
                      Pos       : Singles.Vector3; Radius : Float;
                      aScript   : Prop_Script; Continue : in out Boolean;
                      Result    : in out Integer) is
      use Singles;
      Max_D_Sq     : constant Float := 0.343;  --  0.7^3 + radius = most of tile;
      Distance     : Vector3 := (0.0, 0.0, 0.0);
      Sq_Distance  : Float := 0.0;
      Prop_Pos     : Vector3 := (0.0, 0.0, 0.0);
   begin
      if not Prop.Was_Collected_By_Player then
         Prop_Pos := Prop.World_Pos;
         Distance := Prop_Pos - Pos;
         Sq_Distance := Float (Maths.Length_Sq (Distance));
         Sq_Distance := Sq_Distance - Radius ** 3;
         if Sq_Distance <= Max_D_Sq then
            Prop_Type := Food_Prop;
            Prop.Was_Collected_By_Player := True;
            Prop.Is_Visible := False;
            Audio.Play_Sound
              (To_String (aScript.Sound_Activate_File_Name),
               True);
            Result := aScript.Value;
            Continue := False;
         end if;
      end if;
   end Do_Food;

   --  -------------------------------------------------------------------------

   procedure Do_Hammer (Prop      : in out Property_Data;
                        Prop_Type : in out Prop_Renderer_Support.Property_Type;
                        aScript   : Prop_Script) is
      use Singles;
      Track  : Unbounded_String := To_Unbounded_String ("");
   begin
      if not Prop.Was_Collected_By_Player then
         Prop_Type := Hammer_Prop;
         Prop.Is_Visible := False;
         Prop.Was_Collected_By_Player := True;
         Audio.Play_Sound
           (To_String (aScript.Sound_Activate_File_Name),
            True);
         FB_Effects.FB_White_Flash;
         if not Audio.Is_Playing_Hammer_Track then
            Track := GUI_Level_Chooser.Get_Selected_Level_Hammer_Music;
            if Track /= To_Unbounded_String ("") then
               Audio.Play_Music (To_String (Track));
            end if;
            Audio.Set_Playing_Hammer_Track (True);
         end if;
      end if;
   end Do_Hammer;

   --  -------------------------------------------------------------------------

   procedure Do_Treasure (Prop      : in out Property_Data;
                          Prop_Type : in out Prop_Renderer_Support.Property_Type;
                          Pos       : Singles.Vector3; Radius : Float;
                          aScript   : Prop_Script; Continue : in out Boolean;
                          Result    : in out Integer) is
      use Singles;
      Max_D_Sq     : constant Float := 0.343;  --  0.7^3 + radius = most of tile;
      Distance     : Vector3 := (0.0, 0.0, 0.0);
      Sq_Distance  : Float := 0.0;
      Prop_Pos     : Vector3 := (0.0, 0.0, 0.0);
      Value        : Integer;
   begin
      if not Prop.Was_Collected_By_Player then
         Prop_Pos := Prop.World_Pos;
         Distance := Prop_Pos - Pos;
         Sq_Distance := Float (Maths.Length_Sq (Distance));
         Sq_Distance := Sq_Distance - Radius ** 3;
         if Sq_Distance <= Max_D_Sq then
            Prop_Type := Treasure_Prop;
            Audio.Play_Sound
              (To_String (aScript.Sound_Activate_File_Name),
               True);
            Prop.Was_Collected_By_Player := True;
            Prop.Is_Visible := False;
            if aScript.Uses_Sprite then
               Sprite_Renderer.Set_Sprite_Visible
                 (Prop.Sprite_Index, False);
            end if;
            Value := aScript.Value;
            if Value > 0 then
               Text.Add_Particle_Text
                 (Integer'Image (Value), (1.0, 1.0, 0.0, 1.0),
                  Prop_Pos + (0.0, 2.0, 0.0));
            end if;
            Character_Controller.Set_Total_Treasure_Found
              (Character_Controller.Total_Treasure_Found + Value);
            Result := Value;
            Continue := False;
         end if;
      end if;

   end Do_Treasure;

   --  -------------------------------------------------------------------------

   function Pick_Up_Item_In
     (Map       : Ints.Vector2; Pos : Singles.Vector3; Radius : Float; Player_Health : Integer;
      Prop_Type : in out Prop_Renderer_Support.Property_Type) return Integer is
      use Prop_Indices_Package;
      use gl.Types.Singles;
      U            : constant Integer := Integer (Map (GL.X));
      V            : constant Integer := Integer (Map (GL.Y));
      Indices_List : constant Prop_Indices_List :=
                       Props_In_Tiles (Integer (U), Integer (V));
      Curs         : Cursor := Indices_List.First;
      Prop_Pos     : Vector3 := (0.0, 0.0, 0.0);
      Props_Size   : Integer;
      Prop_Index   : Integer;
      Prop         : Property_Data;
      Scrip_Index  : Positive;
      aScript      : Prop_Script;
      Continue     : Boolean := True;
      Result       : Integer := Property_Type'Enum_Rep (Generic_Prop);
   begin
      if Tiles_Manager.Is_Tile_Valid (Map) then
         Props_Size := Integer (Indices_List.Length);
         while Has_Element (Curs) and Continue loop
            Prop_Index := Integer (Element (Curs));
            Prop := Properties_Manager.Get_Property_Data (Prop_Index);
            Scrip_Index := Prop.Script_Index;
            aScript := Properties_Manager.Get_Script_Data (Scrip_Index);
            case aScript.Script_Type is
               when Treasure_Prop =>
                  Do_Treasure (Prop, Prop_Type, Pos, Radius, aScript,
                               Continue, Result);
               when Food_Prop =>
                  if Player_Health < 100 then
                     Do_Food (Prop, Prop_Type, Pos, Radius, aScript,
                              Continue, Result);
                  end if;
               when Hammer_Prop =>
                  Do_Hammer (Prop, Prop_Type, aScript);
                  Continue := False;
               when others => null;
            end case;
            Next (Curs);
         end loop;
      end if;
      return Result;
   end Pick_Up_Item_In;

   --  ------------------------------------------------------------------------

   function Props_In_Tiles_Size (U, V : Integer) return Natural is
   begin
      return Natural (Props_In_Tiles (U, V).Last_Index);
   end Props_In_Tiles_Size;

   --  ------------------------------------------------------------------------

   procedure Render_Property (Prop_ID : Positive) is
      use GL.Culling;
      use Prop_Indices_Package;
      use Properties_Shader_Manager;
      Curr_Time    : Single;
      Elapsed_Time : Single;
      Property     : Property_Data :=
                       Properties_Manager.Get_Property_Data (Prop_ID);
      Script_ID    : Positive := Property.Script_Index;
      aScript      : Prop_Script := Properties_Manager.Get_Script_Data (Script_ID);
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
      aScript := Properties_Manager.Get_Script_Data (Script_ID);
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
      Properties_Manager.Replace_Property (Prop_ID, Property);

      if Settings.Render_OLS and aScript.Draw_Outlines then
         Set_Outline_Shaders (Prop_Type, aScript);
      end if;
      GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, aScript.Vertex_Count);

   end Render_Property;

   --  -------------------------------------------------------------------------

   function Push_Boulder_In_Tile (Map_U, Map_V     : Int;
                                  Hand_Y_World_Pos : Single) return Boolean is
      Prop_IDs   : Prop_Indices_List;
      Prop_ID    : Integer;
      Property   : Property_Data;
      Script_ID  : Positive;
      Script     : Prop_Script;
      Dist       : Single;
      Result     : Boolean := False;
   begin
      if not Tiles_Manager.Is_Tile_Valid ((Map_U, Map_V)) then
         raise Prop_Renderer_Exception with
           "Prop_Renderer.Activate_Door_In_Tile, invalid tile indices: " &
           Int'Image (Map_U) & ", " & Int'Image (Map_V);
      end if;

      Prop_IDs := Props_In_Tiles (Integer (Map_U), Integer (Map_V));
      for index in Prop_IDs.First_Index .. Prop_IDs.Last_Index  loop
         if not Result then
            Prop_ID := Integer (Prop_IDs.Element (index));
            Property := Properties_Manager.Get_Property_Data (Prop_ID);
            Script_ID := Property.Script_Index;
            Script := Properties_Manager.Get_Script_Data (Script_ID);
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
      Left           : constant Int := Maths.Max_Int (0, V - Tiles_Distance) + 1;
      Right          : constant Int := Maths.Min_Int (Batch_Manager.Max_Cols - 1,
                                                      V + Tiles_Distance) + 1;
      Up             : constant Int := Maths.Max_Int (0, U - Tiles_Distance) + 1;
      Down           : constant Int := Maths.Min_Int (Batch_Manager.Max_Rows - 1,
                                                      U + Tiles_Distance) + 1;
      --  Diamond Bob
      Curr_Time      : constant Single := Single (Glfw.Time);
      Elapsed        : constant Single := Curr_Time - Prev_Time;
      Hdg_Dia        : constant Single := 20.0 * Elapsed;
      Hgt_Dia        : constant Single := 0.5 * Sin (2.0 * Curr_Time);
      Prop_Indices   : Prop_Indices_List;
      Property       : Property_Data;
      Script_Index   : Integer;
      theScript_Type : Property_Type;
      aScript        : Prop_Script;
      Ssi            : Natural;
      Mesh_Index     : Integer;
      Bone_Count     : Integer;
      Rot_Dia        : Singles.Matrix4;
      Trans_Dia      : Singles.Matrix4;
      Trans          : Singles.Vector3;
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
            if not Tiles_Manager.Is_Tile_Valid ((ui, vi)) then
               raise Prop_Renderer_Exception with
                 "Prop_Renderer.Activate_Door_In_Tile, invalid tile indices: " &
                 Int'Image (ui) & ", " & Int'Image (vi);
            end if;
            Prop_Indices := Props_In_Tiles (Integer (ui), Integer (vi));
            for Props_Index in Prop_Indices.First_Index ..
              Prop_Indices.Last_Index loop
               Property := Properties_Manager.Get_Property_Data (Props_Index);
               Script_Index := Property.Script_Index;
               aScript := Properties_Manager.Get_Script_Data (Script_Index);
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
                  theScript_Type := aScript.Script_Type;
                  --                          Put_Line ("Prop_Renderer.Render_Props_Around_Split Script_Type: "
                  --                               & Property_Type'Image (aScript.Script_Type));

                  if theScript_Type = Door_Prop or
                    theScript_Type = Pillar_Prop or
                    theScript_Type = Anim_Loop_Prop or
                    theScript_Type = Windlass_Prop then
                     Shadows.Set_Depth_Skinned_Model_Matrix
                       (Property.Model_Matrix);
                     Mesh_Index := aScript.Mesh_Index;
                     Bone_Count := Mesh_Loader.Bone_Count (Mesh_Index);
                     Shadows.Set_Depth_Skinned_Bone_Matrices
                       (Property.Current_Bone_Transforms);
                  elsif theScript_Type = Diamond_Trigger_Prop then
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
            Prop_Indices := Props_In_Tiles (Integer (ui), Integer (vi));
            for Props_Index in Prop_Indices.First_Index ..
              Prop_Indices.Last_Index loop
               --                      Put_Line ("Prop_Renderer.Render_Props_Around_Split u, v, Props_Index: "
               --                             & Int'Image (ui) & ", " & Int'Image (vi) & ", " &
               --                             Integer'Image (Props_Index));
               Property := Properties_Manager.Get_Property_Data (Props_Index);
               Script_Index := Property.Script_Index;
               aScript := Properties_Manager.Get_Script_Data (Script_Index);
               Ssi := aScript.Smashed_Script_Index;
               if Property.Was_Smashed and Ssi >= 0 then
                  Script_Index := Ssi;
               end if;
               Prop_Type := aScript.Script_Type;

               if Property.Is_Visible or GL_Utils.Is_Edit_Mode then
                  Put_Line ("Prop_Renderer.Render_Props_Around_Split visible Prop_Type: "
                            & Property_Type'Image (Prop_Type));
                  if aScript.Uses_Sprite then
                     Put_Line ("Prop_Renderer.Render_Props_Around_Split Uses_Sprite");
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
                     Properties_Manager.Replace_Property
                       (Props_Index, Property);

                  elsif Frustum.Is_Sphere_In_Frustum
                    (Property.Origin_World, aScript.Bounding_Radius) and
                    aScript.Transparent then
                     Put_Line ("Prop_Renderer.Render_Props_Around_Split Add_Transparency_Item");
                     Add_Transparency_Item
                       (Transparency_Prop, Props_Index,
                        Property.Origin_World,
                        aScript.Bounding_Radius);
                  else
                     --                              Put_Line ("Prop_Renderer.Render_Props_Around_Split default Prop_Type: "
                     --                                       & Property_Type'Image (Prop_Type));
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
         Put_Line ("Prop_Renderer.Render_Props_Around_Split exception");
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

   procedure Set_End_Camera_Matrix (Mat : Singles.Matrix4) is
   begin
      End_Camera_Matrix := Mat;
   end Set_End_Camera_Matrix;

   --  ------------------------------------------------------------------------

   procedure Set_End_Camera_Position (Pos : Singles.Vector3) is
   begin
      End_Camera_Position := Pos;
   end Set_End_Camera_Position;

   --  ------------------------------------------------------------------------

   procedure Set_Portal_Index (Index : Natural) is
   begin
      Portal_Index := Index;
   end Set_Portal_Index;

   --  ------------------------------------------------------------------------

   procedure Splash_Particles_At (Pos : Singles.Vector3) is
   begin
      Audio.Play_Sound (Splash_Sound_File, True);
      Particle_System.Set_Particle_System_Position (Splash_Particles, Pos);
      Particle_System.Start_Particle_System (Splash_Particles);
   end Splash_Particles_At;

   --  -------------------------------------------------------------------------

   function Sq_Dist_To_End_Level_Portal (Pos : Singles.Vector3) return Float is
      use GL.Types.Singles;
      Data    : Prop_Renderer_Support.Property_Data;
      Dist_3D : Singles.Vector3;
      Result  : Float := 10000.0;
   begin
      if Portal_Index > 0 then
         Data := Properties_Manager.Get_Property_Data (Portal_Index);
         Dist_3D := Pos - Data.World_Pos;
         Result := Abs (Float (Maths.Length_Sq (Dist_3D)));
      end if;
      return Result;
   end Sq_Dist_To_End_Level_Portal;

   --  -------------------------------------------------------------------------

   procedure Trigger_Any_Tx_In
     (Map : Ints.Vector2; Act_Type : Properties_Manager.Activator_Type;
      Pos : Singles.Vector3; Radius : Single) is
      use Properties_Manager;
      U            : constant Positive := Positive (Map (GL.X));
      V            : constant Positive := Positive (Map (GL.Y));
      Prop_Indices : constant Prop_Indices_List := Props_In_Tiles (U, V);
      Prop_Index   : Positive;
      aProp        : Property_Data;
      Script_Index : Positive;
      aScript      : Prop_Script;
      Script_Type  : Prop_Renderer_Support.Property_Type;
      Continue     : Boolean := True;
   begin
      if not Tiles_Manager.Is_Tile_Valid (Map) then
         raise Prop_Renderer_Exception with
           "Trigger_Any_Tx_In, invalid tile indices: " &
           Integer'Image (U) & ", " & Integer'Image (V);
      end if;

      for index in Prop_Indices.First_Index .. Prop_Indices.Last_Index loop
         --  only trigger once
         --  tx_codes will be increased for receivers too because the editor
         --  does tx++ and rx++ with the same button
         Prop_Index := Positive (Prop_Indices.Element (index));
         aProp := Properties_Manager.Get_Property_Data (Prop_Index);
         Script_Index := aProp.Script_Index;
         aScript := Get_Script_Data (Script_Index);
         if aProp.Tx_Code >= 0 and
           (not aProp.Was_Triggered or not aScript.Trigger_Only_Once) then
            if (Act_Type /= Prop_Activator_Player_State or
                  aScript.Character_Activated) and
              (Act_Type /= Prop_Activator_Npc_State or
                 aScript.Npc_Activated) then
               Script_Type := aScript.Script_Type;
               case Script_Type is
                  when Touch_Plate_Prop => Trigger_Touch_Plate
                       (aProp.World_Pos, Pos, Radius, aProp, aScript, Continue);
                  when Windlass_Prop => Trigger_Windlass
                       (aProp.World_Pos, Pos, Radius, aProp, aScript,
                        Prop_Index, Continue);
                  when Diamond_Trigger_Prop => Trigger_Diamond
                       (aProp.World_Pos, Pos, Radius, aProp, aScript, Continue);
                  when others => Continue := False;
               end case;
               if Continue then
                  if aScript.Hide_After_Triggering then
                     aProp.Is_Visible := False;
                     if aProp.Script_Index > 0 then
                        Sprite_Renderer.Set_Sprite_Visible
                          (aProp.Script_Index, False);
                     end if;
                  end if;

                  Event_Controller.Transmit_Code (aProp.Tx_Code);
                  aProp.Was_Triggered :=True;
               end if;
               Properties_Manager.Replace_Property (Prop_Index, aProp);
            end if;
         end if;
      end loop;

   end Trigger_Any_Tx_In;

   --  -------------------------------------------------------------------------

   procedure Update_Anim_Looped_Prop (Prop_Index : Positive; Seconds : Single) is
      use Mesh_Loader;
      Property     : Property_Data;
      Script_Index : Integer;
      aScript      : Prop_Script;
      Smashed_SI   : Integer;
      Meshes       : Mesh_List := Loaded_Meshes;
      Mesh_Index   : Integer;
      aMesh        : Mesh;
      Animation_1  : Animation;
      Duration     : Float;
      Elapsed_Time : Float;
   begin
      if not Properties_Manager.Index_Is_Valid (Prop_Index) then
         raise Prop_Renderer_Exception with
           "Prop_Renderer.Update_Anim_Looped_Prop invalid Prop_Index: " &
           Positive'Image (Prop_Index);
      end if;

      Property := Properties_Manager.Get_Property_Data (Prop_Index);
      if Property.Is_Visible then
         Script_Index := Property.Script_Index;
         aScript := Properties_Manager.Get_Script_Data (Script_Index);
         Smashed_SI := aScript.Smashed_Script_Index;
         if Property.Was_Smashed then
            Script_Index := Smashed_SI;
         end if;
         Mesh_Index := aScript.Mesh_Index;
      end if;
      Property.Anim_Elapsed_Time := Property.Anim_Elapsed_Time + Float (Seconds);
      Duration := Mesh_Loader.Animation_Duration (Mesh_Index, 1);

      if Property.Anim_Elapsed_Time >= Duration then
         Property.Anim_Elapsed_Time :=
           Property.Anim_Elapsed_Time - Duration;
      end if;
      Elapsed_Time := Property.Anim_Elapsed_Time;
      aMesh := Loaded_Mesh (Mesh_Index);
      if Mesh_Loader.Mesh_Animation (Mesh_Index, 1, Animation_1) then
         Mesh_Loader.Recurse_Animation_Tree
           (aMesh, Animation_1, Elapsed_Time, 0, Singles.Identity4 );
      end if;

      Properties_Manager.Replace_Property (Prop_Index, Property);

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
                                    Dist                      : Single) is
   begin
      Prop_Dyn_Light_Pos_Wor := World_Pos;
      Prop_Dyn_Light_Diff := Diff;
      Prop_Dyn_Light_Spec := Specular;
      Prop_Dyn_Light_Range := Dist;
      Prop_Dyn_Light_Dirty := True;
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
            Property := Properties_Manager.Get_Property_Data (P_Index);
            S_Index := Property.Script_Index;
            aScript := Properties_Manager.Get_Script_Data (S_Index);
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
            Properties_Manager.Replace_Property (P_Index, Property);
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

   procedure Update_Props_In_Tiles_Index (U, V       : Integer;
                                          Prop_Index : GL.Types.Int) is
   begin
      if Properties_Manager.Index_Is_Valid (Integer (Prop_Index)) then
         Props_In_Tiles (U, V).Append (Prop_Index);
         --               Put_Line ("Prop_Renderer.Update_Props_In_Tiles_Index Props_In_Tiles (U, V) length: "
         --                & Integer'Image (U) & ", " & Integer'Image (V) & ", "
         --                & Integer'Image (Integer (Props_In_Tiles(U, V).Length)));
      else
         raise Prop_Renderer_Exception with
           "Prop_Renderer.Update_Props_In_Tiles_Index called with invalid index: "
           & GL.Types.Int'Image (Prop_Index);
      end if;
   end Update_Props_In_Tiles_Index;

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

   --  ------------------------------------------------------------------------

   procedure Update_Windlass (Prop_Index : Positive; Seconds : Float) is
   begin
      null;
   end Update_Windlass;

   --  -------------------------------------------------------------------------

end Prop_Renderer;
