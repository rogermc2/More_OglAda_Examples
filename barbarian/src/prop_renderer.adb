
with Glfw;

with GL.Toggles;

with Batch_Manager;
with Frustum;
with Game_Utils;
with GL_Utils;
with Particle_System;
with Properties_Shader_Manager;
with Properties_Skinned_Shader_Manager;
with Prop_Renderer.Boulder;
with Shadows;
with Tiles_Manager;

package body Prop_Renderer is

   Head_Particles_File   : constant String := "blood_artery_jet.particles";
   Splash_Particles_File : constant String := "splash.particles";
   Dust_Particles_File   : constant String := "dust.particles";
   Pot_Particles_File    : constant String := "pot.particles";
   Mirror_Particles_File : constant String := "mirror.particles";

   Max_Scene_Lamp_Locs        : constant integer := 3;
   --  Up To 32 Types, With 8 Active Ones Of Each Type
   Max_Decap_Types            : constant integer :=  32;
   Max_Active_Decaps_Per_Type : constant integer := 8;
   Max_Decap_Particles        : constant Int := 4;
   Max_Mirrors                : constant integer := 16;

   package Property_Scripts_Package is new Ada.Containers.Vectors
     (Positive, Prop_Script);
   type Script_List is new Property_Scripts_Package.Vector with null Record;

   package Properties_Package is new Ada.Containers.Vectors
     (Positive, Property_Data);
   type Properties_List is new Properties_Package.Vector with null Record;

   package Indicies_Package is new Ada.Containers.Vectors (Positive, Positive);
   type Indicies_List is new Indicies_Package.Vector with null Record;

   --  Animation and rendering
   Model_Matrix                : Singles.Matrix4 := (others => (others => 0.0));
   --     Current_Bone_Transforms : Singles.Matrix4_Array (1 .. Mesh_Loader.Max_Bones);
   Anim_Duration               : Integer := 0;
   Anim_Elapsed_Time           : Integer := 0;
   Sprite_Duration             : Integer := 0;
   Delay_Countdown             : Integer := 0;
   --  Hack to stop decap head bouncing when stuck
   Bounce_Count                : Integer := 0;
   Scripts                     : Script_List;
   Properties                  : Properties_List;
   Active_Properties_A         : Indicies_List;
   Active_Properties_B         : Indicies_List;
   Curr_Active_Props_A         : Boolean := True;
   Basic_Props_Render_List     : Indicies_List;
   Skinned_Props_Render_List   : Indicies_List;
   Jav_Stand_Props_Render_List : Indicies_List;
   Portal_Props_Render_List    : Indicies_List;
   Treasure_Props_Render_List  : Indicies_List;
   Props_In_Tiles              : Props_In_Tiles_Array;
   Head_Particles              : Int_Array (1 .. Max_Decap_Particles);
   Dust_Particles              : Int := -1;
   Dust_Particlesb             : Int := -1;
   Dust_Particlesc             : Int := -1;
   Pot_Particles               : Int := -1;
   Mirror_Particles            : Int := -1;
   Splash_Particles            : Int := -1;

   Mirror_Indices              : array (1 .. Max_Mirrors) of Positive;
   Prop_Count                  : Int := 0;
   Mirror_Count                : Int := 0;
   Live_Mirror_Count           : Int := 0;
   Num_Types_Decap_Heads       : Int := 0;
   Last_Head_Particles_Used    : Int := 0;
   Prev_Time                   : Single := Single (Glfw.Time);

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
      return Prop.Element (Index);
   end Get_Property_Index;

   --  -------------------------------------------------------------------------

   function Get_Property_Data (Prop_Index : Positive) return Property_Data is
   begin
      return Properties.Element (Prop_Index);
   end Get_Property_Data;

   --  -------------------------------------------------------------------------

   function Get_Script_Data (Script_Index : Positive) return Prop_Script is
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
      Basic_Props_Render_List.Clear;
      Skinned_Props_Render_List.Clear;
      Jav_Stand_Props_Render_List.Clear;
      Portal_Props_Render_List.Clear;
      Treasure_Props_Render_List.Clear;

      Properties_Shader_Manager.Load_Prop_Shaders;
      Game_Utils.Game_Log ("Prop_Shaders loaded");

      for index in Int range 1 .. Max_Decap_Particles loop
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

   procedure Render_Props_Around_Depth_Only (U, V, Tiles_Distance : Int) is
      use Prop_Indices_Package;
      use GL.Objects.Vertex_Arrays;
      use GL.Toggles;
      use Maths;
      use Single_Math_Functions;
      use Singles;
      Left         : constant Int := Maths.Max_Int (0, V - Tiles_Distance);
      Right        : constant Int := Maths.Min_Int (Batch_Manager.Max_Cols - 1, V + Tiles_Distance);
      Up           : constant Int := Maths.Max_Int (0, U - Tiles_Distance);
      Down         : constant Int := Maths.Min_Int (Batch_Manager.Max_Rows - 1, U + Tiles_Distance);
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
      Ssi          : Integer;
      Mesh_Index   : Integer;
      Bone_Count   : Integer;
      Rot_Dia      : Singles.Matrix4;
      Trans_Dia    : Singles.Matrix4;
      Trans        : Singles.Vector3;
   begin
      Prev_Time := Curr_Time;
      Enable (Depth_Test);
      for vi in Left .. Right loop
            for ui in Up .. Down loop
               --                 Props_Size := Tile_Data'Size;
            Prop_Indices := Tile_Data (Integer (ui), Integer (vi));
            for Props_Index in Prop_Indices.First_Index .. Prop_Indices.Last_Index loop
               Property := Properties.Element (Props_Index);
               Script_Index := Property.Script_Index;
               aScript := Scripts.Element (Script_Index);
               Ssi := aScript.Smashed_Script_Index;
               if Property.Was_Smashed and Ssi >= 0 then
                  Script_Index := Ssi;
               end if;

               if (Property.Is_Visible or GL_Utils.Is_Edit_Mode) or else
                 aScript.Casts_Shadow or else
                 not aScript.Uses_Sprite or else
                 not aScript.Transparent or else
                 Frustum.Is_Sphere_In_Frustum
                   (Property.Origin_World, aScript.Bounding_Radius) then
                  GL_Utils.Bind_VAO (aScript.Vao);
                  Script_Type := aScript.Script_Type;
                  if Script_Type = Door_Prop or
                    Script_Type = Pillar_Prop or
                    Script_Type = Anim_Loop_Prop or
                    Script_Type = Windlass_Prop then
                     Shadows.Set_Depth_Skinned_Model_Matrix
                       (Property.Model_Mat);
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
                     Shadows.Set_Depth_Model_Matrix (Property.Model_Mat);
                  end if;
                  Draw_Arrays (Triangles, 0, aScript.Vertex_Count);
               end if;
            end loop;
         end loop;
      end loop;
   end Render_Props_Around_Depth_Only;

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
      Basic_Props_Render_List.Clear;
      Skinned_Props_Render_List.Clear;
      Jav_Stand_Props_Render_List.Clear;
      Portal_Props_Render_List.Clear;
      Treasure_Props_Render_List.Clear;

      Prop_Count := 0;
      Num_Types_Decap_Heads := 0;
      Last_Head_Particles_Used := 0;
      Mirror_Count := 0;
      Live_Mirror_Count := 0;

      for index in Int range 1 .. Max_Decap_Particles loop
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
      Properties_Shader_Manager.Set_L_A (Level);
      Properties_Skinned_Shader_Manager.Set_L_A (Level);
   end Set_Ambient_Light_Level;

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
      use Tile_Indices_Package;
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
      Properties_Shader_Manager.Set_Light_Pos (Positions);
      Properties_Skinned_Shader_Manager.Set_Light_Pos (Positions);
      Properties_Shader_Manager.Set_Light_Diff (Diffuse);
      Properties_Skinned_Shader_Manager.Set_Light_Diff (Diffuse);
      Properties_Shader_Manager.Set_Light_Spec (Specular);
      Properties_Skinned_Shader_Manager.Set_Light_Spec (Specular);
      Properties_Shader_Manager.Set_Light_Range (Ranges);
      Properties_Skinned_Shader_Manager.Set_Light_Range (Ranges);
   end Update_Static_Lights_Uniforms;

   --  ----------------------------------------------------------------------------

end Prop_Renderer;
