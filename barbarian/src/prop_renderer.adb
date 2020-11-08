
with Ada.Containers.Vectors;

with Batch_Manager;
with Game_Utils;
with Manifold;
with Particle_System;
with Properties_Shader_Manager;
with Properties_Skinned_Shader_Manager;

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

   type Tile_Data_Array is array (1 .. Manifold.Max_Tile_Cols,
                                  1 .. Manifold.Max_Tile_Cols) of Integer;

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
   Props_In_Tiles              : Tile_Data_Array := (others => (others => 0));
   Head_Particles              : Int_Array (1 .. Max_Decap_Particles);
   Dust_Particles              : Int := -1;
   Dust_Particlesb             : Int := -1;
   Dust_Particlesc             : Int := -1;
   Pot_Particles               : Int := -1;
   Mirror_Particles            : Int := -1;
   Splash_Particles            : Int := -1;

   Prop_Count                  : Int := 0;
   Mirror_Count                : Int := 0;
   Live_Mirror_Count           : Int := 0;
   Num_Types_Decap_Heads       : Int := 0;
   Last_Head_Particles_Used    : Int := 0;

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

   procedure Reset_Properties is
      -- Particle Systems
      Start_Now      : constant Boolean := False;
      Always_Update  : constant Boolean := True;
      Always_Draw    : constant Boolean := False;
   begin
      for row in Tile_Data_Array'Range loop
         for col in Tile_Data_Array'Range (2) loop
            Props_In_Tiles (row, col) := 0;
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

   function Update_Props (Seconds : Float) return Boolean is
   begin
      return False;
   end Update_Props;

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
