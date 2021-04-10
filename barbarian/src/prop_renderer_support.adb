
with Glfw;

with  GL.Culling;
with GL.Objects.Programs;

WITH Audio;
with Camera;
with Character_Controller;
with Coins_Shader_Manager;
with Depth_Skinned_Shader_Manager;
with GL_Maths;
with GL_Utils;
with Jav_Stand_Shader_Manager;
with Manifold;
with Particle_System;
with Prop_Renderer;
with Portal_Shader_Manager;
with Properties_Shader_Manager;
with Properties_Skinned_Shader_Manager;
with Specs_Manager;
with Settings;
with Shadows;

package body Prop_Renderer_Support is

    --       generic
    --           type Shader_Directory is (<>);
    --           type View_Matrix is (<>);
    --           type Projection_Matrix is (<>);
    --        procedure Set_Camera;
    --
    --        procedure Set_Camera is
    --        begin
    --            if Camera.Is_Dirty then
    --                 Shader_Directory.Set_View (View_Matrix);
    --                 Shader_Directory.Set_Perspective (Projection_Matrix);
    --              end if;
    --        end Set_Camera;

    --  -------------------------------------------------------------------------

    procedure Do_Javelin_Shader (Property     : in out Property_Data;
                                 Prop_Type    : Property_Type;
                                 aScript      : Prop_Script;
                                 Gold_Current : Integer; Elapsed : Single) is
        use Singles;
        use Maths;
        use Single_Math_Functions;
        use Properties_Shader_Manager;
        CSI          : Integer;
        Curr_Time    : Single;
        Rot_Matrix   : Matrix4 := Identity4;
        Trans_Matrix : Matrix4 := Identity4;
        Model_Matrix : Matrix4 := Identity4;
        Hdg_Dia      : Degree;
        Hgt_Dia      : Single;
        Tra          : Vector3;
        Pep          : Vector3;
    begin
        --  Prop_Type = Jav_Stand_Prop or Prop_Type = Diamond_Trigger_Prop or
        --  Prop_Type = Tavern_Prop
        GL.Objects.Programs.Use_Program (Jav_Stand_Shader);
        Jav_Stand_Shader_Manager.Set_Model (Identity4);
        if Camera.Is_Dirty then
            Jav_Stand_Shader_Manager.Set_View (Camera.View_Matrix);
            Jav_Stand_Shader_Manager.Set_Perspective (Camera.Projection_Matrix);
        end if;

        if Prop_Type /= Diamond_Trigger_Prop then
            if Prop_Type /= Jav_Stand_Prop or
              Character_Controller.Javelin_Count (1) >=
              Character_Controller.Max_Inventory_Javelins then
                CSI := Character_Controller.Spec_Index (1);
                if Prop_Type /= Tavern_Prop or not (Gold_Current < 5 and
                                                      Character_Controller.Current_Health (1) <
                                                      Specs_Manager.Initial_Health (CSI)) then
                    Curr_Time := Single (Glfw.Time);
                    Jav_Stand_Shader_Manager.Set_Time (Curr_Time);
                    Rot_Matrix := Rotate_Y_Degree
                      (Identity4, To_Degrees (Radian (20.0 * Curr_Time)));
                    Trans_Matrix := Translation_Matrix
                      ((0.0, 1.0 + 0.5 * Sin (2.0 * Curr_Time), 0.0));
                    Model_Matrix := Property.Model_Matrix * Trans_Matrix * Rot_Matrix;
                    Jav_Stand_Shader_Manager.Set_Model (Model_Matrix);
                end if;
            end if;

        else  --  Prop_Type = Diamond_Trigger_Prop
            Hdg_Dia := Degree (20.0 * Elapsed);
            Hgt_Dia := 0.5 * Sin (2.0 * Curr_Time);
            Property.Heading_Deg := Property.Heading_Deg + Hdg_Dia;
            Rot_Matrix := Rotate_Y_Degree (Identity4, Property.Heading_Deg);
            Tra := Property.World_Pos;
            Tra (GL.Y) := Tra (GL.Y) + Hgt_Dia;
            Pep := aScript.Particles_Offset + Tra;
            Trans_Matrix := Translation_Matrix (Tra);
            Model_Matrix := Trans_Matrix * Rot_Matrix;

            Jav_Stand_Shader_Manager.Set_Model (Model_Matrix);
            Jav_Stand_Shader_Manager.Set_Time (Single (Glfw.Time));
        end if;
    end Do_Javelin_Shader;

    --  -------------------------------------------------------------------------

    procedure Draw_Outline (aScript : in out Prop_Script) is

    begin
        if aScript.Outlines_Vertex_Count > 0 then
            GL_Utils.Bind_VAO (aScript.Outlines_Vao);
            GL.Objects.Vertex_Arrays.Draw_Arrays
              (Triangles, 0, aScript.Outlines_Vertex_Count);
            GL_Utils.Bind_VAO (aScript.Vao);
        else
            GL.Objects.Vertex_Arrays.Draw_Arrays
              (Triangles, 0, aScript.Vertex_Count);
        end if;
    end Draw_Outline;

    --  -------------------------------------------------------------------------

    procedure Set_Outline_Shaders (Prop_Type    : Property_Type;
                                   aScript      : in out Prop_Script) is
        use GL.Culling;
        use Properties_Shader_Manager;
        use Singles;
        use Maths;
        use GL_Maths;
        use Singles_Array_Package;
        aCharacter   : Character_Controller.Barbarian_Character;
    begin
        Set_Front_Face (Clockwise);
        if Prop_Type = Door_Prop or Prop_Type = Pillar_Prop or
          Prop_Type = Anim_Loop_Prop or Prop_Type = Windlass_Prop then
            Properties_Skinned_Shader_Manager.Set_Outline_Pass (1.0);
            Draw_Outline (aScript);
            Properties_Skinned_Shader_Manager.Set_Outline_Pass (0.0);

        elsif Prop_Type = Treasure_Prop or Prop_Type = Hammer_Prop or
          Prop_Type = Food_Prop then
            Coins_Shader_Manager.Set_Outline_Pass (1.0);
            Draw_Outline (aScript);
            Coins_Shader_Manager.Set_Outline_Pass (0.0);

        elsif Prop_Type = Jav_Stand_Prop or Prop_Type = Diamond_Trigger_Prop or
          Prop_Type = Tavern_Prop then
            Jav_Stand_Shader_Manager.Set_Outline_Pass (1.0);
            Draw_Outline (aScript);
            Jav_Stand_Shader_Manager.Set_Outline_Pass (0.0);

        elsif Prop_Type = Portal_Prop then
            GL.Objects.Vertex_Arrays.Draw_Arrays
              (Triangles, 0, aScript.Vertex_Count);

        else
            Properties_Shader_Manager.Set_Outline_Pass (1.0);
            Draw_Outline (aScript);
            Properties_Shader_Manager.Set_Outline_Pass (0.0);
        end if;

        Set_Front_Face (Counter_Clockwise);
    end Set_Outline_Shaders;

    --  -------------------------------------------------------------------------

    procedure Set_Shaders (Property     : in out  Property_Data;
                           Prop_Type    : Property_Type;
                           aScript      : Prop_Script;
                           Gold_Current : Integer;
                           Elapsed      : Single) is
        use Properties_Shader_Manager;
        use Singles;
        use Maths;
        use GL_Maths;
        use Singles_Array_Package;
        aCharacter   : Character_Controller.Barbarian_Character;
    begin
        if Prop_Type = Door_Prop or Prop_Type = Pillar_Prop or
          Prop_Type = Anim_Loop_Prop or Prop_Type = Windlass_Prop then
            GL.Objects.Programs.Use_Program (Prop_Skinned_Shader);
            if Settings.Shadows_Enabled then
                Properties_Skinned_Shader_Manager.Set_Shadow_Enabled (1.0);
                Properties_Skinned_Shader_Manager.Set_Caster_Position (Shadows.Caster_Position);
                Shadows.Bind_Cube_Shadow_Texture (3);
            else
                Properties_Skinned_Shader_Manager.Set_Shadow_Enabled (0.0);
            end if;

            Properties_Skinned_Shader_Manager.Set_Bone_Matrices
              (Property.Current_Bone_Transforms);
            Properties_Skinned_Shader_Manager.Set_Model (Property.Model_Matrix);
            Properties_Skinned_Shader_Manager.Set_Skinned_Static_Light_Indices
              ((Manifold.Get_Light_Index (Positive (Property.Map_U),
               Positive (Property.Map_V), 1),
               Manifold.Get_Light_Index (Positive (Property.Map_U),
                 Positive (Property.Map_V), 2)));

        elsif Prop_Type = Treasure_Prop or Prop_Type = Hammer_Prop or
          Prop_Type = Food_Prop then
            GL.Objects.Programs.Use_Program (Coins_Shader);
            if Camera.Is_Dirty then
                Coins_Shader_Manager.Set_View (Camera.View_Matrix);
                Coins_Shader_Manager.Set_Perspective (Camera.Projection_Matrix);
            end if;
            if Settings.Shadows_Enabled then
                Coins_Shader_Manager.Set_Shadow_Enabled (1.0);
                Coins_Shader_Manager.Set_Caster_Pos_World (Shadows.Caster_Position);
                Shadows.Bind_Cube_Shadow_Texture (3);
            else
                Coins_Shader_Manager.Set_Shadow_Enabled (0.0);
            end if;
            Coins_Shader_Manager.Set_Model (Property.Model_Matrix);
            Coins_Shader_Manager.Set_Time (Single (Glfw.Time));

        elsif Prop_Type = Jav_Stand_Prop or Prop_Type = Diamond_Trigger_Prop or
          Prop_Type = Tavern_Prop then
            Do_Javelin_Shader (Property, Prop_Type, aScript, Gold_Current, Elapsed);

        elsif Prop_Type = Portal_Prop then
            GL.Objects.Programs.Use_Program (Portal_Shader);
            if Camera.Is_Dirty then
                Portal_Shader_Manager.Set_View (Camera.View_Matrix);
                Portal_Shader_Manager.Set_Perspective (Camera.Projection_Matrix);
            end if;
            Portal_Shader_Manager.Set_Model (Property.Model_Matrix);
            Portal_Shader_Manager.Set_Time (Single (Glfw.Time));

        else
            GL.Objects.Programs.Use_Program (Prop_Shader);
            if Camera.Is_Dirty then
                Properties_Shader_Manager.Set_View (Camera.View_Matrix);
                Properties_Shader_Manager.Set_Perspective (Camera.Projection_Matrix);
            end if;
            if Settings.Shadows_Enabled then
                Properties_Shader_Manager.Set_Shadows_Enabled (1.0);
                Properties_Shader_Manager.Set_Caster_Position (Shadows.Caster_Position);
                Shadows.Bind_Cube_Shadow_Texture (3);
            else
                Properties_Shader_Manager.Set_Shadows_Enabled (0.0);
            end if;

            Properties_Shader_Manager.Set_Model (Property.Model_Matrix);
            Properties_Shader_Manager.Set_Inverse_Matrix
              (From_Real_Matrix4 (Inverse (To_Real_Matrix4 (Property.Model_Matrix))));
            Properties_Shader_Manager.Set_Static_Light_Indices
              ((Manifold.Get_Light_Index
               (Positive (Property.Map_U), Positive (Property.Map_V), 1),
               Manifold.Get_Light_Index
                 (Positive (Property.Map_U), Positive (Property.Map_V), 2)));
        end if;

    end Set_Shaders;

    --  -------------------------------------------------------------------------

    procedure Trigger_Diamond (World_Pos, Pos : Singles.Vector3;
                               Radius         : Single; aProp : Property_Data;
                               aScript        : Prop_Script;
                               Continue       : in out Boolean) is
        use Singles;
        TP_D_Sq : constant Single := 1.0;  --  tile to sides w/ a sphere
        Dist    : constant Vector3 := World_Pos - Pos;
        Sq_Dist : Single := Maths.Length_Sq (Dist);
    begin
        Sq_Dist := Sq_Dist - Radius ** 3;
        Continue := Sq_Dist <= TP_D_Sq;
        if Continue then
            Particle_System.Stop_Particle_System (aProp.Particle_System_Index);
            Audio.Play_Sound (To_String (aScript.Sound_Activate_File_Name), True);
        end if;
    end Trigger_Diamond;

    --  -------------------------------------------------------------------------

    procedure Trigger_Touch_Plate (World_Pos, Pos : Singles.Vector3;
                                   Radius         : Single; aProp : In Out Property_Data;
                                   aScript        : Prop_Script;
                                   Continue       : in out Boolean) is
        use Singles;
        TP_D_Sq : constant Single := 1.0;  --  tile to sides w/ a sphere
        Dist    : constant Vector3 := World_Pos - Pos;
        Sq_Dist : Single := Maths.Length_Sq (Dist);
    begin
        Sq_Dist := Sq_Dist - Radius ** 3;
        Continue := Sq_Dist <= TP_D_Sq;
        if Continue then
            if aScript.Trigger_Only_Once then
                Audio.Play_Sound (To_String (aScript.Sound_Activate_File_Name), True);
            end if;
            if aScript.Hide_After_Triggering then
                aProp.Is_Visible := False;
            end if;
        end if;
    end Trigger_Touch_Plate;

    --  -------------------------------------------------------------------------

    procedure Trigger_Windlass (World_Pos, Pos : Singles.Vector3;
                                Radius         : Single; aProp : In Out Property_Data;
                                aScript        : Prop_Script; Index : Positive;
                                Continue       : in out Boolean) is
        use Singles;
        TP_D_Sq : constant Single := 1.2 ** 3;  --  tile to sides w/ a sphere
        Dist    : constant Vector3 := World_Pos - Pos;
        Sq_Dist : Single := Maths.Length_Sq (Dist);
    begin
        Sq_Dist := Sq_Dist - Radius ** 3;
        Continue := Sq_Dist <= TP_D_Sq;
        if Continue then
            Audio.Play_Sound (To_String (aScript.Sound_Activate_File_Name), True);
            Prop_Renderer.Activate_Property (Index, False);
            aProp.Anim_Elapsed_Time := 0.0;
        end if;
    end Trigger_Windlass;

    --  -------------------------------------------------------------------------

end Prop_Renderer_Support;
