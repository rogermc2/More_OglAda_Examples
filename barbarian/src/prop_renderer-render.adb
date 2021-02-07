
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
with Settings;
with Shadows;
with Specs_Manager;
with Sprite_Renderer;
with Texture_Manager;
with Tiles_Manager;
with Transparency;

package body Prop_Renderer.Render is
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

    --  ------------------------------------------------------------------------

    procedure Render_Basic (Prop_Dyn_Light_Pos_Wor, Prop_Dyn_Light_Diff,
                            Prop_Dyn_Light_Spec : Singles.Vector3;
                            Prop_Dyn_Light_Range : Single := 1.0;
                            Prop_Dyn_Light_Dirty : Boolean := True) is
        use GL.Objects.Vertex_Arrays;
        use GL_Maths.Indices_Package;
        use Properties_Shader_Manager;
        use Properties_Manager;
        use Prop_Renderer;
        Count    : constant Integer := Integer (Basic_Render_List.Length);
        Property : Prop_Renderer_Support.Property_Data;
        Script   : Prop_Renderer_Support.Prop_Script;
        Prop_I   : Integer;
        Script_I : Integer;
        Ssi      : Integer;
        U        : Positive;
        V        : Positive;
    begin
        if not Is_Empty (Basic_Render_List) then
            GL.Objects.Programs.Use_Program (Prop_Shader);
            if Camera.Is_Dirty then
                Set_View (Camera.View_Matrix);
                Set_Perspective (Camera.Projection_Matrix);
                Set_Camera_Position (Camera.World_Position);
            end if;
            if Prop_Dyn_Light_Dirty then
                Set_Dyn_Light_Pos (Prop_Dyn_Light_Pos_Wor);
                Set_Dyn_Light_Diff (Prop_Dyn_Light_Diff);
                Set_Dyn_Light_Spec (Prop_Dyn_Light_Spec);
                Set_Dyn_Light_Range (Prop_Dyn_Light_Range);
            end if;
            if Settings.Shadows_Enabled then
                Set_Shadows_Enabled (1.0);
                Set_Caster_Position (Shadows.Caster_Position);
                Shadows.Bind_Cube_Shadow_Texture (3);
            else
                Set_Shadows_Enabled (0.0);
            end if;

            for Param_I in 1 .. Count loop
                Prop_I := Basic_Render_List (Param_I);
                Property := Get_Property_Data (Prop_I);
                Script_I := Get_Script_Index (Prop_I);
                Script := Get_Script_Data (Script_I);
                Ssi := Script.Smashed_Script_Index;
                if Property.Was_Smashed and Ssi > 0 then
                    Script_I := Ssi;
                end if;
                U := Positive (Property.Map_U);
                V := Positive (Property.Map_V);
                Set_Model (Property.Model_Matrix);
                Set_Inverse_Matrix (Property.Model_Matrix);
                Set_Static_Light_Indices
                  ((Manifold.Get_Light_Index (U, V, 0),
                   Manifold.Get_Light_Index (U, V, 1)));
                if Settings.Render_OLS and Script.Draw_Outlines then
                    GL.Culling.Set_Front_Face (Clockwise);
                    Set_Outline_Pass (1.0);
                    if Script.Outlines_Vertex_Count > 0 then
                        GL_Utils.Bind_VAO (Script.Outlines_Vao);
                    else
                        GL_Utils.Bind_VAO (Script.Vao);
                        Draw_Arrays (Triangles, 0, Script.Outlines_Vertex_Count);
                        Draw_Arrays (Triangles, 0, Script.Vertex_Count);
                    end if;
                    Set_Outline_Pass (0.0);
                    GL.Culling.Set_Front_Face (Counter_Clockwise);
                end if;
                GL_Utils.Bind_VAO (Script.Vao);
                Texture_Manager.Bind_Texture (0, Script.Diffuse_Map_Id);
                Texture_Manager.Bind_Texture (1, Script.Specular_Map_Id);
                Texture_Manager.Bind_Texture (2, Script.Normal_Map_Id);
                Draw_Arrays (Triangles, 0, Script.Vertex_Count);
            end loop;
        end if;
    end Render_Basic;

    --  -------------------------------------------------------------------------

    procedure Render_Javelin_Standard is
        use GL.Objects.Vertex_Arrays;
        use Singles;
        use Character_Controller;
        use GL_Maths.Indices_Package;
        use Jav_Stand_Shader_Manager;
        use Maths;
        use Single_Math_Functions;
        Character    : Barbarian_Character;
        Count        : Integer;
        Property     : Prop_Renderer_Support.Property_Data;
        Script       : Prop_Renderer_Support.Prop_Script;
        Prop_I       : Integer;
        Script_I     : Integer;
        Spec_I       : Integer;
        Tim          : constant Single := Single (Glfw.Time);
        Prop_Kind    : Property_Type;
        Heading_Dia  : Degree;
        Height_Dia   : Single;
        Translate    : Vector3;
        Rot_Matrix   : Matrix4 := Identity4;
        Trans_Matrix : Matrix4 := Identity4;
        Model_Matrix : Matrix4 := Identity4;
        Continue     : Boolean := True;
    begin
        Character := Get_Character (1);
        Count := Integer (Jav_Stand_Render_List.Length);
        if not Is_Empty (Jav_Stand_Render_List) then
            Put_line ("Prop_renderer.Render Jav_Stand_Render_List not empty ");
            GL.Objects.Programs.Use_Program
              (Properties_Shader_Manager.Jav_Stand_Shader);
            Set_Time (Tim);
            if Camera.Is_Dirty then
                Set_View (Camera.View_Matrix);
                Set_Perspective (Camera.Projection_Matrix);
            end if;

            for Param_I in 1 .. Count loop
                Put_line ("Prop_renderer.Render Param_I: " &
                Integer'Image (Param_I));
                if  Continue then
                    Prop_I := Jav_Stand_Render_List (Param_I);
                    Property := Properties_Manager.Get_Property_Data (Prop_I);
                    Script_I := Get_Script_Index (Prop_I);
--                      Script_I := Properties (Prop_I).Script_Index;
                    Script := Properties_Manager.Get_Script_Data (Script_I);
                    Spec_I := Spec_Index (1);
                    Prop_Kind := Script.Script_Type;
                    if Prop_Kind = Jav_Stand_Prop or Prop_Kind = Tavern_Prop then
                        --  don't draw if can't buy right now
                        if Prop_Kind = Jav_Stand_Prop then
                            Continue := (Gold_Current > 4 and
                                           Javelin_Count (1) < Max_Inventory_Javelins);
                        elsif Prop_Kind = Tavern_Prop then
                            Continue :=
                              (Gold_Current > 4 and Current_Health (1) <
                                   Specs_Manager.Initial_Health (Spec_I));
                        end if;
                        if Continue then
                            Rot_Matrix := Rotate_Y_Degree
                              (Rot_Matrix, Degree (20.0 * Tim));
                            Trans_Matrix := Translation_Matrix
                              ((0.0, 1.0 + 0.25 * Sin (2.0 * Tim), 0.0));
                            Model_Matrix :=
                              Rot_Matrix * Trans_Matrix * Property.Model_Matrix;
                            Set_Model (Model_Matrix);
                        end if;
                    else
                        Heading_Dia := Degree (20.0 * Tim);
                        Height_Dia := 0.5 * Sin (2.0 * Tim);
                        Translate := Property.World_Pos;
                        Translate (GL.Y) := Translate (GL.Y) + Height_Dia;
                        Property.Heading_Deg := Property.Heading_Deg + Heading_Dia;
                        Rot_Matrix := Rotate_Y_Degree
                          (Rot_Matrix, Property.Heading_Deg);
                        Trans_Matrix := Translation_Matrix  (Translate);
                        Model_Matrix := Rot_Matrix * Trans_Matrix;
                        Set_Model (Model_Matrix);
                        Particle_System.Set_Particle_System_Position
                          (Property.Particle_System_Index,
                           Script.Particles_Offset + Translate);
                        Properties_Manager.Replace_Property (Prop_I, Property);
                    end if;

                    if Continue then
                        if Settings.Render_OLS and Script.Draw_Outlines then
                            GL.Culling.Set_Front_Face (Clockwise);
                            Set_Outline_Pass (1.0);
                            if Script.Outlines_Vertex_Count > 0 then
                                GL_Utils.Bind_VAO (Script.Outlines_Vao);
                                Draw_Arrays
                                  (Triangles, 0, Script.Outlines_Vertex_Count);
                            else
                                GL_Utils.Bind_VAO (Script.Vao);
                                Draw_Arrays (Triangles, 0, Script.Vertex_Count);
                            end if;
                            Set_Outline_Pass (0.0);
                            GL.Culling.Set_Front_Face (Counter_Clockwise);
                        end if;

                        GL_Utils.Bind_VAO (Script.Vao);
                        Texture_Manager.Bind_Texture (0, Script.Diffuse_Map_Id);
                        Draw_Arrays (Triangles, 0, Script.Vertex_Count);
                    end if;
                end if;
            end loop;
        end if;
    end Render_Javelin_Standard;

    --  -------------------------------------------------------------------------

    procedure Render_Portal is
        use GL.Objects.Vertex_Arrays;
        use Singles;
        use Character_Controller;
        use GL_Maths.Indices_Package;
        use Portal_Shader_Manager;
        use Maths;
        use Single_Math_Functions;
        Character     : Barbarian_Character := Get_Character (1);
        Count        : constant Integer := Integer (Jav_Stand_Render_List.Length);
        Property     : Prop_Renderer_Support.Property_Data;
        Script       : Prop_Renderer_Support.Prop_Script;
        Prop_I       : Integer;
        Script_I     : Integer;
        Tim          : constant Single := Single (Glfw.Time);
    begin
        if not Is_Empty (Portal_Render_List) then
            GL.Objects.Programs.Use_Program
              (Properties_Shader_Manager.Portal_Shader);
            Set_Time (Tim);
            if Camera.Is_Dirty then
                Set_View (Camera.View_Matrix);
                Set_Perspective (Camera.Projection_Matrix);
            end if;

            for Param_I in 1 .. Count loop
                Prop_I := Jav_Stand_Render_List (Param_I);
                Property := Properties_Manager.Get_Property_Data (Prop_I);
                Script_I := Get_Script_Index (Prop_I);
--                  Script_I := Properties (Prop_I).Script_Index;
                Script := Properties_Manager.Get_Script_Data (Script_I);
                Set_Model (Property.Model_Matrix);
                GL_Utils.Bind_VAO (Script.Vao);
                Texture_Manager.Bind_Texture (0, Script.Diffuse_Map_Id);
                Draw_Arrays (Triangles, 0, Script.Vertex_Count);
            end loop;
        end if;
    end Render_Portal;

    --  -------------------------------------------------------------------------

    procedure Render_Skinned (Prop_Dyn_Light_Pos_Wor, Prop_Dyn_Light_Diff,
                              Prop_Dyn_Light_Spec : Singles.Vector3;
                              Prop_Dyn_Light_Range : Single := 1.0;
                              Prop_Dyn_Light_Dirty : Boolean := True) is
        use GL.Objects.Vertex_Arrays;
        use GL_Maths.Indices_Package;
        use Properties_Skinned_Shader_Manager;
        Count    : constant Integer := Integer (Skinned_Render_List.Length);
        Property : Prop_Renderer_Support.Property_Data;
        Script   : Prop_Renderer_Support.Prop_Script;
        Prop_I   : Integer;
        Script_I : Integer;
        --          Mesh_I   : Integer;
        Ssi      : Integer;
        U        : Positive;
        V        : Positive;
    begin
        if not Is_Empty (Skinned_Render_List) then
            GL.Objects.Programs.Use_Program
              (Properties_Shader_Manager.Prop_Skinned_Shader);
            if Camera.Is_Dirty then
                Set_View (Camera.View_Matrix);
                Set_Perspective (Camera.Projection_Matrix);
            end if;
            if Prop_Dyn_Light_Dirty then
                Set_Dyn_Light_Pos (Prop_Dyn_Light_Pos_Wor);
                Set_Dyn_Light_Diff (Prop_Dyn_Light_Diff);
                Set_Dyn_Light_Spec (Prop_Dyn_Light_Spec);
                Set_Dyn_Light_Range (Prop_Dyn_Light_Range);
            end if;
            if Settings.Shadows_Enabled then
                Properties_Shader_Manager.Set_Shadows_Enabled (1.0);
                Set_Caster_Position (Shadows.Caster_Position);
                Shadows.Bind_Cube_Shadow_Texture (3);
            else
                Properties_Shader_Manager.Set_Shadows_Enabled (0.0);
            end if;

            for Param_I in 1 .. Count loop
                Prop_I := Basic_Render_List (Param_I);
                Script_I := Get_Script_Index (Prop_I);
                Script := Properties_Manager.Get_Script_Data (Script_I);
                Ssi := Script.Smashed_Script_Index;
                Property := Properties_Manager.Get_Property_Data (Prop_I);
                if Property.Was_Smashed and Ssi > 0 then
                    Script_I := Ssi;
                end if;
                --                  Mesh_I := Script.Mesh_Index;
                Set_Bone_Matrices (Property.Current_Bone_Transforms);
                Set_Model (Property.Model_Matrix);
                U := Positive (Property.Map_U);
                V := Positive (Property.Map_V);
                Set_Static_Light_Indices
                  ((Manifold.Get_Light_Index (U, V, 0),
                   Manifold.Get_Light_Index (U, V, 1)));
                if Settings.Render_OLS and Script.Draw_Outlines then
                    GL.Culling.Set_Front_Face (Clockwise);
                    Set_Outline_Pass (1.0);
                    if Script.Outlines_Vertex_Count > 0 then
                        GL_Utils.Bind_VAO (Script.Outlines_Vao);
                    else
                        GL_Utils.Bind_VAO (Script.Vao);
                        Draw_Arrays (Triangles, 0, Script.Outlines_Vertex_Count);
                        Draw_Arrays (Triangles, 0, Script.Vertex_Count);
                    end if;
                    Set_Outline_Pass (0.0);
                    GL.Culling.Set_Front_Face (Counter_Clockwise);
                end if;
                GL_Utils.Bind_VAO (Script.Vao);
                Texture_Manager.Bind_Texture (0, Script.Diffuse_Map_Id);
                Texture_Manager.Bind_Texture (1, Script.Specular_Map_Id);
                Draw_Arrays (Triangles, 0, Script.Vertex_Count);
            end loop;
        end if;
    end Render_Skinned;

    --  -------------------------------------------------------------------------

    procedure Render_Treasure is
        use GL.Objects.Vertex_Arrays;
        use GL_Maths.Indices_Package;
        use Coins_Shader_Manager;
        Tim      : constant Single := Single (Glfw.Time);
        Count    : constant Integer := Integer (Treasure_Render_List.Length);
        Property : Prop_Renderer_Support.Property_Data;
        Script   : Prop_Renderer_Support.Prop_Script;
        Prop_I   : Integer;
        Script_I : Integer;
    begin
        if not Is_Empty (Treasure_Render_List) then
            GL.Objects.Programs.Use_Program
              (Properties_Shader_Manager.Prop_Skinned_Shader);
            Set_Time (Tim);
            if Camera.Is_Dirty then
                Set_View (Camera.View_Matrix);
                Set_Perspective (Camera.Projection_Matrix);
            end if;

            if Settings.Shadows_Enabled then
                Properties_Shader_Manager.Set_Shadows_Enabled (1.0);
                Set_Caster_Pos_World (Shadows.Caster_Position);
                Shadows.Bind_Cube_Shadow_Texture (1);
            else
                Properties_Shader_Manager.Set_Shadows_Enabled (0.0);
            end if;

            for Param_I in 1 .. Count loop
                Prop_I := Basic_Render_List (Param_I);
                Property := Properties_Manager.Get_Property_Data (Prop_I);
                Script_I := Get_Script_Index (Prop_I);
                Script := Properties_Manager.Get_Script_Data (Script_I);
                Set_Model (Property.Model_Matrix);
                if Settings.Render_OLS and Script.Draw_Outlines then
                    GL.Culling.Set_Front_Face (Clockwise);
                    Set_Outline_Pass (1.0);
                    if Script.Outlines_Vertex_Count > 0 then
                        GL_Utils.Bind_VAO (Script.Outlines_Vao);
                        Draw_Arrays (Triangles, 0, Script.Outlines_Vertex_Count);
                    else
                        GL_Utils.Bind_VAO (Script.Vao);
                        Draw_Arrays (Triangles, 0, Script.Vertex_Count);
                    end if;
                    Set_Outline_Pass (0.0);
                    GL.Culling.Set_Front_Face (Counter_Clockwise);
                end if;
                GL_Utils.Bind_VAO (Script.Vao);
                Texture_Manager.Bind_Texture (0, Script.Diffuse_Map_Id);
                Draw_Arrays (Triangles, 0, Script.Vertex_Count);
            end loop;
        end if;
    end Render_Treasure;

    --  -------------------------------------------------------------------------

end Prop_Renderer.Render;
