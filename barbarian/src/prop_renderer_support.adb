
with Glfw;

with  GL.Culling;
with GL.Objects.Programs;

with Camera;
with Character_Controller;
with Coins_Shader_Manager;
with Depth_Skinned_Shader_Manager;
with GL_Maths;
with GL_Utils;
with Jav_Stand_Shader_Manager;
with Manifold;
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
           Int (Character_Controller.Javelin_Count (1)) >=
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
               Model_Matrix := Property.Model_Mat * Trans_Matrix * Rot_Matrix;
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

         Properties_Skinned_Shader_Manager.Set_Bone_Matrices (Property.Current_Bone_Transforms);
         Properties_Skinned_Shader_Manager.Set_Model (Property.Model_Mat);
         Properties_Skinned_Shader_Manager.Set_Static_Light_Indices
           ((Manifold.Get_Light_Index (Property.Map_U, Property.Map_V, 0),
            Manifold.Get_Light_Index (Property.Map_U, Property.Map_V, 1)));

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
            Set_Shadow_Enabled (0.0);
         end if;
         Coins_Shader_Manager.Set_Model (Property.Model_Mat);
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
         Portal_Shader_Manager.Set_Model (Property.Model_Mat);
         Portal_Shader_Manager.Set_Time (Single (Glfw.Time));

      else
         GL.Objects.Programs.Use_Program (Prop_Shader);
         if Camera.Is_Dirty then
            Properties_Shader_Manager.Set_View (Camera.View_Matrix);
            Properties_Shader_Manager.Set_Perspective (Camera.Projection_Matrix);
         end if;
         if Settings.Shadows_Enabled then
            Properties_Shader_Manager.Set_Shadow_Enabled (1.0);
            Properties_Shader_Manager.Set_Caster_Position (Shadows.Caster_Position);
            Shadows.Bind_Cube_Shadow_Texture (3);
         else
            Properties_Shader_Manager.Set_Shadow_Enabled (0.0);
         end if;
         Properties_Shader_Manager.Set_Model (Property.Model_Mat);
         Properties_Shader_Manager.Set_Inverse_Map
           (From_Real_Matrix4 (Inverse (To_Real_Matrix4 (Property.Model_Mat))));
         Properties_Shader_Manager.Set_Static_Light_Indices
           ((Manifold.Get_Light_Index (Property.Map_U, Property.Map_V, 0),
            Manifold.Get_Light_Index (Property.Map_U, Property.Map_V, 1)));
      end if;

   end Set_Shaders;

   --  -------------------------------------------------------------------------

end Prop_Renderer_Support;