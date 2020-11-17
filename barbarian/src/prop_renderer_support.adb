
with Glfw;

with GL.Objects.Programs;

with Camera;
with Character_Controller;
with Coins_Shader_Manager;
with Depth_Skinned_Shader_Manager;
with Jav_Stand_Shader_Manager;
with Manifold;
with Portal_Shader_Manager;
with Properties_Shader_Manager;
with Properties_Skinned_Shader_Manager;
with Specs_Manager;
with Settings;
with Shadows;

package body Prop_Renderer_Support is

   --    generic
   --        type Shader_Directory is (<>);
   --     procedure Set_Camera (View, Perspective : Singles.Matrix4);
   --
   --     procedure Set_Camera (View, Perspective : Singles.Matrix4) is
   --     begin
   --         if Camera.Is_Dirty then
   --              Shader_Directory.Set_View (Camera.View_Matrix);
   --              Shader_Directory.Set_Perspective (Camera.Projection_Matrix);
   --           end if;
   --     end Set_Camera;

   --  -------------------------------------------------------------------------

   procedure Set_Shaders (Property     : Property_Data; Prop_Type : Property_Type;
                          Gold_Current : Integer) is
      use Properties_Shader_Manager;
      use Singles;
      use Maths;
      use Single_Math_Functions;
      aCharacter   : Character_Controller.Barbarian_Character;
      CSI          : Integer;
      Curr_Time    : Single;
      Rot_Matrix   : Matrix4 := Identity4;
      Trans_Matrix : Matrix4 := Identity4;
      Model_Matrix : Matrix4 := Identity4;
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
         GL.Objects.Programs.Use_Program (Jav_Stand_Shader);
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
         end if;

      elsif Prop_Type = Portal_Prop then
         GL.Objects.Programs.Use_Program (Portal_Shader);
         if Camera.Is_Dirty then
            Portal_Shader_Manager.Set_View (Camera.View_Matrix);
            Portal_Shader_Manager.Set_Perspective (Camera.Projection_Matrix);
         end if;
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
      end if;

   end Set_Shaders;

   --  -------------------------------------------------------------------------

end Prop_Renderer_Support;
