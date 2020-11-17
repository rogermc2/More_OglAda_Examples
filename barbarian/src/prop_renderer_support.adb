
with Glfw;

with GL.Objects.Programs;

with Camera;
with Coins_Shader_Manager;
with Depth_Skinned_Shader_Manager;
with Jav_Stand_Shader_Manager;
with Manifold;
with Portal_Shader_Manager;
with Properties_Shader_Manager;
with Properties_Skinned_Shader_Manager;
with Settings;
with Shadows;

package body Prop_Renderer_Support is

  Model_Matrix                  : GL.Types.Singles.Matrix4 := (others => (others => 0.0));

   --  -------------------------------------------------------------------------

   procedure Set_Shaders (Property : Property_Data; Prop_Type : Property_Type) is
      use Properties_Shader_Manager;
   begin
      if Prop_Type = Door_Prop or Prop_Type = Pillar_Prop or
        Prop_Type = Anim_Loop_Prop or Prop_Type = Windlass_Prop then
         GL.Objects.Programs.Use_Program (Prop_Skinned_Shader);
         if Camera.Is_Dirty then
            Properties_Skinned_Shader_Manager.Set_View (Camera.View_Matrix);
            Properties_Skinned_Shader_Manager.Set_Perspective (Camera.Projection_Matrix);
         end if;
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
