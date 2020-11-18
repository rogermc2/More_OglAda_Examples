
with Glfw;
with Glfw.Input.Keys;

with Utilities;

with Audio;
with Blood_Splats;
with Character_Map;
with Event_Controller;
with FB_Effects;
with GUI;
with GUI_Level_Chooser;
with Input_Callback;
with Manifold;
with Particle_System;
with Prop_Renderer;
with Settings;
with Shadows;
with Sprite_Renderer;
with Sprite_World_Map;
with Text;
with Transparency;

package body Main_Loop.Game_Support is

   Shadow_Caster_Max_Tiles_Away : constant GL.Types.Int := 10;

   function Cheat_Check_1 return Boolean is
      use Glfw.Input.Keys;
      Cheating : Boolean := False;
   begin
      if Input_Callback.Is_Key_Down (M) and
        Input_Callback.Is_Key_Down (L) and
        Input_Callback.Is_Key_Down (I) then
         Cheating := True;

      elsif Input_Callback.Is_Key_Down (A) and
        Input_Callback.Is_Key_Down (N) and
        Input_Callback.Is_Key_Down (T) then
         Cheating := True;
      elsif Input_Callback.Is_Key_Down (R) and
        Input_Callback.Is_Key_Down (O) and
        Input_Callback.Is_Key_Down (M) then
         Cheating := True;
      elsif Input_Callback.Is_Key_Down (D) and
        Input_Callback.Is_Key_Down (A) and
        Input_Callback.Is_Key_Down (V) then
         Cheating := True;
      end if;
      return Cheating;
   end Cheat_Check_1;

   --  -------------------------------------------------------------------------

   procedure Check_Victory_Defeat is
   begin
       null;
   end Check_Victory_Defeat;

   --  -------------------------------------------------------------------------

   procedure Player_1_View is
      use GL.Types;
      use Shadows;
      Camera_Position : constant Singles.Vector3 := Camera.World_Position;
      Centre_X        : constant Int := Int ((1.0 + Camera_Position (GL.X)) / 2.0);
      Centre_Z        : constant Int := Int ((1.0 + Camera_Position (GL.Z)) / 2.0);
   begin
      if Settings.Shadows_Enabled and Camera.Is_Dirty then
         for index in Shadow_Direction'Range loop
            Bind_Shadow_FB (index);
            Manifold.Draw_Manifold_Around_Depth_Only;
            Prop_Renderer.Render_Props_Around_Depth_Only
              (Centre_X, Centre_Z, Shadow_Caster_Max_Tiles_Away);
         end loop;
      end if;   --  end of shadow mapping pass

      FB_Effects.Bind_Main_Scene_FB;
      Utilities.Clear_Colour_Buffer_And_Depth;
      Transparency.Reset_Transparency_List (Camera_Position);
      Manifold.Draw_Manifold_Around (Camera_Position,
                                     Single (Settings.Render_Distance));
      Blood_Splats.Render_Splats;
      Prop_Renderer.Render_Props_Around_Split (Centre_X, Centre_Z,
                                               Int (Settings.Render_Distance));
      Sprite_World_Map.Cache_Sprites_Around (Centre_X, Centre_Z,
                                             Int (Settings.Render_Distance));
      Transparency.Draw_Transparency_List;

   end Player_1_View;

   --  -------------------------------------------------------------------------

   procedure Unload_Level is
      use GUI_Level_Chooser;
   begin
      Set_Cheated_On_Map (False);
      Set_Boulder_Crushes (0);
      Set_Hammer_Kills (0);
      Set_Fall_Kills (0);
      Camera.Set_Camera_Position ((2.0, 10.0, 2.0));
      FB_Effects.Set_Feedback_Effect (FB_Effects.FB_Default);
      Event_Controller.Reset;
      Text.Unload_Comic_Texts;
      Sprite_World_Map.Free_Sprite_World_Map;
      Sprite_Renderer.Clear_Sprites;
      Manifold.Clear_Manifold_Lights;
      Particle_System.Stop_Particle_Systems;
      GUI.Reset_GUIs;
      Prop_Renderer.Reset_Properties;
      Character_Map.Free_Character_Map;
      Blood_Splats.Clear_Splats;
      Manifold.Free_Manifold_Mesh_Data;
      Manifold.Reset_Manifold_Vars;
      Audio.Stop_All_Sounds;
      Camera.Set_Screen_Shake_Countdown (0.0);
   end  Unload_Level;

   --  -------------------------------------------------------------------------

   procedure Update_Timers (Last_Time, Delta_Time, Avg_Frame_Time_Accum_Ms,
                            Curr_Frame_Time_Accum_Ms            : in out Float;
                            Avg_Frames_Count, Curr_Frames_Count : in out Integer) is
      Current_Time     : constant Float := Float (Glfw.Time);
      Delta_Time_Ms    : Float := 0.0;
   begin
      Delta_Time := Current_Time - Last_Time;
      Delta_Time_Ms := 1000.0 * Delta_Time;
      Last_Time := Current_Time;
      Avg_Frame_Time_Accum_Ms := Avg_Frame_Time_Accum_Ms + Delta_Time_Ms;
      Curr_Frame_Time_Accum_Ms := Curr_Frame_Time_Accum_Ms + Delta_Time_Ms;
      Avg_Frames_Count := Avg_Frames_Count + 1;
      Curr_Frames_Count := Curr_Frames_Count + 1;
      if Avg_Frames_Count > 999 then
         Avg_Frames_Count := 0;
         Avg_Frame_Time_Accum_Ms := 0.0;
      end if;
      if Curr_Frames_Count > 99 then
         Curr_Frames_Count := 0;
         Curr_Frame_Time_Accum_Ms := 0.0;
      end if;

   end Update_Timers;

end Main_Loop.Game_Support;
