
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Textures;

with Texture_Manager;

package body Controller_Textures_Manager is

   type Glyph_Ids is array (1 .. Num_Steam_Controller_Images) of
     GL.Objects.Textures.Texture;
   type Controller_Button_Overlays is record
      Loaded_Glyph_Ids : Glyph_Ids;
   end record;

   Ctrl_Butt_Overlays : Controller_Button_Overlays;

   procedure Load_Controller_Textures is
      Controller_Glyphs : array (1 .. Num_Steam_Controller_Images) of Unbounded_String :=
                            (To_Unbounded_String ("button_a.png"), -- matches enum k_EControllerActionOrigin_A
                             To_Unbounded_String ("button_b.png"),
                             To_Unbounded_String ("button_x.png"),
                             To_Unbounded_String ("button_y.png"),
                             To_Unbounded_String ("shoulder_l.png"), --  LeftBumper??

                             To_Unbounded_String ("shoulder_r.png"),
                             To_Unbounded_String ("grip_l.png"),
                             To_Unbounded_String ("grip_r.png"),
                             To_Unbounded_String ("button_start.png"),
                             To_Unbounded_String ("button_select.png"),  --  Back?

                             To_Unbounded_String ("pad_l_touch.png"),
                             To_Unbounded_String ("pad_l_swipe.png"),
                             To_Unbounded_String ("pad_l_click.png"),
                             To_Unbounded_String ("pad_l_dpad_n.png"),
                             To_Unbounded_String ("pad_l_dpad_s.png"),

                             To_Unbounded_String ("pad_l_dpad_w.png"),
                             To_Unbounded_String ("pad_l_dpad_e.png"),
                             To_Unbounded_String ("pad_r_touch.png"),
                             To_Unbounded_String ("pad_r_swipe.png"),
                             To_Unbounded_String ("pad_r_click.png"),

                             To_Unbounded_String ("pad_r_dpad_n.png"),
                             To_Unbounded_String ("pad_r_dpad_s.png"),
                             To_Unbounded_String ("pad_r_dpad_w.png"),
                             To_Unbounded_String ("pad_r_dpad_e.png"),
                             To_Unbounded_String ("trigger_l_pull.png"),

                             To_Unbounded_String ("trigger_l_click.png"),
                             To_Unbounded_String ("trigger_r_pull.png"),
                             To_Unbounded_String ("trigger_r_click.png"),
                             To_Unbounded_String ("stick_move.png"),    --  lstick?
                             To_Unbounded_String ("stick_click.png"),

                             To_Unbounded_String ("stick_dpad_n.png"),
                             To_Unbounded_String ("stick_dpad_s.png"),
                             To_Unbounded_String ("stick_dpad_w.png"),
                             To_Unbounded_String ("stick_dpad_e.png"),
                             To_Unbounded_String ("gyro.png"),     --   gmove

                             To_Unbounded_String ("gyro.png"),     --   gpitch
                             To_Unbounded_String ("gyro.png"),     --   gyaw
                             To_Unbounded_String ("gyro.png"),     --   groll
                             To_Unbounded_String ("button_steam_off.png"),     --   not enumerated explicitly
                             To_Unbounded_String ("button_steam_on.png"),     --   not enumerated explicitly

                             To_Unbounded_String ("pad_l.png"),      --   not enumerated explicitly
                             To_Unbounded_String ("pad_r.png"),     --   not enumerated explicitly
                             To_Unbounded_String ("stick.png"));
      Dir               : String := "src/textures/steam_controller/";
   begin
      for index in 1 .. Num_Steam_Controller_Images loop
         declare
            Path : String := Dir & To_String (Controller_Glyphs (index));
         begin
            Texture_Manager.Load_Image_To_Texture
              (Path, Ctrl_Butt_Overlays.Loaded_Glyph_Ids (index), False, True);
         end;
      end loop;

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Controller_Textures_Manager.Load_Controller_Textures.");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
         raise;
   end Load_Controller_Textures;

end Controller_Textures_Manager;
