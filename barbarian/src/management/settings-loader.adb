
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

with Game_Utils;
with Input_Handler;

package body Settings.Loader is

   --  ------------------------------------------------------------------------

   procedure Load_Settings (theSettings : in out Settings.Settings_Data) is
      use Ada.Strings;
      Input_File      : File_Type;
   begin
      Open (Input_File, In_File, "src/settings.cfg");
      while not End_Of_File (Input_File) loop
         declare
            aLine : constant String := Get_Line (Input_File);
            Last  : constant Integer := aLine'Length;
            Pos1  : constant Natural := Fixed.Index (aLine, "_");
            Head  : constant String := aLine (1 .. Pos1);
            Tail  : constant String := aLine (Pos1 + 1 .. Last);
            Pos2  : constant Natural := Fixed.Index (Tail, "_");
            Pos3  : Natural := Fixed.Index (Tail, " ");
            Pos4  : Natural;
            V1    : Integer := 0;
         begin
            if Pos3 = 0 then
               Pos3 := Last;
            end if;

            if Head = "KEY_" then
               Input_Handler.Read_Key_Config (aLine);
            elsif Head = "Joy_" then
               if Pos2 /= 0 then
                  if Tail (Pos2 + 1 .. Pos3 - 1) = "ENABLED" then
                     V1 := Integer'Value (Tail (Pos3 + 1 .. Last));
                     theSettings.Disable_Joystick := V1 /= 0;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "AXIS_THRESH" then
                     theSettings.Joy_Axis_Threshold := Float'Value (Tail (Pos3 + 1 .. Last));
                  end if;
               else
                  Input_Handler.Read_Key_Config (aLine);
               end if;

            elsif Head = "GFX_" then
               if Pos2 /= 0 then
                  if Tail (Pos2 + 1 .. Pos3 - 1) = "PRESETS" then
                     V1 := Integer'Value (Tail (Pos3 + 1 .. Last));
                     theSettings.Gfx_Presets := Settings.Gfx_Preset_Type'Enum_Val (V1);
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "GL_V" then
                     V1 := Integer'Value (Tail (Pos3 + 1 .. Last));
                     theSettings.GL_Version := Settings.V_GL'Enum_Val (V1);
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "RES" then
                     Pos4 := Fixed.Index (Tail (Pos3 + 1 .. Last), "x");
                     theSettings.GL_Window_Width := Integer'Value (Tail (Pos3 + 1 .. Pos4 - 1));
                     theSettings.GL_Window_Height := Integer'Value (Tail (Pos4 + 1 .. Last));
                     theSettings.GL_Window_Width_To_Save := theSettings.GL_Window_Width;
                     theSettings.GL_Window_Height_To_Save := theSettings.GL_Window_Height;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "FULLSCREEN" then
                     theSettings.Full_Screen := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "VSYNC" then
                     theSettings.V_Sync := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "MSAA_FACTOR" then
                     theSettings.Multi_Sample_Anti_Aliasing := Integer'Value (Tail (Pos3 + 1 .. Last));
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "SSAA_FACTOR" then
                     theSettings.Super_Sample_Anti_Aliasing := Float'Value (Tail (Pos3 + 1 .. Last));
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "RENDER_DIST" then
                     theSettings.Render_Distance := Integer'Value (Tail (Pos3 + 1 .. Last));
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "FAR_CLIP" then
                     theSettings.Far_Clip := GL.Types.Single'Value (Tail (Pos3 + 1 .. Last));
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "TILE_BATCH_WIDTH" then
                     theSettings.Tile_Batch_Width := Integer'Value (Tail (Pos3 + 1 .. Last));
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "TEXTURE_FILTER_MODE" then
                     theSettings.Texture_Filtering := Integer'Value (Tail (Pos3 + 1 .. Last));
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "ANISOTROPY_FACTOR" then
                     theSettings.Anisotroic_Texturing_Factor := Integer'Value (Tail (Pos3 + 1 .. Last));
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "RENDER_OUTLINES" then
                     theSettings.Render_OLS := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "SHADOWS_ENABLED" then
                     theSettings.Shadows_Enabled := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "SHADOW_SIZE" then
                     theSettings.Shadow_Size := Integer'Value (Tail (Pos3 + 1 .. Last));
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "FB_EFFECTS_ENABLED" then
                     theSettings.Fb_Effects_Enabled := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "AUTO_BLOOD_WIPE" then
                     theSettings.Auto_Blood_Wipe := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "PARTICLES_ENABLED" then
                     theSettings.Particles_Enabled := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "PARTICLE_MIPS_ENABLED" then
                     theSettings.Particle_Mipmaps_Enabled := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "SHOW_FPS" then
                     theSettings.Show_Fps := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "VID_REC_MODE" then
                     theSettings.Vid_Rec_Mode := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  end if;
               else
                  Input_Handler.Read_Key_Config (aLine);
               end if;
            elsif Head = "AUD_" then
               if Pos2 /= 0 then
                  if Tail (Pos2 + 1 .. Pos3 - 1) = "MASTER_VOLUME" then
                     theSettings.Audio_Volume :=  Integer'Value (Tail (Pos3 + 1 .. Last));
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "MUSIC_VOLUME" then
                     theSettings.Music_Volume := Integer'Value (Tail (Pos3 + 1 .. Last));
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "AUD_ALLOW_RAND_PITCH" then
                     theSettings.Allow_Rand_Pitch := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  end if;
               end if;
            end if;
         end;  --  declare block
      end loop;
      Close (Input_File);

   end Load_Settings;

   --  ------------------------------------------------------------------------

end Settings.Loader;
