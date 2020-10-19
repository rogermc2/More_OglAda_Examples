
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

with Game_Utils;
with Input_Handler;

package body Settings_Loader is

   --  ------------------------------------------------------------------------

   procedure Load_Settings is
      use Ada.Strings;
      Input_File : File_Type;
      Loaded     : Settings.Settings;
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
            V1     : Integer := 0;
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
                     Loaded.Disable_Joystick := V1 /= 0;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "AXIS_THRESH" then
                     Loaded.Joy_Axis_Threshold := Float'Value (Tail (Pos3 + 1 .. Last));
                  end if;
               else
                  Input_Handler.Read_Key_Config (aLine);
               end if;

            elsif Head = "GFX_" then
               if Pos2 /= 0 then
                  if Tail (Pos2 + 1 .. Pos3 - 1) = "PRESETS" then
                     V1 := Integer'Value (Tail (Pos3 + 1 .. Last));
                     Loaded.Gfx_Presets := Settings.Gfx_Preset_Type'Enum_Val (V1);
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "GL_V" then
                     V1 := Integer'Value (Tail (Pos3 + 1 .. Last));
                     Loaded.GL_Version := Settings.V_GL'Enum_Val (V1);
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "RES" then
                     Pos4 := Fixed.Index (Tail (Pos3 + 1 .. Last), "x");
                     Loaded.GL_Window_Width := Integer'Value (Tail (Pos3 + 1 .. Pos4 - 1));
                     Loaded.GL_Window_Height := Integer'Value (Tail (Pos4 + 1 .. Last));
                     Loaded.GL_Window_Width_To_Save := Loaded.GL_Window_Width;
                     Loaded.GL_Window_Height_To_Save := Loaded.GL_Window_Height;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "FULLSCREEN" then
                     Loaded.Full_Screen := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "VSYNC" then
                     Loaded.V_Sync := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "MSAA_FACTOR" then
                     Loaded.Multi_Sample_Anti_Aliasing := Integer'Value (Tail (Pos3 + 1 .. Last));
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "SSAA_FACTOR" then
                     Loaded.Super_Sample_Anti_Aliasing := Float'Value (Tail (Pos3 + 1 .. Last));
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "RENDER_DIST" then
                     Loaded.Render_Distance := Integer'Value (Tail (Pos3 + 1 .. Last));
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "FAR_CLIP" then
                     Loaded.Far_Clip := GL.Types.Single'Value (Tail (Pos3 + 1 .. Last));
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "TILE_BATCH_WIDTH" then
                     Loaded.Tile_Batch_Width := Integer'Value (Tail (Pos3 + 1 .. Last));
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "TEXTURE_FILTER_MODE" then
                     Loaded.Texture_Filtering := Integer'Value (Tail (Pos3 + 1 .. Last));
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "ANISOTROPY_FACTOR" then
                     Loaded.Anisotroic_Texturing_Factor := Integer'Value (Tail (Pos3 + 1 .. Last));
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "RENDER_OUTLINES" then
                     Loaded.Render_OLS := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "SHADOWS_ENABLED" then
                     Loaded.Shadows_Enabled := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "SHADOW_SIZE" then
                     Loaded.Shadow_Size := Integer'Value (Tail (Pos3 + 1 .. Last));
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "FB_EFFECTS_ENABLED" then
                     Loaded.Fb_Effects_Enabled := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "AUTO_BLOOD_WIPE" then
                     Loaded.Auto_Blood_Wipe := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "PARTICLES_ENABLED" then
                     Loaded.Particles_Enabled := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "PARTICLE_MIPS_ENABLED" then
                     Loaded.Particle_Mipmaps_Enabled := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "SHOW_FPS" then
                     Loaded.Show_Fps := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "VID_REC_MODE" then
                     Loaded.Vid_Rec_Mode := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  end if;
               else
                  Input_Handler.Read_Key_Config (aLine);
               end if;
            elsif Head = "AUD_" then
               if Pos2 /= 0 then
                  if Tail (Pos2 + 1 .. Pos3 - 1) = "MASTER_VOLUME" then
                     Loaded.Audio_Volume :=  Integer'Value (Tail (Pos3 + 1 .. Last));
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "MUSIC_VOLUME" then
                     Loaded.Music_Volume := Integer'Value (Tail (Pos3 + 1 .. Last));
                  elsif Tail (Pos2 + 1 .. Pos3 - 1) = "AUD_ALLOW_RAND_PITCH" then
                     Loaded.Allow_Rand_Pitch := Integer'Value (Tail (Pos3 + 1 .. Last)) /= 0;
                  end if;
               end if;
            end if;
         end;  --  declare block
      end loop;
      Close (Input_File);
      Settings.Load_Settings (Loaded);

   end Load_Settings;

   --  ------------------------------------------------------------------------

end Settings_Loader;
