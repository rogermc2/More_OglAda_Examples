
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Audio_Manager;
with Character_Controller;
with Game_Utils;
with GUI_Level_Chooser;
with Projectile_Manager;
with Properties_Manager;
with Text_Manager;
with Tiles_Manager;

package body Maps_Manager is

   procedure Load_Maps (Map_Path : String; theMap : out Map;
                        Tile_Tex, Tile_Spec_Tex, Ramp_Diff_Tex,
                        Ramp_Spec_Tex : in out GL.Objects.Textures.Texture) is
      use Ada.Strings;
      Input_File       : File_Type;
      aLine            : Unbounded_String;
      Pos1             : Natural;
      Num_Story_Lines  : Natural;
      --          Story_Lines      : Story_Lines_List;
   begin
--        Put_Line ("Maps_Manager.Load_Maps opening " & Map_Path);
      Game_Utils.Game_Log ("Maps_Manager.Load_Maps opening " & Map_Path);
      Open (Input_File, In_File, Map_Path);
--        Put_Line ("Maps_Manager.Load_Maps, " & Map_Path & " opened.");
      Game_Utils.Game_Log ("Maps_Manager.Load_Maps, " & Map_Path & " opened.");
      theMap.Level_Title := To_Unbounded_String (Get_Line (Input_File));
      theMap.Level_Par_Time := To_Unbounded_String (Get_Line (Input_File));

      --  Story
      aLine := To_Unbounded_String (Get_Line (Input_File));
      Pos1 := Index (aLine, " ");
      Num_Story_Lines := Integer'Value (Slice (aLine, Pos1, Length (aLine)));
      for line_num in 1 .. Num_Story_Lines loop
         aLine := To_Unbounded_String (Get_Line (Input_File));
      end loop;
      --  Music_Track
      aLine := To_Unbounded_String (Get_Line (Input_File));
      --  Hammer_Music_Track
      aLine := To_Unbounded_String (Get_Line (Input_File));

--        Put_Line ("Maps_Manager.Load_Maps loading tiles ");
      Tiles_Manager.Load_Tiles (Input_File, Tile_Tex, Tile_Spec_Tex,
                                Ramp_Diff_Tex, Ramp_Spec_Tex);
      Properties_Manager.Load_Properties (Input_File);
      Character_Controller.Init;
      Character_Controller.Load_Characters (Input_File, False);
      Projectile_Manager.Init;
      Audio_Manager.Load_Ambient_Sounds (Input_File);
      Text_Manager.Preload_Comic_Texts (Input_File);
      Close (Input_File);
      Game_Utils.Game_Log ("Maps_Manager.Load_Maps loaded: " & Map_Path);

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Maps_Manager.Load_Maps!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
   end Load_Maps;

   --  ----------------------------------------------------------------------------

   procedure Set_Title (aMap : in out Map; Title : Unbounded_String) is
   begin
      aMap.Level_Title := Title;
   end Set_Title;

   --  ----------------------------------------------------------------------------

   procedure Set_Par_Time (aMap : in out Map; Time : Unbounded_String) is
   begin
      aMap.Level_Title := Time;
   end Set_Par_Time;

   --  ----------------------------------------------------------------------------

   procedure Set_Music_Track (aMap : in out Map; Track : Unbounded_String) is
   begin
      aMap.Music_Track := Track;
   end Set_Music_Track;

   --  ----------------------------------------------------------------------------

   procedure Set_Hammer_Music_Track (aMap : in out Map; Track : Unbounded_String) is
   begin
      aMap.Hammer_Music_Track := Track;
   end Set_Hammer_Music_Track;

   --  ----------------------------------------------------------------------------

end Maps_Manager;
