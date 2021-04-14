
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
                        Tile_Diff_Tex, Tile_Spec_Tex, Ramp_Diff_Tex,
                        Ramp_Spec_Tex : in out GL.Objects.Textures.Texture) is
      use Ada.Strings;
      Input_File       : File_Type;
      aLine            : Unbounded_String;
      Pos1             : Natural;
      Num_Story_Lines  : Natural;
      --          Story_Lines      : Story_Lines_List;
   begin
      Game_Utils.Game_Log ("Maps_Manager.Load_Maps opening " & Map_Path);
      Open (Input_File, In_File, Map_Path);
--        Game_Utils.Game_Log ("Maps_Manager.Load_Maps, " & Map_Path & " opened.");
      theMap.Level_Title := To_Unbounded_String (Get_Line (Input_File));
      Game_Utils.Game_Log ("Maps_Manager.Load_Maps Level_Title " &
                             To_String (theMap.Level_Title));
      theMap.Level_Par_Time := To_Unbounded_String (Get_Line (Input_File));

      --  Story
      aLine := To_Unbounded_String (Get_Line (Input_File));
      Pos1 := Index (aLine, " ");
      Num_Story_Lines := Integer'Value (Slice (aLine, Pos1, Length (aLine)));
     --  Skip story lines
      for line_num in 1 .. Num_Story_Lines loop
         aLine := To_Unbounded_String (Get_Line (Input_File));
      end loop;
      --  Skip Music_Track
      aLine := To_Unbounded_String (Get_Line (Input_File));
      --  Skip Hammer_Music_Track
      aLine := To_Unbounded_String (Get_Line (Input_File));

      Tiles_Manager.Load_Tiles (Input_File, Tile_Diff_Tex, Tile_Spec_Tex,
                                Ramp_Diff_Tex, Ramp_Spec_Tex);
      Game_Utils.Game_Log ("Maps_Manager.Load_Maps, Tiles loaded");
      Properties_Manager.Load_Properties (Input_File);
--        Game_Utils.Game_Log ("Maps_Manager.Load_Maps, Properties loaded");
      Character_Controller.Init;
      Character_Controller.Load_Characters (Input_File, False);
--        Game_Utils.Game_Log ("Maps_Manager.Load_Maps, Characters loaded");
      Projectile_Manager.Init;
      Audio_Manager.Load_Ambient_Sounds (Input_File);
      Text_Manager.Preload_Comic_Texts (Input_File);
      Game_Utils.Game_Log ("Maps_Manager.Load_Maps, Comic_Texts loaded");
      Close (Input_File);

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
