
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;
with Maths;

with Audio_Manager;
with Batch_Manager;
with Character_Controller;
with Game_Utils;
with GUI_Level_Chooser;
with Projectile_Manager;
with Properties_Manager;
with Text_Manager;
with Tiles_Manager;

package body Maps_Manager is

   type Static_Light_Data is record
      Row      : Positive;
      Column   : Positive;
      Position : GL.Types.Singles.Vector3 := Maths.Vec3_0;
      Diffuse  : GL.Types.Singles.Vector3 := Maths.Vec3_0;
      Specular : GL.Types.Singles.Vector3 := Maths.Vec3_0;
      Distance : GL.Types.Single := 0.0;
   end record;

   package Static_Light_Package is new Ada.Containers.Vectors
     (Positive, Static_Light_Data);
   type Static_Light_List is new Static_Light_Package.Vector with null record;

   Static_Lights : Static_Light_List;

   --  ----------------------------------------------------------------------------

   procedure Add_Static_Light (Row, Col                  : Tiles_Manager.Tiles_RC_Index;
                               Tile_Height_Offset        : Integer;
                               Offset, Diffuse, Specular : GL.Types.Singles.Vector3;
                               Light_Range               : GL.Types.Single);

   --  ----------------------------------------------------------------------------

   procedure Add_Dummy_Manifold_Lights is
      use Maths;
   begin
      Add_Static_Light (1, 1, 0, Vec3_0, Vec3_0, Vec3_0, 0.0);
      Add_Static_Light (1, 1, 0, Vec3_0, Vec3_0, Vec3_0, 0.0);

   end Add_Dummy_Manifold_Lights;

   --  ----------------------------------------------------------------------------

   procedure Add_Static_Light (Row, Col                  : Tiles_Manager.Tiles_RC_Index;
                               Tile_Height_Offset        : Integer;
                               Offset, Diffuse, Specular : GL.Types.Singles.Vector3;
                               Light_Range               : GL.Types.Single) is
      use GL.Types;
      use Batch_Manager;
      use Batches_Package;
      use Tiles_Manager;
      Curs          : Batches_Package.Cursor := Batch_List.First;
      aBatch        : Batch_Meta;
      Tile_Index    : constant Tiles_RC_Index := Row * Positive (Max_Map_Cols) + Col;
      X             : constant Single := Single (2 * Col) + Offset (GL.X);
      Y             : constant Single :=
                        Single (2 * Get_Tile_Level (Tile_Index) +
                                  Tile_Height_Offset) + Offset (GL.Y);
      Z             : constant Single := Single (2 * (Row - 1)) + Offset (GL.Z);
      Total_Batches : constant Integer := Batches_Across * Batches_Down;
      --          Sorted        : Boolean := False;
      New_Light     : Static_Light_Data;
   begin
      --        Put_Line ("Tiles_Manager.Add_Static_Light Total_Batches: " &
      --                 Integer'Image (Total_Batches));
      New_Light.Row := Positive (Row);
      New_Light.Column := Positive (Col);
      New_Light.Position := (X, Y, Z);
      New_Light.Diffuse := Diffuse;
      New_Light.Specular := Specular;
      New_Light.Distance := Light_Range;
      Static_Lights.Append (New_Light);

      if Batch_List.Is_Empty then
         raise Tiles_Manager_Exception with
           "Tiles_Manager.Add_Static_Light Batch_List is empty! ";
      end if;

      --        Put_Line ("Tiles_Manager.Add_Static_Light Batch_List size: " &
      --                 Integer'Image (Integer (Batch_List.Length)));
      for index in 0 .. Total_Batches - 1 loop
         --           Put_Line ("Tiles_Manager.Add_Static_Light index: " &
         --                       Integer'Image (index));
         aBatch := Batch_List.Element (index);
         aBatch.Static_Light_Indices.Append (Static_Lights.Last_Index);
         Update_Batch (index, aBatch);
      end loop;

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Tiles_Manager.Add_Static_Light!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
         raise;

   end Add_Static_Light;

   --  ----------------------------------------------------------------------------

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
      Tiles_Manager.Add_Tiles_To_Batches;
      --          Game_Utils.Game_Log ("Maps_Manager.Load_Maps, Tiles added To_Batches.");
      Add_Dummy_Manifold_Lights;
      Game_Utils.Game_Log ("Maps_Manager.Load_Maps, Tiles loaded and Manifold generated.");
      Properties_Manager.Load_Properties (Input_File);
      Game_Utils.Game_Log ("Maps_Manager.Load_Maps, Properties loaded");
      Character_Controller.Init;
      Character_Controller.Load_Characters (Input_File, False);
      Game_Utils.Game_Log ("Maps_Manager.Load_Maps, Characters loaded");
      Projectile_Manager.Init;
      Audio_Manager.Load_Ambient_Sounds (Input_File);
      Text_Manager.Preload_Comic_Texts (Input_File);
      Game_Utils.Game_Log ("Maps_Manager.Load_Maps, Comic_Texts loaded");
      Close (Input_File);

--         Batch_Manager.Print_Points ("Maps_Manager.Load_Maps", 0);

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
