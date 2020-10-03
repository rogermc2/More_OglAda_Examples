
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Types; use GL.Types;

with Game_Utils;
with Tiles_Manager;

package body Audio_Manager is

   Min_Sound_D        : constant Float := 8.0;
   Max_Ambient_Sounds : constant Integer := 128;
   Max_Boulder_Sounds : constant Integer := 64;
   Max_Loaded_Sounds  : constant Integer := 512;
   Max_Music_Tracks   : constant Integer := 128;

   type Ambient_Sound is record
      --  	Snd       : Irrklang::Isound* ;
      X              : Float := 0.0;
      Y              : Float := 0.0;
      Z              : Float := 0.0;
      Cut_Off_Radius : Float := 0.0;
      Map_S          : Int := 0;
      Map_T          : Int := 0;
      H              : Integer := 0;
      File_Name      : Unbounded_String := To_Unbounded_String ("");
   end record;

   type Boulder_Sound is record
      --  	Snd       : Irrklang::Isound* ;
      Pos     : Singles.Vector3 := (0.0, 0.0, 0.0);
      Playing : Boolean := False;
   end record;

   type Loaded_Sound is record
      File_Name      : Unbounded_String := To_Unbounded_String ("");
      --  	Snd       : Irrklang::Isound* ;
   end record;

   type VOM_Data is array (Integer range <>) of UByte;
   type VOM_File is array (Integer range <>) of Unbounded_String;
   type VOM_Size is array (Integer range <>) of Integer;

   type Music_VOM is record
      Data        : VOM_Data (1 .. Max_Music_Tracks);
      File_Names  : VOM_File (1 .. Max_Music_Tracks) :=
                      (others => To_Unbounded_String (""));
      Sz          : VOM_Size (1 .. Max_Music_Tracks) :=
                      (others => 0);
      Num_Tracks  : Integer := 0;
   end record;

   type Ambient_Sound_Array is array (Integer range <>) of Ambient_Sound;
   type Boulder_Sound_Array is array (Integer range <>) of Boulder_Sound;
   type Loaded_Sound_Array is array (Integer range <>) of Loaded_Sound;

   type Audio is record
      --  	 Device          : Irrklang::Isoundengine* ;
      --  	 *Curr_Music_Snd : Irrklang::Isound;
      --  	 *Credits_Snd    :Irrklang::Isound;
      Ambient_Sounds      : Ambient_Sound_Array (1 .. Max_Ambient_Sounds);
      Boulder_Sounds      : Boulder_Sound_Array (1 ..Max_Boulder_Sounds);
      Loaded_Sounds       : Loaded_Sound_Array (1 .. Max_Loaded_Sounds);
      Snd_Vom             : Music_Vom;
      Mus_Vom             : Music_Vom;
      Boulder_Sound_Count : Integer := 0;
      Ambient_Sound_Count : Integer := 0;
      Loaded_Sound_Count  : Integer := 0;
      Device_Name         : Unbounded_String := To_Unbounded_String ("None");
      Was_Init            : Boolean := False;
   end record;

   type Loaded_Sound_Data is record
      File_Name : Unbounded_String := To_Unbounded_String ("");

   end record;

   Game_Audio          : Audio;

   procedure Release_Ambient_Sounds;

   --  ------------------------------------------------------------------------

   procedure Create_Managed_Ambient_Source
     (Sound_File : String; Map_S, Map_T : Int; Height : Integer; Radius : Float) is
      Play_Looped  : Boolean := True;  --  differs
      Start_Paused : Boolean := True;
      Track        : Boolean := True;  --  differs
      Index        : Integer := 0;
   begin
      if Game_Audio.Was_Init then
         if Sound_File'Length < 1 then
            raise Audio_Manager_Exception with
              "Create_Managed_Ambient_Source, invalid sound file name: " &
              Sound_File;
         elsif not Tiles_Manager.Is_Tile_Valid (Map_S, Map_T) then
            raise Audio_Manager_Exception with
              "Create_Managed_Ambient_Source, invalid tile parameters: " &
              Int'Image (Map_S) & ", " & Int'Image (Map_T);
         elsif Game_Audio.Ambient_Sound_Count > Max_Ambient_Sounds then
            raise Audio_Manager_Exception with
              "Create_Managed_Ambient_Source, too many sound sources";
         else
            Game_Utils.Game_Log ("Audio_Manager.Create_Managed_Ambient_Source "
                                 & "creating managed ambient source from " &
                                   Sound_File);
            Index := Index + 1;
            Game_Audio.Ambient_Sounds (Index).File_Name :=
              To_Unbounded_String (Sound_File);
--              Game_Audio.Ambient_Sounds (Index).Snd := Snd;
            Game_Audio.Ambient_Sounds (Index).X := 2.0 * Float (Map_S);
            Game_Audio.Ambient_Sounds (Index).Y := 2.0 * Float (Height);
            Game_Audio.Ambient_Sounds (Index).Z := 2.0 * Float (Map_T);
            Game_Audio.Ambient_Sounds (Index).Cut_Off_Radius := Radius;
            Game_Audio.Ambient_Sounds (Index).Map_S := Map_S;
            Game_Audio.Ambient_Sounds (Index).Map_T := Map_T;
            Game_Audio.Ambient_Sounds (Index).H := Height;
         end if;
      end if;
   end Create_Managed_Ambient_Source;

   --  ------------------------------------------------------------------------

   function Init_Audio return Boolean is
   begin
      Game_Audio.Device_Name := To_Unbounded_String ("None");
      return False;
   end Init_Audio;

   --  ------------------------------------------------------------------------

   procedure Load_Ambient_Sounds (Input_File : File_Type) is
      use Ada.Strings;
      aLine      : constant String := Get_Line (Input_File);
      Last       : constant Integer := Integer (aLine'Last);
      Sound_File : Unbounded_String;
      Pos1       : Integer := Fixed.Index (aLine, " ");
      Pos2       : Integer;
      Num_Sounds : Integer;
      S          : Int;
      T          : Int;
      Height     : Integer;
      Radius     : Float;
   begin
      Release_Ambient_Sounds;
      Num_Sounds := Integer'Value (aLine (Pos1 + 1 .. Last));
      for count in 1 .. Num_Sounds loop
         declare
            aString : constant String := Get_Line (Input_File);
         begin
            Pos1 := Fixed.Index (aLine, " ");
            Sound_File := To_Unbounded_String (aLine (1 .. Pos1 - 1));
            Pos2 := Fixed.Index (aLine (Pos1 + 1 .. Last), ",");
            S := Int'Value (aLine (Pos1 + 1 .. Pos2 - 1));
            Pos1 := Fixed.Index (aLine (Pos2 + 2 .. Last), " ");
            T := Int'Value (aLine (Pos2 + 1 .. Pos1 - 1));
            Pos2 := Fixed.Index (aLine (Pos1 + 1 .. Last), " ");
            Height := Integer'Value (aLine (Pos1 + 1 .. Pos2 - 1));
            Radius := Float'Value (aLine (Pos2 + 1 .. Last));
         end;
         Create_Managed_Ambient_Source (To_String (Sound_File),
                                        S, T, Height, Radius);
      end loop;
   end Load_Ambient_Sounds;

   --  ------------------------------------------------------------------------

   procedure Release_Ambient_Sounds is
   begin
      for index in 1 .. Game_Audio.Ambient_Sound_Count loop
--           if Game_Audio.Ambient_Sounds (index).Snd /= 0 then
--              null;
--           end if;
         Game_Audio.Ambient_Sound_Count := 0;
      end loop;
   end Release_Ambient_Sounds;

   --  ------------------------------------------------------------------------

end Audio_Manager;
