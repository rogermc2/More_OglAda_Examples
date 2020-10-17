
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Types;

with GID_Image_Loader;

with Game_Utils;
with Settings;

package body Texture_Manager is

   type Mod_16 is mod 2 ** 16;

   package Bound_Textures_Package is new Ada.Containers.Vectors
     (Positive, GL.Objects.Textures.Texture);
   type Bound_Textures_List is new Bound_Textures_Package.Vector with null record;

   type Loaded_Texture is record
      File_Name   : Unbounded_String;
      Texture_ID  : GL.Types.UInt;
      Has_Mipmaps : Boolean := False;
   end record;

   package Loaded_Textures_Package is new Ada.Containers.Vectors
     (Natural, Loaded_Texture);
   type Loaded_Textures_List is new Loaded_Textures_Package.Vector with null record;

   --      Max_Textures         : constant Integer := 256;
   --      Loaded_Texture_Count_Allocated : Integer := Max_Textures;
   Loaded_Texture_Count : Integer := 0;
   Bound_Textures       : Bound_Textures_List;
   Loaded_Textures      : Loaded_Textures_List;

   function Is_Bound (Slot : Natural) return Boolean;

   --  ------------------------------------------------------------------------

   procedure Bind_Texture (Slot : Natural; Tex : GL.Objects.Textures.Texture) is
      use GL.Objects.Textures.Targets;
      use GL.Types;
   begin
      if Slot > 12 then
         raise Texture_Exception with
           "Texture.Bind_Texture, active texture unit number for binding "
           & "is too high:" & Natural'Image (Slot);
      elsif not Is_Bound (Slot) then
         Set_Active_Unit (GL.Types.Int (Slot));
         Texture_2D.Bind (Tex);
         Bound_Textures.Replace_Element (Slot, Tex);
      end if;

   end Bind_Texture;

   --  ------------------------------------------------------------------------

   procedure Bind_Cube_Texture (Slot : Natural;
                                Tex  : GL.Objects.Textures.Texture) is
      use GL.Objects.Textures.Targets;
      use GL.Types;
      OK : Boolean := False;
   begin
      OK := Slot < 12;
      if not OK then
         raise Texture_Exception with
           "Texture.Bind_Cube_Texture, active texture unit number for binding "
           & "is high:" & Positive'Image (Slot);
      elsif not Is_Bound (Slot) then
         Set_Active_Unit (GL.Types.Int (Slot));
         Texture_Cube_Map.Bind (Tex);
         Bound_Textures.Replace_Element (Slot, Tex);
         OK := True;
      end if;

   end Bind_Cube_Texture;

   --  ------------------------------------------------------------------------

   procedure Create_Default_Texture is
      use GL.Objects.Textures.Targets;
      use GL.Pixels;
      use GL.Types;
      Dt_Pixel_C      : constant Integer := 16 * 16;
      Dt_Data_Size    : constant Integer :=  4 * Dt_Pixel_C;
      Dt_Data         : array (1 .. Dt_Data_Size) of UByte := (others => 0);
      Sq_Ac           : Integer;
      Sq_Dn           : Integer;
      Index           : Positive := 1;
      Default_Texture : Texture;
   begin
      ---  Generate RGBA pixels
      while Index <= Dt_Data_Size loop
         Sq_Ac := (Index - 1) / 16;
         if 2 * (Sq_Ac / 2) = Sq_Ac then
            Dt_Data (Index) := 0;
            Dt_Data (Index + 1) := 0;
            Dt_Data (Index + 2) := 0;
            Dt_Data (Index + 3) := 255;
         else
            Dt_Data (Index) := 255;
            Dt_Data (Index + 1) := 0;
            Dt_Data (Index + 2) := 255;
            Dt_Data (Index + 3) := 255;
         end if;
         Sq_Dn := Index / 16;
         if 2 * (Sq_Dn / 2) = Sq_Dn then
            Dt_Data (Index) := 255;
            Dt_Data (Index + 2) :=  255 - Dt_Data (Index + 2);
         end if;
         Index := Index + 4;
      end loop;

      Default_Texture.Initialize_Id;
      Set_Active_Unit (0);
      Texture_2D.Bind (Default_Texture);

      Texture_2D.Load_From_Data
        (0, RGBA, 16, 16, RGBA, Unsigned_Byte,
         GL.Objects.Textures.Image_Source (Dt_Data'Address));

      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Nearest);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Nearest);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_S
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_T

      Bound_Textures.Append (Default_Texture);
      Loaded_Texture_Count := Loaded_Texture_Count + 1;
--        Game_Utils.Game_Log ("Default texture loaded.");

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Texture_Manager.Create_Default_Texture!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
         Game_Utils.Game_Log  ("An exception occurred in Texture_Manager.Create_Default_Texture!");
         Game_Utils.Game_Log (Ada.Exceptions.Exception_Information (anError));
   end Create_Default_Texture;

   --  ------------------------------------------------------------------------

   procedure Init is
   begin
      Game_Utils.Game_Log ("Initializing texture manager.");
      Bound_Textures.Clear;
      Loaded_Textures.Clear;
      Create_Default_Texture;
      Game_Utils.Game_Log ("Texture manager initialized.");
   end Init;

   --  ------------------------------------------------------------------------

   function Is_Bound (Slot : Natural) return Boolean is
      use Bound_Textures_Package;
      Found : Boolean := False;
      Index : Natural;
      Curs  : Cursor;
   begin
      if not Bound_Textures.Is_Empty then
         Index := Bound_Textures.First_Index;
         while Index <= Bound_Textures.Last_Index and not Found loop
            Found := Slot = Index;
            Curs := Bound_Textures.To_Cursor (Index);
            Next (Curs);
         end loop;
      end if;
      return Found;
   end Is_Bound;

   --  ------------------------------------------------------------------------

   procedure Load_Image_To_Texture (File_Name          : String;
                                    aTexture           : in out Texture;
                                    Gen_Mips, Use_SRGB : Boolean) is
      use GL.Objects.Textures.Targets;
      use GL.Pixels;
      use GL.Types;
      use Loaded_Textures_Package;
      Curs                  : Cursor := Loaded_Textures.First;
      X_Width               : GL.Types.Int := 0;  --  Image width in pixels
      Y_Height              : GL.Types.Int := 0;  --  Image height in pixels
      X_16                  : Mod_16 := 0;
      Y_16                  : Mod_16 := 0;
      Force_Channels        : constant GL.Types.Int := 4;
      Image_Data_Ptr        : GID_Image_Loader.Raw_Data_Ptr;
      Data_Length           : GL.Types.Int := 0;   --  in bytes
--        Half_Height_In_Pixels : GL.Types.Int := 0;
--        Height_In_Pixels      : GL.Types.Int := 0;
      --  Assuming RGBA for 4 components per pixel.
      Num_Color_Components  : constant GL.Types.Int := 4;
      --  Assuming each color component is an unsigned char.
--        Width_In_Chars        : GL.Types.Int := 0;
      Texture_Loaded        : Boolean := False;
      Texture_Data          : Loaded_Texture;
   begin
      while Has_Element (Curs) and not Texture_Loaded loop
         Texture_Loaded := Element (Curs).File_Name = File_Name;
         Next (Curs);
      end loop;

      if not Texture_Loaded then
         GID_Image_Loader.Load_File_To_Image
           (File_Name, Image_Data_Ptr, Data_Length, X_Width, Y_Height, Force_Channels);
--           Game_Utils.Game_Log ("Texture_Manager.Load_Image_To_Texture, Load_Image_To_Texture result: X, Y Data_Length "
--                                & GL.Types.Int'Image (X_Width) & "  " &
--                                  GL.Types.Int'Image (Y_Height) & "  " &
--                                  GL.Types.Int'Image (Data_Length));

         X_16 := Mod_16 (X_Width);
         Y_16 := Mod_16 (Y_Height);
         if (X_16 and (X_16 - 1)) /= 0 or (Y_16 and (Y_16 - 1)) /= 0 then
            Game_Utils.Game_Log
              ("WARNING: Texture_Manager.Load_Image_To_Texture, texture is " &
                 "not power-of-two dimensions " & File_Name);
         end if;

         declare
            Data_Raw     : GID_Image_Loader.Raw_Data (1 .. Data_Length);
            -- Data is an array of UBytes
            Data         : array (1 .. Data_Length) of GID_Image_Loader.Component;
            Data_Top     : GL.Types.Int := 0;
            Data_Bottom  : GL.Types.Int := 0;
         begin
            Data_Raw := Image_Data_Ptr.all;
--              Game_Utils.Game_Log ("Load_Image_To_Texture data length " &
--                                     GL.Types.Int'Image (Data_Length));
            for index in 1 .. Data_Length loop
               Data (index) :=  Data_Raw (index);
            end loop;
            GID_Image_Loader.Free_Data (Image_Data_Ptr);

--              Half_Height_In_Pixels := Y_Height / 2;
--              Height_In_Pixels := Y_Height;
--              Width_In_Chars := X_Width * Num_Color_Components;
--              for index_h in 1 .. Half_Height_In_Pixels loop
--                 Data_Top := index_h * Width_In_Chars;
--                 Data_Bottom :=
--                   (Height_In_Pixels - index_h - 1) * Width_In_Chars;
--                 for index_h in 1 .. Width_In_Chars loop
--                    Swap := Data (Data_Top);
--                    Data (Data_Top) := Data (Data_Bottom);
--                    Data (Data_Bottom) := Swap;
--                    Data_Top := Data_Top + 1;
--                    Data_Bottom := Data_Bottom + 1;
--                 end loop;
--              end loop;

            --  Copy Data into an OpenGL texture
            aTexture.Initialize_Id;
            Texture_Data.File_Name := To_Unbounded_String (File_Name);
            Texture_Data.Texture_ID := aTexture.Raw_Id;
            Set_Active_Unit (0);
            Texture_2D.Bind (aTexture);

            if Bound_Textures.Is_Empty then
               Bound_Textures.Append (aTexture);
            else
               Bound_Textures.Replace_Element (1, aTexture);
            end if;

            if Use_SRGB then
               Texture_2D.Load_From_Data
                 (0, SRGB_Alpha, X_Width, Y_Height, RGBA, Unsigned_Byte,
                  GL.Objects.Textures.Image_Source (Data'Address));
            else
               Texture_2D.Load_From_Data
                 (0, RGBA, X_Width, Y_Height, RGBA, Unsigned_Byte,
                  GL.Objects.Textures.Image_Source (Data'Address));
            end if;
            Texture_Loaded := True;

            if Gen_Mips then
               Texture_2D.Generate_Mipmap;
               if Settings.Texture_Filter = 1 then
                  Texture_2D.Set_Minifying_Filter
                    (GL.Objects.Textures.Nearest_Mipmap_Linear);
               elsif Settings.Texture_Filter = 2 then
                  Texture_2D.Set_Minifying_Filter
                    (GL.Objects.Textures.Linear_Mipmap_Linear);
               else
                  Texture_2D.Set_Minifying_Filter
                    (GL.Objects.Textures.Nearest_Mipmap_Nearest);
               end if;
               Texture_Data.Has_Mipmaps := True;

            else  -- not Generate_Mipmaps
               if Settings.Texture_Filter > 0 then
                  Texture_2D.Set_Minifying_Filter
                    (GL.Objects.Textures.Linear);
               else
                  Texture_2D.Set_Minifying_Filter
                    (GL.Objects.Textures.Nearest);
               end if;
            end if;

            if Settings.Texture_Filter > 0 then
               Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
            else
               Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Nearest);
            end if;
            Texture_2D.Set_X_Wrapping (Clamp_To_Edge);
            Texture_2D.Set_Y_Wrapping (Clamp_To_Edge);

            Texture_Data.Texture_ID := aTexture.Raw_Id;
            Loaded_Textures.Append (Texture_Data);
            Loaded_Texture_Count := Loaded_Texture_Count + 1;
         end; -- declare block
      else
         Game_Utils.Game_Log ("Texture_Manager.Load_Image_To_Texture image " &
                              File_Name & " already loaded.");
      end if;

   exception
      when anError : others =>
         Put_Line ("An exception occurred in " &
                     "Texture_Manager.Load_Image_To_Texture when loading " &
                     File_Name);
         Put_Line (Ada.Exceptions.Exception_Information (anError));
   end Load_Image_To_Texture;

   --  ------------------------------------------------------------------------

end Texture_Manager;
