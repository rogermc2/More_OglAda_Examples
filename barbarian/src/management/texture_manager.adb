
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

    package Bound_Textures_Package is new Ada.Containers.Vectors
      (Natural, GL.Objects.Textures.Texture);
    type Bound_Textures_List is new Bound_Textures_Package.Vector with null record;

    type Loaded_Texture is record
        File_Name   : Unbounded_String;
        Texture_ID  : GL.Types.UInt;
        Has_Mipmaps : Boolean := False;
    end record;
    package Loaded_Textures_Package is new Ada.Containers.Vectors
      (Natural, Loaded_Texture);
    type Loaded_Textures_List is new Loaded_Textures_Package.Vector with null record;

    Bound_Textures  : Bound_Textures_List;
    Loaded_Textures : Loaded_Textures_List;

    function Is_Bound (Slot : Natural) return Boolean;

    --  ------------------------------------------------------------------------

    procedure Bind_Texture (Slot : Natural;
                            Tex : GL.Objects.Textures.Texture) is
        use GL.Objects.Textures.Targets;
        use GL.Types;
    begin
        if Slot > 11 then
            raise Texture_Exception with
              "Texture.Bind_Texture, active texture unit number for binding "
              & "is high:" & Natural'Image (Slot);
        end if;
        if not Is_Bound (Slot) then
            Set_Active_Unit (GL.Types.Int (Slot - 1));
            Texture_2D.Bind (Tex);
            Bound_Textures.Replace_Element (Slot, Tex);
        end if;

    end Bind_Texture;

    --  ------------------------------------------------------------------------

    procedure Bind_Cube_Texture (Slot : Natural;
                                 Tex : GL.Objects.Textures.Texture) is
        use GL.Objects.Textures.Targets;
        use GL.Types;
    begin
        if Slot > 11 then
            raise Texture_Exception with
              "Texture.Bind_Cube_Texture, active texture unit number for binding "
              & "is high:" & Positive'Image (Slot);
        end if;
        if not Is_Bound (Slot) then
            Set_Active_Unit (GL.Types.Int (Slot));
            Texture_Cube_Map.Bind (Tex);
            Bound_Textures.Replace_Element (Slot, Tex);
        end if;

    end Bind_Cube_Texture;

    --  ------------------------------------------------------------------------

    function Create_Default_Texture return Boolean is
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
        Game_Utils.Game_Log ("Creating default texture Dt_Data_Size " &
                               Integer'Image (Dt_Data_Size));
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
        Bound_Textures.Append (Default_Texture);
        Texture_2D.Load_From_Data
          (0, RGBA, 16, 16, RGBA, Unsigned_Byte,
           GL.Objects.Textures.Image_Source (Dt_Data'Address));

        Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Nearest);
        Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Nearest);
        Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_S
        Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_T

        Game_Utils.Game_Log ("Default texture loaded.");
        return True;
    exception
        when anError : others =>
            Put_Line ("An exception occurred in Texture_Manager.Create_Default_Texture!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
            Game_Utils.Game_Log  ("An exception occurred in Texture_Manager.Create_Default_Texture!");
            Game_Utils.Game_Log (Ada.Exceptions.Exception_Information (anError));
            return False;
    end Create_Default_Texture;

    --  ------------------------------------------------------------------------

    function Init_Texture_Manager return Boolean is
    begin
        Game_Utils.Game_Log ("Initializing texture manager.");
        return Create_Default_Texture;
    end Init_Texture_Manager;

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

    --      pragma Warnings (off);
    function Load_Image_To_Texture (File_Name : String;
                                     aTexture : in out Texture;
                                     Gen_Mips, Use_SRGB : Boolean) return Boolean is
        use GL.Objects.Textures.Targets;
        use GL.Pixels;
        use GL.Types;
        use Loaded_Textures_Package;
        Curs                  : Cursor := Loaded_Textures.First;
        X                     : GL.Types.Int := 0;  --  Image width
        Y                     : GL.Types.Int := 0;  --  Image height
        --          N                : Integer;  --  Bytes per pixel
        Force_Channels        : constant GL.Types.Int := 4;
        Data_Ptr              : GID_Image_Loader.Raw_Data_Ptr;
        Data_Length           : GL.Types.Int := 0;
        Half_Height_In_Pixels : GL.Types.Int := 0;
        Height_In_Pixels      : GL.Types.Int := 0;
        --  Assuming RGBA for 4 components per pixel.
        Num_Color_Components  : constant GL.Types.Int := 4;
        --  Assuming each color component is an unsigned char.
        Width_In_Chars        : GL.Types.Int := 0;
        Dt_Data               : Unbounded_String := To_Unbounded_String ("");
        Texture_Loaded        : Boolean := False;
    begin
        while Has_Element (Curs) and not Texture_Loaded loop
            Texture_Loaded := Element (Curs).File_Name = File_Name;
            Next (Curs);
        end loop;

        if not Texture_Loaded then
            if not Initialized (aTexture) then
                aTexture.Initialize_Id;
            end if;
            Bind_Texture (0, aTexture);
            Game_Utils.Game_Log ("Load_Image_To_Texture loading image " & File_Name);
            GID_Image_Loader.Load_File_To_Image
              (File_Name, Data_Ptr, Data_Length, X, Y, Force_Channels);
            Game_Utils.Game_Log ("Load_Image_To_Texture, X, Y Data_Length " &
                                   GL.Types.Int'Image (X) & "  " & GL.Types.Int'Image (Y) & "  "
                                 & GL.Types.Int'Image (Data_Length));

            declare
                Data_Raw     : constant GID_Image_Loader.Raw_Data := Data_Ptr.all;
                -- Data is an array of UBytes
                Data         : array (1 .. Data_Length) of UByte;
                Swap         : UByte;
                --                  Swap         : GID_Image_Loader.Component;  --  GL.Types.UByte
                Data_Top     : GL.Types.Int := 0;
                Data_Bottom  : GL.Types.Int := 0;
                Texture_Data : Loaded_Texture;
            begin
                Game_Utils.Game_Log
                  ("Texture_Manager.Load_Image_To_Texture declare block entered");
                Texture_Data.Texture_ID := aTexture.Raw_Id;
                Game_Utils.Game_Log ("Loading data; Texture_Data.Texture_ID " &
                                       GL.Types.UInt'Image (Texture_Data.Texture_ID));
                for index in 1 .. Data_Length loop
                    Game_Utils.Game_Log
                      ("Loading data; index " & GL.Types.Int'Image (index));
                    Data (index) := UByte (Data_Raw (index));
                end loop;
                Game_Utils.Game_Log
                  ("Texture_Manager.Load_Image_To_Texture calling GID_Image_Loader.to Data_Ptr");

                GID_Image_Loader.Free_Data (Data_Ptr);
                Game_Utils.Game_Log
                  ("Texture_Manager.Load_Image_To_Texture, image loaded from "
                   & File_Name);

                Half_Height_In_Pixels := Y / 2;
                Height_In_Pixels := Y;
                Width_In_Chars := X * Num_Color_Components;
                for index_h in 1 .. Half_Height_In_Pixels loop
                    Data_Top := index_h * Width_In_Chars;
                    Data_Bottom :=
                      (Height_In_Pixels - index_h - 1) * Width_In_Chars;
                    for index_h in 1 .. Width_In_Chars loop
                        Swap := Data (Data_Top);
                        Data (Data_Top) := Data (Data_Bottom);
                        Data (Data_Bottom) := Swap;
                        Data_Top := Data_Top + 1;
                        Data_Bottom := Data_Bottom + 1;
                    end loop;
                end loop;

                Set_Active_Unit (0);
                aTexture.Initialize_Id;
                Texture_2D.Bind (aTexture);
                Bound_Textures.Append (aTexture);
                if Use_SRGB then
                    Texture_2D.Load_From_Data
                      (0, SRGB_Alpha, 16, 16, RGBA, Unsigned_Byte,
                       GL.Objects.Textures.Image_Source (Dt_Data'Address));
                else
                    Texture_2D.Load_From_Data
                      (0, RGBA, 16, 16, RGBA, Unsigned_Byte,
                       GL.Objects.Textures.Image_Source (Dt_Data'Address));
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
            end; -- declare block
        else
            Game_Utils.Game_Log ("Image " & File_Name & " already loaded.");
        end if;
        return True;

    exception
        when anError : others =>
            Put_Line ("An exception occurred in " &
                        "Texture_Manager.Load_Image_To_Texture when loading " &
                        File_Name);
            Put_Line (Ada.Exceptions.Exception_Information (anError));
            return False;
    end Load_Image_To_Texture;

    --  ------------------------------------------------------------------------

end Texture_Manager;
