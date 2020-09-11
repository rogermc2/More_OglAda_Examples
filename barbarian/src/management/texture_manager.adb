
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Textures.Targets;
with GL.Pixels;

with GID_Image_Loader;

with Game_Utils;
with Settings;

package body Texture_Manager is

    package Bound_Textures_Package is new Ada.Containers.Doubly_Linked_Lists
      (GL.Objects.Textures.Texture);
    type Bound_Textures_List is new Bound_Textures_Package.List with null record;

    type Loaded_Texture is record
        File_Name   : Unbounded_String;
        Texture_ID  : GL.Types.UInt;
        Has_Mipmaps : Boolean := False;
    end record;
    package Loaded_Textures_Package is new Ada.Containers.Doubly_Linked_Lists
      (Loaded_Texture);
    type Loaded_Textures_List is new Loaded_Textures_Package.List with null record;

    Bound_Textures  : Bound_Textures_List;
    Loaded_Textures : Loaded_Textures_List;

    --  ------------------------------------------------------------------------

    procedure Bind_Texture (Slot : GL.Types.Int;
                            Tex : GL.Objects.Textures.Texture) is
        use GL.Objects.Textures.Targets;
        use GL.Types;
    begin
        if Slot > 12 then
            raise Texture_Exception with
              "Texture.Bind_Texture, active texture unit number for binding "
               & "is high:" & Int'Image (Slot);
        end if;
        if Tex /= Bound_Textures (Slot) then
            Set_Active_Unit (Slot - 1);
            Texture_2D.Bind (Tex);
            Bound_Textures (Slot) := Tex;
        end if;

    end Bind_Texture;

    --  ------------------------------------------------------------------------

    procedure Create_Default_Texture is
        use GL.Objects.Textures.Targets;
        use GL.Pixels;
        Dt_Pixel_C   : constant Integer := 16 * 16;
        Dt_Data_Size : constant Integer :=  4 * Dt_Pixel_C;
        Dt_Data      : String (1 .. Dt_Data_Size);
        Char_String  : String (1 .. 1);
        Sq_Ac        : Integer;
        Sq_Dn        : Integer;
        Index        : Integer := 1;
        theTexture   : GL.Objects.Textures.Texture;
    begin
        Game_Utils.Game_Log ("Creating default texture.");
        ---  Generate RGBA pixels
        while Index <= Dt_Data_Size loop
            Sq_Ac := Index / 16;
            if 2 * (Sq_Ac / 2) = Sq_Ac then
                Dt_Data (Index) := Character'Val (0);
                Dt_Data (Index + 1) := Character'Val (0);
                Dt_Data (Index + 2) := Character'Val (0);
                Dt_Data (Index + 3) := Character'Val (255);
            else
                Dt_Data (Index) := Character'Val (255);
                Dt_Data (Index + 1) := Character'Val (0);
                Dt_Data (Index + 2) := Character'Val (255);
                Dt_Data (Index + 3) := Character'Val (255);
            end if;

            Sq_Dn := Index / 16;
            if 2 * (Sq_Dn / 2) = Sq_Dn then
                Char_String (1) := Dt_Data (Index);
                Dt_Data (Index) :=
                  Character'Val (255 - Integer'Value (Char_String));
                Char_String (1) := Dt_Data (Index + 2);
                Dt_Data (Index + 2) :=
                  Character'Val (255 - Integer'Value (Char_String));
            end if;
            Index := Index + 4;
        end loop;

        theTexture.Initialize_Id;
        GL.Objects.Textures.Set_Active_Unit (0);
        Texture_2D.Bind (theTexture);
        Bound_Textures.Append (theTexture);
        Texture_2D.Load_From_Data
          (0, RGBA, 16, 16, RGBA, Unsigned_Byte,
           GL.Objects.Textures.Image_Source (Dt_Data'Address));

        Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Nearest);
        Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Nearest);
        Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_S
        Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_T

        Game_Utils.Game_Log ("Default texture loaded.");

    exception
        when others =>
            Put_Line ("An exception occurred in Texture_Manager.Create_Default_Texture!");
    end Create_Default_Texture;

    --  ------------------------------------------------------------------------

    procedure Init_Texture_Manager is
    begin
        Game_Utils.Game_Log ("Initializing texture manager.");
        Create_Default_Texture;
    end Init_Texture_Manager;

    --  ------------------------------------------------------------------------

    procedure Load_Image_To_Texture (File_Name : String;
                                     aTexture : out Texture;
                                     Gen_Mips, Use_SRGB : Boolean) is
        use Ada.Streams;
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
        Input_Stream          : Stream_IO.File_Type;
        Data_Length           : GL.Types.Int;
        Half_Height_In_Pixels : GL.Types.Int;
        Height_In_Pixels      : GL.Types.Int;
        --  Assuming RGBA for 4 components per pixel.
        Num_Color_Components  : constant GL.Types.Int := 4;
        --  Assuming each color component is an unsigned char.
        Width_In_Chars        : GL.Types.Int;
        Dt_Data               : Unbounded_String;
        Texture_Loaded        : Boolean := False;
    begin
        Game_Utils.Game_Log ("Loading image to texture.");

        while Has_Element (Curs) and not Texture_Loaded loop
            Texture_Loaded := Element (Curs).File_Name = File_Name;
            Next (Curs);
        end loop;

        if not Texture_Loaded then
            Game_Utils.Game_Log ("Loading image" & File_Name);
            Stream_IO.Open (Input_Stream, Stream_IO.In_File, File_Name);
            --              Input_Stream_Access := Stream_IO.Stream (Input_Stream);
            --              GID.Load_image_header (Descriptor, Input_Stream_Access.all);
            --              Width := GL.Types.Size (GID.Pixel_width (Descriptor));
            --              Height := GL.Types.Size (GID.Pixel_height (Descriptor));
            GID_Image_Loader.Load_File_To_Image
              (File_Name, Data_Ptr, Data_Length, X, Y, Force_Channels);
            declare
                Data_Raw     : constant GID_Image_Loader.Raw_Data := Data_Ptr.all;
                Data         : array (1 .. Data_Length) of GID_Image_Loader.Component;
                Swap         : GID_Image_Loader.Component;
                Data_Top     : GL.Types.Int;
                Data_Bottom  : GL.Types.Int;
                Texture_Data : Loaded_Texture;
            begin
                Texture_Data.Texture_ID := aTexture.Raw_Id;
                for index in Data'Range loop
                    Data (index) := Data_Raw (index);
                end loop;
                GID_Image_Loader.Free_Data (Data_Ptr);
                Game_Utils.Game_Log
                  ("Texture_Manager.Load_Image_To_Textur, image loaded from "
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
        end if;

    exception
        when others =>
            Put_Line ("An exception occurred in Texture_Manager.Load_Image_To_Texture!");
    end Load_Image_To_Texture;

    --  ------------------------------------------------------------------------

end Texture_Manager;
