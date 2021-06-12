
--  with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Textures.Targets;

with Load_BMP_File;

package body Textures_Manager is

    --  -------------------------------------------------------------------------

    procedure Load_Texture (File_Name : String;
                            aTexture : in out GL.Objects.Textures.Texture) is
      use GL.Objects.Textures;
      use GL.Objects.Textures.Targets;
    begin
        Set_Active_Unit (0);
        Load_BMP_File.Load_BMP_To_Texture (File_Name, False, aTexture);
        Texture_2D.Set_Minifying_Filter (Linear);
        Texture_2D.Set_Magnifying_Filter (Linear);

    end Load_Texture;

    --  ------------------------------------------------------------------------

end Textures_Manager;
