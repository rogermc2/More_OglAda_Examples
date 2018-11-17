
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use  Ada.Text_IO;

with GL.Errors;
with GL.Objects.Textures.Targets;

package body My_Texture is

    procedure Setup_2D_Texture (Texture : in out GL.Objects.Textures.Texture) is
    begin
        Texture.Initialize_Id;
        GL.Objects.Textures.Targets.Texture_2D.Bind (Texture);

    exception
        when others =>
            Put_Line ("An exceptiom occurred in Setup_2D_Texture.");
            raise;
    end Setup_2D_Texture;

end My_Texture;
