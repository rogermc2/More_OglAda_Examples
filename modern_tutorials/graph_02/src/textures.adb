
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Images;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Types;

with Maths;
with Utilities;

package body Textures is

   --  -------------------------------------------------------------------------

   procedure Load_Texture (aTexture : in out GL.Objects.Textures.Texture) is
      use GL.Objects.Textures;
      use GL.Objects.Textures.Targets;
      use GL.Types;
      use Maths.Single_Math_Functions;
      Graph : Utilities.UByte_Array (1 .. 2048);
      X     : Single;
      Y     : Single;
   begin
      for index in Graph'Range loop
         X := (Single (index - 1) - 1024.0) / 100.0;
         Y := Sin (10.0 * X) / (1.0 + X ** 2);
         Graph (index) := UByte (Single'Rounding (128.0 * (Y + 1.0)));
      end loop;

      GL.Objects.Textures.Set_Active_Unit (0);
      aTexture.Initialize_Id;
      Texture_2D.Bind (aTexture);
      Put_Line ("Textures.Load_Texture aTexture bound");
      Texture_2D.Load_From_Data (Level           => 0,
                                 Internal_Format => GL.Pixels.Red,
                                 Width           => 2048,
                                 Height          => 1,
                                 Source_Format   => GL.Pixels.Red,
                                 Source_Type     => GL.Pixels.Unsigned_Byte,
                                 Source          => Image_Source (Graph'Address));

   exception
      when others =>
         Put_Line ("An exception occurred in Textures.Load_Texture");
         raise;
   end Load_Texture;

   --  -------------------------------------------------------------------------

end Textures;
