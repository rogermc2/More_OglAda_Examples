
with GL.Objects.Textures;
with GL.Types;

package Picking_Texture is

   type Pixel_Info is private;

   procedure  Bind (aTexture : in out GL.Objects.Textures.Texture;
                    Texture_Unit : GL.Objects.Textures.Texture_Unit);
   procedure Disable_Writing;
   procedure Enable_Writing;
   procedure Init_Picking_Texture (Window_Width, Window_Height : GL.Types.UInt);

private
   type Pixel_Info is record
      Object_ID : GL.Types.Single := 0.0;
      Draw_ID   : GL.Types.Single := 0.0;
      Prim_ID   : GL.Types.Single := 0.0;
   end record;

end Picking_Texture;
