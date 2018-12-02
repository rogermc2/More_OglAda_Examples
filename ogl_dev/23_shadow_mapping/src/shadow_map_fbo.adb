
with System;

with Ada.Exceptions;
with Ada.Numerics.Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Buffers;
with GL.Framebuffer;
with GL.Objects.Textures.Targets;
with GL.Pixels;

package body Shadow_Map_FBO is

   procedure Bind_For_Reading (aShadow_Map : in out Shadow_Map;
                               Tex_Unit : GL.Objects.Textures.Texture_Unit) is
       use GL.Objects.Textures.Targets;
   begin
        GL.Objects.Textures.Set_Active_Unit (Tex_Unit);
        Texture_2D.Bind (aShadow_Map.Texture);
   end Bind_For_Reading;

   --  ------------------------------------------------------------------------------

   procedure Bind_For_Writing (aShadow_Map : in out Shadow_Map) is
   begin
--          GL.Objects.Framebuffers.Draw_Target.Bind (aShadow_Map.FBO);
        GL.Objects.Framebuffers.Read_And_Draw_Target.Bind (aShadow_Map.FBO);
   end Bind_For_Writing;

   --  ------------------------------------------------------------------------------

   procedure Init (aShadow_Map : in out Shadow_Map;
                   Window_Width, Window_Height : GL.Types.Int) is
--                     Draw_Buffer_List : in out GL.Buffers.Explicit_Color_Buffer_List) is
      use GL.Objects.Framebuffers;
      use GL.Objects.Textures.Targets;
      use GL.Types;
--        theTexture  : GL.Objects.Textures.Texture := aShadow_Map.Texture;
--        Num_MipMaps : constant Int := GL.Types.Int (Float'Ceiling
--          (Ada.Numerics.Elementary_Functions.Log(512.0, 2.0)))+1;
   begin
      aShadow_Map.FBO.Initialize_Id;

--        Put_Line ("Main_Loop.Init, Num_MipMaps" & Int'Image (Num_MipMaps));
      --  Inialize the texture buffer
      aShadow_Map.Texture.Initialize_Id;
      Texture_2D.Bind (aShadow_Map.Texture);
      Texture_2D.Storage (1, GL.Pixels.RGBA8, 512, 512);

--        Texture_2D.Storage (1, GL.Pixels.Depth_Component32,
--                            Window_Width, Window_Height);
--        Texture_2D.Load_From_Data (0, GL.Pixels.Depth_Component,
--                                   Window_Width, Window_Height,
--                                   GL.Pixels.Depth_Component,
--                                   GL.Pixels.Float,
--                                   GL.Objects.Textures.Image_Source (System.Null_Address));
     Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge);
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge);

      Read_And_Draw_Target.Bind (aShadow_Map.FBO);
      Read_And_Draw_Target.Attach_Texture (Color_Attachment_0, aShadow_Map.Texture, 0);

--        Read_And_Draw_Target.Attach_Texture (Depth_Attachment, aShadow_Map.Texture, 0);
        GL.Buffers.Set_Active_Buffer (GL.Buffers.Color_Attachment0);
      --  Disable writes to the color buffer
--        GL.Buffers.Set_Active_Buffer (GL.Buffers.None);
      --  None is not available for Set_Read_Buffer
      --  GL.Framebuffer.Set_Read_Buffer (GL.Buffers.None);

--        Draw_Buffer_List (1) := GL.Buffers.Color_Attachment0;
--        GL.Buffers.Set_Active_Buffers (Draw_Buffer_List);

      if Status (Read_And_Draw_Target) /= GL.Objects.Framebuffers.Complete then
         Put_Line ("Shadow_Map_FBO.Init FBO error");
         Put_line (Framebuffer_Status'Image (Status (Read_And_Draw_Target)));
      end if;
      --  Reset Read_And_Draw_Target to screen framebuffer
      Read_And_Draw_Target.Bind (Default_Framebuffer);

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Init.");
         raise;
   end Init;

end Shadow_Map_FBO;
