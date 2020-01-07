
with Ada.Text_IO; use Ada.Text_IO;

with GL.Buffers;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Types; use GL.Types;

with SOIL;
with SOIL.Images;

package body Textures_Manager is

   procedure Init (Frame_Buffer : in out GL.Objects.Framebuffers.Framebuffer;
                   Depth_Texture : in out GL.Objects.Textures.Texture) is
      use GL.Objects.Framebuffers;
      use GL.Objects.Textures.Targets;
      Status   : Framebuffer_Status;
   begin
      Frame_Buffer.Initialize_Id;
      Read_And_Draw_Target.Bind (Frame_Buffer);

      Depth_Texture.Initialize_Id;
      Texture_2D.Bind (Depth_Texture);
      --  All upcoming GL_TEXTURE_2D operations now have effect on
      --  this texture object
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Repeat); --  Wrap_S
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Repeat); --  Wrap_T
      Texture_2D.Set_Compare_Function (GL.Types.LEqual);
      Texture_2D.Toggle_Compare_X_To_Texture (True);

      Texture_2D.Load_Empty_Texture  (0, GL.Pixels.Depth_Component16, 1024, 1024);
--        Texture_2D.Generate_Mipmap;

      Read_And_Draw_Target.Attach_Texture (Depth_Attachment, Depth_Texture, 0);

     --  No color output in the bound framebuffer, only depth.
      GL.Buffers.Set_Active_Buffer (GL.Buffers.None);
      Status := GL.Objects.Framebuffers.Status (Read_And_Draw_Target);
      if Status /= Complete then
         Put_Line ("Set_Up; Bind (Frame_Buffer), Status: " &
                   Framebuffer_Status'Image (Status));
         raise Framebuffer_Error;
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Textures_Manager.Init");
         raise;
   end Init;

   --  -----------------------------------------------------------------------------

    procedure Load_Texture (Frame_Buffer : in out GL.Objects.Framebuffers.Framebuffer;
                            Image_File_Name : String) is
      use GL.Objects.Framebuffers;
      use GL.Objects.Textures.Targets;
      Image    : SOIL.Images.Image;
      Width    : Int;
      Height   : Int;
      Status   : Framebuffer_Status;
   begin
      Frame_Buffer.Initialize_Id;
      Read_And_Draw_Target.Bind (Frame_Buffer);

      Image.Load (Image_File_Name);
      Width := SOIL.Images.Width (Image);
      Height := SOIL.Images.Height (Image);
      if not SOIL.Images.Loaded (Image) then
         raise Image_Error with
           "Load_Image; " & Image_File_Name & " failed to load.";
      end if;

      Texture_2D.Load_Empty_Texture  (0, GL.Pixels.Depth_Component16, Width, Height);
--        Texture_2D.Generate_Mipmap;

      Status := GL.Objects.Framebuffers.Status (Read_And_Draw_Target);
      if Status /= Complete then
         Put_Line ("Set_Up; Bind (Frame_Buffer), Status: " &
                   Framebuffer_Status'Image (Status));
         raise Framebuffer_Error;
      end if;

      --  Reset Read_And_Draw_Target to screen framebffer
      Read_And_Draw_Target.Bind (Default_Framebuffer);

   exception
      when others =>
         Put_Line ("An exception occurred in Textures_Manager.Load_Texture");
         raise;
   end Load_Texture;

   --  -----------------------------------------------------------------------------

end Textures_Manager;
