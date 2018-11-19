
with System;

with Ada.Exceptions;
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
      Put_Line ("Shadow_Map_FBO.Bind_For_Reading Active_Unit set.");
        Texture_2D.Bind (aShadow_Map.Map);
      Put_Line ("Shadow_Map_FBO.Bind_For_Reading Shadow_Map bound.");
   end Bind_For_Reading;

   --  ------------------------------------------------------------------------------

   procedure Bind_For_Writing (aShadow_Map : in out Shadow_Map) is
   begin
      Put_Line ("Shadow_Map_FBO.Bind_For_Writing binding Shadow_Map.");
        GL.Objects.Framebuffers.Draw_Target.Bind (aShadow_Map.FBO);
      Put_Line ("Shadow_Map_FBO.Bind_For_Writing Shadow_Map bound.");
   end Bind_For_Writing;

   --  ------------------------------------------------------------------------------

   procedure Init (aShadow_Map : in out Shadow_Map;
                  Window_Width, Window_Height : GL.Types.Int) is
      use GL.Objects.Framebuffers;
      use GL.Objects.Textures.Targets;
   begin
      aShadow_Map.FBO.Initialize_Id;
      Bind (Read_And_Draw_Target, aShadow_Map.FBO);

      aShadow_Map.Map.Initialize_Id;
      Texture_2D.Bind (aShadow_Map.Map);
      Texture_2D.Load_From_Data (0, GL.Pixels.Depth_Component,
                                 Window_Width, Window_Height,
                                 GL.Pixels.Depth_Component,
                                 GL.Pixels.Float,
                                 GL.Objects.Textures.Image_Source (System.Null_Address));
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge);
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge);

      Bind (Read_And_Draw_Target, aShadow_Map.FBO);
      Read_And_Draw_Target.Attach_Texture (Depth_Attachment, aShadow_Map.Map, 0);

      GL.Buffers.Set_Active_Buffer (GL.Buffers.None);

      if not (Status (Read_And_Draw_Target) /= Complete) then
         Put_Line ("Shadow_Map_FBO.Init FBO error");
--           raise Shadow_Map_Exception with "Shadow_Map_FBO FBO error: " &
--           Ada.Exceptions.Exception_Information(Shadow_Map_Exception);
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Init.");
         raise;
   end Init;

end Shadow_Map_FBO;
