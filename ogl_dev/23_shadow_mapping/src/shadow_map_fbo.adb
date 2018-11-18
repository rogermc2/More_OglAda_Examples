
with System;

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
        Texture_2D.Bind (aShadow_Map.Map);
   end Bind_For_Reading;

   --  ------------------------------------------------------------------------------

   procedure Bind_For_Writing (aShadow_Map : in out Shadow_Map) is
   begin
        GL.Objects.Framebuffers.Draw_Target.Bind (aShadow_Map.FBO);
   end Bind_For_Writing;

   --  ------------------------------------------------------------------------------

   function Init (aShadow_Map : in out Shadow_Map;
                  Window_Width, Window_Height : GL.Types.Int) return Boolean is
      use GL.Objects.Framebuffers;
      use GL.Objects.Textures.Targets;
      Result : Boolean := False;
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
      Attach_Texture (Target     => Read_And_Draw_Target,
                      Attachment => Depth_Attachment,
                      Object     =>  aShadow_Map.Map,
                      Level      => 0);
      GL.Buffers.Set_Active_Buffer (GL.Buffers.None);
      Result := Status (Read_And_Draw_Target) /= Complete;
      if not Result then
         raise Shadow_Map_Exception with "Shadow_Map_FBO FBO error";
      end if;
      return Result;
   end Init;

end Shadow_Map_FBO;
