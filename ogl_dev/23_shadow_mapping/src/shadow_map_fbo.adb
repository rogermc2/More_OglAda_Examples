
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
        GL.Objects.Framebuffers.Read_And_Draw_Target.Bind (aShadow_Map.FBO);
    end Bind_For_Writing;

    --  ------------------------------------------------------------------------------

    procedure Init (aShadow_Map : in out Shadow_Map;
                    Window_Width, Window_Height : GL.Types.Int) is
        use GL.Objects.Framebuffers;
        use GL.Objects.Textures.Targets;
        use GL.Types;
    begin
        aShadow_Map.FBO.Initialize_Id;
        Read_And_Draw_Target.Bind (aShadow_Map.FBO);
        --  Inialize the texture buffer
        aShadow_Map.Texture.Initialize_Id;
        Texture_2D.Bind (aShadow_Map.Texture);
        Texture_2D.Load_Empty_Texture (Level           => 0,
                                       Internal_Format => GL.Pixels.Depth_Component,
                                       Width           => Window_Width,
                                       Height          => Window_Height);
        Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
        Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
        Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge);
        Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge);

        Read_And_Draw_Target.Bind (aShadow_Map.FBO);
        Read_And_Draw_Target.Attach_Texture (Depth_Attachment, aShadow_Map.Texture, 0);
        if Read_And_Draw_Target.Status /= Complete then
            raise Shadow_Map_Exception with "Shadow_Map_FBO.Init, Attachment incomplete: " &
                        Framebuffer_Status'Image (Read_And_Draw_Target.Status);
        end if;

        --  Disable writes to the color buffer
        GL.Buffers.Set_Active_Buffer (GL.Buffers.None);

        if Status (Read_And_Draw_Target) /= GL.Objects.Framebuffers.Complete then
            raise Shadow_Map_Exception with "Shadow_Map_FBO.Init FBO error" &
              Framebuffer_Status'Image (Status (Read_And_Draw_Target));
        end if;

    exception
        when others =>
            Put_Line ("An exception occurred in Main_Loop.Init.");
            raise;
    end Init;

end Shadow_Map_FBO;
