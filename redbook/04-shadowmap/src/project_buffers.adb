
with System;

with Ada.Text_IO; use  Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Types; use  GL.Types;

with Utilities;
with Vertex_Data;

package body Project_Buffers is

    Depth_Texture_Size : constant GL.Types.Int := 2048;
    --     Image_Error        : exception;

    --  ------------------------------------------------------------------------

    procedure Init_Depth_Frame_Buffer (Depth_Frame_Buffer : in out GL.Objects.Framebuffers.Framebuffer;
                                       Depth_Texture : GL.Objects.Textures.Texture) is
        use GL.Objects.Framebuffers;
    begin
        Depth_Frame_Buffer.Initialize_Id;
        Read_And_Draw_Target.Bind (Depth_Frame_Buffer);
        Read_And_Draw_Target.Attach_Texture (Attachment => Depth_Attachment,
                                             Object     => Depth_Texture,
                                             Level      => 0);
        if Read_And_Draw_Target.Status /= Complete then
            Put_Line ("Buffers.Init_Depth_Frame_Buffer, Attachment incomplete: " &
                        Framebuffer_Status'Image (Read_And_Draw_Target.Status));
        end if;
        GL.Buffers.Set_Active_Buffer (GL.Buffers.None);

        Read_And_Draw_Target.Bind (Default_Framebuffer);

    exception
        when others =>
            Put_Line ("An exception occurred in Project_Buffers.Init_Depth_Frame_Buffer.");
            raise;
    end Init_Depth_Frame_Buffer;

    --  ------------------------------------------------------------------------

    procedure Init_Ground_Buffer (Ground_Buffer  : in out GL.Objects.Buffers.Buffer) is
        use GL.Objects.Buffers;
        Vertex_Data_Bytes  : constant Long := Vertex_Data.Ground_Vertices'Size / 8;
        Normals_Data_Bytes : constant Long := Vertex_Data.Ground_Normals'Size / 8;
        Buffer_Size        : constant Long := Vertex_Data_Bytes + Normals_Data_Bytes;
    begin
        Ground_Buffer.Initialize_Id;
        Array_Buffer.Bind (Ground_Buffer);

        Array_Buffer.Allocate (Buffer_Size, Static_Draw);
        --        Utilities.Load_Vertex_Buffer (Array_Buffer, Vertex_Data.Ground_Vertices, Static_Draw);
        Utilities.Load_Vertex_Sub_Buffer (Array_Buffer, 0, Vertex_Data.Ground_Vertices);
        Utilities.Load_Vertex_Sub_Buffer
          (Array_Buffer, Int (Vertex_Data_Bytes), Vertex_Data.Ground_Normals);

        GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => 4,
                                                 Kind   => GL.Types.Single_Type,
                                                 Stride => 7, Offset => 0);
        GL.Attributes.Enable_Vertex_Attrib_Array (0);

        GL.Attributes.Set_Vertex_Attrib_Pointer
          (1, 3, GL.Types.Single_Type, 7, 4);
        GL.Attributes.Enable_Vertex_Attrib_Array (1);

    exception
        when others =>
            Put_Line ("An exception occurred in Project_Buffers.Init_Ground_Buffer.");
            raise;
    end Init_Ground_Buffer;

    --  ------------------------------------------------------------------------

    procedure Init_Depth_Texture (Depth_Texture : in out GL.Objects.Textures.Texture) is
        use GL.Objects.Textures;
        use GL.Objects.Textures.Targets;
    begin
        Depth_Texture.Initialize_Id;
        Texture_2D.Bind (Depth_Texture);
--          Texture_2D.Load_Empty_Texture (Level           => 0,
--                                         Internal_Format => GL.Pixels.Depth_Component32,
--                                         Width           => Depth_Texture_Size,
--                                         Height          => Depth_Texture_Size);
        Texture_2D.Load_From_Data (Level  => 0,
                                   Internal_Format => GL.Pixels.Depth_Component32,
                                   Width           => Depth_Texture_Size,
                                   Height          => Depth_Texture_Size,
                                   Source_Format   => GL.Pixels.Depth_Component,
                                   Source_Type     => GL.Pixels.Float,
                                   Source          => Image_Source (System.Null_Address));

        Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
        Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
        Texture_2D.Toggle_Compare_X_To_Texture  (True);
        Texture_2D.Set_Compare_Function (GL.Types.LEqual);
        Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_S
        Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_T

    exception
        when others =>
            Put_Line ("An exception occurred in Project_Buffers.Init_Depth_Texture.");
            raise;
    end Init_Depth_Texture;

    --  ------------------------------------------------------------------------

end Project_Buffers;
