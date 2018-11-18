
with Ada.Text_IO; use  Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Toggles;
with GL.Types; use  GL.Types;

with Vertex_Data;

package body My_Buffers is

    procedure Load_Vertex_Buffer is new GL.Objects.Buffers.Load_To_Buffer (Vertex_Data.pVertex_Pointers);
    procedure Load_Index_Buffer is new GL.Objects.Buffers.Load_To_Buffer (Vertex_Data.pIndex_Pointers);

    --  -----------------------------------------------------------------------------------------------------------------------

    procedure Setup_Buffers (Frame_Buffer     : in out GL.Objects.Framebuffers.Framebuffer;
                             Position_Buffer  : in out GL.Objects.Buffers.Buffer;
                             Index_Buffer     : in out GL.Objects.Buffers.Buffer) is
        use GL.Objects.Buffers;
        use GL.Objects.Framebuffers;
        Stride  : constant GL.Types.Size := 5;
    begin

        Position_Buffer.Initialize_Id;
        Array_Buffer.Bind (Position_Buffer);
        Array_Buffer.Bind (Position_Buffer);      -- Set current buffer
        Load_Vertex_Buffer (Array_Buffer, Vertex_Data.Vertices_With_Tex, Static_Draw);

        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        GL.Attributes.Enable_Vertex_Attrib_Array (1);
        GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => 3,
                                                 Kind   => GL.Types.Single_Type,
                                                 Stride => Stride, Offset => 0);

        GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, GL.Types.Single_Type,
                                                 Stride, 3);

        Index_Buffer.Initialize_Id;
        Element_Array_Buffer.Bind (Index_Buffer);
        Element_Array_Buffer.Bind (Index_Buffer);  -- Set current buffer
        Load_Index_Buffer (Element_Array_Buffer, Vertex_Data.Vertex_Indices, Static_Draw);

        GL.Toggles.Enable (GL.Toggles.Cull_Face);
        GL.Toggles.Enable (GL.Toggles.Depth_Test);
        GL.Buffers.Set_Depth_Function (GL.Types.GEqual);

        Frame_Buffer.Initialize_Id;
        -- Read_And_Draw_Target : constant Framebuffer_Target; = GL_FRAMEBUFFER
        Read_And_Draw_Target.Bind (Frame_Buffer);  -- Set current buffer

    exception
        when others =>
            Put_Line ("An exceptiom occurred in Setup_Buffers.");
            raise;
    end Setup_Buffers;

    --  ------------------------------------------------------------------------

    procedure Setup_Textures (Frame_Buffer     : in out GL.Objects.Framebuffers.Framebuffer;
                              Colour_Texture   : in out GL.Objects.Textures.Texture;
                              Draw_Buffer_List : in out GL.Buffers.Explicit_Color_Buffer_List) is
        use GL.Objects.Framebuffers;
        use GL.Objects.Textures.Targets;
        Depth_Texture    : GL.Objects.Textures.Texture;
    begin
        Colour_Texture.Initialize_Id;
        Texture_2D.Bind (Colour_Texture);
        Texture_2D.Storage (9, GL.Pixels.RGBA8, 512, 512);

        Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
        Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);

        Depth_Texture.Initialize_Id;
        Texture_2D.Bind (Depth_Texture);
        Texture_2D.Storage (9, GL.Pixels.Depth_Component32, 512, 512);

        Read_And_Draw_Target.Bind (Frame_Buffer);  -- Set current buffer
        --  Read_And_Draw_Target = GL_FRAMEBUFFER
        Read_And_Draw_Target.Attach_Texture (Color_Attachment_0, Colour_Texture, 0);
        Read_And_Draw_Target.Attach_Texture (Depth_Attachment, Depth_Texture, 0);

        Draw_Buffer_List (0) := GL.Buffers.Color_Attachment0;
        GL.Buffers.Set_Active_Buffers (Draw_Buffer_List);

    exception
        when others =>
            Put_Line ("An exceptiom occurred in Setup_Textures.");
            raise;
    end Setup_Textures;

end My_Buffers;
