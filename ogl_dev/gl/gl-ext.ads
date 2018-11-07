
with GL.Objects.Programs;
with GL.Types; use  GL.Types;

package GL.Ext is

    GL_Lines_Adjacency_Ext                       : constant Uint := 16#A#;
    GL_Line_Strip_Adjacency_Ext                  : constant Uint := 16#B#;
    GL_Triangles_Adjacency_Ext                   : constant Uint := 16#C#;
    GL_Triangles_Strip_Adjacency_Ext             : constant Uint := 16#D#;
    GL_Max_Varying_Components_Ext                : constant Uint := 16#8B4B#;
    Gl_Max_Geometry_Texture_Image_Units_Ext      : constant Uint := 16#8C29#;
    GL_Framebuffer_Attachment_Texture_Layer_Ext  : constant Uint := 16#8CD4#;
    GL_Framebuffer_Attachment_Layered_Ext        : constant Uint := 16#8DA7#;
    GL_Framebuffer_Incomplete_Layer_Targets_Ext  : constant Uint := 16#8DA8#;
    GL_Framebuffer_Incomplete_Layer_Count_Ext    : constant Uint := 16#8DA9#;
    Gl_Geometry_Shader_Ext                       : constant Uint := 16#8DD9#;
    Gl_Geometry_Vertices_Out_Ext                 : constant Uint := 16#8DDA#;
    Gl_Geometry_Input_Type_Ext                   : constant Uint := 16#8DDB#;
    Gl_Geometry_Output_Type_Ext                  : constant Uint := 16#8DDC#;
    Gl_Max_Geometry_Varying_Components_Ext       : constant Uint := 16#8DDD#;
    GL_Max_Vertex_Varying_Components_Ext         : constant Uint := 16#8DDE#;
    Gl_Max_Geometry_Uniform_Components_Ext       : constant Uint := 16#8DDF#;
    Gl_Max_Geometry_Output_Vertices_Ext          : constant Uint := 16#8DE0#;
    Gl_Max_Geometry_Total_Output_Components_Ext  : constant Uint := 16#8DE1#;

    procedure Set_Geometry_Input_Type (theProgram : GL.Objects.Programs.Program;
                                       Input_Type : Connection_Mode);
    procedure Set_Geometry_Output_Type (theProgram : GL.Objects.Programs.Program;
                                        Output_Type : Connection_Mode);
    procedure Set_Geometry_Vertices_Out_Type (theProgram : GL.Objects.Programs.Program;
                                              Output_Type : UInt);

end GL.Ext;
