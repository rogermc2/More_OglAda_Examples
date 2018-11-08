
package GL.API.Ext is
   -- GL.Enum
   type Geometry_Type is (GL_Lines_Adjacency_Ext,
                          GL_Line_Strip_Adjacency_Ext,
                          GL_Triangles_Adjacency_Ext,
                          GL_Triangles_Strip_Adjacency_Ext,
                          GL_Max_Varying_Components_Ext,
                          Gl_Max_Geometry_Texture_Image_Units_Ext,
                          GL_Framebuffer_Attachment_Texture_Layer_Ext,
                          GL_Framebuffer_Attachment_Layered_Ext,
                          GL_Framebuffer_Incomplete_Layer_Targets_Ext,
                          GL_Framebuffer_Incomplete_Layer_Count_Ext,
                          Gl_Geometry_Shader_Ext,
                          Gl_Geometry_Vertices_Out_Ext,
                          Gl_Geometry_Input_Type_Ext,
                          Gl_Geometry_Output_Type_Ext,
                          Gl_Max_Geometry_Varying_Components_Ext,
                          GL_Max_Vertex_Varying_Components_Ext,
                          Gl_Max_Geometry_Uniform_Components_Ext,
                          Gl_Max_Geometry_Output_Vertices_Ext,
                          Gl_Max_Geometry_Total_Output_Components_Ext);
   pragma Convention (C, Geometry_Type);

   --  void APIENTRY glProgramParameteri (GLuint program, GLenum pname, GLint value)
   procedure Program_Parameter (Program: UInt; Name : Geometry_Type;
                                Value  : Connection_Mode);
   pragma Import (C, Program_Parameter, "glProgramParameteriEXT");

private

   for Geometry_Type use (GL_Lines_Adjacency_Ext                       => 16#A#,
                          GL_Line_Strip_Adjacency_Ext                  => 16#B#,
                          GL_Triangles_Adjacency_Ext                   => 16#C#,
                          GL_Triangles_Strip_Adjacency_Ext             => 16#D#,
                          GL_Max_Varying_Components_Ext                => 16#8B4B#,
                          Gl_Max_Geometry_Texture_Image_Units_Ext      => 16#8C29#,
                          GL_Framebuffer_Attachment_Texture_Layer_Ext  => 16#8CD4#,
                          GL_Framebuffer_Attachment_Layered_Ext        => 16#8DA7#,
                          GL_Framebuffer_Incomplete_Layer_Targets_Ext  => 16#8DA8#,
                          GL_Framebuffer_Incomplete_Layer_Count_Ext    => 16#8DA9#,
                          Gl_Geometry_Shader_Ext                       => 16#8DD9#,
                          Gl_Geometry_Vertices_Out_Ext                 => 16#8DDA#,
                          Gl_Geometry_Input_Type_Ext                   => 16#8DDB#,
                          Gl_Geometry_Output_Type_Ext                  => 16#8DDC#,
                          Gl_Max_Geometry_Varying_Components_Ext       => 16#8DDD#,
                          GL_Max_Vertex_Varying_Components_Ext         => 16#8DDE#,
                          Gl_Max_Geometry_Uniform_Components_Ext       => 16#8DDF#,
                          Gl_Max_Geometry_Output_Vertices_Ext          => 16#8DE0#,
                          Gl_Max_Geometry_Total_Output_Components_Ext  => 16#8DE1#);
   for Geometry_Type'Size use Low_Level.Enum'Size;

end GL.API.Ext;
