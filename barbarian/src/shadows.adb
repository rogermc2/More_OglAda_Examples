
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;

with Maths;

with Debug_Quad_Shader_Manager;
with Depth_Shader_Manager;
with Depth_Skinned_Shader_Manager;
with Game_Utils;
with GL_Utils;
with Screen_Space_Quad;
with Settings;
with Shader_Attributes;
with Texture_Manager;

package body Shadows is

   type Shadow_Data is record
      --  Camera settings
      Near             : Single := 0.01;
      Far              : Single := 100.0;
      Fovy             : Maths.Degree := 90.0;
      Aspect           : Single := 1.0;
      Caster_Vs        : Singles.Matrix4_Array (1 .. 6) :=
                           (others => (others => (others => 0.0)));
      Caster_P         : Singles.Matrix4 := Singles.Identity4;
      Caster_Pos_Wor   : Singles.Vector3 := Maths.Vec3_0;
      --  Shaders
      Depth_Sp         : GL.Objects.Programs.Program;
      Depth_Skinned_Sp : GL.Objects.Programs.Program;
      --  Debug
      Debug_Quad_Sp    : GL.Objects.Programs.Program;
      Debug_Quad_Vao   : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Cube_Fb          : GL.Objects.Buffers.Buffer;
      Cube_Colour_Tex  : GL.Objects.Textures.Texture;
      Render_Buffer    : GL.Objects.Buffers.Buffer;
   end record;

   G_Shadows : Shadow_Data;

   procedure Reset_Shadows;

   --  ----------------------------------------------------------------------------

   procedure Bind_Cube_Shadow_Texture (Slot : Integer) is
   begin
      if Settings.Shadows_Enabled then
         Texture_Manager.Bind_Cube_Texture (Slot, G_Shadows.Cube_Colour_Tex);
      end if;
   end Bind_Cube_Shadow_Texture;

   --  ----------------------------------------------------------------------------

   procedure Init is
      use GL.Attributes;
      use GL.Objects.Buffers;
      use Shader_Attributes;
      use Screen_Space_Quad;
      Bone_Matrices : constant Depth_Skinned_Shader_Manager.Bone_Matrices_Array
        := (others => Singles.Identity4);
      Quad_VP_Buffer : GL.Objects.Buffers.Buffer;
      Quad_VT_Buffer : GL.Objects.Buffers.Buffer;
   begin
      Game_Utils.Game_Log ("---INITIALISING SHADOWS---");
      Reset_Shadows;
      Depth_Shader_Manager.Init (G_Shadows.Depth_Sp);
      Depth_Shader_Manager.Set_Projection_Matrix (G_Shadows.Caster_P);
      Depth_Skinned_Shader_Manager.Init (G_Shadows.Depth_Skinned_Sp);
      Depth_Skinned_Shader_Manager.Set_Projection_Matrix (G_Shadows.Caster_P);
      Depth_Skinned_Shader_Manager.Set_Bone_Matrices (Bone_Matrices);
      Debug_Quad_Shader_Manager.Init (G_Shadows.Debug_Quad_Sp);

      Game_Utils.Game_Log ("Using CUBEMAP + renderbuffer shadows");
      Quad_VP_Buffer := GL_Utils.Create_2D_VBO (SS_Quad_Position);
      Quad_VT_Buffer := GL_Utils.Create_3D_VBO (SS_Quad_ST);

      G_Shadows.Debug_Quad_Vao.Initialize_Id;
      G_Shadows.Debug_Quad_Vao.Bind;
      Array_Buffer.Bind (Quad_VP_Buffer);
      Set_Vertex_Attrib_Pointer (Attrib_VP, 2, Single_Type, False, 0, 0);
      Enable_Vertex_Attrib_Array (Attrib_VP);

      Array_Buffer.Bind (Quad_VT_Buffer);
      Set_Vertex_Attrib_Pointer (Attrib_VT, 3, Single_Type, False, 0, 0);
      Enable_Vertex_Attrib_Array (Attrib_VT);

      Game_Utils.Game_Log ("---SHADOWS INITIALISED---");

   exception
      when others =>
         Put_Line ("An exception occurred in Shadows.Init.");
         raise;
   end Init;

   --  ----------------------------------------------------------------------------

   function Caster_Position return Singles.Vector3 is
   begin
      return G_Shadows.Caster_Pos_Wor;
   end Caster_Position;

   --  ---------------------------------------------------------------------

   procedure Reset_Shadows is
      use GL.Types;
      use Maths;
      Caster_Pos : constant Singles.Vector3 := (0.0, 0.0, 0.0);
      --  view matrix for the shadow caster
      Target_Pos : constant Singles.Vector3_Array (1 .. 6) :=
                     ((1.0, 0.0, 0.0),    --  posx
                      (-1.0, 0.0, 0.0),   --  negx
                      (0.0, 1.0, 0.0),    -- posy
                      (0.0, -1.0, 0.0),   -- negy
                      (0.0, 0.0, 1.0),    -- posz
                      (0.0, 0.0, -1.0));  -- negz
      --  upside-down up to store texture right- way up
      Up : constant Singles.Vector3_Array (1 .. 6) :=
                     ((0.0, -1.0, 0.0),
                      (0.0, -1.0, 0.0),
                      (0.0, 0.0, 1.0),    -- posy
                      (0.0, 0.0, -1.0),   -- negy
                      (0.0, -1.0, 0.0),
                      (0.0, -1.0, 0.0));
      LA_Matrix  : Singles.Matrix4;
   begin
      G_Shadows.Near := 0.01;
      G_Shadows.Far := 100.0;
      G_Shadows.Fovy := 90.0;
      G_Shadows.Aspect := 1.0;
      G_Shadows.Caster_Pos_Wor := (0.0, 0.0, 0.0);

      for index in Int range 1 .. 6 loop
         Init_Lookat_Transform (Caster_Pos, Target_Pos (index),
                                Up (index), LA_Matrix);
         G_Shadows.Caster_Vs (index) := LA_Matrix;
      end loop;
      G_Shadows.Caster_P := Perspective_Matrix
        (G_Shadows.Fovy, G_Shadows.Aspect, G_Shadows.Near, G_Shadows.Far);

   end Reset_Shadows;

   --  ----------------------------------------------------------------------------

end Shadows;
