
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Low_Level.Enums;
with GL.Objects.Buffers;
with GL.Objects.Framebuffers;
with GL.Objects.Programs;
with GL.Objects.Textures.Targets;
with GL.Objects.Renderbuffers;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Pixels;
with GL.Types.Colors;
with GL.Window;

with Maths;
with Utilities;

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
      Caster_Pos_World : Singles.Vector3 := Maths.Vec3_0;
      --  Shaders
      Depth_Sp         : GL.Objects.Programs.Program;
      Depth_Skinned_Sp : GL.Objects.Programs.Program;
      --  Debug
      Debug_Quad_Sp    : GL.Objects.Programs.Program;
      Debug_Quad_Vao   : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Cube_Framebuffer : GL.Objects.Framebuffers.Framebuffer;
      Cube_Colour_Tex  : GL.Objects.Textures.Texture;
      Render_Buffer    : GL.Objects.Renderbuffers.Renderbuffer;
   end record;

   Black     : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 1.0);
   G_Shadows : Shadow_Data;

    procedure Load_Cube_Map_Texture
      (theTexture : GL.Objects.Textures.Targets.Cube_Map_Side_Target.Fillable_Target);
   procedure Reset_Shadows;

   --  ----------------------------------------------------------------------------

   procedure Bind_Cube_Shadow_Texture (Slot : Integer) is
   begin
      if Settings.Shadows_Enabled then
         Texture_Manager.Bind_Cube_Texture (Slot, G_Shadows.Cube_Colour_Tex);
      end if;
   end Bind_Cube_Shadow_Texture;

   --  ----------------------------------------------------------------------------

   procedure Bind_Shadow_FB (Dir : Shadow_Direction) is
      use GL.Objects.Framebuffers;
      use GL.Objects.Textures.Targets;
   begin
      if Settings.Shadows_Enabled then
         Game_Utils.Game_Log ("Shadows.Bind_Shadow_FB Bind");
         Read_And_Draw_Target.Bind (G_Shadows.Cube_Framebuffer);
         Game_Utils.Game_Log ("Shadows.Bind_Shadow_FB Cube_Framebuffer bound");
         --  Stuff like water has no depth render, but the bottom should appear
         --  as if maximally far away which == white.
         --  If it's cleared to black we see odd artifacts appear on these
         --  surfaces because they are picked up as minimally close instead of
         --  maximally far.
         GL.Buffers.Set_Color_Clear_Value ((1.0, 1.0, 1.0, 1.0));
         GL.Window.Set_Viewport (0, 0, Int (Settings.Shadows_Size),
                                 Int (Settings.Shadows_Size));
         Read_And_Draw_Target.Attach_Texture
           (Color_Attachment_0, G_Shadows.Cube_Colour_Tex, 0);
         Game_Utils.Game_Log ("**** Shadows.Bind_Shadow_FB Cube_Colour_Tex attached");
         Utilities.Clear_Colour_Buffer_And_Depth;

         GL.Objects.Programs.Use_Program (G_Shadows.Depth_Sp);
         Depth_Shader_Manager.Set_View_Matrix
           (G_Shadows.Caster_Vs (Shadow_Direction'Enum_Rep (Dir)));
         GL.Objects.Programs.Use_Program (G_Shadows.Depth_Skinned_Sp);
         Depth_Skinned_Shader_Manager.Set_View_Matrix
           (G_Shadows.Caster_Vs (Shadow_Direction'Enum_Rep (Dir)));
         Utilities.Clear_Background_Colour (Black);
      end if;
   end Bind_Shadow_FB;

    --  ----------------------------------------------------------------------------

    procedure Change_Shadow_Size (Dim : Integer) is
          use GL.Objects.Framebuffers;
          use GL.Objects.Textures;
          use GL.Objects.Textures.Targets;
    begin
        Game_Utils.Game_Log
          ("Shadows.Change_Shadow_Size generating shadow textures, size " &
             Integer'Image (Dim));
        Read_And_Draw_Target.Bind (G_Shadows.Cube_Framebuffer);

        Texture_Manager.Bind_Cube_Texture (0, G_Shadows.Cube_Colour_Tex);
        Load_Cube_Map_Texture (Texture_Cube_Map_Positive_X);
        Load_Cube_Map_Texture (Texture_Cube_Map_Positive_Y);
        Load_Cube_Map_Texture (Texture_Cube_Map_Positive_Z);
        Load_Cube_Map_Texture (Texture_Cube_Map_Negative_X);
        Load_Cube_Map_Texture (Texture_Cube_Map_Negative_Y);
        Load_Cube_Map_Texture (Texture_Cube_Map_Negative_Z);

        Texture_Cube_Map.Set_Magnifying_Filter (Linear);
        Texture_Cube_Map.Set_Minifying_Filter (Linear);
        Texture_Cube_Map.Set_X_Wrapping (Clamp_To_Edge);
        Texture_Cube_Map.Set_Y_Wrapping (Clamp_To_Edge);
        Texture_Cube_Map.Set_Z_Wrapping (Clamp_To_Edge);

        Read_And_Draw_Target.Attach_Texture_2D
          (Attachment => Color_Attachment_0,
           Tex_Target => GL.Low_Level.Enums.Texture_Cube_Map_Negative_Z,
           Object     => G_Shadows.Cube_Colour_Tex);
        Game_Utils.Game_Log
          ("Shadows.Change_Shadow_Size Texture attached 2");
        GL.Objects.Renderbuffers.Active_Renderbuffer.Allocate
         (GL.Pixels.Depth_Component, Int (Settings.Shadows_Size),
          Int (Settings.Shadows_Size));
        Game_Utils.Game_Log
          ("Shadows.Change_Shadow_Size Renderbuffer Allocated");
        Read_And_Draw_Target.Attach_Renderbuffer
          (Depth_Attachment, G_Shadows.Render_Buffer);

        if not GL_Utils.Verify_Bound_Framebuffer then
            raise Shadows_Error with
              "Shadows.Change_Shadow_Size; error verifying framebuffer for depth";
        end if;
        Read_And_Draw_Target.Bind (Default_Framebuffer);

    exception
        when others => raise Shadows_Error with
              "Shadows.Change_Shadow_Size exception";
    end Change_Shadow_Size;

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

      G_Shadows.Debug_Quad_Vao.Clear;
      G_Shadows.Debug_Quad_Vao.Initialize_Id;
      G_Shadows.Debug_Quad_Vao.Bind;
      Array_Buffer.Bind (Quad_VP_Buffer);
      Set_Vertex_Attrib_Pointer (Attrib_VP, 2, Single_Type, False, 0, 0);
      Enable_Vertex_Attrib_Array (Attrib_VP);

      Array_Buffer.Bind (Quad_VT_Buffer);
      Set_Vertex_Attrib_Pointer (Attrib_VT, 3, Single_Type, False, 0, 0);
      Enable_Vertex_Attrib_Array (Attrib_VT);

      G_Shadows.Cube_Framebuffer.Initialize_Id;
      G_Shadows.Cube_Colour_Tex.Initialize_Id;
      G_Shadows.Render_Buffer.Initialize_Id;

      Game_Utils.Game_Log ("---SHADOWS INITIALISED---");
      Change_Shadow_Size (Settings.Shadows_Size);

   exception
      when others =>
         Put_Line ("An exception occurred in Shadows.Init.");
         raise;
   end Init;

   --  -------------------------------------------------------------------------

   function Caster_Position return Singles.Vector3 is
   begin
      return G_Shadows.Caster_Pos_World;
   end Caster_Position;

   --  ---------------------------------------------------------------------

    procedure Load_Cube_Map_Texture
      (theTexture : GL.Objects.Textures.Targets.Cube_Map_Side_Target.Fillable_Target) is
      use GL.Objects.Textures.Targets;
    begin
        theTexture.Load_Empty_Texture
          (Level => 0, Internal_Format => GL.Pixels.RGBA16F,
           Width => Int (Settings.Shadows_Size),
           Height  => Int (Settings.Shadows_Size));
    end Load_Cube_Map_Texture;

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
      G_Shadows.Caster_Pos_World := (0.0, 0.0, 0.0);

      for index in Int range 1 .. 6 loop
         Init_Lookat_Transform (Caster_Pos, Target_Pos (index),
                                Up (index), LA_Matrix);
         G_Shadows.Caster_Vs (index) := LA_Matrix;
      end loop;
      G_Shadows.Caster_P := Perspective_Matrix
        (G_Shadows.Fovy, G_Shadows.Aspect, G_Shadows.Near, G_Shadows.Far);

   end Reset_Shadows;

   --  ----------------------------------------------------------------------------

   procedure Set_Depth_Model_Matrix (Mat : Singles.Matrix4) is
    begin
      if Settings.Shadows_Enabled then
         GL.Objects.Programs.Use_Program (G_Shadows.Depth_Sp);
         Depth_Shader_Manager.Set_Model_Matrix (Mat);
        end if;
   end Set_Depth_Model_Matrix;

   --  ----------------------------------------------------------------------------

   procedure Set_Depth_Skinned_Bone_Matrices (Mats : Singles.Matrix4_Array) is
    begin
      if Settings.Shadows_Enabled then
         GL.Objects.Programs.Use_Program (G_Shadows.Depth_Skinned_Sp);
         Depth_Skinned_Shader_Manager.Set_Bone_Matrices (Mats);
      end if;
   end Set_Depth_Skinned_Bone_Matrices;

   --  ----------------------------------------------------------------------------

   procedure Set_Depth_Skinned_Model_Matrix (Mat : Singles.Matrix4) is
    begin
      if Settings.Shadows_Enabled then
         GL.Objects.Programs.Use_Program (G_Shadows.Depth_Skinned_Sp);
         Depth_Skinned_Shader_Manager.Set_Model_Matrix (Mat);
      end if;
   end Set_Depth_Skinned_Model_Matrix;

   --  ----------------------------------------------------------------------------

end Shadows;
