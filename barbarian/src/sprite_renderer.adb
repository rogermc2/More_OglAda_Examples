
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;

with Batch_Manager;
with Frustum;
with Game_Utils;
with GL_Maths;
with GL_Utils;
with Manifold;
with Settings;
with Shader_Attributes;
with Shadows;
with Sprite_Shader_Manager;
with Sprite_World_Map;
with Texture_Manager;
with Tiles_Manager;

package body Sprite_Renderer is

   Max_Sprites : constant Positive := 1024;

   --      type Sprite_Boolean_Array is array (1 .. Max_Sprites) of Boolean;
   --      type Sprite_Int_Array is array (1 .. Max_Sprites) of Int;
   --      type Sprite_Float_Array is array (1 .. Max_Sprites) of Float;
   --      type Sprite_Textures_Array is array (1 .. Max_Sprites) of
   --        GL.Objects.Textures.Texture;

   Points_VBO   : GL.Objects.Buffers.Buffer;
   Normals_VBO  : GL.Objects.Buffers.Buffer;
   Tex_VBO      : GL.Objects.Buffers.Buffer;

   --  -------------------------------------------------------------------------

   type Sprite_Data is record
      Model_Matrix           : Singles.Matrix4 := Singles.Identity4;
      World_Position         : Singles.Vector3 := (0.0, 0.0, 0.0);
      Scale                  : Singles.Vector3 := (1.0, 1.0, 1.0);
      Heading_Deg            : Maths.Degree := 0.0;
      Pitch_Deg              : Maths.Degree := 0.0;
      Sprite_Map_Diffuse     : GL.Objects.Textures.Texture;
      Sprite_Map_Specular    : GL.Objects.Textures.Texture;
      Sprite_Map_Rows        : Int := 0;
      Sprite_Map_Columns     : Int := 0;
      Current_Sprite         : Natural := 0;
      Wmap_U                 : Int := -1;
      Wmap_V                 : Int := -1;
      Has_Pitch              : Boolean := False;
      Is_Visible             : Boolean := True;
   end record;

   Sprites           : array (1 .. Max_Sprites) of Sprite_Data;
   Num_Sprites       : Natural := 0;
   Sprite_VAO        : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Sprites_Use_Mips  : Boolean := True;
   Sprite_Lamp_Moved : array (1 .. 2) of Boolean := (False, False);

   --  -------------------------------------------------------------------------

   function Add_Sprite (Diffuse, Specular : GL.Objects.Textures.Texture;
                        Rows, Columns     : Integer) return Natural is
   begin
      if Num_Sprites > Max_Sprites then
         raise Sprite_Exception with "Sprite_Renderer.Add_Sprite, " &
           "maximum number of sprites has already been assigned";
      end if;
      if not Diffuse.Initialized then
         raise Sprite_Exception with "Sprite_Renderer.Add_Sprite, " &
           "Diffuse texture has not been initialized";
      end if;
      if not Specular.Initialized then
         raise Sprite_Exception with "Sprite_Renderer.Add_Sprite, " &
           "Specular texture has not been initialized";
      end if;

      Num_Sprites := Num_Sprites + 1;
      Sprites (Num_Sprites).Sprite_Map_Diffuse := Diffuse;
      Sprites (Num_Sprites).Sprite_Map_Specular := Specular;
      Sprites (Num_Sprites).Sprite_Map_Rows := Int (Rows);
      Sprites (Num_Sprites).Sprite_Map_Columns := Int (Columns);
      return Num_Sprites;

   end Add_Sprite;

   --  -------------------------------------------------------------------------

   procedure Clear_Sprites is
   begin
      for index in 1 .. Num_Sprites loop
         Sprites (index).Model_Matrix  := Singles.Identity4;
         Sprites (index).World_Position := (0.0, 0.0, 0.0);
         Sprites (index).Scale := (1.0, 1.0, 1.0);
         Sprites (index).Heading_Deg := 0.0;
         Sprites (index).Pitch_Deg := 0.0;
         Sprites (index).Sprite_Map_Rows := 0;
         Sprites (index).Sprite_Map_Columns := 0;
         Sprites (index).Current_Sprite := 0;
         Sprites (index).Wmap_U := -1;
         Sprites (index).Wmap_V := -1;
         Sprites (index).Has_Pitch := False;
         Sprites (index).Is_Visible := True;
      end loop;
   end Clear_Sprites;

   --  -----------------------------------------------------------------

   procedure Create_Character_VBOs is
      use GL.Attributes;
      use GL.Objects.Buffers;
      use Shader_Attributes;
      Points       : constant Singles.Vector3_Array (1 .. 6) :=
                       ((-1.0, 0.0, -1.0),
                        (-1.0, 0.0,  1.0),
                        (1.0,  0.0,  1.0),
                        (-1.0, 0.0, -1.0),
                        (1.0, 0.0,  1.0),
                        (1.0, 0.0, -1.0));
      Tex_Coords   : constant Singles.Vector2_Array (1 .. 6) :=
                       ((0.0, 1.0),
                        (0.0, 0.0),
                        (1.0, 0.0),
                        (0.0, 1.0),
                        (1.0, 0.0),
                        (1.0, 1.0));
      Normals      : constant Singles.Vector3_Array (1 .. 6) :=
                       ((0.0, 1.0, 0.0),
                        (0.0, 1.0, 0.0),
                        (0.0, 1.0, 0.0),
                        (0.0, 1.0, 0.0),
                        (0.0, 1.0, 0.0),
                        (0.0, 1.0, 0.0));
   begin
      Points_VBO := GL_Utils.Create_3D_VBO (Points);
      Normals_VBO := GL_Utils.Create_3D_VBO (Normals);
      Tex_VBO := GL_Utils.Create_2D_VBO (Tex_Coords);

      Sprite_VAO.Clear;
      Sprite_VAO.Initialize_Id;

      GL_Utils.Add_Attribute_To_Array (Sprite_VAO, Attrib_VP, Points_VBO, 3);
--        Sprite_VAO.Bind;

      GL_Utils.Add_Attribute_To_Array (Sprite_VAO, Attrib_VT, Tex_VBO, 2);
      GL_Utils.Add_Attribute_To_Array (Sprite_VAO, Attrib_VN, Normals_VBO, 3);

   end Create_Character_VBOs;

   --  -------------------------------------------------------------------------

   procedure Init is
   begin
      Game_Utils.Game_Log ("--- Initializing Sprites ---");
      Clear_Sprites;
      Create_Character_VBOs;
      Sprite_Shader_Manager.Load_Sprite_Shaders;
      Game_Utils.Game_Log ("--- Sprites Initialized ---");
   end Init;

   --  -------------------------------------------------------------------------

   procedure Render_Sprite (Sprite_Index : Positive) is
      use Singles;
      use Maths;
      use Sprite_Shader_Manager;
      use Texture_Manager;
      Opacity : constant Single := 1.0;
      U       : constant Positive := Positive (Sprites (Sprite_Index).Wmap_U);
      V       : constant Positive := Positive (Sprites (Sprite_Index).Wmap_V);
   begin
      if Sprites (Sprite_Index).Is_Visible and Frustum.Is_Sphere_In_Frustum
        (Sprites (Sprite_Index).World_Position, 1.0) then
         GL_Utils.Bind_Vao (Sprite_VAO);
         Bind_Texture (0, Sprites (Sprite_Index).Sprite_Map_Diffuse);
         Bind_Texture (1, Sprites (Sprite_Index).Sprite_Map_Specular);

         Use_Sprite_Shader;
         Set_Rows (Single (Sprites (Sprite_Index).Sprite_Map_Rows));
         Set_Columns (Single (Sprites (Sprite_Index).Sprite_Map_Columns));
         Set_Current_Sprite (Single (Sprites (Sprite_Index).Current_Sprite));
         Set_Opacity (Opacity);
         Set_Model (Sprites (Sprite_Index).Model_Matrix);
         Set_Static_Light_Indices ((Manifold.Get_Light_Index (U, V, 1),
                                   Manifold.Get_Light_Index (U, V, 2)));
         GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 6);
      end if;
   end Render_Sprite;

   --  -------------------------------------------------------------------------

   procedure Set_Ambient_Light_Level (Rgb : Singles.Vector3) is
   begin
      Sprite_Shader_Manager.Use_Sprite_Shader;
      Sprite_Shader_Manager.Set_L_A (Rgb);
   end Set_Ambient_Light_Level;

   --  -------------------------------------------------------------------------

   procedure Set_Sprite_Current_Sprite (Sprite_Index, Current_Sprite : Natural) is
   begin
      Sprites (Sprite_Index).Current_Sprite := Current_Sprite;
   end Set_Sprite_Current_Sprite;

   --  -------------------------------------------------------------------------

   procedure Set_Sprite_Heading (Sprite_Index : Positive;
                                 Heading_Deg  : Maths.Degree) is
   begin
      Sprites (Sprite_Index).Heading_Deg := Heading_Deg;
   end Set_Sprite_Heading;

   --  -------------------------------------------------------------------------

   procedure Set_Sprite_Pitch (Sprite_Index : Positive;
                               Pitch_Deg    : Maths.Degree) is
   begin
      Sprites (Sprite_Index).Pitch_Deg := Pitch_Deg;
   end Set_Sprite_Pitch;

   --  -------------------------------------------------------------------------

   procedure Set_Sprite_Position (Sprite_Index : Positive;
                                  World_Pos    : Singles.Vector3) is
      use Singles;
      use Maths;
      U    : Int := Int (Max (0.5 * (World_Pos (GL.X) + 1.0), 0.0));
      V    : Int := Int (Max (0.5 * (World_Pos (GL.Y) + 1.0), 0.0));
   begin
      U := Min_Int (U, Int (Tiles_Manager.Max_Map_Rows - 1));
      V := Min_Int (V, Int (Tiles_Manager.Max_Map_Cols - 1));

      Sprites (Sprite_Index).World_Position := World_Pos;
      Sprites (Sprite_Index).Model_Matrix :=
        Scaling_Matrix (Sprites (Sprite_Index).Scale);

      if Sprites (Sprite_Index).Has_Pitch then
         Sprites (Sprite_Index).Model_Matrix :=
           Rotation_Matrix (To_Radians (Sprites (Sprite_Index).Pitch_Deg),
                            (1.0, 0.0, 0.0)) *
             Sprites (Sprite_Index).Model_Matrix;
      end if;

      Sprites (Sprite_Index).Model_Matrix :=
        Rotation_Matrix (To_Radians (Sprites (Sprite_Index).Heading_Deg),
                         (0.0, 1.0, 0.0)) * Sprites (Sprite_Index).Model_Matrix;
      Sprite_World_Map.Move_Sprite_In_World_Map
        (Sprites (Sprite_Index).Wmap_U, Sprites (Sprite_Index).Wmap_V,
         U, V, World_Pos (GL.X), Sprite_Index);
      Sprites (Sprite_Index).Wmap_U := U;
      Sprites (Sprite_Index).Wmap_V := V;

   end Set_Sprite_Position;

   --  -------------------------------------------------------------------------

   procedure Set_Sprite_Scale (Sprite_Index : Positive;
                               Scale        : Singles.Vector3) is
   begin
      Sprites (Sprite_Index).Scale := Scale;
   end Set_Sprite_Scale;

   --  -------------------------------------------------------------------------

   procedure Set_Sprite_Visible (Sprite_Index : Positive; Visible : Boolean) is
   begin
      Sprites (Sprite_Index).Is_Visible := Visible;
   end Set_Sprite_Visible;

   --  -------------------------------------------------------------------------

   function Sprite_World_Pos (Sprite_Index : Positive) return Singles.Vector3  is
   begin
      return Sprites (Sprite_Index).World_Position;
   end Sprite_World_Pos;

   --  -------------------------------------------------------------------------
   --  NOTE : assuming blend etc. already set
   --  glEnable (GL_BLEND);
   --  glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   --  disable writing to depth buffer but still allow testing it
   --  for 3d transparency.
   --  glDepthMask (GL_FALSE);
   procedure Start_Sprite_Rendering is
   begin
      GL_Utils.Bind_VAO (Sprite_VAO);
      Sprite_Shader_Manager.Use_Sprite_Shader;
      if Camera.Is_Dirty then
         Sprite_Shader_Manager.Set_View (Camera.View_Matrix);
         Sprite_Shader_Manager.Set_Perspective (Camera.Projection_Matrix);
      end if;

      if Settings.Shadows_Enabled then
         Sprite_Shader_Manager.Set_Shadow_Enabled (1.0);
         Shadows.Bind_Cube_Shadow_Texture (3);
         Sprite_Shader_Manager.Set_Caster_Position (Shadows.Caster_Position);
      else
         Sprite_Shader_Manager.Set_Shadow_Enabled (0.0);
      end if;

   end Start_Sprite_Rendering;

   --  -------------------------------------------------------------------------

   procedure Update_Dynamic_Light (Pos_Wor, Diff, Spec : Singles.Vector3;
                                   Light_Range         : Single) is
   begin
      Sprite_Shader_Manager.Use_Sprite_Shader;
      Sprite_Shader_Manager.Set_Dyn_Light_Pos (Pos_Wor);
      Sprite_Shader_Manager.Set_Dyn_Light_Diff (Diff);
      Sprite_Shader_Manager.Set_Dyn_Light_Spec (Spec);
      Sprite_Shader_Manager.Set_Dyn_Light_Range (Light_Range);
   end Update_Dynamic_Light;

   --  -------------------------------------------------------------------------

   procedure Update_Static_Lights_Uniforms is
      use Batch_Manager;
      use GL_Maths.Indices_Package;
      use Sprite_Shader_Manager;
      aLight    : Static_Light_Data;
      Positions : Light_Array;
      Diffuse   : Light_Array;
      Specular  : Light_Array;
      Ranges    : Light_Range_Array;
   begin
      Sprite_Shader_Manager.Use_Sprite_Shader;
      for Index in Static_Lights.First_Index .. Static_Lights.Last_Index loop
         aLight := Element (Static_Lights, Index);
         Positions (Int (Index)) := aLight.Position;
         Diffuse (Int (Index)) := aLight.Diffuse;
         Specular (Int (Index)) := aLight.Specular;
         Ranges (Int (Index)) := aLight.Light_Range;
      end loop;

      Sprite_Shader_Manager.Set_Light_Pos (Positions);
      Sprite_Shader_Manager.Set_Light_Diff (Diffuse);
      Sprite_Shader_Manager.Set_Light_Spec (Specular);
      Sprite_Shader_Manager.Set_Light_Range (Ranges);

   end Update_Static_Lights_Uniforms;

   --  -------------------------------------------------------------------------

end Sprite_Renderer;
