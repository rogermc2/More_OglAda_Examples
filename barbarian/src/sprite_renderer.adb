
with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;

with Game_Utils;
with GL_Utils;
with Manifold;
with Shader_Attributes;
with Sprite_Shader_Manager;
with Sprite_World_Map;

package body Sprite_Renderer is

    Max_Sprites : constant Integer := 1024;

    --      type Sprite_Boolean_Array is array (1 .. Max_Sprites) of Boolean;
    --      type Sprite_Int_Array is array (1 .. Max_Sprites) of Int;
    --      type Sprite_Float_Array is array (1 .. Max_Sprites) of Float;
    --      type Sprite_Textures_Array is array (1 .. Max_Sprites) of
    --        GL.Objects.Textures.Texture;

    type Sprite_Data is record
        Model_Matrix           : Singles.Matrix4 := Singles.Identity4;
        World_Position         : Singles.Vector3 := (0.0, 0.0, 0.0);
        Scale                  : Singles.Vector3 := (1.0, 1.0, 1.0);
        Heading_Deg            : Maths.Degree := 0.0;
        Pitch_Deg              : Maths.Degree := 0.0;
        Sprite_Map_Diffuse_Id  : GL.Objects.Textures.Texture;
        Sprite_Map_Specular_Id : GL.Objects.Textures.Texture;
        Sprite_Map_Rows        : Int := 0;
        Sprite_Map_Columns     : Int := 0;
        Current_Sprite         : Integer := 0;
        Wmap_U                 : Int := -1;
        Wmap_V                 : Int := -1;
        Has_Pitch              : Boolean := False;
        Is_Visible             : Boolean := True;
    end record;

    Sprites           : array (1 .. Max_Sprites) of Sprite_Data;
    Num_Sprites       : Integer := 0;
    Sprite_VAO        : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Sprites_Use_Mips  : Boolean := True;
    Sprite_Lamp_Moved : array (1 .. 2) of Boolean := (False, False);

    --  -------------------------------------------------------------------------

    function Add_Sprite (Diffuse, Specular : GL.Objects.Textures.Texture;
                         Columns, Rows : Integer) return Integer is
    begin
        if Num_Sprites > Max_Sprites then
            raise Sprite_Exception with "Sprite_Renderer.Add_Sprite, " &
              "maximum number of sprites has already been assigned";
        end if;
        Num_Sprites := Num_Sprites + 1;
        Sprites (Num_Sprites).Sprite_Map_Diffuse_Id := Diffuse;
        Sprites (Num_Sprites).Sprite_Map_Specular_Id := Specular;
        Sprites (Num_Sprites).Sprite_Map_Rows := Int (Rows);
        Sprites (Num_Sprites).Sprite_Map_Columns := Int (Columns);
        return Num_Sprites;

    end Add_Sprite;

    --  -------------------------------------------------------------------------

    procedure Clear_Sprites is
    begin
        null;
    end Clear_Sprites;

    --  -----------------------------------------------------------------

    procedure Create_Character_VBOs is
        use GL.Attributes;
        use GL.Objects.Buffers;
        use Shader_Attributes;
        Points     : constant Singles.Vector3_Array (1 .. 6) :=
                       ((-1.0, 0.0, -1.0),
                        (-1.0, 0.0,  1.0),
                        (1.0,  0.0,  1.0),
                        (-1.0, 0.0, -1.0),
                        (1.0, 0.0,  1.0),
                        (1.0, 0.0, -1.0));
        Tex_Coords : constant Singles.Vector2_Array (1 .. 6) :=
                       ((0.0, 1.0),
                        (0.0, 0.0),
                        (1.0, 0.0),
                        (0.0, 1.0),
                        (1.0, 0.0),
                        (1.0, 1.0));
        Normals     : constant Singles.Vector3_Array (1 .. 6) :=
                        ((0.0, 1.0, 0.0),
                         (0.0, 1.0, 0.0),
                         (0.0, 1.0, 0.0),
                         (0.0, 1.0, 0.0),
                         (0.0, 1.0, 0.0),
                         (0.0, 1.0, 0.0));
        Points_VBO   : constant GL.Objects.Buffers.Buffer :=
                         GL_Utils.Create_3D_VBO (Points);
        Normals_VBO  : constant GL.Objects.Buffers.Buffer :=
                         GL_Utils.Create_3D_VBO (Normals);
        Tex_VBO      : constant GL.Objects.Buffers.Buffer :=
                         GL_Utils.Create_2D_VBO (Tex_Coords);
    begin
        Sprite_VAO.Initialize_Id;
        Sprite_VAO.Bind;
        Array_Buffer.Bind (Points_VBO);
        Set_Vertex_Attrib_Pointer (Attrib_VP, 3, Single_Type, False, 0, 0);
        Enable_Vertex_Attrib_Array (Attrib_VP);

        Array_Buffer.Bind (Tex_VBO);
        Set_Vertex_Attrib_Pointer (Attrib_VT, 2, Single_Type, False, 0, 0);
        Enable_Vertex_Attrib_Array (Attrib_VT);

        Array_Buffer.Bind (Normals_VBO);
        Set_Vertex_Attrib_Pointer (Attrib_VN, 3, Single_Type, False, 0, 0);
        Enable_Vertex_Attrib_Array (Attrib_VN);

    end Create_Character_VBOs;

    --  -------------------------------------------------------------------------

    procedure Init is
    begin
        Game_Utils.Game_Log ("--- Initializing Sprites ---");
        Clear_Sprites;
        Create_Character_VBOs;
        Sprite_Shader_Manager.Load_Sprite_Shaders;
    end Init;

    --  -------------------------------------------------------------------------

    function Get_Sprite_World_Pos (Sprite_Index : Integer) return Singles.Vector3  is
    begin
        return Sprites (Sprite_Index).World_Position;
    end Get_Sprite_World_Pos;

    --  -------------------------------------------------------------------------

    procedure Render_Sprite (Sprite_Index : Integer) is
    begin
        null;
    end Render_Sprite;

    --  -------------------------------------------------------------------------

    procedure Set_Sprite_Current_Sprite
      (Sprite_Index, Current_Sprite : Integer) is
    begin
        Sprites (Sprite_Index).Current_Sprite := Current_Sprite;
    end Set_Sprite_Current_Sprite;

    --  -------------------------------------------------------------------------

    procedure Set_Sprite_Heading (Sprite_Index : Integer;
                                  Heading_Deg : Maths.Degree) is
    begin
        Sprites (Sprite_Index).Heading_Deg := Heading_Deg;
    end Set_Sprite_Heading;

    --  -------------------------------------------------------------------------

    procedure Set_Sprite_Pitch (Sprite_Index : Integer;
                                Pitch_Deg : Maths.Degree) is
    begin
        Sprites (Sprite_Index).Pitch_Deg := Pitch_Deg;
    end Set_Sprite_Pitch;

    --  -------------------------------------------------------------------------

    procedure Set_Sprite_Position (Sprite_Index : Integer;
                                   World_Pos : Singles.Vector3) is
        use Singles;
        use Maths;
	U    : Int := Int (Max (0.5 * (World_Pos (GL.X) + 1.0), 0.0));
	V    : Int := Int (Max (0.5 * (World_Pos (GL.Y) + 1.0), 0.0));
    begin
	U := Min_Int (U, Int (Manifold.Max_Tile_Cols - 1));
	V := Min_Int (V, Int (Manifold.Max_Tile_Cols - 1));

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

    procedure Set_Sprite_Scale (Sprite_Index : Integer;
                                Scale : Singles.Vector3) is
    begin
        Sprites (Sprite_Index).Scale := Scale;
    end Set_Sprite_Scale;

    --  -------------------------------------------------------------------------

    procedure Set_Sprite_Visible (Sprite_Index : Integer; Visible : Boolean) is
    begin
        Sprites (Sprite_Index).Is_Visible := Visible;
    end Set_Sprite_Visible;

    --  -------------------------------------------------------------------------

    procedure Sr_Set_Ambient_Light_Level (Rgb : Singles.Vector3) is
    begin
        Sprite_Shader_Manager.Set_L_A (Rgb);
    end Sr_Set_Ambient_Light_Level;

    --  -------------------------------------------------------------------------

    procedure Start_Sprite_Rendering is
    begin
        null;
    end Start_Sprite_Rendering;

    --  -------------------------------------------------------------------------

    procedure Update_Sprites_Dynamic_Light (Pos_Wor, Diff, Spec : Singles.Vector3;
                                            Light_Range : Float) is
    begin
        Sprite_Shader_Manager.Set_Dyn_Light_Pos (Pos_Wor);
        Sprite_Shader_Manager.Set_Dyn_Light_Diff (Diff);
        Sprite_Shader_Manager.Set_Dyn_Light_Spec (Spec);
        Sprite_Shader_Manager.Set_Dyn_Light_Range (Single (Light_Range));
    end Update_Sprites_Dynamic_Light;

    --  -------------------------------------------------------------------------

    procedure Update_Sprites_Static_Lights_Uniforms is
    begin
        null;
    end Update_Sprites_Static_Lights_Uniforms;

    --  -------------------------------------------------------------------------

end Sprite_Renderer;
