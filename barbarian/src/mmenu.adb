
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types.Colors;

with Glfw;

with Maths;
with Utilities;

with Camera;
with Cursor_Shader_Manager;
with GL_Utils;
with Shader_Attributes;
with Title_Shader_Manager;

package body MMenu is

    Black                 : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 1.0);
    Cursor_VAO            : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Title_VAO             : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Num_Mmenu_Entries     : constant Integer := 7;
--      Num_Gra_Entries   : constant Integer := 17;
--      Num_Aud_Entries   : constant Integer := 3;
--      Num_Inp_Entries   : constant Integer := 4;
--      Num_Con_Entries   : constant Integer := 2;

    Mmenu_Credits_Open     : Boolean := False;
    Mmenu_End_Story_Open   : Boolean := False;
    User_Chose_Custom_Maps : Boolean := False;
    User_Chose_New_Game    : Boolean := False;
    We_Are_In_Custom_Maps  : Boolean := False;
    Title_Author_Text      : Unbounded_String := To_Unbounded_String ("");
    Title_Skull_Texture    : GL.Objects.Textures.Texture;

    Mmenu_Cursor_Curr_Item : Integer := -1;
    Cursor_Point_Count     : GL.Types.Int := 0;
    Title_Point_Count     : GL.Types.Int := 0;
    Title_M                : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
    Title_V                : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
    Cursor_M               : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
    Cursor_V               : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
    Title_Bounce_Timer     : Float := 5.0;
    Text_Timer             : Float := 0.0;

    --  ------------------------------------------------------------------------

    function Are_We_In_Custom_Maps return Boolean is
    begin
        return We_Are_In_Custom_Maps;
    end Are_We_In_Custom_Maps;

    --  ------------------------------------------------------------------------

    function Did_User_Choose_Custom_Maps return Boolean is
    begin
        return User_Chose_Custom_Maps;
    end Did_User_Choose_Custom_Maps;

    --  ------------------------------------------------------------------------

    function Did_User_Choose_New_Game return Boolean is
    begin
        return User_Chose_New_Game;
    end Did_User_Choose_New_Game;

    --  ------------------------------------------------------------------------

    procedure Draw_Menu (Elapsed : Float) is
        use GL.Toggles;
    begin
        if Mmenu_Cursor_Curr_Item < 0 then
           Mmenu_Cursor_Curr_Item := Num_Mmenu_Entries - 1;
        end if;
        if Mmenu_Cursor_Curr_Item >= Num_Mmenu_Entries then
           Mmenu_Cursor_Curr_Item := 0;
        end if;
        Utilities.Clear_Depth;
        if Mmenu_Credits_Open then
            Utilities.Clear_Background_Colour_And_Depth (Black);
            Disable (Depth_Test);
        end if;
        Text_Timer := Text_Timer + Elapsed;
        Enable (Depth_Test);
    end Draw_Menu;

    --  ------------------------------------------------------------------------

    procedure Draw_Title_Only is
        use GL.Types;
        use Singles;
        use Maths;
        S_Matrix     : constant Singles.Matrix4 := Scaling_Matrix (10.0);
        T_Matrix     : constant Singles.Matrix4 :=
                         Translation_Matrix ((0.0, -10.0, -30.0));
        M_Matrix     : constant Singles.Matrix4 := T_Matrix * S_Matrix;
        Title_Matrix : constant Singles.Matrix4 :=
                         Translation_Matrix ((-0.4, -3.0, -1.0));
        Current_Time : constant Single := Single (Glfw.Time);
    begin
        --  Draw cursor skull in background
        GL.Objects.Textures.Targets.Texture_2D.Bind (Title_Skull_Texture);
        Cursor_VAO.Initialize_Id;
        Cursor_VAO.Bind;
        Cursor_Shader_Manager.Set_Perspective_Matrix (Camera.Projection_Matrix);
        Cursor_Shader_Manager.Set_View_Matrix (Cursor_V);
        Cursor_Shader_Manager.Set_Model_Matrix (M_Matrix);
        GL_Utils.Draw_Triangles (Cursor_Point_Count);

        --  3D title
        Title_Shader_Manager.Set_View_Matrix (Title_V);
        Title_Shader_Manager.Set_Model_Matrix (Title_Matrix);
        Title_Shader_Manager.Set_Perspective_Matrix (Camera.Projection_Matrix);
        Title_Shader_Manager.Set_Time (Current_Time);
        Title_VAO.Initialize_Id;
        Title_VAO.Bind;
        GL_Utils.Draw_Triangles (Title_Point_Count);

        --  Draw library logos and stuff
        --  Later
    end Draw_Title_Only;

    --  ------------------------------------------------------------------------

    function End_Story_Open return Boolean is
    begin
        return Mmenu_End_Story_Open;
    end End_Story_Open;

    --  ------------------------------------------------------------------------

    procedure Init_MMenu is
        use GL.Objects.Buffers;
        use GL.Types;
        Vertex_Array    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        Vertex_Buffer   : Buffer;
        Texture_Buffer  : Buffer;
        Position_Array  : constant Singles.Vector2_Array (1 .. 6) :=
        ((-1.0, 1.0), (-1.0, -1.0),  (1.0, -1.0),
         (1.0, -1.0), (1.0, 1.0), (-1.0, 1.0));
        Texture_Array   : Singles.Vector2_Array (1 .. 6) :=
        ((0.0, 1.0), (0.0, 0.0),  (1.0, 0.0),
         (1.0, 0.0), (1.0, 1.0), (0.0, 1.0));
         Title_Mesh      : Integer := 0;
         Cursor_Mesh     : Integer := 0;
    begin
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;
        Vertex_Buffer.Initialize_Id;
        Texture_Buffer.Initialize_Id;

        GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VP);
        Array_Buffer.Bind (Vertex_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer
          (Shader_Attributes.Attrib_VP, 2, Single_Type, False, 0, 0);

        GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VT);
        Array_Buffer.Bind (Vertex_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer
          (Shader_Attributes.Attrib_VT, 2, Single_Type, False, 0, 0);

    end Init_MMenu;

    --  ------------------------------------------------------------------------

    procedure Start_Mmenu_Title_Bounce is
    begin
        Title_Bounce_Timer := 0.0;
    end Start_Mmenu_Title_Bounce;

    --  ------------------------------------------------------------------------

    function Update_MMenu (Delta_Time : Float) return Boolean is
    begin
        return False;
    end Update_MMenu;

    --  ------------------------------------------------------------------------

end MMenu;