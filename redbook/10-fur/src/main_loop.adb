
with Interfaces;
with Interfaces.C;

with Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;
--  with GL.Pixels;
with GL.Rasterization;
with GL.Objects.Textures.Targets;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;
--  with Vertex_Data;

with Load_VB_Object;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
    use GL.Types;
    use GL.Uniforms;

    Black                     : constant Colors.Color := (0.0, 0.0, 0.0, 1.0);
    Base_Shader_Program       : GL.Objects.Programs.Program;
    Fur_Shader_Program        : GL.Objects.Programs.Program;
    Vertex_Array              : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Vertex_Buffer             : GL.Objects.Buffers.Buffer;
    Element_Buffer            : GL.Objects.Buffers.Buffer;
    Position_Attribute_ID     : GL.Attributes.Attribute;
    Base_Model_Matrix_ID      : Uniform := -1;
    Base_Projection_Matrix_ID : Uniform := -1;
    Fur_Model_Matrix_ID       : Uniform := -1;
    Fur_Projection_Matrix_ID  : Uniform := -1;
    Fur_Texture_Buffer        : GL.Objects.Buffers.Buffer;
    Fur_Texture               : GL.Objects.Textures.Texture;

    Inner                : Single := 10.0;
    Outer                : Single := 10.0;
    Mode                 : GL.Rasterization.Polygon_Mode_Type :=
                             GL.Rasterization.Line;
    Background           : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 1.0);

    --  ------------------------------------------------------------------------

    --     procedure Process_Keyboard (Window : in out Glfw.Windows.Window) is
    --        use Glfw.Input;
    --        use GL.Rasterization;
    --     begin
    --        if Window'Access.Key_State (Keys.K) = Pressed then
    --           Inner := Inner - 1.0;
    --           if Inner < 1.0 then
    --              Inner := 1.0;
    --           end if;
    --           Set_Single (Inner_Location_ID, Inner);
    --        elsif Window'Access.Key_State (Keys.I) = Pressed then
    --           Inner := Inner + 1.0;
    --           if Inner > 64.0 then
    --              Inner := 64.0;
    --           end if;
    --           Set_Single (Inner_Location_ID, Inner);
    --        elsif Window'Access.Key_State (Keys.L) = Pressed then
    --           Outer := Outer - 1.0;
    --           if Outer < 1.0 then
    --              Outer := 1.0;
    --           end if;
    --           Set_Single (Outer_Location_ID, Outer);
    --        elsif Window'Access.Key_State (Keys.O) = Pressed then
    --           Outer := Outer + 1.0;
    --           if Outer > 64.0 then
    --              Outer := 64.0;
    --           end if;
    --           Set_Single (Outer_Location_ID, Outer);
    --        elsif Window'Access.Key_State (Keys.R) = Pressed then
    --           Inner := 10.0;
    --           Outer := 10.0;
    --           Set_Single (Inner_Location_ID, Inner);
    --           Set_Single (Outer_Location_ID, Outer);
    --        elsif Window'Access.Key_State (Keys.M) = Pressed then
    --           if Mode = Fill then
    --              Mode := Line;
    --           else
    --              Mode := Fill;
    --           end if;
    --           GL.Rasterization.Set_Polygon_Mode (Mode);
    --        end if;
    --
    --     end Process_Keyboard;

    --  -------------------------------------------------------------------------

    procedure Setup is
        use Interfaces;
        use GL.Objects.Buffers;
        use GL.Objects.Programs;
        use GL.Objects.Shaders;
        --        use GL.Types.Singles;
        use Program_Loader;
        --        use GL.Objects.Textures.Targets;
        Vertex_Location       : constant Int := 0;
        Normal_Location       : constant Int := 1;
        Tex_Coord0_Location   : constant Int := 2;
        Tex : Singles.Vector4_Array (1 .. 1024 * 1024);
        X   : Int;
        Y   : Int;
        --        Flat_Array : constant GL.Types.Int_Array (1 .. 16 * Vertex_Data.Num_Teapot_Patches)
        --          := Utilities.Flatten (Vertex_Data.Teapot_Indices);
        VBM_Object  : Load_VB_Object.VB_Object;
        VBM_Result  : Boolean := False;
    begin
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;

        Utilities.Clear_Background_Colour_And_Depth (Background);

        Base_Shader_Program := Program_From
          ((Src ("src/shaders/base.vs", Vertex_Shader),
           Src ("src/shaders/base.fs", Fragment_Shader)));
        GL.Objects.Programs.Use_Program (Base_Shader_Program);

        Base_Model_Matrix_ID := Uniform_Location (Base_Shader_Program, "model_matrix");
        Base_Projection_Matrix_ID := Uniform_Location (Base_Shader_Program, "projection_matrix");

        Fur_Shader_Program := Program_From
          ((Src ("src/shaders/fur.vs", Vertex_Shader),
           Src ("src/shaders/fur.gs", Geometry_Shader),
           Src ("src/shaders/fur.fs", Fragment_Shader)));
        GL.Objects.Programs.Use_Program (Fur_Shader_Program);

        Fur_Model_Matrix_ID := Uniform_Location (Fur_Shader_Program, "model_matrix");
        Fur_Projection_Matrix_ID := Uniform_Location (Fur_Shader_Program, "projection_matrix");
        Fur_Texture_Buffer.Initialize_Id;

        for n in Int range 1 .. 256 loop
            for m in Int range 1 .. 1270 loop
                X := Int (Unsigned_16 (abs (Maths.Random_Float)) and 16#3ff#);
                Y := Int (Unsigned_16 (abs (Maths.Random_Float)) and 16#3ff#);
                Tex (Y * 1024 + X) :=
                  (Single (Unsigned_16 (abs (Maths.Random_Float)) and 16#3f# + 16#c0#),
                   Single (Unsigned_16 (abs (Maths.Random_Float)) and 16#3f# + 16#c0#),
                   Single (Unsigned_16 (abs (Maths.Random_Float)) and 16#3f# + 16#c0#),
                   Single (n));
            end loop;

        end loop;

        GL.Objects.Textures.Targets.Texture_2D.Bind (Fur_Texture);
        --        Allocate (GL.Objects.Buffers.Texture_Buffer, GL.Pixels.RGBA, Fur_Texture_Buffer);
        GL.Objects.Buffers.Texture_Buffer.Bind (Fur_Texture_Buffer);
        Utilities.Load_Vertex_Buffer (Texture_Buffer, Tex, Dynamic_Draw);

        GL.Objects.Textures.Targets.Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
        GL.Objects.Textures.Targets.Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);

        Load_VB_Object.Load_From_VBM ("../media/ninja.vbm", VBM_Object,
                                      Vertex_Location, Normal_Location,
                                      Tex_Coord0_Location, VBM_Result);

--          Vertex_Buffer.Initialize_Id;
--          Array_Buffer.Bind (Vertex_Buffer);
--          Utilities.Load_Vertex_Buffer (Array_Buffer, Vertex_Data.Teapot_Vertices,
--                                        Static_Draw);
--
--          GL.Attributes.Enable_Vertex_Attrib_Array (Position_Attribute_ID);
--          GL.Attributes.Set_Vertex_Attrib_Pointer (Position_Attribute_ID, 3,
--                                                   Single_Type, True, 0, 0);

    exception
        when others =>
            Put_Line ("An exception occurred in Main_Loop.Setup.");
            raise;
    end Setup;

    --  ------------------------------------------------------------------------

    procedure Display (Main_Window : in out Glfw.Windows.Window) is
        use Interfaces.C;
        use Ada.Numerics;
        Use Maths;
        use Single_Math_Functions;
        use GL.Objects.Buffers;
        use GL.Types.Singles;
        T                 : Single := Single (Glfw.Time);
        Window_Width      : Glfw.Size;
        Window_Height     : Glfw.Size;
        Aspect            : Single;
        X                 : Singles.Vector3 := (1.0, 0.0, 0.0);
        Y                 : Singles.Vector3 := (0.0, 1.0, 0.0);
        Z                 : Singles.Vector3 := (0.0, 0.0, 1.0);
        Model_Matrix      : Matrix4 := Identity4;
        Projection_Matrix : Matrix4;
    begin

        Utilities.Clear_Background_Colour_And_Depth (Black);
        Main_Window.Get_Framebuffer_Size (Window_Width, Window_Height);
        --        Process_Keyboard (Main_Window);
        GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                                GL.Types.Int (Window_Height));
        Aspect := Single (Window_Height / Window_Width);
        Model_Matrix := Translation_Matrix
          ((0.0, 0.0, 100.0 * Sin (2.0 * Pi * T) - 130.0)) *
          Rotation_Matrix (Degree (360.0 * T), (0.0, 1.0, 0.0)) *
          Rotation_Matrix (Degree (180.0), (0.0, 0.0, 1.0)) *
          Translation_Matrix ((0.0, -80.0, 0.0)) * Model_Matrix;

        Projection_Matrix := Perspective_Matrix
          (Top    => -1.0, Bottom => 1.0, Left   => Aspect, Right  => -Aspect,
           Near   => 1.0, Far    => 5000.0);

        GL.Objects.Programs.Use_Program (Base_Shader_Program);
        GL.Uniforms.Set_Single (Base_Projection_Matrix_ID, Projection_Matrix);
        GL.Uniforms.Set_Single (Base_Model_Matrix_ID, Model_Matrix);

        --        Draw_Elements (Patches, Vertex_Data.Num_Teapot_Vertices, UInt_Type);

    exception
        when  others =>
            Put_Line ("An exception occurred in Main_Loop.Display.");
            raise;
    end Display;

    --  ------------------------------------------------------------------------

    use Glfw.Input;   Running : Boolean := True;
begin
    Setup;
    while Running loop
        Display (Main_Window);
        Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        Glfw.Input.Poll_Events;
        Running := Running and not
          (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
        Running := Running and not Main_Window.Should_Close;
    end loop;

exception
    when others =>
        Put_Line ("An exception occurred in Main_Loop.");
        raise;
end Main_Loop;
