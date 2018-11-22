
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Low_Level.Enums;
with GL.Objects;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

with Ogldev_Camera;
with Ogldev_Math;
with Ogldev_Pipeline;
with Ogldev_Texture;

with Buffers;
with Lighting_Technique_21;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
    use GL.Types;

    type vertex is record
        Pos  : GL.Types.Singles.Vector3;
        Tex  : GL.Types.Singles.Vector2;
        Norm : GL.Types.Singles.Vector3;
    end record;

    Background             : constant GL.Types.Colors.Color := (0.4, 0.4, 0.4, 0.0);
    Field_Depth            : constant := 20.0;
    Field_Width            : constant := 10.0;

    Shader_Technique       : Lighting_Technique_21.Technique;

    VAO                    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Vertex_Buffer          : GL.Objects.Buffers.Buffer;
    Game_Camera            : Ogldev_Camera.Camera;
    theTexture             : Ogldev_Texture.Ogl_Texture;
    Direct_Light           : Lighting_Technique_21.Directional_Light;
    Perspective_Proj_Info  : Ogldev_Math.Perspective_Projection_Info;
    Scale                  : Single := 0.0;

    --  ------------------------------------------------------------------------

    procedure Init (Window : in out Glfw.Windows.Window; Result : out Boolean) is

        Window_Width        : Glfw.Size;
        Window_Height       : Glfw.Size;
        Position            : constant Singles.Vector3 := (5.0, 1.0, -3.0); --  Normalized by Camera.Init
        Target              : constant Singles.Vector3 := (0.0, 0.0, 1.0);  --  Normalized by Camera.Init
        Up                  : constant Singles.Vector3 := (0.0, 1.0, 0.0);
    begin
        Result := Lighting_Technique_21.Init (Shader_Technique);
        if Result then
            VAO.Initialize_Id;
            VAO.Bind;

            Window.Get_Framebuffer_Size (Window_Width, Window_Height);
            Lighting_Technique_21.Init_Directional_Light (Direct_Light);
            Ogldev_Math.Set_Perspective_FOV (Perspective_Proj_Info, 60.0);
            Ogldev_Math.Set_Perspective_Height (Perspective_Proj_Info, GL.Types.UInt (Window_Height));
            Ogldev_Math.Set_Perspective_Width (Perspective_Proj_Info, GL.Types.UInt (Window_Width));
            Ogldev_Math.Set_Perspective_Near (Perspective_Proj_Info, 1.0);
            Ogldev_Math.Set_Perspective_Far (Perspective_Proj_Info, 50.0);

            Ogldev_Camera.Init_Camera (Game_Camera, Int (Window_Width), Int (Window_Height),
                                       Position, Target, Up);
            Buffers.Create_Vertex_Buffer (Vertex_Buffer);

            Lighting_Technique_21.Use_Program (Shader_Technique);
            Lighting_Technique_21.Set_Texture_Unit (Shader_Technique, 0);

            Result := Ogldev_Texture.Init_Texture (theTexture, GL.Low_Level.Enums.Texture_2D,
                                                   "../content/test.png");
            if Result then
                Ogldev_Texture.Load (theTexture);
            else
                Put_Line ("Main_Loop.Init. Init_Texture failed");
            end if;

            Glfw.Input.Poll_Events;
        end if;

    exception
        when others =>
            Put_Line ("An exception occurred in Main_Loop.Init.");
            raise;
    end Init;

    --  ------------------------------------------------------------------------

    procedure Update_Lighting (Window : in out Glfw.Windows.Window) is
        use Glfw.Input;
        use Lighting_Technique_21;
    begin
              if Window'Access.Key_State (Keys.A) = Pressed then
                 Set_Directional_Ambient (Direct_Light,
                                          Get_Directional_Ambient (Direct_Light) + 0.05);
              elsif Window'Access.Key_State (Keys.S) = Pressed then
                 Set_Directional_Ambient (Direct_Light,
                                          Get_Directional_Ambient (Direct_Light) - 0.05);
              elsif Window'Access.Key_State (Keys.Z) = Pressed then
                 Set_Directional_Diffuse (Direct_Light,
                                          Get_Directional_Diffuse (Direct_Light) + 0.05);
              elsif Window'Access.Key_State (Keys.X) = Pressed then
                 Set_Directional_Diffuse (Direct_Light,
                                          Get_Directional_Diffuse (Direct_Light) - 0.05);
              end if;
    end Update_Lighting;

    --  -------------------------------------------------------------------------

    procedure Render_Scene (Window : in out Glfw.Windows.Window) is
        use Maths.Single_Math_Functions;
        Window_Width         : Glfw.Size;
        Window_Height        : Glfw.Size;
        World_Transformation : GL.Types.Singles.Matrix4;
        Pipe                 : Ogldev_Pipeline.Pipeline;
        Point_Lights         : Lighting_Technique_21.Point_Lights_Array (1 .. 2);
        Spot_Lights          : Lighting_Technique_21.Spot_Lights_Array (1 .. 2);
    begin
        Scale := Scale + 0.0057;
        Update_Lighting (Window);
        Ogldev_Camera.Update_Camera (Game_Camera, Window);
        Utilities.Clear_Background_Colour (Background);

        Lighting_Technique_21.Set_Point_Light (Light   => Point_Lights (1),
                                               Diffuse =>  0.25,
                                               Colour  => (1.0, 0.5, 0.0),
                                               Pos     => (3.0, 1.0, 0.5 * Field_Depth * (1.0 + Cos (Scale))),
                                               Atten   => (0.1, 0.0, 0.0));
        Lighting_Technique_21.Set_Point_Light (Point_Lights (2), 0.25, (0.0, 0.5, 1.0),
                                               (7.0, 1.0, 0.5 * Field_Depth * (1.0 + Sin (Scale))),
                                               (0.1, 0.0, 0.0));
        Lighting_Technique_21.Set_Point_Light_Locations (Shader_Technique, Point_Lights);

        Lighting_Technique_21.Set_Spot_Light (Light     => Spot_Lights (1),
                                              Diffuse   => 0.9,
                                              Colour    => (0.0, 1.0, 1.0),
                                              Pos       => Ogldev_Camera.Get_Position (Game_Camera),
                                              Direction => Ogldev_Camera.Get_Target (Game_Camera),
                                              Atten     => (0.1, 0.0, 0.0),
                                              Cut_Off   => 10.0);
        Lighting_Technique_21.Set_Spot_Light (Spot_Lights (2), 0.9, (1.0, 1.0, 1.0), (5.0, 3.0, 10.0),
                                              (0.0, -1.0, 0.0), (0.1, 0.0, 0.0), 20.0);
        Lighting_Technique_21.Set_Spot_Light_Locations (Shader_Technique, Spot_Lights);

        Window.Get_Framebuffer_Size (Window_Width, Window_Height);
        GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                                GL.Types.Int (Window_Height));

        Ogldev_Pipeline.Set_World_Position (Pipe, 0.0, 0.0, 1.0);
        Ogldev_Pipeline.Set_Camera (Pipe, Ogldev_Camera.Get_Position (Game_Camera),
                                    Ogldev_Camera.Get_Target (Game_Camera),
                                    Ogldev_Camera.Get_Up (Game_Camera));
        Ogldev_Pipeline.Set_Perspective_Info (Pipe, Perspective_Proj_Info);

        Lighting_Technique_21.Set_WVP (Shader_Technique,
                                       Ogldev_Pipeline.Get_WVP_Transform (Pipe));
        World_Transformation := Ogldev_Pipeline.Get_World_Transform (Pipe);
        Put_Line ("AMain_Loop.Render_Scene. World_Transformation set");

        Ogldev_Pipeline.Set_Rotation (Pipe, 0.0, Scale, 0.0);
        Ogldev_Pipeline.Set_Perspective_Info (Pipe, Perspective_Proj_Info);
        Ogldev_Pipeline.Init_Transforms (Pipe);

        Lighting_Technique_21.Set_World_Matrix (Shader_Technique, World_Transformation);
        Lighting_Technique_21.Set_Directional_Light (Shader_Technique, Direct_Light);
        Lighting_Technique_21.Set_Eye_World_Pos (Shader_Technique,
                                                 Ogldev_Camera.Get_Position (Game_Camera));
        Lighting_Technique_21.Set_Mat_Specular_Intensity (Shader_Technique, 1.0);
        Lighting_Technique_21.Set_Mat_Specular_Power (Shader_Technique, 32.0);

        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        GL.Attributes.Enable_Vertex_Attrib_Array (1);
        GL.Attributes.Enable_Vertex_Attrib_Array (2);

        --  First attribute buffer : Vertices
        GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 8, 0);
        --  Second attribute buffer : Textures
        GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, Single_Type, 8, 3);
        --  Third attribute buffer : Normals
        GL.Attributes.Set_Vertex_Attrib_Pointer (2, 3, Single_Type, 8, 5);

        GL.Objects.Textures.Set_Active_Unit (0);
        GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 6);

        GL.Attributes.Disable_Vertex_Attrib_Array (0);
        GL.Attributes.Disable_Vertex_Attrib_Array (1);
        GL.Attributes.Disable_Vertex_Attrib_Array (2);

    exception
        when  others =>
            Put_Line ("An exception occurred in Main_Loop.Render_Scene.");
            raise;
    end Render_Scene;

    --  ------------------------------------------------------------------------

    use Glfw.Input;
    Running : Boolean;
begin
    Init (Main_Window, Running);
    while Running loop
        Render_Scene (Main_Window);
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
