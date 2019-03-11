

with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with GL.Objects.Shaders.Lists;

with Program_Loader;

Package body Billboard_Technique is

    function Billboard_Program (theTechnique : Technique)
                               return GL.Objects.Programs.Program is
    begin
        return theTechnique.Program;
    end Billboard_Program;

    --  -------------------------------------------------------------------------

    procedure Init (theTechnique : out Technique) is
        use Program_Loader;
        use  GL.Objects.Shaders;
    begin
        theTechnique.Program := Program_From
          ((Src ("src/shaders/billboard.vs", Vertex_Shader),
           Src ("src/shaders/billboard.fs", Fragment_Shader),
           Src ("src/shaders/billboard.gs", Geometry_Shader)));

        GL.Objects.Programs.Use_Program  (theTechnique.Program);
        theTechnique.View_Point_Location := GL.Objects.Programs.Uniform_Location
          (theTechnique.Program, "gVP");
        theTechnique.Camera_Position_Location := GL.Objects.Programs.Uniform_Location
          (theTechnique.Program, "gCameraPos");
        theTechnique.Billboard_Size_Location := GL.Objects.Programs.Uniform_Location
          (theTechnique.Program, "gBillboardSize");
        theTechnique.Colour_Map_Location := GL.Objects.Programs.Uniform_Location
          (theTechnique.Program, "gColorMap");
    end Init;

    --  -------------------------------------------------------------------------

    procedure Set_Billboard_Size (theTechnique : Technique;
                                  Size : GL.Types.Single) is
    begin
        GL.Objects.Programs.Use_Program (theTechnique.Program);
        GL.Uniforms.Set_Single (theTechnique.Billboard_Size_Location, Size);
    end Set_Billboard_Size;

    --  -------------------------------------------------------------------------

    procedure Set_Camera_Position (theTechnique : Technique;
                                   Position : GL.Types.Singles.Vector3) is
    begin
        GL.Objects.Programs.Use_Program (theTechnique.Program);
        GL.Uniforms.Set_Single (theTechnique.Camera_Position_Location, Position);
    end Set_Camera_Position;

    --  -------------------------------------------------------------------------

    procedure Set_Colour_Texture_Unit (theTechnique : Technique;
                                       Texture_Unit : GL.Types.Int) is
    begin
        GL.Objects.Programs.Use_Program (theTechnique.Program);
        GL.Uniforms.Set_Int (theTechnique.Colour_Map_Location, Texture_Unit);
    end Set_Colour_Texture_Unit;

    --  -------------------------------------------------------------------------

    procedure Set_View_Point (theTechnique : Technique;
                              View_Point : GL.Types.Singles.Matrix4) is
    begin
        GL.Objects.Programs.Use_Program (theTechnique.Program);
        GL.Uniforms.Set_Single (theTechnique.View_Point_Location, View_Point);
    end Set_View_Point;

    --  -------------------------------------------------------------------------

    procedure Use_Program (theTechnique : Technique) is
        use GL.Objects.Programs;
        use GL.Objects.Shaders.Lists;
    begin
--          if not GL.Objects.Programs.Validate_Status (theTechnique.Program) then
--              Put_Line ("Billboard_Technique.Use_Program Update_Program validation failed.");
--          else
--              Put_Line ("Billboard_Technique.Use_Program Update_Program validated.");
            declare
                Shaders_List : GL.Objects.Shaders.Lists.List :=
                                 GL.Objects.Programs.Attached_Shaders (theTechnique.Program);
                Curs         : GL.Objects.Shaders.Lists.Cursor := Shaders_List.First;
            begin
                if Curs = GL.Objects.Shaders.Lists.No_Element then
                    Put_Line ("Billboard_Technique.Use_Program, Shaders list is empty");
                else
                    GL.Objects.Programs.Use_Program (theTechnique.Program);
                end if;
            end;  -- declare block
--          end if;

    exception
        when  others =>
            Put_Line ("An exception occurred in Billboard_Technique.Use_Program.");
            raise;
    end Use_Program;

    --  -------------------------------------------------------------------------

end Billboard_Technique;
