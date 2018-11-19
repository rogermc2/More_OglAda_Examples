

with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with GL.Objects.Shaders.Lists;

with Maths;
with Program_Loader;

Package body Shadow_Map_Technique is

      function Get_Uniform_Location (theTechnique : Technique; Uniform_Name : String)
                                   return GL.Uniforms.Uniform is
    begin
        return GL.Objects.Programs.Uniform_Location (Light_Program (theTechnique), Uniform_Name);
    end Get_Uniform_Location;

    --  -------------------------------------------------------------------------

    procedure Init (theTechnique : out Technique) is
        use Program_Loader;
        use  GL.Objects.Shaders;
    begin
        theTechnique.Lighting_Program := Program_From
          ((Src ("src/shaders/shadow_map.vs", Vertex_Shader),
           Src ("src/shaders/shadow_map.fs", Fragment_Shader)));

        GL.Objects.Programs.Use_Program  (theTechnique.Lighting_Program);
        theTechnique.WVP_Location := GL.Objects.Programs.Uniform_Location
          (theTechnique.Lighting_Program, "gWVP");
        theTechnique.Texture_Location := GL.Objects.Programs.Uniform_Location
          (theTechnique.Lighting_Program, "gShadowMap");

    end Init;

    --  -------------------------------------------------------------------------

    function Light_Program (theTechnique : Technique)
                            return GL.Objects.Programs.Program is
    begin
        return theTechnique.Lighting_Program;
    end Light_Program;

    --  -------------------------------------------------------------------------

    procedure Set_Shadow_Map_Texture_Unit (theTechnique : Technique;
                                           Texture_Unit : GL.Types.Int) is
    begin
        GL.Objects.Programs.Use_Program (theTechnique.Lighting_Program);
        GL.Uniforms.Set_Int (theTechnique.Texture_Location, Texture_Unit);
    end Set_Shadow_Map_Texture_Unit;

   --  -------------------------------------------------------------------------

   procedure Set_WVP (theTechnique : Technique;
                      WVP : GL.Types.Singles.Matrix4) is
    begin
        GL.Objects.Programs.Use_Program (theTechnique.Lighting_Program);
        GL.Uniforms.Set_Single (theTechnique.WVP_Location, WVP);
    end Set_WVP;

    --  -------------------------------------------------------------------------

    procedure Use_Program (theTechnique : Technique) is
        use GL.Objects.Programs;
        use GL.Objects.Shaders.Lists;
    begin
        if GL.Objects.Programs.Validate_Status (theTechnique.Lighting_Program) then
               declare
                Shaders_List : GL.Objects.Shaders.Lists.List :=
                                 GL.Objects.Programs.Attached_Shaders (theTechnique.Lighting_Program);
                Curs         : GL.Objects.Shaders.Lists.Cursor := Shaders_List.First;
            begin
                if Curs = GL.Objects.Shaders.Lists.No_Element then
                    Put_Line ("Shadow_Map_Technique.Use_Program, Shaders list is empty");
                else
                    GL.Objects.Programs.Use_Program (theTechnique.Lighting_Program);
                end if;
            end;  -- declare block
        end if;

    exception
        when  others =>
            Put_Line ("An exception occurred in Shadow_Map_Technique.Use_Program.");
            raise;
    end Use_Program;

    --  -------------------------------------------------------------------------

end Shadow_Map_Technique;
