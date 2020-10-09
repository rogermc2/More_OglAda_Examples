
with GL.Objects.Shaders;

with Program_Loader;

with Shader_Attributes;

package body Shader_Manager is

   package Shaders_Package is new
     Ada.Containers.Vectors (Positive, Program);
   type Shaders_List is new Shaders_Package.Vector with null Record;

   Fallback_Shaders  : Shaders_List;
   Fallback_Uniforms : Shader_Uniforms_List;

   procedure Init_Fallback;

   --  -------------------------------------------------------------------------

   procedure Init is
   begin
      Init_Fallback;
   end Init;

   --  -------------------------------------------------------------------------

   procedure Init_Fallback is
      use GL.Objects.Shaders;
      use Program_Loader;
      SP      : Program;
      SP_Data : Shader_Data;
      begin
         SP:= Program_From
           ((Src ("src/shaders_3_2/fallback_410.vert", Vertex_Shader),
            Src ("src/shaders_3_2/fallback_410.frag", Fragment_Shader)));

         Bind_Attrib_Location (SP, Shader_Attributes.Attrib_VP, "vp");

         Fallback_Shaders.Append (SP);

         SP_Data.Sp := Uniform_Location (SP, "P");
         SP_Data.Vs := Uniform_Location (SP, "V");
         SP_Data.Fs :=  Uniform_Location (SP, "M");

      end Init_Fallback;

      --  -------------------------------------------------------------------------

   end Shader_Manager;
