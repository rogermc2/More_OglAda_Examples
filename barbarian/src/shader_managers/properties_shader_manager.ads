
with GL.Objects.Programs;
with GL.Types; use GL.Types;

package Properties_Shader_Manager is
   use GL.Objects.Programs;

   type Light_Array is new Singles.Vector3_Array (1 .. 32);
   type Light_Range_Array is new Single_Array (1 .. 32);

   Prop_Shader            : Program;  --  Basic shader
   Prop_Skinned_Shader    : Program;  --  Skinned shader
   Coins_Shader           : Program;  --  Shiny treasure shader
   Jav_Stand_Shader       : Program;  --  Pulsing 'look at me' lighting shader
   Portal_Shader          : Program;  --  Wobbly portal shader
   Bounding_Sphere_Shader : Program;  --  Bounding sphere shader

   procedure Load_Prop_Shaders;

end Properties_Shader_Manager;
