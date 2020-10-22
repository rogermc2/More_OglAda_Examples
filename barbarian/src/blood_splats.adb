
with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;
with GL.Types;

with Game_Utils;
with Settings;
with Shader_Attributes;
with Splats_Shader_Manager;
with Texture_Manager;

package body Blood_Splats is
   use GL.Types;
   Num_Splats_In_Play   : Integer := 0;
   Next_Splat_Mem_Index : Integer := 0;
   Max_Splats           : constant Int := 256;
   Splat_Vao            : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Splat_Buffer         : GL.Objects.Buffers.Buffer;
   Splat_Normals_Vbo    : GL.Objects.Buffers.Buffer;
   Splat_Sp             : GL.Objects.Programs.Program;
   Blood_Splats_Tex     : GL.Objects.Textures.Texture;

   Bsb_Sz   : Single_Array (1 .. 18 * Max_Splats) := (others => 0.0);
   Bsbn_Sz  : Single_Array (1 .. 18 * Max_Splats) := (others => 0.0);
   Bsbt_Sz  : Single_Array (1 .. 12 * Max_Splats) := (others => 0.0);

   procedure Init is
      use GL.Attributes;
      use GL.Objects.Buffers;
      use Shader_Attributes;
      VT_VBO  : Buffer;
      VT_12   : constant Single_Array (1 .. 12) :=
                  (1.0, 1.0, 0.0, 1.0, 0.0, 0.0,
                   0.0, 0.0, 1.0, 0.0,1.0, 1.0);

      VT      : Single_Array (1 .. 12 * Max_Splats) := (others => 0.0);
      Mipmaps : constant Boolean := True;
      SRGB    : constant Boolean := True;
      Index   : Int := 0;
    begin
      Game_Utils.Game_Log ("----INIT BLOOD SPLATS---");
      Texture_Manager.Load_Image_To_Texture
        ("src/textures/splat.png", Blood_Splats_Tex, Mipmaps, SRGB);
      Splat_Vao.Initialize_Id;
      Splat_Vao.Bind;

      for splati in 1 .. Max_Splats loop
         VT (splati .. splati + 11) := VT_12;
      end Loop;

      Splat_Buffer.Initialize_Id;
      Array_Buffer.Bind (Splat_Buffer);
      Enable_Vertex_Attrib_Array (Attrib_VP);
      Set_Vertex_Attrib_Pointer (Attrib_VP, 3, Single_Type, False, 0, 0);

      VT_VBO.Initialize_Id;
      Array_Buffer.Bind (VT_VBO);
      Enable_Vertex_Attrib_Array (Attrib_VT);
      Set_Vertex_Attrib_Pointer (Attrib_VT, 2, Single_Type, False, 0, 0);

      Splat_Normals_Vbo.Initialize_Id;
      Array_Buffer.Bind (Splat_Normals_Vbo);
      Enable_Vertex_Attrib_Array (Attrib_VN);
      Set_Vertex_Attrib_Pointer (Attrib_VN, 3, Single_Type, False, 0, 0);

      Splats_Shader_Manager.Init (Splat_Sp);
      Splats_Shader_Manager.Set_Cube_Texture (1);
      if Settings.Shadows_Enabled then
         Splats_Shader_Manager.Set_Shadow_Enabled (1.0);
      else
         Splats_Shader_Manager.Set_Shadow_Enabled (0.0);
      end if;
      Game_Utils.Game_Log ("----BLOOD SPLATS INITIALIZED---");

    end Init;

end Blood_Splats;
