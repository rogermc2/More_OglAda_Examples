
with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;

with Frustum_Shader_Manager;
with Shader_Attributes;

package body Frustum is
--  All normals
    N_Right     : Singles.Vector3 := Maths.Vec3_0;
    N_Left      : Singles.Vector3 := Maths.Vec3_0;
    N_Top       : Singles.Vector3 := Maths.Vec3_0;
    N_Bottom    : Singles.Vector3 := Maths.Vec3_0;
    N_Near      : Singles.Vector3 := Maths.Vec3_0;
    N_Far       : Singles.Vector3 := Maths.Vec3_0;
    --  Clip planes
    F_Clip_Plane : Singles.Vector3 := Maths.Vec3_0;
    N_Clip_Plane : Singles.Vector3 := Maths.Vec3_0;
    --  Get 8 corner points
    Ftl_Corner   : Singles.Vector3 := Maths.Vec3_0;
    Ftr_Corner   : Singles.Vector3 := Maths.Vec3_0;
    Fbl_Corner   : Singles.Vector3 := Maths.Vec3_0;
    Fbr_Corner   : Singles.Vector3 := Maths.Vec3_0;
    Ntl_Corner   : Singles.Vector3 := Maths.Vec3_0;
    Ntr_Corner   : Singles.Vector3 := Maths.Vec3_0;
    Nbl_Corner   : Singles.Vector3 := Maths.Vec3_0;
    Nbr_Corner   : Singles.Vector3 := Maths.Vec3_0;

    F_Cam_Pos    : Singles.Vector3 := Maths.Vec3_0;

    Cull_Enabled   : Boolean := True;
    Update_Enabled : Boolean := True;

    Frustum_Wireframe_Shader_Program : GL.Objects.Programs.Program;
    Frustum_Wireframe_VAO            : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Frustum_Solid_VAO                : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Frustum_Wireframe_VBO            : GL.Objects.Buffers.Buffer;
    Frustum_Solid_VBO                : GL.Objects.Buffers.Buffer;

    --  ------------------------------------------------------------------------

    function Is_Sphere_In_Frustum (Centre : Singles.Vector3;  Radius : Single)
                                   return Boolean is
    begin
        return False;
    end Is_Sphere_In_Frustum;

    --  ------------------------------------------------------------------------

    function Is_Aabb_In_Frustum (Mins, Maxs : Singles.Vector3) return Boolean is
    begin
        return False;
    end Is_Aabb_In_Frustum;

    --  ------------------------------------------------------------------------
    --  Init debug variables for frustum wireframe
    procedure Draw_Frustum_Box is
    begin
        null;
    end Draw_Frustum_Box;

    --  ------------------------------------------------------------------------
    --  Set to false to stop frustum culling alogether for testing
    function Frustum_Cull_Enabled return Boolean is
    begin
        return Cull_Enabled;
    end Frustum_Cull_Enabled;

    --  ------------------------------------------------------------------------

    procedure Init is
        use GL.Attributes;
        use GL.Objects.Buffers;
        use GL.Objects.Vertex_Arrays;
        use Shader_Attributes;
    begin
    Frustum_Wireframe_VAO.Initialize_Id;
    Frustum_Wireframe_VAO.Bind;

    Frustum_Wireframe_VBO.Initialize_Id;
    Array_Buffer.Bind (Frustum_Wireframe_VBO);
    Allocate (Array_Buffer, 16, Static_Draw);
    Set_Vertex_Attrib_Pointer (Attrib_VP, 3, Single_Type, 0, 0);
    Enable_Vertex_Attrib_Array (Attrib_VP);

    Frustum_Solid_VAO.Initialize_Id;
    Frustum_Solid_VAO.Bind;

    Frustum_Solid_VBO.Initialize_Id ;
    Array_Buffer.Bind (Frustum_Solid_VBO);
    Allocate (Array_Buffer, 36, Static_Draw);
    Set_Vertex_Attrib_Pointer (Attrib_VP, 3, Single_Type, 0, 0);
    Enable_Vertex_Attrib_Array (Attrib_VP);

    Frustum_Shader_Manager.Init (Frustum_Wireframe_Shader_Program);

    end Init;

    --  ------------------------------------------------------------------------

    --  Set To False To Stop Frustum Plane Extraction
    function Frustum_Update_Enabled return Boolean is
    begin
        return Update_Enabled;
    end Frustum_Update_Enabled;

    --  ------------------------------------------------------------------------

    procedure Re_Extract_Frustum_Planes
      (Fovy_Deg : Maths.Degree; Aspect, Near, Far : Single;
       Cam_Pos : Singles.Vector3; V : Singles.Matrix4) is
    begin
        null;
    end Re_Extract_Frustum_Planes;

    --  ------------------------------------------------------------------------

end Frustum;
