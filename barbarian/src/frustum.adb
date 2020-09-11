
with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;

with Utilities;

with Camera;
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
    Ftl   : Singles.Vector3 := Maths.Vec3_0;
    Ftr   : Singles.Vector3 := Maths.Vec3_0;
    Fbl   : Singles.Vector3 := Maths.Vec3_0;
    Fbr   : Singles.Vector3 := Maths.Vec3_0;
    Ntl   : Singles.Vector3 := Maths.Vec3_0;
    Ntr   : Singles.Vector3 := Maths.Vec3_0;
    Nbl   : Singles.Vector3 := Maths.Vec3_0;
    Nbr   : Singles.Vector3 := Maths.Vec3_0;

    F_Camera_Position : Singles.Vector3 := Maths.Vec3_0;

    Cull_Enabled      : Boolean := True;
    Update_Enabled    : Boolean := True;

    --      Prev_Vp_Size   : Integer := 0;

    Frustum_Wireframe_Shader_Program : GL.Objects.Programs.Program;
    Frustum_Wireframe_VAO            : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Frustum_Solid_VAO                : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Frustum_Wireframe_VBO            : GL.Objects.Buffers.Buffer;
    Frustum_Solid_VBO                : GL.Objects.Buffers.Buffer;

    --  ------------------------------------------------------------------------

    function Is_Sphere_In_Frustum (Centre : Singles.Vector3;  Radius : Single)
                                   return Boolean is
        Out_Of_Planes : Boolean;
        In_Planes     : Boolean := not Frustum_Cull_Enabled;

        function Is_Negative (P1, P2 : Singles.Vector3) return Boolean is
            Distance      : Single := 0.0;
        begin
            Distance := Singles.Dot_Product (P1, Centre) -
              Singles.Dot_Product (P1, P2);
            return Distance + Radius < 0.0;
        end Is_Negative;
    begin
        if not In_Planes then
            Out_Of_Planes := Is_Negative (N_Right, F_Camera_Position) or else
              Is_Negative (N_Left, F_Camera_Position) or else
              Is_Negative (N_Top, F_Camera_Position) or else
              Is_Negative (N_Bottom, F_Camera_Position) or else
              Is_Negative (N_Near, N_Clip_Plane) or else
              Is_Negative (N_Far, F_Clip_Plane) or else
              Is_Negative (N_Left, N_Clip_Plane);
            In_Planes := not Out_Of_Planes;
        end if;
        return In_Planes;

    end Is_Sphere_In_Frustum;

    --  ------------------------------------------------------------------------

    function Is_Aabb_In_Frustum (Mins, Maxs : Singles.Vector3) return Boolean is
    begin
        return False;
    end Is_Aabb_In_Frustum;

    --  ------------------------------------------------------------------------
    --  Init debug variables for frustum wireframe
    procedure Draw_Frustum_Box is
        use GL.Objects.Buffers;
        use GL.Toggles;
        Solid_Points : constant Singles.Vector3_Array (1 .. 36) :=
        --  Front
                         (Ntr, Ntl, Nbl, Nbl, Nbr, Ntr,
                                         --  Rear
                          Ftl, Ftr, Fbr, Fbr, Fbl, Ftl,
                          -- Left
                          Ntl, Ftl, Fbl, Fbl, Nbl, Ntl,
                          --  Right
                          Ftr, Ntr, Nbr, Nbr, Fbr, Ftr,
                          --  Top
                          Ftr, Ftl, Ntl, Ntl, Ntr, Nbl,
                          --  Bottom
                          Fbr, Fbl, Nbl, Nbl, Nbr, Fbr);

        Wire_Frame_Points : constant Singles.Vector3_Array (1 .. 16) :=
        --  Front
                              (Nbl, Ntl, Ntr, Nbr,
                                                   --  Rear
                               Fbr, Ftr, Ftl, Fbl,
                               -- Left
                               Nbl,
                               --  Front
                               Nbr, Fbr,
                               --  Rear
                               Fbl,  Ftl,
                               --  Top
                               Ntl, Ntr, Ftr);
    begin
        if not Frustum_Update_Enabled then
            Array_Buffer.Bind (Frustum_Solid_VBO);
            Utilities.Load_Vertex_Buffer (Array_Buffer, Solid_Points, Dynamic_Draw);

            Array_Buffer.Bind (Frustum_Wireframe_VBO);
            Utilities.Load_Vertex_Buffer (Array_Buffer, Wire_Frame_Points,
                                          Dynamic_Draw);
            Frustum_Shader_Manager.Set_Model_Matrix (Camera.PV_Matrix);
            Frustum_Shader_Manager.Set_Colour ((1.0, 1.0, 1.0, 0.5));

            Frustum_Solid_VAO.Bind;
            Disable (Cull_Face);
            Enable (Blend);
            GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 36);

            Enable (Cull_Face);
            Disable (Blend);
            Frustum_Shader_Manager.Set_Colour ((1.0, 1.0, 1.0, 1.0));
            Frustum_Wireframe_VAO.Bind;
            GL.Objects.Vertex_Arrays.Draw_Arrays (Line_Strip, 0, 16);
        end if;

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
        Set_Vertex_Attrib_Pointer (Attrib_VP, 3, Single_Type, False, 0, 0);
        Enable_Vertex_Attrib_Array (Attrib_VP);

        Frustum_Solid_VAO.Initialize_Id;
        Frustum_Solid_VAO.Bind;

        Frustum_Solid_VBO.Initialize_Id ;
        Array_Buffer.Bind (Frustum_Solid_VBO);
        Allocate (Array_Buffer, 36, Static_Draw);
        Set_Vertex_Attrib_Pointer (Attrib_VP, 3, Single_Type, False, 0, 0);
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
