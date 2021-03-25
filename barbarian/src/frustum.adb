
with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;

with Utilities;

with Camera;
with Frustum_Shader_Manager;
with Game_Utils;
with GL_Maths;
with GL_Utils;
with Shader_Attributes;

package body Frustum is

    --  All normals
    Norm_Right        : Singles.Vector3 := Maths.Vec3_0;
    Norm_Left         : Singles.Vector3 := Maths.Vec3_0;
    Norm_Top          : Singles.Vector3 := Maths.Vec3_0;
    Norm_Bottom       : Singles.Vector3 := Maths.Vec3_0;
    Norm_Near         : Singles.Vector3 := Maths.Vec3_0;
    Norm_Far          : Singles.Vector3 := Maths.Vec3_0;
    --  Centre points
    Far_Centre        : Singles.Vector3 := Maths.Vec3_0;
    Near_Centre       : Singles.Vector3 := Maths.Vec3_0;
    --  Get 8 corner points
    Far_Top_Left      : Singles.Vector3 := Maths.Vec3_0;
    Far_Top_Right     : Singles.Vector3 := Maths.Vec3_0;
    Far_Bot_Left      : Singles.Vector3 := Maths.Vec3_0;
    Far_Bot_Right     : Singles.Vector3 := Maths.Vec3_0;
    Near_Top_Left     : Singles.Vector3 := Maths.Vec3_0;
    Near_Top_Right    : Singles.Vector3 := Maths.Vec3_0;
    Near_Bot_Left     : Singles.Vector3 := Maths.Vec3_0;
    Near_Bot_Right    : Singles.Vector3 := Maths.Vec3_0;

    F_Camera_Position : Singles.Vector3 := Maths.Vec3_0;

    Cull_Enabled      : Boolean := False;
    --     Cull_Enabled      : Boolean := True;
    Update_Enabled    : Boolean := True;

    --      Prev_Vp_Size   : Integer := 0;

    Frustum_Wireframe_Shader_Program : GL.Objects.Programs.Program;
    Frustum_Wireframe_VAO            : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Frustum_Solid_VAO                : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Frustum_Wireframe_VBO            : GL.Objects.Buffers.Buffer;
    Frustum_Solid_VBO                : GL.Objects.Buffers.Buffer;

    --  ------------------------------------------------------------------------

    function Compare_Plane_Aab (Mins, Maxs, Plane, Plane_Point :
                                Singles.Vector3) return Boolean is
        use GL;
        use GL.Types.Singles;
        Dist           : Singles.Vector3;
        Farthest_Point : Singles.Vector3;
    begin
        if Plane (X) < 0.0 then
            Farthest_Point (X) := Mins (X);
        else
            Farthest_Point (X) := Maxs (X);
        end if;
        if Plane (Y) < 0.0 then
            Farthest_Point (Y) := Mins (Y);
        else
            Farthest_Point (Y) := Maxs (Y);
        end if;
        if Plane (Z) < 0.0 then
            Farthest_Point (Z) := Mins (Z);
        else
            Farthest_Point (Z) := Maxs (Z);
        end if;

        Dist := Farthest_Point - Plane_Point;
        return Dot_Product (Plane, Dist) >= 0.0;
    end Compare_Plane_Aab;

    --  ------------------------------------------------------------------------
    --  Init debug variables for frustum wireframe
    procedure Draw_Frustum_Box is
        use GL.Objects.Buffers;
        use GL.Toggles;
        Solid_Points : constant Singles.Vector3_Array (1 .. 36) :=
        --  Front
                         (Near_Top_Right, Near_Top_Left, Near_Bot_Left,
                          Near_Bot_Left, Near_Bot_Right, Near_Top_Right,
                          --  Rear
                          Far_Top_Left, Far_Top_Right, Far_Bot_Right,
                          Far_Bot_Right, Far_Bot_Left, Far_Top_Left,
                          -- Left
                          Near_Top_Left, Far_Top_Left, Near_Bot_Left,
                          Near_Bot_Left, Near_Bot_Left, Near_Top_Right,
                          --  Right
                          Far_Top_Right, Near_Top_Right, Near_Bot_Right,
                          Near_Bot_Right, Far_Bot_Right, Far_Top_Right,
                          --  Top
                          Far_Top_Right, Far_Top_Left, Near_Top_Left,
                          Near_Top_Left, Near_Top_Right, Near_Bot_Left,
                          --  Bottom
                          Far_Bot_Right, Near_Bot_Left, Near_Bot_Left,
                          Near_Bot_Left, Near_Bot_Right, Far_Bot_Right);


        Wire_Frame_Points : constant Singles.Vector3_Array (1 .. 16) :=
        --  Front
                              (Near_Bot_Left, Near_Top_Left,
                               Near_Top_Right, Near_Bot_Right,
                               --  Rear
                               Far_Bot_Right, Far_Top_Left,
                               Far_Top_Left, Far_Bot_Left,
                               -- Left
                               Near_Bot_Left,
                               --  Front
                               Near_Bot_Right, Far_Bot_Right,
                               --  Rear
                               Far_Bot_Left, Far_Top_Left,
                               --  Top
                               Near_Top_Left, Near_Top_Right, Far_Top_Right);
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

    procedure Enable_Frustum_Cull (State : Boolean) is
    begin
        Cull_Enabled := State;
    end Enable_Frustum_Cull;

    --  ------------------------------------------------------------------------

    procedure Enable_Update (State : Boolean) is
    begin
        Update_Enabled := State;
    end Enable_Update;

    --  ------------------------------------------------------------------------

    function Frustum_Camera_Position return Singles.Vector3 is
    begin
        return F_Camera_Position;
    end Frustum_Camera_Position;

    --  ------------------------------------------------------------------------
    --  Set to false to stop frustum culling alogether for testing
    function Frustum_Cull_Enabled return Boolean is
    begin
        return Cull_Enabled;
    end Frustum_Cull_Enabled;

    --  ------------------------------------------------------------------------

    --  Set To False To Stop Frustum Plane Extraction
    function Frustum_Update_Enabled return Boolean is
    begin
        return Update_Enabled;
    end Frustum_Update_Enabled;

    --  ------------------------------------------------------------------------

    procedure Init is
        use GL.Attributes;
        use GL.Objects.Buffers;
        use GL.Objects.Vertex_Arrays;
        use Shader_Attributes;
    begin
        Frustum_Wireframe_VAO.Initialize_Id;
        --        Frustum_Wireframe_VAO.Bind;

        Frustum_Wireframe_VBO.Initialize_Id;
        Array_Buffer.Bind (Frustum_Wireframe_VBO);
        Allocate (Array_Buffer, 16, Static_Draw);
        GL_Utils.Add_Attribute_To_Array
          (Frustum_Wireframe_VAO, Attrib_VP, Frustum_Wireframe_VBO, 3);
        --        Set_Vertex_Attrib_Pointer (Attrib_VP, 3, Single_Type, False, 0, 0);
        --        Enable_Vertex_Attrib_Array (Attrib_VP);

        Frustum_Solid_VAO.Initialize_Id;
        --        Frustum_Solid_VAO.Bind;

        Frustum_Solid_VBO.Initialize_Id ;
        Array_Buffer.Bind (Frustum_Solid_VBO);
        Allocate (Array_Buffer, 36, Static_Draw);
        GL_Utils.Add_Attribute_To_Array
          (Frustum_Solid_VAO, Attrib_VP, Frustum_Solid_VBO, 3);
        --        Set_Vertex_Attrib_Pointer (Attrib_VP, 3, Single_Type, False, 0, 0);
        --        Enable_Vertex_Attrib_Array (Attrib_VP);

        Frustum_Shader_Manager.Init (Frustum_Wireframe_Shader_Program);

    end Init;

    --  ------------------------------------------------------------------------
    --  For each frustum plane:
    --   assemble farthest xyz box corner point in direction of plane
    --   return false (cull) if that point is behind plane
    --   return true if all 6 farthest xyz points are in front of their planes
    function Is_Aabb_In_Frustum (Mins, Maxs : in out Singles.Vector3)
                                return Boolean is
        use Singles;
        Min3   : constant Vector3 := Mins;
        Result : Boolean := not Cull_Enabled;
    begin
        if not Result then
            for index in Vector3'Range loop
                if Mins (index) > Maxs (index) then
                    Mins (index) := Maxs (index);
                    Maxs (index) := Min3 (index);
                end if;
            end loop;

            Result := Compare_Plane_Aab (Mins, Maxs, Norm_Near, Near_Top_Right);
            if Result then
                Result := Compare_Plane_Aab (Mins, Maxs, Norm_Right, Near_Top_Right);
            end if;
            if Result then
                Result := Compare_Plane_Aab (Mins, Maxs, Norm_Left, Near_Top_Right);
            end if;
            if Result then
                Result := Compare_Plane_Aab (Mins, Maxs, Norm_Top, Near_Top_Right);
            end if;
            if Result then
                Result := Compare_Plane_Aab (Mins, Maxs, Norm_Bottom, Near_Top_Right);
            end if;
            if Result then
                Result := Compare_Plane_Aab (Mins, Maxs, Norm_Far, Near_Top_Right);
            end if;
        end if;

        return Result;
    end Is_Aabb_In_Frustum;

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
            Out_Of_Planes := Is_Negative (Norm_Right, F_Camera_Position) or else
              Is_Negative (Norm_Left, F_Camera_Position) or else
              Is_Negative (Norm_Top, F_Camera_Position) or else
              Is_Negative (Norm_Bottom, F_Camera_Position) or else
              Is_Negative (Norm_Near, Near_Centre) or else
              Is_Negative (Norm_Far, Far_Centre) or else
              Is_Negative (Norm_Left, Near_Centre);
            In_Planes := not Out_Of_Planes;
        end if;
        return In_Planes;

    end Is_Sphere_In_Frustum;

    --  ------------------------------------------------------------------------

    procedure Re_Extract_Frustum_Planes
      (Fovy_Deg : Maths.Degree; Aspect, Near, Far : Single;
       Cam_Pos  : Singles.Vector3; Mat : Singles.Matrix4) is
        use GL;
        use GL.Types.Singles;
        use Maths;
        use Maths.Single_Math_Functions;
        use GL_Maths;
        use GL_Maths.Singles_Array_Package;
        Inv_Matrix  : Single_Matrix (1 .. 4, 1 .. 4);
        Fwd_Local   : constant Single_Vector (1 .. 4) := (0.0, 0.0, -1.0, 0.0);
        Fwd_World   : Singles.Vector3;
        Up_Local    : constant Single_Vector (1 .. 4) := (0.0, 1.0, 0.0, 0.0);
        Up_World    : Singles.Vector3;
        Right_World : Singles.Vector3;
        Near_Height : Single;
        Near_Width  : Single;
        Far_Height  : Single;
        Far_Width   : Single;
        FOV_Rad     : constant Single := Single (To_Radians (Fovy_Deg));
        Fa_Hat      : Singles.Vector3;
        Fb_Hat      : Singles.Vector3;
        Fc_Hat      : Singles.Vector3;
        Fd_Hat      : Singles.Vector3;
        Scale       : constant Single := 2.0 * Tan (0.5 * FOV_Rad);
        OffsetM     : Singles.Vector3;
        OffsetP     : Singles.Vector3;
    begin
        if Cull_Enabled and Update_Enabled then
            Inv_Matrix := Inverse (To_Real_Matrix4 (Mat));
            F_Camera_Position := Cam_Pos;
            Fwd_World := From_Real_Vector3 (Inv_Matrix * Fwd_Local);
            Up_World := From_Real_Vector3 (Inv_Matrix * Up_Local);
            Right_World := Cross_Product (Vector3 (Fwd_World), Vector3 (Up_World));

            Near_Height := Scale * Near;
            Near_Width := Near_Height * Aspect;
            Far_Height := Scale * Far;
            Far_Width := Far_Height * Aspect;

            Far_Centre := Cam_Pos + Fwd_World * Far;
            Near_Centre := Cam_Pos + Fwd_World * Near;

            OffsetM :=  0.5 * Up_World * Far_Height - 0.5 * Right_World * Far_Width;
            OffsetP :=  0.5 * Up_World * Far_Height + 0.5 * Right_World * Far_Width;
            Far_Top_Left := Far_Centre + OffsetM;
            Far_Top_Right := Far_Centre + OffsetP;
            Far_Bot_Left := Far_Centre - OffsetM;
            Far_Bot_Right := Far_Centre - OffsetP;

            OffsetM :=  0.5 * Up_World * Near_Height - 0.5 * Right_World * Near_Width;
            OffsetP :=  0.5 * Up_World * Near_Height + 0.5 * Right_World * Near_Width;
            Near_Top_Left := Far_Centre + OffsetM;
            Near_Top_Right := Far_Centre + OffsetP;
            Near_Bot_Left := Far_Centre - OffsetM;
            Near_Bot_Right := Far_Centre - OffsetP;

            Fa_Hat := Normalized (Near_Centre + 0.5 * Right_World * Near_Width - Cam_Pos);
            Fb_Hat := Normalized (Near_Centre - 0.5 * Right_World * Near_Width - Cam_Pos);
            Fc_Hat := Normalized (Near_Centre + 0.5 * Up_World * Near_Height - Cam_Pos);
            Fd_Hat := Normalized (Near_Centre - 0.5 * Up_World * Near_Height - Cam_Pos);

            --        Game_Utils.Game_Log ("Frustum.Re_Extract_Frustum_Planes Norm_Right");
            Norm_Right := Normalized (Cross_Product (Up_World, Fa_Hat));
            Norm_Left := Normalized (Cross_Product (Fb_Hat, Up_World));
            Norm_Top := Normalized (Cross_Product (Fc_Hat, Right_World));
            Norm_Bottom := Normalized (Cross_Product (Right_World, Fd_Hat));
            Norm_Near := Normalized (Fwd_World);
            Norm_Far := Normalized (-Fwd_World);
        end if;
    end Re_Extract_Frustum_Planes;

    --  ------------------------------------------------------------------------

end Frustum;
