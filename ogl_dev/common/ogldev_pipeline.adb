
with Maths;

package body Ogldev_Pipeline is

    procedure Set_Proj_Transform (P : in out Pipeline);
    procedure Set_View_Transform (P : in out Pipeline);
    procedure Set_WP_Transform (P : in out Pipeline);
    procedure Set_World_Transform (P : in out Pipeline);
    procedure Set_WV_Ortho_P_Transform (P : in out Pipeline);

    --  -------------------------------------------------------------------------

    function Get_World_Transform (P : Pipeline) return Singles.Matrix4 is
    begin
        return P.W_Transformation;
    end Get_World_Transform;

    --  -------------------------------------------------------------------------

    function Get_WVP_Transform (P : Pipeline) return Singles.Matrix4 is
    begin
        return P.WVP_Transformation;
    end Get_WVP_Transform;

    --  -------------------------------------------------------------------------

    function Get_Proj_Transform (P : Pipeline) return Singles.Matrix4 is
    begin
        return P.Proj_Transformation;
    end Get_Proj_Transform;

    --  -------------------------------------------------------------------------

    function Get_View_Transform (P : Pipeline) return Singles.Matrix4 is
    begin
        return P.V_Transformation;
    end Get_View_Transform;

    --  ------------------------------------------------------------------

    function Get_VP_Transform (P : Pipeline) return Singles.Matrix4 is
    begin
        return P.VP_Transformation;
    end Get_VP_Transform;

    --  -------------------------------------------------------------------------

    function Get_WP_Transform (P : Pipeline) return Singles.Matrix4 is
    begin
        return P.WP_Transformation;
    end Get_WP_Transform;

    --  -------------------------------------------------------------------------

    function Get_WV_Transform (P : Pipeline) return Singles.Matrix4 is
    begin
        return P.WV_Transformation;
    end Get_WV_Transform;

    --  -------------------------------------------------------------------------

    function Get_WV_Ortho_P_Transform (P : Pipeline) return Singles.Matrix4 is
    begin
        return P.WVP_Transformation;
    end Get_WV_Ortho_P_Transform;

    --  -------------------------------------------------------------------------

    procedure Init_Transforms  (P : in out Pipeline) is
        use GL.Types.Singles;
    begin
        Set_World_Transform (P);
        Set_View_Transform (P);
        Set_Proj_Transform (P);
        P.VP_Transformation := P.Proj_Transformation * P.V_Transformation;
        Set_WP_Transform (P);
        P.WV_Transformation := P.V_Transformation * P.W_Transformation;
        Set_WV_Ortho_P_Transform (P);
        P.WVP_Transformation := P.VP_Transformation * P.W_Transformation;
    end Init_Transforms;

    --  -------------------------------------------------------------------------

    procedure Set_Camera (P : in out Pipeline; C : Ogldev_Camera.Camera) is
    begin
        P.Camera.Position := Ogldev_Camera.Get_Position (C);
        P.Camera.Target := Ogldev_Camera.Get_Target (C);
        P.Camera.Up := Ogldev_Camera.Get_Up (C);
    end Set_Camera;

    --  -------------------------------------------------------------------------

    procedure Set_Camera (P : in out Pipeline;
                          Pos, Target, Up : Singles.Vector3) is
    begin
        P.Camera.Position := Pos;
        P.Camera.Target := Target;
        P.Camera.Up := Up;
    end Set_Camera;

    --  -------------------------------------------------------------------------


    procedure Set_Scale (P : in out Pipeline; S : Single) is
    begin
        P.Scale := (S, S, S);
    end Set_Scale;

    --  -------------------------------------------------------------------------

    procedure Set_Scale (P : in out Pipeline; X, Y, Z : Single) is
    begin
        P.Scale := (X, Y, Z);
    end Set_Scale;

    --  -------------------------------------------------------------------------

    procedure Set_Scale (P : in out Pipeline; S : Singles.Vector3) is
    begin
        P.Scale := S;
    end Set_Scale;

    --  -------------------------------------------------------------------------

    procedure Set_Orthographic_Proj (P : in out Pipeline;
                                     OP : Ogldev_Math.Orthographic_Projection_Info) is
    begin
        P.Orthographic_Proj := OP;
    end Set_Orthographic_Proj;

    --  -------------------------------------------------------------------------

    procedure Set_Perspective_Proj (P : in out Pipeline;
                                    PP : Ogldev_Math.Perspective_Projection_Info) is
    begin
        P.Perspective_Proj := PP;
    end Set_Perspective_Proj;

    --  -------------------------------------------------------------------------

    procedure Set_Proj_Transform (P : in out Pipeline) is
        use Ogldev_Math;
    begin
        Maths.Init_Perspective_Transform
          (View_Angle => Maths.Degree (Get_Perspective_FOV (P.Perspective_Proj)),
           Width      => Single (Get_Perspective_Width (P.Perspective_Proj)),
           Height     => Single (Get_Perspective_Height (P.Perspective_Proj)),
           Z_Near     => Get_Perspective_Near (P.Perspective_Proj),
           Z_Far      => Get_Perspective_Far (P.Perspective_Proj),
           Transform  => P.Proj_Transformation);
    end Set_Proj_Transform;

    --  -------------------------------------------------------------------------

    procedure Set_Rotation (P : in out Pipeline; X, Y, Z : Single) is
    begin
        P.Rotation_Info := (X, Y, Z);
    end Set_Rotation;

    --  -------------------------------------------------------------------------

    procedure Set_Rotation (P : in out Pipeline; S : Singles.Vector3) is
    begin
        P.Rotation_Info := S;
    end Set_Rotation;

    --  -------------------------------------------------------------------------

    procedure Set_View_Transform (P : in out Pipeline) is
        use GL.Types.Singles;
        Camera_Translation_Trans : Matrix4;
        Camera_Rotate_Trans      : Matrix4;
    begin
        Camera_Translation_Trans := Maths.Translation_Matrix (-P.Camera.Position);
        Camera_Rotate_Trans := Ogldev_Math.Init_Camera_Transform (P.Camera.Target, P.Camera.Up);
        P.V_Transformation := Camera_Translation_Trans * Camera_Rotate_Trans;
    end Set_View_Transform;

    --  ------------------------------------------------------------------

    procedure Set_World_Position (P : in out Pipeline; X, Y, Z : Single) is
    begin
        P.World_Pos := (X, Y, Z);
    end Set_World_Position;

    --  -------------------------------------------------------------------------

    procedure Set_World_Position (P : in out Pipeline; S : Singles.Vector3) is
    begin
        P.World_Pos := S;
    end Set_World_Position;

    --  -------------------------------------------------------------------------

    procedure Set_World_Transform (P : in out Pipeline) is
        use GL.Types.Singles;
        Scale_Xform       : constant Matrix4 := Maths.Scaling_Matrix (P.Scale);
        Rotate_Xform      : Matrix4;
    begin
        Maths.Init_Rotation_Transform (P.Rotation_Info, Rotate_Xform);
        P.W_Transformation :=
          Maths.Translation_Matrix (P.World_Pos) * Rotate_Xform * Scale_Xform;
    end Set_World_Transform;

    --  -------------------------------------------------------------------------

    procedure Set_WP_Transform (P : in out Pipeline) is
        use GL.Types.Singles;
        use Ogldev_Math;
        Pers_Proj_Trans : Matrix4;
    begin
        Maths.Init_Perspective_Transform
          (Maths.Degree (Get_Perspective_FOV (P.Perspective_Proj)),
           Single (Get_Perspective_Width (P.Perspective_Proj)),
           Single (Get_Perspective_Height (P.Perspective_Proj)),
           Get_Perspective_Near (P.Perspective_Proj),
           Get_Perspective_Far (P.Perspective_Proj), Pers_Proj_Trans);
        P.WP_Transformation := Pers_Proj_Trans * P.W_Transformation;
    end Set_WP_Transform;

    --  -------------------------------------------------------------------------

    procedure Set_WV_Ortho_P_Transform (P : in out Pipeline) is
        use GL.Types.Singles;
        use Ogldev_Math;
        Ortho_Proj : Matrix4;
    begin
        Maths.Init_Orthographic_Transform
          (Get_Orthograpic_Top (P.Orthographic_Proj),
           Get_Orthograpic_Bottom (P.Orthographic_Proj),
           Get_Orthograpic_Left (P.Orthographic_Proj),
           Get_Orthograpic_Right (P.Orthographic_Proj),
           Get_Orthograpic_Near (P.Orthographic_Proj),
           Get_Orthograpic_Far (P.Orthographic_Proj), Ortho_Proj);
        P.WVP_Transformation := Ortho_Proj * P.V_Transformation * P.W_Transformation;
    end Set_WV_Ortho_P_Transform;

    --  -------------------------------------------------------------------------

end Ogldev_Pipeline;
