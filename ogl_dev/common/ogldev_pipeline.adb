
with Maths;

package body Ogldev_Pipeline is

   function Get_World_Transform (P : in out Pipeline) return Singles.Matrix4 is
      use GL.Types.Singles;
      Scale_Xform       : constant Matrix4 := Maths.Scaling_Matrix (P.Scale);
      Rotate_Xform      : Matrix4;
      Translation_Xform : constant Matrix4 := Maths.Translation_Matrix (P.World_Pos);
   begin
      Maths.Init_Rotation_Transform (P.Rotation_Info, Rotate_Xform);
      P.W_Transformation := Scale_Xform * Translation_Xform * Rotate_Xform;
      return P.W_Transformation;
   end Get_World_Transform;

   --  -------------------------------------------------------------------------

   function Get_WVP_Transform (P : in out Pipeline) return Singles.Matrix4 is
      use GL.Types.Singles;
      WT  : constant Matrix4 := Get_World_Transform (P);
      VPT : constant Matrix4 := Get_VP_Transform (P);
   begin
      P.WVP_Transformation := VPT * WT;
      return P.WVP_Transformation;
   end Get_WVP_Transform;

   --  -------------------------------------------------------------------------

function Get_Proj_Transform (P : in out Pipeline) return Singles.Matrix4 is
   begin
      Maths.Init_Perspective_Transform
        (Maths.Degree (P.Perspective_Proj.FOV), Single (P.Perspective_Proj.Width),
         Single (P.Perspective_Proj.Height), P.Perspective_Proj.Z_Near,
         P.Perspective_Proj.Z_Far, P.Proj_Transformation);
      return P.Proj_Transformation;
   end Get_Proj_Transform;

   --  -------------------------------------------------------------------------

   function Get_View_Transform (P : in out Pipeline) return Singles.Matrix4 is
      use GL.Types.Singles;
      Camera_Translation_Trans : Matrix4;
      Camera_Rotate_Trans      : Matrix4;
   begin
      Camera_Translation_Trans := Maths.Translation_Matrix (-P.Camera.Position);
      Camera_Rotate_Trans := Ogldev_Math.Init_Camera_Transform (P.Camera.Target, P.Camera.Up);
      P.V_Transformation := Camera_Translation_Trans * Camera_Rotate_Trans;
      return P.V_Transformation;
   end Get_View_Transform;

   --  ------------------------------------------------------------------
   -------
   function Get_VP_Transform (P : in out Pipeline) return Singles.Matrix4 is
      use GL.Types.Singles;
      VT : constant Matrix4 := Get_View_Transform (P);
      PT : constant Matrix4 := Get_Proj_Transform (P);
   begin
       P.VP_Transformation := PT * VT;
      return P.VP_Transformation;
   end Get_VP_Transform;

   --  -------------------------------------------------------------------------

    function Get_WP_Transform (P : in out Pipeline) return Singles.Matrix4 is
      use GL.Types.Singles;
      WT   : constant Matrix4 := Get_World_Transform (P);
      Pers_Proj_Trans : Matrix4;
   begin
      Maths.Init_Perspective_Transform
        (Maths.Degree (P.Perspective_Proj.FOV), Single (P.Perspective_Proj.Width),
         Single (P.Perspective_Proj.Height), P.Perspective_Proj.Z_Near,
         P.Perspective_Proj.Z_Far, Pers_Proj_Trans);
      P.WP_Transformation := Pers_Proj_Trans * WT;
      return P.WP_Transformation;
   end Get_WP_Transform;

   --  -------------------------------------------------------------------------

   function Get_WV_Transform (P : in out Pipeline) return Singles.Matrix4 is
      use GL.Types.Singles;
      VT   : constant Matrix4 := Get_View_Transform (P);
      WT   : constant Matrix4 := Get_World_Transform (P);
   begin
      P.WV_Transformation := VT * WT;
      return P.WV_Transformation;
   end Get_WV_Transform;

   --  -------------------------------------------------------------------------

   function Get_WV_Ortho_P_Transform (P : in out Pipeline) return Singles.Matrix4 is
      use GL.Types.Singles;
      VT         : constant Matrix4 := Get_View_Transform (P);
      WT         : constant Matrix4 := Get_World_Transform (P);
      Ortho_Proj : Matrix4;
   begin
      Maths.Init_Orthographic_Transform
                 (P.Orthographic_Proj.Top, P.Orthographic_Proj.Bottom,
                  P.Orthographic_Proj.Left, P.Orthographic_Proj.Right,
                  P.Orthographic_Proj.Z_Near, P.Orthographic_Proj.Z_Far, Ortho_Proj);
      P.WVP_Transformation := Ortho_Proj * VT * WT;
      return P.WVP_Transformation;
   end Get_WV_Ortho_P_Transform;

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

end Ogldev_Pipeline;
