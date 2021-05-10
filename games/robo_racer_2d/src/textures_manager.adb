
package body Textures_Manager is

    --  -------------------------------------------------------------------------

    procedure Add_Texture_To_List (theTextures : in out Texture_List;
                                   aTexture    : Texture) is
    begin
        theTextures.Append (aTexture);
    end Add_Texture_To_List;

    --  -------------------------------------------------------------------------

    function Get_Last_Index (theTextures : Texture_List) return Positive is
    begin
        return theTextures.Last_Index;
    end Get_Last_Index;

    --  -------------------------------------------------------------------------

    function Get_Texture (theTextures : Texture_List; Index : Positive) return
      GL.Objects.Textures.Texture is
    begin
        return theTextures.Element (Index);
    end Get_Texture;

    --  -------------------------------------------------------------------------

end Textures_Manager;
