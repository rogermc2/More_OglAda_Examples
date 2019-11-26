
package Cache.View is

   type Virtual_Pixel_Method is (Undefined_VP_Method,
                                 Background_VP_Method,
                                 Constant_VP_Method,  --  deprecated
                                 Dither_VP_Method,
                                 Edge_VP_Method,
                                 Mirror_VP_Method,
                                 Random_VP_Method,
                                 Tile_VP_Method,
                                 Transparent_VP_Method,
                                 Mask_VP_Method,
                                 Black_VP_Method,
                                 Gray_VP_Method,
                                 White_VP_Method,
                                 Horizontal_Tile_VP_Method,
                                 Vertical_Tile_VP_Method,
                                 Horizontal_Tile_Edge_VP_Method,
                                 Vertical_Tile_Edge_VP_Method,
                                 Checker_Tile_VP_Method);
   pragma Convention (C, Virtual_Pixel_Method);

end Cache.View;
