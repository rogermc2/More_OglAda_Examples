
 package Effect is

    type Preview_Type is (Undefined_Preview,
                          Rotate_Preview,
                          Shear_Preview,
                          Roll_Preview,
                          Hue_Preview,
                          Saturation_Preview,
                          Brightness_Preview,
                          Gamma_Preview,
                          Spiff_Preview,
                          Dull_Preview,
                          Grayscale_Preview,
                          Quantize_Preview,
                          Despeckle_Preview,
                          Reduce_Noise_Preview,
                          Add_Noise_Preview,
                          Sharpen_Preview,
                          Blur_Preview,
                          Threshold_Preview,
                          Edge_Detect_Preview,
                          Spread_Preview,
                          Solarize_Preview,
                          Shade_Preview,
                          Raise_Preview,
                          Segment_Preview,
                          Swirl_Preview,
                          Implode_Preview,
                          Wave_Preview,
                          Oil_Paint_Preview,
                          Charcoal_Drawing_Preview,
                          JPEG_Preview);
   pragma Convention (C, Preview_Type);

end Effect;
