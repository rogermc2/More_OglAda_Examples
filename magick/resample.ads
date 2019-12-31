
package Resample is

  type Filter_Types is (Undefined_Filter,
                        Point_Filter,
                        Box_Filter,
                        Triangle_Filter,
                        Hermite_Filter,
                        Hanning_Filter,
                        Hamming_Filter,
                        Blackman_Filter,
                        Gaussian_Filter,
                        Quadratic_Filter,
                        Cubic_Filter,
                        Catrom_Filter,
                        Mitchell_Filter,
                        Jinc_Filter,
                        Sinc_Filter,
                        SincFast_Filter,
                        Kaiser_Filter,
                        Welsh_Filter,
                        Parzen_Filter,
                        Bohman_Filter,
                        Bartlett_Filter,
                        Lagrange_Filter,
                        Lanczos_Filter,
                        Lanczos_Sharp_Filter,
                        Lanczos2_Filter,
                        Lanczos2_Sharp_Filter,
                        Robidoux_Filter,
                        Robidoux_Sharp_Filter,
                        Cosine_Filter,
                        Spline_Filter,
                        Sentinel_Filter  --  count of all the _Filters,
                        --  not a real _Filter
                        );
   pragma Convention (C, Filter_Types);

end Resample;
