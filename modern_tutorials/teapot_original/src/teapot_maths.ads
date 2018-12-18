
with GL.Types; use GL.Types;

package Teapot_Maths is

   Maths_Error : Exception;

    function Bernstein_Polynomial (I, N : Int; U : Single) return Single;
    function Binomial_Coefficient (I, N : Int) return Single;
    function Factorial (N : Int) return Int;

end Teapot_Maths;
