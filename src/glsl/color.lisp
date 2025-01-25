(in-package #:mono/glsl)

(defconstant +palette+ "
vec3 palette(float t, vec3 a, vec3 b, vec3 c, vec3 d){
    return a + b*cos( 6.283185*(c*t+d) );
}
" "Source: https://iquilezles.org/articles/palettes/")
