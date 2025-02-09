(ql:quickload :mono)

(defparameter w 900)
(defparameter h 1200)
(defparameter proj (kit.glm:ortho-matrix 0 900 1200 0 -500 500))

(defparameter numcols 50)
(defparameter numrows 50)

(defparameter pts
  (let* ((c-width (/ w numcols))
         (c-height (/ h numrows)))
    (mono:with-return (v (mono:empty-vec))
      (dotimes (y numrows)
        (dotimes (x numcols)
          (mono:push v (+ (* x c-width 1.0) (* c-width 0.5)))
          (mono:push v (+ (* y c-height 1.0) (* c-height 0.5))))))))

(defparameter vs  "#version 330 core
layout (location = 0) in vec2 aPos;
out vec4 vCol;
void main(){
  gl_Position =  vec4(vec3(aPos, 0.0), 1.0);
}")

(defparameter gs (format nil "
#version 330 core
layout (points) in;
layout (triangle_strip, max_vertices = 4) out;
uniform mat4 projectionMatrix;
uniform float time;
uniform vec2 r;
out vec3 vPos;
out vec2 gid;

~a
~a

void main() {
    vec4 pos = gl_in[0].gl_Position;
    float t = time * 0.01;
    float scale = 0.0005;

    float xd = snoise(vec3(time*0.005, pos.y * scale , pos.x * scale));
    float yd = snoise(vec3(pos.x * scale, time*0.005, pos.y * scale));
    float zd = snoise(vec3(pos.x * scale, pos.y * scale, time*0.005));

    mat4 rot = rot3d(vec3(xd,yd,zd), 3.14);

    vec4 p1 = rot * vec4(0.0, -r.x * 0.5, r.y * 0.5, 1.0);
    vec4 p2 = rot * vec4(0.0, r.x * 0.5, r.y*0.5, 1.0);
    vec4 p3 = rot * vec4(0.0, -r.x * 0.5, -r.y * 0.5 , 1.0);
    vec4 p4 = rot * vec4(0.0, r.x * 0.5, -r.y * 0.5, 1.0);

    p1 = projectionMatrix * (pos + vec4(p1.xyz, 0.0));
    gl_Position = p1;
    vPos = p1.xyz;
    gid = pos.xy;
    EmitVertex();

    p2 = projectionMatrix * (pos + vec4(p2.xyz, 0.0));
    gl_Position = p2;
    vPos = p2.xyz;
    gid = pos.xy;
    EmitVertex();

    p3 = projectionMatrix * (pos + vec4(p3.xyz, 0.0));
    gl_Position = p3;
    vPos = p3.xyz;
    gid = pos.xy;
    EmitVertex();

    p4 = projectionMatrix * (pos + vec4(p4.xyz, 0.0));
    gl_Position = p4;
    vPos = p4.xyz;
    gid = pos.xy;
    EmitVertex();

    EndPrimitive();
}" mono/glsl:+rot3d+ mono/glsl:+simplex-3d+))

(defparameter fs (format nil "
#version 330 core

in vec3 vPos;
in vec2 gid;

~a

void main(){
  vec3 normal = normalize(cross(dFdx(vPos), dFdy(vPos)));
  vec3 eye = vec3(0.0,0.0,1.0);
  vec3 viewDir = normalize(eye - vPos);
  vec3 perturb = sin(vPos * 10.0 + gid.x + gid.y) * 0.5;
  float diff = dot(normal + perturb, viewDir);
  vec3 col = palette(diff, vec3(0.5), vec3(0.5), vec3(1.0), vec3(0.0));
  gl_FragColor = vec4(col, 1.0);
}
" mono/glsl:+palette+))

(mono:start (:width w :height h :samples 8)
  (mono:with-gl-resources (:buffers (pts-vbo)
                           :shaders ((my-shader (:vertex-source vs
                                                 :geometry-source gs
                                                 :fragment-source fs))))
    (gl:use-program my-shader)
    (gl:enable :depth-test)
    (mono:write-array-buffer pts-vbo (mono:gl-array pts))
    (mono:set-vertex-attrib 2)
    (mono:uniform-mat4f "projectionMatrix" (kit.glm:transpose-matrix proj))
    (mono:uniform2f "r" 30.0 70.0)
    (mono:with-loop
      (mono:uniformf "time" (* mono::frame-num 1.0))
      (gl:clear-color 1.0 1.0 1.0 1.0)
      (gl:clear :color-buffer-bit :depth-buffer-bit)
      (gl:draw-arrays :points 0 (* numcols numrows)))))
