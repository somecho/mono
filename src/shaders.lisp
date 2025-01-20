(in-package #:niu)

(defconstant +vs-default+ "
#version 330 core
layout (location = 0) in vec3 aPos;
void main(){
  gl_Position = vec4(aPos, 1.0);
}
")

(defconstant +fs-white+ "
#version 330
out vec4 FragColor;

void main()
{
  FragColor = vec4(1.0,1.0,1.0,1.0);
}
")
