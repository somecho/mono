(in-package #:niu)

(defconstant +vs-default+ "
#version 330 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec4 aCol;

out vec4 vCol;

void main(){
  gl_Position = vec4(aPos, 1.0);
  vCol = aCol;
}
")

(defconstant +vs-projection+ "
#version 330 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec4 aCol;
uniform mat4 projectionMatrix;

out vec4 vCol;

void main(){
  gl_Position = projectionMatrix * vec4(aPos, 1.0);
  vCol = aCol;
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

(defconstant +fs-default+ "
#version 330

in vec4 vCol;
out vec4 FragColor;

void main()
{
  FragColor = vCol;
}
")
