(in-package #:mono)

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

(defmacro with-shader (symbol vertex-shader-source fragment-shader-source &body body)
  "Creates a form that compiles a vertex and fragment shader from source, links
it and makes it available throughout the form."
  `(handler-case
       (let ((vs (gl:create-shader :vertex-shader))
             (fs (gl:create-shader :fragment-shader))
             (,symbol (gl:create-program)))
         (gl:shader-source vs ,vertex-shader-source)
         (format t "Compiling vertex shader...~%")
         (gl:compile-shader vs)
         (let ((info (gl:get-shader-info-log vs)))
           (if (not (string-equal "" info))
               (format t "~a" (gl:get-shader-info-log fs))))
         (gl:shader-source fs ,fragment-shader-source)
         (format t "Compiling fragment shader...~%")
         (gl:compile-shader fs)
         (let ((info (gl:get-shader-info-log fs)))
           (if (not (string-equal "" info))
               (format t "~a" (gl:get-shader-info-log fs))))
         (format t "Attaching vertex shader...~%")
         (gl:attach-shader ,symbol vs)
         (format t "Attaching fragment shader...~%")
         (gl:attach-shader ,symbol fs)
         (format t "Linking shader program~%")
         (gl:link-program ,symbol)
         (print (gl:get-program-info-log ,symbol))
         (gl:delete-shader vs)
         (gl:delete-shader fs)
         (gl:use-program ,symbol)
         ,@body
         (gl:delete-program ,symbol))
     (error (c)
       (print c)
       (print (gl:get-error)))))
