(ns jast.demos.minimal-webgl
  (:require [jast.core :refer [jast->js-program]]
            [jast.tools :refer [unquotable]]))

(def jast-webgl-program
  (unquotable
   '((let canvas (document.createElement "canvas"))
     (document.body.appendChild canvas)
     (let a (= canvas.width innerWidth))
     ("/=" a (= canvas.height (+ innerHeight 0.1)))
     (let gl (canvas.getContext "webgl2"))
     (let p (gl.createProgram (= t 35633)))
     (.map [(raw-js
             "`out V u;
              void main(){
                X x=X(-1);
                x[gl_VertexID]=3.;
                gl_Position=X(u=x.xy,0,1);
                u.x*=${a};
              }`")
            (raw-js
             "`out X c;
              uniform float t;
              in V u;
              bool f(v p){
                m(x,.8);
                m(y,.5);
                return length(p)<.5;
              }
              float r(V e){
                return fract(sin(92.*u.x)*89.1+sin(87.*u.y)*-67.1+sin(e.x)*83.+sin(e.y)*93.+sin(t)*-97.);
              }
              void main(){
                float a=0.;
                l(i,6){v p=v(0,0,-.8);
                  v d=normalize(v(u,1));
                  float t=1.;
                  l(j,16){
                    p-=log(R(0))*.1*d;
                    if(length(p)>1.){
                      a+=.8<dot(d,v(-.2,.2,-.9))?
                      t:0.;
                      break;
                    }
                    if(f(p)){
                      t*=.2;
                      d=normalize(((2.*v(R(1),R(2),R(3)))-1.));
                    }
                  }
                }
                c=X(v(7.*a),1.);
              }`")]
           (fn [x]
             (gl.attachShader
              p
              (= s (gl.createShader (-- t)))
              (gl.shaderSource s
                               (+ (raw-js
                                   "`#version 300 es
                                    #define R(a)r(V(i+a,j))
                                    #define m(b,c)p+=sin(22.*p.b*pow(2.,sin(t*c))+2.*t)*v(.1,.1,.05)
                                    #define l(i,m)for(int i=0;i<m;i++)
                                    #define v vec3
                                    #define V vec2
                                    #define X vec4
                                    precision highp float ;`")
                                  x))
              (gl.compileShader s))))
     (gl.linkProgram p)
     (gl.useProgram p)
     ((= k (fn [_]
             (requestAnimationFrame k
                                    (gl.uniform1f
                                     (gl.getUniformLocation p "t")
                                     (/ (performance.now) 1000))
                                    (gl.drawArrays 5 0 3))))))))

(defn start-demo []
  (js/eval (jast->js-program jast-webgl-program)))

(defn init []
  (js/window.addEventListener "load" start-demo))
