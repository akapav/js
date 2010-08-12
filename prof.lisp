(declaim (optimize (safety 0) (debug 0) (speed 3)))
(asdf:oos 'asdf:load-op :js)
(in-package :js-user)

(require :sb-sprof)

#{javascript}
function fib(n)
{
  if(n < 2) return 1;
  return fib(n - 1) + fib(n - 2);
}
.

#{javascript}
//benchmark form language shootout
function A(i,j) {
  return 1/((i+j)*(i+j+1)/2+i+1);
}

function Au(u,v) {
  for (var i=0; i<u.length; ++i) {
    var t = 0;
    for (var j=0; j<u.length; ++j)
      t += A(i,j) * u[j];
    v[i] = t;
  }
}

function Atu(u,v) {
  for (var i=0; i<u.length; ++i) {
    var t = 0;
    for (var j=0; j<u.length; ++j)
      t += A(j,i) * u[j];
    v[i] = t;
  }
}


function AtAu(u,v,w) {
  Au(u,w);
  Atu(w,v);
}

function spectralnorm(n) {
  var i
  var u=[]
  var v=[]
  var w=[]
  var vv=0
  var vBv=0;
  for (i=0; i<n; ++i) {
    u[i] = 1; v[i] = w[i] = 0;
  }
  for (i=0; i<10; ++i) {
    AtAu(u,v,w);
    AtAu(v,u,w);
  }
  for (i=0; i<n; ++i) {
    vBv += u[i]*v[i];
    vv  += v[i]*v[i];
  }
  return vBv/vv;
}
.

#+nil (sqrt (js-eval "spectralnorm(200);"))

(sb-sprof:profile-call-counts "JS")

(sb-sprof:with-profiling
    (#+nil(:max-samples 10000)
     :mode :cpu 
     :report :graph
     :reset t
     :loop nil)
(js::js-funcall  (js-eval "fib;") 30))
