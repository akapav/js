(declaim (optimize (safety 0) (debug 0) (speed 3)))
(asdf:oos 'asdf:load-op :js)
(in-package :js)

(require :sb-sprof)

#{javascript}
function fib(n)
{
  if(n < 2) return 1;
  return fib(n - 1) + fib(n - 2);
}
.

(sb-sprof:profile-call-counts "JS")

(sb-sprof:with-profiling
    (#+nil(:max-samples 10000)
     :mode :cpu 
     :report :graph
     :reset t
     :loop nil)
  (fib 20))
