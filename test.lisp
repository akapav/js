(in-package :js-user) ;;todo: make separate package

(defmacro no-warn (&body body)
  `(locally #+sbcl (declare (sb-ext:muffle-conditions warning))
	    #-sbcl ()
	    (progn ,@body)))

(defun test1 ()
  #+nil(setf (prop this "a") (make-instance 'native-hash))
#{javascript}

a = new Object;
f = function (o) {
  return o.a;
}

a.a = 1;

r1 = f(this);
r2 = f(a);

f2 = function f2() {
  return this.a;
}

r3 = f2();

a.f3 = f2;
r4 = a.f3();

f4 = function f4() {
 return f(this);
};

r5 = f4();
r6 = this.f4();
a.f5 = f4;
r7 = a.f5();

fifi = 3;
a.fifi = 4;
a.f = function () {return fifi;}
r8 = a.f()

.

(no-warn
  (test r1 a)
  (test r2 1)
  (test r3 a)
  (test r4 1)
  (test r5 a)
  (test r6 a)
  (test r7 1)
  (test r8 3)))


(defun test2 ()

#{javascript}
a = new Object
a.x = 3;

function f33()
{
  var v = new Object;
  v.v1 = new Object;
  v.v1.v2 = 4;
  this.y = v.v1.v2;
}

f33.prototype = a;

b = new f33();
r1 = b.x;
r2 = b.y;
.

(no-warn
  (test r1 3)
  (test r2 4)))


(defun test3 ()
  (labels ((fib2 (n)
	     (if (< n 2) 1
		 (+ (fib2 (1- n)) (fib2 (- n 2))))))

#{javascript}
function fib(n)
{
  if(n < 2) return 1;
  return fib(n - 1) + fib(n - 2);
}

function ffib(n)
{
   var s1 = 1;
   var s2 = 1;
   var  res = 1;
   while(n > 1) {
     res = s1 + s2;
     s1 = s2;
     s2 = res;
     n = n - 1;
   }
   return res;
}
.
    (loop for i from 1 to 10 do
      (test (fib i) (fib2 i)))

    (loop for i from 1 to 10 do
      (test (ffib i) (fib2 i)))
	t))


(defun test4 ()
#{javascript}
function afun(a)
{
  from_inside = 1;
  arguments[0] = a + 1;
  r1 = a + 1;
  r2 = a;
  r3 = arguments[0];
  r4 = arguments[1];
}
.

(no-warn
 (afun 100 200)
 (test 102 r1)
 (test 101 r2)
 (test 101 r3)
 (test 200 r4)
 (test 1 from_inside)))

(defun test5 ()
#{javascript}
function adder(n)
{
return function(m) {return n+m;};
}
r1 = adder(1)(2);
.
  (no-warn
   (test r1 3)))

(defun test6 ()
#{javascript}
function fbla(x)
{
var a = function f1(n) {return b(n);};
var b = function f2(n, m)
{
  if(m) return n+n;
  return f2(n + 1, 5);
};
return a(x);
}
.
  (no-warn
   (test (fbla 2) 6)))

#{javascript}
function f4()
{
//var b; //=12;
return b;
  function b() {return 1;}

}
.

(defun test7 ()

#{javascript}
s = 0;
for(i = 0; i < 10; i = i + 1)
	s = s + 1;

r1 = s;

s = 0;
for(i  = 0;; i = i + 1) {
	s = s + i;
	if(s > 100) break;
}

r2 = i;

s = 0;
for(i = 0; i < 10; i = i +1 ) {
	if(i%2) continue; //oddp
	s = s + i;
}

r3 = s;


a = 100;
while(a)
{
	a = a - 1;
}

r4 = a;

a = 100;
do {
	a = a - 1;
} while(a > 0);

r5 = a;

do a = a - 1;
while(a > 0);

r6 = a;
.

(no-warn
 (test r1 10)
 (test r2 14)
 (test r3 20)
 (test r4 0)
 (test r5 0)
 (test r6 -1)))


(defun test8 ()
#{javascript}
function O (){}
O.prototype.f = function () {return this.n + 1;}
obj = new O;
obj.n = 20;
r1 = obj.f();
.
  (no-warn
   (test 21 r1)))

(defun test9 ()
#{javascript}
a: for(;;) {for(;;) break a;}  
b: for(i = 0; i< 10 ; i = i + 1) {for(;;) continue b;}
.
  (format t "test9 passed~%")
  t)

(defun test10 ()
#{javascript}
function f1()
{
   return function(val) {x10=val;}
}

o10 = new Object;
o10.x10=2;
f=f1()
with(o10) f(3);
.

(no-warn
  (test (!eval "o10.x10;") 2)
  (test x10 3)))

(defun test11 ()
#{javascript}
o = function () {}
o.x11="asd"
function f11x(o)
{
   with(o) {return function(val) {x11=val;};}
}

o11 = new Object
f11 = f11x(o11)
f11(5)

r1 = x11
r2 = o11.x11

o11.x11 = 3
f11(4)

r3 = x11
r4 = o11.x11

.
 
 (no-warn
    (test r1 5)
    (test r2 :undefined)
    (test r3 5)
    (test r4 4)))

;;;;;

;; (defun js-ast (stream)
;;   `'(,(process-ast
;; 	   (parse-js-string (read-line-stream stream)))))

;; (define-reader 'js-ast #'js-ast)

;; #{js-ast}
;; x=function a(b)
;; {
;; };
;; .

(defun fast-test ()
  (macrolet ((flist (&rest syms)
	       `(list ,@(mapcar (lambda (s) `(function ,s)) syms))))
    (every #'identity
	   (mapcar #'funcall
		   (flist test1 test2 test3 test4
			  test5 test6 test7 test8
			  test9 test10 test11)))))
