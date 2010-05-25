(asdf:oos 'asdf:load-op :ptester)

(in-package :js-user) ;;todo: make separate package
(use-package :ptester)

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

(defun test12 ()
#{javascript}
function f12x(x, o)
{
    function r() {return x;}
    with(o) return {'a': function (y) {x = y;}, 'b': r};
}

o12 = new Object
f12 = f12x(10, o12)
f12['a'](100)
r1 = o12.x; //undefined
r2 = f12['b'](); //100
o12.x = 1
f12['a'](200)
r3 = o12.x //200
r4 = f12['b']() //100
.

(no-warn
  (test r1 :undefined)
  (test r2 100)
  (test r3 200)
  (test r4 100)))

(defun test13 ()
#{javascript}
o13x=new Object
o13x.g = function() {return 2;}

function f13(o) {
  with(o) {
    function g() {return 1;}
    return g();
  }
}

.
(no-warn
  (test (f13 o13x) 2)))

(defun test14 ()
#{javascript}
try {
   r1 = bflj();
} catch(e) {
   r1 = 100;
} finally {
   r1 = 2 * r1;
}
.
(no-warn (test r1 200)))

(defun test15 ()
#{javascript}
function f15(x,s1)
{
  return function(y, s2) {return eval(s1+s2);}
}

r15 = f15(5, "y+")(6, "x;");
.

(no-warn (test r15 11)))

(defun test16 ()
#{javascript}
function f16(x,s)
{
  return eval(s);
}

r16 = f16(55, "x+x;")
.
(no-warn (test r16 110)))

(defun test17 ()
#{javascript}
function f17(o)
{
  var x = 3;
  with(o) {x = 4;}
  return x
}

oo = new Object
r17_1 = f17(oo)

oo.x = 7
r17_2 = f17(oo)
r17_3 = oo.x
.

(no-warn
  (test r17_1 4)
  (test r17_2 3)
  (test r17_3 4)))

(defun test18 ()
#{javascript}
function f18(o)
{
   with(o) return function(y) {x = y;}
}

oo = new Object
f18_1 = f18(oo)
f18_1(5)
r18_1 = x

oo.x = 7
f18_1(6)
r18_2 = x
r18_3 = oo.x
.
(no-warn
  (test r18_1 5)
  (test r18_2 5)
  (test r18_3 6)))

(defun test19 ()
#{javascript}
function f19(o, str)
{
   with(o) return function(y) {eval(str);}
}
oo = new Object
f19_1 = f19(oo, "x=y;")
f19_1(5)
r19_1 = x

oo.x = 7
f19_1(6)
r19_2 = x
r19_3 = oo.x
.
(no-warn
  (test r19_1 5)
  (test r19_2 5)
  (test r19_3 6)))

(defun test20 ()
#{javascript}
s1 = String(123)
s2 = new String(456)
String.prototype.y=12
r20_1 = s2.y
r20_2 = s2.charAt(1)
r20_3 = String.prototype.length
r20_4 = s2.length
r20_5 = (s2.length = 555)
r20_6 = s2.length
r20_7 = s2.substr("1",8)
r20_8 = s2.substring (0, 100)
.
(no-warn
  (test r20_1 12)
  (test r20_2 "5" :test #'string-equal)
  (test r20_3 0)
  (test r20_4 3)
  (test r20_5 555)
  (test r20_6 3)
  (test r20_7 "56" :test #'string-equal)
  (test r20_8 "456" :test #'string-equal)))

(defun test21 ()
#{javascript}
function test_splice()
{
  var a = new Array(1,2,3)
  r21_1 =  [a.splice(0, 0), a]

  var a = new Array(1,2,3)
  r21_2 =  [a.splice(0, 0, 100, 200), a]

  var a = new Array(1,2,3)
  r21_3 =  [a.splice(1, 1000), a]

  var a = new Array(1,2,3)
  r21_4 =  [a.splice(0, 1), a]

  var a = new Array(1,2,3)
  r21_5 =  [a.splice(0, 2, 10, 11, 12), a]

  var a = new Array(1,2,3)
  r21_6 =  [a.splice(0, 3, 10, 11, 12), a]

  var a = new Array(1,2,3)
  r21_7 =  [a.splice(2, 2), a]

  var a = new Array(1,2,3)
  r21_8 =  [a.splice(2, 2, 10, 12), a]

  var a = new Array(1,2,3)
  r21_9 =  [a.splice(1, 1, 10, 12), a]

  var a = new Array(1,2,3)
  r21_10 =  [a.splice(100, 100, 10, 12), a]

  var a = new Array(1,2,3)
  r21_11 =  [a.splice(1, 1, 7, 8, 9), a]

}

test_splice()
.

(defun dump-r21 (val arr1 arr2)
	   (let* ((p (js::value val))
		  (p1 (aref p 0))
		  (p2 (aref p 1)))
	     (test arr1 (js::value p1) :test #'equalp)
	     (test arr2 (js::value p2) :test #'equalp)))

(no-warn (mapc (lambda (l) (apply #'dump-r21 l))
	       (list
		(list r21_1 #() #(1 2 3))
		(list r21_2 #() #(100 200 1 2 3))
		(list r21_3 #(2 3) #(1))
		(list r21_4 #(1) #(2 3))
		(list r21_5 #(1 2) #(10 11 12 3))
		(list r21_6 #(1 2 3) #(10 11 12))
		(list r21_7 #(3) #(1 2))
		(list r21_8 #(3) #(1 2 10 12))
		(list r21_9 #(2) #(1 10 12 3))
		(list r21_10 #() #(1 2 3 10 12))
		(list r21_11 #(2) #(1 7 8 9 3))))))

(defun test22 ()
#{javascript}
function f22(x, y, str)
{
  f(0)
  r22_1 = x
  function f(x) {
     eval(str)
     r22_2 = x
  }
}

f22(1, 2, "x=y;")
.

(no-warn
  (test r22_1 1)
  (test r22_2 2)))

(defun test23 ()
#{javascript}
function f23(str)
{
  var f23_1 = function() {return 100;}
  eval(str);
  return f23_1()
}

r23_1 = f23("f23_1 = function() {return 5;};")
r23_2 = 0;
try {
 f23_1();
} catch(e) {
  r23_2 = 2;
}
r23_3 = f23("f23_2 = function() {return 5;};")
r23_4 = f23_2()
.

(no-warn
  (test r23_1 5)
  (test r23_2 2)
  (test r23_3 100)
  (test r23_4 5)))

(defun test24 ()
#{javascript}
r24_1 = new Function("a", "b", "return a+b;")(2,3)
r24_2 = new Function.prototype.constructor("a", "b", "return a+b;")(3,4)
r24_3 = new (function(){}).constructor("a", "b", "return a+b;")(4,5)

r24_4 = Function;
r24_5 = Function.prototype.constructor;
r24_6 = (function(){}).constructor;
.

(no-warn
  (test r24_1 5)
  (test r24_2 7)
  (test r24_3 9)
  (test r24_4 r24_5 :test #'eq)
  (test r24_4 r24_6 :test #'eq)))

(defun test25 ()
#{javascript}
r25_1 = Function.call(1, 'a', 'b' , 'return a*b;')(2,3)
r25_2 = "abc".charAt.call("def", 1); //todo: possible parser error -
                                     //when ; is omitted, program compiles
                                     //next line as an arguments list and
	         		     //tries to apply it to the result of
                                     //the previous one ("e")
r25_3 = (function(x,y) {return x-y;}).call(new Object,10,5)
.
(no-warn
  (test r25_1 6)
  (test r25_2 "e" :test #'string-equal)
  (test r25_3 5)))

(defun test26 ()
#{javascript}
function f26(x,y,z)
{
  return x+y*z;
}

r26_1 = f26.apply(new Object, [1,2,3,4])
r26_2 = f26.apply(new Object, new Array(1,2,3,4))

function f26_1()
{
  return f26.apply(new Object, arguments)
}

r26_3 = f26_1(1,2,3,4)

v26=[7,8,9];
[1,2,3].splice.apply(v26, [0,2,100,101])
for(var i = 0; i < v26.length; ++i)
  eval("r26_arr" + i + "=v26[i];")
.

(no-warn
  (test r26_1 7)
  (test r26_2 7)
  (test r26_3 7)
  (test r26_arr0 100)
  (test r26_arr1 101)
  (test r26_arr2 9)))
;;;;;

(defun js-ast (stream)
  `'(,(js::process-ast
       (parse-js-string (js::read-line-stream stream)))))

(define-reader 'js-ast #'js-ast)

#{js-ast}
x=function a(b)
{
};
.

(defun fast-test ()
  (macrolet ((flist (&rest syms)
	       `(list ,@(mapcar (lambda (s) `(function ,s)) syms))))
    (every #'identity
	   (mapcar #'funcall
		   (flist test1 test2 test3 test4
			  test5 test6 test7 test8
			  test9 test10 test11 test12
			  test13 test14 test15 test16
			  test17 test18 test19 test20
			  test21 test22 test23 test24
			  test25 test26)))))


