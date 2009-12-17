(in-package :js) ;;todo: make separate package

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

  (test r1 a)
  (test r2 1)
  (test r3 a)
  (test r4 1)
  (test r5 a)
  (test r6 a)
  (test r7 1)
  (test r8 3))


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

  (test r1 3)
  (test r2 4))


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

(afun 100 200)
(test 102 r1)
(test 101 r2)
(test 101 r3)
(test 200 r4)
(test 1 from_inside))

(defun test5 ()
#{javascript}
function adder(n)
{
return function(m) {return n+m;};
}
r1 = adder(1)(2);
.
  (test r1 3))

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
return a(2);
}
.

  (test (fbla 2) 6))

#{javascript}
function f4()
{
var b=12;
return b;
  function b() {return 1;}

}
.
