#{javascript}
function f(x,s1)
{
  return function(y, s2) {return eval(s1+s2);}
}

f(5, "y+")(6, "x;")
.

#{javascript}
function f(x,s)
{
  return eval(s);
}

f(55, "x+x;")
.


#{javascript}
function f(o)
{
  var x = 3;
  with(o) {x = 4;}
  print(x)
}

oo = new Object
f(oo)

oo.x = 7
f(oo)
print(oo.x)
.

#{javascript}
function f(o)
{
   with(o) return function(y) {x = y;}
}

oo = new Object
f2 = f(oo)
f2(5)
print(x)

oo.x = 7
f2(6)
print(x)
print(oo.x)
.

#{javascript}
function f(o, str)
{
   with(o) return function(y) {eval(str);}
}
oo = new Object
f2 = f(oo, "x=y;")
f2(5)
print(x)

oo.x = 7
f2(6)
print(x)
print(oo.x)
.
