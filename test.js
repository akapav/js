function $eq(a, b) {
  if (a != b)
    throw "$eq(" + a + ", " + b + ") failed";
}
function $arrEq(a, b) {
  var mismatch = a.length != b.length;
  if (!mismatch) {
    for (var i = 0; i < a.length; i++)
      if (a[i] != b[i]) mismatch = true;
  }
  if (mismatch)
    throw "$arrEq(" + a + ", " + b + ") failed";
}

function test_1() {
  this.a = new Object;
  a.a = 1;
  function f(o) {return o.a;}
  $eq(f(this), a);
  $eq(f(a), 1);

  function f2() {return this.a;}
  $eq(f2(), a);

  a.f3 = f2;
  $eq(a.f3(), 1);

  function f4() {return f(this);}
  $eq(f4(), a);
  topf4 = f4;
  $eq(this.topf4(), a);
  a.f5 = f4;
  $eq(a.f5(), 1);

  var fifi = 3;
  a.fifi = 4;
  a.f = function () {return fifi;};
  $eq(a.f(), 3);
}

function test_2() {
  function f33() {
    var v = {v1: {v2: 4}};
    this.y = v.v1.v2;
  }
  f33.prototype = {x: 3};

  b = new f33();
  $eq(b.x, 3);
  $eq(b.y, 4);
}

function test_3() {
  function fib(n) {
    if (n < 2) return 1;
    return fib(n - 1) + fib(n - 2);
  }
  function ffib(n) {
    var s1 = 1, s2 = 1, res = 1;
    while(n > 1) {
      res = s1 + s2;
      s1 = s2;
      s2 = res;
      n--;
    }
    return res;
  }

  $eq(ffib(14), 610);
  for (var i = 0; i < 10; i++)
    $eq(fib(i), ffib(i));
}

function test_4() {
  function afun(a) {
    from_inside = 1;
    arguments[0] = a + 1;
    $eq(a + 1, 102);
    $eq(a, 101);
    $eq(arguments[0], 101);
    $eq(arguments[1], 200);
  }
  afun(100, 200);
  $eq(from_inside, 1);
}

function test_5() {
  function adder(n){
    return function(m) {return n+m;};
  }
  $eq(adder(1)(2), 3);
}

function test_6() {
  function fbla(x) {
    var a = function f1(n) {return b(n);};
    var b = function f2(n, m) {
      if (m) return n+n;
      return f2(n + 1, 5);
    };
    return a(x);
  }
  $eq(fbla(2), 6);
}

function test_7() {
  var s = 0;
  for (var i = 0; i < 10; i = i + 1)
    s = s + 1;
  $eq(s, 10);

  s = 0;
  for (var i = 0;; i = i + 1) {
    s = s + i;
    if(s > 100) break;
  }
  $eq(i, 14);

  s = 0;
  for (i = 0; i < 10; i = i +1 ) {
    if(i%2) continue;
    s = s + i;
  }
  $eq(s, 20);

  var a = 100;
  while (a) {a = a - 1;}
  $eq(a, 0);

  a = 100;
  do {a = a - 1;} while (a > 0);
  $eq(a, 0);

  do a = a - 1;
  while (a > 0);
  $eq(a, -1);
}

function test_8() {
  function O(){}
  O.prototype.f = function (){return this.n + 1;}
  var obj = new O;
  obj.n = 20;
  $eq(obj.f(), 21);
}

function test_9() {
  a: for (;;) {for (;;) break a;}
  b: for (var i = 0; i < 10 ; i = i + 1) {for (;;) continue b;}
}

function test_10() {
  var x10 = null;
  function f1() {
    return function(val) {x10=val;}
  }

  var o10 = new Object;
  o10.x10=2;
  var f=f1();
  with(o10) f(3);
  $eq(o10.x10, 2);
  $eq(x10, 3);
}

function test_11() {
  var o = function () {}
  o.x11="asd";
  function f11x(o) {
    with(o) {return function(val) {x11=val;};}
  }
  var o11 = new Object;
  var f11 = f11x(o11);
  f11(5);
  $eq(x11, 5);
  $eq(o11.x11, undefined);

  o11.x11 = 3;
  f11(4);
  $eq(x11, 5);
  $eq(o11.x11, 4);
}

function test_12() {
  function f12x(x, o) {
    function r() {return x;}
    with(o) return {'a': function (y) {x = y;}, 'b': r};
  }

  var o12 = new Object;
  var f12 = f12x(10, o12);
  f12['a'](100);
  $eq(o12.x, undefined);
  $eq(f12['b'](), 100);
  o12.x = 1;
  f12['a'](200);
  $eq(o12.x, 200);
  $eq(f12['b'](), 100);
}

function test_13() {
  var o13x = {g: function(){return 2;}};
  function f13(o) {
    with(o) {
      function g() {return 1;}
      return g();
    }
  }
  $eq(f13(o13x), 2);
}

function test_14() {
  var r1;
  try {r1 = bflj();}
  catch(e) {r1 = 100;}
  finally {r1 = 2 * r1;}
  $eq(r1, 200);
}

function test_15() {
  function f15(x,s1) {
    return function(y, s2) {return eval(s1+s2);}
  }
  $eq(f15(5, "y+")(6, "x;"), 11);
}


function test_16() {
  function f16(x,s) {
    return eval(s);
  }
  $eq(f16(55, "x+x;"), 110);
}

function test_17() {
  function f17(o) {
    var x = 3;
    with(o) {x = 4;}
    return x
  }
  var oo = new Object
  $eq(f17(oo), 4);
  oo.x = 7;
  $eq(f17(oo), 3);
  $eq(oo.x, 4);
}

function test_18() {
  var x = null;
  function f18(o) {
    with(o) return function(y) {x = y;}
  }
  var oo = new Object;
  f18_1 = f18(oo);
  f18_1(5);
  $eq(x, 5);

  oo.x = 7;
  f18_1(6);
  $eq(x, 5);
  $eq(oo.x, 6);
}

function test_19() {
  function f19(o, str) {
    with(o) return function(y) {eval(str);}
  }
  var oo = new Object
  f19_1 = f19(oo, "x=y;");
  f19_1(5);
  $eq(x, 5);
  oo.x = 7;
  f19_1(6);
  $eq(x, 5);
  $eq(oo.x, 6);
}

function test_20() {
  var s1 = String(123);
  var s2 = new String(456);
  String.prototype.y = 12;
  $eq(s2.y, 12);
  $eq(s2.charAt(1), "5");
  $eq(String.prototype.length, 0);
  $eq(s2.length, 3);
  $eq(s2.length = 555, 555);
  $eq(s2.length, 3);
  $eq(s2.substr("1", 8), "56");
  $eq(s2.substring (0, 100), "456");
}

function test_21() {
  var a = [1, 2, 3];
  $arrEq(a.splice(0, 0), []);
  $arrEq(a, [1, 2, 3]);
  a.splice(0, 0, 100, 200);
  $arrEq(a, [100, 200, 1, 2, 3]);
  a.splice(1, 1000);
  $arrEq(a, [100]);
  a = [1, 2, 3]; a.splice(0, 1);
  $arrEq(a, [2, 3]);
  a = [1, 2, 3]; a.splice(0, 2, 10, 11, 12);
  $arrEq(a, [10, 11, 12, 3]);
  a.splice(0, 4, 8, 9, 10);
  $arrEq(a, [8, 9, 10]);
  a = [1, 2, 3]; a.splice(2, 2);
  $arrEq(a, [1, 2]);
  a = [1, 2, 3]; a.splice(2, 2, 10, 12);
  $arrEq(a, [1, 2, 10, 12]);
  // Commenting this for now. Having these as methods of the
  // constructor seems widespread, but isn't in the standard. I want
  // to focus on the standard first.
  // $arrEq(Array.splice(a, 0, 1), [1]);
  // $arrEq(a, [2, 10, 12]);
}

function test_22() {
  function t22(x, y, str) {
    f(0);
    $eq(x, 1);
    function f(x) {
      eval(str);
      $eq(x, 2);
    }
  }
  t22(1, 2, "x=y;");
}

function test_23() {
  function f23(str) {
    var f23_1 = function() {return 100;};
    eval(str);
    return f23_1();
  }

  $eq(f23("f23_1 = function() {return 5;};"), 5);
  var p = 0;
  try {f23_1();}
  catch(e) {p = 2;}
  $eq(p, 2);
  $eq(f23("f23_2 = function() {return 5;};"), 100);
  $eq(f23_2(), 5);
}

function test_24() {
  $eq(new Function("a", "b", "return a+b;")(2,3), 5);
  $eq(new Function.prototype.constructor("a", "b", "return a+b;")(3,4), 7);
  $eq(new (function(){}).constructor("a", "b", "return a+b;")(4,5), 9);

  $eq(Function, Function.prototype.constructor);
  $eq(Function, (function(){}).constructor);
}

function test_25() {
  $eq(Function.call(1, 'a', 'b' , 'return a*b;')(2,3), 6);
  $eq("abc".charAt.call("def", 1), "e");
  $eq((function(x,y) {return x-y;}).call(new Object,10,5), 5);
}

function test_26() {
  function f26(x,y,z) {
    return x+y*z;
  }
  $eq(f26.apply(new Object, [1,2,3,4]), 7);
  $eq(f26.apply(new Object, new Array(1,2,3,4)), 7);

  function f26_1() {
    return f26.apply(new Object, arguments)
  }
  $eq(f26_1(1,2,3,4), 7);

  var v26 = [7,8,9];
  [1,2,3].splice.apply(v26, [0, 2, 100, 101]);
  $arrEq(v26, [100, 101, 9]);
}

function test_27() {
    o27 = new Object
    $eq(this['o2' + '7'], o27)
    this['o2' + '7'].x=5
    with(o27) {
	$eq(x, 5)
	x = 6
    }
    $eq(o27.x, 6)
}

function test_28() {
  $eq(4 && 5, 5);
  $eq({} && 7, 7);
  $eq(0 && 7, 0);
  $eq(4 || 5, 4);
  $eq(false || 7, 7);
  $eq(null || 7, 7);
  $eq(!0, true);
  $eq(!1, false);

  var x = 0;
  true || (x = 1);
  false && (x = 1);
  $eq(x, 0);
}

function test_29() {
  var a = {a: 10};
  $eq(a.hasOwnProperty("a"), true);
  $eq(a.hasOwnProperty("b"), false);
  $eq(a.hasOwnProperty("toString"), false);
  $eq(a.propertyIsEnumerable("a"), true);
  $eq(a.propertyIsEnumerable("b"), false);
  $eq(a.propertyIsEnumerable("toString"), false);
  $eq(Object.prototype.hasOwnProperty("toString"), true);
  $eq(Object.prototype.propertyIsEnumerable("toString"), false);
}

function test_30() {
  $eq(new Error("foo").message, "foo");
  $eq(new Error("foo").toString, Error.prototype.toString);
  try {throw 1;}
  catch(e) {var err = e;}
  $eq(err, 1);
  try {undefined.prop;}
  catch(e) {$eq(e.toString, TypeError.prototype.toString);}
  try {eval("a b c");}
  catch(e) {$eq(e.toString, SyntaxError.prototype.toString);}
}

function test_31() {
  $eq('xaba'.replace('a', 'c'), 'xcba');
  $eq('xaba'.replace(/a/, 'c'), 'xcba');
  $eq('xaba'.replace(/a/g, 'c'), 'xcbc');
  $eq('xaba'.replace(/a/g, function(){return 33;}), 'x33b33');
  $eq('xdabda'.replace(/d(a)/g, function(full) {return full + 1;}), 'xda1bda1');
  $eq('xdabda'.replace(/d(a)/g, function(full, g1) {return g1;}), 'xaba');
  $eq('xdabda'.replace(/d(a)/g, '\\1'), 'xaba');
  $eq('xdabda'.replace(/d(a)/g, function(full, g1, pos) {return pos;}), 'x1b4');
  $eq('xdabda'.replace(/d(a)/g, function(full, g1, pos, all) {return all;}), 'xxdabdabxdabda');
  $eq('foo'.replace(/f(x)?/, '\\1'), 'oo');
}

function test_32() {
  var re1 = /foo(bar)?/i;
  var re2 = /foobar/gm;
  $eq(re1.ignoreCase, true);
  $eq(re1.multiline, false);
  $eq(re1.global, false);
  $eq(re2.ignoreCase, false);
  $eq(re2.multiline, true);
  $eq(re2.global, true);
  var m = re1.exec("foobar");
  $eq(m.length, 2);
  $eq(m[0], "foobar");
  $eq(m[1], "bar");
  m = re1.exec("foo");
  $eq(m.length, 2);
  $eq(m[0], "foo");
  $eq(m[1], undefined);
  $eq(re2.test("foobar"), true);
  $eq(re2.test("quux"), false);
  $eq(re2("quux"), null);
}

function test_33() {
  var a, b;
  function testcase(x) {
    switch (x) {
    default:
    case 1: a = 1; break;
    case 2: a = 2;
    case 3: b = 3;
      break;
    case 4: a = 100;
    }
  }
  testcase(1);
  $eq(a, 1);
  testcase(2);
  $eq(a, 2);
  $eq(b, 3);
  testcase(4);
  $eq(a, 100);
  $eq(b, 3);
  testcase(5);
  $eq(a, 1);
}

function test_34() {
  function a() {return "wrong"}
  var obj = {u: "right", a: function() {return this.u;}};
  with (obj) {$eq(a(), "right");}
}

function test_35() {
  $eq('3' ^ 4, 7);
  $eq(3 ^ 4, 7);
  $eq('12321312312312' ^ '1512312423423423', 1807226439);
}

function test_36() {
  var base = {a: 10, b: 20}, clone = Object.create(base);
  $eq(clone.a, 10);
  $arrEq(Object.keys(base), Object.keys(clone));
  $eq(Object.keys(base).length, 2);
  $eq(clone.c, undefined);
  base.c = 30;
  $eq(clone.c, 30);
  var clone2 = Object.create(base, {d: 6});
  $eq(clone2.d, 6);
  base.d = 5;
  $eq(clone2.d, 6);
}

function test_37() {
  var data = [1, 2, 3, 4];
  function add(a, b) {return a + b;}
  function conc(a, b) {return a + "" + b;}
  $eq(data.reduce(add), 10);
  $eq(data.reduceRight(add), 10);
  $eq(data.reduce(conc, 0), "01234");
  $eq(data.reduceRight(conc, 5), "54321");
  $eq([].reduce(add, 1), 1);
  $eq([].reduceRight(add, 1), 1);
  try {[].reduce(add); $eq(1, 2);}
  catch(e){$eq(e instanceof TypeError, true);}
  try {[].reduceRight(add); $eq(1, 2);}
  catch(e){$eq(e instanceof TypeError, true);}
}

// This used to confuse the type inferrer
function test_38() {
  function a(){return b();}
  function b(){if (false) return a(); throw 1;}
  try{return a();}catch(e){}
}

function test_39() {
    var x = 10;
    $eq((x === 10) &&
        (function () { var x = 10; return x === 10; })() &&
        (function () { return 10 === 10; })(),
        true);
}

function test_40() {
    var foo = function foo() { return arguments.callee.caller; };
    var bar = function bar() { return foo(); };
    if (typeof bar() != "function") {
        throw "arguments.callee.caller doesn't work";
    }
}

function test_41() {
    $eq(-12 % 7, -5);
}

function test_42() {
  var a = [3];
  $eq(a.unshift(2), 2);
  $arrEq(a, [2, 3]);
  $eq(a.unshift(0, 1), 4);
  $arrEq(a, [0, 1, 2, 3]);

  a = [3, 4];
  $eq(a.shift(), 3);
  $arrEq(a, [4]);
  $eq(a.shift(), 4);
  $arrEq(a, []);
  $eq(a.shift(), undefined);
  $arrEq(a, []);

  $eq(a.unshift(0), 1);
  $arrEq(a, [0]);
  $eq(a.unshift(), 1);
  $arrEq(a, [0]);
}

function runTests() {
  var failures = [];
  var run = 0;
  for (var name in this) {
    if (name.length > 5 && name.substr(0, 5) == "test_") {
      run++;
      try {this[name]();}
      catch (e) {
	failures.push(name + ": " + String(e));
      }
    }
  }
  print(run + " test run...");
  if (failures.length)
    print(failures.length + " failures:\n  " + failures.join("\n  "));
  else
    print("All passed!");
}

runTests();
