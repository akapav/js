(in-package :js)

#{javascript}
function Object(val) {return val;}

Array.shift = function(arr) {if(arr.length > 0) return  arr.splice(0,1)[0];}
Array.prototype.shift = function() {return Array.shift(this);}

Array.unshift = function(arr, el)
{
  if(arguments.length > 1) arr.splice(0, 0, el)
  return arr.length
}

Array.prototype.unshift = function(el)
{
  print(arguments.length)
  if(arguments.length) return Array.unshift(this, el);
  return this.length;
}

print("standard library loaded");
.
