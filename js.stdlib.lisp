(in-package :js)

#{javascript}
Array.shift = function(arr) {if(arr.length > 0) return  arr.splice(0,1)[0];}
Array.prototype.shift = function() {return Array.shift(this);}

print("standard library loaded");
.
