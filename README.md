CL-Javascript
=============

See [http://marijnhaverbeke.nl/cl-javascript][home] for the project
homepage.

CL-JavaScript is a compiler (translator) aimed to enable scripting
software written in Common Lisp with JavaScript. It is developed by
Alan Pavičić, Marijn Haverbeke (also the author of [parse-js
library][parse]) and Iva Jurišić.

CL-JavaScript is licensed under MIT public license.

[home]: http://marijnhaverbeke.nl/cl-javascript
[parse]: http://marijnhaverbeke.nl/parse-js/

## Testing test262

Install node and npm

Then you can install `test262-harness` npm package
and clone test suite. Guide assumes that you'll clone
into root of this repository.

~~~shell
$ npm install -g test262-harness
$ git clone https://github.com/tc39/test262.git --depth 1
~~~

Once that done you can use shell script runner to test your
cl-javascript against tests.

~~~shell
$ ./run-test262.sh test262/test/language/comments/S7.4_A1_T1.js
~~~

To test full suite it's better to build ros script first and then run the script:

~~~shell
$ ros build roswell/cl-js.ros
$ ./run-test262.sh test262/test/**/*.js
~~~

Have fun!
