// Selected framework code + JavaScript parser from http://marijn.haverbeke.nl/codemirror

function codemirrorBench(str) {
  var total = 0;
  highlightText(str, function(token) {total += token.value.length;});
  if (total != str.length) throw new Error("Invalid output!");  
  else return "OK";
}

var StopIteration = {toString: function() {return "StopIteration"}};

var Editor = {};
var indentUnit = 2;

(function(){
  function normaliseString(string) {
    var tab = "";
    for (var i = 0; i < indentUnit; i++) tab += " ";

    string = string.replace(/\t/g, tab).replace(/\u00a0/g, " ").replace(/\r\n?/g, "\n");
    var pos = 0, parts = [], lines = string.split("\n");
    for (var line = 0; line < lines.length; line++) {
      if (line != 0) parts.push("\n");
      parts.push(lines[line]);
    }

    return {
      next: function() {
        if (pos < parts.length) return parts[pos++];
        else throw StopIteration;
      }
    };
  }

  this.highlightText = function(string, callback, parser) {
    parser = (parser || Editor.Parser).make(stringStream(normaliseString(string)));
    try {
      while (true) callback(parser.next());
    }
    catch (e) {
      if (e != StopIteration) throw e;
    }
  }
})();

/* String streams are the things fed to parsers (which can feed them
 * to a tokenizer if they want). They provide peek and next methods
 * for looking at the current character (next 'consumes' this
 * character, peek does not), and a get method for retrieving all the
 * text that was consumed since the last time get was called.
 *
 * An easy mistake to make is to let a StopIteration exception finish
 * the token stream while there are still characters pending in the
 * string stream (hitting the end of the buffer while parsing a
 * token). To make it easier to detect such errors, the stringstreams
 * throw an exception when this happens.
 */

// Make a stringstream stream out of an iterator that returns strings.
// This is applied to the result of traverseDOM (see codemirror.js),
// and the resulting stream is fed to the parser.
var stringStream = function(source){
  // String that's currently being iterated over.
  var current = "";
  // Position in that string.
  var pos = 0;
  // Accumulator for strings that have been iterated over but not
  // get()-ed yet.
  var accum = "";
  // Make sure there are more characters ready, or throw
  // StopIteration.
  function ensureChars() {
    while (pos == current.length) {
      accum += current;
      current = ""; // In case source.next() throws
      pos = 0;
      try {current = source.next();}
      catch (e) {
        if (e != StopIteration) throw e;
        else return false;
      }
    }
    return true;
  }

  return {
    // Return the next character in the stream.
    peek: function() {
      if (!ensureChars()) return null;
      return current.charAt(pos);
    },
    // Get the next character, throw StopIteration if at end, check
    // for unused content.
    next: function() {
      if (!ensureChars()) {
        if (accum.length > 0)
          throw "End of stringstream reached without emptying buffer ('" + accum + "').";
        else
          throw StopIteration;
      }
      return current.charAt(pos++);
    },
    // Return the characters iterated over since the last call to
    // .get().
    get: function() {
      var temp = accum;
      accum = "";
      if (pos > 0){
        temp += current.slice(0, pos);
        current = current.slice(pos);
        pos = 0;
      }
      return temp;
    },
    // Push a string back into the stream.
    push: function(str) {
      current = current.slice(0, pos) + str + current.slice(pos);
    },
    lookAhead: function(str, consume, skipSpaces, caseInsensitive) {
      function cased(str) {return caseInsensitive ? str.toLowerCase() : str;}
      str = cased(str);
      var found = false;

      var _accum = accum, _pos = pos;
      if (skipSpaces) this.nextWhileMatches(/[\s\u00a0]/);

      while (true) {
        var end = pos + str.length, left = current.length - pos;
        if (end <= current.length) {
          found = str == cased(current.slice(pos, end));
          pos = end;
          break;
        }
        else if (str.slice(0, left) == cased(current.slice(pos))) {
          accum += current; current = "";
          try {current = source.next();}
          catch (e) {break;}
          pos = 0;
          str = str.slice(left);
        }
        else {
          break;
        }
      }

      if (!(found && consume)) {
        current = accum.slice(_accum.length) + current;
        pos = _pos;
        accum = _accum;
      }

      return found;
    },

    // Utils built on top of the above
    more: function() {
      return this.peek() !== null;
    },
    applies: function(test) {
      var next = this.peek();
      return (next !== null && test(next));
    },
    nextWhile: function(test) {
      var next;
      while ((next = this.peek()) !== null && test(next))
        this.next();
    },
    matches: function(re) {
      var next = this.peek();
      return (next !== null && re.test(next));
    },
    nextWhileMatches: function(re) {
      var next;
      while ((next = this.peek()) !== null && re.test(next))
        this.next();
    },
    equals: function(ch) {
      return ch === this.peek();
    },
    endOfLine: function() {
      var next = this.peek();
      return next == null || next == "\n";
    }
  };
};
// A framework for simple tokenizers. Takes care of newlines and
// white-space, and of getting the text from the source stream into
// the token object. A state is a function of two arguments -- a
// string stream and a setState function. The second can be used to
// change the tokenizer's state, and can be ignored for stateless
// tokenizers. This function should advance the stream over a token
// and return a string or object containing information about the next
// token, or null to pass and have the (new) state be called to finish
// the token. When a string is given, it is wrapped in a {style, type}
// object. In the resulting object, the characters consumed are stored
// under the content property. Any whitespace following them is also
// automatically consumed, and added to the value property. (Thus,
// content is the actual meaningful part of the token, while value
// contains all the text it spans.)

function tokenizer(source, state) {
  // Newlines are always a separate token.
  function isWhiteSpace(ch) {
    // The messy regexp is because IE's regexp matcher is of the
    // opinion that non-breaking spaces are no whitespace.
    return ch != "\n" && /^[\s\u00a0]*$/.test(ch);
  }

  var tokenizer = {
    state: state,

    take: function(type) {
      if (typeof(type) == "string")
        type = {style: type, type: type};

      type.content = (type.content || "") + source.get();
      if (!/\n$/.test(type.content))
        source.nextWhile(isWhiteSpace);
      type.value = type.content + source.get();
      return type;
    },

    next: function () {
      if (!source.more()) throw StopIteration;

      var type;
      if (source.equals("\n")) {
        source.next();
        return this.take("whitespace");
      }
      
      if (source.applies(isWhiteSpace))
        type = "whitespace";
      else
        while (!type)
          type = this.state(source, function(s) {tokenizer.state = s;});

      return this.take(type);
    }
  };
  return tokenizer;
}
/* Tokenizer for JavaScript code */

var tokenizeJavaScript = (function() {
  // Advance the stream until the given character (not preceded by a
  // backslash) is encountered, or the end of the line is reached.
  function nextUntilUnescaped(source, end) {
    var escaped = false;
    while (!source.endOfLine()) {
      var next = source.next();
      if (next == end && !escaped)
        return false;
      escaped = !escaped && next == "\\";
    }
    return escaped;
  }

  // A map of JavaScript's keywords. The a/b/c keyword distinction is
  // very rough, but it gives the parser enough information to parse
  // correct code correctly (we don't care that much how we parse
  // incorrect code). The style information included in these objects
  // is used by the highlighter to pick the correct CSS style for a
  // token.
  var keywords = function(){
    function result(type, style){
      return {type: type, style: "js-" + style};
    }
    // keywords that take a parenthised expression, and then a
    // statement (if)
    var keywordA = result("keyword a", "keyword");
    // keywords that take just a statement (else)
    var keywordB = result("keyword b", "keyword");
    // keywords that optionally take an expression, and form a
    // statement (return)
    var keywordC = result("keyword c", "keyword");
    var operator = result("operator", "keyword");
    var atom = result("atom", "atom");
    return {
      "if": keywordA, "while": keywordA, "with": keywordA,
      "else": keywordB, "do": keywordB, "try": keywordB, "finally": keywordB,
      "return": keywordC, "break": keywordC, "continue": keywordC, "new": keywordC, "delete": keywordC, "throw": keywordC,
      "in": operator, "typeof": operator, "instanceof": operator,
      "var": result("var", "keyword"), "function": result("function", "keyword"), "catch": result("catch", "keyword"),
      "for": result("for", "keyword"), "switch": result("switch", "keyword"),
      "case": result("case", "keyword"), "default": result("default", "keyword"),
      "true": atom, "false": atom, "null": atom, "undefined": atom, "NaN": atom, "Infinity": atom
    };
  }();

  // Some helper regexps
  var isOperatorChar = /[+\-*&%=<>!?|]/;
  var isHexDigit = /[0-9A-Fa-f]/;
  var isWordChar = /[\w\$_]/;

  // Wrapper around jsToken that helps maintain parser state (whether
  // we are inside of a multi-line comment and whether the next token
  // could be a regular expression).
  function jsTokenState(inside, regexp) {
    return function(source, setState) {
      var newInside = inside;
      var type = jsToken(inside, regexp, source, function(c) {newInside = c;});
      var newRegexp = type.type == "operator" || type.type == "keyword c" || type.type.match(/^[\[{}\(,;:]$/);
      if (newRegexp != regexp || newInside != inside)
        setState(jsTokenState(newInside, newRegexp));
      return type;
    };
  }

  // The token reader, intended to be used by the tokenizer from
  // tokenize.js (through jsTokenState). Advances the source stream
  // over a token, and returns an object containing the type and style
  // of that token.
  function jsToken(inside, regexp, source, setInside) {
    function readHexNumber(){
      source.next(); // skip the 'x'
      source.nextWhileMatches(isHexDigit);
      return {type: "number", style: "js-atom"};
    }

    function readNumber() {
      source.nextWhileMatches(/[0-9]/);
      if (source.equals(".")){
        source.next();
        source.nextWhileMatches(/[0-9]/);
      }
      if (source.equals("e") || source.equals("E")){
        source.next();
        if (source.equals("-"))
          source.next();
        source.nextWhileMatches(/[0-9]/);
      }
      return {type: "number", style: "js-atom"};
    }
    // Read a word, look it up in keywords. If not found, it is a
    // variable, otherwise it is a keyword of the type found.
    function readWord() {
      source.nextWhileMatches(isWordChar);
      var word = source.get();
      var known = keywords.hasOwnProperty(word) && keywords.propertyIsEnumerable(word) && keywords[word];
      return known ? {type: known.type, style: known.style, content: word} :
      {type: "variable", style: "js-variable", content: word};
    }
    function readRegexp() {
      nextUntilUnescaped(source, "/");
      source.nextWhileMatches(/[gi]/);
      return {type: "regexp", style: "js-string"};
    }
    // Mutli-line comments are tricky. We want to return the newlines
    // embedded in them as regular newline tokens, and then continue
    // returning a comment token for every line of the comment. So
    // some state has to be saved (inside) to indicate whether we are
    // inside a /* */ sequence.
    function readMultilineComment(start){
      var newInside = "/*";
      var maybeEnd = (start == "*");
      while (true) {
        if (source.endOfLine())
          break;
        var next = source.next();
        if (next == "/" && maybeEnd){
          newInside = null;
          break;
        }
        maybeEnd = (next == "*");
      }
      setInside(newInside);
      return {type: "comment", style: "js-comment"};
    }
    function readOperator() {
      source.nextWhileMatches(isOperatorChar);
      return {type: "operator", style: "js-operator"};
    }
    function readString(quote) {
      var endBackSlash = nextUntilUnescaped(source, quote);
      setInside(endBackSlash ? quote : null);
      return {type: "string", style: "js-string"};
    }

    // Fetch the next token. Dispatches on first character in the
    // stream, or first two characters when the first is a slash.
    if (inside == "\"" || inside == "'")
      return readString(inside);
    var ch = source.next();
    if (inside == "/*")
      return readMultilineComment(ch);
    else if (ch == "\"" || ch == "'")
      return readString(ch);
    // with punctuation, the type of the token is the symbol itself
    else if (/[\[\]{}\(\),;\:\.]/.test(ch))
      return {type: ch, style: "js-punctuation"};
    else if (ch == "0" && (source.equals("x") || source.equals("X")))
      return readHexNumber();
    else if (/[0-9]/.test(ch))
      return readNumber();
    else if (ch == "/"){
      if (source.equals("*"))
      { source.next(); return readMultilineComment(ch); }
      else if (source.equals("/"))
      { nextUntilUnescaped(source, null); return {type: "comment", style: "js-comment"};}
      else if (regexp)
        return readRegexp();
      else
        return readOperator();
    }
    else if (isOperatorChar.test(ch))
      return readOperator();
    else
      return readWord();
  }

  // The external interface to the tokenizer.
  return function(source, startState) {
    return tokenizer(source, startState || jsTokenState(false, true));
  };
})();
/* Parse function for JavaScript. Makes use of the tokenizer from
 * tokenizejavascript.js. Note that your parsers do not have to be
 * this complicated -- if you don't want to recognize local variables,
 * in many languages it is enough to just look for braces, semicolons,
 * parentheses, etc, and know when you are inside a string or comment.
 *
 * See manual.html for more info about the parser interface.
 */

var JSParser = Editor.Parser = (function() {
  // Token types that can be considered to be atoms.
  var atomicTypes = {"atom": true, "number": true, "variable": true, "string": true, "regexp": true};
  // Setting that can be used to have JSON data indent properly.
  var json = false;
  // Constructor for the lexical context objects.
  function JSLexical(indented, column, type, align, prev, info) {
    // indentation at start of this line
    this.indented = indented;
    // column at which this scope was opened
    this.column = column;
    // type of scope ('vardef', 'stat' (statement), 'form' (special form), '[', '{', or '(')
    this.type = type;
    // '[', '{', or '(' blocks that have any text after their opening
    // character are said to be 'aligned' -- any lines below are
    // indented all the way to the opening character.
    if (align != null)
      this.align = align;
    // Parent scope, if any.
    this.prev = prev;
    this.info = info;
  }

  // My favourite JavaScript indentation rules.
  function indentJS(lexical) {
    return function(firstChars) {
      var firstChar = firstChars && firstChars.charAt(0), type = lexical.type;
      var closing = firstChar == type;
      if (type == "vardef")
        return lexical.indented + 4;
      else if (type == "form" && firstChar == "{")
        return lexical.indented;
      else if (type == "stat" || type == "form")
        return lexical.indented + indentUnit;
      else if (lexical.info == "switch" && !closing)
        return lexical.indented + (/^(?:case|default)\b/.test(firstChars) ? indentUnit : 2 * indentUnit);
      else if (lexical.align)
        return lexical.column - (closing ? 1 : 0);
      else
        return lexical.indented + (closing ? 0 : indentUnit);
    };
  }

  // The parser-iterator-producing function itself.
  function parseJS(input, basecolumn) {
    // Wrap the input in a token stream
    var tokens = tokenizeJavaScript(input);
    // The parser state. cc is a stack of actions that have to be
    // performed to finish the current statement. For example we might
    // know that we still need to find a closing parenthesis and a
    // semicolon. Actions at the end of the stack go first. It is
    // initialized with an infinitely looping action that consumes
    // whole statements.
    var cc = [json ? expressions : statements];
    // Context contains information about the current local scope, the
    // variables defined in that, and the scopes above it.
    var context = null;
    // The lexical scope, used mostly for indentation.
    var lexical = new JSLexical((basecolumn || 0) - indentUnit, 0, "block", false);
    // Current column, and the indentation at the start of the current
    // line. Used to create lexical scope objects.
    var column = 0;
    var indented = 0;
    // Variables which are used by the mark, cont, and pass functions
    // below to communicate with the driver loop in the 'next'
    // function.
    var consume, marked;
  
    // The iterator object.
    var parser = {next: next, copy: copy};

    function next(){
      // Start by performing any 'lexical' actions (adjusting the
      // lexical variable), or the operations below will be working
      // with the wrong lexical state.
      while(cc[cc.length - 1].lex)
        cc.pop()();

      // Fetch a token.
      var token = tokens.next();

      // Adjust column and indented.
      if (token.type == "whitespace" && column == 0)
        indented = token.value.length;
      column += token.value.length;
      if (token.content == "\n"){
        indented = column = 0;
        // If the lexical scope's align property is still undefined at
        // the end of the line, it is an un-aligned scope.
        if (!("align" in lexical))
          lexical.align = false;
        // Newline tokens get an indentation function associated with
        // them.
        token.indentation = indentJS(lexical);
      }
      // No more processing for meaningless tokens.
      if (token.type == "whitespace" || token.type == "comment")
        return token;
      // When a meaningful token is found and the lexical scope's
      // align is undefined, it is an aligned scope.
      if (!("align" in lexical))
        lexical.align = true;

      // Execute actions until one 'consumes' the token and we can
      // return it.
      while(true) {
        consume = marked = false;
        // Take and execute the topmost action.
        cc.pop()(token.type, token.content);
        if (consume){
          // Marked is used to change the style of the current token.
          if (marked)
            token.style = marked;
          // Here we differentiate between local and global variables.
          else if (token.type == "variable" && inScope(token.content))
            token.style = "js-localvariable";
          return token;
        }
      }
    }

    // This makes a copy of the parser state. It stores all the
    // stateful variables in a closure, and returns a function that
    // will restore them when called with a new input stream. Note
    // that the cc array has to be copied, because it is contantly
    // being modified. Lexical objects are not mutated, and context
    // objects are not mutated in a harmful way, so they can be shared
    // between runs of the parser.
    function copy(){
      var _context = context, _lexical = lexical, _cc = cc.concat([]), _tokenState = tokens.state;
  
      return function copyParser(input){
        context = _context;
        lexical = _lexical;
        cc = _cc.concat([]); // copies the array
        column = indented = 0;
        tokens = tokenizeJavaScript(input, _tokenState);
        return parser;
      };
    }

    // Helper function for pushing a number of actions onto the cc
    // stack in reverse order.
    function push(fs){
      for (var i = fs.length - 1; i >= 0; i--)
        cc.push(fs[i]);
    }
    // cont and pass are used by the action functions to add other
    // actions to the stack. cont will cause the current token to be
    // consumed, pass will leave it for the next action.
    function cont(){
      push(arguments);
      consume = true;
    }
    function pass(){
      push(arguments);
      consume = false;
    }
    // Used to change the style of the current token.
    function mark(style){
      marked = style;
    }

    // Push a new scope. Will automatically link the current scope.
    function pushcontext(){
      context = {prev: context, vars: {"this": true, "arguments": true}};
    }
    // Pop off the current scope.
    function popcontext(){
      context = context.prev;
    }
    // Register a variable in the current scope.
    function register(varname){
      if (context){
        mark("js-variabledef");
        context.vars[varname] = true;
      }
    }
    // Check whether a variable is defined in the current scope.
    function inScope(varname){
      var cursor = context;
      while (cursor) {
        if (cursor.vars[varname])
          return true;
        cursor = cursor.prev;
      }
      return false;
    }
  
    // Push a new lexical context of the given type.
    function pushlex(type, info) {
      var result = function(){
        lexical = new JSLexical(indented, column, type, null, lexical, info)
      };
      result.lex = true;
      return result;
    }
    // Pop off the current lexical context.
    function poplex(){
      lexical = lexical.prev;
    }
    poplex.lex = true;
    // The 'lex' flag on these actions is used by the 'next' function
    // to know they can (and have to) be ran before moving on to the
    // next token.
  
    // Creates an action that discards tokens until it finds one of
    // the given type.
    function expect(wanted){
      return function expecting(type){
        if (type == wanted) cont();
        else cont(arguments.callee);
      };
    }

    // Looks for a statement, and then calls itself.
    function statements(type){
      return pass(statement, statements);
    }
    function expressions(type){
      return pass(expression, expressions);
    }
    // Dispatches various types of statements based on the type of the
    // current token.
    function statement(type){
      if (type == "var") cont(pushlex("vardef"), vardef1, expect(";"), poplex);
      else if (type == "keyword a") cont(pushlex("form"), expression, statement, poplex);
      else if (type == "keyword b") cont(pushlex("form"), statement, poplex);
      else if (type == "{") cont(pushlex("}"), block, poplex);
      else if (type == "function") cont(functiondef);
      else if (type == "for") cont(pushlex("form"), expect("("), pushlex(")"), forspec1, expect(")"), poplex, statement, poplex);
      else if (type == "variable") cont(pushlex("stat"), maybelabel);
      else if (type == "switch") cont(pushlex("form"), expression, pushlex("}", "switch"), expect("{"), block, poplex, poplex);
      else if (type == "case") cont(expression, expect(":"));
      else if (type == "default") cont(expect(":"));
      else if (type == "catch") cont(pushlex("form"), pushcontext, expect("("), funarg, expect(")"), statement, poplex, popcontext);
      else pass(pushlex("stat"), expression, expect(";"), poplex);
    }
    // Dispatch expression types.
    function expression(type){
      if (atomicTypes.hasOwnProperty(type)) cont(maybeoperator);
      else if (type == "function") cont(functiondef);
      else if (type == "keyword c") cont(expression);
      else if (type == "(") cont(pushlex(")"), expression, expect(")"), poplex, maybeoperator);
      else if (type == "operator") cont(expression);
      else if (type == "[") cont(pushlex("]"), commasep(expression, "]"), poplex, maybeoperator);
      else if (type == "{") cont(pushlex("}"), commasep(objprop, "}"), poplex, maybeoperator);
      else cont();
    }
    // Called for places where operators, function calls, or
    // subscripts are valid. Will skip on to the next action if none
    // is found.
    function maybeoperator(type){
      if (type == "operator") cont(expression);
      else if (type == "(") cont(pushlex(")"), expression, commasep(expression, ")"), poplex, maybeoperator);
      else if (type == ".") cont(property, maybeoperator);
      else if (type == "[") cont(pushlex("]"), expression, expect("]"), poplex, maybeoperator);
    }
    // When a statement starts with a variable name, it might be a
    // label. If no colon follows, it's a regular statement.
    function maybelabel(type){
      if (type == ":") cont(poplex, statement);
      else pass(maybeoperator, expect(";"), poplex);
    }
    // Property names need to have their style adjusted -- the
    // tokenizer thinks they are variables.
    function property(type){
      if (type == "variable") {mark("js-property"); cont();}
    }
    // This parses a property and its value in an object literal.
    function objprop(type){
      if (type == "variable") mark("js-property");
      if (atomicTypes.hasOwnProperty(type)) cont(expect(":"), expression);
    }
    // Parses a comma-separated list of the things that are recognized
    // by the 'what' argument.
    function commasep(what, end){
      function proceed(type) {
        if (type == ",") cont(what, proceed);
        else if (type == end) cont();
        else cont(expect(end));
      }
      return function commaSeparated(type) {
        if (type == end) cont();
        else pass(what, proceed);
      };
    }
    // Look for statements until a closing brace is found.
    function block(type){
      if (type == "}") cont();
      else pass(statement, block);
    }
    // Variable definitions are split into two actions -- 1 looks for
    // a name or the end of the definition, 2 looks for an '=' sign or
    // a comma.
    function vardef1(type, value){
      if (type == "variable"){register(value); cont(vardef2);}
      else cont();
    }
    function vardef2(type, value){
      if (value == "=") cont(expression, vardef2);
      else if (type == ",") cont(vardef1);
    }
    // For loops.
    function forspec1(type){
      if (type == "var") cont(vardef1, forspec2);
      else if (type == ";") pass(forspec2);
      else if (type == "variable") cont(formaybein);
      else pass(forspec2);
    }
    function formaybein(type, value){
      if (value == "in") cont(expression);
      else cont(maybeoperator, forspec2);
    }
    function forspec2(type, value){
      if (type == ";") cont(forspec3);
      else if (value == "in") cont(expression);
      else cont(expression, expect(";"), forspec3);
    }
    function forspec3(type) {
      if (type == ")") pass();
      else cont(expression);
    }
    // A function definition creates a new context, and the variables
    // in its argument list have to be added to this context.
    function functiondef(type, value){
      if (type == "variable"){register(value); cont(functiondef);}
      else if (type == "(") cont(pushcontext, commasep(funarg, ")"), statement, popcontext);
    }
    function funarg(type, value){
      if (type == "variable"){register(value); cont();}
    }
  
    return parser;
  }

  return {
    make: parseJS,
    electricChars: "{}:",
    configure: function(obj) {
      if (obj.json != null) json = obj.json;
    }
  };
})();
