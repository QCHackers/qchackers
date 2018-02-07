var math = require('mathjs');

var I =  math.matrix = ([[1.0, 0.0], [0.0, 1.0]])

var X =  math.matrix = ([[0.0, 1.0], [1.0, 0.0]])

var Y =  math.matrix = ([[0.0, math.complex(0.0, -1.0)], [math.complex(0.0, 1.0), 1.0]])

var Z =  math.matrix = ([[1.0, 0.0], [0.0, -1.0]])



console.log(Y)

console.log(math.sqrt(-4))
console.log("Hello World");
