import './hll.html'

Template.hll.onCreated(function() {
  // counter starts at 0
  console.log("started hll");
});


Template.hll.events({
  'submit .matrix-hll'(event, instance) {
    // increment the counter when button is clicked
    event.preventDefault();
 
    // Get value from form element
    let matrixA = {
    	'A-1x1': 0,
    	'A-1x2': 0,
    	'A-2x1': 0,
    	'A-2x2': 0,
    	'B-1x1': 0,
    	'B-1x2': 0
    }
    for (var key in matrixA) {
    	matrixA[key] = event.target[key].value
	}
	console.log(matrixA)


  },
});