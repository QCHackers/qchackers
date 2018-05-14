import './hll-viz.html';

import Detector from '../../../startup/client/Detector.js';
import THREE  from '../../../startup/client/three.min.js';

var seqStack = [];
var next = true;



Template.hllviz.onRendered(function () {

	var scene = new THREE.Scene();
	let WIDTH = window.innerWidth ;
	var camera = new THREE.PerspectiveCamera( 45, WIDTH / window.innerHeight, 1, 2000 );
	var rotateCam = true;
	var animating = false;
	var clock = new THREE.Clock();

	renderer = new THREE.WebGLRenderer( { antialias: true } );

	renderer.setSize( WIDTH, window.innerHeight );

	container = document.getElementById( 'canvas' );
	document.body.appendChild( container );
	container.appendChild( renderer.domElement );
		
	// Bloch Sphere	
	initSphere();

	initAxis();

	var origin = new THREE.Vector3( 0, 0, 0 );
	var currPos = new THREE.Vector3(0, 1, 0);
	var arrow = new THREE.ArrowHelper(currPos, origin, 1, 0xffff00, 0.2, 0.1);
	scene.add(arrow);

	var targetPos = new THREE.Vector3(0, 1, 0);


	camera.position.set(4,4,4);
	camera.lookAt(new THREE.Vector3(0,0,0));

	var backTime = 0;

	function onDocumentMouseMove( event ) {
		rotateCam = !rotateCam;
		if(rotateCam){
			clock.start();
		}else{
			clock.stop();
			backTime += clock.getElapsedTime();
		}
	}


	function interesting(){
		if(seqStack.length && next){
			targetPos = seqStack[0][0];
		}else{ // Stack is empty, no updates need to be applied
			return
		}

		if(currPos.angleTo(targetPos) > 0.2){
			let dir = currPos;
			//normalize the direction vector (convert to vector of length 1)
			let axis = new THREE.Vector3(0,0,0);
			axis.crossVectors ( currPos, targetPos )
			currPos.applyAxisAngle(axis, 0.2)

			dir.normalize();
			arrow.setDirection(dir);
			
		}else{

			let dir = targetPos;
			dir.normalize();
			arrow.setDirection(dir);
			updateStatus('Completed: '+ seqStack[0][1])
			seqStack.shift();
			next = false;
		}
	}

	function isEqual(v1, v2){
		let bool = (v1.x.toFixed(1) == v2.x.toFixed(1)) && (v1.y.toFixed(1) == v2.y.toFixed(1)) && (v1.z.toFixed(1) == v2.z.toFixed(1))
		if (bool){
			// console.log("IS EQUAL")	
		}
		return bool
	}

	// document.addEventListener( 'click', onDocumentMouseMove, false );

	var i = 0;
	function animate() {
		requestAnimationFrame( animate );
		
		var elapsedTime = (clock.getElapsedTime() + backTime) * 0.1 ;
		interesting()
		if(rotateCam){
			i += 1;
			camera.position.x = 5 * Math.cos( elapsedTime );         
			camera.position.z = 5 * Math.sin( elapsedTime );
			// camera.position.y = 1 * Math.cos( elapsedTime );
			camera.lookAt( new THREE.Vector3(0,0,0) );
			
		}
		renderer.render( scene, camera );
	}
	animate();

	function initAxis(){
		// (Y, Z, X )
		let origin = new THREE.Vector3( 0, 0, 0 );

		let x_dir = new THREE.Vector3( 1, 0, 0 );
		let x_axis = new THREE.ArrowHelper( x_dir, origin, 2, 0xff0000, 0.1, 0.05 ); // RED - Y-axis
		scene.add( x_axis )

		let y_dir = new THREE.Vector3( 0, 1, 0 );
		let y_axis = new THREE.ArrowHelper( y_dir, origin, 2, 0x46e294, 0.1, 0.05 ); // GREEN - Z-axis
		scene.add( y_axis )

		let z_dir = new THREE.Vector3( 0, 0, 1 );
		let z_axis = new THREE.ArrowHelper( z_dir, origin, 2, 0x7485e8, 0.1, 0.05 ); // BLUE - X-axis
		scene.add( z_axis )
	}

	function initSphere(){
		var geometry = new THREE.SphereBufferGeometry( 1, 20, 20 );
		var edges = new THREE.EdgesGeometry( geometry );
		var line = new THREE.LineSegments( edges, new THREE.LineBasicMaterial( { color: 0xFFFFFF, transparent: true, opacity: 0.4 } ) );
		scene.add( line );
	}
	

}); // Closing onRendered

function updateStatus(text){
	document.getElementById("status").innerHTML = text;
}

Template.hllviz.events({
  'click #hide-button'(event, instance) {
  		let curr = document.getElementById("not-canvas")
  		
	    if(curr.style.visibility == 'hidden'){
	    	curr.style.visibility = 'visible';
	    	
	    }else{
	    	curr.style.visibility = 'hidden';
	    }
    },
   'click #next-button'(event, instance){
	   	next = true;


  //  		var plusOrMinus = Math.random() < 0.5 ? -1 : 1;
  // 		var r1 = Math.random() * plusOrMinus
  // 		plusOrMinus = Math.random() < 0.5 ? -1 : 1;
  // 		var r2 = Math.random() * plusOrMinus
  // 		plusOrMinus = Math.random() < 0.5 ? -1 : 1;
  // 		var r3 = Math.random() * plusOrMinus

  // 		let newVect = new THREE.Vector3(r1, r2, r3);
		// seqStack.push([newVect.normalize(), "H-Gate"]);
   		
   		if(seqStack.length){
   			updateStatus("Running: " + seqStack[0][1])
   		}else{
   			updateStatus('Program completed. <a href="/">Learn more.</a>')
   		}
   },
  'click button'(event, instance){
  	let val = document.getElementById("myTextArea").value
  	if(val == ""){
  		alert('Yo! You forgot to enter some code. We will let it slide this time...')
  	}else{
  		// FOR TESTING
  // 		var plusOrMinus = Math.random() < 0.5 ? -1 : 1;
  // 		var r1 = Math.random() * plusOrMinus
  // 		plusOrMinus = Math.random() < 0.5 ? -1 : 1;
  // 		var r2 = Math.random() * plusOrMinus
  // 		plusOrMinus = Math.random() < 0.5 ? -1 : 1;
  // 		var r3 = Math.random() * plusOrMinus

  // 		let newVect = new THREE.Vector3(r1, r2, r3);
  // 		newVect.normalize()
		// seqStack.push([newVect.normalize(), "H-Gate"]);

		// newVect = new THREE.Vector3(-4, 2, -2);
		// seqStack.push([newVect.normalize(), "Y-Gate"]);

		// newVect = new THREE.Vector3(-4, -2, 2);
		// seqStack.push([newVect.normalize(), "X-Gate"]);
  		
  		Meteor.call('postCode', val, function(err,response) {
			if(err) {
				console.log(err)
				return;
			}else{
				console.log('response')
				console.log(response.content)
				obj = JSON.parse(response.content)
				console.log(obj);

				let newVect;
				for (i = 0; i < obj.spin.length; i++) { 
				    newVect = new THREE.Vector3(obj.spin[i][1], obj.spin[i][2], obj.spin[i][0]);
				    seqStack.push([newVect.normalize(), obj.callHist[i]]);

				}
				console.log("output")
				console.log(obj.output)
				document.getElementById("output").innerHTML = obj.output;
				console.log(seqStack)
				// let newVect = new THREE.Vector3(4, 2, -2);
				// seqStack.push([newVect.normalize(), "H-Gate"]);

				// newVect = new THREE.Vector3(-4, 2, -2);
				// seqStack.push([newVect.normalize(), "Y-Gate"]);

				// newVect = new THREE.Vector3(-4, -2, 2);
				// seqStack.push([newVect.normalize(), "X-Gate"]);

				document.getElementById("not-canvas").style.visibility = 'hidden';
				document.getElementById("hide-button").style.visibility = 'visible';
				document.getElementById("next-button").style.visibility = 'visible';
				updateStatus('Click next to run: '+ seqStack[0][1])
				
			}
			
		});
  			
  	}
  	// console.log(val);
  }
});