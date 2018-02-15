import './home.html';

import '../../components/hello/hello.js';
import '../../components/info/info.js';
import '../../components/hll/hll.js';
import Typed from 'typed.js';

Template.App_home.onRendered(function bodyOnCreated() {

	var options = {
	  strings: ["Quantum Computing", "Quantum Virtual Machine", "Quantum Visualization", "Quantum ..."],
	  typeSpeed: 100,
	  backSpeed: 40,
	  cursorChar: '',
	  shuffle: true,
	  smartBackspace: false,
      loop: true
	}

	var typed = new Typed("#typed", options);
});