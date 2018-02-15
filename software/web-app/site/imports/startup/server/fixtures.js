// Fill the DB with example data on startup
var Future = Npm.require('fibers/future');

import { Meteor } from 'meteor/meteor';
import { Links } from '../../api/links/links.js';
import { HTTP } from 'meteor/http'

Meteor.startup(() => {
  // if the Links collection is empty
  Meteor.methods({
    postCode: function (code) {
      // console.log('posting code');
      // console.log(code)
      this.unblock()
      return HTTP.call('POST', 'http://localhost:5000/', {
        params: { 'code': code }
      });
      
    },
  });
});
