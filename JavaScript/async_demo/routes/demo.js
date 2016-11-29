var express = require('express');
var router = express.Router();

var mongoClient = require('mongodb').MongoClient;
var assert = require('assert');

// Connection URL
var url = 'mongodb://localhost:27017/cecs424demo';
// Use connect method to connect to the Server


router.get('/', function(req, res, next) {
    // Connections to a database are asynchronous. They can take a long time, and it's OK for many other things to
    // happen in the software while the connection is opening.
    mongoClient.connect(url, function(err, db) {
        // Likewise, inserting into a database is an asynchronous operation. Any interaction with a db is.
        db.collection('demoCollection').insert({'some_key' : 'some_value'}, function(err, result) {

            // If we wanted to do ANOTHER db operation, we'd need ANOTHER nested anonymous callback function.
            res.send('value inserted into database with ID ' + result.ops[0]._id);
            db.close();

            // Problems:
            // What if I wanted to RETURN something from the outermost function?


            // What if I need to throw an exception?


        });
    });
});

// A different approach using promises.
// This code is not real, it is just a demo of ideas.


Mongo.connect(url)
    .then(insertSomething)
    .then(deleteSomething)
    .then(logStuff);

// What if I have a few promises and I want to wait for all to resolve?

var p1 = checkLoginInformation();
var p2 = downloadAvatarImage();

// Wait for the login and download to finish, then log the results of those operations.
Promise.all([p1, p2])
    .then(function(vals) {
        addToLog(vals[0], vals[1]);
    });



// This style of promises still requires lots of function callbacks. Instead of all the .then() stuff,
// why can't we just declare a variable and have the program pause until the value is resolved?
// That's where the "async/await" pattern comes in.


var asyncLogin = checkLoginInformation(); // this does not return a real value, just the placeholder like a promise.
var asyncDownload = downloadAvatarImage();

// Wait for those two to finish before logging the results.
var loginResult = await asyncLogin;
var downloadResult = await asyncDownload;

var logResult = await addToLog(loginResult, downloadResult);




module.exports = router;
