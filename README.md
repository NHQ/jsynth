# jsynth-ww

This is jsynth with webworkers, and it is somewhat different because your function needs to return your function, for proper context scoping.  See below.

Use this module to generate audio/DSP with javascript functions in the browser. 

Pass your audio context, and your DSP function to the constructor.  Optionally pass a 3rd param, a buffer size.  See the Web Audio API for about that.  A default is provided for the buffer size.

It will return a Web Audio Script Processing Node, which can be connected to any other Web Audio Node on the graph.

Simple use, generate a tone:
```js
var master = new webkitAudioContext();

var jsynth = require('jsynth-ww')
;
/* 
  the following actually returns the function that will be used for
  audio process.  It is encapsulated in a function for scoped variable definitions.
  This is because the whole thing is going in a webworker scope.
*/
var sineGenerator = function (time, index, input){
  var tau = Math.PI * 2;
  var f = 444
  return function(time){
    return Math.sin(time * tau * f)
  }
}

var synth = jsynth(master, sineGenerator, 4096); // returns a web audio node

synth.connect(master.destination)

```

Your function will be called with the following arguments:

* Time, in seconds (float)
* Sample index (integer)
* Input sample (float), MONO, this will be zero if there is no input. Use this if you are connecting other web Audio Api Nodes to this one.

You function should return a float between [-1, 1]-ish, but don't be afraid to multiply amplitudes. See examples below.

# usage

```bash
npm install jsynth
```

# Mobile Safari
On mobile safari webkit (iOS), you can only initiate web audio API sounds from within a user event context, such as a click.

# Example

to run the example, use [opa](https://github.com/nhq/opa)
```
npm install -g watchify opa
```
(watchify is browserify + file a file watcher) 
Then:
```
git clone git@github.com:NHQ/jsynth.git
cd jsynth
opa -e example.js -o public/bundle.js 
```
open your browser to http://localhost:11001
