# jsynth

Generate audio/DSP with javascript functions in the browser. Compatible with [baudio](https://github.com/substack/baudio) style functions. Works on webkits, including Chrome, and iOS 6 mobile safari (see note below). Maybe Firefox now, but untested.

Pass your audio context, and your DSP function to the constructor.
It will return a Web Audio Script Processing Node, which can be connected to any other Web Audio Node on the graph.

Simple use, generate a tone:
```js
var master = new webkitAudioContext();

var jsynth = require('jsynth')
  , tau = Math.PI * 2
  , frequency = 555
;

var sineGenerator = function (time, index, input){
  return Math.sin(time * tau * frequency)
}

var synth = jsynth(master, sineGenerator); // returns a web audio node

synth.connect(master.destination)

```

Your function will be called with the following arguments:

* Time, in seconds (float)
* Sample index (integer)
* Input sample (float), MONO, this will be zero if there is no input. Use this if you are connecting other web Audio Api Nodes to this one. In the near future this will be a an array, for multiple input channels.

You function should return a float between [-1, 1]. See examples below.

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
opa -n -e example.js 
```
open your browser to http://localhost:11001