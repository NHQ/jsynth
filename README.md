# webaudio

Generate audio streams with functions in the browser. Compatible with [baudio](https://github.com/substack/baudio) functions. Works on iOS 6 mobile safari (see note below).

Your function will be called with the following arguments:

*Time (float)*
*Sample index* (integer)
*Input sample* (float), this will be zero if there is no input. See connecting nodes below.
// Midi commands: this is not implemented.
// Fundamental Frequency: this is not implemented either, just an idea for cases such as effects node chains
// Data Object : also not implemented.

As a helper, this module will write _SAMPLERATE on the window. You cannot change sampleRates in the web audio api as of yet. I tried writing a mock downsampler into webaudio, but it did not sound right. 

You function should return a float between [-1, 1]. See examples below.

To play around with it, do

```bash
npm install -g browserify opa
git clone https://github.com/NHQ/webaudio
cd webaudio
opa
```

Open your browser to http://localhost:11001
Edit entry.js and refresh

see also [opa](https://github.com/NHQ/opa)

# usage

```bash
npm install webaudio
```

Simple use:
```js
var webaudio = require('webaudio')
  , tau = Math.PI * 2
  , frequency = 555
;

function sine(time, i){
  return Math.sin(time * tau * frequency)
}

var channel = webaudio(sine);

channel.play()
// later, channel.stop()

```
For more advanced use, pass an audioContext as the first argument. You then use the same audio context to connect and use other webaudio nodes/functions, or other HTML5 Web Audio API nodes. This module just returns a wrapped up javascript node, which you can connect to other nodes, or, to the final audioContext.destination:

```js
var webaudio = require('webaudio')
  , tau = Math.PI * 2
  , frequency = 555
  , context = new webkitAudioContext();

function sine(time, i){
  return Math.sin(time * tau * frequency)
}

function gain(time, i, inputSample){
  return inputSample * 1 / 4 
}

var signal = webaudio(context, sine)
var gain = webaudio(context, gain)

signal.connect(gain);

path.connect(context.destination)

/*
setTimeout(function(){
  path.disconnect(); 	// path.stop()
  path.connect(recorder);
  recorder.connect(repeater);
  repeater.connect(audio.destination)
}, 1000)
*/
```

## Mobile Safari
*Note, on mobile safari webkit (iOS), you can only initiate web audio API sounds from within a user event context (at least the fist time...?). Ergo, channel.play(), or channel.connect(context.destination) must be called from inside an event callback.*

#Methods

## channel.play()
Start playing the sound. This connects the node to the master contexts destination for you.

## channel.stop()
Disconnects the node from the master

## channel.createSample(duration)
This helper method will run your function for as long as **duration**, in *samples*.
ie, 48000 =~ one second, depending on your sound card
It returns an [audioBuffer node](http://www.w3.org/TR/webaudio/#AudioBuffer),
which are what you use for precisely timed samples and loops. 
So you could for instance write a script that writes 1 second samples for every note value, through your synth function, and the buttons to trigger those samples.











