// this simple example shows how to create a tone with a sine wave

var master = new webkitAudioContext();

var jsynth = require('./')
  , tau = Math.PI * 2
;

// this is where your math/magic goes
var sineGenerator = function (time){
  return Math.sin(time * tau * 440)
}

var synth = jsynth(master, sineGenerator);

synth.connect(master.destination)

console.log(master, synth)
