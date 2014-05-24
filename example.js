// this simple example shows how to create a tone with a sine wave

var master = new webkitAudioContext();

var jsynth = require('./')
;

// this is where your math/magic goes
var sineGenerator = function (){
  var tau = Math.PI * 2
  return function(time){
    return Math.sin(time * tau * 440)
  }
}

var synth = jsynth(master, sineGenerator, 4096);

synth.connect(master.destination)

setTimeout(function(){
  var Generator = function (){
    var tau = Math.PI * 2
    return function(time){
      return Math.sin(time * tau * 440 * 2)
    }
  }
  synth.update(Generator)
}, 5000)
