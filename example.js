// this simple example shows how to create a tone with a sine wave

var master = new webkitAudioContext();

var jsynth = require('./')
var srcr = require('../iframarfi/src.js')

var code = function(){
  var amod = require('amod')  
  var sineGenerator = function (){
    var tau = Math.PI * 2
    return function(time){
      return Math.sin(time * tau * 440) 
    }
  }
  return sineGenerator
}

var synth = jsynth(master, code, 4096);

synth.connect(master.destination)
