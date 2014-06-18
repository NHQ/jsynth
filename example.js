var master = new webkitAudioContext();

var jsynth = require('./')
var srcr = require('../iframarfi/src.js')

var code = "" + 
  "var tau = Math.PI * 2; " +
  "return function(time){" +
  "  return Math.sin(time * tau * 440)}"

var code2 = "" + 
  "var tau = Math.PI * 2; " +
  "return function(time){" +
  "  return Math.sin(time * tau * 440 * 2)}"

var synth = jsynth(master, code);

synth.connect(master.destination)

setTimeout(function(){synth.update(code2)},5000)
