// this is an entry file for use with *browserify* v.2 http://github.com/substack/browserify
// browserify is the breeder's choice for front-end web development the Node.js way
// this repo can also be used with OPA, a web dev heloper tool http://github.com/NHQ/opa
// npm install -g browserify opa
// cd path-to-webaudio/
// opa
//


var webaudio = require('./webaudio')
  , audio = new webkitAudioContext()
  , play = document.getElementById('play')
  , stop = document.getElementById('stop')
  , tau = Math.PI * 2
  , f = 555
;

function sine(time, i, sample){
  return Math.sin(time * tau * f)
}

function gain(time, i, sample){
  return sample * .02
};

var channel1 = webaudio(audio, sine);

var channel2 = webaudio(audio, gain);

play.addEventListener('click', function(){
	channel1.connect(channel2)
	channel2.play() // .connect(audio.destination);

});

stop.addEventListener('click', function(){
	channel1.stop()
});
