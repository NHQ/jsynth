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
  return sample * .25		
};

var channel1 = webaudio(audio, sine);

var channel2 = webaudio(audio, gain);

play.addEventListener('click', function(){
	channel1.connect(channel2)
	channel2.play() // .connect(audio.destination);

/*
	var buffer = channel1.createSourceBuffer(48000);
	var source = audio.createSample();
	source.connect(audio.destination)
  source.buffer = buffer
  source.noteOn(0);
*/
 // channel1.record()

});

stop.addEventListener('click', function(){
	channel1.stop()
});
