module.exports = function(){
  var amod = require('amod')  
  var sineGenerator = function (){
    var tau = Math.PI * 2
    return function(time){
      return Math.sin(time * tau * 440) 
    }
  }
  return sineGenerator
}
