module.exports = function(){
  var xxx = require('jmao')
  var detect = require('detective')
  var dereq = require('derequire')
  var moduleCache = {}
  var t = 0, i = 0, rate = 44100 
  var _require = require
  require = function(module){
    console.log(module)
    if(module = moduleCache[module]) return _require(module.bundle)
    else return _require(module)
  }
  var outbuf = new Float32Array(4096 * 2 * 2)

  var fn = function(){return outbuf}

  var tick = function (output, input) { // a fill-a-buffer function

    for (var ii = 0; ii < output.length; ii += 1) {

        t = i / rate;

        i += 1;

        output[ii] = fn(t, i, input);

    }

    return output
    
  };

  this.onmessage = function(evt){
    var data = xxx.construct(evt.data)
    switch(data.type){
      case 'data':
        this.postMessage(xxx.deconstruct(tick(outbuf, data.input)))
      break
      case 'function':
        fn = data.fn()
      break;
      case 'config':
      //  ctonsole.log(new Function('return ' + data.fn))
        rate = data.sampleRate || rate
        var modules = detect(data.fn)
        if(modules.length){
          var _fn = data.fn
          var body = {"options":{"standalone":true}, dependencies: {}}
          var cached = []
          var help = {}
          modules.forEach(function(module){
            if(moduleCache[module]) {
              cached.push(module)            
              return
            }
            else{
              body.dependencies[module.split('@')[0]] = module.split('@')[1] || ''
              help[module] = module.split('@')[0]
            }
          })
          if(cached.length == modules.length) {
            // all cached
          }
          else{
            var post = new XMLHttpRequest();
            post.open('POST', 'http://wzrd.in/multi', true)
            post.onload = function(e){
              if(this.status == 200){
                var mods = JSON.parse(this.responseText);
                var names = Object.keys(mods)
                names.forEach(function(name){
                  moduleCache[help[name]] = mods[name]
                })
                console.log(moduleCache.amod.bundle)
                var f = dereq(_fn)
                eval('fn = ' + f + '()')
              }
              else console.log(this.status)
            } 
            post.send(JSON.stringify(body))
          }
          
        }
        else{
          eval('fn = ' + data.fn + '()')
        }
        outbuf = new Float32Array(data.size) || outbuf
      break;

    }

  }

}
