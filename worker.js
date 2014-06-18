module.exports = function(){
  var xxx = require('jmao')
  var detect = require('detective')
  var moduleCache = {}
  t = 0, i = 0, rate = 44100 
  var _require = require
  require = function(module){
    if(moduleCache[module]) return moduleCache[module].exports
    else return _require(module)
  }
  var outbuf = new Float32Array(4096 * 2 * 2)

  fn = function(){return outbuf}

  tick = function (output, input) { // a fill-a-buffer function

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
      case 'config':
        rate = data.sampleRate || rate
        var _fn = new Function(['require'], data.fn)
        var modules = detect(_fn.toString())
        if(modules.length){
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
                  var x = moduleCache[help[name]] = mods[name]
                  var m = {exports: {}}
                  Function([help[name]], x.bundle)(m)
                  moduleCache[help[name]].exports = m.exports
                })
                fn = _fn(require)
                console.log(fn)
                
                //eval('fn = ' + _fn + '()()')
              }
              else console.log(this.status)
            } 
            post.send(JSON.stringify(body))
          }
          
        }
        else{
          fn = new Function(data.fn)()
          console.log(data.fn)
          eval('fn = function anon(){' + data.fn + '}()')
        }
        outbuf = new Float32Array(data.size) || outbuf
      break;

    }

  }

}
