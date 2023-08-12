"use strict";
var fs = require('fs')
var path = require('path')
var process = require('process')

fs.mkdir('generated', {recursive: true}, function(err) {
  if (err) console.log(err)
});

var app = require('./ImportData.js').Elm.ImportData.init()

// -------------------------- Console ----------------------------

var consoleCommand = function(message) {
  switch (message.kind) {
    case 'write':
      console.log(message.text)
      break
    case 'error':
      console.log("################# ERROR ############")
      console.log(message.text)
      console.log("")
      break
    case 'log':
      var filename = 'output/'+(message.filename || 'log.txt')
      fs.appendFile(filename, message.text+"\n", function(err) {
        if (err) console.log(err)
      })
      break
    case 'readFile':
      //console.log('js readfile')
      var filename = 'data/'+message.filename
      fs.readFile(filename, function(err, contents) {
        var result
        if (err) {
          result = {err : err}
          console.log(err)
        } else {
          result = {ok : contents.toString()}
        }
        if (app.ports.consoleEvent) {
          app.ports.consoleEvent.send({
            kind: 'readfile',
            filename: message.filename,
            result: result,
          })
        }
      })
      break
    case 'readFileBinary':
      //console.log('js readfileBinary')
      var filename = 'data/'+message.filename
      fs.readFile(filename, function(err, contents) {
        var result
        if (err) {
          result = {err : err}
          console.log(err)
        } else {
          result = {ok : contents.toString('hex')}
        }
        if (app.ports.consoleEvent) {
          app.ports.consoleEvent.send({
            kind: 'readfileBinary',
            filename: message.filename,
            result: result,
          })
        }
      })
      break
    case 'writeFile':
      //console.log(message)
      var filename = 'generated/'+message.filename
      fs.mkdir(path.dirname(filename), {recursive: true}, function(err) {
        var result
        if (err) {
          result = {err : err}
          console.log(err)
          if (app.ports.consoleEvent) {
            app.ports.consoleEvent.send({
              kind: 'writefile',
              filename: message.filename,
              result: result,
            })
          }
        } else {
          fs.writeFile(filename, message.text, function(err) {
            if (err) {
              result = {err : err}
              console.log(err)
            } else {
              result = {ok : true}
            }
            if (app.ports.consoleEvent) {
              app.ports.consoleEvent.send({
                kind: 'writefile',
                filename: message.filename,
                result: result,
              })
            }
          })
        }
      });
      break
    case 'exit':
      console.log('js exit')
      process.exit()
      break
    default:
      console.log('unknown console command', message)
      break
  }
}

if (app.ports.consoleCommand) {
  app.ports.consoleCommand.subscribe(consoleCommand)
}

if (app.ports.consoleSigInt) {
  var sigIntReceived = false
  process.on('SIGINT', function() {
    //console.log('js sigint')
    if (sigIntReceived) {
      console.log('js sigint exit')
      process.exit()
    }
    app.ports.consoleSigInt.send()
    sigIntReceived = true
    setTimeout(function() {sigIntReceived = false}, 1000)
  })
}

//var run = function() {
  //setTimeout(run, 1000)
//}
//run()
