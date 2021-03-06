const app = Elm.Main.init({flags : {width : window.innerWidth, height : window.innerHeight}})

if (app.ports.loadSound) app.ports.loadSound.subscribe(createBuffer)
if (app.ports.toEngine) app.ports.toEngine.subscribe(engine)
if (app.ports.toggleRecord) app.ports.toggleRecord.subscribe(toggleRecord)
if (app.ports.requestSoundDraw) app.ports.requestSoundDraw.subscribe(drawSound)
if (app.ports.requestCutSample) app.ports.requestCutSample.subscribe(cutSample)

const buffers = {}
    , ro = new ResizeObserver(sendSize)
    , nodeToRecord = Tone.context.createGain()
    , recorder = new Recorder(nodeToRecord)
Tone.Master.connect(nodeToRecord)
ro.observe(document.getElementById('svgResizeObserver'))

let playing = {}

let deb = null

function sendSize(entries) {
    app.ports.newSVGSize.send(entries[0].contentRect)
}

function drawSound(soundName) {
  if (buffers[soundName]) {
    drawSamples(Array.from(buffers[soundName].getChannelData()))
    app.ports.soundDrawn.send(soundName)
  } else console.log(soundName + ' isn’t loaded, cannot draw')
}

function createBuffer(soundName) {
  if (buffers[soundName]) {
    app.ports.soundLoaded.send(soundName + ' already Loaded')
  } else {
    buffers[soundName] = new Tone.Buffer('./sons/' + soundName, ()=>loadOk(soundName), e=>loadErr(e, soundName))
  }
}

function loadOk(soundName) {
  app.ports.soundLoaded.send(
  { path : soundName
  , length : buffers[soundName].duration
  })
}

function loadErr(err, soundName) {
  console.log(err)
  app.ports.soundLoaded.send(soundName + ' got ' + err)
}

function toggleRecord(bool) {
    if (bool) recorder.record()
    else {
        recorder.stop()
        recorder.exportWAV(bl => app.ports.gotRecord.send(URL.createObjectURL(bl)))
        recorder.clear()
    }
}

function cutSample(infos) {
    if (!buffers[infos.fromFileName]) {console.err(infos.fromFileName + " ain’t loaded, cannot cut");return;}

    let buf = buffers[infos.fromFileName]._buffer
      , start = infos.percents[0] * buf.length - 1
      , end = infos.percents[1] * buf.length + 1
      , newBuf = new AudioBuffer(
        { length : end - start
        , numberOfChannels : buf.numberOfChannels
        , sampleRate : buf.sampleRate
        })

    for (let i = 0 ; i < buf.numberOfChannels ; i++) {
        let chan = buf.getChannelData(i).slice(start, end)
        newBuf.copyToChannel(chan, i)
    }

    app.ports.gotNewSample.send(new File([audioBufferToWav(newBuf)], infos.newFileName + ".wav", {type: "audio/wav"}))
}

function engine(o) {
  let model = null
  switch ( o.action ) {
    case "stopReset" :
        for ( id in playing) {
            stop(playing[id])
        }
        playing = {}
        break;
    case "playPause" :
        let t = Tone.now()+0.1
        o.gears.map(g=>playPause(g,t))
        break;
    case "mute" :
        model = o.beadIndexes.reduce((acc, v) => {if (acc && acc.players) return acc.players[v]}, playing[o.id])
        if (model) {
            model.mute = o.value
            setVolume(model)
        }
        break;
    case "volume" :
        model = o.beadIndexes.reduce((acc, v) => {if (acc && acc.players) return acc.players[v]}, playing[o.id])
        if (model) {
            model.volume = o.value
            setVolume(model)
        }
        break;
    }
}

function playPause(model,t) {
    if (!playing[model.id]) {
        playing[model.id] = prepare(model)
    }
    if (playing[model.id].paused) {
        play(playing[model.id], t, model)
    }
    else {
        pause(playing[model.id], t)
    }
}

function drawSamples(samples) {
  setTimeout(() => {
      let canvas = document.getElementById('waveform')
        , ctx = canvas.getContext('2d')
        , {width, height} = canvas
        , pxPerSample = width / samples.length

      ctx.clearRect(0, 0, width, height)

      ctx.strokeStyle = 'black'
      ctx.beginPath()
      ctx.moveTo(0, height / 2)
      ctx.lineTo(width, height / 2)
      ctx.stroke()

      ctx.strokeRect(0, 0, width, height)

      if (pxPerSample < 0.5) {
        for (let x = 0 ; x < width ; x++) {
          let px = samples.slice(Math.floor(x / pxPerSample), Math.floor((x + 1) / pxPerSample))
          ctx.strokeStyle = 'black'
          ctx.beginPath()
          ctx.moveTo(x, (Math.min.apply(null, px) + 1) * height / 2)
          ctx.lineTo(x, (Math.max.apply(null, px) + 1) * height / 2)
          ctx.stroke()

          let rms = Math.sqrt(px.reduce((acc,v,i,a) => acc + Math.pow(v, 2)) / px.length)
          ctx.strokeStyle = 'gray'
          ctx.beginPath()
          ctx.moveTo(x, (1 - rms) * height / 2)
          ctx.lineTo(x, (1 + rms) * height / 2)
          ctx.stroke()
        }
      } else {
        ctx.strokeStyle = 'black'
        ctx.beginPath()
        ctx.moveTo(0, (samples[0] + 1) * height / 2)
        for (let i = 1 ; i < samples.length ; i++) {
          ctx.lineTo(i * pxPerSample, (samples[i] + 1) * height / 2)
        }
        ctx.stroke()
      }
  }, 10)
}
