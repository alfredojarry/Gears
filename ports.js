const app = Elm.Main.init({flags : {width : window.innerWidth, height : window.innerHeight}})

if (app.ports.loadSound) app.ports.loadSound.subscribe(createBuffer)
if (app.ports.toEngine) app.ports.toEngine.subscribe(engine)

const buffers = {}
    , playing = {}
    , ro = new ResizeObserver(sendSize)
ro.observe(document.getElementById('svgResizeObserver'))

let deb = null

function sendSize(entries) {
    app.ports.newSVGSize.send(entries[0].contentRect)
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

function engine(o) {
  switch ( o.action ) {
    case "stopReset" :
        for ( id in playing) stop(id)
        break;
    case "playPause" :
        o.gears.map(playPause)
        break;
    case "mute" :
        mute(o.gearId, o.value)
        break;
    case "volume" :
        changeVolume(o.gearId, o.value)
    }
}

function playPause(model) {
    if (!playing[model.id]) {
        playing[model.id] = model
        play(model.id)
    } else if (playing[model.id].paused) unpause(model.id)
    else pause(model.id)
}

function play(id) {
    let model = playing[id]
      , s = model.player = new Tone.Player(buffers[model.soundName]).toMaster()
      , g = model.gear = SVG.adopt(document.getElementById(model.id))
    model.paused = false
    g.animate(model.length * 1000).transform({rotation:360, cx:0, cy:0}).loop()
    s.loop = true
    setVolume(s, model.volume, model.mute)
    s.playbackRate = model.rate = s.buffer.duration / model.length
    s.start()
    model.startTime = Tone.context.now()
}

function pause(id) {
    let model = playing[id]
    model.paused = true
    model.gear.animate().pause()
    model.player.stop()
    model.pauseOffset = (Tone.context.now() - model.startTime) * model.rate
}

function unpause(id) {
    let model = playing[id]
    model.paused = false
    model.gear.animate().play()
    model.player.start(Tone.context.now(), model.pauseOffset)
    model.startTime = Tone.context.now() - model.pauseOffset / model.rate
}

function stop(id) {
    let model = playing[id]
    if (!model) return;
    model.gear.animate().play().finish()
    model.player.stop()
    playing[id] = null
}

function mute(id, mute) {
    let model = playing[id]
    if (!model) return;
    setVolume(model.player, model.volume, mute)
}

function changeVolume(id, volume) {
    let model = playing[id]
    if (!model) return;
    model.volume = volume
    setVolume(model.player, volume, model.mute)
}

function setVolume(source, volume, mute) {
    mute ? source.mute = true : source.volume.value = (volume - 1) * 60
}
