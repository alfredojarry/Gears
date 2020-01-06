// TODO clean changeVolume, use Tone.Draw

function prepare(model, rate = 1) {
    model.paused = true
    model.pauseOffset = 0
    model.length = model.length
    if (model.soundName) {
        model.player = new Tone.Player(buffers[model.soundName]).toMaster()
        model.duration = model.player.buffer.duration
        model.player.playbackRate = model.rate = rate * model.duration / model.length
        model.player.loop = true
    }
    if (model.mobile) {
        model.duration = model.mobile.length
        model.rate = rate * model.duration / model.length
        model.gears = model.mobile.gears.map(v => prepare(v, model.rate))
    }
    if (model.collar) {
        model.duration = model.collar.length // TODO when preparing top collar, duration should ignored
        model.rate = (rate * model.duration / model.length) || 1 // TODO when preparing top collar, no model.length
        model.durs = model.collar.beads.map(v => v.length / model.rate)
        let totalDur = model.durs.reduce((a,b) => a+b, 0)
        model.players = model.collar.beads.map(v => prepare(v, model.rate))
        model.clocks = model.players.map((subModel,i,a) => {
            return new Tone.Clock(t => {
                if (model.paused && (model.progPause <= t)) return;
              // if paused, clock t < progPause
                model.progClock = t
                model.current = i
                let prec = (i + a.length - 1) % a.length
                play(subModel, t, subModel, model.volume, model.mute)
                pause(a[prec], t, model.paused) // hence force
                if (model.paused) pause(subModel, model.progPause) // and pause next
                console.log(i, Math.min(model.players[i].duration - model.players[i].pauseOffset, model.players[i].pauseOffset)) // TODO Small drift…
            }, 1/totalDur)
        })
    }
    return model
}

function play(model, t, newModel = {}, volume = 1, mute = false) { // TODO What if no new model (first play)
    if (!model.paused) return;
    model.paused = false
    model.volume = newModel.volume || 1 // TODO cf first TODO
    model.mute = newModel.mute || false // TODO cf first TODO
    model.startTime = t - model.pauseOffset / model.rate
    if (model.soundName) {
        if (mute || model.mute) model.player.mute = true
        else model.player.volume.value = ((model.volume * volume) - 1) * 60
        model.player.start(t, model.pauseOffset)
    }
    if (model.mobile) {
        model.gears.map((v,i) => play(v, t, model.gears[i], model.volume * volume, model.mute || mute))
    }
    if (model.collar) {
        let current = 0
          , acc = 0
          , ratedOffset = model.pauseOffset / model.rate
        while (acc <= ratedOffset) {
          acc += model.durs[current]
          current = (current + 1) % model.durs.length
        }
        play(model.players[(current + model.durs.length - 1) % model.durs.length], t, {}, model.volume, model.mute)
        acc = t + acc - ratedOffset
        for (let i = 0 ; i < model.clocks.length ; i++) {
          let j = (i + current) % model.clocks.length
          model.clocks[j].start(acc)
          acc += model.durs[j]
        }
    }
}

function pause(model, t, force = false) {
    if (model.paused && !force) return;
    model.paused = true
    model.pauseOffset = ((t - model.startTime) * model.rate)
    if (model.soundName) {
        model.player.stop(t)
    }
    if (model.mobile) {
        model.gears.map(pause)
    }
    if (model.collar) {
        model.progPause = t
        if (t <= model.progClock) {
            let prec = (model.current + model.clocks.length - 1) % model.clocks.length
            pause(model.players[prec], t, true)
            pause(model.players[model.current], model.progClock)
        }
        model.clocks.map(v => v.stop(t))
        model.players.map(v => pause(v, t))
    }
}

function stop(model) {
    if (model.soundName) model.player.stop()
    if (model.mobile) model.gears.map(stop)
    if (model.collar) {
        model.clocks.map(v => v.stop())
        model.players.map(stop)
    }
}

function setVolume(model, volume = 1, mute = false) {
    if (model.soundName) {
        if (mute || model.mute) model.player.mute = true
        else model.player.volume.value = ((model.volume * volume) - 1) * 60
    }
    if (model.mobile) model.gears.map(v => setVolume(v, model.volume * volume, model.mute || mute))
    if (model.collar) model.players.map(v => setVolume(v, model.volume * volume, model.mute || mute))
}