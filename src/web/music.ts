import * as Music from '../lib/music.js'
import { ICE } from '../result.js'
import * as JZZ from './jzz/jzz-combined.cjs'

type Duration = Music.Duration
type Composition = Music.Composition
type MIDI = any  // TODO: ugh, how do I specify this type!?
type Synth = any  // TODO: ...this one, too!

type InstrInfo = { isPercussion: boolean, value: number }

type Msg = {
  time: number,
  data: MIDI,
  instrument?: InstrInfo 
}

const msg = (time: number, data: MIDI, instrument?: InstrInfo): Msg =>
  ({ time, data, instrument })

function ratioToDouble(ratio: Duration) {
  return ratio.num / ratio.den
}

function durationToTimeMs(beat: Duration, bpm: number, dur: Duration) {
  return ratioToDouble(dur) / (ratioToDouble(beat) * bpm) * 60 * 1000
}

function compositionToMsgs(
    beat: Duration, bpm: number, velocity: number, isPercussion: boolean,
    instrument: number, startTime: number, composition: Composition): { endTime: number, msgs: Msg[] } {
  switch (composition.tag) {
    case 'note':
      const endTime = startTime + durationToTimeMs(beat, bpm, composition.duration)
      return {
        endTime: endTime,
        msgs: [
          msg(
            startTime,
            JZZ.MIDI.noteOn(0, composition.note, velocity),
            { isPercussion, value: instrument }
          ),
          msg(
            endTime,
            JZZ.MIDI.noteOff(0, composition.note, velocity)
          )
        ]
      }

    case 'rest':
      return {
        endTime: startTime + durationToTimeMs(beat, bpm, composition.duration),
        msgs: []
      }

    case 'par': {
      const msgs: Msg[] = []
      let endTime = 0
      composition.notes.forEach(note => {
        const result = compositionToMsgs(beat, bpm, velocity, isPercussion, instrument, startTime, note)
        msgs.push(...result.msgs)
        endTime = Math.max(result.endTime, endTime)
      })
      msgs.sort((c1, c2) => c1.time - c2.time)
      return { endTime: endTime, msgs }
    }

    case 'seq': {
      const msgs: Msg[] = []
      let time = startTime
      composition.notes.forEach(note => {
        const result = compositionToMsgs(beat, bpm, velocity, isPercussion, instrument, time, note)
        msgs.push(...result.msgs)
        time = result.endTime
        console.log(time)
      })
      msgs.sort((c1, c2) => c1.time - c2.time)
      return { endTime: time, msgs }
    }

    case 'mod': {
      if (composition.mod.tag === 'instrument') {
        return compositionToMsgs(beat, bpm, velocity, false, composition.mod.instrument, startTime, composition.note)
      } else if (composition.mod.tag === 'pitchBend') {
        const msgs = []
        const data = compositionToMsgs(beat, bpm, velocity, isPercussion, instrument, startTime, composition.note)
        msgs.push({ time: startTime, data: JZZ.MIDI.pitchBendF(0, composition.mod.amount) })
        msgs.push(...data.msgs)
        msgs.push({ time: data.endTime, data: JZZ.MIDI.pitchBendF(0, 0) })
        return { msgs, endTime: data.endTime }
      } else if (composition.mod.tag === 'tempo') {
        return compositionToMsgs(composition.mod.beat, composition.mod.bpm, velocity, isPercussion, instrument, startTime, composition.note)
      } else if (composition.mod.tag === 'dynamics'){
        return compositionToMsgs(beat, bpm, composition.mod.amount, isPercussion, instrument, startTime, composition.note)
      } else {
        throw new ICE('compositionToMsgs', `unknown mod tag: ${composition.mod}`)
      }
    }
  }
}

function playback(synth: Synth, composition: Composition): number {
  const startTime = window.performance.now()
  const msgs = compositionToMsgs({num: 1, den: 4}, 120, 64, false, 0, 0, composition).msgs
  console.log(msgs)
  let i = 0
  const id = window.setInterval(() => {
    const now = window.performance.now()
    while (i < msgs.length) {
      const msg = msgs[i]
      if (msg.time + startTime <= now) {
        if (msg.instrument && msg.instrument.isPercussion) {
          synth.setSynth(1, synth.getSynth(msg.instrument.value, true), true)
        } else if (msg.instrument && !msg.instrument.isPercussion) {
          synth.setSynth(0, synth.getSynth(msg.instrument.value))
        }
        synth.send(msgs[i].data)
        i += 1
      } else {
        return
      }
    }
    clearInterval(id)
  })
  return id
}

export function emitCompositionWidget(synth: any, node: Element) {
  const composition = JSON.parse(node.textContent!)
  node.textContent = ''  // N.B., clear the contents of the node for the buttons
  const playButton = document.createElement('button')
  playButton.textContent = '▶'
  const stopButton = document.createElement('button')
  stopButton.textContent = '■'
  let timer: number | undefined = undefined
  playButton.onclick = function(_e) {
    timer = playback(synth, composition)
  }
  stopButton.onclick = function(_e) {
    if (timer !== undefined) {
      clearInterval(timer)
    }
  }
  node.appendChild(playButton)
  node.appendChild(stopButton)
}