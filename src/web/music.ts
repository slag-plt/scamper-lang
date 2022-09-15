import { mod } from '../lib/docs.js'
import * as Music from '../lib/music.js'
import { ICE } from '../result.js'
import * as JZZ from './jzz/jzz-combined.cjs'

type Duration = Music.Duration
type Composition = Music.Composition
type MIDI = any  // TODO: ugh, how do I specify this type!?
type Synth = any  // TODO: ...this one, too!

type Msg = MidiMsg

type MidiMsg = {
  tag: 'midi',
  time: number,
  data: MIDI,
}

const midiMsg = (time: number, data: MIDI): Msg =>
  ({ tag: 'midi', time, data })

function ratioToDouble(ratio: Duration) {
  return ratio.num / ratio.den
}

function durationToTimeMs(beat: Duration, bpm: number, dur: Duration) {
  return ratioToDouble(dur) / (ratioToDouble(beat) * bpm) * 60 * 1000
}

function setPrograms(synth: Synth, programMap: number[]) {
  console.log(`setPrograms: ${programMap}`)
  for (let i = 0; i < programMap.length; i++) {
    synth.setSynth(i, synth.getSynth(programMap[i]))
  }
}

function compositionToMsgs(
    beat: Duration, bpm: number, velocity: number, startTime: number,
    program: number, composition: Composition): { endTime: number, msgs: Msg[] } {
  switch (composition.tag) {
    case 'empty':
      return { endTime: startTime, msgs: [] }
    case 'note':
      const endTime = startTime + durationToTimeMs(beat, bpm, composition.duration)
      return {
        endTime: endTime,
        msgs: [
          midiMsg(
            startTime,
            JZZ.MIDI.noteOn(program, composition.note, velocity),
          ),
          midiMsg(
            endTime,
            JZZ.MIDI.noteOff(program, composition.note, velocity)
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
        const result = compositionToMsgs(beat, bpm, velocity, startTime, program, note)
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
        const result = compositionToMsgs(beat, bpm, velocity, time, program, note)
        msgs.push(...result.msgs)
        time = result.endTime
        console.log(time)
      })
      msgs.sort((c1, c2) => c1.time - c2.time)
      return { endTime: time, msgs }
    }

    case 'mod': {
      if (composition.mod.tag === 'percussion') {
        return compositionToMsgs(beat, bpm, velocity, startTime, 9, composition.note)
      } else if (composition.mod.tag === 'pitchBend') {
        const msgs = []
        const data = compositionToMsgs(beat, bpm, velocity, startTime, program, composition.note)
        msgs.push(midiMsg(startTime, JZZ.MIDI.pitchBendF(0, composition.mod.amount)))
        msgs.push(...data.msgs)
        msgs.push(midiMsg(data.endTime, JZZ.MIDI.pitchBendF(0, 0)))
        return { msgs, endTime: data.endTime }
      } else if (composition.mod.tag === 'tempo') {
        return compositionToMsgs(composition.mod.beat, composition.mod.bpm, velocity, startTime, program, composition.note)
      } else if (composition.mod.tag === 'dynamics'){
        return compositionToMsgs(beat, bpm, composition.mod.amount, startTime, program, composition.note)
      } else {
        throw new ICE('compositionToMsgs', `unknown mod tag: ${composition.mod}`)
      }
    }
  }
}

function playback(synth: Synth, composition: Composition): number {
  const startTime = window.performance.now()
  const msgs = compositionToMsgs({num: 1, den: 4}, 120, 64, 0, 0, composition).msgs
  console.log(msgs)
  let i = 0
  const id = window.setInterval(() => {
    const now = window.performance.now()
    while (i < msgs.length) {
      const msg = msgs[i]
      if (msg.time + startTime <= now) {
        synth.send(msg.data)
        i += 1
        continue // N.B., try to process the next message
      } else {
        return  // N.B., wait for the next interval to process msgs again
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