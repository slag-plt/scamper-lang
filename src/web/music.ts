/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/restrict-template-expressions */
/* eslint-disable @typescript-eslint/no-unsafe-call */
/* eslint-disable @typescript-eslint/no-unsafe-assignment */
import * as Music from '../lib/music.js'
import { ICE } from '../result.js'
import * as JZZ from './jzz/jzz-combined.cjs'
import * as Scamper from '../index.js'

type Duration = Music.Duration
type Composition = Music.Composition
type MIDI = any // TODO: ugh, how do I specify this type!?
type Synth = any // TODO: ...this one, too!

type MidiMsg = {
  tag: 'midi',
  time: number,
  data: MIDI,
}

type TriggerMsg = {
  tag: 'trigger',
  time: number,
  callback: Scamper.Lang.FunctionType
}

const testProgramMap = [
  56, // piano
  5, // electric piano
  19, // rock organ
  26, // jazz guitar
  31, // distorted gutiar
  33, // electric bass
  40, // violin
  56, // trumpet
  65 // alto sax
]

type Msg = MidiMsg | TriggerMsg

const midiMsg = (time: number, data: MIDI): Msg =>
  ({ tag: 'midi', time, data })

const triggerMsg = (time: number, callback: Scamper.Lang.FunctionType): Msg =>
  ({ tag: 'trigger', time, callback })

function ratioToDouble (ratio: Duration) {
  return ratio.fields[0] / ratio.fields[1]
}

function durationToTimeMs (beat: Duration, bpm: number, dur: Duration) {
  return ratioToDouble(dur) / (ratioToDouble(beat) * bpm) * 60 * 1000
}

function compositionToMsgs (
  beat: Duration, bpm: number, velocity: number, startTime: number,
  program: number, composition: Composition): { endTime: number, msgs: Msg[] } {
  switch (composition.tag) {
    case 'empty':
      return { endTime: startTime, msgs: [] }
    case 'note': {
      const endTime = startTime + durationToTimeMs(beat, bpm, composition.duration)
      return {
        endTime,
        msgs: [
          midiMsg(
            startTime,
            JZZ.MIDI.noteOn(program, composition.note, velocity)
          ),
          midiMsg(
            endTime,
            JZZ.MIDI.noteOff(program, composition.note, velocity)
          )
        ]
      }
    }
    case 'rest':
      return {
        endTime: startTime + durationToTimeMs(beat, bpm, composition.duration),
        msgs: []
      }
    case 'trigger': {
      return {
        endTime: startTime,
        msgs: [triggerMsg(startTime, composition.fn)]
      }
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
      return { endTime, msgs }
    }

    case 'seq': {
      const msgs: Msg[] = []
      let time = startTime
      composition.notes.forEach(note => {
        const result = compositionToMsgs(beat, bpm, velocity, time, program, note)
        msgs.push(...result.msgs)
        time = result.endTime
      })
      msgs.sort((c1, c2) => c1.time - c2.time)
      return { endTime: time, msgs }
    }

    case 'pickup': {
      const pickup = compositionToMsgs(beat, bpm, velocity, startTime, program, composition.pickup)
      const pickupDuration = pickup.endTime - startTime
      let notes: { endTime: number, msgs: Msg[] } | undefined
      // If the pickup would start in negative time, then rebase the composition to start
      // with the pickup instead.
      if (startTime - pickupDuration < 0) {
        pickup.msgs.forEach(msg => {
          msg.time += pickupDuration
        })
        notes = compositionToMsgs(beat, bpm, velocity, pickupDuration, program, composition.notes)

      // Otherwise, rebase pickup to start before the composition.
      } else {
        pickup.msgs.forEach(msg => {
          msg.time -= pickupDuration
        })
        notes = compositionToMsgs(beat, bpm, velocity, startTime, program, composition.notes)
      }
      const msgs: Msg[] = []
      msgs.push(...pickup.msgs)
      msgs.push(...notes.msgs)
      return { endTime: notes.endTime, msgs }
    }

    case 'mod': {
      if (composition.mod.type === 'percussion') {
        return compositionToMsgs(beat, bpm, velocity, startTime, 9, composition.note)
      } else if (composition.mod.type === 'pitchBend') {
        const msgs = []
        const data = compositionToMsgs(beat, bpm, velocity, startTime, program, composition.note)
        msgs.push(midiMsg(startTime, JZZ.MIDI.pitchBendF(0, composition.mod.fields[0])))
        msgs.push(...data.msgs)
        msgs.push(midiMsg(data.endTime, JZZ.MIDI.pitchBendF(0, 0)))
        return { msgs, endTime: data.endTime }
      } else if (composition.mod.type === 'tempo') {
        return compositionToMsgs(composition.mod.fields[0], composition.mod.fields[1], velocity, startTime, program, composition.note)
      } else if (composition.mod.type === 'dynamics') {
        return compositionToMsgs(beat, bpm, composition.mod.fields[0], startTime, program, composition.note)
      } else if (composition.mod.type === 'instrument') {
        // TODO: need to add an additional argument to compositionToMsgs
        // to pass the "current" voice. Or do we need to add infrastructure to
        // map one new voice to each channel? Need to check if channels 0-8 are
        // actually available for playback...
        console.log(composition.mod.fields[0])
        return compositionToMsgs(beat, bpm, velocity, startTime, composition.mod.fields[0], composition.note)
      } else {
        throw new ICE('compositionToMsgs', `unknown mod tag: ${composition.mod}`)
      }
    }
  }
}

function playback (synth: Synth, composition: Composition): number {
  const startTime = window.performance.now()
  const msgs = compositionToMsgs(Music.dur(1, 4), 120, 64, 0, 0, composition).msgs
  let i = 0
  // NOT WORKING, I WONDER WHY!?
  for (let i = 0; i < testProgramMap.length; i++) {
    console.log(`Setting ${i} to ${testProgramMap[i]}`)
    JZZ.MIDI.program(i, testProgramMap[i])
  }
  // JZZ.synth.Tiny.setSynth(0, 50)
  // eslint-disable-next-line @typescript-eslint/no-misused-promises
  const id = window.setInterval(async () => {
    const now = window.performance.now()
    while (i < msgs.length) {
      const msg = msgs[i]
      if (msg.time + startTime <= now) {
        if (msg.tag === 'midi') {
          synth.send(msg.data)
        } else if (msg.tag === 'trigger') {
          console.log((msg.callback as Scamper.Lang.LambdaType).env)
          const result = await Scamper.evaluateExp(Scamper.preludeEnv, Scamper.Lang.nlecall(Scamper.Lang.nlevalue(msg.callback), []))
          console.log(result)
        }
        i += 1
        continue // N.B., try to process the next message
      } else {
        return Promise.resolve(undefined) // N.B., wait for the next interval to process msgs again
      }
    }
    clearInterval(id)
    return Promise.resolve(undefined)
  })
  return id
}

export function emitCompositionWidget (synth: any, node: Element) {
  const id = parseInt(node.id)
  const composition: Composition = Scamper.store.get(id)! as Composition
  node.textContent = '' // N.B., clear the contents of the node for the buttons
  const playButton = document.createElement('button')
  playButton.textContent = '▶'
  const stopButton = document.createElement('button')
  stopButton.textContent = '■'
  let timer: number | undefined
  playButton.onclick = function (_e) {
    timer = playback(synth, composition)
  }
  stopButton.onclick = function (_e) {
    if (timer !== undefined) {
      clearInterval(timer)
    }
  }
  node.appendChild(playButton)
  node.appendChild(stopButton)
}
