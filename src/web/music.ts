import * as Music from '../lib/music.js'
import * as JZZ from './jzz/jzz-combined.cjs'

type Duration = Music.Duration
type Composition = Music.Composition
type MIDI = any  // TODO: ugh, how do I specify this type!?
type Synth = any  // TODO: ...this one, too!
type Msg = { time: number, data: MIDI }

function ratioToDouble(ratio: Duration) {
  return ratio.num / ratio.den
}

function durationToTimeMs(beat: Duration, bpm: number, dur: Duration) {
  return ratioToDouble(dur) / (ratioToDouble(beat) * bpm) * 60 * 1000
}

function compositionToMsgs(beat: Duration, bpm: number, startTime: number, composition: Composition): { endTime: number, msgs: Msg[] } {
  switch (composition.tag) {
    case 'note':
      const endTime = startTime + durationToTimeMs(beat, bpm, composition.duration)
      return {
        endTime: endTime,
        msgs: [
          {
            time: startTime,
            data: JZZ.MIDI.noteOn(0, composition.pitch + composition.octave, 127)
          },
          {
            time: endTime,
            data: JZZ.MIDI.noteOff(0, composition.pitch + composition.octave, 127)
          }
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
        const result = compositionToMsgs(beat, bpm, startTime, note)
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
        const result = compositionToMsgs(beat, bpm, time, note)
        msgs.push(...result.msgs)
        time = result.endTime
        console.log(time)
      })
      msgs.sort((c1, c2) => c1.time - c2.time)
      return { endTime: time, msgs }
    }

    case 'mod':
      // TODO: fill in once we have mods!
      return compositionToMsgs(beat, bpm, startTime, composition.note)
  }
}

function playback(synth: Synth, composition: Composition): number {
  const startTime = window.performance.now()
  const msgs = compositionToMsgs({num: 1, den: 4}, 120, 0, composition).msgs
  let i = 0
  const id = window.setInterval(() => {
    const now = window.performance.now()
    while (i < msgs.length) {
      if (msgs[i].time + startTime <= now) {
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