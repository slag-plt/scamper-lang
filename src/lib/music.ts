/* eslint-disable camelcase */
/* eslint-disable no-use-before-define */
/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/restrict-template-expressions */
/* eslint-disable @typescript-eslint/no-unsafe-call */
/* eslint-disable @typescript-eslint/no-unsafe-assignment */
import * as L from '../lang.js'
import { msg } from '../messages.js'
import { ICE, ok } from '../result.js'
import { runtimeError } from '../runtime.js'
import { evaluateExp } from '../evaluator.js'
import * as Docs from './docs.js'
import * as Pretty from '../pretty.js'
import * as Utils from './utils.js'

import * as webaudiofont from './webaudiofont/WebAudioFontPlayer.js'
import * as instr from './webaudiofont/instruments_FluidR3_GM_sf2_file.js'
import * as perc from './webaudiofont/percussion_FluidR3_GM_sf2_file.js'

const player: any = new webaudiofont.WebAudioFontPlayer()
const audioContext = new window.AudioContext()
player.loader.decodeAfterLoading(audioContext, '_tone_0000_FluidR3_GM_sf2_file')
for (let i = 0; i < 128; i++) {
  player.loader.decodeAfterLoading(audioContext, `_tone_${i.toString().padStart(3, '0')}0_FluidR3_GM_sf2_file`)
}
for (let i = 35; i <= 81; i++) {
  player.loader.decodeAfterLoading(audioContext, `_drum_${i}_0_FluidR3_GM_sf2_file`)
}

const instrMap: Map<number, any> = new Map([
  [0, instr._tone_0000_FluidR3_GM_sf2_file],
  [1, instr._tone_0010_FluidR3_GM_sf2_file],
  [2, instr._tone_0020_FluidR3_GM_sf2_file],
  [3, instr._tone_0030_FluidR3_GM_sf2_file],
  [4, instr._tone_0040_FluidR3_GM_sf2_file],
  [5, instr._tone_0050_FluidR3_GM_sf2_file],
  [6, instr._tone_0060_FluidR3_GM_sf2_file],
  [7, instr._tone_0070_FluidR3_GM_sf2_file],
  [8, instr._tone_0080_FluidR3_GM_sf2_file],
  [9, instr._tone_0090_FluidR3_GM_sf2_file],
  [10, instr._tone_0100_FluidR3_GM_sf2_file],
  [11, instr._tone_0110_FluidR3_GM_sf2_file],
  [12, instr._tone_0120_FluidR3_GM_sf2_file],
  [13, instr._tone_0130_FluidR3_GM_sf2_file],
  [14, instr._tone_0140_FluidR3_GM_sf2_file],
  [15, instr._tone_0150_FluidR3_GM_sf2_file],
  [16, instr._tone_0160_FluidR3_GM_sf2_file],
  [17, instr._tone_0170_FluidR3_GM_sf2_file],
  [18, instr._tone_0180_FluidR3_GM_sf2_file],
  [19, instr._tone_0190_FluidR3_GM_sf2_file],
  [20, instr._tone_0200_FluidR3_GM_sf2_file],
  [21, instr._tone_0210_FluidR3_GM_sf2_file],
  [22, instr._tone_0220_FluidR3_GM_sf2_file],
  [23, instr._tone_0230_FluidR3_GM_sf2_file],
  [24, instr._tone_0240_FluidR3_GM_sf2_file],
  [25, instr._tone_0250_FluidR3_GM_sf2_file],
  [26, instr._tone_0260_FluidR3_GM_sf2_file],
  [27, instr._tone_0270_FluidR3_GM_sf2_file],
  [28, instr._tone_0280_FluidR3_GM_sf2_file],
  [29, instr._tone_0290_FluidR3_GM_sf2_file],
  [30, instr._tone_0300_FluidR3_GM_sf2_file],
  [31, instr._tone_0310_FluidR3_GM_sf2_file],
  [32, instr._tone_0320_FluidR3_GM_sf2_file],
  [33, instr._tone_0330_FluidR3_GM_sf2_file],
  [34, instr._tone_0340_FluidR3_GM_sf2_file],
  [35, instr._tone_0350_FluidR3_GM_sf2_file],
  [36, instr._tone_0360_FluidR3_GM_sf2_file],
  [37, instr._tone_0370_FluidR3_GM_sf2_file],
  [38, instr._tone_0380_FluidR3_GM_sf2_file],
  [39, instr._tone_0390_FluidR3_GM_sf2_file],
  [40, instr._tone_0400_FluidR3_GM_sf2_file],
  [41, instr._tone_0410_FluidR3_GM_sf2_file],
  [42, instr._tone_0420_FluidR3_GM_sf2_file],
  [43, instr._tone_0430_FluidR3_GM_sf2_file],
  [44, instr._tone_0440_FluidR3_GM_sf2_file],
  [45, instr._tone_0450_FluidR3_GM_sf2_file],
  [46, instr._tone_0460_FluidR3_GM_sf2_file],
  [47, instr._tone_0470_FluidR3_GM_sf2_file],
  [48, instr._tone_0480_FluidR3_GM_sf2_file],
  [49, instr._tone_0490_FluidR3_GM_sf2_file],
  [50, instr._tone_0500_FluidR3_GM_sf2_file],
  [51, instr._tone_0510_FluidR3_GM_sf2_file],
  [52, instr._tone_0520_FluidR3_GM_sf2_file],
  [53, instr._tone_0530_FluidR3_GM_sf2_file],
  [54, instr._tone_0540_FluidR3_GM_sf2_file],
  [55, instr._tone_0550_FluidR3_GM_sf2_file],
  [56, instr._tone_0560_FluidR3_GM_sf2_file],
  [57, instr._tone_0570_FluidR3_GM_sf2_file],
  [58, instr._tone_0580_FluidR3_GM_sf2_file],
  [59, instr._tone_0590_FluidR3_GM_sf2_file],
  [60, instr._tone_0600_FluidR3_GM_sf2_file],
  [61, instr._tone_0610_FluidR3_GM_sf2_file],
  [62, instr._tone_0620_FluidR3_GM_sf2_file],
  [63, instr._tone_0630_FluidR3_GM_sf2_file],
  [64, instr._tone_0640_FluidR3_GM_sf2_file],
  [65, instr._tone_0650_FluidR3_GM_sf2_file],
  [66, instr._tone_0660_FluidR3_GM_sf2_file],
  [67, instr._tone_0670_FluidR3_GM_sf2_file],
  [68, instr._tone_0680_FluidR3_GM_sf2_file],
  [69, instr._tone_0690_FluidR3_GM_sf2_file],
  [70, instr._tone_0700_FluidR3_GM_sf2_file],
  [71, instr._tone_0710_FluidR3_GM_sf2_file],
  [72, instr._tone_0720_FluidR3_GM_sf2_file],
  [73, instr._tone_0730_FluidR3_GM_sf2_file],
  [74, instr._tone_0740_FluidR3_GM_sf2_file],
  [75, instr._tone_0750_FluidR3_GM_sf2_file],
  [76, instr._tone_0760_FluidR3_GM_sf2_file],
  [77, instr._tone_0770_FluidR3_GM_sf2_file],
  [78, instr._tone_0780_FluidR3_GM_sf2_file],
  [79, instr._tone_0790_FluidR3_GM_sf2_file],
  [80, instr._tone_0800_FluidR3_GM_sf2_file],
  [81, instr._tone_0810_FluidR3_GM_sf2_file],
  [82, instr._tone_0820_FluidR3_GM_sf2_file],
  [83, instr._tone_0830_FluidR3_GM_sf2_file],
  [84, instr._tone_0840_FluidR3_GM_sf2_file],
  [85, instr._tone_0850_FluidR3_GM_sf2_file],
  [86, instr._tone_0860_FluidR3_GM_sf2_file],
  [87, instr._tone_0870_FluidR3_GM_sf2_file],
  [88, instr._tone_0880_FluidR3_GM_sf2_file],
  [89, instr._tone_0890_FluidR3_GM_sf2_file],
  [90, instr._tone_0900_FluidR3_GM_sf2_file],
  [91, instr._tone_0910_FluidR3_GM_sf2_file],
  [92, instr._tone_0920_FluidR3_GM_sf2_file],
  [93, instr._tone_0930_FluidR3_GM_sf2_file],
  [94, instr._tone_0940_FluidR3_GM_sf2_file],
  [95, instr._tone_0950_FluidR3_GM_sf2_file],
  [96, instr._tone_0960_FluidR3_GM_sf2_file],
  [97, instr._tone_0970_FluidR3_GM_sf2_file],
  [98, instr._tone_0980_FluidR3_GM_sf2_file],
  [99, instr._tone_0990_FluidR3_GM_sf2_file],
  [100, instr._tone_1000_FluidR3_GM_sf2_file],
  [101, instr._tone_1010_FluidR3_GM_sf2_file],
  [102, instr._tone_1020_FluidR3_GM_sf2_file],
  [103, instr._tone_1030_FluidR3_GM_sf2_file],
  [104, instr._tone_1040_FluidR3_GM_sf2_file],
  [105, instr._tone_1050_FluidR3_GM_sf2_file],
  [106, instr._tone_1060_FluidR3_GM_sf2_file],
  [107, instr._tone_1070_FluidR3_GM_sf2_file],
  [108, instr._tone_1080_FluidR3_GM_sf2_file],
  [109, instr._tone_1090_FluidR3_GM_sf2_file],
  [110, instr._tone_1100_FluidR3_GM_sf2_file],
  [111, instr._tone_1110_FluidR3_GM_sf2_file],
  [112, instr._tone_1120_FluidR3_GM_sf2_file],
  [113, instr._tone_1130_FluidR3_GM_sf2_file],
  [114, instr._tone_1140_FluidR3_GM_sf2_file],
  [115, instr._tone_1150_FluidR3_GM_sf2_file],
  [116, instr._tone_1160_FluidR3_GM_sf2_file],
  [117, instr._tone_1170_FluidR3_GM_sf2_file],
  [118, instr._tone_1180_FluidR3_GM_sf2_file],
  [119, instr._tone_1190_FluidR3_GM_sf2_file],
  [120, instr._tone_1200_FluidR3_GM_sf2_file],
  [121, instr._tone_1210_FluidR3_GM_sf2_file],
  [122, instr._tone_1220_FluidR3_GM_sf2_file],
  [123, instr._tone_1230_FluidR3_GM_sf2_file],
  [124, instr._tone_1240_FluidR3_GM_sf2_file],
  [125, instr._tone_1250_FluidR3_GM_sf2_file],
  [126, instr._tone_1260_FluidR3_GM_sf2_file],
  [127, instr._tone_1270_FluidR3_GM_sf2_file]
])

const percMap: Map<number, any> = new Map([
  [35, perc._drum_35_0_FluidR3_GM_sf2_file],
  [36, perc._drum_36_0_FluidR3_GM_sf2_file],
  [37, perc._drum_37_0_FluidR3_GM_sf2_file],
  [38, perc._drum_38_0_FluidR3_GM_sf2_file],
  [39, perc._drum_39_0_FluidR3_GM_sf2_file],
  [40, perc._drum_40_0_FluidR3_GM_sf2_file],
  [41, perc._drum_41_0_FluidR3_GM_sf2_file],
  [42, perc._drum_42_0_FluidR3_GM_sf2_file],
  [43, perc._drum_43_0_FluidR3_GM_sf2_file],
  [44, perc._drum_44_0_FluidR3_GM_sf2_file],
  [45, perc._drum_45_0_FluidR3_GM_sf2_file],
  [46, perc._drum_46_0_FluidR3_GM_sf2_file],
  [47, perc._drum_47_0_FluidR3_GM_sf2_file],
  [48, perc._drum_48_0_FluidR3_GM_sf2_file],
  [49, perc._drum_49_0_FluidR3_GM_sf2_file],
  [50, perc._drum_50_0_FluidR3_GM_sf2_file],
  [51, perc._drum_51_0_FluidR3_GM_sf2_file],
  [52, perc._drum_52_0_FluidR3_GM_sf2_file],
  [53, perc._drum_53_0_FluidR3_GM_sf2_file],
  [54, perc._drum_54_0_FluidR3_GM_sf2_file],
  [55, perc._drum_55_0_FluidR3_GM_sf2_file],
  [56, perc._drum_56_0_FluidR3_GM_sf2_file],
  [57, perc._drum_57_0_FluidR3_GM_sf2_file],
  [58, perc._drum_58_0_FluidR3_GM_sf2_file],
  [59, perc._drum_59_0_FluidR3_GM_sf2_file],
  [60, perc._drum_60_0_FluidR3_GM_sf2_file],
  [61, perc._drum_61_0_FluidR3_GM_sf2_file],
  [62, perc._drum_62_0_FluidR3_GM_sf2_file],
  [63, perc._drum_63_0_FluidR3_GM_sf2_file],
  [64, perc._drum_64_0_FluidR3_GM_sf2_file],
  [65, perc._drum_65_0_FluidR3_GM_sf2_file],
  [66, perc._drum_66_0_FluidR3_GM_sf2_file],
  [67, perc._drum_67_0_FluidR3_GM_sf2_file],
  [68, perc._drum_68_0_FluidR3_GM_sf2_file],
  [69, perc._drum_69_0_FluidR3_GM_sf2_file],
  [70, perc._drum_70_0_FluidR3_GM_sf2_file],
  [71, perc._drum_71_0_FluidR3_GM_sf2_file],
  [72, perc._drum_72_0_FluidR3_GM_sf2_file],
  [73, perc._drum_73_0_FluidR3_GM_sf2_file],
  [74, perc._drum_74_0_FluidR3_GM_sf2_file],
  [75, perc._drum_75_0_FluidR3_GM_sf2_file],
  [76, perc._drum_76_0_FluidR3_GM_sf2_file],
  [77, perc._drum_77_0_FluidR3_GM_sf2_file],
  [78, perc._drum_78_0_FluidR3_GM_sf2_file],
  [79, perc._drum_79_0_FluidR3_GM_sf2_file],
  [80, perc._drum_80_0_FluidR3_GM_sf2_file],
  [81, perc._drum_81_0_FluidR3_GM_sf2_file]
])

export type PitchClass = string
export type Octave = number

export type Duration = {
  _scamperTag: 'struct',
  kind: 'dur',
  fields: [number, number]
}
export const dur = (num: number, den: number): Duration => ({ _scamperTag: 'struct', kind: 'dur', fields: [num, den] })

const isPitchClass = (s: string): boolean =>
  /^[A-Ga-g][#b]{0,2}$/.test(s)

const isOctave = (n: number): boolean =>
  n >= 0 && n <= 10

const isValidMidiNote = (n: number): boolean =>
  n >= 0 && n <= 127

export type Note = { renderAs: 'composition', tag: 'note', note: number, duration: Duration }
const note = (note: number, duration: Duration): Note => ({
  renderAs: 'composition', tag: 'note', note, duration
})

export type NoteFreq = { renderAs: 'composition', tag: 'note-freq', freq: number, duration: Duration }
const noteFreq = (freq: number, duration: Duration): NoteFreq => ({
  renderAs: 'composition', tag: 'note-freq', freq, duration
})

const repeatPrim: L.Prim = async (env, args, app) =>
  Utils.checkArgsResult('repeat', ['integer?', 'composition'], undefined, args, app).asyncAndThen(async _ => {
    const n = args[0] as number
    if (n < 0) {
      return runtimeError(msg('error-precondition-not-met', 'repeat', 1, 'non-negative integer', Pretty.expToString(0, L.nlevalue(args[0]))))
    } else {
      return evaluateExp(env,
        L.nleif(
          L.nlecall(L.nlevar('='), [L.nlevalue(n), L.nlenumber(0)]),
          L.nlevar('empty'),
          L.nlecall(L.nlevar('seq'), [
            L.nlevalue(args[1]),
            L.nlecall(L.nlevar('repeat'), [
              L.nlecall(L.nlevar('-'), [L.nlevalue(n), L.nlenumber(1)]),
              L.nlevalue(args[1])
            ])
          ])
        )
      )
    }
  })

type Empty = { renderAs: 'composition', tag: 'empty' }
const empty = (): Empty => ({ renderAs: 'composition', tag: 'empty' })

type Rest = { renderAs: 'composition', tag: 'rest', duration: Duration }
const rest = (duration: Duration): Rest => ({ renderAs: 'composition', tag: 'rest', duration })

type Trigger = { renderAs: 'composition', tag: 'trigger', fn: L.FunctionType }
const trigger = (fn: L.FunctionType): Trigger => {
  return ({ renderAs: 'composition', tag: 'trigger', fn })
}

type Par = { renderAs: 'composition', tag: 'par', notes: Composition[] }
const par = (notes: Composition[]): Par => ({ renderAs: 'composition', tag: 'par', notes })

type Seq = { renderAs: 'composition', tag: 'seq', notes: Composition[] }
const seq = (notes: Composition[]): Seq => ({ renderAs: 'composition', tag: 'seq', notes })

type Pickup = { renderAs: 'composition', tag: 'pickup', pickup: Composition, notes: Composition }
const pickup = (pickup: Composition, notes: Composition): Composition =>
  ({ renderAs: 'composition', tag: 'pickup', pickup, notes })

type ModKind = Percussion | PitchBend | Tempo | Dynamics | Instrument
type Percussion = { _scamperTag: 'struct', kind: 'mod', type: 'percussion', fields: []}
const percussion = (): Percussion => ({ _scamperTag: 'struct', kind: 'mod', type: 'percussion', fields: [] })

type PitchBend = { _scamperTag: 'struct', kind: 'mod', type: 'pitchBend', fields: [number] }
const pitchBend = (amount: number): PitchBend =>
  ({ _scamperTag: 'struct', kind: 'mod', type: 'pitchBend', fields: [amount] })
const pitchBendPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('bend', ['number?'], undefined, args, app).andThen(_ => {
    const amount = args[0] as number
    if (amount < -1 || amount > 1) {
      return runtimeError(msg('error-precondition-not-met', 'bend', 1, '-1 <= amount <= 1', Pretty.expToString(0, L.nlevalue(amount)), app))
    } else {
      return ok(pitchBend(amount))
    }
  }))

type Tempo = { _scamperTag: 'struct', kind: 'mod', type: 'tempo', fields: [Duration, number] }
const tempo = (beat: Duration, bpm: number): Tempo => ({ _scamperTag: 'struct', kind: 'mod', type: 'tempo', fields: [beat, bpm] })
const tempoPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('tempo', ['dur', 'number?'], undefined, args, app).andThen(_ => {
    const beat = args[0] as Duration
    const value = args[1] as number
    return value < 0
      ? runtimeError(msg('error-precondition-not-met', 'tempo', 1, 'tempo >= 0', Pretty.expToString(0, L.nlevalue(args[1]))), app)
      : ok(tempo(beat, value))
  }))

type Dynamics = { _scamperTag: 'struct', kind: 'mod', type: 'dynamics', fields: [number] }
const dynamics = (amount: number): Dynamics => ({ _scamperTag: 'struct', kind: 'mod', type: 'dynamics', fields: [amount] })
const dynamicsPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('dynamics', ['number?'], undefined, args, app).andThen(_ => {
    const amount = args[0] as number
    if (amount < 0 || amount > 127) {
      return runtimeError(msg('error-precondition-not-met', 'dynamics', 1, '0 <= amount <= 127', Pretty.expToString(0, L.nlevalue(args[0]))), app)
    } else {
      return ok(dynamics(amount))
    }
  }))

type Instrument = { _scamperTag: 'struct', kind: 'mod', type: 'instrument', fields: [number] }
const instrument = (program: number): Instrument =>
  ({ _scamperTag: 'struct', kind: 'mod', type: 'instrument', fields: [program] })
const instrumentPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('instrment', ['number?'], undefined, args, app).andThen(_ => {
    const amount = args[0] as number
    if (amount < 0 || amount > 127) {
      return runtimeError(msg('error-precondition-not-met', 'instrment', 1, '0 <= amount <= 127', Pretty.expToString(0, L.nlevalue(args[0]))), app)
    } else {
      return ok(instrument(amount))
    }
  }))

export type Mod = { renderAs: 'composition', tag: 'mod', note: Composition, mod: ModKind }
export const mod = (mod: ModKind, note: Composition): Mod => ({ renderAs: 'composition', tag: 'mod', note, mod })
export const modPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('mod', ['mod', 'composition'], undefined, args, app)
    .andThen(_ => ok(mod(args[0] as ModKind, args[1] as Composition))))

export type Composition = Empty | Note | NoteFreq | Rest | Trigger | Par | Seq | Pickup | Mod

const pitchQPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('pitch?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.valueIsString(args[0]) && isPitchClass(args[0] as string))))

const octavePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('octave?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.valueIsInteger(args[0]) && isOctave(args[0] as number))))

const durQPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('dur?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.valueIsStructKind(args[0], 'dur'))))

const durPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('dur', ['number?', 'number?'], undefined, args, app).andThen(_ =>
    ok({ _scamperTag: 'struct', kind: 'dur', fields: [args[0], args[1]] })))

const notePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('note', ['integer?', 'dur'], undefined, args, app).andThen(_ => {
    const midiNote = args[0] as number
    const dur = args[1] as Duration
    if (!isValidMidiNote(midiNote)) {
      return runtimeError(msg('error-precondition-not-met', 'note', 1,
        '0 <= amount <= 128', Pretty.expToString(0, L.nlevalue(args[0])), app))
    } else {
      return ok(note(midiNote, dur))
    }
  }))

const noteFreqPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('note', ['number?', 'dur'], undefined, args, app).andThen(_ => {
    const freq = args[0] as number
    const dur = args[1] as Duration
    if (freq < 0 && freq > 4000) {
      return runtimeError(msg('error-precondition-not-met', 'note-freq', 1,
        '0 <= frequency <= 4000', Pretty.expToString(0, L.nlevalue(args[0])), app))
    } else {
      return ok(noteFreq(freq, dur))
    }
  }))

const restPrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('rest', ['dur'], undefined, args, app)
  if (argErr) { return Promise.resolve(argErr) }
  const dur = args[0] as Duration
  return Promise.resolve(ok(rest(dur)))
}

const triggerPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('trigger', ['procedure?'], undefined, args, app).andThen(_ => {
    const proc = args[0] as L.FunctionType
    return ok(trigger(proc))
  }))

const parPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('par', [], 'composition', args, app).andThen(_ =>
    ok(par(args as Composition[]))))

const seqPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('seq', [], 'composition', args, app).andThen(_ =>
    ok(seq(args as Composition[]))))

const pickupPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('pickup', ['composition', 'composition'], undefined, args, app).andThen(_ =>
    ok(pickup(args[0] as Composition, args[1] as Composition))))

const numeratorPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('numerator', ['dur'], undefined, args, app).andThen(_ =>
    ok((args[0] as Duration).fields[0])))

const denominatorPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('denominator', ['dur'], undefined, args, app).andThen(_ =>
    ok((args[0] as Duration).fields[1])))

const playCompositionPrim: L.Prim = (env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('play-composition', ['composition'], undefined, args, app).andThen(_ => {
    playComposition(env, args[0] as Composition)
    return ok(undefined)
  }))

const musicEntry = (prim: L.Prim, docs?: L.Doc) => L.entry(L.vprim(prim), 'music', undefined, docs)

export const musicLib: L.Env = new L.Env([
  ['pitch?', musicEntry(pitchQPrim, Docs.pitch)],
  ['octave?', musicEntry(octavePrim, Docs.octave)],
  ['dur?', musicEntry(durQPrim, Docs.durQ)],
  ['dur', musicEntry(durPrim, Docs.dur)],
  ['numerator', musicEntry(numeratorPrim, Docs.numerator)],
  ['denominator', musicEntry(denominatorPrim, Docs.denominator)],
  ['empty', L.entry(empty(), 'music', undefined, Docs.empty)],
  ['note', musicEntry(notePrim, Docs.note)],
  ['note-freq', musicEntry(noteFreqPrim, Docs.noteFreq)],
  ['rest', musicEntry(restPrim, Docs.rest)],
  ['par', musicEntry(parPrim, Docs.par)],
  ['seq', musicEntry(seqPrim, Docs.seq)],
  ['pickup', musicEntry(pickupPrim, Docs.pickup)],
  ['mod', musicEntry(modPrim, Docs.mod)],
  ['percussion', L.entry(percussion(), 'music', undefined, Docs.percussion)],
  ['bend', musicEntry(pitchBendPrim, Docs.bend)],
  ['tempo', musicEntry(tempoPrim, Docs.tempo)],
  ['dynamics', musicEntry(dynamicsPrim, Docs.dynamics)],
  ['instrument', musicEntry(instrumentPrim, Docs.instrument)],
  ['trigger', musicEntry(triggerPrim, Docs.trigger)],
  ['repeat', musicEntry(repeatPrim, Docs.repeat)],
  ['wn', L.entry(dur(1, 1), 'music', undefined, Docs.wn)],
  ['hn', L.entry(dur(1, 2), 'music', undefined, Docs.hn)],
  ['qn', L.entry(dur(1, 4), 'music', undefined, Docs.qn)],
  ['en', L.entry(dur(1, 8), 'music', undefined, Docs.en)],
  ['sn', L.entry(dur(1, 16), 'music', undefined, Docs.sn)],
  ['tn', L.entry(dur(1, 32), 'music', undefined, Docs.tn)],
  ['play-composition', musicEntry(playCompositionPrim, Docs.playComposition)]
])

type MidiMsg = {
  tag: 'midi',
  time: number,
  duration: number,
  note: number,
  instrument: number
}

type TriggerMsg = {
  tag: 'trigger',
  time: number,
  callback: L.FunctionType
}

type Msg = MidiMsg | TriggerMsg

const midiMsg = (time: number, duration: number, note: number, instrument: number): Msg =>
  ({ tag: 'midi', time, duration, note, instrument })

const triggerMsg = (time: number, callback: L.FunctionType): Msg =>
  ({ tag: 'trigger', time, callback })

function ratioToDouble (ratio: Duration) {
  return ratio.fields[0] / ratio.fields[1]
}

function durationToTimeMs (beat: Duration, bpm: number, dur: Duration) {
  return ratioToDouble(dur) / (ratioToDouble(beat) * bpm) * 60 * 1000
}

// eslint-disable-next-line no-unused-vars
function freqToNoteOffset (freq: number): { note: number, offset: number } {
  const value = Math.log2(freq / 440) * 12 + 69
  const note = Math.floor(value)
  // N.B., assume a pitch bend of _two_ semitones, so we want half the fractional part
  // to obtain the percentage to bend within that note.
  const offset = (value - note) / 2
  return { note, offset }
}

// eslint-disable-next-line no-unused-vars
function _pitchBendF (_ch: number, _amt: number): void {
  // N.B., JZZ.MIDI doesn't define pitchBendF for some reason...
  // return (JZZ.MIDI as any).pitchBendF(ch, amt)
  throw new Error('NOT IMPLEMENTED AHHH')
}

function compositionToMsgs (
  beat: Duration, bpm: number, velocity: number, startTime: number,
  instrument: number, composition: Composition): { endTime: number, msgs: Msg[] } {
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
            durationToTimeMs(beat, bpm, composition.duration),
            composition.note,
            instrument
          )
        ]
      }
    }
    case 'note-freq': {
      const endTime = startTime + durationToTimeMs(beat, bpm, composition.duration)
      // const { note, offset } = freqToNoteOffset(composition.freq)
      // TODO: add pitch bend to msgs
      return {
        endTime,
        msgs: [
        ]
      }
      // return {
      //   endTime,
      //   msgs: [
      //     midiMsg(
      //       startTime,
      //       pitchBendF(0, offset)
      //     ),
      //     midiMsg(
      //       startTime,
      //       JZZ.MIDI.noteOn(program, note, velocity)
      //     ),
      //     midiMsg(
      //       endTime,
      //       JZZ.MIDI.noteOff(program, note, velocity)
      //     ),
      //     midiMsg(
      //       endTime,
      //       pitchBendF(0, 0)
      //     )
      //   ]
      // }
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
        const result = compositionToMsgs(beat, bpm, velocity, startTime, instrument, note)
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
        const result = compositionToMsgs(beat, bpm, velocity, time, instrument, note)
        msgs.push(...result.msgs)
        time = result.endTime
      })
      msgs.sort((c1, c2) => c1.time - c2.time)
      return { endTime: time, msgs }
    }

    case 'pickup': {
      const pickup = compositionToMsgs(beat, bpm, velocity, startTime, instrument, composition.pickup)
      const pickupDuration = pickup.endTime - startTime
      let notes: { endTime: number, msgs: Msg[] } | undefined
      // If the pickup would start in negative time, then rebase the composition to start
      // with the pickup instead.
      if (startTime - pickupDuration < 0) {
        pickup.msgs.forEach(msg => {
          msg.time += pickupDuration
        })
        notes = compositionToMsgs(beat, bpm, velocity, pickupDuration, instrument, composition.notes)

      // Otherwise, rebase pickup to start before the composition.
      } else {
        pickup.msgs.forEach(msg => {
          msg.time -= pickupDuration
        })
        notes = compositionToMsgs(beat, bpm, velocity, startTime, instrument, composition.notes)
      }
      const msgs: Msg[] = []
      msgs.push(...pickup.msgs)
      msgs.push(...notes.msgs)
      return { endTime: notes.endTime, msgs }
    }

    case 'mod': {
      if (composition.mod.type === 'percussion') {
        return compositionToMsgs(beat, bpm, velocity, startTime, 128, composition.note)
      } else if (composition.mod.type === 'pitchBend') {
        const msgs: Msg[] = []
        const data = compositionToMsgs(beat, bpm, velocity, startTime, instrument, composition.note)
        // TODO: handle pitch bends
        // msgs.push(midiMsg(startTime, pitchBendF(0, composition.mod.fields[0])))
        // msgs.push(...data.msgs)
        // msgs.push(midiMsg(data.endTime, pitchBendF(0, 0)))
        return { msgs, endTime: data.endTime }
      } else if (composition.mod.type === 'tempo') {
        return compositionToMsgs(composition.mod.fields[0], composition.mod.fields[1], velocity, startTime, instrument, composition.note)
      } else if (composition.mod.type === 'dynamics') {
        return compositionToMsgs(beat, bpm, composition.mod.fields[0], startTime, instrument, composition.note)
      } else if (composition.mod.type === 'instrument') {
        return compositionToMsgs(beat, bpm, velocity, startTime, composition.mod.fields[0], composition.note)
      } else {
        throw new ICE('compositionToMsgs', `unknown mod tag: ${composition.mod}`)
      }
    }
  }
}

export function playComposition (env: L.Env, composition: Composition): number {
  const msgs = compositionToMsgs(dur(1, 4), 120, 64, 0, 0, composition).msgs
  const triggers = msgs.filter(msg => msg.tag === 'trigger')
  const startTime = audioContext.currentTime

  // Enqueue notes
  for (const msg of msgs) {
    // const elapsed = audioContext.currentTime - startTime
    if (msg.tag === 'midi' && msg.instrument < 128) {
      player.queueWaveTable(audioContext, audioContext.destination, instrMap.get(msg.instrument)!, startTime + msg.time / 1000, msg.note, msg.duration / 1000, 1.0)
    } else if (msg.tag === 'midi' && msg.instrument === 128) {
      player.queueWaveTable(audioContext, audioContext.destination, percMap.get(msg.note)!, startTime + msg.time / 1000, msg.note, msg.duration / 1000, 1.0)
    }
  }

  // Set up a timer to discharge triggers

  let i = 0
  // eslint-disable-next-line @typescript-eslint/no-misused-promises
  const id = window.setInterval(async () => {
    // N.B., in milliseconds
    const now = window.performance.now()
    while (i < triggers.length) {
      const trigger = triggers[i]
      if (trigger.time + startTime * 1000 <= now) {
        await evaluateExp(env, L.nlecall(L.nlevalue((trigger as TriggerMsg).callback), []))
        i += 1
        continue
      } else {
        return Promise.resolve(undefined)
      }
    }
    clearInterval(id)
    return Promise.resolve(undefined)
  })
  return id
}
