package dsl

import scala.annotation.tailrec

final case class Pitch(midiPitch: Int):
  def sharp: Pitch = Pitch(midiPitch + 1)
  def flat: Pitch = Pitch(midiPitch - 1)

object Pitch:
  def C(octave: Int): Pitch = Pitch(octave * 12 + 12)
  def D(octave: Int): Pitch = Pitch(octave * 12 + 14)
  def E(octave: Int): Pitch = Pitch(octave * 12 + 16)
  def F(octave: Int): Pitch = Pitch(octave * 12 + 17)
  def G(octave: Int): Pitch = Pitch(octave * 12 + 19)
  def A(octave: Int): Pitch = Pitch(octave * 12 + 21)
  def B(octave: Int): Pitch = Pitch(octave * 12 + 23)

final case class Velocity(val midiVelocity: Int):
  def getNormalisedVelocity: Double = this.midiVelocity / 127.0

object Velocity:
  val TheSilentTreatment = Velocity(0)
  val Softest = Velocity(10)
  val Soft = Velocity(50)
  val Medium = Velocity(80)
  val Assertively = Velocity(100)
  val Loud = Velocity(115)
  val OnFull = Velocity(127)

final case class Duration(val duration: Double):
  def dotted: Duration = Duration(duration * 1.5)

object Duration:
  val Whole = Duration(1.0)
  val Half = Duration(0.5)
  val Quarter = Duration(0.25)
  val Eighth = Duration(0.125)
  val Sixteenth = Duration(0.0625)
  val ThirtySecond = Duration(0.03125)

sealed trait MusicEvent:
  def +(secondEvent: MusicEvent): MusicEvent = Melody(this, secondEvent)
  def |(secondEvent: MusicEvent): MusicEvent =
    Melody(this, secondEvent) // at end of bar

  def *(repetitions: Int): MusicEvent = repeat(repetitions)
  def repeat(repetitions: Int): MusicEvent = {
    @tailrec
    def loop(count: Int, accum: MusicEvent): MusicEvent = {
      if count <= 1 then accum
      else loop(count - 1, Melody(this, accum))
    }

    loop(repetitions, this)
  }

final case class Note(
    pitch: Pitch,
    duration: Duration = Duration.Quarter,
    velocity: Velocity = Velocity.OnFull
) extends MusicEvent:
  def sharp: Note = this.copy(pitch = pitch.sharp)
  def flat: Note = this.copy(pitch = pitch.flat)
  def dotted: Note = this.copy(duration = duration.dotted)
  def whole: Note = this.copy(duration = Duration.Whole)
  def half: Note = this.copy(duration = Duration.Half)
  def quarter: Note = this.copy(duration = Duration.Quarter)
  def eighth: Note = this.copy(duration = Duration.Eighth)
  def sixteenth: Note = this.copy(duration = Duration.Sixteenth)
  def thirtySecond: Note = this.copy(duration = Duration.ThirtySecond)
  def softest: Note = this.copy(velocity = Velocity.Softest)
  def soft: Note = this.copy(velocity = Velocity.Soft)
  def medium: Note = this.copy(velocity = Velocity.Medium)
  def assertively: Note = this.copy(velocity = Velocity.Assertively)
  def loud: Note = this.copy(velocity = Velocity.Loud)
  def onFull: Note = this.copy(velocity = Velocity.OnFull)

object Note:
  def C(octave: Int): Note = Note(Pitch.C(octave))
  def D(octave: Int): Note = Note(Pitch.D(octave))
  def E(octave: Int): Note = Note(Pitch.E(octave))
  def F(octave: Int): Note = Note(Pitch.F(octave))
  def G(octave: Int): Note = Note(Pitch.G(octave))
  def A(octave: Int): Note = Note(Pitch.A(octave))
  def B(octave: Int): Note = Note(Pitch.B(octave))

final case class Melody(left: MusicEvent, right: MusicEvent) extends MusicEvent

final case class Harmony(notes: Note*) extends MusicEvent

final case class Rest(duration: Duration) extends MusicEvent

def perform(event: MusicEvent): Unit = {
  import javax.sound.midi.*

  val TICKS_PER_QUARTER = 48
  val END_OF_TRACK = 0x2f

  val sequence = Sequence(Sequence.PPQ, TICKS_PER_QUARTER)
  val track = sequence.createTrack()

  def addNote(pitch: Int, length: Int, velocity: Int, tick: Int): Unit = {
    val onMessage = ShortMessage(ShortMessage.NOTE_ON, 1, pitch, velocity)
    val offMessage = ShortMessage(ShortMessage.NOTE_OFF, 1, pitch, velocity)

    track.add(MidiEvent(onMessage, tick))
    track.add(MidiEvent(offMessage, tick + length))
  }

  def render(event: MusicEvent, tick: Int): Int = {
    event match
      case Note(pitch, duration, velocity) =>
        val length = (duration.duration * 4 * TICKS_PER_QUARTER).toInt
        addNote(pitch.midiPitch, length, velocity.midiVelocity, tick)
        tick + length
      case Rest(duration) =>
        val length = (duration.duration * 4 * TICKS_PER_QUARTER).toInt
        tick + length
      case Harmony(notes*) =>
        val length = notes.map(note => render(note, tick)).max
        tick + length
      case Melody(left, right) =>
        val mid = render(left, tick)
        render(right, mid)
  }

  render(event, 0)

  val sequencer = MidiSystem.getSequencer()
  sequencer.open()
  sequencer.setTempoInBPM(60)
  sequencer.setSequence(sequence)
  sequencer.start();
  sequencer.addMetaEventListener { metaMsg =>
    if metaMsg.getType() == END_OF_TRACK then sequencer.close()
  }
}

@main def musicDemo(): Unit = {
  import Note.*

  val song =
    C(4).eighth * 2 + G(4).eighth * 2 + A(4).eighth * 2 + G(4) |
    F(4).eighth * 2 + E(4).eighth * 2 + D(4).eighth * 2 + C(4) |
    Rest(Duration.Half) + Harmony(C(4).half, E(4).half, G(4).half)

  perform(song)
}