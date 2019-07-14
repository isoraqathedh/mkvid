# mkvid

This is a project to allow one to create videos
with moving text, images and geometrical figures.
It is essentially a bit of a [manim][] clone, 
but more inspired by "PowerPoint animations" à la [Vhèkkyo Boy][].

Unlike other projects such as `lut` and `bocproc`,
this is not a scripting language, but an actual library 
that's meant to integrate with the rest of Common Lisp.

[manim]: https://github.com/3b1b/manim
[Vhèkkyo Boy]: https://www.facebook.com/VhekkyoBoy/

# Concept
A video contains a number of **scenes**, with a number of **actors**
that can move around, grow, shrink or otherwise transform.
It is heavily built around `flare`, a particle animation library,
and much of its features is built around it.

# How to make your own animation
The first step you need to do is to invent some **actors**.
These are simply classes that have one of the following as a superclass:

- `entity`
- `flare:container-unit`

Some common types of actors are included, such as rectangles, text
and an image-based sprite.

Then you need to define a `paint` method on it.
This would automatically call `flare:paint` in such a way
that you can assume the object is drawn with its location at (0, 0).
Keep in mind that this `paint` uses the `progn` method combination,
so all methods that are defined on the object's superclass will be run.

Also keep in mind that you would be drawing on a `qpainter`,
which means you would need to use `qtools` or similar to do the drawing.

Once that's done you can create the scene.
This scene is a `presentation` and the syntax goes like this:

```
    (define-presentation name (width height) 
       ...progression-definitions...)

```

where `progression-definitions` is exactly as in flare's `progression`s.

After that, you can preview the animation by running

```
    (present presentation-name)
```

Where `presentation-name` is the name of your presentation.
This would pull up a GUI.
The GUI will show your presentation at 30 frames per second.
However, you do not decide on a frame rate at all
while composing the presentation;
anything can happen wherever a double floating point number exists.
You only decide on the frame rate when it is time to export the video.

# Things to come
## Definitely

- Export to video
- Fully functional UI
- Time manipulation
- Seeking

## Possibly

- Cue times:
  Occasionally, it makes more sense to have an accumulator time on the background,
  which you could then manipulate via `incf` or similar.
  Also, in other times one might find some times to be easier stated at a definition at the top,
  and then pop them out later via `pop` and `cdr`.
- Rotations
- Scaling

# License

MIT

