Ada synth lib
=============

Ada synth lib is a very simple synthethizer library with a mostly recreative
purpose. The design goal is to make a library from which you can make synthesis
musical instruments that can run on bareboard devices such as the STM32F4 board
or the Rpi.

Here is a [small demo of the library (youtube video)](https://www.youtube.com/watch?v=2eiWnN1xWcs)

Build
=====

You'll need AdaCore's latest GPL release, that you can get through
[Adacore libre site](http://libre.adacore.com/), or you can use the FSF
compiler available on most Linux distributions. It has been tested to work with
Debian Stable and Ubuntu 15.04.

For native:

~~~shell
$ gprbuild -p ada_synth_lib.gpr
~~~

For native with libsoundio support:

~~~shell
$ gprbuild -p ada_synth_lib_soundio.gpr
~~~

For bareboard/sfp:

~~~shell
$ gprbuild -p -f -Xtarget=bareboard
~~~

If you want to build the examples, run

~~~shell
$ gprbuild -p examples/asl_examples.gpr
~~~

Producing sound from the examples will require you to have some command line
program that you can pipe sound into and that will play it on your speakers.
Here is how you'd do it with `aplay`:

~~~shell
$ examples/obj/audio | aplay -f S16_LE -c1 -r44100
~~~
