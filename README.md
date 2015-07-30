Ada synth lib
=============

Ada synth lib is a very simple synthethizer library with a mostly recreative
purpose. The design goal is to make a library from which you can make synthesis
musical instruments that can run on bareboard devices such as the STM32F4 board
or the Rpi.

Here is a small demo of the library:

<iframe width="420" height="315" src="https://www.youtube.com/embed/2eiWnN1xWcs" frameborder="0" allowfullscreen></iframe>

Build
=====

You'll need AdaCore's latest GPL release, that you can get through
[Adacore libre site](http://libre.adacore.com/)

Although the lib is probably building with the FSF versions of the tools, I did
not test it myself.

For native:

~~~shell
$ gprbuild -p
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
