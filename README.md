Ada synth lib
=============

Ada synth lib is a very simple synthetizer library with a mostly recreative
purpose. The design goal is to make a library from which you can make synthesis
musical instruments that can run on bareboard devices such as the STM32F4 board
or the Rpi.

Build
=====

You'll probably need AdaCore's latest GPL release, that you can get through
[Adacore libre site](http://libre.adacore.com/)

Although the lib is probably building with the FSF versions of the tools, I did
not test it myself.

For native:

$ gprbuild -p

For bareboard/sfp:

$ gprbuild -p -f -Xtarget=bareboard
