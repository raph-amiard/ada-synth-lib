#! /usr/bin/python

import argparse

parser = argparse.ArgumentParser()
parser.add_argument(
    "--with-soundio", help="Compile with soundio support",
    action="store_true"
)

parser.add_argument(
    "--soundio-path", help="Give path to soundio ada bindings",
    default="ada-soundio"
)

args = parser.parse_args()
in_file_content = open("./ada_synth_lib.gpr.in").read()
source_dirs = ["src"]
soundio_include = ""

if args.with_soundio:
    source_dirs.append("soundio-src")
    soundio_include = 'with "{}";'.format(args.soundio_path + "/soundio.gpr")

with open("./ada_synth_lib.gpr", "w") as prj_file:
    prj_file.write(in_file_content.format(
        source_dirs=", ".join('"{}"'.format(s) for s in source_dirs),
        soundio_include=soundio_include
    ))
