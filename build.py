#! /usr/bin/env python3

import os
import pathlib
import shutil
import subprocess

DIR = pathlib.Path(__file__).parent
OUT = DIR / 'out'

if OUT.exists():
   shutil.rmtree(OUT)
os.mkdir(OUT)
os.chdir(OUT)

COMPILE_FLAGS = "/std:c++17 /EHsc".split(' ')
FILES = "..\klc-slideshow.cpp".split(' ')
LINK_FLAGS = "/link /subsystem:WINDOWS".split(' ')
OUTPUT = "klc-slideshow.scr"

def run(*args):
   print('> ', args, '\n')
   return subprocess.run(args, check=True);

link_flags = LINK_FLAGS + ['/out:'+OUTPUT]
run('cl', *COMPILE_FLAGS, *FILES, *link_flags);
print('Build complete.')
