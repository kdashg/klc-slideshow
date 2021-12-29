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
OUTPUT = "klc-slideshow.exe" # VS doesn't like debugging a .scr
#OUTPUT = "klc-slideshow.scr"

if 1:
   COMPILE_FLAGS.append('/Zi') # debug info

def run(*args):
   print('> ', args, '\n')
   try:
      return subprocess.run(args, check=True);
   except:
      print('');
      raise

link_flags = LINK_FLAGS + ['/out:'+OUTPUT]
run('cl', *COMPILE_FLAGS, *FILES, *link_flags);
print('Build complete.')
