#! /usr/bin/env python3

import os
import pathlib
import shutil
import subprocess

DIR = pathlib.Path(__file__).parent
OUT = DIR / 'out'

if not OUT.exists():
   os.mkdir(OUT)
os.chdir(OUT)

# cl user32.lib advapi32.lib gdi32.lib klc-slideshow.cpp /link /subsystem:WINDOWS
subprocess.run('cl /std:c++17 /EHsc ..\klc-slideshow.cpp /link /subsystem:WINDOWS', check=True);
print('Build complete.')
