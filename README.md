# klc-slideshow

Let's make a basic slideshow screensaver for Windows!


## Building

1. Have
  1. Visual Studio (I have 2019)
  2. Python 3 (I have 3.9)
2. Open e.g. "x64 Native Tools Command Prompt for VS 2019".
3. `build.py`

Output will be `out/klc-slideshow.scr`.


## View Debug Output

We use OutputDebugString, which you can view using Microsoft's
sysinternal's DebugView:
https://docs.microsoft.com/en-us/sysinternals/downloads/debugview


## Installing

Copy to `\Windows` or `\Windows\System32`.

Select it via "Change screen saver" or "Screen Saver Settings".
It should show up as "klc-slideshow".


## Thanks

### Rachel Grey: "Writing an OpenGL Screensaver for Windows"

I'm indebted to Rachel Grey for her 2002(!) tutorial:
http://www.cityintherain.com/howtoscr.html

### Microsoft: "Get Started with Win32 and C++"
In particular, "Module 1. Your First Windows Program":
https://docs.microsoft.com/en-us/windows/win32/learnwin32/your-first-windows-program
