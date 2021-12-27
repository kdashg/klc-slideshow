// Thank you to Rachel Grey:
// http://www.cityintherain.com/howtoscr.html

#pragma comment(lib, "comctl32.lib")
#pragma comment(lib, "user32.lib")
#pragma comment(lib, "advapi32.lib")
#pragma comment(lib, "gdi32.lib")

#ifndef UNICODE
#define UNICODE
#endif

// ifdef UNICODE, you'll need scrnsavw.lib instead of scrnsave.lib
#pragma comment(lib, "scrnsavw.lib")

#include <windows.h>
#include <scrnsave.h>

#include <memory>
#include <cstdlib>
#include <cstdint>
#include <array>
#include <sstream>

//-

struct DebugStream {
   std::stringstream stream;

   template<typename T>
   DebugStream& operator<<(T&& a) {
      stream << a;
      return *this;
   }

   ~DebugStream() {
      stream << "\n";
      OutputDebugStringA(stream.str().c_str());
   }
};

inline DebugStream dout() {
   return {};
}

//-

struct Brush final {
   const HBRUSH handle;

   static std::shared_ptr<Brush> CreateSolid(const uint32_t aabbggrr) {
      const auto handle = CreateSolidBrush(aabbggrr);
      if (!handle) return nullptr;
      return std::make_shared<Brush>(handle);
   }

   Brush(HBRUSH handle) : handle(handle) {}

   ~Brush() {
      DeleteObject(handle);
   }

   operator HBRUSH() const { return handle; }
};

// -

class Slideshow {
   const HWND mWindow;

public:
   Slideshow(HWND window) : mWindow(window) {}

   ~Slideshow() = default;

   void OnPaint() {
      PAINTSTRUCT ps;
      const auto dc = BeginPaint(mWindow, &ps);

      const auto& win_rect = ps.rcPaint;
      //auto win_rect = RECT{};
      //GetClientRect(mWindow, &win_rect);

      dout() << "win_rect.top,bottom: " << win_rect.top << "," << win_rect.bottom;
      dout() << "win_rect.left,right: " << win_rect.left << "," << win_rect.right;

      constexpr uint32_t BLUE = 0xfacf5b;
      constexpr uint32_t PINK = 0xb9abf5;
      constexpr uint32_t WHITE = 0xffffff;
      constexpr std::array STRIPES{
         BLUE, PINK, WHITE, PINK, BLUE,
      };

      const auto win_height = win_rect.bottom - win_rect.top;
      dout() << "win_height: " << win_height;
      constexpr auto count = STRIPES.size();
      for (size_t i = 0; i < count; i++) {
         const auto& color = STRIPES[i];
         const auto brush = Brush::CreateSolid(color);
         auto rect = win_rect;
         rect.top = win_rect.top + win_height * float(i) / count;
         rect.bottom = win_rect.top + win_height * float(i+1) / count;
         FillRect(dc, &rect, *brush);
      }

      EndPaint(mWindow, &ps);
   }
};

//-

LRESULT WINAPI ScreenSaverProcW(HWND window, UINT msg, WPARAM wp, LPARAM lp) {

   static std::unique_ptr<Slideshow> sSlideshow;

   switch (msg) {
   case WM_CREATE:
      sSlideshow.reset(new Slideshow(window));
      return 0;

   case WM_DESTROY:
      sSlideshow = nullptr;
      return 0;

   case WM_PAINT:
      OutputDebugStringA("WM_PAINT\n");
      if (sSlideshow) {
         sSlideshow->OnPaint();
      }
      return 0;
   }

   return DefScreenSaverProc(window, msg, wp, lp);
}

BOOL WINAPI
ScreenSaverConfigureDialog(HWND, UINT, WPARAM, LPARAM) {
   return false;
}

BOOL WINAPI RegisterDialogClasses(HANDLE) {
   return true;
}
