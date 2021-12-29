// Thank you to Rachel Grey:
// http://www.cityintherain.com/howtoscr.html

#pragma comment(lib, "comctl32.lib")
#pragma comment(lib, "user32.lib")
#pragma comment(lib, "advapi32.lib")
#pragma comment(lib, "gdi32.lib")
#pragma comment(lib, "D3D11.lib")
#pragma comment(lib, "Dcomp.lib")

#ifndef UNICODE
#define UNICODE
#endif

// ifdef UNICODE, you'll need scrnsavw.lib instead of scrnsave.lib
#pragma comment(lib, "scrnsavw.lib")

#include <windows.h>
#include <scrnsave.h>
#include <d3d11.h>
#include <dcomp.h>
#include <dxgi.h>

#include <array>
#include <chrono>
#include <cstdlib>
#include <cstdint>
#include <memory>
#include <sstream>
#include <vector>

using std::move;
using u32 = uint32_t;

// -

#define REL_ASSERT(X,M) \
   do { \
      if (X) {} else { \
         dout() << __FILE__ << ":" << __LINE__ << ": ASSERT (" << #X << ") FAILED: " M; \
         std::abort(); \
      } \
   } while (false);

// -

struct DebugStream {
   std::stringstream stream;

   DebugStream(const DebugStream&) = delete;
   DebugStream& operator=(const DebugStream&) = delete;

   DebugStream() {
      // The biggest eyeroll to whoever decided that e.g. 10000000.1
      // should truncate by default.
      stream.precision(std::numeric_limits<long double>::digits10 + 1);
   }

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

// -

struct vec3 final {
   float x = 0;
   float y = 0;
   float z = 0;
};

// -

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
// HAndle Object
// e.g. HBRUSH, destroyed by DeleteObject().
template<typename T>
class hao final {
   T mHandle = nullptr;

public:
   hao() = default;
   explicit hao(const T handle) : mHandle(handle) {}
   hao(std::nullptr_t) {}

   ~hao() {
      reset(nullptr);
   }

   // No copy!
   hao(const hao&) = delete;
   hao& operator=(const hao&) = delete;

   // Move ok!
   hao(hao&& rhs) {
      *this = std::move(rhs);
   }
   hao& operator=(hao&& rhs) {
      reset(nullptr);
      std::swap(mHandle, rhs.mHandle);
      return *this;
   }

   // Null-assign ok!
   hao& operator=(std::nullptr_t) {
      reset(nullptr);
      return *this;
   }

   void reset(const T handle) {
      if (mHandle) {
         DeleteObject(mHandle);
      }
      mHandle = handle;
   }

   T operator*() const { return mHandle; }
};

template<typename T>
hao<T> make_hao(const T handle) {
   return hao<T>(handle);
}

// -
// Strong Pointer sp<T>

template<typename T>
class sp final {
   using Type = T;

   T* mPtr = nullptr;

public:
   sp() = default;

   explicit sp(T* const rhs) {
      *this = rhs;
   }

   sp(const sp& rhs) {
      *this = rhs;
   }
   sp(sp&& rhs) {
      *this = move(rhs);
   }

   template<typename U>
   sp(const sp<U>& rhs) {
      *this = rhs;
   }
   template<typename U>
   sp(sp<U>&& rhs) {
      *this = move(rhs);
   }

   ~sp() {
      reset(nullptr);
   }

   // -
   // From pointer
   sp& operator=(std::nullptr_t) {
      reset(rhs);
      return *this;
   }

   sp& operator=(T* const rhs) {
      reset(rhs);
      return *this;
   }

   // Copy
   sp& operator=(const sp& rhs) {
      *this = rhs.mPtr;
      return *this;
   }
   template<typename U>
   sp& operator=(const sp<U>& rhs) {
      *this = rhs.mPtr;
      return *this;
   }

   // Move
   sp& operator=(sp&& rhs) {
      reset(nullptr);
      std::swap(mPtr, rhs.mPtr);
      return *this;
   }
   template<typename U>
   sp& operator=(sp<U>&& rhs) {
      reset(nullptr);
      std::swap(mPtr, rhs.mPtr);
      return *this;
   }

   // -

   T* get() const { return mPtr; }
   operator T*() const { return mPtr; }

   void reset(T* ptr) {
      if (mPtr) {
         mPtr->Release();
      }
      mPtr = ptr;
      if (mPtr) {
         mPtr->AddRef();
      }
   }

   template<typename U = T>
   U*& setter_addrefs() {
      reset(nullptr);
      return *static_cast<U**>(&mPtr);
   }

   // -

   T* operator->() {
      REL_ASSERT(mPtr, "null-deref");
      return mPtr;
   }
   T* operator->() const {
      REL_ASSERT(mPtr, "null-deref");
      return mPtr;
   }

   T& operator*() {
      REL_ASSERT(mPtr, "null-deref");
      return *mPtr;
   }
   T& operator*() const {
      REL_ASSERT(mPtr, "null-deref");
      return *mPtr;
   }

   explicit operator bool() const {
      return mPtr;
   }

   // -
   // COM helpers

   /* E.g.
      sp<ID3D11Device> d3d;
      [...]
      const auto dxgi = d3d.qi<IDXGIDevice>(); // sp<IDXGIDevice>
      [...]
      sp<IDCompositionDevice> dcomp;
      DCompositionCreateDevice(dxgi, dcomp.UUID(),
                               &dcomp.setter_addrefs<void>());
    */

   template<typename U>
   sp<U> qi() const {
      sp<U> ret;
      (void)mPtr->QueryInterface(&ret.setter_addrefs());
      return ret;
   }

   template<>
   void*& setter_addrefs<void>() {
      auto& ret = setter_addrefs<T>();
      return *reinterpret_cast<void**>(&ret);
   }

   static auto UUID() {
       T* v{};
       return __uuidof(*v);
   }
};

// -

template<typename CallableT>
class ScopeExitT final {
    const CallableT mCallable;
    bool mSkipCalling = false;

public:
    ScopeExitT(CallableT&& callable)
        : mCallable(std::move(callable)) {}

    ~ScopeExitT() {
        if (mSkipCalling) return;
        mCallable();
    }
    void release() {
        mSkipCalling = true;
    }
};

template<typename CallableT>
auto scope_exit(CallableT&& callable) {
    return ScopeExitT<CallableT>(std::move(callable));
}

// -


constexpr uint32_t BLUE = 0xfacf5b;
constexpr uint32_t PINK = 0xb9abf5;
constexpr uint32_t WHITE = 0xffffff;
constexpr std::array STRIPES{
   BLUE, PINK, WHITE, PINK, BLUE,
};

hao<HBITMAP> make_flag_bitmap() {
   // 5x3 aspect ratio.
   const auto W = STRIPES.size() * 5;
   const auto H = STRIPES.size() * 3;
   RECT rect = {};
   rect.right = W;
   rect.bottom = H;

   std::vector<u32> data;
   data.reserve(W*H);
   const auto elems_per_stripe = W*H / STRIPES.size();
   for (const auto& color : STRIPES) {
      data.resize(data.size() + elems_per_stripe, color | 0xff000000);
   }

   auto bitmap = make_hao(CreateBitmap(W, H, 1, 32, data.data()));
   return bitmap;
}

void draw_flag(const HDC dc, const RECT& rect) {
   const auto height = rect.bottom - rect.top;
   dout() << "draw_flag height: " << height;
   constexpr auto count = STRIPES.size();
   for (size_t i = 0; i < count; i++) {
      const auto& xxbbggrr = STRIPES[i];
      const auto brush = make_hao(CreateSolidBrush(xxbbggrr));
      auto stripe_rect = rect;
      stripe_rect.top = rect.top + height * float(i) / count;
      stripe_rect.bottom = rect.top + height * float(i+1) / count;
      FillRect(dc, &stripe_rect, *brush);
   }
}

// -

class Renderer {
public:
   struct Data {
      HWND window = nullptr;
      sp<ID3D11Device> d3d;
      sp<IDCompositionDevice> dcomp;
      sp<IDCompositionTarget> dc_target;
      sp<IDCompositionVisual> flag_vis;
      sp<IDCompositionSurface> flag_surf;
   };
   const Data m;

   // -

   static std::unique_ptr<Renderer> Create(const HWND window) {
      auto m = Data{};
      m.window = window;
      u32 flags = D3D11_CREATE_DEVICE_BGRA_SUPPORT;
      constexpr auto FEATURE_LEVELS = std::array{
         D3D_FEATURE_LEVEL_11_1,
         D3D_FEATURE_LEVEL_11_0,
         D3D_FEATURE_LEVEL_10_1,
         D3D_FEATURE_LEVEL_10_0,
      };
      (void)D3D11CreateDevice(nullptr,
         D3D_DRIVER_TYPE_HARDWARE,
         nullptr,
         flags,
         FEATURE_LEVELS.data(), FEATURE_LEVELS.size(),
         D3D11_SDK_VERSION,
         &m.d3d.setter_addrefs(),
         nullptr,
         nullptr);
      if (!m.d3d) return nullptr;

      const auto dxgi = m.d3d.qi<IDXGIDevice>();
      if (!dxgi) return nullptr;

      DCompositionCreateDevice(dxgi, m.dcomp.UUID(),
                               &m.dcomp.setter_addrefs<void>());
      if (!m.dcomp) return nullptr;

      m.dcomp->CreateTargetForHwnd(window, true, &m.dc_target.setter_addrefs());
      if (!m.dc_target) return nullptr;

      m.dcomp->CreateVisual(&m.flag_vis.setter_addrefs());
      if (!m.flag_vis) return nullptr;

      // -
      // Create a visual from a bitmap.

      {
         const auto flag_bitmap = make_flag_bitmap();
         auto bm_info = BITMAP{};
         {
            auto ok = !!GetObject(*flag_bitmap, sizeof(bm_info), &bm_info);
            REL_ASSERT(ok, "GetObject");
         }
         dout() << "bm_info " << bm_info.bmWidth << "," << bm_info.bmHeight;

         m.dcomp->CreateSurface(bm_info.bmWidth, bm_info.bmHeight,
            DXGI_FORMAT_B8G8R8A8_UNORM, DXGI_ALPHA_MODE_IGNORE,
            &m.flag_surf.setter_addrefs());
         REL_ASSERT(m.flag_surf, "dcomp->CreateSurface");

         sp<IDXGISurface1> flag_surf_dxgi;
         auto draw_offset = POINT{};
         m.flag_surf->BeginDraw(nullptr, flag_surf_dxgi.UUID(),
               &flag_surf_dxgi.setter_addrefs<void>(), &draw_offset);
         REL_ASSERT(flag_surf_dxgi, "flag_surf->BeginDraw");
         const auto end_draw = scope_exit([&]() {
            dout() << "EndDraw()";
            m.flag_surf->EndDraw();
         });
         dout() << "Before EndDraw()";

         HDC dst_dc = nullptr;
         flag_surf_dxgi->GetDC(false, &dst_dc);
         REL_ASSERT(dst_dc, "flag_surf_dxgi->GetDC");
         if (!dst_dc) return nullptr;
         const auto release_dc = scope_exit([&]() {
            flag_surf_dxgi->ReleaseDC(nullptr);
         });

         const auto src_dc = CreateCompatibleDC(dst_dc);
         REL_ASSERT(src_dc, "CreateCompatibleDC");
         if (!src_dc) return nullptr;
         const auto release_src_dc = scope_exit([&]() {
            DeleteDC(src_dc);
         });

         const auto prev = SelectObject(src_dc, *flag_bitmap);
         const auto restore_bitmap = scope_exit([&]() {
            SelectObject(src_dc, prev);
         });
         dout() << "flag_surf->BeginDraw draw_offset " << draw_offset.x << "," << draw_offset.y;
         bool ok = BitBlt(dst_dc, draw_offset.x, draw_offset.y,
                     bm_info.bmWidth, bm_info.bmHeight,
                     src_dc, 0, 0, SRCCOPY);
         REL_ASSERT(ok, "BitBlit failed");
      }

      auto hr = m.flag_vis->SetContent(m.flag_surf);
      REL_ASSERT(SUCCEEDED(hr), "SetContent");

      return std::unique_ptr<Renderer>(new Renderer(std::move(m)));
   }

private:
   Renderer(Data&& m) : m(move(m)) {}

public:
   ~Renderer() = default;

   void Draw(const vec3& offset) const {
      //GetWindowRect
      auto hr = m.flag_vis->SetOffsetX(offset.x);
      REL_ASSERT(SUCCEEDED(hr), "SetOffsetX");
      hr = m.flag_vis->SetOffsetY(offset.y);
      REL_ASSERT(SUCCEEDED(hr), "SetOffsetY");

      hr = m.dcomp->Commit();
      REL_ASSERT(SUCCEEDED(hr), "dcomp->Commit");
      dout() << "dcomp->Commit()";

      auto stats = DCOMPOSITION_FRAME_STATISTICS{};
      m.dcomp->GetFrameStatistics(&stats);
      dout() << "timeFrequency: " << stats.timeFrequency.QuadPart;
      //dout() << "lastFrameTime: " << stats.lastFrameTime.QuadPart;
      const double per_sec = stats.timeFrequency.QuadPart;
      dout() << "lastFrameTime: " << stats.lastFrameTime.QuadPart / per_sec;
      dout() << "currentTime: " << stats.currentTime.QuadPart / per_sec;
      dout() << "nextEstimatedFrameTime: " << stats.nextEstimatedFrameTime.QuadPart / per_sec;
      const auto& rate = stats.currentCompositionRate;
      dout() << "currentCompositionRate: " << rate.Numerator << "/" << rate.Denominator;
   }
};

// -

namespace chrono = std::chrono;

class Slideshow {
public:
   struct Data final {
      HWND window;
      std::unique_ptr<Renderer> renderer;
      chrono::high_resolution_clock::time_point time_start;
   };
   const Data m;

   static std::unique_ptr<Slideshow> Create(const HWND window) {
      auto m = Data{};
      m.window = window;
      m.time_start = chrono::high_resolution_clock::now();

      m.renderer = Renderer::Create(window);
      if (!m.renderer) return nullptr;

      return std::unique_ptr<Slideshow>(new Slideshow(std::move(m)));
   }

   Slideshow(Data&& data) : m(std::move(data)) {}

   ~Slideshow() = default;
/*
   void OnPaint() {
      PAINTSTRUCT ps;
      const auto dc = BeginPaint(mWindow, &ps);

      const auto& win_rect = ps.rcPaint;
      //auto win_rect = RECT{};
      //GetClientRect(mWindow, &win_rect);

      dout() << "win_rect.top,bottom: " << win_rect.top << "," << win_rect.bottom;
      dout() << "win_rect.left,right: " << win_rect.left << "," << win_rect.right;
      draw_flag(dc, win_rect);

      EndPaint(mWindow, &ps);
   }
   */

   void OnPaint() {
      const auto now = chrono::high_resolution_clock::now();
      const auto rel_now = chrono::duration_cast<
            chrono::duration<float>>(now - m.time_start)
            .count();
      dout() << "OnPaint@" << rel_now << "s";

      constexpr float radius = 50;
      vec3 draw_p = {};
      draw_p.x = 50 + std::sin(rel_now) * radius;
      draw_p.y = 50 + std::cos(rel_now) * radius;
      dout() << "draw_p " << draw_p.x << "," << draw_p.y;

      if (1) {
         m.renderer->Draw(draw_p);
         ValidateRect(m.window, nullptr);
         //InvalidateRect(m.window, nullptr, false);
      } else {
         PAINTSTRUCT ps;
         const auto dc = BeginPaint(m.window, &ps);
         const auto& win_rect = ps.rcPaint;

         draw_flag(dc, win_rect);

         EndPaint(m.window, &ps);
      }
   }
};

//-

LRESULT WINAPI ScreenSaverProcW(HWND window, UINT msg, WPARAM wp, LPARAM lp) {

   static std::unique_ptr<Slideshow> sSlideshow;

   switch (msg) {
   case WM_CREATE:
      OutputDebugStringA("klc-slideshow WM_CREATE\n");
      sSlideshow = Slideshow::Create(window);
      REL_ASSERT(sSlideshow, "Slideshow::Create");
      return 0;

   case WM_DESTROY:
      sSlideshow = nullptr;
      return 0;

   // WM_DISPLAYCHANGE when resolution changes

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
