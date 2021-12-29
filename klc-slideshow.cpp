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
#include <iomanip>      // std::setprecision
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

namespace chrono = std::chrono;

static const chrono::high_resolution_clock::time_point TIME_START =
      chrono::high_resolution_clock::now();

double run_ms() {
   const auto now = chrono::high_resolution_clock::now();
   const auto rel_now = chrono::duration_cast<
         chrono::duration<double>>(now - TIME_START)
         .count();
   return rel_now * 1000;
}

// -

struct DebugStream {
   std::stringstream stream;

   DebugStream(const DebugStream&) = delete;
   DebugStream& operator=(const DebugStream&) = delete;

   DebugStream() {
      stream << "[+";
      stream << std::fixed << std::setprecision(2) << run_ms();
      stream << "ms] ";

      // The biggest eyeroll to whoever decided that e.g. 10000000.1
      // should truncate by default.
      stream << std::showbase;
      stream << std::defaultfloat;
      stream << std::setprecision(std::numeric_limits<long double>::digits10 + 1);
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


//constexpr uint32_t BLUE = 0xfacf5b; // bbggrr
//constexpr uint32_t PINK = 0xb9abf5; // bbggrr
constexpr uint32_t BLUE = 0xff5bcffa; // aarrggbb
constexpr uint32_t PINK = 0xfff5abb9; // aarrggbb
constexpr uint32_t WHITE = 0xffffffff;
constexpr std::array STRIPES_aarrggbb{
   BLUE, PINK, WHITE, PINK, BLUE,
};

struct PixelData final {
   vec3 size = {0,1,1};
   std::vector<u32> pixels;
};

PixelData make_flag() {
   // 5x3 aspect ratio.
   const auto W = STRIPES_aarrggbb.size() * 5;
   const auto H = STRIPES_aarrggbb.size() * 3;

   auto ret = PixelData{};
   ret.size.x = W;
   ret.size.y = H;
   ret.pixels.reserve(W*H);
   const auto elems_per_stripe = W*H / STRIPES_aarrggbb.size();
   for (const auto& color : STRIPES_aarrggbb) {
      dout() << std::hex << color;
      ret.pixels.resize(ret.pixels.size() + elems_per_stripe, color);
   }
   return ret;
}

hao<HBITMAP> make_flag_bitmap() {
   const auto data = make_flag();
   auto bitmap = make_hao(CreateBitmap(data.size.x, data.size.y, 1, 32, data.pixels.data()));
   return bitmap;
}

void draw_flag(const HDC dc, const RECT& rect) {
   const auto height = rect.bottom - rect.top;
   dout() << "draw_flag height: " << height;
   constexpr auto count = STRIPES_aarrggbb.size();
   for (size_t i = 0; i < count; i++) {
      const auto& aarrggbb = STRIPES_aarrggbb[i];
      const auto _00bbggrr = ((aarrggbb & 0x0000ff) << 16 |
                              (aarrggbb & 0x00ff00) <<  0 |
                              (aarrggbb & 0xff0000) >> 16);
      const auto brush = make_hao(CreateSolidBrush(_00bbggrr));
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
      vec3 flag_size = {};
   };
   const Data m;

   // -

   static std::unique_ptr<Renderer> Create(const HWND window) {
      auto m = Data{};
      m.window = window;

      u32 flags = D3D11_CREATE_DEVICE_BGRA_SUPPORT;
      flags |= D3D11_CREATE_DEVICE_DEBUG;

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

      auto hr = m.dc_target->SetRoot(m.flag_vis);
      REL_ASSERT(SUCCEEDED(hr), "SetRoot");

      // -
      // Create a visual from a bitmap.

      {
         const auto flag = make_flag();
         m.flag_size = flag.size;
         m.dcomp->CreateSurface(flag.size.x, flag.size.y,
            DXGI_FORMAT_B8G8R8A8_UNORM, DXGI_ALPHA_MODE_IGNORE,
            &m.flag_surf.setter_addrefs());
         REL_ASSERT(m.flag_surf, "dcomp->CreateSurface");

         if (1) {
            sp<ID3D11Texture2D> flag_surf_tex;
            auto draw_offset = POINT{};
            m.flag_surf->BeginDraw(nullptr, flag_surf_tex.UUID(),
                  &flag_surf_tex.setter_addrefs<void>(), &draw_offset);
            dout() << "BeginDraw()";
            REL_ASSERT(flag_surf_tex, "flag_surf_tex");
            const auto end_draw = scope_exit([&]() {
               dout() << "EndDraw()";
               m.flag_surf->EndDraw();
            });

            sp<ID3D11DeviceContext> context;
            m.d3d->GetImmediateContext(&context.setter_addrefs());
            REL_ASSERT(context, "context");

            context->UpdateSubresource(flag_surf_tex, 0, nullptr,
                                       flag.pixels.data(),
                                       flag.size.x * 4,
                                       flag.size.y * flag.size.x * 4);
            context->Flush();
         } else {
            const auto flag_bitmap = make_flag_bitmap();
            auto bm_info = BITMAP{};
            {
               auto ok = !!GetObject(*flag_bitmap, sizeof(bm_info), &bm_info);
               REL_ASSERT(ok, "GetObject");
            }
            dout() << "bm_info " << bm_info.bmWidth << "," << bm_info.bmHeight;

            sp<IDXGISurface1> flag_surf_dxgi;
            auto draw_offset = POINT{};
            m.flag_surf->BeginDraw(nullptr, flag_surf_dxgi.UUID(),
                  &flag_surf_dxgi.setter_addrefs<void>(), &draw_offset);
            REL_ASSERT(flag_surf_dxgi, "flag_surf->BeginDraw");
            const auto end_draw = scope_exit([&]() {
               dout() << "EndDraw()";
               m.flag_surf->EndDraw();
            });

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
      }

      hr = m.flag_vis->SetContent(m.flag_surf);
      REL_ASSERT(SUCCEEDED(hr), "SetContent");

      const auto debug = m.d3d.qi<ID3D11Debug>();
      REL_ASSERT(debug, "debug");
      debug->ReportLiveDeviceObjects( D3D11_RLDO_SUMMARY | D3D11_RLDO_DETAIL );

      return std::unique_ptr<Renderer>(new Renderer(std::move(m)));
   }

private:
   Renderer(Data&& m) : m(move(m)) {}

public:
   ~Renderer() = default;

   void Draw(const vec3& from, const vec3& to) const {
      dout() << "from " << from.x << "," << from.y;
      dout() << "to " << to.x << "," << to.y;
      auto mat = D2D_MATRIX_3X2_F{};
      mat.m11 = (to.x - from.x) / m.flag_size.x;
      mat.m22 = (to.y - from.y) / m.flag_size.y;
      mat.dx = from.x;
      mat.dy = from.y;

      auto hr = m.flag_vis->SetTransform(mat);
      REL_ASSERT(SUCCEEDED(hr), "SetTransform");

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

class Slideshow {
public:
   struct Data final {
      HWND window;
      std::unique_ptr<Renderer> renderer;
   };
   const Data m;

   static std::unique_ptr<Slideshow> Create(const HWND window) {
      auto m = Data{};
      m.window = window;

      m.renderer = Renderer::Create(window);
      if (!m.renderer) return nullptr;

      return std::unique_ptr<Slideshow>(new Slideshow(std::move(m)));
   }

   Slideshow(Data&& data) : m(std::move(data)) {}

   ~Slideshow() = default;

   // -

   static constexpr uintptr_t TIMER_ID = 244860726; // Math.random()

   void Paint() {
      dout() << "Paint";

      const auto now = run_ms() / 1000;

      constexpr float radius = 0.1;
      auto center = vec3{};
      center.x = 0.5 + std::sin(now) * radius;
      center.y = 0.5 + -std::cos(now) * radius;

      const float size = 0.7;
      auto from = center;
      from.x -= size/2;
      from.y -= size/2;

      auto to = center;
      to.x += size/2;
      to.y += size/2;

      auto win_rect = RECT{};
      GetWindowRect(m.window, &win_rect);
      from.x *= win_rect.right - win_rect.left;
      from.y *= win_rect.bottom - win_rect.top;
      to.x *= win_rect.right - win_rect.left;
      to.y *= win_rect.bottom - win_rect.top;

      if (1) {
         m.renderer->Draw(from, to);
         ValidateRect(m.window, nullptr);

         //InvalidateRect(m.window, nullptr, false);
      } else {
         PAINTSTRUCT ps;
         const auto dc = BeginPaint(m.window, &ps);
         const auto& win_rect = ps.rcPaint;

         draw_flag(dc, win_rect);

         EndPaint(m.window, &ps);
      }

      constexpr int ms = 10;
      SetTimer(m.window, TIMER_ID, ms, nullptr);
   }

   // -

   void OnPaint() {
      Paint();
   }

   void OnTimer(const uintptr_t timer_id) {
      if (timer_id == TIMER_ID) {
         Paint();
      }
   }
};

//-

LRESULT WINAPI ScreenSaverProcW(HWND window, UINT msg, WPARAM wp, LPARAM lp) {

   static std::unique_ptr<Slideshow> sSlideshow;

   switch (msg) {
   case WM_CREATE:
      OutputDebugStringA(" ");
      OutputDebugStringA("klc-slideshow WM_CREATE");
      sSlideshow = Slideshow::Create(window);
      REL_ASSERT(sSlideshow, "Slideshow::Create");
      return 0;

   case WM_DESTROY:
      sSlideshow = nullptr;
      return 0;

   // WM_DISPLAYCHANGE when resolution changes

   case WM_TIMER:
      dout() << "WM_TIMER";
      if (sSlideshow) {
         sSlideshow->OnTimer(wp);
      }
      return 0;

   case WM_PAINT:
      OutputDebugStringA("WM_PAINT");
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
