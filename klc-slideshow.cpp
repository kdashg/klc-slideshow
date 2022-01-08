// Thank you to Rachel Grey:
// http://www.cityintherain.com/howtoscr.html

#pragma comment(lib, "comctl32.lib")
#pragma comment(lib, "user32.lib")
#pragma comment(lib, "advapi32.lib")
#pragma comment(lib, "gdi32.lib")
#pragma comment(lib, "Ole32.lib")
#pragma comment(lib, "Shell32.lib")
#pragma comment(lib, "D3D11.lib")
#pragma comment(lib, "Dcomp.lib")
#pragma comment(lib, "Gdiplus.lib")

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
#include <Gdiplus.h>
#include <KnownFolders.h>
#include <Shlobj.h>

#include <array>
#include <cassert>
#include <chrono>
#include <cstdlib>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <iomanip>      // std::setprecision
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <sstream>
#include <unordered_map>
#include <variant>
#include <vector>

using std::move;
using std::vector;
using std::string;
using std::optional;
using std::unique_ptr;
using std::shared_ptr;
namespace chrono = std::chrono;
namespace fs = std::filesystem;

// Rust has *such* better names.
using f32 = float;
using i32 = int32_t;
using u32 = uint32_t;
using f64 = double;
using i64 = int64_t;
using u64 = uint64_t;
using usize = size_t;

constexpr bool INTENSIFY = false;

// -

#define REL_ASSERT(X,M) \
   do { \
      if (X) {} else { \
         dout() << __FILE__ << ":" << __LINE__ << ": ASSERT (" << #X << ") FAILED: " M; \
         std::abort(); \
      } \
   } while (false);

// -

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

struct vec2 final {
   float x = 0;
   float y = 0;
};

struct ivec2 final {
   i32 x = 0;
   i32 y = 0;
};

struct vec3 final {
   float x = 0;
   float y = 0;
   float z = 0;

   constexpr explicit operator vec2() {
      return {x, y};
   }
};

// -

struct ImplNoCopy {
   ImplNoCopy() = default;
   ImplNoCopy(const ImplNoCopy&) = delete;
   ImplNoCopy& operator=(const ImplNoCopy&) = delete;
   ImplNoCopy(ImplNoCopy&&) = default;
   ImplNoCopy& operator=(ImplNoCopy&&) = default;
};

// -

struct PixelData : public ImplNoCopy {
   ivec2 size = {};
   std::vector<u32> pixels;
};

// -

optional<fs::path> get_path_by_known_folder_id(const KNOWNFOLDERID& rfid) {
   //  "C:\Users" is returned rather than "C:\Users\"

   wchar_t* path_buffer = nullptr;
   const auto release = scope_exit([&]() {
      CoTaskMemFree(path_buffer); // null ok too!
   });
   auto hr = SHGetKnownFolderPath(rfid, 0, nullptr, &path_buffer);
   if (!SUCCEEDED(hr)) return {};

   auto path_str = std::wstring(path_buffer);
   return fs::path(path_str);
}

optional<fs::path> get_pictures_path() {
   return get_path_by_known_folder_id(FOLDERID_Pictures);
}
optional<fs::path> get_home_path() {
   return get_path_by_known_folder_id(FOLDERID_Profile);
}

// -

std::string_view trim_front(std::string_view view) {
   while (view.size()) {
      const auto c = view.front();
      if (!isspace(c)) break;
      view = view.substr(1);
   }
   return view;
}

std::string_view trim_back(std::string_view view) {
   while (view.size()) {
      const auto c = view.back();
      if (!isspace(c)) break;
      view = view.substr(0, view.size() - 1);
   }
   return view;
}

std::string_view trim(std::string_view view) {
   view = trim_front(view);
   view = trim_back(view);
   return view;
}

// -


// -

// "foo\"bar"_
//           ^-end_of_string
size_t find_end_of_quote(const std::string_view& view, size_t pos) {
   const auto terminal = view.at(pos);
   pos += 1;
   while (pos != view.size()) {
      pos = view.find(terminal, pos);
      if (pos == string::npos) return string::npos;
      const auto prev_view = view.substr(0, pos);
      assert(prev_view.size());
      pos += 1;
      assert(pos <= view.size());
      if (prev_view.back() == '\\') {
         // Ugh.
         const auto pos_before_escapes = prev_view.find_last_not_of('\\');
         assert(pos_before_escapes != string::npos);
         // E.g. `"foo\"bar"` -> 1
         //   before-^  ^-pos
         const auto escape_count = pos - 2 - pos_before_escapes;
         const auto is_end_escaped = escape_count % 2 == 1;
         if (is_end_escaped) continue;
      }
      return pos;
   }
   return string::npos;
}

size_t find_first_of_not_in_quote(const std::string_view& view,
                                  const std::string_view& delim_list) {
   size_t pos = 0;

   static const auto BASIC_QUOTES = string{"\"'"};
   auto targets_and_quotes = BASIC_QUOTES;
   targets_and_quotes += delim_list;

   while (pos != view.size()) {
      pos = view.find_first_of(targets_and_quotes, pos);
      if (pos == string::npos) break;
      const auto c = view.at(pos);
      for (const auto& d : delim_list) {
         if (c == d) return pos;
      }

      // Must be a quote.
      pos = find_end_of_quote(view, pos); // Skip straight past it.
      continue;
   }
   return string::npos;
}

// -

struct IniData final {
   struct Section final {
      std::unordered_map<std::string, std::string> val_by_key;
      // Lines with no '=' are stored as if "line" = ""
   };
   std::unordered_map<std::string, Section> section_by_full_name;
   // E.g. if you declare [foo] and then [.bar], you need to access it
   // by "foo.bar".

   // Not actually fallible. :)
   static IniData parse(std::string_view view) {
      auto data = IniData{};

      size_t line_num = 0;
      auto cur_section_name = std::string{};
      auto cur_section = Section{};
      while (view.size()) {
         line_num += 1;
         (void)line_num; // We might not use this, but we want it.

         const auto end_line = view.find('\n');
         auto line = view.substr(0, end_line);
         view = view.substr(end_line);

         // -

         static const auto COMMENT_DELIMS = std::string(";#");
         const auto comment_pos = find_first_of_not_in_quote(line, COMMENT_DELIMS);
         if (comment_pos != string::npos) {
            line = view.substr(0, comment_pos);
         }
         line = trim(line);
         if (!line.size()) continue;

         // -

         if (line.size() && line.front() == '[' && line.back() == ']') {
            auto next_section_name = std::string{line.substr(1, line.size()-2)};
            if (next_section_name.size() && next_section_name.front() == '.') {
               next_section_name = cur_section_name + next_section_name;
            }
            data.section_by_full_name[move(cur_section_name)] = move(cur_section);

            // This'll work even if we're switching from a section to itself.
            cur_section_name = move(next_section_name);
            cur_section = move(data.section_by_full_name[cur_section_name]);
            continue;
         }

         // -

         const auto equal_pos = find_first_of_not_in_quote(line, string("="));
         auto key = string{trim_back(line.substr(0, equal_pos))};
         auto val = string{};
         if (equal_pos != string::npos) {
            val = trim_front(line.substr(equal_pos));
         }

         // Drop quotes if present.
         if (key.size() >= 2 && key.front() == '"' && key.back() == '"') {
            key = key.substr(1, key.size() - 2);
         }
         if (val.size() >= 2 && val.front() == '"' && val.back() == '"') {
            val = val.substr(1, val.size() - 2);
         }
         cur_section.val_by_key[move(key)] = move(val);
      }
      data.section_by_full_name[move(cur_section_name)] = move(cur_section);
      return data;
   }
};

/// E.g. result<Data,string>
template<class T, class E>
class result : protected std::variant<T, E> {
public:
   result(T&& val) : std::variant<T, E>{move(val)} {}
   result(E&& err) : std::variant<T, E>{move(err)} {}

   constexpr operator bool() noexcept {
      return bool(val());
   }
   constexpr T& operator*() const {
      return *val();
   }
   constexpr T* operator->() const {
      return val();
   }

   constexpr T* val() noexcept {
      return std::get_if<0>(this);
   }
   constexpr E* err() noexcept {
      return std::get_if<1>(this);
   }
};

optional<string> read_small_text_file(const fs::path& path) {
   // Method via "insane coder" from 2011:
   // https://insanecoding.blogspot.com/2011/11/how-to-read-in-file-in-c.html
   // > C++ streams have some very fast copying to another stream via
   // > operator<< on their internal buffers. Therefore, we can copy
   // > directly into a string stream, and then return the string that
   // > string stream uses.

   // Though we might as well create a filebuf directly.
   // Tragically, that means you miss out on an std::wifstream joke. :P
   auto in = std::basic_filebuf<typename fs::path::value_type>{};
   if (!in.open(path.c_str(), std::ios::in | std::ios::binary)) {
      return {};
   }
   std::ostringstream out;
   out << &in;
   in.close();
   return out.str(); // Does not look like there's a way around
                     // this copy. Ouch, oof.
}

enum class SceneLayout {
   Solo,
   DuoLeftMajor,
   DuoRightMajor,
   Trio,
};

static const string DEFAULT_CONFIG_INI = R"(

secs_per_image = 5

# 0: no pan, 1.0: starts just off screen, ends
pan_screen_ratio = 0.2

[scene weights]
# These are pooled and selected from, so they don't have to sum to
# anything particular.
solo = 50
duo_left_major = 11 ; Slightly sinister (also yes semicolons start comments too!
duo_right_major = 10
trio = 30

[folders]
~/Pictures

)";

// -

template<class T>
optional<T> parse_as(std::string_view) = delete;

template<>
optional<f32> parse_as<f32>(std::string_view view) {
   auto pos = view.data();
   const auto ret = strtof(pos, const_cast<char**>(&pos));
   // Yep, standard lib isn't const-correct here...

   if (pos == view.data()) {
      // Didn't move, must have failed.
      return {};
   }
   return ret;
}

template<>
optional<i32> parse_as<i32>(std::string_view view) {
   auto pos = view.data();
   const auto ret = strtol(pos, const_cast<char**>(&pos), 10);
   if (pos == view.data()) {
      // Didn't move, must have failed.
      return {};
   }
   return ret;
}

template<>
optional<u32> parse_as<u32>(std::string_view view) {
   auto pos = view.data();
   const auto ret = strtoul(pos, const_cast<char**>(&pos), 10);
   if (pos == view.data()) {
      // Didn't move, must have failed.
      return {};
   }
   return ret;
}

template<>
optional<f64> parse_as<f64>(std::string_view view) {
   auto pos = view.data();
   const auto ret = strtod(pos, const_cast<char**>(&pos));
   if (pos == view.data()) {
      // Didn't move, must have failed.
      return {};
   }
   return ret;
}

template<>
optional<i64> parse_as<i64>(std::string_view view) {
   auto pos = view.data();
   const auto ret = strtoll(pos, const_cast<char**>(&pos), 10);
   if (pos == view.data()) {
      // Didn't move, must have failed.
      return {};
   }
   return ret;
}

template<>
optional<u64> parse_as<u64>(std::string_view view) {
   auto pos = view.data();
   const auto ret = strtoull(pos, const_cast<char**>(&pos), 10);
   if (pos == view.data()) {
      // Didn't move, must have failed.
      return {};
   }
   return ret;
}

// -

template<class T>
bool parse_into(std::string_view view, T* const out) {
   const auto parsed = parse_as<T>(view);
   if (parsed) {
      *out = *parsed;
   }
   return !!parsed;
}

// -

struct Config final : public ImplNoCopy {
   // These are pooled and selected from, so they don't have to sum to
   // anything particular.
   struct SceneWeights {
      // Let's do a map, so there's a reasonable order, even if we're
      // choosing randomly.
      std::map<SceneLayout, double> weight_by_scene;

      SceneLayout choose(const double zero_to_one) const {
         if (!weight_by_scene.size()) return SceneLayout::Solo;
         double sum = 0;
         for (const auto& pair : weight_by_scene) {
            if (pair.second <= 0) continue;
            sum += pair.second;
         }
         auto selected = zero_to_one * sum;
         for (const auto& pair : weight_by_scene) {
            if (pair.second <= 0) continue;
            selected -= pair.second;
            if (selected <= 0) return pair.first;
         }
         // Float math is a trip, so let's play it safe:
         return weight_by_scene.begin()->first;
      }
   };

   // -

   // Deliberately make these different from the actual default config.
   double secs_per_image = -1;
   double pan_screen_ratio = -1; // 0: no pan, 1.0: starts just off screen, ends
   optional<usize> random_seed;

   vector<string> image_folders;
   SceneWeights scene_weights;

   vector<string> unrecognized_sections;
   vector<std::array<string,2>> unrecognized_keys;
   vector<std::array<string,3>> unrecognized_values;

   // -

   static result<Config,string> load() {
      const auto home_dir = get_home_path();
      if (!home_dir) return string{"home_path() failed"};
      auto config_dir = *home_dir / ".config";

      const auto xdg_config_dir = std::getenv("XDG_CONFIG_HOME");
      if (xdg_config_dir) {
         config_dir = xdg_config_dir;
      }

      auto default_ini = IniData::parse(DEFAULT_CONFIG_INI);
      auto defaults = from_ini_data(move(default_ini));
      REL_ASSERT(defaults.unrecognized_sections.empty(), "defaults.unrecognized_sections");
      REL_ASSERT(defaults.unrecognized_keys.empty(), "defaults.unrecognized_keys");
      REL_ASSERT(defaults.unrecognized_values.empty(), "defaults.unrecognized_values");

      const auto config_file = config_dir / "klc-slideshow.ini";
      if (!fs::exists(config_file)) {
         dout() << string{"config_file does not exist: "} + config_file.string();
         return defaults;
      }
      const auto config_text = read_small_text_file(config_file);
      REL_ASSERT(config_text, "read_small_text_file failed on extant file?");

      auto ini_data = IniData::parse(*config_text);
      // I love that parsing ini cannot fail.
      // We always just get...something.

      auto ret = from_ini_data(move(ini_data));

      if (ret.secs_per_image < 0) {
         ret.secs_per_image = defaults.secs_per_image;
         dout() << "[secs_per_image] defaulted.";
      }
      if (ret.pan_screen_ratio < 0) {
         ret.pan_screen_ratio = defaults.pan_screen_ratio;
         dout() << "[pan_ratio] defaulted.";
      }
      if (ret.image_folders.empty()) {
         ret.image_folders = defaults.image_folders;
         dout() << "[image_folders] defaulted.";
      }
      if (ret.scene_weights.weight_by_scene.empty()) {
         ret.scene_weights.weight_by_scene = defaults.scene_weights.weight_by_scene;
         dout() << "[scene_weights] defaulted.";
      }

      for (const auto& name : ret.unrecognized_sections) {
         dout() << "Unrecognized section [" << name << "] in " << config_file;
      }
      for (const auto& arr : ret.unrecognized_keys) {
         dout() << "Unrecognized key [" << arr[0] << "] \""
                << arr[1] << "\" in " << config_file;
      }
      for (const auto& arr : ret.unrecognized_values) {
         dout() << "Unrecognized value [" << arr[0] << "] \""
                << arr[1] << "\" = \"" << arr[2] << "\" in " << config_file;
      }
      return ret;
   }

   // So here's my take:
   // Literally extract (and remove from maps) the sections and keys as
   // you read them. If there are any leftover afterwards, you can let
   // someone know, because it's probably a mispelled/unrecognized name.

   static Config from_ini_data(IniData&& data) {
      auto config = Config{};

      // Let's do the root "[]" section keys
      auto section = data.section_by_full_name.extract("");
      if (section) {
         auto& val_by_key = section.mapped().val_by_key;

         auto cur = val_by_key.extract("secs_per_image");
         if (cur) {
            (void)parse_into(cur.mapped(), &config.secs_per_image);
         }

         cur = val_by_key.extract("pan_screen_ratio");
         if (cur) {
            (void)parse_into(cur.mapped(), &config.pan_screen_ratio);
         }

         cur = val_by_key.extract("random_seed");
         if (cur) {
            const auto& val = cur.mapped();
            config.random_seed = std::hash<string>{}(val);
         }

         for (const auto& key__val : val_by_key) {
            config.unrecognized_keys.emplace_back(std::array{
               section.key(), key__val.first
            });
         }
      }

      section = data.section_by_full_name.extract("image_folders");
      if (section) {
         auto& val_by_key = section.mapped().val_by_key;

         for (const auto& key__val : val_by_key) {
            const auto& key = key__val.first;
            if (key.size()) {
               config.image_folders.push_back(key);
            }
         }
      }

      section = data.section_by_full_name.extract("scene_weights");
      if (section) {
         auto& val_by_key = section.mapped().val_by_key;

         auto& weight_by_scene = config.scene_weights.weight_by_scene;
         for (const auto& key__val : val_by_key) {
            const auto& key = key__val.first;
            SceneLayout scene;
            if (key == "solo") {
               scene = SceneLayout::Solo;
            } else if (key == "duo_left_major") {
               scene = SceneLayout::DuoLeftMajor;
            } else if (key == "duo_right_major") {
               scene = SceneLayout::DuoRightMajor;
            } else if (key == "trio") {
               scene = SceneLayout::Trio;
            } else {
               config.unrecognized_values.emplace_back(std::array{
                  section.key(), key__val.first, key__val.second
               });
               continue;
            }
            const auto weight = parse_as<double>(key__val.second);
            if (!weight) continue;
            weight_by_scene[scene] = *weight;
         }
      }

      for (const auto& name__section : data.section_by_full_name) {
         config.unrecognized_sections.push_back(name__section.first);
      }

      // -

      return config;
   }
};

// -

template<class T>
unique_ptr<T> as_unique_ptr(T* ptr) {
   return unique_ptr<T>(ptr);
}

// -

inline bool starts_with(const string& str, const string& prefix) {
   return str.find(prefix) == 0;
}

template<class T>
inline usize std_hash(const T& x) {
   return std::hash<T>{}(x);
}

class ImageLoader : public ImplNoCopy {
   struct Entry final {
      usize primary_order_key = 0;
      fs::path path = {};

      auto tie() const {
         return std::tie(primary_order_key, path);
      }

      bool operator<(const Entry& rhs) const {
         return tie() < rhs.tie();
      }
   };
   const optional<usize> _random_seed;
   std::set<Entry> _path_entries;
   std::set<Entry>::iterator _prev_loaded = {}; // Never invalidated.

public:
   explicit ImageLoader(const Config& config) : _random_seed(config.random_seed) {
      _prev_loaded = _path_entries.begin();

      for (const auto& folder_str : config.image_folders) {
         fs::path path;
         if (starts_with(folder_str, "~/")) {
            const auto rel_path = folder_str.substr(2);
            path = *get_home_path() / rel_path;
         } else {
            path = fs::u8path(folder_str);
         }
         if (!fs::exists(path)) {
            dout() << "Path does not exist: " << path.string()
                   << " (" << folder_str << ")";
            continue;
         }

         const auto options = fs::directory_options::follow_directory_symlink
            | fs::directory_options::skip_permission_denied;
         auto ec = std::error_code{};
         for (const auto& cur_path :
               fs::recursive_directory_iterator(path, options, ec)) {
            add(cur_path);
         }
      }
   }

   auto primary_order_key(const fs::path& path) const {
      usize ret = 0;
      if (_random_seed) {
         ret = std_hash(path.string());
         ret ^= *_random_seed;
         ret = std_hash(ret);
      }
      return ret;
   }

   std::set<Entry>::iterator add(const fs::path& path) {
      const auto order = primary_order_key(path);
      return _path_entries.insert({order, path}).first;
   }

   static inline optional<PixelData> load(const fs::path& path) {
      // "You can create Image objects based on files of a variety
      //  of types including BMP, GIF, JPEG, PNG, TIFF, and EMF."
      constexpr bool USE_EMBEDDED_COLOR_MANAGEMENT = true;
      auto bm = as_unique_ptr(Gdiplus::Bitmap::FromFile(path.c_str(),
            USE_EMBEDDED_COLOR_MANAGEMENT));
      if (!bm) return {}; // Yeah, so maybe that was a folder...

      const auto rect = Gdiplus::Rect{{}, {
         static_cast<i32>(bm->GetWidth()),
         static_cast<i32>(bm->GetHeight())}};
      auto mapping = Gdiplus::BitmapData{};
      bm->LockBits(&rect, Gdiplus::ImageLockModeRead,
         PixelFormat32bppPARGB, // premult-argb
         &mapping);
      const auto unmap = scope_exit([&]() {
         bm->UnlockBits(&mapping);
      });

      auto data = PixelData{};
      data.size = {static_cast<i32>(bm->GetWidth()),
                   static_cast<i32>(bm->GetHeight())};
      data.pixels.resize(data.size.x * data.size.y);

      auto src = static_cast<const std::byte*>(mapping.Scan0);
      auto dst = reinterpret_cast<std::byte*>(data.pixels.data());
      const auto srcStride = mapping.Stride;
      const auto bytesPerRow = data.size.x * 4;
      const auto dstStride = bytesPerRow;
      for (int y = 0; y < data.size.y; y++) {
         memcpy(dst, src, bytesPerRow);
         src += srcStride;
         dst += dstStride;
      }

      return data;
   }

   auto size() const {
      return _path_entries.size();
   }

   optional<PixelData> load_next() {
      while (_path_entries.size()) {
         auto cur_itr = _prev_loaded;
         ++cur_itr;
         if (cur_itr == _path_entries.end()) {
            cur_itr = _path_entries.begin();
         }
         auto ret = load(cur_itr->path);
         if (ret) {
            _prev_loaded = cur_itr;
            return ret;
         }
         _path_entries.erase(cur_itr);
      }
      return {};
   }
};

class Scene final {
public:
   static unique_ptr<Scene> Build(const Config& config,
                                  ImageLoader& loader) {
      const f64 rand_0_1incl = f64(rand()) / RAND_MAX;
      auto layout = config.scene_weights.choose(rand_0_1incl);
      layout = SceneLayout::Solo; // Oops all solo.
      switch (layout) {
      case SceneLayout::Solo:
         {

         }
      case SceneLayout::DuoLeftMajor:
      case SceneLayout::DuoRightMajor:
      case SceneLayout::Trio:
         REL_ASSERT(false, "todo");
      }
   }
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

//constexpr uint32_t BLUE = 0xfacf5b; // bbggrr
//constexpr uint32_t PINK = 0xb9abf5; // bbggrr
constexpr uint32_t BLUE = 0xff5bcffa; // aarrggbb
constexpr uint32_t PINK = 0xfff5abb9; // aarrggbb
constexpr uint32_t WHITE = 0xffffffff;
constexpr std::array STRIPES_aarrggbb{
   BLUE, PINK, WHITE, PINK, BLUE,
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

// We're using DComp parlance
class Visual {
public:
   struct Data {
      sp<IDCompositionVisual> vis;
      ivec2 size = {};
   };
   const Data m;

   static shared_ptr<Visual> create(IDCompositionDevice& dcomp,
                                    ID3D11Device& d3d,
                                    const PixelData& src) {
      sp<IDCompositionSurface> surf;
      dcomp.CreateSurface(src.size.x, src.size.y,
         DXGI_FORMAT_B8G8R8A8_UNORM, DXGI_ALPHA_MODE_IGNORE,
         &surf.setter_addrefs());
      REL_ASSERT(surf, "dcomp->CreateSurface");

      {
         sp<ID3D11Texture2D> dst_tex;
         auto draw_offset = POINT{};
         surf->BeginDraw(nullptr, dst_tex.UUID(),
               &dst_tex.setter_addrefs<void>(), &draw_offset);
         REL_ASSERT(dst_tex, "surf->BeginDraw");

         const auto end_draw = scope_exit([&]() {
            surf->EndDraw();
         });

         sp<ID3D11DeviceContext> context;
         d3d.GetImmediateContext(&context.setter_addrefs());
         REL_ASSERT(context, "context");

         auto dst_box = D3D11_BOX{};
         dst_box.left = draw_offset.x;
         dst_box.top = draw_offset.y;
         dst_box.right = dst_box.left + src.size.x;
         dst_box.bottom = dst_box.top + src.size.y;
         dst_box.front = 0;
         dst_box.back = 1;
         context->UpdateSubresource(dst_tex, 0, &dst_box,
                                    src.pixels.data(),
                                    src.size.x * 4,
                                    src.size.y * src.size.x * 4);
         //context->Flush();
      }

      auto m = Data{};
      m.size = src.size;
      dcomp.CreateVisual(&m.vis.setter_addrefs());
      REL_ASSERT(m.vis, "CreateVisual");
      auto hr = m.vis->SetContent(surf);
      REL_ASSERT(SUCCEEDED(hr), "SetContent");

      return std::make_shared<Visual>(std::move(m));
   }

   Visual(Data&& data) : m(std::move(data)) {}

   // No-op would be from:0,0 to:1,1
   void set_rect(const vec3& from, const vec3& to) const {
      //dout() << "from " << from.x << "," << from.y;
      //dout() << "to " << to.x << "," << to.y;
      auto mat = D2D_MATRIX_3X2_F{};
      mat.m11 = (to.x - from.x);
      mat.m22 = (to.y - from.y);
      mat.dx = from.x * m.size.x;
      mat.dy = from.y * m.size.y;

      auto hr = m.vis->SetTransform(mat);
      REL_ASSERT(SUCCEEDED(hr), "SetTransform");
   }
};

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
      ivec2 flag_size = {};
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

      HRESULT hr;

      //auto hr = m.dc_target->SetRoot(m.flag_vis);
      //REL_ASSERT(SUCCEEDED(hr), "SetRoot");

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

            auto tex_desc = D3D11_TEXTURE2D_DESC{};
            flag_surf_tex->GetDesc(&tex_desc);
            dout() << "tex_desc: "
               << "Width " << tex_desc.Width << ", "
               << "Height " << tex_desc.Height;
            auto flag_box = D3D11_BOX{};
            flag_box.left = draw_offset.x;
            flag_box.top = draw_offset.y;
            flag_box.right = flag_box.left + m.flag_size.x;
            flag_box.bottom = flag_box.top + m.flag_size.y;
            flag_box.front = 0;
            flag_box.back = 1;

            const auto* pbox = &flag_box;
            if (INTENSIFY) {
               pbox = nullptr; // Can has little a corruption, as a treat.
            }
            context->UpdateSubresource(flag_surf_tex, 0, pbox,
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

      hr = m.dc_target->SetRoot(m.flag_vis);
      REL_ASSERT(SUCCEEDED(hr), "SetRoot");

      const auto debug = m.d3d.qi<ID3D11Debug>();
      REL_ASSERT(debug, "debug");
      debug->ReportLiveDeviceObjects( D3D11_RLDO_SUMMARY | D3D11_RLDO_DETAIL );

      return std::unique_ptr<Renderer>(new Renderer(std::move(m)));
   }

private:
   Renderer(Data&& m) : m(move(m)) {}

public:
   ~Renderer() = default;

   void SetFrontPos(const vec3& from, const vec3& to) const {

   }

   void Draw(const vec3& from, const vec3& to) const {
      //dout() << "from " << from.x << "," << from.y;
      //dout() << "to " << to.x << "," << to.y;
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
      if (0) {
         dout() << "timeFrequency: " << stats.timeFrequency.QuadPart;
         //dout() << "lastFrameTime: " << stats.lastFrameTime.QuadPart;
         const double per_sec = stats.timeFrequency.QuadPart;
         dout() << "lastFrameTime: " << stats.lastFrameTime.QuadPart / per_sec;
         dout() << "currentTime: " << stats.currentTime.QuadPart / per_sec;
         dout() << "nextEstimatedFrameTime: " << stats.nextEstimatedFrameTime.QuadPart / per_sec;
         const auto& rate = stats.currentCompositionRate;
         dout() << "currentCompositionRate: " << rate.Numerator << "/" << rate.Denominator;
      }
   }
};

// -

class Slideshow {
public:
   struct Data final {
      HWND window;
      std::unique_ptr<Renderer> renderer;
      Config config = {};
   };
   const Data m;
   shared_ptr<Visual> _front_vis;
   ImageLoader _image_loader;

   static std::unique_ptr<Slideshow> Create(const HWND window) {
      auto m = Data{};
      m.window = window;

      auto res = Config::load();
      if (!res) {
         dout() << "Config::load failed: " << *res.err();
      }
      REL_ASSERT(res, "Config::load");
      m.config = move(*res.val());

      m.renderer = Renderer::Create(window);
      if (!m.renderer) return nullptr;

      return std::unique_ptr<Slideshow>(new Slideshow(std::move(m)));
   }

   Slideshow(Data&& data) : m(std::move(data)), _image_loader(m.config) {
      const auto next = _image_loader.load_next();
      REL_ASSERT(next, "_image_loader.load_next");

      const auto vis = Visual::create(*m.renderer->m.dcomp,
            *m.renderer->m.d3d, *next);
      REL_ASSERT(vis, "Visual::create");
      _front_vis = vis;
   }

   ~Slideshow() = default;

   // -

   static constexpr uintptr_t TIMER_ID = 244860726; // Math.random()

   void Paint() {
      dout() << "Paint";

      auto now = run_ms() / 1000;
      if (INTENSIFY) {
         now *= 1000 * 1000;
      }

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

      if (1) {
         if (_front_vis) {
            auto front_from = from;
            front_from.x = front_from.y;
            auto front_to = to;
            front_to.x = front_to.y;
            _front_vis->set_rect(front_from, front_to);
         }

         auto win_rect = RECT{};
         GetWindowRect(m.window, &win_rect);
         auto win_from = from;
         auto win_to = to;
         win_from.x *= win_rect.right - win_rect.left;
         win_from.y *= win_rect.bottom - win_rect.top;
         win_to.x *= win_rect.right - win_rect.left;
         win_to.y *= win_rect.bottom - win_rect.top;

         m.renderer->Draw(win_from, win_to);
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
