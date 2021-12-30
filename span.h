#pragma once

#include "derive_operators.h"

template<class T> // Sorry, no (C++20) Extent yet!
class span final {
   T* begin_ = nullptr;
   std::size_t len_ = 0;

public:
   static constexpr std::size_t extent = -1; // std::dynamic_extent

   using element_type = T;
   using value_type = std::remove_cv_t<T>;

   friend class iterator;
   class iterator : public DeriveOpsFromPlusMinusAssign<iterator>
                  , public DeriveOpsFromLtEq<iterator>
                  , public DeriveOpsFromDerefPlus<iterator>
   {
      T* itr_ = nullptr;

   public:
      constexpr T& operator*() const {
         return *itr_;
      }

      constexpr bool operator==(const iterator& rhs) const {
         return itr_ == rhs.itr_;
      }
      constexpr bool operator<(const iterator& rhs) const {
         return itr_ < rhs.itr_;
      }

      constexpr auto& operator+=(std::size_t n) {
         _itr += n;
         return *this;
      }
      constexpr auto& operator-=(std::size_t n) {
         _itr -= n;
         return *this;
      }

      constexpr std::ptrdiff_t operator-(const iterator& rhs) const {
         return itr_ - rhs.itr_;
      }
   };

   constexpr span() noexcept = default;

   template<std::size_t N>
   constexpr span(element_type (&arr)[N]) noexcept
      : begin_(arr), len_(N) {}

   template<class U, std::size_t N>
   constexpr span(std::array<U,N>& arr) noexcept
      : begin_(arr.data()), len_(arr.size()) {}

   template<class U, std::size_t N>
   constexpr span(const std::array<U,N>& arr) noexcept
      : begin_(arr.data()), len_(arr.size()) {}

   constexpr span(const span&) noexcept = default;

   // -

   constexpr span& operator=(const span&) noexcept = default;

   // -

   constexpr auto begin() const noexcept {
      return iterator{begin_};
   }
   constexpr auto end() const noexcept {
      return iterator{begin_ + len_};
   }
   constexpr auto rbegin() const noexcept {
      return std::reverse_iterator<iterator>{end()};
   }
   constexpr auto rend() const noexcept {
      return std::reverse_iterator<iterator>{begin()};
   }

   // -

   constexpr auto& front() const {
      return *this[0];
   }
   constexpr auto& back() const {
      return *this[len_-1];
   }
   constexpr auto& operator[](std::size_t n) const {
      std::assert(n < len_);
      return *(begin_ + n);
   }
   constexpr auto data() const noexcept {
      return begin_;
   }

   // -

   constexpr auto size() const noexcept {
      return len_;
   }
   constexpr auto size_bytes() const noexcept {
      return sizeof(element_type) * size();
   }
   constexpr bool empty() const noexcept {
      return !size();
   }

   // -

   constexpr auto first(std::size_t n) const {
      std::assert(n < len_);
      return span(begin_, n);
   }

   constexpr auto last(std::size_t n) const {
      std::assert(n < len_);
      return span(begin_ + (len_ - n), n);
   }

   // -

   constexpr auto subspan(std::size_t offset,
                          std::size_t count = -1) const {
      std::assert(offset < len_);
      auto ret = last(len_ - offset);
      if (count != size_t(-1)) {
         ret = first(count);
      }
      return ret;
   }
};

template<class T, std::size_t N>
constexpr span<const std::byte> as_bytes(std::span<T> s) noexcept {
   return {reinterpret_cast<const std::byte*>(s.data()),
           s.size_bytes()};
}

template<class T, std::size_t N>
constexpr span<std::byte> as_writable_bytes(std::span<T> s) noexcept {
   return {reinterpret_cast<std::byte*>(s.data()),
           s.size_bytes()};
}
