#pragma once

#define MutDerived() (*reinterpret_cast<DerivedT*>(this))
#define Derived() (*reinterpret_cast<const DerivedT*>(this))

template<typename DerivedT>
class DeriveOpDerefArrow {
public:
   auto operator->() const {
      return &*Derived();
   }
};

// -

template<typename DerivedT>
class DeriveOpNe {
public:
   bool operator!=(const DerivedT& rhs) const {
      return !(Derived() == rhs);
   }
};

// -

template<typename DerivedT>
class DeriveOpIncDecPostfix { // 🙄
public:
   auto& operator++(int) {
      auto ret = MutDerived();
      ++MutDerived();
      return ret;
   }
   auto& operator--(int) {
      auto ret = MutDerived();
      --MutDerived();
      return ret;
   }
};

// -

template<typename DerivedT>
class DeriveOpsFromDerefPlus
   : public DeriveOpDerefArrow<DeriveOpsFromDerefPlus>
{
public:
   auto& operator[](size_t n) const {
      return *(Derived() + n);
   }
};

// -

template<typename DerivedT>
class DeriveOpsFromLtEq
   : public DeriveOpNe<DeriveOpsFromLtEq>
{
public:
   bool operator>(const DerivedT& rhs) const {
      return rhs < Derived();
   }
   bool operator<=(const DerivedT& rhs) const {
      return !(Derived() > rhs);
   }
   bool operator>=(const DerivedT& rhs) const {
      return !(Derived() < rhs);
   }
};

// -

template<typename DerivedT, typename ValT>
class DeriveOpsFromPlusMinusAssign
   : public DeriveOpIncDecPostfix<DeriveOpsFromPlusMinusAssign>
{
public:
   auto operator+(const ValT& n) const {
      auto ret = MutDerived();
      ret += n;
      return ret;
   }
   auto operator-(const ValT& n) const {
      auto ret = MutDerived();
      ret -= n;
      return ret;
   }
   auto& operator++() {
      MutDerived() += 1;
      return MutDerived();
   }
   auto& operator--() {
      MutDerived() -= 1;
      return MutDerived();
   }
};
// Commutative:
template<typename DerivedT, typename ValT,
         typename Rhs =
            DeriveOpsFromPlusMinusAssign<DerivedT>>
auto operator+(const ValT& n, const Rhs& rhs) {
   return rhs + n;
}

#undef MutDerived
#undef Derived
