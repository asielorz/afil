#pragma once

// C++20 safe integer comparisons as defined by https://en.cppreference.com/w/cpp/utility/intcmp

#include <limits>
#include <type_traits>

namespace intcmp
{

    template< class T, class U >
    constexpr bool cmp_equal(T t, U u) noexcept
    {
        using UT = std::make_unsigned_t<T>;
        using UU = std::make_unsigned_t<U>;
        if constexpr (std::is_signed_v<T> == std::is_signed_v<U>)
            return t == u;
        else if constexpr (std::is_signed_v<T>)
            return t < 0 ? false : UT(t) == u;
        else
            return u < 0 ? false : t == UU(u);
    }

    template< class T, class U >
    constexpr bool cmp_not_equal(T t, U u) noexcept
    {
        return !cmp_equal(t, u);
    }

    template< class T, class U >
    constexpr bool cmp_less(T t, U u) noexcept
    {
        using UT = std::make_unsigned_t<T>;
        using UU = std::make_unsigned_t<U>;
        if constexpr (std::is_signed_v<T> == std::is_signed_v<U>)
            return t < u;
        else if constexpr (std::is_signed_v<T>)
            return t < 0 ? true : UT(t) < u;
        else
            return u < 0 ? false : t < UU(u);
    }

    template< class T, class U >
    constexpr bool cmp_greater(T t, U u) noexcept
    {
        return cmp_less(u, t);
    }

    template< class T, class U >
    constexpr bool cmp_less_equal(T t, U u) noexcept
    {
        return !cmp_greater(t, u);
    }

    template< class T, class U >
    constexpr bool cmp_greater_equal(T t, U u) noexcept
    {
        return !cmp_less(t, u);
    }

    template< class R, class T >
    constexpr bool in_range(T t) noexcept
    {
        return cmp_greater_equal(t, std::numeric_limits<R>::min()) &&
            cmp_less_equal(t, std::numeric_limits<R>::max());
    }

} // namespace intcmp
