/****************************************************************************

    MIT License

    Copyright (c) 2021 Aria Janke

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.

*****************************************************************************/

#pragma once

#include <stdint.h>
#include <array>
#include <tuple>

#include <algorithm>
#include <stdexcept>
#include <optional>

#include <cmath>
#include <cassert>

struct ColorStringHelpers;

class ColorString final {
public:
    template <typename ... Types>
    using Tuple            = std::tuple<Types...>;
    template <typename ArgType>
    using EnableForNumeric = std::enable_if_t<std::is_arithmetic_v<ArgType>, ColorString>;
    using ComponentString  = std::array<char, 3>;
    using FullString       = std::array<char, 10>;
    using Helpers          = ColorStringHelpers;

    static constexpr const uint8_t k_default_alpha = 0xFF;
    enum Component { k_red, k_green, k_blue, k_alpha };

    template <Component kt_component>
    class ComponentFunctions final {
    public:

        ///
        /// @warning This function maybe prone to overflow!
        template <typename T>
        constexpr EnableForNumeric<T> portion(T num, T denom = T(1)) const;

        constexpr ColorString replace_with(const char * part_string) const;

        constexpr ColorString replace_with(const ComponentString & cmpstr) const
            { return replace_with(cmpstr.data()); }

        constexpr ComponentString part() const;

        constexpr uint8_t u8() const;

        constexpr int length() const;

    private:
        friend class ColorString;
        constexpr ComponentFunctions(const ColorString * parent):
            m_str(parent) {}

        constexpr const char * begin() const;

        const ColorString * m_str;
    };

    constexpr ColorString() {}

    constexpr explicit ColorString(const char *);

    ColorString(const char * red_part , const char * green_part,
                const char * blue_part, const char * alpha_part = nullptr);

    constexpr ColorString & operator = (const char *);

    constexpr uint32_t to_rgba_u32() const;

    /// Creates a new ColorString with a different size.
    /// Acceptable sizes are: 4, 5, 7, 9
    ///
    /// @param alpha value to be used iff the alpha component was not already
    ///              present in the string (for instance size 4 to 5, however
    ///              size 5 to 7 or #RGBA to #RRGGBB will cause this argument
    ///              to be ignored)
    ColorString new_length(int, uint8_t alpha = 0xFF) const;

    constexpr int length() const;

    constexpr auto red  () const { return ComponentFunctions<k_red  >{this}; }
    constexpr auto green() const { return ComponentFunctions<k_green>{this}; }
    constexpr auto blue () const { return ComponentFunctions<k_blue >{this}; }
    constexpr auto alpha() const { return ComponentFunctions<k_alpha>{this}; }

    constexpr const char * c_str() const noexcept { return m_str.data(); }

    constexpr const char * begin() const noexcept { return m_str.data(); }

    constexpr const char * end() const noexcept { return begin() + length(); }

    constexpr bool operator <  (const ColorString & rhs) const { return compare(rhs) <  0; }
    constexpr bool operator >  (const ColorString & rhs) const { return compare(rhs) >  0; }
    constexpr bool operator <= (const ColorString & rhs) const { return compare(rhs) <= 0; }
    constexpr bool operator >= (const ColorString & rhs) const { return compare(rhs) >= 0; }
    constexpr bool operator == (const ColorString & rhs) const { return compare(rhs) == 0; }
    constexpr bool operator != (const ColorString & rhs) const { return compare(rhs) != 0; }

    constexpr bool operator <  (const char * rhs) const { return compare(rhs) <  0; }
    constexpr bool operator >  (const char * rhs) const { return compare(rhs) >  0; }
    constexpr bool operator <= (const char * rhs) const { return compare(rhs) <= 0; }
    constexpr bool operator >= (const char * rhs) const { return compare(rhs) >= 0; }
    constexpr bool operator == (const char * rhs) const { return compare(rhs) == 0; }
    constexpr bool operator != (const char * rhs) const { return compare(rhs) != 0; }

private:
    using SizeInt = int;

    static constexpr const char * verify_valid_string
        (const char * caller, const char * value);

    static constexpr const char * verify_valid_portion
        (const char * caller, const char * value);

    static constexpr bool is_valid_hex_char(char c);

    static constexpr uint8_t char_to_val(char c);

    static constexpr ColorString make_with_byte_components
        (const char * rpart, const char * gpart, const char * bpart,
         const char * apart) noexcept;

    static constexpr FullString init(const char * str);

    constexpr int compare(const ColorString &) const;

    constexpr int compare(const char *) const;

    ColorString & copy(const ColorString &);

    ColorString & copy(const char *);

    FullString m_str = FullString{};
    SizeInt m_len = 0;
};

// ---------------------------- ColorStringHelpers ----------------------------

struct ColorStringHelpers final {
    static constexpr const auto k_red   = ColorString::k_red  ;
    static constexpr const auto k_green = ColorString::k_green;
    static constexpr const auto k_blue  = ColorString::k_blue ;
    static constexpr const auto k_alpha = ColorString::k_alpha;

    using Component = ColorString::Component;
    using ComponentString = ColorString::ComponentString;
    using InvArg = std::invalid_argument;
    friend class ColorString;

    template <typename ... Types>
    using Tuple = std::tuple<Types...>;

    template <Component>
    friend class ColorString::ComponentFunctions;

    static InvArg make_bad_size(const char * caller) noexcept {
        return InvArg(std::string(caller) + ": size of color string must be 4, 5, 7, or 9");
    }

    static constexpr const char * end_of(const char * s)
        { return *s ? end_of(s + 1) : s; }

    template <Component kt_component>
    static constexpr int component_string_offset(int strlen) {
        [[maybe_unused]] constexpr const auto k_no_alpha_msg =
            "component_string_offset: cannot get offset for string size which "
            "does not have an alpha portion.";

        switch (strlen) {
        case 4:
            if constexpr (kt_component == k_alpha) { throw InvArg(k_no_alpha_msg); }
            [[fallthrough]];
        case 5: return 1 + kt_component;
        case 7:
            if constexpr (kt_component == k_alpha) { throw InvArg(k_no_alpha_msg); }
            [[fallthrough]];
        case 9: return 1 + kt_component*2;
        default: throw make_bad_size("component_string_offset");
        }
    }

    template <Component kt_component>
    static constexpr char funnel_small(const char * beg, const char * end) {
        const auto offset = component_string_offset<kt_component>(end - beg);
        switch (end - beg) {
        case 4: case 5: return beg[ offset ];
        case 7: case 9:
            return char(int(beg[ offset ]) + int(beg[ offset + 1 ]) / 2);
        default: throw make_bad_size("funnel_small");
        }
    }

    template <Component kt_component>
    static constexpr ComponentString funnel_large(const char * beg, const char * end) {
        const auto offset = component_string_offset<kt_component>(end - beg);
        switch (end - beg) {
        case 4: case 5: return { beg[offset    ], beg[offset    ], '\0' };
        case 7: case 9: return { beg[offset + 0], beg[offset + 1], '\0' };
        default: throw make_bad_size("funnel_large");
        }
    }

    template <Component kt_component>
    static constexpr auto make_small_funnel
        (const char * source_beg, int len,
         const char * part_beg, int part_len)
    {
        return [=] (const Component k_other_comp) {
            if (k_other_comp == kt_component) {
                // pull from part
                if (part_len == 1) return *part_beg;
                return char((int(part_beg[0]) + int(part_beg[1])) / 2);
            }
            switch (k_other_comp) {
            case k_red  : return funnel_small<k_red  >(source_beg, source_beg + len);
            case k_green: return funnel_small<k_green>(source_beg, source_beg + len);
            case k_blue : return funnel_small<k_blue >(source_beg, source_beg + len);
            case k_alpha: return funnel_small<k_alpha>(source_beg, source_beg + len);
            }
        };
    }

    template <Component kt_component>
    static constexpr auto make_large_funnel
        (const char * source_beg, int len,
         const char * part_beg, int part_len)
    {
        // there must be a way to reduce this, perhaps to a single template
        // function?
        return [=] (const Component k_other_comp) {
            if (k_other_comp == kt_component) {
                // pull from part
                if (part_len == 1)
                    return ComponentString{ part_beg[0], part_beg[0], '\0'};
                return ComponentString{ part_beg[0], part_beg[1], '\0'};
            }
            switch (k_other_comp) {
            case k_red  : return funnel_large<k_red  >(source_beg, source_beg + len);
            case k_green: return funnel_large<k_green>(source_beg, source_beg + len);
            case k_blue : return funnel_large<k_blue >(source_beg, source_beg + len);
            case k_alpha: return funnel_large<k_alpha>(source_beg, source_beg + len);
            }
        };
    }

    template <int idx_0, int idx_1>
    static constexpr auto make_to_u8() {
        const auto char_to_val = [](char c)
            { return std::get<uint8_t>(id_hex_char(c)); };
        return [char_to_val](const char * beg)
            { return (char_to_val(beg[idx_0]) << 4) | char_to_val(beg[idx_1]); };
    }

    template <int len>
    static constexpr auto make_to_part() {
        return [](const char * beg) {
            return std::array
                {len == 0 ? '\0' : beg[0], len == 1 ? '\0' : beg[1], '\0'};
        };
    }

    static constexpr Tuple<bool, uint8_t> id_hex_char(char c) {
        using std::make_tuple;
        if (c >= 'a' && c <= 'f') return make_tuple(true, c - 'a');
        if (c >= 'A' && c <= 'F') return make_tuple(true, c - 'A');
        if (c >= '0' && c <= '9') return make_tuple(true, c - '0');
        return make_tuple(false, '\0');
    }

    template <typename T, typename Func>
    static constexpr bool all_of(const T * beg, const T * end, Func && f) {
        if (beg == end) throw "";
        if (end - beg == 1) return f(*beg);
        return f(*beg) && all_of(beg + 1, end, std::move(f));
    }

    template <typename T, typename Func>
    static constexpr bool any_of(const T * beg, const T * end, Func && f) {
        if (beg == end) throw "";
        if (end - beg == 1) return f(*beg);
        return f(*beg) || all_of(beg + 1, end, std::move(f));
    }

    static constexpr int len_of(const char * s) { return end_of(s) - s; }

    static constexpr ComponentString u8_to_string(uint8_t u8) {
        auto to_a = [](int i) -> char {
            auto c = i > 9 ? ( (i - 10) + 'A' ) : (i + '0');
            if (!std::get<bool>(id_hex_char(c))) throw "";
            return c;
        };
        return { to_a(u8 / 0x10), to_a(u8 % 0x10), '\0' };
    }

    template <typename T>
    static constexpr uint8_t portion_val
        (const T num, const T denom, const uint8_t comp_val)
    {
        const T val = (num*comp_val) / denom;
        if constexpr (std::is_floating_point_v<T>) {
            return std::round(val);
        }
        return val;
    }
};

// --------------------- ColorString::ComponentFunctions ----------------------

template <ColorString::Component kt_component>
constexpr const char * ColorString::ComponentFunctions<kt_component>::begin() const {
    using H = Helpers;
    return    m_str->begin()
           + H::component_string_offset<kt_component>(m_str->length());
}

template <ColorString::Component kt_component>
template <typename T>
constexpr ColorString::EnableForNumeric<T>
    ColorString::ComponentFunctions<kt_component>::portion(T num, T denom) const
{
    const auto replace_with_u8 = [this](uint8_t val)
        { return replace_with(Helpers::u8_to_string(val)); };

    return replace_with_u8(Helpers::portion_val( num, denom, u8() ));
}

template <ColorString::Component kt_component>
constexpr ColorString ColorString::ComponentFunctions<kt_component>::replace_with
    (const char * part_string) const
{
    using H = Helpers;
    if (!*part_string) return ColorString{*m_str};

    const auto part_len = H::len_of(ColorString::verify_valid_portion(
        "replace_with", part_string));
    const auto parent_len = m_str->length();
    const bool has_alpha =
        kt_component == k_alpha || parent_len == 5 || parent_len == 9;

    if (part_len == 2 || parent_len > 5) {
        const auto large_funnel = Helpers::make_large_funnel
            <kt_component>(m_str->begin(), parent_len, part_string, part_len);
        return make_with_byte_components(
                        large_funnel(k_red  ).data(),
                        large_funnel(k_green).data(),
                        large_funnel(k_blue ).data(),
            has_alpha ? large_funnel(k_alpha).data() : nullptr
        );
    }

    const auto small_funnel = Helpers::make_small_funnel
        <kt_component>(m_str->begin(), parent_len, part_string, part_len);
    const std::array strarr = { '#',
                    small_funnel(k_red  ),
                    small_funnel(k_green),
                    small_funnel(k_blue ),
        has_alpha ? small_funnel(k_alpha) : '\0', '\0' };
    return ColorString{strarr.data()};
}
template <ColorString::Component kt_component>
constexpr ColorString::ComponentString
    ColorString::ComponentFunctions<kt_component>::part() const
{
    using H = Helpers;
    switch (length()) {
    case 0: return H::make_to_part<0>()(begin());
    case 1: return H::make_to_part<1>()(begin());
    case 2: return H::make_to_part<2>()(begin());
    default: throw "";
    }
}

template <ColorString::Component kt_component>
constexpr uint8_t ColorString::ComponentFunctions<kt_component>::u8() const {
    // MSVC is picky with constexpr switch statements
    const auto v = [this] () -> std::optional<uint8_t> {
        switch (length()) {
        // there must be a fallback for alpha
        case 0:
            if constexpr (kt_component == k_alpha)
                { return k_default_alpha; }
            return {};
        case 1: return Helpers::make_to_u8<0, 0>()(begin());
        case 2: return Helpers::make_to_u8<0, 1>()(begin());
        default: return {};
        }

    } ();
    if (v) { return *v; }
    else throw "";
}

template <ColorString::Component kt_component>
constexpr int ColorString::ComponentFunctions<kt_component>::length() const {
    switch (m_str->length()) {
    case 4:
        if constexpr (kt_component == k_alpha) return 0;
    case 5: return 1;
    case 7:
        if constexpr (kt_component == k_alpha) return 0;
    case 9: return 2;
    default: throw std::runtime_error("");
    }
}

// ------------------------------- ColorString --------------------------------

constexpr inline ColorString::ColorString(const char * str):
    m_str(init(str)), // init verifies str
    m_len(Helpers::len_of(str))
{}

inline ColorString::ColorString
    (const char * rpart, const char * gpart, const char * bpart,
     const char * apart)
{
    using H = Helpers;
    // kinda have to "cheat" here
    // *only* apart maybe nullptr
    const std::array lens = {
                H::len_of(verify_valid_portion("ColorString", rpart)),
                H::len_of(verify_valid_portion("ColorString", gpart)),
                H::len_of(verify_valid_portion("ColorString", bpart)),
        apart ? H::len_of(verify_valid_portion("ColorString", apart)) : 0
    };
    const bool any_long = std::any_of(lens.begin(), lens.end(), [](int i) { return i > 1; });
    if (any_long) {

    }
}

constexpr ColorString & ColorString::operator = (const char * str) {
    m_str = init(str);
    m_len = Helpers::len_of(str);
    return *this;
}

constexpr uint32_t ColorString::to_rgba_u32() const {
    return    uint32_t(red ().u8())        | (uint32_t(green().u8()) << 8)
           | (uint32_t(blue().u8()) << 16) | (uint32_t(alpha().u8()) << 24);
}

inline ColorString ColorString::new_length(int, uint8_t alpha) const {
    throw "";
}

constexpr int ColorString::length() const
    { return m_len; }

/* private static */ constexpr const char *
    ColorString::verify_valid_string
    (const char * caller, const char * value)
{
    using InvArg = std::invalid_argument;
    const auto * end = Helpers::end_of(value);
    switch (end - value) {
    case 4: case 5: case 7: case 9: break;
    default:
        throw InvArg(std::string(caller) + ": color string must have a size "
                     "of 4, 5, 7, or 9 characters.");
    }
    if (*value != '#') {
        throw InvArg(std::string(caller) + ": color string must be prefaced "
                     "with '#'.");
    }
    if (Helpers::all_of(value + 1, end, is_valid_hex_char)) return value;
    else throw InvArg(std::string(caller) + ": invalid hex characters detected.");
}

/* private static */ constexpr const char * ColorString::verify_valid_portion
    (const char * caller, const char * value)
{
    using InvArg = std::invalid_argument;
    const auto * end = Helpers::end_of(value);
    if (end - value != 1 && end - value != 2) {
        throw InvArg(std::string(caller) + ": portion string must be 1 or 2 characters.");
    }
    if (Helpers::all_of(value, end, is_valid_hex_char)) return value;
    else throw InvArg(std::string(caller) + ": invalid hex characters detected.");
}

/* private static */ constexpr bool ColorString::is_valid_hex_char(char c)
    { return std::get<bool>(Helpers::id_hex_char(c)); }

/* private static */ constexpr uint8_t ColorString::char_to_val(char c) {
    const auto [b, rv] = Helpers::id_hex_char(c);
    if (b) return rv;
    else throw "";
}

/* private static */ constexpr ColorString
    ColorString::make_with_byte_components
    (const char * rpart, const char * gpart, const char * bpart,
     const char * apart) noexcept
{
    using H = Helpers;
    // all verifies are now assertions with noexcept
    const std::array lens = {
                H::len_of(verify_valid_portion("ColorString", rpart)),
                H::len_of(verify_valid_portion("ColorString", gpart)),
                H::len_of(verify_valid_portion("ColorString", bpart)),
        apart ? H::len_of(verify_valid_portion("ColorString", apart)) : 0
    };
    assert(!H::any_of(&lens.front(), &lens.back() + 1, [](int i) { return i == 1; }));
    const std::array k_str { '#',
        rpart[0], rpart[1], gpart[0], gpart[1], bpart[0], bpart[1],
        apart ? apart[0] : '\0',
        apart ? apart[1] : '\0',
        '\0'
    };
    return ColorString{k_str.data()};
}

/* private static */ constexpr ColorString::FullString ColorString::init
    (const char * str)
{
    auto end = Helpers::end_of(verify_valid_string("copy", str));

    FullString rv{};
    auto otr = rv.begin();
    auto wtr = str;
    while (wtr != end) {
        *otr++ = *wtr++;
    }
    return rv;
}

/* private */ constexpr int ColorString::compare(const ColorString & rhs) const
    { return compare(rhs.c_str()); }

/* private */ constexpr int ColorString::compare(const char * cstr) const {
    struct F final {
    static constexpr int compare(const char * l, const char * r) {
        if (!*l || !*r) return *l - *r;
        if (*l == *r) return compare(l + 1, r + 1);
        return *l - *r;
    }
    };
    return F::compare(c_str(), cstr);
}

inline /* private */ ColorString & ColorString::copy(const ColorString & rhs) {
    return copy(rhs.m_str.data());
}

inline /* private */ ColorString & ColorString::copy(const char * s) {
    std::copy(s, Helpers::end_of(verify_valid_string("copy", s)),
        m_str.begin());
    return *this;
}

// ---------------------------- Compile Time Tests ----------------------------

static_assert(ColorStringHelpers::len_of("hello") == 5, "");
//static_assert(ColorStringHelpers::char_to_val('7') == 7, "");
static_assert(ColorStringHelpers::component_string_offset<ColorString::k_red>(4) == 1, "");
static_assert(ColorString{}.length() == 0, "");
static_assert((ColorString{} = "#777").length() == 4, "");
static_assert(ColorString{"#777"}.length() == 4, "");
static_assert(ColorString{"#777"}.red().length() == 1, "");
static_assert(   ColorString{"#678"}.red().part()[0] == '6'
              && ColorString{"#678"}.red().part()[1] == '\0', "");
static_assert(ColorStringHelpers::make_to_u8<0, 0>()("1") == 0x11, "");
static_assert(ColorString{"#777"}.red().u8() == 0x77, "");
static_assert(ColorString{"#123456"}.red().length() == 2, "");
static_assert(ColorString{"#123456"}.red  ().u8() == 0x12, "");
static_assert(ColorString{"#123456"}.green().u8() == 0x34, "");
static_assert(ColorString{"#123456"}.blue ().u8() == 0x56, "");
static_assert(ColorString{"#123456"}.alpha().u8() == 0xFF, "");
static_assert(ColorString{"#888"}.green().replace_with("4").green().u8() == 0x44, "");
static_assert(ColorStringHelpers::portion_val(1, 2, 8) == 4, "");

static_assert(ColorString{"#888"}.green().u8() == 0x88, "");
static_assert(ColorStringHelpers::portion_val(1, 2, 0x88) == 0x44, "");
static_assert(ColorString{"#888"}.green().replace_with("4").green().u8() == 0x44, "");

static_assert(ColorStringHelpers::portion_val( 1, 2, ColorString{"#888"}.green().u8() ) == 0x44, "");

static_assert(ColorString{"#888"}.green().portion(1, 2).green().u8() == 0x44, "");
static_assert(ColorString{"#888"} > ColorString{"#777"}, "");
static_assert(ColorString{"#EF9"} == "#EF9", "");
