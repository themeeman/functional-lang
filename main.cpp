#include <iostream>
#include <cstdint>
#include <asmjit/x86.h>
#include <cassert>
#include <algorithm>
#include <vector>
#include <optional>
#include <charconv>
#include <memory>
#include <format>
#include <ranges>

#define MOVE(...) (static_cast<std::remove_reference_t<decltype(__VA_ARGS__)>&&>(__VA_ARGS__))

using i64 = std::int64_t;
using f64 = double;

template <i64 N>
struct static_string {
    consteval static_string(const char *s) { // NOLINT
        for (i64 i = 0; i < N; i++) {
            buf[i] = *s++;
        }
    }

    consteval friend bool operator==(static_string, static_string) = default;

    char buf[N] = {};

    constexpr char operator[](i64 i) const {
        return buf[i];
    }

    constexpr static inline auto size = N;

    constexpr explicit operator std::string_view() const {
        return std::string_view(std::begin(buf), std::end(buf));
    }
};

template <i64 N>
static_string(const char(&)[N]) -> static_string<N-1>;

template <i64 A, i64 B>
consteval bool operator==(static_string<A>, static_string<B>) {
    return false;
}

template <static_string S>
consteval auto operator"" _c() {
    return S;
}

struct Token {
    enum class Type : i64 {
        IDEN,
        STRING_LIT,
        INT_LIT,
        FLOAT_LIT,
        IMAG_INT_LIT,
        IMAG_FLOAT_LIT,

        END = -1,
    };

    using enum Type;

    struct End {};

    ~Token()  {
        if (type_ == IDEN || type_ == STRING_LIT) {
            std::destroy_at(&value.str);
        }
    }

    Token(const Token& token) : line(token.line), col(token.col), type_(token.type_), value{} {
        if (token.type_ == IDEN || type_ == STRING_LIT) {
            std::construct_at(&value.str, token.value.str);
        } else {
            memcpy(&value.prim, &token.value.prim, sizeof value.prim);
        }
    }

    Token(Token&& token) noexcept : line(token.line), col(token.col), type_(token.type_), value{} {
        if (token.type_ == IDEN || type_ == STRING_LIT) {
            std::construct_at(&value.str, MOVE(token.value.str));
        } else {
            memcpy(&value.prim, &token.value.prim, sizeof value.prim);
        }
    }

    Token& operator=(const Token& token) {
        line = token.line;
        col = token.col;

        if (token.type_ != IDEN && token.type_ != STRING_LIT) {
            if (type_ == IDEN || type_ == STRING_LIT) {
                std::destroy_at(&value.str);
            }

            memcpy(&value.prim, &token.value.prim, sizeof value.prim);
        } else {
            std::construct_at(&value.str, token.value.str);
        }

        type_ = token.type_;
        return *this;
    }

    Token& operator=(Token&& token) noexcept {
        line = token.line;
        col = token.col;

        if (token.type_ != IDEN && token.type_ != STRING_LIT) {
            if (type_ == IDEN || type_ == STRING_LIT) {
                std::destroy_at(&value.str);
            }

            memcpy(&value.prim, &token.value.prim, sizeof value.prim);
        } else {
            std::construct_at(&value.str, MOVE(token.value.str));
        }

        type_ = token.type_;
        return *this;
    }

    [[nodiscard]] bool is(Type t) const {
        return type_ == t;
    }

    template <i64 N>
    [[nodiscard]] bool is(const Type (&arr)[N]) const {
        return std::ranges::find(arr, type_) != std::end(arr);
    }

    template <Type Ty>
    [[nodiscard]] auto as() const {
        assert(Ty == type_);
        if constexpr (Ty == Type::INT_LIT || Ty == Type::IMAG_INT_LIT) {
            return value.prim.i;
        } else if constexpr (Ty == Type::FLOAT_LIT || Ty == Type::IMAG_FLOAT_LIT) {
            return value.prim.f;
        } else if constexpr (Ty == Type::IDEN || Ty == Type::STRING_LIT) {
            return value.str;
        } else {
            return End{};
        }
    }

    [[nodiscard]] i64 to_int() const {
        assert(is({ Type::INT_LIT, Type::IMAG_INT_LIT }));
        return value.prim.i;
    }

    [[nodiscard]] f64 to_float() const {
        assert(is({ Type::FLOAT_LIT, Type::IMAG_FLOAT_LIT }));
        return value.prim.f;
    }

    [[nodiscard]] const std::string& to_str() const& {
        assert(is({ Type::STRING_LIT, Type::IDEN }));

        return value.str;
    }

    [[nodiscard]] std::string to_str() && {
        assert(is({ Type::STRING_LIT, Type::IDEN }));

        return MOVE(value.str);
    }

    [[nodiscard]] Type type() const {
        return type_;
    }

private:
    explicit Token(i64 line, i64 col, Type t) : line(line), col(col), value{.prim = {.end = {}}}, type_(t) {
    }
    explicit Token(i64 line, i64 col, std::string str, Type t) : line(line), col(col), value{.str = MOVE(str)}, type_(t) {
        assert(t == IDEN || t == STRING_LIT);
    }
    explicit Token(i64 line, i64 col, i64 val, Type t) : line(line), col(col), value{.prim = {.i = val}}, type_(t) {
        assert(t == INT_LIT || t == IMAG_INT_LIT);
    }
    explicit Token(i64 line, i64 col, f64 val, Type t) : line(line), col(col), value{.prim = {.f = val}}, type_(t) {
        assert(t == FLOAT_LIT || t == IMAG_FLOAT_LIT);
    }

    i64 line;
    i64 col;
    union Value {
        union {
            End end;
            i64 i;
            f64 f;
        } prim;
        std::string str;

        ~Value() {}
    } value;
    Type type_;

    friend struct TokenBuilder;
};

template <auto... Es>
struct Enum {
    constexpr static inline i64 size = sizeof...(Es);
};

template <static_string... Ss> struct List {
    constexpr static inline std::string_view values[] = { std::string_view(Ss)... };
    constexpr static inline i64 size = sizeof...(Ss);
};

using Types = Enum<
Token::Type::IDEN,
Token::Type::STRING_LIT,
Token::Type::INT_LIT,
Token::Type::FLOAT_LIT,
Token::Type::IMAG_INT_LIT,
Token::Type::IMAG_FLOAT_LIT>;
using Symbols = List<"=", "(", ")", "{", "}", "||", "&&", "<", ">", "<=", ">=", "==", "!=", "+", "-", "|", "^", "*", "/", "%", "&", "**", "[", "]", ".*", ",", ":" >;
using Keywords = List<"const", "func", "return", "if", "while", "let", "bool", "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64" >;

template <typename, typename>
struct Combine {};

template <static_string... Syms, static_string... Keyws>
struct Combine<List<Syms...>, List<Keyws...>> : List<Syms..., Keyws...> {
};

template <static_string S, typename L> requires (std::ranges::find(L::values, std::string_view(S)) != std::end(L::values))
consteval i64 index(L) {
    return std::ranges::find(L::values, std::string_view(S)) - std::begin(L::values);
}

template <static_string S>
consteval Token::Type operator""_tok() {
    return static_cast<Token::Type>(Types::size + index<S>(Combine<Symbols, Keywords>{}));
}

constexpr Token::Type symbol(i64 index) {
    return static_cast<Token::Type>(Types::size + index);
}

constexpr Token::Type keyword(i64 index) {
    return static_cast<Token::Type>(Types::size + Symbols::size + index);
}

constexpr std::string_view to_string(Token::Type t) {
    return Combine<Symbols, Keywords>::values[static_cast<i64>(t) - Types::size];
}

struct imag_t {};

struct TokenBuilder {
    TokenBuilder(i64 l, i64 c) : line(l), col(c) {
    }

    [[nodiscard]] Token end() const {
        return Token(line, col, Token::Type::END);
    }
    [[nodiscard]] Token iden(std::string s) const {
        return Token(line, col, MOVE(s), Token::IDEN);
    }
    [[nodiscard]] Token str(std::string s) const {
        return Token(line, col, MOVE(s), Token::STRING_LIT);
    }
    [[nodiscard]] Token int_(i64 i) const {
        return Token(line, col, i, Token::INT_LIT);
    }
    [[nodiscard]] Token int_(i64 i, imag_t) const {
        return Token(line, col, i, Token::IMAG_INT_LIT);
    }
    [[nodiscard]] Token float_(f64 f) const {
        return Token(line, col, f, Token::FLOAT_LIT);
    }
    [[nodiscard]] Token float_(f64 f, imag_t) const {
        return Token(line, col, f, Token::IMAG_FLOAT_LIT);
    }
    Token operator()(Token::Type kw) const {
        return Token(line, col, kw);
    }

private:
    i64 line;
    i64 col;
};

template <std::forward_iterator Iter, static_string lineSep>
struct PositionalIterator {
    using difference_type = std::ptrdiff_t;
    using value_type = char;

    PositionalIterator() = default;
    explicit PositionalIterator(Iter i) : it(i) {}

    std::iter_reference_t<Iter> operator*() const {
        return *it;
    }

    PositionalIterator& operator++() {
        if (checkLineSep()) {
            col_ = 1;
            ++line_;
        } else {
            ++col_;
            ++it;
        }

        return *this;
    }

    PositionalIterator operator++(int) { // NOLINT
        auto self = *this;
        ++*this;
        return self;
    }

    friend bool operator==(PositionalIterator, PositionalIterator) = default;
    friend bool operator==(PositionalIterator lhs, Iter rhs) {
        return lhs.it == rhs;
    }

    [[nodiscard]] i64 line() const {
        return line_;
    }

    [[nodiscard]] i64 col() const {
        return col_;
    }

    [[nodiscard]] Iter base() const {
        return it;
    }

private:
    [[nodiscard]] bool checkLineSep() {
        if (*it != lineSep[0]) {
            return false;
        }

        std::forward_iterator auto cpy = it;
        ++cpy;
        for (i64 i = 1; i < lineSep.size; i++) {
            if (*cpy != lineSep[i]) return false;
            ++cpy;
        }

        it = cpy;
        return true;
    }

    Iter it;
    i64 line_ = 1;
    i64 col_ = 1;
};

static_assert(std::forward_iterator<PositionalIterator<char*, "\n">>);

template <std::forward_iterator Iter>
struct Lexer {
    template <std::ranges::forward_range R>
    explicit Lexer(R&& r) : Lexer(begin(r), end(r)) {}

    explicit Lexer(Iter b, Iter e) : iter_(PositionalIterator<Iter, "\n">(b)), end_(e) {}

    template <char Min, char Max>
    constexpr static bool in_range(char c) {
        return Min <= c && c <= Max;
    }

    constexpr static bool is_digit(char c) {
        return in_range<'0', '9'>(c);
    }

    constexpr static bool is_alpha(char c) {
        return in_range<'A', 'Z'>(c) || in_range<'a', 'z'>(c) || c == '_';
    }

    constexpr static bool is_iden(char c) {
        return is_alpha(c) || is_digit(c);
    }

    constexpr static bool is_ws(char c) {
        return c == '\t' || c == '\n' || c == '\v' || c == '\f' || c == '\r' || c == ' ';
    }

    [[nodiscard]] char lookahead() const {
        auto x = iter_;
        ++x;
        return *x;
    }

    Token eat() {
        while (iter_ != end_ && is_ws(*iter_)) ++iter_;

        auto builder = TokenBuilder(iter_.line(), iter_.col());

        if (iter_ != end_) {
            if (is_alpha(*iter_)) {
                auto start = iter_;
                auto end = eat_iden(start);
                iter_ = end;
                auto s = std::string(start.base(), end.base());
                if (auto x = find_keyword(s); x.has_value()) {
                    return set(builder(*x));
                } else {
                    return set(builder.iden(MOVE(s)));
                }
            } else if (is_digit(*iter_) || (*iter_ == '-' || *iter_ == '.') && is_digit(lookahead())) {
                auto start = iter_;
                auto end = start;
                bool imag = false;
                if (*end == '-') ++end;
                end = eat_digits(end); // integral part
                if (*end != '.' && *end != 'e' && *end != 'E') {
                    auto s = std::string(start.base(), end.base());
                    if (*end == 'i' || *end == 'I') {
                        ++end;
                        imag = true;
                    }
                    i64 i;
                    std::from_chars(s.data(), s.data() + s.size(), i);
                    iter_ = end;
                    return set(imag ? builder.int_(i, imag_t{}) : builder.int_(i));
                }

                if (*end == '.') ++end;
                end = eat_digits(end); // fractional part
                if (*end == 'e' || *end == 'E') {
                    ++end;
                    if (*end == '+' || *end == '-') ++end;

                    end = eat_digits(end); // exponent
                }
                auto s = std::string(start.base(), end.base());
                if (*end == 'i' || *end == 'I') {
                    ++end;
                    imag = true;
                }
                f64 f;
                std::from_chars(s.data(), s.data() + s.size(), f);
                iter_ = end;
                return set(imag ? builder.float_(f, imag_t{}) : builder.float_(f));
            } else if (*iter_ == '"') {
                ++iter_;
                std::string s;
                while (*iter_ != '"') {
                    if (*iter_ == '\\') {
                        ++iter_;
                    }
                    s.push_back(*iter_);
                    ++iter_;
                }
                ++iter_;
                return builder.str(MOVE(s));
            } else if (std::ranges::find(Symbols::values, *iter_, [](std::string_view s) { return s[0]; }) != std::end(Symbols::values)) {
                bool candidates[Symbols::size] = {};
                std::ranges::fill(candidates, true);
                const auto start = iter_;
                auto end = iter_;
                bool done = false;
                i64 scan = 0;
                Token::Type result;

                while (!done) {
                    done = true;
                    for (i64 i = 0; i < Symbols::size; i++) {
                        if (candidates[i]) {
                            done = false;
                            if (scan >= Symbols::values[i].size() || *end != Symbols::values[i][scan]) {
                                candidates[i] = false;
                            }
                        }
                    }
                    ++end;
                    for (i64 i = 0; i < Symbols::size; i++) {
                        if (candidates[i]) {
                            if (scan+1 == Symbols::values[i].size() && std::ranges::equal(start, end, Symbols::values[i].begin(), Symbols::values[i].end())) {
                                result = symbol(i);
                                iter_ = end;
                            }
                        }
                    }

                    scan++;
                }

                return set(builder(result));
            }
        }

        return set(builder.end());
    }

    [[nodiscard]] const Token& peek() const {
        return *current;
    }

private:
    PositionalIterator<Iter, "\n"> eat_iden(PositionalIterator<Iter, "\n"> it) {
        while (it != end_ && is_iden(*it)) ++it;

        return it;
    }

    PositionalIterator<Iter, "\n"> eat_digits(PositionalIterator<Iter, "\n"> it) {
        while (it != end_ && is_digit(*it)) ++it;

        return it;
    }

    std::optional<Token::Type> find_keyword(std::string_view tok) {
        i64 i = std::ranges::find(Keywords::values, tok) - std::begin(Keywords::values);

        return i == std::size(Keywords::values) ? std::nullopt : std::optional<Token::Type>(keyword(i));
    }

    Token& set(Token x) {
        current = MOVE(x);
        return *current;
    }

    std::optional<Token> current = {};
    PositionalIterator<Iter, "\n"> iter_;
    Iter end_;
};

template <typename T>
struct Box;

template <typename T>
Box<T> make_box(T x);

template <typename T>
struct Box {
    Box(const Box& other) : ptr(std::make_unique<T>(*other.ptr)) {}
    Box(Box&&) noexcept = default;

    Box& operator=(const Box& other) {
        ptr = std::make_unique<T>(*other.ptr);
    }

    Box& operator=(Box&&) noexcept = default;

    T& operator*() & { return *ptr; }
    const T& operator*() const& { return *ptr; }
    T&& operator*() && { return MOVE(*ptr); }
    const T&& operator*() const&& { return MOVE(*ptr); }

    T* operator->() {
        return &**this;
    }

    const T* operator->() const {
        return &**this;
    }

    friend Box<T> make_box<T>(T x);
private:
    explicit Box(std::unique_ptr<T> p) : ptr(MOVE(p)) {}

    std::unique_ptr<T> ptr;
};

template <typename T>
Box<T> make_box(T x) {
    return Box<T>(std::make_unique<T>(MOVE(x)));
}

struct Expr {
    Expr(const Expr& other) : type_{other.type_} {
        cpy(other);
    }

    Expr(Expr&& other) noexcept : type_{other.type_} {
        move(static_cast<Expr&&>(other));
    }

    Expr& operator=(const Expr& other) {
        destroy();
        cpy(other);
        type_ = other.type_;
        return *this;
    }

    Expr& operator=(Expr&& other) noexcept {
        destroy();
        move(static_cast<Expr&&>(other));
        type_ = other.type_;
        return *this;
    }

    ~Expr() {
        destroy();
    }

    enum class Type {
        INT_CONST,
        FLOAT_CONST,
        STRING_CONST,

        IDEN_EXPR,
        UNARY_EXPR,
        BINARY_EXPR,
        CALL_EXPR,
    };

    using enum Type;

    struct IntConst {
        i64 value;
        bool imag;
    };

    struct FloatConst {
        f64 value;
        bool imag;
    };

    struct StringConst {
        std::string value;
    };

    struct IdenExpr {
        std::string name;
    };

    struct CallExpr {
        Box<Expr> f;
        std::vector<Box<Expr>> args;
        Token::Type op;
    };

    struct UnaryExpr {
        Box<Expr> expr;
        Token::Type op;
    };

    struct BinaryExpr {
        Box<Expr> lhs;
        Box<Expr> rhs;
        Token::Type op;
    };

    explicit Expr(IntConst ic) : type_(INT_CONST), value{.prim = { .ic = ic }} {}
    explicit Expr(FloatConst fc) : type_(FLOAT_CONST), value{.prim = { .fc = fc }} {}
    explicit Expr(StringConst sc) : type_(STRING_CONST), value{ .sc = MOVE(sc) } {}
    explicit Expr(IdenExpr iden) : type_(IDEN_EXPR), value{ .iden = MOVE(iden) } {}
    explicit Expr(CallExpr call) : type_(CALL_EXPR), value{ .call = MOVE(call) } {}
    explicit Expr(UnaryExpr unary) : type_(UNARY_EXPR), value{ .unary = MOVE(unary) } {}
    explicit Expr(BinaryExpr bin) : type_(BINARY_EXPR), value{ .bin = MOVE(bin) } {}

    [[nodiscard]] Type type() const {
        return type_;
    }

    std::format_context::iterator format(std::format_context& ctx) const {
        switch (type_) {
        case Type::INT_CONST:
            if (value.prim.ic.imag) {
                std::format_to(ctx.out(), "{}i", value.prim.ic.value);
            } else {
                std::format_to(ctx.out(), "{}", value.prim.ic.value);
            }
            break;
        case Type::FLOAT_CONST:
            if (value.prim.fc.imag) {
                std::format_to(ctx.out(), "{}i", value.prim.fc.value);
            } else {
                std::format_to(ctx.out(), "{}", value.prim.fc.value);
            }
            break;
        case Type::STRING_CONST:
            std::format_to(ctx.out(), "'{}'", value.sc.value);
            break;
        case Type::IDEN_EXPR:
            std::format_to(ctx.out(), "{}", value.iden.name);
            break;
        case Type::UNARY_EXPR:
            std::format_to(ctx.out(), "(");
            value.unary.expr->format(ctx);
            std::format_to(ctx.out(), "){}", to_string(value.unary.op));
            break;
        case Type::BINARY_EXPR:
            std::format_to(ctx.out(), "{}(", to_string(value.bin.op));
            value.bin.lhs->format(ctx);
            std::format_to(ctx.out(), ", ");
            value.bin.rhs->format(ctx);
            std::format_to(ctx.out(), ")");
            break;
        case Type::CALL_EXPR:
            std::format_to(ctx.out(), "(");
            value.call.f->format(ctx);
            std::format_to(ctx.out(), "){}", to_string(value.call.op));
            if (!value.call.args.empty()) {
                value.call.args[0]->format(ctx);
                for (const auto& x : value.call.args | std::views::drop(1)) {
                    std::format_to(ctx.out(), ", ");
                    x->format(ctx);
                }
            }

            auto end = (value.call.op == "("_tok) ? ")" : "]";
            std::format_to(ctx.out(), "{}",end);
            break;
        }

        return ctx.out();
    }

    [[nodiscard]] bool is(Type ty) const {
        return ty == type_;
    }

    template <Type Ty>
    [[nodiscard]] const auto& as() const {
        assert(Ty == type_);
        if constexpr (Ty == Type::INT_CONST) {
            return value.prim.ic;
        } else if constexpr (Ty == Type::FLOAT_CONST) {
            return value.prim.fc;
        } else if constexpr (Ty == Type::STRING_CONST) {
            return value.sc;
        } else if constexpr (Ty == Type::IDEN_EXPR) {
            return value.iden;
        } else if constexpr (Ty == Type::UNARY_EXPR) {
            return value.unary;
        } else if constexpr (Ty == Type::BINARY_EXPR) {
            return value.bin;
        } else if constexpr (Ty == Type::CALL_EXPR) {
            return value.call;
        }
    }

    template <Type Ty>
    [[nodiscard]] auto as() && {
        assert(Ty == type_);
        if constexpr (Ty == Type::INT_CONST) {
            return MOVE(value.prim.ic);
        } else if constexpr (Ty == Type::FLOAT_CONST) {
            return MOVE(value.prim.fc);
        } else if constexpr (Ty == Type::STRING_CONST) {
            return MOVE(value.sc);
        } else if constexpr (Ty == Type::IDEN_EXPR) {
            return MOVE(value.iden);
        } else if constexpr (Ty == Type::UNARY_EXPR) {
            return MOVE(value.unary);
        } else if constexpr (Ty == Type::BINARY_EXPR) {
            return MOVE(value.bin);
        } else if constexpr (Ty == Type::CALL_EXPR) {
            return MOVE(value.call);
        }
    }

    template <typename F>
    decltype(auto) visit(F f) const& {
        switch (type_) {
        case Type::INT_CONST:
            return f(value.prim.ic);
        case Type::FLOAT_CONST:
            return f(value.prim.fc);
        case Type::STRING_CONST:
            return f(value.sc);
        case Type::IDEN_EXPR:
            return f(value.iden);
        case Type::UNARY_EXPR:
            return f(value.unary);
        case Type::BINARY_EXPR:
            return f(value.bin);
        case Type::CALL_EXPR:
            return f(value.call);
        }
        assert(false);
    }

    template <typename F>
    decltype(auto) visit(F f) && {
        switch (type_) {
        case Type::INT_CONST:
            return f(auto(value.prim.ic));
        case Type::FLOAT_CONST:
            return f(auto(value.prim.fc));
        case Type::STRING_CONST:
            return f(auto(MOVE(value.sc)));
        case Type::IDEN_EXPR:
            return f(auto(MOVE(value.iden)));
        case Type::UNARY_EXPR:
            return f(auto(MOVE(value.unary)));
        case Type::BINARY_EXPR:
            return f(auto(MOVE(value.bin)));
        case Type::CALL_EXPR:
            return f(auto(MOVE(value.call)));
        }
        assert(false);
    }

private:
    void destroy() {
        switch (type_) {
        case Type::INT_CONST:
        case Type::FLOAT_CONST:
            break;
        case Type::STRING_CONST:
            std::destroy_at(&value.sc);
            break;
        case Type::IDEN_EXPR:
            std::destroy_at(&value.iden);
            break;
        case Type::UNARY_EXPR:
            std::destroy_at(&value.unary);
            break;
        case Type::BINARY_EXPR:
            std::destroy_at(&value.bin);
            break;
        case Type::CALL_EXPR:
            std::destroy_at(&value.call);
            break;
        }
    }

    void cpy(const Expr& other) {
        switch (other.type_) {
        case Type::INT_CONST:
        case Type::FLOAT_CONST:
            memcpy(&value.prim, &other.value.prim, sizeof other.value.prim);
            break;
        case Type::STRING_CONST:
            std::construct_at(&value.sc, other.value.sc);
            break;
        case Type::UNARY_EXPR:
            std::construct_at(&value.unary, other.value.unary);
            break;
        case Type::BINARY_EXPR:
            std::construct_at(&value.bin, other.value.bin);
            break;
        case Type::CALL_EXPR:
            std::construct_at(&value.call, other.value.call);
            break;
        default:
            assert(false);
        }
    }

    void move(Expr&& other) {
        switch (other.type_) {
        case Type::INT_CONST:
        case Type::FLOAT_CONST:
            memcpy(&value.prim, &other.value.prim, sizeof other.value.prim);
            break;
        case Type::STRING_CONST:
            std::construct_at(&value.sc, MOVE(other.value.sc));
            break;
        case Type::IDEN_EXPR:
            std::construct_at(&value.iden, MOVE(other.value.iden));
            break;
        case Type::UNARY_EXPR:
            std::construct_at(&value.unary, MOVE(other.value.unary));
            break;
        case Type::BINARY_EXPR:
            std::construct_at(&value.bin, MOVE(other.value.bin));
            break;
        case Type::CALL_EXPR:
            std::construct_at(&value.call, MOVE(other.value.call));
            break;
        }
    }

    Type type_;

    union Value {
        union {
            IntConst ic{};
            FloatConst fc;
        } prim{};
        StringConst sc;
        IdenExpr iden;
        CallExpr call;
        UnaryExpr unary;
        BinaryExpr bin;

        ~Value() {}
    } value = {};
};

template<>
struct std::formatter<Expr, char> {
    constexpr format_parse_context::iterator parse(format_parse_context& ctx)
    {
        return ctx.begin();
    }

    static format_context::iterator format(const Expr& e, format_context& ctx)
    {
        return e.format(ctx);
    }
};

struct Func {

};

struct Parser {
    explicit Parser(const Lexer<std::string::const_iterator>& lexer) noexcept : lexer_(lexer) {
        lexer_.eat();
    }

    constexpr static inline Token::Type ops[] = {
    "**"_tok,
    "*"_tok,
    "/"_tok,
    "%"_tok,
    "&"_tok,
    "+"_tok,
    "-"_tok,
    "|"_tok,
    "^"_tok,
    "<"_tok,
    "<="_tok,
    ">"_tok,
    ">="_tok,
    "=="_tok,
    "!="_tok,
    "&&"_tok,
    "||"_tok,
    "="_tok,
    };

    constexpr static int prec(Token::Type op) {
        switch (op) {
        case "**"_tok:
            return 7;
        case "*"_tok:
        case "/"_tok:
        case "%"_tok:
        case "&"_tok:
            return 6;
        case "+"_tok:
        case "-"_tok:
        case "|"_tok:
        case "^"_tok:
            return 5;
        case "<"_tok:
        case "<="_tok:
        case ">"_tok:
        case ">="_tok:
        case "=="_tok:
        case "!="_tok:
            return 4;
        case "&&"_tok:
            return 3;
        case "||"_tok:
            return 2;
        case "="_tok:
            return 1;
        default:
            assert(false);
        }
    }

    Expr parse_expr() {
        using enum Token::Type;

        auto tok = lexer_.peek();
        assert(tok.is({ IDEN, INT_LIT, IMAG_INT_LIT, FLOAT_LIT, IMAG_FLOAT_LIT, STRING_LIT, "("_tok }));

        auto expr = parse_postfix();
        auto curPrec = 1;

        if (lexer_.peek().is(ops)) {
            expr = parse_bin_op_rhs(MOVE(expr));
        }

        return expr;
    }

    Expr parse_bin_op_rhs(Expr lhs, int minPrec = 0) {
        while (lexer_.peek().is(ops) && prec(lexer_.peek().type()) >= minPrec) {
            auto op = lexer_.peek().type();
            lexer_.eat();
            auto rhs = parse_postfix();

            while (lexer_.peek().is(ops) && (prec(lexer_.peek().type()) > prec(op) || (op == "="_tok && lexer_.peek().is("="_tok)))) {
                rhs = parse_bin_op_rhs(MOVE(rhs), prec(op) + (prec(lexer_.peek().type()) > prec(op)));
            }

            lhs = Expr( Expr::BinaryExpr{ make_box(MOVE(lhs)), make_box(MOVE(rhs)), op } );
        }

        return lhs;
    }

    Expr parse_postfix() {
        auto expr = parse_primary();
        while (lexer_.peek().is({ "("_tok, "["_tok, ".*"_tok })) {
            if (lexer_.peek().is({ "("_tok, "["_tok })) {
                auto op = lexer_.peek().type();
                lexer_.eat();
                auto end = op == "("_tok ? ")"_tok : "]"_tok;

                std::vector<Box<Expr>> args;
                while (!lexer_.peek().is(end)) {
                    args.push_back(make_box(parse_expr()));

                    assert(lexer_.peek().is({ ","_tok, end }));

                    if (lexer_.peek().is(","_tok)) {
                        lexer_.eat();
                    }
                }

                lexer_.eat();
                expr = Expr( Expr::CallExpr{ make_box(MOVE(expr)), MOVE(args), op } );
            } else {
                lexer_.eat();
                expr = Expr( Expr::UnaryExpr{ make_box(MOVE(expr)), ".*"_tok } );
            }
        }

        return expr;
    }

    Expr parse_primary() {
        using enum Token::Type;

        auto tok = lexer_.peek();

        if (tok.is(IDEN)) {
            lexer_.eat();
            return Expr(Expr::IdenExpr{ MOVE(tok).to_str() });
        } else if (tok.is({ INT_LIT, IMAG_INT_LIT })) {
            lexer_.eat();
            return Expr{ Expr::IntConst{ tok.to_int(), tok.type() == IMAG_INT_LIT } };
        } else if (tok.is({ FLOAT_LIT, IMAG_FLOAT_LIT })) {
            lexer_.eat();
            return Expr{ Expr::FloatConst{ tok.to_float(), tok.type() == IMAG_FLOAT_LIT } };
        } else if (tok.is(STRING_LIT)) {
            lexer_.eat();
            return Expr{ Expr::StringConst{ MOVE(tok).to_str() } };
        } else if (tok.is("("_tok)) {
            lexer_.eat();
            auto expr = parse_expr();
            assert(lexer_.peek().is(")"_tok));
            lexer_.eat();
            return expr;
        }
    }

private:

    Lexer<std::string::const_iterator> lexer_;
};


struct Type {
    enum class Class {
        I8, I16, I32, I64, I128,
        U8, U16, U32, U64, U128,
        BOOL, TYPE_REF,
    };
};


struct Program {
    Expr expr;
};

class MyErrorHandler : public asmjit::ErrorHandler {
public:
    void handleError(asmjit::Error err, const char* message, asmjit::BaseEmitter* origin) override {
        printf("AsmJit error: %s\n", message);
    }
};

struct Compiler {
    Compiler() {
        code.init(rt.environment(), rt.cpuFeatures());
        code.setErrorHandler(&myErrorHandler);
        code.attach(&cc);
    }

    void codegen(const Program& e) {
        using Func = int(*)();
        asmjit::FuncNode* func = cc.addFunc(asmjit::FuncSignature::build<int>());
        asmjit::Operand res = codegen(e.expr);

        if (res.opType() == asmjit::OperandType::kReg) {
            cc.ret(res.as<asmjit::x86::Gp>());
        } else if (res.opType() == asmjit::OperandType::kImm) {
            auto r = cc.newGpd();
            cc.mov(r, res.as<asmjit::Imm>());
            cc.ret(r);
        }
        cc.endFunc();

        asmjit::String s;
        asmjit::Formatter::formatNodeList(s, {}, &cc);
        printf("%s\n-----------------\n", s.data());


        cc.finalize();

        Func fn;
        rt.add(&fn, &code);
        printf("%d\n", fn());
        rt.release(fn);

    }

    asmjit::Operand codegen(const Expr& e) {
        if (e.is(Expr::Type::INT_CONST)) {
            return asmjit::imm(e.as<Expr::Type::INT_CONST>().value);
        } else if (e.is(Expr::Type::BINARY_EXPR)) {
            const auto& bin = e.as<Expr::Type::BINARY_EXPR>();

            auto lhs = codegen(*bin.lhs);
            auto rhs = codegen(*bin.rhs);
            switch (bin.op) {
            case "+"_tok: {
                auto r = cc.newInt32();
                if (lhs.opType() == asmjit::OperandType::kReg) {
                    cc.mov(r, lhs.as<asmjit::x86::Gp>());
                } else if (lhs.opType() == asmjit::OperandType::kImm) {
                    cc.mov(r, lhs.as<asmjit::Imm>());
                }

                if (rhs.opType() == asmjit::OperandType::kReg) {
                    cc.add(r, rhs.as<asmjit::x86::Gp>());
                } else if (rhs.opType() == asmjit::OperandType::kImm) {
                    cc.add(r, rhs.as<asmjit::Imm>());
                }
                return r;
            }
//            case "-"_tok: {
//                auto r = cc.newInt32();
//                cc.mov(r, lhs);
//                cc.sub(r, rhs);
//                return r;
//            }
//            case "*"_tok: {
//                auto r = cc.newInt32();
//                cc.mov(r, lhs);
//                cc.imul(r, rhs);
//                return r;
//            }
//            case "/"_tok: {
//                auto quot = cc.newGpd();
//                auto rem = cc.newGpd();
//                cc.mov(quot, lhs);
//                cc.cdq( rem, quot);
//                cc.idiv(rem, quot, rhs);
//                return quot;
//            }
//            case "%"_tok: {
//                auto quot = cc.newGpd();
//                auto rem = cc.newGpd();
//                cc.mov(quot, lhs);
//                cc.cdq( rem, quot);
//                cc.idiv(rem, quot, rhs);
//                return rem;
//            }
//            case "&"_tok: {
//                auto r = cc.newInt32();
//                cc.mov(r, lhs);
//                cc.and_(r, rhs);
//                return r;
//            }
//            case "|"_tok: {
//                auto r = cc.newInt32();
//                cc.mov(r, lhs);
//                cc.or_(r, rhs);
//                return r;
//            }
//            case "^"_tok: {
//                auto r = cc.newInt32();
//                cc.mov(r, lhs);
//                cc.xor_(r, rhs);
//                return r;
//            }

            assert(false);
            }
        }
        assert(false);
    }

    MyErrorHandler myErrorHandler;
    asmjit::JitRuntime rt;
    asmjit::CodeHolder code;
    asmjit::x86::Compiler cc;
};

int main() {
    std::string s = "return x0 + 2.3";
    Lexer l(begin(s), end(s));
    assert(l.eat().type() == "return"_tok);
    assert(l.eat().to_str() == "x0");
    assert(l.eat().is("+"_tok));
    assert(l.eat().to_float() == 2.3);
    assert(l.eat().type() == Token::END);

    std::string s2 = " 4";
    Parser p ( Lexer(cbegin(s2), cend(s2)) );

    auto y = p.parse_expr();

//    std::cout << to_string("**"_tok);
    std::cout << std::format("{}\n", y);
//    std::cout << (l.eat()->as<Token::Type::SYMBOL>() == "!="_sym) << '\n';

    static_assert(symbol(3) == "{"_tok);
    assert(to_string("^"_tok) == "^");

    Compiler c;
    c.codegen(Program{ y });
}
