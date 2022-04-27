#include "LN.h"

//region constructors and destructor
LN::LN(long long num)
{
    initVector();

    isNaN_ = false;
    isNegative_ = num < 0;
    if (num == 0LL) {
        push_back(0);
    }

    while (num != 0) {
        digit_t last = num % 10; // NOLINT(cppcoreguidelines-narrowing-conversions)
        last = last < 0 ? -last : last; // NOLINT(cppcoreguidelines-narrowing-conversions)

        push_back(last);

        num /= 10;
    }
}

LN operator ""_ln(const char *s)
{
    return LN(s);
}

LN::LN(const char *s)
{
    size_t length = strlen(s);
    initVector(length);

    isNegative_ = false;
    isNaN_ = false;
    if (length == 3 && s[0] == 'N' && s[1] == 'a' && s[2] == 'N') {
        isNaN_ = true;
        return;
    }

    if (length > 0) {
        size_ = length;
        if (s[0] == '-') {
            isNegative_ = true;
            size_--;
        }

        for (size_t i = 0; i < size_; ++i) {
            digits_[i] = (digit_t) (s[length - i - 1] - '0');
        }
    } else {
        isNaN_ = true;
    }
}

LN::LN(const std::string_view &s)
{
    size_t length = s.length();
    initVector(length);

    isNegative_ = false;
    isNaN_ = false;
    if (length == 3 && s[0] == 'N' && s[1] == 'a' && s[2] == 'N') {
        isNaN_ = true;
        return;
    }

    if (length > 0) {
        size_ = length;
        if (s[0] == '-') {
            isNegative_ = true;
            size_--;
        }

        for (size_t i = 0; i < size_; ++i) {
            digits_[i] = (digit_t) (s[length - i - 1] - '0');
        }
    } else {
        isNaN_ = true;
    }
}

LN::LN(bool b)
{
    initVector(1);
    push_back((digit_t) b);
    isNaN_ = false;
    isNegative_ = false;
}

LN::~LN()
{
    free(digits_);
}
//endregion

//region copy and move
LN::LN(const LN &that)
{
    initVector(that.size_);

    std::memcpy(digits_, that.digits_, that.size_ * sizeof(digit_t));
    isNaN_ = that.isNaN_;
    isNegative_ = that.isNegative_;
    size_ = that.size_;
    capacity_ = that.capacity_;
}

LN::LN(LN &&that) noexcept
{
    digits_ = that.digits_;
    isNegative_ = that.isNegative_;
    isNaN_ = that.isNaN_;
    size_ = that.size_;
    capacity_ = that.capacity_;

    that.digits_ = nullptr;
    that.size_ = 0;
    that.capacity_ = 0;
}

LN &LN::operator=(const LN &that)
{
    if (this == &that) {
        return *this;
    }

    resize(that.size_);

    std::memcpy(digits_, that.digits_, that.size_ * sizeof(digit_t));
    isNaN_ = that.isNaN_;
    isNegative_ = that.isNegative_;
    size_ = that.size_;
    capacity_ = that.capacity_;
    return *this;
}

LN &LN::operator=(LN &&that) noexcept
{
    free(digits_);
    digits_ = that.digits_;
    isNegative_ = that.isNegative_;
    isNaN_ = that.isNaN_;
    size_ = that.size_;
    capacity_ = that.capacity_;

    that.digits_ = nullptr;
    that.size_ = 0;
    that.capacity_ = 0;
    return *this;
}
//endregion

//region cast operators
LN::operator long long() const
{
    if (isNaN_) {
        throw std::bad_cast();
    }

    if (isZero()) {
        return 0LL;
    }

    long long mul = isNegative_ ? -1 : 1;
    long long res = digits_[0] * mul;
    long long lastRes = res;
    long long power = 10;
    for (size_t i = 1; i < size_; ++i) {
        res += (digits_[i] * mul) * power;
        if ((!isNegative_ && res < lastRes) ||
            (isNegative_ && res > lastRes)) {
            throw std::bad_cast();
        }

        power *= 10;
        lastRes = res;
    }

    return res;
}

LN::operator bool() const
{
    if (isNaN_) {
        return true;
    }
    return !isZero();
}

LN::operator std::string() const
{
    if (isNaN_) {
        return "NaN";
    }

    if (isZero()) {
        return "0";
    }

    std::string res = isNegative_ ? "-" : "";
    for (size_t i = 0; i < size_; ++i) {
        res += (char) digits_[size_ - i - 1] + '0'; // NOLINT(cppcoreguidelines-narrowing-conversions)
    }

    return res;
}
//endregion

//region stream operators
std::ostream &operator<<(std::ostream &os, const LN &num)
{
    if (num.isNaN_) {
        os << "NaN";
        return os;
    }

    if (num.isZero()) {
        os << 0;
        return os;
    }

    if (num.isNegative_) {
        os << "-";
    }
    for (size_t i = 0; i < num.size_; ++i) {
        os << static_cast<int>(num.digits_[num.size_ - i - 1]);
    }

    return os;
}

std::istream &operator>>(std::istream &is, LN &num)
{
    std::string t;
    if (!(is >> t)) {
        return is;
    }
    num = LN(t);
    return is;
}
//endregion

//region order operators and equals methods
bool LN::isZero() const
{
    if (isNaN_) {
        return false;
    }

    return size_ == 0 || (size_ == 1 && digits_[0] == 0);
}

bool LN::isUnsignedOne() const
{
    if (isNaN_) {
        return false;
    }

    return size_ == 1 && digits_[0] == 1;
}

int LN::sign() const
{
    if (isZero()) {
        return 0;
    }

    return isNegative_ ? -1 : 1;
}

int LN::compare(const LN &that) const
{
    int s1 = sign();
    int s2 = that.sign();
    if (s1 != s2) {
        return s1 - s2;
    }
    if (s1 == 0 && s2 == 0) {
        return 0;
    }

    if (size_ != that.size_) {
        if (s1 == 1) {
            return size_ > that.size_ ? 1 : -1;
        }
        if (s1 == -1) {
            return that.size_ > size_ ? 1 : -1;
        }
    }

    for (size_t i = 0; i < size_; ++i) {
        size_t id = size_ - i - 1;
        if (digits_[id] == that.digits_[id]) {
            continue;
        }

        if (s1 == 1) {
            return digits_[id] > that.digits_[id] ? 1 : -1;
        }
        if (s1 == -1) {
            return that.digits_[id] > digits_[id] ? 1 : -1;
        }
    }

    return 0;
}

int LN::compareByAbs(const LN &that) const
{
    if (size_ != that.size_) {
        return size_ > that.size_ ? 1 : -1;
    }

    for (size_t i = 0; i < size_; ++i) {
        size_t id = size_ - i - 1;
        if (digits_[id] == that.digits_[id]) {
            continue;
        }

        return digits_[id] > that.digits_[id] ? 1 : -1;
    }

    return 0;
}


bool operator<(const LN &l, const LN &r)
{
    if (l.isNaN_ || r.isNaN_) {
        return false;
    }

    return l.compare(r) < 0;
}

bool operator<=(const LN &l, const LN &r)
{
    if (l.isNaN_ || r.isNaN_) {
        return false;
    }

    return l.compare(r) <= 0;
}

bool operator>(const LN &l, const LN &r)
{
    if (l.isNaN_ || r.isNaN_) {
        return false;
    }

    return l.compare(r) > 0;
}

bool operator>=(const LN &l, const LN &r)
{
    if (l.isNaN_ || r.isNaN_) {
        return false;
    }

    return l.compare(r) >= 0;
}

bool operator==(const LN &l, const LN &r)
{
    if (l.isNaN_ || r.isNaN_) {
        return false;
    }

    return l.compare(r) == 0;
}

bool operator!=(const LN &l, const LN &r)
{
    if (l.isNaN_ || r.isNaN_) {
        return true;
    }

    return l.compare(r) != 0;
}
//endregion

//region arithmetics
void LN::digit_sum(const LN &that)
{
    size_t sizeMax = std::max(size_, that.size_);
    resize(sizeMax + 1);
    for (size_t i = 0; i < sizeMax; i++) {
        if (i < that.size_) {
            digits_[i] += that.digits_[i]; // NOLINT(cppcoreguidelines-narrowing-conversions)
        }
        digits_[i + 1] += (digits_[i] > 9); // NOLINT(cppcoreguidelines-narrowing-conversions)
        digits_[i] %= 10;
    }
    digits_[size_ - 1] %= 10;
}

void LN::digit_sub(const LN &that)
{
    int compRes = compareByAbs(that);
    if (compRes == 0) {
        makeZero();
    } else if (compRes > 0) {
        for (size_t i = 0; i < size_; i++) {
            if (i < that.size_) {
                digits_[i] -= that.digits_[i]; // NOLINT(cppcoreguidelines-narrowing-conversions)
            }
            if (digits_[i] < 0) {
                for (size_t j = i + 1; j < size_; j++) {
                    if (digits_[j] != 0) {
                        digits_[j]--;
                        setOnRange<digit_t>(digits_, i + 1, j, 9);
                        break;
                    }
                }
                digits_[i] += 10;
            }
        }
    } else if (compRes < 0) {
        isNegative_ = !isNegative_;
        auto *res = (digit_t *) malloc(that.size_ * sizeof(digit_t));
        if (res == nullptr) {
            throw std::bad_alloc();
        }

        memcpy(res, that.digits_, that.size_ * sizeof(digit_t));
        for (size_t i = 0; i < that.size_; i++) {
            if (i < size_) {
                res[i] -= digits_[i]; // NOLINT(cppcoreguidelines-narrowing-conversions)
            }
            if (res[i] < 0) {
                for (size_t j = i + 1; j < that.size_; j++) {
                    if (res[j] != 0) {
                        res[j]--;
                        setOnRange<digit_t>(res, i + 1, j, 9);
                        break;
                    }
                }
                res[i] += 10;
            }
        }

        free(digits_);
        digits_ = res;
        capacity_ = that.size_;
        size_ = that.size_;
    }
}

LN &LN::operator+=(const LN &that)
{
    if (isNaN_ || that.isNaN_) {
        isNaN_ = true;
        return *this;
    }

    if (isNegative_ == that.isNegative_) {
        digit_sum(that);
    } else {
        digit_sub(that);
    }

    removeLeadZeros();

    return *this;
}

LN operator+(const LN &l, const LN &r)
{
    LN res(l);
    res += r;
    return res;
}

LN &LN::operator-=(const LN &that)
{
    if (isNaN_ || that.isNaN_) {
        isNaN_ = true;
        return *this;
    }

    if (isNegative_ == that.isNegative_) {
        digit_sub(that);
    } else {
        digit_sum(that);
    }

    removeLeadZeros();

    return *this;
}

LN operator-(const LN &l, const LN &r)
{
    LN res(l);
    res -= r;
    return res;
}

LN &LN::operator*=(const LN &that)
{
    *this = (*this) * that;
    return *this;
}

LN operator*(const LN &l, const LN &r)
{
    if (l.isNaN_ || r.isNaN_) {
        return LN("NaN");
    }
    if (l.isZero() || r.isZero()) {
        return 0_ln;
    }

    LN res(0LL);
    size_t length = l.size_ + r.size_;
    auto *c = (long long *) malloc((length + 1) * sizeof(long long));
    if (c == nullptr) {
        throw std::bad_alloc();
    }

    LN::setOnRange<long long>(c, 0, length + 1, 0);
    for (size_t i = 0; i < l.size_; i++) {
        for (size_t j = 0; j < r.size_; j++) {
            c[i + j] += l.digits_[i] * r.digits_[j];
        }
    }

    for (size_t i = 0; i < length; i++) {
        c[i + 1] += c[i] / 10;
        c[i] %= 10;
    }

    res.resize(length + 1, 0);
    for (size_t i = 0; i <= length; i++) {
        res.digits_[i] = (LN::digit_t) c[i];
    }

    free(c);

    res.removeLeadZeros();
    res.isNegative_ = l.isNegative_ ^ r.isNegative_;
    return res;
}

LN &LN::operator/=(const LN &that)
{
    *this = (*this) / that;
    return *this;
}

LN operator/(const LN &l, const LN &r)
{
    if (l.isNaN_ || r.isNaN_ || r.isZero()) {
        return LN("NaN");
    }

    if (r.isUnsignedOne()) {
        return (r.isNegative_ ? -l : l);
    }
    if (l.compareByAbs(r) < 0) {
        return 0_ln;
    }
    if (l.compareByAbs(r) == 0) {
        return (l.isNegative_ ^ r.isNegative_ ? -1_ln : 1_ln);
    }

    LN res;
    res.resize(l.size_);

    LN rest = 0_ln;
    for (size_t i = 0; i < l.size_; ++i) {
        size_t id = l.size_ - i - 1;
        rest.push_front(0);
        rest += LN((long long) l.digits_[id]);
        res.digits_[id] = 0;
        while (rest.compareByAbs(r) >= 0) {
            rest += (r.isNegative_ ? r : -r);
            res.digits_[id]++;
        }
    }

    res.removeLeadZeros();
    res.isNegative_ = (l.isNegative_ ^ r.isNegative_);
    return res;
}

LN &LN::operator%=(const LN &that)
{
    *this = (*this) % that;
    return *this;
}

LN operator%(const LN &l, const LN &r)
{
    if (l.isNaN_ || r.isNaN_ || r.isZero()) {
        return LN("NaN");
    }

    return l - l / r * r;
}

LN operator-(const LN &that)
{
    LN res(that);
    res.isNegative_ = !res.isNegative_;
    return res;
}

LN &LN::operator-()
{
    isNegative_ = !isNegative_;
    return *this;
}

LN LN::sqrt() const
{
    if (isNaN_ || isNegative_) {
        return LN("NaN");
    }

    if (isZero()) {
        return 0_ln;
    }

    LN low = 0_ln;
    LN high = *this + 1_ln;
    while (high - low > 1_ln) {
        LN mid = (low + high) / 2_ln;
        if (mid * mid <= *this) {
            low = mid;
        } else {
            high = mid;
        }
    }

    return low;
}
//endregion

// ---------------
// -   PRIVATE   -
// ---------------

//region vector logic
void LN::initVector(const size_t &requiredCapacity)
{
    digits_ = (digit_t *) malloc(requiredCapacity * sizeof(digit_t));
    if (digits_ == nullptr) {
        throw std::bad_alloc();
    }
    size_ = 0;
    capacity_ = requiredCapacity;
}

void LN::push_back(const digit_t &item)
{
    ensureCapacity(size_ + 1);
    digits_[size_++] = item;
}

void LN::push_front(const digit_t &item)
{
    ensureCapacity(size_ + 1);
    memmove(digits_ + 1, digits_, (size_++) * sizeof(digit_t));
    digits_[0] = item;
}

void LN::resize(size_t newSize, const digit_t &x)
{
    ensureCapacity(newSize);
    setOnRange(digits_, size_, newSize, x);
    size_ = newSize;
}

void LN::ensureCapacity(const size_t &requiredCapacity)
{
    if (requiredCapacity <= capacity_) {
        if (requiredCapacity < capacity_ / 4) {
            size_t newCapacity = capacity_ / 2;
            auto *newArr = (digit_t *) realloc(digits_, newCapacity * sizeof(digit_t));
            if (newArr == nullptr) {
                return;
            }

            digits_ = newArr;
            capacity_ = newCapacity;
        }
        return;
    }

    size_t newCapacity = requiredCapacity * 2;
    auto *newArr = (digit_t *) realloc(digits_, newCapacity * sizeof(digit_t));
    if (newArr == nullptr) {
        throw std::bad_alloc();
    }

    digits_ = newArr;
    capacity_ = newCapacity;
}

template<typename T>
void LN::setOnRange(T *arr, const size_t &start, const size_t &end, const T &x)
{
    for (size_t i = start; i < end; ++i) {
        arr[i] = x;
    }
}

void LN::removeLeadZeros()
{
    for (size_t i = 0; i < size_; ++i) {
        size_t id = size_ - i - 1;
        if (digits_[id] != 0) {
            resize(id + 1);
            return;
        }
    }
    resize(1); // or 0
}

void LN::makeZero()
{
    resize(1);
    digits_[0] = 0;
    isNegative_ = false;
}
//endregion