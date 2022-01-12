#ifndef LABORATORNAYA_RABOTA_4_DLINNAYA_ARIFMETIKA_PECHHENKA_LN_H
#define LABORATORNAYA_RABOTA_4_DLINNAYA_ARIFMETIKA_PECHHENKA_LN_H

#include <string>
#include <stdexcept>
#include <string_view>
#include <cstring>
#include <iostream>

class LN {
public:

    explicit LN(long long num = 0);

    LN(const LN &a);
    LN(LN &&that) noexcept;
    LN &operator=(const LN &that);
    LN &operator=(LN &&that) noexcept;

    friend LN operator ""_ln(const char *s);
    explicit LN(const char *s);
    explicit LN(const std::string_view &s);
    explicit LN(bool b);

    ~LN();

    explicit operator long long() const;
    explicit operator bool() const;
    explicit operator std::string() const;

    friend std::ostream &operator<<(std::ostream &os, const LN &num);
    friend std::istream &operator>>(std::istream &is, LN &num);

    friend bool operator<(const LN &l, const LN &r);
    friend bool operator<=(const LN &l, const LN &r);
    friend bool operator>(const LN &l, const LN &r);
    friend bool operator>=(const LN &l, const LN &r);
    friend bool operator==(const LN &l, const LN &r);
    friend bool operator!=(const LN &l, const LN &r);

    friend LN operator+(const LN &l, const LN &r);
    friend LN operator-(const LN &l, const LN &r);
    friend LN operator*(const LN &l, const LN &r);
    friend LN operator/(const LN &l, const LN &r);
    friend LN operator%(const LN &l, const LN &r);
    friend LN operator-(const LN &that);

    LN &operator+=(const LN &that);
    LN &operator-=(const LN &that);
    LN &operator*=(const LN &that);
    LN &operator/=(const LN &that);
    LN &operator%=(const LN &that);
    LN &operator-();

    [[nodiscard]] LN sqrt() const;

    [[nodiscard]] bool isZero() const;
    [[nodiscard]] bool isUnsignedOne() const;

private:

    typedef int8_t digit_t;

    digit_t *digits_{};
    bool isNegative_{}, isNaN_{};

    size_t size_{}, capacity_{};

    void initVector(const size_t &requiredCapacity = 16);

    void push_back(const digit_t &item);
    void push_front(const digit_t &item);
    void resize(size_t newSize, const digit_t &x = 0);
    void ensureCapacity(const size_t &requiredCapacity);

    template<typename T>
    static void setOnRange(T *arr, const size_t &start, const size_t &end, const T &x);
    void removeLeadZeros();

    [[nodiscard]] int sign() const;
    [[nodiscard]] int compare(const LN &that) const;
    [[nodiscard]] int compareByAbs(const LN &that) const;

    void makeZero();

    void digit_sum(const LN &that);
    void digit_sub(const LN &src);
};

#endif //LABORATORNAYA_RABOTA_4_DLINNAYA_ARIFMETIKA_PECHHENKA_LN_H
