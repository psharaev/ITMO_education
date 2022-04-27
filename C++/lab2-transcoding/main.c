#include <stdio.h>
#include <stdint.h>

#define UTF8_WITHOUT_BOM 0
#define UTF8_WITH_BOM 1
#define UTF16LE 2
#define UTF16BE 3
#define UTF32LE 4
#define UTF32BE 5

#define MODE_LE 0
#define MODE_BE 1

void swap(int *a, int *b)
{
    int t = *a;
    *a = *b;
    *b = t;
}

int getSizeBOM(const int encode)
{
    switch (encode) {
    default:
    case UTF8_WITHOUT_BOM:
        return 0;
    case UTF8_WITH_BOM:
        return 3;
    case UTF16LE:
    case UTF16BE:
        return 2;
    case UTF32LE:
    case UTF32BE:
        return 4;
    }
}

int getEncoding(FILE *inp)
{
    unsigned char buff[4] = {fgetc(inp), fgetc(inp), fgetc(inp), fgetc(inp)};

    int res;
    if (buff[0] == 0xFF && buff[1] == 0xFE && buff[2] == 0x00 && buff[3] == 0x00) {
        res = UTF32LE;
    } else if (buff[0] == 0x00 && buff[1] == 0x00 && buff[2] == 0xFE && buff[3] == 0xFF) {
        res = UTF32BE;
    } else if (buff[0] == 0xFF && buff[1] == 0xFE) {
        res = UTF16LE;
    } else if (buff[0] == 0xFE && buff[1] == 0xFF) {
        res = UTF16BE;
    } else if (buff[0] == 0xEF && buff[1] == 0xBB && buff[2] == 0xBF) {
        res = UTF8_WITH_BOM;
    } else {
        res = UTF8_WITHOUT_BOM;
    }

    fseek(inp, getSizeBOM(res), SEEK_SET);

    return res;
}

void writeBOM(FILE *f, int encode)
{
    switch (encode) {
    case UTF32LE:
        fputc(0x00, f);
        fputc(0x00, f);
        fputc(0xFE, f);
        fputc(0xFF, f);
        break;
    case UTF32BE:
        fputc(0xFF, f);
        fputc(0xFE, f);
        fputc(0x00, f);
        fputc(0x00, f);
        break;
    case UTF16LE:
        fputc(0xFF, f);
        fputc(0xFE, f);
        break;
    case UTF16BE:
        fputc(0xFE, f);
        fputc(0xFF, f);
        break;
    case UTF8_WITH_BOM:
        fputc(0xEF, f);
        fputc(0xBB, f);
        fputc(0xBF, f);
        break;
    case UTF8_WITHOUT_BOM:
    default:
        break;
    }
}


uint32_t readUTF8(FILE *f, int *errorCode)
{
    int arr[4];
    uint32_t res = 0;
    *errorCode = 1;

    arr[0] = fgetc(f);
    if (arr[0] == -1) {
        return res;
    }

    if ((arr[0] & 0x80) == 0) { // один байт
        res = arr[0];
    } else if ((arr[0] & 0xE0) == 0xC0) { // два байта
        arr[1] = fgetc(f);
        res = ((arr[0] & (0x1F)) << 6) |
              (arr[1] & (0x3F));
    } else if ((arr[0] & 0xF0) == 0xE0) { // три байта
        arr[1] = fgetc(f);
        arr[2] = fgetc(f);
        res = ((arr[0] & (0x1F)) << 12) |
              ((arr[1] & (0x3F)) << 6) |
              (arr[2] & (0x3F));
    } else if ((arr[0] & 0xF8) == 0xF0) { // четыре байта
        arr[1] = fgetc(f);
        arr[2] = fgetc(f);
        arr[3] = fgetc(f);
        res = ((arr[0] & (0x1F)) << 18) |
              ((arr[1] & (0x3F)) << 12) |
              ((arr[2] & (0x3F)) << 6) |
              (arr[2] & (0x3F));
    }

    *errorCode = 0;
    return res;
}

void writeUTF8(FILE *f, const uint32_t symbol)
{
    union {
        uint32_t s;
        unsigned char b[4];
    } res;

    res.s = 0;
    if (symbol <= 0x7F) { // 1 байт
        fputc(symbol, f);
    } else if (symbol <= 0x7FF) { // 2 байта
        res.b[0] = (symbol >> 6) | 0xC0;
        res.b[1] = (symbol & 0x3f) | 0x80;
        fputc(res.b[0], f);
        fputc(res.b[1], f);
    } else if (symbol <= 0xFFFF) { // 3 байта
        res.b[0] = (symbol >> 12) | 0xE0;
        res.b[1] = ((symbol >> 6) & 0x3F) | 0x80;
        res.b[2] = (symbol & 0x3F) | 0x80;
        fputc(res.b[0], f);
        fputc(res.b[1], f);
        fputc(res.b[2], f);
    } else if (symbol <= 0x1FFFFF) { // 4 байта
        res.b[0] = (symbol >> 18) | 0xF0;
        res.b[1] = ((symbol >> 12) & 0x3F) | 0x80;
        res.b[2] = ((symbol >> 6) & 0x3F) | 0x80;
        res.b[3] = (symbol & 0x3F) | 0x80;
        fputc(res.b[0], f);
        fputc(res.b[1], f);
        fputc(res.b[2], f);
        fputc(res.b[3], f);
    }
}

uint32_t read2Bytes(FILE *f, const int mode, int *errorCode)
{
    int arr[2] = {fgetc(f), fgetc(f)};
    uint32_t res = 0;
    if (arr[1] == -1) {
        *errorCode = 1;
        return res;
    }

    if (mode == MODE_LE) {
        swap(&arr[0], &arr[1]);
    }

    res = (arr[0] << 8) | (arr[1]);
    *errorCode = 0;
    return res;
}

uint32_t read4Bytes(FILE *f, const int mode, int *errorCode)
{
    int arr[4] = {fgetc(f), fgetc(f), fgetc(f), fgetc(f)};
    uint32_t res = 0;
    if (arr[3] == -1) {
        *errorCode = 1;
        return res;
    }

    if (mode == MODE_LE) {
        swap(&arr[0], &arr[3]);
        swap(&arr[1], &arr[2]);
    }

    res = (arr[0] << 24) | (arr[1] << 16) | (arr[2] << 8) | (arr[3]);
    *errorCode = 0;
    return res;
}

void write2Bytes(FILE *f, const int bytes, const int mode)
{
    if (mode == MODE_LE) {
        fputc(bytes & 0xFF, f);
        fputc(bytes >> 8, f);
    } else {
        fputc(bytes >> 8, f);
        fputc(bytes & 0xFF, f);
    }
}

void write4Bytes(FILE *f, const int bytes, const int mode)
{
    if (mode == MODE_LE) {
        fputc(bytes & 0xFF, f);
        fputc((bytes >> 8) & 0xFF, f);
        fputc((bytes >> 16) & 0xFF, f);
        fputc((bytes >> 24) & 0xFF, f);
    } else {
        fputc((bytes >> 24) & 0xFF, f);
        fputc((bytes >> 16) & 0xFF, f);
        fputc((bytes >> 8) & 0xFF, f);
        fputc(bytes & 0xFF, f);
    }
}

uint32_t readUTF16(FILE *f, const int mode, int *errorCode)
{
    uint32_t res = 0;
    *errorCode = -1;

    int Leading = read2Bytes(f, mode, errorCode);
    if (*errorCode != 0) {
        return res;
    }

    if (Leading < 0xD800 || Leading > 0xDFFF) {
        res = Leading;
    } else if (Leading >= 0xDC00) {
        *errorCode = 1;
        return res;
    } else {
        res = (Leading & 0x3FF) << 10;

        int Tailing = read2Bytes(f, mode, errorCode);
        if (*errorCode != 0) {
            return res;
        }

        if (Tailing < 0xDC00 || Tailing > 0xDFFF) {
            *errorCode = 1;
            return res;
        } else {
            res = res | (Tailing & 0x3FF);
            res += 0x10000;
        }
    }

    *errorCode = 0;
    return res;
}

uint32_t readUTF16BE(FILE *f, int *errorCode)
{
    return readUTF16(f, MODE_BE, errorCode);
}

uint32_t readUTF16LE(FILE *f, int *errorCode)
{
    return readUTF16(f, MODE_LE, errorCode);
}

void writeUTF16(FILE *f, const uint32_t symbol, const int mode)
{
    if (symbol < 0x10000) {
        write2Bytes(f, (int) symbol & 0xFFFF, mode);
    } else {
        int t = (int) symbol - 0x10000;
        int low = (t & 0x3FF) & 0xFFFF;
        int high = (t >> 10) & 0xFFFF;
        write2Bytes(f, 0xD800 | high, mode);
        write2Bytes(f, 0xDC00 | low, mode);
    }
}

void writeUTF16LE(FILE *f, const uint32_t symbol)
{
    writeUTF16(f, symbol, MODE_LE);
}

void writeUTF16BE(FILE *f, const uint32_t symbol)
{
    writeUTF16(f, symbol, MODE_BE);
}

uint32_t readUTF32LE(FILE *f, int *errorCode)
{
    return read4Bytes(f, MODE_LE, errorCode);
}

uint32_t readUTF32BE(FILE *f, int *errorCode)
{
    return read4Bytes(f, MODE_BE, errorCode);
}

void writeUTF32LE(FILE *f, const uint32_t symbol)
{
    write4Bytes(f, symbol, MODE_LE);
}

void writeUTF32BE(FILE *f, const uint32_t symbol)
{
    write4Bytes(f, symbol, MODE_BE);
}

void copyRemainingBytes(FILE *in, FILE *out)
{
    int ch;
    while ((ch = fgetc(in)) != EOF)
        fputc(ch, out);
}


int main(int argc, char *argv[])
{
    if (argc != 4) {
        fprintf(stderr, "wrong count arguments");
        return 1;
    }

    int encodeOut = argv[3][0] - '0';
    if (encodeOut < 0 || 5 < encodeOut) {
        fprintf(stderr, "unknown output encoding");
        return 1;
    }

    FILE *inFile = fopen(argv[1], "rb");
    if (inFile == NULL) {
        fprintf(stderr, "error open read input file: %s", argv[1]);
        return 1;
    }

    FILE *outFile = fopen(argv[2], "wb");
    if (outFile == NULL) {
        fprintf(stderr, "error open write output file: %s", argv[2]);
        fclose(inFile);
        return 1;
    }

    int encodeIn = getEncoding(inFile);
    uint32_t (*readSymbol)(FILE *, int *);
    void (*writeSymbol)(FILE *, uint32_t);

    switch (encodeIn) {
    default:
    case UTF8_WITHOUT_BOM:
    case UTF8_WITH_BOM:
        readSymbol = readUTF8;
        break;
    case UTF16LE:
        readSymbol = readUTF16LE;
        break;
    case UTF16BE:
        readSymbol = readUTF16BE;
        break;
    case UTF32LE:
        readSymbol = readUTF32LE;
        break;
    case UTF32BE:
        readSymbol = readUTF32BE;
        break;
    }
    switch (encodeOut) {
    default:
    case UTF8_WITHOUT_BOM:
    case UTF8_WITH_BOM:
        writeSymbol = writeUTF8;
        break;
    case UTF16LE:
        writeSymbol = writeUTF16LE;
        break;
    case UTF16BE:
        writeSymbol = writeUTF16BE;
        break;
    case UTF32LE:
        writeSymbol = writeUTF32LE;
        break;
    case UTF32BE:
        writeSymbol = writeUTF32BE;
        break;
    }


    int errorCode = 0;
    writeBOM(outFile, encodeOut);

    if (encodeIn == encodeOut ||
        (encodeIn == UTF8_WITHOUT_BOM && encodeOut == UTF8_WITH_BOM) ||
        (encodeIn == UTF8_WITH_BOM && encodeOut == UTF8_WITHOUT_BOM)) {
        copyRemainingBytes(inFile, outFile);
    } else {
        while (1) {
            uint32_t symbol = readSymbol(inFile, &errorCode);
            if (ferror(inFile)) {
                fprintf(stderr, "error read input file: %s", argv[1]);
                fclose(inFile);
                fclose(outFile);
                return 2;
            }
            if (errorCode != 0) {
                break;
            }

            writeSymbol(outFile, symbol);
            if (ferror(outFile)) {
                fprintf(stderr, "error write output file: %s", argv[2]);
                fclose(inFile);
                fclose(outFile);
                return 2;
            }
        }
    }

    fclose(inFile);
    fclose(outFile);

    return 0;
}