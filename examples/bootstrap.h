#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

typedef int32_t i32;

typedef struct str_head {
    uint32_t rc; 
    // After the rc field - a bunch of chars as the content.
} str_head;

typedef struct str {
    size_t length;
    str_head* head_ptr;
} str;

static inline char* str_c_string(str string) {
    return (char*)(string.head_ptr + 1);
}

static inline str make_str_len(size_t len) {
    str_head* head_ptr = (str_head*)malloc(sizeof(str_head) + len + 1);
    head_ptr->rc = 1;
    str out = {len, head_ptr};
    str_c_string(out)[len] = '\0';
    return out;
}

static inline str make_str(const char* c_str) {
    size_t len = strlen(c_str);
    str out = make_str_len(len);
    memcpy(str_c_string(out), c_str, len);
    return out;
}

static inline char str_get(str string, int32_t index) {
    if (index < 0 || index >= string.length) {
        // TODO: Error handling.
        return '\0';
    }
    return str_c_string(string)[index];
}

static inline int32_t str_len(str string) {
    return string.length;
}

static inline str substr(str string, int32_t start, int32_t end) {
    start = start < 0 ? 0 : start;
    end = end > string.length ? string.length : end < start ? start : end;
    str out = make_str_len(end - start);
    memcpy(str_c_string(out), str_c_string(string) + start, end - start);
    str_c_string(out)[end - start] = '\0';
    return out;
}

static inline bool str_eq(str a, str b) {
    return a.length == b.length && memcmp(str_c_string(a), str_c_string(b), a.length) == 0;
}

static inline int print(str string) {
    printf("%s", str_c_string(string));
    return 0;
}

static inline int println(str string) {
    printf("%s\n", str_c_string(string));
    return 0;
}

static inline int print_i32(int32_t i32) {
    printf("%d", i32);
    return 0;
}

static inline int print_bool(bool b) {
    printf("%s", b ? "true" : "false");
    return 0;
}

static inline str input(int) {
    char buffer[1024];
    fgets(buffer, 1024, stdin);
    if (buffer[strlen(buffer) - 1] == '\n')
        buffer[strlen(buffer) - 1] = '\0';
    return make_str(buffer);
}

static inline i32 input_i32(int) {
    int i = 0;
    scanf("%d", &i);
    return (i32)i;
}

static inline str read(str filename) {
    FILE* file = fopen(str_c_string(filename), "rb");
    if (file == NULL) {
        return make_str("");
    }
    // Get the length of the file.
    fseek(file, 0, SEEK_END);
    size_t len = ftell(file);
    fseek(file, 0, SEEK_SET);
    // Allocate a string with the length of the file.
    str_head* head_ptr = (str_head*)malloc(sizeof(str_head) + len + 1);
    head_ptr->rc = 1;
    // Read the file into the string.
    char* content = (char*)(head_ptr + 1);
    fread(content, 1, len, file);
    content[len] = '\0';
    // Done!
    fclose(file);
    str out = {len, head_ptr};
    return out;
}

typedef struct array_str {
    size_t length;
    str* data;
} array_str;

static inline array_str make_array_str(int32_t len) {
    array_str arr;
    arr.length = len;
    arr.data = (str*)malloc(sizeof(str) * len);
    return arr;
}

static inline int32_t str_arr_len(array_str arr) {
    return arr.length;
}

static inline array_str str_arr_append(array_str arr, str item) {
    int32_t len = arr.length;
    str* data = (str*)malloc(sizeof(str) * (len + 1));
    memcpy(data, arr.data, sizeof(str) * len);
    data[len] = item;
    array_str out = {(size_t)len + 1, data};
    return out;
}

static inline array_str split(str string, str sep) {
    array_str ret = {0, NULL};
    int32_t len = string.length;
    int32_t sep_len = sep.length;

    size_t start = 0;
    size_t end = 0;
    while (start < len && end < len) {
        if (memcmp(str_c_string(string) + end, str_c_string(sep), sep_len) == 0) {
            str item = substr(string, start, end);
            array_str appendend = str_arr_append(ret, item);
            free(ret.data);
            ret = appendend;
            start = end + sep_len;
            end = start;
        } else {
            end++;
        }
    }
    if (start < len) {
        str item = substr(string, start, len);
        array_str appendend = str_arr_append(ret, item);
        free(ret.data);
        ret = appendend;
    }
    return ret;
}

static inline array_str split_lines(str string) {
    return split(string, make_str("\n"));
}

static inline str str_from_char(char ch) {
    str out = make_str_len(1);
    str_c_string(out)[0] = ch;
    return out;
}

static inline bool bootstrap_not(bool b) {
    return !b;
}

static inline str strip(str string) {
    int32_t start = 0;
    int32_t end = string.length - 1;
    if (end < 0) {
        return string;
    }
    while (start <= end && isspace(str_c_string(string)[start])) {
        start++;
    }
    while (end > start && isspace(str_c_string(string)[end])) {
        end--;
    }
    return substr(string, start, end + 1);
}

static inline bool gt(i32 a, i32 b) {
    return a > b;
}

static inline bool gte(i32 a, i32 b) {
    return a >= b;
}

static inline bool lt(i32 a, i32 b) {
    return a < b;
}

static inline bool lte(i32 a, i32 b) {
    return a <= b;
}

static inline bool is_whitespace(char ch) {
    return isspace(ch);
}

static inline str concat(str a, str b) {
    str out = make_str_len(a.length + b.length);
    memcpy(str_c_string(out), str_c_string(a), a.length);
    // +1 to copy the null terminator
    memcpy(str_c_string(out) + a.length, str_c_string(b), b.length + 1);
    return out;
}

/*
static inline bool and(bool a, bool b) {
    return a && b;
}
*/

#define and(a, b) ((a) && (b))

#define or(a, b) ((a) || (b))

static inline i32 char_to_i32(char ch) {
    return ch;
}

static inline char char_from_i32(i32 i) {
    return i;
}

static inline bool char_eq(char a, char b) {
    return a == b;
}

static inline i32 mod(i32 a, i32 b) {
    return a % b;
}
