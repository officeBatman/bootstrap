#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

typedef int32_t bootstrap_i32;
typedef bool bootstrap_bool;
typedef char bootstrap_char;

typedef struct str_head {
    uint32_t rc; 
    // After the rc field - a bunch of chars as the content.
} str_head;

typedef struct str {
    size_t length;
    str_head* head_ptr;
} str;

typedef str bootstrap_str;

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

static inline char bootstrap_std_str_get(str string, int32_t index) {
    if (index < 0 || index >= string.length) {
        // TODO: Error handling.
        return '\0';
    }
    return str_c_string(string)[index];
}

static inline int32_t bootstrap_std_str_len(str string) {
    return string.length;
}

static inline str bootstrap_std_str_substr(str string, int32_t start, int32_t end) {
    str out = make_str_len(end - start);
    memcpy(str_c_string(out), str_c_string(string) + start, end - start);
    str_c_string(out)[end - start] = '\0';
    return out;
}

static inline bool bootstrap_std_str_eq(str a, str b) {
    return a.length == b.length && memcmp(str_c_string(a), str_c_string(b), a.length) == 0;
}

static inline void bootstrap_std_io_print(str string) {
    printf("%s\n", str_c_string(string));
}

static inline void bootstrap_std_io_print_i32(int32_t i32) {
    printf("%d\n", i32);
}

static inline void bootstrap_std_io_print_bool(bool b) {
    printf("%s\n", b ? "true" : "false");
}

static inline str bootstrap_std_io_input(int) {
    char buffer[1024];
    fgets(buffer, 1024, stdin);
    if (buffer[strlen(buffer) - 1] == '\n')
        buffer[strlen(buffer) - 1] = '\0';
    return make_str(buffer);
}

static inline str bootstrap_std_io_read(str filename) {
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

typedef struct bootstrap_array_str {
    size_t length;
    str* data;
} bootstrap_array_str;

static inline bootstrap_array_str bootstrap_make_array_str(int32_t len) {
    bootstrap_array_str arr;
    arr.length = len;
    arr.data = (str*)malloc(sizeof(str) * len);
    return arr;
}

static inline int32_t bootstrap_std_arr_len(bootstrap_array_str arr) {
    return arr.length;
}

static inline str bootstrap_std_arr_get(bootstrap_array_str arr, int32_t i) {
    if (i < 0 || i >= arr.length) {
        // TODO: Error handling.
        return make_str("");
    }
    return arr.data[i];
}

static inline bootstrap_array_str bootstrap_std_arr_append(bootstrap_array_str arr, str item) {
    int32_t len = arr.length;
    str* data = (str*)malloc(sizeof(str) * (len + 1));
    memcpy(data, arr.data, sizeof(str) * len);
    data[len] = item;
    bootstrap_array_str out = {(size_t)len + 1, data};
    return out;
}

static inline bootstrap_array_str bootstrap_std_str_split(str string, str sep) {
    bootstrap_array_str ret = {0, NULL};
    int32_t len = string.length;
    int32_t sep_len = sep.length;

    size_t start = 0;
    size_t end = 0;
    while (start < len && end < len) {
        if (memcmp(str_c_string(string) + end, str_c_string(sep), sep_len) == 0) {
            str item = bootstrap_std_str_substr(string, start, end);
            bootstrap_array_str appendend = bootstrap_std_arr_append(ret, item);
            free(ret.data);
            ret = appendend;
            start = end + sep_len;
            end = start;
        } else {
            end++;
        }
    }
    if (start < len) {
        str item = bootstrap_std_str_substr(string, start, len);
        bootstrap_array_str appendend = bootstrap_std_arr_append(ret, item);
        free(ret.data);
        ret = appendend;
    }
    return ret;
}

static inline bootstrap_array_str bootstrap_std_str_split_lines(str string) {
    return bootstrap_std_str_split(string, make_str("\n"));
}

static inline str bootstrap_std_str_from_char(char ch) {
    str out = make_str_len(1);
    str_c_string(out)[0] = ch;
    return out;
}

static inline bool bootstrap_not(bool b) {
    return !b;
}

static inline str bootstrap_std_str_strip(str string) {
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
    return bootstrap_std_str_substr(string, start, end + 1);
}

static inline bool bootstrap_gt(bootstrap_i32 a, bootstrap_i32 b) {
    return a > b;
}
