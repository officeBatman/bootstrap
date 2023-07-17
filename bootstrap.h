#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdbool.h>
#include <string.h>

typedef struct str_head {
    uint32_t rc; 
    // After the rc field - a bunch of chars as the content.
} str_head;

typedef struct str {
    size_t length;
    str_head* head_ptr;
} str;

static inline str make_str(const char* c_str) {
    size_t len = strlen(c_str);
    str_head* head_ptr = (str_head*)malloc(sizeof(str_head) + len + 1);
    head_ptr->rc = 1;
    memcpy(head_ptr + 1, c_str, len + 1);
    str out = {len, head_ptr};
    return out;
}

static inline const char* str_c_string(str string) {
    return (const char*)(string.head_ptr + 1);
}

static inline void bootstrap_std_io_print(str string) {
    printf("%s\n", str_c_string(string));
}

static bool bootstrap_true = true;
static bool bootstrap_false = false;

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
