#include "bootstrap.h"

typedef struct token token;

typedef struct str2 str2;

int main();

token make_token(str _0, str _1);

str2 make_str2(str _0, str _1);

bool is_letter(char ch);

bool is_symbol(char ch);

bool first_char_is_letter(str string);

bool first_char_is_symbol(str string);

str2 get_first_token(str string);

struct token { str _0; str _1; };

struct str2 { str _0; str _1; };

int main() {
    str file_name = make_str("rewrite/main.bs");
    str text = std_str_strip(std_io_read(file_name));
    array_str lines = std_str_split_lines(text);
    array_str array_var_0 = make_array_str(1);
    array_var_0.data[0] = make_str("");
    array_str tokens = array_var_0;
    i32 i = 0;
    for (0; (i < std_arr_len(lines)); i++) {
        str line = std_str_strip(std_arr_get(lines, i));
        while (gt(std_str_len(line), 0)) {
            int match_out_4;
            str token = get_first_token(line)._0;
            str rest = get_first_token(line)._1;
            if (((1 && 1) && 1)) {
                line = rest;
                std_io_print(make_str("next token is: "));
                std_io_println(token);
                array_str array_var_1 = make_array_str(1);
                array_var_1.data[0] = token;
                array_str plus_ret_2 = make_array_str((tokens.length + array_var_1.length));
                i32 i_3 = 0;
                for (0; (i_3 < tokens.length); i_3++) {
                    plus_ret_2.data[i_3] = tokens.data[i_3];
                }
                for (0; (i_3 < (tokens.length + array_var_1.length)); i_3++) {
                    plus_ret_2.data[i_3] = array_var_1.data[i_3];
                }
                tokens = plus_ret_2;
                match_out_4 = 0;
            } else {
            }
            match_out_4;
        }
    }
    i32 j = 0;
    while (std_str_eq(tokens.data[0], make_str(""))) {
        j = (j + 1);
    }
    if (std_str_eq(tokens.data[0], make_str("import"))) {
        std_io_println(make_str("Found import!"));
    }
    return 0;
}

token make_token(str _0, str _1) {
    token out;
    out._0 = _0;
    out._1 = _1;
    return out;
}

str2 make_str2(str _0, str _1) {
    str2 out;
    out._0 = _0;
    out._1 = _1;
    return out;
}

bool is_letter(char ch) {
    i32 i = std_char_to_i32(ch);
    i32 a = std_char_to_i32('a');
    i32 z = std_char_to_i32('z');
    i32 A = std_char_to_i32('A');
    i32 Z = std_char_to_i32('Z');
    bool is_lower = and(lte(a, i), lte(i, z));
    bool is_upper = and(lte(A, i), lte(i, Z));
    bool is_underscore = std_char_eq(ch, '_');
    return or(or(is_lower, is_upper), is_underscore);
}

bool is_symbol(char ch) {
    return bootstrap_not(or(is_letter(ch), std_char_is_whitespace(ch)));
}

bool first_char_is_letter(str string) {
    return and(gt(std_str_len(string), 0), is_letter(std_str_get(string, 0)));
}

bool first_char_is_symbol(str string) {
    return and(gt(std_str_len(string), 0), is_symbol(std_str_get(string, 0)));
}

str2 get_first_token(str string) {
    string = std_str_strip(string);
    i32 len = std_str_len(string);
    str ret = make_str("");
    if (gt(std_str_len(string), 0)) {
        char ch = std_str_get(string, 0);
        if (is_symbol(ch)) {
            while (first_char_is_symbol(string)) {
                char ch = std_str_get(string, 0);
                str ch_str = std_str_from_char(ch);
                ret = std_str_concat(ret, ch_str);
                string = std_str_substr(string, 1, len);
            }
        } else {
            while (first_char_is_letter(string)) {
                char ch = std_str_get(string, 0);
                str ch_str = std_str_from_char(ch);
                ret = std_str_concat(ret, ch_str);
                string = std_str_substr(string, 1, len);
            }
        }
    }
    return make_str2(ret, string);
}

