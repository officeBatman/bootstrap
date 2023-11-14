#include "bootstrap.h"

i32 n_1;

int main();

int fizz_buzz_0(i32 n);

int main() {
    print(make_str("How much fizz buzz do you want? "));
    n_1 = input_i32(0);
    fizz_buzz_0(n_1);
    return 0;
}

int fizz_buzz_0(i32 n) {
    i32 i_2 = 1;
    for (0; (i_2 < (n + 1)); i_2++) {
        if ((mod(i_2, 15) == 0)) {
            println(make_str("Fizzbuzz"));
        } else {
            if ((mod(i_2, 3) == 0)) {
                println(make_str("Fizz"));
            } else {
                if ((mod(i_2, 5) == 0)) {
                    println(make_str("Buzz"));
                } else {
                    print_i32(i_2);
                    println(make_str(""));
                }
            }
        }
    }
    return 1;
}

