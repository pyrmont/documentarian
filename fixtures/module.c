#include <stdio.h>
#include <string.h>

static const JanetReg cfuns[] = {
    {"my-func", cfun_my_func,
     "(my-module/my-fun)\n\n"
     "This is a function that does nothing "
     "but act as an example."
    },
    {NULL, NULL, NULL}
};

void markable_register_converter(JanetTable *env) {
    janet_cfuns(env, "my-module", cfuns);
}
