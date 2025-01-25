#pragma once

#include <string>
#include <iostream>

namespace kaleidoscope
{
enum Token
{
    TOK_EOF = -1,
    TOK_DEF = -2,
    TOK_EXTERN = -3,
    TOK_IDENTIFIER = -4,
    TOK_NUMBER = -5,
    TOK_IF = -6,
    TOK_THEN = -7,
    TOK_ELSE = -8,
};

extern std::string identitfier_str;
extern double num_val;

int GetToken();
}