#include <kaleidoscope/token.h>

namespace kaleidoscope {
std::string identitfier_str;
double num_val;

int GetToken() {
  static int last_char = ' ';

  while (isspace(last_char))
    last_char = getchar();
  if (isalpha(last_char)) {
    identitfier_str = last_char;
    while (isalnum(last_char = getchar()))
      identitfier_str += last_char;

    if (identitfier_str == "def")
      return TOK_DEF;
    if (identitfier_str == "extern")
      return TOK_EXTERN;
    if (identitfier_str == "if")
      return TOK_IF;
    if (identitfier_str == "then")
      return TOK_THEN;
    if (identitfier_str == "else")
      return TOK_ELSE;
    if (identitfier_str == "for")
      return TOK_FOR;
    if (identitfier_str == "in")
      return TOK_IN;
    if (identitfier_str == "binary")
      return TOK_BINARY;
    if (identitfier_str == "unary")
      return TOK_UNARY;
    if (identitfier_str == "var")
      return TOK_VAR;
    return TOK_IDENTIFIER;
  }

  if (isdigit(last_char)) {
    std::string num_str;

    do {
      num_str += last_char;
      last_char = getchar();
    } while (isdigit(last_char) || last_char == '.');

    num_val = strtod(num_str.c_str(), 0);
    return TOK_NUMBER;
  }

  if (last_char == '#') {
    last_char = getchar();
    while (last_char != EOF && last_char != '\n' && last_char != '\r')
      last_char = getchar();

    if (last_char != EOF)
      return GetToken();
  }

  if (last_char == EOF)
    return TOK_EOF;

  int this_char = last_char;
  last_char = getchar();
  return this_char;
}
} // namespace kaleidoscope