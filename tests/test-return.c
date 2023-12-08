#include <stdbool.h>

int test_arg;

signed char
int_to_signed_char()
{
  return (signed char) test_arg;
}

short
int_to_short()
{
  return (short) test_arg;
}

int
int_to_int()
{
  return (int) test_arg;
}

unsigned char
int_to_unsigned_char()
{
  return (unsigned char) test_arg;
}

unsigned short
int_to_unsigned_short()
{
  return (unsigned short) test_arg;
}

unsigned int
int_to_unsigned_int()
{
  return (unsigned int) test_arg;
}

_Bool int_to_bool()
{
  return (_Bool) test_arg;
}

