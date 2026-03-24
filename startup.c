#include <stdio.h>

#define fixnum_mask 3
#define fixnum_tag 0
#define fixnum_shift 2

#define char_tag 0x0f
#define char_shift 8
#define char_mask 0xff

extern int scheme_entry(void);

int main(int argc, char *argv[]) {
  int val = scheme_entry();
  if ((val & fixnum_mask) == fixnum_tag) {
    printf("%d\n", val >> fixnum_shift);
  } else if ((val & char_mask) == char_tag) {
    printf("%c\n", val >> char_shift);
  } else {
    puts("()");
  }
  return 0;
}
