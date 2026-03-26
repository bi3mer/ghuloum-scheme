#include <stdio.h>

#define fixnum_mask 3
#define fixnum_tag 0
#define fixnum_shift 2

#define char_tag 0x0f
#define char_shift 8
#define char_mask 0xff

#define bool_tag 0x1f
#define bool_shift 7
#define bool_mask 0x7f

#define null 0x2F

extern int scheme_entry(void);

int main(int argc, char *argv[]) {
  int val = scheme_entry();
  if ((val & fixnum_mask) == fixnum_tag) {
    printf("%d\n", val >> fixnum_shift);
  } else if ((val & char_mask) == char_tag) {
    printf("%c\n", val >> char_shift);
  } else if ((val & bool_mask) == bool_tag) {
    printf("%s\n", (val >> bool_shift) & 1 ? "#t" : "#f");
  } else if (val == null) {
    printf("()\n");
  } else {
    puts("()\n");
  }

  return 0;
}
