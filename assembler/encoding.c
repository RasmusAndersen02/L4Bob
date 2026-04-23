#include "encoding.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void add_sym(char *name, uint16_t address) {
  // sym *current = lookup_list;

  sym *new_sym = malloc(sizeof(sym));
  // maybe mem error
  new_sym->name = strdup(name + 1);
  new_sym->name[strlen(new_sym->name) - 1] = '\0';
  new_sym->address = address;
  // smart...xd
  new_sym->next = lookup_list;
  lookup_list = new_sym;
}
uint16_t lookup_sym(char *name) {
  sym *current = lookup_list;
  while (current) {
    if (strcmp(current->name, name) == 0) {
      return current->address;
    }
    current = current->next;
  }
  fprintf(stderr, "the identifier is not in symtable");
  return 1;
}
uint16_t arith_uni(opcode op, uint16_t regd) {
  uint16_t encoding = 0x0c << 12;
  encoding |= regd << 8;
  switch (op) {
  case OP_ADD1:
    encoding |= 0x06;
    break;
  case OP_SUB1:
    encoding |= 0x0f;
    break;
  case OP_NEG:
    encoding |= 0x07;
    break;
  default:
    fprintf(stderr, "Wrong helper");
  }
  return (encoding);
}
uint16_t arith_bin(opcode op, uint16_t regd, uint16_t regs) {
  uint16_t encoding = 0x0c << 12;
  encoding |= regd << 8;
  encoding |= regs << 4;
  switch (op) {
  case OP_ADD:
    encoding |= 0x04;
    break;
  case OP_SUB:
    encoding |= 0x0d;
    break;
  case OP_XOR:
    encoding |= 0x00;
    break;
  default:
    fprintf(stderr, "Wrong helper");
  }
  return (encoding);
}
uint16_t arith_na(opcode op, uint16_t regd) {
  uint16_t encoding = 0x00;
  encoding |= regd << 8;
  switch (op) {
  case OP_MUL2:
    encoding |= 0x0a << 12;
    break;
  case OP_DIV2:
    encoding |= 0x09 << 12;
    break;
  default:
    fprintf(stderr, "Wrong helper");
  }
  return (encoding);
}
uint16_t arith_xori(opcode op, uint16_t regd, uint16_t imm) {
  uint16_t encoding = 0x00;
  encoding |= regd << 8;
  encoding |= imm;
  return (encoding);
}
uint16_t mem_exchange(opcode op, uint16_t regd, uint16_t rega) {
  uint16_t encoding = 0x08 << 12;
  encoding |= regd << 8;
  encoding |= rega << 4;
  return (encoding);
}
uint16_t branch_regoff(opcode op, uint16_t regd, uint16_t offset) {
  uint16_t encoding = 0x00;
  encoding |= regd << 8;
  encoding |= (offset & 0xFF);
  switch (op) {
  case OP_BGEZ:
    encoding |= 0x03 << 12;
    break;
  case OP_BLZ:
    encoding |= 0x02 << 12;
    break;
  case OP_BEVN:
    encoding |= 0x05 << 12;
    break;
  case OP_BODD:
    encoding |= 0x04 << 12;
    break;
  default:
    fprintf(stderr, "Wrong helper");
  }
  return (encoding);
}
uint16_t branch_off(opcode op, uint16_t offset) {
  uint16_t encoding = 0x01 << 12;
  encoding |= offset;
  return (encoding);
}
uint16_t branch_reg(opcode op, uint16_t regd) {
  uint16_t encoding = 0x00;
  encoding |= regd << 8;
  switch (op) {
  case OP_SWB:
    encoding |= 0x06 << 12;
    break;
  case OP_RSWB:
    encoding |= 0x07 << 12;
    break;
  default:
    fprintf(stderr, "Wrong helper");
  }
  return (encoding);
}

bool write_to_bin(uint16_t bin, FILE *file) {
  if (!fwrite(&bin, sizeof(uint16_t), 1, file)) {
    return false;
  }
  return true;
}
bool in_range(int16_t offimm) {
  if (offimm < -127 || offimm > 128) {
    return false;
  } else {
    return true;
  }
}
