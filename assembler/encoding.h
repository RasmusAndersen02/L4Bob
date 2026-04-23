#ifndef ENCODING_H
#define ENCODING_H
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

typedef struct sym {
  char *name;
  uint16_t address;
  struct sym *next;
} sym;
extern sym *lookup_list;

typedef enum {
  OP_ADD,
  OP_SUB,
  OP_XOR,
  OP_ADD1,
  OP_SUB1,
  OP_NEG,
  OP_XORI,
  OP_MUL2,
  OP_DIV2,
  OP_BGEZ,
  OP_BLZ,
  OP_BEVN,
  OP_BODD,
  OP_BRA,
  OP_SWB,
  OP_RSWB,
  OP_EXCH
} opcode;

uint16_t arith_uni(opcode op, uint16_t regd);
uint16_t arith_bin(opcode op, uint16_t regd, uint16_t regs);
uint16_t arith_na(opcode op, uint16_t regd);
uint16_t arith_xori(opcode op, uint16_t regd, uint16_t imm);

uint16_t mem_exchange(opcode op, uint16_t regd, uint16_t rega);
uint16_t branch_regoff(opcode op, uint16_t regd, uint16_t offset);
uint16_t branch_off(opcode op, uint16_t offset);
uint16_t branch_reg(opcode op, uint16_t regd);

void add_sym(char *name, uint16_t address);
uint16_t lookup_sym(char *name);

bool write_to_bin(uint16_t bin, FILE *file);
bool in_range(int16_t offimm);

#endif
