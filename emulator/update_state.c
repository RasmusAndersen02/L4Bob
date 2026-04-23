#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "emulator.h"

uint16_t mask_and_shift(mask m, instruction inst) {
  switch (m) {
  case OP:
    return (inst & OP) >> 12;
  case REGd:
    return (inst & REGd) >> 8;
  case REGs:
    return (inst & REGs) >> 4;
  case ARITH:
    return (inst & ARITH);
  case OFFIMM:
    return (inst & OFFIMM);
  default:
    fprintf(stderr, "mask issue, enum: %d", m);
  }
}

void arith_uni(ProgramState *curr, uint16_t regd, arith_code arith,
               int8_t dir) {

  switch (arith) {
  case ADD1:
    curr->standard_registers[regd] += dir;
    break;
  case SUB1:
    curr->standard_registers[regd] -= dir;
    break;
  case NEG:
    curr->standard_registers[regd] = -curr->standard_registers[regd];
    break;
  default:
    fprintf(stderr, "didnt hit unary enum: %d", arith);
    break;
  }
}

void arith_bin(ProgramState *curr, uint16_t regd, uint16_t regs,
               arith_code arith, int8_t dir) {
  switch (arith) {
  case ADD:
    curr->standard_registers[regd] += dir * curr->standard_registers[regs];
    break;
  case SUB:
    curr->standard_registers[regd] -= dir * curr->standard_registers[regs];
    break;
  case XOR:
    curr->standard_registers[regd] ^= curr->standard_registers[regs];
    break;
  default:
    fprintf(stderr, "didnt hit binary enum: %d", arith);
    break;
  }
}

void arith_wrapper(ProgramState *curr, uint16_t regd, uint16_t regs,
                   arith_code arith, int8_t dir) {
  if (regs == 0) {
    arith_uni(curr, regd, arith, dir);
  } else {
    arith_bin(curr, regd, regs, arith, dir);
  }
}

void arith_xori(ProgramState *curr, uint16_t regd, uint16_t immediate) {
  curr->standard_registers[regd] ^= immediate;
}
// Kan bruge BGEZ / BLZ for bounds checking af regd.
void arith_mul(ProgramState *curr, uint16_t regd, int8_t dir) {
  // Shoutout ChatiGippity
  if (dir == -1) {
    arith_div(curr, regd, -dir);
  } else {
    uint16_t temp = (uint16_t)curr->standard_registers[regd];
    uint16_t res = (temp << 1) | (temp >> 15);
    curr->standard_registers[regd] = (int16_t)res;
  }
}
void arith_div(ProgramState *curr, uint16_t regd, int8_t dir) {
  if (dir == -1) {
    arith_mul(curr, regd, -dir);
  } else {
    uint16_t temp = (uint16_t)curr->standard_registers[regd];
    uint16_t res = (temp >> 1) | (temp << 15);
    curr->standard_registers[regd] = (int16_t)temp;
  }
}
void mem_exchange(ProgramState *curr, uint16_t regd, uint16_t rega) {
  curr->standard_registers[regd] ^= curr->standard_registers[rega];
  curr->standard_registers[rega] ^= curr->standard_registers[regd];
  curr->standard_registers[regd] ^= curr->standard_registers[rega];
}
void branch_bgez(ProgramState *curr, uint16_t regd, uint16_t offset) {
  if (curr->standard_registers[regd] >= 0) {
    curr->br_register += offset;
  }
}
void branch_blz(ProgramState *curr, uint16_t regd, int16_t offset) {
  if (curr->standard_registers[regd] < 0) {
    curr->br_register += offset;
  }
}
void branch_bevn(ProgramState *curr, uint16_t regd, uint16_t offset) {
  if (curr->standard_registers[regd] % 2 == 0) {
    curr->br_register += offset;
  }
}
void branch_bodd(ProgramState *curr, uint16_t regd, uint16_t offset) {
  if (abs(curr->standard_registers[regd] % 2) == 1) {
    curr->br_register += offset;
  }
}
void branch_bra(ProgramState *curr, uint16_t offset) {
  curr->br_register += offset;
}
void branch_rswb(ProgramState *curr, uint16_t regd) {
  curr->standard_registers[regd] ^= curr->br_register;
  curr->br_register ^= curr->standard_registers[regd];
  curr->standard_registers[regd] ^= curr->br_register;
  curr->direction_bit = -curr->direction_bit;
}
void branch_swb(ProgramState *curr, uint16_t regd) {
  curr->standard_registers[regd] ^= curr->br_register;
  curr->br_register ^= curr->standard_registers[regd];
  curr->standard_registers[regd] ^= curr->br_register;
}
