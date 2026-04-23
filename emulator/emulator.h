#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#define MEM_SIZE = 65526;
// required for backward branch jumps otherwise, funny casting errors

typedef uint16_t word;
typedef uint16_t instruction;

typedef struct ProgramState {
  int16_t *memory;
  int16_t *standard_registers;
  int16_t br_register;
  uint16_t program_counter;
  int8_t direction_bit;
} ProgramState;

typedef enum {
  ADD = 4,
  SUB = 13,
  ADD1 = 6,
  SUB1 = 15,
  NEG = 7,
  XOR = 0
} arith_code;
typedef enum {
  OP = 0xf000,
  REGd = 0x0f00,
  REGs = 0x00f0,
  ARITH = 0x000f,
  OFFIMM = 0x00ff,
} mask;
typedef enum {
  ARITH_OP = 12,
  ARITH_XORI = 0,
  ARITH_MUL2 = 10,
  ARITH_DIV2 = 9,
  MEM_EXCH = 8,
  BGEZ = 3,
  BLZ = 2,
  BEVN = 5,
  BODD = 4,
  BRA = 1,
  RSWB = 7,
  SWB = 6
} op_code;

uint16_t mask_and_shift(mask m, instruction inst);
void arith_uni(ProgramState *curr, uint16_t regd, arith_code arith, int8_t dir);
void arith_bin(ProgramState *curr, uint16_t regd, uint16_t regs,
               arith_code arith, int8_t dir);
void arith_wrapper(ProgramState *curr, uint16_t regd, uint16_t regs,
                   arith_code arith, int8_t dir);
void arith_mul(ProgramState *curr, uint16_t regd, int8_t dir);
void arith_div(ProgramState *curr, uint16_t regd, int8_t dir);
void arith_xori(ProgramState *curr, uint16_t regd, uint16_t immidiate);

void mem_exchange(ProgramState *curr, uint16_t regd, uint16_t regs);

void branch_bgez(ProgramState *curr, uint16_t regd, uint16_t offset);
void branch_blz(ProgramState *curr, uint16_t regd, int16_t offset);
void branch_bevn(ProgramState *curr, uint16_t regd, uint16_t offset);
void branch_bodd(ProgramState *curr, uint16_t regd, uint16_t offset);
void branch_bra(ProgramState *curr, uint16_t offset);
void branch_rswb(ProgramState *curr, uint16_t regd);
void branch_swb(ProgramState *curr, uint16_t regd);
