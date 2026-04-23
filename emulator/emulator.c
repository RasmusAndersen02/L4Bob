#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include "emulator.h"

ProgramState *new_state(ProgramState *prev_state, instruction input) {
  ProgramState *curr_state = malloc(sizeof(ProgramState));

  curr_state->program_counter = prev_state->program_counter;
  curr_state->br_register = prev_state->br_register;
  curr_state->direction_bit = prev_state->direction_bit;
  curr_state->memory = malloc(sizeof(uint16_t) * (1 << 16));

  memcpy(curr_state->memory, prev_state->memory, sizeof(uint16_t) * (1 << 16));

  curr_state->standard_registers = malloc(sizeof(uint16_t) * 16);
  memcpy(curr_state->standard_registers, prev_state->standard_registers,
         sizeof(uint16_t) * 16);

  op_code opcode = mask_and_shift(OP, input);
  uint16_t regd = mask_and_shift(REGd, input);
  uint16_t regs = mask_and_shift(REGs, input);
  arith_code arith = mask_and_shift(ARITH, input);
  int16_t offimm = mask_and_shift(OFFIMM, input);

  switch (opcode) {
  case ARITH_OP:
    arith_wrapper(curr_state, regd, regs, arith, curr_state->direction_bit);
    break;
  case ARITH_XORI:
    arith_xori(curr_state, regd, offimm);
    break;
  case ARITH_MUL2:
    arith_mul(curr_state, regd, curr_state->direction_bit);
    break;
  case ARITH_DIV2:
    arith_div(curr_state, regd, curr_state->direction_bit);
    break;
  case MEM_EXCH:
    mem_exchange(curr_state, regd, regs);
    break;
  case BGEZ:
    branch_bgez(curr_state, regd, offimm);
    break;
  case BLZ:
    branch_blz(curr_state, regd, offimm);
    break;
  case BEVN:
    branch_bevn(curr_state, regd, offimm);
    break;
  case BODD:
    branch_bodd(curr_state, regd, offimm);
    break;
  case BRA:
    branch_bra(curr_state, offimm);
    break;
  case RSWB:
    branch_rswb(curr_state, regd);
    break;
  case SWB:
    branch_swb(curr_state, regd);
    break;
  }
  if (curr_state->br_register != 0) {
    curr_state->program_counter +=
        curr_state->direction_bit * curr_state->br_register;
  } else {
    curr_state->program_counter += curr_state->direction_bit;
  }

  return curr_state;
}
ProgramState *init_state() {
  ProgramState *init = malloc(sizeof(ProgramState));
  init->memory = malloc(sizeof(uint16_t) * (1 << 16));
  init->program_counter = 1;
  init->br_register = 0;
  init->standard_registers = calloc(16, sizeof(uint16_t));
  init->direction_bit = 1;
  return init;
}
void print_states(ProgramState **state_array, size_t state_count) {
  for (size_t i = 0; i <= state_count; i++) {
    ProgramState *state = state_array[i];

    fprintf(stdout, "____State %zu - ", i);
    fprintf(stdout, "Program Counter: %u - ", state->program_counter);
    fprintf(stdout, "Branch Register: %u - ", state->br_register);
    fprintf(stdout, "Direction Bit: %d \n", state->direction_bit);
    for (size_t j = 0; j < 16; j++) {
      fprintf(stdout, "r%zu: %d | ", j, state->standard_registers[j]);
    }
    fprintf(stdout, "\n");
  }
}

ProgramState **runner(uint16_t *file_buf, uint16_t words_in_file,
                      uint16_t *total_states) {
  ProgramState **state_array = malloc(sizeof(ProgramState *) * 100);
  state_array[*total_states] = init_state();
  ProgramState *curr_state = state_array[*total_states];
  while (curr_state->program_counter <= words_in_file) {
    state_array[*total_states + 1] = new_state(
        state_array[*total_states], file_buf[curr_state->program_counter - 1]);
    curr_state = state_array[++*total_states];
    if (curr_state->direction_bit == -1 && curr_state->program_counter == 0) {
      break;
    }
  }
  return state_array;
}

int main(int argc, char *argv[]) {

  FILE *asm_file;
  asm_file = fopen(argv[1], "rb");
  if (ferror(asm_file) != 0) {
    fprintf(stderr, "error in reading asm file \n");
  }
  fseek(asm_file, 0L, SEEK_END);
  uint16_t instruction_count = ftell(asm_file) / 2;

  uint16_t *file_buffer = malloc(sizeof(uint16_t) * instruction_count);
  rewind(asm_file);
  fread(file_buffer, sizeof(uint16_t), instruction_count, asm_file);
  uint16_t total_states = 0;
  ProgramState **exec = runner(file_buffer, instruction_count, &total_states);
  print_states(exec, total_states);

  return EXIT_SUCCESS;
}
