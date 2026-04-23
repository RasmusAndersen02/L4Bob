CC = gcc
CFLAGS = -Wall -g -O0 -Wextra

emulator: emulator.o update_state.o
	$(CC) $(CFLAGS) emulator.o update_state.o -o emulator 

emulator.o: emulator.c emulator.h
	$(CC) $(CFLAGS) -c  emulator.c

update_state.o : update_state.c emulator.h
	$(CC) $(CFLAGS) -c  update_state.c

clean:
	rm -f emulator emulator.o update_state.o
