
all: assembler emulator

assembler:
	$(MAKE) -C assembler
emulator:
	$(MAKE) -C emulator
# Clean all
clean:
	$(MAKE) -C assembler clean
	$(MAKE) -C emulator clean

.PHONY: all assembler emulator run clean
