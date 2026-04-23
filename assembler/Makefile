CC = gcc
CFLAGS = -Wall -g

# all: assembler

bob-ass: parser.tab.c lex.yy.c encoding.c
	$(CC) $(CFLAGS) -o assembler parser.tab.c lex.yy.c encoding.c

parser.tab.c parser.tab.h: parser.y
	bison -d parser.y

lex.yy.c: lexer.l parser.tab.h
	flex lexer.l

clean:
	rm -f assembler parser.tab.c parser.tab.h lex.yy.c
