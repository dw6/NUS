FILENAMES = AND LDCI RTN CALL GOTO LDF GREATER LDRF TIMES INSTRUCTION LESS MINUS DIV NOT DONE JOF OPCODES EQUAL LD OR LDCB PLUS TAILCALL START

CLASSFILES = $(FILENAMES:%=%.class)

all: $(CLASSFILES)

%.class: %.java
	javac $?

clean:
	rm *.class *~
