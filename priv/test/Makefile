
LEVELDB_DIR=../../c_src/leveldb
SNAPPY_DIR=../../c_src/snappy

CC=gcc
CFLAGS=-Wall -I $(LEVELDB_DIR)/include
LDFLAGS=$(LEVELDB_DIR)/lib/libleveldb.a $(SNAPPY_DIR)/lib/libsnappy.a -lstdc++ -lpthread

TEST=qc_leveldb_issue44

HEADERS=qc_statemc_lets.h
SOURCES=qc_statemc_lets.c $(TEST:=.c)
OBJECTS=$(SOURCES:.c=.o)

.PHONY: clean

all: $(SOURCES) $(TEST)

$(TEST): $(OBJECTS) $(HEADERS)
	$(CC) $(OBJECTS) $(LDFLAGS) -o $@

.c.o:
	$(CC) -c $(CFLAGS) $< -o $@

clean:
	rm -rf *.o $(EXECUTABLE) qc_leveldb
