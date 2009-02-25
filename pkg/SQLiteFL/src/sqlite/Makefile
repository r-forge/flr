include $(MkInclude)

PKG_CFLAGS=-DTHREADSAFE=0 -DSQLITE_DEFAULT_FILE_FORMAT=4

library: lib

OBJ = sqlite3.o

LIB = ../sqlite.a

lib: $(LIB)

$(LIB): $(OBJ)
	$(AR) -rucs $(LIB) $(OBJ)

clean:
	@-rm -f $(LIB)
	@-rm -f $(OBJ)
