.SUFFIXES:
.SUFFIXES: .o .cpp .h .d .l .y
.PHONY : clean

LIB     = libwrapper.a

TARGET  = $(LIB)

#CFLAGS = -O3 -g -ansi -pedantic -Wall -frounding-math -fno-fast-math $(INCLUDES)
CFLAGS  = -O3 -g -fPIC -I. -stdlib=libc++

CPP     = g++
MV      = mv
SED		= sed
AR      = ar
RANLIB  = ranlib
RM      = rm
LN      = lnP

SRC     = wrapper.cpp
OBJECT  = $(foreach file, $(SRC), $(basename $(file)).o)
DEP_FILE = $(OBJECT:.o=.d)

all: $(TARGET)

clean:
	-$(RM) -f $(TARGET) $(OBJECT) $(DEP_FILE) *~

$(LIB): $(OBJECT)
	$(AR) cru $@ $^
	$(RANLIB) $@

.c.o .cc.o .cpp.o:
	$(CXX) $(CFLAGS) -c $<

.c.d .cc.d .cpp.d:
	$(CXX) $(CFLAGS) -M $< | \
    $(SED) 's,\($(notdir $*)\.o\) *:,$(dir $@)\1 $@: $(MAKE_INC) $(MAKE_CFG) ,' >$@.tmp
	$(MV) $@.tmp $@
