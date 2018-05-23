
OUTPUT = hash_map_test

CONFIG = debug
#CONFIG = profile
#CONFIG = release

CFLAGS +=
CXXFLAGS += -std=c++1z
CPPFLAGS +=
LDFLAGS +=

SRCDIRS = . src
SRCEXTS = .c .cpp

HDRDIRS = $(SRCDIRS) ../include
HDREXTS = .h .hpp

LIBDIRS = lib
LIBS =

#CC = clang
#CXX = clang++

DEBUGFLAGS = -g -O2
PROFILEFLAGS = -g -O2 -DNDEBUG
RELEASEFLAGS = -ffast-math -O2 -DNDEBUG

SOURCES = $(foreach d,$(SRCDIRS),$(wildcard $(addprefix $(d)/*,$(SRCEXTS))))
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))
DEPENDS = $(OBJECTS:.o=.d)

ifeq ($(CONFIG),release)
	CONFIGFLAGS = $(RELEASEFLAGS)
endif
ifeq ($(CONFIG),profile)
	CONFIGFLAGS = $(PROFILEFLAGS)
endif
ifeq ($(CONFIG),debug)
	CONFIGFLAGS = $(DEBUGFLAGS)
endif
CFLAGS += $(CONFIGFLAGS)
CXXFLAGS += $(CONFIGFLAGS)

HDRFLAGS = $(foreach d,$(HDRDIRS),-I$(d))
LIBFLAGS = $(foreach d,$(LIBDIRS),-L$(d)) \
	$(foreach d,$(LIBS),-l$(d))
DEPFLAGS = -MD -MP

COMPILE.c   = $(CC)  $(DEPFLAGS) $(HDRFLAGS) $(CFLAGS)   $(CPPFLAGS) -c
COMPILE.cpp = $(CXX) $(DEPFLAGS) $(HDRFLAGS) $(CXXFLAGS) $(CPPFLAGS) -c
LINK.c   = $(CC)  $(CFLAGS)   $(CPPFLAGS) $(LDFLAGS)
LINK.cpp = $(CXX) $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS)
RM = rm -f

%.o:%.c
	$(COMPILE.c) $< -o $@

%.o:%.cpp
	$(COMPILE.cpp) $< -o $@

$(OUTPUT): Makefile $(OBJECTS)
	$(LINK.cpp) $(OBJECTS) $(LIBFLAGS) -o $@

all: $(OUTPUT)

run: all
	./$(OUTPUT)

debug: all
	gdb -ex run --quiet ./$(OUTPUT)

PROFILEOUTPUT = callgrind.out
#PROFILEPRINTFLAGS = --tree=calling
PROFILEPRINTFLAGS += $(foreach d,$(SRCDIRS),--include=$(d)) $(foreach d,$(HDRDIRS),--include=$(d))
profile: all
	valgrind --tool=callgrind --callgrind-out-file=$(PROFILEOUTPUT) ./$(OUTPUT)
	callgrind_annotate $(PROFILEPRINTFLAGS) $(PROFILEOUTPUT)

clean:
	$(RM) $(OBJECTS) $(OUTPUT)

distclean: clean
	$(RM) $(DEPENDS)

show:
	@echo "CONFIGFLAGS = $(CONFIGFLAGS)"
	@echo "SOURCES = $(SOURCES)"
	@echo "OBJECTS = $(OBJECTS)"
	@echo "DEPENDS = $(DEPENDS)"

ifndef NODEPS
ifneq ($(DEPENDS),)
sinclude $(DEPENDS)
endif
endif

