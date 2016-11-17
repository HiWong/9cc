# Makefile for 7cc

# version
MAJOR = 0
MINOR = 4
FIXES = 0
EXTRAVERSION = -dev

CFLAGS = -Wall -std=c99 -I.
LDFLAGS =
CONFIG_FLAGS =
KERNEL := $(shell uname)
RM = @rm -f
AR = ar
ARFLAGS = cru
CONFIG_H = config.h
BUILD_DIR = "$(shell pwd)"
libutils_dir = libutils/
libcpp_dir = libcpp/
burg_dir = 7burg/
BURG = $(burg_dir)7burg
7CC = 7cc
7CC_OBJ =
7CC_INC =
CC1 = cc1
CC1_OBJ =
CC1_INC =
LIBUTILS = $(libutils_dir)libutils.a
LIBUTILS_OBJ =
LIBUTILS_INC =
LIBCPP = $(libcpp_dir)libcpp.a
LIBCPP_OBJ =
LIBCPP_INC =

CC1_OBJ += error.o
CC1_OBJ += ast.o
CC1_OBJ += cc.o
CC1_OBJ += symtab.o
CC1_OBJ += type.o
CC1_OBJ += parser.o
CC1_OBJ += sema.o
CC1_OBJ += tree.o
CC1_OBJ += eval.o
CC1_OBJ += gen.o
CC1_OBJ += print.o
CC1_OBJ += debug.o

CC1_INC += gen.h
CC1_INC += cc.h
CC1_INC += debug.h

TARGET_SPEC = x86_64-linux.7brg
TARGET_OBJ = x86_64-linux.o
TARGET_SRC = x86_64-linux.c

LIBUTILS_OBJ += $(libutils_dir)alloc.o
LIBUTILS_OBJ += $(libutils_dir)wrapper.o
LIBUTILS_OBJ += $(libutils_dir)strbuf.o
LIBUTILS_OBJ += $(libutils_dir)vector.o
LIBUTILS_OBJ += $(libutils_dir)string.o
LIBUTILS_OBJ += $(libutils_dir)list.o
LIBUTILS_OBJ += $(libutils_dir)file.o

LIBUTILS_INC += $(libutils_dir)utils.h
LIBUTILS_INC += $(libutils_dir)strbuf.h
LIBUTILS_INC += $(libutils_dir)vector.h
LIBUTILS_INC += $(libutils_dir)list.h
LIBUTILS_INC += $(libutils_dir)color.h

LIBCPP_OBJ += $(libcpp_dir)lex.o
LIBCPP_OBJ += $(libcpp_dir)cpp.o
LIBCPP_OBJ += $(libcpp_dir)hideset.o
LIBCPP_OBJ += $(libcpp_dir)idtab.o
LIBCPP_OBJ += $(libcpp_dir)input.o
LIBCPP_OBJ += $(libcpp_dir)error.o
LIBCPP_OBJ += $(libcpp_dir)expr.o
LIBCPP_OBJ += $(libcpp_dir)strtab.o
LIBCPP_OBJ += $(libcpp_dir)sys.o

LIBCPP_INC += $(libcpp_dir)lex.h
LIBCPP_INC += $(libcpp_dir)token.def
LIBCPP_INC += $(libcpp_dir)internal.h

7CC_OBJ += 7cc.o

ifneq (, ${STAGE})
CONFIG_FLAGS += -DSTAGE=${STAGE}
else
CFLAGS += -g
# CFLAGS += -pg
# LDFLAGS += -pg
endif

ifeq (Linux, $(KERNEL))
CONFIG_FLAGS += -DCONFIG_LINUX -DCONFIG_COLOR_TERM
else ifeq (Darwin, $(KERNEL))
CONFIG_FLAGS += -DCONFIG_DARWIN -DCONFIG_COLOR_TERM
XCODE_SDK_DIR := $(shell xcrun --show-sdk-path)
OSX_SDK_VERSION := $(shell xcrun --show-sdk-version)
else
$(error unsupported platform '$(KERNEL)')
endif

CFLAGS += $(CONFIG_FLAGS)

all:: $(CONFIG_H) $(7CC) $(CC1)

$(7CC): $(LIBUTILS) $(7CC_OBJ)
	$(CC) $(7CC_OBJ) $(LIBUTILS) $(LDFLAGS) -o $@

$(CC1): $(LIBUTILS) $(LIBCPP) $(CC1_OBJ) $(TARGET_OBJ)
	$(CC) $(CC1_OBJ) $(TARGET_OBJ) $(LIBCPP) $(LIBUTILS) $(LDFLAGS) -o $@

$(CC1_OBJ): $(CC1_INC) $(CONFIG_H)

$(TARGET_OBJ): $(TARGET_SRC)
	$(CC) $(CFLAGS) -c $(TARGET_SRC) -o $@

$(TARGET_SRC): $(TARGET_SPEC) $(CC1_INC) $(CONFIG_H) $(BURG)
	$(BURG) $(TARGET_SPEC) -o $@

$(LIBUTILS_OBJ): $(LIBUTILS_INC)

$(LIBUTILS): $(LIBUTILS_OBJ)
	$(AR) $(ARFLAGS) $@ $(LIBUTILS_OBJ)

$(LIBCPP_OBJ): $(LIBCPP_INC)

$(LIBCPP): $(LIBCPP_OBJ)
	$(AR) $(ARFLAGS) $@ $(LIBCPP_OBJ)

$(BURG):
	cd $(burg_dir) && make

$(CONFIG_H):
	@echo "/* Auto-generated by makefile. */" > $@
	@echo "#ifndef CONFIG_H" >> $@
	@echo "#define CONFIG_H" >> $@
	@echo >> $@
	@echo "#define VERSION \"$(MAJOR).$(MINOR).$(FIXES)$(EXTRAVERSION)\"" >> $@
	@echo "#define BUILD_DIR \"$(BUILD_DIR)\"" >> $@
ifeq (Darwin, $(KERNEL))
	@echo "#define XCODE_DIR \"$(XCODE_SDK_DIR)\"" >> $@
	@echo "#define OSX_SDK_VERSION \"$(OSX_SDK_VERSION)\"" >> $@
endif
	@echo >> $@
	@echo "#endif" >> $@

#
# Bootstrap
#
stage1:
	$(MAKE) objclean
	$(MAKE) CC=cc STAGE=1
	mv 7cc stage1
	mv cc1 cc1_stage1
	ln -s cc1_stage1 cc1

stage2: stage1
	$(MAKE) objclean
	$(MAKE) CC=./stage1 STAGE=2
	mv 7cc stage2
	mv cc1 cc1_stage2
	ln -s cc1_stage2 cc1

stage3: stage2
	$(MAKE) objclean
	$(MAKE) CC=./stage2 STAGE=3
	mv 7cc stage3
	mv cc1 cc1_stage3
	ln -s cc1_stage3 cc1

bootstrap: stage3
	cmp stage2 stage3
	cmp cc1_stage2 cc1_stage3

objclean::
	$(RM) $(CC1_OBJ)
	$(RM) *.o
	$(RM) $(LIBUTILS_OBJ) $(LIBUTILS)
	$(RM) $(LIBCPP_OBJ) $(LIBCPP)
	@cd $(burg_dir) && make clean

clean:: objclean
	$(RM) $(7CC) $(CC1)
	$(RM) stage1 stage2 stage3 cc1_stage1 cc1_stage2 cc1_stage3
	$(RM) $(CONFIG_H)
	$(RM) $(TARGET_SRC)

