#
# Default makefile for the CPU part of a MaxCompiler project.
#
# IMPORTANT INFORMATION:
#   - This file must not be included from other Makefiles. (Use make -C instead)
#   - The first file included in this makefile must be the RunRule Makefile.include.
#   - This file cannot reside under a path that includes $THIS_FILENAME or Makefile.include as directory names.
#   - If you change the name of this file you have to update $THIS_FILENAME below.
#   - If you move this file out of the CPU code directory, you need to update $PATH_TO_PROJECT_ROOT below.
#
THIS_FILENAME:=Makefile
PATH_TO_PROJECT_ROOT:=..
.PHONY: all build clean distclean run startsim stopsim runsim


# ======== DO NOT CHANGE ANYTHING IN THE FOLLOWING SECTION ========
#
ifndef RUNRULE
        $(error RUNRULE environment variable is not set. Usage: 'make RUNRULE="<RUNRULE>"')
endif

nullstring :=
space := $(nullstring) # a space at the end
# get the full filename. (respect spaces)
THIS_FILE:=$(subst $(space),\ ,$(wordlist 1, $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST)))
# get the path (respect spaces)
THIS_PATH:=$(subst $(THIS_FILENAME),,$(THIS_FILE))
# set project root variable
PROJECTROOT:=$(THIS_PATH)$(PATH_TO_PROJECT_ROOT)
# get a quoted version of the RunRule
RUNRULE_QUOTE:=$(subst $(space),\ ,$(RUNRULE))
# now include the auto-managed include file of the RunRule
RUNRULE_DIR:=$(PROJECTROOT)/RunRules/$(RUNRULE_QUOTE)

include $(RUNRULE_DIR)/Makefile.include


# ===============  CHANGE  BELOW  ===============
# You may change the following parts of this file.
# A few hints:
#   - All object files need to go into $OBJDIR
#   - Do not forget to include $MAXFILEOBJECTS when linking
#   - If you need a .max file, look into $MAXDIR
#   - All .max files will be automatically re-built if missing.
#     (Currently there is no auto-update if you change the engine code.)
#   - All .max files will be automatically converted to object files.
#
# The following values are provided by the Makefile.include inside each
# RunRule directory.
#
# Directories:
# ~~~~~~~~~~~
#   MAXDIR    the directory containing the maxfiles of a RUNRULE
#   OBJDIR    the directory containing the object files for RUNRULE
#   BINDIR    the directory containing the executable/shared object for RUNRULE
#   INCDIR    the directory containing all RunRule related include files
#
# MAX-Files:
# ~~~~~~~~~~
#   MAXFILEOBJECTS     a list of all 'maxfilecompiled' maxfile objects.
#   SLICFILEOBJECTS    a list of all 'sliccompiled' maxfile objects.
#                      (Used instead of MAXFILEOBJECTS when USE_SLIC is set to 1.)
#   BUILDNAME_MAXFILE_<number>
#                      the name of the <number>th (>0) maxfile of the RunRule.
#   MAXFILE_<number>   the filename of the <number>th (>0) maxfile of the RunRule.
#   MAXFILEBUILDNAMEDEFS
#                      a string that can be passed to gcc and makes all
#                      BUILDNAME_MAXFILE_<Number> and MAXFILE_<Number> definitions
#                      available in CPU code.
#
# Application/Shared Object:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
#   EXE_SO_NAME    the name of the executable or shared object being built
#   LIBLDFLAGS     empty when building an application, 
#                  set to "-shared" when building a shared object
#   LIBGCCFLAGS    empty when building an application,
#                  set to "-fPIC" when building a shared object
#
# Other:
# ~~~~~~
#   DEVICE_NAME      the socket name for which the RUNRULE is being built
#   PORT_NAME_SIM    the simulation port name (socketName0:socketName)
#   MAXCOMPILER_INC  default includes needed by MaxCompiler
#   MAXGENFD_INC     default includes needed by MaxGenFD (if available)
#   MAXCOMPILER_LIBS default libraries needed by MaxCompiler
#   MAXGENFD_LIBS    default libraries needed by MaxGenFD (if available)
#
#   RUNENV           Either empty (normal hw run), the remote execution script,
#                    or the environment for simulation
#   RUNARGS          Arguments as specified in the RunRule (CPU Code tab)
#
#   IS_SIMULATION_PROFILE
#                    One (1) if the RunRule is a simulation RunRule, 0 otherwise.
#   IS_HARDWARE_PROFILE
#                    One (1) if the RunRule is a hardware RunRule, 0 otherwise.
#



# ==== (I) Object Files / MaxCompiler Build Flags ====
# Include the automatically managed file 'Makefile.files.include'.
# This file defines SOURCES and HEADERS. Check that we do not
# have any .cxx or .cc file extensions for source files as this
# would break this Makefile.
include Makefile.files.include
ifneq (,$(findstring .cxx,$(SOURCES)))
        $(error Please rename all .cxx files to .cpp or change the CPU code's Makefile.)
endif
ifneq (,$(findstring .cc,$(SOURCES)))
        $(error Please rename all .cc files to .c or change the CPU code's Makefile.)
endif


# Get the list of object files from SOURCES.
# Also determine the mode: C or C++ (the latter if there is at least on .cpp file).
APP_OBJECTS:=$(patsubst %.cpp, $(OBJDIR)/%.o, $(patsubst %.c, $(OBJDIR)/%.o, $(SOURCES)))
APP_OBJECT_DIRS:= $(patsubst %, $(OBJDIR)/%/, $(sort $(dir $(SOURCES))))
CXX_MODE:=$(findstring .cpp,$(SOURCES))


# Setup compiler/linker flags for MaxCompiler includes etc..
MAXELER_COMPILER_FLAGS=-I $(INCDIR) $(MAXCOMPILER_INC) -D_XOPEN_SOURCE=600 $(LIBGCCFLAGS)
MAXELER_LINKER_FLAGS=$(MAXCOMPILER_LIBS) $(LIBLDFLAGS)



# ==== (II) SLiC ==== 
# If the project uses the SLiC interface, use SLICFILEOBJECTS as
# prerequisites in the rules below.
ifeq ($(USE_SLIC), 1)
MAXFILEOBJECTS:=$(SLICFILEOBJECTS)
endif



# ==== (III) MaxGenFD ==== 
# We need some additional magic for MaxGenFD. (Notice that a MaxGenFD
# design can only have one maxfile.)
ifeq ($(USE_MAXGENFD), 1)
include $(MAXGENFDDIR)/lib/Makefile-maxgenfd.include

# MaxGenFD requires a special init object:
$(OBJDIR)/fdinit_$(BUILDNAME_MAXFILE_1).o: $(MAXDIR)/$(BUILDNAME_MAXFILE_1).max
	$(MAXGENFDMAXCOMPILE) $^ $@ "$(BUILDNAME_MAXFILE_1)"

APP_OBJECTS+=$(OBJDIR)/fdinit_$(BUILDNAME_MAXFILE_1).o

# add MaxGenFD libs and includes to the flags
MAXELER_COMPILER_FLAGS+=$(MAXGENFD_INC)
ifeq ($(IS_SIMULATION_PROFILE), 1)
MAXELER_COMPILER_FLAGS+= -DSIM 
endif
MAXELER_LINKER_FLAGS+=$(MAXGENFD_LIBS)
endif



# ==== (IV) C / C++ Flags ====
# Setup some default C and C++ flags (May be overridden...)
ifndef DEBUG
CFLAGS ?=  -g -O3 -Wall -Wextra -fmessage-length=0
CXXFLAGS ?=  -g -O3 -Wall -Wextra -fmessage-length=0
else
CFLAGS ?=  -g -O0 -Wall -Wextra -fmessage-length=0
CXXFLAGS ?=  -g -O0 -Wall -Wextra -fmessage-length=0
endif

# Final definition of Flags: add MaxCompiler specific things...
override CFLAGS+= $(MAXELER_COMPILER_FLAGS) -std=c99
override CXXFLAGS+= $(MAXELER_COMPILER_FLAGS)
override LDFLAGS+=$(MAXELER_LINKER_FLAGS)


# ==== (VI) Build Rules ====
# The default target will only build the application. (Anything else will confuse MaxIDE..)
all: build


help:
	@echo
	@echo
	@echo "List of Targets"
	@echo "---------------"
	@echo "all (default) ... calls build"
	@echo "build         ... builds the shared object / application and" 
	@echo "                  also builds the .max file(s) if missing." 
	@echo "clean         ... removes temporary files except the .max file" 
	@echo "distclean     ... like clean but also removes the .max file"
	@echo 
	@echo "runsim        ... startsim, run, stopsim (can also be used for HW builds)"
	@echo "startsim      ... starts the simulator if simulation RunRule"
	@echo "stopsim       ... stops the simulator if simulation RunRule"
	@echo "run           ... runs the application"
	@echo
	@echo "List of Environment Variables"
	@echo "-----------------------------"
	@echo "RUNRULE        ... Name of a RunRule; current value: $(RUNRULE)"
	@echo "MAXCOMPILERDIR ... MaxCompiler; current value: $(MAXCOMPILERDIR)"
	@echo "MAXCOMPILER_BUILD_DIR (optional)" 
	@echo "               ... Build directory; current value: $(MAXCOMPILER_BUILD_DIR)" 	
	@echo "CC             ... C compiler; current value: $(CC)"
	@echo "CFLAGS         ... Flags passed to the C compiler; current value: see below"
	@echo "CXX            ... C++ compiler; current value: $(CXX)"
	@echo "CXXFLAGS       ... Flags passed to the C++ compiler; current value: see below"
	@echo
	@echo "ARGS (optional)... Use these arguments to run the application -"
	@echo "                   not the ones stored in the RunRule"
	@echo "EXTRAARGS (optional)"
	@echo "               ... Additional arguments appended to either the"
	@echo "                   arguments stored in the RunRule or passed in via ARGS"
	@echo
	@echo "CFLAGS=$(CFLAGS)"
	@echo "CXXFLAGS=$(CXXFLAGS)"
	@echo
	@echo "Example"
	@echo "-------"
	@echo "Build and run RunRule \"Simulation\": make RUNRULE=Simulation runsim"
	@echo "Build RunRule \"DFE\":                make RUNRULE=DFE"
	@echo
	

# (1) Create all needed directories and then build the application
build:| $(MAXDIR) $(OBJDIR) $(BINDIR) $(APP_OBJECT_DIRS)
	$(MAKE) -f $(THIS_FILENAME) $(BINDIR)/$(EXE_SO_NAME)


# (2) Link all the objects: do not forget the $MAXFILEOBJECTS!
#     Notice: We don't use $^ here because of potential spaces in prereqs.
ifneq (,$(CXX_MODE))
# In C++ mode: use g++
$(BINDIR)/$(EXE_SO_NAME) : $(APP_OBJECTS)
	$(CXX) $(APP_OBJECTS) $(MAXFILEOBJECTS) $(LDFLAGS) -o "$@"
else
# In C mode: use cc
$(BINDIR)/$(EXE_SO_NAME) : $(APP_OBJECTS)
	$(CC) $(APP_OBJECTS) $(MAXFILEOBJECTS) $(LDFLAGS) -o "$@"
endif


# (3) Compile .c/.cpp files to objects.
#     Because of SLiC, C files are potentially dependent on the maxfiles, so
#     specify MAXFILEOBJECTS as prerequisites.
ifneq (,$(CXX_MODE))
# In C++ mode: use g++
$(OBJDIR)/%.o : %.c $(MAXFILEOBJECTS)
	$(CXX) $(CXXFLAGS) $(MAXFILEBUILDNAMEDEFS) -o "$@" -c "$<"
	$(CXX) $(CXXFLAGS) $(MAXFILEBUILDNAMEDEFS) -MM -MP -MT "$(OBJDIR)/$(notdir $@)" "$<" > "$(@:.o=.d)"

$(OBJDIR)/%.o : %.cpp $(MAXFILEOBJECTS)
	$(CXX) $(CXXFLAGS) $(MAXFILEBUILDNAMEDEFS) -o "$@" -c "$<"
	$(CXX) $(CXXFLAGS) $(MAXFILEBUILDNAMEDEFS) -MM -MP -MT "$(OBJDIR)/$(notdir $@)" "$<" > "$(@:.o=.d)"

else
# In C mode: use cc
$(OBJDIR)/%.o : %.c $(MAXFILEOBJECTS)
	$(CC) $(CFLAGS) $(MAXFILEBUILDNAMEDEFS) -o "$@" -c "$<"
	$(CC) $(CFLAGS) $(MAXFILEBUILDNAMEDEFS) -MM -MP -MT "$(OBJDIR)/$(notdir $@)" "$<" > "$(@:.o=.d)"

endif



# (4) Clean will remove all objects and the application
clean:
	$(RM) -r $(OBJDIR)/* $(BINDIR)/$(EXE_SO_NAME)

# Distclean will also remove all maxfiles - use with care!
distclean: clean
	$(RM) -r $(MAXDIR)/*



# (5) Generic run target
run: build
	cd $(PATH_TO_PROJECT_ROOT)/CPUCode && $(RUNENV) "$(realpath $(BINDIR)/$(EXE_SO_NAME))" $(RUNARGS)

# Generic runsim target (starts + stops simulated system automatically)
# Notice: You can also call this on hardware builds (startsim and stopsim
#         targets will be NOPs there...)
runsim: startsim
	$(MAKE) -f $(THIS_FILENAME) run
	$(MAKE) -f $(THIS_FILENAME) stopsim



# create BINDIR
$(BINDIR):
	mkdir -p $@

# create OBJDIR
$(OBJDIR):
	mkdir -p $@

# create MAXDIR
$(MAXDIR):
	mkdir -p $@

# create any OBJDIR-subdirectory that is needed 
$(APP_OBJECT_DIRS):
	mkdir -p $@

-include $(APP_OBJECTS:.o=.d)

