#######################################################################
# Template of for compiling erlang files                              #
# The environment variable  $TOOLSHOME home has to be set to where    #  
# the generic make script is installed (erlang).                      #
#######################################################################
# code to compile
SOURCE = _common/distributed_networks.erl \
	

#Where include files are stored ".hrl"
EFLAGS = #-I../include \
#	 -I../dummy1/include


#######################################################################
# Generic make script for compiling erlang code                       #
# The environment variable $ERLHOME has to be set to where erlang/OTP #
# is installed                                                        #
# Compiles the code into a ebin dir. relative to the source dir.      #
# (../ebin)                                                           #
####################################################################### 
#Compiles the code into out dir. relative to the source dir. 
ERLHOME = /usr/local
EBIN = out
ERL = erl
GEN = beam
ERLC_EMULATOR = erl -boot start_clean
PATH= .:$(ERLHOME)/bin:/bin:/usr/bin:/usr/local/bin
TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

CODE = $(SOURCE:%.erl=$(EBIN)/%.beam) 

$(EBIN)/%.beam: %.erl
	$(ERLHOME)/bin/erlc  -W -b beam -o $(EBIN) $(EFLAGS) $(WAIT) $<


all: $(TARGETS)
 

   
clean:
	\rm -f $(CODE) 


realclean: clean
	\rm -f \.* *~ *\% #*  *.beam


#######################################################################
# Do not edit below this line                                         #
#######################################################################
#Include following generic make script
#include $(TOOLSHOME)/erlang
