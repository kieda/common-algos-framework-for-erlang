#######################################################################
# Template of for compiling erlang files                              #
# The environment variable  $TOOLSHOME home has to be set to where    #  
# the generic make script is installed (erlang).                      #
#######################################################################
# code to compile
SOURCE = _common/caffe_util.erl \
	_common/ezpr.erl \
	_common/cyclic_queue.erl \
	_common/caffe_graph.erl \
	_common/caffe.erl \
	_system_plugins/graph_state.erl \
	_system_plugins/messenger.erl \
	_system_plugins/terminator.erl \
	testutil/graph1.erl \
	testutil/worker_random_messenger.erl \
	chapter2/lamport_clock.erl \
	chapter2/vector_clock.erl

#Where include files are stored ".hrl"
EFLAGS = -I _common \
 	 -I _system_plugins \
	 -I chapter2


#######################################################################
# Generic make script for compiling erlang code                       #
# The environment variable $ERLHOME has to be set to where erlang/OTP #
# is installed                                                        #
# Compiles the code into a ebin dir. relative to the source dir.      #
# (../ebin)                                                           #
####################################################################### 
#Compiles the code into out dir. relative to the source dir. 
ERLHOME = /usr/local
EBIN = ../out
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
