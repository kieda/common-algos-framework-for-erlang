## Common algos framework for erlang (Caffe)

* Is a framework to create and deploy distributed algos on a graph G = (V, E). 
* Separates user logic from distributed algos running on each vertex
* Distributed algos are composable via plug-ins

### Example Usage

    G = {V, E} = caffe_graph:load(graph1),
    Network = caffe:build( G, caffe:args_from_list( [ { V, { [ lamport_clock ], worker_random_messenger } } ] ) ),
    caffe:start( Network )

* We load a graph from a module using `caffe_graph:load/1`
* `worker_random_messenger` represents the user-based function running on each vertex in `V`\
   This function sends a unique message to a random outgoing edge.
* `[ lamport_clock ]` represents the set of plugins/distributed algorithms we want to run on top of the user-based function\
  This algorithm creates a logical clock based on send and receive events\
  You may specify multiple plugins, and plugins that use other plugins (dependencies) are auto-loaded
