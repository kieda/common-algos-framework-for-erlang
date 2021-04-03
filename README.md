## Common algos framework for erlang (Caffe)

* Is a framework to create and deploy distributed algos on a graph G = (V, E). 
* Separates user logic from distributed algos running on each vertex
* Distributed algos are composable via plug-in manager barista (yet to come up with witty acronym for this one - ya i know it's a stretch)

### Example Usage (WIP - find elegant API)
<code>
  G = {V, E} = {[a, b, c, d], [{a, b}, {c, d}, {a, d}, {b, c}].
  
  possibly have a helper function in barista.erl that will contain defaults to run the system, with barista -> caffe -> caffe_graph.
  
  todo: gather list of arguments that we will eventully specify. How many can we fit in as a default? How many are common across many vertices & can be specified that way?
</code>
