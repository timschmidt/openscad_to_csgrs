# openscad_to_csgrs
OpenSCAD to csgrs converter built with tree sitter

parse_openscad_code() creates a Parser, sets the OpenSCAD grammar, then parses the entire code into a tree_sitter::Tree.
The returned root_node is usually source_file.

eval_node() recursively traverses the parse tree, identifying node kinds like "source_file", "_statement", "union_block", and so on.
We gather child geometry (sub-nodes) and combine them (union, difference, intersection, etc.).

eval_module_call() handles a node like module_call, i.e. translate(...), cube(...). We read arguments (naively), interpret them, and produce a CSG. For transforms like translate([x,y,z]) { body }, we evaluate the body into a CSG and then call .translate(...) on it.

Argument/Expression Parsing in this example is very simplified. We just look for numeric children, ignoring named parameters or complex expressions. OpenSCAD has more sophisticated usage (e.g. cube(size=[1,2,3], center=true)). We need a more thorough expression evaluator for fully correct results.

# todo
- Named Parameters: Look for assignment nodes inside the arguments, e.g. r = 5, center = true. We can store them in a hash map, then interpret them properly.
- for(...) loops, if(...) statements, etc.: We can detect those node types (e.g. "for_block", "if_block") and expand them in an AST pass or interpret them on the fly (unrolling loops, branching on if conditions, etc.).
- Variables and Scope: In OpenSCAD, you can have a = 10; translate([a,0,0]) cube(a);. We can store variables in a symbol table, handle scoping, and so forth.
- Modules (user-defined subroutines): A node of type module_declaration can define a named block of code. We can store it in a map from name → parse subtree, then when we see a module_call to that name, we expand or interpret that subtree.
- Use more complete OpenSCAD grammar: https://github.com/mkatychev/tree-sitter-openscad/blob/master/CHANGELOG.md#version-060
