
puts "Loading ProB Tree Inspector"
source treeInspector.tcl
namespace import ::treeInspector::*
puts "Starting Tree Inspector"
openTreeInspector infolog_problems
puts "Done"
