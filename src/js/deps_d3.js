goog.provide('depsd3');
goog.addDependency("deps_d3.js", ['depsd3'], ['cljsjs.d3']);

depsd3.debugMessage = 'Dead Code';

depsd3.dependency = function() {
    var instance = {};

    instance.update = function(data) {

        var diameter = 1000,
            radius = diameter / 2,
            innerRadius = radius - 120;

        var cluster = d3.layout.cluster()
                .size([360, innerRadius])
                .sort(null)
                .value(function(d) { return d.size; });

        var bundle = d3.layout.bundle();

        var line = d3.svg.line.radial()
                .interpolate("bundle")
                .tension(.85)
                .radius(function(d) { return d.y; })
                .angle(function(d) { return d.x / 180 * Math.PI; });

        var svg = d3.select("#depgraph")
                .append("g").attr("transform", "translate(" + radius + "," + radius + ")");

        var link = svg.append("g").selectAll(".link"),
            node = svg.append("g").selectAll(".node");

     
      
    };



    instance.sayHello = function(message) {
        console.log(message);
    };
    instance.getMessage = function() {
        return d3.version;
    };
    return instance;
};
