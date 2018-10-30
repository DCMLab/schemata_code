function ratings(id, d3, ratings, highlights) {    
    var rt = document.getElementById(id);

    // dimensions
    var aspectRatio = 0.5;
    var margin = {top: 20, right: 30, bottom: 30, left: 40},
        width = rt.getBoundingClientRect().width,
        height = width*aspectRatio,
        iwidth = width - margin.right - margin.left,
        iheight = height - margin.top - margin.bottom;

    // scales
    var xScale = d3.scaleLinear()
        .range([0,iwidth])
        .domain([d3.min(ratings, r => r.onset),
                 d3.max(ratings, r => r.offset)]);
    x = xScale;
    var yScale = d3.scaleLinear()
        .range([iheight,0])
        .domain(d3.extent(ratings, r => r.score));
    y = yScale;
    
    var hlScale = d3.scaleOrdinal(d3.schemeCategory10);

    // canvas: for content
    d3.select(rt).select("canvas").remove();
    var canvas = d3.select(rt).append("canvas")
        .attr("width", iwidth)
        .attr("height", iheight)
        .style("transform", "translate("+margin.left+"px,"+margin.top+"px)")
        .style("position", "absolute");
        
    var context = canvas.node().getContext("2d");

    // svg: for axes
    d3.select(rt).select("svg").remove();
    var svg = d3.select(rt).append("svg")
        .attr("width", width).attr("height", height)
      .append("g")
        .attr("transform", "translate("+margin.left+","+margin.top+")");
    var gX = svg.append("g")
        .attr("transform", "translate(0,"+iheight+")")
        .call(d3.axisBottom(x));
    var gY = svg.append("g")
        .call(d3.axisLeft(y));

    // drawing
    
    function drawRating(rating) {
        var sc = y(rating.score);
        context.beginPath();
        context.moveTo(x(rating.onset), sc);
        context.lineTo(x(rating.offset), sc);
        context.stroke();
    }

    function draw() {
        context.clearRect(0,0,iwidth,iheight);

        context.strokeStyle = "lightgrey";
        ratings.forEach(drawRating);

        highlights.forEach(function(hl, i) {
            context.strokeStyle = hlScale(i);
            hl.forEach(drawRating);
        });
    }

    draw();

    // zooming

    function zoomrt() {
        x = d3.event.transform.rescaleX(xScale);
        gX.call(d3.axisBottom(x));
        y = d3.event.transform.rescaleY(yScale);
        gY.call(d3.axisLeft(y));
        draw();
    }

    var zoom = d3.zoom()
        .on("zoom", zoomrt);
    canvas.call(zoom);
    
    // updating content
    
    rt.updateRatings = function(newrs) {
        console.log("updateRatings");
        ratings = newrs;

        xScale.domain([d3.min(ratings, r => r.onset),
                       d3.max(ratings, r => r.offset)]);
        x = xScale;
        gX.call(d3.axisBottom(x));

        yScale.domain(d3.extent(ratings, r => r.score));
        y = yScale;
        gY.call(d3.axisLeft(y));

        draw();
    };

    rt.updateHighlights = function(newhls) {
        highlights = newhls;
        //TODO: zoom
        draw();
    };
}
