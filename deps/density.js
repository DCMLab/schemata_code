function density(id, vega, vl, data) {
    var elem = document.getElementById(id);

    var aspectRatio = 0.5;
    var width  = elem.getBoundingClientRect().width,
        height = width*aspectRatio;

    var spec = {
        "width": width,
        "height": height,
        "data": { "values": data },
        "mark": "rect",
        "encoding": {
            "x": {
                "field": "onset",
                "type": "quantitative"
            },
            "x2": {
                "field": "offset",
                "type": "quantitative"
            },
            "y": {
                "field": "value",
                "type": "quantitative"
            }
        },
        "selection": {
            "grid": { "bind":"scales", "type": "interval"}
        }
    };
    console.log(spec);

    vgSpec = vl.compile(spec).spec;
    console.log(vgSpec);

    var view = new vega.View(vega.parse(vgSpec))
        .renderer('svg')
        .initialize(elem)
        .hover()
        .run();
    console.log(view);
}
