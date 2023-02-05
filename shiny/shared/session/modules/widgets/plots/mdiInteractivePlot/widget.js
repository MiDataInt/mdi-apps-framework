// initialize plot interactions
Shiny.addCustomMessageHandler("mdiInteractivePlotInit", function(opt){

    // parse interactive targets
    const hoverId = opt.prefix + "hover";    
    const wrapperId = "#" + opt.prefix + "outerWrapper";
    const mdiCrosshair  = wrapperId + " .mdiCrosshair";
    const mdiHorizontal = wrapperId + " .mdiHorizontal";
    const mdiVertical   = wrapperId + " .mdiVertical";
    const mdiBrushBox   = wrapperId + " .mdiBrushBox";

    // convert document to widget coordinates
    const relCoord = function(event, element){return {
        x: event.pageX - $(element).offset().left,
        y: event.pageY - $(element).offset().top,
    }}

    // initialize widget state
    let mouseIsDown = false;
    let isDrawingBox = false;
    let boxStart = {};
    let isBrushEvent = false;
    let hoverJob = 0;
    let isHovered = false;

    // handle all requested interactions, listed here if rough order of occurrence
    $(wrapperId).off("mouseenter").on("mouseenter", function() {
        $(mdiCrosshair).show();
    });
    $(wrapperId).off("mousedown").on("mousedown", function(event) {
        const coord = relCoord(event, this);
        if(coord.y > 0 && coord.x > 0)
            $(mdiBrushBox).css({top: coord.y - 1, left: coord.x - 1});       
        mouseIsDown = true;
    });
    $(wrapperId).off("mousemove").on("mousemove", function(event) {
        const coord = relCoord(event, this);
        $(mdiHorizontal).css({top:  coord.y - 1}); // -1 to prevent divs from taking the focus
        $(mdiVertical)  .css({left: coord.x - 1});
        clearInterval(hoverJob);
        if(isHovered) {
            if(opt.hover) Shiny.setInputValue(hoverId, undefined, { priority: "event" }); 
            isHovered = false;
        }
        if(mouseIsDown){
            if(!isDrawingBox) {
                $(mdiBrushBox).css({width: "0px", height: "0px"}).show();                
                boxStart = coord; 
                $(mdiCrosshair).hide();
                isDrawingBox = true;
            }
            $(mdiBrushBox).css({
                width:  (coord.x - boxStart.x) + "px", 
                height: (coord.y - boxStart.y) + "px"
            });
        } else {
            hoverJob = setTimeout(function(){
                if(opt.hover) Shiny.setInputValue(hoverId, {coord: coord, keys: {}}, { priority: "event" });
                isHovered = true;
            }, opt.delay);            
        }
    });
    $(wrapperId).off("mouseup").on("mouseup", function(event) {
        if(isDrawingBox){
            let boxEnd = relCoord(event, this);
            let data = {
                coord: {
                    x1: boxStart.x,
                    y1: boxStart.y,
                    x2: boxEnd.x,
                    y2: boxEnd.y
                }, 
                keys: {
                    ctrl:  event.ctrlKey,
                    alt:   event.altKey,
                    shift: event.shiftKey
                }  
            };
            if(opt.brush) Shiny.setInputValue(opt.prefix + "brush", data, { priority: "event" });
            isBrushEvent = true; // suppress the ensuing click event
            $(mdiBrushBox).hide();
            isDrawingBox = false;
        }
        mouseIsDown = false;
        $(mdiCrosshair).show();
    });
    $(wrapperId).off("click").on("click", function(event){
        if(!isBrushEvent) {
            const data = {
                coord: relCoord(event, this),
                keys: {
                    ctrl:  event.ctrlKey,
                    alt:   event.altKey,
                    shift: event.shiftKey
                }
            };
            // if(opt.brush) Shiny.setInputValue(opt.prefix + "click", data, { priority: "event" });
            Shiny.setInputValue(opt.prefix + "click", data, { priority: "event" });
        }
        isBrushEvent = false;
    });
    $(wrapperId).off("mouseleave").on("mouseleave", function() {
        $(mdiCrosshair).hide();
        $(mdiBrushBox).hide();
        clearInterval(hoverJob);
        if(opt.hover) Shiny.setInputValue(hoverId, undefined, { priority: "event" }); 
        isHovered = false;
    });
});

// update the plot or image
Shiny.addCustomMessageHandler("mdiInteractivePlotUpdate", function(opt){
    const wrapperId = "#" + opt.prefix + "outerWrapper";
    const imageId   = "#" + opt.prefix + "image";  
    $(wrapperId).css({
        width:  opt.width + "px",
        height: opt.height + "px"
    });
    $(imageId).attr({
        src: opt.src
    });     
});
