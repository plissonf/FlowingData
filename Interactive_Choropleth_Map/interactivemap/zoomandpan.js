// Zooming
rsr.setViewBox(0, 0, rsr.width, rsr.height);
var viewBoxWidth = rsr.width;
var viewBoxHeight = rsr.height;
var canvasID = "#map";
var startX, startY;
var mousedown = false;
var dX, dY;
var oX = 0, oY = 0, oWidth = viewBoxWidth, oHeight = viewBoxHeight;
var currzoom = 0;

var viewBox = rsr.setViewBox(oX, oY, viewBoxWidth, viewBoxHeight);
viewBox.X = oX;
viewBox.Y = oY;

// Helper function to handle zooming changes
function handle(delta) {
    vBHo = viewBoxHeight;
    vBWo = viewBoxWidth;
    
    if (delta < 0) {
        if (viewBoxWidth < 350) return;
        viewBoxWidth *= 0.95;
        viewBoxHeight*= 0.95;
    }
    else {
        if (viewBoxWidth > rsr.width) return;
        viewBoxWidth *= 1.05;
        viewBoxHeight *= 1.05;
    }
    
    viewBox.X -= (viewBoxWidth - vBWo) / 2;
    viewBox.Y -= (viewBoxHeight - vBHo) / 2;                        
    rsr.setViewBox(viewBox.X, viewBox.Y, viewBoxWidth, viewBoxHeight);
}


// Event handler for mouse wheel zooming
function wheel(event) {
    var delta = 0;
    if (!event) /* For IE. */
        event = window.event;
    
    if (event.wheelDelta) { /* IE/Opera. */
        delta = event.wheelDelta/120;
    } else if (event.detail) { /** Mozilla case. */
        /** In Mozilla, sign of delta is different than in IE.
         * Also, delta is multiple of 3.
         */
         delta = -event.detail/3;
    }
    
    /** If delta is nonzero, handle it.
      * Basically, delta is now positive if wheel was scrolled up,
      * and negative, if wheel was scrolled down.
      */
    if (delta) {
        handle(delta);
        $(tooltip).hide();
    }
        
    
    /** Prevent default actions caused by mouse wheel.
      * That might be ugly, but we handle scrolls somehow
      * anyway, so don't bother here..
      */
    if (event.preventDefault)
        event.preventDefault();
    
    event.returnValue = false;
}


// Initialization and add listeners.
if (window.addEventListener)
    /** DOMMouseScroll is for mozilla. */
    window.addEventListener('DOMMouseScroll', wheel, false);

/** IE/Opera. */
window.onmousewheel = document.onmousewheel = wheel;

// Add event handling for mouse actions.
$(canvasID).mousedown(function(e) {
    mousedown = true;
    startX = e.pageX;
    startY = e.pageY;
});

$(canvasID).mousemove(function(e) {
    if (mousedown == false) { return; }
    dX = startX - e.pageX;
    dY = startY - e.pageY;
    x = viewBoxWidth / rsr.width; 
    y = viewBoxHeight / rsr.height;
    dX *= x;
    dY *= y;
    rsr.setViewBox(viewBox.X + dX, viewBox.Y + dY, viewBoxWidth, viewBoxHeight);
});
            
$(canvasID).mouseup(function(e) {
    if (mousedown == false) return; 
    viewBox.X += dX; 
    viewBox.Y += dY; 
    mousedown = false;         
});