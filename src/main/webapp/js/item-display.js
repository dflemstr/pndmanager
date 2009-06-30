/*
 * Animations for pages displaying packages.
 */

$(document).ready(function() {
    $('.downloadlink')
        .css({
            backgroundPosition: "0 -350px",
            color: "#465A00"
        })
        .live("mouseover", function(){
            $(this).stop().animate({
                backgroundPosition: "0 0",
                color: "#FFFFFF"
            }, 1000);
        })
        .live("mouseout", function(){
            $(this).stop().animate({
                backgroundPosition:"0 -350px",
                color: "#465A00"
            }, 500);
        });
});
