/*
 * Animations for pages displaying packages.
 */

jQuery(document).ready(function() {
    jQuery('.downloadlink')
        .css({
            backgroundPosition: "0 -350px",
            color: "#465A00"
        })
        .live("mouseover", function(){
            jQuery(this).stop().animate({
                backgroundPosition: "0 0",
                color: "#FFFFFF"
            }, 1000);
        })
        .live("mouseout", function(){
            jQuery(this).stop().animate({
                backgroundPosition:"0 -350px",
                color: "#465A00"
            }, 500);
        });
});
