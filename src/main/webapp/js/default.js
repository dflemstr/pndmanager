/*
 * Animations for the default template.
 */

/*
 * Returns true if the page was switched and not refreshed the last page load
 */
function pageSwitched() {
    return !(document.referrer.toString() === window.location.toString());
}

/*
 * Returns true if the last page switch meant that we went up a "folder level"
 */
function traversedUp() {
    var r = document.referrer.toString(), l = window.location.toString();
    return (r.length > l.length && r.indexOf(l) === 0);
}

function revealCurrentMenu() {
    jQuery(".selectedmenu span")
        .stop()
        .animate({
            paddingLeft: "15px"
        }, "fast", function() {
                jQuery(this).addClass("witharrow");
            });

    if(!traversedUp()) {
        jQuery(".selectedmenu ul")
            .slideDown("fast");
    }
    else {
        jQuery(".selectedmenu ul").show();
    }
}

function retractCurrentMenu() {
    jQuery(".selectedmenu span")
        .stop()
        .animate({
            paddingLeft: "5px"
        }, "fast").removeClass("witharrow");
}

jQuery(document).ready(function() {
    if(pageSwitched()) {
        jQuery(".selectedmenu ul").hide();
        jQuery(".selectedmenu span").css("padding-left", "5px");
        revealCurrentMenu();
    }

    jQuery(".menuitem a").hover(retractCurrentMenu, revealCurrentMenu);

    jQuery("new-packages").click(function() {
        jQuery(this).slideUp("fast");
    });
});

/*
 * Gets called by the Lift framework when AJAX happens
 */
function showAjax() {
    jQuery("#ajax-loader").fadeIn("normal");
}

/*
 * Gets called by the Lift framework when AJAX doesn't happen
 */
function hideAjax() {
    jQuery("#ajax-loader").fadeOut("normal");
}