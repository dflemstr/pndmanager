/*
 * Animations for the default template.
 */

/*
 * Returns true if the page was switched and not refreshed the last page load
 */
function pageSwitched() {
    return !(document.referrer.toString() === window.location.toString())
}

/*
 * Returns true if the last page switch meant that we went up a "folder level"
 */
function traversedUp() {
    var r = document.referrer.toString(), l = window.location.toString();
    return (r.length > l.length && r.indexOf(l) == 0);
}

$(document).ready(function() {
    if(pageSwitched())
    {
        $(".selectedmenu ul").hide();
        $(".selectedmenu span").css("padding-left", "5px");
        revealCurrentMenu();
    }

    $(".menuitem a").hover(retractCurrentMenu, revealCurrentMenu);

    $("new-packages").click(function() {
        $(this).slideUp("fast")
    });
});


function revealCurrentMenu() {
    $(".selectedmenu span")
        .stop()
        .animate({
            paddingLeft: "15px"
        }, "fast", function() {
                $(this).addClass("witharrow");
            });

    if(!traversedUp())
        $(".selectedmenu ul")
            .slideDown("fast");
    else
        $(".selectedmenu ul").show();
}

function retractCurrentMenu() {
    $(".selectedmenu span")
        .stop()
        .animate({
            paddingLeft: "5px"
        }, "fast").removeClass("witharrow");
}

/*
 * Gets called by the Lift framework when AJAX happens
 */
function showAjax() {
    $("#ajax-loader").fadeIn("normal");
}

/*
 * Gets called by the Lift framework when AJAX doesn't happen
 */
function hideAjax() {
    $("#ajax-loader").fadeOut("normal");
}