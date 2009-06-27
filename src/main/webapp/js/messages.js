/*
 * Animations for the messaging system
 */

$(document).ready(function() {
    $(".success, .error, .notice").slideDown("slow");

    $(".success, .error, .notice").click(function() {
        $(this).slideUp("slow");
    });
});