/*
 * Animations for the messaging system
 */

$(document).ready(function() {
    $(".note, .error, .warning").slideDown("slow");
});

$(".note, .error, .warning").click(function() {
    $(this).slideUp("slow");
});