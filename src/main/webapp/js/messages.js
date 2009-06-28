/*
 * Animations for the messaging system
 */

jQuery(document).ready(function() {
    jQuery(".success, .error, .notice").slideDown("slow");

    jQuery(".success, .error, .notice").click(function() {
        jQuery(this).slideUp("slow");
    });
});