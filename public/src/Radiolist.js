$(function() {
    $(".showRadiosButton").mouseover(function() {
        $("#radioList").show("slow");
    });
    
    $("#radioList").mouseleave(function() {
        $("#radioList").hide("slow");
    });

    $(".radioLink").click(function() {
        var stationUrl = $(this).attr("data-url");
        app.player.play(stationUrl);
    });
});
