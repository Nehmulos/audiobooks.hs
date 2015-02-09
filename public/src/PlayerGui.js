function PlayerGui() {
    this.init();
}

PlayerGui.prototype.init = function() {
    var _this = this;
    $(".playerGui .stopButton").click(function() {
        app.player.stop();
    });
    
    $(".playerGui .pauseButton").click(function() {
        app.player.pause();
        if (!$(this).hasClass("disabled")) {
            $(".playerGui .pauseButton").hide();
            $(".playerGui .playButton").show();
        }
    });
    
    $(".playerGui .playButton").click(function() {
        app.player.continuePlaying();
        if (!$(this).hasClass("disabled")) {
            $(".playerGui .playButton").hide();
            $(".playerGui .pauseButton").show();
        }
    });
    
    $(".volumeSettings").mouseover(function() {
        var bar = $(this).find(".percentageBar");
        bar.show();
        bar.css("top", $(this).position().top - bar.height());
        bar.css("left", $(this).position().left);
    });
    
    $(".volumeSettings").mouseout(function() {
        var bar = $(this).find(".percentageBar");
        bar.hide();
    });
    
    $(".volumeSettings .percentageBar").click(function(event) {
        var clickY = event.pageY - $(this).offset().top;
        var percent = 1.0 - clickY / $(this).height();
        _this.setVolume(percent);
        app.player.setVolume(percent);
    });
}

PlayerGui.prototype.setVolume = function(percent) {
    $(".volumeSettings .percentageBar").each(function() {
        $(this).find(".disabledFill").css(
            "height",
            (1.0 - percent) * $(this).height()
        );    
    });
    $(".volumeSettings").attr("data-volume:", percent);
}
