function Tools() {
    this.init();
}

Tools.prototype.init = function() {
    var _this = this;
    $(".cleanCacheButton").click(function() { _this.cleanCache(); });
    $(".toggleSpeakersButton").click(function() { _this.togglePlayer(); });
}

Tools.prototype.cleanCache = function() {
    
}

Tools.prototype.togglePlayer = function() {
    var newPlayer;
    //app.player.pause();
    
    if (app.player.typeName == "WebPlayer") {
        newPlayer = new RemotePlayer();
    } else {
        newPlayer = new WebPlayer();
    }

    //newPlayer.transfereFromPlayer(app.player);
    //app.player.stop();
    localStorage.setItem("abp_playerType", newPlayer.typeName);
    app.player = newPlayer;
}
