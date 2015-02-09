function RemotePlayer() {
    Player.prototype.constructor.call(this, "RemotePlayer");
    
    this.fetchPlayStatus();
    this.fetchVolume();
    
    var _this = this;
    this.progressUpdater = function() {
        if (_this.status == "playing") {
            if (_this.progress.current >= _this.progress.length) {
                //_this.fetchProgress();
                _this.fetchPlayStatus();
            } else {
                _this.progress.current += 1;
                _this.setProgress(_this.progress);
            }
        }
    }
    window.setInterval(this.progressUpdater, 1000);
    
    $(".tool.toggleSpeakersButton").attr("src", "img/headsetIcon.png");
}
RemotePlayer.prototype = new Player();

RemotePlayer.prototype.setTrack = function(url) {
    var _this = this;
    
    Player.prototype.setTrack.call(_this, url);
    
    this.stop(function() {
        _this.setTrackList([url], function() {
            _this.play();
        });
    });
    
}

RemotePlayer.prototype.fetchProgress = function() {
    var _this = this;
    $.getJSON("api/getProgress", function(data) {
        Player.prototype.setProgress.call(_this, data.progress);
    });
}

RemotePlayer.prototype.fetchPlayStatus = function() {
    var _this = this;
    $.getJSON("api/getPlayStatus", function(data) {
        Player.prototype.setTrack.call(_this, data.track);
        Player.prototype.setProgress.call(_this, data.progress);
        Player.prototype.setTrackList.call(_this, data.trackList);
        _this.setStatus(data.paused ? "paused" : "playing");
    });
}

RemotePlayer.prototype.fetchVolume = function() {
    var _this = this;
    $.getJSON("api/getVolume", function(data) {
        app.playerGui.setVolume(data.volume/100);
    });
}

RemotePlayer.prototype.stop = function(callback) {
    
    var gotJson = function(data) {
        this.mplayerInstanceExists = false;
        if (callback) {
            callback();
        }
    };

    Player.prototype.setTrack.call(this, null);
    Player.prototype.setProgress.call(this, null);
    $.getJSON("api/stop", gotJson).error(gotJson);
}

RemotePlayer.prototype.setTrackList = function(url, callback) {
    Player.prototype.setTrackList.call(this, url);
    var args = {trackList: url};
    
    var gotJson = function(data) {
        console.log(data);
        if (callback) {
            callback();
        }
    };
    
    $.ajax({
        url: "api/setTrackList",
        type:"PUT",
        data:JSON.stringify(args)
    }).done(gotJson);
}

RemotePlayer.prototype.continuePlaying = function() {
    var _this = this;
    Player.prototype.continuePlaying.call(this);
    $.getJSON("api/unPause", function(data) {
        console.log(data);
        if (data && !data.error) {
            _this.setStatus("playing");
        }
        $.getJSON("api/activateTimeout", function(timeoutData) {
            console.log("timeout set", timeoutData);
        });
    });
}

RemotePlayer.prototype.pause = function(callback) {
    var _this = this;
    Player.prototype.pause.call(this);
    $.getJSON("api/pause", function(data) {
        console.log(data);
        
        if (data && !data.error) {
            _this.setStatus("paused");
        }
        
        if (callback) {
            callback();
        }
    });
}

RemotePlayer.prototype.updateProgress = function() {
    var _this = this;
    $.getJSON("api/getProgress", function(data) {
        if (!data.error) {
            _this.progress = data.progress;
            console.log("progress: " + data.progress);
        } else {
            console.error(error);
        }
    });
}

RemotePlayer.prototype.playNextTrack = function() {
    // asume tracklist is already on server
    // avoid playing single tracks on remote player,
    // because that requires an active client
    this.continuePlaying(); 
}

RemotePlayer.prototype.setVolume = function(volumePercent) {
    var volume = Math.floor(volumePercent * 100);
    $.getJSON("api/setVolume?" + volume, function(data) {
        console.log(data);
    });
}
