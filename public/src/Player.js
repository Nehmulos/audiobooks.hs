// TODO get rid of this baseclass sillyness. 
// RemotePlayer and WebPlayer are too different and don't share a lot.
// put stuff from here into playerGui to update the gui

function Player(typeName) {
    this.typeName = typeName;
    this.track = null;
    this.trackList = [];
    this.status = "paused";
    this.progress = {current: 0, length: 100};
}

Player.prototype.transfereFromPlayer = function(player) {
    var tracks;
    if (player.track) {
        tracks = [player.track].concat(player.trackList);
    } else {
        tracks = player.trackList;
    }
    
    if (tracks.length > 0) {
        this.setTrackList(tracks);
        if (player.status == "playing") {
            this.play();
        }
    }
}

Player.prototype.nowPlaying = function() {
    var albumName;
    var artistName;
    var trackName;
    var extension;
    
    var parts = this.track ? this.track.split("/") : [];
    if (parts.length >= 4) {
        albumName = parts[parts.length-3];
        artistName = parts[parts.length-4];
        
        if (/([^\/]+)\..*$/.test(parts[parts.length-1])) {
            var nameMatches = /([^\/]+)(\..*)$/.exec(parts[parts.length-1]);
            trackName = nameMatches[1];
            extension = nameMatches[2];
        }
    }
    return {
        artistName: artistName,
        albumName: albumName,
        trackName: trackName,
        extension: extension
    }
}

Player.prototype.setTrack = function(url) {
    var trackName = url;
    var artistName = "nobody";
    
    $(".nowPlayingText .track").removeClass("clickable");
    $(".nowPlayingText .artist").removeClass("clickable");
    $(".nowPlayingText .track").unbind("click");
    $(".nowPlayingText .artist").unbind("click");
    
    this.track = url;
    var nowPlaying = this.nowPlaying();
    if (url) {
        if (nowPlaying.artistName && nowPlaying.albumName) {
            $(".nowPlayingText .track").addClass("clickable");
            $(".nowPlayingText .track").click(function() {
                window.location.hash = "#!/" +
                    nowPlaying.artistName + "/" +
                    nowPlaying.albumName;
            });
        }
        
        if (nowPlaying.artistName) {
            $(".nowPlayingText .artist").addClass("clickable");
            $(".nowPlayingText .artist").click(function() {
                window.location.hash = "#!/" + nowPlaying.artistName;
            });
        }
        
        
        $(".playerGui .pauseButton").removeClass("disabled");
        $(".playerGui .playButton").removeClass("disabled");
        
        $(".nowPlayingText .track")
        
    } else {
        $(".playerGui .pauseButton").addClass("disabled");
        $(".playerGui .playButton").addClass("disabled");
    }
    $(".nowPlayingText .track").text(nowPlaying.trackName);
    $(".nowPlayingText .artist").text(nowPlaying.artistName);
}

Player.prototype.setStatus = function(status) {
    this.status = status;
    if (status == "paused") {
        $(".playerGui .pauseButton").hide();
        $(".playerGui .playButton").show();
    } else if (status == "playing") {
        $(".playerGui .pauseButton").show();
        $(".playerGui .playButton").hide();
    } else {
        $(".playerGui .pauseButton").addClass("disabled");
        $(".playerGui .playButton").addClass("disabled");
    }
}

Player.prototype.setTrackList = function(trackList) {
    this.trackList = trackList;
}

Player.prototype.playList = function(list) {
    var _this = this;
    this.stop(function() {
        _this.setTrackList(list, function() {
            _this.play();
        });
    });
}

Player.prototype.play = function(url, startTime) {
    if (url != undefined) {
        this.setTrack(url, startTime);
        if (url != null) {
            this.continuePlaying();
        }
    } else if (this.track) {
        this.continuePlaying();
    } else {
        this.playNextTrack();
    }
}

Player.prototype.setProgress = function(progress) {
    progress = progress || {current:0, length: 0};
    this.progress = progress;
    $(".playerGui .progressBar").attr("value", progress.current);
    $(".playerGui .progressBar").attr("max", progress.length);
}

Player.prototype.stop = function(url) {
    this.setTrack(null);
}

Player.prototype.continuePlaying = function() {
    
}

Player.prototype.playNextTrack = function() {
    if (this.trackList.length > 0) {
        var track = this.trackList[0];
        this.trackList.splice(0,1);
        this.play(track);
    }
}

Player.prototype.pause = function() {
    this.updateProgress();
}

Player.prototype.updateProgress = function() {
    
}

Player.prototype.setVolume = function() {
}

