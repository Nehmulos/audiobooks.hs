function WebPlayer() {
    Player.prototype.constructor.call(this, "WebPlayer");
    
    $(".tool.toggleSpeakersButton").attr("src", "img/speakerIcon.png");
    
    var _this = this;
    this.audioTag = $("#audioPlayer");
    
    
    this.audioTag.on("ended", function() {
        _this.playNextTrack();
    });
    
    this.audioTag.on("timeupdate", function(event) {
        _this.setProgress({
            current: Math.floor(_this.audioTag.get(0).currentTime),
            length: Math.floor(_this.audioTag.get(0).duration)
        });
    });
}
WebPlayer.prototype = new Player();

WebPlayer.prototype.setTrack = function(url) {
    Player.prototype.setTrack.call(this, url);
    var audioElement = $("#audioPlayer").get(0);
    audioElement.src = url + "?stream";
    this.progress = {current: 0, length: audioElement.duration};
}

WebPlayer.prototype.setTrackList = function(list, callback) {
    Player.prototype.setTrackList.call(this, list);
    if (callback) {
        callback();
    }
}

WebPlayer.prototype.stop = function(callback) {
    Player.prototype.stop.call(this);
    if (callback) {
        callback();
    }
}

WebPlayer.prototype.continuePlaying = function() {
    Player.prototype.continuePlaying.call(this);
    var audioElement = $("#audioPlayer").get(0);
    audioElement.play();
}

WebPlayer.prototype.pause = function(callback) {
    Player.prototype.pause.call(this);
    var audioElement = $("#audioPlayer").get(0);
    audioElement.pause();
    if (callback) {
        callback();
    }
}

WebPlayer.prototype.stop = function(callback) {
    var audioElement = $("#audioPlayer").get(0);
    audioElement.pause();
    audioElement.src = "";
    if (callback) {
        callback();
    }
}

WebPlayer.prototype.jumpTo = function(secTime, callback) {
    var audioElement = $("#audioPlayer").get(0);
    audioElement.currentTime = secTime;
    if (callback) {
        callback();
    }
}

WebPlayer.prototype.setVolume = function(volumePercent) {
    this.audioTag.get(0).volume = volumePercent;
}
