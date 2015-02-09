function TrackScreen(author, book) {
    this.author = author;
    this.book = book;
}

TrackScreen.prototype.createNodes = function() {
    var _this = this;
    
    app.toolbar.setAuthor(this.author);
    app.toolbar.setBook(this.book);
    
    var url = 
        "api/book.json?author=" + encodeURIComponent(_this.author) +
        "&book=" + encodeURIComponent(_this.book);

    $.getJSON(url, function(data) {
        //data.cds = Utils.sortObject(data.cds);
        
        data.cds.sort(function(a,b) {
            return a.name > b.name;
        });
        
        $.each(data.cds, function() {
            this.tracks.sort();
        });
        
        _this.buildNodesFromJson(data);
    });
}

TrackScreen.prototype.buildNodesFromJson = function(data) {
    var _this = this;
    this.data = data;
    data.cds.sort(function(a, b) {
	return a.name > b.name;
    });
    $.each(data.cds, function(cdIndex, cd) {
        var cdName = this.name;
        var $cdDiv = $("<div>"+ cdName +"</div>");
        var $cdList = $("<ol/>");
        var $normalizeCdButton = $("<div class='normalizeCdButton'></div>");
        $("#main").append($cdDiv);
        
        $normalizeCdButton.append("<img src='img/normalizeIcon.png'/>");
        $normalizeCdButton.append("<span class='status'></span>");
        $normalizeCdButton.click(function() {
            var thisButton = this;
            $(this).find(".status").text("updating...");
            $.getJSON("api/unifyTrackNamesForCd?" + _this.author + "/" + _this.book + "/" + cdName,
                function(data) {
                $(thisButton).find(".status").text(data.status || "no response");
                app.setScreenFromLocationHash(); // TODO just fetch this cd
            });
        });
        $cdDiv.append($normalizeCdButton);
        
        $.each(cd.tracks, function(trackIndex) {
            var $trackLi = $("<li/>");
            var $playTrackLink = $("<a class='PlayTrackLink' href='"+window.location.hash+"'>"+this+"</a>");
            $playTrackLink.text(this); 
            $playTrackLink.attr("data-cd", cdName);
            $playTrackLink.attr("data-cd-index", cdIndex);
            $playTrackLink.attr("data-track-index", trackIndex);
            $playTrackLink.attr("data-track", this);
            $playTrackLink.attr("data-file", "books/" + _this.author + "/" +
                                _this.book + "/" + cdName + "/" + this);
            $trackLi.append($playTrackLink);
            $cdList.append($trackLi);
        });
        $cdDiv.append($cdList);
        
        // jump to now playing track
        var nowPlaying = app.player.nowPlaying();
        var nowTrack = nowPlaying.trackName + nowPlaying.extension;
        var offset = $cdList.find("[data-track=\"" + nowTrack + "\"]").offset()
        console.log($cdList.find("[data-track=\"" + nowTrack + "\"]"));
        if (offset) {
            window.scrollTo(0, Math.max(0, offset.top - 32));
        }
    });
    
    $(".PlayTrackLink").click(function() {
        var trackIndex = parseInt($(this).attr("data-track-index"));
        var cdIndex = parseInt($(this).attr("data-cd-index"));
        var tracks = _this.data.cds[cdIndex].tracks.slice(trackIndex);
        
        var tracksToPaths = function(array, cdName) {
            for (var i=0; i < array.length; ++i) {
                array[i] = "books/" + _this.author + "/" +
                             _this.book + "/" + cdName + "/" + array[i];
            }
        }
        tracksToPaths(tracks, _this.data.cds[cdIndex].name);
        
        for (var i=cdIndex+1; i < _this.data.cds.length; ++i) {
            var cdPaths = _this.data.cds[i].tracks.slice();
            tracksToPaths(cdPaths, _this.data.cds[i].name);
            tracks = tracks.concat(cdPaths);
        }
        
        app.player.playList(tracks);
    });
    
}

TrackScreen.prototype.removeNodes = function() {
    $("#main").empty();
}
