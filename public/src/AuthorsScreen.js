function AuthorsScreen() {
    Screen.prototype.constructor.call(this);
}

AuthorsScreen.prototype = new Screen();

AuthorsScreen.prototype.createNodes = function() {
    var _this = this;
    
    app.toolbar.setAuthor(null);
    
    var buildNodesFromJson = function(data) {
        for (var i=0; i < data.authors.length; ++i) {
            var author = data.authors[i];
            var path = "/" + author + "/";
            var cover = Cover.createElement(path, author, data.covers);
            document.getElementById("main").appendChild(cover);
        };
    };
    
    $.getJSON("api/authors.json", function(data) {
        console.log(data);
        data.authors.sort();
        buildNodesFromJson(data);
    });
}

AuthorsScreen.prototype.removeNodes = function() {
    $("#main").empty();
}
