function BooksScreen(author) {
    this.author = author;
}

BooksScreen.prototype.createNodes = function() {
    var _this = this;
    app.toolbar.setAuthor(this.author);
    
    var buildNodesFromJson = function(data) {
        for (var i=0; i < data.books.length; ++i) {
            var book = data.books[i];
            var path = "/" + _this.author + "/" + book + "/";
            var cover = Cover.createElement(path, book, data.covers);
            document.getElementById("main").appendChild(cover);
        };
    };
    
    var url = "api/author.json?" + encodeURIComponent(_this.author);
    $.getJSON(url, function(data) {
        data.books.sort();
        buildNodesFromJson(data);
    });
}

BooksScreen.prototype.removeNodes = function() {
    $("#main").empty();
}
