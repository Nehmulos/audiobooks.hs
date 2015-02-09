// TODO rename to navigationbar
function Toolbar() {
    this.author = null;
    this.book = null;
}

Toolbar.prototype.setAuthor = function(author) {
    this.author = author;

    this.setBook(null);
    $("#toolbar .authorName").empty();
    if (author) {
        var a = $("<a href='#!/" + author + "'>"+author+"</a>");
        $("#toolbar .authorName").append(a);
    }
}

Toolbar.prototype.setBook = function(book) {
    this.book = book;
    $("#toolbar .bookName").empty();
    if (book) {
        var a = $("<a href='#!/" + this.author + "/" + book+"'>"+book+"</a>");
        $("#toolbar .bookName").append(a);
        $("#toolbar .authorName").append($("<span class='arrow'> &gt;</span>"));
    }
}

