function Application() {
    this.screen = null;
    this.toolbar = new Toolbar();
    this.tools = new Tools();
    if (localStorage.getItem("abp_playerType") === "RemotePlayer") {
        this.player = new RemotePlayer();
    } else {
        this.player = new WebPlayer();
    }
    this.playerGui = new PlayerGui();
}

Application.prototype.setScreenFromLocationHash = function()
{
    if(!this.screen) {
        $("#main").empty(); // remove no js advice
    }
    
    var fields = Utils.parseHashUrl(window.location.hash);
    
    if(fields.length === 1) {
        this.setScreen(new BooksScreen(fields[0]));
    } else if(fields.length === 2) {
        this.setScreen(new TrackScreen(fields[0], fields[1]));
    } else {
        this.setScreen(new AuthorsScreen());
    }
}

Application.prototype.setScreen = function(screen) {
    if(this.screen) {
        this.screen.removeNodes();
    }
    this.screen = screen;
    if(screen) {
        screen.createNodes();
    }
}
