function Cover() {
    
}

Cover.createElement = function(path, captionText, coversArray) {
    var jewelDiv = document.createElement("div");
    jewelDiv.className = "JewelCase";
    
    var jewelAnchor = document.createElement("a");
    jewelAnchor.href= "#!" + path;

    var coverPath = "books" + path + "cover";
    if (coversArray) { 
        coverPath = Cover.pathFromArray(path, coversArray)
    }
    
    var jewelCoverImage = document.createElement("img");
    jewelCoverImage.className = "Cover";
    jewelCoverImage.onerror = Cover.multiPostfixCoverFunction(coverPath);
    jewelCoverImage.src = coversArray ? coverPath : coverPath + ".png";
    jewelAnchor.appendChild(jewelCoverImage);
    
    var overlay = document.createElement("img");
    overlay.className = "CoverOverlay";
    overlay.src = "img/coverOverlay_old.png";
    jewelAnchor.appendChild(overlay);
    
    var caption = document.createElement("span");
    caption.className="Caption";
    caption.textContent = captionText;
    jewelAnchor.appendChild(caption);
    
    jewelDiv.appendChild(jewelAnchor);
    return jewelDiv;
}

Cover.pathFromArray = function(dir, coversArray) {
    if (dir && dir[0] == "/") {
        dir = dir.substr(1);
    }
    var match = dir.match(/(.+\/)(.+\/)/);
    dir = match ? match[2] : dir;
    var prefix = match ? match[1] : "";
    
    for (var i=0; i < coversArray.length; ++i) {
        if (coversArray[i].indexOf(dir) == 0) {
            return "books/" + prefix + coversArray[i];
        }
    }
    return "img/missingCover.png";
}

// onError function for cover img element. Set this as onerror and src = png
Cover.multiPostfixCoverFunction = function(prefix, storage) {
    return function(event) {
        // try jpg
        event.target.onerror = function(event) {
            // try jpeg
            event.target.onerror = function(event) {
                // use 404 img
                event.target.src = "img/missingCover.png";
                event.target.onerror = null;
            };
            event.target.src = prefix + ".jpeg";
        };
        event.target.src = prefix + ".jpg";
    };
}
