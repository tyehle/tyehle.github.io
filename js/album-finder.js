function getURLWithAuthorization(clientID, url, callback) {
  let requester = new XMLHttpRequest();
  requester.onreadystatechange = function () {
    if(requester.readyState == 4 && requester.status == 200)
      callback(requester.responseText);
  }
  requester.open("GET", url);
  requester.setRequestHeader("Authorization", "Client-ID "+clientID);
  requester.send();
}

function getRateLimit(clientID, callback) {
  getURLWithAuthorization(clientID, "https://api.imgur.com/3/credits", callback);
}

// id should be "d9a8cf1ed42d1da"
function buildGallery(clientID, albumID, containerID, galleryOptions, lightGalleryOptions) {
  let defaultOptions = { thumbBuilder: defaultThumb,
                         margin: "0px",
                         thumbWidth: "184px",
                         thumbHeight: "230px",
                         "background-size": "cover",
                         "background-position": "center"
                       };

  let globalOptions = defaultOptions;
  for(key in galleryOptions)
    if(key !== "photos")
      globalOptions[key] = galleryOptions[key];

  // let url = "https://api.imgur.com/3/album/"+albumID+"/images";
  let url = "/sample-response.json";
  getURLWithAuthorization(clientID, url, function(response) {
    let album = JSON.parse(response);
    galleryFromObject(album, globalOptions, galleryOptions.photos, containerID, lightGalleryOptions);
  });
}

function galleryFromObject(album, globalOptions, localOptions, containerID, lightGalleryOptions) {
  let frag = document.createDocumentFragment();

  for(key in album.data) {
    let photo = album.data[key];
    $.extend(photo, globalOptions, localOptions[photo.id]);
    frag.appendChild(photo.thumbBuilder(photo));
  }

  let container = document.getElementById(containerID);
  if(container)
    container.appendChild(frag);
  else
    console.log("Container with id "+containerID+" not found");

  // call the lightgallery code (need the jquery object to call the code right)
  $("#"+containerID).lightGallery(lightGalleryOptions);
}

function defaultThumb(photo) {
  let title = document.createElement("h3");
  title.textContent = photo.title;

  let caption = document.createElement("div");
  caption.className = "caption";
  caption.style.display = "none";
  caption.appendChild(title);
  if(photo.description) {
    caption.appendChild(document.createTextNode(photo.description));
  }

  let image = document.createElement("div");
  image.style.height = photo.thumbHeight;
  image.style.width = photo.thumbWidth;
  image.style["background-size"] = photo["background-size"];
  image.style["background-position"] = photo["background-position"];
  image.style["background-image"] = "url("+photo.link+")";
  image.style.display = "inline-block";
  image.appendChild(caption);

  let link = document.createElement("a");
  link.href = photo.link;
  link.setAttribute("data-sub-html", ".caption");
  link.style.display = "inline-block";
  link.style.margin = photo.margin;
  link.appendChild(image);

  return link;
}
