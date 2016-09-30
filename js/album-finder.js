function getURLWithAuthorization(clientID, url, callback) {
  let requester = new XMLHttpRequest();
  requester.onreadystatechange = function () {
    if(requester.readyState == 4 && requester.status == 200)
      callback(requester.responseText);
  };
  requester.open("GET", url);
  requester.setRequestHeader("Authorization", "Client-ID "+clientID);
  requester.send();
}

function getRateLimit(clientID, callback) {
  getURLWithAuthorization(clientID, "https://api.imgur.com/3/credits", callback);
}

function imgurGallery(clientID, albumID, containerID, galleryOptions, lightGalleryOptions) {
  let defaultOptions = { thumbBuilder: "defaultThumb",
                         margin: "0px",
                         thumbWidth: "184px",
                         thumbHeight: "230px",
                         backgroundSize: "cover",
                         backgroundPosition: "center",
                         containerAlign: "center"
                       };

  let lgOptionsExtended = { preload: 3,
                            speed: 0,
                            download: false,
                            autoplay: false,
                            thumbnail: true,
                            exThumbImage: "href",
                            toogleThumb: true,
                            showThumbByDefault: true,
                            subHtmlSelectorRelative: true
                          };
  $.extend(lgOptionsExtended, lightGalleryOptions);


  let globalOptions = defaultOptions;
  for(let key in galleryOptions)
    if(key !== "photos")
      globalOptions[key] = galleryOptions[key];

  document.getElementById(containerID).style.textAlign = globalOptions.containerAlign;

  // let url = "https://api.imgur.com/3/album/"+albumID+"/images";
  let url = "/sample-response.json";
  getURLWithAuthorization(clientID, url, function(response) {
    let album = JSON.parse(response);
    galleryFromObject(album, globalOptions, galleryOptions.photos, containerID, lgOptionsExtended);
  });
}

function yamlGallery(album, containerID) {
  let frag = document.createDocumentFragment();

  let defaultPhotoOptions = {};
  for(let key in album)
    if(key !== "photos" && key !== "lightGalleryParams")
      defaultPhotoOptions[key] = album[key];

  for(let key in album.photos) {
    let photo = $.extend(true, {}, defaultPhotoOptions, album.photos[key]);
    frag.appendChild(window[photo.thumbBuilder](photo));
  }

  let container = document.getElementById(containerID);
  if(container) {
    container.style.textAlign = album.containerAlign;
    container.appendChild(frag);
  }
  else {
    console.error("Container with id "+containerID+" not found");
  }

  // call the lightgallery code with the jquery object
  $("#"+containerID).lightGallery(album.lightgalleryParams);
}

function galleryFromObject(album, globalOptions, localOptions, containerID, lightGalleryOptions) {
  let frag = document.createDocumentFragment();

  for(let key in album.data) {
    let photo = album.data[key];
    $.extend(photo, globalOptions, localOptions[photo.id]);
    frag.appendChild(window[photo.thumbBuilder](photo));
  }

  let container = document.getElementById(containerID);
  if(container)
    container.appendChild(frag);
  else
    console.error("Container with id "+containerID+" not found");

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
  if(photo.widthFromHeight) {
    let newWidth = parseInt(photo.thumbHeight) * photo.widthFromHeight;
    photo.thumbWidth = newWidth+"px";
  }
  image.style.width = photo.thumbWidth;
  image.style.backgroundSize = photo.backgroundSize;
  image.style.backgroundPosition = photo.backgroundPosition;
  image.style.backgroundImage = "url("+photo.link+")";
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
