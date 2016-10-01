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

function imgurGallery(pageData, containerID) {
  let url = "https://api.imgur.com/3/album/"+pageData.albumID+"/images";
  // let url = "/sample-response.json";
  getURLWithAuthorization(pageData.clientID, url, function(response) {
    let albumPhotos = JSON.parse(response).data;

    function indexOfPhotoWithID(id) {
      for(let i in albumPhotos)
        if(albumPhotos[i].id == id)
          return i;
      return null;
    }

    // merge page data into the response
    for(let i in pageData.photos) {
      let matching = indexOfPhotoWithID(pageData.photos[i].id);
      if(matching) {
        $.extend(true, albumPhotos[matching], pageData.photos[i]);
      } else {
        albumPhotos.push(pageData.photos[i]);
      }
    }

    // update the page data
    pageData.photos = albumPhotos;
    galleryFromObject(pageData, containerID);
  });
}

function galleryFromObject(album, containerID) {
  $(document).ready(function() {
    let frag = document.createDocumentFragment();

    let defaultPhotoOptions = {};
    for(let key in album)
      if(key !== "photos" && key !== "lightGalleryParams")
        defaultPhotoOptions[key] = album[key];

    for(let key in album.photos) {
      let photo = $.extend(true, {}, defaultPhotoOptions, album.photos[key]);
      let thumbBuilder = window[photo.thumbBuilder];
      let captionBuilder = window[photo.captionBuilder];
      frag.appendChild(thumbBuilder(photo, captionBuilder));
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
  });
}

function defaultThumb(photo, captionBuilder) {
  let caption = captionBuilder(photo);

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

function defaultCaption(photo) {
  let title = document.createElement("h3");
  title.textContent = photo.title;

  let caption = document.createElement("div");
  caption.className = "caption";
  caption.style.display = "none";
  caption.appendChild(title);
  if(photo.description) {
    caption.appendChild(document.createTextNode(photo.description));
    caption.appendChild(document.createElement("br"));
  }
  if(photo.datetime) {
    if(typeof photo.datetime == "string") {
      // try parsing the date to get it into the standard format,
      // but fall back on the string if that fails
      let msec = Date.parse(photo.datetime);
      if(isNaN(msec)) {
        caption.appendChild(document.createTextNode(photo.datetime));
      }
      caption.appendChild(formatDate(msec));
    } else if(typeof photo.datetime == "number") {
      let msec = photo.datetime * 1000;
      caption.appendChild(formatDate(msec));
    } else {
      console.error("Invalid date: "+photo.datetime);
    }
  }

  return caption;
}

function formatDate(msec) {
  let date = new Date(msec);
  let months = ["January", "February", "March", "April", "May", "June", "July",
                "August", "September", "October", "November", "December"];
  let element = document.createElement("small");
  let dateString = months[date.getMonth()] + " " + date.getFullYear();
  element.appendChild(document.createTextNode(dateString));
  return element;
}
