---
layout: page
title: Pottery
permalink: /pottery
weight: 4
require_lightgallery: true
gallery:
  height: 230px
  width: 184px
  margin: .5px
  photos:
  - location: https://imgur.com/PeMIMnJ.png
    title: Mission Accomplished
    caption: The probe has touched down on Laythe, and is communicating with Kerbin.
---

<script type="text/javascript">
  let galleryYAML = {{page.gallery | jsonify}};
</script>

<div id="lightGallery" style="line-height: 0; text-align: center;">
</div>

<script src="js/jquery.min.js"></script>

<script src="js/album-finder.js"></script>

<script src="js/lightgallery.min.js"></script>
<script src="js/lg-thumbnail.min.js"></script>
<script src="js/lg-fullscreen.min.js"></script>
<script src="js/lg-autoplay.min.js"></script>

<script type="text/javascript">
  $(document).ready(function() {
    buildGallery("d9a8cf1ed42d1da",
                 "nhtWX",
                 "lightGallery",
                 { thumbHeight: "230px",
                   thumbWidth: "184px",
                   margin: ".5px",
                   photos: { "rHHV0wV": { title: "First Cup",
                                          description: "Leeches over Temmecu"},
                             "asdf": { thumbWidth: "250px" }
                           }
                 },
                 { preload: 3,
                   speed: 0,
                   download: false,
                   autoplay: false,
                   thumbnail: true,
                   exThumbImage: 'href',
                   toogleThumb: true,
                   showThumbByDefault: true,
                   subHtmlSelectorRelative: true
                 });
  });
</script>
