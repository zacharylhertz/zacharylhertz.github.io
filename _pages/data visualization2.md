---
layout: archive
title:
permalink: 
author_profile: true

---
<html>
<head>
<link rel="stylesheet" href="perfundo.min.css">
</head>
<body>

<div class="perfundo">
  <a class="perfundo__link" href="#perfundo-img1">
    <img src="/images/plots/Warren/Warren.png" alt="Demo image">
  </a>
  <div id="perfundo-img1" class="perfundo__overlay fadeIn">
    <figure class="perfundo__content perfundo__figure">
      <img src="/images/plots/Warren/Warren.png" alt="Demo image">
      <div class="perfundo__image" style="width: 800px; padding-top: 66.25%; background-image: url(img/img1.jpg);"></div>
    </figure>
    <a href="#perfundo-untarget" class="perfundo__close perfundo__control">Close</a>
    <a class="perfundo__next perfundo__control" href="#perfundo-img2">Next</a>
  </div>
</div>

<div class="perfundo">
  <a class="perfundo__link" href="#perfundo-img2">
    <img src="/images/plots/Wisconsin/Individual II Senate version.png" alt="Demo image">
  </a>
  <div id="perfundo-img2" class="perfundo__overlay fadeIn">
    <figure class="perfundo__content perfundo__figure">
      <img src="/images/plots/Wisconsin/Individual II Senate version.png" alt="Demo image">
      <div class="perfundo__image" style="width: 800px; padding-top: 66.25%; background-image: url(img/img2.jpg);"></div>
    </figure>
    <a href="#perfundo-untarget" class="perfundo__close perfundo__control">Close</a>
    <a class="perfundo__next perfundo__control" href="#perfundo-img3">Next</a>
    <a class="perfundo__prev perfundo__control" href="#perfundo-img1">Prev</a>
  </div>
</div>

<div class="perfundo">
  <a class="perfundo__link" href="#perfundo-img3">
    <img src="/images/plots/Wisconsin/Distance plot.png" alt="Demo image">
  </a>
  <div id="perfundo-img3" class="perfundo__overlay fadeIn">
    <figure class="perfundo__content perfundo__figure">
      <img src="/images/plots/Wisconsin/Distance plot.png" alt="Demo image">
      <div class="perfundo__image" style="width: 800px; padding-top: 66.25%; background-image: url(img/img3.jpg);"></div>
    </figure>
    <a href="#perfundo-untarget" class="perfundo__close perfundo__control">Close</a>
    <a class="perfundo__prev perfundo__control" href="#perfundo-img2">Prev</a>
  </div>
</div>

<script src="perfundo.min.js"></script>
<script>
  perfundo('.perfundo');
</script>

</body>
</html>
