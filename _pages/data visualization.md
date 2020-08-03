---
layout: archive
title:
permalink: /data-viz/
author_profile: true

---


<html>
<head>
<meta name="viewport" content="width=device-width, initial-scale=1">
<style>
* {
  box-sizing: border-box;
}

body {
  margin: 0;
  font-family: Arial;
}

/* The grid: Four equal columns that floats next to each other */
.column {
  float: left;
  width: 25%;
  padding: 10px;
}

/* Style the images inside the grid */
.column img {
  opacity: 0.8; 
  cursor: pointer; 
}

.column img:hover {
  opacity: 1;
}

/* Clear floats after the columns */
.row:after {
  content: "";
  display: table;
  clear: both;
}

/* The expanding image container */
.container {
  position: relative;
  display: none;
}


</style>
</head>
<body>

<div style="text-align:center">
  <h2>Data Visualizations</h2>
  <p>Click on the images below to expand them:</p>
</div>

<!-- The four columns -->
<div class="row">
  <div class="column">
   <img src="/images/plots/Warren/Warren.png" alt="MA_elections" style="width:100%" onclick="myFunction(this);">
  </div>
  <div class="column">
    <img src="/images/plots/Wisconsin/Individual II Senate version.png" alt="WI_density" style="width:100%" onclick="myFunction(this);">
  </div>
  <div class="column">
    <img src="/images/plots/Wisconsin/Distance plot.png" alt="WI_density_avg" style="width:100%" onclick="myFunction(this);">
  </div>
  <div class="column">
    <img src="/images/plots/NOMINATE/2D NOMINATE.png" alt="full-nominate" style="width:100%" onclick="myFunction(this);">
  </div>
  <div class="column">
    <img src="/images/plots/NOMINATE/NOMINATE D1.png" alt="nominate_d1" style="width:100%" onclick="myFunction(this);">
  </div>
  <div class="column">
    <img src="/images/plots/NOMINATE/NOMINATE D2.png" alt="https://twitter.com/zacharylhertz/status/1288497353465442304/" style="width:100%" onclick="myFunction(this);">
  </div>
</div>

<div class="container">
  <span onclick="this.parentElement.style.display='none'" class="closebtn">&times;</span>
  <img id="expandedImg" style="width:100%">
</div>

<script>
function myFunction(imgs) {
  var expandImg = document.getElementById("expandedImg");
  expandImg.src = imgs.src;
  expandImg.href = imgs.alt;
  expandImg.parentElement.style.display = "block";
}
</script>

</body>
</html>
