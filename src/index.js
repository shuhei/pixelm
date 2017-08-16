require('./main.css');
var logoPath = require('./logo.svg');
var Elm = require('./App.elm');

// TOOD: Share these with Elm.
var pixelSize = 20;

var root = document.getElementById('root');

var app = Elm.App.embed(root, logoPath);
app.ports.download.subscribe(function (grid) {
  var canvas = document.getElementById('canvas');
  canvas.width = grid.colSize * pixelSize;
  canvas.height = grid.rowSize * pixelSize;

  drawInCanvas(canvas, grid);
  downloadCanvas(canvas);
});

function drawInCanvas(canvas, grid) {
  var ctx = canvas.getContext('2d');
  ctx.clearRect(0, 0, canvas.width, canvas.height);

  for (var i = 0; i < grid.data.length; i++) {
    var pixel = grid.data[i];
    var x = pixel[0][0] * pixelSize;
    var y = pixel[0][1] * pixelSize;
    ctx.fillStyle = fillColor(pixel[1]);
    ctx.fillRect(x, y, pixelSize, pixelSize);
  }
}

function fillColor(rgba) {
  return [
    'rgba(',
    rgba.red,
    ',',
    rgba.green,
    ',',
    rgba.blue,
    ',',
    rgba.alpha,
    ')'
  ].join('');
}

function downloadCanvas(canvas) {
  var a = document.createElement('a');
  a.href = canvas.toDataURL('image/gif');
  a.download = 'pixels.gif';
  a.target = '_blank';
  a.click();
}

// Stop scroll on touch devices.
setTimeout(function () {
  var grid = document.querySelector('.pixel-grid-container');
  grid.addEventListener('touchmove', function (e) {
    e.preventDefault();
    e.stopPropagation();
  });
}, 500);
