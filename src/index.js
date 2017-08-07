require('./main.css');
var logoPath = require('./logo.svg');
var Elm = require('./App.elm');

// TOOD: Share these with Elm.
var pixelSize = 20;
var resolution = 16;

var root = document.getElementById('root');

var app = Elm.App.embed(root, logoPath);
app.ports.download.subscribe(function (grid) {
  var canvas = document.getElementById('canvas');
  canvas.width = pixelSize * resolution;
  canvas.height = pixelSize * resolution;

  drawInCanvas(canvas, grid);
  downloadCanvas(canvas);
});

function drawInCanvas(canvas, grid) {
  var ctx = canvas.getContext('2d');
  ctx.clearRect(0, 0, canvas.width, canvas.height);

  for (var row = 0; row < grid.length; row++) {
    var cols = grid[row];
    for (var col = 0; col < cols.length; col++) {
      var rgba = cols[col];
      var x = col * pixelSize;
      var y = row * pixelSize;
      ctx.fillStyle = fillColor(rgba);
      ctx.fillRect(x, y, pixelSize, pixelSize);
    }
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
  a.click();
}
