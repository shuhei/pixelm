require('./main.css');
var Elm = require('./App.elm');

// TOOD: Share these with Elm.
var pixelSize = 20;
var resolution = 16;

var root = document.getElementById('root');

var app = Elm.App.embed(root, {
  pencil: require('./pencil.svg'),
  eraser: require('./eraser.svg'),
  bucket: require('./bucket.svg'),
  move: require('./move.svg'),
  trash: require('./trash.svg'),
  undo: require('./undo.svg'),
  download: require('./download.svg')
});
app.ports.download.subscribe(function (grid) {
  exportSvg(grid);
  exportGif(grid);
});

function exportSvg(grid) {
  var rects = grid.map(function (row, y) {
    return row.map(function (rgba, x) {
      return buildRect(x, y, rgba);
    }).join('');
  }).join('');
  var size = pixelSize * resolution;
  var svg = [
    '<svg xmlns="http://www.w3.org/2000/svg" width="',
    size,
    '" height="',
    size,
    '" viewBox="0 0 ',
    size,
    ' ',
    size,
    '">',
    rects,
    '</svg>'
  ].join('');
  var url = 'data:image/svg+xml;utf8,' + svg;
  downloadData('pixels.svg', url);
}

function buildRect(x, y, rgba) {
  var xx = x * pixelSize;
  var yy = y * pixelSize;
  var color = fillColor(rgba);
  return [
    '<rect width="',
    pixelSize,
    '" height="',
    pixelSize,
    '" x="',
    xx,
    '" y="',
    yy,
    '" fill="',
    color,
    '" />'
  ].join('');
}

function exportGif(grid) {
  var canvas = document.getElementById('canvas');
  canvas.width = pixelSize * resolution;
  canvas.height = pixelSize * resolution;

  drawInCanvas(canvas, grid);
  downloadData('pixels.gif', canvas.toDataURL('image/gif'));
}

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

function downloadData(filename, url) {
  var a = document.createElement('a');
  a.href = url;
  a.download = filename;
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
