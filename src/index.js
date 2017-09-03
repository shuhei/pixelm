require('blueimp-canvas-to-blob');
require('mobile-drag-drop').polyfill();
var fileSaver = require('file-saver');
var GIF = require('gif.js');
var Elm = require('./App.elm');
require('./main.css');

// TOOD: Share these with Elm.
var pixelSize = 20;
var resolution = 16;

var root = document.getElementById('root');

var app = Elm.App.embed(root, {
  pencil: require('./images/pencil.svg'),
  eraser: require('./images/eraser.svg'),
  bucket: require('./images/bucket.svg'),
  move: require('./images/move.svg'),
  trash: require('./images/trash.svg'),
  plus : require('./images/plus.svg'),
  undo: require('./images/undo.svg'),
  redo: require('./images/redo.svg'),
  download: require('./images/download.svg')
});
app.ports.download.subscribe(function (data) {
  switch (data.format) {
    case 'svg':
      exportSvg(data.grids[0]);
      break;
    case 'gif':
      exportGif(data.grids[0]);
      break;
    case 'animated-gif':
      exportAnimatedGif(data.grids);
      break;
  }
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
  var blob = new Blob([svg], {
    type: 'image/svg+xml;charset=utf-8'
  });
  fileSaver.saveAs(blob, 'pixels.svg', true);
}

function buildRect(x, y, rgba) {
  if (rgba.alpha === 0) {
    return '';
  }
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

function createCanvas(width, height) {
  var canvas = document.createElement('canvas');
  canvas.width = width;
  canvas.height = height;
  return canvas;
}

function exportAnimatedGif(grids) {
  const size = pixelSize * resolution;
  var gif = new GIF({
    width: size,
    height: size
  });
  var canvas = createCanvas(size, size);
  var ctx = canvas.getContext('2d');
  for (var i = 0; i < grids.length; i++) {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    drawInCanvas(ctx, grids[i]);
    gif.addFrame(ctx, { copy: true, delay: 100 });
  }
  gif.on('finished', function (blob) {
    fileSaver.saveAs(blob, 'animation.gif');
  });
  gif.render();
}

function exportGif(grid) {
  var canvas = createCanvas(pixelSize * resolution, pixelSize * resolution);
  var ctx = canvas.getContext('2d');
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  drawInCanvas(ctx, grid);
  canvas.toBlob(function (blob) {
    fileSaver.saveAs(blob, 'pixels.gif');
  }, 'image/gif');
}

function drawInCanvas(ctx, grid) {
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

// Stop scroll on touch devices.
setTimeout(function () {
  var grid = document.querySelector('.pixel-grid-container');
  grid.addEventListener('touchmove', function (e) {
    e.preventDefault();
    e.stopPropagation();
  });
}, 500);
