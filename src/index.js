require('./main.css');
require('blueimp-canvas-to-blob');
require('mobile-drag-drop').polyfill();
var fileSaver = require('file-saver');
var GIF = require('gif.js');
var Elm = require('./App.elm');
import registerServiceWorker from './registerServiceWorker';

var root = document.getElementById('root');

var app = Elm.Elm.App.init({
  node: root,
  flags: {
    pencil: require('./images/pencil.svg'),
    eraser: require('./images/eraser.svg'),
    bucket: require('./images/bucket.svg'),
    move: require('./images/move.svg'),
    trash: require('./images/trash.svg'),
    plus : require('./images/plus.svg'),
    undo: require('./images/undo.svg'),
    redo: require('./images/redo.svg'),
    download: require('./images/download.svg'),
    settings: require('./images/settings.svg')
  }
});
app.ports.download.subscribe(function (data) {
  switch (data.format) {
    case 'svg':
      exportSvg(data.grids[0], data.options);
      break;
    case 'gif':
      exportGif(data.grids[0], data.options);
      break;
    case 'animated-gif':
      exportAnimatedGif(data.grids, data.options);
      break;
  }
});

function exportSvg(grid, options) {
  var rects = grid.map(function (row, y) {
    return row.map(function (rgba, x) {
      return buildRect(x, y, rgba);
    }).join('');
  }).join('');
  var size = options.pixelSize * options.resolution;
  var svg = [
    '<svg xmlns="http://www.w3.org/2000/svg" width="',
    size,
    '" height="',
    size,
    '" viewBox="0 0 ',
    options.resolution,
    ' ',
    options.resolution,
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
  var color = fillColor(rgba);
  return [
    '<rect width="1" height="1" x="',
    x,
    '" y="',
    y,
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

function exportAnimatedGif(grids, options) {
  const size = options.pixelSize * options.resolution;
  var gif = new GIF({
    width: size,
    height: size
  });
  var canvas = createCanvas(size, size);
  var delay = 1000 / options.fps;
  var ctx = canvas.getContext('2d');
  for (var i = 0; i < grids.length; i++) {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    drawInCanvas(ctx, grids[i], options);
    gif.addFrame(ctx, { copy: true, delay: delay });
  }
  gif.on('finished', function (blob) {
    fileSaver.saveAs(blob, 'animation.gif');
  });
  gif.render();
}

function exportGif(grid, options) {
  var size = options.pixelSize * options.resolution;
  var canvas = createCanvas(size, size);
  var ctx = canvas.getContext('2d');
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  drawInCanvas(ctx, grid, options);
  canvas.toBlob(function (blob) {
    fileSaver.saveAs(blob, 'pixels.gif');
  }, 'image/gif');
}

function drawInCanvas(ctx, grid, options) {
  for (var row = 0; row < grid.length; row++) {
    var cols = grid[row];
    for (var col = 0; col < cols.length; col++) {
      var rgba = cols[col];
      var x = col * options.pixelSize;
      var y = row * options.pixelSize;
      ctx.fillStyle = fillColor(rgba);
      ctx.fillRect(x, y, options.pixelSize, options.pixelSize);
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

registerServiceWorker();
