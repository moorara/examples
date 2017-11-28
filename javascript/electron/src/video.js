const constraints = {
  audio: false,
  video: {
    minWidth: 900,
    maxWidth: 550,
    minHeight: 900,
    maxHeight: 550
  }
}

const handleSuccess = (videoEl, stream) => {
  videoEl.src = window.URL.createObjectURL(stream)
}

const handleError = (err) => {
  console.log('Camera error: ', err)
}

function init (videoEl) {
  navigator.getUserMedia(constraints, handleSuccess.bind(null, videoEl), handleError)
}

function captureBytes (ctx, videoEl, canvasEl) {
  ctx.drawImage(videoEl, 0, 0)
  return canvasEl.toDataURL('image/png')
}

function captureBytesFromLiveCanvas (canvasEl) {
  return canvasEl.toDataURL('image/png')
}

module.exports = { init, captureBytes, captureBytesFromLiveCanvas }
