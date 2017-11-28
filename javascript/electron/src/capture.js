const { remote, shell, ipcRenderer: ipc } = require('electron')

const video = require('./video')
const counter = require('./counter')
const flash = require('./flash')
const effect = require('./effect')
const image = remote.require('./image')

let seriously
let videoSrc
let canvasTarget

const formatImgTag = (bytes) => {
  const img = new Image()
  img.classList.add('photoImg')
  img.src = bytes
  
  const close = document.createElement('div')
  close.classList.add('photoClose')

  const div = document.createElement('div')
  div.classList.add('photo')
  div.appendChild(img)
  div.appendChild(close)

  return div
}

window.addEventListener('DOMContentLoaded', _ => {
  const videoEl = document.getElementById('video')
  const canvasEl = document.getElementById('canvas')
  const recordEl = document.getElementById('record')
  const counterEl = document.getElementById('counter')
  const flashEl = document.getElementById('flash')
  const photosEl = document.querySelector('.photosContainer')

  video.init(videoEl)

  seriously = new Seriously()
  videoSrc = seriously.source('#video')
  canvasTarget = seriously.target('#canvas')
  effect.choose(seriously, videoSrc, canvasTarget)

  recordEl.addEventListener('click', _ => {
    counter.countdown(counterEl, 3, _ => {
      flash(flashEl)
      const bytes = video.captureBytesFromLiveCanvas(canvasEl)
      photosEl.appendChild(formatImgTag(bytes))
      ipc.send('image-captured', bytes)
    })
  })

  photosEl.addEventListener('click', event => {
    const isRm = event.target.classList.contains('photoClose')
    const selector = isRm ? '.photoClose' : '.photoImg'

    const photos = Array.from(document.querySelectorAll(selector))
    const index = photos.findIndex(el => el === event.target)

    if (index > -1) {
      if (isRm) {
        ipc.send('image-remove', index)
      } else {
        shell.showItemInFolder(image.lookupCache(index))
      }
    }
  })

  ipc.on('image-removed', (event, index) => {
    document
      .getElementById('photos')
      .removeChild(
        document.querySelectorAll('.photo')[index]
      )
  })
  
  ipc.on('effect-cycle', event => {
    effect.cycle(seriously, videoSrc, canvasTarget)
  })

  ipc.on('effect-choose', (event, effectName) => {
    effect.choose(seriously, videoSrc, canvasTarget, effectName)
  })
})
