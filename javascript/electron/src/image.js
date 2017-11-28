const fs = require('fs')
const path = require('path')
const { shell } = require('electron')
const { spawn } = require('child_process')

let images = []
const openCmds = {
  darwin: 'open',
  win32: 'explorer',
  linux: 'nautilus'
}

const logError = err => err && console.error(err)

function mkdir (directory) {
  fs.stat(directory, (err, stats) => {
    if (err && err.code !== 'ENOENT') {
      return logError(err)
    } else if (err || !stats.isDirectory) {
      fs.mkdir(directory, logError)
    }
  })
}

function getPicturesDir (app) {
  return path.join(app.getPath('pictures'), 'photoboom')
}

function save (directory, content) {
  const imagePath = path.join(directory, `${new Date}.png`)
  const base64Data = content.replace(/^data:image\/png;base64,/, '')

  fs.writeFile(imagePath, base64Data, { encoding: 'base64' }, err => {
    if (err) return logError(err)
    cache(imagePath)
  })
}

function cache (imagePath) {
  images = images.concat([ imagePath ])
  return images
}

function lookupCache (index) {
  return images[index]
}

function rm (index, done) {
  fs.unlink(images[index], err => {
    if (err) return logError(err)
    images.splice(index, 1)
    done()
  })
}

function openDir (dirPath) {
  const cmd = openCmds[process.platform]
  if (cmd) {
    spawn(cmd, [ dirPath ])
  } else {
    shell.showItemInFolder()
  }
}

module.exports = { mkdir, getPicturesDir, save, cache, lookupCache, rm, openDir }
