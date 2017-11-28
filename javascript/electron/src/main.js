const url = require('url')
const path = require('path')
const { app, BrowserWindow, Menu, ipcMain: ipc } = require('electron')

const image = require('./image')
const menu = require('./menu')

let mainWindow

app.on('ready', _ => {
  mainWindow = new BrowserWindow({
    width: 1200,
    height: 900,
    resizable: false
  })

  mainWindow.loadURL(
    url.format({
      pathname: path.join(__dirname, 'index.html'),
      protocol: 'file:',
      slashes: true
    })
  )
  
  // mainWindow.webContents.openDevTools()
  
  image.mkdir(image.getPicturesDir(app))

  mainWindow.on('closed', _ => {
    mainWindow = null
  })

  const menuContent = Menu.buildFromTemplate(menu.getTemplate(mainWindow))
  Menu.setApplicationMenu(menuContent)
})

ipc.on('image-captured', (event, content) => {
  let directory = image.getPicturesDir(app)
  image.save(directory, content)
})

ipc.on('image-remove', (event, index) => {
  image.rm(index, _ => {
    event.sender.send('image-removed', index)
  })
})
