const { app } = require('electron')

const image = require('./image')

const enableCycleEffect = (items) => {
  const nonEffectMenuOffset = 2
  const selectedIndex = items.findIndex(item => item.checked)
  const nextIndex = selectedIndex + 1 < items.length ? selectedIndex + 1 : nonEffectMenuOffset
  items[nextIndex].checked = true
}

function getTemplate (mainWindow) {
  const template = [
    {
      label: 'Effects',
      submenu: [
        {
          label: 'Cycle',
          accelerator: 'Shift+CmdOrCtrl+E',
          click: menuItem => {
            enableCycleEffect(menuItem.menu.items)
            mainWindow.webContents.send('effect-cycle')
          }
        },
        { type: 'separator' },
        {
          label: 'Vanilla',
          type: 'radio',
          click: _ => mainWindow.webContents.send('effect-choose')
        },
        {
          label: 'Ascii',
          type: 'radio',
          click: _ => mainWindow.webContents.send('effect-choose', 'ascii')
        },
        {
          label: 'Daltonize',
          type: 'radio',
          click: _ => mainWindow.webContents.send('effect-choose', 'daltonize')
        },
        {
          label: 'Hex',
          type: 'radio',
          click: _ => mainWindow.webContents.send('effect-choose', 'hex')
        }
      ]
    },
    {
      label: 'View',
      submenu: [
        {
          label: 'Photos Directory',
          click: _ => image.openDir(image.getPicturesDir(app))
        }
      ]
    }
  ]
  
  if (process.platform === 'darwin') {
    const name = app.getName()
    template.unshift({
      label: name,
      submenu: [
        {
          label: 'About ' + name,
          role: 'about'
        },
        { type: 'separator' },
        {
          label: 'Hide ' + name,
          accelerator: 'Command+H',
          role: 'hide'
        },
        {
          label: 'Hide Others',
          accelerator: 'Command+Shift+H',
          role: 'hideothers'
        },
        {
          label: 'Show All',
          role: 'unhide'
        },
        { type: 'separator' },
        {
          label: 'Quit',
          accelerator: 'Command+Q',
          click: _ => { app.quit() }
        }
      ]
    })
  }

  return template
}

module.exports = { getTemplate }
