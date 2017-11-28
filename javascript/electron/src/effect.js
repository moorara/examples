const effects = {
  vanilla: (seriously, src, target) => {
    target.source = src
    seriously.go()
  },
  ascii: (seriously, src, target) => {
    const ascii = seriously.effect('ascii')
    connectEffect(seriously, src, target, ascii)
  },
  daltonize: (seriously, src, target) => {
    const daltonize = seriously.effect('daltonize')
    daltonize.type = '0.8'
    connectEffect(seriously, src, target, daltonize)
  },
  hex: (seriously, src, target) => {
    const hex = seriously.effect('hex')
    hex.size = 0.03
    connectEffect(seriously, src, target, hex)
  }
}

let currentIndex = 0
const effectNames = Object.keys(effects)

const connectEffect = (seriously, src, target, effect) => {
  effect.source = src
  target.source = effect
  seriously.go()
}

const setNextIndex = () => {
  currentIndex = (currentIndex + 1) % effectNames.length
  return currentIndex
}

const setCurrentIndex = (effectName) => {
  currentIndex = effectNames.indexOf(effectName)
  return currentIndex
}

function choose (seriously, src, target, effectName = 'vanilla') {
  effects[effectName](seriously, src, target)
  setCurrentIndex(effectName)
}

function cycle (seriously, src, target) {
  setNextIndex()
  effects[effectNames[currentIndex]](seriously, src, target)
}

module.exports = { choose, cycle }
