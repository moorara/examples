let timer

function flash (el) {
  if (el.classList.contains('is-flashing')) {
    el.classList.remove('is-flashing')
  }

  clearTimeout(timer)
  el.classList.add('is-flashing')
  timer = setTimeout(_ => el.classList.remove('is-flashing'), 2000)
}

module.exports = flash
