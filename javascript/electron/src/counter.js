const setCount = (counterEl, count) => {
  counterEl.innerHTML = count > 0 ? count : ''
}

function countdown (counterEl, downFrom, done) {
  for (let i = 0; i <= downFrom; i++) {
    setTimeout(_ => {
      setCount(counterEl, downFrom - i)
      if (i === downFrom) done()
    }, i * 1000)
  }
}

module.exports = { countdown }
