import data from './data.json'

const DELAY = 1000

export default class Api {
  static getMessages () {
    return new Promise((resolve, reject) => {
      setTimeout(() => {
        resolve(Object.assign([], data.messages))
      }, DELAY)
    })
  }

  static sendMessage (text) {
    return new Promise((resolve, reject) => {
      setTimeout(() => {
        let message = { text }
        data.messages.push(message)
        resolve(message)
      }, DELAY)
    })
  }
}
