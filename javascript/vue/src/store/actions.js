import Api from '../api'
import * as types from './types'

export const getMessages = ({ commit }) => {
  Api.getMessages().then(messages => commit(types.GET_MESSAGES, { messages }))
}

export const sendMessage = ({ commit }, text) => {
  Api.sendMessage(text).then(message => commit(types.SEND_MESSAGE, { message }))
}
