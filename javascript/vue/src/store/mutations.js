import * as types from './types'

export default {
  [types.GET_MESSAGES] (state, { messages }) {
    state.messages = messages
  },

  [types.SEND_MESSAGE] (state, { message }) {
    state.messages.push(message)
  }
}
