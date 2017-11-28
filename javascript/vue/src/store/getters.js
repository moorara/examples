export const count = (state) => {
  return state.messages.length
}

export const messages = (state) => {
  return Object.assign([], state.messages)
}
