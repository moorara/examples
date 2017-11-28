<style>
#message-list {
  margin-left: 20px;
}
</style>

<template>
  <div id="message-list">
    <h2>Messages</h2>
    <message-item
      v-for="message in messages"
      v-bind:key="message.text"
      v-bind:message="message">
    </message-item>
  </div>
</template>

<script>
import MessageItem from './MessageItem.vue'

export default {
  name: 'message-list',
  components: { MessageItem },
  data () {
    return {
      messages: this.$store.state.messages
    }
  }
}
</script>
