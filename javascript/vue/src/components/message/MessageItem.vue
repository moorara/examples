<style>
#message-item {
  margin-left: 40px;
}
</style>

<template>
  <div id="message-item">
    <h3>{{ message.text }}</h3>
  </div>
</template>

<script>
export default {
  name: 'message-item',
  props: {
    message: Object
  }
}
</script>
