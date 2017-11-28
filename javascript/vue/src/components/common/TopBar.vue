<style>
h1, h2 {
  font-weight: normal;
}
</style>

<template>
  <div id="top-bar">
    <h1>{{ title }}</h1>
  </div>
</template>

<script>
export default {
  name: 'top-bar',
  data() {
    return {
      title: 'Vue.js Application'
    }
  }
}
</script>
