<style>
#app {
  font-family: Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  color: #225577;
}
</style>

<template>
  <div id="app">
    <top-bar></top-bar>
    <router-view />
  </div>
</template>

<script>
import TopBar from './common/TopBar.vue'

export default {
  name: 'app',
  components: { TopBar }
}
</script>
