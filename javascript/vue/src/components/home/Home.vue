<style>
#home {
  font-size: 16px;
}
a {
  font-size: 14px;
}
</style>

<template>
  <div id="home">
    <p>Welcome to Home Page!</p>
    <router-link to="/messages">See all messages</router-link>
  </div>
</template>

<script>
export default {
  name: 'home'
}
</script>
