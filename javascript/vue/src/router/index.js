import Vue from 'vue'
import Router from 'vue-router'

import Home from '../components/home/Home'
import MessageList from '../components/message/MessageList'

Vue.use(Router)

export default new Router({
  routes: [
    {
      path: '/',
      component: Home
    },
    {
      path: '/messages',
      component: MessageList
    }
  ]
})
