import { browser, by, element } from 'protractor'

export class AppPage {
  navigateToHome() {
    return browser.get('/home')
  }

  navigateToMessages() {
    return browser.get('/messages')
  }

  getHomePageTitle() {
    return element(by.css('#home h2')).getText()
  }
  
  getMessagesPageTitle() {
    return element(by.css('#message-list h2')).getText()
  }
}
