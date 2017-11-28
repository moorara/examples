import { AppPage } from './app.po'

describe('angular App', () => {
  let page: AppPage

  beforeEach(() => {
    page = new AppPage()
  })

  it('should display home page', () => {
    page.navigateToHome()
    expect(page.getHomePageTitle()).toEqual('Welcome to Angi App!')
  })
  
  it('should display messages page', () => {
    page.navigateToMessages()
    expect(page.getMessagesPageTitle()).toEqual('List of Messages #2')
  })
})
