import { data } from '../data'
import StateApi from '../stateApi'

describe('Api', () => {
  const store = new StateApi(data)

  it('exposes authors as an object', () => {
    const authors = store.getState().authors
    const authorId = data.authors[0].id
    const authorFirstName = data.authors[0].firstName

    expect(authors).toHaveProperty(authorId)
    expect(authors[authorId].firstName).toBe(authorFirstName)
  })

  it('exposes articles as an object', () => {
    const articles = store.getState().articles
    const articleId = data.articles[0].id
    const articleTitle = data.articles[0].title

    expect(articles).toHaveProperty(articleId)
    expect(articles[articleId].title).toBe(articleTitle)
  })
})
