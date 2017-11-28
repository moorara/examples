import React from 'react'
import { configure, shallow } from 'enzyme'
import Adapter from 'enzyme-adapter-react-16'

import ArticleList from '../ArticleList'

describe('ArticleList', () => {
  configure({
    adapter: new Adapter()
  })

  const testProps = {
    articles: {
      a: { id: 'a' },
      b: { id: 'b' },
    }
  }

  it('renders correctly', () => {
    const wrapper = shallow(
      <ArticleList {...testProps} />
    )

    expect(wrapper).toMatchSnapshot()
    expect(wrapper.find('ArticleContainer').length).toBe(2)
  })
})
