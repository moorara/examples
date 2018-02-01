import React from 'react'
import PropTypes from 'prop-types'

import Article from './Article'

class ArticleList extends React.PureComponent {
  static propTypes = {
    articles: PropTypes.object.isRequired
  }

  render () {
    const { articles } = this.props

    return (
      <div>
        {Object.values(articles).map(article =>
          <Article
            key={article.id}
            article={article}
          />
        )}
      </div>
    )
  }
}

export default ArticleList
