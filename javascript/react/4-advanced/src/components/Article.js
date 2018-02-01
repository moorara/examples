import React from 'react'
import PropTypes from 'prop-types'

import storeProvider from './storeProvider'

const styles = {
  title: {
    fontWeight: 'bold',
  },
  date: {
    fontSize: '0.85em',
    color: '#888',
  },
  author: {
    paddingTop: 10,
    paddingBottom: 10,
  },
  body: {
    paddingLeft: 20,
  },
  article: {
    paddingBottom: 10,
    borderBottomStyle: 'solid',
    borderBottomColor: '#aaa',
    borderBottomWidth: 1,
    marginBottom: 10,
  },
}

const dateDisplay = (dateString) => new Date(dateString).toDateString()

class Article extends React.PureComponent {
  static propTypes = {
    author: PropTypes.shape({
      firstName: PropTypes.string.isRequired,
      lastName: PropTypes.string.isRequired,
      website: PropTypes.string.isRequired,
    }),
    article: PropTypes.shape({
      title: PropTypes.string.isRequired,
      body: PropTypes.string.isRequired,
      date: PropTypes.string.isRequired,
    })
  }

  render () {
    const { article, author } = this.props

    return (
      <div style={styles.article}>
        <div style={styles.title}>{article.title}</div>
        <div style={styles.date}>
          {dateDisplay(article.date)}
        </div>
        <div style={styles.author}>
          <a href={author.website}>
            {author.firstName} {author.lastName}
          </a>
        </div>
        <div style={styles.body}>{article.body}</div>
      </div>
    )
  }
}

function extraProps (store, originalProps) {
  return {
    author: store.lookupAuthor(originalProps.article.authorId),
  }
}

export default storeProvider(extraProps)(Article)
