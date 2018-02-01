/*
 * Run this code at https://jscomplete.com
 */


const Card = (props) => {
  return (
    <div style={{ margin: '1em' }}>
      <img width="75" src={props.avatar_url} />
      <div style={{ display: 'inline-block', marginLeft: 10 }}>
        <div style={{ fontSize: '1.1em', fontWeight: 'bold' }}>{props.name}</div>
        <div style={{ fontSize: '1em' }}>{props.company}, {props.location}</div>
      </div>
    </div>
  )
}

class CardList extends React.Component {
  render () {
    return (
      <div>
        {this.props.cards.map(card => <Card key={card.id} {...card} />)}
      </div>
    )
  }
}

class Form extends React.Component {
  state = {
    username: ''
  }

  handleSubmit = (event) => {
    event.preventDefault()
    axios.get(`https://api.github.com/users/${this.state.username}`).then(res => {
      this.props.addNewFunc(res.data)
      this.setState({ username: '' })
    })
  }

  render () {
    return (
      <form onSubmit={this.handleSubmit}>
        <input type="text"
          value={this.state.username}
          onChange={event => this.setState({ username: event.target.value })}
          placeholder="GitHub username" required></input>
        <button type="submit">Add Card</button>
      </form>
    )
  }
}

class App extends React.Component {
  state = {
    cards: []
  }

  addNew = (user) => {
    this.setState(prevState => ({
      cards: prevState.cards.concat(user)
    }))
  }

  render () {
    return (
      <div>
        <Form addNewFunc={this.addNew} />
        <CardList cards={this.state.cards}/>
      </div>
    )
  }
}

ReactDOM.render(<App />, mountNode)
