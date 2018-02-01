/*
 * Run this code at https://jscomplete.com
 */


const Result = (props) => {
  return (
    <div>{props.counter}</div>
  )
}

class Button extends React.Component {
  handleClick = () => {
    this.props.onClickFunc(this.props.step)
  }

  render () {
    return (
      <button onClick={this.handleClick}>
        +{this.props.step}
      </button>
    )
  }
}

class App extends React.Component {
  /* constructor (props) {
    super(props)
    this.state = {
      counter: 0
    }
  } */

  state = {
    counter: 0
  }

  incCounter = (step) => {
    // Asynchronous
    /* this.setState({
      counter: this.state.counter + step
    }) */

    // Synchronous: updating state using a value from current state
    this.setState(prevState => ({
      counter: prevState.counter + step
    }))
  }

  render () {
    return (
      <div>
        <Button step={1} onClickFunc={this.incCounter} />
        <Button step={5} onClickFunc={this.incCounter} />
        <Button step={10} onClickFunc={this.incCounter} />
        <Button step={100} onClickFunc={this.incCounter} />
        <Result counter={this.state.counter} />
      </div>
    )
  }
}

ReactDOM.render(<App />, mountNode)
