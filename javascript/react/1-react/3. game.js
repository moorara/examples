/*
 * Run this code at https://jscomplete.com
 */


const possibleCombinationSum = (arr, n) => {
  if (arr.indexOf(n) >= 0) { return true }
  if (arr[0] > n) { return false }
  if (arr[arr.length - 1] > n) {
    arr.pop();
    return possibleCombinationSum(arr, n);
  }

  let listSize = arr.length, combinationsCount = (1 << listSize)
  for (let i = 1; i < combinationsCount ; i++ ) {
    let combinationSum = 0
    for (var j=0 ; j < listSize ; j++) {
      if (i & (1 << j)) {
        combinationSum += arr[j]
      }
    }
    if (n === combinationSum) { return true }
  }
  return false
};


const Stars = (props) => {
  return (
    <div className="col-5">
      {_.range(props.starsCount).map(i => <i key={i} className="fa fa-star"></i>)}
    </div>
  )
}

const Button = (props) => {
  let button
  switch (props.answerIsCorrect) {
    case true:
      button =
        <button className="btn btn-success" onClick={props.acceptAnswer}>
          <i className="fa fa-check"></i>
        </button>
      break;
    case false:
      button =
        <button className="btn btn-danger">
          <i className="fa fa-times"></i>
        </button>
      break;
    default:
      button =
        <button className="btn"
          onClick={props.checkAnswer}
          disabled={props.selectedNumbers.length === 0}>
          <i className="fa fa-question"></i>
        </button>
      break
  }

  return (
    <div className="col-2 text-center">
      {button}
      <br /><br />
      <button className="btn btn-warning btn-sm"
        onClick={props.redraw}
        disabled={props.redraws === 0} >
        <i className="fa fa-refresh"></i> {props.redraws}
      </button>
    </div>
  )
}

const Answer = (props) => {
  return (
    <div className="col-5">
      {props.selectedNumbers.map((no, i) =>
        <span key={i}
          onClick={() => props.unselectNumber(no)}>
          {no}
        </span>
      )}
    </div>
  )
}

const Numbers = (props) => {
  const getClassName = (no) => {
    if (props.usedNumbers.indexOf(no) >= 0) {
      return 'used'
    } else if (props.selectedNumbers.indexOf(no) >= 0) {
      return 'selected'
    }
  }

  return (
    <div className="card text-center">
      <div>
        {_.range(1, 10).map((no, i) =>
          <span key={i}
            className={getClassName(no)}
            onClick={() => props.selectNumber(no)}>
            {no}
          </span>
        )}
      </div>
    </div>
  )
}

const DoneFrame = (props) => {
  return (
    <div className="text-center">
      <h2>{props.status}</h2>
      <button onClick={props.resetGame}
        className="btn btn-secondary">
        Play Again!
      </button>
    </div>
  )
}

class Game extends React.Component {
  static randomNumber () {
    return 1 + Math.floor(Math.random() * 9)
  }

  static initState () {
    return {
      redraws: 5,
      starsCount: Game.randomNumber(),
      selectedNumbers: [],
      usedNumbers: [],
      answerIsCorrect: null,
      status: null
    }
  }

  state = Game.initState()

  resetGame = () => this.setState(Game.initState())

  selectNumber = (no) => {
    if (this.state.selectedNumbers.indexOf(no) >= 0) {
      return
    }
    this.setState(prevState => ({
      answerIsCorrect: null,
      selectedNumbers: prevState.selectedNumbers.concat(no)
    }))
  }

  unselectNumber = (no) => {
    this.setState(prevState => ({
      answerIsCorrect: null,
      selectedNumbers: prevState.selectedNumbers.filter(n => n !== no)
    }))
  }

  checkAnswer = () => {
    this.setState(prevState => ({
      answerIsCorrect: prevState.starsCount === prevState.selectedNumbers.reduce((acc, n) => acc + n, 0)
    }))
  }

  acceptAnswer = () => {
    this.setState(prevState => ({
      usedNumbers: prevState.usedNumbers.concat(prevState.selectedNumbers),
      selectedNumbers: [],
      answerIsCorrect: null,
      starsCount: Game.randomNumber()
    }), this.updateStatus)
  }

  redraw = () => {
    if (this.state.redraws === 0) {
      return
    }
    this.setState(prevState => ({
      redraws: prevState.redraws - 1,
      starsCount: Game.randomNumber(),
      selectedNumbers: [],
      answerIsCorrect: null
    }), this.updateStatus)
  }

  hasSolution = ({ starsCount, usedNumbers }) => {
    const possibleNumbers = _.range(1, 10).filter(no => usedNumbers.indexOf(no) === -1)
    return possibleCombinationSum(possibleNumbers, starsCount)
  }

  updateStatus = () => {
    this.setState(prevState => {
      if (prevState.usedNumbers.length === 9) {
        return { status: 'Done. Nice!' }
      } else if (prevState.redraws === 0 && !this.hasSolution(prevState)) {
        return { status: 'Game Over!' }
      }
    })
  }

  render () {
    const { redraws, starsCount, selectedNumbers, usedNumbers, answerIsCorrect, status } = this.state
    return (
      <div className="container">
        <h3>Play Nine</h3>
        <hr />
        <div className="row">
          <Stars starsCount={starsCount}/>
          <Button redraws={redraws} selectedNumbers={selectedNumbers} answerIsCorrect={answerIsCorrect}
            checkAnswer={this.checkAnswer} acceptAnswer={this.acceptAnswer} redraw={this.redraw} />
          <Answer selectedNumbers={selectedNumbers} unselectNumber={this.unselectNumber} />
        </div>
        <br />
        {
          status ?
          <DoneFrame status={status} resetGame={this.resetGame} /> :
          <Numbers selectedNumbers={selectedNumbers} usedNumbers={usedNumbers} selectNumber={this.selectNumber} />
        }
      </div>
    )
  }
}

class App extends React.Component {
  render () {
    return (
      <Game />
    )
  }
}

ReactDOM.render(<App />, mountNode)
