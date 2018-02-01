const React = require('react')

const About = (props) => (
  <div style={{ margin: 40 }}>
    <h1>About</h1>
    <br />
    <p className="text-info">
      This application uses the following technologies:
    </p>
    <p className="text-success">
      <ul>
        <li>React</li>
        <li>React Router</li>
        <li>Flux</li>
        <li>Node</li>
        <li>Bootstrap</li>
        <li>Gulp</li>
        <li>Browserify</li>
      </ul>
    </p>
  </div>
)

module.exports = About
