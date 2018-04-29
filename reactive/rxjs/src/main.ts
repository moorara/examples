import 'promise/polyfill'
import 'whatwg-fetch'

interface GitHubRepo {
  id: number
  name: string
  full_name: string
}

fetch('https://api.github.com/users/moorara/repos').then(res => {
  console.log(res.status)
  return res.json()
}).then(data => {
  console.log(data.map((r: GitHubRepo) => r.name))
}).catch(err => {
  console.log(err)
})
