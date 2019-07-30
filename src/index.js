import {Elm} from './Main.elm'
import * as serviceWorker from './serviceWorker'

const base64 = {
  encode: v => btoa(v),
  decode: v => atob(v),
}

const KEY = 'persisted_config'

const app = Elm.Main.init({ node: document.getElementById('root') })

app.ports.saveToStorage.subscribe((data) => {
  const encoded = base64.encode(JSON.stringify(data))

  localStorage.setItem(KEY, encoded)
})

app.ports.doLoadFromStorage.subscribe(() => {
  let data = localStorage.getItem(KEY)
  if (data) {
    try {
      data = JSON.parse(base64.decode(data))
    } catch(e) {
      localStorage.clearItem(KEY)
    }
  }
  app.ports.loadFromStorage.send(data)
})

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister()
